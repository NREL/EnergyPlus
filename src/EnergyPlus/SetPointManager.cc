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
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::SetPointManager {

// Module containing the SetPoint Manager routines

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl
//       DATE WRITTEN   July 1998
//       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
//                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
//                        Add new setpoint managers:
//                          SET POINT MANAGER:SINGLE ZONE HEATING and
//                          SET POINT MANAGER:SINGLE ZONE COOLING
//                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
//                        Work supported by ASHRAE research project 1254-RP
//                      Phil Haves Oct 2004
//                      B. Griffith Aug. 2006.
//                      R. Raustad - FSEC: added AllSetPtMgr used for node conflict checks
//                      July 2010 B.A. Nigusse, FSEC/UCF
//                        Added new setpoint managers:
//                          SetpointManager:MultiZone:Heating:Average
//                          SetpointManager:MultiZone:Cooling:Average
//                          SetpointManager:MultiZone:MinimumHumidity:Average
//                          SetpointManager:MultiZone:MaximumHumidity:Average
//                       22Aug2010 Craig Wray - added Fan:ComponentModel
//                      Aug 2010 B.A. Nigusse, FSEC/UCF
//                        Added new setpoint managers:
//                          SetpointManager:MultiZone:Humidity:Minimum
//                          SetpointManager:MultiZone:Humidity:Maximum
//                      July 2011 Chandan Sharma, FSEC/UCF
//                        Added new setpoint managers:
//                          SetpointManager:FollowOutdoorAirTemperature
//                          SetpointManager:FollowSystemNodeTemperature
//                          SetpointManager:FollowGroundTemperature
//                      March 2012, Atefe Makhmalbaf and Heejin Cho, PNNL
//                        Added new setpoint manager:
//                          SetpointManager:CondenserEnteringReset
//                      Jan 2022 Wooyoung Jung, Jeremy Lerond and Jian Zhang, PNNL
//                        Added new setpoint managers:
//                          SetpointManager:SystemNodeReset:Temperature
//                          SetpointManager:SystemNodeReset:Humidity
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// determine all the controller setpoints in the problem.

// METHODOLOGY EMPLOYED:
// Previous time step node data will be used, in a set of fixed, precoded algorithms,
// to determine the current time step's controller setpoints.

using namespace DataLoopNode;
using namespace DataAirLoop;
using namespace ScheduleManager;
using namespace CurveManager;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbW;

constexpr std::array<std::string_view, static_cast<int>(CtrlVarType::Num)> controlTypeName = {"Temperature",
                                                                                              "MaximumTemperature",
                                                                                              "MinimumTemperature",
                                                                                              "HumidityRatio",
                                                                                              "MaximumHumidityRatio",
                                                                                              "MinimumHumidityRatio",
                                                                                              "MassFlowRate",
                                                                                              "MaximumMassFlowRate",
                                                                                              "MinimumMassFlowRate"};

constexpr std::array<std::string_view, static_cast<int>(CtrlVarType::Num)> controlTypeNameUC = {"TEMPERATURE",
                                                                                                "MAXIMUMTEMPERATURE",
                                                                                                "MINIMUMTEMPERATURE",
                                                                                                "HUMIDITYRATIO",
                                                                                                "MAXIMUMHUMIDITYRATIO",
                                                                                                "MINIMUMHUMIDITYRATIO",
                                                                                                "MASSFLOWRATE",
                                                                                                "MAXIMUMMASSFLOWRATE",
                                                                                                "MINIMUMMASSFLOWRATE"};

constexpr std::array<std::string_view, static_cast<int>(ControlStrategy::Num)> strategyNamesUC = {
    "TEMPERATUREFIRST",
    "FLOWFIRST",
};

constexpr std::array<std::string_view, static_cast<int>(SetPointManagerType::Num)> managerTypeName = {
    "SetpointManager:Scheduled",
    "SetpointManager:Scheduled:DualSetpoint",
    "SetpointManager:OutdoorAirReset",
    "SetpointManager:SingleZone:Reheat",
    "SetpointManager:SingleZone:Heating",
    "SetpointManager:SingleZone:Cooling",
    "SetpointManager:SingleZone:Humidity:Minimum",
    "SetpointManager:SingleZone:Humidity:Maximum",
    "SetpointManager:MixedAir",
    "SetpointManager:OutdoorAirPretreat",
    "SetpointManager:Warmest",
    "SetpointManager:Coldest",
    "SetpointManager:WarmestTemperatureFlow",
    "SetpointManager:ReturnAirBypassFlow",
    "SetpointManager:MultiZone:Cooling:Average",
    "SetpointManager:MultiZone:Heating:Average",
    "SetpointManager:MultiZone:MinimumHumidity:Average",
    "SetpointManager:MultiZone:MaximumHumidity:Average",
    "SetpointManager:MultiZone:Humidity:Minimum",
    "SetpointManager:MultiZone:Humidity:Maximum",
    "SetpointManager:FollowOutdoorAirTemperature",
    "SetpointManager:FollowSystemNodeTemperature",
    "SetpointManager:FollowGroundTemperature",
    "SetpointManager:CondenserEnteringReset",
    "SetpointManager:CondenserEnteringReset:Ideal",
    "SetpointManager:SingleZone:OneStageCooling",
    "SetpointManager:SingleZone:OneStageHeating",
    "SetpointManager:ReturnTemperature:ChilledWater",
    "SetpointManager:ReturnTemperature:HotWater",
    "SetpointManager:ScheduledTES",
    "SetpointManager:SystemNodeReset:Temperature",
    "SetpointManager:SystemNodeReset:Humidity"};

void ManageSetPoints(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor, Rick Strand
    //       DATE WRITTEN   May 1998
    //       MODIFIED       Fred Buhl May 2000
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:

    // METHODOLOGY EMPLOYED:
    // Each flag is checked and the appropriate manager is then called.

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum; // loop index

    // First time ManageSetPoints is called, get the input for all the setpoint managers
    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    InitSetPointManagers(state);

    if (state.dataSetPointManager->ManagerOn) {
        SimSetPointManagers(state);
        UpdateSetPointManagers(state);
        // The Mixed Air Setpoint Managers (since they depend on other setpoints, they must be calculated
        // and updated next to last).
        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMixedAirSetPtMgrs; ++SetPtMgrNum) {
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).calculate(state);
        }
        UpdateMixedAirSetPoints(state);
        // The Outside Air Pretreat Setpoint Managers (since they depend on other setpoints, they must be calculated
        // and updated last).
        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOAPretreatSetPtMgrs; ++SetPtMgrNum) {
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).calculate(state);
        }
        UpdateOAPretreatSetPoints(state);
    }
}

void GetSetPointManagerInputs(EnergyPlusData &state)
{
    // wrapper for GetInput to allow unit testing when fatal inputs are detected
    bool ErrorsFound(false);
    const char *RoutineName("GetSetPointManagerInputs: "); // include trailing blank space

    GetSetPointManagerInputData(state, ErrorsFound);

    if (ErrorsFound) {
        ShowFatalError(state, format("{}Errors found in input.  Program terminates.", RoutineName));
    }
}

void GetSetPointManagerInputData(EnergyPlusData &state, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   July 1998
    //       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
    //                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
    //                        Add new setpoint managers:
    //                          SET POINT MANAGER:SINGLE ZONE HEATING and
    //                          SET POINT MANAGER:SINGLE ZONE COOLING
    //                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
    //                        Work supported by ASHRAE research project 1254-RP
    //                      Haves October 2004
    //                      Witte (GARD), Sep 2006
    //                      July 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers:
    //                          SetpointManager:MultiZone:Heating:Average
    //                          SetpointManager:MultiZone:Cooling:Average
    //                          SetpointManager:MultiZone:MinimumHumidity:Average
    //                          SetpointManager:MultiZone:MaximumHumidity:Average
    //                      Aug 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers:
    //                          SetpointManager:MultiZone:Humidity:Minimum
    //                          SetpointManager:MultiZone:Humidity:Maximum
    //                      Jan 2022 Wooyoung Jung, Jeremy Lerond, and Jian Zhang, PNNL
    //                        Added new setpoint managers:
    //                          SetpointManager:SystemNodeReset:Temperature
    //                          SetpointManager:SystemNodeReset:Humidity

    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Input the SetPointManager data and store it in the SetPtMgrIn array.
    // Examine the Controllers in the input data and determine which ones
    // will have their setpoints set by a particular Setpoint Manager.

    // METHODOLOGY EMPLOYED:
    // Use the Get routines from the InputProcessor module.

    // Using/Aliasing
    using DataZoneEquipment::GetSystemNodeNumberForZone;
    using General::FindNumberInList;

    using NodeInputManager::GetNodeNums;
    using NodeInputManager::GetOnlySingleNode;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName{"GetSetPointManagerInputs"};

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D_string cAlphaFieldNames;
    Array1D_string cNumericFieldNames;
    Array1D_bool lNumericFieldBlanks;
    Array1D_bool lAlphaFieldBlanks;
    Array1D_string cAlphaArgs;
    Array1D<Real64> rNumericArgs;
    std::string cCurrentModuleObject;

    int NumNums;   // Number of real numbers returned by GetObjectItem
    int NumAlphas; // Number of alphanumerics returned by GetObjectItem
    int NumParams;
    int SetPtMgrNum;    // Setpoint Manager index
    int AllSetPtMgrNum; // Setpoint Manager index to ALL setpoint managers in single TYPE
    int IOStat;         // Status flag from GetObjectItem
    int NumNodesCtrld;  // number of controlled nodes in input node list
    int CtrldNodeNum;   // index of the items in the controlled node node list
    int NumZones;       // number of zone nodes in input node list
    int ZoneNum;        // loop index for zone nodes
    int NumNodes;
    Array1D_int NodeNums;
    bool NodeListError(false);
    bool ErrInList;
    int Found;

    NumNodesCtrld = 0;
    CtrldNodeNum = 0;
    NumZones = 0;
    ZoneNum = 0;

    cCurrentModuleObject = "SetpointManager:Scheduled";
    state.dataSetPointManager->NumSchSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:Scheduled'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = NumNums;
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = NumAlphas;

    cCurrentModuleObject = "SetpointManager:Scheduled:DualSetpoint";
    state.dataSetPointManager->NumDualSchSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:Scheduled:DualSetpoint'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:OutdoorAirReset";
    state.dataSetPointManager->NumOutAirSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:OutdoorAirReset'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:Reheat";
    state.dataSetPointManager->NumSZRhSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:SingleZone:Reheat'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:Heating";
    state.dataSetPointManager->NumSZHtSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:SingleZone:Heating'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:Cooling";
    state.dataSetPointManager->NumSZClSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:SingleZone:Cooling'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Minimum";
    state.dataSetPointManager->NumSZMinHumSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:SingleZone:Humidity:Minimum'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Maximum";
    state.dataSetPointManager->NumSZMaxHumSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:SingleZone:Humidity:Maximum'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MixedAir";
    state.dataSetPointManager->NumMixedAirSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:MixedAir'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:OutdoorAirPretreat";
    state.dataSetPointManager->NumOAPretreatSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:OutdoorAirPretreat'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:Warmest";
    state.dataSetPointManager->NumWarmestSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:Warmest'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:Coldest";
    state.dataSetPointManager->NumColdestSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:Coldest'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:WarmestTemperatureFlow";
    state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:WarmestTemperatureFlow'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:ReturnAirBypassFlow";
    state.dataSetPointManager->NumRABFlowSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:ReturnAirBypassFlow'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MultiZone:Cooling:Average";
    state.dataSetPointManager->NumMZClgAverageSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:MultiZone:Cooling:Average'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MultiZone:Heating:Average";
    state.dataSetPointManager->NumMZHtgAverageSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:MultiZone:Heating:Average'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MultiZone:MinimumHumidity:Average";
    state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
        state, cCurrentModuleObject); // 'SetpointManager:MultiZone:MinimumHumidity:Average'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MultiZone:MaximumHumidity:Average";
    state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
        state, cCurrentModuleObject); // 'SetpointManager:MultiZone:MaximumHumidity:Average'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Minimum";
    state.dataSetPointManager->NumMZMinHumSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:MultiZone:Humidity:Minimum'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Maximum";
    state.dataSetPointManager->NumMZMaxHumSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:MultiZone:Humidity:Maximum'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:FollowOutdoorAirTemperature";
    state.dataSetPointManager->NumFollowOATempSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:FollowOutdoorAirTemperature'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:FollowSystemNodeTemperature";
    state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:FollowSystemNodeTemperature'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:FollowGroundTemperature";
    state.dataSetPointManager->NumGroundTempSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:FollowGroundTemperature'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:CondenserEnteringReset";
    state.dataSetPointManager->NumCondEntSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:CondenserEnteringReset'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:CondenserEnteringReset:Ideal";
    state.dataSetPointManager->NumIdealCondEntSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // 'SetpointManager:CondenserEnteringReset:Ideal'
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:OneStageCooling";
    state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SingleZone:OneStageHeating";
    state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:ReturnTemperature:ChilledWater";
    state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:ReturnTemperature:HotWater";
    state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SystemNodeReset:Temperature";
    state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    cCurrentModuleObject = "SetpointManager:SystemNodeReset:Humidity";
    state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumParams, NumAlphas, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, NumNums);
    state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas = max(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, NumAlphas);

    state.dataSetPointManager->NumAllSetPtMgrs =
        state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs + state.dataSetPointManager->NumOutAirSetPtMgrs +
        state.dataSetPointManager->NumSZRhSetPtMgrs + state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
        state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
        state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
        state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
        state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
        state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
        state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
        state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
        state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
        state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
        state.dataSetPointManager->NumIdealCondEntSetPtMgrs + state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs +
        state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs + state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs +
        state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs + state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs +
        state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs;

    cAlphaFieldNames.allocate(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas);
    cAlphaArgs.allocate(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas);
    lAlphaFieldBlanks.dimension(state.dataSetPointManager->GetSetPointManagerInputMaxNumAlphas, false);
    cNumericFieldNames.allocate(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers);
    rNumericArgs.dimension(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, 0.0);
    lNumericFieldBlanks.dimension(state.dataSetPointManager->GetSetPointManagerInputMaxNumNumbers, false);

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
    NodeNums.dimension(NumParams, 0);

    if (state.dataSetPointManager->NumAllSetPtMgrs > 0)
        state.dataSetPointManager->AllSetPtMgr.allocate(
            state.dataSetPointManager->NumAllSetPtMgrs); // Allocate the entire Setpoint Manager input data array

    // Input the Scheduled Setpoint Managers

    if (state.dataSetPointManager->NumSchSetPtMgrs > 0)
        state.dataSetPointManager->SchSetPtMgr.allocate(state.dataSetPointManager->NumSchSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager

    cCurrentModuleObject = "SetpointManager:Scheduled";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        // setup program flow control integers
        if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumTemperature")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxTemp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumTemperature")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinTemp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "HumidityRatio")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::HumRat;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumHumidityRatio")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxHumRat;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumHumidityRatio")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinHumRat;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MassFlowRate")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MassFlow;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumMassFlowRate")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxMassFlow;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumMassFlowRate")) {
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinMassFlow;
        } else {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, R"(..Valid values are "Temperature","MaximumTemperature","MinimumTemperature",)");
            ShowContinueError(state, R"(     "HumidityRatio","MaximumHumidityRatio","MinimumHumidityRatio","MassFlowRate",)");
            ShowContinueError(state, R"(     "MaximumMassFlowRate" or "MinimumMassFlowRate")");
            ErrorsFound = true;
        }

        state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).Sched = cAlphaArgs(3);
        state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
        if (state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr == 0) {
            if (lAlphaFieldBlanks(3)) {
                ShowSevereError(state, format("{}: {}=\"{}\", blank required field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..required field " + cAlphaFieldNames(3));
            } else {
                ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            }
            ErrorsFound = true;
        }
        state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodeListName = cAlphaArgs(4);
        NodeListError = false;
        GetNodeNums(state,
                    state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerScheduled,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(4));

        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::Scheduled;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Scheduled Setpoint Managers DUAL SETPOINT

    if (state.dataSetPointManager->NumDualSchSetPtMgrs > 0)
        state.dataSetPointManager->DualSchSetPtMgr.allocate(
            state.dataSetPointManager->NumDualSchSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:Scheduled:DualSetpoint";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumDualSchSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedHi = cAlphaArgs(3);
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedPtrHi = GetScheduleIndex(state, cAlphaArgs(3));
        if (state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedPtrHi == 0) {
            if (lAlphaFieldBlanks(3)) {
                ShowSevereError(state, format("{}: {}=\"{}\", blank required field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..required field " + cAlphaFieldNames(3));
            } else {
                ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            }
            ErrorsFound = true;
        }
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedLo = cAlphaArgs(4);
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedPtrLo = GetScheduleIndex(state, cAlphaArgs(4));
        if (state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedPtrLo == 0) {
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, format("{}: {}=\"{}\", blank required field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..required field " + cAlphaFieldNames(4));
            } else {
                ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            }
            ErrorsFound = true;
        }
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodeListName = cAlphaArgs(5);
        NodeListError = false;
        GetNodeNums(state,
                    state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerScheduledDualSetpoint,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(5));

        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SetPtHi = 0.0;
            state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SetPtLo = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else { // check getnodenums/nodelist
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::ScheduledDual;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Outside Air Setpoint Managers

    if (state.dataSetPointManager->NumOutAirSetPtMgrs > 0)
        state.dataSetPointManager->OutAirSetPtMgr.allocate(
            state.dataSetPointManager->NumOutAirSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:OutdoorAirReset";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOutAirSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumTemperature")) {
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxTemp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumTemperature")) {
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinTemp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLowSetPt1 = rNumericArgs(1);
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow1 = rNumericArgs(2);
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHighSetPt1 = rNumericArgs(3);
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh1 = rNumericArgs(4);
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodeListName = cAlphaArgs(3);
        if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh1 < state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow1) {
            ShowWarningError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(4),
                                     state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh1,
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow1));
        }
        // Get optional input: schedule and 2nd reset rule
        if (NumAlphas == 4 && NumNums == 8) {
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).Sched = cAlphaArgs(4);
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
            // Schedule is optional here, so no check on SchedPtr
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLowSetPt2 = rNumericArgs(5);
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow2 = rNumericArgs(6);
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHighSetPt2 = rNumericArgs(7);
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh2 = rNumericArgs(8);
            if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh2 < state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow2) {
                ShowWarningError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state,
                                  format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                         cNumericFieldNames(8),
                                         state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh2,
                                         cNumericFieldNames(6),
                                         state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow2));
            }
        } else {
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).Sched = "";
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SchedPtr = 0;
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLowSetPt2 = 0.0;
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutLow2 = 0.0;
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHighSetPt2 = 0.0;
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).OutHigh2 = 0.0;
        }
        NodeListError = false;
        GetNodeNums(state,
                    state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerOutdoorAirReset,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3));
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::OutsideAir;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Single Zone Reheat Setpoint Managers

    if (state.dataSetPointManager->NumSZRhSetPtMgrs > 0)
        state.dataSetPointManager->SingZoneRhSetPtMgr.allocate(
            state.dataSetPointManager->NumSZRhSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SingleZone:Reheat";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZRhSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ControlZoneName = cAlphaArgs(3);
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MinSetTemp));
        }
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(4),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneReheat,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneInletNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(5),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneReheat,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(6),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneReheat,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(6)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        // get the actual zone number of the control zone
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ControlZoneNum =
            UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataHeatBal->Zone);
        if (state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ControlZoneNum == 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).SetPt = 0.0;

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZReheat;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Single Zone Heating Setpoint Managers

    if (state.dataSetPointManager->NumSZHtSetPtMgrs > 0)
        state.dataSetPointManager->SingZoneHtSetPtMgr.allocate(
            state.dataSetPointManager->NumSZHtSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SingleZone:Heating";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZHtSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ControlZoneName = cAlphaArgs(3);
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).MinSetTemp));
        }
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(4),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHeating,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneInletNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(5),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHeating,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(6),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHeating,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(6)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        // get the actual zone number of the control zone
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ControlZoneNum =
            UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataHeatBal->Zone);
        if (state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ControlZoneNum == 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).SetPt = 0.0;

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZHeating;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Single Zone Cooling Setpoint Managers

    if (state.dataSetPointManager->NumSZClSetPtMgrs > 0)
        state.dataSetPointManager->SingZoneClSetPtMgr.allocate(
            state.dataSetPointManager->NumSZClSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SingleZone:Cooling";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ControlZoneName = cAlphaArgs(3);
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MinSetTemp));
        }
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(4),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneCooling,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneInletNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(5),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneCooling,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(6),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneCooling,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(6)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        // get the actual zone number of the control zone
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ControlZoneNum =
            UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataHeatBal->Zone);
        if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ControlZoneNum == 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).SetPt = 0.0;

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZCooling;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Single Zone Minimum Humidity Setpoint Managers

    if (state.dataSetPointManager->NumSZMinHumSetPtMgrs > 0)
        state.dataSetPointManager->SZMinHumSetPtMgr.allocate(state.dataSetPointManager->NumSZMinHumSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Minimum";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMinHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ctrlVarType = "MinimumHumidityRatio";
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinHumRat;

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(2),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHumidityMinimum,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(4)); // nodes whose min humidity ratio will be set
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        ErrInList = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    ErrInList,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHumidityMinimum,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::Sensor,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // nodes of zones whose humidity is being controlled
        if (ErrInList) {
            ErrorsFound = true;
        }
        NumZones = NumNodes;
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumZones = NumZones;
        // only allow one control zone for now
        if (NumNodes > 1) {
            ShowSevereError(state, format("{}: {}=\"{}\", entered nodelist.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ShowContinueError(state, "..only one control zone is allowed.");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNodes.allocate(NumZones);
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNum.allocate(NumZones);
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlZoneNum.allocate(NumZones);

        for (ZoneNum = 1; ZoneNum <= NumZones; ++ZoneNum) {
            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNodes(ZoneNum) = NodeNums(ZoneNum);
            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNum(ZoneNum) = 0;
            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlZoneNum(ZoneNum) = 0;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZMinHum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Single Zone Maximum Humidity Setpoint Managers

    if (state.dataSetPointManager->NumSZMaxHumSetPtMgrs > 0)
        state.dataSetPointManager->SZMaxHumSetPtMgr.allocate(state.dataSetPointManager->NumSZMaxHumSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Maximum";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ctrlVarType = "MaximumHumidityRatio";
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxHumRat;

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(2),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHumidityMaximum,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(2)); // nodes whose max humidity ratio will be set
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        ErrInList = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    ErrInList,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneHumidityMaximum,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::Sensor,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // nodes of zones whose humidity is being controlled
        if (ErrInList) {
            ErrorsFound = true;
        }
        NumZones = NumNodes;
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumZones = NumZones;
        // only allow one control zone for now
        if (NumNodes > 1) {
            ShowSevereError(state, format("{}: {}=\"{}\", entered nodelist.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
            ShowContinueError(state, "..only one control zone is allowed.");
            ErrorsFound = true;
        }
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNodes.allocate(NumZones);
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNum.allocate(NumZones);
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlZoneNum.allocate(NumZones);

        for (ZoneNum = 1; ZoneNum <= NumZones; ++ZoneNum) {
            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNodes(ZoneNum) = NodeNums(ZoneNum);
            //   Actual zone node and controlled zone numbers set in Init subroutine
            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNum(ZoneNum) = 0;
            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlZoneNum(ZoneNum) = 0;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZMaxHum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Mixed Air Setpoint Managers

    if (state.dataSetPointManager->NumMixedAirSetPtMgrs > 0)
        state.dataSetPointManager->MixedAirSetPtMgr.allocate(state.dataSetPointManager->NumMixedAirSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:MixedAir";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMixedAirSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(3),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerMixedAir,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(4),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerMixedAir,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(5),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerMixedAir,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(6),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMixedAir,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(6)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        Found = FindNumberInList(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode,
                                 state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes,
                                 state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes);
        if (Found > 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", reference node.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            if (state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes > 1) {
                ShowContinueError(state, "..Reference Node is the same as one of the nodes in SetPoint NodeList");
            } else {
                ShowContinueError(state, "..Reference Node is the same as the SetPoint Node");
            }
            ShowContinueError(state,
                              "Reference Node Name=\"" +
                                  state.dataLoopNodes->NodeID(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode) + "\".");
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs;

        if (NumAlphas > 7) {
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CoolCoilInNode =
                GetOnlySingleNode(state,
                                  cAlphaArgs(7),
                                  ErrorsFound,
                                  DataLoopNode::ConnectionObjectType::SetpointManagerMixedAir,
                                  cAlphaArgs(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::ConnectionType::Sensor,
                                  NodeInputManager::CompFluidStream::Primary,
                                  ObjectIsNotParent);
            state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CoolCoilOutNode =
                GetOnlySingleNode(state,
                                  cAlphaArgs(8),
                                  ErrorsFound,
                                  DataLoopNode::ConnectionObjectType::SetpointManagerMixedAir,
                                  cAlphaArgs(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::ConnectionType::Sensor,
                                  NodeInputManager::CompFluidStream::Primary,
                                  ObjectIsNotParent);
            if (NumNums == 1) {
                state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).MinCoolCoilOutTemp = rNumericArgs(1);
            }
        }

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MixedAir;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).RefNode = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode;
    }

    // Input the Outside Air Pretreat Setpoint Managers

    if (state.dataSetPointManager->NumOAPretreatSetPtMgrs > 0)
        state.dataSetPointManager->OAPretreatSetPtMgr.allocate(state.dataSetPointManager->NumOAPretreatSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:OutdoorAirPretreat";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOAPretreatSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);

        // setup program flow control integers.
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode = static_cast<CtrlVarType>(getEnumerationValue(
            controlTypeNameUC, UtilityRoutines::MakeUPPERCase(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ctrlVarType)));
        if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Invalid) {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, R"(..Valid values are "Temperature","HumidityRatio","MaximumHumidityRatio" or "MinimumHumidityRatio".)");
            ErrorsFound = true;
        }

        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetTemp));
        }
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetHumRat = rNumericArgs(3);
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetHumRat = rNumericArgs(4);
        if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetHumRat <
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetHumRat) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(4),
                                     state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetHumRat,
                                     cNumericFieldNames(3),
                                     state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetHumRat));
        }

        // Because a zero humidity ratio setpoint is a special value indicating "off" or "no load"
        // must not allow MinSetHumRat or MaxSetHumRat to be <=0.0
        if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetHumRat <= 0.0) {
            ShowWarningError(state, format("{}: {}=\"{}\", invalid value.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "Minimum setpoint humidity ratio <=0.0, resetting to 0.00001");
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MinSetHumRat = 0.00001;
        }
        if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetHumRat <= 0.0) {
            ShowWarningError(state, format("{}: {}=\"{}\", invalid value.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "Maximum setpoint humidity ratio <=0.0, resetting to 0.00001");
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MaxSetHumRat = 0.00001;
        }

        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(3),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerOutdoorAirPretreat,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(4),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerOutdoorAirPretreat,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(5),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerOutdoorAirPretreat,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode =
            GetOnlySingleNode(state,
                              cAlphaArgs(6),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerOutdoorAirPretreat,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(7),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerOutdoorAirPretreat,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(7)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        Found = FindNumberInList(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode,
                                 state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlNodes,
                                 state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).NumCtrlNodes);
        if (Found > 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", reference node.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).NumCtrlNodes > 1) {
                ShowContinueError(state, "..Reference Node is the same as one of the nodes in SetPoint NodeList");
            } else {
                ShowContinueError(state, "..Reference Node is the same as the SetPoint Node");
            }
            ShowContinueError(state,
                              "Reference Node Name=\"" +
                                  state.dataLoopNodes->NodeID(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode) + "\".");
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::OutsideAirPretreat;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Warmest Setpoint Managers

    if (state.dataSetPointManager->NumWarmestSetPtMgrs > 0)
        state.dataSetPointManager->WarmestSetPtMgr.allocate(state.dataSetPointManager->NumWarmestSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:Warmest";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(3);
        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).MaxSetTemp < state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        if (UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)) == controlTypeNameUC[static_cast<int>(CtrlVarType::MaxTemp)]) {
            state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).Strategy = SupplyFlowTempStrategy::MaxTemp;
        } else {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            ShowContinueError(state, "..Valid value is \"MaximumTemperature\".");
            ErrorsFound = true;
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(5),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerWarmest,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(5)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::Warmest;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Coldest Setpoint Managers

    if (state.dataSetPointManager->NumColdestSetPtMgrs > 0)
        state.dataSetPointManager->ColdestSetPtMgr.allocate(state.dataSetPointManager->NumColdestSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:Coldest";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumColdestSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(3);
        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).MaxSetTemp < state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        if (UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)) == controlTypeNameUC[static_cast<int>(CtrlVarType::MinTemp)]) {
            state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).Strategy = SupplyFlowTempStrategy::MinTemp;
        } else {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            ShowContinueError(state, "..Valid value is \"MinimumTemperature\".");
            ErrorsFound = true;
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(5),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerColdest,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(5)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::Coldest;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Warmest Temp Flow Setpoint Managers

    if (state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow > 0)
        state.dataSetPointManager->WarmestSetPtMgrTempFlow.allocate(state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:WarmestTemperatureFlow";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopName = cAlphaArgs(3);
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        if (state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MinSetTemp));
        }
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MinTurndown = rNumericArgs(3);
        if (state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MinTurndown >= 0.8) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.2R}] is greater than 0.8;",
                                     cNumericFieldNames(3),
                                     state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).MinTurndown));
            ShowContinueError(state, "...typical values for " + cNumericFieldNames(3) + " are less than 0.8.");
        }

        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Strategy =
            static_cast<ControlStrategy>(getEnumerationValue(strategyNamesUC, UtilityRoutines::MakeUPPERCase(cAlphaArgs(4))));
        if (state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Strategy == ControlStrategy::Invalid) {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            ShowContinueError(state, R"(..Valid values are "TemperatureFirst" or "FlowFirst".)");
            ErrorsFound = true;
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(5),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerWarmestTemperatureFlow,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(5)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::WarmestTempFlow;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Return Air Bypass Flow Setpoint Managers

    if (state.dataSetPointManager->NumRABFlowSetPtMgrs > 0)
        state.dataSetPointManager->RABFlowSetPtMgr.allocate(state.dataSetPointManager->NumRABFlowSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:ReturnAirBypassFlow";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumRABFlowSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).NumCtrlNodes = 1;
        NumNodesCtrld = 1;

        if (UtilityRoutines::SameString(state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).ctrlVarType, "Flow")) {
            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MassFlow;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid value is \"Temperature\".");
            ErrorsFound = true;
        }
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(3);
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).Sched = cAlphaArgs(4);
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
        if (state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).SchedPtr == 0) {
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, format("{}: {}=\"{}\", bland required field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..required field " + cAlphaFieldNames(4));
            } else {
                ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            }
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow;

        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AllSetPtMgrIndex = AllSetPtMgrNum;
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlNodes = 0;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
        // need to reset this to the control node (RABSplitOutNode) in Init, will be 0 here
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlNodes;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::RAB;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the MultiZone Average Cooling Setpoint Managers
    if (state.dataSetPointManager->NumMZClgAverageSetPtMgrs > 0)
        state.dataSetPointManager->MZAverageCoolingSetPtMgr.allocate(state.dataSetPointManager->NumMZClgAverageSetPtMgrs);

    // Input the data for each setpoint manager
    cCurrentModuleObject = "SetpointManager:MultiZone:Cooling:Average";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZClgAverageSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(2);
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).ctrlVarType = "Temperature";
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;

        if (state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMultiZoneCoolingAverage,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MZCoolingAverage;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the MultiZone Average Heating Setpoint Managers
    if (state.dataSetPointManager->NumMZHtgAverageSetPtMgrs > 0)
        state.dataSetPointManager->MZAverageHeatingSetPtMgr.allocate(state.dataSetPointManager->NumMZHtgAverageSetPtMgrs);

    // Input the data for each setpoint manager
    cCurrentModuleObject = "SetpointManager:MultiZone:Heating:Average";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(2);
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(1);
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).ctrlVarType = "Temperature";
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;

        if (state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMultiZoneHeatingAverage,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MZHeatingAverage;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the MultiZone Average Minimum Humidity Setpoint Managers
    if (state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs > 0)
        state.dataSetPointManager->MZAverageMinHumSetPtMgr.allocate(state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs);

    // Input the data for each setpoint manager
    cCurrentModuleObject = "SetpointManager:MultiZone:MinimumHumidity:Average";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(2);
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).MinSetHum = rNumericArgs(1);
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).MaxSetHum = rNumericArgs(2);
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).ctrlVarType = "MinimumHumidityRatio";
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinHumRat;

        if (state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).MaxSetHum <
            state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).MinSetHum) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.3R}] is less than {}=[{:.3R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).MaxSetHum,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).MinSetHum));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMultiZoneMinimumHumidityAverage,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MZMinHumAverage;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the MultiZone Average Maximum Humidity SetPoint Managers
    if (state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs > 0)
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr.allocate(state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs);

    // Input the data for each setpoint manager
    cCurrentModuleObject = "SetpointManager:MultiZone:MaximumHumidity:Average";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(2);
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).MinSetHum = rNumericArgs(1);
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).MaxSetHum = rNumericArgs(2);
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).ctrlVarType = "MaximumHumidityRatio";
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxHumRat;

        if (state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).MaxSetHum <
            state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).MinSetHum) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.3R}] is less than {}=[{:.3R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).MaxSetHum,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).MinSetHum));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMultiZoneMaximumHumidityAverage,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MZMaxHumAverage;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Multizone Minimum Humidity Ratio SetPoint Managers
    if (state.dataSetPointManager->NumMZMinHumSetPtMgrs > 0)
        state.dataSetPointManager->MZMinHumSetPtMgr.allocate(state.dataSetPointManager->NumMZMinHumSetPtMgrs);

    // Input the data for each setpoint manager
    cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Minimum";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMinHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(2);
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).MinSetHum = rNumericArgs(1);
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).MaxSetHum = rNumericArgs(2);
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).ctrlVarType = "MinimumHumidityRatio";
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinHumRat;

        if (state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).MaxSetHum < state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).MinSetHum) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.3R}] is less than {}=[{:.3R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).MaxSetHum,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).MinSetHum));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMultiZoneHumidityMinimum,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MZMinHum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Multizone Maximum Humidity Ratio SetPoint Managers
    if (state.dataSetPointManager->NumMZMaxHumSetPtMgrs > 0)
        state.dataSetPointManager->MZMaxHumSetPtMgr.allocate(state.dataSetPointManager->NumMZMaxHumSetPtMgrs);

    // Input the data for each setpoint manager
    cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Maximum";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopName = cAlphaArgs(2);
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum = 0;
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).MinSetHum = rNumericArgs(1);
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).MaxSetHum = rNumericArgs(2);
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).ctrlVarType = "MaximumHumidityRatio";
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxHumRat;

        if (state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).MaxSetHum < state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).MinSetHum) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.3R}] is less than {}=[{:.3R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).MaxSetHum,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).MinSetHum));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::SetpointManagerMultiZoneHumidityMaximum,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::MZMaxHum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Follow Outdoor Air Temperature Setpoint Managers

    if (state.dataSetPointManager->NumFollowOATempSetPtMgrs > 0)
        state.dataSetPointManager->FollowOATempSetPtMgr.allocate(state.dataSetPointManager->NumFollowOATempSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:FollowOutdoorAirTemperature";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowOATempSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumTemperature")) {
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxTemp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumTemperature")) {
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinTemp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, R"(..Valid values are "Temperature","MaximumTemperature" or "MinimumTemperature".)");
            ErrorsFound = true;
        }
        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTempType = cAlphaArgs(3);
        if (UtilityRoutines::SameString(state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTempType, "OutdoorAirWetBulb")) {
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceTempType::WetBulb;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTempType, "OutdoorAirDryBulb")) {
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceTempType::DryBulb;
        } else {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ShowContinueError(state, R"(..Valid values are "OutdoorAirWetBulb" or "OutdoorAirDryBulb".)");
            ErrorsFound = true;
        }
        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).Offset = rNumericArgs(1);
        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(3);
        if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(3),
                                     state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(4),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerFollowOutdoorAirTemperature,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(4)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::FollowOATemp;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Follow System Node Temperature Setpoint Managers

    if (state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs > 0)
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr.allocate(state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:FollowSystemNodeTemperature";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumTemperature")) {
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxTemp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumTemperature")) {
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinTemp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, R"(..Valid values are "Temperature","MaximumTemperature" or "MinimumTemperature".)");
            ErrorsFound = true;
        }
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefNodeNum =
            GetOnlySingleNode(state,
                              cAlphaArgs(3),
                              ErrorsFound,
                              DataLoopNode::ConnectionObjectType::SetpointManagerFollowSystemNodeTemperature,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Blank,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent);
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTempType = cAlphaArgs(4);
        if (UtilityRoutines::SameString(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTempType, "NodeWetBulb")) {
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceTempType::WetBulb;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTempType, "NodeDryBulb")) {
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceTempType::DryBulb;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ShowContinueError(state, R"(..Valid values are "NodeWetBulb" or "NodeDryBulb".)");
            ErrorsFound = true;
        }
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).Offset = rNumericArgs(1);
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(3);
        if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(3),
                                     state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(5),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerFollowSystemNodeTemperature,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(5)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::FollowSysNodeTemp;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Ground Temperature Setpoint Managers

    if (state.dataSetPointManager->NumGroundTempSetPtMgrs > 0)
        state.dataSetPointManager->GroundTempSetPtMgr.allocate(state.dataSetPointManager->NumGroundTempSetPtMgrs);

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:FollowGroundTemperature";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumGroundTempSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).ctrlVarType, "MaximumTemperature")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxTemp;
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).ctrlVarType, "MinimumTemperature")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MinTemp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, R"(..Valid values are "Temperature","MaximumTemperature" or "MinimumTemperature".)");
            ErrorsFound = true;
        }
        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefGroundTempObjType = cAlphaArgs(3);
        if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefGroundTempObjType,
                                        "Site:GroundTemperature:BuildingSurface")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceGroundTempObjectType::BuildingSurface;
            if (state.dataSetPointManager->NoSurfaceGroundTempObjWarning) {
                if (!state.dataEnvrn->GroundTempObjInput) {
                    ShowWarningError(state,
                                     format("{}: {}=\"{}\" requires \"Site:GroundTemperature:BuildingSurface\" in the input..",
                                            RoutineName,
                                            cCurrentModuleObject,
                                            cAlphaArgs(1)));
                    ShowContinueError(state, format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp));
                }
                state.dataSetPointManager->NoSurfaceGroundTempObjWarning = false;
            }
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefGroundTempObjType,
                                               "Site:GroundTemperature:Shallow")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceGroundTempObjectType::Shallow;
            if (state.dataSetPointManager->NoShallowGroundTempObjWarning) {
                if (!state.dataEnvrn->GroundTemp_SurfaceObjInput) {
                    ShowWarningError(state,
                                     format("{}: {}=\"{}\" requires \"Site:GroundTemperature:Shallow\" in the input.",
                                            RoutineName,
                                            cCurrentModuleObject,
                                            cAlphaArgs(1)));
                    ShowContinueError(
                        state, format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp_Surface));
                }
                state.dataSetPointManager->NoShallowGroundTempObjWarning = false;
            }
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefGroundTempObjType,
                                               "Site:GroundTemperature:Deep")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceGroundTempObjectType::Deep;
            if (state.dataSetPointManager->NoDeepGroundTempObjWarning) {
                if (!state.dataEnvrn->GroundTemp_DeepObjInput) {
                    ShowWarningError(state,
                                     format("{}: {}=\"{}\" requires \"Site:GroundTemperature:Deep\" in the input.",
                                            RoutineName,
                                            cCurrentModuleObject,
                                            cAlphaArgs(1)));
                    ShowContinueError(state,
                                      format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp_Deep));
                }
                state.dataSetPointManager->NoDeepGroundTempObjWarning = false;
            }
        } else if (UtilityRoutines::SameString(state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefGroundTempObjType,
                                               "Site:GroundTemperature:FCfactorMethod")) {
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceGroundTempObjectType::FCFactorMethod;
            if (state.dataSetPointManager->NoFCGroundTempObjWarning) {
                if (!state.dataEnvrn->FCGroundTemps) {
                    ShowWarningError(state,
                                     format("{}: {}=\"{}\" requires \"Site:GroundTemperature:FCfactorMethod\" in the input.",
                                            RoutineName,
                                            cCurrentModuleObject,
                                            cAlphaArgs(1)));
                    ShowContinueError(state,
                                      format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTempFC));
                }
                state.dataSetPointManager->NoFCGroundTempObjWarning = false;
            }
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
            ShowContinueError(state, R"(..Valid values are "Site:GroundTemperature:BuildingSurface", "Site:GroundTemperature:Shallow",)");
            ShowContinueError(state, R"(     "Site:GroundTemperature:Deep" or "Site:GroundTemperature:FCfactorMethod".)");
            ErrorsFound = true;
        }
        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).Offset = rNumericArgs(1);
        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).MaxSetTemp = rNumericArgs(2);
        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).MinSetTemp = rNumericArgs(3);
        if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).MaxSetTemp <
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).MinSetTemp) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).MaxSetTemp,
                                     cNumericFieldNames(3),
                                     state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).MinSetTemp));
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(4),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerFollowGroundTemperature,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(4)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::GroundTemp;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum) {
        SetupOutputVariable(state,
                            "Setpoint Manager Warmest Temperature Critical Zone Number",
                            OutputProcessor::Unit::None,
                            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CritZoneNum,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name);
        SetupOutputVariable(state,
                            "Setpoint Manager Warmest Temperature Turndown Flow Fraction",
                            OutputProcessor::Unit::None,
                            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Turndown,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name);
    }

    // Input the Condenser Entering Set Point Managers

    if (state.dataSetPointManager->NumCondEntSetPtMgrs > 0)
        state.dataSetPointManager->CondEntSetPtMgr.allocate(
            state.dataSetPointManager->NumCondEntSetPtMgrs); // Allocate the Set Point Manager input data array

    // Input the data for each Set Point Manager
    cCurrentModuleObject = "SetpointManager:CondenserEnteringReset";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumCondEntSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, " found invalid control type of " + cAlphaArgs(2) + " in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
            ErrorsFound = true;
        }
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CondEntTempSched = cAlphaArgs(3);
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CondEntTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).MinTwrWbCurve = GetCurveIndex(state, cAlphaArgs(4));
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).MinOaWbCurve = GetCurveIndex(state, cAlphaArgs(5));
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).OptCondEntCurve = GetCurveIndex(state, cAlphaArgs(6));
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).MinimumLiftTD = rNumericArgs(1);
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).MaxCondEntTemp = rNumericArgs(2);
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).TowerDsnInletAirWetBulb = rNumericArgs(3);
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodeListName = cAlphaArgs(7);
        if (state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).MaxCondEntTemp <
            state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).TowerDsnInletAirWetBulb) {
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).MaxCondEntTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).TowerDsnInletAirWetBulb));
        }

        NodeListError = false;
        GetNodeNums(state,
                    state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerCondenserEnteringReset,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(7));
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::CondEntReset;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the Ideal Condenser Entering Set Point Managers

    // Allocate the Set Point Manager input data array
    if (state.dataSetPointManager->NumIdealCondEntSetPtMgrs > 0)
        state.dataSetPointManager->IdealCondEntSetPtMgr.allocate(state.dataSetPointManager->NumIdealCondEntSetPtMgrs);

    // Input the data for each Set Point Manager
    cCurrentModuleObject = "SetpointManager:CondenserEnteringReset:Ideal";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumIdealCondEntSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).ctrlVarType = cAlphaArgs(2);
        if (UtilityRoutines::SameString(state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).ctrlVarType, "Temperature")) {
            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        } else {
            ShowSevereError(state, " found invalid control type of " + cAlphaArgs(2) + " in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
            ErrorsFound = true;
        }
        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).MinimumLiftTD = rNumericArgs(1);
        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).MaxCondEntTemp = rNumericArgs(2);
        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodeListName = cAlphaArgs(3);

        NodeListError = false;
        GetNodeNums(state,
                    state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerCondenserEnteringResetIdeal,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3));
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes = state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::IdealCondEntReset;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    if (state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs > 0)
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr.allocate(state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs);

    cCurrentModuleObject = "SetpointManager:SingleZone:OneStageCooling";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).ctrlVarType = "Temperature";
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOnTemp = rNumericArgs(1);
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOffTemp = rNumericArgs(2);

        if (state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOffTemp <
            state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOnTemp) {
            // throw warning, off must be warmer than on
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOffTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOnTemp));
        }

        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).ControlZoneName = cAlphaArgs(2);
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).ZoneNodeNum = GetSystemNodeNumberForZone(state, cAlphaArgs(2));
        // get the actual zone number of the control zone
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).ControlZoneNum =
            UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).ControlZoneNum == 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        } else {
            if (allocated(state.dataZoneCtrls->StageZoneLogic)) {
                if (!state.dataZoneCtrls->StageZoneLogic(state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).ControlZoneNum)) {
                    ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                    ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ShowContinueError(state, "Zone thermostat must use ZoneControl:Thermostat:StagedDualSetpoint.");
                    ErrorsFound = true;
                }
            }
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneOneStageCooling,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
                         state.dataSetPointManager->NumIdealCondEntSetPtMgrs;
        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZOneStageCooling;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    if (state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs > 0)
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr.allocate(state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs);

    cCurrentModuleObject = "SetpointManager:SingleZone:OneStageHeating";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).ctrlVarType = "Temperature";
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOnTemp = rNumericArgs(1);
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOffTemp = rNumericArgs(2);

        if (state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOffTemp >
            state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOnTemp) {
            // throw warning, off must be cooler than on
            ShowWarningError(state, format("{}: {}=\"{}\",", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(2),
                                     state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOnTemp,
                                     cNumericFieldNames(1),
                                     state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOffTemp));
        }

        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).ControlZoneName = cAlphaArgs(2);
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).ZoneNodeNum = GetSystemNodeNumberForZone(state, cAlphaArgs(2));
        // get the actual zone number of the control zone
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).ControlZoneNum =
            UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).ControlZoneNum == 0) {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        } else {
            if (allocated(state.dataZoneCtrls->StageZoneLogic)) {
                if (!state.dataZoneCtrls->StageZoneLogic(state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).ControlZoneNum)) {
                    ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                    ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ShowContinueError(state, "Zone thermostat must use ZoneControl:Thermostat:StagedDualSetpoint.");
                    ErrorsFound = true;
                }
            }
        }

        NodeListError = false;
        GetNodeNums(state,
                    cAlphaArgs(3),
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSingleZoneOneStageHeating,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(3)); // setpoint nodes
        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes = NumNodesCtrld;
            state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
                         state.dataSetPointManager->NumIdealCondEntSetPtMgrs + state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs;
        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SZOneStageHeating;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    if (state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs > 0)
        state.dataSetPointManager->ReturnWaterResetChWSetPtMgr.allocate(state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs);

    cCurrentModuleObject = "SetpointManager:ReturnTemperature:ChilledWater";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs; ++SetPtMgrNum) {

        // get the object inputs
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);

        // process the sense and actuate nodes
        bool errFlag = false;
        state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).supplyNodeIndex =
            GetOnlySingleNode(state,
                              cAlphaArgs(2),
                              errFlag,
                              DataLoopNode::ConnectionObjectType::SetpointManagerReturnTemperatureChilledWater,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Blank,
                              DataLoopNode::ConnectionType::SetPoint,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFieldNames(2)); // setpoint nodes
        state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).returnNodeIndex =
            GetOnlySingleNode(state,
                              cAlphaArgs(3),
                              errFlag,
                              DataLoopNode::ConnectionObjectType::SetpointManagerReturnTemperatureChilledWater,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Blank,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFieldNames(3)); // setpoint nodes

        // process the setpoint inputs
        state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).minimumChilledWaterSetpoint = rNumericArgs(1);
        state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).maximumChilledWaterSetpoint = rNumericArgs(2);

        // process the return temperature type/value
        std::string returnType(cAlphaArgs(4));
        if (UtilityRoutines::SameString(returnType, "SCHEDULED")) {
            state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).returnTemperatureScheduleIndex =
                GetScheduleIndex(state, cAlphaArgs(5));
            if (state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).returnTemperatureScheduleIndex == 0) {
                ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(returnType, "CONSTANT")) {
            state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).returnTemperatureConstantTarget = rNumericArgs(3);
        } else if (UtilityRoutines::SameString(returnType, "RETURNTEMPERATURESETPOINT")) {
            state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).useReturnTempSetpoint = true;
        } else {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            ErrorsFound = true;
        }

        // setup the "base" class
        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
                         state.dataSetPointManager->NumIdealCondEntSetPtMgrs + state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs +
                         state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(1);
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes(1) =
            state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).supplyNodeIndex;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::ReturnWaterResetChW;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = 1;
    }

    if (state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs > 0)
        state.dataSetPointManager->ReturnWaterResetHWSetPtMgr.allocate(state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs);

    cCurrentModuleObject = "SetpointManager:ReturnTemperature:HotWater";
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs; ++SetPtMgrNum) {

        // get the object inputs
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).Name = cAlphaArgs(1);

        // process the sense and actuate nodes
        bool errFlag = false;
        state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).supplyNodeIndex =
            GetOnlySingleNode(state,
                              cAlphaArgs(2),
                              errFlag,
                              DataLoopNode::ConnectionObjectType::SetpointManagerReturnTemperatureHotWater,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Blank,
                              DataLoopNode::ConnectionType::SetPoint,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFieldNames(2)); // setpoint nodes
        state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).returnNodeIndex =
            GetOnlySingleNode(state,
                              cAlphaArgs(3),
                              errFlag,
                              DataLoopNode::ConnectionObjectType::SetpointManagerReturnTemperatureHotWater,
                              cAlphaArgs(1),
                              DataLoopNode::NodeFluidType::Blank,
                              DataLoopNode::ConnectionType::Sensor,
                              NodeInputManager::CompFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFieldNames(3)); // setpoint nodes

        // process the setpoint inputs
        state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).minimumHotWaterSetpoint = rNumericArgs(1);
        state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).maximumHotWaterSetpoint = rNumericArgs(2);

        // process the return temperature type/value
        std::string returnType(cAlphaArgs(4));
        if (UtilityRoutines::SameString(returnType, "SCHEDULED")) {
            state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).returnTemperatureScheduleIndex =
                GetScheduleIndex(state, cAlphaArgs(5));
            if (state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).returnTemperatureScheduleIndex == 0) {
                ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
                ShowContinueError(state, "..invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(returnType, "CONSTANT")) {
            state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).returnTemperatureConstantTarget = rNumericArgs(3);
        } else if (UtilityRoutines::SameString(returnType, "RETURNTEMPERATURESETPOINT")) {
            state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).useReturnTempSetpoint = true;
        } else {
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
            ErrorsFound = true;
        }

        // setup the "base" class
        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
                         state.dataSetPointManager->NumIdealCondEntSetPtMgrs + state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs +
                         state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs + state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(1);
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes(1) =
            state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).supplyNodeIndex;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::ReturnWaterResetHW;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode = CtrlVarType::Temp;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes = 1;
    }

    // Input the System Node Reset Temperature Setpoint Managers

    if (state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs > 0 || state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs > 0)
        state.dataSetPointManager->SystemNodeResetSetPtMgr.allocate(
            state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs +
            state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs); // Allocate the Setpoint Manager input data array

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SystemNodeReset:Temperature";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        auto &setpointManager = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum);

        setpointManager.Name = cAlphaArgs(1);
        setpointManager.ctrlVarType = cAlphaArgs(2);

        int typeNum = getEnumerationValue(SetPointManager::controlTypeNameUC, cAlphaArgs(2));
        setpointManager.CtrlTypeMode = static_cast<SetPointManager::CtrlVarType>(typeNum);
        if (setpointManager.CtrlTypeMode == SetPointManager::CtrlVarType::Invalid) {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid values are \"Temperature\", \"MaximumTemperature\", or \"MinimumTemperature\".");
            ErrorsFound = true;
        }

        setpointManager.SpAtLowRef = rNumericArgs(1);
        setpointManager.SpAtHighRef = rNumericArgs(2);
        setpointManager.LowRef = rNumericArgs(3);
        setpointManager.HighRef = rNumericArgs(4);

        setpointManager.RefNodeNum = GetOnlySingleNode(state,
                                                       cAlphaArgs(3),
                                                       ErrorsFound,
                                                       DataLoopNode::ConnectionObjectType::SetpointManagerSystemNodeResetTemperature,
                                                       cAlphaArgs(1),
                                                       DataLoopNode::NodeFluidType::Blank,
                                                       DataLoopNode::ConnectionType::Sensor,
                                                       NodeInputManager::CompFluidStream::Primary,
                                                       ObjectIsNotParent);

        setpointManager.CtrlNodeListName = cAlphaArgs(4);
        if (setpointManager.HighRef < setpointManager.LowRef) {
            ShowWarningError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(3),
                                     setpointManager.LowRef,
                                     cNumericFieldNames(4),
                                     setpointManager.HighRef));
        }

        NodeListError = false;

        GetNodeNums(state,
                    setpointManager.CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSystemNodeResetTemperature,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(4));

        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            setpointManager.CtrlNodes.allocate(NumNodesCtrld);
            setpointManager.NumCtrlNodes = NumNodesCtrld;
            setpointManager.SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                setpointManager.CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
                         state.dataSetPointManager->NumIdealCondEntSetPtMgrs + state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs +
                         state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs + state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs +
                         state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SystemNodeResetTemp;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).NumCtrlNodes;
    }

    // Input the System Node Reset Humidity Setpoint Managers

    // Input the data for each Setpoint Manager
    cCurrentModuleObject = "SetpointManager:SystemNodeReset:Humidity";

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SetPtMgrNum,
                                                                 cAlphaArgs,
                                                                 NumAlphas,
                                                                 rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        int SetPtRstMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs;

        auto &setpointManager = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtRstMgrNum);

        setpointManager.Name = cAlphaArgs(1);
        setpointManager.ctrlVarType = cAlphaArgs(2);

        int typeNum = getEnumerationValue(SetPointManager::controlTypeNameUC, cAlphaArgs(2));
        setpointManager.CtrlTypeMode = static_cast<SetPointManager::CtrlVarType>(typeNum);
        if (setpointManager.CtrlTypeMode == SetPointManager::CtrlVarType::Invalid) {
            // should not come here if idd type choice and key list is working
            ShowSevereError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state, "..invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
            ShowContinueError(state, "..Valid values are \"HumidityRatio\", \"MaximumHumidityRatio\", or \"MinimumHumidityRatio\".");
            ErrorsFound = true;
        }

        setpointManager.SpAtLowRef = rNumericArgs(1);
        setpointManager.SpAtHighRef = rNumericArgs(2);
        setpointManager.LowRef = rNumericArgs(3);
        setpointManager.HighRef = rNumericArgs(4);

        setpointManager.RefNodeNum = GetOnlySingleNode(state,
                                                       cAlphaArgs(3),
                                                       ErrorsFound,
                                                       DataLoopNode::ConnectionObjectType::SetpointManagerSystemNodeResetHumidity,
                                                       cAlphaArgs(1),
                                                       DataLoopNode::NodeFluidType::Blank,
                                                       DataLoopNode::ConnectionType::Sensor,
                                                       NodeInputManager::CompFluidStream::Primary,
                                                       ObjectIsNotParent);

        setpointManager.CtrlNodeListName = cAlphaArgs(4);
        if (setpointManager.HighRef < setpointManager.LowRef) {
            ShowWarningError(state, format("{}: {}=\"{}\", invalid field.", RoutineName, cCurrentModuleObject, cAlphaArgs(1)));
            ShowContinueError(state,
                              format("...{}=[{:.1R}] is less than {}=[{:.1R}].",
                                     cNumericFieldNames(3),
                                     setpointManager.LowRef,
                                     cNumericFieldNames(4),
                                     setpointManager.HighRef));
        }

        NodeListError = false;

        GetNodeNums(state,
                    setpointManager.CtrlNodeListName,
                    NumNodes,
                    NodeNums,
                    NodeListError,
                    DataLoopNode::NodeFluidType::Blank,
                    DataLoopNode::ConnectionObjectType::SetpointManagerSystemNodeResetHumidity,
                    cAlphaArgs(1),
                    DataLoopNode::ConnectionType::SetPoint,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsNotParent,
                    _,
                    cAlphaFieldNames(4));

        if (!NodeListError) {
            NumNodesCtrld = NumNodes;
            setpointManager.CtrlNodes.allocate(NumNodesCtrld);
            setpointManager.NumCtrlNodes = NumNodesCtrld;
            setpointManager.SetPt = 0.0;

            for (CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum) {
                setpointManager.CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum);
            }
        } else {
            ErrorsFound = true;
        }

        AllSetPtMgrNum = SetPtMgrNum + state.dataSetPointManager->NumSchSetPtMgrs + state.dataSetPointManager->NumDualSchSetPtMgrs +
                         state.dataSetPointManager->NumOutAirSetPtMgrs + state.dataSetPointManager->NumSZRhSetPtMgrs +
                         state.dataSetPointManager->NumSZHtSetPtMgrs + state.dataSetPointManager->NumSZClSetPtMgrs +
                         state.dataSetPointManager->NumSZMinHumSetPtMgrs + state.dataSetPointManager->NumSZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMixedAirSetPtMgrs + state.dataSetPointManager->NumOAPretreatSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrs + state.dataSetPointManager->NumColdestSetPtMgrs +
                         state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow + state.dataSetPointManager->NumRABFlowSetPtMgrs +
                         state.dataSetPointManager->NumMZClgAverageSetPtMgrs + state.dataSetPointManager->NumMZHtgAverageSetPtMgrs +
                         state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs + state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumMZMinHumSetPtMgrs + state.dataSetPointManager->NumMZMaxHumSetPtMgrs +
                         state.dataSetPointManager->NumFollowOATempSetPtMgrs + state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs +
                         state.dataSetPointManager->NumGroundTempSetPtMgrs + state.dataSetPointManager->NumCondEntSetPtMgrs +
                         state.dataSetPointManager->NumIdealCondEntSetPtMgrs + state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs +
                         state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs + state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs +
                         state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs + state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs;

        if (!NodeListError) {
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes.allocate(NumNodesCtrld);
            state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlNodes =
                state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtRstMgrNum).CtrlNodes;
        }
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).Name = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).Name;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMType = SetPointManagerType::SystemNodeResetHum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).SPMIndex = SetPtMgrNum;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).CtrlTypeMode =
            state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtRstMgrNum).CtrlTypeMode;
        state.dataSetPointManager->AllSetPtMgr(AllSetPtMgrNum).NumCtrlNodes =
            state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtRstMgrNum).NumCtrlNodes;
    }

    cAlphaFieldNames.deallocate();
    cAlphaArgs.deallocate();
    lAlphaFieldBlanks.deallocate();
    cNumericFieldNames.deallocate();
    rNumericArgs.deallocate();
    lNumericFieldBlanks.deallocate();
}

void VerifySetPointManagers(EnergyPlusData &state, [[maybe_unused]] bool &ErrorsFound) // flag to denote node conflicts in input. !unused1208
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   July 2008
    //       MODIFIED       Rick Strand, Aug 2014 (removed deallocation of AllSetPtMgrs so ScheduledTES could also verify control nodes)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Check the SetPointManager data to eliminate conflicts.

    // METHODOLOGY EMPLOYED:
    // 1) Check for duplicate names in individual setpoint managers.
    // Control nodes = A B C D
    // Check A with B, C, and D
    // Check B with C and D
    // Check C with D
    // 2) Check for duplicate names in all other setpoint managers
    //    Verify setpoint managers use same control type (e.g. TEMP) and then check for duplicate nodes
    // SPM 1 - Control nodes A - D, SPM 2 - Control nodes E - H, SPM 3 - Control nodes I - L
    // If SPM 1 has same control type as SPM 2 and SPM 3 (e.g. all use SPM%CtrlTypeMode = CtrlVarType::Temp) then:
    // Check A with E-H and I-L
    // Check B with E-H and I-L
    // Check C with E-H and I-L
    // Check D with E-H and I-L
    // Then check SPM 2 nodes with SPM 3. Check E with I-L, F with I-L, etc.
    // 3) For SET POINT MANAGER:RETURN AIR BYPASS FLOW
    //    check for duplicate air loop names.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int SetPtMgrNum;      // Setpoint Manager index
    int TempSetPtMgrNum;  // Setpoint Manager index for warning messages
    int CtrldNodeNum;     // index of the items in the controlled node node list
    int TempCtrldNodeNum; // index of the items in the controlled node node list, used for warning messages

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumAllSetPtMgrs; ++SetPtMgrNum) {

        // check for duplicate nodes in each setpoint managers control node list (node lists of size 1 do not need verification)
        // issue warning only since duplicate node names within a setpoint manager does not cause a conflict (i.e., same
        // value written to node) but may indicate an error in the node name.
        for (CtrldNodeNum = 1; CtrldNodeNum <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes - 1; ++CtrldNodeNum) {
            for (TempCtrldNodeNum = CtrldNodeNum + 1; TempCtrldNodeNum <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes;
                 ++TempCtrldNodeNum) {
                if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) !=
                    state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(TempCtrldNodeNum))
                    continue;
                ShowWarningError(state,
                                 format("{} =\"{}\"",
                                        managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType)],
                                        state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).Name));
                ShowContinueError(state,
                                  format("...duplicate node specified = {}",
                                         state.dataLoopNodes->NodeID(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum))));
                ShowContinueError(state,
                                  format("...control type variable    = {}",
                                         controlTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode)]));
            }
        }

        // check for node conflicts in all other setpoint managers
        for (TempSetPtMgrNum = SetPtMgrNum + 1; TempSetPtMgrNum <= state.dataSetPointManager->NumAllSetPtMgrs; ++TempSetPtMgrNum) {

            if ((state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType == SetPointManagerType::RAB &&
                 state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).SPMType == SetPointManagerType::RAB)) {

                //     check the air loop name for duplicates in this SP manager type
                if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).AirLoopNum ==
                    state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).AirLoopNum) {
                    ShowWarningError(state,
                                     format("{}=\"{}\"",
                                            managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType)],
                                            state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).Name));
                    ShowContinueError(state, "...air loop name conflicts with another setpoint manager.");
                    ShowContinueError(state,
                                      format("...conflicting setpoint manager = {} \"{}\"",
                                             managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).SPMType)],
                                             state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).Name));
                    ShowContinueError(state, "...conflicting air loop name = " + state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).AirLoopName);
                    //        ErrorsFound=.TRUE.
                }

                //     check for duplicate control nodes
                if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode !=
                    state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).CtrlTypeMode)
                    continue;

                for (CtrldNodeNum = 1; CtrldNodeNum <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrldNodeNum) {
                    for (TempCtrldNodeNum = 1; TempCtrldNodeNum <= state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).NumCtrlNodes;
                         ++TempCtrldNodeNum) {
                        if ((state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) ==
                             state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).CtrlNodes(TempCtrldNodeNum)) &&
                            state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) != 0) {
                            ShowWarningError(state,
                                             format("{}=\"{}\"",
                                                    managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType)],
                                                    state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).Name));
                            ShowContinueError(state, "...setpoint node conflicts with another setpoint manager.");
                            ShowContinueError(
                                state,
                                format("...conflicting setpoint manager = {} \"{}\"",
                                       managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).SPMType)],
                                       state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).Name));
                            ShowContinueError(
                                state,
                                "...conflicting node name = " +
                                    state.dataLoopNodes->NodeID(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum)));
                            ShowContinueError(
                                state,
                                format("...control type variable = {}",
                                       controlTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode)]));
                            //            ErrorsFound=.TRUE.
                        }
                    }
                }

            } else { // not a RAB setpoint manager

                //     check just the control nodes for other types of SP managers
                if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode !=
                    state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).CtrlTypeMode)
                    continue;

                for (CtrldNodeNum = 1; CtrldNodeNum <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrldNodeNum) {
                    for (TempCtrldNodeNum = 1; TempCtrldNodeNum <= state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).NumCtrlNodes;
                         ++TempCtrldNodeNum) {

                        if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) !=
                            state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).CtrlNodes(TempCtrldNodeNum))
                            continue;

                        //         only warn if scheduled setpoint manager is setting mass flow rate on the same node used by RAB
                        if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType == SetPointManagerType::RAB ||
                            state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).SPMType == SetPointManagerType::RAB) {
                            ShowWarningError(state,
                                             format("{}=\"{}\"",
                                                    managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType)],
                                                    state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).Name));
                            ShowContinueError(state, "...setpoint node conflicts with another setpoint manager.");
                            ShowContinueError(
                                state,
                                format("...conflicting setpoint manager ={}:\"{}\"",
                                       managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).SPMType)],
                                       state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).Name));
                            ShowContinueError(
                                state,
                                "...conflicting node name = " +
                                    state.dataLoopNodes->NodeID(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum)));
                            ShowContinueError(
                                state,
                                format("...control type variable = {}",
                                       controlTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode)]));
                            ShowContinueError(state,
                                              "...return air bypass flow setpoint manager will have priority setting mass flow rate on this node.");
                        } else { // severe error for other SP manager types
                            ShowWarningError(state,
                                             format("{}=\"{}\"",
                                                    managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType)],
                                                    state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).Name));
                            ShowContinueError(state, "...setpoint node conflicts with another setpoint manager.");
                            ShowContinueError(
                                state,
                                format("...conflicting setpoint manager = {}:\"{}\"",
                                       managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).SPMType)],
                                       state.dataSetPointManager->AllSetPtMgr(TempSetPtMgrNum).Name));
                            ShowContinueError(
                                state,
                                "...conflicting node name = " +
                                    state.dataLoopNodes->NodeID(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum)));
                            ShowContinueError(
                                state,
                                format("...control type variable = {}",
                                       controlTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode)]));
                            //            ErrorsFound=.TRUE.
                        }
                    }
                }
            }

        } // DO TempSetPtMgrNum = SetPtMgrNum+1, AllSetPtMgrs

    } // DO SetPtMgrNum = 1, AllSetPtMgrs

    // Removed the following line for ScheduledTES control implementation
    // if ( allocated( AllSetPtMgr ) ) AllSetPtMgr.deallocate();
}

void InitSetPointManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   October 2000
    //       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
    //                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
    //                        Add new setpoint managers:
    //                          SET POINT MANAGER:SINGLE ZONE HEATING and
    //                          SET POINT MANAGER:SINGLE ZONE COOLING
    //                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
    //                        Work supported by ASHRAE research project 1254-RP
    //                      Haves Oct 2004
    //                      July 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers:
    //                          SetpointManager:MultiZone:Heating:Average
    //                          SetpointManager:MultiZone:Cooling:Average
    //                          SetpointManager:MultiZone:MinimumHumidity:Average
    //                          SetpointManager:MultiZone:MaximumHumidity:Average
    //                      Aug 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers:
    //                          SetpointManager:MultiZone:Humidity:Minimum
    //                          SetpointManager:MultiZone:Humidity:Maximum
    //                      Sep 2010 B.A. Nigusse, FSEC/UCF
    //                         Added control varibles for SetpointManage:Scheduled
    //                      Jan 2022 Wooyoung Jung, Jeremy Lerond and Jian Zhang, PNNL
    //                         Added new setpoint managers:
    //                          SetpointManager:SystemNodeReset:Temperature
    //                          SetpointManager:SystemNodeReset:Humidity
    //
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the Setpoint Manager objects.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    // Using/Aliasing
    using namespace DataPlant;
    using OutAirNodeManager::CheckOutAirNodeNumber;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int SetZoneNum;
    int ControlledZoneNum;
    int ZoneNode;
    int ZoneInletNode;
    int SetPtMgrNum;
    int ZoneIndex;
    int CtrlNodeIndex;
    int NodeNum;
    int AirLoopNum;
    int LoopNum;
    int LoopNum2;
    bool ErrorsFound(false);
    int ConZoneNum;
    int MixedAirNode;
    int BranchNum;
    int BranchNum2;
    int InletBranchNum;
    int CompNum;
    int CompNum2;
    std::string CompType;
    std::string cSetPointManagerType;
    int FanNodeIn;
    int FanNodeOut;
    int LoopInNode;
    int HStatZoneNum;
    bool HstatZoneFound;
    int ZonesCooledIndex; // Cooled zones index in an air loop
    int BranchNumPlantSide;
    int CompNumPlantSide;

    state.dataSetPointManager->ManagerOn = true;

    // One time initializations

    if (state.dataZoneEquip->ZoneEquipInputsFilled &&
        state.dataAirLoop->AirLoopInputsFilled) { // check that the zone equipment and air loop data has been read in

        if (state.dataSetPointManager->InitSetPointManagersOneTimeFlag) {

            // "SetpointManager:SingleZone:Heating"
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::SZHeating)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZHtSetPtMgrs; ++SetPtMgrNum) {
                ZoneInletNode = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneInletNodeNum;
                ZoneNode = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneNodeNum;
                // find the index in the ZoneEquipConfig array of the control zone (the one with the main or only thermostat)
                ConZoneNum = 0;
                for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                    if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode == ZoneNode) {
                        ConZoneNum = ControlledZoneNum;
                    }
                }
                if (ConZoneNum == 0) {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).Name +
                                        "\", Zone Node not found:");
                    ShowContinueError(state,
                                      "Node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneNodeNum) +
                                          "\", not found in any controlled Zone");
                    ErrorsFound = true;
                } else {
                    bool found = false;
                    for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).NumInletNodes; ++zoneInNode) {
                        if (state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneInletNodeNum ==
                            state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).InletNode(zoneInNode)) {
                            found = true;
                        }
                    }
                    if (!found) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).Name +
                                            "\", The zone inlet node of " +
                                            state.dataLoopNodes->NodeID(state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneInletNodeNum));
                        ShowContinueError(
                            state, "is not found in Zone = " + state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).ZoneName + ". Please check inputs.");
                        ErrorsFound = true;
                    }
                }
            }

            // "SetpointManager:SingleZone:Cooling"
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::SZCooling)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) {
                ZoneInletNode = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneInletNodeNum;
                ZoneNode = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneNodeNum;
                // find the index in the ZoneEquipConfig array of the control zone (the one with the main or only thermostat)
                ConZoneNum = 0;
                for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                    if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode == ZoneNode) {
                        ConZoneNum = ControlledZoneNum;
                    }
                }
                if (ConZoneNum == 0) {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).Name +
                                        "\", Zone Node not found:");
                    ShowContinueError(state,
                                      "Node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneNodeNum) +
                                          "\", not found in any controlled Zone");
                    ErrorsFound = true;
                } else {
                    bool found = false;
                    for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).NumInletNodes; ++zoneInNode) {
                        if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneInletNodeNum ==
                            state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).InletNode(zoneInNode)) {
                            found = true;
                        }
                    }
                    if (!found) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).Name +
                                            "\", The zone inlet node of " +
                                            state.dataLoopNodes->NodeID(state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneInletNodeNum));
                        ShowContinueError(
                            state, "is not found in Zone = " + state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).ZoneName + ". Please check inputs.");
                        ErrorsFound = true;
                    }
                }
            }

            // Minimum humidity setpoint managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::SZMinHum)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMinHumSetPtMgrs; ++SetPtMgrNum) {
                for (SetZoneNum = 1; SetZoneNum <= state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumZones; ++SetZoneNum) {
                    // set the actual and controlled zone numbers
                    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode ==
                            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNodes(SetZoneNum)) {
                            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlZoneNum(SetZoneNum) = ControlledZoneNum;
                            state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum) =
                                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
                            break;
                        }
                    }
                    // still need to validate...
                    if (state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlZoneNum(SetZoneNum) == 0) { // didn't find
                        ShowSevereError(
                            state, cSetPointManagerType + "=\"" + state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).Name + "\", invalid zone");
                        ShowContinueError(
                            state,
                            "could not find Controlled Zone=" +
                                state.dataHeatBal->Zone(state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum)).Name);
                        ErrorsFound = true;
                    } else {
                        // make sure humidity controlled zone
                        HstatZoneFound = false;
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum))
                                continue;
                            HstatZoneFound = true;
                            break;
                        }
                        if (!HstatZoneFound) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).Name +
                                                "\", invalid humidistat specification");
                            ShowContinueError(
                                state,
                                "could not locate Humidistat in Zone=" +
                                    state.dataHeatBal->Zone(state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum)).Name);
                            ErrorsFound = true;
                        }
                    }
                }
            }

            // Maximum humidity setpoint managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::SZMaxHum)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMaxHumSetPtMgrs; ++SetPtMgrNum) {
                for (SetZoneNum = 1; SetZoneNum <= state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumZones; ++SetZoneNum) {
                    // set the actual and controlled zone numbers
                    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode ==
                            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNodes(SetZoneNum)) {
                            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlZoneNum(SetZoneNum) = ControlledZoneNum;
                            state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum) =
                                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
                            break;
                        }
                    }
                    // still need to validate...
                    if (state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlZoneNum(SetZoneNum) == 0) { // didn't find
                        ShowSevereError(
                            state, cSetPointManagerType + "=\"" + state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).Name + "\", invalid zone");
                        ShowContinueError(
                            state,
                            "could not find Controlled Zone=" +
                                state.dataHeatBal->Zone(state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum)).Name);
                        ErrorsFound = true;
                    } else {
                        // make sure humidity controlled zone
                        HstatZoneFound = false;
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum))
                                continue;
                            HstatZoneFound = true;
                            break;
                        }
                        if (!HstatZoneFound) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).Name +
                                                "\", invalid humidistat specification");
                            ShowContinueError(
                                state,
                                "could not locate Humidistat in Zone=" +
                                    state.dataHeatBal->Zone(state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNum(SetZoneNum)).Name);
                            ErrorsFound = true;
                        }
                    }
                }
            }

            // single zone reheat setpoint manager
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::SZReheat)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZRhSetPtMgrs; ++SetPtMgrNum) {
                FanNodeIn = 0;
                FanNodeOut = 0;
                MixedAirNode = 0;
                AirLoopNum = 0;
                InletBranchNum = 0;
                LoopInNode = 0;
                bool LookForFan = false;
                ZoneInletNode = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneInletNodeNum;
                ZoneNode = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneNodeNum;
                // find the index in the ZoneEquipConfig array of the control zone (the one with the main or only thermostat)
                ConZoneNum = 0;
                for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                    if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode == ZoneNode) {
                        ConZoneNum = ControlledZoneNum;
                    }
                }
                if (ConZoneNum == 0) {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).Name +
                                        "\", Zone Node not found:");
                    ShowContinueError(state,
                                      "Node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneNodeNum) +
                                          "\", not found in any controlled Zone");
                    ErrorsFound = true;
                } else {
                    bool found = false;
                    for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).NumInletNodes; ++zoneInNode) {
                        if (state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneInletNodeNum ==
                            state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).InletNode(zoneInNode)) {
                            found = true;
                            AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).InletNodeAirLoopNum(zoneInNode);
                        }
                    }
                    if (!found) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).Name +
                                            "\", The zone inlet node of " +
                                            state.dataLoopNodes->NodeID(state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneInletNodeNum));
                        ShowContinueError(
                            state, "is not found in Zone = " + state.dataZoneEquip->ZoneEquipConfig(ConZoneNum).ZoneName + ". Please check inputs.");
                        ErrorsFound = true;
                    }
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).Name +
                                            "\", The zone inlet node is not connected to an air loop.");
                        ErrorsFound = true;
                        continue;
                    }
                    MixedAirNode = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OASysOutletNodeNum;
                    InletBranchNum = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).InletBranchNum[0];
                    LoopInNode = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(InletBranchNum).NodeNumIn;
                    // get the supply fan inlet and outlet nodes
                    if (MixedAirNode > 0) {
                        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents;
                                 ++CompNum) {
                                CompType = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).TypeOf;
                                if (MixedAirNode ==
                                    state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).NodeNumIn) {
                                    LookForFan = true;
                                }
                                if (LookForFan) {
                                    if (UtilityRoutines::SameString(CompType, "Fan:ConstantVolume") ||
                                        UtilityRoutines::SameString(CompType, "Fan:VariableVolume") ||
                                        UtilityRoutines::SameString(CompType, "Fan:OnOff") ||
                                        UtilityRoutines::SameString(CompType, "Fan:ComponentModel")) {
                                        FanNodeIn = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).NodeNumIn;
                                        FanNodeOut =
                                            state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).NodeNumOut;
                                        break;
                                    }
                                }
                            }
                        }
                    } else {
                        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents;
                                 ++CompNum) {
                                CompType = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).TypeOf;
                                if (UtilityRoutines::SameString(CompType, "Fan:ConstantVolume") ||
                                    UtilityRoutines::SameString(CompType, "Fan:VariableVolume") ||
                                    UtilityRoutines::SameString(CompType, "Fan:OnOff") ||
                                    UtilityRoutines::SameString(CompType, "Fan:ComponentModel")) {
                                    FanNodeIn = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).NodeNumIn;
                                    FanNodeOut = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).NodeNumOut;
                                }
                            }
                        }
                    }
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).FanNodeIn = FanNodeIn;
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).FanNodeOut = FanNodeOut;
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).MixedAirNode = MixedAirNode;
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).OAInNode =
                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OAMixOAInNodeNum;
                    // this next line assumes that OA system is the first thing on the branch, what if there is a relief fan or heat recovery coil
                    // or other component in there first? does it matter?
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).RetNode =
                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OASysInletNodeNum;
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).LoopInNode = LoopInNode;
                }
            }

            // Warmest Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::Warmest)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(state,
                                          "Air Loop not found =\"" + state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                    }
                    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).Name +
                                            "\", no zones with cooling found:");
                        ShowContinueError(state,
                                          "Air Loop provides no cooling, Air Loop=\"" +
                                              state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // Coldest Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::Coldest)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumColdestSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(state,
                                          "Air Loop not found =\"" + state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                    }
                    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesHeated == 0) {
                        if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled == 0) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).Name +
                                                "\", no zones with heating or cooling found:");
                            ShowContinueError(state,
                                              "Air Loop provides no heating or cooling, Air Loop=\"" +
                                                  state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // Warmest Temp Flow Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::WarmestTempFlow)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(
                            state, "Air Loop not found =\"" + state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum = AirLoopNum;
                        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).SimReady = true;
                    }
                    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name +
                                            "\", no zones with cooling found:");
                        ShowContinueError(state,
                                          "Air Loop provides no cooling, Air Loop=\"" +
                                              state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // return air bypass flow set manager
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::RAB)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumRABFlowSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AllSetPtMgrIndex).AirLoopNum =
                        AirLoopNum;
                    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AllSetPtMgrIndex).AirLoopName =
                        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopName;
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(state,
                                          "Air Loop not found =\"" + state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                        if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).RABExists) {
                            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).RABMixInNode =
                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).RABMixInNode;
                            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).SupMixInNode =
                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).SupMixInNode;
                            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).MixOutNode =
                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).MixOutNode;
                            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).RABSplitOutNode =
                                state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).RABSplitOutNode;
                            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).SysOutNode =
                                state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(1);
                            state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlNodes(1) =
                                state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).RABSplitOutNode;
                            state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AllSetPtMgrIndex)
                                .CtrlNodes(1) = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).RABSplitOutNode;
                        } else {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).Name +
                                                "\", no RAB in air loop found:");
                            ShowContinueError(state, "Air Loop=\"" + state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // MultiZone Average Cooling Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::MZCoolingAverage)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZClgAverageSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(
                            state, "Air Loop not found =\"" + state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                    }
                    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).Name +
                                            "\", no zones with cooling found:");
                        ShowContinueError(state,
                                          "Air Loop provides no cooling, Air Loop=\"" +
                                              state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // MultiZone Average Heating Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::MZHeatingAverage)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(
                            state, "Air Loop not found =\"" + state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                    }
                    // Commented out as we are using %NumZonesCooled instead of %NumZonesHeated for all systems for now
                    // IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated == 0) THEN
                    //  CALL ShowSevereError(state, TRIM(cSetPointManagerType)//': Air Loop provides no heating ' // &
                    //                       TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name))
                    //  CALL ShowContinueError(state, 'Occurs in Setpoint Manager='//TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name))
                    //  ErrorsFound = .TRUE.
                    // END IF
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // MultiZone Average Minimum Humidity Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::MZMinHumAverage)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(
                            state, "Air Loop not found =\"" + state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                        // make sure humidity controlled zone
                        HstatZoneFound = false;
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            for (ZonesCooledIndex = 1;
                                 ZonesCooledIndex <=
                                 state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                     .NumZonesCooled;
                                 ++ZonesCooledIndex) {
                                if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                    state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                        .CoolCtrlZoneNums(ZonesCooledIndex))
                                    continue;
                                HstatZoneFound = true;
                                break;
                            }
                        }
                        if (!HstatZoneFound) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).Name +
                                                "\", invalid humidistat specification");
                            ShowContinueError(state,
                                              "could not locate Humidistat in any of the zones served by the Air loop=" +
                                                  state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name);
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // MultiZone Average Maximum Humidity Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::MZMaxHumAverage)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(
                            state, "Air Loop not found =\"" + state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                        // make sure humidity controlled zone
                        HstatZoneFound = false;
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            for (ZonesCooledIndex = 1;
                                 ZonesCooledIndex <=
                                 state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                     .NumZonesCooled;
                                 ++ZonesCooledIndex) {
                                if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                    state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                        .CoolCtrlZoneNums(ZonesCooledIndex))
                                    continue;
                                HstatZoneFound = true;
                                break;
                            }
                        }
                        if (!HstatZoneFound) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).Name +
                                                "\", invalid humidistat specification");
                            ShowContinueError(state,
                                              "could not locate Humidistat in any of the zones served by the Air loop=" +
                                                  state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name);
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // Multizone Minimum Humidity Ratio Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::MZMinHum)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMinHumSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(state,
                                          "Air Loop not found =\"" + state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                        // make sure humidity controlled zone
                        HstatZoneFound = false;
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            for (ZonesCooledIndex = 1;
                                 ZonesCooledIndex <=
                                 state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                     .NumZonesCooled;
                                 ++ZonesCooledIndex) {
                                if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                    state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                        .CoolCtrlZoneNums(ZonesCooledIndex))
                                    continue;
                                HstatZoneFound = true;
                                break;
                            }
                        }
                        if (!HstatZoneFound) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).Name +
                                                "\", invalid humidistat specification");
                            ShowContinueError(state,
                                              "could not locate Humidistat in any of the zones served by the Air loop=" +
                                                  state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name);
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // Multizone Maximum Humidity Ratio Setpoint Managers
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::MZMaxHum)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMaxHumSetPtMgrs; ++SetPtMgrNum) {
                if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
                    AirLoopNum = UtilityRoutines::FindItemInList(state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopName,
                                                                 state.dataAirLoop->AirToZoneNodeInfo,
                                                                 &AirLoopZoneEquipConnectData::AirLoopName);
                    if (AirLoopNum == 0) {
                        ShowSevereError(state,
                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).Name +
                                            "\", invalid Air Loop specified:");
                        ShowContinueError(state,
                                          "Air Loop not found =\"" + state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopName + "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum = AirLoopNum;
                        // make sure humidity controlled zone
                        HstatZoneFound = false;
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            for (ZonesCooledIndex = 1;
                                 ZonesCooledIndex <=
                                 state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                     .NumZonesCooled;
                                 ++ZonesCooledIndex) {
                                if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                    state.dataAirLoop->AirToZoneNodeInfo(state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).AirLoopNum)
                                        .CoolCtrlZoneNums(ZonesCooledIndex))
                                    continue;
                                HstatZoneFound = true;
                                break;
                            }
                        }
                        if (!HstatZoneFound) {
                            ShowSevereError(state,
                                            cSetPointManagerType + "=\"" + state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).Name +
                                                "\", invalid humidistat specification");
                            ShowContinueError(state,
                                              "could not locate Humidistat in any of the zones served by the Air loop=" +
                                                  state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name);
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    cSetPointManagerType + "=\"" + state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).Name +
                                        "\", no AirLoopHVAC objects found:");
                    ShowContinueError(state, "Setpoint Manager needs an AirLoopHVAC to operate.");
                    ErrorsFound = true;
                }
            }

            // condenser entering water temperature reset setpoint manager
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::CondEntReset)];
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumCondEntSetPtMgrs; ++SetPtMgrNum) {
                // Scan loops and find the loop index that includes the condenser cooling tower node used as setpoint
                for (LoopNum = 1; LoopNum <= state.dataHVACGlobal->NumCondLoops + state.dataHVACGlobal->NumPlantLoops;
                     ++LoopNum) { // Begin demand side loops ... When condenser is added becomes NumLoops
                    for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                        if (state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum ==
                            state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex)) {
                            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).TotalBranches;
                                 ++BranchNum) {
                                for (CompNum = 1;
                                     CompNum <=
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).Branch(BranchNum).TotalComponents;
                                     ++CompNum) {
                                    // Check if cooling tower is single speed and generate and error
                                    state.dataSetPointManager->InitType =
                                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).Branch(BranchNum).Comp(CompNum).Type;
                                    if (state.dataSetPointManager->InitType == PlantEquipmentType::CoolingTower_SingleSpd) {
                                        ShowSevereError(state,
                                                        cSetPointManagerType + "=\"" + state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).Name +
                                                            "\", invalid tower found");
                                        ShowContinueError(state,
                                                          "Found SingleSpeed Cooling Tower, Cooling Tower=" + state.dataPlnt->PlantLoop(LoopNum)
                                                                                                                  .LoopSide(LoopSideLocation::Supply)
                                                                                                                  .Branch(BranchNum)
                                                                                                                  .Comp(CompNum)
                                                                                                                  .Name);
                                        ShowContinueError(state, "SingleSpeed cooling towers cannot be used with this setpoint manager.");
                                        ErrorsFound = true;
                                    }
                                }
                            }
                            // Scan all attached chillers in the condenser loop index found to find the chiller index
                            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).TotalBranches;
                                 ++BranchNum) {
                                for (CompNum = 1;
                                     CompNum <=
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).Branch(BranchNum).TotalComponents;
                                     ++CompNum) {
                                    state.dataSetPointManager->InitType =
                                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).Branch(BranchNum).Comp(CompNum).Type;
                                    switch (state.dataSetPointManager->InitType) {

                                    case PlantEquipmentType::Chiller_Absorption:
                                    case PlantEquipmentType::Chiller_Indirect_Absorption:
                                    case PlantEquipmentType::Chiller_CombTurbine:
                                    case PlantEquipmentType::Chiller_ConstCOP:
                                    case PlantEquipmentType::Chiller_Electric:
                                    case PlantEquipmentType::Chiller_ElectricEIR:
                                    case PlantEquipmentType::Chiller_DFAbsorption:
                                    case PlantEquipmentType::Chiller_ElectricReformEIR:
                                    case PlantEquipmentType::Chiller_EngineDriven: {
                                        // Scan the supply side to find the chiller index and branch index on plantloop
                                        state.dataSetPointManager->ChillerType = state.dataPlnt->PlantLoop(LoopNum)
                                                                                     .LoopSide(LoopSideLocation::Demand)
                                                                                     .Branch(BranchNum)
                                                                                     .Comp(CompNum)
                                                                                     .Type;
                                        for (LoopNum2 = 1; LoopNum2 <= state.dataHVACGlobal->NumCondLoops + state.dataHVACGlobal->NumPlantLoops;
                                             ++LoopNum2) {
                                            for (BranchNumPlantSide = 1;
                                                 BranchNumPlantSide <=
                                                 state.dataPlnt->PlantLoop(LoopNum2).LoopSide(LoopSideLocation::Supply).TotalBranches;
                                                 ++BranchNumPlantSide) {
                                                for (CompNumPlantSide = 1; CompNumPlantSide <= state.dataPlnt->PlantLoop(LoopNum2)
                                                                                                   .LoopSide(LoopSideLocation::Supply)
                                                                                                   .Branch(BranchNumPlantSide)
                                                                                                   .TotalComponents;
                                                     ++CompNumPlantSide) {
                                                    if (state.dataPlnt->PlantLoop(LoopNum2)
                                                            .LoopSide(LoopSideLocation::Supply)
                                                            .Branch(BranchNumPlantSide)
                                                            .Comp(CompNumPlantSide)
                                                            .Type == state.dataSetPointManager->ChillerType) {
                                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).LoopIndexPlantSide = LoopNum2;
                                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).ChillerIndexPlantSide =
                                                            CompNumPlantSide;
                                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).BranchIndexPlantSide =
                                                            BranchNumPlantSide;
                                                    }
                                                }
                                            }
                                        }
                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).Type = state.dataSetPointManager->ChillerType;
                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).LoopIndexDemandSide = LoopNum;
                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).ChillerIndexDemandSide = CompNum;
                                        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).BranchIndexDemandSide = BranchNum;
                                    } break;

                                    default:
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Ideal condenser entering water temperature reset setpoint manager
            cSetPointManagerType = managerTypeName[static_cast<int>(SetPointManagerType::IdealCondEntReset)];
            state.dataSetPointManager->InitSetPointManagerNumChiller = 0;
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumIdealCondEntSetPtMgrs; ++SetPtMgrNum) {
                // Scan loops and find the loop index that includes the condenser cooling tower node used as setpoint
                for (LoopNum = 1; LoopNum <= state.dataHVACGlobal->NumCondLoops + state.dataHVACGlobal->NumPlantLoops;
                     ++LoopNum) { // Begin demand side loops ... When condenser is added becomes NumLoops
                    for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes;
                         ++CtrlNodeIndex) {
                        if (state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum ==
                            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex)) {
                            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).TotalBranches;
                                 ++BranchNum) {
                                for (CompNum = 1;
                                     CompNum <=
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).Branch(BranchNum).TotalComponents;
                                     ++CompNum) {
                                    // Check if cooling tower is single speed and generate and error
                                    state.dataSetPointManager->InitType =
                                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Supply).Branch(BranchNum).Comp(CompNum).Type;
                                    if (state.dataSetPointManager->InitType == PlantEquipmentType::CoolingTower_SingleSpd) {
                                        ShowSevereError(state,
                                                        cSetPointManagerType + "=\"" +
                                                            state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).Name +
                                                            "\", invalid cooling tower found");
                                        ShowContinueError(state,
                                                          "Found Single Speed Cooling Tower, Cooling Tower=" + state.dataPlnt->PlantLoop(LoopNum)
                                                                                                                   .LoopSide(LoopSideLocation::Supply)
                                                                                                                   .Branch(BranchNum)
                                                                                                                   .Comp(CompNum)
                                                                                                                   .Name);
                                        ShowContinueError(state, "SingleSpeed cooling towers cannot be used with this setpoint manager on each loop");
                                        ErrorsFound = true;
                                    } else if (state.dataSetPointManager->InitType == PlantEquipmentType::CoolingTower_TwoSpd ||
                                               state.dataSetPointManager->InitType == PlantEquipmentType::CoolingTower_VarSpd) {
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CondTowerBranchNum.push_back(BranchNum);
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).TowerNum.push_back(CompNum);
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).numTowers++;
                                    }
                                    // Scan the pump on the condenser water loop
                                    if (state.dataSetPointManager->InitType == PlantEquipmentType::PumpVariableSpeed ||
                                        state.dataSetPointManager->InitType == PlantEquipmentType::PumpConstantSpeed) {
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CondPumpNum = CompNum;
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CondPumpBranchNum = BranchNum;
                                    }
                                }
                            }
                            // Scan all attached chillers in the condenser loop index found to find the chiller index
                            for (BranchNum = 1; BranchNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).TotalBranches;
                                 ++BranchNum) {
                                for (CompNum = 1;
                                     CompNum <=
                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).Branch(BranchNum).TotalComponents;
                                     ++CompNum) {
                                    state.dataSetPointManager->InitType =
                                        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideLocation::Demand).Branch(BranchNum).Comp(CompNum).Type;

                                    switch (state.dataSetPointManager->InitType) {

                                    case PlantEquipmentType::Chiller_Absorption:
                                    case PlantEquipmentType::Chiller_Indirect_Absorption:
                                    case PlantEquipmentType::Chiller_CombTurbine:
                                    case PlantEquipmentType::Chiller_ConstCOP:
                                    case PlantEquipmentType::Chiller_Electric:
                                    case PlantEquipmentType::Chiller_ElectricEIR:
                                    case PlantEquipmentType::Chiller_DFAbsorption:
                                    case PlantEquipmentType::Chiller_ElectricReformEIR:
                                    case PlantEquipmentType::Chiller_EngineDriven: {
                                        // Scan the supply side to find the chiller index and branch index on plantloop
                                        state.dataSetPointManager->ChillerType = state.dataPlnt->PlantLoop(LoopNum)
                                                                                     .LoopSide(LoopSideLocation::Demand)
                                                                                     .Branch(BranchNum)
                                                                                     .Comp(CompNum)
                                                                                     .Type;
                                        for (LoopNum2 = 1; LoopNum2 <= state.dataHVACGlobal->NumCondLoops + state.dataHVACGlobal->NumPlantLoops;
                                             ++LoopNum2) {
                                            for (BranchNumPlantSide = 1;
                                                 BranchNumPlantSide <=
                                                 state.dataPlnt->PlantLoop(LoopNum2).LoopSide(LoopSideLocation::Supply).TotalBranches;
                                                 ++BranchNumPlantSide) {
                                                for (CompNumPlantSide = 1; CompNumPlantSide <= state.dataPlnt->PlantLoop(LoopNum2)
                                                                                                   .LoopSide(LoopSideLocation::Supply)
                                                                                                   .Branch(BranchNumPlantSide)
                                                                                                   .TotalComponents;
                                                     ++CompNumPlantSide) {
                                                    state.dataSetPointManager->InitType = state.dataPlnt->PlantLoop(LoopNum2)
                                                                                              .LoopSide(LoopSideLocation::Supply)
                                                                                              .Branch(BranchNumPlantSide)
                                                                                              .Comp(CompNumPlantSide)
                                                                                              .Type;
                                                    if (state.dataSetPointManager->InitType == state.dataSetPointManager->ChillerType) {
                                                        ++state.dataSetPointManager->InitSetPointManagerNumChiller;
                                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).LoopIndexPlantSide = LoopNum2;
                                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).ChillerIndexPlantSide =
                                                            CompNumPlantSide;
                                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).BranchIndexPlantSide =
                                                            BranchNumPlantSide;
                                                        // Scan the pump on the chilled water loop
                                                        for (BranchNum2 = 1;
                                                             BranchNum2 <=
                                                             state.dataPlnt->PlantLoop(LoopNum2).LoopSide(LoopSideLocation::Supply).TotalBranches;
                                                             ++BranchNum2) {
                                                            for (CompNum2 = 1; CompNum2 <= state.dataPlnt->PlantLoop(LoopNum2)
                                                                                               .LoopSide(LoopSideLocation::Supply)
                                                                                               .Branch(BranchNum2)
                                                                                               .TotalComponents;
                                                                 ++CompNum2) {
                                                                state.dataSetPointManager->InitType = state.dataPlnt->PlantLoop(LoopNum2)
                                                                                                          .LoopSide(LoopSideLocation::Supply)
                                                                                                          .Branch(BranchNum2)
                                                                                                          .Comp(CompNum2)
                                                                                                          .Type;
                                                                if (state.dataSetPointManager->InitType == PlantEquipmentType::PumpVariableSpeed ||
                                                                    state.dataSetPointManager->InitType == PlantEquipmentType::PumpConstantSpeed) {
                                                                    state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).ChilledPumpNum =
                                                                        CompNum2;
                                                                    state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum)
                                                                        .ChilledPumpBranchNum = BranchNum2;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        if (state.dataSetPointManager->InitSetPointManagerNumChiller > 1) {
                                            ShowSevereError(state,
                                                            cSetPointManagerType + "=\"" +
                                                                state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).Name +
                                                                "\", too many chillers found");
                                            ShowContinueError(state, "only one chiller can be used with this setpoint manager on each loop");
                                            ShowContinueError(state,
                                                              "Found more than one chiller, chiller =" + state.dataPlnt->PlantLoop(LoopNum)
                                                                                                             .LoopSide(LoopSideLocation::Demand)
                                                                                                             .Branch(BranchNum)
                                                                                                             .Comp(CompNum)
                                                                                                             .Name);
                                            ErrorsFound = true;
                                        }
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).Type = state.dataSetPointManager->ChillerType;
                                        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CondLoopNum = LoopNum;
                                    } break;

                                    default:
                                        break;
                                    }
                                }
                            }
                            state.dataSetPointManager->InitSetPointManagerNumChiller = 0;
                        }
                    }
                }
            }

            VerifySetPointManagers(state, ErrorsFound);
        }

        state.dataSetPointManager->InitSetPointManagersOneTimeFlag = false;

        if (ErrorsFound) {
            ShowFatalError(state, "InitSetPointManagers: Errors found in getting SetPointManager input.");
        }
    }

    if ((state.dataGlobal->BeginEnvrnFlag && state.dataSetPointManager->InitSetPointManagersMyEnvrnFlag) ||
        state.dataSetPointManager->InitSetPointManagersOneTimeFlag2) {

        state.dataSetPointManager->ManagerOn = false;

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                // Initialize scheduled setpoints
                switch (state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode) {
                case CtrlVarType::Temp: {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MaxTemp: {
                    state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MinTemp: {
                    state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::HumRat: {
                    state.dataLoopNodes->Node(NodeNum).HumRatSetPoint =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MaxHumRat: {
                    state.dataLoopNodes->Node(NodeNum).HumRatMax =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MinHumRat: {
                    state.dataLoopNodes->Node(NodeNum).HumRatMin =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MassFlow: {
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MaxMassFlow: {
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMax =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                case CtrlVarType::MinMassFlow: {
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMin =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SchedPtr);
                } break;
                default:
                    break;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumDualSchSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedPtrHi);
                    state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SchedPtrLo);
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                        (state.dataLoopNodes->Node(NodeNum).TempSetPointHi + state.dataLoopNodes->Node(NodeNum).TempSetPointLo) / 2.0;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOutAirSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).calculate(state);
                NodeNum = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt;
                } else if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt;
                } else if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMinHumSetPtMgrs; ++SetPtMgrNum) { // Minimum humidity setpoint managers
            for (ZoneIndex = 1; ZoneIndex <= state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumZones; ++ZoneIndex) {
                ZoneNode = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).ZoneNodes(ZoneIndex);
                state.dataLoopNodes->Node(ZoneNode).MassFlowRate = 0.0;
            }
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                state.dataLoopNodes->Node(NodeNum).HumRatMin = 0.007;                                        // Set the setpoint
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMaxHumSetPtMgrs; ++SetPtMgrNum) { // Maximum humidity setpoint managers
            for (ZoneIndex = 1; ZoneIndex <= state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumZones; ++ZoneIndex) {
                ZoneNode = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).ZoneNodes(ZoneIndex);
                state.dataLoopNodes->Node(ZoneNode).MassFlowRate = 0.0;
            }
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                state.dataLoopNodes->Node(NodeNum).HumRatMax = 0.011;                                        // Set the setpoint
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZRhSetPtMgrs; ++SetPtMgrNum) { // single zone reheat setpoint managers
            ZoneInletNode = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneInletNodeNum;
            ZoneNode = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).ZoneNodeNum;
            state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ZoneNode).MassFlowRate = 0.0;
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZHtSetPtMgrs; ++SetPtMgrNum) { // single zone heating setpoint managers
            ZoneInletNode = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneInletNodeNum;
            ZoneNode = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).ZoneNodeNum;
            state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ZoneNode).MassFlowRate = 0.0;
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) { // single zone cooling setpoint managers
            ZoneInletNode = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneInletNodeNum;
            ZoneNode = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ZoneNodeNum;
            state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ZoneNode).MassFlowRate = 0.0;
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMixedAirSetPtMgrs; ++SetPtMgrNum) { // mixed air setpoint managers

            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).RefNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanInNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            state.dataLoopNodes->Node(state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).FanOutNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOAPretreatSetPtMgrs;
             ++SetPtMgrNum) { // Outside Air Pretreat setpoint managers

            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode).Temp = 20.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode).Quality = 1.0;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode).Press = state.dataEnvrn->OutBaroPress;
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).RefNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).MixedOutNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).OAInNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            state.dataLoopNodes->Node(state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).ReturnInNode).Enthalpy =
                PsyHFnTdbW(DataPrecisionGlobals::constant_twenty, state.dataEnvrn->OutHumRat);
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
                if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxHumRat) {
                    state.dataLoopNodes->Node(NodeNum).HumRatMax = state.dataEnvrn->OutHumRat; // Set the setpoint
                }
                if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinHumRat) {
                    state.dataLoopNodes->Node(NodeNum).HumRatMin = state.dataEnvrn->OutHumRat; // Set the setpoint
                }
                if (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::HumRat) {
                    state.dataLoopNodes->Node(NodeNum).HumRatSetPoint = state.dataEnvrn->OutHumRat; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumColdestSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the temperature setpoint
                    if (state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum != 0) {
                        state.dataAirLoop->AirLoopFlow(state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum).ReqSupplyFrac =
                            1.0; // PH 10/09/04 Set the flow
                        state.dataAirLoop->AirLoopControlInfo(state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum)
                            .LoopFlowRateSet = true; // PH 10/09/04 Set the flag
                    }
                }
            }
        }

        if (state.dataZoneEquip->ZoneEquipInputsFilled && state.dataAirLoop->AirLoopInputsFilled) {
            for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumRABFlowSetPtMgrs; ++SetPtMgrNum) {
                NodeNum = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).RABSplitOutNode;
                if (state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MassFlow) {
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint = 0.0;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZClgAverageSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                state.dataLoopNodes->Node(NodeNum).HumRatMin = 0.007;                                               // Set the setpoint
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                state.dataLoopNodes->Node(NodeNum).HumRatMax = 0.011;                                               // Set the setpoint
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMinHumSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                state.dataLoopNodes->Node(NodeNum).HumRatMin = 0.007;                                        // Set the setpoint
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMaxHumSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                state.dataLoopNodes->Node(NodeNum).HumRatMax = 0.011;                                        // Set the setpoint
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowOATempSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceTempType::WetBulb) {
                    if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->OutWetBulbTemp; // Set the setpoint
                    } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->OutWetBulbTemp; // Set the setpoint
                    } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->OutWetBulbTemp; // Set the setpoint
                    }
                } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceTempType::DryBulb) {
                    if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->OutDryBulbTemp; // Set the setpoint
                    } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->OutDryBulbTemp; // Set the setpoint
                    } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->OutDryBulbTemp; // Set the setpoint
                    }
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
                 ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (CheckOutAirNodeNumber(state, state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefNodeNum)) {
                    if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceTempType::WetBulb) {
                        state.dataLoopNodes->Node(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefNodeNum).SPMNodeWetBulbRepReq =
                            true;
                        if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                            state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->OutWetBulbTemp; // Set the setpoint
                        } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                            state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->OutWetBulbTemp; // Set the setpoint
                        } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                            state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->OutWetBulbTemp; // Set the setpoint
                        }
                    } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceTempType::DryBulb) {
                        if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                            state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->OutDryBulbTemp; // Set the setpoint
                        } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                            state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->OutDryBulbTemp; // Set the setpoint
                        } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                            state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->OutDryBulbTemp; // Set the setpoint
                        }
                    }
                } else {
                    // If reference node is a water node, then set RefTypeMode to NodeDryBulb
                    if (state.dataLoopNodes->Node(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefNodeNum).FluidType ==
                        DataLoopNode::NodeFluidType::Water) {
                        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTypeMode = ReferenceTempType::DryBulb;
                    } else if (state.dataLoopNodes->Node(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefNodeNum).FluidType ==
                               DataLoopNode::NodeFluidType::Air) {
                        if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceTempType::WetBulb) {
                            state.dataLoopNodes->Node(state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).RefNodeNum)
                                .SPMNodeWetBulbRepReq = true;
                        }
                    }
                    if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = 20.0; // Set the setpoint
                    } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = 20.0; // Set the setpoint
                    } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = 20.0; // Set the setpoint
                    }
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumGroundTempSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceGroundTempObjectType::BuildingSurface) {
                    if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->GroundTemp; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->GroundTemp; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->GroundTemp; // Set the setpoint
                    }
                } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceGroundTempObjectType::Shallow) {
                    if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->GroundTemp_Surface; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->GroundTemp_Surface; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->GroundTemp_Surface; // Set the setpoint
                    }
                } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceGroundTempObjectType::Deep) {
                    if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->GroundTemp_Deep; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->GroundTemp_Deep; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->GroundTemp_Deep; // Set the setpoint
                    }
                } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).RefTypeMode == ReferenceGroundTempObjectType::FCFactorMethod) {
                    if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataEnvrn->GroundTempFC; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataEnvrn->GroundTempFC; // Set the setpoint
                    } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                        state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataEnvrn->GroundTempFC; // Set the setpoint
                    }
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumCondEntSetPtMgrs;
             ++SetPtMgrNum) { // Condenser entering water Set point managers
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                        GetCurrentScheduleValue(state, state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CondEntTempSchedPtr);
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumIdealCondEntSetPtMgrs;
             ++SetPtMgrNum) { // Ideal Condenser entering water Set point managers
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).MaxCondEntTemp;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
                 ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CoolingOffTemp;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
                 ++CtrlNodeIndex) {
                NodeNum = state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                if (state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).HeatingOffTemp;
                }
            }
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs; ++SetPtMgrNum) {
            state.dataLoopNodes->Node(state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).supplyNodeIndex).TempSetPoint =
                state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum).minimumChilledWaterSetpoint;
        }

        for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs; ++SetPtMgrNum) {
            state.dataLoopNodes->Node(state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).supplyNodeIndex).TempSetPoint =
                state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum).maximumHotWaterSetpoint;
        }

        // System Node Reset Temperature and Humidity
        for (int SetPtMgrNum = 1;
             SetPtMgrNum <= (state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs + state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs);
             ++SetPtMgrNum) {
            for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
                state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).calculate(state);
                NodeNum = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
                switch (state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).CtrlTypeMode) {
                case CtrlVarType::Temp:
                    state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt;
                    break;
                case CtrlVarType::MaxTemp:
                    state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt;
                    break;
                case CtrlVarType::MinTemp:
                    state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt;
                    break;
                case CtrlVarType::HumRat:
                    state.dataLoopNodes->Node(NodeNum).HumRatSetPoint = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt;
                    break;
                case CtrlVarType::MaxHumRat:
                    state.dataLoopNodes->Node(NodeNum).HumRatMax = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt;
                    break;
                case CtrlVarType::MinHumRat:
                    state.dataLoopNodes->Node(NodeNum).HumRatMin = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt;
                    break;
                default:
                    break;
                }
            }
        }

        state.dataSetPointManager->InitSetPointManagersMyEnvrnFlag = false;
        if (!state.dataSetPointManager->InitSetPointManagersOneTimeFlag) state.dataSetPointManager->InitSetPointManagersOneTimeFlag2 = false;

        if (ErrorsFound) {
            ShowFatalError(state, "InitSetPointManagers: Errors found. Program Terminates.");
        }

    } // end begin environment inits
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataSetPointManager->InitSetPointManagersMyEnvrnFlag = true;
    }
}

void SimSetPointManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   July 1998
    //       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
    //                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
    //                        Add new setpoint managers:
    //                          SET POINT MANAGER:SINGLE ZONE HEATING and
    //                          SET POINT MANAGER:SINGLE ZONE COOLING
    //                        Work supported by ASHRAE research project 1254-RP
    //                      Haves Oct 2004
    //                      July 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers
    //                          SetpointManager:MultiZone:Heating:Average
    //                          SetpointManager:MultiZone:Cooling:Average
    //                          SetpointManager:MultiZone:MinimumHumidity:Average
    //                          SetpointManager:MultiZone:MaximumHumidity:Average
    //                      Aug 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers:
    //                          SetpointManager:MultiZone:Humidity:Minimum
    //                          SetpointManager:MultiZone:Humidity:Maximum
    //                      Aug 2014 Rick Strand, UIUC
    //                         SetpointManager:ScheduleTES (internally defined)
    //                      Jan 2022 Wooyoung Jung, Jeremy Lerond and Jian Zhang, PNNL
    //                        Added new setpoint managers:
    //                          SetpointManager:SystemNodeReset:Temperature
    //                          SetpointManager:SystemNodeReset:Humidity
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Loop over all the Setpoint Managers and invoke the correct
    // Setpoint Manager algorithm.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;

    // Execute all the Setpoint Managers

    // The Scheduled Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Scheduled TES Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchTESSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SchTESSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Scheduled Dual Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumDualSchSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Outside Air Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOutAirSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Single Zone Reheat Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZRhSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Single Zone Heating Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZHtSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Single Zone Cooling Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Single Zone Minimum Humidity Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMinHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Single Zone Maximum Humidity Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Warmest Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Coldest Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumColdestSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Warmest Temp Flow Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum) {
        state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).calculate(state);
    }

    // The RAB Temp Flow Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumRABFlowSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Multizone Average Cooling Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZClgAverageSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Multizone Average Heating Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Multizone Average Minimum Humidity Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Multizone Average Maximum Humidity Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Multizone Minimum Humidity Ratio Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMinHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Multizone Maximum Humidity Ratio Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Follow Outdoor Air  Temperature Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowOATempSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Follow System Node Temp Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Ground Temp Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumGroundTempSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Condenser Entering Water Temperature Set Point Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumCondEntSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The Ideal Condenser Entering Water Temperature Set Point Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumIdealCondEntSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // the single zone cooling on/off staged control setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // the single zone heating on/off staged control setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // return water reset
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs; ++SetPtMgrNum) {
        auto &returnWaterSPM(state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum));
        returnWaterSPM.calculate(
            state, state.dataLoopNodes->Node(returnWaterSPM.returnNodeIndex), state.dataLoopNodes->Node(returnWaterSPM.supplyNodeIndex));
    }

    // hot-water return water reset
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs; ++SetPtMgrNum) {
        auto &returnWaterSPM(state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum));
        returnWaterSPM.calculate(
            state, state.dataLoopNodes->Node(returnWaterSPM.returnNodeIndex), state.dataLoopNodes->Node(returnWaterSPM.supplyNodeIndex));
    }

    // The System Node Reset Temperature Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).calculate(state);
    }

    // The System Node Reset Humidity Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs; ++SetPtMgrNum) {
        state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum + state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs).calculate(state);
    }
}

void DefineScheduledSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   July 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the setpoint using a simple schedule.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENTS:

    this->SetPt = GetCurrentScheduleValue(state, this->SchedPtr);
}

void DefineScheduledTESSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Aug 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the setpoint using a simple schedule, then modify the value based on TES simple controls logic

    // METHODOLOGY EMPLOYED:
    // Modified schedule setpoint manager logic

    // Locals
    Real64 CurSchValOnPeak;
    Real64 CurSchValCharge;
    Real64 constexpr OnVal(0.5);

    CurSchValOnPeak = GetCurrentScheduleValue(state, this->SchedPtr);
    CurSchValCharge = GetCurrentScheduleValue(state, this->SchedPtrCharge);

    // CtrlType bug
    //        if (this->CompOpType == DataPlant::CtrlType::CoolingOp) { // this is some sort of chiller
    if (this->CompOpType == DataPlant::CtrlType::HeatingOp) { // this is some sort of chiller
        if (CurSchValOnPeak >= OnVal) {
            this->SetPt = this->NonChargeCHWTemp;
        } else if (CurSchValCharge < OnVal) {
            this->SetPt = this->NonChargeCHWTemp;
        } else {
            this->SetPt = this->ChargeCHWTemp;
        }
        // CtrlType Bug
        //        } else if (this->CompOpType == DataPlant::CtrlType::DualOp) { // this is some sort of ice storage system
    } else if (this->CompOpType == DataPlant::CtrlType::CoolingOp) { // this is some sort of ice storage system
        this->SetPt = this->NonChargeCHWTemp;
    }
}

void DefineSchedDualSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   May 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the both setpoint using a simple schedule.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENTS:

    this->SetPtHi = GetCurrentScheduleValue(state, this->SchedPtrHi);
    this->SetPtLo = GetCurrentScheduleValue(state, this->SchedPtrLo);
}

void DefineOutsideAirSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE ARGUMENTS:

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SchedVal;
    Real64 OutLowTemp;
    Real64 OutHighTemp;
    Real64 SetTempAtOutLow;
    Real64 SetTempAtOutHigh;

    if (this->SchedPtr > 0) {
        SchedVal = GetCurrentScheduleValue(state, this->SchedPtr);
    } else {
        SchedVal = 0.0;
    }

    if (SchedVal == 2.0) {
        OutLowTemp = this->OutLow2;
        OutHighTemp = this->OutHigh2;
        SetTempAtOutLow = this->OutLowSetPt2;
        SetTempAtOutHigh = this->OutHighSetPt2;
    } else {
        OutLowTemp = this->OutLow1;
        OutHighTemp = this->OutHigh1;
        SetTempAtOutLow = this->OutLowSetPt1;
        SetTempAtOutHigh = this->OutHighSetPt1;
    }

    this->SetPt = this->calcSetPointLinInt(OutLowTemp, OutHighTemp, state.dataEnvrn->OutDryBulbTemp, SetTempAtOutLow, SetTempAtOutHigh);
}

void DefineSZReheatSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // From the heating or cooling load of the control zone, calculate the supply air setpoint
    // needed to meet that zone load

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;
    using Psychrometrics::PsyTdbFnHW;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneLoad;     // required zone load [W]
    Real64 ZoneMassFlow; // zone inlet mass flow rate [kg/s]
    Real64 CpAir;        // inlet air specific heat [J/kg-C]
    int ZoneInletNode;
    int ZoneNode;
    int ZoneNum;
    Real64 ZoneTemp;
    Real64 ZoneLoadToCoolSetPt;
    Real64 ZoneLoadToHeatSetPt;
    Real64 TSetPt;
    Real64 TSetPt1;
    Real64 TSetPt2;
    bool DeadBand;
    int FanNodeIn;
    int FanNodeOut;
    int RetNode;
    int OAMixOAInNode;
    Real64 FanDeltaT;
    Real64 TMixAtMinOA;
    Real64 EnthMixAtMinOA;
    Real64 HumRatMixAtMinOA;
    int AirLoopNum;
    Real64 OAFrac;
    int LoopInNode;

    ZoneInletNode = this->ZoneInletNodeNum;
    ZoneNum = this->ControlZoneNum;
    ZoneNode = this->ZoneNodeNum;
    FanNodeIn = this->FanNodeIn;
    FanNodeOut = this->FanNodeOut;
    RetNode = this->RetNode;
    OAMixOAInNode = this->OAInNode;
    AirLoopNum = this->AirLoopNum;
    OAFrac =
        state.dataAirLoop->AirLoopFlow(AirLoopNum).OAFrac; // changed from MinOAFrac, now updates to current oa fraction for improve deadband control
    ZoneMassFlow = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
    ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired;
    ZoneLoadToCoolSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP;
    ZoneLoadToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP;
    DeadBand = state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum);
    ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
    LoopInNode = this->LoopInNode;
    if (OAMixOAInNode > 0) {
        HumRatMixAtMinOA = (1.0 - OAFrac) * state.dataLoopNodes->Node(RetNode).HumRat + OAFrac * state.dataLoopNodes->Node(OAMixOAInNode).HumRat;
        EnthMixAtMinOA = (1.0 - OAFrac) * state.dataLoopNodes->Node(RetNode).Enthalpy + OAFrac * state.dataLoopNodes->Node(OAMixOAInNode).Enthalpy;
        TMixAtMinOA = PsyTdbFnHW(EnthMixAtMinOA, HumRatMixAtMinOA);
    } else {
        TMixAtMinOA = state.dataLoopNodes->Node(LoopInNode).Temp;
    }
    if (FanNodeOut > 0 && FanNodeIn > 0) {
        FanDeltaT = state.dataLoopNodes->Node(FanNodeOut).Temp - state.dataLoopNodes->Node(FanNodeIn).Temp;
    } else {
        FanDeltaT = 0.0;
    }
    state.dataSetPointManager->TSupNoHC = TMixAtMinOA + FanDeltaT;
    CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
    state.dataSetPointManager->ExtrRateNoHC = CpAir * ZoneMassFlow * (state.dataSetPointManager->TSupNoHC - ZoneTemp);
    if (ZoneMassFlow <= SmallMassFlow) {
        TSetPt = state.dataSetPointManager->TSupNoHC;
    } else if (DeadBand || std::abs(ZoneLoad) < SmallLoad) {
        // if air with no active heating or cooling provides cooling
        if (state.dataSetPointManager->ExtrRateNoHC < 0.0) {
            // if still in deadband, do no active heating or cooling;
            // if below heating setpoint, set a supply temp that will cool to the heating setpoint
            if (state.dataSetPointManager->ExtrRateNoHC >= ZoneLoadToHeatSetPt) {
                TSetPt = state.dataSetPointManager->TSupNoHC;
            } else {
                TSetPt = ZoneTemp + ZoneLoadToHeatSetPt / (CpAir * ZoneMassFlow);
            }
            // if air with no active heating or cooling provides heating
        } else if (state.dataSetPointManager->ExtrRateNoHC > 0.0) {
            // if still in deadband, do no active heating or cooling;
            // if above cooling setpoint, set a supply temp that will heat to the cooling setpoint
            if (state.dataSetPointManager->ExtrRateNoHC <= ZoneLoadToCoolSetPt) {
                TSetPt = state.dataSetPointManager->TSupNoHC;
            } else {
                TSetPt = ZoneTemp + ZoneLoadToCoolSetPt / (CpAir * ZoneMassFlow);
            }
        } else {
            TSetPt = state.dataSetPointManager->TSupNoHC;
        }
    } else if (ZoneLoad < (-1.0 * SmallLoad)) {
        TSetPt1 = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlow);
        TSetPt2 = ZoneTemp + ZoneLoadToHeatSetPt / (CpAir * ZoneMassFlow);
        if (TSetPt1 > state.dataSetPointManager->TSupNoHC) {
            if (TSetPt2 > state.dataSetPointManager->TSupNoHC) {
                TSetPt = TSetPt2;
            } else {
                TSetPt = state.dataSetPointManager->TSupNoHC;
            }
        } else {
            TSetPt = TSetPt1;
        }
    } else if (ZoneLoad > SmallLoad) {
        TSetPt1 = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlow);
        TSetPt2 = ZoneTemp + ZoneLoadToCoolSetPt / (CpAir * ZoneMassFlow);
        if (TSetPt1 < state.dataSetPointManager->TSupNoHC) {
            if (TSetPt2 < state.dataSetPointManager->TSupNoHC) {
                TSetPt = TSetPt2;
            } else {
                TSetPt = state.dataSetPointManager->TSupNoHC;
            }
        } else {
            TSetPt = TSetPt1;
        }
    } else {
        TSetPt = state.dataSetPointManager->TSupNoHC;
    }

    TSetPt = max(min(TSetPt, this->MaxSetTemp), this->MinSetTemp);
    this->SetPt = TSetPt;
}

void DefineSZHeatingSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. J. Witte based on CalcSingZoneRhSetPoint by Fred Buhl,
    //                        Work supported by ASHRAE research project 1254-RP
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // From the heating load of the control zone, calculate the supply air setpoint
    // needed to meet that zone load (based on CalcSingZoneRhSetPoint)

    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    Real64 ZoneLoadtoHeatSP; // required zone load to zone heating setpoint [W]
    Real64 ZoneMassFlow;     // zone inlet mass flow rate [kg/s]
    Real64 CpAir;            // inlet air specific heat [J/kg-C]
    int ZoneInletNode;
    int ZoneNode;
    int ZoneNum;
    Real64 ZoneTemp;

    ZoneInletNode = this->ZoneInletNodeNum;
    ZoneNum = this->ControlZoneNum;
    ZoneNode = this->ZoneNodeNum;
    ZoneMassFlow = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
    ZoneLoadtoHeatSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP;
    ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
    if (ZoneMassFlow <= SmallMassFlow) {
        this->SetPt = this->MinSetTemp;
    } else {
        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
        this->SetPt = ZoneTemp + ZoneLoadtoHeatSP / (CpAir * ZoneMassFlow);
        this->SetPt = max(this->SetPt, this->MinSetTemp);
        this->SetPt = min(this->SetPt, this->MaxSetTemp);
    }
}

void DefineSZCoolingSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. J. Witte based on CalcSingZoneRhSetPoint by Fred Buhl,
    //                        Work supported by ASHRAE research project 1254-RP
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // From the Cooling load of the control zone, calculate the supply air setpoint
    // needed to meet that zone load (based on CalcSingZoneRhSetPoint)

    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    Real64 ZoneLoadtoCoolSP; // required zone load to zone Cooling setpoint [W]
    Real64 ZoneMassFlow;     // zone inlet mass flow rate [kg/s]
    Real64 CpAir;            // inlet air specific Cool [J/kg-C]
    int ZoneInletNode;
    int ZoneNode;
    int ZoneNum;
    Real64 ZoneTemp;

    ZoneInletNode = this->ZoneInletNodeNum;
    ZoneNum = this->ControlZoneNum;
    ZoneNode = this->ZoneNodeNum;
    ZoneMassFlow = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
    ZoneLoadtoCoolSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP;
    ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
    if (ZoneMassFlow <= SmallMassFlow) {
        this->SetPt = this->MaxSetTemp;
    } else {
        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
        this->SetPt = ZoneTemp + ZoneLoadtoCoolSP / (CpAir * ZoneMassFlow);
        this->SetPt = max(this->SetPt, this->MinSetTemp);
        this->SetPt = min(this->SetPt, this->MaxSetTemp);
    }
}

void DefineSZOneStageCoolinggSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   August 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate the setpoint for staged on/off cooling

    // METHODOLOGY EMPLOYED:
    // Evaluate stage in zone energy demand structure and choose setpoint accordingly

    if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(this->ControlZoneNum).StageNum >= 0) {
        this->SetPt = this->CoolingOffTemp;
    } else { // negative so a cooling stage is set
        this->SetPt = this->CoolingOnTemp;
    }
}

void DefineSZOneStageHeatingSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   August 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate the setpoint for staged on/off control

    // METHODOLOGY EMPLOYED:
    // Evaluate stage in zone energy demand structure and choose setpoint accordingly

    if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(this->ControlZoneNum).StageNum <= 0) {
        this->SetPt = this->HeatingOffTemp;
    } else { // positive so a heating stage is set
        this->SetPt = this->HeatingOnTemp;
    }
}

void DefineSZMinHumSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   October 2000
    //       MODIFIED       Shirey/Raustad Jan 2002
    //                      Gu, Dec 2007
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // From humidity load of the control zone, calculate the supply air humidity
    // needed to meet the minimum humidity setpoint

    // METHODOLOGY EMPLOYED:
    // Zone moisture load from ZoneTempPredictorCorrector (via DataZoneEnergyDemands)
    // is used to calculate the minimum supply air humidity ratio
    // needed to meet minimum zone relative humidity requirement

    // Using/Aliasing
    using DataHVACGlobals::SmallMassFlow;
    using Psychrometrics::PsyWFnTdbRhPb;

    int ZoneNode;
    Real64 ZoneMassFlow;
    int ZoneNum;
    Real64 MoistureLoad; // Zone moisture load (kg moisture/second) required to meet the relative humidity setpoint
    // Value obtained from ZoneTempPredictorCorrector (via ZoneSysMoistureDemand in DataZoneEnergyDemands)
    Real64 SupplyAirHumRat; // Desired air humidity ratio

    this->SetPt = 0.0;
    // Only use one zone for now
    ZoneNode = this->ZoneNodes(1);
    ZoneMassFlow = state.dataLoopNodes->Node(ZoneNode).MassFlowRate;
    ZoneNum = this->ZoneNum(1);

    if (ZoneMassFlow > SmallMassFlow) {

        MoistureLoad = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(this->ZoneNum(1)).OutputRequiredToHumidifyingSP;

        SupplyAirHumRat = max(0.0, state.dataLoopNodes->Node(ZoneNode).HumRat + MoistureLoad / ZoneMassFlow);

        // Positive Humidity Ratio MoistureLoad means a humidification load and only humidifying can raise up to a minimum
        //  IF(MoistureLoad .GT. 0.0) SZMinHumSetPtMgr(SetPtMgrNum)%SetPt = SupplyAirHumRat
        this->SetPt = SupplyAirHumRat;
    }
}

void DefineSZMaxHumSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Raustad/Shirey, FSEC
    //       DATE WRITTEN   January 2004
    //       MODIFIED       Gu, Dec. 2007
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // From humidity load of the control zone, calculate the supply air humidity
    // needed to meet the maximum humidity setpoint

    // METHODOLOGY EMPLOYED:
    // Zone moisture load from ZoneTempPredictorCorrector (via DataZoneEnergyDemands)
    // is used to calculate the maximum supply air humidity ratio
    // needed to meet maximum zone relative humidity requirement

    // Using/Aliasing
    using DataHVACGlobals::SmallMassFlow;
    using Psychrometrics::PsyWFnTdbRhPb;

    int ZoneNode;        // Control zone air node number
    Real64 ZoneMassFlow; // Zone air mass flow rate (kg/s)
    Real64 MoistureLoad; // Zone moisture load (kg moisture/sec) required to meet the relative humidity setpoint
    // Value obtained from ZoneTempPredictorCorrector (via ZoneSysMoistureDemand in DataZoneEnergyDemands)
    Real64 SupplyAirHumRat; // Desired air humidity ratio
    Real64 SystemMassFlow;

    this->SetPt = 0.0;
    // Only use one zone for now
    ZoneNode = this->ZoneNodes(1);
    ZoneMassFlow = state.dataLoopNodes->Node(ZoneNode).MassFlowRate;

    if (ZoneMassFlow > SmallMassFlow) {

        MoistureLoad = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(this->ZoneNum(1)).OutputRequiredToDehumidifyingSP;

        SystemMassFlow = state.dataLoopNodes->Node(this->CtrlNodes(1)).MassFlowRate;

        // MoistureLoad (negative for dehumidification) may be so large that a negative humrat results, cap at 0.00001
        SupplyAirHumRat = max(0.00001, state.dataLoopNodes->Node(ZoneNode).HumRat + MoistureLoad / ZoneMassFlow);

        // This hum rat is currently used in Controller:Simple, control variable "TEMPandHUMRAT" (Jan 2004)
        // Negative MoistureLoad means a dehumidification load
        this->SetPt = SupplyAirHumRat;
    }
}

void DefineMixedAirSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Starting with the setpoint at the reference node, subtract the supply fan
    // temperature rise and set the resulting temperature at the mixed air node.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using EMSManager::CheckIfNodeSetPointManagedByEMS;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FanInNode;       // supply fan inlet node number
    int FanOutNode;      // supply fan outlet node number
    int RefNode;         // setpoint reference node number
    int CoolCoilInNode;  // Cooling coil inlet node number
    int CoolCoilOutNode; // Cooling coil outlet node number
    Real64 MinTemp;      // Minimum temperature at cooling coil outlet node
    Real64 dtFan;        // Temperature difference across a fan
    Real64 dtCoolCoil;   // Temperature difference across a coolig coil

    FanInNode = this->FanInNode;
    FanOutNode = this->FanOutNode;
    RefNode = this->RefNode;
    CoolCoilInNode = this->CoolCoilInNode;
    CoolCoilOutNode = this->CoolCoilOutNode;
    MinTemp = this->MinCoolCoilOutTemp;
    this->FreezeCheckEnable = false;

    if (!state.dataGlobal->SysSizingCalc && this->MySetPointCheckFlag) {

        RefNode = this->RefNode;
        if (state.dataLoopNodes->Node(RefNode).TempSetPoint == SensedNodeFlagValue) {
            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                ShowSevereError(state, "CalcMixedAirSetPoint: Missing reference temperature setpoint for Mixed Air Setpoint Manager " + this->Name);
                ShowContinueError(state, "Node Referenced =" + state.dataLoopNodes->NodeID(RefNode));
                ShowContinueError(
                    state, "  use an additional Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at this node.");
                state.dataHVACGlobal->SetPointErrorFlag = true;
            } else {
                // need call to check if this is the target of an EnergyManagementSystem:Actuator object
                CheckIfNodeSetPointManagedByEMS(
                    state, RefNode, EMSManager::SPControlType::TemperatureSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                if (state.dataHVACGlobal->SetPointErrorFlag) {
                    ShowSevereError(state,
                                    "CalcMixedAirSetPoint: Missing reference temperature setpoint for Mixed Air Setpoint Manager " + this->Name);
                    ShowContinueError(state, "Node Referenced =" + state.dataLoopNodes->NodeID(RefNode));
                    ShowContinueError(
                        state, "  use an additional Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at this node.");
                    ShowContinueError(state, "Or add EMS Actuator to provide temperature setpoint at this node");
                }
            }
        }

        this->MySetPointCheckFlag = false;
    }

    this->SetPt =
        state.dataLoopNodes->Node(RefNode).TempSetPoint - (state.dataLoopNodes->Node(FanOutNode).Temp - state.dataLoopNodes->Node(FanInNode).Temp);
    if (CoolCoilInNode > 0 && CoolCoilOutNode > 0) {
        dtFan = state.dataLoopNodes->Node(FanOutNode).Temp - state.dataLoopNodes->Node(FanInNode).Temp;
        dtCoolCoil = state.dataLoopNodes->Node(CoolCoilInNode).Temp - state.dataLoopNodes->Node(CoolCoilOutNode).Temp;
        if (dtCoolCoil > 0.0 && MinTemp > state.dataEnvrn->OutDryBulbTemp) {
            this->FreezeCheckEnable = true;
            if (state.dataLoopNodes->Node(RefNode).Temp == state.dataLoopNodes->Node(CoolCoilOutNode).Temp) { // blow through
                this->SetPt = max(state.dataLoopNodes->Node(RefNode).TempSetPoint, MinTemp) - dtFan + dtCoolCoil;
            } else {                              // draw through
                if (RefNode != CoolCoilOutNode) { // Ref node is outlet node
                    this->SetPt = max(state.dataLoopNodes->Node(RefNode).TempSetPoint - dtFan, MinTemp) + dtCoolCoil;
                } else {
                    this->SetPt = max(state.dataLoopNodes->Node(RefNode).TempSetPoint, MinTemp) + dtCoolCoil;
                }
            }
        }
    }
}

void DefineOAPretreatSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. J. Witte based on CalcMixedAirSetPoint by Fred Buhl,
    //                        Work supported by ASHRAE research project 1254-RP
    //       DATE WRITTEN   January 2005
    //       MODIFIED       Witte (GARD), Sep 2006
    //                      Griffith( NREL), May 2009, added EMS setpoint checks
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Starting with the setpoint at the reference node, determine the required
    // outside air inlet conditions which when mixed with return air result in
    // the reference setpoint at the mixed air node.
    // (based on CalcMixedAirSetPoint)

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using EMSManager::CheckIfNodeSetPointManagedByEMS;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RefNode;                // setpoint reference node number
    int MixedOutNode;           // mixed air outlet node number
    int OAInNode;               // outside air inlet node number
    int ReturnInNode;           // return air inlet node number
    Real64 OAFraction;          // outside air fraction of mixed flow rate
    Real64 ReturnInValue = 0;   // return air inlet node mass flow rate
    Real64 RefNodeSetPoint = 0; // setpoint at reference node
    Real64 MinSetPoint = 0;     // minimum allowed setpoint
    Real64 MaxSetPoint = 0;     // maximum allowed setpoint
    bool HumiditySetPoint;      // logical to indicate if this is a humidity setpoint

    RefNode = this->RefNode;
    MixedOutNode = this->MixedOutNode;
    OAInNode = this->OAInNode;
    ReturnInNode = this->ReturnInNode;
    HumiditySetPoint = false;

    switch (this->CtrlTypeMode) {
    case CtrlVarType::Temp: { // 'Temperature'
        RefNodeSetPoint = state.dataLoopNodes->Node(RefNode).TempSetPoint;
        ReturnInValue = state.dataLoopNodes->Node(ReturnInNode).Temp;
        MinSetPoint = this->MinSetTemp;
        MaxSetPoint = this->MaxSetTemp;
    } break;
    case CtrlVarType::MaxHumRat: { // 'HUMRATMAX'
        RefNodeSetPoint = state.dataLoopNodes->Node(RefNode).HumRatMax;
        ReturnInValue = state.dataLoopNodes->Node(ReturnInNode).HumRat;
        MinSetPoint = this->MinSetHumRat;
        MaxSetPoint = this->MaxSetHumRat;
        HumiditySetPoint = true;
    } break;
    case CtrlVarType::MinHumRat: { // 'HUMRATMIN'
        RefNodeSetPoint = state.dataLoopNodes->Node(RefNode).HumRatMin;
        ReturnInValue = state.dataLoopNodes->Node(ReturnInNode).HumRat;
        MinSetPoint = this->MinSetHumRat;
        MaxSetPoint = this->MaxSetHumRat;
        HumiditySetPoint = true;
    } break;
    case CtrlVarType::HumRat: { // 'HumidityRatio'
        RefNodeSetPoint = state.dataLoopNodes->Node(RefNode).HumRatSetPoint;
        ReturnInValue = state.dataLoopNodes->Node(ReturnInNode).HumRat;
        MinSetPoint = this->MinSetHumRat;
        MaxSetPoint = this->MaxSetHumRat;
        HumiditySetPoint = true;
    } break;
    default:
        break;
    }

    if (!state.dataGlobal->SysSizingCalc && this->MySetPointCheckFlag) {
        this->MySetPointCheckFlag = false;
        if (RefNodeSetPoint == SensedNodeFlagValue) {
            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                ShowSevereError(state, "CalcOAPretreatSetPoint: Missing reference setpoint for Outdoor Air Pretreat Setpoint Manager " + this->Name);
                ShowContinueError(state, "Node Referenced =" + state.dataLoopNodes->NodeID(RefNode));
                ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at this node.");
                ShowFatalError(state, "Missing reference setpoint.");
            } else {
                bool LocalSetPointCheckFailed = false;
                switch (this->CtrlTypeMode) {
                case CtrlVarType::Temp: { // 'Temperature'
                    CheckIfNodeSetPointManagedByEMS(state, RefNode, EMSManager::SPControlType::TemperatureSetPoint, LocalSetPointCheckFailed);
                } break;
                case CtrlVarType::MaxHumRat: { // 'HUMRATMAX'
                    CheckIfNodeSetPointManagedByEMS(state, RefNode, EMSManager::SPControlType::HumidityRatioMaxSetPoint, LocalSetPointCheckFailed);
                } break;
                case CtrlVarType::MinHumRat: { // 'HUMRATMIN'
                    CheckIfNodeSetPointManagedByEMS(state, RefNode, EMSManager::SPControlType::HumidityRatioMinSetPoint, LocalSetPointCheckFailed);
                } break;
                case CtrlVarType::HumRat: { // 'HumidityRatio'
                    CheckIfNodeSetPointManagedByEMS(state, RefNode, EMSManager::SPControlType::HumidityRatioSetPoint, LocalSetPointCheckFailed);
                } break;
                default:
                    break;
                }
                if (LocalSetPointCheckFailed) {
                    ShowSevereError(state,
                                    "CalcOAPretreatSetPoint: Missing reference setpoint for Outdoor Air Pretreat Setpoint Manager " + this->Name);
                    ShowContinueError(state, "Node Referenced =" + state.dataLoopNodes->NodeID(RefNode));
                    ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at this node.");
                    ShowContinueError(state, "Or use an EMS actuator to control a setpoint at this node.");
                    ShowFatalError(state, "Missing reference setpoint.");
                }
            }
        }
    }
    if ((state.dataLoopNodes->Node(MixedOutNode).MassFlowRate <= 0.0) || (state.dataLoopNodes->Node(OAInNode).MassFlowRate <= 0.0)) {
        this->SetPt = RefNodeSetPoint;
    } else if (HumiditySetPoint && (RefNodeSetPoint == 0.0)) {
        // For humidity setpoints, zero is special meaning "off" or "no load"
        // so pass through zero setpoints without enforcing the max/min setpoint limits
        this->SetPt = 0.0;
    } else {
        OAFraction = state.dataLoopNodes->Node(OAInNode).MassFlowRate / state.dataLoopNodes->Node(MixedOutNode).MassFlowRate;
        this->SetPt = ReturnInValue + (RefNodeSetPoint - ReturnInValue) / OAFraction;
        // Apply maximum and minimum values
        this->SetPt = max(this->SetPt, MinSetPoint);
        this->SetPt = min(this->SetPt, MaxSetPoint);
    }
}

void DefineWarmestSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the "warmest" supply air setpoint temperature that will satisfy the cooling
    // requirements of all the zones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone sensible heat balance

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneLoad;         // required zone load [W]
    Real64 ZoneMassFlowMax;  // zone inlet maximum mass flow rate [kg/s]
    Real64 CpAir;            // inlet air specific heat [J/kg-C]
    int AirLoopNum;          // the index of the air loop served by this setpoint manager
    Real64 TotCoolLoad;      // sum of the zone cooling loads for this air loop [W]
    int ZonesCooledIndex;    // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;         // the controlled zone index
    int ZoneInletNode;       // the zone inlet node number
    Real64 ZoneTemp;         // zone temperature [C]
    Real64 ZoneSetPointTemp; // zone supply air temperature [C]
    Real64 SetPointTemp;     // the system setpoint temperature [C]
    int ZoneNode;            // the zone node number of the current zone
    int ZoneNum;             // the actual zone number

    AirLoopNum = this->AirLoopNum;
    TotCoolLoad = 0.0;
    SetPointTemp = this->MaxSetTemp;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
        ZoneMassFlowMax = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRateMax;
        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired;
        ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
        ZoneSetPointTemp = this->MaxSetTemp;
        if (ZoneLoad < 0.0) {
            TotCoolLoad += std::abs(ZoneLoad);
            CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
            if (ZoneMassFlowMax > SmallMassFlow) {
                ZoneSetPointTemp = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlowMax);
            }
        }
        SetPointTemp = min(SetPointTemp, ZoneSetPointTemp);
    }

    SetPointTemp = max(this->MinSetTemp, min(SetPointTemp, this->MaxSetTemp));
    if (TotCoolLoad < SmallLoad) {
        SetPointTemp = this->MaxSetTemp;
    }

    this->SetPt = SetPointTemp;
}

void DefineColdestSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the "coldest" supply air setpoint temperature that will satisfy the heating
    // requirements of all the zones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone sensible heat balance

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneLoad;         // required zone load [W]
    Real64 ZoneMassFlowMax;  // zone inlet maximum mass flow rate [kg/s]
    Real64 CpAir;            // inlet air specific heat [J/kg-C]
    int AirLoopNum;          // the index of the air loop served by this setpoint manager
    Real64 TotHeatLoad;      // sum of the zone heating loads for this air loop [W]
    int ZonesHeatedIndex;    // DO loop index for zones heated by the air loop
    int CtrlZoneNum;         // the controlled zone index
    int ZoneInletNode;       // the zone inlet node number
    Real64 ZoneTemp;         // zone temperature [C]
    Real64 ZoneSetPointTemp; // zone supply air temperature [C]
    Real64 SetPointTemp;     // the system setpoint temperature [C]
    int ZoneNode;            // the zone node number of the current zone
    int ZoneNum;             // the actual zone number

    AirLoopNum = this->AirLoopNum;
    TotHeatLoad = 0.0;
    SetPointTemp = this->MinSetTemp;

    if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesHeated > 0) {
        // dual-duct heated only zones
        for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesHeated; ++ZonesHeatedIndex) {
            CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums(ZonesHeatedIndex);
            ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).HeatZoneInletNodes(ZonesHeatedIndex);
            ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
            ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
            ZoneMassFlowMax = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRateMax;
            ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired;
            ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
            ZoneSetPointTemp = this->MinSetTemp;
            if (ZoneLoad > 0.0) {
                TotHeatLoad += ZoneLoad;
                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
                if (ZoneMassFlowMax > SmallMassFlow) {
                    ZoneSetPointTemp = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlowMax);
                }
            }
            SetPointTemp = max(SetPointTemp, ZoneSetPointTemp);
        }
    } else {
        // single-duct or central heated and cooled zones
        for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesHeatedIndex) {
            CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesHeatedIndex);
            ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesHeatedIndex);
            ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
            ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
            ZoneMassFlowMax = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRateMax;
            ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired;
            ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
            ZoneSetPointTemp = this->MinSetTemp;
            if (ZoneLoad > 0.0) {
                TotHeatLoad += ZoneLoad;
                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
                if (ZoneMassFlowMax > SmallMassFlow) {
                    ZoneSetPointTemp = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlowMax);
                }
            }
            SetPointTemp = max(SetPointTemp, ZoneSetPointTemp);
        }
    }

    SetPointTemp = min(this->MaxSetTemp, max(SetPointTemp, this->MinSetTemp));
    if (TotHeatLoad < SmallLoad) {
        SetPointTemp = this->MinSetTemp;
    }

    this->SetPt = SetPointTemp;
}

void DefWarmestSetPtManagerTempFlow::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2002
    //       MODIFIED       Haves, Oct 2004
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the "warmest" supply air setpoint temperature that will satisfy the cooling
    // requirements of all the zones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone sensible heat balance

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneLoad;         // required zone load [W]
    Real64 ZoneMassFlowMax;  // zone inlet maximum mass flow rate [kg/s]
    Real64 CpAir;            // inlet air specific heat [J/kg-C]
    int AirLoopNum;          // the index of the air loop served by this setpoint manager
    Real64 TotCoolLoad;      // sum of the zone cooling loads for this air loop [W]
    int ZonesCooledIndex;    // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;         // the controlled zone index
    int ZoneInletNode;       // the zone inlet node number
    Real64 ZoneTemp;         // zone temperature [C]
    Real64 ZoneSetPointTemp; // zone supply air temperature [C]
    Real64 SetPointTemp;     // the system setpoint temperature [C]
    int ZoneNode;            // the zone node number of the current zone
    int ZoneNum;             // the actual zone number
    Real64 MinFracFlow;
    Real64 ZoneFracFlow;
    Real64 FracFlow;
    Real64 MaxSetPointTemp;
    Real64 MinSetPointTemp;
    int CritZoneNumTemp;
    int CritZoneNumFlow;

    if (!this->SimReady) return;
    AirLoopNum = this->AirLoopNum;
    TotCoolLoad = 0.0;
    MaxSetPointTemp = this->MaxSetTemp;
    SetPointTemp = MaxSetPointTemp;
    MinSetPointTemp = this->MinSetTemp;
    MinFracFlow = this->MinTurndown;
    FracFlow = MinFracFlow;
    CritZoneNumTemp = 0;
    CritZoneNumFlow = 0;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ActualZoneNum;
        ZoneMassFlowMax = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRateMax;
        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired;
        ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
        ZoneSetPointTemp = MaxSetPointTemp;
        ZoneFracFlow = MinFracFlow;
        if (ZoneLoad < 0.0) {
            TotCoolLoad += std::abs(ZoneLoad);
            CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
            if (ZoneMassFlowMax > SmallMassFlow) {
                if (this->Strategy == ControlStrategy::TempFirst) {
                    // First find supply air temperature required to meet the load at minimum flow. If this is
                    // below the minimum supply air temperature, calculate the fractional flow rate required to meet the
                    // load at the minimum supply air temperature.
                    ZoneSetPointTemp = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlowMax * MinFracFlow);
                    if (ZoneSetPointTemp < MinSetPointTemp) {
                        ZoneFracFlow = (ZoneLoad / (CpAir * (MinSetPointTemp - ZoneTemp))) / ZoneMassFlowMax;
                    } else {
                        ZoneFracFlow = MinFracFlow;
                    }
                } else { // ControlStrategy = FlowFirst
                    // First find supply air flow rate required to meet the load at maximum supply air temperature. If this
                    // is above the maximum supply air flow rate, calculate the supply air temperature required to meet the
                    // load at the maximum flow.
                    ZoneFracFlow = (ZoneLoad / (CpAir * (MaxSetPointTemp - ZoneTemp))) / ZoneMassFlowMax;
                    if (ZoneFracFlow > 1.0 || ZoneFracFlow < 0.0) {
                        ZoneSetPointTemp = ZoneTemp + ZoneLoad / (CpAir * ZoneMassFlowMax);
                    } else {
                        ZoneSetPointTemp = MaxSetPointTemp;
                    }
                }
            }
        }
        if (ZoneSetPointTemp < SetPointTemp) {
            SetPointTemp = ZoneSetPointTemp;
            CritZoneNumTemp = ZoneNum;
        }
        if (ZoneFracFlow > FracFlow) {
            FracFlow = ZoneFracFlow;
            CritZoneNumFlow = ZoneNum;
        }
    }

    SetPointTemp = max(MinSetPointTemp, min(SetPointTemp, MaxSetPointTemp));
    FracFlow = max(MinFracFlow, min(FracFlow, 1.0));
    if (TotCoolLoad < SmallLoad) {
        SetPointTemp = MaxSetPointTemp;
        FracFlow = MinFracFlow;
    }

    this->SetPt = SetPointTemp;
    this->Turndown = FracFlow;
    if (this->Strategy == ControlStrategy::TempFirst) {
        if (CritZoneNumFlow != 0) {
            this->CritZoneNum = CritZoneNumFlow;
        } else {
            this->CritZoneNum = CritZoneNumTemp;
        }
    } else { // ControlStrategy = FlowFirst
        if (CritZoneNumTemp != 0) {
            this->CritZoneNum = CritZoneNumTemp;
        } else {
            this->CritZoneNum = CritZoneNumFlow;
        }
    }
}

void DefRABFlowSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Given the desired setpoint temperature, calulate the flow rate through the
    // return asir branch that will deliver the desired temperature at the loop outlet
    // node.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MixerRABInNode;  // Mixer RAB inlet node number
    int MixerSupInNode;  // Mixer supply inlet node number
    int MixerOutNode;    // Mixer outlet node number
    int LoopOutNode;     // loop outlet node number
    Real64 TempSetPt;    // the setpoint temperature (from schedule) [C]
    Real64 TempSetPtMod; // the setpoint temperature modified for fan heat gain [C]
    Real64 SupFlow;      // supply flow rate before mixing [kg/s]
    Real64 RABFlow;      // Return Air Bypass flow rate [kg/s]
    Real64 TotSupFlow;   // supply air flow after mixing [kg/s]
    Real64 TempSup;      // temperature of supply air before mixing [kg/s]
    Real64 TempRAB;      // temperature of return bypass air

    MixerRABInNode = this->RABMixInNode;
    MixerSupInNode = this->SupMixInNode;
    MixerOutNode = this->MixOutNode;
    LoopOutNode = this->SysOutNode;
    TempSetPt = GetCurrentScheduleValue(state, this->SchedPtr);
    TempSetPtMod = TempSetPt - (state.dataLoopNodes->Node(LoopOutNode).Temp - state.dataLoopNodes->Node(MixerOutNode).Temp);
    SupFlow = state.dataLoopNodes->Node(MixerSupInNode).MassFlowRate;
    TempSup = state.dataLoopNodes->Node(MixerSupInNode).Temp;
    TotSupFlow = state.dataLoopNodes->Node(MixerOutNode).MassFlowRate;
    TempRAB = state.dataLoopNodes->Node(MixerRABInNode).Temp;
    RABFlow = (TotSupFlow * TempSetPtMod - SupFlow * TempSup) / max(TempRAB, 1.0);
    RABFlow = min(RABFlow, TotSupFlow);
    RABFlow = max(0.0, RABFlow);
    this->FlowSetPt = RABFlow;
}

void DefMultiZoneAverageHeatingSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse, FSEC
    //       DATE WRITTEN   July 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the "Average" supply air setpoint temperature that will satisfy the heating
    // requirements of multizones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone sensible (heating load) heat balance around the zones served by a central air system

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneLoad;                 // zone load predicted to the setpoint [W]
    Real64 ZoneMassFlowRate;         // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
    Real64 CpAir;                    // inlet air specific heat [J/kg-C]
    int AirLoopNum;                  // the index of the air loop served by this setpoint manager
    Real64 SumHeatLoad;              // sum of the zone's predicted heating loads for this air loop [W]
    Real64 SumProductMdotCpTZoneTot; // sum of the product of zone inlet node actual mass flow rate,
    // Cp of air at zone air node and zone air node temperature for
    // all zones in the air loop [W]
    Real64 SumProductMdotCp; // sum of the product of zone inlet node actual mass flow rate, and
    // Cp of air at zone inlet node for all heated zones in the airloop [W/C]
    Real64 SumProductMdotCpTot; // sum of the product of zone inlet node actual mass flow rate, and
    // Cp of air at zone air node for all zones in the airloop [W/C]
    Real64 ZoneAverageTemp; // multizone average zone air node temperature [C]
    int ZonesHeatedIndex;   // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;        // the controlled zone index
    int ZoneInletNode;      // the zone inlet node number
    Real64 ZoneTemp;        // zone air node temperature [C]
    Real64 SetPointTemp;    // the system setpoint temperature [C]
    int ZoneNode;           // the zone node number of the current zone

    SumHeatLoad = 0.0;
    ZoneAverageTemp = 0.0;
    SumProductMdotCp = 0.0;
    SumProductMdotCpTot = 0.0;
    SumProductMdotCpTZoneTot = 0.0;
    AirLoopNum = this->AirLoopNum;
    SetPointTemp = this->MinSetTemp;

    for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesHeatedIndex) {
        // DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
        // Using AirToZoneNodeInfo(AirLoopNum)%Cool* structure variables since they include heating and cooling.

        // The data for number of zones heated is included in the data structure of the variable
        // "AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled" for all systems.  The data structure
        // "AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated" applies to Dual Duct System only and
        // if used will limit the application of this setpoint manager to other systems.  Thus,
        // the "AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled" data is used instead.

        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesHeatedIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesHeatedIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(CtrlZoneNum).TotalOutputRequired;
        ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);
        SumProductMdotCpTot += ZoneMassFlowRate * CpAir;
        SumProductMdotCpTZoneTot += ZoneMassFlowRate * CpAir * ZoneTemp;
        if (ZoneLoad > 0.0) {
            CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
            SumHeatLoad += ZoneLoad;
            SumProductMdotCp += ZoneMassFlowRate * CpAir;
        }
    }
    if (SumProductMdotCpTot > 0.0) ZoneAverageTemp = SumProductMdotCpTZoneTot / SumProductMdotCpTot;
    if (SumProductMdotCp > 0.0) SetPointTemp = ZoneAverageTemp + SumHeatLoad / SumProductMdotCp;

    SetPointTemp = min(this->MaxSetTemp, max(SetPointTemp, this->MinSetTemp));
    if (SumHeatLoad < SmallLoad) {
        SetPointTemp = this->MinSetTemp;
    }
    this->SetPt = SetPointTemp;
}

void DefMultiZoneAverageCoolingSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse, FSEC
    //       DATE WRITTEN   July 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the "Average" supply air setpoint temperature that will satisfy the cooling
    // requirements of all the zones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone sensible (cooling load) heat balance around the zones served by a central air system

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneLoad;                 // zone load predicted to the setpoint [W]
    Real64 ZoneMassFlowRate;         // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
    Real64 CpAir;                    // inlet air specific heat [J/kg-C]
    int AirLoopNum;                  // the index of the air loop served by this setpoint manager
    Real64 SumCoolLoad;              // sum of the zone cooling loads for this air loop [W]
    Real64 SumProductMdotCpTZoneTot; // sum of the product of zone inlet node actual mass flow rate,
    // Cp of air at zone air node and zone air node temperature for
    // all zones in the air loop [W]
    Real64 SumProductMdotCp; // sum of the product of zone inlet node actual mass flow rate, and
    // Cp of air at zone inlet node for cooled zones in the airloop [W/C]
    Real64 SumProductMdotCpTot; // sum of the product of zone inlet node actual mass flow rate, and
    // Cp of air at zone air node for all zones in the airloop [W/C]
    Real64 ZoneAverageTemp; // multizone average zone Air node temperature [C]
    int ZonesCooledIndex;   // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;        // the controlled zone index
    int ZoneInletNode;      // the zone inlet node number
    Real64 ZoneTemp;        // zone air node temperature [C]
    Real64 SetPointTemp;    // the system setpoint temperature [C]
    int ZoneNode;           // the zone node number of the current zone

    SumCoolLoad = 0.0;
    ZoneAverageTemp = 0.0;
    SumProductMdotCp = 0.0;
    SumProductMdotCpTot = 0.0;
    SumProductMdotCpTZoneTot = 0.0;
    AirLoopNum = this->AirLoopNum;
    SetPointTemp = this->MaxSetTemp;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(CtrlZoneNum).TotalOutputRequired;
        ZoneTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);
        SumProductMdotCpTot += ZoneMassFlowRate * CpAir;
        SumProductMdotCpTZoneTot += ZoneMassFlowRate * CpAir * ZoneTemp;
        if (ZoneLoad < 0.0) {
            CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneInletNode).HumRat);
            SumCoolLoad += ZoneLoad;
            SumProductMdotCp += ZoneMassFlowRate * CpAir;
        }
    }
    if (SumProductMdotCpTot > 0.0) ZoneAverageTemp = SumProductMdotCpTZoneTot / SumProductMdotCpTot;
    if (SumProductMdotCp > 0.0) SetPointTemp = ZoneAverageTemp + SumCoolLoad / SumProductMdotCp;

    SetPointTemp = max(this->MinSetTemp, min(SetPointTemp, this->MaxSetTemp));

    if (std::abs(SumCoolLoad) < SmallLoad) {
        SetPointTemp = this->MaxSetTemp;
    }

    this->SetPt = SetPointTemp;
}

void DefMultiZoneAverageMinHumSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse, FSEC
    //       DATE WRITTEN   July 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the "Average" supply air minimum humidity setpoint that will satisfy the minimum
    // humidity ratio requirements of multiple zones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone latent load balance around the zones served by a central air system

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MoistureLoad;         // zone's moisture load predicted to the setpoint [kgWater/s]
    Real64 ZoneMassFlowRate;     // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
    int AirLoopNum;              // the index of the air loop served by this setpoint manager
    Real64 SumMoistureLoad;      // sum of the zone moisture loads for this air loop [W]
    Real64 SumMdot;              // sum of the actual mass flow rate for controlled zones in the air loop [kg/s]
    Real64 SumMdotTot;           // sum of the actual mass flow rate for this air loop [kg/s]
    Real64 SumProductMdotHumTot; // sum of product of actual mass flow rate at the zone inlet node,
    // and humidity ratio at zones air node for all zones in the airloop [kgWater/s]
    Real64 AverageZoneHum; // multizone average zone air node humidity ratio of all zones in the air loop [kg/kg]
    int ZonesCooledIndex;  // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;       // the controlled zone index
    int ZoneInletNode;     // the zone inlet node number
    Real64 ZoneHum;        // zone air node humidity ratio [kg/kg]
    Real64 SetPointHum;    // system setpoint humidity ratio [kg/kg]
    int ZoneNode;          // the zone node number of the current zone

    SumMdot = 0.0;
    SumMdotTot = 0.0;
    AverageZoneHum = 0.0;
    SumMoistureLoad = 0.0;
    SumProductMdotHumTot = 0.0;
    AirLoopNum = this->AirLoopNum;
    SetPointHum = this->MinSetHum;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
        MoistureLoad = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(CtrlZoneNum).OutputRequiredToHumidifyingSP;
        ZoneHum = state.dataLoopNodes->Node(ZoneNode).HumRat;
        SumMdotTot += ZoneMassFlowRate;
        SumProductMdotHumTot += ZoneMassFlowRate * ZoneHum;
        // For humidification the moisture load is positive
        if (MoistureLoad > 0.0) {
            SumMdot += ZoneMassFlowRate;
            SumMoistureLoad += MoistureLoad;
        }
    }
    if (SumMdotTot > SmallMassFlow) AverageZoneHum = SumProductMdotHumTot / SumMdotTot;
    if (SumMdot > SmallMassFlow) SetPointHum = max(0.0, AverageZoneHum + SumMoistureLoad / SumMdot);

    SetPointHum = min(this->MaxSetHum, max(SetPointHum, this->MinSetHum));

    this->SetPt = SetPointHum;
}

void DefMultiZoneAverageMaxHumSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse, FSEC
    //       DATE WRITTEN   July 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the "Average" supply air maximum humidity setpoint that will satisfy the maximum
    // himudity ratio requirements of multiple zones served by a central air system.

    // METHODOLOGY EMPLOYED:
    // Zone latent load balance around the zones served by a central air system

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MoistureLoad;         // zone's moisture load predicted to the setpoint [kgWater/s]
    Real64 ZoneMassFlowRate;     // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
    int AirLoopNum;              // the index of the air loop served by this setpoint manager
    Real64 SumMoistureLoad;      // sum of the zone moisture loads for this air loop [W]
    Real64 SumMdot;              // sum of the actual mass flow rate for controlled zones in the air loop [kg/s]
    Real64 SumMdotTot;           // sum of the actual mass flow rate for this air loop [kg/s]
    Real64 SumProductMdotHumTot; // sum of product of actual mass flow rate at the zone inlet node,
    // and humidity ratio at zones air node for all zones in the airloop [kgWater/s]
    Real64 AverageZoneHum; // multizone average zone air node humidity ratio of all zones in the air loop [kg/kg]
    int ZonesCooledIndex;  // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;       // the controlled zone index
    int ZoneInletNode;     // the zone inlet node number
    Real64 ZoneHum;        // zone air node humidity ratio [kg/kg]
    //  REAL(r64)      :: AverageSetPointHum   ! Supply air humidity ratio [kg/kg]
    Real64 SetPointHum; // system setpoint humidity ratio [kg/kg]
    int ZoneNode;       // the zone node number of the current zone

    SumMdot = 0.0;
    SumMdotTot = 0.0;
    AverageZoneHum = 0.0;
    SumMoistureLoad = 0.0;
    SumProductMdotHumTot = 0.0;
    AirLoopNum = this->AirLoopNum;
    SetPointHum = this->MaxSetHum;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
        MoistureLoad = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(CtrlZoneNum).OutputRequiredToDehumidifyingSP;
        ZoneHum = state.dataLoopNodes->Node(ZoneNode).HumRat;
        SumMdotTot += ZoneMassFlowRate;
        SumProductMdotHumTot += ZoneMassFlowRate * ZoneHum;
        // For dehumidification the moisture load is negative
        if (MoistureLoad < 0.0) {
            SumMdot += ZoneMassFlowRate;
            SumMoistureLoad += MoistureLoad;
        }
    }
    if (SumMdotTot > SmallMassFlow) AverageZoneHum = SumProductMdotHumTot / SumMdotTot;
    if (SumMdot > SmallMassFlow) SetPointHum = max(0.0, AverageZoneHum + SumMoistureLoad / SumMdot);

    SetPointHum = max(this->MinSetHum, min(SetPointHum, this->MaxSetHum));
    this->SetPt = SetPointHum;
}

void DefMultiZoneMinHumSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse, FSEC/UCF
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the minimum supply air humidity ratio based on humidification requirements of
    // a controlled zone with critical humidification need (i.e., a zone with the highest
    // humidity ratio setpoint) in an air loop served by a central air-conditioner.
    // METHODOLOGY EMPLOYED:
    // Uses moisture mass balance to calculate the humidity ratio setpoint. The algorithm loops
    // over all the zones that a central air system can humidify and calculates the setpoint based
    // on a zone with the highest humidity ratio setpoint requirement:

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr SmallMoistureLoad(0.00001); // small moisture load [kgWater/s]

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopNum;              // the index of the air loop served by this setpoint manager
    int ZonesCooledIndex;        // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;             // the controlled zone index
    int ZoneInletNode;           // the zone inlet node number
    int ZoneNode;                // the zone node number of the current zone
    Real64 ZoneHum;              // zone air node humidity ratio [kg/kg]
    Real64 SetPointHum;          // system setpoint humidity ratio [kg/kg]
    Real64 ZoneSetPointHum;      // Zone setpoint humidity ratio [kg/kg]
    Real64 MoistureLoad;         // zone's moisture load predicted to the setpoint [kgWater/s]
    Real64 ZoneMassFlowRate;     // zone inlet node actual supply air mass flow rate [kg/s]
    Real64 SumMoistureLoad(0.0); // sum of the zone moisture loads for this air loop [W]

    AirLoopNum = this->AirLoopNum;
    SetPointHum = this->MinSetHum;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
        MoistureLoad = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(CtrlZoneNum).OutputRequiredToHumidifyingSP;
        ZoneHum = state.dataLoopNodes->Node(ZoneNode).HumRat;
        ZoneSetPointHum = this->MinSetHum;
        // For humidification the moisture load is positive
        if (MoistureLoad > 0.0) {
            SumMoistureLoad += MoistureLoad;
            if (ZoneMassFlowRate > SmallMassFlow) {
                ZoneSetPointHum = max(0.0, ZoneHum + MoistureLoad / ZoneMassFlowRate);
            }
        }
        SetPointHum = max(SetPointHum, ZoneSetPointHum);
    }
    SetPointHum = min(this->MaxSetHum, max(SetPointHum, this->MinSetHum));
    if (SumMoistureLoad < SmallMoistureLoad) {
        SetPointHum = this->MinSetHum;
    }
    this->SetPt = SetPointHum;
}

void DefMultiZoneMaxHumSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse, FSEC/UCF
    //       DATE WRITTEN   August 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the maximum supply air humidity ratio based on dehumidification requirements of
    // a controlled zone with critical dehumidification need (i.e., a zone with the lowest
    // humidity ratio setpoint) in an air loop served by a central air-conditioner.

    // METHODOLOGY EMPLOYED:
    // Uses moisture mass balance to calculate the humidity ratio setpoint. The algorithm loops
    // over all the zones that a central air system can dehumidify and calculates the setpoint
    // based on a zone with the lowest humidity ratio setpoint requirement:

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr SmallMoistureLoad(0.00001); // small moisture load [kgWater/s]

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopNum;              // the index of the air loop served by this setpoint manager
    int ZonesCooledIndex;        // DO loop index for zones cooled by the air loop
    int CtrlZoneNum;             // the controlled zone index
    int ZoneInletNode;           // the zone inlet node number
    int ZoneNode;                // the zone node number of the current zone
    Real64 ZoneHum;              // zone air node humidity ratio [kg/kg]
    Real64 SetPointHum;          // system setpoint humidity ratio [kg/kg]
    Real64 ZoneSetPointHum;      // Zone setpoint humidity ratio [kg/kg]
    Real64 MoistureLoad;         // zone's moisture load predicted to the setpoint [kgWater/s]
    Real64 ZoneMassFlowRate;     // zone inlet node actual supply air mass flow rate [kg/s]
    Real64 SumMoistureLoad(0.0); // sum of the zone moisture loads for this air loop [W]

    AirLoopNum = this->AirLoopNum;
    SetPointHum = this->MaxSetHum;

    for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZonesCooledIndex) {
        CtrlZoneNum = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledIndex);
        ZoneInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZonesCooledIndex);
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneNode;
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate;
        MoistureLoad = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(CtrlZoneNum).OutputRequiredToDehumidifyingSP;
        ZoneHum = state.dataLoopNodes->Node(ZoneNode).HumRat;
        ZoneSetPointHum = this->MaxSetHum;

        // For dehumidification the moisture load is negative
        if (MoistureLoad < 0.0) {
            SumMoistureLoad += MoistureLoad;
            if (ZoneMassFlowRate > SmallMassFlow) {
                ZoneSetPointHum = max(0.0, ZoneHum + MoistureLoad / ZoneMassFlowRate);
            }
        }
        SetPointHum = min(SetPointHum, ZoneSetPointHum);
    }
    SetPointHum = max(this->MinSetHum, min(SetPointHum, this->MaxSetHum));

    if (std::abs(SumMoistureLoad) < SmallMoistureLoad) {
        SetPointHum = this->MaxSetHum;
    }

    this->SetPt = SetPointHum;
}

void DefineFollowOATempSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma, FSEC
    //       DATE WRITTEN   July 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the setpoint based on outdoor air dry-bulb/wet-bulb temperature

    // METHODOLOGY EMPLOYED:
    // Based on reference temperature type specifed in the setpoint manager,
    // the setpoint is calculated as OutWetBulbTemp(Or OutDryBulbTemp) + Offset.
    // The sign convention is that a positive Offset will increase the resulting setpoint.
    // Final value of the setpoint is limited by the Max and Min limit specified in the setpoint manager.

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    //  INTEGER      :: CtrldNodeNum    ! index of the items in the controlled node list
    Real64 MinSetPoint; // minimum allowed setpoint
    Real64 MaxSetPoint; // maximum allowed setpoint

    MaxSetPoint = this->MaxSetTemp;
    MinSetPoint = this->MinSetTemp;

    switch (this->RefTypeMode) {
    case ReferenceTempType::WetBulb: {
        this->SetPt = state.dataEnvrn->OutWetBulbTemp + this->Offset;
    } break;
    case ReferenceTempType::DryBulb: {
        this->SetPt = state.dataEnvrn->OutDryBulbTemp + this->Offset;
    } break;
    default:
        break;
    }

    // Apply maximum and minimum values
    this->SetPt = max(this->SetPt, MinSetPoint);
    this->SetPt = min(this->SetPt, MaxSetPoint);
}

void DefineFollowSysNodeTempSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma, FSEC
    //       DATE WRITTEN   July 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the setpoint based on current temperatures at a separate system node.

    // METHODOLOGY EMPLOYED:
    // The current value of the temperature at a reference node are obtained and used
    // to generate setpoint on a second system node.  If the reference node is also designated
    // to be an outdoor air (intake) node, then this setpoint manager can be used to follow
    // outdoor air conditions that are adjusted for altitude.
    // Also, based on reference temperature type specifed in the setpoint manager, the out door air wet-bulb
    // or dry-bulb temperature at the reference node could be used.
    // A temperature offset will be applied to the value obtained from the reference system node.
    // If this value is zero, and the limits are met, then the resulting setpoint will be exactly the same
    // as the reference system node temperature.  The sign convention is that a positive offset will increase
    // the resulting setpoint.

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RefNode;        // setpoint reference node number
    Real64 RefNodeTemp; // setpoint at reference node
    Real64 MinSetPoint; // minimum allowed setpoint
    Real64 MaxSetPoint; // maximum allowed setpoint

    RefNodeTemp = 0.0;

    MaxSetPoint = this->MaxSetTemp;
    MinSetPoint = this->MinSetTemp;

    RefNode = this->RefNodeNum;

    switch (this->RefTypeMode) {
    case ReferenceTempType::WetBulb: {
        if (allocated(state.dataLoopNodes->MoreNodeInfo)) {
            RefNodeTemp = state.dataLoopNodes->MoreNodeInfo(RefNode).WetBulbTemp;
        }
    } break;
    case ReferenceTempType::DryBulb: {
        RefNodeTemp = state.dataLoopNodes->Node(RefNode).Temp;
    } break;
    default:
        break;
    }

    this->SetPt = RefNodeTemp + this->Offset;

    // Apply maximum and minimum values
    this->SetPt = max(this->SetPt, MinSetPoint);
    this->SetPt = min(this->SetPt, MaxSetPoint);
}

void DefineGroundTempSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma, FSEC
    //       DATE WRITTEN   July 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set the setpoint based on current ground temperature

    // METHODOLOGY EMPLOYED:
    // Based on reference ground temperature object type specifed in the setpoint manager,
    // the setpoint is calculated as GroundTemperature + Offset.
    // The sign convention is that a positive Offset will increase the resulting setpoint.
    // Final value of the setpoint is limited by the Max and Min limit specified in the setpoint manager.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MinSetPoint; // minimum allowed setpoint
    Real64 MaxSetPoint; // maximum allowed setpoint

    MaxSetPoint = this->MaxSetTemp;
    MinSetPoint = this->MinSetTemp;

    switch (this->RefTypeMode) {
    case ReferenceGroundTempObjectType::BuildingSurface: {
        this->SetPt = state.dataEnvrn->GroundTemp + this->Offset;
    } break;
    case ReferenceGroundTempObjectType::Shallow: {
        this->SetPt = state.dataEnvrn->GroundTemp_Surface + this->Offset;
    } break;
    case ReferenceGroundTempObjectType::Deep: {
        this->SetPt = state.dataEnvrn->GroundTemp_Deep + this->Offset;
    } break;
    case ReferenceGroundTempObjectType::FCFactorMethod: {
        this->SetPt = state.dataEnvrn->GroundTempFC + this->Offset;
    } break;
    default:
        break;
    }

    // Apply maximum and minimum values
    this->SetPt = max(this->SetPt, MinSetPoint);
    this->SetPt = min(this->SetPt, MaxSetPoint);
}

void DefineCondEntSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Atefe Makhmalbaf and Heejin Cho, PNNL
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the optimal condenser water temperature set point for a chiller plant
    // with one or more chillers.  The condenser water leaving the tower should be at this temperature
    // for optimal operation of the chiller plant.

    // METHODOLOGY EMPLOYED:
    // using one curve to determine the optimum condenser entering water temperature for a given timestep
    // and two other curves to place boundary conditions on the optimal setpoint value.

    // REFERENCES:
    // na

    // Using/Aliasing
    using CurveManager::CurveValue;
    using ScheduleManager::GetCurrentScheduleValue;
    using namespace DataPlant;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    //////////// hoisted into namespace ////////////////////////////////////////////////
    // static Real64 Dsn_EntCondTemp( 0.0 ); // The chiller design entering condenser temp, C; e.g. 29.44C {85F} // DCESPMDsn_EntCondTemp
    // static Real64 Dsn_MinCondSetpt( 0.0 ); // The design minimum condenser water temp, C; e.g. 18.33C {65 F} // DCESPMDsn_MinCondSetpt
    // static Real64 Cur_MinLiftTD( 0.0 ); // Minimum lift (TCond entering - Tevap leaving) TD this timestep // DCESPMCur_MinLiftTD
    // static Real64 Design_Load_Sum( 0.0 ); // the design load of the chillers, W // DCESPMDesign_Load_Sum
    // static Real64 Actual_Load_Sum( 0.0 ); // the actual load of the chillers, W // DCESPMActual_Load_Sum
    // static Real64 Weighted_Actual_Load_Sum( 0.0 ); // Intermediate weighted value of actual load on plant, W // DCESPMWeighted_Actual_Load_Sum
    // static Real64 Weighted_Design_Load_Sum( 0.0 ); // Intermediate weighted value of design load on plant, W // DCESPMWeighted_Design_Load_Sum
    // static Real64 Weighted_Ratio( 0.0 ); // Weighted part load ratio of chillers // DCESPMWeighted_Ratio
    // static Real64 Min_DesignWB( 0.0 ); // Minimum design twr wet bulb allowed, C // DCESPMMin_DesignWB
    // static Real64 Min_ActualWb( 0.0 ); // Minimum actual oa wet bulb allowed, C // DCESPMMin_ActualWb
    // static Real64 Opt_CondEntTemp( 0.0 ); // Optimized Condenser entering water temperature setpoint this timestep, C // DCESPMOpt_CondEntTemp
    // static Real64 DesignClgCapacity_Watts( 0.0 ); // DCESPMDesignClgCapacity_Watts
    // static Real64 CurrentLoad_Watts( 0.0 ); // DCESPMCurrentLoad_Watts
    // static Real64 CondInletTemp( 0.0 ); // Condenser water inlet temperature (C) // DCESPMCondInletTemp
    // static Real64 EvapOutletTemp( 0.0 ); // Evaporator water outlet temperature (C) // DCESPMEvapOutletTemp
    ////////////////////////////////////////////////////////////////////////////////////
    Real64 NormDsnCondFlow(0.0);        // Normalized design condenser flow for cooling towers, m3/s per watt
    Real64 Twr_DesignWB(0.0);           // The cooling tower design inlet air wet bulb temperature, C
    Real64 Dsn_CondMinThisChiller(0.0); // Design Minimum Condenser Entering for current chillers this timestep
    Real64 temp_MinLiftTD(0.0);         // Intermediate variable associated with lift (TCond entering - Tevap leaving) TD
    Real64 Des_Load(0.0);               // array of chiller design loads
    Real64 Act_Load(0.0);               // array of chiller actual loads
    Real64 ALW(0.0);                    // Actual load weighting of each chiller, W
    Real64 DLW(0.0);                    // Design capacity of each chiller, W
    Real64 SetPoint(0.0);               // Condenser entering water temperature setpoint this timestep, C
    Real64 CondWaterSetPoint(0.0);      // Condenser entering water temperature setpoint this timestep, C
    Real64 TempDesCondIn(0.0);          // Design condenser inlet temp. C , or 25.d0
    Real64 TempEvapOutDesign(0.0);      // design evaporator outlet temperature, water side
    Real64 CurLoad(0.0);
    int ChillerIndexPlantSide(0);
    int ChillerIndexDemandSide(0);
    int BranchIndexPlantSide(0);
    int BranchIndexDemandSide(0);
    int LoopIndexPlantSide(0);
    int LoopIndexDemandSide(0);
    DataPlant::PlantEquipmentType Type(DataPlant::PlantEquipmentType::Invalid);

    // Get from tower design values
    NormDsnCondFlow = 5.38e-8; // m3/s per watt (typically 3 gpm/ton)=(Volume of condenser fluid)/(ton of heat rejection)

    // Grab tower design inlet air wet bulb from setpoint manager
    Twr_DesignWB = this->TowerDsnInletAirWetBulb;

    // Current timestep's condenser water entering setpoint
    CondWaterSetPoint = GetCurrentScheduleValue(state, this->CondEntTempSchedPtr);
    LoopIndexPlantSide = this->LoopIndexPlantSide;
    ChillerIndexPlantSide = this->ChillerIndexPlantSide;
    BranchIndexPlantSide = this->BranchIndexPlantSide;
    Type = this->Type;
    LoopIndexDemandSide = this->LoopIndexDemandSide;
    ChillerIndexDemandSide = this->ChillerIndexDemandSide;
    BranchIndexDemandSide = this->BranchIndexDemandSide;

    // If chiller is on
    CurLoad = std::abs(state.dataPlnt->PlantLoop(LoopIndexPlantSide)
                           .LoopSide(LoopSideLocation::Supply)
                           .Branch(BranchIndexPlantSide)
                           .Comp(ChillerIndexPlantSide)
                           .MyLoad);
    if (CurLoad > 0) {
        if (Type == PlantEquipmentType::Chiller_Absorption || Type == PlantEquipmentType::Chiller_CombTurbine ||
            Type == PlantEquipmentType::Chiller_Electric || Type == PlantEquipmentType::Chiller_ElectricReformEIR ||
            Type == PlantEquipmentType::Chiller_EngineDriven) {
            TempDesCondIn = state.dataPlnt->PlantLoop(LoopIndexPlantSide)
                                .LoopSide(LoopSideLocation::Supply)
                                .Branch(BranchIndexPlantSide)
                                .Comp(ChillerIndexPlantSide)
                                .TempDesCondIn;
            state.dataSetPointManager->DCESPMCondInletTemp = state.dataLoopNodes
                                                                 ->Node(state.dataPlnt->PlantLoop(LoopIndexDemandSide)
                                                                            .LoopSide(LoopSideLocation::Demand)
                                                                            .Branch(BranchIndexDemandSide)
                                                                            .Comp(ChillerIndexDemandSide)
                                                                            .NodeNumIn)
                                                                 .Temp;
            state.dataSetPointManager->DCESPMEvapOutletTemp = state.dataLoopNodes
                                                                  ->Node(state.dataPlnt->PlantLoop(LoopIndexPlantSide)
                                                                             .LoopSide(LoopSideLocation::Supply)
                                                                             .Branch(BranchIndexPlantSide)
                                                                             .Comp(ChillerIndexPlantSide)
                                                                             .NodeNumOut)
                                                                  .Temp;
            TempEvapOutDesign = state.dataPlnt->PlantLoop(LoopIndexPlantSide)
                                    .LoopSide(LoopSideLocation::Supply)
                                    .Branch(BranchIndexPlantSide)
                                    .Comp(ChillerIndexPlantSide)
                                    .TempDesEvapOut;
            state.dataSetPointManager->DCESPMDesignClgCapacity_Watts = state.dataPlnt->PlantLoop(LoopIndexPlantSide)
                                                                           .LoopSide(LoopSideLocation::Supply)
                                                                           .Branch(BranchIndexPlantSide)
                                                                           .Comp(ChillerIndexPlantSide)
                                                                           .MaxLoad;
            state.dataSetPointManager->DCESPMCurrentLoad_Watts = state.dataPlnt->PlantLoop(LoopIndexPlantSide).CoolingDemand;
        } else if (Type == PlantEquipmentType::Chiller_Indirect_Absorption || Type == PlantEquipmentType::Chiller_DFAbsorption) {
            TempDesCondIn = state.dataPlnt->PlantLoop(LoopIndexPlantSide)
                                .LoopSide(LoopSideLocation::Supply)
                                .Branch(BranchIndexPlantSide)
                                .Comp(ChillerIndexPlantSide)
                                .TempDesCondIn;
            TempEvapOutDesign = 6.666;
        } else {
            TempDesCondIn = 25.0;
            TempEvapOutDesign = 6.666;
        }

        // for attached chillers (that are running this timestep) find their Dsn_MinCondSetpt and Dsn_EntCondTemp
        state.dataSetPointManager->DCESPMDsn_MinCondSetpt = 999.0;
        state.dataSetPointManager->DCESPMDsn_EntCondTemp = 0.0;

        // Design Minimum Condenser Entering as a function of the minimum lift and TEvapLvg
        // for chillers operating on current cond loop this timestep
        Dsn_CondMinThisChiller = TempEvapOutDesign + (this->MinimumLiftTD);
        state.dataSetPointManager->DCESPMDsn_MinCondSetpt = min(state.dataSetPointManager->DCESPMDsn_MinCondSetpt, Dsn_CondMinThisChiller);

        // Design entering condenser water temperature for chillers operating
        // on current cond loop this timestep
        state.dataSetPointManager->DCESPMDsn_EntCondTemp = max(state.dataSetPointManager->DCESPMDsn_EntCondTemp, TempDesCondIn);

        // Load this array with the design capacity and actual load of each chiller this timestep
        Des_Load = state.dataSetPointManager->DCESPMDesignClgCapacity_Watts;
        Act_Load = state.dataSetPointManager->DCESPMCurrentLoad_Watts;

        // ***** Load Calculations *****
        // In this section the sum of the actual load (watts) and design load (watts)
        // of the chillers that are on is calculated.
        state.dataSetPointManager->DCESPMActual_Load_Sum += Act_Load;
        state.dataSetPointManager->DCESPMDesign_Load_Sum += Des_Load;

        // Exit if the chillers are all off this hour
        if (state.dataSetPointManager->DCESPMActual_Load_Sum <= 0) {
            CondWaterSetPoint = state.dataSetPointManager->DCESPMDsn_EntCondTemp;
            return;
        }

        // ***** Weighted Ratio Calculation *****
        // This section first calculates the actual (ALW) and design (DLW) individual
        // weights. Then the weighted actual and design loads are computed. Finally
        // the Weighted Ratio is found.
        if (state.dataSetPointManager->DCESPMActual_Load_Sum != 0 && state.dataSetPointManager->DCESPMDesign_Load_Sum != 0) {
            ALW = ((Act_Load / state.dataSetPointManager->DCESPMActual_Load_Sum) * Act_Load);
            DLW = ((Des_Load / state.dataSetPointManager->DCESPMDesign_Load_Sum) * Des_Load);
        } else {
            ALW = 0.0;
            DLW = 0.0;
        }
        state.dataSetPointManager->DCESPMWeighted_Actual_Load_Sum += ALW;
        state.dataSetPointManager->DCESPMWeighted_Design_Load_Sum += DLW;
        state.dataSetPointManager->DCESPMWeighted_Ratio =
            state.dataSetPointManager->DCESPMWeighted_Actual_Load_Sum / state.dataSetPointManager->DCESPMWeighted_Design_Load_Sum;

        // ***** Optimal Temperature Calculation *****
        // In this section the optimal temperature is computed along with the minimum
        // design wet bulb temp and the minimum actual wet bulb temp.
        // Min_DesignWB = ACoef1 + ACoef2*OaWb + ACoef3*WPLR + ACoef4*TwrDsnWB + ACoef5*NF
        state.dataSetPointManager->DCESPMMin_DesignWB = CurveValue(state,
                                                                   this->MinTwrWbCurve,
                                                                   state.dataEnvrn->OutWetBulbTemp,
                                                                   state.dataSetPointManager->DCESPMWeighted_Ratio,
                                                                   Twr_DesignWB,
                                                                   NormDsnCondFlow);

        // Min_ActualWb = BCoef1 + BCoef2*MinDsnWB + BCoef3*WPLR + BCoef4*TwrDsnWB + BCoef5*NF
        state.dataSetPointManager->DCESPMMin_ActualWb = CurveValue(state,
                                                                   this->MinOaWbCurve,
                                                                   state.dataSetPointManager->DCESPMMin_DesignWB,
                                                                   state.dataSetPointManager->DCESPMWeighted_Ratio,
                                                                   Twr_DesignWB,
                                                                   NormDsnCondFlow);

        // Opt_CondEntTemp = CCoef1 + CCoef2*OaWb + CCoef3*WPLR + CCoef4*TwrDsnWB + CCoef5*NF
        state.dataSetPointManager->DCESPMOpt_CondEntTemp = CurveValue(state,
                                                                      this->OptCondEntCurve,
                                                                      state.dataEnvrn->OutWetBulbTemp,
                                                                      state.dataSetPointManager->DCESPMWeighted_Ratio,
                                                                      Twr_DesignWB,
                                                                      NormDsnCondFlow);

        // ***** Calculate (Cond ent - Evap lvg) Section *****
        // In this section we find the worst case of (Cond ent - Evap lvg) for the
        // chillers that are running.
        state.dataSetPointManager->DCESPMCur_MinLiftTD = 9999.0;
        // temp_MinLiftTD = 20.0 / 1.8;
        temp_MinLiftTD = state.dataSetPointManager->DCESPMCondInletTemp - state.dataSetPointManager->DCESPMEvapOutletTemp;
        state.dataSetPointManager->DCESPMCur_MinLiftTD = min(state.dataSetPointManager->DCESPMCur_MinLiftTD, temp_MinLiftTD);
    }

    // ***** Limit conditions Section *****
    // Check for limit conditions and control to the proper value.
    if ((state.dataSetPointManager->DCESPMWeighted_Ratio >= 0.90) &&
        (state.dataSetPointManager->DCESPMOpt_CondEntTemp >= (state.dataSetPointManager->DCESPMDsn_EntCondTemp + 1.0))) {
        // Optimized value exceeds the design condenser entering condition or chillers
        // near full load condition; reset condenser entering setpoint to its design value
        SetPoint = state.dataSetPointManager->DCESPMDsn_EntCondTemp + 1.0;
    } else {
        if ((state.dataEnvrn->OutWetBulbTemp >= state.dataSetPointManager->DCESPMMin_ActualWb) &&
            (Twr_DesignWB >= state.dataSetPointManager->DCESPMMin_DesignWB) &&
            (state.dataSetPointManager->DCESPMCur_MinLiftTD > this->MinimumLiftTD)) {
            // Boundaries are satified; use optimized condenser entering water temp
            SetPoint = state.dataSetPointManager->DCESPMOpt_CondEntTemp;
        } else {
            // Boundaries violated; Reset to scheduled value of condenser water entering setpoint
            SetPoint = CondWaterSetPoint;
        }
    }
    // Do not allow new setpoint to be less than the design condenser minimum entering condition,
    // i.e., TCondWaterEnt not allowed to be less than DsnEvapWaterLvg + MinimumLiftTD
    CondWaterSetPoint = max(SetPoint, state.dataSetPointManager->DCESPMDsn_MinCondSetpt);
    this->SetPt = CondWaterSetPoint;
}

void DefineIdealCondEntSetPointManager::calculate(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Heejin Cho, PNNL
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the optimal condenser water entering temperature set point for a chiller plant.

    // METHODOLOGY EMPLOYED:
    // The "ideal" chiller-tower optimization scheme uses a search algorithm to find the ideal optimal setpoint
    // at a given timestep. This requires resimulating HVAC systems at each timestep until finding
    // an "optimal" condenser water entering setpoint (OptSetpoint) which gives the minimum total chiller,
    // cooling tower, chilled water pump and condenser water pump power consumption.
    // The OptSetpoint falls between realistic minimum and maximum boundaries, which are set by the user.
    // The minimum boundary is determined based on the minimum lift (user input)
    // and evaporator leaving water temperature. The maximum boundary is specified by the user.
    // It is assumed that a single minimum point exists between these boundaries.

    // Using/Aliasing
    using namespace DataPlant;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &CondWaterSetPoint = state.dataSetPointManager->CondWaterSetPoint;
    auto &EvapOutletTemp = state.dataSetPointManager->EvapOutletTemp;
    auto &CondTempLimit = state.dataSetPointManager->CondTempLimit;
    auto &CurLoad = state.dataSetPointManager->CurLoad;
    auto &TotEnergy = state.dataSetPointManager->TotEnergy;
    auto &TotEnergyPre = state.dataSetPointManager->TotEnergyPre;

    if (state.dataGlobal->MetersHaveBeenInitialized) {
        // Setup meter vars
        if (this->SetupIdealCondEntSetPtVars) {
            this->SetupMeteredVarsForSetPt(state);
            this->SetupIdealCondEntSetPtVars = false;
        }
    }

    if (state.dataGlobal->MetersHaveBeenInitialized && state.dataGlobal->RunOptCondEntTemp) {

        // If chiller is on
        CurLoad = std::abs(state.dataPlnt->PlantLoop(this->LoopIndexPlantSide)
                               .LoopSide(LoopSideLocation::Supply)
                               .Branch(this->BranchIndexPlantSide)
                               .Comp(this->ChillerIndexPlantSide)
                               .MyLoad);

        if (CurLoad > 0) {

            // Calculate the minimum condenser inlet temperature boundary for set point
            if (this->Type == PlantEquipmentType::Chiller_Absorption || this->Type == PlantEquipmentType::Chiller_CombTurbine ||
                this->Type == PlantEquipmentType::Chiller_Electric || this->Type == PlantEquipmentType::Chiller_ElectricReformEIR ||
                this->Type == PlantEquipmentType::Chiller_EngineDriven) {
                EvapOutletTemp = state.dataLoopNodes
                                     ->Node(state.dataPlnt->PlantLoop(this->LoopIndexPlantSide)
                                                .LoopSide(LoopSideLocation::Supply)
                                                .Branch(this->BranchIndexPlantSide)
                                                .Comp(this->ChillerIndexPlantSide)
                                                .NodeNumOut)
                                     .Temp;
            } else {
                EvapOutletTemp = 6.666;
            }
            CondTempLimit = this->MinimumLiftTD + EvapOutletTemp;

            TotEnergy = this->calculateCurrentEnergyUsage(state);

            this->setupSetPointAndFlags(TotEnergy,
                                        TotEnergyPre,
                                        CondWaterSetPoint,
                                        CondTempLimit,
                                        state.dataGlobal->RunOptCondEntTemp,
                                        state.dataSetPointManager->RunSubOptCondEntTemp,
                                        state.dataSetPointManager->RunFinalOptCondEntTemp);

        } else {
            CondWaterSetPoint = this->MaxCondEntTemp;
            TotEnergyPre = 0.0;
            state.dataGlobal->RunOptCondEntTemp = false;
            state.dataSetPointManager->RunSubOptCondEntTemp = false;
        }
    } else {
        CondWaterSetPoint = this->MaxCondEntTemp;
        state.dataGlobal->RunOptCondEntTemp = false;
        state.dataSetPointManager->RunSubOptCondEntTemp = false;
    }

    this->SetPt = CondWaterSetPoint;
}

void DefineIdealCondEntSetPointManager::setupSetPointAndFlags(Real64 &TotEnergy,
                                                              Real64 &TotEnergyPre,
                                                              Real64 &CondWaterSetPoint,
                                                              Real64 &CondTempLimit,
                                                              bool &RunOptCondEntTemp,
                                                              bool &RunSubOptCondEntTemp,
                                                              bool &RunFinalOptCondEntTemp) const
{
    Real64 DeltaTotEnergy;
    if (TotEnergyPre != 0.0) {
        // Calculate the total energy consumption difference
        DeltaTotEnergy = TotEnergyPre - TotEnergy;
        // Search for the minimum total energy consumption
        if ((DeltaTotEnergy > 0) && (CondWaterSetPoint >= CondTempLimit) && (!RunFinalOptCondEntTemp)) {
            if (!RunSubOptCondEntTemp) {
                --CondWaterSetPoint;
                RunOptCondEntTemp = true;
            } else {
                CondWaterSetPoint -= 0.2;
                RunOptCondEntTemp = true;
            }
            TotEnergyPre = TotEnergy;
            // Set smaller set point (0.2 degC) decrease
        } else if ((DeltaTotEnergy < 0) && (!RunSubOptCondEntTemp) && (CondWaterSetPoint > CondTempLimit) && (!RunFinalOptCondEntTemp)) {
            CondWaterSetPoint += 0.8;
            RunOptCondEntTemp = true;
            RunSubOptCondEntTemp = true;
        } else {
            if (!RunFinalOptCondEntTemp) {
                CondWaterSetPoint += 0.2;
                RunOptCondEntTemp = true;
                RunSubOptCondEntTemp = false;
                RunFinalOptCondEntTemp = true;
            } else {
                // CondWaterSetPoint = CondWaterSetPoint; // Self-assignment commented out
                TotEnergyPre = 0.0;
                RunOptCondEntTemp = false;
                RunSubOptCondEntTemp = false;
                RunFinalOptCondEntTemp = false;
            }
        }
    } else {
        CondWaterSetPoint = this->MaxCondEntTemp - 1.0;
        TotEnergyPre = TotEnergy;
        RunOptCondEntTemp = true;
        RunSubOptCondEntTemp = false;
    }
}

Real64 DefineIdealCondEntSetPointManager::calculateCurrentEnergyUsage(EnergyPlusData &state)
{

    Real64 ChillerEnergy(0.0);     // Chiller energy consumption
    Real64 ChilledPumpEnergy(0.0); // Chilled water pump energy consumption
    Real64 TowerFanEnergy(0.0);    // Cooling tower fan energy consumption
    Real64 CondPumpEnergy(0.0);    // Condenser water pump energy consumption

    // Energy consumption metered variable number = 1

    // Get the chiller energy consumption
    ChillerEnergy = GetInternalVariableValue(state, this->ChllrVarType, this->ChllrVarIndex);

    // Get the chilled water pump energy consumption
    ChilledPumpEnergy = GetInternalVariableValue(state, this->ChlPumpVarType, this->ChlPumpVarIndex);

    // Get the cooling tower fan energy consumption
    TowerFanEnergy = 0;
    for (int i = 1; i <= this->numTowers; i++) {
        TowerFanEnergy += GetInternalVariableValue(state, this->ClTowerVarType(i), this->ClTowerVarIndex(i));
    }

    // Get the condenser pump energy consumption
    CondPumpEnergy = GetInternalVariableValue(state, this->CndPumpVarType, this->CndPumpVarIndex);

    // Calculate the total energy consumption
    return (ChillerEnergy + ChilledPumpEnergy + TowerFanEnergy + CondPumpEnergy);
}

void DefineReturnWaterChWSetPointManager::calculate(EnergyPlusData &state, DataLoopNode::NodeData &returnNode, DataLoopNode::NodeData &supplyNode)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee, NREL
    //       DATE WRITTEN   May 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the plant supply temperature reset required to achieve a target plant return temperature

    // METHODOLOGY EMPLOYED:
    // The setpoint manager follows this procedure:
    //  1. Calculate the current demand
    //    a. Sense the current return temperature
    //    b. Sense the current supply temperature
    //    c. Sense the current flow rate
    //    d. Approximate the fluid properties (rho, Cp) from the temperatures
    //    ---> Use these to calculate the demand with Q_demand = V_dot * rho * C_p * (T_return_sensed - T_supply_sensed)
    //  2. Calculate a new value of supply setpoint that will reject this much Q_demand, while providing a target return temperature
    //    * this assumes that the demand will be the same value on the next time around
    //    * at any time step, the value of target return temperature may vary if it is scheduled (or actuated with EMS)
    //    a. T_supply_setpoint = T_return_target - Q_demand / ( V_dot * rho * C_p )
    //  3. Constrain this value to limits
    //    a. T_supply_setpoint will be within: [ Design Chilled Water Supply Temperature, Maximum Supply Water Reset Temperature ]

    // NOTES:
    // The assumptions related to lagging of setpoint are most suited for smaller timesteps and/or plants that don't vary wildly from one time
    // step to another The assumptions also become affected by variable flow plants more-so than constant-flow plants

    // Using/Aliasing
    using namespace DataPlant;

    // we need to know the plant to get the fluid ID in case it is glycol
    // but we have to wait in case plant isn't initialized yet
    // if plant isn't initialized, assume index=1 (water)
    int fluidIndex = 1;
    if (this->plantLoopIndex == 0) {
        for (int plantIndex = 1; plantIndex <= state.dataPlnt->TotNumLoops; plantIndex++) {
            if (this->supplyNodeIndex == state.dataPlnt->PlantLoop(plantIndex).LoopSide(DataPlant::LoopSideLocation::Supply).NodeNumOut) {
                this->plantLoopIndex = plantIndex;
                this->plantSetpointNodeIndex = state.dataPlnt->PlantLoop(plantIndex).TempSetPointNodeNum;
                fluidIndex = state.dataPlnt->PlantLoop(plantIndex).FluidIndex;
                // now that we've found the plant populated, let's verify that the nodes match
                if (!PlantUtilities::verifyTwoNodeNumsOnSamePlantLoop(state, this->supplyNodeIndex, this->returnNodeIndex)) {
                    ShowSevereError(state, "Node problem for SetpointManager:ReturnTemperature:ChilledWater.");
                    ShowContinueError(state, "Return and Supply nodes were not found on the same plant loop.  Verify node names.");
                    ShowFatalError(state, "Simulation aborts due to setpoint node problem");
                }
            }
        }
    }

    // we don't need fluid names since we have a real index, so just pass in the temperature and get properties
    Real64 avgTemp = (returnNode.Temp + supplyNode.Temp) / 2;
    Real64 cp = FluidProperties::GetSpecificHeatGlycol(state, "", avgTemp, fluidIndex, "ReturnWaterChWSetPointManager::calculate");

    // get the operating flow rate
    Real64 mdot = supplyNode.MassFlowRate;

    // calculate the current demand
    Real64 Qdemand = mdot * cp * (returnNode.Temp - supplyNode.Temp);

    // check for strange conditions
    if (Qdemand < 0) {
        this->currentSupplySetPt = this->minimumChilledWaterSetpoint;
        return;
    }

    // Determine a return target, default is to use the constant value, but scheduled or externally
    //  set on the return node TempSetPoint will overwrite it.  Note that the schedule index is only
    //  greater than zero if the input type is scheduled, and the useReturnTempSetpoint flag is only
    //  true if the input type is specified as such
    Real64 T_return_target = this->returnTemperatureConstantTarget;
    if (this->returnTemperatureScheduleIndex > 0) {
        T_return_target = GetCurrentScheduleValue(state, this->returnTemperatureScheduleIndex);
    } else if (this->useReturnTempSetpoint) {
        if (returnNode.TempSetPoint != SensedNodeFlagValue) {
            T_return_target = returnNode.TempSetPoint;
        } else {
            ShowSevereError(state, "Return temperature reset setpoint manager encountered an error.");
            ShowContinueError(state,
                              "The manager is specified to look to the return node setpoint to find a target return temperature, but the node "
                              "setpoint was invalid");
            ShowContinueError(state,
                              "Verify that a separate sepoint manager is specified to set the setpoint on the return node named \"" +
                                  state.dataLoopNodes->NodeID(this->returnNodeIndex) + "\"");
            ShowContinueError(state, "Or change the target return temperature input type to constant or scheduled");
            ShowFatalError(state, "Missing reference setpoint");
        }
    }

    // calculate the supply setpoint to use, default to the design value if flow is zero
    Real64 T_supply_setpoint = this->minimumChilledWaterSetpoint;
    if (mdot > DataConvergParams::PlantFlowRateToler) {
        T_supply_setpoint = T_return_target - Qdemand / (mdot * cp);
    }

    // now correct it to bring it into range
    T_supply_setpoint = min(max(T_supply_setpoint, this->minimumChilledWaterSetpoint), this->maximumChilledWaterSetpoint);

    // now save it for use in the update routine
    this->currentSupplySetPt = T_supply_setpoint;
}

void DefineReturnWaterHWSetPointManager::calculate(EnergyPlusData &state, DataLoopNode::NodeData &returnNode, DataLoopNode::NodeData &supplyNode)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee, NREL
    //       DATE WRITTEN   May 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the plant supply temperature reset required to achieve a target plant return temperature

    // METHODOLOGY EMPLOYED:
    // The setpoint manager follows this procedure:
    //  1. Calculate the current demand
    //    a. Sense the current return temperature
    //    b. Sense the current supply temperature
    //    c. Sense the current flow rate
    //    d. Approximate the fluid properties (rho, Cp) from the temperatures
    //    ---> Use these to calculate the demand with Q_demand = V_dot * rho * C_p * (T_supply_sensed - T_return_sensed)
    //  2. Calculate a new value of supply setpoint that will reject this much Q_demand, while providing a target return temperature
    //    * this assumes that the demand will be the same value on the next time around
    //    * at any time step, the value of target return temperature may vary if it is scheduled (or actuated with EMS)
    //    a. T_supply_setpoint = T_return_target + Q_demand / ( V_dot * rho * C_p )
    //  3. Constrain this value to limits
    //    a. T_supply_setpoint will be within: [ Minimum Chilled Water Reset Temperature, Design Hot Water Supply Temperature ]

    // NOTES:
    // The assumptions related to lagging of setpoint are most suited for smaller timesteps and/or plants that don't vary wildly from one time
    // step to another The assumptions also become affected by variable flow plants more-so than constant-flow plants

    // Using/Aliasing
    using namespace DataPlant;

    // we need to know the plant to get the fluid ID in case it is glycol
    // but we have to wait in case plant isn't initialized yet
    // if plant isn't initialized, assume index=1 (water)
    int fluidIndex = 1;
    if (this->plantLoopIndex == 0) {
        for (int plantIndex = 1; plantIndex <= state.dataPlnt->TotNumLoops; plantIndex++) {
            if (this->supplyNodeIndex == state.dataPlnt->PlantLoop(plantIndex).LoopSide(DataPlant::LoopSideLocation::Supply).NodeNumOut) {
                this->plantLoopIndex = plantIndex;
                this->plantSetpointNodeIndex = state.dataPlnt->PlantLoop(plantIndex).TempSetPointNodeNum;
                fluidIndex = state.dataPlnt->PlantLoop(plantIndex).FluidIndex;
                // now that we've found the plant populated, let's verify that the nodes match
                if (!PlantUtilities::verifyTwoNodeNumsOnSamePlantLoop(state, this->supplyNodeIndex, this->returnNodeIndex)) {
                    ShowSevereError(state, "Node problem for SetpointManager:ReturnTemperature:HotWater.");
                    ShowContinueError(state, "Return and Supply nodes were not found on the same plant loop.  Verify node names.");
                    ShowFatalError(state, "Simulation aborts due to setpoint node problem");
                }
            }
        }
    }

    // we don't need fluid names since we have a real index, so just pass in the temperature and get properties
    Real64 avgTemp = (returnNode.Temp + supplyNode.Temp) / 2;
    Real64 cp = FluidProperties::GetSpecificHeatGlycol(state, "", avgTemp, fluidIndex, "ReturnWaterHWSetPointManager::calculate");

    // get the operating flow rate
    Real64 mdot = supplyNode.MassFlowRate;

    // calculate the current demand
    Real64 Qdemand = mdot * cp * (supplyNode.Temp - returnNode.Temp);

    // check for strange conditions
    if (Qdemand < 0) {
        this->currentSupplySetPt = this->maximumHotWaterSetpoint;
        return;
    }

    // Determine a return target, default is to use the constant value, but scheduled overwrites it
    Real64 T_return_target = this->returnTemperatureConstantTarget;
    if (this->returnTemperatureScheduleIndex > 0) {
        T_return_target = GetCurrentScheduleValue(state, this->returnTemperatureScheduleIndex);
    } else if (this->useReturnTempSetpoint) {
        if (returnNode.TempSetPoint != SensedNodeFlagValue) {
            T_return_target = returnNode.TempSetPoint;
        } else {
            ShowSevereError(state, "Return temperature reset setpoint manager encountered an error.");
            ShowContinueError(state,
                              "The manager is specified to look to the return node setpoint to find a target return temperature, but the node "
                              "setpoint was invalid");
            ShowContinueError(state,
                              "Verify that a separate sepoint manager is specified to set the setpoint on the return node named \"" +
                                  state.dataLoopNodes->NodeID(this->returnNodeIndex) + "\"");
            ShowContinueError(state, "Or change the target return temperature input type to constant or scheduled");
            ShowFatalError(state, "Missing reference setpoint");
        }
    }

    // calculate the supply setpoint to use, default to the design value if flow is zero
    Real64 T_supply_setpoint = this->maximumHotWaterSetpoint;
    if (mdot > DataConvergParams::PlantFlowRateToler) {
        T_supply_setpoint = T_return_target + Qdemand / (mdot * cp);
    }

    // now correct it to bring it into range
    T_supply_setpoint = max(min(T_supply_setpoint, this->maximumHotWaterSetpoint), this->minimumHotWaterSetpoint);

    // now save it for use in the update routine
    this->currentSupplySetPt = T_supply_setpoint;
}

void DefineIdealCondEntSetPointManager::SetupMeteredVarsForSetPt(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   Sep 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // For the Ideal Cond reset setpoint manager, this sets up the
    // report variables used during the calculation.

    // Using/Aliasing
    using namespace DataPlant;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string TypeOfComp;
    std::string NameOfComp;

    Array1D_int VarIndexes;                                         // Variable Numbers
    Array1D<OutputProcessor::VariableType> VarTypes;                // Variable Types (1=integer, 2=real, 3=meter)
    Array1D<OutputProcessor::TimeStepType> IndexTypes;              // Variable Index Types (1=Zone,2=HVAC)
    Array1D<OutputProcessor::Unit> unitsForVar;                     // units from enum for each variable
    std::map<int, DataGlobalConstants::ResourceType> ResourceTypes; // ResourceTypes for each variable
    Array1D_string EndUses;                                         // EndUses for each variable
    Array1D_string Groups;                                          // Groups for each variable
    Array1D_string Names;                                           // Variable Names for each variable
    int NumVariables;
    int NumFound;

    // Chiller and ChW pump location, assumes supply side
    int ChillerLoopNum(this->LoopIndexPlantSide);         // Chiller loop number
    int ChillerBranchNum(this->BranchIndexPlantSide);     // Chiller branch number
    int ChillerNum(this->ChillerIndexPlantSide);          // Chiller number
    int ChilledPumpBranchNum(this->ChilledPumpBranchNum); // Chilled water pump branch number
    int ChilledPumpNum(this->ChilledPumpNum);             // Chilled water pump number

    // Tower and CW pump location, assumes supply side, and tower branch/comp nums are used directly instead of local variable copies
    int TowerLoopNum(this->CondLoopNum);            // Tower loop number
    int CondPumpBranchNum(this->CondPumpBranchNum); // Condenser water pump branch number
    int CondPumpNum(this->CondPumpNum);             // Condenser pump number

    TypeOfComp = state.dataPlnt->PlantLoop(ChillerLoopNum).LoopSide(LoopSideLocation::Supply).Branch(ChillerBranchNum).Comp(ChillerNum).TypeOf;
    NameOfComp = state.dataPlnt->PlantLoop(ChillerLoopNum).LoopSide(LoopSideLocation::Supply).Branch(ChillerBranchNum).Comp(ChillerNum).Name;
    NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
    VarIndexes.allocate(NumVariables);
    VarTypes.allocate(NumVariables);
    IndexTypes.allocate(NumVariables);
    unitsForVar.allocate(NumVariables);

    for (int varN = 1; varN <= NumVariables; ++varN) {
        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
    }

    EndUses.allocate(NumVariables);
    Groups.allocate(NumVariables);
    Names.allocate(NumVariables);

    GetMeteredVariables(
        state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);
    this->ChllrVarType = VarTypes(1);
    this->ChllrVarIndex = VarIndexes(1);

    TypeOfComp =
        state.dataPlnt->PlantLoop(ChillerLoopNum).LoopSide(LoopSideLocation::Supply).Branch(ChilledPumpBranchNum).Comp(ChilledPumpNum).TypeOf;
    NameOfComp = state.dataPlnt->PlantLoop(ChillerLoopNum).LoopSide(LoopSideLocation::Supply).Branch(ChilledPumpBranchNum).Comp(ChilledPumpNum).Name;
    NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
    VarIndexes.allocate(NumVariables);
    VarTypes.allocate(NumVariables);
    IndexTypes.allocate(NumVariables);
    unitsForVar.allocate(NumVariables);

    ResourceTypes.clear();
    for (int varN = 1; varN <= NumVariables; ++varN) {
        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
    }

    EndUses.allocate(NumVariables);
    Groups.allocate(NumVariables);
    Names.allocate(NumVariables);

    GetMeteredVariables(
        state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);
    this->ChlPumpVarType = VarTypes(1);
    this->ChlPumpVarIndex = VarIndexes(1);

    for (int i = 1; i <= this->numTowers; i++) {
        TypeOfComp = state.dataPlnt->PlantLoop(TowerLoopNum)
                         .LoopSide(LoopSideLocation::Supply)
                         .Branch(this->CondTowerBranchNum(i))
                         .Comp(this->TowerNum(i))
                         .TypeOf;
        NameOfComp = state.dataPlnt->PlantLoop(TowerLoopNum)
                         .LoopSide(LoopSideLocation::Supply)
                         .Branch(this->CondTowerBranchNum(i))
                         .Comp(this->TowerNum(i))
                         .Name;
        NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
        VarIndexes.allocate(NumVariables);
        VarTypes.allocate(NumVariables);
        IndexTypes.allocate(NumVariables);
        unitsForVar.allocate(NumVariables);

        ResourceTypes.clear();
        for (int varN = 1; varN <= NumVariables; ++varN) {
            ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
        }

        EndUses.allocate(NumVariables);
        Groups.allocate(NumVariables);
        Names.allocate(NumVariables);

        GetMeteredVariables(
            state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);
        this->ClTowerVarType.push_back(VarTypes(1));
        this->ClTowerVarIndex.push_back(VarIndexes(1));
    }

    TypeOfComp = state.dataPlnt->PlantLoop(TowerLoopNum).LoopSide(LoopSideLocation::Supply).Branch(CondPumpBranchNum).Comp(CondPumpNum).TypeOf;
    NameOfComp = state.dataPlnt->PlantLoop(TowerLoopNum).LoopSide(LoopSideLocation::Supply).Branch(CondPumpBranchNum).Comp(CondPumpNum).Name;
    NumVariables = GetNumMeteredVariables(state, TypeOfComp, NameOfComp);
    VarIndexes.allocate(NumVariables);
    VarTypes.allocate(NumVariables);
    IndexTypes.allocate(NumVariables);
    unitsForVar.allocate(NumVariables);

    ResourceTypes.clear();
    for (int varN = 1; varN <= NumVariables; ++varN) {
        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
    }

    EndUses.allocate(NumVariables);
    Groups.allocate(NumVariables);
    Names.allocate(NumVariables);

    GetMeteredVariables(
        state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);
    this->CndPumpVarType = VarTypes(1);
    this->CndPumpVarIndex = VarIndexes(1);
}

void DefineSysNodeResetSetPointManager::calculate(EnergyPlusData &state)
{
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SpAtLow;      // Setpoint at low reference value
    Real64 SpAtHigh;     // Setpoint at high reference value
    Real64 LowRefVal;    // Low reference value
    Real64 HighRefVal;   // High reference value
    Real64 RefValue = 0; // Reference value from the Reference node
    int RefNode;         // Reference node number

    RefNode = this->RefNodeNum;

    switch (this->CtrlTypeMode) {
    case CtrlVarType::Temp: {
        RefValue = state.dataLoopNodes->Node(RefNode).Temp;
    } break;
    case CtrlVarType::MaxTemp: {
        RefValue = state.dataLoopNodes->Node(RefNode).Temp;
    } break;
    case CtrlVarType::MinTemp: {
        RefValue = state.dataLoopNodes->Node(RefNode).Temp;
    } break;
    case CtrlVarType::HumRat: {
        RefValue = state.dataLoopNodes->Node(RefNode).HumRat;
    } break;
    case CtrlVarType::MaxHumRat: {
        RefValue = state.dataLoopNodes->Node(RefNode).HumRat;
    } break;
    case CtrlVarType::MinHumRat: {
        RefValue = state.dataLoopNodes->Node(RefNode).HumRat;
    } break;
    default:
        break;
    }

    SpAtLow = this->SpAtLowRef;
    SpAtHigh = this->SpAtHighRef;
    LowRefVal = this->LowRef;
    HighRefVal = this->HighRef;

    this->SetPt = this->calcSetPointLinInt(LowRefVal, HighRefVal, RefValue, SpAtLow, SpAtHigh);
}

Real64
SPBase::calcSetPointLinInt(Real64 const LowVal, Real64 const HighVal, Real64 const RefVal, Real64 const SetptAtLowVal, Real64 const SetptAtHighVal)
{
    Real64 SetPt;
    if (LowVal < HighVal) {
        if (RefVal <= LowVal) {
            SetPt = SetptAtLowVal;
        } else if (RefVal >= HighVal) {
            SetPt = SetptAtHighVal;
        } else {
            SetPt = SetptAtLowVal - ((RefVal - LowVal) / (HighVal - LowVal)) * (SetptAtLowVal - SetptAtHighVal);
        }

    } else {
        SetPt = 0.5 * (SetptAtLowVal + SetptAtHighVal);
    }
    return SetPt;
}

void UpdateSetPointManagers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   July 1998
    //       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
    //                      P. Haves Oct 2004
    //                        Add new setpoint managers:
    //                          SET POINT MANAGER:WARMEST TEMP FLOW and
    //                          SET POINT MANAGER:COLDEST TEMP FLOW
    //                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
    //                        Add new setpoint managers:
    //                          SET POINT MANAGER:SINGLE ZONE HEATING and
    //                          SET POINT MANAGER:SINGLE ZONE COOLING
    //                        Work supported by ASHRAE research project 1254-RP
    //                      B. Griffith Aug. 2006.  Allow HUMRAT for scheduled setpoint manager
    //                      P. Haves Aug 2007
    //                        SET POINT MANAGER:WARMEST TEMP FLOW:
    //                          Set AirLoopControlInfo()%LoopFlowRateSet every call not just on
    //                          initialization (flag now reset in SUBROUTINE ResetHVACControl)
    //                        Removed SET POINT MANAGER:COLDEST TEMP FLOW
    //                      July 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers
    //                          SetpointManager:MultiZone:Heating:Average
    //                          SetpointManager:MultiZone:Cooling:Average
    //                          SetpointManager:MultiZone:MinimumHumidity:Average
    //                          SetpointManager:MultiZone:MaximumHumidity:Average
    //                      Aug 2010 B.A. Nigusse, FSEC/UCF
    //                        Added new setpoint managers:
    //                          SetpointManager:MultiZone:Humidity:Minimum
    //                          SetpointManager:MultiZone:Humidity:Maximum
    //                      Aug 2014 Rick Strand, UIUC
    //                          SetpointManager:ScheduledTES (internally defined)
    //                      Jan 2022 Wooyoung Jung, Jeremy Lerond and Jian Zhang, PNNL
    //                        Added new setpoint managers:
    //                          SetpointManager:SystemNodeReset:Temperature
    //                          SetpointManager:SystemNodeReset:Humidity

    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Loop over all the Setpoint Managers and use their output arrays
    // to set the node setpoints.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // Using/Aliasing
    using EMSManager::CheckIfNodeSetPointManagedByEMS;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;
    int CtrlNodeIndex;
    int NodeNum;

    // Loop over all the Scheduled Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            switch (state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode) {
                // set the setpoint depending on the type of variable being controlled
            case CtrlVarType::Temp: {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MaxTemp: {
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MinTemp: {
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::HumRat: {
                state.dataLoopNodes->Node(NodeNum).HumRatSetPoint = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MaxHumRat: {
                state.dataLoopNodes->Node(NodeNum).HumRatMax = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MinHumRat: {
                state.dataLoopNodes->Node(NodeNum).HumRatMin = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MassFlow: {
                state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MaxMassFlow: {
                state.dataLoopNodes->Node(NodeNum).MassFlowRateMax = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            case CtrlVarType::MinMassFlow: {
                state.dataLoopNodes->Node(NodeNum).MassFlowRateMin = state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).SetPt;
            } break;
            default:
                break;
            }

        } // nodes in list

    } // setpoint manger:scheduled

    // Loop over all the Scheduled TES Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchTESSetPtMgrs; ++SetPtMgrNum) {

        // only one setpoint for each scheduled TES setpoint manager and its a temperature setpoint
        NodeNum = state.dataSetPointManager->SchTESSetPtMgr(SetPtMgrNum).CtrlNodeNum; // Get the node number
        state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->SchTESSetPtMgr(SetPtMgrNum).SetPt;

    } // setpoint manger:scheduledTES

    // Loop over all the Scheduled Dual Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumDualSchSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                    state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SetPtHi; // Set the setpoint High
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                    state.dataSetPointManager->DualSchSetPtMgr(SetPtMgrNum).SetPtLo; // Set the setpoint Low
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    (state.dataLoopNodes->Node(NodeNum).TempSetPointHi + state.dataLoopNodes->Node(NodeNum).TempSetPointLo) /
                    2.0; // average of the high and low
            }
        }
    }

    // Loop over all the Outside Air Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOutAirSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            } else if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                    state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt; // Set the high temperature setpoint
            } else if (state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                    state.dataSetPointManager->OutAirSetPtMgr(SetPtMgrNum).SetPt; // Set the low temperature setpoint
            }
        }
    }

    // Loop over all the Single Zone Reheat Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZRhSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->SingZoneRhSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            }
        }
    }

    // Loop over all the Single Zone Heating Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZHtSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->SingZoneHtSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            }
        }
    }

    // Loop over all the Single Zone Cooling Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            }
        }
    }

    // Loop over all the Single Zone Minimum Humidity Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMinHumSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            state.dataLoopNodes->Node(NodeNum).HumRatMin = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
        }
    }

    // Loop over all the Single Zone Maximum Humidity Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMaxHumSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            state.dataLoopNodes->Node(NodeNum).HumRatMax = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
        }
    }

    // Loop over all the Warmest Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->WarmestSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            }
        }
    }

    // Loop over all the Coldest Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumColdestSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->ColdestSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            }
        }
    }

    // Loop over all the Warmest Temp Flow Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).SetPt; // Set the supply air temperature setpoint
                state.dataAirLoop->AirLoopFlow(state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum).ReqSupplyFrac =
                    state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).Turndown; // Set the supply air flow rate
                state.dataAirLoop->AirLoopControlInfo(state.dataSetPointManager->WarmestSetPtMgrTempFlow(SetPtMgrNum).AirLoopNum).LoopFlowRateSet =
                    true; // PH 8/17/07
            }
        }
    }

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumRABFlowSetPtMgrs; ++SetPtMgrNum) {

        NodeNum = state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).RABSplitOutNode; // Get the node number

        if (state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MassFlow) {
            state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint =
                state.dataSetPointManager->RABFlowSetPtMgr(SetPtMgrNum).FlowSetPt; // Set the flow setpoint
        }
    }

    // Loop over all the MultiZone Average Cooling Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZClgAverageSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->MZAverageCoolingSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // Loop over all the MultiZone Average Heating Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->MZAverageHeatingSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // Loop over all the MultiZone Average Minimum Humidity Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinHumRat) {
                state.dataLoopNodes->Node(NodeNum).HumRatMin =
                    state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).SetPt; // Set the humidity ratio setpoint
            }
        }
    }

    // Loop over all the MultiZone Average Maxiumum Humidity Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxHumRat) {
                state.dataLoopNodes->Node(NodeNum).HumRatMax =
                    state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).SetPt; // Set the humidity ratio setpoint
            }
        }
    }

    // Loop over all the Multizone Minimum Humidity Ratio Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMinHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinHumRat) {
                state.dataLoopNodes->Node(NodeNum).HumRatMin =
                    state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).SetPt; // Set the humidity ratio setpoint
            }
        }
    }

    // Loop over all the Multizone Maximum Humidity Ratio Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxHumRat) {
                state.dataLoopNodes->Node(NodeNum).HumRatMax =
                    state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).SetPt; // Set the humidity ratio setpoint
            }
        }
    }

    // Loop over all the Follow Outdoor Air Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowOATempSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                    state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            } else if (state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                    state.dataSetPointManager->FollowOATempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // Loop over all the Follow System Node Temperature Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                    state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            } else if (state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                    state.dataSetPointManager->FollowSysNodeTempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // Loop over all the Ground Tempearture Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumGroundTempSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                    state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            } else if (state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MinTemp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                    state.dataSetPointManager->GroundTempSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // Loop over all Condenser Entering Set Point Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumCondEntSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            //  set points from this set point manager
            NodeNum = state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->CondEntSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // Loop over all Ideal Condenser Entering Set Point Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumIdealCondEntSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // set points from this set point manager
            NodeNum = state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->IdealCondEntSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // loop over all single zone on/off cooling setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // set points from this set point manager
            NodeNum = state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->SZOneStageCoolingSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // loop over all single zone on/off heating setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // set points from this set point manager
            NodeNum = state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            if (state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->SZOneStageHeatingSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
            }
        }
    }

    // return water temperature reset setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetChWSetPtMgrs; ++SetPtMgrNum) {
        auto &returnWaterSPM(state.dataSetPointManager->ReturnWaterResetChWSetPtMgr(SetPtMgrNum));
        if (returnWaterSPM.plantSetpointNodeIndex > 0) {
            state.dataLoopNodes->Node(returnWaterSPM.plantSetpointNodeIndex).TempSetPoint = returnWaterSPM.currentSupplySetPt;
        } else {
            // if plant isn't set up yet, no need to set anything, just wait
        }
    }

    // hot-water return water temperature reset setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumReturnWaterResetHWSetPtMgrs; ++SetPtMgrNum) {
        auto &returnWaterSPM(state.dataSetPointManager->ReturnWaterResetHWSetPtMgr(SetPtMgrNum));
        if (returnWaterSPM.plantSetpointNodeIndex > 0) {
            state.dataLoopNodes->Node(returnWaterSPM.plantSetpointNodeIndex).TempSetPoint = returnWaterSPM.currentSupplySetPt;
        } else {
            // if plant isn't set up yet, no need to set anything, just wait
        }
    }

    // Loop over all the System Node Reset Setpoint Managers
    for (SetPtMgrNum = 1;
         SetPtMgrNum <= (state.dataSetPointManager->NumSystemNodeResetTempSetPtMgrs + state.dataSetPointManager->NumSystemNodeResetHumSetPtMgrs);
         ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number
            switch (state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).CtrlTypeMode) {
            case CtrlVarType::Temp:
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt; // Set the temperature setpoint
                break;
            case CtrlVarType::MaxTemp:
                state.dataLoopNodes->Node(NodeNum).TempSetPointHi =
                    state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt; // Set the maximum temperature setpoint
                break;
            case CtrlVarType::MinTemp:
                state.dataLoopNodes->Node(NodeNum).TempSetPointLo =
                    state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt; // Set the minimum temperature setpoint
                break;
            case CtrlVarType::HumRat:
                state.dataLoopNodes->Node(NodeNum).HumRatSetPoint =
                    state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt; // Set the humidity ratio setpoint
                break;
            case CtrlVarType::MaxHumRat:
                state.dataLoopNodes->Node(NodeNum).HumRatMax =
                    state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt; // Set the maximum humidity ratio setpoint
                break;
            case CtrlVarType::MinHumRat:
                state.dataLoopNodes->Node(NodeNum).HumRatMin =
                    state.dataSetPointManager->SystemNodeResetSetPtMgr(SetPtMgrNum).SetPt; // Set the minimum humidity ratio setpoint
                break;
            default:
                break;
            }
        }
    }
}

void UpdateMixedAirSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Loop over all the Mixed Air Managers and use their output arrays
    // to set the node setpoints.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;
    int CtrlNodeIndex;
    int NodeNum;

    // Loop over all the Mixed Air Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMixedAirSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            if (state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::Temp) {
                state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            }
        }
    }
}

void UpdateOAPretreatSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. J. Witte based on UpdateMixedAirSetPoints by Fred Buhl,
    //                        Work supported by ASHRAE research project 1254-RP
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Loop over all the Outside Air Pretreat Managers and use their output arrays
    // to set the node setpoints.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;
    int CtrlNodeIndex;
    int NodeNum;

    // Loop over all the Mixed Air Setpoint Managers

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumOAPretreatSetPtMgrs; ++SetPtMgrNum) {

        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).NumCtrlNodes;
             ++CtrlNodeIndex) { // Loop over the list of nodes wanting
            // setpoints from this setpoint manager
            NodeNum = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex); // Get the node number

            switch (state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).CtrlTypeMode) {
            case CtrlVarType::Temp: { // 'Temperature'
                state.dataLoopNodes->Node(NodeNum).TempSetPoint =
                    state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            } break;
            case CtrlVarType::MaxHumRat: { // 'MaximumHumidityRatio'
                state.dataLoopNodes->Node(NodeNum).HumRatMax = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            } break;
            case CtrlVarType::MinHumRat: { // 'MinimumHumidityRatio'
                state.dataLoopNodes->Node(NodeNum).HumRatMin = state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            } break;
            case CtrlVarType::HumRat: { // 'HumidityRatio'
                state.dataLoopNodes->Node(NodeNum).HumRatSetPoint =
                    state.dataSetPointManager->OAPretreatSetPtMgr(SetPtMgrNum).SetPt; // Set the setpoint
            } break;
            default:
                break;
            }
        }
    }
}

int getSPMBasedOnNode(
    EnergyPlusData &state, int const NodeNum, CtrlVarType const SetPtType, SetPointManagerType const SPMType, CtrlNodeType ctrlOrRefNode)
{

    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    int getSPMBasedOnNode = 0;

    for (int SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumAllSetPtMgrs; ++SetPtMgrNum) {
        if (SetPtType ==
            state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode) { // SetPtType is e.g., CtrlVarType::Temp, CtrlVarType::HumRat, etc.
            switch (ctrlOrRefNode) { // ctrlOrRefNode is enum type of node to look for, either control node or reference node
            case CtrlNodeType::Control: {
                for (int NumNode = 1; NumNode <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++NumNode) {
                    // SPMType is type of set point manager, e.g., SetPointManagerType::Scheduled, SetPointManagerType::MixedAir, etc.
                    if (NodeNum == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(NumNode) &&
                        SPMType == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType) {
                        getSPMBasedOnNode = state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMIndex; // SPMIndex is index to specific type of SPM
                        break;
                    }
                }
                break;
            }
            case CtrlNodeType::Reference: {
                // SPMType is type of set point manager, e.g., SetPointManagerType::Scheduled, SetPointManagerType::MixedAir, etc.
                if (NodeNum == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).RefNode &&
                    SPMType == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMType) {
                    getSPMBasedOnNode = state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).SPMIndex; // SPMIndex is index to specific type of SPM
                }
                break;
            }
            default:
                assert(false);
            }
        }
        if (getSPMBasedOnNode > 0) break;
    }

    return getSPMBasedOnNode;
}

bool IsNodeOnSetPtManager(EnergyPlusData &state, int const NodeNum, CtrlVarType const SetPtType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   January 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Determines if a particular node is acted upon by a specific setpoint manager

    // METHODOLOGY EMPLOYED:
    // Cycle through all setpoint managers and find if the node passed in has a setpoint manager of passed
    // in type associated to it.

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    bool IsNodeOnSetPtManager;

    // Locals
    int SetPtMgrNum;
    int NumNode;

    // First time called, get the input for all the setpoint managers
    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    IsNodeOnSetPtManager = false;

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumAllSetPtMgrs; ++SetPtMgrNum) {
        if (SetPtType == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode) {
            for (NumNode = 1; NumNode <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++NumNode) {
                if (NodeNum == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(NumNode)) {
                    IsNodeOnSetPtManager = true;
                    break;
                }
            }
        }
    }

    return IsNodeOnSetPtManager;
}

bool NodeHasSPMCtrlVarType(EnergyPlusData &state, int const NodeNum, CtrlVarType const CtrlVarType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   March 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Determines if a particular node is acted upon by a specific setpoint manager

    // METHODOLOGY EMPLOYED:
    // Cycle through all setpoint managers and find if the node has a specific control type

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // SUBROUTINE ARGUMENT DEFINITIONS:

    // Return value
    bool NodeHasSPMCtrlVarType;

    // Locals
    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum; // loop counter for each set point manager
    int NumNode;     // loop counter for each node and specific control type

    // First time called, get the input for all the setpoint managers
    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    // Initialize to false that node is not controlled by set point manager
    NodeHasSPMCtrlVarType = false;

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumAllSetPtMgrs; ++SetPtMgrNum) {
        for (NumNode = 1; NumNode <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++NumNode) {
            if (NodeNum == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(NumNode)) {
                if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType) {
                    //       If specific control type is found, it doesn't matter if there are other of same type.
                    NodeHasSPMCtrlVarType = true;
                    goto SPMLoop_exit;
                }
            }
        }
    }
SPMLoop_exit:;

    return NodeHasSPMCtrlVarType;
}

void ResetHumidityRatioCtrlVarType(EnergyPlusData &state, int const NodeNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   August 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Resets setpoint control variable type to "Maximum Humidty Ratio" if control variable type
    // is "Humidity Ratio".

    // METHODOLOGY EMPLOYED:
    // Cycle through all setpoint managers and find if the node has a "Humidity Ratio" control
    // variable type. This routine is called from "GetControllerInput" routine.  This reset is
    // just to stop false warning message due to control variable type mismatch.

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    const char *RoutineName("ResetHumidityRatioCtrlVarType: ");

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // Locals
    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;        // loop counter for each set point manager
    int NumNode;            // loop counter for each node and specific control type
    int SetPtMgrNumPtr;     // setpoint manager
    bool ResetCntrlVarType; // if true reset Hum Rat control var type to maxhumidity ratio

    // First time called, get the input for all the setpoint managers
    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    ResetCntrlVarType = false;

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumAllSetPtMgrs; ++SetPtMgrNum) {
        for (NumNode = 1; NumNode <= state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++NumNode) {
            if (NodeNum == state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlNodes(NumNode)) {
                if (state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::HumRat) {
                    state.dataSetPointManager->AllSetPtMgr(SetPtMgrNum).CtrlTypeMode = CtrlVarType::MaxHumRat;
                    SetPtMgrNumPtr = SetPtMgrNum;
                    ResetCntrlVarType = true;
                    goto SPMLoop_exit;
                }
            }
        }
    }

SPMLoop_exit:;

    if (ResetCntrlVarType) {
        ShowWarningError(state,
                         format("{}{}=\"{}\". ",
                                RoutineName,
                                managerTypeName[static_cast<int>(state.dataSetPointManager->AllSetPtMgr(SetPtMgrNumPtr).SPMType)],
                                state.dataSetPointManager->AllSetPtMgr(SetPtMgrNumPtr).Name));
        ShowContinueError(state, " ..Humidity ratio control variable type specified is = HumidityRatio");
        ShowContinueError(state, " ..Humidity ratio control variable type allowed with water coils is = MaximumHumidityRatio");
        ShowContinueError(state, " ..Setpointmanager control variable type is reset to = MaximumHumidityRatio");
        ShowContinueError(state, " ..Simulation continues. ");
    }
}

void CheckIfAnyIdealCondEntSetPoint(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Heejin Cho, PNNL
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Determine if ideal condenser entering set point manager is used in model and set flag

    std::string cCurrentModuleObject;

    cCurrentModuleObject = "SetpointManager:CondenserEnteringReset:Ideal";
    state.dataSetPointManager->NumIdealCondEntSetPtMgrs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataSetPointManager->NumIdealCondEntSetPtMgrs > 0) {
        state.dataGlobal->AnyIdealCondEntSetPointInModel = true;
    } else {
        state.dataGlobal->AnyIdealCondEntSetPointInModel = false;
    }
}

CtrlVarType GetHumidityRatioVariableType(EnergyPlusData &state, int const CntrlNodeNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. A. Nigusse
    //       DATE WRITTEN   December 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Loop over all the humidity setpoint Managers to determine the
    // humidity ratio setpoint type

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    CtrlVarType HumRatCntrlType;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;
    int CtrlNodeIndex;
    int NodeNum;

    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    HumRatCntrlType = CtrlVarType::HumRat;

    // Loop over the single zone maximum humidity setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            NodeNum = state.dataSetPointManager->SZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex);
            if (CntrlNodeNum == NodeNum) {
                HumRatCntrlType = CtrlVarType::MaxHumRat;
                return HumRatCntrlType;
            }
        }
    }
    // Loop over the MultiZone maximum humidity setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMaxHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            NodeNum = state.dataSetPointManager->MZMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex);
            if (CntrlNodeNum == NodeNum) {
                HumRatCntrlType = CtrlVarType::MaxHumRat;
                return HumRatCntrlType;
            }
        }
    }
    // Loop over the MultiZone Average Maxiumum Humidity Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            NodeNum = state.dataSetPointManager->MZAverageMaxHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex);
            if (CntrlNodeNum == NodeNum) {
                HumRatCntrlType = CtrlVarType::MaxHumRat;
                return HumRatCntrlType;
            }
        }
    }
    // Loop over the single zone minimum humidity setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZMinHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            NodeNum = state.dataSetPointManager->SZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex);
            if (CntrlNodeNum == NodeNum) {
                HumRatCntrlType = CtrlVarType::MinHumRat;
                return HumRatCntrlType;
            }
        }
    }
    // Loop over the MultiZone minimum humidity setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZMinHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            NodeNum = state.dataSetPointManager->MZMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex);
            if (CntrlNodeNum == NodeNum) {
                HumRatCntrlType = CtrlVarType::MinHumRat;
                return HumRatCntrlType;
            }
        }
    }
    // Loop over the MultiZone Average Minimum Humidity Setpoint Managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            NodeNum = state.dataSetPointManager->MZAverageMinHumSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex);
            if (CntrlNodeNum == NodeNum) {
                HumRatCntrlType = CtrlVarType::MinHumRat;
                return HumRatCntrlType;
            }
        }
    }
    // Loop over the schedule setpoint managers
    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSchSetPtMgrs; ++SetPtMgrNum) {
        for (CtrlNodeIndex = 1; CtrlNodeIndex <= state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrlNodeIndex) {
            if (CntrlNodeNum == state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlNodes(CtrlNodeIndex)) {
                if (state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::HumRat) {
                    return CtrlVarType::HumRat;
                } else if (state.dataSetPointManager->SchSetPtMgr(SetPtMgrNum).CtrlTypeMode == CtrlVarType::MaxHumRat) {
                    return CtrlVarType::MaxHumRat;
                }
            }
        }
    }

    return HumRatCntrlType;
}

void SetUpNewScheduledTESSetPtMgr(EnergyPlusData &state,
                                  int const SchedPtr,
                                  int const SchedPtrCharge,
                                  Real64 NonChargeCHWTemp,
                                  Real64 ChargeCHWTemp,
                                  DataPlant::CtrlType CompOpType,
                                  int const ControlNodeNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Set up new scheduled TES setpoint managers based on plant control Simple TES

    // METHODOLOGY EMPLOYED:
    // Set up internally created scheduled setpoint managers to control the setpoints
    // of various ice storage equipment with the user having to do this manually.  The
    // point is to provide a simpler input description and take care of logic internally.

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFoundinTESSchSetup(false);
    int NodeNum;

    state.dataSetPointManager->NumSchTESSetPtMgrs += 1;
    state.dataSetPointManager->NumAllSetPtMgrs += 1;

    // allocate/redimension structures for new item
    if (state.dataSetPointManager->NumSchTESSetPtMgrs == 1) { // first time through--main structure not allocated yet
        state.dataSetPointManager->SchTESSetPtMgr.allocate(1);
    } else if (state.dataSetPointManager->NumSchTESSetPtMgrs > 1) { // no longer first time through--redimension to new size
        state.dataSetPointManager->SchTESSetPtMgr.redimension(state.dataSetPointManager->NumSchTESSetPtMgrs);
    }
    state.dataSetPointManager->AllSetPtMgr.redimension(state.dataSetPointManager->NumAllSetPtMgrs);

    // Set up the scheduled TES setpoint manager information
    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).SchedPtr = SchedPtr;
    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).SchedPtrCharge = SchedPtrCharge;
    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).NonChargeCHWTemp = NonChargeCHWTemp;
    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).ChargeCHWTemp = ChargeCHWTemp;
    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).CompOpType = CompOpType;
    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).CtrlNodeNum = ControlNodeNum;

    // Set up the all setpoint manager information for "verification" that no other setpoint manager controls the node that this new ones does
    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->NumAllSetPtMgrs).CtrlNodes.allocate(1);
    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->NumAllSetPtMgrs).CtrlNodes(1) =
        state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).CtrlNodeNum;
    // Give it a Name just in case it's used for error reporting
    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->NumAllSetPtMgrs).Name =
        format("Auto generated TES SPM {}", state.dataSetPointManager->NumSchTESSetPtMgrs);
    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->NumAllSetPtMgrs).SPMType = SetPointManagerType::TESScheduled;
    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->NumAllSetPtMgrs).CtrlTypeMode = CtrlVarType::Temp;
    state.dataSetPointManager->AllSetPtMgr(state.dataSetPointManager->NumAllSetPtMgrs).NumCtrlNodes = 1;

    // Now verify that there is no overlap (no other SPM uses the node of the new setpoint manager)
    ErrorsFoundinTESSchSetup = false;
    VerifySetPointManagers(state, ErrorsFoundinTESSchSetup);
    if (ErrorsFoundinTESSchSetup) {
        ShowFatalError(state, "Errors found in verification step of SetUpNewScheduledTESSetPtMgr.  Program terminates.");
    }
    // Since all of the other setpoint managers not only been read and verified but also initialized, simulated, and updated,
    // we must now also initialize, simulate, and update the current SchTESStPtMgr that was just added.  But the init and simulate
    // steps are the same so we can call the simulate first.

    state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).calculate(state);

    // Now update reusing code from Update routine specialized to only doing the current (new) setpoint manager and then we are done

    NodeNum = state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).CtrlNodeNum; // Get the node number
    state.dataLoopNodes->Node(NodeNum).TempSetPoint = state.dataSetPointManager->SchTESSetPtMgr(state.dataSetPointManager->NumSchTESSetPtMgrs).SetPt;

} // end of SetUpNewScheduledTESSetPtMgr

bool GetCoilFreezingCheckFlag(EnergyPlusData &state, int const MixedAirSPMNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         L. Gu
    //       DATE WRITTEN   Nov. 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Get freezing check status

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    bool FeezigCheckFlag;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CtrldNodeNum;

    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    FeezigCheckFlag = false;

    for (CtrldNodeNum = 1; CtrldNodeNum <= state.dataSetPointManager->MixedAirSetPtMgr(MixedAirSPMNum).NumCtrlNodes; ++CtrldNodeNum) {
        if (state.dataSetPointManager->MixedAirSetPtMgr(MixedAirSPMNum).FreezeCheckEnable) {
            FeezigCheckFlag = true;
            break;
        }
    }

    return FeezigCheckFlag;
} // End of GetCoilFreezingCheckFlag

int GetMixedAirNumWithCoilFreezingCheck(EnergyPlusData &state, int const MixedAirNode)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         L. Gu
    //       DATE WRITTEN   Nov. 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Loop over all the MixedAir setpoint Managers to find coil freezing check flag

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int MixedAirSPMNum;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SetPtMgrNum;
    int CtrldNodeNum;

    if (state.dataSetPointManager->GetInputFlag) {
        GetSetPointManagerInputs(state);
        state.dataSetPointManager->GetInputFlag = false;
    }

    MixedAirSPMNum = 0;

    for (SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumMixedAirSetPtMgrs; ++SetPtMgrNum) {

        for (CtrldNodeNum = 1; CtrldNodeNum <= state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).NumCtrlNodes; ++CtrldNodeNum) {
            if (state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CtrlNodes(CtrldNodeNum) == MixedAirNode) {
                if (state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CoolCoilInNode > 0 &&
                    state.dataSetPointManager->MixedAirSetPtMgr(SetPtMgrNum).CoolCoilOutNode > 0) {
                    MixedAirSPMNum = CtrldNodeNum;
                    break;
                }
            }
        }
    }

    return MixedAirSPMNum;
} // End of GetMixedAirNumWithCoilFreezingCheck(

} // namespace EnergyPlus::SetPointManager
