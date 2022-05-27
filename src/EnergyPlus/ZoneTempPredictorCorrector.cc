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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus::ZoneTempPredictorCorrector {

// MODULE INFORMATION:
//       AUTHOR         Russell D. Taylor
//       DATE WRITTEN   1997
//       MODIFIED       Aug 2001(FW): make SNLoadHeatRate public
//                      Nov 2010  BN(FSEC) added TemperatureAndHumidity Control
//       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
//                      July 2006 (BG) added operative temp control
//                      February 2008 (BG) reworked zone air temp histories

// PURPOSE OF THIS MODULE:
// This module contains routines to predict and correct zone temperatures.
//  also includes zone thermostatic controlling
//  Model the "Air Heat Balance" part of the the "Zone Heat Balance Method."

// METHODOLOGY EMPLOYED:
// apply model equations for air heat balance solved for zone air temp.
//    sum up values for the terms (e.g SUMHAT, SUMHA etc. )
//    "Predict" step is used to get zone loads for HVAC equipment
//    "correct" step determines zone air temp with available HVAC

// Using/Aliasing
using namespace DataHVACGlobals;
using namespace DataHeatBalance;
using namespace DataHeatBalFanSys;
using namespace Psychrometrics;
using namespace DataRoomAirModel;
using namespace DataZoneControls;
using namespace FaultsManager;
using namespace HybridModel;
using ScheduleManager::GetCurrentScheduleValue;

static constexpr std::array<std::string_view, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> ValidControlTypes = {
    "Uncontrolled",
    "ThermostatSetpoint:SingleHeating",
    "ThermostatSetpoint:SingleCooling",
    "ThermostatSetpoint:SingleHeatingOrCooling",
    "ThermostatSetpoint:DualSetpoint"};

static constexpr std::array<std::string_view, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> ValidControlTypesUC = {
    "UNCONTROLLED",
    "THERMOSTATSETPOINT:SINGLEHEATING",
    "THERMOSTATSETPOINT:SINGLECOOLING",
    "THERMOSTATSETPOINT:SINGLEHEATINGORCOOLING",
    "THERMOSTATSETPOINT:DUALSETPOINT"};

static constexpr std::array<std::string_view, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> ValidComfortControlTypes = {
    "Uncontrolled",
    "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating",
    "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling",
    "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling",
    "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint"};

static constexpr std::array<std::string_view, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> ValidComfortControlTypesUC = {
    "UNCONTROLLED",
    "THERMOSTATSETPOINT:THERMALCOMFORT:FANGER:SINGLEHEATING",
    "THERMOSTATSETPOINT:THERMALCOMFORT:FANGER:SINGLECOOLING",
    "THERMOSTATSETPOINT:THERMALCOMFORT:FANGER:SINGLEHEATINGORCOOLING",
    "THERMOSTATSETPOINT:THERMALCOMFORT:FANGER:DUALSETPOINT"};

Array1D_string const cZControlTypes(6,
                                    {"ZoneControl:Thermostat",
                                     "ZoneControl:Thermostat:ThermalComfort",
                                     "ZoneControl:Thermostat:OperativeTemperature",
                                     "ZoneControl:Humidistat",
                                     "ZoneControl:Thermostat:TemperatureAndHumidity",
                                     "ZoneControl:Thermostat:StagedDualSetpoint"});

Array1D_string const AdaptiveComfortModelTypes(8,
                                               {"None",
                                                "AdaptiveASH55CentralLine",
                                                "AdaptiveASH5590PercentUpperLine",
                                                "AdaptiveASH5580PercentUpperLine",
                                                "AdaptiveCEN15251CentralLine",
                                                "AdaptiveCEN15251CategoryIUpperLine",
                                                "AdaptiveCEN15251CategoryIIUpperLine",
                                                "AdaptiveCEN15251CategoryIIIUpperLine"});

// Functions
void ManageZoneAirUpdates(EnergyPlusData &state,
                          DataHeatBalFanSys::PredictorCorrectorCtrl const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
                          Real64 &ZoneTempChange,                                     // Temp change in zone air btw previous and current timestep
                          bool const ShortenTimeStepSys,
                          bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                          Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
)
{

    // SUBROUTINE INFORMATION
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   September 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  Brent Griffith Feb. 2008,  added arguments

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine predicts or corrects the zone air temperature
    // depending on the simulation status and determines the correct
    // temperature setpoint for each zone from the schedule manager.

    if (state.dataZoneCtrls->GetZoneAirStatsInputFlag) {
        GetZoneAirSetPoints(state);
        state.dataZoneCtrls->GetZoneAirStatsInputFlag = false;
    }

    InitZoneAirSetPoints(state);

    switch (UpdateType) {
    case DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints: {
        CalcZoneAirTempSetPoints(state);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep: {
        PredictSystemLoads(state, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep: {
        CorrectZoneAirTemp(state, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::RevertZoneTimestepHistories: {
        RevertZoneTimestepHistories(state);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::PushZoneTimestepHistories: {
        PushZoneTimestepHistories(state);
    } break;
    case DataHeatBalFanSys::PredictorCorrectorCtrl::PushSystemTimestepHistories: {
        PushSystemTimestepHistories(state);
    } break;
    default:
        break;
    }
}

void GetZoneAirSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russell Taylor
    //       DATE WRITTEN   September 1998
    //       MODIFIED       L.Gu, May 2006, B. Griffith June 2006
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the inputs related to thermostatic control.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing
    using General::CheckCreatedZoneItemName;
    using General::FindNumberInList;

    using ScheduleManager::CheckScheduleValue;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleMaxValue;
    using ScheduleManager::GetScheduleMinValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetZoneAirSetpoints: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int TempControlledZoneNum; // The Splitter that you are currently loading input into
    int NumAlphas;
    int NumNums;
    int ControlTypeNum;
    int IOStat;
    bool ErrorsFound(false);
    bool errFlag;
    int CTIndex;
    int HumidControlledZoneNum; // The Humidity Controller that information is being loaded into
    bool ValidScheduleControlType;
    bool ValidRadFractSched;          // check for if radiative fraction schedule has valid numbers
    bool ValidZoneOvercoolRangeSched; // check for if Zone Overcool range schedule has valid numbers
    int SchedMin;
    int SchedMax;
    int ActualZoneNum;
    int SchedTypeIndex;

    int ComfortControlledZoneNum; // The Splitter that you are currently loading input into
    int i;
    int IZoneCount;
    int found;
    int NumStageControlledZones; // Number of staged controlled objects
    int StageControlledZoneNum;  // Index for staged controlled zones

    Array1D_int CTSchedMapToControlledZone;
    Array1D_int CCmSchedMapToControlledZone;
    int Item;
    int Item1;
    int ZLItem;

    struct NeededControlTypes
    {
        // Members 4= the four control types + uncontrolled
        std::array<bool, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> MustHave = {false, false, false, false, false};
        std::array<bool, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> DidHave = {false, false, false, false, false};
    };

    struct NeededComfortControlTypes
    {
        // Members 4= the four control types + uncontrolled
        std::array<bool, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> MustHave = {false, false, false, false, false};
        std::array<bool, static_cast<int>(DataHVACGlobals::ThermostatType::Num)> DidHave = {false, false, false, false, false};
    };

    // Object Data
    Array1D<NeededControlTypes> TStatControlTypes;
    Array1D<NeededComfortControlTypes> TComfortControlTypes;

    // Formats
    static constexpr std::string_view Header(
        "! <Zone Volume Capacitance Multiplier>, Sensible Heat Capacity Multiplier, Moisture Capacity Multiplier, Carbon "
        "Dioxide Capacity Multiplier, Generic Contaminant Capacity Multiplier\n");
    static constexpr std::string_view Format_701("Zone Volume Capacitance Multiplier,{:8.3F} ,{:8.3F},{:8.3F},{:8.3F}\n");

    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    auto &TStatObjects = state.dataZoneCtrls->TStatObjects;
    auto &NumTStatStatements = state.dataZoneCtrls->NumTStatStatements;
    auto &NumTempControlledZones = state.dataZoneCtrls->NumTempControlledZones;
    auto &Zone = state.dataHeatBal->Zone;
    auto &ZoneList = state.dataHeatBal->ZoneList;
    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &HumidityControlZone = state.dataZoneCtrls->HumidityControlZone;
    auto &ComfortTStatObjects = state.dataZoneCtrls->ComfortTStatObjects;
    auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone;
    auto &NumOfZones = state.dataGlobal->NumOfZones;
    auto &StageControlledZone = state.dataZoneCtrls->StageControlledZone;
    auto &SetPointSingleHeating = state.dataZoneTempPredictorCorrector->SetPointSingleHeating;
    auto &SetPointSingleCooling = state.dataZoneTempPredictorCorrector->SetPointSingleCooling;
    auto &cAlphaArgs = state.dataIPShortCut->cAlphaArgs;
    auto &rNumericArgs = state.dataIPShortCut->rNumericArgs;
    auto &lNumericFieldBlanks = state.dataIPShortCut->lNumericFieldBlanks;
    auto &lAlphaFieldBlanks = state.dataIPShortCut->lAlphaFieldBlanks;
    auto &cAlphaFieldNames = state.dataIPShortCut->cAlphaFieldNames;
    auto &cNumericFieldNames = state.dataIPShortCut->cNumericFieldNames;
    auto &inputProcessor = state.dataInputProcessing->inputProcessor;
    auto &SetPointDualHeatCool = state.dataZoneTempPredictorCorrector->SetPointDualHeatCool;

    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::TStat));
    NumTStatStatements = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    TStatObjects.allocate(NumTStatStatements);

    // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
    NumTempControlledZones = 0;
    for (Item = 1; Item <= NumTStatStatements; ++Item) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Item,
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

        TStatObjects(Item).Name = cAlphaArgs(1);
        Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
        if (Item1 > 0) {
            TStatObjects(Item).TempControlledZoneStartPtr = NumTempControlledZones + 1;
            ++NumTempControlledZones;
            TStatObjects(Item).NumOfZones = 1;
            TStatObjects(Item).ZoneListActive = false;
            TStatObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            TStatObjects(Item).TempControlledZoneStartPtr = NumTempControlledZones + 1;
            NumTempControlledZones += ZoneList(ZLItem).NumOfZones;
            TStatObjects(Item).NumOfZones = ZoneList(ZLItem).NumOfZones;
            TStatObjects(Item).ZoneListActive = true;
            TStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowSevereError(state, "GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects.");
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        NumTempControlledZones = 0;
    }

    if (NumTempControlledZones > 0) {
        TempControlledZone.allocate(NumTempControlledZones);
        TStatControlTypes.allocate(NumTempControlledZones); // Number of set point types
        CTSchedMapToControlledZone.dimension(NumTempControlledZones, 0);

        TempControlledZoneNum = 0;
        state.dataZoneTempPredictorCorrector->NumOnOffCtrZone = 0;
        for (Item = 1; Item <= NumTStatStatements; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            for (Item1 = 1; Item1 <= TStatObjects(Item).NumOfZones; ++Item1) {
                ++TempControlledZoneNum;
                if (TStatObjects(Item).ZoneListActive) {
                    cAlphaArgs(2) = Zone(ZoneList(TStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                }
                int ZoneAssigned =
                    UtilityRoutines::FindItemInList(cAlphaArgs(2), TempControlledZone, &ZoneTempControls::ZoneName, TempControlledZoneNum - 1);
                if (ZoneAssigned == 0) {
                    TempControlledZone(TempControlledZoneNum).ZoneName = cAlphaArgs(2);
                    TempControlledZone(TempControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                    if (TempControlledZone(TempControlledZoneNum).ActualZoneNum == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                        ErrorsFound = true;
                    } else {
                        Zone(TempControlledZone(TempControlledZoneNum).ActualZoneNum).TempControlledZoneIndex = TempControlledZoneNum;
                    }
                } else {
                    TempControlledZone(TempControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                    ShowContinueError(state, "...Zone was previously assigned to Thermostat=\"" + TempControlledZone(ZoneAssigned).Name + "\".");
                    ErrorsFound = true;
                    continue;
                }

                if (!TStatObjects(Item).ZoneListActive) {
                    TempControlledZone(TempControlledZoneNum).Name = cAlphaArgs(1);
                } else {
                    CheckCreatedZoneItemName(state,
                                             RoutineName,
                                             cCurrentModuleObject,
                                             Zone(ZoneList(TStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name,
                                             ZoneList(TStatObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                                             TStatObjects(Item).Name,
                                             TempControlledZone,
                                             TempControlledZoneNum - 1,
                                             TempControlledZone(TempControlledZoneNum).Name,
                                             errFlag);
                    if (errFlag) ErrorsFound = true;
                }

                TempControlledZone(TempControlledZoneNum).ControlTypeSchedName = cAlphaArgs(3);
                TempControlledZone(TempControlledZoneNum).CTSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
                if (Item1 == 1) { // only show error on first of several if zone list
                    if (TempControlledZone(TempControlledZoneNum).CTSchedIndex == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Check validity of control types.
                        ValidScheduleControlType =
                            CheckScheduleValueMinMax(state, TempControlledZone(TempControlledZoneNum).CTSchedIndex, ">=", 0.0, "<=", 4.0);
                        if (!ValidScheduleControlType) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(2) + "=\"" +
                                                cAlphaArgs(2) + "\"");
                            ShowContinueError(state, "..contains values outside of range [0,4].");
                            ErrorsFound = true;
                        }
                    }
                }

                if (lAlphaFieldBlanks(7)) {
                    NumAlphas = 5;
                } else if (lAlphaFieldBlanks(9)) {
                    NumAlphas = 7;
                } else if (lAlphaFieldBlanks(11)) {
                    NumAlphas = 9;
                }

                TempControlledZone(TempControlledZoneNum).NumControlTypes = nint((NumAlphas - 3.0) / 2.0);
                TempControlledZone(TempControlledZoneNum).ControlType.allocate(TempControlledZone(TempControlledZoneNum).NumControlTypes);
                TempControlledZone(TempControlledZoneNum).ControlTypeName.allocate(TempControlledZone(TempControlledZoneNum).NumControlTypes);
                TempControlledZone(TempControlledZoneNum).ControlTypeEnum.allocate(TempControlledZone(TempControlledZoneNum).NumControlTypes);

                for (ControlTypeNum = 1; ControlTypeNum <= TempControlledZone(TempControlledZoneNum).NumControlTypes; ++ControlTypeNum) {

                    TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3));
                    TempControlledZone(TempControlledZoneNum).ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 3));

                    if (!TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum).empty()) {
                        auto ctrlType = static_cast<DataHVACGlobals::ThermostatType>(
                            getEnumerationValue(ValidControlTypesUC, TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum)));
                        TempControlledZone(TempControlledZoneNum).ControlTypeEnum(ControlTypeNum) = ctrlType;
                        if (ctrlType == DataHVACGlobals::ThermostatType::Invalid) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3)) + "\"");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                            cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)) + "=\"<blank>\"");
                        ErrorsFound = true;
                    }
                }
                if (NumNums > 0) {
                    if (rNumericArgs(1) >= 0.0) {
                        TempControlledZone(TempControlledZoneNum).DeltaTCutSet = rNumericArgs(1);
                        if (rNumericArgs(1) > 0.0) state.dataZoneTempPredictorCorrector->NumOnOffCtrZone++;
                    } else {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                        ShowContinueError(state, "..Allowable values must be greater or equal to 0");
                        ErrorsFound = true;
                    }
                }
                if (TempControlledZone(TempControlledZoneNum).DeltaTCutSet > 0.0) {
                    for (ControlTypeNum = 1; ControlTypeNum <= TempControlledZone(TempControlledZoneNum).NumControlTypes; ++ControlTypeNum) {
                        if (UtilityRoutines::SameString(TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum),
                                                        "ThermostatSetpoint:SingleHeatingOrCooling")) {
                            ShowWarningError(state,
                                             cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                                 ": The choice of Temperature Difference Between Cutout And Setpoint will not be applied to "
                                                 "ThermostatSetpoint:SingleHeatingOrCooling.");
                        }
                    }
                }
            }
        } // NumTStatStatements
    }     // Check on number of TempControlledZones

    cCurrentModuleObject = ValidControlTypesUC[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)];
    state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls > 0)
        SetPointSingleHeating.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &singleHtgSetpoint = SetPointSingleHeating(idx);
        singleHtgSetpoint.Name = cAlphaArgs(1);
        singleHtgSetpoint.TempSchedName = cAlphaArgs(2);
        singleHtgSetpoint.TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleHtgSetpoint.TempSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }

    } // SingleTempHeatingControlNum

    cCurrentModuleObject = ValidControlTypesUC[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)];
    state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls > 0)
        SetPointSingleCooling.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &singleClgSetpoint = SetPointSingleCooling(idx);
        singleClgSetpoint.Name = cAlphaArgs(1);
        singleClgSetpoint.TempSchedName = cAlphaArgs(2);
        singleClgSetpoint.TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleClgSetpoint.TempSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }

    } // SingleTempCoolingControlNum

    cCurrentModuleObject = ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)];
    state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
                                      cAlphaArgs,
                                      NumAlphas,
                                      rNumericArgs,
                                      NumNums,
                                      IOStat,
                                      lNumericFieldBlanks,
                                      lAlphaFieldBlanks,
                                      cAlphaFieldNames,
                                      cNumericFieldNames);
        auto &singleHeatCoolSetpoint = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(idx);
        singleHeatCoolSetpoint.Name = cAlphaArgs(1);
        singleHeatCoolSetpoint.TempSchedName = cAlphaArgs(2);
        singleHeatCoolSetpoint.TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleHeatCoolSetpoint.TempSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }

    } // SingleTempHeatCoolControlNum

    cCurrentModuleObject = ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)];
    state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls > 0)
        SetPointDualHeatCool.allocate(state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &dualHeatCoolSetpoint = SetPointDualHeatCool(idx);
        dualHeatCoolSetpoint.Name = cAlphaArgs(1);
        dualHeatCoolSetpoint.HeatTempSetptSchedName = cAlphaArgs(2);
        dualHeatCoolSetpoint.HeatTempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (dualHeatCoolSetpoint.HeatTempSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }
        dualHeatCoolSetpoint.CoolTempSetptSchedName = cAlphaArgs(3);
        dualHeatCoolSetpoint.CoolTempSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        if (dualHeatCoolSetpoint.CoolTempSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\" not found.");
            ErrorsFound = true;
        }

    } // DualTempHeatCoolControlNum

    // Finish filling in Schedule pointing indexes
    int setPointObjectArrayIndex;
    for (TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum) {
        for (int ct = 1; ct <= state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes; ct++) {
            switch (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeEnum(ct)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                setPointObjectArrayIndex =
                    UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(ct), SetPointSingleHeating);
                TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatSetPoint =
                    state.dataZoneTempPredictorCorrector->SetPointSingleHeating(setPointObjectArrayIndex).TempSchedIndex;
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                setPointObjectArrayIndex =
                    UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(ct), SetPointSingleCooling);
                TempControlledZone(TempControlledZoneNum).SchIndx_SingleCoolSetPoint =
                    state.dataZoneTempPredictorCorrector->SetPointSingleCooling(setPointObjectArrayIndex).TempSchedIndex;
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                setPointObjectArrayIndex = UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(ct),
                                                                     state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool);
                TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatCoolSetPoint =
                    state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(setPointObjectArrayIndex).TempSchedIndex;
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                setPointObjectArrayIndex = UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(ct),
                                                                     state.dataZoneTempPredictorCorrector->SetPointDualHeatCool);
                TempControlledZone(TempControlledZoneNum).SchIndx_DualSetPointWDeadBandHeat =
                    state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(setPointObjectArrayIndex).HeatTempSchedIndex;
                TempControlledZone(TempControlledZoneNum).SchIndx_DualSetPointWDeadBandCool =
                    state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(setPointObjectArrayIndex).CoolTempSchedIndex;
                break;
            default:
                assert(false);
            }
        }
    }

    // Now, Check the schedule values/indices for validity

    for (TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum) {

        ActualZoneNum = TempControlledZone(TempControlledZoneNum).ActualZoneNum;
        CTIndex = TempControlledZone(TempControlledZoneNum).CTSchedIndex;
        if (CTIndex == 0) continue; // error will be caught elsewhere
        SchedMin = GetScheduleMinValue(state, CTIndex);
        SchedMax = GetScheduleMaxValue(state, CTIndex);

        if (SchedMin == 0 && SchedMax == 0) {
            if (FindNumberInList(CTIndex, CTSchedMapToControlledZone, NumTempControlledZones) == 0) {
                ShowSevereError(state, "Control Type Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state, "..specifies control type 0 for all entries.");
                ShowContinueError(state, "All zones using this Control Type Schedule have no heating or cooling available.");
            }
            CTSchedMapToControlledZone(TempControlledZoneNum) = CTIndex;
        }

        for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

            int TempIndex = 0;
            switch (static_cast<DataHVACGlobals::ThermostatType>(ControlTypeNum)) {
            case DataHVACGlobals::ThermostatType::Uncontrolled:
                break;
            case DataHVACGlobals::ThermostatType::SingleHeating:
                TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatSetPoint;
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating))) {
                        ShowSevereError(state, "Control Type Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies control type 1 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                              TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_SingleCoolSetPoint;
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling))) {
                        ShowSevereError(state, "Control Type Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies control type 2 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                              TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatCoolSetPoint;
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool))) {
                        ShowSevereError(state, "Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies control type 3 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                              TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                TempIndex = TempControlledZone(TempControlledZoneNum)
                                .SchIndx_DualSetPointWDeadBandHeat; // using "Heat" as a sentinel that dualsetpoint is on this zone control object
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand))) {
                        ShowSevereError(state, "Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies control type 4 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                              TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            default:
                ShowSevereError(state,
                                format("GetZoneAirSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                       Zone(ActualZoneNum).Name,
                                       ControlTypeNum,
                                       TempControlledZone(TempControlledZoneNum).ControlTypeSchedName));
                ShowContinueError(state, "..valid range values are [0,4].");
                ErrorsFound = true;
            }
        }
    }

    for (TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum) {

        ActualZoneNum = TempControlledZone(TempControlledZoneNum).ActualZoneNum;
        CTIndex = TempControlledZone(TempControlledZoneNum).CTSchedIndex;
        if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

        for (ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum) {
            if (TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum] && TStatControlTypes(TempControlledZoneNum).DidHave[ControlTypeNum])
                continue;

            switch (static_cast<DataHVACGlobals::ThermostatType>(ControlTypeNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 1 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                      TempControlledZone(TempControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 2 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                      TempControlledZone(TempControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 3 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                      TempControlledZone(TempControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 4 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + '=' +
                                      TempControlledZone(TempControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                break;
            default:
                break;
            }
        }
    }

    if (allocated(TStatControlTypes)) TStatControlTypes.deallocate();
    // This starts the Humidity Control Get Input section
    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::HStat));
    state.dataZoneCtrls->NumHumidityControlZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneCtrls->NumHumidityControlZones > 0) {
        HumidityControlZone.allocate(state.dataZoneCtrls->NumHumidityControlZones);
        state.dataZoneTempPredictorCorrector->HumidityControlZoneUniqueNames.reserve(
            static_cast<unsigned>(state.dataZoneCtrls->NumHumidityControlZones));
    }

    for (HumidControlledZoneNum = 1; HumidControlledZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HumidControlledZoneNum) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      HumidControlledZoneNum,
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

        HumidityControlZone(HumidControlledZoneNum).ControlName = cAlphaArgs(1);
        GlobalNames::IntraObjUniquenessCheck(state,
                                             cAlphaArgs(2),
                                             cCurrentModuleObject,
                                             cAlphaFieldNames(2),
                                             state.dataZoneTempPredictorCorrector->HumidityControlZoneUniqueNames,
                                             ErrorsFound);

        HumidityControlZone(HumidControlledZoneNum).ZoneName = cAlphaArgs(2);
        HumidityControlZone(HumidControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItem(cAlphaArgs(2), Zone);
        if (HumidityControlZone(HumidControlledZoneNum).ActualZoneNum == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }
        HumidityControlZone(HumidControlledZoneNum).HumidifyingSched = cAlphaArgs(3);
        HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        if (HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\" not found.");
            ErrorsFound = true;
        }
        if (NumAlphas == 4) {
            HumidityControlZone(HumidControlledZoneNum).DehumidifyingSched = cAlphaArgs(4);
            HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
            if (HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                    "\" not found.");
                ErrorsFound = true;
            }
        } else {
            HumidityControlZone(HumidControlledZoneNum).DehumidifyingSched = cAlphaArgs(3);
            HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        }

    } // HumidControlledZoneNum

    // Start to read Thermal comfort control objects
    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat));
    state.dataZoneCtrls->NumComfortTStatStatements = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    ComfortTStatObjects.allocate(state.dataZoneCtrls->NumComfortTStatStatements);

    // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
    state.dataZoneCtrls->NumComfortControlledZones = 0;
    errFlag = false;
    for (Item = 1; Item <= state.dataZoneCtrls->NumComfortTStatStatements; ++Item) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Item,
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

        Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
        ComfortTStatObjects(Item).Name = cAlphaArgs(1);
        if (Item1 > 0) {
            ComfortTStatObjects(Item).ComfortControlledZoneStartPtr = state.dataZoneCtrls->NumComfortControlledZones + 1;
            ++state.dataZoneCtrls->NumComfortControlledZones;
            ComfortTStatObjects(Item).NumOfZones = 1;
            ComfortTStatObjects(Item).ZoneListActive = false;
            ComfortTStatObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            ComfortTStatObjects(Item).ComfortControlledZoneStartPtr = state.dataZoneCtrls->NumComfortControlledZones + 1;
            state.dataZoneCtrls->NumComfortControlledZones += ZoneList(ZLItem).NumOfZones;
            ComfortTStatObjects(Item).NumOfZones = ZoneList(ZLItem).NumOfZones;
            ComfortTStatObjects(Item).ZoneListActive = true;
            ComfortTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            errFlag = true;
            ErrorsFound = true;
        }
    }

    if (errFlag) {
        ShowSevereError(state, "GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects.");
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataZoneCtrls->NumComfortControlledZones = 0;
    }

    if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
        ComfortControlledZone.allocate(state.dataZoneCtrls->NumComfortControlledZones);
        TComfortControlTypes.allocate(state.dataZoneCtrls->NumComfortControlledZones); // Number of set point types
        CCmSchedMapToControlledZone.dimension(state.dataZoneCtrls->NumComfortControlledZones, 0);

        ComfortControlledZoneNum = 0;
        for (Item = 1; Item <= state.dataZoneCtrls->NumComfortTStatStatements; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            for (Item1 = 1; Item1 <= ComfortTStatObjects(Item).NumOfZones; ++Item1) {
                ++ComfortControlledZoneNum;
                if (ComfortTStatObjects(Item).ZoneListActive) {
                    cAlphaArgs(2) = state.dataHeatBal->Zone(ZoneList(ComfortTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                }
                int ZoneAssigned = UtilityRoutines::FindItemInList(
                    cAlphaArgs(2), ComfortControlledZone, &ZoneComfortControls::ZoneName, ComfortControlledZoneNum - 1);
                if (ZoneAssigned == 0) {
                    ComfortControlledZone(ComfortControlledZoneNum).ZoneName = cAlphaArgs(2);
                    ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                    if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }
                } else {
                    ComfortControlledZone(ComfortControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                    ShowContinueError(state, "...Zone was previously assigned to Thermostat=\"" + ComfortControlledZone(ZoneAssigned).Name + "\".");
                    ErrorsFound = true;
                    continue;
                }

                if (!ComfortTStatObjects(Item).ZoneListActive) {
                    ComfortControlledZone(ComfortControlledZoneNum).Name = cAlphaArgs(1);
                } else {
                    ComfortControlledZone(ComfortControlledZoneNum).Name =
                        state.dataHeatBal->Zone(ZoneList(ComfortTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name + ' ' +
                        ComfortTStatObjects(Item).Name;
                }

                // Read Fields A3 and A4 for averaging method
                IZoneCount = 0;
                for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                    if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) {
                        ++IZoneCount;
                    }
                }
                // Could not find a people object for this particular zone
                if (IZoneCount == 0 && ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum > 0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " no PEOPLE in " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" - cannot use Comfort Control.");
                    ErrorsFound = true;
                }
                ComfortControlledZone(ComfortControlledZoneNum).AverageMethod = DataZoneControls::AverageMethod::NO;
                if (IZoneCount > 1) {
                    ComfortControlledZone(ComfortControlledZoneNum).AverageMethodName = cAlphaArgs(3);
                    if (UtilityRoutines::SameString(cAlphaArgs(3), "SpecificObject")) {
                        ComfortControlledZone(ComfortControlledZoneNum).AverageMethod = DataZoneControls::AverageMethod::SPE;
                    }
                    if (UtilityRoutines::SameString(cAlphaArgs(3), "ObjectAverage")) {
                        ComfortControlledZone(ComfortControlledZoneNum).AverageMethod = DataZoneControls::AverageMethod::OBJ;
                    }
                    if (UtilityRoutines::SameString(cAlphaArgs(3), "PeopleAverage")) {
                        ComfortControlledZone(ComfortControlledZoneNum).AverageMethod = DataZoneControls::AverageMethod::PEO;
                    }
                    if (ComfortControlledZone(ComfortControlledZoneNum).AverageMethod == DataZoneControls::AverageMethod::NO) {
                        ShowSevereError(
                            state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                        ShowContinueError(state, "Allowed keys are SpecificObject, ObjectAverage, or PeopleAverage");
                        ErrorsFound = true;
                    }
                    if (ComfortControlledZone(ComfortControlledZoneNum).AverageMethod == DataZoneControls::AverageMethod::SPE) {
                        ComfortControlledZone(ComfortControlledZoneNum).AverageObjectName = cAlphaArgs(4);
                        if (UtilityRoutines::FindItem(cAlphaArgs(4), state.dataHeatBal->People) == 0) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                                "\".");
                            ErrorsFound = true;
                        } else {
                            ComfortControlledZone(ComfortControlledZoneNum).SpecificObjectNum =
                                UtilityRoutines::FindItem(cAlphaArgs(4), state.dataHeatBal->People);
                        }
                    }
                } else {
                    for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                        if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) break;
                    }
                    ComfortControlledZone(ComfortControlledZoneNum).SpecificObjectNum = i;
                }
                // Check values used for thermal comfort calculation
                for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                    if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) {
                        // Check activity level
                        if (state.dataHeatBal->People(i).ActivityLevelPtr > 0) {
                            ValidScheduleControlType =
                                CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).ActivityLevelPtr, ">=", 72.0, "<=", 909.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(state,
                                                "GetPeople Activity Level: Invalid activity level values entered for thermal comfort calculation");
                                ShowContinueError(state, "Outside of range values [72,909], Reference object=" + state.dataHeatBal->People(i).Name);
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            "GetPeople Activity Level: Activity level schedule is not found=" + state.dataHeatBal->People(i).Name);
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                        // Check Work Efficiency
                        if (state.dataHeatBal->People(i).WorkEffPtr > 0) {
                            ValidScheduleControlType = CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).WorkEffPtr, ">=", 0.0, "<=", 1.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(state,
                                                "GetPeople work efficiency: Invalid work efficiency values entered for thermal comfort calculation");
                                ShowContinueError(state, "Outside of range values [0,1], Reference object=" + state.dataHeatBal->People(i).Name);
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            "GetPeople work efficiency: Work efficiency schedule is not found=" + state.dataHeatBal->People(i).Name);
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                        // Check Clothing Insulation
                        if (state.dataHeatBal->People(i).ClothingPtr > 0) {
                            ValidScheduleControlType = CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).ClothingPtr, ">", 0.0, "<=", 2.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(
                                    state,
                                    "GetPeople Clothing Insulation: Invalid Clothing Insulation values entered for thermal comfort calculation");
                                ShowContinueError(state, "Outside of range values [0.0,2.0], Reference object=" + state.dataHeatBal->People(i).Name);
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            "GetPeople Clothing Insulation: Clothing Insulation schedule is not found=" +
                                                state.dataHeatBal->People(i).Name);
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                        // Check Air velocity
                        if (state.dataHeatBal->People(i).AirVelocityPtr <= 0) {
                            ShowSevereError(state, "GetPeople Air Velocity: Air velocity schedule is not found=" + state.dataHeatBal->People(i).Name);
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                    }
                }

                // Read Max and Min temperature setpoint
                if (NumNums > 0) {
                    ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint = rNumericArgs(1);
                    if (rNumericArgs(1) > 50 || rNumericArgs(1) < 0) {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                        ShowContinueError(state, "..Allowable values must be between 0 C and 50 C");
                        ErrorsFound = true;
                    }
                }
                if (NumNums > 1) {
                    ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint = rNumericArgs(2);
                    if (rNumericArgs(2) > 50 || rNumericArgs(2) < 0) {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(2), rNumericArgs(2)));
                        ShowContinueError(state, "..Allowable values must be between 0 C and 50 C");
                        ErrorsFound = true;
                    }
                }
                // Ensure MaxTemp >= MinTemp
                if (ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint > ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                    ShowContinueError(state, ".." + cNumericFieldNames(1) + " > " + cNumericFieldNames(2));
                    ShowContinueError(state, format("..[{:.0T}] > [{:.0T}].", rNumericArgs(1), rNumericArgs(2)));
                    ErrorsFound = true;
                }
                // If MaxTemp = MinTemp, no thermal comfort control
                if (ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint ==
                    ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                    ShowContinueError(state, ".." + cNumericFieldNames(1) + " = " + cNumericFieldNames(2));
                    ShowContinueError(state, "The zone will be controlled using this dry-bulb temperature setpoint.");
                }
                // read Thermal comfort type schedule name
                ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName = cAlphaArgs(5);
                ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex = GetScheduleIndex(state, cAlphaArgs(5));
                if (ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex == 0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                        "\" not found.");
                    ErrorsFound = true;
                } else {
                    // Check validity of control types.
                    ValidScheduleControlType =
                        CheckScheduleValueMinMax(state, ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex, ">=", 0.0, "<=", 4.0);
                    if (!ValidScheduleControlType) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(5) + "=\"" +
                                            cAlphaArgs(5) + "\"");
                        ShowContinueError(state, "..contains values outside of range [0,4].");
                        ErrorsFound = true;
                    }
                }
                ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes = nint((NumAlphas - 5.0) / 2.0);
                ComfortControlledZone(ComfortControlledZoneNum).ControlType.allocate(ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
                ComfortControlledZone(ComfortControlledZoneNum)
                    .ControlTypeName.allocate(ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
                ComfortControlledZone(ComfortControlledZoneNum)
                    .ControlTypeSchIndx.allocate(ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);

                for (ControlTypeNum = 1; ControlTypeNum <= ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes; ++ControlTypeNum) {
                    ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5));
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 5));
                    if (ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum) != "") {
                        CTIndex = getEnumerationValue(
                            ValidComfortControlTypesUC,
                            UtilityRoutines::MakeUPPERCase(ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum)));
                        if (CTIndex == 0) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5)) + "\"");
                            ErrorsFound = true;
                        }
                        if (CTIndex > 4) { // For Fanger control only for the time being
                            ShowSevereError(state,
                                            cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5)) + "\"");
                            ShowContinueError(state, "..Fanger is the only valid model.");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                            cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"<blank>\"");
                        ErrorsFound = true;
                    }
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ControlTypeNum) = 0;
                }
            }
        } // NumComfortTStatStatements
    }
    // End of Thermal comfort control reading and checking

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)];
    state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger.allocate(
            state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &singleSetpointHtgFanger = state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(idx);
        singleSetpointHtgFanger.Name = cAlphaArgs(1);
        singleSetpointHtgFanger.PMVSchedName = cAlphaArgs(2);
        singleSetpointHtgFanger.PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleSetpointHtgFanger.PMVSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, singleSetpointHtgFanger.PMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }
    } // SingleFangerHeatingControlNum

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)];
    state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls > 0) {
        state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger.allocate(
            state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls);
    }

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &singleSetpointClgFanger = state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(idx);
        singleSetpointClgFanger.Name = cAlphaArgs(1);
        singleSetpointClgFanger.PMVSchedName = cAlphaArgs(2);
        singleSetpointClgFanger.PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleSetpointClgFanger.PMVSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, singleSetpointClgFanger.PMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }

    } // SingleFangerCoolingControlNum

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)];
    state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger.allocate(
            state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &singleSetpointHeatCoolFanger = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(idx);
        singleSetpointHeatCoolFanger.Name = cAlphaArgs(1);
        singleSetpointHeatCoolFanger.PMVSchedName = cAlphaArgs(2);
        singleSetpointHeatCoolFanger.PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleSetpointHeatCoolFanger.PMVSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, singleSetpointHeatCoolFanger.PMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }

    } // SingleFangerHeatCoolControlNum

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)];
    state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger.allocate(
            state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls);

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls; ++idx) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      idx,
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
        auto &dualSetpointHeatCoolFanger = state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(idx);
        dualSetpointHeatCoolFanger.Name = cAlphaArgs(1);
        dualSetpointHeatCoolFanger.HeatPMVSetptSchedName = cAlphaArgs(2);
        dualSetpointHeatCoolFanger.HeatPMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (dualSetpointHeatCoolFanger.HeatPMVSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }
        dualSetpointHeatCoolFanger.CoolPMVSetptSchedName = cAlphaArgs(3);
        dualSetpointHeatCoolFanger.CoolPMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        if (dualSetpointHeatCoolFanger.CoolPMVSchedIndex == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\" not found.");
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, dualSetpointHeatCoolFanger.HeatPMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
            ValidScheduleControlType = CheckScheduleValueMinMax(state, dualSetpointHeatCoolFanger.CoolPMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\" entered.");
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }

    } // DualFangerHeatCoolControlNum

    // Finish filling in Schedule pointing indexes for Thermal Comfort Control
    for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {

        int ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)],
                                                     ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
        ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SingleHeating = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) =
                UtilityRoutines::FindItem(ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex),
                                          state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)] = true;
        }

        ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)],
                                                 ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                 ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
        ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SingleCooling = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) =
                UtilityRoutines::FindItem(ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex),
                                          state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)] = true;
        }

        ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)],
                                                 ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                 ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
        ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SingleHeatCool = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) =
                UtilityRoutines::FindItem(ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex),
                                          state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)] = true;
        }

        ComfortIndex =
            UtilityRoutines::FindItem(ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)],
                                      ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                      ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
        ComfortControlledZone(ComfortControlledZoneNum).SchIndx_DualSetPointWithDeadBand = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) =
                UtilityRoutines::FindItem(ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex),
                                          state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)] =
                true;
        }
    }

    // Now, Check the schedule values/indices for validity for Thermal Comfort Control

    for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {

        ActualZoneNum = ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum;
        CTIndex = ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex;
        if (CTIndex == 0) continue; // error will be caught elsewhere
        SchedMin = GetScheduleMinValue(state, CTIndex);
        SchedMax = GetScheduleMaxValue(state, CTIndex);

        if (SchedMin == 0 && SchedMax == 0) {
            if (FindNumberInList(CTIndex, CCmSchedMapToControlledZone, state.dataZoneCtrls->NumComfortControlledZones) == 0) {
                ShowWarningError(state, "Control Type Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state, "..specifies control type 0 for all entries.");
                ShowContinueError(state, "All zones using this Control Type Schedule have no thermal comfort control.");
            }
            CCmSchedMapToControlledZone(ComfortControlledZoneNum) = CTIndex;
        }

        for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

            int ComfortIndex;
            switch (static_cast<DataHVACGlobals::ThermostatType>(ControlTypeNum)) {
            case DataHVACGlobals::ThermostatType::Uncontrolled:
                break;
            case DataHVACGlobals::ThermostatType::SingleHeating:
                ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SingleHeating;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)],
                                               ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating))) {
                        ShowSevereError(state, "Control Type Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies thermal control type 1 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                              ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SingleCooling;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)],
                                               ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling))) {
                        ShowSevereError(state, "Control Type Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies thermal control type 2 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                              ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SingleHeatCool;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)],
                                               ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool))) {
                        ShowSevereError(state, "Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state,
                                          format("..specifies thermal control type 3 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                              ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_DualSetPointWithDeadBand;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)] =
                    true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)],
                                               ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand))) {
                        ShowSevereError(state, "Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(
                            state,
                            format("..specifies thermal control type 4 ({}) as the control type. Not valid for this zone.",
                                   ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)]));
                        ShowContinueError(state,
                                          "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                              ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                break;
            default:
                ShowSevereError(state,
                                format("GetZoneAirSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                       Zone(ActualZoneNum).Name,
                                       ControlTypeNum,
                                       ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName));
                ShowContinueError(state, "..valid range values are [0,4].");
                ErrorsFound = true;
                break;
            }
        }
    }

    for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {

        ActualZoneNum = ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum;
        CTIndex = ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex;
        if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

        for (ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum) {
            if (TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum] &&
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[ControlTypeNum])
                continue;

            switch (static_cast<DataHVACGlobals::ThermostatType>(ControlTypeNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 1 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                      ComfortControlledZone(ComfortControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 2 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                      ComfortControlledZone(ComfortControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 3 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                      ComfortControlledZone(ComfortControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, "Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                ShowContinueError(state,
                                  format("...should include control type 4 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)]));
                ShowContinueError(state,
                                  "..reference " + cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)) + '=' +
                                      ComfortControlledZone(ComfortControlledZoneNum).Name);
                ShowContinueError(state, "..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                break;
            default:
                break;
            }
        }
    }

    if (allocated(TComfortControlTypes)) TComfortControlTypes.deallocate();

    // Get the Hybrid Model setting inputs
    GetHybridModelZone(state);

    // Default multiplier values
    Real64 ZoneVolCapMultpSens = 1.0;
    Real64 ZoneVolCapMultpMoist = 1.0;
    Real64 ZoneVolCapMultpCO2 = 1.0;
    Real64 ZoneVolCapMultpGenContam = 1.0;

    // Get the Zone Air Capacitance Multiplier for use in the Predictor-Corrector Procedure
    cCurrentModuleObject = "ZoneCapacitanceMultiplier:ResearchSpecial";
    int NumZoneCapaMultiplier = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); // Number of ZonesCapacityMultiplier object
    if (NumZoneCapaMultiplier == 0) {
        // Assign default multiplier values to all zones
        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ZoneNum++) {
            Zone(ZoneNum).ZoneVolCapMultpSens = ZoneVolCapMultpSens;
            Zone(ZoneNum).ZoneVolCapMultpMoist = ZoneVolCapMultpMoist;
            Zone(ZoneNum).ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2;
            Zone(ZoneNum).ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam;
        }

    } else {

        // Allow user to specify ZoneCapacitanceMultiplier:ResearchSpecial at zone level
        // Added by S. Lee and R. Zhang in Oct. 2016.
        // Assign the user inputted multipliers to specified zones
        for (int ZoneCapNum = 1; ZoneCapNum <= NumZoneCapaMultiplier; ZoneCapNum++) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          ZoneCapNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (lAlphaFieldBlanks(2)) {
                // default multiplier values for all the zones not specified (zone or zonelist name field is empty)
                ZoneVolCapMultpSens = rNumericArgs(1);
                ZoneVolCapMultpMoist = rNumericArgs(2);
                ZoneVolCapMultpCO2 = rNumericArgs(3);
                ZoneVolCapMultpGenContam = rNumericArgs(4);
            } else {
                // multiplier values for the specified zone(s)
                int ZoneNum = 0;
                ZLItem = 0;
                Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
                if (Item1 > 0) {
                    ZoneNum = Item1;
                    Zone(ZoneNum).FlagCustomizedZoneCap = true;
                    Zone(ZoneNum).ZoneVolCapMultpSens = rNumericArgs(1);
                    Zone(ZoneNum).ZoneVolCapMultpMoist = rNumericArgs(2);
                    Zone(ZoneNum).ZoneVolCapMultpCO2 = rNumericArgs(3);
                    Zone(ZoneNum).ZoneVolCapMultpGenContam = rNumericArgs(4);
                } else if (ZLItem > 0) {
                    for (int ZonePtrNum = 1; ZonePtrNum < ZoneList(ZLItem).NumOfZones; ZonePtrNum++) {
                        ZoneNum = ZoneList(ZLItem).Zone(ZonePtrNum);
                        Zone(ZoneNum).FlagCustomizedZoneCap = true;
                        Zone(ZoneNum).ZoneVolCapMultpSens = rNumericArgs(1);
                        Zone(ZoneNum).ZoneVolCapMultpMoist = rNumericArgs(2);
                        Zone(ZoneNum).ZoneVolCapMultpCO2 = rNumericArgs(3);
                        Zone(ZoneNum).ZoneVolCapMultpGenContam = rNumericArgs(4);
                    }

                } else {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" not found.");
                    ErrorsFound = true;
                }
            }
        }

        // Assign default multiplier values to all the other zones
        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ZoneNum++) {
            if (!Zone(ZoneNum).FlagCustomizedZoneCap) {
                Zone(ZoneNum).ZoneVolCapMultpSens = ZoneVolCapMultpSens;
                Zone(ZoneNum).ZoneVolCapMultpMoist = ZoneVolCapMultpMoist;
                Zone(ZoneNum).ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2;
                Zone(ZoneNum).ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam;
            }
        }

        // Calculate the average multiplier value from all zones
        {
            Real64 ZoneVolCapMultpSens_temp = 0.0;
            Real64 ZoneVolCapMultpMoist_temp = 0.0;
            Real64 ZoneVolCapMultpCO2_temp = 0.0;
            Real64 ZoneVolCapMultpGenContam_temp = 0.0;

            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ZoneNum++) {
                ZoneVolCapMultpSens_temp += Zone(ZoneNum).ZoneVolCapMultpSens;
                ZoneVolCapMultpMoist_temp += Zone(ZoneNum).ZoneVolCapMultpMoist;
                ZoneVolCapMultpCO2_temp += Zone(ZoneNum).ZoneVolCapMultpCO2;
                ZoneVolCapMultpGenContam_temp += Zone(ZoneNum).ZoneVolCapMultpGenContam;
            }

            if (NumOfZones > 0) {
                ZoneVolCapMultpSens = ZoneVolCapMultpSens_temp / NumOfZones;
                ZoneVolCapMultpMoist = ZoneVolCapMultpMoist_temp / NumOfZones;
                ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2_temp / NumOfZones;
                ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam_temp / NumOfZones;
            }
        }
    }

    print(state.files.eio, Header);
    print(state.files.eio, Format_701, ZoneVolCapMultpSens, ZoneVolCapMultpMoist, ZoneVolCapMultpCO2, ZoneVolCapMultpGenContam);

    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::OTTStat));
    state.dataZoneCtrls->NumOpTempControlledZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneCtrls->NumOpTempControlledZones > 0) {
        state.dataZoneCtrls->AnyOpTempControl = true;

        for (int idx = 1; idx <= state.dataZoneCtrls->NumOpTempControlledZones; ++idx) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          idx,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // find matching name of  ZONECONTROL:THERMOSTAT object
            found = UtilityRoutines::FindItem(cAlphaArgs(1), TStatObjects);
            if (found == 0) {
                // It might be in the TempControlledZones
                found = UtilityRoutines::FindItem(cAlphaArgs(1), TempControlledZone);
                if (found == 0) { // throw error
                    ShowSevereError(state,
                                    cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " +
                                        cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + " reference not found.");
                    ErrorsFound = true;
                } else {
                    TempControlledZoneNum = found;
                    TempControlledZone(TempControlledZoneNum).OperativeTempControl = true;
                    if (UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled")) {
                        TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled = true;
                    }
                    if ((!(UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled"))) && (!(UtilityRoutines::SameString(cAlphaArgs(2), "Constant")))) {
                        ShowSevereError(
                            state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                        ErrorsFound = true;
                    }

                    TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction = rNumericArgs(1);
                    TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched = GetScheduleIndex(state, cAlphaArgs(3));
                    if ((TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched == 0) &&
                        (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled)) { // throw error
                        ShowSevereError(state,
                                        cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }

                    // check validity of fixed radiative fraction
                    if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction < 0.0) &&
                        (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }
                    if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction >= 0.9) &&
                        (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot >= .9.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }

                    // check schedule min max.
                    if (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled) {
                        ValidRadFractSched = CheckScheduleValueMinMax(
                            state, TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                        if (!ValidRadFractSched) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(3) + "=[" +
                                                cAlphaArgs(3) + "\".");
                            ShowContinueError(state, "..Values outside of range [0.0,0.9).");
                            ErrorsFound = true;
                        }
                    }

                    // added Jan, 2017 - Xuan Luo
                    // read adaptive comfort model and calculate adaptive thermal comfort setpoint
                    if (TempControlledZone(TempControlledZoneNum).OperativeTempControl) {
                        if (NumAlphas >= 4 && !lAlphaFieldBlanks(4)) {
                            int adaptiveComfortModelTypeIndex =
                                UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                            if (!adaptiveComfortModelTypeIndex) {
                                ShowSevereError(state,
                                                cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                    cAlphaArgs(4) + "\" not found.");
                                ErrorsFound = true;
                            } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                TempControlledZone(TempControlledZoneNum).AdaptiveComfortTempControl = true;
                                TempControlledZone(TempControlledZoneNum).AdaptiveComfortModelTypeIndex =
                                    UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                if (!state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.initialized) {
                                    Array1D<Real64> runningAverageASH(state.dataWeatherManager->NumDaysInYear, 0.0);
                                    Array1D<Real64> runningAverageCEN(state.dataWeatherManager->NumDaysInYear, 0.0);
                                    CalculateMonthlyRunningAverageDryBulb(state, runningAverageASH, runningAverageCEN);
                                    CalculateAdaptiveComfortSetPointSchl(state, runningAverageASH, runningAverageCEN);
                                }
                            }
                        }
                    }

                    // CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
                    SetupOutputVariable(state,
                                        "Zone Thermostat Operative Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataHeatBal->ZnAirRpt(TempControlledZone(TempControlledZoneNum).ActualZoneNum).ThermOperativeTemp,
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        Zone(TempControlledZone(TempControlledZoneNum).ActualZoneNum).Name);
                }
            } else {
                for (Item = 1; Item <= TStatObjects(found).NumOfZones; ++Item) {
                    TempControlledZoneNum = TStatObjects(found).TempControlledZoneStartPtr + Item - 1;
                    if (NumTempControlledZones == 0) continue;
                    TempControlledZone(TempControlledZoneNum).OperativeTempControl = true;
                    if (UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled")) {
                        TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled = true;
                    }
                    if (Item == 1) {
                        if ((!(UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled"))) &&
                            (!(UtilityRoutines::SameString(cAlphaArgs(2), "Constant")))) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                "\".");
                            ErrorsFound = true;
                        }
                    }

                    TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction = rNumericArgs(1);
                    TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched = GetScheduleIndex(state, cAlphaArgs(3));
                    if (Item == 1) {
                        if ((TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched == 0) &&
                            (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled)) { // throw error
                            ShowSevereError(state,
                                            cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                                "\" not found.");
                            ErrorsFound = true;
                        }
                    }

                    // check validity of fixed radiative fraction
                    if (Item == 1) {
                        if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction < 0.0) &&
                            (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                    }
                    if (Item == 1) {
                        if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction >= 0.9) &&
                            (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot >= .9.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                    }

                    // check schedule min max.
                    if (Item == 1) {
                        if (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled) {
                            ValidRadFractSched = CheckScheduleValueMinMax(
                                state, TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                            if (!ValidRadFractSched) {
                                ShowSevereError(state,
                                                cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(3) + "=[" +
                                                    cAlphaArgs(3) + "\".");
                                ShowContinueError(state, "..Values outside of range [0.0,0.9).");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // added Jan, 2017 - Xuan Luo
                    // read adaptive comfort model and calculate adaptive thermal comfort setpoint
                    if (TempControlledZone(TempControlledZoneNum).OperativeTempControl) {
                        if (NumAlphas >= 4 && !lAlphaFieldBlanks(4)) {
                            int adaptiveComfortModelTypeIndex =
                                UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                            if (!adaptiveComfortModelTypeIndex) {
                                ShowSevereError(state,
                                                cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                    cAlphaArgs(4) + "\" not found.");
                                ErrorsFound = true;
                            } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                TempControlledZone(TempControlledZoneNum).AdaptiveComfortTempControl = true;
                                TempControlledZone(TempControlledZoneNum).AdaptiveComfortModelTypeIndex =
                                    UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                if (!state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.initialized) {
                                    Array1D<Real64> runningAverageASH(state.dataWeatherManager->NumDaysInYear, 0.0);
                                    Array1D<Real64> runningAverageCEN(state.dataWeatherManager->NumDaysInYear, 0.0);
                                    CalculateMonthlyRunningAverageDryBulb(state, runningAverageASH, runningAverageCEN);
                                    CalculateAdaptiveComfortSetPointSchl(state, runningAverageASH, runningAverageCEN);
                                }
                            }
                        }
                    }

                    // CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
                    SetupOutputVariable(state,
                                        "Zone Thermostat Operative Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataHeatBal->ZnAirRpt(TempControlledZone(TempControlledZoneNum).ActualZoneNum).ThermOperativeTemp,
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        Zone(TempControlledZone(TempControlledZoneNum).ActualZoneNum).Name);
                } // TStat Objects Loop
            }     // found thermostat referene
        }         // loop over NumOpTempControlledZones
    }             // NumOpTempControlledZones > 0

    // Overcool dehumidificaton GetInput starts here
    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::TandHStat));
    state.dataZoneCtrls->NumTempAndHumidityControlledZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneCtrls->NumTempAndHumidityControlledZones > 0) {
        state.dataZoneCtrls->AnyZoneTempAndHumidityControl = true;

        for (int idx = 1; idx <= state.dataZoneCtrls->NumTempAndHumidityControlledZones; ++idx) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          idx,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // find matching name of  ZONECONTROL:THERMOSTAT object
            found = UtilityRoutines::FindItem(cAlphaArgs(1), TStatObjects);
            if (found == 0) {
                // It might be in the TempControlledZones
                found = UtilityRoutines::FindItem(cAlphaArgs(1), TempControlledZone);
                if (found == 0) { // throw error
                    ShowSevereError(state,
                                    cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " +
                                        cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)) + " reference not found.");
                    ErrorsFound = true;
                } else {
                    TempControlledZoneNum = found;
                    TempControlledZone(TempControlledZoneNum).DehumidifyingSched = cAlphaArgs(2);
                    TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
                    if (TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = true;
                    if ((UtilityRoutines::SameString(cAlphaArgs(3), "None"))) {
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = false;
                    }
                    if (UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled")) {
                        TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled = true;
                    }
                    if ((!(UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled"))) && (!(UtilityRoutines::SameString(cAlphaArgs(4), "Constant")))) {
                        ShowSevereError(
                            state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                        ErrorsFound = true;
                    }

                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange = rNumericArgs(1);
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                    if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex == 0) &&
                        (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled)) { // throw error
                        ShowSevereError(state,
                                        cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }

                    // check validity of zone Overcool constant range
                    if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange < 0.0) &&
                        (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }
                    if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange > 3.0) &&
                        (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be > 3.0",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }

                    // check zone Overcool range schedule min/max values.
                    if (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled) {
                        ValidZoneOvercoolRangeSched = CheckScheduleValueMinMax(
                            state, TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                        if (!ValidZoneOvercoolRangeSched) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(5) + "=[" +
                                                cAlphaArgs(5) + "\".");
                            ShowContinueError(state, "..Values outside of range [0.0,3.0].");
                            ErrorsFound = true;
                        }
                    }
                    // check Overcool Control Ratio limits
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio = rNumericArgs(2);
                    if (TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio < 0.0) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(2),
                                               cNumericFieldNames(2),
                                               rNumericArgs(2)));
                        ErrorsFound = true;
                    }
                }
            } else {
                for (Item = 1; Item <= TStatObjects(found).NumOfZones; ++Item) {
                    TempControlledZoneNum = TStatObjects(found).TempControlledZoneStartPtr + Item - 1;
                    TempControlledZone(TempControlledZoneNum).DehumidifyingSched = cAlphaArgs(2);
                    TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
                    if (TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = true;
                    if ((UtilityRoutines::SameString(cAlphaArgs(3), "None"))) {
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = false;
                    }
                    if (UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled")) {
                        TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled = false;
                    }
                    if (Item == 1) {
                        if ((!(UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled"))) &&
                            (!(UtilityRoutines::SameString(cAlphaArgs(4), "Constant")))) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                                "\".");
                            ErrorsFound = true;
                        }
                    }
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange = rNumericArgs(1);
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex = GetScheduleIndex(state, cAlphaArgs(6));
                    if (Item == 1) {
                        if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex == 0) &&
                            (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled)) { // throw error
                            ShowSevereError(state,
                                            cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                                "\" not found.");
                            ErrorsFound = true;
                        }
                    }
                    // check validity of zone Overcool constant range
                    if (Item == 1) {
                        if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange < 0.0) &&
                            (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                    }
                    if (Item == 1) {
                        if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange > 3.0) &&
                            (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot > 3.0",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                    }
                    // check zone Overcool range schedule min/max values.
                    if (Item == 1) {
                        if (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled) {
                            ValidZoneOvercoolRangeSched = CheckScheduleValueMinMax(
                                state, TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                            if (!ValidZoneOvercoolRangeSched) {
                                ShowSevereError(state,
                                                cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(5) + "=[" +
                                                    cAlphaArgs(5) + "\".");
                                ShowContinueError(state, "..Values outside of range [0.0,3.0].");
                                ErrorsFound = true;
                            }
                        }
                    }
                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio = rNumericArgs(2);
                    // check Overcool Control Ratio limits
                    if (Item == 1) {
                        if (TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio < 0.0) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(2),
                                                   cNumericFieldNames(2),
                                                   rNumericArgs(2)));
                            ErrorsFound = true;
                        }
                    }

                } // TStat Objects Loop
            }     // found thermostat reference
        }         // loop over NumTempAndHumidityControlledZones
    }             // NumTempAndHumidityControlledZones > 0

    // Staged thermostat control inputs start
    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::StagedDual));
    NumStageControlledZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (NumStageControlledZones > 0) state.dataZoneCtrls->StagedTStatObjects.allocate(NumStageControlledZones);

    // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
    state.dataZoneTempPredictorCorrector->NumStageCtrZone = 0;
    for (Item = 1; Item <= NumStageControlledZones; ++Item) {
        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Item,
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

        state.dataZoneCtrls->StagedTStatObjects(Item).Name = cAlphaArgs(1);
        Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
        if (Item1 > 0) {
            state.dataZoneCtrls->StagedTStatObjects(Item).StageControlledZoneStartPtr = state.dataZoneTempPredictorCorrector->NumStageCtrZone + 1;
            ++state.dataZoneTempPredictorCorrector->NumStageCtrZone;
            state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones = 1;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive = false;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            state.dataZoneCtrls->StagedTStatObjects(Item).TempControlledZoneStartPtr = state.dataZoneTempPredictorCorrector->NumStageCtrZone + 1;
            state.dataZoneTempPredictorCorrector->NumStageCtrZone += ZoneList(ZLItem).NumOfZones;
            state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones = ZoneList(ZLItem).NumOfZones;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive = true;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowSevereError(state, "GetStagedDualSetpoint: Errors with invalid names in " + cCurrentModuleObject + " objects.");
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataZoneTempPredictorCorrector->NumStageCtrZone = 0;
    }

    if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
        StageControlledZone.allocate(state.dataZoneTempPredictorCorrector->NumStageCtrZone);
        state.dataZoneCtrls->StageZoneLogic.dimension(NumOfZones, false);

        StageControlledZoneNum = 0;
        for (Item = 1; Item <= NumStageControlledZones; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            for (Item1 = 1; Item1 <= state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones; ++Item1) {
                ++StageControlledZoneNum;
                if (state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive) {
                    cAlphaArgs(2) =
                        state.dataHeatBal->Zone(ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                }
                int ZoneAssigned =
                    UtilityRoutines::FindItemInList(cAlphaArgs(2), StageControlledZone, &ZoneStagedControls::ZoneName, StageControlledZoneNum - 1);
                if (ZoneAssigned == 0) {
                    StageControlledZone(StageControlledZoneNum).ZoneName = cAlphaArgs(2);
                    StageControlledZone(StageControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                    if (StageControlledZone(StageControlledZoneNum).ActualZoneNum == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                        ErrorsFound = true;
                    } else {
                        //           Zone(StageControlledZone(StageControlledZoneNum)%ActualZoneNum)%StageControlledZoneIndex =
                        //           StageControlledZoneNum
                    }
                    state.dataZoneCtrls->StageZoneLogic(StageControlledZone(StageControlledZoneNum).ActualZoneNum) = true;
                } else {
                    StageControlledZone(StageControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                    ShowContinueError(state, "...Zone was previously assigned to Thermostat=\"" + StageControlledZone(ZoneAssigned).Name + "\".");
                    ErrorsFound = true;
                    continue;
                }

                if (!state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive) {
                    StageControlledZone(StageControlledZoneNum).Name = cAlphaArgs(1);
                } else {
                    CheckCreatedZoneItemName(
                        state,
                        RoutineName,
                        cCurrentModuleObject,
                        state.dataHeatBal->Zone(ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name,
                        ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                        state.dataZoneCtrls->StagedTStatObjects(Item).Name,
                        StageControlledZone,
                        StageControlledZoneNum - 1,
                        StageControlledZone(StageControlledZoneNum).Name,
                        errFlag);
                    if (errFlag) ErrorsFound = true;
                }

                StageControlledZone(StageControlledZoneNum).NumOfHeatStages = rNumericArgs(1);
                if (rNumericArgs(1) < 1 || rNumericArgs(1) > 4) {
                    ShowSevereError(
                        state,
                        format("{}=\"{}\" invalid range {}=\"{:.0R}\"", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                    ShowContinueError(state, "..contains values outside of range [1,4].");
                    ErrorsFound = true;
                }

                StageControlledZone(StageControlledZoneNum).HeatSetBaseSchedName = cAlphaArgs(3);
                StageControlledZone(StageControlledZoneNum).HSBchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
                if (Item1 == 1) { // only show error on first of several if zone list
                    if (StageControlledZone(StageControlledZoneNum).HSBchedIndex == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }
                }

                StageControlledZone(StageControlledZoneNum).HeatThroRange = rNumericArgs(2);
                if (rNumericArgs(1) < 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"" + cAlphaArgs(1) + "\" negative value is found at {}=\"{:.1R}\"",
                                           cCurrentModuleObject,
                                           cNumericFieldNames(2),
                                           rNumericArgs(2)));
                    ShowContinueError(state, ".. The minumum value is 0.");
                    ErrorsFound = true;
                }

                if (StageControlledZone(StageControlledZoneNum).NumOfHeatStages > 0) {
                    StageControlledZone(StageControlledZoneNum).HeatTOffset.allocate(StageControlledZone(StageControlledZoneNum).NumOfHeatStages);
                    for (i = 1; i <= StageControlledZone(StageControlledZoneNum).NumOfHeatStages; ++i) {
                        StageControlledZone(StageControlledZoneNum).HeatTOffset(i) = rNumericArgs(2 + i);
                        if (rNumericArgs(2 + i) > 0.0) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" positive value is found at " +
                                                format("{}=\"{:.1R}\"", cNumericFieldNames(2 + i), rNumericArgs(2 + i)));
                            ShowContinueError(state, ".. The maximum value is 0.");
                            ErrorsFound = true;
                        }
                        if (lNumericFieldBlanks(2 + i)) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + " object =" + cAlphaArgs(1) + ". The input of " + cNumericFieldNames(2 + i) +
                                                " is required, but a blank is found.");
                            ErrorsFound = true;
                        }
                        if (i > 1) {
                            if (rNumericArgs(2 + i) >= rNumericArgs(1 + i)) {
                                ShowSevereError(state,
                                                format("{}=\"{}\" The value at {}=\"{:.1R}\" has to be less than ",
                                                       cCurrentModuleObject,
                                                       cAlphaArgs(1),
                                                       cNumericFieldNames(2 + i),
                                                       rNumericArgs(2 + i)));
                                ShowContinueError(state, format("{}=\"{:.1R}", cNumericFieldNames(1 + i), rNumericArgs(1 + i)));
                                ErrorsFound = true;
                            }
                        }
                    }
                }

                StageControlledZone(StageControlledZoneNum).NumOfCoolStages = rNumericArgs(7);
                if (rNumericArgs(7) < 1 || rNumericArgs(7) > 4) {
                    ShowSevereError(
                        state,
                        format("{}=\"{}\" invalid range {}=\"{:.0R}\"", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(7), rNumericArgs(7)));
                    ShowContinueError(state, "..contains values outside of range [1,4].");
                    ErrorsFound = true;
                }

                StageControlledZone(StageControlledZoneNum).CoolSetBaseSchedName = cAlphaArgs(4);
                StageControlledZone(StageControlledZoneNum).CSBchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                if (Item1 == 1) { // only show error on first of several if zone list
                    if (StageControlledZone(StageControlledZoneNum).CSBchedIndex == 0) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                            "\" not found.");
                        ErrorsFound = true;
                    }
                }

                StageControlledZone(StageControlledZoneNum).CoolThroRange = rNumericArgs(8);
                if (rNumericArgs(8) < 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" negative value is found at {}=\"{:.1R}\"",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cNumericFieldNames(8),
                                           rNumericArgs(8)));
                    ShowContinueError(state, ".. The minumum value is 0.");
                    ErrorsFound = true;
                }

                if (StageControlledZone(StageControlledZoneNum).NumOfCoolStages > 0) {
                    StageControlledZone(StageControlledZoneNum).CoolTOffset.allocate(StageControlledZone(StageControlledZoneNum).NumOfCoolStages);
                    for (i = 1; i <= StageControlledZone(StageControlledZoneNum).NumOfCoolStages; ++i) {
                        StageControlledZone(StageControlledZoneNum).CoolTOffset(i) = rNumericArgs(8 + i);
                        if (rNumericArgs(8 + i) < 0.0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\" negative value is found at {}=\"{:.1R}\"",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(8 + i),
                                                   rNumericArgs(8 + i)));
                            ShowContinueError(state, ".. The minimum value is 0.");
                            ErrorsFound = true;
                        }
                        if (lNumericFieldBlanks(8 + i)) {
                            ShowSevereError(state,
                                            cCurrentModuleObject + " object =" + cAlphaArgs(1) + ". The input of " + cNumericFieldNames(8 + i) +
                                                " is required, but a blank is found.");
                            ErrorsFound = true;
                        }
                        if (i > 1) {
                            if (rNumericArgs(8 + i) <= rNumericArgs(7 + i)) {
                                ShowSevereError(state,
                                                format("{}=\"{}\" The value at {}=\"{:.1R}\" has to be greater than ",
                                                       cCurrentModuleObject,
                                                       cAlphaArgs(1),
                                                       cNumericFieldNames(8 + i),
                                                       rNumericArgs(8 + i)));
                                ShowContinueError(state, format("{}=\"{:.1R}", cNumericFieldNames(7 + i), rNumericArgs(7 + i)));
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }
        } // loop over NumStageControlledZones
        if ((inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed") == 0) &&
            (inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:UnitarySystem") == 0) &&
            (inputProcessor->getNumObjectsFound(state, "SetpointManager:SingleZone:OneStageCooling") == 0) &&
            (inputProcessor->getNumObjectsFound(state, "SetpointManager:SingleZone:OneStageHeating") == 0)) {
            ShowWarningError(state, cCurrentModuleObject + " is applicable to only selected HVAC objects which are missing from input.");
            ShowContinueError(state, "Model should include one or more of the following objects:  ");
            ShowContinueError(state, "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, AirLoopHVAC:UnitarySystem, ");
            ShowContinueError(
                state, "SetpointManager:SingleZone:OneStageCooling, and/or SetpointManager:SingleZone:OneStageHeating. The simulation continues...");
        }
    } // NumStageControlledZones > 0

    if (ErrorsFound) {
        ShowFatalError(state, "Errors getting Zone Control input data.  Preceding condition(s) cause termination.");
    }
}

void CalculateMonthlyRunningAverageDryBulb(EnergyPlusData &state, Array1D<Real64> &runningAverageASH, Array1D<Real64> &runningAverageCEN)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Xuan Luo
    //       DATE WRITTEN   January 2017
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculate the monthly running average dry bulb temperature;

    // Using/Aliasing

    using OutputReportTabular::GetColumnUsingTabs;
    using OutputReportTabular::StrToReal;

    // SUBROUTINE PARAMETER DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    std::string lineIn;
    std::string lineAvg;
    std::string epwLine;

    Real64 dryBulb;
    Real64 avgDryBulb;

    int readStat;
    int calcEndDay;
    int calcStartDayASH;
    int calcStartDayCEN;

    std::string::size_type pos;
    int ind, i, j;

    Array1D<Real64> adaptiveTemp(state.dataWeatherManager->NumDaysInYear, 0.0);
    Array1D<Real64> dailyDryTemp(state.dataWeatherManager->NumDaysInYear, 0.0);

    readStat = 0;
    if (FileSystem::fileExists(state.files.inputWeatherFilePath.filePath)) {
        // Read hourly dry bulb temperature first
        auto epwFile = state.files.inputWeatherFilePath.open(state, "CalcThermalComfortAdaptive");
        for (i = 1; i <= 9; ++i) { // Headers
            epwFile.readLine();
        }
        for (i = 1; i <= state.dataWeatherManager->NumDaysInYear; ++i) {
            avgDryBulb = 0.0;
            for (j = 1; j <= 24; ++j) {
                epwLine = epwFile.readLine().data;
                for (ind = 1; ind <= 6; ++ind) {
                    pos = index(epwLine, ',');
                    epwLine.erase(0, pos + 1);
                }
                pos = index(epwLine, ',');
                dryBulb = StrToReal(epwLine.substr(0, pos));
                avgDryBulb += (dryBulb / 24.0);
            }
            dailyDryTemp(i) = avgDryBulb;
        }
        epwFile.close();

        // Calculate monthly running average dry bulb temperature.
        int dayOfYear = 0;
        while (dayOfYear < state.dataWeatherManager->NumDaysInYear) {
            dayOfYear++;
            calcEndDay = dayOfYear - 1;
            calcStartDayASH = calcEndDay - 30;
            calcStartDayCEN = calcEndDay - 7;

            if (calcStartDayASH > 0) {
                for (i = calcStartDayASH; i <= calcStartDayASH + 30; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                }
                runningAverageASH(dayOfYear) /= 30;
            } else { // Do special things for wrapping the epw
                calcStartDayASH += state.dataWeatherManager->NumDaysInYear;
                for (i = 1; i <= calcEndDay; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                }
                for (i = calcStartDayASH; i < state.dataWeatherManager->NumDaysInYear; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                }
                runningAverageASH(dayOfYear) /= 30;
            }

            if (calcStartDayCEN > 0) {
                for (i = calcStartDayCEN; i <= calcStartDayCEN + 7; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                }
                runningAverageCEN(dayOfYear) /= 7;
            } else { // Do special things for wrapping the epw
                calcStartDayCEN += state.dataWeatherManager->NumDaysInYear;
                for (i = 1; i <= calcEndDay; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                }
                for (i = calcStartDayCEN; i < state.dataWeatherManager->NumDaysInYear; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                }
                runningAverageCEN(dayOfYear) /= 7;
            }
        }
    } else {
        ShowFatalError(state,
                       "CalcThermalComfortAdaptive: Could not open file " + state.files.inputWeatherFilePath.filePath.string() +
                           " for input (read). (File does not exist)");
    }
}

void CalculateAdaptiveComfortSetPointSchl(EnergyPlusData &state, Array1D<Real64> const &runningAverageASH, Array1D<Real64> const &runningAverageCEN)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Xuan Luo
    //       DATE WRITTEN   January 2017
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the zone operative temperature setpoint using adaptive comfort model.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int constexpr summerDesignDayTypeIndex(9);
    Real64 GrossApproxAvgDryBulbDesignDay(0.0);

    auto &AdapComfortDailySetPointSchedule = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule;
    auto &AdapComfortSetPointSummerDesDay = state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay;

    for (size_t i = 1; i <= state.dataWeatherManager->DesDayInput.size(); i++) {
        // Summer design day
        if (state.dataWeatherManager->DesDayInput(i).DayType == summerDesignDayTypeIndex) {
            GrossApproxAvgDryBulbDesignDay =
                (state.dataWeatherManager->DesDayInput(i).MaxDryBulb +
                 (state.dataWeatherManager->DesDayInput(i).MaxDryBulb - state.dataWeatherManager->DesDayInput(i).DailyDBRange)) /
                2.0;
            if (GrossApproxAvgDryBulbDesignDay > 10 && GrossApproxAvgDryBulbDesignDay < 33.5) {
                AdapComfortSetPointSummerDesDay[0] = 0.31 * GrossApproxAvgDryBulbDesignDay + 17.8;
                AdapComfortSetPointSummerDesDay[1] = 0.31 * GrossApproxAvgDryBulbDesignDay + 20.3;
                AdapComfortSetPointSummerDesDay[2] = 0.31 * GrossApproxAvgDryBulbDesignDay + 21.3;
            }
            if (GrossApproxAvgDryBulbDesignDay > 10 && GrossApproxAvgDryBulbDesignDay < 30) {
                AdapComfortSetPointSummerDesDay[3] = 0.33 * GrossApproxAvgDryBulbDesignDay + 18.8;
                AdapComfortSetPointSummerDesDay[4] = 0.33 * GrossApproxAvgDryBulbDesignDay + 20.8;
                ;
                AdapComfortSetPointSummerDesDay[5] = 0.33 * GrossApproxAvgDryBulbDesignDay + 21.8;
                ;
                AdapComfortSetPointSummerDesDay[6] = 0.33 * GrossApproxAvgDryBulbDesignDay + 22.8;
                ;
            }
        }
    }

    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central.allocate(state.dataWeatherManager->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90.allocate(state.dataWeatherManager->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80.allocate(state.dataWeatherManager->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central.allocate(state.dataWeatherManager->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I.allocate(state.dataWeatherManager->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II.allocate(state.dataWeatherManager->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III.allocate(state.dataWeatherManager->NumDaysInYear);

    // Calculate the set points based on different models, set flag as -1 when running average temperature is not in the range.
    for (int day = 1; day <= state.dataWeatherManager->NumDaysInYear; day++) {
        if (runningAverageASH(day) > 10 && runningAverageASH(day) < 33.5) {
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(day) = 0.31 * runningAverageASH(day) + 17.8;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(day) = 0.31 * runningAverageASH(day) + 20.3;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(day) = 0.31 * runningAverageASH(day) + 21.3;
        } else {
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(day) = -1;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(day) = -1;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(day) = -1;
        }
        if (runningAverageCEN(day) > 10 && runningAverageCEN(day) < 30) {
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(day) = 0.33 * runningAverageCEN(day) + 18.8;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(day) = 0.33 * runningAverageCEN(day) + 20.8;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(day) = 0.33 * runningAverageCEN(day) + 21.8;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(day) = 0.33 * runningAverageCEN(day) + 22.8;
        } else {
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(day) = -1;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(day) = -1;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(day) = -1;
            AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(day) = -1;
        }
    }
    AdapComfortDailySetPointSchedule.initialized = true;
}

void InitZoneAirSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russell Taylor
    //       DATE WRITTEN   September 1998
    //       MODIFIED       November 2004, M. J. Witte additional report variables
    //       MODIFIED       L.Gu, May 2006
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the data for the zone air setpoints.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InitZoneAirSetpoints: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int ZoneNum;
    bool FirstSurfFlag;
    int TRefFlag; // Flag for Reference Temperature process in Zones
    int SurfNum;

    auto &Zone = state.dataHeatBal->Zone;
    auto &ZoneList = state.dataHeatBal->ZoneList;
    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &TempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint;
    auto &TempControlType = state.dataHeatBalFanSys->TempControlType;
    auto &TempControlTypeRpt = state.dataHeatBalFanSys->TempControlTypeRpt;
    auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone;
    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;
    auto &NumOfZones = state.dataGlobal->NumOfZones;
    auto &ZoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand;
    auto &ZoneSysMoistureDemand = state.dataZoneEnergyDemand->ZoneSysMoistureDemand;

    if (state.dataZoneTempPredictorCorrector->InitZoneAirSetPointsOneTimeFlag) {
        TempZoneThermostatSetPoint.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint.dimension(NumOfZones, 0.0);
        ZoneThermostatSetPointHi.dimension(NumOfZones, 0.0);
        ZoneThermostatSetPointLo.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver.dimension(NumOfZones, 0.0);

        state.dataHeatBalFanSys->LoadCorrectionFactor.dimension(NumOfZones, 0.0);
        TempControlType.dimension(NumOfZones, DataHVACGlobals::ThermostatType::Uncontrolled);
        TempControlTypeRpt.dimension(NumOfZones, 0);
        if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
            state.dataHeatBalFanSys->ComfortControlType.dimension(NumOfZones, DataHVACGlobals::ThermostatType::Uncontrolled);
            state.dataHeatBalFanSys->ComfortControlTypeRpt.dimension(NumOfZones, 0);
            state.dataHeatBalFanSys->ZoneComfortControlsFanger.allocate(NumOfZones);
        }
        state.dataZoneTempPredictorCorrector->ZoneSetPointLast.dimension(NumOfZones, 0.0);
        state.dataZoneEnergyDemand->Setback.dimension(NumOfZones, false);
        state.dataZoneEnergyDemand->DeadBandOrSetback.dimension(NumOfZones, false);
        state.dataZoneEnergyDemand->CurDeadBandOrSetback.dimension(NumOfZones, false);
        state.dataHeatBal->ZoneSNLoadHeatEnergy.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneSNLoadCoolEnergy.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneSNLoadHeatRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneSNLoadCoolRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneSNLoadPredictedRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneSNLoadPredictedHSPRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneSNLoadPredictedCSPRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneMoisturePredictedRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneMoisturePredictedHumSPRate.dimension(NumOfZones, 0.0);
        state.dataHeatBal->ZoneMoisturePredictedDehumSPRate.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus3.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus4.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->DSWZoneTimeMinus1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->DSWZoneTimeMinus2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->DSWZoneTimeMinus3.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->DSWZoneTimeMinus4.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneAirHumRatTemp.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus1Temp.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus2Temp.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinus3Temp.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->WZoneTimeMinusP.dimension(NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->TempIndZnLd.dimension(NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->TempDepZnLd.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->NonAirSystemResponse.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->SysDepZoneLoads.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->SysDepZoneLoadsLagged.dimension(NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->ZoneAirRelHum.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneWMX.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneWM2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneT1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneW1.dimension(NumOfZones, 0.0);

        state.dataHeatBal->ZoneListSNLoadHeatEnergy.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
        state.dataHeatBal->ZoneListSNLoadCoolEnergy.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
        state.dataHeatBal->ZoneListSNLoadHeatRate.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
        state.dataHeatBal->ZoneListSNLoadCoolRate.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);

        state.dataHeatBal->ZoneGroupSNLoadHeatEnergy.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBal->ZoneGroupSNLoadCoolEnergy.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBal->ZoneGroupSNLoadHeatRate.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBal->ZoneGroupSNLoadCoolRate.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBalFanSys->AIRRAT.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZTM1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZTM2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZTM3.dimension(NumOfZones, 0.0);

        // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredZT1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredZT2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredZT3.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat3.dimension(NumOfZones, 0.0);

        // Allocate Derived Types
        ZoneSysEnergyDemand.allocate(NumOfZones);
        ZoneSysMoistureDemand.allocate(NumOfZones);

        for (Loop = 1; Loop <= NumOfZones; ++Loop) {
            FirstSurfFlag = true;
            for (SurfNum = Zone(Loop).HTSurfaceFirst; SurfNum <= Zone(Loop).HTSurfaceLast; ++SurfNum) {
                if (FirstSurfFlag) {
                    TRefFlag = state.dataSurface->SurfTAirRef(SurfNum);
                    FirstSurfFlag = false;
                }
                // for each particular zone, the reference air temperature(s) should be the same
                // (either mean air, bulk air, or supply air temp).
                if (state.dataSurface->SurfTAirRef(SurfNum) != TRefFlag) {
                    ShowWarningError(state, "Different reference air temperatures for difference surfaces encountered in zone " + Zone(Loop).Name);
                }
            }
        }

        // CurrentModuleObject='Zone'
        for (Loop = 1; Loop <= NumOfZones; ++Loop) {
            SetupOutputVariable(state,
                                "Zone Air System Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSNLoadHeatEnergy(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                Zone(Loop).Name,
                                _,
                                "ENERGYTRANSFER",
                                "Heating",
                                _,
                                "Building",
                                Zone(Loop).Name,
                                Zone(Loop).Multiplier,
                                Zone(Loop).ListMultiplier);
            SetupOutputVariable(state,
                                "Zone Air System Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSNLoadCoolEnergy(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                Zone(Loop).Name,
                                _,
                                "ENERGYTRANSFER",
                                "Cooling",
                                _,
                                "Building",
                                Zone(Loop).Name,
                                Zone(Loop).Multiplier,
                                Zone(Loop).ListMultiplier);
            SetupOutputVariable(state,
                                "Zone Air System Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSNLoadHeatRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Air System Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSNLoadCoolRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalFanSys->ZT(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Thermostat Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalFanSys->TempTstatAir(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Air Humidity Ratio",
                                OutputProcessor::Unit::None,
                                state.dataHeatBalFanSys->ZoneAirHumRat(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Air Relative Humidity",
                                OutputProcessor::Unit::Perc,
                                state.dataZoneTempPredictorCorrector->ZoneAirRelHum(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);

            // The following output variables are for the predicted Heating/Cooling load for the zone which can be compared to actual load.
            // There are two sets of data available: one where zone and group multipliers have been applied and another where the multipliers have
            // not. First, these report variables are NOT multiplied by zone and group multipliers
            SetupOutputVariable(state,
                                "Zone Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSNLoadPredictedRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSNLoadPredictedHSPRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSNLoadPredictedCSPRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            // Second, these report variable ARE multiplied by zone and group multipliers
            SetupOutputVariable(state,
                                "Zone System Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                ZoneSysEnergyDemand(Loop).TotalOutputRequired,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone System Predicted Sensible Load to Heating Setpoint Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                ZoneSysEnergyDemand(Loop).OutputRequiredToHeatingSP,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone System Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                ZoneSysEnergyDemand(Loop).OutputRequiredToCoolingSP,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);

            // The following output variables are for the predicted moisture load for the zone with humidity controlled specified.
            // There are two sets of data available: one where zone and group multipliers have been applied and another where the multipliers have
            // not. First, these report variables are NOT multiplied by zone and group multipliers
            SetupOutputVariable(state,
                                "Zone Predicted Moisture Load Moisture Transfer Rate",
                                OutputProcessor::Unit::kgWater_s,
                                state.dataHeatBal->ZoneMoisturePredictedRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate",
                                OutputProcessor::Unit::kgWater_s,
                                state.dataHeatBal->ZoneMoisturePredictedHumSPRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate",
                                OutputProcessor::Unit::kgWater_s,
                                state.dataHeatBal->ZoneMoisturePredictedDehumSPRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            // Second, these report variable ARE multiplied by zone and group multipliers
            SetupOutputVariable(state,
                                "Zone System Predicted Moisture Load Moisture Transfer Rate",
                                OutputProcessor::Unit::kgWater_s,
                                ZoneSysMoistureDemand(Loop).TotalOutputRequired,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone System Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate",
                                OutputProcessor::Unit::kgWater_s,
                                ZoneSysMoistureDemand(Loop).OutputRequiredToHumidifyingSP,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone System Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate",
                                OutputProcessor::Unit::kgWater_s,
                                ZoneSysMoistureDemand(Loop).OutputRequiredToDehumidifyingSP,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);

            SetupOutputVariable(state,
                                "Zone Thermostat Control Type",
                                OutputProcessor::Unit::None,
                                TempControlTypeRpt(Loop),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Thermostat Heating Setpoint Temperature",
                                OutputProcessor::Unit::C,
                                ZoneThermostatSetPointLo(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Thermostat Cooling Setpoint Temperature",
                                OutputProcessor::Unit::C,
                                ZoneThermostatSetPointHi(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Adaptive Comfort Operative Temperature Set Point",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(Loop),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Predicted Sensible Load Room Air Correction Factor",
                                OutputProcessor::Unit::None,
                                state.dataHeatBalFanSys->LoadCorrectionFactor(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Zone(Loop).Name);

            if (allocated(state.dataZoneCtrls->StageZoneLogic)) {
                if (state.dataZoneCtrls->StageZoneLogic(Loop)) {
                    SetupOutputVariable(state,
                                        "Zone Thermostat Staged Number",
                                        OutputProcessor::Unit::None,
                                        ZoneSysEnergyDemand(Loop).StageNum,
                                        OutputProcessor::SOVTimeStepType::System,
                                        OutputProcessor::SOVStoreType::Average,
                                        Zone(Loop).Name);
                }
            }

        } // Loop

        // Thermal comfort control output
        if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
            // CurrentModuleObject='ZoneControl:Thermostat:ThermalComfort'
            for (Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
                ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;
                SetupOutputVariable(state,
                                    "Zone Thermal Comfort Control Type",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBalFanSys->ComfortControlTypeRpt(ZoneNum),
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    Zone(ZoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Comfort Control Fanger Low Setpoint PMV",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ZoneNum).LowPMV,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    Zone(ZoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Comfort Control Fanger High Setpoint PMV",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ZoneNum).HighPMV,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    Zone(ZoneNum).Name);
            }
        }

        // CurrentModuleObject='ZoneList'
        for (Loop = 1; Loop <= state.dataHeatBal->NumOfZoneLists; ++Loop) {
            SetupOutputVariable(state,
                                "Zone List Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneListSNLoadHeatEnergy(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                ZoneList(Loop).Name);
            SetupOutputVariable(state,
                                "Zone List Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneListSNLoadCoolEnergy(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                ZoneList(Loop).Name);
            SetupOutputVariable(state,
                                "Zone List Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneListSNLoadHeatRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                ZoneList(Loop).Name);
            SetupOutputVariable(state,
                                "Zone List Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneListSNLoadCoolRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                ZoneList(Loop).Name);
        } // Loop

        // CurrentModuleObject='ZoneGroup'
        for (Loop = 1; Loop <= state.dataHeatBal->NumOfZoneGroups; ++Loop) {
            SetupOutputVariable(state,
                                "Zone Group Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGroupSNLoadHeatEnergy(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGroup(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Group Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGroupSNLoadCoolEnergy(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGroup(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Group Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGroupSNLoadHeatRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGroup(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Group Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGroupSNLoadCoolRate(Loop),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGroup(Loop).Name);
        } // Loop

        state.dataZoneTempPredictorCorrector->InitZoneAirSetPointsOneTimeFlag = false;
    }

    // Do the Begin Environment initializations
    if (state.dataZoneTempPredictorCorrector->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
        state.dataHeatBalFanSys->AIRRAT = 0.0;
        state.dataHeatBalFanSys->ZTM1 = 0.0;
        state.dataHeatBalFanSys->ZTM2 = 0.0;
        state.dataHeatBalFanSys->ZTM3 = 0.0;
        state.dataHeatBalFanSys->WZoneTimeMinus1 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->WZoneTimeMinus2 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->WZoneTimeMinus3 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->WZoneTimeMinus4 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->WZoneTimeMinusP = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->DSWZoneTimeMinus1 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->DSWZoneTimeMinus2 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->DSWZoneTimeMinus3 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->DSWZoneTimeMinus4 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->WZoneTimeMinus1Temp = 0.0;
        state.dataHeatBalFanSys->WZoneTimeMinus2Temp = 0.0;
        state.dataHeatBalFanSys->WZoneTimeMinus3Temp = 0.0;
        state.dataHeatBalFanSys->ZoneAirHumRatTemp = 0.0;
        TempZoneThermostatSetPoint = 0.0;
        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint = 0.0;
        ZoneThermostatSetPointHi = 0.0;
        ZoneThermostatSetPointLo = 0.0;

        state.dataHeatBalFanSys->LoadCorrectionFactor = 1.0;
        TempControlType = DataHVACGlobals::ThermostatType::Uncontrolled;
        for (auto &e : ZoneSysEnergyDemand) {
            e.RemainingOutputRequired = 0.0;
            e.TotalOutputRequired = 0.0;
        }
        for (auto &e : ZoneSysMoistureDemand) {
            e.RemainingOutputRequired = 0.0;
            e.TotalOutputRequired = 0.0;
        }
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired)) ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired = 0.0;
            if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP))
                ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP = 0.0;
            if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP))
                ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP = 0.0;
            if (allocated(ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired)) ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired = 0.0;
            if (allocated(ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP))
                ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP = 0.0;
            if (allocated(ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP))
                ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP = 0.0;
        }

        state.dataZoneEnergyDemand->DeadBandOrSetback = false;
        state.dataHeatBal->ZoneSNLoadHeatEnergy = 0.0;
        state.dataHeatBal->ZoneSNLoadCoolEnergy = 0.0;
        state.dataHeatBal->ZoneSNLoadHeatRate = 0.0;
        state.dataHeatBal->ZoneSNLoadCoolRate = 0.0;
        state.dataHeatBal->ZoneSNLoadPredictedRate = 0.0;
        state.dataHeatBal->ZoneSNLoadPredictedHSPRate = 0.0;
        state.dataHeatBal->ZoneSNLoadPredictedCSPRate = 0.0;
        state.dataHeatBal->ZoneMoisturePredictedRate = 0.0;
        state.dataHeatBal->ZoneMoisturePredictedHumSPRate = 0.0;
        state.dataHeatBal->ZoneMoisturePredictedDehumSPRate = 0.0;

        state.dataZoneTempPredictorCorrector->TempIndZnLd = 0.0;
        state.dataZoneTempPredictorCorrector->TempDepZnLd = 0.0;
        state.dataHeatBalFanSys->NonAirSystemResponse = 0.0;
        state.dataHeatBalFanSys->SysDepZoneLoads = 0.0;
        state.dataHeatBalFanSys->SysDepZoneLoadsLagged = 0.0;
        state.dataZoneTempPredictorCorrector->ZoneAirRelHum = 0.0;
        for (auto &e : Zone)
            e.NoHeatToReturnAir = false;
        state.dataHeatBalFanSys->ZoneT1 = 0.0;
        state.dataHeatBalFanSys->ZoneW1 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->ZoneWMX = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->ZoneWM2 = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->PreviousMeasuredZT1 = 0.0;     // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredZT2 = 0.0;     // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredZT3 = 0.0;     // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredHumRat1 = 0.0; // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredHumRat2 = 0.0; // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredHumRat3 = 0.0; // Hybrid modeling

        state.dataZoneTempPredictorCorrector->MyEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataZoneTempPredictorCorrector->MyEnvrnFlag = true;
    }

    // Do the Begin Day initializations
    if (state.dataZoneTempPredictorCorrector->MyDayFlag && state.dataGlobal->BeginDayFlag) {
        state.dataZoneTempPredictorCorrector->MyDayFlag = false;
    }

    if (!state.dataGlobal->BeginDayFlag) {
        state.dataZoneTempPredictorCorrector->MyDayFlag = true;
    }

    for (Loop = 1; Loop <= state.dataZoneCtrls->NumTempControlledZones; ++Loop) {
        if (state.dataZoneEquip->ZoneEquipInputsFilled && !state.dataZoneTempPredictorCorrector->ControlledZonesChecked) {
            if (!VerifyControlledZoneForThermostat(state, TempControlledZone(Loop).ZoneName)) {
                ShowSevereError(state,
                                format("{}Zone=\"{}\" has specified a Thermostatic control but is not a controlled zone.",
                                       RoutineName,
                                       TempControlledZone(Loop).ZoneName));
                ShowContinueError(state, "...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                state.dataZoneTempPredictorCorrector->ErrorsFound = true;
            }
        }

        if (TempControlledZone(Loop).ManageDemand) {
            ZoneNum = TempControlledZone(Loop).ActualZoneNum;

            switch (TempControlType(ZoneNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                if (TempZoneThermostatSetPoint(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit) {
                    TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).HeatingResetLimit;
                    ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                if (TempZoneThermostatSetPoint(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit) {
                    TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).CoolingResetLimit;
                    ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                if ((TempZoneThermostatSetPoint(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit) ||
                    (TempZoneThermostatSetPoint(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit)) {

                    TempControlType(ZoneNum) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;
                    TempControlTypeRpt(ZoneNum) = static_cast<int>(TempControlType(ZoneNum));
                    ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);

                    if (ZoneThermostatSetPointLo(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit)
                        ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).HeatingResetLimit;
                    if (ZoneThermostatSetPointHi(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit)
                        ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).CoolingResetLimit;
                }
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                if (ZoneThermostatSetPointLo(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit)
                    ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).HeatingResetLimit;
                if (ZoneThermostatSetPointHi(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit)
                    ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).CoolingResetLimit;
                break;
            default:
                break;
            }
        }
    }

    for (Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
        if (state.dataZoneEquip->ZoneEquipInputsFilled && !state.dataZoneTempPredictorCorrector->ControlledZonesChecked) {
            if (!VerifyControlledZoneForThermostat(state, ComfortControlledZone(Loop).ZoneName)) {
                ShowSevereError(state,
                                format("{}Zone=\"{}\" has specified a Comfort control but is not a controlled zone.",
                                       RoutineName,
                                       ComfortControlledZone(Loop).ZoneName));
                ShowContinueError(state, "...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                state.dataZoneTempPredictorCorrector->ErrorsFound = true;
            }
        }
        if (ComfortControlledZone(Loop).ManageDemand) {
            ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;

            switch (state.dataHeatBalFanSys->ComfortControlType(ZoneNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                if (TempZoneThermostatSetPoint(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit) {
                    TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).HeatingResetLimit;
                    ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    TempControlType(ZoneNum) = DataHVACGlobals::ThermostatType::SingleHeating;
                    TempControlTypeRpt(ZoneNum) = static_cast<int>(TempControlType(ZoneNum));
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                if (TempZoneThermostatSetPoint(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit) {
                    TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).CoolingResetLimit;
                    ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    TempControlType(ZoneNum) = DataHVACGlobals::ThermostatType::SingleCooling;
                    TempControlTypeRpt(ZoneNum) = static_cast<int>(TempControlType(ZoneNum));
                }
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                if ((TempZoneThermostatSetPoint(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit) ||
                    (TempZoneThermostatSetPoint(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit)) {

                    TempControlType(ZoneNum) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;
                    TempControlTypeRpt(ZoneNum) = static_cast<int>(TempControlType(ZoneNum));
                    ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);

                    if (ZoneThermostatSetPointLo(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit)
                        ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).HeatingResetLimit;
                    if (ZoneThermostatSetPointHi(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit)
                        ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).CoolingResetLimit;
                }
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                TempControlType(ZoneNum) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;
                TempControlTypeRpt(ZoneNum) = static_cast<int>(TempControlType(ZoneNum));
                if (ZoneThermostatSetPointLo(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit)
                    ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).HeatingResetLimit;
                if (ZoneThermostatSetPointHi(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit)
                    ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).CoolingResetLimit;
                break;
            default:
                break;
            }
        } // Demand manager
    }

    if (state.dataZoneTempPredictorCorrector->ErrorsFound) {
        ShowFatalError(state, "InitZoneAirSetpoints - program terminates due to previous condition.");
    }

    if (state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataZoneTempPredictorCorrector->ControlledZonesChecked = true;
    }
}

void PredictSystemLoads(EnergyPlusData &state,
                        bool const ShortenTimeStepSys,
                        bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                        Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  July 2003 (Peter Graham Ellis)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is responsible for determining
    // how much of each type of energy every zone requires.
    // In effect, this subroutine defines and simulates all
    // the system types and in the case of hybrid systems
    // which use more than one type of energy must determine
    // how to apportion the load. An example of a hybrid system
    // is a water loop heat pump with supplemental air.  In
    // this case, a zone will require water from the loop and
    // cooled or heated air from the air system. A simpler
    // example would be a VAV system with baseboard heaters.

    //  Basic Air System Types
    //  1) Constant Volume Single Duct
    //  2) Variable Volume Single Duct
    //  3) Constant Volume Dual Duct
    //  4) Variable Volume Dual Duct

    // METHODOLOGY EMPLOYED:
    // 0.  Determine if simulation has downstepped and readjust history and revert node results
    // 1.  Determine zone load - this is zone temperature dependent
    // 2.  Determine balance point - the temperature at which the
    //     zone load is balanced by the system output. The way the
    //     balance point is determined will be different depending on
    //     the type of system being simulated.
    // 3.  Calculate zone energy requirements

    using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;
    using RoomAirModelAirflowNetwork::LoadPredictionRoomAirModelAirflowNetwork;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SumIntGain; // Zone sum of convective internal gains
    Real64 SumHA;      // Zone sum of Hc*Area
    Real64 SumHATsurf; // Zone sum of Hc*Area*Tsurf
    Real64 SumHATref;  // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
    Real64 SumMCp;     // Zone sum of MassFlowRate*Cp
    Real64 SumMCpT;    // Zone sum of MassFlowRate*Cp*T
    Real64 SumSysMCp;  // Zone sum of air system MassFlowRate*Cp
    Real64 SumSysMCpT; // Zone sum of air system MassFlowRate*Cp*
    Real64 SumIntGainExceptPeople;
    Real64 TempDepCoef; // Formerly CoefSumha
    Real64 TempIndCoef; // Formerly CoefSumhat
    Real64 AirCap;      // Formerly CoefAirrat
    Real64 TempHistoryTerm;
    int ZoneNum;
    Real64 ZoneT; // Zone temperature at previous time step
    int RelativeZoneNum;
    int ActualZoneNum;
    int I;
    int Itemp;
    Real64 SetpointOffset;
    int RoomAirNode;
    int LoopNode;
    Real64 RAFNFrac;

    SumIntGainExceptPeople = 0.0;

    auto &Zone = state.dataHeatBal->Zone;
    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &TempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint;
    auto &TempControlType = state.dataHeatBalFanSys->TempControlType;
    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;
    auto &RoomAirflowNetworkZoneInfo = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo;
    auto &StageControlledZone = state.dataZoneCtrls->StageControlledZone;
    auto &TempDepZnLd = state.dataZoneTempPredictorCorrector->TempDepZnLd;
    auto &TempIndZnLd = state.dataZoneTempPredictorCorrector->TempIndZnLd;
    auto &ZoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand;
    auto &MAT = state.dataHeatBalFanSys->MAT;
    auto &ZoneT1 = state.dataHeatBalFanSys->ZoneT1;
    auto &Node = state.dataLoopNodes->Node;
    auto &ZoneAirSolutionAlgo = state.dataHeatBal->ZoneAirSolutionAlgo;
    auto &AIRRAT = state.dataHeatBalFanSys->AIRRAT;

    // Staged thermostat setpoint
    if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
        for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneTempPredictorCorrector->NumStageCtrZone; ++RelativeZoneNum) {
            ActualZoneNum = StageControlledZone(RelativeZoneNum).ActualZoneNum;
            ZoneT = MAT(ActualZoneNum);
            if (ShortenTimeStepSys) ZoneT = state.dataHeatBalFanSys->XMPT(ActualZoneNum);
            StageControlledZone(RelativeZoneNum).HeatSetPoint = GetCurrentScheduleValue(state, StageControlledZone(RelativeZoneNum).HSBchedIndex);
            StageControlledZone(RelativeZoneNum).CoolSetPoint = GetCurrentScheduleValue(state, StageControlledZone(RelativeZoneNum).CSBchedIndex);
            if (StageControlledZone(RelativeZoneNum).HeatSetPoint >= StageControlledZone(RelativeZoneNum).CoolSetPoint) {
                ++StageControlledZone(RelativeZoneNum).StageErrCount;
                if (StageControlledZone(RelativeZoneNum).StageErrCount < 2) {
                    ShowWarningError(state,
                                     "ZoneControl:Thermostat:StagedDualSetpoint: The heating setpoint is equal to or above the cooling setpoint in " +
                                         StageControlledZone(RelativeZoneNum).Name);
                    ShowContinueError(state, "The zone heating setpoint is set to the cooling setpoint - 0.1C.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The heating setpoint is still above the cooling setpoint",
                                                   StageControlledZone(RelativeZoneNum).StageErrIndex,
                                                   StageControlledZone(RelativeZoneNum).HeatSetPoint,
                                                   StageControlledZone(RelativeZoneNum).HeatSetPoint);
                }
                StageControlledZone(RelativeZoneNum).HeatSetPoint = StageControlledZone(RelativeZoneNum).CoolSetPoint - 0.1; //???????????
            }
            // Determine either cooling or heating
            if (StageControlledZone(RelativeZoneNum).CoolSetPoint < ZoneT) { // Cooling
                SetpointOffset = ZoneT - StageControlledZone(RelativeZoneNum).CoolSetPoint;
                Itemp = 0;
                for (I = 1; I <= StageControlledZone(RelativeZoneNum).NumOfCoolStages; ++I) {
                    if (SetpointOffset >= StageControlledZone(RelativeZoneNum).CoolTOffset(I)) {
                        Itemp = -I;
                    }
                }
                ZoneSysEnergyDemand(ActualZoneNum).StageNum = Itemp;
                if (SetpointOffset >= 0.5 * StageControlledZone(RelativeZoneNum).CoolThroRange) {
                    ZoneThermostatSetPointHi(ActualZoneNum) =
                        StageControlledZone(RelativeZoneNum).CoolSetPoint - 0.5 * StageControlledZone(RelativeZoneNum).CoolThroRange;
                } else {
                    ZoneThermostatSetPointHi(ActualZoneNum) =
                        StageControlledZone(RelativeZoneNum).CoolSetPoint + 0.5 * StageControlledZone(RelativeZoneNum).CoolThroRange;
                }
                ZoneThermostatSetPointLo(ActualZoneNum) = ZoneThermostatSetPointHi(ActualZoneNum);
            } else if (StageControlledZone(RelativeZoneNum).HeatSetPoint > ZoneT) { // heating
                SetpointOffset = ZoneT - StageControlledZone(RelativeZoneNum).HeatSetPoint;
                Itemp = 0;
                for (I = 1; I <= StageControlledZone(RelativeZoneNum).NumOfHeatStages; ++I) {
                    if (std::abs(SetpointOffset) >= std::abs(StageControlledZone(RelativeZoneNum).HeatTOffset(I))) {
                        Itemp = I;
                    }
                }
                ZoneSysEnergyDemand(ActualZoneNum).StageNum = Itemp;
                if (std::abs(SetpointOffset) >= 0.5 * StageControlledZone(RelativeZoneNum).CoolThroRange) {
                    ZoneThermostatSetPointLo(ActualZoneNum) =
                        StageControlledZone(RelativeZoneNum).HeatSetPoint + 0.5 * StageControlledZone(RelativeZoneNum).HeatThroRange;
                } else {
                    ZoneThermostatSetPointLo(ActualZoneNum) =
                        StageControlledZone(RelativeZoneNum).HeatSetPoint - 0.5 * StageControlledZone(RelativeZoneNum).HeatThroRange;
                }
                ZoneThermostatSetPointHi(ActualZoneNum) = ZoneThermostatSetPointLo(ActualZoneNum);
            } else {
                ZoneThermostatSetPointHi(ActualZoneNum) =
                    StageControlledZone(RelativeZoneNum).CoolSetPoint + 0.5 * StageControlledZone(RelativeZoneNum).CoolThroRange;
                ZoneThermostatSetPointLo(ActualZoneNum) =
                    StageControlledZone(RelativeZoneNum).HeatSetPoint - 0.5 * StageControlledZone(RelativeZoneNum).HeatThroRange;
                ZoneSysEnergyDemand(ActualZoneNum).StageNum = 0;
            }
        }
    }

    // Setpoint revision for onoff thermostat
    if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
        Real64 TempTole = 0.02;
        Real64 Tprev;
        for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {
            if (TempControlledZone(RelativeZoneNum).DeltaTCutSet > 0.0) {
                if (ShortenTimeStepSys) {
                    TempControlledZone(RelativeZoneNum).HeatModeLast = TempControlledZone(RelativeZoneNum).HeatModeLastSave;
                    TempControlledZone(RelativeZoneNum).CoolModeLast = TempControlledZone(RelativeZoneNum).CoolModeLastSave;
                } else {
                    TempControlledZone(RelativeZoneNum).HeatModeLastSave = TempControlledZone(RelativeZoneNum).HeatModeLast;
                    TempControlledZone(RelativeZoneNum).CoolModeLastSave = TempControlledZone(RelativeZoneNum).CoolModeLast;
                }
                ZoneNum = TempControlledZone(RelativeZoneNum).ActualZoneNum;

                TempControlledZone(RelativeZoneNum).CoolOffFlag = false;
                TempControlledZone(RelativeZoneNum).HeatOffFlag = false;
                if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
                    Tprev = MAT(ZoneNum);
                    if (ShortenTimeStepSys) Tprev = state.dataHeatBalFanSys->XMPT(ZoneNum);
                } else {
                    Tprev = ZoneT1(ZoneNum);
                }

                switch (TempControlType(ZoneNum)) {
                case DataHVACGlobals::ThermostatType::SingleHeating:
                    TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                    ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                    if (Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempTole) {
                        TempZoneThermostatSetPoint(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                        ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    } else if (Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo &&
                               (Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo +
                                            TempControlledZone(RelativeZoneNum).DeltaTCutSet - TempTole)) {
                        TempZoneThermostatSetPoint(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                        ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    } else {
                        TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                    }
                    if (TempControlledZone(RelativeZoneNum).HeatModeLast && Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo) {
                        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                        ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                        TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                    }
                    break;
                case DataHVACGlobals::ThermostatType::SingleCooling:
                    TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                    ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                    if (Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempTole) {
                        TempZoneThermostatSetPoint(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                        ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    } else if (Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi &&
                               Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi -
                                           TempControlledZone(RelativeZoneNum).DeltaTCutSet + TempTole) {
                        TempZoneThermostatSetPoint(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                        ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                    } else {
                        TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                    }
                    if (TempControlledZone(RelativeZoneNum).CoolModeLast && Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi) {
                        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                        ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                        TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                    }
                    break;
                case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                    ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                    ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                    if (Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempTole) {
                        ZoneThermostatSetPointHi(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                    } else if (Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi &&
                               Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi -
                                           TempControlledZone(RelativeZoneNum).DeltaTCutSet + TempTole) {
                        ZoneThermostatSetPointHi(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                    } else {
                        TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                    }
                    if (TempControlledZone(RelativeZoneNum).CoolModeLast && Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi) {
                        ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                        TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                    }

                    if (Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempTole) {
                        ZoneThermostatSetPointLo(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                    } else if (Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo &&
                               (Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo +
                                            TempControlledZone(RelativeZoneNum).DeltaTCutSet - TempTole)) {
                        ZoneThermostatSetPointLo(ZoneNum) =
                            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                    } else {
                        TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                    }
                    if (TempControlledZone(RelativeZoneNum).HeatModeLast && Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo) {
                        ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                        TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                    }
                    // check setpoint for both and provde an error message
                    if (ZoneThermostatSetPointLo(ZoneNum) >= ZoneThermostatSetPointHi(ZoneNum)) {
                        ShowSevereError(state,
                                        "DualSetPointWithDeadBand: When Temperature Difference Between Cutout And Setpoint is applied, the heating "
                                        "setpoint is greater than the cooling setpoint. ");
                        ShowContinueErrorTimeStamp(state, "occurs in Zone=" + Zone(ZoneNum).Name);
                        ShowContinueError(state, format("Zone Heating ThermostatSetPoint={:.2R}", ZoneThermostatSetPointLo(ZoneNum)));
                        ShowContinueError(state, format("Zone Cooling ThermostatSetPoint={:.2R}", ZoneThermostatSetPointHi(ZoneNum)));
                        ShowFatalError(state, "Program terminates due to above conditions.");
                    }
                    break;
                default:
                    break;
                }
            }
        }
    }

    auto &TimeStepSys(state.dataHVACGlobal->TimeStepSys);

    // Update zone temperatures
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        if (ShortenTimeStepSys) {
            // timestep has just shifted from full zone timestep to a new shorter system timestep
            // throw away last updates in corrector and rewind for resimulating smaller timestep
            if (Zone(ZoneNum).SystemZoneNodeNumber > 0) { // roll back result for zone air node,
                Node(Zone(ZoneNum).SystemZoneNodeNumber).Temp = state.dataHeatBalFanSys->XMAT(ZoneNum);
                state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataHeatBalFanSys->XMAT(ZoneNum);
                Node(Zone(ZoneNum).SystemZoneNodeNumber).HumRat = state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum);
                Node(Zone(ZoneNum).SystemZoneNodeNumber).Enthalpy =
                    PsyHFnTdbW(state.dataHeatBalFanSys->XMAT(ZoneNum), state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum));
            }

            if (state.dataHVACGlobal->NumOfSysTimeSteps !=
                state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time

                //  MAT(ZoneNum),   state.dataHeatBalFanSys->XMAT(ZoneNum),   state.dataHeatBalFanSys->XM2T(ZoneNum),
                //  state.dataHeatBalFanSys->XM3T(ZoneNum),   state.dataHeatBalFanSys->XM4T(ZoneNum), &
                DownInterpolate4HistoryValues(PriorTimeStep,
                                              TimeStepSys,
                                              state.dataHeatBalFanSys->XMAT(ZoneNum),
                                              state.dataHeatBalFanSys->XM2T(ZoneNum),
                                              state.dataHeatBalFanSys->XM3T(ZoneNum),
                                              state.dataHeatBalFanSys->XM4T(ZoneNum),
                                              state.dataHeatBalFanSys->XM4T(ZoneNum),
                                              MAT(ZoneNum),
                                              state.dataHeatBalFanSys->DSXMAT(ZoneNum),
                                              state.dataHeatBalFanSys->DSXM2T(ZoneNum),
                                              state.dataHeatBalFanSys->DSXM3T(ZoneNum),
                                              state.dataHeatBalFanSys->DSXM4T(ZoneNum));
                //     ZoneAirHumRat(ZoneNum),   WZoneTimeMinus1(ZoneNum),   WZoneTimeMinus2(ZoneNum),   &
                //                                 WZoneTimeMinus3(ZoneNum),   WZoneTimeMinus4(ZoneNum), &
                DownInterpolate4HistoryValues(PriorTimeStep,
                                              TimeStepSys,
                                              state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum),
                                              state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus1(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus2(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus3(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus4(ZoneNum));

                if (state.dataRoomAirMod->IsZoneDV(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {

                    //   MATFloor(ZoneNum),   XMATFloor(ZoneNum),    XM2TFloor(ZoneNum),  &
                    //                        XM3TFloor(ZoneNum),    XM4TFloor(ZoneNum) ,   &
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataRoomAirMod->XMATFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM2TFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM3TFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM4TFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM4TFloor(ZoneNum),
                                                  state.dataRoomAirMod->MATFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXMATFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXM2TFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXM3TFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXM4TFloor(ZoneNum));
                    //      MATOC(ZoneNum),   XMATOC(ZoneNum),    XM2TOC(ZoneNum),  &
                    //                        XM3TOC(ZoneNum),    XM4TOC(ZoneNum) ,   &
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataRoomAirMod->XMATOC(ZoneNum),
                                                  state.dataRoomAirMod->XM2TOC(ZoneNum),
                                                  state.dataRoomAirMod->XM3TOC(ZoneNum),
                                                  state.dataRoomAirMod->XM4TOC(ZoneNum),
                                                  state.dataRoomAirMod->XM4TOC(ZoneNum),
                                                  state.dataRoomAirMod->MATOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXMATOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXM2TOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXM3TOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXM4TOC(ZoneNum));
                    //  MATMX(ZoneNum),   XMATMX(ZoneNum),    XM2TMX(ZoneNum),  &
                    //                    XM3TMX(ZoneNum),    XM4TMX(ZoneNum) ,   &
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataRoomAirMod->XMATMX(ZoneNum),
                                                  state.dataRoomAirMod->XM2TMX(ZoneNum),
                                                  state.dataRoomAirMod->XM3TMX(ZoneNum),
                                                  state.dataRoomAirMod->XM4TMX(ZoneNum),
                                                  state.dataRoomAirMod->XM4TMX(ZoneNum),
                                                  state.dataRoomAirMod->MATMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXMATMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXM2TMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXM3TMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXM4TMX(ZoneNum));
                }
                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        auto &ThisRAFNNode(RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode));
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      ThisRAFNNode.AirTemp,
                                                      ThisRAFNNode.AirTempX1,
                                                      ThisRAFNNode.AirTempX2,
                                                      ThisRAFNNode.AirTempX3,
                                                      ThisRAFNNode.AirTempX4,
                                                      ThisRAFNNode.AirTemp,
                                                      ThisRAFNNode.AirTempDSX1,
                                                      ThisRAFNNode.AirTempDSX2,
                                                      ThisRAFNNode.AirTempDSX3,
                                                      ThisRAFNNode.AirTempDSX4);
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      ThisRAFNNode.HumRat,
                                                      ThisRAFNNode.HumRatX1,
                                                      ThisRAFNNode.HumRatX2,
                                                      ThisRAFNNode.HumRatX3,
                                                      ThisRAFNNode.HumRatX4,
                                                      ThisRAFNNode.HumRat,
                                                      ThisRAFNNode.HumRatDSX1,
                                                      ThisRAFNNode.HumRatDSX2,
                                                      ThisRAFNNode.HumRatDSX3,
                                                      ThisRAFNNode.HumRatDSX4);
                    }
                }
            } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                     // do nothing because DS history would have been pushed prior and should be ready
            }
        }
        // now update the variables actually used in the balance equations.
        if (UseZoneTimeStepHistory) {
            state.dataHeatBalFanSys->ZTM1(ZoneNum) = state.dataHeatBalFanSys->XMAT(ZoneNum);
            state.dataHeatBalFanSys->ZTM2(ZoneNum) = state.dataHeatBalFanSys->XM2T(ZoneNum);
            state.dataHeatBalFanSys->ZTM3(ZoneNum) = state.dataHeatBalFanSys->XM3T(ZoneNum);

            state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum);

        } else { // use down-stepped history
            state.dataHeatBalFanSys->ZTM1(ZoneNum) = state.dataHeatBalFanSys->DSXMAT(ZoneNum);
            state.dataHeatBalFanSys->ZTM2(ZoneNum) = state.dataHeatBalFanSys->DSXM2T(ZoneNum);
            state.dataHeatBalFanSys->ZTM3(ZoneNum) = state.dataHeatBalFanSys->DSXM3T(ZoneNum);

            state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus1(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus2(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus3(ZoneNum);
        }

        AIRRAT(ZoneNum) = Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpSens *
                          PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                          PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
        AirCap = AIRRAT(ZoneNum);
        RAFNFrac = 0.0;

        // Calculate the various heat balance sums

        // NOTE: SumSysMCp and SumSysMCpT are not used in the predict step
        CalcZoneSums(state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, false);

        // Sum all convective internal gains except for people: SumIntGainExceptPeople
        if (state.dataHybridModel->FlagHybridModel_PC) {
            SumIntGainExceptPeople = SumAllInternalConvectionGainsExceptPeople(state, ZoneNum);
        }

        TempDepCoef = SumHA + SumMCp;
        TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);
        if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::Mixing) {
            TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                        (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
            TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
            TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
        } else if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
            // UCSD displacement ventilation model - make dynamic term independent of TimeStepSys
            TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                        (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
            TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
            TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
        } else if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
            // UCSD UFAD model - make dynamic term independent of TimeStepSys
            TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                        (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
            TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
            TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
        } else if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
            // RoomAirflowNetworkModel - make dynamic term independent of TimeStepSys
            if (RoomAirflowNetworkZoneInfo(ZoneNum).IsUsed) {
                RoomAirNode = RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
                LoadPredictionRoomAirModelAirflowNetwork(state, ZoneNum, RoomAirNode);
                TempDepCoef =
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp;
                TempIndCoef = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntSensibleGain +
                              RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf -
                              RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref +
                              RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT +
                              RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SysDepZoneLoadsLagged;
                AirCap = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume * Zone(ZoneNum).ZoneVolCapMultpSens *
                         RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir * RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).CpAir /
                         (TimeStepSys * DataGlobalConstants::SecInHour);
                AIRRAT(ZoneNum) = AirCap;
                TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                            (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
                TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
                if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HasHVACAssigned)
                    RAFNFrac = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HVAC(1).SupplyFraction;
            }
        } else { // other imperfectly mixed room models
            TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                        (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
            TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
            TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
        }

        // Exact solution or Euler method
        state.dataHVACGlobal->ShortenTimeStepSysRoomAir = false;
        if (ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
            if (ShortenTimeStepSys && TimeStepSys < state.dataGlobal->TimeStepZone) {
                if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                    ZoneT1(ZoneNum) = state.dataHeatBalFanSys->ZoneTM2(ZoneNum);
                    state.dataHeatBalFanSys->ZoneW1(ZoneNum) = state.dataHeatBalFanSys->ZoneWM2(ZoneNum);
                    if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                        for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 =
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2;
                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 =
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2;
                        }
                    }
                } else {
                    ZoneT1(ZoneNum) = state.dataHeatBalFanSys->ZoneTMX(ZoneNum);
                    state.dataHeatBalFanSys->ZoneW1(ZoneNum) = state.dataHeatBalFanSys->ZoneWMX(ZoneNum);
                    if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                        for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 =
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 =
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                        }
                    }
                }
                state.dataHVACGlobal->ShortenTimeStepSysRoomAir = true;
            } else {
                ZoneT1(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
                state.dataHeatBalFanSys->ZoneW1(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum);
                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                    }
                }
            }
            TempDepZnLd(ZoneNum) = TempDepCoef;
            TempIndZnLd(ZoneNum) = TempIndCoef;
        }

        // Calculate the predicted zone load to be provided by the system with the given desired zone air temperature
        CalcPredictedSystemLoad(state, ZoneNum, RAFNFrac);

        // Calculate the predicted zone load to be provided by the system with the given desired humidity ratio
        CalcPredictedHumidityRatio(state, ZoneNum, RAFNFrac);
    }

    if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
        for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {
            if (TempControlledZone(RelativeZoneNum).DeltaTCutSet > 0.0) {
                ZoneNum = TempControlledZone(RelativeZoneNum).ActualZoneNum;
                if (TempControlledZone(RelativeZoneNum).CoolOffFlag && ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired >= 0.0) {
                    TempControlledZone(RelativeZoneNum).CoolModeLast = true;
                } else {
                    TempControlledZone(RelativeZoneNum).CoolModeLast = false;
                }
                if (TempControlledZone(RelativeZoneNum).HeatOffFlag && ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired <= 0.0) {
                    TempControlledZone(RelativeZoneNum).HeatModeLast = true;
                } else {
                    TempControlledZone(RelativeZoneNum).HeatModeLast = false;
                }
            }
        }
    }
}

void CalcZoneAirTempSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       Aug 2013, Xiufeng Pang (XP) - Added code for updating set points during
    //                      optimum start period
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine sets what the setpoints for each controlled zone should be based on schedules.
    // This is called each time step.

    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleValuesForDay;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RelativeZoneNum;
    int ActualZoneNum;
    int TempControlSchedIndex;
    int SetPointTempSchedIndexHot;
    int SetPointTempSchedIndexCold;
    int SchedNameIndex;
    Array2D<Real64> DaySPValues; // Day room temp setpoint values - for optimum start
    int OccStartTime;            // Occupancy start time - for optimum start
    Real64 DeltaT;               // Temperature difference between cutout and setpoint

    auto &Zone = state.dataHeatBal->Zone;
    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &TempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint;
    auto &TempControlType = state.dataHeatBalFanSys->TempControlType;
    auto &TempControlTypeRpt = state.dataHeatBalFanSys->TempControlTypeRpt;
    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;
    auto &NumOfZones = state.dataGlobal->NumOfZones;

    TempControlType = DataHVACGlobals::ThermostatType::Uncontrolled; // Default

    // Place holder for occupied heating and cooling set points - for optimum start
    if (!allocated(state.dataZoneCtrls->OccRoomTSetPointHeat)) {
        state.dataZoneCtrls->OccRoomTSetPointHeat.allocate(NumOfZones);
    }
    if (!allocated(state.dataZoneCtrls->OccRoomTSetPointCool)) {
        state.dataZoneCtrls->OccRoomTSetPointCool.allocate(NumOfZones);
    }
    state.dataZoneCtrls->OccRoomTSetPointHeat = 0.0;
    state.dataZoneCtrls->OccRoomTSetPointCool = 100.0;
    DeltaT = 0.0;

    for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {

        // What if this zone not controlled???
        ActualZoneNum = TempControlledZone(RelativeZoneNum).ActualZoneNum;
        TempControlSchedIndex = TempControlledZone(RelativeZoneNum).CTSchedIndex;
        TempControlType(ActualZoneNum) = static_cast<DataHVACGlobals::ThermostatType>(GetCurrentScheduleValue(state, TempControlSchedIndex));
        TempControlTypeRpt(ActualZoneNum) = static_cast<int>(TempControlType(ActualZoneNum));
        // Error detection for these values is done in the Get routine

        switch (TempControlType(ActualZoneNum)) {
        case DataHVACGlobals::ThermostatType::Uncontrolled:
            break;
        case DataHVACGlobals::ThermostatType::SingleHeating:
            SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatSetPoint;
            TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(state, SchedNameIndex);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = TempZoneThermostatSetPoint(ActualZoneNum);

            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
            ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            break;
        case DataHVACGlobals::ThermostatType::SingleCooling:
            SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleCoolSetPoint;
            TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(state, SchedNameIndex);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = TempZoneThermostatSetPoint(ActualZoneNum);

            // Added Jan 17 (X. Luo)
            // Adjust operative temperature based on adaptive comfort model
            if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
                state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            }

            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
            ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);

            AdjustCoolingSetPointforTempAndHumidityControl(state, RelativeZoneNum, ActualZoneNum);
            break;
        case DataHVACGlobals::ThermostatType::SingleHeatCool:

            SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatCoolSetPoint;

            TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(state, SchedNameIndex);

            // Added Jan 17 (X. Luo)
            // Adjust operative temperature based on adaptive comfort model
            if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
                state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            }

            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));

            ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);

            // Change the room set point to occupied set point during optimum start period--------------

            if (allocated(state.dataHVACGlobal->OptStartData.OptStartFlag)) {
                if (!allocated(DaySPValues)) {
                    DaySPValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
                }
                if (state.dataHVACGlobal->OptStartData.ActualZoneNum(ActualZoneNum) == ActualZoneNum) {
                    GetScheduleValuesForDay(state, SetPointTempSchedIndexCold, DaySPValues);
                    OccStartTime = CEILING(state.dataHVACGlobal->OptStartData.OccStartTime(ActualZoneNum)) + 1;
                    TempZoneThermostatSetPoint(ActualZoneNum) = DaySPValues(1, OccStartTime);
                }

                if (state.dataHVACGlobal->OptStartData.OptStartFlag(ActualZoneNum)) {
                    ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                }
            }
            //--------------------------------------------------------------------------------------------
            break;
        case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
            SetPointTempSchedIndexHot = TempControlledZone(RelativeZoneNum).SchIndx_DualSetPointWDeadBandHeat;
            SetPointTempSchedIndexCold = TempControlledZone(RelativeZoneNum).SchIndx_DualSetPointWDeadBandCool;

            ZoneThermostatSetPointHi(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndexCold);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = ZoneThermostatSetPointHi(ActualZoneNum);

            // Added Jan 17 (X. Luo)
            // Adjust operative temperature based on adaptive comfort model
            if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, ZoneThermostatSetPointHi(ActualZoneNum));
                state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = ZoneThermostatSetPointHi(ActualZoneNum);
            }

            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointHi(ActualZoneNum));

            ZoneThermostatSetPointLo(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndexHot);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = ZoneThermostatSetPointLo(ActualZoneNum);
            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointLo(ActualZoneNum));

            // Change the room set point to occupied set point during optimum start period--------------

            if (allocated(state.dataHVACGlobal->OptStartData.OptStartFlag)) {
                if (!allocated(DaySPValues)) {
                    DaySPValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
                }
                if (state.dataHVACGlobal->OptStartData.ActualZoneNum(ActualZoneNum) == ActualZoneNum) {
                    GetScheduleValuesForDay(state, SetPointTempSchedIndexCold, DaySPValues);
                    OccStartTime = CEILING(state.dataHVACGlobal->OptStartData.OccStartTime(ActualZoneNum)) + 1;
                    state.dataZoneCtrls->OccRoomTSetPointCool(ActualZoneNum) = DaySPValues(1, OccStartTime);
                    GetScheduleValuesForDay(state, SetPointTempSchedIndexHot, DaySPValues);
                    state.dataZoneCtrls->OccRoomTSetPointHeat(ActualZoneNum) = DaySPValues(1, OccStartTime);
                }

                if (state.dataHVACGlobal->OptStartData.OptStartFlag(ActualZoneNum)) {
                    ZoneThermostatSetPointHi(ActualZoneNum) = state.dataZoneCtrls->OccRoomTSetPointCool(ActualZoneNum);
                    ZoneThermostatSetPointLo(ActualZoneNum) = state.dataZoneCtrls->OccRoomTSetPointHeat(ActualZoneNum);
                }
            }
            //--------------------------------------------------------------------------------------------

            AdjustCoolingSetPointforTempAndHumidityControl(state, RelativeZoneNum, ActualZoneNum);
            break;
        default:
            ShowSevereError(state,
                            format("CalcZoneAirTempSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                   Zone(ActualZoneNum).Name,
                                   TempControlType(ActualZoneNum),
                                   TempControlledZone(RelativeZoneNum).ControlTypeSchedName));

            break;
        }

        // Apply offset for faulty therostats
        if ((state.dataFaultsMgr->NumFaultyThermostat > 0) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            //  loop through the FaultsThermostatOffset objects to find the one for the zone
            for (int iFault = 1; iFault <= state.dataFaultsMgr->NumFaultyThermostat; ++iFault) {

                if (UtilityRoutines::SameString(TempControlledZone(RelativeZoneNum).Name,
                                                state.dataFaultsMgr->FaultsThermostatOffset(iFault).FaultyThermostatName)) {

                    // Check fault availability schedules
                    if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsThermostatOffset(iFault).AvaiSchedPtr) > 0.0) {

                        // Check fault severity schedules to update the reference thermostat offset
                        double rSchVal = 1.0;
                        double offsetUpdated;
                        if (state.dataFaultsMgr->FaultsThermostatOffset(iFault).SeveritySchedPtr >= 0) {
                            rSchVal = GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsThermostatOffset(iFault).SeveritySchedPtr);
                        }
                        offsetUpdated = rSchVal * state.dataFaultsMgr->FaultsThermostatOffset(iFault).Offset;

                        // Positive offset means the sensor reading is higher than the actual value
                        TempZoneThermostatSetPoint(ActualZoneNum) -= offsetUpdated;
                        ZoneThermostatSetPointLo(ActualZoneNum) -= offsetUpdated;
                        ZoneThermostatSetPointHi(ActualZoneNum) -= offsetUpdated;
                    }

                    // Stop searching the FaultsThermostatOffset object for the zone
                    break;
                }
            }
        }
    }

    if (state.dataZoneCtrls->NumComfortControlledZones > 0) CalcZoneAirComfortSetPoints(state);
    OverrideAirSetPointsforEMSCntrl(state);
}

void CalcPredictedSystemLoad(EnergyPlusData &state, int const ZoneNum, Real64 RAFNFrac)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the predicted system load for a time step.

    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 LoadToHeatingSetPoint;
    Real64 LoadToCoolingSetPoint;
    Real64 ZoneSetPoint;

    auto &Zone = state.dataHeatBal->Zone;
    auto &TempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint;
    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;
    auto &TempDepZnLd = state.dataZoneTempPredictorCorrector->TempDepZnLd;
    auto &TempIndZnLd = state.dataZoneTempPredictorCorrector->TempIndZnLd;
    auto &ZoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand;
    auto &ZoneT1 = state.dataHeatBalFanSys->ZoneT1;
    auto &Node = state.dataLoopNodes->Node;
    auto &ZoneAirSolutionAlgo = state.dataHeatBal->ZoneAirSolutionAlgo;
    auto &AIRRAT = state.dataHeatBalFanSys->AIRRAT;

    state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = false;
    ZoneSetPoint = 0.0;
    LoadToHeatingSetPoint = 0.0;
    LoadToCoolingSetPoint = 0.0;

    switch (state.dataHeatBalFanSys->TempControlType(ZoneNum)) {
    case DataHVACGlobals::ThermostatType::Uncontrolled:
        // Uncontrolled Zone
        LoadToHeatingSetPoint = 0.0;
        LoadToCoolingSetPoint = 0.0;
        ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
        break;
    case DataHVACGlobals::ThermostatType::SingleHeating:
        if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum));
            // Exact solution
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (TempDepZnLd(ZoneNum) == 0.0) { // B=0
                LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                LoadToHeatingSetPoint =
                    TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                    TempIndZnLd(ZoneNum);
            }
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                    TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum)) - TempIndZnLd(ZoneNum);
        }
        if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
        ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
        ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum);
        LoadToCoolingSetPoint = LoadToHeatingSetPoint;
        // for consistency with the other cases, use LE instead of LT and don't subtract 1.0 Watt as a way of pushing the zero load
        // case over the threshold
        if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) <= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;

        break;
    case DataHVACGlobals::ThermostatType::SingleCooling:
        if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum);
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (TempDepZnLd(ZoneNum) == 0.0) { // B=0
                LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                LoadToCoolingSetPoint =
                    TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                    TempIndZnLd(ZoneNum);
            }
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                    TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum);
        }
        if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
        if (Zone(ZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum) * Zone(ZoneNum).AdjustedReturnTempByITE - TempIndZnLd(ZoneNum);
        }
        ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
        ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum);
        LoadToHeatingSetPoint = LoadToCoolingSetPoint;
        // for consistency with the other cases, use GE instead of GT and don't add 1.0 Watt as a way of pushing the zero load
        // case over the threshold
        if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) >= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
        break;
    case DataHVACGlobals::ThermostatType::SingleHeatCool:
        if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum)) - TempIndZnLd(ZoneNum));
            LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum)) - TempIndZnLd(ZoneNum));
            // Exact solution
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (TempDepZnLd(ZoneNum) == 0.0) { // B=0
                LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
                LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                LoadToHeatingSetPoint =
                    TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                    TempIndZnLd(ZoneNum);
                LoadToCoolingSetPoint =
                    TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                    TempIndZnLd(ZoneNum);
            }
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                    TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum);
            LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                    TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum);
        }
        ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum);
        if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
        if (RAFNFrac > 0.0) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

        if (Zone(ZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum) * Zone(ZoneNum).AdjustedReturnTempByITE - TempIndZnLd(ZoneNum);
        }

        // Note that LoadToHeatingSetPoint is generally not equal to LoadToCoolingSetPoint
        // when the heating and cooling set-points are equal if the zone is unmixed,
        // e.g. displacement ventilation or UFAD, since the stratification is generally not the same in heating and cooling modes

        // Possible combinations:
        // 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
        // 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
        //                                                                 as a poor choice of set-points
        // 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
        // 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation ! includes zero load cases
        // First trap bad set-points
        if (LoadToHeatingSetPoint > LoadToCoolingSetPoint) {
            ShowSevereError(
                state,
                "DataHVACGlobals::ThermostatType::SingleHeatCool: Effective heating set-point higher than effective cooling set-point - use "
                "DualSetPointWithDeadBand if using unmixed air model");
            ShowContinueErrorTimeStamp(state, "occurs in Zone=" + Zone(ZoneNum).Name);
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", TempDepZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", TempIndZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", TempZoneThermostatSetPoint(ZoneNum)));
            ShowFatalError(state, "Program terminates due to above conditions.");
        }

        if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
            ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
        } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
            ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
        } else if (LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
            ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
            if (Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                ZoneSetPoint = Node(Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                ZoneSetPoint = max(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                ZoneSetPoint = min(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
            }
            state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
        } else { // this should never occur!
            ShowSevereError(state,
                            "SingleHeatCoolSetPoint: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
            ShowContinueErrorTimeStamp(state, "occurs in Zone=" + Zone(ZoneNum).Name);
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", TempDepZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", TempIndZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", TempZoneThermostatSetPoint(ZoneNum)));
            ShowFatalError(state, "Program terminates due to above conditions.");
        }
        break;
    case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
        if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum)) - TempIndZnLd(ZoneNum));
            LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum)) - TempIndZnLd(ZoneNum));
            // Exact solution
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (TempDepZnLd(ZoneNum) == 0.0) { // B=0
                LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
                LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                LoadToHeatingSetPoint =
                    TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                    TempIndZnLd(ZoneNum);
                LoadToCoolingSetPoint =
                    TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                    TempIndZnLd(ZoneNum);
            }
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) +
                                    TempDepZnLd(ZoneNum) * ZoneThermostatSetPointLo(ZoneNum) - TempIndZnLd(ZoneNum);
            LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) +
                                    TempDepZnLd(ZoneNum) * ZoneThermostatSetPointHi(ZoneNum) - TempIndZnLd(ZoneNum);
        }
        if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
        if (RAFNFrac > 0.0) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

        if (Zone(ZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum) * Zone(ZoneNum).AdjustedReturnTempByITE - TempIndZnLd(ZoneNum);
        }

        // Possible combinations:
        // 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
        // 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
        //                                                                  as a poor choice of set-points
        // 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
        // 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
        // First trap bad set-points
        if (LoadToHeatingSetPoint > LoadToCoolingSetPoint) {
            ShowSevereError(state,
                            "DualSetPointWithDeadBand: Effective heating set-point higher than effective cooling set-point - increase "
                            "deadband if using unmixed air model");
            ShowContinueErrorTimeStamp(state, "occurs in Zone=" + Zone(ZoneNum).Name);
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", TempDepZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", TempIndZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone Heating ThermostatSetPoint={:.2R}", ZoneThermostatSetPointLo(ZoneNum)));
            ShowContinueError(state, format("Zone Cooling ThermostatSetPoint={:.2R}", ZoneThermostatSetPointHi(ZoneNum)));
            ShowFatalError(state, "Program terminates due to above conditions.");
        }

        if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
            ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
            ZoneSetPoint = ZoneThermostatSetPointLo(ZoneNum);
        } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
            ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
            ZoneSetPoint = ZoneThermostatSetPointHi(ZoneNum);
        } else if (LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
            // this turns out to cause instabilities sometimes? that lead to setpoint errors if predictor is off.
            ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
            if (Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                ZoneSetPoint = Node(Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                ZoneSetPoint = max(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                ZoneSetPoint = min(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
            }
            state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
        } else { // this should never occur!
            ShowSevereError(
                state, "DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
            ShowContinueErrorTimeStamp(state, "occurs in Zone=" + Zone(ZoneNum).Name);
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone Heating Set-point={:.2R}", ZoneThermostatSetPointLo(ZoneNum)));
            ShowContinueError(state, format("Zone Cooling Set-point={:.2R}", ZoneThermostatSetPointHi(ZoneNum)));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", TempDepZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", TempIndZnLd(ZoneNum)));
            ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", TempZoneThermostatSetPoint(ZoneNum)));

            ShowFatalError(state, "Program terminates due to above conditions.");
        }
        break;
    default:
        break;
    }

    // Staged control zone
    if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
        if (state.dataZoneCtrls->StageZoneLogic(ZoneNum)) {
            if (ZoneSysEnergyDemand(ZoneNum).StageNum == 0) { // No load
                LoadToHeatingSetPoint = 0.0;
                LoadToCoolingSetPoint = 0.0;
                ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
                if (Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                    ZoneSetPoint = Node(Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                    ZoneSetPoint = max(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                    ZoneSetPoint = min(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
                }
                state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
            } else if (ZoneSysEnergyDemand(ZoneNum).StageNum < 0) { // Cooling load
                if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
                    LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum)) - TempIndZnLd(ZoneNum));
                } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
                    if (TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                        LoadToCoolingSetPoint =
                            TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            TempIndZnLd(ZoneNum);
                    }
                } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
                    LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) +
                                            TempDepZnLd(ZoneNum) * ZoneThermostatSetPointHi(ZoneNum) - TempIndZnLd(ZoneNum);
                }
                ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                ZoneSetPoint = ZoneThermostatSetPointHi(ZoneNum);
                LoadToHeatingSetPoint = LoadToCoolingSetPoint;
                if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) >= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
            } else { // Heating load
                if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
                    LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * ZoneThermostatSetPointLo(ZoneNum) - TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
                    if (TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            TempIndZnLd(ZoneNum);
                    }
                } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
                    LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) +
                                            TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum)) - TempIndZnLd(ZoneNum);
                }
                ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                ZoneSetPoint = ZoneThermostatSetPointLo(ZoneNum);
                LoadToCoolingSetPoint = LoadToHeatingSetPoint;
                if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) <= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
            }
        }
    }

    // If the ZoneNodeNum has been set for a Controlled Zone, then the zone setpoint is placed on the node.
    if (Zone(ZoneNum).SystemZoneNodeNumber > 0) {
        Node(Zone(ZoneNum).SystemZoneNodeNumber).TempSetPoint = ZoneSetPoint;
    }

    if (ZoneSetPoint > state.dataZoneTempPredictorCorrector->ZoneSetPointLast(ZoneNum)) {
        state.dataZoneEnergyDemand->Setback(ZoneNum) = true;
    } else {
        state.dataZoneEnergyDemand->Setback(ZoneNum) = false;
    }

    state.dataZoneTempPredictorCorrector->ZoneSetPointLast(ZoneNum) = ZoneSetPoint;
    TempZoneThermostatSetPoint(ZoneNum) = ZoneSetPoint; // needed to fix Issue # 5048
    state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum);

    // Apply the Zone Multiplier and Load Correction factor as needed
    ReportSensibleLoadsZoneMultiplier(ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired,
                                      ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP,
                                      ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP,
                                      state.dataHeatBal->ZoneSNLoadPredictedRate(ZoneNum),
                                      state.dataHeatBal->ZoneSNLoadPredictedHSPRate(ZoneNum),
                                      state.dataHeatBal->ZoneSNLoadPredictedCSPRate(ZoneNum),
                                      LoadToHeatingSetPoint,
                                      LoadToCoolingSetPoint,
                                      state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum),
                                      Zone(ZoneNum).Multiplier,
                                      Zone(ZoneNum).ListMultiplier);

    // init each sequenced demand to the full output
    if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired))
        ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired = ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired; // array assignment
    if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP))
        ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP = ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP; // array assignment
    if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP))
        ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP = ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP; // array assignment
}

void ReportSensibleLoadsZoneMultiplier(Real64 &TotalLoad,
                                       Real64 &TotalHeatLoad,
                                       Real64 &TotalCoolLoad,
                                       Real64 &SensLoadSingleZone,
                                       Real64 &SensLoadHeatSingleZone,
                                       Real64 &SensLoadCoolSingleZone,
                                       Real64 const OutputHeatSP,
                                       Real64 const OutputCoolSP,
                                       Real64 const LoadCorrFactor,
                                       Real64 const ZoneMultiplier,
                                       Real64 const ZoneMultiplierList)
{
    SensLoadSingleZone = TotalLoad * LoadCorrFactor;
    SensLoadHeatSingleZone = OutputHeatSP * LoadCorrFactor;
    SensLoadCoolSingleZone = OutputCoolSP * LoadCorrFactor;

    Real64 ZoneMultFac = ZoneMultiplier * ZoneMultiplierList;

    TotalLoad = SensLoadSingleZone * ZoneMultFac;
    TotalHeatLoad = SensLoadHeatSingleZone * ZoneMultFac;
    TotalCoolLoad = SensLoadCoolSingleZone * ZoneMultFac;
}

void CalcPredictedHumidityRatio(EnergyPlusData &state, int const ZoneNum, Real64 RAFNFrac)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   May 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine does the prediction step for humidity control

    // METHODOLOGY EMPLOYED:
    // This solves for the required system moisture required to try and achieve the desired
    // Humidity Ratio in the Zone

    // REFERENCES:
    // Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
    // for BLAST.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcPredictedHumidityRatio");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 LatentGain; // Zone latent load
    Real64 RhoAir;
    Real64 A;
    Real64 B;
    Real64 C;
    Real64 SysTimeStepInSeconds;
    Real64 H2OHtOfVap;
    Real64 RHSetPoint; // Relative Humidity in percent
    Real64 WZoneSetPoint;
    int HumidControlledZoneNum;
    bool ControlledHumidZoneFlag;       // This determines whether this is a humidity controlled zone or not
    Real64 ZoneRHHumidifyingSetPoint;   // Zone humidifying set point (%)
    Real64 ZoneRHDehumidifyingSetPoint; // Zone dehumidifying set point (%)
    Real64 LoadToHumidifySetPoint;      // Moisture load at humidifying set point
    Real64 LoadToDehumidifySetPoint;    // Moisture load at dehumidifying set point
    Real64 ZoneAirRH;                   // Zone air relative humidity
    bool SingleSetPoint;                // This determines whether both setpoint are equal or not
    int RoomAirNode;

    auto &Zone = state.dataHeatBal->Zone;
    auto &HumidityControlZone = state.dataZoneCtrls->HumidityControlZone;
    auto &RoomAirflowNetworkZoneInfo = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo;
    auto &ZT = state.dataHeatBalFanSys->ZT;
    auto &MAT = state.dataHeatBalFanSys->MAT;
    auto &ZoneAirHumRat = state.dataHeatBalFanSys->ZoneAirHumRat;
    auto &ZoneAirSolutionAlgo = state.dataHeatBal->ZoneAirSolutionAlgo;
    auto &ZoneSysMoistureDemand = state.dataZoneEnergyDemand->ZoneSysMoistureDemand;

    LoadToHumidifySetPoint = 0.0;
    LoadToDehumidifySetPoint = 0.0;
    SingleSetPoint = false;
    ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = 0.0;
    ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP = 0.0;
    ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

    // Check to see if this is a "humidity controlled zone"
    ControlledHumidZoneFlag = false;
    // Check all the controlled zones to see if it matches the zone simulated
    for (HumidControlledZoneNum = 1; HumidControlledZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HumidControlledZoneNum) {
        if (HumidityControlZone(HumidControlledZoneNum).ActualZoneNum != ZoneNum) continue;
        ZoneAirRH = PsyRhFnTdbWPb(state, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress) * 100.0;
        ZoneRHHumidifyingSetPoint = GetCurrentScheduleValue(state, HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex);
        ZoneRHDehumidifyingSetPoint = GetCurrentScheduleValue(state, HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex);

        // Apply EMS values to overwrite the humidistat values
        if (HumidityControlZone(HumidControlledZoneNum).EMSOverrideHumidifySetPointOn) {
            ZoneRHHumidifyingSetPoint = HumidityControlZone(HumidControlledZoneNum).EMSOverrideHumidifySetPointValue;
        }
        if (HumidityControlZone(HumidControlledZoneNum).EMSOverrideDehumidifySetPointOn) {
            ZoneRHDehumidifyingSetPoint = HumidityControlZone(HumidControlledZoneNum).EMSOverrideDehumidifySetPointValue;
        }

        // Apply offsets for faulty humidistats
        if ((state.dataFaultsMgr->NumFaultyHumidistat > 0) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {

            //  loop through the FaultsHumidistatOffset objects to find the one for the zone
            for (int iFault = 1; iFault <= state.dataFaultsMgr->NumFaultyHumidistat; ++iFault) {

                if (UtilityRoutines::SameString(HumidityControlZone(HumidControlledZoneNum).ControlName,
                                                state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyHumidistatName)) {

                    if (UtilityRoutines::SameString(state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyHumidistatType,
                                                    "ThermostatOffsetDependent")) {
                        // For Humidistat Offset Type I: ThermostatOffsetDependent

                        bool IsThermostatFound = false;
                        double offsetThermostat = 0.0;
                        double offsetZoneRHHumidifyingSetPoint = 0.0;
                        double offsetZoneRHDehumidifyingSetPoint = 0.0;
                        double faultZoneWHumidifyingSetPoint;
                        double faultZoneWDehumidifyingSetPoint;

                        // Get the offset value of the corresponding thermostat fault object
                        if (state.dataFaultsMgr->NumFaultyThermostat > 0) {

                            //  loop through the FaultsThermostatOffset objects to find the one causes the Humidistat Offset
                            for (int iFaultThermo = 1; iFaultThermo <= state.dataFaultsMgr->NumFaultyThermostat; ++iFaultThermo) {

                                if (UtilityRoutines::SameString(state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyThermostatName,
                                                                state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).Name)) {
                                    IsThermostatFound = true;

                                    // Check fault availability schedules
                                    if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).AvaiSchedPtr) >
                                        0.0) {

                                        // Check fault severity schedules to update the reference thermostat offset
                                        double rSchVal = 1.0;
                                        if (state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).SeveritySchedPtr >= 0) {
                                            rSchVal = GetCurrentScheduleValue(
                                                state, state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).SeveritySchedPtr);
                                        }
                                        offsetThermostat = rSchVal * state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).Offset;
                                    }

                                    // Stop searching the FaultsThermostatOffset object for the Humidistat Offset
                                    break;
                                }
                            }
                        }

                        // The FaultsThermostatOffset specified in the FaultHumidistatOffset is not found
                        if (!IsThermostatFound) {
                            ShowSevereError(state,
                                            "FaultModel:HumidistatOffset = \"" + state.dataFaultsMgr->FaultsHumidistatOffset(iFault).Name +
                                                "\" invalid Reference Humidistat Offset Name = \"" +
                                                state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyThermostatName + "\" not found.");
                            ShowFatalError(state, "Errors getting FaultModel input data.  Preceding condition(s) cause termination.");
                        }

                        if (offsetThermostat != 0.0) {
                            // Calculate the humidistat offset value from the thermostat offset value
                            faultZoneWHumidifyingSetPoint = PsyWFnTdbRhPb(
                                state, (MAT(ZoneNum) + offsetThermostat), (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                            faultZoneWDehumidifyingSetPoint = PsyWFnTdbRhPb(
                                state, (MAT(ZoneNum) + offsetThermostat), (ZoneRHDehumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                            offsetZoneRHHumidifyingSetPoint =
                                ZoneRHHumidifyingSetPoint -
                                PsyRhFnTdbWPb(state, MAT(ZoneNum), faultZoneWHumidifyingSetPoint, state.dataEnvrn->OutBaroPress) * 100.0;
                            offsetZoneRHDehumidifyingSetPoint =
                                ZoneRHDehumidifyingSetPoint -
                                PsyRhFnTdbWPb(state, MAT(ZoneNum), faultZoneWDehumidifyingSetPoint, state.dataEnvrn->OutBaroPress) * 100.0;

                            // Apply the calculated humidistat offset value
                            // Positive offset means the sensor reading is higher than the actual value
                            ZoneRHHumidifyingSetPoint -= offsetZoneRHHumidifyingSetPoint;
                            ZoneRHDehumidifyingSetPoint -= offsetZoneRHDehumidifyingSetPoint;

                            // constrain value to something reasonable
                            ZoneRHHumidifyingSetPoint = min(100.0, max(0.0, ZoneRHHumidifyingSetPoint));
                            ZoneRHDehumidifyingSetPoint = min(100.0, max(0.0, ZoneRHDehumidifyingSetPoint));
                        }

                    } else {
                        // For Humidistat Offset Type II: ThermostatOffsetIndependent

                        // Check fault availability schedules
                        if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsHumidistatOffset(iFault).AvaiSchedPtr) > 0.0) {

                            // Check fault severity schedules to update the reference humidistat offset
                            double rSchVal = 1.0;
                            double offsetUpdated;
                            if (state.dataFaultsMgr->FaultsHumidistatOffset(iFault).SeveritySchedPtr >= 0) {
                                rSchVal = GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsHumidistatOffset(iFault).SeveritySchedPtr);
                            }
                            offsetUpdated = rSchVal * state.dataFaultsMgr->FaultsHumidistatOffset(iFault).Offset;

                            // Positive offset means the sensor reading is higher than the actual value
                            ZoneRHHumidifyingSetPoint -= offsetUpdated;
                            ZoneRHDehumidifyingSetPoint -= offsetUpdated;

                            // constrain value to something reasonable
                            ZoneRHHumidifyingSetPoint = min(100.0, max(0.0, ZoneRHHumidifyingSetPoint));
                            ZoneRHDehumidifyingSetPoint = min(100.0, max(0.0, ZoneRHDehumidifyingSetPoint));
                        }
                    }

                    // Stop searching the FaultsHumidistatOffset object for the zone
                    break;
                }
            }
        }

        // Run-time error check
        if (ZoneRHHumidifyingSetPoint > ZoneRHDehumidifyingSetPoint) {
            //      HumidityControlZone(HumidControlledZoneNum)%ErrorCount = HumidityControlZone(HumidControlledZoneNum)%ErrorCount + 1
            if (HumidityControlZone(HumidControlledZoneNum).ErrorIndex == 0) {
                ShowWarningMessage(state,
                                   "HUMIDISTAT: The humidifying setpoint is above the dehumidifying setpoint in " +
                                       HumidityControlZone(HumidControlledZoneNum).ControlName);
                ShowContinueError(state, "The zone humidifying setpoint is set to the dehumidifying setpoint.");
                ShowContinueErrorTimeStamp(state, "Occurrence info:");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           "The humidifying setpoint is still above the dehumidifying setpoint",
                                           HumidityControlZone(HumidControlledZoneNum).ErrorIndex,
                                           ZoneRHHumidifyingSetPoint,
                                           ZoneRHHumidifyingSetPoint);
            ZoneRHHumidifyingSetPoint = ZoneRHDehumidifyingSetPoint;
        }
        if (ZoneRHHumidifyingSetPoint == ZoneRHDehumidifyingSetPoint) SingleSetPoint = true;
        ControlledHumidZoneFlag = true;

        break;
    } // HumidControlledZoneNum

    if (ControlledHumidZoneFlag) {

        // Calculate hourly humidity ratio from infiltration + humidity added from latent load
        // to determine system added/subtracted moisture.
        LatentGain = state.dataHeatBalFanSys->ZoneLatentGain(ZoneNum) + state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) +
                     state.dataHeatBalFanSys->SumLatentPool(ZoneNum);

        SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

        // Calculate the coefficients for the 3rd Order derivative for final
        // zone humidity ratio.  The A, B, C coefficients are analogous to the heat balance.
        // SumHmARaW and SumHmARa will be used with the Moisture Balance on the building elements and
        // are currently set to zero when the CTF only version is used.

        // if no surface in the zone uses EMPD or HAMT then zero
        bool no_ht_EMPD_or_HAMT(true);
        for (int i = Zone(ZoneNum).HTSurfaceFirst, e = Zone(ZoneNum).HTSurfaceLast; i <= e; ++i) {
            auto const &htAlgo(state.dataSurface->Surface(i).HeatTransferAlgorithm);
            if ((htAlgo == DataSurfaces::HeatTransferModel::EMPD) || (htAlgo == DataSurfaces::HeatTransferModel::HAMT)) {
                no_ht_EMPD_or_HAMT = false;
                break;
            }
        }
        if (no_ht_EMPD_or_HAMT) {
            state.dataHeatBalFanSys->SumHmARaW(ZoneNum) = 0.0;
            state.dataHeatBalFanSys->SumHmARa(ZoneNum) = 0.0;
        }

        // The density of air and latent heat of vaporization are calculated as functions.
        RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName);
        H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneNum), ZT(ZoneNum));

        // Assume that the system will have flow
        if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            B = (LatentGain / H2OHtOfVap) + state.afn->exchangeData(ZoneNum).SumMHrW + state.afn->exchangeData(ZoneNum).SumMMHrW +
                state.dataHeatBalFanSys->SumHmARaW(ZoneNum);
            A = state.afn->exchangeData(ZoneNum).SumMHr + state.afn->exchangeData(ZoneNum).SumMMHr + state.dataHeatBalFanSys->SumHmARa(ZoneNum);
        } else {
            B = (LatentGain / H2OHtOfVap) +
                ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                 state.dataEnvrn->OutHumRat) +
                state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
                state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
            A = state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
                state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
        }
        C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;

        if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
            RoomAirNode = RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
            H2OHtOfVap = PsyHgAirFnWTdb(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat,
                                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp);
            A = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa;
            B = (RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain / H2OHtOfVap) +
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW;
            C = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir * RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume *
                Zone(ZoneNum).ZoneVolCapMultpMoist / (DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys);
        }

        // Use a 3rd Order derivative to predict zone moisture addition or removal and
        // smooth the changes using the zone air capacitance.  Positive values of Moist Load means that
        // this amount of moisture must be added to the zone to reach the setpoint.  Negative values represent
        // the amount of moisture that must be removed by the system.
        // MoistLoadHumidSetPoint = massflow * HumRat = kgDryAir/s * kgWater/kgDryAir = kgWater/s
        WZoneSetPoint = PsyWFnTdbRhPb(state, ZT(ZoneNum), (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress, RoutineName);
        Real64 exp_700_A_C(0.0);
        if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToHumidifySetPoint =
                ((11.0 / 6.0) * C + A) * WZoneSetPoint - (B + C * (3.0 * state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) -
                                                                   (3.0 / 2.0) * state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) +
                                                                   (1.0 / 3.0) * state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum)));
            // Exact solution
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (A == 0.0) { // B=0
                LoadToHumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) - B;
            } else {
                exp_700_A_C = std::exp(min(700.0, -A / C)); // Tuned Save expensive value
                LoadToHumidifySetPoint = A * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum) * exp_700_A_C) / (1.0 - exp_700_A_C) - B;
            }
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToHumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) + A * WZoneSetPoint - B;
        }
        if (RAFNFrac > 0.0) LoadToHumidifySetPoint = LoadToHumidifySetPoint / RAFNFrac;
        ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP = LoadToHumidifySetPoint;
        WZoneSetPoint = PsyWFnTdbRhPb(state, ZT(ZoneNum), (ZoneRHDehumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress, RoutineName);
        if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToDehumidifySetPoint =
                ((11.0 / 6.0) * C + A) * WZoneSetPoint - (B + C * (3.0 * state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) -
                                                                   (3.0 / 2.0) * state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) +
                                                                   (1.0 / 3.0) * state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum)));
            // Exact solution
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (A == 0.0) { // B=0
                LoadToDehumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) - B;
            } else {
                LoadToDehumidifySetPoint =
                    A * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum) * exp_700_A_C) / (1.0 - exp_700_A_C) - B; // exp_700_A_C set above
            }
        } else if (ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToDehumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) + A * WZoneSetPoint - B;
        }
        if (RAFNFrac > 0.0) LoadToDehumidifySetPoint = LoadToDehumidifySetPoint / RAFNFrac;
        ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP = LoadToDehumidifySetPoint;

        // The load is added to the TotalOutputRequired as in the Temperature Predictor.  There is also the remaining
        // output variable for those who will use this for humidity control and stored in DataZoneEnergyDemands with the
        // analogous temperature terms.
        if (SingleSetPoint) {
            ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = LoadToHumidifySetPoint;
        } else {
            if (LoadToHumidifySetPoint > 0.0 && LoadToDehumidifySetPoint > 0.0) {
                ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = LoadToHumidifySetPoint;
                RHSetPoint = ZoneRHHumidifyingSetPoint;
            } else if (LoadToHumidifySetPoint < 0.0 && LoadToDehumidifySetPoint < 0.0) {
                ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = LoadToDehumidifySetPoint;
                RHSetPoint = ZoneRHDehumidifyingSetPoint;
            } else if (LoadToHumidifySetPoint <= 0.0 && LoadToDehumidifySetPoint >= 0.0) { // deadband includes zero loads
                ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = 0.0;
            } else { // this should never occur!
                ShowSevereError(
                    state, "Humidistat: Unanticipated combination of humidifying and dehumidifying loads - report to EnergyPlus Development Team");
                ShowContinueErrorTimeStamp(state, "occurs in Zone=" + Zone(ZoneNum).Name);
                ShowContinueError(
                    state,
                    format("LoadToHumidifySetPoint={:.5R}, LoadToDehumidifySetPoint={:.5R}", LoadToHumidifySetPoint, LoadToDehumidifySetPoint));
                ShowContinueError(state, format("Zone RH Humidifying Set-point={:.1R}", ZoneRHHumidifyingSetPoint));
                ShowContinueError(state, format("Zone RH Dehumidifying Set-point={:.2R}", ZoneRHDehumidifyingSetPoint));
                ShowFatalError(state, "Program terminates due to above conditions.");
            }
        }
    }

    // Apply zone multipliers as needed
    ReportMoistLoadsZoneMultiplier(ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired,
                                   ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP,
                                   ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP,
                                   state.dataHeatBal->ZoneMoisturePredictedRate(ZoneNum),
                                   state.dataHeatBal->ZoneMoisturePredictedHumSPRate(ZoneNum),
                                   state.dataHeatBal->ZoneMoisturePredictedDehumSPRate(ZoneNum),
                                   Zone(ZoneNum).Multiplier,
                                   Zone(ZoneNum).ListMultiplier);

    // init each sequenced demand to the full output
    if (allocated(ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired))
        ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired = ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired; // array assignment
    if (allocated(ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP))
        ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP =
            ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP; // array assignment
    if (allocated(ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP))
        ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP =
            ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP; // array assignment
}

void ReportMoistLoadsZoneMultiplier(Real64 &TotalLoad,
                                    Real64 &TotalHumidLoad,
                                    Real64 &TotalDehumidLoad,
                                    Real64 &MoistLoadSingleZone,
                                    Real64 &MoistLoadHumidSingleZone,
                                    Real64 &MoistLoadDehumidSingleZone,
                                    Real64 const ZoneMultiplier,
                                    Real64 const ZoneMultiplierList)
{
    MoistLoadSingleZone = TotalLoad;
    MoistLoadHumidSingleZone = TotalHumidLoad;
    MoistLoadDehumidSingleZone = TotalDehumidLoad;

    Real64 ZoneMultFac = ZoneMultiplier * ZoneMultiplierList;

    TotalLoad *= ZoneMultFac;
    TotalHumidLoad *= ZoneMultFac;
    TotalDehumidLoad *= ZoneMultFac;
}

void CorrectZoneAirTemp(EnergyPlusData &state,
                        Real64 &ZoneTempChange, // Temperature change in zone air between previous and current timestep
                        bool const ShortenTimeStepSys,
                        bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
                        Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russell Taylor
    //       DATE WRITTEN   ???
    //       MODIFIED       November 1999, LKL; November 2016 Sang Hoon Lee, Tianzhen Hong, Rongpeng Zhang;
    //       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
    //                      February 2008 (Brent Griffith reworked history )

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the zone air temperature and modifies the system
    // time step.

    using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;
    using RoomAirModelManager::ManageAirModel;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleMaxValue;
    using ScheduleManager::GetScheduleMinValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CorrectZoneAirTemp");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 CpAir;                       // specific heat of air
    Real64 SumIntGain(0.0);             // Zone sum of convective internal gains
    Real64 SumIntGainExceptPeople(0.0); // Zone sum of convective internal gains except for convective heat from people, HybridModel
    Real64 SumHA(0.0);                  // Zone sum of Hc*Area
    Real64 SumHATsurf(0.0);             // Zone sum of Hc*Area*Tsurf
    Real64 SumHATref(0.0);              // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
    Real64 SumMCp(0.0);                 // Zone sum of MassFlowRate*Cp
    Real64 SumMCpT(0.0);                // Zone sum of MassFlowRate*Cp*T
    Real64 SumSysMCp(0.0);              // Zone sum of air system MassFlowRate*Cp
    Real64 SumSysMCpT(0.0);             // Zone sum of air system MassFlowRate*Cp*T
    Real64 ZoneEnthalpyIn(0.0);         // Zone inlet air enthalpy
    Real64 TempDepCoef(0.0);            // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
    Real64 TempIndCoef(0.0);            // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
    Real64 AirCap(0.0);                 // Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"
    Real64 SNLoad(0.0);                 // Sensible load calculated for zone in watts and then loaded in report variables
    int ZoneNum(0);
    int ZoneNodeNum(0); // System node number for air flow through zone either by system or as a plenum

    Real64 TempSupplyAir;
    Real64 ZoneMult;
    int LoopNode;

    // Initializations
    ZoneTempChange = DataPrecisionGlobals::constant_zero;

    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    auto &Zone = state.dataHeatBal->Zone;
    auto &RoomAirflowNetworkZoneInfo = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo;
    auto &ZT = state.dataHeatBalFanSys->ZT;
    auto &MAT = state.dataHeatBalFanSys->MAT;
    auto &ZoneT1 = state.dataHeatBalFanSys->ZoneT1;
    auto &Node = state.dataLoopNodes->Node;
    auto &ZoneAirHumRat = state.dataHeatBalFanSys->ZoneAirHumRat;
    auto &ZoneAirSolutionAlgo = state.dataHeatBal->ZoneAirSolutionAlgo;
    auto &AirModel = state.dataRoomAirMod->AirModel;
    auto &AIRRAT = state.dataHeatBalFanSys->AIRRAT;

    // Update zone temperatures
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

        if (ShortenTimeStepSys) {
            // time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
            if (state.dataHVACGlobal->NumOfSysTimeSteps !=
                state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time
                DownInterpolate4HistoryValues(PriorTimeStep,
                                              TimeStepSys,
                                              MAT(ZoneNum),
                                              state.dataHeatBalFanSys->XMAT(ZoneNum),
                                              state.dataHeatBalFanSys->XM2T(ZoneNum),
                                              state.dataHeatBalFanSys->XM3T(ZoneNum),
                                              state.dataHeatBalFanSys->XM4T(ZoneNum),
                                              MAT(ZoneNum),
                                              state.dataHeatBalFanSys->DSXMAT(ZoneNum),
                                              state.dataHeatBalFanSys->DSXM2T(ZoneNum),
                                              state.dataHeatBalFanSys->DSXM3T(ZoneNum),
                                              state.dataHeatBalFanSys->DSXM4T(ZoneNum));
                DownInterpolate4HistoryValues(PriorTimeStep,
                                              TimeStepSys,
                                              ZoneAirHumRat(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum),
                                              state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum),
                                              ZoneAirHumRat(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus1(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus2(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus3(ZoneNum),
                                              state.dataHeatBalFanSys->DSWZoneTimeMinus4(ZoneNum));
                if (state.dataRoomAirMod->IsZoneDV(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataRoomAirMod->MATFloor(ZoneNum),
                                                  state.dataRoomAirMod->XMATFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM2TFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM3TFloor(ZoneNum),
                                                  state.dataRoomAirMod->XM4TFloor(ZoneNum),
                                                  state.dataRoomAirMod->MATFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXMATFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXM2TFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXM3TFloor(ZoneNum),
                                                  state.dataRoomAirMod->DSXM4TFloor(ZoneNum));
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataRoomAirMod->MATOC(ZoneNum),
                                                  state.dataRoomAirMod->XMATOC(ZoneNum),
                                                  state.dataRoomAirMod->XM2TOC(ZoneNum),
                                                  state.dataRoomAirMod->XM3TOC(ZoneNum),
                                                  state.dataRoomAirMod->XM4TOC(ZoneNum),
                                                  state.dataRoomAirMod->MATOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXMATOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXM2TOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXM3TOC(ZoneNum),
                                                  state.dataRoomAirMod->DSXM4TOC(ZoneNum));
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataRoomAirMod->MATMX(ZoneNum),
                                                  state.dataRoomAirMod->XMATMX(ZoneNum),
                                                  state.dataRoomAirMod->XM2TMX(ZoneNum),
                                                  state.dataRoomAirMod->XM3TMX(ZoneNum),
                                                  state.dataRoomAirMod->XM4TMX(ZoneNum),
                                                  state.dataRoomAirMod->MATMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXMATMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXM2TMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXM3TMX(ZoneNum),
                                                  state.dataRoomAirMod->DSXM4TMX(ZoneNum));
                }
                if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        auto &ThisRAFNNode(RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode));
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      ThisRAFNNode.AirTemp,
                                                      ThisRAFNNode.AirTempX1,
                                                      ThisRAFNNode.AirTempX2,
                                                      ThisRAFNNode.AirTempX3,
                                                      ThisRAFNNode.AirTempX4,
                                                      ThisRAFNNode.AirTemp,
                                                      ThisRAFNNode.AirTempDSX1,
                                                      ThisRAFNNode.AirTempDSX2,
                                                      ThisRAFNNode.AirTempDSX3,
                                                      ThisRAFNNode.AirTempDSX4);
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      ThisRAFNNode.HumRat,
                                                      ThisRAFNNode.HumRatX1,
                                                      ThisRAFNNode.HumRatX2,
                                                      ThisRAFNNode.HumRatX3,
                                                      ThisRAFNNode.HumRatX4,
                                                      ThisRAFNNode.HumRat,
                                                      ThisRAFNNode.HumRatDSX1,
                                                      ThisRAFNNode.HumRatDSX2,
                                                      ThisRAFNNode.HumRatDSX3,
                                                      ThisRAFNNode.HumRatDSX4);
                    }
                }
            } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                     // do nothing because DS history would have been pushed prior and should be ready?
            }
        }

        // now update the variables actually used in the balance equations.
        if (!UseZoneTimeStepHistory) {
            state.dataHeatBalFanSys->ZTM1(ZoneNum) = state.dataHeatBalFanSys->DSXMAT(ZoneNum);
            state.dataHeatBalFanSys->ZTM2(ZoneNum) = state.dataHeatBalFanSys->DSXM2T(ZoneNum);
            state.dataHeatBalFanSys->ZTM3(ZoneNum) = state.dataHeatBalFanSys->DSXM3T(ZoneNum);

            state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus1(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus2(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus3(ZoneNum);
        } else {
            state.dataHeatBalFanSys->ZTM1(ZoneNum) = state.dataHeatBalFanSys->XMAT(ZoneNum);
            state.dataHeatBalFanSys->ZTM2(ZoneNum) = state.dataHeatBalFanSys->XM2T(ZoneNum);
            state.dataHeatBalFanSys->ZTM3(ZoneNum) = state.dataHeatBalFanSys->XM3T(ZoneNum);

            state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum);
        }

        AIRRAT(ZoneNum) = Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpSens *
                          PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName) *
                          PsyCpAirFnW(ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);

        AirCap = AIRRAT(ZoneNum);

        ManageAirModel(state, ZoneNum);

        // Calculate the various heat balance sums
        CalcZoneSums(state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);

        // Sum all convective internal gains except for people: SumIntGainExceptPeople
        if (state.dataHybridModel->FlagHybridModel_PC) {
            SumIntGainExceptPeople = SumAllInternalConvectionGainsExceptPeople(state, ZoneNum);
        }

        //    ZoneTempHistoryTerm = (3.0D0 * ZTM1(ZoneNum) - (3.0D0/2.0D0) * ZTM2(ZoneNum) + (1.0D0/3.0D0) * ZTM3(ZoneNum))
        ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;

        SNLoad = 0.0;

        if (ZoneNodeNum > 0) { // This zone is controlled by a zone equipment configuration or zone plenum

            // Heat balance coefficients for controlled zone, i.e. with system air flow
            TempDepCoef = SumHA + SumMCp + SumSysMCp;
            TempIndCoef =
                SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
            //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                TempIndCoef += state.afn->exchangeData(ZoneNum).TotalSen;
            }
            //    TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
            //    TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
            // Solve for zone air temperature
            switch (ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                ZT(ZoneNum) =
                    (TempIndCoef + AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                             (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum))) /
                    ((11.0 / 6.0) * AirCap + TempDepCoef);
                // Exact solution
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    ZT(ZoneNum) = ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    ZT(ZoneNum) =
                        (ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) + TempIndCoef / TempDepCoef;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                ZT(ZoneNum) = (AirCap * ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
            }
            // Update zone node temperature and thermostat temperature unless already updated in Room Air Model,
            // calculate load correction factor
            if ((AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::Mixing) || (!AirModel(ZoneNum).SimAirModel)) {
                // Fully mixed
                Node(ZoneNodeNum).Temp = ZT(ZoneNum);
                state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = ZT(ZoneNum);
                state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
            } else if (state.dataRoomAirMod->IsZoneDV(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                // UCSDDV: Not fully mixed - calculate factor to correct load for fully mixed assumption
                if (SumSysMCp > SmallMassFlow) {
                    TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
                    if (std::abs(TempSupplyAir - ZT(ZoneNum)) > state.dataHeatBal->TempConvergTol) {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) =
                            (TempSupplyAir - Node(ZoneNodeNum).Temp) / (TempSupplyAir - ZT(ZoneNum));
                        // constrain value to something reasonable
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = max(-3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum));
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = min(3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum));

                    } else {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0; // Indeterminate
                    }
                } else {
                    // Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
                    state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
                }
            } else if (AirModel(ZoneNum).SimAirModel && ((AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UserDefined) ||
                                                         (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::Mundt))) {
                if (SumSysMCp > SmallMassFlow) {
                    TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
                    if (std::abs(TempSupplyAir - ZT(ZoneNum)) > state.dataHeatBal->TempConvergTol) {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) =
                            (TempSupplyAir - Node(ZoneNodeNum).Temp) / (TempSupplyAir - ZT(ZoneNum));
                        // constrain value
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = max(-3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum));
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = min(3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum));

                    } else {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0; // Indeterminate
                    }
                } else {
                    // Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
                    state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
                }
            } else if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                // Zone node used in the RoomAirflowNetwork model
                ZT(ZoneNum) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).AirTemp;
                Node(ZoneNodeNum).Temp = ZT(ZoneNum);
                state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = ZT(ZoneNum);
                state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
            } else {
                Node(ZoneNodeNum).Temp = ZT(ZoneNum);
                state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = ZT(ZoneNum);
                state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
            }

            // Sensible load is the enthalpy into the zone minus the enthalpy that leaves the zone.
            CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
            ZoneEnthalpyIn = SumSysMCpT;

            // SNLOAD is the single zone load, without Zone Multiplier or Zone List Multiplier
            SNLoad = ZoneEnthalpyIn - (Node(ZoneNodeNum).MassFlowRate / ZoneMult) * CpAir * Node(ZoneNodeNum).Temp +
                     state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);

        } else {

            // Heat balance coefficients for uncontrolled zone, i.e. without system air flow
            TempDepCoef = SumHA + SumMCp;
            TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT;

            //      TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                TempIndCoef += state.afn->exchangeData(ZoneNum).TotalSen;
            }
            //      TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
            //      TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef

            // Solve for zone air temperature
            switch (ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                ZT(ZoneNum) =
                    (TempIndCoef + AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                             (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum))) /
                    ((11.0 / 6.0) * AirCap + TempDepCoef);
                // Exact solution
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    ZT(ZoneNum) = ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    ZT(ZoneNum) =
                        (ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) + TempIndCoef / TempDepCoef;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                ZT(ZoneNum) = (AirCap * ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
            }

            if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                ZT(ZoneNum) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).AirTemp;
            }

            // No sensible load
            SNLoad = 0.0;
        }

        // Hybrid modeling start
        if ((state.dataHybridModel->HybridModelZone(ZoneNum).InfiltrationCalc_T ||
             state.dataHybridModel->HybridModelZone(ZoneNum).InternalThermalMassCalc_T ||
             state.dataHybridModel->HybridModelZone(ZoneNum).PeopleCountCalc_T) &&
            (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
            InverseModelTemperature(
                state, ZoneNum, SumIntGain, SumIntGainExceptPeople, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, AirCap);
        }

        MAT(ZoneNum) = ZT(ZoneNum);

        // Determine sensible load heating/cooling rate and energy
        state.dataHeatBal->ZoneSNLoadHeatRate(ZoneNum) = max(SNLoad, 0.0);
        state.dataHeatBal->ZoneSNLoadCoolRate(ZoneNum) = std::abs(min(SNLoad, 0.0));
        state.dataHeatBal->ZoneSNLoadHeatEnergy(ZoneNum) = max(SNLoad, 0.0) * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataHeatBal->ZoneSNLoadCoolEnergy(ZoneNum) = std::abs(min(SNLoad, 0.0) * TimeStepSys * DataGlobalConstants::SecInHour);

        // Final humidity calcs
        CorrectZoneHumRat(state, ZoneNum);

        ZoneAirHumRat(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
        state.dataZoneTempPredictorCorrector->ZoneAirRelHum(ZoneNum) =
            100.0 * PsyRhFnTdbWPb(state, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress, RoutineName);

        // ZoneTempChange is used by HVACManager to determine if the timestep needs to be shortened.
        switch (ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
                if (state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) == 0) {
                    ZoneTempChange = max(ZoneTempChange,
                                         max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->ZTM1OC(ZoneNum)),
                                             std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTM1MX(ZoneNum))));
                } else {
                    ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)));
                }
            } else if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                if (state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) == 0) {
                    ZoneTempChange = max(ZoneTempChange,
                                         max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->ZTM1OC(ZoneNum)),
                                             std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTM1MX(ZoneNum))));
                } else {
                    ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)));
                }
            } else {
                ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)));
            }
        } break;
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution:
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
                if (state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) == 0) {
                    ZoneTempChange = max(ZoneTempChange,
                                         max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->Zone1OC(ZoneNum)),
                                             std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->Zone1MX(ZoneNum))));
                } else {
                    ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZoneT1(ZoneNum)));
                }
            } else if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                if (state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) == 0) {
                    ZoneTempChange = max(ZoneTempChange,
                                         max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->Zone1OC(ZoneNum)),
                                             std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->Zone1MX(ZoneNum))));
                } else {
                    ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZoneT1(ZoneNum)));
                }
            } else {
                ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZoneT1(ZoneNum)));
            }
        } break;
        default:
            break;
        }

        CalcZoneComponentLoadSums(state,
                                  ZoneNum,
                                  TempDepCoef,
                                  TempIndCoef,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumIntGains,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumHADTsurfs,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumMCpDTzones,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumMCpDtInfil,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumMCpDTsystem,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumNonAirSystem,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).CzdTdt,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).imBalance,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumEnthalpyM,
                                  state.dataHeatBal->ZnAirRpt(ZoneNum).SumEnthalpyH);

    } // ZoneNum
}

void PushZoneTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // push histories for timestep advancing

    // SUBROUTINE ARGUMENT DEFINITIONS:
    constexpr auto CorrectZoneAirTemp("CorrectZoneAirTemp");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int LoopNode;

    auto &RoomAirflowNetworkZoneInfo = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo;
    auto &ZT = state.dataHeatBalFanSys->ZT;
    auto &AirModel = state.dataRoomAirMod->AirModel;

    // Push the temperature and humidity ratio histories

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        state.dataHeatBalFanSys->XM4T(ZoneNum) = state.dataHeatBalFanSys->XM3T(ZoneNum);
        state.dataHeatBalFanSys->XM3T(ZoneNum) = state.dataHeatBalFanSys->XM2T(ZoneNum);
        state.dataHeatBalFanSys->XM2T(ZoneNum) = state.dataHeatBalFanSys->XMAT(ZoneNum);
        state.dataHeatBalFanSys->XMAT(ZoneNum) = state.dataHeatBalFanSys->ZTAV(ZoneNum); // using average for whole zone time step.
        state.dataHeatBalFanSys->XMPT(ZoneNum) = ZT(ZoneNum);
        //      MAT(ZoneNum)  = ZT(ZoneNum)

        state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum);
        state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum);
        state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum);
        state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum) =
            state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum); // using average for whole zone time step.
        state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
        state.dataHeatBalFanSys->WZoneTimeMinusP(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
        state.dataZoneTempPredictorCorrector->ZoneAirRelHum(ZoneNum) =
            100.0 *
            PsyRhFnTdbWPb(state, ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress, CorrectZoneAirTemp);

        if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV ||
            AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
            AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
            state.dataRoomAirMod->XM4TFloor(ZoneNum) = state.dataRoomAirMod->XM3TFloor(ZoneNum);
            state.dataRoomAirMod->XM3TFloor(ZoneNum) = state.dataRoomAirMod->XM2TFloor(ZoneNum);
            state.dataRoomAirMod->XM2TFloor(ZoneNum) = state.dataRoomAirMod->XMATFloor(ZoneNum);
            state.dataRoomAirMod->XMATFloor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            state.dataRoomAirMod->MATFloor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);

            state.dataRoomAirMod->XM4TOC(ZoneNum) = state.dataRoomAirMod->XM3TOC(ZoneNum);
            state.dataRoomAirMod->XM3TOC(ZoneNum) = state.dataRoomAirMod->XM2TOC(ZoneNum);
            state.dataRoomAirMod->XM2TOC(ZoneNum) = state.dataRoomAirMod->XMATOC(ZoneNum);
            state.dataRoomAirMod->XMATOC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
            state.dataRoomAirMod->MATOC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);

            state.dataRoomAirMod->XM4TMX(ZoneNum) = state.dataRoomAirMod->XM3TMX(ZoneNum);
            state.dataRoomAirMod->XM3TMX(ZoneNum) = state.dataRoomAirMod->XM2TMX(ZoneNum);
            state.dataRoomAirMod->XM2TMX(ZoneNum) = state.dataRoomAirMod->XMATMX(ZoneNum);
            state.dataRoomAirMod->XMATMX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
            state.dataRoomAirMod->MATMX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        }

        // for RoomAirflowNetwork model
        if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
            for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX4 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX1;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;

                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX4 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX1;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
            }
        }

        if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
            state.dataHeatBalFanSys->ZoneTM2(ZoneNum) = state.dataHeatBalFanSys->ZoneTMX(ZoneNum);
            state.dataHeatBalFanSys->ZoneTMX(ZoneNum) = state.dataHeatBalFanSys->ZTAV(ZoneNum); // using average for whole zone time step.
            state.dataHeatBalFanSys->ZoneWM2(ZoneNum) = state.dataHeatBalFanSys->ZoneWMX(ZoneNum);
            state.dataHeatBalFanSys->ZoneWMX(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum); // using average for whole zone time step.
            if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV ||
                AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
                AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
                state.dataRoomAirMod->ZoneM2Floor(ZoneNum) = state.dataRoomAirMod->ZoneMXFloor(ZoneNum);
                state.dataRoomAirMod->ZoneMXFloor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum); // using average for whole zone time step.
                state.dataRoomAirMod->ZoneM2OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                state.dataRoomAirMod->ZoneMXOC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum); // using average for whole zone time step.
                state.dataRoomAirMod->ZoneM2MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
                state.dataRoomAirMod->ZoneMXMX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum); // using average for whole zone time step.
            }

            if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                    //                        RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX =
                    // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                    //                        RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX =
                    // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1;
                }
            }
        }
    } // zone loop
}

void PushSystemTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // push histories

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int LoopNode;

    auto &RoomAirflowNetworkZoneInfo = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo;
    auto &MAT = state.dataHeatBalFanSys->MAT;
    auto &AirModel = state.dataRoomAirMod->AirModel;

    // Push the temperature and humidity ratio histories back in time

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        state.dataHeatBalFanSys->DSXM4T(ZoneNum) = state.dataHeatBalFanSys->DSXM3T(ZoneNum);
        state.dataHeatBalFanSys->DSXM3T(ZoneNum) = state.dataHeatBalFanSys->DSXM2T(ZoneNum);
        state.dataHeatBalFanSys->DSXM2T(ZoneNum) = state.dataHeatBalFanSys->DSXMAT(ZoneNum);
        state.dataHeatBalFanSys->DSXMAT(ZoneNum) = MAT(ZoneNum);

        state.dataHeatBalFanSys->DSWZoneTimeMinus4(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus3(ZoneNum);
        state.dataHeatBalFanSys->DSWZoneTimeMinus3(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus2(ZoneNum);
        state.dataHeatBalFanSys->DSWZoneTimeMinus2(ZoneNum) = state.dataHeatBalFanSys->DSWZoneTimeMinus1(ZoneNum);
        state.dataHeatBalFanSys->DSWZoneTimeMinus1(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum);

        if (state.dataRoomAirMod->IsZoneDV(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
            state.dataRoomAirMod->DSXM4TFloor(ZoneNum) = state.dataRoomAirMod->DSXM3TFloor(ZoneNum);
            state.dataRoomAirMod->DSXM3TFloor(ZoneNum) = state.dataRoomAirMod->DSXM2TFloor(ZoneNum);
            state.dataRoomAirMod->DSXM2TFloor(ZoneNum) = state.dataRoomAirMod->DSXMATFloor(ZoneNum);
            state.dataRoomAirMod->DSXMATFloor(ZoneNum) = state.dataRoomAirMod->MATFloor(ZoneNum);

            state.dataRoomAirMod->DSXM4TOC(ZoneNum) = state.dataRoomAirMod->DSXM3TOC(ZoneNum);
            state.dataRoomAirMod->DSXM3TOC(ZoneNum) = state.dataRoomAirMod->DSXM2TOC(ZoneNum);
            state.dataRoomAirMod->DSXM2TOC(ZoneNum) = state.dataRoomAirMod->DSXMATOC(ZoneNum);
            state.dataRoomAirMod->DSXMATOC(ZoneNum) = state.dataRoomAirMod->MATOC(ZoneNum);

            state.dataRoomAirMod->DSXM4TMX(ZoneNum) = state.dataRoomAirMod->DSXM3TMX(ZoneNum);
            state.dataRoomAirMod->DSXM3TMX(ZoneNum) = state.dataRoomAirMod->DSXM2TMX(ZoneNum);
            state.dataRoomAirMod->DSXM2TMX(ZoneNum) = state.dataRoomAirMod->DSXMATMX(ZoneNum);
            state.dataRoomAirMod->DSXMATMX(ZoneNum) = state.dataRoomAirMod->MATMX(ZoneNum);
        }
        if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
            for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX4 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX3;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX2;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX1;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;

                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX4 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX3;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX2;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX1;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
            }
        }
    } // zone loop

    if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->ZoneTM2(ZoneNum) = state.dataHeatBalFanSys->ZoneTMX(ZoneNum);
            state.dataHeatBalFanSys->ZoneTMX(ZoneNum) = MAT(ZoneNum); // using average for whole zone time step.
            state.dataHeatBalFanSys->ZoneWM2(ZoneNum) = state.dataHeatBalFanSys->ZoneWMX(ZoneNum);
            state.dataHeatBalFanSys->ZoneWMX(ZoneNum) =
                state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum); // using average for whole zone time step.

            if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV ||
                AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
                AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
                state.dataRoomAirMod->ZoneM2Floor(ZoneNum) = state.dataRoomAirMod->ZoneMXFloor(ZoneNum);
                state.dataRoomAirMod->ZoneMXFloor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum); // using average for whole zone time step.
                state.dataRoomAirMod->ZoneM2OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                state.dataRoomAirMod->ZoneMXOC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum); // using average for whole zone time step.
                state.dataRoomAirMod->ZoneM2MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
                state.dataRoomAirMod->ZoneMXMX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum); // using average for whole zone time step.
            }
            if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                    //                        RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX =
                    // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                    //                        RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX =
                    // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1;
                }
            }
        } // zone loop
    }
}

void RevertZoneTimestepHistories(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // rewind histories to undo inadvertent pushing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int LoopNode;

    auto &RoomAirflowNetworkZoneInfo = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo;
    auto &AirModel = state.dataRoomAirMod->AirModel;

    // Revert the temperature and humidity ratio histories

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        //  MAT(ZoneNum)  = state.dataHeatBalFanSys->XMAT(ZoneNum)
        state.dataHeatBalFanSys->XMAT(ZoneNum) = state.dataHeatBalFanSys->XM2T(ZoneNum);
        state.dataHeatBalFanSys->XM2T(ZoneNum) = state.dataHeatBalFanSys->XM3T(ZoneNum);
        state.dataHeatBalFanSys->XM3T(ZoneNum) = state.dataHeatBalFanSys->XM4T(ZoneNum);

        //   ZoneAirHumRat(ZoneNum)  = WZoneTimeMinus1(ZoneNum)
        state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum);
        state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum);
        state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum);

        if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV ||
            AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
            AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {

            //      MATFloor(ZoneNum)= XMATFloor(ZoneNum)
            state.dataRoomAirMod->XMATFloor(ZoneNum) = state.dataRoomAirMod->XM2TFloor(ZoneNum);
            state.dataRoomAirMod->XM2TFloor(ZoneNum) = state.dataRoomAirMod->XM3TFloor(ZoneNum);
            state.dataRoomAirMod->XM3TFloor(ZoneNum) = state.dataRoomAirMod->XM4TFloor(ZoneNum);
            //      MATOC(ZoneNum) = XMATOC(ZoneNum)
            state.dataRoomAirMod->XMATOC(ZoneNum) = state.dataRoomAirMod->XM2TOC(ZoneNum);
            state.dataRoomAirMod->XM2TOC(ZoneNum) = state.dataRoomAirMod->XM3TOC(ZoneNum);
            state.dataRoomAirMod->XM3TOC(ZoneNum) = state.dataRoomAirMod->XM4TOC(ZoneNum);

            //     MATMX(ZoneNum)=  XMATMX(ZoneNum)
            state.dataRoomAirMod->XMATMX(ZoneNum) = state.dataRoomAirMod->XM2TMX(ZoneNum);
            state.dataRoomAirMod->XM2TMX(ZoneNum) = state.dataRoomAirMod->XM3TMX(ZoneNum);
            state.dataRoomAirMod->XM3TMX(ZoneNum) = state.dataRoomAirMod->XM4TMX(ZoneNum);
        }

        if (AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
            for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX4;

                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3;
                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX4;
            }
        }
    } // zone loop
}

void CorrectZoneHumRat(EnergyPlusData &state, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the zone humidities.

    // REFERENCES:
    // Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
    // for BLAST.

    // Using/Aliasing
    using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CorrectZoneHumRat");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NodeNum;
    int ZoneNodeNum;
    int ZoneEquipConfigNum;
    bool ControlledZoneAirFlag;
    int ZoneRetPlenumNum;
    int ZoneSupPlenumNum;
    bool ZoneRetPlenumAirFlag;
    bool ZoneSupPlenumAirFlag;
    Real64 LatentGain;             // Zone latent load
    Real64 LatentGainExceptPeople; // Zone latent load except people -- hybrid model
    Real64 RhoAir;
    Real64 A;
    Real64 B;
    Real64 C;
    Real64 WZSat;
    Real64 MoistureMassFlowRate;
    Real64 ZoneMassFlowRate;
    Real64 SysTimeStepInSeconds;
    Real64 H2OHtOfVap;
    Real64 ZoneMult;
    int ADUListIndex;
    int ADUNum;
    int ADUInNode;
    int ADUOutNode;

    auto &Zone = state.dataHeatBal->Zone;
    auto &ZT = state.dataHeatBalFanSys->ZT;
    auto &Node = state.dataLoopNodes->Node;
    auto &ZoneRetPlenCond = state.dataZonePlenum->ZoneRetPlenCond;
    auto &ZoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig;
    auto &AirDistUnit = state.dataDefineEquipment->AirDistUnit;

    MoistureMassFlowRate = 0.0;
    ZoneMassFlowRate = 0.0;
    ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

    // Check to see if this is a controlled zone
    ControlledZoneAirFlag = Zone(ZoneNum).IsControlled;

    // Check to see if this is a plenum zone
    ZoneRetPlenumAirFlag = Zone(ZoneNum).IsReturnPlenum;
    ZoneSupPlenumAirFlag = Zone(ZoneNum).IsSupplyPlenum;

    if (ControlledZoneAirFlag) { // If there is system flow then calculate the flow rates
        ZoneEquipConfigNum = Zone(ZoneNum).ZoneEqNum;
        // Calculate moisture flow rate into each zone
        for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {

            MoistureMassFlowRate += (Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                                     Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).HumRat) /
                                    ZoneMult;
            ZoneMassFlowRate += Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
        } // NodeNum

        // Do the calculations for the plenum zone
    } else if (ZoneRetPlenumAirFlag) {
        ZoneRetPlenumNum = Zone(ZoneNum).PlenumCondNum;
        for (NodeNum = 1; NodeNum <= ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {

            MoistureMassFlowRate += (Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                     Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).HumRat) /
                                    ZoneMult;
            ZoneMassFlowRate += Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
        } // NodeNum
        // add in the leak flow
        for (ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
            ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
            if (AirDistUnit(ADUNum).UpStreamLeak) {
                ADUInNode = AirDistUnit(ADUNum).InletNodeNum;
                MoistureMassFlowRate += (AirDistUnit(ADUNum).MassFlowRateUpStrLk * Node(ADUInNode).HumRat) / ZoneMult;
                ZoneMassFlowRate += AirDistUnit(ADUNum).MassFlowRateUpStrLk / ZoneMult;
            }
            if (AirDistUnit(ADUNum).DownStreamLeak) {
                ADUOutNode = AirDistUnit(ADUNum).OutletNodeNum;
                MoistureMassFlowRate += (AirDistUnit(ADUNum).MassFlowRateDnStrLk * Node(ADUOutNode).HumRat) / ZoneMult;
                ZoneMassFlowRate += AirDistUnit(ADUNum).MassFlowRateDnStrLk / ZoneMult;
            }
        }

    } else if (ZoneSupPlenumAirFlag) {
        ZoneSupPlenumNum = Zone(ZoneNum).PlenumCondNum;
        MoistureMassFlowRate += (Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate *
                                 Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).HumRat) /
                                ZoneMult;
        ZoneMassFlowRate += Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate / ZoneMult;
    }

    // Calculate hourly humidity ratio from infiltration + humdidity added from latent load + system added moisture
    LatentGain = state.dataHeatBalFanSys->ZoneLatentGain(ZoneNum) + state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) +
                 state.dataHeatBalFanSys->SumLatentPool(ZoneNum);

    if (state.dataHybridModel->HybridModelZone(ZoneNum).PeopleCountCalc_H) {
        LatentGainExceptPeople = state.dataHeatBalFanSys->ZoneLatentGainExceptPeople(ZoneNum) + state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) +
                                 state.dataHeatBalFanSys->SumLatentPool(ZoneNum);
    }

    SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

    // Calculate the coefficients for the 3rd order derivative for final
    // zone humidity ratio.  The A, B, C coefficients are analogous to the
    // heat balance.  There are 2 cases that should be considered, system
    // operating and system shutdown.
    // SumHmARaW and SumHmARa will be used with the moisture balance on the building elements and
    // are currently set to zero to remind us where they need to be in the future
    bool no_ht_EMPD_or_HAMT(true);
    for (int i = Zone(ZoneNum).HTSurfaceFirst, e = Zone(ZoneNum).HTSurfaceLast; i <= e; ++i) {
        auto const &htAlgo(state.dataSurface->Surface(i).HeatTransferAlgorithm);
        if ((htAlgo == DataSurfaces::HeatTransferModel::EMPD) || (htAlgo == DataSurfaces::HeatTransferModel::HAMT)) {
            no_ht_EMPD_or_HAMT = false;
            break;
        }
    }
    if (no_ht_EMPD_or_HAMT) {
        state.dataHeatBalFanSys->SumHmARaW(ZoneNum) = 0.0;
        state.dataHeatBalFanSys->SumHmARa(ZoneNum) = 0.0;
    }

    RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), RoutineName);
    H2OHtOfVap = PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), ZT(ZoneNum));

    B = (LatentGain / H2OHtOfVap) +
        ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
         state.dataEnvrn->OutHumRat) +
        state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + (MoistureMassFlowRate) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
        state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
    A = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
        state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
        state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);

    if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
        state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
        (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
        // Multizone airflow calculated in AirflowNetwork
        B = (LatentGain / H2OHtOfVap) + (state.afn->exchangeData(ZoneNum).SumMHrW + state.afn->exchangeData(ZoneNum).SumMMHrW) +
            (MoistureMassFlowRate) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum);
        A = ZoneMassFlowRate + state.afn->exchangeData(ZoneNum).SumMHr + state.afn->exchangeData(ZoneNum).SumMMHr +
            state.dataHeatBalFanSys->SumHmARa(ZoneNum);
    }
    C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;

    if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
        B += state.afn->exchangeData(ZoneNum).TotalLat;
    }

    // Use a 3rd order derivative to predict final zone humidity ratio and
    // smooth the changes using the zone air capacitance.
    switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
    case DataHeatBalance::SolutionAlgo::ThirdOrder: {
        state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = (B + C * (3.0 * state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) -
                                                                        (3.0 / 2.0) * state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) +
                                                                        (1.0 / 3.0) * state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum))) /
                                                              ((11.0 / 6.0) * C + A);
        // Exact solution
    } break;
    case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
        if (A == 0.0) { // B=0
            state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = state.dataHeatBalFanSys->ZoneW1(ZoneNum) + B / C;
        } else {
            state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) =
                (state.dataHeatBalFanSys->ZoneW1(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
        }
    } break;
    case DataHeatBalance::SolutionAlgo::EulerMethod: {
        state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = (C * state.dataHeatBalFanSys->ZoneW1(ZoneNum) + B) / (C + A);
    } break;
    default:
        break;
    }

    // Set the humidity ratio to zero if the zone has been dried out
    if (state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) < 0.0) state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = 0.0;

    // Check to make sure that is saturated there is condensation in the zone
    // by resetting to saturation conditions.
    WZSat = PsyWFnTdbRhPb(state, ZT(ZoneNum), 1.0, state.dataEnvrn->OutBaroPress, RoutineName);

    if (state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) > WZSat) state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = WZSat;

    if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
        state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                                                                  .Node(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID)
                                                                  .HumRat;
    }

    // HybridModel with measured humidity ratio begins
    if ((state.dataHybridModel->HybridModelZone(ZoneNum).InfiltrationCalc_H || state.dataHybridModel->HybridModelZone(ZoneNum).PeopleCountCalc_H) &&
        (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
        InverseModelHumidity(state, ZoneNum, LatentGain, LatentGainExceptPeople, ZoneMassFlowRate, MoistureMassFlowRate, H2OHtOfVap, RhoAir);
    }

    // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
    ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
    if (ZoneNodeNum > 0) {
        Node(ZoneNodeNum).HumRat = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
        Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum));
    }
}

void DownInterpolate4HistoryValues(Real64 const OldTimeStep,
                                   Real64 const NewTimeStep,
                                   Real64 &oldVal0,
                                   Real64 &oldVal1,
                                   Real64 &oldVal2,
                                   [[maybe_unused]] Real64 &oldVal3,
                                   [[maybe_unused]] Real64 &oldVal4,
                                   Real64 &newVal0,
                                   Real64 &newVal1,
                                   Real64 &newVal2,
                                   Real64 &newVal3, // unused 1208
                                   Real64 &newVal4  // unused 1208
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Feb 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // provide a reusable routine for the various places that need to
    // interpolate a new set of history values on a different time scale
    // Once the systemtimestep has shortened, the new history terms need to be interpolated

    // METHODOLOGY EMPLOYED:
    // This routine assumes that the direction is to a shorter timestep.
    // The down step ratio, DSRatio = OldTimeStep/ NewTimeStep
    //  is expected to be roughly integer-valued and near 2.0 or 3.0 or 4.0 or more.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 oldTime0;
    Real64 oldTime1;
    Real64 oldTime2;
    Real64 oldTime3;
    Real64 oldTime4;
    Real64 newTime0;
    Real64 newTime1;
    Real64 newTime2;
    Real64 newTime3;
    Real64 newTime4;

    Real64 DSRatio;

    // first construct data on timestamps for interpolating with later
    oldTime0 = 0.0;
    oldTime1 = oldTime0 - OldTimeStep;
    oldTime2 = oldTime1 - OldTimeStep;
    oldTime3 = oldTime2 - OldTimeStep;
    oldTime4 = oldTime3 - OldTimeStep;

    newTime0 = 0.0;
    newTime1 = newTime0 - NewTimeStep;
    newTime2 = newTime1 - NewTimeStep;
    newTime3 = newTime2 - NewTimeStep;
    newTime4 = newTime3 - NewTimeStep;

    DSRatio = OldTimeStep / NewTimeStep; // should pretty much be an integer value 2, 3, 4, etc.

    newVal0 = oldVal0;

    if (std::abs(DSRatio - 2.0) < 0.01) { // DSRatio = 2
        // first two points lie between oldVal0 and oldVal1
        newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep));
        newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep));
        // last two points lie between oldVal1 and oldVal2
        newVal3 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime3) / (OldTimeStep));
        newVal4 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime4) / (OldTimeStep));
    } else if (std::abs(DSRatio - 3.0) < 0.01) { // DSRatio = 3
        // first three points lie between oldVal0 and oldVal1
        newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep));
        newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep));
        newVal3 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime3) / (OldTimeStep));
        // last point lie between oldVal1 and oldVal2
        newVal4 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime4) / (OldTimeStep));

    } else if (DSRatio > 3.99) { // DSRatio = 4 or more
        // all new points lie between oldVal0 and oldVal1
        newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep));
        newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep));
        newVal3 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime3) / (OldTimeStep));
        newVal4 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime4) / (OldTimeStep));
    }
}

void InverseModelTemperature(EnergyPlusData &state,
                             int const ZoneNum,              // Zone number
                             Real64 &SumIntGain,             // Zone sum of convective internal gains
                             Real64 &SumIntGainExceptPeople, // Zone sum of convective internal gains except for people
                             Real64 &SumHA,                  // Zone sum of Hc*Area
                             Real64 &SumHATsurf,             // Zone sum of Hc*Area*Tsurf
                             Real64 &SumHATref,              // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
                             Real64 &SumMCp,                 // Zone sum of MassFlowRate*Cp
                             Real64 &SumMCpT,                // Zone sum of MassFlowRate*Cp*T
                             Real64 &SumSysMCp,              // Zone sum of air system MassFlowRate*Cp
                             Real64 &SumSysMCpT,             // Zone sum of air system MassFlowRate*Cp*T
                             Real64 &AirCap                  // Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"rd
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Han Li
    //       DATE WRITTEN   February 2019
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine inversely solve infiltration airflow rate or people count with zone air temperatures measurements.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 CpAir;            // specific heat of air
    Real64 TempDepCoef(0.0); // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
    Real64 TempIndCoef(0.0); // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
    Real64 AirCapHM(0.0);    // Air power capacity for hybrid modeling

    Real64 AA(0.0);
    Real64 BB(0.0);
    Real64 CC(0.0);
    Real64 DD(0.0);
    Real64 SumIntGainPeople(0.0); // Inversely solved convective heat gain from people
    Real64 SumSysMCp_HM(0.0);
    Real64 SumSysMCpT_HM(0.0);
    Real64 NumPeople(0.0);          // Inversely solved number of people in the zone
    Real64 FractionSensible(0.0);   // Default sensible portion of the total heat from people
    Real64 FractionRadiation(0.0);  // Default radiation portion of the sensible heat from people
    Real64 FractionConvection(0.0); // Default convection portion of the sensible heat from people
    Real64 ActivityLevel(0.0);      // People activity level
    Real64 UpperBound(0.0);         // Upper bound of number of people
    Real64 zone_M_T(0.0);
    Real64 delta_T(0.0);
    Real64 AirDensity(0.0);
    Real64 M_inf(0.0);
    Real64 ACH_inf(0.0);
    Real64 ZoneMult;

    auto &Zone = state.dataHeatBal->Zone;
    auto &ZT = state.dataHeatBalFanSys->ZT;
    auto &HybridModelZone = state.dataHybridModel->HybridModelZone;

    ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
    Zone(ZoneNum).ZoneMeasuredTemperature = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneMeasuredTemperatureSchedulePtr);

    // HM calculation only HM calculation period start
    if (state.dataEnvrn->DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear &&
        state.dataEnvrn->DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
        Real64 HMMultiplierAverage(1.0);
        Real64 MultpHM(1.0);

        ZT(ZoneNum) = Zone(ZoneNum).ZoneMeasuredTemperature; // Array1D<Real64> ZT -- Zone
                                                             // Air Temperature Averaged over
                                                             // the System Time Increment
        if (HybridModelZone(ZoneNum).InfiltrationCalc_T && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            static constexpr std::string_view RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");

            if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirTemperatureSchedulePtr);
                Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);
                // Calculate the air humidity ratio at supply air inlet.
                Real64 CpAirInlet(0.0);
                CpAirInlet = PsyCpAirFnW(Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio);

                SumSysMCp_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                SumSysMCpT_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet * Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature;

                AA = SumSysMCp_HM + SumHA + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) +
                     state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
                BB = SumSysMCpT_HM + SumIntGain + SumHATsurf - SumHATref + state.dataHeatBalFanSys->MCPTV(ZoneNum) +
                     state.dataHeatBalFanSys->MCPTM(ZoneNum) + state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
                     state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp +
                     (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
            } else {
                AA = SumHA + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) +
                     state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
                BB = SumIntGain + SumHATsurf - SumHATref + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) +
                     state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
                     state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;
            }
            CC = AirCap;
            DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) +
                  (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum));

            zone_M_T = Zone(ZoneNum).ZoneMeasuredTemperature;
            delta_T = (Zone(ZoneNum).ZoneMeasuredTemperature - Zone(ZoneNum).OutDryBulbTemp);
            CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
            AirDensity = PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, Zone(ZoneNum).OutDryBulbTemp, state.dataEnvrn->OutHumRat, RoutineNameInfiltration);
            Zone(ZoneNum).delta_T = delta_T;

            // s4 - Set ACH to 0 when delta_T <= 0.5, add max and min limits to ach
            if (std::abs(delta_T) <= 0.5) {
                M_inf = 0.0;
            } else {
                M_inf = (BB + CC * DD - ((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredTemperature) / (CpAir * delta_T);
            }
            ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / Zone(ZoneNum).Volume * DataGlobalConstants::SecInHour));
            M_inf = (ACH_inf / DataGlobalConstants::SecInHour) * Zone(ZoneNum).Volume * AirDensity;

            // Overwrite variable with inverse solution
            Zone(ZoneNum).MCPIHM = M_inf;
            Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;

        } // Hybrid model infiltration calculation end

        // Hybrid modeling internal thermal mass calculation start
        if (HybridModelZone(ZoneNum).InternalThermalMassCalc_T && SumSysMCpT == 0 &&
            ZT(ZoneNum) != state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) &&
            state.dataHVACGlobal->UseZoneTimeStepHistory) { // HM calculation only when SumSysMCpT =0,
                                                            // TimeStepZone (not @ TimeStepSys)
            TempDepCoef = SumHA + SumMCp + SumSysMCp;
            TempIndCoef =
                SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
            //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                TempIndCoef += state.afn->exchangeData(ZoneNum).TotalSen;
            }
            // Calculate air capacity using DataHeatBalance::SolutionAlgo::AnalyticalSolution
            if (TempDepCoef == 0.0) {
                // Is this correct? Shouldn't we use log?? What if ZT(ZoneNum) ==
                // PreviousMeasuredZT1(ZoneNum)??
                AirCapHM = TempIndCoef / (ZT(ZoneNum) - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)); // Inverse equation
            } else {
                Real64 AirCapHM_temp = 0.0;
                if (TempIndCoef == TempDepCoef * ZT(ZoneNum)) {
                    AirCapHM_temp = 0.0; //  This is the denominator.
                } else {
                    AirCapHM_temp = (TempIndCoef - TempDepCoef * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)) /
                                    (TempIndCoef - TempDepCoef * ZT(ZoneNum));
                }

                if ((AirCapHM_temp > 0) && (AirCapHM_temp != 1)) {    // Avoide IND
                    AirCapHM = TempDepCoef / std::log(AirCapHM_temp); // Inverse equation
                } else {
                    AirCapHM = TempIndCoef / (ZT(ZoneNum) - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum));
                }
            }

            // Calculate multiplier
            if (std::abs(ZT(ZoneNum) - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)) > 0.05) { // Filter
                MultpHM = AirCapHM /
                          (Zone(ZoneNum).Volume *
                           PsyRhoAirFnPbTdbW(state,
                                             state.dataEnvrn->OutBaroPress,
                                             ZT(ZoneNum),
                                             state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                           PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum))) *
                          (state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour); // Inverse equation
                if ((MultpHM < 1.0) || (MultpHM > 30.0)) {                                   // Temperature capacity multiplier greater than
                                                                                             // 1 and less than 30
                    MultpHM = 1.0;                                                           // Default value 1.0
                }
            } else {
                MultpHM = 1.0; // Default value 1.0
            }

            // For timestep output
            Zone(ZoneNum).ZoneVolCapMultpSensHM = MultpHM;

            // Calculate the average multiplier of the zone for the whole running
            // period
            {
                // count for hybrid model calculations
                if (MultpHM > 1.0) {
                    Zone(ZoneNum).ZoneVolCapMultpSensHMSum += MultpHM;
                    Zone(ZoneNum).ZoneVolCapMultpSensHMCountSum++;
                }

                // Calculate and store the multiplier average at the end of HM
                // simulations
                if (state.dataEnvrn->DayOfYear == HybridModelZone(ZoneNum).HybridEndDayOfYear && state.dataGlobal->EndDayFlag) {
                    HMMultiplierAverage = Zone(ZoneNum).ZoneVolCapMultpSensHMSum / Zone(ZoneNum).ZoneVolCapMultpSensHMCountSum;
                    Zone(ZoneNum).ZoneVolCapMultpSensHMAverage = HMMultiplierAverage;
                }
            }
        } // Hybrid model internal thermal mass calcualtion end

        // Hybrid model people count calculation
        if (HybridModelZone(ZoneNum).PeopleCountCalc_T && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            Zone(ZoneNum).ZoneMeasuredTemperature = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneMeasuredTemperatureSchedulePtr);
            Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
            Zone(ZoneNum).ZonePeopleSensibleHeatFraction =
                GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleSensibleFractionSchedulePtr);
            Zone(ZoneNum).ZonePeopleRadiantHeatFraction =
                GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleRadiationFractionSchedulePtr);

            FractionSensible = Zone(ZoneNum).ZonePeopleSensibleHeatFraction;
            FractionRadiation = Zone(ZoneNum).ZonePeopleRadiantHeatFraction;
            ActivityLevel = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);

            if (FractionSensible <= 0.0) {
                FractionSensible = 0.6;
            }

            if (FractionRadiation <= 0.0) {
                FractionConvection = 0.7;
            } else {
                FractionConvection = 1.0 - FractionRadiation;
            }

            if (ActivityLevel <= 0.0) {
                ActivityLevel = 130.0;
            }

            if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirTemperatureSchedulePtr);
                Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                // Calculate the air humidity ratio at supply air inlet.
                Real64 CpAirInlet(0.0);
                CpAirInlet = PsyCpAirFnW(Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio);

                SumSysMCp_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                SumSysMCpT_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet * Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature;

                AA = SumSysMCp_HM + SumHA + SumMCp;
                BB = SumSysMCpT_HM + SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT +
                     (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
            } else {
                AA = SumHA + SumMCp;
                BB = SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT;
            }

            CC = AirCap;
            DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) +
                  (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum));

            SumIntGainPeople = ((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredTemperature - BB - CC * DD;
            UpperBound = max(0.0, SumIntGain / (ActivityLevel * FractionSensible * FractionConvection));
            NumPeople = min(UpperBound, max(0.0, SumIntGainPeople / (ActivityLevel * FractionSensible * FractionConvection)));

            if (NumPeople < 0.05) {
                NumPeople = 0;
            }
            Zone(ZoneNum).NumOccHM = NumPeople;
        }
    }

    // Update zone temperatures in the previous steps
    state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) = ZT(ZoneNum);
}

void InverseModelHumidity(EnergyPlusData &state,
                          int const ZoneNum,              // Zone number
                          Real64 &LatentGain,             // Zone sum of latent gain
                          Real64 &LatentGainExceptPeople, // Zone sum of latent gain except for people
                          Real64 &ZoneMassFlowRate,       // Zone air mass flow rate
                          Real64 &MoistureMassFlowRate,   // Zone moisture mass flow rate
                          Real64 &H2OHtOfVap,             // Heat of vaporization of air
                          Real64 &RhoAir                  // Air density
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Han Li
    //       DATE WRITTEN   February 2019
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine inversely solve infiltration airflow rate or people count with zone air humidity measurements.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InverseModelHumidity");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AA(0.0);
    Real64 BB(0.0);
    Real64 CC(0.0);
    Real64 DD(0.0);
    Real64 SumSysM_HM(0.0);
    Real64 SumSysMHumRat_HM(0.0);
    Real64 LatentGainPeople(0.0);
    Real64 NumPeople(0.0);
    Real64 FractionSensible(0.0);
    Real64 ActivityLevel(0.0);
    Real64 UpperBound(0.0);
    Real64 zone_M_HR(0.0);
    Real64 delta_HR(0.0);
    Real64 AirDensity(0.0);
    Real64 CpAir(0.0);
    Real64 M_inf(0.0);
    Real64 ACH_inf(0.0);
    Real64 SysTimeStepInSeconds(0.0);
    SysTimeStepInSeconds = DataGlobalConstants::SecInHour * state.dataHVACGlobal->TimeStepSys;

    auto &Zone = state.dataHeatBal->Zone;
    auto &HybridModelZone = state.dataHybridModel->HybridModelZone;

    // Get measured zone humidity ratio
    Zone(ZoneNum).ZoneMeasuredHumidityRatio = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneMeasuredHumidityRatioSchedulePtr);

    if (state.dataEnvrn->DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear &&
        state.dataEnvrn->DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
        state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) = Zone(ZoneNum).ZoneMeasuredHumidityRatio;

        // Hybrid Model calculate air infiltration rate
        if (HybridModelZone(ZoneNum).InfiltrationCalc_H && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            // Conditionally calculate the time dependent and time independent terms
            if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                SumSysM_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                SumSysMHumRat_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio;

                AA = SumSysM_HM + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) +
                     state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = SumSysMHumRat_HM + (LatentGain / H2OHtOfVap) +
                     ((state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) +
                     state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
            } else {
                AA = state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) +
                     state.dataHeatBalFanSys->SumHmARa(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                     state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = (LatentGain / H2OHtOfVap) +
                     ((state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) +
                     state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
            }

            CC = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;
            DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) -
                  (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) +
                  (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum));

            zone_M_HR = Zone(ZoneNum).ZoneMeasuredHumidityRatio;
            delta_HR = (Zone(ZoneNum).ZoneMeasuredHumidityRatio - state.dataEnvrn->OutHumRat);

            CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
            AirDensity =
                PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Zone(ZoneNum).OutDryBulbTemp, state.dataEnvrn->OutHumRat, RoutineName);

            if (std::abs(Zone(ZoneNum).ZoneMeasuredHumidityRatio - state.dataEnvrn->OutHumRat) < 0.0000001) {
                M_inf = 0.0;
            } else {
                M_inf = (CC * DD + BB - ((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredHumidityRatio) / delta_HR;
            }

            // Add threshold for air change rate
            ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / Zone(ZoneNum).Volume * DataGlobalConstants::SecInHour));
            M_inf = (ACH_inf / DataGlobalConstants::SecInHour) * Zone(ZoneNum).Volume * AirDensity;
            Zone(ZoneNum).MCPIHM = M_inf;
            Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;
        }

        // Hybrid Model calculate people count
        if (HybridModelZone(ZoneNum).PeopleCountCalc_H && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
            Zone(ZoneNum).ZonePeopleSensibleHeatFraction =
                GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleSensibleFractionSchedulePtr);
            Zone(ZoneNum).ZonePeopleRadiantHeatFraction =
                GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleRadiationFractionSchedulePtr);

            FractionSensible = Zone(ZoneNum).ZonePeopleSensibleHeatFraction;

            if (FractionSensible <= 0.0) {
                FractionSensible = 0.6;
            }

            if (ActivityLevel <= 0.0) {
                ActivityLevel = 130.0;
            }

            // Conditionally calculate the humidity-dependent and humidity-independent
            // terms.
            if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                SumSysM_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                SumSysMHumRat_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio;

                AA = SumSysM_HM + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                     state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = SumSysMHumRat_HM + (LatentGainExceptPeople / H2OHtOfVap) +
                     ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                      state.dataEnvrn->OutHumRat) +
                     state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
            } else {
                AA = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) +
                     state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                BB = (LatentGainExceptPeople / H2OHtOfVap) +
                     ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) *
                      state.dataEnvrn->OutHumRat) +
                     state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + (MoistureMassFlowRate) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
                     state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
            }

            CC = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;
            DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) -
                  (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) +
                  (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum));

            LatentGainPeople = (((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredHumidityRatio - BB - CC * DD) * H2OHtOfVap;
            UpperBound = max(0.0, LatentGain / (ActivityLevel * (1.0 - FractionSensible)));
            NumPeople = min(UpperBound, max(0.0, LatentGainPeople / (ActivityLevel * (1.0 - FractionSensible))));
            NumPeople = floor(NumPeople * 100.00 + 0.5) / 100.00;
            if (NumPeople < 0.05) {
                NumPeople = 0;
            }
            Zone(ZoneNum).NumOccHM = NumPeople;
        }
    }

    // Update zone humidity ratio in the previous steps
    state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) = Zone(ZoneNum).ZoneMeasuredHumidityRatio;
}

void CalcZoneSums(EnergyPlusData &state,
                  int const ZoneNum,  // Zone number
                  Real64 &SumIntGain, // Zone sum of convective internal gains
                  Real64 &SumHA,      // Zone sum of Hc*Area
                  Real64 &SumHATsurf, // Zone sum of Hc*Area*Tsurf
                  Real64 &SumHATref,  // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
                  Real64 &SumMCp,     // Zone sum of MassFlowRate*Cp
                  Real64 &SumMCpT,    // Zone sum of MassFlowRate*Cp*T
                  Real64 &SumSysMCp,  // Zone sum of air system MassFlowRate*Cp
                  Real64 &SumSysMCpT, // Zone sum of air system MassFlowRate*Cp*T
                  bool const CorrectorFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2003
    //       MODIFIED       Aug 2003, FCW: add SumHA contributions from window frame and divider
    //                      Aug 2003, CC: change how the reference temperatures are used
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the various sums that go into the zone heat balance
    // equation.  This replaces the SUMC, SUMHA, and SUMHAT calculations that were
    // previously done in various places throughout the program.
    // The SumHAT portion of the code is reproduced in RadiantSystemHighTemp and
    // RadiantSystemLowTemp and should be updated accordingly.
    // A reference temperature (Tref) is specified for use with the ceiling diffuser
    // convection correlation.  A bogus value of Tref = -999.9 defaults to using
    // the zone air (i.e. outlet) temperature for the reference temperature.
    // If Tref is applied to all surfaces, SumHA = 0, and SumHATref /= 0.
    // If Tref is not used at all, SumHATref = 0, and SumHA /= 0.
    // For future implementations, Tref can be easily converted into an array to
    // allow a different reference temperature to be specified for each surface.

    // Using/Aliasing
    using namespace DataSurfaces;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using InternalHeatGains::SumAllInternalConvectionGains;
    using InternalHeatGains::SumAllReturnAirConvectionGains;
    // using ZonePlenum::ZoneRetPlenCond;
    // using ZonePlenum::ZoneSupPlenCond;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 NodeTemp(0.0); // System node temperature //Autodesk:Init Initialization added to elim poss of use uninitialized
    Real64 MassFlowRate;  // System node mass flow rate
    int ZoneEquipConfigNum;
    bool ControlledZoneAirFlag;
    int ZoneRetPlenumNum;
    int ZoneSupPlenumNum;
    bool ZoneRetPlenumAirFlag;
    bool ZoneSupPlenumAirFlag;
    Real64 CpAir;      // Specific heat of air
    int SurfNum;       // Surface number
    Real64 HA;         // Hc*Area
    Real64 Area;       // Effective surface area
    Real64 RefAirTemp; // Reference air temperature for surface convection calculations
    Real64 ZoneMult;
    int ADUNum;
    int ADUInNode;
    int ADUOutNode;
    Real64 RetAirGain;

    SumIntGain = 0.0;
    SumHA = 0.0;
    SumHATsurf = 0.0;
    SumHATref = 0.0;
    SumMCp = 0.0;
    SumMCpT = 0.0;
    SumSysMCp = 0.0;
    SumSysMCpT = 0.0;

    auto &Zone = state.dataHeatBal->Zone;
    auto &MAT = state.dataHeatBalFanSys->MAT;
    auto &Node = state.dataLoopNodes->Node;
    auto &AirDistUnit = state.dataDefineEquipment->AirDistUnit;

    // Sum all convective internal gains: SumIntGain
    SumIntGain = SumAllInternalConvectionGains(state, ZoneNum);
    SumIntGain += state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumConvPool(ZoneNum);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    // low or zero)
    if (Zone(ZoneNum).NoHeatToReturnAir) {
        RetAirGain = SumAllReturnAirConvectionGains(state, ZoneNum, 0);
        SumIntGain += RetAirGain;
    }

    // Sum all non-system air flow, i.e. infiltration, simple ventilation, mixing, earth tube: SumMCp, SumMCpT
    SumMCp = state.dataHeatBalFanSys->MCPI(ZoneNum) + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) +
             state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
    SumMCpT = state.dataHeatBalFanSys->MCPTI(ZoneNum) + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) +
              state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
              state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;

    // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
    if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
        state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
        (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
        // Multizone airflow calculated in AirflowNetwork
        SumMCp = state.afn->exchangeData(ZoneNum).SumMCp + state.afn->exchangeData(ZoneNum).SumMVCp + state.afn->exchangeData(ZoneNum).SumMMCp;
        SumMCpT = state.afn->exchangeData(ZoneNum).SumMCpT + state.afn->exchangeData(ZoneNum).SumMVCpT + state.afn->exchangeData(ZoneNum).SumMMCpT;
    }

    // Sum all system air flow: SumSysMCp, SumSysMCpT
    // Check to see if this is a controlled zone
    ControlledZoneAirFlag = Zone(ZoneNum).IsControlled;
    if (CorrectorFlag) {
        // Check to see if this is a plenum zone
        ZoneRetPlenumAirFlag = Zone(ZoneNum).IsReturnPlenum;
        ZoneSupPlenumAirFlag = Zone(ZoneNum).IsSupplyPlenum;

        // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
        if (ControlledZoneAirFlag) {
            ZoneEquipConfigNum = Zone(ZoneNum).ZoneEqNum;
            auto const &zec(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum));
            for (int NodeNum = 1, NodeNum_end = zec.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum) {
                // Get node conditions
                //  this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call?
                //  how can we tell?  predict step must be lagged ?  correct step, systems have run.
                auto const &node(Node(zec.InletNode(NodeNum)));
                NodeTemp = node.Temp;
                MassFlowRate = node.MassFlowRate;
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));

                Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                SumSysMCp += MassFlowRate_CpAir;
                SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
            } // NodeNum

        } else if (ZoneRetPlenumAirFlag) {
            ZoneRetPlenumNum = Zone(ZoneNum).PlenumCondNum;
            auto const &zrpc(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum));
            Real64 const air_hum_rat(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            for (int NodeNum = 1, NodeNum_end = zrpc.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum) {
                // Get node conditions
                auto const &node(Node(zrpc.InletNode(NodeNum)));
                NodeTemp = node.Temp;
                MassFlowRate = node.MassFlowRate;
                CpAir = PsyCpAirFnW(air_hum_rat);

                Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                SumSysMCp += MassFlowRate_CpAir;
                SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
            } // NodeNum
            // add in the leaks
            for (int ADUListIndex = 1, ADUListIndex_end = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs;
                 ADUListIndex <= ADUListIndex_end;
                 ++ADUListIndex) {
                ADUNum = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                if (AirDistUnit(ADUNum).UpStreamLeak) {
                    ADUInNode = AirDistUnit(ADUNum).InletNodeNum;
                    NodeTemp = Node(ADUInNode).Temp;
                    MassFlowRate = AirDistUnit(ADUNum).MassFlowRateUpStrLk;
                    CpAir = PsyCpAirFnW(air_hum_rat);
                    Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                    SumSysMCp += MassFlowRate_CpAir;
                    SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
                }
                if (AirDistUnit(ADUNum).DownStreamLeak) {
                    ADUOutNode = AirDistUnit(ADUNum).OutletNodeNum;
                    NodeTemp = Node(ADUOutNode).Temp;
                    MassFlowRate = AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                    CpAir = PsyCpAirFnW(air_hum_rat);
                    Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                    SumSysMCp += MassFlowRate_CpAir;
                    SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
                }
            }

        } else if (ZoneSupPlenumAirFlag) {
            ZoneSupPlenumNum = Zone(ZoneNum).PlenumCondNum;
            // Get node conditions
            NodeTemp = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
            MassFlowRate = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));

            SumSysMCp += MassFlowRate * CpAir;
            SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
        }

        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

        SumSysMCp /= ZoneMult;
        SumSysMCpT /= ZoneMult;
    }
    // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
    for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
        HA = 0.0;
        Area = state.dataSurface->Surface(SurfNum).Area; // For windows, this is the glazing area

        if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) {
            auto const shading_flag(state.dataSurface->SurfWinShadingFlag(SurfNum));

            // Add to the convective internal gains
            if (ANY_INTERIOR_SHADE_BLIND(shading_flag)) {
                // The shade area covers the area of the glazing plus the area of the dividers.
                Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                // If interior shade or blind is present it is assumed that both the convective and IR radiative gain
                // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
                // interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
                // at the same time that the interaction between glass and shade is calculated.
                SumIntGain += state.dataSurface->SurfWinDividerHeatGain(SurfNum);
            }

            // Other convection term is applicable to equivalent layer window (ASHWAT) model
            if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL)
                SumIntGain += state.dataSurface->SurfWinOtherConvHeatGain(SurfNum);

            // Convective heat gain from natural convection in gap between glass and interior shade or blind
            if (ANY_INTERIOR_SHADE_BLIND(shading_flag)) SumIntGain += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

            // Convective heat gain from airflow window
            if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                SumIntGain += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                if (Zone(ZoneNum).NoHeatToReturnAir) {
                    SumIntGain += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                    state.dataSurface->SurfWinHeatGain(SurfNum) += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                    if (state.dataSurface->SurfWinHeatGain(SurfNum) >= 0.0) {
                        state.dataSurface->SurfWinHeatGainRep(SurfNum) = state.dataSurface->SurfWinHeatGain(SurfNum);
                        state.dataSurface->SurfWinHeatGainRepEnergy(SurfNum) =
                            state.dataSurface->SurfWinHeatGainRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                    } else {
                        state.dataSurface->SurfWinHeatLossRep(SurfNum) = -state.dataSurface->SurfWinHeatGain(SurfNum);
                        state.dataSurface->SurfWinHeatLossRepEnergy(SurfNum) =
                            state.dataSurface->SurfWinHeatLossRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                    }
                    state.dataSurface->SurfWinHeatTransferRepEnergy(SurfNum) =
                        state.dataSurface->SurfWinHeatGain(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                }
            }

            // Add to the surface convection sums
            if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                // Window frame contribution
                Real64 const HA_surf(state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                                     (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)));
                SumHATsurf += HA_surf * state.dataSurface->SurfWinFrameTempIn(SurfNum);
                HA += HA_surf;
            }

            if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && !ANY_INTERIOR_SHADE_BLIND(shading_flag)) {
                // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                Real64 const HA_surf(state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                     (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)));
                SumHATsurf += HA_surf * state.dataSurface->SurfWinDividerTempIn(SurfNum);
                HA += HA_surf;
            }

        } // End of check if window

        HA += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area;
        SumHATsurf += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area * state.dataHeatBalSurf->SurfTempInTmp(SurfNum);

        // determine reference air temperature for this surface
        switch (state.dataSurface->SurfTAirRef(SurfNum)) {
        case DataSurfaces::RefAirTemp::ZoneMeanAirTemp:
            // The zone air is the reference temperature (which is to be solved for in CorrectZoneAirTemp).
            RefAirTemp = MAT(ZoneNum);
            SumHA += HA;
            break;
        case DataSurfaces::RefAirTemp::AdjacentAirTemp:
            RefAirTemp = state.dataHeatBal->SurfTempEffBulkAir(SurfNum);
            SumHATref += HA * RefAirTemp;
            break;
        case DataSurfaces::RefAirTemp::ZoneSupplyAirTemp:
            // check whether this zone is a controlled zone or not
            if (!ControlledZoneAirFlag) {
                ShowFatalError(state, "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone(ZoneNum).Name);
                return;
            }
            // determine supply air temperature as a weighted average of the inlet temperatures.
            if (SumSysMCp > 0.0) {
                RefAirTemp = SumSysMCpT / SumSysMCp;
                SumHATref += HA * RefAirTemp;
            } else {
                // no system flow (yet) so just use zone air temperature
                // #5906
                SumHA += HA;
            }
            break;
        default:
            // currently set to mean air temp but should add error warning here
            RefAirTemp = MAT(ZoneNum);
            SumHA += HA;
            break;
        }

    } // SurfNum
}

void CalcZoneComponentLoadSums(EnergyPlusData &state,
                               int const ZoneNum,        // Zone number
                               Real64 const TempDepCoef, // Dependent coefficient
                               Real64 const TempIndCoef, // Independent coefficient
                               Real64 &SumIntGains,      // Zone sum of convective internal gains
                               Real64 &SumHADTsurfs,     // Zone sum of Hc*Area*(Tsurf - Tz)
                               Real64 &SumMCpDTzones,    // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
                               Real64 &SumMCpDtInfil,    // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
                               Real64 &SumMCpDTsystem,   // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
                               Real64 &SumNonAirSystem,  // Zone sum of non air system convective heat gains
                               Real64 &CzdTdt,           // Zone air energy storage term.
                               Real64 &imBalance,        // put all terms in eq. 5 on RHS , should be zero
                               Real64 &SumEnthalpyM,     // Zone sum of phase change material melting enthlpy
                               Real64 &SumEnthalpyH      // Zone sum of phase change material freezing enthalpy
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Feb 2008
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the various sums that go into the zone heat balance
    // equation for reporting (and diagnostic) purposes only.
    // It was derived from CalcZoneSums but differs in that that routine
    // breaks up the component's dependence on zone air temp in order to *solve* for zone air temp,
    // but here we *use* the result for zone air temp and calculate the terms of the heat balance
    // Go back and calculate each of the 6 terms in Equation 5 and fill report variables.
    // notes on these raw terms for zone air heat balance model :
    //  these are state variables at the end of the last system timestep.
    //  they are not necessarily proper averages for what happened over entire zone time step
    //  these are not multiplied by zone multipliers.
    //  The values are all Watts.

    // REFERENCES:
    // Equation 5 in Engineering Reference.

    // Using/Aliasing
    using namespace DataSurfaces;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;

    using InternalHeatGains::SumAllInternalConvectionGains;
    using InternalHeatGains::SumAllReturnAirConvectionGains;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NodeNum;          // System node number
    Real64 NodeTemp(0.0); // System node temperature //Autodesk:Init Initialization added to elim poss of use uninitialized
    Real64 MassFlowRate;  // System node mass flow rate
    int ZoneEquipConfigNum;
    bool ControlledZoneAirFlag;
    int ZoneRetPlenumNum;
    int ZoneSupPlenumNum;
    bool ZoneRetPlenumAirFlag;
    bool ZoneSupPlenumAirFlag;
    Real64 RhoAir;
    Real64 CpAir; // Specific heat of air
    int SurfNum;  // Surface number
    Real64 Area;  // Effective surface area
    int ADUListIndex;
    int ADUNum;
    int ADUInNode;
    int ADUOutNode;
    Real64 Threshold;
    Real64 SumRetAirGains;
    Real64 ADUHeatAddRate;
    Real64 QSensRate;

    SumIntGains = 0.0;    // Zone sum of convective internal gains
    SumHADTsurfs = 0.0;   // Zone sum of Hc*Area*(Tsurf - Tz)
    SumMCpDTzones = 0.0;  // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
    SumMCpDtInfil = 0.0;  // Zone sum of MassFlowRate*Cp*(Tout - Tz)
    SumMCpDTsystem = 0.0; // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
    SumNonAirSystem = 0.0;
    CzdTdt = 0.0;
    imBalance = 0.0;
    SumEnthalpyM = 0.0;
    SumEnthalpyH = 0.0;
    ADUHeatAddRate = 0.0;
    ADUNum = 0;
    QSensRate = 0;

    auto &Zone = state.dataHeatBal->Zone;
    auto &MAT = state.dataHeatBalFanSys->MAT;
    auto &Node = state.dataLoopNodes->Node;
    auto &ZoneAirHumRat = state.dataHeatBalFanSys->ZoneAirHumRat;
    auto &ZoneRetPlenCond = state.dataZonePlenum->ZoneRetPlenCond;
    auto &ZoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig;
    auto &AIRRAT = state.dataHeatBalFanSys->AIRRAT;
    auto &AirDistUnit = state.dataDefineEquipment->AirDistUnit;

    // Sum all convective internal gains: SumIntGain
    SumIntGains = SumAllInternalConvectionGains(state, ZoneNum);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    // low or zero)
    if (Zone(ZoneNum).NoHeatToReturnAir) {
        SumRetAirGains = SumAllReturnAirConvectionGains(state, ZoneNum, 0);
        SumIntGains += SumRetAirGains;
    }

    // sum non-system air flow transfers between zones
    SumMCpDTzones =
        state.dataHeatBalFanSys->MCPTM(ZoneNum) - state.dataHeatBalFanSys->MCPM(ZoneNum) * MAT(ZoneNum); // but maybe it should be ZTAV(ZoneNum)

    // Sum non-system air flow, i.e. infiltration, simple ventilation, earth tube
    //  reuse SumMCp, SumMCpT from CalcZoneSum but use MAT (or maybe ZTAV?) to complete
    SumMCpDtInfil = (state.dataHeatBalFanSys->MCPTI(ZoneNum) - state.dataHeatBalFanSys->MCPI(ZoneNum) * MAT(ZoneNum)) +
                    (state.dataHeatBalFanSys->MCPTV(ZoneNum) - state.dataHeatBalFanSys->MCPV(ZoneNum) * MAT(ZoneNum)) +
                    (state.dataHeatBalFanSys->MCPTE(ZoneNum) - state.dataHeatBalFanSys->MCPE(ZoneNum) * MAT(ZoneNum)) +
                    (state.dataHeatBalFanSys->MCPTC(ZoneNum) - state.dataHeatBalFanSys->MCPC(ZoneNum) * MAT(ZoneNum)) +
                    (state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp -
                     state.dataHeatBalFanSys->MDotCPOA(ZoneNum) *
                         MAT(ZoneNum)); // infiltration | Ventilation (simple) | Earth tube. | Cooltower | combined OA flow

    // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model (if used)
    if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
        state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
        (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS && state.afn->AirflowNetworkFanActivated)) {
        // Multizone airflow calculated in AirflowNetwork
        SumMCpDtInfil = state.afn->exchangeData(ZoneNum).SumMCpT + state.afn->exchangeData(ZoneNum).SumMVCpT -
                        (state.afn->exchangeData(ZoneNum).SumMCp + state.afn->exchangeData(ZoneNum).SumMVCp) * state.dataHeatBalFanSys->MAT(ZoneNum);
        SumMCpDTzones = state.afn->exchangeData(ZoneNum).SumMMCpT - state.afn->exchangeData(ZoneNum).SumMMCp * MAT(ZoneNum);
    }

    // Sum all system air flow: reusing how SumSysMCp, SumSysMCpT are calculated in CalcZoneSums

    // Check to see if this is a controlled zone
    ControlledZoneAirFlag = Zone(ZoneNum).IsControlled;

    // Check to see if this is a plenum zone
    ZoneRetPlenumAirFlag = Zone(ZoneNum).IsReturnPlenum;
    ZoneSupPlenumAirFlag = Zone(ZoneNum).IsSupplyPlenum;

    // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
    if (ControlledZoneAirFlag) {
        ZoneEquipConfigNum = Zone(ZoneNum).ZoneEqNum;
        for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
            // Get node conditions
            NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
            MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
            CalcZoneSensibleOutput(MassFlowRate, NodeTemp, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), QSensRate);
            SumMCpDTsystem += QSensRate;

            ADUNum = ZoneEquipConfig(ZoneEquipConfigNum).InletNodeADUNum(NodeNum);
            if (ADUNum > 0) {
                NodeTemp = Node(AirDistUnit(ADUNum).OutletNodeNum).Temp;
                MassFlowRate = Node(AirDistUnit(ADUNum).OutletNodeNum).MassFlowRate;
                CalcZoneSensibleOutput(MassFlowRate, NodeTemp, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), ADUHeatAddRate);
                AirDistUnit(ADUNum).HeatRate = max(0.0, ADUHeatAddRate);
                AirDistUnit(ADUNum).CoolRate = std::abs(min(0.0, ADUHeatAddRate));
                AirDistUnit(ADUNum).HeatGain = AirDistUnit(ADUNum).HeatRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                AirDistUnit(ADUNum).CoolGain = AirDistUnit(ADUNum).CoolRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            }

        } // NodeNum

    } else if (ZoneRetPlenumAirFlag) {
        ZoneRetPlenumNum = Zone(ZoneNum).PlenumCondNum;
        for (NodeNum = 1; NodeNum <= ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {
            // Get node conditions
            NodeTemp = Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).Temp;
            MassFlowRate = Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate;
            CalcZoneSensibleOutput(MassFlowRate, NodeTemp, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), QSensRate);
            SumMCpDTsystem += QSensRate;

        } // NodeNum
        // add in the leaks
        for (ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
            ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
            if (AirDistUnit(ADUNum).UpStreamLeak) {
                ADUInNode = AirDistUnit(ADUNum).InletNodeNum;
                NodeTemp = Node(ADUInNode).Temp;
                MassFlowRate = AirDistUnit(ADUNum).MassFlowRateUpStrLk;
                CalcZoneSensibleOutput(MassFlowRate, NodeTemp, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), QSensRate);
                SumMCpDTsystem += QSensRate;
            }
            if (AirDistUnit(ADUNum).DownStreamLeak) {
                ADUOutNode = AirDistUnit(ADUNum).OutletNodeNum;
                NodeTemp = Node(ADUOutNode).Temp;
                MassFlowRate = AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                CalcZoneSensibleOutput(MassFlowRate, NodeTemp, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), QSensRate);
                SumMCpDTsystem += QSensRate;
            }
        }

    } else if (ZoneSupPlenumAirFlag) {
        ZoneSupPlenumNum = Zone(ZoneNum).PlenumCondNum;
        // Get node conditions
        NodeTemp = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
        MassFlowRate = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
        CalcZoneSensibleOutput(MassFlowRate, NodeTemp, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), QSensRate);
        SumMCpDTsystem += QSensRate;
    }

    // non air system response.
    SumNonAirSystem = state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) + state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) +
                      state.dataHeatBalFanSys->SumConvPool(ZoneNum);

    // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
    for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {

        Area = state.dataSurface->Surface(SurfNum).Area; // For windows, this is the glazing area
        Real64 RefAirTemp = state.dataSurface->Surface(SurfNum).getInsideAirTemperature(state, SurfNum);

        if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) {

            // Add to the convective internal gains
            if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                // The shade area covers the area of the glazing plus the area of the dividers.
                Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                // If interior shade or blind is present it is assumed that both the convective and IR radiative gain
                // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
                // interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
                // at the same time that the interaction between glass and shade is calculated.
                SumIntGains += state.dataSurface->SurfWinDividerHeatGain(SurfNum);
            }

            // Other convection term is applicable to equivalent layer window (ASHWAT) model
            if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL)
                SumIntGains += state.dataSurface->SurfWinOtherConvHeatGain(SurfNum);

            // Convective heat gain from natural convection in gap between glass and interior shade or blind
            if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)))
                SumIntGains += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

            // Convective heat gain from airflow window
            if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                SumIntGains += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                if (Zone(ZoneNum).NoHeatToReturnAir) {
                    SumIntGains += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                }
            }

            // Add to the surface convection sums
            if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                // Window frame contribution

                SumHADTsurfs += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                                (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) *
                                (state.dataSurface->SurfWinFrameTempIn(SurfNum) - RefAirTemp);
            }

            if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                SumHADTsurfs += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                (state.dataSurface->SurfWinDividerTempIn(SurfNum) - RefAirTemp);
            }

        } // End of check if window

        SumHADTsurfs += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area * (state.dataHeatBalSurf->SurfTempInTmp(SurfNum) - RefAirTemp);

        // Accumulate Zone Phase Change Material Melting/Freezing Enthalpy output variables
        if (state.dataSurface->Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::CondFD) {
            state.dataHeatBal->ZnAirRpt(ZoneNum).SumEnthalpyM += state.dataHeatBalFiniteDiffMgr->SurfaceFD(SurfNum).EnthalpyM;
            state.dataHeatBal->ZnAirRpt(ZoneNum).SumEnthalpyH += state.dataHeatBalFiniteDiffMgr->SurfaceFD(SurfNum).EnthalpyF;
        }
    } // SurfNum

    // now calculate air energy storage source term.
    // capacitance is volume * density * heat capacity
    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
    RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, MAT(ZoneNum), ZoneAirHumRat(ZoneNum));

    switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
    case DataHeatBalance::SolutionAlgo::ThirdOrder: {
        CzdTdt = RhoAir * CpAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpSens * (MAT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)) /
                 (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        // Exact solution
    } break;
    case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
        CzdTdt = TempIndCoef - TempDepCoef * MAT(ZoneNum);
    } break;
    case DataHeatBalance::SolutionAlgo::EulerMethod: {
        CzdTdt = AIRRAT(ZoneNum) * (MAT(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum));
    } break;
    default:
        break;
    }

    if (state.dataGlobal->DisplayZoneAirHeatBalanceOffBalance) {
        imBalance = SumIntGains + SumHADTsurfs + SumMCpDTzones + SumMCpDtInfil + SumMCpDTsystem + SumNonAirSystem - CzdTdt;

        // throw warning if seriously out of balance (this may need to be removed if too noisy... )
        // formulate dynamic threshold value based on 20% of quadrature sum of components
        Threshold = 0.2 * std::sqrt(pow_2(SumIntGains) + pow_2(SumHADTsurfs) + pow_2(SumMCpDTzones) + pow_2(SumMCpDtInfil) + pow_2(SumMCpDTsystem) +
                                    pow_2(SumNonAirSystem) + pow_2(CzdTdt));
        if ((std::abs(imBalance) > Threshold) && (!state.dataGlobal->WarmupFlag) &&
            (!state.dataGlobal->DoingSizing)) { // air balance is out by more than threshold
            if (Zone(ZoneNum).AirHBimBalanceErrIndex == 0) {
                ShowWarningMessage(state, "Zone Air Heat Balance is out of balance for zone named " + Zone(ZoneNum).Name);
                ShowContinueError(state, format("Zone Air Heat Balance Deviation Rate is more than {:.1R} {{W}}", Threshold));
                if (state.dataHVACGlobal->TurnFansOn) {
                    ShowContinueError(state, "Night cycle fan operation may be causing above error");
                }

                ShowContinueErrorTimeStamp(state, " Occurrence info:");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           "Zone Air Heat Balance is out of balance ... zone named " + Zone(ZoneNum).Name,
                                           Zone(ZoneNum).AirHBimBalanceErrIndex,
                                           std::abs(imBalance) - Threshold,
                                           std::abs(imBalance) - Threshold,
                                           _,
                                           "{W}",
                                           "{W}");
        }
    }
}

bool VerifyThermostatInZone(EnergyPlusData &state, std::string const &ZoneName) // Zone to verify
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   Feb 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function verifies that a zone (by name) has a Zone Control:Thermostatic
    // object entered.

    // Return value
    bool HasThermostat; // True if does, false if not.

    if (state.dataZoneCtrls->GetZoneAirStatsInputFlag) {
        GetZoneAirSetPoints(state);
        state.dataZoneCtrls->GetZoneAirStatsInputFlag = false;
    }
    if (state.dataZoneCtrls->NumTempControlledZones > 0) {
        if (UtilityRoutines::FindItemInList(ZoneName, state.dataZoneCtrls->TempControlledZone, &ZoneTempControls::ZoneName) > 0) {
            HasThermostat = true;
        } else {
            HasThermostat = false;
        }
    } else {
        HasThermostat = false;
    }
    return HasThermostat;
}

bool VerifyControlledZoneForThermostat(EnergyPlusData &state, std::string const &ZoneName) // Zone to verify
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   Mar 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function verifies that a zone (by name) has a ZoneHVAC:EquipmentConnections
    // object entered.

    // Using/Aliasing
    using DataZoneEquipment::EquipConfiguration;

    return (UtilityRoutines::FindItemInList(ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName) > 0);
}

void DetectOscillatingZoneTemp(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Oscillating temperatures between HVAC timesteps indicate that the
    // simulation may be poor. Code is trying to be fast since the purpose
    // is to see the impact on oscillating by trying longer time steps in
    // an attempt to speed up the simulation.
    // Note that the OscillateMagnitude threshold must be less than
    // MaxZoneTempDiff since ManageHVAC keeps shortening the timestep
    // until that is reached unless it goes to less than the
    // MinTimeStepSys.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int iZone;
    Real64 NegOscillateMagnitude;
    bool isOscillate;
    Real64 Diff12;
    Real64 Diff23;
    Real64 Diff34;
    bool isAnyZoneOscillating;
    bool isAnyZoneOscillatingDuringOccupancy;
    bool isAnyZoneOscillatingInDeadband;

    auto &Zone = state.dataHeatBal->Zone;
    auto &NumOfZones = state.dataGlobal->NumOfZones;

    // first time run allocate arrays and setup output variable
    if (state.dataZoneTempPredictorCorrector->SetupOscillationOutputFlag) {
        state.dataZoneTempPredictorCorrector->ZoneTempHist.allocate(4, NumOfZones);
        state.dataZoneTempPredictorCorrector->ZoneTempHist = 0.0;
        state.dataZoneTempPredictorCorrector->ZoneTempOscillate.dimension(NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy.dimension(NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband.dimension(NumOfZones, 0.0);
        // set up zone by zone variables
        // CurrentModuleObject='Zone'
        for (iZone = 1; iZone <= NumOfZones; ++iZone) {
            SetupOutputVariable(state,
                                "Zone Oscillating Temperatures Time",
                                OutputProcessor::Unit::hr,
                                state.dataZoneTempPredictorCorrector->ZoneTempOscillate(iZone),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                Zone(iZone).Name);
            SetupOutputVariable(state,
                                "Zone Oscillating Temperatures During Occupancy Time",
                                OutputProcessor::Unit::hr,
                                state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy(iZone),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                Zone(iZone).Name);
            SetupOutputVariable(state,
                                "Zone Oscillating Temperatures in Deadband Time",
                                OutputProcessor::Unit::hr,
                                state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband(iZone),
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                Zone(iZone).Name);
        }
        // set up a variable covering all zones
        SetupOutputVariable(state,
                            "Facility Any Zone Oscillating Temperatures Time",
                            OutputProcessor::Unit::hr,
                            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "Facility");
        SetupOutputVariable(state,
                            "Facility Any Zone Oscillating Temperatures During Occupancy Time",
                            OutputProcessor::Unit::hr,
                            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "Facility");
        SetupOutputVariable(state,
                            "Facility Any Zone Oscillating Temperatures in Deadband Time",
                            OutputProcessor::Unit::hr,
                            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "Facility");
        // test if the oscillation variables are even used
        if (ReportingThisVariable(state, "Zone Oscillating Temperatures Time") ||
            ReportingThisVariable(state, "Zone Oscillating Temperatures During Occupancy Time") ||
            ReportingThisVariable(state, "Zone Oscillating Temperatures in Deadband Time") ||
            ReportingThisVariable(state, "Facility Any Zone Oscillating Temperatures Time") ||
            ReportingThisVariable(state, "Facility Any Zone Oscillating Temperatures During Occupancy Time") ||
            ReportingThisVariable(state, "Facility Any Zone Oscillating Temperatures in Deadband Time")) {
            state.dataZoneTempPredictorCorrector->OscillationVariablesNeeded = true;
        }
        state.dataZoneTempPredictorCorrector->SetupOscillationOutputFlag = false;
    }

    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    if (state.dataZoneTempPredictorCorrector->OscillationVariablesNeeded) {
        // precalc the negative value for performance
        NegOscillateMagnitude = -OscillateMagnitude;
        // assume no zone is oscillating
        isAnyZoneOscillating = false;
        isAnyZoneOscillatingDuringOccupancy = false;
        isAnyZoneOscillatingInDeadband = false;

        for (iZone = 1; iZone <= NumOfZones; ++iZone) {
            isOscillate = false;
            state.dataZoneTempPredictorCorrector->ZoneTempHist(4, iZone) = state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone);
            state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone) = state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone);
            state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone) = state.dataZoneTempPredictorCorrector->ZoneTempHist(1, iZone);
            state.dataZoneTempPredictorCorrector->ZoneTempHist(1, iZone) = state.dataHeatBalFanSys->ZT(iZone);
            Diff34 = state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone) - state.dataZoneTempPredictorCorrector->ZoneTempHist(4, iZone);
            Diff23 = state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone) - state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone);
            Diff12 = state.dataZoneTempPredictorCorrector->ZoneTempHist(1, iZone) - state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone);
            // roll out the conditionals for increased performance
            if (Diff12 > OscillateMagnitude) {
                if (Diff23 < NegOscillateMagnitude) {
                    if (Diff34 > OscillateMagnitude) {
                        isOscillate = true;
                    }
                }
            }
            // now try the opposite sequence of swings
            if (Diff12 < NegOscillateMagnitude) {
                if (Diff23 > OscillateMagnitude) {
                    if (Diff34 < NegOscillateMagnitude) {
                        isOscillate = true;
                    }
                }
            }
            state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy(iZone) = 0.0;
            state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband(iZone) = 0.0;
            if (isOscillate) {
                state.dataZoneTempPredictorCorrector->ZoneTempOscillate(iZone) = TimeStepSys;
                isAnyZoneOscillating = true;
                if (allocated(state.dataThermalComforts->ThermalComfortInASH55)) {
                    if (state.dataThermalComforts->ThermalComfortInASH55(iZone).ZoneIsOccupied) {
                        state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy(iZone) = TimeStepSys;
                        isAnyZoneOscillatingDuringOccupancy = true;
                    }
                }
                if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(iZone)) {
                    state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband(iZone) = TimeStepSys;
                    isAnyZoneOscillatingInDeadband = true;
                }
            } else {
                state.dataZoneTempPredictorCorrector->ZoneTempOscillate(iZone) = 0.0;
            }
        }
        // any zone variable
        if (isAnyZoneOscillating) {
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate = TimeStepSys;
        } else {
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate = 0.0;
        }
        if (isAnyZoneOscillatingDuringOccupancy) {
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy = TimeStepSys;
        } else {
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy = 0.0;
        }
        if (isAnyZoneOscillatingInDeadband) {
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband = TimeStepSys;
        } else {
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband = 0.0;
        }

        // annual/runperiod sum for _perflog.csv file
        state.dataZoneTempPredictorCorrector->AnnualAnyZoneTempOscillate += state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate;
        state.dataZoneTempPredictorCorrector->AnnualAnyZoneTempOscillateDuringOccupancy +=
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy;
        state.dataZoneTempPredictorCorrector->AnnualAnyZoneTempOscillateInDeadband +=
            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband;
    }
}

void AdjustAirSetPointsforOpTempCntrl(EnergyPlusData &state, int const TempControlledZoneID, int const ActualZoneNum, Real64 &ZoneAirSetPoint)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   June 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine modifies the air temperature setpoint to effect operative temperature control

    // METHODOLOGY EMPLOYED:
    // pass in data and alter setpoint if needed

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 thisMRT;         // local variable for mean radiant temperature in this zone
    Real64 thisMRTFraction; // local variable for fraction that MRT is in Op Temp definition
    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;

    if (!(state.dataZoneCtrls->AnyOpTempControl)) return; // do nothing to setpoint

    if (!(TempControlledZone(TempControlledZoneID).OperativeTempControl)) return; // do nothing to setpoint

    // is operative temp radiative fraction scheduled or fixed?
    if (TempControlledZone(TempControlledZoneID).OpTempCntrlModeScheduled) {
        thisMRTFraction = GetCurrentScheduleValue(state, TempControlledZone(TempControlledZoneID).OpTempRadiativeFractionSched);
    } else {
        thisMRTFraction = TempControlledZone(TempControlledZoneID).FixedRadiativeFraction;
    }

    // get mean radiant temperature for zone
    thisMRT = state.dataHeatBal->ZoneMRT(ActualZoneNum);

    // modify setpoint for operative temperature control
    //  traping for MRT fractions between 0.0 and 0.9 during get input, so shouldn't be able to divide by zero here.
    ZoneAirSetPoint = (ZoneAirSetPoint - thisMRTFraction * thisMRT) / (1.0 - thisMRTFraction);
}

void AdjustOperativeSetPointsforAdapComfort(EnergyPlusData &state, int const TempControlledZoneID, Real64 &ZoneAirSetPoint)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Xuan Luo
    //       DATE WRITTEN   Jan 2017
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine adjust the operative setpoints for each controlled adaptive thermal comfort models.

    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &AdapComfortDailySetPointSchedule = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int originZoneAirSetPoint = ZoneAirSetPoint;
    int AdaptiveComfortModelTypeIndex = TempControlledZone(TempControlledZoneID).AdaptiveComfortModelTypeIndex;

    // adjust zone operative setpoint
    if (!(TempControlledZone(TempControlledZoneID).AdaptiveComfortTempControl)) return; // do nothing to setpoint
    if ((state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn != DataGlobalConstants::KindOfSim::DesignDay) &&
        (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn != DataGlobalConstants::KindOfSim::HVACSizeDesignDay)) {
        // Adjust run period cooling set point
        switch (AdaptiveComfortModelTypeIndex) {
        case static_cast<int>(AdaptiveComfortModel::ASH55_CENTRAL):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(state.dataEnvrn->DayOfYear);
            break;
        case static_cast<int>(AdaptiveComfortModel::ASH55_UPPER_90):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(state.dataEnvrn->DayOfYear);
            break;
        case static_cast<int>(AdaptiveComfortModel::ASH55_UPPER_80):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(state.dataEnvrn->DayOfYear);
            break;
        case static_cast<int>(AdaptiveComfortModel::CEN15251_CENTRAL):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(state.dataEnvrn->DayOfYear);
            break;
        case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_I):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(state.dataEnvrn->DayOfYear);
            break;
        case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_II):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(state.dataEnvrn->DayOfYear);
            break;
        case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_III):
            ZoneAirSetPoint = AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(state.dataEnvrn->DayOfYear);
            break;
        default:;
        }
    } else {
        int const envrnDayNum(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum);
        int constexpr summerDesignDayTypeIndex(9);
        // Adjust summer design day set point
        if (state.dataWeatherManager->DesDayInput(envrnDayNum).DayType == summerDesignDayTypeIndex) {
            ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay[AdaptiveComfortModelTypeIndex - 2];
        }
    }
    // If adaptive operative temperature not applicable, set back
    if (ZoneAirSetPoint < originZoneAirSetPoint) {
        ZoneAirSetPoint = originZoneAirSetPoint;
    }
    // If meet fault flag, set back
    if (ZoneAirSetPoint == -1) {
        ZoneAirSetPoint = originZoneAirSetPoint;
    }
}

void CalcZoneAirComfortSetPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine sets the thermal comfort setpoints for each controlled zone based on air tempeature
    // obtained from thermal comfort models.

    // Using/Aliasing

    using ScheduleManager::GetCurrentScheduleValue;
    using ThermalComfort::ManageThermalComfort;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RelativeZoneNum;
    int ActualZoneNum;
    int ComfortControlSchedIndex;
    int SetPointComfortSchedIndex;
    int SetPointComfortSchedIndexHot;
    int SetPointComfortSchedIndexCold;
    int SchedNameIndex;
    int SchedTypeIndex;
    int PeopleNum;
    int ObjectCount;
    Real64 PeopleCount;
    Real64 SetPointLo;
    Real64 SetPointHi;
    Real64 NumberOccupants;
    Real64 Tset;

    auto &Zone = state.dataHeatBal->Zone;
    auto &TempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint;
    auto &ZoneComfortControlsFanger = state.dataHeatBalFanSys->ZoneComfortControlsFanger;
    auto &TempControlType = state.dataHeatBalFanSys->TempControlType;
    auto &TempControlTypeRpt = state.dataHeatBalFanSys->TempControlTypeRpt;
    auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone;
    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;
    auto &ComfortControlType = state.dataHeatBalFanSys->ComfortControlType;
    auto &ComfortControlTypeRpt = state.dataHeatBalFanSys->ComfortControlTypeRpt;

    // Call thermal comfort module to read zone control comfort object
    if (state.dataZoneTempPredictorCorrector->CalcZoneAirComfortSetPointsFirstTimeFlag) {
        ManageThermalComfort(state, true);
        state.dataZoneTempPredictorCorrector->CalcZoneAirComfortSetPointsFirstTimeFlag = false;
    }

    ComfortControlType = DataHVACGlobals::ThermostatType::Uncontrolled; // Default

    for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++RelativeZoneNum) {

        ActualZoneNum = ComfortControlledZone(RelativeZoneNum).ActualZoneNum;
        ComfortControlSchedIndex = ComfortControlledZone(RelativeZoneNum).ComfortSchedIndex;
        ComfortControlType(ActualZoneNum) = static_cast<DataHVACGlobals::ThermostatType>(GetCurrentScheduleValue(state, ComfortControlSchedIndex));
        ComfortControlTypeRpt = static_cast<int>(ComfortControlType(ActualZoneNum));

        // Get PMV values
        switch (ComfortControlType(ActualZoneNum)) {
        case DataHVACGlobals::ThermostatType::Uncontrolled:
            ZoneComfortControlsFanger(ActualZoneNum).LowPMV = -999.0;
            ZoneComfortControlsFanger(ActualZoneNum).HighPMV = -999.0;
            break;
        case DataHVACGlobals::ThermostatType::SingleHeating:
            SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_SingleHeating;
            SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
            SetPointComfortSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SchedTypeIndex).PMVSchedIndex;
            ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeating);
            ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
            ZoneComfortControlsFanger(ActualZoneNum).HighPMV = -999.0;
            break;
        case DataHVACGlobals::ThermostatType::SingleCooling:
            SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_SingleCooling;
            SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
            SetPointComfortSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SchedTypeIndex).PMVSchedIndex;
            ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(DataHVACGlobals::ThermostatType::SingleCooling);
            ZoneComfortControlsFanger(ActualZoneNum).LowPMV = -999.0;
            ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
            break;
        case DataHVACGlobals::ThermostatType::SingleHeatCool:
            SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_SingleHeatCool;
            SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
            SetPointComfortSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SchedTypeIndex).PMVSchedIndex;
            ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(DataHVACGlobals::ThermostatType::SingleHeatCool);
            ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
            ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
            break;
        case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
            SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_DualSetPointWithDeadBand;
            SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
            SetPointComfortSchedIndexHot = state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).HeatPMVSchedIndex;
            SetPointComfortSchedIndexCold = state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).CoolPMVSchedIndex;
            ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand);
            ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndexHot);
            ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndexCold);
            if (ZoneComfortControlsFanger(ActualZoneNum).LowPMV > ZoneComfortControlsFanger(ActualZoneNum).HighPMV) {
                ++ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrCount;
                if (ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrCount < 2) {
                    ShowWarningError(state,
                                     "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint: The heating PMV setpoint is above the cooling "
                                     "PMV setpoint in " +
                                         state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).Name);
                    ShowContinueError(state, "The zone dual heating PMV setpoint is set to the dual cooling PMV setpoint.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The heating PMV setpoint is still above the cooling PMV setpoint",
                                                   ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrIndex,
                                                   ZoneComfortControlsFanger(ActualZoneNum).LowPMV,
                                                   ZoneComfortControlsFanger(ActualZoneNum).LowPMV);
                }
                ZoneComfortControlsFanger(ActualZoneNum).LowPMV = ZoneComfortControlsFanger(ActualZoneNum).HighPMV;
            }
            break;
        default:
            ShowSevereError(state,
                            format("CalcZoneAirTempSetpoints: Illegal thermal control control type for Zone={}, Found value={}, in Schedule={}",
                                   Zone(ActualZoneNum).Name,
                                   ComfortControlTypeRpt(ActualZoneNum),
                                   ComfortControlledZone(RelativeZoneNum).ControlTypeSchedName));
            break;
        }

        // Check Average method
        switch (ComfortControlledZone(RelativeZoneNum).AverageMethod) {
        case DataZoneControls::AverageMethod::NO:
            PeopleNum = ComfortControlledZone(RelativeZoneNum).SpecificObjectNum;
            if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::SingleCooling) {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointLo);
            } else {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, SetPointLo);
            }
            if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointHi);
            break;
        case DataZoneControls::AverageMethod::SPE:
            PeopleNum = ComfortControlledZone(RelativeZoneNum).SpecificObjectNum;
            if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::SingleCooling) {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointLo);
            } else {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, SetPointLo);
            }
            if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand)
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointHi);
            break;
        case DataZoneControls::AverageMethod::OBJ:
            ObjectCount = 0;
            SetPointLo = 0.0;
            SetPointHi = 0.0;
            for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                if (ActualZoneNum == state.dataHeatBal->People(PeopleNum).ZonePtr) {
                    ++ObjectCount;
                    GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                    SetPointLo += Tset;
                    if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand) {
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                        SetPointHi += Tset;
                    }
                }
            }
            SetPointLo /= ObjectCount;
            if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand) SetPointHi /= ObjectCount;
            break;
        case DataZoneControls::AverageMethod::PEO:
            PeopleCount = 0.0;
            SetPointLo = 0.0;
            SetPointHi = 0.0;
            for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                if (ActualZoneNum == state.dataHeatBal->People(PeopleNum).ZonePtr) {
                    NumberOccupants = state.dataHeatBal->People(PeopleNum).NumberOfPeople *
                                      GetCurrentScheduleValue(state, state.dataHeatBal->People(PeopleNum).NumberOfPeoplePtr);
                    PeopleCount += NumberOccupants;
                    GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                    SetPointLo += Tset * NumberOccupants;
                    if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand) {
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                        SetPointHi += Tset * NumberOccupants;
                    }
                }
            }
            if (PeopleCount > 0) {
                SetPointLo /= PeopleCount;
                if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand) SetPointHi /= PeopleCount;
            } else {
                if (ComfortControlledZone(RelativeZoneNum).PeopleAverageErrIndex == 0) {
                    ShowWarningMessage(state,
                                       "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " + Zone(ActualZoneNum).Name +
                                           " is zero. The People Average option is not used.");
                    ShowContinueError(state, "The Object Average option is used instead. Simulation continues .....");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " +
                                                   Zone(ActualZoneNum).Name + " is still zero. The People Average option is not used",
                                               ComfortControlledZone(RelativeZoneNum).PeopleAverageErrIndex,
                                               PeopleCount,
                                               PeopleCount);
                ObjectCount = 0;
                SetPointLo = 0.0;
                SetPointHi = 0.0;
                for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                    if (ActualZoneNum == state.dataHeatBal->People(PeopleNum).ZonePtr) {
                        ++ObjectCount;
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                        SetPointLo += Tset;
                        if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand) {
                            GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                            SetPointHi += Tset;
                        }
                    }
                }
                SetPointLo /= ObjectCount;
                if (ComfortControlType(ActualZoneNum) == DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand) SetPointHi /= ObjectCount;
            }
            break;
        default:
            break;
        }

        // Assign setpoint
        switch (ComfortControlType(ActualZoneNum)) {
        case DataHVACGlobals::ThermostatType::Uncontrolled:
            switch (TempControlType(ActualZoneNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                ZoneThermostatSetPointHi(ActualZoneNum) = 0.0;
                break;
            case DataHVACGlobals::ThermostatType::SingleCooling:
                ZoneThermostatSetPointLo(ActualZoneNum) = 0.0;
                break;
            default:
                break;
            }
            break;
        case DataHVACGlobals::ThermostatType::SingleHeating:
            if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                if (ComfortControlledZone(RelativeZoneNum).TdbMinErrIndex < 2) {
                    ShowWarningMessage(state,
                                       "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is below the Minimum dry-bulb "
                                       "temperature setpoint " +
                                           ComfortControlledZone(RelativeZoneNum).Name);
                    ShowContinueError(state, "The zone heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is still below the "
                                               "Minimum dry-bulb temperature setpoint ...",
                                               ComfortControlledZone(RelativeZoneNum).TdbMinErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
            ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            TempControlType(ActualZoneNum) = DataHVACGlobals::ThermostatType::SingleHeating;
            TempControlTypeRpt(ActualZoneNum) = static_cast<int>(TempControlType(ActualZoneNum));
            break;
        case DataHVACGlobals::ThermostatType::SingleCooling:
            if (SetPointLo > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                if (ComfortControlledZone(RelativeZoneNum).TdbMaxErrIndex == 0) {
                    ShowWarningMessage(state,
                                       "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is above the Maximum dry-bulb "
                                       "temperature setpoint " +
                                           ComfortControlledZone(RelativeZoneNum).Name);
                    ShowContinueError(state, "The zone cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is still above the "
                                               "Maximum dry-bulb temperature setpoint ...",
                                               ComfortControlledZone(RelativeZoneNum).TdbMaxErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
            ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            TempControlType(ActualZoneNum) = DataHVACGlobals::ThermostatType::SingleCooling;
            TempControlTypeRpt(ActualZoneNum) = static_cast<int>(TempControlType(ActualZoneNum));
            break;
        case DataHVACGlobals::ThermostatType::SingleHeatCool:
            if (ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint == ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
            }
            if (SetPointLo > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint)
                SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
            if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint)
                SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
            if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint ||
                SetPointLo > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                if (ComfortControlledZone(RelativeZoneNum).TdbHCErrIndex == 0) {
                    ShowWarningMessage(state,
                                       "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is above the Maximum or "
                                       "below the Minimum dry-bulb temperature setpoint " +
                                           ComfortControlledZone(RelativeZoneNum).Name);
                    ShowContinueError(state,
                                      "The zone setpoint is set to the Maximum dry-bulb temperature setpoint if above or the Minimum "
                                      "dry-bulb temperature setpoint if below");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is still beyond "
                                               "the range between Maximum and Minimum dry-bulb temperature setpoint ...",
                                               ComfortControlledZone(RelativeZoneNum).TdbHCErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
            ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            TempControlType(ActualZoneNum) = DataHVACGlobals::ThermostatType::SingleHeatCool;
            TempControlTypeRpt(ActualZoneNum) = static_cast<int>(TempControlType(ActualZoneNum));
            break;
        case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
            if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;

                if (ComfortControlledZone(RelativeZoneNum).TdbDualMinErrIndex == 0) {
                    ShowWarningMessage(state,
                                       "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is below the Minimum dry-bulb "
                                       "temperature setpoint " +
                                           ComfortControlledZone(RelativeZoneNum).Name);
                    ShowContinueError(state, "The zone dual heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still below the Minimum "
                                               "dry-bulb temperature setpoint ...",
                                               ComfortControlledZone(RelativeZoneNum).TdbDualMinErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            if (SetPointHi > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                SetPointHi = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                //          ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount =
                //          ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount + 1
                if (ComfortControlledZone(RelativeZoneNum).TdbDualMaxErrIndex == 0) {
                    ShowWarningMessage(state,
                                       "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is above the Maximum dry-bulb "
                                       "temperature setpoint " +
                                           ComfortControlledZone(RelativeZoneNum).Name);
                    ShowContinueError(state, "The zone dual cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still above the Maximum "
                                               "dry-bulb temperature setpoint ...",
                                               ComfortControlledZone(RelativeZoneNum).TdbDualMaxErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }

            ZoneThermostatSetPointLo(ActualZoneNum) = SetPointLo;
            ZoneThermostatSetPointHi(ActualZoneNum) = SetPointHi;
            TempControlType(ActualZoneNum) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;
            TempControlTypeRpt(ActualZoneNum) = static_cast<int>(TempControlType(ActualZoneNum));
            break;
        default:
            ShowSevereError(state,
                            format("CalcZoneAirComfortSetpoints: Illegal thermal control control type for Zone={}, Found value={}, in Schedule={}",
                                   Zone(ActualZoneNum).Name,
                                   ComfortControlTypeRpt(ActualZoneNum),
                                   ComfortControlledZone(ActualZoneNum).ControlTypeSchedName));
        }
    }
}

void GetComfortSetPoints(EnergyPlusData &state,
                         int const PeopleNum,
                         int const ComfortControlNum,
                         Real64 const PMVSet,
                         Real64 &Tset // drybulb setpoint temperature for a given PMV value
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May, 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na
    // PURPOSE OF THIS SUBROUTINE:

    // This routine sets what the thermal comfort setpoints for each controlled zone should be based on air tempeature
    // obtained from thermal comfort models.
    // This is called each time step.

    // Using/Aliasing
    using General::SolveRoot;
    using ThermalComfort::CalcThermalComfortFanger;

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // 0 = Solution; 1 = Set to Min; 2 Set to Max

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr Acc(0.001); // accuracy control for SolveRoot
    int constexpr MaxIter(500);  // iteration control for SolveRoot

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 Tmin;            // Minimum drybulb setpoint temperature
    Real64 Tmax;            // Minimum drybulb setpoint temperature
    Real64 PMVResult;       // Calculated PMV value
    Real64 PMVMin;          // Minimum allowed PMV value
    Real64 PMVMax;          // Calculated PMV value
    Array1D<Real64> Par(2); // Passed parameter for RegularFalsi function
    int SolFla;             // feed back flag from SolveRoot

    auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone;

    Tmin = ComfortControlledZone(ComfortControlNum).TdbMinSetPoint;
    Tmax = ComfortControlledZone(ComfortControlNum).TdbMaxSetPoint;

    CalcThermalComfortFanger(state, PeopleNum, Tmin, PMVResult);
    PMVMin = PMVResult;
    CalcThermalComfortFanger(state, PeopleNum, Tmax, PMVResult);
    PMVMax = PMVResult;
    if (PMVSet > PMVMin && PMVSet < PMVMax) {
        Par(1) = PMVSet;
        Par(2) = double(PeopleNum);
        General::SolveRoot(state, Acc, MaxIter, SolFla, Tset, PMVResidual, Tmin, Tmax, Par);
        if (SolFla == -1) {
            if (!state.dataGlobal->WarmupFlag) {
                ++state.dataZoneTempPredictorCorrector->IterLimitExceededNum1;
                if (state.dataZoneTempPredictorCorrector->IterLimitExceededNum1 == 1) {
                    ShowWarningError(state,
                                     ComfortControlledZone(ComfortControlNum).Name +
                                         ": Iteration limit exceeded calculating thermal comfort Fanger setpoint and non-converged setpoint is used");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   ComfortControlledZone(ComfortControlNum).Name +
                                                       ":  Iteration limit exceeded calculating thermal comfort setpoint.",
                                                   state.dataZoneTempPredictorCorrector->IterLimitErrIndex1,
                                                   Tset,
                                                   Tset);
                }
            }
        } else if (SolFla == -2) {
            if (!state.dataGlobal->WarmupFlag) {
                ++state.dataZoneTempPredictorCorrector->IterLimitExceededNum2;
                if (state.dataZoneTempPredictorCorrector->IterLimitExceededNum2 == 1) {
                    ShowWarningError(state,
                                     ComfortControlledZone(ComfortControlNum).Name +
                                         ": Solution is not found in calculating thermal comfort Fanger setpoint and the minimum setpoint is used");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   ComfortControlledZone(ComfortControlNum).Name +
                                                       ":  Solution is not found in  calculating thermal comfort Fanger setpoint.",
                                                   state.dataZoneTempPredictorCorrector->IterLimitErrIndex2,
                                                   Tset,
                                                   Tset);
                }
            }
        }
    } else if (PMVSet < PMVMin) {
        Tset = Tmin;
    } else if (PMVSet > PMVMax) {
        Tset = Tmax;
    }
}

Real64 PMVResidual(EnergyPlusData &state,
                   Real64 const Tset,
                   Array1D<Real64> const &Par // par(1) = PMV set point
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   May 2006
    //       MODIFIED       L.Gu, May 2006
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function (desired PMV value - actual PMV value) for thermal comfort control.

    // METHODOLOGY EMPLOYED:
    //  Calls CalcThermalComfortFanger to get PMV value at the given zone and people conditions
    //  and calculates the residual as defined above

    // Using/Aliasing
    using ThermalComfort::CalcThermalComfortFanger;

    // Return value
    Real64 PMVResidual;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PeopleNum;    // index of people object
    Real64 PMVresult; // resulting PMV values

    PeopleNum = int(Par(2));
    CalcThermalComfortFanger(state, PeopleNum, Tset, PMVresult);
    PMVResidual = Par(1) - PMVresult;
    return PMVResidual;
}

void AdjustCoolingSetPointforTempAndHumidityControl(EnergyPlusData &state,
                                                    int const TempControlledZoneID,
                                                    int const ActualZoneNum // controlled zone actual zone number
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket A Nigusse, FSEC/UCF
    //       DATE WRITTEN   Nov 2010
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine modifies the air cooling setpoint temperature to effect zone air Temperature
    //  and humidity control
    // METHODOLOGY EMPLOYED:
    //  Alter the zone air cooling setpoint if the zone air relative humidity value exceeds the
    //  the zone dehumidifying relative humidity setpoint.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MaxAllowedOvercoolRange; // Maximum allowed zone overcool range [DeltaC]
    Real64 RelativeHumidityDiff;    // Difference between zone air relative humidity and dehumidifying setpoint [%RH]
    Real64 ZoneOvercoolRange;
    Real64 ZoneOvercoolControlRatio;

    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;

    if (!(state.dataZoneCtrls->AnyZoneTempAndHumidityControl)) return; // do nothing to setpoint

    if (!(TempControlledZone(TempControlledZoneID).ZoneOvercoolControl)) return; // do nothing to setpoint

    if (TempControlledZone(TempControlledZoneID).OvercoolCntrlModeScheduled) {
        ZoneOvercoolRange = GetCurrentScheduleValue(state, TempControlledZone(TempControlledZoneID).ZoneOvercoolRangeSchedIndex);
    } else {
        ZoneOvercoolRange = TempControlledZone(TempControlledZoneID).ZoneOvercoolConstRange;
    }
    ZoneOvercoolControlRatio = TempControlledZone(TempControlledZoneID).ZoneOvercoolControlRatio;

    // For Dual Setpoint thermostat the overcool range is limited by the temperature difference between cooling
    // and heating setpoints
    MaxAllowedOvercoolRange = ZoneThermostatSetPointHi(ActualZoneNum) - state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
    if (MaxAllowedOvercoolRange > 0.0) {
        ZoneOvercoolRange = min(ZoneOvercoolRange, MaxAllowedOvercoolRange);
    }
    // Calculate difference between zone air relative humidity and the dehumidifying setpoint
    RelativeHumidityDiff = state.dataZoneTempPredictorCorrector->ZoneAirRelHum(ActualZoneNum) -
                           GetCurrentScheduleValue(state, TempControlledZone(TempControlledZoneID).DehumidifyingSchedIndex);
    if (RelativeHumidityDiff > 0.0 && ZoneOvercoolControlRatio > 0.0) {
        // proportionally reset the cooling setpoint temperature downward (zone Overcool)
        ZoneOvercoolRange = min(ZoneOvercoolRange, RelativeHumidityDiff / ZoneOvercoolControlRatio);
        ZoneThermostatSetPointHi(ActualZoneNum) -= ZoneOvercoolRange;
    }
}

void OverrideAirSetPointsforEMSCntrl(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         L. Gu
    //       DATE WRITTEN   June 2017
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine overrides the air temperature setpoint based on EMS

    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone;
    auto &TempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint;
    auto &TempControlType = state.dataHeatBalFanSys->TempControlType;
    auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone;
    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;

    for (int Loop = 1; Loop <= state.dataZoneCtrls->NumTempControlledZones; ++Loop) {
        if (TempControlledZone(Loop).EMSOverrideHeatingSetPointOn) {
            int ZoneNum = TempControlledZone(Loop).ActualZoneNum;

            switch (TempControlType(ZoneNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                break;
            default:
                break;
            }
        }
        if (TempControlledZone(Loop).EMSOverrideCoolingSetPointOn) {
            int ZoneNum = TempControlledZone(Loop).ActualZoneNum;

            switch (TempControlType(ZoneNum)) {
            case DataHVACGlobals::ThermostatType::SingleCooling:
                TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                break;
            default:
                break;
            }
        }
    }

    for (int Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
        if (ComfortControlledZone(Loop).EMSOverrideHeatingSetPointOn) {
            int ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;
            switch (state.dataHeatBalFanSys->ComfortControlType(ZoneNum)) {
            case DataHVACGlobals::ThermostatType::SingleHeating:
                TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                break;
            default:
                break;
            }
        }

        if (ComfortControlledZone(Loop).EMSOverrideCoolingSetPointOn) {
            int ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;
            switch (static_cast<DataHVACGlobals::ThermostatType>(state.dataHeatBalFanSys->ComfortControlType(ZoneNum))) {
            case DataHVACGlobals::ThermostatType::SingleCooling:
                TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::SingleHeatCool:
                TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                break;
            case DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                break;
            default:
                break;
            }
        }
    }
}

// add values to the LEED tabular report related to schedules used by the thermostat objects
void FillPredefinedTableOnThermostatSetpoints(EnergyPlusData &state)
{
    // J.Glazer - Aug 2017
    using namespace OutputReportPredefined;
    std::vector<int> uniqSch;
    uniqSch.reserve(
        state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls + state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls +
        state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls + state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls * 2);
    Real64 setPointAt11;
    Real64 setPointAt23;
    int numDays;
    std::string monthAssumed;
    std::string monthAssumed2;
    constexpr int wednesday = 4;

    auto &SetPointSingleHeating = state.dataZoneTempPredictorCorrector->SetPointSingleHeating;
    auto &SetPointSingleCooling = state.dataZoneTempPredictorCorrector->SetPointSingleCooling;
    auto &SetPointDualHeatCool = state.dataZoneTempPredictorCorrector->SetPointDualHeatCool;

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls; ++idx) {
        auto &singleHtgSetpoint = SetPointSingleHeating(idx);
        if (std::find(uniqSch.begin(), uniqSch.end(), singleHtgSetpoint.TempSchedIndex) == uniqSch.end()) {
            uniqSch.emplace_back(singleHtgSetpoint.TempSchedIndex);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed, singleHtgSetpoint.TempSchedName, singleHtgSetpoint.Name);

            std::tie(setPointAt11, numDays, monthAssumed) = temperatureAndCountInSch(state, singleHtgSetpoint.TempSchedIndex, false, wednesday, 11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, singleHtgSetpoint.TempSchedName, setPointAt11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, singleHtgSetpoint.TempSchedName, numDays);

            std::tie(setPointAt23, numDays, monthAssumed) = temperatureAndCountInSch(state, singleHtgSetpoint.TempSchedIndex, false, wednesday, 23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, singleHtgSetpoint.TempSchedName, setPointAt23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, singleHtgSetpoint.TempSchedName, numDays);

            PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, singleHtgSetpoint.TempSchedName, monthAssumed);
        }
    }
    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls; ++idx) {
        auto &singleClgSetpoint = SetPointSingleCooling(idx);
        if (std::find(uniqSch.begin(), uniqSch.end(), singleClgSetpoint.TempSchedIndex) == uniqSch.end()) {
            uniqSch.emplace_back(singleClgSetpoint.TempSchedIndex);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed, singleClgSetpoint.TempSchedName, singleClgSetpoint.Name);

            std::tie(setPointAt11, numDays, monthAssumed) = temperatureAndCountInSch(state, singleClgSetpoint.TempSchedIndex, true, wednesday, 11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, singleClgSetpoint.TempSchedName, setPointAt11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, singleClgSetpoint.TempSchedName, numDays);

            std::tie(setPointAt23, numDays, monthAssumed) = temperatureAndCountInSch(state, singleClgSetpoint.TempSchedIndex, true, wednesday, 23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, singleClgSetpoint.TempSchedName, setPointAt23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, singleClgSetpoint.TempSchedName, numDays);

            PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, singleClgSetpoint.TempSchedName, monthAssumed);
        }
    }
    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls; ++idx) {
        auto &singleHeatCoolSetpoint = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(idx);
        if (std::find(uniqSch.begin(), uniqSch.end(), singleHeatCoolSetpoint.TempSchedIndex) == uniqSch.end()) {
            uniqSch.emplace_back(singleHeatCoolSetpoint.TempSchedIndex);
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed, singleHeatCoolSetpoint.TempSchedName, singleHeatCoolSetpoint.Name);

            std::string schNm = singleHeatCoolSetpoint.TempSchedName + " (summer)";
            std::tie(setPointAt11, numDays, monthAssumed) =
                temperatureAndCountInSch(state, singleHeatCoolSetpoint.TempSchedIndex, true, wednesday, 11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, schNm, setPointAt11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, schNm, numDays);

            std::tie(setPointAt23, numDays, monthAssumed) =
                temperatureAndCountInSch(state, singleHeatCoolSetpoint.TempSchedIndex, true, wednesday, 23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, schNm, setPointAt23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, schNm, numDays);

            schNm = singleHeatCoolSetpoint.TempSchedName + " (winter)";
            std::tie(setPointAt11, numDays, monthAssumed2) =
                temperatureAndCountInSch(state, singleHeatCoolSetpoint.TempSchedIndex, false, wednesday, 11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, schNm, setPointAt11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, schNm, numDays);

            std::tie(setPointAt23, numDays, monthAssumed2) =
                temperatureAndCountInSch(state, singleHeatCoolSetpoint.TempSchedIndex, false, wednesday, 23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, schNm, setPointAt23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, schNm, numDays);

            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed,
                             singleHeatCoolSetpoint.TempSchedName,
                             monthAssumed + " and " + monthAssumed2);
        }
    }
    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls; ++idx) {
        auto &dualHeatCoolSetpoint = SetPointDualHeatCool(idx);
        if (std::find(uniqSch.begin(), uniqSch.end(), dualHeatCoolSetpoint.HeatTempSchedIndex) == uniqSch.end()) {
            uniqSch.emplace_back(dualHeatCoolSetpoint.HeatTempSchedIndex);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                             dualHeatCoolSetpoint.HeatTempSetptSchedName,
                             dualHeatCoolSetpoint.Name);

            std::tie(setPointAt11, numDays, monthAssumed) =
                temperatureAndCountInSch(state, dualHeatCoolSetpoint.HeatTempSchedIndex, false, wednesday, 11);
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, dualHeatCoolSetpoint.HeatTempSetptSchedName, setPointAt11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, dualHeatCoolSetpoint.HeatTempSetptSchedName, numDays);

            std::tie(setPointAt23, numDays, monthAssumed) =
                temperatureAndCountInSch(state, dualHeatCoolSetpoint.HeatTempSchedIndex, false, wednesday, 23);
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, dualHeatCoolSetpoint.HeatTempSetptSchedName, setPointAt23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, dualHeatCoolSetpoint.HeatTempSetptSchedName, numDays);

            PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, dualHeatCoolSetpoint.HeatTempSetptSchedName, monthAssumed);
        }
        if (std::find(uniqSch.begin(), uniqSch.end(), dualHeatCoolSetpoint.CoolTempSchedIndex) == uniqSch.end()) {
            uniqSch.emplace_back(dualHeatCoolSetpoint.CoolTempSchedIndex);
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                             dualHeatCoolSetpoint.CoolTempSetptSchedName,
                             dualHeatCoolSetpoint.Name);

            std::tie(setPointAt11, numDays, monthAssumed) =
                temperatureAndCountInSch(state, dualHeatCoolSetpoint.CoolTempSchedIndex, true, wednesday, 11);
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, dualHeatCoolSetpoint.CoolTempSetptSchedName, setPointAt11);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, dualHeatCoolSetpoint.CoolTempSetptSchedName, numDays);

            std::tie(setPointAt23, numDays, monthAssumed) =
                temperatureAndCountInSch(state, dualHeatCoolSetpoint.CoolTempSchedIndex, true, wednesday, 23);
            PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, dualHeatCoolSetpoint.CoolTempSetptSchedName, setPointAt23);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, dualHeatCoolSetpoint.CoolTempSetptSchedName, numDays);

            PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, dualHeatCoolSetpoint.CoolTempSetptSchedName, monthAssumed);
        }
    }
}

// returns the temperature value from a schedule at a certain time for the first day of the week in either January or July
std::tuple<Real64, int, std::string>
temperatureAndCountInSch(EnergyPlusData &state, int const scheduleIndex, bool const isSummer, int const dayOfWeek, int const hourOfDay)
{
    // J.Glazer - Aug 2017

    // determine month to use based on hemiphere and season
    int monthToUse;
    if (isSummer) {
        if (state.dataEnvrn->Latitude > 0.) {
            monthToUse = 7; // July - summer in northern hemisphere
        } else {
            monthToUse = 1; // January - summer in southern hemisphere
        }
    } else {
        if (state.dataEnvrn->Latitude > 0.) {
            monthToUse = 1; // January - winter in northern hemisphere
        } else {
            monthToUse = 7; // July - winter in southern hemisphere
        }
    }
    std::string monthName;
    if (monthToUse == 1) {
        monthName = "January";
    } else {
        monthName = "July";
    }

    int jdateSelect = General::nthDayOfWeekOfMonth(state, dayOfWeek, 1, monthToUse);

    // determine number of days in year
    int DaysInYear;
    if (state.dataEnvrn->CurrentYearIsLeapYear) {
        DaysInYear = 366;
    } else {
        DaysInYear = 365;
    }

    // should adjust date if lands on a holiday but for now assume that it does not

    // adjust time of day for daylight savings time
    int hourSelect = hourOfDay + state.dataWeatherManager->DSTIndex(jdateSelect);

    // get the value at the selected time
    int constexpr firstTimeStep = 1;
    int weekSchIndexSelect = state.dataScheduleMgr->Schedule(scheduleIndex).WeekSchedulePointer(jdateSelect);
    int daySchIndexSelect = state.dataScheduleMgr->WeekSchedule(weekSchIndexSelect).DaySchedulePointer(dayOfWeek);
    Real64 valueAtSelectTime = state.dataScheduleMgr->DaySchedule(daySchIndexSelect).TSValue(firstTimeStep, hourSelect);
    int countOfSame = 0;

    // count the number of times with that same value
    for (int jdateOfYear = 1; jdateOfYear <= DaysInYear; ++jdateOfYear) {
        int wkSch = state.dataScheduleMgr->Schedule(scheduleIndex).WeekSchedulePointer(jdateOfYear);
        if (wkSch == weekSchIndexSelect) { // if same week schedule can short circuit rest of testing and increment counter
            ++countOfSame;
        } else {
            int daySch = state.dataScheduleMgr->WeekSchedule(wkSch).DaySchedulePointer(dayOfWeek);
            if (daySch == daySchIndexSelect) { // if same day schedule can short circuit rest of testing and increment counter
                ++countOfSame;
            } else {
                Real64 valueAt = state.dataScheduleMgr->DaySchedule(daySch).TSValue(firstTimeStep, hourSelect);
                if (valueAt == valueAtSelectTime) {
                    ++countOfSame;
                }
            }
        }
    }

    return std::make_tuple(valueAtSelectTime, countOfSame, monthName);
}

} // namespace EnergyPlus::ZoneTempPredictorCorrector
