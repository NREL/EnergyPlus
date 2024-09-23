// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataStringGlobals.hh>
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

enum class ZoneControlTypes
{
    Invalid = -1,
    TStat = 1,
    TCTStat = 2,
    OTTStat = 3,
    HStat = 4,
    TandHStat = 5,
    StagedDual = 6,
    Num
};

enum class AdaptiveComfortModel
{
    Invalid = -1,
    ADAP_NONE = 1,
    ASH55_CENTRAL = 2,
    ASH55_UPPER_90 = 3,
    ASH55_UPPER_80 = 4,
    CEN15251_CENTRAL = 5,
    CEN15251_UPPER_I = 6,
    CEN15251_UPPER_II = 7,
    CEN15251_UPPER_III = 8,
    Num
};

static constexpr std::array<std::string_view, static_cast<int>(HVAC::ThermostatType::Num)> ValidControlTypes = {
    "Uncontrolled",
    "ThermostatSetpoint:SingleHeating",
    "ThermostatSetpoint:SingleCooling",
    "ThermostatSetpoint:SingleHeatingOrCooling",
    "ThermostatSetpoint:DualSetpoint"};

static constexpr std::array<std::string_view, static_cast<int>(HVAC::ThermostatType::Num)> ValidControlTypesUC = {
    "UNCONTROLLED",
    "THERMOSTATSETPOINT:SINGLEHEATING",
    "THERMOSTATSETPOINT:SINGLECOOLING",
    "THERMOSTATSETPOINT:SINGLEHEATINGORCOOLING",
    "THERMOSTATSETPOINT:DUALSETPOINT"};

static constexpr std::array<std::string_view, static_cast<int>(HVAC::ThermostatType::Num)> ValidComfortControlTypes = {
    "Uncontrolled",
    "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating",
    "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling",
    "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling",
    "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint"};

static constexpr std::array<std::string_view, static_cast<int>(HVAC::ThermostatType::Num)> ValidComfortControlTypesUC = {
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
        ZoneTempChange = correctZoneAirTemps(state, UseZoneTimeStepHistory);
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
    int SchedMin;
    int SchedMax;
    int ActualZoneNum;
    int SchedTypeIndex;

    int ComfortControlledZoneNum; // The Splitter that you are currently loading input into
    int i;
    int found;
    int NumStageControlledZones; // Number of staged controlled objects

    Array1D_int CTSchedMapToControlledZone;
    Array1D_int CCmSchedMapToControlledZone;
    int Item;
    int Item1;
    int ZLItem;

    struct NeededControlTypes
    {
        // Members 4= the four control types + uncontrolled
        std::array<bool, static_cast<int>(HVAC::ThermostatType::Num)> MustHave = {false, false, false, false, false};
        std::array<bool, static_cast<int>(HVAC::ThermostatType::Num)> DidHave = {false, false, false, false, false};
    };

    struct NeededComfortControlTypes
    {
        // Members 4= the four control types + uncontrolled
        std::array<bool, static_cast<int>(HVAC::ThermostatType::Num)> MustHave = {false, false, false, false, false};
        std::array<bool, static_cast<int>(HVAC::ThermostatType::Num)> DidHave = {false, false, false, false, false};
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
    int NumOfZones = state.dataGlobal->NumOfZones;
    auto &cAlphaArgs = state.dataIPShortCut->cAlphaArgs;
    auto &rNumericArgs = state.dataIPShortCut->rNumericArgs;
    auto &lNumericFieldBlanks = state.dataIPShortCut->lNumericFieldBlanks;
    auto &lAlphaFieldBlanks = state.dataIPShortCut->lAlphaFieldBlanks;
    auto &cAlphaFieldNames = state.dataIPShortCut->cAlphaFieldNames;
    auto &cNumericFieldNames = state.dataIPShortCut->cNumericFieldNames;
    auto &inputProcessor = state.dataInputProcessing->inputProcessor;

    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::TStat));
    // Update Num in state and make local convenience copy
    int NumTStatStatements = state.dataZoneCtrls->NumTStatStatements = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataZoneCtrls->TStatObjects.allocate(NumTStatStatements);

    // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
    state.dataZoneCtrls->NumTempControlledZones = 0;
    for (Item = 1; Item <= NumTStatStatements; ++Item) {
        auto &TStatObjects = state.dataZoneCtrls->TStatObjects(Item);
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        TStatObjects.Name = cAlphaArgs(1);
        Item1 = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
        if (Item1 > 0) {
            TStatObjects.TempControlledZoneStartPtr = state.dataZoneCtrls->NumTempControlledZones + 1;
            ++state.dataZoneCtrls->NumTempControlledZones;
            TStatObjects.NumOfZones = 1;
            TStatObjects.ZoneListActive = false;
            TStatObjects.ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            auto const &ZoneList = state.dataHeatBal->ZoneList(ZLItem);
            TStatObjects.TempControlledZoneStartPtr = state.dataZoneCtrls->NumTempControlledZones + 1;
            state.dataZoneCtrls->NumTempControlledZones += ZoneList.NumOfZones;
            TStatObjects.NumOfZones = ZoneList.NumOfZones;
            TStatObjects.ZoneListActive = true;
            TStatObjects.ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowSevereError(state, format("GetZoneAirSetpoints: Errors with invalid names in {} objects.", cCurrentModuleObject));
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataZoneCtrls->NumTempControlledZones = 0;
    }

    if (state.dataZoneCtrls->NumTempControlledZones > 0) {
        state.dataZoneCtrls->TempControlledZone.allocate(state.dataZoneCtrls->NumTempControlledZones);
        TStatControlTypes.allocate(state.dataZoneCtrls->NumTempControlledZones); // Number of set point types
        CTSchedMapToControlledZone.dimension(state.dataZoneCtrls->NumTempControlledZones, 0);

        TempControlledZoneNum = 0;
        state.dataZoneTempPredictorCorrector->NumOnOffCtrZone = 0;
        for (Item = 1; Item <= NumTStatStatements; ++Item) {
            auto &TStatObjects = state.dataZoneCtrls->TStatObjects(Item);
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
            for (Item1 = 1; Item1 <= TStatObjects.NumOfZones; ++Item1) {
                ++TempControlledZoneNum;
                auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
                if (TStatObjects.ZoneListActive) {
                    auto &ZoneList = state.dataHeatBal->ZoneList(TStatObjects.ZoneOrZoneListPtr);
                    cAlphaArgs(2) = state.dataHeatBal->Zone(ZoneList.Zone(Item1)).Name;
                }
                int ZoneAssigned = Util::FindItemInList(
                    cAlphaArgs(2), state.dataZoneCtrls->TempControlledZone, &DataZoneControls::ZoneTempControls::ZoneName, TempControlledZoneNum - 1);
                if (ZoneAssigned == 0) {
                    TempControlledZone.ZoneName = cAlphaArgs(2);
                    TempControlledZone.ActualZoneNum = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                    if (TempControlledZone.ActualZoneNum == 0) {
                        ShowSevereError(
                            state,
                            format(
                                "{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                        ErrorsFound = true;
                    } else {
                        state.dataHeatBal->Zone(TempControlledZone.ActualZoneNum).TempControlledZoneIndex = TempControlledZoneNum;
                    }
                } else {
                    TempControlledZone.ZoneName = cAlphaArgs(2); // for continuity
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" zone previously assigned.",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cAlphaFieldNames(2),
                                           cAlphaArgs(2)));
                    ShowContinueError(
                        state,
                        format("...Zone was previously assigned to Thermostat=\"{}\".", state.dataZoneCtrls->TempControlledZone(ZoneAssigned).Name));
                    ErrorsFound = true;
                    continue;
                }

                if (!TStatObjects.ZoneListActive) {
                    TempControlledZone.Name = cAlphaArgs(1);
                } else {
                    auto &ZoneList = state.dataHeatBal->ZoneList(TStatObjects.ZoneOrZoneListPtr);
                    CheckCreatedZoneItemName(state,
                                             RoutineName,
                                             cCurrentModuleObject,
                                             state.dataHeatBal->Zone(ZoneList.Zone(Item1)).Name,
                                             ZoneList.MaxZoneNameLength,
                                             TStatObjects.Name,
                                             state.dataZoneCtrls->TempControlledZone,
                                             TempControlledZoneNum - 1,
                                             TempControlledZone.Name,
                                             errFlag);
                    if (errFlag) ErrorsFound = true;
                }

                TempControlledZone.ControlTypeSchedName = cAlphaArgs(3);
                TempControlledZone.CTSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
                if (Item1 == 1) { // only show error on first of several if zone list
                    if (TempControlledZone.CTSchedIndex == 0) {
                        ShowSevereError(
                            state,
                            format(
                                "{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                        ErrorsFound = true;
                    } else {
                        // Check validity of control types.
                        ValidScheduleControlType = CheckScheduleValueMinMax(state, TempControlledZone.CTSchedIndex, ">=", 0.0, "<=", 4.0);
                        if (!ValidScheduleControlType) {
                            ShowSevereError(
                                state,
                                format("{}=\"{}\" invalid range {}=\"{}\"", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
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

                TempControlledZone.NumControlTypes = nint((NumAlphas - 3.0) / 2.0);
                TempControlledZone.ControlType.allocate(TempControlledZone.NumControlTypes);
                TempControlledZone.ControlTypeName.allocate(TempControlledZone.NumControlTypes);
                TempControlledZone.ControlTypeEnum.allocate(TempControlledZone.NumControlTypes);

                for (ControlTypeNum = 1; ControlTypeNum <= TempControlledZone.NumControlTypes; ++ControlTypeNum) {

                    TempControlledZone.ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3));
                    TempControlledZone.ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 3));

                    if (!TempControlledZone.ControlType(ControlTypeNum).empty()) {
                        HVAC::ThermostatType ctrlType =
                            static_cast<HVAC::ThermostatType>(getEnumValue(ValidControlTypesUC, TempControlledZone.ControlType(ControlTypeNum)));
                        TempControlledZone.ControlTypeEnum(ControlTypeNum) = ctrlType;
                        if (ctrlType == HVAC::ThermostatType::Invalid) {
                            ShowSevereError(state,
                                            format("{}=\"{}\" invalid {}=\"{}\"",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)),
                                                   cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3))));
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid {}=\"<blank>\"",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3))));
                        ErrorsFound = true;
                    }
                }
                if (NumNums > 0) {
                    if (rNumericArgs(1) >= 0.0) {
                        TempControlledZone.DeltaTCutSet = rNumericArgs(1);
                        if (rNumericArgs(1) > 0.0) state.dataZoneTempPredictorCorrector->NumOnOffCtrZone++;
                    } else {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                        ShowContinueError(state, "..Allowable values must be greater or equal to 0");
                        ErrorsFound = true;
                    }
                }
                if (TempControlledZone.DeltaTCutSet > 0.0) {
                    for (ControlTypeNum = 1; ControlTypeNum <= TempControlledZone.NumControlTypes; ++ControlTypeNum) {
                        if (Util::SameString(TempControlledZone.ControlType(ControlTypeNum), "ThermostatSetpoint:SingleHeatingOrCooling")) {
                            ShowWarningError(state,
                                             format("{}=\"{}: The choice of Temperature Difference Between Cutout And Setpoint will not be applied "
                                                    "to ThermostatSetpoint:SingleHeatingOrCooling.",
                                                    cCurrentModuleObject,
                                                    cAlphaArgs(1)));
                        }
                    }
                }
            }
        } // NumTStatStatements
    }     // Check on number of TempControlledZones

    cCurrentModuleObject = ValidControlTypesUC[static_cast<int>(HVAC::ThermostatType::SingleHeating)];
    state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointSingleHeating.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls);

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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &singleHtgSetpoint = state.dataZoneTempPredictorCorrector->SetPointSingleHeating(idx);
        singleHtgSetpoint.Name = cAlphaArgs(1);
        singleHtgSetpoint.TempSchedName = cAlphaArgs(2);
        singleHtgSetpoint.TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleHtgSetpoint.TempSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }

    } // SingleTempHeatingControlNum

    cCurrentModuleObject = ValidControlTypesUC[static_cast<int>(HVAC::ThermostatType::SingleCooling)];
    state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointSingleCooling.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls);

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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &singleClgSetpoint = state.dataZoneTempPredictorCorrector->SetPointSingleCooling(idx);
        singleClgSetpoint.Name = cAlphaArgs(1);
        singleClgSetpoint.TempSchedName = cAlphaArgs(2);
        singleClgSetpoint.TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleClgSetpoint.TempSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }

    } // SingleTempCoolingControlNum

    cCurrentModuleObject = ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)];
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
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }

    } // SingleTempHeatCoolControlNum

    cCurrentModuleObject = ValidControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)];
    state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls > 0)
        state.dataZoneTempPredictorCorrector->SetPointDualHeatCool.allocate(state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls);

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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &dualHeatCoolSetpoint = state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(idx);
        dualHeatCoolSetpoint.Name = cAlphaArgs(1);
        dualHeatCoolSetpoint.HeatTempSetptSchedName = cAlphaArgs(2);
        dualHeatCoolSetpoint.HeatTempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (dualHeatCoolSetpoint.HeatTempSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }
        dualHeatCoolSetpoint.CoolTempSetptSchedName = cAlphaArgs(3);
        dualHeatCoolSetpoint.CoolTempSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        if (dualHeatCoolSetpoint.CoolTempSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
            ErrorsFound = true;
        }

    } // DualTempHeatCoolControlNum

    // Finish filling in Schedule pointing indexes
    int setPointObjectArrayIndex;
    for (TempControlledZoneNum = 1; TempControlledZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TempControlledZoneNum) {
        for (int ct = 1; ct <= state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes; ct++) {
            auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
            switch (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeEnum(ct)) {
            case HVAC::ThermostatType::SingleHeating:
                setPointObjectArrayIndex =
                    Util::FindItem(TempControlledZone.ControlTypeName(ct), state.dataZoneTempPredictorCorrector->SetPointSingleHeating);
                TempControlledZone.SchIndx_SingleHeatSetPoint =
                    state.dataZoneTempPredictorCorrector->SetPointSingleHeating(setPointObjectArrayIndex).TempSchedIndex;
                break;
            case HVAC::ThermostatType::SingleCooling:
                setPointObjectArrayIndex =
                    Util::FindItem(TempControlledZone.ControlTypeName(ct), state.dataZoneTempPredictorCorrector->SetPointSingleCooling);
                TempControlledZone.SchIndx_SingleCoolSetPoint =
                    state.dataZoneTempPredictorCorrector->SetPointSingleCooling(setPointObjectArrayIndex).TempSchedIndex;
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                setPointObjectArrayIndex =
                    Util::FindItem(TempControlledZone.ControlTypeName(ct), state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool);
                TempControlledZone.SchIndx_SingleHeatCoolSetPoint =
                    state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(setPointObjectArrayIndex).TempSchedIndex;
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                setPointObjectArrayIndex =
                    Util::FindItem(TempControlledZone.ControlTypeName(ct), state.dataZoneTempPredictorCorrector->SetPointDualHeatCool);
                TempControlledZone.SchIndx_DualSetPointWDeadBandHeat =
                    state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(setPointObjectArrayIndex).HeatTempSchedIndex;
                TempControlledZone.SchIndx_DualSetPointWDeadBandCool =
                    state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(setPointObjectArrayIndex).CoolTempSchedIndex;
                break;
            default:
                assert(false);
            }
        }
    }

    // Now, Check the schedule values/indices for validity

    for (TempControlledZoneNum = 1; TempControlledZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TempControlledZoneNum) {

        auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
        ActualZoneNum = TempControlledZone.ActualZoneNum;
        CTIndex = TempControlledZone.CTSchedIndex;
        if (CTIndex == 0) continue; // error will be caught elsewhere
        SchedMin = GetScheduleMinValue(state, CTIndex);
        SchedMax = GetScheduleMaxValue(state, CTIndex);

        if (SchedMin == 0 && SchedMax == 0) {
            if (FindNumberInList(CTIndex, CTSchedMapToControlledZone, state.dataZoneCtrls->NumTempControlledZones) == 0) {
                ShowSevereError(state, format("Control Type Schedule={}", TempControlledZone.ControlTypeSchedName));
                ShowContinueError(state, "..specifies control type 0 for all entries.");
                ShowContinueError(state, "All zones using this Control Type Schedule have no heating or cooling available.");
            }
            CTSchedMapToControlledZone(TempControlledZoneNum) = CTIndex;
        }

        for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

            int TempIndex = 0;
            switch (static_cast<HVAC::ThermostatType>(ControlTypeNum)) {
            case HVAC::ThermostatType::Uncontrolled:
                break;
            case HVAC::ThermostatType::SingleHeating:
                TempIndex = TempControlledZone.SchIndx_SingleHeatSetPoint;
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::SingleHeating))) {
                        ShowSevereError(state, format("Control Type Schedule={}", TempControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies control type 1 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)]));
                        ShowContinueError(
                            state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            case HVAC::ThermostatType::SingleCooling:
                TempIndex = TempControlledZone.SchIndx_SingleCoolSetPoint;
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::SingleCooling))) {
                        ShowSevereError(state, format("Control Type Schedule={}", TempControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies control type 2 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)]));
                        ShowContinueError(
                            state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                TempIndex = TempControlledZone.SchIndx_SingleHeatCoolSetPoint;
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::SingleHeatCool))) {
                        ShowSevereError(state, format("Schedule={}", TempControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies control type 3 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)]));
                        ShowContinueError(
                            state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                TempIndex = TempControlledZone
                                .SchIndx_DualSetPointWDeadBandHeat; // using "Heat" as a sentinel that dualsetpoint is on this zone control object
                if (TempIndex == 0) {
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand))) {
                        ShowSevereError(state, format("Schedule={}", TempControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies control type 4 ({}) as the control type. Not valid for this zone.",
                                                 ValidControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)]));
                        ShowContinueError(
                            state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            default:
                ShowSevereError(state,
                                format("GetZoneAirSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                       state.dataHeatBal->Zone(ActualZoneNum).Name,
                                       ControlTypeNum,
                                       TempControlledZone.ControlTypeSchedName));
                ShowContinueError(state, "..valid range values are [0,4].");
                ErrorsFound = true;
            }
        }
    }

    for (TempControlledZoneNum = 1; TempControlledZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TempControlledZoneNum) {

        auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
        ActualZoneNum = TempControlledZone.ActualZoneNum;
        CTIndex = TempControlledZone.CTSchedIndex;
        if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

        for (ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum) {
            if (TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum] && TStatControlTypes(TempControlledZoneNum).DidHave[ControlTypeNum])
                continue;

            switch (static_cast<HVAC::ThermostatType>(ControlTypeNum)) {
            case HVAC::ThermostatType::SingleHeating:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", TempControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 1 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)]));
                ShowContinueError(state,
                                  format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                break;
            case HVAC::ThermostatType::SingleCooling:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", TempControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 2 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)]));
                ShowContinueError(state,
                                  format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", TempControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 3 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)]));
                ShowContinueError(state,
                                  format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                if (!TStatControlTypes(TempControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", TempControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 4 ({}) but does not.",
                                         ValidControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)]));
                ShowContinueError(state,
                                  format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TStat)), TempControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", TempControlledZone.ZoneName));
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
        state.dataZoneCtrls->HumidityControlZone.allocate(state.dataZoneCtrls->NumHumidityControlZones);
        state.dataZoneTempPredictorCorrector->HumidityControlZoneUniqueNames.reserve(
            static_cast<unsigned>(state.dataZoneCtrls->NumHumidityControlZones));
    }

    for (HumidControlledZoneNum = 1; HumidControlledZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HumidControlledZoneNum) {
        auto &HumidityControlZone = state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum);
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        HumidityControlZone.ControlName = cAlphaArgs(1);
        GlobalNames::IntraObjUniquenessCheck(state,
                                             cAlphaArgs(2),
                                             cCurrentModuleObject,
                                             cAlphaFieldNames(2),
                                             state.dataZoneTempPredictorCorrector->HumidityControlZoneUniqueNames,
                                             ErrorsFound);

        HumidityControlZone.ZoneName = cAlphaArgs(2);
        HumidityControlZone.ActualZoneNum = Util::FindItem(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (HumidityControlZone.ActualZoneNum == 0) {
            ShowSevereError(state,
                            format("{}=\"{} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        } else {
            state.dataHeatBal->Zone(HumidityControlZone.ActualZoneNum).humidityControlZoneIndex = HumidControlledZoneNum;
        }
        HumidityControlZone.HumidifyingSched = cAlphaArgs(3);
        HumidityControlZone.HumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        if (HumidityControlZone.HumidifyingSchedIndex == 0) {
            ShowSevereError(state,
                            format("{}=\"{} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
            ErrorsFound = true;
        }
        if (NumAlphas == 4) {
            HumidityControlZone.DehumidifyingSched = cAlphaArgs(4);
            HumidityControlZone.DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
            if (HumidityControlZone.DehumidifyingSchedIndex == 0) {
                ShowSevereError(
                    state, format("{}=\"{} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                ErrorsFound = true;
            }
        } else {
            HumidityControlZone.DehumidifyingSched = cAlphaArgs(3);
            HumidityControlZone.DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        }

    } // HumidControlledZoneNum

    // Start to read Thermal comfort control objects
    cCurrentModuleObject = cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat));
    state.dataZoneCtrls->NumComfortTStatStatements = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataZoneCtrls->ComfortTStatObjects.allocate(state.dataZoneCtrls->NumComfortTStatStatements);

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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        Item1 = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
        auto &ComfortTStatObjects = state.dataZoneCtrls->ComfortTStatObjects(Item);
        ComfortTStatObjects.Name = cAlphaArgs(1);
        if (Item1 > 0) {
            ComfortTStatObjects.ComfortControlledZoneStartPtr = state.dataZoneCtrls->NumComfortControlledZones + 1;
            ++state.dataZoneCtrls->NumComfortControlledZones;
            ComfortTStatObjects.NumOfZones = 1;
            ComfortTStatObjects.ZoneListActive = false;
            ComfortTStatObjects.ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            auto const &ZoneList = state.dataHeatBal->ZoneList(ZLItem);
            ComfortTStatObjects.ComfortControlledZoneStartPtr = state.dataZoneCtrls->NumComfortControlledZones + 1;
            state.dataZoneCtrls->NumComfortControlledZones += ZoneList.NumOfZones;
            ComfortTStatObjects.NumOfZones = ZoneList.NumOfZones;
            ComfortTStatObjects.ZoneListActive = true;
            ComfortTStatObjects.ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            errFlag = true;
            ErrorsFound = true;
        }
    }

    if (errFlag) {
        ShowSevereError(state, format("GetZoneAirSetpoints: Errors with invalid names in {} objects.", cCurrentModuleObject));
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataZoneCtrls->NumComfortControlledZones = 0;
    }

    if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
        state.dataZoneCtrls->ComfortControlledZone.allocate(state.dataZoneCtrls->NumComfortControlledZones);
        TComfortControlTypes.allocate(state.dataZoneCtrls->NumComfortControlledZones); // Number of set point types
        CCmSchedMapToControlledZone.dimension(state.dataZoneCtrls->NumComfortControlledZones, 0);

        ComfortControlledZoneNum = 0;
        for (Item = 1; Item <= state.dataZoneCtrls->NumComfortTStatStatements; ++Item) {
            auto &ComfortTStatObjects = state.dataZoneCtrls->ComfortTStatObjects(Item);
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
            for (Item1 = 1; Item1 <= ComfortTStatObjects.NumOfZones; ++Item1) {
                ++ComfortControlledZoneNum;
                auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum);
                if (ComfortTStatObjects.ZoneListActive) {
                    auto &ZoneList = state.dataHeatBal->ZoneList(ComfortTStatObjects.ZoneOrZoneListPtr);
                    cAlphaArgs(2) = state.dataHeatBal->Zone(ZoneList.Zone(Item1)).Name;
                }
                int ZoneAssigned = Util::FindItemInList(cAlphaArgs(2),
                                                        state.dataZoneCtrls->ComfortControlledZone,
                                                        &DataZoneControls::ZoneComfortControls::ZoneName,
                                                        ComfortControlledZoneNum - 1);
                if (ZoneAssigned == 0) {
                    ComfortControlledZone.ZoneName = cAlphaArgs(2);
                    ComfortControlledZone.ActualZoneNum = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                    if (ComfortControlledZone.ActualZoneNum == 0) {
                        ShowSevereError(
                            state,
                            format(
                                "{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                        ErrorsFound = true;
                    }
                } else {
                    ComfortControlledZone.ZoneName = cAlphaArgs(2); // for continuity
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" zone previously assigned.",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cAlphaFieldNames(2),
                                           cAlphaArgs(2)));
                    ShowContinueError(state,
                                      format("...Zone was previously assigned to Thermostat=\"{}\".",
                                             state.dataZoneCtrls->ComfortControlledZone(ZoneAssigned).Name));
                    ErrorsFound = true;
                    continue;
                }

                if (!ComfortTStatObjects.ZoneListActive) {
                    ComfortControlledZone.Name = cAlphaArgs(1);
                } else {
                    auto &ZoneList = state.dataHeatBal->ZoneList(ComfortTStatObjects.ZoneOrZoneListPtr);
                    ComfortControlledZone.Name = state.dataHeatBal->Zone(ZoneList.Zone(Item1)).Name + ' ' + ComfortTStatObjects.Name;
                }

                // Read Fields A3 and A4 for averaging method
                int IZoneCount = 0;
                for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                    if (ComfortControlledZone.ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) {
                        ++IZoneCount;
                    }
                }
                // Could not find a people object for this particular zone
                if (IZoneCount == 0 && ComfortControlledZone.ActualZoneNum > 0) {
                    ShowSevereError(state,
                                    format("{}=\"{} no PEOPLE in {}=\"{}\" - cannot use Comfort Control.",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cAlphaFieldNames(2),
                                           cAlphaArgs(2)));
                    ErrorsFound = true;
                }
                ComfortControlledZone.AverageMethod = DataZoneControls::AverageMethod::NO;
                if (IZoneCount > 1) {
                    ComfortControlledZone.AverageMethodName = cAlphaArgs(3);
                    if (Util::SameString(cAlphaArgs(3), "SpecificObject")) {
                        ComfortControlledZone.AverageMethod = DataZoneControls::AverageMethod::SPE;
                    }
                    if (Util::SameString(cAlphaArgs(3), "ObjectAverage")) {
                        ComfortControlledZone.AverageMethod = DataZoneControls::AverageMethod::OBJ;
                    }
                    if (Util::SameString(cAlphaArgs(3), "PeopleAverage")) {
                        ComfortControlledZone.AverageMethod = DataZoneControls::AverageMethod::PEO;
                    }
                    if (ComfortControlledZone.AverageMethod == DataZoneControls::AverageMethod::NO) {
                        ShowSevereError(
                            state, format("{}=\"{} invalid {}=\"{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                        ShowContinueError(state, "Allowed keys are SpecificObject, ObjectAverage, or PeopleAverage");
                        ErrorsFound = true;
                    }
                    if (ComfortControlledZone.AverageMethod == DataZoneControls::AverageMethod::SPE) {
                        ComfortControlledZone.AverageObjectName = cAlphaArgs(4);
                        if (Util::FindItem(cAlphaArgs(4), state.dataHeatBal->People) == 0) {
                            ShowSevereError(
                                state, format("{}=\"{} invalid {}=\"{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                            ErrorsFound = true;
                        } else {
                            ComfortControlledZone.SpecificObjectNum = Util::FindItem(cAlphaArgs(4), state.dataHeatBal->People);
                        }
                    }
                } else {
                    for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                        if (ComfortControlledZone.ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) break;
                    }
                    ComfortControlledZone.SpecificObjectNum = i;
                }
                // Check values used for thermal comfort calculation
                for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                    if (ComfortControlledZone.ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) {
                        // Check activity level
                        if (state.dataHeatBal->People(i).ActivityLevelPtr > 0) {
                            ValidScheduleControlType =
                                CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).ActivityLevelPtr, ">=", 72.0, "<=", 909.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(state,
                                                "GetPeople Activity Level: Invalid activity level values entered for thermal comfort calculation");
                                ShowContinueError(state,
                                                  format("Outside of range values [72,909], Reference object={}", state.dataHeatBal->People(i).Name));
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(
                                state,
                                format("GetPeople Activity Level: Activity level schedule is not found={}", state.dataHeatBal->People(i).Name));
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                        // Check Work Efficiency
                        if (state.dataHeatBal->People(i).WorkEffPtr > 0) {
                            ValidScheduleControlType = CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).WorkEffPtr, ">=", 0.0, "<=", 1.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(state,
                                                "GetPeople work efficiency: Invalid work efficiency values entered for thermal comfort calculation");
                                ShowContinueError(state,
                                                  format("Outside of range values [0,1], Reference object={}", state.dataHeatBal->People(i).Name));
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(
                                state,
                                format("GetPeople work efficiency: Work efficiency schedule is not found={}", state.dataHeatBal->People(i).Name));
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
                                ShowContinueError(
                                    state, format("Outside of range values [0.0,2.0], Reference object={}", state.dataHeatBal->People(i).Name));
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            format("GetPeople Clothing Insulation: Clothing Insulation schedule is not found={}",
                                                   state.dataHeatBal->People(i).Name));
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                        // Check Air velocity
                        if (state.dataHeatBal->People(i).AirVelocityPtr <= 0) {
                            ShowSevereError(
                                state, format("GetPeople Air Velocity: Air velocity schedule is not found={}", state.dataHeatBal->People(i).Name));
                            ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                            ErrorsFound = true;
                        }
                    }
                }

                // Read Max and Min temperature setpoint
                if (NumNums > 0) {
                    ComfortControlledZone.TdbMinSetPoint = rNumericArgs(1);
                    if (rNumericArgs(1) > 50 || rNumericArgs(1) < 0) {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                        ShowContinueError(state, "..Allowable values must be between 0 C and 50 C");
                        ErrorsFound = true;
                    }
                }
                if (NumNums > 1) {
                    ComfortControlledZone.TdbMaxSetPoint = rNumericArgs(2);
                    if (rNumericArgs(2) > 50 || rNumericArgs(2) < 0) {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(2), rNumericArgs(2)));
                        ShowContinueError(state, "..Allowable values must be between 0 C and 50 C");
                        ErrorsFound = true;
                    }
                }
                // Ensure MaxTemp >= MinTemp
                if (ComfortControlledZone.TdbMinSetPoint > ComfortControlledZone.TdbMaxSetPoint) {
                    ShowSevereError(state, format("{}=\"{}", cCurrentModuleObject, cAlphaArgs(1)));
                    ShowContinueError(state, format("..{} > {}", cNumericFieldNames(1), cNumericFieldNames(2)));
                    ShowContinueError(state, format("..[{:.0T}] > [{:.0T}].", rNumericArgs(1), rNumericArgs(2)));
                    ErrorsFound = true;
                }
                // If MaxTemp = MinTemp, no thermal comfort control
                if (ComfortControlledZone.TdbMinSetPoint == ComfortControlledZone.TdbMaxSetPoint) {
                    ShowSevereError(state, format("{}=\"{}", cCurrentModuleObject, cAlphaArgs(1)));
                    ShowContinueError(state, format("..{} = {}", cNumericFieldNames(1), cNumericFieldNames(2)));
                    ShowContinueError(state, "The zone will be controlled using this dry-bulb temperature setpoint.");
                }
                // read Thermal comfort type schedule name
                ComfortControlledZone.ControlTypeSchedName = cAlphaArgs(5);
                ComfortControlledZone.ComfortSchedIndex = GetScheduleIndex(state, cAlphaArgs(5));
                if (ComfortControlledZone.ComfortSchedIndex == 0) {
                    ShowSevereError(
                        state,
                        format("{}=\"{} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    ErrorsFound = true;
                } else {
                    // Check validity of control types.
                    ValidScheduleControlType = CheckScheduleValueMinMax(state, ComfortControlledZone.ComfortSchedIndex, ">=", 0.0, "<=", 4.0);
                    if (!ValidScheduleControlType) {
                        ShowSevereError(
                            state,
                            format("{}=\"{}\" invalid range {}=\"{}\"", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                        ShowContinueError(state, "..contains values outside of range [0,4].");
                        ErrorsFound = true;
                    }
                }
                ComfortControlledZone.NumControlTypes = nint((NumAlphas - 5.0) / 2.0);
                ComfortControlledZone.ControlType.allocate(ComfortControlledZone.NumControlTypes);
                ComfortControlledZone.ControlTypeName.allocate(ComfortControlledZone.NumControlTypes);
                ComfortControlledZone.ControlTypeSchIndx.allocate(ComfortControlledZone.NumControlTypes);

                for (ControlTypeNum = 1; ControlTypeNum <= ComfortControlledZone.NumControlTypes; ++ControlTypeNum) {
                    ComfortControlledZone.ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5));
                    ComfortControlledZone.ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 5));
                    if (ComfortControlledZone.ControlType(ControlTypeNum) != "") {
                        CTIndex = getEnumValue(ValidComfortControlTypesUC, Util::makeUPPER(ComfortControlledZone.ControlType(ControlTypeNum)));
                        if (CTIndex == 0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\" invalid {}=\"{}\"",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)),
                                                   cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5))));
                            ErrorsFound = true;
                        }
                        if (CTIndex > 4) { // For Fanger control only for the time being
                            ShowSevereError(state,
                                            format("{}=\"{}\" invalid {}=\"{}\"",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)),
                                                   cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5))));
                            ShowContinueError(state, "..Fanger is the only valid model.");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid {}=\"<blank>\"",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5))));
                        ErrorsFound = true;
                    }
                    ComfortControlledZone.ControlTypeSchIndx(ControlTypeNum) = 0;
                }
            }
        } // NumComfortTStatStatements
    }
    // End of Thermal comfort control reading and checking

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)];
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &singleSetpointHtgFanger = state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(idx);
        singleSetpointHtgFanger.Name = cAlphaArgs(1);
        singleSetpointHtgFanger.PMVSchedName = cAlphaArgs(2);
        singleSetpointHtgFanger.PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleSetpointHtgFanger.PMVSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, singleSetpointHtgFanger.PMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(
                    state,
                    format(
                        "{}=\"{}\" invalid PMV values {}=\"{}\" entered.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }
    } // SingleFangerHeatingControlNum

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)];
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &singleSetpointClgFanger = state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(idx);
        singleSetpointClgFanger.Name = cAlphaArgs(1);
        singleSetpointClgFanger.PMVSchedName = cAlphaArgs(2);
        singleSetpointClgFanger.PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleSetpointClgFanger.PMVSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, singleSetpointClgFanger.PMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(
                    state,
                    format(
                        "{}=\"{}\" invalid PMV values {}=\"{}\" entered.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }

    } // SingleFangerCoolingControlNum

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)];
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &singleSetpointHeatCoolFanger = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(idx);
        singleSetpointHeatCoolFanger.Name = cAlphaArgs(1);
        singleSetpointHeatCoolFanger.PMVSchedName = cAlphaArgs(2);
        singleSetpointHeatCoolFanger.PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (singleSetpointHeatCoolFanger.PMVSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, singleSetpointHeatCoolFanger.PMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(
                    state,
                    format(
                        "{}=\"{}\" invalid PMV values {}=\"{}\" entered.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }

    } // SingleFangerHeatCoolControlNum

    cCurrentModuleObject = ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)];
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        auto &dualSetpointHeatCoolFanger = state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(idx);
        dualSetpointHeatCoolFanger.Name = cAlphaArgs(1);
        dualSetpointHeatCoolFanger.HeatPMVSetptSchedName = cAlphaArgs(2);
        dualSetpointHeatCoolFanger.HeatPMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
        if (dualSetpointHeatCoolFanger.HeatPMVSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }
        dualSetpointHeatCoolFanger.CoolPMVSetptSchedName = cAlphaArgs(3);
        dualSetpointHeatCoolFanger.CoolPMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
        if (dualSetpointHeatCoolFanger.CoolPMVSchedIndex == 0) {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
            ErrorsFound = true;
        } else {
            ValidScheduleControlType = CheckScheduleValueMinMax(state, dualSetpointHeatCoolFanger.HeatPMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(
                    state,
                    format(
                        "{}=\"{}\" invalid PMV values {}=\"{}\" entered.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
            ValidScheduleControlType = CheckScheduleValueMinMax(state, dualSetpointHeatCoolFanger.CoolPMVSchedIndex, ">=", -3.0, "<=", 3.0);
            if (!ValidScheduleControlType) {
                ShowSevereError(
                    state,
                    format(
                        "{}=\"{}\" invalid PMV values {}=\"{}\" entered.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                ShowContinueError(state, "..Values outside of range [-3,+3].");
                ErrorsFound = true;
            }
        }

    } // DualFangerHeatCoolControlNum

    // Finish filling in Schedule pointing indexes for Thermal Comfort Control
    for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {
        auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum);

        int ComfortIndex = Util::FindItem(ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)],
                                          ComfortControlledZone.ControlType,
                                          ComfortControlledZone.NumControlTypes);
        ComfortControlledZone.SchIndx_SingleHeating = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone.ControlTypeSchIndx(ComfortIndex) = Util::FindItem(
                ComfortControlledZone.ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(HVAC::ThermostatType::SingleHeating)] = true;
        }

        ComfortIndex = Util::FindItem(ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)],
                                      ComfortControlledZone.ControlType,
                                      ComfortControlledZone.NumControlTypes);
        ComfortControlledZone.SchIndx_SingleCooling = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone.ControlTypeSchIndx(ComfortIndex) = Util::FindItem(
                ComfortControlledZone.ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(HVAC::ThermostatType::SingleCooling)] = true;
        }

        ComfortIndex = Util::FindItem(ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)],
                                      ComfortControlledZone.ControlType,
                                      ComfortControlledZone.NumControlTypes);
        ComfortControlledZone.SchIndx_SingleHeatCool = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone.ControlTypeSchIndx(ComfortIndex) = Util::FindItem(
                ComfortControlledZone.ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)] = true;
        }

        ComfortIndex = Util::FindItem(ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)],
                                      ComfortControlledZone.ControlType,
                                      ComfortControlledZone.NumControlTypes);
        ComfortControlledZone.SchIndx_DualSetPointWithDeadBand = ComfortIndex;
        if (ComfortIndex > 0) {
            ComfortControlledZone.ControlTypeSchIndx(ComfortIndex) =
                Util::FindItem(ComfortControlledZone.ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger);
            TComfortControlTypes(ComfortControlledZoneNum).MustHave[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)] = true;
        }
    }

    // Now, Check the schedule values/indices for validity for Thermal Comfort Control

    for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {
        auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum);

        ActualZoneNum = ComfortControlledZone.ActualZoneNum;
        CTIndex = ComfortControlledZone.ComfortSchedIndex;
        if (CTIndex == 0) continue; // error will be caught elsewhere
        SchedMin = GetScheduleMinValue(state, CTIndex);
        SchedMax = GetScheduleMaxValue(state, CTIndex);

        if (SchedMin == 0 && SchedMax == 0) {
            if (FindNumberInList(CTIndex, CCmSchedMapToControlledZone, state.dataZoneCtrls->NumComfortControlledZones) == 0) {
                ShowWarningError(state, format("Control Type Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                ShowContinueError(state, "..specifies control type 0 for all entries.");
                ShowContinueError(state, "All zones using this Control Type Schedule have no thermal comfort control.");
            }
            CCmSchedMapToControlledZone(ComfortControlledZoneNum) = CTIndex;
        }

        for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

            int ComfortIndex;
            switch (static_cast<HVAC::ThermostatType>(ControlTypeNum)) {
            case HVAC::ThermostatType::Uncontrolled:
                break;
            case HVAC::ThermostatType::SingleHeating:
                ComfortIndex = ComfortControlledZone.SchIndx_SingleHeating;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(HVAC::ThermostatType::SingleHeating)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone.ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)],
                                               ComfortControlledZone.ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::SingleHeating))) {
                        ShowSevereError(state, format("Control Type Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies thermal control type 1 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)]));
                        ShowContinueError(
                            state,
                            format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            case HVAC::ThermostatType::SingleCooling:
                ComfortIndex = ComfortControlledZone.SchIndx_SingleCooling;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(HVAC::ThermostatType::SingleCooling)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone.ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)],
                                               ComfortControlledZone.ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::SingleCooling))) {
                        ShowSevereError(state, format("Control Type Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies thermal control type 2 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)]));
                        ShowContinueError(
                            state,
                            format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                ComfortIndex = ComfortControlledZone.SchIndx_SingleHeatCool;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone.ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)],
                                               ComfortControlledZone.ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::SingleHeatCool))) {
                        ShowSevereError(state, format("Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies thermal control type 3 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)]));
                        ShowContinueError(
                            state,
                            format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                ComfortIndex = ComfortControlledZone.SchIndx_DualSetPointWithDeadBand;
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)] = true;
                if (ComfortIndex != 0) {
                    SchedTypeIndex = ComfortControlledZone.ControlTypeSchIndx(ComfortIndex);
                    if (SchedTypeIndex == 0) {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Could not find {} Schedule={}",
                                               ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)],
                                               ComfortControlledZone.ControlTypeName(ComfortIndex)));
                        ErrorsFound = true;
                    }
                } else { // ComfortIndex = 0
                    if (CheckScheduleValue(state, CTIndex, static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand))) {
                        ShowSevereError(state, format("Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                        ShowContinueError(state,
                                          format("..specifies thermal control type 4 ({}) as the control type. Not valid for this zone.",
                                                 ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)]));
                        ShowContinueError(
                            state,
                            format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                        ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                        ErrorsFound = true;
                    }
                }
                break;
            default:
                ShowSevereError(state,
                                format("GetZoneAirSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                       state.dataHeatBal->Zone(ActualZoneNum).Name,
                                       ControlTypeNum,
                                       ComfortControlledZone.ControlTypeSchedName));
                ShowContinueError(state, "..valid range values are [0,4].");
                ErrorsFound = true;
                break;
            }
        }
    }

    for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {
        auto &ComfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum);

        ActualZoneNum = ComfortControlledZone.ActualZoneNum;
        CTIndex = ComfortControlledZone.ComfortSchedIndex;
        if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

        for (ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum) {
            if (TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum] &&
                TComfortControlTypes(ComfortControlledZoneNum).DidHave[ControlTypeNum])
                continue;

            switch (static_cast<HVAC::ThermostatType>(ControlTypeNum)) {
            case HVAC::ThermostatType::SingleHeating:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 1 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeating)]));
                ShowContinueError(
                    state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                break;
            case HVAC::ThermostatType::SingleCooling:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 2 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleCooling)]));
                ShowContinueError(
                    state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 3 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::SingleHeatCool)]));
                ShowContinueError(
                    state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave[ControlTypeNum]) continue;
                ShowWarningError(state, format("Schedule={}", ComfortControlledZone.ControlTypeSchedName));
                ShowContinueError(state,
                                  format("...should include control type 4 ({}) but does not.",
                                         ValidComfortControlTypes[static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand)]));
                ShowContinueError(
                    state, format("..reference {}={}", cZControlTypes(static_cast<int>(ZoneControlTypes::TCTStat)), ComfortControlledZone.Name));
                ShowContinueError(state, format("..reference ZONE={}", ComfortControlledZone.ZoneName));
                break;
            default:
                break;
            }
        }
    }

    if (allocated(TComfortControlTypes)) TComfortControlTypes.deallocate();

    // Get the Hybrid Model setting inputs
    HybridModel::GetHybridModelZone(state);

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
            auto &Zone = state.dataHeatBal->Zone(ZoneNum);
            Zone.ZoneVolCapMultpSens = ZoneVolCapMultpSens;
            Zone.ZoneVolCapMultpMoist = ZoneVolCapMultpMoist;
            Zone.ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2;
            Zone.ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam;
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
                ZLItem = 0;
                Item1 = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
                if (Item1 > 0) {
                    auto &Zone = state.dataHeatBal->Zone(Item1);
                    Zone.FlagCustomizedZoneCap = true;
                    Zone.ZoneVolCapMultpSens = rNumericArgs(1);
                    Zone.ZoneVolCapMultpMoist = rNumericArgs(2);
                    Zone.ZoneVolCapMultpCO2 = rNumericArgs(3);
                    Zone.ZoneVolCapMultpGenContam = rNumericArgs(4);
                } else if (ZLItem > 0) {
                    auto &ZoneList = state.dataHeatBal->ZoneList(ZLItem);
                    for (int ZonePtrNum = 1; ZonePtrNum < ZoneList.NumOfZones; ZonePtrNum++) {
                        auto &Zone = state.dataHeatBal->Zone(ZoneList.Zone(ZonePtrNum));
                        Zone.FlagCustomizedZoneCap = true;
                        Zone.ZoneVolCapMultpSens = rNumericArgs(1);
                        Zone.ZoneVolCapMultpMoist = rNumericArgs(2);
                        Zone.ZoneVolCapMultpCO2 = rNumericArgs(3);
                        Zone.ZoneVolCapMultpGenContam = rNumericArgs(4);
                    }

                } else {
                    ShowSevereError(
                        state,
                        format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    ErrorsFound = true;
                }
            }
        }

        // Assign default multiplier values to all the other zones
        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ZoneNum++) {
            auto &Zone = state.dataHeatBal->Zone(ZoneNum);
            if (!Zone.FlagCustomizedZoneCap) {
                Zone.ZoneVolCapMultpSens = ZoneVolCapMultpSens;
                Zone.ZoneVolCapMultpMoist = ZoneVolCapMultpMoist;
                Zone.ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2;
                Zone.ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam;
            }
        }

        // Calculate the average multiplier value from all zones
        {
            Real64 ZoneVolCapMultpSens_temp = 0.0;
            Real64 ZoneVolCapMultpMoist_temp = 0.0;
            Real64 ZoneVolCapMultpCO2_temp = 0.0;
            Real64 ZoneVolCapMultpGenContam_temp = 0.0;

            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ZoneNum++) {
                auto const &Zone = state.dataHeatBal->Zone(ZoneNum);
                ZoneVolCapMultpSens_temp += Zone.ZoneVolCapMultpSens;
                ZoneVolCapMultpMoist_temp += Zone.ZoneVolCapMultpMoist;
                ZoneVolCapMultpCO2_temp += Zone.ZoneVolCapMultpCO2;
                ZoneVolCapMultpGenContam_temp += Zone.ZoneVolCapMultpGenContam;
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
            found = Util::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TStatObjects);
            if (found == 0) {
                // It might be in the TempControlledZones
                found = Util::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TempControlledZone);
                if (found == 0) { // throw error
                    ShowSevereError(state,
                                    format("{}={} invalid {} reference not found.",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cZControlTypes(static_cast<int>(ZoneControlTypes::TStat))));
                    ErrorsFound = true;
                } else {
                    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(found);
                    TempControlledZone.OperativeTempControl = true;
                    if (Util::SameString(cAlphaArgs(2), "Scheduled")) {
                        TempControlledZone.OpTempCntrlModeScheduled = true;
                    }
                    if ((!(Util::SameString(cAlphaArgs(2), "Scheduled"))) && (!(Util::SameString(cAlphaArgs(2), "Constant")))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=\"{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                        ErrorsFound = true;
                    }

                    TempControlledZone.FixedRadiativeFraction = rNumericArgs(1);
                    TempControlledZone.OpTempRadiativeFractionSched = GetScheduleIndex(state, cAlphaArgs(3));
                    if ((TempControlledZone.OpTempRadiativeFractionSched == 0) && (TempControlledZone.OpTempCntrlModeScheduled)) { // throw error
                        ShowSevereError(
                            state,
                            format("{}={} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                        ErrorsFound = true;
                    }

                    // check validity of fixed radiative fraction
                    if ((TempControlledZone.FixedRadiativeFraction < 0.0) && (!(TempControlledZone.OpTempCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }
                    if ((TempControlledZone.FixedRadiativeFraction >= 0.9) && (!(TempControlledZone.OpTempCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot >= .9.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }

                    // check schedule min max.
                    if (TempControlledZone.OpTempCntrlModeScheduled) {
                        bool ValidRadFractSched =
                            CheckScheduleValueMinMax(state, TempControlledZone.OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                        if (!ValidRadFractSched) {
                            ShowSevereError(
                                state,
                                format("{}={} invalid values {}=[{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                            ShowContinueError(state, "..Values outside of range [0.0,0.9).");
                            ErrorsFound = true;
                        }
                    }

                    // added Jan, 2017 - Xuan Luo
                    // read adaptive comfort model and calculate adaptive thermal comfort setpoint
                    if (TempControlledZone.OperativeTempControl) {
                        if (NumAlphas >= 4 && !lAlphaFieldBlanks(4)) {
                            int adaptiveComfortModelTypeIndex =
                                Util::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                            if (!adaptiveComfortModelTypeIndex) {
                                ShowSevereError(state,
                                                format("{}={} invalid {}=\"{}\" not found.",
                                                       cCurrentModuleObject,
                                                       cAlphaArgs(1),
                                                       cAlphaFieldNames(4),
                                                       cAlphaArgs(4)));
                                ErrorsFound = true;
                            } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                TempControlledZone.AdaptiveComfortTempControl = true;
                                TempControlledZone.AdaptiveComfortModelTypeIndex =
                                    Util::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                if (!state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.initialized) {
                                    Array1D<Real64> runningAverageASH(state.dataWeather->NumDaysInYear, 0.0);
                                    Array1D<Real64> runningAverageCEN(state.dataWeather->NumDaysInYear, 0.0);
                                    CalculateMonthlyRunningAverageDryBulb(state, runningAverageASH, runningAverageCEN);
                                    CalculateAdaptiveComfortSetPointSchl(state, runningAverageASH, runningAverageCEN);
                                }
                            }
                        }
                    }

                    // CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
                    SetupOutputVariable(state,
                                        "Zone Thermostat Operative Temperature",
                                        Constant::Units::C,
                                        state.dataHeatBal->ZnAirRpt(TempControlledZone.ActualZoneNum).ThermOperativeTemp,
                                        OutputProcessor::TimeStepType::Zone,
                                        OutputProcessor::StoreType::Average,
                                        state.dataHeatBal->Zone(TempControlledZone.ActualZoneNum).Name);
                }
            } else {
                auto const &TStatObjects = state.dataZoneCtrls->TStatObjects(found);
                for (Item = 1; Item <= TStatObjects.NumOfZones; ++Item) {
                    TempControlledZoneNum = TStatObjects.TempControlledZoneStartPtr + Item - 1;
                    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
                    if (state.dataZoneCtrls->NumTempControlledZones == 0) continue;
                    TempControlledZone.OperativeTempControl = true;
                    if (Util::SameString(cAlphaArgs(2), "Scheduled")) {
                        TempControlledZone.OpTempCntrlModeScheduled = true;
                    }
                    if (Item == 1) {
                        if ((!(Util::SameString(cAlphaArgs(2), "Scheduled"))) && (!(Util::SameString(cAlphaArgs(2), "Constant")))) {
                            ShowSevereError(
                                state, format("{}={} invalid {}=\"{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                            ErrorsFound = true;
                        }
                    }

                    TempControlledZone.FixedRadiativeFraction = rNumericArgs(1);
                    TempControlledZone.OpTempRadiativeFractionSched = GetScheduleIndex(state, cAlphaArgs(3));
                    if (Item == 1) {
                        if ((TempControlledZone.OpTempRadiativeFractionSched == 0) && (TempControlledZone.OpTempCntrlModeScheduled)) { // throw error
                            ShowSevereError(
                                state,
                                format(
                                    "{}={} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                            ErrorsFound = true;
                        }
                        // check validity of fixed radiative fraction
                        if ((TempControlledZone.FixedRadiativeFraction < 0.0) && (!(TempControlledZone.OpTempCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                        if ((TempControlledZone.FixedRadiativeFraction >= 0.9) && (!(TempControlledZone.OpTempCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot >= .9.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                        // check schedule min max.
                        if (TempControlledZone.OpTempCntrlModeScheduled) {
                            bool ValidRadFractSched =
                                CheckScheduleValueMinMax(state, TempControlledZone.OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                            if (!ValidRadFractSched) {
                                ShowSevereError(
                                    state,
                                    format(
                                        "{}={} invalid values {}=[{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                                ShowContinueError(state, "..Values outside of range [0.0,0.9).");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // added Jan, 2017 - Xuan Luo
                    // read adaptive comfort model and calculate adaptive thermal comfort setpoint
                    if (TempControlledZone.OperativeTempControl) {
                        if (NumAlphas >= 4 && !lAlphaFieldBlanks(4)) {
                            int adaptiveComfortModelTypeIndex =
                                Util::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                            if (!adaptiveComfortModelTypeIndex) {
                                ShowSevereError(state,
                                                format("{}={} invalid {}=\"{}\" not found.",
                                                       cCurrentModuleObject,
                                                       cAlphaArgs(1),
                                                       cAlphaFieldNames(4),
                                                       cAlphaArgs(4)));
                                ErrorsFound = true;
                            } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                TempControlledZone.AdaptiveComfortTempControl = true;
                                TempControlledZone.AdaptiveComfortModelTypeIndex =
                                    Util::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                if (!state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.initialized) {
                                    Array1D<Real64> runningAverageASH(state.dataWeather->NumDaysInYear, 0.0);
                                    Array1D<Real64> runningAverageCEN(state.dataWeather->NumDaysInYear, 0.0);
                                    CalculateMonthlyRunningAverageDryBulb(state, runningAverageASH, runningAverageCEN);
                                    CalculateAdaptiveComfortSetPointSchl(state, runningAverageASH, runningAverageCEN);
                                }
                            }
                        }
                    }

                    // CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
                    SetupOutputVariable(state,
                                        "Zone Thermostat Operative Temperature",
                                        Constant::Units::C,
                                        state.dataHeatBal->ZnAirRpt(TempControlledZone.ActualZoneNum).ThermOperativeTemp,
                                        OutputProcessor::TimeStepType::Zone,
                                        OutputProcessor::StoreType::Average,
                                        state.dataHeatBal->Zone(TempControlledZone.ActualZoneNum).Name);
                } // TStat Objects Loop
            }     // found thermostat reference
        }         // loop over NumOpTempControlledZones
    }             // NumOpTempControlledZones > 0

    // Overcool dehumidification GetInput starts here
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
            found = Util::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TStatObjects);
            if (found == 0) {
                // It might be in the TempControlledZones
                found = Util::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TempControlledZone);
                if (found == 0) { // throw error
                    ShowSevereError(state,
                                    format("{}={} invalid {} reference not found.",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cZControlTypes(static_cast<int>(ZoneControlTypes::TStat))));
                    ErrorsFound = true;
                } else {
                    TempControlledZoneNum = found;
                    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
                    TempControlledZone.DehumidifyingSched = cAlphaArgs(2);
                    TempControlledZone.DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
                    if (TempControlledZone.DehumidifyingSchedIndex == 0) {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                        ErrorsFound = true;
                    }
                    TempControlledZone.ZoneOvercoolControl = true;
                    if ((Util::SameString(cAlphaArgs(3), "None"))) {
                        TempControlledZone.ZoneOvercoolControl = false;
                    }
                    if (Util::SameString(cAlphaArgs(4), "Scheduled")) {
                        TempControlledZone.OvercoolCntrlModeScheduled = true;
                    }
                    if ((!(Util::SameString(cAlphaArgs(4), "Scheduled"))) && (!(Util::SameString(cAlphaArgs(4), "Constant")))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=\"{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                        ErrorsFound = true;
                    }

                    TempControlledZone.ZoneOvercoolConstRange = rNumericArgs(1);
                    TempControlledZone.ZoneOvercoolRangeSchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                    if ((TempControlledZone.ZoneOvercoolRangeSchedIndex == 0) && (TempControlledZone.OvercoolCntrlModeScheduled)) { // throw error
                        ShowSevereError(
                            state,
                            format("{}={} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                        ErrorsFound = true;
                    }

                    // check validity of zone Overcool constant range
                    if ((TempControlledZone.ZoneOvercoolConstRange < 0.0) && (!(TempControlledZone.OvercoolCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }
                    if ((TempControlledZone.ZoneOvercoolConstRange > 3.0) && (!(TempControlledZone.OvercoolCntrlModeScheduled))) {
                        ShowSevereError(state,
                                        format("{}={} invalid {}=[{:.2T}\" cannot be > 3.0",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ErrorsFound = true;
                    }

                    // check zone Overcool range schedule min/max values.
                    if (TempControlledZone.OvercoolCntrlModeScheduled) {
                        bool ValidZoneOvercoolRangeSched =
                            CheckScheduleValueMinMax(state, TempControlledZone.ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                        if (!ValidZoneOvercoolRangeSched) {
                            ShowSevereError(
                                state,
                                format("{}={} invalid values {}=[{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                            ShowContinueError(state, "..Values outside of range [0.0,3.0].");
                            ErrorsFound = true;
                        }
                    }
                    // check Overcool Control Ratio limits
                    TempControlledZone.ZoneOvercoolControlRatio = rNumericArgs(2);
                    if (TempControlledZone.ZoneOvercoolControlRatio < 0.0) {
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
                auto const &TStatObjects = state.dataZoneCtrls->TStatObjects(found);
                for (Item = 1; Item <= TStatObjects.NumOfZones; ++Item) {
                    TempControlledZoneNum = TStatObjects.TempControlledZoneStartPtr + Item - 1;
                    auto &TempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum);
                    TempControlledZone.DehumidifyingSched = cAlphaArgs(2);
                    TempControlledZone.DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
                    if (TempControlledZone.DehumidifyingSchedIndex == 0) {
                        ShowSevereError(
                            state,
                            format("{}=\"{} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                        ErrorsFound = true;
                    }
                    TempControlledZone.ZoneOvercoolControl = true;
                    if ((Util::SameString(cAlphaArgs(3), "None"))) {
                        TempControlledZone.ZoneOvercoolControl = false;
                    }
                    if (Util::SameString(cAlphaArgs(4), "Scheduled")) {
                        TempControlledZone.OvercoolCntrlModeScheduled = false;
                    }
                    if (Item == 1) {
                        if ((!(Util::SameString(cAlphaArgs(4), "Scheduled"))) && (!(Util::SameString(cAlphaArgs(4), "Constant")))) {
                            ShowSevereError(
                                state, format("{}={} invalid {}=\"{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                            ErrorsFound = true;
                        }
                    }
                    TempControlledZone.ZoneOvercoolConstRange = rNumericArgs(1);
                    TempControlledZone.ZoneOvercoolRangeSchedIndex = GetScheduleIndex(state, cAlphaArgs(6));
                    if (Item == 1) {
                        if ((TempControlledZone.ZoneOvercoolRangeSchedIndex == 0) && (TempControlledZone.OvercoolCntrlModeScheduled)) { // throw error
                            ShowSevereError(
                                state,
                                format(
                                    "{}={} invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                            ErrorsFound = true;
                        }
                        // check validity of zone Overcool constant range
                        if ((TempControlledZone.ZoneOvercoolConstRange < 0.0) && (!(TempControlledZone.OvercoolCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                        if ((TempControlledZone.ZoneOvercoolConstRange > 3.0) && (!(TempControlledZone.OvercoolCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot > 3.0",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                        // check zone Overcool range schedule min/max values.
                        if (TempControlledZone.OvercoolCntrlModeScheduled) {
                            bool ValidZoneOvercoolRangeSched =
                                CheckScheduleValueMinMax(state, TempControlledZone.ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                            if (!ValidZoneOvercoolRangeSched) {
                                ShowSevereError(
                                    state,
                                    format(
                                        "{}={} invalid values {}=[{}\".", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                                ShowContinueError(state, "..Values outside of range [0.0,3.0].");
                                ErrorsFound = true;
                            }
                        }
                    }
                    TempControlledZone.ZoneOvercoolControlRatio = rNumericArgs(2);
                    // check Overcool Control Ratio limits
                    if (Item == 1) {
                        if (TempControlledZone.ZoneOvercoolControlRatio < 0.0) {
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
        Util::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataZoneCtrls->StagedTStatObjects(Item).Name = cAlphaArgs(1);
        Item1 = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
        if (Item1 > 0) {
            state.dataZoneCtrls->StagedTStatObjects(Item).StageControlledZoneStartPtr = state.dataZoneTempPredictorCorrector->NumStageCtrZone + 1;
            ++state.dataZoneTempPredictorCorrector->NumStageCtrZone;
            state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones = 1;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive = false;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            auto const &ZoneList = state.dataHeatBal->ZoneList(ZLItem);
            state.dataZoneCtrls->StagedTStatObjects(Item).TempControlledZoneStartPtr = state.dataZoneTempPredictorCorrector->NumStageCtrZone + 1;
            state.dataZoneTempPredictorCorrector->NumStageCtrZone += ZoneList.NumOfZones;
            state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones = ZoneList.NumOfZones;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive = true;
            state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, format("{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowSevereError(state, format("GetStagedDualSetpoint: Errors with invalid names in {} objects.", cCurrentModuleObject));
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataZoneTempPredictorCorrector->NumStageCtrZone = 0;
    }

    if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
        state.dataZoneCtrls->StageControlledZone.allocate(state.dataZoneTempPredictorCorrector->NumStageCtrZone);
        state.dataZoneCtrls->StageZoneLogic.dimension(NumOfZones, false);

        int StageControlledZoneNum = 0;
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
                    auto &ZoneList = state.dataHeatBal->ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr);
                    cAlphaArgs(2) = state.dataHeatBal->Zone(ZoneList.Zone(Item1)).Name;
                }
                int ZoneAssigned = Util::FindItemInList(cAlphaArgs(2),
                                                        state.dataZoneCtrls->StageControlledZone,
                                                        &DataZoneControls::ZoneStagedControls::ZoneName,
                                                        StageControlledZoneNum - 1);
                auto &stageControlledZone = state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum);
                if (ZoneAssigned == 0) {
                    stageControlledZone.ZoneName = cAlphaArgs(2);
                    stageControlledZone.ActualZoneNum = Util::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                    if (stageControlledZone.ActualZoneNum == 0) {
                        ShowSevereError(
                            state,
                            format(
                                "{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                        ErrorsFound = true;
                    } else {
                        //           Zone(stageControlledZone%ActualZoneNum)%StageControlledZoneIndex =
                        //           StageControlledZoneNum
                    }
                    state.dataZoneCtrls->StageZoneLogic(stageControlledZone.ActualZoneNum) = true;
                } else {
                    stageControlledZone.ZoneName = cAlphaArgs(2); // for continuity
                    ShowSevereError(state,
                                    format("{}=\"{}\" invalid {}=\"{}\" zone previously assigned.",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cAlphaFieldNames(2),
                                           cAlphaArgs(2)));
                    ShowContinueError(
                        state,
                        format("...Zone was previously assigned to Thermostat=\"{}\".", state.dataZoneCtrls->StageControlledZone(ZoneAssigned).Name));
                    ErrorsFound = true;
                    continue;
                }

                if (!state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive) {
                    stageControlledZone.Name = cAlphaArgs(1);
                } else {
                    auto &ZoneList = state.dataHeatBal->ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr);
                    CheckCreatedZoneItemName(state,
                                             RoutineName,
                                             cCurrentModuleObject,
                                             state.dataHeatBal->Zone(ZoneList.Zone(Item1)).Name,
                                             ZoneList.MaxZoneNameLength,
                                             state.dataZoneCtrls->StagedTStatObjects(Item).Name,
                                             state.dataZoneCtrls->StageControlledZone,
                                             StageControlledZoneNum - 1,
                                             stageControlledZone.Name,
                                             errFlag);
                    if (errFlag) ErrorsFound = true;
                }

                stageControlledZone.NumOfHeatStages = rNumericArgs(1);
                if (rNumericArgs(1) < 1 || rNumericArgs(1) > 4) {
                    ShowSevereError(
                        state,
                        format("{}=\"{}\" invalid range {}=\"{:.0R}\"", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                    ShowContinueError(state, "..contains values outside of range [1,4].");
                    ErrorsFound = true;
                }

                stageControlledZone.HeatSetBaseSchedName = cAlphaArgs(3);
                stageControlledZone.HSBchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
                if (Item1 == 1) { // only show error on first of several if zone list
                    if (stageControlledZone.HSBchedIndex == 0) {
                        ShowSevereError(
                            state,
                            format(
                                "{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                        ErrorsFound = true;
                    }
                }

                stageControlledZone.HeatThroRange = rNumericArgs(2);
                if (rNumericArgs(1) < 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" negative value is found at {}=\"{:.1R}\"",
                                           cAlphaArgs(1),
                                           cCurrentModuleObject,
                                           cNumericFieldNames(2),
                                           rNumericArgs(2)));
                    ShowContinueError(state, ".. The minimum value is 0.");
                    ErrorsFound = true;
                }

                if (stageControlledZone.NumOfHeatStages > 0) {
                    stageControlledZone.HeatTOffset.allocate(stageControlledZone.NumOfHeatStages);
                    for (i = 1; i <= stageControlledZone.NumOfHeatStages; ++i) {
                        stageControlledZone.HeatTOffset(i) = rNumericArgs(2 + i);
                        if (rNumericArgs(2 + i) > 0.0) {
                            ShowSevereError(state,
                                            format("{}=\"{}\" positive value is found at {}",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   format("{}=\"{:.1R}\"", cNumericFieldNames(2 + i), rNumericArgs(2 + i))));
                            ShowContinueError(state, ".. The maximum value is 0.");
                            ErrorsFound = true;
                        }
                        if (lNumericFieldBlanks(2 + i)) {
                            ShowSevereError(state,
                                            format("{} object ={}. The input of {} is required, but a blank is found.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(2 + i)));
                            ErrorsFound = true;
                        }
                        if (i > 1) {
                            if (rNumericArgs(2 + i) >= rNumericArgs(1 + i)) {
                                ShowSevereError(state,
                                                format(R"({}="{}" The value at {}="{:.1R}" has to be less than )",
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

                stageControlledZone.NumOfCoolStages = rNumericArgs(7);
                if (rNumericArgs(7) < 1 || rNumericArgs(7) > 4) {
                    ShowSevereError(
                        state,
                        format("{}=\"{}\" invalid range {}=\"{:.0R}\"", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(7), rNumericArgs(7)));
                    ShowContinueError(state, "..contains values outside of range [1,4].");
                    ErrorsFound = true;
                }

                stageControlledZone.CoolSetBaseSchedName = cAlphaArgs(4);
                stageControlledZone.CSBchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                if (Item1 == 1) { // only show error on first of several if zone list
                    if (stageControlledZone.CSBchedIndex == 0) {
                        ShowSevereError(
                            state,
                            format(
                                "{}=\"{}\" invalid {}=\"{}\" not found.", cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                        ErrorsFound = true;
                    }
                }

                stageControlledZone.CoolThroRange = rNumericArgs(8);
                if (rNumericArgs(8) < 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" negative value is found at {}=\"{:.1R}\"",
                                           cCurrentModuleObject,
                                           cAlphaArgs(1),
                                           cNumericFieldNames(8),
                                           rNumericArgs(8)));
                    ShowContinueError(state, ".. The minimum value is 0.");
                    ErrorsFound = true;
                }

                if (stageControlledZone.NumOfCoolStages > 0) {
                    stageControlledZone.CoolTOffset.allocate(stageControlledZone.NumOfCoolStages);
                    for (i = 1; i <= stageControlledZone.NumOfCoolStages; ++i) {
                        stageControlledZone.CoolTOffset(i) = rNumericArgs(8 + i);
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
                                            format("{} object ={}. The input of {} is required, but a blank is found.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(8 + i)));
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
            ShowWarningError(state, format("{} is applicable to only selected HVAC objects which are missing from input.", cCurrentModuleObject));
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
    Real64 dryBulb;
    Real64 avgDryBulb;

    std::string::size_type pos;

    Array1D<Real64> adaptiveTemp(state.dataWeather->NumDaysInYear, 0.0);
    Array1D<Real64> dailyDryTemp(state.dataWeather->NumDaysInYear, 0.0);

    if (FileSystem::fileExists(state.files.inputWeatherFilePath.filePath)) {
        // Read hourly dry bulb temperature first
        auto epwFile = state.files.inputWeatherFilePath.open(state, "CalcThermalComfortAdaptive");
        for (int i = 1; i <= 9; ++i) { // Headers
            epwFile.readLine();
        }
        for (int i = 1; i <= state.dataWeather->NumDaysInYear; ++i) {
            avgDryBulb = 0.0;
            for (int j = 1; j <= 24; ++j) {
                std::string epwLine = epwFile.readLine().data;
                for (int ind = 1; ind <= 6; ++ind) {
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
        while (dayOfYear < state.dataWeather->NumDaysInYear) {
            dayOfYear++;
            int calcEndDay = dayOfYear - 1;
            int calcStartDayASH = calcEndDay - 30;
            int calcStartDayCEN = calcEndDay - 7;

            if (calcStartDayASH > 0) {
                for (int i = calcStartDayASH; i <= calcStartDayASH + 30; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                }
                runningAverageASH(dayOfYear) /= 30;
            } else { // Do special things for wrapping the epw
                calcStartDayASH += state.dataWeather->NumDaysInYear;
                for (int i = 1; i <= calcEndDay; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                }
                for (int i = calcStartDayASH; i < state.dataWeather->NumDaysInYear; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                }
                runningAverageASH(dayOfYear) /= 30;
            }

            if (calcStartDayCEN > 0) {
                for (int i = calcStartDayCEN; i <= calcStartDayCEN + 7; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                }
                runningAverageCEN(dayOfYear) /= 7;
            } else { // Do special things for wrapping the epw
                calcStartDayCEN += state.dataWeather->NumDaysInYear;
                for (int i = 1; i <= calcEndDay; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                }
                for (int i = calcStartDayCEN; i < state.dataWeather->NumDaysInYear; i++) {
                    avgDryBulb = dailyDryTemp(i);
                    runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                }
                runningAverageCEN(dayOfYear) /= 7;
            }
        }
    } else {
        ShowFatalError(state,
                       format("CalcThermalComfortAdaptive: Could not open file {} for input (read). (File does not exist)",
                              state.files.inputWeatherFilePath.filePath));
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

    for (size_t i = 1; i <= state.dataWeather->DesDayInput.size(); i++) {
        // Summer design day
        if (state.dataWeather->DesDayInput(i).DayType == summerDesignDayTypeIndex) {
            GrossApproxAvgDryBulbDesignDay = (state.dataWeather->DesDayInput(i).MaxDryBulb +
                                              (state.dataWeather->DesDayInput(i).MaxDryBulb - state.dataWeather->DesDayInput(i).DailyDBRange)) /
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

    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central.allocate(state.dataWeather->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90.allocate(state.dataWeather->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80.allocate(state.dataWeather->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central.allocate(state.dataWeather->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I.allocate(state.dataWeather->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II.allocate(state.dataWeather->NumDaysInYear);
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III.allocate(state.dataWeather->NumDaysInYear);

    // Calculate the set points based on different models, set flag as -1 when running average temperature is not in the range.
    for (int day = 1; day <= state.dataWeather->NumDaysInYear; day++) {
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
    int NumOfZones = state.dataGlobal->NumOfZones;

    if (state.dataZoneTempPredictorCorrector->InitZoneAirSetPointsOneTimeFlag) {
        int TRefFlag; // Flag for Reference Temperature process in Zones
        state.dataHeatBalFanSys->TempZoneThermostatSetPoint.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneThermostatSetPointHi.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneThermostatSetPointLo.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver.dimension(NumOfZones, 0.0);

        state.dataHeatBalFanSys->LoadCorrectionFactor.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->TempControlType.dimension(NumOfZones, HVAC::ThermostatType::Uncontrolled);
        state.dataHeatBalFanSys->TempControlTypeRpt.dimension(NumOfZones, 0);
        if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
            state.dataHeatBalFanSys->ComfortControlType.dimension(NumOfZones, HVAC::ThermostatType::Uncontrolled);
            state.dataHeatBalFanSys->ComfortControlTypeRpt.dimension(NumOfZones, 0);
            state.dataHeatBalFanSys->ZoneComfortControlsFanger.allocate(NumOfZones);
        }
        state.dataZoneEnergyDemand->Setback.dimension(NumOfZones, false);
        state.dataZoneEnergyDemand->DeadBandOrSetback.dimension(NumOfZones, false);
        state.dataZoneEnergyDemand->CurDeadBandOrSetback.dimension(NumOfZones, false);

        state.dataHeatBal->ZoneListSNLoadHeatEnergy.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
        state.dataHeatBal->ZoneListSNLoadCoolEnergy.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
        state.dataHeatBal->ZoneListSNLoadHeatRate.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
        state.dataHeatBal->ZoneListSNLoadCoolRate.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);

        state.dataHeatBal->ZoneGroupSNLoadHeatEnergy.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBal->ZoneGroupSNLoadCoolEnergy.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBal->ZoneGroupSNLoadHeatRate.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
        state.dataHeatBal->ZoneGroupSNLoadCoolRate.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);

        // Hybrid modeling
        state.dataHeatBalFanSys->PreviousMeasuredZT1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredZT2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredZT3.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat1.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat2.dimension(NumOfZones, 0.0);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat3.dimension(NumOfZones, 0.0);

        // Allocate Derived Types
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(NumOfZones);
        state.dataZoneEnergyDemand->ZoneSysMoistureDemand.allocate(NumOfZones);
        if (state.dataHeatBal->doSpaceHeatBalanceSimulation || state.dataHeatBal->doSpaceHeatBalanceSizing) {
            state.dataZoneEnergyDemand->spaceSysEnergyDemand.allocate(state.dataGlobal->numSpaces);
            state.dataZoneEnergyDemand->spaceSysMoistureDemand.allocate(state.dataGlobal->numSpaces);
        }

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            bool FirstSurfFlag = true;
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                auto const &thisSpace = state.dataHeatBal->space(spaceNum);
                for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                    if (FirstSurfFlag) {
                        TRefFlag = state.dataSurface->SurfTAirRef(SurfNum);
                        FirstSurfFlag = false;
                    }
                    // for each particular zone, the reference air temperature(s) should be the same
                    // (either mean air, bulk air, or supply air temp).
                    if (state.dataSurface->SurfTAirRef(SurfNum) != TRefFlag) {
                        ShowWarningError(state,
                                         format("Different reference air temperatures for difference surfaces encountered in zone {}",
                                                state.dataHeatBal->Zone(zoneNum).Name));
                    }
                }
            }
        }

        // CurrentModuleObject='Zone'
        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            auto &thisZone = state.dataHeatBal->Zone(zoneNum);
            state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).setUpOutputVars(state, DataStringGlobals::zonePrefix, thisZone.Name);
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation) {
                for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                    state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).setUpOutputVars(
                        state, DataStringGlobals::spacePrefix, state.dataHeatBal->space(spaceNum).Name);
                }
            }
            bool staged = false;
            if (allocated(state.dataZoneCtrls->StageZoneLogic)) {
                staged = state.dataZoneCtrls->StageZoneLogic(zoneNum);
            }
            // If not doSpaceHeatBalanceSimulation then meter zones, not spaces
            bool attachMeters = !state.dataHeatBal->doSpaceHeatBalanceSimulation;
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).setUpOutputVars(
                state, DataStringGlobals::zonePrefix, thisZone.Name, staged, attachMeters, thisZone.Multiplier, thisZone.ListMultiplier);
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation) {
                // If doSpaceHeatBalanceSimulation then meter spaces, not zones
                attachMeters = state.dataHeatBal->doSpaceHeatBalanceSimulation;
                for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                    state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).setUpOutputVars(state,
                                                                                               DataStringGlobals::spacePrefix,
                                                                                               state.dataHeatBal->space(spaceNum).Name,
                                                                                               staged,
                                                                                               attachMeters,
                                                                                               thisZone.Multiplier,
                                                                                               thisZone.ListMultiplier);
                }
            }
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(zoneNum).setUpOutputVars(state, DataStringGlobals::zonePrefix, thisZone.Name);
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation) {
                for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                    state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum).setUpOutputVars(
                        state, DataStringGlobals::spacePrefix, state.dataHeatBal->space(spaceNum).Name);
                }
            }
            SetupOutputVariable(state,
                                "Zone Thermostat Air Temperature",
                                Constant::Units::C,
                                state.dataHeatBalFanSys->TempTstatAir(zoneNum),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                thisZone.Name);
            SetupOutputVariable(state,
                                "Zone Thermostat Control Type",
                                Constant::Units::None,
                                state.dataHeatBalFanSys->TempControlTypeRpt(zoneNum),
                                OutputProcessor::TimeStepType::Zone,
                                OutputProcessor::StoreType::Average,
                                thisZone.Name);
            SetupOutputVariable(state,
                                "Zone Thermostat Heating Setpoint Temperature",
                                Constant::Units::C,
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(zoneNum),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                thisZone.Name);
            SetupOutputVariable(state,
                                "Zone Thermostat Cooling Setpoint Temperature",
                                Constant::Units::C,
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(zoneNum),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                thisZone.Name);
            SetupOutputVariable(state,
                                "Zone Adaptive Comfort Operative Temperature Set Point",
                                Constant::Units::C,
                                state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(zoneNum),
                                OutputProcessor::TimeStepType::Zone,
                                OutputProcessor::StoreType::Average,
                                thisZone.Name);
            SetupOutputVariable(state,
                                "Zone Predicted Sensible Load Room Air Correction Factor",
                                Constant::Units::None,
                                state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                thisZone.Name);
        } // zoneNum

        // Thermal comfort control output
        if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
            // CurrentModuleObject='ZoneControl:Thermostat:ThermalComfort'
            for (int Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
                int zoneNum = state.dataZoneCtrls->ComfortControlledZone(Loop).ActualZoneNum;
                auto &thisZone = state.dataHeatBal->Zone(zoneNum);
                SetupOutputVariable(state,
                                    "Zone Thermal Comfort Control Type",
                                    Constant::Units::None,
                                    state.dataHeatBalFanSys->ComfortControlTypeRpt(zoneNum),
                                    OutputProcessor::TimeStepType::Zone,
                                    OutputProcessor::StoreType::Average,
                                    thisZone.Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Comfort Control Fanger Low Setpoint PMV",
                                    Constant::Units::None,
                                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(zoneNum).LowPMV,
                                    OutputProcessor::TimeStepType::Zone,
                                    OutputProcessor::StoreType::Average,
                                    thisZone.Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Comfort Control Fanger High Setpoint PMV",
                                    Constant::Units::None,
                                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(zoneNum).HighPMV,
                                    OutputProcessor::TimeStepType::Zone,
                                    OutputProcessor::StoreType::Average,
                                    thisZone.Name);
            }
        }

        // CurrentModuleObject='ZoneList'
        for (int Loop = 1; Loop <= state.dataHeatBal->NumOfZoneLists; ++Loop) {
            auto &zoneList = state.dataHeatBal->ZoneList(Loop);
            SetupOutputVariable(state,
                                "Zone List Sensible Heating Energy",
                                Constant::Units::J,
                                state.dataHeatBal->ZoneListSNLoadHeatEnergy(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zoneList.Name);
            SetupOutputVariable(state,
                                "Zone List Sensible Cooling Energy",
                                Constant::Units::J,
                                state.dataHeatBal->ZoneListSNLoadCoolEnergy(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zoneList.Name);
            SetupOutputVariable(state,
                                "Zone List Sensible Heating Rate",
                                Constant::Units::W,
                                state.dataHeatBal->ZoneListSNLoadHeatRate(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                zoneList.Name);
            SetupOutputVariable(state,
                                "Zone List Sensible Cooling Rate",
                                Constant::Units::W,
                                state.dataHeatBal->ZoneListSNLoadCoolRate(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                zoneList.Name);
        } // Loop

        // CurrentModuleObject='ZoneGroup'
        for (int Loop = 1; Loop <= state.dataHeatBal->NumOfZoneGroups; ++Loop) {
            auto &zoneGroup = state.dataHeatBal->ZoneGroup(Loop);
            SetupOutputVariable(state,
                                "Zone Group Sensible Heating Energy",
                                Constant::Units::J,
                                state.dataHeatBal->ZoneGroupSNLoadHeatEnergy(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zoneGroup.Name);
            SetupOutputVariable(state,
                                "Zone Group Sensible Cooling Energy",
                                Constant::Units::J,
                                state.dataHeatBal->ZoneGroupSNLoadCoolEnergy(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zoneGroup.Name);
            SetupOutputVariable(state,
                                "Zone Group Sensible Heating Rate",
                                Constant::Units::W,
                                state.dataHeatBal->ZoneGroupSNLoadHeatRate(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                zoneGroup.Name);
            SetupOutputVariable(state,
                                "Zone Group Sensible Cooling Rate",
                                Constant::Units::W,
                                state.dataHeatBal->ZoneGroupSNLoadCoolRate(Loop),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                zoneGroup.Name);
        } // Loop

        state.dataZoneTempPredictorCorrector->InitZoneAirSetPointsOneTimeFlag = false;
    }

    // Do the Begin Environment initializations
    if (state.dataZoneTempPredictorCorrector->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
        for (auto &thisZoneHB : state.dataZoneTempPredictorCorrector->zoneHeatBalance) {
            thisZoneHB.beginEnvironmentInit(state);
        }
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
                thisSpaceHB.beginEnvironmentInit(state);
            }
        }
        state.dataHeatBalFanSys->TempZoneThermostatSetPoint = 0.0;
        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint = 0.0;
        state.dataHeatBalFanSys->ZoneThermostatSetPointHi = 0.0;
        state.dataHeatBalFanSys->ZoneThermostatSetPointLo = 0.0;

        state.dataHeatBalFanSys->LoadCorrectionFactor = 1.0;
        state.dataHeatBalFanSys->TempControlType = HVAC::ThermostatType::Uncontrolled;
        for (auto &e : state.dataZoneEnergyDemand->ZoneSysEnergyDemand) {
            e.beginEnvironmentInit();
        }
        for (auto &e : state.dataZoneEnergyDemand->ZoneSysMoistureDemand) {
            e.beginEnvironmentInit();
        }
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (auto &e : state.dataZoneEnergyDemand->spaceSysEnergyDemand) {
                e.beginEnvironmentInit();
            }
            for (auto &e : state.dataZoneEnergyDemand->spaceSysMoistureDemand) {
                e.beginEnvironmentInit();
            }
        }

        state.dataZoneEnergyDemand->DeadBandOrSetback = false;

        for (auto &e : state.dataHeatBal->Zone)
            e.NoHeatToReturnAir = false;
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

    for (int Loop = 1; Loop <= state.dataZoneCtrls->NumTempControlledZones; ++Loop) {
        auto &tempControlledZone = state.dataZoneCtrls->TempControlledZone(Loop);
        if (state.dataZoneEquip->ZoneEquipInputsFilled && !state.dataZoneTempPredictorCorrector->ControlledZonesChecked) {
            if (!VerifyControlledZoneForThermostat(state, tempControlledZone.ZoneName)) {
                ShowSevereError(state,
                                format("{}Zone=\"{}\" has specified a Thermostatic control but is not a controlled zone.",
                                       RoutineName,
                                       tempControlledZone.ZoneName));
                ShowContinueError(state, "...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                state.dataZoneTempPredictorCorrector->ErrorsFound = true;
            }
        }

        if (tempControlledZone.ManageDemand) {
            int ZoneNum = tempControlledZone.ActualZoneNum;
            auto &zoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum);
            auto &zoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum);
            auto &tempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
            auto &tempControlType = state.dataHeatBalFanSys->TempControlType(ZoneNum);

            switch (tempControlType) {
            case HVAC::ThermostatType::SingleHeating:
                if (tempZoneThermostatSetPoint > tempControlledZone.HeatingResetLimit) {
                    tempZoneThermostatSetPoint = tempControlledZone.HeatingResetLimit;
                    zoneThermostatSetPointLo = tempZoneThermostatSetPoint;
                }
                break;
            case HVAC::ThermostatType::SingleCooling:
                if (tempZoneThermostatSetPoint < tempControlledZone.CoolingResetLimit) {
                    tempZoneThermostatSetPoint = tempControlledZone.CoolingResetLimit;
                    zoneThermostatSetPointHi = tempZoneThermostatSetPoint;
                }
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                if ((tempZoneThermostatSetPoint > tempControlledZone.HeatingResetLimit) ||
                    (tempZoneThermostatSetPoint < tempControlledZone.CoolingResetLimit)) {

                    tempControlType = HVAC::ThermostatType::DualSetPointWithDeadBand;
                    state.dataHeatBalFanSys->TempControlTypeRpt(ZoneNum) = static_cast<int>(tempControlType);
                    zoneThermostatSetPointLo = tempZoneThermostatSetPoint;
                    zoneThermostatSetPointHi = tempZoneThermostatSetPoint;

                    if (zoneThermostatSetPointLo > tempControlledZone.HeatingResetLimit)
                        zoneThermostatSetPointLo = tempControlledZone.HeatingResetLimit;
                    if (zoneThermostatSetPointHi < tempControlledZone.CoolingResetLimit)
                        zoneThermostatSetPointHi = tempControlledZone.CoolingResetLimit;
                }
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                if (zoneThermostatSetPointLo > tempControlledZone.HeatingResetLimit) zoneThermostatSetPointLo = tempControlledZone.HeatingResetLimit;
                if (zoneThermostatSetPointHi < tempControlledZone.CoolingResetLimit) zoneThermostatSetPointHi = tempControlledZone.CoolingResetLimit;
                break;
            default:
                break;
            }
        }
    }

    for (int Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
        auto &comfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(Loop);
        if (state.dataZoneEquip->ZoneEquipInputsFilled && !state.dataZoneTempPredictorCorrector->ControlledZonesChecked) {
            if (!VerifyControlledZoneForThermostat(state, comfortControlledZone.ZoneName)) {
                ShowSevereError(state,
                                format("{}Zone=\"{}\" has specified a Comfort control but is not a controlled zone.",
                                       RoutineName,
                                       comfortControlledZone.ZoneName));
                ShowContinueError(state, "...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                state.dataZoneTempPredictorCorrector->ErrorsFound = true;
            }
        }
        if (comfortControlledZone.ManageDemand) {
            int ZoneNum = comfortControlledZone.ActualZoneNum;
            auto &zoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum);
            auto &zoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum);
            auto &tempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
            auto &tempControlTypeRpt = state.dataHeatBalFanSys->TempControlTypeRpt(ZoneNum);
            auto &tempControlType = state.dataHeatBalFanSys->TempControlType(ZoneNum);

            switch (state.dataHeatBalFanSys->ComfortControlType(ZoneNum)) {
            case HVAC::ThermostatType::SingleHeating:
                if (tempZoneThermostatSetPoint >= comfortControlledZone.HeatingResetLimit) {
                    tempZoneThermostatSetPoint = comfortControlledZone.HeatingResetLimit;
                    zoneThermostatSetPointLo = tempZoneThermostatSetPoint;
                    tempControlType = HVAC::ThermostatType::SingleHeating;
                    tempControlTypeRpt = static_cast<int>(tempControlType);
                }
                break;
            case HVAC::ThermostatType::SingleCooling:
                if (tempZoneThermostatSetPoint <= comfortControlledZone.CoolingResetLimit) {
                    tempZoneThermostatSetPoint = comfortControlledZone.CoolingResetLimit;
                    zoneThermostatSetPointHi = tempZoneThermostatSetPoint;
                    tempControlType = HVAC::ThermostatType::SingleCooling;
                    tempControlTypeRpt = static_cast<int>(tempControlType);
                }
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                if ((tempZoneThermostatSetPoint >= comfortControlledZone.HeatingResetLimit) ||
                    (tempZoneThermostatSetPoint <= comfortControlledZone.CoolingResetLimit)) {

                    tempControlType = HVAC::ThermostatType::DualSetPointWithDeadBand;
                    tempControlTypeRpt = static_cast<int>(tempControlType);
                    zoneThermostatSetPointLo = tempZoneThermostatSetPoint;
                    zoneThermostatSetPointHi = tempZoneThermostatSetPoint;

                    if (zoneThermostatSetPointLo >= comfortControlledZone.HeatingResetLimit)
                        zoneThermostatSetPointLo = comfortControlledZone.HeatingResetLimit;
                    if (zoneThermostatSetPointHi <= comfortControlledZone.CoolingResetLimit)
                        zoneThermostatSetPointHi = comfortControlledZone.CoolingResetLimit;
                }
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                tempControlType = HVAC::ThermostatType::DualSetPointWithDeadBand;
                tempControlTypeRpt = static_cast<int>(tempControlType);
                if (zoneThermostatSetPointLo >= comfortControlledZone.HeatingResetLimit)
                    zoneThermostatSetPointLo = comfortControlledZone.HeatingResetLimit;
                if (zoneThermostatSetPointHi <= comfortControlledZone.CoolingResetLimit)
                    zoneThermostatSetPointHi = comfortControlledZone.CoolingResetLimit;
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

void ZoneSpaceHeatBalanceData::beginEnvironmentInit(EnergyPlusData &state)
{
    for (int i = 0; i <= 3; ++i) {
        this->ZTM[i] = 0.0;
        this->WPrevZoneTS[i] = state.dataEnvrn->OutHumRat;
        this->DSWPrevZoneTS[i] = state.dataEnvrn->OutHumRat;
        this->WPrevZoneTSTemp[i] = 0.0;
    }
    this->WTimeMinusP = state.dataEnvrn->OutHumRat;
    this->W1 = state.dataEnvrn->OutHumRat;
    this->WMX = state.dataEnvrn->OutHumRat;
    this->WM2 = state.dataEnvrn->OutHumRat;
    this->airHumRatTemp = 0.0;
    this->tempIndLoad = 0.0;
    this->tempDepLoad = 0.0;
    this->airRelHum = 0.0;
    this->AirPowerCap = 0.0;
    this->T1 = 0.0;
}

void ZoneSpaceHeatBalanceData::setUpOutputVars(EnergyPlusData &state, std::string_view prefix, std::string const &name)
{
    SetupOutputVariable(state,
                        format("{} Air Temperature", prefix),
                        Constant::Units::C,
                        this->ZT,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Humidity Ratio", prefix),
                        Constant::Units::None,
                        this->airHumRat,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Relative Humidity", prefix),
                        Constant::Units::Perc,
                        this->airRelHum,
                        OutputProcessor::TimeStepType::System,
                        OutputProcessor::StoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Mean Radiant Temperature", prefix),
                        Constant::Units::C,
                        this->MRT,
                        OutputProcessor::TimeStepType::Zone,
                        OutputProcessor::StoreType::Average,
                        name);
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

    // Staged thermostat setpoint
    if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
        for (int RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneTempPredictorCorrector->NumStageCtrZone; ++RelativeZoneNum) {
            auto &thisStageControlZone = state.dataZoneCtrls->StageControlledZone(RelativeZoneNum);
            int ActualZoneNum = thisStageControlZone.ActualZoneNum;
            auto const &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ActualZoneNum);
            auto &thisZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
            auto &thisZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);
            Real64 ZoneT = thisZoneHB.MAT; // Zone temperature at previous time step
            if (ShortenTimeStepSys) ZoneT = thisZoneHB.XMPT;
            thisStageControlZone.HeatSetPoint = ScheduleManager::GetCurrentScheduleValue(state, thisStageControlZone.HSBchedIndex);
            thisStageControlZone.CoolSetPoint = ScheduleManager::GetCurrentScheduleValue(state, thisStageControlZone.CSBchedIndex);
            if (thisStageControlZone.HeatSetPoint >= thisStageControlZone.CoolSetPoint) {
                ++thisStageControlZone.StageErrCount;
                if (thisStageControlZone.StageErrCount < 2) {
                    ShowWarningError(
                        state,
                        format("ZoneControl:Thermostat:StagedDualSetpoint: The heating setpoint is equal to or above the cooling setpoint in {}",
                               thisStageControlZone.Name));
                    ShowContinueError(state, "The zone heating setpoint is set to the cooling setpoint - 0.1C.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The heating setpoint is still above the cooling setpoint",
                                                   thisStageControlZone.StageErrIndex,
                                                   thisStageControlZone.HeatSetPoint,
                                                   thisStageControlZone.HeatSetPoint);
                }
                thisStageControlZone.HeatSetPoint = thisStageControlZone.CoolSetPoint - 0.1; //???????????
            }
            // Determine either cooling or heating
            if (thisStageControlZone.CoolSetPoint < ZoneT) { // Cooling
                Real64 SetpointOffset = ZoneT - thisStageControlZone.CoolSetPoint;
                int Itemp = 0;
                for (int I = 1; I <= thisStageControlZone.NumOfCoolStages; ++I) {
                    if (SetpointOffset >= thisStageControlZone.CoolTOffset(I)) {
                        Itemp = -I;
                    }
                }
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum = Itemp;
                if (SetpointOffset >= 0.5 * thisStageControlZone.CoolThroRange) {
                    thisZoneThermostatSetPointHi = thisStageControlZone.CoolSetPoint - 0.5 * thisStageControlZone.CoolThroRange;
                } else {
                    thisZoneThermostatSetPointHi = thisStageControlZone.CoolSetPoint + 0.5 * thisStageControlZone.CoolThroRange;
                }
                thisZoneThermostatSetPointLo = thisZoneThermostatSetPointHi;
            } else if (thisStageControlZone.HeatSetPoint > ZoneT) { // heating
                Real64 SetpointOffset = ZoneT - thisStageControlZone.HeatSetPoint;
                int Itemp = 0;
                for (int I = 1; I <= thisStageControlZone.NumOfHeatStages; ++I) {
                    if (std::abs(SetpointOffset) >= std::abs(thisStageControlZone.HeatTOffset(I))) {
                        Itemp = I;
                    }
                }
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum = Itemp;
                if (std::abs(SetpointOffset) >= 0.5 * thisStageControlZone.CoolThroRange) {
                    thisZoneThermostatSetPointLo = thisStageControlZone.HeatSetPoint + 0.5 * thisStageControlZone.HeatThroRange;
                } else {
                    thisZoneThermostatSetPointLo = thisStageControlZone.HeatSetPoint - 0.5 * thisStageControlZone.HeatThroRange;
                }
                thisZoneThermostatSetPointHi = thisZoneThermostatSetPointLo;
            } else {
                thisZoneThermostatSetPointHi = thisStageControlZone.CoolSetPoint + 0.5 * thisStageControlZone.CoolThroRange;
                thisZoneThermostatSetPointLo = thisStageControlZone.HeatSetPoint - 0.5 * thisStageControlZone.HeatThroRange;
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum = 0;
            }
            // SpaceHB TODO: For now, set space stagenum to zone stagenum - later need to see what space the thermostat is in
            if (state.dataHeatBal->doSpaceHeatBalance) {
                for (int spaceNum : state.dataHeatBal->Zone(ActualZoneNum).spaceIndexes) {
                    state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).StageNum =
                        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum;
                }
            }
        }
    }

    // Setpoint revision for onoff thermostat
    if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
        Real64 TempTole = 0.02;
        Real64 Tprev;
        for (int RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {
            auto &thisTempControlledZone = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum);
            if (thisTempControlledZone.DeltaTCutSet > 0.0) {
                if (ShortenTimeStepSys) {
                    thisTempControlledZone.HeatModeLast = thisTempControlledZone.HeatModeLastSave;
                    thisTempControlledZone.CoolModeLast = thisTempControlledZone.CoolModeLastSave;
                } else {
                    thisTempControlledZone.HeatModeLastSave = thisTempControlledZone.HeatModeLast;
                    thisTempControlledZone.CoolModeLastSave = thisTempControlledZone.CoolModeLast;
                }
                auto &thisTempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(thisTempControlledZone.ActualZoneNum);
                auto const &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(thisTempControlledZone.ActualZoneNum);
                auto &thisZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(thisTempControlledZone.ActualZoneNum);
                auto &thisZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(thisTempControlledZone.ActualZoneNum);

                thisTempControlledZone.CoolOffFlag = false;
                thisTempControlledZone.HeatOffFlag = false;
                if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
                    Tprev = thisZoneHB.MAT;
                    if (ShortenTimeStepSys) Tprev = thisZoneHB.XMPT;
                } else {
                    Tprev = thisZoneHB.T1;
                }

                switch (state.dataHeatBalFanSys->TempControlType(thisTempControlledZone.ActualZoneNum)) {
                case HVAC::ThermostatType::SingleHeating:
                    thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointLo;
                    thisZoneThermostatSetPointLo = thisTempControlledZone.ZoneThermostatSetPointLo;
                    if (Tprev < thisTempControlledZone.ZoneThermostatSetPointLo + TempTole) {
                        thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointLo + thisTempControlledZone.DeltaTCutSet;
                        thisZoneThermostatSetPointLo = thisTempZoneThermostatSetPoint;
                    } else if (Tprev > thisTempControlledZone.ZoneThermostatSetPointLo &&
                               (Tprev < thisTempControlledZone.ZoneThermostatSetPointLo + thisTempControlledZone.DeltaTCutSet - TempTole)) {
                        thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointLo + thisTempControlledZone.DeltaTCutSet;
                        thisZoneThermostatSetPointLo = thisTempZoneThermostatSetPoint;
                    } else {
                        thisTempControlledZone.HeatOffFlag = true;
                    }
                    if (thisTempControlledZone.HeatModeLast && Tprev > thisTempControlledZone.ZoneThermostatSetPointLo) {
                        thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointLo;
                        thisZoneThermostatSetPointLo = thisTempControlledZone.ZoneThermostatSetPointLo;
                        thisTempControlledZone.HeatOffFlag = true;
                    }
                    break;
                case HVAC::ThermostatType::SingleCooling:
                    thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointHi;
                    thisZoneThermostatSetPointHi = thisTempControlledZone.ZoneThermostatSetPointHi;
                    if (Tprev > thisTempControlledZone.ZoneThermostatSetPointHi - TempTole) {
                        thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointHi - thisTempControlledZone.DeltaTCutSet;
                        thisZoneThermostatSetPointHi = thisTempZoneThermostatSetPoint;
                    } else if (Tprev < thisTempControlledZone.ZoneThermostatSetPointHi &&
                               Tprev > thisTempControlledZone.ZoneThermostatSetPointHi - thisTempControlledZone.DeltaTCutSet + TempTole) {
                        thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointHi - thisTempControlledZone.DeltaTCutSet;
                        thisZoneThermostatSetPointHi = thisTempZoneThermostatSetPoint;
                    } else {
                        thisTempControlledZone.CoolOffFlag = true;
                    }
                    if (thisTempControlledZone.CoolModeLast && Tprev < thisTempControlledZone.ZoneThermostatSetPointHi) {
                        thisTempZoneThermostatSetPoint = thisTempControlledZone.ZoneThermostatSetPointHi;
                        thisZoneThermostatSetPointHi = thisTempControlledZone.ZoneThermostatSetPointHi;
                        thisTempControlledZone.CoolOffFlag = true;
                    }
                    break;
                case HVAC::ThermostatType::DualSetPointWithDeadBand:
                    thisZoneThermostatSetPointHi = thisTempControlledZone.ZoneThermostatSetPointHi;
                    thisZoneThermostatSetPointLo = thisTempControlledZone.ZoneThermostatSetPointLo;
                    if (Tprev > thisTempControlledZone.ZoneThermostatSetPointHi - TempTole) {
                        thisZoneThermostatSetPointHi = thisTempControlledZone.ZoneThermostatSetPointHi - thisTempControlledZone.DeltaTCutSet;
                    } else if (Tprev < thisTempControlledZone.ZoneThermostatSetPointHi &&
                               Tprev > thisTempControlledZone.ZoneThermostatSetPointHi - thisTempControlledZone.DeltaTCutSet + TempTole) {
                        thisZoneThermostatSetPointHi = thisTempControlledZone.ZoneThermostatSetPointHi - thisTempControlledZone.DeltaTCutSet;
                    } else {
                        thisTempControlledZone.CoolOffFlag = true;
                    }
                    if (thisTempControlledZone.CoolModeLast && Tprev < thisTempControlledZone.ZoneThermostatSetPointHi) {
                        thisZoneThermostatSetPointHi = thisTempControlledZone.ZoneThermostatSetPointHi;
                        thisTempControlledZone.CoolOffFlag = true;
                    }

                    if (Tprev < thisTempControlledZone.ZoneThermostatSetPointLo + TempTole) {
                        thisZoneThermostatSetPointLo = thisTempControlledZone.ZoneThermostatSetPointLo + thisTempControlledZone.DeltaTCutSet;
                    } else if (Tprev > thisTempControlledZone.ZoneThermostatSetPointLo &&
                               (Tprev < thisTempControlledZone.ZoneThermostatSetPointLo + thisTempControlledZone.DeltaTCutSet - TempTole)) {
                        thisZoneThermostatSetPointLo = thisTempControlledZone.ZoneThermostatSetPointLo + thisTempControlledZone.DeltaTCutSet;
                    } else {
                        thisTempControlledZone.HeatOffFlag = true;
                    }
                    if (thisTempControlledZone.HeatModeLast && Tprev > thisTempControlledZone.ZoneThermostatSetPointLo) {
                        thisZoneThermostatSetPointLo = thisTempControlledZone.ZoneThermostatSetPointLo;
                        thisTempControlledZone.HeatOffFlag = true;
                    }
                    // check setpoint for both and provide an error message
                    if (thisZoneThermostatSetPointLo >= thisZoneThermostatSetPointHi) {
                        ShowSevereError(state,
                                        "DualSetPointWithDeadBand: When Temperature Difference Between Cutout And Setpoint is applied, the heating "
                                        "setpoint is greater than the cooling setpoint. ");
                        ShowContinueErrorTimeStamp(state,
                                                   format("occurs in Zone={}", state.dataHeatBal->Zone(thisTempControlledZone.ActualZoneNum).Name));
                        ShowContinueError(state, format("Zone Heating ThermostatSetPoint={:.2R}", thisZoneThermostatSetPointLo));
                        ShowContinueError(state, format("Zone Cooling ThermostatSetPoint={:.2R}", thisZoneThermostatSetPointHi));
                        ShowFatalError(state, "Program terminates due to above conditions.");
                    }
                    break;
                default:
                    break;
                }
            }
        }
    }

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum);
        thisZoneHB.predictSystemLoad(state, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep, zoneNum);
        for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
            if (state.dataHeatBal->doSpaceHeatBalance) {
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).predictSystemLoad(
                    state, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep, zoneNum, spaceNum);
            } else if (ShortenTimeStepSys) {
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).MAT = thisZoneHB.MAT;
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).airHumRat = thisZoneHB.airHumRat;
            }
        }
    }
    if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
        for (int RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {
            auto &thisTempControlledZone = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum);
            if (thisTempControlledZone.DeltaTCutSet > 0.0) {
                int ZoneNum = thisTempControlledZone.ActualZoneNum;
                if (thisTempControlledZone.CoolOffFlag && state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired >= 0.0) {
                    thisTempControlledZone.CoolModeLast = true;
                } else {
                    thisTempControlledZone.CoolModeLast = false;
                }
                if (thisTempControlledZone.HeatOffFlag && state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired <= 0.0) {
                    thisTempControlledZone.HeatModeLast = true;
                } else {
                    thisTempControlledZone.HeatModeLast = false;
                }
            }
        }
    }
}
void ZoneSpaceHeatBalanceData::predictSystemLoad(
    EnergyPlusData &state,
    bool const shortenTimeStepSys,
    bool const useZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
    Real64 const priorTimeStep,        // the old value for timestep length is passed for possible use in interpolating
    int zoneNum,
    int spaceNum)
{
    assert(zoneNum > 0);
    this->updateTemperatures(state, shortenTimeStepSys, useZoneTimeStepHistory, priorTimeStep, zoneNum, spaceNum);

    Real64 TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    Real64 volume = 0.0;
    if (spaceNum > 0) {
        volume = state.dataHeatBal->space(spaceNum).Volume;
    } else {
        volume = state.dataHeatBal->Zone(zoneNum).Volume;
    }

    this->AirPowerCap = volume * state.dataHeatBal->Zone(zoneNum).ZoneVolCapMultpSens *
                        Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, this->MAT, this->airHumRat) *
                        Psychrometrics::PsyCpAirFnW(this->airHumRat) / TimeStepSysSec;
    Real64 RAFNFrac = 0.0;

    // Calculate the various heat balance sums

    // NOTE: SumSysMCp and SumSysMCpT are not used in the predict step
    this->calcZoneOrSpaceSums(state, false, zoneNum, spaceNum);

    // Sum all convective internal gains except for people: SumIntGainExceptPeople
    if (spaceNum == 0 && state.dataHybridModel->FlagHybridModel_PC) {
        this->SumIntGainExceptPeople = 0.0;
        this->SumIntGainExceptPeople = InternalHeatGains::SumAllInternalConvectionGainsExceptPeople(state, zoneNum);
    }

    this->TempDepCoef = this->SumHA + this->SumMCp;
    this->TempIndCoef = this->SumIntGain + this->SumHATsurf - this->SumHATref + this->SumMCpT + this->SysDepZoneLoadsLagged;
    this->TempHistoryTerm = this->AirPowerCap * (3.0 * this->ZTM[0] - (3.0 / 2.0) * this->ZTM[1] + (1.0 / 3.0) * this->ZTM[2]);
    this->tempDepLoad = (11.0 / 6.0) * this->AirPowerCap + this->TempDepCoef;
    this->tempIndLoad = this->TempHistoryTerm + this->TempIndCoef;
    if (state.dataRoomAir->anyNonMixingRoomAirModel) {
        if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
            // RoomAirflowNetworkModel - make dynamic term independent of TimeStepSys
            auto &afnZoneInfo = state.dataRoomAir->AFNZoneInfo(zoneNum);
            if (afnZoneInfo.IsUsed) {
                int RoomAirNode = afnZoneInfo.ControlAirNodeID;
                RoomAir::LoadPredictionRoomAirModelAFN(state, zoneNum, RoomAirNode);
                this->TempDepCoef = afnZoneInfo.Node(RoomAirNode).SumHA + afnZoneInfo.Node(RoomAirNode).SumLinkMCp;
                this->TempIndCoef = afnZoneInfo.Node(RoomAirNode).SumIntSensibleGain + afnZoneInfo.Node(RoomAirNode).SumHATsurf -
                                    afnZoneInfo.Node(RoomAirNode).SumHATref + afnZoneInfo.Node(RoomAirNode).SumLinkMCpT +
                                    afnZoneInfo.Node(RoomAirNode).SysDepZoneLoadsLagged;
                this->AirPowerCap = afnZoneInfo.Node(RoomAirNode).AirVolume * state.dataHeatBal->Zone(zoneNum).ZoneVolCapMultpSens *
                                    afnZoneInfo.Node(RoomAirNode).RhoAir * afnZoneInfo.Node(RoomAirNode).CpAir / TimeStepSysSec;
                this->TempHistoryTerm = this->AirPowerCap * (3.0 * this->ZTM[0] - (3.0 / 2.0) * this->ZTM[1] + (1.0 / 3.0) * this->ZTM[2]);
                this->tempDepLoad = (11.0 / 6.0) * this->AirPowerCap + this->TempDepCoef;
                this->tempIndLoad = this->TempHistoryTerm + this->TempIndCoef;
                if (afnZoneInfo.Node(RoomAirNode).HasHVACAssigned) RAFNFrac = afnZoneInfo.Node(RoomAirNode).HVAC(1).SupplyFraction;
            }
        }
    }

    // Exact solution or Euler method
    state.dataHVACGlobal->ShortenTimeStepSysRoomAir = false;
    if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
        if (shortenTimeStepSys && TimeStepSys < state.dataGlobal->TimeStepZone) {
            if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                this->T1 = this->TM2;
                this->W1 = this->WM2;
                if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                    auto &afnZoneInfo = state.dataRoomAir->AFNZoneInfo(zoneNum);
                    for (auto &afnNode : afnZoneInfo.Node) {
                        afnNode.AirTempT1 = afnNode.AirTempT2;
                        afnNode.HumRatT1 = afnNode.HumRatT2;
                    }
                }
            } else {
                this->T1 = this->TMX;
                this->W1 = this->WMX;
                if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                    auto &afnZoneInfo = state.dataRoomAir->AFNZoneInfo(zoneNum);
                    for (auto &afnNode : afnZoneInfo.Node) {
                        afnNode.AirTempT1 = afnNode.AirTempTX;
                        afnNode.HumRatT1 = afnNode.HumRatTX;
                    }
                }
            }
            state.dataHVACGlobal->ShortenTimeStepSysRoomAir = true;
        } else {
            this->T1 = this->ZT;
            this->W1 = this->airHumRat;
            if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                auto &afnZoneInfo = state.dataRoomAir->AFNZoneInfo(zoneNum);
                for (auto &afnNode : afnZoneInfo.Node) {
                    afnNode.AirTempT1 = afnNode.AirTemp;
                    afnNode.HumRatT1 = afnNode.HumRat;
                }
            }
        }
        this->tempDepLoad = this->TempDepCoef;
        this->tempIndLoad = this->TempIndCoef;
    }

    // Calculate the predicted zone load to be provided by the system with the given desired zone air temperature
    this->calcPredictedSystemLoad(state, RAFNFrac, zoneNum, spaceNum);

    // Calculate the predicted zone load to be provided by the system with the given desired humidity ratio
    this->calcPredictedHumidityRatio(state, RAFNFrac, zoneNum, spaceNum);
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

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RelativeZoneNum;
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
    int NumOfZones = state.dataGlobal->NumOfZones;

    TempControlType = HVAC::ThermostatType::Uncontrolled; // Default

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
        int ActualZoneNum = TempControlledZone(RelativeZoneNum).ActualZoneNum;
        int TempControlSchedIndex = TempControlledZone(RelativeZoneNum).CTSchedIndex;
        TempControlType(ActualZoneNum) = static_cast<HVAC::ThermostatType>(ScheduleManager::GetCurrentScheduleValue(state, TempControlSchedIndex));
        TempControlTypeRpt(ActualZoneNum) = static_cast<int>(TempControlType(ActualZoneNum));
        // Error detection for these values is done in the Get routine

        switch (TempControlType(ActualZoneNum)) {
        case HVAC::ThermostatType::Uncontrolled:
            break;
        case HVAC::ThermostatType::SingleHeating:
            SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatSetPoint;
            TempZoneThermostatSetPoint(ActualZoneNum) = ScheduleManager::GetCurrentScheduleValue(state, SchedNameIndex);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = TempZoneThermostatSetPoint(ActualZoneNum);

            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
            ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
            break;
        case HVAC::ThermostatType::SingleCooling:
            SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleCoolSetPoint;
            TempZoneThermostatSetPoint(ActualZoneNum) = ScheduleManager::GetCurrentScheduleValue(state, SchedNameIndex);
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
        case HVAC::ThermostatType::SingleHeatCool:

            SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatCoolSetPoint;

            TempZoneThermostatSetPoint(ActualZoneNum) = ScheduleManager::GetCurrentScheduleValue(state, SchedNameIndex);

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

            if (allocated(state.dataAvail->OptStart)) {
                if (!allocated(DaySPValues)) {
                    DaySPValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
                }
                if (state.dataAvail->OptStart(ActualZoneNum).ActualZoneNum == ActualZoneNum) {
                    ScheduleManager::GetScheduleValuesForDay(state, SetPointTempSchedIndexCold, DaySPValues);
                    OccStartTime = CEILING(state.dataAvail->OptStart(ActualZoneNum).OccStartTime) + 1;
                    TempZoneThermostatSetPoint(ActualZoneNum) = DaySPValues(1, OccStartTime);
                }

                if (state.dataAvail->OptStart(ActualZoneNum).OptStartFlag) {
                    ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                }
            }
            //--------------------------------------------------------------------------------------------
            break;
        case HVAC::ThermostatType::DualSetPointWithDeadBand:
            SetPointTempSchedIndexHot = TempControlledZone(RelativeZoneNum).SchIndx_DualSetPointWDeadBandHeat;
            SetPointTempSchedIndexCold = TempControlledZone(RelativeZoneNum).SchIndx_DualSetPointWDeadBandCool;

            ZoneThermostatSetPointHi(ActualZoneNum) = ScheduleManager::GetCurrentScheduleValue(state, SetPointTempSchedIndexCold);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = ZoneThermostatSetPointHi(ActualZoneNum);

            // Added Jan 17 (X. Luo)
            // Adjust operative temperature based on adaptive comfort model
            if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, ZoneThermostatSetPointHi(ActualZoneNum));
                state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = ZoneThermostatSetPointHi(ActualZoneNum);
            }

            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointHi(ActualZoneNum));

            ZoneThermostatSetPointLo(ActualZoneNum) = ScheduleManager::GetCurrentScheduleValue(state, SetPointTempSchedIndexHot);
            TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = ZoneThermostatSetPointLo(ActualZoneNum);
            AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointLo(ActualZoneNum));

            // Change the room set point to occupied set point during optimum start period--------------

            if (allocated(state.dataAvail->OptStart)) {
                if (!allocated(DaySPValues)) {
                    DaySPValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
                }
                if (state.dataAvail->OptStart(ActualZoneNum).ActualZoneNum == ActualZoneNum) {
                    ScheduleManager::GetScheduleValuesForDay(state, SetPointTempSchedIndexCold, DaySPValues);
                    OccStartTime = CEILING(state.dataAvail->OptStart(ActualZoneNum).OccStartTime) + 1;
                    state.dataZoneCtrls->OccRoomTSetPointCool(ActualZoneNum) = DaySPValues(1, OccStartTime);
                    ScheduleManager::GetScheduleValuesForDay(state, SetPointTempSchedIndexHot, DaySPValues);
                    state.dataZoneCtrls->OccRoomTSetPointHeat(ActualZoneNum) = DaySPValues(1, OccStartTime);
                }

                if (state.dataAvail->OptStart(ActualZoneNum).OptStartFlag) {
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

        // Apply offset for faulty thermostats
        if ((state.dataFaultsMgr->NumFaultyThermostat > 0) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            //  loop through the FaultsThermostatOffset objects to find the one for the zone
            for (int iFault = 1; iFault <= state.dataFaultsMgr->NumFaultyThermostat; ++iFault) {

                if (Util::SameString(TempControlledZone(RelativeZoneNum).Name,
                                     state.dataFaultsMgr->FaultsThermostatOffset(iFault).FaultyThermostatName)) {

                    // Check fault availability schedules
                    if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsThermostatOffset(iFault).availSchedNum) > 0.0) {

                        // Check fault severity schedules to update the reference thermostat offset
                        Real64 rSchVal = 1.0;
                        Real64 offsetUpdated;
                        if (state.dataFaultsMgr->FaultsThermostatOffset(iFault).severitySchedNum >= 0) {
                            rSchVal =
                                ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsThermostatOffset(iFault).severitySchedNum);
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

void ZoneSpaceHeatBalanceData::calcPredictedHumidityRatio(EnergyPlusData &state, Real64 const RAFNFrac, int const zoneNum, int const spaceNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   May 2001

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine does the prediction step for humidity control

    // METHODOLOGY EMPLOYED:
    // This solves for the required system moisture required to try and achieve the desired
    // Humidity Ratio in the Zone

    // REFERENCES:
    // Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
    // for BLAST.

    static constexpr std::string_view RoutineName("calcPredictedHumidityRatio");

    Real64 ZoneRHHumidifyingSetPoint = 0.0;   // Zone humidifying set point (%)
    Real64 ZoneRHDehumidifyingSetPoint = 0.0; // Zone dehumidifying set point (%)

    auto &thisZone = state.dataHeatBal->Zone(zoneNum);
    bool SingleSetPoint = false; // This determines whether both setpoint are equal or not

    // Check to see if this is a "humidity controlled zone"
    bool ControlledHumidZoneFlag = false;
    // Check all the controlled zones to see if it matches the zone simulated
    if (thisZone.humidityControlZoneIndex > 0) {
        auto &humidityControlZone = state.dataZoneCtrls->HumidityControlZone(thisZone.humidityControlZoneIndex);
        assert(humidityControlZone.ActualZoneNum == zoneNum);
        ZoneRHHumidifyingSetPoint = ScheduleManager::GetCurrentScheduleValue(state, humidityControlZone.HumidifyingSchedIndex);
        ZoneRHDehumidifyingSetPoint = ScheduleManager::GetCurrentScheduleValue(state, humidityControlZone.DehumidifyingSchedIndex);

        // Apply EMS values to overwrite the humidistat values
        if (humidityControlZone.EMSOverrideHumidifySetPointOn) {
            ZoneRHHumidifyingSetPoint = humidityControlZone.EMSOverrideHumidifySetPointValue;
        }
        if (humidityControlZone.EMSOverrideDehumidifySetPointOn) {
            ZoneRHDehumidifyingSetPoint = humidityControlZone.EMSOverrideDehumidifySetPointValue;
        }

        // Apply offsets for faulty humidistats
        if ((state.dataFaultsMgr->NumFaultyHumidistat > 0) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {

            //  loop through the FaultsHumidistatOffset objects to find the one for the zone
            for (int iFault = 1; iFault <= state.dataFaultsMgr->NumFaultyHumidistat; ++iFault) {

                if (Util::SameString(humidityControlZone.ControlName, state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyHumidistatName)) {

                    if (Util::SameString(state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyHumidistatType, "ThermostatOffsetDependent")) {
                        // For Humidistat Offset Type I: ThermostatOffsetDependent

                        bool IsThermostatFound = false;
                        Real64 offsetThermostat = 0.0;

                        // Get the offset value of the corresponding thermostat fault object
                        if (state.dataFaultsMgr->NumFaultyThermostat > 0) {

                            //  loop through the FaultsThermostatOffset objects to find the one causes the Humidistat Offset
                            for (int iFaultThermo = 1; iFaultThermo <= state.dataFaultsMgr->NumFaultyThermostat; ++iFaultThermo) {

                                if (Util::SameString(state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyThermostatName,
                                                     state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).Name)) {
                                    IsThermostatFound = true;

                                    // Check fault availability schedules
                                    if (ScheduleManager::GetCurrentScheduleValue(
                                            state, state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).availSchedNum) > 0.0) {

                                        // Check fault severity schedules to update the reference thermostat offset
                                        Real64 rSchVal = 1.0;
                                        if (state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).severitySchedNum >= 0) {
                                            rSchVal = ScheduleManager::GetCurrentScheduleValue(
                                                state, state.dataFaultsMgr->FaultsThermostatOffset(iFaultThermo).severitySchedNum);
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
                            ShowSevereError(
                                state,
                                format("FaultModel:HumidistatOffset = \"{}\" invalid Reference Humidistat Offset Name = \"{}\" not found.",
                                       state.dataFaultsMgr->FaultsHumidistatOffset(iFault).Name,
                                       state.dataFaultsMgr->FaultsHumidistatOffset(iFault).FaultyThermostatName));
                            ShowFatalError(state, "Errors getting FaultModel input data.  Preceding condition(s) cause termination.");
                        }

                        if (offsetThermostat != 0.0) {
                            // Calculate the humidistat offset value from the thermostat offset value
                            Real64 faultZoneWHumidifyingSetPoint = Psychrometrics::PsyWFnTdbRhPb(
                                state, (this->MAT + offsetThermostat), (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                            Real64 faultZoneWDehumidifyingSetPoint = Psychrometrics::PsyWFnTdbRhPb(
                                state, (this->MAT + offsetThermostat), (ZoneRHDehumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                            Real64 offsetZoneRHHumidifyingSetPoint =
                                ZoneRHHumidifyingSetPoint -
                                Psychrometrics::PsyRhFnTdbWPb(state, this->MAT, faultZoneWHumidifyingSetPoint, state.dataEnvrn->OutBaroPress) * 100.0;
                            Real64 offsetZoneRHDehumidifyingSetPoint =
                                ZoneRHDehumidifyingSetPoint -
                                Psychrometrics::PsyRhFnTdbWPb(state, this->MAT, faultZoneWDehumidifyingSetPoint, state.dataEnvrn->OutBaroPress) *
                                    100.0;

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
                        if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsHumidistatOffset(iFault).availSchedNum) >
                            0.0) {

                            // Check fault severity schedules to update the reference humidistat offset
                            Real64 rSchVal = 1.0;
                            Real64 offsetUpdated;
                            if (state.dataFaultsMgr->FaultsHumidistatOffset(iFault).severitySchedNum >= 0) {
                                rSchVal = ScheduleManager::GetCurrentScheduleValue(
                                    state, state.dataFaultsMgr->FaultsHumidistatOffset(iFault).severitySchedNum);
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
                    break;
                }
            }
        }

        // Run-time error check
        if (ZoneRHHumidifyingSetPoint > ZoneRHDehumidifyingSetPoint) {
            if (humidityControlZone.ErrorIndex == 0) {
                ShowWarningMessage(
                    state, format("HUMIDISTAT: The humidifying setpoint is above the dehumidifying setpoint in {}", humidityControlZone.ControlName));
                ShowContinueError(state, "The zone humidifying setpoint is set to the dehumidifying setpoint.");
                ShowContinueErrorTimeStamp(state, "Occurrence info:");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           "The humidifying setpoint is still above the dehumidifying setpoint",
                                           humidityControlZone.ErrorIndex,
                                           ZoneRHHumidifyingSetPoint,
                                           ZoneRHHumidifyingSetPoint);
            ZoneRHHumidifyingSetPoint = ZoneRHDehumidifyingSetPoint;
        }
        if (ZoneRHHumidifyingSetPoint == ZoneRHDehumidifyingSetPoint) SingleSetPoint = true;
        ControlledHumidZoneFlag = true;

    } // HumidControlledZoneNum

    // if zone latent sizing is requested but no humidistat exists
    if (state.dataGlobal->DoingSizing && !ControlledHumidZoneFlag && state.dataHeatBal->DoLatentSizing) {
        for (size_t zoneEqConfigNum = 1; zoneEqConfigNum <= state.dataZoneEquip->ZoneEquipConfig.size(); ++zoneEqConfigNum) {
            auto &zoneEqConfig = state.dataZoneEquip->ZoneEquipConfig(zoneEqConfigNum);
            if (!zoneEqConfig.IsControlled) continue;
            int ZoneSizNum = Util::FindItemInList(zoneEqConfig.ZoneName, state.dataSize->ZoneSizingInput, &DataSizing::ZoneSizingInputData::ZoneName);
            // should use the first Sizing:Zone object if not found
            if (ZoneSizNum == 0 && !state.dataSize->ZoneSizingInput.empty()) ZoneSizNum = 1;
            if (ZoneSizNum > 0) {
                auto &zoneSizingInput = state.dataSize->ZoneSizingInput(ZoneSizNum);
                if (zoneSizingInput.zoneLatentSizing) {
                    ZoneRHDehumidifyingSetPoint = (zoneSizingInput.zoneRHDehumidifySchIndex)
                                                      ? ScheduleManager::GetCurrentScheduleValue(state, zoneSizingInput.zoneRHDehumidifySchIndex)
                                                      : zoneSizingInput.zoneRHDehumidifySetPoint;
                    ZoneRHHumidifyingSetPoint = (zoneSizingInput.zoneRHHumidifySchIndex)
                                                    ? ScheduleManager::GetCurrentScheduleValue(state, zoneSizingInput.zoneRHHumidifySchIndex)
                                                    : zoneSizingInput.zoneRHHumidifySetPoint;
                    if (ZoneRHHumidifyingSetPoint > ZoneRHDehumidifyingSetPoint) ZoneRHHumidifyingSetPoint = ZoneRHDehumidifyingSetPoint;
                    if (ZoneRHHumidifyingSetPoint == ZoneRHDehumidifyingSetPoint) SingleSetPoint = true;
                    ControlledHumidZoneFlag = true;
                }
            }
            break;
        }
    }

    Real64 LoadToHumidifySetPoint = 0.0;   // Moisture load at humidifying set point
    Real64 LoadToDehumidifySetPoint = 0.0; // Moisture load at dehumidifying set point
    Real64 totalOutputRequired = 0.0;
    if (ControlledHumidZoneFlag) {

        // Calculate hourly humidity ratio from infiltration + humidity added from latent load
        // to determine system added/subtracted moisture.
        Real64 LatentGain = this->latentGain + state.dataHeatBalFanSys->SumLatentHTRadSys(zoneNum) + state.dataHeatBalFanSys->SumLatentPool(zoneNum);

        Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

        // Calculate the coefficients for the 3rd Order derivative for final
        // zone humidity ratio.  The A, B, C coefficients are analogous to the heat balance.
        // SumHmARaW and SumHmARa will be used with the Moisture Balance on the building elements and
        // are currently set to zero when the CTF only version is used.

        // The density of air and latent heat of vaporization are calculated as functions.
        Real64 RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, this->ZT, this->airHumRat, RoutineName);
        Real64 H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(this->airHumRat, this->ZT);

        // Assume that the system will have flow
        Real64 A = 0.0;
        Real64 B = 0.0;
        Real64 C = 0.0;
        if (state.afn->multizone_always_simulated ||
            (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation &&
             state.afn->AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            B = (LatentGain / H2OHtOfVap) + state.afn->exchangeData(zoneNum).SumMHrW + state.afn->exchangeData(zoneNum).SumMMHrW + this->SumHmARaW;
            A = state.afn->exchangeData(zoneNum).SumMHr + state.afn->exchangeData(zoneNum).SumMMHr + this->SumHmARa;
        } else {
            B = (LatentGain / H2OHtOfVap) + ((this->OAMFL + this->VAMFL + this->CTMFL) * state.dataEnvrn->OutHumRat) + this->EAMFLxHumRat +
                this->SumHmARaW + this->MixingMassFlowXHumRat + this->MDotOA * state.dataEnvrn->OutHumRat;
            A = this->OAMFL + this->VAMFL + this->EAMFL + this->CTMFL + this->SumHmARa + this->MixingMassFlowZone + this->MDotOA;
        }
        Real64 volume = 0.0;
        if (spaceNum > 0) {
            volume = state.dataHeatBal->space(spaceNum).Volume;
        } else {
            volume = thisZone.Volume;
        }
        C = RhoAir * volume * thisZone.ZoneVolCapMultpMoist / TimeStepSysSec;

        if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
            auto &roomAFNInfo = state.dataRoomAir->AFNZoneInfo(zoneNum);
            int RoomAirNode = roomAFNInfo.ControlAirNodeID;
            H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(roomAFNInfo.Node(RoomAirNode).HumRat, roomAFNInfo.Node(RoomAirNode).AirTemp);
            A = roomAFNInfo.Node(RoomAirNode).SumLinkM + roomAFNInfo.Node(RoomAirNode).SumHmARa;
            B = (roomAFNInfo.Node(RoomAirNode).SumIntLatentGain / H2OHtOfVap) + roomAFNInfo.Node(RoomAirNode).SumLinkMW +
                roomAFNInfo.Node(RoomAirNode).SumHmARaW;
            C = roomAFNInfo.Node(RoomAirNode).RhoAir * roomAFNInfo.Node(RoomAirNode).AirVolume * thisZone.ZoneVolCapMultpMoist / TimeStepSysSec;
        }

        // Use a 3rd Order derivative to predict zone moisture addition or removal and
        // smooth the changes using the zone air capacitance.  Positive values of Moist Load means that
        // this amount of moisture must be added to the zone to reach the setpoint.  Negative values represent
        // the amount of moisture that must be removed by the system.
        // MoistLoadHumidSetPoint = massflow * HumRat = kgDryAir/s * kgWater/kgDryAir = kgWater/s
        Real64 WZoneSetPoint =
            Psychrometrics::PsyWFnTdbRhPb(state, this->ZT, (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress, RoutineName);
        Real64 exp_700_A_C(0.0);
        if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToHumidifySetPoint =
                ((11.0 / 6.0) * C + A) * WZoneSetPoint -
                (B + C * (3.0 * this->WPrevZoneTSTemp[0] - (3.0 / 2.0) * this->WPrevZoneTSTemp[1] + (1.0 / 3.0) * this->WPrevZoneTSTemp[2]));
            // Exact solution
        } else if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (A == 0.0) { // B=0
                LoadToHumidifySetPoint = C * (WZoneSetPoint - this->W1) - B;
            } else {
                exp_700_A_C = std::exp(min(700.0, -A / C)); // Tuned Save expensive value
                LoadToHumidifySetPoint = A * (WZoneSetPoint - this->W1 * exp_700_A_C) / (1.0 - exp_700_A_C) - B;
            }
        } else if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToHumidifySetPoint = C * (WZoneSetPoint - this->W1) + A * WZoneSetPoint - B;
        }
        if (RAFNFrac > 0.0) LoadToHumidifySetPoint = LoadToHumidifySetPoint / RAFNFrac;
        WZoneSetPoint =
            Psychrometrics::PsyWFnTdbRhPb(state, this->ZT, (ZoneRHDehumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress, RoutineName);
        if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::ThirdOrder) {
            LoadToDehumidifySetPoint =
                ((11.0 / 6.0) * C + A) * WZoneSetPoint -
                (B + C * (3.0 * this->WPrevZoneTSTemp[0] - (3.0 / 2.0) * this->WPrevZoneTSTemp[1] + (1.0 / 3.0) * this->WPrevZoneTSTemp[2]));
            // Exact solution
        } else if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::AnalyticalSolution) {
            if (A == 0.0) { // B=0
                LoadToDehumidifySetPoint = C * (WZoneSetPoint - this->W1) - B;
            } else {
                LoadToDehumidifySetPoint = A * (WZoneSetPoint - this->W1 * exp_700_A_C) / (1.0 - exp_700_A_C) - B; // exp_700_A_C set above
            }
        } else if (state.dataHeatBal->ZoneAirSolutionAlgo == DataHeatBalance::SolutionAlgo::EulerMethod) {
            LoadToDehumidifySetPoint = C * (WZoneSetPoint - this->W1) + A * WZoneSetPoint - B;
        }
        if (RAFNFrac > 0.0) LoadToDehumidifySetPoint = LoadToDehumidifySetPoint / RAFNFrac;

        // The load is added to the TotalOutputRequired as in the Temperature Predictor.  There is also the remaining
        // output variable for those who will use this for humidity control and stored in DataZoneEnergyDemands with the
        // analogous temperature terms.

        if (SingleSetPoint) {
            totalOutputRequired = LoadToHumidifySetPoint;
        } else {
            if (LoadToHumidifySetPoint > 0.0 && LoadToDehumidifySetPoint > 0.0) {
                totalOutputRequired = LoadToHumidifySetPoint;
            } else if (LoadToHumidifySetPoint < 0.0 && LoadToDehumidifySetPoint < 0.0) {
                totalOutputRequired = LoadToDehumidifySetPoint;
            } else if (LoadToHumidifySetPoint <= 0.0 && LoadToDehumidifySetPoint >= 0.0) { // deadband includes zero loads
                totalOutputRequired = 0.0;
            } else { // this should never occur!
                ShowSevereError(
                    state, "Humidistat: Unanticipated combination of humidifying and dehumidifying loads - report to EnergyPlus Development Team");
                ShowContinueErrorTimeStamp(state, format("occurs in Zone = {}", thisZone.Name));
                ShowContinueError(
                    state,
                    format("LoadToHumidifySetPoint={:.5R}, LoadToDehumidifySetPoint={:.5R}", LoadToHumidifySetPoint, LoadToDehumidifySetPoint));
                ShowContinueError(state, format("Zone RH Humidifying Set-point={:.1R}", ZoneRHHumidifyingSetPoint));
                ShowContinueError(state, format("Zone RH Dehumidifying Set-point={:.2R}", ZoneRHDehumidifyingSetPoint));
                ShowFatalError(state, "Program terminates due to above conditions.");
            }
        }
    }

    // Apply zone multipliers as needed or set to zero
    if (spaceNum > 0) {
        auto &thisspaceSysMoistureDemand = state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum);
        if (ControlledHumidZoneFlag) {
            thisspaceSysMoistureDemand.reportMoistLoadsZoneMultiplier(
                state, zoneNum, totalOutputRequired, LoadToHumidifySetPoint, LoadToDehumidifySetPoint);
        } else {
            thisspaceSysMoistureDemand.TotalOutputRequired = 0.0;
            thisspaceSysMoistureDemand.OutputRequiredToDehumidifyingSP = 0.0;
            thisspaceSysMoistureDemand.OutputRequiredToHumidifyingSP = 0.0;
        }
    } else {
        auto &thisZoneSysMoistureDemand = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(zoneNum);
        if (ControlledHumidZoneFlag) {
            thisZoneSysMoistureDemand.reportMoistLoadsZoneMultiplier(
                state, zoneNum, totalOutputRequired, LoadToHumidifySetPoint, LoadToDehumidifySetPoint);
        } else {
            thisZoneSysMoistureDemand.TotalOutputRequired = 0.0;
            thisZoneSysMoistureDemand.OutputRequiredToDehumidifyingSP = 0.0;
            thisZoneSysMoistureDemand.OutputRequiredToHumidifyingSP = 0.0;
        }
    }
}

Real64 correctZoneAirTemps(EnergyPlusData &state,
                           bool useZoneTimeStepHistory // if true then use zone timestep history, if false use system time step history
)
{
    Real64 maxTempChange = DataPrecisionGlobals::constant_zero; // Max absolute air temperature change between previous and current timestep
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum);
        Real64 zoneTempChange = thisZoneHB.correctAirTemp(state, useZoneTimeStepHistory, zoneNum);
        auto &thisZone = state.dataHeatBal->Zone(zoneNum);
        for (int spaceNum : thisZone.spaceIndexes) {
            auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum);
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation &&
                !state.dataGlobal->DoingSizing) { // Need space air temps to match zone temps for sizing
                Real64 spaceTempChange = thisSpaceHB.correctAirTemp(state, useZoneTimeStepHistory, zoneNum, spaceNum);
                maxTempChange = max(maxTempChange, spaceTempChange);
            } else {
                // If doing sizing and zone is controlled, then set space node to match zone node
                if (state.dataHeatBal->doSpaceHeatBalanceSizing && thisZone.IsControlled) {
                    auto const &thisZoneNode = state.dataLoopNodes->Node(thisZone.SystemZoneNodeNumber);
                    auto &thisSpaceNode = state.dataLoopNodes->Node(state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber);
                    thisSpaceNode.Temp = thisZoneNode.Temp;
                    thisSpaceNode.HumRat = thisZoneNode.HumRat;
                    thisSpaceNode.Enthalpy = thisZoneNode.Enthalpy;
                }
                // If no SpaceHB or doing sizing, then set space temps and humrat to match zone
                thisSpaceHB.ZT = thisZoneHB.ZT;
                thisSpaceHB.ZTM = thisZoneHB.ZTM;
                thisSpaceHB.MAT = thisZoneHB.MAT;
                thisSpaceHB.airHumRat = thisZoneHB.airHumRat;
                thisSpaceHB.airRelHum = thisZoneHB.airRelHum;
                // thisSpaceHB.ZTAVComf = thisZoneHB.ZTAVComf;
            }
        }
        maxTempChange = max(maxTempChange, zoneTempChange);

        CalcZoneComponentLoadSums(
            state, zoneNum, &state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum), state.dataHeatBal->ZnAirRpt(zoneNum));
        if (state.dataHeatBal->doSpaceHeatBalanceSimulation) {
            for (int spaceNum : thisZone.spaceIndexes) {
                CalcZoneComponentLoadSums(
                    state, zoneNum, &state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum), state.dataHeatBal->spaceAirRpt(spaceNum));
            }
        }
    }
    return maxTempChange;
}

Real64 ZoneSpaceHeatBalanceData::correctAirTemp(
    EnergyPlusData &state,
    bool const useZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
    int const zoneNum,
    int const spaceNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russell Taylor
    //       MODIFIED       November 1999, LKL; November 2016 Sang Hoon Lee, Tianzhen Hong, Rongpeng Zhang;
    //       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
    //                      February 2008 (Brent Griffith reworked history )

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the zone air temperature and modifies the system
    // time step.

    static constexpr std::string_view RoutineName("correctAirTemp");

    Real64 tempChange = DataPrecisionGlobals::constant_zero; // Zone or space air temperature change between previous and current timestep

    assert(zoneNum > 0);
    auto &thisZone = state.dataHeatBal->Zone(zoneNum);

    // Update zone temperatures

    Real64 ZoneMult = thisZone.Multiplier * thisZone.ListMultiplier;

    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    // update the variables actually used in the balance equations.
    if (!useZoneTimeStepHistory) {
        this->ZTM = this->DSXMAT;
        this->WPrevZoneTSTemp = this->DSWPrevZoneTS;
    } else {
        this->ZTM = this->XMAT;
        this->WPrevZoneTSTemp = this->WPrevZoneTS;
    }

    Real64 volume = 0.0;
    if (spaceNum > 0) {
        volume = state.dataHeatBal->space(spaceNum).Volume;
    } else {
        volume = thisZone.Volume;
    }
    this->AirPowerCap = volume * thisZone.ZoneVolCapMultpSens *
                        Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, this->MAT, this->airHumRat, RoutineName) *
                        Psychrometrics::PsyCpAirFnW(this->airHumRat) / TimeStepSysSec;

    // SpaceHB TODO: For now, room air model is only for zones
    if (spaceNum == 0) {
        RoomAir::ManageAirModel(state, zoneNum);
    }

    // Calculate the various heat balance sums
    this->calcZoneOrSpaceSums(state, true, zoneNum, spaceNum);

    // Sum all convective internal gains except for people: SumIntGainExceptPeople
    if (state.dataHybridModel->FlagHybridModel_PC) {
        // TODO: For now, don't do space heat balance with hybrid model
        this->SumIntGainExceptPeople = InternalHeatGains::SumAllInternalConvectionGainsExceptPeople(state, zoneNum);
    }

    //    ZoneTempHistoryTerm = (3.0D0 * ZTM1(zoneNum) - (3.0D0/2.0D0) * ZTM2(zoneNum) + (1.0D0/3.0D0) * ZTM3(zoneNum))
    int ZoneNodeNum = thisZone.SystemZoneNodeNumber;
    if (spaceNum > 0) {
        ZoneNodeNum = state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber;
    }

    Real64 SNLoad = 0.0;

    if (ZoneNodeNum > 0) { // This zone is controlled by a zone equipment configuration or zone plenum
        auto &thisSystemNode = state.dataLoopNodes->Node(ZoneNodeNum);

        // Heat balance coefficients for controlled zone, i.e. with system air flow
        this->TempDepCoef = this->SumHA + this->SumMCp + this->SumSysMCp;
        this->TempIndCoef = this->SumIntGain + this->SumHATsurf - this->SumHATref + this->SumMCpT + this->SumSysMCpT +
                            (this->NonAirSystemResponse / ZoneMult + this->SysDepZoneLoadsLagged);

        if (state.afn->distribution_simulated) {
            this->TempIndCoef += state.afn->exchangeData(zoneNum).TotalSen;
        }

        // Solve for zone air temperature
        switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            this->ZT = (this->TempIndCoef + this->AirPowerCap * (3.0 * this->ZTM[0] - (3.0 / 2.0) * this->ZTM[1] + (1.0 / 3.0) * this->ZTM[2])) /
                       ((11.0 / 6.0) * this->AirPowerCap + this->TempDepCoef);
        } break;
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
            if (this->TempDepCoef == 0.0) { // B=0
                this->ZT = this->T1 + this->TempIndCoef / this->AirPowerCap;
            } else {
                this->ZT = (this->T1 - this->TempIndCoef / this->TempDepCoef) * std::exp(min(700.0, -this->TempDepCoef / this->AirPowerCap)) +
                           this->TempIndCoef / this->TempDepCoef;
            }
        } break;
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            this->ZT = (this->AirPowerCap * this->T1 + this->TempIndCoef) / (this->AirPowerCap + this->TempDepCoef);
        } break;
        default:
            break;
        }
        // Update zone node temperature and thermostat temperature unless already updated in Room Air Model,
        // calculate load correction factor
        if (!state.dataRoomAir->anyNonMixingRoomAirModel) {
            // Fully mixed
            thisSystemNode.Temp = this->ZT;
            // SpaceHB TODO: What to do here if this is for space
            if (spaceNum == 0) {
                state.dataHeatBalFanSys->TempTstatAir(zoneNum) = this->ZT;
            }
            state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0;
        } else {
            auto const &thisAirModel = state.dataRoomAir->AirModel(zoneNum);
            if ((thisAirModel.AirModel == RoomAir::RoomAirModel::Mixing) || (!thisAirModel.SimAirModel)) {
                // Fully mixed
                thisSystemNode.Temp = this->ZT;
                // SpaceHB TODO: What to do here if this is for space
                if (spaceNum == 0) {
                    state.dataHeatBalFanSys->TempTstatAir(zoneNum) = this->ZT;
                }
                state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0;
            } else if (state.dataRoomAir->IsZoneDispVent3Node(zoneNum) || state.dataRoomAir->IsZoneUFAD(zoneNum)) {
                // UCSDDV: Not fully mixed - calculate factor to correct load for fully mixed assumption
                // Space HB TODO: Space HB doesn't mix with DV etc.
                if (this->SumSysMCp > HVAC::SmallMassFlow) {
                    Real64 TempSupplyAir = this->SumSysMCpT / this->SumSysMCp; // Non-negligible flow, calculate supply air temperature
                    if (std::abs(TempSupplyAir - this->ZT) > state.dataHeatBal->TempConvergTol) {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = (TempSupplyAir - thisSystemNode.Temp) / (TempSupplyAir - this->ZT);
                        // constrain value to something reasonable
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = max(-3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum));
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = min(3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum));

                    } else {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0; // Indeterminate
                    }
                } else {
                    // Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
                    state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0;
                }
            } else if (thisAirModel.SimAirModel && ((thisAirModel.AirModel == RoomAir::RoomAirModel::UserDefined) ||
                                                    (thisAirModel.AirModel == RoomAir::RoomAirModel::DispVent1Node))) {
                if (this->SumSysMCp > HVAC::SmallMassFlow) {
                    Real64 TempSupplyAir = this->SumSysMCpT / this->SumSysMCp; // Non-negligible flow, calculate supply air temperature
                    if (std::abs(TempSupplyAir - this->ZT) > state.dataHeatBal->TempConvergTol) {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = (TempSupplyAir - thisSystemNode.Temp) / (TempSupplyAir - this->ZT);
                        // constrain value
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = max(-3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum));
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = min(3.0, state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum));

                    } else {
                        state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0; // Indeterminate
                    }
                } else {
                    // Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
                    state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0;
                }
            } else if (thisAirModel.AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                // Zone node used in the RoomAirflowNetwork model
                this->ZT = state.dataRoomAir->AFNZoneInfo(zoneNum).Node(state.dataRoomAir->AFNZoneInfo(zoneNum).ControlAirNodeID).AirTemp;
                thisSystemNode.Temp = this->ZT;
                // SpaceHB TODO: What to do here if this is for space
                if (spaceNum == 0) {
                    state.dataHeatBalFanSys->TempTstatAir(zoneNum) = this->ZT;
                }
                state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0;
            } else {
                thisSystemNode.Temp = this->ZT;
                // SpaceHB TODO: What to do here if this is for space
                if (spaceNum == 0) {
                    state.dataHeatBalFanSys->TempTstatAir(zoneNum) = this->ZT;
                }
                state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum) = 1.0;
            }
        }

        // Sensible load is the enthalpy into the zone minus the enthalpy that leaves the zone.
        Real64 CpAir = Psychrometrics::PsyCpAirFnW(this->airHumRat);
        Real64 ZoneEnthalpyIn = this->SumSysMCpT;

        // SNLOAD is the single zone load, without Zone Multiplier or Zone List Multiplier
        SNLoad = ZoneEnthalpyIn - (thisSystemNode.MassFlowRate / ZoneMult) * CpAir * thisSystemNode.Temp + this->NonAirSystemResponse / ZoneMult +
                 this->SysDepZoneLoadsLagged;

    } else {

        // Heat balance coefficients for uncontrolled zone, i.e. without system air flow
        this->TempDepCoef = this->SumHA + this->SumMCp;
        this->TempIndCoef = this->SumIntGain + this->SumHATsurf - this->SumHATref + this->SumMCpT;

        if (state.afn->distribution_simulated) {
            this->TempIndCoef += state.afn->exchangeData(zoneNum).TotalSen;
        }

        // Solve for zone air temperature
        switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            this->ZT = (this->TempIndCoef + this->AirPowerCap * (3.0 * this->ZTM[0] - (3.0 / 2.0) * this->ZTM[1] + (1.0 / 3.0) * this->ZTM[2])) /
                       ((11.0 / 6.0) * this->AirPowerCap + this->TempDepCoef);
            // Exact solution
        } break;
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
            if (this->TempDepCoef == 0.0) { // B=0
                this->ZT = this->T1 + this->TempIndCoef / this->AirPowerCap;
            } else {
                this->ZT = (this->T1 - this->TempIndCoef / this->TempDepCoef) * std::exp(min(700.0, -this->TempDepCoef / this->AirPowerCap)) +
                           this->TempIndCoef / this->TempDepCoef;
            }
        } break;
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            this->ZT = (this->AirPowerCap * this->T1 + this->TempIndCoef) / (this->AirPowerCap + this->TempDepCoef);
        } break;
        default:
            break;
        }

        // SpaceHB TODO: For now, room air model is only for zones
        if (spaceNum == 0 && state.dataRoomAir->anyNonMixingRoomAirModel) {
            if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                this->ZT = state.dataRoomAir->AFNZoneInfo(zoneNum).Node(state.dataRoomAir->AFNZoneInfo(zoneNum).ControlAirNodeID).AirTemp;
            }
        }

        // No sensible load
        SNLoad = 0.0;
    }

    // Hybrid modeling start
    // SpaceHB TODO: For now, hybrid model is only for zones
    if (spaceNum == 0 && state.dataHybridModel->FlagHybridModel) {
        if ((state.dataHybridModel->HybridModelZone(zoneNum).InfiltrationCalc_T ||
             state.dataHybridModel->HybridModelZone(zoneNum).InternalThermalMassCalc_T ||
             state.dataHybridModel->HybridModelZone(zoneNum).PeopleCountCalc_T) &&
            (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
            InverseModelTemperature(state,
                                    zoneNum,
                                    this->SumIntGain,
                                    this->SumIntGainExceptPeople,
                                    this->SumHA,
                                    this->SumHATsurf,
                                    this->SumHATref,
                                    this->SumMCp,
                                    this->SumMCpT,
                                    this->SumSysMCp,
                                    this->SumSysMCpT,
                                    this->AirPowerCap);
        }
    }

    this->MAT = this->ZT;

    // Determine sensible load heating/cooling rate and energy
    if (spaceNum > 0) {
        state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).reportZoneAirSystemSensibleLoads(state, SNLoad);
    } else {
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).reportZoneAirSystemSensibleLoads(state, SNLoad);
    }

    // Final humidity calcs
    this->correctHumRat(state, zoneNum, spaceNum);

    this->airHumRat = this->airHumRatTemp;
    this->airRelHum = 100.0 * Psychrometrics::PsyRhFnTdbWPb(state, this->ZT, this->airHumRat, state.dataEnvrn->OutBaroPress, RoutineName);

    // tempChange is used by HVACManager to determine if the timestep needs to be shortened.
    bool isMixed = true;
    // SpaceHB TODO: For now, room air model is only for zones
    if (spaceNum == 0 && state.dataRoomAir->anyNonMixingRoomAirModel) {
        isMixed = !((state.dataRoomAir->IsZoneDispVent3Node(zoneNum) && !state.dataRoomAir->ZoneDispVent3NodeMixedFlag(zoneNum)) ||
                    (state.dataRoomAir->IsZoneUFAD(zoneNum) && !state.dataRoomAir->ZoneUFADMixedFlag(zoneNum)));
    }
    switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
    case DataHeatBalance::SolutionAlgo::ThirdOrder: {
        if (isMixed) {
            tempChange = max(tempChange, std::abs(this->ZT - this->ZTM[0]));
        } else {
            tempChange = max(tempChange,
                             max(std::abs(state.dataRoomAir->ZTOC(zoneNum) - state.dataRoomAir->ZTMOC(zoneNum)[0]),
                                 std::abs(state.dataRoomAir->ZTMX(zoneNum) - state.dataRoomAir->ZTMMX(zoneNum)[0])));
        }
    } break;
    case DataHeatBalance::SolutionAlgo::AnalyticalSolution:
    case DataHeatBalance::SolutionAlgo::EulerMethod: {
        if (isMixed) {
            tempChange = max(tempChange, std::abs(this->ZT - this->T1));
        } else {
            tempChange = max(tempChange,
                             max(std::abs(state.dataRoomAir->ZTOC(zoneNum) - state.dataRoomAir->Zone1OC(zoneNum)),
                                 std::abs(state.dataRoomAir->ZTMX(zoneNum) - state.dataRoomAir->Zone1MX(zoneNum))));
        }
    } break;
    default:
        break;
    }

    return tempChange;
}

void PushZoneTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2008

    // PURPOSE OF THIS SUBROUTINE:
    // push histories for timestep advancing

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).pushZoneTimestepHistory(state, zoneNum);
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).pushZoneTimestepHistory(state, zoneNum, spaceNum);
            }
        }
    }
}

void ZoneSpaceHeatBalanceData::pushZoneTimestepHistory(EnergyPlusData &state, int const zoneNum, int const spaceNum)
{

    constexpr std::string_view routineName("pushTimestepHistories");
    assert(zoneNum > 0);

    auto &thisAirModel = state.dataRoomAir->AirModel(zoneNum);

    // Push the temperature and humidity ratio histories

    for (int iHistory = 3; iHistory >= 1; --iHistory) {
        this->XMAT[iHistory] = this->XMAT[iHistory - 1];
        this->WPrevZoneTS[iHistory] = this->WPrevZoneTS[iHistory - 1];
    }
    this->XMAT[0] = this->ZTAV; // using average for whole zone time step.
    this->XMPT = this->ZT;
    this->WPrevZoneTS[0] = this->airHumRatAvg; // using average for whole zone time step.
    this->airHumRat = this->airHumRatTemp;
    this->WTimeMinusP = this->airHumRatTemp;
    this->airRelHum = 100.0 * Psychrometrics::PsyRhFnTdbWPb(state, this->ZT, this->airHumRat, state.dataEnvrn->OutBaroPress, routineName);

    // SpaceHB TODO: For now, room air model is only for zones
    if (spaceNum == 0) {
        if (thisAirModel.AirModel == RoomAir::RoomAirModel::DispVent3Node || thisAirModel.AirModel == RoomAir::RoomAirModel::UFADInt ||
            thisAirModel.AirModel == RoomAir::RoomAirModel::UFADExt) {
            state.dataRoomAir->XMATFloor(zoneNum)[3] = state.dataRoomAir->XMATFloor(zoneNum)[2];
            state.dataRoomAir->XMATFloor(zoneNum)[2] = state.dataRoomAir->XMATFloor(zoneNum)[1];
            state.dataRoomAir->XMATFloor(zoneNum)[1] = state.dataRoomAir->XMATFloor(zoneNum)[0];
            state.dataRoomAir->XMATFloor(zoneNum)[0] = state.dataRoomAir->ZTFloor(zoneNum);
            state.dataRoomAir->MATFloor(zoneNum) = state.dataRoomAir->ZTFloor(zoneNum);

            state.dataRoomAir->XMATOC(zoneNum)[3] = state.dataRoomAir->XMATOC(zoneNum)[2];
            state.dataRoomAir->XMATOC(zoneNum)[2] = state.dataRoomAir->XMATOC(zoneNum)[1];
            state.dataRoomAir->XMATOC(zoneNum)[1] = state.dataRoomAir->XMATOC(zoneNum)[0];
            state.dataRoomAir->XMATOC(zoneNum)[0] = state.dataRoomAir->ZTOC(zoneNum);
            state.dataRoomAir->MATOC(zoneNum) = state.dataRoomAir->ZTOC(zoneNum);

            state.dataRoomAir->XMATMX(zoneNum)[3] = state.dataRoomAir->XMATMX(zoneNum)[2];
            state.dataRoomAir->XMATMX(zoneNum)[2] = state.dataRoomAir->XMATMX(zoneNum)[1];
            state.dataRoomAir->XMATMX(zoneNum)[1] = state.dataRoomAir->XMATMX(zoneNum)[0];
            state.dataRoomAir->XMATMX(zoneNum)[0] = state.dataRoomAir->ZTMX(zoneNum);
            state.dataRoomAir->MATMX(zoneNum) = state.dataRoomAir->ZTMX(zoneNum);
        }

        // for RoomAirflowNetwork model
        if (thisAirModel.AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
            for (auto &afnNode : state.dataRoomAir->AFNZoneInfo(zoneNum).Node) {
                afnNode.AirTempX[3] = afnNode.AirTempX[2];
                afnNode.AirTempX[2] = afnNode.AirTempX[1];
                afnNode.AirTempX[1] = afnNode.AirTempX[0];
                afnNode.AirTempX[0] = afnNode.AirTemp;

                afnNode.HumRatX[3] = afnNode.HumRatX[2];
                afnNode.HumRatX[2] = afnNode.HumRatX[1];
                afnNode.HumRatX[1] = afnNode.HumRatX[0];
                afnNode.HumRatX[0] = afnNode.HumRat;
            }
        }
    }

    if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
        this->TM2 = this->TMX;
        this->TMX = this->ZTAV; // using average for whole zone time step.
        this->WM2 = this->WMX;
        this->WMX = this->airHumRatAvg; // using average for whole zone time step.
        // SpaceHB TODO: For now, room air model is only for zones
        if (spaceNum == 0) {
            if (thisAirModel.AirModel == RoomAir::RoomAirModel::DispVent3Node || thisAirModel.AirModel == RoomAir::RoomAirModel::UFADInt ||
                thisAirModel.AirModel == RoomAir::RoomAirModel::UFADExt) {
                state.dataRoomAir->ZoneM2Floor(zoneNum) = state.dataRoomAir->ZoneMXFloor(zoneNum);
                state.dataRoomAir->ZoneMXFloor(zoneNum) = state.dataRoomAir->ZTFloor(zoneNum); // using average for whole zone time step.
                state.dataRoomAir->ZoneM2OC(zoneNum) = state.dataRoomAir->ZoneMXOC(zoneNum);
                state.dataRoomAir->ZoneMXOC(zoneNum) = state.dataRoomAir->ZTOC(zoneNum); // using average for whole zone time step.
                state.dataRoomAir->ZoneM2MX(zoneNum) = state.dataRoomAir->ZoneMXMX(zoneNum);
                state.dataRoomAir->ZoneMXMX(zoneNum) = state.dataRoomAir->ZTMX(zoneNum); // using average for whole zone time step.
            }

            if (thisAirModel.AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                for (auto &afnNode : state.dataRoomAir->AFNZoneInfo(zoneNum).Node) {
                    afnNode.AirTempT2 = afnNode.AirTempTX;
                    afnNode.AirTempTX = afnNode.AirTemp;

                    afnNode.HumRatT2 = afnNode.HumRatTX;
                    afnNode.HumRatTX = afnNode.HumRat;
                }
            }
        }
    }
}

void PushSystemTimestepHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2008

    // PURPOSE OF THIS SUBROUTINE:
    // Push the temperature and humidity ratio histories back in time

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).pushSystemTimestepHistory(state, zoneNum);
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).pushSystemTimestepHistory(state, zoneNum, spaceNum);
            }
        }
    }
}

void ZoneSpaceHeatBalanceData::pushSystemTimestepHistory(EnergyPlusData &state, int const zoneNum, int const spaceNum)
{
    assert(zoneNum > 0);
    for (int iHistory = 3; iHistory >= 1; --iHistory) {
        this->DSXMAT[iHistory] = this->DSXMAT[iHistory - 1];
        this->DSWPrevZoneTS[iHistory] = this->DSWPrevZoneTS[iHistory - 1];
    }
    this->DSXMAT[0] = this->MAT;
    this->DSWPrevZoneTS[0] = this->airHumRat;

    // SpaceHB TODO: For now, room air model is only for zones
    if (spaceNum == 0 && state.dataRoomAir->anyNonMixingRoomAirModel) {
        if (state.dataRoomAir->IsZoneDispVent3Node(zoneNum) || state.dataRoomAir->IsZoneUFAD(zoneNum)) {
            state.dataRoomAir->DSXMATFloor(zoneNum)[3] = state.dataRoomAir->DSXMATFloor(zoneNum)[2];
            state.dataRoomAir->DSXMATFloor(zoneNum)[2] = state.dataRoomAir->DSXMATFloor(zoneNum)[1];
            state.dataRoomAir->DSXMATFloor(zoneNum)[1] = state.dataRoomAir->DSXMATFloor(zoneNum)[0];
            state.dataRoomAir->DSXMATFloor(zoneNum)[0] = state.dataRoomAir->MATFloor(zoneNum);

            state.dataRoomAir->DSXMATOC(zoneNum)[3] = state.dataRoomAir->DSXMATOC(zoneNum)[2];
            state.dataRoomAir->DSXMATOC(zoneNum)[2] = state.dataRoomAir->DSXMATOC(zoneNum)[1];
            state.dataRoomAir->DSXMATOC(zoneNum)[1] = state.dataRoomAir->DSXMATOC(zoneNum)[0];
            state.dataRoomAir->DSXMATOC(zoneNum)[0] = state.dataRoomAir->MATOC(zoneNum);

            state.dataRoomAir->DSXMATMX(zoneNum)[3] = state.dataRoomAir->DSXMATMX(zoneNum)[2];
            state.dataRoomAir->DSXMATMX(zoneNum)[2] = state.dataRoomAir->DSXMATMX(zoneNum)[1];
            state.dataRoomAir->DSXMATMX(zoneNum)[1] = state.dataRoomAir->DSXMATMX(zoneNum)[0];
            state.dataRoomAir->DSXMATMX(zoneNum)[0] = state.dataRoomAir->MATMX(zoneNum);
        }
        if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
            for (auto &afnNode : state.dataRoomAir->AFNZoneInfo(zoneNum).Node) {
                afnNode.AirTempDSX[3] = afnNode.AirTempDSX[2];
                afnNode.AirTempDSX[2] = afnNode.AirTempDSX[1];
                afnNode.AirTempDSX[1] = afnNode.AirTempDSX[0];
                afnNode.AirTempDSX[0] = afnNode.AirTemp;

                afnNode.HumRatDSX[3] = afnNode.HumRatDSX[2];
                afnNode.HumRatDSX[2] = afnNode.HumRatDSX[1];
                afnNode.HumRatDSX[1] = afnNode.HumRatDSX[0];
                afnNode.HumRatDSX[0] = afnNode.HumRat;
            }
        }
    }

    if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
        this->TM2 = this->TMX;
        this->TMX = this->MAT; // using average for whole zone time step.
        this->WM2 = this->WMX;
        this->WMX = this->airHumRatTemp; // using average for whole zone time step.

        // SpaceHB TODO: For now, room air model is only for zones
        if (spaceNum == 0) {
            if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::DispVent3Node ||
                state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::UFADInt ||
                state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::UFADExt) {
                state.dataRoomAir->ZoneM2Floor(zoneNum) = state.dataRoomAir->ZoneMXFloor(zoneNum);
                state.dataRoomAir->ZoneMXFloor(zoneNum) = state.dataRoomAir->ZTFloor(zoneNum); // using average for whole zone time step.
                state.dataRoomAir->ZoneM2OC(zoneNum) = state.dataRoomAir->ZoneMXOC(zoneNum);
                state.dataRoomAir->ZoneMXOC(zoneNum) = state.dataRoomAir->ZTOC(zoneNum); // using average for whole zone time step.
                state.dataRoomAir->ZoneM2MX(zoneNum) = state.dataRoomAir->ZoneMXMX(zoneNum);
                state.dataRoomAir->ZoneMXMX(zoneNum) = state.dataRoomAir->ZTMX(zoneNum); // using average for whole zone time step.
            }
            if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                for (int LoopNode = 1; LoopNode <= state.dataRoomAir->AFNZoneInfo(zoneNum).NumOfAirNodes; ++LoopNode) {
                    auto &afnNode = state.dataRoomAir->AFNZoneInfo(zoneNum).Node(LoopNode);
                    afnNode.AirTempT2 = afnNode.AirTempTX;
                    afnNode.AirTempTX = afnNode.AirTemp;

                    afnNode.HumRatT2 = afnNode.HumRatTX;
                    afnNode.HumRatTX = afnNode.HumRat;
                }
            }
        }
    }
}

void RevertZoneTimestepHistories(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2008

    // PURPOSE OF THIS SUBROUTINE:
    // Revert the temperature and humidity ratio histories

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).revertZoneTimestepHistory(state, zoneNum);
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).revertZoneTimestepHistory(state, zoneNum, spaceNum);
            }
        }
    }
}

void ZoneSpaceHeatBalanceData::revertZoneTimestepHistory(EnergyPlusData &state, int const zoneNum, int const spaceNum)
{
    assert(zoneNum > 0);

    for (int iHistory = 0; iHistory <= 2; ++iHistory) {
        this->XMAT[iHistory] = this->XMAT[iHistory + 1];
        this->WPrevZoneTS[iHistory] = this->WPrevZoneTS[iHistory + 1];
    }

    // SpaceHB TODO: For now, room air model is only for zones
    if (spaceNum == 0) {
        if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::DispVent3Node ||
            state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::UFADInt ||
            state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::UFADExt) {

            state.dataRoomAir->XMATFloor(zoneNum)[0] = state.dataRoomAir->XMATFloor(zoneNum)[1];
            state.dataRoomAir->XMATFloor(zoneNum)[1] = state.dataRoomAir->XMATFloor(zoneNum)[2];
            state.dataRoomAir->XMATFloor(zoneNum)[2] = state.dataRoomAir->XMATFloor(zoneNum)[3];

            state.dataRoomAir->XMATOC(zoneNum)[0] = state.dataRoomAir->XMATOC(zoneNum)[1];
            state.dataRoomAir->XMATOC(zoneNum)[1] = state.dataRoomAir->XMATOC(zoneNum)[2];
            state.dataRoomAir->XMATOC(zoneNum)[2] = state.dataRoomAir->XMATOC(zoneNum)[3];

            state.dataRoomAir->XMATMX(zoneNum)[0] = state.dataRoomAir->XMATMX(zoneNum)[1];
            state.dataRoomAir->XMATMX(zoneNum)[1] = state.dataRoomAir->XMATMX(zoneNum)[2];
            state.dataRoomAir->XMATMX(zoneNum)[3] = state.dataRoomAir->XMATMX(zoneNum)[3];
        }

        if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
            for (auto &afnNode : state.dataRoomAir->AFNZoneInfo(zoneNum).Node) {
                afnNode.AirTempX[0] = afnNode.AirTempX[1];
                afnNode.AirTempX[1] = afnNode.AirTempX[2];
                afnNode.AirTempX[2] = afnNode.AirTempX[3];

                afnNode.HumRatX[0] = afnNode.HumRatX[1];
                afnNode.HumRatX[1] = afnNode.HumRatX[2];
                afnNode.HumRatX[2] = afnNode.HumRatX[3];
            }
        }
    }
}

void ZoneSpaceHeatBalanceData::correctHumRat(EnergyPlusData &state, int const zoneNum, int const spaceNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   2000
    // REFERENCES: Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron for BLAST.

    assert(zoneNum > 0);
    static constexpr std::string_view RoutineName("correctHumRat");

    Real64 MoistureMassFlowRate = 0.0;
    Real64 ZoneMassFlowRate = 0.0;
    auto &zone = state.dataHeatBal->Zone(zoneNum);
    int ZoneMult = zone.Multiplier * zone.ListMultiplier;
    bool ControlledZoneAirFlag = zone.IsControlled;
    bool ZoneRetPlenumAirFlag = zone.IsReturnPlenum;
    bool ZoneSupPlenumAirFlag = zone.IsSupplyPlenum;

    if (ControlledZoneAirFlag) { // If there is system flow then calculate the flow rates
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(zoneNum);
        // Calculate moisture flow rate into each zone
        for (int NodeNum = 1; NodeNum <= zoneEquipConfig.NumInletNodes; ++NodeNum) {
            auto const &inletNode = state.dataLoopNodes->Node(zoneEquipConfig.InletNode(NodeNum));
            MoistureMassFlowRate += (inletNode.MassFlowRate * inletNode.HumRat) / ZoneMult;
            ZoneMassFlowRate += inletNode.MassFlowRate / ZoneMult;
        }

        // Do the calculations for the plenum zone
    } else if (ZoneRetPlenumAirFlag) {
        int ZoneRetPlenumNum = zone.PlenumCondNum;
        auto &zoneRetPlenCond = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum);
        for (int NodeNum = 1; NodeNum <= zoneRetPlenCond.NumInletNodes; ++NodeNum) {
            auto const &inletNode = state.dataLoopNodes->Node(zoneRetPlenCond.InletNode(NodeNum));
            MoistureMassFlowRate += (inletNode.MassFlowRate * inletNode.HumRat) / ZoneMult;
            ZoneMassFlowRate += inletNode.MassFlowRate / ZoneMult;
        }
        // add in the leak flow
        for (int ADUListIndex = 1; ADUListIndex <= zoneRetPlenCond.NumADUs; ++ADUListIndex) {
            int ADUNum = zoneRetPlenCond.ADUIndex(ADUListIndex);
            auto const &airDistUnit = state.dataDefineEquipment->AirDistUnit(ADUNum);
            if (airDistUnit.UpStreamLeak) {
                int ADUInNode = airDistUnit.InletNodeNum;
                MoistureMassFlowRate += (airDistUnit.MassFlowRateUpStrLk * state.dataLoopNodes->Node(ADUInNode).HumRat) / ZoneMult;
                ZoneMassFlowRate += airDistUnit.MassFlowRateUpStrLk / ZoneMult;
            }
            if (airDistUnit.DownStreamLeak) {
                int ADUOutNode = airDistUnit.OutletNodeNum;
                MoistureMassFlowRate += (airDistUnit.MassFlowRateDnStrLk * state.dataLoopNodes->Node(ADUOutNode).HumRat) / ZoneMult;
                ZoneMassFlowRate += airDistUnit.MassFlowRateDnStrLk / ZoneMult;
            }
        }

    } else if (ZoneSupPlenumAirFlag) {
        int ZoneSupPlenumNum = zone.PlenumCondNum;
        auto const &inletNode = state.dataLoopNodes->Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode);
        MoistureMassFlowRate += (inletNode.MassFlowRate * inletNode.HumRat) / ZoneMult;
        ZoneMassFlowRate += inletNode.MassFlowRate / ZoneMult;
    }

    // Calculate hourly humidity ratio from infiltration + humidity added from latent load + system added moisture
    Real64 LatentGain = this->latentGain + state.dataHeatBalFanSys->SumLatentHTRadSys(zoneNum) + state.dataHeatBalFanSys->SumLatentPool(zoneNum);

    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    // Calculate the coefficients for the 3rd order derivative for final
    // zone humidity ratio.  The A, B, C coefficients are analogous to the
    // heat balance.  There are 2 cases that should be considered, system
    // operating and system shutdown.

    Real64 const RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, this->ZT, this->airHumRat, RoutineName);
    Real64 const H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(this->airHumRat, this->ZT);

    Real64 B = (LatentGain / H2OHtOfVap) + ((this->OAMFL + this->VAMFL + this->CTMFL) * state.dataEnvrn->OutHumRat) + this->EAMFLxHumRat +
               (MoistureMassFlowRate) + this->SumHmARaW + this->MixingMassFlowXHumRat + this->MDotOA * state.dataEnvrn->OutHumRat;
    Real64 A = ZoneMassFlowRate + this->OAMFL + this->VAMFL + this->EAMFL + this->CTMFL + this->SumHmARa + this->MixingMassFlowZone + this->MDotOA;

    if (state.afn->multizone_always_simulated ||
        (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation &&
         state.afn->AirflowNetworkFanActivated)) {
        auto const &exchangeData = state.afn->exchangeData(zoneNum);
        // Multizone airflow calculated in AirflowNetwork
        B = (LatentGain / H2OHtOfVap) + (exchangeData.SumMHrW + exchangeData.SumMMHrW) + (MoistureMassFlowRate) + this->SumHmARaW;
        A = ZoneMassFlowRate + exchangeData.SumMHr + exchangeData.SumMMHr + this->SumHmARa;
    }
    Real64 C = RhoAir * zone.Volume * zone.ZoneVolCapMultpMoist / TimeStepSysSec;

    if (state.afn->distribution_simulated) {
        B += state.afn->exchangeData(zoneNum).TotalLat;
    }

    // Use a 3rd order derivative to predict final zone humidity ratio and
    // smooth the changes using the zone air capacitance.
    // auto &zoneAirHumRatTemp = this->ZoneAirHumRatTemp;
    // auto &zoneW1 = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum).ZoneW1;
    switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
    case DataHeatBalance::SolutionAlgo::ThirdOrder: {
        this->airHumRatTemp =
            (B + C * (3.0 * this->WPrevZoneTSTemp[0] - (3.0 / 2.0) * this->WPrevZoneTSTemp[1] + (1.0 / 3.0) * this->WPrevZoneTSTemp[2])) /
            ((11.0 / 6.0) * C + A);
        // Exact solution
    } break;
    case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
        if (A == 0.0) { // B=0
            this->airHumRatTemp = this->W1 + B / C;
        } else {
            this->airHumRatTemp = (this->W1 - B / A) * std::exp(min(700.0, -A / C)) + B / A;
        }
    } break;
    case DataHeatBalance::SolutionAlgo::EulerMethod: {
        this->airHumRatTemp = (C * this->W1 + B) / (C + A);
    } break;
    default:
        break;
    }

    // Set the humidity ratio to zero if the zone has been dried out
    if (this->airHumRatTemp < 0.0) this->airHumRatTemp = 0.0;

    // Check to make sure that is saturated there is condensation in the zone
    // by resetting to saturation conditions.
    Real64 const WZSat = Psychrometrics::PsyWFnTdbRhPb(state, this->ZT, 1.0, state.dataEnvrn->OutBaroPress, RoutineName);

    if (this->airHumRatTemp > WZSat) this->airHumRatTemp = WZSat;

    if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
        this->airHumRatTemp = state.dataRoomAir->AFNZoneInfo(zoneNum).Node(state.dataRoomAir->AFNZoneInfo(zoneNum).ControlAirNodeID).HumRat;
    }

    // HybridModel with measured humidity ratio begins
    // SpaceHB TODO: For now, hybrid model is only for zones
    if (spaceNum == 0 && state.dataHybridModel->FlagHybridModel) {
        if ((state.dataHybridModel->HybridModelZone(zoneNum).InfiltrationCalc_H ||
             state.dataHybridModel->HybridModelZone(zoneNum).PeopleCountCalc_H) &&
            (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
            Real64 LatentGainExceptPeople = 0.0;
            if (state.dataHybridModel->HybridModelZone(zoneNum).PeopleCountCalc_H) {
                LatentGainExceptPeople = this->latentGainExceptPeople + state.dataHeatBalFanSys->SumLatentHTRadSys(zoneNum) +
                                         state.dataHeatBalFanSys->SumLatentPool(zoneNum);
            }

            InverseModelHumidity(state, zoneNum, LatentGain, LatentGainExceptPeople, ZoneMassFlowRate, MoistureMassFlowRate, H2OHtOfVap, RhoAir);
        }
    }

    // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
    int ZoneNodeNum = zone.SystemZoneNodeNumber;
    if (spaceNum > 0) {
        ZoneNodeNum = state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber;
    }
    if (ZoneNodeNum > 0) {
        state.dataLoopNodes->Node(ZoneNodeNum).HumRat = this->airHumRatTemp;
        state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(this->ZT, this->airHumRatTemp);
    }
    if (state.dataHeatBal->DoLatentSizing) {
        Real64 sensibleLoad = 0.0;
        Real64 pSat = Psychrometrics::PsyPsatFnTemp(state, this->ZT, RoutineName);
        Real64 Tdp = Psychrometrics::PsyTdpFnWPb(state, this->airHumRatTemp, state.dataEnvrn->StdBaroPress);
        Real64 vaporPressureDiff = pSat - Psychrometrics::PsyPsatFnTemp(state, Tdp, RoutineName);
        if (spaceNum > 0) {
            sensibleLoad = state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).airSysHeatRate +
                           state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).airSysCoolRate;
            state.dataZoneEnergyDemand->spaceSysMoistureDemand(spaceNum).reportZoneAirSystemMoistureLoads(
                state, LatentGain, sensibleLoad, vaporPressureDiff);
        } else {
            sensibleLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).airSysHeatRate +
                           state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).airSysCoolRate;
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(zoneNum).reportZoneAirSystemMoistureLoads(
                state, LatentGain, sensibleLoad, vaporPressureDiff);
        }
    }
}

void DownInterpolate4HistoryValues(Real64 const OldTimeStep,
                                   Real64 const NewTimeStep,
                                   Real64 const oldVal0,
                                   Real64 const oldVal1,
                                   Real64 const oldVal2,
                                   Real64 &newVal0,
                                   Real64 &newVal1,
                                   Real64 &newVal2,
                                   Real64 &newVal3,
                                   Real64 &newVal4)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Feb 2008

    // PURPOSE OF THIS SUBROUTINE:
    // provide a reusable routine for the various places that need to
    // interpolate a new set of history values on a different time scale
    // Once the systemtimestep has shortened, the new history terms need to be interpolated

    // METHODOLOGY EMPLOYED:
    // This routine assumes that the direction is to a shorter timestep.
    // The down step ratio, DSRatio = OldTimeStep/ NewTimeStep
    //  is expected to be roughly integer-valued and near 2.0 or 3.0 or 4.0 or more.

    // old math variables
    // Real64 const oldTime0 = 0.0;
    // Real64 const oldTime1 = oldTime0 - OldTimeStep;
    // Real64 const newTime0 = 0.0;
    // Real64 const newTime1 = newTime0 - NewTimeStep;
    // Real64 const newTime2 = newTime1 - NewTimeStep;
    // Real64 const newTime3 = newTime2 - NewTimeStep;
    // Real64 const newTime4 = newTime3 - NewTimeStep;

    Real64 constexpr realTWO = 2.0;
    Real64 constexpr realTHREE = 3.0;
    // first determine the ratio of system time step to zone time step
    Real64 const DSRatio = OldTimeStep / NewTimeStep; // should pretty much be an integer value 2, 3, 4, etc.

    newVal0 = oldVal0;

    if (std::abs(DSRatio - realTWO) < 0.01) { // DSRatio = 2
        // when DSRatio = 2 the 1st point lies exactly between old points, and 2nd point is old 1st point
        // first two points lie between oldVal0 and oldVal1
        // old math example
        // newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep));
        // newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep));
        newVal1 = (oldVal0 + oldVal1) / realTWO;
        newVal2 = oldVal1;
        // when DSRatio = 2 the 3rd point lies exactly between old points, and 4th point is old 2nd point
        // last two points lie between oldVal1 and oldVal2
        // newVal3 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime3) / (OldTimeStep));
        // newVal4 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime4) / (OldTimeStep));
        newVal3 = (oldVal1 + oldVal2) / realTWO;
        newVal4 = oldVal2;
    } else if (std::abs(DSRatio - realTHREE) < 0.01) { // DSRatio = 3
        // when DSRatio = 3 the 1st point lies 1/3 way between old points, and 2nd and 3rd points are 2/3 and 3/3 the way
        // first three points lie between oldVal0 and oldVal1
        // newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep));
        // newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep));
        // newVal3 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime3) / (OldTimeStep));
        Real64 delta10 = (oldVal1 - oldVal0) / realTHREE;
        newVal1 = oldVal0 + delta10;
        newVal2 = newVal1 + delta10;
        newVal3 = oldVal1;
        // last point lies 1/3 way between oldVal1 and oldVal2
        // newVal4 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime4) / (OldTimeStep));
        newVal4 = oldVal1 + (oldVal2 - oldVal1) / realTHREE;

    } else { // DSRatio = 4 or more
        // all new points lie between oldVal0 and oldVal1 (if DSRatio = 4, newVal4 = oldVal1)
        // newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep));
        // newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep));
        // newVal3 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime3) / (OldTimeStep));
        // newVal4 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime4) / (OldTimeStep));
        Real64 delta10 = (oldVal1 - oldVal0) / DSRatio;
        newVal1 = oldVal0 + delta10;
        newVal2 = newVal1 + delta10;
        newVal3 = newVal2 + delta10;
        newVal4 = newVal3 + delta10;
    }
}

Real64 DownInterpolate4HistoryValues(Real64 OldTimeStep, Real64 NewTimeStep, std::array<Real64, 4> const &oldVals, std::array<Real64, 4> &newVals)
{
    Real64 constexpr realTWO = 2.0;
    Real64 constexpr realTHREE = 3.0;
    // first determine the ratio of system time step to zone time step
    Real64 const DSRatio = OldTimeStep / NewTimeStep; // should pretty much be an integer value 2, 3, 4, etc.

    newVals[0] = oldVals[0];

    if (std::abs(DSRatio - realTWO) < 0.01) { // DSRatio = 2
        // first point lies exactly between (oldVals[0] and oldVals[1])
        newVals[1] = (oldVals[0] + oldVals[1]) / realTWO;
        // 2nd point is oldVal[1] and last point lies exactly between (oldVals[1] and oldVals[2])
        newVals[2] = oldVals[1];
        newVals[3] = (oldVals[1] + oldVals[2]) / realTWO;

    } else if (std::abs(DSRatio - realTHREE) < 0.01) { // DSRatio = 3
        // first two points lie between (oldVals[0] and oldVals[1])
        Real64 delta10 = (oldVals[1] - oldVals[0]) / realTHREE;
        newVals[1] = oldVals[0] + delta10;
        newVals[2] = newVals[1] + delta10;
        // last point is oldVals[1]
        newVals[3] = oldVals[1];

    } else { // DSRatio = 4 or more
        // all new points lie between (oldVals[0] and oldVals[1])
        Real64 delta10 = (oldVals[1] - oldVals[0]) / DSRatio;
        newVals[1] = oldVals[0] + delta10;
        newVals[2] = newVals[1] + delta10;
        newVals[3] = newVals[2] + delta10;
    }
    return oldVals[0];
}
void InverseModelTemperature(EnergyPlusData &state,
                             int const ZoneNum,                   // Zone number
                             Real64 const SumIntGain,             // Zone sum of convective internal gains
                             Real64 const SumIntGainExceptPeople, // Zone sum of convective internal gains except for people
                             Real64 const SumHA,                  // Zone sum of Hc*Area
                             Real64 const SumHATsurf,             // Zone sum of Hc*Area*Tsurf
                             Real64 const SumHATref,              // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
                             Real64 const SumMCp,                 // Zone sum of MassFlowRate*Cp
                             Real64 const SumMCpT,                // Zone sum of MassFlowRate*Cp*T
                             Real64 const SumSysMCp,              // Zone sum of air system MassFlowRate*Cp
                             Real64 const SumSysMCpT,             // Zone sum of air system MassFlowRate*Cp*T
                             Real64 const AirCap                  // Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"rd
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Han Li
    //       DATE WRITTEN   February 2019

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine inversely solve infiltration airflow rate or people count with zone air temperatures measurements.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AirCapHM(0.0); // Air power capacity for hybrid modeling
    Real64 AA(0.0);
    Real64 BB(0.0);
    Real64 FractionConvection(0.0); // Default convection portion of the sensible heat from people

    auto &zone = state.dataHeatBal->Zone(ZoneNum);
    auto &hybridModelZone = state.dataHybridModel->HybridModelZone(ZoneNum);
    auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum);

    int ZoneMult = zone.Multiplier * zone.ListMultiplier;
    zone.ZoneMeasuredTemperature = ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneMeasuredTemperatureSchedulePtr);
    zone.ZoneVolCapMultpSensHM = 1.0; // Initialize to 1.0 in case hybrid not active

    // HM calculation only HM calculation period start
    if (state.dataEnvrn->DayOfYear >= hybridModelZone.HybridStartDayOfYear && state.dataEnvrn->DayOfYear <= hybridModelZone.HybridEndDayOfYear) {
        Real64 MultpHM(1.0);

        thisZoneHB.ZT = zone.ZoneMeasuredTemperature; // Array1D<Real64> ZT -- Zone
                                                      // Air Temperature Averaged over
                                                      // the System Time Increment
        if (hybridModelZone.InfiltrationCalc_T && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            static constexpr std::string_view RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");

            if (hybridModelZone.IncludeSystemSupplyParameters) {
                zone.ZoneMeasuredSupplyAirTemperature =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirTemperatureSchedulePtr);
                zone.ZoneMeasuredSupplyAirFlowRate =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirMassFlowRateSchedulePtr);
                zone.ZoneMeasuredSupplyAirHumidityRatio =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirHumidityRatioSchedulePtr);
                // Calculate the air humidity ratio at supply air inlet.
                Real64 CpAirInlet(0.0);
                CpAirInlet = Psychrometrics::PsyCpAirFnW(zone.ZoneMeasuredSupplyAirHumidityRatio);

                Real64 SumSysMCp_HM = zone.ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                Real64 SumSysMCpT_HM = zone.ZoneMeasuredSupplyAirFlowRate * CpAirInlet * zone.ZoneMeasuredSupplyAirTemperature;

                AA = SumSysMCp_HM + SumHA + thisZoneHB.MCPV + thisZoneHB.MCPM + thisZoneHB.MCPE + thisZoneHB.MCPC + thisZoneHB.MDotCPOA;
                BB = SumSysMCpT_HM + SumIntGain + SumHATsurf - SumHATref + thisZoneHB.MCPTV + thisZoneHB.MCPTM + thisZoneHB.MCPTE + thisZoneHB.MCPTC +
                     thisZoneHB.MDotCPOA * zone.OutDryBulbTemp + (thisZoneHB.NonAirSystemResponse / ZoneMult + thisZoneHB.SysDepZoneLoadsLagged);
            } else {
                AA = SumHA + thisZoneHB.MCPV + thisZoneHB.MCPM + thisZoneHB.MCPE + thisZoneHB.MCPC + thisZoneHB.MDotCPOA;
                BB = SumIntGain + SumHATsurf - SumHATref + thisZoneHB.MCPTV + thisZoneHB.MCPTM + thisZoneHB.MCPTE + thisZoneHB.MCPTC +
                     thisZoneHB.MDotCPOA * zone.OutDryBulbTemp;
            }
            Real64 CC = AirCap;
            Real64 DD =
                (3.0 * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) +
                 (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum));

            Real64 delta_T = (zone.ZoneMeasuredTemperature - zone.OutDryBulbTemp);
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
            Real64 AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, zone.OutDryBulbTemp, state.dataEnvrn->OutHumRat, RoutineNameInfiltration);
            zone.delta_T = delta_T;

            // s4 - Set ACH to 0 when delta_T <= 0.5, add max and min limits to ach
            Real64 M_inf = 0.0;
            if (std::abs(delta_T) > 0.5) {
                M_inf = (BB + CC * DD - ((11.0 / 6.0) * CC + AA) * zone.ZoneMeasuredTemperature) / (CpAir * delta_T);
            }
            Real64 ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / zone.Volume * Constant::SecInHour));
            M_inf = (ACH_inf / Constant::SecInHour) * zone.Volume * AirDensity;

            // Overwrite variable with inverse solution
            zone.MCPIHM = M_inf;
            zone.InfilOAAirChangeRateHM = ACH_inf;

        } // Hybrid model infiltration calculation end

        // Hybrid modeling internal thermal mass calculation start
        if (hybridModelZone.InternalThermalMassCalc_T && SumSysMCpT == 0 && thisZoneHB.ZT != state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) &&
            state.dataHVACGlobal->UseZoneTimeStepHistory) { // HM calculation only when SumSysMCpT =0,
                                                            // TimeStepZone (not @ TimeStepSys)
            Real64 TempDepCoef = SumHA + SumMCp + SumSysMCp;
            Real64 TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                                 (thisZoneHB.NonAirSystemResponse / ZoneMult + thisZoneHB.SysDepZoneLoadsLagged);
            //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

            if (state.afn->distribution_simulated) {
                TempIndCoef += state.afn->exchangeData(ZoneNum).TotalSen;
            }
            // Calculate air capacity using DataHeatBalance::SolutionAlgo::AnalyticalSolution
            if (TempDepCoef == 0.0) {
                // Is this correct? Shouldn't we use log?? What if thisZT ==
                // PreviousMeasuredZT1(ZoneNum)??
                AirCapHM = TempIndCoef / (thisZoneHB.ZT - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)); // Inverse equation
            } else {
                Real64 AirCapHM_temp = 0.0;
                if (TempIndCoef == TempDepCoef * thisZoneHB.ZT) {
                    AirCapHM_temp = 0.0; //  This is the denominator.
                } else {
                    AirCapHM_temp = (TempIndCoef - TempDepCoef * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)) /
                                    (TempIndCoef - TempDepCoef * thisZoneHB.ZT);
                }

                if ((AirCapHM_temp > 0) && (AirCapHM_temp != 1)) {    // Avoid IND
                    AirCapHM = TempDepCoef / std::log(AirCapHM_temp); // Inverse equation
                } else {
                    AirCapHM = TempIndCoef / (thisZoneHB.ZT - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum));
                }
            }

            // Calculate multiplier
            if (std::abs(thisZoneHB.ZT - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)) > 0.05) { // Filter
                MultpHM = AirCapHM /
                          (zone.Volume *
                           Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                             state.dataEnvrn->OutBaroPress,
                                                             thisZoneHB.ZT,
                                                             thisZoneHB.airHumRat) *
                           Psychrometrics::PsyCpAirFnW(thisZoneHB.airHumRat)) *
                          (state.dataGlobal->TimeStepZone * Constant::SecInHour); // Inverse equation
            } else {
                MultpHM = 1.0; // Default value 1.0
            }

            processInverseModelMultpHM(
                state, MultpHM, zone.ZoneVolCapMultpSensHMSum, zone.ZoneVolCapMultpSensHMCountSum, zone.ZoneVolCapMultpSensHMAverage, ZoneNum);
            zone.ZoneVolCapMultpSensHM = MultpHM;

        } // Hybrid model internal thermal mass calculation end

        // Hybrid model people count calculation
        if (hybridModelZone.PeopleCountCalc_T && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            zone.ZoneMeasuredTemperature = ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneMeasuredTemperatureSchedulePtr);
            zone.ZonePeopleActivityLevel = ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleActivityLevelSchedulePtr);
            zone.ZonePeopleSensibleHeatFraction =
                ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleSensibleFractionSchedulePtr);
            zone.ZonePeopleRadiantHeatFraction =
                ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleRadiationFractionSchedulePtr);

            Real64 FractionSensible = zone.ZonePeopleSensibleHeatFraction;
            Real64 FractionRadiation = zone.ZonePeopleRadiantHeatFraction;
            Real64 ActivityLevel = ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleActivityLevelSchedulePtr);

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

            if (hybridModelZone.IncludeSystemSupplyParameters) {
                zone.ZoneMeasuredSupplyAirTemperature =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirTemperatureSchedulePtr);
                zone.ZoneMeasuredSupplyAirFlowRate =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirMassFlowRateSchedulePtr);
                zone.ZoneMeasuredSupplyAirHumidityRatio =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirHumidityRatioSchedulePtr);

                // Calculate the air humidity ratio at supply air inlet.
                Real64 CpAirInlet = Psychrometrics::PsyCpAirFnW(zone.ZoneMeasuredSupplyAirHumidityRatio);

                Real64 SumSysMCp_HM = zone.ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                Real64 SumSysMCpT_HM = zone.ZoneMeasuredSupplyAirFlowRate * CpAirInlet * zone.ZoneMeasuredSupplyAirTemperature;

                AA = SumSysMCp_HM + SumHA + SumMCp;
                BB = SumSysMCpT_HM + SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT +
                     (thisZoneHB.NonAirSystemResponse / ZoneMult + thisZoneHB.SysDepZoneLoadsLagged);
            } else {
                AA = SumHA + SumMCp;
                BB = SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT;
            }

            Real64 CC = AirCap;
            Real64 DD =
                (3.0 * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) +
                 (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum));

            Real64 SumIntGainPeople = ((11.0 / 6.0) * CC + AA) * zone.ZoneMeasuredTemperature - BB - CC * DD;
            Real64 UpperBound = max(0.0, SumIntGain / (ActivityLevel * FractionSensible * FractionConvection));
            Real64 NumPeople = min(UpperBound, max(0.0, SumIntGainPeople / (ActivityLevel * FractionSensible * FractionConvection)));

            if (NumPeople < 0.05) {
                NumPeople = 0;
            }
            zone.NumOccHM = NumPeople;
        }
    }

    // Update zone temperatures in the previous steps
    state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) = thisZoneHB.ZT;
}

void processInverseModelMultpHM(EnergyPlusData &state,
                                Real64 &multiplierHM, // Hybrid model thermal mass multiplier
                                Real64 &multSumHM,    // Sum of Hybrid model thermal mass multipliers
                                Real64 &countSumHM,   // Count of number of points in sum
                                Real64 &multAvgHM,    // Average of hybrid model mass multiplier
                                int zoneNum           // Zone number for the hybrid model
)
{
    Real64 constexpr minHMMultValue = 1.0;
    Real64 constexpr maxHMMultValue = 30.0;

    auto &zone = state.dataHeatBal->Zone(zoneNum);
    auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum);

    // Apply limits and generate warnings as needed
    if (multiplierHM < minHMMultValue) { // don't allow this to be less than minimum (potential for instability)
        multiplierHM = minHMMultValue;
    } else if (multiplierHM > maxHMMultValue) { // as per suggestions in Defect #10508, only warn if greater than the max
        if (thisZoneHB.hmThermalMassMultErrIndex == 0) {
            ShowWarningMessage(state, format("Hybrid model thermal mass multiplier higher than the limit for {}", zone.Name));
            ShowContinueError(state, "This means that the ratio of the zone air heat capacity for the current time step to the");
            ShowContinueError(state, format("zone air heat storage is higher than the maximum limit of {:.1R}.", maxHMMultValue));
        }
        ShowRecurringWarningErrorAtEnd(
            state, "Hybrid model thermal mass multiplier limit exceeded in zone " + zone.Name, thisZoneHB.hmThermalMassMultErrIndex);
    }

    // Update running totals (but only when there is a valid multiplier, i.e. multiplier is greater than min but not higher than the max)
    if (multiplierHM > minHMMultValue) {
        multSumHM += multiplierHM;
        countSumHM++;
    }

    // Calculate average (always so that it does get calculated)
    if (countSumHM >= 1) multAvgHM = multSumHM / countSumHM;
}

void InverseModelHumidity(EnergyPlusData &state,
                          int const ZoneNum,                   // Zone number
                          Real64 const LatentGain,             // Zone sum of latent gain
                          Real64 const LatentGainExceptPeople, // Zone sum of latent gain except for people
                          Real64 const ZoneMassFlowRate,       // Zone air mass flow rate
                          Real64 const MoistureMassFlowRate,   // Zone moisture mass flow rate
                          Real64 const H2OHtOfVap,             // Heat of vaporization of air
                          Real64 const RhoAir                  // Air density
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Han Li
    //       DATE WRITTEN   February 2019

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine inversely solve infiltration airflow rate or people count with zone air humidity measurements.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InverseModelHumidity");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AA(0.0);
    Real64 BB(0.0);
    Real64 ActivityLevel(0.0);
    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    auto &zone = state.dataHeatBal->Zone(ZoneNum);
    auto &hybridModelZone = state.dataHybridModel->HybridModelZone(ZoneNum);
    auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum);

    // Get measured zone humidity ratio
    zone.ZoneMeasuredHumidityRatio = ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneMeasuredHumidityRatioSchedulePtr);

    if (state.dataEnvrn->DayOfYear >= hybridModelZone.HybridStartDayOfYear && state.dataEnvrn->DayOfYear <= hybridModelZone.HybridEndDayOfYear) {
        thisZoneHB.airHumRat = zone.ZoneMeasuredHumidityRatio;

        // Hybrid Model calculate air infiltration rate
        if (hybridModelZone.InfiltrationCalc_H && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            // Conditionally calculate the time dependent and time independent terms
            if (hybridModelZone.IncludeSystemSupplyParameters) {
                zone.ZoneMeasuredSupplyAirFlowRate =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirMassFlowRateSchedulePtr);
                zone.ZoneMeasuredSupplyAirHumidityRatio =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirHumidityRatioSchedulePtr);

                Real64 SumSysM_HM = zone.ZoneMeasuredSupplyAirFlowRate;
                Real64 SumSysMHumRat_HM = zone.ZoneMeasuredSupplyAirFlowRate * zone.ZoneMeasuredSupplyAirHumidityRatio;

                AA = SumSysM_HM + thisZoneHB.VAMFL + thisZoneHB.EAMFL + thisZoneHB.CTMFL + thisZoneHB.SumHmARa + thisZoneHB.MixingMassFlowZone +
                     thisZoneHB.MDotOA;
                BB = SumSysMHumRat_HM + (LatentGain / H2OHtOfVap) + ((thisZoneHB.VAMFL + thisZoneHB.CTMFL) * state.dataEnvrn->OutHumRat) +
                     thisZoneHB.EAMFLxHumRat + thisZoneHB.SumHmARaW + thisZoneHB.MixingMassFlowXHumRat +
                     thisZoneHB.MDotOA * state.dataEnvrn->OutHumRat;
            } else {
                AA = thisZoneHB.VAMFL + thisZoneHB.EAMFL + thisZoneHB.CTMFL + thisZoneHB.SumHmARa + thisZoneHB.MixingMassFlowZone + thisZoneHB.MDotOA;
                BB = (LatentGain / H2OHtOfVap) + ((thisZoneHB.VAMFL + thisZoneHB.CTMFL) * state.dataEnvrn->OutHumRat) + thisZoneHB.EAMFLxHumRat +
                     thisZoneHB.SumHmARaW + thisZoneHB.MixingMassFlowXHumRat + thisZoneHB.MDotOA * state.dataEnvrn->OutHumRat;
            }

            Real64 CC = RhoAir * zone.Volume * zone.ZoneVolCapMultpMoist / TimeStepSysSec;
            Real64 DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) -
                         (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) +
                         (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum));

            Real64 delta_HR = (zone.ZoneMeasuredHumidityRatio - state.dataEnvrn->OutHumRat);

            Real64 AirDensity =
                Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, zone.OutDryBulbTemp, state.dataEnvrn->OutHumRat, RoutineName);

            Real64 M_inf = 0.0;
            if (std::abs(zone.ZoneMeasuredHumidityRatio - state.dataEnvrn->OutHumRat) > 0.0000001) {
                M_inf = (CC * DD + BB - ((11.0 / 6.0) * CC + AA) * zone.ZoneMeasuredHumidityRatio) / delta_HR;
            }

            // Add threshold for air change rate
            Real64 ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / zone.Volume * Constant::SecInHour));
            M_inf = (ACH_inf / Constant::SecInHour) * zone.Volume * AirDensity;
            zone.MCPIHM = M_inf;
            zone.InfilOAAirChangeRateHM = ACH_inf;
        }

        // Hybrid Model calculate people count
        if (hybridModelZone.PeopleCountCalc_H && state.dataHVACGlobal->UseZoneTimeStepHistory) {
            zone.ZonePeopleActivityLevel = ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleActivityLevelSchedulePtr);
            zone.ZonePeopleSensibleHeatFraction =
                ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleSensibleFractionSchedulePtr);
            zone.ZonePeopleRadiantHeatFraction =
                ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZonePeopleRadiationFractionSchedulePtr);

            Real64 FractionSensible = zone.ZonePeopleSensibleHeatFraction;

            if (FractionSensible <= 0.0) {
                FractionSensible = 0.6;
            }

            if (ActivityLevel <= 0.0) {
                ActivityLevel = 130.0;
            }

            // Conditionally calculate the humidity-dependent and humidity-independent
            // terms.
            if (hybridModelZone.IncludeSystemSupplyParameters) {
                zone.ZoneMeasuredSupplyAirFlowRate =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirMassFlowRateSchedulePtr);
                zone.ZoneMeasuredSupplyAirHumidityRatio =
                    ScheduleManager::GetCurrentScheduleValue(state, hybridModelZone.ZoneSupplyAirHumidityRatioSchedulePtr);

                Real64 SumSysM_HM = zone.ZoneMeasuredSupplyAirFlowRate;
                Real64 SumSysMHumRat_HM = zone.ZoneMeasuredSupplyAirFlowRate * zone.ZoneMeasuredSupplyAirHumidityRatio;

                AA = SumSysM_HM + thisZoneHB.OAMFL + thisZoneHB.VAMFL + thisZoneHB.EAMFL + thisZoneHB.CTMFL + thisZoneHB.SumHmARa +
                     thisZoneHB.MixingMassFlowZone + thisZoneHB.MDotOA;
                BB = SumSysMHumRat_HM + (LatentGainExceptPeople / H2OHtOfVap) +
                     ((thisZoneHB.OAMFL + thisZoneHB.VAMFL + thisZoneHB.CTMFL) * state.dataEnvrn->OutHumRat) + thisZoneHB.EAMFLxHumRat +
                     thisZoneHB.SumHmARaW + thisZoneHB.MixingMassFlowXHumRat + thisZoneHB.MDotOA * state.dataEnvrn->OutHumRat;
            } else {
                AA = ZoneMassFlowRate + thisZoneHB.OAMFL + thisZoneHB.VAMFL + thisZoneHB.EAMFL + thisZoneHB.CTMFL + thisZoneHB.SumHmARa +
                     thisZoneHB.MixingMassFlowZone + thisZoneHB.MDotOA;
                BB = (LatentGainExceptPeople / H2OHtOfVap) + ((thisZoneHB.OAMFL + thisZoneHB.VAMFL + thisZoneHB.CTMFL) * state.dataEnvrn->OutHumRat) +
                     thisZoneHB.EAMFLxHumRat + (MoistureMassFlowRate) + thisZoneHB.SumHmARaW + thisZoneHB.MixingMassFlowXHumRat +
                     thisZoneHB.MDotOA * state.dataEnvrn->OutHumRat;
            }

            Real64 CC = RhoAir * zone.Volume * zone.ZoneVolCapMultpMoist / TimeStepSysSec;
            Real64 DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) -
                         (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) +
                         (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum));

            Real64 LatentGainPeople = (((11.0 / 6.0) * CC + AA) * zone.ZoneMeasuredHumidityRatio - BB - CC * DD) * H2OHtOfVap;
            Real64 UpperBound = max(0.0, LatentGain / (ActivityLevel * (1.0 - FractionSensible)));
            Real64 NumPeople = min(UpperBound, max(0.0, LatentGainPeople / (ActivityLevel * (1.0 - FractionSensible))));
            NumPeople = floor(NumPeople * 100.00 + 0.5) / 100.00;
            if (NumPeople < 0.05) {
                NumPeople = 0;
            }
            zone.NumOccHM = NumPeople;
        }
    }

    // Update zone humidity ratio in the previous steps
    state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum);
    state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) = zone.ZoneMeasuredHumidityRatio;
}

void ZoneSpaceHeatBalanceData::calcZoneOrSpaceSums(EnergyPlusData &state,
                                                   bool const CorrectorFlag, // Corrector call flag
                                                   int const zoneNum,
                                                   int const spaceNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   July 2003
    //       MODIFIED       Aug 2003, FCW: add this->SumHA contributions from window frame and divider
    //                      Aug 2003, CC: change how the reference temperatures are used

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the various sums that go into the zone heat balance
    // equation.  This replaces the SUMC, SumHA, and SumHAT calculations that were
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
    assert(zoneNum > 0);

    this->SumHA = 0.0;
    this->SumHATsurf = 0.0;
    this->SumHATref = 0.0;
    this->SumSysMCp = 0.0;
    this->SumSysMCpT = 0.0;
    // Sum all convective internal gains: this->SumIntGain
    if (spaceNum == 0) {
        this->SumIntGain = InternalHeatGains::zoneSumAllInternalConvectionGains(state, zoneNum);
    } else {
        this->SumIntGain = InternalHeatGains::spaceSumAllInternalConvectionGains(state, spaceNum);
    }
    this->SumIntGain += state.dataHeatBalFanSys->SumConvHTRadSys(zoneNum) + state.dataHeatBalFanSys->SumConvPool(zoneNum);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very low or zero)
    assert(zoneNum > 0);
    auto &thisZone = state.dataHeatBal->Zone(zoneNum);
    if (thisZone.NoHeatToReturnAir) {
        if (spaceNum == 0) {
            this->SumIntGain += InternalHeatGains::zoneSumAllReturnAirConvectionGains(state, zoneNum, 0);
        } else {
            this->SumIntGain += InternalHeatGains::spaceSumAllReturnAirConvectionGains(state, spaceNum, 0);
        }
    }

    // Sum all non-system air flow, i.e. infiltration, simple ventilation, mixing, earth tube: this->SumMCp, this->SumMCpT
    this->SumMCp = this->MCPI + this->MCPV + this->MCPM + this->MCPE + this->MCPC + this->MDotCPOA;
    this->SumMCpT = this->MCPTI + this->MCPTV + this->MCPTM + this->MCPTE + this->MCPTC + this->MDotCPOA * thisZone.OutDryBulbTemp;

    // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
    if (state.afn->multizone_always_simulated ||
        (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation &&
         state.afn->AirflowNetworkFanActivated)) {
        auto const &exchangeData = state.afn->exchangeData(zoneNum);
        this->SumMCp = exchangeData.SumMCp + exchangeData.SumMVCp + exchangeData.SumMMCp;
        this->SumMCpT = exchangeData.SumMCpT + exchangeData.SumMVCpT + exchangeData.SumMMCpT;
    }

    // Sum all system air flow: this->SumSysMCp, this->SumSysMCpT and check to see if this is a controlled zone
    // If the space is controlled, use space supply nodes, otherwise use zone supply nodes and allocate later
    bool isSpaceControlled = (spaceNum > 0 && state.dataZoneEquip->spaceEquipConfig(spaceNum).IsControlled);
    if (CorrectorFlag) {
        // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
        if (thisZone.IsControlled) {
            auto const &zsec = (isSpaceControlled ? state.dataZoneEquip->spaceEquipConfig(spaceNum) : state.dataZoneEquip->ZoneEquipConfig(zoneNum));
            for (int NodeNum = 1, NodeNum_end = zsec.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum) {
                // Get node conditions, this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call?
                //  how can we tell?  predict step must be lagged ?  correct step, systems have run.
                auto const &node(state.dataLoopNodes->Node(zsec.InletNode(NodeNum)));
                Real64 CpAir = Psychrometrics::PsyCpAirFnW(this->airHumRat);
                Real64 const MassFlowRate_CpAir(node.MassFlowRate * CpAir);
                this->SumSysMCp += MassFlowRate_CpAir;
                this->SumSysMCpT += MassFlowRate_CpAir * node.Temp;
            }

        } else if (thisZone.IsReturnPlenum) {
            auto const &zrpc(state.dataZonePlenum->ZoneRetPlenCond(thisZone.PlenumCondNum));
            Real64 const air_hum_rat(this->airHumRat);
            for (int NodeNum = 1, NodeNum_end = zrpc.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum) {
                auto const &node(state.dataLoopNodes->Node(zrpc.InletNode(NodeNum)));
                Real64 const MassFlowRate_CpAir(node.MassFlowRate * Psychrometrics::PsyCpAirFnW(air_hum_rat));
                this->SumSysMCp += MassFlowRate_CpAir;
                this->SumSysMCpT += MassFlowRate_CpAir * node.Temp;
            }
            // add in the leaks
            for (int ADUListIndex = 1, ADUListIndex_end = zrpc.NumADUs; ADUListIndex <= ADUListIndex_end; ++ADUListIndex) {
                auto &airDistUnit = state.dataDefineEquipment->AirDistUnit(zrpc.ADUIndex(ADUListIndex));
                if (airDistUnit.UpStreamLeak) {
                    Real64 const MassFlowRate_CpAir(airDistUnit.MassFlowRateUpStrLk * Psychrometrics::PsyCpAirFnW(air_hum_rat));
                    this->SumSysMCp += MassFlowRate_CpAir;
                    this->SumSysMCpT += MassFlowRate_CpAir * state.dataLoopNodes->Node(airDistUnit.InletNodeNum).Temp;
                }
                if (airDistUnit.DownStreamLeak) {
                    Real64 const MassFlowRate_CpAir(airDistUnit.MassFlowRateDnStrLk * Psychrometrics::PsyCpAirFnW(air_hum_rat));
                    this->SumSysMCp += MassFlowRate_CpAir;
                    this->SumSysMCpT += MassFlowRate_CpAir * state.dataLoopNodes->Node(airDistUnit.OutletNodeNum).Temp;
                }
            }

        } else if (thisZone.IsSupplyPlenum) {
            Real64 MassFlowRate = state.dataLoopNodes->Node(state.dataZonePlenum->ZoneSupPlenCond(thisZone.PlenumCondNum).InletNode).MassFlowRate;
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(this->airHumRat);
            this->SumSysMCp += MassFlowRate * CpAir;
            this->SumSysMCpT +=
                MassFlowRate * CpAir * state.dataLoopNodes->Node(state.dataZonePlenum->ZoneSupPlenCond(thisZone.PlenumCondNum).InletNode).Temp;
        }

        int ZoneMult = thisZone.Multiplier * thisZone.ListMultiplier;

        this->SumSysMCp /= ZoneMult;
        this->SumSysMCpT /= ZoneMult;
    }

    if (spaceNum > 0 && !isSpaceControlled) {
        // If space is not controlled, allocate zone-level airflow by volume
        Real64 spaceFrac = state.dataHeatBal->space(spaceNum).fracZoneVolume;
        this->SumSysMCp *= spaceFrac;
        this->SumSysMCpT *= spaceFrac;
    }

    // Sum all surface convection: this->SumHA, this->SumHATsurf, this->SumHATref (and additional contributions to this->SumIntGain)
    SumHATOutput sumHATResults; // space or zone return values
    sumHATResults = this->calcSumHAT(state, zoneNum, spaceNum);
    this->SumIntGain += sumHATResults.sumIntGain;
    this->SumHA = sumHATResults.sumHA;
    this->SumHATsurf = sumHATResults.sumHATsurf;
    this->SumHATref = sumHATResults.sumHATref;
}

SumHATOutput ZoneHeatBalanceData::calcSumHAT(EnergyPlusData &state, int const zoneNum, [[maybe_unused]] int const spaceNum)
{
    assert(zoneNum > 0);
    assert(spaceNum == 0);
    SumHATOutput zoneResults; // zone-level return values
    for (int zoneSpaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
        SumHATOutput spaceResults; // temporary return value from space-level calcSumHAT
        spaceResults = state.dataZoneTempPredictorCorrector->spaceHeatBalance(zoneSpaceNum).calcSumHAT(state, zoneNum, zoneSpaceNum);
        zoneResults.sumIntGain += spaceResults.sumIntGain;
        zoneResults.sumHA += spaceResults.sumHA;
        zoneResults.sumHATsurf += spaceResults.sumHATsurf;
        zoneResults.sumHATref += spaceResults.sumHATref;
    }
    return zoneResults;
}

SumHATOutput SpaceHeatBalanceData::calcSumHAT(EnergyPlusData &state, int const zoneNum, int const spaceNum)
{
    assert(zoneNum > 0);
    assert(spaceNum > 0);
    auto &thisZone = state.dataHeatBal->Zone(zoneNum);
    auto &thisSpace = state.dataHeatBal->space(spaceNum);
    SumHATOutput results; // space-level return values

    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
        Real64 HA = 0.0;
        Real64 Area = state.dataSurface->Surface(SurfNum).Area; // For windows, this is the glazing area

        if (state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) {
            DataSurfaces::WinShadingType const shading_flag = state.dataSurface->SurfWinShadingFlag(SurfNum);

            // Add to the convective internal gains
            if (ANY_INTERIOR_SHADE_BLIND(shading_flag)) {
                // The shade area covers the area of the glazing plus the area of the dividers.
                Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                // If interior shade or blind is present it is assumed that both the convective and IR radiative gain
                // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
                // interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
                // at the same time that the interaction between glass and shade is calculated.
                results.sumIntGain += state.dataSurface->SurfWinDividerHeatGain(SurfNum);
            }

            // Other convection term is applicable to equivalent layer window (ASHWAT) model
            if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL)
                results.sumIntGain += state.dataSurface->SurfWinOtherConvHeatGain(SurfNum);

            // Convective heat gain from natural convection in gap between glass and interior shade or blind
            if (ANY_INTERIOR_SHADE_BLIND(shading_flag)) results.sumIntGain += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

            // Convective heat gain from airflow window
            if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                results.sumIntGain += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                if (thisZone.NoHeatToReturnAir) {
                    results.sumIntGain += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
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
                results.sumHATsurf += HA_surf * state.dataSurface->SurfWinFrameTempIn(SurfNum);
                HA += HA_surf;
            }

            if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && !ANY_INTERIOR_SHADE_BLIND(shading_flag)) {
                // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                Real64 const HA_surf(state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                     (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)));
                results.sumHATsurf += HA_surf * state.dataSurface->SurfWinDividerTempIn(SurfNum);
                HA += HA_surf;
            }

        } // End of check if window

        HA += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area;
        results.sumHATsurf += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area * state.dataHeatBalSurf->SurfTempInTmp(SurfNum);

        // determine reference air temperature for this surface
        switch (state.dataSurface->SurfTAirRef(SurfNum)) {
        case DataSurfaces::RefAirTemp::ZoneMeanAirTemp:
            // The zone air is the reference temperature (which is to be solved for in CorrectZoneAirTemp).
            results.sumHA += HA;
            break;
        case DataSurfaces::RefAirTemp::AdjacentAirTemp:
            results.sumHATref += HA * state.dataHeatBal->SurfTempEffBulkAir(SurfNum);
            break;
        case DataSurfaces::RefAirTemp::ZoneSupplyAirTemp:
            // check whether this zone is a controlled zone or not
            if (!thisZone.IsControlled) {
                ShowFatalError(state,
                               format("Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone {}", thisZone.Name));
                return results;
            }
            // determine supply air temperature as a weighted average of the inlet temperatures.
            // TODO: For now, use zone-level values for system flow
            if (state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).SumSysMCp > 0.0) {
                results.sumHATref += HA * state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).SumSysMCpT /
                                     state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum).SumSysMCp;
            } else {
                // no system flow (yet) so just use zone air temperature #5906
                results.sumHA += HA;
            }
            break;
        default:
            // currently set to mean air temp but should add error warning here
            results.sumHA += HA;
            break;
        }

    } // SurfNum
    return results;
}
void CalcZoneComponentLoadSums(EnergyPlusData &state,
                               int ZoneNum, // Zone number
                               ZoneTempPredictorCorrector::ZoneSpaceHeatBalanceData *thisHB,
                               DataHeatBalance::AirReportVars &thisAirRpt)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Feb 2008

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the various sums that go into the zone heat balance
    // equation for reporting (and diagnostic) purposes only.
    // It was derived from CalcZonethisAirRpt.Sums but differs in that that routine
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

    thisAirRpt.SumIntGains = 0.0;    // Zone sum of convective internal gains
    thisAirRpt.SumHADTsurfs = 0.0;   // Zone sum of Hc*Area*(Tsurf - Tz)
    thisAirRpt.SumMCpDTzones = 0.0;  // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
    thisAirRpt.SumMCpDtInfil = 0.0;  // Zone sum of MassFlowRate*Cp*(Tout - Tz)
    thisAirRpt.SumMCpDTsystem = 0.0; // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
    thisAirRpt.SumNonAirSystem = 0.0;
    thisAirRpt.CzdTdt = 0.0;
    thisAirRpt.imBalance = 0.0;
    thisAirRpt.SumEnthalpyM = 0.0;
    thisAirRpt.SumEnthalpyH = 0.0;

    auto &thisZone = state.dataHeatBal->Zone(ZoneNum);

    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    // Sum all convective internal gains: SumIntGain
    thisAirRpt.SumIntGains = InternalHeatGains::zoneSumAllInternalConvectionGains(state, ZoneNum);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    // low or zero)
    if (thisZone.NoHeatToReturnAir) {
        thisAirRpt.SumIntGains += InternalHeatGains::zoneSumAllReturnAirConvectionGains(state, ZoneNum, 0);
    }

    // sum non-system air flow transfers between zones
    thisAirRpt.SumMCpDTzones = thisHB->MCPTM - thisHB->MCPM * thisHB->MAT; // but maybe it should be ZTAV(ZoneNum)

    // Sum non-system air flow, i.e. infiltration, simple ventilation, earth tube
    //  reuse SumMCp, SumMCpT from CalcZoneSum but use MAT (or maybe ZTAV?) to complete
    thisAirRpt.SumMCpDtInfil = (thisHB->MCPTI - thisHB->MCPI * thisHB->MAT) + (thisHB->MCPTV - thisHB->MCPV * thisHB->MAT) +
                               (thisHB->MCPTE - thisHB->MCPE * thisHB->MAT) + (thisHB->MCPTC - thisHB->MCPC * thisHB->MAT) +
                               (thisHB->MDotCPOA * thisZone.OutDryBulbTemp -
                                thisHB->MDotCPOA * thisHB->MAT); // infiltration | Ventilation (simple) | Earth tube. | Cooltower | combined OA flow

    // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model (if used)
    if (state.afn->multizone_always_simulated ||
        (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation &&
         state.afn->AirflowNetworkFanActivated)) {
        // Multizone airflow calculated in AirflowNetwork
        thisAirRpt.SumMCpDtInfil = state.afn->exchangeData(ZoneNum).SumMCpT + state.afn->exchangeData(ZoneNum).SumMVCpT -
                                   (state.afn->exchangeData(ZoneNum).SumMCp + state.afn->exchangeData(ZoneNum).SumMVCp) * thisHB->MAT;
        thisAirRpt.SumMCpDTzones = state.afn->exchangeData(ZoneNum).SumMMCpT - state.afn->exchangeData(ZoneNum).SumMMCp * thisHB->MAT;
    }

    // Sum all system air flow: reusing how SumSysMCp, SumSysMCpT are calculated in CalcZoneSums
    // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
    Real64 QSensRate = 0.0;
    if (thisZone.IsControlled) {
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ZoneNum);
        for (int NodeNum = 1; NodeNum <= zoneEquipConfig.NumInletNodes; ++NodeNum) {
            // Get node conditions
            Real64 const NodeTemp = state.dataLoopNodes->Node(zoneEquipConfig.InletNode(NodeNum)).Temp;
            Real64 const MassFlowRate = state.dataLoopNodes->Node(zoneEquipConfig.InletNode(NodeNum)).MassFlowRate;
            QSensRate = calcZoneSensibleOutput(MassFlowRate, NodeTemp, thisHB->MAT, thisHB->airHumRat);
            thisAirRpt.SumMCpDTsystem += QSensRate;

            if (zoneEquipConfig.InletNodeADUNum(NodeNum) > 0) {
                auto &airDistUnit = state.dataDefineEquipment->AirDistUnit(zoneEquipConfig.InletNodeADUNum(NodeNum));
                Real64 ADUHeatAddRate = calcZoneSensibleOutput(state.dataLoopNodes->Node(airDistUnit.OutletNodeNum).MassFlowRate,
                                                               state.dataLoopNodes->Node(airDistUnit.OutletNodeNum).Temp,
                                                               thisHB->MAT,
                                                               thisHB->airHumRat);
                airDistUnit.HeatRate = max(0.0, ADUHeatAddRate);
                airDistUnit.CoolRate = std::abs(min(0.0, ADUHeatAddRate));
                airDistUnit.HeatGain = airDistUnit.HeatRate * TimeStepSysSec;
                airDistUnit.CoolGain = airDistUnit.CoolRate * TimeStepSysSec;
            }
        }

    } else if (thisZone.IsReturnPlenum) {
        auto &zoneRetPlenCond = state.dataZonePlenum->ZoneRetPlenCond(thisZone.PlenumCondNum);
        for (int NodeNum = 1; NodeNum <= zoneRetPlenCond.NumInletNodes; ++NodeNum) {
            QSensRate = calcZoneSensibleOutput(state.dataLoopNodes->Node(zoneRetPlenCond.InletNode(NodeNum)).MassFlowRate,
                                               state.dataLoopNodes->Node(zoneRetPlenCond.InletNode(NodeNum)).Temp,
                                               thisHB->MAT,
                                               thisHB->airHumRat);
            thisAirRpt.SumMCpDTsystem += QSensRate;
        }
        // add in the leaks
        for (int ADUListIndex = 1; ADUListIndex <= zoneRetPlenCond.NumADUs; ++ADUListIndex) {
            auto &airDistUnit = state.dataDefineEquipment->AirDistUnit(zoneRetPlenCond.ADUIndex(ADUListIndex));
            if (airDistUnit.UpStreamLeak) {
                QSensRate = calcZoneSensibleOutput(
                    airDistUnit.MassFlowRateUpStrLk, state.dataLoopNodes->Node(airDistUnit.InletNodeNum).Temp, thisHB->MAT, thisHB->airHumRat);
                thisAirRpt.SumMCpDTsystem += QSensRate;
            }
            if (airDistUnit.DownStreamLeak) {
                QSensRate = calcZoneSensibleOutput(
                    airDistUnit.MassFlowRateDnStrLk, state.dataLoopNodes->Node(airDistUnit.OutletNodeNum).Temp, thisHB->MAT, thisHB->airHumRat);
                thisAirRpt.SumMCpDTsystem += QSensRate;
            }
        }

    } else if (thisZone.IsSupplyPlenum) {
        auto &zoneSupPlenCond = state.dataZonePlenum->ZoneSupPlenCond(thisZone.PlenumCondNum);
        QSensRate = calcZoneSensibleOutput(state.dataLoopNodes->Node(zoneSupPlenCond.InletNode).MassFlowRate,
                                           state.dataLoopNodes->Node(zoneSupPlenCond.InletNode).Temp,
                                           thisHB->MAT,
                                           thisHB->airHumRat);
        thisAirRpt.SumMCpDTsystem += QSensRate;
    }

    // non air system response.
    thisAirRpt.SumNonAirSystem =
        thisHB->NonAirSystemResponse + state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumConvPool(ZoneNum);

    // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
    for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
        auto const &thisSpace = state.dataHeatBal->space(spaceNum);
        for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {

            Real64 Area = state.dataSurface->Surface(SurfNum).Area; // For windows, this is the glazing area
            Real64 RefAirTemp = state.dataSurface->Surface(SurfNum).getInsideAirTemperature(state, SurfNum);

            if (state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) {

                // Add to the convective internal gains
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // The shade area covers the area of the glazing plus the area of the dividers.
                    Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                    // If interior shade or blind is present it is assumed that both the convective and IR radiative gain
                    // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
                    // interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
                    // at the same time that the interaction between glass and shade is calculated.
                    thisAirRpt.SumIntGains += state.dataSurface->SurfWinDividerHeatGain(SurfNum);
                }

                // Other convection term is applicable to equivalent layer window (ASHWAT) model
                if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL)
                    thisAirRpt.SumIntGains += state.dataSurface->SurfWinOtherConvHeatGain(SurfNum);

                // Convective heat gain from natural convection in gap between glass and interior shade or blind
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)))
                    thisAirRpt.SumIntGains += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

                // Convective heat gain from airflow window
                if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                    thisAirRpt.SumIntGains += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                    if (thisZone.NoHeatToReturnAir) {
                        thisAirRpt.SumIntGains += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                    }
                }

                // Add to the surface convection sums
                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    thisAirRpt.SumHADTsurfs += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                                               (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) *
                                               (state.dataSurface->SurfWinFrameTempIn(SurfNum) - RefAirTemp);
                }

                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 &&
                    !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    thisAirRpt.SumHADTsurfs += state.dataHeatBalSurf->SurfHConvInt(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                               (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                               (state.dataSurface->SurfWinDividerTempIn(SurfNum) - RefAirTemp);
                }

            } // End of check if window

            thisAirRpt.SumHADTsurfs +=
                state.dataHeatBalSurf->SurfHConvInt(SurfNum) * Area * (state.dataHeatBalSurf->SurfTempInTmp(SurfNum) - RefAirTemp);

            // Accumulate Zone Phase Change Material Melting/Freezing Enthalpy output variables
            if (state.dataSurface->Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::CondFD) {
                thisAirRpt.SumEnthalpyM += state.dataHeatBalFiniteDiffMgr->SurfaceFD(SurfNum).EnthalpyM;
                thisAirRpt.SumEnthalpyH += state.dataHeatBalFiniteDiffMgr->SurfaceFD(SurfNum).EnthalpyF;
            }
        }
    }
    // now calculate air energy storage source term.
    // capacitance is volume * density * heat capacity
    Real64 CpAir = Psychrometrics::PsyCpAirFnW(thisHB->airHumRat);
    Real64 RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisHB->MAT, thisHB->airHumRat);

    switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
    case DataHeatBalance::SolutionAlgo::ThirdOrder: {
        thisAirRpt.CzdTdt = RhoAir * CpAir * thisZone.Volume * thisZone.ZoneVolCapMultpSens * (thisHB->MAT - thisHB->ZTM[0]) / TimeStepSysSec;
        // Exact solution
    } break;
    case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
        thisAirRpt.CzdTdt = thisHB->TempIndCoef - thisHB->TempDepCoef * thisHB->MAT;
    } break;
    case DataHeatBalance::SolutionAlgo::EulerMethod: {
        thisAirRpt.CzdTdt = thisHB->AirPowerCap * (thisHB->MAT - thisHB->T1);
    } break;
    default:
        break;
    }

    if (state.dataGlobal->DisplayZoneAirHeatBalanceOffBalance) {
        thisAirRpt.imBalance = thisAirRpt.SumIntGains + thisAirRpt.SumHADTsurfs + thisAirRpt.SumMCpDTzones + thisAirRpt.SumMCpDtInfil +
                               thisAirRpt.SumMCpDTsystem + thisAirRpt.SumNonAirSystem - thisAirRpt.CzdTdt;

        // throw warning if seriously out of balance (this may need to be removed if too noisy... )
        // formulate dynamic threshold value based on 20% of quadrature sum of components
        Real64 Threshold = 0.2 * std::sqrt(pow_2(thisAirRpt.SumIntGains) + pow_2(thisAirRpt.SumHADTsurfs) + pow_2(thisAirRpt.SumMCpDTzones) +
                                           pow_2(thisAirRpt.SumMCpDtInfil) + pow_2(thisAirRpt.SumMCpDTsystem) + pow_2(thisAirRpt.SumNonAirSystem) +
                                           pow_2(thisAirRpt.CzdTdt));
        if ((std::abs(thisAirRpt.imBalance) > Threshold) && (!state.dataGlobal->WarmupFlag) &&
            (!state.dataGlobal->DoingSizing)) { // air balance is out by more than threshold
            if (thisZone.AirHBimBalanceErrIndex == 0) {
                ShowWarningMessage(state, format("Zone Air Heat Balance is out of balance for zone named {}", thisZone.Name));
                ShowContinueError(state, format("Zone Air Heat Balance Deviation Rate is more than {:.1R} {{W}}", Threshold));
                if (state.dataHVACGlobal->TurnFansOn) {
                    ShowContinueError(state, "Night cycle fan operation may be causing above error");
                }

                ShowContinueErrorTimeStamp(state, " Occurrence info:");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           format("Zone Air Heat Balance is out of balance ... zone named {}", thisZone.Name),
                                           thisZone.AirHBimBalanceErrIndex,
                                           std::abs(thisAirRpt.imBalance) - Threshold,
                                           std::abs(thisAirRpt.imBalance) - Threshold,
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

    // PURPOSE OF THIS FUNCTION:
    // This function verifies that a zone (by name) has a Zone Control:Thermostatic object entered.

    if (state.dataZoneCtrls->GetZoneAirStatsInputFlag) {
        GetZoneAirSetPoints(state);
        state.dataZoneCtrls->GetZoneAirStatsInputFlag = false;
    }
    if (state.dataZoneCtrls->NumTempControlledZones > 0) {
        if (Util::FindItemInList(ZoneName, state.dataZoneCtrls->TempControlledZone, &DataZoneControls::ZoneTempControls::ZoneName) > 0) {
            return true;
        } else {
            return false;
        }
    }
    return false;
}

bool VerifyControlledZoneForThermostat(EnergyPlusData &state, std::string const &ZoneName) // Zone to verify
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   Mar 2007

    // PURPOSE OF THIS FUNCTION:
    // This function verifies that a zone (by name) has a ZoneHVAC:EquipmentConnections object entered.

    return (Util::FindItemInList(ZoneName, state.dataZoneEquip->ZoneEquipConfig, &DataZoneEquipment::EquipConfiguration::ZoneName) > 0);
}

void DetectOscillatingZoneTemp(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   August 2005

    // PURPOSE OF THIS SUBROUTINE:
    // Oscillating temperatures between HVAC timesteps indicate that the
    // simulation may be poor. Code is trying to be fast since the purpose
    // is to see the impact on oscillating by trying longer time steps in
    // an attempt to speed up the simulation.
    // Note that the OscillateMagnitude threshold must be less than
    // MaxZoneTempDiff since ManageHVAC keeps shortening the timestep
    // until that is reached unless it goes to less than the
    // MinTimeStepSys.

    // first time run allocate arrays and setup output variable
    if (state.dataZoneTempPredictorCorrector->SetupOscillationOutputFlag) {
        state.dataZoneTempPredictorCorrector->ZoneTempHist.allocate(4, state.dataGlobal->NumOfZones);
        state.dataZoneTempPredictorCorrector->ZoneTempHist = 0.0;
        state.dataZoneTempPredictorCorrector->ZoneTempOscillate.dimension(state.dataGlobal->NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy.dimension(state.dataGlobal->NumOfZones, 0.0);
        state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband.dimension(state.dataGlobal->NumOfZones, 0.0);
        // set up zone by zone variables, CurrentModuleObject='Zone'
        for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
            auto &zone = state.dataHeatBal->Zone(iZone);
            SetupOutputVariable(state,
                                "Zone Oscillating Temperatures Time",
                                Constant::Units::hr,
                                state.dataZoneTempPredictorCorrector->ZoneTempOscillate(iZone),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zone.Name);
            SetupOutputVariable(state,
                                "Zone Oscillating Temperatures During Occupancy Time",
                                Constant::Units::hr,
                                state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy(iZone),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zone.Name);
            SetupOutputVariable(state,
                                "Zone Oscillating Temperatures in Deadband Time",
                                Constant::Units::hr,
                                state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband(iZone),
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                zone.Name);
        }
        // set up a variable covering all zones
        SetupOutputVariable(state,
                            "Facility Any Zone Oscillating Temperatures Time",
                            Constant::Units::hr,
                            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Sum,
                            "Facility");
        SetupOutputVariable(state,
                            "Facility Any Zone Oscillating Temperatures During Occupancy Time",
                            Constant::Units::hr,
                            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Sum,
                            "Facility");
        SetupOutputVariable(state,
                            "Facility Any Zone Oscillating Temperatures in Deadband Time",
                            Constant::Units::hr,
                            state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband,
                            OutputProcessor::TimeStepType::System,
                            OutputProcessor::StoreType::Sum,
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

    Real64 TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    if (state.dataZoneTempPredictorCorrector->OscillationVariablesNeeded) {
        // precalc the negative value for performance
        Real64 NegOscillateMagnitude = -HVAC::OscillateMagnitude;
        // assume no zone is oscillating
        bool isAnyZoneOscillating = false;
        bool isAnyZoneOscillatingDuringOccupancy = false;
        bool isAnyZoneOscillatingInDeadband = false;

        for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
            bool isOscillate = false;
            state.dataZoneTempPredictorCorrector->ZoneTempHist(4, iZone) = state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone);
            state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone) = state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone);
            state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone) = state.dataZoneTempPredictorCorrector->ZoneTempHist(1, iZone);
            state.dataZoneTempPredictorCorrector->ZoneTempHist(1, iZone) = state.dataZoneTempPredictorCorrector->zoneHeatBalance(iZone).ZT;
            Real64 Diff34 =
                state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone) - state.dataZoneTempPredictorCorrector->ZoneTempHist(4, iZone);
            Real64 Diff23 =
                state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone) - state.dataZoneTempPredictorCorrector->ZoneTempHist(3, iZone);
            Real64 Diff12 =
                state.dataZoneTempPredictorCorrector->ZoneTempHist(1, iZone) - state.dataZoneTempPredictorCorrector->ZoneTempHist(2, iZone);
            // roll out the conditionals for increased performance
            if (Diff12 > HVAC::OscillateMagnitude) {
                if (Diff23 < NegOscillateMagnitude) {
                    if (Diff34 > HVAC::OscillateMagnitude) {
                        isOscillate = true;
                    }
                }
            }
            // now try the opposite sequence of swings
            if (Diff12 < NegOscillateMagnitude) {
                if (Diff23 > HVAC::OscillateMagnitude) {
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
        state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate = (isAnyZoneOscillating) ? TimeStepSys : 0.0;
        state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy = (isAnyZoneOscillatingDuringOccupancy) ? TimeStepSys : 0.0;
        state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband = (isAnyZoneOscillatingInDeadband) ? TimeStepSys : 0.0;

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

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine modifies the air temperature setpoint to effect operative temperature control

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 thisMRTFraction; // local variable for fraction that MRT is in Op Temp definition

    if (!(state.dataZoneCtrls->AnyOpTempControl)) return; // do nothing to setpoint

    auto &tempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID);
    if (!(tempControlledZone.OperativeTempControl)) return; // do nothing to setpoint

    // is operative temp radiative fraction scheduled or fixed?
    thisMRTFraction = (tempControlledZone.OpTempCntrlModeScheduled)
                          ? ScheduleManager::GetCurrentScheduleValue(state, tempControlledZone.OpTempRadiativeFractionSched)
                          : tempControlledZone.FixedRadiativeFraction;

    // get mean radiant temperature for zone
    Real64 thisMRT = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ActualZoneNum).MRT;

    // modify setpoint for operative temperature control
    //  trapping for MRT fractions between 0.0 and 0.9 during get input, so shouldn't be able to divide by zero here.
    ZoneAirSetPoint = (ZoneAirSetPoint - thisMRTFraction * thisMRT) / (1.0 - thisMRTFraction);
}

void AdjustOperativeSetPointsforAdapComfort(EnergyPlusData &state, int const TempControlledZoneID, Real64 &ZoneAirSetPoint)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Xuan Luo
    //       DATE WRITTEN   Jan 2017

    // PURPOSE OF THIS SUBROUTINE:
    // This routine adjust the operative setpoints for each controlled adaptive thermal comfort models.

    auto &tempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID);
    auto const &AdapComfortDailySetPointSchedule = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int originZoneAirSetPoint = ZoneAirSetPoint;
    int AdaptiveComfortModelTypeIndex = tempControlledZone.AdaptiveComfortModelTypeIndex;

    // adjust zone operative setpoint
    if (!(tempControlledZone.AdaptiveComfortTempControl)) return; // do nothing to setpoint
    if ((state.dataWeather->Environment(state.dataWeather->Envrn).KindOfEnvrn != Constant::KindOfSim::DesignDay) &&
        (state.dataWeather->Environment(state.dataWeather->Envrn).KindOfEnvrn != Constant::KindOfSim::HVACSizeDesignDay)) {
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
        default:
            break;
        }
    } else {
        int const envrnDayNum(state.dataWeather->Environment(state.dataWeather->Envrn).DesignDayNum);
        int constexpr summerDesignDayTypeIndex(9);
        // Adjust summer design day set point
        if (state.dataWeather->DesDayInput(envrnDayNum).DayType == summerDesignDayTypeIndex) {
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

    // PURPOSE OF THIS SUBROUTINE:
    // This routine sets the thermal comfort setpoints for each controlled zone based on air temperature obtained from thermal comfort models.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SetPointLo = 0.0;
    Real64 SetPointHi = 0.0;
    Real64 Tset = 0.0;
    int ObjectCount = 0;
    Real64 PeopleCount = 0.0;
    int SetPointComfortSchedIndex = 0;
    int SchedTypeIndex = 0;
    int PeopleNum;

    // Call thermal comfort module to read zone control comfort object
    if (state.dataZoneTempPredictorCorrector->CalcZoneAirComfortSetPointsFirstTimeFlag) {
        ThermalComfort::ManageThermalComfort(state, true);
        state.dataZoneTempPredictorCorrector->CalcZoneAirComfortSetPointsFirstTimeFlag = false;
    }

    state.dataHeatBalFanSys->ComfortControlType = HVAC::ThermostatType::Uncontrolled; // Default

    for (int RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++RelativeZoneNum) {

        auto &comfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum);
        int ActualZoneNum = comfortControlledZone.ActualZoneNum;
        auto &zone = state.dataHeatBal->Zone(ActualZoneNum);
        auto &comfortControlType = state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum);
        auto &comfortControlTypeRpt = state.dataHeatBalFanSys->ComfortControlTypeRpt(ActualZoneNum);
        auto &tempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
        auto &zoneComfortControlsFanger = state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum);
        comfortControlType =
            static_cast<HVAC::ThermostatType>(ScheduleManager::GetCurrentScheduleValue(state, comfortControlledZone.ComfortSchedIndex));
        comfortControlTypeRpt = static_cast<int>(comfortControlType);

        // Get PMV values
        switch (comfortControlType) {
        case HVAC::ThermostatType::Uncontrolled:
            zoneComfortControlsFanger.LowPMV = -999.0;
            zoneComfortControlsFanger.HighPMV = -999.0;
            break;
        case HVAC::ThermostatType::SingleHeating:
            zoneComfortControlsFanger.FangerType = static_cast<int>(HVAC::ThermostatType::SingleHeating);
            zoneComfortControlsFanger.LowPMV = ScheduleManager::GetCurrentScheduleValue(
                state,
                state.dataZoneTempPredictorCorrector
                    ->SetPointSingleHeatingFanger(comfortControlledZone.ControlTypeSchIndx(comfortControlledZone.SchIndx_SingleHeating))
                    .PMVSchedIndex);
            zoneComfortControlsFanger.HighPMV = -999.0;
            break;
        case HVAC::ThermostatType::SingleCooling:
            zoneComfortControlsFanger.FangerType = static_cast<int>(HVAC::ThermostatType::SingleCooling);
            zoneComfortControlsFanger.LowPMV = -999.0;
            zoneComfortControlsFanger.HighPMV = ScheduleManager::GetCurrentScheduleValue(
                state,
                state.dataZoneTempPredictorCorrector
                    ->SetPointSingleCoolingFanger(comfortControlledZone.ControlTypeSchIndx(comfortControlledZone.SchIndx_SingleCooling))
                    .PMVSchedIndex);
            break;
        case HVAC::ThermostatType::SingleHeatCool:
            SetPointComfortSchedIndex =
                state.dataZoneTempPredictorCorrector
                    ->SetPointSingleHeatCoolFanger(comfortControlledZone.ControlTypeSchIndx(comfortControlledZone.SchIndx_SingleHeatCool))
                    .PMVSchedIndex;
            zoneComfortControlsFanger.FangerType = static_cast<int>(HVAC::ThermostatType::SingleHeatCool);
            zoneComfortControlsFanger.LowPMV = ScheduleManager::GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
            zoneComfortControlsFanger.HighPMV = ScheduleManager::GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
            break;
        case HVAC::ThermostatType::DualSetPointWithDeadBand:
            SchedTypeIndex = comfortControlledZone.ControlTypeSchIndx(comfortControlledZone.SchIndx_DualSetPointWithDeadBand);
            zoneComfortControlsFanger.FangerType = static_cast<int>(HVAC::ThermostatType::DualSetPointWithDeadBand);
            zoneComfortControlsFanger.LowPMV = ScheduleManager::GetCurrentScheduleValue(
                state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).HeatPMVSchedIndex);
            zoneComfortControlsFanger.HighPMV = ScheduleManager::GetCurrentScheduleValue(
                state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).CoolPMVSchedIndex);
            if (zoneComfortControlsFanger.LowPMV > zoneComfortControlsFanger.HighPMV) {
                ++zoneComfortControlsFanger.DualPMVErrCount;
                if (zoneComfortControlsFanger.DualPMVErrCount < 2) {
                    ShowWarningError(state,
                                     format("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint: The heating PMV setpoint is above the "
                                            "cooling PMV setpoint in {}",
                                            state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).Name));
                    ShowContinueError(state, "The zone dual heating PMV setpoint is set to the dual cooling PMV setpoint.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The heating PMV setpoint is still above the cooling PMV setpoint",
                                                   zoneComfortControlsFanger.DualPMVErrIndex,
                                                   zoneComfortControlsFanger.LowPMV,
                                                   zoneComfortControlsFanger.LowPMV);
                }
                zoneComfortControlsFanger.LowPMV = zoneComfortControlsFanger.HighPMV;
            }
            break;
        default:
            ShowSevereError(state,
                            format("CalcZoneAirTempSetpoints: Illegal thermal control control type for Zone={}, Found value={}, in Schedule={}",
                                   zone.Name,
                                   comfortControlTypeRpt,
                                   comfortControlledZone.ControlTypeSchedName));
            break;
        }

        // Check Average method
        switch (comfortControlledZone.AverageMethod) {
        case DataZoneControls::AverageMethod::NO:
            PeopleNum = comfortControlledZone.SpecificObjectNum;
            if (comfortControlType == HVAC::ThermostatType::SingleCooling) {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, SetPointLo);
            } else {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, zoneComfortControlsFanger.LowPMV, SetPointLo);
            }
            if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand)
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, SetPointHi);
            break;
        case DataZoneControls::AverageMethod::SPE:
            PeopleNum = comfortControlledZone.SpecificObjectNum;
            if (comfortControlType == HVAC::ThermostatType::SingleCooling) {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, SetPointLo);
            } else {
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, zoneComfortControlsFanger.LowPMV, SetPointLo);
            }
            if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand)
                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, SetPointHi);
            break;
        case DataZoneControls::AverageMethod::OBJ:
            SetPointLo = 0.0;
            SetPointHi = 0.0;
            for (int peopleNum = 1; peopleNum <= state.dataHeatBal->TotPeople; ++peopleNum) {
                if (ActualZoneNum == state.dataHeatBal->People(peopleNum).ZonePtr) {
                    ++ObjectCount;
                    GetComfortSetPoints(state, peopleNum, RelativeZoneNum, zoneComfortControlsFanger.LowPMV, Tset);
                    SetPointLo += Tset;
                    if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand) {
                        GetComfortSetPoints(state, peopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, Tset);
                        SetPointHi += Tset;
                    }
                }
            }
            SetPointLo /= ObjectCount;
            if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand) SetPointHi /= ObjectCount;
            break;
        case DataZoneControls::AverageMethod::PEO:
            SetPointLo = 0.0;
            SetPointHi = 0.0;
            for (int peopleNum = 1; peopleNum <= state.dataHeatBal->TotPeople; ++peopleNum) {
                if (ActualZoneNum == state.dataHeatBal->People(peopleNum).ZonePtr) {
                    int NumberOccupants = state.dataHeatBal->People(peopleNum).NumberOfPeople *
                                          ScheduleManager::GetCurrentScheduleValue(state, state.dataHeatBal->People(peopleNum).NumberOfPeoplePtr);
                    PeopleCount += NumberOccupants;
                    GetComfortSetPoints(state, peopleNum, RelativeZoneNum, zoneComfortControlsFanger.LowPMV, Tset);
                    SetPointLo += Tset * NumberOccupants;
                    if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand) {
                        GetComfortSetPoints(state, peopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, Tset);
                        SetPointHi += Tset * NumberOccupants;
                    }
                }
            }
            if (PeopleCount > 0) {
                SetPointLo /= PeopleCount;
                if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand) SetPointHi /= PeopleCount;
            } else {
                if (comfortControlledZone.PeopleAverageErrIndex == 0) {
                    ShowWarningMessage(state,
                                       format("ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = {} is zero. The People "
                                              "Average option is not used.",
                                              zone.Name));
                    ShowContinueError(state, "The Object Average option is used instead. Simulation continues .....");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " + zone.Name +
                                                   " is still zero. The People Average option is not used",
                                               comfortControlledZone.PeopleAverageErrIndex,
                                               PeopleCount,
                                               PeopleCount);
                SetPointLo = 0.0;
                SetPointHi = 0.0;
                for (int peopleNum = 1; peopleNum <= state.dataHeatBal->TotPeople; ++peopleNum) {
                    if (ActualZoneNum == state.dataHeatBal->People(peopleNum).ZonePtr) {
                        ++ObjectCount;
                        GetComfortSetPoints(state, peopleNum, RelativeZoneNum, zoneComfortControlsFanger.LowPMV, Tset);
                        SetPointLo += Tset;
                        if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand) {
                            GetComfortSetPoints(state, peopleNum, RelativeZoneNum, zoneComfortControlsFanger.HighPMV, Tset);
                            SetPointHi += Tset;
                        }
                    }
                }
                SetPointLo /= ObjectCount;
                if (comfortControlType == HVAC::ThermostatType::DualSetPointWithDeadBand) SetPointHi /= ObjectCount;
            }
            break;
        default:
            break;
        }

        // Assign setpoint
        switch (comfortControlType) {
        case HVAC::ThermostatType::Uncontrolled:
            switch (state.dataHeatBalFanSys->TempControlType(ActualZoneNum)) {
            case HVAC::ThermostatType::SingleHeating:
                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = 0.0;
                break;
            case HVAC::ThermostatType::SingleCooling:
                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = 0.0;
                break;
            default:
                break;
            }
            break;
        case HVAC::ThermostatType::SingleHeating:
            if (SetPointLo < comfortControlledZone.TdbMinSetPoint) {
                SetPointLo = comfortControlledZone.TdbMinSetPoint;
                if (comfortControlledZone.TdbMinErrIndex < 2) {
                    ShowWarningMessage(state,
                                       format("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is below the Minimum dry-bulb "
                                              "temperature setpoint {}",
                                              comfortControlledZone.Name));
                    ShowContinueError(state, "The zone heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is still below the "
                                               "Minimum dry-bulb temperature setpoint ...",
                                               comfortControlledZone.TdbMinErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            tempZoneThermostatSetPoint = SetPointLo;
            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = tempZoneThermostatSetPoint;
            state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = HVAC::ThermostatType::SingleHeating;
            state.dataHeatBalFanSys->TempControlTypeRpt(ActualZoneNum) = static_cast<int>(state.dataHeatBalFanSys->TempControlType(ActualZoneNum));
            break;
        case HVAC::ThermostatType::SingleCooling:
            if (SetPointLo > comfortControlledZone.TdbMaxSetPoint) {
                SetPointLo = comfortControlledZone.TdbMaxSetPoint;
                if (comfortControlledZone.TdbMaxErrIndex == 0) {
                    ShowWarningMessage(state,
                                       format("ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is above the Maximum dry-bulb "
                                              "temperature setpoint {}",
                                              comfortControlledZone.Name));
                    ShowContinueError(state, "The zone cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is still above the "
                                               "Maximum dry-bulb temperature setpoint ...",
                                               comfortControlledZone.TdbMaxErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            tempZoneThermostatSetPoint = SetPointLo;
            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = tempZoneThermostatSetPoint;
            state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = HVAC::ThermostatType::SingleCooling;
            state.dataHeatBalFanSys->TempControlTypeRpt(ActualZoneNum) = static_cast<int>(state.dataHeatBalFanSys->TempControlType(ActualZoneNum));
            break;
        case HVAC::ThermostatType::SingleHeatCool:
            if (comfortControlledZone.TdbMaxSetPoint == comfortControlledZone.TdbMinSetPoint) {
                SetPointLo = comfortControlledZone.TdbMaxSetPoint;
            }
            if (SetPointLo > comfortControlledZone.TdbMaxSetPoint) SetPointLo = comfortControlledZone.TdbMaxSetPoint;
            if (SetPointLo < comfortControlledZone.TdbMinSetPoint) SetPointLo = comfortControlledZone.TdbMinSetPoint;
            if (SetPointLo < comfortControlledZone.TdbMinSetPoint || SetPointLo > comfortControlledZone.TdbMaxSetPoint) {
                if (comfortControlledZone.TdbHCErrIndex == 0) {
                    ShowWarningMessage(state,
                                       format("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is above the Maximum or "
                                              "below the Minimum dry-bulb temperature setpoint {}",
                                              comfortControlledZone.Name));
                    ShowContinueError(state,
                                      "The zone setpoint is set to the Maximum dry-bulb temperature setpoint if above or the Minimum "
                                      "dry-bulb temperature setpoint if below");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is still beyond "
                                               "the range between Maximum and Minimum dry-bulb temperature setpoint ...",
                                               comfortControlledZone.TdbHCErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            tempZoneThermostatSetPoint = SetPointLo;
            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = tempZoneThermostatSetPoint;
            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = tempZoneThermostatSetPoint;
            state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = HVAC::ThermostatType::SingleHeatCool;
            state.dataHeatBalFanSys->TempControlTypeRpt(ActualZoneNum) = static_cast<int>(state.dataHeatBalFanSys->TempControlType(ActualZoneNum));
            break;
        case HVAC::ThermostatType::DualSetPointWithDeadBand:
            if (SetPointLo < comfortControlledZone.TdbMinSetPoint) {
                SetPointLo = comfortControlledZone.TdbMinSetPoint;

                if (comfortControlledZone.TdbDualMinErrIndex == 0) {
                    ShowWarningMessage(state,
                                       format("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is below the Minimum dry-bulb "
                                              "temperature setpoint {}",
                                              comfortControlledZone.Name));
                    ShowContinueError(state, "The zone dual heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still below the Minimum "
                                               "dry-bulb temperature setpoint ...",
                                               comfortControlledZone.TdbDualMinErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }
            if (SetPointHi > comfortControlledZone.TdbMaxSetPoint) {
                SetPointHi = comfortControlledZone.TdbMaxSetPoint;
                if (comfortControlledZone.TdbDualMaxErrIndex == 0) {
                    ShowWarningMessage(state,
                                       format("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is above the Maximum dry-bulb "
                                              "temperature setpoint in zone = {}",
                                              comfortControlledZone.Name));
                    ShowContinueError(state, "The zone dual cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still above the Maximum "
                                               "dry-bulb temperature setpoint ...",
                                               comfortControlledZone.TdbDualMaxErrIndex,
                                               SetPointLo,
                                               SetPointLo);
            }

            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = SetPointLo;
            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = SetPointHi;
            state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = HVAC::ThermostatType::DualSetPointWithDeadBand;
            state.dataHeatBalFanSys->TempControlTypeRpt(ActualZoneNum) = static_cast<int>(state.dataHeatBalFanSys->TempControlType(ActualZoneNum));
            break;
        default:
            ShowSevereError(state,
                            format("CalcZoneAirComfortSetpoints: Illegal thermal control control type for Zone={}, Found value={}, in Schedule={}",
                                   zone.Name,
                                   comfortControlTypeRpt,
                                   comfortControlledZone.ControlTypeSchedName));
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
    // PURPOSE OF THIS SUBROUTINE:
    // This routine sets what the thermal comfort setpoints for each controlled zone should be based on air temperature
    // obtained from thermal comfort models. This is called each time step.

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // 0 = Solution; 1 = Set to Min; 2 Set to Max

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr Acc(0.001); // accuracy control for SolveRoot
    int constexpr MaxIter(500);  // iteration control for SolveRoot

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PMVResult = 0.0; // Calculated PMV value

    auto &comfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum);
    Real64 Tmin = comfortControlledZone.TdbMinSetPoint;
    Real64 Tmax = comfortControlledZone.TdbMaxSetPoint;

    ThermalComfort::CalcThermalComfortFanger(state, PeopleNum, Tmin, PMVResult);
    Real64 PMVMin = PMVResult;
    ThermalComfort::CalcThermalComfortFanger(state, PeopleNum, Tmax, PMVResult);
    Real64 PMVMax = PMVResult;
    if (PMVSet > PMVMin && PMVSet < PMVMax) {

        auto f = [&state, PMVSet, PeopleNum](Real64 Tset) {
            Real64 PMVresult = 0.0; // resulting PMV values
            ThermalComfort::CalcThermalComfortFanger(state, PeopleNum, Tset, PMVresult);
            return (PMVSet - PMVresult);
        };

        int SolFla = 0; // feed back flag from SolveRoot
        General::SolveRoot(state, Acc, MaxIter, SolFla, Tset, f, Tmin, Tmax);
        if (SolFla == -1) {
            if (!state.dataGlobal->WarmupFlag) {
                ++state.dataZoneTempPredictorCorrector->IterLimitExceededNum1;
                if (state.dataZoneTempPredictorCorrector->IterLimitExceededNum1 == 1) {
                    ShowWarningError(
                        state,
                        format("{}: Iteration limit exceeded calculating thermal comfort Fanger setpoint and non-converged setpoint is used",
                               comfortControlledZone.Name));
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   comfortControlledZone.Name + ":  Iteration limit exceeded calculating thermal comfort setpoint.",
                                                   state.dataZoneTempPredictorCorrector->IterLimitErrIndex1,
                                                   Tset,
                                                   Tset);
                }
            }
        } else if (SolFla == -2) {
            if (!state.dataGlobal->WarmupFlag) {
                ++state.dataZoneTempPredictorCorrector->IterLimitExceededNum2;
                if (state.dataZoneTempPredictorCorrector->IterLimitExceededNum2 == 1) {
                    ShowWarningError(
                        state,
                        format("{}: Solution is not found in calculating thermal comfort Fanger setpoint and the minimum setpoint is used",
                               comfortControlledZone.Name));
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        format("{}:  Solution is not found in  calculating thermal comfort Fanger setpoint.", comfortControlledZone.Name),
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

void AdjustCoolingSetPointforTempAndHumidityControl(EnergyPlusData &state,
                                                    int const TempControlledZoneID,
                                                    int const ActualZoneNum // controlled zone actual zone number
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket A Nigusse, FSEC/UCF
    //       DATE WRITTEN   Nov 2010

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine modifies the air cooling setpoint temperature to effect zone air Temperature and humidity control
    //  Alter the zone air cooling setpoint if the zone air relative humidity value exceeds the the zone dehumidifying relative humidity setpoint.

    Real64 ZoneOvercoolRange = 0.0;
    auto &tempControlledZone = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID);

    if (!(state.dataZoneCtrls->AnyZoneTempAndHumidityControl)) return; // do nothing to setpoint
    if (!(tempControlledZone.ZoneOvercoolControl)) return;             // do nothing to setpoint

    if (tempControlledZone.OvercoolCntrlModeScheduled) {
        ZoneOvercoolRange = ScheduleManager::GetCurrentScheduleValue(state, tempControlledZone.ZoneOvercoolRangeSchedIndex);
    } else {
        ZoneOvercoolRange = tempControlledZone.ZoneOvercoolConstRange;
    }
    Real64 ZoneOvercoolControlRatio = tempControlledZone.ZoneOvercoolControlRatio;

    // For Dual Setpoint thermostat the overcool range is limited by the temperature difference between cooling and heating setpoints
    Real64 MaxAllowedOvercoolRange =
        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) - state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
    if (MaxAllowedOvercoolRange > 0.0) {
        ZoneOvercoolRange = min(ZoneOvercoolRange, MaxAllowedOvercoolRange);
    }
    // Calculate difference between zone air relative humidity and the dehumidifying setpoint
    Real64 RelativeHumidityDiff = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ActualZoneNum).airRelHum -
                                  ScheduleManager::GetCurrentScheduleValue(state, tempControlledZone.DehumidifyingSchedIndex);
    if (RelativeHumidityDiff > 0.0 && ZoneOvercoolControlRatio > 0.0) {
        // proportionally reset the cooling setpoint temperature downward (zone Overcool)
        ZoneOvercoolRange = min(ZoneOvercoolRange, RelativeHumidityDiff / ZoneOvercoolControlRatio);
        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) -= ZoneOvercoolRange;
    }
}

void OverrideAirSetPointsforEMSCntrl(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         L. Gu
    //       DATE WRITTEN   June 2017

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine overrides the air temperature setpoint based on EMS

    auto &ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo;
    auto &ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi;

    for (int Loop = 1; Loop <= state.dataZoneCtrls->NumTempControlledZones; ++Loop) {
        auto const &tempControlledZone = state.dataZoneCtrls->TempControlledZone(Loop);
        if (tempControlledZone.EMSOverrideHeatingSetPointOn) {
            int ZoneNum = tempControlledZone.ActualZoneNum;

            switch (state.dataHeatBalFanSys->TempControlType(ZoneNum)) {
            case HVAC::ThermostatType::SingleHeating:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = tempControlledZone.EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = tempControlledZone.EMSOverrideHeatingSetPointValue;
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = tempControlledZone.EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = tempControlledZone.EMSOverrideHeatingSetPointValue;
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointLo(ZoneNum) = tempControlledZone.EMSOverrideHeatingSetPointValue;
                break;
            default:
                break;
            }
        }
        if (tempControlledZone.EMSOverrideCoolingSetPointOn) {
            int ZoneNum = tempControlledZone.ActualZoneNum;

            switch (state.dataHeatBalFanSys->TempControlType(ZoneNum)) {
            case HVAC::ThermostatType::SingleCooling:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = tempControlledZone.EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = tempControlledZone.EMSOverrideCoolingSetPointValue;
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = tempControlledZone.EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = tempControlledZone.EMSOverrideCoolingSetPointValue;
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointHi(ZoneNum) = tempControlledZone.EMSOverrideCoolingSetPointValue;
                break;
            default:
                break;
            }
        }
    }

    for (int Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
        auto &comfortControlledZone = state.dataZoneCtrls->ComfortControlledZone(Loop);
        if (comfortControlledZone.EMSOverrideHeatingSetPointOn) {
            int ZoneNum = comfortControlledZone.ActualZoneNum;
            switch (state.dataHeatBalFanSys->ComfortControlType(ZoneNum)) {
            case HVAC::ThermostatType::SingleHeating:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = comfortControlledZone.EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = comfortControlledZone.EMSOverrideHeatingSetPointValue;
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = comfortControlledZone.EMSOverrideHeatingSetPointValue;
                ZoneThermostatSetPointLo(ZoneNum) = comfortControlledZone.EMSOverrideHeatingSetPointValue;
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointLo(ZoneNum) = comfortControlledZone.EMSOverrideHeatingSetPointValue;
                break;
            default:
                break;
            }
        }

        if (comfortControlledZone.EMSOverrideCoolingSetPointOn) {
            int ZoneNum = comfortControlledZone.ActualZoneNum;
            switch (static_cast<HVAC::ThermostatType>(state.dataHeatBalFanSys->ComfortControlType(ZoneNum))) {
            case HVAC::ThermostatType::SingleCooling:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = comfortControlledZone.EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = comfortControlledZone.EMSOverrideCoolingSetPointValue;
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = comfortControlledZone.EMSOverrideCoolingSetPointValue;
                ZoneThermostatSetPointHi(ZoneNum) = comfortControlledZone.EMSOverrideCoolingSetPointValue;
                break;
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                ZoneThermostatSetPointHi(ZoneNum) = comfortControlledZone.EMSOverrideCoolingSetPointValue;
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

    for (int idx = 1; idx <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls; ++idx) {
        auto &singleHtgSetpoint = state.dataZoneTempPredictorCorrector->SetPointSingleHeating(idx);
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
        auto &singleClgSetpoint = state.dataZoneTempPredictorCorrector->SetPointSingleCooling(idx);
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
        auto &dualHeatCoolSetpoint = state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(idx);
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

    // determine month to use based on hemisphere and season
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
    int hourSelect = hourOfDay + state.dataWeather->DSTIndex(jdateSelect);

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

void FillPredefinedTableOnThermostatSchedules(EnergyPlusData &state)
{
    // add values to the System Summary tabular report related to schedules used by the thermostat objects
    // J.Glazer - March 2024
    using OutputReportPredefined::PreDefTableEntry;
    auto &orp = state.dataOutRptPredefined;
    for (int idx = 1; idx <= state.dataZoneCtrls->NumTempControlledZones; ++idx) {
        auto &tcz = state.dataZoneCtrls->TempControlledZone(idx);
        PreDefTableEntry(state, orp->pdchStatName, tcz.ZoneName, tcz.Name);
        PreDefTableEntry(state, orp->pdchStatCtrlTypeSchd, tcz.ZoneName, tcz.ControlTypeSchedName);
        for (int ctInx = 1; ctInx <= tcz.NumControlTypes; ++ctInx) {
            PreDefTableEntry(state, orp->pdchStatSchdType1, tcz.ZoneName, HVAC::thermostatTypeNames[(int)tcz.ControlTypeEnum(ctInx)]);
            PreDefTableEntry(state, orp->pdchStatSchdTypeName1, tcz.ZoneName, tcz.ControlTypeName(1));
            switch (tcz.ControlTypeEnum(ctInx)) {
            case HVAC::ThermostatType::DualSetPointWithDeadBand:
                PreDefTableEntry(
                    state, orp->pdchStatSchdHeatName, tcz.ZoneName, ScheduleManager::GetScheduleName(state, tcz.SchIndx_DualSetPointWDeadBandHeat));
                PreDefTableEntry(
                    state, orp->pdchStatSchdCoolName, tcz.ZoneName, ScheduleManager::GetScheduleName(state, tcz.SchIndx_DualSetPointWDeadBandCool));
                break;
            case HVAC::ThermostatType::SingleHeatCool:
                PreDefTableEntry(
                    state, orp->pdchStatSchdHeatName, tcz.ZoneName, ScheduleManager::GetScheduleName(state, tcz.SchIndx_SingleHeatCoolSetPoint));
                PreDefTableEntry(
                    state, orp->pdchStatSchdCoolName, tcz.ZoneName, ScheduleManager::GetScheduleName(state, tcz.SchIndx_SingleHeatCoolSetPoint));
                break;
            case HVAC::ThermostatType::SingleCooling:
                PreDefTableEntry(
                    state, orp->pdchStatSchdHeatName, tcz.ZoneName, ScheduleManager::GetScheduleName(state, tcz.SchIndx_SingleCoolSetPoint));
                break;
            case HVAC::ThermostatType::SingleHeating:
                PreDefTableEntry(
                    state, orp->pdchStatSchdCoolName, tcz.ZoneName, ScheduleManager::GetScheduleName(state, tcz.SchIndx_SingleHeatSetPoint));
                break;
            }
        }
    }
}

void ZoneSpaceHeatBalanceData::updateTemperatures(EnergyPlusData &state,
                                                  bool const ShortenTimeStepSys,
                                                  bool const UseZoneTimeStepHistory,
                                                  Real64 const PriorTimeStep,
                                                  int const zoneNum,
                                                  int const spaceNum)
{
    assert(zoneNum > 0);
    if (ShortenTimeStepSys) {
        // timestep has just shifted from full zone timestep to a new shorter system timestep
        // throw away last updates in corrector and rewind for resimulating smaller timestep
        if (spaceNum == 0) {
            if (state.dataHeatBal->Zone(zoneNum).SystemZoneNodeNumber > 0) { // roll back result for zone air node,
                auto &zoneNode = state.dataLoopNodes->Node(state.dataHeatBal->Zone(zoneNum).SystemZoneNodeNumber);
                zoneNode.Temp = this->XMAT[0];
                state.dataHeatBalFanSys->TempTstatAir(zoneNum) = this->XMAT[0];
                zoneNode.HumRat = this->WPrevZoneTS[0];
                zoneNode.Enthalpy = Psychrometrics::PsyHFnTdbW(this->XMAT[0], this->WPrevZoneTS[0]);
            }
        } else {
            if (state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber > 0) { // roll back result for space air node,
                auto &spaceNode = state.dataLoopNodes->Node(state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber);
                spaceNode.Temp = this->XMAT[0];
                state.dataHeatBalFanSys->TempTstatAir(zoneNum) = this->XMAT[0];
                spaceNode.HumRat = this->WPrevZoneTS[0];
                spaceNode.Enthalpy = Psychrometrics::PsyHFnTdbW(this->XMAT[0], this->WPrevZoneTS[0]);
            }
        }

        if (state.dataHVACGlobal->NumOfSysTimeSteps !=
            state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time
            Real64 TimeStepSys = state.dataHVACGlobal->TimeStepSys;
            this->MAT = DownInterpolate4HistoryValues(PriorTimeStep, TimeStepSys, this->XMAT, this->DSXMAT);
            this->airHumRat = DownInterpolate4HistoryValues(PriorTimeStep, TimeStepSys, this->WPrevZoneTS, this->DSWPrevZoneTS);

            if (spaceNum == 0 && state.dataRoomAir->anyNonMixingRoomAirModel) {
                if (state.dataRoomAir->IsZoneDispVent3Node(zoneNum) || state.dataRoomAir->IsZoneUFAD(zoneNum)) {

                    state.dataRoomAir->MATFloor(zoneNum) = DownInterpolate4HistoryValues(
                        PriorTimeStep, TimeStepSys, state.dataRoomAir->XMATFloor(zoneNum), state.dataRoomAir->DSXMATFloor(zoneNum));
                    state.dataRoomAir->MATOC(zoneNum) = DownInterpolate4HistoryValues(
                        PriorTimeStep, TimeStepSys, state.dataRoomAir->XMATOC(zoneNum), state.dataRoomAir->DSXMATOC(zoneNum));
                    state.dataRoomAir->MATMX(zoneNum) = DownInterpolate4HistoryValues(
                        PriorTimeStep, TimeStepSys, state.dataRoomAir->XMATMX(zoneNum), state.dataRoomAir->DSXMATMX(zoneNum));
                }
                if (state.dataRoomAir->AirModel(zoneNum).AirModel == RoomAir::RoomAirModel::AirflowNetwork) {
                    for (auto &afnNode : state.dataRoomAir->AFNZoneInfo(zoneNum).Node) {
                        afnNode.AirTemp = DownInterpolate4HistoryValues(PriorTimeStep, TimeStepSys, afnNode.AirTempX, afnNode.AirTempDSX);

                        afnNode.HumRat = DownInterpolate4HistoryValues(PriorTimeStep, TimeStepSys, afnNode.HumRatX, afnNode.HumRatDSX);
                    }
                }
            }
        } else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
                 // do nothing because DS history would have been pushed prior and should be ready
        }
    }
    // now update the variables actually used in the balance equations.
    if (UseZoneTimeStepHistory) {
        this->ZTM = this->XMAT;
        this->WPrevZoneTSTemp = this->WPrevZoneTS;
    } else { // use down-stepped history
        this->ZTM = this->DSXMAT;
        this->WPrevZoneTSTemp = this->DSWPrevZoneTS;
    }
}

void ZoneSpaceHeatBalanceData::calcPredictedSystemLoad(EnergyPlusData &state, Real64 const RAFNFrac, int const zoneNum, int const spaceNum)
{
    // Calculate the predicted system load for a time step.

    assert(zoneNum > 0);
    auto const &thisZone = state.dataHeatBal->Zone(zoneNum);
    Real64 const thisTempZoneThermostatSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(zoneNum);
    Real64 const thisZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(zoneNum);
    Real64 const thisZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(zoneNum);

    bool thisDeadBandOrSetBack = false;
    Real64 ZoneSetPoint = 0.0;
    Real64 totalLoad = 0.0;
    Real64 LoadToHeatingSetPoint = 0.0;
    Real64 LoadToCoolingSetPoint = 0.0;

    int zoneNodeNum = thisZone.SystemZoneNodeNumber;
    if (spaceNum > 0) {
        zoneNodeNum = state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber;
    }

    switch (state.dataHeatBalFanSys->TempControlType(zoneNum)) {
    case HVAC::ThermostatType::Uncontrolled:
        // Uncontrolled Zone
        LoadToHeatingSetPoint = 0.0;
        LoadToCoolingSetPoint = 0.0;
        totalLoad = 0.0;
        break;
    case HVAC::ThermostatType::SingleHeating:
        switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            LoadToHeatingSetPoint = (this->tempDepLoad * thisTempZoneThermostatSetPoint - this->tempIndLoad);
            break;
        }
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
            if (this->tempDepLoad == 0.0) { // B=0
                LoadToHeatingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) - this->tempIndLoad;
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -this->tempDepLoad / this->AirPowerCap)));
                LoadToHeatingSetPoint =
                    this->tempDepLoad * (thisTempZoneThermostatSetPoint - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
            }
            break;
        }
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            LoadToHeatingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) +
                                    this->tempDepLoad * (thisTempZoneThermostatSetPoint) - this->tempIndLoad;
            break;
        }
        default: {
            assert(false);
        }
        }
        if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
        totalLoad = LoadToHeatingSetPoint;
        ZoneSetPoint = thisTempZoneThermostatSetPoint;
        LoadToCoolingSetPoint = LoadToHeatingSetPoint;
        // for consistency with the other cases, use LE instead of LT and don't subtract 1.0 Watt as a way of pushing the zero load
        // case over the threshold
        if ((totalLoad) <= 0.0) thisDeadBandOrSetBack = true;

        break;
    case HVAC::ThermostatType::SingleCooling:
        switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            LoadToCoolingSetPoint = this->tempDepLoad * thisTempZoneThermostatSetPoint - this->tempIndLoad;
            break;
        }
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
            if (this->tempDepLoad == 0.0) { // B=0
                LoadToCoolingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) - this->tempIndLoad;
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -this->tempDepLoad / this->AirPowerCap)));
                LoadToCoolingSetPoint =
                    this->tempDepLoad * (thisTempZoneThermostatSetPoint - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
            }
            break;
        }
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            LoadToCoolingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) +
                                    this->tempDepLoad * thisTempZoneThermostatSetPoint - this->tempIndLoad;
            break;
        }
        default: {
            assert(false);
        }
        }
        if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
        if (thisZone.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
            LoadToCoolingSetPoint = this->tempDepLoad * thisZone.AdjustedReturnTempByITE - this->tempIndLoad;
        }
        totalLoad = LoadToCoolingSetPoint;
        ZoneSetPoint = thisTempZoneThermostatSetPoint;
        LoadToHeatingSetPoint = LoadToCoolingSetPoint;
        // for consistency with the other cases, use GE instead of GT and don't add 1.0 Watt as a way of pushing the zero load
        // case over the threshold
        if ((totalLoad) >= 0.0) thisDeadBandOrSetBack = true;
        break;
    case HVAC::ThermostatType::SingleHeatCool:
        switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            LoadToHeatingSetPoint = (this->tempDepLoad * (thisTempZoneThermostatSetPoint) - this->tempIndLoad);
            LoadToCoolingSetPoint = (this->tempDepLoad * (thisTempZoneThermostatSetPoint) - this->tempIndLoad);
            break;
        }
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
            if (this->tempDepLoad == 0.0) { // B=0
                LoadToHeatingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) - this->tempIndLoad;
                LoadToCoolingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) - this->tempIndLoad;
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -this->tempDepLoad / this->AirPowerCap)));
                LoadToHeatingSetPoint =
                    this->tempDepLoad * (thisTempZoneThermostatSetPoint - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
                LoadToCoolingSetPoint =
                    this->tempDepLoad * (thisTempZoneThermostatSetPoint - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
            }
            break;
        }
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            LoadToHeatingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) +
                                    this->tempDepLoad * thisTempZoneThermostatSetPoint - this->tempIndLoad;
            LoadToCoolingSetPoint = this->AirPowerCap * (thisTempZoneThermostatSetPoint - this->T1) +
                                    this->tempDepLoad * thisTempZoneThermostatSetPoint - this->tempIndLoad;
            break;
        }
        default: {
            assert(false);
        }
        }
        ZoneSetPoint = thisTempZoneThermostatSetPoint;
        if (RAFNFrac > 0.0) {
            LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
            LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;
        }

        if (thisZone.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
            LoadToCoolingSetPoint = this->tempDepLoad * thisZone.AdjustedReturnTempByITE - this->tempIndLoad;
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
            ShowSevereError(state,
                            "HVAC::ThermostatType::SingleHeatCool: Effective heating set-point higher than effective cooling set-point - use "
                            "DualSetPointWithDeadBand if using unmixed air model");
            ShowContinueErrorTimeStamp(state, format("occurs in Zone={}", thisZone.Name));
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", this->tempDepLoad));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", this->tempIndLoad));
            ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", thisTempZoneThermostatSetPoint));
            ShowFatalError(state, "Program terminates due to above conditions.");
        }

        if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
            totalLoad = LoadToHeatingSetPoint;
        } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
            totalLoad = LoadToCoolingSetPoint;
        } else if (LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
            totalLoad = 0.0;
            if (zoneNodeNum > 0) {
                ZoneSetPoint = state.dataLoopNodes->Node(zoneNodeNum).Temp;
                ZoneSetPoint = max(ZoneSetPoint, thisZoneThermostatSetPointLo); // trap out of deadband
                ZoneSetPoint = min(ZoneSetPoint, thisZoneThermostatSetPointHi); // trap out of deadband
            }
            thisDeadBandOrSetBack = true;
        } else { // this should never occur!
            ShowSevereError(state,
                            "SingleHeatCoolSetPoint: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
            ShowContinueErrorTimeStamp(state, format("occurs in Zone={}", thisZone.Name));
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", this->tempDepLoad));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", this->tempIndLoad));
            ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", thisTempZoneThermostatSetPoint));
            ShowFatalError(state, "Program terminates due to above conditions.");
        }
        break;
    case HVAC::ThermostatType::DualSetPointWithDeadBand:
        switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
        case DataHeatBalance::SolutionAlgo::ThirdOrder: {
            LoadToHeatingSetPoint = (this->tempDepLoad * (thisZoneThermostatSetPointLo) - this->tempIndLoad);
            LoadToCoolingSetPoint = (this->tempDepLoad * (thisZoneThermostatSetPointHi) - this->tempIndLoad);
            break;
        }
        case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
            if (this->tempDepLoad == 0.0) { // B=0
                LoadToHeatingSetPoint = this->AirPowerCap * (thisZoneThermostatSetPointLo - this->T1) - this->tempIndLoad;
                LoadToCoolingSetPoint = this->AirPowerCap * (thisZoneThermostatSetPointHi - this->T1) - this->tempIndLoad;
            } else {
                Real64 const exp_700_TA(std::exp(min(700.0, -this->tempDepLoad / this->AirPowerCap)));
                LoadToHeatingSetPoint =
                    this->tempDepLoad * (thisZoneThermostatSetPointLo - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
                LoadToCoolingSetPoint =
                    this->tempDepLoad * (thisZoneThermostatSetPointHi - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
            }
            break;
        }
        case DataHeatBalance::SolutionAlgo::EulerMethod: {
            LoadToHeatingSetPoint =
                this->AirPowerCap * (thisZoneThermostatSetPointLo - this->T1) + this->tempDepLoad * thisZoneThermostatSetPointLo - this->tempIndLoad;
            LoadToCoolingSetPoint =
                this->AirPowerCap * (thisZoneThermostatSetPointHi - this->T1) + this->tempDepLoad * thisZoneThermostatSetPointHi - this->tempIndLoad;
            break;
        }
        default: {
            assert(false);
        }
        }
        if (RAFNFrac > 0.0) { // several of these inside the switch/case ??
            LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
            LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;
        }

        if (thisZone.HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
            LoadToCoolingSetPoint = this->tempDepLoad * thisZone.AdjustedReturnTempByITE - this->tempIndLoad;
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
            ShowContinueErrorTimeStamp(state, format("occurs in Zone={}", thisZone.Name));
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", this->tempDepLoad));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", this->tempIndLoad));
            ShowContinueError(state, format("Zone Heating ThermostatSetPoint={:.2R}", thisZoneThermostatSetPointLo));
            ShowContinueError(state, format("Zone Cooling ThermostatSetPoint={:.2R}", thisZoneThermostatSetPointHi));
            ShowFatalError(state, "Program terminates due to above conditions.");
        }

        if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
            totalLoad = LoadToHeatingSetPoint;
            ZoneSetPoint = thisZoneThermostatSetPointLo;
        } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
            totalLoad = LoadToCoolingSetPoint;
            ZoneSetPoint = thisZoneThermostatSetPointHi;
        } else if (LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
            // this turns out to cause instabilities sometimes? that lead to setpoint errors if predictor is off.
            totalLoad = 0.0;
            if (zoneNodeNum > 0) {
                ZoneSetPoint = state.dataLoopNodes->Node(zoneNodeNum).Temp;
                ZoneSetPoint = max(ZoneSetPoint, thisZoneThermostatSetPointLo); // trap out of deadband
                ZoneSetPoint = min(ZoneSetPoint, thisZoneThermostatSetPointHi); // trap out of deadband
            }
            thisDeadBandOrSetBack = true;
        } else { // this should never occur!
            ShowSevereError(
                state, "DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
            ShowContinueErrorTimeStamp(state, format("occurs in Zone={}", thisZone.Name));
            ShowContinueError(state,
                              format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
            ShowContinueError(state, format("Zone Heating Set-point={:.2R}", thisZoneThermostatSetPointLo));
            ShowContinueError(state, format("Zone Cooling Set-point={:.2R}", thisZoneThermostatSetPointHi));
            ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", this->tempDepLoad));
            ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", this->tempIndLoad));
            ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", thisTempZoneThermostatSetPoint));

            ShowFatalError(state, "Program terminates due to above conditions.");
        }
        break;
    default:
        break;
    }

    int systemNodeNumber = 0;
    int stageNum = 0;
    if (spaceNum > 0) {
        systemNodeNumber = state.dataHeatBal->space(spaceNum).SystemZoneNodeNumber;
        stageNum = state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).StageNum;
        assert(stageNum == state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).StageNum);
    } else {
        systemNodeNumber = thisZone.SystemZoneNodeNumber;
        stageNum = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).StageNum;
    }
    // Staged control zone
    if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
        if (state.dataZoneCtrls->StageZoneLogic(zoneNum)) {
            if (stageNum == 0) { // No load
                LoadToHeatingSetPoint = 0.0;
                LoadToCoolingSetPoint = 0.0;
                totalLoad = 0.0;
                if (systemNodeNumber > 0) {
                    ZoneSetPoint = state.dataLoopNodes->Node(systemNodeNumber).Temp;
                    ZoneSetPoint = max(ZoneSetPoint, thisZoneThermostatSetPointLo); // trap out of deadband
                    ZoneSetPoint = min(ZoneSetPoint, thisZoneThermostatSetPointHi); // trap out of deadband
                }
                thisDeadBandOrSetBack = true;
            } else if (stageNum < 0) { // Cooling load
                switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
                case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                    LoadToCoolingSetPoint = (this->tempDepLoad * (thisZoneThermostatSetPointHi) - this->tempIndLoad);
                    break;
                }
                case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                    if (this->tempDepLoad == 0.0) { // B=0
                        LoadToCoolingSetPoint = this->AirPowerCap * (thisZoneThermostatSetPointHi - this->T1) - this->tempIndLoad;
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -this->tempDepLoad / this->AirPowerCap)));
                        LoadToCoolingSetPoint =
                            this->tempDepLoad * (thisZoneThermostatSetPointHi - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
                    }
                    break;
                }
                case DataHeatBalance::SolutionAlgo::EulerMethod: {
                    LoadToCoolingSetPoint = this->AirPowerCap * (thisZoneThermostatSetPointHi - this->T1) +
                                            this->tempDepLoad * thisZoneThermostatSetPointHi - this->tempIndLoad;
                    break;
                }
                default: {
                    assert(false);
                }
                }
                totalLoad = LoadToCoolingSetPoint;
                ZoneSetPoint = thisZoneThermostatSetPointHi;
                LoadToHeatingSetPoint = LoadToCoolingSetPoint;
                if ((totalLoad) >= 0.0) thisDeadBandOrSetBack = true;
            } else { // Heating load
                switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
                case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                    LoadToHeatingSetPoint = (this->tempDepLoad * thisZoneThermostatSetPointLo - this->tempIndLoad);
                    break;
                }
                case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                    if (this->tempDepLoad == 0.0) { // B=0
                        LoadToHeatingSetPoint = this->AirPowerCap * (thisZoneThermostatSetPointLo - this->T1) - this->tempIndLoad;
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -this->tempDepLoad / this->AirPowerCap)));
                        LoadToHeatingSetPoint =
                            this->tempDepLoad * (thisZoneThermostatSetPointLo - this->T1 * exp_700_TA) / (1.0 - exp_700_TA) - this->tempIndLoad;
                    }
                    break;
                }
                case DataHeatBalance::SolutionAlgo::EulerMethod: {
                    LoadToHeatingSetPoint = this->AirPowerCap * (thisZoneThermostatSetPointLo - this->T1) +
                                            this->tempDepLoad * (thisZoneThermostatSetPointLo) - this->tempIndLoad;
                    break;
                }
                default: {
                    assert(false);
                }
                }
                totalLoad = LoadToHeatingSetPoint;
                ZoneSetPoint = thisZoneThermostatSetPointLo;
                LoadToCoolingSetPoint = LoadToHeatingSetPoint;
                if ((totalLoad) <= 0.0) thisDeadBandOrSetBack = true;
            }
        }
    }

    // If the ZoneNodeNum has been set for a Controlled Zone, then the zone setpoint is placed on the node.
    if (zoneNodeNum > 0) {
        state.dataLoopNodes->Node(zoneNodeNum).TempSetPoint = ZoneSetPoint;
    }

    state.dataZoneEnergyDemand->Setback(zoneNum) = (ZoneSetPoint > this->setPointLast);

    this->setPointLast = ZoneSetPoint;
    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(zoneNum) = ZoneSetPoint; // needed to fix Issue # 5048
    state.dataZoneEnergyDemand->DeadBandOrSetback(zoneNum) = thisDeadBandOrSetBack;
    state.dataZoneEnergyDemand->CurDeadBandOrSetback(zoneNum) = thisDeadBandOrSetBack;

    // Apply the Zone Multiplier and Load Correction factor as needed
    if (spaceNum > 0) {
        state.dataZoneEnergyDemand->spaceSysEnergyDemand(spaceNum).reportSensibleLoadsZoneMultiplier(
            state, zoneNum, totalLoad, LoadToHeatingSetPoint, LoadToCoolingSetPoint);
    } else {
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneNum).reportSensibleLoadsZoneMultiplier(
            state, zoneNum, totalLoad, LoadToHeatingSetPoint, LoadToCoolingSetPoint);
    }
}
} // namespace EnergyPlus::ZoneTempPredictorCorrector
