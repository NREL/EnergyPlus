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
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
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
#include <EnergyPlus/TempSolveRoot.hh>
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

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // Controls for PredictorCorrector
    // INTEGER, PUBLIC, PARAMETER :: iGetZoneSetPoints             = 1
    // INTEGER, PUBLIC, PARAMETER :: iPredictStep                  = 2
    // INTEGER, PUBLIC, PARAMETER :: iCorrectStep                  = 3
    // INTEGER, PUBLIC, PARAMETER :: iRevertZoneTimestepHistories  = 4
    // INTEGER, PUBLIC, PARAMETER :: iPushZoneTimestepHistories    = 5
    // INTEGER, PUBLIC, PARAMETER :: iPushSystemTimestepHistories  = 6

    Array1D_string const ValidControlTypes(4,
                                           {"ThermostatSetpoint:SingleHeating",
                                            "ThermostatSetpoint:SingleCooling",
                                            "ThermostatSetpoint:SingleHeatingOrCooling",
                                            "ThermostatSetpoint:DualSetpoint"});

    Array1D_string const ValidComfortControlTypes(12,
                                                  {"ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating",
                                                   "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling",
                                                   "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling",
                                                   "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint",
                                                   "ThermostatSetpoint:ThermalComfort:Pierce:SingleHeating",
                                                   "ThermostatSetpoint:ThermalComfort:Pierce:SingleCooling",
                                                   "ThermostatSetpoint:ThermalComfort:Pierce:SingleHeatingOrCooling",
                                                   "ThermostatSetpoint:ThermalComfort:Pierce:DualSetpoint",
                                                   "ThermostatSetpoint:ThermalComfort:KSU:SingleHeating",
                                                   "ThermostatSetpoint:ThermalComfort:KSU:SingleCooling",
                                                   "ThermostatSetpoint:ThermalComfort:KSU:SingleHeatingOrCooling",
                                                   "ThermostatSetpoint:ThermalComfort:KSU:DualSetpoint"});

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
    void ManageZoneAirUpdates(EnergyPlusData &state, int const UpdateType,   // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
                              Real64 &ZoneTempChange, // Temp change in zone air btw previous and current timestep
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

        {
            auto const SELECT_CASE_var(UpdateType);

            if (SELECT_CASE_var == iGetZoneSetPoints) {
                CalcZoneAirTempSetPoints(state);

            } else if (SELECT_CASE_var == iPredictStep) {
                PredictSystemLoads(state, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            } else if (SELECT_CASE_var == iCorrectStep) {
                CorrectZoneAirTemp(state, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            } else if (SELECT_CASE_var == iRevertZoneTimestepHistories) {
                RevertZoneTimestepHistories(state);

            } else if (SELECT_CASE_var == iPushZoneTimestepHistories) {
                PushZoneTimestepHistories(state);

            } else if (SELECT_CASE_var == iPushSystemTimestepHistories) {
                PushSystemTimestepHistories(state);
            }
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
        using namespace DataIPShortCuts;
        using General::CheckCreatedZoneItemName;
        using General::FindNumberInList;

        using ScheduleManager::CheckScheduleValue;
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr auto RoutineName("GetZoneAirSetpoints: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TempControlledZoneNum; // The Splitter that you are currently loading input into
        int NumAlphas;
        int NumNums;
        int SingleTempHeatingControlNum;
        int SingleTempCoolingControlNum;
        int SingleTempHeatCoolControlNum;
        int DualTempHeatCoolControlNum;
        int ControlTypeNum;
        int IOStat;
        bool ErrorsFound(false);
        bool errFlag;
        int CTIndex;
        int HumidControlledZoneNum; // The Humidity Controller that information is being loaded into
        bool ValidScheduleControlType;
        bool ValidRadFractSched;          // check for if radiative fraction schedule has valid numbers
        bool ValidZoneOvercoolRangeSched; // check for if Zone Overcool range schedule has valid numbers
        int TempIndex;
        int SchedMin;
        int SchedMax;
        int ActualZoneNum;
        int SchedTypeIndex;

        int ComfortControlledZoneNum; // The Splitter that you are currently loading input into
        int i;
        int IZoneCount;

        int OpTempContrlNum; // do loop index
        int found;

        int TempHumidityCntrlNum; // do loop index for overcooled controlled zone

        int SingleFangerHeatingControlNum;
        int SingleFangerCoolingControlNum;
        int SingleFangerHeatCoolControlNum;
        int DualFangerHeatCoolControlNum;
        int ComfortIndex;
        int ZoneAssigned;

        int NumStageControlledZones; // Number of staged controlled objects
        int StageControlledZoneNum;  // Index for staged controlled zones

        Array1D_int CTSchedMapToControlledZone;
        Array1D_int CCmSchedMapToControlledZone;
        int Item;
        int Item1;
        int ZLItem;

        struct NeededControlTypes
        {
            // Members
            Array1D_bool MustHave; // 4= the four control types
            Array1D_bool DidHave;

            // Default Constructor
            NeededControlTypes() : MustHave(4, false), DidHave(4, false)
            {
            }
        };

        struct NeededComfortControlTypes
        {
            // Members
            Array1D_bool MustHave; // 4= the four control types
            Array1D_bool DidHave;

            // Default Constructor
            NeededComfortControlTypes() : MustHave(12, false), DidHave(12, false)
            {
            }
        };

        // Object Data
        Array1D<NeededControlTypes> TStatControlTypes;
        Array1D<NeededComfortControlTypes> TComfortControlTypes;

        // Formats
        static constexpr auto Header("! <Zone Volume Capacitance Multiplier>, Sensible Heat Capacity Multiplier, Moisture Capacity Multiplier, Carbon Dioxide Capacity Multiplier, Generic Contaminant Capacity Multiplier\n");
        static constexpr auto Format_701("Zone Volume Capacitance Multiplier,{:8.3F} ,{:8.3F},{:8.3F},{:8.3F}\n");


        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::TStat));
        state.dataZoneCtrls->NumTStatStatements = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        state.dataZoneCtrls->TStatObjects.allocate(state.dataZoneCtrls->NumTStatStatements);

        // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
        state.dataZoneCtrls->NumTempControlledZones = 0;
        for (Item = 1; Item <= state.dataZoneCtrls->NumTStatStatements; ++Item) {
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

            state.dataZoneCtrls->TStatObjects(Item).Name = cAlphaArgs(1);
            Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataZoneCtrls->TStatObjects(Item).TempControlledZoneStartPtr = state.dataZoneCtrls->NumTempControlledZones + 1;
                ++state.dataZoneCtrls->NumTempControlledZones;
                state.dataZoneCtrls->TStatObjects(Item).NumOfZones = 1;
                state.dataZoneCtrls->TStatObjects(Item).ZoneListActive = false;
                state.dataZoneCtrls->TStatObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataZoneCtrls->TStatObjects(Item).TempControlledZoneStartPtr = state.dataZoneCtrls->NumTempControlledZones + 1;
                state.dataZoneCtrls->NumTempControlledZones += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataZoneCtrls->TStatObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataZoneCtrls->TStatObjects(Item).ZoneListActive = true;
                state.dataZoneCtrls->TStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowSevereError(state, "GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataZoneCtrls->NumTempControlledZones = 0;
        }

        if (state.dataZoneCtrls->NumTempControlledZones > 0) {
            state.dataZoneCtrls->TempControlledZone.allocate(state.dataZoneCtrls->NumTempControlledZones);
            TStatControlTypes.allocate(state.dataZoneCtrls->NumTempControlledZones); // Number of set point types
            CTSchedMapToControlledZone.dimension(state.dataZoneCtrls->NumTempControlledZones, 0);

            TempControlledZoneNum = 0;
            state.dataZoneTempPredictorCorrector->NumOnOffCtrZone = 0;
            for (Item = 1; Item <= state.dataZoneCtrls->NumTStatStatements; ++Item) {
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
                for (Item1 = 1; Item1 <= state.dataZoneCtrls->TStatObjects(Item).NumOfZones; ++Item1) {
                    ++TempControlledZoneNum;
                    if (state.dataZoneCtrls->TStatObjects(Item).ZoneListActive) {
                        cAlphaArgs(2) = state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataZoneCtrls->TStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                    }
                    ZoneAssigned =
                        UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataZoneCtrls->TempControlledZone, &ZoneTempControls::ZoneName, TempControlledZoneNum - 1);
                    if (ZoneAssigned == 0) {
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName = cAlphaArgs(2);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" +
                                            cAlphaArgs(2) + "\" not found.");
                            ErrorsFound = true;
                        } else {
                            state.dataHeatBal->Zone(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum).TempControlledZoneIndex = TempControlledZoneNum;
                        }
                    } else {
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                        ShowContinueError(state, "...Zone was previously assigned to Thermostat=\"" + state.dataZoneCtrls->TempControlledZone(ZoneAssigned).Name + "\".");
                        ErrorsFound = true;
                        continue;
                    }

                    if (!state.dataZoneCtrls->TStatObjects(Item).ZoneListActive) {
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name = cAlphaArgs(1);
                    } else {
                        CheckCreatedZoneItemName(state, RoutineName,
                                                 cCurrentModuleObject,
                                                 state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataZoneCtrls->TStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name,
                                                 state.dataHeatBal->ZoneList(state.dataZoneCtrls->TStatObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                                                 state.dataZoneCtrls->TStatObjects(Item).Name,
                                                 state.dataZoneCtrls->TempControlledZone,
                                                 TempControlledZoneNum - 1,
                                                 state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name,
                                                 errFlag);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName = cAlphaArgs(3);
                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).CTSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
                    if (Item1 == 1) { // only show error on first of several if zone list
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).CTSchedIndex == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                            cAlphaArgs(3) + "\" not found.");
                            ErrorsFound = true;
                        } else {
                            // Check validity of control types.
                            ValidScheduleControlType =
                                CheckScheduleValueMinMax(state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).CTSchedIndex, ">=", 0.0, "<=", 4.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(2) + "=\"" +
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

                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes = nint((NumAlphas - 3.0) / 2.0);
                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType.allocate(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);
                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName.allocate(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);
                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx.allocate(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);

                    for (ControlTypeNum = 1; ControlTypeNum <= state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes; ++ControlTypeNum) {

                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3));
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 3));

                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum) != "") {
                            CTIndex = UtilityRoutines::FindItem(
                                state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum), ValidControlTypes, 4);
                            if (CTIndex == 0) {
                                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3)) + "\"");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                            cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)) + "=\"<blank>\"");
                            ErrorsFound = true;
                        }
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(ControlTypeNum) = 0;
                    }
                    if (NumNums > 0) {
                        if (rNumericArgs(1) >= 0.0) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DeltaTCutSet = rNumericArgs(1);
                            if (rNumericArgs(1) > 0.0) state.dataZoneTempPredictorCorrector->NumOnOffCtrZone++;
                        } else {
                            ShowSevereError(
                                state,
                                format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                            ShowContinueError(state, "..Allowable values must be greater or equal to 0");
                            ErrorsFound = true;
                        }
                    }
                    if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DeltaTCutSet > 0.0) {
                        for (ControlTypeNum = 1; ControlTypeNum <= state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes; ++ControlTypeNum) {
                            if (UtilityRoutines::SameString(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum),
                                                            "ThermostatSetpoint:SingleHeatingOrCooling")) {
                                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                                 ": The choice of Temperature Difference Between Cutout And Setpoint will not be applied to "
                                                 "ThermostatSetpoint:SingleHeatingOrCooling.");
                            }
                        }
                    }
                }
            } // NumTStatStatements
        }     // Check on number of TempControlledZones

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint));
        state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls > 0) state.dataZoneTempPredictorCorrector->SetPointSingleHeating.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls);

        for (SingleTempHeatingControlNum = 1; SingleTempHeatingControlNum <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls; ++SingleTempHeatingControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          SingleTempHeatingControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // SingleTempHeatingControlNum

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint));
        state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls > 0) state.dataZoneTempPredictorCorrector->SetPointSingleCooling.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls);

        for (SingleTempCoolingControlNum = 1; SingleTempCoolingControlNum <= state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls; ++SingleTempCoolingControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          SingleTempCoolingControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // SingleTempCoolingControlNum

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint));
        state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls > 0) state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool.allocate(state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls);

        for (SingleTempHeatCoolControlNum = 1; SingleTempHeatCoolControlNum <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls; ++SingleTempHeatCoolControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          SingleTempHeatCoolControlNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // SingleTempHeatCoolControlNum

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint));
        state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls > 0) state.dataZoneTempPredictorCorrector->SetPointDualHeatCool.allocate(state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls);

        for (DualTempHeatCoolControlNum = 1; DualTempHeatCoolControlNum <= state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls; ++DualTempHeatCoolControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          DualTempHeatCoolControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName = cAlphaArgs(3);
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
            if (state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // DualTempHeatCoolControlNum

        // Finish filling in Schedule pointing indexes
        for (TempControlledZoneNum = 1; TempControlledZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TempControlledZoneNum) {
            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)),
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType,
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatSetPoint = TempIndex;
            if (TempIndex > 0) {
                state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), state.dataZoneTempPredictorCorrector->SetPointSingleHeating);
                TStatControlTypes(TempControlledZoneNum).MustHave(SingleHeatingSetPoint) = true;
            }

            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)),
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType,
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_SingleCoolSetPoint = TempIndex;
            if (TempIndex > 0) {
                state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), state.dataZoneTempPredictorCorrector->SetPointSingleCooling);
                TStatControlTypes(TempControlledZoneNum).MustHave(SingleCoolingSetPoint) = true;
            }

            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)),
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType,
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatCoolSetPoint = TempIndex;
            if (TempIndex > 0) {
                state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool);
                TStatControlTypes(TempControlledZoneNum).MustHave(SingleHeatCoolSetPoint) = true;
            }

            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)),
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlType,
                                                  state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_DualSetPointWDeadBand = TempIndex;
            if (TempIndex > 0) {
                state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), state.dataZoneTempPredictorCorrector->SetPointDualHeatCool);
                TStatControlTypes(TempControlledZoneNum).MustHave(DualSetPointWithDeadBand) = true;
            }
        }

        // Now, Check the schedule values/indices for validity

        for (TempControlledZoneNum = 1; TempControlledZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TempControlledZoneNum) {

            ActualZoneNum = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum;
            CTIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).CTSchedIndex;
            if (CTIndex == 0) continue; // error will be caught elsewhere
            SchedMin = GetScheduleMinValue(state, CTIndex);
            SchedMax = GetScheduleMaxValue(state, CTIndex);

            if (SchedMin == 0 && SchedMax == 0) {
                if (FindNumberInList(CTIndex, CTSchedMapToControlledZone, state.dataZoneCtrls->NumTempControlledZones) == 0) {
                    ShowSevereError(state, "Control Type Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                    ShowContinueError(state, "..specifies control type 0 for all entries.");
                    ShowContinueError(state, "All zones using this Control Type Schedule have no heating or cooling available.");
                }
                CTSchedMapToControlledZone(TempControlledZoneNum) = CTIndex;
            }

            for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {

                        TempIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatSetPoint;
                        TStatControlTypes(TempControlledZoneNum).DidHave(SingleHeatingSetPoint) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)) +
                                                " Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(state, CTIndex, SingleHeatingSetPoint)) {
                                ShowSevereError(state, "Control Type Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies control type 1 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {

                        TempIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_SingleCoolSetPoint;
                        TStatControlTypes(TempControlledZoneNum).DidHave(SingleCoolingSetPoint) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)) +
                                                " Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(state, CTIndex, SingleCoolingSetPoint)) {
                                ShowSevereError(state, "Control Type Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies control type 2 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {

                        TempIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatCoolSetPoint;
                        TStatControlTypes(TempControlledZoneNum).DidHave(SingleHeatCoolSetPoint) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)) +
                                                " Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(state, CTIndex, SingleHeatCoolSetPoint)) {
                                ShowSevereError(state, "Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies control type 3 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {

                        TempIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).SchIndx_DualSetPointWDeadBand;
                        TStatControlTypes(TempControlledZoneNum).DidHave(DualSetPointWithDeadBand) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)) +
                                                " Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(state, CTIndex, DualSetPointWithDeadBand)) {
                                ShowSevereError(state, "Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies control type 4 (" + ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                               state.dataHeatBal->Zone(ActualZoneNum).Name,
                                               ControlTypeNum,
                                               state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName));
                        ShowContinueError(state, "..valid range values are [0,4].");
                        ErrorsFound = true;
                    }
                }
            }
        }

        for (TempControlledZoneNum = 1; TempControlledZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++TempControlledZoneNum) {

            ActualZoneNum = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum;
            CTIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).CTSchedIndex;
            if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

            for (ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum) {
                if (TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum) &&
                    TStatControlTypes(TempControlledZoneNum).DidHave(ControlTypeNum))
                    continue;

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 1 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 2 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 3 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 4 (" + ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else {
                    }
                }
            }
        }

        if (allocated(TStatControlTypes)) TStatControlTypes.deallocate();
        // This starts the Humidity Control Get Input section
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::HStat));
        state.dataZoneCtrls->NumHumidityControlZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneCtrls->NumHumidityControlZones > 0) {
            state.dataZoneCtrls->HumidityControlZone.allocate(state.dataZoneCtrls->NumHumidityControlZones);
            state.dataZoneTempPredictorCorrector->HumidityControlZoneUniqueNames.reserve(static_cast<unsigned>(state.dataZoneCtrls->NumHumidityControlZones));
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

            state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ControlName = cAlphaArgs(1);
            GlobalNames::IntraObjUniquenessCheck(state,
                cAlphaArgs(2), cCurrentModuleObject, cAlphaFieldNames(2), state.dataZoneTempPredictorCorrector->HumidityControlZoneUniqueNames, ErrorsFound);

            state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ZoneName = cAlphaArgs(2);
            state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItem(cAlphaArgs(2), state.dataHeatBal->Zone);
            if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ActualZoneNum == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
            state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).HumidifyingSched = cAlphaArgs(3);
            state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
            if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            }
            if (NumAlphas == 4) {
                state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).DehumidifyingSched = cAlphaArgs(4);
                state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).DehumidifyingSched = cAlphaArgs(3);
                state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
            }

        } // HumidControlledZoneNum

        // Start to read Thermal comfort control objects
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::TCTStat));
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
            UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
            state.dataZoneCtrls->ComfortTStatObjects(Item).Name = cAlphaArgs(1);
            if (Item1 > 0) {
                state.dataZoneCtrls->ComfortTStatObjects(Item).ComfortControlledZoneStartPtr = state.dataZoneCtrls->NumComfortControlledZones + 1;
                ++state.dataZoneCtrls->NumComfortControlledZones;
                state.dataZoneCtrls->ComfortTStatObjects(Item).NumOfZones = 1;
                state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneListActive = false;
                state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataZoneCtrls->ComfortTStatObjects(Item).ComfortControlledZoneStartPtr = state.dataZoneCtrls->NumComfortControlledZones + 1;
                state.dataZoneCtrls->NumComfortControlledZones += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataZoneCtrls->ComfortTStatObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneListActive = true;
                state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
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
            state.dataZoneCtrls->ComfortControlledZone.allocate(state.dataZoneCtrls->NumComfortControlledZones);
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
                for (Item1 = 1; Item1 <= state.dataZoneCtrls->ComfortTStatObjects(Item).NumOfZones; ++Item1) {
                    ++ComfortControlledZoneNum;
                    if (state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneListActive) {
                        cAlphaArgs(2) = state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                    }
                    ZoneAssigned = UtilityRoutines::FindItemInList(
                        cAlphaArgs(2), state.dataZoneCtrls->ComfortControlledZone, &ZoneComfortControls::ZoneName, ComfortControlledZoneNum - 1);
                    if (ZoneAssigned == 0) {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName = cAlphaArgs(2);
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                        if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" +
                                            cAlphaArgs(2) + "\" not found.");
                            ErrorsFound = true;
                        }
                    } else {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                        ShowContinueError(state, "...Zone was previously assigned to Thermostat=\"" + state.dataZoneCtrls->ComfortControlledZone(ZoneAssigned).Name + "\".");
                        ErrorsFound = true;
                        continue;
                    }

                    if (!state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneListActive) {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name = cAlphaArgs(1);
                    } else {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name =
                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataZoneCtrls->ComfortTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name + ' ' + state.dataZoneCtrls->ComfortTStatObjects(Item).Name;
                    }

                    // Read Fields A3 and A4 for averaging method
                    IZoneCount = 0;
                    for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                        if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) {
                            ++IZoneCount;
                        }
                    }
                    // Could not find a people object for this particular zone
                    if (IZoneCount == 0 && state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum > 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " no PEOPLE in " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\" - cannot use Comfort Control.");
                        ErrorsFound = true;
                    }
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::NO);
                    if (IZoneCount > 1) {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodName = cAlphaArgs(3);
                        if (UtilityRoutines::SameString(cAlphaArgs(3), "SpecificObject")) {
                            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::SPE);
                        }
                        if (UtilityRoutines::SameString(cAlphaArgs(3), "ObjectAverage")) {
                            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::OBJ);
                        }
                        if (UtilityRoutines::SameString(cAlphaArgs(3), "PeopleAverage")) {
                            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::PEO);
                        }
                        if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\".");
                            ShowContinueError(state, "Allowed keys are SpecificObject, ObjectAverage, or PeopleAverage");
                            ErrorsFound = true;
                        }
                        if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum == static_cast<int>(AverageMethod::SPE)) {
                            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).AverageObjectName = cAlphaArgs(4);
                            if (UtilityRoutines::FindItem(cAlphaArgs(4), state.dataHeatBal->People) == 0) {
                                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                cAlphaArgs(4) + "\".");
                                ErrorsFound = true;
                            } else {
                                state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SpecificObjectNum = UtilityRoutines::FindItem(cAlphaArgs(4), state.dataHeatBal->People);
                            }
                        }
                    } else {
                        for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                            if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) break;
                        }
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SpecificObjectNum = i;
                    }
                    // Check values used for thermal comfort calculation
                    for (i = 1; i <= state.dataHeatBal->TotPeople; ++i) {
                        if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == state.dataHeatBal->People(i).ZonePtr) {
                            // Check activity level
                            if (state.dataHeatBal->People(i).ActivityLevelPtr > 0) {
                                ValidScheduleControlType = CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).ActivityLevelPtr, ">=", 72.0, "<=", 909.0);
                                if (!ValidScheduleControlType) {
                                    ShowSevereError(state,
                                        "GetPeople Activity Level: Invalid activity level values entered for thermal comfort calculation");
                                    ShowContinueError(state, "Outside of range values [72,909], Reference object=" + state.dataHeatBal->People(i).Name);
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(state, "GetPeople Activity Level: Activity level schedule is not found=" + state.dataHeatBal->People(i).Name);
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
                                ShowSevereError(state, "GetPeople work efficiency: Work efficiency schedule is not found=" + state.dataHeatBal->People(i).Name);
                                ShowContinueError(state, "Required when the zone has Thermal Comfort Controls.");
                                ErrorsFound = true;
                            }
                            // Check Clothing Insulation
                            if (state.dataHeatBal->People(i).ClothingPtr > 0) {
                                ValidScheduleControlType = CheckScheduleValueMinMax(state, state.dataHeatBal->People(i).ClothingPtr, ">", 0.0, "<=", 2.0);
                                if (!ValidScheduleControlType) {
                                    ShowSevereError(state,
                                        "GetPeople Clothing Insulation: Invalid Clothing Insulation values entered for thermal comfort calculation");
                                    ShowContinueError(state, "Outside of range values [0.0,2.0], Reference object=" + state.dataHeatBal->People(i).Name);
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(state, "GetPeople Clothing Insulation: Clothing Insulation schedule is not found=" + state.dataHeatBal->People(i).Name);
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
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint = rNumericArgs(1);
                        if (rNumericArgs(1) > 50 || rNumericArgs(1) < 0) {
                            ShowSevereError(
                                state,
                                format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(1), rNumericArgs(1)));
                            ShowContinueError(state, "..Allowable values must be between 0 C and 50 C");
                            ErrorsFound = true;
                        }
                    }
                    if (NumNums > 1) {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint = rNumericArgs(2);
                        if (rNumericArgs(2) > 50 || rNumericArgs(2) < 0) {
                            ShowSevereError(
                                state,
                                format("{}=\"{} invalid {}=[{:.0T}].", cCurrentModuleObject, cAlphaArgs(1), cNumericFieldNames(2), rNumericArgs(2)));
                            ShowContinueError(state, "..Allowable values must be between 0 C and 50 C");
                            ErrorsFound = true;
                        }
                    }
                    // Ensure MaxTemp >= MinTemp
                    if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint >
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                        ShowContinueError(state, ".." + cNumericFieldNames(1) + " > " + cNumericFieldNames(2));
                        ShowContinueError(state, format("..[{:.0T}] > [{:.0T}].", rNumericArgs(1), rNumericArgs(2)));
                        ErrorsFound = true;
                    }
                    // If MaxTemp = MinTemp, no thermal comfort control
                    if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint ==
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                        ShowContinueError(state, ".." + cNumericFieldNames(1) + " = " + cNumericFieldNames(2));
                        ShowContinueError(state, "The zone will be controlled using this dry-bulb temperature setpoint.");
                    }
                    // read Thermal comfort type schedule name
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName = cAlphaArgs(5);
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex = GetScheduleIndex(state, cAlphaArgs(5));
                    if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                        "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Check validity of control types.
                        ValidScheduleControlType =
                            CheckScheduleValueMinMax(state, state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex, ">=", 0.0, "<=", 4.0);
                        if (!ValidScheduleControlType) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(5) + "=\"" +
                                            cAlphaArgs(5) + "\"");
                            ShowContinueError(state, "..contains values outside of range [0,4].");
                            ErrorsFound = true;
                        }
                    }
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes = nint((NumAlphas - 5.0) / 2.0);
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum)
                        .ControlType.allocate(state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum)
                        .ControlTypeName.allocate(state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum)
                        .ControlTypeSchIndx.allocate(state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);

                    for (ControlTypeNum = 1; ControlTypeNum <= state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes; ++ControlTypeNum) {
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5));
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 5));
                        if (state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum) != "") {
                            CTIndex = UtilityRoutines::FindItem(
                                state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum), ValidComfortControlTypes, 12);
                            if (CTIndex == 0) {
                                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5)) + "\"");
                                ErrorsFound = true;
                            }
                            if (CTIndex > 4) { // For Fanger control only for the time being
                                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5)) + "\"");
                                ShowContinueError(state, "..Fanger is the only valid model.");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                            cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"<blank>\"");
                            ErrorsFound = true;
                        }
                        state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ControlTypeNum) = 0;
                    }
                }
            } // NumComfortTStatStatements
        }
        // End of Thermal comfort control reading and checking

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger));
        state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls > 0) state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger.allocate(state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls);

        for (SingleFangerHeatingControlNum = 1; SingleFangerHeatingControlNum <= state.dataZoneTempPredictorCorrector->NumSingleFangerHeatingControls; ++SingleFangerHeatingControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          SingleFangerHeatingControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError(state, "..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }
        } // SingleFangerHeatingControlNum

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger));
        state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls > 0) {
            state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger.allocate(state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls);
        }

        for (SingleFangerCoolingControlNum = 1; SingleFangerCoolingControlNum <= state.dataZoneTempPredictorCorrector->NumSingleFangerCoolingControls; ++SingleFangerCoolingControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          SingleFangerCoolingControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(state, state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError(state, "..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }

        } // SingleFangerCoolingControlNum

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger));
        state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls > 0) state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger.allocate(state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls);

        for (SingleFangerHeatCoolControlNum = 1; SingleFangerHeatCoolControlNum <= state.dataZoneTempPredictorCorrector->NumSingleFangerHeatCoolControls;
             ++SingleFangerHeatCoolControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          SingleFangerHeatCoolControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError(state, "..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }

        } // SingleFangerHeatCoolControlNum

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger));
        state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls > 0) state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger.allocate(state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls);

        for (DualFangerHeatCoolControlNum = 1; DualFangerHeatCoolControlNum <= state.dataZoneTempPredictorCorrector->NumDualFangerHeatCoolControls; ++DualFangerHeatCoolControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          DualFangerHeatCoolControlNum,
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

            state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).Name = cAlphaArgs(1);
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSetptSchedName = cAlphaArgs(2);
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSetptSchedName = cAlphaArgs(3);
            state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
            if (state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSchedIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError(state, "..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\" entered.");
                    ShowContinueError(state, "..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }

        } // DualFangerHeatCoolControlNum

        // Finish filling in Schedule pointing indexes for Thermal Comfort Control
        for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {
            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)),
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHeatSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) = true;
            }

            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)),
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglCoolSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) = true;
            }

            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)),
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHCSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::SglHCSetPointFanger)) = true;
            }

            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)),
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_DualSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::DualSetPointFanger)) = true;
            }
        }

        // Now, Check the schedule values/indices for validity for Thermal Comfort Control

        for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {

            ActualZoneNum = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum;
            CTIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex;
            if (CTIndex == 0) continue; // error will be caught elsewhere
            SchedMin = GetScheduleMinValue(state, CTIndex);
            SchedMax = GetScheduleMaxValue(state, CTIndex);

            if (SchedMin == 0 && SchedMax == 0) {
                if (FindNumberInList(CTIndex, CCmSchedMapToControlledZone, state.dataZoneCtrls->NumComfortControlledZones) == 0) {
                    ShowWarningError(state, "Control Type Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                    ShowContinueError(state, "..specifies control type 0 for all entries.");
                    ShowContinueError(state, "All zones using this Control Type Schedule have no thermal comfort control.");
                }
                CCmSchedMapToControlledZone(ComfortControlledZoneNum) = CTIndex;
            }

            for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == 0) { // Thermal comfort uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {

                        ComfortIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHeatSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) +
                                                " Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(state, CTIndex, static_cast<int>(ComfortControl::SglHeatSetPointFanger))) {
                                ShowSevereError(state, "Control Type Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies thermal control type 1 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {

                        ComfortIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglCoolSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) +
                                                " Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(state, CTIndex, static_cast<int>(ComfortControl::SglCoolSetPointFanger))) {
                                ShowSevereError(state, "Control Type Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies thermal control type 2 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {

                        ComfortIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHCSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::SglHCSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)) +
                                                " Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(state, CTIndex, static_cast<int>(ComfortControl::SglHCSetPointFanger))) {
                                ShowSevereError(state, "Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies thermal control type 3 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {

                        ComfortIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).SchIndx_DualSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::DualSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError(state, "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)) +
                                                " Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(state, CTIndex, static_cast<int>(ComfortControl::DualSetPointFanger))) {
                                ShowSevereError(state, "Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError(state, "..specifies thermal control type 4 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }
                        // CASE PIERCE
                        // CASE KSU

                    } else {
                        ShowSevereError(state,
                                        format("GetZoneAirSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                               state.dataHeatBal->Zone(ActualZoneNum).Name,
                                               ControlTypeNum,
                                               state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName));
                        ShowContinueError(state, "..valid range values are [0,4].");
                        ErrorsFound = true;
                    }
                }
            }
        }

        for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++ComfortControlledZoneNum) {

            ActualZoneNum = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum;
            CTIndex = state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex;
            if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

            for (ControlTypeNum = 1; ControlTypeNum <= 12; ++ControlTypeNum) {
                if (TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum) &&
                    TComfortControlTypes(ComfortControlledZoneNum).DidHave(ControlTypeNum))
                    continue;

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 1 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 2 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 3 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError(state, "Schedule=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError(state, "...should include control type 4 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)) + ") but does not.");
                        ShowContinueError(state, "..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError(state, "..reference ZONE=" + state.dataZoneCtrls->ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                        // CASE PIERCE
                        // CASE KSU

                    } else {
                    }
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
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ZoneNum++) {
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens = ZoneVolCapMultpSens;
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist = ZoneVolCapMultpMoist;
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2;
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam;
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
                    Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                    if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
                    if (Item1 > 0) {
                        ZoneNum = Item1;
                        state.dataHeatBal->Zone(ZoneNum).FlagCustomizedZoneCap = true;
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens = rNumericArgs(1);
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist = rNumericArgs(2);
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 = rNumericArgs(3);
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam = rNumericArgs(4);
                    } else if (ZLItem > 0) {
                        for (int ZonePtrNum = 1; ZonePtrNum < state.dataHeatBal->ZoneList(ZLItem).NumOfZones; ZonePtrNum++) {
                            ZoneNum = state.dataHeatBal->ZoneList(ZLItem).Zone(ZonePtrNum);
                            state.dataHeatBal->Zone(ZoneNum).FlagCustomizedZoneCap = true;
                            state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens = rNumericArgs(1);
                            state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist = rNumericArgs(2);
                            state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 = rNumericArgs(3);
                            state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam = rNumericArgs(4);
                        }

                    } else {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" not found.");
                        ErrorsFound = true;
                    }
                }
            }

            // Assign default multiplier values to all the other zones
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ZoneNum++) {
                if (!state.dataHeatBal->Zone(ZoneNum).FlagCustomizedZoneCap) {
                    state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens = ZoneVolCapMultpSens;
                    state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist = ZoneVolCapMultpMoist;
                    state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2;
                    state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam;
                }
            }

            // Calculate the average multiplier value from all zones
            {
                Real64 ZoneVolCapMultpSens_temp = 0.0;
                Real64 ZoneVolCapMultpMoist_temp = 0.0;
                Real64 ZoneVolCapMultpCO2_temp = 0.0;
                Real64 ZoneVolCapMultpGenContam_temp = 0.0;

                for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ZoneNum++) {
                    ZoneVolCapMultpSens_temp += state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens;
                    ZoneVolCapMultpMoist_temp += state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist;
                    ZoneVolCapMultpCO2_temp += state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpCO2;
                    ZoneVolCapMultpGenContam_temp += state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpGenContam;
                }

                if (state.dataGlobal->NumOfZones > 0) {
                    ZoneVolCapMultpSens = ZoneVolCapMultpSens_temp / state.dataGlobal->NumOfZones;
                    ZoneVolCapMultpMoist = ZoneVolCapMultpMoist_temp / state.dataGlobal->NumOfZones;
                    ZoneVolCapMultpCO2 = ZoneVolCapMultpCO2_temp / state.dataGlobal->NumOfZones;
                    ZoneVolCapMultpGenContam = ZoneVolCapMultpGenContam_temp / state.dataGlobal->NumOfZones;
                }
            }
        }

        print(state.files.eio, Header);
        print(state.files.eio, Format_701, ZoneVolCapMultpSens, ZoneVolCapMultpMoist, ZoneVolCapMultpCO2, ZoneVolCapMultpGenContam);

        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::OTTStat));
        state.dataZoneCtrls->NumOpTempControlledZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneCtrls->NumOpTempControlledZones > 0) {
            state.dataZoneCtrls->AnyOpTempControl = true;

            for (OpTempContrlNum = 1; OpTempContrlNum <= state.dataZoneCtrls->NumOpTempControlledZones; ++OpTempContrlNum) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              OpTempContrlNum,
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
                found = UtilityRoutines::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TStatObjects);
                if (found == 0) {
                    // It might be in the TempControlledZones
                    found = UtilityRoutines::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TempControlledZone);
                    if (found == 0) { // throw error
                        ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) +
                                        " reference not found.");
                        ErrorsFound = true;
                    } else {
                        TempControlledZoneNum = found;
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OperativeTempControl = true;
                        if (UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled")) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled = true;
                        }
                        if ((!(UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled"))) &&
                            (!(UtilityRoutines::SameString(cAlphaArgs(2), "Constant")))) {
                            ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\".");
                            ErrorsFound = true;
                        }

                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction = rNumericArgs(1);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched = GetScheduleIndex(state, cAlphaArgs(3));
                        if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched == 0) &&
                            (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled)) { // throw error
                            ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\" not found.");
                            ErrorsFound = true;
                        }

                        // check validity of fixed radiative fraction
                        if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction < 0.0) &&
                            (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                        if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction >= 0.9) &&
                            (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot >= .9.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }

                        // check schedule min max.
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled) {
                            ValidRadFractSched =
                                CheckScheduleValueMinMax(state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                            if (!ValidRadFractSched) {
                                ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(3) + "=[" +
                                                cAlphaArgs(3) + "\".");
                                ShowContinueError(state, "..Values outside of range [0.0,0.9).");
                                ErrorsFound = true;
                            }
                        }

                        // added Jan, 2017 - Xuan Luo
                        // read adaptive comfort model and calculate adaptive thermal comfort setpoint
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OperativeTempControl) {
                            if (NumAlphas >= 4 && !lAlphaFieldBlanks(4)) {
                                int adaptiveComfortModelTypeIndex =
                                    UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                if (!adaptiveComfortModelTypeIndex) {
                                    ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                    cAlphaArgs(4) + "\" not found.");
                                    ErrorsFound = true;
                                } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).AdaptiveComfortTempControl = true;
                                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).AdaptiveComfortModelTypeIndex =
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
                        SetupOutputVariable(state, "Zone Thermostat Operative Temperature",
                                            OutputProcessor::Unit::C,
                                            state.dataHeatBal->ZnAirRpt(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum).ThermOperativeTemp,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum).Name);
                    }
                } else {
                    for (Item = 1; Item <= state.dataZoneCtrls->TStatObjects(found).NumOfZones; ++Item) {
                        TempControlledZoneNum = state.dataZoneCtrls->TStatObjects(found).TempControlledZoneStartPtr + Item - 1;
                        if (state.dataZoneCtrls->NumTempControlledZones == 0) continue;
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OperativeTempControl = true;
                        if (UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled")) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled = true;
                        }
                        if (Item == 1) {
                            if ((!(UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled"))) &&
                                (!(UtilityRoutines::SameString(cAlphaArgs(2), "Constant")))) {
                                ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" +
                                                cAlphaArgs(2) + "\".");
                                ErrorsFound = true;
                            }
                        }

                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction = rNumericArgs(1);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched = GetScheduleIndex(state, cAlphaArgs(3));
                        if (Item == 1) {
                            if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched == 0) &&
                                (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled)) { // throw error
                                ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" +
                                                cAlphaArgs(3) + "\" not found.");
                                ErrorsFound = true;
                            }
                        }

                        // check validity of fixed radiative fraction
                        if (Item == 1) {
                            if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction < 0.0) &&
                                (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
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
                            if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction >= 0.9) &&
                                (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
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
                            if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled) {
                                ValidRadFractSched = CheckScheduleValueMinMax(state,
                                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                                if (!ValidRadFractSched) {
                                    ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(3) + "=[" +
                                                    cAlphaArgs(3) + "\".");
                                    ShowContinueError(state, "..Values outside of range [0.0,0.9).");
                                    ErrorsFound = true;
                                }
                            }
                        }

                        // added Jan, 2017 - Xuan Luo
                        // read adaptive comfort model and calculate adaptive thermal comfort setpoint
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OperativeTempControl) {
                            if (NumAlphas >= 4 && !lAlphaFieldBlanks(4)) {
                                int adaptiveComfortModelTypeIndex =
                                    UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                if (!adaptiveComfortModelTypeIndex) {
                                    ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                    cAlphaArgs(4) + "\" not found.");
                                    ErrorsFound = true;
                                } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).AdaptiveComfortTempControl = true;
                                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).AdaptiveComfortModelTypeIndex =
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
                        SetupOutputVariable(state, "Zone Thermostat Operative Temperature",
                                            OutputProcessor::Unit::C,
                                            state.dataHeatBal->ZnAirRpt(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum).ThermOperativeTemp,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ActualZoneNum).Name);
                    } // TStat Objects Loop
                }     // found thermostat referene
            }         // loop over NumOpTempControlledZones
        }             // NumOpTempControlledZones > 0

        // Overcool dehumidificaton GetInput starts here
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::TandHStat));
        state.dataZoneCtrls->NumTempAndHumidityControlledZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataZoneCtrls->NumTempAndHumidityControlledZones > 0) {
            state.dataZoneCtrls->AnyZoneTempAndHumidityControl = true;

            for (TempHumidityCntrlNum = 1; TempHumidityCntrlNum <= state.dataZoneCtrls->NumTempAndHumidityControlledZones; ++TempHumidityCntrlNum) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              TempHumidityCntrlNum,
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
                found = UtilityRoutines::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TStatObjects);
                if (found == 0) {
                    // It might be in the TempControlledZones
                    found = UtilityRoutines::FindItem(cAlphaArgs(1), state.dataZoneCtrls->TempControlledZone);
                    if (found == 0) { // throw error
                        ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) +
                                        " reference not found.");
                        ErrorsFound = true;
                    } else {
                        TempControlledZoneNum = found;
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DehumidifyingSched = cAlphaArgs(2);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                            ErrorsFound = true;
                        }
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = true;
                        if ((UtilityRoutines::SameString(cAlphaArgs(3), "None"))) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = false;
                        }
                        if (UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled")) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled = true;
                        }
                        if ((!(UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled"))) &&
                            (!(UtilityRoutines::SameString(cAlphaArgs(4), "Constant")))) {
                            ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                            "\".");
                            ErrorsFound = true;
                        }

                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange = rNumericArgs(1);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                        if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex == 0) &&
                            (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled)) { // throw error
                            ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                            "\" not found.");
                            ErrorsFound = true;
                        }

                        // check validity of zone Overcool constant range
                        if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange < 0.0) &&
                            (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be negative.",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }
                        if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange > 3.0) &&
                            (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                            ShowSevereError(state,
                                            format("{}={} invalid {}=[{:.2T}\" cannot be > 3.0",
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   cNumericFieldNames(1),
                                                   rNumericArgs(1)));
                            ErrorsFound = true;
                        }

                        // check zone Overcool range schedule min/max values.
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled) {
                            ValidZoneOvercoolRangeSched =
                                CheckScheduleValueMinMax(state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                            if (!ValidZoneOvercoolRangeSched) {
                                ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(5) + "=[" +
                                                cAlphaArgs(5) + "\".");
                                ShowContinueError(state, "..Values outside of range [0.0,3.0].");
                                ErrorsFound = true;
                            }
                        }
                        // check Overcool Control Ratio limits
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio = rNumericArgs(2);
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio < 0.0) {
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
                    for (Item = 1; Item <= state.dataZoneCtrls->TStatObjects(found).NumOfZones; ++Item) {
                        TempControlledZoneNum = state.dataZoneCtrls->TStatObjects(found).TempControlledZoneStartPtr + Item - 1;
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DehumidifyingSched = cAlphaArgs(2);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(state, cAlphaArgs(2));
                        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\" not found.");
                            ErrorsFound = true;
                        }
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = true;
                        if ((UtilityRoutines::SameString(cAlphaArgs(3), "None"))) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControl = false;
                        }
                        if (UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled")) {
                            state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled = false;
                        }
                        if (Item == 1) {
                            if ((!(UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled"))) &&
                                (!(UtilityRoutines::SameString(cAlphaArgs(4), "Constant")))) {
                                ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                cAlphaArgs(4) + "\".");
                                ErrorsFound = true;
                            }
                        }
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange = rNumericArgs(1);
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex = GetScheduleIndex(state, cAlphaArgs(6));
                        if (Item == 1) {
                            if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex == 0) &&
                                (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled)) { // throw error
                                ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" +
                                                cAlphaArgs(5) + "\" not found.");
                                ErrorsFound = true;
                            }
                        }
                        // check validity of zone Overcool constant range
                        if (Item == 1) {
                            if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange < 0.0) &&
                                (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
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
                            if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange > 3.0) &&
                                (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
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
                            if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled) {
                                ValidZoneOvercoolRangeSched = CheckScheduleValueMinMax(state,
                                    state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                                if (!ValidZoneOvercoolRangeSched) {
                                    ShowSevereError(state, cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(5) + "=[" +
                                                    cAlphaArgs(5) + "\".");
                                    ShowContinueError(state, "..Values outside of range [0.0,3.0].");
                                    ErrorsFound = true;
                                }
                            }
                        }
                        state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio = rNumericArgs(2);
                        // check Overcool Control Ratio limits
                        if (Item == 1) {
                            if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio < 0.0) {
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
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::StagedDual));
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
            Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataZoneCtrls->StagedTStatObjects(Item).StageControlledZoneStartPtr = state.dataZoneTempPredictorCorrector->NumStageCtrZone + 1;
                ++state.dataZoneTempPredictorCorrector->NumStageCtrZone;
                state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones = 1;
                state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive = false;
                state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataZoneCtrls->StagedTStatObjects(Item).TempControlledZoneStartPtr = state.dataZoneTempPredictorCorrector->NumStageCtrZone + 1;
                state.dataZoneTempPredictorCorrector->NumStageCtrZone += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataZoneCtrls->StagedTStatObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive = true;
                state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowSevereError(state, "GetStagedDualSetpoint: Errors with invalid names in " + cCurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataZoneTempPredictorCorrector->NumStageCtrZone = 0;
        }

        if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
            state.dataZoneCtrls->StageControlledZone.allocate(state.dataZoneTempPredictorCorrector->NumStageCtrZone);
            state.dataZoneCtrls->StageZoneLogic.dimension(state.dataGlobal->NumOfZones, false);

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
                        cAlphaArgs(2) = state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                    }
                    ZoneAssigned = UtilityRoutines::FindItemInList(
                        cAlphaArgs(2), state.dataZoneCtrls->StageControlledZone, &ZoneStagedControls::ZoneName, StageControlledZoneNum - 1);
                    if (ZoneAssigned == 0) {
                        state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).ZoneName = cAlphaArgs(2);
                        state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
                        if (state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).ActualZoneNum == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" +
                                            cAlphaArgs(2) + "\" not found.");
                            ErrorsFound = true;
                        } else {
                            //           Zone(StageControlledZone(StageControlledZoneNum)%ActualZoneNum)%StageControlledZoneIndex =
                            //           StageControlledZoneNum
                        }
                        state.dataZoneCtrls->StageZoneLogic(state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).ActualZoneNum) = true;
                    } else {
                        state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                        ShowContinueError(state, "...Zone was previously assigned to Thermostat=\"" + state.dataZoneCtrls->StageControlledZone(ZoneAssigned).Name + "\".");
                        ErrorsFound = true;
                        continue;
                    }

                    if (!state.dataZoneCtrls->StagedTStatObjects(Item).ZoneListActive) {
                        state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).Name = cAlphaArgs(1);
                    } else {
                        CheckCreatedZoneItemName(state, RoutineName,
                                                 cCurrentModuleObject,
                                                 state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name,
                                                 state.dataHeatBal->ZoneList(state.dataZoneCtrls->StagedTStatObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                                                 state.dataZoneCtrls->StagedTStatObjects(Item).Name,
                                                 state.dataZoneCtrls->StageControlledZone,
                                                 StageControlledZoneNum - 1,
                                                 state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).Name,
                                                 errFlag);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfHeatStages = rNumericArgs(1);
                    if (rNumericArgs(1) < 1 || rNumericArgs(1) > 4) {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid range {}=\"{:.0R}\"",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(1),
                                               rNumericArgs(1)));
                        ShowContinueError(state, "..contains values outside of range [1,4].");
                        ErrorsFound = true;
                    }

                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).HeatSetBaseSchedName = cAlphaArgs(3);
                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).HSBchedIndex = GetScheduleIndex(state, cAlphaArgs(3));
                    if (Item1 == 1) { // only show error on first of several if zone list
                        if (state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).HSBchedIndex == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                            cAlphaArgs(3) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }

                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).HeatThroRange = rNumericArgs(2);
                    if (rNumericArgs(1) < 0.0) {
                        ShowSevereError(state,
                                        format("{}=\"" + cAlphaArgs(1) + "\" negative value is found at {}=\"{:.1R}\"",
                                               cCurrentModuleObject,
                                               cNumericFieldNames(2),
                                               rNumericArgs(2)));
                        ShowContinueError(state, ".. The minumum value is 0.");
                        ErrorsFound = true;
                    }

                    if (state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfHeatStages > 0) {
                        state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).HeatTOffset.allocate(state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfHeatStages);
                        for (i = 1; i <= state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfHeatStages; ++i) {
                            state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).HeatTOffset(i) = rNumericArgs(2 + i);
                            if (rNumericArgs(2 + i) > 0.0) {
                                ShowSevereError(state,
                                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" positive value is found at " +
                                                    format("{}=\"{:.1R}\"", cNumericFieldNames(2 + i), rNumericArgs(2 + i)));
                                ShowContinueError(state, ".. The maximum value is 0.");
                                ErrorsFound = true;
                            }
                            if (lNumericFieldBlanks(2 + i)) {
                                ShowSevereError(state, cCurrentModuleObject + " object =" + cAlphaArgs(1) + ". The input of " + cNumericFieldNames(2 + i) +
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

                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfCoolStages = rNumericArgs(7);
                    if (rNumericArgs(7) < 1 || rNumericArgs(7) > 4) {
                        ShowSevereError(state,
                                        format("{}=\"{}\" invalid range {}=\"{:.0R}\"",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               cNumericFieldNames(7),
                                               rNumericArgs(7)));
                        ShowContinueError(state, "..contains values outside of range [1,4].");
                        ErrorsFound = true;
                    }

                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).CoolSetBaseSchedName = cAlphaArgs(4);
                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).CSBchedIndex = GetScheduleIndex(state, cAlphaArgs(4));
                    if (Item1 == 1) { // only show error on first of several if zone list
                        if (state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).CSBchedIndex == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" +
                                            cAlphaArgs(4) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }

                    state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).CoolThroRange = rNumericArgs(8);
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

                    if (state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfCoolStages > 0) {
                        state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).CoolTOffset.allocate(state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfCoolStages);
                        for (i = 1; i <= state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).NumOfCoolStages; ++i) {
                            state.dataZoneCtrls->StageControlledZone(StageControlledZoneNum).CoolTOffset(i) = rNumericArgs(8 + i);
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
                                ShowSevereError(state, cCurrentModuleObject + " object =" + cAlphaArgs(1) + ". The input of " + cNumericFieldNames(8 + i) +
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
                ShowContinueError(state,"Model should include one or more of the following objects:  ");
                ShowContinueError(state,"AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, AirLoopHVAC:UnitarySystem, ");
                ShowContinueError(state,
                    "SetpointManager:SingleZone:OneStageCooling, and/or SetpointManager:SingleZone:OneStageHeating. The simulation continues...");
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
        if (FileSystem::fileExists(state.files.inputWeatherFileName.fileName)) {
            // Read hourly dry bulb temperature first
            auto epwFile = state.files.inputWeatherFileName.open(state, "CalcThermalComfortAdaptive");
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
            ShowFatalError(state, "CalcThermalComfortAdaptive: Could not open file " + state.files.inputWeatherFileName.fileName + " for input (read). (File does not exist)");
        }
    }

    void CalculateAdaptiveComfortSetPointSchl(EnergyPlusData &state, Array1D<Real64> const &runningAverageASH, Array1D<Real64> const &runningAverageCEN)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Xuan Luo
        //       DATE WRITTEN   January 2017
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculate the zone operative temperature setpoint using adaptive comfort model.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int const summerDesignDayTypeIndex(9);
        Real64 GrossApproxAvgDryBulbDesignDay(0.0);

        for (size_t i = 1; i <= state.dataWeatherManager->DesDayInput.size(); i++) {
            // Summer design day
            if (state.dataWeatherManager->DesDayInput(i).DayType == summerDesignDayTypeIndex) {
                GrossApproxAvgDryBulbDesignDay = (state.dataWeatherManager->DesDayInput(i).MaxDryBulb + (state.dataWeatherManager->DesDayInput(i).MaxDryBulb - state.dataWeatherManager->DesDayInput(i).DailyDBRange)) / 2.0;
                if (GrossApproxAvgDryBulbDesignDay > 10 && GrossApproxAvgDryBulbDesignDay < 33.5) {
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(1) = 0.31 * GrossApproxAvgDryBulbDesignDay + 17.8;
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(2) = 0.31 * GrossApproxAvgDryBulbDesignDay + 20.3;
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(3) = 0.31 * GrossApproxAvgDryBulbDesignDay + 21.3;
                }
                if (GrossApproxAvgDryBulbDesignDay > 10 && GrossApproxAvgDryBulbDesignDay < 30) {
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(4) = 0.33 * GrossApproxAvgDryBulbDesignDay + 18.8;
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(5) = 0.33 * GrossApproxAvgDryBulbDesignDay + 20.8;
                    ;
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(6) = 0.33 * GrossApproxAvgDryBulbDesignDay + 21.8;
                    ;
                    state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(7) = 0.33 * GrossApproxAvgDryBulbDesignDay + 22.8;
                    ;
                }
            }
        }

        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central.allocate(state.dataWeatherManager->NumDaysInYear);
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90.allocate(state.dataWeatherManager->NumDaysInYear);
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80.allocate(state.dataWeatherManager->NumDaysInYear);
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central.allocate(state.dataWeatherManager->NumDaysInYear);
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I.allocate(state.dataWeatherManager->NumDaysInYear);
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II.allocate(state.dataWeatherManager->NumDaysInYear);
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III.allocate(state.dataWeatherManager->NumDaysInYear);

        // Calculate the set points based on different models, set flag as -1 when running average temperature is not in the range.
        for (int day = 1; day <= state.dataWeatherManager->NumDaysInYear; day++) {
            if (runningAverageASH(day) > 10 && runningAverageASH(day) < 33.5) {
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(day) = 0.31 * runningAverageASH(day) + 17.8;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(day) = 0.31 * runningAverageASH(day) + 20.3;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(day) = 0.31 * runningAverageASH(day) + 21.3;
            } else {
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(day) = -1;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(day) = -1;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(day) = -1;
            }
            if (runningAverageCEN(day) > 10 && runningAverageCEN(day) < 30) {
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(day) = 0.33 * runningAverageCEN(day) + 18.8;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(day) = 0.33 * runningAverageCEN(day) + 20.8;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(day) = 0.33 * runningAverageCEN(day) + 21.8;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(day) = 0.33 * runningAverageCEN(day) + 22.8;
            } else {
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(day) = -1;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(day) = -1;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(day) = -1;
                state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(day) = -1;
            }
        }
        state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.initialized = true;
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
        constexpr auto RoutineName("InitZoneAirSetpoints: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int ZoneNum;
        bool FirstSurfFlag;
        int TRefFlag; // Flag for Reference Temperature process in Zones
        int SurfNum;


        if (state.dataZoneTempPredictorCorrector->InitZoneAirSetPointsOneTimeFlag) {
            state.dataHeatBalFanSys->TempZoneThermostatSetPoint.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->AdapComfortCoolingSetPoint.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneThermostatSetPointHi.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneThermostatSetPointLo.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver.dimension(state.dataGlobal->NumOfZones, 0.0);

            state.dataHeatBalFanSys->LoadCorrectionFactor.dimension(state.dataGlobal->NumOfZones, 0.0); // PH 3/3/04
            state.dataHeatBalFanSys->TempControlType.dimension(state.dataGlobal->NumOfZones, 0);
            if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
                state.dataHeatBalFanSys->ComfortControlType.dimension(state.dataGlobal->NumOfZones, 0);
                state.dataHeatBalFanSys->ZoneComfortControlsFanger.allocate(state.dataGlobal->NumOfZones);
            }
            state.dataZoneTempPredictorCorrector->ZoneSetPointLast.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataZoneEnergyDemand->Setback.dimension(state.dataGlobal->NumOfZones, false);
            state.dataZoneEnergyDemand->DeadBandOrSetback.dimension(state.dataGlobal->NumOfZones, false);
            state.dataZoneEnergyDemand->CurDeadBandOrSetback.dimension(state.dataGlobal->NumOfZones, false);
            state.dataHeatBal->SNLoadHeatEnergy.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->SNLoadCoolEnergy.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->SNLoadHeatRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->SNLoadCoolRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->SNLoadPredictedRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->SNLoadPredictedHSPRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->SNLoadPredictedCSPRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->MoisturePredictedRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->MoisturePredictedHumSPRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBal->MoisturePredictedDehumSPRate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus4.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->DSWZoneTimeMinus1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->DSWZoneTimeMinus2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->DSWZoneTimeMinus3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->DSWZoneTimeMinus4.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneAirHumRatTemp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus1Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus2Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinus3Temp.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->WZoneTimeMinusP.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataZoneTempPredictorCorrector->TempIndZnLd.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataZoneTempPredictorCorrector->TempDepZnLd.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->NonAirSystemResponse.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->SysDepZoneLoads.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->SysDepZoneLoadsLagged.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataZoneTempPredictorCorrector->ZoneAirRelHum.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneWMX.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneWM2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneT1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZoneW1.dimension(state.dataGlobal->NumOfZones, 0.0);

            state.dataHeatBal->ListSNLoadHeatEnergy.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
            state.dataHeatBal->ListSNLoadCoolEnergy.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
            state.dataHeatBal->ListSNLoadHeatRate.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);
            state.dataHeatBal->ListSNLoadCoolRate.dimension(state.dataHeatBal->NumOfZoneLists, 0.0);

            state.dataHeatBal->GroupSNLoadHeatEnergy.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
            state.dataHeatBal->GroupSNLoadCoolEnergy.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
            state.dataHeatBal->GroupSNLoadHeatRate.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
            state.dataHeatBal->GroupSNLoadCoolRate.dimension(state.dataHeatBal->NumOfZoneGroups, 0.0);
            state.dataHeatBalFanSys->AIRRAT.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZTM1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZTM2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->ZTM3.dimension(state.dataGlobal->NumOfZones, 0.0);

            // Hybrid modeling
            state.dataHeatBalFanSys->PreviousMeasuredZT1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->PreviousMeasuredZT2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->PreviousMeasuredZT3.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->PreviousMeasuredHumRat1.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->PreviousMeasuredHumRat2.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHeatBalFanSys->PreviousMeasuredHumRat3.dimension(state.dataGlobal->NumOfZones, 0.0);

            // Allocate Derived Types
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(state.dataGlobal->NumOfZones);
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand.allocate(state.dataGlobal->NumOfZones);

            for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                FirstSurfFlag = true;
                for (SurfNum = state.dataHeatBal->Zone(Loop).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(Loop).HTSurfaceLast; ++SurfNum) {
                    if (FirstSurfFlag) {
                        TRefFlag = state.dataSurface->Surface(SurfNum).TAirRef;
                        FirstSurfFlag = false;
                    }
                    // for each particular zone, the reference air temperature(s) should be the same
                    // (either mean air, bulk air, or supply air temp).
                    if (state.dataSurface->Surface(SurfNum).TAirRef != TRefFlag) {
                        ShowWarningError(state, "Different reference air temperatures for difference surfaces encountered in zone " + state.dataHeatBal->Zone(Loop).Name);
                    }
                }
            }

            // CurrentModuleObject='Zone'
            for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                SetupOutputVariable(state, "Zone Air System Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->SNLoadHeatEnergy(Loop),
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(Loop).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "Heating",
                                    _,
                                    "Building",
                                    state.dataHeatBal->Zone(Loop).Name,
                                    state.dataHeatBal->Zone(Loop).Multiplier,
                                    state.dataHeatBal->Zone(Loop).ListMultiplier);
                SetupOutputVariable(state, "Zone Air System Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->SNLoadCoolEnergy(Loop),
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(Loop).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "Cooling",
                                    _,
                                    "Building",
                                    state.dataHeatBal->Zone(Loop).Name,
                                    state.dataHeatBal->Zone(Loop).Multiplier,
                                    state.dataHeatBal->Zone(Loop).ListMultiplier);
                SetupOutputVariable(state,
                    "Zone Air System Sensible Heating Rate", OutputProcessor::Unit::W, state.dataHeatBal->SNLoadHeatRate(Loop), "System", "Average", state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                    "Zone Air System Sensible Cooling Rate", OutputProcessor::Unit::W, state.dataHeatBal->SNLoadCoolRate(Loop), "System", "Average", state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Air Temperature", OutputProcessor::Unit::C, state.dataHeatBalFanSys->ZT(Loop), "System", "Average", state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                    "Zone Thermostat Air Temperature", OutputProcessor::Unit::C, state.dataHeatBalFanSys->TempTstatAir(Loop), "System", "Average", state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                    "Zone Air Humidity Ratio", OutputProcessor::Unit::None, state.dataHeatBalFanSys->ZoneAirHumRat(Loop), "System", "Average", state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state,
                    "Zone Air Relative Humidity", OutputProcessor::Unit::Perc, state.dataZoneTempPredictorCorrector->ZoneAirRelHum(Loop), "System", "Average", state.dataHeatBal->Zone(Loop).Name);

                // The following output variables are for the predicted Heating/Cooling load for the zone which can be compared to actual load.
                // There are two sets of data available: one where zone and group multipliers have been applied and another where the multipliers have not.
                // First, these report variables are NOT multiplied by zone and group multipliers
                SetupOutputVariable(state, "Zone Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->SNLoadPredictedRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->SNLoadPredictedHSPRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->SNLoadPredictedCSPRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                //Second, these report variable ARE multiplied by zone and group multipliers
                SetupOutputVariable(state, "Zone System Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(Loop).TotalOutputRequired,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone System Predicted Sensible Load to Heating Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(Loop).OutputRequiredToHeatingSP,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone System Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(Loop).OutputRequiredToCoolingSP,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);

                // The following output variables are for the predicted moisture load for the zone with humidity controlled specified.
                // There are two sets of data available: one where zone and group multipliers have been applied and another where the multipliers have not.
                // First, these report variables are NOT multiplied by zone and group multipliers
                SetupOutputVariable(state, "Zone Predicted Moisture Load Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    state.dataHeatBal->MoisturePredictedRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    state.dataHeatBal->MoisturePredictedHumSPRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    state.dataHeatBal->MoisturePredictedDehumSPRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                //Second, these report variable ARE multiplied by zone and group multipliers
                SetupOutputVariable(state, "Zone System Predicted Moisture Load Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(Loop).TotalOutputRequired,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone System Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(Loop).OutputRequiredToHumidifyingSP,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone System Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(Loop).OutputRequiredToDehumidifyingSP,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);

                SetupOutputVariable(state,
                    "Zone Thermostat Control Type", OutputProcessor::Unit::None, state.dataHeatBalFanSys->TempControlType(Loop), "Zone", "Average", state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Thermostat Heating Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Thermostat Cooling Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Adaptive Comfort Operative Temperature Set Point",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(Loop),
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);
                SetupOutputVariable(state, "Zone Predicted Sensible Load Room Air Correction Factor",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBalFanSys->LoadCorrectionFactor(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(Loop).Name);

                if (allocated(state.dataZoneCtrls->StageZoneLogic)) {
                    if (state.dataZoneCtrls->StageZoneLogic(Loop)) {
                        SetupOutputVariable(state, "Zone Thermostat Staged Number",
                                            OutputProcessor::Unit::None,
                                            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(Loop).StageNum,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(Loop).Name);
                    }
                }

            } // Loop

            // Thermal comfort control output
            if (state.dataZoneCtrls->NumComfortControlledZones > 0) {
                // CurrentModuleObject='ZoneControl:Thermostat:ThermalComfort'
                for (Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
                    ZoneNum = state.dataZoneCtrls->ComfortControlledZone(Loop).ActualZoneNum;
                    SetupOutputVariable(state, "Zone Thermal Comfort Control Type",
                                        OutputProcessor::Unit::None,
                                        state.dataHeatBalFanSys->ComfortControlType(ZoneNum),
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNum).Name);
                    SetupOutputVariable(state, "Zone Thermal Comfort Control Fanger Low Setpoint PMV",
                                        OutputProcessor::Unit::None,
                                        state.dataHeatBalFanSys->ZoneComfortControlsFanger(ZoneNum).LowPMV,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNum).Name);
                    SetupOutputVariable(state, "Zone Thermal Comfort Control Fanger High Setpoint PMV",
                                        OutputProcessor::Unit::None,
                                        state.dataHeatBalFanSys->ZoneComfortControlsFanger(ZoneNum).HighPMV,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNum).Name);
                }
            }

            // CurrentModuleObject='ZoneList'
            for (Loop = 1; Loop <= state.dataHeatBal->NumOfZoneLists; ++Loop) {
                SetupOutputVariable(state,
                    "Zone List Sensible Heating Energy", OutputProcessor::Unit::J, state.dataHeatBal->ListSNLoadHeatEnergy(Loop), "System", "Sum", state.dataHeatBal->ZoneList(Loop).Name);
                SetupOutputVariable(state,
                    "Zone List Sensible Cooling Energy", OutputProcessor::Unit::J, state.dataHeatBal->ListSNLoadCoolEnergy(Loop), "System", "Sum", state.dataHeatBal->ZoneList(Loop).Name);
                SetupOutputVariable(state,
                    "Zone List Sensible Heating Rate", OutputProcessor::Unit::W, state.dataHeatBal->ListSNLoadHeatRate(Loop), "System", "Average", state.dataHeatBal->ZoneList(Loop).Name);
                SetupOutputVariable(state,
                    "Zone List Sensible Cooling Rate", OutputProcessor::Unit::W, state.dataHeatBal->ListSNLoadCoolRate(Loop), "System", "Average", state.dataHeatBal->ZoneList(Loop).Name);
            } // Loop

            // CurrentModuleObject='ZoneGroup'
            for (Loop = 1; Loop <= state.dataHeatBal->NumOfZoneGroups; ++Loop) {
                SetupOutputVariable(state, "Zone Group Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->GroupSNLoadHeatEnergy(Loop),
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->ZoneGroup(Loop).Name);
                SetupOutputVariable(state, "Zone Group Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->GroupSNLoadCoolEnergy(Loop),
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->ZoneGroup(Loop).Name);
                SetupOutputVariable(state, "Zone Group Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->GroupSNLoadHeatRate(Loop),
                                    "System",
                                    "Average",
                                    state.dataHeatBal->ZoneGroup(Loop).Name);
                SetupOutputVariable(state, "Zone Group Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->GroupSNLoadCoolRate(Loop),
                                    "System",
                                    "Average",
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
            state.dataHeatBalFanSys->TempZoneThermostatSetPoint = 0.0;
            state.dataHeatBalFanSys->AdapComfortCoolingSetPoint = 0.0;
            state.dataHeatBalFanSys->ZoneThermostatSetPointHi = 0.0;
            state.dataHeatBalFanSys->ZoneThermostatSetPointLo = 0.0;

            state.dataHeatBalFanSys->LoadCorrectionFactor = 1.0; // PH 3/3/04
            state.dataHeatBalFanSys->TempControlType = 0;
            for (auto &e : state.dataZoneEnergyDemand->ZoneSysEnergyDemand) {
                e.RemainingOutputRequired = 0.0;
                e.TotalOutputRequired = 0.0;
            }
            for (auto &e : state.dataZoneEnergyDemand->ZoneSysMoistureDemand) {
                e.RemainingOutputRequired = 0.0;
                e.TotalOutputRequired = 0.0;
            }
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (allocated(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired)) state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired = 0.0;
                if (allocated(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP))
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP = 0.0;
                if (allocated(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP))
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP = 0.0;
                if (allocated(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired)) state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired = 0.0;
                if (allocated(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP))
                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP = 0.0;
                if (allocated(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP))
                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP = 0.0;
            }

            state.dataZoneEnergyDemand->DeadBandOrSetback = false;
            state.dataHeatBal->SNLoadHeatEnergy = 0.0;
            state.dataHeatBal->SNLoadCoolEnergy = 0.0;
            state.dataHeatBal->SNLoadHeatRate = 0.0;
            state.dataHeatBal->SNLoadCoolRate = 0.0;
            state.dataHeatBal->SNLoadPredictedRate = 0.0;
            state.dataHeatBal->SNLoadPredictedHSPRate = 0.0;
            state.dataHeatBal->SNLoadPredictedCSPRate = 0.0;
            state.dataHeatBal->MoisturePredictedRate = 0.0;
            state.dataHeatBal->MoisturePredictedHumSPRate = 0.0;
            state.dataHeatBal->MoisturePredictedDehumSPRate = 0.0;

            state.dataZoneTempPredictorCorrector->TempIndZnLd = 0.0;
            state.dataZoneTempPredictorCorrector->TempDepZnLd = 0.0;
            state.dataHeatBalFanSys->NonAirSystemResponse = 0.0;
            state.dataHeatBalFanSys->SysDepZoneLoads = 0.0;
            state.dataHeatBalFanSys->SysDepZoneLoadsLagged = 0.0;
            state.dataZoneTempPredictorCorrector->ZoneAirRelHum = 0.0;
            for (auto &e : state.dataHeatBal->Zone)
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
                if (!VerifyControlledZoneForThermostat(state, state.dataZoneCtrls->TempControlledZone(Loop).ZoneName)) {
                    ShowSevereError(state, format("{}Zone=\"{}\" has specified a Thermostatic control but is not a controlled zone.", RoutineName, state.dataZoneCtrls->TempControlledZone(Loop).ZoneName));
                    ShowContinueError(state, "...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                    state.dataZoneTempPredictorCorrector->ErrorsFound = true;
                }
            }

            if (state.dataZoneCtrls->TempControlledZone(Loop).ManageDemand) {
                ZoneNum = state.dataZoneCtrls->TempControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        if (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) > state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit) {
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                        }

                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        if (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) < state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit) {
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                        }

                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        if ((state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) > state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit) ||
                            (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) < state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit)) {

                            state.dataHeatBalFanSys->TempControlType(ZoneNum) = DualSetPointWithDeadBand;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);

                            if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) > state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit)
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit;
                            if (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) < state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit)
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit;
                        }

                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) > state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit)
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).HeatingResetLimit;
                        if (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) < state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit)
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).CoolingResetLimit;

                    } else {
                        // Do nothing
                    }
                }
            }
        }

        for (Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
            if (state.dataZoneEquip->ZoneEquipInputsFilled && !state.dataZoneTempPredictorCorrector->ControlledZonesChecked) {
                if (!VerifyControlledZoneForThermostat(state, state.dataZoneCtrls->ComfortControlledZone(Loop).ZoneName)) {
                    ShowSevereError(state, format("{}Zone=\"{}\" has specified a Comfort control but is not a controlled zone.", RoutineName, state.dataZoneCtrls->ComfortControlledZone(Loop).ZoneName));
                    ShowContinueError(state, "...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                    state.dataZoneTempPredictorCorrector->ErrorsFound = true;
                }
            }
            if (state.dataZoneCtrls->ComfortControlledZone(Loop).ManageDemand) {
                ZoneNum = state.dataZoneCtrls->ComfortControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(state.dataHeatBalFanSys->ComfortControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        if (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) >= state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit) {
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            state.dataHeatBalFanSys->TempControlType(ZoneNum) = SingleHeatingSetPoint;
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        if (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) <= state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit) {
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            state.dataHeatBalFanSys->TempControlType(ZoneNum) = SingleCoolingSetPoint;
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        if ((state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) >= state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit) ||
                            (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) <= state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit)) {

                            state.dataHeatBalFanSys->TempControlType(ZoneNum) = DualSetPointWithDeadBand;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);

                            if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) >= state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit)
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit;
                            if (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) <= state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit)
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit;
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        state.dataHeatBalFanSys->TempControlType(ZoneNum) = DualSetPointWithDeadBand;
                        if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) >= state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit)
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).HeatingResetLimit;
                        if (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) <= state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit)
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).CoolingResetLimit;

                    } else {
                        // Do nothing
                    }
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

        // Using/Aliasing
        using DataLoopNode::Node;

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

        // Staged thermostat setpoint
        if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
            for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneTempPredictorCorrector->NumStageCtrZone; ++RelativeZoneNum) {
                ActualZoneNum = state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).ActualZoneNum;
                ZoneT = state.dataHeatBalFanSys->MAT(ActualZoneNum);
                if (ShortenTimeStepSys) ZoneT = state.dataHeatBalFanSys->XMPT(ActualZoneNum);
                state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint = GetCurrentScheduleValue(state, state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HSBchedIndex);
                state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint = GetCurrentScheduleValue(state, state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CSBchedIndex);
                if (state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint >= state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint) {
                    ++state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).StageErrCount;
                    if (state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).StageErrCount < 2) {
                        ShowWarningError(state,
                            "ZoneControl:Thermostat:StagedDualSetpoint: The heating setpoint is equal to or above the cooling setpoint in " +
                            state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).Name);
                        ShowContinueError(state, "The zone heating setpoint is set to the cooling setpoint - 0.1C.");
                        ShowContinueErrorTimeStamp(state, "Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state, "The heating setpoint is still above the cooling setpoint",
                                                       state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).StageErrIndex,
                                                       state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint,
                                                       state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint);
                    }
                    state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint = state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint - 0.1; //???????????
                }
                // Determine either cooling or heating
                if (state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint < ZoneT) { // Cooling
                    SetpointOffset = ZoneT - state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint;
                    Itemp = 0;
                    for (I = 1; I <= state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).NumOfCoolStages; ++I) {
                        if (SetpointOffset >= state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolTOffset(I)) {
                            Itemp = -I;
                        }
                    }
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum = Itemp;
                    if (SetpointOffset >= 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolThroRange) {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) =
                            state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint - 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolThroRange;
                    } else {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) =
                            state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint + 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolThroRange;
                    }
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);
                } else if (state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint > ZoneT) { // heating
                    SetpointOffset = ZoneT - state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint;
                    Itemp = 0;
                    for (I = 1; I <= state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).NumOfHeatStages; ++I) {
                        if (std::abs(SetpointOffset) >= std::abs(state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatTOffset(I))) {
                            Itemp = I;
                        }
                    }
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum = Itemp;
                    if (std::abs(SetpointOffset) >= 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolThroRange) {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) =
                            state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint + 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatThroRange;
                    } else {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) =
                            state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint - 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatThroRange;
                    }
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
                } else {
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) =
                        state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolSetPoint + 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).CoolThroRange;
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) =
                        state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatSetPoint - 0.5 * state.dataZoneCtrls->StageControlledZone(RelativeZoneNum).HeatThroRange;
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).StageNum = 0;
                }
            }
        }

        // Setpoint revision for onoff thermostat
        if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
            Real64 TempTole = 0.02;
            Real64 Tprev;
            for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {
                if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet > 0.0) {
                    if (ShortenTimeStepSys) {
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLast = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLastSave;
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLast = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLastSave;
                    } else {
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLastSave = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLast;
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLastSave = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLast;
                    }
                    ZoneNum = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ActualZoneNum;
                    {
                        auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum));
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolOffFlag = false;
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatOffFlag = false;
                        if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                            Tprev = state.dataHeatBalFanSys->MAT(ZoneNum);
                            if (ShortenTimeStepSys) Tprev = state.dataHeatBalFanSys->XMPT(ZoneNum);
                        } else {
                            Tprev = state.dataHeatBalFanSys->ZoneT1(ZoneNum);
                        }

                        if (SELECT_CASE_var == SingleHeatingSetPoint) {
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                            if (Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempTole) {
                                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            } else if (Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo &&
                                       (Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo +
                                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet - TempTole)) {
                                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            } else {
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                            }
                            if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLast &&
                                Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo) {
                                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                            }
                        } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                            if (Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempTole) {
                                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            } else if (Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi &&
                                       Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi -
                                                   state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet + TempTole) {
                                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                            } else {
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                            }
                            if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLast &&
                                Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi) {
                                state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                            }

                        } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                            if (Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - TempTole) {
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                            } else if (Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi &&
                                       Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi -
                                                   state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet + TempTole) {
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi - state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                            } else {
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                            }
                            if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLast &&
                                Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi) {
                                state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                            }

                            if (Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + TempTole) {
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                            } else if (Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo &&
                                       (Tprev < state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo +
                                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet - TempTole)) {
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) =
                                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo + state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet;
                            } else {
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                            }
                            if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLast &&
                                Tprev > state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo) {
                                state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                                state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                            }
                            // check setpoint for both and provde an error message
                            if (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) >= state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)) {
                                ShowSevereError(state,
                                    "DualSetPointWithDeadBand: When Temperature Difference Between Cutout And Setpoint is applied, the heating "
                                    "setpoint is greater than the cooling setpoint. ");
                                ShowContinueErrorTimeStamp(state, "occurs in Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
                                ShowContinueError(state, format("Zone Heating ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)));
                                ShowContinueError(state, format("Zone Cooling ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)));
                                ShowFatalError(state, "Program terminates due to above conditions.");
                            }
                        }
                    }
                }
            }
        }

        // Update zone temperatures
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

            if (ShortenTimeStepSys) {
                // timestep has just shifted from full zone timestep to a new shorter system timestep
                // throw away last updates in corrector and rewind for resimulating smaller timestep
                if (state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber > 0) { // roll back result for zone air node,
                    Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).Temp = state.dataHeatBalFanSys->XMAT(ZoneNum);
                    state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataHeatBalFanSys->XMAT(ZoneNum);
                    Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).HumRat = state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum);
                    Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).Enthalpy = PsyHFnTdbW(state.dataHeatBalFanSys->XMAT(ZoneNum), state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum));
                }

                if (NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time

                    //  state.dataHeatBalFanSys->MAT(ZoneNum),   state.dataHeatBalFanSys->XMAT(ZoneNum),   state.dataHeatBalFanSys->XM2T(ZoneNum),   state.dataHeatBalFanSys->XM3T(ZoneNum),   state.dataHeatBalFanSys->XM4T(ZoneNum), &
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataHeatBalFanSys->XMAT(ZoneNum),
                                                  state.dataHeatBalFanSys->XM2T(ZoneNum),
                                                  state.dataHeatBalFanSys->XM3T(ZoneNum),
                                                  state.dataHeatBalFanSys->XM4T(ZoneNum),
                                                  state.dataHeatBalFanSys->XM4T(ZoneNum),
                                                  state.dataHeatBalFanSys->MAT(ZoneNum),
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
                        for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                            auto &ThisRAFNNode(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode));
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

            state.dataHeatBalFanSys->AIRRAT(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                              PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) * PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) /
                              (TimeStepSys * DataGlobalConstants::SecInHour);
            AirCap = state.dataHeatBalFanSys->AIRRAT(ZoneNum);
            RAFNFrac = 0.0;

            // Calculate the various heat balance sums

            // NOTE: SumSysMCp and SumSysMCpT are not used in the predict step
            CalcZoneSums(state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, false);

            // Sum all convective internal gains except for people: SumIntGainExceptPeople
            if (HybridModel::FlagHybridModel_PC) {
                SumAllInternalConvectionGainsExceptPeople(state, ZoneNum, SumIntGainExceptPeople);
            }

            TempDepCoef = SumHA + SumMCp;
            TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::Mixing) {
                TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            } else if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
                // UCSD displacement ventilation model - make dynamic term independent of TimeStepSys
                TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            } else if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                // UCSD UFAD model - make dynamic term independent of TimeStepSys
                TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            } else if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                // RoomAirflowNetworkModel - make dynamic term independent of TimeStepSys
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).IsUsed) {
                    RoomAirNode = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
                    LoadPredictionRoomAirModelAirflowNetwork(state, ZoneNum, RoomAirNode);
                    TempDepCoef = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA +
                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp;
                    TempIndCoef = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntSensibleGain +
                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf -
                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref +
                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT +
                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SysDepZoneLoadsLagged;
                    AirCap = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                             state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir *
                             state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).CpAir / (TimeStepSys * DataGlobalConstants::SecInHour);
                    state.dataHeatBalFanSys->AIRRAT(ZoneNum) = AirCap;
                    TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
                    state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                    state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
                    if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HasHVACAssigned)
                        RAFNFrac = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HVAC(1).SupplyFraction;
                }
            } else { // other imperfectly mixed room models
                TempHistoryTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));
                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            }

            // Exact solution or Euler method
            ShortenTimeStepSysRoomAir = false;
            if (state.dataHeatBal->ZoneAirSolutionAlgo != Use3rdOrder) {
                if (ShortenTimeStepSys && TimeStepSys < state.dataGlobal->TimeStepZone) {
                    if (PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                        state.dataHeatBalFanSys->ZoneT1(ZoneNum) = state.dataHeatBalFanSys->ZoneTM2(ZoneNum);
                        state.dataHeatBalFanSys->ZoneW1(ZoneNum) = state.dataHeatBalFanSys->ZoneWM2(ZoneNum);
                        if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                            for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 =
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2;
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 =
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2;
                            }
                        }
                    } else {
                        state.dataHeatBalFanSys->ZoneT1(ZoneNum) = state.dataHeatBalFanSys->ZoneTMX(ZoneNum);
                        state.dataHeatBalFanSys->ZoneW1(ZoneNum) = state.dataHeatBalFanSys->ZoneWMX(ZoneNum);
                        if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                            for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 =
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 =
                                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                            }
                        }
                    }
                    ShortenTimeStepSysRoomAir = true;
                } else {
                    state.dataHeatBalFanSys->ZoneT1(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->ZoneW1(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum);
                    if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                        for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                        }
                    }
                }
                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) = TempDepCoef;
                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum) = TempIndCoef;
            }

            // Calculate the predicted zone load to be provided by the system with the given desired zone air temperature
            CalcPredictedSystemLoad(state, ZoneNum, RAFNFrac);

            // Calculate the predicted zone load to be provided by the system with the given desired humidity ratio
            CalcPredictedHumidityRatio(state, ZoneNum, RAFNFrac);
        }

        if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
            for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {
                if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).DeltaTCutSet > 0.0) {
                    ZoneNum = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ActualZoneNum;
                    if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolOffFlag && state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired >= 0.0) {
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLast = true;
                    } else {
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CoolModeLast = false;
                    }
                    if (state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatOffFlag && state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired <= 0.0) {
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLast = true;
                    } else {
                        state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).HeatModeLast = false;
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
        int SetPointTempSchedIndex;
        int SetPointTempSchedIndexHot;
        int SetPointTempSchedIndexCold;
        int SchedNameIndex;
        int SchedTypeIndex;
        Array2D<Real64> DaySPValues; // Day room temp setpoint values - for optimum start
        int OccStartTime;            // Occupancy start time - for optimum start
        Real64 DeltaT;               // Temperature difference between cutout and setpoint


        state.dataHeatBalFanSys->TempControlType = 0; // Default

        // Place holder for occupied heating and cooling set points - for optimum start
        if (!allocated(state.dataZoneCtrls->OccRoomTSetPointHeat)) {
            state.dataZoneCtrls->OccRoomTSetPointHeat.allocate(state.dataGlobal->NumOfZones);
        }
        if (!allocated(state.dataZoneCtrls->OccRoomTSetPointCool)) {
            state.dataZoneCtrls->OccRoomTSetPointCool.allocate(state.dataGlobal->NumOfZones);
        }
        state.dataZoneCtrls->OccRoomTSetPointHeat = 0.0;
        state.dataZoneCtrls->OccRoomTSetPointCool = 100.0;
        DeltaT = 0.0;

        for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumTempControlledZones; ++RelativeZoneNum) {

            // What if this zone not controlled???
            ActualZoneNum = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ActualZoneNum;
            TempControlSchedIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).CTSchedIndex;
            state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = GetCurrentScheduleValue(state, TempControlSchedIndex);
            // Error detection for these values is done in the Get routine

            {
                auto const SELECT_CASE_var(
                        state.dataHeatBalFanSys->TempControlType(ActualZoneNum)); // Is this missing the possibility of sometimes having no control on a zone
                // during the simulation?
                if (SELECT_CASE_var == 0) { // Uncontrolled

                } else if (SELECT_CASE_var == SingleHeatingSetPoint) {

                    SchedNameIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatSetPoint;

                    SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SchedTypeIndex).TempSchedIndex;
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndex);
                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);

                    AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum));
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    //        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum)

                } else if (SELECT_CASE_var == SingleCoolingSetPoint) {

                    SchedNameIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).SchIndx_SingleCoolSetPoint;

                    SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SchedTypeIndex).TempSchedIndex;
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndex);
                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);

                    // Added Jan 17 (X. Luo)
                    // Adjust operative temperature based on adaptive comfort model
                    if ((state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                        AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum));
                        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    }

                    AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum));
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    //        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum)

                    AdjustCoolingSetPointforTempAndHumidityControl(state, RelativeZoneNum, ActualZoneNum);

                } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {

                    SchedNameIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatCoolSetPoint;

                    SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SchedTypeIndex).TempSchedIndex;
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndex);

                    // Added Jan 17 (X. Luo)
                    // Adjust operative temperature based on adaptive comfort model
                    if ((state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                        AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum));
                        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    }

                    AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum));

                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);

                    // Change the room set point to occupied set point during optimum start period--------------

                    if (allocated(OptStartData.OptStartFlag)) {
                        if (!allocated(DaySPValues)) {
                            DaySPValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
                        }
                        if (OptStartData.ActualZoneNum(ActualZoneNum) == ActualZoneNum) {
                            GetScheduleValuesForDay(state, SetPointTempSchedIndexCold, DaySPValues);
                            OccStartTime = CEILING(OptStartData.OccStartTime(ActualZoneNum)) + 1;
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = DaySPValues(1, OccStartTime);
                        }

                        if (OptStartData.OptStartFlag(ActualZoneNum)) {
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                        }
                    }
                    //--------------------------------------------------------------------------------------------

                } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {

                    SchedNameIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).SchIndx_DualSetPointWDeadBand;

                    SchedTypeIndex = state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndexHot = state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(SchedTypeIndex).HeatTempSchedIndex;
                    SetPointTempSchedIndexCold = state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(SchedTypeIndex).CoolTempSchedIndex;
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndexCold);
                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);

                    // Added Jan 17 (X. Luo)
                    // Adjust operative temperature based on adaptive comfort model
                    if ((state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                        AdjustOperativeSetPointsforAdapComfort(state, RelativeZoneNum, state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum));
                        state.dataHeatBalFanSys->AdapComfortCoolingSetPoint(ActualZoneNum) = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum);
                    }

                    AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum));

                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = GetCurrentScheduleValue(state, SetPointTempSchedIndexHot);
                    state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
                    AdjustAirSetPointsforOpTempCntrl(state, RelativeZoneNum, ActualZoneNum, state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum));

                    // Change the room set point to occupied set point during optimum start period--------------

                    if (allocated(OptStartData.OptStartFlag)) {
                        if (!allocated(DaySPValues)) {
                            DaySPValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
                        }
                        if (OptStartData.ActualZoneNum(ActualZoneNum) == ActualZoneNum) {
                            GetScheduleValuesForDay(state, SetPointTempSchedIndexCold, DaySPValues);
                            OccStartTime = CEILING(OptStartData.OccStartTime(ActualZoneNum)) + 1;
                            state.dataZoneCtrls->OccRoomTSetPointCool(ActualZoneNum) = DaySPValues(1, OccStartTime);
                            GetScheduleValuesForDay(state, SetPointTempSchedIndexHot, DaySPValues);
                            state.dataZoneCtrls->OccRoomTSetPointHeat(ActualZoneNum) = DaySPValues(1, OccStartTime);
                        }

                        if (OptStartData.OptStartFlag(ActualZoneNum)) {
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataZoneCtrls->OccRoomTSetPointCool(ActualZoneNum);
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataZoneCtrls->OccRoomTSetPointHeat(ActualZoneNum);
                        }
                    }
                    //--------------------------------------------------------------------------------------------

                    AdjustCoolingSetPointforTempAndHumidityControl(state, RelativeZoneNum, ActualZoneNum);

                } else {
                    ShowSevereError(state,
                                    format("CalcZoneAirTempSetpoints: Illegal control type for Zone={}, Found value={}, in Schedule={}",
                                           state.dataHeatBal->Zone(ActualZoneNum).Name,
                                           state.dataHeatBalFanSys->TempControlType(ActualZoneNum),
                                           state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).ControlTypeSchedName));
                }
            }

            // Apply offset for faulty therostats
            if ((NumFaultyThermostat > 0) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation)) {

                //  loop through the FaultsThermostatOffset objects to find the one for the zone
                for (int iFault = 1; iFault <= NumFaultyThermostat; ++iFault) {

                    if (UtilityRoutines::SameString(state.dataZoneCtrls->TempControlledZone(RelativeZoneNum).Name, FaultsThermostatOffset(iFault).FaultyThermostatName)) {

                        // Check fault availability schedules
                        if (GetCurrentScheduleValue(state, FaultsThermostatOffset(iFault).AvaiSchedPtr) > 0.0) {

                            // Check fault severity schedules to update the reference thermostat offset
                            double rSchVal = 1.0;
                            double offsetUpdated;
                            if (FaultsThermostatOffset(iFault).SeveritySchedPtr >= 0) {
                                rSchVal = GetCurrentScheduleValue(state, FaultsThermostatOffset(iFault).SeveritySchedPtr);
                            }
                            offsetUpdated = rSchVal * FaultsThermostatOffset(iFault).Offset;

                            // Positive offset means the sensor reading is higher than the actual value
                            state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) -= offsetUpdated;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) -= offsetUpdated;
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) -= offsetUpdated;
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

        // Using/Aliasing
        using DataLoopNode::Node;

        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 LoadToHeatingSetPoint;
        Real64 LoadToCoolingSetPoint;
        Real64 ZoneSetPoint;


        state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = false;
        ZoneSetPoint = 0.0;
        LoadToHeatingSetPoint = 0.0;
        LoadToCoolingSetPoint = 0.0;

        {
            auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum));

            if (SELECT_CASE_var == 0) {
                // Uncontrolled Zone
                LoadToHeatingSetPoint = 0.0;
                LoadToCoolingSetPoint = 0.0;
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;

            } else if (SELECT_CASE_var == SingleHeatingSetPoint) {

                // PH 3/2/04      LoadToHeatingSetPoint = TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum)
                if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToHeatingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) / state.dataHeatBalFanSys->AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    }
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                        state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                ZoneSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                LoadToCoolingSetPoint = LoadToHeatingSetPoint;
                // for consistency with the other cases, use LE instead of LT and don't subtract 1.0 Watt as a way of pushing the zero load
                // case over the threshold
                if ((state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) <= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;

            } else if (SELECT_CASE_var == SingleCoolingSetPoint) {

                // PH 3/2/04      LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
                if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToCoolingSetPoint = state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) / state.dataHeatBalFanSys->AIRRAT(ZoneNum))));
                        LoadToCoolingSetPoint =
                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    }
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                if (state.dataHeatBal->Zone(ZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                    LoadToCoolingSetPoint = state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).AdjustedReturnTempByITE - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                ZoneSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                LoadToHeatingSetPoint = LoadToCoolingSetPoint;
                // for consistency with the other cases, use GE instead of GT and don't add 1.0 Watt as a way of pushing the zero load
                // case over the threshold
                if ((state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) >= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;

            } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {

                // PH 3/2/04      LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
                // PH 3/2/04      !LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
                if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToHeatingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                    LoadToCoolingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) / state.dataHeatBalFanSys->AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint =
                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    }
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }
                ZoneSetPoint = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum);
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                if (RAFNFrac > 0.0) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

                if (state.dataHeatBal->Zone(ZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                    LoadToCoolingSetPoint = state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).AdjustedReturnTempByITE - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }

                // PH 3/2/04      ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint ! = LoadToCoolingSetPoint
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
                    ShowSevereError(state, "SingleHeatCoolSetPoint: Effective heating set-point higher than effective cooling set-point - use "
                                    "DualSetPointWithDeadBand if using unmixed air model");
                    ShowContinueErrorTimeStamp(state, "occurs in Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
                    ShowContinueError(
                        state, format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
                    ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)));
                    ShowFatalError(state, "Program terminates due to above conditions.");
                }

                if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                } else if (LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
                    if (state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                        ZoneSetPoint = Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                        ZoneSetPoint = max(ZoneSetPoint, state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                        ZoneSetPoint = min(ZoneSetPoint, state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
                    }
                    state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
                } else { // this should never occur!
                    ShowSevereError(state,
                        "SingleHeatCoolSetPoint: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
                    ShowContinueErrorTimeStamp(state, "occurs in Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
                    ShowContinueError(
                        state, format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
                    ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)));
                    ShowFatalError(state, "Program terminates due to above conditions.");
                }

            } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {

                if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToHeatingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                    LoadToCoolingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) / state.dataHeatBalFanSys->AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint =
                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    }
                } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                            state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                if (RAFNFrac > 0.0) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

                if (state.dataHeatBal->Zone(ZoneNum).HasAdjustedReturnTempByITE && !(state.dataGlobal->BeginSimFlag)) {
                    LoadToCoolingSetPoint = state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).AdjustedReturnTempByITE - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                }

                // Possible combinations:
                // 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
                // 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
                //                                                                  as a poor choice of set-points
                // 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
                // 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
                // First trap bad set-points
                if (LoadToHeatingSetPoint > LoadToCoolingSetPoint) {
                    ShowSevereError(state, "DualSetPointWithDeadBand: Effective heating set-point higher than effective cooling set-point - increase "
                                    "deadband if using unmixed air model");
                    ShowContinueErrorTimeStamp(state, "occurs in Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
                    ShowContinueError(
                        state, format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
                    ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone Heating ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)));
                    ShowContinueError(state, format("Zone Cooling ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)));
                    ShowFatalError(state, "Program terminates due to above conditions.");
                }

                if (LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0) {
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                    ZoneSetPoint = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum);
                } else if (LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0) {
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                    ZoneSetPoint = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum);
                } else if (LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0) { // deadband includes zero loads
                    // this turns out to cause instabilities sometimes? that lead to setpoint errors if predictor is off.
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
                    if (state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                        ZoneSetPoint = Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                        ZoneSetPoint = max(ZoneSetPoint, state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                        ZoneSetPoint = min(ZoneSetPoint, state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
                    }
                    state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
                } else { // this should never occur!
                    ShowSevereError(state,
                        "DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
                    ShowContinueErrorTimeStamp(state, "occurs in Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
                    ShowContinueError(
                        state, format("LoadToHeatingSetPoint={:.3R}, LoadToCoolingSetPoint={:.3R}", LoadToHeatingSetPoint, LoadToCoolingSetPoint));
                    ShowContinueError(state, format("Zone Heating Set-point={:.2R}", state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)));
                    ShowContinueError(state, format("Zone Cooling Set-point={:.2R}", state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)));
                    ShowContinueError(state, format("Zone TempDepZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone TempIndZnLd={:.2R}", state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum)));
                    ShowContinueError(state, format("Zone ThermostatSetPoint={:.2R}", state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)));

                    ShowFatalError(state, "Program terminates due to above conditions.");
                }
            }
        }

        // Staged control zone
        if (state.dataZoneTempPredictorCorrector->NumStageCtrZone > 0) {
            if (state.dataZoneCtrls->StageZoneLogic(ZoneNum)) {
                if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).StageNum == 0) { // No load
                    LoadToHeatingSetPoint = 0.0;
                    LoadToCoolingSetPoint = 0.0;
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
                    if (state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                        ZoneSetPoint = Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                        ZoneSetPoint = max(ZoneSetPoint, state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                        ZoneSetPoint = min(ZoneSetPoint, state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
                    }
                    state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
                } else if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).StageNum < 0) { // Cooling load
                    if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                        LoadToCoolingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                    } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                        if (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) == 0.0) { // B=0
                            LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        } else {
                            Real64 const exp_700_TA(std::exp(min(700.0, -state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) / state.dataHeatBalFanSys->AIRRAT(ZoneNum))));
                            LoadToCoolingSetPoint =
                                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        }
                    } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                        LoadToCoolingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    }
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                    ZoneSetPoint = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum);
                    LoadToHeatingSetPoint = LoadToCoolingSetPoint;
                    if ((state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) >= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
                } else { // Heating load
                    if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                        LoadToHeatingSetPoint = (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum));
                        // Exact solution
                    } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                        if (state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) == 0.0) { // B=0
                            LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        } else {
                            Real64 const exp_700_TA(std::exp(min(700.0, -state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) / state.dataHeatBalFanSys->AIRRAT(ZoneNum))));
                            LoadToHeatingSetPoint =
                                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                                state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                        }
                    } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                        LoadToHeatingSetPoint = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)) +
                                                state.dataZoneTempPredictorCorrector->TempDepZnLd(ZoneNum) * (state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)) - state.dataZoneTempPredictorCorrector->TempIndZnLd(ZoneNum);
                    }
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                    ZoneSetPoint = state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum);
                    LoadToCoolingSetPoint = LoadToHeatingSetPoint;
                    if ((state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) <= 0.0) state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum) = true;
                }
            }
        }

        // If the ZoneNodeNum has been set for a Controlled Zone, then the zone setpoint is placed on the node.
        if (state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber > 0) {
            Node(state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber).TempSetPoint = ZoneSetPoint;
        }

        if (ZoneSetPoint > state.dataZoneTempPredictorCorrector->ZoneSetPointLast(ZoneNum)) {
            state.dataZoneEnergyDemand->Setback(ZoneNum) = true;
        } else {
            state.dataZoneEnergyDemand->Setback(ZoneNum) = false;
        }

        state.dataZoneTempPredictorCorrector->ZoneSetPointLast(ZoneNum) = ZoneSetPoint;
        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = ZoneSetPoint; // needed to fix Issue # 5048
        state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) = state.dataZoneEnergyDemand->DeadBandOrSetback(ZoneNum);

        // Apply the Zone Multiplier and Load Correction factor as needed
        ReportSensibleLoadsZoneMultiplier(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired,
                                          state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP,
                                          state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP,
                                          state.dataHeatBal->SNLoadPredictedRate(ZoneNum),state.dataHeatBal->SNLoadPredictedHSPRate(ZoneNum),state.dataHeatBal->SNLoadPredictedCSPRate(ZoneNum),
                                          LoadToHeatingSetPoint,LoadToCoolingSetPoint,
                                          state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum),state.dataHeatBal->Zone(ZoneNum).Multiplier,state.dataHeatBal->Zone(ZoneNum).ListMultiplier);

        // init each sequenced demand to the full output
        if (allocated(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired))
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired; // array assignment
        if (allocated(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP))
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP =
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP; // array assignment
        if (allocated(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP))
            state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP =
                state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP; // array assignment
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
                                    Real64 const ZoneMultiplierList
    )
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
        using DataSurfaces::HeatTransferModel_EMPD;
        using DataSurfaces::HeatTransferModel_HAMT;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr auto RoutineName("CalcPredictedHumidityRatio");

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


        LoadToHumidifySetPoint = 0.0;
        LoadToDehumidifySetPoint = 0.0;
        SingleSetPoint = false;
        state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = 0.0;
        state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP = 0.0;
        state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

        // Check to see if this is a "humidity controlled zone"
        ControlledHumidZoneFlag = false;
        // Check all the controlled zones to see if it matches the zone simulated
        for (HumidControlledZoneNum = 1; HumidControlledZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HumidControlledZoneNum) {
            if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ActualZoneNum != ZoneNum) continue;
            ZoneAirRH = PsyRhFnTdbWPb(state, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress) * 100.0;
            ZoneRHHumidifyingSetPoint = GetCurrentScheduleValue(state, state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex);
            ZoneRHDehumidifyingSetPoint = GetCurrentScheduleValue(state, state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex);

            // Apply EMS values to overwrite the humidistat values
            if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).EMSOverrideHumidifySetPointOn) {
                ZoneRHHumidifyingSetPoint = state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).EMSOverrideHumidifySetPointValue;
            }
            if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).EMSOverrideDehumidifySetPointOn) {
                ZoneRHDehumidifyingSetPoint = state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).EMSOverrideDehumidifySetPointValue;
            }

            // Apply offsets for faulty humidistats
            if ((NumFaultyHumidistat > 0) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation)) {

                //  loop through the FaultsHumidistatOffset objects to find the one for the zone
                for (int iFault = 1; iFault <= NumFaultyHumidistat; ++iFault) {

                    if (UtilityRoutines::SameString(state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ControlName,
                                                    FaultsHumidistatOffset(iFault).FaultyHumidistatName)) {

                        if (UtilityRoutines::SameString(FaultsHumidistatOffset(iFault).FaultyHumidistatType, "ThermostatOffsetDependent")) {
                            // For Humidistat Offset Type I: ThermostatOffsetDependent

                            bool IsThermostatFound = false;
                            double offsetThermostat = 0.0;
                            double offsetZoneRHHumidifyingSetPoint = 0.0;
                            double offsetZoneRHDehumidifyingSetPoint = 0.0;
                            double faultZoneWHumidifyingSetPoint;
                            double faultZoneWDehumidifyingSetPoint;

                            // Get the offset value of the corresponding thermostat fault object
                            if (NumFaultyThermostat > 0) {

                                //  loop through the FaultsThermostatOffset objects to find the one causes the Humidistat Offset
                                for (int iFaultThermo = 1; iFaultThermo <= NumFaultyThermostat; ++iFaultThermo) {

                                    if (UtilityRoutines::SameString(FaultsHumidistatOffset(iFault).FaultyThermostatName,
                                                                    FaultsThermostatOffset(iFaultThermo).Name)) {
                                        IsThermostatFound = true;

                                        // Check fault availability schedules
                                        if (GetCurrentScheduleValue(state, FaultsThermostatOffset(iFaultThermo).AvaiSchedPtr) > 0.0) {

                                            // Check fault severity schedules to update the reference thermostat offset
                                            double rSchVal = 1.0;
                                            if (FaultsThermostatOffset(iFaultThermo).SeveritySchedPtr >= 0) {
                                                rSchVal = GetCurrentScheduleValue(state, FaultsThermostatOffset(iFaultThermo).SeveritySchedPtr);
                                            }
                                            offsetThermostat = rSchVal * FaultsThermostatOffset(iFaultThermo).Offset;
                                        }

                                        // Stop searching the FaultsThermostatOffset object for the Humidistat Offset
                                        break;
                                    }
                                }
                            }

                            // The FaultsThermostatOffset specified in the FaultHumidistatOffset is not found
                            if (!IsThermostatFound) {
                                ShowSevereError(state, "FaultModel:HumidistatOffset = \"" + FaultsHumidistatOffset(iFault).Name +
                                                "\" invalid Reference Humidistat Offset Name = \"" +
                                                FaultsHumidistatOffset(iFault).FaultyThermostatName + "\" not found.");
                                ShowFatalError(state, "Errors getting FaultModel input data.  Preceding condition(s) cause termination.");
                            }

                            if (offsetThermostat != 0.0) {
                                // Calculate the humidistat offset value from the thermostat offset value
                                faultZoneWHumidifyingSetPoint =
                                    PsyWFnTdbRhPb(state, (state.dataHeatBalFanSys->MAT(ZoneNum) + offsetThermostat), (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                                faultZoneWDehumidifyingSetPoint =
                                    PsyWFnTdbRhPb(state, (state.dataHeatBalFanSys->MAT(ZoneNum) + offsetThermostat), (ZoneRHDehumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                                offsetZoneRHHumidifyingSetPoint =
                                    ZoneRHHumidifyingSetPoint - PsyRhFnTdbWPb(state, state.dataHeatBalFanSys->MAT(ZoneNum), faultZoneWHumidifyingSetPoint, state.dataEnvrn->OutBaroPress) * 100.0;
                                offsetZoneRHDehumidifyingSetPoint =
                                    ZoneRHDehumidifyingSetPoint - PsyRhFnTdbWPb(state, state.dataHeatBalFanSys->MAT(ZoneNum), faultZoneWDehumidifyingSetPoint, state.dataEnvrn->OutBaroPress) * 100.0;

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
                            if (GetCurrentScheduleValue(state, FaultsHumidistatOffset(iFault).AvaiSchedPtr) > 0.0) {

                                // Check fault severity schedules to update the reference humidistat offset
                                double rSchVal = 1.0;
                                double offsetUpdated;
                                if (FaultsHumidistatOffset(iFault).SeveritySchedPtr >= 0) {
                                    rSchVal = GetCurrentScheduleValue(state, FaultsHumidistatOffset(iFault).SeveritySchedPtr);
                                }
                                offsetUpdated = rSchVal * FaultsHumidistatOffset(iFault).Offset;

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
                if (state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ErrorIndex == 0) {
                    ShowWarningMessage(state, "HUMIDISTAT: The humidifying setpoint is above the dehumidifying setpoint in " +
                                       state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ControlName);
                    ShowContinueError(state, "The zone humidifying setpoint is set to the dehumidifying setpoint.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state, "The humidifying setpoint is still above the dehumidifying setpoint",
                                               state.dataZoneCtrls->HumidityControlZone(HumidControlledZoneNum).ErrorIndex,
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
            LatentGain = state.dataHeatBalFanSys->ZoneLatentGain(ZoneNum) + state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumLatentPool(ZoneNum);

            SysTimeStepInSeconds = DataGlobalConstants::SecInHour * TimeStepSys;

            // Calculate the coefficients for the 3rd Order derivative for final
            // zone humidity ratio.  The A, B, C coefficients are analogous to the heat balance.
            // SumHmARaW and SumHmARa will be used with the Moisture Balance on the building elements and
            // are currently set to zero when the CTF only version is used.

            // if no surface in the zone uses EMPD or HAMT then zero
            bool no_ht_EMPD_or_HAMT(true);
            for (int i = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst, e = state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; i <= e; ++i) {
                auto const &htAlgo(state.dataSurface->Surface(i).HeatTransferAlgorithm);
                if ((htAlgo == HeatTransferModel_EMPD) || (htAlgo == HeatTransferModel_HAMT)) {
                    no_ht_EMPD_or_HAMT = false;
                    break;
                }
            }
            if (no_ht_EMPD_or_HAMT) {
                state.dataHeatBalFanSys->SumHmARaW(ZoneNum) = 0.0;
                state.dataHeatBalFanSys->SumHmARa(ZoneNum) = 0.0;
            }

            // The density of air and latent heat of vaporization are calculated as functions.
            RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), RoutineName);
            H2OHtOfVap = PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataHeatBalFanSys->ZT(ZoneNum));

            // Assume that the system will have flow
            if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
                 AirflowNetwork::AirflowNetworkFanActivated)) {
                // Multizone airflow calculated in AirflowNetwork
                B = (LatentGain / H2OHtOfVap) + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMHrW +
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMHrW + state.dataHeatBalFanSys->SumHmARaW(ZoneNum);
                A = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMHr + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMHr +
                        state.dataHeatBalFanSys->SumHmARa(ZoneNum);
            } else {
                B = (LatentGain / H2OHtOfVap) + ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) + state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) +
                        state.dataHeatBalFanSys->SumHmARaW(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
                A = state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                        state.dataHeatBalFanSys->MDotOA(ZoneNum);
            }
            C = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;

            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                RoomAirNode = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
                H2OHtOfVap = PsyHgAirFnWTdb(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat,
                                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp);
                A = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM + state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa;
                B = (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain / H2OHtOfVap) +
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW + state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW;
                C = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir * state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume *
                    state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist / (DataGlobalConstants::SecInHour * TimeStepSys);
            }

            // Use a 3rd Order derivative to predict zone moisture addition or removal and
            // smooth the changes using the zone air capacitance.  Positive values of Moist Load means that
            // this amount of moisture must be added to the zone to reach the setpoint.  Negative values represent
            // the amount of moisture that must be removed by the system.
            // MoistLoadHumidSetPoint = massflow * HumRat = kgDryAir/s * kgWater/kgDryAir = kgWater/s
            WZoneSetPoint = PsyWFnTdbRhPb(state, state.dataHeatBalFanSys->ZT(ZoneNum), (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress, RoutineName);
            Real64 exp_700_A_C(0.0);
            if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                LoadToHumidifySetPoint = ((11.0 / 6.0) * C + A) * WZoneSetPoint -
                                         (B + C * (3.0 * state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) +
                                                   (1.0 / 3.0) * state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum)));
                // Exact solution
            } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                if (A == 0.0) { // B=0
                    LoadToHumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) - B;
                } else {
                    exp_700_A_C = std::exp(min(700.0, -A / C)); // Tuned Save expensive value
                    LoadToHumidifySetPoint = A * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum) * exp_700_A_C) / (1.0 - exp_700_A_C) - B;
                }
            } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                LoadToHumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) + A * WZoneSetPoint - B;
            }
            if (RAFNFrac > 0.0) LoadToHumidifySetPoint = LoadToHumidifySetPoint / RAFNFrac;
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP = LoadToHumidifySetPoint;
            WZoneSetPoint = PsyWFnTdbRhPb(state, state.dataHeatBalFanSys->ZT(ZoneNum), (ZoneRHDehumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress, RoutineName);
            if (state.dataHeatBal->ZoneAirSolutionAlgo == Use3rdOrder) {
                LoadToDehumidifySetPoint = ((11.0 / 6.0) * C + A) * WZoneSetPoint -
                                           (B + C * (3.0 * state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) +
                                                     (1.0 / 3.0) * state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum)));
                // Exact solution
            } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                if (A == 0.0) { // B=0
                    LoadToDehumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) - B;
                } else {
                    LoadToDehumidifySetPoint = A * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum) * exp_700_A_C) / (1.0 - exp_700_A_C) - B; // exp_700_A_C set above
                }
            } else if (state.dataHeatBal->ZoneAirSolutionAlgo == UseEulerMethod) {
                LoadToDehumidifySetPoint = C * (WZoneSetPoint - state.dataHeatBalFanSys->ZoneW1(ZoneNum)) + A * WZoneSetPoint - B;
            }
            if (RAFNFrac > 0.0) LoadToDehumidifySetPoint = LoadToDehumidifySetPoint / RAFNFrac;
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP = LoadToDehumidifySetPoint;

            // The load is added to the TotalOutputRequired as in the Temperature Predictor.  There is also the remaining
            // output variable for those who will use this for humidity control and stored in DataZoneEnergyDemands with the
            // analogous temperature terms.
            if (SingleSetPoint) {
                state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = LoadToHumidifySetPoint;
            } else {
                if (LoadToHumidifySetPoint > 0.0 && LoadToDehumidifySetPoint > 0.0) {
                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = LoadToHumidifySetPoint;
                    RHSetPoint = ZoneRHHumidifyingSetPoint;
                } else if (LoadToHumidifySetPoint < 0.0 && LoadToDehumidifySetPoint < 0.0) {
                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = LoadToDehumidifySetPoint;
                    RHSetPoint = ZoneRHDehumidifyingSetPoint;
                } else if (LoadToHumidifySetPoint <= 0.0 && LoadToDehumidifySetPoint >= 0.0) { // deadband includes zero loads
                    state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = 0.0;
                } else { // this should never occur!
                    ShowSevereError(state,
                        "Humidistat: Unanticipated combination of humidifying and dehumidifying loads - report to EnergyPlus Development Team");
                    ShowContinueErrorTimeStamp(state, "occurs in Zone=" + state.dataHeatBal->Zone(ZoneNum).Name);
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
        ReportMoistLoadsZoneMultiplier(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired,
                                       state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP,
                                       state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP,
                                       state.dataHeatBal->MoisturePredictedRate(ZoneNum),state.dataHeatBal->MoisturePredictedHumSPRate(ZoneNum),state.dataHeatBal->MoisturePredictedDehumSPRate(ZoneNum),
                                       state.dataHeatBal->Zone(ZoneNum).Multiplier,state.dataHeatBal->Zone(ZoneNum).ListMultiplier);

        // init each sequenced demand to the full output
        if (allocated(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired))
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequired = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired; // array assignment
        if (allocated(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP))
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToHumidSP =
                state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP; // array assignment
        if (allocated(state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP))
            state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).SequencedOutputRequiredToDehumidSP =
                state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP; // array assignment
    }

    void ReportMoistLoadsZoneMultiplier(Real64 &TotalLoad,
                                        Real64 &TotalHumidLoad,
                                        Real64 &TotalDehumidLoad,
                                        Real64 &MoistLoadSingleZone,
                                        Real64 &MoistLoadHumidSingleZone,
                                        Real64 &MoistLoadDehumidSingleZone,
                                        Real64 const ZoneMultiplier,
                                        Real64 const ZoneMultiplierList
    )
    {
        MoistLoadSingleZone = TotalLoad;
        MoistLoadHumidSingleZone = TotalHumidLoad;
        MoistLoadDehumidSingleZone = TotalDehumidLoad;

        Real64 ZoneMultFac = ZoneMultiplier * ZoneMultiplierList;

        TotalLoad *= ZoneMultFac;
        TotalHumidLoad *= ZoneMultFac;
        TotalDehumidLoad *= ZoneMultFac;
    }

    void CorrectZoneAirTemp(EnergyPlusData &state, Real64 &ZoneTempChange, // Temperature change in zone air between previous and current timestep
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

        using DataLoopNode::Node;
        using RoomAirModelManager::ManageAirModel;
        using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr auto RoutineName("CorrectZoneAirTemp");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CpAir;                              // specific heat of air
        static Real64 SumIntGain(0.0);             // Zone sum of convective internal gains
        static Real64 SumIntGainExceptPeople(0.0); // Zone sum of convective internal gains except for convective heat from people, HybridModel
        static Real64 SumHA(0.0);                  // Zone sum of Hc*Area
        static Real64 SumHATsurf(0.0);             // Zone sum of Hc*Area*Tsurf
        static Real64 SumHATref(0.0);              // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
        static Real64 SumMCp(0.0);                 // Zone sum of MassFlowRate*Cp
        static Real64 SumMCpT(0.0);                // Zone sum of MassFlowRate*Cp*T
        static Real64 SumSysMCp(0.0);              // Zone sum of air system MassFlowRate*Cp
        static Real64 SumSysMCpT(0.0);             // Zone sum of air system MassFlowRate*Cp*T
        static Real64 ZoneEnthalpyIn(0.0);         // Zone inlet air enthalpy
        static Real64 TempDepCoef(0.0);            // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
        static Real64 TempIndCoef(0.0);            // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
        static Real64 AirCap(0.0);                 // Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"
        static Real64 SNLoad(0.0);                 // Sensible load calculated for zone in watts and then loaded in report variables
        static int ZoneNum(0);
        static int ZoneNodeNum(0); // System node number for air flow through zone either by system or as a plenum

        Real64 TempSupplyAir;
        Real64 ZoneMult;
        int LoopNode;


        // Initializations
        ZoneTempChange = DataPrecisionGlobals::constant_zero;

        // Update zone temperatures
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

            ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;

            if (ShortenTimeStepSys) {
                // time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
                if (NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataHeatBalFanSys->MAT(ZoneNum),
                                                  state.dataHeatBalFanSys->XMAT(ZoneNum),
                                                  state.dataHeatBalFanSys->XM2T(ZoneNum),
                                                  state.dataHeatBalFanSys->XM3T(ZoneNum),
                                                  state.dataHeatBalFanSys->XM4T(ZoneNum),
                                                  state.dataHeatBalFanSys->MAT(ZoneNum),
                                                  state.dataHeatBalFanSys->DSXMAT(ZoneNum),
                                                  state.dataHeatBalFanSys->DSXM2T(ZoneNum),
                                                  state.dataHeatBalFanSys->DSXM3T(ZoneNum),
                                                  state.dataHeatBalFanSys->DSXM4T(ZoneNum));
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum),
                                                  state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum),
                                                  state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum),
                                                  state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum),
                                                  state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum),
                                                  state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum),
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
                    if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                        for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                            auto &ThisRAFNNode(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode));
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

            state.dataHeatBalFanSys->AIRRAT(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                              PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), RoutineName) *
                              PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);

            AirCap = state.dataHeatBalFanSys->AIRRAT(ZoneNum);

            ManageAirModel(state, ZoneNum);

            // Calculate the various heat balance sums
            CalcZoneSums(state, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);

            // Sum all convective internal gains except for people: SumIntGainExceptPeople
            if (HybridModel::FlagHybridModel_PC) {
                SumAllInternalConvectionGainsExceptPeople(state, ZoneNum, SumIntGainExceptPeople);
            }

            //    ZoneTempHistoryTerm = (3.0D0 * ZTM1(ZoneNum) - (3.0D0/2.0D0) * ZTM2(ZoneNum) + (1.0D0/3.0D0) * ZTM3(ZoneNum))
            ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;

            SNLoad = 0.0;

            if (ZoneNodeNum > 0) { // This zone is controlled by a zone equipment configuration or zone plenum

                // Heat balance coefficients for controlled zone, i.e. with system air flow
                TempDepCoef = SumHA + SumMCp + SumSysMCp;
                TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                              (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
                //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    TempIndCoef += state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).TotalSen;
                }
                //    TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
                //    TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
                // Solve for zone air temperature
                {
                    auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        state.dataHeatBalFanSys->ZT(ZoneNum) = (TempIndCoef + AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum))) /
                                      ((11.0 / 6.0) * AirCap + TempDepCoef);
                        // Exact solution
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            state.dataHeatBalFanSys->ZT(ZoneNum) = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            state.dataHeatBalFanSys->ZT(ZoneNum) = (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                          TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        state.dataHeatBalFanSys->ZT(ZoneNum) = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
                // Update zone node temperature and thermostat temperature unless already updated in Room Air Model,
                // calculate load correction factor
                if ((state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::Mixing) || (!state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel)) {
                    // Fully mixed
                    Node(ZoneNodeNum).Temp = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
                } else if (state.dataRoomAirMod->IsZoneDV(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                    // UCSDDV: Not fully mixed - calculate factor to correct load for fully mixed assumption
                    if (SumSysMCp > SmallMassFlow) {
                        TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
                        if (std::abs(TempSupplyAir - state.dataHeatBalFanSys->ZT(ZoneNum)) > state.dataHeatBal->TempConvergTol) {
                            state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = (TempSupplyAir - Node(ZoneNodeNum).Temp) / (TempSupplyAir - state.dataHeatBalFanSys->ZT(ZoneNum));
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
                } else if (state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel &&
                           ((state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UserDefined) || (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::Mundt))) {
                    if (SumSysMCp > SmallMassFlow) {
                        TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
                        if (std::abs(TempSupplyAir - state.dataHeatBalFanSys->ZT(ZoneNum)) > state.dataHeatBal->TempConvergTol) {
                            state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = (TempSupplyAir - Node(ZoneNodeNum).Temp) / (TempSupplyAir - state.dataHeatBalFanSys->ZT(ZoneNum));
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
                } else if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    // Zone node used in the RoomAirflowNetwork model
                    state.dataHeatBalFanSys->ZT(ZoneNum) = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).AirTemp;
                    Node(ZoneNodeNum).Temp = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
                } else {
                    Node(ZoneNodeNum).Temp = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
                    state.dataHeatBalFanSys->LoadCorrectionFactor(ZoneNum) = 1.0;
                }

                // Sensible load is the enthalpy into the zone minus the enthalpy that leaves the zone.
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                ZoneEnthalpyIn = SumSysMCpT;

                // SNLOAD is the single zone load, without Zone Multiplier or Zone List Multiplier
                SNLoad = ZoneEnthalpyIn - (Node(ZoneNodeNum).MassFlowRate / ZoneMult) * CpAir * Node(ZoneNodeNum).Temp +
                        state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);

            } else {

                // Heat balance coefficients for uncontrolled zone, i.e. without system air flow
                TempDepCoef = SumHA + SumMCp;
                TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT;

                //      TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    TempIndCoef += state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).TotalSen;
                }
                //      TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
                //      TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef

                // Solve for zone air temperature
                {
                    auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        state.dataHeatBalFanSys->ZT(ZoneNum) = (TempIndCoef + AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum))) /
                                      ((11.0 / 6.0) * AirCap + TempDepCoef);
                        // Exact solution
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            state.dataHeatBalFanSys->ZT(ZoneNum) = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            state.dataHeatBalFanSys->ZT(ZoneNum) = (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                          TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        state.dataHeatBalFanSys->ZT(ZoneNum) = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }

                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    state.dataHeatBalFanSys->ZT(ZoneNum) = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).AirTemp;
                }

                // No sensible load
                SNLoad = 0.0;
            }

            // Hybrid modeling start
            if ((HybridModelZone(ZoneNum).InfiltrationCalc_T || HybridModelZone(ZoneNum).InternalThermalMassCalc_T ||
                 HybridModelZone(ZoneNum).PeopleCountCalc_T) &&
                (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
                InverseModelTemperature(
                    state, ZoneNum, SumIntGain, SumIntGainExceptPeople, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, AirCap);
            }

            state.dataHeatBalFanSys->MAT(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);

            // Determine sensible load heating/cooling rate and energy
            state.dataHeatBal->SNLoadHeatRate(ZoneNum) = max(SNLoad, 0.0);
            state.dataHeatBal->SNLoadCoolRate(ZoneNum) = std::abs(min(SNLoad, 0.0));
            state.dataHeatBal->SNLoadHeatEnergy(ZoneNum) = max(SNLoad, 0.0) * TimeStepSys * DataGlobalConstants::SecInHour;
            state.dataHeatBal->SNLoadCoolEnergy(ZoneNum) = std::abs(min(SNLoad, 0.0) * TimeStepSys * DataGlobalConstants::SecInHour);

            // Final humidity calcs
            CorrectZoneHumRat(state, ZoneNum);

            state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
            state.dataZoneTempPredictorCorrector->ZoneAirRelHum(ZoneNum) = 100.0 * PsyRhFnTdbWPb(state, state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress, RoutineName);

            // ZoneTempChange is used by HVACManager to determine if the timestep needs to be shortened.
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
                        if (state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->ZTM1OC(ZoneNum)), std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTM1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)));
                        }
                    } else if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                        if (state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->ZTM1OC(ZoneNum)), std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTM1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)));
                        }
                    } else {
                        ZoneTempChange = max(ZoneTempChange, std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)));
                    }
                } else if ((SELECT_CASE_var == UseAnalyticalSolution) || (SELECT_CASE_var == UseEulerMethod)) {
                    if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
                        if (state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->Zone1OC(ZoneNum)), std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->Zone1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)));
                        }
                    } else if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
                        if (state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->Zone1OC(ZoneNum)), std::abs(state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->Zone1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)));
                        }
                    } else {
                        ZoneTempChange = max(ZoneTempChange, std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum)));
                    }
                }
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

        // Push the temperature and humidity ratio histories

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->XM4T(ZoneNum) = state.dataHeatBalFanSys->XM3T(ZoneNum);
            state.dataHeatBalFanSys->XM3T(ZoneNum) = state.dataHeatBalFanSys->XM2T(ZoneNum);
            state.dataHeatBalFanSys->XM2T(ZoneNum) = state.dataHeatBalFanSys->XMAT(ZoneNum);
            state.dataHeatBalFanSys->XMAT(ZoneNum) = state.dataHeatBalFanSys->ZTAV(ZoneNum); // using average for whole zone time step.
            state.dataHeatBalFanSys->XMPT(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
            //      state.dataHeatBalFanSys->MAT(ZoneNum)  = ZT(ZoneNum)

            state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum); // using average for whole zone time step.
            state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinusP(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
            state.dataZoneTempPredictorCorrector->ZoneAirRelHum(ZoneNum) = 100.0 * PsyRhFnTdbWPb(state, state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress, CorrectZoneAirTemp);

            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV || state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
                state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
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
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX4 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX1;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;

                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX4 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX1;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                }
            }

            if (state.dataHeatBal->ZoneAirSolutionAlgo != Use3rdOrder) {
                state.dataHeatBalFanSys->ZoneTM2(ZoneNum) = state.dataHeatBalFanSys->ZoneTMX(ZoneNum);
                state.dataHeatBalFanSys->ZoneTMX(ZoneNum) = state.dataHeatBalFanSys->ZTAV(ZoneNum); // using average for whole zone time step.
                state.dataHeatBalFanSys->ZoneWM2(ZoneNum) = state.dataHeatBalFanSys->ZoneWMX(ZoneNum);
                state.dataHeatBalFanSys->ZoneWMX(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum); // using average for whole zone time step.
                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV || state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
                        state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
                    state.dataRoomAirMod->ZoneM2Floor(ZoneNum) = state.dataRoomAirMod->ZoneMXFloor(ZoneNum);
                    state.dataRoomAirMod->ZoneMXFloor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum); // using average for whole zone time step.
                    state.dataRoomAirMod->ZoneM2OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                    state.dataRoomAirMod->ZoneMXOC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum); // using average for whole zone time step.
                    state.dataRoomAirMod->ZoneM2MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
                    state.dataRoomAirMod->ZoneMXMX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum); // using average for whole zone time step.
                }

                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                        //                        RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX =
                        // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
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

        // Push the temperature and humidity ratio histories back in time

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->DSXM4T(ZoneNum) = state.dataHeatBalFanSys->DSXM3T(ZoneNum);
            state.dataHeatBalFanSys->DSXM3T(ZoneNum) = state.dataHeatBalFanSys->DSXM2T(ZoneNum);
            state.dataHeatBalFanSys->DSXM2T(ZoneNum) = state.dataHeatBalFanSys->DSXMAT(ZoneNum);
            state.dataHeatBalFanSys->DSXMAT(ZoneNum) = state.dataHeatBalFanSys->MAT(ZoneNum);

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
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX4 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX3;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX3 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX2;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX1;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempDSX1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;

                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX4 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX3;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX3 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX2;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX1;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatDSX1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                }
            }
        } // zone loop

        if (state.dataHeatBal->ZoneAirSolutionAlgo != Use3rdOrder) {
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                state.dataHeatBalFanSys->ZoneTM2(ZoneNum) = state.dataHeatBalFanSys->ZoneTMX(ZoneNum);
                state.dataHeatBalFanSys->ZoneTMX(ZoneNum) = state.dataHeatBalFanSys->MAT(ZoneNum); // using average for whole zone time step.
                state.dataHeatBalFanSys->ZoneWM2(ZoneNum) = state.dataHeatBalFanSys->ZoneWMX(ZoneNum);
                state.dataHeatBalFanSys->ZoneWMX(ZoneNum) = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum); // using average for whole zone time step.

                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV || state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
                        state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
                    state.dataRoomAirMod->ZoneM2Floor(ZoneNum) = state.dataRoomAirMod->ZoneMXFloor(ZoneNum);
                    state.dataRoomAirMod->ZoneMXFloor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum); // using average for whole zone time step.
                    state.dataRoomAirMod->ZoneM2OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                    state.dataRoomAirMod->ZoneMXOC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum); // using average for whole zone time step.
                    state.dataRoomAirMod->ZoneM2MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
                    state.dataRoomAirMod->ZoneMXMX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum); // using average for whole zone time step.
                }
                if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                        //                        RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX =
                        // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
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

        // REvert the temperature and humidity ratio histories

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            //  state.dataHeatBalFanSys->MAT(ZoneNum)  = state.dataHeatBalFanSys->XMAT(ZoneNum)
            state.dataHeatBalFanSys->XMAT(ZoneNum) = state.dataHeatBalFanSys->XM2T(ZoneNum);
            state.dataHeatBalFanSys->XM2T(ZoneNum) = state.dataHeatBalFanSys->XM3T(ZoneNum);
            state.dataHeatBalFanSys->XM3T(ZoneNum) = state.dataHeatBalFanSys->XM4T(ZoneNum);

            //   ZoneAirHumRat(ZoneNum)  = WZoneTimeMinus1(ZoneNum)
            state.dataHeatBalFanSys->WZoneTimeMinus1(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus2(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum);
            state.dataHeatBalFanSys->WZoneTimeMinus3(ZoneNum) = state.dataHeatBalFanSys->WZoneTimeMinus4(ZoneNum);

            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV || state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {

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

            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                for (LoopNode = 1; LoopNode <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX3 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempX4;

                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX1 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX2 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX3 = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatX4;
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
        using DataLoopNode::Node;
        using DataSurfaces::HeatTransferModel_EMPD;
        using DataSurfaces::HeatTransferModel_HAMT;
        using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr auto RoutineName("CorrectZoneHumRat");

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


        MoistureMassFlowRate = 0.0;
        ZoneMassFlowRate = 0.0;
        ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;

        // Check to see if this is a controlled zone
        ControlledZoneAirFlag = state.dataHeatBal->Zone(ZoneNum).IsControlled;

        // Check to see if this is a plenum zone
        ZoneRetPlenumAirFlag = state.dataHeatBal->Zone(ZoneNum).IsReturnPlenum;
        ZoneSupPlenumAirFlag = state.dataHeatBal->Zone(ZoneNum).IsSupplyPlenum;

        if (ControlledZoneAirFlag) { // If there is system flow then calculate the flow rates
            ZoneEquipConfigNum = state.dataHeatBal->Zone(ZoneNum).ZoneEqNum;
            // Calculate moisture flow rate into each zone
            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {

                MoistureMassFlowRate += (Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate *
                                         Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).HumRat) /
                                        ZoneMult;
                ZoneMassFlowRate += Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
            } // NodeNum

            // Do the calculations for the plenum zone
        } else if (ZoneRetPlenumAirFlag) {
            ZoneRetPlenumNum = state.dataHeatBal->Zone(ZoneNum).PlenumCondNum;
            for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {

                MoistureMassFlowRate += (Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                         Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).HumRat) /
                                        ZoneMult;
                ZoneMassFlowRate += Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
            } // NodeNum
            // add in the leak flow
            for (ADUListIndex = 1; ADUListIndex <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                ADUNum = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).UpStreamLeak) {
                    ADUInNode = state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum;
                    MoistureMassFlowRate += (state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk * Node(ADUInNode).HumRat) / ZoneMult;
                    ZoneMassFlowRate += state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk / ZoneMult;
                }
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).DownStreamLeak) {
                    ADUOutNode = state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum;
                    MoistureMassFlowRate += (state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk * Node(ADUOutNode).HumRat) / ZoneMult;
                    ZoneMassFlowRate += state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk / ZoneMult;
                }
            }

        } else if (ZoneSupPlenumAirFlag) {
            ZoneSupPlenumNum = state.dataHeatBal->Zone(ZoneNum).PlenumCondNum;
            MoistureMassFlowRate +=
                (Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate * Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).HumRat) /
                ZoneMult;
            ZoneMassFlowRate += Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate / ZoneMult;
        }

        // Calculate hourly humidity ratio from infiltration + humdidity added from latent load + system added moisture
        LatentGain = state.dataHeatBalFanSys->ZoneLatentGain(ZoneNum) + state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumLatentPool(ZoneNum);

        if (HybridModelZone(ZoneNum).PeopleCountCalc_H) {
            LatentGainExceptPeople = state.dataHeatBalFanSys->ZoneLatentGainExceptPeople(ZoneNum) + state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumLatentPool(ZoneNum);
        }

        SysTimeStepInSeconds = DataGlobalConstants::SecInHour * TimeStepSys;

        // Calculate the coefficients for the 3rd order derivative for final
        // zone humidity ratio.  The A, B, C coefficients are analogous to the
        // heat balance.  There are 2 cases that should be considered, system
        // operating and system shutdown.
        // SumHmARaW and SumHmARa will be used with the moisture balance on the building elements and
        // are currently set to zero to remind us where they need to be in the future
        bool no_ht_EMPD_or_HAMT(true);
        for (int i = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst, e = state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; i <= e; ++i) {
            auto const &htAlgo(state.dataSurface->Surface(i).HeatTransferAlgorithm);
            if ((htAlgo == HeatTransferModel_EMPD) || (htAlgo == HeatTransferModel_HAMT)) {
                no_ht_EMPD_or_HAMT = false;
                break;
            }
        }
        if (no_ht_EMPD_or_HAMT) {
            state.dataHeatBalFanSys->SumHmARaW(ZoneNum) = 0.0;
            state.dataHeatBalFanSys->SumHmARa(ZoneNum) = 0.0;
        }

        RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), RoutineName);
        H2OHtOfVap = PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataHeatBalFanSys->ZT(ZoneNum));

        B = (LatentGain / H2OHtOfVap) + ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) + state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) +
            (MoistureMassFlowRate) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
        A = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                state.dataHeatBalFanSys->MDotOA(ZoneNum);

        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
             AirflowNetwork::AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            B = (LatentGain / H2OHtOfVap) +
                (state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMHrW + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMHrW) +
                (MoistureMassFlowRate) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum);
            A = ZoneMassFlowRate + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMHr +
                state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMHr + state.dataHeatBalFanSys->SumHmARa(ZoneNum);
        }
        C = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;

        if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
            B += state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).TotalLat;
        }

        // Use a 3rd order derivative to predict final zone humidity ratio and
        // smooth the changes using the zone air capacitance.
        {
            auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
            if (SELECT_CASE_var == Use3rdOrder) {
                state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = (B + C * (3.0 * state.dataHeatBalFanSys->WZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->WZoneTimeMinus2Temp(ZoneNum) +
                                                       (1.0 / 3.0) * state.dataHeatBalFanSys->WZoneTimeMinus3Temp(ZoneNum))) /
                                             ((11.0 / 6.0) * C + A);
                // Exact solution
            } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                if (A == 0.0) { // B=0
                    state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = state.dataHeatBalFanSys->ZoneW1(ZoneNum) + B / C;
                } else {
                    state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = (state.dataHeatBalFanSys->ZoneW1(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
                }
            } else if (SELECT_CASE_var == UseEulerMethod) {
                state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = (C * state.dataHeatBalFanSys->ZoneW1(ZoneNum) + B) / (C + A);
            }
        }

        // Set the humidity ratio to zero if the zone has been dried out
        if (state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) < 0.0) state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = 0.0;

        // Check to make sure that is saturated there is condensation in the zone
        // by resetting to saturation conditions.
        WZSat = PsyWFnTdbRhPb(state, state.dataHeatBalFanSys->ZT(ZoneNum), 1.0, state.dataEnvrn->OutBaroPress, RoutineName);

        if (state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) > WZSat) state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = WZSat;

        if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
            state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum) = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).HumRat;
        }

        // HybridModel with measured humidity ratio begins
        if ((HybridModelZone(ZoneNum).InfiltrationCalc_H || HybridModelZone(ZoneNum).PeopleCountCalc_H) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) {
            InverseModelHumidity(state, ZoneNum, LatentGain, LatentGainExceptPeople, ZoneMassFlowRate, MoistureMassFlowRate, H2OHtOfVap, RhoAir);
        }

        // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
        ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
        if (ZoneNodeNum > 0) {
            Node(ZoneNodeNum).HumRat = state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum);
            Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRatTemp(ZoneNum));
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
        Real64 CpAir;                   // specific heat of air
        static Real64 TempDepCoef(0.0); // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
        static Real64 TempIndCoef(0.0); // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
        static Real64 AirCapHM(0.0);    // Air power capacity for hybrid modeling

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

        ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
        state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneMeasuredTemperatureSchedulePtr);

        // HM calculation only HM calculation period start
        if (state.dataEnvrn->DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear && state.dataEnvrn->DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
            Real64 HMMultiplierAverage(1.0);
            Real64 MultpHM(1.0);

            state.dataHeatBalFanSys->ZT(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature; // Array1D<Real64> ZT -- Zone
                                                                 // Air Temperature Averaged over
                                                                 // the System Time Increment
            if (HybridModelZone(ZoneNum).InfiltrationCalc_T && UseZoneTimeStepHistory) {

                constexpr auto RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");

                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirTemperatureSchedulePtr);
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);
                    // Calculate the air humidity ratio at supply air inlet.
                    Real64 CpAirInlet(0.0);
                    CpAirInlet = PsyCpAirFnW(state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio);

                    SumSysMCp_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                    SumSysMCpT_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature;

                    AA = SumSysMCp_HM + SumHA + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) + state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
                    BB = SumSysMCpT_HM + SumIntGain + SumHATsurf - SumHATref + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) + state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
                            state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp +
                         (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
                } else {
                    AA = SumHA + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) + state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
                    BB = SumIntGain + SumHATsurf - SumHATref + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) + state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
                            state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;
                }
                CC = AirCap;
                DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum));

                zone_M_T = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature;
                delta_T = (state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature - state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp);
                CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp, state.dataEnvrn->OutHumRat, RoutineNameInfiltration);
                state.dataHeatBal->Zone(ZoneNum).delta_T = delta_T;

                // s4 - Set ACH to 0 when delta_T <= 0.5, add max and min limits to ach
                if (std::abs(delta_T) <= 0.5) {
                    M_inf = 0.0;
                } else {
                    M_inf = (BB + CC * DD - ((11.0 / 6.0) * CC + AA) * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature) / (CpAir * delta_T);
                }
                ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / state.dataHeatBal->Zone(ZoneNum).Volume * DataGlobalConstants::SecInHour));
                M_inf = (ACH_inf / DataGlobalConstants::SecInHour) * state.dataHeatBal->Zone(ZoneNum).Volume * AirDensity;

                // Overwrite variable with inverse solution
                state.dataHeatBal->Zone(ZoneNum).MCPIHM = M_inf;
                state.dataHeatBal->Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;

            } // Hybrid model infiltration calcualtion end

            // Hybrid modeling internal thermal mass calcualtion start
            if (HybridModelZone(ZoneNum).InternalThermalMassCalc_T && SumSysMCpT == 0 && state.dataHeatBalFanSys->ZT(ZoneNum) != state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) &&
                UseZoneTimeStepHistory) { // HM calculation only when SumSysMCpT =0,
                                          // TimeStepZone (not @ TimeStepSys)
                TempDepCoef = SumHA + SumMCp + SumSysMCp;
                TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                              (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
                //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    TempIndCoef += state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).TotalSen;
                }
                // Calculate air capacity using UseAnalyticalSolution
                if (TempDepCoef == 0.0) {
                    // Is this correct? Shouldn't we use log?? What if ZT(ZoneNum) ==
                    // PreviousMeasuredZT1(ZoneNum)??
                    AirCapHM = TempIndCoef / (state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)); // Inverse equation
                } else {
                    Real64 AirCapHM_temp = 0.0;
                    if (TempIndCoef == TempDepCoef * state.dataHeatBalFanSys->ZT(ZoneNum)) {
                        AirCapHM_temp = 0.0; //  This is the denominator.
                    } else {
                        AirCapHM_temp = (TempIndCoef - TempDepCoef * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)) / (TempIndCoef - TempDepCoef * state.dataHeatBalFanSys->ZT(ZoneNum));
                    }

                    if ((AirCapHM_temp > 0) && (AirCapHM_temp != 1)) {    // Avoide IND
                        AirCapHM = TempDepCoef / std::log(AirCapHM_temp); // Inverse equation
                    } else {
                        AirCapHM = TempIndCoef / (state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum));
                    }
                }

                // Calculate multiplier
                if (std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum)) > 0.05) { // Filter
                    MultpHM = AirCapHM /
                              (state.dataHeatBal->Zone(ZoneNum).Volume * PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->ZT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                               PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum))) *
                              (state.dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour);      // Inverse equation
                    if ((MultpHM < 1.0) || (MultpHM > 30.0)) { // Temperature capacity multiplier greater than
                                                               // 1 and less than 30
                        MultpHM = 1.0;                         // Default value 1.0
                    }
                } else {
                    MultpHM = 1.0; // Default value 1.0
                }

                // For timestep output
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSensHM = MultpHM;

                // Calculate the average multiplier of the zone for the whole running
                // period
                {
                    // count for hybrid model calculations
                    if (MultpHM > 1.0) {
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSensHMSum += MultpHM;
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSensHMCountSum++;
                    }

                    // Calculate and store the multiplier average at the end of HM
                    // simulations
                    if (state.dataEnvrn->DayOfYear == HybridModelZone(ZoneNum).HybridEndDayOfYear && state.dataGlobal->EndDayFlag) {
                        HMMultiplierAverage = state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSensHMSum / state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSensHMCountSum;
                        state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSensHMAverage = HMMultiplierAverage;
                    }
                }
            } // Hybrid model internal thermal mass calcualtion end

            // Hybrid model people count calculation
            if (HybridModelZone(ZoneNum).PeopleCountCalc_T && UseZoneTimeStepHistory) {
                state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneMeasuredTemperatureSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZonePeopleSensibleHeatFraction =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleSensibleFractionSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZonePeopleRadiantHeatFraction =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleRadiationFractionSchedulePtr);

                FractionSensible = state.dataHeatBal->Zone(ZoneNum).ZonePeopleSensibleHeatFraction;
                FractionRadiation = state.dataHeatBal->Zone(ZoneNum).ZonePeopleRadiantHeatFraction;
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
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirTemperatureSchedulePtr);
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                    // Calculate the air humidity ratio at supply air inlet.
                    Real64 CpAirInlet(0.0);
                    CpAirInlet = PsyCpAirFnW(state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio);

                    SumSysMCp_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                    SumSysMCpT_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature;

                    AA = SumSysMCp_HM + SumHA + SumMCp;
                    BB = SumSysMCpT_HM + SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT +
                         (state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum));
                } else {
                    AA = SumHA + SumMCp;
                    BB = SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT;
                }

                CC = AirCap;
                DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) + (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum));

                SumIntGainPeople = ((11.0 / 6.0) * CC + AA) * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredTemperature - BB - CC * DD;
                UpperBound = max(0.0, SumIntGain / (ActivityLevel * FractionSensible * FractionConvection));
                NumPeople = min(UpperBound, max(0.0, SumIntGainPeople / (ActivityLevel * FractionSensible * FractionConvection)));

                if (NumPeople < 0.05) {
                    NumPeople = 0;
                }
                state.dataHeatBal->Zone(ZoneNum).NumOccHM = NumPeople;
            }
        }

        // Update zone temperatures in the previous steps
        state.dataHeatBalFanSys->PreviousMeasuredZT3(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum);
        state.dataHeatBalFanSys->PreviousMeasuredZT2(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum);
        state.dataHeatBalFanSys->PreviousMeasuredZT1(ZoneNum) = state.dataHeatBalFanSys->ZT(ZoneNum);
    }

    void InverseModelHumidity(EnergyPlusData &state, int const ZoneNum,              // Zone number
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
        constexpr auto RoutineName("InverseModelHumidity");

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
        SysTimeStepInSeconds = DataGlobalConstants::SecInHour * TimeStepSys;

        // Get measured zone humidity ratio
        state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneMeasuredHumidityRatioSchedulePtr);

        if (state.dataEnvrn->DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear && state.dataEnvrn->DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
            state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio;

            // Hybrid Model calculate air infiltration rate
            if (HybridModelZone(ZoneNum).InfiltrationCalc_H && UseZoneTimeStepHistory) {
                // Conditionally calculate the time dependent and time independent terms
                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                    SumSysM_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                    SumSysMHumRat_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio;

                    AA = SumSysM_HM + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) +
                            state.dataHeatBalFanSys->MDotOA(ZoneNum);
                    BB = SumSysMHumRat_HM + (LatentGain / H2OHtOfVap) + ((state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) + state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) +
                            state.dataHeatBalFanSys->SumHmARaW(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
                } else {
                    AA = state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                    BB = (LatentGain / H2OHtOfVap) + ((state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) + state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +
                            state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
                }

                CC = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;
                DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) +
                      (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum));

                zone_M_HR = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio;
                delta_HR = (state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio - state.dataEnvrn->OutHumRat);

                CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp, state.dataEnvrn->OutHumRat, RoutineName);

                if (std::abs(state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio - state.dataEnvrn->OutHumRat) < 0.0000001) {
                    M_inf = 0.0;
                } else {
                    M_inf = (CC * DD + BB - ((11.0 / 6.0) * CC + AA) * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio) / delta_HR;
                }

                // Add threshold for air change rate
                ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / state.dataHeatBal->Zone(ZoneNum).Volume * DataGlobalConstants::SecInHour));
                M_inf = (ACH_inf / DataGlobalConstants::SecInHour) * state.dataHeatBal->Zone(ZoneNum).Volume * AirDensity;
                state.dataHeatBal->Zone(ZoneNum).MCPIHM = M_inf;
                state.dataHeatBal->Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;
            }

            // Hybrid Model calculate people count
            if (HybridModelZone(ZoneNum).PeopleCountCalc_H && UseZoneTimeStepHistory) {
                state.dataHeatBal->Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZonePeopleSensibleHeatFraction =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleSensibleFractionSchedulePtr);
                state.dataHeatBal->Zone(ZoneNum).ZonePeopleRadiantHeatFraction =
                    GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZonePeopleRadiationFractionSchedulePtr);

                FractionSensible = state.dataHeatBal->Zone(ZoneNum).ZonePeopleSensibleHeatFraction;

                if (FractionSensible <= 0.0) {
                    FractionSensible = 0.6;
                }

                if (ActivityLevel <= 0.0) {
                    ActivityLevel = 130.0;
                }

                // Conditionally calculate the humidity-dependent and humidity-independent
                // terms.
                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(state, HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                    SumSysM_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                    SumSysMHumRat_HM = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio;

                    AA = SumSysM_HM + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
                            state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                    BB = SumSysMHumRat_HM + (LatentGainExceptPeople / H2OHtOfVap) + ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) +
                            state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
                } else {
                    AA = ZoneMassFlowRate + state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->EAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum) + state.dataHeatBalFanSys->SumHmARa(ZoneNum) +
                            state.dataHeatBalFanSys->MixingMassFlowZone(ZoneNum) + state.dataHeatBalFanSys->MDotOA(ZoneNum);
                    BB = (LatentGainExceptPeople / H2OHtOfVap) + ((state.dataHeatBalFanSys->OAMFL(ZoneNum) + state.dataHeatBalFanSys->VAMFL(ZoneNum) + state.dataHeatBalFanSys->CTMFL(ZoneNum)) * state.dataEnvrn->OutHumRat) +
                            state.dataHeatBalFanSys->EAMFLxHumRat(ZoneNum) + (MoistureMassFlowRate) + state.dataHeatBalFanSys->SumHmARaW(ZoneNum) + state.dataHeatBalFanSys->MixingMassFlowXHumRat(ZoneNum) +
                            state.dataHeatBalFanSys->MDotOA(ZoneNum) * state.dataEnvrn->OutHumRat;
                }

                CC = RhoAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;
                DD = (3.0 * state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) +
                      (1.0 / 3.0) * state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum));

                LatentGainPeople = (((11.0 / 6.0) * CC + AA) * state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio - BB - CC * DD) * H2OHtOfVap;
                UpperBound = max(0.0, LatentGain / (ActivityLevel * (1.0 - FractionSensible)));
                NumPeople = min(UpperBound, max(0.0, LatentGainPeople / (ActivityLevel * (1.0 - FractionSensible))));
                NumPeople = floor(NumPeople * 100.00 + 0.5) / 100.00;
                if (NumPeople < 0.05) {
                    NumPeople = 0;
                }
                state.dataHeatBal->Zone(ZoneNum).NumOccHM = NumPeople;
            }
        }

        // Update zone humidity ratio in the previous steps
        state.dataHeatBalFanSys->PreviousMeasuredHumRat3(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat2(ZoneNum) = state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum);
        state.dataHeatBalFanSys->PreviousMeasuredHumRat1(ZoneNum) = state.dataHeatBal->Zone(ZoneNum).ZoneMeasuredHumidityRatio;
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
                      bool const CorrectorFlag
    )
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
        using DataLoopNode::Node;
        using InternalHeatGains::SumAllInternalConvectionGains;
        using InternalHeatGains::SumAllReturnAirConvectionGains;
        //using ZonePlenum::ZoneRetPlenCond;
        //using ZonePlenum::ZoneSupPlenCond;

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

        // Sum all convective internal gains: SumIntGain
        SumAllInternalConvectionGains(state, ZoneNum, SumIntGain);
        SumIntGain += state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumConvPool(ZoneNum);

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
        // low or zero)
        if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(state, ZoneNum, RetAirGain, 0);
            SumIntGain += RetAirGain;
        }

        // Sum all non-system air flow, i.e. infiltration, simple ventilation, mixing, earth tube: SumMCp, SumMCpT
        SumMCp = state.dataHeatBalFanSys->MCPI(ZoneNum) + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) + state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
        SumMCpT =
                state.dataHeatBalFanSys->MCPTI(ZoneNum) + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) + state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;

        // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
             AirflowNetwork::AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            SumMCp = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCp + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCp;
            SumMCpT = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCpT + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCpT;
        }

        // Sum all system air flow: SumSysMCp, SumSysMCpT
        // Check to see if this is a controlled zone
        ControlledZoneAirFlag = state.dataHeatBal->Zone(ZoneNum).IsControlled;
        if (CorrectorFlag) {
            // Check to see if this is a plenum zone
            ZoneRetPlenumAirFlag = state.dataHeatBal->Zone(ZoneNum).IsReturnPlenum;
            ZoneSupPlenumAirFlag = state.dataHeatBal->Zone(ZoneNum).IsSupplyPlenum;

            // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
            if (ControlledZoneAirFlag) {
                ZoneEquipConfigNum = state.dataHeatBal->Zone(ZoneNum).ZoneEqNum;
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
                ZoneRetPlenumNum = state.dataHeatBal->Zone(ZoneNum).PlenumCondNum;
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
                    if (state.dataDefineEquipment->AirDistUnit(ADUNum).UpStreamLeak) {
                        ADUInNode = state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum;
                        NodeTemp = Node(ADUInNode).Temp;
                        MassFlowRate = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk;
                        CpAir = PsyCpAirFnW(air_hum_rat);
                        Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                        SumSysMCp += MassFlowRate_CpAir;
                        SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
                    }
                    if (state.dataDefineEquipment->AirDistUnit(ADUNum).DownStreamLeak) {
                        ADUOutNode = state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum;
                        NodeTemp = Node(ADUOutNode).Temp;
                        MassFlowRate = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                        CpAir = PsyCpAirFnW(air_hum_rat);
                        Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                        SumSysMCp += MassFlowRate_CpAir;
                        SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
                    }
                }

            } else if (ZoneSupPlenumAirFlag) {
                ZoneSupPlenumNum = state.dataHeatBal->Zone(ZoneNum).PlenumCondNum;
                // Get node conditions
                NodeTemp = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
                MassFlowRate = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));

                SumSysMCp += MassFlowRate * CpAir;
                SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            }

            ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;

            SumSysMCp /= ZoneMult;
            SumSysMCpT /= ZoneMult;
        }
        // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
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
                if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL) SumIntGain += state.dataSurface->SurfWinOtherConvHeatGain(SurfNum);

                // Convective heat gain from natural convection in gap between glass and interior shade or blind
                if (ANY_INTERIOR_SHADE_BLIND(shading_flag)) SumIntGain += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

                // Convective heat gain from airflow window
                if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                    SumIntGain += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
                        SumIntGain += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                        state.dataSurface->SurfWinHeatGain(SurfNum) += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                        state.dataSurface->SurfWinHeatTransfer(SurfNum) += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                        if (state.dataSurface->SurfWinHeatGain(SurfNum) >= 0.0) {
                            state.dataSurface->SurfWinHeatGainRep(SurfNum) = state.dataSurface->SurfWinHeatGain(SurfNum);
                            state.dataSurface->SurfWinHeatGainRepEnergy(SurfNum) = state.dataSurface->SurfWinHeatGainRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                        } else {
                            state.dataSurface->SurfWinHeatLossRep(SurfNum) = - state.dataSurface->SurfWinHeatGain(SurfNum);
                            state.dataSurface->SurfWinHeatLossRepEnergy(SurfNum) = state.dataSurface->SurfWinHeatLossRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                        }
                        state.dataSurface->SurfWinHeatTransferRepEnergy(SurfNum) = state.dataSurface->SurfWinHeatTransfer(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                    }
                }

                // Add to the surface convection sums
                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    Real64 const HA_surf(state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)));
                    SumHATsurf += HA_surf * state.dataSurface->SurfWinFrameTempSurfIn(SurfNum);
                    HA += HA_surf;
                }

                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && !ANY_INTERIOR_SHADE_BLIND(shading_flag)) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    Real64 const HA_surf(state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) * (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)));
                    SumHATsurf += HA_surf * state.dataSurface->SurfWinDividerTempSurfIn(SurfNum);
                    HA += HA_surf;
                }

            } // End of check if window

            HA += state.dataHeatBal->HConvIn(SurfNum) * Area;
            SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum);

            // determine reference air temperature for this surface
            {
                auto const SELECT_CASE_var(state.dataSurface->Surface(SurfNum).TAirRef);
                if (SELECT_CASE_var == ZoneMeanAirTemp) {
                    // The zone air is the reference temperature (which is to be solved for in CorrectZoneAirTemp).
                    RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                    SumHA += HA;
                } else if (SELECT_CASE_var == AdjacentAirTemp) {
                    RefAirTemp = state.dataHeatBal->TempEffBulkAir(SurfNum);
                    SumHATref += HA * RefAirTemp;
                } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                    // check whether this zone is a controlled zone or not
                    if (!ControlledZoneAirFlag) {
                        ShowFatalError(state, "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + state.dataHeatBal->Zone(ZoneNum).Name);
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
                } else {
                    // currently set to mean air temp but should add error warning here
                    RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                    SumHA += HA;
                }
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
        using DataLoopNode::Node;

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
        Real64 Area;       // Effective surface area
        Real64 RefAirTemp; // Reference air temperature for surface convection calculations
        int ADUListIndex;
        int ADUNum;
        int ADUInNode;
        int ADUOutNode;
        Real64 SumSysMCp;
        Real64 SumSysMCpT;
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
        SumSysMCp = 0.0;
        SumSysMCpT = 0.0;
        ADUHeatAddRate = 0.0;
        ADUNum = 0;
        QSensRate = 0;

        // Sum all convective internal gains: SumIntGain
        SumAllInternalConvectionGains(state, ZoneNum, SumIntGains);

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
        // low or zero)
        if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(state, ZoneNum, SumRetAirGains, 0);
            SumIntGains += SumRetAirGains;
        }

        // sum non-system air flow transfers between zones
        SumMCpDTzones = state.dataHeatBalFanSys->MCPTM(ZoneNum) - state.dataHeatBalFanSys->MCPM(ZoneNum) * state.dataHeatBalFanSys->MAT(ZoneNum); // but maybe it should be ZTAV(ZoneNum)

        // Sum non-system air flow, i.e. infiltration, simple ventilation, earth tube
        //  reuse SumMCp, SumMCpT from CalcZoneSum but use MAT (or maybe ZTAV?) to complete
        SumMCpDtInfil = (state.dataHeatBalFanSys->MCPTI(ZoneNum) - state.dataHeatBalFanSys->MCPI(ZoneNum) * state.dataHeatBalFanSys->MAT(ZoneNum)) + (state.dataHeatBalFanSys->MCPTV(ZoneNum) - state.dataHeatBalFanSys->MCPV(ZoneNum) * state.dataHeatBalFanSys->MAT(ZoneNum)) +
                        (state.dataHeatBalFanSys->MCPTE(ZoneNum) - state.dataHeatBalFanSys->MCPE(ZoneNum) * state.dataHeatBalFanSys->MAT(ZoneNum)) + (state.dataHeatBalFanSys->MCPTC(ZoneNum) - state.dataHeatBalFanSys->MCPC(ZoneNum) * state.dataHeatBalFanSys->MAT(ZoneNum)) +
                        (state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp -
                                state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBalFanSys->MAT(ZoneNum)); // infiltration | Ventilation (simple) | Earth tube. | Cooltower | combined OA flow

        // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model (if used)
        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
             AirflowNetwork::AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            SumMCpDtInfil = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCpT -
                            state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCp * state.dataHeatBalFanSys->MAT(ZoneNum);
            SumMCpDTzones = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCpT -
                            state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCp * state.dataHeatBalFanSys->MAT(ZoneNum);
        }

        // Sum all system air flow: reusing how SumSysMCp, SumSysMCpT are calculated in CalcZoneSums

        // Check to see if this is a controlled zone
        ControlledZoneAirFlag = state.dataHeatBal->Zone(ZoneNum).IsControlled;

        // Check to see if this is a plenum zone
        ZoneRetPlenumAirFlag = state.dataHeatBal->Zone(ZoneNum).IsReturnPlenum;
        ZoneSupPlenumAirFlag = state.dataHeatBal->Zone(ZoneNum).IsSupplyPlenum;

        // Plenum and controlled zones have a different set of inlet nodes which must be calculated.
        if (ControlledZoneAirFlag) {
            ZoneEquipConfigNum = state.dataHeatBal->Zone(ZoneNum).ZoneEqNum;
            for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                // Get node conditions
                NodeTemp = Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                MassFlowRate = Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                CalcZoneSensibleOutput(MassFlowRate, NodeTemp, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), QSensRate);
                SumMCpDTsystem += QSensRate;

                ADUNum = state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNodeADUNum(NodeNum);
                if (ADUNum > 0) {
                    NodeTemp = Node(state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum).Temp;
                    MassFlowRate = Node(state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum).MassFlowRate;
                    CalcZoneSensibleOutput(MassFlowRate, NodeTemp, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), ADUHeatAddRate);
                    state.dataDefineEquipment->AirDistUnit(ADUNum).HeatRate = max(0.0, ADUHeatAddRate);
                    state.dataDefineEquipment->AirDistUnit(ADUNum).CoolRate = std::abs(min(0.0, ADUHeatAddRate));
                    state.dataDefineEquipment->AirDistUnit(ADUNum).HeatGain = state.dataDefineEquipment->AirDistUnit(ADUNum).HeatRate * TimeStepSys * DataGlobalConstants::SecInHour;
                    state.dataDefineEquipment->AirDistUnit(ADUNum).CoolGain = state.dataDefineEquipment->AirDistUnit(ADUNum).CoolRate * TimeStepSys * DataGlobalConstants::SecInHour;
                }

            } // NodeNum

        } else if (ZoneRetPlenumAirFlag) {
            ZoneRetPlenumNum = state.dataHeatBal->Zone(ZoneNum).PlenumCondNum;
            for (NodeNum = 1; NodeNum <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {
                // Get node conditions
                NodeTemp = Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).Temp;
                MassFlowRate = Node(state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate;
                CalcZoneSensibleOutput(MassFlowRate, NodeTemp, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), QSensRate);
                SumMCpDTsystem += QSensRate;

            } // NodeNum
            // add in the leaks
            for (ADUListIndex = 1; ADUListIndex <= state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                ADUNum = state.dataZonePlenum->ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).UpStreamLeak) {
                    ADUInNode = state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum;
                    NodeTemp = Node(ADUInNode).Temp;
                    MassFlowRate = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateUpStrLk;
                    CalcZoneSensibleOutput(MassFlowRate, NodeTemp, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), QSensRate);
                    SumMCpDTsystem += QSensRate;
                }
                if (state.dataDefineEquipment->AirDistUnit(ADUNum).DownStreamLeak) {
                    ADUOutNode = state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum;
                    NodeTemp = Node(ADUOutNode).Temp;
                    MassFlowRate = state.dataDefineEquipment->AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                    CalcZoneSensibleOutput(MassFlowRate, NodeTemp, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), QSensRate);
                    SumMCpDTsystem += QSensRate;
                }
            }

        } else if (ZoneSupPlenumAirFlag) {
            ZoneSupPlenumNum = state.dataHeatBal->Zone(ZoneNum).PlenumCondNum;
            // Get node conditions
            NodeTemp = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
            MassFlowRate = Node(state.dataZonePlenum->ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
            CalcZoneSensibleOutput(MassFlowRate, NodeTemp, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), QSensRate);
            SumMCpDTsystem += QSensRate;
        }

        // non air system response.
        SumNonAirSystem = state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) + state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumConvPool(ZoneNum);

        // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {

            Area = state.dataSurface->Surface(SurfNum).Area; // For windows, this is the glazing area
            // determine reference air temperature for this surface's convective heat transfer model
            {
                auto const SELECT_CASE_var(state.dataSurface->Surface(SurfNum).TAirRef);
                if (SELECT_CASE_var == ZoneMeanAirTemp) {
                    // The zone air is the reference temperature
                    RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                } else if (SELECT_CASE_var == AdjacentAirTemp) {
                    RefAirTemp = state.dataHeatBal->TempEffBulkAir(SurfNum);
                } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                    // check whether this zone is a controlled zone or not
                    if (!ControlledZoneAirFlag) {
                        ShowFatalError(state, "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + state.dataHeatBal->Zone(ZoneNum).Name);
                        return;
                    }
                    // determine supply air temperature as a weighted average of the inlet temperatures.
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                        // Get node conditions
                        NodeTemp = Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                        MassFlowRate = Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));

                        SumSysMCp += MassFlowRate * CpAir;
                        SumSysMCpT += MassFlowRate * CpAir * NodeTemp;

                    } // NodeNum
                    if (SumSysMCp > 0.0) {
                        RefAirTemp = SumSysMCpT / SumSysMCp;
                    } else {
                        // no system flow (yet) so just use last value for zone air temp
                        RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                    }

                } else {
                    // currently set to mean air temp but should add error warning here
                    RefAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                }
            }

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
                if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL) SumIntGains += state.dataSurface->SurfWinOtherConvHeatGain(SurfNum);

                // Convective heat gain from natural convection in gap between glass and interior shade or blind
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)))
                    SumIntGains += state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum);

                // Convective heat gain from airflow window
                if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                    SumIntGains += state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum);
                    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
                        SumIntGains += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                    }
                }

                // Add to the surface convection sums
                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution

                    SumHADTsurfs += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) *
                                    (state.dataSurface->SurfWinFrameTempSurfIn(SurfNum) - RefAirTemp);
                }

                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHADTsurfs += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) * (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                    (state.dataSurface->SurfWinDividerTempSurfIn(SurfNum) - RefAirTemp);
                }

            } // End of check if window

            SumHADTsurfs += state.dataHeatBal->HConvIn(SurfNum) * Area * (TempSurfInTmp(SurfNum) - RefAirTemp);

            // Accumulate Zone Phase Change Material Melting/Freezing Enthalpy output variables
            if (state.dataSurface->Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::HeatTransferModel_CondFD) {
                state.dataHeatBal->ZnAirRpt(ZoneNum).SumEnthalpyM += HeatBalFiniteDiffManager::SurfaceFD(SurfNum).EnthalpyM;
                state.dataHeatBal->ZnAirRpt(ZoneNum).SumEnthalpyH += HeatBalFiniteDiffManager::SurfaceFD(SurfNum).EnthalpyF;
            }
        } // SurfNum

        // now calculate air energy storage source term.
        // capacitance is volume * density * heat capacity
        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
        RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));

        {
            auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
            if (SELECT_CASE_var == Use3rdOrder) {
                CzdTdt = RhoAir * CpAir * state.dataHeatBal->Zone(ZoneNum).Volume * state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens * (state.dataHeatBalFanSys->MAT(ZoneNum) - state.dataHeatBalFanSys->ZTM1(ZoneNum)) /
                         (TimeStepSys * DataGlobalConstants::SecInHour);
                // Exact solution
            } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                CzdTdt = TempIndCoef - TempDepCoef * state.dataHeatBalFanSys->MAT(ZoneNum);
            } else if (SELECT_CASE_var == UseEulerMethod) {
                CzdTdt = state.dataHeatBalFanSys->AIRRAT(ZoneNum) * (state.dataHeatBalFanSys->MAT(ZoneNum) - state.dataHeatBalFanSys->ZoneT1(ZoneNum));
            }
        }

        if (state.dataGlobal->DisplayZoneAirHeatBalanceOffBalance) {
            imBalance = SumIntGains + SumHADTsurfs + SumMCpDTzones + SumMCpDtInfil + SumMCpDTsystem + SumNonAirSystem - CzdTdt;

            // throw warning if seriously out of balance (this may need to be removed if too noisy... )
            // formulate dynamic threshold value based on 20% of quadrature sum of components
            Threshold = 0.2 * std::sqrt(pow_2(SumIntGains) + pow_2(SumHADTsurfs) + pow_2(SumMCpDTzones) + pow_2(SumMCpDtInfil) +
                                        pow_2(SumMCpDTsystem) + pow_2(SumNonAirSystem) + pow_2(CzdTdt));
            if ((std::abs(imBalance) > Threshold) && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing)) { // air balance is out by more than threshold
                if (state.dataHeatBal->Zone(ZoneNum).AirHBimBalanceErrIndex == 0) {
                    ShowWarningMessage(state, "Zone Air Heat Balance is out of balance for zone named " + state.dataHeatBal->Zone(ZoneNum).Name);
                    ShowContinueError(state, format("Zone Air Heat Balance Deviation Rate is more than {:.1R} {{W}}", Threshold));
                    if (TurnFansOn) {
                        ShowContinueError(state, "Night cycle fan operation may be causing above error");
                    }

                    ShowContinueErrorTimeStamp(state, " Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(state, "Zone Air Heat Balance is out of balance ... zone named " + state.dataHeatBal->Zone(ZoneNum).Name,
                                               state.dataHeatBal->Zone(ZoneNum).AirHBimBalanceErrIndex,
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

        // first time run allocate arrays and setup output variable
        if (state.dataZoneTempPredictorCorrector->SetupOscillationOutputFlag) {
            state.dataZoneTempPredictorCorrector->ZoneTempHist.allocate(4, state.dataGlobal->NumOfZones);
            state.dataZoneTempPredictorCorrector->ZoneTempHist = 0.0;
            state.dataZoneTempPredictorCorrector->ZoneTempOscillate.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband.dimension(state.dataGlobal->NumOfZones, 0.0);
            // set up zone by zone variables
            // CurrentModuleObject='Zone'
            for (iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                SetupOutputVariable(state,
                    "Zone Oscillating Temperatures Time", OutputProcessor::Unit::hr, state.dataZoneTempPredictorCorrector->ZoneTempOscillate(iZone), "System", "Sum", state.dataHeatBal->Zone(iZone).Name);
                SetupOutputVariable(state,
                    "Zone Oscillating Temperatures During Occupancy Time", OutputProcessor::Unit::hr, state.dataZoneTempPredictorCorrector->ZoneTempOscillateDuringOccupancy(iZone), "System", "Sum", state.dataHeatBal->Zone(iZone).Name);
                SetupOutputVariable(state,
                    "Zone Oscillating Temperatures in Deadband Time", OutputProcessor::Unit::hr, state.dataZoneTempPredictorCorrector->ZoneTempOscillateInDeadband(iZone), "System", "Sum", state.dataHeatBal->Zone(iZone).Name);
            }
            // set up a variable covering all zones
            SetupOutputVariable(state,
                "Facility Any Zone Oscillating Temperatures Time", OutputProcessor::Unit::hr, state.dataZoneTempPredictorCorrector->AnyZoneTempOscillate, "System", "Sum", "Facility");
            SetupOutputVariable(state,
                "Facility Any Zone Oscillating Temperatures During Occupancy Time", OutputProcessor::Unit::hr, state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy, "System", "Sum", "Facility");
            SetupOutputVariable(state,
                "Facility Any Zone Oscillating Temperatures in Deadband Time", OutputProcessor::Unit::hr, state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband, "System", "Sum", "Facility");
            // test if the oscillation variables are even used
            if (ReportingThisVariable(state, "Zone Oscillating Temperatures Time") ||
                ReportingThisVariable(state, "Zone Oscillating Temperatures During Occupancy Time") ||
                ReportingThisVariable(state, "Zone Oscillating Temperatures in Deadband Time") ||
                ReportingThisVariable(state, "Facility Any Zone Oscillating Temperatures Time") ||
                ReportingThisVariable(state, "Facility Any Zone Oscillating Temperatures During Occupancy Time") ||
                ReportingThisVariable(state, "Facility Any Zone Oscillating Temperatures in Deadband Time") ) {
                state.dataZoneTempPredictorCorrector->OscillationVariablesNeeded = true;
            }
            state.dataZoneTempPredictorCorrector->SetupOscillationOutputFlag = false;
        }
        if (state.dataZoneTempPredictorCorrector->OscillationVariablesNeeded) {
            // precalc the negative value for performance
            NegOscillateMagnitude = -OscillateMagnitude;
            // assume no zone is oscillating
            isAnyZoneOscillating = false;
            isAnyZoneOscillatingDuringOccupancy = false;
            isAnyZoneOscillatingInDeadband = false;

            for (iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
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
            state.dataZoneTempPredictorCorrector->AnnualAnyZoneTempOscillateDuringOccupancy += state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateDuringOccupancy;
            state.dataZoneTempPredictorCorrector->AnnualAnyZoneTempOscillateInDeadband += state.dataZoneTempPredictorCorrector->AnyZoneTempOscillateInDeadband;
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

        if (!(state.dataZoneCtrls->AnyOpTempControl)) return; // do nothing to setpoint

        if (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OperativeTempControl)) return; // do nothing to setpoint

        // is operative temp radiative fraction scheduled or fixed?
        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OpTempCntrlModeScheduled) {
            thisMRTFraction = GetCurrentScheduleValue(state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OpTempRadiativeFractionSched);
        } else {
            thisMRTFraction = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).FixedRadiativeFraction;
        }

        // get mean radiant temperature for zone
        thisMRT = state.dataHeatBal->MRT(ActualZoneNum);

        // modify setpoint for operative temperature control
        //  traping for MRT fractions between 0.0 and 0.9 during get input, so shouldn't be able to divide by zero here.
        ZoneAirSetPoint = (ZoneAirSetPoint - thisMRTFraction * thisMRT) / (1.0 - thisMRTFraction);
    }

    void AdjustOperativeSetPointsforAdapComfort(EnergyPlusData& state, int const TempControlledZoneID, Real64 &ZoneAirSetPoint)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Xuan Luo
        //       DATE WRITTEN   Jan 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine adjust the operative setpoints for each controlled adaptive thermal comfort models.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int originZoneAirSetPoint = ZoneAirSetPoint;
        int AdaptiveComfortModelTypeIndex = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).AdaptiveComfortModelTypeIndex;


        // adjust zone operative setpoint
        if (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).AdaptiveComfortTempControl)) return; // do nothing to setpoint
        if ((state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn != DataGlobalConstants::KindOfSim::DesignDay) && (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn != DataGlobalConstants::KindOfSim::HVACSizeDesignDay)) {
            // Adjust run period cooling set point
            switch (AdaptiveComfortModelTypeIndex) {
            case static_cast<int>(AdaptiveComfortModel::ASH55_CENTRAL):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(state.dataEnvrn->DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::ASH55_UPPER_90):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(state.dataEnvrn->DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::ASH55_UPPER_80):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(state.dataEnvrn->DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_CENTRAL):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(state.dataEnvrn->DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_I):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(state.dataEnvrn->DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_II):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(state.dataEnvrn->DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_III):
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(state.dataEnvrn->DayOfYear);
                break;
            default:;
            }
        } else {
            int const envrnDayNum(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum);
            int const summerDesignDayTypeIndex(9);
            // Adjust summer design day set point
            if (state.dataWeatherManager->DesDayInput(envrnDayNum).DayType == summerDesignDayTypeIndex) {
                ZoneAirSetPoint = state.dataZoneTempPredictorCorrector->AdapComfortSetPointSummerDesDay(AdaptiveComfortModelTypeIndex - 1);
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

    void CalcZoneAirComfortSetPoints(EnergyPlusData& state)
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


        // Call thermal comfort module to read zone control comfort object
        if (state.dataZoneTempPredictorCorrector->CalcZoneAirComfortSetPointsFirstTimeFlag) {
            ManageThermalComfort(state, true);
            state.dataZoneTempPredictorCorrector->CalcZoneAirComfortSetPointsFirstTimeFlag = false;
        }

        state.dataHeatBalFanSys->ComfortControlType = 0; // Default

        for (RelativeZoneNum = 1; RelativeZoneNum <= state.dataZoneCtrls->NumComfortControlledZones; ++RelativeZoneNum) {

            ActualZoneNum = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ActualZoneNum;
            ComfortControlSchedIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ComfortSchedIndex;
            state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) = GetCurrentScheduleValue(state, ComfortControlSchedIndex);

            // Get PMV values

            {
                auto const SELECT_CASE_var(
                        state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum)); // Is this missing the possibility of sometimes having no control on a zone
                // during the simulation?
                if (SELECT_CASE_var == 0) { // Uncontrolled for thermal comfort
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV = -999.0;
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV = -999.0;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {

                    SchedNameIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).SchIndx_SglHeatSetPointFanger;
                    SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleHeatingFanger(SchedTypeIndex).PMVSchedIndex;
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::SglHeatSetPointFanger);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV = -999.0;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {

                    SchedNameIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).SchIndx_SglCoolSetPointFanger;
                    SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleCoolingFanger(SchedTypeIndex).PMVSchedIndex;
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::SglCoolSetPointFanger);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV = -999.0;
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {

                    SchedNameIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).SchIndx_SglHCSetPointFanger;
                    SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndex = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCoolFanger(SchedTypeIndex).PMVSchedIndex;
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::SglHCSetPointFanger);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndex);

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {

                    SchedNameIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).SchIndx_DualSetPointFanger;
                    SchedTypeIndex = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndexHot = state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).HeatPMVSchedIndex;
                    SetPointComfortSchedIndexCold = state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).CoolPMVSchedIndex;
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::DualSetPointFanger);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndexHot);
                    state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(state, SetPointComfortSchedIndexCold);
                    if (state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV > state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV) {
                        ++state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrCount;
                        if (state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrCount < 2) {
                            ShowWarningError(state, "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint: The heating PMV setpoint is above the cooling "
                                             "PMV setpoint in " +
                                             state.dataZoneTempPredictorCorrector->SetPointDualHeatCoolFanger(SchedTypeIndex).Name);
                            ShowContinueError(state, "The zone dual heating PMV setpoint is set to the dual cooling PMV setpoint.");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        } else {
                            ShowRecurringWarningErrorAtEnd(state, "The heating PMV setpoint is still above the cooling PMV setpoint",
                                                           state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrIndex,
                                                           state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV,
                                                           state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV);
                        }
                        state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV = state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV;
                    }

                } else {
                    ShowSevereError(
                        state,
                        format("CalcZoneAirTempSetpoints: Illegal thermal control control type for Zone={}, Found value={}, in Schedule={}",
                               state.dataHeatBal->Zone(ActualZoneNum).Name,
                               state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum),
                               state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).ControlTypeSchedName));
                }
            }

            // Check Average method
            {
                auto const SELECT_CASE_var(state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).AverageMethodNum);
                if (SELECT_CASE_var == static_cast<int>(AverageMethod::NO)) {
                    PeopleNum = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).SpecificObjectNum;
                    if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointLo);
                    } else {
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV, SetPointLo);
                    }
                    if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger))
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointHi);
                } else if (SELECT_CASE_var == static_cast<int>(AverageMethod::SPE)) {
                    PeopleNum = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).SpecificObjectNum;
                    if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointLo);
                    } else {
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV, SetPointLo);
                    }
                    if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger))
                        GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointHi);
                } else if (SELECT_CASE_var == static_cast<int>(AverageMethod::OBJ)) {
                    ObjectCount = 0;
                    SetPointLo = 0.0;
                    SetPointHi = 0.0;
                    for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                        if (ActualZoneNum == state.dataHeatBal->People(PeopleNum).ZonePtr) {
                            ++ObjectCount;
                            GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                            SetPointLo += Tset;
                            if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                                SetPointHi += Tset;
                            }
                        }
                    }
                    SetPointLo /= ObjectCount;
                    if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) SetPointHi /= ObjectCount;
                } else if (SELECT_CASE_var == static_cast<int>(AverageMethod::PEO)) {
                    PeopleCount = 0.0;
                    SetPointLo = 0.0;
                    SetPointHi = 0.0;
                    for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                        if (ActualZoneNum == state.dataHeatBal->People(PeopleNum).ZonePtr) {
                            NumberOccupants = state.dataHeatBal->People(PeopleNum).NumberOfPeople * GetCurrentScheduleValue(state, state.dataHeatBal->People(PeopleNum).NumberOfPeoplePtr);
                            PeopleCount += NumberOccupants;
                            GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                            SetPointLo += Tset * NumberOccupants;
                            if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                                SetPointHi += Tset * NumberOccupants;
                            }
                        }
                    }
                    if (PeopleCount > 0) {
                        SetPointLo /= PeopleCount;
                        if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) SetPointHi /= PeopleCount;
                    } else {
                        // recurring warnings
                        //          ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount = &
                        //                                           ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount + 1
                        if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).PeopleAverageErrIndex == 0) {
                            ShowWarningMessage(state, "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " +
                                               state.dataHeatBal->Zone(ActualZoneNum).Name + " is zero. The People Average option is not used.");
                            ShowContinueError(state, "The Object Average option is used instead. Simulation continues .....");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state, "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " +
                                                           state.dataHeatBal->Zone(ActualZoneNum).Name + " is still zero. The People Average option is not used",
                                                       state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).PeopleAverageErrIndex,
                                                       PeopleCount,
                                                       PeopleCount);
                        ObjectCount = 0;
                        SetPointLo = 0.0;
                        SetPointHi = 0.0;
                        for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                            if (ActualZoneNum == state.dataHeatBal->People(PeopleNum).ZonePtr) {
                                ++ObjectCount;
                                GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                                SetPointLo += Tset;
                                if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                                    GetComfortSetPoints(state, PeopleNum, RelativeZoneNum, state.dataHeatBalFanSys->ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                                    SetPointHi += Tset;
                                }
                            }
                        }
                        SetPointLo /= ObjectCount;
                        if (state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) SetPointHi /= ObjectCount;
                    }
                }
            }

            // Assign setpoint
            {
                auto const SELECT_CASE_var(
                        state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum)); // Is this missing the possibility of sometimes having no control on a zone
                // during the simulation?
                if (SELECT_CASE_var == 0) { // Uncontrolled for thermal comfort
                    {
                        auto const SELECT_CASE_var1(state.dataHeatBalFanSys->TempControlType(ActualZoneNum));
                        if (SELECT_CASE_var1 == SingleHeatingSetPoint) {
                            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = 0.0;
                        } else if (SELECT_CASE_var1 == SingleCoolingSetPoint) {
                            state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = 0.0;
                        }
                    }

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                    if (SetPointLo < state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                        SetPointLo = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount + 1
                        if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinErrIndex < 2) {
                            ShowWarningMessage(state, "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is below the Minimum dry-bulb "
                                               "temperature setpoint " +
                                               state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError(state, "The zone heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state, "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is still below the "
                                                       "Minimum dry-bulb temperature setpoint ...",
                                                       state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = SingleHeatingSetPoint;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {

                    if (SetPointLo > state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                        SetPointLo = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount + 1
                        if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxErrIndex == 0) {
                            ShowWarningMessage(state, "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is above the Maximum dry-bulb "
                                               "temperature setpoint " +
                                               state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError(state, "The zone cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state, "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is still above the "
                                                       "Maximum dry-bulb temperature setpoint ...",
                                                       state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = SingleCoolingSetPoint;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {

                    if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint == state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                        SetPointLo = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                    }
                    if (SetPointLo > state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint)
                        SetPointLo = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                    if (SetPointLo < state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint)
                        SetPointLo = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                    if (SetPointLo < state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint ||
                        SetPointLo > state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                        //          ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount = ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount + 1
                        if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbHCErrIndex == 0) {
                            ShowWarningMessage(state, "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is above the Maximum or "
                                               "below the Minimum dry-bulb temperature setpoint " +
                                               state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError(state, "The zone setpoint is set to the Maximum dry-bulb temperature setpoint if above or the Minimum "
                                              "dry-bulb temperature setpoint if below");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state, "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is still beyond "
                                                       "the range between Maximum and Minimum dry-bulb temperature setpoint ...",
                                                       state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbHCErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ActualZoneNum);
                    state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = SingleHeatCoolSetPoint;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {

                    if (SetPointLo < state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                        SetPointLo = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount =
                        //          ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount+1
                        if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbDualMinErrIndex == 0) {
                            ShowWarningMessage(state, "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is below the Minimum dry-bulb "
                                               "temperature setpoint " +
                                               state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError(state, "The zone dual heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state, "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still below the Minimum "
                                                       "dry-bulb temperature setpoint ...",
                                                       state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbDualMinErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    if (SetPointHi > state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                        SetPointHi = state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount =
                        //          ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount + 1
                        if (state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbDualMaxErrIndex == 0) {
                            ShowWarningMessage(state, "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is above the Maximum dry-bulb "
                                               "temperature setpoint " +
                                               state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError(state, "The zone dual cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp(state, "Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd(state, "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still above the Maximum "
                                                       "dry-bulb temperature setpoint ...",
                                                       state.dataZoneCtrls->ComfortControlledZone(RelativeZoneNum).TdbDualMaxErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }

                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) = SetPointLo;
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) = SetPointHi;
                    state.dataHeatBalFanSys->TempControlType(ActualZoneNum) = DualSetPointWithDeadBand;

                } else {
                    ShowSevereError(
                        state,
                        format("CalcZoneAirComfortSetpoints: Illegal thermal control control type for Zone={}, Found value={}, in Schedule={}",
                               state.dataHeatBal->Zone(ActualZoneNum).Name,
                               state.dataHeatBalFanSys->ComfortControlType(ActualZoneNum),
                               state.dataZoneCtrls->ComfortControlledZone(ActualZoneNum).ControlTypeSchedName));
                }
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
        Real64 const Acc(0.001); // accuracy control for SolveRoot
        int const MaxIter(500);  // iteration control for SolveRoot

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Tmin;            // Minimun drybulb setpoint temperature
        Real64 Tmax;            // Maximun drybulb setpoint temperature
        Real64 PMVResult;       // Calculated PMV value
        Real64 PMVMin;          // Minimum allowed PMV value
        Real64 PMVMax;          // Calculated PMV value
        Array1D<Real64> Par(2); // Passed parameter for RegularFalsi function
        int SolFla;             // feed back flag from SolveRoot
        static int IterLimitExceededNum1(0);
        static int IterLimitErrIndex1(0);
        static int IterLimitExceededNum2(0);
        static int IterLimitErrIndex2(0);

        Tmin = state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum).TdbMinSetPoint;
        Tmax = state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum).TdbMaxSetPoint;

        CalcThermalComfortFanger(state, PeopleNum, Tmin, PMVResult);
        PMVMin = PMVResult;
        CalcThermalComfortFanger(state, PeopleNum, Tmax, PMVResult);
        PMVMax = PMVResult;
        if (PMVSet > PMVMin && PMVSet < PMVMax) {
            Par(1) = PMVSet;
            Par(2) = double(PeopleNum);
            TempSolveRoot::SolveRoot(state, Acc, MaxIter, SolFla, Tset, PMVResidual, Tmin, Tmax, Par);
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    ++IterLimitExceededNum1;
                    if (IterLimitExceededNum1 == 1) {
                        ShowWarningError(state, state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum).Name +
                                         ": Iteration limit exceeded calculating thermal comfort Fanger setpoint and non-converged setpoint is used");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state, state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum).Name +
                                                           ":  Iteration limit exceeded calculating thermal comfort setpoint.",
                                                       IterLimitErrIndex1,
                                                       Tset,
                                                       Tset);
                    }
                }
            } else if (SolFla == -2) {
                if (!state.dataGlobal->WarmupFlag) {
                    ++IterLimitExceededNum2;
                    if (IterLimitExceededNum2 == 1) {
                        ShowWarningError(state, state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum).Name +
                                         ": Solution is not found in calculating thermal comfort Fanger setpoint and the minimum setpoint is used");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state, state.dataZoneCtrls->ComfortControlledZone(ComfortControlNum).Name +
                                                           ":  Solution is not found in  calculating thermal comfort Fanger setpoint.",
                                                       IterLimitErrIndex2,
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

        if (!(state.dataZoneCtrls->AnyZoneTempAndHumidityControl)) return; // do nothing to setpoint

        if (!(state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).ZoneOvercoolControl)) return; // do nothing to setpoint

        if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OvercoolCntrlModeScheduled) {
            ZoneOvercoolRange = GetCurrentScheduleValue(state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).ZoneOvercoolRangeSchedIndex);
        } else {
            ZoneOvercoolRange = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).ZoneOvercoolConstRange;
        }
        ZoneOvercoolControlRatio = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).ZoneOvercoolControlRatio;

        // For Dual Setpoint thermostat the overcool range is limited by the temperature difference between cooling
        // and heating setpoints
        MaxAllowedOvercoolRange = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) - state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum);
        if (MaxAllowedOvercoolRange > 0.0) {
            ZoneOvercoolRange = min(ZoneOvercoolRange, MaxAllowedOvercoolRange);
        }
        // Calculate difference between zone air relative humidity and the dehumidifying setpoint
        RelativeHumidityDiff =
            state.dataZoneTempPredictorCorrector->ZoneAirRelHum(ActualZoneNum) - GetCurrentScheduleValue(state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).DehumidifyingSchedIndex);
        if (RelativeHumidityDiff > 0.0 && ZoneOvercoolControlRatio > 0.0) {
            // proportionally reset the cooling setpoint temperature downward (zone Overcool)
            ZoneOvercoolRange = min(ZoneOvercoolRange, RelativeHumidityDiff / ZoneOvercoolControlRatio);
            state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) -= ZoneOvercoolRange;
        }
    }

    void OverrideAirSetPointsforEMSCntrl(EnergyPlusData& state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         L. Gu
        //       DATE WRITTEN   June 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine overrides the air temperature setpoint based on EMS

        for (int Loop = 1; Loop <= state.dataZoneCtrls->NumTempControlledZones; ++Loop) {
            if (state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideHeatingSetPointOn) {
                int ZoneNum = state.dataZoneCtrls->TempControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        // do nothing
                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
            if (state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideCoolingSetPointOn) {
                int ZoneNum = state.dataZoneCtrls->TempControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        // do nothing
                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
        }

        for (int Loop = 1; Loop <= state.dataZoneCtrls->NumComfortControlledZones; ++Loop) {
            if (state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideHeatingSetPointOn) {
                int ZoneNum = state.dataZoneCtrls->ComfortControlledZone(Loop).ActualZoneNum;
                {
                    auto const SELECT_CASE_var(state.dataHeatBalFanSys->ComfortControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        // do nothing
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
            if (state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideCoolingSetPointOn) {
                int ZoneNum = state.dataZoneCtrls->ComfortControlledZone(Loop).ActualZoneNum;
                {
                    auto const SELECT_CASE_var(state.dataHeatBalFanSys->ComfortControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        // do nothing
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) = state.dataZoneCtrls->ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
        }
    }

    // add values to the LEED tabular report related to schedules used by the thermostat objects
    void FillPredefinedTableOnThermostatSetpoints(EnergyPlusData& state)
    {
        // J.Glazer - Aug 2017
        using namespace OutputReportPredefined;
        std::vector<int> uniqSch;
        uniqSch.reserve(state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls + state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls + state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls +
                        state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls * 2);
        Real64 setPointAt11;
        Real64 setPointAt23;
        int numDays;
        std::string monthAssumed;
        std::string monthAssumed2;
        const int wednesday = 4;

        for (int SingleTempHeatingControlNum = 1; SingleTempHeatingControlNum <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatingControls; ++SingleTempHeatingControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex, false, wednesday, 11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, setPointAt11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex, false, wednesday, 23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, setPointAt23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, numDays);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, state.dataZoneTempPredictorCorrector->SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, monthAssumed);
            }
        }
        for (int SingleTempCoolingControlNum = 1; SingleTempCoolingControlNum <= state.dataZoneTempPredictorCorrector->NumSingleTempCoolingControls; ++SingleTempCoolingControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex, true, wednesday, 11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, setPointAt11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex, true, wednesday, 23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, setPointAt23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, numDays);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, state.dataZoneTempPredictorCorrector->SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, monthAssumed);
            }
        }
        for (int SingleTempHeatCoolControlNum = 1; SingleTempHeatCoolControlNum <= state.dataZoneTempPredictorCorrector->NumSingleTempHeatCoolControls; ++SingleTempHeatCoolControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).Name);

                std::string schNm = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName + " (summer)";
                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, true, wednesday, 11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, schNm, setPointAt11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, schNm, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, true, wednesday, 23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, schNm, setPointAt23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, schNm, numDays);

                schNm = state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName + " (winter)";
                std::tie(setPointAt11, numDays, monthAssumed2) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, false, wednesday, 11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, schNm, setPointAt11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, schNm, numDays);

                std::tie(setPointAt23, numDays, monthAssumed2) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, false, wednesday, 23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, schNm, setPointAt23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, schNm, numDays);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed,
                                 state.dataZoneTempPredictorCorrector->SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName,
                                 monthAssumed + " and " + monthAssumed2);
            }
        }
        for (int DualTempHeatCoolControlNum = 1; DualTempHeatCoolControlNum <= state.dataZoneTempPredictorCorrector->NumDualTempHeatCoolControls; ++DualTempHeatCoolControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                                 state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName,
                                 state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex, false, wednesday, 11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, setPointAt11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex, false, wednesday, 23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, setPointAt23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, numDays);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, monthAssumed);
            }
            if (std::find(uniqSch.begin(), uniqSch.end(), state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtFirstObjUsed,
                                 state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName,
                                 state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex, true, wednesday, 11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWednesday, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, setPointAt11);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11amWedCnt, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(state, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex, true, wednesday, 23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWednesday, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, setPointAt23);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedSchStPt11pmWedCnt, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, numDays);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdChLeedSchStPtMonthUsed, state.dataZoneTempPredictorCorrector->SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, monthAssumed);
            }
        }
    }

    // returns the temperature value from a schedule at a certain time for the first day of the week in either January or July
    std::tuple<Real64, int, std::string>
    temperatureAndCountInSch(EnergyPlusData& state, int const &scheduleIndex, bool const &isSummer, int const &dayOfWeek, int const &hourOfDay)
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
        int const firstTimeStep = 1;
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

} // namespace ZoneTempPredictorCorrector
