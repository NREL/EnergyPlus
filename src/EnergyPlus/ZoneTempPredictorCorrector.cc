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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>

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
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/IOFiles.hh>
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

namespace EnergyPlus {

namespace ZoneTempPredictorCorrector {

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
    using namespace DataPrecisionGlobals;
    using namespace DataGlobals;
    using namespace DataHVACGlobals;
    using namespace DataHeatBalance;
    using namespace DataHeatBalFanSys;
    using DataEnvironment::OutBaroPress;
    using DataEnvironment::OutHumRat;
    using DataZoneEnergyDemands::CurDeadBandOrSetback;
    using DataZoneEnergyDemands::DeadBandOrSetback;
    using DataZoneEnergyDemands::Setback;
    using DataZoneEnergyDemands::ZoneSysEnergyDemand;
    using DataZoneEnergyDemands::ZoneSysMoistureDemand;
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

        if (GetZoneAirStatsInputFlag) {
            GetZoneAirSetPoints(state.dataZoneTempPredictorCorrector, state.files);
            GetZoneAirStatsInputFlag = false;
        }

        InitZoneAirSetPoints(state.dataZoneTempPredictorCorrector);

        {
            auto const SELECT_CASE_var(UpdateType);

            if (SELECT_CASE_var == iGetZoneSetPoints) {
                CalcZoneAirTempSetPoints(state.dataZoneTempPredictorCorrector, state.files);

            } else if (SELECT_CASE_var == iPredictStep) {
                PredictSystemLoads(state, state.dataZoneTempPredictorCorrector, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            } else if (SELECT_CASE_var == iCorrectStep) {
                CorrectZoneAirTemp(state, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            } else if (SELECT_CASE_var == iRevertZoneTimestepHistories) {
                RevertZoneTimestepHistories();

            } else if (SELECT_CASE_var == iPushZoneTimestepHistories) {
                PushZoneTimestepHistories(state.dataZoneTempPredictorCorrector);

            } else if (SELECT_CASE_var == iPushSystemTimestepHistories) {
                PushSystemTimestepHistories();
            }
        }
    }

    void GetZoneAirSetPoints(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, IOFiles &ioFiles)
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using ScheduleManager::CheckScheduleValue;
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;
        using WeatherManager::NumDaysInYear;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneAirSetpoints: ");

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

        // FLOW:
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::TStat));
        NumTStatStatements = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        TStatObjects.allocate(NumTStatStatements);

        // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
        NumTempControlledZones = 0;
        for (Item = 1; Item <= NumTStatStatements; ++Item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            TStatObjects(Item).Name = cAlphaArgs(1);
            Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            ZLItem = 0;
            if (Item1 == 0 && NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowSevereError("GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects.");
            ShowContinueError("...These will not be read in.  Other errors may occur.");
            NumTempControlledZones = 0;
        }

        if (NumTempControlledZones > 0) {
            TempControlledZone.allocate(NumTempControlledZones);
            TStatControlTypes.allocate(NumTempControlledZones); // Number of set point types
            CTSchedMapToControlledZone.dimension(NumTempControlledZones, 0);

            TempControlledZoneNum = 0;
            dataZoneTempPredictorCorrector.NumOnOffCtrZone = 0;
            for (Item = 1; Item <= NumTStatStatements; ++Item) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
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
                    ZoneAssigned =
                        UtilityRoutines::FindItemInList(cAlphaArgs(2), TempControlledZone, &ZoneTempControls::ZoneName, TempControlledZoneNum - 1);
                    if (ZoneAssigned == 0) {
                        TempControlledZone(TempControlledZoneNum).ZoneName = cAlphaArgs(2);
                        TempControlledZone(TempControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                        if (TempControlledZone(TempControlledZoneNum).ActualZoneNum == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" +
                                            cAlphaArgs(2) + "\" not found.");
                            ErrorsFound = true;
                        } else {
                            Zone(TempControlledZone(TempControlledZoneNum).ActualZoneNum).TempControlledZoneIndex = TempControlledZoneNum;
                        }
                    } else {
                        TempControlledZone(TempControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                        ShowContinueError("...Zone was previously assigned to Thermostat=\"" + TempControlledZone(ZoneAssigned).Name + "\".");
                        ErrorsFound = true;
                        continue;
                    }

                    if (!TStatObjects(Item).ZoneListActive) {
                        TempControlledZone(TempControlledZoneNum).Name = cAlphaArgs(1);
                    } else {
                        CheckCreatedZoneItemName(RoutineName,
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
                    TempControlledZone(TempControlledZoneNum).CTSchedIndex = GetScheduleIndex(cAlphaArgs(3));
                    if (Item1 == 1) { // only show error on first of several if zone list
                        if (TempControlledZone(TempControlledZoneNum).CTSchedIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                            cAlphaArgs(3) + "\" not found.");
                            ErrorsFound = true;
                        } else {
                            // Check validity of control types.
                            ValidScheduleControlType =
                                CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum).CTSchedIndex, ">=", 0.0, "<=", 4.0);
                            if (!ValidScheduleControlType) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(2) + "=\"" +
                                                cAlphaArgs(2) + "\"");
                                ShowContinueError("..contains values outside of range [0,4].");
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
                    TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx.allocate(TempControlledZone(TempControlledZoneNum).NumControlTypes);

                    for (ControlTypeNum = 1; ControlTypeNum <= TempControlledZone(TempControlledZoneNum).NumControlTypes; ++ControlTypeNum) {

                        TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3));
                        TempControlledZone(TempControlledZoneNum).ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 3));

                        if (TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum) != "") {
                            CTIndex = UtilityRoutines::FindItem(
                                TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum), ValidControlTypes, 4);
                            if (CTIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 3)) + "\"");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                            cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 3)) + "=\"<blank>\"");
                            ErrorsFound = true;
                        }
                        TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(ControlTypeNum) = 0;
                    }
                    if (NumNums > 0) {
                        if (rNumericArgs(1) >= 0.0) {
                            TempControlledZone(TempControlledZoneNum).DeltaTCutSet = rNumericArgs(1);
                            if (rNumericArgs(1) > 0.0) dataZoneTempPredictorCorrector.NumOnOffCtrZone++;
                        } else {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                            TrimSigDigits(rNumericArgs(1), 0) + "].");
                            ShowContinueError("..Allowable values must be greater or equal to 0");
                            ErrorsFound = true;
                        }
                    }
                    if (TempControlledZone(TempControlledZoneNum).DeltaTCutSet > 0.0) {
                        for (ControlTypeNum = 1; ControlTypeNum <= TempControlledZone(TempControlledZoneNum).NumControlTypes; ++ControlTypeNum) {
                            if (UtilityRoutines::SameString(TempControlledZone(TempControlledZoneNum).ControlType(ControlTypeNum),
                                                            "ThermostatSetpoint:SingleHeatingOrCooling")) {
                                ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                                 ": The choice of Temperature Difference Between Cutout And Setpoint will not be applied to "
                                                 "ThermostatSetpoint:SingleHeatingOrCooling.");
                            }
                        }
                    }
                }
            } // NumTStatStatements
        }     // Check on number of TempControlledZones

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint));
        dataZoneTempPredictorCorrector.NumSingleTempHeatingControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumSingleTempHeatingControls > 0) dataZoneTempPredictorCorrector.SetPointSingleHeating.allocate(dataZoneTempPredictorCorrector.NumSingleTempHeatingControls);

        for (SingleTempHeatingControlNum = 1; SingleTempHeatingControlNum <= dataZoneTempPredictorCorrector.NumSingleTempHeatingControls; ++SingleTempHeatingControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // SingleTempHeatingControlNum

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint));
        dataZoneTempPredictorCorrector.NumSingleTempCoolingControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumSingleTempCoolingControls > 0) dataZoneTempPredictorCorrector.SetPointSingleCooling.allocate(dataZoneTempPredictorCorrector.NumSingleTempCoolingControls);

        for (SingleTempCoolingControlNum = 1; SingleTempCoolingControlNum <= dataZoneTempPredictorCorrector.NumSingleTempCoolingControls; ++SingleTempCoolingControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // SingleTempCoolingControlNum

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint));
        dataZoneTempPredictorCorrector.NumSingleTempHeatCoolControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumSingleTempHeatCoolControls > 0) dataZoneTempPredictorCorrector.SetPointSingleHeatCool.allocate(dataZoneTempPredictorCorrector.NumSingleTempHeatCoolControls);

        for (SingleTempHeatCoolControlNum = 1; SingleTempHeatCoolControlNum <= dataZoneTempPredictorCorrector.NumSingleTempHeatCoolControls; ++SingleTempHeatCoolControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // SingleTempHeatCoolControlNum

        cCurrentModuleObject = ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint));
        dataZoneTempPredictorCorrector.NumDualTempHeatCoolControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumDualTempHeatCoolControls > 0) dataZoneTempPredictorCorrector.SetPointDualHeatCool.allocate(dataZoneTempPredictorCorrector.NumDualTempHeatCoolControls);

        for (DualTempHeatCoolControlNum = 1; DualTempHeatCoolControlNum <= dataZoneTempPredictorCorrector.NumDualTempHeatCoolControls; ++DualTempHeatCoolControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
            dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName = cAlphaArgs(3);
            dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex = GetScheduleIndex(cAlphaArgs(3));
            if (dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            }

        } // DualTempHeatCoolControlNum

        // Finish filling in Schedule pointing indexes
        for (TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum) {
            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)),
                                                  TempControlledZone(TempControlledZoneNum).ControlType,
                                                  TempControlledZone(TempControlledZoneNum).NumControlTypes);
            TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatSetPoint = TempIndex;
            if (TempIndex > 0) {
                TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), dataZoneTempPredictorCorrector.SetPointSingleHeating);
                TStatControlTypes(TempControlledZoneNum).MustHave(SingleHeatingSetPoint) = true;
            }

            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)),
                                                  TempControlledZone(TempControlledZoneNum).ControlType,
                                                  TempControlledZone(TempControlledZoneNum).NumControlTypes);
            TempControlledZone(TempControlledZoneNum).SchIndx_SingleCoolSetPoint = TempIndex;
            if (TempIndex > 0) {
                TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), dataZoneTempPredictorCorrector.SetPointSingleCooling);
                TStatControlTypes(TempControlledZoneNum).MustHave(SingleCoolingSetPoint) = true;
            }

            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)),
                                                  TempControlledZone(TempControlledZoneNum).ControlType,
                                                  TempControlledZone(TempControlledZoneNum).NumControlTypes);
            TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatCoolSetPoint = TempIndex;
            if (TempIndex > 0) {
                TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), dataZoneTempPredictorCorrector.SetPointSingleHeatCool);
                TStatControlTypes(TempControlledZoneNum).MustHave(SingleHeatCoolSetPoint) = true;
            }

            TempIndex = UtilityRoutines::FindItem(ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)),
                                                  TempControlledZone(TempControlledZoneNum).ControlType,
                                                  TempControlledZone(TempControlledZoneNum).NumControlTypes);
            TempControlledZone(TempControlledZoneNum).SchIndx_DualSetPointWDeadBand = TempIndex;
            if (TempIndex > 0) {
                TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex) =
                    UtilityRoutines::FindItem(TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex), dataZoneTempPredictorCorrector.SetPointDualHeatCool);
                TStatControlTypes(TempControlledZoneNum).MustHave(DualSetPointWithDeadBand) = true;
            }
        }

        // Now, Check the schedule values/indices for validity

        for (TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum) {

            ActualZoneNum = TempControlledZone(TempControlledZoneNum).ActualZoneNum;
            CTIndex = TempControlledZone(TempControlledZoneNum).CTSchedIndex;
            if (CTIndex == 0) continue; // error will be caught elsewhere
            SchedMin = GetScheduleMinValue(CTIndex);
            SchedMax = GetScheduleMaxValue(CTIndex);

            if (SchedMin == 0 && SchedMax == 0) {
                if (FindNumberInList(CTIndex, CTSchedMapToControlledZone, NumTempControlledZones) == 0) {
                    ShowSevereError("Control Type Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                    ShowContinueError("..specifies control type 0 for all entries.");
                    ShowContinueError("All zones using this Control Type Schedule have no heating or cooling available.");
                }
                CTSchedMapToControlledZone(TempControlledZoneNum) = CTIndex;
            }

            for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {

                        TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatSetPoint;
                        TStatControlTypes(TempControlledZoneNum).DidHave(SingleHeatingSetPoint) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)) +
                                                " Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(CTIndex, SingleHeatingSetPoint)) {
                                ShowSevereError("Control Type Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies control type 1 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {

                        TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_SingleCoolSetPoint;
                        TStatControlTypes(TempControlledZoneNum).DidHave(SingleCoolingSetPoint) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)) +
                                                " Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(CTIndex, SingleCoolingSetPoint)) {
                                ShowSevereError("Control Type Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies control type 2 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {

                        TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_SingleHeatCoolSetPoint;
                        TStatControlTypes(TempControlledZoneNum).DidHave(SingleHeatCoolSetPoint) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)) +
                                                " Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(CTIndex, SingleHeatCoolSetPoint)) {
                                ShowSevereError("Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies control type 3 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {

                        TempIndex = TempControlledZone(TempControlledZoneNum).SchIndx_DualSetPointWDeadBand;
                        TStatControlTypes(TempControlledZoneNum).DidHave(DualSetPointWithDeadBand) = true;
                        if (TempIndex != 0) {
                            SchedTypeIndex = TempControlledZone(TempControlledZoneNum).ControlTypeSchIndx(TempIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)) +
                                                " Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeName(TempIndex));
                                ErrorsFound = true;
                            }
                        } else { // TempIndex = 0
                            if (CheckScheduleValue(CTIndex, DualSetPointWithDeadBand)) {
                                ShowSevereError("Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies control type 4 (" + ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else {
                        ShowSevereError("GetZoneAirSetpoints: Illegal control type for Zone=" + Zone(ActualZoneNum).Name +
                                        ", Found value=" + TrimSigDigits(ControlTypeNum) +
                                        ", in Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("..valid range values are [0,4].");
                        ErrorsFound = true;
                    }
                }
            }
        }

        for (TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum) {

            ActualZoneNum = TempControlledZone(TempControlledZoneNum).ActualZoneNum;
            CTIndex = TempControlledZone(TempControlledZoneNum).CTSchedIndex;
            if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

            for (ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum) {
                if (TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum) &&
                    TStatControlTypes(TempControlledZoneNum).DidHave(ControlTypeNum))
                    continue;

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 1 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHeatSetPoint)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 2 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglCoolSetPoint)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 3 (" + ValidControlTypes(static_cast<int>(ComfortControl::SglHCSetPoint)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        if (!TStatControlTypes(TempControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + TempControlledZone(TempControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 4 (" + ValidControlTypes(static_cast<int>(ComfortControl::DualSetPoint)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) + '=' + TempControlledZone(TempControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + TempControlledZone(TempControlledZoneNum).ZoneName);

                    } else {
                    }
                }
            }
        }

        if (allocated(TStatControlTypes)) TStatControlTypes.deallocate();
        // This starts the Humidity Control Get Input section
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::HStat));
        NumHumidityControlZones = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumHumidityControlZones > 0) {
            HumidityControlZone.allocate(NumHumidityControlZones);
            dataZoneTempPredictorCorrector.HumidityControlZoneUniqueNames.reserve(static_cast<unsigned>(NumHumidityControlZones));
        }

        for (HumidControlledZoneNum = 1; HumidControlledZoneNum <= NumHumidityControlZones; ++HumidControlledZoneNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            HumidityControlZone(HumidControlledZoneNum).ControlName = cAlphaArgs(1);
            GlobalNames::IntraObjUniquenessCheck(
                cAlphaArgs(2), cCurrentModuleObject, cAlphaFieldNames(2), dataZoneTempPredictorCorrector.HumidityControlZoneUniqueNames, ErrorsFound);

            HumidityControlZone(HumidControlledZoneNum).ZoneName = cAlphaArgs(2);
            HumidityControlZone(HumidControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItem(cAlphaArgs(2), Zone);
            if (HumidityControlZone(HumidControlledZoneNum).ActualZoneNum == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
            HumidityControlZone(HumidControlledZoneNum).HumidifyingSched = cAlphaArgs(3);
            HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(3));
            if (HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            }
            if (NumAlphas == 4) {
                HumidityControlZone(HumidControlledZoneNum).DehumidifyingSched = cAlphaArgs(4);
                HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(4));
                if (HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            } else {
                HumidityControlZone(HumidControlledZoneNum).DehumidifyingSched = cAlphaArgs(3);
                HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(3));
            }

        } // HumidControlledZoneNum

        // Start to read Thermal comfort control objects
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::TCTStat));
        NumComfortTStatStatements = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        ComfortTStatObjects.allocate(NumComfortTStatStatements);

        // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
        NumComfortControlledZones = 0;
        errFlag = false;
        for (Item = 1; Item <= NumComfortTStatStatements; ++Item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            ZLItem = 0;
            if (Item1 == 0 && NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
            ComfortTStatObjects(Item).Name = cAlphaArgs(1);
            if (Item1 > 0) {
                ComfortTStatObjects(Item).ComfortControlledZoneStartPtr = NumComfortControlledZones + 1;
                ++NumComfortControlledZones;
                ComfortTStatObjects(Item).NumOfZones = 1;
                ComfortTStatObjects(Item).ZoneListActive = false;
                ComfortTStatObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                ComfortTStatObjects(Item).ComfortControlledZoneStartPtr = NumComfortControlledZones + 1;
                NumComfortControlledZones += ZoneList(ZLItem).NumOfZones;
                ComfortTStatObjects(Item).NumOfZones = ZoneList(ZLItem).NumOfZones;
                ComfortTStatObjects(Item).ZoneListActive = true;
                ComfortTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                errFlag = true;
                ErrorsFound = true;
            }
        }

        if (errFlag) {
            ShowSevereError("GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects.");
            ShowContinueError("...These will not be read in.  Other errors may occur.");
            NumComfortControlledZones = 0;
        }

        if (NumComfortControlledZones > 0) {
            ComfortControlledZone.allocate(NumComfortControlledZones);
            TComfortControlTypes.allocate(NumComfortControlledZones); // Number of set point types
            CCmSchedMapToControlledZone.dimension(NumComfortControlledZones, 0);

            ComfortControlledZoneNum = 0;
            for (Item = 1; Item <= NumComfortTStatStatements; ++Item) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
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
                        cAlphaArgs(2) = Zone(ZoneList(ComfortTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                    }
                    ZoneAssigned = UtilityRoutines::FindItemInList(
                        cAlphaArgs(2), ComfortControlledZone, &ZoneComfortControls::ZoneName, ComfortControlledZoneNum - 1);
                    if (ZoneAssigned == 0) {
                        ComfortControlledZone(ComfortControlledZoneNum).ZoneName = cAlphaArgs(2);
                        ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                        if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" +
                                            cAlphaArgs(2) + "\" not found.");
                            ErrorsFound = true;
                        }
                    } else {
                        ComfortControlledZone(ComfortControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                        ShowContinueError("...Zone was previously assigned to Thermostat=\"" + ComfortControlledZone(ZoneAssigned).Name + "\".");
                        ErrorsFound = true;
                        continue;
                    }

                    if (!ComfortTStatObjects(Item).ZoneListActive) {
                        ComfortControlledZone(ComfortControlledZoneNum).Name = cAlphaArgs(1);
                    } else {
                        ComfortControlledZone(ComfortControlledZoneNum).Name =
                            Zone(ZoneList(ComfortTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name + ' ' + ComfortTStatObjects(Item).Name;
                    }

                    // Read Fields A3 and A4 for averaging method
                    IZoneCount = 0;
                    for (i = 1; i <= TotPeople; ++i) {
                        if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == People(i).ZonePtr) {
                            ++IZoneCount;
                        }
                    }
                    // Could not find a people object for this particular zone
                    if (IZoneCount == 0 && ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum > 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " no PEOPLE in " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\" - cannot use Comfort Control.");
                        ErrorsFound = true;
                    }
                    ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::NO);
                    if (IZoneCount > 1) {
                        ComfortControlledZone(ComfortControlledZoneNum).AverageMethodName = cAlphaArgs(3);
                        if (UtilityRoutines::SameString(cAlphaArgs(3), "SpecificObject")) {
                            ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::SPE);
                        }
                        if (UtilityRoutines::SameString(cAlphaArgs(3), "ObjectAverage")) {
                            ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::OBJ);
                        }
                        if (UtilityRoutines::SameString(cAlphaArgs(3), "PeopleAverage")) {
                            ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum = static_cast<int>(AverageMethod::PEO);
                        }
                        if (ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\".");
                            ShowContinueError("Allowed keys are SpecificObject, ObjectAverage, or PeopleAverage");
                            ErrorsFound = true;
                        }
                        if (ComfortControlledZone(ComfortControlledZoneNum).AverageMethodNum == static_cast<int>(AverageMethod::SPE)) {
                            ComfortControlledZone(ComfortControlledZoneNum).AverageObjectName = cAlphaArgs(4);
                            if (UtilityRoutines::FindItem(cAlphaArgs(4), People) == 0) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                cAlphaArgs(4) + "\".");
                                ErrorsFound = true;
                            } else {
                                ComfortControlledZone(ComfortControlledZoneNum).SpecificObjectNum = UtilityRoutines::FindItem(cAlphaArgs(4), People);
                            }
                        }
                    } else {
                        for (i = 1; i <= TotPeople; ++i) {
                            if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == People(i).ZonePtr) break;
                        }
                        ComfortControlledZone(ComfortControlledZoneNum).SpecificObjectNum = i;
                    }
                    // Check values used for thermal comfort calculation
                    for (i = 1; i <= TotPeople; ++i) {
                        if (ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum == People(i).ZonePtr) {
                            // Check activity level
                            if (People(i).ActivityLevelPtr > 0) {
                                ValidScheduleControlType = CheckScheduleValueMinMax(People(i).ActivityLevelPtr, ">=", 72.0, "<=", 909.0);
                                if (!ValidScheduleControlType) {
                                    ShowSevereError(
                                        "GetPeople Activity Level: Invalid activity level values entered for thermal comfort calculation");
                                    ShowContinueError("Outside of range values [72,909], Reference object=" + People(i).Name);
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError("GetPeople Activity Level: Activity level schedule is not found=" + People(i).Name);
                                ShowContinueError("Required when the zone has Thermal Comfort Controls.");
                                ErrorsFound = true;
                            }
                            // Check Work Efficiency
                            if (People(i).WorkEffPtr > 0) {
                                ValidScheduleControlType = CheckScheduleValueMinMax(People(i).WorkEffPtr, ">=", 0.0, "<=", 1.0);
                                if (!ValidScheduleControlType) {
                                    ShowSevereError(
                                        "GetPeople work efficiency: Invalid work efficiency values entered for thermal comfort calculation");
                                    ShowContinueError("Outside of range values [0,1], Reference object=" + People(i).Name);
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError("GetPeople work efficiency: Work efficiency schedule is not found=" + People(i).Name);
                                ShowContinueError("Required when the zone has Thermal Comfort Controls.");
                                ErrorsFound = true;
                            }
                            // Check Clothing Insulation
                            if (People(i).ClothingPtr > 0) {
                                ValidScheduleControlType = CheckScheduleValueMinMax(People(i).ClothingPtr, ">", 0.0, "<=", 2.0);
                                if (!ValidScheduleControlType) {
                                    ShowSevereError(
                                        "GetPeople Clothing Insulation: Invalid Clothing Insulation values entered for thermal comfort calculation");
                                    ShowContinueError("Outside of range values [0.0,2.0], Reference object=" + People(i).Name);
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError("GetPeople Clothing Insulation: Clothing Insulation schedule is not found=" + People(i).Name);
                                ShowContinueError("Required when the zone has Thermal Comfort Controls.");
                                ErrorsFound = true;
                            }
                            // Check Air velocity
                            if (People(i).AirVelocityPtr <= 0) {
                                ShowSevereError("GetPeople Air Velocity: Air velocity schedule is not found=" + People(i).Name);
                                ShowContinueError("Required when the zone has Thermal Comfort Controls.");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Read Max and Min temperature setpoint
                    if (NumNums > 0) {
                        ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint = rNumericArgs(1);
                        if (rNumericArgs(1) > 50 || rNumericArgs(1) < 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                            TrimSigDigits(rNumericArgs(1), 0) + "].");
                            ShowContinueError("..Allowable values must be between 0 C and 50 C");
                            ErrorsFound = true;
                        }
                    }
                    if (NumNums > 1) {
                        ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint = rNumericArgs(2);
                        if (rNumericArgs(2) > 50 || rNumericArgs(2) < 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cNumericFieldNames(2) + "=[" +
                                            TrimSigDigits(rNumericArgs(2), 0) + "].");
                            ShowContinueError("..Allowable values must be between 0 C and 50 C");
                            ErrorsFound = true;
                        }
                    }
                    // Ensure MaxTemp >= MinTemp
                    if (ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint >
                        ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                        ShowContinueError(".." + cNumericFieldNames(1) + " > " + cNumericFieldNames(2));
                        ShowContinueError("..[" + TrimSigDigits(rNumericArgs(1), 0) + "] > [" + TrimSigDigits(rNumericArgs(2), 0) + "].");
                        ErrorsFound = true;
                    }
                    // If MaxTemp = MinTemp, no thermal comfort control
                    if (ComfortControlledZone(ComfortControlledZoneNum).TdbMinSetPoint ==
                        ComfortControlledZone(ComfortControlledZoneNum).TdbMaxSetPoint) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1));
                        ShowContinueError(".." + cNumericFieldNames(1) + " = " + cNumericFieldNames(2));
                        ShowContinueError("The zone will be controlled using this dry-bulb temperature setpoint.");
                    }
                    // read Thermal comfort type schedule name
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName = cAlphaArgs(5);
                    ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex = GetScheduleIndex(cAlphaArgs(5));
                    if (ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                        "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Check validity of control types.
                        ValidScheduleControlType =
                            CheckScheduleValueMinMax(ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex, ">=", 0.0, "<=", 4.0);
                        if (!ValidScheduleControlType) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cAlphaFieldNames(5) + "=\"" +
                                            cAlphaArgs(5) + "\"");
                            ShowContinueError("..contains values outside of range [0,4].");
                            ErrorsFound = true;
                        }
                    }
                    ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes = nint((NumAlphas - 5.0) / 2.0);
                    ComfortControlledZone(ComfortControlledZoneNum)
                        .ControlType.allocate(ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
                    ComfortControlledZone(ComfortControlledZoneNum)
                        .ControlTypeName.allocate(ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
                    ComfortControlledZone(ComfortControlledZoneNum)
                        .ControlTypeSchIndx.allocate(ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);

                    for (ControlTypeNum = 1; ControlTypeNum <= ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes; ++ControlTypeNum) {
                        ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5));
                        ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ControlTypeNum) = cAlphaArgs(nint(2.0 * ControlTypeNum + 5));
                        if (ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum) != "") {
                            CTIndex = UtilityRoutines::FindItem(
                                ComfortControlledZone(ComfortControlledZoneNum).ControlType(ControlTypeNum), ValidComfortControlTypes, 12);
                            if (CTIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5)) + "\"");
                                ErrorsFound = true;
                            }
                            if (CTIndex > 4) { // For Fanger control only for the time being
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                                cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"" +
                                                cAlphaArgs(nint(2.0 * ControlTypeNum - 1 + 5)) + "\"");
                                ShowContinueError("..Fanger is the only valid model.");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " +
                                            cAlphaFieldNames(nint(2.0 * ControlTypeNum - 1 + 5)) + "=\"<blank>\"");
                            ErrorsFound = true;
                        }
                        ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ControlTypeNum) = 0;
                    }
                }
            } // NumComfortTStatStatements
        }
        // End of Thermal comfort control reading and checking

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger));
        dataZoneTempPredictorCorrector.NumSingleFangerHeatingControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumSingleFangerHeatingControls > 0) dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger.allocate(dataZoneTempPredictorCorrector.NumSingleFangerHeatingControls);

        for (SingleFangerHeatingControlNum = 1; SingleFangerHeatingControlNum <= dataZoneTempPredictorCorrector.NumSingleFangerHeatingControls; ++SingleFangerHeatingControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger(SingleFangerHeatingControlNum).PMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError("..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }
        } // SingleFangerHeatingControlNum

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger));
        dataZoneTempPredictorCorrector.NumSingleFangerCoolingControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumSingleFangerCoolingControls > 0) {
            dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger.allocate(dataZoneTempPredictorCorrector.NumSingleFangerCoolingControls);
        }

        for (SingleFangerCoolingControlNum = 1; SingleFangerCoolingControlNum <= dataZoneTempPredictorCorrector.NumSingleFangerCoolingControls; ++SingleFangerCoolingControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger(SingleFangerCoolingControlNum).PMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError("..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }

        } // SingleFangerCoolingControlNum

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger));
        dataZoneTempPredictorCorrector.NumSingleFangerHeatCoolControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumSingleFangerHeatCoolControls > 0) dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger.allocate(dataZoneTempPredictorCorrector.NumSingleFangerHeatCoolControls);

        for (SingleFangerHeatCoolControlNum = 1; SingleFangerHeatCoolControlNum <= dataZoneTempPredictorCorrector.NumSingleFangerHeatCoolControls;
             ++SingleFangerHeatCoolControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum).PMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError("..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }

        } // SingleFangerHeatCoolControlNum

        cCurrentModuleObject = ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger));
        dataZoneTempPredictorCorrector.NumDualFangerHeatCoolControls = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (dataZoneTempPredictorCorrector.NumDualFangerHeatCoolControls > 0) dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger.allocate(dataZoneTempPredictorCorrector.NumDualFangerHeatCoolControls);

        for (DualFangerHeatCoolControlNum = 1; DualFangerHeatCoolControlNum <= dataZoneTempPredictorCorrector.NumDualFangerHeatCoolControls; ++DualFangerHeatCoolControlNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).Name = cAlphaArgs(1);
            dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSetptSchedName = cAlphaArgs(2);
            dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSchedIndex = GetScheduleIndex(cAlphaArgs(2));
            if (dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
            dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSetptSchedName = cAlphaArgs(3);
            dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSchedIndex = GetScheduleIndex(cAlphaArgs(3));
            if (dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSchedIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            } else {
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).HeatPMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" entered.");
                    ShowContinueError("..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
                ValidScheduleControlType =
                    CheckScheduleValueMinMax(dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum).CoolPMVSchedIndex, ">=", -3.0, "<=", 3.0);
                if (!ValidScheduleControlType) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid PMV values " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\" entered.");
                    ShowContinueError("..Values outside of range [-3,+3].");
                    ErrorsFound = true;
                }
            }

        } // DualFangerHeatCoolControlNum

        // Finish filling in Schedule pointing indexes for Thermal Comfort Control
        for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= NumComfortControlledZones; ++ComfortControlledZoneNum) {
            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)),
                                                     ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHeatSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) = true;
            }

            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)),
                                                     ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglCoolSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) = true;
            }

            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)),
                                                     ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHCSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::SglHCSetPointFanger)) = true;
            }

            ComfortIndex = UtilityRoutines::FindItem(ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)),
                                                     ComfortControlledZone(ComfortControlledZoneNum).ControlType,
                                                     ComfortControlledZone(ComfortControlledZoneNum).NumControlTypes);
            ComfortControlledZone(ComfortControlledZoneNum).SchIndx_DualSetPointFanger = ComfortIndex;
            if (ComfortIndex > 0) {
                ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex) = UtilityRoutines::FindItem(
                    ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex), dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger);
                TComfortControlTypes(ComfortControlledZoneNum).MustHave(static_cast<int>(ComfortControl::DualSetPointFanger)) = true;
            }
        }

        // Now, Check the schedule values/indices for validity for Thermal Comfort Control

        for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= NumComfortControlledZones; ++ComfortControlledZoneNum) {

            ActualZoneNum = ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum;
            CTIndex = ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex;
            if (CTIndex == 0) continue; // error will be caught elsewhere
            SchedMin = GetScheduleMinValue(CTIndex);
            SchedMax = GetScheduleMaxValue(CTIndex);

            if (SchedMin == 0 && SchedMax == 0) {
                if (FindNumberInList(CTIndex, CCmSchedMapToControlledZone, NumComfortControlledZones) == 0) {
                    ShowWarningError("Control Type Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                    ShowContinueError("..specifies control type 0 for all entries.");
                    ShowContinueError("All zones using this Control Type Schedule have no thermal comfort control.");
                }
                CCmSchedMapToControlledZone(ComfortControlledZoneNum) = CTIndex;
            }

            for (ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum) {

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == 0) { // Thermal comfort uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {

                        ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHeatSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) +
                                                " Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(CTIndex, static_cast<int>(ComfortControl::SglHeatSetPointFanger))) {
                                ShowSevereError("Control Type Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies thermal control type 1 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {

                        ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglCoolSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) +
                                                " Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(CTIndex, static_cast<int>(ComfortControl::SglCoolSetPointFanger))) {
                                ShowSevereError("Control Type Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies thermal control type 2 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {

                        ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_SglHCSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::SglHCSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)) +
                                                " Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(CTIndex, static_cast<int>(ComfortControl::SglHCSetPointFanger))) {
                                ShowSevereError("Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies thermal control type 3 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {

                        ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum).SchIndx_DualSetPointFanger;
                        TComfortControlTypes(ComfortControlledZoneNum).DidHave(static_cast<int>(ComfortControl::DualSetPointFanger)) = true;
                        if (ComfortIndex != 0) {
                            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchIndx(ComfortIndex);
                            if (SchedTypeIndex == 0) {
                                ShowSevereError("GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)) +
                                                " Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeName(ComfortIndex));
                                ErrorsFound = true;
                            }
                        } else { // ComfortIndex = 0
                            if (CheckScheduleValue(CTIndex, static_cast<int>(ComfortControl::DualSetPointFanger))) {
                                ShowSevereError("Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                                ShowContinueError("..specifies thermal control type 4 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)) +
                                                  ") as the control type. Not valid for this zone.");
                                ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' +
                                                  ComfortControlledZone(ComfortControlledZoneNum).Name);
                                ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);
                                ErrorsFound = true;
                            }
                        }
                        // CASE PIERCE
                        // CASE KSU

                    } else {
                        ShowSevereError("GetZoneAirSetpoints: Illegal control type for Zone=" + Zone(ActualZoneNum).Name +
                                        ", Found value=" + TrimSigDigits(ControlTypeNum) +
                                        ", in Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("..valid range values are [0,4].");
                        ErrorsFound = true;
                    }
                }
            }
        }

        for (ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= NumComfortControlledZones; ++ComfortControlledZoneNum) {

            ActualZoneNum = ComfortControlledZone(ComfortControlledZoneNum).ActualZoneNum;
            CTIndex = ComfortControlledZone(ComfortControlledZoneNum).ComfortSchedIndex;
            if (CTIndex == 0) continue; // error caught elsewhere -- would just be confusing here

            for (ControlTypeNum = 1; ControlTypeNum <= 12; ++ControlTypeNum) {
                if (TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum) &&
                    TComfortControlTypes(ComfortControlledZoneNum).DidHave(ControlTypeNum))
                    continue;

                {
                    auto const SELECT_CASE_var(ControlTypeNum);

                    if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 1 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHeatSetPointFanger)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 2 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglCoolSetPointFanger)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 3 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::SglHCSetPointFanger)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        if (!TComfortControlTypes(ComfortControlledZoneNum).MustHave(ControlTypeNum)) continue;
                        ShowWarningError("Schedule=" + ComfortControlledZone(ComfortControlledZoneNum).ControlTypeSchedName);
                        ShowContinueError("...should include control type 4 (" + ValidComfortControlTypes(static_cast<int>(ComfortControl::DualSetPointFanger)) + ") but does not.");
                        ShowContinueError("..reference " + cZControlTypes(static_cast<int>(ZControlTypes::TCTStat)) + '=' + ComfortControlledZone(ComfortControlledZoneNum).Name);
                        ShowContinueError("..reference ZONE=" + ComfortControlledZone(ComfortControlledZoneNum).ZoneName);

                        // CASE PIERCE
                        // CASE KSU

                    } else {
                    }
                }
            }
        }

        if (allocated(TComfortControlTypes)) TComfortControlTypes.deallocate();

        // Get the Hybrid Model setting inputs
        GetHybridModelZone();

        // Default multiplier values
        Real64 ZoneVolCapMultpSens = 1.0;
        Real64 ZoneVolCapMultpMoist = 1.0;
        Real64 ZoneVolCapMultpCO2 = 1.0;
        Real64 ZoneVolCapMultpGenContam = 1.0;

        // Get the Zone Air Capacitance Multiplier for use in the Predictor-Corrector Procedure
        cCurrentModuleObject = "ZoneCapacitanceMultiplier:ResearchSpecial";
        int NumZoneCapaMultiplier = inputProcessor->getNumObjectsFound(cCurrentModuleObject); // Number of ZonesCapacityMultiplier object
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
                inputProcessor->getObjectItem(cCurrentModuleObject,
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
                    if (Item1 == 0 && NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
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

        print(ioFiles.eio, Header);
        print(ioFiles.eio, Format_701, ZoneVolCapMultpSens, ZoneVolCapMultpMoist, ZoneVolCapMultpCO2, ZoneVolCapMultpGenContam);

        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::OTTStat));
        NumOpTempControlledZones = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumOpTempControlledZones > 0) {
            AnyOpTempControl = true;

            for (OpTempContrlNum = 1; OpTempContrlNum <= NumOpTempControlledZones; ++OpTempContrlNum) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
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
                found = UtilityRoutines::FindItem(cAlphaArgs(1), TStatObjects);
                if (found == 0) {
                    // It might be in the TempControlledZones
                    found = UtilityRoutines::FindItem(cAlphaArgs(1), TempControlledZone);
                    if (found == 0) { // throw error
                        ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) +
                                        " reference not found.");
                        ErrorsFound = true;
                    } else {
                        TempControlledZoneNum = found;
                        TempControlledZone(TempControlledZoneNum).OperativeTempControl = true;
                        if (UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled")) {
                            TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled = true;
                        }
                        if ((!(UtilityRoutines::SameString(cAlphaArgs(2), "Scheduled"))) &&
                            (!(UtilityRoutines::SameString(cAlphaArgs(2), "Constant")))) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                            "\".");
                            ErrorsFound = true;
                        }

                        TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction = rNumericArgs(1);
                        TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched = GetScheduleIndex(cAlphaArgs(3));
                        if ((TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched == 0) &&
                            (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled)) { // throw error
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                            "\" not found.");
                            ErrorsFound = true;
                        }

                        // check validity of fixed radiative fraction
                        if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction < 0.0) &&
                            (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                            TrimSigDigits(rNumericArgs(1), 2) + "\" cannot be negative.");
                            ErrorsFound = true;
                        }
                        if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction >= 0.9) &&
                            (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                            TrimSigDigits(rNumericArgs(1), 2) + "\" cannot >= .9.");
                            ErrorsFound = true;
                        }

                        // check schedule min max.
                        if (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled) {
                            ValidRadFractSched =
                                CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                            if (!ValidRadFractSched) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(3) + "=[" +
                                                cAlphaArgs(3) + "\".");
                                ShowContinueError("..Values outside of range [0.0,0.9).");
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
                                    ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                    cAlphaArgs(4) + "\" not found.");
                                    ErrorsFound = true;
                                } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                    TempControlledZone(TempControlledZoneNum).AdaptiveComfortTempControl = true;
                                    TempControlledZone(TempControlledZoneNum).AdaptiveComfortModelTypeIndex =
                                        UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                    if (!dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.initialized) {
                                        Array1D<Real64> runningAverageASH(NumDaysInYear, 0.0);
                                        Array1D<Real64> runningAverageCEN(NumDaysInYear, 0.0);
                                        CalculateMonthlyRunningAverageDryBulb(ioFiles, runningAverageASH, runningAverageCEN);
                                        CalculateAdaptiveComfortSetPointSchl(dataZoneTempPredictorCorrector, runningAverageASH, runningAverageCEN);
                                    }
                                }
                            }
                        }

                        // CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
                        SetupOutputVariable("Zone Thermostat Operative Temperature",
                                            OutputProcessor::Unit::C,
                                            ZnAirRpt(TempControlledZone(TempControlledZoneNum).ActualZoneNum).ThermOperativeTemp,
                                            "Zone",
                                            "Average",
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
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" +
                                                cAlphaArgs(2) + "\".");
                                ErrorsFound = true;
                            }
                        }

                        TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction = rNumericArgs(1);
                        TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched = GetScheduleIndex(cAlphaArgs(3));
                        if (Item == 1) {
                            if ((TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched == 0) &&
                                (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled)) { // throw error
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(3) + "=\"" +
                                                cAlphaArgs(3) + "\" not found.");
                                ErrorsFound = true;
                            }
                        }

                        // check validity of fixed radiative fraction
                        if (Item == 1) {
                            if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction < 0.0) &&
                                (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                                TrimSigDigits(rNumericArgs(1), 2) + "\" cannot be negative.");
                                ErrorsFound = true;
                            }
                        }
                        if (Item == 1) {
                            if ((TempControlledZone(TempControlledZoneNum).FixedRadiativeFraction >= 0.9) &&
                                (!(TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled))) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                                TrimSigDigits(rNumericArgs(1), 2) + "\" cannot >= .9.");
                                ErrorsFound = true;
                            }
                        }

                        // check schedule min max.
                        if (Item == 1) {
                            if (TempControlledZone(TempControlledZoneNum).OpTempCntrlModeScheduled) {
                                ValidRadFractSched = CheckScheduleValueMinMax(
                                    TempControlledZone(TempControlledZoneNum).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9);
                                if (!ValidRadFractSched) {
                                    ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(3) + "=[" +
                                                    cAlphaArgs(3) + "\".");
                                    ShowContinueError("..Values outside of range [0.0,0.9).");
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
                                    ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                    cAlphaArgs(4) + "\" not found.");
                                    ErrorsFound = true;
                                } else if (adaptiveComfortModelTypeIndex != static_cast<int>(AdaptiveComfortModel::ADAP_NONE)) {
                                    TempControlledZone(TempControlledZoneNum).AdaptiveComfortTempControl = true;
                                    TempControlledZone(TempControlledZoneNum).AdaptiveComfortModelTypeIndex =
                                        UtilityRoutines::FindItem(cAlphaArgs(4), AdaptiveComfortModelTypes, AdaptiveComfortModelTypes.isize());
                                    if (!dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.initialized) {
                                        Array1D<Real64> runningAverageASH(NumDaysInYear, 0.0);
                                        Array1D<Real64> runningAverageCEN(NumDaysInYear, 0.0);
                                        CalculateMonthlyRunningAverageDryBulb(ioFiles, runningAverageASH, runningAverageCEN);
                                        CalculateAdaptiveComfortSetPointSchl(dataZoneTempPredictorCorrector, runningAverageASH, runningAverageCEN);
                                    }
                                }
                            }
                        }

                        // CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
                        SetupOutputVariable("Zone Thermostat Operative Temperature",
                                            OutputProcessor::Unit::C,
                                            ZnAirRpt(TempControlledZone(TempControlledZoneNum).ActualZoneNum).ThermOperativeTemp,
                                            "Zone",
                                            "Average",
                                            Zone(TempControlledZone(TempControlledZoneNum).ActualZoneNum).Name);
                    } // TStat Objects Loop
                }     // found thermostat referene
            }         // loop over NumOpTempControlledZones
        }             // NumOpTempControlledZones > 0

        // Overcool dehumidificaton GetInput starts here
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::TandHStat));
        NumTempAndHumidityControlledZones = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumTempAndHumidityControlledZones > 0) {
            AnyZoneTempAndHumidityControl = true;

            for (TempHumidityCntrlNum = 1; TempHumidityCntrlNum <= NumTempAndHumidityControlledZones; ++TempHumidityCntrlNum) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
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
                found = UtilityRoutines::FindItem(cAlphaArgs(1), TStatObjects);
                if (found == 0) {
                    // It might be in the TempControlledZones
                    found = UtilityRoutines::FindItem(cAlphaArgs(1), TempControlledZone);
                    if (found == 0) { // throw error
                        ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cZControlTypes(static_cast<int>(ZControlTypes::TStat)) +
                                        " reference not found.");
                        ErrorsFound = true;
                    } else {
                        TempControlledZoneNum = found;
                        TempControlledZone(TempControlledZoneNum).DehumidifyingSched = cAlphaArgs(2);
                        TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(2));
                        if (TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
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
                        if ((!(UtilityRoutines::SameString(cAlphaArgs(4), "Scheduled"))) &&
                            (!(UtilityRoutines::SameString(cAlphaArgs(4), "Constant")))) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                            "\".");
                            ErrorsFound = true;
                        }

                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange = rNumericArgs(1);
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex = GetScheduleIndex(cAlphaArgs(4));
                        if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex == 0) &&
                            (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled)) { // throw error
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) +
                                            "\" not found.");
                            ErrorsFound = true;
                        }

                        // check validity of zone Overcool constant range
                        if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange < 0.0) &&
                            (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                            TrimSigDigits(rNumericArgs(1), 2) + "\" cannot be negative.");
                            ErrorsFound = true;
                        }
                        if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange > 3.0) &&
                            (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                            TrimSigDigits(rNumericArgs(1), 2) + "\" cannot be > 3.0");
                            ErrorsFound = true;
                        }

                        // check zone Overcool range schedule min/max values.
                        if (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled) {
                            ValidZoneOvercoolRangeSched =
                                CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                            if (!ValidZoneOvercoolRangeSched) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(5) + "=[" +
                                                cAlphaArgs(5) + "\".");
                                ShowContinueError("..Values outside of range [0.0,3.0].");
                                ErrorsFound = true;
                            }
                        }
                        // check Overcool Control Ratio limits
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio = rNumericArgs(2);
                        if (TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio < 0.0) {
                            ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(2) + " invalid " + cNumericFieldNames(2) + "=[" +
                                            TrimSigDigits(rNumericArgs(2), 2) + "\" cannot be negative.");
                            ErrorsFound = true;
                        }
                    }
                } else {
                    for (Item = 1; Item <= TStatObjects(found).NumOfZones; ++Item) {
                        TempControlledZoneNum = TStatObjects(found).TempControlledZoneStartPtr + Item - 1;
                        TempControlledZone(TempControlledZoneNum).DehumidifyingSched = cAlphaArgs(2);
                        TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(2));
                        if (TempControlledZone(TempControlledZoneNum).DehumidifyingSchedIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
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
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(4) + "=\"" +
                                                cAlphaArgs(4) + "\".");
                                ErrorsFound = true;
                            }
                        }
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange = rNumericArgs(1);
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex = GetScheduleIndex(cAlphaArgs(6));
                        if (Item == 1) {
                            if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex == 0) &&
                                (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled)) { // throw error
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cAlphaFieldNames(5) + "=\"" +
                                                cAlphaArgs(5) + "\" not found.");
                                ErrorsFound = true;
                            }
                        }
                        // check validity of zone Overcool constant range
                        if (Item == 1) {
                            if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange < 0.0) &&
                                (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                                TrimSigDigits(rNumericArgs(1), 2) + "\" cannot be negative.");
                                ErrorsFound = true;
                            }
                        }
                        if (Item == 1) {
                            if ((TempControlledZone(TempControlledZoneNum).ZoneOvercoolConstRange > 3.0) &&
                                (!(TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled))) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid " + cNumericFieldNames(1) + "=[" +
                                                TrimSigDigits(rNumericArgs(1), 2) + "\" cannot > 3.0");
                                ErrorsFound = true;
                            }
                        }
                        // check zone Overcool range schedule min/max values.
                        if (Item == 1) {
                            if (TempControlledZone(TempControlledZoneNum).OvercoolCntrlModeScheduled) {
                                ValidZoneOvercoolRangeSched = CheckScheduleValueMinMax(
                                    TempControlledZone(TempControlledZoneNum).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0);
                                if (!ValidZoneOvercoolRangeSched) {
                                    ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(1) + " invalid values " + cAlphaFieldNames(5) + "=[" +
                                                    cAlphaArgs(5) + "\".");
                                    ShowContinueError("..Values outside of range [0.0,3.0].");
                                    ErrorsFound = true;
                                }
                            }
                        }
                        TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio = rNumericArgs(2);
                        // check Overcool Control Ratio limits
                        if (Item == 1) {
                            if (TempControlledZone(TempControlledZoneNum).ZoneOvercoolControlRatio < 0.0) {
                                ShowSevereError(cCurrentModuleObject + '=' + cAlphaArgs(2) + " invalid " + cNumericFieldNames(2) + "=[" +
                                                TrimSigDigits(rNumericArgs(2), 2) + "\" cannot be negative.");
                                ErrorsFound = true;
                            }
                        }

                    } // TStat Objects Loop
                }     // found thermostat reference
            }         // loop over NumTempAndHumidityControlledZones
        }             // NumTempAndHumidityControlledZones > 0

        // Staged thermostat control inputs start
        cCurrentModuleObject = cZControlTypes(static_cast<int>(ZControlTypes::StagedDual));
        NumStageControlledZones = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (NumStageControlledZones > 0) StagedTStatObjects.allocate(NumStageControlledZones);

        // Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
        dataZoneTempPredictorCorrector.NumStageCtrZone = 0;
        for (Item = 1; Item <= NumStageControlledZones; ++Item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            StagedTStatObjects(Item).Name = cAlphaArgs(1);
            Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            ZLItem = 0;
            if (Item1 == 0 && NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
            if (Item1 > 0) {
                StagedTStatObjects(Item).StageControlledZoneStartPtr = dataZoneTempPredictorCorrector.NumStageCtrZone + 1;
                ++dataZoneTempPredictorCorrector.NumStageCtrZone;
                StagedTStatObjects(Item).NumOfZones = 1;
                StagedTStatObjects(Item).ZoneListActive = false;
                StagedTStatObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                StagedTStatObjects(Item).TempControlledZoneStartPtr = dataZoneTempPredictorCorrector.NumStageCtrZone + 1;
                dataZoneTempPredictorCorrector.NumStageCtrZone += ZoneList(ZLItem).NumOfZones;
                StagedTStatObjects(Item).NumOfZones = ZoneList(ZLItem).NumOfZones;
                StagedTStatObjects(Item).ZoneListActive = true;
                StagedTStatObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowSevereError("GetStagedDualSetpoint: Errors with invalid names in " + cCurrentModuleObject + " objects.");
            ShowContinueError("...These will not be read in.  Other errors may occur.");
            dataZoneTempPredictorCorrector.NumStageCtrZone = 0;
        }

        if (dataZoneTempPredictorCorrector.NumStageCtrZone > 0) {
            StageControlledZone.allocate(dataZoneTempPredictorCorrector.NumStageCtrZone);
            StageZoneLogic.dimension(NumOfZones, false);

            StageControlledZoneNum = 0;
            for (Item = 1; Item <= NumStageControlledZones; ++Item) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
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
                for (Item1 = 1; Item1 <= StagedTStatObjects(Item).NumOfZones; ++Item1) {
                    ++StageControlledZoneNum;
                    if (StagedTStatObjects(Item).ZoneListActive) {
                        cAlphaArgs(2) = Zone(ZoneList(StagedTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name;
                    }
                    ZoneAssigned = UtilityRoutines::FindItemInList(
                        cAlphaArgs(2), StageControlledZone, &ZoneStagedControls::ZoneName, StageControlledZoneNum - 1);
                    if (ZoneAssigned == 0) {
                        StageControlledZone(StageControlledZoneNum).ZoneName = cAlphaArgs(2);
                        StageControlledZone(StageControlledZoneNum).ActualZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                        if (StageControlledZone(StageControlledZoneNum).ActualZoneNum == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" +
                                            cAlphaArgs(2) + "\" not found.");
                            ErrorsFound = true;
                        } else {
                            //           Zone(StageControlledZone(StageControlledZoneNum)%ActualZoneNum)%StageControlledZoneIndex =
                            //           StageControlledZoneNum
                        }
                        StageZoneLogic(StageControlledZone(StageControlledZoneNum).ActualZoneNum) = true;
                    } else {
                        StageControlledZone(StageControlledZoneNum).ZoneName = cAlphaArgs(2); // for continuity
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" zone previously assigned.");
                        ShowContinueError("...Zone was previously assigned to Thermostat=\"" + StageControlledZone(ZoneAssigned).Name + "\".");
                        ErrorsFound = true;
                        continue;
                    }

                    if (!StagedTStatObjects(Item).ZoneListActive) {
                        StageControlledZone(StageControlledZoneNum).Name = cAlphaArgs(1);
                    } else {
                        CheckCreatedZoneItemName(RoutineName,
                                                 cCurrentModuleObject,
                                                 Zone(ZoneList(StagedTStatObjects(Item).ZoneOrZoneListPtr).Zone(Item1)).Name,
                                                 ZoneList(StagedTStatObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                                                 StagedTStatObjects(Item).Name,
                                                 StageControlledZone,
                                                 StageControlledZoneNum - 1,
                                                 StageControlledZone(StageControlledZoneNum).Name,
                                                 errFlag);
                        if (errFlag) ErrorsFound = true;
                    }

                    StageControlledZone(StageControlledZoneNum).NumOfHeatStages = rNumericArgs(1);
                    if (rNumericArgs(1) < 1 || rNumericArgs(1) > 4) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cNumericFieldNames(1) + "=\"" +
                                        RoundSigDigits(rNumericArgs(1), 0) + "\"");
                        ShowContinueError("..contains values outside of range [1,4].");
                        ErrorsFound = true;
                    }

                    StageControlledZone(StageControlledZoneNum).HeatSetBaseSchedName = cAlphaArgs(3);
                    StageControlledZone(StageControlledZoneNum).HSBchedIndex = GetScheduleIndex(cAlphaArgs(3));
                    if (Item1 == 1) { // only show error on first of several if zone list
                        if (StageControlledZone(StageControlledZoneNum).HSBchedIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                            cAlphaArgs(3) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }

                    StageControlledZone(StageControlledZoneNum).HeatThroRange = rNumericArgs(2);
                    if (rNumericArgs(1) < 0.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" negative value is found at " + cNumericFieldNames(2) +
                                        "=\"" + RoundSigDigits(rNumericArgs(2), 1) + "\"");
                        ShowContinueError(".. The minumum value is 0.");
                        ErrorsFound = true;
                    }

                    if (StageControlledZone(StageControlledZoneNum).NumOfHeatStages > 0) {
                        StageControlledZone(StageControlledZoneNum).HeatTOffset.allocate(StageControlledZone(StageControlledZoneNum).NumOfHeatStages);
                        for (i = 1; i <= StageControlledZone(StageControlledZoneNum).NumOfHeatStages; ++i) {
                            StageControlledZone(StageControlledZoneNum).HeatTOffset(i) = rNumericArgs(2 + i);
                            if (rNumericArgs(2 + i) > 0.0) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" positive value is found at " +
                                                cNumericFieldNames(2 + i) + "=\"" + RoundSigDigits(rNumericArgs(2 + i), 1) + "\"");
                                ShowContinueError(".. The maximum value is 0.");
                                ErrorsFound = true;
                            }
                            if (lNumericFieldBlanks(2 + i)) {
                                ShowSevereError(cCurrentModuleObject + " object =" + cAlphaArgs(1) + ". The input of " + cNumericFieldNames(2 + i) +
                                                " is required, but a blank is found.");
                                ErrorsFound = true;
                            }
                            if (i > 1) {
                                if (rNumericArgs(2 + i) >= rNumericArgs(1 + i)) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" The value at " + cNumericFieldNames(2 + i) +
                                                    "=\"" + RoundSigDigits(rNumericArgs(2 + i), 1) + "\" has to be less than ");
                                    ShowContinueError(cNumericFieldNames(1 + i) + "=\"" + RoundSigDigits(rNumericArgs(1 + i), 1));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    StageControlledZone(StageControlledZoneNum).NumOfCoolStages = rNumericArgs(7);
                    if (rNumericArgs(7) < 1 || rNumericArgs(7) > 4) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid range " + cNumericFieldNames(7) + "=\"" +
                                        RoundSigDigits(rNumericArgs(7), 0) + "\"");
                        ShowContinueError("..contains values outside of range [1,4].");
                        ErrorsFound = true;
                    }

                    StageControlledZone(StageControlledZoneNum).CoolSetBaseSchedName = cAlphaArgs(4);
                    StageControlledZone(StageControlledZoneNum).CSBchedIndex = GetScheduleIndex(cAlphaArgs(4));
                    if (Item1 == 1) { // only show error on first of several if zone list
                        if (StageControlledZone(StageControlledZoneNum).CSBchedIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" +
                                            cAlphaArgs(4) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }

                    StageControlledZone(StageControlledZoneNum).CoolThroRange = rNumericArgs(8);
                    if (rNumericArgs(8) < 0.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" negative value is found at " + cNumericFieldNames(8) +
                                        "=\"" + RoundSigDigits(rNumericArgs(8), 1) + "\"");
                        ShowContinueError(".. The minumum value is 0.");
                        ErrorsFound = true;
                    }

                    if (StageControlledZone(StageControlledZoneNum).NumOfCoolStages > 0) {
                        StageControlledZone(StageControlledZoneNum).CoolTOffset.allocate(StageControlledZone(StageControlledZoneNum).NumOfCoolStages);
                        for (i = 1; i <= StageControlledZone(StageControlledZoneNum).NumOfCoolStages; ++i) {
                            StageControlledZone(StageControlledZoneNum).CoolTOffset(i) = rNumericArgs(8 + i);
                            if (rNumericArgs(8 + i) < 0.0) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" negative value is found at " +
                                                cNumericFieldNames(8 + i) + "=\"" + RoundSigDigits(rNumericArgs(8 + i), 1) + "\"");
                                ShowContinueError(".. The minimum value is 0.");
                                ErrorsFound = true;
                            }
                            if (lNumericFieldBlanks(8 + i)) {
                                ShowSevereError(cCurrentModuleObject + " object =" + cAlphaArgs(1) + ". The input of " + cNumericFieldNames(8 + i) +
                                                " is required, but a blank is found.");
                                ErrorsFound = true;
                            }
                            if (i > 1) {
                                if (rNumericArgs(8 + i) <= rNumericArgs(7 + i)) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" The value at " + cNumericFieldNames(8 + i) +
                                                    "=\"" + RoundSigDigits(rNumericArgs(8 + i), 1) + "\" has to be greater than ");
                                    ShowContinueError(cNumericFieldNames(7 + i) + "=\"" + RoundSigDigits(rNumericArgs(7 + i), 1));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                }
            } // loop over NumStageControlledZones
            if ((inputProcessor->getNumObjectsFound("AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed") == 0) &&
                (inputProcessor->getNumObjectsFound("AirLoopHVAC:UnitarySystem") == 0) &&
                (inputProcessor->getNumObjectsFound("SetpointManager:SingleZone:OneStageCooling") == 0) &&
                (inputProcessor->getNumObjectsFound("SetpointManager:SingleZone:OneStageHeating") == 0)) {
                ShowWarningError(cCurrentModuleObject + " is applicable to only selected HVAC objects which are missing from input.");
                ShowContinueError("Model should include one or more of the following objects:  ");
                ShowContinueError("AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, AirLoopHVAC:UnitarySystem, ");
                ShowContinueError(
                    "SetpointManager:SingleZone:OneStageCooling, and/or SetpointManager:SingleZone:OneStageHeating. The simulation continues...");
            }
        } // NumStageControlledZones > 0

        if (ErrorsFound) {
            ShowFatalError("Errors getting Zone Control input data.  Preceding condition(s) cause termination.");
        }
    }

    void CalculateMonthlyRunningAverageDryBulb(IOFiles &ioFiles, Array1D<Real64> &runningAverageASH, Array1D<Real64> &runningAverageCEN)
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
        using WeatherManager::NumDaysInYear;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");

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

        Array1D<Real64> adaptiveTemp(NumDaysInYear, 0.0);
        Array1D<Real64> dailyDryTemp(NumDaysInYear, 0.0);

        readStat = 0;
        if (FileSystem::fileExists(ioFiles.inputWeatherFileName.fileName)) {
            // Read hourly dry bulb temperature first
            auto epwFile = ioFiles.inputWeatherFileName.open("CalcThermalComfortAdaptive");
            for (i = 1; i <= 9; ++i) { // Headers
                epwFile.readLine();
            }
            for (i = 1; i <= NumDaysInYear; ++i) {
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
            while (dayOfYear < NumDaysInYear) {
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
                    calcStartDayASH += NumDaysInYear;
                    for (i = 1; i <= calcEndDay; i++) {
                        avgDryBulb = dailyDryTemp(i);
                        runningAverageASH(dayOfYear) = runningAverageASH(dayOfYear) + avgDryBulb;
                    }
                    for (i = calcStartDayASH; i < NumDaysInYear; i++) {
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
                    calcStartDayCEN += NumDaysInYear;
                    for (i = 1; i <= calcEndDay; i++) {
                        avgDryBulb = dailyDryTemp(i);
                        runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                    }
                    for (i = calcStartDayCEN; i < NumDaysInYear; i++) {
                        avgDryBulb = dailyDryTemp(i);
                        runningAverageCEN(dayOfYear) = runningAverageCEN(dayOfYear) + avgDryBulb;
                    }
                    runningAverageCEN(dayOfYear) /= 7;
                }
            }
        } else {
            ShowFatalError("CalcThermalComfortAdaptive: Could not open file " + ioFiles.inputWeatherFileName.fileName + " for input (read). (File does not exist)");
        }
    }

    void CalculateAdaptiveComfortSetPointSchl(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, Array1D<Real64> const &runningAverageASH, Array1D<Real64> const &runningAverageCEN)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Xuan Luo
        //       DATE WRITTEN   January 2017
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculate the zone operative temperature setpoint using adaptive comfort model.

        // Using/Aliasing
        using WeatherManager::DesDayInput;
        using WeatherManager::NumDaysInYear;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int const summerDesignDayTypeIndex(9);
        Real64 GrossApproxAvgDryBulbDesignDay(0.0);

        for (size_t i = 1; i <= DesDayInput.size(); i++) {
            // Summer design day
            if (DesDayInput(i).DayType == summerDesignDayTypeIndex) {
                GrossApproxAvgDryBulbDesignDay = (DesDayInput(i).MaxDryBulb + (DesDayInput(i).MaxDryBulb - DesDayInput(i).DailyDBRange)) / 2.0;
                if (GrossApproxAvgDryBulbDesignDay > 10 && GrossApproxAvgDryBulbDesignDay < 33.5) {
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(1) = 0.31 * GrossApproxAvgDryBulbDesignDay + 17.8;
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(2) = 0.31 * GrossApproxAvgDryBulbDesignDay + 20.3;
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(3) = 0.31 * GrossApproxAvgDryBulbDesignDay + 21.3;
                }
                if (GrossApproxAvgDryBulbDesignDay > 10 && GrossApproxAvgDryBulbDesignDay < 30) {
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(4) = 0.33 * GrossApproxAvgDryBulbDesignDay + 18.8;
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(5) = 0.33 * GrossApproxAvgDryBulbDesignDay + 20.8;
                    ;
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(6) = 0.33 * GrossApproxAvgDryBulbDesignDay + 21.8;
                    ;
                    dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(7) = 0.33 * GrossApproxAvgDryBulbDesignDay + 22.8;
                    ;
                }
            }
        }

        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central.allocate(NumDaysInYear);
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90.allocate(NumDaysInYear);
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80.allocate(NumDaysInYear);
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central.allocate(NumDaysInYear);
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I.allocate(NumDaysInYear);
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II.allocate(NumDaysInYear);
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III.allocate(NumDaysInYear);

        // Calculate the set points based on different models, set flag as -1 when running average temperature is not in the range.
        for (int day = 1; day <= NumDaysInYear; day++) {
            if (runningAverageASH(day) > 10 && runningAverageASH(day) < 33.5) {
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(day) = 0.31 * runningAverageASH(day) + 17.8;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(day) = 0.31 * runningAverageASH(day) + 20.3;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(day) = 0.31 * runningAverageASH(day) + 21.3;
            } else {
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(day) = -1;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(day) = -1;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(day) = -1;
            }
            if (runningAverageCEN(day) > 10 && runningAverageCEN(day) < 30) {
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(day) = 0.33 * runningAverageCEN(day) + 18.8;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(day) = 0.33 * runningAverageCEN(day) + 20.8;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(day) = 0.33 * runningAverageCEN(day) + 21.8;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(day) = 0.33 * runningAverageCEN(day) + 22.8;
            } else {
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(day) = -1;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(day) = -1;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(day) = -1;
                dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(day) = -1;
            }
        }
        dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.initialized = true;
    }

    void InitZoneAirSetPoints(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector)
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

        // Using/Aliasing
        using DataSurfaces::Surface;
        using DataZoneEquipment::ZoneEquipInputsFilled;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitZoneAirSetpoints: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int ZoneNum;
        bool FirstSurfFlag;
        int TRefFlag; // Flag for Reference Temperature process in Zones
        int SurfNum;

        // FLOW:
        if (dataZoneTempPredictorCorrector.InitZoneAirSetPointsOneTimeFlag) {
            TempZoneThermostatSetPoint.dimension(NumOfZones, 0.0);
            AdapComfortCoolingSetPoint.dimension(NumOfZones, 0.0);
            ZoneThermostatSetPointHi.dimension(NumOfZones, 0.0);
            ZoneThermostatSetPointLo.dimension(NumOfZones, 0.0);
            ZoneThermostatSetPointHiAver.dimension(NumOfZones, 0.0);
            ZoneThermostatSetPointLoAver.dimension(NumOfZones, 0.0);

            LoadCorrectionFactor.dimension(NumOfZones, 0.0); // PH 3/3/04
            TempControlType.dimension(NumOfZones, 0);
            if (NumComfortControlledZones > 0) {
                ComfortControlType.dimension(NumOfZones, 0);
                ZoneComfortControlsFanger.allocate(NumOfZones);
            }
            dataZoneTempPredictorCorrector.ZoneSetPointLast.dimension(NumOfZones, 0.0);
            Setback.dimension(NumOfZones, false);
            DeadBandOrSetback.dimension(NumOfZones, false);
            CurDeadBandOrSetback.dimension(NumOfZones, false);
            SNLoadHeatEnergy.dimension(NumOfZones, 0.0);
            SNLoadCoolEnergy.dimension(NumOfZones, 0.0);
            SNLoadHeatRate.dimension(NumOfZones, 0.0);
            SNLoadCoolRate.dimension(NumOfZones, 0.0);
            SNLoadPredictedRate.dimension(NumOfZones, 0.0);
            SNLoadPredictedHSPRate.dimension(NumOfZones, 0.0);
            SNLoadPredictedCSPRate.dimension(NumOfZones, 0.0);
            MoisturePredictedRate.dimension(NumOfZones, 0.0);
            MoisturePredictedHumSPRate.dimension(NumOfZones, 0.0);
            MoisturePredictedDehumSPRate.dimension(NumOfZones, 0.0);
            WZoneTimeMinus1.dimension(NumOfZones, 0.0);
            WZoneTimeMinus2.dimension(NumOfZones, 0.0);
            WZoneTimeMinus3.dimension(NumOfZones, 0.0);
            WZoneTimeMinus4.dimension(NumOfZones, 0.0);
            DSWZoneTimeMinus1.dimension(NumOfZones, 0.0);
            DSWZoneTimeMinus2.dimension(NumOfZones, 0.0);
            DSWZoneTimeMinus3.dimension(NumOfZones, 0.0);
            DSWZoneTimeMinus4.dimension(NumOfZones, 0.0);
            ZoneAirHumRatTemp.dimension(NumOfZones, 0.0);
            WZoneTimeMinus1Temp.dimension(NumOfZones, 0.0);
            WZoneTimeMinus2Temp.dimension(NumOfZones, 0.0);
            WZoneTimeMinus3Temp.dimension(NumOfZones, 0.0);
            WZoneTimeMinusP.dimension(NumOfZones, 0.0);
            dataZoneTempPredictorCorrector.TempIndZnLd.dimension(NumOfZones, 0.0);
            dataZoneTempPredictorCorrector.TempDepZnLd.dimension(NumOfZones, 0.0);
            NonAirSystemResponse.dimension(NumOfZones, 0.0);
            SysDepZoneLoads.dimension(NumOfZones, 0.0);
            SysDepZoneLoadsLagged.dimension(NumOfZones, 0.0);
            dataZoneTempPredictorCorrector.ZoneAirRelHum.dimension(NumOfZones, 0.0);
            ZoneWMX.dimension(NumOfZones, 0.0);
            ZoneWM2.dimension(NumOfZones, 0.0);
            ZoneT1.dimension(NumOfZones, 0.0);
            ZoneW1.dimension(NumOfZones, 0.0);

            ListSNLoadHeatEnergy.dimension(NumOfZoneLists, 0.0);
            ListSNLoadCoolEnergy.dimension(NumOfZoneLists, 0.0);
            ListSNLoadHeatRate.dimension(NumOfZoneLists, 0.0);
            ListSNLoadCoolRate.dimension(NumOfZoneLists, 0.0);

            GroupSNLoadHeatEnergy.dimension(NumOfZoneGroups, 0.0);
            GroupSNLoadCoolEnergy.dimension(NumOfZoneGroups, 0.0);
            GroupSNLoadHeatRate.dimension(NumOfZoneGroups, 0.0);
            GroupSNLoadCoolRate.dimension(NumOfZoneGroups, 0.0);
            AIRRAT.dimension(NumOfZones, 0.0);
            ZTM1.dimension(NumOfZones, 0.0);
            ZTM2.dimension(NumOfZones, 0.0);
            ZTM3.dimension(NumOfZones, 0.0);

            // Hybrid modeling
            PreviousMeasuredZT1.dimension(NumOfZones, 0.0);
            PreviousMeasuredZT2.dimension(NumOfZones, 0.0);
            PreviousMeasuredZT3.dimension(NumOfZones, 0.0);
            PreviousMeasuredHumRat1.dimension(NumOfZones, 0.0);
            PreviousMeasuredHumRat2.dimension(NumOfZones, 0.0);
            PreviousMeasuredHumRat3.dimension(NumOfZones, 0.0);

            // Allocate Derived Types
            ZoneSysEnergyDemand.allocate(NumOfZones);
            ZoneSysMoistureDemand.allocate(NumOfZones);

            for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                FirstSurfFlag = true;
                if (Zone(Loop).SurfaceFirst > 0) {
                    for (SurfNum = Zone(Loop).SurfaceFirst; SurfNum <= Zone(Loop).SurfaceLast; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

                        if (FirstSurfFlag) {
                            TRefFlag = Surface(SurfNum).TAirRef;
                            FirstSurfFlag = false;
                        }
                        // for each particular zone, the reference air temperature(s) should be the same
                        // (either mean air, bulk air, or supply air temp).
                        if (Surface(SurfNum).TAirRef != TRefFlag) {
                            ShowWarningError("Different reference air temperatures for difference surfaces encountered in zone " + Zone(Loop).Name);
                        }
                    }
                }
            }

            // CurrentModuleObject='Zone'
            for (Loop = 1; Loop <= NumOfZones; ++Loop) {
                SetupOutputVariable("Zone Air System Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    SNLoadHeatEnergy(Loop),
                                    "System",
                                    "Sum",
                                    Zone(Loop).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "Heating",
                                    _,
                                    "Building",
                                    Zone(Loop).Name,
                                    Zone(Loop).Multiplier,
                                    Zone(Loop).ListMultiplier);
                SetupOutputVariable("Zone Air System Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    SNLoadCoolEnergy(Loop),
                                    "System",
                                    "Sum",
                                    Zone(Loop).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "Cooling",
                                    _,
                                    "Building",
                                    Zone(Loop).Name,
                                    Zone(Loop).Multiplier,
                                    Zone(Loop).ListMultiplier);
                SetupOutputVariable(
                    "Zone Air System Sensible Heating Rate", OutputProcessor::Unit::W, SNLoadHeatRate(Loop), "System", "Average", Zone(Loop).Name);
                SetupOutputVariable(
                    "Zone Air System Sensible Cooling Rate", OutputProcessor::Unit::W, SNLoadCoolRate(Loop), "System", "Average", Zone(Loop).Name);
                SetupOutputVariable("Zone Air Temperature", OutputProcessor::Unit::C, ZT(Loop), "System", "Average", Zone(Loop).Name);
                SetupOutputVariable(
                    "Zone Thermostat Air Temperature", OutputProcessor::Unit::C, TempTstatAir(Loop), "System", "Average", Zone(Loop).Name);
                SetupOutputVariable(
                    "Zone Air Humidity Ratio", OutputProcessor::Unit::None, ZoneAirHumRat(Loop), "System", "Average", Zone(Loop).Name);
                SetupOutputVariable(
                    "Zone Air Relative Humidity", OutputProcessor::Unit::Perc, dataZoneTempPredictorCorrector.ZoneAirRelHum(Loop), "System", "Average", Zone(Loop).Name);

                // The following output variables are for the predicted Heating/Cooling load for the zone which can be compared to actual load.
                // There are two sets of data available: one where zone and group multipliers have been applied and another where the multipliers have not.
                // First, these report variables are NOT multiplied by zone and group multipliers
                SetupOutputVariable("Zone Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    SNLoadPredictedRate(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    SNLoadPredictedHSPRate(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    SNLoadPredictedCSPRate(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                //Second, these report variable ARE multiplied by zone and group multipliers
                SetupOutputVariable("Zone System Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    ZoneSysEnergyDemand(Loop).TotalOutputRequired,
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone System Predicted Sensible Load to Heating Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    ZoneSysEnergyDemand(Loop).OutputRequiredToHeatingSP,
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone System Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    ZoneSysEnergyDemand(Loop).OutputRequiredToCoolingSP,
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);

                // The following output variables are for the predicted moisture load for the zone with humidity controlled specified.
                // There are two sets of data available: one where zone and group multipliers have been applied and another where the multipliers have not.
                // First, these report variables are NOT multiplied by zone and group multipliers
                SetupOutputVariable("Zone Predicted Moisture Load Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    MoisturePredictedRate(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    MoisturePredictedHumSPRate(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    MoisturePredictedDehumSPRate(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                //Second, these report variable ARE multiplied by zone and group multipliers
                SetupOutputVariable("Zone System Predicted Moisture Load Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    ZoneSysMoistureDemand(Loop).TotalOutputRequired,
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone System Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    ZoneSysMoistureDemand(Loop).OutputRequiredToHumidifyingSP,
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone System Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate",
                                    OutputProcessor::Unit::kgWater_s,
                                    ZoneSysMoistureDemand(Loop).OutputRequiredToDehumidifyingSP,
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);

                SetupOutputVariable(
                    "Zone Thermostat Control Type", OutputProcessor::Unit::None, TempControlType(Loop), "Zone", "Average", Zone(Loop).Name);
                SetupOutputVariable("Zone Thermostat Heating Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    ZoneThermostatSetPointLo(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Thermostat Cooling Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    ZoneThermostatSetPointHi(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Adaptive Comfort Operative Temperature Set Point",
                                    OutputProcessor::Unit::C,
                                    AdapComfortCoolingSetPoint(Loop),
                                    "Zone",
                                    "Average",
                                    Zone(Loop).Name);
                SetupOutputVariable("Zone Predicted Sensible Load Room Air Correction Factor",
                                    OutputProcessor::Unit::None,
                                    LoadCorrectionFactor(Loop),
                                    "System",
                                    "Average",
                                    Zone(Loop).Name);

                if (allocated(StageZoneLogic)) {
                    if (StageZoneLogic(Loop)) {
                        SetupOutputVariable("Zone Thermostat Staged Number",
                                            OutputProcessor::Unit::None,
                                            ZoneSysEnergyDemand(Loop).StageNum,
                                            "System",
                                            "Average",
                                            Zone(Loop).Name);
                    }
                }

            } // Loop

            // Thermal comfort control output
            if (NumComfortControlledZones > 0) {
                // CurrentModuleObject='ZoneControl:Thermostat:ThermalComfort'
                for (Loop = 1; Loop <= NumComfortControlledZones; ++Loop) {
                    ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;
                    SetupOutputVariable("Zone Thermal Comfort Control Type",
                                        OutputProcessor::Unit::None,
                                        ComfortControlType(ZoneNum),
                                        "Zone",
                                        "Average",
                                        Zone(ZoneNum).Name);
                    SetupOutputVariable("Zone Thermal Comfort Control Fanger Low Setpoint PMV",
                                        OutputProcessor::Unit::None,
                                        ZoneComfortControlsFanger(ZoneNum).LowPMV,
                                        "Zone",
                                        "Average",
                                        Zone(ZoneNum).Name);
                    SetupOutputVariable("Zone Thermal Comfort Control Fanger High Setpoint PMV",
                                        OutputProcessor::Unit::None,
                                        ZoneComfortControlsFanger(ZoneNum).HighPMV,
                                        "Zone",
                                        "Average",
                                        Zone(ZoneNum).Name);
                }
            }

            // CurrentModuleObject='ZoneList'
            for (Loop = 1; Loop <= NumOfZoneLists; ++Loop) {
                SetupOutputVariable(
                    "Zone List Sensible Heating Energy", OutputProcessor::Unit::J, ListSNLoadHeatEnergy(Loop), "System", "Sum", ZoneList(Loop).Name);
                SetupOutputVariable(
                    "Zone List Sensible Cooling Energy", OutputProcessor::Unit::J, ListSNLoadCoolEnergy(Loop), "System", "Sum", ZoneList(Loop).Name);
                SetupOutputVariable(
                    "Zone List Sensible Heating Rate", OutputProcessor::Unit::W, ListSNLoadHeatRate(Loop), "System", "Average", ZoneList(Loop).Name);
                SetupOutputVariable(
                    "Zone List Sensible Cooling Rate", OutputProcessor::Unit::W, ListSNLoadCoolRate(Loop), "System", "Average", ZoneList(Loop).Name);
            } // Loop

            // CurrentModuleObject='ZoneGroup'
            for (Loop = 1; Loop <= NumOfZoneGroups; ++Loop) {
                SetupOutputVariable("Zone Group Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    GroupSNLoadHeatEnergy(Loop),
                                    "System",
                                    "Sum",
                                    ZoneGroup(Loop).Name);
                SetupOutputVariable("Zone Group Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    GroupSNLoadCoolEnergy(Loop),
                                    "System",
                                    "Sum",
                                    ZoneGroup(Loop).Name);
                SetupOutputVariable("Zone Group Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    GroupSNLoadHeatRate(Loop),
                                    "System",
                                    "Average",
                                    ZoneGroup(Loop).Name);
                SetupOutputVariable("Zone Group Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    GroupSNLoadCoolRate(Loop),
                                    "System",
                                    "Average",
                                    ZoneGroup(Loop).Name);
            } // Loop

            dataZoneTempPredictorCorrector.InitZoneAirSetPointsOneTimeFlag = false;
        }

        // Do the Begin Environment initializations
        if (dataZoneTempPredictorCorrector.MyEnvrnFlag && BeginEnvrnFlag) {
            AIRRAT = 0.0;
            ZTM1 = 0.0;
            ZTM2 = 0.0;
            ZTM3 = 0.0;
            WZoneTimeMinus1 = OutHumRat;
            WZoneTimeMinus2 = OutHumRat;
            WZoneTimeMinus3 = OutHumRat;
            WZoneTimeMinus4 = OutHumRat;
            WZoneTimeMinusP = OutHumRat;
            DSWZoneTimeMinus1 = OutHumRat;
            DSWZoneTimeMinus2 = OutHumRat;
            DSWZoneTimeMinus3 = OutHumRat;
            DSWZoneTimeMinus4 = OutHumRat;
            WZoneTimeMinus1Temp = 0.0;
            WZoneTimeMinus2Temp = 0.0;
            WZoneTimeMinus3Temp = 0.0;
            ZoneAirHumRatTemp = 0.0;
            TempZoneThermostatSetPoint = 0.0;
            AdapComfortCoolingSetPoint = 0.0;
            ZoneThermostatSetPointHi = 0.0;
            ZoneThermostatSetPointLo = 0.0;

            LoadCorrectionFactor = 1.0; // PH 3/3/04
            TempControlType = 0;
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

            DeadBandOrSetback = false;
            SNLoadHeatEnergy = 0.0;
            SNLoadCoolEnergy = 0.0;
            SNLoadHeatRate = 0.0;
            SNLoadCoolRate = 0.0;
            SNLoadPredictedRate = 0.0;
            SNLoadPredictedHSPRate = 0.0;
            SNLoadPredictedCSPRate = 0.0;
            MoisturePredictedRate = 0.0;
            MoisturePredictedHumSPRate = 0.0;
            MoisturePredictedDehumSPRate = 0.0;

            dataZoneTempPredictorCorrector.TempIndZnLd = 0.0;
            dataZoneTempPredictorCorrector.TempDepZnLd = 0.0;
            NonAirSystemResponse = 0.0;
            SysDepZoneLoads = 0.0;
            SysDepZoneLoadsLagged = 0.0;
            dataZoneTempPredictorCorrector.ZoneAirRelHum = 0.0;
            for (auto &e : Zone)
                e.NoHeatToReturnAir = false;
            ZoneT1 = 0.0;
            ZoneW1 = OutHumRat;
            ZoneWMX = OutHumRat;
            ZoneWM2 = OutHumRat;
            PreviousMeasuredZT1 = 0.0;     // Hybrid modeling
            PreviousMeasuredZT2 = 0.0;     // Hybrid modeling
            PreviousMeasuredZT3 = 0.0;     // Hybrid modeling
            PreviousMeasuredHumRat1 = 0.0; // Hybrid modeling
            PreviousMeasuredHumRat2 = 0.0; // Hybrid modeling
            PreviousMeasuredHumRat3 = 0.0; // Hybrid modeling

            dataZoneTempPredictorCorrector.MyEnvrnFlag = false;
        }

        if (!BeginEnvrnFlag) {
            dataZoneTempPredictorCorrector.MyEnvrnFlag = true;
        }

        // Do the Begin Day initializations
        if (dataZoneTempPredictorCorrector.MyDayFlag && BeginDayFlag) {
            dataZoneTempPredictorCorrector.MyDayFlag = false;
        }

        if (!BeginDayFlag) {
            dataZoneTempPredictorCorrector.MyDayFlag = true;
        }

        for (Loop = 1; Loop <= NumTempControlledZones; ++Loop) {
            if (ZoneEquipInputsFilled && !dataZoneTempPredictorCorrector.ControlledZonesChecked) {
                if (!VerifyControlledZoneForThermostat(TempControlledZone(Loop).ZoneName)) {
                    ShowSevereError(RoutineName + "Zone=\"" + TempControlledZone(Loop).ZoneName +
                                    "\" has specified a Thermostatic control but is not a controlled zone.");
                    ShowContinueError("...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                    dataZoneTempPredictorCorrector.ErrorsFound = true;
                }
            }

            if (TempControlledZone(Loop).ManageDemand) {
                ZoneNum = TempControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(TempControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        if (TempZoneThermostatSetPoint(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit) {
                            TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).HeatingResetLimit;
                            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                        }

                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        if (TempZoneThermostatSetPoint(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit) {
                            TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).CoolingResetLimit;
                            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                        }

                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        if ((TempZoneThermostatSetPoint(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit) ||
                            (TempZoneThermostatSetPoint(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit)) {

                            TempControlType(ZoneNum) = DualSetPointWithDeadBand;
                            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);

                            if (ZoneThermostatSetPointLo(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit)
                                ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).HeatingResetLimit;
                            if (ZoneThermostatSetPointHi(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit)
                                ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).CoolingResetLimit;
                        }

                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        if (ZoneThermostatSetPointLo(ZoneNum) > TempControlledZone(Loop).HeatingResetLimit)
                            ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).HeatingResetLimit;
                        if (ZoneThermostatSetPointHi(ZoneNum) < TempControlledZone(Loop).CoolingResetLimit)
                            ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).CoolingResetLimit;

                    } else {
                        // Do nothing
                    }
                }
            }
        }

        for (Loop = 1; Loop <= NumComfortControlledZones; ++Loop) {
            if (ZoneEquipInputsFilled && !dataZoneTempPredictorCorrector.ControlledZonesChecked) {
                if (!VerifyControlledZoneForThermostat(ComfortControlledZone(Loop).ZoneName)) {
                    ShowSevereError(RoutineName + "Zone=\"" + ComfortControlledZone(Loop).ZoneName +
                                    "\" has specified a Comfort control but is not a controlled zone.");
                    ShowContinueError("...must have a ZoneHVAC:EquipmentConnections specification for this zone.");
                    dataZoneTempPredictorCorrector.ErrorsFound = true;
                }
            }
            if (ComfortControlledZone(Loop).ManageDemand) {
                ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(ComfortControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        if (TempZoneThermostatSetPoint(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit) {
                            TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).HeatingResetLimit;
                            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                            TempControlType(ZoneNum) = SingleHeatingSetPoint;
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        if (TempZoneThermostatSetPoint(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit) {
                            TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).CoolingResetLimit;
                            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                            TempControlType(ZoneNum) = SingleCoolingSetPoint;
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        if ((TempZoneThermostatSetPoint(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit) ||
                            (TempZoneThermostatSetPoint(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit)) {

                            TempControlType(ZoneNum) = DualSetPointWithDeadBand;
                            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);
                            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum);

                            if (ZoneThermostatSetPointLo(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit)
                                ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).HeatingResetLimit;
                            if (ZoneThermostatSetPointHi(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit)
                                ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).CoolingResetLimit;
                        }

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        TempControlType(ZoneNum) = DualSetPointWithDeadBand;
                        if (ZoneThermostatSetPointLo(ZoneNum) >= ComfortControlledZone(Loop).HeatingResetLimit)
                            ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).HeatingResetLimit;
                        if (ZoneThermostatSetPointHi(ZoneNum) <= ComfortControlledZone(Loop).CoolingResetLimit)
                            ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).CoolingResetLimit;

                    } else {
                        // Do nothing
                    }
                }
            } // Demand manager
        }

        if (dataZoneTempPredictorCorrector.ErrorsFound) {
            ShowFatalError("InitZoneAirSetpoints - program terminates due to previous condition.");
        }

        if (ZoneEquipInputsFilled) {
            dataZoneTempPredictorCorrector.ControlledZonesChecked = true;
        }
    }

    void PredictSystemLoads(EnergyPlusData &state, ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, bool const ShortenTimeStepSys,
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

        // USE STATEMENTS:

        // Using/Aliasing
        using DataLoopNode::Node;
        using DataRoomAirModel::AirModel;
        using DataRoomAirModel::IsZoneDV;
        using DataRoomAirModel::IsZoneUI;
        using DataRoomAirModel::MATFloor;
        using DataRoomAirModel::MATMX;
        using DataRoomAirModel::MATOC;
        using DataRoomAirModel::RoomAirModel_Mixing;
        using DataRoomAirModel::XM2TFloor;
        using DataRoomAirModel::XM2TMX;
        using DataRoomAirModel::XM2TOC;
        using DataRoomAirModel::XM3TFloor;
        using DataRoomAirModel::XM3TMX;
        using DataRoomAirModel::XM3TOC;
        using DataRoomAirModel::XM4TFloor;
        using DataRoomAirModel::XM4TMX;
        using DataRoomAirModel::XM4TOC;
        using DataRoomAirModel::XMATFloor;
        using DataRoomAirModel::XMATMX;
        using DataRoomAirModel::XMATOC;
        using DataRoomAirModel::ZoneDVMixedFlag;
        using DataRoomAirModel::ZTFloor;
        using DataRoomAirModel::ZTM1MX;
        using DataRoomAirModel::ZTM1OC;
        using DataRoomAirModel::ZTMX;
        using DataRoomAirModel::ZTOC;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;
        using RoomAirModelAirflowNetwork::LoadPredictionRoomAirModelAirflowNetwork;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

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
        if (dataZoneTempPredictorCorrector.NumStageCtrZone > 0) {
            for (RelativeZoneNum = 1; RelativeZoneNum <= dataZoneTempPredictorCorrector.NumStageCtrZone; ++RelativeZoneNum) {
                ActualZoneNum = StageControlledZone(RelativeZoneNum).ActualZoneNum;
                ZoneT = MAT(ActualZoneNum);
                if (ShortenTimeStepSys) ZoneT = XMPT(ActualZoneNum);
                StageControlledZone(RelativeZoneNum).HeatSetPoint = GetCurrentScheduleValue(StageControlledZone(RelativeZoneNum).HSBchedIndex);
                StageControlledZone(RelativeZoneNum).CoolSetPoint = GetCurrentScheduleValue(StageControlledZone(RelativeZoneNum).CSBchedIndex);
                if (StageControlledZone(RelativeZoneNum).HeatSetPoint >= StageControlledZone(RelativeZoneNum).CoolSetPoint) {
                    ++StageControlledZone(RelativeZoneNum).StageErrCount;
                    if (StageControlledZone(RelativeZoneNum).StageErrCount < 2) {
                        ShowWarningError(
                            "ZoneControl:Thermostat:StagedDualSetpoint: The heating setpoint is equal to or above the cooling setpoint in " +
                            StageControlledZone(RelativeZoneNum).Name);
                        ShowContinueError("The zone heating setpoint is set to the cooling setpoint - 0.1C.");
                        ShowContinueErrorTimeStamp("Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd("The heating setpoint is still above the cooling setpoint",
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
        if (dataZoneTempPredictorCorrector.NumOnOffCtrZone > 0) {
            Real64 TempTole = 0.02;
            Real64 Tprev;
            for (RelativeZoneNum = 1; RelativeZoneNum <= NumTempControlledZones; ++RelativeZoneNum) {
                if (TempControlledZone(RelativeZoneNum).DeltaTCutSet > 0.0) {
                    if (ShortenTimeStepSys) {
                        TempControlledZone(RelativeZoneNum).HeatModeLast = TempControlledZone(RelativeZoneNum).HeatModeLastSave;
                        TempControlledZone(RelativeZoneNum).CoolModeLast = TempControlledZone(RelativeZoneNum).CoolModeLastSave;
                    } else {
                        TempControlledZone(RelativeZoneNum).HeatModeLastSave = TempControlledZone(RelativeZoneNum).HeatModeLast;
                        TempControlledZone(RelativeZoneNum).CoolModeLastSave = TempControlledZone(RelativeZoneNum).CoolModeLast;
                    }
                    ZoneNum = TempControlledZone(RelativeZoneNum).ActualZoneNum;
                    {
                        auto const SELECT_CASE_var(TempControlType(ZoneNum));
                        TempControlledZone(RelativeZoneNum).CoolOffFlag = false;
                        TempControlledZone(RelativeZoneNum).HeatOffFlag = false;
                        if (ZoneAirSolutionAlgo == Use3rdOrder) {
                            Tprev = MAT(ZoneNum);
                            if (ShortenTimeStepSys) Tprev = XMPT(ZoneNum);
                        } else {
                            Tprev = ZoneT1(ZoneNum);
                        }

                        if (SELECT_CASE_var == SingleHeatingSetPoint) {
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
                            if (TempControlledZone(RelativeZoneNum).HeatModeLast &&
                                Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo) {
                                TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                                ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                                TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                            }
                        } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
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
                            if (TempControlledZone(RelativeZoneNum).CoolModeLast &&
                                Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi) {
                                TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                                ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi;
                                TempControlledZone(RelativeZoneNum).CoolOffFlag = true;
                            }

                        } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
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
                            if (TempControlledZone(RelativeZoneNum).CoolModeLast &&
                                Tprev < TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi) {
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
                            if (TempControlledZone(RelativeZoneNum).HeatModeLast &&
                                Tprev > TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo) {
                                ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo;
                                TempControlledZone(RelativeZoneNum).HeatOffFlag = true;
                            }
                            // check setpoint for both and provde an error message
                            if (ZoneThermostatSetPointLo(ZoneNum) >= ZoneThermostatSetPointHi(ZoneNum)) {
                                ShowSevereError(
                                    "DualSetPointWithDeadBand: When Temperature Difference Between Cutout And Setpoint is applied, the heating "
                                    "setpoint is greater than the cooling setpoint. ");
                                ShowContinueErrorTimeStamp("occurs in Zone=" + Zone(ZoneNum).Name);
                                ShowContinueError("Zone Heating ThermostatSetPoint=" + RoundSigDigits(ZoneThermostatSetPointLo(ZoneNum), 2));
                                ShowContinueError("Zone Cooling ThermostatSetPoint=" + RoundSigDigits(ZoneThermostatSetPointHi(ZoneNum), 2));
                                ShowFatalError("Program terminates due to above conditions.");
                            }
                        }
                    }
                }
            }
        }

        // Update zone temperatures
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            if (ShortenTimeStepSys) {
                // timestep has just shifted from full zone timestep to a new shorter system timestep
                // throw away last updates in corrector and rewind for resimulating smaller timestep
                if (Zone(ZoneNum).SystemZoneNodeNumber > 0) { // roll back result for zone air node,
                    Node(Zone(ZoneNum).SystemZoneNodeNumber).Temp = XMAT(ZoneNum);
                    TempTstatAir(ZoneNum) = XMAT(ZoneNum);
                    Node(Zone(ZoneNum).SystemZoneNodeNumber).HumRat = WZoneTimeMinus1(ZoneNum);
                    Node(Zone(ZoneNum).SystemZoneNodeNumber).Enthalpy = PsyHFnTdbW(XMAT(ZoneNum), WZoneTimeMinus1(ZoneNum));
                }

                if (NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time

                    //  MAT(ZoneNum),   XMAT(ZoneNum),   XM2T(ZoneNum),   XM3T(ZoneNum),   XM4T(ZoneNum), &
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  XMAT(ZoneNum),
                                                  XM2T(ZoneNum),
                                                  XM3T(ZoneNum),
                                                  XM4T(ZoneNum),
                                                  XM4T(ZoneNum),
                                                  MAT(ZoneNum),
                                                  DSXMAT(ZoneNum),
                                                  DSXM2T(ZoneNum),
                                                  DSXM3T(ZoneNum),
                                                  DSXM4T(ZoneNum));
                    //     ZoneAirHumRat(ZoneNum),   WZoneTimeMinus1(ZoneNum),   WZoneTimeMinus2(ZoneNum),   &
                    //                                 WZoneTimeMinus3(ZoneNum),   WZoneTimeMinus4(ZoneNum), &
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  WZoneTimeMinus1(ZoneNum),
                                                  WZoneTimeMinus2(ZoneNum),
                                                  WZoneTimeMinus3(ZoneNum),
                                                  WZoneTimeMinus4(ZoneNum),
                                                  WZoneTimeMinus4(ZoneNum),
                                                  ZoneAirHumRat(ZoneNum),
                                                  DSWZoneTimeMinus1(ZoneNum),
                                                  DSWZoneTimeMinus2(ZoneNum),
                                                  DSWZoneTimeMinus3(ZoneNum),
                                                  DSWZoneTimeMinus4(ZoneNum));

                    if (IsZoneDV(ZoneNum) || IsZoneUI(ZoneNum)) {

                        //   MATFloor(ZoneNum),   XMATFloor(ZoneNum),    XM2TFloor(ZoneNum),  &
                        //                        XM3TFloor(ZoneNum),    XM4TFloor(ZoneNum) ,   &
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      XMATFloor(ZoneNum),
                                                      XM2TFloor(ZoneNum),
                                                      XM3TFloor(ZoneNum),
                                                      XM4TFloor(ZoneNum),
                                                      XM4TFloor(ZoneNum),
                                                      MATFloor(ZoneNum),
                                                      DSXMATFloor(ZoneNum),
                                                      DSXM2TFloor(ZoneNum),
                                                      DSXM3TFloor(ZoneNum),
                                                      DSXM4TFloor(ZoneNum));
                        //      MATOC(ZoneNum),   XMATOC(ZoneNum),    XM2TOC(ZoneNum),  &
                        //                        XM3TOC(ZoneNum),    XM4TOC(ZoneNum) ,   &
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      XMATOC(ZoneNum),
                                                      XM2TOC(ZoneNum),
                                                      XM3TOC(ZoneNum),
                                                      XM4TOC(ZoneNum),
                                                      XM4TOC(ZoneNum),
                                                      MATOC(ZoneNum),
                                                      DSXMATOC(ZoneNum),
                                                      DSXM2TOC(ZoneNum),
                                                      DSXM3TOC(ZoneNum),
                                                      DSXM4TOC(ZoneNum));
                        //  MATMX(ZoneNum),   XMATMX(ZoneNum),    XM2TMX(ZoneNum),  &
                        //                    XM3TMX(ZoneNum),    XM4TMX(ZoneNum) ,   &
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      XMATMX(ZoneNum),
                                                      XM2TMX(ZoneNum),
                                                      XM3TMX(ZoneNum),
                                                      XM4TMX(ZoneNum),
                                                      XM4TMX(ZoneNum),
                                                      MATMX(ZoneNum),
                                                      DSXMATMX(ZoneNum),
                                                      DSXM2TMX(ZoneNum),
                                                      DSXM3TMX(ZoneNum),
                                                      DSXM4TMX(ZoneNum));
                    }
                    if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
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
                ZTM1(ZoneNum) = XMAT(ZoneNum);
                ZTM2(ZoneNum) = XM2T(ZoneNum);
                ZTM3(ZoneNum) = XM3T(ZoneNum);

                WZoneTimeMinus1Temp(ZoneNum) = WZoneTimeMinus1(ZoneNum);
                WZoneTimeMinus2Temp(ZoneNum) = WZoneTimeMinus2(ZoneNum);
                WZoneTimeMinus3Temp(ZoneNum) = WZoneTimeMinus3(ZoneNum);

            } else { // use down-stepped history
                ZTM1(ZoneNum) = DSXMAT(ZoneNum);
                ZTM2(ZoneNum) = DSXM2T(ZoneNum);
                ZTM3(ZoneNum) = DSXM3T(ZoneNum);

                WZoneTimeMinus1Temp(ZoneNum) = DSWZoneTimeMinus1(ZoneNum);
                WZoneTimeMinus2Temp(ZoneNum) = DSWZoneTimeMinus2(ZoneNum);
                WZoneTimeMinus3Temp(ZoneNum) = DSWZoneTimeMinus3(ZoneNum);
            }

            AIRRAT(ZoneNum) = Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpSens *
                              PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneNum), ZoneAirHumRat(ZoneNum)) * PsyCpAirFnW(ZoneAirHumRat(ZoneNum)) /
                              (TimeStepSys * SecInHour);
            AirCap = AIRRAT(ZoneNum);
            RAFNFrac = 0.0;

            // Calculate the various heat balance sums

            // NOTE: SumSysMCp and SumSysMCpT are not used in the predict step
            CalcZoneSums(state.dataZonePlenum, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, false);

            // Sum all convective internal gains except for people: SumIntGainExceptPeople
            if (HybridModel::FlagHybridModel_PC) {
                SumAllInternalConvectionGainsExceptPeople(ZoneNum, SumIntGainExceptPeople);
            }

            TempDepCoef = SumHA + SumMCp;
            TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SysDepZoneLoadsLagged(ZoneNum);
            if (AirModel(ZoneNum).AirModelType == RoomAirModel_Mixing) {
                TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum));
                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            } else if (IsZoneDV(ZoneNum)) {
                // UCSD displacement ventilation model - make dynamic term independent of TimeStepSys
                TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum));
                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            } else if (IsZoneUI(ZoneNum)) {
                // UCSD UFAD model - make dynamic term independent of TimeStepSys
                TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum));
                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            } else if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                // RoomAirflowNetworkModel - make dynamic term independent of TimeStepSys
                if (RoomAirflowNetworkZoneInfo(ZoneNum).IsUsed) {
                    RoomAirNode = RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
                    LoadPredictionRoomAirModelAirflowNetwork(state, ZoneNum, RoomAirNode);
                    TempDepCoef = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA +
                                  RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp;
                    TempIndCoef = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntSensibleGain +
                                  RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf -
                                  RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref +
                                  RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT +
                                  RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SysDepZoneLoadsLagged;
                    AirCap = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume * Zone(ZoneNum).ZoneVolCapMultpSens *
                             RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir *
                             RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).CpAir / (TimeStepSys * SecInHour);
                    AIRRAT(ZoneNum) = AirCap;
                    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum));
                    dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                    dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
                    if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HasHVACAssigned)
                        RAFNFrac = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HVAC(1).SupplyFraction;
                }
            } else { // other imperfectly mixed room models
                TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum));
                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) = (11.0 / 6.0) * AirCap + TempDepCoef;
                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef;
            }

            // Exact solution or Euler method
            ShortenTimeStepSysRoomAir = false;
            if (ZoneAirSolutionAlgo != Use3rdOrder) {
                if (ShortenTimeStepSys && TimeStepSys < TimeStepZone) {
                    if (PreviousTimeStep < TimeStepZone) {
                        ZoneT1(ZoneNum) = ZoneTM2(ZoneNum);
                        ZoneW1(ZoneNum) = ZoneWM2(ZoneNum);
                        if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                            for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 =
                                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2;
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 =
                                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2;
                            }
                        }
                    } else {
                        ZoneT1(ZoneNum) = ZoneTMX(ZoneNum);
                        ZoneW1(ZoneNum) = ZoneWMX(ZoneNum);
                        if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                            for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 =
                                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                                RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 =
                                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                            }
                        }
                    }
                    ShortenTimeStepSysRoomAir = true;
                } else {
                    ZoneT1(ZoneNum) = ZT(ZoneNum);
                    ZoneW1(ZoneNum) = ZoneAirHumRat(ZoneNum);
                    if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                        for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempT1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatW1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                        }
                    }
                }
                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) = TempDepCoef;
                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum) = TempIndCoef;
            }

            // Calculate the predicted zone load to be provided by the system with the given desired zone air temperature
            CalcPredictedSystemLoad(dataZoneTempPredictorCorrector, ZoneNum, RAFNFrac);

            // Calculate the predicted zone load to be provided by the system with the given desired humidity ratio
            CalcPredictedHumidityRatio(ZoneNum, RAFNFrac);
        }

        if (dataZoneTempPredictorCorrector.NumOnOffCtrZone > 0) {
            for (RelativeZoneNum = 1; RelativeZoneNum <= NumTempControlledZones; ++RelativeZoneNum) {
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

    void CalcZoneAirTempSetPoints(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, IOFiles &ioFiles)
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

        // Using/Aliasing
        using DataZoneControls::OccRoomTSetPointCool;
        using DataZoneControls::OccRoomTSetPointHeat;
        using General::TrimSigDigits;
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

        // FLOW:
        TempControlType = 0; // Default

        // Place holder for occupied heating and cooling set points - for optimum start
        if (!allocated(OccRoomTSetPointHeat)) {
            OccRoomTSetPointHeat.allocate(NumOfZones);
        }
        if (!allocated(OccRoomTSetPointCool)) {
            OccRoomTSetPointCool.allocate(NumOfZones);
        }
        OccRoomTSetPointHeat = 0.0;
        OccRoomTSetPointCool = 100.0;
        DeltaT = 0.0;

        for (RelativeZoneNum = 1; RelativeZoneNum <= NumTempControlledZones; ++RelativeZoneNum) {

            // What if this zone not controlled???
            ActualZoneNum = TempControlledZone(RelativeZoneNum).ActualZoneNum;
            TempControlSchedIndex = TempControlledZone(RelativeZoneNum).CTSchedIndex;
            TempControlType(ActualZoneNum) = GetCurrentScheduleValue(TempControlSchedIndex);
            // Error detection for these values is done in the Get routine

            {
                auto const SELECT_CASE_var(
                    TempControlType(ActualZoneNum)); // Is this missing the possibility of sometimes having no control on a zone
                // during the simulation?
                if (SELECT_CASE_var == 0) { // Uncontrolled

                } else if (SELECT_CASE_var == SingleHeatingSetPoint) {

                    SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatSetPoint;

                    SchedTypeIndex = TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndex = dataZoneTempPredictorCorrector.SetPointSingleHeating(SchedTypeIndex).TempSchedIndex;
                    TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndex);
                    TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = TempZoneThermostatSetPoint(ActualZoneNum);

                    AdjustAirSetPointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
                    ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    //        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

                } else if (SELECT_CASE_var == SingleCoolingSetPoint) {

                    SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleCoolSetPoint;

                    SchedTypeIndex = TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndex = dataZoneTempPredictorCorrector.SetPointSingleCooling(SchedTypeIndex).TempSchedIndex;
                    TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndex);
                    TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = TempZoneThermostatSetPoint(ActualZoneNum);

                    // Added Jan 17 (X. Luo)
                    // Adjust operative temperature based on adaptive comfort model
                    if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                        AdjustOperativeSetPointsforAdapComfort(dataZoneTempPredictorCorrector, RelativeZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
                        AdapComfortCoolingSetPoint(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    }

                    AdjustAirSetPointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
                    ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    //        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

                    AdjustCoolingSetPointforTempAndHumidityControl(dataZoneTempPredictorCorrector, RelativeZoneNum, ActualZoneNum);

                } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {

                    SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_SingleHeatCoolSetPoint;

                    SchedTypeIndex = TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndex = dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SchedTypeIndex).TempSchedIndex;
                    TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndex);

                    // Added Jan 17 (X. Luo)
                    // Adjust operative temperature based on adaptive comfort model
                    if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                        AdjustOperativeSetPointsforAdapComfort(dataZoneTempPredictorCorrector, RelativeZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));
                        AdapComfortCoolingSetPoint(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    }

                    AdjustAirSetPointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum));

                    ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);

                    // Change the room set point to occupied set point during optimum start period--------------

                    if (allocated(OptStartData.OptStartFlag)) {
                        if (!allocated(DaySPValues)) {
                            DaySPValues.allocate(NumOfTimeStepInHour, 24);
                        }
                        if (OptStartData.ActualZoneNum(ActualZoneNum) == ActualZoneNum) {
                            GetScheduleValuesForDay(SetPointTempSchedIndexCold, DaySPValues);
                            OccStartTime = CEILING(OptStartData.OccStartTime(ActualZoneNum)) + 1;
                            TempZoneThermostatSetPoint(ActualZoneNum) = DaySPValues(1, OccStartTime);
                        }

                        if (OptStartData.OptStartFlag(ActualZoneNum)) {
                            ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                            ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                        }
                    }
                    //--------------------------------------------------------------------------------------------

                } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {

                    SchedNameIndex = TempControlledZone(RelativeZoneNum).SchIndx_DualSetPointWDeadBand;

                    SchedTypeIndex = TempControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);

                    SetPointTempSchedIndexHot = dataZoneTempPredictorCorrector.SetPointDualHeatCool(SchedTypeIndex).HeatTempSchedIndex;
                    SetPointTempSchedIndexCold = dataZoneTempPredictorCorrector.SetPointDualHeatCool(SchedTypeIndex).CoolTempSchedIndex;
                    ZoneThermostatSetPointHi(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndexCold);
                    TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointHi = ZoneThermostatSetPointHi(ActualZoneNum);

                    // Added Jan 17 (X. Luo)
                    // Adjust operative temperature based on adaptive comfort model
                    if ((TempControlledZone(RelativeZoneNum).AdaptiveComfortTempControl)) {
                        AdjustOperativeSetPointsforAdapComfort(dataZoneTempPredictorCorrector, RelativeZoneNum, ZoneThermostatSetPointHi(ActualZoneNum));
                        AdapComfortCoolingSetPoint(ActualZoneNum) = ZoneThermostatSetPointHi(ActualZoneNum);
                    }

                    AdjustAirSetPointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointHi(ActualZoneNum));

                    ZoneThermostatSetPointLo(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndexHot);
                    TempControlledZone(RelativeZoneNum).ZoneThermostatSetPointLo = ZoneThermostatSetPointLo(ActualZoneNum);
                    AdjustAirSetPointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointLo(ActualZoneNum));

                    // Change the room set point to occupied set point during optimum start period--------------

                    if (allocated(OptStartData.OptStartFlag)) {
                        if (!allocated(DaySPValues)) {
                            DaySPValues.allocate(NumOfTimeStepInHour, 24);
                        }
                        if (OptStartData.ActualZoneNum(ActualZoneNum) == ActualZoneNum) {
                            GetScheduleValuesForDay(SetPointTempSchedIndexCold, DaySPValues);
                            OccStartTime = CEILING(OptStartData.OccStartTime(ActualZoneNum)) + 1;
                            OccRoomTSetPointCool(ActualZoneNum) = DaySPValues(1, OccStartTime);
                            GetScheduleValuesForDay(SetPointTempSchedIndexHot, DaySPValues);
                            OccRoomTSetPointHeat(ActualZoneNum) = DaySPValues(1, OccStartTime);
                        }

                        if (OptStartData.OptStartFlag(ActualZoneNum)) {
                            ZoneThermostatSetPointHi(ActualZoneNum) = OccRoomTSetPointCool(ActualZoneNum);
                            ZoneThermostatSetPointLo(ActualZoneNum) = OccRoomTSetPointHeat(ActualZoneNum);
                        }
                    }
                    //--------------------------------------------------------------------------------------------

                    AdjustCoolingSetPointforTempAndHumidityControl(dataZoneTempPredictorCorrector, RelativeZoneNum, ActualZoneNum);

                } else {
                    ShowSevereError("CalcZoneAirTempSetpoints: Illegal control type for Zone=" + Zone(ActualZoneNum).Name +
                                    ", Found value=" + TrimSigDigits(TempControlType(ActualZoneNum)) +
                                    ", in Schedule=" + TempControlledZone(RelativeZoneNum).ControlTypeSchedName);
                }
            }

            // Apply offset for faulty therostats_Feb. 2015, zrp
            if ((NumFaultyThermostat > 0) && (!WarmupFlag) && (!DoingSizing) && (!KickOffSimulation)) {

                //  loop through the FaultsThermostatOffset objects to find the one for the zone
                for (int iFault = 1; iFault <= NumFaultyThermostat; ++iFault) {

                    if (UtilityRoutines::SameString(TempControlledZone(RelativeZoneNum).Name, FaultsThermostatOffset(iFault).FaultyThermostatName)) {

                        // Check fault availability schedules
                        if (GetCurrentScheduleValue(FaultsThermostatOffset(iFault).AvaiSchedPtr) > 0.0) {

                            // Check fault severity schedules to update the reference thermostat offset
                            double rSchVal = 1.0;
                            double offsetUpdated;
                            if (FaultsThermostatOffset(iFault).SeveritySchedPtr >= 0) {
                                rSchVal = GetCurrentScheduleValue(FaultsThermostatOffset(iFault).SeveritySchedPtr);
                            }
                            offsetUpdated = rSchVal * FaultsThermostatOffset(iFault).Offset;

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

        if (NumComfortControlledZones > 0) CalcZoneAirComfortSetPoints(dataZoneTempPredictorCorrector, ioFiles);
        OverrideAirSetPointsforEMSCntrl();
    }

    void CalcPredictedSystemLoad(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, int const ZoneNum, Real64 RAFNFrac)
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
        using General::RoundSigDigits;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 LoadToHeatingSetPoint;
        Real64 LoadToCoolingSetPoint;
        Real64 ZoneSetPoint;

        // FLOW:
        DeadBandOrSetback(ZoneNum) = false;
        ZoneSetPoint = 0.0;
        LoadToHeatingSetPoint = 0.0;
        LoadToCoolingSetPoint = 0.0;

        {
            auto const SELECT_CASE_var(TempControlType(ZoneNum));

            if (SELECT_CASE_var == 0) {
                // Uncontrolled Zone
                LoadToHeatingSetPoint = 0.0;
                LoadToCoolingSetPoint = 0.0;
                ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;

            } else if (SELECT_CASE_var == SingleHeatingSetPoint) {

                // PH 3/2/04      LoadToHeatingSetPoint = TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum)
                if (ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToHeatingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    }
                } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                        dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                }
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum);
                LoadToCoolingSetPoint = LoadToHeatingSetPoint;
                // for consistency with the other cases, use LE instead of LT and don't subtract 1.0 Watt as a way of pushing the zero load
                // case over the threshold
                if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) <= 0.0) DeadBandOrSetback(ZoneNum) = true;

            } else if (SELECT_CASE_var == SingleCoolingSetPoint) {

                // PH 3/2/04      LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
                if (ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToCoolingSetPoint = dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                        LoadToCoolingSetPoint =
                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    }
                } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                }
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                if (DataHeatBalance::Zone(ZoneNum).HasAdjustedReturnTempByITE && !(DataGlobals::BeginSimFlag)) {
                    LoadToCoolingSetPoint = dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * DataHeatBalance::Zone(ZoneNum).AdjustedReturnTempByITE - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                }
                ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum);
                LoadToHeatingSetPoint = LoadToCoolingSetPoint;
                // for consistency with the other cases, use GE instead of GT and don't add 1.0 Watt as a way of pushing the zero load
                // case over the threshold
                if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) >= 0.0) DeadBandOrSetback(ZoneNum) = true;

            } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {

                // PH 3/2/04      LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
                // PH 3/2/04      !LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
                if (ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToHeatingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                    LoadToCoolingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint =
                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    }
                } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (TempZoneThermostatSetPoint(ZoneNum) - ZoneT1(ZoneNum)) +
                                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                }
                ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum);
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                if (RAFNFrac > 0.0) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

                if (DataHeatBalance::Zone(ZoneNum).HasAdjustedReturnTempByITE && !(DataGlobals::BeginSimFlag)) {
                    LoadToCoolingSetPoint = dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * DataHeatBalance::Zone(ZoneNum).AdjustedReturnTempByITE - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
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
                    ShowSevereError("SingleHeatCoolSetPoint: Effective heating set-point higher than effective cooling set-point - use "
                                    "DualSetPointWithDeadBand if using unmixed air model");
                    ShowContinueErrorTimeStamp("occurs in Zone=" + Zone(ZoneNum).Name);
                    ShowContinueError("LoadToHeatingSetPoint=" + RoundSigDigits(LoadToHeatingSetPoint, 3) +
                                      ", LoadToCoolingSetPoint=" + RoundSigDigits(LoadToCoolingSetPoint, 3));
                    ShowContinueError("Zone TempDepZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum), 2));
                    ShowContinueError("Zone TempIndZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum), 2));
                    ShowContinueError("Zone ThermostatSetPoint=" + RoundSigDigits(TempZoneThermostatSetPoint(ZoneNum), 2));
                    ShowFatalError("Program terminates due to above conditions.");
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
                    DeadBandOrSetback(ZoneNum) = true;
                } else { // this should never occur!
                    ShowSevereError(
                        "SingleHeatCoolSetPoint: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
                    ShowContinueErrorTimeStamp("occurs in Zone=" + Zone(ZoneNum).Name);
                    ShowContinueError("LoadToHeatingSetPoint=" + RoundSigDigits(LoadToHeatingSetPoint, 3) +
                                      ", LoadToCoolingSetPoint=" + RoundSigDigits(LoadToCoolingSetPoint, 3));
                    ShowContinueError("Zone TempDepZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum), 2));
                    ShowContinueError("Zone TempIndZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum), 2));
                    ShowContinueError("Zone ThermostatSetPoint=" + RoundSigDigits(TempZoneThermostatSetPoint(ZoneNum), 2));
                    ShowFatalError("Program terminates due to above conditions.");
                }

            } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {

                if (ZoneAirSolutionAlgo == Use3rdOrder) {
                    LoadToHeatingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                    LoadToCoolingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                    // Exact solution
                } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                    if (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) == 0.0) { // B=0
                        LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    } else {
                        Real64 const exp_700_TA(std::exp(min(700.0, -dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                        LoadToHeatingSetPoint =
                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        LoadToCoolingSetPoint =
                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                            dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    }
                } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                    LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) +
                                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * ZoneThermostatSetPointLo(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) +
                                            dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * ZoneThermostatSetPointHi(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                }
                if (RAFNFrac > 0.0) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
                if (RAFNFrac > 0.0) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

                if (DataHeatBalance::Zone(ZoneNum).HasAdjustedReturnTempByITE && !(DataGlobals::BeginSimFlag)) {
                    LoadToCoolingSetPoint = dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * DataHeatBalance::Zone(ZoneNum).AdjustedReturnTempByITE - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                }

                // Possible combinations:
                // 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
                // 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
                //                                                                  as a poor choice of set-points
                // 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
                // 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
                // First trap bad set-points
                if (LoadToHeatingSetPoint > LoadToCoolingSetPoint) {
                    ShowSevereError("DualSetPointWithDeadBand: Effective heating set-point higher than effective cooling set-point - increase "
                                    "deadband if using unmixed air model");
                    ShowContinueErrorTimeStamp("occurs in Zone=" + Zone(ZoneNum).Name);
                    ShowContinueError("LoadToHeatingSetPoint=" + RoundSigDigits(LoadToHeatingSetPoint, 3) +
                                      ", LoadToCoolingSetPoint=" + RoundSigDigits(LoadToCoolingSetPoint, 3));
                    ShowContinueError("Zone TempDepZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum), 2));
                    ShowContinueError("Zone TempIndZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum), 2));
                    ShowContinueError("Zone Heating ThermostatSetPoint=" + RoundSigDigits(ZoneThermostatSetPointLo(ZoneNum), 2));
                    ShowContinueError("Zone Cooling ThermostatSetPoint=" + RoundSigDigits(ZoneThermostatSetPointHi(ZoneNum), 2));
                    ShowFatalError("Program terminates due to above conditions.");
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
                    DeadBandOrSetback(ZoneNum) = true;
                } else { // this should never occur!
                    ShowSevereError(
                        "DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team");
                    ShowContinueErrorTimeStamp("occurs in Zone=" + Zone(ZoneNum).Name);
                    ShowContinueError("LoadToHeatingSetPoint=" + RoundSigDigits(LoadToHeatingSetPoint, 3) +
                                      ", LoadToCoolingSetPoint=" + RoundSigDigits(LoadToCoolingSetPoint, 3));
                    ShowContinueError("Zone Heating Set-point=" + RoundSigDigits(ZoneThermostatSetPointLo(ZoneNum), 2));
                    ShowContinueError("Zone Cooling Set-point=" + RoundSigDigits(ZoneThermostatSetPointHi(ZoneNum), 2));
                    ShowContinueError("Zone TempDepZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum), 2));
                    ShowContinueError("Zone TempIndZnLd=" + RoundSigDigits(dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum), 2));
                    ShowContinueError("Zone ThermostatSetPoint=" + RoundSigDigits(TempZoneThermostatSetPoint(ZoneNum), 2));

                    ShowFatalError("Program terminates due to above conditions.");
                }
            }
        }

        // Staged control zone
        if (dataZoneTempPredictorCorrector.NumStageCtrZone > 0) {
            if (StageZoneLogic(ZoneNum)) {
                if (ZoneSysEnergyDemand(ZoneNum).StageNum == 0) { // No load
                    LoadToHeatingSetPoint = 0.0;
                    LoadToCoolingSetPoint = 0.0;
                    ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = 0.0;
                    if (Zone(ZoneNum).SystemZoneNodeNumber > 0) {
                        ZoneSetPoint = Node(Zone(ZoneNum).SystemZoneNodeNumber).Temp;
                        ZoneSetPoint = max(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)); // trap out of deadband
                        ZoneSetPoint = min(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)); // trap out of deadband
                    }
                    DeadBandOrSetback(ZoneNum) = true;
                } else if (ZoneSysEnergyDemand(ZoneNum).StageNum < 0) { // Cooling load
                    if (ZoneAirSolutionAlgo == Use3rdOrder) {
                        LoadToCoolingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                    } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                        if (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) == 0.0) { // B=0
                            LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        } else {
                            Real64 const exp_700_TA(std::exp(min(700.0, -dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                            LoadToCoolingSetPoint =
                                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        }
                    } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                        LoadToCoolingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointHi(ZoneNum) - ZoneT1(ZoneNum)) +
                                                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * ZoneThermostatSetPointHi(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    }
                    ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToCoolingSetPoint;
                    ZoneSetPoint = ZoneThermostatSetPointHi(ZoneNum);
                    LoadToHeatingSetPoint = LoadToCoolingSetPoint;
                    if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) >= 0.0) DeadBandOrSetback(ZoneNum) = true;
                } else { // Heating load
                    if (ZoneAirSolutionAlgo == Use3rdOrder) {
                        LoadToHeatingSetPoint = (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * ZoneThermostatSetPointLo(ZoneNum) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum));
                        // Exact solution
                    } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                        if (dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) == 0.0) { // B=0
                            LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        } else {
                            Real64 const exp_700_TA(std::exp(min(700.0, -dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) / AIRRAT(ZoneNum))));
                            LoadToHeatingSetPoint =
                                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum) * exp_700_TA) / (1.0 - exp_700_TA) -
                                dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                        }
                    } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                        LoadToHeatingSetPoint = AIRRAT(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum) - ZoneT1(ZoneNum)) +
                                                dataZoneTempPredictorCorrector.TempDepZnLd(ZoneNum) * (ZoneThermostatSetPointLo(ZoneNum)) - dataZoneTempPredictorCorrector.TempIndZnLd(ZoneNum);
                    }
                    ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired = LoadToHeatingSetPoint;
                    ZoneSetPoint = ZoneThermostatSetPointLo(ZoneNum);
                    LoadToCoolingSetPoint = LoadToHeatingSetPoint;
                    if ((ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired) <= 0.0) DeadBandOrSetback(ZoneNum) = true;
                }
            }
        }

        // If the ZoneNodeNum has been set for a Controlled Zone, then the zone setpoint is placed on the node.
        if (Zone(ZoneNum).SystemZoneNodeNumber > 0) {
            Node(Zone(ZoneNum).SystemZoneNodeNumber).TempSetPoint = ZoneSetPoint;
        }

        if (ZoneSetPoint > dataZoneTempPredictorCorrector.ZoneSetPointLast(ZoneNum)) {
            Setback(ZoneNum) = true;
        } else {
            Setback(ZoneNum) = false;
        }

        dataZoneTempPredictorCorrector.ZoneSetPointLast(ZoneNum) = ZoneSetPoint;
        TempZoneThermostatSetPoint(ZoneNum) = ZoneSetPoint; // needed to fix Issue # 5048
        CurDeadBandOrSetback(ZoneNum) = DeadBandOrSetback(ZoneNum);

        // Apply the Zone Multiplier and Load Correction factor as needed
        ReportSensibleLoadsZoneMultiplier(ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired,
                                          ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP,
                                          ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP,
                                          SNLoadPredictedRate(ZoneNum),SNLoadPredictedHSPRate(ZoneNum),SNLoadPredictedCSPRate(ZoneNum),
                                          LoadToHeatingSetPoint,LoadToCoolingSetPoint,
                                          LoadCorrectionFactor(ZoneNum),Zone(ZoneNum).Multiplier,Zone(ZoneNum).ListMultiplier);

        // init each sequenced demand to the full output
        if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired))
            ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequired = ZoneSysEnergyDemand(ZoneNum).TotalOutputRequired; // array assignment
        if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP))
            ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToHeatingSP =
                ZoneSysEnergyDemand(ZoneNum).OutputRequiredToHeatingSP; // array assignment
        if (allocated(ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP))
            ZoneSysEnergyDemand(ZoneNum).SequencedOutputRequiredToCoolingSP =
                ZoneSysEnergyDemand(ZoneNum).OutputRequiredToCoolingSP; // array assignment
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


    void CalcPredictedHumidityRatio(int const ZoneNum, Real64 RAFNFrac)
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
        using DataSurfaces::Surface;
        using General::RoundSigDigits;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcPredictedHumidityRatio");

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

        // FLOW:
        LoadToHumidifySetPoint = 0.0;
        LoadToDehumidifySetPoint = 0.0;
        SingleSetPoint = false;
        ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired = 0.0;
        ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP = 0.0;
        ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

        // Check to see if this is a "humidity controlled zone"
        ControlledHumidZoneFlag = false;
        // Check all the controlled zones to see if it matches the zone simulated
        for (HumidControlledZoneNum = 1; HumidControlledZoneNum <= NumHumidityControlZones; ++HumidControlledZoneNum) {
            if (HumidityControlZone(HumidControlledZoneNum).ActualZoneNum != ZoneNum) continue;
            ZoneAirRH = PsyRhFnTdbWPb(MAT(ZoneNum), ZoneAirHumRat(ZoneNum), OutBaroPress) * 100.0;
            ZoneRHHumidifyingSetPoint = GetCurrentScheduleValue(HumidityControlZone(HumidControlledZoneNum).HumidifyingSchedIndex);
            ZoneRHDehumidifyingSetPoint = GetCurrentScheduleValue(HumidityControlZone(HumidControlledZoneNum).DehumidifyingSchedIndex);

            // Apply EMS values to overwrite the humidistat values
            if (HumidityControlZone(HumidControlledZoneNum).EMSOverrideHumidifySetPointOn) {
                ZoneRHHumidifyingSetPoint = HumidityControlZone(HumidControlledZoneNum).EMSOverrideHumidifySetPointValue;
            }
            if (HumidityControlZone(HumidControlledZoneNum).EMSOverrideDehumidifySetPointOn) {
                ZoneRHDehumidifyingSetPoint = HumidityControlZone(HumidControlledZoneNum).EMSOverrideDehumidifySetPointValue;
            }

            // Apply offsets for faulty humidistats_Feb. 2015, zrp
            if ((NumFaultyHumidistat > 0) && (!WarmupFlag) && (!DoingSizing) && (!KickOffSimulation)) {

                //  loop through the FaultsHumidistatOffset objects to find the one for the zone
                for (int iFault = 1; iFault <= NumFaultyHumidistat; ++iFault) {

                    if (UtilityRoutines::SameString(HumidityControlZone(HumidControlledZoneNum).ControlName,
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
                                        if (GetCurrentScheduleValue(FaultsThermostatOffset(iFaultThermo).AvaiSchedPtr) > 0.0) {

                                            // Check fault severity schedules to update the reference thermostat offset
                                            double rSchVal = 1.0;
                                            if (FaultsThermostatOffset(iFaultThermo).SeveritySchedPtr >= 0) {
                                                rSchVal = GetCurrentScheduleValue(FaultsThermostatOffset(iFaultThermo).SeveritySchedPtr);
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
                                ShowSevereError("FaultModel:HumidistatOffset = \"" + FaultsHumidistatOffset(iFault).Name +
                                                "\" invalid Reference Humidistat Offset Name = \"" +
                                                FaultsHumidistatOffset(iFault).FaultyThermostatName + "\" not found.");
                                ShowFatalError("Errors getting FaultModel input data.  Preceding condition(s) cause termination.");
                            }

                            if (offsetThermostat != 0.0) {
                                // Calculate the humidistat offset value from the thermostat offset value
                                faultZoneWHumidifyingSetPoint =
                                    PsyWFnTdbRhPb((MAT(ZoneNum) + offsetThermostat), (ZoneRHHumidifyingSetPoint / 100.0), OutBaroPress);
                                faultZoneWDehumidifyingSetPoint =
                                    PsyWFnTdbRhPb((MAT(ZoneNum) + offsetThermostat), (ZoneRHDehumidifyingSetPoint / 100.0), OutBaroPress);
                                offsetZoneRHHumidifyingSetPoint =
                                    ZoneRHHumidifyingSetPoint - PsyRhFnTdbWPb(MAT(ZoneNum), faultZoneWHumidifyingSetPoint, OutBaroPress) * 100.0;
                                offsetZoneRHDehumidifyingSetPoint =
                                    ZoneRHDehumidifyingSetPoint - PsyRhFnTdbWPb(MAT(ZoneNum), faultZoneWDehumidifyingSetPoint, OutBaroPress) * 100.0;

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
                            if (GetCurrentScheduleValue(FaultsHumidistatOffset(iFault).AvaiSchedPtr) > 0.0) {

                                // Check fault severity schedules to update the reference humidistat offset
                                double rSchVal = 1.0;
                                double offsetUpdated;
                                if (FaultsHumidistatOffset(iFault).SeveritySchedPtr >= 0) {
                                    rSchVal = GetCurrentScheduleValue(FaultsHumidistatOffset(iFault).SeveritySchedPtr);
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
                if (HumidityControlZone(HumidControlledZoneNum).ErrorIndex == 0) {
                    ShowWarningMessage("HUMIDISTAT: The humidifying setpoint is above the dehumidifying setpoint in " +
                                       HumidityControlZone(HumidControlledZoneNum).ControlName);
                    ShowContinueError("The zone humidifying setpoint is set to the dehumidifying setpoint.");
                    ShowContinueErrorTimeStamp("Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd("The humidifying setpoint is still above the dehumidifying setpoint",
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
            LatentGain = ZoneLatentGain(ZoneNum) + SumLatentHTRadSys(ZoneNum) + SumLatentPool(ZoneNum);

            SysTimeStepInSeconds = SecInHour * TimeStepSys;

            // Calculate the coefficients for the 3rd Order derivative for final
            // zone humidity ratio.  The A, B, C coefficients are analogous to the heat balance.
            // SumHmARaW and SumHmARa will be used with the Moisture Balance on the building elements and
            // are currently set to zero when the CTF only version is used.

            // if no surface in the zone uses EMPD or HAMT then zero
            bool no_ht_EMPD_or_HAMT(true);
            for (int i = Zone(ZoneNum).SurfaceFirst, e = Zone(ZoneNum).SurfaceLast; i <= e; ++i) {
                auto const &htAlgo(Surface(i).HeatTransferAlgorithm);
                if ((htAlgo == HeatTransferModel_EMPD) || (htAlgo == HeatTransferModel_HAMT)) {
                    no_ht_EMPD_or_HAMT = false;
                    break;
                }
            }
            if (no_ht_EMPD_or_HAMT) {
                SumHmARaW(ZoneNum) = 0.0;
                SumHmARa(ZoneNum) = 0.0;
            }

            // The density of air and latent heat of vaporization are calculated as functions.
            RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName);
            H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneNum), ZT(ZoneNum));

            // Assume that the system will have flow
            if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
                AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
                (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
                 AirflowNetwork::AirflowNetworkFanActivated)) {
                // Multizone airflow calculated in AirflowNetwork
                B = (LatentGain / H2OHtOfVap) + dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMHrW +
                    dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMHrW + SumHmARaW(ZoneNum);
                A = dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMHr + dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMHr +
                    SumHmARa(ZoneNum);
            } else {
                B = (LatentGain / H2OHtOfVap) + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutHumRat) + EAMFLxHumRat(ZoneNum) +
                    SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum) * OutHumRat;
                A = OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + SumHmARa(ZoneNum) + MixingMassFlowZone(ZoneNum) +
                    MDotOA(ZoneNum);
            }
            C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;

            if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                RoomAirNode = RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
                H2OHtOfVap = PsyHgAirFnWTdb(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat,
                                            RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp);
                A = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa;
                B = (RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain / H2OHtOfVap) +
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW;
                C = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir * RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume *
                    Zone(ZoneNum).ZoneVolCapMultpMoist / (SecInHour * TimeStepSys);
            }

            // Use a 3rd Order derivative to predict zone moisture addition or removal and
            // smooth the changes using the zone air capacitance.  Positive values of Moist Load means that
            // this amount of moisture must be added to the zone to reach the setpoint.  Negative values represent
            // the amount of moisture that must be removed by the system.
            // MoistLoadHumidSetPoint = massflow * HumRat = kgDryAir/s * kgWater/kgDryAir = kgWater/s
            WZoneSetPoint = PsyWFnTdbRhPb(ZT(ZoneNum), (ZoneRHHumidifyingSetPoint / 100.0), OutBaroPress, RoutineName);
            Real64 exp_700_A_C(0.0);
            if (ZoneAirSolutionAlgo == Use3rdOrder) {
                LoadToHumidifySetPoint = ((11.0 / 6.0) * C + A) * WZoneSetPoint -
                                         (B + C * (3.0 * WZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * WZoneTimeMinus2Temp(ZoneNum) +
                                                   (1.0 / 3.0) * WZoneTimeMinus3Temp(ZoneNum)));
                // Exact solution
            } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                if (A == 0.0) { // B=0
                    LoadToHumidifySetPoint = C * (WZoneSetPoint - ZoneW1(ZoneNum)) - B;
                } else {
                    exp_700_A_C = std::exp(min(700.0, -A / C)); // Tuned Save expensive value
                    LoadToHumidifySetPoint = A * (WZoneSetPoint - ZoneW1(ZoneNum) * exp_700_A_C) / (1.0 - exp_700_A_C) - B;
                }
            } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                LoadToHumidifySetPoint = C * (WZoneSetPoint - ZoneW1(ZoneNum)) + A * WZoneSetPoint - B;
            }
            if (RAFNFrac > 0.0) LoadToHumidifySetPoint = LoadToHumidifySetPoint / RAFNFrac;
            ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP = LoadToHumidifySetPoint;
            WZoneSetPoint = PsyWFnTdbRhPb(ZT(ZoneNum), (ZoneRHDehumidifyingSetPoint / 100.0), OutBaroPress, RoutineName);
            if (ZoneAirSolutionAlgo == Use3rdOrder) {
                LoadToDehumidifySetPoint = ((11.0 / 6.0) * C + A) * WZoneSetPoint -
                                           (B + C * (3.0 * WZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * WZoneTimeMinus2Temp(ZoneNum) +
                                                     (1.0 / 3.0) * WZoneTimeMinus3Temp(ZoneNum)));
                // Exact solution
            } else if (ZoneAirSolutionAlgo == UseAnalyticalSolution) {
                if (A == 0.0) { // B=0
                    LoadToDehumidifySetPoint = C * (WZoneSetPoint - ZoneW1(ZoneNum)) - B;
                } else {
                    LoadToDehumidifySetPoint = A * (WZoneSetPoint - ZoneW1(ZoneNum) * exp_700_A_C) / (1.0 - exp_700_A_C) - B; // exp_700_A_C set above
                }
            } else if (ZoneAirSolutionAlgo == UseEulerMethod) {
                LoadToDehumidifySetPoint = C * (WZoneSetPoint - ZoneW1(ZoneNum)) + A * WZoneSetPoint - B;
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
                        "Humidistat: Unanticipated combination of humidifying and dehumidifying loads - report to EnergyPlus Development Team");
                    ShowContinueErrorTimeStamp("occurs in Zone=" + Zone(ZoneNum).Name);
                    ShowContinueError("LoadToHumidifySetPoint=" + RoundSigDigits(LoadToHumidifySetPoint, 5) +
                                      ", LoadToDehumidifySetPoint=" + RoundSigDigits(LoadToDehumidifySetPoint, 5));
                    ShowContinueError("Zone RH Humidifying Set-point=" + RoundSigDigits(ZoneRHHumidifyingSetPoint, 1));
                    ShowContinueError("Zone RH Dehumidifying Set-point=" + RoundSigDigits(ZoneRHDehumidifyingSetPoint, 2));
                    ShowFatalError("Program terminates due to above conditions.");
                }
            }
        }

        // Apply zone multipliers as needed
        ReportMoistLoadsZoneMultiplier(ZoneSysMoistureDemand(ZoneNum).TotalOutputRequired,
                                       ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP,
                                       ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP,
                                       MoisturePredictedRate(ZoneNum),MoisturePredictedHumSPRate(ZoneNum),MoisturePredictedDehumSPRate(ZoneNum),
                                       Zone(ZoneNum).Multiplier,Zone(ZoneNum).ListMultiplier);

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

        // Using/Aliasing

        using DataLoopNode::Node;
        using DataRoomAirModel::AirModel;
        using DataRoomAirModel::IsZoneDV;
        using DataRoomAirModel::IsZoneUI;
        using DataRoomAirModel::MATFloor;
        using DataRoomAirModel::MATMX;
        using DataRoomAirModel::MATOC;
        using DataRoomAirModel::RoomAirModel_Mixing;
        using DataRoomAirModel::RoomAirModel_Mundt;
        using DataRoomAirModel::RoomAirModel_UserDefined;
        using DataRoomAirModel::XM2TFloor;
        using DataRoomAirModel::XM2TMX;
        using DataRoomAirModel::XM2TOC;
        using DataRoomAirModel::XM3TFloor;
        using DataRoomAirModel::XM3TMX;
        using DataRoomAirModel::XM3TOC;
        using DataRoomAirModel::XM4TFloor;
        using DataRoomAirModel::XM4TMX;
        using DataRoomAirModel::XM4TOC;
        using DataRoomAirModel::XMATFloor;
        using DataRoomAirModel::XMATMX;
        using DataRoomAirModel::XMATOC;
        using DataRoomAirModel::ZoneDVMixedFlag;
        using DataRoomAirModel::ZTFloor;
        using DataRoomAirModel::ZTM1MX;
        using DataRoomAirModel::ZTM1OC;
        using DataRoomAirModel::ZTMX;
        using DataRoomAirModel::ZTOC;
        using General::TrimSigDigits;
        using RoomAirModelManager::ManageAirModel;

        // HybridModel
        using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CorrectZoneAirTemp");

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

        // FLOW:
        // Initializations
        ZoneTempChange = constant_zero;

        // Update zone temperatures
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

            if (ShortenTimeStepSys) {
                // time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
                if (NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep) { // cannot reuse existing DS data, interpolate from zone time
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  MAT(ZoneNum),
                                                  XMAT(ZoneNum),
                                                  XM2T(ZoneNum),
                                                  XM3T(ZoneNum),
                                                  XM4T(ZoneNum),
                                                  MAT(ZoneNum),
                                                  DSXMAT(ZoneNum),
                                                  DSXM2T(ZoneNum),
                                                  DSXM3T(ZoneNum),
                                                  DSXM4T(ZoneNum));
                    DownInterpolate4HistoryValues(PriorTimeStep,
                                                  TimeStepSys,
                                                  ZoneAirHumRat(ZoneNum),
                                                  WZoneTimeMinus1(ZoneNum),
                                                  WZoneTimeMinus2(ZoneNum),
                                                  WZoneTimeMinus3(ZoneNum),
                                                  WZoneTimeMinus4(ZoneNum),
                                                  ZoneAirHumRat(ZoneNum),
                                                  DSWZoneTimeMinus1(ZoneNum),
                                                  DSWZoneTimeMinus2(ZoneNum),
                                                  DSWZoneTimeMinus3(ZoneNum),
                                                  DSWZoneTimeMinus4(ZoneNum));
                    if (IsZoneDV(ZoneNum) || IsZoneUI(ZoneNum)) {
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      MATFloor(ZoneNum),
                                                      XMATFloor(ZoneNum),
                                                      XM2TFloor(ZoneNum),
                                                      XM3TFloor(ZoneNum),
                                                      XM4TFloor(ZoneNum),
                                                      MATFloor(ZoneNum),
                                                      DSXMATFloor(ZoneNum),
                                                      DSXM2TFloor(ZoneNum),
                                                      DSXM3TFloor(ZoneNum),
                                                      DSXM4TFloor(ZoneNum));
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      MATOC(ZoneNum),
                                                      XMATOC(ZoneNum),
                                                      XM2TOC(ZoneNum),
                                                      XM3TOC(ZoneNum),
                                                      XM4TOC(ZoneNum),
                                                      MATOC(ZoneNum),
                                                      DSXMATOC(ZoneNum),
                                                      DSXM2TOC(ZoneNum),
                                                      DSXM3TOC(ZoneNum),
                                                      DSXM4TOC(ZoneNum));
                        DownInterpolate4HistoryValues(PriorTimeStep,
                                                      TimeStepSys,
                                                      MATMX(ZoneNum),
                                                      XMATMX(ZoneNum),
                                                      XM2TMX(ZoneNum),
                                                      XM3TMX(ZoneNum),
                                                      XM4TMX(ZoneNum),
                                                      MATMX(ZoneNum),
                                                      DSXMATMX(ZoneNum),
                                                      DSXM2TMX(ZoneNum),
                                                      DSXM3TMX(ZoneNum),
                                                      DSXM4TMX(ZoneNum));
                    }
                    if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
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
                ZTM1(ZoneNum) = DSXMAT(ZoneNum);
                ZTM2(ZoneNum) = DSXM2T(ZoneNum);
                ZTM3(ZoneNum) = DSXM3T(ZoneNum);

                WZoneTimeMinus1Temp(ZoneNum) = DSWZoneTimeMinus1(ZoneNum);
                WZoneTimeMinus2Temp(ZoneNum) = DSWZoneTimeMinus2(ZoneNum);
                WZoneTimeMinus3Temp(ZoneNum) = DSWZoneTimeMinus3(ZoneNum);
            } else {
                ZTM1(ZoneNum) = XMAT(ZoneNum);
                ZTM2(ZoneNum) = XM2T(ZoneNum);
                ZTM3(ZoneNum) = XM3T(ZoneNum);

                WZoneTimeMinus1Temp(ZoneNum) = WZoneTimeMinus1(ZoneNum);
                WZoneTimeMinus2Temp(ZoneNum) = WZoneTimeMinus2(ZoneNum);
                WZoneTimeMinus3Temp(ZoneNum) = WZoneTimeMinus3(ZoneNum);
            }

            AIRRAT(ZoneNum) = Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpSens *
                              PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName) *
                              PsyCpAirFnW(ZoneAirHumRat(ZoneNum)) / (TimeStepSys * SecInHour);

            AirCap = AIRRAT(ZoneNum);

            ManageAirModel(state, ZoneNum);

            // Calculate the various heat balance sums
            CalcZoneSums(state.dataZonePlenum, ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);

            // Sum all convective internal gains except for people: SumIntGainExceptPeople
            if (HybridModel::FlagHybridModel_PC) {
                SumAllInternalConvectionGainsExceptPeople(ZoneNum, SumIntGainExceptPeople);
            }

            //    ZoneTempHistoryTerm = (3.0D0 * ZTM1(ZoneNum) - (3.0D0/2.0D0) * ZTM2(ZoneNum) + (1.0D0/3.0D0) * ZTM3(ZoneNum))
            ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;

            SNLoad = 0.0;

            if (ZoneNodeNum > 0) { // This zone is controlled by a zone equipment configuration or zone plenum

                // Heat balance coefficients for controlled zone, i.e. with system air flow
                TempDepCoef = SumHA + SumMCp + SumSysMCp;
                TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                              (NonAirSystemResponse(ZoneNum) / ZoneMult + SysDepZoneLoadsLagged(ZoneNum));
                //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    TempIndCoef += dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).TotalSen;
                }
                //    TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
                //    TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
                // Solve for zone air temperature
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        ZT(ZoneNum) = (TempIndCoef + AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum))) /
                                      ((11.0 / 6.0) * AirCap + TempDepCoef);
                        // Exact solution
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            ZT(ZoneNum) = ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            ZT(ZoneNum) = (ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                          TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        ZT(ZoneNum) = (AirCap * ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
                // Update zone node temperature and thermostat temperature unless already updated in Room Air Model,
                // calculate load correction factor
                if ((AirModel(ZoneNum).AirModelType == RoomAirModel_Mixing) || (!AirModel(ZoneNum).SimAirModel)) {
                    // Fully mixed
                    Node(ZoneNodeNum).Temp = ZT(ZoneNum);
                    TempTstatAir(ZoneNum) = ZT(ZoneNum);
                    LoadCorrectionFactor(ZoneNum) = 1.0;
                } else if (IsZoneDV(ZoneNum) || IsZoneUI(ZoneNum)) {
                    // UCSDDV: Not fully mixed - calculate factor to correct load for fully mixed assumption
                    if (SumSysMCp > SmallMassFlow) {
                        TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
                        if (std::abs(TempSupplyAir - ZT(ZoneNum)) > TempConvergTol) {
                            LoadCorrectionFactor(ZoneNum) = (TempSupplyAir - Node(ZoneNodeNum).Temp) / (TempSupplyAir - ZT(ZoneNum));
                            // constrain value to something reasonable
                            LoadCorrectionFactor(ZoneNum) = max(-3.0, LoadCorrectionFactor(ZoneNum));
                            LoadCorrectionFactor(ZoneNum) = min(3.0, LoadCorrectionFactor(ZoneNum));

                        } else {
                            LoadCorrectionFactor(ZoneNum) = 1.0; // Indeterminate
                        }
                    } else {
                        // Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
                        LoadCorrectionFactor(ZoneNum) = 1.0;
                    }
                } else if (AirModel(ZoneNum).SimAirModel &&
                           ((AirModel(ZoneNum).AirModelType == RoomAirModel_UserDefined) || (AirModel(ZoneNum).AirModelType == RoomAirModel_Mundt))) {
                    if (SumSysMCp > SmallMassFlow) {
                        TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
                        if (std::abs(TempSupplyAir - ZT(ZoneNum)) > TempConvergTol) {
                            LoadCorrectionFactor(ZoneNum) = (TempSupplyAir - Node(ZoneNodeNum).Temp) / (TempSupplyAir - ZT(ZoneNum));
                            // constrain value
                            LoadCorrectionFactor(ZoneNum) = max(-3.0, LoadCorrectionFactor(ZoneNum));
                            LoadCorrectionFactor(ZoneNum) = min(3.0, LoadCorrectionFactor(ZoneNum));

                        } else {
                            LoadCorrectionFactor(ZoneNum) = 1.0; // Indeterminate
                        }
                    } else {
                        // Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
                        LoadCorrectionFactor(ZoneNum) = 1.0;
                    }
                } else if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                    // Zone node used in the RoomAirflowNetwork model
                    ZT(ZoneNum) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).AirTemp;
                    Node(ZoneNodeNum).Temp = ZT(ZoneNum);
                    TempTstatAir(ZoneNum) = ZT(ZoneNum);
                    LoadCorrectionFactor(ZoneNum) = 1.0;
                } else {
                    Node(ZoneNodeNum).Temp = ZT(ZoneNum);
                    TempTstatAir(ZoneNum) = ZT(ZoneNum);
                    LoadCorrectionFactor(ZoneNum) = 1.0;
                }

                // Sensible load is the enthalpy into the zone minus the enthalpy that leaves the zone.
                CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
                ZoneEnthalpyIn = SumSysMCpT;

                // SNLOAD is the single zone load, without Zone Multiplier or Zone List Multiplier
                SNLoad = ZoneEnthalpyIn - (Node(ZoneNodeNum).MassFlowRate / ZoneMult) * CpAir * Node(ZoneNodeNum).Temp +
                         NonAirSystemResponse(ZoneNum) / ZoneMult + SysDepZoneLoadsLagged(ZoneNum);

            } else {

                // Heat balance coefficients for uncontrolled zone, i.e. without system air flow
                TempDepCoef = SumHA + SumMCp;
                TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT;

                //      TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    TempIndCoef += dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).TotalSen;
                }
                //      TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
                //      TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef

                // Solve for zone air temperature
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        ZT(ZoneNum) = (TempIndCoef + AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + (1.0 / 3.0) * ZTM3(ZoneNum))) /
                                      ((11.0 / 6.0) * AirCap + TempDepCoef);
                        // Exact solution
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            ZT(ZoneNum) = ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            ZT(ZoneNum) = (ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                          TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        ZT(ZoneNum) = (AirCap * ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }

                if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                    ZT(ZoneNum) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).AirTemp;
                }

                // No sensible load
                SNLoad = 0.0;
            }

            // Hybrid modeling start
            if ((HybridModelZone(ZoneNum).InfiltrationCalc_T || HybridModelZone(ZoneNum).InternalThermalMassCalc_T ||
                 HybridModelZone(ZoneNum).PeopleCountCalc_T) &&
                (!WarmupFlag) && (!DoingSizing)) {
                InverseModelTemperature(
                    ZoneNum, SumIntGain, SumIntGainExceptPeople, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, AirCap);
            }

            MAT(ZoneNum) = ZT(ZoneNum);

            // Determine sensible load heating/cooling rate and energy
            SNLoadHeatRate(ZoneNum) = max(SNLoad, 0.0);
            SNLoadCoolRate(ZoneNum) = std::abs(min(SNLoad, 0.0));
            SNLoadHeatEnergy(ZoneNum) = max(SNLoad, 0.0) * TimeStepSys * SecInHour;
            SNLoadCoolEnergy(ZoneNum) = std::abs(min(SNLoad, 0.0) * TimeStepSys * SecInHour);

            // Final humidity calcs
            CorrectZoneHumRat(state.dataZonePlenum, ZoneNum);

            ZoneAirHumRat(ZoneNum) = ZoneAirHumRatTemp(ZoneNum);
            state.dataZoneTempPredictorCorrector.ZoneAirRelHum(ZoneNum) = 100.0 * PsyRhFnTdbWPb(ZT(ZoneNum), ZoneAirHumRat(ZoneNum), OutBaroPress, RoutineName);

            // ZoneTempChange is used by HVACManager to determine if the timestep needs to be shortened.
            {
                auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    if (IsZoneDV(ZoneNum)) {
                        if (ZoneDVMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(ZTOC(ZoneNum) - ZTM1OC(ZoneNum)), std::abs(ZTMX(ZoneNum) - ZTM1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZTM1(ZoneNum)));
                        }
                    } else if (IsZoneUI(ZoneNum)) {
                        if (ZoneUFMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(ZTOC(ZoneNum) - ZTM1OC(ZoneNum)), std::abs(ZTMX(ZoneNum) - ZTM1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZTM1(ZoneNum)));
                        }
                    } else {
                        ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZTM1(ZoneNum)));
                    }
                } else if ((SELECT_CASE_var == UseAnalyticalSolution) || (SELECT_CASE_var == UseEulerMethod)) {
                    if (IsZoneDV(ZoneNum)) {
                        if (ZoneDVMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(ZTOC(ZoneNum) - Zone1OC(ZoneNum)), std::abs(ZTMX(ZoneNum) - Zone1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZoneT1(ZoneNum)));
                        }
                    } else if (IsZoneUI(ZoneNum)) {
                        if (ZoneUFMixedFlag(ZoneNum) == 0) {
                            ZoneTempChange =
                                max(ZoneTempChange, max(std::abs(ZTOC(ZoneNum) - Zone1OC(ZoneNum)), std::abs(ZTMX(ZoneNum) - Zone1MX(ZoneNum))));
                        } else {
                            ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZoneT1(ZoneNum)));
                        }
                    } else {
                        ZoneTempChange = max(ZoneTempChange, std::abs(ZT(ZoneNum) - ZoneT1(ZoneNum)));
                    }
                }
            }

            CalcZoneComponentLoadSums(state.dataZonePlenum, ZoneNum,
                                      TempDepCoef,
                                      TempIndCoef,
                                      ZnAirRpt(ZoneNum).SumIntGains,
                                      ZnAirRpt(ZoneNum).SumHADTsurfs,
                                      ZnAirRpt(ZoneNum).SumMCpDTzones,
                                      ZnAirRpt(ZoneNum).SumMCpDtInfil,
                                      ZnAirRpt(ZoneNum).SumMCpDTsystem,
                                      ZnAirRpt(ZoneNum).SumNonAirSystem,
                                      ZnAirRpt(ZoneNum).CzdTdt,
                                      ZnAirRpt(ZoneNum).imBalance,
                                      ZnAirRpt(ZoneNum).SumEnthalpyM,
                                      ZnAirRpt(ZoneNum).SumEnthalpyH);

        } // ZoneNum
    }

    void PushZoneTimestepHistories(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   February 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // push histories for timestep advancing

        // SUBROUTINE ARGUMENT DEFINITIONS:
        static std::string const CorrectZoneAirTemp("CorrectZoneAirTemp");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        int LoopNode;

        // Push the temperature and humidity ratio histories

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            XM4T(ZoneNum) = XM3T(ZoneNum);
            XM3T(ZoneNum) = XM2T(ZoneNum);
            XM2T(ZoneNum) = XMAT(ZoneNum);
            XMAT(ZoneNum) = ZTAV(ZoneNum); // using average for whole zone time step.
            XMPT(ZoneNum) = ZT(ZoneNum);
            //      MAT(ZoneNum)  = ZT(ZoneNum)

            WZoneTimeMinus4(ZoneNum) = WZoneTimeMinus3(ZoneNum);
            WZoneTimeMinus3(ZoneNum) = WZoneTimeMinus2(ZoneNum);
            WZoneTimeMinus2(ZoneNum) = WZoneTimeMinus1(ZoneNum);
            WZoneTimeMinus1(ZoneNum) = ZoneAirHumRatAvg(ZoneNum); // using average for whole zone time step.
            ZoneAirHumRat(ZoneNum) = ZoneAirHumRatTemp(ZoneNum);
            WZoneTimeMinusP(ZoneNum) = ZoneAirHumRatTemp(ZoneNum);
            dataZoneTempPredictorCorrector.ZoneAirRelHum(ZoneNum) = 100.0 * PsyRhFnTdbWPb(ZT(ZoneNum), ZoneAirHumRat(ZoneNum), OutBaroPress, CorrectZoneAirTemp);

            if (AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDDV || AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFI ||
                AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFE) {
                XM4TFloor(ZoneNum) = XM3TFloor(ZoneNum);
                XM3TFloor(ZoneNum) = XM2TFloor(ZoneNum);
                XM2TFloor(ZoneNum) = XMATFloor(ZoneNum);
                XMATFloor(ZoneNum) = ZTFloor(ZoneNum);
                MATFloor(ZoneNum) = ZTFloor(ZoneNum);

                XM4TOC(ZoneNum) = XM3TOC(ZoneNum);
                XM3TOC(ZoneNum) = XM2TOC(ZoneNum);
                XM2TOC(ZoneNum) = XMATOC(ZoneNum);
                XMATOC(ZoneNum) = ZTOC(ZoneNum);
                MATOC(ZoneNum) = ZTOC(ZoneNum);

                XM4TMX(ZoneNum) = XM3TMX(ZoneNum);
                XM3TMX(ZoneNum) = XM2TMX(ZoneNum);
                XM2TMX(ZoneNum) = XMATMX(ZoneNum);
                XMATMX(ZoneNum) = ZTMX(ZoneNum);
                MATMX(ZoneNum) = ZTMX(ZoneNum);
            }

            // for RoomAirflowNetwork model
            if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
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

            if (ZoneAirSolutionAlgo != Use3rdOrder) {
                ZoneTM2(ZoneNum) = ZoneTMX(ZoneNum);
                ZoneTMX(ZoneNum) = ZTAV(ZoneNum); // using average for whole zone time step.
                ZoneWM2(ZoneNum) = ZoneWMX(ZoneNum);
                ZoneWMX(ZoneNum) = ZoneAirHumRatAvg(ZoneNum); // using average for whole zone time step.
                if (AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDDV || AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFI ||
                    AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFE) {
                    ZoneM2Floor(ZoneNum) = ZoneMXFloor(ZoneNum);
                    ZoneMXFloor(ZoneNum) = ZTFloor(ZoneNum); // using average for whole zone time step.
                    ZoneM2OC(ZoneNum) = ZoneMXOC(ZoneNum);
                    ZoneMXOC(ZoneNum) = ZTOC(ZoneNum); // using average for whole zone time step.
                    ZoneM2MX(ZoneNum) = ZoneMXMX(ZoneNum);
                    ZoneMXMX(ZoneNum) = ZTMX(ZoneNum); // using average for whole zone time step.
                }

                if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                        //						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX =
                        // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                        //						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX =
                        // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1;
                    }
                }
            }
        } // zone loop
    }

    void PushSystemTimestepHistories()
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

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            DSXM4T(ZoneNum) = DSXM3T(ZoneNum);
            DSXM3T(ZoneNum) = DSXM2T(ZoneNum);
            DSXM2T(ZoneNum) = DSXMAT(ZoneNum);
            DSXMAT(ZoneNum) = MAT(ZoneNum);

            DSWZoneTimeMinus4(ZoneNum) = DSWZoneTimeMinus3(ZoneNum);
            DSWZoneTimeMinus3(ZoneNum) = DSWZoneTimeMinus2(ZoneNum);
            DSWZoneTimeMinus2(ZoneNum) = DSWZoneTimeMinus1(ZoneNum);
            DSWZoneTimeMinus1(ZoneNum) = ZoneAirHumRat(ZoneNum);

            if (IsZoneDV(ZoneNum) || IsZoneUI(ZoneNum)) {
                DSXM4TFloor(ZoneNum) = DSXM3TFloor(ZoneNum);
                DSXM3TFloor(ZoneNum) = DSXM2TFloor(ZoneNum);
                DSXM2TFloor(ZoneNum) = DSXMATFloor(ZoneNum);
                DSXMATFloor(ZoneNum) = MATFloor(ZoneNum);

                DSXM4TOC(ZoneNum) = DSXM3TOC(ZoneNum);
                DSXM3TOC(ZoneNum) = DSXM2TOC(ZoneNum);
                DSXM2TOC(ZoneNum) = DSXMATOC(ZoneNum);
                DSXMATOC(ZoneNum) = MATOC(ZoneNum);

                DSXM4TMX(ZoneNum) = DSXM3TMX(ZoneNum);
                DSXM3TMX(ZoneNum) = DSXM2TMX(ZoneNum);
                DSXM2TMX(ZoneNum) = DSXMATMX(ZoneNum);
                DSXMATMX(ZoneNum) = MATMX(ZoneNum);
            }
            if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
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

        if (ZoneAirSolutionAlgo != Use3rdOrder) {
            for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                ZoneTM2(ZoneNum) = ZoneTMX(ZoneNum);
                ZoneTMX(ZoneNum) = MAT(ZoneNum); // using average for whole zone time step.
                ZoneWM2(ZoneNum) = ZoneWMX(ZoneNum);
                ZoneWMX(ZoneNum) = ZoneAirHumRatTemp(ZoneNum); // using average for whole zone time step.

                if (AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDDV || AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFI ||
                    AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFE) {
                    ZoneM2Floor(ZoneNum) = ZoneMXFloor(ZoneNum);
                    ZoneMXFloor(ZoneNum) = ZTFloor(ZoneNum); // using average for whole zone time step.
                    ZoneM2OC(ZoneNum) = ZoneMXOC(ZoneNum);
                    ZoneMXOC(ZoneNum) = ZTOC(ZoneNum); // using average for whole zone time step.
                    ZoneM2MX(ZoneNum) = ZoneMXMX(ZoneNum);
                    ZoneMXMX(ZoneNum) = ZTMX(ZoneNum); // using average for whole zone time step.
                }
                if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
                    for (LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++LoopNode) {
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTempTMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).AirTemp;
                        //						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX =
                        // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWM2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX;
                        RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRatWMX = RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopNode).HumRat;
                        //						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX =
                        // RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1;
                    }
                }
            } // zone loop
        }
    }

    void RevertZoneTimestepHistories()
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

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            //  MAT(ZoneNum)  = XMAT(ZoneNum)
            XMAT(ZoneNum) = XM2T(ZoneNum);
            XM2T(ZoneNum) = XM3T(ZoneNum);
            XM3T(ZoneNum) = XM4T(ZoneNum);

            //   ZoneAirHumRat(ZoneNum)  = WZoneTimeMinus1(ZoneNum)
            WZoneTimeMinus1(ZoneNum) = WZoneTimeMinus2(ZoneNum);
            WZoneTimeMinus2(ZoneNum) = WZoneTimeMinus3(ZoneNum);
            WZoneTimeMinus3(ZoneNum) = WZoneTimeMinus4(ZoneNum);

            if (AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDDV || AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFI ||
                AirModel(ZoneNum).AirModelType == RoomAirModel_UCSDUFE) {

                //      MATFloor(ZoneNum)= XMATFloor(ZoneNum)
                XMATFloor(ZoneNum) = XM2TFloor(ZoneNum);
                XM2TFloor(ZoneNum) = XM3TFloor(ZoneNum);
                XM3TFloor(ZoneNum) = XM4TFloor(ZoneNum);
                //      MATOC(ZoneNum) = XMATOC(ZoneNum)
                XMATOC(ZoneNum) = XM2TOC(ZoneNum);
                XM2TOC(ZoneNum) = XM3TOC(ZoneNum);
                XM3TOC(ZoneNum) = XM4TOC(ZoneNum);

                //     MATMX(ZoneNum)=  XMATMX(ZoneNum)
                XMATMX(ZoneNum) = XM2TMX(ZoneNum);
                XM2TMX(ZoneNum) = XM3TMX(ZoneNum);
                XM3TMX(ZoneNum) = XM4TMX(ZoneNum);
            }

            if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
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

    void CorrectZoneHumRat(ZonePlenumData &dataZonePlenum, int const ZoneNum)
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
        using DataDefineEquip::AirDistUnit;
        using DataLoopNode::Node;
        using DataSurfaces::HeatTransferModel_EMPD;
        using DataSurfaces::HeatTransferModel_HAMT;
        using DataSurfaces::Surface;
        using DataZoneEquipment::ZoneEquipConfig;
        //using ZonePlenum::ZoneRetPlenCond;
        //using ZonePlenum::ZoneSupPlenCond;

        using DataGlobals::TimeStepZone;

        using InternalHeatGains::SumAllInternalConvectionGainsExceptPeople;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CorrectZoneHumRat");

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

        // FLOW:
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
            for (NodeNum = 1; NodeNum <= dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {

                MoistureMassFlowRate += (Node(dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate *
                                         Node(dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).HumRat) /
                                        ZoneMult;
                ZoneMassFlowRate += Node(dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate / ZoneMult;
            } // NodeNum
            // add in the leak flow
            for (ADUListIndex = 1; ADUListIndex <= dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                ADUNum = dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
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
            MoistureMassFlowRate +=
                (Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate * Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).HumRat) /
                ZoneMult;
            ZoneMassFlowRate += Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate / ZoneMult;
        }

        // Calculate hourly humidity ratio from infiltration + humdidity added from latent load + system added moisture
        LatentGain = ZoneLatentGain(ZoneNum) + SumLatentHTRadSys(ZoneNum) + SumLatentPool(ZoneNum);

        if (HybridModelZone(ZoneNum).PeopleCountCalc_H) {
            LatentGainExceptPeople = ZoneLatentGainExceptPeople(ZoneNum) + SumLatentHTRadSys(ZoneNum) + SumLatentPool(ZoneNum);
        }

        SysTimeStepInSeconds = SecInHour * TimeStepSys;

        // Calculate the coefficients for the 3rd order derivative for final
        // zone humidity ratio.  The A, B, C coefficients are analogous to the
        // heat balance.  There are 2 cases that should be considered, system
        // operating and system shutdown.
        // SumHmARaW and SumHmARa will be used with the moisture balance on the building elements and
        // are currently set to zero to remind us where they need to be in the future
        bool no_ht_EMPD_or_HAMT(true);
        for (int i = Zone(ZoneNum).SurfaceFirst, e = Zone(ZoneNum).SurfaceLast; i <= e; ++i) {
            auto const &htAlgo(Surface(i).HeatTransferAlgorithm);
            if ((htAlgo == HeatTransferModel_EMPD) || (htAlgo == HeatTransferModel_HAMT)) {
                no_ht_EMPD_or_HAMT = false;
                break;
            }
        }
        if (no_ht_EMPD_or_HAMT) {
            SumHmARaW(ZoneNum) = 0.0;
            SumHmARa(ZoneNum) = 0.0;
        }

        RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum), RoutineName);
        H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneNum), ZT(ZoneNum));

        B = (LatentGain / H2OHtOfVap) + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutHumRat) + EAMFLxHumRat(ZoneNum) +
            (MoistureMassFlowRate) + SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum) * OutHumRat;
        A = ZoneMassFlowRate + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + SumHmARa(ZoneNum) + MixingMassFlowZone(ZoneNum) +
            MDotOA(ZoneNum);

        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
             AirflowNetwork::AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            B = (LatentGain / H2OHtOfVap) +
                (dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMHrW + dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMHrW) +
                (MoistureMassFlowRate) + SumHmARaW(ZoneNum);
            A = ZoneMassFlowRate + dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMHr +
                dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMHr + SumHmARa(ZoneNum);
        }
        C = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;

        if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
            B += dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).TotalLat;
        }

        // Use a 3rd order derivative to predict final zone humidity ratio and
        // smooth the changes using the zone air capacitance.
        {
            auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
            if (SELECT_CASE_var == Use3rdOrder) {
                ZoneAirHumRatTemp(ZoneNum) = (B + C * (3.0 * WZoneTimeMinus1Temp(ZoneNum) - (3.0 / 2.0) * WZoneTimeMinus2Temp(ZoneNum) +
                                                       (1.0 / 3.0) * WZoneTimeMinus3Temp(ZoneNum))) /
                                             ((11.0 / 6.0) * C + A);
                // Exact solution
            } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                if (A == 0.0) { // B=0
                    ZoneAirHumRatTemp(ZoneNum) = ZoneW1(ZoneNum) + B / C;
                } else {
                    ZoneAirHumRatTemp(ZoneNum) = (ZoneW1(ZoneNum) - B / A) * std::exp(min(700.0, -A / C)) + B / A;
                }
            } else if (SELECT_CASE_var == UseEulerMethod) {
                ZoneAirHumRatTemp(ZoneNum) = (C * ZoneW1(ZoneNum) + B) / (C + A);
            }
        }

        // Set the humidity ratio to zero if the zone has been dried out
        if (ZoneAirHumRatTemp(ZoneNum) < 0.0) ZoneAirHumRatTemp(ZoneNum) = 0.0;

        // Check to make sure that is saturated there is condensation in the zone
        // by resetting to saturation conditions.
        WZSat = PsyWFnTdbRhPb(ZT(ZoneNum), 1.0, OutBaroPress, RoutineName);

        if (ZoneAirHumRatTemp(ZoneNum) > WZSat) ZoneAirHumRatTemp(ZoneNum) = WZSat;

        if (AirModel(ZoneNum).AirModelType == RoomAirModel_AirflowNetwork) {
            ZoneAirHumRatTemp(ZoneNum) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID).HumRat;
        }

        // HybridModel with measured humidity ratio begins
        if ((HybridModelZone(ZoneNum).InfiltrationCalc_H || HybridModelZone(ZoneNum).PeopleCountCalc_H) && (!WarmupFlag) && (!DoingSizing)) {
            InverseModelHumidity(ZoneNum, LatentGain, LatentGainExceptPeople, ZoneMassFlowRate, MoistureMassFlowRate, H2OHtOfVap, RhoAir);
        }

        // Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
        ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
        if (ZoneNodeNum > 0) {
            Node(ZoneNodeNum).HumRat = ZoneAirHumRatTemp(ZoneNum);
            Node(ZoneNodeNum).Enthalpy = PsyHFnTdbW(ZT(ZoneNum), ZoneAirHumRatTemp(ZoneNum));
        }
    }

    void DownInterpolate4HistoryValues(Real64 const OldTimeStep,
                                       Real64 const NewTimeStep,
                                       Real64 &oldVal0,
                                       Real64 &oldVal1,
                                       Real64 &oldVal2,
                                       Real64 &EP_UNUSED(oldVal3),
                                       Real64 &EP_UNUSED(oldVal4),
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

    void InverseModelTemperature(int const ZoneNum,              // Zone number
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
        // This subroutine inversely solve infiltration airflow rate or people count with zone air tempearture measurements.

        // Using/Aliasing
        using DataEnvironment::DayOfYear;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InverseModelTemperature");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CpAir;                   // specific heat of air
        static Real64 TempDepCoef(0.0); // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
        static Real64 TempIndCoef(0.0); // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
        static Real64 AirCapHM(0.0);    // Air power capacity for hybrid modeling

        Real64 AA(0.0);
        Real64 BB(0.0);
        Real64 CC(0.0);
        Real64 DD(0.0);
        Real64 SumIntGainPeople(0.0); // Inversely solved convectice heat gain from people
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

        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
        Zone(ZoneNum).ZoneMeasuredTemperature = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneMeasuredTemperatureSchedulePtr);

        // HM calculation only HM calculation period start
        if (DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear && DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
            Real64 HMMultiplierAverage(1.0);
            Real64 MultpHM(1.0);

            ZT(ZoneNum) = Zone(ZoneNum).ZoneMeasuredTemperature; // Array1D<Real64> ZT -- Zone
                                                                 // Air Temperature Averaged over
                                                                 // the System Time Increment
            if (HybridModelZone(ZoneNum).InfiltrationCalc_T && UseZoneTimeStepHistory) {

                static std::string const RoutineNameInfiltration("CalcAirFlowSimple:Infiltration");

                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirTemperatureSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);
                    // Calculate the air humidity ratio at supply air inlet.
                    Real64 CpAirInlet(0.0);
                    CpAirInlet = PsyCpAirFnW(Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio);

                    SumSysMCp_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                    SumSysMCpT_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet * Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature;

                    AA = SumSysMCp_HM + SumHA + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MDotCPOA(ZoneNum);
                    BB = SumSysMCpT_HM + SumIntGain + SumHATsurf - SumHATref + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) +
                         MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp +
                         (NonAirSystemResponse(ZoneNum) / ZoneMult + SysDepZoneLoadsLagged(ZoneNum));
                } else {
                    AA = SumHA + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MDotCPOA(ZoneNum);
                    BB = SumIntGain + SumHATsurf - SumHATref + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) +
                         MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;
                }
                CC = AirCap;
                DD = (3.0 * PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * PreviousMeasuredZT2(ZoneNum) + (1.0 / 3.0) * PreviousMeasuredZT3(ZoneNum));

                zone_M_T = Zone(ZoneNum).ZoneMeasuredTemperature;
                delta_T = (Zone(ZoneNum).ZoneMeasuredTemperature - Zone(ZoneNum).OutDryBulbTemp);
                CpAir = PsyCpAirFnW(OutHumRat);
                AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, Zone(ZoneNum).OutDryBulbTemp, OutHumRat, RoutineNameInfiltration);
                Zone(ZoneNum).delta_T = delta_T;

                // s4 - Set ACH to 0 when delta_T <= 0.5, add max and min limits to ach
                if (std::abs(delta_T) <= 0.5) {
                    M_inf = 0.0;
                } else {
                    M_inf = (BB + CC * DD - ((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredTemperature) / (CpAir * delta_T);
                }
                ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / Zone(ZoneNum).Volume * SecInHour));
                M_inf = (ACH_inf / SecInHour) * Zone(ZoneNum).Volume * AirDensity;

                // Overwrite variable with inverse solution
                Zone(ZoneNum).MCPIHM = M_inf;
                Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;

            } // Hybrid model infiltration calcualtion end

            // Hybrid modeling internal thermal mass calcualtion start
            if (HybridModelZone(ZoneNum).InternalThermalMassCalc_T && SumSysMCpT == 0 && ZT(ZoneNum) != PreviousMeasuredZT1(ZoneNum) &&
                UseZoneTimeStepHistory) { // HM calculation only when SumSysMCpT =0,
                                          // TimeStepZone (not @ TimeStepSys)
                TempDepCoef = SumHA + SumMCp + SumSysMCp;
                TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT +
                              (NonAirSystemResponse(ZoneNum) / ZoneMult + SysDepZoneLoadsLagged(ZoneNum));
                //    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

                if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone) {
                    TempIndCoef += dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).TotalSen;
                }
                // Calculate air capacity using UseAnalyticalSolution
                if (TempDepCoef == 0.0) {
                    // Is this correct? Shouldn't we use log?? What if ZT(ZoneNum) ==
                    // PreviousMeasuredZT1(ZoneNum)??
                    AirCapHM = TempIndCoef / (ZT(ZoneNum) - PreviousMeasuredZT1(ZoneNum)); // Inverse equation
                } else {
                    Real64 AirCapHM_temp = 0.0;
                    if (TempIndCoef == TempDepCoef * ZT(ZoneNum)) {
                        AirCapHM_temp = 0.0; //  This is the denominator.
                    } else {
                        AirCapHM_temp = (TempIndCoef - TempDepCoef * PreviousMeasuredZT1(ZoneNum)) / (TempIndCoef - TempDepCoef * ZT(ZoneNum));
                    }

                    if ((AirCapHM_temp > 0) && (AirCapHM_temp != 1)) {    // Avoide IND
                        AirCapHM = TempDepCoef / std::log(AirCapHM_temp); // Inverse equation
                    } else {
                        AirCapHM = TempIndCoef / (ZT(ZoneNum) - PreviousMeasuredZT1(ZoneNum));
                    }
                }

                // Calculate multiplier
                if (std::abs(ZT(ZoneNum) - PreviousMeasuredZT1(ZoneNum)) > 0.05) { // Filter
                    MultpHM = AirCapHM /
                              (Zone(ZoneNum).Volume * PsyRhoAirFnPbTdbW(OutBaroPress, ZT(ZoneNum), ZoneAirHumRat(ZoneNum)) *
                               PsyCpAirFnW(ZoneAirHumRat(ZoneNum))) *
                              (TimeStepZone * SecInHour);      // Inverse equation
                    if ((MultpHM < 1.0) || (MultpHM > 30.0)) { // Temperature capacity multiplier greater than
                                                               // 1 and less than 30
                        MultpHM = 1.0;                         // Default value 1.0
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
                    if (DayOfYear == HybridModelZone(ZoneNum).HybridEndDayOfYear && EndDayFlag) {
                        HMMultiplierAverage = Zone(ZoneNum).ZoneVolCapMultpSensHMSum / Zone(ZoneNum).ZoneVolCapMultpSensHMCountSum;
                        Zone(ZoneNum).ZoneVolCapMultpSensHMAverage = HMMultiplierAverage;
                    }
                }
            } // Hybrid model internal thermal mass calcualtion end

            // Hybrid model people count calculation
            if (HybridModelZone(ZoneNum).PeopleCountCalc_T && UseZoneTimeStepHistory) {
                Zone(ZoneNum).ZoneMeasuredTemperature = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneMeasuredTemperatureSchedulePtr);
                Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
                Zone(ZoneNum).ZonePeopleSensibleHeatFraction =
                    GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleSensibleFractionSchedulePtr);
                Zone(ZoneNum).ZonePeopleRadiantHeatFraction =
                    GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleRadiationFractionSchedulePtr);

                FractionSensible = Zone(ZoneNum).ZonePeopleSensibleHeatFraction;
                FractionRadiation = Zone(ZoneNum).ZonePeopleRadiantHeatFraction;
                ActivityLevel = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);

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
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirTemperatureSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                    // Calculate the air humidity ratio at supply air inlet.
                    Real64 CpAirInlet(0.0);
                    CpAirInlet = PsyCpAirFnW(Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio);

                    SumSysMCp_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet;
                    SumSysMCpT_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * CpAirInlet * Zone(ZoneNum).ZoneMeasuredSupplyAirTemperature;

                    AA = SumSysMCp_HM + SumHA + SumMCp;
                    BB = SumSysMCpT_HM + SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT +
                         (NonAirSystemResponse(ZoneNum) / ZoneMult + SysDepZoneLoadsLagged(ZoneNum));
                } else {
                    AA = SumHA + SumMCp;
                    BB = SumIntGainExceptPeople + SumHATsurf - SumHATref + SumMCpT;
                }

                CC = AirCap;
                DD = (3.0 * PreviousMeasuredZT1(ZoneNum) - (3.0 / 2.0) * PreviousMeasuredZT2(ZoneNum) + (1.0 / 3.0) * PreviousMeasuredZT3(ZoneNum));

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
        PreviousMeasuredZT3(ZoneNum) = PreviousMeasuredZT2(ZoneNum);
        PreviousMeasuredZT2(ZoneNum) = PreviousMeasuredZT1(ZoneNum);
        PreviousMeasuredZT1(ZoneNum) = ZT(ZoneNum);
    }

    void InverseModelHumidity(int const ZoneNum,              // Zone number
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

        // Using/Aliasing
        using DataEnvironment::DayOfYear;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InverseModelHumidity");

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
        SysTimeStepInSeconds = SecInHour * TimeStepSys;

        // Get measured zone humidity ratio
        Zone(ZoneNum).ZoneMeasuredHumidityRatio = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneMeasuredHumidityRatioSchedulePtr);

        if (DayOfYear >= HybridModelZone(ZoneNum).HybridStartDayOfYear && DayOfYear <= HybridModelZone(ZoneNum).HybridEndDayOfYear) {
            ZoneAirHumRat(ZoneNum) = Zone(ZoneNum).ZoneMeasuredHumidityRatio;

            // Hybrid Model calculate air infiltration rate
            if (HybridModelZone(ZoneNum).InfiltrationCalc_H && UseZoneTimeStepHistory) {
                // Conditionally calculate the time dependent and time independent terms
                if (HybridModelZone(ZoneNum).IncludeSystemSupplyParameters) {
                    Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                    SumSysM_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                    SumSysMHumRat_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio;

                    AA = SumSysM_HM + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + SumHmARa(ZoneNum) + MixingMassFlowZone(ZoneNum) +
                         MDotOA(ZoneNum);
                    BB = SumSysMHumRat_HM + (LatentGain / H2OHtOfVap) + ((VAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutHumRat) + EAMFLxHumRat(ZoneNum) +
                         SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum) * OutHumRat;
                } else {
                    AA = VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + SumHmARa(ZoneNum) + MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    BB = (LatentGain / H2OHtOfVap) + ((VAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutHumRat) + EAMFLxHumRat(ZoneNum) + SumHmARaW(ZoneNum) +
                         MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum) * OutHumRat;
                }

                CC = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;
                DD = (3.0 * PreviousMeasuredHumRat1(ZoneNum) - (3.0 / 2.0) * PreviousMeasuredHumRat2(ZoneNum) +
                      (1.0 / 3.0) * PreviousMeasuredHumRat3(ZoneNum));

                zone_M_HR = Zone(ZoneNum).ZoneMeasuredHumidityRatio;
                delta_HR = (Zone(ZoneNum).ZoneMeasuredHumidityRatio - OutHumRat);

                CpAir = PsyCpAirFnW(OutHumRat);
                AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, Zone(ZoneNum).OutDryBulbTemp, OutHumRat, RoutineName);

                if (std::abs(Zone(ZoneNum).ZoneMeasuredHumidityRatio - OutHumRat) < 0.0000001) {
                    M_inf = 0.0;
                } else {
                    M_inf = (CC * DD + BB - ((11.0 / 6.0) * CC + AA) * Zone(ZoneNum).ZoneMeasuredHumidityRatio) / delta_HR;
                }

                // Add threshold for air change rate
                ACH_inf = max(0.0, min(10.0, (M_inf / AirDensity) / Zone(ZoneNum).Volume * SecInHour));
                M_inf = (ACH_inf / SecInHour) * Zone(ZoneNum).Volume * AirDensity;
                Zone(ZoneNum).MCPIHM = M_inf;
                Zone(ZoneNum).InfilOAAirChangeRateHM = ACH_inf;
            }

            // Hybrid Model calculate people count
            if (HybridModelZone(ZoneNum).PeopleCountCalc_H && UseZoneTimeStepHistory) {
                Zone(ZoneNum).ZonePeopleActivityLevel = GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleActivityLevelSchedulePtr);
                Zone(ZoneNum).ZonePeopleSensibleHeatFraction =
                    GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleSensibleFractionSchedulePtr);
                Zone(ZoneNum).ZonePeopleRadiantHeatFraction =
                    GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZonePeopleRadiationFractionSchedulePtr);

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
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirMassFlowRateSchedulePtr);
                    Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio =
                        GetCurrentScheduleValue(HybridModelZone(ZoneNum).ZoneSupplyAirHumidityRatioSchedulePtr);

                    SumSysM_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate;
                    SumSysMHumRat_HM = Zone(ZoneNum).ZoneMeasuredSupplyAirFlowRate * Zone(ZoneNum).ZoneMeasuredSupplyAirHumidityRatio;

                    AA = SumSysM_HM + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + SumHmARa(ZoneNum) +
                         MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    BB = SumSysMHumRat_HM + (LatentGainExceptPeople / H2OHtOfVap) + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutHumRat) +
                         EAMFLxHumRat(ZoneNum) + SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum) * OutHumRat;
                } else {
                    AA = ZoneMassFlowRate + OAMFL(ZoneNum) + VAMFL(ZoneNum) + EAMFL(ZoneNum) + CTMFL(ZoneNum) + SumHmARa(ZoneNum) +
                         MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum);
                    BB = (LatentGainExceptPeople / H2OHtOfVap) + ((OAMFL(ZoneNum) + VAMFL(ZoneNum) + CTMFL(ZoneNum)) * OutHumRat) +
                         EAMFLxHumRat(ZoneNum) + (MoistureMassFlowRate) + SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum) +
                         MDotOA(ZoneNum) * OutHumRat;
                }

                CC = RhoAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpMoist / SysTimeStepInSeconds;
                DD = (3.0 * PreviousMeasuredHumRat1(ZoneNum) - (3.0 / 2.0) * PreviousMeasuredHumRat2(ZoneNum) +
                      (1.0 / 3.0) * PreviousMeasuredHumRat3(ZoneNum));

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
        PreviousMeasuredHumRat3(ZoneNum) = PreviousMeasuredHumRat2(ZoneNum);
        PreviousMeasuredHumRat2(ZoneNum) = PreviousMeasuredHumRat1(ZoneNum);
        PreviousMeasuredHumRat1(ZoneNum) = Zone(ZoneNum).ZoneMeasuredHumidityRatio;
    }

    void CalcZoneSums(ZonePlenumData &dataZonePlenum, int const ZoneNum,  // Zone number
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
        using DataDefineEquip::AirDistUnit;
        using DataLoopNode::Node;
        using DataZoneEquipment::ZoneEquipConfig;
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

        // FLOW:
        SumIntGain = 0.0;
        SumHA = 0.0;
        SumHATsurf = 0.0;
        SumHATref = 0.0;
        SumMCp = 0.0;
        SumMCpT = 0.0;
        SumSysMCp = 0.0;
        SumSysMCpT = 0.0;

        // Sum all convective internal gains: SumIntGain
        SumAllInternalConvectionGains(ZoneNum, SumIntGain);
        SumIntGain += SumConvHTRadSys(ZoneNum) + SumConvPool(ZoneNum);

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
        // low or zero)
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(ZoneNum, RetAirGain, 0);
            SumIntGain += RetAirGain;
        }

        // Sum all non-system air flow, i.e. infiltration, simple ventilation, mixing, earth tube: SumMCp, SumMCpT
        SumMCp = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MDotCPOA(ZoneNum);
        SumMCpT =
            MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;

        // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
             AirflowNetwork::AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            SumMCp = dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMCp + dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMCp;
            SumMCpT = dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMCpT + dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMCpT;
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
                auto const &zec(ZoneEquipConfig(ZoneEquipConfigNum));
                for (int NodeNum = 1, NodeNum_end = zec.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum) {
                    // Get node conditions
                    //  this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call?
                    //  how can we tell?  predict step must be lagged ?  correct step, systems have run.
                    auto const &node(Node(zec.InletNode(NodeNum)));
                    NodeTemp = node.Temp;
                    MassFlowRate = node.MassFlowRate;
                    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));

                    Real64 const MassFlowRate_CpAir(MassFlowRate * CpAir);
                    SumSysMCp += MassFlowRate_CpAir;
                    SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
                } // NodeNum

            } else if (ZoneRetPlenumAirFlag) {
                ZoneRetPlenumNum = Zone(ZoneNum).PlenumCondNum;
                auto const &zrpc(dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum));
                Real64 const air_hum_rat(ZoneAirHumRat(ZoneNum));
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
                for (int ADUListIndex = 1, ADUListIndex_end = dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs;
                     ADUListIndex <= ADUListIndex_end;
                     ++ADUListIndex) {
                    ADUNum = dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
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
                NodeTemp = Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
                MassFlowRate = Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
                CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));

                SumSysMCp += MassFlowRate * CpAir;
                SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            }

            ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

            SumSysMCp /= ZoneMult;
            SumSysMCpT /= ZoneMult;
        }
        // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
        for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {

            if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            HA = 0.0;
            Area = Surface(SurfNum).Area; // For windows, this is the glazing area

            if (Surface(SurfNum).Class == SurfaceClass_Window) {
                auto const shading_flag(SurfaceWindow(SurfNum).ShadingFlag);

                // Add to the convective internal gains
                if (shading_flag == IntShadeOn || shading_flag == IntBlindOn) {
                    // The shade area covers the area of the glazing plus the area of the dividers.
                    Area += SurfaceWindow(SurfNum).DividerArea;
                    // If interior shade or blind is present it is assumed that both the convective and IR radiative gain
                    // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
                    // interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
                    // at the same time that the interaction between glass and shade is calculated.
                    SumIntGain += SurfaceWindow(SurfNum).DividerHeatGain;
                }

                // Other convection term is applicable to equivalent layer window (ASHWAT) model
                if (dataConstruction.Construct(Surface(SurfNum).Construction).WindowTypeEQL) SumIntGain += SurfaceWindow(SurfNum).OtherConvHeatGain;

                // Convective heat gain from natural convection in gap between glass and interior shade or blind
                if (shading_flag == IntShadeOn || shading_flag == IntBlindOn) SumIntGain += SurfaceWindow(SurfNum).ConvHeatFlowNatural;

                // Convective heat gain from airflow window
                if (SurfaceWindow(SurfNum).AirflowThisTS > 0.0) {
                    SumIntGain += SurfaceWindow(SurfNum).ConvHeatGainToZoneAir;
                    if (Zone(ZoneNum).NoHeatToReturnAir) {
                        SumIntGain += SurfaceWindow(SurfNum).RetHeatGainToZoneAir;
                        WinHeatGain(SurfNum) += SurfaceWindow(SurfNum).RetHeatGainToZoneAir;
                        WinHeatTransfer(SurfNum) += SurfaceWindow(SurfNum).RetHeatGainToZoneAir;
                        if (WinHeatGain(SurfNum) >= 0.0) {
                            WinHeatGainRep(SurfNum) = WinHeatGain(SurfNum);
                            WinHeatGainRepEnergy(SurfNum) = WinHeatGainRep(SurfNum) * TimeStepZoneSec;
                        } else {
                            WinHeatLossRep(SurfNum) = -WinHeatGain(SurfNum);
                            WinHeatLossRepEnergy(SurfNum) = WinHeatLossRep(SurfNum) * TimeStepZoneSec;
                        }
                        WinHeatTransferRepEnergy(SurfNum) = WinHeatTransfer(SurfNum) * TimeStepZoneSec;
                    }
                }

                // Add to the surface convection sums
                if (SurfaceWindow(SurfNum).FrameArea > 0.0) {
                    // Window frame contribution
                    Real64 const HA_surf(HConvIn(SurfNum) * SurfaceWindow(SurfNum).FrameArea * (1.0 + SurfaceWindow(SurfNum).ProjCorrFrIn));
                    SumHATsurf += HA_surf * SurfaceWindow(SurfNum).FrameTempSurfIn;
                    HA += HA_surf;
                }

                if (SurfaceWindow(SurfNum).DividerArea > 0.0 && shading_flag != IntShadeOn && shading_flag != IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    Real64 const HA_surf(HConvIn(SurfNum) * SurfaceWindow(SurfNum).DividerArea * (1.0 + 2.0 * SurfaceWindow(SurfNum).ProjCorrDivIn));
                    SumHATsurf += HA_surf * SurfaceWindow(SurfNum).DividerTempSurfIn;
                    HA += HA_surf;
                }

            } // End of check if window

            HA += HConvIn(SurfNum) * Area;
            SumHATsurf += HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum);

            // determine reference air temperature for this surface
            {
                auto const SELECT_CASE_var(Surface(SurfNum).TAirRef);
                if (SELECT_CASE_var == ZoneMeanAirTemp) {
                    // The zone air is the reference temperature (which is to be solved for in CorrectZoneAirTemp).
                    RefAirTemp = MAT(ZoneNum);
                    SumHA += HA;
                } else if (SELECT_CASE_var == AdjacentAirTemp) {
                    RefAirTemp = TempEffBulkAir(SurfNum);
                    SumHATref += HA * RefAirTemp;
                } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                    // check whether this zone is a controlled zone or not
                    if (!ControlledZoneAirFlag) {
                        ShowFatalError("Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone(ZoneNum).Name);
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
                    RefAirTemp = MAT(ZoneNum);
                    SumHA += HA;
                }
            }

        } // SurfNum
    }

    void CalcZoneComponentLoadSums(ZonePlenumData &dataZonePlenum, int const ZoneNum,        // Zone number
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
        //  they are not necessarily proper averages for what happend over entire zone time step
        //  these are not mulitplied by zone multipliers.
        //  The values are all Watts.

        // REFERENCES:
        // Equation 5 in Engineering Reference.

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;
        using DataDefineEquip::AirDistUnit;
        using DataLoopNode::Node;
        using DataZoneEquipment::ZoneEquipConfig;
        using General::RoundSigDigits;
        using InternalHeatGains::SumAllInternalConvectionGains;
        using InternalHeatGains::SumAllReturnAirConvectionGains;
        //using ZonePlenum::ZoneRetPlenCond;
        //using ZonePlenum::ZoneSupPlenCond;

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

        // Sum all convective internal gains: SumIntGain
        SumAllInternalConvectionGains(ZoneNum, SumIntGains);

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
        // low or zero)
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(ZoneNum, SumRetAirGains, 0);
            SumIntGains += SumRetAirGains;
        }

        // sum non-system air flow transfers between zones
        SumMCpDTzones = MCPTM(ZoneNum) - MCPM(ZoneNum) * MAT(ZoneNum); // but maybe it should be ZTAV(ZoneNum)

        // Sum non-system air flow, i.e. infiltration, simple ventilation, earth tube
        //  reuse SumMCp, SumMCpT from CalcZoneSum but use MAT (or maybe ZTAV?) to complete
        SumMCpDtInfil = (MCPTI(ZoneNum) - MCPI(ZoneNum) * MAT(ZoneNum)) + (MCPTV(ZoneNum) - MCPV(ZoneNum) * MAT(ZoneNum)) +
                        (MCPTE(ZoneNum) - MCPE(ZoneNum) * MAT(ZoneNum)) + (MCPTC(ZoneNum) - MCPC(ZoneNum) * MAT(ZoneNum)) +
                        (MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp -
                         MDotCPOA(ZoneNum) * MAT(ZoneNum)); // infiltration | Ventilation (simple) | Earth tube. | Cooltower | combined OA flow

        // Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model (if used)
        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone ||
            AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
            (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS &&
             AirflowNetwork::AirflowNetworkFanActivated)) {
            // Multizone airflow calculated in AirflowNetwork
            SumMCpDtInfil = dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMCpT -
                            dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMCp * MAT(ZoneNum);
            SumMCpDTzones = dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMCpT -
                            dataAirflowNetworkBalanceManager.exchangeData(ZoneNum).SumMMCp * MAT(ZoneNum);
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
                CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));

                SumMCpDTsystem += MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum));

                ADUNum = ZoneEquipConfig(ZoneEquipConfigNum).InletNodeADUNum(NodeNum);
                if (ADUNum > 0) {
                    NodeTemp = Node(AirDistUnit(ADUNum).OutletNodeNum).Temp;
                    MassFlowRate = Node(AirDistUnit(ADUNum).OutletNodeNum).MassFlowRate;
                    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
                    ADUHeatAddRate = MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum));
                    AirDistUnit(ADUNum).HeatRate = max(0.0, ADUHeatAddRate);
                    AirDistUnit(ADUNum).CoolRate = std::abs(min(0.0, ADUHeatAddRate));
                    AirDistUnit(ADUNum).HeatGain = AirDistUnit(ADUNum).HeatRate * TimeStepSys * SecInHour;
                    AirDistUnit(ADUNum).CoolGain = AirDistUnit(ADUNum).CoolRate * TimeStepSys * SecInHour;
                }

            } // NodeNum

        } else if (ZoneRetPlenumAirFlag) {
            ZoneRetPlenumNum = Zone(ZoneNum).PlenumCondNum;
            for (NodeNum = 1; NodeNum <= dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {
                // Get node conditions
                NodeTemp = Node(dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).Temp;
                MassFlowRate = Node(dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate;
                CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));

                SumMCpDTsystem += MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum));

            } // NodeNum
            // add in the leaks
            for (ADUListIndex = 1; ADUListIndex <= dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
                ADUNum = dataZonePlenum.ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
                if (AirDistUnit(ADUNum).UpStreamLeak) {
                    ADUInNode = AirDistUnit(ADUNum).InletNodeNum;
                    NodeTemp = Node(ADUInNode).Temp;
                    MassFlowRate = AirDistUnit(ADUNum).MassFlowRateUpStrLk;
                    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
                    SumMCpDTsystem += MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum));
                }
                if (AirDistUnit(ADUNum).DownStreamLeak) {
                    ADUOutNode = AirDistUnit(ADUNum).OutletNodeNum;
                    NodeTemp = Node(ADUOutNode).Temp;
                    MassFlowRate = AirDistUnit(ADUNum).MassFlowRateDnStrLk;
                    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
                    SumMCpDTsystem += MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum));
                }
            }

        } else if (ZoneSupPlenumAirFlag) {
            ZoneSupPlenumNum = Zone(ZoneNum).PlenumCondNum;
            // Get node conditions
            NodeTemp = Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
            MassFlowRate = Node(dataZonePlenum.ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
            CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));

            SumMCpDTsystem += MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum));
        }

        // non air system response.
        SumNonAirSystem = NonAirSystemResponse(ZoneNum) + SumConvHTRadSys(ZoneNum) + SumConvPool(ZoneNum);

        // Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
        for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {

            if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            Area = Surface(SurfNum).Area; // For windows, this is the glazing area
            // determine reference air temperature for this surface's convective heat transfer model
            {
                auto const SELECT_CASE_var(Surface(SurfNum).TAirRef);
                if (SELECT_CASE_var == ZoneMeanAirTemp) {
                    // The zone air is the reference temperature
                    RefAirTemp = MAT(ZoneNum);
                } else if (SELECT_CASE_var == AdjacentAirTemp) {
                    RefAirTemp = TempEffBulkAir(SurfNum);
                } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                    // check whether this zone is a controlled zone or not
                    if (!ControlledZoneAirFlag) {
                        ShowFatalError("Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone(ZoneNum).Name);
                        return;
                    }
                    // determine supply air temperature as a weighted average of the inlet temperatures.
                    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                        // Get node conditions
                        NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                        MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                        CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));

                        SumSysMCp += MassFlowRate * CpAir;
                        SumSysMCpT += MassFlowRate * CpAir * NodeTemp;

                    } // NodeNum
                    if (SumSysMCp > 0.0) {
                        RefAirTemp = SumSysMCpT / SumSysMCp;
                    } else {
                        // no system flow (yet) so just use last value for zone air temp
                        RefAirTemp = MAT(ZoneNum);
                    }

                } else {
                    // currently set to mean air temp but should add error warning here
                    RefAirTemp = MAT(ZoneNum);
                }
            }

            if (Surface(SurfNum).Class == SurfaceClass_Window) {

                // Add to the convective internal gains
                if (SurfaceWindow(SurfNum).ShadingFlag == IntShadeOn || SurfaceWindow(SurfNum).ShadingFlag == IntBlindOn) {
                    // The shade area covers the area of the glazing plus the area of the dividers.
                    Area += SurfaceWindow(SurfNum).DividerArea;
                    // If interior shade or blind is present it is assumed that both the convective and IR radiative gain
                    // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
                    // interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
                    // at the same time that the interaction between glass and shade is calculated.
                    SumIntGains += SurfaceWindow(SurfNum).DividerHeatGain;
                }

                // Other convection term is applicable to equivalent layer window (ASHWAT) model
                if (dataConstruction.Construct(Surface(SurfNum).Construction).WindowTypeEQL) SumIntGains += SurfaceWindow(SurfNum).OtherConvHeatGain;

                // Convective heat gain from natural convection in gap between glass and interior shade or blind
                if (SurfaceWindow(SurfNum).ShadingFlag == IntShadeOn || SurfaceWindow(SurfNum).ShadingFlag == IntBlindOn)
                    SumIntGains += SurfaceWindow(SurfNum).ConvHeatFlowNatural;

                // Convective heat gain from airflow window
                if (SurfaceWindow(SurfNum).AirflowThisTS > 0.0) {
                    SumIntGains += SurfaceWindow(SurfNum).ConvHeatGainToZoneAir;
                    if (Zone(ZoneNum).NoHeatToReturnAir) {
                        SumIntGains += SurfaceWindow(SurfNum).RetHeatGainToZoneAir;
                    }
                }

                // Add to the surface convection sums
                if (SurfaceWindow(SurfNum).FrameArea > 0.0) {
                    // Window frame contribution

                    SumHADTsurfs += HConvIn(SurfNum) * SurfaceWindow(SurfNum).FrameArea * (1.0 + SurfaceWindow(SurfNum).ProjCorrFrIn) *
                                    (SurfaceWindow(SurfNum).FrameTempSurfIn - RefAirTemp);
                }

                if (SurfaceWindow(SurfNum).DividerArea > 0.0 && SurfaceWindow(SurfNum).ShadingFlag != IntShadeOn &&
                    SurfaceWindow(SurfNum).ShadingFlag != IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHADTsurfs += HConvIn(SurfNum) * SurfaceWindow(SurfNum).DividerArea * (1.0 + 2.0 * SurfaceWindow(SurfNum).ProjCorrDivIn) *
                                    (SurfaceWindow(SurfNum).DividerTempSurfIn - RefAirTemp);
                }

            } // End of check if window

            SumHADTsurfs += HConvIn(SurfNum) * Area * (TempSurfInTmp(SurfNum) - RefAirTemp);

            // Accumulate Zone Phase Change Material Melting/Freezing Enthalpy output variables
            if (DataSurfaces::Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::HeatTransferModel_CondFD) {
                ZnAirRpt(ZoneNum).SumEnthalpyM += HeatBalFiniteDiffManager::SurfaceFD(SurfNum).EnthalpyM;
                ZnAirRpt(ZoneNum).SumEnthalpyH += HeatBalFiniteDiffManager::SurfaceFD(SurfNum).EnthalpyF;
            }
        } // SurfNum

        // now calculate air energy storage source term.
        // capacitance is volume * density * heat capacity
        CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
        RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneNum), ZoneAirHumRat(ZoneNum));

        {
            auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
            if (SELECT_CASE_var == Use3rdOrder) {
                CzdTdt = RhoAir * CpAir * Zone(ZoneNum).Volume * Zone(ZoneNum).ZoneVolCapMultpSens * (MAT(ZoneNum) - ZTM1(ZoneNum)) /
                         (TimeStepSys * SecInHour);
                // Exact solution
            } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                CzdTdt = TempIndCoef - TempDepCoef * MAT(ZoneNum);
            } else if (SELECT_CASE_var == UseEulerMethod) {
                CzdTdt = AIRRAT(ZoneNum) * (MAT(ZoneNum) - ZoneT1(ZoneNum));
            }
        }

        if (DisplayZoneAirHeatBalanceOffBalance) {
            imBalance = SumIntGains + SumHADTsurfs + SumMCpDTzones + SumMCpDtInfil + SumMCpDTsystem + SumNonAirSystem - CzdTdt;

            // throw warning if seriously out of balance (this may need to be removed if too noisy... )
            // formulate dynamic threshold value based on 20% of quadrature sum of components
            Threshold = 0.2 * std::sqrt(pow_2(SumIntGains) + pow_2(SumHADTsurfs) + pow_2(SumMCpDTzones) + pow_2(SumMCpDtInfil) +
                                        pow_2(SumMCpDTsystem) + pow_2(SumNonAirSystem) + pow_2(CzdTdt));
            if ((std::abs(imBalance) > Threshold) && (!WarmupFlag) && (!DoingSizing)) { // air balance is out by more than threshold
                if (Zone(ZoneNum).AirHBimBalanceErrIndex == 0) {
                    ShowWarningMessage("Zone Air Heat Balance is out of balance for zone named " + Zone(ZoneNum).Name);
                    ShowContinueError("Zone Air Heat Balance Deviation Rate is more than " + RoundSigDigits(Threshold, 1) + " {W}");
                    if (TurnFansOn) {
                        ShowContinueError("Night cycle fan operation may be causing above error");
                    }

                    ShowContinueErrorTimeStamp(" Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd("Zone Air Heat Balance is out of balance ... zone named " + Zone(ZoneNum).Name,
                                               Zone(ZoneNum).AirHBimBalanceErrIndex,
                                               std::abs(imBalance) - Threshold,
                                               std::abs(imBalance) - Threshold,
                                               _,
                                               "{W}",
                                               "{W}");
            }
        }
    }

    bool VerifyThermostatInZone(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, IOFiles &ioFiles, std::string const &ZoneName) // Zone to verify
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

        if (GetZoneAirStatsInputFlag) {
            GetZoneAirSetPoints(dataZoneTempPredictorCorrector, ioFiles);
            GetZoneAirStatsInputFlag = false;
        }
        if (NumTempControlledZones > 0) {
            if (UtilityRoutines::FindItemInList(ZoneName, TempControlledZone, &ZoneTempControls::ZoneName) > 0) {
                HasThermostat = true;
            } else {
                HasThermostat = false;
            }
        } else {
            HasThermostat = false;
        }
        return HasThermostat;
    }

    bool VerifyControlledZoneForThermostat(std::string const &ZoneName) // Zone to verify
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
        using DataZoneEquipment::ZoneEquipConfig;

        return (UtilityRoutines::FindItemInList(ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName) > 0);
    }

    void DetectOscillatingZoneTemp(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector)
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

        using ThermalComfort::ThermalComfortInASH55;
        using DataZoneEnergyDemands::CurDeadBandOrSetback;

        // first time run allocate arrays and setup output variable
        if (dataZoneTempPredictorCorrector.SetupOscillationOutputFlag) {
            dataZoneTempPredictorCorrector.ZoneTempHist.allocate(4, NumOfZones);
            dataZoneTempPredictorCorrector.ZoneTempHist = 0.0;
            dataZoneTempPredictorCorrector.ZoneTempOscillate.dimension(NumOfZones, 0.0);
            dataZoneTempPredictorCorrector.ZoneTempOscillateDuringOccupancy.dimension(NumOfZones, 0.0);
            dataZoneTempPredictorCorrector.ZoneTempOscillateInDeadband.dimension(NumOfZones, 0.0);
            // set up zone by zone variables
            // CurrentModuleObject='Zone'
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                SetupOutputVariable(
                    "Zone Oscillating Temperatures Time", OutputProcessor::Unit::hr, dataZoneTempPredictorCorrector.ZoneTempOscillate(iZone), "System", "Sum", Zone(iZone).Name);
                SetupOutputVariable(
                    "Zone Oscillating Temperatures During Occupancy Time", OutputProcessor::Unit::hr, dataZoneTempPredictorCorrector.ZoneTempOscillateDuringOccupancy(iZone), "System", "Sum", Zone(iZone).Name);
                SetupOutputVariable(
                    "Zone Oscillating Temperatures in Deadband Time", OutputProcessor::Unit::hr, dataZoneTempPredictorCorrector.ZoneTempOscillateInDeadband(iZone), "System", "Sum", Zone(iZone).Name);
            }
            // set up a variable covering all zones
            SetupOutputVariable(
                "Facility Any Zone Oscillating Temperatures Time", OutputProcessor::Unit::hr, dataZoneTempPredictorCorrector.AnyZoneTempOscillate, "System", "Sum", "Facility");
            SetupOutputVariable(
                "Facility Any Zone Oscillating Temperatures During Occupancy Time", OutputProcessor::Unit::hr, dataZoneTempPredictorCorrector.AnyZoneTempOscillateDuringOccupancy, "System", "Sum", "Facility");
            SetupOutputVariable(
                "Facility Any Zone Oscillating Temperatures in Deadband Time", OutputProcessor::Unit::hr, dataZoneTempPredictorCorrector.AnyZoneTempOscillateInDeadband, "System", "Sum", "Facility");
            // test if the oscillation variables are even used
            if (ReportingThisVariable("Zone Oscillating Temperatures Time") ||
                ReportingThisVariable("Zone Oscillating Temperatures During Occupancy Time") ||
                ReportingThisVariable("Zone Oscillating Temperatures in Deadband Time") ||
                ReportingThisVariable("Facility Any Zone Oscillating Temperatures Time") ||
                ReportingThisVariable("Facility Any Zone Oscillating Temperatures During Occupancy Time") ||
                ReportingThisVariable("Facility Any Zone Oscillating Temperatures in Deadband Time") ) {
                dataZoneTempPredictorCorrector.OscillationVariablesNeeded = true;
            }
            dataZoneTempPredictorCorrector.SetupOscillationOutputFlag = false;
        }
        if (dataZoneTempPredictorCorrector.OscillationVariablesNeeded) {
            // precalc the negative value for performance
            NegOscillateMagnitude = -OscillateMagnitude;
            // assume no zone is oscillating
            isAnyZoneOscillating = false;
            isAnyZoneOscillatingDuringOccupancy = false;
            isAnyZoneOscillatingInDeadband = false;

            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                isOscillate = false;
                dataZoneTempPredictorCorrector.ZoneTempHist(4, iZone) = dataZoneTempPredictorCorrector.ZoneTempHist(3, iZone);
                dataZoneTempPredictorCorrector.ZoneTempHist(3, iZone) = dataZoneTempPredictorCorrector.ZoneTempHist(2, iZone);
                dataZoneTempPredictorCorrector.ZoneTempHist(2, iZone) = dataZoneTempPredictorCorrector.ZoneTempHist(1, iZone);
                dataZoneTempPredictorCorrector.ZoneTempHist(1, iZone) = ZT(iZone);
                Diff34 = dataZoneTempPredictorCorrector.ZoneTempHist(3, iZone) - dataZoneTempPredictorCorrector.ZoneTempHist(4, iZone);
                Diff23 = dataZoneTempPredictorCorrector.ZoneTempHist(2, iZone) - dataZoneTempPredictorCorrector.ZoneTempHist(3, iZone);
                Diff12 = dataZoneTempPredictorCorrector.ZoneTempHist(1, iZone) - dataZoneTempPredictorCorrector.ZoneTempHist(2, iZone);
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
                dataZoneTempPredictorCorrector.ZoneTempOscillateDuringOccupancy(iZone) = 0.0;
                dataZoneTempPredictorCorrector.ZoneTempOscillateInDeadband(iZone) = 0.0;
                if (isOscillate) {
                    dataZoneTempPredictorCorrector.ZoneTempOscillate(iZone) = TimeStepSys;
                    isAnyZoneOscillating = true;
                    if (allocated(ThermalComfortInASH55)) {
                        if (ThermalComfortInASH55(iZone).ZoneIsOccupied) {
                            dataZoneTempPredictorCorrector.ZoneTempOscillateDuringOccupancy(iZone) = TimeStepSys;
                            isAnyZoneOscillatingDuringOccupancy = true;
                        }
                    }
                    if (CurDeadBandOrSetback(iZone)) {
                        dataZoneTempPredictorCorrector.ZoneTempOscillateInDeadband(iZone) = TimeStepSys;
                        isAnyZoneOscillatingInDeadband = true;
                    }
                } else {
                    dataZoneTempPredictorCorrector.ZoneTempOscillate(iZone) = 0.0;
                }
            }
            // any zone variable
            if (isAnyZoneOscillating) {
                dataZoneTempPredictorCorrector.AnyZoneTempOscillate = TimeStepSys;
            } else {
                dataZoneTempPredictorCorrector.AnyZoneTempOscillate = 0.0;
            }
            if (isAnyZoneOscillatingDuringOccupancy) {
                dataZoneTempPredictorCorrector.AnyZoneTempOscillateDuringOccupancy = TimeStepSys;
            } else {
                dataZoneTempPredictorCorrector.AnyZoneTempOscillateDuringOccupancy = 0.0;
            }
            if (isAnyZoneOscillatingInDeadband) {
                dataZoneTempPredictorCorrector.AnyZoneTempOscillateInDeadband = TimeStepSys;
            } else {
                dataZoneTempPredictorCorrector.AnyZoneTempOscillateInDeadband = 0.0;
            }

            // annual/runperiod sum for _perflog.csv file
            dataZoneTempPredictorCorrector.AnnualAnyZoneTempOscillate += dataZoneTempPredictorCorrector.AnyZoneTempOscillate;
            dataZoneTempPredictorCorrector.AnnualAnyZoneTempOscillateDuringOccupancy += dataZoneTempPredictorCorrector.AnyZoneTempOscillateDuringOccupancy;
            dataZoneTempPredictorCorrector.AnnualAnyZoneTempOscillateInDeadband += dataZoneTempPredictorCorrector.AnyZoneTempOscillateInDeadband;
        }
    }

    void AdjustAirSetPointsforOpTempCntrl(int const TempControlledZoneID, int const ActualZoneNum, Real64 &ZoneAirSetPoint)
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
        using DataHeatBalance::MRT;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 thisMRT;         // local variable for mean radiant temperature in this zone
        Real64 thisMRTFraction; // local variable for fraction that MRT is in Op Temp definition

        if (!(AnyOpTempControl)) return; // do nothing to setpoint

        if (!(TempControlledZone(TempControlledZoneID).OperativeTempControl)) return; // do nothing to setpoint

        // is operative temp radiative fraction scheduled or fixed?
        if (TempControlledZone(TempControlledZoneID).OpTempCntrlModeScheduled) {
            thisMRTFraction = GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID).OpTempRadiativeFractionSched);
        } else {
            thisMRTFraction = TempControlledZone(TempControlledZoneID).FixedRadiativeFraction;
        }

        // get mean radiant temperature for zone
        thisMRT = MRT(ActualZoneNum);

        // modify setpoint for operative temperature control
        //  traping for MRT fractions between 0.0 and 0.9 during get input, so shouldn't be able to divide by zero here.
        ZoneAirSetPoint = (ZoneAirSetPoint - thisMRTFraction * thisMRT) / (1.0 - thisMRTFraction);
    }

    void AdjustOperativeSetPointsforAdapComfort(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, int const TempControlledZoneID, Real64 &ZoneAirSetPoint)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Xuan Luo
        //       DATE WRITTEN   Jan 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine adjust the operative setpoints for each controlled adaptive thermal comfort models.

        // Using/Aliasing
        using DataEnvironment::DayOfYear;
        using WeatherManager::DesDayInput;
        using WeatherManager::Environment;
        using WeatherManager::Envrn;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int originZoneAirSetPoint = ZoneAirSetPoint;
        int AdaptiveComfortModelTypeIndex = TempControlledZone(TempControlledZoneID).AdaptiveComfortModelTypeIndex;

        // FLOW:
        // adjust zone operative setpoint
        if (!(TempControlledZone(TempControlledZoneID).AdaptiveComfortTempControl)) return; // do nothing to setpoint
        if ((Environment(Envrn).KindOfEnvrn != ksDesignDay) && (Environment(Envrn).KindOfEnvrn != ksHVACSizeDesignDay)) {
            // Adjust run period cooling set point
            switch (AdaptiveComfortModelTypeIndex) {
            case static_cast<int>(AdaptiveComfortModel::ASH55_CENTRAL):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::ASH55_UPPER_90):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::ASH55_UPPER_80):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_CENTRAL):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_I):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_II):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(DayOfYear);
                break;
            case static_cast<int>(AdaptiveComfortModel::CEN15251_UPPER_III):
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(DayOfYear);
                break;
            default:;
            }
        } else {
            int const envrnDayNum(Environment(Envrn).DesignDayNum);
            int const summerDesignDayTypeIndex(9);
            // Adjust summer design day set point
            if (DesDayInput(envrnDayNum).DayType == summerDesignDayTypeIndex) {
                ZoneAirSetPoint = dataZoneTempPredictorCorrector.AdapComfortSetPointSummerDesDay(AdaptiveComfortModelTypeIndex - 1);
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

    void CalcZoneAirComfortSetPoints(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, IOFiles &ioFiles)
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
        using General::TrimSigDigits;
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

        // FLOW:
        // Call thermal comfort module to read zone control comfort object
        if (dataZoneTempPredictorCorrector.CalcZoneAirComfortSetPointsFirstTimeFlag) {
            ManageThermalComfort(dataZoneTempPredictorCorrector, ioFiles, true);
            dataZoneTempPredictorCorrector.CalcZoneAirComfortSetPointsFirstTimeFlag = false;
        }

        ComfortControlType = 0; // Default

        for (RelativeZoneNum = 1; RelativeZoneNum <= NumComfortControlledZones; ++RelativeZoneNum) {

            ActualZoneNum = ComfortControlledZone(RelativeZoneNum).ActualZoneNum;
            ComfortControlSchedIndex = ComfortControlledZone(RelativeZoneNum).ComfortSchedIndex;
            ComfortControlType(ActualZoneNum) = GetCurrentScheduleValue(ComfortControlSchedIndex);

            // Get PMV values

            {
                auto const SELECT_CASE_var(
                    ComfortControlType(ActualZoneNum)); // Is this missing the possibility of sometimes having no control on a zone
                // during the simulation?
                if (SELECT_CASE_var == 0) { // Uncontrolled for thermal comfort
                    ZoneComfortControlsFanger(ActualZoneNum).LowPMV = -999.0;
                    ZoneComfortControlsFanger(ActualZoneNum).HighPMV = -999.0;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {

                    SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_SglHeatSetPointFanger;
                    SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndex = dataZoneTempPredictorCorrector.SetPointSingleHeatingFanger(SchedTypeIndex).PMVSchedIndex;
                    ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::SglHeatSetPointFanger);
                    ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex);
                    ZoneComfortControlsFanger(ActualZoneNum).HighPMV = -999.0;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {

                    SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_SglCoolSetPointFanger;
                    SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndex = dataZoneTempPredictorCorrector.SetPointSingleCoolingFanger(SchedTypeIndex).PMVSchedIndex;
                    ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::SglCoolSetPointFanger);
                    ZoneComfortControlsFanger(ActualZoneNum).LowPMV = -999.0;
                    ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex);

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {

                    SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_SglHCSetPointFanger;
                    SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndex = dataZoneTempPredictorCorrector.SetPointSingleHeatCoolFanger(SchedTypeIndex).PMVSchedIndex;
                    ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::SglHCSetPointFanger);
                    ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex);
                    ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex);

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {

                    SchedNameIndex = ComfortControlledZone(RelativeZoneNum).SchIndx_DualSetPointFanger;
                    SchedTypeIndex = ComfortControlledZone(RelativeZoneNum).ControlTypeSchIndx(SchedNameIndex);
                    SetPointComfortSchedIndexHot = dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(SchedTypeIndex).HeatPMVSchedIndex;
                    SetPointComfortSchedIndexCold = dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(SchedTypeIndex).CoolPMVSchedIndex;
                    ZoneComfortControlsFanger(ActualZoneNum).FangerType = static_cast<int>(ComfortControl::DualSetPointFanger);
                    ZoneComfortControlsFanger(ActualZoneNum).LowPMV = GetCurrentScheduleValue(SetPointComfortSchedIndexHot);
                    ZoneComfortControlsFanger(ActualZoneNum).HighPMV = GetCurrentScheduleValue(SetPointComfortSchedIndexCold);
                    if (ZoneComfortControlsFanger(ActualZoneNum).LowPMV > ZoneComfortControlsFanger(ActualZoneNum).HighPMV) {
                        ++ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrCount;
                        if (ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrCount < 2) {
                            ShowWarningError("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint: The heating PMV setpoint is above the cooling "
                                             "PMV setpoint in " +
                                             dataZoneTempPredictorCorrector.SetPointDualHeatCoolFanger(SchedTypeIndex).Name);
                            ShowContinueError("The zone dual heating PMV setpoint is set to the dual cooling PMV setpoint.");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        } else {
                            ShowRecurringWarningErrorAtEnd("The heating PMV setpoint is still above the cooling PMV setpoint",
                                                           ZoneComfortControlsFanger(ActualZoneNum).DualPMVErrIndex,
                                                           ZoneComfortControlsFanger(ActualZoneNum).LowPMV,
                                                           ZoneComfortControlsFanger(ActualZoneNum).LowPMV);
                        }
                        ZoneComfortControlsFanger(ActualZoneNum).LowPMV = ZoneComfortControlsFanger(ActualZoneNum).HighPMV;
                    }

                } else {
                    ShowSevereError("CalcZoneAirTempSetpoints: Illegal thermal control control type for Zone=" + Zone(ActualZoneNum).Name +
                                    ", Found value=" + TrimSigDigits(ComfortControlType(ActualZoneNum)) +
                                    ", in Schedule=" + ComfortControlledZone(RelativeZoneNum).ControlTypeSchedName);
                }
            }

            // Check Average method
            {
                auto const SELECT_CASE_var(ComfortControlledZone(RelativeZoneNum).AverageMethodNum);
                if (SELECT_CASE_var == static_cast<int>(AverageMethod::NO)) {
                    PeopleNum = ComfortControlledZone(RelativeZoneNum).SpecificObjectNum;
                    if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointLo);
                    } else {
                        GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, SetPointLo);
                    }
                    if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger))
                        GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointHi);
                } else if (SELECT_CASE_var == static_cast<int>(AverageMethod::SPE)) {
                    PeopleNum = ComfortControlledZone(RelativeZoneNum).SpecificObjectNum;
                    if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointLo);
                    } else {
                        GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, SetPointLo);
                    }
                    if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger))
                        GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, SetPointHi);
                } else if (SELECT_CASE_var == static_cast<int>(AverageMethod::OBJ)) {
                    ObjectCount = 0;
                    SetPointLo = 0.0;
                    SetPointHi = 0.0;
                    for (PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum) {
                        if (ActualZoneNum == People(PeopleNum).ZonePtr) {
                            ++ObjectCount;
                            GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                            SetPointLo += Tset;
                            if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                                GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                                SetPointHi += Tset;
                            }
                        }
                    }
                    SetPointLo /= ObjectCount;
                    if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) SetPointHi /= ObjectCount;
                } else if (SELECT_CASE_var == static_cast<int>(AverageMethod::PEO)) {
                    PeopleCount = 0.0;
                    SetPointLo = 0.0;
                    SetPointHi = 0.0;
                    for (PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum) {
                        if (ActualZoneNum == People(PeopleNum).ZonePtr) {
                            NumberOccupants = People(PeopleNum).NumberOfPeople * GetCurrentScheduleValue(People(PeopleNum).NumberOfPeoplePtr);
                            PeopleCount += NumberOccupants;
                            GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                            SetPointLo += Tset * NumberOccupants;
                            if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                                GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                                SetPointHi += Tset * NumberOccupants;
                            }
                        }
                    }
                    if (PeopleCount > 0) {
                        SetPointLo /= PeopleCount;
                        if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) SetPointHi /= PeopleCount;
                    } else {
                        // recurring warnings
                        //          ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount = &
                        //                                           ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount + 1
                        if (ComfortControlledZone(RelativeZoneNum).PeopleAverageErrIndex == 0) {
                            ShowWarningMessage("ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " +
                                               Zone(ActualZoneNum).Name + " is zero. The People Average option is not used.");
                            ShowContinueError("The Object Average option is used instead. Simulation continues .....");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd("ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " +
                                                           Zone(ActualZoneNum).Name + " is still zero. The People Average option is not used",
                                                       ComfortControlledZone(RelativeZoneNum).PeopleAverageErrIndex,
                                                       PeopleCount,
                                                       PeopleCount);
                        ObjectCount = 0;
                        SetPointLo = 0.0;
                        SetPointHi = 0.0;
                        for (PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum) {
                            if (ActualZoneNum == People(PeopleNum).ZonePtr) {
                                ++ObjectCount;
                                GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).LowPMV, Tset);
                                SetPointLo += Tset;
                                if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                                    GetComfortSetPoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum).HighPMV, Tset);
                                    SetPointHi += Tset;
                                }
                            }
                        }
                        SetPointLo /= ObjectCount;
                        if (ComfortControlType(ActualZoneNum) == static_cast<int>(ComfortControl::DualSetPointFanger)) SetPointHi /= ObjectCount;
                    }
                }
            }

            // Assign setpoint
            {
                auto const SELECT_CASE_var(
                    ComfortControlType(ActualZoneNum)); // Is this missing the possibility of sometimes having no control on a zone
                // during the simulation?
                if (SELECT_CASE_var == 0) { // Uncontrolled for thermal comfort
                    {
                        auto const SELECT_CASE_var1(TempControlType(ActualZoneNum));
                        if (SELECT_CASE_var1 == SingleHeatingSetPoint) {
                            ZoneThermostatSetPointHi(ActualZoneNum) = 0.0;
                        } else if (SELECT_CASE_var1 == SingleCoolingSetPoint) {
                            ZoneThermostatSetPointLo(ActualZoneNum) = 0.0;
                        }
                    }

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                    if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                        SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount + 1
                        if (ComfortControlledZone(RelativeZoneNum).TdbMinErrIndex < 2) {
                            ShowWarningMessage("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is below the Minimum dry-bulb "
                                               "temperature setpoint " +
                                               ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError("The zone heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is still below the "
                                                       "Minimum dry-bulb temperature setpoint ...",
                                                       ComfortControlledZone(RelativeZoneNum).TdbMinErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
                    ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    TempControlType(ActualZoneNum) = SingleHeatingSetPoint;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {

                    if (SetPointLo > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                        SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount + 1
                        if (ComfortControlledZone(RelativeZoneNum).TdbMaxErrIndex == 0) {
                            ShowWarningMessage("ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is above the Maximum dry-bulb "
                                               "temperature setpoint " +
                                               ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError("The zone cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd("ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is still above the "
                                                       "Maximum dry-bulb temperature setpoint ...",
                                                       ComfortControlledZone(RelativeZoneNum).TdbMaxErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
                    ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    TempControlType(ActualZoneNum) = SingleCoolingSetPoint;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {

                    if (ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint == ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                        SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                    }
                    if (SetPointLo > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint)
                        SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint;
                    if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint)
                        SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                    if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint ||
                        SetPointLo > ComfortControlledZone(RelativeZoneNum).TdbMaxSetPoint) {
                        //          ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount = ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount + 1
                        if (ComfortControlledZone(RelativeZoneNum).TdbHCErrIndex == 0) {
                            ShowWarningMessage("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is above the Maximum or "
                                               "below the Minimum dry-bulb temperature setpoint " +
                                               ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError("The zone setpoint is set to the Maximum dry-bulb temperature setpoint if above or the Minimum "
                                              "dry-bulb temperature setpoint if below");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is still beyond "
                                                       "the range between Maximum and Minimum dry-bulb temperature setpoint ...",
                                                       ComfortControlledZone(RelativeZoneNum).TdbHCErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }
                    TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo;
                    ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum);
                    TempControlType(ActualZoneNum) = SingleHeatCoolSetPoint;

                } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {

                    if (SetPointLo < ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint) {
                        SetPointLo = ComfortControlledZone(RelativeZoneNum).TdbMinSetPoint;
                        //          ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount =
                        //          ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount+1
                        if (ComfortControlledZone(RelativeZoneNum).TdbDualMinErrIndex == 0) {
                            ShowWarningMessage("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is below the Minimum dry-bulb "
                                               "temperature setpoint " +
                                               ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError("The zone dual heating setpoint is set to the Minimum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still below the Minimum "
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
                            ShowWarningMessage("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is above the Maximum dry-bulb "
                                               "temperature setpoint " +
                                               ComfortControlledZone(RelativeZoneNum).Name);
                            ShowContinueError("The zone dual cooling setpoint is set to the Maximum dry-bulb temperature setpoint");
                            ShowContinueErrorTimeStamp("Occurrence info:");
                        }
                        ShowRecurringWarningErrorAtEnd("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still above the Maximum "
                                                       "dry-bulb temperature setpoint ...",
                                                       ComfortControlledZone(RelativeZoneNum).TdbDualMaxErrIndex,
                                                       SetPointLo,
                                                       SetPointLo);
                    }

                    ZoneThermostatSetPointLo(ActualZoneNum) = SetPointLo;
                    ZoneThermostatSetPointHi(ActualZoneNum) = SetPointHi;
                    TempControlType(ActualZoneNum) = DualSetPointWithDeadBand;

                } else {
                    ShowSevereError("CalcZoneAirComfortSetpoints: Illegal thermal control control type for Zone=" + Zone(ActualZoneNum).Name +
                                    ", Found value=" + TrimSigDigits(ComfortControlType(ActualZoneNum)) +
                                    ", in Schedule=" + ComfortControlledZone(ActualZoneNum).ControlTypeSchedName);
                }
            }
        }
    }

    void GetComfortSetPoints(int const PeopleNum,
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

        Tmin = ComfortControlledZone(ComfortControlNum).TdbMinSetPoint;
        Tmax = ComfortControlledZone(ComfortControlNum).TdbMaxSetPoint;

        CalcThermalComfortFanger(PeopleNum, Tmin, PMVResult);
        PMVMin = PMVResult;
        CalcThermalComfortFanger(PeopleNum, Tmax, PMVResult);
        PMVMax = PMVResult;
        if (PMVSet > PMVMin && PMVSet < PMVMax) {
            Par(1) = PMVSet;
            Par(2) = double(PeopleNum);
            SolveRoot(Acc, MaxIter, SolFla, Tset, PMVResidual, Tmin, Tmax, Par);
            if (SolFla == -1) {
                if (!WarmupFlag) {
                    ++IterLimitExceededNum1;
                    if (IterLimitExceededNum1 == 1) {
                        ShowWarningError(ComfortControlledZone(ComfortControlNum).Name +
                                         ": Iteration limit exceeded calculating thermal comfort Fanger setpoint and non-converged setpoint is used");
                    } else {
                        ShowRecurringWarningErrorAtEnd(ComfortControlledZone(ComfortControlNum).Name +
                                                           ":  Iteration limit exceeded calculating thermal comfort setpoint.",
                                                       IterLimitErrIndex1,
                                                       Tset,
                                                       Tset);
                    }
                }
            } else if (SolFla == -2) {
                if (!WarmupFlag) {
                    ++IterLimitExceededNum2;
                    if (IterLimitExceededNum2 == 1) {
                        ShowWarningError(ComfortControlledZone(ComfortControlNum).Name +
                                         ": Solution is not found in calculating thermal comfort Fanger setpoint and the minimum setpoint is used");
                    } else {
                        ShowRecurringWarningErrorAtEnd(ComfortControlledZone(ComfortControlNum).Name +
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

    Real64 PMVResidual(Real64 const Tset,
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
        CalcThermalComfortFanger(PeopleNum, Tset, PMVresult);
        PMVResidual = Par(1) - PMVresult;
        return PMVResidual;
    }

    void AdjustCoolingSetPointforTempAndHumidityControl(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector, int const TempControlledZoneID,
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

        if (!(AnyZoneTempAndHumidityControl)) return; // do nothing to setpoint

        if (!(TempControlledZone(TempControlledZoneID).ZoneOvercoolControl)) return; // do nothing to setpoint

        if (TempControlledZone(TempControlledZoneID).OvercoolCntrlModeScheduled) {
            ZoneOvercoolRange = GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID).ZoneOvercoolRangeSchedIndex);
        } else {
            ZoneOvercoolRange = TempControlledZone(TempControlledZoneID).ZoneOvercoolConstRange;
        }
        ZoneOvercoolControlRatio = TempControlledZone(TempControlledZoneID).ZoneOvercoolControlRatio;

        // For Dual Setpoint thermostat the overcool range is limited by the temperature difference between cooling
        // and heating setpoints
        MaxAllowedOvercoolRange = ZoneThermostatSetPointHi(ActualZoneNum) - ZoneThermostatSetPointLo(ActualZoneNum);
        if (MaxAllowedOvercoolRange > 0.0) {
            ZoneOvercoolRange = min(ZoneOvercoolRange, MaxAllowedOvercoolRange);
        }
        // Calculate difference between zone air relative humidity and the dehumidifying setpoint
        RelativeHumidityDiff =
            dataZoneTempPredictorCorrector.ZoneAirRelHum(ActualZoneNum) - GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID).DehumidifyingSchedIndex);
        if (RelativeHumidityDiff > 0.0 && ZoneOvercoolControlRatio > 0.0) {
            // proportionally reset the cooling setpoint temperature downward (zone Overcool)
            ZoneOvercoolRange = min(ZoneOvercoolRange, RelativeHumidityDiff / ZoneOvercoolControlRatio);
            ZoneThermostatSetPointHi(ActualZoneNum) -= ZoneOvercoolRange;
        }
    }

    void OverrideAirSetPointsforEMSCntrl()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         L. Gu
        //       DATE WRITTEN   June 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine overrides the air temperature setpoint based on EMS

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;    // index of temp control
        int ZoneNum; // Zone index

        for (Loop = 1; Loop <= NumTempControlledZones; ++Loop) {
            if (TempControlledZone(Loop).EMSOverrideHeatingSetPointOn) {
                ZoneNum = TempControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(TempControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        // do nothing
                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
            if (TempControlledZone(Loop).EMSOverrideCoolingSetPointOn) {
                ZoneNum = TempControlledZone(Loop).ActualZoneNum;

                {
                    auto const SELECT_CASE_var(TempControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                        // do nothing
                    } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                        ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
        }

        for (Loop = 1; Loop <= NumComfortControlledZones; ++Loop) {
            if (ComfortControlledZone(Loop).EMSOverrideHeatingSetPointOn) {
                ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;
                {
                    auto const SELECT_CASE_var(ComfortControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        // do nothing
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                        ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
            if (ComfortControlledZone(Loop).EMSOverrideCoolingSetPointOn) {
                ZoneNum = ComfortControlledZone(Loop).ActualZoneNum;
                {
                    auto const SELECT_CASE_var(ComfortControlType(ZoneNum));

                    if (SELECT_CASE_var == 0) { // Uncontrolled

                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHeatSetPointFanger)) {
                        // do nothing
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglCoolSetPointFanger)) {
                        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::SglHCSetPointFanger)) {
                        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                        ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else if (SELECT_CASE_var == static_cast<int>(ComfortControl::DualSetPointFanger)) {
                        ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue;
                    } else {
                        // Do nothing
                    }
                }
            }
        }
    }

    // add values to the LEED tabular report related to schedules used by the thermostat objects
    void FillPredefinedTableOnThermostatSetpoints(ZoneTempPredictorCorrectorData &dataZoneTempPredictorCorrector)
    {
        // J.Glazer - Aug 2017
        using namespace OutputReportPredefined;
        std::vector<int> uniqSch;
        uniqSch.reserve(dataZoneTempPredictorCorrector.NumSingleTempHeatingControls + dataZoneTempPredictorCorrector.NumSingleTempCoolingControls + dataZoneTempPredictorCorrector.NumSingleTempHeatCoolControls +
                        dataZoneTempPredictorCorrector.NumDualTempHeatCoolControls * 2);
        Real64 setPointAt11;
        Real64 setPointAt23;
        int numDays;
        std::string monthAssumed;
        std::string monthAssumed2;
        const int wednesday = 4;

        for (int SingleTempHeatingControlNum = 1; SingleTempHeatingControlNum <= dataZoneTempPredictorCorrector.NumSingleTempHeatingControls; ++SingleTempHeatingControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex);
                PreDefTableEntry(pdChLeedSchStPtFirstObjUsed,
                                 dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName,
                                 dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex, false, wednesday, 11);
                PreDefTableEntry(pdchLeedSchStPt11amWednesday, dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, setPointAt11);
                PreDefTableEntry(pdchLeedSchStPt11amWedCnt, dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedIndex, false, wednesday, 23);
                PreDefTableEntry(pdchLeedSchStPt11pmWednesday, dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, setPointAt23);
                PreDefTableEntry(pdchLeedSchStPt11pmWedCnt, dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, numDays);

                PreDefTableEntry(pdChLeedSchStPtMonthUsed, dataZoneTempPredictorCorrector.SetPointSingleHeating(SingleTempHeatingControlNum).TempSchedName, monthAssumed);
            }
        }
        for (int SingleTempCoolingControlNum = 1; SingleTempCoolingControlNum <= dataZoneTempPredictorCorrector.NumSingleTempCoolingControls; ++SingleTempCoolingControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex);
                PreDefTableEntry(pdChLeedSchStPtFirstObjUsed,
                                 dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName,
                                 dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex, true, wednesday, 11);
                PreDefTableEntry(pdchLeedSchStPt11amWednesday, dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, setPointAt11);
                PreDefTableEntry(pdchLeedSchStPt11amWedCnt, dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedIndex, true, wednesday, 23);
                PreDefTableEntry(pdchLeedSchStPt11pmWednesday, dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, setPointAt23);
                PreDefTableEntry(pdchLeedSchStPt11pmWedCnt, dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, numDays);

                PreDefTableEntry(pdChLeedSchStPtMonthUsed, dataZoneTempPredictorCorrector.SetPointSingleCooling(SingleTempCoolingControlNum).TempSchedName, monthAssumed);
            }
        }
        for (int SingleTempHeatCoolControlNum = 1; SingleTempHeatCoolControlNum <= dataZoneTempPredictorCorrector.NumSingleTempHeatCoolControls; ++SingleTempHeatCoolControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex);
                PreDefTableEntry(pdChLeedSchStPtFirstObjUsed,
                                 dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName,
                                 dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).Name);

                std::string schNm = dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName + " (summer)";
                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, true, wednesday, 11);
                PreDefTableEntry(pdchLeedSchStPt11amWednesday, schNm, setPointAt11);
                PreDefTableEntry(pdchLeedSchStPt11amWedCnt, schNm, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, true, wednesday, 23);
                PreDefTableEntry(pdchLeedSchStPt11pmWednesday, schNm, setPointAt23);
                PreDefTableEntry(pdchLeedSchStPt11pmWedCnt, schNm, numDays);

                schNm = dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName + " (winter)";
                std::tie(setPointAt11, numDays, monthAssumed2) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, false, wednesday, 11);
                PreDefTableEntry(pdchLeedSchStPt11amWednesday, schNm, setPointAt11);
                PreDefTableEntry(pdchLeedSchStPt11amWedCnt, schNm, numDays);

                std::tie(setPointAt23, numDays, monthAssumed2) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedIndex, false, wednesday, 23);
                PreDefTableEntry(pdchLeedSchStPt11pmWednesday, schNm, setPointAt23);
                PreDefTableEntry(pdchLeedSchStPt11pmWedCnt, schNm, numDays);

                PreDefTableEntry(pdChLeedSchStPtMonthUsed,
                                 dataZoneTempPredictorCorrector.SetPointSingleHeatCool(SingleTempHeatCoolControlNum).TempSchedName,
                                 monthAssumed + " and " + monthAssumed2);
            }
        }
        for (int DualTempHeatCoolControlNum = 1; DualTempHeatCoolControlNum <= dataZoneTempPredictorCorrector.NumDualTempHeatCoolControls; ++DualTempHeatCoolControlNum) {
            if (std::find(uniqSch.begin(), uniqSch.end(), dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex);
                PreDefTableEntry(pdChLeedSchStPtFirstObjUsed,
                                 dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName,
                                 dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex, false, wednesday, 11);
                PreDefTableEntry(pdchLeedSchStPt11amWednesday, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, setPointAt11);
                PreDefTableEntry(pdchLeedSchStPt11amWedCnt, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSchedIndex, false, wednesday, 23);
                PreDefTableEntry(pdchLeedSchStPt11pmWednesday, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, setPointAt23);
                PreDefTableEntry(pdchLeedSchStPt11pmWedCnt, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, numDays);

                PreDefTableEntry(pdChLeedSchStPtMonthUsed, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).HeatTempSetptSchedName, monthAssumed);
            }
            if (std::find(uniqSch.begin(), uniqSch.end(), dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex) == uniqSch.end()) {
                uniqSch.emplace_back(dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex);
                PreDefTableEntry(pdChLeedSchStPtFirstObjUsed,
                                 dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName,
                                 dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).Name);

                std::tie(setPointAt11, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex, true, wednesday, 11);
                PreDefTableEntry(pdchLeedSchStPt11amWednesday, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, setPointAt11);
                PreDefTableEntry(pdchLeedSchStPt11amWedCnt, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, numDays);

                std::tie(setPointAt23, numDays, monthAssumed) =
                    temperatureAndCountInSch(dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSchedIndex, true, wednesday, 23);
                PreDefTableEntry(pdchLeedSchStPt11pmWednesday, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, setPointAt23);
                PreDefTableEntry(pdchLeedSchStPt11pmWedCnt, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, numDays);

                PreDefTableEntry(pdChLeedSchStPtMonthUsed, dataZoneTempPredictorCorrector.SetPointDualHeatCool(DualTempHeatCoolControlNum).CoolTempSetptSchedName, monthAssumed);
            }
        }
    }

    // returns the temperature value from a schedule at a certain time for the first day of the week in either January or July
    std::tuple<Real64, int, std::string>
    temperatureAndCountInSch(int const &scheduleIndex, bool const &isSummer, int const &dayOfWeek, int const &hourOfDay)
    {
        // J.Glazer - Aug 2017

        // determine month to use based on hemiphere and season
        int monthToUse;
        if (isSummer) {
            if (DataEnvironment::Latitude > 0.) {
                monthToUse = 7; // July - summer in northern hemisphere
            } else {
                monthToUse = 1; // January - summer in southern hemisphere
            }
        } else {
            if (DataEnvironment::Latitude > 0.) {
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

        int jdateSelect = General::nthDayOfWeekOfMonth(dayOfWeek, 1, monthToUse);

        // determine number of days in year
        int DaysInYear;
        if (DataEnvironment::CurrentYearIsLeapYear) {
            DaysInYear = 366;
        } else {
            DaysInYear = 365;
        }

        // should adjust date if lands on a holiday but for now assume that it does not

        // adjust time of day for daylight savings time
        int hourSelect = hourOfDay + WeatherManager::DSTIndex(jdateSelect);

        // get the value at the selected time
        int const firstTimeStep = 1;
        int weekSchIndexSelect = ScheduleManager::Schedule(scheduleIndex).WeekSchedulePointer(jdateSelect);
        int daySchIndexSelect = ScheduleManager::WeekSchedule(weekSchIndexSelect).DaySchedulePointer(dayOfWeek);
        Real64 valueAtSelectTime = ScheduleManager::DaySchedule(daySchIndexSelect).TSValue(firstTimeStep, hourSelect);
        int countOfSame = 0;

        // count the number of times with that same value
        for (int jdateOfYear = 1; jdateOfYear <= DaysInYear; ++jdateOfYear) {
            int wkSch = ScheduleManager::Schedule(scheduleIndex).WeekSchedulePointer(jdateOfYear);
            if (wkSch == weekSchIndexSelect) { // if same week schedule can short circuit rest of testing and increment counter
                ++countOfSame;
            } else {
                int daySch = ScheduleManager::WeekSchedule(wkSch).DaySchedulePointer(dayOfWeek);
                if (daySch == daySchIndexSelect) { // if same day schedule can short circuit rest of testing and increment counter
                    ++countOfSame;
                } else {
                    Real64 valueAt = ScheduleManager::DaySchedule(daySch).TSValue(firstTimeStep, hourSelect);
                    if (valueAt == valueAtSelectTime) {
                        ++countOfSame;
                    }
                }
            }
        }

        return std::make_tuple(valueAtSelectTime, countOfSame, monthName);
    }

} // namespace ZoneTempPredictorCorrector

} // namespace EnergyPlus
