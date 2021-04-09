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
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace HybridModel {

    // MODULE INFORMATION:
    //       AUTHOR         Sang Hoon Lee, Tianzhen Hong, Rongpeng Zhang. LBNL
    //       DATE WRITTEN   Oct 2015

    // PURPOSE OF THIS MODULE:
    // This module manages hybrid model.

    // METHODOLOGY EMPLOYED:
    //  The model uses measured zone air temperature to calculate internal thermal mass or infiltration air flow rate.

    // USE STATEMENTS:

    // Using/Aliasing
    using namespace DataHeatBalance;
    using namespace DataRoomAirModel;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    // Object Data

    // Functions

    void GetHybridModelZone(EnergyPlusData &state)
    {

        using ScheduleManager::GetScheduleIndex;

        bool ErrorsFound(false); // If errors detected in input
        Array1D_bool lAlphaFieldBlanks(16, false);
        Array1D_bool lNumericFieldBlanks(4, false);
        int NumAlphas;  // Number of Alphas for each GetobjectItem call
        int NumNumbers; // Number of Numbers for each GetobjectItem call
        int IOStatus;
        int ZonePtr;                     // Pointer to the zone
        int ZoneListPtr;                 // Pointer to the zone list
        std::string CurrentModuleObject; // to assist in getting input
        Array1D_string cAlphaArgs(16);   // Alpha input items for object
        Array1D_string cAlphaFieldNames(16);
        Array1D_string cNumericFieldNames(16);
        Array1D<Real64> rNumericArgs(4); // Numeric input items for object
        int HybridModelStartMonth(0);    // Hybrid model start month
        int HybridModelStartDate(0);     // Hybrid model start date of month
        int HybridModelEndMonth(0);      // Hybrid model end month
        int HybridModelEndDate(0);       // Hybrid model end date of month
        int HMStartDay(0);
        int HMEndDay(0);

        int TemperatureSchPtr(0);      // Temperature schedule pointer
        int HumidityRatioSchPtr(0);    // Humidity ratio schedule pointer
        int CO2ConcentrationSchPtr(0); // CO2 concentration schedule pointer

        int PeopleActivityLevelSchPtr(0);    // People activity level schedule pointer
        int PeopleSensibleFractionSchPtr(0); // People sensible heat portion schedule pointer
        int PeopleRadiantFractionSchPtr(0);  // People radiant heat portion (of sensible heat) schedule pointer
        int PeopleCO2GenRateSchPtr(0);       // People CO2 generation rate schedule pointer

        int SupplyAirTemperatureSchPtr(0);
        int SupplyAirMassFlowRateSchPtr(0);
        int SupplyAirHumidityRatioSchPtr(0);
        int SupplyAirCO2ConcentrationSchPtr(0);

        // Read hybrid model input
        CurrentModuleObject = "HybridModel:Zone";
        state.dataHybridModel->NumOfHybridModelZones = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHybridModel->HybridModelZone.allocate(state.dataGlobal->NumOfZones);

        if (state.dataHybridModel->NumOfHybridModelZones > 0) {

            for (int HybridModelNum = 1; HybridModelNum <= state.dataHybridModel->NumOfHybridModelZones; ++HybridModelNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         HybridModelNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                ZoneListPtr = 0;
                ZonePtr =
                    UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone); // "Zone" is a 1D array, cAlphaArgs(2) is the zone name
                if (ZonePtr == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                    ZoneListPtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
                if (ZonePtr > 0) {
                    state.dataHybridModel->HybridModelZone(ZonePtr).Name = cAlphaArgs(1);                          // Zone HybridModel name
                    state.dataHybridModel->FlagHybridModel_TM = UtilityRoutines::SameString(cAlphaArgs(3), "Yes"); // Calculate thermal mass option
                    state.dataHybridModel->FlagHybridModel_AI =
                        UtilityRoutines::SameString(cAlphaArgs(4), "Yes"); // Calculate infiltration rate option
                    state.dataHybridModel->FlagHybridModel_PC = UtilityRoutines::SameString(cAlphaArgs(5), "Yes"); // Calculate people count option

                    // Pointers used to help decide which unknown parameter to solve
                    // Zone Air Infiltration Rate and Zone Internal Thermal Mass calculations cannot be performed simultaneously
                    TemperatureSchPtr = GetScheduleIndex(state, cAlphaArgs(6));
                    HumidityRatioSchPtr = GetScheduleIndex(state, cAlphaArgs(7));
                    CO2ConcentrationSchPtr = GetScheduleIndex(state, cAlphaArgs(8));

                    // Not used for now
                    PeopleActivityLevelSchPtr = GetScheduleIndex(state, cAlphaArgs(9));
                    PeopleSensibleFractionSchPtr = GetScheduleIndex(state, cAlphaArgs(10));
                    PeopleRadiantFractionSchPtr = GetScheduleIndex(state, cAlphaArgs(11));
                    PeopleCO2GenRateSchPtr = GetScheduleIndex(state, cAlphaArgs(12));

                    // Pointers used to help decide wheather to include system supply terms in the inverse algorithms
                    SupplyAirTemperatureSchPtr = GetScheduleIndex(state, cAlphaArgs(13));
                    SupplyAirMassFlowRateSchPtr = GetScheduleIndex(state, cAlphaArgs(14));
                    SupplyAirHumidityRatioSchPtr = GetScheduleIndex(state, cAlphaArgs(15));
                    SupplyAirCO2ConcentrationSchPtr = GetScheduleIndex(state, cAlphaArgs(16));

                    /*  Note: Internal thermal mass can be calculated only with measured temperature.
                                      Air infiltration rate can be calculated with either measured temperature, humifity ratio, or CO2 concentration.
                                      People count can be calculated with either measured temperature, humifity ratio, or CO2 concentration.
                    */
                    // Initially set all flags to be false
                    state.dataHybridModel->HybridModelZone(ZonePtr).InternalThermalMassCalc_T = false;
                    state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T = false;
                    state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_H = false;
                    state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_C = false;
                    state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T = false;
                    state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H = false;
                    state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C = false;

                    // Scenario 1: Only one unknown parameter to solve
                    // Scenario 1-1: To solve thermal mass
                    if (state.dataHybridModel->FlagHybridModel_TM) {
                        if (state.dataHybridModel->FlagHybridModel_AI) {
                            ShowSevereError(state,
                                            "Field \"" + cAlphaFieldNames(3) + " and " + cAlphaFieldNames(4) + "\" cannot be both set to YES.");
                            ErrorsFound = true;
                        }

                        if (state.dataHybridModel->FlagHybridModel_PC) {
                            ShowSevereError(state,
                                            "Field \"" + cAlphaFieldNames(3) + " and " + cAlphaFieldNames(5) + "\" cannot be both set to YES.");
                            ErrorsFound = true;
                        }

                        if (TemperatureSchPtr == 0) {
                            ShowSevereError(state, "Measured Zone Air Tempearture Schedule is not defined for: " + CurrentModuleObject);
                            ErrorsFound = true;
                        } else {
                            state.dataHybridModel->HybridModelZone(ZonePtr).InternalThermalMassCalc_T = true;
                        }
                    }

                    // Scenario 1-2: To solve infiltration rate
                    if (state.dataHybridModel->FlagHybridModel_AI) {
                        if (state.dataHybridModel->FlagHybridModel_PC) {
                            ShowSevereError(state,
                                            "Field \"" + cAlphaFieldNames(4) + "\" and \"" + cAlphaFieldNames(5) + "\" cannot be both set to YES.");
                            ErrorsFound = true;
                        }
                        if (TemperatureSchPtr == 0 && HumidityRatioSchPtr == 0 && CO2ConcentrationSchPtr == 0) {
                            // Show fatal error if no measurement schedule is provided
                            ShowSevereError(state, "No measured envrionmental parameter is provided for: " + CurrentModuleObject);
                            ShowContinueError(state,
                                              "One of the field \"" + cAlphaFieldNames(6) + "\", \"" + cAlphaFieldNames(7) + "\", or " +
                                                  cAlphaFieldNames(8) + "\" must be provided for the HybridModel:Zone.");
                            ErrorsFound = true;
                        } else {
                            if (TemperatureSchPtr > 0 && !state.dataHybridModel->FlagHybridModel_TM) {
                                // Temperature schedule is provided, igonore humidity ratio and CO2 concentration schedules.
                                state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T = true;
                                if (HumidityRatioSchPtr > 0) {
                                    ShowWarningError(state, "Field \"" + cAlphaFieldNames(6) + "\" is provided.");
                                    ShowContinueError(state, "Field \"" + cAlphaFieldNames(7) + "\" will not be used.");
                                }
                                if (CO2ConcentrationSchPtr > 0) {
                                    ShowWarningError(state, "Field \"" + cAlphaFieldNames(6) + "\" is provided.");
                                    ShowContinueError(state, "Field \"" + cAlphaFieldNames(8) + "\" will not be used.");
                                }
                            }
                            if (HumidityRatioSchPtr > 0 && TemperatureSchPtr == 0) {
                                // Humidity ratio schedule is provided, ignore CO2 concentration schedule.
                                state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_H = true;
                                if (CO2ConcentrationSchPtr > 0) {
                                    ShowWarningError(state, "Field \"" + cAlphaFieldNames(7) + "\" is provided.");
                                    ShowContinueError(state, "Field \"" + cAlphaFieldNames(8) + "\" will not be used.");
                                }
                            }
                            if (CO2ConcentrationSchPtr > 0 && TemperatureSchPtr == 0 && HumidityRatioSchPtr == 0) {
                                // Only CO2 concentration schedule is provided.
                                state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_C = true;
                            }
                        }
                    }

                    // Scenario 1-3: To solve people count
                    if (state.dataHybridModel->FlagHybridModel_PC) {
                        if (TemperatureSchPtr == 0 && HumidityRatioSchPtr == 0 && CO2ConcentrationSchPtr == 0) {
                            // Show fatal error if no measurement schedule is provided
                            ShowSevereError(state, "No measured envrionmental parameter is provided for: " + CurrentModuleObject);
                            ShowContinueError(state,
                                              "One of the field \"" + cAlphaFieldNames(6) + "\", \"" + cAlphaFieldNames(7) + "\", or " +
                                                  cAlphaFieldNames(8) + "\" must be provided for the HybridModel:Zone.");
                            ErrorsFound = true;
                        } else {
                            if (TemperatureSchPtr > 0 && !state.dataHybridModel->FlagHybridModel_TM) {
                                // Temperature schedule is provided, igonore humidity ratio and CO2 concentration schedules.
                                state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T = true;
                                if (HumidityRatioSchPtr > 0) {
                                    ShowWarningError(
                                        state,
                                        "The meausured air humidity ratio schedule will not be used since measured air temperature is provided.");
                                }
                                if (CO2ConcentrationSchPtr > 0) {
                                    ShowWarningError(
                                        state,
                                        "The meausured air CO2 concentration schedule will not be used since measured air temperature is provided.");
                                }
                            }
                            if (HumidityRatioSchPtr > 0 && TemperatureSchPtr == 0) {
                                // Humidity ratio schedule is provided, ignore CO2 concentration schedule.
                                state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H = true;
                                if (CO2ConcentrationSchPtr > 0) {
                                    ShowWarningError(state,
                                                     "The meausured air CO2 concentration schedule will not be used since measured air humidity "
                                                     "ratio is provided.");
                                }
                            }
                            if (CO2ConcentrationSchPtr > 0 && TemperatureSchPtr == 0 && HumidityRatioSchPtr == 0) {
                                // Only CO2 concentration schedule is provided.
                                state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C = true;
                            }
                        }
                    }

                    // Decide if system supply terms are valid to be included in the inverse solution
                    if (SupplyAirTemperatureSchPtr > 0 && SupplyAirMassFlowRateSchPtr > 0 && SupplyAirHumidityRatioSchPtr) {
                        if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T ||
                            state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).IncludeSystemSupplyParameters = true;
                        } else {
                            ShowWarningError(state,
                                             "Field \"" + cAlphaFieldNames(13) + "\", " + cAlphaFieldNames(14) + ", and \"" + cAlphaFieldNames(15) +
                                                 "\" will not be used in the inverse balance euqation.");
                        }
                    }

                    if (SupplyAirHumidityRatioSchPtr && SupplyAirMassFlowRateSchPtr > 0) {
                        if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_H ||
                            state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).IncludeSystemSupplyParameters = true;
                        } else {
                            ShowWarningError(state,
                                             "Field \"" + cAlphaFieldNames(15) + "\" and \"" + cAlphaFieldNames(14) +
                                                 "\" will not be used in the inverse balance euqation.");
                        }
                    }

                    if (SupplyAirCO2ConcentrationSchPtr > 0 && SupplyAirMassFlowRateSchPtr > 0) {
                        if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_C ||
                            state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).IncludeSystemSupplyParameters = true;
                        } else {
                            ShowWarningError(state,
                                             "Field \"" + cAlphaFieldNames(16) + "\" and \"" + cAlphaFieldNames(14) +
                                                 "\" will not be used in the inverse balance euqation.");
                        }
                    }

                    // Flags showing Hybrid Modeling settings
                    state.dataHybridModel->FlagHybridModel = state.dataHybridModel->HybridModelZone(ZonePtr).InternalThermalMassCalc_T ||
                                                             state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T ||
                                                             state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_H ||
                                                             state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_C ||
                                                             state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T ||
                                                             state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H ||
                                                             state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C;

                    if (state.dataHybridModel->HybridModelZone(ZonePtr).InternalThermalMassCalc_T ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T) {
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureSchedulePtr = GetScheduleIndex(state, cAlphaArgs(6));
                    }

                    if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_H ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H) {
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredHumidityRatioSchedulePtr = GetScheduleIndex(state, cAlphaArgs(7));
                    }

                    if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_C ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C) {
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredCO2ConcentrationSchedulePtr =
                            GetScheduleIndex(state, cAlphaArgs(8));
                    }

                    if (state.dataHybridModel->HybridModelZone(ZonePtr).IncludeSystemSupplyParameters) {
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneSupplyAirTemperatureSchedulePtr = GetScheduleIndex(state, cAlphaArgs(13));
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneSupplyAirMassFlowRateSchedulePtr =
                            GetScheduleIndex(state, cAlphaArgs(14));
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneSupplyAirHumidityRatioSchedulePtr =
                            GetScheduleIndex(state, cAlphaArgs(15));
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneSupplyAirCO2ConcentrationSchedulePtr =
                            GetScheduleIndex(state, cAlphaArgs(16));
                    }

                    // Get optional people related schedules
                    if (state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C) {
                        if (PeopleActivityLevelSchPtr > 0) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).ZonePeopleActivityLevelSchedulePtr =
                                GetScheduleIndex(state, cAlphaArgs(9));
                        } else {
                            ShowWarningError(state,
                                             "Field \"" + cAlphaFieldNames(9) +
                                                 "\": default people activity level is not provided, default value of 130W/person will be used.");
                        }
                        if (PeopleSensibleFractionSchPtr > 0) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).ZonePeopleSensibleFractionSchedulePtr =
                                GetScheduleIndex(state, cAlphaArgs(10));
                        } else {
                            ShowWarningError(state,
                                             "Field \"" + cAlphaFieldNames(10) +
                                                 "\": default people sensible heat rate is not provided, default value of 0.6 will be used.");
                        }
                        if (PeopleRadiantFractionSchPtr > 0) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).ZonePeopleRadiationFractionSchedulePtr =
                                GetScheduleIndex(state, cAlphaArgs(11));
                        } else {
                            ShowWarningError(
                                state,
                                "Field \"" + cAlphaFieldNames(11) +
                                    "\": default people radiant heat portion (of sensible heat) is not provided, default value of 0.7 will be used.");
                        }
                        if (PeopleCO2GenRateSchPtr > 0) {
                            state.dataHybridModel->HybridModelZone(ZonePtr).ZonePeopleCO2GenRateSchedulePtr = GetScheduleIndex(state, cAlphaArgs(12));
                        } else {
                            ShowWarningError(
                                state,
                                "Field \"" + cAlphaFieldNames(12) +
                                    "\": default people CO2 generation rate is not provided, default value of 0.0000000382 kg/W will be used.");
                        }
                    }

                    if (state.dataHybridModel->FlagHybridModel) {
                        // prepare start and end date for Hybrid Modeling
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureStartMonth = rNumericArgs(1);
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureStartDate = rNumericArgs(2);
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureEndMonth = rNumericArgs(3);
                        state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureEndDate = rNumericArgs(4);
                        {
                            int HMDayArr[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

                            HybridModelStartMonth = state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureStartMonth;
                            HybridModelStartDate = state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureStartDate;
                            HybridModelEndMonth = state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureEndMonth;
                            HybridModelEndDate = state.dataHybridModel->HybridModelZone(ZonePtr).ZoneMeasuredTemperatureEndDate;

                            if (HybridModelStartMonth >= 1 && HybridModelStartMonth <= 12) {
                                HMStartDay = HMDayArr[HybridModelStartMonth - 1];
                            } else {
                                HMStartDay = 0;
                            }

                            if (HybridModelEndMonth >= 1 && HybridModelEndMonth <= 12) {
                                HMEndDay = HMDayArr[HybridModelEndMonth - 1];
                            } else {
                                HMEndDay = 0;
                            }

                            state.dataHybridModel->HybridModelZone(ZonePtr).HybridStartDayOfYear = HMStartDay + HybridModelStartDate;
                            state.dataHybridModel->HybridModelZone(ZonePtr).HybridEndDayOfYear = HMEndDay + HybridModelEndDate;
                        }
                    }

                    // Output variable
                    if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_H ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_C) {
                        SetupOutputVariable(state,
                                            "Zone Infiltration Hybrid Model Air Change Rate",
                                            OutputProcessor::Unit::ach,
                                            state.dataHeatBal->Zone(ZonePtr).InfilOAAirChangeRateHM,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Infiltration Hybrid Model Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            state.dataHeatBal->Zone(ZonePtr).MCPIHM,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(ZonePtr).Name);
                    }
                    if (state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_T ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_H ||
                        state.dataHybridModel->HybridModelZone(ZonePtr).PeopleCountCalc_C) {
                        SetupOutputVariable(state,
                                            "Zone Hybrid Model People Count",
                                            OutputProcessor::Unit::None,
                                            state.dataHeatBal->Zone(ZonePtr).NumOccHM,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(ZonePtr).Name);
                    }

                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                        "\" not found.");
                    ErrorsFound = true;
                }
            }

            // ZoneAirMassFlowConservation should not be activated during the Hybrid Modeling infiltration calculations
            if (state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T && state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance = false;
                ShowWarningError(state, "ZoneAirMassFlowConservation is deactivated when Hybrid Modeling is performed.");
            }

            // RoomAirModelType should be Mixing if Hybrid Modeling is performed for the zone
            if (state.dataHybridModel->FlagHybridModel) {
                for (ZonePtr = 1; ZonePtr <= state.dataGlobal->NumOfZones; ZonePtr++) {
                    if ((state.dataHybridModel->HybridModelZone(ZonePtr).InternalThermalMassCalc_T ||
                         state.dataHybridModel->HybridModelZone(ZonePtr).InfiltrationCalc_T) &&
                        (state.dataRoomAirMod->AirModel(ZonePtr).AirModelType != DataRoomAirModel::RoomAirModel::Mixing)) {
                        state.dataRoomAirMod->AirModel(ZonePtr).AirModelType = DataRoomAirModel::RoomAirModel::Mixing;
                        ShowWarningError(state, "Room Air Model Type should be Mixing if Hybrid Modeling is performed for the zone.");
                    }
                }
            }

            if (ErrorsFound) {
                ShowFatalError(state, "Errors getting Hybrid Model input data. Preceding condition(s) cause termination.");
            }
        }
    }

    // Needed for unit tests, should not be normally called.

} // namespace HybridModel

} // namespace EnergyPlus
