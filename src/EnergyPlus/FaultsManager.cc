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

// EnergyPlus Headers
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/ChillerAbsorption.hh>
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
#include <EnergyPlus/ChillerReformulatedEIR.hh>
#include <EnergyPlus/CondenserLoopTowers.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

namespace FaultsManager {

    // MODULE INFORMATION:
    //       AUTHOR         Tianzhen Hong, LBNL
    //       DATE WRITTEN   August 2013
    //       MODIFIED       Sep. 2013, Xiufeng Pang (XP), LBNL. Added Fouling Coil fault
    //                      Feb. 2015, Rongpeng Zhang, LBNL. Added Thermostat/Humidistat Offset faults
    //                      Apr. 2015, Rongpeng Zhang, LBNL. Added Fouling Air Filter fault
    //                      May. 2016, Rongpeng Zhang, LBNL. Added Chiller/Condenser Supply Water Temperature Sensor fault
    //                      Jun. 2016, Rongpeng Zhang, LBNL. Added Tower Scaling fault
    //                      Jul. 2016, Rongpeng Zhang, LBNL. Added Coil Supply Air Temperature Sensor fault
    //                      Oct. 2016, Rongpeng Zhang, LBNL. Added Fouling Boiler fault
    //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault
    //                      Jan. 2017, Rongpeng Zhang, LBNL. Added Fouling Evaporative Cooler fault
    //       RE-ENGINEERED

    // PURPOSE OF THIS MODULE:
    // This module manages operational faults of buildings and systems.

    // METHODOLOGY EMPLOYED:
    //  Various methods are employed depending types of faults

    // USE STATEMENTS:

    // Using/Aliasing
    // Data
    // MODULE PARAMETER DEFINITIONS

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE TYPE DECLARATIONS:

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this module should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.

    } // namespace

    // FaultTypeEnum

    // Types of faults under Group Operational Faults in IDD
    //  1. Temperature sensor offset (FY14)
    //  2. Humidity sensor offset (FY14)
    //  3. Enthalpy sensor offset (FY14)
    //  4. Fouling coils (FY14)
    //  5. Thermostat offset (FY15)
    //  6. Humidistat offset (FY15)
    //  7. Fouling air filter (FY15)
    //  8. Chiller Supply Water Temperature Sensor Offset (FY16)
    //  9. Condenser Supply Water Temperature Sensor Offset (FY16)
    //  10. Cooling Tower Scaling (FY16)
    //  11. Coil Supply Air Temperature Sensor Offset (FY16)
    // coming ...
    //  Fouling: chillers, boilers, cooling towers
    //  Damper leakage: return air, outdoor air
    //  Blockage: pipe
    //  Meter: air flow, water flow
    //  CO2 sensor
    //  Pressure sensor offset
    //  more

    Array1D_string const cFaults({0, 15},
                                 {"FaultModel:TemperatureSensorOffset:OutdoorAir",
                                  "FaultModel:HumiditySensorOffset:OutdoorAir",
                                  "FaultModel:EnthalpySensorOffset:OutdoorAir",
                                  "FaultModel:TemperatureSensorOffset:ReturnAir",
                                  "FaultModel:EnthalpySensorOffset:ReturnAir",
                                  "FaultModel:Fouling:Coil",
                                  "FaultModel:ThermostatOffset",
                                  "FaultModel:HumidistatOffset",
                                  "FaultModel:Fouling:AirFilter",
                                  "FaultModel:TemperatureSensorOffset:ChillerSupplyWater",
                                  "FaultModel:TemperatureSensorOffset:CondenserSupplyWater",
                                  "FaultModel:Fouling:CoolingTower",
                                  "FaultModel:TemperatureSensorOffset:CoilSupplyAir",
                                  "FaultModel:Fouling:Boiler",
                                  "FaultModel:Fouling:Chiller",
                                  "FaultModel:Fouling:EvaporativeCooler"});
    //      'FaultModel:PressureSensorOffset:OutdoorAir   ', &
    //      'FaultModel:TemperatureSensorOffset:SupplyAir ', &
    //      'FaultModel:TemperatureSensorOffset:ZoneAir   ', &
    //      'FaultModel:Blockage:Branch                   ', &
    //      'FaultModel:Fouling:Chiller                   ', &
    //      'FaultModel:Fouling:Boiler                    ', &
    //      'FaultModel:DamperLeakage:ReturnAir           ', &
    //      'FaultModel:DamperLeakage:OutdoorAir          ' /)

    void CheckAndReadFaults(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tianzhen Hong, LBNL
        //       DATE WRITTEN   August 2013
        //       MODIFIED       Sep. 2013, Xiufeng Pang (XP), LBNL. Added Fouling Coil fault
        //                      Feb. 2015, Rongpeng Zhang, LBNL. Added Thermostat/Humidistat Offset faults
        //                      Apr. 2015, Rongpeng Zhang, LBNL. Added Fouling Air Filter fault
        //                      May. 2016, Rongpeng Zhang, LBNL. Added Chiller/Condenser Supply Water Temperature Sensor fault
        //                      Jun. 2016, Rongpeng Zhang, LBNL. Added Tower Scaling fault
        //                      Jul. 2016, Rongpeng Zhang, LBNL. Added Coil Supply Air Temperature Sensor fault
        //                      Oct. 2016, Rongpeng Zhang, LBNL. Added Fouling Boiler fault
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault
        //                      Jan. 2017, Rongpeng Zhang, LBNL. Added Fouling Evaporative Cooler fault
        //
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        //  1. Determine if any operational faults are present in a model and set flags
        //  2. Read faults input

        // METHODOLOGY EMPLOYED:
        // Get number of faults-related input objects and assign faults input to data structure

        // Using/Aliasing
        using CurveManager::GetCurveIndex;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;  // Number of Alphas for each GetobjectItem call
        int NumNumbers; // Number of Numbers for each GetobjectItem call
        int IOStatus;
        Array1D_string cAlphaArgs(10); // Alpha input items for object
        Array1D_bool lAlphaFieldBlanks(10, false);
        Array1D_bool lNumericFieldBlanks(10, false);
        Array1D_string cAlphaFieldNames(10);
        Array1D_string cNumericFieldNames(10);
        Array1D<Real64> rNumericArgs(10); // Numeric input items for object
        std::string cFaultCurrentObject;

        if (state.dataFaultsMgr->RunFaultMgrOnceFlag) return;

        // check number of faults
        state.dataFaultsMgr->NumFaults = 0;
        state.dataFaultsMgr->NumFaultyEconomizer = 0;
        for (int NumFaultsTemp = 0, i = 0; i <= 15; ++i) {
            NumFaultsTemp = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cFaults(i));
            state.dataFaultsMgr->NumFaults += NumFaultsTemp;

            if (i <= 4) {
                // 1st-5th fault: economizer sensor offset
                state.dataFaultsMgr->NumFaultyEconomizer += NumFaultsTemp;
            } else if (i == 5) {
                // 6th fault: Coil fouling
                state.dataFaultsMgr->NumFouledCoil = NumFaultsTemp;
            } else if (i == 6) {
                // 7th fault: Faulty thermostat
                state.dataFaultsMgr->NumFaultyThermostat = NumFaultsTemp;
            } else if (i == 7) {
                // 8th fault: Faulty humidistat
                state.dataFaultsMgr->NumFaultyHumidistat = NumFaultsTemp;
            } else if (i == 8) {
                // 9th fault: Fouled air filter
                state.dataFaultsMgr->NumFaultyAirFilter = NumFaultsTemp;
            } else if (i == 9) {
                // 10th fault: Faulty Chillers Supply Water Temperature Sensor
                state.dataFaultsMgr->NumFaultyChillerSWTSensor = NumFaultsTemp;
            } else if (i == 10) {
                // 11th fault: Faulty Condenser Supply Water Temperature Sensor
                state.dataFaultsMgr->NumFaultyCondenserSWTSensor = NumFaultsTemp;
            } else if (i == 11) {
                // 12th fault: Faulty Towers with Scaling
                state.dataFaultsMgr->NumFaultyTowerFouling = NumFaultsTemp;
            } else if (i == 12) {
                // 13th fault: Faulty Coil Supply Air Temperature Sensor
                state.dataFaultsMgr->NumFaultyCoilSATSensor = NumFaultsTemp;
            } else if (i == 13) {
                // 14th fault: Faulty Boiler with Fouling
                state.dataFaultsMgr->NumFaultyBoilerFouling = NumFaultsTemp;
            } else if (i == 14) {
                // 15th fault: Faulty Chiller with Fouling
                state.dataFaultsMgr->NumFaultyChillerFouling = NumFaultsTemp;
            } else if (i == 15) {
                // 16th fault: Faulty Evaporative Cooler with Fouling
                state.dataFaultsMgr->NumFaultyEvapCoolerFouling = NumFaultsTemp;
            }
        }

        if (state.dataFaultsMgr->NumFaults > 0) {
            state.dataFaultsMgr->AnyFaultsInModel = true;
        } else {
            state.dataFaultsMgr->AnyFaultsInModel = false;
        }

        if (!state.dataFaultsMgr->AnyFaultsInModel) {
            state.dataFaultsMgr->RunFaultMgrOnceFlag = true;
            return;
        }

        // allocate fault array
        if (state.dataFaultsMgr->NumFaultyEconomizer > 0) state.dataFaultsMgr->FaultsEconomizer.allocate(state.dataFaultsMgr->NumFaultyEconomizer);
        if (state.dataFaultsMgr->NumFouledCoil > 0) state.dataFaultsMgr->FouledCoils.allocate(state.dataFaultsMgr->NumFouledCoil);
        if (state.dataFaultsMgr->NumFaultyThermostat > 0)
            state.dataFaultsMgr->FaultsThermostatOffset.allocate(state.dataFaultsMgr->NumFaultyThermostat);
        if (state.dataFaultsMgr->NumFaultyHumidistat > 0)
            state.dataFaultsMgr->FaultsHumidistatOffset.allocate(state.dataFaultsMgr->NumFaultyHumidistat);
        if (state.dataFaultsMgr->NumFaultyAirFilter > 0)
            state.dataFaultsMgr->FaultsFouledAirFilters.allocate(state.dataFaultsMgr->NumFaultyAirFilter);
        if (state.dataFaultsMgr->NumFaultyChillerSWTSensor > 0)
            state.dataFaultsMgr->FaultsChillerSWTSensor.allocate(state.dataFaultsMgr->NumFaultyChillerSWTSensor);
        if (state.dataFaultsMgr->NumFaultyCondenserSWTSensor > 0)
            state.dataFaultsMgr->FaultsCondenserSWTSensor.allocate(state.dataFaultsMgr->NumFaultyCondenserSWTSensor);
        if (state.dataFaultsMgr->NumFaultyTowerFouling > 0)
            state.dataFaultsMgr->FaultsTowerFouling.allocate(state.dataFaultsMgr->NumFaultyTowerFouling);
        if (state.dataFaultsMgr->NumFaultyCoilSATSensor > 0)
            state.dataFaultsMgr->FaultsCoilSATSensor.allocate(state.dataFaultsMgr->NumFaultyCoilSATSensor);
        if (state.dataFaultsMgr->NumFaultyBoilerFouling > 0)
            state.dataFaultsMgr->FaultsBoilerFouling.allocate(state.dataFaultsMgr->NumFaultyBoilerFouling);
        if (state.dataFaultsMgr->NumFaultyChillerFouling > 0)
            state.dataFaultsMgr->FaultsChillerFouling.allocate(state.dataFaultsMgr->NumFaultyChillerFouling);
        if (state.dataFaultsMgr->NumFaultyEvapCoolerFouling > 0)
            state.dataFaultsMgr->FaultsEvapCoolerFouling.allocate(state.dataFaultsMgr->NumFaultyEvapCoolerFouling);

        // read faults input of Evaporative Cooler Fouling
        for (int jFault_EvapCoolerFouling = 1; jFault_EvapCoolerFouling <= state.dataFaultsMgr->NumFaultyEvapCoolerFouling;
             ++jFault_EvapCoolerFouling) {

            cFaultCurrentObject = cFaults(15); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_EvapCoolerFouling,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).FaultTypeEnum = Fault::Fouling_EvapCooler;
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).FoulingFactor = rNumericArgs(1);

            // Evaporative cooler type
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Evaporative cooler name
            state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Evaporative cooler check
            {
                auto const SELECT_CASE_VAR(state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerType);

                int EvapCoolerNum;

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "EvaporativeCooler:Indirect:WetCoil")) {
                    // Read in evaporative cooler is not done yet
                    if (state.dataEvapCoolers->GetInputEvapComponentsFlag) {
                        EvaporativeCoolers::GetEvapInput(state);
                        state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
                    }

                    // Check whether the evaporative cooler  name and type match each other;
                    EvapCoolerNum =
                        UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerName,
                                                        state.dataEvapCoolers->EvapCond,
                                                        &EvaporativeCoolers::EvapConditions::EvapCoolerName);
                    if (EvapCoolerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the boiler with the fault model
                        state.dataEvapCoolers->EvapCond(EvapCoolerNum).FaultyEvapCoolerFoulingFlag = true;
                        state.dataEvapCoolers->EvapCond(EvapCoolerNum).FaultyEvapCoolerFoulingIndex = jFault_EvapCoolerFouling;
                    }
                }
            }
        }

        // read faults input of Chiller Fouling
        for (int jFault_ChillerFouling = 1; jFault_ChillerFouling <= state.dataFaultsMgr->NumFaultyChillerFouling; ++jFault_ChillerFouling) {

            cFaultCurrentObject = cFaults(14); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_ChillerFouling,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).FaultTypeEnum = Fault::Fouling_Chiller;
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).FoulingFactor = rNumericArgs(1);

            // Chiller type
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller name
            state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller check
            {
                auto const SELECT_CASE_VAR(state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerType);

                int ChillerNum;

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->ElectricChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->ElectricChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified chiller is not water cooled. The chiller fouling fault model will not be applied.");

                        } else {
                            // Link the chiller with the fault model
                            state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                            state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                        }
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric:EIR")) {
                    // Read in chiller if not done yet
                    if (state.dataChillerElectricEIR->getInputFlag) {
                        ChillerElectricEIR::GetElectricEIRChillerInput(state);
                        state.dataChillerElectricEIR->getInputFlag = false;
                    }

                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName,
                                                                 state.dataChillerElectricEIR->ElectricEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified chiller is not water cooled. The chiller fouling fault model will not be applied.");

                        } else {
                            // Link the chiller with the fault model
                            state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                            state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                        }
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric:ReformulatedEIR")) {

                    // Read in chiller if not done yet
                    if (state.dataChillerReformulatedEIR->GetInputREIR) {
                        ChillerReformulatedEIR::GetElecReformEIRChillerInput(state);
                        state.dataChillerReformulatedEIR->GetInputREIR = false;
                    }

                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName,
                                                                 state.dataChillerReformulatedEIR->ElecReformEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).CondenserType !=
                            DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified chiller is not water cooled. The chiller fouling fault model will not be applied.");

                        } else {
                            // Link the chiller with the fault model
                            state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                            state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                        }
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:ConstantCOP")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->ConstCOPChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->ConstCOPChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified chiller is not water cooled. The chiller fouling fault model will not be applied.");

                        } else {
                            // Link the chiller with the fault model
                            state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                            state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                        }
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:EngineDriven")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->EngineDrivenChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->EngineDrivenChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified chiller is not water cooled. The chiller fouling fault model will not be applied.");

                        } else {
                            // Link the fault model with the water cooled chiller
                            state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                            state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                        }
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:CombustionTurbine")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->GTChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->GTChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified chiller is not water cooled. The chiller fouling fault model will not be applied.");

                        } else {
                            // Link the fault model with the water cooled chiller
                            state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                            state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                        }
                    }
                }
            }
        }

        // read faults input of Boiler Fouling
        for (int jFault_BoilerFouling = 1; jFault_BoilerFouling <= state.dataFaultsMgr->NumFaultyBoilerFouling; ++jFault_BoilerFouling) {

            cFaultCurrentObject = cFaults(13); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_BoilerFouling,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).FaultTypeEnum = Fault::Fouling_Boiler;
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).FoulingFactor = rNumericArgs(1);

            // Boiler type
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).BoilerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Boiler name
            state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).BoilerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Boiler check and link
            {
                if (state.dataBoilers->getBoilerInputFlag) {
                    Boilers::GetBoilerInput(state);
                    state.dataBoilers->getBoilerInputFlag = false;
                }
                // Check the boiler name and boiler type
                int BoilerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling).BoilerName,
                                                                state.dataBoilers->Boiler);
                if (BoilerNum <= 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the boiler with the fault model
                    state.dataBoilers->Boiler(BoilerNum).FaultyBoilerFoulingFlag = true;
                    state.dataBoilers->Boiler(BoilerNum).FaultyBoilerFoulingIndex = jFault_BoilerFouling;
                }
            }
        }

        // read faults input of Coil SAT Sensor Offset
        for (int jFault_CoilSAT = 1; jFault_CoilSAT <= state.dataFaultsMgr->NumFaultyCoilSATSensor; ++jFault_CoilSAT) {

            cFaultCurrentObject = cFaults(12); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_CoilSAT,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).FaultTypeEnum = Fault::TemperatureSensorOffset_CoilSupplyAir;
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // offset - degree of fault
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).Offset = rNumericArgs(1);

            // Coil type
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Coil name
            state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Coil check and link
            {
                auto const SELECT_CASE_VAR(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilType);

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Electric") ||
                    UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Fuel") ||
                    UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Desuperheater")) {
                    // Read in coil input if not done yet
                    if (state.dataHeatingCoils->GetCoilsInputFlag) {
                        HeatingCoils::GetHeatingCoilInput(state);
                        state.dataHeatingCoils->GetCoilsInputFlag = false;
                    }
                    // Check the coil name and coil type
                    int CoilNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName,
                                                                  state.dataHeatingCoils->HeatingCoil);
                    if (CoilNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the coil with the fault model
                        state.dataHeatingCoils->HeatingCoil(CoilNum).FaultyCoilSATFlag = true;
                        state.dataHeatingCoils->HeatingCoil(CoilNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Steam")) {

                    // Read in coil input if not done yet
                    if (state.dataSteamCoils->GetSteamCoilsInputFlag) {
                        SteamCoils::GetSteamCoilInput(state);
                        state.dataSteamCoils->GetSteamCoilsInputFlag = false;
                    }
                    // Check the coil name and coil type
                    int CoilNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName,
                                                                  state.dataSteamCoils->SteamCoil);
                    if (CoilNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {

                        if (state.dataSteamCoils->SteamCoil(CoilNum).TypeOfCoil != state.dataSteamCoils->TemperatureSetPointControl) {
                            // The fault model is only applicable to the coils controlled on leaving air temperature
                            ShowWarningError(
                                state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\". The specified coil is not controlled on leaving air temperature. The coil SAT sensor fault model "
                                    "will not be applied.");
                        } else {
                            // Link the fault model with the coil that is controlled on leaving air temperature
                            state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATFlag = true;
                            state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATIndex = jFault_CoilSAT;
                        }
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Water") ||
                           UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Cooling:Water") ||
                           UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Cooling:Water:Detailedgeometry")) {
                    // Read in coil input if not done yet
                    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
                        WaterCoils::GetWaterCoilInput(state);
                        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
                    }
                    // Check the coil name and coil type
                    int CoilNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName,
                                                                  state.dataWaterCoils->WaterCoil);
                    if (CoilNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }

                    // Read in Water Coil Controller Name
                    state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).WaterCoilControllerName = cAlphaArgs(6);
                    if (lAlphaFieldBlanks(6)) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + " = \"" +
                                            cAlphaArgs(6) + "\" blank.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                    // Read in controller input if not done yet
                    if (state.dataHVACControllers->GetControllerInputFlag) {
                        HVACControllers::GetControllerInput(state);
                        state.dataHVACControllers->GetControllerInputFlag = false;
                    }
                    // Check the controller name
                    int ControlNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).WaterCoilControllerName,
                                                                     state.dataHVACControllers->ControllerProps,
                                                                     &HVACControllers::ControllerPropsType::ControllerName);
                    if (ControlNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + " = \"" +
                                            cAlphaArgs(6) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the controller with the fault model
                        state.dataHVACControllers->ControllerProps(ControlNum).FaultyCoilSATFlag = true;
                        state.dataHVACControllers->ControllerProps(ControlNum).FaultyCoilSATIndex = jFault_CoilSAT;

                        // Check whether the controller match the coil
                        if (state.dataHVACControllers->ControllerProps(ControlNum).SensedNode !=
                            state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum) {
                            ShowSevereError(state,
                                            cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + " = \"" +
                                                cAlphaArgs(6) + "\" does not match " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5));
                            state.dataFaultsMgr->ErrorsFound = true;
                        }
                    }
                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "CoilSystem:Cooling:DX")) {
                    // Read in DXCoolingSystem input if not done yet
                    if (state.dataHVACDXSys->GetInputFlag) {
                        HVACDXSystem::GetDXCoolingSystemInput(state);
                        state.dataHVACDXSys->GetInputFlag = false;
                    }

                    // Check the coil name and coil type
                    int CoilSysNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName,
                                                                     state.dataHVACDXSys->DXCoolingSystem);
                    if (CoilSysNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the coil system with the fault model
                        state.dataHVACDXSys->DXCoolingSystem(CoilSysNum).FaultyCoilSATFlag = true;
                        state.dataHVACDXSys->DXCoolingSystem(CoilSysNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }
                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "CoilSystem:Heating:DX")) {
                    // Read in DXCoolingSystem input if not done yet
                    if (state.dataHVACDXHeatPumpSys->GetInputFlag) {
                        HVACDXHeatPumpSystem::GetDXHeatPumpSystemInput(state);
                        state.dataHVACDXHeatPumpSys->GetInputFlag = false;
                    }

                    // Check the coil name and coil type
                    int CoilSysNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName,
                                                                     state.dataHVACDXHeatPumpSys->DXHeatPumpSystem);
                    if (CoilSysNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the coil system with the fault model
                        state.dataHVACDXHeatPumpSys->DXHeatPumpSystem(CoilSysNum).FaultyCoilSATFlag = true;
                        state.dataHVACDXHeatPumpSys->DXHeatPumpSystem(CoilSysNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }
                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "AirLoopHVAC:UnitarySystem")) {
                    // UnitarySystem model connects to FaultManager via function call to FaultsManager::SetFaultyCoilSATSensor
                }
            }
        } // End read faults input of Fault_type 113

        // read faults input of Cooling tower scaling
        for (int jFault_TowerFouling = 1; jFault_TowerFouling <= state.dataFaultsMgr->NumFaultyTowerFouling; ++jFault_TowerFouling) {

            cFaultCurrentObject = cFaults(11); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_TowerFouling,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).FaultTypeEnum = Fault::Fouling_Tower;
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // UAReductionFactor - degree of fault
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).UAReductionFactor = rNumericArgs(1);

            // Cooling tower type
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).TowerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Cooling tower name
            state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).TowerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Tower check and link
            {
                // Read in tower input if not done yet
                if (state.dataCondenserLoopTowers->GetInput) {
                    CondenserLoopTowers::GetTowerInput(state);
                    state.dataCondenserLoopTowers->GetInput = false;
                }
                // Check the tower name and tower type
                int TowerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).TowerName,
                                                               state.dataCondenserLoopTowers->towers);
                if (TowerNum <= 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the tower with the fault model
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingFlag = true;
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingIndex = jFault_TowerFouling;

                    // Check the faulty tower type
                    if (!UtilityRoutines::SameString(state.dataCondenserLoopTowers->towers(TowerNum).TowerType,
                                                     state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).TowerType)) {
                        ShowWarningError(state,
                                         cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" +
                                             cAlphaArgs(4) + "\" not match the type of " + cAlphaFieldNames(5) +
                                             ". Tower type in the fault model is updated. ");
                        state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling).TowerType =
                            state.dataCondenserLoopTowers->towers(TowerNum).TowerType;
                    }

                    // Check the tower model
                    // Performance Input Method should be UFactorTimesAreaAndDesignWaterFlowRate to apply the fault model
                    if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num != CondenserLoopTowers::PIM::UFactor) {
                        ShowWarningError(state,
                                         cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaFieldNames(5) +
                                             ". Tower Performance Input Method is not UFactorTimesAreaAndDesignWaterFlowRate. " +
                                             "The tower fouling fault model will not be applied to the tower. ");
                        state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingFlag = false;
                    }
                }
            }
        }

        // read faults input of Condenser SWT Sensor Offset
        for (int jFault_CondenserSWT = 1; jFault_CondenserSWT <= state.dataFaultsMgr->NumFaultyCondenserSWTSensor; ++jFault_CondenserSWT) {

            cFaultCurrentObject = cFaults(10); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_CondenserSWT,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).FaultTypeEnum = Fault::TemperatureSensorOffset_CondenserSupplyWater;
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // offset - degree of fault
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).Offset = rNumericArgs(1);

            // Cooling tower type
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Cooling tower name
            state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Tower check and link
            {
                // Read in tower input if not done yet
                if (state.dataCondenserLoopTowers->GetInput) {
                    CondenserLoopTowers::GetTowerInput(state);
                    state.dataCondenserLoopTowers->GetInput = false;
                }
                // Check the tower name and tower type
                int TowerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerName,
                                                               state.dataCondenserLoopTowers->towers);
                if (TowerNum <= 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the tower with the fault model
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyCondenserSWTFlag = true;
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyCondenserSWTIndex = jFault_CondenserSWT;

                    // Check the faulty tower type
                    if (!UtilityRoutines::SameString(state.dataCondenserLoopTowers->towers(TowerNum).TowerType,
                                                     state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerType)) {
                        ShowWarningError(state,
                                         cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" +
                                             cAlphaArgs(4) + "\" not match the type of " + cAlphaFieldNames(5) + ". Tower type is updated. ");
                        state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerType =
                            state.dataCondenserLoopTowers->towers(TowerNum).TowerType;
                    }
                }
            }
        }

        // read faults input of Chiller SWT Sensor Offset
        for (int jFault_ChillerSWT = 1; jFault_ChillerSWT <= state.dataFaultsMgr->NumFaultyChillerSWTSensor; ++jFault_ChillerSWT) {

            cFaultCurrentObject = cFaults(9); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_ChillerSWT,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).FaultTypeEnum = Fault::TemperatureSensorOffset_ChillerSupplyWater;
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).Name = cAlphaArgs(1);

            // Fault availability schedule
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // offset - degree of fault
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).Offset = rNumericArgs(1);

            // Chiller type
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller name
            state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller check
            {
                auto const SELECT_CASE_VAR(state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerType);

                int ChillerNum;

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->ElectricChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric:EIR")) {
                    // Read in chiller if not done yet
                    if (state.dataChillerElectricEIR->getInputFlag) {
                        ChillerElectricEIR::GetElectricEIRChillerInput(state);
                        state.dataChillerElectricEIR->getInputFlag = false;
                    }
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerElectricEIR->ElectricEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric:ReformulatedEIR")) {
                    // Read in chiller if not done yet
                    if (state.dataChillerReformulatedEIR->GetInputREIR) {
                        ChillerReformulatedEIR::GetElecReformEIRChillerInput(state);
                        state.dataChillerReformulatedEIR->GetInputREIR = false;
                    }
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerReformulatedEIR->ElecReformEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:EngineDriven")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->EngineDrivenChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:CombustionTurbine")) {
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->GTChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:ConstantCOP")) {
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto &ch : state.dataPlantChillers->ConstCOPChiller) {
                        thisChil++;
                        if (ch.Name == state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Absorption")) {
                    // Read in chiller if not done yet
                    if (state.dataChillerAbsorber->getInput) {
                        ChillerAbsorption::GetBLASTAbsorberInput(state);
                        state.dataChillerAbsorber->getInput = false;
                    }
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerAbsorber->absorptionChillers);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataChillerAbsorber->absorptionChillers(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataChillerAbsorber->absorptionChillers(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Absorption:Indirect")) {
                    // Read in chiller if not done yet
                    if (state.dataChillerIndirectAbsorption->GetInput) {
                        ChillerIndirectAbsorption::GetIndirectAbsorberInput(state);
                        state.dataChillerIndirectAbsorption->GetInput = false;
                    }
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerIndirectAbsorption->IndirectAbsorber);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    } else {
                        state.dataChillerIndirectAbsorption->IndirectAbsorber(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataChillerIndirectAbsorption->IndirectAbsorber(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }
                }
            }
        }

        // read faults input of Fouled Air Filters
        for (int jFault_AirFilter = 1; jFault_AirFilter <= state.dataFaultsMgr->NumFaultyAirFilter; ++jFault_AirFilter) {

            // Read in fan if not done yet
            if (state.dataFans->GetFanInputFlag) {
                Fans::GetFanInput(state);
            }

            cFaultCurrentObject = cFaults(8); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_AirFilter,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultTypeEnum = Fault::Fouling_AirFilter;
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).Name = cAlphaArgs(1);

            // Information of the fan associated with the fouling air filter
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanType = cAlphaArgs(2);
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanName = cAlphaArgs(3);

            // Check whether the specified fan exists in the fan list
            if (UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataFans->Fan, &Fans::FanEquipConditions::FanName) <= 0) {
                ShowSevereError(state,
                                cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Assign fault index to the fan object
            for (int FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
                if (UtilityRoutines::SameString(state.dataFans->Fan(FanNum).FanName, cAlphaArgs(3))) {
                    state.dataFans->Fan(FanNum).FaultyFilterFlag = true;
                    state.dataFans->Fan(FanNum).FaultyFilterIndex = jFault_AirFilter;
                    break;
                }
            }

            // Fault availability schedule
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fan pressure increase fraction schedule
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSche = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSchePtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSchePtr =
                    GetScheduleIndex(state, cAlphaArgs(5));
                if (state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSchePtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fan curve describing the relationship between fan pressure rise and air flow rate
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurve = cAlphaArgs(6);
            state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurvePtr = GetCurveIndex(state, cAlphaArgs(6));
            if (state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurvePtr == 0) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\"");
                ShowContinueError(state, "Invalid " + cAlphaFieldNames(6) + " = \"" + cAlphaArgs(6) + "\" not found.");
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Checking  whether the specified fan curve covers the **design** operational point of the fan cannot be done here
            // as the fan might be autosized and is not sized yet, so we call it in Fan::SizeFan instead

            // In the fan object, calculate by each time-step: 1) pressure increase value; 2) air flow rate decrease value.
        }

        // read faults input of HumidistatOffset
        for (int jFault_Humidistat = 1; jFault_Humidistat <= state.dataFaultsMgr->NumFaultyHumidistat; ++jFault_Humidistat) {

            cFaultCurrentObject = cFaults(7); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_Humidistat,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).FaultTypeEnum = Fault::HumidistatOffset;
            state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).Name = cAlphaArgs(1);
            state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).FaultyHumidistatName = cAlphaArgs(2);
            state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).FaultyHumidistatType = cAlphaArgs(3);

            if (UtilityRoutines::SameString(state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).FaultyHumidistatType,
                                            "ThermostatOffsetDependent")) {
                // For Humidistat Offset Type: ThermostatOffsetDependent

                // Related Thermostat Offset Fault Name is required for Humidistat Offset Type: ThermostatOffsetDependent
                if (lAlphaFieldBlanks(6)) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\": " + cAlphaFieldNames(6) +
                                        " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetDependent\".");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).FaultyThermostatName = cAlphaArgs(6);
                }

            } else {
                // For Humidistat Offset Type: ThermostatOffsetIndependent

                // Availability schedule
                state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedule = cAlphaArgs(4);
                if (lAlphaFieldBlanks(4)) {
                    state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedPtr = -1; // returns schedule value of 1
                } else {
                    state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                    if (state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedPtr == 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" +
                                            cAlphaArgs(4) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                // Severity schedule
                state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedule = cAlphaArgs(5);
                if (lAlphaFieldBlanks(5)) {
                    state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedPtr = -1; // returns schedule value of 1
                } else {
                    state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(5));
                    if (state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedPtr == 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                            cAlphaArgs(5) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                // Reference offset value is required for Humidistat Offset Type: ThermostatOffsetIndependent
                if (lAlphaFieldBlanks(1)) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\": " + cNumericFieldNames(1) +
                                        " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetIndependent\".");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat).Offset = rNumericArgs(1);
                }
            }
        }

        // read faults input of ThermostatOffset
        for (int jFault_Thermostat = 1; jFault_Thermostat <= state.dataFaultsMgr->NumFaultyThermostat; ++jFault_Thermostat) {

            cFaultCurrentObject = cFaults(6); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_Thermostat,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).FaultTypeEnum = Fault::ThermostatOffset;
            state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).Name = cAlphaArgs(1);
            state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).FaultyThermostatName = cAlphaArgs(2);

            // Availability schedule
            state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Severity schedule
            state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).SeveritySchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Reference offset value is required
            if (lAlphaFieldBlanks(1)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cNumericFieldNames(1) + "\" cannot be blank.");
                state.dataFaultsMgr->ErrorsFound = true;
            } else {
                state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat).Offset = rNumericArgs(1);
            }
        }

        // read faults input of Fouling_Coil
        for (int jFault_FoulingCoil = 1; jFault_FoulingCoil <= state.dataFaultsMgr->NumFouledCoil; ++jFault_FoulingCoil) {

            cFaultCurrentObject = cFaults(5); // fault object string
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cFaultCurrentObject,
                                                                     jFault_FoulingCoil,
                                                                     cAlphaArgs,
                                                                     NumAlphas,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FaultType = cFaultCurrentObject;
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FaultTypeEnum = Fault::Fouling_Coil;
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).Name = cAlphaArgs(1);
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FouledCoilName = cAlphaArgs(2);

            // Availability schedule
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).AvaiSchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Severity schedule
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).SeveritySchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).SeveritySchedPtr == 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                        "\" not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(5)));
                if (SELECT_CASE_var == "FOULEDUARATED") {
                    state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FoulingInputMethod = FouledCoil::UARated;

                } else if (SELECT_CASE_var == "FOULINGFACTOR") {
                    state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FoulingInputMethod = FouledCoil::FoulingFactor;

                } else {
                    state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FoulingInputMethod = FouledCoil::UARated;
                }
            }

            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).UAFouled = rNumericArgs(1);
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).Rfw = rNumericArgs(2);
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).Rfa = rNumericArgs(3);
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).Aout = rNumericArgs(4);
            state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).Aratio = rNumericArgs(5);

            // Coil check and link
            {
                // Obtains and Allocates WaterCoil related parameters from input file
                if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
                    WaterCoils::GetWaterCoilInput(state);
                    state.dataWaterCoils->GetWaterCoilsInputFlag = false;
                }

                // Check the coil name and type
                int CoilNum = UtilityRoutines::FindItemInList(state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FouledCoilName,
                                                              state.dataWaterCoils->WaterCoil);
                if (CoilNum <= 0) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\". Referenced Coil named \"" +
                                        state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FouledCoilName + "\" was not found.");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Coil is found: check if the right type
                    if ((state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) ||
                        (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling)) {
                        // Link the Coil with the fault model
                        state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFlag = true;
                        state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingIndex = jFault_FoulingCoil;

                        state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FouledCoiledType =
                            state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType;
                        state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil).FouledCoilNum = CoilNum;

                        SetupOutputVariable(state,
                                            "Coil Fouling Factor",
                                            OutputProcessor::Unit::K_W,
                                            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor,
                                            "System",
                                            "Average",
                                            state.dataWaterCoils->WaterCoil(CoilNum).Name);

                        // Coil:Cooling:Water doesn't report UA because it's not variable,
                        // but here, it's useful since we do change it via fouling, so report it
                        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                            SetupOutputVariable(state,
                                                "Cooling Coil Total U Factor Times Area Value",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil External U Factor Times Area Value",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil Internal U Factor Times Area Value",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil Total U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil External U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilExternal,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil Internal U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilInternal,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                        } else {
                            SetupOutputVariable(state,
                                                "Heating Coil U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable,
                                                "System",
                                                "Average",
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
                        }
                    } else {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" +
                                            cAlphaArgs(2) + "\".");
                        ShowContinueError(
                            state, R"(Coil was found but it is not one of the supported types ("Coil:Cooling:Water" or "Coil:Heating:Water").)");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }
            }
        }

        // read faults input: Fault_type 0 to 4, which are related with economizer sensors
        for (int j = 0, i = 0; i <= 4; ++i) {
            cFaultCurrentObject = cFaults(i); // fault object string
            int NumFaultsTemp = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cFaultCurrentObject);

            for (int jj = 1; jj <= NumFaultsTemp; ++jj) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cFaultCurrentObject,
                                                                         jj,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                ++j;
                state.dataFaultsMgr->FaultsEconomizer(j).FaultType = cFaultCurrentObject;
                state.dataFaultsMgr->FaultsEconomizer(j).FaultTypeEnum = static_cast<Fault>(i);

                state.dataFaultsMgr->FaultsEconomizer(j).Name = cAlphaArgs(1);
                state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedule = cAlphaArgs(2);
                // check availability schedule
                if (lAlphaFieldBlanks(2)) {
                    state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedPtr = -1; // returns schedule value of 1
                } else {
                    state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                    if (state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedPtr == 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" +
                                            cAlphaArgs(2) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedule = cAlphaArgs(3);
                // check severity schedule
                if (lAlphaFieldBlanks(3)) {
                    state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr = -1; // returns schedule value of 1
                } else {
                    state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                    if (state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr == 0) {
                        ShowSevereError(state,
                                        cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" +
                                            cAlphaArgs(3) + "\" not found.");
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                state.dataFaultsMgr->FaultsEconomizer(j).ControllerType = cAlphaArgs(4);
                // check controller type
                if (lAlphaFieldBlanks(4)) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                        "\" blank.");
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)));
                        if (SELECT_CASE_var == "CONTROLLER:OUTDOORAIR") {
                            state.dataFaultsMgr->FaultsEconomizer(j).ControllerTypeEnum = iController_AirEconomizer;

                            // CASE ...

                        } else {
                        }
                    }
                }

                state.dataFaultsMgr->FaultsEconomizer(j).ControllerName = cAlphaArgs(5);
                // check controller name
                if (lAlphaFieldBlanks(5)) {
                    ShowSevereError(state,
                                    cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                        "\" blank.");
                    state.dataFaultsMgr->ErrorsFound = true;
                }

                // offset - degree of fault
                state.dataFaultsMgr->FaultsEconomizer(j).Offset = rNumericArgs(1);
            }
        }

        state.dataFaultsMgr->RunFaultMgrOnceFlag = true;

        if (state.dataFaultsMgr->ErrorsFound) {
            ShowFatalError(state, "CheckAndReadFaults: Errors found in getting FaultModel input data. Preceding condition(s) cause termination.");
        }
    }

    Real64 FaultProperties::CalFaultOffsetAct(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rongpeng Zhang
        //       DATE WRITTEN   Jun. 2016

        // PURPOSE OF THIS SUBROUTINE:
        //       To calculate the dynamic fault offset based on the fault availability schedule and severity schedule.

        // Using/Aliasing
        using CurveManager::CurveValue;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFac(0.0); // fault modification factor
        Real64 OffsetAct;     // actual offset after applying the modification factor

        // Check fault availability schedules
        if (GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules
            if (this->SeveritySchedPtr >= 0) {
                FaultFac = GetCurrentScheduleValue(state, this->SeveritySchedPtr);
            } else {
                FaultFac = 1.0;
            }
        }

        OffsetAct = FaultFac * this->Offset;

        return OffsetAct;
    }

    Real64 FaultPropertiesFouling::CalFoulingFactor(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rongpeng Zhang
        //       DATE WRITTEN   Nov. 2016

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the dynamic Nominal Capacity or Efficiency Reduction due to fouling, based on the fault availability schedule and severity
        // schedule. The factor is the ratio between the nominal capacity or efficiency at fouling case and that at fault free case

        // Using/Aliasing
        using CurveManager::CurveValue;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFac(0.0); // fault modification factor
        Real64 FoulingFactor(
            1.0); // Actual Nominal Fouling Factor, ratio between the nominal capacity or efficiency at fouling case and that at fault free case

        // Check fault availability schedules
        if (GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules
            if (this->SeveritySchedPtr >= 0) {
                FaultFac = GetCurrentScheduleValue(state, this->SeveritySchedPtr);
            } else {
                FaultFac = 1.0;
            }
        }

        // The more severe the fouling fault is (i.e., larger FaultFac), the less the FoulingFactor is
        if (FaultFac > 0.0) FoulingFactor = min(this->FoulingFactor / FaultFac, 1.0);

        return FoulingFactor;
    }

    Real64 FaultPropertiesTowerFouling::CalFaultyTowerFoulingFactor(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rongpeng Zhang
        //       DATE WRITTEN   Jul. 2016

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the dynamic tower fouling factor based on the fault availability schedule and severity schedule.
        // Fouling factor is the ratio between the UA value at fouling case and that at fault free case

        // Using/Aliasing
        using CurveManager::CurveValue;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFac(0.0);             // fault modification factor
        Real64 UAReductionFactorAct(1.0); // actual UA Reduction Factor, ratio between the UA value at fouling case and that at fault free case

        // Check fault availability schedules
        if (GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules
            if (this->SeveritySchedPtr >= 0) {
                FaultFac = GetCurrentScheduleValue(state, this->SeveritySchedPtr);
            } else {
                FaultFac = 1.0;
            }
        }

        // The more severe the fouling fault is (i.e., larger FaultFac), the less the UAReductionFactor is
        if (FaultFac > 0.0) UAReductionFactorAct = min(this->UAReductionFactor / FaultFac, 1.0);

        return UAReductionFactorAct;
    }

    Real64 FaultPropertiesFoulingCoil::FaultFraction(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Julien Marrec, EffiBEM
        //       DATE WRITTEN   Feb. 2020

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the Fault Fraction based on Availability and Severity Schedules

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFrac(0.0); // Fault Fraction

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules (Ptr initialized to -1, so would return a FaultFrac of 1 if not set)
            FaultFrac = ScheduleManager::GetCurrentScheduleValue(state, this->SeveritySchedPtr);
        }

        return FaultFrac;
    }

    void FaultPropertiesChillerSWT::CalFaultChillerSWT(bool FlagVariableFlow, // True if chiller is variable flow and false if it is constant flow
                                                       Real64 FaultyChillerSWTOffset, // Faulty chiller SWT sensor offset
                                                       Real64 Cp,                     // Local fluid specific heat
                                                       Real64 EvapInletTemp,          // Chiller evaporator inlet water temperature
                                                       Real64 &EvapOutletTemp,        // Chiller evaporator outlet water temperature
                                                       Real64 &EvapMassFlowRate,      // Chiller mass flow rate
                                                       Real64 &QEvaporator            // Chiller evaporator heat transfer rate
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rongpeng Zhang
        //       DATE WRITTEN   Jun. 2016

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the mass flow rate and supply water temperature of a chiller with faulty SWT sensor.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // Variables for fault free cases
        Real64 EvapOutletTemp_ff = EvapOutletTemp;     // Chiller supply water temperature, fault free [C]
        Real64 EvapMassFlowRate_ff = EvapMassFlowRate; // Chiller mass flow rate, fault free [kg/s]
        Real64 QEvaporator_ff = QEvaporator;           // Chiller evaporator heat transfer rate, fault free [W]

        // Variables for faulty cases
        Real64 EvapOutletTemp_f = EvapOutletTemp_ff;     // Chiller supply water temperature, faulty case [C]
        Real64 EvapMassFlowRate_f = EvapMassFlowRate_ff; // Chiller mass flow rate, faulty case [kg/s]
        Real64 QEvaporator_f = QEvaporator_ff;           // Chiller evaporator heat transfer rate, faulty case [W]

        if (!FlagVariableFlow) {
            // Chillers with ConstantFlow mode

            EvapOutletTemp_f = EvapOutletTemp_ff - FaultyChillerSWTOffset;

            if ((EvapInletTemp > EvapOutletTemp_f) && (EvapMassFlowRate_ff > 0)) {
                QEvaporator_f = EvapMassFlowRate_ff * Cp * (EvapInletTemp - EvapOutletTemp_f);
            } else {
                EvapMassFlowRate_f = 0.0;
                QEvaporator_f = 0.0;
            }

        } else {
            // Chillers with LeavingSetpointModulated mode

            EvapOutletTemp_f = EvapOutletTemp_ff - FaultyChillerSWTOffset;

            if ((EvapInletTemp > EvapOutletTemp_f) && (Cp > 0) && (EvapMassFlowRate_ff > 0)) {
                EvapMassFlowRate_f = QEvaporator_ff / Cp / (EvapInletTemp - EvapOutletTemp_ff);
                QEvaporator_f = EvapMassFlowRate_f * Cp * (EvapInletTemp - EvapOutletTemp_f);
            } else {
                EvapMassFlowRate_f = 0.0;
                QEvaporator_f = 0.0;
            }
        }

        // Return variables
        EvapOutletTemp = EvapOutletTemp_f;
        EvapMassFlowRate = EvapMassFlowRate_f;
        QEvaporator = QEvaporator_f;
    }

    bool FaultPropertiesAirFilter::CheckFaultyAirFilterFanCurve(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rongpeng Zhang
        //       DATE WRITTEN   Apr. 2015

        // PURPOSE OF THIS SUBROUTINE:
        // To check whether the fan curve specified in the FaultModel:Fouling:AirFilter object
        // covers the rated operational point of the corresponding fan
        // Return true if the curve covers the fan rated operational point

        // Using/Aliasing
        using CurveManager::CurveValue;
        using namespace Fans;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FanMaxAirFlowRate; // Design Max Specified Volume Flow Rate of Fan [m3/sec]
        Real64 FanDeltaPress;     // Design Delta Pressure Across the Fan [Pa]
        Real64 FanDeltaPressCal;  // Calculated Delta Pressure Across the Fan [Pa]
        bool FanFound;            // Whether the fan is found or not

        std::string const FanName = this->FaultyAirFilterFanName; // name of the fan
        int const FanCurvePtr = this->FaultyAirFilterFanCurvePtr; // pointer of the fan curve

        FanFound = false;

        for (int FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
            if (UtilityRoutines::SameString(state.dataFans->Fan(FanNum).FanName, FanName)) {
                FanMaxAirFlowRate = state.dataFans->Fan(FanNum).MaxAirFlowRate;
                FanDeltaPress = state.dataFans->Fan(FanNum).DeltaPress;
                FanFound = true;
                break;
            }
        }

        if (!FanFound) {
            return false;
        }

        FanDeltaPressCal = CurveValue(state, FanCurvePtr, FanMaxAirFlowRate);

        return ((FanDeltaPressCal > 0.95 * FanDeltaPress) && (FanDeltaPressCal < 1.05 * FanDeltaPress));
    }

    void SetFaultyCoilSATSensor(
        EnergyPlusData &state, std::string const &CompType, std::string_view CompName, bool &FaultyCoilSATFlag, int &FaultyCoilSATIndex)
    {

        FaultyCoilSATFlag = false;
        FaultyCoilSATIndex = 0;
        if (state.dataFaultsMgr->NumFaultyCoilSATSensor == 0) return;
        for (int jFault_CoilSAT = 1; jFault_CoilSAT <= state.dataFaultsMgr->NumFaultyCoilSATSensor; ++jFault_CoilSAT) {
            if (UtilityRoutines::SameString(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilType, CompType) &&
                UtilityRoutines::SameString(state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT).CoilName, CompName)) {
                FaultyCoilSATFlag = true;
                FaultyCoilSATIndex = jFault_CoilSAT;
                break;
            }
        }
    }

} // namespace FaultsManager

} // namespace EnergyPlus
