// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

    // PURPOSE OF THIS MODULE:
    // This module manages operational faults of buildings and systems.

    // METHODOLOGY EMPLOYED:
    //  Various methods are employed depending types of faults

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

    enum class ChillerType
    {
        Invalid = -1,
        ChillerElectric,
        ChillerElectricEIR,
        ChillerElectricReformulatedEIR,
        ChillerConstantCOP,
        ChillerEngineDriven,
        ChillerCombustionTurbine,
        ChillerAbsorption,
        ChillerAbsorptionIndirect,
        Num
    };

    enum class CoilType
    {
        Invalid = -1,
        CoilHeatingElectric,
        CoilHeatingFuel,
        CoilHeatingDesuperheater,
        CoilHeatingSteam,
        CoilHeatingWater,
        CoilCoolingWater,
        CoilCoolingWaterDetailedgeometry,
        CoilSystemCoolingDX,
        CoilSystemHeatingDX,
        AirLoopHVACUnitarySystem,
        Num
    };

    constexpr std::array<std::string_view, static_cast<int>(ChillerType::Num)> ChillerTypeNamesUC{"CHILLER:ELECTRIC",
                                                                                                  "CHILLER:ELECTRIC:EIR",
                                                                                                  "CHILLER:ELECTRIC:REFORMULATEDEIR",
                                                                                                  "CHILLER:CONSTANTCOP",
                                                                                                  "CHILLER:ENGINEDRIVEN",
                                                                                                  "CHILLER:COMBUSTIONTURBINE",
                                                                                                  "CHILLER:ABSORPTION",
                                                                                                  "CHILLER:ABSORPTION:INDIRECT"};

    constexpr std::array<std::string_view, static_cast<int>(CoilType::Num)> CoilTypeNamesUC{"COIL:HEATING:ELECTRIC",
                                                                                            "COIL:HEATING:FUEL",
                                                                                            "COIL:HEATING:DESUPERHEATER",
                                                                                            "COIL:HEATING:STEAM",
                                                                                            "COIL:HEATING:WATER",
                                                                                            "COIL:COOLING:WATER",
                                                                                            "COIL:COOLING:WATER:DETAILEDGEOMETRY",
                                                                                            "COILSYSTEM:COOLING:DX",
                                                                                            "COILSYSTEM:HEATING:DX",
                                                                                            "AIRLOOPHVAC:UNITARYSYSTEM"};

    constexpr std::array<std::string_view, static_cast<int>(FouledCoil::Num)> FouledCoilNamesUC{"FOULEDUARATED", "FOULINGFACTOR"};

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

        // PURPOSE OF THIS SUBROUTINE:
        //  1. Determine if any operational faults are present in a model and set flags
        //  2. Read faults input

        // METHODOLOGY EMPLOYED:
        // Get number of faults-related input objects and assign faults input to data structure

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
            auto &faultsECFouling = state.dataFaultsMgr->FaultsEvapCoolerFouling(jFault_EvapCoolerFouling);

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

            faultsECFouling.FaultType = cFaultCurrentObject;
            faultsECFouling.FaultTypeEnum = Fault::Fouling_EvapCooler;
            faultsECFouling.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsECFouling.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsECFouling.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsECFouling.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsECFouling.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsECFouling.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsECFouling.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsECFouling.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsECFouling.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            faultsECFouling.FoulingFactor = rNumericArgs(1);

            // Evaporative cooler type
            faultsECFouling.EvapCoolerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Evaporative cooler name
            faultsECFouling.EvapCoolerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Evaporative cooler check
            if (UtilityRoutines::SameString(faultsECFouling.EvapCoolerType, "EvaporativeCooler:Indirect:WetCoil")) {
                // Read in evaporative cooler is not done yet
                if (state.dataEvapCoolers->GetInputEvapComponentsFlag) {
                    EvaporativeCoolers::GetEvapInput(state);
                    state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
                }

                // Check whether the evaporative cooler  name and type match each other;
                int EvapCoolerNum = UtilityRoutines::FindItemInList(
                    faultsECFouling.EvapCoolerName, state.dataEvapCoolers->EvapCond, &EvaporativeCoolers::EvapConditions::Name);
                if (EvapCoolerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the boiler with the fault model
                    state.dataEvapCoolers->EvapCond(EvapCoolerNum).FaultyEvapCoolerFoulingFlag = true;
                    state.dataEvapCoolers->EvapCond(EvapCoolerNum).FaultyEvapCoolerFoulingIndex = jFault_EvapCoolerFouling;
                }
            }
        }

        // read faults input of Chiller Fouling
        for (int jFault_ChillerFouling = 1; jFault_ChillerFouling <= state.dataFaultsMgr->NumFaultyChillerFouling; ++jFault_ChillerFouling) {
            auto &faultsChillerFouling = state.dataFaultsMgr->FaultsChillerFouling(jFault_ChillerFouling);

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

            faultsChillerFouling.FaultType = cFaultCurrentObject;
            faultsChillerFouling.FaultTypeEnum = Fault::Fouling_Chiller;
            faultsChillerFouling.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsChillerFouling.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsChillerFouling.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsChillerFouling.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsChillerFouling.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsChillerFouling.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsChillerFouling.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsChillerFouling.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsChillerFouling.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            faultsChillerFouling.FoulingFactor = rNumericArgs(1);

            // Chiller type
            faultsChillerFouling.ChillerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller name
            faultsChillerFouling.ChillerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller check
            int ChillerNum;
            ChillerType ChillerTypeCheck =
                static_cast<ChillerType>(getEnumValue(ChillerTypeNamesUC, UtilityRoutines::makeUPPER(faultsChillerFouling.ChillerType)));
            switch (ChillerTypeCheck) {
            case ChillerType::ChillerElectric: {
                // Check whether the chiller name and chiller type match each other
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->ElectricChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerFouling.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {

                    if (state.dataPlantChillers->ElectricChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                        // The fault model is only applicable to the chillers with water based condensers
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified chiller is not water cooled. The chiller fouling "
                                                "fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));

                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                        state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                    }
                }
            } break;
            case ChillerType::ChillerElectricEIR: {
                // Read in chiller if not done yet
                if (state.dataChillerElectricEIR->getInputFlag) {
                    ChillerElectricEIR::GetElectricEIRChillerInput(state);
                    state.dataChillerElectricEIR->getInputFlag = false;
                }

                // Check whether the chiller name and chiller type match each other
                ChillerNum = UtilityRoutines::FindItemInList(faultsChillerFouling.ChillerName, state.dataChillerElectricEIR->ElectricEIRChiller);
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {

                    if (state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                        // The fault model is only applicable to the chillers with water based condensers
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified chiller is not water cooled. The chiller fouling "
                                                "fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));

                    } else {
                        // Link the chiller with the fault model
                        state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                        state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                    }
                }
            } break;
            case ChillerType::ChillerElectricReformulatedEIR: {
                // Read in chiller if not done yet
                if (state.dataChillerReformulatedEIR->GetInputREIR) {
                    ChillerReformulatedEIR::GetElecReformEIRChillerInput(state);
                    state.dataChillerReformulatedEIR->GetInputREIR = false;
                }

                // Check whether the chiller name and chiller type match each other
                ChillerNum =
                    UtilityRoutines::FindItemInList(faultsChillerFouling.ChillerName, state.dataChillerReformulatedEIR->ElecReformEIRChiller);
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {

                    if (state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                        // The fault model is only applicable to the chillers with water based condensers
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified chiller is not water cooled. The chiller fouling "
                                                "fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));

                    } else {
                        // Link the chiller with the fault model
                        state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                        state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                    }
                }
            } break;
            case ChillerType::ChillerConstantCOP: {
                // Check whether the chiller name and chiller type match each other
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->ConstCOPChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerFouling.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {

                    if (state.dataPlantChillers->ConstCOPChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                        // The fault model is only applicable to the chillers with water based condensers
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified chiller is not water cooled. The chiller fouling "
                                                "fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));

                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                        state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                    }
                }
            } break;
            case ChillerType::ChillerEngineDriven: {
                // Check whether the chiller name and chiller type match each other
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->EngineDrivenChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerFouling.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {

                    if (state.dataPlantChillers->EngineDrivenChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                        // The fault model is only applicable to the chillers with water based condensers
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified chiller is not water cooled. The chiller fouling "
                                                "fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));

                    } else {
                        // Link the fault model with the water cooled chiller
                        state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                        state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                    }
                }
            } break;
            case ChillerType::ChillerCombustionTurbine: {
                // Check whether the chiller name and chiller type match each other
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->GTChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerFouling.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    if (state.dataPlantChillers->GTChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                        // The fault model is only applicable to the chillers with water based condensers
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified chiller is not water cooled. The chiller fouling "
                                                "fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));

                    } else {
                        // Link the fault model with the water cooled chiller
                        state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerFoulingFlag = true;
                        state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerFoulingIndex = jFault_ChillerFouling;
                    }
                }
            } break;
            default:
                break;
            }
        }

        // read faults input of Boiler Fouling
        for (int jFault_BoilerFouling = 1; jFault_BoilerFouling <= state.dataFaultsMgr->NumFaultyBoilerFouling; ++jFault_BoilerFouling) {
            auto &faultsBoilerFouling = state.dataFaultsMgr->FaultsBoilerFouling(jFault_BoilerFouling);

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

            faultsBoilerFouling.FaultType = cFaultCurrentObject;
            faultsBoilerFouling.FaultTypeEnum = Fault::Fouling_Boiler;
            faultsBoilerFouling.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsBoilerFouling.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsBoilerFouling.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsBoilerFouling.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsBoilerFouling.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsBoilerFouling.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsBoilerFouling.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsBoilerFouling.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsBoilerFouling.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            faultsBoilerFouling.FoulingFactor = rNumericArgs(1);

            // Boiler type
            faultsBoilerFouling.BoilerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Boiler name
            faultsBoilerFouling.BoilerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Boiler check and link
            {
                if (state.dataBoilers->getBoilerInputFlag) {
                    Boilers::GetBoilerInput(state);
                    state.dataBoilers->getBoilerInputFlag = false;
                }
                // Check the boiler name and boiler type
                int BoilerNum = UtilityRoutines::FindItemInList(faultsBoilerFouling.BoilerName, state.dataBoilers->Boiler);
                if (BoilerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
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
            auto &faultsCoilSATFouling = state.dataFaultsMgr->FaultsCoilSATSensor(jFault_CoilSAT);

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

            faultsCoilSATFouling.FaultType = cFaultCurrentObject;
            faultsCoilSATFouling.FaultTypeEnum = Fault::TemperatureSensorOffset_CoilSupplyAir;
            faultsCoilSATFouling.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsCoilSATFouling.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsCoilSATFouling.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsCoilSATFouling.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsCoilSATFouling.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsCoilSATFouling.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsCoilSATFouling.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsCoilSATFouling.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsCoilSATFouling.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // offset - degree of fault
            faultsCoilSATFouling.Offset = rNumericArgs(1);

            // Coil type
            faultsCoilSATFouling.CoilType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Coil name
            faultsCoilSATFouling.CoilName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Coil check and link
            CoilType CoilTypeCheck = static_cast<CoilType>(getEnumValue(CoilTypeNamesUC, UtilityRoutines::makeUPPER(faultsCoilSATFouling.CoilType)));
            switch (CoilTypeCheck) {
            case CoilType::CoilHeatingElectric:
            case CoilType::CoilHeatingFuel:
            case CoilType::CoilHeatingDesuperheater: {
                // Read in coil input if not done yet
                if (state.dataHeatingCoils->GetCoilsInputFlag) {
                    HeatingCoils::GetHeatingCoilInput(state);
                    state.dataHeatingCoils->GetCoilsInputFlag = false;
                }
                // Check the coil name and coil type
                int CoilNum = UtilityRoutines::FindItemInList(faultsCoilSATFouling.CoilName, state.dataHeatingCoils->HeatingCoil);
                if (CoilNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the coil with the fault model
                    state.dataHeatingCoils->HeatingCoil(CoilNum).FaultyCoilSATFlag = true;
                    state.dataHeatingCoils->HeatingCoil(CoilNum).FaultyCoilSATIndex = jFault_CoilSAT;
                }
            } break;
            case CoilType::CoilHeatingSteam: {
                // Read in coil input if not done yet
                if (state.dataSteamCoils->GetSteamCoilsInputFlag) {
                    SteamCoils::GetSteamCoilInput(state);
                    state.dataSteamCoils->GetSteamCoilsInputFlag = false;
                }
                // Check the coil name and coil type
                int CoilNum = UtilityRoutines::FindItemInList(faultsCoilSATFouling.CoilName, state.dataSteamCoils->SteamCoil);
                if (CoilNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {

                    if (state.dataSteamCoils->SteamCoil(CoilNum).TypeOfCoil != SteamCoils::CoilControlType::TemperatureSetPoint) {
                        // The fault model is only applicable to the coils controlled on leaving air temperature
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\". The specified coil is not controlled on leaving air temperature. "
                                                "The coil SAT sensor fault model will not be applied.",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(5),
                                                cAlphaArgs(5)));
                    } else {
                        // Link the fault model with the coil that is controlled on leaving air temperature
                        state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATFlag = true;
                        state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }
                }
            } break;
            case CoilType::CoilHeatingWater:
            case CoilType::CoilCoolingWater:
            case CoilType::CoilCoolingWaterDetailedgeometry: {
                // Read in coil input if not done yet
                if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
                    WaterCoils::GetWaterCoilInput(state);
                    state.dataWaterCoils->GetWaterCoilsInputFlag = false;
                }
                // Check the coil name and coil type
                int CoilNum = UtilityRoutines::FindItemInList(faultsCoilSATFouling.CoilName, state.dataWaterCoils->WaterCoil);
                if (CoilNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }

                // Read in Water Coil Controller Name
                faultsCoilSATFouling.WaterCoilControllerName = cAlphaArgs(6);
                if (lAlphaFieldBlanks(6)) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(6), cAlphaArgs(6)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
                // Read in controller input if not done yet
                if (state.dataHVACControllers->GetControllerInputFlag) {
                    HVACControllers::GetControllerInput(state);
                    state.dataHVACControllers->GetControllerInputFlag = false;
                }
                // Check the controller name
                int ControlNum = UtilityRoutines::FindItemInList(faultsCoilSATFouling.WaterCoilControllerName,
                                                                 state.dataHVACControllers->ControllerProps,
                                                                 &HVACControllers::ControllerPropsType::ControllerName);
                if (ControlNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(6), cAlphaArgs(6)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the controller with the fault model
                    state.dataHVACControllers->ControllerProps(ControlNum).FaultyCoilSATFlag = true;
                    state.dataHVACControllers->ControllerProps(ControlNum).FaultyCoilSATIndex = jFault_CoilSAT;

                    // Check whether the controller match the coil
                    if (state.dataHVACControllers->ControllerProps(ControlNum).SensedNode !=
                        state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum) {
                        ShowSevereError(state,
                                        format("{} = \"{}\" invalid {} = \"{}\" does not match {} = \"{}",
                                               cFaultCurrentObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(6),
                                               cAlphaArgs(6),
                                               cAlphaFieldNames(5),
                                               cAlphaArgs(5)));
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }
            } break;
            case CoilType::CoilSystemCoolingDX: {
                // see case CoilCheck::AirLoopHVACUnitarySystem: below
                // UnitarySystem connects a different way. Make sure this works by testing a CoilSystem model.
                // Read in DXCoolingSystem input if not done yet
                // if (state.dataHVACDXSys->GetInputFlag) {
                //    HVACDXSystem::GetDXCoolingSystemInput(state);
                //    state.dataHVACDXSys->GetInputFlag = false;
                //}

                //// Check the coil name and coil type
                // int CoilSysNum = UtilityRoutines::FindItemInList(faultsCoilSATFouling.CoilName,
                //                                                 state.dataHVACDXSys->DXCoolingSystem);
                // if (CoilSysNum <= 0) {
                //    ShowSevereError(state, format("{}{} = \"{}\" invalid {} = \"{}{}\" not found.", //, cFaultCurrentObject, cAlphaArgs(1),
                //    cAlphaFieldNames(5), //, cAlphaArgs(5))); state.dataFaultsMgr->ErrorsFound = true;
                //} else {
                //    // Link the coil system with the fault model
                //    state.dataHVACDXSys->DXCoolingSystem(CoilSysNum).FaultyCoilSATFlag = true;
                //    state.dataHVACDXSys->DXCoolingSystem(CoilSysNum).FaultyCoilSATIndex = jFault_CoilSAT;
                //}
            } break;
            case CoilType::CoilSystemHeatingDX: {
                // Read in DXCoolingSystem input if not done yet
                if (state.dataHVACDXHeatPumpSys->GetInputFlag) {
                    HVACDXHeatPumpSystem::GetDXHeatPumpSystemInput(state);
                    state.dataHVACDXHeatPumpSys->GetInputFlag = false;
                }

                // Check the coil name and coil type
                int CoilSysNum = UtilityRoutines::FindItemInList(faultsCoilSATFouling.CoilName, state.dataHVACDXHeatPumpSys->DXHeatPumpSystem);
                if (CoilSysNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the coil system with the fault model
                    state.dataHVACDXHeatPumpSys->DXHeatPumpSystem(CoilSysNum).FaultyCoilSATFlag = true;
                    state.dataHVACDXHeatPumpSys->DXHeatPumpSystem(CoilSysNum).FaultyCoilSATIndex = jFault_CoilSAT;
                }
            } break;
            case CoilType::AirLoopHVACUnitarySystem: {
                // UnitarySystem model connects to FaultManager via function call to FaultsManager::SetFaultyCoilSATSensor
            } break;
            default:
                break;
            }
        } // End read faults input of Fault_type 113

        // read faults input of Cooling tower scaling
        for (int jFault_TowerFouling = 1; jFault_TowerFouling <= state.dataFaultsMgr->NumFaultyTowerFouling; ++jFault_TowerFouling) {
            auto &faultsTowerFouling = state.dataFaultsMgr->FaultsTowerFouling(jFault_TowerFouling);

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

            faultsTowerFouling.FaultType = cFaultCurrentObject;
            faultsTowerFouling.FaultTypeEnum = Fault::Fouling_Tower;
            faultsTowerFouling.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsTowerFouling.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsTowerFouling.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsTowerFouling.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsTowerFouling.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsTowerFouling.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsTowerFouling.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsTowerFouling.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsTowerFouling.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // UAReductionFactor - degree of fault
            faultsTowerFouling.UAReductionFactor = rNumericArgs(1);

            // Cooling tower type
            faultsTowerFouling.TowerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Cooling tower name
            faultsTowerFouling.TowerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
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
                int TowerNum = UtilityRoutines::FindItemInList(faultsTowerFouling.TowerName, state.dataCondenserLoopTowers->towers);
                if (TowerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the tower with the fault model
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingFlag = true;
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingIndex = jFault_TowerFouling;

                    // Check the faulty tower type
                    if (!UtilityRoutines::SameString(
                            DataPlant::PlantEquipTypeNames[static_cast<int>(state.dataCondenserLoopTowers->towers(TowerNum).TowerType)],
                            faultsTowerFouling.TowerType)) {
                        ShowWarningError(
                            state,
                            format("{} = \"{}\" invalid {} = \"{}\" not match the type of {}. Tower type in the fault model is updated. ",
                                   cFaultCurrentObject,
                                   cAlphaArgs(1),
                                   cAlphaFieldNames(4),
                                   cAlphaArgs(4),
                                   cAlphaFieldNames(5)));
                        faultsTowerFouling.TowerType =
                            DataPlant::PlantEquipTypeNames[static_cast<int>(state.dataCondenserLoopTowers->towers(TowerNum).TowerType)];
                    }

                    // Check the tower model
                    // Performance Input Method should be UFactorTimesAreaAndDesignWaterFlowRate to apply the fault model
                    if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num != CondenserLoopTowers::PIM::UFactor) {
                        ShowWarningError(
                            state,
                            format("{} = \"{}\" invalid {} = \"{}. Tower Performance Input Method is not UFactorTimesAreaAndDesignWaterFlowRate. The "
                                   "tower fouling fault model will not be applied to the tower. ",
                                   cFaultCurrentObject,
                                   cAlphaArgs(1),
                                   cAlphaFieldNames(5),
                                   cAlphaFieldNames(5)));
                        state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingFlag = false;
                    }
                }
            }
        }

        // read faults input of Condenser SWT Sensor Offset
        for (int jFault_CondenserSWT = 1; jFault_CondenserSWT <= state.dataFaultsMgr->NumFaultyCondenserSWTSensor; ++jFault_CondenserSWT) {
            auto &faultsCondSWTFouling = state.dataFaultsMgr->FaultsCondenserSWTSensor(jFault_CondenserSWT);

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

            faultsCondSWTFouling.FaultType = cFaultCurrentObject;
            faultsCondSWTFouling.FaultTypeEnum = Fault::TemperatureSensorOffset_CondenserSupplyWater;
            faultsCondSWTFouling.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsCondSWTFouling.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsCondSWTFouling.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsCondSWTFouling.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsCondSWTFouling.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsCondSWTFouling.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsCondSWTFouling.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsCondSWTFouling.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsCondSWTFouling.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // offset - degree of fault
            faultsCondSWTFouling.Offset = rNumericArgs(1);

            // Cooling tower type
            faultsCondSWTFouling.TowerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Cooling tower name
            faultsCondSWTFouling.TowerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
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
                int TowerNum = UtilityRoutines::FindItemInList(faultsCondSWTFouling.TowerName, state.dataCondenserLoopTowers->towers);
                if (TowerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the tower with the fault model
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyCondenserSWTFlag = true;
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyCondenserSWTIndex = jFault_CondenserSWT;

                    // Check the faulty tower type
                    if (!UtilityRoutines::SameString(
                            DataPlant::PlantEquipTypeNames[static_cast<int>(state.dataCondenserLoopTowers->towers(TowerNum).TowerType)],
                            faultsCondSWTFouling.TowerType)) {
                        ShowWarningError(state,
                                         format("{} = \"{}\" invalid {} = \"{}\" not match the type of {}. Tower type is updated. ",
                                                cFaultCurrentObject,
                                                cAlphaArgs(1),
                                                cAlphaFieldNames(4),
                                                cAlphaArgs(4),
                                                cAlphaFieldNames(5)));
                        faultsCondSWTFouling.TowerType =
                            DataPlant::PlantEquipTypeNames[static_cast<int>(state.dataCondenserLoopTowers->towers(TowerNum).TowerType)];
                    }
                }
            }
        }

        // read faults input of Chiller SWT Sensor Offset
        for (int jFault_ChillerSWT = 1; jFault_ChillerSWT <= state.dataFaultsMgr->NumFaultyChillerSWTSensor; ++jFault_ChillerSWT) {
            auto &faultsChillerSWT = state.dataFaultsMgr->FaultsChillerSWTSensor(jFault_ChillerSWT);

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

            faultsChillerSWT.FaultType = cFaultCurrentObject;
            faultsChillerSWT.FaultTypeEnum = Fault::TemperatureSensorOffset_ChillerSupplyWater;
            faultsChillerSWT.Name = cAlphaArgs(1);

            // Fault availability schedule
            faultsChillerSWT.AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                faultsChillerSWT.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsChillerSWT.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                if (faultsChillerSWT.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fault severity schedule
            faultsChillerSWT.SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsChillerSWT.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsChillerSWT.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsChillerSWT.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // offset - degree of fault
            faultsChillerSWT.Offset = rNumericArgs(1);

            // Chiller type
            faultsChillerSWT.ChillerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller name
            faultsChillerSWT.ChillerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(
                    state, format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Chiller check
            int ChillerNum;
            ChillerType ChillerTypeCheck =
                static_cast<ChillerType>(getEnumValue(ChillerTypeNamesUC, UtilityRoutines::makeUPPER(faultsChillerSWT.ChillerType)));
            switch (ChillerTypeCheck) {
            case ChillerType::ChillerElectric: {
                // Check whether the chiller name and chiller type match each other
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->ElectricChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerSWT.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataPlantChillers->ElectricChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerElectricEIR: {
                // Read in chiller if not done yet
                if (state.dataChillerElectricEIR->getInputFlag) {
                    ChillerElectricEIR::GetElectricEIRChillerInput(state);
                    state.dataChillerElectricEIR->getInputFlag = false;
                }
                // Check whether the chiller name and chiller type match each other
                ChillerNum = UtilityRoutines::FindItemInList(faultsChillerSWT.ChillerName, state.dataChillerElectricEIR->ElectricEIRChiller);
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerElectricReformulatedEIR: {
                // Read in chiller if not done yet
                if (state.dataChillerReformulatedEIR->GetInputREIR) {
                    ChillerReformulatedEIR::GetElecReformEIRChillerInput(state);
                    state.dataChillerReformulatedEIR->GetInputREIR = false;
                }
                // Check whether the chiller name and chiller type match each other
                ChillerNum = UtilityRoutines::FindItemInList(faultsChillerSWT.ChillerName, state.dataChillerReformulatedEIR->ElecReformEIRChiller);
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerEngineDriven: {
                // Check whether the chiller name and chiller type match each other
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->EngineDrivenChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerSWT.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerCombustionTurbine: {
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->GTChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerSWT.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerConstantCOP: {
                ChillerNum = 0;
                int thisChil = 0;
                for (auto const &ch : state.dataPlantChillers->ConstCOPChiller) {
                    thisChil++;
                    if (ch.Name == faultsChillerSWT.ChillerName) {
                        ChillerNum = thisChil;
                    }
                }
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataPlantChillers->ConstCOPChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerAbsorption: {
                // Read in chiller if not done yet
                if (state.dataChillerAbsorber->getInput) {
                    ChillerAbsorption::GetBLASTAbsorberInput(state);
                    state.dataChillerAbsorber->getInput = false;
                }
                // Check whether the chiller name and chiller type match each other
                ChillerNum = UtilityRoutines::FindItemInList(faultsChillerSWT.ChillerName, state.dataChillerAbsorber->absorptionChillers);
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Link the chiller with the fault model
                    state.dataChillerAbsorber->absorptionChillers(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataChillerAbsorber->absorptionChillers(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            case ChillerType::ChillerAbsorptionIndirect: {
                // Read in chiller if not done yet
                if (state.dataChillerIndirectAbsorption->GetInput) {
                    ChillerIndirectAbsorption::GetIndirectAbsorberInput(state);
                    state.dataChillerIndirectAbsorption->GetInput = false;
                }
                // Check whether the chiller name and chiller type match each other
                ChillerNum = UtilityRoutines::FindItemInList(faultsChillerSWT.ChillerName, state.dataChillerIndirectAbsorption->IndirectAbsorber);
                if (ChillerNum <= 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    state.dataChillerIndirectAbsorption->IndirectAbsorber(ChillerNum).FaultyChillerSWTFlag = true;
                    state.dataChillerIndirectAbsorption->IndirectAbsorber(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                }
            } break;
            default:
                break;
            }
        }

        // read faults input of Fouled Air Filters
        for (int jFault_AirFilter = 1; jFault_AirFilter <= state.dataFaultsMgr->NumFaultyAirFilter; ++jFault_AirFilter) {
            auto &faultsAirFilter = state.dataFaultsMgr->FaultsFouledAirFilters(jFault_AirFilter);

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

            faultsAirFilter.FaultType = cFaultCurrentObject;
            faultsAirFilter.FaultTypeEnum = Fault::Fouling_AirFilter;
            faultsAirFilter.Name = cAlphaArgs(1);

            // Information of the fan associated with the fouling air filter
            faultsAirFilter.FaultyAirFilterFanType = cAlphaArgs(2);
            faultsAirFilter.FaultyAirFilterFanName = cAlphaArgs(3);

            // Check whether the specified fan exists in the fan list
            if (UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataFans->Fan, &Fans::FanEquipConditions::FanName) <= 0) {
                ShowSevereError(
                    state,
                    format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
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
            faultsAirFilter.AvaiSchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                faultsAirFilter.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsAirFilter.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(4));
                if (faultsAirFilter.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fan pressure increase fraction schedule
            faultsAirFilter.FaultyAirFilterPressFracSche = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                faultsAirFilter.FaultyAirFilterPressFracSchePtr = -1; // returns schedule value of 1
            } else {
                faultsAirFilter.FaultyAirFilterPressFracSchePtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(5));
                if (faultsAirFilter.FaultyAirFilterPressFracSchePtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Fan curve describing the relationship between fan pressure rise and air flow rate
            faultsAirFilter.FaultyAirFilterFanCurve = cAlphaArgs(6);
            faultsAirFilter.FaultyAirFilterFanCurvePtr = Curve::GetCurveIndex(state, cAlphaArgs(6));
            if (faultsAirFilter.FaultyAirFilterFanCurvePtr == 0) {
                ShowSevereError(state, format("{} = \"{}\"", cFaultCurrentObject, cAlphaArgs(1)));
                ShowContinueError(state, format("Invalid {} = \"{}\" not found.", cAlphaFieldNames(6), cAlphaArgs(6)));
                state.dataFaultsMgr->ErrorsFound = true;
            }

            // Checking  whether the specified fan curve covers the **design** operational point of the fan cannot be done here
            // as the fan might be autosized and is not sized yet, so we call it in Fan::SizeFan instead

            // In the fan object, calculate by each time-step: 1) pressure increase value; 2) air flow rate decrease value.
        }

        // read faults input of HumidistatOffset
        for (int jFault_Humidistat = 1; jFault_Humidistat <= state.dataFaultsMgr->NumFaultyHumidistat; ++jFault_Humidistat) {
            auto &faultsHStat = state.dataFaultsMgr->FaultsHumidistatOffset(jFault_Humidistat);

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

            faultsHStat.FaultType = cFaultCurrentObject;
            faultsHStat.FaultTypeEnum = Fault::HumidistatOffset;
            faultsHStat.Name = cAlphaArgs(1);
            faultsHStat.FaultyHumidistatName = cAlphaArgs(2);
            faultsHStat.FaultyHumidistatType = cAlphaArgs(3);

            if (UtilityRoutines::SameString(faultsHStat.FaultyHumidistatType, "ThermostatOffsetDependent")) {
                // For Humidistat Offset Type: ThermostatOffsetDependent

                // Related Thermostat Offset Fault Name is required for Humidistat Offset Type: ThermostatOffsetDependent
                if (lAlphaFieldBlanks(6)) {
                    ShowSevereError(state,
                                    format("{} = \"{}\": {} cannot be blank for Humidistat Offset Type = \"ThermostatOffsetDependent\".",
                                           cFaultCurrentObject,
                                           cAlphaArgs(1),
                                           cAlphaFieldNames(6)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    faultsHStat.FaultyThermostatName = cAlphaArgs(6);
                }

            } else {
                // For Humidistat Offset Type: ThermostatOffsetIndependent

                // Availability schedule
                faultsHStat.AvaiSchedule = cAlphaArgs(4);
                if (lAlphaFieldBlanks(4)) {
                    faultsHStat.AvaiSchedPtr = -1; // returns schedule value of 1
                } else {
                    faultsHStat.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(4));
                    if (faultsHStat.AvaiSchedPtr == 0) {
                        ShowSevereError(state,
                                        format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                               cFaultCurrentObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(4),
                                               cAlphaArgs(4)));
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                // Severity schedule
                faultsHStat.SeveritySchedule = cAlphaArgs(5);
                if (lAlphaFieldBlanks(5)) {
                    faultsHStat.SeveritySchedPtr = -1; // returns schedule value of 1
                } else {
                    faultsHStat.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(5));
                    if (faultsHStat.SeveritySchedPtr == 0) {
                        ShowSevereError(state,
                                        format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                               cFaultCurrentObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(5),
                                               cAlphaArgs(5)));
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                // Reference offset value is required for Humidistat Offset Type: ThermostatOffsetIndependent
                if (lAlphaFieldBlanks(1)) {
                    ShowSevereError(state,
                                    format("{} = \"{}\": {} cannot be blank for Humidistat Offset Type = \"ThermostatOffsetIndependent\".",
                                           cFaultCurrentObject,
                                           cAlphaArgs(1),
                                           cNumericFieldNames(1)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    faultsHStat.Offset = rNumericArgs(1);
                }
            }
        }

        // read faults input of ThermostatOffset
        for (int jFault_Thermostat = 1; jFault_Thermostat <= state.dataFaultsMgr->NumFaultyThermostat; ++jFault_Thermostat) {
            auto &faultsTStat = state.dataFaultsMgr->FaultsThermostatOffset(jFault_Thermostat);

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

            faultsTStat.FaultType = cFaultCurrentObject;
            faultsTStat.FaultTypeEnum = Fault::ThermostatOffset;
            faultsTStat.Name = cAlphaArgs(1);
            faultsTStat.FaultyThermostatName = cAlphaArgs(2);

            // Availability schedule
            faultsTStat.AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsTStat.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsTStat.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsTStat.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Severity schedule
            faultsTStat.SeveritySchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                faultsTStat.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsTStat.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(4));
                if (faultsTStat.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Reference offset value is required
            if (lAlphaFieldBlanks(1)) {
                ShowSevereError(state, format("{} = \"{}\" cannot be blank.", cFaultCurrentObject, cNumericFieldNames(1)));
                state.dataFaultsMgr->ErrorsFound = true;
            } else {
                faultsTStat.Offset = rNumericArgs(1);
            }
        }

        // read faults input of Fouling_Coil
        for (int jFault_FoulingCoil = 1; jFault_FoulingCoil <= state.dataFaultsMgr->NumFouledCoil; ++jFault_FoulingCoil) {
            auto &faultsFoulCoil = state.dataFaultsMgr->FouledCoils(jFault_FoulingCoil);

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

            faultsFoulCoil.FaultType = cFaultCurrentObject;
            faultsFoulCoil.FaultTypeEnum = Fault::Fouling_Coil;
            faultsFoulCoil.Name = cAlphaArgs(1);
            faultsFoulCoil.FouledCoilName = cAlphaArgs(2);

            // Availability schedule
            faultsFoulCoil.AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                faultsFoulCoil.AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                faultsFoulCoil.AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                if (faultsFoulCoil.AvaiSchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(3), cAlphaArgs(3)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            // Severity schedule
            faultsFoulCoil.SeveritySchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                faultsFoulCoil.SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                faultsFoulCoil.SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(4));
                if (faultsFoulCoil.SeveritySchedPtr == 0) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" not found.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                    state.dataFaultsMgr->ErrorsFound = true;
                }
            }

            faultsFoulCoil.FoulingInputMethod = static_cast<FouledCoil>(getEnumValue(FouledCoilNamesUC, UtilityRoutines::makeUPPER(cAlphaArgs(5))));
            if (faultsFoulCoil.FoulingInputMethod == FouledCoil::Invalid) {
                faultsFoulCoil.FoulingInputMethod = FouledCoil::UARated;
            }

            faultsFoulCoil.UAFouled = rNumericArgs(1);
            faultsFoulCoil.Rfw = rNumericArgs(2);
            faultsFoulCoil.Rfa = rNumericArgs(3);
            faultsFoulCoil.Aout = rNumericArgs(4);
            faultsFoulCoil.Aratio = rNumericArgs(5);

            // Coil check and link
            {
                // Obtains and Allocates WaterCoil related parameters from input file
                if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
                    WaterCoils::GetWaterCoilInput(state);
                    state.dataWaterCoils->GetWaterCoilsInputFlag = false;
                }

                // Check the coil name and type
                int CoilNum = UtilityRoutines::FindItemInList(faultsFoulCoil.FouledCoilName, state.dataWaterCoils->WaterCoil);
                if (CoilNum <= 0) {
                    ShowSevereError(state,
                                    format("{} = \"{}\". Referenced Coil named \"{}\" was not found.",
                                           cFaultCurrentObject,
                                           cAlphaArgs(1),
                                           faultsFoulCoil.FouledCoilName));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    // Coil is found: check if the right type
                    if ((state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::PlantEquipmentType::CoilWaterSimpleHeating) ||
                        (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::PlantEquipmentType::CoilWaterCooling)) {
                        // Link the Coil with the fault model
                        state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFlag = true;
                        state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingIndex = jFault_FoulingCoil;

                        faultsFoulCoil.FouledCoilType = state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType;
                        faultsFoulCoil.FouledCoilNum = CoilNum;

                        SetupOutputVariable(state,
                                            "Coil Fouling Factor",
                                            OutputProcessor::Unit::K_W,
                                            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataWaterCoils->WaterCoil(CoilNum).Name);

                        // Coil:Cooling:Water doesn't report UA because it's not variable,
                        // but here, it's useful since we do change it via fouling, so report it
                        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::PlantEquipmentType::CoilWaterCooling) {
                            SetupOutputVariable(state,
                                                "Cooling Coil Total U Factor Times Area Value",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil External U Factor Times Area Value",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil Internal U Factor Times Area Value",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil Total U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil External U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilExternal,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state,
                                                "Cooling Coil Internal U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilInternal,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);

                        } else {
                            SetupOutputVariable(state,
                                                "Heating Coil U Factor Times Area Value Before Fouling",
                                                OutputProcessor::Unit::W_K,
                                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
                        }
                    } else {
                        ShowSevereError(
                            state,
                            format("{} = \"{}\" invalid {} = \"{}\".", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(2), cAlphaArgs(2)));
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
                    state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                    if (state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedPtr == 0) {
                        ShowSevereError(state,
                                        format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                               cFaultCurrentObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(2),
                                               cAlphaArgs(2)));
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedule = cAlphaArgs(3);
                // check severity schedule
                if (lAlphaFieldBlanks(3)) {
                    state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr = -1; // returns schedule value of 1
                } else {
                    state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr = ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                    if (state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr == 0) {
                        ShowSevereError(state,
                                        format("{} = \"{}\" invalid {} = \"{}\" not found.",
                                               cFaultCurrentObject,
                                               cAlphaArgs(1),
                                               cAlphaFieldNames(3),
                                               cAlphaArgs(3)));
                        state.dataFaultsMgr->ErrorsFound = true;
                    }
                }

                state.dataFaultsMgr->FaultsEconomizer(j).ControllerType = cAlphaArgs(4);
                // check controller type
                if (lAlphaFieldBlanks(4)) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(4), cAlphaArgs(4)));
                    state.dataFaultsMgr->ErrorsFound = true;
                } else {
                    if (UtilityRoutines::makeUPPER(cAlphaArgs(4)) == "CONTROLLER:OUTDOORAIR") {
                        state.dataFaultsMgr->FaultsEconomizer(j).ControllerTypeEnum = iController_AirEconomizer;

                        // CASE ...

                    } else {
                    }
                }

                state.dataFaultsMgr->FaultsEconomizer(j).ControllerName = cAlphaArgs(5);
                // check controller name
                if (lAlphaFieldBlanks(5)) {
                    ShowSevereError(
                        state,
                        format("{} = \"{}\" invalid {} = \"{}\" blank.", cFaultCurrentObject, cAlphaArgs(1), cAlphaFieldNames(5), cAlphaArgs(5)));
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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFac(0.0); // fault modification factor
        Real64 OffsetAct;     // actual offset after applying the modification factor

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules
            if (this->SeveritySchedPtr >= 0) {
                FaultFac = ScheduleManager::GetCurrentScheduleValue(state, this->SeveritySchedPtr);
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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFac(0.0); // fault modification factor
        Real64 FoulingFactor(
            1.0); // Actual Nominal Fouling Factor, ratio between the nominal capacity or efficiency at fouling case and that at fault free case

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules
            if (this->SeveritySchedPtr >= 0) {
                FaultFac = ScheduleManager::GetCurrentScheduleValue(state, this->SeveritySchedPtr);
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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FaultFac(0.0);             // fault modification factor
        Real64 UAReductionFactorAct(1.0); // actual UA Reduction Factor, ratio between the UA value at fouling case and that at fault free case

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules
            if (this->SeveritySchedPtr >= 0) {
                FaultFac = ScheduleManager::GetCurrentScheduleValue(state, this->SeveritySchedPtr);
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

        // Check fault availability schedules
        if (ScheduleManager::GetCurrentScheduleValue(state, this->AvaiSchedPtr) > 0.0) {

            // Check fault severity schedules (Ptr initialized to -1, so would return a FaultFrac of 1 if not set)
            return ScheduleManager::GetCurrentScheduleValue(state, this->SeveritySchedPtr);
        }

        return 0.0;
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
        Real64 EvapOutletTemp_f;                         // Chiller supply water temperature, faulty case [C]
        Real64 EvapMassFlowRate_f = EvapMassFlowRate_ff; // Chiller mass flow rate, faulty case [kg/s]
        Real64 QEvaporator_f;                            // Chiller evaporator heat transfer rate, faulty case [W]

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

        FanDeltaPressCal = Curve::CurveValue(state, FanCurvePtr, FanMaxAirFlowRate);

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
