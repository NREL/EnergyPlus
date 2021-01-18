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

        bool RunFaultMgrOnceFlag(false); // True if CheckAndReadFaults is already done
        bool ErrorsFound(false);         // True if errors detected in input
    }                                    // namespace

    // ControllerTypeEnum
    int const iController_AirEconomizer(1001);

    // Input methods for fouling coils
    int const iFouledCoil_UARated(9001);
    int const iFouledCoil_FoulingFactor(9002);

    // MODULE VARIABLE DECLARATIONS:
    int const NumFaultTypes(16);
    int const NumFaultTypesEconomizer(5);

    // FaultTypeEnum
    int const iFault_TemperatureSensorOffset_OutdoorAir(101);
    int const iFault_HumiditySensorOffset_OutdoorAir(102);
    int const iFault_EnthalpySensorOffset_OutdoorAir(103);
    int const iFault_TemperatureSensorOffset_ReturnAir(104);
    int const iFault_EnthalpySensorOffset_ReturnAir(105);
    int const iFault_Fouling_Coil(106);
    int const iFault_ThermostatOffset(107);
    int const iFault_HumidistatOffset(108);
    int const iFault_Fouling_AirFilter(109);
    int const iFault_TemperatureSensorOffset_ChillerSupplyWater(110);
    int const iFault_TemperatureSensorOffset_CondenserSupplyWater(111);
    int const iFault_Fouling_Tower(112);
    int const iFault_TemperatureSensorOffset_CoilSupplyAir(113);
    int const iFault_Fouling_Boiler(114);
    int const iFault_Fouling_Chiller(115);
    int const iFault_Fouling_EvapCooler(116);

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

    Array1D_string const cFaults(NumFaultTypes,
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

    Array1D_int const iFaultTypeEnums(NumFaultTypes,
                                      {iFault_TemperatureSensorOffset_OutdoorAir,
                                       iFault_HumiditySensorOffset_OutdoorAir,
                                       iFault_EnthalpySensorOffset_OutdoorAir,
                                       iFault_TemperatureSensorOffset_ReturnAir,
                                       iFault_EnthalpySensorOffset_ReturnAir,
                                       iFault_Fouling_Coil,
                                       iFault_ThermostatOffset,
                                       iFault_HumidistatOffset,
                                       iFault_Fouling_AirFilter,
                                       iFault_TemperatureSensorOffset_ChillerSupplyWater,
                                       iFault_TemperatureSensorOffset_CondenserSupplyWater,
                                       iFault_Fouling_Tower,
                                       iFault_TemperatureSensorOffset_CoilSupplyAir,
                                       iFault_Fouling_Boiler,
                                       iFault_Fouling_Chiller,
                                       iFault_Fouling_EvapCooler});

    bool AnyFaultsInModel(false); // True if there are operational faults in the model
    int NumFaults(0);             // Number of faults (include multiple faults of same type) in the model

    int NumFaultyEconomizer(0);         // Total number of faults related with the economizer
    int NumFouledCoil(0);               // Total number of fouled coils
    int NumFaultyThermostat(0);         // Total number of faulty thermostat with offset
    int NumFaultyHumidistat(0);         // Total number of faulty humidistat with offset
    int NumFaultyAirFilter(0);          // Total number of fouled air filters
    int NumFaultyChillerSWTSensor(0);   // Total number of faulty Chillers Supply Water Temperature Sensor
    int NumFaultyCondenserSWTSensor(0); // Total number of faulty Condenser Supply Water Temperature Sensor
    int NumFaultyTowerFouling(0);       // Total number of faulty Towers with Scaling
    int NumFaultyCoilSATSensor(0);      // Total number of faulty Coil Supply Air Temperature Sensor
    int NumFaultyBoilerFouling(0);      // Total number of faulty Boilers with Fouling
    int NumFaultyChillerFouling(0);     // Total number of faulty Chillers with Fouling
    int NumFaultyEvapCoolerFouling(0);  // Total number of faulty Evaporative Coolers with Fouling

    // SUBROUTINE SPECIFICATIONS:

    // Object Data
    Array1D<FaultPropertiesEconomizer> FaultsEconomizer;
    Array1D<FaultPropertiesFoulingCoil> FouledCoils;
    Array1D<FaultPropertiesThermostat> FaultsThermostatOffset;
    Array1D<FaultPropertiesHumidistat> FaultsHumidistatOffset;
    Array1D<FaultPropertiesAirFilter> FaultsFouledAirFilters;
    Array1D<FaultPropertiesChillerSWT> FaultsChillerSWTSensor;
    Array1D<FaultPropertiesCondenserSWT> FaultsCondenserSWTSensor;
    Array1D<FaultPropertiesTowerFouling> FaultsTowerFouling;
    Array1D<FaultPropertiesCoilSAT> FaultsCoilSATSensor;
    Array1D<FaultPropertiesBoilerFouling> FaultsBoilerFouling;
    Array1D<FaultPropertiesChillerFouling> FaultsChillerFouling;
    Array1D<FaultPropertiesEvapCoolerFouling> FaultsEvapCoolerFouling;

    // Functions

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

        if (RunFaultMgrOnceFlag) return;

        // check number of faults
        NumFaults = 0;
        NumFaultyEconomizer = 0;
        for (int NumFaultsTemp = 0, i = 1; i <= NumFaultTypes; ++i) {
            NumFaultsTemp = inputProcessor->getNumObjectsFound(state, cFaults(i));
            NumFaults += NumFaultsTemp;

            if (i <= 5) {
                // 1st-5th fault: economizer sensor offset
                NumFaultyEconomizer += NumFaultsTemp;
            } else if (i == 6) {
                // 6th fault: Coil fouling
                NumFouledCoil = NumFaultsTemp;
            } else if (i == 7) {
                // 7th fault: Faulty thermostat
                NumFaultyThermostat = NumFaultsTemp;
            } else if (i == 8) {
                // 8th fault: Faulty humidistat
                NumFaultyHumidistat = NumFaultsTemp;
            } else if (i == 9) {
                // 9th fault: Fouled air filter
                NumFaultyAirFilter = NumFaultsTemp;
            } else if (i == 10) {
                // 10th fault: Faulty Chillers Supply Water Temperature Sensor
                NumFaultyChillerSWTSensor = NumFaultsTemp;
            } else if (i == 11) {
                // 11th fault: Faulty Condenser Supply Water Temperature Sensor
                NumFaultyCondenserSWTSensor = NumFaultsTemp;
            } else if (i == 12) {
                // 12th fault: Faulty Towers with Scaling
                NumFaultyTowerFouling = NumFaultsTemp;
            } else if (i == 13) {
                // 13th fault: Faulty Coil Supply Air Temperature Sensor
                NumFaultyCoilSATSensor = NumFaultsTemp;
            } else if (i == 14) {
                // 14th fault: Faulty Boiler with Fouling
                NumFaultyBoilerFouling = NumFaultsTemp;
            } else if (i == 15) {
                // 15th fault: Faulty Chiller with Fouling
                NumFaultyChillerFouling = NumFaultsTemp;
            } else if (i == 16) {
                // 16th fault: Faulty Evaporative Cooler with Fouling
                NumFaultyEvapCoolerFouling = NumFaultsTemp;
            }
        }

        if (NumFaults > 0) {
            AnyFaultsInModel = true;
        } else {
            AnyFaultsInModel = false;
        }

        if (!AnyFaultsInModel) {
            RunFaultMgrOnceFlag = true;
            return;
        }

        // allocate fault array
        if (NumFaultyEconomizer > 0) FaultsEconomizer.allocate(NumFaultyEconomizer);
        if (NumFouledCoil > 0) FouledCoils.allocate(NumFouledCoil);
        if (NumFaultyThermostat > 0) FaultsThermostatOffset.allocate(NumFaultyThermostat);
        if (NumFaultyHumidistat > 0) FaultsHumidistatOffset.allocate(NumFaultyHumidistat);
        if (NumFaultyAirFilter > 0) FaultsFouledAirFilters.allocate(NumFaultyAirFilter);
        if (NumFaultyChillerSWTSensor > 0) FaultsChillerSWTSensor.allocate(NumFaultyChillerSWTSensor);
        if (NumFaultyCondenserSWTSensor > 0) FaultsCondenserSWTSensor.allocate(NumFaultyCondenserSWTSensor);
        if (NumFaultyTowerFouling > 0) FaultsTowerFouling.allocate(NumFaultyTowerFouling);
        if (NumFaultyCoilSATSensor > 0) FaultsCoilSATSensor.allocate(NumFaultyCoilSATSensor);
        if (NumFaultyBoilerFouling > 0) FaultsBoilerFouling.allocate(NumFaultyBoilerFouling);
        if (NumFaultyChillerFouling > 0) FaultsChillerFouling.allocate(NumFaultyChillerFouling);
        if (NumFaultyEvapCoolerFouling > 0) FaultsEvapCoolerFouling.allocate(NumFaultyEvapCoolerFouling);

        // read faults input of Fault_type 116: Evaporative Cooler Fouling
        for (int jFault_EvapCoolerFouling = 1; jFault_EvapCoolerFouling <= NumFaultyEvapCoolerFouling; ++jFault_EvapCoolerFouling) {

            cFaultCurrentObject = cFaults(16); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).FaultType = cFaultCurrentObject;
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).FaultTypeEnum = iFault_Fouling_EvapCooler;
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).FoulingFactor = rNumericArgs(1);

            // Evaporative cooler type
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Evaporative cooler name
            FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Evaporative cooler check
            {
                auto const SELECT_CASE_VAR(FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerType);

                int EvapCoolerNum;

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "EvaporativeCooler:Indirect:WetCoil")) {
                    // Read in evaporative cooler is not done yet
                    if (EvaporativeCoolers::GetInputEvapComponentsFlag) {
                        EvaporativeCoolers::GetEvapInput(state);
                        EvaporativeCoolers::GetInputEvapComponentsFlag = false;
                    }

                    // Check whether the evaporative cooler  name and type match each other;
                    EvapCoolerNum = UtilityRoutines::FindItemInList(FaultsEvapCoolerFouling(jFault_EvapCoolerFouling).EvapCoolerName,
                                                                    EvaporativeCoolers::EvapCond,
                                                                    &EvaporativeCoolers::EvapConditions::EvapCoolerName);
                    if (EvapCoolerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the boiler with the fault model
                        EvaporativeCoolers::EvapCond(EvapCoolerNum).FaultyEvapCoolerFoulingFlag = true;
                        EvaporativeCoolers::EvapCond(EvapCoolerNum).FaultyEvapCoolerFoulingIndex = jFault_EvapCoolerFouling;
                    }
                }
            }
        }

        // read faults input of Fault_type 115: Chiller Fouling
        for (int jFault_ChillerFouling = 1; jFault_ChillerFouling <= NumFaultyChillerFouling; ++jFault_ChillerFouling) {

            cFaultCurrentObject = cFaults(15); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsChillerFouling(jFault_ChillerFouling).FaultType = cFaultCurrentObject;
            FaultsChillerFouling(jFault_ChillerFouling).FaultTypeEnum = iFault_Fouling_Chiller;
            FaultsChillerFouling(jFault_ChillerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsChillerFouling(jFault_ChillerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsChillerFouling(jFault_ChillerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            FaultsChillerFouling(jFault_ChillerFouling).FoulingFactor = rNumericArgs(1);

            // Chiller type
            FaultsChillerFouling(jFault_ChillerFouling).ChillerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Chiller name
            FaultsChillerFouling(jFault_ChillerFouling).ChillerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Chiller check
            {
                auto const SELECT_CASE_VAR(FaultsChillerFouling(jFault_ChillerFouling).ChillerType);

                int ChillerNum;

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto & ch : state.dataPlantChillers->ElectricChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->ElectricChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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
                    ChillerNum = UtilityRoutines::FindItemInList(FaultsChillerFouling(jFault_ChillerFouling).ChillerName,
                                                                 state.dataChillerElectricEIR->ElectricEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataChillerElectricEIR->ElectricEIRChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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
                    ChillerNum = UtilityRoutines::FindItemInList(FaultsChillerFouling(jFault_ChillerFouling).ChillerName,
                                                                 state.dataChillerReformulatedEIR->ElecReformEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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
                    for (auto & ch : state.dataPlantChillers->ConstCOPChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->ConstCOPChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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
                    for (auto & ch : state.dataPlantChillers->EngineDrivenChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->EngineDrivenChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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
                    for (auto & ch : state.dataPlantChillers->GTChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerFouling(jFault_ChillerFouling).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataPlantChillers->GTChiller(ChillerNum).CondenserType != DataPlant::CondenserType::WaterCooled) {
                            // The fault model is only applicable to the chillers with water based condensers
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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

        // read faults input of Fault_type 114: Boiler Fouling
        for (int jFault_BoilerFouling = 1; jFault_BoilerFouling <= NumFaultyBoilerFouling; ++jFault_BoilerFouling) {

            cFaultCurrentObject = cFaults(14); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsBoilerFouling(jFault_BoilerFouling).FaultType = cFaultCurrentObject;
            FaultsBoilerFouling(jFault_BoilerFouling).FaultTypeEnum = iFault_Fouling_Boiler;
            FaultsBoilerFouling(jFault_BoilerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsBoilerFouling(jFault_BoilerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsBoilerFouling(jFault_BoilerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // CapReductionFactor - degree of fault
            FaultsBoilerFouling(jFault_BoilerFouling).FoulingFactor = rNumericArgs(1);

            // Boiler type
            FaultsBoilerFouling(jFault_BoilerFouling).BoilerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Boiler name
            FaultsBoilerFouling(jFault_BoilerFouling).BoilerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Boiler check and link
            {
                if (state.dataBoilers->getBoilerInputFlag) {
                    Boilers::GetBoilerInput(state);
                    state.dataBoilers->getBoilerInputFlag = false;
                }
                // Check the boiler name and boiler type
                int BoilerNum = UtilityRoutines::FindItemInList(FaultsBoilerFouling(jFault_BoilerFouling).BoilerName, state.dataBoilers->Boiler);
                if (BoilerNum <= 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" not found.");
                    ErrorsFound = true;
                } else {
                    // Link the boiler with the fault model
                    state.dataBoilers->Boiler(BoilerNum).FaultyBoilerFoulingFlag = true;
                    state.dataBoilers->Boiler(BoilerNum).FaultyBoilerFoulingIndex = jFault_BoilerFouling;
                }
            }
        }

        // read faults input of Fault_type 113: Coil SAT Sensor Offset
        for (int jFault_CoilSAT = 1; jFault_CoilSAT <= NumFaultyCoilSATSensor; ++jFault_CoilSAT) {

            cFaultCurrentObject = cFaults(13); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsCoilSATSensor(jFault_CoilSAT).FaultType = cFaultCurrentObject;
            FaultsCoilSATSensor(jFault_CoilSAT).FaultTypeEnum = iFault_TemperatureSensorOffset_CoilSupplyAir;
            FaultsCoilSATSensor(jFault_CoilSAT).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsCoilSATSensor(jFault_CoilSAT).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsCoilSATSensor(jFault_CoilSAT).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // offset - degree of fault
            FaultsCoilSATSensor(jFault_CoilSAT).Offset = rNumericArgs(1);

            // Coil type
            FaultsCoilSATSensor(jFault_CoilSAT).CoilType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Coil name
            FaultsCoilSATSensor(jFault_CoilSAT).CoilName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Coil check and link
            {
                auto const SELECT_CASE_VAR(FaultsCoilSATSensor(jFault_CoilSAT).CoilType);

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Electric") ||
                    UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Fuel") ||
                    UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Desuperheater")) {
                    // Read in coil input if not done yet
                    if (HeatingCoils::GetCoilsInputFlag) {
                        HeatingCoils::GetHeatingCoilInput(state);
                        HeatingCoils::GetCoilsInputFlag = false;
                    }
                    // Check the coil name and coil type
                    int CoilNum = UtilityRoutines::FindItemInList(FaultsCoilSATSensor(jFault_CoilSAT).CoilName, HeatingCoils::HeatingCoil);
                    if (CoilNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the coil with the fault model
                        HeatingCoils::HeatingCoil(CoilNum).FaultyCoilSATFlag = true;
                        HeatingCoils::HeatingCoil(CoilNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Coil:Heating:Steam")) {

                    // Read in coil input if not done yet
                    if (state.dataSteamCoils->GetSteamCoilsInputFlag) {
                        SteamCoils::GetSteamCoilInput(state);
                        state.dataSteamCoils->GetSteamCoilsInputFlag = false;
                    }
                    // Check the coil name and coil type
                    int CoilNum = UtilityRoutines::FindItemInList(FaultsCoilSATSensor(jFault_CoilSAT).CoilName, state.dataSteamCoils->SteamCoil);
                    if (CoilNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {

                        if (state.dataSteamCoils->SteamCoil(CoilNum).TypeOfCoil != state.dataSteamCoils->TemperatureSetPointControl) {
                            // The fault model is only applicable to the coils controlled on leaving air temperature
                            ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                             cAlphaArgs(5) +
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
                    int CoilNum = UtilityRoutines::FindItemInList(FaultsCoilSATSensor(jFault_CoilSAT).CoilName, state.dataWaterCoils->WaterCoil);
                    if (CoilNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    }

                    // Read in Water Coil Controller Name
                    FaultsCoilSATSensor(jFault_CoilSAT).WaterCoilControllerName = cAlphaArgs(6);
                    if (lAlphaFieldBlanks(6)) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + " = \"" +
                                        cAlphaArgs(6) + "\" blank.");
                        ErrorsFound = true;
                    }
                    // Read in controller input if not done yet
                    if (HVACControllers::GetControllerInputFlag) {
                        HVACControllers::GetControllerInput(state);
                        HVACControllers::GetControllerInputFlag = false;
                    }
                    // Check the controller name
                    int ControlNum = UtilityRoutines::FindItemInList(FaultsCoilSATSensor(jFault_CoilSAT).WaterCoilControllerName,
                                                                     HVACControllers::ControllerProps,
                                                                     &HVACControllers::ControllerPropsType::ControllerName);
                    if (ControlNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + " = \"" +
                                        cAlphaArgs(6) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the controller with the fault model
                        HVACControllers::ControllerProps(ControlNum).FaultyCoilSATFlag = true;
                        HVACControllers::ControllerProps(ControlNum).FaultyCoilSATIndex = jFault_CoilSAT;

                        // Check whether the controller match the coil
                        if (HVACControllers::ControllerProps(ControlNum).SensedNode != state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum) {
                            ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + " = \"" +
                                            cAlphaArgs(6) + "\" does not match " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5));
                            ErrorsFound = true;
                        }
                    }
                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "CoilSystem:Cooling:DX")) {
                    // Read in DXCoolingSystem input if not done yet
                    if (HVACDXSystem::GetInputFlag) {
                        HVACDXSystem::GetDXCoolingSystemInput(state);
                        HVACDXSystem::GetInputFlag = false;
                    }

                    // Check the coil name and coil type
                    int CoilSysNum = UtilityRoutines::FindItemInList(FaultsCoilSATSensor(jFault_CoilSAT).CoilName, HVACDXSystem::DXCoolingSystem);
                    if (CoilSysNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the coil system with the fault model
                        HVACDXSystem::DXCoolingSystem(CoilSysNum).FaultyCoilSATFlag = true;
                        HVACDXSystem::DXCoolingSystem(CoilSysNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }
                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "CoilSystem:Heating:DX")) {
                    // Read in DXCoolingSystem input if not done yet
                    if (HVACDXHeatPumpSystem::GetInputFlag) {
                        HVACDXHeatPumpSystem::GetDXHeatPumpSystemInput(state);
                        HVACDXHeatPumpSystem::GetInputFlag = false;
                    }

                    // Check the coil name and coil type
                    int CoilSysNum =
                        UtilityRoutines::FindItemInList(FaultsCoilSATSensor(jFault_CoilSAT).CoilName, HVACDXHeatPumpSystem::DXHeatPumpSystem);
                    if (CoilSysNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the coil system with the fault model
                        HVACDXHeatPumpSystem::DXHeatPumpSystem(CoilSysNum).FaultyCoilSATFlag = true;
                        HVACDXHeatPumpSystem::DXHeatPumpSystem(CoilSysNum).FaultyCoilSATIndex = jFault_CoilSAT;
                    }
                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "AirLoopHVAC:UnitarySystem")) {
                    // UnitarySystem model connects to FaultManager via function call to FaultsManager::SetFaultyCoilSATSensor
                }
            }
        } // End read faults input of Fault_type 113

        // read faults input of Fault_type 112: Cooling tower scaling
        for (int jFault_TowerFouling = 1; jFault_TowerFouling <= NumFaultyTowerFouling; ++jFault_TowerFouling) {

            cFaultCurrentObject = cFaults(12); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsTowerFouling(jFault_TowerFouling).FaultType = cFaultCurrentObject;
            FaultsTowerFouling(jFault_TowerFouling).FaultTypeEnum = iFault_Fouling_Tower;
            FaultsTowerFouling(jFault_TowerFouling).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsTowerFouling(jFault_TowerFouling).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsTowerFouling(jFault_TowerFouling).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsTowerFouling(jFault_TowerFouling).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsTowerFouling(jFault_TowerFouling).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsTowerFouling(jFault_TowerFouling).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsTowerFouling(jFault_TowerFouling).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsTowerFouling(jFault_TowerFouling).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsTowerFouling(jFault_TowerFouling).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // UAReductionFactor - degree of fault
            FaultsTowerFouling(jFault_TowerFouling).UAReductionFactor = rNumericArgs(1);

            // Cooling tower type
            FaultsTowerFouling(jFault_TowerFouling).TowerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Cooling tower name
            FaultsTowerFouling(jFault_TowerFouling).TowerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Tower check and link
            {
                // Read in tower input if not done yet
                if (state.dataCondenserLoopTowers->GetInput) {
                    CondenserLoopTowers::GetTowerInput(state);
                    state.dataCondenserLoopTowers->GetInput = false;
                }
                // Check the tower name and tower type
                int TowerNum = UtilityRoutines::FindItemInList(FaultsTowerFouling(jFault_TowerFouling).TowerName, state.dataCondenserLoopTowers->towers);
                if (TowerNum <= 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" not found.");
                    ErrorsFound = true;
                } else {
                    // Link the tower with the fault model
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingFlag = true;
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingIndex = jFault_TowerFouling;

                    // Check the faulty tower type
                    if (!UtilityRoutines::SameString(state.dataCondenserLoopTowers->towers(TowerNum).TowerType,
                                                     FaultsTowerFouling(jFault_TowerFouling).TowerType)) {
                        ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" +
                                         cAlphaArgs(4) + "\" not match the type of " + cAlphaFieldNames(5) +
                                         ". Tower type in the fault model is updated. ");
                        FaultsTowerFouling(jFault_TowerFouling).TowerType = state.dataCondenserLoopTowers->towers(TowerNum).TowerType;
                    }

                    // Check the tower model
                    // Performance Input Method should be UFactorTimesAreaAndDesignWaterFlowRate to apply the fault model
                    if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num != CondenserLoopTowers::PIM::UFactor) {
                        ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                         cAlphaFieldNames(5) + ". Tower Performance Input Method is not UFactorTimesAreaAndDesignWaterFlowRate. " +
                                         "The tower fouling fault model will not be applied to the tower. ");
                        state.dataCondenserLoopTowers->towers(TowerNum).FaultyTowerFoulingFlag = false;
                    }
                }
            }
        }

        // read faults input of Fault_type 111: Condenser SWT Sensor Offset
        for (int jFault_CondenserSWT = 1; jFault_CondenserSWT <= NumFaultyCondenserSWTSensor; ++jFault_CondenserSWT) {

            cFaultCurrentObject = cFaults(11); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsCondenserSWTSensor(jFault_CondenserSWT).FaultType = cFaultCurrentObject;
            FaultsCondenserSWTSensor(jFault_CondenserSWT).FaultTypeEnum = iFault_TemperatureSensorOffset_CondenserSupplyWater;
            FaultsCondenserSWTSensor(jFault_CondenserSWT).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsCondenserSWTSensor(jFault_CondenserSWT).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsCondenserSWTSensor(jFault_CondenserSWT).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // offset - degree of fault
            FaultsCondenserSWTSensor(jFault_CondenserSWT).Offset = rNumericArgs(1);

            // Cooling tower type
            FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Cooling tower name
            FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Tower check and link
            {
                // Read in tower input if not done yet
                if (state.dataCondenserLoopTowers->GetInput) {
                    CondenserLoopTowers::GetTowerInput(state);
                    state.dataCondenserLoopTowers->GetInput = false;
                }
                // Check the tower name and tower type
                int TowerNum =
                    UtilityRoutines::FindItemInList(FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerName, state.dataCondenserLoopTowers->towers);
                if (TowerNum <= 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" not found.");
                    ErrorsFound = true;
                } else {
                    // Link the tower with the fault model
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyCondenserSWTFlag = true;
                    state.dataCondenserLoopTowers->towers(TowerNum).FaultyCondenserSWTIndex = jFault_CondenserSWT;

                    // Check the faulty tower type
                    if (!UtilityRoutines::SameString(state.dataCondenserLoopTowers->towers(TowerNum).TowerType,
                                                     FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerType)) {
                        ShowWarningError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" +
                                         cAlphaArgs(4) + "\" not match the type of " + cAlphaFieldNames(5) + ". Tower type is updated. ");
                        FaultsCondenserSWTSensor(jFault_CondenserSWT).TowerType = state.dataCondenserLoopTowers->towers(TowerNum).TowerType;
                    }
                }
            }
        }

        // read faults input of Fault_type 110: Chiller SWT Sensor Offset
        for (int jFault_ChillerSWT = 1; jFault_ChillerSWT <= NumFaultyChillerSWTSensor; ++jFault_ChillerSWT) {

            cFaultCurrentObject = cFaults(10); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsChillerSWTSensor(jFault_ChillerSWT).FaultType = cFaultCurrentObject;
            FaultsChillerSWTSensor(jFault_ChillerSWT).FaultTypeEnum = iFault_TemperatureSensorOffset_ChillerSupplyWater;
            FaultsChillerSWTSensor(jFault_ChillerSWT).Name = cAlphaArgs(1);

            // Fault availability schedule
            FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (FaultsChillerSWTSensor(jFault_ChillerSWT).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fault severity schedule
            FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsChillerSWTSensor(jFault_ChillerSWT).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // offset - degree of fault
            FaultsChillerSWTSensor(jFault_ChillerSWT).Offset = rNumericArgs(1);

            // Chiller type
            FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerType = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Chiller name
            FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                "\" blank.");
                ErrorsFound = true;
            }

            // Chiller check
            {
                auto const SELECT_CASE_VAR(FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerType);

                int ChillerNum;

                if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:Electric")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto & ch : state.dataPlantChillers->ElectricChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
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
                    ChillerNum = UtilityRoutines::FindItemInList(FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerElectricEIR->ElectricEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
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
                    ChillerNum = UtilityRoutines::FindItemInList(FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerReformulatedEIR->ElecReformEIRChiller);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataChillerReformulatedEIR->ElecReformEIRChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:EngineDriven")) {
                    // Check whether the chiller name and chiller type match each other
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto & ch : state.dataPlantChillers->EngineDrivenChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataPlantChillers->EngineDrivenChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:CombustionTurbine")) {
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto & ch : state.dataPlantChillers->GTChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        // Link the chiller with the fault model
                        state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataPlantChillers->GTChiller(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }

                } else if (UtilityRoutines::SameString(SELECT_CASE_VAR, "Chiller:ConstantCOP")) {
                    ChillerNum = 0;
                    int thisChil = 0;
                    for (auto & ch : state.dataPlantChillers->ConstCOPChiller) {
                        thisChil++;
                        if (ch.Name == FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName) {
                            ChillerNum = thisChil;
                        }
                    }
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
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
                    ChillerNum =
                        UtilityRoutines::FindItemInList(FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName, state.dataChillerAbsorber->absorptionChillers);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
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
                    ChillerNum = UtilityRoutines::FindItemInList(FaultsChillerSWTSensor(jFault_ChillerSWT).ChillerName,
                                                                 state.dataChillerIndirectAbsorption->IndirectAbsorber);
                    if (ChillerNum <= 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    } else {
                        state.dataChillerIndirectAbsorption->IndirectAbsorber(ChillerNum).FaultyChillerSWTFlag = true;
                        state.dataChillerIndirectAbsorption->IndirectAbsorber(ChillerNum).FaultyChillerSWTIndex = jFault_ChillerSWT;
                    }
                }
            }
        }

        // read faults input of Fault_type 109: Fouled Air Filters
        for (int jFault_AirFilter = 1; jFault_AirFilter <= NumFaultyAirFilter; ++jFault_AirFilter) {

            // Read in fan if not done yet
            if (state.dataFans->GetFanInputFlag) {
                Fans::GetFanInput(state);
            }

            cFaultCurrentObject = cFaults(9); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsFouledAirFilters(jFault_AirFilter).FaultType = cFaultCurrentObject;
            FaultsFouledAirFilters(jFault_AirFilter).FaultTypeEnum = iFault_Fouling_AirFilter;
            FaultsFouledAirFilters(jFault_AirFilter).Name = cAlphaArgs(1);

            // Information of the fan associated with the fouling air filter
            FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanType = cAlphaArgs(2);
            FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanName = cAlphaArgs(3);

            // Check whether the specified fan exists in the fan list
            if (UtilityRoutines::FindItemInList(cAlphaArgs(3), Fans::Fan, &Fans::FanEquipConditions::FanName) <= 0) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                "\" not found.");
                ErrorsFound = true;
            }

            // Assign fault index to the fan object
            for (int FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
                if (UtilityRoutines::SameString(Fans::Fan(FanNum).FanName, cAlphaArgs(3))) {
                    Fans::Fan(FanNum).FaultyFilterFlag = true;
                    Fans::Fan(FanNum).FaultyFilterIndex = jFault_AirFilter;
                    break;
                }
            }

            // Fault availability schedule
            FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (FaultsFouledAirFilters(jFault_AirFilter).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fan pressure increase fraction schedule
            FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSche = cAlphaArgs(5);
            if (lAlphaFieldBlanks(5)) {
                FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSchePtr = -1; // returns schedule value of 1
            } else {
                FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSchePtr = GetScheduleIndex(state, cAlphaArgs(5));
                if (FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterPressFracSchePtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Fan curve describing the relationship between fan pressure rise and air flow rate
            FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurve = cAlphaArgs(6);
            FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurvePtr = GetCurveIndex(state, cAlphaArgs(6));
            if (FaultsFouledAirFilters(jFault_AirFilter).FaultyAirFilterFanCurvePtr == 0) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\"");
                ShowContinueError(state, "Invalid " + cAlphaFieldNames(6) + " = \"" + cAlphaArgs(6) + "\" not found.");
                ErrorsFound = true;
            }

            // Checking  whether the specified fan curve covers the **design** operational point of the fan cannot be done here
            // as the fan might be autosized and is not sized yet, so we call it in Fan::SizeFan instead

            // In the fan object, calculate by each time-step: 1) pressure increase value; 2) air flow rate decrease value.
        }

        // read faults input of Fault_type 108: HumidistatOffset
        for (int jFault_Humidistat = 1; jFault_Humidistat <= NumFaultyHumidistat; ++jFault_Humidistat) {

            cFaultCurrentObject = cFaults(8); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsHumidistatOffset(jFault_Humidistat).FaultType = cFaultCurrentObject;
            FaultsHumidistatOffset(jFault_Humidistat).FaultTypeEnum = iFault_HumidistatOffset;
            FaultsHumidistatOffset(jFault_Humidistat).Name = cAlphaArgs(1);
            FaultsHumidistatOffset(jFault_Humidistat).FaultyHumidistatName = cAlphaArgs(2);
            FaultsHumidistatOffset(jFault_Humidistat).FaultyHumidistatType = cAlphaArgs(3);

            if (UtilityRoutines::SameString(FaultsHumidistatOffset(jFault_Humidistat).FaultyHumidistatType, "ThermostatOffsetDependent")) {
                // For Humidistat Offset Type: ThermostatOffsetDependent

                // Related Thermostat Offset Fault Name is required for Humidistat Offset Type: ThermostatOffsetDependent
                if (lAlphaFieldBlanks(6)) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\": " + cAlphaFieldNames(6) +
                                    " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetDependent\".");
                    ErrorsFound = true;
                } else {
                    FaultsHumidistatOffset(jFault_Humidistat).FaultyThermostatName = cAlphaArgs(6);
                }

            } else {
                // For Humidistat Offset Type: ThermostatOffsetIndependent

                // Availability schedule
                FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedule = cAlphaArgs(4);
                if (lAlphaFieldBlanks(4)) {
                    FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedPtr = -1; // returns schedule value of 1
                } else {
                    FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                    if (FaultsHumidistatOffset(jFault_Humidistat).AvaiSchedPtr == 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" +
                                        cAlphaArgs(4) + "\" not found.");
                        ErrorsFound = true;
                    }
                }

                // Severity schedule
                FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedule = cAlphaArgs(5);
                if (lAlphaFieldBlanks(5)) {
                    FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedPtr = -1; // returns schedule value of 1
                } else {
                    FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(5));
                    if (FaultsHumidistatOffset(jFault_Humidistat).SeveritySchedPtr == 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" +
                                        cAlphaArgs(5) + "\" not found.");
                        ErrorsFound = true;
                    }
                }

                // Reference offset value is required for Humidistat Offset Type: ThermostatOffsetIndependent
                if (lNumericFieldBlanks(1)) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\": " + cNumericFieldNames(1) +
                                    " cannot be blank for Humidistat Offset Type = \"ThermostatOffsetIndependent\".");
                    ErrorsFound = true;
                } else {
                    FaultsHumidistatOffset(jFault_Humidistat).Offset = rNumericArgs(1);
                }
            }
        }

        // read faults input of Fault_type 107: ThermostatOffset
        for (int jFault_Thermostat = 1; jFault_Thermostat <= NumFaultyThermostat; ++jFault_Thermostat) {

            cFaultCurrentObject = cFaults(7); // fault object string
            inputProcessor->getObjectItem(state,
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

            FaultsThermostatOffset(jFault_Thermostat).FaultType = cFaultCurrentObject;
            FaultsThermostatOffset(jFault_Thermostat).FaultTypeEnum = iFault_ThermostatOffset;
            FaultsThermostatOffset(jFault_Thermostat).Name = cAlphaArgs(1);
            FaultsThermostatOffset(jFault_Thermostat).FaultyThermostatName = cAlphaArgs(2);

            // Availability schedule
            FaultsThermostatOffset(jFault_Thermostat).AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FaultsThermostatOffset(jFault_Thermostat).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsThermostatOffset(jFault_Thermostat).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FaultsThermostatOffset(jFault_Thermostat).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Severity schedule
            FaultsThermostatOffset(jFault_Thermostat).SeveritySchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                FaultsThermostatOffset(jFault_Thermostat).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FaultsThermostatOffset(jFault_Thermostat).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (FaultsThermostatOffset(jFault_Thermostat).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Reference offset value is required
            if (lNumericFieldBlanks(1)) {
                ShowSevereError(state, cFaultCurrentObject + " = \"" + cNumericFieldNames(1) + "\" cannot be blank.");
                ErrorsFound = true;
            } else {
                FaultsThermostatOffset(jFault_Thermostat).Offset = rNumericArgs(1);
            }
        }

        // read faults input of Fault_type 106: Fouling_Coil
        for (int jFault_FoulingCoil = 1; jFault_FoulingCoil <= NumFouledCoil; ++jFault_FoulingCoil) {

            cFaultCurrentObject = cFaults(6); // fault object string
            inputProcessor->getObjectItem(state,
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

            FouledCoils(jFault_FoulingCoil).FaultType = cFaultCurrentObject;
            FouledCoils(jFault_FoulingCoil).FaultTypeEnum = iFault_Fouling_Coil;
            FouledCoils(jFault_FoulingCoil).Name = cAlphaArgs(1);
            FouledCoils(jFault_FoulingCoil).FouledCoilName = cAlphaArgs(2);

            // Availability schedule
            FouledCoils(jFault_FoulingCoil).AvaiSchedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                FouledCoils(jFault_FoulingCoil).AvaiSchedPtr = -1; // returns schedule value of 1
            } else {
                FouledCoils(jFault_FoulingCoil).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (FouledCoils(jFault_FoulingCoil).AvaiSchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" + cAlphaArgs(3) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Severity schedule
            FouledCoils(jFault_FoulingCoil).SeveritySchedule = cAlphaArgs(4);
            if (lAlphaFieldBlanks(4)) {
                FouledCoils(jFault_FoulingCoil).SeveritySchedPtr = -1; // returns schedule value of 1
            } else {
                FouledCoils(jFault_FoulingCoil).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (FouledCoils(jFault_FoulingCoil).SeveritySchedPtr == 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" not found.");
                    ErrorsFound = true;
                }
            }

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(5)));
                if (SELECT_CASE_var == "FOULEDUARATED") {
                    FouledCoils(jFault_FoulingCoil).FoulingInputMethod = iFouledCoil_UARated;

                } else if (SELECT_CASE_var == "FOULINGFACTOR") {
                    FouledCoils(jFault_FoulingCoil).FoulingInputMethod = iFouledCoil_FoulingFactor;

                } else {
                    FouledCoils(jFault_FoulingCoil).FoulingInputMethod = iFouledCoil_UARated;
                }
            }

            FouledCoils(jFault_FoulingCoil).UAFouled = rNumericArgs(1);
            FouledCoils(jFault_FoulingCoil).Rfw = rNumericArgs(2);
            FouledCoils(jFault_FoulingCoil).Rfa = rNumericArgs(3);
            FouledCoils(jFault_FoulingCoil).Aout = rNumericArgs(4);
            FouledCoils(jFault_FoulingCoil).Aratio = rNumericArgs(5);


            // Coil check and link
            {
                // Obtains and Allocates WaterCoil related parameters from input file
                if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
                    WaterCoils::GetWaterCoilInput(state);
                    state.dataWaterCoils->GetWaterCoilsInputFlag = false;
                }

                // Check the coil name and type
                int CoilNum = UtilityRoutines::FindItemInList(FouledCoils(jFault_FoulingCoil).FouledCoilName, state.dataWaterCoils->WaterCoil);
                if (CoilNum <= 0) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\". Referenced Coil named \"" +
                                    FouledCoils(jFault_FoulingCoil).FouledCoilName + "\" was not found.");
                    ErrorsFound = true;
                } else {
                    // Coil is found: check if the right type
                    if ( (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num == state.dataWaterCoils->WaterCoil_SimpleHeating) ||
                         (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num == state.dataWaterCoils->WaterCoil_Cooling) )
                    {
                        // Link the Coil with the fault model
                        state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFlag = true;
                        state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingIndex = jFault_FoulingCoil;

                        FouledCoils(jFault_FoulingCoil).FouledCoiledType = state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num;
                        FouledCoils(jFault_FoulingCoil).FouledCoilNum = CoilNum;

                        SetupOutputVariable(state, "Coil Fouling Factor",
                                    OutputProcessor::Unit::K_W,
                                    state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                        // Coil:Cooling:Water doesn't report UA because it's not variable,
                        // but here, it's useful since we do change it via fouling, so report it
                        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num == state.dataWaterCoils->WaterCoil_Cooling) {
                            SetupOutputVariable(state, "Cooling Coil Total U Factor Times Area Value",
                                    OutputProcessor::Unit::W_K,
                                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state, "Cooling Coil External U Factor Times Area Value",
                                    OutputProcessor::Unit::W_K,
                                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state, "Cooling Coil Internal U Factor Times Area Value",
                                    OutputProcessor::Unit::W_K,
                                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state, "Cooling Coil Total U Factor Times Area Value Before Fouling",
                                    OutputProcessor::Unit::W_K,
                                    state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state, "Cooling Coil External U Factor Times Area Value Before Fouling",
                                    OutputProcessor::Unit::W_K,
                                    state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilExternal,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                            SetupOutputVariable(state, "Cooling Coil Internal U Factor Times Area Value Before Fouling",
                                    OutputProcessor::Unit::W_K,
                                    state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilInternal,
                                    "System",
                                    "Average",
                                    state.dataWaterCoils->WaterCoil(CoilNum).Name);

                        } else {
                            SetupOutputVariable(state, "Heating Coil U Factor Times Area Value Before Fouling",
                                OutputProcessor::Unit::W_K,
                                state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable,
                                "System",
                                "Average",
                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
                        }
                    } else {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid "
                                       + cAlphaFieldNames(2) + " = \"" + cAlphaArgs(2) + "\".");
                        ShowContinueError(state, "Coil was found but it is not one of the supported types (\"Coil:Cooling:Water\" or \"Coil:Heating:Water\").");
                        ErrorsFound = true;
                    }
                }
            }
        }

        // read faults input: Fault_type 101-105, which are related with economizer sensors
        for (int j = 0, i = 1; i <= NumFaultTypesEconomizer; ++i) {
            cFaultCurrentObject = cFaults(i); // fault object string
            int NumFaultsTemp = inputProcessor->getNumObjectsFound(state, cFaultCurrentObject);

            for (int jj = 1; jj <= NumFaultsTemp; ++jj) {
                inputProcessor->getObjectItem(state,
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
                FaultsEconomizer(j).FaultType = cFaultCurrentObject;
                FaultsEconomizer(j).FaultTypeEnum = iFaultTypeEnums(i);

                FaultsEconomizer(j).Name = cAlphaArgs(1);
                FaultsEconomizer(j).AvaiSchedule = cAlphaArgs(2);
                // check availability schedule
                if (lAlphaFieldBlanks(2)) {
                    FaultsEconomizer(j).AvaiSchedPtr = -1; // returns schedule value of 1
                } else {
                    FaultsEconomizer(j).AvaiSchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                    if (FaultsEconomizer(j).AvaiSchedPtr == 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + " = \"" +
                                        cAlphaArgs(2) + "\" not found.");
                        ErrorsFound = true;
                    }
                }

                FaultsEconomizer(j).SeveritySchedule = cAlphaArgs(3);
                // check severity schedule
                if (lAlphaFieldBlanks(3)) {
                    FaultsEconomizer(j).SeveritySchedPtr = -1; // returns schedule value of 1
                } else {
                    FaultsEconomizer(j).SeveritySchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                    if (FaultsEconomizer(j).SeveritySchedPtr == 0) {
                        ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + " = \"" +
                                        cAlphaArgs(3) + "\" not found.");
                        ErrorsFound = true;
                    }
                }

                FaultsEconomizer(j).ControllerType = cAlphaArgs(4);
                // check controller type
                if (lAlphaFieldBlanks(4)) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + " = \"" + cAlphaArgs(4) +
                                    "\" blank.");
                    ErrorsFound = true;
                } else {
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)));
                        if (SELECT_CASE_var == "CONTROLLER:OUTDOORAIR") {
                            FaultsEconomizer(j).ControllerTypeEnum = iController_AirEconomizer;

                            // CASE ...

                        } else {
                        }
                    }
                }

                FaultsEconomizer(j).ControllerName = cAlphaArgs(5);
                // check controller name
                if (lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, cFaultCurrentObject + " = \"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + " = \"" + cAlphaArgs(5) +
                                    "\" blank.");
                    ErrorsFound = true;
                }

                // offset - degree of fault
                FaultsEconomizer(j).Offset = rNumericArgs(1);
            }
        }

        RunFaultMgrOnceFlag = true;

        if (ErrorsFound) {
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
        Real64 FaultFac(0.0);  // fault modification factor
        Real64 OffsetAct;      // actual offset after applying the modification factor

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

        // FLOW

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

        // FLOW

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

        // FLOW

        FanFound = false;

        for (int FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
            if (UtilityRoutines::SameString(Fan(FanNum).FanName, FanName)) {
                FanMaxAirFlowRate = Fan(FanNum).MaxAirFlowRate;
                FanDeltaPress = Fan(FanNum).DeltaPress;
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

    // Clears the global data in Fans.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        RunFaultMgrOnceFlag = false;
        ErrorsFound = false;
        AnyFaultsInModel = false;

        NumFaults = 0;
        NumFaultyEconomizer = 0;
        NumFouledCoil = 0;
        NumFaultyThermostat = 0;
        NumFaultyHumidistat = 0;
        NumFaultyAirFilter = 0;
        NumFaultyChillerSWTSensor = 0;
        NumFaultyCondenserSWTSensor = 0;
        NumFaultyTowerFouling = 0;
        NumFaultyCoilSATSensor = 0;

        FaultsEconomizer.deallocate();
        FouledCoils.deallocate();
        FaultsThermostatOffset.deallocate();
        FaultsHumidistatOffset.deallocate();
        FaultsFouledAirFilters.deallocate();
        FaultsChillerSWTSensor.deallocate();
        FaultsCondenserSWTSensor.deallocate();
        FaultsTowerFouling.deallocate();
        FaultsCoilSATSensor.deallocate();
    }

    void SetFaultyCoilSATSensor(std::string const &CompType, std::string const &CompName, bool &FaultyCoilSATFlag, int &FaultyCoilSATIndex)
    {

        FaultyCoilSATFlag = false;
        FaultyCoilSATIndex = 0;
        if (NumFaultyCoilSATSensor == 0) return;
        for (int jFault_CoilSAT = 1; jFault_CoilSAT <= NumFaultyCoilSATSensor; ++jFault_CoilSAT) {
            if (UtilityRoutines::SameString(FaultsCoilSATSensor(jFault_CoilSAT).CoilType, CompType) &&
                UtilityRoutines::SameString(FaultsCoilSATSensor(jFault_CoilSAT).CoilName, CompName)) {
                FaultyCoilSATFlag = true;
                FaultyCoilSATIndex = jFault_CoilSAT;
                break;
            }
        }
    }

} // namespace FaultsManager

} // namespace EnergyPlus
