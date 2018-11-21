// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef FaultsManager_hh_INCLUDED
#define FaultsManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace FaultsManager {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE TYPE DECLARATIONS:

    // ControllerTypeEnum
    extern int const iController_AirEconomizer;

    // Input methods for fouling coils
    extern int const iFouledCoil_UARated;
    extern int const iFouledCoil_FoulingFactor;

    // MODULE VARIABLE DECLARATIONS:
    extern int const NumFaultTypes;
    extern int const NumFaultTypesEconomizer;

    // FaultTypeEnum
    extern int const iFault_TemperatureSensorOffset_OutdoorAir;
    extern int const iFault_HumiditySensorOffset_OutdoorAir;
    extern int const iFault_EnthalpySensorOffset_OutdoorAir;
    extern int const iFault_TemperatureSensorOffset_ReturnAir;
    extern int const iFault_EnthalpySensorOffset_ReturnAir;
    extern int const iFault_Fouling_Coil;
    extern int const iFault_ThermostatOffset;
    extern int const iFault_HumidistatOffset;
    extern int const iFault_Fouling_AirFilter;
    extern int const iFault_TemperatureSensorOffset_ChillerSupplyWater;
    extern int const iFault_TemperatureSensorOffset_CondenserSupplyWater;
    extern int const iFault_TemperatureSensorOffset_CoilSupplyAir;
    extern int const iFault_Fouling_Tower;
    extern int const iFault_Fouling_Boiler;
    extern int const iFault_Fouling_Chiller;
    extern int const iFault_Fouling_EvapCooler;

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

    extern Array1D_string const cFaults;
    //      'FaultModel:PressureSensorOffset:OutdoorAir   ', &
    //      'FaultModel:TemperatureSensorOffset:SupplyAir ', &
    //      'FaultModel:TemperatureSensorOffset:ZoneAir   ', &
    //      'FaultModel:Blockage:Branch                   ', &
    //      'FaultModel:Dirty:AirFilter                   ', &
    //      'FaultModel:Fouling:Chiller                   ', &
    //      'FaultModel:Fouling:Boiler                    ', &
    //      'FaultModel:Fouling:CoolingTower              ', &
    //      'FaultModel:DamperLeakage:ReturnAir           ', &
    //      'FaultModel:DamperLeakage:OutdoorAir          ' /)

    extern Array1D_int const iFaultTypeEnums;

    extern bool AnyFaultsInModel;           // True if there are operational faults in the model
    extern int NumFaults;                   // Total number of all faults
    extern int NumFaultyEconomizer;         // Total number of faults related with the economizer
    extern int NumFouledCoil;               // Total number of fouled coils
    extern int NumFaultyThermostat;         // Total number of faulty thermostat with offset
    extern int NumFaultyHumidistat;         // Total number of faulty humidistat with offset
    extern int NumFaultyAirFilter;          // Total number of fouled air filters
    extern int NumFaultyChillerSWTSensor;   // Total number of faulty Chillers Supply Water Temperature Sensor
    extern int NumFaultyCondenserSWTSensor; // Total number of faulty Condenser Supply Water Temperature Sensor
    extern int NumFaultyTowerFouling;       // Total number of faulty Towers with Scaling
    extern int NumFaultyCoilSATSensor;      // Total number of faulty Coil Supply Air Temperature Sensor
    extern int NumFaultyBoilerFouling;      // Total number of faulty Boilers with Fouling
    extern int NumFaultyChillerFouling;     // Total number of faulty Chillers with Fouling
    extern int NumFaultyEvapCoolerFouling;  // Total number of faulty Evaporative Coolers with Fouling

    // SUBROUTINE SPECIFICATIONS:

    // Types

    struct FaultProperties // Base Class for operational faults
    {
        // Members
        std::string Name;
        std::string FaultType;        // Fault type
        std::string AvaiSchedule;     // Availability schedule
        std::string SeveritySchedule; // Severity schedule, multipliers to the Offset
        int FaultTypeEnum;
        int AvaiSchedPtr;
        int SeveritySchedPtr;
        Real64 Offset; // offset, + means sensor reading is higher than actual value
        bool Status;   // for future use

        // Default Constructor
        FaultProperties()
            : Name(""), FaultType(""), AvaiSchedule(""), SeveritySchedule(""), FaultTypeEnum(0), AvaiSchedPtr(0), SeveritySchedPtr(0), Offset(0.0),
              Status(false)
        {
        }

        // Virtual Destructor
        virtual ~FaultProperties() = default;

    public:
        Real64 CalFaultOffsetAct();
    };

    struct FaultPropertiesEconomizer : public FaultProperties // Class for fault models related with economizer
    {
        // Members
        int ControllerTypeEnum;
        int ControllerID;           // Point to a controller associated with the fault
        std::string ControllerType; // Controller type
        std::string ControllerName; // Controller name

        // Default Constructor
        FaultPropertiesEconomizer() : ControllerTypeEnum(0), ControllerID(0), ControllerType(""), ControllerName("")
        {
        }

        // Destructor
        virtual ~FaultPropertiesEconomizer() = default;
    };

    struct FaultPropertiesThermostat : public FaultProperties // Class for FaultModel:ThermostatOffset
    {
        // Members
        std::string FaultyThermostatName; // The faulty thermostat name

        // Default Constructor
        FaultPropertiesThermostat() : FaultyThermostatName("")
        {
        }

        // Destructor
        virtual ~FaultPropertiesThermostat() = default;
    };

    struct FaultPropertiesHumidistat : public FaultProperties // Class for FaultModel:HumidistatOffset
    {
        // Members
        std::string FaultyThermostatName; // The faulty thermostat name
        std::string FaultyHumidistatName; // The faulty humidistat name
        std::string FaultyHumidistatType; // The faulty humidistat type

        // Default Constructor
        FaultPropertiesHumidistat() : FaultyThermostatName(""), FaultyHumidistatName(""), FaultyHumidistatType("")
        {
        }

        // Destructor
        virtual ~FaultPropertiesHumidistat() = default;
    };

    struct FaultPropertiesFoulingCoil : public FaultProperties // Class for FaultModel:Fouling:Coil
    {
        // Members
        std::string FouledCoilName; // The fouled coil name
        int FouledCoilID;           // Point to a fouling coil
        int FoulingInputMethod;     // Coil fouling input method
        Real64 UAFouled;            // Fouling coil UA under rating conditions
        Real64 Rfw;                 // Water side fouling factor
        Real64 Rfa;                 // Air side fouling factor
        Real64 Aout;                // Coil outside surface area
        Real64 Aratio;              // Inside to outside surface area ratio

        // Default Constructor
        FaultPropertiesFoulingCoil()
            : FouledCoilName(""), FouledCoilID(0), FoulingInputMethod(0), UAFouled(0.0), Rfw(0.0), Rfa(0.0), Aout(0.0), Aratio(0.0)
        {
        }

        // Destructor
        virtual ~FaultPropertiesFoulingCoil() = default;
    };

    struct FaultPropertiesAirFilter : public FaultProperties // Class for FaultModel:Fouling:AirFilter, derived from FaultProperties
    {
        // Members
        std::string FaultyAirFilterFanName;       // The name of the fan corresponding to the fouled air filter
        std::string FaultyAirFilterFanType;       // The type of the fan corresponding to the fouled air filter
        std::string FaultyAirFilterFanCurve;      // The name of the fan curve
        std::string FaultyAirFilterPressFracSche; // Schedule describing variations of the fan pressure rise
        int FaultyAirFilterFanCurvePtr;           // The index to the curve
        int FaultyAirFilterPressFracSchePtr;      // The pointer to the schedule
        Real64 FaultyAirFilterFanPressInc;        // The increase of the fan pressure due to fouled air filter
        Real64 FaultyAirFilterFanFlowDec;         // The decrease of the fan airflow rate due to fouled air filter

        // Default Constructor
        FaultPropertiesAirFilter()
            : FaultyAirFilterFanName(""), FaultyAirFilterFanType(""), FaultyAirFilterFanCurve(""), FaultyAirFilterPressFracSche(""),
              FaultyAirFilterFanCurvePtr(0), FaultyAirFilterPressFracSchePtr(0), FaultyAirFilterFanPressInc(0.0), FaultyAirFilterFanFlowDec(0.0)
        {
        }

        // Destructor
        virtual ~FaultPropertiesAirFilter() = default;

    public:
        bool CheckFaultyAirFilterFanCurve();
    };

    struct FaultPropertiesCoilSAT : public FaultProperties // Class for FaultModel:TemperatureSensorOffset:CoilSupplyAir
    {
        // Members
        std::string CoilType;                // Coil type
        std::string CoilName;                // Coil name
        std::string WaterCoilControllerName; // Water coil controller name

        // Default Constructor
        FaultPropertiesCoilSAT() : CoilType(""), CoilName(""), WaterCoilControllerName("")
        {
        }
    };

    struct FaultPropertiesChillerSWT : public FaultProperties // Class for FaultModel:TemperatureSensorOffset:ChillerSupplyWater
    {
        // Members
        std::string ChillerType; // Chiller type
        std::string ChillerName; // Chiller name

        // Default Constructor
        FaultPropertiesChillerSWT() : ChillerType(""), ChillerName("")
        {
        }

        // Destructor
        virtual ~FaultPropertiesChillerSWT() = default;

    public:
        void CalFaultChillerSWT(bool FlagConstantFlowChiller,  // True if chiller is constant flow and false if it is variable flow
                                Real64 FaultyChillerSWTOffset, // Faulty chiller SWT sensor offset
                                Real64 Cp,                     // Local fluid specific heat
                                Real64 EvapInletTemp,          // Chiller evaporator inlet water temperature
                                Real64 &EvapOutletTemp,        // Chiller evaporator outlet water temperature
                                Real64 &EvapMassFlowRate,      // Chiller mass flow rate
                                Real64 &QEvaporator            // Chiller evaporator heat transfer rate
        );
    };

    struct FaultPropertiesCondenserSWT : public FaultProperties // Class for FaultModel:TemperatureSensorOffset:CondenserSupplyWater
    {
        // Members
        std::string TowerType; // Tower type
        std::string TowerName; // Tower name

        // Default Constructor
        FaultPropertiesCondenserSWT() : TowerType(""), TowerName("")
        {
        }
    };

    struct FaultPropertiesTowerFouling : public FaultProperties // Class for FaultModel:Fouling:CoolingTower
    {
        // Members
        std::string TowerType;    // Tower type
        std::string TowerName;    // Tower name
        Real64 UAReductionFactor; // UA Reduction Factor

        // Default Constructor
        FaultPropertiesTowerFouling() : TowerType(""), TowerName(""), UAReductionFactor(1.0)
        {
        }

    public:
        Real64 CalFaultyTowerFoulingFactor();
    };

    struct FaultPropertiesFouling : public FaultProperties // Class for FaultModel:Fouling
    {
        // Members
        Real64 FoulingFactor; // Fouling Factor

        // Default Constructor
        FaultPropertiesFouling() : FoulingFactor(1.0)
        {
        }

    public:
        Real64 CalFoulingFactor(); // To calculate the dynamic fouling factor
    };

    struct FaultPropertiesBoilerFouling : public FaultPropertiesFouling // Class for FaultModel:Fouling:Boiler
    {
        // Members
        std::string BoilerType; // Boiler type
        std::string BoilerName; // Boiler name

        // Default Constructor
        FaultPropertiesBoilerFouling() : BoilerType(""), BoilerName("")
        {
        }
    };

    struct FaultPropertiesChillerFouling : public FaultPropertiesFouling // Class for FaultModel:Fouling:Chiller
    {
        // Members
        std::string ChillerType; // Chiller type
        std::string ChillerName; // Chiller name

        // Default Constructor
        FaultPropertiesChillerFouling() : ChillerType(""), ChillerName("")
        {
        }
    };

    struct FaultPropertiesEvapCoolerFouling : public FaultPropertiesFouling // Class for FaultModel:Fouling:EvaporativeCooler
    {
        // Members
        std::string EvapCoolerType; // Evaporative Cooler type
        std::string EvapCoolerName; // Evaporative Cooler name

        // Default Constructor
        FaultPropertiesEvapCoolerFouling() : EvapCoolerType(""), EvapCoolerName("")
        {
        }
    };

    // Object Data
    extern Array1D<FaultPropertiesEconomizer> FaultsEconomizer;
    extern Array1D<FaultPropertiesFoulingCoil> FouledCoils;
    extern Array1D<FaultPropertiesThermostat> FaultsThermostatOffset;
    extern Array1D<FaultPropertiesHumidistat> FaultsHumidistatOffset;
    extern Array1D<FaultPropertiesAirFilter> FaultsFouledAirFilters;
    extern Array1D<FaultPropertiesChillerSWT> FaultsChillerSWTSensor;
    extern Array1D<FaultPropertiesCondenserSWT> FaultsCondenserSWTSensor;
    extern Array1D<FaultPropertiesTowerFouling> FaultsTowerFouling;
    extern Array1D<FaultPropertiesCoilSAT> FaultsCoilSATSensor;
    extern Array1D<FaultPropertiesBoilerFouling> FaultsBoilerFouling;
    extern Array1D<FaultPropertiesChillerFouling> FaultsChillerFouling;
    extern Array1D<FaultPropertiesEvapCoolerFouling> FaultsEvapCoolerFouling;

    // Functions

    void CheckAndReadFaults();

    void clear_state();

    void SetFaultyCoilSATSensor(std::string const &CompType, std::string const &CompName, bool &FaultyCoilSATFlag, int &FaultyCoilSATIndex);

} // namespace FaultsManager

} // namespace EnergyPlus

#endif
