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

#ifndef ExteriorEnergyUse_hh_INCLUDED
#define ExteriorEnergyUse_hh_INCLUDED

#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ExteriorEnergyUse {

    enum class ExteriorFuelUsage {
        Unknown = 0,
        ElecUse = 1,
        GasUse = 2,
        WaterUse = 3,
        CoalUse = 4,
        FuelOil1Use = 5,
        FuelOil2Use = 6,
        PropaneUse = 7,
        GasolineUse = 8,
        DieselUse = 9,
        SteamUse = 10,
        DistrictCoolUse = 11,
        DistrictHeatUse = 12,
        OtherFuel1Use = 13,
        OtherFuel2Use = 14
    };

    enum class LightControlType {
        ScheduleOnly = 1,      // exterior lights only on schedule
        AstroClockOverride = 2 // exterior lights controlled to turn off during day.
    };

    struct ExteriorLightUsage
    {
        // Members
        std::string Name;          // Descriptive name -- will show on reporting
        int SchedPtr;              // Can be scheduled
        Real64 DesignLevel;        // Consumption in Watts
        Real64 Power;              // Power = DesignLevel * ScheduleValue
        Real64 CurrentUse;         // Use for this time step
        LightControlType ControlMode;           // Control mode Schedule Only or Astronomical Clock plus schedule
        bool ManageDemand;         // Flag to indicate whether to use demand limiting
        Real64 DemandLimit;        // Demand limit set by demand manager [W]
        bool PowerActuatorOn;      // EMS flag
        Real64 PowerActuatorValue; // EMS value
        Real64 SumConsumption;     // sum of electric consumption [J] for reporting
        Real64 SumTimeNotZeroCons; // sum of time of positive electric consumption [hr]

        // Default Constructor
        ExteriorLightUsage()
                : SchedPtr(0), DesignLevel(0.0), Power(0.0), CurrentUse(0.0), ControlMode(LightControlType::ScheduleOnly), ManageDemand(false), DemandLimit(0.0),
                  PowerActuatorOn(false), PowerActuatorValue(0.0), SumConsumption(0.0), SumTimeNotZeroCons(0.0)
        {
        }
    };

    struct ExteriorEquipmentUsage
    {
        // Members
        std::string Name; // Descriptive name -- will show on reporting
        ExteriorFuelUsage FuelType;
        int SchedPtr;       // Can be scheduled
        Real64 DesignLevel; // Design Consumption (Watts, except for Water Equipment)
        Real64 Power;       // Power = DesignLevel * ScheduleValue
        Real64 CurrentUse;  // Use for this time step
        bool ManageDemand;  // Flag to indicate whether to use demand limiting
        Real64 DemandLimit; // Demand limit set by demand manager [W]

        // Default Constructor
        ExteriorEquipmentUsage() : FuelType(ExteriorFuelUsage::Unknown), SchedPtr(0), DesignLevel(0.0), Power(0.0), CurrentUse(0.0), ManageDemand(false), DemandLimit(0.0)
        {
        }
    };

    void ManageExteriorEnergyUse(EnergyPlusData &state);

    void GetExteriorEnergyUseInput(EnergyPlusData &state);

    void ValidateFuelType(EnergyPlusData &state,
                          ExteriorEnergyUse::ExteriorFuelUsage &FuelTypeNumber,                    // Fuel Type to be set in structure.
                          std::string const &FuelTypeAlpha,       // Fuel Type String
                          std::string &FuelTypeString,            // Standardized Fuel Type String (for variable naming)
                          std::string const &CurrentModuleObject, // object being parsed
                          std::string const &CurrentField,        // current field being parsed
                          std::string const &CurrentName          // current object name being parsed
    );

    void ReportExteriorEnergyUse(EnergyPlusData &state);

} // namespace ExteriorEnergyUse

    struct ExteriorEnergyUseData : BaseGlobalStruct {

        int NumExteriorLights; // Number of Exterior Light Inputs
        int NumExteriorEqs;    // Number of Exterior Equipment Inputs
        Array1D<ExteriorEnergyUse::ExteriorLightUsage> ExteriorLights;        // Structure for Exterior Light reporting
        Array1D<ExteriorEnergyUse::ExteriorEquipmentUsage> ExteriorEquipment; // Structure for Exterior Equipment Reporting
        std::unordered_map<std::string, std::string> UniqueExteriorEquipNames;
        bool GetExteriorEnergyInputFlag; // First time, input is "gotten"

        void clear_state() override {
            NumExteriorLights = 0;
            NumExteriorEqs = 0;
            ExteriorLights.deallocate();
            ExteriorEquipment.deallocate();
            UniqueExteriorEquipNames.clear();
            GetExteriorEnergyInputFlag = true;
        }
        // Default Constructor
        ExteriorEnergyUseData() : NumExteriorLights(0), NumExteriorEqs(0), GetExteriorEnergyInputFlag(true){}
    };

} // namespace EnergyPlus

#endif
