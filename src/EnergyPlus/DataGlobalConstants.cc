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

// C++ Headers
#include <map>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::Constant {

ResourceType AssignResourceTypeNum(std::string const &ResourceTypeChar)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Assists in assigning proper numeric resource types to data structures.

    {
        std::string const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(ResourceTypeChar));

        if (SELECT_CASE_var == "ELECTRICITY") {
            return ResourceType::Electricity;

        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS")) {
            return ResourceType::Natural_Gas;

        } else if (SELECT_CASE_var == "GASOLINE") {
            return ResourceType::Gasoline;

        } else if (SELECT_CASE_var == "DIESEL") {
            return ResourceType::Diesel;

        } else if (SELECT_CASE_var == "COAL") {
            return ResourceType::Coal;

        } else if (SELECT_CASE_var == "FUELOILNO1") {
            return ResourceType::FuelOil_1;

        } else if (SELECT_CASE_var == "FUELOILNO2") {
            return ResourceType::FuelOil_2;

        } else if (SELECT_CASE_var == "PROPANE") {
            return ResourceType::Propane;

        } else if (SELECT_CASE_var == "OTHERFUEL1") {
            return ResourceType::OtherFuel1;

        } else if (SELECT_CASE_var == "OTHERFUEL2") {
            return ResourceType::OtherFuel2;

        } else if ((SELECT_CASE_var == "WATER") || (SELECT_CASE_var == "H2O")) {
            return ResourceType::Water; // use record keeping

        } else if ((SELECT_CASE_var == "ONSITEWATER") || (SELECT_CASE_var == "WATERPRODUCED") || (SELECT_CASE_var == "ONSITE WATER")) {
            return ResourceType::OnSiteWater; // these are for supply record keeping

        } else if ((SELECT_CASE_var == "MAINSWATER") || (SELECT_CASE_var == "WATERSUPPLY")) {
            return ResourceType::MainsWater; // record keeping

        } else if ((SELECT_CASE_var == "RAINWATER") || (SELECT_CASE_var == "PRECIPITATION")) {
            return ResourceType::RainWater; // record keeping

        } else if ((SELECT_CASE_var == "WELLWATER") || (SELECT_CASE_var == "Groundwater")) {
            return ResourceType::WellWater; // record keeping

        } else if (SELECT_CASE_var == "CONDENSATE") {
            return ResourceType::Condensate;

        } else if (SELECT_CASE_var == "ENERGYTRANSFER") {
            return ResourceType::EnergyTransfer;

        } else if (SELECT_CASE_var == "STEAM") {
            return ResourceType::Steam;

        } else if (SELECT_CASE_var == "DISTRICTCOOLING") {
            return ResourceType::DistrictCooling;

        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
            return ResourceType::DistrictHeating;

        } else if (SELECT_CASE_var == "ELECTRICITYPRODUCED") {
            return ResourceType::ElectricityProduced;

        } else if (SELECT_CASE_var == "ELECTRICITYPURCHASED") {
            return ResourceType::ElectricityPurchased;

        } else if (SELECT_CASE_var == "ELECTRICITYSURPLUSSOLD") {
            return ResourceType::ElectricitySurplusSold;

        } else if (SELECT_CASE_var == "ELECTRICITYNET") {
            return ResourceType::ElectricityNet;

        } else if (SELECT_CASE_var == "SOLARWATER") {
            return ResourceType::SolarWater;

        } else if (SELECT_CASE_var == "SOLARAIR") {
            return ResourceType::SolarAir;

        } else if (SELECT_CASE_var == "SO2") {
            return ResourceType::SO2;

        } else if (SELECT_CASE_var == "NOX") {
            return ResourceType::NOx;

        } else if (SELECT_CASE_var == "N2O") {
            return ResourceType::N2O;

        } else if (SELECT_CASE_var == "PM") {
            return ResourceType::PM;

        } else if (SELECT_CASE_var == "PM2.5") {
            return ResourceType::PM2_5;

        } else if (SELECT_CASE_var == "PM10") {
            return ResourceType::PM10;

        } else if (SELECT_CASE_var == "CO") {
            return ResourceType::CO;

        } else if (SELECT_CASE_var == "CO2") {
            return ResourceType::CO2;

        } else if (SELECT_CASE_var == "CH4") {
            return ResourceType::CH4;

        } else if (SELECT_CASE_var == "NH3") {
            return ResourceType::NH3;

        } else if (SELECT_CASE_var == "NMVOC") {
            return ResourceType::NMVOC;

        } else if (SELECT_CASE_var == "HG") {
            return ResourceType::Hg;

        } else if (SELECT_CASE_var == "PB") {
            return ResourceType::Pb;

        } else if (SELECT_CASE_var == "NUCLEAR HIGH") {
            return ResourceType::NuclearHigh;

        } else if (SELECT_CASE_var == "NUCLEAR LOW") {
            return ResourceType::NuclearLow;

        } else if (SELECT_CASE_var == "WATERENVIRONMENTALFACTORS") {
            return ResourceType::WaterEnvironmentalFactors;

        } else if (SELECT_CASE_var == "CARBON EQUIVALENT") {
            return ResourceType::CarbonEquivalent;

        } else if (SELECT_CASE_var == "SOURCE") {
            return ResourceType::Source;

        } else if (SELECT_CASE_var == "PLANTLOOPHEATINGDEMAND") {
            return ResourceType::PlantLoopHeatingDemand;

        } else if (SELECT_CASE_var == "PLANTLOOPCOOLINGDEMAND") {
            return ResourceType::PlantLoopCoolingDemand;

        } else {
            return ResourceType::None;
        }
    }
}

std::string GetResourceTypeChar(ResourceType const ResourceTypeNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Shows the resource type character string, given the resource type numeric.

    switch (ResourceTypeNum) {
    case ResourceType::Electricity: {
        return "Electricity";
    } break;
    case ResourceType::Natural_Gas: {
        return "NaturalGas";
    } break;
    case ResourceType::Gasoline: {
        return "Gasoline";
    } break;
    case ResourceType::Diesel: {
        return "Diesel";
    } break;
    case ResourceType::Coal: {
        return "Coal";
    } break;
    case ResourceType::FuelOil_1: {
        return "FuelOilNo1";
    } break;
    case ResourceType::FuelOil_2: {
        return "FuelOilNo2";
    } break;
    case ResourceType::Propane: {
        return "Propane";
    } break;
    case ResourceType::OtherFuel1: {
        return "OtherFuel1";
    } break;
    case ResourceType::OtherFuel2: {
        return "OtherFuel2";
    } break;
    case ResourceType::Water: {
        return "Water";
    } break;
    case ResourceType::OnSiteWater: {
        return "OnSiteWater";
    } break;
    case ResourceType::MainsWater: {
        return "MainsWater";
    } break;
    case ResourceType::RainWater: {
        return "RainWater";
    } break;
    case ResourceType::Condensate: {
        return "Condensate";
    } break;
    case ResourceType::WellWater: {
        return "WellWater";
    } break;
    case ResourceType::EnergyTransfer: {
        return "EnergyTransfer";
    } break;
    case ResourceType::Steam: {
        return "Steam";
    } break;
    case ResourceType::DistrictCooling: {
        return "DistrictCooling";
    } break;
    case ResourceType::DistrictHeating: {
        return "DistrictHeating";
    } break;
    case ResourceType::ElectricityProduced: {
        return "ElectricityProduced";
    } break;
    case ResourceType::ElectricityPurchased: {
        return "ElectricityPurchased";
    } break;
    case ResourceType::ElectricitySurplusSold: {
        return "ElectricitySurplusSold";
    } break;
    case ResourceType::ElectricityNet: {
        return "ElectricityNet";
    } break;
    case ResourceType::SolarWater: {
        return "SolarWater";
    } break;
    case ResourceType::SolarAir: {
        return "SolarAir";
    } break;
    case ResourceType::SO2: {
        return "SO2";
    } break;
    case ResourceType::NOx: {
        return "NOx";
    } break;
    case ResourceType::N2O: {
        return "N2O";
    } break;
    case ResourceType::PM: {
        return "PM";
    } break;
    case ResourceType::PM2_5: {
        return "PM2.5";
    } break;
    case ResourceType::PM10: {
        return "PM10";
    } break;
    case ResourceType::CO: {
        return "CO";
    } break;
    case ResourceType::CO2: {
        return "CO2";
    } break;
    case ResourceType::CH4: {
        return "CH4";
    } break;
    case ResourceType::NH3: {
        return "NH3";
    } break;
    case ResourceType::NMVOC: {
        return "NMVOC";
    } break;
    case ResourceType::Hg: {
        return "Hg";
    } break;
    case ResourceType::Pb: {
        return "Pb";
    } break;
    case ResourceType::NuclearHigh: {
        return "Nuclear High";
    } break;
    case ResourceType::NuclearLow: {
        return "Nuclear Low";
    } break;
    case ResourceType::WaterEnvironmentalFactors: {
        return "WaterEnvironmentalFactors";
    } break;
    case ResourceType::CarbonEquivalent: {
        return "Carbon Equivalent";
    } break;
    case ResourceType::Source: {
        return "Source";
    } break;
    case ResourceType::PlantLoopHeatingDemand: {
        return "PlantLoopHeatingDemand";
    } break;
    case ResourceType::PlantLoopCoolingDemand: {
        return "PlantLoopCoolingDemand";
    } break;
    default: {
        return "Unknown";
    } break;
    }
}

} // namespace EnergyPlus::Constant
