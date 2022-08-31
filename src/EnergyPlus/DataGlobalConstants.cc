// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

namespace EnergyPlus::DataGlobalConstants {

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
    case ResourceType::NaturalGas: {
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
    case ResourceType::FuelOilNo1: {
        return "FuelOilNo1";
    } break;
    case ResourceType::FuelOilNo2: {
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

} // namespace EnergyPlus::DataGlobalConstants
