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
#include <map>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::DataGlobalConstants {

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
        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(ResourceTypeChar));

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

    {
        auto const SELECT_CASE_var(ResourceTypeNum);

        if (SELECT_CASE_var == ResourceType::Electricity) {
            return "Electricity";

        } else if (SELECT_CASE_var == ResourceType::Natural_Gas) {
            return "NaturalGas";

        } else if (SELECT_CASE_var == ResourceType::Gasoline) {
            return "Gasoline";

        } else if (SELECT_CASE_var == ResourceType::Diesel) {
            return "Diesel";

        } else if (SELECT_CASE_var == ResourceType::Coal) {
            return "Coal";

        } else if (SELECT_CASE_var == ResourceType::FuelOil_1) {
            return "FuelOilNo1";

        } else if (SELECT_CASE_var == ResourceType::FuelOil_2) {
            return "FuelOilNo2";

        } else if (SELECT_CASE_var == ResourceType::Propane) {
            return "Propane";

        } else if (SELECT_CASE_var == ResourceType::OtherFuel1) {
            return "OtherFuel1";

        } else if (SELECT_CASE_var == ResourceType::OtherFuel2) {
            return "OtherFuel2";

        } else if (SELECT_CASE_var == ResourceType::Water) {
            return "Water";

        } else if (SELECT_CASE_var == ResourceType::OnSiteWater) {
            return "OnSiteWater";

        } else if (SELECT_CASE_var == ResourceType::MainsWater) {
            return "MainsWater";

        } else if (SELECT_CASE_var == ResourceType::RainWater) {
            return "RainWater";

        } else if (SELECT_CASE_var == ResourceType::Condensate) {
            return "Condensate";

        } else if (SELECT_CASE_var == ResourceType::WellWater) {
            return "WellWater";

        } else if (SELECT_CASE_var == ResourceType::EnergyTransfer) {
            return "EnergyTransfer";

        } else if (SELECT_CASE_var == ResourceType::Steam) {
            return "Steam";

        } else if (SELECT_CASE_var == ResourceType::DistrictCooling) {
            return "DistrictCooling";

        } else if (SELECT_CASE_var == ResourceType::DistrictHeating) {
            return "DistrictHeating";

        } else if (SELECT_CASE_var == ResourceType::ElectricityProduced) {
            return "ElectricityProduced";

        } else if (SELECT_CASE_var == ResourceType::ElectricityPurchased) {
            return "ElectricityPurchased";

        } else if (SELECT_CASE_var == ResourceType::ElectricitySurplusSold) {
            return "ElectricitySurplusSold";

        } else if (SELECT_CASE_var == ResourceType::ElectricityNet) {
            return "ElectricityNet";

        } else if (SELECT_CASE_var == ResourceType::SolarWater) {
            return "SolarWater";

        } else if (SELECT_CASE_var == ResourceType::SolarAir) {
            return "SolarAir";

        } else if (SELECT_CASE_var == ResourceType::SO2) {
            return "SO2";

        } else if (SELECT_CASE_var == ResourceType::NOx) {
            return "NOx";

        } else if (SELECT_CASE_var == ResourceType::N2O) {
            return "N2O";

        } else if (SELECT_CASE_var == ResourceType::PM) {
            return "PM";

        } else if (SELECT_CASE_var == ResourceType::PM2_5) {
            return "PM2.5";

        } else if (SELECT_CASE_var == ResourceType::PM10) {
            return "PM10";

        } else if (SELECT_CASE_var == ResourceType::CO) {
            return "CO";

        } else if (SELECT_CASE_var == ResourceType::CO2) {
            return "CO2";

        } else if (SELECT_CASE_var == ResourceType::CH4) {
            return "CH4";

        } else if (SELECT_CASE_var == ResourceType::NH3) {
            return "NH3";

        } else if (SELECT_CASE_var == ResourceType::NMVOC) {
            return "NMVOC";

        } else if (SELECT_CASE_var == ResourceType::Hg) {
            return "Hg";

        } else if (SELECT_CASE_var == ResourceType::Pb) {
            return "Pb";

        } else if (SELECT_CASE_var == ResourceType::NuclearHigh) {
            return "Nuclear High";

        } else if (SELECT_CASE_var == ResourceType::NuclearLow) {
            return "Nuclear Low";

        } else if (SELECT_CASE_var == ResourceType::WaterEnvironmentalFactors) {
            return "WaterEnvironmentalFactors";

        } else if (SELECT_CASE_var == ResourceType::CarbonEquivalent) {
            return "Carbon Equivalent";

        } else if (SELECT_CASE_var == ResourceType::Source) {
            return "Source";

        } else if (SELECT_CASE_var == ResourceType::PlantLoopHeatingDemand) {
            return "PlantLoopHeatingDemand";

        } else if (SELECT_CASE_var == ResourceType::PlantLoopCoolingDemand) {
            return "PlantLoopCoolingDemand";

        } else {
            return "Unknown";
        }
    }
}

} // namespace EnergyPlus::DataGlobalConstants
