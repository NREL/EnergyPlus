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

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataGlobalConstants {

    // Module containing the data constants for components, meters, etc throughout
    // EnergyPlus

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Provide a central storage place for various constants and their "integer equivalents"
    // used throughout EnergyPlus.  Integer equivalents are needed for efficiency in run time.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // OTHER NOTES:
    // na

    // USE STATEMENTS:
    // na

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // End Use Parameters
    int const NumEndUses(14);

    int const endUseHeating(1);
    int const endUseCooling(2);
    int const endUseInteriorLights(3);
    int const endUseExteriorLights(4);
    int const endUseInteriorEquipment(5);
    int const endUseExteriorEquipment(6);
    int const endUseFans(7);
    int const endUsePumps(8);
    int const endUseHeatRejection(9);
    int const endUseHumidification(10);
    int const endUseHeatRecovery(11);
    int const endUseWaterSystem(12);
    int const endUseRefrigeration(13);
    int const endUseCogeneration(14);

    // Resource Types
    int const iRT_None(1000);
    int const iRT_Electricity(1001);
    int const iRT_Natural_Gas(1002);
    int const iRT_Gasoline(1003);
    int const iRT_Diesel(1004);
    int const iRT_Coal(1005);
    int const iRT_FuelOil_1(1006);
    int const iRT_FuelOil_2(1007);
    int const iRT_Propane(1008);
    int const iRT_Water(1009);
    int const iRT_EnergyTransfer(1010);
    int const iRT_Steam(1011);
    int const iRT_DistrictCooling(1012);
    int const iRT_DistrictHeating(1013);
    int const iRT_ElectricityProduced(1014);
    int const iRT_ElectricityPurchased(1015);
    int const iRT_ElectricitySurplusSold(1016);
    int const iRT_ElectricityNet(1017);
    int const iRT_SolarWater(1018);
    int const iRT_SolarAir(1019);
    int const iRT_SO2(1020);
    int const iRT_NOx(1021);
    int const iRT_N2O(1022);
    int const iRT_PM(1023);
    int const iRT_PM2_5(1024);
    int const iRT_PM10(1025);
    int const iRT_CO(1026);
    int const iRT_CO2(1027);
    int const iRT_CH4(1028);
    int const iRT_NH3(1029);
    int const iRT_NMVOC(1030);
    int const iRT_Hg(1031);
    int const iRT_Pb(1032);
    int const iRT_NuclearHigh(1033);
    int const iRT_NuclearLow(1034);
    int const iRT_WaterEnvironmentalFactors(1035);
    int const iRT_CarbonEquivalent(1036);
    int const iRT_Source(1037);
    int const iRT_PlantLoopHeatingDemand(1038);
    int const iRT_PlantLoopCoolingDemand(1039);
    int const iRT_OnSiteWater(1040);
    int const iRT_MainsWater(1041);
    int const iRT_RainWater(1042);
    int const iRT_WellWater(1043);
    int const iRT_Condensate(1044);
    int const iRT_OtherFuel1(1045);
    int const iRT_OtherFuel2(1046);
    int const NumOfResourceTypes(46);
    int const ResourceTypeInitialOffset(1000); // to reach "ValidTypes"

    int AssignResourceTypeNum(std::string const &ResourceTypeChar)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   June 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Assists in assigning proper numeric resource types to data structures.

        // Return value
        int ResourceTypeNum;

        ResourceTypeNum = 0;

        {
            auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(ResourceTypeChar));

            if (SELECT_CASE_var == "ELECTRICITY") {
                ResourceTypeNum = iRT_Electricity;

            } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS")) {
                ResourceTypeNum = iRT_Natural_Gas;

            } else if (SELECT_CASE_var == "GASOLINE") {
                ResourceTypeNum = iRT_Gasoline;

            } else if (SELECT_CASE_var == "DIESEL") {
                ResourceTypeNum = iRT_Diesel;

            } else if (SELECT_CASE_var == "COAL") {
                ResourceTypeNum = iRT_Coal;

            } else if ((SELECT_CASE_var == "FUELOILNO1") || (SELECT_CASE_var == "FuelOilNo1")) {
                ResourceTypeNum = iRT_FuelOil_1;

            } else if ((SELECT_CASE_var == "FUELOILNO2") || (SELECT_CASE_var == "FuelOilNo2")) {
                ResourceTypeNum = iRT_FuelOil_2;

            } else if (SELECT_CASE_var == "PROPANE") {
                ResourceTypeNum = iRT_Propane;

            } else if (SELECT_CASE_var == "OTHERFUEL1") {
                ResourceTypeNum = iRT_OtherFuel1;

            } else if (SELECT_CASE_var == "OTHERFUEL2") {
                ResourceTypeNum = iRT_OtherFuel2;

            } else if ((SELECT_CASE_var == "WATER") || (SELECT_CASE_var == "H2O")) {
                ResourceTypeNum = iRT_Water; // use record keeping

            } else if ((SELECT_CASE_var == "ONSITEWATER") || (SELECT_CASE_var == "WATERPRODUCED") || (SELECT_CASE_var == "ONSITE WATER")) {
                ResourceTypeNum = iRT_OnSiteWater; // these are for supply record keeping

            } else if ((SELECT_CASE_var == "MAINSWATER") || (SELECT_CASE_var == "WATERSUPPLY")) {
                ResourceTypeNum = iRT_MainsWater; // record keeping

            } else if ((SELECT_CASE_var == "RAINWATER") || (SELECT_CASE_var == "PRECIPITATION")) {
                ResourceTypeNum = iRT_RainWater; // record keeping

            } else if ((SELECT_CASE_var == "WELLWATER") || (SELECT_CASE_var == "Groundwater")) {
                ResourceTypeNum = iRT_WellWater; // record keeping

            } else if (SELECT_CASE_var == "CONDENSATE") {
                ResourceTypeNum = iRT_Condensate;

            } else if (SELECT_CASE_var == "ENERGYTRANSFER") {
                ResourceTypeNum = iRT_EnergyTransfer;

            } else if (SELECT_CASE_var == "STEAM") {
                ResourceTypeNum = iRT_Steam;

            } else if (SELECT_CASE_var == "DISTRICTCOOLING") {
                ResourceTypeNum = iRT_DistrictCooling;

            } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                ResourceTypeNum = iRT_DistrictHeating;

            } else if (SELECT_CASE_var == "ELECTRICITYPRODUCED") {
                ResourceTypeNum = iRT_ElectricityProduced;

            } else if (SELECT_CASE_var == "ELECTRICITYPURCHASED") {
                ResourceTypeNum = iRT_ElectricityPurchased;

            } else if (SELECT_CASE_var == "ELECTRICITYSURPLUSSOLD") {
                ResourceTypeNum = iRT_ElectricitySurplusSold;

            } else if (SELECT_CASE_var == "ELECTRICITYNET") {
                ResourceTypeNum = iRT_ElectricityNet;

            } else if (SELECT_CASE_var == "SOLARWATER") {
                ResourceTypeNum = iRT_SolarWater;

            } else if (SELECT_CASE_var == "SOLARAIR") {
                ResourceTypeNum = iRT_SolarAir;

            } else if (SELECT_CASE_var == "SO2") {
                ResourceTypeNum = iRT_SO2;

            } else if (SELECT_CASE_var == "NOX") {
                ResourceTypeNum = iRT_NOx;

            } else if (SELECT_CASE_var == "N2O") {
                ResourceTypeNum = iRT_N2O;

            } else if (SELECT_CASE_var == "PM") {
                ResourceTypeNum = iRT_PM;

            } else if (SELECT_CASE_var == "PM2.5") {
                ResourceTypeNum = iRT_PM2_5;

            } else if (SELECT_CASE_var == "PM10") {
                ResourceTypeNum = iRT_PM10;

            } else if (SELECT_CASE_var == "CO") {
                ResourceTypeNum = iRT_CO;

            } else if (SELECT_CASE_var == "CO2") {
                ResourceTypeNum = iRT_CO2;

            } else if (SELECT_CASE_var == "CH4") {
                ResourceTypeNum = iRT_CH4;

            } else if (SELECT_CASE_var == "NH3") {
                ResourceTypeNum = iRT_NH3;

            } else if (SELECT_CASE_var == "NMVOC") {
                ResourceTypeNum = iRT_NMVOC;

            } else if (SELECT_CASE_var == "HG") {
                ResourceTypeNum = iRT_Hg;

            } else if (SELECT_CASE_var == "PB") {
                ResourceTypeNum = iRT_Pb;

            } else if (SELECT_CASE_var == "NUCLEAR HIGH") {
                ResourceTypeNum = iRT_NuclearHigh;

            } else if (SELECT_CASE_var == "NUCLEAR LOW") {
                ResourceTypeNum = iRT_NuclearLow;

            } else if (SELECT_CASE_var == "WATERENVIRONMENTALFACTORS") {
                ResourceTypeNum = iRT_WaterEnvironmentalFactors;

            } else if (SELECT_CASE_var == "CARBON EQUIVALENT") {
                ResourceTypeNum = iRT_CarbonEquivalent;

            } else if (SELECT_CASE_var == "SOURCE") {
                ResourceTypeNum = iRT_Source;

            } else if (SELECT_CASE_var == "PLANTLOOPHEATINGDEMAND") {
                ResourceTypeNum = iRT_PlantLoopHeatingDemand;

            } else if (SELECT_CASE_var == "PLANTLOOPCOOLINGDEMAND") {
                ResourceTypeNum = iRT_PlantLoopCoolingDemand;

            } else {
                ResourceTypeNum = 0;
            }
        }

        return ResourceTypeNum;
    }

    std::string GetResourceTypeChar(int const ResourceTypeNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   June 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Shows the resource type character string, given the resource type numeric.

        // Return value
        std::string ResourceTypeChar;

        {
            auto const SELECT_CASE_var(ResourceTypeNum);

            if (SELECT_CASE_var == iRT_Electricity) {
                ResourceTypeChar = "Electricity";

            } else if (SELECT_CASE_var == iRT_Natural_Gas) {
                ResourceTypeChar = "NaturalGas";

            } else if (SELECT_CASE_var == iRT_Gasoline) {
                ResourceTypeChar = "Gasoline";

            } else if (SELECT_CASE_var == iRT_Diesel) {
                ResourceTypeChar = "Diesel";

            } else if (SELECT_CASE_var == iRT_Coal) {
                ResourceTypeChar = "Coal";

            } else if (SELECT_CASE_var == iRT_FuelOil_1) {
                ResourceTypeChar = "FuelOilNo1";

            } else if (SELECT_CASE_var == iRT_FuelOil_2) {
                ResourceTypeChar = "FuelOilNo2";

            } else if (SELECT_CASE_var == iRT_Propane) {
                ResourceTypeChar = "Propane";

            } else if (SELECT_CASE_var == iRT_OtherFuel1) {
                ResourceTypeChar = "OtherFuel1";

            } else if (SELECT_CASE_var == iRT_OtherFuel2) {
                ResourceTypeChar = "OtherFuel2";

            } else if (SELECT_CASE_var == iRT_Water) {
                ResourceTypeChar = "Water";

            } else if (SELECT_CASE_var == iRT_OnSiteWater) {
                ResourceTypeChar = "OnSiteWater";

            } else if (SELECT_CASE_var == iRT_MainsWater) {
                ResourceTypeChar = "MainsWater";

            } else if (SELECT_CASE_var == iRT_RainWater) {
                ResourceTypeChar = "RainWater";

            } else if (SELECT_CASE_var == iRT_Condensate) {
                ResourceTypeChar = "Condensate";

            } else if (SELECT_CASE_var == iRT_WellWater) {
                ResourceTypeChar = "WellWater";

            } else if (SELECT_CASE_var == iRT_EnergyTransfer) {
                ResourceTypeChar = "EnergyTransfer";

            } else if (SELECT_CASE_var == iRT_Steam) {
                ResourceTypeChar = "Steam";

            } else if (SELECT_CASE_var == iRT_DistrictCooling) {
                ResourceTypeChar = "DistrictCooling";

            } else if (SELECT_CASE_var == iRT_DistrictHeating) {
                ResourceTypeChar = "DistrictHeating";

            } else if (SELECT_CASE_var == iRT_ElectricityProduced) {
                ResourceTypeChar = "ElectricityProduced";

            } else if (SELECT_CASE_var == iRT_ElectricityPurchased) {
                ResourceTypeChar = "ElectricityPurchased";

            } else if (SELECT_CASE_var == iRT_ElectricitySurplusSold) {
                ResourceTypeChar = "ElectricitySurplusSold";

            } else if (SELECT_CASE_var == iRT_ElectricityNet) {
                ResourceTypeChar = "ElectricityNet";

            } else if (SELECT_CASE_var == iRT_SolarWater) {
                ResourceTypeChar = "SolarWater";

            } else if (SELECT_CASE_var == iRT_SolarAir) {
                ResourceTypeChar = "SolarAir";

            } else if (SELECT_CASE_var == iRT_SO2) {
                ResourceTypeChar = "SO2";

            } else if (SELECT_CASE_var == iRT_NOx) {
                ResourceTypeChar = "NOx";

            } else if (SELECT_CASE_var == iRT_N2O) {
                ResourceTypeChar = "N2O";

            } else if (SELECT_CASE_var == iRT_PM) {
                ResourceTypeChar = "PM";

            } else if (SELECT_CASE_var == iRT_PM2_5) {
                ResourceTypeChar = "PM2.5";

            } else if (SELECT_CASE_var == iRT_PM10) {
                ResourceTypeChar = "PM10";

            } else if (SELECT_CASE_var == iRT_CO) {
                ResourceTypeChar = "CO";

            } else if (SELECT_CASE_var == iRT_CO2) {
                ResourceTypeChar = "CO2";

            } else if (SELECT_CASE_var == iRT_CH4) {
                ResourceTypeChar = "CH4";

            } else if (SELECT_CASE_var == iRT_NH3) {
                ResourceTypeChar = "NH3";

            } else if (SELECT_CASE_var == iRT_NMVOC) {
                ResourceTypeChar = "NMVOC";

            } else if (SELECT_CASE_var == iRT_Hg) {
                ResourceTypeChar = "Hg";

            } else if (SELECT_CASE_var == iRT_Pb) {
                ResourceTypeChar = "Pb";

            } else if (SELECT_CASE_var == iRT_NuclearHigh) {
                ResourceTypeChar = "Nuclear High";

            } else if (SELECT_CASE_var == iRT_NuclearLow) {
                ResourceTypeChar = "Nuclear Low";

            } else if (SELECT_CASE_var == iRT_WaterEnvironmentalFactors) {
                ResourceTypeChar = "WaterEnvironmentalFactors";

            } else if (SELECT_CASE_var == iRT_CarbonEquivalent) {
                ResourceTypeChar = "Carbon Equivalent";

            } else if (SELECT_CASE_var == iRT_Source) {
                ResourceTypeChar = "Source";

            } else if (SELECT_CASE_var == iRT_PlantLoopHeatingDemand) {
                ResourceTypeChar = "PlantLoopHeatingDemand";

            } else if (SELECT_CASE_var == iRT_PlantLoopCoolingDemand) {
                ResourceTypeChar = "PlantLoopCoolingDemand";

            } else {
                ResourceTypeChar = "Unknown";
            }
        }

        return ResourceTypeChar;
    }

} // namespace DataGlobalConstants

} // namespace EnergyPlus
