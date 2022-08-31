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

// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, DataGlobalConstants_AssignResourceTypeNum)
{

    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Electricity,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Electricity")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::NaturalGas,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("NaturalGas")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Gasoline,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Gasoline")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Diesel,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Diesel")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Coal,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Coal")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::FuelOilNo1,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("FuelOilNo1")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::FuelOilNo2,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("FuelOilNo2")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Propane,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Propane")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::OtherFuel1,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("OtherFuel1")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::OtherFuel2,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("OtherFuel2")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Water,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Water")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::OnSiteWater,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("OnSiteWater")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::MainsWater,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("MainsWater")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::RainWater,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("RainWater")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::WellWater,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("WellWater")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Condensate,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Condensate")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::EnergyTransfer,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("EnergyTransfer")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Steam,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Steam")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::DistrictCooling,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("DistrictCooling")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::DistrictHeating,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("DistrictHeating")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::ElectricityProduced,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("ElectricityProduced")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::ElectricityPurchased,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("ElectricityPurchased")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::ElectricitySurplusSold,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("ElectricitySurplusSold")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::ElectricityNet,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("ElectricityNet")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::SolarWater,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("SolarWater")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::SolarAir,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("SolarAir")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::SO2,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("SO2")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::NOx,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("NOx")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::N2O,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("N2O")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::PM,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("PM")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::PM2_5,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("PM2.5")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::PM10,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("PM10")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::CO,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("CO")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::CO2,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("CO2")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::CH4,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("CH4")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::NH3,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("NH3")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::NMVOC,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("NMVOC")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Hg,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Hg")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Pb,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Pb")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::NuclearHigh,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Nuclear High")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::NuclearLow,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Nuclear Low")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::WaterEnvironmentalFactors,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("WaterEnvironmentalFactors")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::CarbonEquivalent,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("Carbon Equivalent")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::PlantLoopHeatingDemand,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("PlantLoopHeatingDemand")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::PlantLoopCoolingDemand,
                              static_cast<DataGlobalConstants::ResourceType>(getEnumerationValue(
                                  DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("PlantLoopCoolingDemand")))));
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Invalid,
                              static_cast<DataGlobalConstants::ResourceType>(
                                  getEnumerationValue(DataGlobalConstants::ResourceTypeNamesUC, UtilityRoutines::MakeUPPERCase("XYZ")))));
}

TEST_F(EnergyPlusFixture, DataGlobalConstants_GetResourceTypeChar)
{

    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Electricity), "Electricity");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::NaturalGas), "NaturalGas");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Gasoline), "Gasoline");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Diesel), "Diesel");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Coal), "Coal");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::FuelOilNo1), "FuelOilNo1");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::FuelOilNo2), "FuelOilNo2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Propane), "Propane");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::OtherFuel1), "OtherFuel1");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::OtherFuel2), "OtherFuel2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Water), "Water");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::OnSiteWater), "OnSiteWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::MainsWater), "MainsWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::RainWater), "RainWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::WellWater), "WellWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Condensate), "Condensate");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::EnergyTransfer), "EnergyTransfer");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Steam), "Steam");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::DistrictCooling), "DistrictCooling");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::DistrictHeating), "DistrictHeating");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::ElectricityProduced), "ElectricityProduced");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::ElectricityPurchased), "ElectricityPurchased");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::ElectricitySurplusSold), "ElectricitySurplusSold");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::ElectricityNet), "ElectricityNet");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::SolarWater), "SolarWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::SolarAir), "SolarAir");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::SO2), "SO2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::NOx), "NOx");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::N2O), "N2O");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::PM), "PM");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::PM2_5), "PM2.5");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::PM10), "PM10");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::CO), "CO");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::CO2), "CO2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::CH4), "CH4");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::NH3), "NH3");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::NMVOC), "NMVOC");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Hg), "Hg");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Pb), "Pb");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::NuclearHigh), "Nuclear High");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::NuclearLow), "Nuclear Low");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::WaterEnvironmentalFactors), "WaterEnvironmentalFactors");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::CarbonEquivalent), "Carbon Equivalent");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::PlantLoopHeatingDemand), "PlantLoopHeatingDemand");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::PlantLoopCoolingDemand), "PlantLoopCoolingDemand");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Invalid), "Unknown");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Invalid), "Unknown");
}
