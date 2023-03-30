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

// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, Constant_AssignResourceTypeNum)
{

    EXPECT_TRUE(compare_enums(Constant::ResourceType::Electricity, Constant::AssignResourceTypeNum("Electricity")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Natural_Gas, Constant::AssignResourceTypeNum("NaturalGas")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Gasoline, Constant::AssignResourceTypeNum("Gasoline")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Diesel, Constant::AssignResourceTypeNum("Diesel")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Coal, Constant::AssignResourceTypeNum("Coal")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::FuelOil_1, Constant::AssignResourceTypeNum("FuelOilNo1")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::FuelOil_2, Constant::AssignResourceTypeNum("FuelOilNo2")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Propane, Constant::AssignResourceTypeNum("Propane")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::OtherFuel1, Constant::AssignResourceTypeNum("OtherFuel1")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::OtherFuel2, Constant::AssignResourceTypeNum("OtherFuel2")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Water, Constant::AssignResourceTypeNum("Water")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::OnSiteWater, Constant::AssignResourceTypeNum("OnSiteWater")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::MainsWater, Constant::AssignResourceTypeNum("MainsWater")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::RainWater, Constant::AssignResourceTypeNum("RainWater")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::WellWater, Constant::AssignResourceTypeNum("WellWater")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Condensate, Constant::AssignResourceTypeNum("Condensate")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::EnergyTransfer, Constant::AssignResourceTypeNum("EnergyTransfer")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::DistrictCooling, Constant::AssignResourceTypeNum("DistrictCooling")));
    EXPECT_TRUE(
        compare_enums(Constant::ResourceType::DistrictHeatingWater, Constant::AssignResourceTypeNum("DistrictHeatingWater")));
    EXPECT_TRUE(
        compare_enums(Constant::ResourceType::DistrictHeatingSteam, Constant::AssignResourceTypeNum("DistrictHeatingSteam")));
    EXPECT_TRUE(
        compare_enums(Constant::ResourceType::ElectricityProduced, Constant::AssignResourceTypeNum("ElectricityProduced")));
    EXPECT_TRUE(
        compare_enums(Constant::ResourceType::ElectricityPurchased, Constant::AssignResourceTypeNum("ElectricityPurchased")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::ElectricitySurplusSold,
                              Constant::AssignResourceTypeNum("ElectricitySurplusSold")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::ElectricityNet, Constant::AssignResourceTypeNum("ElectricityNet")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::SolarWater, Constant::AssignResourceTypeNum("SolarWater")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::SolarAir, Constant::AssignResourceTypeNum("SolarAir")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::SO2, Constant::AssignResourceTypeNum("SO2")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::NOx, Constant::AssignResourceTypeNum("NOx")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::N2O, Constant::AssignResourceTypeNum("N2O")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::PM, Constant::AssignResourceTypeNum("PM")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::PM2_5, Constant::AssignResourceTypeNum("PM2.5")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::PM10, Constant::AssignResourceTypeNum("PM10")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::CO, Constant::AssignResourceTypeNum("CO")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::CO2, Constant::AssignResourceTypeNum("CO2")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::CH4, Constant::AssignResourceTypeNum("CH4")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::NH3, Constant::AssignResourceTypeNum("NH3")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::NMVOC, Constant::AssignResourceTypeNum("NMVOC")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Hg, Constant::AssignResourceTypeNum("Hg")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::Pb, Constant::AssignResourceTypeNum("Pb")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::NuclearHigh, Constant::AssignResourceTypeNum("Nuclear High")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::NuclearLow, Constant::AssignResourceTypeNum("Nuclear Low")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::WaterEnvironmentalFactors,
                              Constant::AssignResourceTypeNum("WaterEnvironmentalFactors")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::CarbonEquivalent, Constant::AssignResourceTypeNum("Carbon Equivalent")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::PlantLoopHeatingDemand,
                              Constant::AssignResourceTypeNum("PlantLoopHeatingDemand")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::PlantLoopCoolingDemand,
                              Constant::AssignResourceTypeNum("PlantLoopCoolingDemand")));
    EXPECT_TRUE(compare_enums(Constant::ResourceType::None, Constant::AssignResourceTypeNum("XYZ")));
}

TEST_F(EnergyPlusFixture, Constant_GetResourceTypeChar)
{

    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Electricity), "Electricity");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Natural_Gas), "NaturalGas");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Gasoline), "Gasoline");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Diesel), "Diesel");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Coal), "Coal");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::FuelOil_1), "FuelOilNo1");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::FuelOil_2), "FuelOilNo2");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Propane), "Propane");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::OtherFuel1), "OtherFuel1");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::OtherFuel2), "OtherFuel2");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Water), "Water");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::OnSiteWater), "OnSiteWater");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::MainsWater), "MainsWater");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::RainWater), "RainWater");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::WellWater), "WellWater");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Condensate), "Condensate");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::EnergyTransfer), "EnergyTransfer");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::DistrictCooling), "DistrictCooling");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::DistrictHeatingWater), "DistrictHeatingWater");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::DistrictHeatingSteam), "DistrictHeatingSteam");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::ElectricityProduced), "ElectricityProduced");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::ElectricityPurchased), "ElectricityPurchased");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::ElectricitySurplusSold), "ElectricitySurplusSold");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::ElectricityNet), "ElectricityNet");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::SolarWater), "SolarWater");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::SolarAir), "SolarAir");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::SO2), "SO2");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::NOx), "NOx");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::N2O), "N2O");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::PM), "PM");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::PM2_5), "PM2.5");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::PM10), "PM10");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::CO), "CO");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::CO2), "CO2");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::CH4), "CH4");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::NH3), "NH3");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::NMVOC), "NMVOC");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Hg), "Hg");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::Pb), "Pb");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::NuclearHigh), "Nuclear High");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::NuclearLow), "Nuclear Low");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::WaterEnvironmentalFactors), "WaterEnvironmentalFactors");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::CarbonEquivalent), "Carbon Equivalent");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::PlantLoopHeatingDemand), "PlantLoopHeatingDemand");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::PlantLoopCoolingDemand), "PlantLoopCoolingDemand");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::None), "Unknown");
    EXPECT_EQ(Constant::GetResourceTypeChar(Constant::ResourceType::None), "Unknown");
}
