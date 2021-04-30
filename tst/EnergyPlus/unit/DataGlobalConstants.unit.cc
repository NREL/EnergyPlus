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

// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, DataGlobalConstants_AssignResourceTypeNum)
{

    EXPECT_EQ(DataGlobalConstants::ResourceType::Electricity, DataGlobalConstants::AssignResourceTypeNum("Electricity"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Natural_Gas, DataGlobalConstants::AssignResourceTypeNum("NaturalGas"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Gasoline, DataGlobalConstants::AssignResourceTypeNum("Gasoline"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Diesel, DataGlobalConstants::AssignResourceTypeNum("Diesel"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Coal, DataGlobalConstants::AssignResourceTypeNum("Coal"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::FuelOil_1, DataGlobalConstants::AssignResourceTypeNum("FuelOilNo1"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::FuelOil_2, DataGlobalConstants::AssignResourceTypeNum("FuelOilNo2"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Propane, DataGlobalConstants::AssignResourceTypeNum("Propane"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::OtherFuel1, DataGlobalConstants::AssignResourceTypeNum("OtherFuel1"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::OtherFuel2, DataGlobalConstants::AssignResourceTypeNum("OtherFuel2"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Water, DataGlobalConstants::AssignResourceTypeNum("Water"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::OnSiteWater, DataGlobalConstants::AssignResourceTypeNum("OnSiteWater"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::MainsWater, DataGlobalConstants::AssignResourceTypeNum("MainsWater"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::RainWater, DataGlobalConstants::AssignResourceTypeNum("RainWater"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::WellWater, DataGlobalConstants::AssignResourceTypeNum("WellWater"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Condensate, DataGlobalConstants::AssignResourceTypeNum("Condensate"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::EnergyTransfer, DataGlobalConstants::AssignResourceTypeNum("EnergyTransfer"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Steam, DataGlobalConstants::AssignResourceTypeNum("Steam"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::DistrictCooling, DataGlobalConstants::AssignResourceTypeNum("DistrictCooling"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::DistrictHeating, DataGlobalConstants::AssignResourceTypeNum("DistrictHeating"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::ElectricityProduced, DataGlobalConstants::AssignResourceTypeNum("ElectricityProduced"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::ElectricityPurchased, DataGlobalConstants::AssignResourceTypeNum("ElectricityPurchased"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::ElectricitySurplusSold, DataGlobalConstants::AssignResourceTypeNum("ElectricitySurplusSold"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::ElectricityNet, DataGlobalConstants::AssignResourceTypeNum("ElectricityNet"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::SolarWater, DataGlobalConstants::AssignResourceTypeNum("SolarWater"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::SolarAir, DataGlobalConstants::AssignResourceTypeNum("SolarAir"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::SO2, DataGlobalConstants::AssignResourceTypeNum("SO2"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::NOx, DataGlobalConstants::AssignResourceTypeNum("NOx"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::N2O, DataGlobalConstants::AssignResourceTypeNum("N2O"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::PM, DataGlobalConstants::AssignResourceTypeNum("PM"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::PM2_5, DataGlobalConstants::AssignResourceTypeNum("PM2.5"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::PM10, DataGlobalConstants::AssignResourceTypeNum("PM10"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::CO, DataGlobalConstants::AssignResourceTypeNum("CO"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::CO2, DataGlobalConstants::AssignResourceTypeNum("CO2"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::CH4, DataGlobalConstants::AssignResourceTypeNum("CH4"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::NH3, DataGlobalConstants::AssignResourceTypeNum("NH3"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::NMVOC, DataGlobalConstants::AssignResourceTypeNum("NMVOC"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Hg, DataGlobalConstants::AssignResourceTypeNum("Hg"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::Pb, DataGlobalConstants::AssignResourceTypeNum("Pb"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::NuclearHigh, DataGlobalConstants::AssignResourceTypeNum("Nuclear High"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::NuclearLow, DataGlobalConstants::AssignResourceTypeNum("Nuclear Low"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::WaterEnvironmentalFactors, DataGlobalConstants::AssignResourceTypeNum("WaterEnvironmentalFactors"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::CarbonEquivalent, DataGlobalConstants::AssignResourceTypeNum("Carbon Equivalent"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::PlantLoopHeatingDemand, DataGlobalConstants::AssignResourceTypeNum("PlantLoopHeatingDemand"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::PlantLoopCoolingDemand, DataGlobalConstants::AssignResourceTypeNum("PlantLoopCoolingDemand"));
    EXPECT_EQ(DataGlobalConstants::ResourceType::None, DataGlobalConstants::AssignResourceTypeNum("XYZ"));
}

TEST_F(EnergyPlusFixture, DataGlobalConstants_GetResourceTypeChar)
{

    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Electricity), "Electricity");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Natural_Gas), "NaturalGas");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Gasoline), "Gasoline");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Diesel), "Diesel");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::Coal), "Coal");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::FuelOil_1), "FuelOilNo1");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::FuelOil_2), "FuelOilNo2");
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
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::None), "Unknown");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::ResourceType::None), "Unknown");
}
