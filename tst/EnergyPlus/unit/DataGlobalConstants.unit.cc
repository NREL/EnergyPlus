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

// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobalConstants.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, DataGlobalConstants_AssignResourceTypeNum)
{

    EXPECT_EQ(DataGlobalConstants::iRT_Electricity, DataGlobalConstants::AssignResourceTypeNum("Electricity"));
    EXPECT_EQ(DataGlobalConstants::iRT_Natural_Gas, DataGlobalConstants::AssignResourceTypeNum("NaturalGas"));
    EXPECT_EQ(DataGlobalConstants::iRT_Gasoline, DataGlobalConstants::AssignResourceTypeNum("Gasoline"));
    EXPECT_EQ(DataGlobalConstants::iRT_Diesel, DataGlobalConstants::AssignResourceTypeNum("Diesel"));
    EXPECT_EQ(DataGlobalConstants::iRT_Coal, DataGlobalConstants::AssignResourceTypeNum("Coal"));
    EXPECT_EQ(DataGlobalConstants::iRT_FuelOil_1, DataGlobalConstants::AssignResourceTypeNum("FuelOilNo1"));
    EXPECT_EQ(DataGlobalConstants::iRT_FuelOil_2, DataGlobalConstants::AssignResourceTypeNum("FuelOilNo2"));
    EXPECT_EQ(DataGlobalConstants::iRT_Propane, DataGlobalConstants::AssignResourceTypeNum("Propane"));
    EXPECT_EQ(DataGlobalConstants::iRT_OtherFuel1, DataGlobalConstants::AssignResourceTypeNum("OtherFuel1"));
    EXPECT_EQ(DataGlobalConstants::iRT_OtherFuel2, DataGlobalConstants::AssignResourceTypeNum("OtherFuel2"));
    EXPECT_EQ(DataGlobalConstants::iRT_Water, DataGlobalConstants::AssignResourceTypeNum("Water"));
    EXPECT_EQ(DataGlobalConstants::iRT_OnSiteWater, DataGlobalConstants::AssignResourceTypeNum("OnSiteWater"));
    EXPECT_EQ(DataGlobalConstants::iRT_MainsWater, DataGlobalConstants::AssignResourceTypeNum("MainsWater"));
    EXPECT_EQ(DataGlobalConstants::iRT_RainWater, DataGlobalConstants::AssignResourceTypeNum("RainWater"));
    EXPECT_EQ(DataGlobalConstants::iRT_WellWater, DataGlobalConstants::AssignResourceTypeNum("WellWater"));
    EXPECT_EQ(DataGlobalConstants::iRT_Condensate, DataGlobalConstants::AssignResourceTypeNum("Condensate"));
    EXPECT_EQ(DataGlobalConstants::iRT_EnergyTransfer, DataGlobalConstants::AssignResourceTypeNum("EnergyTransfer"));
    EXPECT_EQ(DataGlobalConstants::iRT_Steam, DataGlobalConstants::AssignResourceTypeNum("Steam"));
    EXPECT_EQ(DataGlobalConstants::iRT_DistrictCooling, DataGlobalConstants::AssignResourceTypeNum("DistrictCooling"));
    EXPECT_EQ(DataGlobalConstants::iRT_DistrictHeating, DataGlobalConstants::AssignResourceTypeNum("DistrictHeating"));
    EXPECT_EQ(DataGlobalConstants::iRT_ElectricityProduced, DataGlobalConstants::AssignResourceTypeNum("ElectricityProduced"));
    EXPECT_EQ(DataGlobalConstants::iRT_ElectricityPurchased, DataGlobalConstants::AssignResourceTypeNum("ElectricityPurchased"));
    EXPECT_EQ(DataGlobalConstants::iRT_ElectricitySurplusSold, DataGlobalConstants::AssignResourceTypeNum("ElectricitySurplusSold"));
    EXPECT_EQ(DataGlobalConstants::iRT_ElectricityNet, DataGlobalConstants::AssignResourceTypeNum("ElectricityNet"));
    EXPECT_EQ(DataGlobalConstants::iRT_SolarWater, DataGlobalConstants::AssignResourceTypeNum("SolarWater"));
    EXPECT_EQ(DataGlobalConstants::iRT_SolarAir, DataGlobalConstants::AssignResourceTypeNum("SolarAir"));
    EXPECT_EQ(DataGlobalConstants::iRT_SO2, DataGlobalConstants::AssignResourceTypeNum("SO2"));
    EXPECT_EQ(DataGlobalConstants::iRT_NOx, DataGlobalConstants::AssignResourceTypeNum("NOx"));
    EXPECT_EQ(DataGlobalConstants::iRT_N2O, DataGlobalConstants::AssignResourceTypeNum("N2O"));
    EXPECT_EQ(DataGlobalConstants::iRT_PM, DataGlobalConstants::AssignResourceTypeNum("PM"));
    EXPECT_EQ(DataGlobalConstants::iRT_PM2_5, DataGlobalConstants::AssignResourceTypeNum("PM2.5"));
    EXPECT_EQ(DataGlobalConstants::iRT_PM10, DataGlobalConstants::AssignResourceTypeNum("PM10"));
    EXPECT_EQ(DataGlobalConstants::iRT_CO, DataGlobalConstants::AssignResourceTypeNum("CO"));
    EXPECT_EQ(DataGlobalConstants::iRT_CO2, DataGlobalConstants::AssignResourceTypeNum("CO2"));
    EXPECT_EQ(DataGlobalConstants::iRT_CH4, DataGlobalConstants::AssignResourceTypeNum("CH4"));
    EXPECT_EQ(DataGlobalConstants::iRT_NH3, DataGlobalConstants::AssignResourceTypeNum("NH3"));
    EXPECT_EQ(DataGlobalConstants::iRT_NMVOC, DataGlobalConstants::AssignResourceTypeNum("NMVOC"));
    EXPECT_EQ(DataGlobalConstants::iRT_Hg, DataGlobalConstants::AssignResourceTypeNum("Hg"));
    EXPECT_EQ(DataGlobalConstants::iRT_Pb, DataGlobalConstants::AssignResourceTypeNum("Pb"));
    EXPECT_EQ(DataGlobalConstants::iRT_NuclearHigh, DataGlobalConstants::AssignResourceTypeNum("Nuclear High"));
    EXPECT_EQ(DataGlobalConstants::iRT_NuclearLow, DataGlobalConstants::AssignResourceTypeNum("Nuclear Low"));
    EXPECT_EQ(DataGlobalConstants::iRT_WaterEnvironmentalFactors, DataGlobalConstants::AssignResourceTypeNum("WaterEnvironmentalFactors"));
    EXPECT_EQ(DataGlobalConstants::iRT_CarbonEquivalent, DataGlobalConstants::AssignResourceTypeNum("Carbon Equivalent"));
    EXPECT_EQ(DataGlobalConstants::iRT_PlantLoopHeatingDemand, DataGlobalConstants::AssignResourceTypeNum("PlantLoopHeatingDemand"));
    EXPECT_EQ(DataGlobalConstants::iRT_PlantLoopCoolingDemand, DataGlobalConstants::AssignResourceTypeNum("PlantLoopCoolingDemand"));
    EXPECT_EQ(0, DataGlobalConstants::AssignResourceTypeNum("XYZ"));
}

TEST_F(EnergyPlusFixture, DataGlobalConstants_GetResourceTypeChar)
{

    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Electricity), "Electricity");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Natural_Gas), "NaturalGas");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Gasoline), "Gasoline");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Diesel), "Diesel");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Coal), "Coal");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_FuelOil_1), "FuelOilNo1");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_FuelOil_2), "FuelOilNo2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Propane), "Propane");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_OtherFuel1), "OtherFuel1");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_OtherFuel2), "OtherFuel2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Water), "Water");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_OnSiteWater), "OnSiteWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_MainsWater), "MainsWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_RainWater), "RainWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_WellWater), "WellWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Condensate), "Condensate");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_EnergyTransfer), "EnergyTransfer");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Steam), "Steam");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_DistrictCooling), "DistrictCooling");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_DistrictHeating), "DistrictHeating");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_ElectricityProduced), "ElectricityProduced");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_ElectricityPurchased), "ElectricityPurchased");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_ElectricitySurplusSold), "ElectricitySurplusSold");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_ElectricityNet), "ElectricityNet");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_SolarWater), "SolarWater");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_SolarAir), "SolarAir");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_SO2), "SO2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_NOx), "NOx");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_N2O), "N2O");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_PM), "PM");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_PM2_5), "PM2.5");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_PM10), "PM10");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_CO), "CO");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_CO2), "CO2");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_CH4), "CH4");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_NH3), "NH3");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_NMVOC), "NMVOC");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Hg), "Hg");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_Pb), "Pb");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_NuclearHigh), "Nuclear High");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_NuclearLow), "Nuclear Low");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_WaterEnvironmentalFactors), "WaterEnvironmentalFactors");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_CarbonEquivalent), "Carbon Equivalent");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_PlantLoopHeatingDemand), "PlantLoopHeatingDemand");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(DataGlobalConstants::iRT_PlantLoopCoolingDemand), "PlantLoopCoolingDemand");
    EXPECT_EQ(DataGlobalConstants::GetResourceTypeChar(0), "Unknown");
}

