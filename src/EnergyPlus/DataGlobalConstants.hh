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

#ifndef DataGlobalConstants_hh_INCLUDED
#define DataGlobalConstants_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataGlobalConstants {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // End Use Parameters
    extern int const NumEndUses;

    extern int const endUseHeating;
    extern int const endUseCooling;
    extern int const endUseInteriorLights;
    extern int const endUseExteriorLights;
    extern int const endUseInteriorEquipment;
    extern int const endUseExteriorEquipment;
    extern int const endUseFans;
    extern int const endUsePumps;
    extern int const endUseHeatRejection;
    extern int const endUseHumidification;
    extern int const endUseHeatRecovery;
    extern int const endUseWaterSystem;
    extern int const endUseRefrigeration;
    extern int const endUseCogeneration;

    // Resource Types
    extern std::string const cRT_None;
    extern std::string const cRT_NoneUC;
    extern int const iRT_None;
    extern std::string const cRT_Electricity;
    extern std::string const cRT_ElectricityUC;
    extern int const iRT_Electricity;
    extern std::string const cRT_Natural_Gas;
    extern std::string const cRT_Natural_GasUC;
    extern int const iRT_Natural_Gas;
    extern std::string const cRT_Gas;
    extern std::string const cRT_GasUC;
    extern int const iRT_Gas;
    extern std::string const cRT_Gasoline;
    extern std::string const cRT_GasolineUC;
    extern int const iRT_Gasoline;
    extern std::string const cRT_Diesel;
    extern std::string const cRT_DieselUC;
    extern int const iRT_Diesel;
    extern std::string const cRT_Coal;
    extern std::string const cRT_CoalUC;
    extern int const iRT_Coal;
    extern std::string const cRT_FuelOil_1;
    extern std::string const cRT_FuelOil_1UC;
    extern int const iRT_FuelOil_1;
    extern std::string const cRT_FuelOil_2;
    extern std::string const cRT_FuelOil_2UC;
    extern int const iRT_FuelOil_2;
    extern std::string const cRT_Propane;
    extern std::string const cRT_PropaneUC;
    extern int const iRT_Propane;
    extern std::string const cRT_Water;
    extern std::string const cRT_WaterUC;
    extern int const iRT_Water;
    extern std::string const cRT_EnergyTransfer;
    extern std::string const cRT_EnergyTransferUC;
    extern int const iRT_EnergyTransfer;
    extern std::string const cRT_Steam;
    extern std::string const cRT_SteamUC;
    extern int const iRT_Steam;
    extern std::string const cRT_DistrictCooling;
    extern std::string const cRT_DistrictCoolingUC;
    extern int const iRT_DistrictCooling;
    extern std::string const cRT_DistrictHeating;
    extern std::string const cRT_DistrictHeatingUC;
    extern int const iRT_DistrictHeating;
    extern std::string const cRT_ElectricityProduced;
    extern std::string const cRT_ElectricityProducedUC;
    extern int const iRT_ElectricityProduced;
    extern std::string const cRT_ElectricityPurchased;
    extern std::string const cRT_ElectricityPurchasedUC;
    extern int const iRT_ElectricityPurchased;
    extern std::string const cRT_ElectricitySurplusSold;
    extern std::string const cRT_ElectricitySurplusSoldUC;
    extern int const iRT_ElectricitySurplusSold;
    extern std::string const cRT_ElectricityNet;
    extern std::string const cRT_ElectricityNetUC;
    extern int const iRT_ElectricityNet;
    extern std::string const cRT_SolarWater;
    extern std::string const cRT_SolarWaterUC;
    extern int const iRT_SolarWater;
    extern std::string const cRT_SolarAir;
    extern std::string const cRT_SolarAirUC;
    extern int const iRT_SolarAir;
    extern std::string const cRT_SO2;
    extern std::string const cRT_SO2UC;
    extern int const iRT_SO2;
    extern std::string const cRT_NOx;
    extern std::string const cRT_NOxUC;
    extern int const iRT_NOx;
    extern std::string const cRT_N2O;
    extern std::string const cRT_N2OUC;
    extern int const iRT_N2O;
    extern std::string const cRT_PM;
    extern std::string const cRT_PMUC;
    extern int const iRT_PM;
    extern std::string const cRT_PM2_5;
    extern std::string const cRT_PM2_5UC;
    extern int const iRT_PM2_5;
    extern std::string const cRT_PM10;
    extern std::string const cRT_PM10UC;
    extern int const iRT_PM10;
    extern std::string const cRT_CO;
    extern std::string const cRT_COUC;
    extern int const iRT_CO;
    extern std::string const cRT_CO2;
    extern std::string const cRT_CO2UC;
    extern int const iRT_CO2;
    extern std::string const cRT_CH4;
    extern std::string const cRT_CH4UC;
    extern int const iRT_CH4;
    extern std::string const cRT_NH3;
    extern std::string const cRT_NH3UC;
    extern int const iRT_NH3;
    extern std::string const cRT_NMVOC;
    extern std::string const cRT_NMVOCUC;
    extern int const iRT_NMVOC;
    extern std::string const cRT_Hg;
    extern std::string const cRT_HgUC;
    extern int const iRT_Hg;
    extern std::string const cRT_Pb;
    extern std::string const cRT_PbUC;
    extern int const iRT_Pb;
    extern std::string const cRT_NuclearHigh;
    extern std::string const cRT_NuclearHighUC;
    extern int const iRT_NuclearHigh;
    extern std::string const cRT_NuclearLow;
    extern std::string const cRT_NuclearLowUC;
    extern int const iRT_NuclearLow;
    extern std::string const cRT_WaterEnvironmentalFactors;
    extern std::string const cRT_WaterEnvironmentalFactorsUC;
    extern int const iRT_WaterEnvironmentalFactors;
    extern std::string const cRT_CarbonEquivalent;
    extern std::string const cRT_CarbonEquivalentUC;
    extern int const iRT_CarbonEquivalent;
    extern std::string const cRT_Source;
    extern std::string const cRT_SourceUC;
    extern int const iRT_Source;
    extern std::string const cRT_PlantLoopHeatingDemand;
    extern std::string const cRT_PlantLoopHeatingDemandUC;
    extern int const iRT_PlantLoopHeatingDemand;
    extern std::string const cRT_PlantLoopCoolingDemand;
    extern std::string const cRT_PlantLoopCoolingDemandUC;
    extern int const iRT_PlantLoopCoolingDemand;
    extern std::string const cRT_OnSiteWater;
    extern std::string const cRT_OnSiteWaterUC;
    extern int const iRT_OnSiteWater;
    extern std::string const cRT_MainsWater;
    extern std::string const cRT_MainsWaterUC;
    extern int const iRT_MainsWater;
    extern std::string const cRT_RainWater;
    extern std::string const cRT_RainWaterUC;
    extern int const iRT_RainWater;
    extern std::string const cRT_WellWater;
    extern std::string const cRT_WellWaterUC;
    extern int const iRT_WellWater;
    extern std::string const cRT_Condensate;
    extern std::string const cRT_CondensateUC;
    extern int const iRT_Condensate;
    extern std::string const cRT_OtherFuel1;
    extern std::string const cRT_OtherFuel1UC;
    extern int const iRT_OtherFuel1;
    extern std::string const cRT_OtherFuel2;
    extern std::string const cRT_OtherFuel2UC;
    extern int const iRT_OtherFuel2;
    extern int const NumOfResourceTypes;
    extern int const ResourceTypeInitialOffset; // to reach "ValidTypes"
    extern Array1D_string const cRT_ValidTypes;

    extern int const iGeneratorICEngine;
    extern int const iGeneratorCombTurbine;
    extern int const iGeneratorPV;
    extern int const iGeneratorFuelCell;
    extern int const iGeneratorMicroCHP;
    extern int const iGeneratorMicroturbine;
    extern int const iGeneratorWindTurbine;
    extern int const iGeneratorPVWatts;

    extern int const iEvapCoolerDirectCELDEKPAD;
    extern int const iEvapCoolerInDirectCELDEKPAD;
    extern int const iEvapCoolerInDirectWETCOIL;
    extern int const iEvapCoolerInDirectRDDSpecial;
    extern int const iEvapCoolerDirectResearchSpecial;

    // DERIVED TYPE DEFINITIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:
    // na

    // SUBROUTINE SPECIFICATIONS FOR MODULE DataGlobalConstants

    // Functions

    int AssignResourceTypeNum(std::string const &ResourceTypeChar);

    std::string GetResourceTypeChar(int const ResourceTypeNum);

} // namespace DataGlobalConstants

} // namespace EnergyPlus

#endif
