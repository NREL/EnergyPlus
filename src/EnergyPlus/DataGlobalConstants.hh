#ifndef DataGlobalConstants_hh_INCLUDED
#define DataGlobalConstants_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

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
	extern std::string const cRT_DistillateOil;
	extern std::string const cRT_DistillateOilUC;
	extern int const iRT_DistillateOil;
	extern std::string const cRT_FuelOil_2;
	extern std::string const cRT_FuelOil_2UC;
	extern int const iRT_FuelOil_2;
	extern std::string const cRT_ResidualOil;
	extern std::string const cRT_ResidualOilUC;
	extern int const iRT_ResidualOil;
	extern std::string const cRT_Propane;
	extern std::string const cRT_PropaneUC;
	extern int const iRT_Propane;
	extern std::string const cRT_LPG;
	extern std::string const cRT_LPGUC;
	extern int const iRT_LPG;
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

	int
	AssignResourceTypeNum( std::string const & ResourceTypeChar );

	std::string
	GetResourceTypeChar( int const ResourceTypeNum );

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataGlobalConstants

} // EnergyPlus

#endif
