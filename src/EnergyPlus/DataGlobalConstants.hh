#ifndef DataGlobalConstants_hh_INCLUDED
#define DataGlobalConstants_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>

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
	extern int const istrLeng;
	extern Fstring const cRT_None;
	extern Fstring const cRT_NoneUC;
	extern int const iRT_None;
	extern Fstring const cRT_Electricity;
	extern Fstring const cRT_ElectricityUC;
	extern int const iRT_Electricity;
	extern Fstring const cRT_Natural_Gas;
	extern Fstring const cRT_Natural_GasUC;
	extern int const iRT_Natural_Gas;
	extern Fstring const cRT_Gas;
	extern Fstring const cRT_GasUC;
	extern int const iRT_Gas;
	extern Fstring const cRT_Gasoline;
	extern Fstring const cRT_GasolineUC;
	extern int const iRT_Gasoline;
	extern Fstring const cRT_Diesel;
	extern Fstring const cRT_DieselUC;
	extern int const iRT_Diesel;
	extern Fstring const cRT_Coal;
	extern Fstring const cRT_CoalUC;
	extern int const iRT_Coal;
	extern Fstring const cRT_FuelOil_1;
	extern Fstring const cRT_FuelOil_1UC;
	extern int const iRT_FuelOil_1;
	extern Fstring const cRT_DistillateOil;
	extern Fstring const cRT_DistillateOilUC;
	extern int const iRT_DistillateOil;
	extern Fstring const cRT_FuelOil_2;
	extern Fstring const cRT_FuelOil_2UC;
	extern int const iRT_FuelOil_2;
	extern Fstring const cRT_ResidualOil;
	extern Fstring const cRT_ResidualOilUC;
	extern int const iRT_ResidualOil;
	extern Fstring const cRT_Propane;
	extern Fstring const cRT_PropaneUC;
	extern int const iRT_Propane;
	extern Fstring const cRT_LPG;
	extern Fstring const cRT_LPGUC;
	extern int const iRT_LPG;
	extern Fstring const cRT_Water;
	extern Fstring const cRT_WaterUC;
	extern int const iRT_Water;
	extern Fstring const cRT_EnergyTransfer;
	extern Fstring const cRT_EnergyTransferUC;
	extern int const iRT_EnergyTransfer;
	extern Fstring const cRT_Steam;
	extern Fstring const cRT_SteamUC;
	extern int const iRT_Steam;
	extern Fstring const cRT_DistrictCooling;
	extern Fstring const cRT_DistrictCoolingUC;
	extern int const iRT_DistrictCooling;
	extern Fstring const cRT_DistrictHeating;
	extern Fstring const cRT_DistrictHeatingUC;
	extern int const iRT_DistrictHeating;
	extern Fstring const cRT_ElectricityProduced;
	extern Fstring const cRT_ElectricityProducedUC;
	extern int const iRT_ElectricityProduced;
	extern Fstring const cRT_ElectricityPurchased;
	extern Fstring const cRT_ElectricityPurchasedUC;
	extern int const iRT_ElectricityPurchased;
	extern Fstring const cRT_ElectricitySurplusSold;
	extern Fstring const cRT_ElectricitySurplusSoldUC;
	extern int const iRT_ElectricitySurplusSold;
	extern Fstring const cRT_ElectricityNet;
	extern Fstring const cRT_ElectricityNetUC;
	extern int const iRT_ElectricityNet;
	extern Fstring const cRT_SolarWater;
	extern Fstring const cRT_SolarWaterUC;
	extern int const iRT_SolarWater;
	extern Fstring const cRT_SolarAir;
	extern Fstring const cRT_SolarAirUC;
	extern int const iRT_SolarAir;
	extern Fstring const cRT_SO2;
	extern Fstring const cRT_SO2UC;
	extern int const iRT_SO2;
	extern Fstring const cRT_NOx;
	extern Fstring const cRT_NOxUC;
	extern int const iRT_NOx;
	extern Fstring const cRT_N2O;
	extern Fstring const cRT_N2OUC;
	extern int const iRT_N2O;
	extern Fstring const cRT_PM;
	extern Fstring const cRT_PMUC;
	extern int const iRT_PM;
	extern Fstring const cRT_PM2_5;
	extern Fstring const cRT_PM2_5UC;
	extern int const iRT_PM2_5;
	extern Fstring const cRT_PM10;
	extern Fstring const cRT_PM10UC;
	extern int const iRT_PM10;
	extern Fstring const cRT_CO;
	extern Fstring const cRT_COUC;
	extern int const iRT_CO;
	extern Fstring const cRT_CO2;
	extern Fstring const cRT_CO2UC;
	extern int const iRT_CO2;
	extern Fstring const cRT_CH4;
	extern Fstring const cRT_CH4UC;
	extern int const iRT_CH4;
	extern Fstring const cRT_NH3;
	extern Fstring const cRT_NH3UC;
	extern int const iRT_NH3;
	extern Fstring const cRT_NMVOC;
	extern Fstring const cRT_NMVOCUC;
	extern int const iRT_NMVOC;
	extern Fstring const cRT_Hg;
	extern Fstring const cRT_HgUC;
	extern int const iRT_Hg;
	extern Fstring const cRT_Pb;
	extern Fstring const cRT_PbUC;
	extern int const iRT_Pb;
	extern Fstring const cRT_NuclearHigh;
	extern Fstring const cRT_NuclearHighUC;
	extern int const iRT_NuclearHigh;
	extern Fstring const cRT_NuclearLow;
	extern Fstring const cRT_NuclearLowUC;
	extern int const iRT_NuclearLow;
	extern Fstring const cRT_WaterEnvironmentalFactors;
	extern Fstring const cRT_WaterEnvironmentalFactorsUC;
	extern int const iRT_WaterEnvironmentalFactors;
	extern Fstring const cRT_CarbonEquivalent;
	extern Fstring const cRT_CarbonEquivalentUC;
	extern int const iRT_CarbonEquivalent;
	extern Fstring const cRT_Source;
	extern Fstring const cRT_SourceUC;
	extern int const iRT_Source;
	extern Fstring const cRT_PlantLoopHeatingDemand;
	extern Fstring const cRT_PlantLoopHeatingDemandUC;
	extern int const iRT_PlantLoopHeatingDemand;
	extern Fstring const cRT_PlantLoopCoolingDemand;
	extern Fstring const cRT_PlantLoopCoolingDemandUC;
	extern int const iRT_PlantLoopCoolingDemand;
	extern Fstring const cRT_OnSiteWater;
	extern Fstring const cRT_OnSiteWaterUC;
	extern int const iRT_OnSiteWater;
	extern Fstring const cRT_MainsWater;
	extern Fstring const cRT_MainsWaterUC;
	extern int const iRT_MainsWater;
	extern Fstring const cRT_RainWater;
	extern Fstring const cRT_RainWaterUC;
	extern int const iRT_RainWater;
	extern Fstring const cRT_WellWater;
	extern Fstring const cRT_WellWaterUC;
	extern int const iRT_WellWater;
	extern Fstring const cRT_Condensate;
	extern Fstring const cRT_CondensateUC;
	extern int const iRT_Condensate;
	extern Fstring const cRT_OtherFuel1;
	extern Fstring const cRT_OtherFuel1UC;
	extern int const iRT_OtherFuel1;
	extern Fstring const cRT_OtherFuel2;
	extern Fstring const cRT_OtherFuel2UC;
	extern int const iRT_OtherFuel2;
	extern int const NumOfResourceTypes;
	extern int const ResourceTypeInitialOffset; // to reach "ValidTypes"
	extern FArray1D_Fstring const cRT_ValidTypes;

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
	AssignResourceTypeNum( Fstring const & ResourceTypeChar );

	Fstring
	GetResourceTypeChar( int const ResourceTypeNum );

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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
