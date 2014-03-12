// ObjexxFCL Headers

// EnergyPlus Headers
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <InputProcessor.hh>

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
	int const NumEndUses( 14 );

	int const endUseHeating( 1 );
	int const endUseCooling( 2 );
	int const endUseInteriorLights( 3 );
	int const endUseExteriorLights( 4 );
	int const endUseInteriorEquipment( 5 );
	int const endUseExteriorEquipment( 6 );
	int const endUseFans( 7 );
	int const endUsePumps( 8 );
	int const endUseHeatRejection( 9 );
	int const endUseHumidification( 10 );
	int const endUseHeatRecovery( 11 );
	int const endUseWaterSystem( 12 );
	int const endUseRefrigeration( 13 );
	int const endUseCogeneration( 14 );

	// Resource Types
	int const istrLeng( 60 );
	Fstring const cRT_None( istrLeng, "None" );
	Fstring const cRT_NoneUC( istrLeng, "NONE" );
	int const iRT_None( 1000 );
	Fstring const cRT_Electricity( istrLeng, "Electricity" );
	Fstring const cRT_ElectricityUC( istrLeng, "ELECTRICITY" );
	int const iRT_Electricity( 1001 );
	Fstring const cRT_Natural_Gas( istrLeng, "NaturalGas" );
	Fstring const cRT_Natural_GasUC( istrLeng, "NATURALGAS" );
	int const iRT_Natural_Gas( 1002 );
	Fstring const cRT_Gas( istrLeng, "Gas" );
	Fstring const cRT_GasUC( istrLeng, "GAS" );
	int const iRT_Gas( 1002 );
	Fstring const cRT_Gasoline( istrLeng, "Gasoline" );
	Fstring const cRT_GasolineUC( istrLeng, "GASOLINE" );
	int const iRT_Gasoline( 1003 );
	Fstring const cRT_Diesel( istrLeng, "Diesel" );
	Fstring const cRT_DieselUC( istrLeng, "DIESEL" );
	int const iRT_Diesel( 1004 );
	Fstring const cRT_Coal( istrLeng, "Coal" );
	Fstring const cRT_CoalUC( istrLeng, "COAL" );
	int const iRT_Coal( 1005 );
	Fstring const cRT_FuelOil_1( istrLeng, "FuelOil#1" );
	Fstring const cRT_FuelOil_1UC( istrLeng, "FUELOIL#1" );
	int const iRT_FuelOil_1( 1006 );
	Fstring const cRT_DistillateOil( istrLeng, "DistillateOil" );
	Fstring const cRT_DistillateOilUC( istrLeng, "DISTILLATEOIL" );
	int const iRT_DistillateOil( 1006 );
	Fstring const cRT_FuelOil_2( istrLeng, "FuelOil#2" );
	Fstring const cRT_FuelOil_2UC( istrLeng, "FUELOIL#2" );
	int const iRT_FuelOil_2( 1007 );
	Fstring const cRT_ResidualOil( istrLeng, "ResidualOil" );
	Fstring const cRT_ResidualOilUC( istrLeng, "RESIDUALOIL" );
	int const iRT_ResidualOil( 1007 );
	Fstring const cRT_Propane( istrLeng, "Propane" );
	Fstring const cRT_PropaneUC( istrLeng, "PROPANE" );
	int const iRT_Propane( 1008 );
	Fstring const cRT_LPG( istrLeng, "LPG" );
	Fstring const cRT_LPGUC( istrLeng, "LPG" );
	int const iRT_LPG( 1008 );
	Fstring const cRT_Water( istrLeng, "Water" );
	Fstring const cRT_WaterUC( istrLeng, "WATER" );
	int const iRT_Water( 1009 );
	Fstring const cRT_EnergyTransfer( istrLeng, "EnergyTransfer" );
	Fstring const cRT_EnergyTransferUC( istrLeng, "ENERGYTRANSFER" );
	int const iRT_EnergyTransfer( 1010 );
	Fstring const cRT_Steam( istrLeng, "Steam" );
	Fstring const cRT_SteamUC( istrLeng, "STEAM" );
	int const iRT_Steam( 1011 );
	Fstring const cRT_DistrictCooling( istrLeng, "DistrictCooling" );
	Fstring const cRT_DistrictCoolingUC( istrLeng, "DISTRICTCOOLING" );
	int const iRT_DistrictCooling( 1012 );
	Fstring const cRT_DistrictHeating( istrLeng, "DistrictHeating" );
	Fstring const cRT_DistrictHeatingUC( istrLeng, "DISTRICTHEATING" );
	int const iRT_DistrictHeating( 1013 );
	Fstring const cRT_ElectricityProduced( istrLeng, "ElectricityProduced" );
	Fstring const cRT_ElectricityProducedUC( istrLeng, "ELECTRICITYPRODUCED" );
	int const iRT_ElectricityProduced( 1014 );
	Fstring const cRT_ElectricityPurchased( istrLeng, "ElectricityPurchased" );
	Fstring const cRT_ElectricityPurchasedUC( istrLeng, "ELECTRICITYPURCHASED" );
	int const iRT_ElectricityPurchased( 1015 );
	Fstring const cRT_ElectricitySurplusSold( istrLeng, "ElectricitySurplusSold" );
	Fstring const cRT_ElectricitySurplusSoldUC( istrLeng, "ELECTRICITYSURPLUSSOLD" );
	int const iRT_ElectricitySurplusSold( 1016 );
	Fstring const cRT_ElectricityNet( istrLeng, "ElectricityNet" );
	Fstring const cRT_ElectricityNetUC( istrLeng, "ELECTRICITYNET" );
	int const iRT_ElectricityNet( 1017 );
	Fstring const cRT_SolarWater( istrLeng, "SolarWater" );
	Fstring const cRT_SolarWaterUC( istrLeng, "SOLARWATER" );
	int const iRT_SolarWater( 1018 );
	Fstring const cRT_SolarAir( istrLeng, "SolarAir" );
	Fstring const cRT_SolarAirUC( istrLeng, "SOLARAIR" );
	int const iRT_SolarAir( 1019 );
	Fstring const cRT_SO2( istrLeng, "SO2" );
	Fstring const cRT_SO2UC( istrLeng, "SO2" );
	int const iRT_SO2( 1020 );
	Fstring const cRT_NOx( istrLeng, "NOx" );
	Fstring const cRT_NOxUC( istrLeng, "NOX" );
	int const iRT_NOx( 1021 );
	Fstring const cRT_N2O( istrLeng, "N2O" );
	Fstring const cRT_N2OUC( istrLeng, "N2O" );
	int const iRT_N2O( 1022 );
	Fstring const cRT_PM( istrLeng, "PM" );
	Fstring const cRT_PMUC( istrLeng, "PM" );
	int const iRT_PM( 1023 );
	Fstring const cRT_PM2_5( istrLeng, "PM2.5" );
	Fstring const cRT_PM2_5UC( istrLeng, "PM2.5" );
	int const iRT_PM2_5( 1024 );
	Fstring const cRT_PM10( istrLeng, "PM10" );
	Fstring const cRT_PM10UC( istrLeng, "PM10" );
	int const iRT_PM10( 1025 );
	Fstring const cRT_CO( istrLeng, "CO" );
	Fstring const cRT_COUC( istrLeng, "CO" );
	int const iRT_CO( 1026 );
	Fstring const cRT_CO2( istrLeng, "CO2" );
	Fstring const cRT_CO2UC( istrLeng, "CO2" );
	int const iRT_CO2( 1027 );
	Fstring const cRT_CH4( istrLeng, "CH4" );
	Fstring const cRT_CH4UC( istrLeng, "CH4" );
	int const iRT_CH4( 1028 );
	Fstring const cRT_NH3( istrLeng, "NH3" );
	Fstring const cRT_NH3UC( istrLeng, "NH3" );
	int const iRT_NH3( 1029 );
	Fstring const cRT_NMVOC( istrLeng, "NMVOC" );
	Fstring const cRT_NMVOCUC( istrLeng, "NMVOC" );
	int const iRT_NMVOC( 1030 );
	Fstring const cRT_Hg( istrLeng, "Hg" );
	Fstring const cRT_HgUC( istrLeng, "HG" );
	int const iRT_Hg( 1031 );
	Fstring const cRT_Pb( istrLeng, "Pb" );
	Fstring const cRT_PbUC( istrLeng, "PB" );
	int const iRT_Pb( 1032 );
	Fstring const cRT_NuclearHigh( istrLeng, "NuclearHigh" );
	Fstring const cRT_NuclearHighUC( istrLeng, "NUCLEARHIGH" );
	int const iRT_NuclearHigh( 1033 );
	Fstring const cRT_NuclearLow( istrLeng, "NuclearLow" );
	Fstring const cRT_NuclearLowUC( istrLeng, "NUCLEARLOW" );
	int const iRT_NuclearLow( 1034 );
	Fstring const cRT_WaterEnvironmentalFactors( istrLeng, "WaterEnvironmentalFactors" );
	Fstring const cRT_WaterEnvironmentalFactorsUC( istrLeng, "WATERENVIRONMENTALFACTORS" );
	int const iRT_WaterEnvironmentalFactors( 1035 );
	Fstring const cRT_CarbonEquivalent( istrLeng, "Carbon Equivalent" );
	Fstring const cRT_CarbonEquivalentUC( istrLeng, "CARBON EQUIVALENT" );
	int const iRT_CarbonEquivalent( 1036 );
	Fstring const cRT_Source( istrLeng, "Source" );
	Fstring const cRT_SourceUC( istrLeng, "SOURCE" );
	int const iRT_Source( 1037 );
	Fstring const cRT_PlantLoopHeatingDemand( istrLeng, "PlantLoopHeatingDemand" );
	Fstring const cRT_PlantLoopHeatingDemandUC( istrLeng, "PLANTLOOPHEATINGDEMAND" );
	int const iRT_PlantLoopHeatingDemand( 1038 );
	Fstring const cRT_PlantLoopCoolingDemand( istrLeng, "PlantLoopCoolingDemand" );
	Fstring const cRT_PlantLoopCoolingDemandUC( istrLeng, "PLANTLOOPCOOLINGDEMAND" );
	int const iRT_PlantLoopCoolingDemand( 1039 );
	Fstring const cRT_OnSiteWater( istrLeng, "OnSiteWater" );
	Fstring const cRT_OnSiteWaterUC( istrLeng, "ONSITEWATER" );
	int const iRT_OnSiteWater( 1040 );
	Fstring const cRT_MainsWater( istrLeng, "MainsWater" );
	Fstring const cRT_MainsWaterUC( istrLeng, "MAINSWATER" );
	int const iRT_MainsWater( 1041 );
	Fstring const cRT_RainWater( istrLeng, "RainWater" );
	Fstring const cRT_RainWaterUC( istrLeng, "RAINWATER" );
	int const iRT_RainWater( 1042 );
	Fstring const cRT_WellWater( istrLeng, "WellWater" );
	Fstring const cRT_WellWaterUC( istrLeng, "WellWATER" );
	int const iRT_WellWater( 1043 );
	Fstring const cRT_Condensate( istrLeng, "Condensate" );
	Fstring const cRT_CondensateUC( istrLeng, "CONDENSATE" );
	int const iRT_Condensate( 1044 );
	Fstring const cRT_OtherFuel1( istrLeng, "OtherFuel1" );
	Fstring const cRT_OtherFuel1UC( istrLeng, "OTHERFUEL1" );
	int const iRT_OtherFuel1( 1045 );
	Fstring const cRT_OtherFuel2( istrLeng, "OtherFuel2" );
	Fstring const cRT_OtherFuel2UC( istrLeng, "OTHERFUEL2" );
	int const iRT_OtherFuel2( 1046 );
	int const NumOfResourceTypes( 46 );
	int const ResourceTypeInitialOffset( 1000 ); // to reach "ValidTypes"
	FArray1D_Fstring const cRT_ValidTypes( {0,NumOfResourceTypes}, sFstring( istrLeng ), { cRT_None, cRT_Electricity, cRT_Gas, cRT_Gasoline, cRT_Diesel, cRT_Coal, cRT_FuelOil_1, cRT_FuelOil_2, cRT_Propane, cRT_Water, cRT_EnergyTransfer, cRT_Steam, cRT_DistrictCooling, cRT_DistrictHeating, cRT_ElectricityProduced, cRT_ElectricityPurchased, cRT_ElectricitySurplusSold, cRT_ElectricityNet, cRT_SolarWater, cRT_SolarAir, cRT_SO2, cRT_NOx, cRT_N2O, cRT_PM, cRT_PM2_5, cRT_PM10, cRT_CO, cRT_CO2, cRT_CH4, cRT_NH3, cRT_NMVOC, cRT_Hg, cRT_Pb, cRT_NuclearHigh, cRT_NuclearLow, cRT_WaterEnvironmentalFactors, cRT_CarbonEquivalent, cRT_Source, cRT_PlantLoopHeatingDemand, cRT_PlantLoopCoolingDemand, cRT_OnSiteWater, cRT_MainsWater, cRT_RainWater, cRT_WellWater, cRT_Condensate, cRT_OtherFuel1, cRT_OtherFuel2 } );

	int const iGeneratorICEngine( 1 );
	int const iGeneratorCombTurbine( 2 );
	int const iGeneratorPV( 3 );
	int const iGeneratorFuelCell( 4 );
	int const iGeneratorMicroCHP( 5 );
	int const iGeneratorMicroturbine( 6 );
	int const iGeneratorWindTurbine( 7 );

	int const iEvapCoolerDirectCELDEKPAD( 1001 );
	int const iEvapCoolerInDirectCELDEKPAD( 1002 );
	int const iEvapCoolerInDirectWETCOIL( 1003 );
	int const iEvapCoolerInDirectRDDSpecial( 1004 );
	int const iEvapCoolerDirectResearchSpecial( 1005 );

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataGlobalConstants

	// Functions

	int
	AssignResourceTypeNum( Fstring const & ResourceTypeChar )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Assists in assigning proper numeric resource types to data structures.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;

		// Return value
		int ResourceTypeNum;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		ResourceTypeNum = 0;

		{ auto const SELECT_CASE_var( MakeUPPERCase( ResourceTypeChar ) );

		if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) ) {
			ResourceTypeNum = iRT_Electricity;

		} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) ) {
			ResourceTypeNum = iRT_Gas;

		} else if ( SELECT_CASE_var == "GASOLINE" ) {
			ResourceTypeNum = iRT_Gasoline;

		} else if ( SELECT_CASE_var == "DIESEL" ) {
			ResourceTypeNum = iRT_Diesel;

		} else if ( SELECT_CASE_var == "COAL" ) {
			ResourceTypeNum = iRT_Coal;

		} else if ( ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
			ResourceTypeNum = iRT_FuelOil_1;

		} else if ( ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
			ResourceTypeNum = iRT_FuelOil_2;

		} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) ) {
			ResourceTypeNum = iRT_Propane;

		} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
			ResourceTypeNum = iRT_OtherFuel1;

		} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
			ResourceTypeNum = iRT_OtherFuel2;

		} else if ( ( SELECT_CASE_var == "WATER" ) || ( SELECT_CASE_var == "H2O" ) ) {
			ResourceTypeNum = iRT_Water; // use record keeping

		} else if ( ( SELECT_CASE_var == "ONSITEWATER" ) || ( SELECT_CASE_var == "WATERPRODUCED" ) || ( SELECT_CASE_var == "ONSITE WATER" ) ) {
			ResourceTypeNum = iRT_OnSiteWater; // these are for supply record keeping

		} else if ( ( SELECT_CASE_var == "MAINSWATER" ) || ( SELECT_CASE_var == "WATERSUPPLY" ) ) {
			ResourceTypeNum = iRT_MainsWater; // record keeping

		} else if ( ( SELECT_CASE_var == "RAINWATER" ) || ( SELECT_CASE_var == "PRECIPITATION" ) ) {
			ResourceTypeNum = iRT_RainWater; // record keeping

		} else if ( ( SELECT_CASE_var == "WELLWATER" ) || ( SELECT_CASE_var == "Groundwater" ) ) {
			ResourceTypeNum = iRT_WellWater; // record keeping

		} else if ( SELECT_CASE_var == "CONDENSATE" ) {
			ResourceTypeNum = iRT_Condensate;

		} else if ( SELECT_CASE_var == "ENERGYTRANSFER" ) {
			ResourceTypeNum = iRT_EnergyTransfer;

		} else if ( SELECT_CASE_var == "STEAM" ) {
			ResourceTypeNum = iRT_Steam;

		} else if ( SELECT_CASE_var == "DISTRICTCOOLING" ) {
			ResourceTypeNum = iRT_DistrictCooling;

		} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
			ResourceTypeNum = iRT_DistrictHeating;

		} else if ( SELECT_CASE_var == "ELECTRICITYPRODUCED" ) {
			ResourceTypeNum = iRT_ElectricityProduced;

		} else if ( SELECT_CASE_var == "ELECTRICITYPURCHASED" ) {
			ResourceTypeNum = iRT_ElectricityPurchased;

		} else if ( SELECT_CASE_var == "ELECTRICITYSURPLUSSOLD" ) {
			ResourceTypeNum = iRT_ElectricitySurplusSold;

		} else if ( SELECT_CASE_var == "ELECTRICITYNET" ) {
			ResourceTypeNum = iRT_ElectricityNet;

		} else if ( SELECT_CASE_var == "SOLARWATER" ) {
			ResourceTypeNum = iRT_SolarWater;

		} else if ( SELECT_CASE_var == "SOLARAIR" ) {
			ResourceTypeNum = iRT_SolarAir;

		} else if ( SELECT_CASE_var == "SO2" ) {
			ResourceTypeNum = iRT_SO2;

		} else if ( SELECT_CASE_var == "NOX" ) {
			ResourceTypeNum = iRT_NOx;

		} else if ( SELECT_CASE_var == "N2O" ) {
			ResourceTypeNum = iRT_N2O;

		} else if ( SELECT_CASE_var == "PM" ) {
			ResourceTypeNum = iRT_PM;

		} else if ( SELECT_CASE_var == "PM2.5" ) {
			ResourceTypeNum = iRT_PM2_5;

		} else if ( SELECT_CASE_var == "PM10" ) {
			ResourceTypeNum = iRT_PM10;

		} else if ( SELECT_CASE_var == "CO" ) {
			ResourceTypeNum = iRT_CO;

		} else if ( SELECT_CASE_var == "CO2" ) {
			ResourceTypeNum = iRT_CO2;

		} else if ( SELECT_CASE_var == "CH4" ) {
			ResourceTypeNum = iRT_CH4;

		} else if ( SELECT_CASE_var == "NH3" ) {
			ResourceTypeNum = iRT_NH3;

		} else if ( SELECT_CASE_var == "NMVOC" ) {
			ResourceTypeNum = iRT_NMVOC;

		} else if ( SELECT_CASE_var == "HG" ) {
			ResourceTypeNum = iRT_Hg;

		} else if ( SELECT_CASE_var == "PB" ) {
			ResourceTypeNum = iRT_Pb;

		} else if ( SELECT_CASE_var == "NUCLEAR HIGH" ) {
			ResourceTypeNum = iRT_NuclearHigh;

		} else if ( SELECT_CASE_var == "NUCLEAR LOW" ) {
			ResourceTypeNum = iRT_NuclearLow;

		} else if ( SELECT_CASE_var == "WATERENVIRONMENTALFACTORS" ) {
			ResourceTypeNum = iRT_WaterEnvironmentalFactors;

		} else if ( SELECT_CASE_var == "CARBON EQUIVALENT" ) {
			ResourceTypeNum = iRT_CarbonEquivalent;

		} else if ( SELECT_CASE_var == "SOURCE" ) {
			ResourceTypeNum = iRT_Source;

		} else if ( SELECT_CASE_var == "PLANTLOOPHEATINGDEMAND" ) {
			ResourceTypeNum = iRT_PlantLoopHeatingDemand;

		} else if ( SELECT_CASE_var == "PLANTLOOPCOOLINGDEMAND" ) {
			ResourceTypeNum = iRT_PlantLoopCoolingDemand;

		} else {
			ResourceTypeNum = 0;

		}}

		return ResourceTypeNum;

	}

	Fstring
	GetResourceTypeChar( int const ResourceTypeNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Shows the resource type character string, given the resource type numeric.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::MaxNameLength;

		// Return value
		Fstring ResourceTypeChar( MaxNameLength );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// na

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		{ auto const SELECT_CASE_var( ResourceTypeNum );

		if ( SELECT_CASE_var == iRT_Electricity ) {
			ResourceTypeChar = "Electricity";

		} else if ( SELECT_CASE_var == iRT_Gas ) {
			ResourceTypeChar = "Gas";

		} else if ( SELECT_CASE_var == iRT_Gasoline ) {
			ResourceTypeChar = "Gasoline";

		} else if ( SELECT_CASE_var == iRT_Diesel ) {
			ResourceTypeChar = "Diesel";

		} else if ( SELECT_CASE_var == iRT_Coal ) {
			ResourceTypeChar = "Coal";

		} else if ( SELECT_CASE_var == iRT_FuelOil_1 ) {
			ResourceTypeChar = "FuelOil#1";

		} else if ( SELECT_CASE_var == iRT_FuelOil_2 ) {
			ResourceTypeChar = "FuelOil#2";

		} else if ( SELECT_CASE_var == iRT_Propane ) {
			ResourceTypeChar = "Propane";

		} else if ( SELECT_CASE_var == iRT_OtherFuel1 ) {
			ResourceTypeChar = "OtherFuel1";

		} else if ( SELECT_CASE_var == iRT_OtherFuel2 ) {
			ResourceTypeChar = "OtherFuel2";

		} else if ( SELECT_CASE_var == iRT_Water ) {
			ResourceTypeChar = "Water";

		} else if ( SELECT_CASE_var == iRT_OnSiteWater ) {
			ResourceTypeChar = "OnSiteWater";

		} else if ( SELECT_CASE_var == iRT_MainsWater ) {
			ResourceTypeChar = "MainsWater";

		} else if ( SELECT_CASE_var == iRT_RainWater ) {
			ResourceTypeChar = "RainWater";

		} else if ( SELECT_CASE_var == iRT_Condensate ) {
			ResourceTypeChar = "Condensate";

		} else if ( SELECT_CASE_var == iRT_WellWater ) {
			ResourceTypeChar = "WellWater";

		} else if ( SELECT_CASE_var == iRT_EnergyTransfer ) {
			ResourceTypeChar = "EnergyTransfer";

		} else if ( SELECT_CASE_var == iRT_Steam ) {
			ResourceTypeChar = "Steam";

		} else if ( SELECT_CASE_var == iRT_DistrictCooling ) {
			ResourceTypeChar = "DistrictCooling";

		} else if ( SELECT_CASE_var == iRT_DistrictHeating ) {
			ResourceTypeChar = "DistrictHeating";

		} else if ( SELECT_CASE_var == iRT_ElectricityProduced ) {
			ResourceTypeChar = "ElectricityProduced";

		} else if ( SELECT_CASE_var == iRT_ElectricityPurchased ) {
			ResourceTypeChar = "ElectricityPurchased";

		} else if ( SELECT_CASE_var == iRT_ElectricitySurplusSold ) {
			ResourceTypeChar = "ElectricitySurplusSold";

		} else if ( SELECT_CASE_var == iRT_ElectricityNet ) {
			ResourceTypeChar = "ElectricityNet";

		} else if ( SELECT_CASE_var == iRT_SolarWater ) {
			ResourceTypeChar = "SolarWater";

		} else if ( SELECT_CASE_var == iRT_SolarAir ) {
			ResourceTypeChar = "SolarAir";

		} else if ( SELECT_CASE_var == iRT_SO2 ) {
			ResourceTypeChar = "SO2";

		} else if ( SELECT_CASE_var == iRT_NOx ) {
			ResourceTypeChar = "NOx";

		} else if ( SELECT_CASE_var == iRT_N2O ) {
			ResourceTypeChar = "N2O";

		} else if ( SELECT_CASE_var == iRT_PM ) {
			ResourceTypeChar = "PM";

		} else if ( SELECT_CASE_var == iRT_PM2_5 ) {
			ResourceTypeChar = "PM2.5";

		} else if ( SELECT_CASE_var == iRT_PM10 ) {
			ResourceTypeChar = "PM10";

		} else if ( SELECT_CASE_var == iRT_CO ) {
			ResourceTypeChar = "CO";

		} else if ( SELECT_CASE_var == iRT_CO2 ) {
			ResourceTypeChar = "CO2";

		} else if ( SELECT_CASE_var == iRT_CH4 ) {
			ResourceTypeChar = "CH4";

		} else if ( SELECT_CASE_var == iRT_NH3 ) {
			ResourceTypeChar = "NH3";

		} else if ( SELECT_CASE_var == iRT_NMVOC ) {
			ResourceTypeChar = "NMVOC";

		} else if ( SELECT_CASE_var == iRT_Hg ) {
			ResourceTypeChar = "Hg";

		} else if ( SELECT_CASE_var == iRT_Pb ) {
			ResourceTypeChar = "Pb";

		} else if ( SELECT_CASE_var == iRT_NuclearHigh ) {
			ResourceTypeChar = "Nuclear High";

		} else if ( SELECT_CASE_var == iRT_NuclearLow ) {
			ResourceTypeChar = "Nuclear Low";

		} else if ( SELECT_CASE_var == iRT_WaterEnvironmentalFactors ) {
			ResourceTypeChar = "WaterEnvironmentalFactors";

		} else if ( SELECT_CASE_var == iRT_CarbonEquivalent ) {
			ResourceTypeChar = "Carbon Equivalent";

		} else if ( SELECT_CASE_var == iRT_Source ) {
			ResourceTypeChar = "Source";

		} else if ( SELECT_CASE_var == iRT_PlantLoopHeatingDemand ) {
			ResourceTypeChar = "PlantLoopHeatingDemand";

		} else if ( SELECT_CASE_var == iRT_PlantLoopCoolingDemand ) {
			ResourceTypeChar = "PlantLoopCoolingDemand";

		} else {
			ResourceTypeChar = "Unknown";
		}}

		return ResourceTypeChar;

	}

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
