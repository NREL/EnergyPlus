// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	std::string const cRT_None( "None" );
	std::string const cRT_NoneUC( "NONE" );
	int const iRT_None( 1000 );
	std::string const cRT_Electricity( "Electricity" );
	std::string const cRT_ElectricityUC( "ELECTRICITY" );
	int const iRT_Electricity( 1001 );
	std::string const cRT_Natural_Gas( "NaturalGas" );
	std::string const cRT_Natural_GasUC( "NATURALGAS" );
	int const iRT_Natural_Gas( 1002 );
	std::string const cRT_Gas( "Gas" );
	std::string const cRT_GasUC( "GAS" );
	int const iRT_Gas( 1002 );
	std::string const cRT_Gasoline( "Gasoline" );
	std::string const cRT_GasolineUC( "GASOLINE" );
	int const iRT_Gasoline( 1003 );
	std::string const cRT_Diesel( "Diesel" );
	std::string const cRT_DieselUC( "DIESEL" );
	int const iRT_Diesel( 1004 );
	std::string const cRT_Coal( "Coal" );
	std::string const cRT_CoalUC( "COAL" );
	int const iRT_Coal( 1005 );
	std::string const cRT_FuelOil_1( "FuelOil#1" );
	std::string const cRT_FuelOil_1UC( "FUELOIL#1" );
	int const iRT_FuelOil_1( 1006 );
	std::string const cRT_DistillateOil( "DistillateOil" );
	std::string const cRT_DistillateOilUC( "DISTILLATEOIL" );
	int const iRT_DistillateOil( 1006 );
	std::string const cRT_FuelOil_2( "FuelOil#2" );
	std::string const cRT_FuelOil_2UC( "FUELOIL#2" );
	int const iRT_FuelOil_2( 1007 );
	std::string const cRT_ResidualOil( "ResidualOil" );
	std::string const cRT_ResidualOilUC( "RESIDUALOIL" );
	int const iRT_ResidualOil( 1007 );
	std::string const cRT_Propane( "Propane" );
	std::string const cRT_PropaneUC( "PROPANE" );
	int const iRT_Propane( 1008 );
	std::string const cRT_LPG( "LPG" );
	std::string const cRT_LPGUC( "LPG" );
	int const iRT_LPG( 1008 );
	std::string const cRT_Water( "Water" );
	std::string const cRT_WaterUC( "WATER" );
	int const iRT_Water( 1009 );
	std::string const cRT_EnergyTransfer( "EnergyTransfer" );
	std::string const cRT_EnergyTransferUC( "ENERGYTRANSFER" );
	int const iRT_EnergyTransfer( 1010 );
	std::string const cRT_Steam( "Steam" );
	std::string const cRT_SteamUC( "STEAM" );
	int const iRT_Steam( 1011 );
	std::string const cRT_DistrictCooling( "DistrictCooling" );
	std::string const cRT_DistrictCoolingUC( "DISTRICTCOOLING" );
	int const iRT_DistrictCooling( 1012 );
	std::string const cRT_DistrictHeating( "DistrictHeating" );
	std::string const cRT_DistrictHeatingUC( "DISTRICTHEATING" );
	int const iRT_DistrictHeating( 1013 );
	std::string const cRT_ElectricityProduced( "ElectricityProduced" );
	std::string const cRT_ElectricityProducedUC( "ELECTRICITYPRODUCED" );
	int const iRT_ElectricityProduced( 1014 );
	std::string const cRT_ElectricityPurchased( "ElectricityPurchased" );
	std::string const cRT_ElectricityPurchasedUC( "ELECTRICITYPURCHASED" );
	int const iRT_ElectricityPurchased( 1015 );
	std::string const cRT_ElectricitySurplusSold( "ElectricitySurplusSold" );
	std::string const cRT_ElectricitySurplusSoldUC( "ELECTRICITYSURPLUSSOLD" );
	int const iRT_ElectricitySurplusSold( 1016 );
	std::string const cRT_ElectricityNet( "ElectricityNet" );
	std::string const cRT_ElectricityNetUC( "ELECTRICITYNET" );
	int const iRT_ElectricityNet( 1017 );
	std::string const cRT_SolarWater( "SolarWater" );
	std::string const cRT_SolarWaterUC( "SOLARWATER" );
	int const iRT_SolarWater( 1018 );
	std::string const cRT_SolarAir( "SolarAir" );
	std::string const cRT_SolarAirUC( "SOLARAIR" );
	int const iRT_SolarAir( 1019 );
	std::string const cRT_SO2( "SO2" );
	std::string const cRT_SO2UC( "SO2" );
	int const iRT_SO2( 1020 );
	std::string const cRT_NOx( "NOx" );
	std::string const cRT_NOxUC( "NOX" );
	int const iRT_NOx( 1021 );
	std::string const cRT_N2O( "N2O" );
	std::string const cRT_N2OUC( "N2O" );
	int const iRT_N2O( 1022 );
	std::string const cRT_PM( "PM" );
	std::string const cRT_PMUC( "PM" );
	int const iRT_PM( 1023 );
	std::string const cRT_PM2_5( "PM2.5" );
	std::string const cRT_PM2_5UC( "PM2.5" );
	int const iRT_PM2_5( 1024 );
	std::string const cRT_PM10( "PM10" );
	std::string const cRT_PM10UC( "PM10" );
	int const iRT_PM10( 1025 );
	std::string const cRT_CO( "CO" );
	std::string const cRT_COUC( "CO" );
	int const iRT_CO( 1026 );
	std::string const cRT_CO2( "CO2" );
	std::string const cRT_CO2UC( "CO2" );
	int const iRT_CO2( 1027 );
	std::string const cRT_CH4( "CH4" );
	std::string const cRT_CH4UC( "CH4" );
	int const iRT_CH4( 1028 );
	std::string const cRT_NH3( "NH3" );
	std::string const cRT_NH3UC( "NH3" );
	int const iRT_NH3( 1029 );
	std::string const cRT_NMVOC( "NMVOC" );
	std::string const cRT_NMVOCUC( "NMVOC" );
	int const iRT_NMVOC( 1030 );
	std::string const cRT_Hg( "Hg" );
	std::string const cRT_HgUC( "HG" );
	int const iRT_Hg( 1031 );
	std::string const cRT_Pb( "Pb" );
	std::string const cRT_PbUC( "PB" );
	int const iRT_Pb( 1032 );
	std::string const cRT_NuclearHigh( "NuclearHigh" );
	std::string const cRT_NuclearHighUC( "NUCLEARHIGH" );
	int const iRT_NuclearHigh( 1033 );
	std::string const cRT_NuclearLow( "NuclearLow" );
	std::string const cRT_NuclearLowUC( "NUCLEARLOW" );
	int const iRT_NuclearLow( 1034 );
	std::string const cRT_WaterEnvironmentalFactors( "WaterEnvironmentalFactors" );
	std::string const cRT_WaterEnvironmentalFactorsUC( "WATERENVIRONMENTALFACTORS" );
	int const iRT_WaterEnvironmentalFactors( 1035 );
	std::string const cRT_CarbonEquivalent( "Carbon Equivalent" );
	std::string const cRT_CarbonEquivalentUC( "CARBON EQUIVALENT" );
	int const iRT_CarbonEquivalent( 1036 );
	std::string const cRT_Source( "Source" );
	std::string const cRT_SourceUC( "SOURCE" );
	int const iRT_Source( 1037 );
	std::string const cRT_PlantLoopHeatingDemand( "PlantLoopHeatingDemand" );
	std::string const cRT_PlantLoopHeatingDemandUC( "PLANTLOOPHEATINGDEMAND" );
	int const iRT_PlantLoopHeatingDemand( 1038 );
	std::string const cRT_PlantLoopCoolingDemand( "PlantLoopCoolingDemand" );
	std::string const cRT_PlantLoopCoolingDemandUC( "PLANTLOOPCOOLINGDEMAND" );
	int const iRT_PlantLoopCoolingDemand( 1039 );
	std::string const cRT_OnSiteWater( "OnSiteWater" );
	std::string const cRT_OnSiteWaterUC( "ONSITEWATER" );
	int const iRT_OnSiteWater( 1040 );
	std::string const cRT_MainsWater( "MainsWater" );
	std::string const cRT_MainsWaterUC( "MAINSWATER" );
	int const iRT_MainsWater( 1041 );
	std::string const cRT_RainWater( "RainWater" );
	std::string const cRT_RainWaterUC( "RAINWATER" );
	int const iRT_RainWater( 1042 );
	std::string const cRT_WellWater( "WellWater" );
	std::string const cRT_WellWaterUC( "WellWATER" );
	int const iRT_WellWater( 1043 );
	std::string const cRT_Condensate( "Condensate" );
	std::string const cRT_CondensateUC( "CONDENSATE" );
	int const iRT_Condensate( 1044 );
	std::string const cRT_OtherFuel1( "OtherFuel1" );
	std::string const cRT_OtherFuel1UC( "OTHERFUEL1" );
	int const iRT_OtherFuel1( 1045 );
	std::string const cRT_OtherFuel2( "OtherFuel2" );
	std::string const cRT_OtherFuel2UC( "OTHERFUEL2" );
	int const iRT_OtherFuel2( 1046 );
	int const NumOfResourceTypes( 46 );
	int const ResourceTypeInitialOffset( 1000 ); // to reach "ValidTypes"
	Array1D_string const cRT_ValidTypes( {0,NumOfResourceTypes}, { cRT_None, cRT_Electricity, cRT_Gas, cRT_Gasoline, cRT_Diesel, cRT_Coal, cRT_FuelOil_1, cRT_FuelOil_2, cRT_Propane, cRT_Water, cRT_EnergyTransfer, cRT_Steam, cRT_DistrictCooling, cRT_DistrictHeating, cRT_ElectricityProduced, cRT_ElectricityPurchased, cRT_ElectricitySurplusSold, cRT_ElectricityNet, cRT_SolarWater, cRT_SolarAir, cRT_SO2, cRT_NOx, cRT_N2O, cRT_PM, cRT_PM2_5, cRT_PM10, cRT_CO, cRT_CO2, cRT_CH4, cRT_NH3, cRT_NMVOC, cRT_Hg, cRT_Pb, cRT_NuclearHigh, cRT_NuclearLow, cRT_WaterEnvironmentalFactors, cRT_CarbonEquivalent, cRT_Source, cRT_PlantLoopHeatingDemand, cRT_PlantLoopCoolingDemand, cRT_OnSiteWater, cRT_MainsWater, cRT_RainWater, cRT_WellWater, cRT_Condensate, cRT_OtherFuel1, cRT_OtherFuel2 } );

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
	AssignResourceTypeNum( std::string const & ResourceTypeChar )
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

	std::string
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

		// Return value
		std::string ResourceTypeChar;

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

} // DataGlobalConstants

} // EnergyPlus
