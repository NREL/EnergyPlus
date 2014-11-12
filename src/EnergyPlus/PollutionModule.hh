#ifndef PollutionModule_hh_INCLUDED
#define PollutionModule_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PollutionModule {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const ElecPollFactor;
	extern int const NatGasPollFactor;
	extern int const FuelOil1PollFactor;
	extern int const FuelOil2PollFactor;
	extern int const CoalPollFactor;
	extern int const GasolinePollFactor;
	extern int const PropanePollFactor;
	extern int const DieselPollFactor;
	extern int const OtherFuel1PollFactor;
	extern int const OtherFuel2PollFactor;
	extern int const PollFactorNumTypes;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	//Total for all of the Pollutants
	//Total Carbon Equivalent Components
	//  !Fuel Types
	//Total Carbon Equivalent Coeffs
	// Purchased Efficiencies

	//Fuel Types used with the Pollution Factors
	//Facility Meter Indexes
	//Facility Meter Values used in Pollution Calcs

	extern bool PollutionReportSetup;
	extern bool GetInputFlagPollution;
	extern int NumEnvImpactFactors;
	extern int NumFuelFactors;

	//         Subroutine Specifications for the Module

	// Types

	struct ComponentProps
	{
		// Members
		int FuelFactorType;
		Real64 Source;
		Real64 CO2Pollution;
		Real64 COPollution;
		Real64 CH4Pollution;
		Real64 NOxPollution;
		Real64 N2OPollution;
		Real64 SO2Pollution;
		Real64 PMPollution;
		Real64 PM10Pollution;
		Real64 PM25Pollution;
		Real64 NH3Pollution;
		Real64 NMVOCPollution;
		Real64 HgPollution;
		Real64 PbPollution;
		Real64 WaterPollution;
		Real64 NucHiPollution;
		Real64 NucLoPollution;

		// Default Constructor
		ComponentProps() :
			FuelFactorType( 0 ),
			Source( 0.0 ),
			CO2Pollution( 0.0 ),
			COPollution( 0.0 ),
			CH4Pollution( 0.0 ),
			NOxPollution( 0.0 ),
			N2OPollution( 0.0 ),
			SO2Pollution( 0.0 ),
			PMPollution( 0.0 ),
			PM10Pollution( 0.0 ),
			PM25Pollution( 0.0 ),
			NH3Pollution( 0.0 ),
			NMVOCPollution( 0.0 ),
			HgPollution( 0.0 ),
			PbPollution( 0.0 ),
			WaterPollution( 0.0 ),
			NucHiPollution( 0.0 ),
			NucLoPollution( 0.0 )
		{}

		// Member Constructor
		ComponentProps(
			int const FuelFactorType,
			Real64 const Source,
			Real64 const CO2Pollution,
			Real64 const COPollution,
			Real64 const CH4Pollution,
			Real64 const NOxPollution,
			Real64 const N2OPollution,
			Real64 const SO2Pollution,
			Real64 const PMPollution,
			Real64 const PM10Pollution,
			Real64 const PM25Pollution,
			Real64 const NH3Pollution,
			Real64 const NMVOCPollution,
			Real64 const HgPollution,
			Real64 const PbPollution,
			Real64 const WaterPollution,
			Real64 const NucHiPollution,
			Real64 const NucLoPollution
		) :
			FuelFactorType( FuelFactorType ),
			Source( Source ),
			CO2Pollution( CO2Pollution ),
			COPollution( COPollution ),
			CH4Pollution( CH4Pollution ),
			NOxPollution( NOxPollution ),
			N2OPollution( N2OPollution ),
			SO2Pollution( SO2Pollution ),
			PMPollution( PMPollution ),
			PM10Pollution( PM10Pollution ),
			PM25Pollution( PM25Pollution ),
			NH3Pollution( NH3Pollution ),
			NMVOCPollution( NMVOCPollution ),
			HgPollution( HgPollution ),
			PbPollution( PbPollution ),
			WaterPollution( WaterPollution ),
			NucHiPollution( NucHiPollution ),
			NucLoPollution( NucLoPollution )
		{}

	};

	struct CoefficientProps
	{
		// Members
		int FuelFactorType;
		bool FuelFactorUsed;
		Real64 Source;
		Real64 CO2;
		Real64 CO;
		Real64 CH4;
		Real64 NOx;
		Real64 N2O;
		Real64 SO2;
		Real64 PM;
		Real64 PM10;
		Real64 PM25;
		Real64 NH3;
		Real64 NMVOC;
		Real64 Hg;
		Real64 Pb;
		Real64 Water;
		Real64 NucHi;
		Real64 NucLo;
		int SourceSched;
		int CO2Sched;
		int COSched;
		int CH4Sched;
		int NOxSched;
		int N2OSched;
		int SO2Sched;
		int PMSched;
		int PM10Sched;
		int PM25Sched;
		int NH3Sched;
		int NMVOCSched;
		int HgSched;
		int PbSched;
		int WaterSched;
		int NucHiSched;
		int NucLoSched;

		// Default Constructor
		CoefficientProps() :
			FuelFactorType( 0 ),
			FuelFactorUsed( false ),
			Source( 0.0 ),
			CO2( 0.0 ),
			CO( 0.0 ),
			CH4( 0.0 ),
			NOx( 0.0 ),
			N2O( 0.0 ),
			SO2( 0.0 ),
			PM( 0.0 ),
			PM10( 0.0 ),
			PM25( 0.0 ),
			NH3( 0.0 ),
			NMVOC( 0.0 ),
			Hg( 0.0 ),
			Pb( 0.0 ),
			Water( 0.0 ),
			NucHi( 0.0 ),
			NucLo( 0.0 ),
			SourceSched( 0 ),
			CO2Sched( 0 ),
			COSched( 0 ),
			CH4Sched( 0 ),
			NOxSched( 0 ),
			N2OSched( 0 ),
			SO2Sched( 0 ),
			PMSched( 0 ),
			PM10Sched( 0 ),
			PM25Sched( 0 ),
			NH3Sched( 0 ),
			NMVOCSched( 0 ),
			HgSched( 0 ),
			PbSched( 0 ),
			WaterSched( 0 ),
			NucHiSched( 0 ),
			NucLoSched( 0 )
		{}

		// Member Constructor
		CoefficientProps(
			int const FuelFactorType,
			bool const FuelFactorUsed,
			Real64 const Source,
			Real64 const CO2,
			Real64 const CO,
			Real64 const CH4,
			Real64 const NOx,
			Real64 const N2O,
			Real64 const SO2,
			Real64 const PM,
			Real64 const PM10,
			Real64 const PM25,
			Real64 const NH3,
			Real64 const NMVOC,
			Real64 const Hg,
			Real64 const Pb,
			Real64 const Water,
			Real64 const NucHi,
			Real64 const NucLo,
			int const SourceSched,
			int const CO2Sched,
			int const COSched,
			int const CH4Sched,
			int const NOxSched,
			int const N2OSched,
			int const SO2Sched,
			int const PMSched,
			int const PM10Sched,
			int const PM25Sched,
			int const NH3Sched,
			int const NMVOCSched,
			int const HgSched,
			int const PbSched,
			int const WaterSched,
			int const NucHiSched,
			int const NucLoSched
		) :
			FuelFactorType( FuelFactorType ),
			FuelFactorUsed( FuelFactorUsed ),
			Source( Source ),
			CO2( CO2 ),
			CO( CO ),
			CH4( CH4 ),
			NOx( NOx ),
			N2O( N2O ),
			SO2( SO2 ),
			PM( PM ),
			PM10( PM10 ),
			PM25( PM25 ),
			NH3( NH3 ),
			NMVOC( NMVOC ),
			Hg( Hg ),
			Pb( Pb ),
			Water( Water ),
			NucHi( NucHi ),
			NucLo( NucLo ),
			SourceSched( SourceSched ),
			CO2Sched( CO2Sched ),
			COSched( COSched ),
			CH4Sched( CH4Sched ),
			NOxSched( NOxSched ),
			N2OSched( N2OSched ),
			SO2Sched( SO2Sched ),
			PMSched( PMSched ),
			PM10Sched( PM10Sched ),
			PM25Sched( PM25Sched ),
			NH3Sched( NH3Sched ),
			NMVOCSched( NMVOCSched ),
			HgSched( HgSched ),
			PbSched( PbSched ),
			WaterSched( WaterSched ),
			NucHiSched( NucHiSched ),
			NucLoSched( NucLoSched )
		{}

	};

	struct PollutionProps
	{
		// Members
		//Components
		ComponentProps ElecComp;
		ComponentProps ElecPurchComp;
		ComponentProps ElecSurplusSoldComp;
		ComponentProps NatGasComp;
		ComponentProps FuelOil1Comp;
		ComponentProps FuelOil2Comp;
		ComponentProps CoalComp;
		ComponentProps GasolineComp;
		ComponentProps PropaneComp;
		ComponentProps DieselComp;
		ComponentProps OtherFuel1Comp;
		ComponentProps OtherFuel2Comp;
		//Total for all of the Pollutants
		Real64 N2OPollutTotal;
		Real64 CH4PollutTotal;
		Real64 CO2PollutTotal;
		//Total Carbon Equivalent Components
		Real64 TotCarbonEquivFromN2O;
		Real64 TotCarbonEquivFromCH4;
		Real64 TotCarbonEquivFromCO2;
		//Fuel Type Coefficients
		CoefficientProps ElecCoef;
		CoefficientProps NatGasCoef;
		CoefficientProps FuelOil1Coef;
		CoefficientProps FuelOil2Coef;
		CoefficientProps CoalCoef;
		CoefficientProps GasolineCoef;
		CoefficientProps PropaneCoef;
		CoefficientProps DieselCoef;
		CoefficientProps OtherFuel1Coef;
		CoefficientProps OtherFuel2Coef;
		//Total Carbon Equivalent Coeffs
		Real64 CarbonEquivN2O;
		Real64 CarbonEquivCH4;
		Real64 CarbonEquivCO2;
		Real64 PurchHeatEffic;
		Real64 PurchCoolCOP;
		Real64 SteamConvEffic;

		// Default Constructor
		PollutionProps() :
			N2OPollutTotal( 0.0 ),
			CH4PollutTotal( 0.0 ),
			CO2PollutTotal( 0.0 ),
			TotCarbonEquivFromN2O( 0.0 ),
			TotCarbonEquivFromCH4( 0.0 ),
			TotCarbonEquivFromCO2( 0.0 ),
			CarbonEquivN2O( 0.0 ),
			CarbonEquivCH4( 0.0 ),
			CarbonEquivCO2( 0.0 ),
			PurchHeatEffic( 0.0 ),
			PurchCoolCOP( 0.0 ),
			SteamConvEffic( 0.0 )
		{}

		// Member Constructor
		PollutionProps(
			ComponentProps const & ElecComp,
			ComponentProps const & ElecPurchComp,
			ComponentProps const & ElecSurplusSoldComp,
			ComponentProps const & NatGasComp,
			ComponentProps const & FuelOil1Comp,
			ComponentProps const & FuelOil2Comp,
			ComponentProps const & CoalComp,
			ComponentProps const & GasolineComp,
			ComponentProps const & PropaneComp,
			ComponentProps const & DieselComp,
			ComponentProps const & OtherFuel1Comp,
			ComponentProps const & OtherFuel2Comp,
			Real64 const N2OPollutTotal,
			Real64 const CH4PollutTotal,
			Real64 const CO2PollutTotal,
			Real64 const TotCarbonEquivFromN2O,
			Real64 const TotCarbonEquivFromCH4,
			Real64 const TotCarbonEquivFromCO2,
			CoefficientProps const & ElecCoef,
			CoefficientProps const & NatGasCoef,
			CoefficientProps const & FuelOil1Coef,
			CoefficientProps const & FuelOil2Coef,
			CoefficientProps const & CoalCoef,
			CoefficientProps const & GasolineCoef,
			CoefficientProps const & PropaneCoef,
			CoefficientProps const & DieselCoef,
			CoefficientProps const & OtherFuel1Coef,
			CoefficientProps const & OtherFuel2Coef,
			Real64 const CarbonEquivN2O,
			Real64 const CarbonEquivCH4,
			Real64 const CarbonEquivCO2,
			Real64 const PurchHeatEffic,
			Real64 const PurchCoolCOP,
			Real64 const SteamConvEffic
		) :
			ElecComp( ElecComp ),
			ElecPurchComp( ElecPurchComp ),
			ElecSurplusSoldComp( ElecSurplusSoldComp ),
			NatGasComp( NatGasComp ),
			FuelOil1Comp( FuelOil1Comp ),
			FuelOil2Comp( FuelOil2Comp ),
			CoalComp( CoalComp ),
			GasolineComp( GasolineComp ),
			PropaneComp( PropaneComp ),
			DieselComp( DieselComp ),
			OtherFuel1Comp( OtherFuel1Comp ),
			OtherFuel2Comp( OtherFuel2Comp ),
			N2OPollutTotal( N2OPollutTotal ),
			CH4PollutTotal( CH4PollutTotal ),
			CO2PollutTotal( CO2PollutTotal ),
			TotCarbonEquivFromN2O( TotCarbonEquivFromN2O ),
			TotCarbonEquivFromCH4( TotCarbonEquivFromCH4 ),
			TotCarbonEquivFromCO2( TotCarbonEquivFromCO2 ),
			ElecCoef( ElecCoef ),
			NatGasCoef( NatGasCoef ),
			FuelOil1Coef( FuelOil1Coef ),
			FuelOil2Coef( FuelOil2Coef ),
			CoalCoef( CoalCoef ),
			GasolineCoef( GasolineCoef ),
			PropaneCoef( PropaneCoef ),
			DieselCoef( DieselCoef ),
			OtherFuel1Coef( OtherFuel1Coef ),
			OtherFuel2Coef( OtherFuel2Coef ),
			CarbonEquivN2O( CarbonEquivN2O ),
			CarbonEquivCH4( CarbonEquivCH4 ),
			CarbonEquivCO2( CarbonEquivCO2 ),
			PurchHeatEffic( PurchHeatEffic ),
			PurchCoolCOP( PurchCoolCOP ),
			SteamConvEffic( SteamConvEffic )
		{}

	};

	struct FuelTypeProps
	{
		// Members
		//FuelType Names
		FArray1D_string FuelTypeNames;
		//Fuel Types used with the Pollution Factors
		Real64 Elec;
		Real64 NatGas;
		Real64 FuelOil1;
		Real64 FuelOil2;
		Real64 Coal;
		Real64 Gasoline;
		Real64 Propane;
		Real64 Diesel;
		Real64 OtherFuel1;
		Real64 OtherFuel2;
		Real64 ElecPurch;
		Real64 ElecSold;
		//Facility Meter Indexes
		int ElecFacilityIndex;
		int DieselFacilityIndex;
		int PurchCoolFacilityIndex;
		int PurchHeatFacilityIndex;
		int NatGasFacilityIndex;
		int GasolineFacilityIndex;
		int CoalFacilityIndex;
		int FuelOil1FacilityIndex;
		int FuelOil2FacilityIndex;
		int PropaneFacilityIndex;
		int OtherFuel1FacilityIndex;
		int OtherFuel2FacilityIndex;
		int ElecProducedFacilityIndex;
		int SteamFacilityIndex;
		int ElecPurchasedFacilityIndex;
		int ElecSurplusSoldFacilityIndex;
		//Facility Meter Values used in Pollution Calcs
		Real64 ElecFacility;
		Real64 DieselFacility;
		Real64 PurchCoolFacility;
		Real64 PurchHeatFacility;
		Real64 NatGasFacility;
		Real64 GasolineFacility;
		Real64 CoalFacility;
		Real64 FuelOil1Facility;
		Real64 FuelOil2Facility;
		Real64 PropaneFacility;
		Real64 OtherFuel1Facility;
		Real64 OtherFuel2Facility;
		Real64 ElecProducedFacility;
		Real64 SteamFacility;
		Real64 ElecPurchasedFacility;
		Real64 ElecSurplusSoldFacility;

		// Default Constructor
		FuelTypeProps() :
			FuelTypeNames( {1,PollFactorNumTypes} ),
			Elec( 0.0 ),
			NatGas( 0.0 ),
			FuelOil1( 0.0 ),
			FuelOil2( 0.0 ),
			Coal( 0.0 ),
			Gasoline( 0.0 ),
			Propane( 0.0 ),
			Diesel( 0.0 ),
			OtherFuel1( 0.0 ),
			OtherFuel2( 0.0 ),
			ElecPurch( 0.0 ),
			ElecSold( 0.0 ),
			ElecFacilityIndex( 0 ),
			DieselFacilityIndex( 0 ),
			PurchCoolFacilityIndex( 0 ),
			PurchHeatFacilityIndex( 0 ),
			NatGasFacilityIndex( 0 ),
			GasolineFacilityIndex( 0 ),
			CoalFacilityIndex( 0 ),
			FuelOil1FacilityIndex( 0 ),
			FuelOil2FacilityIndex( 0 ),
			PropaneFacilityIndex( 0 ),
			OtherFuel1FacilityIndex( 0 ),
			OtherFuel2FacilityIndex( 0 ),
			ElecProducedFacilityIndex( 0 ),
			SteamFacilityIndex( 0 ),
			ElecPurchasedFacilityIndex( 0 ),
			ElecSurplusSoldFacilityIndex( 0 ),
			ElecFacility( 0.0 ),
			DieselFacility( 0.0 ),
			PurchCoolFacility( 0.0 ),
			PurchHeatFacility( 0.0 ),
			NatGasFacility( 0.0 ),
			GasolineFacility( 0.0 ),
			CoalFacility( 0.0 ),
			FuelOil1Facility( 0.0 ),
			FuelOil2Facility( 0.0 ),
			PropaneFacility( 0.0 ),
			OtherFuel1Facility( 0.0 ),
			OtherFuel2Facility( 0.0 ),
			ElecProducedFacility( 0.0 ),
			SteamFacility( 0.0 ),
			ElecPurchasedFacility( 0.0 ),
			ElecSurplusSoldFacility( 0.0 )
		{}

		// Member Constructor
		FuelTypeProps(
			FArray1_string const & FuelTypeNames,
			Real64 const Elec,
			Real64 const NatGas,
			Real64 const FuelOil1,
			Real64 const FuelOil2,
			Real64 const Coal,
			Real64 const Gasoline,
			Real64 const Propane,
			Real64 const Diesel,
			Real64 const OtherFuel1,
			Real64 const OtherFuel2,
			Real64 const ElecPurch,
			Real64 const ElecSold,
			int const ElecFacilityIndex,
			int const DieselFacilityIndex,
			int const PurchCoolFacilityIndex,
			int const PurchHeatFacilityIndex,
			int const NatGasFacilityIndex,
			int const GasolineFacilityIndex,
			int const CoalFacilityIndex,
			int const FuelOil1FacilityIndex,
			int const FuelOil2FacilityIndex,
			int const PropaneFacilityIndex,
			int const OtherFuel1FacilityIndex,
			int const OtherFuel2FacilityIndex,
			int const ElecProducedFacilityIndex,
			int const SteamFacilityIndex,
			int const ElecPurchasedFacilityIndex,
			int const ElecSurplusSoldFacilityIndex,
			Real64 const ElecFacility,
			Real64 const DieselFacility,
			Real64 const PurchCoolFacility,
			Real64 const PurchHeatFacility,
			Real64 const NatGasFacility,
			Real64 const GasolineFacility,
			Real64 const CoalFacility,
			Real64 const FuelOil1Facility,
			Real64 const FuelOil2Facility,
			Real64 const PropaneFacility,
			Real64 const OtherFuel1Facility,
			Real64 const OtherFuel2Facility,
			Real64 const ElecProducedFacility,
			Real64 const SteamFacility,
			Real64 const ElecPurchasedFacility,
			Real64 const ElecSurplusSoldFacility
		) :
			FuelTypeNames( {1,PollFactorNumTypes}, FuelTypeNames ),
			Elec( Elec ),
			NatGas( NatGas ),
			FuelOil1( FuelOil1 ),
			FuelOil2( FuelOil2 ),
			Coal( Coal ),
			Gasoline( Gasoline ),
			Propane( Propane ),
			Diesel( Diesel ),
			OtherFuel1( OtherFuel1 ),
			OtherFuel2( OtherFuel2 ),
			ElecPurch( ElecPurch ),
			ElecSold( ElecSold ),
			ElecFacilityIndex( ElecFacilityIndex ),
			DieselFacilityIndex( DieselFacilityIndex ),
			PurchCoolFacilityIndex( PurchCoolFacilityIndex ),
			PurchHeatFacilityIndex( PurchHeatFacilityIndex ),
			NatGasFacilityIndex( NatGasFacilityIndex ),
			GasolineFacilityIndex( GasolineFacilityIndex ),
			CoalFacilityIndex( CoalFacilityIndex ),
			FuelOil1FacilityIndex( FuelOil1FacilityIndex ),
			FuelOil2FacilityIndex( FuelOil2FacilityIndex ),
			PropaneFacilityIndex( PropaneFacilityIndex ),
			OtherFuel1FacilityIndex( OtherFuel1FacilityIndex ),
			OtherFuel2FacilityIndex( OtherFuel2FacilityIndex ),
			ElecProducedFacilityIndex( ElecProducedFacilityIndex ),
			SteamFacilityIndex( SteamFacilityIndex ),
			ElecPurchasedFacilityIndex( ElecPurchasedFacilityIndex ),
			ElecSurplusSoldFacilityIndex( ElecSurplusSoldFacilityIndex ),
			ElecFacility( ElecFacility ),
			DieselFacility( DieselFacility ),
			PurchCoolFacility( PurchCoolFacility ),
			PurchHeatFacility( PurchHeatFacility ),
			NatGasFacility( NatGasFacility ),
			GasolineFacility( GasolineFacility ),
			CoalFacility( CoalFacility ),
			FuelOil1Facility( FuelOil1Facility ),
			FuelOil2Facility( FuelOil2Facility ),
			PropaneFacility( PropaneFacility ),
			OtherFuel1Facility( OtherFuel1Facility ),
			OtherFuel2Facility( OtherFuel2Facility ),
			ElecProducedFacility( ElecProducedFacility ),
			SteamFacility( SteamFacility ),
			ElecPurchasedFacility( ElecPurchasedFacility ),
			ElecSurplusSoldFacility( ElecSurplusSoldFacility )
		{}

	};

	// Object Data
	extern PollutionProps Pollution;
	extern FuelTypeProps FuelType;

	// Functions

	void
	CalculatePollution();

	// Get Input Section of the Module
	//******************************************************************************

	void
	SetupPollutionCalculations();

	void
	GetPollutionFactorInput();

	void
	SetupPollutionMeterReporting();

	void
	CheckPollutionMeterReporting();

	void
	CheckFFSchedule(
		std::string const & currentModuleObject, // the module Object
		std::string const & resourceType, // resource type (Natural Gas, etc)
		std::string const & fieldName, // Actual field name
		std::string const & ScheduleName, // Schedule Name as input
		int & SchedulePtr, // Schedule Index
		bool & ErrorsFound // true if errors found
	);

	// End of Get Input subroutines for the Pollution Module
	//******************************************************************************

	void
	CalcPollution();

	void
	ReadEnergyMeters();

	// *****************************************************************************
	// Utility Routines to allow access to data inside this module.
	// *****************************************************************************

	void
	GetFuelFactorInfo(
		std::string const & fuelName, // input fuel name  (standard from Tabular reports)
		bool & fuelFactorUsed, // return value true if user has entered this fuel
		Real64 & fuelSourceFactor, // if used, the source factor
		bool & fuelFactorScheduleUsed, // if true, schedules for this fuel are used
		int & ffScheduleIndex // if schedules for this fuel are used, return schedule index
	);

	void
	GetEnvironmentalImpactFactorInfo(
		Real64 & efficiencyDistrictHeating, // if entered, the efficiency of District Heating
		Real64 & efficiencyDistrictCooling, // if entered, the efficiency of District Cooling
		Real64 & sourceFactorSteam // if entered, the source factor for Steam
	);

	// *****************************************************************************
	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // PollutionModule

} // EnergyPlus

#endif
