#ifndef ThermalComfort_hh_INCLUDED
#define ThermalComfort_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ThermalComfort {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern Real64 const TAbsConv; // Converter for absolute temperature
	extern Real64 const ActLevelConv; // Converter for activity level (1Met = 58.2 W/m2)
	extern Real64 const BodySurfArea; // Dubois body surface area of the human body (m2)
	extern Real64 const RadSurfEff; // Fraction of surface effective for radiation
	extern Real64 const StefanBoltz; // Stefan-Boltzmann constant (W/m2K4)

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern Real64 AbsAirTemp; // Absolute air temperature; K
	extern Real64 AbsCloSurfTemp; // Absolute clothing surface temperature; K
	extern Real64 AbsRadTemp; // Absolute radiant temperature; K
	extern Real64 AcclPattern; // The pattern of acclimation
	extern Real64 ActLevel; // Metabolic rate; w/m2
	extern Real64 AirVel; // Air velocity; m/s
	extern Real64 AirTemp; // Air temperature; C
	extern Real64 CloBodyRat; // Ratio of clothed body
	extern Real64 CloInsul; // Clothing insulation
	extern Real64 CloPermeatEff; // Clothing permeation efficiency
	extern Real64 CloSurfTemp; // Clothing surface temperature; K
	extern Real64 CloThermEff; // The Burton thermal efficiency factor for clothing
	extern Real64 CloUnit; // Clothing unit; CLO
	extern Real64 ConvHeatLoss; // Convective heat loss
	extern Real64 CoreTempChange; // Temperature change of core in 1 minute
	extern Real64 CoreTemp; // Body core temperature
	extern Real64 CoreTempNeut; // Body core temperature of neutral state
	extern Real64 CoreThermCap; // Thermal capacity of core
	extern Real64 DryHeatLoss; // Heat loss from clothing surface due to both convection and radiation
	extern Real64 DryRespHeatLoss; // Dry respiration heat loss
	extern Real64 EvapHeatLoss; // Evaporative heat loss from skin
	extern Real64 EvapHeatLossDiff; // Evaporative heat loss due to moisture diffusion through skin
	extern Real64 EvapHeatLossMax; // Maximum evaporative heat loss
	extern Real64 EvapHeatLossRegComf; // Evaporative heat loss due to regulatory sweating at the state of comfort
	extern Real64 EvapHeatLossRegSweat; // Evaporative heat loss from regulatory sweating
	extern Real64 EvapHeatLossSweat; // Evaporative heat loss from the sweat secreted
	extern Real64 EvapHeatLossSweatPrev; // Old value of evaporative heat loss from the sweat secreted (KSU)
	extern Real64 H; // Combined heat transfer coefficient
	extern Real64 Hc; // Convective heat transfer coeffiency
	extern Real64 HcFor; // Convective heat transfer coeffiency - Forced
	extern Real64 HcNat; // Convective heat transfer coeffiency - Natural
	extern Real64 HeatFlow; // Heat flow from core to skin
	extern Real64 Hr; // Radiant heat transfer coeffiency
	extern Real64 IntHeatProd; // Internal heat production
	extern int IterNum; // Number of iteration
	extern Real64 LatRespHeatLoss; // Latent respiration heat loss
	extern int MaxZoneNum; // Number of zones
	extern int MRTCalcType; // The type of MRT calculation (ZoneAveraged or SurfaceWeighted)
	extern Real64 OpTemp; // Operative temperature
	extern int PeopleNum; // People number
	extern Real64 RadHeatLoss; // Radiant heat loss
	extern Real64 RadTemp; // Radiant temperature; C
	extern Real64 RelHum; // Relative humidity; Fraction
	extern Real64 RespHeatLoss; // The rate of respiratory heat loss
	extern Real64 SatSkinVapPress; // Saturated vapor pressure at skin temperature
	extern Real64 ShivResponse; // Metalbolic heat production due to shivering
	extern Real64 SkinComfTemp; // Skin temperature required to achieve thermal comfort; C
	extern Real64 SkinComfVPress; // Saturated water vapor pressure at required skin temperature; Torr
	extern Real64 SkinTemp; // Skin temperature
	extern Real64 SkinTempChange; // Temperature change of skin in 1 minute
	extern Real64 SkinTempNeut; // Skin temperature at neutral state
	extern Real64 SkinThermCap; // Thermal capacity of Skin
	extern Real64 SkinWetDiff; // Skin wettedness for nonsweating portion of skin
	extern Real64 SkinWetSweat; // Skin wettedness required to evaporate regulatory sweat
	extern Real64 SkinWetTot; // Total skin wettedness
	extern Real64 SkinVapPress; // Vapor pressure at skin
	extern Real64 SurfaceTemp; // Surface temperature when MRTType is 'SurfaceWeighted'
	extern Real64 ThermCndct; // Thermal conductance of skin
	extern Real64 ThermSensTransCoef; // Theraml sensation coefficient for PMV
	extern Real64 Time; // Time, hr
	extern Real64 TimeChange; // Change of time, hr
	extern Real64 VapPress; // Vapor pressure; Torr  ?? BG Oct 2005 humm, this should be kPa
	extern Real64 VasoconstrictFac; // Constriction factor of blood vessel
	extern Real64 VasodilationFac; // Dilation factor of blood vessel
	extern Real64 WorkEff; // Energy cosumption by external work; w/m2
	extern int ZoneNum; // Zone number
	extern Real64 TemporarySixAMTemperature; // Temperature at 6am

	//time that any zone is not comfortable based on simple ASHRAE 55 using summer clothes
	extern Real64 AnyZoneTimeNotSimpleASH55Summer;
	//time that any zone is not comfortable based on simple ASHRAE 55 using winter clothes
	extern Real64 AnyZoneTimeNotSimpleASH55Winter;
	//time that any zone is not comfortable based on simple ASHRAE 55 using summer or winter clothes
	extern Real64 AnyZoneTimeNotSimpleASH55Either;

	//time that any zone has unmet met loads
	extern Real64 AnyZoneNotMetHeating;
	extern Real64 AnyZoneNotMetCooling;
	extern Real64 AnyZoneNotMetHeatingOccupied;
	extern Real64 AnyZoneNotMetCoolingOccupied;
	extern Real64 AnyZoneNotMetOccupied;
	//total time from beginning of simulation AnyZoneTimeNotSimpleASH55
	extern Real64 TotalAnyZoneTimeNotSimpleASH55Summer;
	extern Real64 TotalAnyZoneTimeNotSimpleASH55Winter;
	extern Real64 TotalAnyZoneTimeNotSimpleASH55Either;
	//total time from beginning of simulation any zone not met
	extern Real64 TotalAnyZoneNotMetHeating;
	extern Real64 TotalAnyZoneNotMetCooling;
	extern Real64 TotalAnyZoneNotMetHeatingOccupied;
	extern Real64 TotalAnyZoneNotMetCoolingOccupied;
	extern Real64 TotalAnyZoneNotMetOccupied;
	extern Array1D< Real64 > ZoneOccHrs;

	// Subroutine Specifications for the Thermal Comfort module

	// Types

	struct ThermalComfortDataType
	{
		// Members
		Real64 FangerPMV;
		Real64 FangerPPD;
		Real64 CloSurfTemp; // clothing surface temp from iteration in FANGER calcs
		Real64 PiercePMVET;
		Real64 PiercePMVSET;
		Real64 PierceDISC;
		Real64 PierceTSENS;
		Real64 KsuTSV;
		Real64 ThermalComfortMRT;
		Real64 ThermalComfortOpTemp;
		Real64 ClothingValue;
		int ThermalComfortAdaptiveASH5590;
		int ThermalComfortAdaptiveASH5580;
		int ThermalComfortAdaptiveCEN15251CatI;
		int ThermalComfortAdaptiveCEN15251CatII;
		int ThermalComfortAdaptiveCEN15251CatIII;
		Real64 TComfASH55;
		Real64 TComfCEN15251;
		Real64 ASHRAE55RunningMeanOutdoorTemp;
		Real64 CEN15251RunningMeanOutdoorTemp;

		// Default Constructor
		ThermalComfortDataType() :
			FangerPMV( 0.0 ),
			FangerPPD( 0.0 ),
			CloSurfTemp( 0.0 ),
			PiercePMVET( 0.0 ),
			PiercePMVSET( 0.0 ),
			PierceDISC( 0.0 ),
			PierceTSENS( 0.0 ),
			KsuTSV( 0.0 ),
			ThermalComfortMRT( 0.0 ),
			ThermalComfortOpTemp( 0.0 ),
			ClothingValue( 0.0 ),
			ThermalComfortAdaptiveASH5590( 0 ),
			ThermalComfortAdaptiveASH5580( 0 ),
			ThermalComfortAdaptiveCEN15251CatI( 0 ),
			ThermalComfortAdaptiveCEN15251CatII( 0 ),
			ThermalComfortAdaptiveCEN15251CatIII( 0 ),
			TComfASH55( 0.0 ),
			TComfCEN15251( 0.0 ),
			ASHRAE55RunningMeanOutdoorTemp( 0.0 ),
			CEN15251RunningMeanOutdoorTemp( 0.0 )
		{}

		// Member Constructor
		ThermalComfortDataType(
			Real64 const FangerPMV,
			Real64 const FangerPPD,
			Real64 const CloSurfTemp, // clothing surface temp from iteration in FANGER calcs
			Real64 const PiercePMVET,
			Real64 const PiercePMVSET,
			Real64 const PierceDISC,
			Real64 const PierceTSENS,
			Real64 const KsuTSV,
			Real64 const ThermalComfortMRT,
			Real64 const ThermalComfortOpTemp,
			Real64 const ClothingValue,
			int const ThermalComfortAdaptiveASH5590,
			int const ThermalComfortAdaptiveASH5580,
			int const ThermalComfortAdaptiveCEN15251CatI,
			int const ThermalComfortAdaptiveCEN15251CatII,
			int const ThermalComfortAdaptiveCEN15251CatIII,
			Real64 const TComfASH55,
			Real64 const TComfCEN15251,
			Real64 const ASHRAE55RunningMeanOutdoorTemp,
			Real64 const CEN15251RunningMeanOutdoorTemp
		) :
			FangerPMV( FangerPMV ),
			FangerPPD( FangerPPD ),
			CloSurfTemp( CloSurfTemp ),
			PiercePMVET( PiercePMVET ),
			PiercePMVSET( PiercePMVSET ),
			PierceDISC( PierceDISC ),
			PierceTSENS( PierceTSENS ),
			KsuTSV( KsuTSV ),
			ThermalComfortMRT( ThermalComfortMRT ),
			ThermalComfortOpTemp( ThermalComfortOpTemp ),
			ClothingValue( ClothingValue ),
			ThermalComfortAdaptiveASH5590( ThermalComfortAdaptiveASH5590 ),
			ThermalComfortAdaptiveASH5580( ThermalComfortAdaptiveASH5580 ),
			ThermalComfortAdaptiveCEN15251CatI( ThermalComfortAdaptiveCEN15251CatI ),
			ThermalComfortAdaptiveCEN15251CatII( ThermalComfortAdaptiveCEN15251CatII ),
			ThermalComfortAdaptiveCEN15251CatIII( ThermalComfortAdaptiveCEN15251CatIII ),
			TComfASH55( TComfASH55 ),
			TComfCEN15251( TComfCEN15251 ),
			ASHRAE55RunningMeanOutdoorTemp( ASHRAE55RunningMeanOutdoorTemp ),
			CEN15251RunningMeanOutdoorTemp( CEN15251RunningMeanOutdoorTemp )
		{}

	};

	struct ThermalComfortInASH55Type
	{
		// Members
		// for debugging
		//REAL(r64)    :: dCurAirTemp
		//REAL(r64)    :: dCurMeanRadiantTemp
		//REAL(r64)    :: dOperTemp
		//REAL(r64)    :: dHumidRatio
		Real64 timeNotSummer; // time when not in summer comfort range based on ASHRAE 55 simplified
		Real64 timeNotWinter; // time when not in winter comfort range based on ASHRAE 55 simplified
		Real64 timeNotEither; // time when  not in summer or winter comfort range based on ASHRAE 55 simplified
		Real64 totalTimeNotSummer; // sum for simulation for summer
		Real64 totalTimeNotWinter; // sum for simulation for winter
		Real64 totalTimeNotEither; // sum for simulation for either
		bool ZoneIsOccupied; // flag if zone has people
		int warningIndex; // variable to store pointer to the recurring warning
		int warningIndex2; // variable to store pointer to the recurring warning
		bool Enable55Warning; // flag if the warning should be able to be shown if appropriate

		// Default Constructor
		ThermalComfortInASH55Type() :
			timeNotSummer( 0.0 ),
			timeNotWinter( 0.0 ),
			timeNotEither( 0.0 ),
			totalTimeNotSummer( 0.0 ),
			totalTimeNotWinter( 0.0 ),
			totalTimeNotEither( 0.0 ),
			ZoneIsOccupied( false ),
			warningIndex( 0 ),
			warningIndex2( 0 ),
			Enable55Warning( false )
		{}

		// Member Constructor
		ThermalComfortInASH55Type(
			Real64 const timeNotSummer, // time when not in summer comfort range based on ASHRAE 55 simplified
			Real64 const timeNotWinter, // time when not in winter comfort range based on ASHRAE 55 simplified
			Real64 const timeNotEither, // time when  not in summer or winter comfort range based on ASHRAE 55 simplified
			Real64 const totalTimeNotSummer, // sum for simulation for summer
			Real64 const totalTimeNotWinter, // sum for simulation for winter
			Real64 const totalTimeNotEither, // sum for simulation for either
			bool const ZoneIsOccupied, // flag if zone has people
			int const warningIndex, // variable to store pointer to the recurring warning
			int const warningIndex2, // variable to store pointer to the recurring warning
			bool const Enable55Warning // flag if the warning should be able to be shown if appropriate
		) :
			timeNotSummer( timeNotSummer ),
			timeNotWinter( timeNotWinter ),
			timeNotEither( timeNotEither ),
			totalTimeNotSummer( totalTimeNotSummer ),
			totalTimeNotWinter( totalTimeNotWinter ),
			totalTimeNotEither( totalTimeNotEither ),
			ZoneIsOccupied( ZoneIsOccupied ),
			warningIndex( warningIndex ),
			warningIndex2( warningIndex2 ),
			Enable55Warning( Enable55Warning )
		{}

	};

	struct ThermalComfortSetPointType
	{
		// Members
		Real64 notMetHeating;
		Real64 notMetCooling;
		Real64 notMetHeatingOccupied;
		Real64 notMetCoolingOccupied;
		Real64 totalNotMetHeating;
		Real64 totalNotMetCooling;
		Real64 totalNotMetHeatingOccupied;
		Real64 totalNotMetCoolingOccupied;

		// Default Constructor
		ThermalComfortSetPointType() :
			notMetHeating( 0.0 ),
			notMetCooling( 0.0 ),
			notMetHeatingOccupied( 0.0 ),
			notMetCoolingOccupied( 0.0 ),
			totalNotMetHeating( 0.0 ),
			totalNotMetCooling( 0.0 ),
			totalNotMetHeatingOccupied( 0.0 ),
			totalNotMetCoolingOccupied( 0.0 )
		{}

		// Member Constructor
		ThermalComfortSetPointType(
			Real64 const notMetHeating,
			Real64 const notMetCooling,
			Real64 const notMetHeatingOccupied,
			Real64 const notMetCoolingOccupied,
			Real64 const totalNotMetHeating,
			Real64 const totalNotMetCooling,
			Real64 const totalNotMetHeatingOccupied,
			Real64 const totalNotMetCoolingOccupied
		) :
			notMetHeating( notMetHeating ),
			notMetCooling( notMetCooling ),
			notMetHeatingOccupied( notMetHeatingOccupied ),
			notMetCoolingOccupied( notMetCoolingOccupied ),
			totalNotMetHeating( totalNotMetHeating ),
			totalNotMetCooling( totalNotMetCooling ),
			totalNotMetHeatingOccupied( totalNotMetHeatingOccupied ),
			totalNotMetCoolingOccupied( totalNotMetCoolingOccupied )
		{}

	};

	struct AngleFactorData
	{
		// Members
		Array1D< Real64 > AngleFactor; // Angle factor of each surface
		std::string Name; // Angle factor list name
		Array1D_string SurfaceName; // Names of the Surfces
		Array1D_int SurfacePtr; // ALLOCATABLE to the names of the Surfces
		int TotAngleFacSurfaces; // Total number of surfaces
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type

		// Default Constructor
		AngleFactorData() :
			TotAngleFacSurfaces( 0 ),
			ZonePtr( 0 )
		{}

		// Member Constructor
		AngleFactorData(
			Array1< Real64 > const & AngleFactor, // Angle factor of each surface
			std::string const & Name, // Angle factor list name
			Array1_string const & SurfaceName, // Names of the Surfces
			Array1_int const & SurfacePtr, // ALLOCATABLE to the names of the Surfces
			int const TotAngleFacSurfaces, // Total number of surfaces
			std::string const & ZoneName, // Name of zone the system is serving
			int const ZonePtr // Point to this zone in the Zone derived type
		) :
			AngleFactor( AngleFactor ),
			Name( Name ),
			SurfaceName( SurfaceName ),
			SurfacePtr( SurfacePtr ),
			TotAngleFacSurfaces( TotAngleFacSurfaces ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr )
		{}

	};

	// Object Data
	extern Array1D< ThermalComfortInASH55Type > ThermalComfortInASH55;
	extern Array1D< ThermalComfortSetPointType > ThermalComfortSetPoint;
	extern Array1D< ThermalComfortDataType > ThermalComfortData;
	extern Array1D< AngleFactorData > AngleFactorList; // Angle Factor List data for each Angle Factor List

	// Functions

	void
	ManageThermalComfort( bool const InitializeOnly ); // when called from ZTPC and calculations aren't needed

	void
	InitThermalComfort();

	void
	CalcThermalComfortFanger(
		Optional_int_const PNum = _, // People number for thermal comfort control
		Optional< Real64 const > Tset = _, // Temperature setpoint for thermal comfort control
		Optional< Real64 > PMVResult = _ // PMV value for thermal comfort control
	);

	void
	CalcThermalComfortPierce();

	void
	CalcThermalComfortKSU();

	void
	DERIV(
		int & TempIndiceNum, // Number of temperature indices  unused1208
		Array1A< Real64 > Temp, // Temperature unused1208
		Array1A< Real64 > TempChange // Change of temperature
	);

	void
	RKG(
		int & NEQ,
		Real64 & H,
		Real64 & X,
		Array1A< Real64 > Y,
		Array1A< Real64 > DY,
		Array1A< Real64 > C
	);

	void
	GetAngleFactorList();

	Real64
	CalcAngleFactorMRT( int const AngleFacNum );

	Real64
	CalcSatVapPressFromTemp( Real64 const Temp );

	Real64
	CalcRadTemp( int const PeopleListNum ); // Type of MRT calculation (zone averaged or surface weighted)

	void
	CalcThermalComfortSimpleASH55();

	void
	CalcIfSetPointMet();

	void
	CalcThermalComfortAdaptiveASH55(
		bool const initiate, // true if supposed to initiate
		Optional_bool_const wthrsim = _, // true if this is a weather simulation
		Optional< Real64 const > avgdrybulb = _ // approximate avg drybulb for design day.  will be used as previous period in design day
	);

	void
	CalcThermalComfortAdaptiveCEN15251(
		bool const initiate, // true if supposed to initiate
		Optional_bool_const wthrsim = _, // true if this is a weather simulation
		Optional< Real64 const > avgdrybulb = _ // approximate avg drybulb for design day.  will be used as previous period in design day
	);

	void
	DynamicClothingModel();

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

} // ThermalComfort

} // EnergyPlus

#endif
