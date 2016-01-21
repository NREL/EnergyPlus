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

	};

	// Object Data
	extern Array1D< ThermalComfortInASH55Type > ThermalComfortInASH55;
	extern Array1D< ThermalComfortSetPointType > ThermalComfortSetPoint;
	extern Array1D< ThermalComfortDataType > ThermalComfortData;
	extern Array1D< AngleFactorData > AngleFactorList; // Angle Factor List data for each Angle Factor List

	// Functions

	void
	clear_state();

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
	ResetThermalComfortSimpleASH55();

	void
	CalcIfSetPointMet();

	void
	ResetSetPointMet();

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

} // ThermalComfort

} // EnergyPlus

#endif
