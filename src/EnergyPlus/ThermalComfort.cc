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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <ThermalComfort.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataStringGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ThermalComfort {

	// Module containing the routines dealing with the CalcThermalComfortFanger,
	// CalcThermalComfortPierce, and CalcThermalComfortKSU

	// MODULE INFORMATION:
	//       AUTHOR         Jaewook Lee
	//       DATE WRITTEN   January 2000
	//       MODIFIED       Rick Strand (for E+ implementation February 2000)
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To calculate thermal comfort indices based on the
	// three thermal comfort prediction models (Fanger, Pierce, KSU)

	// METHODOLOGY EMPLOYED:
	// For each thermal comfort model type, the subroutines will loop through
	// the people statements and perform the requested thermal comfort evaluations

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using DataHeatBalance::MRT;
	using DataHeatBalance::People;
	using DataHeatBalance::PeopleData;
	using DataHeatBalance::Zone;
	using DataHeatBalance::ZoneAveraged;
	using DataHeatBalance::SurfaceWeighted;
	using DataHeatBalance::AngleFactor;
	using DataHeatBalance::TotPeople;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutDryBulbTemp;
	using DataHeatBalFanSys::MAT;
	using DataHeatBalFanSys::ZTAV;
	using DataHeatBalFanSys::ZoneAirHumRat;
	using DataHeatBalFanSys::ZoneComfortControlsFanger;
	using DataHeatBalFanSys::ZoneAirHumRatAvg;
	using DataHeatBalFanSys::ZTAVComf;
	using DataHeatBalFanSys::ZoneAirHumRatAvgComf;
	using ScheduleManager::GetCurrentScheduleValue;
	using DataRoomAirModel::IsZoneDV;
	using DataRoomAirModel::TCMF;
	using DataRoomAirModel::IsZoneCV;
	using DataRoomAirModel::ZTREC;
	using DataRoomAirModel::ZTJET;
	using DataRoomAirModel::Ujet;
	using DataRoomAirModel::Urec;
	using DataRoomAirModel::ZoneUCSDCV;
	using DataRoomAirModel::IsZoneUI;
	using DataRoomAirModel::VComfort_Jet;
	using DataRoomAirModel::VComfort_Recirculation;

	//Use statements for access to subroutines in other modules
	using Psychrometrics::PsyRhFnTdbWPb;

	namespace {
		// clear_state variables
		bool FirstTimeFlag( true ); // Flag set to make sure you get input once
	}

	// Data
	// MODULE PARAMETER DEFINITIONS
	Real64 const TAbsConv( KelvinConv ); // Converter for absolute temperature
	Real64 const ActLevelConv( 58.2 ); // Converter for activity level (1Met = 58.2 W/m2)
	Real64 const BodySurfArea( 1.8 ); // Dubois body surface area of the human body (m2)
	Real64 const RadSurfEff( 0.72 ); // Fraction of surface effective for radiation
	Real64 const StefanBoltz( 5.67e-8 ); // Stefan-Boltzmann constant (W/m2K4)

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	Real64 AbsAirTemp( 0.0 ); // Absolute air temperature; K
	Real64 AbsCloSurfTemp( 0.0 ); // Absolute clothing surface temperature; K
	Real64 AbsRadTemp( 0.0 ); // Absolute radiant temperature; K
	Real64 AcclPattern( 0.0 ); // The pattern of acclimation
	Real64 ActLevel( 0.0 ); // Metabolic rate; w/m2
	Real64 AirVel( 0.0 ); // Air velocity; m/s
	Real64 AirTemp( 0.0 ); // Air temperature; C
	Real64 CloBodyRat( 0.0 ); // Ratio of clothed body
	Real64 CloInsul( 0.0 ); // Clothing insulation
	Real64 CloPermeatEff( 0.0 ); // Clothing permeation efficiency
	Real64 CloSurfTemp( 0.0 ); // Clothing surface temperature; K
	Real64 CloThermEff( 0.0 ); // The Burton thermal efficiency factor for clothing
	Real64 CloUnit( 0.0 ); // Clothing unit; CLO
	Real64 ConvHeatLoss( 0.0 ); // Convective heat loss
	Real64 CoreTempChange( 0.0 ); // Temperature change of core in 1 minute
	Real64 CoreTemp( 0.0 ); // Body core temperature
	Real64 CoreTempNeut( 0.0 ); // Body core temperature of neutral state
	Real64 CoreThermCap( 0.0 ); // Thermal capacity of core
	Real64 DryHeatLoss( 0.0 ); // Heat loss from clothing surface due to both convection and radiation
	Real64 DryRespHeatLoss( 0.0 ); // Dry respiration heat loss
	Real64 EvapHeatLoss( 0.0 ); // Evaporative heat loss from skin
	Real64 EvapHeatLossDiff( 0.0 ); // Evaporative heat loss due to moisture diffusion through skin
	Real64 EvapHeatLossMax( 0.0 ); // Maximum evaporative heat loss
	Real64 EvapHeatLossRegComf( 0.0 ); // Evaporative heat loss due to regulatory sweating at the state of comfort
	Real64 EvapHeatLossRegSweat( 0.0 ); // Evaporative heat loss from regulatory sweating
	Real64 EvapHeatLossSweat( 0.0 ); // Evaporative heat loss from the sweat secreted
	Real64 EvapHeatLossSweatPrev( 0.0 ); // Old value of evaporative heat loss from the sweat secreted (KSU)
	Real64 H( 0.0 ); // Combined heat transfer coefficient
	Real64 Hc( 0.0 ); // Convective heat transfer coeffiency
	Real64 HcFor( 0.0 ); // Convective heat transfer coeffiency - Forced
	Real64 HcNat( 0.0 ); // Convective heat transfer coeffiency - Natural
	Real64 HeatFlow( 0.0 ); // Heat flow from core to skin
	Real64 Hr( 0.0 ); // Radiant heat transfer coeffiency
	Real64 IntHeatProd( 0.0 ); // Internal heat production
	int IterNum( 0 ); // Number of iteration
	Real64 LatRespHeatLoss( 0.0 ); // Latent respiration heat loss
	int MaxZoneNum( 0 ); // Number of zones
	int MRTCalcType( 0 ); // The type of MRT calculation (ZoneAveraged or SurfaceWeighted)
	Real64 OpTemp( 0.0 ); // Operative temperature
	int PeopleNum( 0 ); // People number
	Real64 RadHeatLoss( 0.0 ); // Radiant heat loss
	Real64 RadTemp( 0.0 ); // Radiant temperature; C
	Real64 RelHum( 0.0 ); // Relative humidity; Fraction
	Real64 RespHeatLoss( 0.0 ); // The rate of respiratory heat loss
	Real64 SatSkinVapPress( 0.0 ); // Saturated vapor pressure at skin temperature
	Real64 ShivResponse( 0.0 ); // Metalbolic heat production due to shivering
	Real64 SkinComfTemp( 0.0 ); // Skin temperature required to achieve thermal comfort; C
	Real64 SkinComfVPress( 0.0 ); // Saturated water vapor pressure at required skin temperature; Torr
	Real64 SkinTemp( 0.0 ); // Skin temperature
	Real64 SkinTempChange( 0.0 ); // Temperature change of skin in 1 minute
	Real64 SkinTempNeut( 0.0 ); // Skin temperature at neutral state
	Real64 SkinThermCap( 0.0 ); // Thermal capacity of Skin
	Real64 SkinWetDiff( 0.0 ); // Skin wettedness for nonsweating portion of skin
	Real64 SkinWetSweat( 0.0 ); // Skin wettedness required to evaporate regulatory sweat
	Real64 SkinWetTot( 0.0 ); // Total skin wettedness
	Real64 SkinVapPress( 0.0 ); // Vapor pressure at skin
	Real64 SurfaceTemp( 0.0 ); // Surface temperature when MRTType is 'SurfaceWeighted'
	Real64 ThermCndct( 0.0 ); // Thermal conductance of skin
	Real64 ThermSensTransCoef( 0.0 ); // Theraml sensation coefficient for PMV
	Real64 Time( 0.0 ); // Time, hr
	Real64 TimeChange( 0.0 ); // Change of time, hr
	Real64 VapPress( 0.0 ); // Vapor pressure; Torr  ?? BG Oct 2005 humm, this should be kPa
	Real64 VasoconstrictFac( 0.0 ); // Constriction factor of blood vessel
	Real64 VasodilationFac( 0.0 ); // Dilation factor of blood vessel
	Real64 WorkEff( 0.0 ); // Energy cosumption by external work; w/m2
	int ZoneNum( 0 ); // Zone number
	Real64 TemporarySixAMTemperature( 0.0 ); // Temperature at 6am

	//time that any zone is not comfortable based on simple ASHRAE 55 using summer clothes
	Real64 AnyZoneTimeNotSimpleASH55Summer( 0.0 );
	//time that any zone is not comfortable based on simple ASHRAE 55 using winter clothes
	Real64 AnyZoneTimeNotSimpleASH55Winter( 0.0 );
	//time that any zone is not comfortable based on simple ASHRAE 55 using summer or winter clothes
	Real64 AnyZoneTimeNotSimpleASH55Either( 0.0 );

	//time that any zone has unmet met loads
	Real64 AnyZoneNotMetHeating( 0.0 );
	Real64 AnyZoneNotMetCooling( 0.0 );
	Real64 AnyZoneNotMetHeatingOccupied( 0.0 );
	Real64 AnyZoneNotMetCoolingOccupied( 0.0 );
	Real64 AnyZoneNotMetOccupied( 0.0 );
	//total time from beginning of simulation AnyZoneTimeNotSimpleASH55
	Real64 TotalAnyZoneTimeNotSimpleASH55Summer( 0.0 );
	Real64 TotalAnyZoneTimeNotSimpleASH55Winter( 0.0 );
	Real64 TotalAnyZoneTimeNotSimpleASH55Either( 0.0 );
	//total time from beginning of simulation any zone not met
	Real64 TotalAnyZoneNotMetHeating( 0.0 );
	Real64 TotalAnyZoneNotMetCooling( 0.0 );
	Real64 TotalAnyZoneNotMetHeatingOccupied( 0.0 );
	Real64 TotalAnyZoneNotMetCoolingOccupied( 0.0 );
	Real64 TotalAnyZoneNotMetOccupied( 0.0 );
	Array1D< Real64 > ZoneOccHrs;

	// Subroutine Specifications for the Thermal Comfort module

	// Object Data
	Array1D< ThermalComfortInASH55Type > ThermalComfortInASH55;
	Array1D< ThermalComfortSetPointType > ThermalComfortSetPoint;
	Array1D< ThermalComfortDataType > ThermalComfortData;
	Array1D< AngleFactorData > AngleFactorList; // Angle Factor List data for each Angle Factor List

	// Functions
	void
	clear_state()
	{
		FirstTimeFlag = true;
		AbsAirTemp = 0.0;
		AbsCloSurfTemp = 0.0;
		AbsRadTemp = 0.0;
		AcclPattern = 0.0;
		ActLevel = 0.0;
		AirVel = 0.0;
		AirTemp = 0.0;
		CloBodyRat = 0.0;
		CloInsul = 0.0;
		CloPermeatEff = 0.0;
		CloSurfTemp = 0.0;
		CloThermEff = 0.0;
		CloUnit = 0.0;
		ConvHeatLoss = 0.0;
		CoreTempChange = 0.0;
		CoreTemp = 0.0;
		CoreTempNeut = 0.0;
		CoreThermCap = 0.0;
		DryHeatLoss = 0.0;
		DryRespHeatLoss = 0.0;
		EvapHeatLoss = 0.0;
		EvapHeatLossDiff = 0.0;
		EvapHeatLossMax = 0.0;
		EvapHeatLossRegComf = 0.0;
		EvapHeatLossRegSweat = 0.0;
		EvapHeatLossSweat = 0.0;
		EvapHeatLossSweatPrev = 0.0;
		H = 0.0;
		Hc = 0.0;
		HcFor = 0.0;
		HcNat = 0.0;
		HeatFlow = 0.0;
		Hr = 0.0;
		IntHeatProd = 0.0;
		IterNum = 0;
		LatRespHeatLoss = 0.0;
		MaxZoneNum = 0;
		MRTCalcType = 0;
		OpTemp = 0.0;
		PeopleNum = 0;
		RadHeatLoss = 0.0;
		RadTemp = 0.0;
		RelHum = 0.0;
		RespHeatLoss = 0.0;
		SatSkinVapPress = 0.0;
		ShivResponse = 0.0;
		SkinComfTemp = 0.0;
		SkinComfVPress = 0.0;
		SkinTemp = 0.0;
		SkinTempChange = 0.0;
		SkinTempNeut = 0.0;
		SkinThermCap = 0.0;
		SkinWetDiff = 0.0;
		SkinWetSweat = 0.0;
		SkinWetTot = 0.0;
		SkinVapPress = 0.0;
		SurfaceTemp = 0.0;
		ThermCndct = 0.0;
		ThermSensTransCoef = 0.0;
		Time = 0.0;
		TimeChange = 0.0;
		VapPress = 0.0;
		VasoconstrictFac = 0.0;
		VasodilationFac = 0.0;
		WorkEff = 0.0;
		ZoneNum = 0;
		TemporarySixAMTemperature = 0.0;
		AnyZoneTimeNotSimpleASH55Summer = 0.0;
		AnyZoneTimeNotSimpleASH55Winter = 0.0;
		AnyZoneTimeNotSimpleASH55Either = 0.0;
		AnyZoneNotMetHeating = 0.0;
		AnyZoneNotMetCooling = 0.0;
		AnyZoneNotMetHeatingOccupied = 0.0;
		AnyZoneNotMetCoolingOccupied = 0.0;
		AnyZoneNotMetOccupied = 0.0;
		TotalAnyZoneTimeNotSimpleASH55Summer = 0.0;
		TotalAnyZoneTimeNotSimpleASH55Winter = 0.0;
		TotalAnyZoneTimeNotSimpleASH55Either = 0.0;
		TotalAnyZoneNotMetHeating = 0.0;
		TotalAnyZoneNotMetCooling = 0.0;
		TotalAnyZoneNotMetHeatingOccupied = 0.0;
		TotalAnyZoneNotMetCoolingOccupied = 0.0;
		TotalAnyZoneNotMetOccupied = 0.0;
		ZoneOccHrs.deallocate();
		ThermalComfortInASH55.deallocate();
		ThermalComfortSetPoint.deallocate();
		ThermalComfortData.deallocate();
		AngleFactorList.deallocate();
	}

	void
	ManageThermalComfort( bool const InitializeOnly ) // when called from ZTPC and calculations aren't needed
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Rick Strand
		//     DATE WRITTEN   February 2000
		//     MODIFIED       na
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the various thermal comfort calculations.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus manager methodology.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ASH55Flag( false );
		static bool CEN15251Flag( false );

		// FLOW:
		// No input to get because this is already done by other heat balance routines

		if ( FirstTimeFlag ) {
			InitThermalComfort(); // Mainly sets up output stuff
			FirstTimeFlag = false;
			if ( TotPeople > 0 ) {
				if ( std::any_of( People.begin(), People.end(), []( PeopleData const & e ){ return e.AdaptiveASH55; } ) ) ASH55Flag = true;
				if ( std::any_of( People.begin(), People.end(), []( PeopleData const & e ){ return e.AdaptiveCEN15251; } ) ) CEN15251Flag = true;
			}
		}

		if ( DayOfSim == 1 ) {
			if ( HourOfDay < 7 ) {
				TemporarySixAMTemperature = 1.868132;
			} else if ( HourOfDay == 7 ) {
				if ( TimeStep == 1 ) {
					TemporarySixAMTemperature = OutDryBulbTemp;
				}
			}
		} else {
			if ( HourOfDay == 7 ) {
				if ( TimeStep == 1 ) {
					TemporarySixAMTemperature = OutDryBulbTemp;
				}
			}
		}

		if ( InitializeOnly ) return;

		if ( BeginEnvrnFlag ) {
			ZoneOccHrs = 0.0;
		}

		if ( ! DoingSizing && ! WarmupFlag ) {
			CalcThermalComfortFanger();
			CalcThermalComfortPierce();
			CalcThermalComfortKSU();
			CalcThermalComfortSimpleASH55();
			CalcIfSetPointMet();
			if ( ASH55Flag ) CalcThermalComfortAdaptiveASH55( false );
			if ( CEN15251Flag ) CalcThermalComfortAdaptiveCEN15251( false );
		}

		// No updating needed

		// No other reporting needed

	}

	void
	InitThermalComfort()
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Rick Strand
		//     DATE WRITTEN   February 2000
		//     MODIFIED       na
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine allocates the proper arrays, sets all values to zero,
		// and sets up the output stuff.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus manager methodology.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop; // DO loop counter
		std::string CurrentGroupName;

		// FLOW:

		ThermalComfortData.allocate( TotPeople );

		for ( Loop = 1; Loop <= TotPeople; ++Loop ) {

			CurrentGroupName = People( Loop ).Name;

			// CurrentModuleObject='People'
			if ( People( Loop ).Fanger ) {
				SetupOutputVariable( "Zone Thermal Comfort Fanger Model PMV []", ThermalComfortData( Loop ).FangerPMV, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Fanger Model PPD [%]", ThermalComfortData( Loop ).FangerPPD, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Clothing Surface Temperature [C]", ThermalComfortData( Loop ).CloSurfTemp, "Zone", "State", People( Loop ).Name );
			}

			if ( People( Loop ).Pierce ) {
				SetupOutputVariable( "Zone Thermal Comfort Pierce Model Effective Temperature PMV []", ThermalComfortData( Loop ).PiercePMVET, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Pierce Model Standard Effective Temperature PMV []", ThermalComfortData( Loop ).PiercePMVSET, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Pierce Model Discomfort Index []", ThermalComfortData( Loop ).PierceDISC, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Pierce Model Thermal Sensation Index []", ThermalComfortData( Loop ).PierceTSENS, "Zone", "State", People( Loop ).Name );
			}

			if ( People( Loop ).KSU ) {
				SetupOutputVariable( "Zone Thermal Comfort KSU Model Thermal Sensation Vote []", ThermalComfortData( Loop ).KsuTSV, "Zone", "State", People( Loop ).Name );
			}

			if ( ( People( Loop ).Fanger ) || ( People( Loop ).Pierce ) || ( People( Loop ).KSU ) ) {
				SetupOutputVariable( "Zone Thermal Comfort Mean Radiant Temperature [C]", ThermalComfortData( Loop ).ThermalComfortMRT, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Operative Temperature [C]", ThermalComfortData( Loop ).ThermalComfortOpTemp, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort Clothing Value [clo]", ThermalComfortData( Loop ).ClothingValue, "Zone", "State", People( Loop ).Name );
			}

			if ( People( Loop ).AdaptiveASH55 ) {
				SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Adaptive Model 90% Acceptability Status []", ThermalComfortData( Loop ).ThermalComfortAdaptiveASH5590, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Adaptive Model 80% Acceptability Status []", ThermalComfortData( Loop ).ThermalComfortAdaptiveASH5580, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Adaptive Model Running Average Outdoor Air Temperature [C]", ThermalComfortData( Loop ).ASHRAE55RunningMeanOutdoorTemp, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Adaptive Model Temperature [C]", ThermalComfortData( Loop ).TComfASH55, "Zone", "State", People( Loop ).Name );
			}

			if ( People( Loop ).AdaptiveCEN15251 ) {
				SetupOutputVariable( "Zone Thermal Comfort CEN 15251 Adaptive Model Category I Status []", ThermalComfortData( Loop ).ThermalComfortAdaptiveCEN15251CatI, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort CEN 15251 Adaptive Model Category II Status []", ThermalComfortData( Loop ).ThermalComfortAdaptiveCEN15251CatII, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort CEN 15251 Adaptive Model Category III Status []", ThermalComfortData( Loop ).ThermalComfortAdaptiveCEN15251CatIII, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort CEN 15251 Adaptive Model Running Average Outdoor Air Temperature [C]", ThermalComfortData( Loop ).CEN15251RunningMeanOutdoorTemp, "Zone", "State", People( Loop ).Name );
				SetupOutputVariable( "Zone Thermal Comfort CEN 15251 Adaptive Model Temperature [C]", ThermalComfortData( Loop ).TComfCEN15251, "Zone", "State", People( Loop ).Name );
			}

		}
		ThermalComfortInASH55.allocate( NumOfZones );

		// ASHRAE 55 Warning. If any people statement for a zone is true, set that zone to true
		for ( Loop = 1; Loop <= TotPeople; ++Loop ) {
			if ( People( Loop ).Show55Warning ) {
				ThermalComfortInASH55( People( Loop ).ZonePtr ).Enable55Warning = true;
			}
		}

		// CurrentModuleObject='Zone'
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time [hr]", ThermalComfortInASH55( Loop ).timeNotSummer, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time [hr]", ThermalComfortInASH55( Loop ).timeNotWinter, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time [hr]", ThermalComfortInASH55( Loop ).timeNotEither, "Zone", "Sum", Zone( Loop ).Name );
		}
		SetupOutputVariable( "Facility Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time [hr]", AnyZoneTimeNotSimpleASH55Summer, "Zone", "Sum", "Facility" );
		SetupOutputVariable( "Facility Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time [hr]", AnyZoneTimeNotSimpleASH55Winter, "Zone", "Sum", "Facility" );
		SetupOutputVariable( "Facility Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time [hr]", AnyZoneTimeNotSimpleASH55Either, "Zone", "Sum", "Facility" );

		ThermalComfortSetPoint.allocate( NumOfZones );
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			SetupOutputVariable( "Zone Heating Setpoint Not Met Time [hr]", ThermalComfortSetPoint( Loop ).notMetHeating, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Heating Setpoint Not Met While Occupied Time [hr]", ThermalComfortSetPoint( Loop ).notMetHeatingOccupied, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Cooling Setpoint Not Met Time [hr]", ThermalComfortSetPoint( Loop ).notMetCooling, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Cooling Setpoint Not Met While Occupied Time [hr]", ThermalComfortSetPoint( Loop ).notMetCoolingOccupied, "Zone", "Sum", Zone( Loop ).Name );
		}

		SetupOutputVariable( "Facility Heating Setpoint Not Met Time [hr]", AnyZoneNotMetHeating, "Zone", "Sum", "Facility" );
		SetupOutputVariable( "Facility Cooling Setpoint Not Met Time [hr]", AnyZoneNotMetCooling, "Zone", "Sum", "Facility" );
		SetupOutputVariable( "Facility Heating Setpoint Not Met While Occupied Time [hr]", AnyZoneNotMetHeatingOccupied, "Zone", "Sum", "Facility" );
		SetupOutputVariable( "Facility Cooling Setpoint Not Met While Occupied Time [hr]", AnyZoneNotMetCoolingOccupied, "Zone", "Sum", "Facility" );

		GetAngleFactorList();

		ZoneOccHrs.dimension( NumOfZones, 0.0 );

	}

	void
	CalcThermalComfortFanger(
		Optional_int_const PNum, // People number for thermal comfort control
		Optional< Real64 const > Tset, // Temperature setpoint for thermal comfort control
		Optional< Real64 > PMVResult // PMV value for thermal comfort control
	)
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   January 2000
		//     MODIFIED       Rick Strand (for E+ implementation February 2000)
		//                    Brent Griffith modifications for CR 5641 (October 2005)
		//                    L. Gu, Added optional arguments for thermal comfort control (May 2006)
		//                    T. Hong, added Fanger PPD (April 2009)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates PMV(Predicted Mean Vote) using the Fanger thermal
		// comfort model. This subroutine is also used for thermal comfort control by determining
		// the temperature at which the PMV is equal to a PMV setpoint specified by the user.

		// METHODOLOGY EMPLOYED:
		// This subroutine is based heavily upon the work performed by Dan Maloney for
		// the BLAST program.  Many of the equations are based on the original Fanger
		// development.  See documentation for further details and references.

		// REFERENCES:
		// Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign
		// BG note (10/21/2005),  This formulation is based on the the BASIC program
		// that is included in ASHRAE Standard 55 Normative Appendix D.

		// Using/Aliasing
		using Psychrometrics::PsyPsatFnTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 150 ); // Limit of iteration
		Real64 const StopIterCrit( 0.00015 ); // Stop criteria for iteration
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 P1; // Intermediate variables to calculate clothed body ratio and clothing temperature
		Real64 P2; // Intermediate variables to calculate clothed body ratio and clothing temperature
		Real64 P3; // Intermediate variables to calculate clothed body ratio and clothing temperature
		Real64 P4; // Intermediate variables to calculate clothed body ratio and clothing temperature
		Real64 XF; // Intermediate variables to calculate clothed body ratio and clothing temperature
		Real64 XN; // Intermediate variables to calculate clothed body ratio and clothing temperature
		Real64 IntermediateClothing;
		//    REAL(r64) :: SkinTempComf        ! Skin temperature required to achieve thermal comfort; C

		Real64 PMV; // temporary variable to store calculated Fanger PMV value
		Real64 PPD; // temporary variable to store calculated Fanger PPD value

		for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {

			// Optional argument is used to access people object when thermal comfort control is used
			if ( present( PNum ) ) {
				if ( PeopleNum != PNum ) continue;
			}

			// If optional argument is used do not cycle regardless of thermal comfort reporting type
			if ( ( ! People( PeopleNum ).Fanger ) && ( ! present( PNum ) ) ) continue;

			ZoneNum = People( PeopleNum ).ZonePtr;
			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
				AirTemp = TCMF( ZoneNum ); //PH 3/7/04
				// UCSD-CV
			} else if ( IsZoneCV( ZoneNum ) ) {
				if ( ZoneUCSDCV( ZoneNum ).VforComfort == VComfort_Jet ) {
					AirTemp = ZTJET( ZoneNum );
				} else if ( ZoneUCSDCV( ZoneNum ).VforComfort == VComfort_Recirculation ) {
					AirTemp = ZTJET( ZoneNum );
				} else {
					// Thermal comfort control uses Tset to determine PMV setpoint value, otherwise use zone temp
					if ( present( PNum ) ) {
						AirTemp = Tset;
					} else {
						AirTemp = ZTAV( ZoneNum );
					}
				}
			} else {
				if ( present( PNum ) ) {
					AirTemp = Tset;
				} else {
					AirTemp = ZTAVComf( ZoneNum );
				}
			}
			RadTemp = CalcRadTemp( PeopleNum );
			// Use mean air temp for calculating RH when thermal comfort control is used
			if ( present( PNum ) ) {
				RelHum = PsyRhFnTdbWPb( MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress );
			} else {
				RelHum = PsyRhFnTdbWPb( ZTAVComf( ZoneNum ), ZoneAirHumRatAvgComf( ZoneNum ), OutBaroPress );
			}
			People( PeopleNum ).TemperatureInZone = AirTemp;
			People( PeopleNum ).RelativeHumidityInZone = RelHum * 100.0;

			// Metabolic rate of body (W/m2)
			ActLevel = GetCurrentScheduleValue( People( PeopleNum ).ActivityLevelPtr ) / BodySurfArea;
			// Energy consumption by external work (W/m2)
			WorkEff = GetCurrentScheduleValue( People( PeopleNum ).WorkEffPtr ) * ActLevel;
			// Clothing unit
			{ auto const SELECT_CASE_var( People( PeopleNum ).ClothingType );
			if ( SELECT_CASE_var == 1 ) {
				CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
			} else if ( SELECT_CASE_var == 2 ) {
				ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
				ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
				DynamicClothingModel();
				CloUnit = ThermalComfortData( PeopleNum ).ClothingValue;
			} else if ( SELECT_CASE_var == 3 ) {
				IntermediateClothing = GetCurrentScheduleValue( People( PeopleNum ).ClothingMethodPtr );
				if ( IntermediateClothing == 1.0 ) {
					CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
					ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
				} else if ( IntermediateClothing == 2.0 ) {
					ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
					ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
					DynamicClothingModel();
					CloUnit = ThermalComfortData( PeopleNum ).ClothingValue;
				} else {
					CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
					ShowWarningError( "PEOPLE=\"" + People( PeopleNum ).Name + "\", Scheduled clothing value will be used rather than clothing calculation method." );
				}
			} else {
				ShowSevereError( "PEOPLE=\"" + People( PeopleNum ).Name + "\", Incorrect Clothing Type" );
			}}

			if ( IsZoneCV( ZoneNum ) ) {
				if ( ZoneUCSDCV( ZoneNum ).VforComfort == VComfort_Jet ) {
					AirVel = Ujet( ZoneNum );
				} else if ( ZoneUCSDCV( ZoneNum ).VforComfort == VComfort_Recirculation ) {
					AirVel = Urec( ZoneNum );
				} else {
					AirVel = 0.2;
				}
			} else {
				AirVel = GetCurrentScheduleValue( People( PeopleNum ).AirVelocityPtr );
				// Ensure air velocity within the reasonable range. Otherwise reccusive warnings is provided
				if ( present( PNum ) && ( AirVel < 0.1 || AirVel > 0.5 ) ) {
					if ( People( PeopleNum ).AirVelErrIndex == 0 ) {
						ShowWarningMessage( "PEOPLE=\"" + People( PeopleNum ).Name + "\", Air velocity is beyond the reasonable range (0.1,0.5) for thermal comfort control." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "PEOPLE=\"" + People( PeopleNum ).Name + "\",Air velocity is still beyond the reasonable range (0.1,0.5)", People( PeopleNum ).AirVelErrIndex, AirVel, AirVel, _, "[m/s]", "[m/s]" );
				}

			}

			// VapPress    = CalcSatVapPressFromTemp(AirTemp)  !original
			// VapPress    = RelHum*VapPress                   !original might be in torrs

			VapPress = PsyPsatFnTemp( AirTemp ); // use psych routines inside E+ , returns Pa

			VapPress *= RelHum; // in units of [Pa]

			IntHeatProd = ActLevel - WorkEff;

			// Compute the Corresponding Clothed Body Ratio
			CloBodyRat = 1.05 + 0.1 * CloUnit; // The ratio of the surface area of the clothed body
			// to the surface area of nude body

			if ( CloUnit < 0.5 ) CloBodyRat = CloBodyRat - 0.05 + 0.1 * CloUnit;

			AbsRadTemp = RadTemp + TAbsConv;
			AbsAirTemp = AirTemp + TAbsConv;

			CloInsul = CloUnit * CloBodyRat * 0.155; // Thermal resistance of the clothing

			P2 = CloInsul * 3.96;
			P3 = CloInsul * 100.0;
			P1 = CloInsul * AbsAirTemp;
			P4 = 308.7 - 0.028 * IntHeatProd + P2 * pow_4( AbsRadTemp / 100.0 );

			// First guess for clothed surface tempeature
			AbsCloSurfTemp = AbsAirTemp + ( 35.5 - AirTemp ) / ( 3.5 * ( CloUnit + 0.1 ) );
			XN = AbsCloSurfTemp / 100.0;
			HcFor = 12.1 * std::sqrt( AirVel ); // Heat transfer coefficient by forced convection
			IterNum = 0;
			XF = XN;

			// COMPUTE SURFACE TEMPERATURE OF CLOTHING BY ITERATIONS
			while ( ( ( std::abs( XN - XF ) > StopIterCrit ) || ( IterNum == 0 ) ) && ( IterNum < MaxIter ) ) {
				XF = ( XF + XN ) / 2.0;
				HcNat = 2.38 * root_4( std::abs( 100.0 * XF - AbsAirTemp ) ); // Heat transfer coefficient by natural convection
				Hc = max( HcFor, HcNat ); // Determination of convective heat transfer coefficient
				XN = ( P4 + P1 * Hc - P2 * pow_4( XF ) ) / ( 100.0 + P3 * Hc );
				++IterNum;
				if ( IterNum > MaxIter ) {
					ShowWarningError( "Max iteration exceeded in CalcThermalFanger" );
				}
			}
			AbsCloSurfTemp = 100.0 * XN;
			CloSurfTemp = AbsCloSurfTemp - TAbsConv;

			// COMPUTE PREDICTED MEAN VOTE
			// Sensible heat loss
			// RadHeatLoss = RadSurfEff*CloBodyRat*SkinEmiss*StefanBoltz* &   !original
			//                            (AbsCloSurfTemp**4 - AbsRadTemp**4) ! Heat loss by radiation

			// following line is ln 480 in ASHRAE 55 append. D
			RadHeatLoss = 3.96 * CloBodyRat * ( pow_4( AbsCloSurfTemp * 0.01 ) - pow_4( AbsRadTemp * 0.01 ) );

			ConvHeatLoss = CloBodyRat * Hc * ( CloSurfTemp - AirTemp ); // Heat loss by convection

			DryHeatLoss = RadHeatLoss + ConvHeatLoss;

			// Evaporative heat loss
			// Heat loss by regulatory sweating
			EvapHeatLossRegComf = 0.0;
			if ( IntHeatProd > 58.2 ) {
				EvapHeatLossRegComf = 0.42 * ( IntHeatProd - ActLevelConv );
			}
			// SkinTempComf = 35.7 - 0.028*IntHeatProd ! Skin temperature required to achieve thermal comfort
			// SatSkinVapPress = 1.92*SkinTempComf - 25.3 ! Water vapor pressure at required skin temperature
			// Heat loss by diffusion
			// EvapHeatLossDiff = 0.4148*(SatSkinVapPress - VapPress) !original
			EvapHeatLossDiff = 3.05 * 0.001 * ( 5733.0 - 6.99 * IntHeatProd - VapPress ); // ln 440 in ASHRAE 55 Append. D

			EvapHeatLoss = EvapHeatLossRegComf + EvapHeatLossDiff;
			// Heat loss by respiration
			// original: LatRespHeatLoss = 0.0023*ActLevel*(44. - VapPress) ! Heat loss by latent respiration
			LatRespHeatLoss = 1.7 * 0.00001 * ActLevel * ( 5867.0 - VapPress ); //ln 460 in ASHRAE 55 Append. D

			// LatRespHeatLoss = 0.017251*ActLevel*(5.8662 - VapPress)
			// V-1.2.2 'fix' BG 3/2005 5th term in LHS Eq (58)  in 2001 HOF Ch. 8
			// this was wrong because VapPress needed to be kPa

			DryRespHeatLoss = 0.0014 * ActLevel * ( 34.0 - AirTemp ); // Heat loss by dry respiration.

			RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss;

			ThermSensTransCoef = 0.303 * std::exp( -0.036 * ActLevel ) + 0.028; // Thermal transfer coefficient to calculate PMV

			PMV = ThermSensTransCoef * ( IntHeatProd - EvapHeatLoss - RespHeatLoss - DryHeatLoss );

			ThermalComfortData( PeopleNum ).FangerPMV = PMV;

			// Pass resulting PMV based on temperature setpoint (Tset) when using thermal comfort control
			if ( present( PNum ) ) {
				PMVResult = PMV;
			}
			ThermalComfortData( PeopleNum ).ThermalComfortMRT = RadTemp;
			ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
			ThermalComfortData( PeopleNum ).CloSurfTemp = CloSurfTemp;

			// Calculate the Fanger PPD (Predicted Percentage of Dissatisfied), as a %
			PPD = 100.0 - 95.0 * std::exp( -0.03353 * pow_4( PMV ) - 0.2179 * pow_2( PMV ) );
			if ( PPD < 0.0 ) {
				PPD = 0.0;
			} else if ( PPD > 100.0 ) {
				PPD = 100.0;
			}

			ThermalComfortData( PeopleNum ).FangerPPD = PPD;
		}

	}

	void
	CalcThermalComfortPierce()
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   January 2000
		//     MODIFIED       Rick Strand (for E+ implementation February 2000)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates PMVET, PMVSET, DISC, and TSENS using the Pierce
		// 2 Node model.

		// METHODOLOGY EMPLOYED:
		// This subroutine is based heavily upon the work performed by Dan Maloney for
		// the BLAST program.  Many of the equations are based on the original Pierce
		// development.  See documentation for further details and references.

		// REFERENCES:
		// Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CloFac( 0.25 ); // Clothing factor determined experimentally
		Real64 const EvapEff( 0.9 ); // Evaporative efficiency
		Real64 const MaxSkinBloodFlow( 90.0 ); // Max. value of skin blood flow
		Real64 const RegSweatMax( 670.0 ); // Max. value of regulatory sweating; w/m2
		Real64 const SkinBloodFlowConst( 200.0 ); // Skin blood flow coefficient for average person; l/m2.hr.k
		Real64 const StdAtm( 1.0 ); // Standard Atmospheres
		Real64 const Str( 0.1 ); // Constriction constant of skin blood flow for average person
		Real64 const SweatContConst( 170.0 ); // Proportionality constant for sweat control; g/m2.hr
		Real64 const VapPressConv( 0.1333227 ); // Vapor pressure converter from torr to Kpa

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 AirEvapHeatResist; // Evaporative heat resistance of air
		Real64 ActMet; // Metalbolic rate in MET
		Real64 ActLevelStart; // Activity level at the start of the minute-by-minute iterations
		Real64 AvgBodyTemp; // Average body temperature
		Real64 AvgBodyTempHigh; // Average body temperature when HSI(Belding's classic heat sterss index) is 100
		Real64 AvgBodyTempLow; // Average body temperature when DISC is 0
		Real64 AvgBodyTempSet; // Setpoint for average body temperature
		Real64 BodyThermSigCold; // Temperature difference of Body when BodyTempSet is higher than BodyTemp
		Real64 BodyTempChange; // Temperature change of body in 1 minute
		Real64 BodyThermSigWarm; // Temperature difference of Body when BodyTemp is higher than BodyTempSet
		Real64 CloCond; // The conductance of the clothing
		Real64 CloEvapHeatResist; // Evaporative heat resistance of clothing
		Real64 CloSurfTempOld; // Old value of clothing surface temperature
		Real64 CoreThermSigCold; // Temperature difference of core when CoreTempSet is higher than CoreTemp
		Real64 CoreHeatStorage; // Heat storage in core compartment
		Real64 CoreTempSet; // Setpoint for body core temperature
		Real64 CoreThermSigWarm; // Temperature difference of core when CoreTemp is higher than CoreTempSet
		Real64 DryHeatLossET; // Heat loss from clothing surface due to both convection and radiation at ET
		Real64 DryHeatLossSET; // Heat loss from clothing surface due to both convection and radiation at SET
		Real64 EffectCloThermEff; // Effective clothing thermal efficiency
		Real64 EffectCloUnit; // Effective clothing unit; clo
		Real64 EnergyBalErrET; // Stop criterion for iteration to solve energy balance
		Real64 EnergyBalErrSET; // Stop criterion for iteration to solve energy balance
		Real64 ET; // Effective temperature
		Real64 EvapHeatLossStart; // Starting value of evaporative heat loss
		bool FirstMinIter;
		Real64 HcAct; // Convective heat transfer coefficient at high activity
		Real64 HcStd; // Standard convective heat transfer coefficient
		Real64 HrStd; // Standard radiant heat transfer coefficient
		Real64 HStd; // Standard combined heat transfer coefficient
		int IterMin; // Time period for the ieterative calculation
		Real64 LewisRat; // Lewis ratio
		Real64 RegSweat; // The rate of regulatory sweating
		Real64 SET; // Standard effective temperature
		Real64 SkinBloodFlow; // The skin blood flow
		Real64 SkinThermSigCold; // Temperature difference of skin when SkinTempSet is higher than SkinTemp
		Real64 SkinHeatLoss; // Heat loss from skin
		Real64 SkinHeatStorage; // Heat storage in skin compartment
		Real64 SkinMassRat; // Actual skin mass to total body mass ratio
		Real64 SkinMassRatSet; // Setpoint for skin mass to total body mass ratio
		Real64 SkinRelHum; // Relative humidity at skin
		Real64 SkinTempSet; // Setpoint for skin temperature
		Real64 SkinThermSigWarm; // Temperature difference of skin when SkinTemp is higher than SkinTempSet
		Real64 StdCloBodyRat; // Standard ratio of clothed body
		Real64 StdCloFac; // Clothing factor determined experimentally at standard environment
		Real64 StdCloPermeatEff; // Standard clothing permeation efficiency
		Real64 StdCloUnit; // standard clothing unit
		Real64 StdEffectCloThermEff; // Standard effective clothing theraml efficiency
		Real64 StdEffectCloUnit; // standard effective clothing unit
		Real64 StdVapPressET; // Standard vapor pressure at effective temperature
		Real64 StdVapPressSET; // Standard vapor pressure at standar effective temperature
		Real64 TotEvapHeatResist; // Total evaporative heat resistance
		Real64 UnevapSweat; // Unevaporated sweat; g/m2/hr
		Real64 IntermediateClothing;

		// FLOW:

		for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {

			if ( ! People( PeopleNum ).Pierce ) continue;

			ZoneNum = People( PeopleNum ).ZonePtr;
			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
				AirTemp = TCMF( ZoneNum ); //PH 3/7/04
			} else {
				AirTemp = ZTAV( ZoneNum );
			}
			RadTemp = CalcRadTemp( PeopleNum );
			RelHum = PsyRhFnTdbWPb( ZTAV( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress );
			// Metabolic rate of body (W/m2)
			ActLevel = GetCurrentScheduleValue( People( PeopleNum ).ActivityLevelPtr ) / BodySurfArea;
			// Energy consumption by external work (W/m2)
			WorkEff = GetCurrentScheduleValue( People( PeopleNum ).WorkEffPtr ) * ActLevel;
			// Clothing unit
			{ auto const SELECT_CASE_var( People( PeopleNum ).ClothingType );
			if ( SELECT_CASE_var == 1 ) {
				CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
			} else if ( SELECT_CASE_var == 2 ) {
				ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
				ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
				DynamicClothingModel();
				CloUnit = ThermalComfortData( PeopleNum ).ClothingValue;
			} else if ( SELECT_CASE_var == 3 ) {
				IntermediateClothing = GetCurrentScheduleValue( People( PeopleNum ).ClothingMethodPtr );
				if ( IntermediateClothing == 1.0 ) {
					CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
					ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
				} else if ( IntermediateClothing == 2.0 ) {
					ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
					ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
					DynamicClothingModel();
					CloUnit = ThermalComfortData( PeopleNum ).ClothingValue;
				} else {
					CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
					ShowWarningError( "Scheduled clothing value will be used rather than clothing calculation method." );
				}
			} else {
				ShowSevereError( "Incorrect Clothing Type" );
			}}

			AirVel = GetCurrentScheduleValue( People( PeopleNum ).AirVelocityPtr );

			VapPress = CalcSatVapPressFromTemp( AirTemp );
			VapPress *= RelHum;
			VapPress *= VapPressConv; // Torr to KPa (5.8662 kPa=44 mmHg; .017251=.0023*760 mmHg/101.325 kPa)
			IntHeatProd = ActLevel - WorkEff;
			ActMet = ActLevel / ActLevelConv;
			// CALCULATE VARIABLESS THAT REMAIN CONSTANT FOR AN HOUR
			CloBodyRat = 1.0 + CloFac * CloUnit;

			if ( CloUnit < 0.01 ) CloUnit = 0.01;

			CloCond = 1.0 / ( CloUnit * 0.155 );

			// INITIALIZE THE POLLOWING VARIABLES
			if ( AirVel < 0.137 ) AirVel = 0.137;

			Hc = 8.6 * std::pow( AirVel, 0.53 );
			if ( ActMet > 0.9 ) {
				HcAct = 5.66 * std::pow( ActMet - 0.85, 0.39 );
				Hc = max( HcAct, Hc );
			}

			// Definition of vascular control signals
			// CoreTempSet, SkinTempSet, and AvgBodyTempSet are the setpoints for core, skin and
			// average body temperatures corresponding to physiol.  neutrality
			// SkinMassRatSet is the ratio of skin mass to total body mass (skin+core)
			// Typical values for CoreTempSet, SkinTempSet and SkinMassRatSet are 36.8, 33.7 and 0.10
			// SkinMassRat is the actual skin to total body mass ratio
			SkinTempSet = 33.7;
			CoreTempSet = 36.8;
			SkinMassRatSet = 0.10;
			AvgBodyTempSet = SkinMassRatSet * SkinTempSet + ( 1.0 - SkinMassRatSet ) * CoreTempSet;

			// APPROXIMATE THE FOLLOWING VALUES TO START
			SkinTemp = 33.7;
			CoreTemp = 36.8;
			SkinBloodFlow = 6.3;
			EvapHeatLossStart = 5.0;
			LatRespHeatLoss = 0.017251 * ActLevel * ( 5.8662 - VapPress );
			EvapHeatLoss = ( EvapHeatLossStart - LatRespHeatLoss );
			SkinMassRat = 0.0417737 + 0.7451832 / ( SkinBloodFlow + 0.585417 );

			// GUESS CloSurfTemp TO START
			CloSurfTemp = ( SkinTemp + AirTemp ) / 2.0;

			// SIMULATION OF TEMPERATURE REGULATION.
			// This SECTION simulates the temperature regulation over 1 minute.
			// Inputs are the physiological data from the previous time step and
			// the current environmental conditions.

			// BEGIN MINUTE BY MINUTE CALCULATIONS FOR ONE HOUR
			ActLevelStart = ActLevel; // ActLevel gets increased by shivering in the following DO
			// loop and must be increased from the start level, not
			// perpetually increased
			for ( IterMin = 1; IterMin <= 60; ++IterMin ) {

				// Dry heat balance:  solve  for CloSurfTemp and Hr
				FirstMinIter = true;
				CloSurfTempOld = 0.0;
				while ( ( std::abs( CloSurfTemp - CloSurfTempOld ) > 0.01 ) || FirstMinIter ) {
					FirstMinIter = false;
					CloSurfTempOld = CloSurfTemp;
					Hr = 4.0 * RadSurfEff * StefanBoltz * pow_3( ( CloSurfTemp + RadTemp ) / 2.0 + TAbsConv );
					CloSurfTemp = ( CloCond * SkinTemp + CloBodyRat * ( Hc * AirTemp + Hr * RadTemp ) ) / ( CloCond + CloBodyRat * ( Hc + Hr ) );
				}

				// CALCULATE THE COMBINED HEAT TRANSFER COEFF. (H)
				H = Hr + Hc;
				// Heat flow from Clothing surface to environment
				DryHeatLoss = CloBodyRat * ( Hc * ( CloSurfTemp - AirTemp ) + Hr * ( CloSurfTemp - RadTemp ) );
				// dry and latent respiratory heat losses
				LatRespHeatLoss = 0.017251 * ActLevel * ( 5.8662 - VapPress );
				DryRespHeatLoss = 0.0014 * ActLevel * ( 34.0 - AirTemp ) * StdAtm;
				RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss;
				// Heat flows to skin and core:
				HeatFlow = ( CoreTemp - SkinTemp ) * ( 5.28 + 1.163 * SkinBloodFlow );
				// 5.28 is skin conductance in the
				// absence of skin blood flow
				SkinHeatStorage = HeatFlow - DryHeatLoss - EvapHeatLoss;
				CoreHeatStorage = ActLevel - ( CoreTemp - SkinTemp ) * ( 5.28 + 1.163 * SkinBloodFlow ) - RespHeatLoss - WorkEff;

				// Thermal capacities (average man: 70 kg, 1.8 square meter).
				CoreThermCap = ActLevelConv * ( 1.0 - SkinMassRat ) * 70.0;
				SkinThermCap = ActLevelConv * SkinMassRat * 70.0;

				// Temperature changes in 1 minute
				SkinTempChange = ( SkinHeatStorage * 1.8 ) / SkinThermCap;
				CoreTempChange = ( CoreHeatStorage * 1.8 ) / CoreThermCap;
				BodyTempChange = SkinMassRat * SkinTempChange + ( 1.0 - SkinMassRat ) * CoreTempChange;
				SkinTemp += SkinTempChange;
				CoreTemp += CoreTempChange;
				AvgBodyTemp = SkinMassRat * SkinTemp + ( 1.0 - SkinMassRat ) * CoreTemp;

				if ( SkinTemp > SkinTempSet ) {
					SkinThermSigWarm = SkinTemp - SkinTempSet;
					SkinThermSigCold = 0.0;
				} else {
					SkinThermSigCold = SkinTempSet - SkinTemp;
					SkinThermSigWarm = 0.0;
				}

				if ( CoreTemp > CoreTempSet ) {
					CoreThermSigWarm = CoreTemp - CoreTempSet;
					CoreThermSigCold = 0.0;
				} else {
					CoreThermSigCold = CoreTempSet - CoreTemp;
					CoreThermSigWarm = 0.0;
				}

				if ( AvgBodyTemp > AvgBodyTempSet ) {
					BodyThermSigWarm = AvgBodyTemp - AvgBodyTempSet;
					BodyThermSigCold = 0.0;
				} else {
					BodyThermSigCold = AvgBodyTempSet - AvgBodyTemp;
					BodyThermSigWarm = 0.0;
				}

				VasodilationFac = SkinBloodFlowConst * CoreThermSigWarm;
				VasoconstrictFac = Str * SkinThermSigCold;
				SkinBloodFlow = ( 6.3 + VasodilationFac ) / ( 1.0 + VasoconstrictFac );

				// SkinBloodFlow is never below 0.5 liter/(m2.hr) nor above MaxSkinBloodFlow
				if ( SkinBloodFlow < 0.5 ) SkinBloodFlow = 0.5;
				if ( SkinBloodFlow > MaxSkinBloodFlow ) SkinBloodFlow = MaxSkinBloodFlow;

				// ratio of skin-core masses change with SkinBloodFlow
				// (SkinMassRat,SkinBloodFlow) = (.15,6.3),(.45,1.24),(.05,90)
				SkinMassRat = 0.0417737 + 0.7451832 / ( SkinBloodFlow + 0.585417 );

				// control of regulatory sweating
				if ( SkinThermSigWarm == 0.0 ) {
					RegSweat = SweatContConst * BodyThermSigWarm;
				} else {
					RegSweat = SweatContConst * BodyThermSigWarm * std::exp( SkinThermSigWarm / 10.7 );
				}

				if ( RegSweat > RegSweatMax ) RegSweat = RegSweatMax;

				EvapHeatLossRegSweat = 0.68 * RegSweat;

				// adjustment of metabolic heat due to shivering (Stolwijk, Hardy)
				ShivResponse = 19.4 * SkinThermSigCold * CoreThermSigCold;
				ActLevel = ActLevelStart + ShivResponse;

				// Evaluation of heat transfer by evaporation at skin surface
				// LewisRat varies with SkinTemp.
				// LewisRat=2.02 C/mmHg or 15.1512 C/kPa at 0 C (lr=2.2 at 25 C)
				LewisRat = 15.1512 * ( SkinTemp + TAbsConv ) / TAbsConv;

				// Mass transfer equation between skin and environment
				// TotEvapHeatResist is total vapor resistance of CloUnitthing + air layer
				// CloInsul is efficiency of mass transfer for CloUnitthing
				// CloInsul IS SET TO .45 (FOR WOVEN MATERIAL)
				// Reference:  Woodcock, Breckenridge and Goldman
				CloInsul = 0.45;
				CloThermEff = 1.0 / ( 1.0 + 0.155 * CloBodyRat * H * CloUnit );

				AirEvapHeatResist = 1.0 / ( LewisRat * CloBodyRat * Hc );
				CloEvapHeatResist = 0.155 * CloUnit / ( LewisRat * CloInsul );
				TotEvapHeatResist = AirEvapHeatResist + CloEvapHeatResist;

				SatSkinVapPress = CalcSatVapPressFromTemp( SkinTemp );
				SatSkinVapPress *= 0.1333227;
				EvapHeatLossMax = ( 1.0 / TotEvapHeatResist ) * ( SatSkinVapPress - VapPress );
				SkinWetSweat = EvapHeatLossRegSweat / EvapHeatLossMax;

				// 0.06 if SkinWetDiff for nonsweating skin --- Kerslake
				SkinWetDiff = ( 1.0 - SkinWetSweat ) * 0.06;
				EvapHeatLossDiff = SkinWetDiff * EvapHeatLossMax;
				EvapHeatLoss = EvapHeatLossRegSweat + EvapHeatLossDiff;
				SkinWetTot = EvapHeatLoss / EvapHeatLossMax;

				// Beginning of dripping (Sweat not evaporated on skin surface)
				if ( ( SkinWetTot >= EvapEff ) && ( EvapHeatLossMax >= 0 ) ) {
					SkinWetTot = EvapEff;
					SkinWetSweat = ( EvapEff - 0.06 ) / 0.94;
					EvapHeatLossRegSweat = SkinWetSweat * EvapHeatLossMax;
					SkinWetDiff = ( 1.0 - SkinWetSweat ) * 0.06;
					EvapHeatLossDiff = SkinWetDiff * EvapHeatLossMax;
					EvapHeatLoss = EvapHeatLossRegSweat + EvapHeatLossDiff;
				}

				// When EvapHeatLossMax<0. condensation on skin occurs.
				if ( EvapHeatLossMax <= 0.0 ) {
					SkinWetDiff = 0.0;
					EvapHeatLossDiff = 0.0;
					EvapHeatLoss = EvapHeatLossMax;
					SkinWetTot = EvapEff;
					SkinWetSweat = EvapEff;
					EvapHeatLossRegSweat = 0.0;
				}

				// UnevapSweat = unevaporated sweat in grams/sq.m/hr
				UnevapSweat = ( RegSweat * 0.68 - SkinWetSweat * EvapHeatLossMax ) / 0.68;
				if ( UnevapSweat <= 0.0 ) UnevapSweat = 0.0;

				// Vapor pressure at skin (as measured by dewpoint sensors)
				SkinVapPress = SkinWetTot * SatSkinVapPress + ( 1.0 - SkinWetTot ) * VapPress;

				// SkinRelHum is skin relative humidity
				SkinRelHum = SkinVapPress / SatSkinVapPress;

			} // END OF MINUTE BY MINUTE TEMPERATURE REGULATION LOOP

			// Computation of comfort indices.
			// Inputs to this SECTION are the physiological data from the simulation of
			// temperature regulation loop

			// PART I: Heat transfer indices in real environment
			OpTemp = ( Hr * RadTemp + Hc * AirTemp ) / H;
			EffectCloUnit = CloUnit - ( CloBodyRat - 1.0 ) / ( 0.155 * CloBodyRat * H );
			EffectCloThermEff = 1.0 / ( 1.0 + 0.155 * H * EffectCloUnit );
			CloPermeatEff = 1.0 / ( 1.0 + ( 0.155 / CloInsul ) * Hc * EffectCloUnit );

			// PART II: ET*(standardization humidity/REAL(r64) CloUnit, StdAtm and Hc)
			// calculation of skin heat Loss (SkinHeatLoss)
			SkinHeatLoss = H * EffectCloThermEff * ( SkinTemp - OpTemp ) + SkinWetTot * LewisRat * Hc * CloPermeatEff * ( SatSkinVapPress - VapPress );
			// Get a low approximation for ET* and solve balance
			// equation by iteration
			ET = SkinTemp - SkinHeatLoss / ( H * EffectCloThermEff );
			// THE STANDARD VAPOR PRESSURE AT THE EFFECTIVE TEMP : StdVapPressET

			while ( true ) {
				StdVapPressET = CalcSatVapPressFromTemp( ET );
				StdVapPressET *= VapPressConv;
				EnergyBalErrET = SkinHeatLoss - H * EffectCloThermEff * ( SkinTemp - ET ) - SkinWetTot * LewisRat * Hc * CloPermeatEff * ( SatSkinVapPress - StdVapPressET / 2.0 );
				if ( EnergyBalErrET >= 0.0 ) break;
				ET += 0.1;
			}

			// Part III: Standard effective temperature SET*
			// standardized humidity.  Hc, CloUnit, StdAtm
			// normalized for given ActLeAirVelivity

			// Standard environment
			HrStd = Hr;
			// HcStd = standard conv. heat tr. coeff. (level walking/still air)
			if ( ActMet <= 0.86 ) ActMet = 0.86;
			HcStd = 5.66 * std::pow( ActMet - 0.85, 0.39 );

			// minimum value of Hc at sea leAirVel = 3.0 (AirVel = .137 m/s)
			if ( HcStd <= 3.0 ) HcStd = 3.0;

			// standard MET - StdCloUnit relation gives SET* = 24 C when PMV = 0
			StdCloUnit = 1.3264 / ( ( ActLevel - WorkEff ) / ActLevelConv + 0.7383 ) - 0.0953;
			StdCloFac = CloFac;
			StdCloBodyRat = 1.0 + StdCloFac * StdCloUnit;
			HStd = HrStd + HcStd;
			StdEffectCloUnit = StdCloUnit - ( StdCloBodyRat - 1.0 ) / ( 0.155 * StdCloBodyRat * HStd );
			StdEffectCloThermEff = 1.0 / ( 1.0 + 0.155 * HStd * StdEffectCloUnit );
			StdCloPermeatEff = 1.0 / ( 1.0 + ( 0.155 / 0.45 ) * HcStd * StdEffectCloUnit );

			// Get a low approximation for SET*
			// and solve balance equ. by iteration
			SET = SkinTemp - SkinHeatLoss / ( HStd * StdEffectCloThermEff );

			while ( true ) {
				StdVapPressSET = CalcSatVapPressFromTemp( SET );
				StdVapPressSET *= VapPressConv;
				EnergyBalErrSET = SkinHeatLoss - HStd * StdEffectCloThermEff * ( SkinTemp - SET ) - SkinWetTot * LewisRat * HcStd * StdCloPermeatEff * ( SatSkinVapPress - StdVapPressSET / 2.0 );
				if ( EnergyBalErrSET >= 0.0 ) break;
				SET += 0.1;
			}

			// Part IV:  Fanger's comfort equation.
			// Thermal transfer coefficient to calculate PMV
			ThermSensTransCoef = 0.303 * std::exp( -0.036 * ActLevel ) + 0.028;
			// Fanger's reg. sweating at comfort threshold (PMV=0) is:
			EvapHeatLossRegComf = ( IntHeatProd - ActLevelConv ) * 0.42;

			// PMV*(PMVET in prgm) uses ET instead of OpTemp
			DryHeatLossET = HStd * StdEffectCloThermEff * ( SkinTemp - ET );
			ThermalComfortData( PeopleNum ).PiercePMVET = ThermSensTransCoef * ( IntHeatProd - RespHeatLoss - DryHeatLossET - EvapHeatLossDiff - EvapHeatLossRegComf );

			// SPMV*(PMVSET in prgm) uses SET instead of OpTemp
			DryHeatLossSET = HStd * StdEffectCloThermEff * ( SkinTemp - SET );
			ThermalComfortData( PeopleNum ).PiercePMVSET = ThermSensTransCoef * ( IntHeatProd - RespHeatLoss - DryHeatLossSET - EvapHeatLossDiff - EvapHeatLossRegComf );

			// Part V:  Heat stress and heat strain indices derived from EvapHeatLoss,
			// EvapHeatLossMax and W (skin wettedness)

			// EvapHeatLossMax is readjusted for EvapEff
			EvapHeatLossMax *= EvapEff;
			// DISC (discomfort) varies with relative thermoregulatory strain
			ThermalComfortData( PeopleNum ).PierceDISC = 5.0 * ( EvapHeatLossRegSweat - EvapHeatLossRegComf ) / ( EvapHeatLossMax - EvapHeatLossRegComf - EvapHeatLossDiff );

			// Part VI:  Thermal sensation TSENS as function of mean body temp.-
			// AvgBodyTempLow is AvgBodyTemp when DISC is 0. (lower limit of zone of evap. regul.)
			AvgBodyTempLow = ( 0.185 / ActLevelConv ) * ( ActLevel - WorkEff ) + 36.313;
			// AvgBodyTempHigh is AvgBodyTemp when HSI=100 (upper limit of zone of evap. regul.)
			AvgBodyTempHigh = ( 0.359 / ActLevelConv ) * ( ActLevel - WorkEff ) + 36.664;

			// TSENS=DISC=4.7 when HSI =1 00 (HSI is Belding's classic heat stress index)
			// In cold, DISC &TSENS are the same and neg. fct of AvgBodyTemp
			if ( AvgBodyTemp > AvgBodyTempLow ) {
				ThermalComfortData( PeopleNum ).PierceTSENS = 4.7 * ( AvgBodyTemp - AvgBodyTempLow ) / ( AvgBodyTempHigh - AvgBodyTempLow );

			} else {
				ThermalComfortData( PeopleNum ).PierceTSENS = 0.68175 * ( AvgBodyTemp - AvgBodyTempLow );
				ThermalComfortData( PeopleNum ).PierceDISC = ThermalComfortData( PeopleNum ).PierceTSENS;
			}

			ThermalComfortData( PeopleNum ).ThermalComfortMRT = RadTemp;
			ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;

		}

	}

	void
	CalcThermalComfortKSU()
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   January 2000
		//     MODIFIED       Rick Strand (for E+ implementation February 2000)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates TSV using the KSU 2 Node model.
		// METHODOLOGY EMPLOYED:
		// This subroutine is based heavily upon the work performed by Dan Maloney for
		// the BLAST program.  Many of the equations are based on the original Pierce
		// development.  See documentation for further details and references.

		// REFERENCES:
		// Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CloEmiss( 0.8 ); // Clothing Emissivity

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Array1D< Real64 > Coeff( 2 ); // Coefficients used in Range-Kutta's Method
		static Array1D< Real64 > Temp( 2 ); // Temperature
		static Array1D< Real64 > TempChange( 2 ); // Change of temperature

		Real64 BodyWt; // Weight of body, kg
		Real64 DayNum; // Number of days of acclimation
		int NumDay; // Loop counter for DayNum
		Real64 EmissAvg; // Average emissivity
		int IncreDayNum; // Number of days of increment in the outputs as desired
		Real64 IntHeatProdMet; // Internal heat production in MET
		Real64 IntHeatProdMetMax; // Maximum value of internal heat production in MET
		int LastDayNum; // Number of days for the last print out
		Real64 SkinWetFac; // Skin wettedness factor
		Real64 SkinWetNeut; // Skin wettedness at neutral state
		int StartDayNum; // Number of days for the first print out
		// Unacclimated man = 1, Acclimated man = 14
		Real64 SweatSuppFac; // Sweat suppression factor due to skin wettedness
		Real64 TempDiffer; // Temperature difference between the rectal and esophageal temperatures
		// If not measured, set it to be 0.5 Deg. C.
		int TempIndiceNum; // Number of temperature indices
		Real64 ThermCndctMin; // Minimum value of thermal conductance
		Real64 ThermCndctNeut; // Thermal conductance at neutral state
		Real64 TimeExpos; // Time period in the exposure, hr
		Real64 TimeInterval; // Time interval of outputs desired, hr
		Real64 TSVMax; // Maximum value of thermal sensation vote
		Real64 IntermediateClothing;

		// FLOW:

		TempIndiceNum = 2;

		// NEXT GROUP OF VARIABLE ARE FIXED FOR BLAST PROGRAM - UNACCLIMATED MAN
		// THE TSV MODEL CAN BE APPLIED TO UNACCLIMATED MAN ONLY.
		TimeInterval = 1.0;
		TSVMax = 4.0;
		StartDayNum = 1;
		LastDayNum = 1;
		IncreDayNum = 1;
		TimeExpos = 1.0;
		TempDiffer = 0.5;

		for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
			// THE NEXT SIX VARIABLES WILL BE READ IN FROM INPUT DECK
			if ( ! People( PeopleNum ).KSU ) continue;

			ZoneNum = People( PeopleNum ).ZonePtr;
			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
				AirTemp = TCMF( ZoneNum ); //PH 3/7/04
			} else {
				AirTemp = ZTAV( ZoneNum );
			}
			RadTemp = CalcRadTemp( PeopleNum );
			RelHum = PsyRhFnTdbWPb( ZTAV( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress );
			ActLevel = GetCurrentScheduleValue( People( PeopleNum ).ActivityLevelPtr ) / BodySurfArea;
			WorkEff = GetCurrentScheduleValue( People( PeopleNum ).WorkEffPtr ) * ActLevel;
			{ auto const SELECT_CASE_var( People( PeopleNum ).ClothingType );
			if ( SELECT_CASE_var == 1 ) {
				CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
			} else if ( SELECT_CASE_var == 2 ) {
				ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
				ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
				DynamicClothingModel();
				CloUnit = ThermalComfortData( PeopleNum ).ClothingValue;
			} else if ( SELECT_CASE_var == 3 ) {
				IntermediateClothing = GetCurrentScheduleValue( People( PeopleNum ).ClothingMethodPtr );
				if ( IntermediateClothing == 1.0 ) {
					CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
					ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
				} else if ( IntermediateClothing == 2.0 ) {
					ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;
					ThermalComfortData( PeopleNum ).ClothingValue = CloUnit;
					DynamicClothingModel();
					CloUnit = ThermalComfortData( PeopleNum ).ClothingValue;
				} else {
					CloUnit = GetCurrentScheduleValue( People( PeopleNum ).ClothingPtr );
					ShowWarningError( "PEOPLE=\"" + People( PeopleNum ).Name + "\", Scheduled clothing value will be used rather than clothing calculation method." );
				}
			} else {
				ShowSevereError( "PEOPLE=\"" + People( PeopleNum ).Name + "\", Incorrect Clothing Type" );
			}}

			AirVel = GetCurrentScheduleValue( People( PeopleNum ).AirVelocityPtr );
			IntHeatProd = ActLevel - WorkEff;
			// THE FOLLOWING ARE TYPICAL VALUES SET FOR BLAST RUNS
			// STANDARD MAN: 70. KG WEIGHT, 1.8 M2 SURFACE AREA
			BodyWt = 70.0;
			CoreTemp = 37.0;
			SkinTemp = 31.0;

			//   CALCULATIONS NEEDED FOR THE PASSIVE STATE EQUATIONS
			CoreThermCap = 0.9 * BodyWt * 0.97 / BodySurfArea;
			SkinThermCap = 0.1 * BodyWt * 0.97 / BodySurfArea;
			//   KERSLAKE'S FORMULA (0.05<AirVel<5. M/S)
			if ( AirVel < 0.137 ) AirVel = 0.137;
			Hc = 8.3 * std::sqrt( AirVel );
			EmissAvg = RadSurfEff * CloEmiss + ( 1.0 - RadSurfEff ) * 1.0;
			//   IBERALL EQUATION
			Hr = EmissAvg * ( 3.87 + 0.031 * RadTemp );
			H = Hr + Hc;
			OpTemp = ( Hc * AirTemp + Hr * RadTemp ) / H;
			VapPress = CalcSatVapPressFromTemp( AirTemp );
			VapPress *= RelHum;
			CloBodyRat = 1.0 + 0.2 * CloUnit;
			CloThermEff = 1.0 / ( 1.0 + 0.155 * H * CloBodyRat * CloUnit );
			CloPermeatEff = 1.0 / ( 1.0 + 0.143 * Hc * CloUnit );
			//  BASIC INFORMATION FOR THERMAL SENSATION.
			IntHeatProdMet = IntHeatProd / ActLevelConv;
			IntHeatProdMetMax = max( 1.0, IntHeatProdMet );
			ThermCndctNeut = 12.05 * std::exp( 0.2266 * ( IntHeatProdMetMax - 1.0 ) );
			SkinWetNeut = 0.02 + 0.4 * ( 1.0 - std::exp( -0.6 * ( IntHeatProdMetMax - 1.0 ) ) );
			ThermCndctMin = ( ThermCndctNeut - 5.3 ) * 0.26074074 + 5.3;
			Real64 const ThemCndct_75_fac( 1.0 / ( 75.0 - ThermCndctNeut ) );
			Real64 const ThemCndct_fac( 1.0 / ( ThermCndctNeut - ThermCndctMin ) );
			//  CALCULATE THE PHYSIOLOGICAL REACTIONS OF AN UNACCLIMATED
			//  MAN (LastDayNum = 1), OR AN ACCLIMATED MAN (LastDayNum = 14, IncreDayNum = 13),
			assert( IncreDayNum > 0 ); //Autodesk:F2C++ Loop setup assumption
			for ( NumDay = StartDayNum; NumDay <= LastDayNum; NumDay += IncreDayNum ) {
				//  INITIAL CONDITIONS IN AN EXPOSURE
				DayNum = double( NumDay );
				Time = 0.0;
				TimeChange = 0.01;
				SweatSuppFac = 1.0;
				Temp( 1 ) = CoreTemp;
				Temp( 2 ) = SkinTemp;
				Coeff( 1 ) = Coeff( 2 ) = 0.0;
				//  PHYSIOLOGICAL ADJUSTMENTS IN HEAT ACCLIMATION.
				AcclPattern = 1.0 - std::exp( -0.12 * ( DayNum - 1.0 ) );
				CoreTempNeut = 36.9 - 0.6 * AcclPattern;
				SkinTempNeut = 33.8 - 1.6 * AcclPattern;
				ActLevel -= 0.07 * ActLevel * AcclPattern;
				Real64 const SkinTempNeut_fac( 1.0 / ( 1.0 - SkinWetNeut ) );
				//  CALCULATION OF CoreTempChange/TempChange & SkinTempChange/TempChange
				DERIV( TempIndiceNum, Temp, TempChange );
				while ( true ) {
					//  CALCULATION OF THERMAL SENSATION VOTE (TSV).
					//  THE TSV MODEL CAN BE APPLIED TO UNACCLIMATED MAN ONLY.
					SkinWetFac = ( SkinWetSweat - SkinWetNeut ) * SkinTempNeut_fac;
					VasodilationFac = ( ThermCndct - ThermCndctNeut ) * ThemCndct_75_fac;
					VasoconstrictFac = ( ThermCndctNeut - ThermCndct ) * ThemCndct_fac;
					//  IF VasodilationFac < 0.0, VASOCONSTRICTION OCCURS AND RESULTS IN COLD SENSATION.
					//  OTHERWISE NORMAL BLOOD FLOW OR VASODILATION OCCURS AND RESULTS IN
					//  THERMAL NEUTRALITY OR WARM SENSATION.
					if ( VasodilationFac < 0 ) {
						ThermalComfortData( PeopleNum ).KsuTSV = -1.46153 * VasoconstrictFac + 3.74721 * pow_2( VasoconstrictFac ) - 6.168856 * pow_3( VasoconstrictFac );
					} else {
						ThermalComfortData( PeopleNum ).KsuTSV = ( 5.0 - 6.56 * ( RelHum - 0.50 ) ) * SkinWetFac;
						if ( ThermalComfortData( PeopleNum ).KsuTSV > TSVMax ) ThermalComfortData( PeopleNum ).KsuTSV = TSVMax;
					}

					ThermalComfortData( PeopleNum ).ThermalComfortMRT = RadTemp;
					ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = ( RadTemp + AirTemp ) / 2.0;

					CoreTemp = Temp( 1 );
					SkinTemp = Temp( 2 );
					EvapHeatLossSweatPrev = EvapHeatLossSweat;

					RKG( TempIndiceNum, TimeChange, Time, Temp, TempChange, Coeff );

					if ( Time > TimeExpos ) break;

				}

			}

		}

	}

	void
	DERIV(
		int & EP_UNUSED( TempIndiceNum ), // Number of temperature indices  unused1208
		Array1A< Real64 > Temp, // Temperature unused1208
		Array1A< Real64 > TempChange // Change of temperature
	)
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   January 2000
		//     MODIFIED       Rick Strand (for E+ implementation February 2000)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// THIS SUBROUTINE CALCULATES HEAT TRANSFER TERMS INVOLVED IN THE
		// THERMOREGULATORY SYSTEM TO OBTAIN THE RATES OF CHANGE OF CoreTemp & SkinTemp
		// VIZ., CoreTempChange/TempChange & SkinTempChange/TempChange RESPECTIVELY.

		// METHODOLOGY EMPLOYED:
		// This subroutine is based heavily upon the work performed by Dan Maloney for
		// the BLAST program.  Many of the equations are based on the original Pierce
		// development.  See documentation for further details and references.

		// REFERENCES:
		// Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		Temp.dim( 2 );
		TempChange.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ActLevelTot; // Total activity level
		Real64 CoreSignalShiv; // Core signal when shivering occurs
		Real64 CoreSignalShivMax; // Maximum value of core signal when shivering occurs
		Real64 CoreSignalSkinSens; // The sensitivity of the skin signal increases
		Real64 CoreSignalSweatMax; // Maximum value of core signal when sweating occurs
		Real64 CoreSignalSweatWarm; // Core signal when sweating occurs
		Real64 CoreTempSweat; // Core temperature when sweating occurs
		Real64 CoreSignalWarm; // Warm core signal
		Real64 CoreSignalWarmMax; // Maximum value of warm core signal
		Real64 EvapHeatLossDrySweat; // Evaporative heat loss by sweating when total skin wettedness < 0.4
		Real64 Err; // Stop criteria for iteration
		Real64 ErrPrev; // Previous value of stop criteria for iteration
		Real64 EvapHeatLossSweatEst; // Estimated evaporative heat loss by sweating
		Real64 EvapHeatLossSweatEstNew; // New value of estimated evaporative heat loss by sweating
		Real64 IntHeatProdTot; // Total internal heat production
		Real64 SkinCndctMax; // Maximum value of skin conductance
		Real64 SkinSignalCold; // Cold skin signal
		Real64 SkinSignalColdMax; // Maximum value of cold skin signal
		Real64 SkinSignalSweatCold; // Cold skin signal for sweat inhibition
		Real64 SkinSignalSweatColdMax; // Maximum value of cold skin signal for sweat inhibition
		Real64 SkinCndctDilation; // Overall skin conductance due to vasodilation
		Real64 SkinCndctConstriction; // Overall skin conductance due to vasoconstriction
		Real64 SkinSignalShiv; // Skin signal when shivering occurs
		Real64 SkinSignalShivMax; // Maximum value of skin signal when shivering occurs
		Real64 SkinSignalSweatMax; // Skin signal when sweating occurs
		Real64 SkinSignalSweatWarm; // Maximum value of skin signal when sweating occurs
		Real64 SkinSignalWarm; // Warm skin signal
		Real64 SkinSignalWarmMax; // Maximum value of warm skin signal
		Real64 SkinTempSweat; // Skin temperature when sweating occurs
		Real64 SkinWetSignal; // Skin wettedness signal
		Real64 SweatCtrlFac; // Sweat control factor
		Real64 SweatSuppFac; // Sweat suppression factor due to skin wettedness
		Real64 WeighFac; // Weighting factor of core siganl

		// THE CONTROLLING SYSTEM.
		// THE CONTROLLING SIGNALS :
		// SIGNALS FOR KS.
		CoreSignalWarm = CoreTemp - 36.98;
		SkinSignalWarm = SkinTemp - 33.8;
		SkinSignalCold = 32.1 - SkinTemp;
		CoreSignalSkinSens = CoreTemp - 35.15;
		CoreSignalWarmMax = max( 0.0, CoreSignalWarm );
		SkinSignalWarmMax = max( 0.0, SkinSignalWarm );
		SkinSignalColdMax = max( 0.0, SkinSignalCold );

		// SIGNALS FOR EvapHeatLossSweat.
		CoreTempSweat = CoreTemp;
		if ( CoreTempSweat > 38.29 ) CoreTempSweat = 38.29;
		CoreSignalSweatWarm = CoreTempSweat - CoreTempNeut;
		SkinTempSweat = SkinTemp;
		if ( SkinTempSweat > 36.1 ) SkinTempSweat = 36.1;
		SkinSignalSweatWarm = SkinTempSweat - SkinTempNeut;
		CoreSignalSweatMax = max( 0.0, CoreSignalSweatWarm );
		SkinSignalSweatMax = max( 0.0, SkinSignalSweatWarm );
		SkinSignalSweatCold = 33.37 - SkinTemp;
		if ( SkinTempNeut < 33.37 ) SkinSignalSweatCold = SkinTempNeut - SkinTemp;
		SkinSignalSweatColdMax = max( 0.0, SkinSignalSweatCold );

		// SIGNALS FOR SHIVERING.
		CoreSignalShiv = 36.9 - CoreTemp;
		SkinSignalShiv = 32.5 - SkinTemp;
		CoreSignalShivMax = max( 0.0, CoreSignalShiv );
		SkinSignalShivMax = max( 0.0, SkinSignalShiv );

		// CONTROLLING FUNCTIONS :
		// SHIVERING RESPONSE IN W/M**2.
		ShivResponse = 20.0 * CoreSignalShivMax * SkinSignalShivMax + 5.0 * SkinSignalShivMax;
		if ( CoreTemp >= 37.1 ) ShivResponse = 0.0;

		// SWEAT FUNCTION IN W/M**2.
		WeighFac = 260.0 + 70.0 * AcclPattern;
		SweatCtrlFac = 1.0 + 0.05 * std::pow( SkinSignalSweatColdMax, 2.4 );

		// EvapHeatLossDrySweat = SWEAT WHEN SkinWetTot < 0.4.
		EvapHeatLossDrySweat = ( ( WeighFac * CoreSignalSweatMax + 0.1 * WeighFac * SkinSignalSweatMax ) * std::exp( SkinSignalSweatMax / 8.5 ) ) / SweatCtrlFac;

		// MAXIMUM EVAPORATIVE POWER, EvapHeatLossMax, IN W/M**2.
		SkinVapPress = CalcSatVapPressFromTemp( SkinTemp );
		EvapHeatLossMax = 2.2 * Hc * ( SkinVapPress - VapPress ) * CloPermeatEff;
		if ( EvapHeatLossMax > 0.0 ) {
			SkinWetSweat = EvapHeatLossDrySweat / EvapHeatLossMax;
			EvapHeatLossDiff = 0.408 * ( SkinVapPress - VapPress );
			EvapHeatLoss = SkinWetSweat * EvapHeatLossMax + ( 1.0 - SkinWetSweat ) * EvapHeatLossDiff;
			SkinWetTot = EvapHeatLoss / EvapHeatLossMax;
			if ( Time == 0.0 ) {
				EvapHeatLossSweat = EvapHeatLossDrySweat;
				EvapHeatLossSweatPrev = EvapHeatLossDrySweat;
			}
			if ( SkinWetTot > 0.4 ) {

				// ITERATION  FOR SWEAT WHEN SkinWetTot IS GREATER THAT 0.4.
				IterNum = 0;
				if ( SkinWetSweat > 1.0 ) SkinWetSweat = 1.0;
				while ( true ) {
					EvapHeatLossSweatEst = EvapHeatLossSweatPrev;
					SkinWetSweat = EvapHeatLossSweatEst / EvapHeatLossMax;

					if ( SkinWetSweat > 1.0 ) SkinWetSweat = 1.0;

					EvapHeatLossDiff = 0.408 * ( SkinVapPress - VapPress );
					EvapHeatLoss = ( 1.0 - SkinWetTot ) * EvapHeatLossDiff + EvapHeatLossSweat;
					SkinWetTot = EvapHeatLoss / EvapHeatLossMax;

					if ( SkinWetTot > 1.0 ) SkinWetTot = 1.0;

					SkinWetSignal = max( 0.0, SkinWetTot - 0.4 );
					SweatSuppFac = 0.5 + 0.5 * std::exp( -5.6 * SkinWetSignal );
					EvapHeatLossSweatEstNew = SweatSuppFac * EvapHeatLossDrySweat;

					if ( IterNum == 0 ) EvapHeatLossSweat = EvapHeatLossSweatEstNew;

					Err = EvapHeatLossSweatEst - EvapHeatLossSweatEstNew;

					if ( IterNum != 0 ) {
						if ( ( ErrPrev * Err ) < 0.0 ) EvapHeatLossSweat = ( EvapHeatLossSweatEst + EvapHeatLossSweatEstNew ) / 2.0;
						if ( ( ErrPrev * Err ) >= 0.0 ) EvapHeatLossSweat = EvapHeatLossSweatEstNew;
					}

					// STOP CRITERION FOR THE ITERATION.
					if ( ( std::abs( Err ) <= 0.5 ) || ( IterNum >= 10 ) ) break;
					++IterNum;
					EvapHeatLossSweatPrev = EvapHeatLossSweat;
					ErrPrev = Err;

				}

			} else {
				EvapHeatLossSweat = EvapHeatLossDrySweat;
			}

		} else {
			SkinWetSweat = 1.0;
			SkinWetTot = 1.0;
			EvapHeatLossSweat = 0.5 * EvapHeatLossDrySweat;
			EvapHeatLoss = EvapHeatLossSweat;
		}

		// OVERALL SKIN CONDUCTANCE, KS, IN W/M**2/C.
		// SkinCndctDilation = EFFECT DUE TO VASODILATION.
		// SkinCndctConstriction = EFFECT DUE TO VASOCONSTRICTION.
		SkinCndctDilation = 42.45 * CoreSignalWarmMax + 8.15 * std::pow( CoreSignalSkinSens, 0.8 ) * SkinSignalWarmMax;
		SkinCndctConstriction = 1.0 + 0.4 * SkinSignalColdMax;
		// ThermCndct IS EQUIVALENT TO KS
		ThermCndct = 5.3 + ( 6.75 + SkinCndctDilation ) / SkinCndctConstriction;
		SkinCndctMax = 75.0 + 10.0 * AcclPattern;
		if ( ThermCndct > SkinCndctMax ) ThermCndct = SkinCndctMax;

		// PASSIVE ENERGY BALANCE EQUATIONS.
		// TOTAL METABOLIC HEAT PRODUCTION RATE, ActLevel, IN W/M**2.
		ActLevelTot = ActLevel + ShivResponse;
		IntHeatProdTot = ActLevelTot - WorkEff;
		// RESPIRATION HEAT LOSS, RespHeatLoss, IN W/M**0.
		LatRespHeatLoss = 0.0023 * ActLevelTot * ( 44.0 - VapPress );
		DryRespHeatLoss = 0.0014 * ActLevelTot * ( 34.0 - AirTemp );
		RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss;
		// HEAT FLOW FROM CORE TO SKIN, HeatFlow, IN W/M**2.
		HeatFlow = ThermCndct * ( CoreTemp - SkinTemp );
		// TempChange(1) = CoreTempChange/TempChange, IN C/HR.
		TempChange( 1 ) = ( IntHeatProdTot - RespHeatLoss - HeatFlow ) / CoreThermCap;
		if ( EvapHeatLoss > EvapHeatLossMax ) EvapHeatLoss = EvapHeatLossMax;

		// DRY HEAT EXCHANGE BY RADIATION & CONVECTION, R+C, IN W/M**2.
		DryHeatLoss = H * CloBodyRat * CloThermEff * ( SkinTemp - OpTemp );
		// TempChange(2) = SkinTempChange/TempChange, IN C/HR.
		TempChange( 2 ) = ( HeatFlow - EvapHeatLoss - DryHeatLoss ) / SkinThermCap;

	}

	void
	RKG(
		int & NEQ,
		Real64 & H,
		Real64 & X,
		Array1A< Real64 > Y,
		Array1A< Real64 > DY,
		Array1A< Real64 > C
	)
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   January 2000
		//     MODIFIED       Rick Strand (for E+ implementation February 2000)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is a subroutine for integration by Runga-Kutta's method.

		// METHODOLOGY EMPLOYED:
		// This subroutine is based heavily upon the work performed by Dan Maloney for
		// the BLAST program.  Many of the equations are based on the original Pierce
		// development.  See documentation for further details and references.

		// REFERENCES:
		// Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		Y.dim( NEQ );
		DY.dim( NEQ );
		C.dim( NEQ );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int I;
		int J;
		Real64 B;
		Real64 H2;
		static Array1D< Real64 > const A( 2, { 0.29289321881345, 1.70710678118654 } );

		H2 = 0.5 * H;

		DERIV( NEQ, Y, DY );
		for ( I = 1; I <= NEQ; ++I ) {
			B = H2 * DY( I ) - C( I );
			Y( I ) += B;
			C( I ) += 3.0 * B - H2 * DY( I );
		}

		X += H2;

		for ( J = 1; J <= 2; ++J ) {
			DERIV( NEQ, Y, DY );
			for ( I = 1; I <= NEQ; ++I ) {
				B = A( J ) * ( H * DY( I ) - C( I ) );
				Y( I ) += B;
				C( I ) += 3.0 * B - A( J ) * H * DY( I );
			}
		}

		X += H2;
		DERIV( NEQ, Y, DY );

		for ( I = 1; I <= NEQ; ++I ) {
			B = ( H * DY( I ) - 2.0 * C( I ) ) / 6.0;
			Y( I ) += B;
			C( I ) += 3.0 * B - H2 * DY( I );
		}

		DERIV( NEQ, Y, DY );

	}

	void
	GetAngleFactorList()
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   July 2001
		//     MODIFIED       na
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataGlobals;
		using namespace DataHeatBalance;
		using DataSurfaces::Surface;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const AngleFacLimit( 0.01 ); // To set the limit of sum of angle factors
		int const MaxSurfaces( 20 ); // Maximum number of surfaces in each AngleFactor List

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  CHARACTER(len=MaxNameLength),  &
		//        DIMENSION(22)       :: Alphas                  ! Alpha strings from Input Processor
		Real64 AllAngleFacSummed; // Sum of angle factors in each zone
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus;
		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas from InputProcessor
		//unused1208  REAL(r64), DIMENSION(20)       :: Numbers                 ! Numbers from Input Processor
		int NumNumbers; // Number of Numbers from Input Processor
		int NumOfAngleFactorLists; // Number of Angle Factor Lists found in IDF
		int SurfNum; // Surface number DO loop counter
		int WhichAFList; // Used in validating AngleFactorList

		cCurrentModuleObject = "ComfortViewFactorAngles";
		NumOfAngleFactorLists = GetNumObjectsFound( cCurrentModuleObject );
		AngleFactorList.allocate( NumOfAngleFactorLists );
		for ( auto & e : AngleFactorList ) {
			e.Name.clear();
			e.ZoneName.clear();
			e.ZonePtr = 0;
		}

		for ( Item = 1; Item <= NumOfAngleFactorLists; ++Item ) {

			AllAngleFacSummed = 0.0;

			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			AngleFactorList( Item ).Name = cAlphaArgs( 1 ); // no need for verification/uniqueness.
			AngleFactorList( Item ).ZoneName = cAlphaArgs( 2 );
			AngleFactorList( Item ).ZonePtr = FindItemInList( cAlphaArgs( 2 ), Zone );
			if ( AngleFactorList( Item ).ZonePtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid - not found" );
				ShowContinueError( "...invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ErrorsFound = true;
			}

			AngleFactorList( Item ).TotAngleFacSurfaces = NumNumbers;
			if ( AngleFactorList( Item ).TotAngleFacSurfaces > MaxSurfaces ) {
				ShowSevereError( cCurrentModuleObject + ": Too many surfaces specified in " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			AngleFactorList( Item ).SurfaceName.allocate( AngleFactorList( Item ).TotAngleFacSurfaces );
			AngleFactorList( Item ).SurfacePtr.allocate( AngleFactorList( Item ).TotAngleFacSurfaces );
			AngleFactorList( Item ).AngleFactor.allocate( AngleFactorList( Item ).TotAngleFacSurfaces );

			for ( SurfNum = 1; SurfNum <= AngleFactorList( Item ).TotAngleFacSurfaces; ++SurfNum ) {
				AngleFactorList( Item ).SurfaceName( SurfNum ) = cAlphaArgs( SurfNum + 2 );
				AngleFactorList( Item ).SurfacePtr( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + 2 ), Surface );
				AngleFactorList( Item ).AngleFactor( SurfNum ) = rNumericArgs( SurfNum );
				// Error trap for surfaces that do not exist or surfaces not in the zone
				if ( AngleFactorList( Item ).SurfacePtr( SurfNum ) == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( SurfNum + 2 ) + ", entered value=" + cAlphaArgs( SurfNum + 2 ) );
					ShowContinueError( "ref " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + " not found in " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ErrorsFound = true;
				} else if ( AngleFactorList( Item ).ZonePtr != 0 ) { // don't look at invalid zones
					// Found Surface, is it in same zone tagged for Angle Factor List?
					if ( AngleFactorList( Item ).ZonePtr != Surface( AngleFactorList( Item ).SurfacePtr( SurfNum ) ).Zone ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid - mismatch " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"" );
						ShowContinueError( "... does not match " + cAlphaFieldNames( 2 ) + "=\"" + Zone( Surface( AngleFactorList( Item ).SurfacePtr( SurfNum ) ).Zone ).Name + "\" for " + cAlphaFieldNames( SurfNum + 2 ) + "=\"" + cAlphaArgs( SurfNum + 2 ) + "\"." );
						ErrorsFound = true;
					}
				}

				AllAngleFacSummed += AngleFactorList( Item ).AngleFactor( SurfNum );

			}

			if ( std::abs( AllAngleFacSummed - 1.0 ) > AngleFacLimit ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid - Sum[AngleFactors]" );
				ShowContinueError( "...Sum of Angle Factors [" + RoundSigDigits( AllAngleFacSummed, 3 ) + "] exceed expected sum [1.0] by more than limit [" + RoundSigDigits( AngleFacLimit, 3 ) + "]." );
				ErrorsFound = true;
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetAngleFactorList: Program terminated due to preceding errors." );
		}

		for ( Item = 1; Item <= TotPeople; ++Item ) {
			if ( People( Item ).MRTCalcType != AngleFactor ) continue;
			People( Item ).AngleFactorListPtr = FindItemInList( People( Item ).AngleFactorListName, AngleFactorList );
			WhichAFList = People( Item ).AngleFactorListPtr;
			if ( WhichAFList == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + People( Item ).AngleFactorListName + "\", invalid" );
				ShowSevereError( "... Angle Factor List Name not found for PEOPLE= " + People( Item ).Name );
				ErrorsFound = true;
			} else if ( People( Item ).ZonePtr != AngleFactorList( WhichAFList ).ZonePtr ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + AngleFactorList( WhichAFList ).Name + " mismatch Zone Name" );
				ShowContinueError( "...Zone=\"" + AngleFactorList( WhichAFList ).ZoneName + " does not match Zone=\"" + Zone( People( Item ).ZonePtr ).Name + "\" in PEOPLE=\"" + People( Item ).Name + "\"." );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetAngleFactorList: Program terminated due to preceding errors." );
		}

	}

	Real64
	CalcAngleFactorMRT( int const AngleFacNum )
	{

		// SUBROUTINE INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   July 2001
		//     MODIFIED       na
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// THIS IS A SUBROUTINE TO CALCULATE ANGLE FACTOR MRT

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalSurface::TH;

		// Return value
		Real64 CalcAngleFactorMRT;

		// Locals
		Real64 SurfaceTemp;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum;
		Real64 SurfTempAngleFacSummed;

		// FLOW:

		SurfTempAngleFacSummed = 0.0;

		for ( SurfNum = 1; SurfNum <= AngleFactorList( AngleFacNum ).TotAngleFacSurfaces; ++SurfNum ) {

			SurfaceTemp = TH( 2, 1, AngleFactorList( AngleFacNum ).SurfacePtr( SurfNum ) );
			SurfTempAngleFacSummed += SurfaceTemp * AngleFactorList( AngleFacNum ).AngleFactor( SurfNum );

		}

		CalcAngleFactorMRT = SurfTempAngleFacSummed;

		return CalcAngleFactorMRT;

	}

	Real64
	CalcSatVapPressFromTemp( Real64 const Temp )
	{

		// FUNCTION INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   January 2000
		//     MODIFIED       Rick Strand (for E+ implementation February 2000)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// THIS IS A FUNCTION TO CALCULATE THE SATURATED VAPOR PRESSURE
		// FROM AIR TEMPERATURE

		// METHODOLOGY EMPLOYED:
		// This function is based upon the work performed by Dan Maloney for
		// the BLAST program.
		// REFERENCES:
		// Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

		// USE STATEMENTS:
		// na

		// Return value

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

		// FLOW

		Real64 const XT( Temp / 100.0 );
		return 6.16796 + 358.1855 * pow_2( XT ) - 550.3543 * pow_3( XT ) + 1048.8115 * pow_4( XT );

	}

	Real64
	CalcRadTemp( int const PeopleListNum ) // Type of MRT calculation (zone averaged or surface weighted)
	{

		// FUNCTION INFORMATION:
		//     AUTHOR         Jaewook Lee
		//     DATE WRITTEN   November 2000
		//     MODIFIED       Rick Strand (for E+ implementation November 2000)
		//                    Rick Strand (for high temperature radiant heaters March 2001)
		//     RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// THIS IS A FUNCTION TO CALCULATE EITHER ZONE AVERAGED MRT OR
		// SURFACE WEIGHTED MRT

		// METHODOLOGY EMPLOYED:
		// The method here is fairly straight-forward.  If the user has selected
		// a zone average MRT calculation, then there is nothing to do other than
		// to assign the function value because the zone MRT has already been
		// calculated.  Note that this value is an "area-emissivity" weighted value.
		// If the user wants to place the occupant "near" a particular surface,
		// then at the limit half of the radiant field will be from this surface.
		// As a result, an average of the zone MRT and the surface temperature
		// is taken to arrive at an approximate radiant temperature.
		// If a high temperature radiant heater is present, then this must also be
		// taken into account.  The equation used to account for this factor is
		// based on equation 49 on page 150 of Fanger's text (see reference below).
		// The additional assumptions for EnergyPlus are that the radiant energy
		// from the heater must be spread over the average area of a human being
		// (see parameter below) and that the emissivity and absorptivity of the
		// occupant are equivalent for the dominant wavelength of radiant energy
		// from the heater.  These assumptions might be off slightly, but it does
		// allow for an approximation of the effects of surfaces and heaters
		// within a space.  Future additions might include the effect of direct
		// solar energy on occupants.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::QHTRadSysToPerson;
		using DataHeatBalFanSys::QHWBaseboardToPerson;
		using DataHeatBalFanSys::QSteamBaseboardToPerson;
		using DataHeatBalFanSys::QElecBaseboardToPerson;
		using DataHeatBalSurface::TH;

		// Return value
		Real64 CalcRadTemp;

		// Locals
		Real64 SurfaceTemp;

		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const AreaEff( 1.8 ); // Effective area of a "standard" person in meters squared
		//  REAL(r64), PARAMETER :: KelvinConv = KelvinConv                ! Conversion from Celsius to Kelvin
		Real64 const StefanBoltzmannConst( 5.6697e-8 ); // Stefan-Boltzmann constant in W/(m2*K4)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneRadTemp;

		// FLOW:
		{ auto const SELECT_CASE_var( People( PeopleListNum ).MRTCalcType );

		if ( SELECT_CASE_var == ZoneAveraged ) {
			RadTemp = MRT( ZoneNum );
		} else if ( SELECT_CASE_var == SurfaceWeighted ) {
			ZoneRadTemp = MRT( ZoneNum );
			SurfaceTemp = TH( 2, 1, People( PeopleListNum ).SurfacePtr );
			RadTemp = ( ZoneRadTemp + SurfaceTemp ) / 2.0;
		} else if ( SELECT_CASE_var == AngleFactor ) {
			RadTemp = CalcAngleFactorMRT( People( PeopleListNum ).AngleFactorListPtr );

		}}

		// If high temperature radiant heater present and on, then must account for this in MRT calculation
		if ( QHTRadSysToPerson( ZoneNum ) > 0.0 || QHWBaseboardToPerson( ZoneNum ) > 0.0 || QSteamBaseboardToPerson( ZoneNum ) > 0.0 || QElecBaseboardToPerson( ZoneNum ) > 0.0 ) {
			RadTemp += KelvinConv; // Convert to Kelvin
			RadTemp = root_4( pow_4( RadTemp ) + ( ( QHTRadSysToPerson( ZoneNum ) + QHWBaseboardToPerson( ZoneNum ) + QSteamBaseboardToPerson( ZoneNum ) + QElecBaseboardToPerson( ZoneNum ) ) / AreaEff / StefanBoltzmannConst ) );
			RadTemp -= KelvinConv; // Convert back to Celsius
		}

		CalcRadTemp = RadTemp;

		return CalcRadTemp;

	}

	void
	CalcThermalComfortSimpleASH55()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   June 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Determines if the space is within the ASHRAE 55-2004 comfort region
		//   based on operative temperature and humidity ratio

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using OutputReportTabular::isInQuadrilateral;
		using General::RoundSigDigits;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::RunPeriodEnvironment;
		using DataEnvironment::EnvironmentStartEnd;
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OperTemp;
		Real64 HumidRatio;
		Real64 CurAirTemp;
		Real64 CurMeanRadiantTemp;
		Real64 NumberOccupants;
		bool isComfortableWithSummerClothes;
		bool isComfortableWithWinterClothes;
		int iPeople;
		int iZone;
		Real64 allowedHours;
		bool showWarning;

		AnyZoneTimeNotSimpleASH55Summer = 0.0;
		AnyZoneTimeNotSimpleASH55Winter = 0.0;
		AnyZoneTimeNotSimpleASH55Either = 0.0;

		//assume the zone is unoccupied
		for ( auto & e : ThermalComfortInASH55 ) e.ZoneIsOccupied = false;
		//loop through the people objects and determine if the zone is currently occupied
		for ( iPeople = 1; iPeople <= TotPeople; ++iPeople ) {
			ZoneNum = People( iPeople ).ZonePtr;
			NumberOccupants = People( iPeople ).NumberOfPeople * GetCurrentScheduleValue( People( iPeople ).NumberOfPeoplePtr );
			if ( NumberOccupants > 0 ) {
				ThermalComfortInASH55( ZoneNum ).ZoneIsOccupied = true;
			}
		}
		//loop through the zones and determine if in simple ashrae 55 comfort regions
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			if ( ThermalComfortInASH55( iZone ).ZoneIsOccupied ) {
				//keep track of occupied hours
				ZoneOccHrs( iZone ) += TimeStepZone;
				if ( IsZoneDV( iZone ) || IsZoneUI( iZone ) ) {
					CurAirTemp = TCMF( iZone );
				} else {
					CurAirTemp = ZTAV( iZone );
				}
				CurMeanRadiantTemp = MRT( iZone );
				OperTemp = CurAirTemp * 0.5 + CurMeanRadiantTemp * 0.5;
				HumidRatio = ZoneAirHumRat( iZone );
				//for debugging
				//ThermalComfortInASH55(iZone)%dCurAirTemp = CurAirTemp
				//ThermalComfortInASH55(iZone)%dCurMeanRadiantTemp = CurMeanRadiantTemp
				//ThermalComfortInASH55(iZone)%dOperTemp = OperTemp
				//ThermalComfortInASH55(iZone)%dHumidRatio = HumidRatio
				// From ASHRAE Standard 55-2004 Appendix D
				//  Run    AirTemp(C)   RH(%)  Season  HumidRatio
				//   1       19.6        86    Winter    0.012
				//   2       23.9        66    Winter    0.012
				//   3       25.7        15    Winter    0.003
				//   4       21.2        20    Winter    0.003
				//   5       23.6        67    Summer    0.012
				//   6       26.8        56    Summer    0.012
				//   7       27.9        13    Summer    0.003
				//   8       24.7        16    Summer    0.003
				// But the standard says "no recommended lower humidity limit" so it should
				// really extend down to the 0.0 Humidity ratio line.  Extrapolating we get
				// the values that are shown in the following table
				//  Run    AirTemp(C)    Season  HumidRatio
				//   1       19.6        Winter    0.012
				//   2       23.9        Winter    0.012
				//   3       26.3        Winter    0.000
				//   4       21.7        Winter    0.000
				//   5       23.6        Summer    0.012
				//   6       26.8        Summer    0.012
				//   7       28.3        Summer    0.000
				//   8       25.1        Summer    0.000
				//check summer clothing conditions
				isComfortableWithSummerClothes = isInQuadrilateral( OperTemp, HumidRatio, 25.1, 0.0, 23.6, 0.012, 26.8, 0.012, 28.3, 0.0 );
				//check winter clothing conditions
				isComfortableWithWinterClothes = isInQuadrilateral( OperTemp, HumidRatio, 21.7, 0.0, 19.6, 0.012, 23.9, 0.012, 26.3, 0.0 );
				if ( isComfortableWithSummerClothes ) {
					ThermalComfortInASH55( iZone ).timeNotSummer = 0.0;
				} else {
					ThermalComfortInASH55( iZone ).timeNotSummer = TimeStepZone;
					ThermalComfortInASH55( iZone ).totalTimeNotSummer += TimeStepZone;
					AnyZoneTimeNotSimpleASH55Summer = TimeStepZone;
				}
				if ( isComfortableWithWinterClothes ) {
					ThermalComfortInASH55( iZone ).timeNotWinter = 0.0;
				} else {
					ThermalComfortInASH55( iZone ).timeNotWinter = TimeStepZone;
					ThermalComfortInASH55( iZone ).totalTimeNotWinter += TimeStepZone;
					AnyZoneTimeNotSimpleASH55Winter = TimeStepZone;
				}
				if ( isComfortableWithSummerClothes || isComfortableWithWinterClothes ) {
					ThermalComfortInASH55( iZone ).timeNotEither = 0.0;
				} else {
					ThermalComfortInASH55( iZone ).timeNotEither = TimeStepZone;
					ThermalComfortInASH55( iZone ).totalTimeNotEither += TimeStepZone;
					AnyZoneTimeNotSimpleASH55Either = TimeStepZone;
				}
			} else {
				//when no one present in that portion of the zone then no one can be uncomfortable
				ThermalComfortInASH55( iZone ).timeNotSummer = 0.0;
				ThermalComfortInASH55( iZone ).timeNotWinter = 0.0;
				ThermalComfortInASH55( iZone ).timeNotEither = 0.0;
			}
		}
		// accumulate total time
		TotalAnyZoneTimeNotSimpleASH55Summer += AnyZoneTimeNotSimpleASH55Summer;
		TotalAnyZoneTimeNotSimpleASH55Winter += AnyZoneTimeNotSimpleASH55Winter;
		TotalAnyZoneTimeNotSimpleASH55Either += AnyZoneTimeNotSimpleASH55Either;
		//was EndEnvrnsFlag prior to CR7562
		if ( EndDesignDayEnvrnsFlag ) {
			allowedHours = double( NumOfDayInEnvrn ) * 24.0 * 0.04;
			//first check if warning should be printed
			showWarning = false;
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				if ( ThermalComfortInASH55( iZone ).Enable55Warning ) {
					if ( ThermalComfortInASH55( iZone ).totalTimeNotEither > allowedHours ) {
						showWarning = true;
					}
				}
			}
			//if any zones should be warning print it out
			if ( showWarning ) {
				ShowWarningError( "More than 4% of time (" + RoundSigDigits( allowedHours, 1 ) + " hours) uncomfortable in one or more zones " );
				ShowContinueError( "Based on ASHRAE 55-2004 graph (Section 5.2.1.1)" );
				if ( RunPeriodEnvironment ) {
					ShowContinueError( "During Environment [" + EnvironmentStartEnd + "]: " + EnvironmentName );
				} else {
					ShowContinueError( "During SizingPeriod Environment [" + EnvironmentStartEnd + "]: " + EnvironmentName );
				}
				for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
					if ( ThermalComfortInASH55( iZone ).Enable55Warning ) {
						if ( ThermalComfortInASH55( iZone ).totalTimeNotEither > allowedHours ) {
							ShowContinueError( RoundSigDigits( ThermalComfortInASH55( iZone ).totalTimeNotEither, 1 ) + " hours were uncomfortable in zone: " + Zone( iZone ).Name );
						}
					}
				}
			}
			// put in predefined reports
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				PreDefTableEntry( pdchSCwinterClothes, Zone( iZone ).Name, ThermalComfortInASH55( iZone ).totalTimeNotWinter );
				PreDefTableEntry( pdchSCsummerClothes, Zone( iZone ).Name, ThermalComfortInASH55( iZone ).totalTimeNotSummer );
				PreDefTableEntry( pdchSCeitherClothes, Zone( iZone ).Name, ThermalComfortInASH55( iZone ).totalTimeNotEither );
			}
			PreDefTableEntry( pdchSCwinterClothes, "Facility", TotalAnyZoneTimeNotSimpleASH55Winter );
			PreDefTableEntry( pdchSCsummerClothes, "Facility", TotalAnyZoneTimeNotSimpleASH55Summer );
			PreDefTableEntry( pdchSCeitherClothes, "Facility", TotalAnyZoneTimeNotSimpleASH55Either );
			//set value for ABUPS report
			TotalTimeNotSimpleASH55EitherForABUPS = TotalAnyZoneTimeNotSimpleASH55Either;
			//reset accumulation for new environment
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				ThermalComfortInASH55( iZone ).totalTimeNotWinter = 0.0;
				ThermalComfortInASH55( iZone ).totalTimeNotSummer = 0.0;
				ThermalComfortInASH55( iZone ).totalTimeNotEither = 0.0;
			}
			TotalAnyZoneTimeNotSimpleASH55Winter = 0.0;
			TotalAnyZoneTimeNotSimpleASH55Summer = 0.0;
			TotalAnyZoneTimeNotSimpleASH55Either = 0.0;
			// report how the aggregation is conducted
			{ auto const SELECT_CASE_var( KindOfSim );
			if ( SELECT_CASE_var == ksDesignDay ) {
				addFootNoteSubTable( pdstSimpleComfort, "Aggregated over the Design Days" );
			} else if ( SELECT_CASE_var == ksRunPeriodDesign ) {
				addFootNoteSubTable( pdstSimpleComfort, "Aggregated over the RunPeriods for Design" );
			} else if ( SELECT_CASE_var == ksRunPeriodWeather ) {
				addFootNoteSubTable( pdstSimpleComfort, "Aggregated over the RunPeriods for Weather" );
			}}
			//report number of occupied hours per week for LEED report
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				PreDefTableEntry( pdchLeedSutHrsWeek, Zone( iZone ).Name, 7 * 24 * ( ZoneOccHrs( iZone ) / ( NumOfDayInEnvrn * 24 ) ) );
			}
		}
	}

	void
	ResetThermalComfortSimpleASH55(){
		// Jason Glazer - October 2015
		// Reset thermal comfort table gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		int iZone;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			ThermalComfortInASH55( iZone ).totalTimeNotWinter = 0.0;
			ThermalComfortInASH55( iZone ).totalTimeNotSummer = 0.0;
			ThermalComfortInASH55( iZone ).totalTimeNotEither = 0.0;
		}
		TotalAnyZoneTimeNotSimpleASH55Winter = 0.0;
		TotalAnyZoneTimeNotSimpleASH55Summer = 0.0;
		TotalAnyZoneTimeNotSimpleASH55Either = 0.0;
	}

	void
	CalcIfSetPointMet()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Report if the setpoint temperature has been met.
		//   Add calculation of how far away from setpoint and if setpoint was not met
		//   during all times and during occupancy.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using DataHeatBalFanSys::TempTstatAir;
		using DataHeatBalFanSys::TempControlType;
		using namespace OutputReportPredefined;
		using DataHVACGlobals::SingleHeatingSetPoint;
		using DataHVACGlobals::SingleCoolingSetPoint;
		using DataHVACGlobals::SingleHeatCoolSetPoint;
		using DataHVACGlobals::DualSetPointWithDeadBand;
		using DataHVACGlobals::deviationFromSetPtThresholdHtg;
		using DataHVACGlobals::deviationFromSetPtThresholdClg;
		using DataRoomAirModel::AirModel;
		using DataRoomAirModel::RoomAirModel_Mixing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SensibleLoadPredictedNoAdj;
		Real64 deltaT;
		int iZone;
		bool testHeating;
		bool testCooling;

		// Get the load predicted - the sign will indicate if heating or cooling
		// was called for
		AnyZoneNotMetHeating = 0.0;
		AnyZoneNotMetCooling = 0.0;
		AnyZoneNotMetOccupied = 0.0;
		AnyZoneNotMetHeatingOccupied = 0.0;
		AnyZoneNotMetCoolingOccupied = 0.0;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			SensibleLoadPredictedNoAdj = ZoneSysEnergyDemand( iZone ).TotalOutputRequired;
			ThermalComfortSetPoint( iZone ).notMetCooling = 0.0;
			ThermalComfortSetPoint( iZone ).notMetHeating = 0.0;
			ThermalComfortSetPoint( iZone ).notMetCoolingOccupied = 0.0;
			ThermalComfortSetPoint( iZone ).notMetHeatingOccupied = 0.0;
			{ auto const SELECT_CASE_var( TempControlType( iZone ) );
			if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
				testHeating = true;
				testCooling = false;
			} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
				testHeating = false;
				testCooling = true;
			} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
				testHeating = true;
				testCooling = true;
			} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
				testHeating = true;
				testCooling = true;
			} else {
				testHeating = true;
				testCooling = true;
			}}
			if ( testHeating && ( SensibleLoadPredictedNoAdj > 0 ) ) { //heating
				if ( AirModel( iZone ).AirModelType != RoomAirModel_Mixing ) {
					deltaT = TempTstatAir( iZone ) - ZoneThermostatSetPointLo( iZone );
				} else {
					deltaT = ZTAV( iZone ) - ZoneThermostatSetPointLo( iZone );
				}
				if ( deltaT < deviationFromSetPtThresholdHtg ) {
					ThermalComfortSetPoint( iZone ).notMetHeating = TimeStepZone;
					ThermalComfortSetPoint( iZone ).totalNotMetHeating += TimeStepZone;
					if ( AnyZoneNotMetHeating == 0.0 ) AnyZoneNotMetHeating = TimeStepZone;
					if ( ThermalComfortInASH55( iZone ).ZoneIsOccupied ) {
						ThermalComfortSetPoint( iZone ).notMetHeatingOccupied = TimeStepZone;
						ThermalComfortSetPoint( iZone ).totalNotMetHeatingOccupied += TimeStepZone;
						if ( AnyZoneNotMetHeatingOccupied == 0.0 ) AnyZoneNotMetHeatingOccupied = TimeStepZone;
						if ( AnyZoneNotMetOccupied == 0.0 ) AnyZoneNotMetOccupied = TimeStepZone;
					}
				}
			} else if ( testCooling && ( SensibleLoadPredictedNoAdj < 0 ) ) { //cooling
				if ( AirModel( iZone ).AirModelType != RoomAirModel_Mixing ) {
					deltaT = TempTstatAir( iZone ) - ZoneThermostatSetPointHi( iZone );
				} else {
					deltaT = ZTAV( iZone ) - ZoneThermostatSetPointHi( iZone );
				}
				if ( deltaT > deviationFromSetPtThresholdClg ) {
					ThermalComfortSetPoint( iZone ).notMetCooling = TimeStepZone;
					ThermalComfortSetPoint( iZone ).totalNotMetCooling += TimeStepZone;
					if ( AnyZoneNotMetCooling == 0.0 ) AnyZoneNotMetCooling = TimeStepZone;
					if ( ThermalComfortInASH55( iZone ).ZoneIsOccupied ) {
						ThermalComfortSetPoint( iZone ).notMetCoolingOccupied = TimeStepZone;
						ThermalComfortSetPoint( iZone ).totalNotMetCoolingOccupied += TimeStepZone;
						if ( AnyZoneNotMetCoolingOccupied == 0.0 ) AnyZoneNotMetCoolingOccupied = TimeStepZone;
						if ( AnyZoneNotMetOccupied == 0.0 ) AnyZoneNotMetOccupied = TimeStepZone;
					}
				}
			}
		}
		TotalAnyZoneNotMetHeating += AnyZoneNotMetHeating;
		TotalAnyZoneNotMetCooling += AnyZoneNotMetCooling;
		TotalAnyZoneNotMetHeatingOccupied += AnyZoneNotMetHeatingOccupied;
		TotalAnyZoneNotMetCoolingOccupied += AnyZoneNotMetCoolingOccupied;
		TotalAnyZoneNotMetOccupied += AnyZoneNotMetOccupied;

		//was EndEnvrnsFlag prior to CR7562
		if ( EndDesignDayEnvrnsFlag ) {
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				PreDefTableEntry( pdchULnotMetHeat, Zone( iZone ).Name, ThermalComfortSetPoint( iZone ).totalNotMetHeating );
				PreDefTableEntry( pdchULnotMetCool, Zone( iZone ).Name, ThermalComfortSetPoint( iZone ).totalNotMetCooling );
				PreDefTableEntry( pdchULnotMetHeatOcc, Zone( iZone ).Name, ThermalComfortSetPoint( iZone ).totalNotMetHeatingOccupied );
				PreDefTableEntry( pdchULnotMetCoolOcc, Zone( iZone ).Name, ThermalComfortSetPoint( iZone ).totalNotMetCoolingOccupied );
			}
			PreDefTableEntry( pdchULnotMetHeat, "Facility", TotalAnyZoneNotMetHeating );
			PreDefTableEntry( pdchULnotMetCool, "Facility", TotalAnyZoneNotMetCooling );
			PreDefTableEntry( pdchULnotMetHeatOcc, "Facility", TotalAnyZoneNotMetHeatingOccupied );
			PreDefTableEntry( pdchULnotMetCoolOcc, "Facility", TotalAnyZoneNotMetCoolingOccupied );
			//set value for ABUPS report
			TotalNotMetHeatingOccupiedForABUPS = TotalAnyZoneNotMetHeatingOccupied;
			TotalNotMetCoolingOccupiedForABUPS = TotalAnyZoneNotMetCoolingOccupied;
			TotalNotMetOccupiedForABUPS = TotalAnyZoneNotMetOccupied;
			//reset counters
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				ThermalComfortSetPoint( iZone ).totalNotMetHeating = 0.0;
				ThermalComfortSetPoint( iZone ).totalNotMetCooling = 0.0;
				ThermalComfortSetPoint( iZone ).totalNotMetHeatingOccupied = 0.0;
				ThermalComfortSetPoint( iZone ).totalNotMetCoolingOccupied = 0.0;
			}
			TotalAnyZoneNotMetHeating = 0.0;
			TotalAnyZoneNotMetCooling = 0.0;
			TotalAnyZoneNotMetHeatingOccupied = 0.0;
			TotalAnyZoneNotMetCoolingOccupied = 0.0;
			TotalAnyZoneNotMetOccupied = 0.0;
			// report how the aggregation is conducted
			{ auto const SELECT_CASE_var( KindOfSim );
			if ( SELECT_CASE_var == ksDesignDay ) {
				addFootNoteSubTable( pdstUnmetLoads, "Aggregated over the Design Days" );
			} else if ( SELECT_CASE_var == ksRunPeriodDesign ) {
				addFootNoteSubTable( pdstUnmetLoads, "Aggregated over the RunPeriods for Design" );
			} else if ( SELECT_CASE_var == ksRunPeriodWeather ) {
				addFootNoteSubTable( pdstUnmetLoads, "Aggregated over the RunPeriods for Weather" );
			}}
		}
	}

	void
	ResetSetPointMet(){
		// Jason Glazer - October 2015
		// Reset set point not met table gathering arrays to zero for multi-year simulations
		// so that only last year is reported in tabular reports
		int iZone;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			ThermalComfortSetPoint( iZone ).totalNotMetHeating = 0.0;
			ThermalComfortSetPoint( iZone ).totalNotMetCooling = 0.0;
			ThermalComfortSetPoint( iZone ).totalNotMetHeatingOccupied = 0.0;
			ThermalComfortSetPoint( iZone ).totalNotMetCoolingOccupied = 0.0;
		}
		TotalAnyZoneNotMetHeating = 0.0;
		TotalAnyZoneNotMetCooling = 0.0;
		TotalAnyZoneNotMetHeatingOccupied = 0.0;
		TotalAnyZoneNotMetCoolingOccupied = 0.0;
		TotalAnyZoneNotMetOccupied = 0.0;
	}


	void
	CalcThermalComfortAdaptiveASH55(
		bool const initiate, // true if supposed to initiate
		Optional_bool_const wthrsim, // true if this is a weather simulation
		Optional< Real64 const > avgdrybulb // approximate avg drybulb for design day.  will be used as previous period in design day
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tyler Hoyt
		//       DATE WRITTEN   July 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sets up and carries out ASHRAE55-2010 adaptive comfort model calculations.
		// Output provided are state variables for the 80% and 90% acceptability limits
		// in the model, the comfort temperature, and the 30-day running average or
		// monthly average outdoor air temperature as parsed from the .STAT file.

		// METHODOLOGY EMPLOYED:
		// In order for the calculations to be possible the user must provide either
		// a .STAT file or .EPW file for the purpose of computing a monthly average
		// temperature or thirty-day running average. The subroutine need only open
		// the relevant file once to initialize, and then operates within the loop.

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using General::InvJulianDay;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::DayOfYear;
		using DataEnvironment::Month;
		using OutputReportTabular::GetColumnUsingTabs;
		using OutputReportTabular::StrToReal;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::string lineIn;
		std::string lineAvg;
		std::string epwLine;
		static Real64 avgDryBulbASH( 0.0 );
		Real64 dryBulb;
		static Real64 runningAverageASH( 0.0 );
		static Array1D< Real64 > monthlyTemp( 12, 0.0 );
		Real64 tComf;
		Real64 numOccupants;
		int statFile;
		int epwFile;
		bool statFileExists;
		bool epwFileExists;
		static bool useStatData( false );
		static bool useEpwData( false );
		int readStat;
		int jStartDay;
		int calcStartDay;
		int calcStartHr;
		int calcEndDay;
		int calcEndHr;
		std::string::size_type pos;
		int ind;
		int i;
		int j;
		bool weathersimulation;
		Real64 inavgdrybulb;

		if ( initiate ) { // not optional on initiate=true.  would otherwise check for presence
			weathersimulation = wthrsim;
			avgDryBulbASH = 0.0;
			runningAverageASH = 0.0;
			monthlyTemp = 0.0;
			inavgdrybulb = avgdrybulb;
		} else {
			weathersimulation = false;
			inavgdrybulb = 0.0;
		}

		if ( initiate && weathersimulation ) {
			{ IOFlags flags; gio::inquire( DataStringGlobals::inStatFileName, flags ); statFileExists = flags.exists(); }
			{ IOFlags flags; gio::inquire( DataStringGlobals::inputWeatherFileName, flags ); epwFileExists = flags.exists(); }
			readStat = 0;
			if ( statFileExists ) {
				statFile = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "READ" ); gio::open( statFile, DataStringGlobals::inStatFileName, flags ); readStat = flags.ios(); }
				if ( readStat != 0 ) {
					ShowFatalError( "CalcThermalComfortAdaptiveASH55: Could not open file "+DataStringGlobals::inStatFileName+" for input (read)." );
				}
				while ( readStat == 0 ) {
					{ IOFlags flags; gio::read( statFile, fmtA, flags ) >> lineIn; readStat = flags.ios(); }
					if ( has( lineIn, "Monthly Statistics for Dry Bulb temperatures" ) ) {
						for ( i = 1; i <= 7; ++i ) {
							{ IOFlags flags; gio::read( statFile, fmtA, flags ); readStat = flags.ios(); }
						}
						{ IOFlags flags; gio::read( statFile, fmtA, flags ) >> lineAvg; readStat = flags.ios(); }
						break;
					}
				}
				gio::close( statFile );
				for ( i = 1; i <= 12; ++i ) {
					monthlyTemp( i ) = StrToReal( GetColumnUsingTabs( lineAvg, i + 2 ) );
				}
				useStatData = true;
			} else if ( epwFileExists ) {
				epwFile = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "READ" ); gio::open( epwFile, DataStringGlobals::inputWeatherFileName, flags ); readStat = flags.ios(); }
				if ( readStat != 0 ) {
					ShowFatalError( "CalcThermalComfortAdaptiveASH55: Could not open file " + DataStringGlobals::inputWeatherFileName+ " for input (read)." );
				}
				for ( i = 1; i <= 9; ++i ) { // Headers
					{ IOFlags flags; gio::read( epwFile, fmtA, flags ); readStat = flags.ios(); }
				}
				jStartDay = DayOfYear - 1;
				calcStartDay = jStartDay - 30;
				if ( calcStartDay > 0 ) {
					calcStartHr = 24 * ( calcStartDay - 1 ) + 1;
					for ( i = 1; i <= calcStartHr - 1; ++i ) {
						{ IOFlags flags; gio::read( epwFile, fmtA, flags ); readStat = flags.ios(); }
					}
					for ( i = 1; i <= 30; ++i ) {
						avgDryBulbASH = 0.0;
						for ( j = 1; j <= 24; ++j ) {
							{ IOFlags flags; gio::read( epwFile, fmtA, flags ) >> epwLine; readStat = flags.ios(); }
							for ( ind = 1; ind <= 6; ++ind ) {
								pos = index( epwLine, ',' );
								epwLine.erase( 0, pos + 1 );
							}
							pos = index( epwLine, ',' );
							dryBulb = StrToReal( epwLine.substr( 0, pos ) );
							avgDryBulbASH += ( dryBulb / 24.0 );
						}
						runningAverageASH = ( 29.0 * runningAverageASH + avgDryBulbASH ) / 30.0;
					}
				} else { // Do special things for wrapping the epw
					calcEndDay = jStartDay;
					calcStartDay += 365;
					calcEndHr = 24 * calcEndDay;
					calcStartHr = 24 * ( calcStartDay - 1 ) + 1;
					for ( i = 1; i <= calcEndDay; ++i ) {
						avgDryBulbASH = 0.0;
						for ( j = 1; j <= 24; ++j ) {
							{ IOFlags flags; gio::read( epwFile, fmtA, flags ) >> epwLine; readStat = flags.ios(); }
							for ( ind = 1; ind <= 6; ++ind ) {
								pos = index( epwLine, ',' );
								epwLine.erase( 0, pos + 1 );
							}
							pos = index( epwLine, ',' );
							dryBulb = StrToReal( epwLine.substr( 0, pos ) );
							avgDryBulbASH += ( dryBulb / 24.0 );
						}
						runningAverageASH = ( 29.0 * runningAverageASH + avgDryBulbASH ) / 30.0;
					}
					for ( i = calcEndHr + 1; i <= calcStartHr - 1; ++i ) {
						{ IOFlags flags; gio::read( epwFile, fmtA, flags ); readStat = flags.ios(); }
					}
					for ( i = 1; i <= 30 - calcEndDay; ++i ) {
						avgDryBulbASH = 0.0;
						for ( j = 1; j <= 24; ++j ) {
							{ IOFlags flags; gio::read( epwFile, fmtA, flags ) >> epwLine; readStat = flags.ios(); }
							for ( ind = 1; ind <= 6; ++ind ) {
								pos = index( epwLine, ',' );
								epwLine.erase( 0, pos + 1 );
							}
							pos = index( epwLine, ',' );
							dryBulb = StrToReal( epwLine.substr( 0, pos ) );
							avgDryBulbASH += ( dryBulb / 24.0 );
						}
						runningAverageASH = ( 29.0 * runningAverageASH + avgDryBulbASH ) / 30.0;
					}
				}
				gio::close( epwFile );
				useEpwData = true;
			}
		} else if ( initiate && ! weathersimulation ) {
			runningAverageASH = inavgdrybulb;
			monthlyTemp = inavgdrybulb;
			avgDryBulbASH = 0.0;
		}

		if ( initiate ) return;

		if ( BeginDayFlag && useEpwData ) {
			// Update the running average, reset the daily avg
			runningAverageASH = ( 29.0 * runningAverageASH + avgDryBulbASH ) / 30.0;
			avgDryBulbASH = 0.0;
		}

		// If exists BeginMonthFlag we can use it to call InvJulianDay once per month.
		if ( BeginDayFlag && useStatData ) {
			//  CALL InvJulianDay(DayOfYear,pMonth,pDay,0)
			//  runningAverageASH = monthlyTemp(pMonth)
			runningAverageASH = monthlyTemp( Month );
		}

		// Update the daily average
		//IF (BeginHourFlag .and. useEpwData) THEN
		if ( BeginHourFlag ) {
			avgDryBulbASH += ( OutDryBulbTemp / 24.0 );
		}

		for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
			if ( ! People( PeopleNum ).AdaptiveASH55 ) continue;
			ZoneNum = People( PeopleNum ).ZonePtr;
			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
				AirTemp = TCMF( ZoneNum );
			} else {
				AirTemp = ZTAV( ZoneNum );
			}
			RadTemp = CalcRadTemp( PeopleNum );
			OpTemp = ( AirTemp + RadTemp ) / 2.0;
			ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = OpTemp;
			ThermalComfortData( PeopleNum ).ASHRAE55RunningMeanOutdoorTemp = runningAverageASH;
			if ( runningAverageASH >= 10.0 && runningAverageASH <= 33.5 ) {
				// Calculate the comfort here  (people/output handling loop)
				numOccupants = People( PeopleNum ).NumberOfPeople * GetCurrentScheduleValue( People( PeopleNum ).NumberOfPeoplePtr );
				tComf = 0.31 * runningAverageASH + 17.8;
				ThermalComfortData( PeopleNum ).TComfASH55 = tComf;
				if ( numOccupants > 0 ) {
					if ( OpTemp < tComf + 2.5 && OpTemp > tComf - 2.5 ) {
						// 80% and 90% limits okay
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5590 = 1;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5580 = 1;
					} else if ( OpTemp < tComf + 3.5 && OpTemp > tComf - 3.5 ) {
						// 80% only
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5590 = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5580 = 1;
						People( PeopleNum ).TimeNotMetASH5590 += SysTimeElapsed;
					} else {
						// Neither
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5590 = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5580 = 0;
						People( PeopleNum ).TimeNotMetASH5580 += SysTimeElapsed;
						People( PeopleNum ).TimeNotMetASH5590 += SysTimeElapsed;
					}
				} else {
					// Unoccupied
					ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5590 = -1;
					ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5580 = -1;
				}
			} else {
				// Monthly temp out of range
				ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5590 = -1;
				ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveASH5580 = -1;
				ThermalComfortData( PeopleNum ).TComfASH55 = -1.0;
			}
		}

	}

	void
	CalcThermalComfortAdaptiveCEN15251(
		bool const initiate, // true if supposed to initiate
		Optional_bool_const wthrsim, // true if this is a weather simulation
		Optional< Real64 const > avgdrybulb // approximate avg drybulb for design day.  will be used as previous period in design day
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tyler Hoyt
		//       DATE WRITTEN   July 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sets up and carries out CEN-15251 adaptive comfort model calculations.
		// Output provided are state variables for the Category I, II, and III
		// limits of the model, the comfort temperature, and the 5-day weighted
		// moving average of the outdoor air temperature.

		// METHODOLOGY EMPLOYED:
		//   na

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::DayOfYear;
		using DataEnvironment::Month;
		using OutputReportTabular::GetColumnUsingTabs;
		using OutputReportTabular::StrToReal;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 const alpha( 0.8 );
		static Array1D< Real64 > const alpha_pow( { pow_6( alpha ), pow_5( alpha ), pow_4( alpha ), pow_3( alpha ), pow_2( alpha ), alpha, 1.0 } ); // alpha^(7-i)
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::string epwLine;
		static Real64 avgDryBulbCEN( 0.0 );
		Real64 dryBulb;
		Real64 tComf;
		Real64 tComfLow;
		static Real64 runningAverageCEN( 0.0 );
		Real64 numOccupants;
		int epwFile;
		bool epwFileExists;
		static bool useEpwData( false );
		static bool firstDaySet( false ); // first day is set with initiate -- so do not update
		int readStat;
		int jStartDay;
		int calcStartDay;
		int calcStartHr;
		int calcEndDay;
		int calcEndHr;
		std::string::size_type pos;
		int ind;
		int i;
		int j;
		bool weathersimulation;
		Real64 inavgdrybulb;

		if ( initiate ) { // not optional on initiate=true.  would otherwise check for presence
			weathersimulation = wthrsim;
			inavgdrybulb = avgdrybulb;
			avgDryBulbCEN = 0.0;
			runningAverageCEN = 0.0;
		} else {
			weathersimulation = false;
			inavgdrybulb = 0.0;
		}

		if ( initiate && weathersimulation ) {
			{ IOFlags flags; gio::inquire( DataStringGlobals::inputWeatherFileName, flags ); epwFileExists = flags.exists(); }
			readStat = 0;
			if ( epwFileExists ) {
				epwFile = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "READ" ); gio::open( epwFile, DataStringGlobals::inputWeatherFileName, flags ); readStat = flags.ios(); }
				if ( readStat != 0 ) {
					ShowFatalError( "CalcThermalComfortAdaptiveCEN15251: Could not open file "+DataStringGlobals::inputWeatherFileName+" for input (read)." );
				}
				for ( i = 1; i <= 9; ++i ) { // Headers
					{ IOFlags flags; gio::read( epwFile, fmtA, flags ); readStat = flags.ios(); }
				}
				jStartDay = DayOfYear - 1;
				calcStartDay = jStartDay - 7;
				if ( calcStartDay > 0 ) {
					calcStartHr = 24 * ( calcStartDay - 1 ) + 1;
					for ( i = 1; i <= calcStartHr - 1; ++i ) {
						{ IOFlags flags; gio::read( epwFile, fmtA, flags ); readStat = flags.ios(); }
					}
					runningAverageCEN = 0.0;
					for ( i = 1; i <= 7; ++i ) {
						avgDryBulbCEN = 0.0;
						for ( j = 1; j <= 24; ++j ) {
							{ IOFlags flags; gio::read( epwFile, fmtA, flags ) >> epwLine; readStat = flags.ios(); }
							for ( ind = 1; ind <= 6; ++ind ) {
								pos = index( epwLine, ',' );
								epwLine.erase( 0, pos + 1 );
							}
							pos = index( epwLine, ',' );
							dryBulb = StrToReal( epwLine.substr( 0, pos ) );
							avgDryBulbCEN += ( dryBulb / 24.0 );
						}
						runningAverageCEN += alpha_pow( i ) * avgDryBulbCEN;
					}
				} else { // Do special things for wrapping the epw
					calcEndDay = jStartDay;
					calcStartDay += 365;
					calcEndHr = 24 * calcEndDay;
					calcStartHr = 24 * ( calcStartDay - 1 ) + 1;
					for ( i = 1; i <= calcEndDay; ++i ) {
						avgDryBulbCEN = 0.0;
						for ( j = 1; j <= 24; ++j ) {
							{ IOFlags flags; gio::read( epwFile, fmtA, flags ) >> epwLine; readStat = flags.ios(); }
							for ( ind = 1; ind <= 6; ++ind ) {
								pos = index( epwLine, ',' );
								epwLine.erase( 0, pos + 1 );
							}
							pos = index( epwLine, ',' );
							dryBulb = StrToReal( epwLine.substr( 0, pos ) );
							avgDryBulbCEN += ( dryBulb / 24.0 );
						}
						runningAverageCEN += std::pow( alpha, calcEndDay - i ) * avgDryBulbCEN;
					}
					for ( i = calcEndHr + 1; i <= calcStartHr - 1; ++i ) {
						{ IOFlags flags; gio::read( epwFile, fmtA, flags ); readStat = flags.ios(); }
					}
					for ( i = 1; i <= 7 - calcEndDay; ++i ) {
						avgDryBulbCEN = 0.0;
						for ( j = 1; j <= 24; ++j ) {
							{ IOFlags flags; gio::read( epwFile, fmtA, flags ) >> epwLine; readStat = flags.ios(); }
							for ( ind = 1; ind <= 6; ++ind ) {
								pos = index( epwLine, ',' );
								epwLine.erase( 0, pos + 1 );
							}
							pos = index( epwLine, ',' );
							dryBulb = StrToReal( epwLine.substr( 0, pos ) );
							avgDryBulbCEN += ( dryBulb / 24.0 );
						}
						runningAverageCEN +=  alpha_pow( i ) * avgDryBulbCEN;
					}
				}
				runningAverageCEN *= ( 1.0 - alpha );
				avgDryBulbCEN = 0.0;
				gio::close( epwFile );
				useEpwData = true;
				firstDaySet = true;
			}
		} else if ( initiate && ! weathersimulation ) {
			runningAverageCEN = inavgdrybulb;
			avgDryBulbCEN = 0.0;
		}
		if ( initiate ) return;

		if ( BeginDayFlag && ! firstDaySet ) {
			// Update the running average, reset the daily avg
			runningAverageCEN = 0.2 * runningAverageCEN + 0.8 * avgDryBulbCEN;
			avgDryBulbCEN = 0.0;
		}

		firstDaySet = false;

		// Update the daily average
		if ( BeginHourFlag ) {
			avgDryBulbCEN += ( OutDryBulbTemp / 24.0 );
		}

		for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
			if ( ! People( PeopleNum ).AdaptiveCEN15251 ) continue;
			ZoneNum = People( PeopleNum ).ZonePtr;
			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
				AirTemp = TCMF( ZoneNum );
			} else {
				AirTemp = ZTAV( ZoneNum );
			}
			RadTemp = CalcRadTemp( PeopleNum );
			OpTemp = ( AirTemp + RadTemp ) / 2.0;
			ThermalComfortData( PeopleNum ).ThermalComfortOpTemp = OpTemp;
			ThermalComfortData( PeopleNum ).CEN15251RunningMeanOutdoorTemp = runningAverageCEN;
			if ( runningAverageCEN >= 10.0 && runningAverageCEN <= 30.0 ) {
				// Calculate the comfort here (people/output handling loop)
				numOccupants = People( PeopleNum ).NumberOfPeople * GetCurrentScheduleValue( People( PeopleNum ).NumberOfPeoplePtr );
				tComf = 0.33 * runningAverageCEN + 18.8;
				ThermalComfortData( PeopleNum ).TComfCEN15251 = tComf;
				if ( numOccupants > 0 ) {
					if ( runningAverageCEN < 15 ) {
						tComfLow = 23.75; // Lower limit is constant in this region
					} else {
						tComfLow = tComf;
					}
					if ( OpTemp < tComf + 2.0 && OpTemp > tComfLow - 2.0 ) {
						// Within Cat I, II, III Limits
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatI = 1;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatII = 1;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatIII = 1;
					} else if ( OpTemp < tComf + 3.0 && OpTemp > tComfLow - 3.0 ) {
						// Within Cat II, III Limits
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatI = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatII = 1;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatIII = 1;
						People( PeopleNum ).TimeNotMetCEN15251CatI += SysTimeElapsed;
					} else if ( OpTemp < tComf + 4.0 && OpTemp > tComfLow - 4.0 ) {
						// Within Cat III Limits
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatI = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatII = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatIII = 1;
						People( PeopleNum ).TimeNotMetCEN15251CatI += SysTimeElapsed;
						People( PeopleNum ).TimeNotMetCEN15251CatII += SysTimeElapsed;
					} else {
						// None
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatI = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatII = 0;
						ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatIII = 0;
						People( PeopleNum ).TimeNotMetCEN15251CatI += SysTimeElapsed;
						People( PeopleNum ).TimeNotMetCEN15251CatII += SysTimeElapsed;
						People( PeopleNum ).TimeNotMetCEN15251CatIII += SysTimeElapsed;
					}
				} else {
					// Unoccupied
					ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatI = -1;
					ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatII = -1;
					ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatIII = -1;
				}
			} else {
				// Monthly temp out of range
				ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatI = -1;
				ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatII = -1;
				ThermalComfortData( PeopleNum ).ThermalComfortAdaptiveCEN15251CatIII = -1;
				ThermalComfortData( PeopleNum ).TComfCEN15251 = -1.0;
			}
		}

	}

	void
	DynamicClothingModel()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   June 2013
		//       MODIFIED
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// USE DataRoomAirModel, ONLY: AirModel, RoomAirModel_Mixing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TemporaryVariable;

		if ( TemporarySixAMTemperature < -5.0 ) {
			ThermalComfortData( PeopleNum ).ClothingValue = 1.0;
		} else if ( ( TemporarySixAMTemperature >= -5.0 ) && ( TemporarySixAMTemperature < 5.0 ) ) {
			ThermalComfortData( PeopleNum ).ClothingValue = 0.818 - 0.0364 * TemporarySixAMTemperature;
		} else if ( ( TemporarySixAMTemperature >= 5.0 ) && ( TemporarySixAMTemperature < 26.0 ) ) {
			TemporaryVariable = -0.1635 - 0.0066 * TemporarySixAMTemperature;
			ThermalComfortData( PeopleNum ).ClothingValue = std::pow( 10.0, TemporaryVariable );
		} else if ( TemporarySixAMTemperature >= 26.0 ) {
			ThermalComfortData( PeopleNum ).ClothingValue = 0.46;
		}

	}

} // ThermalComfort

} // EnergyPlus
