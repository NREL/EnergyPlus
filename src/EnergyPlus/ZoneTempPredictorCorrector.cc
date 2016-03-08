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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ZoneTempPredictorCorrector.hh>
#include <DataAirflowNetwork.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <FaultsManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <RoomAirModelAirflowNetwork.hh>
#include <RoomAirModelManager.hh>
#include <ScheduleManager.hh>
#include <ThermalComfort.hh>
#include <UtilityRoutines.hh>
#include <ZonePlenum.hh>
#include <DirectAirManager.hh>

namespace EnergyPlus {

namespace ZoneTempPredictorCorrector {

	// MODULE INFORMATION:
	//       AUTHOR         Russell D. Taylor
	//       DATE WRITTEN   1997
	//       MODIFIED       Aug 2001(FW): make SNLoadHeatRate public
	//                      Nov 2010  BN(FSEC) added TemperatureAndHumidity Control
	//       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
	//                      July 2006 (BG) added operative temp control
	//                      February 2008 (BG) reworked zone air temp histories

	// PURPOSE OF THIS MODULE:
	// This module contains routines to predict and correct zone temperatures.
	//  also includes zone thermostatic controlling
	//  Model the "Air Heat Balance" part of the the "Zone Heat Balance Method."

	// METHODOLOGY EMPLOYED:
	// apply model equations for air heat balance solved for zone air temp.
	//    sum up values for the terms (e.g SUMHAT, SUMHA etc. )
	//    "Predict" step is used to get zone loads for HVAC equipment
	//    "correct" step determines zone air temp with available HVAC

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using namespace DataHeatBalance;
	using namespace DataHeatBalFanSys;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::OutBaroPress;
	using DataZoneEnergyDemands::ZoneSysEnergyDemand;
	using DataZoneEnergyDemands::ZoneSysMoistureDemand;
	using DataZoneEnergyDemands::DeadBandOrSetback;
	using DataZoneEnergyDemands::CurDeadBandOrSetback;
	using DataZoneEnergyDemands::Setback;
	using namespace Psychrometrics;
	using DataAirflowNetwork::SimulateAirflowNetwork;
	using DataAirflowNetwork::AirflowNetworkExchangeData;
	using DataAirflowNetwork::AirflowNetworkZoneExhaustFan;
	using DataAirflowNetwork::AirflowNetworkNumOfExhFan;
	using DataAirflowNetwork::AirflowNetworkFanActivated;
	using DataAirflowNetwork::AirflowNetworkControlMultizone;
	using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
	using DataAirflowNetwork::AirflowNetworkControlMultiADS;
	using namespace DataRoomAirModel;
	using namespace DataZoneControls;
	using namespace FaultsManager;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Controls for PredictorCorrector
	//INTEGER, PUBLIC, PARAMETER :: iGetZoneSetPoints             = 1
	//INTEGER, PUBLIC, PARAMETER :: iPredictStep                  = 2
	//INTEGER, PUBLIC, PARAMETER :: iCorrectStep                  = 3
	//INTEGER, PUBLIC, PARAMETER :: iRevertZoneTimestepHistories  = 4
	//INTEGER, PUBLIC, PARAMETER :: iPushZoneTimestepHistories    = 5
	//INTEGER, PUBLIC, PARAMETER :: iPushSystemTimestepHistories  = 6

	Array1D_string const ValidControlTypes( 4, { "ThermostatSetpoint:SingleHeating", "ThermostatSetpoint:SingleCooling", "ThermostatSetpoint:SingleHeatingOrCooling", "ThermostatSetpoint:DualSetpoint" } );

	Array1D_string const ValidComfortControlTypes( 12, { "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating", "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling", "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling", "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint", "ThermostatSetpoint:ThermalComfort:Pierce:SingleHeating", "ThermostatSetpoint:ThermalComfort:Pierce:SingleCooling", "ThermostatSetpoint:ThermalComfort:Pierce:SingleHeatingOrCooling", "ThermostatSetpoint:ThermalComfort:Pierce:DualSetpoint", "ThermostatSetpoint:ThermalComfort:KSU:SingleHeating", "ThermostatSetpoint:ThermalComfort:KSU:SingleCooling", "ThermostatSetpoint:ThermalComfort:KSU:SingleHeatingOrCooling", "ThermostatSetpoint:ThermalComfort:KSU:DualSetpoint" } );

	Array1D_string const cZControlTypes( 6, { "ZoneControl:Thermostat", "ZoneControl:Thermostat:ThermalComfort", "ZoneControl:Thermostat:OperativeTemperature", "ZoneControl:Humidistat", "ZoneControl:Thermostat:TemperatureAndHumidity", "ZoneControl:Thermostat:StagedDualSetpoint" } );

	int const iZC_TStat( 1 );
	int const iZC_TCTStat( 2 );
	int const iZC_OTTStat( 3 );
	int const iZC_HStat( 4 );
	int const iZC_TandHStat( 5 );
	int const iZC_StagedDual( 6 );
	Array1D_int const iZControlTypes( 6, { iZC_TStat, iZC_TCTStat, iZC_OTTStat, iZC_HStat, iZC_TandHStat, iZC_StagedDual } );

	int const SglHeatSetPoint( 1 );
	int const SglCoolSetPoint( 2 );
	int const SglHCSetPoint( 3 );
	int const DualSetPoint( 4 );
	int const SglHeatSetPointFanger( 1 );
	int const SglCoolSetPointFanger( 2 );
	int const SglHCSetPointFanger( 3 );
	int const DualSetPointFanger( 4 );
	int const SglHeatSetPointPierce( 5 );
	int const SglCoolSetPointPierce( 6 );
	int const SglHCSetPointPierce( 7 );
	int const DualSetPointPierce( 8 );
	int const SglHeatSetPointKSU( 9 );
	int const SglCoolSetPointKSU( 10 );
	int const SglHCSetPointKSU( 11 );
	int const DualSetPointKSU( 12 );

	// Average method parameter with multiple people objects in a zone
	int const AverageMethodNum_NO( 0 ); // No multiple people objects
	int const AverageMethodNum_SPE( 1 ); // Specific people object
	int const AverageMethodNum_OBJ( 2 ); // People object average
	int const AverageMethodNum_PEO( 3 ); // People number average

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	int NumSingleTempHeatingControls( 0 );
	int NumSingleTempCoolingControls( 0 );
	int NumSingleTempHeatCoolControls( 0 );
	int NumDualTempHeatCoolControls( 0 );

	// Number of Thermal comfort control types
	int NumSingleFangerHeatingControls( 0 );
	int NumSingleFangerCoolingControls( 0 );
	int NumSingleFangerHeatCoolControls( 0 );
	int NumDualFangerHeatCoolControls( 0 );

	// Number of zone with staged controlled objects
	int NumStageCtrZone( 0 );

	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool InitZoneAirSetPointsOneTimeFlag( true );
		bool SetupOscillationOutputFlag( true );
	}
	Array1D< Real64 > ZoneSetPointLast;
	Array1D< Real64 > TempIndZnLd;
	Array1D< Real64 > TempDepZnLd;
	Array1D< Real64 > ZoneAirRelHum; // Zone relative humidity in percent

	// Zone temperature history - used only for oscillation test
	Array2D< Real64 > ZoneTempHist;
	Array1D< Real64 > ZoneTempOscillate;
	Real64 AnyZoneTempOscillate;

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< ZoneTempControlType > SetPointSingleHeating;
	Array1D< ZoneTempControlType > SetPointSingleCooling;
	Array1D< ZoneTempControlType > SetPointSingleHeatCool;
	Array1D< ZoneTempControlType > SetPointDualHeatCool;
	Array1D< ZoneComfortFangerControlType > SetPointSingleHeatingFanger;
	Array1D< ZoneComfortFangerControlType > SetPointSingleCoolingFanger;
	Array1D< ZoneComfortFangerControlType > SetPointSingleHeatCoolFanger;
	Array1D< ZoneComfortFangerControlType > SetPointDualHeatCoolFanger;

	// Functions
	void
	clear_state()
	{

		NumSingleTempHeatingControls = 0;
		NumSingleTempCoolingControls = 0;
		NumSingleTempHeatCoolControls = 0;
		NumDualTempHeatCoolControls = 0;
		NumSingleFangerHeatingControls = 0;
		NumSingleFangerCoolingControls = 0;
		NumSingleFangerHeatCoolControls = 0;
		NumDualFangerHeatCoolControls = 0;
		NumStageCtrZone = 0;
		InitZoneAirSetPointsOneTimeFlag = true ;
		SetupOscillationOutputFlag =  true;
		ZoneSetPointLast.deallocate();
		TempIndZnLd.deallocate();
		TempDepZnLd.deallocate();
		ZoneAirRelHum.deallocate();
		ZoneTempHist.deallocate();
		ZoneTempOscillate.deallocate();
		AnyZoneTempOscillate= 0.0;
		SetPointSingleHeating.deallocate();
		SetPointSingleCooling.deallocate();
		SetPointSingleHeatCool.deallocate();
		SetPointDualHeatCool.deallocate();
		SetPointSingleHeatingFanger.deallocate();
		SetPointSingleCoolingFanger.deallocate();
		SetPointSingleHeatCoolFanger.deallocate();
		SetPointDualHeatCoolFanger.deallocate();
	}

	void
	ManageZoneAirUpdates(
		int const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
		Real64 & ZoneTempChange, // Temp change in zone air btw previous and current timestep
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	)
	{

		// SUBROUTINE INFORMATION
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   September 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  Brent Griffith Feb. 2008,  added arguments

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine predicts or corrects the zone air temperature
		// depending on the simulation status and determines the correct
		// temperature setpoint for each zone from the schedule manager.

		// METHODOLOGY EMPLOYED:
		// na

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
		//unused1208  INTEGER :: zoneloop

		if ( GetZoneAirStatsInputFlag ) {
			GetZoneAirSetPoints();
			GetZoneAirStatsInputFlag = false;
		}

		InitZoneAirSetPoints();

		{ auto const SELECT_CASE_var( UpdateType );

		if ( SELECT_CASE_var == iGetZoneSetPoints ) {
			CalcZoneAirTempSetPoints();

		} else if ( SELECT_CASE_var == iPredictStep ) {
			PredictSystemLoads( ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		} else if ( SELECT_CASE_var == iCorrectStep ) {
			CorrectZoneAirTemp( ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		} else if ( SELECT_CASE_var == iRevertZoneTimestepHistories ) {
			RevertZoneTimestepHistories();

		} else if ( SELECT_CASE_var == iPushZoneTimestepHistories ) {
			PushZoneTimestepHistories();

		} else if ( SELECT_CASE_var == iPushSystemTimestepHistories ) {
			PushSystemTimestepHistories();

		}}

	}

	void
	GetZoneAirSetPoints()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russell Taylor
		//       DATE WRITTEN   September 1998
		//       MODIFIED       L.Gu, May 2006, B. Griffith June 2006
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the inputs related to thermostatic control.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace InputProcessor;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleMinValue;
		using ScheduleManager::GetScheduleMaxValue;
		using ScheduleManager::CheckScheduleValue;
		using General::TrimSigDigits;
		using General::FindNumberInList;
		using General::RoundSigDigits;
		using General::CheckCreatedZoneItemName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneAirSetpoints: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TempControlledZoneNum; // The Splitter that you are currently loading input into
		int NumAlphas;
		int NumNums;
		int SingleTempHeatingControlNum;
		int SingleTempCoolingControlNum;
		int SingleTempHeatCoolControlNum;
		int DualTempHeatCoolControlNum;
		int ControlTypeNum;
		int IOStat;
		//unused1208  REAL(r64), DIMENSION(2) :: NumArray
		//unused1208  CHARACTER(len=MaxNameLength), DIMENSION(29) :: AlphArray
		static bool ErrorsFound( false );
		bool errFlag;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CTIndex;
		int HumidControlledZoneNum; // The Humidity Controller that information is being loaded into
		bool ValidScheduleControlType;
		bool ValidRadFractSched; // check for if radiative fraction schedule has valid numbers
		bool ValidZoneOvercoolRangeSched; // check for if Zone Overcool range schedule has valid numbers
		int TempIndex;
		int SchedMin;
		int SchedMax;
		int ActualZoneNum;
		int SchedTypeIndex;

		int ComfortControlledZoneNum; // The Splitter that you are currently loading input into
		int i;
		int IZoneCount;

		int OpTempContrlNum; // do loop index
		int found;

		int TempHumidityCntrlNum; // do loop index for overcooled controlled zone

		int SingleFangerHeatingControlNum;
		int SingleFangerCoolingControlNum;
		int SingleFangerHeatCoolControlNum;
		int DualFangerHeatCoolControlNum;
		int ComfortIndex;
		int ZoneAssigned;

		int NumStageControlledZones; // Number of staged controlled objects
		int StageControlledZoneNum; // Index for staged controlled zones

		Array1D_int CTSchedMapToControlledZone;
		Array1D_int CCmSchedMapToControlledZone;
		int Item;
		int Item1;
		int ZLItem;

		struct NeededControlTypes
		{
			// Members
			Array1D_bool MustHave; // 4= the four control types
			Array1D_bool DidHave;

			// Default Constructor
			NeededControlTypes() :
				MustHave( 4, false ),
				DidHave( 4, false )
			{}

		};

		struct NeededComfortControlTypes
		{
			// Members
			Array1D_bool MustHave; // 4= the four control types
			Array1D_bool DidHave;

			// Default Constructor
			NeededComfortControlTypes() :
				MustHave( 12, false ),
				DidHave( 12, false )
			{}

		};

		// Object Data
		Array1D< NeededControlTypes > TStatControlTypes;
		Array1D< NeededComfortControlTypes > TComfortControlTypes;

		// Formats
		static gio::Fmt Format_700( "('! <Zone Volume Capacitance Multiplier>, Sensible Heat Capacity Multiplier, Moisture Capacity Multiplier, ','Carbon Dioxide Capacity Multiplier, Generic Contaminant Capacity Multiplier')" );
		static gio::Fmt Format_701( "('Zone Volume Capacitance Multiplier,',F8.3,' ,',F8.3,',',F8.3,',',F8.3)" );

		// FLOW:
		cCurrentModuleObject = cZControlTypes( iZC_TStat );
		NumTStatStatements = GetNumObjectsFound( cCurrentModuleObject );
		TStatObjects.allocate( NumTStatStatements );

		// Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
		NumTempControlledZones = 0;
		for ( Item = 1; Item <= NumTStatStatements; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), TStatObjects, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			TStatObjects( Item ).Name = cAlphaArgs( 1 );
			Item1 = FindItemInList( cAlphaArgs( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( cAlphaArgs( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				TStatObjects( Item ).TempControlledZoneStartPtr = NumTempControlledZones + 1;
				++NumTempControlledZones;
				TStatObjects( Item ).NumOfZones = 1;
				TStatObjects( Item ).ZoneListActive = false;
				TStatObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				TStatObjects( Item ).TempControlledZoneStartPtr = NumTempControlledZones + 1;
				NumTempControlledZones += ZoneList( ZLItem ).NumOfZones;
				TStatObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				TStatObjects( Item ).ZoneListActive = true;
				TStatObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowSevereError( "GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			NumTempControlledZones = 0;
		}

		if ( NumTempControlledZones > 0 ) {
			TempControlledZone.allocate( NumTempControlledZones );
			TStatControlTypes.allocate( NumTempControlledZones ); // Number of set point types
			CTSchedMapToControlledZone.dimension( NumTempControlledZones, 0 );

			TempControlledZoneNum = 0;
			for ( Item = 1; Item <= NumTStatStatements; ++Item ) {
				GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				for ( Item1 = 1; Item1 <= TStatObjects( Item ).NumOfZones; ++Item1 ) {
					++TempControlledZoneNum;
					if ( TStatObjects( Item ).ZoneListActive ) {
						cAlphaArgs( 2 ) = Zone( ZoneList( TStatObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name;
					}
					ZoneAssigned = FindItemInList( cAlphaArgs( 2 ), TempControlledZone, &ZoneTempControls::ZoneName, TempControlledZoneNum - 1 );
					if ( ZoneAssigned == 0 ) {
						TempControlledZone( TempControlledZoneNum ).ZoneName = cAlphaArgs( 2 );
						TempControlledZone( TempControlledZoneNum ).ActualZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone );
						if ( TempControlledZone( TempControlledZoneNum ).ActualZoneNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
							ErrorsFound = true;
						} else {
							Zone( TempControlledZone( TempControlledZoneNum ).ActualZoneNum ).TempControlledZoneIndex = TempControlledZoneNum;
						}
					} else {
						TempControlledZone( TempControlledZoneNum ).ZoneName = cAlphaArgs( 2 ); // for continuity
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" zone previously assigned." );
						ShowContinueError( "...Zone was previously assigned to Thermostat=\"" + TempControlledZone( ZoneAssigned ).Name + "\"." );
						ErrorsFound = true;
						continue;
					}

					if ( ! TStatObjects( Item ).ZoneListActive ) {
						TempControlledZone( TempControlledZoneNum ).Name = cAlphaArgs( 1 );
					} else {
						CheckCreatedZoneItemName( RoutineName, cCurrentModuleObject, Zone( ZoneList( TStatObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( TStatObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, TStatObjects( Item ).Name, TempControlledZone, TempControlledZoneNum - 1, TempControlledZone( TempControlledZoneNum ).Name, errFlag );
						if ( errFlag ) ErrorsFound = true;
					}

					TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName = cAlphaArgs( 3 );
					TempControlledZone( TempControlledZoneNum ).CTSchedIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
					if ( Item1 == 1 ) { // only show error on first of several if zone list
						if ( TempControlledZone( TempControlledZoneNum ).CTSchedIndex == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						} else {
							// Check validity of control types.
							ValidScheduleControlType = CheckScheduleValueMinMax( TempControlledZone( TempControlledZoneNum ).CTSchedIndex, ">=", 0.0, "<=", 4.0 );
							if ( ! ValidScheduleControlType ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid range " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"" );
								ShowContinueError( "..contains values outside of range [0,4]." );
								ErrorsFound = true;
							}
						}
					}

					TempControlledZone( TempControlledZoneNum ).NumControlTypes = nint( ( NumAlphas - 3.0 ) / 2.0 );
					TempControlledZone( TempControlledZoneNum ).ControlType.allocate( TempControlledZone( TempControlledZoneNum ).NumControlTypes );
					TempControlledZone( TempControlledZoneNum ).ControlTypeName.allocate( TempControlledZone( TempControlledZoneNum ).NumControlTypes );
					TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx.allocate( TempControlledZone( TempControlledZoneNum ).NumControlTypes );

					for ( ControlTypeNum = 1; ControlTypeNum <= TempControlledZone( TempControlledZoneNum ).NumControlTypes; ++ControlTypeNum ) {

						TempControlledZone( TempControlledZoneNum ).ControlType( ControlTypeNum ) = cAlphaArgs( nint( 2.0 * ControlTypeNum - 1 + 3 ) );
						TempControlledZone( TempControlledZoneNum ).ControlTypeName( ControlTypeNum ) = cAlphaArgs( nint( 2.0 * ControlTypeNum + 3 ) );

						if ( TempControlledZone( TempControlledZoneNum ).ControlType( ControlTypeNum ) != "" ) {
							CTIndex = FindItem( TempControlledZone( TempControlledZoneNum ).ControlType( ControlTypeNum ), ValidControlTypes, 4 );
							if ( CTIndex == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( nint( 2.0 * ControlTypeNum - 1 + 3 ) ) + "=\"" + cAlphaArgs( nint( 2.0 * ControlTypeNum - 1 + 3 ) ) + "\"" );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( nint( 2.0 * ControlTypeNum - 1 + 3 ) ) + "=\"<blank>\"" );
							ErrorsFound = true;
						}
						TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( ControlTypeNum ) = 0;
					}
				}
			} // NumTStatStatements
		} // Check on number of TempControlledZones

		cCurrentModuleObject = ValidControlTypes( SglHeatSetPoint );
		NumSingleTempHeatingControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSingleTempHeatingControls > 0 ) SetPointSingleHeating.allocate( NumSingleTempHeatingControls );

		for ( SingleTempHeatingControlNum = 1; SingleTempHeatingControlNum <= NumSingleTempHeatingControls; ++SingleTempHeatingControlNum ) {
			GetObjectItem( cCurrentModuleObject, SingleTempHeatingControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointSingleHeating, SingleTempHeatingControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SetPointSingleHeating( SingleTempHeatingControlNum ).Name = cAlphaArgs( 1 );
			SetPointSingleHeating( SingleTempHeatingControlNum ).TempSchedName = cAlphaArgs( 2 );
			SetPointSingleHeating( SingleTempHeatingControlNum ).TempSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointSingleHeating( SingleTempHeatingControlNum ).TempSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}

		} // SingleTempHeatingControlNum

		cCurrentModuleObject = ValidControlTypes( SglCoolSetPoint );
		NumSingleTempCoolingControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSingleTempCoolingControls > 0 ) SetPointSingleCooling.allocate( NumSingleTempCoolingControls );

		for ( SingleTempCoolingControlNum = 1; SingleTempCoolingControlNum <= NumSingleTempCoolingControls; ++SingleTempCoolingControlNum ) {
			GetObjectItem( cCurrentModuleObject, SingleTempCoolingControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointSingleCooling, SingleTempCoolingControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SetPointSingleCooling( SingleTempCoolingControlNum ).Name = cAlphaArgs( 1 );
			SetPointSingleCooling( SingleTempCoolingControlNum ).TempSchedName = cAlphaArgs( 2 );
			SetPointSingleCooling( SingleTempCoolingControlNum ).TempSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointSingleCooling( SingleTempCoolingControlNum ).TempSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}

		} // SingleTempCoolingControlNum

		cCurrentModuleObject = ValidControlTypes( SglHCSetPoint );
		NumSingleTempHeatCoolControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSingleTempHeatCoolControls > 0 ) SetPointSingleHeatCool.allocate( NumSingleTempHeatCoolControls );

		for ( SingleTempHeatCoolControlNum = 1; SingleTempHeatCoolControlNum <= NumSingleTempHeatCoolControls; ++SingleTempHeatCoolControlNum ) {
			GetObjectItem( cCurrentModuleObject, SingleTempHeatCoolControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			SetPointSingleHeatCool( SingleTempHeatCoolControlNum ).Name = cAlphaArgs( 1 );
			SetPointSingleHeatCool( SingleTempHeatCoolControlNum ).TempSchedName = cAlphaArgs( 2 );
			SetPointSingleHeatCool( SingleTempHeatCoolControlNum ).TempSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointSingleHeatCool( SingleTempHeatCoolControlNum ).TempSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}

		} // SingleTempHeatCoolControlNum

		cCurrentModuleObject = ValidControlTypes( DualSetPoint );
		NumDualTempHeatCoolControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumDualTempHeatCoolControls > 0 ) SetPointDualHeatCool.allocate( NumDualTempHeatCoolControls );

		for ( DualTempHeatCoolControlNum = 1; DualTempHeatCoolControlNum <= NumDualTempHeatCoolControls; ++DualTempHeatCoolControlNum ) {
			GetObjectItem( cCurrentModuleObject, DualTempHeatCoolControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointDualHeatCool, DualTempHeatCoolControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SetPointDualHeatCool( DualTempHeatCoolControlNum ).Name = cAlphaArgs( 1 );
			SetPointDualHeatCool( DualTempHeatCoolControlNum ).HeatTempSetptSchedName = cAlphaArgs( 2 );
			SetPointDualHeatCool( DualTempHeatCoolControlNum ).HeatTempSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointDualHeatCool( DualTempHeatCoolControlNum ).HeatTempSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}
			SetPointDualHeatCool( DualTempHeatCoolControlNum ).CoolTempSetptSchedName = cAlphaArgs( 3 );
			SetPointDualHeatCool( DualTempHeatCoolControlNum ).CoolTempSchedIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
			if ( SetPointDualHeatCool( DualTempHeatCoolControlNum ).CoolTempSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
				ErrorsFound = true;
			}

		} // DualTempHeatCoolControlNum

		// Finish filling in Schedule pointing indexes
		for ( TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum ) {
			TempIndex = FindItem( ValidControlTypes( SglHeatSetPoint ), TempControlledZone( TempControlledZoneNum ).ControlType, TempControlledZone( TempControlledZoneNum ).NumControlTypes );
			TempControlledZone( TempControlledZoneNum ).SchIndx_SingleHeatSetPoint = TempIndex;
			if ( TempIndex > 0 ) {
				TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex ) = FindItem( TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ), SetPointSingleHeating );
				TStatControlTypes( TempControlledZoneNum ).MustHave( SingleHeatingSetPoint ) = true;
			}

			TempIndex = FindItem( ValidControlTypes( SglCoolSetPoint ), TempControlledZone( TempControlledZoneNum ).ControlType, TempControlledZone( TempControlledZoneNum ).NumControlTypes );
			TempControlledZone( TempControlledZoneNum ).SchIndx_SingleCoolSetPoint = TempIndex;
			if ( TempIndex > 0 ) {
				TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex ) = FindItem( TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ), SetPointSingleCooling );
				TStatControlTypes( TempControlledZoneNum ).MustHave( SingleCoolingSetPoint ) = true;
			}

			TempIndex = FindItem( ValidControlTypes( SglHCSetPoint ), TempControlledZone( TempControlledZoneNum ).ControlType, TempControlledZone( TempControlledZoneNum ).NumControlTypes );
			TempControlledZone( TempControlledZoneNum ).SchIndx_SingleHeatCoolSetPoint = TempIndex;
			if ( TempIndex > 0 ) {
				TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex ) = FindItem( TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ), SetPointSingleHeatCool );
				TStatControlTypes( TempControlledZoneNum ).MustHave( SingleHeatCoolSetPoint ) = true;
			}

			TempIndex = FindItem( ValidControlTypes( DualSetPoint ), TempControlledZone( TempControlledZoneNum ).ControlType, TempControlledZone( TempControlledZoneNum ).NumControlTypes );
			TempControlledZone( TempControlledZoneNum ).SchIndx_DualSetPointWDeadBand = TempIndex;
			if ( TempIndex > 0 ) {
				TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex ) = FindItem( TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ), SetPointDualHeatCool );
				TStatControlTypes( TempControlledZoneNum ).MustHave( DualSetPointWithDeadBand ) = true;
			}
		}

		// Now, Check the schedule values/indices for validity

		for ( TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum ) {

			ActualZoneNum = TempControlledZone( TempControlledZoneNum ).ActualZoneNum;
			CTIndex = TempControlledZone( TempControlledZoneNum ).CTSchedIndex;
			if ( CTIndex == 0 ) continue; // error will be caught elsewhere
			SchedMin = GetScheduleMinValue( CTIndex );
			SchedMax = GetScheduleMaxValue( CTIndex );

			if ( SchedMin == 0 && SchedMax == 0 ) {
				if ( FindNumberInList( CTIndex, CTSchedMapToControlledZone, NumTempControlledZones ) == 0 ) {
					ShowSevereError( "Control Type Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "..specifies control type 0 for all entries." );
					ShowContinueError( "All zones using this Control Type Schedule have no heating or cooling available." );
				}
				CTSchedMapToControlledZone( TempControlledZoneNum ) = CTIndex;
			}

			for ( ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum ) {

				{ auto const SELECT_CASE_var( ControlTypeNum );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {

					TempIndex = TempControlledZone( TempControlledZoneNum ).SchIndx_SingleHeatSetPoint;
					TStatControlTypes( TempControlledZoneNum ).DidHave( SingleHeatingSetPoint ) = true;
					if ( TempIndex != 0 ) {
						SchedTypeIndex = TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidControlTypes( SglHeatSetPoint ) + " Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ) );
							ErrorsFound = true;
						}
					} else { // TempIndex = 0
						if ( CheckScheduleValue( CTIndex, SingleHeatingSetPoint ) ) {
							ShowSevereError( "Control Type Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies control type 1 (" + ValidControlTypes( SglHeatSetPoint ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {

					TempIndex = TempControlledZone( TempControlledZoneNum ).SchIndx_SingleCoolSetPoint;
					TStatControlTypes( TempControlledZoneNum ).DidHave( SingleCoolingSetPoint ) = true;
					if ( TempIndex != 0 ) {
						SchedTypeIndex = TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidControlTypes( SglCoolSetPoint ) + " Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ) );
							ErrorsFound = true;
						}
					} else { // TempIndex = 0
						if ( CheckScheduleValue( CTIndex, SingleCoolingSetPoint ) ) {
							ShowSevereError( "Control Type Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies control type 2 (" + ValidControlTypes( SglCoolSetPoint ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {

					TempIndex = TempControlledZone( TempControlledZoneNum ).SchIndx_SingleHeatCoolSetPoint;
					TStatControlTypes( TempControlledZoneNum ).DidHave( SingleHeatCoolSetPoint ) = true;
					if ( TempIndex != 0 ) {
						SchedTypeIndex = TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidControlTypes( SglHCSetPoint ) + " Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ) );
							ErrorsFound = true;
						}
					} else { // TempIndex = 0
						if ( CheckScheduleValue( CTIndex, SingleHeatCoolSetPoint ) ) {
							ShowSevereError( "Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies control type 3 (" + ValidControlTypes( SglHCSetPoint ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {

					TempIndex = TempControlledZone( TempControlledZoneNum ).SchIndx_DualSetPointWDeadBand;
					TStatControlTypes( TempControlledZoneNum ).DidHave( DualSetPointWithDeadBand ) = true;
					if ( TempIndex != 0 ) {
						SchedTypeIndex = TempControlledZone( TempControlledZoneNum ).ControlTypeSchIndx( TempIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidControlTypes( DualSetPoint ) + " Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeName( TempIndex ) );
							ErrorsFound = true;
						}
					} else { // TempIndex = 0
						if ( CheckScheduleValue( CTIndex, DualSetPointWithDeadBand ) ) {
							ShowSevereError( "Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies control type 4 (" + ValidControlTypes( DualSetPoint ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else {
					ShowSevereError( "GetZoneAirSetpoints: Illegal control type for Zone=" + Zone( ActualZoneNum ).Name + ", Found value=" + TrimSigDigits( ControlTypeNum ) + ", in Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "..valid range values are [0,4]." );
					ErrorsFound = true;

				}}
			}

		}

		for ( TempControlledZoneNum = 1; TempControlledZoneNum <= NumTempControlledZones; ++TempControlledZoneNum ) {

			ActualZoneNum = TempControlledZone( TempControlledZoneNum ).ActualZoneNum;
			CTIndex = TempControlledZone( TempControlledZoneNum ).CTSchedIndex;
			if ( CTIndex == 0 ) continue; // error caught elsewhere -- would just be confusing here

			for ( ControlTypeNum = 1; ControlTypeNum <= 4; ++ControlTypeNum ) {
				if ( TStatControlTypes( TempControlledZoneNum ).MustHave( ControlTypeNum ) && TStatControlTypes( TempControlledZoneNum ).DidHave( ControlTypeNum ) ) continue;

				{ auto const SELECT_CASE_var( ControlTypeNum );

				if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
					if ( ! TStatControlTypes( TempControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 1 (" + ValidControlTypes( SglHeatSetPoint ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );

				} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
					if ( ! TStatControlTypes( TempControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 2 (" + ValidControlTypes( SglCoolSetPoint ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );

				} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
					if ( ! TStatControlTypes( TempControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 3 (" + ValidControlTypes( SglHCSetPoint ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );

				} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
					if ( ! TStatControlTypes( TempControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + TempControlledZone( TempControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 4 (" + ValidControlTypes( DualSetPoint ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TStat ) + '=' + TempControlledZone( TempControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + TempControlledZone( TempControlledZoneNum ).ZoneName );

				} else {
				}}
			}
		}

		if ( allocated( TStatControlTypes ) ) TStatControlTypes.deallocate();
		// This starts the Humidity Control Get Input section
		cCurrentModuleObject = cZControlTypes( iZC_HStat );
		NumHumidityControlZones = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumHumidityControlZones > 0 ) HumidityControlZone.allocate( NumHumidityControlZones );

		for ( HumidControlledZoneNum = 1; HumidControlledZoneNum <= NumHumidityControlZones; ++HumidControlledZoneNum ) {
			GetObjectItem( cCurrentModuleObject, HumidControlledZoneNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), HumidityControlZone, &ZoneHumidityControls::ControlName, HumidControlledZoneNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			HumidityControlZone( HumidControlledZoneNum ).ControlName = cAlphaArgs( 1 );
			// Ensure unique zone name
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 2 ), HumidityControlZone, &ZoneHumidityControls::ZoneName, HumidControlledZoneNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Zone Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 2 ) = "xxxxx";
			}

			HumidityControlZone( HumidControlledZoneNum ).ZoneName = cAlphaArgs( 2 );
			HumidityControlZone( HumidControlledZoneNum ).ActualZoneNum = FindItem( cAlphaArgs( 2 ), Zone );
			if ( HumidityControlZone( HumidControlledZoneNum ).ActualZoneNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}
			HumidityControlZone( HumidControlledZoneNum ).HumidifyingSched = cAlphaArgs( 3 );
			HumidityControlZone( HumidControlledZoneNum ).HumidifyingSchedIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
			if ( HumidityControlZone( HumidControlledZoneNum ).HumidifyingSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
				ErrorsFound = true;
			}
			if ( NumAlphas == 4 ) {
				HumidityControlZone( HumidControlledZoneNum ).DehumidifyingSched = cAlphaArgs( 4 );
				HumidityControlZone( HumidControlledZoneNum ).DehumidifyingSchedIndex = GetScheduleIndex( cAlphaArgs( 4 ) );
				if ( HumidityControlZone( HumidControlledZoneNum ).DehumidifyingSchedIndex == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" not found." );
					ErrorsFound = true;
				}
			} else {
				HumidityControlZone( HumidControlledZoneNum ).DehumidifyingSched = cAlphaArgs( 3 );
				HumidityControlZone( HumidControlledZoneNum ).DehumidifyingSchedIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
			}

		} // HumidControlledZoneNum

		// Start to read Thermal comfort control objects
		cCurrentModuleObject = cZControlTypes( iZC_TCTStat );
		NumComfortTStatStatements = GetNumObjectsFound( cCurrentModuleObject );
		ComfortTStatObjects.allocate( NumComfortTStatStatements );

		// Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
		NumComfortControlledZones = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumComfortTStatStatements; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// will not do much verifying -- that will come later.
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ComfortTStatObjects, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				errFlag = true;
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Item1 = FindItemInList( cAlphaArgs( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( cAlphaArgs( 2 ), ZoneList );
			ComfortTStatObjects( Item ).Name = cAlphaArgs( 1 );
			if ( Item1 > 0 ) {
				ComfortTStatObjects( Item ).ComfortControlledZoneStartPtr = NumComfortControlledZones + 1;
				++NumComfortControlledZones;
				ComfortTStatObjects( Item ).NumOfZones = 1;
				ComfortTStatObjects( Item ).ZoneListActive = false;
				ComfortTStatObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				ComfortTStatObjects( Item ).ComfortControlledZoneStartPtr = NumComfortControlledZones + 1;
				NumComfortControlledZones += ZoneList( ZLItem ).NumOfZones;
				ComfortTStatObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				ComfortTStatObjects( Item ).ZoneListActive = true;
				ComfortTStatObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				errFlag = true;
				ErrorsFound = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( "GetZoneAirSetpoints: Errors with invalid names in " + cCurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			NumComfortControlledZones = 0;
		}

		if ( NumComfortControlledZones > 0 ) {
			ComfortControlledZone.allocate( NumComfortControlledZones );
			TComfortControlTypes.allocate( NumComfortControlledZones ); // Number of set point types
			CCmSchedMapToControlledZone.dimension( NumComfortControlledZones, 0 );

			ComfortControlledZoneNum = 0;
			for ( Item = 1; Item <= NumComfortTStatStatements; ++Item ) {
				GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				for ( Item1 = 1; Item1 <= ComfortTStatObjects( Item ).NumOfZones; ++Item1 ) {
					++ComfortControlledZoneNum;
					if ( ComfortTStatObjects( Item ).ZoneListActive ) {
						cAlphaArgs( 2 ) = Zone( ZoneList( ComfortTStatObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name;
					}
					ZoneAssigned = FindItemInList( cAlphaArgs( 2 ), ComfortControlledZone, &ZoneComfortControls::ZoneName, ComfortControlledZoneNum - 1 );
					if ( ZoneAssigned == 0 ) {
						ComfortControlledZone( ComfortControlledZoneNum ).ZoneName = cAlphaArgs( 2 );
						ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone );
						if ( ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
							ErrorsFound = true;
						}
					} else {
						ComfortControlledZone( ComfortControlledZoneNum ).ZoneName = cAlphaArgs( 2 ); // for continuity
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" zone previously assigned." );
						ShowContinueError( "...Zone was previously assigned to Thermostat=\"" + ComfortControlledZone( ZoneAssigned ).Name + "\"." );
						ErrorsFound = true;
						continue;
					}

					if ( ! ComfortTStatObjects( Item ).ZoneListActive ) {
						ComfortControlledZone( ComfortControlledZoneNum ).Name = cAlphaArgs( 1 );
					} else {
						ComfortControlledZone( ComfortControlledZoneNum ).Name = Zone( ZoneList( ComfortTStatObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name + ' ' + ComfortTStatObjects( Item ).Name;
					}

					// Read Fields A3 and A4 for averaging method
					IZoneCount = 0;
					for ( i = 1; i <= TotPeople; ++i ) {
						if ( ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum == People( i ).ZonePtr ) {
							++IZoneCount;
						}
					}
					// Could not find a people object for this particular zone
					if ( IZoneCount == 0 && ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum > 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " no PEOPLE in " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" - cannot use Comfort Control." );
						ErrorsFound = true;
					}
					ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodNum = AverageMethodNum_NO;
					if ( IZoneCount > 1 ) {
						ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodName = cAlphaArgs( 3 );
						if ( SameString( cAlphaArgs( 3 ), "SpecificObject" ) ) {
							ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodNum = AverageMethodNum_SPE;
						}
						if ( SameString( cAlphaArgs( 3 ), "ObjectAverage" ) ) {
							ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodNum = AverageMethodNum_OBJ;
						}
						if ( SameString( cAlphaArgs( 3 ), "PeopleAverage" ) ) {
							ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodNum = AverageMethodNum_PEO;
						}
						if ( ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
							ShowContinueError( "Allowed keys are SpecificObject, ObjectAverage, or PeopleAverage" );
							ErrorsFound = true;
						}
						if ( ComfortControlledZone( ComfortControlledZoneNum ).AverageMethodNum == AverageMethodNum_SPE ) {
							ComfortControlledZone( ComfortControlledZoneNum ).AverageObjectName = cAlphaArgs( 4 );
							if ( FindItem( cAlphaArgs( 4 ), People ) == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
								ErrorsFound = true;
							} else {
								ComfortControlledZone( ComfortControlledZoneNum ).SpecificObjectNum = FindItem( cAlphaArgs( 4 ), People );
							}
						}
					} else {
						for ( i = 1; i <= TotPeople; ++i ) {
							if ( ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum == People( i ).ZonePtr ) break;
						}
						ComfortControlledZone( ComfortControlledZoneNum ).SpecificObjectNum = i;
					}
					// Check values used for thermal comfort calculation
					for ( i = 1; i <= TotPeople; ++i ) {
						if ( ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum == People( i ).ZonePtr ) {
							// Check activity level
							if ( People( i ).ActivityLevelPtr > 0 ) {
								ValidScheduleControlType = CheckScheduleValueMinMax( People( i ).ActivityLevelPtr, ">=", 72.0, "<=", 909.0 );
								if ( ! ValidScheduleControlType ) {
									ShowSevereError( "GetPeople Activity Level: Invalid activity level values entered for thermal comfort calculation" );
									ShowContinueError( "Outside of range values [72,909], Reference object=" + People( i ).Name );
									ErrorsFound = true;
								}
							} else {
								ShowSevereError( "GetPeople Activity Level: Activity level schedule is not found=" + People( i ).Name );
								ShowContinueError( "Required when the zone has Thermal Comfort Controls." );
								ErrorsFound = true;
							}
							// Check Work Efficiency
							if ( People( i ).WorkEffPtr > 0 ) {
								ValidScheduleControlType = CheckScheduleValueMinMax( People( i ).WorkEffPtr, ">=", 0.0, "<=", 1.0 );
								if ( ! ValidScheduleControlType ) {
									ShowSevereError( "GetPeople work efficiency: Invalid work efficiency values entered for thermal comfort calculation" );
									ShowContinueError( "Outside of range values [0,1], Reference object=" + People( i ).Name );
									ErrorsFound = true;
								}
							} else {
								ShowSevereError( "GetPeople work efficiency: Work efficiency schedule is not found=" + People( i ).Name );
								ShowContinueError( "Required when the zone has Thermal Comfort Controls." );
								ErrorsFound = true;
							}
							// Check Clothing Insulation
							if ( People( i ).ClothingPtr > 0 ) {
								ValidScheduleControlType = CheckScheduleValueMinMax( People( i ).ClothingPtr, ">", 0.0, "<=", 2.0 );
								if ( ! ValidScheduleControlType ) {
									ShowSevereError( "GetPeople Clothing Insulation: Invalid Clothing Insulation values entered for thermal comfort calculation" );
									ShowContinueError( "Outside of range values [0.0,2.0], Reference object=" + People( i ).Name );
									ErrorsFound = true;
								}
							} else {
								ShowSevereError( "GetPeople Clothing Insulation: Clothing Insulation schedule is not found=" + People( i ).Name );
								ShowContinueError( "Required when the zone has Thermal Comfort Controls." );
								ErrorsFound = true;
							}
							// Check Air velocity
							if ( People( i ).AirVelocityPtr <= 0 ) {
								ShowSevereError( "GetPeople Air Velocity: Air velocity schedule is not found=" + People( i ).Name );
								ShowContinueError( "Required when the zone has Thermal Comfort Controls." );
								ErrorsFound = true;
							}
						}
					}

					// Read Max and Min temperature setpoint
					if ( NumNums > 0 ) {
						ComfortControlledZone( ComfortControlledZoneNum ).TdbMinSetPoint = rNumericArgs( 1 );
						if ( rNumericArgs( 1 ) > 50 || rNumericArgs( 1 ) < 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 0 ) + "]." );
							ShowContinueError( "..Allowable values must be between 0 C and 50 C" );
							ErrorsFound = true;
						}
					}
					if ( NumNums > 1 ) {
						ComfortControlledZone( ComfortControlledZoneNum ).TdbMaxSetPoint = rNumericArgs( 2 );
						if ( rNumericArgs( 2 ) > 50 || rNumericArgs( 2 ) < 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 2 ) + "=[" + TrimSigDigits( rNumericArgs( 2 ), 0 ) + "]." );
							ShowContinueError( "..Allowable values must be between 0 C and 50 C" );
							ErrorsFound = true;
						}
					}
					// Ensure MaxTemp >= MinTemp
					if ( ComfortControlledZone( ComfortControlledZoneNum ).TdbMinSetPoint > ComfortControlledZone( ComfortControlledZoneNum ).TdbMaxSetPoint ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) );
						ShowContinueError( ".." + cNumericFieldNames( 1 ) + " > " + cNumericFieldNames( 2 ) );
						ShowContinueError( "..[" + TrimSigDigits( rNumericArgs( 1 ), 0 ) + "] > [" + TrimSigDigits( rNumericArgs( 2 ), 0 ) + "]." );
						ErrorsFound = true;
					}
					// If MaxTemp = MinTemp, no thermal comfort control
					if ( ComfortControlledZone( ComfortControlledZoneNum ).TdbMinSetPoint == ComfortControlledZone( ComfortControlledZoneNum ).TdbMaxSetPoint ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) );
						ShowContinueError( ".." + cNumericFieldNames( 1 ) + " = " + cNumericFieldNames( 2 ) );
						ShowContinueError( "The zone will be controlled using this dry-bulb temperature setpoint." );
					}
					// read Thermal comfort type schedule name
					ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName = cAlphaArgs( 5 );
					ComfortControlledZone( ComfortControlledZoneNum ).ComfortSchedIndex = GetScheduleIndex( cAlphaArgs( 5 ) );
					if ( ComfortControlledZone( ComfortControlledZoneNum ).ComfortSchedIndex == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\" not found." );
						ErrorsFound = true;
					} else {
						// Check validity of control types.
						ValidScheduleControlType = CheckScheduleValueMinMax( ComfortControlledZone( ComfortControlledZoneNum ).ComfortSchedIndex, ">=", 0.0, "<=", 4.0 );
						if ( ! ValidScheduleControlType ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid range " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"" );
							ShowContinueError( "..contains values outside of range [0,4]." );
							ErrorsFound = true;
						}
					}
					ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes = nint( ( NumAlphas - 5.0 ) / 2.0 );
					ComfortControlledZone( ComfortControlledZoneNum ).ControlType.allocate( ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );
					ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName.allocate( ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );
					ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx.allocate( ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );

					for ( ControlTypeNum = 1; ControlTypeNum <= ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes; ++ControlTypeNum ) {
						ComfortControlledZone( ComfortControlledZoneNum ).ControlType( ControlTypeNum ) = cAlphaArgs( nint( 2.0 * ControlTypeNum - 1 + 5 ) );
						ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ControlTypeNum ) = cAlphaArgs( nint( 2.0 * ControlTypeNum + 5 ) );
						if ( ComfortControlledZone( ComfortControlledZoneNum ).ControlType( ControlTypeNum ) != "" ) {
							CTIndex = FindItem( ComfortControlledZone( ComfortControlledZoneNum ).ControlType( ControlTypeNum ), ValidComfortControlTypes, 12 );
							if ( CTIndex == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( nint( 2.0 * ControlTypeNum - 1 + 5 ) ) + "=\"" + cAlphaArgs( nint( 2.0 * ControlTypeNum - 1 + 5 ) ) + "\"" );
								ErrorsFound = true;
							}
							if ( CTIndex > 4 ) { // For Fanger control only for the time being
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( nint( 2.0 * ControlTypeNum - 1 + 5 ) ) + "=\"" + cAlphaArgs( nint( 2.0 * ControlTypeNum - 1 + 5 ) ) + "\"" );
								ShowContinueError( "..Fanger is the only valid model." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( nint( 2.0 * ControlTypeNum - 1 + 5 ) ) + "=\"<blank>\"" );
							ErrorsFound = true;
						}
						ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ControlTypeNum ) = 0;
					}
				}
			} // NumComfortTStatStatements
		}
		// End of Thermal comfort control reading and checking

		cCurrentModuleObject = ValidComfortControlTypes( SglHeatSetPointFanger );
		NumSingleFangerHeatingControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSingleFangerHeatingControls > 0 ) SetPointSingleHeatingFanger.allocate( NumSingleFangerHeatingControls );

		for ( SingleFangerHeatingControlNum = 1; SingleFangerHeatingControlNum <= NumSingleFangerHeatingControls; ++SingleFangerHeatingControlNum ) {
			GetObjectItem( cCurrentModuleObject, SingleFangerHeatingControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointSingleHeatingFanger, SingleFangerHeatingControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SetPointSingleHeatingFanger( SingleFangerHeatingControlNum ).Name = cAlphaArgs( 1 );
			SetPointSingleHeatingFanger( SingleFangerHeatingControlNum ).PMVSchedName = cAlphaArgs( 2 );
			SetPointSingleHeatingFanger( SingleFangerHeatingControlNum ).PMVSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointSingleHeatingFanger( SingleFangerHeatingControlNum ).PMVSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			} else {
				ValidScheduleControlType = CheckScheduleValueMinMax( SetPointSingleHeatingFanger( SingleFangerHeatingControlNum ).PMVSchedIndex, ">=", -3.0, "<=", 3.0 );
				if ( ! ValidScheduleControlType ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid PMV values " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" entered." );
					ShowContinueError( "..Values outside of range [-3,+3]." );
					ErrorsFound = true;
				}
			}
		} // SingleFangerHeatingControlNum

		cCurrentModuleObject = ValidComfortControlTypes( SglCoolSetPointFanger );
		NumSingleFangerCoolingControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSingleFangerCoolingControls > 0 ) SetPointSingleCoolingFanger.allocate( NumSingleFangerCoolingControls );

		for ( SingleFangerCoolingControlNum = 1; SingleFangerCoolingControlNum <= NumSingleFangerCoolingControls; ++SingleFangerCoolingControlNum ) {
			GetObjectItem( cCurrentModuleObject, SingleFangerCoolingControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointSingleCoolingFanger, SingleFangerCoolingControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SetPointSingleCoolingFanger( SingleFangerCoolingControlNum ).Name = cAlphaArgs( 1 );
			SetPointSingleCoolingFanger( SingleFangerCoolingControlNum ).PMVSchedName = cAlphaArgs( 2 );
			SetPointSingleCoolingFanger( SingleFangerCoolingControlNum ).PMVSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointSingleCoolingFanger( SingleFangerCoolingControlNum ).PMVSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			} else {
				ValidScheduleControlType = CheckScheduleValueMinMax( SetPointSingleCoolingFanger( SingleFangerCoolingControlNum ).PMVSchedIndex, ">=", -3.0, "<=", 3.0 );
				if ( ! ValidScheduleControlType ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid PMV values " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" entered." );
					ShowContinueError( "..Values outside of range [-3,+3]." );
					ErrorsFound = true;
				}
			}

		} // SingleFangerCoolingControlNum

		cCurrentModuleObject = ValidComfortControlTypes( SglHCSetPointFanger );
		NumSingleFangerHeatCoolControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSingleFangerHeatCoolControls > 0 ) SetPointSingleHeatCoolFanger.allocate( NumSingleFangerHeatCoolControls );

		for ( SingleFangerHeatCoolControlNum = 1; SingleFangerHeatCoolControlNum <= NumSingleFangerHeatCoolControls; ++SingleFangerHeatCoolControlNum ) {
			GetObjectItem( cCurrentModuleObject, SingleFangerHeatCoolControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointSingleCoolingFanger, SingleFangerCoolingControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}

			SetPointSingleHeatCoolFanger( SingleFangerHeatCoolControlNum ).Name = cAlphaArgs( 1 );
			SetPointSingleHeatCoolFanger( SingleFangerHeatCoolControlNum ).PMVSchedName = cAlphaArgs( 2 );
			SetPointSingleHeatCoolFanger( SingleFangerHeatCoolControlNum ).PMVSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointSingleHeatCoolFanger( SingleFangerHeatCoolControlNum ).PMVSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			} else {
				ValidScheduleControlType = CheckScheduleValueMinMax( SetPointSingleHeatCoolFanger( SingleFangerHeatCoolControlNum ).PMVSchedIndex, ">=", -3.0, "<=", 3.0 );
				if ( ! ValidScheduleControlType ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid PMV values " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" entered." );
					ShowContinueError( "..Values outside of range [-3,+3]." );
					ErrorsFound = true;
				}
			}

		} // SingleFangerHeatCoolControlNum

		cCurrentModuleObject = ValidComfortControlTypes( DualSetPointFanger );
		NumDualFangerHeatCoolControls = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumDualFangerHeatCoolControls > 0 ) SetPointDualHeatCoolFanger.allocate( NumDualFangerHeatCoolControls );

		for ( DualFangerHeatCoolControlNum = 1; DualFangerHeatCoolControlNum <= NumDualFangerHeatCoolControls; ++DualFangerHeatCoolControlNum ) {
			GetObjectItem( cCurrentModuleObject, DualFangerHeatCoolControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SetPointDualHeatCoolFanger, DualFangerHeatCoolControlNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).Name = cAlphaArgs( 1 );
			SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).HeatPMVSetptSchedName = cAlphaArgs( 2 );
			SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).HeatPMVSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).HeatPMVSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}
			SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).CoolPMVSetptSchedName = cAlphaArgs( 3 );
			SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).CoolPMVSchedIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
			if ( SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).CoolPMVSchedIndex == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
				ErrorsFound = true;
			} else {
				ValidScheduleControlType = CheckScheduleValueMinMax( SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).HeatPMVSchedIndex, ">=", -3.0, "<=", 3.0 );
				if ( ! ValidScheduleControlType ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid PMV values " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" entered." );
					ShowContinueError( "..Values outside of range [-3,+3]." );
					ErrorsFound = true;
				}
				ValidScheduleControlType = CheckScheduleValueMinMax( SetPointDualHeatCoolFanger( DualFangerHeatCoolControlNum ).CoolPMVSchedIndex, ">=", -3.0, "<=", 3.0 );
				if ( ! ValidScheduleControlType ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid PMV values " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" entered." );
					ShowContinueError( "..Values outside of range [-3,+3]." );
					ErrorsFound = true;
				}
			}

		} // DualFangerHeatCoolControlNum

		// Finish filling in Schedule pointing indexes for Thermal Comfort Control
		for ( ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= NumComfortControlledZones; ++ComfortControlledZoneNum ) {
			ComfortIndex = FindItem( ValidComfortControlTypes( SglHeatSetPointFanger ), ComfortControlledZone( ComfortControlledZoneNum ).ControlType, ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );
			ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_SglHeatSetPointFanger = ComfortIndex;
			if ( ComfortIndex > 0 ) {
				ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex ) = FindItem( ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ), SetPointSingleHeatingFanger );
				TComfortControlTypes( ComfortControlledZoneNum ).MustHave( SglHeatSetPointFanger ) = true;
			}

			ComfortIndex = FindItem( ValidComfortControlTypes( SglCoolSetPointFanger ), ComfortControlledZone( ComfortControlledZoneNum ).ControlType, ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );
			ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_SglCoolSetPointFanger = ComfortIndex;
			if ( ComfortIndex > 0 ) {
				ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex ) = FindItem( ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ), SetPointSingleCoolingFanger );
				TComfortControlTypes( ComfortControlledZoneNum ).MustHave( SglCoolSetPointFanger ) = true;
			}

			ComfortIndex = FindItem( ValidComfortControlTypes( SglHCSetPointFanger ), ComfortControlledZone( ComfortControlledZoneNum ).ControlType, ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );
			ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_SglHCSetPointFanger = ComfortIndex;
			if ( ComfortIndex > 0 ) {
				ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex ) = FindItem( ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ), SetPointSingleHeatCoolFanger );
				TComfortControlTypes( ComfortControlledZoneNum ).MustHave( SglHCSetPointFanger ) = true;
			}

			ComfortIndex = FindItem( ValidComfortControlTypes( DualSetPointFanger ), ComfortControlledZone( ComfortControlledZoneNum ).ControlType, ComfortControlledZone( ComfortControlledZoneNum ).NumControlTypes );
			ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_DualSetPointFanger = ComfortIndex;
			if ( ComfortIndex > 0 ) {
				ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex ) = FindItem( ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ), SetPointDualHeatCoolFanger );
				TComfortControlTypes( ComfortControlledZoneNum ).MustHave( DualSetPointFanger ) = true;
			}
		}

		// Now, Check the schedule values/indices for validity for Thermal Comfort Control

		for ( ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= NumComfortControlledZones; ++ComfortControlledZoneNum ) {

			ActualZoneNum = ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum;
			CTIndex = ComfortControlledZone( ComfortControlledZoneNum ).ComfortSchedIndex;
			if ( CTIndex == 0 ) continue; // error will be caught elsewhere
			SchedMin = GetScheduleMinValue( CTIndex );
			SchedMax = GetScheduleMaxValue( CTIndex );

			if ( SchedMin == 0 && SchedMax == 0 ) {
				if ( FindNumberInList( CTIndex, CCmSchedMapToControlledZone, NumComfortControlledZones ) == 0 ) {
					ShowWarningError( "Control Type Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "..specifies control type 0 for all entries." );
					ShowContinueError( "All zones using this Control Type Schedule have no thermal comfort control." );
				}
				CCmSchedMapToControlledZone( ComfortControlledZoneNum ) = CTIndex;
			}

			for ( ControlTypeNum = SchedMin; ControlTypeNum <= SchedMax; ++ControlTypeNum ) {

				{ auto const SELECT_CASE_var( ControlTypeNum );

				if ( SELECT_CASE_var == 0 ) { // Thermal comfort uncontrolled

				} else if ( SELECT_CASE_var == SglHeatSetPointFanger ) {

					ComfortIndex = ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_SglHeatSetPointFanger;
					TComfortControlTypes( ComfortControlledZoneNum ).DidHave( SglHeatSetPointFanger ) = true;
					if ( ComfortIndex != 0 ) {
						SchedTypeIndex = ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes( SglHeatSetPointFanger ) + " Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ) );
							ErrorsFound = true;
						}
					} else { // ComfortIndex = 0
						if ( CheckScheduleValue( CTIndex, SglHeatSetPointFanger ) ) {
							ShowSevereError( "Control Type Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies thermal control type 1 (" + ValidComfortControlTypes( SglHeatSetPointFanger ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {

					ComfortIndex = ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_SglCoolSetPointFanger;
					TComfortControlTypes( ComfortControlledZoneNum ).DidHave( SglCoolSetPointFanger ) = true;
					if ( ComfortIndex != 0 ) {
						SchedTypeIndex = ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes( SglCoolSetPointFanger ) + " Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ) );
							ErrorsFound = true;
						}
					} else { // ComfortIndex = 0
						if ( CheckScheduleValue( CTIndex, SglCoolSetPointFanger ) ) {
							ShowSevereError( "Control Type Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies thermal control type 2 (" + ValidComfortControlTypes( SglCoolSetPointFanger ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {

					ComfortIndex = ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_SglHCSetPointFanger;
					TComfortControlTypes( ComfortControlledZoneNum ).DidHave( SglHCSetPointFanger ) = true;
					if ( ComfortIndex != 0 ) {
						SchedTypeIndex = ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes( SglHCSetPointFanger ) + " Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ) );
							ErrorsFound = true;
						}
					} else { // ComfortIndex = 0
						if ( CheckScheduleValue( CTIndex, SglHCSetPointFanger ) ) {
							ShowSevereError( "Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies thermal control type 3 (" + ValidComfortControlTypes( SglHCSetPointFanger ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == DualSetPointFanger ) {

					ComfortIndex = ComfortControlledZone( ComfortControlledZoneNum ).SchIndx_DualSetPointFanger;
					TComfortControlTypes( ComfortControlledZoneNum ).DidHave( DualSetPointFanger ) = true;
					if ( ComfortIndex != 0 ) {
						SchedTypeIndex = ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchIndx( ComfortIndex );
						if ( SchedTypeIndex == 0 ) {
							ShowSevereError( "GetZoneAirSetpoints: Could not find " + ValidComfortControlTypes( DualSetPointFanger ) + " Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeName( ComfortIndex ) );
							ErrorsFound = true;
						}
					} else { // ComfortIndex = 0
						if ( CheckScheduleValue( CTIndex, DualSetPointFanger ) ) {
							ShowSevereError( "Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
							ShowContinueError( "..specifies thermal control type 4 (" + ValidComfortControlTypes( DualSetPointFanger ) + ") as the control type. Not valid for this zone." );
							ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
							ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );
							ErrorsFound = true;
						}
					}
					// CASE PIERCE
					// CASE KSU

				} else {
					ShowSevereError( "GetZoneAirSetpoints: Illegal control type for Zone=" + Zone( ActualZoneNum ).Name + ", Found value=" + TrimSigDigits( ControlTypeNum ) + ", in Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "..valid range values are [0,4]." );
					ErrorsFound = true;

				}}
			}

		}

		for ( ComfortControlledZoneNum = 1; ComfortControlledZoneNum <= NumComfortControlledZones; ++ComfortControlledZoneNum ) {

			ActualZoneNum = ComfortControlledZone( ComfortControlledZoneNum ).ActualZoneNum;
			CTIndex = ComfortControlledZone( ComfortControlledZoneNum ).ComfortSchedIndex;
			if ( CTIndex == 0 ) continue; // error caught elsewhere -- would just be confusing here

			for ( ControlTypeNum = 1; ControlTypeNum <= 12; ++ControlTypeNum ) {
				if ( TComfortControlTypes( ComfortControlledZoneNum ).MustHave( ControlTypeNum ) && TComfortControlTypes( ComfortControlledZoneNum ).DidHave( ControlTypeNum ) ) continue;

				{ auto const SELECT_CASE_var( ControlTypeNum );

				if ( SELECT_CASE_var == SglHeatSetPointFanger ) {
					if ( ! TComfortControlTypes( ComfortControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 1 (" + ValidComfortControlTypes( SglHeatSetPointFanger ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );

				} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {
					if ( ! TComfortControlTypes( ComfortControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 2 (" + ValidComfortControlTypes( SglCoolSetPointFanger ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );

				} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {
					if ( ! TComfortControlTypes( ComfortControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 3 (" + ValidComfortControlTypes( SglHCSetPointFanger ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );

				} else if ( SELECT_CASE_var == DualSetPointFanger ) {
					if ( ! TComfortControlTypes( ComfortControlledZoneNum ).MustHave( ControlTypeNum ) ) continue;
					ShowWarningError( "Schedule=" + ComfortControlledZone( ComfortControlledZoneNum ).ControlTypeSchedName );
					ShowContinueError( "...should include control type 4 (" + ValidComfortControlTypes( DualSetPointFanger ) + ") but does not." );
					ShowContinueError( "..reference " + cZControlTypes( iZC_TCTStat ) + '=' + ComfortControlledZone( ComfortControlledZoneNum ).Name );
					ShowContinueError( "..reference ZONE=" + ComfortControlledZone( ComfortControlledZoneNum ).ZoneName );

					// CASE PIERCE
					// CASE KSU

				} else {
				}}
			}
		}

		if ( allocated( TComfortControlTypes ) ) TComfortControlTypes.deallocate();

		// Get the Zone Air Capacitance Multiplier for use in the Predictor-Corrrector Procedure
		cCurrentModuleObject = "ZoneCapacitanceMultiplier:ResearchSpecial";
		NumNums = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumNums == 0 ) {
			ZoneVolCapMultpSens = 1.0;
			ZoneVolCapMultpMoist = 1.0;
			ZoneVolCapMultpCO2 = 1.0;
			ZoneVolCapMultpGenContam = 1.0;
		} else {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ZoneVolCapMultpSens = rNumericArgs( 1 );
			ZoneVolCapMultpMoist = rNumericArgs( 2 );
			ZoneVolCapMultpCO2 = rNumericArgs( 3 );
			ZoneVolCapMultpGenContam = rNumericArgs( 4 );
		}

		gio::write( OutputFileInits, Format_700 );
		gio::write( OutputFileInits, Format_701 ) << ZoneVolCapMultpSens << ZoneVolCapMultpMoist << ZoneVolCapMultpCO2 << ZoneVolCapMultpGenContam;

		cCurrentModuleObject = cZControlTypes( iZC_OTTStat );
		NumOpTempControlledZones = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumOpTempControlledZones > 0 ) {
			AnyOpTempControl = true;

			for ( OpTempContrlNum = 1; OpTempContrlNum <= NumOpTempControlledZones; ++OpTempContrlNum ) {
				GetObjectItem( cCurrentModuleObject, OpTempContrlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				// find matching name of  ZONECONTROL:THERMOSTAT object
				found = FindItem( cAlphaArgs( 1 ), TStatObjects );
				if ( found == 0 ) {
					// It might be in the TempControlledZones
					found = FindItem( cAlphaArgs( 1 ), TempControlledZone );
					if ( found == 0 ) { // throw error
						ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cZControlTypes( iZC_TStat ) + " reference not found." );
						ErrorsFound = true;
					} else {
						TempControlledZoneNum = found;
						TempControlledZone( TempControlledZoneNum ).OperativeTempControl = true;
						if ( SameString( cAlphaArgs( 2 ), "Scheduled" ) ) {
							TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled = true;
						}
						if ( ( ! ( SameString( cAlphaArgs( 2 ), "Scheduled" ) ) ) && ( ! ( SameString( cAlphaArgs( 2 ), "Constant" ) ) ) ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
							ErrorsFound = true;
						}

						TempControlledZone( TempControlledZoneNum ).FixedRadiativeFraction = rNumericArgs( 1 );
						TempControlledZone( TempControlledZoneNum ).OpTempRadiativeFractionSched = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( ( TempControlledZone( TempControlledZoneNum ).OpTempRadiativeFractionSched == 0 ) && ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) ) { //throw error
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}

						//check validity of fixed radiative fraction
						if ( ( TempControlledZone( TempControlledZoneNum ).FixedRadiativeFraction < 0.0 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) ) ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot be negative." );
							ErrorsFound = true;
						}
						if ( ( TempControlledZone( TempControlledZoneNum ).FixedRadiativeFraction >= 0.9 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) ) ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot >= .9." );
							ErrorsFound = true;
						}

						// check schedule min max.
						if ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) {
							ValidRadFractSched = CheckScheduleValueMinMax( TempControlledZone( TempControlledZoneNum ).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9 );
							if ( ! ValidRadFractSched ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid values " + cAlphaFieldNames( 3 ) + "=[" + cAlphaArgs( 3 ) + "\"." );
								ShowContinueError( "..Values outside of range [0.0,0.9)." );
								ErrorsFound = true;
							}
						}

						// CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
						SetupOutputVariable( "Zone Thermostat Operative Temperature [C]", ZnAirRpt( TempControlledZone( TempControlledZoneNum ).ActualZoneNum ).ThermOperativeTemp, "Zone", "Average", Zone( TempControlledZone( TempControlledZoneNum ).ActualZoneNum ).Name );
					}
				} else {
					for ( Item = 1; Item <= TStatObjects( found ).NumOfZones; ++Item ) {
						TempControlledZoneNum = TStatObjects( found ).TempControlledZoneStartPtr + Item - 1;
						if ( NumTempControlledZones == 0 ) continue;
						TempControlledZone( TempControlledZoneNum ).OperativeTempControl = true;
						if ( SameString( cAlphaArgs( 2 ), "Scheduled" ) ) {
							TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled = true;
						}
						if ( Item == 1 ) {
							if ( ( ! ( SameString( cAlphaArgs( 2 ), "Scheduled" ) ) ) && ( ! ( SameString( cAlphaArgs( 2 ), "Constant" ) ) ) ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
								ErrorsFound = true;
							}
						}

						TempControlledZone( TempControlledZoneNum ).FixedRadiativeFraction = rNumericArgs( 1 );
						TempControlledZone( TempControlledZoneNum ).OpTempRadiativeFractionSched = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( Item == 1 ) {
							if ( ( TempControlledZone( TempControlledZoneNum ).OpTempRadiativeFractionSched == 0 ) && ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) ) { //throw error
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
								ErrorsFound = true;
							}
						}

						//check validity of fixed radiative fraction
						if ( Item == 1 ) {
							if ( ( TempControlledZone( TempControlledZoneNum ).FixedRadiativeFraction < 0.0 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) ) ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot be negative." );
								ErrorsFound = true;
							}
						}
						if ( Item == 1 ) {
							if ( ( TempControlledZone( TempControlledZoneNum ).FixedRadiativeFraction >= 0.9 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) ) ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot >= .9." );
								ErrorsFound = true;
							}
						}

						// check schedule min max.
						if ( Item == 1 ) {
							if ( TempControlledZone( TempControlledZoneNum ).OpTempCntrlModeScheduled ) {
								ValidRadFractSched = CheckScheduleValueMinMax( TempControlledZone( TempControlledZoneNum ).OpTempRadiativeFractionSched, ">=", 0.0, "<", 0.9 );
								if ( ! ValidRadFractSched ) {
									ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid values " + cAlphaFieldNames( 3 ) + "=[" + cAlphaArgs( 3 ) + "\"." );
									ShowContinueError( "..Values outside of range [0.0,0.9)." );
									ErrorsFound = true;
								}
							}
						}

						// CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
						SetupOutputVariable( "Zone Thermostat Operative Temperature [C]", ZnAirRpt( TempControlledZone( TempControlledZoneNum ).ActualZoneNum ).ThermOperativeTemp, "Zone", "Average", Zone( TempControlledZone( TempControlledZoneNum ).ActualZoneNum ).Name );
					} // TStat Objects Loop
				} // found thermostat referene
			} //loop over NumOpTempControlledZones
		} // NumOpTempControlledZones > 0

		// Overcool dehumidificaton GetInput starts here
		cCurrentModuleObject = cZControlTypes( iZC_TandHStat );
		NumTempAndHumidityControlledZones = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumTempAndHumidityControlledZones > 0 ) {
			AnyZoneTempAndHumidityControl = true;

			for ( TempHumidityCntrlNum = 1; TempHumidityCntrlNum <= NumTempAndHumidityControlledZones; ++TempHumidityCntrlNum ) {
				GetObjectItem( cCurrentModuleObject, TempHumidityCntrlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				// find matching name of  ZONECONTROL:THERMOSTAT object
				found = FindItem( cAlphaArgs( 1 ), TStatObjects );
				if ( found == 0 ) {
					// It might be in the TempControlledZones
					found = FindItem( cAlphaArgs( 1 ), TempControlledZone );
					if ( found == 0 ) { // throw error
						ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cZControlTypes( iZC_TStat ) + " reference not found." );
						ErrorsFound = true;
					} else {
						TempControlledZoneNum = found;
						TempControlledZone( TempControlledZoneNum ).DehumidifyingSched = cAlphaArgs( 2 );
						TempControlledZone( TempControlledZoneNum ).DehumidifyingSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( TempControlledZone( TempControlledZoneNum ).DehumidifyingSchedIndex == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
							ErrorsFound = true;
						}
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControl = true;
						if ( ( SameString( cAlphaArgs( 3 ), "None" ) ) ) {
							TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControl = false;
						}
						if ( SameString( cAlphaArgs( 4 ), "Scheduled" ) ) {
							TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled = true;
						}
						if ( ( ! ( SameString( cAlphaArgs( 4 ), "Scheduled" ) ) ) && ( ! ( SameString( cAlphaArgs( 4 ), "Constant" ) ) ) ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
							ErrorsFound = true;
						}

						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolConstRange = rNumericArgs( 1 );
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolRangeSchedIndex = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolRangeSchedIndex == 0 ) && ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) ) { //throw error
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\" not found." );
							ErrorsFound = true;
						}

						//check validity of zone Overcool constant range
						if ( ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolConstRange < 0.0 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) ) ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot be negative." );
							ErrorsFound = true;
						}
						if ( ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolConstRange > 3.0 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) ) ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot be > 3.0" );
							ErrorsFound = true;
						}

						// check zone Overcool range schedule min/max values.
						if ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) {
							ValidZoneOvercoolRangeSched = CheckScheduleValueMinMax( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0 );
							if ( ! ValidZoneOvercoolRangeSched ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid values " + cAlphaFieldNames( 5 ) + "=[" + cAlphaArgs( 5 ) + "\"." );
								ShowContinueError( "..Values outside of range [0.0,3.0]." );
								ErrorsFound = true;
							}
						}
						// check Overcool Control Ratio limits
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControlRatio = rNumericArgs( 2 );
						if ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControlRatio < 0.0 ) {
							ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 2 ) + " invalid " + cNumericFieldNames( 2 ) + "=[" + TrimSigDigits( rNumericArgs( 2 ), 2 ) + "\" cannot be negative." );
							ErrorsFound = true;
						}
					}
				} else {
					for ( Item = 1; Item <= TStatObjects( found ).NumOfZones; ++Item ) {
						TempControlledZoneNum = TStatObjects( found ).TempControlledZoneStartPtr + Item - 1;
						TempControlledZone( TempControlledZoneNum ).DehumidifyingSched = cAlphaArgs( 2 );
						TempControlledZone( TempControlledZoneNum ).DehumidifyingSchedIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( TempControlledZone( TempControlledZoneNum ).DehumidifyingSchedIndex == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
							ErrorsFound = true;
						}
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControl = true;
						if ( ( SameString( cAlphaArgs( 3 ), "None" ) ) ) {
							TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControl = false;
						}
						if ( SameString( cAlphaArgs( 4 ), "Scheduled" ) ) {
							TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled = false;
						}
						if ( Item == 1 ) {
							if ( ( ! ( SameString( cAlphaArgs( 4 ), "Scheduled" ) ) ) && ( ! ( SameString( cAlphaArgs( 4 ), "Constant" ) ) ) ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
								ErrorsFound = true;
							}
						}
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolConstRange = rNumericArgs( 1 );
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolRangeSchedIndex = GetScheduleIndex( cAlphaArgs( 6 ) );
						if ( Item == 1 ) {
							if ( ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolRangeSchedIndex == 0 ) && ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) ) { //throw error
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\" not found." );
								ErrorsFound = true;
							}
						}
						//check validity of zone Overcool constant range
						if ( Item == 1 ) {
							if ( ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolConstRange < 0.0 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) ) ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot be negative." );
								ErrorsFound = true;
							}
						}
						if ( Item == 1 ) {
							if ( ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolConstRange > 3.0 ) && ( ! ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) ) ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 2 ) + "\" cannot > 3.0" );
								ErrorsFound = true;
							}
						}
						// check zone Overcool range schedule min/max values.
						if ( Item == 1 ) {
							if ( TempControlledZone( TempControlledZoneNum ).OvercoolCntrlModeScheduled ) {
								ValidZoneOvercoolRangeSched = CheckScheduleValueMinMax( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolRangeSchedIndex, ">=", 0.0, "<=", 3.0 );
								if ( ! ValidZoneOvercoolRangeSched ) {
									ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) + " invalid values " + cAlphaFieldNames( 5 ) + "=[" + cAlphaArgs( 5 ) + "\"." );
									ShowContinueError( "..Values outside of range [0.0,3.0]." );
									ErrorsFound = true;
								}
							}
						}
						TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControlRatio = rNumericArgs( 2 );
						// check Overcool Control Ratio limits
						if ( Item == 1 ) {
							if ( TempControlledZone( TempControlledZoneNum ).ZoneOvercoolControlRatio < 0.0 ) {
								ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 2 ) + " invalid " + cNumericFieldNames( 2 ) + "=[" + TrimSigDigits( rNumericArgs( 2 ), 2 ) + "\" cannot be negative." );
								ErrorsFound = true;
							}
						}

					} // TStat Objects Loop
				} // found thermostat reference
			} //loop over NumTempAndHumidityControlledZones
		} // NumTempAndHumidityControlledZones > 0

		// Staged thermostat control inputs start
		cCurrentModuleObject = cZControlTypes( iZC_StagedDual );
		NumStageControlledZones = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumStageControlledZones > 0 ) StagedTStatObjects.allocate( NumStageControlledZones );

		// Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
		NumStageCtrZone = 0;
		for ( Item = 1; Item <= NumStageControlledZones; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), StagedTStatObjects, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			StagedTStatObjects( Item ).Name = cAlphaArgs( 1 );
			Item1 = FindItemInList( cAlphaArgs( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( cAlphaArgs( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				StagedTStatObjects( Item ).StageControlledZoneStartPtr = NumStageCtrZone + 1;
				++NumStageCtrZone;
				StagedTStatObjects( Item ).NumOfZones = 1;
				StagedTStatObjects( Item ).ZoneListActive = false;
				StagedTStatObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				StagedTStatObjects( Item ).TempControlledZoneStartPtr = NumStageCtrZone + 1;
				NumStageCtrZone += ZoneList( ZLItem ).NumOfZones;
				StagedTStatObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				StagedTStatObjects( Item ).ZoneListActive = true;
				StagedTStatObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowSevereError( "GetStagedDualSetpoint: Errors with invalid names in " + cCurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			NumStageCtrZone = 0;
		}

		if ( NumStageCtrZone > 0 ) {
			StageControlledZone.allocate( NumStageCtrZone );
			StageZoneLogic.dimension( NumOfZones, false );

			StageControlledZoneNum = 0;
			for ( Item = 1; Item <= NumStageControlledZones; ++Item ) {
				GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				for ( Item1 = 1; Item1 <= StagedTStatObjects( Item ).NumOfZones; ++Item1 ) {
					++StageControlledZoneNum;
					if ( StagedTStatObjects( Item ).ZoneListActive ) {
						cAlphaArgs( 2 ) = Zone( ZoneList( StagedTStatObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name;
					}
					ZoneAssigned = FindItemInList( cAlphaArgs( 2 ), StageControlledZone, &ZoneStagedControls::ZoneName, StageControlledZoneNum - 1 );
					if ( ZoneAssigned == 0 ) {
						StageControlledZone( StageControlledZoneNum ).ZoneName = cAlphaArgs( 2 );
						StageControlledZone( StageControlledZoneNum ).ActualZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone );
						if ( StageControlledZone( StageControlledZoneNum ).ActualZoneNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
							ErrorsFound = true;
						} else {
							//           Zone(StageControlledZone(StageControlledZoneNum)%ActualZoneNum)%StageControlledZoneIndex = StageControlledZoneNum
						}
						StageZoneLogic( StageControlledZone( StageControlledZoneNum ).ActualZoneNum ) = true;
					} else {
						StageControlledZone( StageControlledZoneNum ).ZoneName = cAlphaArgs( 2 ); // for continuity
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" zone previously assigned." );
						ShowContinueError( "...Zone was previously assigned to Thermostat=\"" + StageControlledZone( ZoneAssigned ).Name + "\"." );
						ErrorsFound = true;
						continue;
					}

					if ( ! StagedTStatObjects( Item ).ZoneListActive ) {
						StageControlledZone( StageControlledZoneNum ).Name = cAlphaArgs( 1 );
					} else {
						CheckCreatedZoneItemName( RoutineName, cCurrentModuleObject, Zone( ZoneList( StagedTStatObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( StagedTStatObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, StagedTStatObjects( Item ).Name, StageControlledZone, StageControlledZoneNum - 1, StageControlledZone( StageControlledZoneNum ).Name, errFlag );
						if ( errFlag ) ErrorsFound = true;
					}

					StageControlledZone( StageControlledZoneNum ).NumOfHeatStages = rNumericArgs( 1 );
					if ( rNumericArgs( 1 ) < 1 || rNumericArgs( 1 ) > 4 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid range " + cNumericFieldNames( 1 ) + "=\"" + RoundSigDigits( rNumericArgs( 1 ), 0 ) + "\"" );
						ShowContinueError( "..contains values outside of range [1,4]." );
						ErrorsFound = true;
					}

					StageControlledZone( StageControlledZoneNum ).HeatSetBaseSchedName = cAlphaArgs( 3 );
					StageControlledZone( StageControlledZoneNum ).HSBchedIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
					if ( Item1 == 1 ) { // only show error on first of several if zone list
						if ( StageControlledZone( StageControlledZoneNum ).HSBchedIndex == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					StageControlledZone( StageControlledZoneNum ).HeatThroRange = rNumericArgs( 2 );
					if ( rNumericArgs( 1 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" negative value is found at " + cNumericFieldNames( 2 ) + "=\"" + RoundSigDigits( rNumericArgs( 2 ), 1 ) + "\"" );
						ShowContinueError( ".. The minumum value is 0." );
						ErrorsFound = true;
					}

					if ( StageControlledZone( StageControlledZoneNum ).NumOfHeatStages > 0 ) {
						StageControlledZone( StageControlledZoneNum ).HeatTOffset.allocate( StageControlledZone( StageControlledZoneNum ).NumOfHeatStages );
						for ( i = 1; i <= StageControlledZone( StageControlledZoneNum ).NumOfHeatStages; ++i ) {
							StageControlledZone( StageControlledZoneNum ).HeatTOffset( i ) = rNumericArgs( 2 + i );
							if ( rNumericArgs( 2 + i ) > 0.0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" positive value is found at " + cNumericFieldNames( 2 + i ) + "=\"" + RoundSigDigits( rNumericArgs( 2 + i ), 1 ) + "\"" );
								ShowContinueError( ".. The maximum value is 0." );
								ErrorsFound = true;
							}
							if ( lNumericFieldBlanks( 2 + i ) ) {
								ShowSevereError( cCurrentModuleObject + " object =" + cAlphaArgs( 1 ) + ". The input of " + cNumericFieldNames( 2 + i ) + " is required, but a blank is found." );
								ErrorsFound = true;
							}
							if ( i > 1 ) {
								if ( rNumericArgs( 2 + i ) >= rNumericArgs( 1 + i ) ) {
									ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" The value at " + cNumericFieldNames( 2 + i ) + "=\"" + RoundSigDigits( rNumericArgs( 2 + i ), 1 ) + "\" has to be less than " );
									ShowContinueError( cNumericFieldNames( 1 + i ) + "=\"" + RoundSigDigits( rNumericArgs( 1 + i ), 1 ) );
									ErrorsFound = true;
								}
							}
						}
					}

					StageControlledZone( StageControlledZoneNum ).NumOfCoolStages = rNumericArgs( 7 );
					if ( rNumericArgs( 7 ) < 1 || rNumericArgs( 7 ) > 4 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid range " + cNumericFieldNames( 7 ) + "=\"" + RoundSigDigits( rNumericArgs( 7 ), 0 ) + "\"" );
						ShowContinueError( "..contains values outside of range [1,4]." );
						ErrorsFound = true;
					}

					StageControlledZone( StageControlledZoneNum ).CoolSetBaseSchedName = cAlphaArgs( 4 );
					StageControlledZone( StageControlledZoneNum ).CSBchedIndex = GetScheduleIndex( cAlphaArgs( 4 ) );
					if ( Item1 == 1 ) { // only show error on first of several if zone list
						if ( StageControlledZone( StageControlledZoneNum ).CSBchedIndex == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}

					StageControlledZone( StageControlledZoneNum ).CoolThroRange = rNumericArgs( 8 );
					if ( rNumericArgs( 8 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" negative value is found at " + cNumericFieldNames( 8 ) + "=\"" + RoundSigDigits( rNumericArgs( 8 ), 1 ) + "\"" );
						ShowContinueError( ".. The minumum value is 0." );
						ErrorsFound = true;
					}

					if ( StageControlledZone( StageControlledZoneNum ).NumOfCoolStages > 0 ) {
						StageControlledZone( StageControlledZoneNum ).CoolTOffset.allocate( StageControlledZone( StageControlledZoneNum ).NumOfCoolStages );
						for ( i = 1; i <= StageControlledZone( StageControlledZoneNum ).NumOfCoolStages; ++i ) {
							StageControlledZone( StageControlledZoneNum ).CoolTOffset( i ) = rNumericArgs( 8 + i );
							if ( rNumericArgs( 8 + i ) < 0.0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" negative value is found at " + cNumericFieldNames( 8 + i ) + "=\"" + RoundSigDigits( rNumericArgs( 8 + i ), 1 ) + "\"" );
								ShowContinueError( ".. The minimum value is 0." );
								ErrorsFound = true;
							}
							if ( lNumericFieldBlanks( 8 + i ) ) {
								ShowSevereError( cCurrentModuleObject + " object =" + cAlphaArgs( 1 ) + ". The input of " + cNumericFieldNames( 8 + i ) + " is required, but a blank is found." );
								ErrorsFound = true;
							}
							if ( i > 1 ) {
								if ( rNumericArgs( 8 + i ) <= rNumericArgs( 7 + i ) ) {
									ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" The value at " + cNumericFieldNames( 8 + i ) + "=\"" + RoundSigDigits( rNumericArgs( 8 + i ), 1 ) + "\" has to be greater than " );
									ShowContinueError( cNumericFieldNames( 7 + i ) + "=\"" + RoundSigDigits( rNumericArgs( 7 + i ), 1 ) );
									ErrorsFound = true;
								}
							}
						}
					}
				}
			} //loop over NumStageControlledZones
			if ( ( GetNumObjectsFound( "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed" ) == 0 ) && ( GetNumObjectsFound( "AirLoopHVAC:UnitarySystem" ) == 0 ) && ( GetNumObjectsFound( "SetpointManager:SingleZone:OneStageCooling" ) == 0 ) && ( GetNumObjectsFound( "SetpointManager:SingleZone:OneStageHeating" ) == 0 ) ) {
				ShowWarningError( cCurrentModuleObject + " is applicable to only selected HVAC objects which are missing from input." );
				ShowContinueError( "Model should include one or more of the following objects:  " );
				ShowContinueError( "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, AirLoopHVAC:UnitarySystem, " );
				ShowContinueError( "SetpointManager:SingleZone:OneStageCooling, and/or SetpointManager:SingleZone:OneStageHeating. The simulation continues..." );
			}
		} // NumStageControlledZones > 0

		if ( ErrorsFound ) {
			ShowFatalError( "Errors getting Zone Control input data.  Preceding condition(s) cause termination." );
		}

	}

	void
	InitZoneAirSetPoints()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russell Taylor
		//       DATE WRITTEN   September 1998
		//       MODIFIED       November 2004, M. J. Witte additional report variables
		//       MODIFIED       L.Gu, May 2006
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the data for the zone air setpoints.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitZoneAirSetpoints: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int ZoneNum;
		//////////// hoisted into namespace changed to InitZoneAirSetPointsOneTimeFlag////////////
		//static bool MyOneTimeFlag( true );
		//////////////////////////////////////
		static bool MyEnvrnFlag( true );
		static bool MyDayFlag( true );
		static bool ErrorsFound( false );
		static bool ControlledZonesChecked( false );
		bool FirstSurfFlag;
		int TRefFlag; // Flag for Reference Temperature process in Zones
		int SurfNum;

		// FLOW:
		if ( InitZoneAirSetPointsOneTimeFlag ) {
			TempZoneThermostatSetPoint.dimension( NumOfZones, 0.0 );
			ZoneThermostatSetPointHi.dimension( NumOfZones, 0.0 );
			ZoneThermostatSetPointLo.dimension( NumOfZones, 0.0 );

			LoadCorrectionFactor.dimension( NumOfZones, 0.0 ); //PH 3/3/04
			TempControlType.dimension( NumOfZones, 0 );
			if ( NumComfortControlledZones > 0 ) {
				ComfortControlType.dimension( NumOfZones, 0 );
				ZoneComfortControlsFanger.allocate( NumOfZones );
			}
			ZoneSetPointLast.dimension( NumOfZones, 0.0 );
			Setback.dimension( NumOfZones, false );
			DeadBandOrSetback.dimension( NumOfZones, false );
			CurDeadBandOrSetback.dimension( NumOfZones, false );
			SNLoadHeatEnergy.dimension( NumOfZones, 0.0 );
			SNLoadCoolEnergy.dimension( NumOfZones, 0.0 );
			SNLoadHeatRate.dimension( NumOfZones, 0.0 );
			SNLoadCoolRate.dimension( NumOfZones, 0.0 );
			SNLoadPredictedRate.dimension( NumOfZones, 0.0 );
			SNLoadPredictedHSPRate.dimension( NumOfZones, 0.0 );
			SNLoadPredictedCSPRate.dimension( NumOfZones, 0.0 );
			MoisturePredictedRate.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus1.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus2.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus3.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus4.dimension( NumOfZones, 0.0 );
			DSWZoneTimeMinus1.dimension( NumOfZones, 0.0 );
			DSWZoneTimeMinus2.dimension( NumOfZones, 0.0 );
			DSWZoneTimeMinus3.dimension( NumOfZones, 0.0 );
			DSWZoneTimeMinus4.dimension( NumOfZones, 0.0 );
			ZoneAirHumRatTemp.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus1Temp.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus2Temp.dimension( NumOfZones, 0.0 );
			WZoneTimeMinus3Temp.dimension( NumOfZones, 0.0 );
			WZoneTimeMinusP.dimension( NumOfZones, 0.0 );
			TempIndZnLd.dimension( NumOfZones, 0.0 );
			TempDepZnLd.dimension( NumOfZones, 0.0 );
			NonAirSystemResponse.dimension( NumOfZones, 0.0 );
			SysDepZoneLoads.dimension( NumOfZones, 0.0 );
			SysDepZoneLoadsLagged.dimension( NumOfZones, 0.0 );
			ZoneAirRelHum.dimension( NumOfZones, 0.0 );
			ZoneWMX.dimension( NumOfZones, 0.0 );
			ZoneWM2.dimension( NumOfZones, 0.0 );
			ZoneT1.dimension( NumOfZones, 0.0 );
			ZoneW1.dimension( NumOfZones, 0.0 );

			ListSNLoadHeatEnergy.dimension( NumOfZoneLists, 0.0 );
			ListSNLoadCoolEnergy.dimension( NumOfZoneLists, 0.0 );
			ListSNLoadHeatRate.dimension( NumOfZoneLists, 0.0 );
			ListSNLoadCoolRate.dimension( NumOfZoneLists, 0.0 );

			GroupSNLoadHeatEnergy.dimension( NumOfZoneGroups, 0.0 );
			GroupSNLoadCoolEnergy.dimension( NumOfZoneGroups, 0.0 );
			GroupSNLoadHeatRate.dimension( NumOfZoneGroups, 0.0 );
			GroupSNLoadCoolRate.dimension( NumOfZoneGroups, 0.0 );
			AIRRAT.dimension( NumOfZones, 0.0 );
			ZTM1.dimension( NumOfZones, 0.0 );
			ZTM2.dimension( NumOfZones, 0.0 );
			ZTM3.dimension( NumOfZones, 0.0 );

			// Allocate Derived Types
			ZoneSysEnergyDemand.allocate( NumOfZones );
			ZoneSysMoistureDemand.allocate( NumOfZones );

			for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
				FirstSurfFlag = true;
				for ( SurfNum = Zone( Loop ).SurfaceFirst; SurfNum <= Zone( Loop ).SurfaceLast; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

					if ( FirstSurfFlag ) {
						TRefFlag = Surface( SurfNum ).TAirRef;
						FirstSurfFlag = false;
					}
					// for each particular zone, the reference air temperature(s) should be the same
					// (either mean air, bulk air, or supply air temp).
					if ( Surface( SurfNum ).TAirRef != TRefFlag ) {
						ShowWarningError( "Different reference air temperatures for difference surfaces encountered in zone " + Zone( Loop ).Name );
					}

				}
			}

			// CurrentModuleObject='Zone'
			for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
				SetupOutputVariable( "Zone Air System Sensible Heating Energy [J]", SNLoadHeatEnergy( Loop ), "System", "Sum", Zone( Loop ).Name, _, "ENERGYTRANSFER", "Heating", _, "Building", Zone( Loop ).Name, Zone( Loop ).Multiplier, Zone( Loop ).ListMultiplier );
				SetupOutputVariable( "Zone Air System Sensible Cooling Energy [J]", SNLoadCoolEnergy( Loop ), "System", "Sum", Zone( Loop ).Name, _, "ENERGYTRANSFER", "Cooling", _, "Building", Zone( Loop ).Name, Zone( Loop ).Multiplier, Zone( Loop ).ListMultiplier );
				SetupOutputVariable( "Zone Air System Sensible Heating Rate [W]", SNLoadHeatRate( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Air System Sensible Cooling Rate [W]", SNLoadCoolRate( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Air Temperature [C]", ZT( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Thermostat Air Temperature [C]", TempTstatAir( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Air Humidity Ratio []", ZoneAirHumRat( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Air Relative Humidity [%]", ZoneAirRelHum( Loop ), "System", "Average", Zone( Loop ).Name );
				// This output variable is for the predicted Heating/Cooling load for the zone which can be compared to actual load
				// These report variables are not multiplied by zone and group multipliers
				SetupOutputVariable( "Zone Predicted Sensible Load to Setpoint Heat Transfer Rate [W]", SNLoadPredictedRate( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate [W]", SNLoadPredictedHSPRate( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate [W]", SNLoadPredictedCSPRate( Loop ), "System", "Average", Zone( Loop ).Name );
				// This output variable is for the predicted moisture load for the zone with humidity controlled specified.
				SetupOutputVariable( "Zone Predicted Moisture Load Moisture Transfer Rate [kgWater/s]", MoisturePredictedRate( Loop ), "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate [kgWater/s]", ZoneSysMoistureDemand( Loop ).OutputRequiredToHumidifyingSP, "System", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate [kgWater/s]", ZoneSysMoistureDemand( Loop ).OutputRequiredToDehumidifyingSP, "System", "Average", Zone( Loop ).Name );
				// Zone thermostat setpoints
				SetupOutputVariable( "Zone Thermostat Control Type []", TempControlType( Loop ), "Zone", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Thermostat Heating Setpoint Temperature [C]", ZoneThermostatSetPointLo( Loop ), "Zone", "Average", Zone( Loop ).Name );
				SetupOutputVariable( "Zone Thermostat Cooling Setpoint Temperature [C]", ZoneThermostatSetPointHi( Loop ), "Zone", "Average", Zone( Loop ).Name );

				SetupOutputVariable( "Zone Predicted Sensible Load Room Air Correction Factor [ ]", LoadCorrectionFactor( Loop ), "System", "Average", Zone( Loop ).Name );

				if ( allocated( StageZoneLogic ) ) {
					if ( StageZoneLogic( Loop ) ) {
						SetupOutputVariable( "Zone Thermostat Staged Number []", ZoneSysEnergyDemand( Loop ).StageNum, "System", "Average", Zone( Loop ).Name );
					}
				}

			} // Loop

			// Thermal comfort control output
			if ( NumComfortControlledZones > 0 ) {
				// CurrentModuleObject='ZoneControl:Thermostat:ThermalComfort'
				for ( Loop = 1; Loop <= NumComfortControlledZones; ++Loop ) {
					ZoneNum = ComfortControlledZone( Loop ).ActualZoneNum;
					SetupOutputVariable( "Zone Thermal Comfort Control Type []", ComfortControlType( ZoneNum ), "Zone", "Average", Zone( ZoneNum ).Name );
					SetupOutputVariable( "Zone Thermal Comfort Control Fanger Low Setpoint PMV []", ZoneComfortControlsFanger( ZoneNum ).LowPMV, "Zone", "Average", Zone( ZoneNum ).Name );
					SetupOutputVariable( "Zone Thermal Comfort Control Fanger High Setpoint PMV []", ZoneComfortControlsFanger( ZoneNum ).HighPMV, "Zone", "Average", Zone( ZoneNum ).Name );
				}
			}

			// CurrentModuleObject='ZoneList'
			for ( Loop = 1; Loop <= NumOfZoneLists; ++Loop ) {
				SetupOutputVariable( "Zone List Sensible Heating Energy [J]", ListSNLoadHeatEnergy( Loop ), "System", "Sum", ZoneList( Loop ).Name );
				SetupOutputVariable( "Zone List Sensible Cooling Energy [J]", ListSNLoadCoolEnergy( Loop ), "System", "Sum", ZoneList( Loop ).Name );
				SetupOutputVariable( "Zone List Sensible Heating Rate [W]", ListSNLoadHeatRate( Loop ), "System", "Average", ZoneList( Loop ).Name );
				SetupOutputVariable( "Zone List Sensible Cooling Rate [W]", ListSNLoadCoolRate( Loop ), "System", "Average", ZoneList( Loop ).Name );
			} // Loop

			// CurrentModuleObject='ZoneGroup'
			for ( Loop = 1; Loop <= NumOfZoneGroups; ++Loop ) {
				SetupOutputVariable( "Zone Group Sensible Heating Energy [J]", GroupSNLoadHeatEnergy( Loop ), "System", "Sum", ZoneGroup( Loop ).Name );
				SetupOutputVariable( "Zone Group Sensible Cooling Energy [J]", GroupSNLoadCoolEnergy( Loop ), "System", "Sum", ZoneGroup( Loop ).Name );
				SetupOutputVariable( "Zone Group Sensible Heating Rate [W]", GroupSNLoadHeatRate( Loop ), "System", "Average", ZoneGroup( Loop ).Name );
				SetupOutputVariable( "Zone Group Sensible Cooling Rate [W]", GroupSNLoadCoolRate( Loop ), "System", "Average", ZoneGroup( Loop ).Name );
			} // Loop

			InitZoneAirSetPointsOneTimeFlag = false;
		}

		// Do the Begin Environment initializations
		if ( MyEnvrnFlag && BeginEnvrnFlag ) {
			AIRRAT = 0.0;
			ZTM1 = 0.0;
			ZTM2 = 0.0;
			ZTM3 = 0.0;
			WZoneTimeMinus1 = OutHumRat;
			WZoneTimeMinus2 = OutHumRat;
			WZoneTimeMinus3 = OutHumRat;
			WZoneTimeMinus4 = OutHumRat;
			WZoneTimeMinusP = OutHumRat;
			DSWZoneTimeMinus1 = OutHumRat;
			DSWZoneTimeMinus2 = OutHumRat;
			DSWZoneTimeMinus3 = OutHumRat;
			DSWZoneTimeMinus4 = OutHumRat;
			WZoneTimeMinus1Temp = 0.0;
			WZoneTimeMinus2Temp = 0.0;
			WZoneTimeMinus3Temp = 0.0;
			ZoneAirHumRatTemp = 0.0;
			TempZoneThermostatSetPoint = 0.0;
			ZoneThermostatSetPointHi = 0.0;
			ZoneThermostatSetPointLo = 0.0;

			LoadCorrectionFactor = 1.0; //PH 3/3/04
			TempControlType = 0;
			for ( auto & e : ZoneSysEnergyDemand ) {
				e.RemainingOutputRequired = 0.0;
				e.TotalOutputRequired = 0.0;
			}
			for ( auto & e : ZoneSysMoistureDemand ) {
				e.RemainingOutputRequired = 0.0;
				e.TotalOutputRequired = 0.0;
			}
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired = 0.0;
				if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP = 0.0;
				if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP = 0.0;
				if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired = 0.0;
				if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP = 0.0;
				if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP = 0.0;
			}

			DeadBandOrSetback = false;
			SNLoadHeatEnergy = 0.0;
			SNLoadCoolEnergy = 0.0;
			SNLoadHeatRate = 0.0;
			SNLoadCoolRate = 0.0;
			SNLoadPredictedRate = 0.0;
			SNLoadPredictedHSPRate = 0.0;
			SNLoadPredictedCSPRate = 0.0;
			MoisturePredictedRate = 0.0;
			TempIndZnLd = 0.0;
			TempDepZnLd = 0.0;
			NonAirSystemResponse = 0.0;
			SysDepZoneLoads = 0.0;
			SysDepZoneLoadsLagged = 0.0;
			ZoneAirRelHum = 0.0;
			for ( auto & e : Zone ) e.NoHeatToReturnAir = false;
			ZoneT1 = 0.0;
			ZoneW1 = OutHumRat;
			ZoneWMX = OutHumRat;
			ZoneWM2 = OutHumRat;

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// Do the Begin Day initializations
		if ( MyDayFlag && BeginDayFlag ) {
			MyDayFlag = false;
		}

		if ( ! BeginDayFlag ) {
			MyDayFlag = true;
		}

		for ( Loop = 1; Loop <= NumTempControlledZones; ++Loop ) {
			if ( ZoneEquipInputsFilled && ! ControlledZonesChecked ) {
				if ( ! VerifyControlledZoneForThermostat( TempControlledZone( Loop ).ZoneName ) ) {
					ShowSevereError( RoutineName + "Zone=\"" + TempControlledZone( Loop ).ZoneName + "\" has specified a Thermostatic control but is not a controlled zone." );
					ShowContinueError( "...must have a ZoneHVAC:EquipmentConnections specification for this zone." );
					ErrorsFound = true;
				}
			}

			if ( TempControlledZone( Loop ).ManageDemand ) {
				ZoneNum = TempControlledZone( Loop ).ActualZoneNum;

				{ auto const SELECT_CASE_var( TempControlType( ZoneNum ) );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
					if ( TempZoneThermostatSetPoint( ZoneNum ) > TempControlledZone( Loop ).HeatingResetLimit ) {
						TempZoneThermostatSetPoint( ZoneNum ) = TempControlledZone( Loop ).HeatingResetLimit;
						ZoneThermostatSetPointLo( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );
					}

				} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
					if ( TempZoneThermostatSetPoint( ZoneNum ) < TempControlledZone( Loop ).CoolingResetLimit ) {
						TempZoneThermostatSetPoint( ZoneNum ) = TempControlledZone( Loop ).CoolingResetLimit;
						ZoneThermostatSetPointHi( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );
					}

				} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
					if ( ( TempZoneThermostatSetPoint( ZoneNum ) > TempControlledZone( Loop ).HeatingResetLimit ) || ( TempZoneThermostatSetPoint( ZoneNum ) < TempControlledZone( Loop ).CoolingResetLimit ) ) {

						TempControlType( ZoneNum ) = DualSetPointWithDeadBand;
						ZoneThermostatSetPointLo( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );
						ZoneThermostatSetPointHi( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );

						if ( ZoneThermostatSetPointLo( ZoneNum ) > TempControlledZone( Loop ).HeatingResetLimit ) ZoneThermostatSetPointLo( ZoneNum ) = TempControlledZone( Loop ).HeatingResetLimit;
						if ( ZoneThermostatSetPointHi( ZoneNum ) < TempControlledZone( Loop ).CoolingResetLimit ) ZoneThermostatSetPointHi( ZoneNum ) = TempControlledZone( Loop ).CoolingResetLimit;

					}

				} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
					if ( ZoneThermostatSetPointLo( ZoneNum ) > TempControlledZone( Loop ).HeatingResetLimit ) ZoneThermostatSetPointLo( ZoneNum ) = TempControlledZone( Loop ).HeatingResetLimit;
					if ( ZoneThermostatSetPointHi( ZoneNum ) < TempControlledZone( Loop ).CoolingResetLimit ) ZoneThermostatSetPointHi( ZoneNum ) = TempControlledZone( Loop ).CoolingResetLimit;

				} else {
					// Do nothing

				}}
			}
		}

		for ( Loop = 1; Loop <= NumComfortControlledZones; ++Loop ) {
			if ( ZoneEquipInputsFilled && ! ControlledZonesChecked ) {
				if ( ! VerifyControlledZoneForThermostat( ComfortControlledZone( Loop ).ZoneName ) ) {
					ShowSevereError( RoutineName + "Zone=\"" + ComfortControlledZone( Loop ).ZoneName + "\" has specified a Comfort control but is not a controlled zone." );
					ShowContinueError( "...must have a ZoneHVAC:EquipmentConnections specification for this zone." );
					ErrorsFound = true;
				}
			}
			if ( ComfortControlledZone( Loop ).ManageDemand ) {
				ZoneNum = ComfortControlledZone( Loop ).ActualZoneNum;

				{ auto const SELECT_CASE_var( ComfortControlType( ZoneNum ) );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SglHeatSetPointFanger ) {
					if ( TempZoneThermostatSetPoint( ZoneNum ) >= ComfortControlledZone( Loop ).HeatingResetLimit ) {
						TempZoneThermostatSetPoint( ZoneNum ) = ComfortControlledZone( Loop ).HeatingResetLimit;
						ZoneThermostatSetPointLo( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );
						TempControlType( ZoneNum ) = SingleHeatingSetPoint;
					}

				} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {
					if ( TempZoneThermostatSetPoint( ZoneNum ) <= ComfortControlledZone( Loop ).CoolingResetLimit ) {
						TempZoneThermostatSetPoint( ZoneNum ) = ComfortControlledZone( Loop ).CoolingResetLimit;
						ZoneThermostatSetPointHi( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );
						TempControlType( ZoneNum ) = SingleCoolingSetPoint;
					}

				} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {
					if ( ( TempZoneThermostatSetPoint( ZoneNum ) >= ComfortControlledZone( Loop ).HeatingResetLimit ) || ( TempZoneThermostatSetPoint( ZoneNum ) <= ComfortControlledZone( Loop ).CoolingResetLimit ) ) {

						TempControlType( ZoneNum ) = DualSetPointWithDeadBand;
						ZoneThermostatSetPointLo( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );
						ZoneThermostatSetPointHi( ZoneNum ) = TempZoneThermostatSetPoint( ZoneNum );

						if ( ZoneThermostatSetPointLo( ZoneNum ) >= ComfortControlledZone( Loop ).HeatingResetLimit ) ZoneThermostatSetPointLo( ZoneNum ) = ComfortControlledZone( Loop ).HeatingResetLimit;
						if ( ZoneThermostatSetPointHi( ZoneNum ) <= ComfortControlledZone( Loop ).CoolingResetLimit ) ZoneThermostatSetPointHi( ZoneNum ) = ComfortControlledZone( Loop ).CoolingResetLimit;

					}

				} else if ( SELECT_CASE_var == DualSetPointFanger ) {
					TempControlType( ZoneNum ) = DualSetPointWithDeadBand;
					if ( ZoneThermostatSetPointLo( ZoneNum ) >= ComfortControlledZone( Loop ).HeatingResetLimit ) ZoneThermostatSetPointLo( ZoneNum ) = ComfortControlledZone( Loop ).HeatingResetLimit;
					if ( ZoneThermostatSetPointHi( ZoneNum ) <= ComfortControlledZone( Loop ).CoolingResetLimit ) ZoneThermostatSetPointHi( ZoneNum ) = ComfortControlledZone( Loop ).CoolingResetLimit;

				} else {
					// Do nothing

				}}
			} //Demand manager
		}

		for ( Loop = 1; Loop <= NumTempControlledZones; ++Loop ) {
			if ( TempControlledZone( Loop ).EMSOverrideHeatingSetPointOn ) {
				ZoneNum = TempControlledZone( Loop ).ActualZoneNum;

				{ auto const SELECT_CASE_var( TempControlType( ZoneNum ) );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
					TempZoneThermostatSetPoint( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
					ZoneThermostatSetPointLo( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
				} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
					// do nothing
				} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
					TempZoneThermostatSetPoint( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
					ZoneThermostatSetPointLo( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
				} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
					ZoneThermostatSetPointLo( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
				} else {
					// Do nothing
				}}
			}
			if ( TempControlledZone( Loop ).EMSOverrideCoolingSetPointOn ) {
				ZoneNum = TempControlledZone( Loop ).ActualZoneNum;

				{ auto const SELECT_CASE_var( TempControlType( ZoneNum ) );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
					// do nothing
				} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
					TempZoneThermostatSetPoint( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
					ZoneThermostatSetPointHi( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
				} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
					TempZoneThermostatSetPoint( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
					ZoneThermostatSetPointHi( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
				} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
					ZoneThermostatSetPointHi( ZoneNum ) = TempControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
				} else {
					// Do nothing
				}}
			}
		}
		for ( Loop = 1; Loop <= NumComfortControlledZones; ++Loop ) {
			if ( ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointOn ) {
				ZoneNum = ComfortControlledZone( Loop ).ActualZoneNum;
				{ auto const SELECT_CASE_var( ComfortControlType( ZoneNum ) );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SglHeatSetPointFanger ) {
					TempZoneThermostatSetPoint( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
					ZoneThermostatSetPointLo( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
				} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {
					// do nothing
				} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {
					TempZoneThermostatSetPoint( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
					ZoneThermostatSetPointLo( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
				} else if ( SELECT_CASE_var == DualSetPointFanger ) {
					ZoneThermostatSetPointLo( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointValue;
				} else {
					// Do nothing
				}}
			}
			if ( ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointOn ) {
				ZoneNum = ComfortControlledZone( Loop ).ActualZoneNum;
				{ auto const SELECT_CASE_var( ComfortControlType( ZoneNum ) );

				if ( SELECT_CASE_var == 0 ) { // Uncontrolled

				} else if ( SELECT_CASE_var == SglHeatSetPointFanger ) {
					// do nothing
				} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {
					TempZoneThermostatSetPoint( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
					ZoneThermostatSetPointHi( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
				} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {
					TempZoneThermostatSetPoint( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
					ZoneThermostatSetPointHi( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
				} else if ( SELECT_CASE_var == DualSetPointFanger ) {
					ZoneThermostatSetPointHi( ZoneNum ) = ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointValue;
				} else {
					// Do nothing
				}}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "InitZoneAirSetpoints - program terminates due to previous condition." );
		}

		if ( ZoneEquipInputsFilled ) {
			ControlledZonesChecked = true;
		}

	}

	void
	PredictSystemLoads(
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  July 2003 (Peter Graham Ellis)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is responsible for determining
		// how much of each type of energy every zone requires.
		// In effect, this subroutine defines and simulates all
		// the system types and in the case of hybrid systems
		// which use more than one type of energy must determine
		// how to apportion the load. An example of a hybrid system
		// is a water loop heat pump with supplemental air.  In
		// this case, a zone will require water from the loop and
		// cooled or heated air from the air system. A simpler
		// example would be a VAV system with baseboard heaters.

		//  Basic Air System Types
		//  1) Constant Volume Single Duct
		//  2) Variable Volume Single Duct
		//  3) Constant Volume Dual Duct
		//  4) Variable Volume Dual Duct

		// METHODOLOGY EMPLOYED:
		// 0.  Determine if simulation has downstepped and readjust history and revert node results
		// 1.  Determine zone load - this is zone temperature dependent
		// 2.  Determine balance point - the temperature at which the
		//     zone load is balanced by the system output. The way the
		//     balance point is determined will be different depending on
		//     the type of system being simulated.
		// 3.  Calculate zone energy requirements

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataRoomAirModel::AirModel;
		using DataRoomAirModel::RoomAirModel_Mixing;
		using DataRoomAirModel::IsZoneDV;
		using DataRoomAirModel::ZoneDVMixedFlag;
		using DataRoomAirModel::IsZoneUI;
		using DataRoomAirModel::ZTFloor;
		using DataRoomAirModel::MATFloor;
		using DataRoomAirModel::XMATFloor;
		using DataRoomAirModel::XM2TFloor;
		using DataRoomAirModel::XM3TFloor;
		using DataRoomAirModel::XM4TFloor;
		using DataRoomAirModel::ZTOC;
		using DataRoomAirModel::ZTM1OC;
		using DataRoomAirModel::MATOC;
		using DataRoomAirModel::XMATOC;
		using DataRoomAirModel::XM2TOC;
		using DataRoomAirModel::XM3TOC;
		using DataRoomAirModel::XM4TOC;
		using DataRoomAirModel::ZTMX;
		using DataRoomAirModel::ZTM1MX;
		using DataRoomAirModel::MATMX;
		using DataRoomAirModel::XMATMX;
		using DataRoomAirModel::XM2TMX;
		using DataRoomAirModel::XM3TMX;
		using DataRoomAirModel::XM4TMX;
		using RoomAirModelAirflowNetwork::LoadPredictionRoomAirModelAirflowNetwork;
		using General::TrimSigDigits;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SumIntGain; // Zone sum of convective internal gains
		Real64 SumHA; // Zone sum of Hc*Area
		Real64 SumHATsurf; // Zone sum of Hc*Area*Tsurf
		Real64 SumHATref; // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
		Real64 SumMCp; // Zone sum of MassFlowRate*Cp
		Real64 SumMCpT; // Zone sum of MassFlowRate*Cp*T
		Real64 SumSysMCp; // Zone sum of air system MassFlowRate*Cp
		Real64 SumSysMCpT; // Zone sum of air system MassFlowRate*Cp*T
		Real64 TempDepCoef; // Formerly CoefSumha
		Real64 TempIndCoef; // Formerly CoefSumhat
		Real64 AirCap; // Formerly CoefAirrat
		//unused1208  REAL(r64)      :: TimeStepSeconds
		Real64 TempHistoryTerm;
		int ZoneNum;
		Real64 ZoneT; // Zone temperature at previous time step
		int RelativeZoneNum;
		int ActualZoneNum;
		int I;
		int Itemp;
		Real64 SetpointOffset;
		int RoomAirNode;
		int LoopNode;
		Real64 RAFNFrac;

		// Staged thermostat setpoint
		if ( NumStageCtrZone > 0 ) {
			for ( RelativeZoneNum = 1; RelativeZoneNum <= NumStageCtrZone; ++RelativeZoneNum ) {
				ActualZoneNum = StageControlledZone( RelativeZoneNum ).ActualZoneNum;
				ZoneT = MAT( ActualZoneNum );
				if ( ShortenTimeStepSys ) ZoneT = XMPT( ActualZoneNum );
				StageControlledZone( RelativeZoneNum ).HeatSetPoint = GetCurrentScheduleValue( StageControlledZone( RelativeZoneNum ).HSBchedIndex );
				StageControlledZone( RelativeZoneNum ).CoolSetPoint = GetCurrentScheduleValue( StageControlledZone( RelativeZoneNum ).CSBchedIndex );
				if ( StageControlledZone( RelativeZoneNum ).HeatSetPoint >= StageControlledZone( RelativeZoneNum ).CoolSetPoint ) {
					++StageControlledZone( RelativeZoneNum ).StageErrCount;
					if ( StageControlledZone( RelativeZoneNum ).StageErrCount < 2 ) {
						ShowWarningError( "ZoneControl:Thermostat:StagedDualSetpoint: The heating setpoint is equal to or above the cooling setpoint in " + StageControlledZone( RelativeZoneNum ).Name );
						ShowContinueError( "The zone heating setpoint is set to the cooling setpoint - 0.1C." );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The heating setpoint is still above the cooling setpoint", StageControlledZone( RelativeZoneNum ).StageErrIndex, StageControlledZone( RelativeZoneNum ).HeatSetPoint, StageControlledZone( RelativeZoneNum ).HeatSetPoint );
					}
					StageControlledZone( RelativeZoneNum ).HeatSetPoint = StageControlledZone( RelativeZoneNum ).CoolSetPoint - 0.1; //???????????
				}
				// Determine either cooling or heating
				if ( StageControlledZone( RelativeZoneNum ).CoolSetPoint < ZoneT ) { // Cooling
					SetpointOffset = ZoneT - StageControlledZone( RelativeZoneNum ).CoolSetPoint;
					Itemp = 0;
					for ( I = 1; I <= StageControlledZone( RelativeZoneNum ).NumOfCoolStages; ++I ) {
						if ( SetpointOffset >= StageControlledZone( RelativeZoneNum ).CoolTOffset( I ) ) {
							Itemp = -I;
						}
					}
					ZoneSysEnergyDemand( ActualZoneNum ).StageNum = Itemp;
					if ( SetpointOffset >= 0.5 * StageControlledZone( RelativeZoneNum ).CoolThroRange ) {
						ZoneThermostatSetPointHi( ActualZoneNum ) = StageControlledZone( RelativeZoneNum ).CoolSetPoint - 0.5 * StageControlledZone( RelativeZoneNum ).CoolThroRange;
					} else {
						ZoneThermostatSetPointHi( ActualZoneNum ) = StageControlledZone( RelativeZoneNum ).CoolSetPoint + 0.5 * StageControlledZone( RelativeZoneNum ).CoolThroRange;
					}
					ZoneThermostatSetPointLo( ActualZoneNum ) = ZoneThermostatSetPointHi( ActualZoneNum );
				} else if ( StageControlledZone( RelativeZoneNum ).HeatSetPoint > ZoneT ) { // heating
					SetpointOffset = ZoneT - StageControlledZone( RelativeZoneNum ).HeatSetPoint;
					Itemp = 0;
					for ( I = 1; I <= StageControlledZone( RelativeZoneNum ).NumOfHeatStages; ++I ) {
						if ( std::abs( SetpointOffset ) >= std::abs( StageControlledZone( RelativeZoneNum ).HeatTOffset( I ) ) ) {
							Itemp = I;
						}
					}
					ZoneSysEnergyDemand( ActualZoneNum ).StageNum = Itemp;
					if ( std::abs( SetpointOffset ) >= 0.5 * StageControlledZone( RelativeZoneNum ).CoolThroRange ) {
						ZoneThermostatSetPointLo( ActualZoneNum ) = StageControlledZone( RelativeZoneNum ).HeatSetPoint + 0.5 * StageControlledZone( RelativeZoneNum ).HeatThroRange;
					} else {
						ZoneThermostatSetPointLo( ActualZoneNum ) = StageControlledZone( RelativeZoneNum ).HeatSetPoint - 0.5 * StageControlledZone( RelativeZoneNum ).HeatThroRange;
					}
					ZoneThermostatSetPointHi( ActualZoneNum ) = ZoneThermostatSetPointLo( ActualZoneNum );
				} else {
					ZoneThermostatSetPointHi( ActualZoneNum ) = StageControlledZone( RelativeZoneNum ).CoolSetPoint + 0.5 * StageControlledZone( RelativeZoneNum ).CoolThroRange;
					ZoneThermostatSetPointLo( ActualZoneNum ) = StageControlledZone( RelativeZoneNum ).HeatSetPoint - 0.5 * StageControlledZone( RelativeZoneNum ).HeatThroRange;
					ZoneSysEnergyDemand( ActualZoneNum ).StageNum = 0;
				}
			}
		}

		//Tuned Precompute controlled equip zone numbers for performance
		std::vector< int > controlledZoneEquipConfigNums;
		for ( int ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum ) {
			if ( Zone( ZoneEquipConfigNum ).IsControlled ) {
				controlledZoneEquipConfigNums.push_back( ZoneEquipConfigNum );
			}
		}

		// Update zone temperatures
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			if ( ShortenTimeStepSys ) {
				// timestep has just shifted from full zone timestep to a new shorter system timestep
				//throw away last updates in corrector and rewind for resimulating smaller timestep
				if ( Zone( ZoneNum ).SystemZoneNodeNumber > 0 ) { // roll back result for zone air node,
					Node( Zone( ZoneNum ).SystemZoneNodeNumber ).Temp = XMAT( ZoneNum );
					TempTstatAir( ZoneNum ) = XMAT( ZoneNum );
					Node( Zone( ZoneNum ).SystemZoneNodeNumber ).HumRat = WZoneTimeMinus1( ZoneNum );
					Node( Zone( ZoneNum ).SystemZoneNodeNumber ).Enthalpy = PsyHFnTdbW( XMAT( ZoneNum ), WZoneTimeMinus1( ZoneNum ) );
				}

				if ( NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep ) { // cannot reuse existing DS data, interpolate from zone time

					//  MAT(ZoneNum),   XMAT(ZoneNum),   XM2T(ZoneNum),   XM3T(ZoneNum),   XM4T(ZoneNum), &
					DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, XMAT( ZoneNum ), XM2T( ZoneNum ), XM3T( ZoneNum ), XM4T( ZoneNum ), XM4T( ZoneNum ), MAT( ZoneNum ), DSXMAT( ZoneNum ), DSXM2T( ZoneNum ), DSXM3T( ZoneNum ), DSXM4T( ZoneNum ) );
					//     ZoneAirHumRat(ZoneNum),   WZoneTimeMinus1(ZoneNum),   WZoneTimeMinus2(ZoneNum),   &
					//                                 WZoneTimeMinus3(ZoneNum),   WZoneTimeMinus4(ZoneNum), &
					DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, WZoneTimeMinus1( ZoneNum ), WZoneTimeMinus2( ZoneNum ), WZoneTimeMinus3( ZoneNum ), WZoneTimeMinus4( ZoneNum ), WZoneTimeMinus4( ZoneNum ), ZoneAirHumRat( ZoneNum ), DSWZoneTimeMinus1( ZoneNum ), DSWZoneTimeMinus2( ZoneNum ), DSWZoneTimeMinus3( ZoneNum ), DSWZoneTimeMinus4( ZoneNum ) );

					if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {

						//   MATFloor(ZoneNum),   XMATFloor(ZoneNum),    XM2TFloor(ZoneNum),  &
						//                        XM3TFloor(ZoneNum),    XM4TFloor(ZoneNum) ,   &
						DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, XMATFloor( ZoneNum ), XM2TFloor( ZoneNum ), XM3TFloor( ZoneNum ), XM4TFloor( ZoneNum ), XM4TFloor( ZoneNum ), MATFloor( ZoneNum ), DSXMATFloor( ZoneNum ), DSXM2TFloor( ZoneNum ), DSXM3TFloor( ZoneNum ), DSXM4TFloor( ZoneNum ) );
						//      MATOC(ZoneNum),   XMATOC(ZoneNum),    XM2TOC(ZoneNum),  &
						//                        XM3TOC(ZoneNum),    XM4TOC(ZoneNum) ,   &
						DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, XMATOC( ZoneNum ), XM2TOC( ZoneNum ), XM3TOC( ZoneNum ), XM4TOC( ZoneNum ), XM4TOC( ZoneNum ), MATOC( ZoneNum ), DSXMATOC( ZoneNum ), DSXM2TOC( ZoneNum ), DSXM3TOC( ZoneNum ), DSXM4TOC( ZoneNum ) );
						//  MATMX(ZoneNum),   XMATMX(ZoneNum),    XM2TMX(ZoneNum),  &
						//                    XM3TMX(ZoneNum),    XM4TMX(ZoneNum) ,   &
						DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, XMATMX( ZoneNum ), XM2TMX( ZoneNum ), XM3TMX( ZoneNum ), XM4TMX( ZoneNum ), XM4TMX( ZoneNum ), MATMX( ZoneNum ), DSXMATMX( ZoneNum ), DSXM2TMX( ZoneNum ), DSXM3TMX( ZoneNum ), DSXM4TMX( ZoneNum ) );
					}
					if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
						for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
							auto & ThisRAFNNode( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ) );
							DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, ThisRAFNNode.AirTemp, ThisRAFNNode.AirTempX1, ThisRAFNNode.AirTempX2, ThisRAFNNode.AirTempX3, ThisRAFNNode.AirTempX4,
								ThisRAFNNode.AirTemp, ThisRAFNNode.AirTempDSX1, ThisRAFNNode.AirTempDSX2, ThisRAFNNode.AirTempDSX3, ThisRAFNNode.AirTempDSX4 );
							DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, ThisRAFNNode.HumRat, ThisRAFNNode.HumRatX1, ThisRAFNNode.HumRatX2, ThisRAFNNode.HumRatX3, ThisRAFNNode.HumRatX4,
								ThisRAFNNode.HumRat, ThisRAFNNode.HumRatDSX1, ThisRAFNNode.HumRatDSX2, ThisRAFNNode.HumRatDSX3, ThisRAFNNode.HumRatDSX4 );
						}
					}
				} else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
					// do nothing because DS history would have been pushed prior and should be ready

				}

			}
			// now update the variables actually used in the balance equations.
			if ( UseZoneTimeStepHistory ) {
				ZTM1( ZoneNum ) = XMAT( ZoneNum );
				ZTM2( ZoneNum ) = XM2T( ZoneNum );
				ZTM3( ZoneNum ) = XM3T( ZoneNum );

				WZoneTimeMinus1Temp( ZoneNum ) = WZoneTimeMinus1( ZoneNum );
				WZoneTimeMinus2Temp( ZoneNum ) = WZoneTimeMinus2( ZoneNum );
				WZoneTimeMinus3Temp( ZoneNum ) = WZoneTimeMinus3( ZoneNum );

			} else { // use down-stepped history
				ZTM1( ZoneNum ) = DSXMAT( ZoneNum );
				ZTM2( ZoneNum ) = DSXM2T( ZoneNum );
				ZTM3( ZoneNum ) = DSXM3T( ZoneNum );

				WZoneTimeMinus1Temp( ZoneNum ) = DSWZoneTimeMinus1( ZoneNum );
				WZoneTimeMinus2Temp( ZoneNum ) = DSWZoneTimeMinus2( ZoneNum );
				WZoneTimeMinus3Temp( ZoneNum ) = DSWZoneTimeMinus3( ZoneNum );

			}

			AIRRAT( ZoneNum ) = Zone( ZoneNum ).Volume * ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ) ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MAT( ZoneNum ) ) / ( TimeStepSys * SecInHour );
			AirCap = AIRRAT( ZoneNum );
			RAFNFrac = 0.0;

			// Calculate the various heat balance sums

			// NOTE: SumSysMCp and SumSysMCpT are not used in the predict step
			CalcZoneSums( ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, controlledZoneEquipConfigNums );

			TempDepCoef = SumHA + SumMCp;
			TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SysDepZoneLoadsLagged( ZoneNum );
			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_Mixing ) {
				TempHistoryTerm = AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) );
				TempDepZnLd( ZoneNum ) = ( 11.0 / 6.0 ) * AirCap + TempDepCoef;
				TempIndZnLd( ZoneNum ) = TempHistoryTerm + TempIndCoef;
			} else if ( IsZoneDV( ZoneNum ) ) {
				// UCSD displacement ventilation model - make dynamic term independent of TimeStepSys
				TempHistoryTerm = AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) );
				TempDepZnLd( ZoneNum ) = ( 11.0 / 6.0 ) * AirCap + TempDepCoef;
				TempIndZnLd( ZoneNum ) = TempHistoryTerm + TempIndCoef;
			} else if ( IsZoneUI( ZoneNum ) ) {
				// UCSD UFAD model - make dynamic term independent of TimeStepSys
				TempHistoryTerm = AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) );
				TempDepZnLd( ZoneNum ) = ( 11.0 / 6.0 ) * AirCap + TempDepCoef;
				TempIndZnLd( ZoneNum ) = TempHistoryTerm + TempIndCoef;
			} else if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
				// RoomAirflowNetworkModel - make dynamic term independent of TimeStepSys
				if ( RoomAirflowNetworkZoneInfo( ZoneNum ).IsUsed ) {
					RoomAirNode = RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID;
					LoadPredictionRoomAirModelAirflowNetwork( ZoneNum, RoomAirNode );
					TempDepCoef = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHA + RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMCp;
					TempIndCoef = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumIntSensibleGain
						+ RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHATsurf
						- RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHATref
						+ RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMCpT + RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SysDepZoneLoadsLagged;
					AirCap = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).AirVolume * ZoneVolCapMultpSens
						* RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).RhoAir
						* RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).CpAir
						/ ( TimeStepSys*SecInHour );
					AIRRAT( ZoneNum ) = AirCap;
					TempHistoryTerm = AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) );
					TempDepZnLd( ZoneNum ) = ( 11.0 / 6.0 ) * AirCap + TempDepCoef;
					TempIndZnLd( ZoneNum ) = TempHistoryTerm + TempIndCoef;
					if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).HasHVACAssigned ) RAFNFrac = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).HVAC( 1 ).SupplyFraction;
				}
			} else { // other imperfectly mixed room models
				TempHistoryTerm = AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) );
				TempDepZnLd( ZoneNum ) = ( 11.0 / 6.0 ) * AirCap + TempDepCoef;
				TempIndZnLd( ZoneNum ) = TempHistoryTerm + TempIndCoef;
			}

			// Exact solution or Euler method
			ShortenTimeStepSysRoomAir = false;
			if ( ZoneAirSolutionAlgo != Use3rdOrder ) {
				if ( ShortenTimeStepSys && TimeStepSys < TimeStepZone ) {
					if ( PreviousTimeStep < TimeStepZone ) {
						ZoneT1( ZoneNum ) = ZoneTM2( ZoneNum );
						ZoneW1( ZoneNum ) = ZoneWM2( ZoneNum );
						if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
							for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTM2;
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWM2;
							}
						}
					} else {
						ZoneT1( ZoneNum ) = ZoneTMX( ZoneNum );
						ZoneW1( ZoneNum ) = ZoneWMX( ZoneNum );
						if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
							for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX;
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX;
					}
						}
					}
					ShortenTimeStepSysRoomAir = true;
				} else {
					ZoneT1( ZoneNum ) = ZT( ZoneNum );
					ZoneW1( ZoneNum ) = ZoneAirHumRat( ZoneNum );
					if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
						for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTemp;
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRat;
				}
					}
				}
				TempDepZnLd( ZoneNum ) = TempDepCoef;
				TempIndZnLd( ZoneNum ) = TempIndCoef;
			}

			// Calculate the predicted zone load to be provided by the system with the given desired zone air temperature
			CalcPredictedSystemLoad( ZoneNum, RAFNFrac );

			// Calculate the predicted zone load to be provided by the system with the given desired humidity ratio
			CalcPredictedHumidityRatio( ZoneNum, RAFNFrac );

		}

	}

	void
	CalcZoneAirTempSetPoints()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       Aug 2013, Xiufeng Pang (XP) - Added code for updating set points during
		//                      optimum start period
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine sets what the setpoints for each controlled zone should be based on schedules.
		// This is called each time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleValuesForDay;
		using General::TrimSigDigits;
		using DataZoneControls::OccRoomTSetPointHeat;
		using DataZoneControls::OccRoomTSetPointCool;
		using InputProcessor::SameString;

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
		int RelativeZoneNum;
		int ActualZoneNum;
		int TempControlSchedIndex;
		int SetPointTempSchedIndex;
		int SetPointTempSchedIndexHot;
		int SetPointTempSchedIndexCold;
		int SchedNameIndex;
		int SchedTypeIndex;
		Array2D< Real64 > DaySPValues; // Day room temp setpoint values - for optimum start
		int OccStartTime; // Occupancy start time - for optimum start

		// FLOW:
		TempControlType = 0; // Default

		// Place holder for occupied heating and cooling set points - for optimum start
		if ( ! allocated( OccRoomTSetPointHeat ) ) {
			OccRoomTSetPointHeat.allocate( NumOfZones );
		}
		if ( ! allocated( OccRoomTSetPointCool ) ) {
			OccRoomTSetPointCool.allocate( NumOfZones );
		}
		OccRoomTSetPointHeat = 0.0;
		OccRoomTSetPointCool = 100.0;

		for ( RelativeZoneNum = 1; RelativeZoneNum <= NumTempControlledZones; ++RelativeZoneNum ) {

			// What if this zone not controlled???
			ActualZoneNum = TempControlledZone( RelativeZoneNum ).ActualZoneNum;
			TempControlSchedIndex = TempControlledZone( RelativeZoneNum ).CTSchedIndex;
			TempControlType( ActualZoneNum ) = GetCurrentScheduleValue( TempControlSchedIndex );

			// Error detection for these values is done in the Get routine

			{ auto const SELECT_CASE_var( TempControlType( ActualZoneNum ) ); // Is this missing the possibility of sometimes having no control on a zone
			// during the simulation?
			if ( SELECT_CASE_var == 0 ) { // Uncontrolled

			} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {

				SchedNameIndex = TempControlledZone( RelativeZoneNum ).SchIndx_SingleHeatSetPoint;

				SchedTypeIndex = TempControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );

				SetPointTempSchedIndex = SetPointSingleHeating( SchedTypeIndex ).TempSchedIndex;
				TempZoneThermostatSetPoint( ActualZoneNum ) = GetCurrentScheduleValue( SetPointTempSchedIndex );
				AdjustAirSetPointsforOpTempCntrl( RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint( ActualZoneNum ) );

				ZoneThermostatSetPointLo( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				//        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

			} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {

				SchedNameIndex = TempControlledZone( RelativeZoneNum ).SchIndx_SingleCoolSetPoint;

				SchedTypeIndex = TempControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );

				SetPointTempSchedIndex = SetPointSingleCooling( SchedTypeIndex ).TempSchedIndex;
				TempZoneThermostatSetPoint( ActualZoneNum ) = GetCurrentScheduleValue( SetPointTempSchedIndex );
				AdjustAirSetPointsforOpTempCntrl( RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint( ActualZoneNum ) );
				ZoneThermostatSetPointHi( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				//        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

				AdjustCoolingSetPointforTempAndHumidityControl( RelativeZoneNum, ActualZoneNum );

			} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {

				SchedNameIndex = TempControlledZone( RelativeZoneNum ).SchIndx_SingleHeatCoolSetPoint;

				SchedTypeIndex = TempControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );

				SetPointTempSchedIndex = SetPointSingleHeatCool( SchedTypeIndex ).TempSchedIndex;
				TempZoneThermostatSetPoint( ActualZoneNum ) = GetCurrentScheduleValue( SetPointTempSchedIndex );
				AdjustAirSetPointsforOpTempCntrl( RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint( ActualZoneNum ) );

				ZoneThermostatSetPointHi( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				ZoneThermostatSetPointLo( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );

				//Change the room set point to occupied set point during optimum start period--------------

				if ( allocated( OptStartData.OptStartFlag ) ) {
					if ( ! allocated( DaySPValues ) ) {
						DaySPValues.allocate( NumOfTimeStepInHour, 24 );
					}
					if ( OptStartData.ActualZoneNum( ActualZoneNum ) == ActualZoneNum ) {
						GetScheduleValuesForDay( SetPointTempSchedIndexCold, DaySPValues );
						OccStartTime = CEILING( OptStartData.OccStartTime( ActualZoneNum ) ) + 1;
						TempZoneThermostatSetPoint( ActualZoneNum ) = DaySPValues( 1, OccStartTime );
					}

					if ( OptStartData.OptStartFlag( ActualZoneNum ) ) {
						ZoneThermostatSetPointHi( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
						ZoneThermostatSetPointLo( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
					}
				}
				//--------------------------------------------------------------------------------------------

			} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {

				SchedNameIndex = TempControlledZone( RelativeZoneNum ).SchIndx_DualSetPointWDeadBand;

				SchedTypeIndex = TempControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );

				SetPointTempSchedIndexHot = SetPointDualHeatCool( SchedTypeIndex ).HeatTempSchedIndex;
				SetPointTempSchedIndexCold = SetPointDualHeatCool( SchedTypeIndex ).CoolTempSchedIndex;
				ZoneThermostatSetPointHi( ActualZoneNum ) = GetCurrentScheduleValue( SetPointTempSchedIndexCold );
				AdjustAirSetPointsforOpTempCntrl( RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointHi( ActualZoneNum ) );

				ZoneThermostatSetPointLo( ActualZoneNum ) = GetCurrentScheduleValue( SetPointTempSchedIndexHot );
				AdjustAirSetPointsforOpTempCntrl( RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointLo( ActualZoneNum ) );

				//Change the room set point to occupied set point during optimum start period--------------

				if ( allocated( OptStartData.OptStartFlag ) ) {
					if ( ! allocated( DaySPValues ) ) {
						DaySPValues.allocate( NumOfTimeStepInHour, 24 );
					}
					if ( OptStartData.ActualZoneNum( ActualZoneNum ) == ActualZoneNum ) {
						GetScheduleValuesForDay( SetPointTempSchedIndexCold, DaySPValues );
						OccStartTime = CEILING( OptStartData.OccStartTime( ActualZoneNum ) ) + 1;
						OccRoomTSetPointCool( ActualZoneNum ) = DaySPValues( 1, OccStartTime );
						GetScheduleValuesForDay( SetPointTempSchedIndexHot, DaySPValues );
						OccRoomTSetPointHeat( ActualZoneNum ) = DaySPValues( 1, OccStartTime );
					}

					if ( OptStartData.OptStartFlag( ActualZoneNum ) ) {
						ZoneThermostatSetPointHi( ActualZoneNum ) = OccRoomTSetPointCool( ActualZoneNum );
						ZoneThermostatSetPointLo( ActualZoneNum ) = OccRoomTSetPointHeat( ActualZoneNum );
					}
				}
				//--------------------------------------------------------------------------------------------

				AdjustCoolingSetPointforTempAndHumidityControl( RelativeZoneNum, ActualZoneNum );

			} else {
				ShowSevereError( "CalcZoneAirTempSetpoints: Illegal control type for Zone=" + Zone( ActualZoneNum ).Name + ", Found value=" + TrimSigDigits( TempControlType( ActualZoneNum ) ) + ", in Schedule=" + TempControlledZone( RelativeZoneNum ).ControlTypeSchedName );

			}}

			//Apply offset for faulty therostats_Feb. 2015, zrp
			if ( ( NumFaultyThermostat > 0 ) && ( ! WarmupFlag ) && ( ! DoingSizing ) && DoWeathSim ) {

				//  loop through the FaultsThermostatOffset objects to find the one for the zone
				for ( int iFault = 1; iFault <= NumFaultyThermostat; ++iFault ) {

					if ( SameString( TempControlledZone( RelativeZoneNum ).Name, FaultsThermostatOffset( iFault ).FaultyThermostatName ) ) {

						// Check fault availability schedules
						if ( GetCurrentScheduleValue( FaultsThermostatOffset( iFault ).AvaiSchedPtr ) > 0.0 ) {

							// Check fault severity schedules to update the reference thermostat offset
							double rSchVal = 1.0;
							double offsetUpdated;
							if ( FaultsThermostatOffset( iFault ).SeveritySchedPtr >= 0 ) {
								rSchVal = GetCurrentScheduleValue( FaultsThermostatOffset( iFault ).SeveritySchedPtr );
							}
							offsetUpdated = rSchVal * FaultsThermostatOffset( iFault ).Offset;

							// Positive offset means the sensor reading is higher than the actual value
							TempZoneThermostatSetPoint( ActualZoneNum ) -= offsetUpdated;
							ZoneThermostatSetPointLo( ActualZoneNum ) -= offsetUpdated;
							ZoneThermostatSetPointHi( ActualZoneNum ) -= offsetUpdated;
						}

						// Stop searching the FaultsThermostatOffset object for the zone
						break;
					}
				}
			}

		}

		if ( NumComfortControlledZones > 0 ) CalcZoneAirComfortSetPoints();

	}

	void
	CalcPredictedSystemLoad( int const ZoneNum, Real64 RAFNFrac )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the predicted system load for a time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataLoopNode::Node;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LoadToHeatingSetPoint;
		Real64 LoadToCoolingSetPoint;
		Real64 ZoneSetPoint;

		// FLOW:
		DeadBandOrSetback( ZoneNum ) = false;
		ZoneSetPoint = 0.0;
		LoadToHeatingSetPoint = 0.0;
		LoadToCoolingSetPoint = 0.0;

		{ auto const SELECT_CASE_var( TempControlType( ZoneNum ) );

		if ( SELECT_CASE_var == 0 ) {
			// Uncontrolled Zone
			LoadToHeatingSetPoint = 0.0;
			LoadToCoolingSetPoint = 0.0;
			ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = 0.0;

		} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
			// Determine zone load based on
			// Qload + Qsys = 0 and Qsys = mCp(Tsys-Tzone)
			// System Load Sign Convention:
			//     - -> Cooling required to reach setpoint
			//     + -> Heating required to reach setpoint

			//PH 3/2/04      LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
			if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
				LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * TempZoneThermostatSetPoint( ZoneNum ) - TempIndZnLd( ZoneNum ) );
				// Exact solution
			} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
				if ( TempDepZnLd( ZoneNum ) == 0.0 ) { // B=0
					LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
				} else {
					Real64 const exp_700_TA( std::exp( min( 700.0, - TempDepZnLd( ZoneNum ) / AIRRAT( ZoneNum ) ) ) );
					LoadToHeatingSetPoint = TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
				}
			} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
				LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum );
			}
			if ( RAFNFrac > 0.0 ) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
			ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToHeatingSetPoint;
			ZoneSetPoint = TempZoneThermostatSetPoint( ZoneNum );
			LoadToCoolingSetPoint = LoadToHeatingSetPoint;
			// for consistency with the other cases, use LE instead of LT and don't subtract 1.0 Watt as a way of pushing the zero load
			// case over the threshold
			if ( ( ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired ) <= 0.0 ) DeadBandOrSetback( ZoneNum ) = true;

		} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {

			//PH 3/2/04      LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
			if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
				LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
			} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
				if ( TempDepZnLd( ZoneNum ) == 0.0 ) { // B=0
					LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
				} else {
					Real64 const exp_700_TA( std::exp( min( 700.0, - TempDepZnLd( ZoneNum ) / AIRRAT( ZoneNum ) ) ) );
					LoadToCoolingSetPoint = TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
				}
			} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
				LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * TempZoneThermostatSetPoint( ZoneNum ) - TempIndZnLd( ZoneNum );
			}
			if ( RAFNFrac > 0.0 ) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
			ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToCoolingSetPoint;
			ZoneSetPoint = TempZoneThermostatSetPoint( ZoneNum );
			LoadToHeatingSetPoint = LoadToCoolingSetPoint;
			// for consistency with the other cases, use GE instead of GT and don't add 1.0 Watt as a way of pushing the zero load
			// case over the threshold
			if ( ( ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired ) >= 0.0 ) DeadBandOrSetback( ZoneNum ) = true;

		} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {

			//PH 3/2/04      LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
			//PH 3/2/04      !LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
			if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
				LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
				LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
				// Exact solution
			} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
				if ( TempDepZnLd( ZoneNum ) == 0.0 ) { // B=0
					LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
					LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
				} else {
					Real64 const exp_700_TA( std::exp( min( 700.0, - TempDepZnLd( ZoneNum ) / AIRRAT( ZoneNum ) ) ) );
					LoadToHeatingSetPoint = TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
					LoadToCoolingSetPoint = TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
				}
			} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
				LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * TempZoneThermostatSetPoint( ZoneNum ) - TempIndZnLd( ZoneNum );
				LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * TempZoneThermostatSetPoint( ZoneNum ) - TempIndZnLd( ZoneNum );
			}
			ZoneSetPoint = TempZoneThermostatSetPoint( ZoneNum );
			if ( RAFNFrac > 0.0 ) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
			if ( RAFNFrac > 0.0 ) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

			//PH 3/2/04      ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint ! = LoadToCoolingSetPoint
			// Note that LoadToHeatingSetPoint is generally not equal to LoadToCoolingSetPoint
			// when the heating and cooling set-points are equal if the zone is unmixed,
			// e.g. displacement ventilation or UFAD, since the stratification is generally not the same in heating and cooling modes

			// Possible combinations:
			// 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
			// 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
			//                                                                 as a poor choice of set-points
			// 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
			// 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation ! includes zero load cases
			// First trap bad set-points
			if ( LoadToHeatingSetPoint > LoadToCoolingSetPoint ) {
				ShowSevereError( "SingleHeatCoolSetPoint: Effective heating set-point higher than effective cooling set-point - use DualSetPointWithDeadBand if using unmixed air model" );
				ShowContinueErrorTimeStamp( "occurs in Zone=" + Zone( ZoneNum ).Name );
				ShowContinueError( "LoadToHeatingSetPoint=" + RoundSigDigits( LoadToHeatingSetPoint, 3 ) + ", LoadToCoolingSetPoint=" + RoundSigDigits( LoadToCoolingSetPoint, 3 ) );
				ShowContinueError( "Zone TempDepZnLd=" + RoundSigDigits( TempDepZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone TempIndZnLd=" + RoundSigDigits( TempIndZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone ThermostatSetPoint=" + RoundSigDigits( TempZoneThermostatSetPoint( ZoneNum ), 2 ) );
				ShowFatalError( "Program terminates due to above conditions." );
			}

			if ( LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0 ) {
				ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToHeatingSetPoint;
			} else if ( LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0 ) {
				ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToCoolingSetPoint;
			} else if ( LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0 ) { // deadband includes zero loads
				ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = 0.0;
				if ( Zone( ZoneNum ).SystemZoneNodeNumber > 0 ) {
					ZoneSetPoint = Node( Zone( ZoneNum ).SystemZoneNodeNumber ).Temp;
					ZoneSetPoint = max( ZoneSetPoint, ZoneThermostatSetPointLo( ZoneNum ) ); // trap out of deadband
					ZoneSetPoint = min( ZoneSetPoint, ZoneThermostatSetPointHi( ZoneNum ) ); // trap out of deadband
				}
				DeadBandOrSetback( ZoneNum ) = true;
			} else { // this should never occur!
				ShowSevereError( "SingleHeatCoolSetPoint: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team" );
				ShowContinueErrorTimeStamp( "occurs in Zone=" + Zone( ZoneNum ).Name );
				ShowContinueError( "LoadToHeatingSetPoint=" + RoundSigDigits( LoadToHeatingSetPoint, 3 ) + ", LoadToCoolingSetPoint=" + RoundSigDigits( LoadToCoolingSetPoint, 3 ) );
				ShowContinueError( "Zone TempDepZnLd=" + RoundSigDigits( TempDepZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone TempIndZnLd=" + RoundSigDigits( TempIndZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone ThermostatSetPoint=" + RoundSigDigits( TempZoneThermostatSetPoint( ZoneNum ), 2 ) );
				ShowFatalError( "Program terminates due to above conditions." );
			}

		} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {

			if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
				LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
				LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
				// Exact solution
			} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
				if ( TempDepZnLd( ZoneNum ) == 0.0 ) { // B=0
					LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
					LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
				} else {
					Real64 const exp_700_TA( std::exp( min( 700.0, - TempDepZnLd( ZoneNum ) / AIRRAT( ZoneNum ) ) ) );
					LoadToHeatingSetPoint = TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
					LoadToCoolingSetPoint = TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
				}
			} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
				LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * ZoneThermostatSetPointLo( ZoneNum ) - TempIndZnLd( ZoneNum );
				LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * ZoneThermostatSetPointHi( ZoneNum ) - TempIndZnLd( ZoneNum );
			}
			if ( RAFNFrac > 0.0 ) LoadToHeatingSetPoint = LoadToHeatingSetPoint / RAFNFrac;
			if ( RAFNFrac > 0.0 ) LoadToCoolingSetPoint = LoadToCoolingSetPoint / RAFNFrac;

			// Possible combinations:
			// 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
			// 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
			//                                                                  as a poor choice of set-points
			// 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
			// 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
			// First trap bad set-points
			if ( LoadToHeatingSetPoint > LoadToCoolingSetPoint ) {
				ShowSevereError( "DualSetPointWithDeadBand: Effective heating set-point higher than effective cooling set-point - increase deadband if using unmixed air model" );
				ShowContinueErrorTimeStamp( "occurs in Zone=" + Zone( ZoneNum ).Name );
				ShowContinueError( "LoadToHeatingSetPoint=" + RoundSigDigits( LoadToHeatingSetPoint, 3 ) + ", LoadToCoolingSetPoint=" + RoundSigDigits( LoadToCoolingSetPoint, 3 ) );
				ShowContinueError( "Zone TempDepZnLd=" + RoundSigDigits( TempDepZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone TempIndZnLd=" + RoundSigDigits( TempIndZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone Heating ThermostatSetPoint=" + RoundSigDigits( ZoneThermostatSetPointLo( ZoneNum ), 2 ) );
				ShowContinueError( "Zone Cooling ThermostatSetPoint=" + RoundSigDigits( ZoneThermostatSetPointHi( ZoneNum ), 2 ) );
				ShowFatalError( "Program terminates due to above conditions." );
			}
			if ( LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0 ) {
				ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToHeatingSetPoint;
				ZoneSetPoint = ZoneThermostatSetPointLo( ZoneNum );
			} else if ( LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0 ) {
				ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToCoolingSetPoint;
				ZoneSetPoint = ZoneThermostatSetPointHi( ZoneNum );
			} else if ( LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0 ) { // deadband includes zero loads
				// this turns out to cause instabilities sometimes? that lead to setpoint errors if predictor is off.
				ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = 0.0;
				if ( Zone( ZoneNum ).SystemZoneNodeNumber > 0 ) {
					ZoneSetPoint = Node( Zone( ZoneNum ).SystemZoneNodeNumber ).Temp;
					ZoneSetPoint = max( ZoneSetPoint, ZoneThermostatSetPointLo( ZoneNum ) ); // trap out of deadband
					ZoneSetPoint = min( ZoneSetPoint, ZoneThermostatSetPointHi( ZoneNum ) ); // trap out of deadband
				}
				DeadBandOrSetback( ZoneNum ) = true;
			} else { // this should never occur!
				ShowSevereError( "DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team" );
				ShowContinueErrorTimeStamp( "occurs in Zone=" + Zone( ZoneNum ).Name );
				ShowContinueError( "LoadToHeatingSetPoint=" + RoundSigDigits( LoadToHeatingSetPoint, 3 ) + ", LoadToCoolingSetPoint=" + RoundSigDigits( LoadToCoolingSetPoint, 3 ) );
				ShowContinueError( "Zone Heating Set-point=" + RoundSigDigits( ZoneThermostatSetPointLo( ZoneNum ), 2 ) );
				ShowContinueError( "Zone Cooling Set-point=" + RoundSigDigits( ZoneThermostatSetPointHi( ZoneNum ), 2 ) );
				ShowContinueError( "Zone TempDepZnLd=" + RoundSigDigits( TempDepZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone TempIndZnLd=" + RoundSigDigits( TempIndZnLd( ZoneNum ), 2 ) );
				ShowContinueError( "Zone ThermostatSetPoint=" + RoundSigDigits( TempZoneThermostatSetPoint( ZoneNum ), 2 ) );

				ShowFatalError( "Program terminates due to above conditions." );
			}

		}}

		// Staged control zone
		if ( NumStageCtrZone > 0 ) {
			if ( StageZoneLogic( ZoneNum ) ) {
				if ( ZoneSysEnergyDemand( ZoneNum ).StageNum == 0 ) { // No load
					LoadToHeatingSetPoint = 0.0;
					LoadToCoolingSetPoint = 0.0;
					ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = 0.0;
					if ( Zone( ZoneNum ).SystemZoneNodeNumber > 0 ) {
						ZoneSetPoint = Node( Zone( ZoneNum ).SystemZoneNodeNumber ).Temp;
						ZoneSetPoint = max( ZoneSetPoint, ZoneThermostatSetPointLo( ZoneNum ) ); // trap out of deadband
						ZoneSetPoint = min( ZoneSetPoint, ZoneThermostatSetPointHi( ZoneNum ) ); // trap out of deadband
					}
					DeadBandOrSetback( ZoneNum ) = true;
				} else if ( ZoneSysEnergyDemand( ZoneNum ).StageNum < 0 ) { // Cooling load
					if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
						LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
					} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
						if ( TempDepZnLd( ZoneNum ) == 0.0 ) { // B=0
							LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
						} else {
							Real64 const exp_700_TA( std::exp( min( 700.0, - TempDepZnLd( ZoneNum ) / AIRRAT( ZoneNum ) ) ) );
							LoadToCoolingSetPoint = TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
						}
					} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
						LoadToCoolingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointHi( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * ZoneThermostatSetPointHi( ZoneNum ) - TempIndZnLd( ZoneNum );
					}
					ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToCoolingSetPoint;
					ZoneSetPoint = ZoneThermostatSetPointHi( ZoneNum );
					LoadToHeatingSetPoint = LoadToCoolingSetPoint;
					if ( ( ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired ) >= 0.0 ) DeadBandOrSetback( ZoneNum ) = true;
				} else { // Heating load
					if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
						LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * ZoneThermostatSetPointLo( ZoneNum ) - TempIndZnLd( ZoneNum ) );
						// Exact solution
					} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
						if ( TempDepZnLd( ZoneNum ) == 0.0 ) { // B=0
							LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) - ZoneT1( ZoneNum ) ) - TempIndZnLd( ZoneNum );
						} else {
							Real64 const exp_700_TA( std::exp( min( 700.0, - TempDepZnLd( ZoneNum ) / AIRRAT( ZoneNum ) ) ) );
							LoadToHeatingSetPoint = TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) - ZoneT1( ZoneNum ) * exp_700_TA ) / ( 1.0 - exp_700_TA ) - TempIndZnLd( ZoneNum );
						}
					} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
						LoadToHeatingSetPoint = AIRRAT( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) - ZoneT1( ZoneNum ) ) + TempDepZnLd( ZoneNum ) * ( ZoneThermostatSetPointLo( ZoneNum ) ) - TempIndZnLd( ZoneNum );
					}
					ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired = LoadToHeatingSetPoint;
					ZoneSetPoint = ZoneThermostatSetPointLo( ZoneNum );
					LoadToCoolingSetPoint = LoadToHeatingSetPoint;
					if ( ( ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired ) <= 0.0 ) DeadBandOrSetback( ZoneNum ) = true;
				}
			}
		}

		//If the ZoneNodeNum has been set for a Controlled Zone, then the zone setpoint is placed on the node.
		if ( Zone( ZoneNum ).SystemZoneNodeNumber > 0 ) {
			Node( Zone( ZoneNum ).SystemZoneNodeNumber ).TempSetPoint = ZoneSetPoint;
		}

		if ( ZoneSetPoint > ZoneSetPointLast( ZoneNum ) ) {
			Setback( ZoneNum ) = true;
		} else {
			Setback( ZoneNum ) = false;
		}

		ZoneSetPointLast( ZoneNum ) = ZoneSetPoint;
		TempZoneThermostatSetPoint( ZoneNum ) = ZoneSetPoint; // needed to fix Issue # 5048

		// Save the unmultiplied zone load to a report variable
		SNLoadPredictedRate( ZoneNum ) = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired * LoadCorrectionFactor( ZoneNum );
		SNLoadPredictedHSPRate( ZoneNum ) = LoadToHeatingSetPoint * LoadCorrectionFactor( ZoneNum );
		SNLoadPredictedCSPRate( ZoneNum ) = LoadToCoolingSetPoint * LoadCorrectionFactor( ZoneNum );
		CurDeadBandOrSetback( ZoneNum ) = DeadBandOrSetback( ZoneNum );

		// Apply the Zone Multiplier and Load Correction factor to the total zone load
		ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired *= Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier * LoadCorrectionFactor( ZoneNum );
		ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToHeatingSP = LoadToHeatingSetPoint * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier * LoadCorrectionFactor( ZoneNum );
		ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToCoolingSP = LoadToCoolingSetPoint * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier * LoadCorrectionFactor( ZoneNum );

		//init each sequenced demand to the full output
		if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired; // array assignment
		if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToHeatingSP; // array assignment
		if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToCoolingSP; // array assignment

	}

	void
	CalcPredictedHumidityRatio( int const ZoneNum, Real64 RAFNFrac )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   May 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does the prediction step for humidity control

		// METHODOLOGY EMPLOYED:
		// This solves for the required system moisture required to try and achieve the desired
		// Humidity Ratio in the Zone

		// REFERENCES:
		// Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
		// for BLAST.

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using General::RoundSigDigits;
		using DataSurfaces::Surface;
		using DataSurfaces::HeatTransferModel_EMPD;
		using DataSurfaces::HeatTransferModel_HAMT;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcPredictedHumidityRatio" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LatentGain; // Zone latent load
		Real64 RhoAir;
		Real64 A;
		Real64 B;
		Real64 C;
		Real64 SysTimeStepInSeconds;
		Real64 H2OHtOfVap;
		Real64 RHSetPoint; // Relative Humidity in percent
		Real64 WZoneSetPoint;
		int HumidControlledZoneNum;
		bool ControlledHumidZoneFlag; // This determines whether this is a humidity controlled zone or not
		Real64 ZoneRHHumidifyingSetPoint; // Zone humidifying set point (%)
		Real64 ZoneRHDehumidifyingSetPoint; // Zone dehumidifying set point (%)
		Real64 LoadToHumidifySetPoint; // Moisture load at humidifying set point
		Real64 LoadToDehumidifySetPoint; // Moisture load at dehumidifying set point
		Real64 ZoneAirRH; // Zone air relative humidity
		bool SingleSetPoint; // This determines whether both setpoint are equal or not
		int RoomAirNode;

		// FLOW:
		LoadToHumidifySetPoint = 0.0;
		LoadToDehumidifySetPoint = 0.0;
		SingleSetPoint = false;
		ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired = 0.0;
		ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToHumidifyingSP = 0.0;
		ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToDehumidifyingSP = 0.0;

		// Check to see if this is a "humidity controlled zone"
		ControlledHumidZoneFlag = false;
		// Check all the controlled zones to see if it matches the zone simulated
		for ( HumidControlledZoneNum = 1; HumidControlledZoneNum <= NumHumidityControlZones; ++HumidControlledZoneNum ) {
			if ( HumidityControlZone( HumidControlledZoneNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneAirRH = PsyRhFnTdbWPb( MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress ) * 100.0;
			ZoneRHHumidifyingSetPoint = GetCurrentScheduleValue( HumidityControlZone( HumidControlledZoneNum ).HumidifyingSchedIndex );
			ZoneRHDehumidifyingSetPoint = GetCurrentScheduleValue( HumidityControlZone( HumidControlledZoneNum ).DehumidifyingSchedIndex );

			// Apply EMS values to overwrite the humidistat values
			if ( HumidityControlZone( HumidControlledZoneNum ).EMSOverrideHumidifySetPointOn ) {
				ZoneRHHumidifyingSetPoint = HumidityControlZone( HumidControlledZoneNum ).EMSOverrideHumidifySetPointValue;
			}
			if ( HumidityControlZone( HumidControlledZoneNum ).EMSOverrideDehumidifySetPointOn ) {
				ZoneRHDehumidifyingSetPoint = HumidityControlZone( HumidControlledZoneNum ).EMSOverrideDehumidifySetPointValue;
			}

			// Apply offsets for faulty humidistats_Feb. 2015, zrp
			if ( ( NumFaultyHumidistat > 0 ) && ( ! WarmupFlag ) && ( ! DoingSizing ) && DoWeathSim ) {

				//  loop through the FaultsHumidistatOffset objects to find the one for the zone
				for ( int iFault = 1; iFault <= NumFaultyHumidistat; ++iFault ) {

					if ( SameString( HumidityControlZone( HumidControlledZoneNum ).ControlName, FaultsHumidistatOffset( iFault ).FaultyHumidistatName ) ) {

						if ( SameString( FaultsHumidistatOffset( iFault ).FaultyHumidistatType, "ThermostatOffsetDependent" ) ) {
						// For Humidistat Offset Type I: ThermostatOffsetDependent

							bool IsThermostatFound = false;
							double offsetThermostat = 0.0;
							double offsetZoneRHHumidifyingSetPoint = 0.0;
							double offsetZoneRHDehumidifyingSetPoint = 0.0;
							double faultZoneWHumidifyingSetPoint;
							double faultZoneWDehumidifyingSetPoint;

							// Get the offset value of the corresponding thermostat fault object
							if ( NumFaultyThermostat > 0 ) {

								//  loop through the FaultsThermostatOffset objects to find the one causes the Humidistat Offset
								for ( int iFaultThermo = 1; iFaultThermo <= NumFaultyThermostat; ++iFaultThermo ) {

									if ( SameString( FaultsHumidistatOffset( iFault ).FaultyThermostatName, FaultsThermostatOffset( iFaultThermo ).Name ) ) {
										IsThermostatFound = true;

										// Check fault availability schedules
										if ( GetCurrentScheduleValue( FaultsThermostatOffset( iFaultThermo ).AvaiSchedPtr ) > 0.0 ) {

											// Check fault severity schedules to update the reference thermostat offset
											double rSchVal = 1.0;
											if ( FaultsThermostatOffset( iFaultThermo ).SeveritySchedPtr >= 0 ) {
												rSchVal = GetCurrentScheduleValue( FaultsThermostatOffset( iFaultThermo ).SeveritySchedPtr );
											}
											offsetThermostat = rSchVal * FaultsThermostatOffset( iFaultThermo ).Offset;
										}

										// Stop searching the FaultsThermostatOffset object for the Humidistat Offset
										break;
									}
								}
							}

							// The FaultsThermostatOffset specified in the FaultHumidistatOffset is not found
							if ( ! IsThermostatFound ) {
								ShowSevereError( "FaultModel:HumidistatOffset = \"" + FaultsHumidistatOffset( iFault ).Name + "\" invalid Reference Humidistat Offset Name = \"" + FaultsHumidistatOffset( iFault ).FaultyThermostatName + "\" not found." );
								ShowFatalError( "Errors getting FaultModel input data.  Preceding condition(s) cause termination." );
							}

							if ( offsetThermostat != 0.0 ) {
								// Calculate the humidistat offset value from the thermostat offset value
								faultZoneWHumidifyingSetPoint = PsyWFnTdbRhPb( ( MAT( ZoneNum ) + offsetThermostat ), ( ZoneRHHumidifyingSetPoint / 100.0 ), OutBaroPress );
								faultZoneWDehumidifyingSetPoint = PsyWFnTdbRhPb( ( MAT( ZoneNum ) + offsetThermostat ), ( ZoneRHDehumidifyingSetPoint / 100.0 ), OutBaroPress );
								offsetZoneRHHumidifyingSetPoint = ZoneRHHumidifyingSetPoint - PsyRhFnTdbWPb( MAT( ZoneNum ), faultZoneWHumidifyingSetPoint, OutBaroPress ) * 100.0;
								offsetZoneRHDehumidifyingSetPoint = ZoneRHDehumidifyingSetPoint - PsyRhFnTdbWPb( MAT( ZoneNum ), faultZoneWDehumidifyingSetPoint, OutBaroPress ) * 100.0;

								// Apply the calculated humidistat offset value
								// Positive offset means the sensor reading is higher than the actual value
								ZoneRHHumidifyingSetPoint -= offsetZoneRHHumidifyingSetPoint;
								ZoneRHDehumidifyingSetPoint -= offsetZoneRHDehumidifyingSetPoint;

								// constrain value to something reasonable
								ZoneRHHumidifyingSetPoint = min( 100.0, max( 0.0, ZoneRHHumidifyingSetPoint ) );
								ZoneRHDehumidifyingSetPoint = min( 100.0, max( 0.0, ZoneRHDehumidifyingSetPoint ) );
							}

						} else {
						// For Humidistat Offset Type II: ThermostatOffsetIndependent

							// Check fault availability schedules
							if ( GetCurrentScheduleValue( FaultsHumidistatOffset( iFault ).AvaiSchedPtr ) > 0.0 ) {

								// Check fault severity schedules to update the reference humidistat offset
								double rSchVal = 1.0;
								double offsetUpdated;
								if ( FaultsHumidistatOffset( iFault ).SeveritySchedPtr >= 0 ) {
									rSchVal = GetCurrentScheduleValue( FaultsHumidistatOffset( iFault ).SeveritySchedPtr );
								}
								offsetUpdated = rSchVal * FaultsHumidistatOffset( iFault ).Offset;

								// Positive offset means the sensor reading is higher than the actual value
								ZoneRHHumidifyingSetPoint -= offsetUpdated;
								ZoneRHDehumidifyingSetPoint -= offsetUpdated;

								// constrain value to something reasonable
								ZoneRHHumidifyingSetPoint = min( 100.0, max( 0.0, ZoneRHHumidifyingSetPoint ) );
								ZoneRHDehumidifyingSetPoint = min( 100.0, max( 0.0, ZoneRHDehumidifyingSetPoint ) );
							}

						}

						// Stop searching the FaultsHumidistatOffset object for the zone
						break;
					}
				}
			}

			// Run-time error check
			if ( ZoneRHHumidifyingSetPoint > ZoneRHDehumidifyingSetPoint ) {
				//      HumidityControlZone(HumidControlledZoneNum)%ErrorCount = HumidityControlZone(HumidControlledZoneNum)%ErrorCount + 1
				if ( HumidityControlZone( HumidControlledZoneNum ).ErrorIndex == 0 ) {
					ShowWarningMessage( "HUMIDISTAT: The humidifying setpoint is above the dehumidifying setpoint in " + HumidityControlZone( HumidControlledZoneNum ).ControlName );
					ShowContinueError( "The zone humidifying setpoint is set to the dehumidifying setpoint." );
					ShowContinueErrorTimeStamp( "Occurrence info:" );
				}
				ShowRecurringWarningErrorAtEnd( "The humidifying setpoint is still above the dehumidifying setpoint", HumidityControlZone( HumidControlledZoneNum ).ErrorIndex, ZoneRHHumidifyingSetPoint, ZoneRHHumidifyingSetPoint );
				ZoneRHHumidifyingSetPoint = ZoneRHDehumidifyingSetPoint;
			}
			if ( ZoneRHHumidifyingSetPoint == ZoneRHDehumidifyingSetPoint ) SingleSetPoint = true;
			ControlledHumidZoneFlag = true;

			break;
		} // HumidControlledZoneNum

		if ( ControlledHumidZoneFlag ) {

			// Calculate hourly humidity ratio from infiltration + humidity added from latent load
			// to determine system added/subtracted moisture.
			LatentGain = ZoneLatentGain( ZoneNum ) + SumLatentHTRadSys( ZoneNum ) + SumLatentPool( ZoneNum );

			SysTimeStepInSeconds = SecInHour * TimeStepSys;

			// Calculate the coefficients for the 3rd Order derivative for final
			// zone humidity ratio.  The A, B, C coefficients are analogous to the heat balance.
			// SumHmARaW and SumHmARa will be used with the Moisture Balance on the building elements and
			// are currently set to zero when the CTF only version is used.

			// if no surface in the zone uses EMPD or HAMT then zero
			bool no_ht_EMPD_or_HAMT( true );
			for ( int i = Zone( ZoneNum ).SurfaceFirst, e = Zone( ZoneNum ).SurfaceLast; i <= e; ++i ) {
				auto const & htAlgo( Surface( i ).HeatTransferAlgorithm );
				if ( ( htAlgo == HeatTransferModel_EMPD ) || ( htAlgo == HeatTransferModel_HAMT ) ) {
					no_ht_EMPD_or_HAMT = false;
					break;
				}
			}
			if ( no_ht_EMPD_or_HAMT ) {
				SumHmARaW( ZoneNum ) = 0.0;
				SumHmARa( ZoneNum ) = 0.0;
			}

			// The density of air and latent heat of vaporization are calculated as functions.
			RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, ZT( ZoneNum ), ZoneAirHumRat( ZoneNum ), RoutineName );
			H2OHtOfVap = PsyHgAirFnWTdb( ZoneAirHumRat( ZoneNum ), ZT( ZoneNum ) );

			// Assume that the system will have flow
			if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS || ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated ) ) {
				// Multizone airflow calculated in AirflowNetwork
				B = ( LatentGain / H2OHtOfVap ) + AirflowNetworkExchangeData( ZoneNum ).SumMHrW + AirflowNetworkExchangeData( ZoneNum ).SumMMHrW + SumHmARaW( ZoneNum );
				A = AirflowNetworkExchangeData( ZoneNum ).SumMHr + AirflowNetworkExchangeData( ZoneNum ).SumMMHr + SumHmARa( ZoneNum );
			} else {
				B = ( LatentGain / H2OHtOfVap ) + ( ( OAMFL( ZoneNum ) + VAMFL( ZoneNum ) + EAMFL( ZoneNum ) + CTMFL( ZoneNum ) ) * OutHumRat ) + SumHmARaW( ZoneNum ) + MixingMassFlowXHumRat( ZoneNum ) + MDotOA( ZoneNum ) * OutHumRat;
				A = OAMFL( ZoneNum ) + VAMFL( ZoneNum ) + EAMFL( ZoneNum ) + CTMFL( ZoneNum ) + SumHmARa( ZoneNum ) + MixingMassFlowZone( ZoneNum ) + MDotOA( ZoneNum );
			}
			C = RhoAir * Zone( ZoneNum ).Volume * ZoneVolCapMultpMoist / SysTimeStepInSeconds;

			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
				RoomAirNode = RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID;
				H2OHtOfVap = PsyHgAirFnWTdb( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).HumRat,
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).AirTemp );
				A = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkM + RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmARa;
				B = ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumIntLatentGain / H2OHtOfVap )
					+ RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMW + RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmARaW;
				C = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).RhoAir * RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).AirVolume
					* ZoneVolCapMultpMoist / ( SecInHour * TimeStepSys );
			}

			// Use a 3rd Order derivative to predict zone moisture addition or removal and
			// smooth the changes using the zone air capacitance.  Positive values of Moist Load means that
			// this amount of moisture must be added to the zone to reach the setpoint.  Negative values represent
			// the amount of moisture that must be removed by the system.
			//MoistLoadHumidSetPoint = massflow * HumRat = kg air/sec  * kg H2O/kg Air = kg H2O/sec
			WZoneSetPoint = PsyWFnTdbRhPb( ZT( ZoneNum ), ( ZoneRHHumidifyingSetPoint / 100.0 ), OutBaroPress, RoutineName );
			Real64 exp_700_A_C( 0.0 );
			if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
				LoadToHumidifySetPoint = ( ( 11.0 / 6.0 ) * C + A ) * WZoneSetPoint - ( B + C * ( 3.0 * WZoneTimeMinus1Temp( ZoneNum ) - ( 3.0 / 2.0 ) * WZoneTimeMinus2Temp( ZoneNum ) + ( 1.0 / 3.0 ) * WZoneTimeMinus3Temp( ZoneNum ) ) );
				// Exact solution
			} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
				if ( A == 0.0 ) { // B=0
					LoadToHumidifySetPoint = C * ( WZoneSetPoint - ZoneW1( ZoneNum ) ) - B;
				} else {
					exp_700_A_C = std::exp( min( 700.0, - A / C ) ); //Tuned Save expensive value
					LoadToHumidifySetPoint = A * ( WZoneSetPoint - ZoneW1( ZoneNum ) * exp_700_A_C ) / ( 1.0 - exp_700_A_C ) - B;
				}
			} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
				LoadToHumidifySetPoint = C * ( WZoneSetPoint - ZoneW1( ZoneNum ) ) + A * WZoneSetPoint - B;
			}
			if ( RAFNFrac > 0.0 ) LoadToHumidifySetPoint = LoadToHumidifySetPoint / RAFNFrac;
			ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToHumidifyingSP = LoadToHumidifySetPoint;
			WZoneSetPoint = PsyWFnTdbRhPb( ZT( ZoneNum ), ( ZoneRHDehumidifyingSetPoint / 100.0 ), OutBaroPress, RoutineName );
			if ( ZoneAirSolutionAlgo == Use3rdOrder ) {
				LoadToDehumidifySetPoint = ( ( 11.0 / 6.0 ) * C + A ) * WZoneSetPoint - ( B + C * ( 3.0 * WZoneTimeMinus1Temp( ZoneNum ) - ( 3.0 / 2.0 ) * WZoneTimeMinus2Temp( ZoneNum ) + ( 1.0 / 3.0 ) * WZoneTimeMinus3Temp( ZoneNum ) ) );
				// Exact solution
			} else if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
				if ( A == 0.0 ) { // B=0
					LoadToDehumidifySetPoint = C * ( WZoneSetPoint - ZoneW1( ZoneNum ) ) - B;
				} else {
					LoadToDehumidifySetPoint = A * ( WZoneSetPoint - ZoneW1( ZoneNum ) * exp_700_A_C ) / ( 1.0 - exp_700_A_C ) - B; // exp_700_A_C set above
				}
			} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
				LoadToDehumidifySetPoint = C * ( WZoneSetPoint - ZoneW1( ZoneNum ) ) + A * WZoneSetPoint - B;
			}
			if ( RAFNFrac > 0.0 ) LoadToDehumidifySetPoint = LoadToDehumidifySetPoint / RAFNFrac;
			ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToDehumidifyingSP = LoadToDehumidifySetPoint;

			// The load is added to the TotalOutputRequired as in the Temperature Predictor.  There is also the remaining
			// output variable for those who will use this for humidity control and stored in DataZoneEnergyDemands with the
			// analogous temperature terms.
			if ( SingleSetPoint ) {
				ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired = LoadToHumidifySetPoint;
			} else {
				if ( LoadToHumidifySetPoint > 0.0 && LoadToDehumidifySetPoint > 0.0 ) {
					ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired = LoadToHumidifySetPoint;
					RHSetPoint = ZoneRHHumidifyingSetPoint;
				} else if ( LoadToHumidifySetPoint < 0.0 && LoadToDehumidifySetPoint < 0.0 ) {
					ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired = LoadToDehumidifySetPoint;
					RHSetPoint = ZoneRHDehumidifyingSetPoint;
				} else if ( LoadToHumidifySetPoint <= 0.0 && LoadToDehumidifySetPoint >= 0.0 ) { // deadband includes zero loads
					ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired = 0.0;
				} else { // this should never occur!
					ShowSevereError( "Humidistat: Unanticipated combination of humidifying and dehumidifying loads - report to EnergyPlus Development Team" );
					ShowContinueErrorTimeStamp( "occurs in Zone=" + Zone( ZoneNum ).Name );
					ShowContinueError( "LoadToHumidifySetPoint=" + RoundSigDigits( LoadToHumidifySetPoint, 5 ) + ", LoadToDehumidifySetPoint=" + RoundSigDigits( LoadToDehumidifySetPoint, 5 ) );
					ShowContinueError( "Zone RH Humidifying Set-point=" + RoundSigDigits( ZoneRHHumidifyingSetPoint, 1 ) );
					ShowContinueError( "Zone RH Dehumidifying Set-point=" + RoundSigDigits( ZoneRHDehumidifyingSetPoint, 2 ) );
					ShowFatalError( "Program terminates due to above conditions." );
				}
			}
		}

		// Save the unmultiplied zone moisture load to a report variable
		MoisturePredictedRate( ZoneNum ) = ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired;

		// Apply the Zone Multiplier to the total zone moisture load
		ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired *= Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
		ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToHumidifyingSP *= Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
		ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToDehumidifyingSP *= Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;

		//init each sequenced demand to the full output
		if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired = ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired; // array assignment
		if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP = ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToHumidifyingSP; // array assignment
		if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP = ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToDehumidifyingSP; // array assignment

	}

	void
	CorrectZoneAirTemp(
		Real64 & ZoneTempChange, // Temperature change in zone air between previous and current timestep
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russell Taylor
		//       DATE WRITTEN   ???
		//       MODIFIED       November 1999, LKL;
		//       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
		//                      February 2008 (Brent Griffith reworked history )

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the zone air temperature and modifies the system
		// time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataRoomAirModel::AirModel;
		using DataRoomAirModel::RoomAirModel_Mixing;
		using DataRoomAirModel::IsZoneDV;
		using DataRoomAirModel::ZoneDVMixedFlag;
		using DataRoomAirModel::IsZoneUI;
		using DataRoomAirModel::ZTFloor;
		using DataRoomAirModel::MATFloor;
		using DataRoomAirModel::XMATFloor;
		using DataRoomAirModel::XM2TFloor;
		using DataRoomAirModel::XM3TFloor;
		using DataRoomAirModel::XM4TFloor;
		using DataRoomAirModel::ZTOC;
		using DataRoomAirModel::ZTM1OC;
		using DataRoomAirModel::MATOC;
		using DataRoomAirModel::XMATOC;
		using DataRoomAirModel::XM2TOC;
		using DataRoomAirModel::XM3TOC;
		using DataRoomAirModel::XM4TOC;
		using DataRoomAirModel::ZTMX;
		using DataRoomAirModel::ZTM1MX;
		using DataRoomAirModel::MATMX;
		using DataRoomAirModel::XMATMX;
		using DataRoomAirModel::XM2TMX;
		using DataRoomAirModel::XM3TMX;
		using DataRoomAirModel::XM4TMX;
		using DataRoomAirModel::RoomAirModel_Mundt;
		using DataRoomAirModel::RoomAirModel_UserDefined;
		using RoomAirModelManager::ManageAirModel;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CorrectZoneAirTemp" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpAir; // specific heat of air
		static Real64 SumIntGain( 0.0 ); // Zone sum of convective internal gains
		static Real64 SumHA( 0.0 ); // Zone sum of Hc*Area
		static Real64 SumHATsurf( 0.0 ); // Zone sum of Hc*Area*Tsurf
		static Real64 SumHATref( 0.0 ); // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
		static Real64 SumMCp( 0.0 ); // Zone sum of MassFlowRate*Cp
		static Real64 SumMCpT( 0.0 ); // Zone sum of MassFlowRate*Cp*T
		static Real64 SumSysMCp( 0.0 ); // Zone sum of air system MassFlowRate*Cp
		static Real64 SumSysMCpT( 0.0 ); // Zone sum of air system MassFlowRate*Cp*T
		static Real64 ZoneEnthalpyIn( 0.0 ); // Zone inlet air enthalpy
		static Real64 TempDepCoef( 0.0 ); // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
		static Real64 TempIndCoef( 0.0 ); // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
		static Real64 AirCap( 0.0 ); // Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"
		static Real64 SNLoad( 0.0 ); // Sensible load calculated for zone in watts and then loaded in report variables
		static int ZoneNum( 0 );
		static int ZoneNodeNum( 0 ); // System node number for air flow through zone either by system or as a plenum
		//  LOGICAL,SAVE   :: OneTimeFlag = .TRUE.
		//unusd1208  LOGICAL,SAVE   :: MyEnvrnFlag = .TRUE.
		Real64 TempSupplyAir;
		Real64 ZoneMult;
		int LoopNode;
		//unused1208  REAL(r64)           :: TimeStepSeconds  ! dt term for denominator under Cz in Seconds

		// FLOW:
		// Initializations
		ZoneTempChange = constant_zero;

		//Tuned Precompute controlled equip zone numbers for performance
		std::vector< int > controlledZoneEquipConfigNums;
		for ( int ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum ) {
			if ( Zone( ZoneEquipConfigNum ).IsControlled ) {
				controlledZoneEquipConfigNums.push_back( ZoneEquipConfigNum );
			}
		}

		// Update zone temperatures
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;

			if ( ShortenTimeStepSys ) {
				// time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
				if ( NumOfSysTimeSteps != NumOfSysTimeStepsLastZoneTimeStep ) { // cannot reuse existing DS data, interpolate from zone time
					DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, MAT( ZoneNum ), XMAT( ZoneNum ), XM2T( ZoneNum ), XM3T( ZoneNum ), XM4T( ZoneNum ), MAT( ZoneNum ), DSXMAT( ZoneNum ), DSXM2T( ZoneNum ), DSXM3T( ZoneNum ), DSXM4T( ZoneNum ) );
					DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, ZoneAirHumRat( ZoneNum ), WZoneTimeMinus1( ZoneNum ), WZoneTimeMinus2( ZoneNum ), WZoneTimeMinus3( ZoneNum ), WZoneTimeMinus4( ZoneNum ), ZoneAirHumRat( ZoneNum ), DSWZoneTimeMinus1( ZoneNum ), DSWZoneTimeMinus2( ZoneNum ), DSWZoneTimeMinus3( ZoneNum ), DSWZoneTimeMinus4( ZoneNum ) );
					if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
						DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, MATFloor( ZoneNum ), XMATFloor( ZoneNum ), XM2TFloor( ZoneNum ), XM3TFloor( ZoneNum ), XM4TFloor( ZoneNum ), MATFloor( ZoneNum ), DSXMATFloor( ZoneNum ), DSXM2TFloor( ZoneNum ), DSXM3TFloor( ZoneNum ), DSXM4TFloor( ZoneNum ) );
						DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, MATOC( ZoneNum ), XMATOC( ZoneNum ), XM2TOC( ZoneNum ), XM3TOC( ZoneNum ), XM4TOC( ZoneNum ), MATOC( ZoneNum ), DSXMATOC( ZoneNum ), DSXM2TOC( ZoneNum ), DSXM3TOC( ZoneNum ), DSXM4TOC( ZoneNum ) );
						DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, MATMX( ZoneNum ), XMATMX( ZoneNum ), XM2TMX( ZoneNum ), XM3TMX( ZoneNum ), XM4TMX( ZoneNum ), MATMX( ZoneNum ), DSXMATMX( ZoneNum ), DSXM2TMX( ZoneNum ), DSXM3TMX( ZoneNum ), DSXM4TMX( ZoneNum ) );
					}
					if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
						for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
							auto & ThisRAFNNode( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ) );
							DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, ThisRAFNNode.AirTemp, ThisRAFNNode.AirTempX1, ThisRAFNNode.AirTempX2, ThisRAFNNode.AirTempX3, ThisRAFNNode.AirTempX4,
								ThisRAFNNode.AirTemp, ThisRAFNNode.AirTempDSX1, ThisRAFNNode.AirTempDSX2, ThisRAFNNode.AirTempDSX3, ThisRAFNNode.AirTempDSX4 );
							DownInterpolate4HistoryValues( PriorTimeStep, TimeStepSys, ThisRAFNNode.HumRat, ThisRAFNNode.HumRatX1, ThisRAFNNode.HumRatX2, ThisRAFNNode.HumRatX3, ThisRAFNNode.HumRatX4,
								ThisRAFNNode.HumRat, ThisRAFNNode.HumRatDSX1, ThisRAFNNode.HumRatDSX2, ThisRAFNNode.HumRatDSX3, ThisRAFNNode.HumRatDSX4 );
						}
					}
				} else { // reuse history data in DS terms from last zone time step to preserve information that would be lost
					// do nothing because DS history would have been pushed prior and should be ready?

				}
			}

			// now update the variables actually used in the balance equations.
			if ( ! UseZoneTimeStepHistory ) {
				ZTM1( ZoneNum ) = DSXMAT( ZoneNum );
				ZTM2( ZoneNum ) = DSXM2T( ZoneNum );
				ZTM3( ZoneNum ) = DSXM3T( ZoneNum );

				WZoneTimeMinus1Temp( ZoneNum ) = DSWZoneTimeMinus1( ZoneNum );
				WZoneTimeMinus2Temp( ZoneNum ) = DSWZoneTimeMinus2( ZoneNum );
				WZoneTimeMinus3Temp( ZoneNum ) = DSWZoneTimeMinus3( ZoneNum );
			} else {
				ZTM1( ZoneNum ) = XMAT( ZoneNum );
				ZTM2( ZoneNum ) = XM2T( ZoneNum );
				ZTM3( ZoneNum ) = XM3T( ZoneNum );

				WZoneTimeMinus1Temp( ZoneNum ) = WZoneTimeMinus1( ZoneNum );
				WZoneTimeMinus2Temp( ZoneNum ) = WZoneTimeMinus2( ZoneNum );
				WZoneTimeMinus3Temp( ZoneNum ) = WZoneTimeMinus3( ZoneNum );
			}

			AIRRAT( ZoneNum ) = Zone( ZoneNum ).Volume * ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ), RoutineName ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MAT( ZoneNum ) ) / ( TimeStepSys * SecInHour );

			AirCap = AIRRAT( ZoneNum );

			ManageAirModel( ZoneNum );

			// Calculate the various heat balance sums
			CalcZoneSums( ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT, controlledZoneEquipConfigNums );
			//    ZoneTempHistoryTerm = (3.0D0 * ZTM1(ZoneNum) - (3.0D0/2.0D0) * ZTM2(ZoneNum) + (1.0D0/3.0D0) * ZTM3(ZoneNum))
			ZoneNodeNum = Zone( ZoneNum ).SystemZoneNodeNumber;

			SNLoad = 0.0;

			if ( ZoneNodeNum > 0 ) { // This zone is controlled by a zone equipment configuration or zone plenum

				// Heat balance coefficients for controlled zone, i.e. with system air flow
				TempDepCoef = SumHA + SumMCp + SumSysMCp;
				TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT + ( NonAirSystemResponse( ZoneNum ) / ZoneMult + SysDepZoneLoadsLagged( ZoneNum ) );
				//    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

				if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone ) {
					TempIndCoef += AirflowNetworkExchangeData( ZoneNum ).TotalSen;
				}
				//    TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
				//    TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
				// Solve for zone air temperature
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZT( ZoneNum ) = ( TempIndCoef + AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) ) ) / ( ( 11.0 / 6.0 ) * AirCap + TempDepCoef );
					// Exact solution
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZT( ZoneNum ) = ZoneT1( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						ZT( ZoneNum ) = ( ZoneT1( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZT( ZoneNum ) = ( AirCap * ZoneT1( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}
				// Update zone node temperature and thermostat temperature unless already updated in Room Air Model,
				// calculate load correction factor
				if ( ( AirModel( ZoneNum ).AirModelType == RoomAirModel_Mixing ) || ( ! AirModel( ZoneNum ).SimAirModel ) ) {
					// Fully mixed
					Node( ZoneNodeNum ).Temp = ZT( ZoneNum );
					TempTstatAir( ZoneNum ) = ZT( ZoneNum );
					LoadCorrectionFactor( ZoneNum ) = 1.0;
				} else if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
					// UCSDDV: Not fully mixed - calculate factor to correct load for fully mixed assumption
					if ( SumSysMCp > SmallMassFlow ) {
						TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
						if ( std::abs( TempSupplyAir - ZT( ZoneNum ) ) > TempConvergTol ) {
							LoadCorrectionFactor( ZoneNum ) = ( TempSupplyAir - Node( ZoneNodeNum ).Temp ) / ( TempSupplyAir - ZT( ZoneNum ) );
							// constrain value to something reasonable
							LoadCorrectionFactor( ZoneNum ) = max( -3.0, LoadCorrectionFactor( ZoneNum ) );
							LoadCorrectionFactor( ZoneNum ) = min( 3.0, LoadCorrectionFactor( ZoneNum ) );

						} else {
							LoadCorrectionFactor( ZoneNum ) = 1.0; // Indeterminate
						}
					} else {
						// Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
						LoadCorrectionFactor( ZoneNum ) = 1.0;
					}
				} else if ( AirModel( ZoneNum ).SimAirModel && ( ( AirModel( ZoneNum ).AirModelType == RoomAirModel_UserDefined ) || ( AirModel( ZoneNum ).AirModelType == RoomAirModel_Mundt ) ) ) {
					if ( SumSysMCp > SmallMassFlow ) {
						TempSupplyAir = SumSysMCpT / SumSysMCp; // Non-negligible flow, calculate supply air temperature
						if ( std::abs( TempSupplyAir - ZT( ZoneNum ) ) > TempConvergTol ) {
							LoadCorrectionFactor( ZoneNum ) = ( TempSupplyAir - Node( ZoneNodeNum ).Temp ) / ( TempSupplyAir - ZT( ZoneNum ) );
							// constrain value
							LoadCorrectionFactor( ZoneNum ) = max( -3.0, LoadCorrectionFactor( ZoneNum ) );
							LoadCorrectionFactor( ZoneNum ) = min( 3.0, LoadCorrectionFactor( ZoneNum ) );

						} else {
							LoadCorrectionFactor( ZoneNum ) = 1.0; // Indeterminate
						}
					} else {
						// Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
						LoadCorrectionFactor( ZoneNum ) = 1.0;
					}
				} else if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
					//Zone node used in the RoomAirflowNetwork model
					ZT( ZoneNum ) = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID ).AirTemp;
					Node( ZoneNodeNum ).Temp = ZT( ZoneNum );
					TempTstatAir( ZoneNum ) = ZT( ZoneNum );
					LoadCorrectionFactor( ZoneNum ) = 1.0;
				} else {
					Node( ZoneNodeNum ).Temp = ZT( ZoneNum );
					TempTstatAir( ZoneNum ) = ZT( ZoneNum );
					LoadCorrectionFactor( ZoneNum ) = 1.0;
				}

				// Sensible load is the enthalpy into the zone minus the enthalpy that leaves the zone.
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), Node( ZoneNodeNum ).Temp );
				ZoneEnthalpyIn = SumSysMCpT;

				// SNLOAD is the single zone load, without Zone Multiplier or Zone List Multiplier
				SNLoad = ZoneEnthalpyIn - ( Node( ZoneNodeNum ).MassFlowRate / ZoneMult ) * CpAir * Node( ZoneNodeNum ).Temp + NonAirSystemResponse( ZoneNum ) / ZoneMult + SysDepZoneLoadsLagged( ZoneNum );

			} else {

				// Heat balance coefficients for uncontrolled zone, i.e. without system air flow
				TempDepCoef = SumHA + SumMCp;
				TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT;

				//      TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

				if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone ) {
					TempIndCoef += AirflowNetworkExchangeData( ZoneNum ).TotalSen;
				}
				//      TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
				//      TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef

				// Solve for zone air temperature
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZT( ZoneNum ) = ( TempIndCoef + AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + ( 1.0 / 3.0 ) * ZTM3( ZoneNum ) ) ) / ( ( 11.0 / 6.0 ) * AirCap + TempDepCoef );
					// Exact solution
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZT( ZoneNum ) = ZoneT1( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						ZT( ZoneNum ) = ( ZoneT1( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZT( ZoneNum ) = ( AirCap * ZoneT1( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}

				if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
					ZT( ZoneNum ) = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID ).AirTemp;
				}

				// No sensible load
				SNLoad = 0.0;
			}

			MAT( ZoneNum ) = ZT( ZoneNum );

			// Determine sensible load heating/cooling rate and energy
			SNLoadHeatRate( ZoneNum ) = max( SNLoad, 0.0 );
			SNLoadCoolRate( ZoneNum ) = std::abs( min( SNLoad, 0.0 ) );
			SNLoadHeatEnergy( ZoneNum ) = max( SNLoad, 0.0 ) * TimeStepSys * SecInHour;
			SNLoadCoolEnergy( ZoneNum ) = std::abs( min( SNLoad, 0.0 ) * TimeStepSys * SecInHour );

			// Final humidity calcs
			CorrectZoneHumRat( ZoneNum, controlledZoneEquipConfigNums );

			ZoneAirHumRat( ZoneNum ) = ZoneAirHumRatTemp( ZoneNum );
			ZoneAirRelHum( ZoneNum ) = 100.0 * PsyRhFnTdbWPb( ZT( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress, RoutineName );

			// ZoneTempChange is used by HVACManager to determine if the timestep needs to be shortened.
			{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
			if ( SELECT_CASE_var == Use3rdOrder ) {
				if ( IsZoneDV( ZoneNum ) ) {
					if ( ZoneDVMixedFlag( ZoneNum ) == 0 ) {
						ZoneTempChange = max( ZoneTempChange, max( std::abs( ZTOC( ZoneNum ) - ZTM1OC( ZoneNum ) ), std::abs( ZTMX( ZoneNum ) - ZTM1MX( ZoneNum ) ) ) );
					} else {
						ZoneTempChange = max( ZoneTempChange, std::abs( ZT( ZoneNum ) - ZTM1( ZoneNum ) ) );
					}
				} else if ( IsZoneUI( ZoneNum ) ) {
					if ( ZoneUFMixedFlag( ZoneNum ) == 0 ) {
						ZoneTempChange = max( ZoneTempChange, max( std::abs( ZTOC( ZoneNum ) - ZTM1OC( ZoneNum ) ), std::abs( ZTMX( ZoneNum ) - ZTM1MX( ZoneNum ) ) ) );
					} else {
						ZoneTempChange = max( ZoneTempChange, std::abs( ZT( ZoneNum ) - ZTM1( ZoneNum ) ) );
					}
				} else {
					ZoneTempChange = max( ZoneTempChange, std::abs( ZT( ZoneNum ) - ZTM1( ZoneNum ) ) );
				}
			} else if ( ( SELECT_CASE_var == UseAnalyticalSolution ) || ( SELECT_CASE_var == UseEulerMethod ) ) {
				if ( IsZoneDV( ZoneNum ) ) {
					if ( ZoneDVMixedFlag( ZoneNum ) == 0 ) {
						ZoneTempChange = max( ZoneTempChange, max( std::abs( ZTOC( ZoneNum ) - Zone1OC( ZoneNum ) ), std::abs( ZTMX( ZoneNum ) - Zone1MX( ZoneNum ) ) ) );
					} else {
						ZoneTempChange = max( ZoneTempChange, std::abs( ZT( ZoneNum ) - ZoneT1( ZoneNum ) ) );
					}
				} else if ( IsZoneUI( ZoneNum ) ) {
					if ( ZoneUFMixedFlag( ZoneNum ) == 0 ) {
						ZoneTempChange = max( ZoneTempChange, max( std::abs( ZTOC( ZoneNum ) - Zone1OC( ZoneNum ) ), std::abs( ZTMX( ZoneNum ) - Zone1MX( ZoneNum ) ) ) );
					} else {
						ZoneTempChange = max( ZoneTempChange, std::abs( ZT( ZoneNum ) - ZoneT1( ZoneNum ) ) );
					}
				} else {
					ZoneTempChange = max( ZoneTempChange, std::abs( ZT( ZoneNum ) - ZoneT1( ZoneNum ) ) );
				}
			}}

			CalcZoneComponentLoadSums( ZoneNum, TempDepCoef, TempIndCoef, ZnAirRpt( ZoneNum ).SumIntGains, ZnAirRpt( ZoneNum ).SumHADTsurfs, ZnAirRpt( ZoneNum ).SumMCpDTzones, ZnAirRpt( ZoneNum ).SumMCpDtInfil, ZnAirRpt( ZoneNum ).SumMCpDTsystem, ZnAirRpt( ZoneNum ).SumNonAirSystem, ZnAirRpt( ZoneNum ).CzdTdt, ZnAirRpt( ZoneNum ).imBalance, controlledZoneEquipConfigNums ); // convection part of internal gains | surface convection heat transfer | interzone mixing | OA of various kinds except via system | air system | non air system | air mass energy storage term | measure of imbalance in zone air heat balance

		} // ZoneNum

	}

	void
	PushZoneTimestepHistories()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// push histories for timestep advancing

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		static std::string const CorrectZoneAirTemp( "CorrectZoneAirTemp" );

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		int LoopNode;

		// Push the temperature and humidity ratio histories

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			XM4T( ZoneNum ) = XM3T( ZoneNum );
			XM3T( ZoneNum ) = XM2T( ZoneNum );
			XM2T( ZoneNum ) = XMAT( ZoneNum );
			XMAT( ZoneNum ) = ZTAV( ZoneNum ); // using average for whole zone time step.
			XMPT( ZoneNum ) = ZT( ZoneNum );
			//      MAT(ZoneNum)  = ZT(ZoneNum)

			WZoneTimeMinus4( ZoneNum ) = WZoneTimeMinus3( ZoneNum );
			WZoneTimeMinus3( ZoneNum ) = WZoneTimeMinus2( ZoneNum );
			WZoneTimeMinus2( ZoneNum ) = WZoneTimeMinus1( ZoneNum );
			WZoneTimeMinus1( ZoneNum ) = ZoneAirHumRatAvg( ZoneNum ); // using average for whole zone time step.
			ZoneAirHumRat( ZoneNum ) = ZoneAirHumRatTemp( ZoneNum );
			WZoneTimeMinusP( ZoneNum ) = ZoneAirHumRatTemp( ZoneNum );
			ZoneAirRelHum( ZoneNum ) = 100.0 * PsyRhFnTdbWPb( ZT( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress, CorrectZoneAirTemp );

			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDDV || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFI || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFE ) {
				XM4TFloor( ZoneNum ) = XM3TFloor( ZoneNum );
				XM3TFloor( ZoneNum ) = XM2TFloor( ZoneNum );
				XM2TFloor( ZoneNum ) = XMATFloor( ZoneNum );
				XMATFloor( ZoneNum ) = ZTFloor( ZoneNum );
				MATFloor( ZoneNum ) = ZTFloor( ZoneNum );

				XM4TOC( ZoneNum ) = XM3TOC( ZoneNum );
				XM3TOC( ZoneNum ) = XM2TOC( ZoneNum );
				XM2TOC( ZoneNum ) = XMATOC( ZoneNum );
				XMATOC( ZoneNum ) = ZTOC( ZoneNum );
				MATOC( ZoneNum ) = ZTOC( ZoneNum );

				XM4TMX( ZoneNum ) = XM3TMX( ZoneNum );
				XM3TMX( ZoneNum ) = XM2TMX( ZoneNum );
				XM2TMX( ZoneNum ) = XMATMX( ZoneNum );
				XMATMX( ZoneNum ) = ZTMX( ZoneNum );
				MATMX( ZoneNum ) = ZTMX( ZoneNum );
			}

			// for RoomAirflowNetwork model
			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
				for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX4 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX3;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX3 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX2;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX1;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTemp;

					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX4 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX3;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX3 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX2;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX1;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRat;
				}
			}

			if ( ZoneAirSolutionAlgo != Use3rdOrder ) {
				ZoneTM2( ZoneNum ) = ZoneTMX( ZoneNum );
				ZoneTMX( ZoneNum ) = ZTAV( ZoneNum ); // using average for whole zone time step.
				ZoneWM2( ZoneNum ) = ZoneWMX( ZoneNum );
				ZoneWMX( ZoneNum ) = ZoneAirHumRatAvg( ZoneNum ); // using average for whole zone time step.
				if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDDV || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFI || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFE ) {
					ZoneM2Floor( ZoneNum ) = ZoneMXFloor( ZoneNum );
					ZoneMXFloor( ZoneNum ) = ZTFloor( ZoneNum ); // using average for whole zone time step.
					ZoneM2OC( ZoneNum ) = ZoneMXOC( ZoneNum );
					ZoneMXOC( ZoneNum ) = ZTOC( ZoneNum ); // using average for whole zone time step.
					ZoneM2MX( ZoneNum ) = ZoneMXMX( ZoneNum );
					ZoneMXMX( ZoneNum ) = ZTMX( ZoneNum ); // using average for whole zone time step.
				}

				if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
					for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTM2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTemp;
//						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWM2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRat;
//						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1;
			}
				}
			}
		} // zone loop

	}

	void
	PushSystemTimestepHistories()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// push histories

		// METHODOLOGY EMPLOYED:
		// <description>

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
		int ZoneNum;
		int LoopNode;

		// Push the temperature and humidity ratio histories back in time

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			DSXM4T( ZoneNum ) = DSXM3T( ZoneNum );
			DSXM3T( ZoneNum ) = DSXM2T( ZoneNum );
			DSXM2T( ZoneNum ) = DSXMAT( ZoneNum );
			DSXMAT( ZoneNum ) = MAT( ZoneNum );

			DSWZoneTimeMinus4( ZoneNum ) = DSWZoneTimeMinus3( ZoneNum );
			DSWZoneTimeMinus3( ZoneNum ) = DSWZoneTimeMinus2( ZoneNum );
			DSWZoneTimeMinus2( ZoneNum ) = DSWZoneTimeMinus1( ZoneNum );
			DSWZoneTimeMinus1( ZoneNum ) = ZoneAirHumRat( ZoneNum );

			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {
				DSXM4TFloor( ZoneNum ) = DSXM3TFloor( ZoneNum );
				DSXM3TFloor( ZoneNum ) = DSXM2TFloor( ZoneNum );
				DSXM2TFloor( ZoneNum ) = DSXMATFloor( ZoneNum );
				DSXMATFloor( ZoneNum ) = MATFloor( ZoneNum );

				DSXM4TOC( ZoneNum ) = DSXM3TOC( ZoneNum );
				DSXM3TOC( ZoneNum ) = DSXM2TOC( ZoneNum );
				DSXM2TOC( ZoneNum ) = DSXMATOC( ZoneNum );
				DSXMATOC( ZoneNum ) = MATOC( ZoneNum );

				DSXM4TMX( ZoneNum ) = DSXM3TMX( ZoneNum );
				DSXM3TMX( ZoneNum ) = DSXM2TMX( ZoneNum );
				DSXM2TMX( ZoneNum ) = DSXMATMX( ZoneNum );
				DSXMATMX( ZoneNum ) = MATMX( ZoneNum );
			}
			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
				for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX4 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX3;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX3 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX2;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX1;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempDSX1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTemp;

					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX4 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX3;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX3 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX2;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX1;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatDSX1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRat;
				}
			}
		} // zone loop

		if ( ZoneAirSolutionAlgo != Use3rdOrder ) {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				ZoneTM2( ZoneNum ) = ZoneTMX( ZoneNum );
				ZoneTMX( ZoneNum ) = MAT( ZoneNum ); // using average for whole zone time step.
				ZoneWM2( ZoneNum ) = ZoneWMX( ZoneNum );
				ZoneWMX( ZoneNum ) = ZoneAirHumRatTemp( ZoneNum ); // using average for whole zone time step.

				if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDDV || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFI || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFE ) {
					ZoneM2Floor( ZoneNum ) = ZoneMXFloor( ZoneNum );
					ZoneMXFloor( ZoneNum ) = ZTFloor( ZoneNum ); // using average for whole zone time step.
					ZoneM2OC( ZoneNum ) = ZoneMXOC( ZoneNum );
					ZoneMXOC( ZoneNum ) = ZTOC( ZoneNum ); // using average for whole zone time step.
					ZoneM2MX( ZoneNum ) = ZoneMXMX( ZoneNum );
					ZoneMXMX( ZoneNum ) = ZTMX( ZoneNum ); // using average for whole zone time step.
				}
				if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
					for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTM2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTemp;
//						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempTMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempT1;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWM2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRat;
//						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatWMX = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatW1;
					}
				}
			} // zone loop
		}

	}

	void
	RevertZoneTimestepHistories()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// rewind histories to undo inadvertent pushing

		// METHODOLOGY EMPLOYED:
		// <description>

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
		int ZoneNum;
		int LoopNode;

		// REvert the temperature and humidity ratio histories

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			//  MAT(ZoneNum)  = XMAT(ZoneNum)
			XMAT( ZoneNum ) = XM2T( ZoneNum );
			XM2T( ZoneNum ) = XM3T( ZoneNum );
			XM3T( ZoneNum ) = XM4T( ZoneNum );

			//   ZoneAirHumRat(ZoneNum)  = WZoneTimeMinus1(ZoneNum)
			WZoneTimeMinus1( ZoneNum ) = WZoneTimeMinus2( ZoneNum );
			WZoneTimeMinus2( ZoneNum ) = WZoneTimeMinus3( ZoneNum );
			WZoneTimeMinus3( ZoneNum ) = WZoneTimeMinus4( ZoneNum );

			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDDV || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFI || AirModel( ZoneNum ).AirModelType == RoomAirModel_UCSDUFE ) {

				//      MATFloor(ZoneNum)= XMATFloor(ZoneNum)
				XMATFloor( ZoneNum ) = XM2TFloor( ZoneNum );
				XM2TFloor( ZoneNum ) = XM3TFloor( ZoneNum );
				XM3TFloor( ZoneNum ) = XM4TFloor( ZoneNum );
				//      MATOC(ZoneNum) = XMATOC(ZoneNum)
				XMATOC( ZoneNum ) = XM2TOC( ZoneNum );
				XM2TOC( ZoneNum ) = XM3TOC( ZoneNum );
				XM3TOC( ZoneNum ) = XM4TOC( ZoneNum );

				//     MATMX(ZoneNum)=  XMATMX(ZoneNum)
				XMATMX( ZoneNum ) = XM2TMX( ZoneNum );
				XM2TMX( ZoneNum ) = XM3TMX( ZoneNum );
				XM3TMX( ZoneNum ) = XM4TMX( ZoneNum );

			}

			if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
				for ( LoopNode = 1; LoopNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopNode ) {
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX2;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX3;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX3 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).AirTempX4;

					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX1 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX2;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX2 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX3;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX3 = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopNode ).HumRatX4;
				}
			}
		} // zone loop

	}

	void
	CorrectZoneHumRat(
		int const ZoneNum,
		std::vector< int > const & controlledZoneEquipConfigNums // Precomputed controlled equip nums
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the zone humidities.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
		// for BLAST.

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipConfig;
		using ZonePlenum::ZoneRetPlenCond;
		using ZonePlenum::ZoneSupPlenCond;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;
		using DataDefineEquip::AirDistUnit;
		using DataSurfaces::Surface;
		using DataSurfaces::HeatTransferModel_HAMT;
		using DataSurfaces::HeatTransferModel_EMPD;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CorrectZoneHumRat" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NodeNum;
		int ZoneNodeNum;
		int ZoneEquipConfigNum;
		bool ControlledZoneAirFlag;
		int ZoneRetPlenumNum;
		int ZoneSupPlenumNum;
		bool ZoneRetPlenumAirFlag;
		bool ZoneSupPlenumAirFlag;
		Real64 LatentGain; // Zone latent load
		Real64 RhoAir;
		Real64 A;
		Real64 B;
		Real64 C;
		Real64 WZSat;
		Real64 MoistureMassFlowRate;
		Real64 ExhMassFlowRate;
		Real64 TotExitMassFlowRate;
		Real64 ZoneMassFlowRate;
		Real64 SysTimeStepInSeconds;
		Real64 H2OHtOfVap;
		Real64 ZoneMult;
		int ADUListIndex;
		int ADUNum;
		int ADUInNode;
		int ADUOutNode;

		// FLOW:
		MoistureMassFlowRate = 0.0;
		ZoneMassFlowRate = 0.0;
		ExhMassFlowRate = 0.0;
		TotExitMassFlowRate = 0.0;
		ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;

		// Check to see if this is a controlled zone
		ZoneEquipConfigNum = 0;
		ControlledZoneAirFlag = false;
		for ( std::vector< int >::size_type i = 0, e = controlledZoneEquipConfigNums.size(); i < e; ++i ) {
			if ( ZoneEquipConfig( controlledZoneEquipConfigNums[ i ] ).ActualZoneNum == ZoneNum ) {
				ZoneEquipConfigNum = controlledZoneEquipConfigNums[ i ];
				ControlledZoneAirFlag = true;
				break;
			}
		}

		// Check to see if this is a plenum zone
		ZoneRetPlenumAirFlag = false;
		for ( ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= NumZoneReturnPlenums; ++ZoneRetPlenumNum ) {
			if ( ZoneRetPlenCond( ZoneRetPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneRetPlenumAirFlag = true;
			break;
		} // ZoneRetPlenumNum
		ZoneSupPlenumAirFlag = false;
		for ( ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= NumZoneSupplyPlenums; ++ZoneSupPlenumNum ) {
			if ( ZoneSupPlenCond( ZoneSupPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneSupPlenumAirFlag = true;
			break;
		} // ZoneSupPlenumNum

		if ( ControlledZoneAirFlag ) { // If there is system flow then calculate the flow rates

			// Calculate moisture flow rate into each zone
			for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {

				MoistureMassFlowRate += ( Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate * Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).HumRat ) / ZoneMult;
				ZoneMassFlowRate += Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate / ZoneMult;
			} // NodeNum

			for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumExhaustNodes; ++NodeNum ) {
				ExhMassFlowRate += Node( ZoneEquipConfig( ZoneEquipConfigNum ).ExhaustNode( NodeNum ) ).MassFlowRate / ZoneMult;
			} // NodeNum
			ExhMassFlowRate -= ZoneEquipConfig( ZoneEquipConfigNum ).ZoneExhBalanced; // Balanced exhaust flow assumes there are other flows providing makeup air such as mixing or infiltration, so subtract it here

			if ( ZoneEquipConfig( ZoneEquipConfigNum ).ReturnAirNode > 0 ) {
				TotExitMassFlowRate = ExhMassFlowRate + Node( ZoneEquipConfig( ZoneEquipConfigNum ).ReturnAirNode ).MassFlowRate / ZoneMult;
			}

			// Do the calculations for the plenum zone
		} else if ( ZoneRetPlenumAirFlag ) {
			for ( NodeNum = 1; NodeNum <= ZoneRetPlenCond( ZoneRetPlenumNum ).NumInletNodes; ++NodeNum ) {

				MoistureMassFlowRate += ( Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).MassFlowRate * Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).HumRat ) / ZoneMult;
				ZoneMassFlowRate += Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).MassFlowRate / ZoneMult;
			} // NodeNum
			// add in the leak flow
			for ( ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond( ZoneRetPlenumNum ).NumADUs; ++ADUListIndex ) {
				ADUNum = ZoneRetPlenCond( ZoneRetPlenumNum ).ADUIndex( ADUListIndex );
				if ( AirDistUnit( ADUNum ).UpStreamLeak ) {
					ADUInNode = AirDistUnit( ADUNum ).InletNodeNum;
					MoistureMassFlowRate += ( AirDistUnit( ADUNum ).MassFlowRateUpStrLk * Node( ADUInNode ).HumRat ) / ZoneMult;
					ZoneMassFlowRate += AirDistUnit( ADUNum ).MassFlowRateUpStrLk / ZoneMult;
				}
				if ( AirDistUnit( ADUNum ).DownStreamLeak ) {
					ADUOutNode = AirDistUnit( ADUNum ).OutletNodeNum;
					MoistureMassFlowRate += ( AirDistUnit( ADUNum ).MassFlowRateDnStrLk * Node( ADUOutNode ).HumRat ) / ZoneMult;
					ZoneMassFlowRate += AirDistUnit( ADUNum ).MassFlowRateDnStrLk / ZoneMult;
				}
			}
			// Do not allow exhaust mass flow for a plenum zone
			ExhMassFlowRate = 0.0;
			TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate;

		} else if ( ZoneSupPlenumAirFlag ) {

			MoistureMassFlowRate += ( Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).MassFlowRate * Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).HumRat ) / ZoneMult;
			ZoneMassFlowRate += Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).MassFlowRate / ZoneMult;
			// Do not allow exhaust mass flow for a plenum zone
			ExhMassFlowRate = 0.0;
			TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate;
		}

		// Calculate hourly humidity ratio from infiltration + humdidity added from latent load + system added moisture
		LatentGain = ZoneLatentGain( ZoneNum ) + SumLatentHTRadSys( ZoneNum )  + SumLatentPool( ZoneNum );

		SysTimeStepInSeconds = SecInHour * TimeStepSys;

		// Calculate the coefficients for the 3rd order derivative for final
		// zone humidity ratio.  The A, B, C coefficients are analogous to the
		// heat balance.  There are 2 cases that should be considered, system
		// operating and system shutdown.
		// SumHmARaW and SumHmARa will be used with the moisture balance on the building elements and
		// are currently set to zero to remind us where they need to be in the future
		bool no_ht_EMPD_or_HAMT( true );
		for ( int i = Zone( ZoneNum ).SurfaceFirst, e = Zone( ZoneNum ).SurfaceLast; i <= e; ++i ) {
			auto const & htAlgo( Surface( i ).HeatTransferAlgorithm );
			if ( ( htAlgo == HeatTransferModel_EMPD ) || ( htAlgo == HeatTransferModel_HAMT ) ) {
				no_ht_EMPD_or_HAMT = false;
				break;
			}
		}
		if ( no_ht_EMPD_or_HAMT ) {
			SumHmARaW( ZoneNum ) = 0.0;
			SumHmARa( ZoneNum ) = 0.0;
		}

		RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, ZT( ZoneNum ), ZoneAirHumRat( ZoneNum ), RoutineName );
		H2OHtOfVap = PsyHgAirFnWTdb( ZoneAirHumRat( ZoneNum ), ZT( ZoneNum ) );

		// Check for the flow and NO flow condition
		if ( ZoneMassFlowRate > 0.0 ) {
			B = ( LatentGain / H2OHtOfVap ) + ( ( OAMFL( ZoneNum ) + VAMFL( ZoneNum ) + EAMFL( ZoneNum ) + CTMFL( ZoneNum ) ) * OutHumRat ) + ( MoistureMassFlowRate ) + SumHmARaW( ZoneNum ) + MixingMassFlowXHumRat( ZoneNum ) + MDotOA( ZoneNum ) * OutHumRat;
			A = TotExitMassFlowRate + OAMFL( ZoneNum ) + VAMFL( ZoneNum ) + EAMFL( ZoneNum ) + CTMFL( ZoneNum ) + SumHmARa( ZoneNum ) + MixingMassFlowZone( ZoneNum ) + MDotOA( ZoneNum );
			if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS || ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated ) ) {
				// Multizone airflow calculated in AirflowNetwork
				B = ( LatentGain / H2OHtOfVap ) + ( AirflowNetworkExchangeData( ZoneNum ).SumMHrW + AirflowNetworkExchangeData( ZoneNum ).SumMMHrW ) + ( MoistureMassFlowRate ) + SumHmARaW( ZoneNum );
				A = TotExitMassFlowRate + AirflowNetworkExchangeData( ZoneNum ).SumMHr + AirflowNetworkExchangeData( ZoneNum ).SumMMHr + SumHmARa( ZoneNum );
			}
			C = RhoAir * Zone( ZoneNum ).Volume * ZoneVolCapMultpMoist / SysTimeStepInSeconds;
		} else if ( ZoneMassFlowRate <= 0.0 ) {
			B = ( LatentGain / H2OHtOfVap ) + ( ( OAMFL( ZoneNum ) + VAMFL( ZoneNum ) + EAMFL( ZoneNum ) + CTMFL( ZoneNum ) + ExhMassFlowRate ) * OutHumRat ) + SumHmARaW( ZoneNum ) + MixingMassFlowXHumRat( ZoneNum );
			A = OAMFL( ZoneNum ) + VAMFL( ZoneNum ) + EAMFL( ZoneNum ) + CTMFL( ZoneNum ) + ExhMassFlowRate + SumHmARa( ZoneNum ) + MixingMassFlowZone( ZoneNum );
			if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS || ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated ) ) {
				// Multizone airflow calculated in AirflowNetwork
				B = ( LatentGain / H2OHtOfVap ) + SumHmARaW( ZoneNum ) + AirflowNetworkExchangeData( ZoneNum ).SumMHrW + AirflowNetworkExchangeData( ZoneNum ).SumMMHrW;
				A = AirflowNetworkExchangeData( ZoneNum ).SumMHr + AirflowNetworkExchangeData( ZoneNum ).SumMMHr + SumHmARa( ZoneNum );
			}
			C = RhoAir * Zone( ZoneNum ).Volume * ZoneVolCapMultpMoist / SysTimeStepInSeconds;
		}

		if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone ) {
			B += AirflowNetworkExchangeData( ZoneNum ).TotalLat;
		}

		// Use a 3rd order derivative to predict final zone humidity ratio and
		// smooth the changes using the zone air capacitance.
		{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
		if ( SELECT_CASE_var == Use3rdOrder ) {
			ZoneAirHumRatTemp( ZoneNum ) = ( B + C * ( 3.0 * WZoneTimeMinus1Temp( ZoneNum ) - ( 3.0 / 2.0 ) * WZoneTimeMinus2Temp( ZoneNum ) + ( 1.0 / 3.0 ) * WZoneTimeMinus3Temp( ZoneNum ) ) ) / ( ( 11.0 / 6.0 ) * C + A );
			// Exact solution
		} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
			if ( A == 0.0 ) { // B=0
				ZoneAirHumRatTemp( ZoneNum ) = ZoneW1( ZoneNum ) + B / C;
			} else {
				ZoneAirHumRatTemp( ZoneNum ) = ( ZoneW1( ZoneNum ) - B / A ) * std::exp( min( 700.0, - A / C ) ) + B / A;
			}
		} else if ( SELECT_CASE_var == UseEulerMethod ) {
			ZoneAirHumRatTemp( ZoneNum ) = ( C * ZoneW1( ZoneNum ) + B ) / ( C + A );
		}}

		// Set the humidity ratio to zero if the zone has been dried out
		if ( ZoneAirHumRatTemp( ZoneNum ) < 0.0 ) ZoneAirHumRatTemp( ZoneNum ) = 0.0;

		// Check to make sure that is saturated there is condensation in the zone
		// by resetting to saturation conditions.
		WZSat = PsyWFnTdbRhPb( ZT( ZoneNum ), 1.0, OutBaroPress, RoutineName );

		if ( ZoneAirHumRatTemp( ZoneNum ) > WZSat ) ZoneAirHumRatTemp( ZoneNum ) = WZSat;

		if ( AirModel( ZoneNum ).AirModelType == RoomAirModel_AirflowNetwork ) {
			ZoneAirHumRatTemp( ZoneNum ) = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID ).HumRat;
		}

		// Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
		ZoneNodeNum = Zone( ZoneNum ).SystemZoneNodeNumber;
		if ( ZoneNodeNum > 0 ) {
			Node( ZoneNodeNum ).HumRat = ZoneAirHumRatTemp( ZoneNum );
			Node( ZoneNodeNum ).Enthalpy = PsyHFnTdbW( ZT( ZoneNum ), ZoneAirHumRatTemp( ZoneNum ) );
		}

	}

	void
	DownInterpolate4HistoryValues(
		Real64 const OldTimeStep,
		Real64 const NewTimeStep,
		Real64 & oldVal0,
		Real64 & oldVal1,
		Real64 & oldVal2,
		Real64 & EP_UNUSED( oldVal3 ),
		Real64 & EP_UNUSED( oldVal4 ),
		Real64 & newVal0,
		Real64 & newVal1,
		Real64 & newVal2,
		Real64 & newVal3, // unused 1208
		Real64 & newVal4 // unused 1208
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Feb 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// provide a reusable routine for the various places that need to
		// interpolate a new set of history values on a different time scale
		// Once the systemtimestep has shortened, the new history terms need to be interpolated

		// METHODOLOGY EMPLOYED:
		// This routine assumes that the direction is to a shorter timestep.
		// The down step ratio, DSRatio = OldTimeStep/ NewTimeStep
		//  is expected to be roughly integer-valued and near 2.0 or 3.0 or 4.0 or more.

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
		Real64 oldTime0;
		Real64 oldTime1;
		Real64 oldTime2;
		Real64 oldTime3;
		Real64 oldTime4;
		Real64 newTime0;
		Real64 newTime1;
		Real64 newTime2;
		Real64 newTime3;
		Real64 newTime4;

		Real64 DSRatio;

		// first construct data on timestamps for interpolating with later
		oldTime0 = 0.0;
		oldTime1 = oldTime0 - OldTimeStep;
		oldTime2 = oldTime1 - OldTimeStep;
		oldTime3 = oldTime2 - OldTimeStep;
		oldTime4 = oldTime3 - OldTimeStep;

		newTime0 = 0.0;
		newTime1 = newTime0 - NewTimeStep;
		newTime2 = newTime1 - NewTimeStep;
		newTime3 = newTime2 - NewTimeStep;
		newTime4 = newTime3 - NewTimeStep;

		DSRatio = OldTimeStep / NewTimeStep; // should pretty much be an integer value 2, 3, 4, etc.

		newVal0 = oldVal0;

		if ( std::abs( DSRatio - 2.0 ) < 0.01 ) { // DSRatio = 2
			// first two points lie between oldVal0 and oldVal1
			newVal1 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime1 ) / ( OldTimeStep ) );
			newVal2 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime2 ) / ( OldTimeStep ) );
			// last two points lie between oldVal1 and oldVal2
			newVal3 = oldVal1 + ( oldVal2 - oldVal1 ) * ( ( oldTime1 - newTime3 ) / ( OldTimeStep ) );
			newVal4 = oldVal1 + ( oldVal2 - oldVal1 ) * ( ( oldTime1 - newTime4 ) / ( OldTimeStep ) );
		} else if ( std::abs( DSRatio - 3.0 ) < 0.01 ) { // DSRatio = 3
			// first three points lie between oldVal0 and oldVal1
			newVal1 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime1 ) / ( OldTimeStep ) );
			newVal2 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime2 ) / ( OldTimeStep ) );
			newVal3 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime3 ) / ( OldTimeStep ) );
			// last point lie between oldVal1 and oldVal2
			newVal4 = oldVal1 + ( oldVal2 - oldVal1 ) * ( ( oldTime1 - newTime4 ) / ( OldTimeStep ) );

		} else if ( DSRatio > 3.99 ) { // DSRatio = 4 or more
			//all new points lie between oldVal0 and oldVal1
			newVal1 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime1 ) / ( OldTimeStep ) );
			newVal2 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime2 ) / ( OldTimeStep ) );
			newVal3 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime3 ) / ( OldTimeStep ) );
			newVal4 = oldVal0 + ( oldVal1 - oldVal0 ) * ( ( oldTime0 - newTime4 ) / ( OldTimeStep ) );
		}

	}

	void
	CalcZoneSums(
		int const ZoneNum, // Zone number
		Real64 & SumIntGain, // Zone sum of convective internal gains
		Real64 & SumHA, // Zone sum of Hc*Area
		Real64 & SumHATsurf, // Zone sum of Hc*Area*Tsurf
		Real64 & SumHATref, // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
		Real64 & SumMCp, // Zone sum of MassFlowRate*Cp
		Real64 & SumMCpT, // Zone sum of MassFlowRate*Cp*T
		Real64 & SumSysMCp, // Zone sum of air system MassFlowRate*Cp
		Real64 & SumSysMCpT, // Zone sum of air system MassFlowRate*Cp*T
		std::vector< int > const & controlledZoneEquipConfigNums // Precomputed controlled equip nums
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       Aug 2003, FCW: add SumHA contributions from window frame and divider
		//                      Aug 2003, CC: change how the reference temperatures are used
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the various sums that go into the zone heat balance
		// equation.  This replaces the SUMC, SUMHA, and SUMHAT calculations that were
		// previously done in various places throughout the program.
		// The SumHAT portion of the code is reproduced in RadiantSystemHighTemp and
		// RadiantSystemLowTemp and should be updated accordingly.
		// A reference temperature (Tref) is specified for use with the ceiling diffuser
		// convection correlation.  A bogus value of Tref = -999.9 defaults to using
		// the zone air (i.e. outlet) temperature for the reference temperature.
		// If Tref is applied to all surfaces, SumHA = 0, and SumHATref /= 0.
		// If Tref is not used at all, SumHATref = 0, and SumHA /= 0.
		// For future implementations, Tref can be easily converted into an array to
		// allow a different reference temperature to be specified for each surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipConfig;
		using ZonePlenum::ZoneRetPlenCond;
		using ZonePlenum::ZoneSupPlenCond;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;
		using DataDefineEquip::AirDistUnit;
		using InternalHeatGains::SumAllInternalConvectionGains;
		using InternalHeatGains::SumAllReturnAirConvectionGains;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 NodeTemp( 0.0 ); // System node temperature //Autodesk:Init Initialization added to elim poss of use uninitialized
		Real64 MassFlowRate; // System node mass flow rate
		int ZoneEquipConfigNum;
		bool ControlledZoneAirFlag;
		int ZoneRetPlenumNum;
		int ZoneSupPlenumNum;
		bool ZoneRetPlenumAirFlag;
		bool ZoneSupPlenumAirFlag;
		Real64 CpAir; // Specific heat of air
		int SurfNum; // Surface number
		Real64 HA; // Hc*Area
		Real64 Area; // Effective surface area
		Real64 RefAirTemp; // Reference air temperature for surface convection calculations
		Real64 ZoneMult;
		int ADUNum;
		int ADUInNode;
		int ADUOutNode;
		Real64 RetAirGain;

		// FLOW:
		SumIntGain = 0.0;
		SumHA = 0.0;
		SumHATsurf = 0.0;
		SumHATref = 0.0;
		SumMCp = 0.0;
		SumMCpT = 0.0;
		SumSysMCp = 0.0;
		SumSysMCpT = 0.0;

		// Sum all convective internal gains: SumIntGain

		SumAllInternalConvectionGains( ZoneNum, SumIntGain );
		SumIntGain += SumConvHTRadSys( ZoneNum ) + SumConvPool( ZoneNum );

		// Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
		// low or zero)
		if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
			SumAllReturnAirConvectionGains( ZoneNum, RetAirGain );
			SumIntGain += RetAirGain;
		}

		// Sum all non-system air flow, i.e. infiltration, simple ventilation, mixing, earth tube: SumMCp, SumMCpT
		SumMCp = MCPI( ZoneNum ) + MCPV( ZoneNum ) + MCPM( ZoneNum ) + MCPE( ZoneNum ) + MCPC( ZoneNum ) + MDotCPOA( ZoneNum );
		SumMCpT = MCPTI( ZoneNum ) + MCPTV( ZoneNum ) + MCPTM( ZoneNum ) + MCPTE( ZoneNum ) + MCPTC( ZoneNum ) + MDotCPOA( ZoneNum ) * Zone( ZoneNum ).OutDryBulbTemp;

		// Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
		if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS || ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated ) ) {
			// Multizone airflow calculated in AirflowNetwork
			SumMCp = AirflowNetworkExchangeData( ZoneNum ).SumMCp + AirflowNetworkExchangeData( ZoneNum ).SumMMCp;
			SumMCpT = AirflowNetworkExchangeData( ZoneNum ).SumMCpT + AirflowNetworkExchangeData( ZoneNum ).SumMMCpT;
		}

		// Sum all system air flow: SumSysMCp, SumSysMCpT
		// Check to see if this is a controlled zone

		ZoneEquipConfigNum = 0;
		ControlledZoneAirFlag = false;
		for ( std::vector< int >::size_type i = 0, e = controlledZoneEquipConfigNums.size(); i < e; ++i ) {
			if ( ZoneEquipConfig( controlledZoneEquipConfigNums[ i ] ).ActualZoneNum == ZoneNum ) {
				ZoneEquipConfigNum = controlledZoneEquipConfigNums[ i ];
				ControlledZoneAirFlag = true;
				break;
			}
		}

		// Check to see if this is a plenum zone
		// BG feb 2008 repeating this do loop every time seems crazy, store ControlledZoneAirFlag in Zone structure?
		ZoneRetPlenumAirFlag = false;
		for ( ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= NumZoneReturnPlenums; ++ZoneRetPlenumNum ) {
			if ( ZoneRetPlenCond( ZoneRetPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneRetPlenumAirFlag = true;
			break;
		} // ZoneRetPlenumNum
		ZoneSupPlenumAirFlag = false;
		for ( ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= NumZoneSupplyPlenums; ++ZoneSupPlenumNum ) {
			if ( ZoneSupPlenCond( ZoneSupPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneSupPlenumAirFlag = true;
			break;
		} // ZoneSupPlenumNum

		// Plenum and controlled zones have a different set of inlet nodes which must be calculated.
		if ( ControlledZoneAirFlag ) {
			auto const & zec( ZoneEquipConfig( ZoneEquipConfigNum ) );
			for ( int NodeNum = 1, NodeNum_end = zec.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum ) {
				// Get node conditions
				//  this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call?
				//  how can we tell?  predict step must be lagged ?  correct step, systems have run.
				auto const & node( Node( zec.InletNode( NodeNum ) ) );
				NodeTemp = node.Temp;
				MassFlowRate = node.MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );

				Real64 const MassFlowRate_CpAir( MassFlowRate * CpAir );
				SumSysMCp += MassFlowRate_CpAir;
				SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
			} // NodeNum

		} else if ( ZoneRetPlenumAirFlag ) {
			auto const & zrpc( ZoneRetPlenCond( ZoneRetPlenumNum ) );
			Real64 const air_hum_rat( ZoneAirHumRat( ZoneNum ) );
			for ( int NodeNum = 1, NodeNum_end = zrpc.NumInletNodes; NodeNum <= NodeNum_end; ++NodeNum ) {
				// Get node conditions
				auto const & node( Node( zrpc.InletNode( NodeNum ) ) );
				NodeTemp = node.Temp;
				MassFlowRate = node.MassFlowRate;
				CpAir = PsyCpAirFnWTdb( air_hum_rat, NodeTemp );

				Real64 const MassFlowRate_CpAir( MassFlowRate * CpAir );
				SumSysMCp += MassFlowRate_CpAir;
				SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
			} // NodeNum
			// add in the leaks
			for ( int ADUListIndex = 1, ADUListIndex_end = ZoneRetPlenCond( ZoneRetPlenumNum ).NumADUs; ADUListIndex <= ADUListIndex_end; ++ADUListIndex ) {
				ADUNum = ZoneRetPlenCond( ZoneRetPlenumNum ).ADUIndex( ADUListIndex );
				if ( AirDistUnit( ADUNum ).UpStreamLeak ) {
					ADUInNode = AirDistUnit( ADUNum ).InletNodeNum;
					NodeTemp = Node( ADUInNode ).Temp;
					MassFlowRate = AirDistUnit( ADUNum ).MassFlowRateUpStrLk;
					CpAir = PsyCpAirFnWTdb( air_hum_rat, NodeTemp );
					Real64 const MassFlowRate_CpAir( MassFlowRate * CpAir );
					SumSysMCp += MassFlowRate_CpAir;
					SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
				}
				if ( AirDistUnit( ADUNum ).DownStreamLeak ) {
					ADUOutNode = AirDistUnit( ADUNum ).OutletNodeNum;
					NodeTemp = Node( ADUOutNode ).Temp;
					MassFlowRate = AirDistUnit( ADUNum ).MassFlowRateDnStrLk;
					CpAir = PsyCpAirFnWTdb( air_hum_rat, NodeTemp );
					Real64 const MassFlowRate_CpAir( MassFlowRate * CpAir );
					SumSysMCp += MassFlowRate_CpAir;
					SumSysMCpT += MassFlowRate_CpAir * NodeTemp;
				}
			}

		} else if ( ZoneSupPlenumAirFlag ) {
			// Get node conditions
			NodeTemp = Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).Temp;
			MassFlowRate = Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).MassFlowRate;
			CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );

			SumSysMCp += MassFlowRate * CpAir;
			SumSysMCpT += MassFlowRate * CpAir * NodeTemp;

		}

		ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;

		SumSysMCp /= ZoneMult;
		SumSysMCpT /= ZoneMult;

		// Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {

			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			HA = 0.0;
			Area = Surface( SurfNum ).Area; // For windows, this is the glazing area

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
				auto const shading_flag( SurfaceWindow( SurfNum ).ShadingFlag );

				// Add to the convective internal gains
				if ( shading_flag == IntShadeOn || shading_flag == IntBlindOn ) {
					// The shade area covers the area of the glazing plus the area of the dividers.
					Area += SurfaceWindow( SurfNum ).DividerArea;
					// If interior shade or blind is present it is assumed that both the convective and IR radiative gain
					// from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
					// interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
					// at the same time that the interaction between glass and shade is calculated.
					SumIntGain += SurfaceWindow( SurfNum ).DividerConduction;
				}

				// Other convection term is applicable to equivalent layer window (ASHWAT) model
				if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) SumIntGain += SurfaceWindow( SurfNum ).OtherConvHeatGain;

				// Convective heat gain from natural convection in gap between glass and interior shade or blind
				if ( shading_flag == IntShadeOn || shading_flag == IntBlindOn ) SumIntGain += SurfaceWindow( SurfNum ).ConvHeatFlowNatural;

				// Convective heat gain from airflow window
				if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
					SumIntGain += SurfaceWindow( SurfNum ).ConvHeatGainToZoneAir;
					if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
						SumIntGain += SurfaceWindow( SurfNum ).RetHeatGainToZoneAir;
						WinHeatGain( SurfNum ) += SurfaceWindow( SurfNum ).RetHeatGainToZoneAir;
						if ( WinHeatGain( SurfNum ) >= 0.0 ) {
							WinHeatGainRep( SurfNum ) = WinHeatGain( SurfNum );
							WinHeatGainRepEnergy( SurfNum ) = WinHeatGainRep( SurfNum ) * TimeStepZoneSec;
						} else {
							WinHeatLossRep( SurfNum ) = -WinHeatGain( SurfNum );
							WinHeatLossRepEnergy( SurfNum ) = WinHeatLossRep( SurfNum ) * TimeStepZoneSec;
						}
					}
				}

				// Add to the surface convection sums
				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution
					Real64 const HA_surf( HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) );
					SumHATsurf += HA_surf * SurfaceWindow( SurfNum ).FrameTempSurfIn;
					HA += HA_surf;
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && shading_flag != IntShadeOn && shading_flag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					Real64 const HA_surf( HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) );
					SumHATsurf += HA_surf * SurfaceWindow( SurfNum ).DividerTempSurfIn;
					HA += HA_surf;
				}

			} // End of check if window

			HA += HConvIn( SurfNum ) * Area;
			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );

			// determine reference air temperature for this surface
			{ auto const SELECT_CASE_var( Surface( SurfNum ).TAirRef );
			if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
				// The zone air is the reference temperature (which is to be solved for in CorrectZoneAirTemp).
				RefAirTemp = MAT( ZoneNum );
				SumHA += HA;
			} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
				RefAirTemp = TempEffBulkAir( SurfNum );
				SumHATref += HA * RefAirTemp;
			} else if ( SELECT_CASE_var == ZoneSupplyAirTemp ) {
				// check whether this zone is a controlled zone or not
				if ( ! ControlledZoneAirFlag ) {
					ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
					return;
				}
				// determine supply air temperature as a weighted average of the inlet temperatures.
				if ( SumSysMCp > 0.0 ) {
					RefAirTemp = SumSysMCpT / SumSysMCp;
				} else {
					// no system flow (yet) so just use last value for inlet node temp, this can happen early in the environment
					RefAirTemp = NodeTemp;
				}
				SumHATref += HA * RefAirTemp;
			} else {
				// currently set to mean air temp but should add error warning here
				RefAirTemp = MAT( ZoneNum );
				SumHA += HA;
			}}

		} // SurfNum

	}

	void
	CalcZoneComponentLoadSums(
		int const ZoneNum, // Zone number
		Real64 const TempDepCoef, // Dependent coefficient
		Real64 const TempIndCoef, // Independent coefficient
		Real64 & SumIntGains, // Zone sum of convective internal gains
		Real64 & SumHADTsurfs, // Zone sum of Hc*Area*(Tsurf - Tz)
		Real64 & SumMCpDTzones, // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
		Real64 & SumMCpDtInfil, // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
		Real64 & SumMCpDTsystem, // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
		Real64 & SumNonAirSystem, // Zone sum of non air system convective heat gains
		Real64 & CzdTdt, // Zone air energy storage term.
		Real64 & imBalance, // put all terms in eq. 5 on RHS , should be zero
		std::vector< int > const & controlledZoneEquipConfigNums // Precomputed controlled equip nums
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Feb 2008
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the various sums that go into the zone heat balance
		// equation for reporting (and diagnostic) purposes only.
		// It was derived from CalcZoneSums but differs in that that routine
		// breaks up the component's dependence on zone air temp in order to *solve* for zone air temp,
		// but here we *use* the result for zone air temp and calculate the terms of the heat balance
		// Go back and calculate each of the 6 terms in Equation 5 and fill report variables.
		// notes on these raw terms for zone air heat balance model :
		//  these are state variables at the end of the last system timestep.
		//  they are not necessarily proper averages for what happend over entire zone time step
		//  these are not mulitplied by zone multipliers.
		//  The values are all Watts.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Equation 5 in Engineering Reference.

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataDefineEquip::AirDistUnit;
		using ZonePlenum::ZoneRetPlenCond;
		using ZonePlenum::ZoneSupPlenCond;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;
		using General::RoundSigDigits;
		using InternalHeatGains::SumAllInternalConvectionGains;
		using InternalHeatGains::SumAllReturnAirConvectionGains;
		using DirectAirManager::DirectAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NodeNum; // System node number
		Real64 NodeTemp( 0.0 ); // System node temperature //Autodesk:Init Initialization added to elim poss of use uninitialized
		Real64 MassFlowRate; // System node mass flow rate
		int ZoneEquipConfigNum;
		bool ControlledZoneAirFlag;
		int ZoneRetPlenumNum;
		int ZoneSupPlenumNum;
		bool ZoneRetPlenumAirFlag;
		bool ZoneSupPlenumAirFlag;
		Real64 RhoAir;
		Real64 CpAir; // Specific heat of air
		int SurfNum; // Surface number
		//unused  REAL(r64)           :: HA                    ! Hc*Area
		Real64 Area; // Effective surface area
		Real64 RefAirTemp; // Reference air temperature for surface convection calculations
		//unused  LOGICAL             :: FirstTimeFlag
		//unused  INTEGER             :: Tref           ! Used to check if reference air temp for all surfaces in the zone are the same
		//unused  REAL(r64)           :: ZoneMult
		int ADUListIndex;
		int ADUNum;
		int ADUInNode;
		int ADUOutNode;
		int SDUNum;
		Real64 SumSysMCp;
		Real64 SumSysMCpT;
		Real64 Threshold;
		Real64 SumRetAirGains;
		Real64 ADUHeatAddRate;
		Real64 SDUHeatAddRate;

		SumIntGains = 0.0; // Zone sum of convective internal gains
		SumHADTsurfs = 0.0; // Zone sum of Hc*Area*(Tsurf - Tz)
		SumMCpDTzones = 0.0; // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
		SumMCpDtInfil = 0.0; // Zone sum of MassFlowRate*Cp*(Tout - Tz)
		SumMCpDTsystem = 0.0; // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
		SumNonAirSystem = 0.0;
		CzdTdt = 0.0;
		imBalance = 0.0;
		SumSysMCp = 0.0;
		SumSysMCpT = 0.0;
		ADUHeatAddRate = 0.0;
		SDUHeatAddRate = 0.0;
		ADUNum = 0;
		SDUNum = 0;

		// Sum all convective internal gains: SumIntGain
		SumAllInternalConvectionGains( ZoneNum, SumIntGains );

		// Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
		// low or zero)
		if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
			SumAllReturnAirConvectionGains( ZoneNum, SumRetAirGains );
			SumIntGains += SumRetAirGains;
		}

		// sum non-system air flow transfers between zones
		SumMCpDTzones = MCPTM( ZoneNum ) - MCPM( ZoneNum ) * MAT( ZoneNum ); // but maybe it should be ZTAV(ZoneNum)

		// Sum non-system air flow, i.e. infiltration, simple ventilation, earth tube
		//  reuse SumMCp, SumMCpT from CalcZoneSum but use MAT (or maybe ZTAV?) to complete
		SumMCpDtInfil = ( MCPTI( ZoneNum ) - MCPI( ZoneNum ) * MAT( ZoneNum ) ) + ( MCPTV( ZoneNum ) - MCPV( ZoneNum ) * MAT( ZoneNum ) ) + ( MCPTE( ZoneNum ) - MCPE( ZoneNum ) * MAT( ZoneNum ) ) + ( MCPTC( ZoneNum ) - MCPC( ZoneNum ) * MAT( ZoneNum ) ) + ( MDotCPOA( ZoneNum ) * Zone( ZoneNum ).OutDryBulbTemp - MDotCPOA( ZoneNum ) * MAT( ZoneNum ) ); // infiltration | Ventilation (simple) | Earth tube. | Cooltower | combined OA flow

		// Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model (if used)
		if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS || ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated ) ) {
			// Multizone airflow calculated in AirflowNetwork
			SumMCpDtInfil = AirflowNetworkExchangeData( ZoneNum ).SumMCpT - AirflowNetworkExchangeData( ZoneNum ).SumMCp * MAT( ZoneNum );
			SumMCpDTzones = AirflowNetworkExchangeData( ZoneNum ).SumMMCpT - AirflowNetworkExchangeData( ZoneNum ).SumMMCp * MAT( ZoneNum );
		}

		// Sum all system air flow: reusing how SumSysMCp, SumSysMCpT are calculated in CalcZoneSums
		// Check to see if this is a controlled zone

		// CR 7384 continuation needed below.  eliminate do loop for speed and clarity
		ZoneEquipConfigNum = 0;
		ControlledZoneAirFlag = false;
		for ( std::vector< int >::size_type i = 0, e = controlledZoneEquipConfigNums.size(); i < e; ++i ) {
			if ( ZoneEquipConfig( controlledZoneEquipConfigNums[ i ] ).ActualZoneNum == ZoneNum ) {
				ZoneEquipConfigNum = controlledZoneEquipConfigNums[ i ];
				ControlledZoneAirFlag = true;
				break;
			}
		}

		// Check to see if this is a plenum zone
		ZoneRetPlenumAirFlag = false;
		for ( ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= NumZoneReturnPlenums; ++ZoneRetPlenumNum ) {
			if ( ZoneRetPlenCond( ZoneRetPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneRetPlenumAirFlag = true;
			break;
		} // ZoneRetPlenumNum
		ZoneSupPlenumAirFlag = false;
		for ( ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= NumZoneSupplyPlenums; ++ZoneSupPlenumNum ) {
			if ( ZoneSupPlenCond( ZoneSupPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneSupPlenumAirFlag = true;
			break;
		} // ZoneSupPlenumNum

		// Plenum and controlled zones have a different set of inlet nodes which must be calculated.
		if ( ControlledZoneAirFlag ) {
			for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
				// Get node conditions
				NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
				MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );

				SumMCpDTsystem += MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );

			} // NodeNum

			if ( ZoneEquipConfig( ZoneEquipConfigNum ).ADUNum > 0 ) {
				ADUNum = ZoneEquipConfig( ZoneEquipConfigNum ).ADUNum;
				NodeTemp = Node( AirDistUnit( ADUNum ).OutletNodeNum ).Temp;
				MassFlowRate = Node( AirDistUnit( ADUNum ).OutletNodeNum ).MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
				ADUHeatAddRate = MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );
				AirDistUnit( ADUNum ).HeatRate = max( 0.0, ADUHeatAddRate );
				AirDistUnit( ADUNum ).CoolRate = std::abs( min( 0.0, ADUHeatAddRate ) );
				AirDistUnit( ADUNum ).HeatGain = AirDistUnit( ADUNum ).HeatRate * TimeStepSys * SecInHour;
				AirDistUnit( ADUNum ).CoolGain = AirDistUnit( ADUNum ).CoolRate * TimeStepSys * SecInHour;
			}

			if ( ZoneEquipConfig( ZoneEquipConfigNum ).SDUNum > 0 ) {
				SDUNum = ZoneEquipConfig( ZoneEquipConfigNum ).SDUNum;
				NodeTemp = Node( DirectAir( SDUNum ).ZoneSupplyAirNode ).Temp;
				MassFlowRate = Node( DirectAir( SDUNum ).ZoneSupplyAirNode ).MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
				SDUHeatAddRate = MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );
				DirectAir( SDUNum ).HeatRate = max( SDUHeatAddRate, 0.0 );
				DirectAir( SDUNum ).CoolRate = std::abs( min( SDUHeatAddRate, 0.0 ) );
				DirectAir( SDUNum ).HeatEnergy = DirectAir( SDUNum ).HeatRate * TimeStepSys * SecInHour;
				DirectAir( SDUNum ).CoolEnergy = DirectAir( SDUNum ).CoolRate * TimeStepSys * SecInHour;
			}

		} else if ( ZoneRetPlenumAirFlag ) {
			for ( NodeNum = 1; NodeNum <= ZoneRetPlenCond( ZoneRetPlenumNum ).NumInletNodes; ++NodeNum ) {
				// Get node conditions
				NodeTemp = Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).Temp;
				MassFlowRate = Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );

				SumMCpDTsystem += MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );

			} // NodeNum
			// add in the leaks
			for ( ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond( ZoneRetPlenumNum ).NumADUs; ++ADUListIndex ) {
				ADUNum = ZoneRetPlenCond( ZoneRetPlenumNum ).ADUIndex( ADUListIndex );
				if ( AirDistUnit( ADUNum ).UpStreamLeak ) {
					ADUInNode = AirDistUnit( ADUNum ).InletNodeNum;
					NodeTemp = Node( ADUInNode ).Temp;
					MassFlowRate = AirDistUnit( ADUNum ).MassFlowRateUpStrLk;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
					SumMCpDTsystem += MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );
				}
				if ( AirDistUnit( ADUNum ).DownStreamLeak ) {
					ADUOutNode = AirDistUnit( ADUNum ).OutletNodeNum;
					NodeTemp = Node( ADUOutNode ).Temp;
					MassFlowRate = AirDistUnit( ADUNum ).MassFlowRateDnStrLk;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
					SumMCpDTsystem += MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );
				}
			}

		} else if ( ZoneSupPlenumAirFlag ) {
			// Get node conditions
			NodeTemp = Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).Temp;
			MassFlowRate = Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).MassFlowRate;
			CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );

			SumMCpDTsystem += MassFlowRate * CpAir * ( NodeTemp - MAT( ZoneNum ) );

		}

		// non air system response.
		SumNonAirSystem = NonAirSystemResponse( ZoneNum ) + SumConvHTRadSys( ZoneNum ) + SumConvPool( ZoneNum );

		// Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {

			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			Area = Surface( SurfNum ).Area; // For windows, this is the glazing area
			// determine reference air temperature for this surface's convective heat transfer model
			{ auto const SELECT_CASE_var( Surface( SurfNum ).TAirRef );
			if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
				// The zone air is the reference temperature
				RefAirTemp = MAT( ZoneNum );
			} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
				RefAirTemp = TempEffBulkAir( SurfNum );
			} else if ( SELECT_CASE_var == ZoneSupplyAirTemp ) {
				// check whether this zone is a controlled zone or not
				if ( ! ControlledZoneAirFlag ) {
					ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
					return;
				}
				// determine supply air temperature as a weighted average of the inlet temperatures.
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
					// Get node conditions
					NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
					MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );

					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;

				} // NodeNum
				if ( SumSysMCp > 0.0 ) {
					RefAirTemp = SumSysMCpT / SumSysMCp;
				} else {
					// no system flow (yet) so just use last value for inlet node temp, this can happen early in the environment
					RefAirTemp = NodeTemp;
				}

			} else {
				// currently set to mean air temp but should add error warning here
				RefAirTemp = MAT( ZoneNum );

			}}

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {

				// Add to the convective internal gains
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The shade area covers the area of the glazing plus the area of the dividers.
					Area += SurfaceWindow( SurfNum ).DividerArea;
					// If interior shade or blind is present it is assumed that both the convective and IR radiative gain
					// from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
					// interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
					// at the same time that the interaction between glass and shade is calculated.
					SumIntGains += SurfaceWindow( SurfNum ).DividerConduction;
				}

				// Other convection term is applicable to equivalent layer window (ASHWAT) model
				if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) SumIntGains += SurfaceWindow( SurfNum ).OtherConvHeatGain;

				// Convective heat gain from natural convection in gap between glass and interior shade or blind
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) SumIntGains += SurfaceWindow( SurfNum ).ConvHeatFlowNatural;

				// Convective heat gain from airflow window
				if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
					SumIntGains += SurfaceWindow( SurfNum ).ConvHeatGainToZoneAir;
					if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
						SumIntGains += SurfaceWindow( SurfNum ).RetHeatGainToZoneAir;
					}
				}

				// Add to the surface convection sums
				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution

					SumHADTsurfs += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * ( SurfaceWindow( SurfNum ).FrameTempSurfIn - RefAirTemp );

				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					SumHADTsurfs += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * ( SurfaceWindow( SurfNum ).DividerTempSurfIn - RefAirTemp );

				}

			} // End of check if window

			SumHADTsurfs += HConvIn( SurfNum ) * Area * ( TempSurfInTmp( SurfNum ) - RefAirTemp );

		} // SurfNum

		// now calculate air energy storage source term.
		// capacitance is volume * density * heat capacity
		CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MAT( ZoneNum ) );
		RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ) );

		{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
		if ( SELECT_CASE_var == Use3rdOrder ) {
			CzdTdt = RhoAir * CpAir * Zone( ZoneNum ).Volume * ZoneVolCapMultpSens * ( MAT( ZoneNum ) - ZTM1( ZoneNum ) ) / ( TimeStepSys * SecInHour );
			// Exact solution
		} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
			CzdTdt = TempIndCoef - TempDepCoef * MAT( ZoneNum );
		} else if ( SELECT_CASE_var == UseEulerMethod ) {
			CzdTdt = AIRRAT( ZoneNum ) * ( MAT( ZoneNum ) - ZoneT1( ZoneNum ) );
		}}

		if ( DisplayZoneAirHeatBalanceOffBalance ) {
			imBalance = SumIntGains + SumHADTsurfs + SumMCpDTzones + SumMCpDtInfil + SumMCpDTsystem + SumNonAirSystem - CzdTdt;

			// throw warning if seriously out of balance (this may need to be removed if too noisy... )
			// formulate dynamic threshold value based on 20% of quadrature sum of components
			Threshold = 0.2 * std::sqrt( pow_2( SumIntGains ) + pow_2( SumHADTsurfs ) + pow_2( SumMCpDTzones ) + pow_2( SumMCpDtInfil ) + pow_2( SumMCpDTsystem ) + pow_2( SumNonAirSystem ) + pow_2( CzdTdt ) );
			if ( ( std::abs( imBalance ) > Threshold ) && ( ! WarmupFlag ) && ( ! DoingSizing ) ) { // air balance is out by more than threshold
				if ( Zone( ZoneNum ).AirHBimBalanceErrIndex == 0 ) {
					ShowWarningMessage( "Zone Air Heat Balance is out of balance for zone named " + Zone( ZoneNum ).Name );
					ShowContinueError( "Zone Air Heat Balance Deviation Rate is more than " + RoundSigDigits( Threshold, 1 ) + " {W}" );
					if ( TurnFansOn ) {
						ShowContinueError( "Night cycle fan operation may be causing above error" );
					}

					ShowContinueErrorTimeStamp( " Occurrence info:" );
				}
				ShowRecurringWarningErrorAtEnd( "Zone Air Heat Balance is out of balance ... zone named " + Zone( ZoneNum ).Name, Zone( ZoneNum ).AirHBimBalanceErrIndex, std::abs( imBalance ) - Threshold, std::abs( imBalance ) - Threshold, _, "{W}", "{W}" );
			}
		}

	}

	bool
	VerifyThermostatInZone( std::string const & ZoneName ) // Zone to verify
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Feb 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function verifies that a zone (by name) has a Zone Control:Thermostatic
		// object entered.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		bool HasThermostat; // True if does, false if not.

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

		if ( GetZoneAirStatsInputFlag ) {
			GetZoneAirSetPoints();
			GetZoneAirStatsInputFlag = false;
		}
		if ( NumTempControlledZones > 0 ) {
			if ( FindItemInList( ZoneName, TempControlledZone, &ZoneTempControls::ZoneName ) > 0 ) {
				HasThermostat = true;
			} else {
				HasThermostat = false;
			}
		} else {
			HasThermostat = false;
		}
		return HasThermostat;

	}

	bool
	VerifyControlledZoneForThermostat( std::string const & ZoneName ) // Zone to verify
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Mar 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function verifies that a zone (by name) has a ZoneHVAC:EquipmentConnections
		// object entered.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::EquipConfiguration;

		// Return value

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

		return ( FindItemInList( ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName ) > 0 );
	}

	void
	DetectOscillatingZoneTemp()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Oscillating temperatures between HVAC timesteps indicate that the
		// simulation may be poor. Code is trying to be fast since the purpose
		// is to see the impact on oscillating by trying longer time steps in
		// an attempt to speed up the simulation.
		// Note that the OscillateMagnitude threshold must be less than
		// MaxZoneTempDiff since ManageHVAC keeps shortening the timestep
		// until that is reached unless it goes to less than the
		// MinTimeStepSys.

		// METHODOLOGY EMPLOYED:
		// na

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
		int iZone;
		Real64 NegOscillateMagnitude;
		bool isOscillate;
		Real64 Diff12;
		Real64 Diff23;
		Real64 Diff34;
		/////////// hoisted into namespace ////////////
		//static bool SetupOscillationOutputFlag( true );
		/////////////////////////////////////////////////
		bool isAnyZoneOscillating;

		//first time run allocate arrays and setup output variable
		if ( SetupOscillationOutputFlag ) {
			ZoneTempHist.allocate( 4, NumOfZones );
			ZoneTempHist = 0.0;
			ZoneTempOscillate.dimension( NumOfZones, 0.0 );
			//set up zone by zone variables
			// CurrentModuleObject='Zone'
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				SetupOutputVariable( "Zone Oscillating Temperatures Time [hr]", ZoneTempOscillate( iZone ), "System", "Sum", Zone( iZone ).Name );
			}
			//set up a variable covering all zones
			SetupOutputVariable( "Facility Any Zone Oscillating Temperatures Time [hr]", AnyZoneTempOscillate, "System", "Sum", "Facility" );
			SetupOscillationOutputFlag = false;
		}
		//precalc the negative value for performance
		NegOscillateMagnitude = -OscillateMagnitude;
		//assume no zone is oscillating
		isAnyZoneOscillating = false;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			isOscillate = false;
			ZoneTempHist( 4, iZone ) = ZoneTempHist( 3, iZone );
			ZoneTempHist( 3, iZone ) = ZoneTempHist( 2, iZone );
			ZoneTempHist( 2, iZone ) = ZoneTempHist( 1, iZone );
			ZoneTempHist( 1, iZone ) = ZT( iZone );
			Diff34 = ZoneTempHist( 3, iZone ) - ZoneTempHist( 4, iZone );
			Diff23 = ZoneTempHist( 2, iZone ) - ZoneTempHist( 3, iZone );
			Diff12 = ZoneTempHist( 1, iZone ) - ZoneTempHist( 2, iZone );
			// roll out the conditionals for increased performance
			if ( Diff12 > OscillateMagnitude ) {
				if ( Diff23 < NegOscillateMagnitude ) {
					if ( Diff34 > OscillateMagnitude ) {
						isOscillate = true;
					}
				}
			}
			// now try the opposite sequence of swings
			if ( Diff12 < NegOscillateMagnitude ) {
				if ( Diff23 > OscillateMagnitude ) {
					if ( Diff34 < NegOscillateMagnitude ) {
						isOscillate = true;
					}
				}
			}
			if ( isOscillate ) {
				ZoneTempOscillate( iZone ) = TimeStepSys;
				isAnyZoneOscillating = true;
			} else {
				ZoneTempOscillate( iZone ) = 0.0;
			}
		}
		//any zone variable
		if ( isAnyZoneOscillating ) {
			AnyZoneTempOscillate = TimeStepSys;
		} else {
			AnyZoneTempOscillate = 0.0;
		}
	}

	void
	AdjustAirSetPointsforOpTempCntrl(
		int const TempControlledZoneID,
		int const ActualZoneNum,
		Real64 & ZoneAirSetPoint
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine modifies the air temperature setpoint to effect operative temperature control

		// METHODOLOGY EMPLOYED:
		// pass in data and alter setpoint if needed

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::MRT;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 thisMRT; // local variable for mean radiant temperature in this zone
		Real64 thisMRTFraction; // local variable for fraction that MRT is in Op Temp definition

		if ( ! ( AnyOpTempControl ) ) return; // do nothing to setpoint

		if ( ! ( TempControlledZone( TempControlledZoneID ).OperativeTempControl ) ) return; // do nothing to setpoint

		// is operative temp radiative fraction scheduled or fixed?
		if ( TempControlledZone( TempControlledZoneID ).OpTempCntrlModeScheduled ) {
			thisMRTFraction = GetCurrentScheduleValue( TempControlledZone( TempControlledZoneID ).OpTempRadiativeFractionSched );
		} else {
			thisMRTFraction = TempControlledZone( TempControlledZoneID ).FixedRadiativeFraction;
		}

		// get mean radiant temperature for zone
		thisMRT = MRT( ActualZoneNum );

		// modify setpoint for operative temperature control
		//  traping for MRT fractions between 0.0 and 0.9 during get input, so shouldn't be able to divide by zero here.
		ZoneAirSetPoint = ( ZoneAirSetPoint - thisMRTFraction * thisMRT ) / ( 1.0 - thisMRTFraction );

	}

	void
	CalcZoneAirComfortSetPoints()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   May 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine sets the thermal comfort setpoints for each controlled zone based on air tempeature
		// obtained from thermal comfort models.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using General::TrimSigDigits;
		using ThermalComfort::ManageThermalComfort;

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
		int RelativeZoneNum;
		int ActualZoneNum;
		int ComfortControlSchedIndex;
		int SetPointComfortSchedIndex;
		int SetPointComfortSchedIndexHot;
		int SetPointComfortSchedIndexCold;
		int SchedNameIndex;
		int SchedTypeIndex;
		int PeopleNum;
		int ObjectCount;
		Real64 PeopleCount;
		Real64 SetPointLo;
		Real64 SetPointHi;
		Real64 NumberOccupants;
		Real64 Tset;

		static bool FirstTimeFlag( true ); // Flag set to make sure you get input once

		// FLOW:
		// Call thermal comfort module to read zone control comfort object
		if ( FirstTimeFlag ) {
			ManageThermalComfort( true );
			FirstTimeFlag = false;
		}

		ComfortControlType = 0; // Default

		for ( RelativeZoneNum = 1; RelativeZoneNum <= NumComfortControlledZones; ++RelativeZoneNum ) {

			ActualZoneNum = ComfortControlledZone( RelativeZoneNum ).ActualZoneNum;
			ComfortControlSchedIndex = ComfortControlledZone( RelativeZoneNum ).ComfortSchedIndex;
			ComfortControlType( ActualZoneNum ) = GetCurrentScheduleValue( ComfortControlSchedIndex );

			// Get PMV values

			{ auto const SELECT_CASE_var( ComfortControlType( ActualZoneNum ) ); // Is this missing the possibility of sometimes having no control on a zone
			// during the simulation?
			if ( SELECT_CASE_var == 0 ) { // Uncontrolled for thermal comfort
				ZoneComfortControlsFanger( ActualZoneNum ).LowPMV = -999.0;
				ZoneComfortControlsFanger( ActualZoneNum ).HighPMV = -999.0;

			} else if ( SELECT_CASE_var == SglHeatSetPointFanger ) {

				SchedNameIndex = ComfortControlledZone( RelativeZoneNum ).SchIndx_SglHeatSetPointFanger;
				SchedTypeIndex = ComfortControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );
				SetPointComfortSchedIndex = SetPointSingleHeatingFanger( SchedTypeIndex ).PMVSchedIndex;
				ZoneComfortControlsFanger( ActualZoneNum ).FangerType = SglHeatSetPointFanger;
				ZoneComfortControlsFanger( ActualZoneNum ).LowPMV = GetCurrentScheduleValue( SetPointComfortSchedIndex );
				ZoneComfortControlsFanger( ActualZoneNum ).HighPMV = -999.0;

			} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {

				SchedNameIndex = ComfortControlledZone( RelativeZoneNum ).SchIndx_SglCoolSetPointFanger;
				SchedTypeIndex = ComfortControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );
				SetPointComfortSchedIndex = SetPointSingleCoolingFanger( SchedTypeIndex ).PMVSchedIndex;
				ZoneComfortControlsFanger( ActualZoneNum ).FangerType = SglCoolSetPointFanger;
				ZoneComfortControlsFanger( ActualZoneNum ).LowPMV = -999.0;
				ZoneComfortControlsFanger( ActualZoneNum ).HighPMV = GetCurrentScheduleValue( SetPointComfortSchedIndex );

			} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {

				SchedNameIndex = ComfortControlledZone( RelativeZoneNum ).SchIndx_SglHCSetPointFanger;
				SchedTypeIndex = ComfortControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );
				SetPointComfortSchedIndex = SetPointSingleHeatCoolFanger( SchedTypeIndex ).PMVSchedIndex;
				ZoneComfortControlsFanger( ActualZoneNum ).FangerType = SglHCSetPointFanger;
				ZoneComfortControlsFanger( ActualZoneNum ).LowPMV = GetCurrentScheduleValue( SetPointComfortSchedIndex );
				ZoneComfortControlsFanger( ActualZoneNum ).HighPMV = GetCurrentScheduleValue( SetPointComfortSchedIndex );

			} else if ( SELECT_CASE_var == DualSetPointFanger ) {

				SchedNameIndex = ComfortControlledZone( RelativeZoneNum ).SchIndx_DualSetPointFanger;
				SchedTypeIndex = ComfortControlledZone( RelativeZoneNum ).ControlTypeSchIndx( SchedNameIndex );
				SetPointComfortSchedIndexHot = SetPointDualHeatCoolFanger( SchedTypeIndex ).HeatPMVSchedIndex;
				SetPointComfortSchedIndexCold = SetPointDualHeatCoolFanger( SchedTypeIndex ).CoolPMVSchedIndex;
				ZoneComfortControlsFanger( ActualZoneNum ).FangerType = DualSetPointFanger;
				ZoneComfortControlsFanger( ActualZoneNum ).LowPMV = GetCurrentScheduleValue( SetPointComfortSchedIndexHot );
				ZoneComfortControlsFanger( ActualZoneNum ).HighPMV = GetCurrentScheduleValue( SetPointComfortSchedIndexCold );
				if ( ZoneComfortControlsFanger( ActualZoneNum ).LowPMV > ZoneComfortControlsFanger( ActualZoneNum ).HighPMV ) {
					++ZoneComfortControlsFanger( ActualZoneNum ).DualPMVErrCount;
					if ( ZoneComfortControlsFanger( ActualZoneNum ).DualPMVErrCount < 2 ) {
						ShowWarningError( "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint: The heating PMV setpoint is above the cooling PMV setpoint in " + SetPointDualHeatCoolFanger( SchedTypeIndex ).Name );
						ShowContinueError( "The zone dual heating PMV setpoint is set to the dual cooling PMV setpoint." );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The heating PMV setpoint is still above the cooling PMV setpoint", ZoneComfortControlsFanger( ActualZoneNum ).DualPMVErrIndex, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV );
					}
					ZoneComfortControlsFanger( ActualZoneNum ).LowPMV = ZoneComfortControlsFanger( ActualZoneNum ).HighPMV;
				}

			} else {
				ShowSevereError( "CalcZoneAirTempSetpoints: Illegal thermal control control type for Zone=" + Zone( ActualZoneNum ).Name + ", Found value=" + TrimSigDigits( ComfortControlType( ActualZoneNum ) ) + ", in Schedule=" + ComfortControlledZone( RelativeZoneNum ).ControlTypeSchedName );

			}}

			// Check Average method
			{ auto const SELECT_CASE_var( ComfortControlledZone( RelativeZoneNum ).AverageMethodNum );
			if ( SELECT_CASE_var == AverageMethodNum_NO ) {
				PeopleNum = ComfortControlledZone( RelativeZoneNum ).SpecificObjectNum;
				if ( ComfortControlType( ActualZoneNum ) == SglCoolSetPointFanger ) {
					GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, SetPointLo );
				} else {
					GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV, SetPointLo );
				}
				if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, SetPointHi );
			} else if ( SELECT_CASE_var == AverageMethodNum_SPE ) {
				PeopleNum = ComfortControlledZone( RelativeZoneNum ).SpecificObjectNum;
				if ( ComfortControlType( ActualZoneNum ) == SglCoolSetPointFanger ) {
					GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, SetPointLo );
				} else {
					GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV, SetPointLo );
				}
				if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, SetPointHi );
			} else if ( SELECT_CASE_var == AverageMethodNum_OBJ ) {
				ObjectCount = 0;
				SetPointLo = 0.0;
				SetPointHi = 0.0;
				for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
					if ( ActualZoneNum == People( PeopleNum ).ZonePtr ) {
						++ObjectCount;
						GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV, Tset );
						SetPointLo += Tset;
						if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) {
							GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, Tset );
							SetPointHi += Tset;
						}
					}
				}
				SetPointLo /= ObjectCount;
				if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) SetPointHi /= ObjectCount;
			} else if ( SELECT_CASE_var == AverageMethodNum_PEO ) {
				PeopleCount = 0.0;
				SetPointLo = 0.0;
				SetPointHi = 0.0;
				for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
					if ( ActualZoneNum == People( PeopleNum ).ZonePtr ) {
						NumberOccupants = People( PeopleNum ).NumberOfPeople * GetCurrentScheduleValue( People( PeopleNum ).NumberOfPeoplePtr );
						PeopleCount += NumberOccupants;
						GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV, Tset );
						SetPointLo += Tset * NumberOccupants;
						if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) {
							GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, Tset );
							SetPointHi += Tset * NumberOccupants;
						}
					}
				}
				if ( PeopleCount > 0 ) {
					SetPointLo /= PeopleCount;
					if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) SetPointHi /= PeopleCount;
				} else {
					// reccurring warnings
					//          ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount = &
					//                                           ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount + 1
					if ( ComfortControlledZone( RelativeZoneNum ).PeopleAverageErrIndex == 0 ) {
						ShowWarningMessage( "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " + Zone( ActualZoneNum ).Name + " is zero. The People Average option is not used." );
						ShowContinueError( "The Object Average option is used instead. Simulation continues ....." );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( "ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = " + Zone( ActualZoneNum ).Name + " is still zero. The People Average option is not used", ComfortControlledZone( RelativeZoneNum ).PeopleAverageErrIndex, PeopleCount, PeopleCount );
					ObjectCount = 0;
					SetPointLo = 0.0;
					SetPointHi = 0.0;
					for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
						if ( ActualZoneNum == People( PeopleNum ).ZonePtr ) {
							++ObjectCount;
							GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).LowPMV, Tset );
							SetPointLo += Tset;
							if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) {
								GetComfortSetPoints( PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger( ActualZoneNum ).HighPMV, Tset );
								SetPointHi += Tset;
							}
						}
					}
					SetPointLo /= ObjectCount;
					if ( ComfortControlType( ActualZoneNum ) == DualSetPointFanger ) SetPointHi /= ObjectCount;
				}
			}}

			// Assign setpoint
			{ auto const SELECT_CASE_var( ComfortControlType( ActualZoneNum ) ); // Is this missing the possibility of sometimes having no control on a zone
			// during the simulation?
			if ( SELECT_CASE_var == 0 ) { // Uncontrolled for thermal comfort
				{ auto const SELECT_CASE_var1( TempControlType( ActualZoneNum ) );
				if ( SELECT_CASE_var1 == SingleHeatingSetPoint ) {
					ZoneThermostatSetPointHi( ActualZoneNum ) = 0.0;
				} else if ( SELECT_CASE_var1 == SingleCoolingSetPoint ) {
					ZoneThermostatSetPointLo( ActualZoneNum ) = 0.0;
				}}

			} else if ( SELECT_CASE_var == SglHeatSetPointFanger ) {
				if ( SetPointLo < ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint ) {
					SetPointLo = ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint;
					//          ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount + 1
					if ( ComfortControlledZone( RelativeZoneNum ).TdbMinErrIndex < 2 ) {
						ShowWarningMessage( "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is below the Minimum dry-bulb temperature setpoint " + ComfortControlledZone( RelativeZoneNum ).Name );
						ShowContinueError( "The zone heating setpoint is set to the Minimum dry-bulb temperature setpoint" );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is still below the Minimum dry-bulb temperature setpoint ...", ComfortControlledZone( RelativeZoneNum ).TdbMinErrIndex, SetPointLo, SetPointLo );
				}
				TempZoneThermostatSetPoint( ActualZoneNum ) = SetPointLo;
				ZoneThermostatSetPointLo( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				TempControlType( ActualZoneNum ) = SingleHeatingSetPoint;

			} else if ( SELECT_CASE_var == SglCoolSetPointFanger ) {

				if ( SetPointLo > ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint ) {
					SetPointLo = ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint;
					//          ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount + 1
					if ( ComfortControlledZone( RelativeZoneNum ).TdbMaxErrIndex == 0 ) {
						ShowWarningMessage( "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is above the Maximum dry-bulb temperature setpoint " + ComfortControlledZone( RelativeZoneNum ).Name );
						ShowContinueError( "The zone cooling setpoint is set to the Maximum dry-bulb temperature setpoint" );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is still above the Maximum dry-bulb temperature setpoint ...", ComfortControlledZone( RelativeZoneNum ).TdbMaxErrIndex, SetPointLo, SetPointLo );
				}
				TempZoneThermostatSetPoint( ActualZoneNum ) = SetPointLo;
				ZoneThermostatSetPointHi( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				TempControlType( ActualZoneNum ) = SingleCoolingSetPoint;

			} else if ( SELECT_CASE_var == SglHCSetPointFanger ) {

				if ( ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint == ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint ) {
					SetPointLo = ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint;
				}
				if ( SetPointLo > ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint ) SetPointLo = ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint;
				if ( SetPointLo < ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint ) SetPointLo = ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint;
				if ( SetPointLo < ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint || SetPointLo > ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint ) {
					//          ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount = ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount + 1
					if ( ComfortControlledZone( RelativeZoneNum ).TdbHCErrIndex == 0 ) {
						ShowWarningMessage( "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is above the Maximum or below the Minimum dry-bulb temperature setpoint " + ComfortControlledZone( RelativeZoneNum ).Name );
						ShowContinueError( "The zone setpoint is set to the Maximum dry-bulb temperature setpoint if above or the Minimum dry-bulb temperature setpoint if below" );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is still beyond the range between Maximum and Minimum dry-bulb temperature setpoint ...", ComfortControlledZone( RelativeZoneNum ).TdbHCErrIndex, SetPointLo, SetPointLo );
				}
				TempZoneThermostatSetPoint( ActualZoneNum ) = SetPointLo;
				ZoneThermostatSetPointHi( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				ZoneThermostatSetPointLo( ActualZoneNum ) = TempZoneThermostatSetPoint( ActualZoneNum );
				TempControlType( ActualZoneNum ) = SingleHeatCoolSetPoint;

			} else if ( SELECT_CASE_var == DualSetPointFanger ) {

				if ( SetPointLo < ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint ) {
					SetPointLo = ComfortControlledZone( RelativeZoneNum ).TdbMinSetPoint;
					//          ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount = ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount+1
					if ( ComfortControlledZone( RelativeZoneNum ).TdbDualMinErrIndex == 0 ) {
						ShowWarningMessage( "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is below the Minimum dry-bulb temperature setpoint " + ComfortControlledZone( RelativeZoneNum ).Name );
						ShowContinueError( "The zone dual heating setpoint is set to the Minimum dry-bulb temperature setpoint" );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still below the Minimum dry-bulb temperature setpoint ...", ComfortControlledZone( RelativeZoneNum ).TdbDualMinErrIndex, SetPointLo, SetPointLo );
				}
				if ( SetPointHi > ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint ) {
					SetPointHi = ComfortControlledZone( RelativeZoneNum ).TdbMaxSetPoint;
					//          ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount = ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount + 1
					if ( ComfortControlledZone( RelativeZoneNum ).TdbDualMaxErrIndex == 0 ) {
						ShowWarningMessage( "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is above the Maximum dry-bulb temperature setpoint " + ComfortControlledZone( RelativeZoneNum ).Name );
						ShowContinueError( "The zone dual cooling setpoint is set to the Maximum dry-bulb temperature setpoint" );
						ShowContinueErrorTimeStamp( "Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still above the Maximum dry-bulb temperature setpoint ...", ComfortControlledZone( RelativeZoneNum ).TdbDualMaxErrIndex, SetPointLo, SetPointLo );
				}

				ZoneThermostatSetPointLo( ActualZoneNum ) = SetPointLo;
				ZoneThermostatSetPointHi( ActualZoneNum ) = SetPointHi;
				TempControlType( ActualZoneNum ) = DualSetPointWithDeadBand;

			} else {
				ShowSevereError( "CalcZoneAirComfortSetpoints: Illegal thermal control control type for Zone=" + Zone( ActualZoneNum ).Name + ", Found value=" + TrimSigDigits( ComfortControlType( ActualZoneNum ) ) + ", in Schedule=" + ComfortControlledZone( ActualZoneNum ).ControlTypeSchedName );

			}}

		}

	}

	void
	GetComfortSetPoints(
		int const PeopleNum,
		int const ComfortControlNum,
		Real64 const PMVSet,
		Real64 & Tset // drybulb setpoint temperature for a given PMV value
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   May, 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:

		// This routine sets what the thermal comfort setpoints for each controlled zone should be based on air tempeature
		// obtained from thermal comfort models.
		// This is called each time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using ThermalComfort::CalcThermalComfortFanger;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// 0 = Solution; 1 = Set to Min; 2 Set to Max

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Acc( 0.001 ); // accuracy control for SolveRegulaFalsi
		int const MaxIter( 500 ); // iteration control for SolveRegulaFalsi

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tmin; // Minimun drybulb setpoint temperature
		Real64 Tmax; // Maximun drybulb setpoint temperature
		Real64 PMVResult; // Calculated PMV value
		Real64 PMVMin; // Minimum allowed PMV value
		Real64 PMVMax; // Calculated PMV value
		Array1D< Real64 > Par( 2 ); // Passed parameter for RegularFalsi function
		int SolFla; // feed back flag from SolveRegulaFalsi
		static int IterLimitExceededNum1( 0 );
		static int IterLimitErrIndex1( 0 );
		static int IterLimitExceededNum2( 0 );
		static int IterLimitErrIndex2( 0 );

		Tmin = ComfortControlledZone( ComfortControlNum ).TdbMinSetPoint;
		Tmax = ComfortControlledZone( ComfortControlNum ).TdbMaxSetPoint;

		CalcThermalComfortFanger( PeopleNum, Tmin, PMVResult );
		PMVMin = PMVResult;
		CalcThermalComfortFanger( PeopleNum, Tmax, PMVResult );
		PMVMax = PMVResult;
		if ( PMVSet > PMVMin && PMVSet < PMVMax ) {
			Par( 1 ) = PMVSet;
			Par( 2 ) = double( PeopleNum );
			SolveRegulaFalsi( Acc, MaxIter, SolFla, Tset, PMVResidual, Tmin, Tmax, Par );
			if ( SolFla == -1 ) {
				if ( ! WarmupFlag ) {
					++IterLimitExceededNum1;
					if ( IterLimitExceededNum1 == 1 ) {
						ShowWarningError( ComfortControlledZone( ComfortControlNum ).Name + ": Iteration limit exceeded calculating thermal comfort Fanger setpoint and non-converged setpoint is used" );
					} else {
						ShowRecurringWarningErrorAtEnd( ComfortControlledZone( ComfortControlNum ).Name + ":  Iteration limit exceeded calculating thermal comfort setpoint.", IterLimitErrIndex1, Tset, Tset );
					}
				}
			} else if ( SolFla == -2 ) {
				if ( ! WarmupFlag ) {
					++IterLimitExceededNum2;
					if ( IterLimitExceededNum2 == 1 ) {
						ShowWarningError( ComfortControlledZone( ComfortControlNum ).Name + ": Solution is not found in calculating thermal comfort Fanger setpoint and the minimum setpoint is used" );
					} else {
						ShowRecurringWarningErrorAtEnd( ComfortControlledZone( ComfortControlNum ).Name + ":  Solution is not found in  calculating thermal comfort Fanger setpoint.", IterLimitErrIndex2, Tset, Tset );
					}
				}
			}
		} else if ( PMVSet < PMVMin ) {
			Tset = Tmin;
		} else if ( PMVSet > PMVMax ) {
			Tset = Tmax;
		}

	}

	Real64
	PMVResidual(
		Real64 const Tset,
		Array1< Real64 > const & Par // par(1) = PMV set point
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   May 2006
		//       MODIFIED       L.Gu, May 2006
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired PMV value - actual PMV value) for thermal comfort control.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcThermalComfortFanger to get PMV value at the given zone and people conditions
		//  and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using ThermalComfort::CalcThermalComfortFanger;

		// Return value
		Real64 PMVResidual;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PeopleNum; // index of people object
		Real64 PMVresult; // resulting PMV values

		PeopleNum = int( Par( 2 ) );
		CalcThermalComfortFanger( PeopleNum, Tset, PMVresult );
		PMVResidual = Par( 1 ) - PMVresult;
		return PMVResidual;

	}

	void
	AdjustCoolingSetPointforTempAndHumidityControl(
		int const TempControlledZoneID,
		int const ActualZoneNum // controlled zone actual zone number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket A Nigusse, FSEC/UCF
		//       DATE WRITTEN   Nov 2010
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine modifies the air cooling setpoint temperature to effect zone air Temperature
		//  and humidity control
		// METHODOLOGY EMPLOYED:
		//  Alter the zone air cooling setpoint if the zone air relative humidity value exceeds the
		//  the zone dehumidifying relative humidity setpoint.

		// REFERENCES:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MaxAllowedOvercoolRange; // Maximum allowed zone overcool range [DeltaC]
		Real64 RelativeHumidityDiff; // Difference between zone air relative humidity and dehumidifying setpoint [%RH]
		Real64 ZoneOvercoolRange;
		Real64 ZoneOvercoolControlRatio;

		if ( ! ( AnyZoneTempAndHumidityControl ) ) return; // do nothing to setpoint

		if ( ! ( TempControlledZone( TempControlledZoneID ).ZoneOvercoolControl ) ) return; // do nothing to setpoint

		if ( TempControlledZone( TempControlledZoneID ).OvercoolCntrlModeScheduled ) {
			ZoneOvercoolRange = GetCurrentScheduleValue( TempControlledZone( TempControlledZoneID ).ZoneOvercoolRangeSchedIndex );
		} else {
			ZoneOvercoolRange = TempControlledZone( TempControlledZoneID ).ZoneOvercoolConstRange;
		}
		ZoneOvercoolControlRatio = TempControlledZone( TempControlledZoneID ).ZoneOvercoolControlRatio;

		// For Dual Setpoint thermostat the overcool range is limited by the temperature difference between cooling
		// and heating setpoints
		MaxAllowedOvercoolRange = ZoneThermostatSetPointHi( ActualZoneNum ) - ZoneThermostatSetPointLo( ActualZoneNum );
		if ( MaxAllowedOvercoolRange > 0.0 ) {
			ZoneOvercoolRange = min( ZoneOvercoolRange, MaxAllowedOvercoolRange );
		}
		// Calculate difference between zone air relative humidity and the dehumidifying setpoint
		RelativeHumidityDiff = ZoneAirRelHum( ActualZoneNum ) - GetCurrentScheduleValue( TempControlledZone( TempControlledZoneID ).DehumidifyingSchedIndex );
		if ( RelativeHumidityDiff > 0.0 && ZoneOvercoolControlRatio > 0.0 ) {
			// proportionally reset the cooling setpoint temperature downward (zone Overcool)
			ZoneOvercoolRange = min( ZoneOvercoolRange, RelativeHumidityDiff / ZoneOvercoolControlRatio );
			ZoneThermostatSetPointHi( ActualZoneNum ) -= ZoneOvercoolRange;
		}

	}

} // ZoneTempPredictorCorrector

} // EnergyPlus
