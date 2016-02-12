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
#include <cassert>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <WaterManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <DataWater.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace WaterManager {

	// Module containing the routines dealing with the management of water

	// MODULE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   August 2006
	//       MODIFIED       DJS to add ecoroof irrigation Jan 2007
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BigNumber;
	using namespace DataWater;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE WaterManager:
	// pointers for water storage tanks and their supply arrays
	// pointers for water storage tanks and their demand arrays

	// Functions

	void
	ManageWater()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the top-level driver subroutine for managine water systems in the building
		// Routine is called at the system timestep level from ManageHVAC
		//  (somewhat analogous to SimHVAC)

		// METHODOLOGY EMPLOYED:
		// State variables are continually recalculated each system iteration
		// except when appropriate to update them.  IF this module is moved up
		// to a different timestep (with less iteration), then numerical solution
		// may need to be added.  Iteration is being used to solve interdependecies
		// of storage, supply, and demand modeling of water system.
		// Most data are declared in data-only module DataWater.cc
		// Calling order,
		//   storage tanks
		//   supply
		//   demands
		//  IF first/last timestep, then do an update.

		// REFERENCES:
		// na

		// USE STATEMENTS:

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
		static bool GetInputFlag( true ); // First time, input is "gotten"
		static int RainColNum( 0 );
		static int TankNum( 0 );
		static int WellNum( 0 );

		if ( GetInputFlag ) {
			GetWaterManagerInput();
			GetInputFlag = false;
		}

		if ( ! ( AnyWaterSystemsInModel ) ) return;

		// this is the main water manager
		// first call all the water storage tanks
		//    (these called first to make control decisions)
		for ( TankNum = 1; TankNum <= NumWaterStorageTanks; ++TankNum ) {
			CalcWaterStorageTank( TankNum );
		} //tank loop

		for ( RainColNum = 1; RainColNum <= NumRainCollectors; ++RainColNum ) {
			CalcRainCollector( RainColNum );
		}

		for ( WellNum = 1; WellNum <= NumGroundWaterWells; ++WellNum ) {
			CalcGroundwaterWell( WellNum );
		}

		//call the tanks again to get updated rain and well activity
		for ( TankNum = 1; TankNum <= NumWaterStorageTanks; ++TankNum ) {
			CalcWaterStorageTank( TankNum );
		} //tank loop

		ReportWaterManager();

	}

	void
	ManageWaterInits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ( AnyWaterSystemsInModel ) ) return;

		UpdateWaterManager();

		UpdatePrecipitation();
		UpdateIrrigation();

	}

	void
	GetWaterManagerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::VerifyName;
		using DataSurfaces::Surface;
		using DataHeatBalance::Zone;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleMinValue;
		using ScheduleManager::GetScheduleMaxValue;
		using ScheduleManager::CheckScheduleValue;
		using General::RoundSigDigits;

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
		int Item; // Item to be "gotten"
		static int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		static int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call
		static int IOStatus( 0 ); // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static bool MyOneTimeFlag( true );
		static int MaxNumAlphas( 0 ); // argument for call to GetObjectDefMaxArgs
		static int MaxNumNumbers( 0 ); // argument for call to GetObjectDefMaxArgs
		static int TotalArgs( 0 ); // argument for call to GetObjectDefMaxArgs
		static bool IsNotOK( false );
		static bool IsBlank( false );
		static int alphaOffset( 0 );
		static int SurfNum( 0 );
		static std::string objNameMsg;
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		static Real64 tmpMax( 0.0 );
		static Real64 tmpMin( 0.0 );
		static Real64 tmpNumerator( 0.0 );
		static Real64 tmpArea( 0.0 );
		static Real64 tmpDenominator( 0.0 );
		static int ThisSurf( 0 );
		int NumIrrigation;
		int Dummy;

		if ( ( MyOneTimeFlag ) && ( ! ( WaterSystemGetInputCalled ) ) ) { //big block for entire subroutine

			cCurrentModuleObject = "WaterUse:Storage";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
			MaxNumNumbers = NumNumbers;
			MaxNumAlphas = NumAlphas;
			cCurrentModuleObject = "WaterUse:RainCollector";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
			MaxNumNumbers = max( MaxNumNumbers, NumNumbers );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "WaterUse:Well";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
			MaxNumNumbers = max( MaxNumNumbers, NumNumbers );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "Site:Precipitation";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
			MaxNumNumbers = max( MaxNumNumbers, NumNumbers );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "RoofIrrigation";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
			MaxNumNumbers = max( MaxNumNumbers, NumNumbers );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

			cAlphaFieldNames.allocate( MaxNumAlphas );
			cAlphaArgs.allocate( MaxNumAlphas );
			lAlphaFieldBlanks.dimension( MaxNumAlphas, false );
			cNumericFieldNames.allocate( MaxNumNumbers );
			rNumericArgs.dimension( MaxNumNumbers, 0.0 );
			lNumericFieldBlanks.dimension( MaxNumNumbers, false );

			MyOneTimeFlag = false;
			cCurrentModuleObject = "WaterUse:Storage";
			NumWaterStorageTanks = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumWaterStorageTanks > 0 ) {
				AnyWaterSystemsInModel = true;
				if ( ! ( allocated( WaterStorage ) ) ) WaterStorage.allocate( NumWaterStorageTanks );

				for ( Item = 1; Item <= NumWaterStorageTanks; ++Item ) {
					GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
					AnyWaterSystemsInModel = true;
					WaterStorage( Item ).Name = cAlphaArgs( 1 );
					VerifyName( cAlphaArgs( 1 ), WaterStorage, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					objNameMsg = cCurrentModuleObject + " = " + cAlphaArgs( 1 );

					WaterStorage( Item ).QualitySubCategoryName = cAlphaArgs( 2 );
					//    If (SameString(cAlphaArgs(2), 'Mains')) Then
					//      WaterStorage(Item)%QualitySubCategory = MainsWater
					//    ELSEIF (SameString(cAlphaArgs(2), 'RAINWATER')) Then
					//      WaterStorage(Item)%QualitySubCategory = RainWater
					//    ELSEIF (SameString(cAlphaArgs(2), 'GREYWATER')) Then
					//      WaterStorage(Item)%QualitySubCategory = GreyWater
					//    ELSEIF (SameString(cAlphaArgs(2), 'WELLWATER')) Then
					//      WaterStorage(Item)%QualitySubCategory =  WellWater
					//    ELSEIF (SameString(cAlphaArgs(2), 'BLACKWATER')) Then
					//      WaterStorage(Item)%QualitySubCategory = BlackWater

					//    ELSE
					//          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
					//          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
					//      ErrorsFound = .TRUE.
					//    ENDIF

					WaterStorage( Item ).MaxCapacity = rNumericArgs( 1 );
					if ( WaterStorage( Item ).MaxCapacity == 0.0 ) { //default
						WaterStorage( Item ).MaxCapacity = BigNumber;
					}

					WaterStorage( Item ).InitialVolume = rNumericArgs( 2 );
					WaterStorage( Item ).MaxInFlowRate = rNumericArgs( 3 );
					if ( WaterStorage( Item ).MaxInFlowRate == 0.0 ) { //default
						WaterStorage( Item ).MaxInFlowRate = BigNumber;
					}

					WaterStorage( Item ).MaxOutFlowRate = rNumericArgs( 4 );
					if ( WaterStorage( Item ).MaxOutFlowRate == 0.0 ) { //default
						WaterStorage( Item ).MaxOutFlowRate = BigNumber;
					}

					WaterStorage( Item ).OverflowTankName = cAlphaArgs( 3 ); // setup later

					if ( SameString( cAlphaArgs( 4 ), "None" ) ) {
						WaterStorage( Item ).ControlSupplyType = NoControlLevel;
					} else if ( SameString( cAlphaArgs( 4 ), "Mains" ) ) {
						WaterStorage( Item ).ControlSupplyType = MainsFloatValve;
					} else if ( SameString( cAlphaArgs( 4 ), "GroundwaterWell" ) ) {
						WaterStorage( Item ).ControlSupplyType = WellFloatValve;
					} else if ( SameString( cAlphaArgs( 4 ), "OtherTank" ) ) {
						WaterStorage( Item ).ControlSupplyType = OtherTankFloatValve;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
					WaterStorage( Item ).ValveOnCapacity = rNumericArgs( 5 );
					WaterStorage( Item ).ValveOffCapacity = rNumericArgs( 6 );
					if ( WaterStorage( Item ).ControlSupplyType != NoControlLevel ) {
						if ( WaterStorage( Item ).ValveOffCapacity < WaterStorage( Item ).ValveOnCapacity ) {
							ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + " and/or " + cNumericFieldNames( 6 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ShowContinueError( cNumericFieldNames( 6 ) + " must be greater than " + cNumericFieldNames( 5 ) );
							ShowContinueError( "Check value for " + cNumericFieldNames( 5 ) + " = " + RoundSigDigits( WaterStorage( Item ).ValveOnCapacity, 5 ) );
							ShowContinueError( "which must be lower than " + cNumericFieldNames( 6 ) + " = " + RoundSigDigits( WaterStorage( Item ).ValveOffCapacity, 5 ) );
							ErrorsFound = true;
						}
					}

					WaterStorage( Item ).BackupMainsCapacity = rNumericArgs( 7 );
					if ( WaterStorage( Item ).BackupMainsCapacity > 0.0 ) { //add backup to well and other thank supply
						if ( WaterStorage( Item ).ControlSupplyType == WellFloatValve ) {
							WaterStorage( Item ).ControlSupplyType = WellFloatMainsBackup;
						}
						if ( WaterStorage( Item ).ControlSupplyType == OtherTankFloatValve ) {
							WaterStorage( Item ).ControlSupplyType = TankMainsBackup;
						}
					}

					WaterStorage( Item ).SupplyTankName = cAlphaArgs( 5 ); //set up later

					if ( SameString( cAlphaArgs( 6 ), "ScheduledTemperature" ) ) {
						WaterStorage( Item ).ThermalMode = ScheduledTankTemp;
					} else if ( SameString( cAlphaArgs( 6 ), "ThermalModel" ) ) {
						WaterStorage( Item ).ThermalMode = TankZoneThermalCoupled;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					}

					if ( WaterStorage( Item ).ThermalMode == ScheduledTankTemp ) {
						WaterStorage( Item ).TempSchedID = GetScheduleIndex( cAlphaArgs( 7 ) );
						if ( WaterStorage( Item ).TempSchedID == 0 ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
						tmpMin = GetScheduleMinValue( WaterStorage( Item ).TempSchedID );
						if ( tmpMin < 0.0 ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ShowContinueError( "Found storage tank temperature schedule value less than 0.0 in " + objNameMsg );
							ErrorsFound = true;
						}
						tmpMax = GetScheduleMaxValue( WaterStorage( Item ).TempSchedID );
						if ( tmpMax > 100.0 ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ShowContinueError( "found storage tank temperature schedule value greater than 100.0 in " + objNameMsg );
							ErrorsFound = true;
						}

					}

					if ( WaterStorage( Item ).ThermalMode == TankZoneThermalCoupled ) {
						if ( SameString( cAlphaArgs( 8 ), "Schedule" ) ) {
							WaterStorage( Item ).AmbientTempIndicator = AmbientTempSchedule;
						} else if ( SameString( cAlphaArgs( 8 ), "Zone" ) ) {
							WaterStorage( Item ).AmbientTempIndicator = AmbientTempZone;
						} else if ( SameString( cAlphaArgs( 8 ), "Outdoors" ) ) {
							WaterStorage( Item ).AmbientTempIndicator = AmbientTempExterior;
						} else {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
						WaterStorage( Item ).AmbientTempSchedule = GetScheduleIndex( cAlphaArgs( 9 ) );
						if ( ( WaterStorage( Item ).AmbientTempSchedule == 0 ) && ( WaterStorage( Item ).AmbientTempIndicator == AmbientTempSchedule ) ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
						WaterStorage( Item ).ZoneID = FindItemInList( cAlphaArgs( 10 ), Zone );
						if ( ( WaterStorage( Item ).ZoneID == 0 ) && ( WaterStorage( Item ).AmbientTempIndicator == AmbientTempZone ) ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
						WaterStorage( Item ).SurfArea = rNumericArgs( 8 );
						WaterStorage( Item ).UValue = rNumericArgs( 9 );
						WaterStorage( Item ).SurfMaterialName = cAlphaArgs( 11 );
						// todo verify material collect and store useful data from it.
					}
				}
			} // num water storage tanks > 0

			cCurrentModuleObject = "WaterUse:RainCollector";
			NumRainCollectors = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumRainCollectors > 0 ) {
				if ( ! ( allocated( RainCollector ) ) ) RainCollector.allocate( NumRainCollectors );
				// allow exensible reference to surfaces.
				AnyWaterSystemsInModel = true;

				for ( Item = 1; Item <= NumRainCollectors; ++Item ) {
					GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
					RainCollector( Item ).Name = cAlphaArgs( 1 );
					VerifyName( cAlphaArgs( 1 ), RainCollector, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Named " );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					objNameMsg = cCurrentModuleObject + " Named " + cAlphaArgs( 1 );

					RainCollector( Item ).StorageTankName = cAlphaArgs( 2 );
					RainCollector( Item ).StorageTankID = FindItemInList( cAlphaArgs( 2 ), WaterStorage );
					if ( RainCollector( Item ).StorageTankID == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

					if ( SameString( cAlphaArgs( 3 ), "Constant" ) ) {
						RainCollector( Item ).LossFactorMode = ConstantRainLossFactor;
					} else if ( SameString( cAlphaArgs( 3 ), "Scheduled" ) ) {
						RainCollector( Item ).LossFactorMode = ScheduledRainLossFactor;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
					RainCollector( Item ).LossFactor = rNumericArgs( 1 );
					if ( RainCollector( Item ).LossFactor > 1.0 ) {
						ShowWarningError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "found rain water collection loss factor greater than 1.0, simulation continues" );
					}
					if ( RainCollector( Item ).LossFactor < 0.0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "found rain water collection loss factor less than 0.0" );
						ErrorsFound = true;
					}

					if ( RainCollector( Item ).LossFactorMode == ScheduledRainLossFactor ) {
						RainCollector( Item ).LossFactorSchedID = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( RainCollector( Item ).LossFactorSchedID == 0 ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
						if ( GetScheduleMinValue( RainCollector( Item ).LossFactorSchedID ) < 0.0 ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ShowContinueError( "found rain water collection loss factor schedule value less than 0.0 in " + objNameMsg );
							ErrorsFound = true;
						}
						if ( GetScheduleMaxValue( RainCollector( Item ).LossFactorSchedID ) > 1.0 ) {
							ShowWarningError( "Potentially invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ShowContinueError( "found rain water collection loss factor schedule value greater than 1.0, simulation continues" );
							// allowing it to continue
						}
					}
					RainCollector( Item ).MaxCollectRate = rNumericArgs( 1 );
					if ( RainCollector( Item ).MaxCollectRate == 0.0 ) RainCollector( Item ).MaxCollectRate = 100000000000.0;

					//number of surfaces is extensible and = NumAlphas - alphaOffset
					alphaOffset = 4; //update this if more alphas inserted ahead of extensible surface listing
					RainCollector( Item ).NumCollectSurfs = NumAlphas - alphaOffset;
					RainCollector( Item ).SurfName.allocate( RainCollector( Item ).NumCollectSurfs );
					RainCollector( Item ).SurfID.allocate( RainCollector( Item ).NumCollectSurfs );
					for ( SurfNum = 1; SurfNum <= RainCollector( Item ).NumCollectSurfs; ++SurfNum ) {
						RainCollector( Item ).SurfName( SurfNum ) = cAlphaArgs( SurfNum + alphaOffset );
						RainCollector( Item ).SurfID( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + alphaOffset ), Surface );
						if ( RainCollector( Item ).SurfID( SurfNum ) == 0 ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( SurfNum + alphaOffset ) + '=' + cAlphaArgs( SurfNum + alphaOffset ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
					}

					// now setup horizontal surface area
					tmpArea = 0.0;
					tmpNumerator = 0.0;
					tmpDenominator = 0.0;
					for ( SurfNum = 1; SurfNum <= RainCollector( Item ).NumCollectSurfs; ++SurfNum ) {
						ThisSurf = RainCollector( Item ).SurfID( SurfNum );
						tmpArea += Surface( ThisSurf ).GrossArea * Surface( ThisSurf ).CosTilt;
						tmpNumerator += Surface( ThisSurf ).Centroid.z * Surface( ThisSurf ).GrossArea;
						tmpDenominator += Surface( ThisSurf ).GrossArea;
					}
					RainCollector( Item ).HorizArea = tmpArea;
					//now setup vertical hieght above ground for height dependent outdoor temps
					RainCollector( Item ).MeanHeight = tmpNumerator / tmpDenominator;

					// now set up tank supply connection
					InternalSetupTankSupplyComponent( RainCollector( Item ).Name, cCurrentModuleObject, RainCollector( Item ).StorageTankName, ErrorsFound, RainCollector( Item ).StorageTankID, RainCollector( Item ).StorageTankSupplyARRID );
				}
			} // (NumRainCollectors > 0)

			cCurrentModuleObject = "WaterUse:Well";
			NumGroundWaterWells = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumGroundWaterWells > 0 ) {
				AnyWaterSystemsInModel = true;
				GroundwaterWell.allocate( NumGroundWaterWells );
				for ( Item = 1; Item <= NumGroundWaterWells; ++Item ) {
					GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					GroundwaterWell( Item ).Name = cAlphaArgs( 1 );
					VerifyName( cAlphaArgs( 1 ), GroundwaterWell, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					objNameMsg = cCurrentModuleObject + " Named " + cAlphaArgs( 1 );
					GroundwaterWell( Item ).StorageTankName = cAlphaArgs( 2 );

					InternalSetupTankSupplyComponent( GroundwaterWell( Item ).Name, cCurrentModuleObject, GroundwaterWell( Item ).StorageTankName, ErrorsFound, GroundwaterWell( Item ).StorageTankID, GroundwaterWell( Item ).StorageTankSupplyARRID );

					if ( allocated( WaterStorage ) ) WaterStorage( GroundwaterWell( Item ).StorageTankID ).GroundWellID = Item;

					GroundwaterWell( Item ).PumpDepth = rNumericArgs( 1 );
					GroundwaterWell( Item ).PumpNomVolFlowRate = rNumericArgs( 2 );
					GroundwaterWell( Item ).PumpNomHead = rNumericArgs( 3 );
					GroundwaterWell( Item ).PumpNomPowerUse = rNumericArgs( 4 );
					GroundwaterWell( Item ).PumpEfficiency = rNumericArgs( 5 );
					GroundwaterWell( Item ).WellRecoveryRate = rNumericArgs( 6 );
					GroundwaterWell( Item ).NomWellStorageVol = rNumericArgs( 7 );
					if ( SameString( cAlphaArgs( 3 ), "Constant" ) ) {
						GroundwaterWell( Item ).GroundwaterTableMode = ConstantWaterTable;
					} else if ( SameString( cAlphaArgs( 3 ), "Scheduled" ) ) {
						GroundwaterWell( Item ).GroundwaterTableMode = ScheduledWaterTable;
					} else if ( lAlphaFieldBlanks( 3 ) ) {
						GroundwaterWell( Item ).GroundwaterTableMode = 0;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

					//  N8, \field water table depth
					GroundwaterWell( Item ).WaterTableDepth = rNumericArgs( 8 );
					// A4; \field water table depth schedule
					GroundwaterWell( Item ).WaterTableDepthSchedID = GetScheduleIndex( cAlphaArgs( 4 ) );
					if ( ( GroundwaterWell( Item ).GroundwaterTableMode == ScheduledWaterTable ) && ( GroundwaterWell( Item ).WaterTableDepthSchedID == 0 ) ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

				}
			} //(NumGroundWaterWells > 0)

			// do some water tank setup
			cCurrentModuleObject = "WaterUse:Storage";
			if ( NumWaterStorageTanks > 0 ) {
				for ( Item = 1; Item <= NumWaterStorageTanks; ++Item ) {
					// check that all storage tanks with ground well controls actually had wells pointing to them
					if ( ( WaterStorage( Item ).ControlSupplyType == WellFloatValve ) || ( WaterStorage( Item ).ControlSupplyType == WellFloatMainsBackup ) ) {
						if ( WaterStorage( Item ).GroundWellID == 0 ) {
							ShowSevereError( cCurrentModuleObject + "= \"" + WaterStorage( Item ).Name + "\" does not have a WaterUse:Well (groundwater well) that names it." );
							ErrorsFound = true;
						}
					}

					// setup tanks whose level is controlled by supply from another tank
					if ( ( WaterStorage( Item ).ControlSupplyType == OtherTankFloatValve ) || ( WaterStorage( Item ).ControlSupplyType == TankMainsBackup ) ) {
						WaterStorage( Item ).SupplyTankID = FindItemInList( WaterStorage( Item ).SupplyTankName, WaterStorage );
						if ( WaterStorage( Item ).SupplyTankID == 0 ) {
							ShowSevereError( "Other tank called " + WaterStorage( Item ).SupplyTankName + " not found for " + cCurrentModuleObject + " Named " + WaterStorage( Item ).Name ); // TODO rename point
							ErrorsFound = true;
						}
						InternalSetupTankDemandComponent( WaterStorage( Item ).Name, cCurrentModuleObject, WaterStorage( Item ).SupplyTankName, ErrorsFound, WaterStorage( Item ).SupplyTankID, WaterStorage( Item ).SupplyTankDemandARRID );
						//call to setup tank supply as well
						InternalSetupTankSupplyComponent( WaterStorage( Item ).SupplyTankName, cCurrentModuleObject, WaterStorage( Item ).Name, ErrorsFound, Dummy, Dummy );
					}
					// setup overflow inputs
					WaterStorage( Item ).OverflowTankID = FindItemInList( WaterStorage( Item ).OverflowTankName, WaterStorage );
					if ( WaterStorage( Item ).OverflowTankID == 0 ) {
						// if blank, then okay it is discarded.  but if not blank then error
						if ( is_blank( WaterStorage( Item ).OverflowTankName ) ) {
							WaterStorage( Item ).OverflowMode = OverflowDiscarded;
						} else {
							ShowSevereError( "Overflow tank name of " + WaterStorage( Item ).OverflowTankName + " not found for " + cCurrentModuleObject + " Named " + WaterStorage( Item ).Name );
							ErrorsFound = true;
						}
					} else {
						WaterStorage( Item ).OverflowMode = OverflowToTank;
					}
					if ( WaterStorage( Item ).OverflowMode == OverflowToTank ) {
						InternalSetupTankSupplyComponent( WaterStorage( Item ).Name, cCurrentModuleObject, WaterStorage( Item ).OverflowTankName, ErrorsFound, WaterStorage( Item ).OverflowTankID, WaterStorage( Item ).OverflowTankSupplyARRID );
					}

				}
			}

			cCurrentModuleObject = "Site:Precipitation";
			NumSiteRainFall = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumSiteRainFall > 1 ) { // throw error
				ShowSevereError( "Only one " + cCurrentModuleObject + " object is allowed" );
				ErrorsFound = true;
			}

			if ( NumSiteRainFall == 1 ) {
				AnyWaterSystemsInModel = true;
				GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );

				if ( SameString( cAlphaArgs( 1 ), "ScheduleAndDesignLevel" ) ) {
					RainFall.ModeID = RainSchedDesign;
				} else {
					ShowSevereError( "Precipitation Model Type of " + cCurrentModuleObject + " is incorrect." );
					ShowContinueError( "Only available option is ScheduleAndDesignLevel." );
					ErrorsFound = true;
				}
				RainFall.RainSchedID = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( ( RainFall.RainSchedID == 0 ) && ( RainFall.ModeID == RainSchedDesign ) ) {
					ShowSevereError( "Schedule not found for " + cCurrentModuleObject + " object" );
					ErrorsFound = true;
				} else if ( ( RainFall.RainSchedID != 0 ) && ( RainFall.ModeID == RainSchedDesign ) ) {
					if ( ! CheckScheduleValueMinMax( RainFall.RainSchedID, ">=", 0.0 ) ) {
						ShowSevereError( "Schedule=" + cAlphaArgs( 2 ) + " for " + cCurrentModuleObject + " object has values < 0." );
						ErrorsFound = true;
					}
				}

				RainFall.DesignAnnualRain = rNumericArgs( 1 );
				RainFall.NomAnnualRain = rNumericArgs( 2 );

			}

			cCurrentModuleObject = "RoofIrrigation";
			NumIrrigation = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumIrrigation > 1 ) {
				ShowSevereError( "Only one " + cCurrentModuleObject + " object is allowed" );
				ErrorsFound = true;
			}

			if ( NumIrrigation == 1 ) {
				AnyIrrigationInModel = true;
				GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );
				if ( SameString( cAlphaArgs( 1 ), "Schedule" ) ) {
					Irrigation.ModeID = IrrSchedDesign;
				} else if ( SameString( cAlphaArgs( 1 ), "SmartSchedule" ) ) {
					Irrigation.ModeID = IrrSmartSched;
				} else {
					ShowSevereError( "Type of " + cCurrentModuleObject + " is incorrect. Options are Schedule or SmartSchedule" );
					ErrorsFound = true;
				}
				Irrigation.IrrSchedID = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( ( Irrigation.IrrSchedID == 0 ) && ( ( Irrigation.ModeID == IrrSchedDesign ) || Irrigation.ModeID == IrrSmartSched ) ) {
					ShowSevereError( "Schedule not found for " + cCurrentModuleObject + " object" );
					ErrorsFound = true;
				} else if ( ( Irrigation.IrrSchedID == 0 ) && ( Irrigation.ModeID == IrrSchedDesign ) ) {
					if ( ! CheckScheduleValueMinMax( Irrigation.IrrSchedID, ">=", 0.0 ) ) {
						ShowSevereError( "Schedule=" + cAlphaArgs( 2 ) + " for " + cCurrentModuleObject + " object has values < 0." );
						ErrorsFound = true;
					}
				}

				// If we later add a designannualirrigation and a nominalannualirrigation variable (for scaling) those
				// would be assigned here... as with the Rainfall...
				Irrigation.IrrigationThreshold = 0.4;
				if ( Irrigation.ModeID == IrrSmartSched && NumNumbers > 0 ) {
					if ( rNumericArgs( 1 ) > 100.0 || rNumericArgs( 1 ) < 0.0 ) {
						ShowSevereError( "Irrigation threshold for " + cCurrentModuleObject + " object has values > 100 or < 0." );
						ErrorsFound = true;
					} else {
						Irrigation.IrrigationThreshold = rNumericArgs( 1 ) / 100.0;
					}
				}

			} // NumIrrigation ==1

			AnyWaterSystemsInModel = true;
			WaterSystemGetInputCalled = true;
			MyOneTimeFlag = false;

			cAlphaFieldNames.deallocate();
			cAlphaArgs.deallocate();
			lAlphaFieldBlanks.deallocate();
			cNumericFieldNames.deallocate();
			rNumericArgs.deallocate();
			lNumericFieldBlanks.deallocate();

			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing input for water manager objects" );
			}
			// <SetupOutputVariables here...>, CurrentModuleObject='WaterUse:Storage'
			for ( Item = 1; Item <= NumWaterStorageTanks; ++Item ) {
				// this next one is a measure of the state of water in the tank, not a flux of m3 that needs to be summed
				SetupOutputVariable( "Water System Storage Tank Volume [m3]", WaterStorage( Item ).ThisTimeStepVolume, "System", "Average", WaterStorage( Item ).Name );
				SetupOutputVariable( "Water System Storage Tank Net Volume Flow Rate [m3/s]", WaterStorage( Item ).NetVdot, "System", "Average", WaterStorage( Item ).Name );
				SetupOutputVariable( "Water System Storage Tank Inlet Volume Flow Rate [m3/s]", WaterStorage( Item ).VdotToTank, "System", "Average", WaterStorage( Item ).Name );
				SetupOutputVariable( "Water System Storage Tank Outlet Volume Flow Rate [m3/s]", WaterStorage( Item ).VdotFromTank, "System", "Average", WaterStorage( Item ).Name );
				SetupOutputVariable( "Water System Storage Tank Mains Water Volume [m3]", WaterStorage( Item ).MainsDrawVol, "System", "Sum", WaterStorage( Item ).Name, _, "MainsWater", "WaterSystem", WaterStorage( Item ).QualitySubCategoryName, "System" );
				SetupOutputVariable( "Water System Storage Tank Mains Water Volume Flow Rate [m3/s]", WaterStorage( Item ).MainsDrawVdot, "System", "Average", WaterStorage( Item ).Name );
				SetupOutputVariable( "Water System Storage Tank Water Temperature [C]", WaterStorage( Item ).Twater, "System", "Average", WaterStorage( Item ).Name );
				SetupOutputVariable( "Water System Storage Tank Overflow Volume Flow Rate [m3/s]", WaterStorage( Item ).VdotOverflow, "System", "Average", WaterStorage( Item ).Name );
				if ( WaterStorage( Item ).OverflowMode == OverflowDiscarded ) {
					SetupOutputVariable( "Water System Storage Tank Overflow Water Volume [m3]", WaterStorage( Item ).VolOverflow, "System", "Sum", WaterStorage( Item ).Name );
					//     ResourceTypeKey='Water',  &
					//     EndUseKey='WaterSystems', &
					//     EndUseSubkey=WaterStorage(item)%QualitySubCategoryName ,&
					//     GroupKey='System')
				} else {
					SetupOutputVariable( "Water System Storage Tank Overflow Water Volume [m3]", WaterStorage( Item ).VolOverflow, "System", "Sum", WaterStorage( Item ).Name );

				}
				SetupOutputVariable( "Water System Storage Tank Overflow Temperature [C]", WaterStorage( Item ).TwaterOverflow, "System", "Average", WaterStorage( Item ).Name );

			}

			if ( NumSiteRainFall == 1 ) { // CurrentModuleObject='Site:Precipitation'
				SetupOutputVariable( "Site Precipitation Rate [m/s]", RainFall.CurrentRate, "System", "Average", "Site:Precipitation" );
				SetupOutputVariable( "Site Precipitation Depth [m]", RainFall.CurrentAmount, "System", "Sum", "Site:Precipitation" );
			}

			if ( NumIrrigation == 1 ) { // CurrentModuleObject='RoofIrrigation'
				SetupOutputVariable( "Water System Roof Irrigation Scheduled Depth [m]", Irrigation.ScheduledAmount, "System", "Sum", "RoofIrrigation" );
				SetupOutputVariable( "Water System Roof Irrigation Actual Depth [m]", Irrigation.ActualAmount, "System", "Sum", "RoofIrrigation" );
			}

			for ( Item = 1; Item <= NumRainCollectors; ++Item ) { // CurrentModuleObject='WaterUse:RainCollector'
				SetupOutputVariable( "Water System Rainwater Collector Volume Flow Rate [m3/s]", RainCollector( Item ).VdotAvail, "System", "Average", RainCollector( Item ).Name );
				SetupOutputVariable( "Water System Rainwater Collector Volume [m3]", RainCollector( Item ).VolCollected, "System", "Sum", RainCollector( Item ).Name, _, "OnSiteWater", "Rainwater", _, "System" );

			}

			for ( Item = 1; Item <= NumGroundWaterWells; ++Item ) { // CurrentModuleObject='WaterUse:Well'
				SetupOutputVariable( "Water System Groundwater Well Requested Volume Flow Rate [m3/s]", GroundwaterWell( Item ).VdotRequest, "System", "Average", GroundwaterWell( Item ).Name );
				SetupOutputVariable( "Water System Groundwater Well Volume Flow Rate [m3/s]", GroundwaterWell( Item ).VdotDelivered, "System", "Average", GroundwaterWell( Item ).Name );
				SetupOutputVariable( "Water System Groundwater Well Volume [m3]", GroundwaterWell( Item ).VolDelivered, "System", "Sum", GroundwaterWell( Item ).Name, _, "OnSiteWater", "Wellwater", _, "System" );
				SetupOutputVariable( "Water System Groundwater Well Pump Electric Power [W]", GroundwaterWell( Item ).PumpPower, "System", "Average", GroundwaterWell( Item ).Name );
				SetupOutputVariable( "Water System Groundwater Well Pump Electric Energy [J]", GroundwaterWell( Item ).PumpEnergy, "System", "Sum", GroundwaterWell( Item ).Name, _, "Electricity", "WaterSystems", _, "System" );

			}

		} // my one time flag block

	}

	void
	UpdatePrecipitation()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//update the current rate of precipitation

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 schedRate;
		Real64 ScaleFactor;

		if ( RainFall.ModeID == RainSchedDesign ) {
			schedRate = GetCurrentScheduleValue( RainFall.RainSchedID ); // m/hr
			ScaleFactor = RainFall.DesignAnnualRain / RainFall.NomAnnualRain;
			RainFall.CurrentRate = schedRate * ScaleFactor / SecInHour; //convert to m/s
			RainFall.CurrentAmount = RainFall.CurrentRate * ( TimeStepSys * SecInHour );
		}

	}

	void
	UpdateIrrigation()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         D. Sailor
		//       DATE WRITTEN   Dec 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//update the current rate of irrigation

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 schedRate;
		//REAL(r64)  :: ScaleFactor

		Irrigation.ScheduledAmount = 0.0;

		if ( Irrigation.ModeID == IrrSchedDesign ) {
			schedRate = GetCurrentScheduleValue( Irrigation.IrrSchedID ); // m/hr
			Irrigation.ScheduledAmount = schedRate * ( TimeStepSys * SecInHour ) / SecInHour; // convert to m/timestep

		} else if ( Irrigation.ModeID == IrrSmartSched ) {
			schedRate = GetCurrentScheduleValue( Irrigation.IrrSchedID ); // m/hr
			Irrigation.ScheduledAmount = schedRate * ( TimeStepSys * SecInHour ) / SecInHour; // convert to m/timestep
		}

	}

	void
	SizeWaterManager()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

	}

	void
	CalcWaterStorageTank( int const TankNum ) // Index of storage tank
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Collect the calculations used to update the modeled values
		// for the storage tanks at each system timestep

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataGlobals::BeginTimeStepFlag;
		using DataHVACGlobals::TimeStepSys;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// see DataWater.cc

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 OrigVdotDemandRequest( 0.0 );
		static Real64 TotVdotDemandAvail( 0.0 );
		static Real64 OrigVolDemandRequest( 0.0 );
		static Real64 TotVolDemandAvail( 0.0 );
		static Real64 OrigVdotSupplyAvail( 0.0 );
		static Real64 TotVdotSupplyAvail( 0.0 );
		static Real64 TotVolSupplyAvail( 0.0 );
		//  REAL(r64)    :: TotVolSupplyAllow = 0.0d0
		static Real64 overflowVdot( 0.0 );
		static Real64 overflowVol( 0.0 );
		static Real64 overflowTwater( 0.0 );
		static Real64 NetVdotAdd( 0.0 );
		static Real64 NetVolAdd( 0.0 );
		static Real64 FillVolRequest( 0.0 );
		static Real64 TotVolAllowed( 0.0 );
		static Real64 AvailVolume( 0.0 );
		static Real64 underflowVdot( 0.0 );
		static Real64 VolumePredict( 0.0 );
		static Real64 OverFillVolume( 0.0 );

		if ( BeginTimeStepFlag ) {
			// initializations are done in UpdateWaterManager
		}

		overflowVdot = 0.0;
		if ( WaterStorage( TankNum ).NumWaterSupplies > 0 ) {
			OrigVdotSupplyAvail = sum( WaterStorage( TankNum ).VdotAvailSupply );
		} else {
			OrigVdotSupplyAvail = 0.0;
		}
		TotVdotSupplyAvail = OrigVdotSupplyAvail; // Init
		if ( TotVdotSupplyAvail > WaterStorage( TankNum ).MaxInFlowRate ) {
			// pipe/filter rate constraints on inlet
			overflowVdot = TotVdotSupplyAvail - WaterStorage( TankNum ).MaxInFlowRate;
			overflowTwater = sum( WaterStorage( TankNum ).VdotAvailSupply * WaterStorage( TankNum ).TwaterSupply ) / sum( WaterStorage( TankNum ).VdotAvailSupply );
			TotVdotSupplyAvail = WaterStorage( TankNum ).MaxInFlowRate;
		}
		TotVolSupplyAvail = TotVdotSupplyAvail * TimeStepSys * SecInHour;
		overflowVol = overflowVdot * TimeStepSys * SecInHour;

		underflowVdot = 0.0;
		if ( WaterStorage( TankNum ).NumWaterDemands > 0 ) {
			OrigVdotDemandRequest = sum( WaterStorage( TankNum ).VdotRequestDemand );
		} else {
			OrigVdotDemandRequest = 0.0;
		}
		OrigVolDemandRequest = OrigVdotDemandRequest * TimeStepSys * SecInHour;
		TotVdotDemandAvail = OrigVdotDemandRequest; // initialize to satisfied then modify if needed
		if ( TotVdotDemandAvail > WaterStorage( TankNum ).MaxOutFlowRate ) {
			// pipe/filter rate constraints on outlet
			underflowVdot = OrigVdotDemandRequest - WaterStorage( TankNum ).MaxOutFlowRate;
			TotVdotDemandAvail = WaterStorage( TankNum ).MaxOutFlowRate;
		}
		TotVolDemandAvail = TotVdotDemandAvail * ( TimeStepSys * SecInHour );

		NetVdotAdd = TotVdotSupplyAvail - TotVdotDemandAvail;
		NetVolAdd = NetVdotAdd * ( TimeStepSys * SecInHour );

		VolumePredict = WaterStorage( TankNum ).LastTimeStepVolume + NetVolAdd;

		// would tank capacity be exceeded?
		TotVolAllowed = WaterStorage( TankNum ).MaxCapacity - WaterStorage( TankNum ).LastTimeStepVolume;
		if ( VolumePredict > WaterStorage( TankNum ).MaxCapacity ) { // too much
			// added overflow to inlet rate limit, new temperature model
			OverFillVolume = ( VolumePredict - WaterStorage( TankNum ).MaxCapacity );
			overflowTwater = ( overflowTwater * overflowVol + OverFillVolume * WaterStorage( TankNum ).Twater ) / ( overflowVol + OverFillVolume );
			overflowVol += OverFillVolume;
			NetVolAdd -= OverFillVolume;
			NetVdotAdd = NetVolAdd / ( TimeStepSys * SecInHour );
			VolumePredict = WaterStorage( TankNum ).MaxCapacity;
		}

		//Is tank too low to meet the request?
		if ( VolumePredict < 0.0 ) {
			AvailVolume = WaterStorage( TankNum ).LastTimeStepVolume + TotVolSupplyAvail;
			AvailVolume = max( 0.0, AvailVolume );
			TotVolDemandAvail = AvailVolume;
			TotVdotDemandAvail = AvailVolume / ( TimeStepSys * SecInHour );
			underflowVdot = OrigVdotDemandRequest - TotVdotDemandAvail;
			NetVdotAdd = TotVdotSupplyAvail - TotVdotDemandAvail;
			NetVolAdd = NetVdotAdd * ( TimeStepSys * SecInHour );
			VolumePredict = 0.0;
		}

		if ( TotVdotDemandAvail < OrigVdotDemandRequest ) { // starvation
			// even distribution
			if ( OrigVdotDemandRequest > 0.0 ) {
				WaterStorage( TankNum ).VdotAvailDemand = ( TotVdotDemandAvail / OrigVdotDemandRequest ) * WaterStorage( TankNum ).VdotRequestDemand;
			} else {
				WaterStorage( TankNum ).VdotAvailDemand = 0.0;
			}
		} else { // requested demand can be served
			if ( WaterStorage( TankNum ).NumWaterDemands > 0 ) {
				WaterStorage( TankNum ).VdotAvailDemand = WaterStorage( TankNum ).VdotRequestDemand;
			}
		}

		// is tank lower than float valve on capacity and requesting fill from controlled supplier?
		FillVolRequest = 0.0;
		if ( ( VolumePredict ) < WaterStorage( TankNum ).ValveOnCapacity ) { //turn on supply to fill tank
			FillVolRequest = WaterStorage( TankNum ).ValveOffCapacity - VolumePredict;

			// set mains draws for float on (all the way to Float off)
			if ( WaterStorage( TankNum ).ControlSupplyType == MainsFloatValve ) {

				WaterStorage( TankNum ).MainsDrawVdot = FillVolRequest / ( TimeStepSys * SecInHour );
				NetVolAdd = FillVolRequest;

			}
			// set demand request in supplying tank if needed
			if ( ( WaterStorage( TankNum ).ControlSupplyType == OtherTankFloatValve ) || ( WaterStorage( TankNum ).ControlSupplyType == TankMainsBackup ) ) {
				WaterStorage( WaterStorage( TankNum ).SupplyTankID ).VdotRequestDemand( WaterStorage( TankNum ).SupplyTankDemandARRID ) = FillVolRequest / ( TimeStepSys * SecInHour );

			}

			// set demand request in groundwater well if needed
			if ( ( WaterStorage( TankNum ).ControlSupplyType == WellFloatValve ) || ( WaterStorage( TankNum ).ControlSupplyType == WellFloatMainsBackup ) ) {
				GroundwaterWell( WaterStorage( TankNum ).GroundWellID ).VdotRequest = FillVolRequest / ( TimeStepSys * SecInHour );
			}

		}

		// set mains flow if mains backup active
		if ( ( VolumePredict ) < WaterStorage( TankNum ).BackupMainsCapacity ) { //turn on supply
			if ( ( WaterStorage( TankNum ).ControlSupplyType == WellFloatMainsBackup ) || ( WaterStorage( TankNum ).ControlSupplyType == TankMainsBackup ) ) {
				FillVolRequest = WaterStorage( TankNum ).ValveOffCapacity - VolumePredict;
				WaterStorage( TankNum ).MainsDrawVdot = FillVolRequest / ( TimeStepSys * SecInHour );
				NetVolAdd = FillVolRequest;

			}
		}

		WaterStorage( TankNum ).ThisTimeStepVolume = WaterStorage( TankNum ).LastTimeStepVolume + NetVolAdd;
		WaterStorage( TankNum ).VdotOverflow = overflowVol / ( TimeStepSys * SecInHour );
		WaterStorage( TankNum ).VolOverflow = overflowVol;
		WaterStorage( TankNum ).TwaterOverflow = overflowTwater;
		WaterStorage( TankNum ).NetVdot = NetVolAdd / ( TimeStepSys * SecInHour );
		WaterStorage( TankNum ).MainsDrawVol = WaterStorage( TankNum ).MainsDrawVdot * ( TimeStepSys * SecInHour );
		WaterStorage( TankNum ).VdotToTank = TotVdotSupplyAvail;
		WaterStorage( TankNum ).VdotFromTank = TotVdotDemandAvail;

		{ auto const SELECT_CASE_var( WaterStorage( TankNum ).ThermalMode );
		if ( SELECT_CASE_var == ScheduledTankTemp ) {
			WaterStorage( TankNum ).Twater = GetCurrentScheduleValue( WaterStorage( TankNum ).TempSchedID );
			WaterStorage( TankNum ).TouterSkin = WaterStorage( TankNum ).Twater;
		} else if ( SELECT_CASE_var == TankZoneThermalCoupled ) {
			ShowFatalError( "WaterUse:Storage (Water Storage Tank) zone thermal model incomplete" );
		}}

		//set supply avail data from overflows in Receiving tank
		if ( WaterStorage( TankNum ).OverflowMode == OverflowToTank ) {
			WaterStorage( WaterStorage( TankNum ).OverflowTankID ).VdotAvailSupply( WaterStorage( TankNum ).OverflowTankSupplyARRID ) = WaterStorage( TankNum ).VdotOverflow;
			WaterStorage( WaterStorage( TankNum ).OverflowTankID ).TwaterSupply( WaterStorage( TankNum ).OverflowTankSupplyARRID ) = WaterStorage( TankNum ).TwaterOverflow;
		}

	}

	void
	SetupTankSupplyComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterSupplyIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Each simulated component that can supply water to a tank
		// makes one call to this subroutine to obtain the data
		// array index it should use to set values in the
		// VdotAvailSupply

		// METHODOLOGY EMPLOYED:
		// push the VdotAvailToTank array and return

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ( WaterSystemGetInputCalled ) ) {
			GetWaterManagerInput();
		}

		InternalSetupTankSupplyComponent( CompName, CompType, TankName, ErrorsFound, TankIndex, WaterSupplyIndex );

	}

	void
	InternalSetupTankSupplyComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterSupplyIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Each simulated component that can supply water to a tank
		// makes one call to this subroutine to obtain the data
		// array index it should use to set values in the
		// VdotAvailSupply

		// METHODOLOGY EMPLOYED:
		// push the VdotAvailToTank array and return

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int oldNumSupply;
		Array1D_string oldSupplyCompNames;
		Array1D_string oldSupplyCompTypes;
		//  LOGICAL , SAVE    :: MyOneTimeFlag = .TRUE.

		TankIndex = FindItemInList( TankName, WaterStorage );
		if ( TankIndex == 0 ) {
			ShowSevereError( "WaterUse:Storage (Water Storage Tank) =\"" + TankName + "\" not found in " + CompType + " called " + CompName );
			ErrorsFound = true;
			return; // So we don't pass TankIndex=0
		}
		oldNumSupply = WaterStorage( TankIndex ).NumWaterSupplies;
		if ( oldNumSupply > 0 ) { // do array push
			if ( allocated( oldSupplyCompNames ) ) oldSupplyCompNames.deallocate();
			oldSupplyCompNames.allocate( oldNumSupply );
			if ( allocated( oldSupplyCompTypes ) ) oldSupplyCompTypes.deallocate();
			oldSupplyCompTypes.allocate( oldNumSupply );
			if ( allocated( WaterStorage( TankIndex ).SupplyCompNames ) ) {
				oldSupplyCompNames = WaterStorage( TankIndex ).SupplyCompNames;
				WaterStorage( TankIndex ).SupplyCompNames.deallocate();
				WaterStorage( TankIndex ).SupplyCompNames.allocate( oldNumSupply + 1 );
				WaterStorage( TankIndex ).SupplyCompNames( {1,oldNumSupply} ) = oldSupplyCompNames; //array assignment
				WaterStorage( TankIndex ).SupplyCompNames( oldNumSupply + 1 ) = CompName;
			}
			if ( allocated( WaterStorage( TankIndex ).SupplyCompTypes ) ) {
				oldSupplyCompTypes = WaterStorage( TankIndex ).SupplyCompTypes;
				WaterStorage( TankIndex ).SupplyCompTypes.deallocate();
				WaterStorage( TankIndex ).SupplyCompTypes.allocate( oldNumSupply + 1 );
				WaterStorage( TankIndex ).SupplyCompTypes( {1,oldNumSupply} ) = oldSupplyCompTypes; //array assignment
				WaterStorage( TankIndex ).SupplyCompTypes( oldNumSupply + 1 ) = CompType;
			}
			WaterStorage( TankIndex ).VdotAvailSupply.deallocate();
			WaterStorage( TankIndex ).VdotAvailSupply.allocate( oldNumSupply + 1 );
			WaterStorage( TankIndex ).VdotAvailSupply = 0.0; //initialize
			WaterStorage( TankIndex ).TwaterSupply.deallocate();
			WaterStorage( TankIndex ).TwaterSupply.allocate( oldNumSupply + 1 );
			WaterStorage( TankIndex ).TwaterSupply = 0.0; //initialize
			WaterSupplyIndex = oldNumSupply + 1;
			++WaterStorage( TankIndex ).NumWaterSupplies;
		} else { // first time (no push)

			WaterStorage( TankIndex ).VdotAvailSupply.allocate( 1 );
			WaterStorage( TankIndex ).VdotAvailSupply = 0.0; //initialize
			WaterStorage( TankIndex ).TwaterSupply.allocate( 1 );
			WaterStorage( TankIndex ).TwaterSupply = 0.0; //initialize
			WaterStorage( TankIndex ).SupplyCompNames.allocate( 1 );
			WaterStorage( TankIndex ).SupplyCompNames( 1 ) = CompName;
			WaterStorage( TankIndex ).SupplyCompTypes.allocate( 1 );
			WaterStorage( TankIndex ).SupplyCompTypes( 1 ) = CompType;
			WaterSupplyIndex = 1;
			WaterStorage( TankIndex ).NumWaterSupplies = 1;
		}

	}

	void
	SetupTankDemandComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterDemandIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Each simulated component that can supply water to a tank
		// makes one call to this subroutine to obtain the data
		// array index it should use to set values in the
		// VdotAvailSupply

		// METHODOLOGY EMPLOYED:
		// push the VdotAvailToTank array and return

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ( WaterSystemGetInputCalled ) ) {
			GetWaterManagerInput();
		}

		InternalSetupTankDemandComponent( CompName, CompType, TankName, ErrorsFound, TankIndex, WaterDemandIndex );

	}

	void
	InternalSetupTankDemandComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterDemandIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Each simulated component that can supply water to a tank
		// makes one call to this subroutine to obtain the data
		// array index it should use to set values in the
		// VdotAvailSupply

		// METHODOLOGY EMPLOYED:
		// push the VdotAvailToTank array and return

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int oldNumDemand;
		Array1D_string oldDemandCompNames;
		Array1D_string oldDemandCompTypes;
		//  LOGICAL , SAVE    :: MyOneTimeFlag = .TRUE.

		TankIndex = FindItemInList( TankName, WaterStorage );
		if ( TankIndex == 0 ) {
			ShowSevereError( "WaterUse:Storage (Water Storage Tank) =\"" + TankName + "\" not found in " + CompType + " called " + CompName );
			ErrorsFound = true;
			return;
		}
		oldNumDemand = WaterStorage( TankIndex ).NumWaterDemands;
		if ( oldNumDemand > 0 ) { // do array push
			if ( allocated( oldDemandCompNames ) ) oldDemandCompNames.deallocate();
			oldDemandCompNames.allocate( oldNumDemand );
			if ( allocated( oldDemandCompTypes ) ) oldDemandCompTypes.deallocate();
			oldDemandCompTypes.allocate( oldNumDemand );
			if ( allocated( WaterStorage( TankIndex ).DemandCompNames ) ) {
				oldDemandCompNames = WaterStorage( TankIndex ).DemandCompNames;
				WaterStorage( TankIndex ).DemandCompNames.deallocate();
				WaterStorage( TankIndex ).DemandCompNames.allocate( oldNumDemand + 1 );
				WaterStorage( TankIndex ).DemandCompNames( {1,oldNumDemand} ) = oldDemandCompNames; //array assignment
				WaterStorage( TankIndex ).DemandCompNames( oldNumDemand + 1 ) = CompName;
			}
			if ( allocated( WaterStorage( TankIndex ).DemandCompTypes ) ) {
				oldDemandCompTypes = WaterStorage( TankIndex ).DemandCompTypes;
				WaterStorage( TankIndex ).DemandCompTypes.deallocate();
				WaterStorage( TankIndex ).DemandCompTypes.allocate( oldNumDemand + 1 );
				WaterStorage( TankIndex ).DemandCompTypes( {1,oldNumDemand} ) = oldDemandCompTypes; //array assignment
				WaterStorage( TankIndex ).DemandCompTypes( oldNumDemand + 1 ) = CompType;
			}

			WaterStorage( TankIndex ).VdotRequestDemand.deallocate();
			WaterStorage( TankIndex ).VdotRequestDemand.allocate( oldNumDemand + 1 );
			WaterStorage( TankIndex ).VdotRequestDemand = 0.0; //initialize

			WaterStorage( TankIndex ).VdotAvailDemand.deallocate();
			WaterStorage( TankIndex ).VdotAvailDemand.allocate( oldNumDemand + 1 );
			WaterStorage( TankIndex ).VdotAvailDemand = 0.0; //initialize

			WaterDemandIndex = oldNumDemand + 1;
			++WaterStorage( TankIndex ).NumWaterDemands;
		} else { // first time (no push)

			WaterStorage( TankIndex ).VdotRequestDemand.allocate( 1 );
			WaterStorage( TankIndex ).VdotRequestDemand = 0.0; //initialize
			WaterStorage( TankIndex ).VdotAvailDemand.allocate( 1 );
			WaterStorage( TankIndex ).VdotAvailDemand = 0.0; //initialize
			WaterStorage( TankIndex ).DemandCompNames.allocate( 1 );
			WaterStorage( TankIndex ).DemandCompNames( 1 ) = CompName;
			WaterStorage( TankIndex ).DemandCompTypes.allocate( 1 );
			WaterStorage( TankIndex ).DemandCompTypes( 1 ) = CompType;
			WaterStorage( TankIndex ).NumWaterDemands = 1;
			WaterDemandIndex = 1;
		}

	}

	void
	CalcRainCollector( int const RainColNum ) // Index of rain collector
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Collect the calculations used to update the modeled values
		// for the rain collector at each system timestep

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::OutWetBulbTempAt;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// see DataWater.cc

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LossFactor( 0.0 );
		Real64 VdotAvail;

		//If (.NOT.(IsRain)) Then ! is it raining now? No don't use this flag since precip schedule might differ from weather file
		if ( RainFall.CurrentRate <= 0.0 ) {
			// set available supply rate in WaterStorage
			WaterStorage( RainCollector( RainColNum ).StorageTankID ).VdotAvailSupply( RainCollector( RainColNum ).StorageTankSupplyARRID ) = 0.0;
			// temperature of water supply is modeled as the same as outdoor drybulb.
			WaterStorage( RainCollector( RainColNum ).StorageTankID ).TwaterSupply( RainCollector( RainColNum ).StorageTankSupplyARRID ) = 0.0;

			RainCollector( RainColNum ).VdotAvail = 0.0;
			RainCollector( RainColNum ).VolCollected = 0.0;
		} else {

			{ auto const SELECT_CASE_var( RainCollector( RainColNum ).LossFactorMode );

			if ( SELECT_CASE_var == ConstantRainLossFactor ) {
				LossFactor = RainCollector( RainColNum ).LossFactor;
			} else if ( SELECT_CASE_var == ScheduledRainLossFactor ) {
				LossFactor = GetCurrentScheduleValue( RainCollector( RainColNum ).LossFactorSchedID );
			} else {
				assert( false );
			}}

			VdotAvail = RainFall.CurrentRate * RainCollector( RainColNum ).HorizArea * ( 1.0 - LossFactor );

			if ( VdotAvail > RainCollector( RainColNum ).MaxCollectRate ) {
				VdotAvail = RainCollector( RainColNum ).MaxCollectRate;
			}

			// set available supply rate in WaterStorage
			WaterStorage( RainCollector( RainColNum ).StorageTankID ).VdotAvailSupply( RainCollector( RainColNum ).StorageTankSupplyARRID ) = VdotAvail;

			// temperature of water supply is modeled as the same as outdoor drybulb.
			WaterStorage( RainCollector( RainColNum ).StorageTankID ).TwaterSupply( RainCollector( RainColNum ).StorageTankSupplyARRID ) = OutWetBulbTempAt( RainCollector( RainColNum ).MeanHeight );

			RainCollector( RainColNum ).VdotAvail = VdotAvail;
			RainCollector( RainColNum ).VolCollected = VdotAvail * TimeStepSys * SecInHour;

		}

	}

	void
	CalcGroundwaterWell( int const WellNum ) // Index of well
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Collect the calculations used to update the modeled values
		// for the groundwater wells at each system timestep

		// METHODOLOGY EMPLOYED:
		// starting simple and ignoring well storage and complex rate restrictions.
		// just uses nominal pump rate and power (assuming well designed well).

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginTimeStepFlag;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::GroundTemp_Deep;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// see DataWater.cc

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 VdotDelivered;
		//  REAL(r64) :: VdotRequest
		Real64 PumpPower;

		if ( BeginTimeStepFlag ) {
			// do any updating needed
			// GroundwaterWell(WellNum)%VdotRequest = 0.0

		}

		VdotDelivered = 0.0;
		PumpPower = 0.0;
		if ( GroundwaterWell( WellNum ).VdotRequest > 0.0 ) {

			if ( GroundwaterWell( WellNum ).VdotRequest >= GroundwaterWell( WellNum ).PumpNomVolFlowRate ) { // run flat out
				WaterStorage( GroundwaterWell( WellNum ).StorageTankID ).VdotAvailSupply( GroundwaterWell( WellNum ).StorageTankSupplyARRID ) = GroundwaterWell( WellNum ).PumpNomVolFlowRate;
				WaterStorage( GroundwaterWell( WellNum ).StorageTankID ).TwaterSupply( GroundwaterWell( WellNum ).StorageTankSupplyARRID ) = GroundTemp_Deep;
				VdotDelivered = GroundwaterWell( WellNum ).PumpNomVolFlowRate;
				PumpPower = GroundwaterWell( WellNum ).PumpNomPowerUse;
			}

			// the run at part load to just meet request
			if ( GroundwaterWell( WellNum ).VdotRequest < GroundwaterWell( WellNum ).PumpNomVolFlowRate ) {
				WaterStorage( GroundwaterWell( WellNum ).StorageTankID ).VdotAvailSupply( GroundwaterWell( WellNum ).StorageTankSupplyARRID ) = GroundwaterWell( WellNum ).VdotRequest;
				WaterStorage( GroundwaterWell( WellNum ).StorageTankID ).TwaterSupply( GroundwaterWell( WellNum ).StorageTankSupplyARRID ) = GroundTemp_Deep;

				VdotDelivered = GroundwaterWell( WellNum ).VdotRequest;
				PumpPower = GroundwaterWell( WellNum ).PumpNomPowerUse * GroundwaterWell( WellNum ).VdotRequest / GroundwaterWell( WellNum ).PumpNomVolFlowRate;

			}
		}

		GroundwaterWell( WellNum ).VdotDelivered = VdotDelivered;
		GroundwaterWell( WellNum ).VolDelivered = VdotDelivered * TimeStepSys * SecInHour;
		GroundwaterWell( WellNum ).PumpPower = PumpPower;
		GroundwaterWell( WellNum ).PumpEnergy = PumpPower * TimeStepSys * SecInHour;

	}

	void
	UpdateWaterManager()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The water manger is iterating and
		// we need to do the timestep record keeping
		// for tracking state variables.
		//  this routine updates variables
		// that hold the value of the Last Timestep

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::WarmupFlag;
		using DataGlobals::KickOffSimulation;
		using DataGlobals::DoingSizing;

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
		int TankNum;
		int RainColNum;
		int WellNum;
		static bool MyEnvrnFlag( true ); // flag for init once at start of environment
		static bool MyWarmupFlag( false ); // flag for init after warmup complete
		static bool MyTankDemandCheckFlag( true );

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( TankNum = 1; TankNum <= NumWaterStorageTanks; ++TankNum ) {

				WaterStorage( TankNum ).LastTimeStepVolume = WaterStorage( TankNum ).InitialVolume;
				WaterStorage( TankNum ).ThisTimeStepVolume = WaterStorage( TankNum ).InitialVolume;
			}
			if ( ( ! DoingSizing ) && ( ! KickOffSimulation ) && MyTankDemandCheckFlag ) {
				if ( NumWaterStorageTanks > 0 ) {
					for ( TankNum = 1; TankNum <= NumWaterStorageTanks; ++TankNum ) {
						if ( WaterStorage( TankNum ).NumWaterDemands == 0 ) {
							ShowWarningError( "Found WaterUse:Tank that has nothing connected to draw water from it." );
							ShowContinueError( "Occurs for WaterUse:Tank = " + WaterStorage( TankNum ).Name );
							ShowContinueError( "Check that input for water consuming components specifies a water supply tank." );
						}
					}
				}
				MyTankDemandCheckFlag = false;
			}

			MyEnvrnFlag = false;
			MyWarmupFlag = true;
		} // end environmental inits
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		if ( MyWarmupFlag && ( ! WarmupFlag ) ) { // do environment inits.  just went out of warmup mode
			for ( TankNum = 1; TankNum <= NumWaterStorageTanks; ++TankNum ) {
				WaterStorage( TankNum ).LastTimeStepVolume = WaterStorage( TankNum ).InitialVolume;
				WaterStorage( TankNum ).ThisTimeStepVolume = WaterStorage( TankNum ).InitialVolume;
				WaterStorage( TankNum ).LastTimeStepTemp = WaterStorage( TankNum ).InitialTankTemp;
			}
			MyWarmupFlag = false;
		}

		for ( TankNum = 1; TankNum <= NumWaterStorageTanks; ++TankNum ) {
			// main location for inits for new timestep.
			WaterStorage( TankNum ).LastTimeStepVolume = max( WaterStorage( TankNum ).ThisTimeStepVolume, 0.0 );
			WaterStorage( TankNum ).MainsDrawVdot = 0.0;
			WaterStorage( TankNum ).MainsDrawVol = 0.0;
			WaterStorage( TankNum ).NetVdot = 0.0;
			WaterStorage( TankNum ).VdotFromTank = 0.0;
			WaterStorage( TankNum ).VdotToTank = 0.0;
			if ( WaterStorage( TankNum ).NumWaterDemands > 0 ) {
				// don't reset the requested demand, it is up to the other components to update it themselves
				//WaterStorage( TankNum ).VdotRequestDemand = 0.0;
				// the available demand is calculated here in the calc routine, so its fine to initialize it
				WaterStorage( TankNum ).VdotAvailDemand = 0.0;
			}
			WaterStorage( TankNum ).VdotOverflow = 0.0;
			if ( WaterStorage( TankNum ).NumWaterSupplies > 0 ) {
				WaterStorage( TankNum ).VdotAvailSupply = 0.0;
			}
			if ( ( WaterStorage( TankNum ).ControlSupplyType == WellFloatValve ) || ( WaterStorage( TankNum ).ControlSupplyType == WellFloatMainsBackup ) ) {
				if ( allocated( GroundwaterWell ) ) GroundwaterWell( WaterStorage( TankNum ).GroundWellID ).VdotRequest = 0.0;
			}
		} //tank loop

		for ( RainColNum = 1; RainColNum <= NumRainCollectors; ++RainColNum ) {

			RainCollector( RainColNum ).VdotAvail = 0.0;
			RainCollector( RainColNum ).VolCollected = 0.0;
		}

		for ( WellNum = 1; WellNum <= NumGroundWaterWells; ++WellNum ) {
			// re init calculated vars
			GroundwaterWell( WellNum ).VdotRequest = 0.0;
			GroundwaterWell( WellNum ).VdotDelivered = 0.0;
			GroundwaterWell( WellNum ).VolDelivered = 0.0;
			GroundwaterWell( WellNum ).PumpPower = 0.0;
			GroundwaterWell( WellNum ).PumpEnergy = 0.0;
		}

	}

	void
	ReportWaterManager()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// <this routine is typically needed only for those cases where you must transform the internal data to a reportable form>

	}

} // WaterManager

} // EnergyPlus
