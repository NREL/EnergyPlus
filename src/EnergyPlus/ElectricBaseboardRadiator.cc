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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <ElectricBaseboardRadiator.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ElectricBaseboardRadiator {

	// Module ElectricBaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Electric)

	// Module containing the routines dealing with the electric baseboard heater

	// MODULE INFORMATION:
	//       AUTHOR         Daeho Kang
	//       DATE WRITTEN   Feb 2010
	//       MODIFIED
	//       RE-ENGINEERED

	// PURPOSE OF THIS MODULE:
	// This module is to calculate the actual convective heat addition that an electrical baseboard heater
	// deliveres to a space.

	// METHODOLOGY EMPLOYED:
	// Based on the convective-only electric baseboard module (Object: ZoneHVAC:Baseboard:Convective:Electric)
	// written by Richard Liesen in Nov 2001, this new electric baseboard module is to add the existing calculation
	// algorithm of radiant heat transfer in the high temperature radiant system module.

	// REFERENCES:
	// HighTempRadiantSystem module (ZoneHVAC:HighTemperatureRadiant)
	// Convective electric baseboard module (ZoneHVAC:Baseboard:Convective:Electric)

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataGlobals;
	using namespace DataPrecisionGlobals;

	// Use statements for access to subroutines in other modules

	// Data
	//MODULE PARAMETER DEFINITIONS
	int const BaseboardRadiator_Electric( 1 );
	std::string const cCMO_BBRadiator_Electric( "ZoneHVAC:Baseboard:RadiantConvective:Electric" );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumElecBaseboards( 0 );
	Array1D< Real64 > QBBElecRadSource; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > QBBElecRadSrcAvg; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QBBRadSrcAvg locally
	Array1D< Real64 > LastQBBElecRadSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;
	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Object Data
	Array1D< ElecBaseboardParams > ElecBaseboard;
	Array1D< ElecBaseboardNumericFieldData > ElecBaseboardNumericFields;

	// Functions

	void
	SimElecBaseboard(
		std::string const & EquipName,
		int const EP_UNUSED( ActualZoneNum ),
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       Feb 2010 Daeho Kang for radiant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the Electric Baseboard units.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Water baseboard module

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BaseboardNum; // Index of unit in baseboard array
		static bool GetInputFlag( true ); // One time get input flag

		if ( GetInputFlag ) {
			GetElectricBaseboardInput();
			GetInputFlag = false;
		}

		// Find the correct Baseboard Equipment
		if ( CompIndex == 0 ) {
			BaseboardNum = FindItemInList( EquipName, ElecBaseboard, &ElecBaseboardParams::EquipName );
			if ( BaseboardNum == 0 ) {
				ShowFatalError( "SimElectricBaseboard: Unit not found=" + EquipName );
			}
			CompIndex = BaseboardNum;
		} else {
			BaseboardNum = CompIndex;
			if ( BaseboardNum > NumElecBaseboards || BaseboardNum < 1 ) {
				ShowFatalError( "SimElectricBaseboard:  Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Number of Units=" + TrimSigDigits( NumElecBaseboards ) + ", Entered Unit name=" + EquipName );
			}
			if ( CheckEquipName( BaseboardNum ) ) {
				if ( EquipName != ElecBaseboard( BaseboardNum ).EquipName ) {
					ShowFatalError( "SimElectricBaseboard: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + ElecBaseboard( BaseboardNum ).EquipName );
				}
				CheckEquipName( BaseboardNum ) = false;
			}
		}

		InitElectricBaseboard( BaseboardNum, ControlledZoneNum, FirstHVACIteration );

		{ auto const SELECT_CASE_var( ElecBaseboard( BaseboardNum ).EquipType );

		if ( SELECT_CASE_var == BaseboardRadiator_Electric ) { // 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC'
			// Simulate baseboard
			CalcElectricBaseboard( BaseboardNum, ControlledZoneNum );

		} else {
			ShowSevereError( "SimElecBaseboard: Errors in Baseboard=" + ElecBaseboard( BaseboardNum ).EquipName );
			ShowContinueError( "Invalid or unimplemented equipment type=" + cCMO_BBRadiator_Electric );
			ShowFatalError( "Preceding condition causes termination." );

		}}

		PowerMet = ElecBaseboard( BaseboardNum ).TotPower;

		UpdateElectricBaseboard( BaseboardNum );
		ReportElectricBaseboard( BaseboardNum );

	}

	void
	GetElectricBaseboardInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       Feb 2010 Daeho Kang for radiant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the Baseboard units.

		// METHODOLOGY EMPLOYED:
		// Standard input processor calls.

		// REFERENCES:
		// Hot water baseboard module

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using DataSurfaces::Surface;
		using GlobalNames::VerifyUniqueBaseboardName;
		using General::RoundSigDigits;
		using ScheduleManager::GetScheduleIndex;
		using namespace DataIPShortCuts;
		using General::TrimSigDigits;
		using DataSizing::AutoSize;
		using DataSizing::HeatingDesignCapacity;
		using DataSizing::CapacityPerFloorArea;
		using DataSizing::FractionOfAutosizedHeatingCapacity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetBaseboardInput: " ); // include trailing blank space
		Real64 const MaxFraction( 1.0 ); // Maximum limit of fractional values
		Real64 const MinFraction( 0.0 ); // Minimum limit of fractional values
		//    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20      ! Maximum number of surfaces that a baseboard heater can radiate to
		int const MinDistribSurfaces( 1 ); // Minimum number of surfaces that a baseboard heater can radiate to
		int const iHeatCAPMAlphaNum( 3 ); // get input index to HW baseboard heating capacity sizing method
		int const iHeatDesignCapacityNumericNum( 1 ); // get input index to HW baseboard heating capacity
		int const iHeatCapacityPerFloorAreaNumericNum( 2 ); // get input index to HW baseboard heating capacity per floor area sizing
		int const iHeatFracOfAutosizedCapacityNumericNum( 3 ); // get input index to HW baseboard heating capacity sizing as fraction of autozized heating capacity

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AllFracsSummed; // Sum of the fractions radiant
		int BaseboardNum;
		int NumAlphas;
		int NumNumbers;
		int SurfNum; // surface number that radiant heat delivered
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;

		cCurrentModuleObject = cCMO_BBRadiator_Electric;

		NumElecBaseboards = GetNumObjectsFound( cCurrentModuleObject );

		// object is extensible, no max args needed as IPShortCuts being used

		ElecBaseboard.allocate( NumElecBaseboards );
		CheckEquipName.allocate( NumElecBaseboards );
		ElecBaseboardNumericFields.allocate( NumElecBaseboards );
		CheckEquipName = true;

		for ( BaseboardNum = 1; BaseboardNum <= NumElecBaseboards; ++BaseboardNum ) {

			GetObjectItem( cCurrentModuleObject, BaseboardNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ElecBaseboardNumericFields( BaseboardNum ).FieldNames.allocate(NumNumbers);
			ElecBaseboardNumericFields( BaseboardNum ).FieldNames = "";
			ElecBaseboardNumericFields( BaseboardNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ElecBaseboard, &ElecBaseboardParams::EquipName, BaseboardNum, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				continue;
			}
			VerifyUniqueBaseboardName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			ElecBaseboard( BaseboardNum ).EquipName = cAlphaArgs( 1 ); // name of this baseboard
			ElecBaseboard( BaseboardNum ).EquipType = BaseboardRadiator_Electric;
			ElecBaseboard( BaseboardNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				ElecBaseboard( BaseboardNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ElecBaseboard( BaseboardNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( ElecBaseboard( BaseboardNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			// Determine HW radiant baseboard heating design capacity sizing method
			if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "HeatingDesignCapacity" ) ) {
				ElecBaseboard( BaseboardNum ).HeatingCapMethod = HeatingDesignCapacity;

				if ( !lNumericFieldBlanks( iHeatDesignCapacityNumericNum ) ) {
					ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatDesignCapacityNumericNum );
					if ( ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 && ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity != AutoSize ) {
						ShowSevereError( cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatDesignCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
				ElecBaseboard( BaseboardNum ).HeatingCapMethod = CapacityPerFloorArea;
				if ( !lNumericFieldBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
					ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatCapacityPerFloorAreaNumericNum );
					if ( ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity <= 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName);
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames (iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
					} else if ( ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
						ShowSevereError( cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName);
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "FractionOfAutosizedHeatingCapacity" ) ) {
				ElecBaseboard( BaseboardNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
				if ( !lNumericFieldBlanks( iHeatFracOfAutosizedCapacityNumericNum ) ) {
					ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum );
					if (ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0) {
						ShowSevereError(cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName);
						ShowContinueError("Illegal " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum ), 7) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError(cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName);
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + " = " + ElecBaseboard( BaseboardNum ).EquipName );
				ShowContinueError( "Illegal " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
				ErrorsFound = true;
			}

			ElecBaseboard( BaseboardNum ).BaseboardEfficiency = rNumericArgs( 4 );
			ElecBaseboard( BaseboardNum ).FracRadiant = rNumericArgs( 5 );
			if ( ElecBaseboard( BaseboardNum ).FracRadiant < MinFraction ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 5 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 2 ) + "]." );
				ElecBaseboard( BaseboardNum ).FracRadiant = MinFraction;
			}
			if ( ElecBaseboard( BaseboardNum ).FracRadiant > MaxFraction ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 5 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
				ElecBaseboard( BaseboardNum ).FracRadiant = MaxFraction;
			}

			// Remaining fraction is added to the zone as convective heat transfer
			AllFracsSummed = ElecBaseboard( BaseboardNum ).FracRadiant;
			if ( AllFracsSummed > MaxFraction ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Fraction Radiant was higher than the allowable maximum." );
				ElecBaseboard( BaseboardNum ).FracRadiant = MaxFraction;
				ElecBaseboard( BaseboardNum ).FracConvect = 0.0;
			} else {
				ElecBaseboard( BaseboardNum ).FracConvect = 1.0 - AllFracsSummed;
			}

			ElecBaseboard( BaseboardNum ).FracDistribPerson = rNumericArgs( 6 );
			if ( ElecBaseboard( BaseboardNum ).FracDistribPerson < MinFraction ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 6 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 2 ) + "]." );
				ElecBaseboard( BaseboardNum ).FracDistribPerson = MinFraction;
			}
			if ( ElecBaseboard( BaseboardNum ).FracDistribPerson > MaxFraction ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 6 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
				ElecBaseboard( BaseboardNum ).FracDistribPerson = MaxFraction;
			}

			ElecBaseboard( BaseboardNum ).TotSurfToDistrib = NumNumbers - 6;
			//      IF (ElecBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
			//        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
			//          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
			//        CALL ShowContinueError('...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))// &
			//           '] will be processed.')
			//        ElecBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
			//      END IF
			if ( ( ElecBaseboard( BaseboardNum ).TotSurfToDistrib < MinDistribSurfaces ) && ( ElecBaseboard( BaseboardNum ).FracRadiant > MinFraction ) ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", the number of surface/radiant fraction groups entered was less than the allowable minimum." );
				ShowContinueError( "...the minimum that must be entered=[" + RoundSigDigits( MinDistribSurfaces ) + "]." );
				ErrorsFound = true;
				ElecBaseboard( BaseboardNum ).TotSurfToDistrib = 0; // error
			}

			ElecBaseboard( BaseboardNum ).SurfaceName.allocate( ElecBaseboard( BaseboardNum ).TotSurfToDistrib );
			ElecBaseboard( BaseboardNum ).SurfaceName = "";
			ElecBaseboard( BaseboardNum ).SurfacePtr.allocate( ElecBaseboard( BaseboardNum ).TotSurfToDistrib );
			ElecBaseboard( BaseboardNum ).SurfacePtr = 0;
			ElecBaseboard( BaseboardNum ).FracDistribToSurf.allocate( ElecBaseboard( BaseboardNum ).TotSurfToDistrib );
			ElecBaseboard( BaseboardNum ).FracDistribToSurf = 0.0;

			AllFracsSummed = ElecBaseboard( BaseboardNum ).FracDistribPerson;
			for ( SurfNum = 1; SurfNum <= ElecBaseboard( BaseboardNum ).TotSurfToDistrib; ++SurfNum ) {
				ElecBaseboard( BaseboardNum ).SurfaceName( SurfNum ) = cAlphaArgs( SurfNum + 3 );
				ElecBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + 3 ), Surface );
				ElecBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum ) = rNumericArgs( SurfNum + 6 );
				if ( ElecBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cAlphaFieldNames( SurfNum + 3 ) + "=\"" + cAlphaArgs( SurfNum + 3 ) + "\" invalid - not found." );
					ErrorsFound = true;
				}
				if ( ElecBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum ) > MaxFraction ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 6 ) + "was greater than the allowable maximum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
					ElecBaseboard( BaseboardNum ).TotSurfToDistrib = MaxFraction;
				}
				if ( ElecBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum ) < MinFraction ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 6 ) + "was less than the allowable minimum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MinFraction, 2 ) + "]." );
					ElecBaseboard( BaseboardNum ).TotSurfToDistrib = MinFraction;
				}
				if ( ElecBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) != 0 ) {
					Surface( ElecBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) ).IntConvSurfGetsRadiantHeat = true;
				}

				AllFracsSummed += ElecBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum );
			} // Surfaces

			if ( AllFracsSummed > ( MaxFraction + 0.01 ) ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Summed radiant fractions for people + surface groups > 1.0" );
				ErrorsFound = true;
			}
			if ( ( AllFracsSummed < ( MaxFraction - 0.01 ) ) && ( ElecBaseboard( BaseboardNum ).FracRadiant > MinFraction ) ) { // User didn't distribute all of the | radiation warn that some will be lost
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Summed radiant fractions for people + surface groups < 1.0" );
				ShowContinueError( "The rest of the radiant energy delivered by the baseboard heater will be lost" );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + cCurrentModuleObject + "Errors found getting input. Program terminates." );
		}

		for ( BaseboardNum = 1; BaseboardNum <= NumElecBaseboards; ++BaseboardNum ) {

			// Setup Report variables for the Electric Baseboards
			// CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Electric'
			SetupOutputVariable( "Baseboard Total Heating Rate [W]", ElecBaseboard( BaseboardNum ).TotPower, "System", "Average", ElecBaseboard( BaseboardNum ).EquipName );

			SetupOutputVariable( "Baseboard Convective Heating Rate [W]", ElecBaseboard( BaseboardNum ).ConvPower, "System", "Average", ElecBaseboard( BaseboardNum ).EquipName );
			SetupOutputVariable( "Baseboard Radiant Heating Rate [W]", ElecBaseboard( BaseboardNum ).RadPower, "System", "Average", ElecBaseboard( BaseboardNum ).EquipName );

			SetupOutputVariable( "Baseboard Electric Energy [J]", ElecBaseboard( BaseboardNum ).ElecUseLoad, "System", "Sum", ElecBaseboard( BaseboardNum ).EquipName, _, "Electric", "HEATING", _, "System" );
			SetupOutputVariable( "Baseboard Electric Power [W]", ElecBaseboard( BaseboardNum ).ElecUseRate, "System", "Average", ElecBaseboard( BaseboardNum ).EquipName );
			SetupOutputVariable( "Baseboard Total Heating Energy [J]", ElecBaseboard( BaseboardNum ).TotEnergy, "System", "Sum", ElecBaseboard( BaseboardNum ).EquipName, _, "ENERGYTRANSFER", "BASEBOARD", _, "System" );

			SetupOutputVariable( "Baseboard Convective Heating Energy [J]", ElecBaseboard( BaseboardNum ).ConvEnergy, "System", "Sum", ElecBaseboard( BaseboardNum ).EquipName );
			SetupOutputVariable( "Baseboard Radiant Heating Energy [J]", ElecBaseboard( BaseboardNum ).RadEnergy, "System", "Sum", ElecBaseboard( BaseboardNum ).EquipName );
		}

	}

	void
	InitElectricBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       Feb 2010 Daeho Kang for radiant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the Baseboard units during simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNode;
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int ZoneNum;
		int Loop;
		static Array1D_bool MyEnvrnFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumElecBaseboards );
			MySizeFlag.allocate( NumElecBaseboards );
			ZeroSourceSumHATsurf.dimension( NumOfZones, 0.0 );
			QBBElecRadSource.dimension( NumElecBaseboards, 0.0 );
			QBBElecRadSrcAvg.dimension( NumElecBaseboards, 0.0 );
			LastQBBElecRadSrc.dimension( NumElecBaseboards, 0.0 );
			LastSysTimeElapsed.dimension( NumElecBaseboards, 0.0 );
			LastTimeStepSys.dimension( NumElecBaseboards, 0.0 );
			MyEnvrnFlag = true;
			MySizeFlag = true;

			MyOneTimeFlag = false;

		}

		if ( ElecBaseboard( BaseboardNum ).ZonePtr <= 0 ) ElecBaseboard( BaseboardNum ).ZonePtr = ZoneEquipConfig( ControlledZoneNumSub ).ActualZoneNum;

		if ( ! SysSizingCalc && MySizeFlag( BaseboardNum ) ) {
			// for each coil, do the sizing once.
			SizeElectricBaseboard( BaseboardNum );
			MySizeFlag( BaseboardNum ) = false;
		}

		// need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumElecBaseboards; ++Loop ) {
				if ( CheckZoneEquipmentList( cCMO_BBRadiator_Electric, ElecBaseboard( Loop ).EquipName ) ) continue;
				ShowSevereError( "InitBaseboard: Unit=[" + cCMO_BBRadiator_Electric + ',' + ElecBaseboard( Loop ).EquipName + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( BaseboardNum ) ) {
			// Initialize
			ZeroSourceSumHATsurf = 0.0;
			QBBElecRadSource = 0.0;
			QBBElecRadSrcAvg = 0.0;
			LastQBBElecRadSrc = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;

			MyEnvrnFlag( BaseboardNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( BaseboardNum ) = true;
		}

		// Do the Begin Day initializations
		if ( BeginDayFlag ) {

		}

		if ( BeginTimeStepFlag && FirstHVACIteration ) {
			ZoneNum = ElecBaseboard( BaseboardNum ).ZonePtr;
			ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum );
			QBBElecRadSrcAvg( BaseboardNum ) = 0.0;
			LastQBBElecRadSrc( BaseboardNum ) = 0.0;
			LastSysTimeElapsed( BaseboardNum ) = 0.0;
			LastTimeStepSys( BaseboardNum ) = 0.0;
		}

		// Do the every time step initializations
		ZoneNode = ZoneEquipConfig( ControlledZoneNumSub ).ZoneNode;
		ElecBaseboard( BaseboardNum ).AirInletTemp = Node( ZoneNode ).Temp;
		ElecBaseboard( BaseboardNum ).AirInletHumRat = Node( ZoneNode ).HumRat;

		// Set the reporting variables to zero at each timestep.
		ElecBaseboard( BaseboardNum ).TotPower = 0.0;
		ElecBaseboard( BaseboardNum ).Power = 0.0;
		ElecBaseboard( BaseboardNum ).ConvPower = 0.0;
		ElecBaseboard( BaseboardNum ).RadPower = 0.0;
		ElecBaseboard( BaseboardNum ).TotEnergy = 0.0;
		ElecBaseboard( BaseboardNum ).Energy = 0.0;
		ElecBaseboard( BaseboardNum ).ConvEnergy = 0.0;
		ElecBaseboard( BaseboardNum ).RadEnergy = 0.0;
		ElecBaseboard( BaseboardNum ).ElecUseLoad = 0.0;
		ElecBaseboard( BaseboardNum ).ElecUseRate = 0.0;

	}

	void
	SizeElectricBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing electric baseboard components for which nominal capacities have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
		// calculated by numerically inverting the baseboard calculation routine.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("SizeElectricBaseboard");

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 NominalCapacityDes; // Design capacity value for reproting
		Real64 NominalCapacityUser; // User hard-sized UA value for reproting
		bool IsAutoSize; // Indicator to autosizing nominal capacity

		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		IsAutoSize = false;
		NominalCapacityDes = 0.0;
		NominalCapacityUser = 0.0;
		DataScalableCapSizingON = false;

		if ( CurZoneEqNum > 0 ) {

			CompType = cCMO_BBRadiator_Electric;
			CompName = ElecBaseboard( BaseboardNum ).EquipName;
			DataFracOfAutosizedHeatingCapacity = 1.0;
			DataZoneNumber = ElecBaseboard( BaseboardNum ).ZonePtr;
			SizingMethod = HeatingCapacitySizing;
			FieldNum = 1;
			PrintFlag = true;
			SizingString = ElecBaseboardNumericFields( BaseboardNum ).FieldNames( FieldNum ) + " [W]";
			CapSizingMethod = ElecBaseboard( BaseboardNum ).HeatingCapMethod;
			ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
			if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
				if ( CapSizingMethod == HeatingDesignCapacity ) {
					if ( ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
						CheckZoneSizing(CompType, CompName);
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
					} else {
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity;
					}
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
				} else if ( CapSizingMethod == CapacityPerFloorArea ) {
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
					TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
					DataScalableCapSizingON = true;
				} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
					CheckZoneSizing(CompType, CompName);
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					DataFracOfAutosizedHeatingCapacity = ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity;
					ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
					TempSize = AutoSize;
					DataScalableCapSizingON = true;
				} else {
					TempSize = ElecBaseboard( BaseboardNum ).ScaledHeatingCapacity;
				}
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				ElecBaseboard( BaseboardNum ).NominalCapacity = TempSize;
				DataScalableCapSizingON = false;
			}

		}

	}

	void
	CalcElectricBaseboard(
		int const BaseboardNum,
		int const EP_UNUSED( ControlledZoneNum )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       Feb 2010 Daeho Kang for radiant component
		//                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the heat exchange rate in a Electric baseboard heater.
		// It includes radiant heat transfer to people and surfaces in a space, and the actual convective
		// system impact of a electric baseboard heater is determined after the radiant heat distribution.

		// METHODOLOGY EMPLOYED:
		// This is primarily modified from Convective Electric Baseboard. An existing algorithm of radiant
		// heat transfer calculation in the High Tmeperature Radiant System module is implemented.

		// REFERENCES:

		// Using/Aliasing
		using Psychrometrics::PsyCpAirFnWTdb;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SimpConvAirFlowSpeed( 0.5 ); // m/s
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		Real64 AirInletTemp;
		Real64 CpAir;
		Real64 AirMassFlowRate;
		Real64 CapacitanceAir;
		Real64 Effic;
		Real64 AirOutletTemp;
		Real64 QBBCap;
		Real64 RadHeat;
		Real64 QZnReq;
		Real64 LoadMet;

		ZoneNum = ElecBaseboard( BaseboardNum ).ZonePtr;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		AirInletTemp = ElecBaseboard( BaseboardNum ).AirInletTemp;
		AirOutletTemp = AirInletTemp;
		CpAir = PsyCpAirFnWTdb( ElecBaseboard( BaseboardNum ).AirInletHumRat, AirInletTemp );
		AirMassFlowRate = SimpConvAirFlowSpeed;
		CapacitanceAir = CpAir * AirMassFlowRate;

		// Currently only the efficiency is used to calculate the electric consumption.  There could be some
		// thermal loss that could be accounted for with this efficiency input.
		Effic = ElecBaseboard( BaseboardNum ).BaseboardEfficiency;

		if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) && GetCurrentScheduleValue( ElecBaseboard( BaseboardNum ).SchedPtr ) > 0.0 ) {

			// If the load exceeds the capacity than the capacity is set to the BB limit.
			if ( QZnReq > ElecBaseboard( BaseboardNum ).NominalCapacity ) {
				QBBCap = ElecBaseboard( BaseboardNum ).NominalCapacity;
			} else {
				QBBCap = QZnReq;
			}
			RadHeat = QBBCap * ElecBaseboard( BaseboardNum ).FracRadiant;
			QBBElecRadSource( BaseboardNum ) = RadHeat;

			if ( ElecBaseboard( BaseboardNum ).FracRadiant > 0.0 ) { // User defines radiant heat addition
				// Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
				DistributeBBElecRadGains();
				// Now "simulate" the system by recalculating the heat balances
				HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
				HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );
				// Here an assumption is made regarding radiant heat transfer to people.
				// While the radiant heat transfer to people array will be used by the thermal comfort
				// routines, the energy transfer to people would get lost from the perspective
				// of the heat balance.  So, to avoid this net loss of energy which clearly
				// gets added to the zones, we must account for it somehow.  This assumption
				// that all energy radiated to people is converted to convective energy is
				// not very precise, but at least it conserves energy. The system impact to heat balance
				// should include this.
				LoadMet = ( SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum ) ) + ( QBBCap * ElecBaseboard( BaseboardNum ).FracConvect ) + ( RadHeat * ElecBaseboard( BaseboardNum ).FracDistribPerson );

				if ( LoadMet < 0.0 ) { // No cooling output
					AirOutletTemp = AirInletTemp;
					ElecBaseboard( BaseboardNum ).ElecUseRate = 0.0;
				} else {
					AirOutletTemp = AirInletTemp + QBBCap / CapacitanceAir;
					// This could be utilized somehow or even reported so the data structures are left in place
					// The Baseboard electric Load is calculated using the efficiency
					ElecBaseboard( BaseboardNum ).ElecUseRate = QBBCap / Effic;
				}

			} else { // zero radiant fraction, no need of recalculation of heat balances

				LoadMet = QBBCap;
				ElecBaseboard( BaseboardNum ).ElecUseRate = QBBCap / Effic;

			}

		} else { // If there is an off condition the BB does nothing.

			QBBCap = 0.0;
			LoadMet = 0.0;
			RadHeat = 0.0;
			AirOutletTemp = AirInletTemp;
			QBBElecRadSource( BaseboardNum ) = 0.0;
			ElecBaseboard( BaseboardNum ).ElecUseRate = 0.0;
		}

		// Assign calculated ones
		ElecBaseboard( BaseboardNum ).AirOutletTemp = AirOutletTemp;
		ElecBaseboard( BaseboardNum ).Power = QBBCap;
		ElecBaseboard( BaseboardNum ).TotPower = LoadMet;
		ElecBaseboard( BaseboardNum ).RadPower = RadHeat;
		ElecBaseboard( BaseboardNum ).ConvPower = QBBCap - RadHeat;

	}

	void
	UpdateElectricBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//                      Rick Strand
		//       DATE WRITTEN   Nov 1997
		//                      February 2001
		//       MODIFIED       Feb 2010 Daeho Kang for radiant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// The update subrotines in water baseboard radiator is modified.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataGlobals::BeginEnvrnFlag;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int Iter( 0 );
		static bool MyEnvrnFlag( true );

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			Iter = 0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// First, update the running average if necessary...
		if ( LastSysTimeElapsed( BaseboardNum ) == SysTimeElapsed ) {
			QBBElecRadSrcAvg( BaseboardNum ) -= LastQBBElecRadSrc( BaseboardNum ) * LastTimeStepSys( BaseboardNum ) / TimeStepZone;
		}
		// Update the running average and the "last" values with the current values of the appropriate variables
		QBBElecRadSrcAvg( BaseboardNum ) += QBBElecRadSource( BaseboardNum ) * TimeStepSys / TimeStepZone;

		LastQBBElecRadSrc( BaseboardNum ) = QBBElecRadSource( BaseboardNum );
		LastSysTimeElapsed( BaseboardNum ) = SysTimeElapsed;
		LastTimeStepSys( BaseboardNum ) = TimeStepSys;

	}

	void
	UpdateBBElecRadSourceValAvg( bool & ElecBaseboardSysOn ) // .TRUE. if the radiant system has run this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       Feb 2010 Daeho Kang for baseboard
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To transfer the average value of the heat source over the entire
		// zone time step back to the heat balance routines so that the heat
		// balance algorithms can simulate one last time with the average source
		// to maintain some reasonable amount of continuity and energy balance
		// in the temperature and flux histories.

		// METHODOLOGY EMPLOYED:
		// All of the record keeping for the average term is done in the Update
		// routine so the only other thing that this subroutine does is check to
		// see if the system was even on.  If any average term is non-zero, then
		// one or more of the radiant systems was running.

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
		int BaseboardNum; // DO loop counter for surface index

		// FLOW:
		ElecBaseboardSysOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( QBBElecRadSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( BaseboardNum = 1; BaseboardNum <= NumElecBaseboards; ++BaseboardNum ) {
			if ( QBBElecRadSrcAvg( BaseboardNum ) != 0.0 ) {
				ElecBaseboardSysOn = true;
				break; //DO loop
			}
		}

		QBBElecRadSource = QBBElecRadSrcAvg;

		// QBBElecRadSource has been modified so we need to redistribute gains

		DistributeBBElecRadGains();

	}

	void
	DistributeBBElecRadGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       Feb 2010 Daeho Kang for baseboard
		//                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To distribute the gains from the electric basebaord heater
		// as specified in the user input file.  This includes distribution
		// of long wavelength radiant gains to surfaces and "people."

		// METHODOLOGY EMPLOYED:
		// We must cycle through all of the radiant systems because each
		// surface could feel the effect of more than one radiant system.
		// Note that the energy radiated to people is assumed to affect them
		// but them it is assumed to be convected to the air.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::QElecBaseboardToPerson;
		using DataHeatBalFanSys::QElecBaseboardSurf;
		using DataHeatBalFanSys::MaxRadHeatFlux;
		using General::RoundSigDigits;
		using DataSurfaces::Surface;
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallestArea( 0.001 ); // Smallest area in meters squared (to avoid a divide by zero)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadSurfNum; // Counter for surfaces receiving radiation from radiant heater
		int BaseboardNum; // Counter for the baseboard
		int SurfNum; // Pointer to the Surface derived type
		int ZoneNum; // Pointer to the Zone derived type
		Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

		// FLOW:
		// Initialize arrays
		QElecBaseboardSurf = 0.0;
		QElecBaseboardToPerson = 0.0;

		for ( BaseboardNum = 1; BaseboardNum <= NumElecBaseboards; ++BaseboardNum ) {

			ZoneNum = ElecBaseboard( BaseboardNum ).ZonePtr;
			QElecBaseboardToPerson( ZoneNum ) += QBBElecRadSource( BaseboardNum ) * ElecBaseboard( BaseboardNum ).FracDistribPerson;

			for ( RadSurfNum = 1; RadSurfNum <= ElecBaseboard( BaseboardNum ).TotSurfToDistrib; ++RadSurfNum ) {
				SurfNum = ElecBaseboard( BaseboardNum ).SurfacePtr( RadSurfNum );
				if ( Surface( SurfNum ).Area > SmallestArea ) {
					ThisSurfIntensity = ( QBBElecRadSource( BaseboardNum ) * ElecBaseboard( BaseboardNum ).FracDistribToSurf( RadSurfNum ) / Surface( SurfNum ).Area );
					QElecBaseboardSurf( SurfNum ) += ThisSurfIntensity;
					if ( ThisSurfIntensity > MaxRadHeatFlux ) {
						ShowSevereError( "DistributeBBElecRadGains:  excessive thermal radiation heat flux intensity detected" );
						ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
						ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
						ShowContinueError( "Occurs in " + cCMO_BBRadiator_Electric + " = " + ElecBaseboard( BaseboardNum ).EquipName );
						ShowContinueError( "Radiation intensity = " + RoundSigDigits( ThisSurfIntensity, 2 ) + " [W/m2]" );
						ShowContinueError( "Assign a larger surface area or more surfaces in " + cCMO_BBRadiator_Electric );
						ShowFatalError( "DistributeBBElecRadGains:  excessive thermal radiation heat flux intensity detected" );
					}
				} else {
					ShowSevereError( "DistributeBBElecRadGains:  surface not large enough to receive thermal radiation heat flux" );
					ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
					ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
					ShowContinueError( "Occurs in " + cCMO_BBRadiator_Electric + " = " + ElecBaseboard( BaseboardNum ).EquipName );
					ShowContinueError( "Assign a larger surface area or more surfaces in " + cCMO_BBRadiator_Electric );
					ShowFatalError( "DistributeBBElecRadGains:  surface not large enough to receive thermal radiation heat flux" );
				}
			}

		}

	}

	void
	ReportElectricBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       Feb 2010 Daeho Kang for additional variables
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

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

		ElecBaseboard( BaseboardNum ).ElecUseLoad = ElecBaseboard( BaseboardNum ).ElecUseRate * TimeStepSys * SecInHour;
		ElecBaseboard( BaseboardNum ).TotEnergy = ElecBaseboard( BaseboardNum ).TotPower * TimeStepSys * SecInHour;
		ElecBaseboard( BaseboardNum ).Energy = ElecBaseboard( BaseboardNum ).Power * TimeStepSys * SecInHour;
		ElecBaseboard( BaseboardNum ).ConvEnergy = ElecBaseboard( BaseboardNum ).ConvPower * TimeStepSys * SecInHour;
		ElecBaseboard( BaseboardNum ).RadEnergy = ElecBaseboard( BaseboardNum ).RadPower * TimeStepSys * SecInHour;

	}

	Real64
	SumHATsurf( int const ZoneNum ) // Zone number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
		// The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
		// and should be updated accordingly.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;

		// Return value
		Real64 SumHATsurf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		Real64 Area; // Effective surface area

		// FLOW:
		SumHATsurf = 0.0;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			Area = Surface( SurfNum ).Area;

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
					Area += SurfaceWindow( SurfNum ).DividerArea;
				}

				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameTempSurfIn;
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * SurfaceWindow( SurfNum ).DividerTempSurfIn;
				}
			}

			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );
		}

		return SumHATsurf;

	}

} // ElectricBaseboardRadiator

} // EnergyPlus
