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

// EnergyPlus Headers
#include <DataSurfaceLists.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataSurfaceLists {

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   September 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains type definitions and variables
	// associated with Radiant System Surface Groups.

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	int NumOfSurfaceLists( 0 ); // Number of surface lists in the user input file
	int NumOfSurfListVentSlab( 0 ); // Number of surface lists in the user input file
	bool SurfaceListInputsFilled( false ); // Set to TRUE after first pass through air loop

	//  CHARACTER(len=*), PARAMETER :: CurrentModuleObject = ' '
	// SUBROUTINE SPECIFICATIONS FOR MODULE DataSurfaceLists

	// Object Data
	Array1D< SurfaceListData > SurfList;
	Array1D< SlabListData > SlabList;

	// Functions

	void
	clear_state()
	{
		NumOfSurfaceLists = 0;
		NumOfSurfListVentSlab = 0;
		SurfaceListInputsFilled = false;
		SurfList.deallocate();
		SlabList.deallocate();
	}

	void
	GetSurfaceListsInputs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the surface lists for the Radiant System Surface Groups input.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::VerifyName;
		using DataHeatBalance::Zone;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject1( "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup" );
		static std::string const CurrentModuleObject2( "ZoneHVAC:VentilatedSlab:SlabGroup" );
		Real64 const FlowFractionTolerance( 0.0001 ); // Smallest deviation from unity for the sum of all fractions
		Real64 const SurfListMinFlowFrac( 0.001 ); // Minimum allowed flow fraction (to avoid divide by zero)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string Alphas; // Alpha items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		int MaxAlphas; // Maximum number of alphas for these input keywords
		int MaxNumbers; // Maximum number of numbers for these input keywords
		int NameConflict; // Used to see if a surface name matches the name of a surface list (not allowed)
		Array1D< Real64 > Numbers; // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumArgs; // Unused variable that is part of a subroutine call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		Real64 SumOfAllFractions; // Summation of all of the fractions for splitting flow (must sum to 1)
		int SurfNum; // DO loop counter for surfaces
		int ZoneForSurface; // Zone number that a particular surface is attached to
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		int Item;
		bool ErrorsFound;
		int IOStatus;
		int AlphaArray;
		int NumArray;
		int SrfList;
		bool IsNotOK;
		bool IsBlank;

		// Obtain all of the user data related to surface lists.  Need to get
		// this before getting the radiant system or ventilated slab data.

		ErrorsFound = false;
		NumOfSurfaceLists = GetNumObjectsFound( CurrentModuleObject1 );
		NumOfSurfListVentSlab = GetNumObjectsFound( CurrentModuleObject2 );

		SurfList.allocate( NumOfSurfaceLists );
		SlabList.allocate( NumOfSurfListVentSlab );

		if ( NumOfSurfaceLists > 0 ) {

			GetObjectDefMaxArgs( CurrentModuleObject1, NumArgs, MaxAlphas, MaxNumbers );
			Alphas.allocate( MaxAlphas );
			lAlphaBlanks.dimension( MaxAlphas, false );
			cAlphaFields.allocate( MaxAlphas );
			Numbers.dimension( MaxNumbers, 0.0 );
			cNumericFields.allocate( MaxNumbers );
			lNumericBlanks.dimension( MaxNumbers, false );

			for ( Item = 1; Item <= NumOfSurfaceLists; ++Item ) {

				GetObjectItem( CurrentModuleObject1, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), SurfList, Item - 1, IsNotOK, IsBlank, CurrentModuleObject1 + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}

				SurfList( Item ).Name = Alphas( 1 );
				SurfList( Item ).NumOfSurfaces = NumAlphas - 1;

				NameConflict = FindItemInList( SurfList( Item ).Name, Surface );
				if ( NameConflict > 0 ) { // A surface list has the same name as a surface--not allowed
					ShowSevereError( CurrentModuleObject1 + " = " + SurfList( Item ).Name + " has the same name as a surface; this is not allowed." );
					ErrorsFound = true;
				}

				if ( SurfList( Item ).NumOfSurfaces < 1 ) {
					ShowSevereError( CurrentModuleObject1 + " = " + SurfList( Item ).Name + " does not have any surfaces listed." );
					ErrorsFound = true;
				} else {
					SurfList( Item ).SurfName.allocate( SurfList( Item ).NumOfSurfaces );
					SurfList( Item ).SurfPtr.allocate( SurfList( Item ).NumOfSurfaces );
					SurfList( Item ).SurfFlowFrac.allocate( SurfList( Item ).NumOfSurfaces );
				}

				SumOfAllFractions = 0.0;
				for ( SurfNum = 1; SurfNum <= SurfList( Item ).NumOfSurfaces; ++SurfNum ) {
					SurfList( Item ).SurfName( SurfNum ) = Alphas( SurfNum + 1 );
					SurfList( Item ).SurfPtr( SurfNum ) = FindItemInList( Alphas( SurfNum + 1 ), Surface );
					if ( SurfList( Item ).SurfPtr( SurfNum ) == 0 ) {
						ShowSevereError( cAlphaFields( SurfNum + 1 ) + " in " + CurrentModuleObject1 + " statement not found = " + SurfList( Item ).SurfName( SurfNum ) );
						ErrorsFound = true;
					} else { // Make sure that all of the surfaces are located in the same zone
						Surface( SurfList( Item ).SurfPtr( SurfNum ) ).PartOfVentSlabOrRadiantSurface = true;
						if ( SurfNum == 1 ) {
							ZoneForSurface = Surface( SurfList( Item ).SurfPtr( SurfNum ) ).Zone;
						}
						if ( SurfNum > 1 ) {
							if ( ZoneForSurface != Surface( SurfList( Item ).SurfPtr( SurfNum ) ).Zone ) {
								ShowSevereError( "Not all surfaces in same zone for " + CurrentModuleObject1 + " = " + SurfList( Item ).Name );
								ErrorsFound = true;
							}
						}
					}
					SurfList( Item ).SurfFlowFrac( SurfNum ) = Numbers( SurfNum );
					if ( SurfList( Item ).SurfFlowFrac( SurfNum ) < SurfListMinFlowFrac ) {
						ShowSevereError( "The Flow Fraction for Surface " + SurfList( Item ).SurfName( SurfNum ) + " in Surface Group " + SurfList( Item ).Name + " is too low" );
						ShowContinueError( "Flow fraction of " + RoundSigDigits( SurfList( Item ).SurfFlowFrac( SurfNum ), 6 ) + " is less than minimum criteria = " + RoundSigDigits( SurfListMinFlowFrac, 6 ) );
						ShowContinueError( "Zero or extremely low flow fractions are not allowed. Remove this surface from the surface group or combine small surfaces together." );
						ErrorsFound = true;
					}
					SumOfAllFractions += SurfList( Item ).SurfFlowFrac( SurfNum );
				}

				if ( std::abs( SumOfAllFractions - 1.0 ) > FlowFractionTolerance ) {
					ShowSevereError( CurrentModuleObject1 + " flow fractions do not add up to unity for " + SurfList( Item ).Name );
					ErrorsFound = true;
				}

			}

			Alphas.deallocate();
			lAlphaBlanks.deallocate();
			cAlphaFields.deallocate();
			Numbers.deallocate();
			cNumericFields.deallocate();
			lNumericBlanks.deallocate();

			if ( ErrorsFound ) ShowSevereError( CurrentModuleObject1 + " errors found getting input. Program will terminate." );
		}

		if ( NumOfSurfListVentSlab > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject2, NumArgs, MaxAlphas, MaxNumbers );
			Alphas.allocate( MaxAlphas );
			lAlphaBlanks.dimension( MaxAlphas, false );
			cAlphaFields.allocate( MaxAlphas );
			Numbers.dimension( MaxNumbers, 0.0 );
			cNumericFields.allocate( MaxNumbers );
			lNumericBlanks.dimension( MaxNumbers, false );

			for ( Item = 1; Item <= NumOfSurfListVentSlab; ++Item ) {

				GetObjectItem( CurrentModuleObject2, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), SlabList, Item - 1, IsNotOK, IsBlank, CurrentModuleObject2 + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}

				SlabList( Item ).Name = Alphas( 1 );
				SlabList( Item ).NumOfSurfaces = ( ( NumAlphas - 1 ) / 4 );

				NameConflict = FindItemInList( SlabList( Item ).Name, Surface );
				if ( NameConflict > 0 ) { // A surface list has the same name as a surface--not allowed
					ShowSevereError( CurrentModuleObject2 + " = " + SlabList( Item ).Name + " has the same name as a slab; this is not allowed." );
					ErrorsFound = true;
				}

				if ( SlabList( Item ).NumOfSurfaces < 1 ) {
					ShowSevereError( CurrentModuleObject2 + " = " + SlabList( Item ).Name + " does not have any slabs listed." );
					ErrorsFound = true;
				} else {
					SlabList( Item ).ZoneName.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).ZonePtr.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).SurfName.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).SurfPtr.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).CoreDiameter.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).CoreLength.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).CoreNumbers.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).SlabInNodeName.allocate( SlabList( Item ).NumOfSurfaces );
					SlabList( Item ).SlabOutNodeName.allocate( SlabList( Item ).NumOfSurfaces );

				}

				AlphaArray = 2;
				NumArray = 1;
				for ( SurfNum = 1; SurfNum <= SlabList( Item ).NumOfSurfaces; ++SurfNum ) {
					SlabList( Item ).ZoneName( SurfNum ) = Alphas( AlphaArray );
					SlabList( Item ).ZonePtr = FindItemInList( Alphas( AlphaArray ), Zone );
					if ( SlabList( Item ).ZonePtr( SurfNum ) == 0 ) {
						ShowSevereError( cAlphaFields( AlphaArray + 1 ) + " in " + CurrentModuleObject2 + " Zone not found = " + SlabList( Item ).SurfName( SurfNum ) );
						ErrorsFound = true;
					}

					SlabList( Item ).SurfName( SurfNum ) = Alphas( AlphaArray + 1 );
					SlabList( Item ).SurfPtr( SurfNum ) = FindItemInList( Alphas( AlphaArray + 1 ), Surface );
					if ( SlabList( Item ).SurfPtr( SurfNum ) == 0 ) {
						ShowSevereError( cAlphaFields( AlphaArray + 1 ) + " in " + CurrentModuleObject2 + " statement not found = " + SlabList( Item ).SurfName( SurfNum ) );
						ErrorsFound = true;

					}
					for ( SrfList = 1; SrfList <= NumOfSurfaceLists; ++SrfList ) {
						NameConflict = FindItemInList( SlabList( Item ).SurfName( SurfNum ), SurfList( SrfList ).SurfName, SurfList( SrfList ).NumOfSurfaces );
						if ( NameConflict > 0 ) { // A slab list includes a surface on a surface list--not allowed
							ShowSevereError( CurrentModuleObject2 + "=\"" + SlabList( Item ).Name + "\", invalid surface specified." );
							ShowContinueError( "Surface=\"" + SlabList( Item ).SurfName( SurfNum ) + "\" is also on a Surface List." );
							ShowContinueError( CurrentModuleObject1 + "=\"" + SurfList( SrfList ).Name + "\" has this surface also." );
							ShowContinueError( "A surface cannot be on both lists. The models cannot operate correctly." );
							ErrorsFound = true;
						}
					}
					Surface( SlabList( Item ).SurfPtr( SurfNum ) ).PartOfVentSlabOrRadiantSurface = true;

					SlabList( Item ).CoreDiameter( SurfNum ) = Numbers( NumArray );
					SlabList( Item ).CoreLength( SurfNum ) = Numbers( NumArray + 1 );
					SlabList( Item ).CoreNumbers( SurfNum ) = Numbers( NumArray + 2 );
					SlabList( Item ).SlabInNodeName( SurfNum ) = Alphas( AlphaArray + 2 );
					SlabList( Item ).SlabOutNodeName( SurfNum ) = Alphas( AlphaArray + 3 );
					AlphaArray = 2 * ( SurfNum + 1 ) + 2 * ( ( SurfNum + 1 ) - 1 );
					NumArray = 2 * SurfNum + ( SurfNum + 1 );
				}
			}

			Alphas.deallocate();
			lAlphaBlanks.deallocate();
			cAlphaFields.deallocate();
			Numbers.deallocate();
			cNumericFields.deallocate();
			lNumericBlanks.deallocate();

			if ( ErrorsFound ) ShowSevereError( CurrentModuleObject2 + " errors found getting input. Program will terminate." );

		}

		if ( ErrorsFound ) ShowFatalError( "GetSurfaceListsInputs: Program terminates due to preceding conditions." );

	}

	int
	GetNumberOfSurfaceLists()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Acts as a target for outside routines to make sure data is gotten before using.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumberOfSurfaceLists;

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

		if ( ! SurfaceListInputsFilled ) {
			GetSurfaceListsInputs();
			SurfaceListInputsFilled = true;
		}

		NumberOfSurfaceLists = NumOfSurfaceLists;
		return NumberOfSurfaceLists;

	}

	int
	GetNumberOfSurfListVentSlab()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Acts as a target for outside routines to make sure data is gotten before using.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumberOfSurfListVentSlab;

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

		if ( ! SurfaceListInputsFilled ) {
			GetSurfaceListsInputs();
			SurfaceListInputsFilled = true;
		}

		NumberOfSurfListVentSlab = NumOfSurfListVentSlab;

		return NumberOfSurfListVentSlab;

	}

} // DataSurfaceLists

} // EnergyPlus
