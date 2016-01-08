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

// EnergyPlus Headers
#include <GlobalNames.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace GlobalNames {

	// Module containing the routines dealing with matching and assuring that
	// various component types are unique by name (e.g. Chillers).

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module allows for verification of uniqueness (by name) across
	// certain component names (esp. Chillers, Boilers)

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using InputProcessor::FindItemInList;
	using InputProcessor::MakeUPPERCase;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumChillers( 0 );
	int NumBoilers( 0 );
	int NumBaseboards( 0 );
	int NumCoils( 0 );
	int numAirDistUnits( 0 );
	int CurMaxChillers( 0 );
	int CurMaxBoilers( 0 );
	int CurMaxBaseboards( 0 );
	int CurMaxCoils( 0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE GlobalNames:

	// Object Data
	Array1D< ComponentNameData > ChillerNames;
	Array1D< ComponentNameData > BoilerNames;
	Array1D< ComponentNameData > BaseboardNames;
	Array1D< ComponentNameData > CoilNames;
	Array1D< ComponentNameData > aDUNames;

	// Functions

	void
	VerifyUniqueChillerName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// chillers.  If not found in the list, it is added before returning.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;
		if ( NumChillers > 0 ) Found = FindItemInList( NameToVerify, ChillerNames, &ComponentNameData::CompName, NumChillers );
		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Chiller Type=\"" + ChillerNames( Found ).CompType + "\"." );
			ShowContinueError( "...Current entry is Chiller Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumChillers == 0 ) {
				CurMaxChillers = 4;
				ChillerNames.allocate( CurMaxChillers );
			} else if ( NumChillers == CurMaxChillers ) {
				CurMaxChillers += 4;
				ChillerNames.redimension( CurMaxChillers );
			}
			++NumChillers;
			ChillerNames( NumChillers ).CompType = MakeUPPERCase( TypeToVerify );
			ChillerNames( NumChillers ).CompName = NameToVerify;
		}
	}

	void
	VerifyUniqueBaseboardName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// Baseboards.  If not found in the list, it is added before returning.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;

		if ( NumBaseboards > 0 ) Found = FindItemInList( NameToVerify, BaseboardNames, &ComponentNameData::CompName, NumBaseboards );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Baseboard Type=\"" + BaseboardNames( Found ).CompType + "\"." );
			ShowContinueError( "...Current entry is Baseboard Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumBaseboards == 0 ) {
				CurMaxBaseboards = 4;
				BaseboardNames.allocate( CurMaxBaseboards );
			} else if ( NumBaseboards == CurMaxBaseboards ) {
				CurMaxBaseboards += 4;
				BaseboardNames.redimension( CurMaxBaseboards );
			}
			++NumBaseboards;
			BaseboardNames( NumBaseboards ).CompType = TypeToVerify;
			BaseboardNames( NumBaseboards ).CompName = NameToVerify;
		}

	}

	void
	VerifyUniqueBoilerName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// Boilers.  If not found in the list, it is added before returning.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;

		if ( NumBoilers > 0 ) Found = FindItemInList( NameToVerify, BoilerNames, &ComponentNameData::CompName, NumBoilers );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Boiler Type=\"" + BoilerNames( Found ).CompType + "\"." );
			ShowContinueError( "...Current entry is Boiler Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumBoilers == 0 ) {
				CurMaxBoilers = 4;
				BoilerNames.allocate( CurMaxBoilers );
			} else if ( NumBoilers == CurMaxBoilers ) {
				CurMaxBoilers += 4;
				BoilerNames.redimension( CurMaxBoilers );
			}
			++NumBoilers;
			BoilerNames( NumBoilers ).CompType = TypeToVerify;
			BoilerNames( NumBoilers ).CompName = NameToVerify;
		}

	}

	void
	VerifyUniqueCoilName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// Coils.  If not found in the list, it is added before returning.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;

		if ( NumCoils > 0 ) Found = FindItemInList( NameToVerify, CoilNames, &ComponentNameData::CompName, NumCoils );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Coil Type=\"" + CoilNames( Found ).CompType + "\"" );
			ShowContinueError( "...Current entry is Coil Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumCoils == 0 ) {
				CurMaxCoils = 4;
				CoilNames.allocate( CurMaxCoils );
			} else if ( NumCoils == CurMaxCoils ) {
				CurMaxCoils += 4;
				CoilNames.redimension( CurMaxCoils );
			}
			++NumCoils;
			CoilNames( NumCoils ).CompType = MakeUPPERCase( TypeToVerify );
			CoilNames( NumCoils ).CompName = NameToVerify;
		}

	}

	void
	VerifyUniqueADUName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		ComponentNameData aDUData;
		ErrorFound = false;
		int Found = 0;

		if ( numAirDistUnits > 0 ) Found = FindItemInList( NameToVerify, aDUNames, &ComponentNameData::CompName, numAirDistUnits );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", ADU Type=\"" + aDUNames( Found ).CompType + "\"" );
			ShowContinueError( "...Current entry is Air Distribution Unit Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			++numAirDistUnits;
			aDUData.CompType = MakeUPPERCase( TypeToVerify );
			aDUData.CompName = NameToVerify;
			aDUNames.push_back( aDUData );
		}

	}

	// Clears the global data in GlobalNames.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumChillers = 0;
		NumBoilers = 0;
		NumBaseboards = 0;
		NumCoils = 0;
		numAirDistUnits = 0;
		CurMaxChillers = 0;
		CurMaxBoilers = 0;
		CurMaxBaseboards = 0;
		CurMaxCoils = 0;

		ChillerNames.deallocate();
		BoilerNames.deallocate();
		BaseboardNames.deallocate();
		CoilNames.deallocate();
		aDUNames.deallocate();
	}

} // GlobalNames

} // EnergyPlus
