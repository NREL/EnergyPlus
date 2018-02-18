// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// EnergyPlus Headers
#include <GlobalNames.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessing/InputProcessor.hh>
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
	std::unordered_map < std::string, std::string > ChillerNames;
	std::unordered_map < std::string, std::string > BoilerNames;
	std::unordered_map < std::string, std::string > BaseboardNames;
	std::unordered_map < std::string, std::string > CoilNames;
	std::unordered_map < std::string, std::string > aDUNames;

	// Functions

	void
	IntraObjUniquenessCheck(
		std::string & NameToVerify,
		std::string const & CurrentModuleObject,
		std::string const & FieldName,
		std::unordered_set< std::string > & UniqueStrings,
		bool & ErrorsFound
	)
	{
		if ( NameToVerify.empty() ) {
			ShowSevereError( "E+ object type " + CurrentModuleObject + " cannot have a blank " + FieldName + " field" );
			ErrorsFound = true;
			NameToVerify = "xxxxx";
			return;
		}

		auto const & find_string = UniqueStrings.find( NameToVerify );
		if ( find_string == UniqueStrings.end() ) {
			UniqueStrings.emplace( NameToVerify );
		} else {
			ErrorsFound = true;
			ShowSevereError( CurrentModuleObject + " has a duplicate field " + NameToVerify );
		}
	}

	bool
	VerifyUniqueInterObjectName(
		std::unordered_map< std::string, std::string > & names,
		std::string & object_name,
		std::string const & object_type,
		std::string const & field_name,
		bool & ErrorsFound
	) {
		if ( object_name.empty() ) {
			ShowSevereError("E+ object type " + object_name + " cannot have blank " + field_name + " field");
			ErrorsFound = true;
			object_name = "xxxxx";
			return true;
		}
		auto const & names_iter = names.find( object_name );
		if ( names_iter == names.end() ) {
			names.emplace( object_name, object_type );
		} else {
			ErrorsFound = true;
			ShowSevereError( object_name + " with object type " + object_type + " duplicates a name in object type " + names_iter->second );
			return true;
		}
		return false;
	}

	bool
	VerifyUniqueInterObjectName(
		std::unordered_map< std::string, std::string > & names,
		std::string & object_name,
		std::string const & object_type,
		bool & ErrorsFound
	) {
		if ( object_name.empty() ) {
			ShowSevereError("E+ object type " + object_name + " has a blank field");
			ErrorsFound = true;
			object_name = "xxxxx";
			return true;
		}
		auto const & names_iter = names.find( object_name );
		if ( names_iter == names.end() ) {
			names.emplace( object_name, object_type );
		} else {
			ErrorsFound = true;
			ShowSevereError( object_name + " with object type " + object_type + " duplicates a name in object type " + names_iter->second );
			return true;
		}
		return false;
	}

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

		ErrorFound = false;
		auto const iter = ChillerNames.find( NameToVerify );
		if ( iter != ChillerNames.end() ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Chiller Type=\"" + iter->second + "\"." );
			ShowContinueError( "...Current entry is Chiller Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			ChillerNames.emplace( NameToVerify, UtilityRoutines::MakeUPPERCase( TypeToVerify ) );
			NumChillers = static_cast< int >( ChillerNames.size() );
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

        ErrorFound = false;
        auto const iter = BaseboardNames.find( NameToVerify );
        if ( iter != BaseboardNames.end() ) {
            ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Baseboard Type=\"" + iter->second + "\"." );
            ShowContinueError( "...Current entry is Baseboard Type=\"" + TypeToVerify + "\"." );
            ErrorFound = true;
        } else {
            BaseboardNames.emplace( NameToVerify, UtilityRoutines::MakeUPPERCase( TypeToVerify ) );
            NumBaseboards = static_cast< int >( BaseboardNames.size() );
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

        ErrorFound = false;
        auto const iter = BoilerNames.find( NameToVerify );
        if ( iter != BoilerNames.end() ) {
            ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Boiler Type=\"" + iter->second + "\"." );
            ShowContinueError( "...Current entry is Boiler Type=\"" + TypeToVerify + "\"." );
            ErrorFound = true;
        } else {
            BoilerNames.emplace( NameToVerify, UtilityRoutines::MakeUPPERCase( TypeToVerify ) );
            NumBoilers = static_cast< int >( BoilerNames.size() );
        }

	}

	void
	VerifyUniqueCoilName(
		std::string const & TypeToVerify,
		std::string & NameToVerify,
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

		if ( NameToVerify.empty() ) {
			ShowSevereError( "\"" + TypeToVerify + "\" cannot have a blank field" );
			ErrorFound = true;
			NameToVerify = "xxxxx";
			return;
		}

        ErrorFound = false;
        auto const iter = CoilNames.find( NameToVerify );
        if ( iter != CoilNames.end() ) {
            ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Coil Type=\"" + iter->second + "\"." );
            ShowContinueError( "...Current entry is Coil Type=\"" + TypeToVerify + "\"." );
            ErrorFound = true;
        } else {
            CoilNames.emplace( NameToVerify, UtilityRoutines::MakeUPPERCase( TypeToVerify ) );
            NumCoils = static_cast< int >( CoilNames.size() );
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
        ErrorFound = false;
        auto const iter = aDUNames.find( NameToVerify );
        if ( iter != aDUNames.end() ) {
            ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", ADU Type=\"" + iter->second + "\"." );
            ShowContinueError( "...Current entry is Air Distribution Unit Type=\"" + TypeToVerify + "\"." );
            ErrorFound = true;
        } else {
            aDUNames.emplace( NameToVerify, UtilityRoutines::MakeUPPERCase( TypeToVerify ) );
            numAirDistUnits = static_cast< int >( aDUNames.size() );
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

		ChillerNames.clear();
		BoilerNames.clear();
		BaseboardNames.clear();
		CoilNames.clear();
		aDUNames.clear();
	}

} // GlobalNames

} // EnergyPlus
