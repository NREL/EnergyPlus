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
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataErrorTracking.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace BranchInputManager {

	// Module containing the routines dealing with the BRANCH and CONNECTOR
	// lists input.

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 1999
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To Get the IDD objects "BranchList", "Branch", "ConnectorList",
	// "Connector:Splitter", and "Connector:Mixer".  Also, to supply other modules/routines with
	// information about these objects.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::OutputFileBNDetails;
	using DataGlobals::DisplayExtraWarnings;
	using namespace DataLoopNode;
	using namespace DataBranchAirLoopPlant;

	// Use statements for access to subroutines in other modules
	using InputProcessor::GetNumObjectsFound;
	using InputProcessor::GetObjectItem;
	using InputProcessor::FindItemInList;
	using InputProcessor::VerifyName;
	using InputProcessor::GetObjectDefMaxArgs;
	using InputProcessor::SameString;
	using namespace NodeInputManager;
	using namespace BranchNodeConnections;

	// Data
	//MODULE PARAMETER DEFINITIONS
	std::string const cMIXER( "Connector:Mixer" );
	std::string const cSPLITTER( "Connector:Splitter" );
	static std::string const BlankString;

	//DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumOfBranchLists( 0 ); // Number of Branch Lists found in IDF
	int NumOfBranches( 0 ); // Number of Branches found in IDF
	int NumOfConnectorLists( 0 ); // Number of Connector Lists found in IDF
	int NumSplitters( 0 ); // Number of Splitters found in IDF
	int NumMixers( 0 ); // Number of Mixers found in IDF

	bool GetBranchInputFlag( true ); // Flag used to retrieve Input
	bool GetBranchListInputFlag( true ); // Flag used to retrieve Input
	bool GetSplitterInputFlag( true ); // Flag used to retrieve Input
	bool GetMixerInputFlag( true ); // Flag used to retrieve Input
	bool GetConnectorListInputFlag( true ); // Flag used to retrieve Input
	bool InvalidBranchDefinitions( false );

	std::string CurrentModuleObject; // for ease in getting objects


	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool GetBranchInputOneTimeFlag( true );
	}
	//SUBROUTINE SPECIFICATIONS FOR MODULE BranchInputManager
	//PUBLIC  TestAirPathIntegrity
	//PRIVATE TestSupplyAirPathIntegrity
	//PRIVATE TestReturnAirPathIntegrity
	//PUBLIC  MyPlantSizingIndex

	// Object Data
	Array1D< BranchListData > BranchList; // Branch List data for each Branch List
	Array1D< BranchData > Branch; // Branch Data for each Branch
	Array1D< ConnectorData > ConnectorLists; // Connector List data for each Connector List
	Array1D< SplitterData > Splitters; // Splitter Data for each Splitter
	Array1D< MixerData > Mixers; // Mixer Data for each Mixer

	// Functions
	void
	clear_state()
	{
		NumOfBranchLists = 0; // Number of Branch Lists found in IDF
		NumOfBranches = 0; // Number of Branches found in IDF
		NumOfConnectorLists = 0; // Number of Connector Lists found in IDF
		NumSplitters = 0; // Number of Splitters found in IDF
		NumMixers = 0; // Number of Mixers found in IDF
		GetBranchInputFlag = true ; // Flag used to retrieve Input
		GetBranchListInputFlag = true ; // Flag used to retrieve Input
		GetSplitterInputFlag = true ; // Flag used to retrieve Input
		GetMixerInputFlag = true ; // Flag used to retrieve Input
		GetConnectorListInputFlag = true ; // Flag used to retrieve Input
		InvalidBranchDefinitions = false ;
		GetBranchInputOneTimeFlag = true;
		BranchList.deallocate(); // Branch List data for each Branch List
		Branch.deallocate(); // Branch Data for each Branch
		ConnectorLists.deallocate(); // Connector List data for each Connector List
		Splitters.deallocate(); // Splitter Data for each Splitter
		Mixers.deallocate(); // Mixer Data for each Mixer
	}

	void
	ManageBranchInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Nov 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called from HVACManager to make sure that branch input is
		// gathered prior to need.

		// METHODOLOGY EMPLOYED:
		// na

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

		if ( GetBranchInputFlag ) {
			GetBranchInput();
			if ( GetBranchListInputFlag ) {
				GetBranchListInputFlag = false;
				GetBranchListInput();
			}
			AuditBranches( false );
			GetBranchInputFlag = false;
		}

	}

	//==================================================================================
	//   Routines that "get" data from internal branch management structure
	//==================================================================================

	void
	GetBranchList(
		std::string const & LoopName, // Name of Loop Branch List is on
		std::string const & BranchListName, // Branch List Name from Input
		int & NumBranchNames, // Number of Branches for this Branch List
		Array1S_string BranchNames, // Names of Branches on this Branch List
		std::string const & LoopType // Type of Loop Branch list is on
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       October 2001, Automatic Extensibility
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine "gets" the branch list specified in a Plant or Condenser loop and
		// returns number and names to the outside calling routine.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found; // Points to correct Branch List/Branch
		bool ErrFound; // True when error has occured (cannot find Branch List)

		ErrFound = false;

		if ( GetBranchListInputFlag ) {
			GetBranchListInputFlag = false;
			GetBranchListInput();
		}

		//  Find this BranchList in the master BranchList Names
		Found = FindItemInList( BranchListName, BranchList );
		if ( Found == 0 ) {
			ShowFatalError( "GetBranchList: BranchList Name not found=" + BranchListName );
		}

		// Set data
		if ( BranchList( Found ).LoopName == BlankString ) {
			BranchList( Found ).LoopName = LoopName;
			BranchList( Found ).LoopType = LoopType;
		} else if ( BranchList( Found ).LoopName != LoopName ) {
			ShowSevereError( "GetBranchList: BranchList Loop Name already assigned" );
			ShowContinueError( "BranchList=" + BranchList( Found ).Name + ", already assigned to loop=" + BranchList( Found ).LoopName );
			ShowContinueError( "Now requesting assignment to Loop=" + LoopName );
			ErrFound = true;
		}

		// Return data
		NumBranchNames = BranchList( Found ).NumOfBranchNames;
		if ( isize( BranchNames ) < NumBranchNames ) {
			ShowSevereError( "GetBranchList: Branch Names array not big enough to hold Branch Names" );
			ShowContinueError( "Input BranchListName=" + BranchListName + ", in Loop=" + LoopName );
			ShowContinueError( "BranchName Array size=" + TrimSigDigits( size( BranchNames ) ) + ", but input size=" + TrimSigDigits( NumBranchNames ) );
			ErrFound = true;
		} else {
			BranchNames = "";
			BranchNames( {1,NumBranchNames} ) = BranchList( Found ).BranchNames( {1,NumBranchNames} );
		}

		if ( ErrFound ) {
			ShowFatalError( "GetBranchList: preceding condition(s) causes program termination." );
		}

	}

	int
	NumBranchesInBranchList( std::string const & BranchListName )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the number of branches in a branch list so that the calling
		// routine can allocate arrays before calling GetBranchList.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NumBranchesInBranchList;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Found;

		if ( GetBranchListInputFlag ) {
			GetBranchListInputFlag = false;
			GetBranchListInput();
		}

		//  Find this BranchList in the master BranchList Names
		Found = FindItemInList( BranchListName, BranchList );
		if ( Found == 0 ) {
			ShowFatalError( "NumBranchesInBranchList: BranchList Name not found=" + BranchListName );
		}

		NumBranchesInBranchList = BranchList( Found ).NumOfBranchNames;

		return NumBranchesInBranchList;

	}

	void
	GetBranchData(
		std::string const & LoopName, // Loop Name of this Branch
		std::string const & BranchName, // Requested Branch Name
		Real64 & BranchMaxFlow, // Max Flow Rate for Branch
		int & PressCurveType, // Index of a pressure curve object
		int & PressCurveIndex, // Index of a pressure curve object
		int & NumComps, // Number of Components on Branch
		Array1S_string CompType, // Component Type for each item on Branch
		Array1S_string CompName, // Component Name for each item on Branch
		Array1S_string CompInletNodeNames, // Component Inlet Node IDs for each item on Branch
		Array1S_int CompInletNodeNums, // Component Inlet Node Numbers for each item on Branch
		Array1S_string CompOutletNodeNames, // Component Outlet Node IDs for each item on Branch
		Array1S_int CompOutletNodeNums, // Component Outlet Node Numbers for each item on Branch
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       October 2001, Automatic Extensibility
		//                      September 2012, B. Griffith, removed component control types
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the Branch Data (internal structure) for the requested
		// Branch Name and returns it in "list structure" to the calling routine.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count; // Loop Counter
		int MinCompsAllowed;

		// Object Data
		static Array1D< ComponentData > BComponents; // Component data to be returned

		// NumComps now defined on input

		BComponents.allocate( NumComps );

		GetInternalBranchData( LoopName, BranchName, BranchMaxFlow, PressCurveType, PressCurveIndex, NumComps, BComponents, ErrorsFound );

		MinCompsAllowed = min( size( CompType ), size( CompName ), size( CompInletNodeNames ), size( CompInletNodeNums ), size( CompOutletNodeNames ), size( CompOutletNodeNums ) );
		if ( MinCompsAllowed < NumComps ) {
			ShowSevereError( "GetBranchData: Component List arrays not big enough to hold Number of Components" );
			ShowContinueError( "Input BranchName=" + BranchName + ", in Loop=" + LoopName );
			ShowContinueError( "Max Component Array size=" + TrimSigDigits( MinCompsAllowed ) + ", but input size=" + TrimSigDigits( NumComps ) );
			ShowFatalError( "Program terminates due to preceding conditions." );
		}

		for ( Count = 1; Count <= NumComps; ++Count ) {
			CompType( Count ) = BComponents( Count ).CType;
			CompName( Count ) = BComponents( Count ).Name;
			CompInletNodeNames( Count ) = BComponents( Count ).InletNodeName;
			CompInletNodeNums( Count ) = BComponents( Count ).InletNode;
			CompOutletNodeNames( Count ) = BComponents( Count ).OutletNodeName;
			CompOutletNodeNums( Count ) = BComponents( Count ).OutletNode;
		}
		BComponents.deallocate();

	}

	int
	NumCompsInBranch( std::string const & BranchName )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the number of components in a branch so that the calling
		// routine can allocate arrays before calling GetBranchData.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NumCompsInBranch;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Found;

		if ( GetBranchInputFlag ) {
			GetBranchInputFlag = false;
			GetBranchInput();
		}

		Found = FindItemInList( BranchName, Branch );
		if ( Found == 0 ) {
			ShowSevereError( "NumCompsInBranch:  Branch not found=" + BranchName );
			NumCompsInBranch = 0;
		} else {
			NumCompsInBranch = Branch( Found ).NumOfComponents;
		}

		return NumCompsInBranch;

	}

	int
	GetAirBranchIndex(
		std::string const & CompType,
		std::string const & CompName
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the branch index so that the calling
		// routine can search for a fan on this branch or use branch flow for sizing.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int GetAirBranchIndex( 0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int BranchNum;
		int CompNum;
		int NumBranches;

		if ( GetBranchInputFlag ) {
			GetBranchInputFlag = false;
			GetBranchInput();
		}

		NumBranches = size( Branch );

		if ( NumBranches == 0 ) {
			ShowSevereError( "GetAirBranchIndex:  Branch not found with component = " + CompType + " \"" + CompName + "\"" );
		} else {
			for ( BranchNum = 1; BranchNum <= NumBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= Branch( BranchNum ).NumOfComponents; ++CompNum ) {
					if ( SameString( CompType, Branch( BranchNum ).Component( CompNum ).CType ) && SameString( CompName, Branch( BranchNum ).Component( CompNum ).Name ) ) {
						GetAirBranchIndex = BranchNum;
						goto BranchLoop_exit;
					}
				}
			}
			BranchLoop_exit: ;
		}

		return GetAirBranchIndex;
	}

	Real64
	GetBranchFlow( int const BranchNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the branch index so that the calling
		// routine can search for a fan on this branch or use branch flow for sizing.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Return value
		Real64 GetBranchFlow( 0.0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int NumBranches;

		if ( GetBranchInputFlag ) {
			GetBranchInputFlag = false;
			GetBranchInput();
		}

		NumBranches = size( Branch );

		if ( NumBranches == 0 ) {
			ShowSevereError( "GetBranchFlow:  Branch index not found = " + TrimSigDigits( BranchNum ) );
		} else {
			if ( BranchNum > 0 && BranchNum <= NumBranches ) {
				GetBranchFlow = Branch( BranchNum ).MaxFlowRate;
			}
		}

		return GetBranchFlow;
	}

	void
	GetBranchFanTypeName(
		int const BranchNum,
		std::string & FanType,
		std::string & FanName,
		bool & ErrFound
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the branch fan flow rate so that the calling
		// routine can either use this flow or use then branch flow for sizing.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// na

		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		int NumBranches;

		if ( GetBranchInputFlag ) {
			GetBranchInputFlag = false;
			GetBranchInput();
		}

		ErrFound = false;
		NumBranches = size( Branch );

		FanType = BlankString;
		FanName = BlankString;

		if ( NumBranches == 0 ) {
			ShowSevereError( "GetBranchFanTypeName:  Branch index not found = " + TrimSigDigits( BranchNum ) );
			ErrFound = true;
		} else {
			if ( BranchNum > 0 && BranchNum <= NumBranches ) {
				for ( CompNum = 1; CompNum <= Branch( BranchNum ).NumOfComponents; ++CompNum ) {
					if ( SameString( "Fan:OnOff", Branch( BranchNum ).Component( CompNum ).CType ) || SameString( "Fan:ConstantVolume", Branch( BranchNum ).Component( CompNum ).CType ) || SameString( "Fan:VariableVolume", Branch( BranchNum ).Component( CompNum ).CType ) ) {
						FanType = Branch( BranchNum ).Component( CompNum ).CType;
						FanName = Branch( BranchNum ).Component( CompNum ).Name;
						break;
					}
				}
				if ( FanType == BlankString ) ErrFound = true;
			} else {
				ShowSevereError( "GetBranchFanTypeName:  Branch index not found = " + TrimSigDigits( BranchNum ) );
				ErrFound = true;
			}
		}

	}

	void
	CheckBranchForOASys(
		std::string const & CompType,
		std::string const & CompName,
		bool & OASysFlag,
		bool & ErrFound
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns TRUE if the branch contains an OA System

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// na

		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CompNum; // loop counter
		int NumBranches; // number of branches
		int BranchNum; // loop index
		int AirBranchIndex( 0 ); // index to branch containing CompType, CompName

		if ( GetBranchInputFlag ) {
			GetBranchInputFlag = false;
			GetBranchInput();
		}

		ErrFound = false;
		OASysFlag = false;
		NumBranches = size( Branch );

		for ( BranchNum = 1; BranchNum <= NumBranches; ++BranchNum ) {
			for ( CompNum = 1; CompNum <= Branch( BranchNum ).NumOfComponents; ++CompNum ) {
				if ( ! SameString( CompType, Branch( BranchNum ).Component( CompNum ).CType ) && ! SameString( CompName, Branch( BranchNum ).Component( CompNum ).Name ) ) continue;
				AirBranchIndex = BranchNum;
				goto BranchLoop_exit;
			}
		}
		BranchLoop_exit: ;

		if ( AirBranchIndex == 0 ) {
			ShowSevereError( "CheckBranchForOASys:  Branch index not found = " + TrimSigDigits( AirBranchIndex ) );
			ErrFound = true;
		} else {
			if ( AirBranchIndex > 0 && AirBranchIndex <= NumBranches ) {
				for ( CompNum = 1; CompNum <= Branch( AirBranchIndex ).NumOfComponents; ++CompNum ) {
					if ( ! SameString( "AirLoopHVAC:OutdoorAirSystem", Branch( AirBranchIndex ).Component( CompNum ).CType ) ) continue;
					OASysFlag = true;
					break;
				}
			} else {
				ShowSevereError( "CheckBranchForOASys:  Branch index not found = " + TrimSigDigits( AirBranchIndex ) );
				ErrFound = true;
			}
		}

	}

	void
	GetInternalBranchData(
		std::string const & LoopName, // Loop Name for Branch
		std::string const & BranchName, // Requested Branch Name
		Real64 & BranchMaxFlow, // Max Flow Rate for Branch
		int & PressCurveType, // Index of pressure curve object
		int & PressCurveIndex, // Index of pressure curve object
		int & NumComps, // Number of Components on Branch
		Array1S< ComponentData > BComponents, // Component data returned
		bool & ErrorsFound // True when Loop Name is already assigned and this not same loop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the Branch Data (internal structure) for the requested
		// Branch Name and returns it to the calling routine.  This is used internally
		// in the module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found; // Pointer to requested Branch Name

		if ( GetBranchInputFlag ) {
			GetBranchInput();
			GetBranchInputFlag = false;
		}

		Found = FindItemInList( BranchName, Branch );
		if ( Found == 0 ) {
			ShowSevereError( "GetInternalBranchData:  Branch not found=" + BranchName );
			ErrorsFound = true;
			BranchMaxFlow = 0.0;
			NumComps = 0;
		} else {
			if ( Branch( Found ).AssignedLoopName == BlankString ) {
				Branch( Found ).AssignedLoopName = LoopName;
				BranchMaxFlow = Branch( Found ).MaxFlowRate;
				PressCurveType = Branch( Found ).PressureCurveType;
				PressCurveIndex = Branch( Found ).PressureCurveIndex;
				NumComps = Branch( Found ).NumOfComponents;
				//      IF (ALLOCATED(BComponents)) THEN
				//        DEALLOCATE(BComponents)
				//      ENDIF
				//      ALLOCATE(BComponents(NumComps))
				BComponents( {1,NumComps} ) = Branch( Found ).Component( {1,NumComps} );
			} else if ( Branch( Found ).AssignedLoopName != LoopName ) {
				ShowSevereError( "Attempt to assign branch to two different loops, Branch=" + BranchName );
				ShowContinueError( "Branch already assigned to loop=" + Branch( Found ).AssignedLoopName );
				ShowContinueError( "New attempt to assign to loop=" + LoopName );
				ErrorsFound = true;
				BranchMaxFlow = 0.0;
				NumComps = 0;
			} else {
				BranchMaxFlow = Branch( Found ).MaxFlowRate;
				PressCurveType = Branch( Found ).PressureCurveType;
				PressCurveIndex = Branch( Found ).PressureCurveIndex;
				NumComps = Branch( Found ).NumOfComponents;
				//      IF (ALLOCATED(BComponents)) THEN
				//        DEALLOCATE(BComponents)
				//      ENDIF
				//      ALLOCATE(BComponents(NumComps))
				BComponents( {1,NumComps} ) = Branch( Found ).Component( {1,NumComps} );
			}
		}

	}

	void
	GetNumSplitterMixerInConntrList(
		std::string const & LoopName, // Loop Name for this Splitter (used in error message)
		std::string const & ConnectorListName, // Requested Connector List Name
		int & NumSplitters, // Number of splitters in the loop
		int & NumMixers, // Number of mixers in the loop
		bool & ErrorsFound // if no connector list
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   April 2005
		//       MODIFIED       Linda Lawrie - September 2005
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns the number of splitter and mixers in a connector list item
		// The data is filled from the idd object 'ConnectorList'

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
		int ConnNum;

		if ( GetConnectorListInputFlag ) {
			GetConnectorListInput();
			GetConnectorListInputFlag = false;
		}

		NumSplitters = 0;
		NumMixers = 0;
		ConnNum = FindItemInList( ConnectorListName, ConnectorLists );

		if ( ConnNum > 0 ) {
			NumSplitters = ConnectorLists( ConnNum ).NumOfSplitters;
			NumMixers = ConnectorLists( ConnNum ).NumOfMixers;
		} else {
			ShowSevereError( "Ref: Loop=" + LoopName + ", Connector List not found=" + ConnectorListName );
			ErrorsFound = true;
		}

	}

	void
	GetConnectorList(
		std::string const & ConnectorListName, // Requested Connector List
		ConnectorData & Connectoid, // Returned Connector Data
		Optional_int_const NumInList // Number of the current connector in the list of connectors
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains connector data for requested connector list.  Also,
		// this subroutine gets the input for the following IDD structure:
		// ConnectorList,
		//         \memo only two connectors allowed per loop
		//         \memo if two entered, one must be Connector:Splitter and one must be Connector:Mixer
		//     A1, \field Name
		//         \required-field
		//         \reference ConnectorLists
		//     A2, \field Connector 1 Object Type
		//         \required-field
		//         \key Connector:Splitter
		//         \key Connector:Mixer
		//     A3, \field Connector 1 Name
		//         \required-field
		//     A4, \field Connector 2 Object Type
		//         \key Connector:Splitter
		//         \key Connector:Mixer
		//     A5; \field Connector 2 Name

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count; // Loop Counter

		if ( GetConnectorListInputFlag ) {
			GetConnectorListInput();
			GetConnectorListInputFlag = false;
		}

		if ( not_blank( ConnectorListName ) ) {
			Count = FindItemInList( ConnectorListName, ConnectorLists );
			if ( Count == 0 ) {
				ShowFatalError( "GetConnectorList: Connector List not found=" + ConnectorListName );
			}
			Connectoid = ConnectorLists( Count );
			if ( present( NumInList ) ) {
				Connectoid.ConnectorType( 1 ) = ConnectorLists( Count ).ConnectorType( NumInList );
				Connectoid.ConnectorName( 1 ) = ConnectorLists( Count ).ConnectorName( NumInList );
				Connectoid.ConnectorType( 2 ) = "";
				Connectoid.ConnectorName( 2 ) = "";
			}
		} else {
			Connectoid.Name = "";
			Connectoid.NumOfConnectors = 0;
			Connectoid.ConnectorType( 1 ) = "";
			Connectoid.ConnectorType( 2 ) = "";
			Connectoid.ConnectorName( 1 ) = "";
			Connectoid.ConnectorName( 2 ) = "";
		}

	}

	void
	GetLoopMixer(
		std::string const & LoopName, // Loop Name for Mixer
		std::string const & ConnectorListName, // Requested Connector List Name
		std::string & MixerName, // Name of Mixer
		bool & IsMixer, // True when Mixer is on this connector
		std::string & OutletNodeName, // Outlet Node ID
		int & OutletNodeNum, // Outlet Node Number
		int & NumInletNodes, // Number of Inlet Nodes
		Array1S_string InletNodeNames, // Inlet Node IDs
		Array1S_int InletNodeNums, // Inlet Node Numbers
		bool & ErrorsFound,
		Optional_int_const ConnectorNumber, // number of the current item in connector list
		Optional_int MixerNumber // Mixer number for this specific splitter
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       October 2001, Automatic Extensibility
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the data for the requested Connector List and returns values indicating
		// if this connector list name is a mixer or not.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::SameString;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count; // Loop Counter
		int Loop; // Loop Counter
		int NumComps; // Number of Components on this Branch
		Real64 MaxFlowRate; // Branch Max Flow Rate
		int PressCurveType;
		int PressCurveIndex;
		bool errFlag; // Error flag from RegisterNodeConnection
		int NumParams;
		int NumAlphas;
		int NumNumbers;

		// Object Data
		ConnectorData Connectoid; // Connector Data
		Array1D< ComponentData > BComponents; // Branch Component Data

		if ( GetMixerInputFlag ) {
			GetMixerInput();
			GetMixerInputFlag = false;
		}

		GetConnectorList( ConnectorListName, Connectoid, ConnectorNumber );
		if ( SameString( Connectoid.ConnectorType( 1 ), cMIXER ) ) {
			Count = FindItemInList( Connectoid.ConnectorName( 1 ), Mixers );
			if ( present( MixerNumber ) ) ++MixerNumber;
			if ( Count == 0 ) {
				ShowFatalError( "GetLoopMixer: No Mixer Found=" + Connectoid.ConnectorName( 1 ) );
			}
		} else if ( SameString( Connectoid.ConnectorType( 2 ), cMIXER ) ) {
			Count = FindItemInList( Connectoid.ConnectorName( 2 ), Mixers );
			if ( Count == 0 ) {
				ShowFatalError( "GetLoopMixer: No Mixer Found=" + Connectoid.ConnectorName( 2 ) );
			}
		} else {
			Count = 0;
		}

		// Set defaults for later error potential
		IsMixer = false;
		MixerName = BlankString;
		OutletNodeName = BlankString;
		OutletNodeNum = 0;
		NumInletNodes = 0;
		InletNodeNames = "";
		InletNodeNums = 0;

		if ( Count != 0 ) { // Build up Output list(s). For each component(?)

			// The inlet nodes for the mixer will be the last "outlet" node of
			// each corresponding inlet branch.  The outlet node for the mixer
			// will be the first "inlet" node of the outlet branch since that
			// would be the first node on the branch.
			MixerName = Mixers( Count ).Name;
			IsMixer = true;
			// The number of "components" on a Mixer is the number of branches.  This is the number of alpha arguments -1.
			GetObjectDefMaxArgs( "Branch", NumParams, NumAlphas, NumNumbers );
			BComponents.allocate( NumAlphas - 1 );
			errFlag = false;
			GetInternalBranchData( LoopName, Mixers( Count ).OutletBranchName, MaxFlowRate, PressCurveType, PressCurveIndex, NumComps, BComponents, errFlag );
			if ( errFlag ) {
				ShowContinueError( "..occurs for Connector:Mixer Name=" + Mixers( Count ).Name );
				ErrorsFound = true;
			}
			if ( NumComps > 0 ) {
				OutletNodeName = BComponents( 1 ).InletNodeName;
				OutletNodeNum = BComponents( 1 ).InletNode;
				NumInletNodes = Mixers( Count ).NumInletBranches;
				// Register this node connection because the mixer gets node information indirectly from the branch
				errFlag = false;
				RegisterNodeConnection( OutletNodeNum, NodeID( OutletNodeNum ), "Connector:Mixer", MixerName, ValidConnectionTypes( NodeConnectionType_Outlet ), 1, ObjectIsNotParent, errFlag );

				if ( NumInletNodes > isize( InletNodeNames ) || NumInletNodes > isize( InletNodeNums ) ) {
					ShowSevereError( "GetLoopMixer: Connector:Mixer=" + MixerName + " contains too many inlets for size of Inlet Array." );
					ShowContinueError( "Max array size=" + TrimSigDigits( size( InletNodeNames ) ) + ", Mixer statement inlets=" + TrimSigDigits( NumInletNodes ) );
					ShowFatalError( "Program terminates due to preceding condition." );
				}
				InletNodeNums = 0;
				InletNodeNames = "";

				for ( Loop = 1; Loop <= Mixers( Count ).NumInletBranches; ++Loop ) {
					GetInternalBranchData( LoopName, Mixers( Count ).InletBranchNames( Loop ), MaxFlowRate, PressCurveType, PressCurveIndex, NumComps, BComponents, ErrorsFound );
					if ( NumComps > 0 ) {
						InletNodeNames( Loop ) = BComponents( NumComps ).OutletNodeName;
						InletNodeNums( Loop ) = BComponents( NumComps ).OutletNode;
						// Register this node connection because the mixer gets node information indirectly from the branch
						errFlag = false;
						RegisterNodeConnection( InletNodeNums( Loop ), NodeID( InletNodeNums( Loop ) ), "Connector:Mixer", MixerName, ValidConnectionTypes( NodeConnectionType_Inlet ), 1, ObjectIsNotParent, errFlag );
					}
				}
			} else {
				// Set so cascading errors don't happen?
				IsMixer = false;
			}
			BComponents.deallocate();
		}

	}

	void
	GetLoopSplitter(
		std::string const & LoopName, // Loop Name for this Splitter
		std::string const & ConnectorListName, // Requested Connector List Name
		std::string & SplitterName, // Name of Splitter
		bool & IsSplitter, // True if splitter on this connector list
		std::string & InletNodeName, // Inlet Node ID
		int & InletNodeNum, // Inlet Node Number
		int & NumOutletNodes, // Number of Outlet Nodes
		Array1S_string OutletNodeNames, // Outlet Node IDs
		Array1S_int OutletNodeNums, // Outlet Node Numbers
		bool & ErrorsFound,
		Optional_int_const ConnectorNumber, // number of the current item in connector list
		Optional_int SplitterNumber // splitter number for this specific splitter
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       October 2001, Automatic Extensibility
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the data for the requested Connector List and returns values indicating
		// if this connector list name is a splitter or not.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::SameString;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count; // Loop Counter
		int Loop; // Loop Counter
		int NumComps; // Number of Components on this Branch
		Real64 MaxFlowRate; // Branch Max Flow Rate
		int PressCurveType;
		int PressCurveIndex;
		bool errFlag; // Error flag from RegisterNodeConnection
		int NumParams;
		int NumAlphas;
		int NumNumbers;

		// Object Data
		ConnectorData Connectoid; // Connector Data
		Array1D< ComponentData > BComponents; // Branch Component Data

		if ( GetSplitterInputFlag ) {
			GetSplitterInput();
			GetSplitterInputFlag = false;
		}

		if ( ConnectorListName == BlankString ) {
			ShowSevereError( "GetLoopSplitter: ConnectorListName is blank.  LoopName=" + LoopName );
			ShowFatalError( "Program terminates due to previous condition." );
		}
		GetConnectorList( ConnectorListName, Connectoid, ConnectorNumber );
		if ( SameString( Connectoid.ConnectorType( 1 ), cSPLITTER ) ) {
			Count = FindItemInList( Connectoid.ConnectorName( 1 ), Splitters );
			if ( present( SplitterNumber ) ) ++SplitterNumber;
			if ( Count == 0 ) {
				ShowFatalError( "GetLoopSplitter: No Splitter Found=" + Connectoid.ConnectorName( 1 ) );
			}
		} else if ( SameString( Connectoid.ConnectorType( 2 ), cSPLITTER ) ) {
			Count = FindItemInList( Connectoid.ConnectorName( 2 ), Splitters );
			if ( Count == 0 ) {
				ShowFatalError( "GetLoopSplitter: No Splitter Found=" + Connectoid.ConnectorName( 2 ) );
			}
		} else {
			Count = 0;
		}

		// Default for any errors
		SplitterName = BlankString;
		IsSplitter = false;
		InletNodeName = BlankString;
		InletNodeNum = 0;
		NumOutletNodes = 0;
		OutletNodeNames = "";
		OutletNodeNums = 0;

		if ( Count != 0 ) { // Build up Output list(s). For each component(?)

			// The inlet node for the splitter will be the last "outlet" node of the inlet
			// branch. The outlet nodes for the splitter will be the first "inlet" node of
			// each corresponding outlet branch since that would be the first node on the branch.

			SplitterName = Splitters( Count ).Name;
			IsSplitter = true;
			// The number of "components" on a Splitter is the number of branches.  This is the number of alpha arguments -1.
			GetObjectDefMaxArgs( "Branch", NumParams, NumAlphas, NumNumbers );
			BComponents.allocate( NumAlphas - 1 );
			errFlag = false;
			GetInternalBranchData( LoopName, Splitters( Count ).InletBranchName, MaxFlowRate, PressCurveType, PressCurveIndex, NumComps, BComponents, errFlag );
			if ( errFlag ) {
				ShowContinueError( "..occurs for Splitter Name=" + Splitters( Count ).Name );
				ErrorsFound = true;
			}
			if ( NumComps > 0 ) {
				InletNodeName = BComponents( NumComps ).OutletNodeName;
				InletNodeNum = BComponents( NumComps ).OutletNode;
				NumOutletNodes = Splitters( Count ).NumOutletBranches;
				// Register this node connection because the splitter gets node information indirectly from the branch
				errFlag = false;
				RegisterNodeConnection( InletNodeNum, NodeID( InletNodeNum ), "Connector:Splitter", SplitterName, ValidConnectionTypes( NodeConnectionType_Inlet ), 1, ObjectIsNotParent, errFlag );

				if ( NumOutletNodes > isize( OutletNodeNames ) || NumOutletNodes > isize( OutletNodeNums ) ) {
					ShowSevereError( "GetLoopSplitter: Connector:Splitter=" + SplitterName + " contains too many outlets for size of Outlet Array." );
					ShowContinueError( "Max array size=" + TrimSigDigits( size( OutletNodeNames ) ) + ", Splitter statement outlets=" + TrimSigDigits( NumOutletNodes ) );
					ShowFatalError( "Program terminates due to preceding condition." );
				}
				OutletNodeNums = 0;
				OutletNodeNames = "";

				for ( Loop = 1; Loop <= Splitters( Count ).NumOutletBranches; ++Loop ) {
					GetInternalBranchData( LoopName, Splitters( Count ).OutletBranchNames( Loop ), MaxFlowRate, PressCurveType, PressCurveIndex, NumComps, BComponents, ErrorsFound );
					if ( NumComps > 0 ) {
						OutletNodeNames( Loop ) = BComponents( 1 ).InletNodeName;
						OutletNodeNums( Loop ) = BComponents( 1 ).InletNode;
						// Register this node connection because the splitter gets node information indirectly from the branch
						errFlag = false;
						RegisterNodeConnection( OutletNodeNums( Loop ), NodeID( OutletNodeNums( Loop ) ), "Connector:Splitter", SplitterName, ValidConnectionTypes( NodeConnectionType_Outlet ), 1, ObjectIsNotParent, errFlag );
					}
				}
			} else {
				//  Set so cascading errors don't happen
				IsSplitter = false;
			}
			BComponents.deallocate();
		}

	}

	std::string
	GetFirstBranchInletNodeName( std::string const & BranchListName ) // Branch List name to search
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function uses the branch structure to obtain the inlet node
		// of the first branch from referenced Branch List.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string InletNodeName; // Inlet node name of first branch in branch list

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Found1; // Pointer to Branch List Name
		int Found2; // Pointer to Branch data

		if ( GetBranchListInputFlag ) {
			GetBranchListInputFlag = false;
			GetBranchListInput();
		}

		Found1 = FindItemInList( BranchListName, BranchList );
		if ( Found1 == 0 ) {
			ShowSevereError( "GetFirstBranchInletNodeName: BranchList=\"" + BranchListName + "\", not a valid BranchList Name" );
			InletNodeName = "Invalid Node Name";
		} else {
			Found2 = FindItemInList( BranchList( Found1 ).BranchNames( 1 ), Branch );
			if ( Found2 == 0 ) {
				ShowSevereError( "GetFirstBranchInletNodeName: BranchList=\"" + BranchListName + "\", Branch=\"" + BranchList( Found1 ).BranchNames( 1 ) + "\" not a valid Branch Name" );
				InletNodeName = "Invalid Node Name";
			} else {
				InletNodeName = Branch( Found2 ).Component( 1 ).InletNodeName;
			}
		}

		return InletNodeName;

	}

	std::string
	GetLastBranchOutletNodeName( std::string const & BranchListName ) // Branch List name to search
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function uses the branch structure to obtain the outlet node
		// of the last branch from referenced Branch List.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string OutletNodeName; // Outlet node name of last branch in branch list

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Found1; // Pointer to Branch List Name
		int Found2; // Pointer to Branch data

		if ( GetBranchListInputFlag ) {
			GetBranchListInputFlag = false;
			GetBranchListInput();
		}

		Found1 = FindItemInList( BranchListName, BranchList );
		if ( Found1 == 0 ) {
			ShowSevereError( "GetLastBranchOutletNodeName: BranchList=\"" + BranchListName + "\", not a valid BranchList Name" );
			OutletNodeName = "Invalid Node Name";
		} else {
			Found2 = FindItemInList( BranchList( Found1 ).BranchNames( BranchList( Found1 ).NumOfBranchNames ), Branch );
			if ( Found2 == 0 ) {
				ShowSevereError( "GetLastBranchOutletNodeName: BranchList=\"" + BranchListName + "\", Branch=\"" + BranchList( Found1 ).BranchNames( BranchList( Found1 ).NumOfBranchNames ) + "\" not a valid Branch Name" );
				OutletNodeName = "Invalid Node Name";
			} else {
				OutletNodeName = Branch( Found2 ).Component( Branch( Found2 ).NumOfComponents ).OutletNodeName;
			}
		}

		return OutletNodeName;

	}

	void
	CheckSystemBranchFlow(
		std::string const & SystemType, // type of air loop equipment
		std::string const & SystemName, // name of air loop equipment
		Real64 & BranchFlow, // branch volumetric flow rate [m3/s]
		Real64 const BranchFanFlow, // branch flow rate [m3/s]
		bool & ErrFound // logical error flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is used to check the branch flow rate with respect to system flow rate

		// METHODOLOGY EMPLOYED:
		// Obtains branch and branch fan flow rate.
		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataHVACGlobals::SmallAirVolFlow;
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
		int BranchNum; // Index to branch on air loop
		int SizeBranch; // size of branch list
		bool OASysFlag; // TRUE when outdoor air system exists
		std::string BranchName; // name of air loop branch

		if ( GetBranchInputFlag ) {
			GetBranchInputFlag = false;
			GetBranchInput();
		}

		SizeBranch = size( Branch );
		BranchNum = GetAirBranchIndex( SystemType, SystemName );
		BranchName = "";
		BranchFlow = 0.0;
		ErrFound = false;
		OASysFlag = false;

		if ( BranchNum > 0 && BranchNum <= SizeBranch ) {
			BranchFlow = Branch( BranchNum ).MaxFlowRate;
			BranchName = Branch( BranchNum ).Name;
		} else {
			ErrFound = true;
			ShowSevereError( "CheckSystemBranchFlow: Branch index not found = " + TrimSigDigits( BranchNum ) );
			ShowContinueError( "Branch search for system type and name = " + SystemType + " \"" + SystemName + "\"" );
		}

		if ( BranchFanFlow > 0.0 && ! ErrFound ) {
			if ( BranchFlow != AutoSize ) {
				CheckBranchForOASys( SystemType, SystemName, OASysFlag, ErrFound );
				if ( std::abs( BranchFlow - BranchFanFlow ) > SmallAirVolFlow && OASysFlag ) {
					ShowWarningError( "Branch maximum flow rate differs from system flow rate." );
					ShowContinueError( "Branch = " + BranchName + " has volume flow rate = " + TrimSigDigits( BranchFlow, 6 ) + " m3/s." );
					ShowContinueError( "System = " + SystemType + " \"" + SystemName + "\" has volume flow rate = " + TrimSigDigits( BranchFanFlow, 6 ) + " m3/s." );
					ShowContinueError( "A branch flow rate that is different from the system flow rate can cause discrepancies with outdoor air control." );
				}
			}
		}

	}

	//==================================================================================
	//   Routines that get the input for the internal branch management structure
	//==================================================================================

	void
	GetBranchInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       October 2001, Automatic Extensibility
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the following IDD structure:
		// Branch,
		//         \extensible:5 Just duplicate last 5 fields and \ comments (changing numbering, please)
		//         \memo List components on the branch in simulation and connection order
		//         \memo Note: this should NOT include splitters or mixers which define
		//         \memo endpoints of branches
		//    A1,  \field Name
		//         \required-field
		//         \reference Branches
		//    N1, \field Maximum Flow Rate
		//         \default 0
		//         \units m3/s
		//         \minimum 0
		//         \autosizable
		//    A2, \field Pressure Curve Name
		//         \type object-list
		//         \reference AllCurves
		//    A3, \field Component 1 Object Type
		//         \required-field
		//    A4, \field Component 1 Name
		//         \required-field
		//    A5, \field Component 1 Inlet Node Name
		//         \required-field
		//    A6, \field Component 1 Outlet Node Name
		//         \required-field
		//    A7, \field Component 1 Branch Control Type
		//         \required-field
		//        \type choice
		//        \key Active
		//        \key Passive
		//        \key SeriesActive
		//        \key Bypass
		//        \note for ACTIVE, Component tries to set branch flow and turns off branch if the component is off
		//        \note for PASSIVE, Component does not try to set branch flow
		//        \note for SERIESACTIVE, component is active but does not turn off branch when the component is off
		//        \note for BYPASS,  Component designates a loop bypass

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using CurveManager::GetPressureCurveTypeAndIndex;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetBranchInput: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//////////// hoisted into namespace changed GetBranchInputOneTimeFlag////////////
		// static bool GetInputFlag( true ); // Set for first time call
		////////////////////////////////////////////////
		int Count; // Loop Counter
		int BCount; // Actual Num of Branches
		int Comp; // Loop Counter
		int Loop; // Loop Counter
		int NumNodes; // Number of Nodes from NodeInputManager
		Array1D_int NodeNums; // Possible Array of Node Numbers (only 1 allowed)
		bool ErrFound; // Flag for error detection
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumInComps; // Number of components actually verified (no SPLITTER or MIXER allowed)
		int NumAlphas; // Used to retrieve names from IDF
		Array1D_string Alphas; // Used to retrieve names from IDF
		int NumNumbers; // Used to retrieve numbers from IDF
		Array1D< Real64 > Numbers; // Used to retrieve numbers from IDF
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D_bool lNumericBlanks;
		Array1D_bool lAlphaBlanks;
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams;
		int ConnectionType; // Used to pass variable node connection type to GetNodeNums
		int PressureCurveType;
		int PressureCurveIndex;

		if ( GetBranchInputOneTimeFlag ) {
			CurrentModuleObject = "Branch";
			NumOfBranches = GetNumObjectsFound( CurrentModuleObject );
			if ( NumOfBranches > 0 ) {
				Branch.allocate( NumOfBranches );
				for ( auto & e : Branch ) e.AssignedLoopName.clear();
				ErrFound = false;
				GetObjectDefMaxArgs( "NodeList", NumParams, NumAlphas, NumNumbers );
				NodeNums.dimension( NumParams, 0 );
				GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
				Alphas.allocate( NumAlphas );
				Numbers.dimension( NumNumbers, 0.0 );
				cAlphaFields.allocate( NumAlphas );
				cNumericFields.allocate( NumNumbers );
				lAlphaBlanks.dimension( NumAlphas, true );
				lNumericBlanks.dimension( NumNumbers, true );
				BCount = 0;
				for ( Count = 1; Count <= NumOfBranches; ++Count ) {
					GetObjectItem( CurrentModuleObject, Count, Alphas, NumAlphas, Numbers, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( Alphas( 1 ), Branch, BCount, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrFound = true;
						if ( IsBlank ) {
							continue;
						} else {
							Alphas( 1 ) = Alphas( 1 ) + "--dup";
						}
					}
					++BCount;
					Branch( BCount ).Name = Alphas( 1 );
					Branch( BCount ).MaxFlowRate = Numbers( 1 );
					GetPressureCurveTypeAndIndex( Alphas( 2 ), PressureCurveType, PressureCurveIndex );
					if ( PressureCurveType == PressureCurve_Error ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
						ShowContinueError( "..Invalid " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
						ShowContinueError( "This curve could not be found in the input deck.  Ensure that this curve has been entered" );
						ShowContinueError( " as either a Curve:Functional:PressureDrop or one of Curve:{Linear,Quadratic,Cubic,Exponent}" );
						ShowContinueError( "This error could be caused by a misspelled curve name" );
						ErrFound = true;
					}
					Branch( BCount ).PressureCurveType = PressureCurveType;
					Branch( BCount ).PressureCurveIndex = PressureCurveIndex;
					Branch( BCount ).NumOfComponents = ( NumAlphas - 2 ) / 5;
					if ( Branch( BCount ).NumOfComponents * 5 != ( NumAlphas - 2 ) ) ++Branch( BCount ).NumOfComponents;
					NumInComps = Branch( BCount ).NumOfComponents;
					Branch( BCount ).Component.allocate( Branch( BCount ).NumOfComponents );
					Comp = 1;
					for ( Loop = 3; Loop <= NumAlphas; Loop += 5 ) {
						if ( SameString( Alphas( Loop ), cSPLITTER ) || SameString( Alphas( Loop ), cMIXER ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
							ShowContinueError( "Connector:Splitter/Connector:Mixer not allowed in object " + CurrentModuleObject );
							ErrFound = true;
							continue;
						}
						if ( Comp > NumInComps ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
							ShowContinueError( "...Number of Arguments indicate [" + RoundSigDigits( NumInComps ) + "], but count of fields indicates [" + RoundSigDigits( Comp ) + ']' );
							ShowContinueError( "...examine " + CurrentModuleObject + " carefully." );
							continue;
						}
						Branch( BCount ).Component( Comp ).CType = Alphas( Loop );
						Branch( BCount ).Component( Comp ).Name = Alphas( Loop + 1 );
						ValidateComponent( Alphas( Loop ), Alphas( Loop + 1 ), IsNotOK, CurrentModuleObject );
						if ( IsNotOK ) {
							ShowContinueError( "Occurs on " + CurrentModuleObject + '=' + Alphas( 1 ) );
							ErrFound = true;
						}
						Branch( BCount ).Component( Comp ).InletNodeName = Alphas( Loop + 2 );
						// If first component on branch, then inlet node is inlet to branch, otherwise node is internal
						if ( Loop == 3 ) {
							ConnectionType = NodeConnectionType_Inlet;
						} else {
							ConnectionType = NodeConnectionType_Internal;
						}
						if ( ! lAlphaBlanks( Loop + 2 ) ) {
							GetNodeNums( Branch( BCount ).Component( Comp ).InletNodeName, NumNodes, NodeNums, ErrFound, NodeType_Unknown, CurrentModuleObject, Branch( BCount ).Name, ConnectionType, 1, ObjectIsParent, _, cAlphaFields( Loop + 2 ) );
							if ( NumNodes > 1 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
								ShowContinueError( "..invalid " + cAlphaFields( Loop + 2 ) + "=\"" + Branch( BCount ).Component( Comp ).InletNodeName + "\" must be a single node - appears to be a list." );
								ShowContinueError( "Occurs on " + cAlphaFields( Loop ) + "=\"" + Alphas( Loop ) + "\", " + cAlphaFields( Loop + 1 ) + "=\"" + Alphas( Loop + 1 ) + "\"." );
								ErrFound = true;
							} else {
								Branch( BCount ).Component( Comp ).InletNode = NodeNums( 1 );
							}
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
							ShowContinueError( "blank required field: " + cAlphaFields( Loop + 2 ) );
							ShowContinueError( "Occurs on " + cAlphaFields( Loop ) + "=\"" + Alphas( Loop ) + "\", " + cAlphaFields( Loop + 1 ) + "=\"" + Alphas( Loop + 1 ) + "\"." );
							ErrFound = true;
						}
						Branch( BCount ).Component( Comp ).OutletNodeName = Alphas( Loop + 3 );
						// If last component on branch, then outlet node is outlet from branch, otherwise node is internal
						if ( Loop == NumAlphas - 4 ) {
							ConnectionType = NodeConnectionType_Outlet;
						} else {
							ConnectionType = NodeConnectionType_Internal;
						}
						if ( ! lAlphaBlanks( Loop + 3 ) ) {
							GetNodeNums( Branch( BCount ).Component( Comp ).OutletNodeName, NumNodes, NodeNums, ErrFound, NodeType_Unknown, CurrentModuleObject, Branch( BCount ).Name, ConnectionType, 1, ObjectIsParent, _, cAlphaFields( Loop + 3 ) );
							if ( NumNodes > 1 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
								ShowContinueError( "..invalid " + cAlphaFields( Loop + 2 ) + "=\"" + Branch( BCount ).Component( Comp ).InletNodeName + "\" must be a single node - appears to be a list." );
								ShowContinueError( "Occurs on " + cAlphaFields( Loop ) + "=\"" + Alphas( Loop ) + "\", " + cAlphaFields( Loop + 1 ) + "=\"" + Alphas( Loop + 1 ) + "\"." );
								ErrFound = true;
							} else {
								Branch( BCount ).Component( Comp ).OutletNode = NodeNums( 1 );
							}
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid data." );
							ShowContinueError( "blank required field: " + cAlphaFields( Loop + 3 ) );
							ShowContinueError( "Occurs on " + cAlphaFields( Loop ) + "=\"" + Alphas( Loop ) + "\", " + cAlphaFields( Loop + 1 ) + "=\"" + Alphas( Loop + 1 ) + "\"." );
							ErrFound = true;
						}

						if ( ! lAlphaBlanks( Loop ) && ! lAlphaBlanks( Loop + 1 ) && ! lAlphaBlanks( Loop + 2 ) && ! lAlphaBlanks( Loop + 3 ) ) SetUpCompSets( CurrentModuleObject, Branch( BCount ).Name, Alphas( Loop ), Alphas( Loop + 1 ), Alphas( Loop + 2 ), Alphas( Loop + 3 ) ); //no blanks in required field set

						//            deprecated control type, was using (Alphas(Loop+4))

						++Comp;
					}
					Branch( BCount ).NumOfComponents = NumInComps;
				}

				NumOfBranches = BCount;
				NodeNums.deallocate();
				Alphas.deallocate();
				Numbers.deallocate();
				cAlphaFields.deallocate();
				cNumericFields.deallocate();
				lAlphaBlanks.deallocate();
				lNumericBlanks.deallocate();
				if ( ErrFound ) {
					ShowSevereError( RoutineName + " Invalid " + CurrentModuleObject + " Input, preceding condition(s) will likely cause termination." );
					InvalidBranchDefinitions = true;
				}
				TestInletOutletNodes( ErrFound );
				GetBranchInputOneTimeFlag = false;
			}
		}

	}

	void
	GetBranchListInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the branch list input and fills up the structures for
		// branch lists.
		// This subroutine gets the input for the following IDD structure:
		// BRANCH LIST,
		//  \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
		//  \memo Branches MUST be listed in flow order: inlet branch, then parallel branches, then outlet branch.
		//  \memo Branches are simulated in the order listed.  Branch names cannot be duplicated within a single branch list.
		//    A1, \field Branch List Name
		//        \required-field
		//        \reference BranchLists
		//    A2, \field Branch Name 1
		//        \required-field
		//        \type object-list
		//        \object-list Branches
		//    A3, \field Branch Name 2
		//        \type object-list
		//        \object-list Branches

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetBranchListInput: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count; // Loop Counter
		int BCount; // Actual Branch List Count
		int Loop; // Loop Counter
		int Found; // Points to correct Branch List/Branch
		bool ErrFound; // True when error has occured (cannot find Branch List)
		// Following are needed because routine calls GetBranchInput
		// which would overwrite the module Alphas and NumAlphas
		bool IsNotOK; // Flag for "VerifyName" routine
		bool IsBlank; // Flag for "blank" name
		int NumAlphas; // Used to retrieve Branch list from IDF
		Array1D_string Alphas; // Used to retrieve names from IDF
		int NumNumbers;
		Array1D< Real64 > Numbers; // Not used in this object
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D_bool lNumericBlanks;
		Array1D_bool lAlphaBlanks;
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams;
		std::string TestName;

		ErrFound = false;
		CurrentModuleObject = "BranchList";
		NumOfBranchLists = GetNumObjectsFound( CurrentModuleObject );
		BranchList.allocate( NumOfBranchLists );
		for ( auto & e : BranchList ) {
			e.LoopName.clear();
			e.LoopType.clear();
		}
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Numbers.dimension( NumNumbers, 0.0 );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		if ( NumNumbers > 0 ) {
			ShowSevereError( RoutineName + CurrentModuleObject + " Object definition contains numbers, cannot be decoded by GetBranchListInput routine." );
			ErrFound = true;
		}
		BCount = 0;
		for ( Count = 1; Count <= NumOfBranchLists; ++Count ) {
			CurrentModuleObject = "BranchList";
			GetObjectItem( CurrentModuleObject, Count, Alphas, NumAlphas, Numbers, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), BranchList, BCount, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrFound = true;
				continue;
			}

			++BCount;
			BranchList( BCount ).Name = Alphas( 1 );
			BranchList( BCount ).NumOfBranchNames = NumAlphas - 1;
			BranchList( BCount ).BranchNames.allocate( NumAlphas - 1 );
			if ( BranchList( BCount ).NumOfBranchNames == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + BranchList( BCount ).Name + "\", No branch names entered." );
				ErrFound = true;
			} else {
				BranchList( BCount ).BranchNames( {1,NumAlphas - 1} ) = Alphas( {2,NumAlphas} );
				for ( Loop = 1; Loop <= BranchList( BCount ).NumOfBranchNames; ++Loop ) {
					// If NumOfBranches = 0 then Branches havent been read yet.
					if ( NumOfBranches == 0 ) {
						GetBranchInput();
					}
					if ( ! BranchList( BCount ).BranchNames( Loop ).empty() ) {
						Found = FindItemInList( BranchList( BCount ).BranchNames( Loop ), Branch );
						if ( Found == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + BranchList( BCount ).Name + "\", invalid data." );
							ShowContinueError( "..invalid Branch Name not found=\"" + BranchList( BCount ).BranchNames( Loop ) + "\"." );
							ErrFound = true;
						}
					}
				}
			}
		}

		// Check for duplicate names specified in Branch Lists
		for ( Count = 1; Count <= NumOfBranchLists; ++Count ) {
			if ( BranchList( Count ).NumOfBranchNames == 0 ) continue;
			TestName = BranchList( Count ).BranchNames( 1 );
			for ( Loop = 2; Loop <= BranchList( Count ).NumOfBranchNames; ++Loop ) {
				if ( TestName != BranchList( Count ).BranchNames( Loop ) ) continue;
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + BranchList( BCount ).Name + "\", invalid data." );
				ShowContinueError( "..invalid: duplicate branch name specified in the list." );
				ShowContinueError( "..Branch Name=" + TestName );
				ShowContinueError( "..Branch Name #" + TrimSigDigits( Loop ) + " is duplicate." );
				ErrFound = true;
			}
		}

		if ( ErrFound ) {
			ShowSevereError( RoutineName + " Invalid Input -- preceding condition(s) will likely cause termination." );
		}
		NumOfBranchLists = BCount;
		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	void
	GetConnectorListInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains connector list input from IDF.
		// ConnectorList,
		//         \memo only two connectors allowed per loop
		//         \memo if two entered, one must be Connector:Splitter and one must be Connector:Mixer
		//     A1, \field Name
		//         \required-field
		//         \reference ConnectorLists
		//     A2, \field Connector 1 Object Type
		//         \required-field
		//         \key Connector:Splitter
		//         \key Connector:Mixer
		//     A3, \field Connector 1 Name
		//         \required-field
		//     A4, \field Connector 2 Object Type
		//         \key Connector:Splitter
		//         \key Connector:Mixer
		//     A5; \field Connector 2 Name
		//  This is in the process of possibly being extended, thus the code herein.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

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
		int Count; // Loop Counter
		int NumAlphas; // Used to retrieve names from IDF
		Array1D_string Alphas; // Used to retrieve names from IDF
		int NumNumbers; // Used to retrieve numbers from IDF
		Array1D< Real64 > Numbers; // Used to retrieve numbers from IDF
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D_bool lNumericBlanks;
		Array1D_bool lAlphaBlanks;
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams;
		int NumConnectors;
		int CCount;
		int Arg;
		int SplitNum;
		int MixerNum;
		Array1D_string BranchNames;
		int NumBranchNames;
		bool ErrorsFound;
		int Loop;
		int Loop1;
		int Loop2;
		bool CurMixer;
		bool CurSplitter;
		int TestNum;
		bool MatchFound;

		if ( ! GetConnectorListInputFlag ) return;
		ErrorsFound = false;
		CurrentModuleObject = "ConnectorList";
		NumOfConnectorLists = GetNumObjectsFound( CurrentModuleObject );
		ConnectorLists.allocate( NumOfConnectorLists );
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		if ( NumAlphas != 5 || NumNumbers != 0 ) {
			ShowWarningError( "GetConnectorList: Illegal \"extension\" to " + CurrentModuleObject + " object. Internal code does not support > 2 connectors (Connector:Splitter and Connector:Mixer)" );
		}
		Alphas.allocate( NumAlphas );
		Numbers.dimension( NumNumbers, 0.0 );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );
		for ( Count = 1; Count <= NumOfConnectorLists; ++Count ) {
			GetObjectItem( CurrentModuleObject, Count, Alphas, NumAlphas, Numbers, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			ConnectorLists( Count ).Name = Alphas( 1 );
			NumConnectors = ( NumAlphas - 1 ) / 2; // potential problem if puts in type but not name
			if ( mod( NumAlphas - 1, 2 ) != 0 ) ++NumConnectors;
			ConnectorLists( Count ).NumOfConnectors = NumConnectors;
			ConnectorLists( Count ).ConnectorType.allocate( NumConnectors );
			ConnectorLists( Count ).ConnectorName.allocate( NumConnectors );
			ConnectorLists( Count ).ConnectorMatchNo.allocate( NumConnectors );
			ConnectorLists( Count ).ConnectorType = "UNKNOWN";
			ConnectorLists( Count ).ConnectorName = "UNKNOWN";
			ConnectorLists( Count ).ConnectorMatchNo = 0;
			ConnectorLists( Count ).NumOfSplitters = 0;
			ConnectorLists( Count ).NumOfMixers = 0;

			CCount = 0;
			for ( Arg = 2; Arg <= NumAlphas; Arg += 2 ) {
				++CCount;
				if ( SameString( Alphas( Arg ), cSPLITTER ) ) {
					ConnectorLists( Count ).ConnectorType( CCount ) = Alphas( Arg ).substr( 0, 30 );
					++ConnectorLists( Count ).NumOfSplitters;
				} else if ( SameString( Alphas( Arg ), cMIXER ) ) {
					ConnectorLists( Count ).ConnectorType( CCount ) = Alphas( Arg ).substr( 0, 30 );
					++ConnectorLists( Count ).NumOfMixers;
				} else {
					ShowWarningError( "GetConnectorListInput: Invalid " + cAlphaFields( Arg ) + '=' + Alphas( Arg ) + " in " + CurrentModuleObject + '=' + Alphas( 1 ) );
				}
				ConnectorLists( Count ).ConnectorName( CCount ) = Alphas( Arg + 1 );
			}
		}
		GetConnectorListInputFlag = false;
		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		// Validity checks on Connector Lists
		if ( GetSplitterInputFlag ) {
			GetSplitterInput();
			GetSplitterInputFlag = false;
		}
		if ( GetMixerInputFlag ) {
			GetMixerInput();
			GetMixerInputFlag = false;
		}

		SplitNum = 0;
		MixerNum = 0;
		for ( Count = 1; Count <= NumOfConnectorLists; ++Count ) {
			if ( ConnectorLists( Count ).NumOfConnectors <= 1 ) continue; // Air Loop only has one.
			if ( ConnectorLists( Count ).NumOfConnectors > 2 ) continue; // Rules not clear for this case
			for ( Loop = 1; Loop <= ConnectorLists( Count ).NumOfConnectors; ++Loop ) {
				if ( ConnectorLists( Count ).ConnectorMatchNo( Loop ) != 0 ) continue;
				if ( SameString( ConnectorLists( Count ).ConnectorType( Loop ), cSPLITTER ) ) {
					CurSplitter = true;
					CurMixer = false;
					SplitNum = FindItemInList( ConnectorLists( Count ).ConnectorName( Loop ), Splitters );
					// Following code sets up branch names to be matched from Splitter/Mixer data structure
					if ( SplitNum == 0 ) {
						ShowSevereError( "Invalid Connector:Splitter(none)=" + ConnectorLists( Count ).ConnectorName( Loop ) + ", referenced by " + CurrentModuleObject + '=' + ConnectorLists( Count ).Name );
						ErrorsFound = true;
						continue;
					}
					NumBranchNames = Splitters( SplitNum ).NumOutletBranches;
					BranchNames = Splitters( SplitNum ).OutletBranchNames;
				} else if ( SameString( ConnectorLists( Count ).ConnectorType( Loop ), cMIXER ) ) {
					CurSplitter = true;
					CurMixer = false;
					MixerNum = FindItemInList( ConnectorLists( Count ).ConnectorName( Loop ), Mixers );
					if ( MixerNum == 0 ) {
						ShowSevereError( "Invalid Connector:Mixer(none)=" + ConnectorLists( Count ).ConnectorName( Loop ) + ", referenced by " + CurrentModuleObject + '=' + ConnectorLists( Count ).Name );
						ErrorsFound = true;
						continue;
					}
					NumBranchNames = Mixers( MixerNum ).NumInletBranches;
					BranchNames = Mixers( MixerNum ).InletBranchNames;
				} else {
					continue;
				}
				// Try to match mixer to splitter
				for ( Loop1 = Loop + 1; Loop1 <= ConnectorLists( Count ).NumOfConnectors; ++Loop1 ) {
					if ( CurMixer && ! SameString( ConnectorLists( Count ).ConnectorType( Loop1 ), cSPLITTER ) ) continue;
					if ( CurSplitter && ! SameString( ConnectorLists( Count ).ConnectorType( Loop1 ), cMIXER ) ) continue;
					if ( ConnectorLists( Count ).ConnectorMatchNo( Loop1 ) != 0 ) continue;
					{ auto const SELECT_CASE_var( CurSplitter );
					if ( SELECT_CASE_var ) {
						// Current "item" is a splitter, candidate is a mixer.
						MixerNum = FindItemInList( ConnectorLists( Count ).ConnectorName( Loop1 ), Mixers );
						if ( MixerNum == 0 ) continue;
						if ( Mixers( MixerNum ).NumInletBranches != NumBranchNames ) continue;
						MatchFound = true;
						for ( Loop2 = 1; Loop2 <= Mixers( MixerNum ).NumInletBranches; ++Loop2 ) {
							TestNum = FindItemInList( Mixers( MixerNum ).InletBranchNames( Loop2 ), BranchNames, NumBranchNames );
							if ( TestNum == 0 ) {
								MatchFound = false;
								break;
							}
						}
						if ( MatchFound ) {
							ConnectorLists( Count ).ConnectorMatchNo( Loop1 ) = MixerNum;
							ConnectorLists( Count ).ConnectorMatchNo( Loop ) = SplitNum;
						}
					} else {
						// Current "item" is a splitter, candidate is a mixer.
						SplitNum = FindItemInList( ConnectorLists( Count ).ConnectorName( Loop1 ), Splitters );
						if ( SplitNum == 0 ) continue;
						if ( Splitters( SplitNum ).NumOutletBranches != NumBranchNames ) continue;
						MatchFound = true;
						for ( Loop2 = 1; Loop2 <= Splitters( SplitNum ).NumOutletBranches; ++Loop2 ) {
							TestNum = FindItemInList( Splitters( SplitNum ).OutletBranchNames( Loop2 ), BranchNames, NumBranchNames );
							if ( TestNum == 0 ) {
								MatchFound = false;
								break;
							}
						}
						if ( MatchFound ) {
							ConnectorLists( Count ).ConnectorMatchNo( Loop1 ) = SplitNum;
							ConnectorLists( Count ).ConnectorMatchNo( Loop ) = MixerNum;
						}
					}}
				}
				BranchNames.deallocate();
			}
		}

		for ( Count = 1; Count <= NumOfConnectorLists; ++Count ) {
			if ( ConnectorLists( Count ).NumOfConnectors <= 1 ) continue; // Air Loop only has one.
			if ( ConnectorLists( Count ).NumOfConnectors > 2 ) continue; // Rules not clear
			for ( Loop = 1; Loop <= ConnectorLists( Count ).NumOfConnectors; ++Loop ) {
				if ( ConnectorLists( Count ).ConnectorMatchNo( Loop ) != 0 ) continue;
				//  = 0, not matched.
				ShowSevereError( "For " + CurrentModuleObject + '=' + ConnectorLists( Count ).Name );
				ShowContinueError( "...Item=" + ConnectorLists( Count ).ConnectorName( Loop ) + ", Type=" + ConnectorLists( Count ).ConnectorType( Loop ) + " was not matched." );
				if ( SameString( ConnectorLists( Count ).ConnectorType( Loop ), "Connector:Splitter" ) ) {
					ShowContinueError( "The BranchList for this Connector:Splitter does not match the BranchList for its corresponding Connector:Mixer." );
				} else {
					ShowContinueError( "The BranchList for this Connector:Mixer does not match the BranchList for its corresponding Connector:Splitter." );
				}
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetConnectorListInput: Program terminates for preceding conditions." );
		}

	}

	void
	GetSplitterInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Sept 2005 (moved from GetLoopSplitter)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the Splitter data that is used in Loops.
		// IDD structure:
		// Connector:Splitter,
		//   \min-fields 3
		//        \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
		//        \memo Split one air/water stream into N outlet streams.  Branch names cannot be duplicated
		//        \memo within a single Splitter list.
		//    A1, \field Name
		//         \required-field
		//    A2, \field Inlet Branch Name
		//         \required-field
		//         \type object-list
		//         \object-list Branches
		//    A3, \field Outlet Branch 1 Name
		//         \required-field
		//         \type object-list
		//         \object-list Branches
		//    A4, \field Outlet Branch 2 Name
		//         \type object-list
		//         \object-list Branches

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

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
		int NumAlphas; // Used to retrieve names from IDF
		Array1D_string Alphas; // Used to retrieve names from IDF
		int NumNumbers; // Used to retrieve numbers from IDF
		Array1D< Real64 > Numbers; // Used to retrieve numbers from IDF
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D_bool lNumericBlanks;
		Array1D_bool lAlphaBlanks;
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams;
		int Loop;
		int Loop1;
		int Count;
		int Found;
		static bool ErrorsFound( false );
		std::string TestName;
		std::string BranchListName;
		std::string FoundSupplyDemandAir;
		std::string SaveSupplyDemandAir;
		std::string FoundLoop;
		std::string SaveLoop;
		bool MatchedLoop;

		if ( ! GetSplitterInputFlag ) return;
		CurrentModuleObject = cSPLITTER;
		NumSplitters = GetNumObjectsFound( CurrentModuleObject );
		Splitters.allocate( NumSplitters );
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Numbers.dimension( NumNumbers, 0.0 );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );
		for ( Count = 1; Count <= NumSplitters; ++Count ) {
			GetObjectItem( CurrentModuleObject, Count, Alphas, NumAlphas, Numbers, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			Splitters( Count ).Name = Alphas( 1 );
			Splitters( Count ).InletBranchName = Alphas( 2 );
			Splitters( Count ).NumOutletBranches = NumAlphas - 2;
			Splitters( Count ).OutletBranchNames.allocate( Splitters( Count ).NumOutletBranches );
			for ( Loop = 1; Loop <= Splitters( Count ).NumOutletBranches; ++Loop ) {
				Splitters( Count ).OutletBranchNames( Loop ) = Alphas( 2 + Loop );
			}
		}
		GetSplitterInputFlag = false;
		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		// More validity -- check splitter "names" against branches.
		if ( ! GetBranchInputFlag ) {
			GetBranchInput();
			GetBranchInputFlag = false;
		}
		for ( Count = 1; Count <= NumSplitters; ++Count ) {
			Found = FindItemInList( Splitters( Count ).InletBranchName, Branch );
			if ( Found == 0 ) {
				ShowSevereError( "GetSplitterInput: Invalid Branch=" + Splitters( Count ).InletBranchName + ", referenced as Inlet Branch to " + CurrentModuleObject + '=' + Splitters( Count ).Name );
				ErrorsFound = true;
			}
			for ( Loop = 1; Loop <= Splitters( Count ).NumOutletBranches; ++Loop ) {
				Found = FindItemInList( Splitters( Count ).OutletBranchNames( Loop ), Branch );
				if ( Found == 0 ) {
					ShowSevereError( "GetSplitterInput: Invalid Branch=" + Splitters( Count ).OutletBranchNames( Loop ) + ", referenced as Outlet Branch # " + TrimSigDigits( Loop ) + " to " + CurrentModuleObject + '=' + Splitters( Count ).Name );
					ErrorsFound = true;
				}
			}
		}

		// Check for duplicate names specified in Splitters
		for ( Count = 1; Count <= NumSplitters; ++Count ) {
			TestName = Splitters( Count ).InletBranchName;
			for ( Loop = 1; Loop <= Splitters( Count ).NumOutletBranches; ++Loop ) {
				if ( TestName != Splitters( Count ).OutletBranchNames( Loop ) ) continue;
				ShowSevereError( CurrentModuleObject + '=' + Splitters( Count ).Name + " specifies an outlet node name the same as the inlet node." );
				ShowContinueError( "..Inlet Node=" + TestName );
				ShowContinueError( "..Outlet Node #" + TrimSigDigits( Loop ) + " is duplicate." );
				ErrorsFound = true;
			}
			for ( Loop = 1; Loop <= Splitters( Count ).NumOutletBranches; ++Loop ) {
				for ( Loop1 = Loop + 1; Loop1 <= Splitters( Count ).NumOutletBranches; ++Loop1 ) {
					if ( Splitters( Count ).OutletBranchNames( Loop ) != Splitters( Count ).OutletBranchNames( Loop1 ) ) continue;
					ShowSevereError( CurrentModuleObject + '=' + Splitters( Count ).Name + " specifies duplicate outlet nodes in its outlet node list." );
					ShowContinueError( "..Outlet Node #" + TrimSigDigits( Loop ) + " Name=" + Splitters( Count ).OutletBranchNames( Loop ) );
					ShowContinueError( "..Outlet Node #" + TrimSigDigits( Loop ) + " is duplicate." );
					ErrorsFound = true;
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetSplitterInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates." );
		}

		//  Everything supposed to be good.  Now make sure all branches in Splitter on same side of loop.
		SaveSupplyDemandAir = BlankString;
		for ( Count = 1; Count <= NumSplitters; ++Count ) {
			// 2.  Find the branch name in branchlist
			TestName = Splitters( Count ).InletBranchName;
			BranchListName = BlankString;
			for ( Loop1 = 1; Loop1 <= NumOfBranchLists; ++Loop1 ) {
				if ( any_eq( BranchList( Loop1 ).BranchNames, TestName ) ) {
					BranchListName = BranchList( Loop1 ).Name;
					break;
				}
			}

			if ( ! BranchListName.empty() ) {
				FoundSupplyDemandAir = BlankString;
				FoundLoop = BlankString;
				MatchedLoop = false;
				// 3.  Find the loop and type
				FindAirPlantCondenserLoopFromBranchList( BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop );
				if ( MatchedLoop ) {
					SaveSupplyDemandAir = FoundSupplyDemandAir;
					SaveLoop = FoundLoop;
				} else {
					ShowSevereError( "GetSplitterInput: Inlet Splitter Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName + "\" not matched to a Air/Plant/Condenser Loop" );
					ShowContinueError( "...and therefore, not a valid Loop Splitter." );
					ShowContinueError( "..." + CurrentModuleObject + '=' + Splitters( Count ).Name );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( "GetSplitterInput: Inlet Splitter Branch=\"" + TestName + "\" not on BranchList" );
				ShowContinueError( "...and therefore, not a valid Loop Splitter." );
				ShowContinueError( "..." + CurrentModuleObject + '=' + Splitters( Count ).Name );
				ErrorsFound = true;
			}
			for ( Loop = 1; Loop <= Splitters( Count ).NumOutletBranches; ++Loop ) {
				TestName = Splitters( Count ).OutletBranchNames( Loop );
				BranchListName = BlankString;
				for ( Loop1 = 1; Loop1 <= NumOfBranchLists; ++Loop1 ) {
					if ( any_eq( BranchList( Loop1 ).BranchNames, TestName ) ) {
						BranchListName = BranchList( Loop1 ).Name;
						break;
					}
				}

				if ( ! BranchListName.empty() ) {
					FoundSupplyDemandAir = BlankString;
					FoundLoop = BlankString;
					MatchedLoop = false;
					// 3.  Find the loop and type
					FindAirPlantCondenserLoopFromBranchList( BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop );
					if ( MatchedLoop ) {
						if ( SaveSupplyDemandAir != FoundSupplyDemandAir || SaveLoop != FoundLoop ) {
							ShowSevereError( "GetSplitterInput: Outlet Splitter Branch=\"" + TestName + "\" does not match types of Inlet Branch." );
							ShowContinueError( "...Inlet Branch is on \"" + SaveLoop + "\" on \"" + SaveSupplyDemandAir + "\" side." );
							ShowContinueError( "...Outlet Branch is on \"" + FoundLoop + "\" on \"" + FoundSupplyDemandAir + "\" side." );
							ShowContinueError( "...All branches in Loop Splitter must be on same kind of loop and supply/demand side." );
							ShowContinueError( "..." + CurrentModuleObject + '=' + Splitters( Count ).Name );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( "GetSplitterInput: Outlet Splitter Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName + "\" not matched to a Air/Plant/Condenser Loop" );
						ShowContinueError( "...and therefore, not a valid Loop Splitter." );
						ShowContinueError( "..." + CurrentModuleObject + '=' + Splitters( Count ).Name );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( "GetSplitterInput: Outlet Splitter Branch=\"" + TestName + "\" not on BranchList" );
					ShowContinueError( "...and therefore, not a valid Loop Splitter" );
					ShowContinueError( "..." + CurrentModuleObject + '=' + Splitters( Count ).Name );
					ErrorsFound = true;
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetSplitterInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates." );
		}

	}

	void
	GetMixerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Sept 2005 (moved from GetLoopMixer)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the Mixer data that is used in Loops.
		// IDD Structure:
		// Connector:Mixer,
		//   \min-fields 3
		//        \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
		//        \memo Mix N inlet air/water streams into one.  Branch names cannot be duplicated within
		//        \memo a single mixer list.
		//    A1 , \field Name
		//         \required-field
		//    A2 , \field Outlet Branch Name
		//         \required-field
		//         \type object-list
		//         \object-list Branches
		//    A3 , \field Inlet Branch 1 Name
		//         \required-field
		//         \type object-list
		//         \object-list Branches
		//    A4 , \field Inlet Branch 2 Name
		//         \type object-list
		//         \object-list Branches

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

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
		int NumAlphas; // Used to retrieve names from IDF
		Array1D_string Alphas; // Used to retrieve names from IDF
		int NumNumbers; // Used to retrieve numbers from IDF
		Array1D< Real64 > Numbers; // Used to retrieve numbers from IDF
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D_bool lNumericBlanks;
		Array1D_bool lAlphaBlanks;
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams;
		int Loop;
		int Loop1;
		int Count;
		int Found;
		static bool ErrorsFound( false );
		std::string TestName;
		std::string BranchListName;
		std::string FoundSupplyDemandAir;
		std::string SaveSupplyDemandAir;
		std::string FoundLoop;
		std::string SaveLoop;
		bool MatchedLoop;

		if ( ! GetMixerInputFlag ) return;

		CurrentModuleObject = cMIXER;

		NumMixers = GetNumObjectsFound( CurrentModuleObject );
		Mixers.allocate( NumMixers );
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Numbers.dimension( NumNumbers, 0.0 );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );
		for ( Count = 1; Count <= NumMixers; ++Count ) {
			GetObjectItem( CurrentModuleObject, Count, Alphas, NumAlphas, Numbers, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			Mixers( Count ).Name = Alphas( 1 );
			Mixers( Count ).OutletBranchName = Alphas( 2 );
			Mixers( Count ).NumInletBranches = NumAlphas - 2;
			Mixers( Count ).InletBranchNames.allocate( Mixers( Count ).NumInletBranches );
			for ( Loop = 1; Loop <= Mixers( Count ).NumInletBranches; ++Loop ) {
				Mixers( Count ).InletBranchNames( Loop ) = Alphas( 2 + Loop );
			}
		}
		GetMixerInputFlag = false;
		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		// More validity -- check mixer "names" against branches.
		if ( ! GetBranchInputFlag ) {
			GetBranchInput();
			GetBranchInputFlag = false;
		}
		for ( Count = 1; Count <= NumMixers; ++Count ) {
			Found = FindItemInList( Mixers( Count ).OutletBranchName, Branch );
			if ( Found == 0 ) {
				ShowSevereError( "GetMixerInput: Invalid Branch=" + Mixers( Count ).OutletBranchName + ", referenced as Outlet Branch in " + CurrentModuleObject + '=' + Mixers( Count ).Name );
				ErrorsFound = true;
			}
			for ( Loop = 1; Loop <= Mixers( Count ).NumInletBranches; ++Loop ) {
				Found = FindItemInList( Mixers( Count ).InletBranchNames( Loop ), Branch );
				if ( Found == 0 ) {
					ShowSevereError( "GetMixerInput: Invalid Branch=" + Mixers( Count ).InletBranchNames( Loop ) + ", referenced as Inlet Branch # " + TrimSigDigits( Loop ) + " in " + CurrentModuleObject + '=' + Mixers( Count ).Name );
					ErrorsFound = true;
				}
			}
		}

		// Check for duplicate names specified in Mixer
		for ( Count = 1; Count <= NumMixers; ++Count ) {
			TestName = Mixers( Count ).OutletBranchName;
			for ( Loop = 1; Loop <= Mixers( Count ).NumInletBranches; ++Loop ) {
				if ( TestName != Mixers( Count ).InletBranchNames( Loop ) ) continue;
				ShowSevereError( CurrentModuleObject + '=' + Mixers( Count ).Name + " specifies an inlet node name the same as the outlet node." );
				ShowContinueError( "..Outlet Node=" + TestName );
				ShowContinueError( "..Inlet Node #" + TrimSigDigits( Loop ) + " is duplicate." );
				ErrorsFound = true;
			}
			for ( Loop = 1; Loop <= Mixers( Count ).NumInletBranches; ++Loop ) {
				for ( Loop1 = Loop + 1; Loop1 <= Mixers( Count ).NumInletBranches; ++Loop1 ) {
					if ( Mixers( Count ).InletBranchNames( Loop ) != Mixers( Count ).InletBranchNames( Loop1 ) ) continue;
					ShowSevereError( CurrentModuleObject + '=' + Mixers( Count ).Name + " specifies duplicate inlet nodes in its inlet node list." );
					ShowContinueError( "..Inlet Node #" + TrimSigDigits( Loop ) + " Name=" + Mixers( Count ).InletBranchNames( Loop ) );
					ShowContinueError( "..Inlet Node #" + TrimSigDigits( Loop ) + " is duplicate." );
					ErrorsFound = true;
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetMixerInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates." );
		}

		//  Everything supposed to be good.  Now make sure all branches in Splitter on same side of loop.
		SaveSupplyDemandAir = BlankString;
		for ( Count = 1; Count <= NumMixers; ++Count ) {
			// 2.  Find the branch name in branchlist
			TestName = Mixers( Count ).OutletBranchName;
			BranchListName = BlankString;
			for ( Loop1 = 1; Loop1 <= NumOfBranchLists; ++Loop1 ) {
				if ( any_eq( BranchList( Loop1 ).BranchNames, TestName ) ) {
					BranchListName = BranchList( Loop1 ).Name;
					break;
				}
			}

			if ( ! BranchListName.empty() ) {
				FoundSupplyDemandAir = BlankString;
				FoundLoop = BlankString;
				MatchedLoop = false;
				// 3.  Find the loop and type
				FindAirPlantCondenserLoopFromBranchList( BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop );
				if ( MatchedLoop ) {
					SaveSupplyDemandAir = FoundSupplyDemandAir;
					SaveLoop = FoundLoop;
				} else {
					ShowSevereError( "GetMixerInput: Outlet Mixer Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName + "\" not matched to a Air/Plant/Condenser Loop" );
					ShowContinueError( "...and therefore, not a valid Loop Mixer." );
					ShowContinueError( "..." + CurrentModuleObject + '=' + Mixers( Count ).Name );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( "GetMixerInput: Outlet Mixer Branch=\"" + TestName + "\" not on BranchList" );
				ShowContinueError( "...and therefore, not a valid Loop Mixer." );
				ShowContinueError( "..." + CurrentModuleObject + '=' + Mixers( Count ).Name );
				ErrorsFound = true;
			}
			for ( Loop = 1; Loop <= Mixers( Count ).NumInletBranches; ++Loop ) {
				TestName = Mixers( Count ).InletBranchNames( Loop );
				BranchListName = BlankString;
				for ( Loop1 = 1; Loop1 <= NumOfBranchLists; ++Loop1 ) {
					if ( any_eq( BranchList( Loop1 ).BranchNames, TestName ) ) {
						BranchListName = BranchList( Loop1 ).Name;
						break;
					}
				}

				if ( ! BranchListName.empty() ) {
					FoundSupplyDemandAir = BlankString;
					FoundLoop = BlankString;
					MatchedLoop = false;
					// 3.  Find the plant loop and type
					FindAirPlantCondenserLoopFromBranchList( BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop );
					if ( MatchedLoop ) {
						if ( SaveSupplyDemandAir != FoundSupplyDemandAir || SaveLoop != FoundLoop ) {
							ShowSevereError( "GetMixerInput: Outlet Mixer Branch=\"" + TestName + "\" does not match types of Inlet Branch." );
							ShowContinueError( "...Outlet Branch is on \"" + SaveLoop + "\" on \"" + SaveSupplyDemandAir + "\" side." );
							ShowContinueError( "...Inlet Branch is on \"" + FoundLoop + "\" on \"" + FoundSupplyDemandAir + "\" side." );
							ShowContinueError( "...All branches in Loop Mixer must be on same kind of loop and supply/demand side." );
							ShowContinueError( "..." + CurrentModuleObject + '=' + Mixers( Count ).Name );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( "GetMixerInput: Inlet Mixer Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName + "\" not matched to a Air/Plant/Condenser Loop" );
						ShowContinueError( "...and therefore, not a valid Loop Mixer." );
						ShowContinueError( "..." + CurrentModuleObject + '=' + Mixers( Count ).Name );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( "GetMixerInput: Inlet Mixer Branch=\"" + TestName + "\" not on BranchList" );
					ShowContinueError( "...and therefore, not a valid Loop Mixer" );
					ShowContinueError( "..." + CurrentModuleObject + '=' + Mixers( Count ).Name );
					ErrorsFound = true;
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetMixerInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates." );
		}

	}

	void
	FindPlantLoopBranchConnection(
		std::string const & BranchListName,
		std::string & FoundPlantLoopName,
		int & FoundPlantLoopNum,
		std::string & FoundSupplyDemand,
		Real64 & FoundVolFlowRate,
		bool & MatchedPlantLoop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// An auxiliary routine locate a plant loop and type from a BranchListName

		// METHODOLOGY EMPLOYED:
		// Calls GetObject for PLANT LOOP

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
		int Num;
		int NumPlantLoops;
		int NumParams;
		Array1D_string Alphas;
		int NumAlphas;
		Array1D< Real64 > Numbers;
		int NumNumbers;
		int IOStat;

		// Get Inputs
		CurrentModuleObject = "PlantLoop";

		NumPlantLoops = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Numbers.allocate( NumNumbers );

		for ( Num = 1; Num <= NumPlantLoops; ++Num ) {
			GetObjectItem( CurrentModuleObject, Num, Alphas, NumAlphas, Numbers, NumNumbers, IOStat );
			// Only looking for BranchList here.
			if ( Alphas( 8 ) == BranchListName ) {
				FoundPlantLoopName = Alphas( 1 );
				FoundSupplyDemand = "Supply";
				FoundVolFlowRate = Numbers( 3 );
				FoundPlantLoopNum = Num;
				MatchedPlantLoop = true;
				break;
			} else if ( Alphas( 12 ) == BranchListName ) {
				FoundPlantLoopName = Alphas( 1 );
				FoundSupplyDemand = "Demand";
				FoundVolFlowRate = Numbers( 3 );
				FoundPlantLoopNum = Num;
				MatchedPlantLoop = true;
				break;
			}
		}

		Alphas.deallocate();
		Numbers.deallocate();

	}

	void
	FindCondenserLoopBranchConnection(
		std::string const & BranchListName,
		std::string & FoundCondLoopName,
		int & FoundCondLoopNum,
		std::string & FoundSupplyDemand,
		Real64 & FoundVolFlowRate,
		bool & MatchedCondLoop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// An auxiliary routine locate a condenser loop and type from a BranchListName

		// METHODOLOGY EMPLOYED:
		// calls GetObject for CONDENSER LOOP

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
		int Num;
		int NumCondLoops;
		int NumParams;
		Array1D_string Alphas;
		int NumAlphas;
		Array1D< Real64 > Numbers;
		int NumNumbers;
		int IOStat;

		// Get Inputs
		CurrentModuleObject = "CondenserLoop";

		NumCondLoops = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Numbers.allocate( NumNumbers );

		for ( Num = 1; Num <= NumCondLoops; ++Num ) {
			GetObjectItem( CurrentModuleObject, Num, Alphas, NumAlphas, Numbers, NumNumbers, IOStat );
			// Only looking for BranchList here.
			if ( Alphas( 8 ) == BranchListName ) {
				FoundCondLoopName = Alphas( 1 );
				FoundSupplyDemand = "Supply";
				FoundVolFlowRate = Numbers( 3 );
				FoundCondLoopNum = Num;
				MatchedCondLoop = true;
				break;
			} else if ( Alphas( 12 ) == BranchListName ) {
				FoundCondLoopName = Alphas( 1 );
				FoundSupplyDemand = "Demand";
				FoundVolFlowRate = Numbers( 3 );
				FoundCondLoopNum = Num;
				MatchedCondLoop = true;
				break;
			}
		}

		Alphas.deallocate();
		Numbers.deallocate();

	}

	void
	FindAirLoopBranchConnection(
		std::string const & BranchListName,
		std::string & FoundAirLoopName,
		int & FoundAirLoopNum,
		std::string & FoundAir,
		Real64 & FoundVolFlowRate,
		bool & MatchedAirLoop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// An auxiliary routine locate a Airenser loop and type from a BranchListName

		// METHODOLOGY EMPLOYED:
		// calls GetObject for PRIMARY AIR LOOP

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
		int Num;
		int NumAirLoops;
		int NumParams;
		Array1D_string Alphas;
		int NumAlphas;
		Array1D< Real64 > Numbers;
		int NumNumbers;
		int IOStat;

		// Get Inputs
		CurrentModuleObject = "AirLoopHVAC";
		NumAirLoops = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Numbers.allocate( NumNumbers );

		for ( Num = 1; Num <= NumAirLoops; ++Num ) {
			GetObjectItem( CurrentModuleObject, Num, Alphas, NumAlphas, Numbers, NumNumbers, IOStat );
			// Only looking for BranchList here.
			if ( Alphas( 4 ) == BranchListName ) {
				FoundAirLoopName = Alphas( 1 );
				FoundAir = "Air";
				FoundVolFlowRate = Numbers( 1 );
				FoundAirLoopNum = Num;
				MatchedAirLoop = true;
				break;
			}
		}

		Alphas.deallocate();
		Numbers.deallocate();

	}

	void
	FindAirPlantCondenserLoopFromBranchList(
		std::string const & BranchListName, // Branch List Name
		std::string & LoopType, // LoopType (if found, Plant,Condenser or Air)
		std::string & LoopSupplyDemandAir, // Supply if "Supply" or Demand if "Demand" or Air if "Air"
		bool & MatchedLoop // true if found
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Assist in validating Loop Splitter/Mixer connections.

		// METHODOLOGY EMPLOYED:
		// Call two previously written subroutines that match a Branch List Name to
		// Plant or Condenser Loop

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
		std::string FoundLoopName;
		int FoundLoopNum;
		Real64 FoundLoopVolFlowRate;

		LoopSupplyDemandAir = BlankString;
		FoundLoopName = BlankString;
		FoundLoopNum = 0;
		FoundLoopVolFlowRate = 0.0;
		MatchedLoop = false;
		LoopType = BlankString;

		// Try Plant first
		FindPlantLoopBranchConnection( BranchListName, FoundLoopName, FoundLoopNum, LoopSupplyDemandAir, FoundLoopVolFlowRate, MatchedLoop );

		if ( MatchedLoop ) LoopType = "Plant";
		if ( ! MatchedLoop ) { // Try Condenser Loop
			LoopSupplyDemandAir = BlankString;
			FoundLoopName = BlankString;
			FoundLoopNum = 0;
			FoundLoopVolFlowRate = 0.0;
			MatchedLoop = false;

			// Try Condenser
			FindCondenserLoopBranchConnection( BranchListName, FoundLoopName, FoundLoopNum, LoopSupplyDemandAir, FoundLoopVolFlowRate, MatchedLoop );
			if ( MatchedLoop ) LoopType = "Condenser";
		}

		if ( ! MatchedLoop ) { // Try Air Loop
			LoopSupplyDemandAir = BlankString;
			FoundLoopName = BlankString;
			FoundLoopNum = 0;
			FoundLoopVolFlowRate = 0.0;
			MatchedLoop = false;

			// Try Air
			FindAirLoopBranchConnection( BranchListName, FoundLoopName, FoundLoopNum, LoopSupplyDemandAir, FoundLoopVolFlowRate, MatchedLoop );
			if ( MatchedLoop ) LoopType = "Air";
		}

	}

	//==================================================================================
	//   Routines that test branch integrity
	//==================================================================================

	void
	AuditBranches(
		bool const mustprint, // true if the warning should be printed.
		Optional_string_const CompType, // when mustprint (ScanPlantLoop)  use CompType in error message and scan
		Optional_string_const CompName // when mustprint (ScanPlantLoop)  use CompName in error message and scan
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will point out any "dangling branches" that are not included on a BranchList.
		// Warnings are produced as the user might clutter up the input file with unused branches.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataErrorTracking::TotalSevereErrors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumDanglingCount; // when mustprint not true, count and report
		int BlNum; // Branch List Counter
		int BrN; // Branch Counter
		int CpN; // Components on Branch
		int Found; // non-zero when found
		std::string FoundBranchName; // Branch matching compname/type
		bool NeverFound;

		NumDanglingCount = 0;
		NeverFound = true;
		for ( BrN = 1; BrN <= NumOfBranches; ++BrN ) {
			Found = 0;
			FoundBranchName = "";
			if ( present( CompType ) && present( CompName ) ) {
				for ( CpN = 1; CpN <= Branch( BrN ).NumOfComponents; ++CpN ) {
					if ( ! SameString( CompType, Branch( BrN ).Component( CpN ).CType ) || ! SameString( CompName, Branch( BrN ).Component( CpN ).Name ) ) continue;
					FoundBranchName = Branch( BrN ).Name;
					NeverFound = false;
				}
			}
			for ( BlNum = 1; BlNum <= NumOfBranchLists; ++BlNum ) {
				Found = FindItemInList( Branch( BrN ).Name, BranchList( BlNum ).BranchNames, BranchList( BlNum ).NumOfBranchNames );
				if ( Found != 0 ) break;
			}
			if ( Found != 0 ) continue;
			++NumDanglingCount;
			if ( DisplayExtraWarnings || mustprint ) {
				if ( mustprint ) {
					ShowContinueError( "AuditBranches: Branch=\"" + Branch( BrN ).Name + "\" not found on any BranchLists." );
					if ( FoundBranchName != "" ) {
						ShowContinueError( "Branch contains component, type=\"" + CompType + "\", name=\"" + CompName + "\"" );
					}
				} else {
					ShowSevereMessage( "AuditBranches: Branch=\"" + Branch( BrN ).Name + "\" not found on any BranchLists." );
					++TotalSevereErrors;
				}
			}
		}
		if ( mustprint && NeverFound ) { // this may be caught during branch input, not sure
			ShowContinueError( "Component, type=\"" + CompType + "\", name=\"" + CompName + "\" was not found on any Branch." );
			ShowContinueError( "Look for mistyped branch or component names/types." );
		}
		if ( ! mustprint && NumDanglingCount > 0 ) {
			ShowSevereMessage( "AuditBranches: There are " + RoundSigDigits( NumDanglingCount ) + " branch(es) that do not appear on any BranchList." );
			TotalSevereErrors += NumDanglingCount;
			ShowContinueError( "Use Output:Diagnostics,DisplayExtraWarnings; for detail of each branch not on a branch list." );
		}

	}

	void
	TestBranchIntegrity( bool & ErrFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine tests branch integrity and displays the loop for each branch.
		// Also, input and output nodes.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int Count;
		int MatchNode; // Node Number for match
		std::string MatchNodeName; // Name for error message if not matched
		std::string BranchInletNodeName; // Branch Inlet Node Name
		std::string BranchOutletNodeName; // Branch Outlet Node Name
		std::string BranchLoopName; // Loop Name which Branch is part of
		std::string BranchLoopType; // Loop Type which Branch is part of
		int NumErr; // Error Counter
		Array1D_bool BranchReported;
		int BCount;
		int Found;
		std::string ChrOut;
		std::string ChrOut1;
		//  LOGICAL UniqueNodeError
		int NodeNum;
		int Loop2;
		bool IsAirBranch;
		int BranchFluidType;
		bool MixedFluidTypesOnBranchList;
		int InitialBranchFluidNode;
		Array1D_int BranchFluidNodes;
		Array1D_int FoundBranches;
		Array1D_int BranchPtrs;
		int NumNodesOnBranchList;
		int NumFluidNodes;
		std::string OriginalBranchFluidType;
		std::string cBranchFluidType;
		int Ptr;
		int EndPtr;

		struct BranchUniqueNodes
		{
			// Members
			int NumNodes;
			Array1D_string UniqueNodeNames;

			// Default Constructor
			BranchUniqueNodes() :
				NumNodes( 0 )
			{}

		};

		// Object Data
		Array1D< BranchUniqueNodes > BranchNodes;

		// Formats
		static gio::Fmt Format_700( "('! <#Branch Lists>,<Number of Branch Lists>')" );
		static gio::Fmt Format_701( "(A)" );
		static gio::Fmt Format_702( "('! <Branch List>,<Branch List Count>,<Branch List Name>,<Loop Name>,<Loop Type>,<Number of Branches>')" );
		static gio::Fmt Format_704( "('! <Branch>,<Branch Count>,<Branch Name>,<Loop Name>,<Loop Type>,<Branch Inlet Node Name>,<Branch Outlet Node Name>')" );
		static gio::Fmt Format_706( "('! <# Orphaned Branches>,<Number of Branches not on Branch Lists>')" );

		BranchReported.dimension( NumOfBranches, false );

		// Do by Branch Lists
		ShowMessage( "Testing Individual Branch Integrity" );
		ErrFound = false;

		BranchNodes.allocate( NumOfBranches );

		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_700 );
		gio::write( ChrOut, fmtLD ) << NumOfBranchLists;
		gio::write( OutputFileBNDetails, Format_701 ) << " #Branch Lists," + stripped( ChrOut );
		gio::write( OutputFileBNDetails, Format_702 );
		gio::write( OutputFileBNDetails, Format_704 );

		for ( BCount = 1; BCount <= NumOfBranchLists; ++BCount ) {

			gio::write( ChrOut, fmtLD ) << BCount;
			gio::write( ChrOut1, fmtLD ) << BranchList( BCount ).NumOfBranchNames;
			gio::write( OutputFileBNDetails, Format_701 ) << " Branch List," + stripped( ChrOut ) + ',' + BranchList( BCount ).Name + ',' + BranchList( BCount ).LoopName + ',' + BranchList( BCount ).LoopType + ',' + stripped( ChrOut1 );

			IsAirBranch = false;
			BranchFluidType = NodeType_Unknown;
			MixedFluidTypesOnBranchList = false;
			NumNodesOnBranchList = 0;
			FoundBranches.allocate( BranchList( BCount ).NumOfBranchNames );
			FoundBranches = 0;
			BranchPtrs.allocate( BranchList( BCount ).NumOfBranchNames + 2 );
			BranchPtrs = 0;
			for ( Count = 1; Count <= BranchList( BCount ).NumOfBranchNames; ++Count ) {
				Found = FindItemInList( BranchList( BCount ).BranchNames( Count ), Branch );
				if ( Found > 0 ) {
					NumNodesOnBranchList += Branch( Found ).NumOfComponents * 2;
					FoundBranches( Count ) = Found;
					BranchPtrs( Count ) = NumNodesOnBranchList;
				} else {
					ShowSevereError( "Branch not found=" + BranchList( BCount ).BranchNames( Count ) );
					ErrFound = true;
				}
			}
			BranchPtrs( BranchList( BCount ).NumOfBranchNames + 1 ) = BranchPtrs( BranchList( BCount ).NumOfBranchNames ) + 1;
			BranchFluidNodes.dimension( NumNodesOnBranchList, 0 );
			OriginalBranchFluidType = BlankString;
			NumFluidNodes = 0;
			for ( Count = 1; Count <= BranchList( BCount ).NumOfBranchNames; ++Count ) {

				ChrOut = RoundSigDigits( Count );
				//      WRITE(ChrOut,*) Count
				//      ChrOut=ADJUSTL(ChrOut)

				Found = FoundBranches( Count );
				if ( Found == 0 ) {
					gio::write( OutputFileBNDetails, Format_701 ) << "   Branch," + ChrOut + ',' + BranchList( BCount ).BranchNames( Count ) + "(not found),**Unknown**,**Unknown**,**Unknown**,**Unknown**";
					continue;
				}
				BranchReported( Found ) = true;
				// Check Branch for connections

				MatchNode = 0;
				InitialBranchFluidNode = 0;
				if ( Branch( Found ).NumOfComponents > 0 ) {
					MatchNode = Branch( Found ).Component( 1 ).InletNode;
					MatchNodeName = Branch( Found ).Component( 1 ).InletNodeName;
					BranchInletNodeName = Branch( Found ).Component( 1 ).InletNodeName;
				} else {
					ShowWarningError( "Branch has no components=" + Branch( Found ).Name );
				}
				NumErr = 0;
				for ( Loop = 1; Loop <= Branch( Found ).NumOfComponents; ++Loop ) {
					if ( Node( Branch( Found ).Component( Loop ).InletNode ).FluidType == NodeType_Air ) IsAirBranch = true;
					if ( BranchFluidType == NodeType_Unknown ) {
						++NumFluidNodes;
						BranchFluidNodes( NumFluidNodes ) = Branch( Found ).Component( Loop ).InletNode;
						BranchFluidType = Node( Branch( Found ).Component( Loop ).InletNode ).FluidType;
						InitialBranchFluidNode = Branch( Found ).Component( Loop ).InletNode;
						OriginalBranchFluidType = ValidNodeFluidTypes( BranchFluidType );
					} else if ( BranchFluidType != Node( Branch( Found ).Component( Loop ).InletNode ).FluidType && Node( Branch( Found ).Component( Loop ).InletNode ).FluidType != NodeType_Unknown ) {
						++NumFluidNodes;
						BranchFluidNodes( NumFluidNodes ) = Branch( Found ).Component( Loop ).InletNode;
						MixedFluidTypesOnBranchList = true;
					} else {
						++NumFluidNodes;
						BranchFluidNodes( NumFluidNodes ) = Branch( Found ).Component( Loop ).InletNode;
					}
					if ( Node( Branch( Found ).Component( Loop ).OutletNode ).FluidType == NodeType_Air ) IsAirBranch = true;
					if ( BranchFluidType == NodeType_Unknown ) {
						++NumFluidNodes;
						BranchFluidNodes( NumFluidNodes ) = Branch( Found ).Component( Loop ).InletNode;
						BranchFluidType = Node( Branch( Found ).Component( Loop ).OutletNode ).FluidType;
						InitialBranchFluidNode = Branch( Found ).Component( Loop ).OutletNode;
						OriginalBranchFluidType = ValidNodeFluidTypes( BranchFluidType );
					} else if ( BranchFluidType != Node( Branch( Found ).Component( Loop ).OutletNode ).FluidType && Node( Branch( Found ).Component( Loop ).OutletNode ).FluidType != NodeType_Unknown ) {
						++NumFluidNodes;
						BranchFluidNodes( NumFluidNodes ) = Branch( Found ).Component( Loop ).OutletNode;
						MixedFluidTypesOnBranchList = true;
					} else {
						++NumFluidNodes;
						BranchFluidNodes( NumFluidNodes ) = Branch( Found ).Component( Loop ).OutletNode;
					}
					if ( Branch( Found ).Component( Loop ).InletNode != MatchNode ) {
						ShowSevereError( "Error Detected in BranchList=" + BranchList( BCount ).Name );
						ShowContinueError( "Actual Error occurs in Branch=" + Branch( Found ).Name );
						ShowContinueError( "Branch Outlet does not match Inlet, Outlet=" + MatchNodeName );
						ShowContinueError( "Inlet Name=" + Branch( Found ).Component( Loop ).InletNodeName );
						ErrFound = true;
						++NumErr;
					} else {
						MatchNode = Branch( Found ).Component( Loop ).OutletNode;
						MatchNodeName = Branch( Found ).Component( Loop ).OutletNodeName;
					}
				}
				Branch( Found ).FluidType = BranchFluidType;
				if ( IsAirBranch && Branch( Found ).MaxFlowRate == 0.0 ) {
					ShowSevereError( "Branch=" + Branch( Found ).Name + " is an air branch with zero max flow rate." );
					ErrFound = true;
				}
				BranchOutletNodeName = MatchNodeName;
				if ( Branch( Found ).AssignedLoopName == BlankString ) {
					BranchLoopName = "**Unknown**";
					BranchLoopType = "**Unknown**";
				} else if ( Branch( Found ).AssignedLoopName == BranchList( BCount ).LoopName ) {
					BranchLoopName = BranchList( BCount ).LoopName;
					BranchLoopType = BranchList( BCount ).LoopType;
				} else {
					BranchLoopName = Branch( Found ).AssignedLoopName;
					BranchLoopType = "**Unknown**";
				}
				gio::write( OutputFileBNDetails, Format_701 ) << "   Branch," + ChrOut + ',' + Branch( Found ).Name + ',' + BranchLoopName + ',' + BranchLoopType + ',' + BranchInletNodeName + ',' + BranchOutletNodeName;
			}
			if ( MixedFluidTypesOnBranchList ) {
				ShowSevereError( "BranchList=" + BranchList( BCount ).Name + " has mixed fluid types in its nodes." );
				ErrFound = true;
				if ( OriginalBranchFluidType == BlankString ) OriginalBranchFluidType = "**Unknown**";
				ShowContinueError( "Initial Node=" + NodeID( InitialBranchFluidNode ) + ", Fluid Type=" + OriginalBranchFluidType );
				ShowContinueError( "BranchList Topology - Note nodes which do not match that fluid type:" );
				Ptr = 1;
				EndPtr = BranchPtrs( 1 );
				for ( Loop = 1; Loop <= BranchList( BCount ).NumOfBranchNames; ++Loop ) {
					if ( FoundBranches( Loop ) != 0 ) {
						ShowContinueError( "..Branch=" + Branch( FoundBranches( Loop ) ).Name );
					} else {
						ShowContinueError( "..Illegal Branch=" + BranchList( BCount ).BranchNames( Loop ) );
						continue;
					}
					for ( Loop2 = Ptr; Loop2 <= EndPtr; ++Loop2 ) {
						cBranchFluidType = ValidNodeFluidTypes( Node( BranchFluidNodes( Loop2 ) ).FluidType );
						if ( cBranchFluidType == BlankString ) cBranchFluidType = "**Unknown**";
						ShowContinueError( "....Node=" + NodeID( BranchFluidNodes( Loop2 ) ) + ", Fluid Type=" + cBranchFluidType );
					}
					Ptr = EndPtr + 1;
					EndPtr = BranchPtrs( Loop + 1 );
				}
			}
			BranchFluidNodes.deallocate();
			BranchPtrs.deallocate();
			FoundBranches.deallocate();
		}

		// Build node names in branches
		for ( Count = 1; Count <= NumOfBranches; ++Count ) {
			BranchNodes( Count ).UniqueNodeNames.allocate( Branch( Count ).NumOfComponents * 2 );
			BranchNodes( Count ).UniqueNodeNames = BlankString;
			NodeNum = 0;
			for ( Loop = 1; Loop <= Branch( Count ).NumOfComponents; ++Loop ) {
				Found = FindItemInList( Branch( Count ).Component( Loop ).InletNodeName, BranchNodes( Count ).UniqueNodeNames, NodeNum );
				if ( Found == 0 ) {
					++NodeNum;
					BranchNodes( Count ).UniqueNodeNames( NodeNum ) = Branch( Count ).Component( Loop ).InletNodeName;
				}
				Found = FindItemInList( Branch( Count ).Component( Loop ).OutletNodeName, BranchNodes( Count ).UniqueNodeNames, NodeNum );
				if ( Found == 0 ) {
					++NodeNum;
					BranchNodes( Count ).UniqueNodeNames( NodeNum ) = Branch( Count ).Component( Loop ).OutletNodeName;
				}
			}
			BranchNodes( Count ).NumNodes = NodeNum;
		}
		// Check Uniqueness branch to branch
		for ( Count = 1; Count <= NumOfBranches; ++Count ) {
			for ( Loop = Count + 1; Loop <= NumOfBranches; ++Loop ) {
				for ( Loop2 = 1; Loop2 <= BranchNodes( Count ).NumNodes; ++Loop2 ) {
					Found = FindItemInList( BranchNodes( Count ).UniqueNodeNames( Loop2 ), BranchNodes( Loop ).UniqueNodeNames, BranchNodes( Loop ).NumNodes );
					if ( Found != 0 ) {
						ShowSevereError( "Non-unique node name found, name=" + BranchNodes( Count ).UniqueNodeNames( Loop2 ) );
						ShowContinueError( "..1st occurence in Branch=" + Branch( Count ).Name );
						ShowContinueError( "..duplicate occurrence in Branch=" + Branch( Loop ).Name );
						ErrFound = true;
					}
				}
			}
		}
		for ( Count = 1; Count <= NumOfBranches; ++Count ) {
			BranchNodes( Count ).UniqueNodeNames.deallocate();
		}
		BranchNodes.deallocate();

		BCount = 0;
		for ( Count = 1; Count <= NumOfBranches; ++Count ) {
			if ( BranchReported( Count ) ) continue;
			++BCount;
		}
		if ( BCount > 0 ) {
			gio::write( OutputFileBNDetails, Format_706 );
			ChrOut = RoundSigDigits( BCount );
			//    WRITE(ChrOut,*) BCount
			gio::write( OutputFileBNDetails, Format_701 ) << " #Orphaned Branches," + ChrOut;
			ShowWarningError( "There are orphaned Branches in input. See .bnd file for details." );

			BCount = 0;

			for ( Count = 1; Count <= NumOfBranches; ++Count ) {
				if ( BranchReported( Count ) ) continue;
				++BCount;
				ShowWarningError( "Orphan Branch=\"" + Branch( Count ).Name + "\"." );

				//        WRITE(ChrOut,*) BCount
				//        ChrOut=ADJUSTL(ChrOut)
				ChrOut = RoundSigDigits( BCount );
				if ( Branch( Count ).NumOfComponents > 0 ) {
					MatchNode = Branch( Count ).Component( 1 ).InletNode;
					MatchNodeName = Branch( Count ).Component( 1 ).InletNodeName;
					BranchInletNodeName = Branch( Count ).Component( 1 ).InletNodeName;
				} else {
					ShowWarningError( "Branch has no components=" + Branch( Count ).Name );
				}
				NumErr = 0;
				for ( Loop = 1; Loop <= Branch( Count ).NumOfComponents; ++Loop ) {
					if ( Branch( Count ).Component( Loop ).InletNode != MatchNode ) {
						ShowSevereError( "Error Detected in Branch=" + Branch( Count ).Name );
						ShowContinueError( "Branch Outlet does not match Inlet, Outlet=" + MatchNodeName );
						ShowContinueError( "Inlet Name=" + Branch( Count ).Component( Loop ).InletNodeName );
						ErrFound = true;
						++NumErr;
					} else {
						MatchNode = Branch( Count ).Component( Loop ).OutletNode;
						MatchNodeName = Branch( Count ).Component( Loop ).OutletNodeName;
					}
				}
				BranchOutletNodeName = MatchNodeName;
				if ( Branch( Count ).AssignedLoopName == BlankString ) {
					BranchLoopName = "**Unknown**";
					BranchLoopType = "**Unknown**";
				} else {
					BranchLoopName = Branch( Count ).AssignedLoopName;
					BranchLoopType = "**Unknown**";
				}
				gio::write( OutputFileBNDetails, Format_701 ) << " Branch," + ChrOut + ',' + Branch( Count ).Name + ',' + BranchLoopName + ',' + BranchLoopType + ',' + BranchInletNodeName + ',' + BranchOutletNodeName;
			}
		}

		if ( ErrFound ) {
			ShowSevereError( "Branch(es) did not pass integrity testing" );
		} else {
			ShowMessage( "All Branches passed integrity testing" );
		}

	}

} // BranchInputManager

} // EnergyPlus
