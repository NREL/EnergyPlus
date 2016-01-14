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
#include <HVACDuct.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HVACDuct {

	// Module containing the routines dealing with the Duct component
	// in forced air air conditioning systems

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   17May2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and routines required to model duct
	// components in the EnergyPlus HVAC simulation

	// METHODOLOGY EMPLOYED:
	// At this point ducts are passive elements in the loop that just pass inlet node
	// conditions to the outlet node. The function of a duct component is to allow the
	// definition of a bypass branch: a branch must contain at least 1 component.

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using namespace DataHVACGlobals;
	using namespace DataLoopNode;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumDucts( 0 );
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACDuct:

	// <name Public routines, optionally name Private routines within this module>

	// Object Data
	Array1D< DuctData > Duct;

	// Functions

	void
	SimDuct(
		std::string const & CompName, // name of the duct component
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep !unused1208
		int & CompIndex // index of duct component
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   17May2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manage the simulation of a duct component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int DuctNum; // index of duct being simulated

		if ( GetInputFlag ) {
			GetDuctInput();
			GetInputFlag = false;
		}

		// Get the duct component index
		if ( CompIndex == 0 ) {
			DuctNum = FindItemInList( CompName, Duct );
			if ( DuctNum == 0 ) {
				ShowFatalError( "SimDuct: Component not found=" + CompName );
			}
			CompIndex = DuctNum;
		} else {
			DuctNum = CompIndex;
			if ( DuctNum > NumDucts || DuctNum < 1 ) {
				ShowFatalError( "SimDuct:  Invalid CompIndex passed=" + TrimSigDigits( DuctNum ) + ", Number of Components=" + TrimSigDigits( NumDucts ) + ", Entered Component name=" + CompName );
			}
			if ( CheckEquipName( DuctNum ) ) {
				if ( CompName != Duct( DuctNum ).Name ) {
					ShowFatalError( "SimDuct: Invalid CompIndex passed=" + TrimSigDigits( DuctNum ) + ", Component name=" + CompName + ", stored Component Name for that index=" + Duct( DuctNum ).Name );
				}
				CheckEquipName( DuctNum ) = false;
			}
		}

		InitDuct( DuctNum );

		CalcDuct( DuctNum );

		UpdateDuct( DuctNum );

		ReportDuct( DuctNum );

	}

	void
	GetDuctInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   17May2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for ducts and stores it in duct data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using namespace DataIPShortCuts;

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
		int DuctNum; // duct index
		static std::string const RoutineName( "GetDuctInput:" );
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		cCurrentModuleObject = "Duct";
		NumDucts = GetNumObjectsFound( cCurrentModuleObject );
		Duct.allocate( NumDucts );
		CheckEquipName.dimension( NumDucts, true );

		for ( DuctNum = 1; DuctNum <= NumDucts; ++DuctNum ) {
			GetObjectItem( cCurrentModuleObject, DuctNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Duct, DuctNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Duct( DuctNum ).Name = cAlphaArgs( 1 );
			Duct( DuctNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Duct( DuctNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Air Nodes" );
		}

		// No output variables

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + " Errors found in input" );
		}

	}

	void
	InitDuct( int const DuctNum ) // number of the current duct being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   17May2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Duct Components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations

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
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;

		// do one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.dimension( NumDucts, true );

			MyOneTimeFlag = false;

		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( DuctNum ) ) {

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( DuctNum ) = true;
		}

		// do these initializations every HVAC time step

	}

	void
	CalcDuct( int const EP_UNUSED( DuctNum ) ) // number of the current duct being simulated !unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   17May2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// na

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
		// na

	}

	void
	UpdateDuct( int const DuctNum ) // number of the current duct being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   17May2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Moves duct output to the outlet nodes

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // inlet node number
		int OutNode; // outlet node number

		InNode = Duct( DuctNum ).InletNodeNum;
		OutNode = Duct( DuctNum ).OutletNodeNum;
		// Set the outlet air node conditions of the duct
		Node( OutNode ).MassFlowRate = Node( InNode ).MassFlowRate;
		Node( OutNode ).Temp = Node( InNode ).Temp;
		Node( OutNode ).HumRat = Node( InNode ).HumRat;
		Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;
		Node( OutNode ).Quality = Node( InNode ).Quality;
		Node( OutNode ).Press = Node( InNode ).Press;
		Node( OutNode ).MassFlowRateMin = Node( InNode ).MassFlowRateMin;
		Node( OutNode ).MassFlowRateMax = Node( InNode ).MassFlowRateMax;
		Node( OutNode ).MassFlowRateMinAvail = Node( InNode ).MassFlowRateMinAvail;
		Node( OutNode ).MassFlowRateMaxAvail = Node( InNode ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( OutNode ).CO2 = Node( InNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( OutNode ).GenContam = Node( InNode ).GenContam;
		}

	}

	void
	ReportDuct( int const EP_UNUSED( DuctNum ) ) // number of the current duct being simulated !unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   17May2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fill remaining report variables

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
		// na

	}

} // HVACDuct

} // EnergyPlus
