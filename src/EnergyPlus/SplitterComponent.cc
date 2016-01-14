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
#include <SplitterComponent.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SplitterComponent {
	// Module containing the Splitter simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   March 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage Air Path Splitter Components

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataLoopNode;

	// Data
	// MODULE PARAMETERS:

	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	bool GetSplitterInputFlag( true );
	// Public because Used by SimAirServingZones and the Direct Air Unit
	int NumSplitters( 0 ); // The Number of Splitters found in the Input
	Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Object Data
	Array1D< SplitterConditions > SplitterCond;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		GetSplitterInputFlag = true ;
		NumSplitters = 0;
		CheckEquipName.deallocate();
		SplitterCond.deallocate();
	}

	void
	SimAirLoopSplitter(
		std::string const & CompName,
		bool const FirstHVACIteration,
		bool const FirstCall,
		bool & SplitterInletChanged,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Splitter component simulation.
		// It is called from the SimAirLoopComponent
		// at the system time step.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SplitterNum; // The Splitter that you are currently loading input for

		// FLOW:

		// Obtains and Allocates Splitter related parameters from input file
		if ( GetSplitterInputFlag ) { //First time subroutine has been entered
			GetSplitterInput();
		}

		// Find the correct SplitterNumber
		if ( CompIndex == 0 ) {
			SplitterNum = FindItemInList( CompName, SplitterCond, &SplitterConditions::SplitterName );
			if ( SplitterNum == 0 ) {
				ShowFatalError( "SimAirLoopSplitter: Splitter not found=" + CompName );
			}
			CompIndex = SplitterNum;
		} else {
			SplitterNum = CompIndex;
			if ( SplitterNum > NumSplitters || SplitterNum < 1 ) {
				ShowFatalError( "SimAirLoopSplitter: Invalid CompIndex passed=" + TrimSigDigits( SplitterNum ) + ", Number of Splitters=" + TrimSigDigits( NumSplitters ) + ", Splitter name=" + CompName );
			}
			if ( CheckEquipName( SplitterNum ) ) {
				if ( CompName != SplitterCond( SplitterNum ).SplitterName ) {
					ShowFatalError( "SimAirLoopSplitter: Invalid CompIndex passed=" + TrimSigDigits( SplitterNum ) + ", Splitter name=" + CompName + ", stored Splitter Name for that index=" + SplitterCond( SplitterNum ).SplitterName );
				}
				CheckEquipName( SplitterNum ) = false;
			}
		}

		InitAirLoopSplitter( SplitterNum, FirstHVACIteration, FirstCall ); // Initialize all Splitter related parameters

		CalcAirLoopSplitter( SplitterNum, FirstCall );

		// Update the current Splitter to the outlet nodes
		UpdateSplitter( SplitterNum, SplitterInletChanged, FirstCall );

		// Report the current Splitter
		ReportSplitter( SplitterNum );

	}

	//*******************************

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetSplitterInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main routine to call other input routines and
		// Get routines.  The Splitter only gets node connection data and not mass
		// flow rates.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSplitterInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SplitterNum; // The Splitter that you are currently loading input into
		int NumAlphas;
		int NumNums;
		int NodeNum;
		int IOStat;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumParams;
		int OutNodeNum1;
		int OutNodeNum2;
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// RESET THE GETINPUT FLAG
		GetSplitterInputFlag = false;

		// Flow
		CurrentModuleObject = "AirLoopHVAC:ZoneSplitter";
		NumSplitters = GetNumObjectsFound( CurrentModuleObject );

		if ( NumSplitters > 0 ) SplitterCond.allocate( NumSplitters );
		CheckEquipName.dimension( NumSplitters, true );

		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
		AlphArray.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		lAlphaBlanks.dimension( NumAlphas, true );
		cNumericFields.allocate( NumNums );
		lNumericBlanks.dimension( NumNums, true );
		NumArray.dimension( NumNums, 0.0 );

		for ( SplitterNum = 1; SplitterNum <= NumSplitters; ++SplitterNum ) {
			GetObjectItem( CurrentModuleObject, SplitterNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SplitterCond, &SplitterConditions::SplitterName, SplitterNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SplitterCond( SplitterNum ).SplitterName = AlphArray( 1 );
			SplitterCond( SplitterNum ).InletNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SplitterCond( SplitterNum ).NumOutletNodes = NumAlphas - 2;

			SplitterCond( SplitterNum ).OutletNode.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletMassFlowRate.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletMassFlowRateMaxAvail.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletMassFlowRateMinAvail.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletTemp.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletHumRat.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletEnthalpy.allocate( SplitterCond( SplitterNum ).NumOutletNodes );
			SplitterCond( SplitterNum ).OutletPressure.allocate( SplitterCond( SplitterNum ).NumOutletNodes );

			SplitterCond( SplitterNum ).InletMassFlowRate = 0.0;
			SplitterCond( SplitterNum ).InletMassFlowRateMaxAvail = 0.0;
			SplitterCond( SplitterNum ).InletMassFlowRateMinAvail = 0.0;

			for ( NodeNum = 1; NodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++NodeNum ) {

				SplitterCond( SplitterNum ).OutletNode( NodeNum ) = GetOnlySingleNode( AlphArray( 2 + NodeNum ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				if ( lAlphaBlanks( 2 + NodeNum ) ) {
					ShowSevereError( cAlphaFields( 2 + NodeNum ) + " is Blank, " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}

		} // end Number of Splitter Loop

		// Check for duplicate names specified in Zone Splitter
		for ( SplitterNum = 1; SplitterNum <= NumSplitters; ++SplitterNum ) {
			NodeNum = SplitterCond( SplitterNum ).InletNode;
			for ( OutNodeNum1 = 1; OutNodeNum1 <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutNodeNum1 ) {
				if ( NodeNum != SplitterCond( SplitterNum ).OutletNode( OutNodeNum1 ) ) continue;
				ShowSevereError( CurrentModuleObject + " = " + SplitterCond( SplitterNum ).SplitterName + " specifies an outlet node name the same as the inlet node." );
				ShowContinueError( ".." + cAlphaFields( 2 ) + '=' + NodeID( NodeNum ) );
				ShowContinueError( "..Outlet Node #" + TrimSigDigits( OutNodeNum1 ) + " is duplicate." );
				ErrorsFound = true;
			}
			for ( OutNodeNum1 = 1; OutNodeNum1 <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutNodeNum1 ) {
				for ( OutNodeNum2 = OutNodeNum1 + 1; OutNodeNum2 <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutNodeNum2 ) {
					if ( SplitterCond( SplitterNum ).OutletNode( OutNodeNum1 ) != SplitterCond( SplitterNum ).OutletNode( OutNodeNum2 ) ) continue;
					ShowSevereError( CurrentModuleObject + " = " + SplitterCond( SplitterNum ).SplitterName + " specifies duplicate outlet nodes in its outlet node list." );
					ShowContinueError( "..Outlet Node #" + TrimSigDigits( OutNodeNum1 ) + " Name=" + NodeID( OutNodeNum1 ) );
					ShowContinueError( "..Outlet Node #" + TrimSigDigits( OutNodeNum2 ) + " is duplicate." );
					ErrorsFound = true;
				}
			}
		}

		AlphArray.deallocate();
		NumArray.deallocate();
		cAlphaFields.deallocate();
		lAlphaBlanks.deallocate();
		cNumericFields.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input." );
		}

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirLoopSplitter(
		int const SplitterNum,
		bool const FirstHVACIteration,
		bool const FirstCall
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initialisations of the Splitter Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using Psychrometrics::PsyHFnTdbW;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::OutdoorCO2;
		using DataContaminantBalance::OutdoorGC;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int NodeNum;
		Real64 AirEnthalpy; // [J/kg]
		static bool MyEnvrnFlag( true );

		// FLOW:

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag ) {

			// Calculate the air density and enthalpy for standard conditions...
			AirEnthalpy = PsyHFnTdbW( 20.0, OutHumRat );

			// Initialize the inlet node to s standard set of conditions so that the
			//  flows match around the loop & do not cause convergence problems.
			InletNode = SplitterCond( SplitterNum ).InletNode;
			Node( InletNode ).Temp = 20.0;
			Node( InletNode ).HumRat = OutHumRat;
			Node( InletNode ).Enthalpy = AirEnthalpy;
			Node( InletNode ).Press = OutBaroPress;
			if ( Contaminant.CO2Simulation ) {
				Node( InletNode ).CO2 = OutdoorCO2;
			}
			if ( Contaminant.GenericContamSimulation ) {
				Node( InletNode ).GenContam = OutdoorGC;
			}

			MyEnvrnFlag = false;

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// Set the inlet node for the Splitter
		InletNode = SplitterCond( SplitterNum ).InletNode;

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.
		// Load the node data in this section for the component simulation

		// This section is very important to understand.  The system off condition is important
		// transfer around the loop even if the splitter does not have enough information to
		// calculate the correct flow rates since the dampers are downstream and there is no pressure
		// simulation.  What happens in this section is the flow from upstream is not zero is
		// arbitrarily split by the number of inlet nodes.  This is by no way meant to determine the
		// correct split flow!  Just to give each outlet a non-zero flow so that the Air Distribution
		// Unit(ADU) downstream knows that the system is operating or has flow.  This is only done the first
		// iteration through and the splitter first pass.  After the first iteration the ADU sets the
		// correct flow and that is used and passed back upstream.
		if ( FirstHVACIteration && FirstCall ) {
			if ( Node( InletNode ).MassFlowRate > 0.0 ) {
				for ( NodeNum = 1; NodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++NodeNum ) {
					OutletNode = SplitterCond( SplitterNum ).OutletNode( NodeNum );
					Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate / SplitterCond( SplitterNum ).NumOutletNodes;
				}
			}
			if ( Node( InletNode ).MassFlowRateMaxAvail > 0.0 ) {
				for ( NodeNum = 1; NodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++NodeNum ) {
					OutletNode = SplitterCond( SplitterNum ).OutletNode( NodeNum );
					Node( OutletNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRateMaxAvail / SplitterCond( SplitterNum ).NumOutletNodes;
				}
			}

		} //For FirstHVACIteration and FirstCall

		if ( FirstCall ) {
			//There is one exception to the rule stated above and that is if the system shuts OFF
			// for some operational or algorithm dependency.  This IF block should catch that condition
			// and then pass the NO flow condition downstream to the waiting ADU's.  Most of the time
			// this IF is jumped over.
			if ( Node( InletNode ).MassFlowRateMaxAvail == 0.0 ) {

				for ( NodeNum = 1; NodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++NodeNum ) {

					OutletNode = SplitterCond( SplitterNum ).OutletNode( NodeNum );
					Node( OutletNode ).MassFlowRate = 0.0;
					Node( OutletNode ).MassFlowRateMaxAvail = 0.0;
					Node( OutletNode ).MassFlowRateMinAvail = 0.0;

				}
			} //For Node inlet Max Avail = 0.0

			//Pass the State Properties through every time.  This is what mainly happens each time
			// through the splitter,
			InletNode = SplitterCond( SplitterNum ).InletNode;
			SplitterCond( SplitterNum ).InletTemp = Node( InletNode ).Temp;
			SplitterCond( SplitterNum ).InletHumRat = Node( InletNode ).HumRat;
			SplitterCond( SplitterNum ).InletEnthalpy = Node( InletNode ).Enthalpy;
			SplitterCond( SplitterNum ).InletPressure = Node( InletNode ).Press;

		} else { //On the second call from the ZoneEquipManager this is where the flows are passed back to
			// the splitter inlet.
			for ( NodeNum = 1; NodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++NodeNum ) {

				OutletNode = SplitterCond( SplitterNum ).OutletNode( NodeNum );
				SplitterCond( SplitterNum ).OutletMassFlowRate( NodeNum ) = Node( OutletNode ).MassFlowRate;
				SplitterCond( SplitterNum ).OutletMassFlowRateMaxAvail( NodeNum ) = Node( OutletNode ).MassFlowRateMaxAvail;
				SplitterCond( SplitterNum ).OutletMassFlowRateMinAvail( NodeNum ) = Node( OutletNode ).MassFlowRateMinAvail;

			}

		} //For FirstCall

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirLoopSplitter(
		int const SplitterNum,
		bool const FirstCall
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

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
		int OutletNodeNum;

		//The first time through the State properties are split and passed through
		if ( FirstCall ) {
			// Moisture balance to get outlet air humidity ratio
			for ( OutletNodeNum = 1; OutletNodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutletNodeNum ) {
				SplitterCond( SplitterNum ).OutletHumRat( OutletNodeNum ) = SplitterCond( SplitterNum ).InletHumRat;
			}

			// "Momentum balance" to get outlet air pressure
			for ( OutletNodeNum = 1; OutletNodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutletNodeNum ) {
				SplitterCond( SplitterNum ).OutletPressure( OutletNodeNum ) = SplitterCond( SplitterNum ).InletPressure;
			}

			// Energy balance to get outlet air enthalpy
			for ( OutletNodeNum = 1; OutletNodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutletNodeNum ) {
				SplitterCond( SplitterNum ).OutletEnthalpy( OutletNodeNum ) = SplitterCond( SplitterNum ).InletEnthalpy;
			}

			// Set outlet temperatures equal to inlet temperature
			for ( OutletNodeNum = 1; OutletNodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutletNodeNum ) {
				SplitterCond( SplitterNum ).OutletTemp( OutletNodeNum ) = SplitterCond( SplitterNum ).InletTemp;
			}

		} else {
			//This is the second time through and this is where the mass flows from each outlet are
			// summed and then assigned upstream to the inlet node.
			// Overall Mass Continuity Equation to get inlet mass flow rates
			//Zero the inlet Totals before the Inlets are summed
			SplitterCond( SplitterNum ).InletMassFlowRate = 0.0;
			SplitterCond( SplitterNum ).InletMassFlowRateMaxAvail = 0.0;
			SplitterCond( SplitterNum ).InletMassFlowRateMinAvail = 0.0;

			for ( OutletNodeNum = 1; OutletNodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++OutletNodeNum ) {
				SplitterCond( SplitterNum ).InletMassFlowRate += SplitterCond( SplitterNum ).OutletMassFlowRate( OutletNodeNum );

				SplitterCond( SplitterNum ).InletMassFlowRateMaxAvail += SplitterCond( SplitterNum ).OutletMassFlowRateMaxAvail( OutletNodeNum );
				SplitterCond( SplitterNum ).InletMassFlowRateMinAvail += SplitterCond( SplitterNum ).OutletMassFlowRateMinAvail( OutletNodeNum );

			}

			// What happens if Splitter inlet mass flow rate is greater than max available
		}

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Splitter Module
	// *****************************************************************************

	void
	UpdateSplitter(
		int const SplitterNum,
		bool & SplitterInletChanged,
		bool const FirstCall
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const FlowRateToler( 0.01 ); // Tolerance for mass flow rate convergence (in kg/s)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int NodeNum;

		//Set the inlet node for this splitter to be used throughout subroutine for either case
		InletNode = SplitterCond( SplitterNum ).InletNode;

		//On the FirstCall the State properties are passed through and the mass flows are not dealt with
		// except for NO flow conditions
		if ( FirstCall ) {
			// Set the outlet nodes for properties that just pass through & not used
			for ( NodeNum = 1; NodeNum <= SplitterCond( SplitterNum ).NumOutletNodes; ++NodeNum ) {
				OutletNode = SplitterCond( SplitterNum ).OutletNode( NodeNum );
				Node( OutletNode ).Temp = SplitterCond( SplitterNum ).OutletTemp( NodeNum );
				Node( OutletNode ).HumRat = SplitterCond( SplitterNum ).OutletHumRat( NodeNum );
				Node( OutletNode ).Enthalpy = SplitterCond( SplitterNum ).OutletEnthalpy( NodeNum );
				Node( OutletNode ).Quality = Node( InletNode ).Quality;
				Node( OutletNode ).Press = SplitterCond( SplitterNum ).OutletPressure( NodeNum );
				if ( Contaminant.CO2Simulation ) {
					Node( OutletNode ).CO2 = Node( InletNode ).CO2;
				}
				if ( Contaminant.GenericContamSimulation ) {
					Node( OutletNode ).GenContam = Node( InletNode ).GenContam;
				}
			}

		} else {
			// The second time through just updates the mass flow conditions back upstream
			//  to the inlet.  Before it sets the inlet it checks to see that the flow rate has not
			//  changed or not.  The tolerance has been relaxed some now that the splitter has been
			//  re-written

			// Set the outlet air nodes of the Splitter if the splitter results have changed
			//  beyond the tolerance.
			if ( std::abs( Node( InletNode ).MassFlowRate - SplitterCond( SplitterNum ).InletMassFlowRate ) > FlowRateToler ) {
				SplitterInletChanged = true;
			}
			Node( InletNode ).MassFlowRate = SplitterCond( SplitterNum ).InletMassFlowRate;
			Node( InletNode ).MassFlowRateMaxAvail = SplitterCond( SplitterNum ).InletMassFlowRateMaxAvail;
			Node( InletNode ).MassFlowRateMinAvail = SplitterCond( SplitterNum ).InletMassFlowRateMinAvail;

		} //The FirstCall END IF

	}

	//        End of Update subroutines for the Splitter Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Splitter Module
	// *****************************************************************************

	void
	ReportSplitter( int const EP_UNUSED( SplitterNum ) )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

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

		// Write(*,*)=SplitterCond(SplitterNum)%SplitterPower    Still needs to report the Splitter power from this component

	}

	int
	GetSplitterOutletNumber(
		std::string const & SplitterName, // must match Splitter names for the Splitter type
		int const SplitterNum, // Index of Splitters
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Feb 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given AirLoopHVAC:ZoneSplitter and returns the number of outlet nodes.  If
		// incorrect AirLoopHVAC:ZoneSplitter name is given, ErrorsFound is returned as true
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int SplitterOutletNumber;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichSplitter;

		// Obtains and Allocates AirLoopHVAC:ZoneSplitter related parameters from input file
		if ( GetSplitterInputFlag ) { //First time subroutine has been entered
			GetSplitterInput();
			GetSplitterInputFlag = false;
		}

		if ( SplitterNum == 0 ) {
			WhichSplitter = FindItemInList( SplitterName, SplitterCond, &SplitterConditions::SplitterName );
		} else {
			WhichSplitter = SplitterNum;
		}

		if ( WhichSplitter != 0 ) {
			SplitterOutletNumber = SplitterCond( WhichSplitter ).NumOutletNodes;
		}

		if ( WhichSplitter == 0 ) {
			ShowSevereError( "GetSplitterOuletNumber: Could not find Splitter = \"" + SplitterName + "\"" );
			ErrorsFound = true;
			SplitterOutletNumber = 0;
		}

		return SplitterOutletNumber;

	}

	Array1D_int
	GetSplitterNodeNumbers(
		std::string const & SplitterName, // must match Splitter names for the Splitter type
		int const SplitterNum, // Index of Splitters
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Feb 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given AirLoopHVAC:ZoneSplitter and returns the node numbers.  If
		// incorrect AirLoopHVAC:ZoneSplitter name is given, ErrorsFound is returned as true
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Array1D_int SplitterNodeNumbers;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichSplitter;
		int i;

		// Obtains and Allocates AirLoopHVAC:ZoneSplitter related parameters from input file
		if ( GetSplitterInputFlag ) { //First time subroutine has been entered
			GetSplitterInput();
			GetSplitterInputFlag = false;
		}

		if ( SplitterNum == 0 ) {
			WhichSplitter = FindItemInList( SplitterName, SplitterCond, &SplitterConditions::SplitterName );
		} else {
			WhichSplitter = SplitterNum;
		}

		if ( WhichSplitter != 0 ) {
			SplitterNodeNumbers.allocate( SplitterCond( WhichSplitter ).NumOutletNodes + 2 );
			SplitterNodeNumbers( 1 ) = SplitterCond( WhichSplitter ).InletNode;
			SplitterNodeNumbers( 2 ) = SplitterCond( WhichSplitter ).NumOutletNodes;
			for ( i = 1; i <= SplitterNodeNumbers( 2 ); ++i ) {
				SplitterNodeNumbers( i + 2 ) = SplitterCond( WhichSplitter ).OutletNode( i );
			}
		}

		if ( WhichSplitter == 0 ) {
			ShowSevereError( "GetSplitterNodeNumbers: Could not find Splitter = \"" + SplitterName + "\"" );
			ErrorsFound = true;
		}

		return SplitterNodeNumbers;

	}

	//        End of Reporting subroutines for the Splitter Module
	// *****************************************************************************

} // SplitterComponent

} // EnergyPlus
