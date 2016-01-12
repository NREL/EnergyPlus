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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <MixerComponent.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace MixerComponent {
	// Module containing the Mixer simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   March 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage Air Path Mixer Components

	// METHODOLOGY EMPLOYED:
	// This Mixer is very simple.  It just takes the inlets and sums them
	// and sets that to the outlet conditions.  For the State Properties
	// it just takes the flow weighted averages of them.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using namespace DataLoopNode;
	using namespace DataHVACGlobals;
	using DataEnvironment::OutBaroPress;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	Real64 const MassFlowTol( 0.001 );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumMixers( 0 ); // The Number of Mixers found in the Input
	int LoopInletNode( 0 );
	int LoopOutletNode( 0 );
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Mixers

	// Object Data
	Array1D< MixerConditions > MixerCond;

	namespace {
		bool SimAirMixerInputFlag( true );
		bool GetZoneMixerIndexInputFlag( true );
	}

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
		clear_state()
	{
		NumMixers = 0;
		LoopInletNode = 0;
		LoopOutletNode = 0;
		GetZoneMixerIndexInputFlag = true;
		SimAirMixerInputFlag = true;
		CheckEquipName.deallocate();
		MixerCond.deallocate();
	}

	void
	SimAirMixer(
		std::string const & CompName,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Mixer component simulation.
		// It is called from the SimAirLoopComponent
		// at the system time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
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
		int MixerNum; // The Mixer that you are currently loading input into
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool GetInputFlag( true ); // Flag set to make sure you get input once
		////////////////////////////////////////////////////////////////////////////////////

		// FLOW:

		// Obtains and Allocates Mixer related parameters from input file
		if ( SimAirMixerInputFlag ) { //First time subroutine has been entered
			GetMixerInput();
			SimAirMixerInputFlag = false;
		}

		// Find the correct MixerNumber
		if ( CompIndex == 0 ) {
			MixerNum = FindItemInList( CompName, MixerCond, &MixerConditions::MixerName );
			if ( MixerNum == 0 ) {
				ShowFatalError( "SimAirLoopMixer: Mixer not found=" + CompName );
			}
			CompIndex = MixerNum;
		} else {
			MixerNum = CompIndex;
			if ( MixerNum > NumMixers || MixerNum < 1 ) {
				ShowFatalError( "SimAirLoopMixer: Invalid CompIndex passed=" + TrimSigDigits( MixerNum ) + ", Number of Mixers=" + TrimSigDigits( NumMixers ) + ", Mixer name=" + CompName );
			}
			if ( CheckEquipName( MixerNum ) ) {
				if ( CompName != MixerCond( MixerNum ).MixerName ) {
					ShowFatalError( "SimAirLoopMixer: Invalid CompIndex passed=" + TrimSigDigits( MixerNum ) + ", Mixer name=" + CompName + ", stored Mixer Name for that index=" + MixerCond( MixerNum ).MixerName );
				}
				CheckEquipName( MixerNum ) = false;
			}
		}

		// With the correct MixerNum Initialize
		InitAirMixer( MixerNum ); // Initialize all Mixer related parameters

		CalcAirMixer( MixerNum );

		// Update the current Mixer to the outlet nodes
		UpdateAirMixer( MixerNum );

		// Report the current Mixer
		ReportMixer( MixerNum );

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetMixerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main routine to call other input routines and Get routines

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
		static std::string const RoutineName( "GetMixerInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MixerNum; // The Mixer that you are currently loading input into
		int NumAlphas;
		int NumNums;
		int NodeNum;
		int IOStat;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumParams;
		int InNodeNum1;
		int InNodeNum2;
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// Flow
		CurrentModuleObject = "AirLoopHVAC:ZoneMixer";
		NumMixers = GetNumObjectsFound( CurrentModuleObject );

		if ( NumMixers > 0 ) MixerCond.allocate( NumMixers );
		CheckEquipName.dimension( NumMixers, true );

		GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
		AlphArray.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		lAlphaBlanks.dimension( NumAlphas, true );
		cNumericFields.allocate( NumNums );
		lNumericBlanks.dimension( NumNums, true );
		NumArray.dimension( NumNums, 0.0 );

		for ( MixerNum = 1; MixerNum <= NumMixers; ++MixerNum ) {
			GetObjectItem( CurrentModuleObject, MixerNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), MixerCond, &MixerConditions::MixerName, MixerNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			MixerCond( MixerNum ).MixerName = AlphArray( 1 );

			MixerCond( MixerNum ).OutletNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			MixerCond( MixerNum ).NumInletNodes = NumAlphas - 2;

			for ( auto & e : MixerCond ) e.InitFlag = true;

			MixerCond( MixerNum ).InletNode.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletMassFlowRate.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletMassFlowRateMaxAvail.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletMassFlowRateMinAvail.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletTemp.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletHumRat.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletEnthalpy.allocate( MixerCond( MixerNum ).NumInletNodes );
			MixerCond( MixerNum ).InletPressure.allocate( MixerCond( MixerNum ).NumInletNodes );

			MixerCond( MixerNum ).InletNode = 0;
			MixerCond( MixerNum ).InletMassFlowRate = 0.0;
			MixerCond( MixerNum ).InletMassFlowRateMaxAvail = 0.0;
			MixerCond( MixerNum ).InletMassFlowRateMinAvail = 0.0;
			MixerCond( MixerNum ).InletTemp = 0.0;
			MixerCond( MixerNum ).InletHumRat = 0.0;
			MixerCond( MixerNum ).InletEnthalpy = 0.0;
			MixerCond( MixerNum ).InletPressure = 0.0;
			MixerCond( MixerNum ).OutletMassFlowRate = 0.0;
			MixerCond( MixerNum ).OutletMassFlowRateMaxAvail = 0.0;
			MixerCond( MixerNum ).OutletMassFlowRateMinAvail = 0.0;
			MixerCond( MixerNum ).OutletTemp = 0.0;
			MixerCond( MixerNum ).OutletHumRat = 0.0;
			MixerCond( MixerNum ).OutletEnthalpy = 0.0;
			MixerCond( MixerNum ).OutletPressure = 0.0;

			for ( NodeNum = 1; NodeNum <= MixerCond( MixerNum ).NumInletNodes; ++NodeNum ) {

				MixerCond( MixerNum ).InletNode( NodeNum ) = GetOnlySingleNode( AlphArray( 2 + NodeNum ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				if ( lAlphaBlanks( 2 + NodeNum ) ) {
					ShowSevereError( cAlphaFields( 2 + NodeNum ) + " is Blank, " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ErrorsFound = true;
				}

			}

		} // end Number of Mixer Loop

		// Check for duplicate names specified in Zone Mixer
		for ( MixerNum = 1; MixerNum <= NumMixers; ++MixerNum ) {
			NodeNum = MixerCond( MixerNum ).OutletNode;
			for ( InNodeNum1 = 1; InNodeNum1 <= MixerCond( MixerNum ).NumInletNodes; ++InNodeNum1 ) {
				if ( NodeNum != MixerCond( MixerNum ).InletNode( InNodeNum1 ) ) continue;
				ShowSevereError( CurrentModuleObject + " = " + MixerCond( MixerNum ).MixerName + " specifies an inlet node name the same as the outlet node." );
				ShowContinueError( ".." + cAlphaFields( 2 ) + " = " + NodeID( NodeNum ) );
				ShowContinueError( "..Inlet Node #" + TrimSigDigits( InNodeNum1 ) + " is duplicate." );
				ErrorsFound = true;
			}
			for ( InNodeNum1 = 1; InNodeNum1 <= MixerCond( MixerNum ).NumInletNodes; ++InNodeNum1 ) {
				for ( InNodeNum2 = InNodeNum1 + 1; InNodeNum2 <= MixerCond( MixerNum ).NumInletNodes; ++InNodeNum2 ) {
					if ( MixerCond( MixerNum ).InletNode( InNodeNum1 ) != MixerCond( MixerNum ).InletNode( InNodeNum2 ) ) continue;
					ShowSevereError( CurrentModuleObject + " = " + MixerCond( MixerNum ).MixerName + " specifies duplicate inlet nodes in its inlet node list." );
					ShowContinueError( "..Inlet Node #" + TrimSigDigits( InNodeNum1 ) + " Name=" + NodeID( InNodeNum1 ) );
					ShowContinueError( "..Inlet Node #" + TrimSigDigits( InNodeNum2 ) + " is duplicate." );
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
	InitAirMixer( int const MixerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for  initializations of the Mixer Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

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
		int InletNode;
		int NodeNum;
		// FLOW:

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.

		//Transfer the node data to MixerCond data structure
		for ( NodeNum = 1; NodeNum <= MixerCond( MixerNum ).NumInletNodes; ++NodeNum ) {

			InletNode = MixerCond( MixerNum ).InletNode( NodeNum );
			//Set all of the inlet mass flow variables from the nodes
			MixerCond( MixerNum ).InletMassFlowRate( NodeNum ) = Node( InletNode ).MassFlowRate;
			MixerCond( MixerNum ).InletMassFlowRateMaxAvail( NodeNum ) = Node( InletNode ).MassFlowRateMaxAvail;
			MixerCond( MixerNum ).InletMassFlowRateMinAvail( NodeNum ) = Node( InletNode ).MassFlowRateMinAvail;
			//Set all of the inlet state variables from the inlet nodes
			MixerCond( MixerNum ).InletTemp( NodeNum ) = Node( InletNode ).Temp;
			MixerCond( MixerNum ).InletHumRat( NodeNum ) = Node( InletNode ).HumRat;
			MixerCond( MixerNum ).InletEnthalpy( NodeNum ) = Node( InletNode ).Enthalpy;
			MixerCond( MixerNum ).InletPressure( NodeNum ) = Node( InletNode ).Press;

		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirMixer( int & MixerNum )
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
		using Psychrometrics::PsyTdbFnHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNodeNum;

		//Reset the totals to zero before they are summed.
		MixerCond( MixerNum ).OutletMassFlowRate = 0.0;
		MixerCond( MixerNum ).OutletMassFlowRateMaxAvail = 0.0;
		MixerCond( MixerNum ).OutletMassFlowRateMinAvail = 0.0;
		MixerCond( MixerNum ).OutletTemp = 0.0;
		MixerCond( MixerNum ).OutletHumRat = 0.0;
		MixerCond( MixerNum ).OutletPressure = 0.0;
		MixerCond( MixerNum ).OutletEnthalpy = 0.0;

		for ( InletNodeNum = 1; InletNodeNum <= MixerCond( MixerNum ).NumInletNodes; ++InletNodeNum ) {
			MixerCond( MixerNum ).OutletMassFlowRate += MixerCond( MixerNum ).InletMassFlowRate( InletNodeNum );
			MixerCond( MixerNum ).OutletMassFlowRateMaxAvail += MixerCond( MixerNum ).InletMassFlowRateMaxAvail( InletNodeNum );
			MixerCond( MixerNum ).OutletMassFlowRateMinAvail += MixerCond( MixerNum ).InletMassFlowRateMinAvail( InletNodeNum );
		}

		if ( MixerCond( MixerNum ).OutletMassFlowRate > 0.0 ) {

			// Mass balance on moisture to get outlet air humidity ratio

			for ( InletNodeNum = 1; InletNodeNum <= MixerCond( MixerNum ).NumInletNodes; ++InletNodeNum ) {
				MixerCond( MixerNum ).OutletHumRat += MixerCond( MixerNum ).InletMassFlowRate( InletNodeNum ) * MixerCond( MixerNum ).InletHumRat( InletNodeNum ) / MixerCond( MixerNum ).OutletMassFlowRate;
			}

			// "Momentum balance" to get outlet air pressure

			for ( InletNodeNum = 1; InletNodeNum <= MixerCond( MixerNum ).NumInletNodes; ++InletNodeNum ) {
				MixerCond( MixerNum ).OutletPressure += MixerCond( MixerNum ).InletPressure( InletNodeNum ) * MixerCond( MixerNum ).InletMassFlowRate( InletNodeNum ) / MixerCond( MixerNum ).OutletMassFlowRate;
			}

			// Energy balance to get outlet air enthalpy

			for ( InletNodeNum = 1; InletNodeNum <= MixerCond( MixerNum ).NumInletNodes; ++InletNodeNum ) {
				MixerCond( MixerNum ).OutletEnthalpy += MixerCond( MixerNum ).InletEnthalpy( InletNodeNum ) * MixerCond( MixerNum ).InletMassFlowRate( InletNodeNum ) / MixerCond( MixerNum ).OutletMassFlowRate;
			}

			// Use Enthalpy and humidity ratio to get outlet temperature from psych chart

			MixerCond( MixerNum ).OutletTemp = PsyTdbFnHW( MixerCond( MixerNum ).OutletEnthalpy, MixerCond( MixerNum ).OutletHumRat );

		} else {
			// Mass Flow in air loop is zero and loop is not operating.
			// Arbitrarily set the output to the first inlet leg
			MixerCond( MixerNum ).OutletHumRat = MixerCond( MixerNum ).InletHumRat( 1 );
			MixerCond( MixerNum ).OutletPressure = MixerCond( MixerNum ).InletPressure( 1 );
			MixerCond( MixerNum ).OutletEnthalpy = MixerCond( MixerNum ).InletEnthalpy( 1 );
			MixerCond( MixerNum ).OutletTemp = MixerCond( MixerNum ).InletTemp( 1 );
		}

		// make sure MassFlowRateMaxAvail is >= MassFlowRate
		MixerCond( MixerNum ).OutletMassFlowRateMaxAvail = max( MixerCond( MixerNum ).OutletMassFlowRateMaxAvail, MixerCond( MixerNum ).OutletMassFlowRate );

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Mixer Module
	// *****************************************************************************

	void
	UpdateAirMixer( int const MixerNum )
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
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;
		int InletNode;
		int InletNodeNum;

		OutletNode = MixerCond( MixerNum ).OutletNode;
		InletNode = MixerCond( MixerNum ).InletNode( 1 ); // For now use first inlet node

		// Set the outlet air nodes of the Mixer
		Node( OutletNode ).MassFlowRate = MixerCond( MixerNum ).OutletMassFlowRate;
		Node( OutletNode ).MassFlowRateMaxAvail = MixerCond( MixerNum ).OutletMassFlowRateMaxAvail;
		Node( OutletNode ).MassFlowRateMinAvail = MixerCond( MixerNum ).OutletMassFlowRateMinAvail;
		Node( OutletNode ).Temp = MixerCond( MixerNum ).OutletTemp;
		Node( OutletNode ).HumRat = MixerCond( MixerNum ).OutletHumRat;
		Node( OutletNode ).Enthalpy = MixerCond( MixerNum ).OutletEnthalpy;
		Node( OutletNode ).Press = MixerCond( MixerNum ).OutletPressure;
		// Set the outlet nodes for properties that just pass through & not used
		Node( OutletNode ).Quality = Node( InletNode ).Quality;

		if ( Contaminant.CO2Simulation ) {
			if ( MixerCond( MixerNum ).OutletMassFlowRate > 0.0 ) {
				// CO2 balance to get outlet air CO2
				Node( OutletNode ).CO2 = 0.0;
				for ( InletNodeNum = 1; InletNodeNum <= MixerCond( MixerNum ).NumInletNodes; ++InletNodeNum ) {
					Node( OutletNode ).CO2 += Node( MixerCond( MixerNum ).InletNode( InletNodeNum ) ).CO2 * MixerCond( MixerNum ).InletMassFlowRate( InletNodeNum ) / MixerCond( MixerNum ).OutletMassFlowRate;
				}
			} else {
				Node( OutletNode ).CO2 = Node( InletNode ).CO2;
			}
		}

		if ( Contaminant.GenericContamSimulation ) {
			if ( MixerCond( MixerNum ).OutletMassFlowRate > 0.0 ) {
				// Generic contaminant balance to get outlet air CO2
				Node( OutletNode ).GenContam = 0.0;
				for ( InletNodeNum = 1; InletNodeNum <= MixerCond( MixerNum ).NumInletNodes; ++InletNodeNum ) {
					Node( OutletNode ).GenContam += Node( MixerCond( MixerNum ).InletNode( InletNodeNum ) ).GenContam * MixerCond( MixerNum ).InletMassFlowRate( InletNodeNum ) / MixerCond( MixerNum ).OutletMassFlowRate;
				}
			} else {
				Node( OutletNode ).GenContam = Node( InletNode ).GenContam;
			}
		}

	}

	//        End of Update subroutines for the Mixer Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Mixer Module
	// *****************************************************************************

	void
	ReportMixer( int const EP_UNUSED( MixerNum ) )
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

		// Write(*,*)=MixerCond(MixerNum)%MixerPower    Still needs to report the Mixer power from this component

	}

	//        End of Reporting subroutines for the Mixer Module

	// Beginning of Utility subroutines for the Mixer Component
	// *****************************************************************************

	void
	GetZoneMixerIndex(
		std::string const & MixerName,
		int & MixerIndex,
		bool & ErrorsFound,
		std::string const & ThisObjectType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given zone mixer -- issues error message if that mixer
		// is not legal mixer.

		// METHODOLOGY EMPLOYED:
		// na

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
		// na
		if ( GetZoneMixerIndexInputFlag ) { //First time subroutine has been entered
			GetMixerInput();
			GetZoneMixerIndexInputFlag = false;
		}

		MixerIndex = FindItemInList( MixerName, MixerCond, &MixerConditions::MixerName );
		if ( MixerIndex == 0 ) {
			if ( ! ThisObjectType.empty() ) {
				ShowSevereError( ThisObjectType + ", GetZoneMixerIndex: Zone Mixer not found=" + MixerName );
			} else {
				ShowSevereError( "GetZoneMixerIndex: Zone Mixer not found=" + MixerName );
			}
			ErrorsFound = true;
		}

	}

	// End of Utility subroutines for the Mixer Component
	// *****************************************************************************

} // MixerComponent

} // EnergyPlus
