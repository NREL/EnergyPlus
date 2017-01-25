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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <TranspiredCollector.hh>
#include <BranchNodeConnections.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace TranspiredCollector {

	// Module containing routines and data dealing with the Transpired Collectors

	// MODULE INFORMATION:
	//       AUTHOR         B.T. Griffith
	//       DATE WRITTEN   November 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Ecapsulates data and routines for simulating unglazed transpired solar collectors (UTSC)
	//   as a component on the HVAC air system.

	// METHODOLOGY EMPLOYED:
	// Two modes, passive and active.  Active is when air is purposely drawn through collector.
	// Passive is when air exchanges are driven by Natural Ventilation rather than outside air system

	// REFERENCES:
	// Heat Exchange effectiveness relations:
	// Kutscher, C.F. 1994. Heat exchange effectiveness and pressure drop for air flow through perforated plates
	//     with and without crosswind. Journal of Heat Transfer. May 1994, Vol. 116, p. 391.
	//     American Society of Mechanical Engineers.
	// Van Decker, G.W.E., K.G.T. Hollands, and A.P. Brunger. 2001. Heat-exchange relations for unglazed transpired
	//     solar collectors with circular holes on a square of triangular pitch. Solar Energy. Vol. 71, No. 1. pp 33-45, 2001.
	// .

	// OTHER NOTES:
	// EnergyPlus implementation is unique and adds new modeling not described in Literature.
	//   See EngineeringReference for details

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DegToRadians;
	using DataGlobals::KelvinConv;
	using DataGlobals::SecInHour;
	using DataVectorTypes::Vector;
	using DataHeatBalance::QRadSWOutIncident;
	using DataHeatBalance::Construct;
	using DataHeatBalance::Material;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const Layout_Square( 1 );
	int const Layout_Triangle( 2 );
	int const Correlation_Kutscher1994( 1 );
	int const Correlation_VanDeckerHollandsBrunger2001( 2 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumUTSC( 0 ); // number of transpired collectors in model
	Array1D_bool CheckEquipName;
	bool GetInputFlag( true ); // First time, input is gotten

	// SUBROUTINE SPECIFICATIONS FOR MODULE TranspiredCollector:

	// Object Data
	Array1D< UTSCDataStruct > UTSC;

	// Functions

	void
	SimTranspiredCollector(
		std::string const & CompName, // component name
		int & CompIndex // component index (to reduce string compares during simulation)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.T. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manage simulation of Transpired Collectors

		// METHODOLOGY EMPLOYED:
		// Setup to avoid string comparisons after first call

		// REFERENCES:
		//  none

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHVACGlobals::TempControlTol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static int UTSCNum( 0 ); // local number index for UTSC

		if ( GetInputFlag ) {
			GetTranspiredCollectorInput();
			GetInputFlag = false;
		}

		// Find the correct transpired collector with the Component name and/or index
		if ( CompIndex == 0 ) {
			UTSCNum = FindItemInList( CompName, UTSC );
			if ( UTSCNum == 0 ) {
				ShowFatalError( "Transpired Collector not found=" + CompName );
			}
			CompIndex = UTSCNum;
		} else {
			UTSCNum = CompIndex;
			if ( UTSCNum > NumUTSC || UTSCNum < 1 ) {
				ShowFatalError( "SimTranspiredCollector: Invalid CompIndex passed=" + TrimSigDigits( UTSCNum ) + ", Number of Transpired Collectors=" + TrimSigDigits( NumUTSC ) + ", UTSC name=" + CompName );
			}
			if ( CheckEquipName( UTSCNum ) ) {
				if ( CompName != UTSC( UTSCNum ).Name ) {
					ShowFatalError( "SimTranspiredCollector: Invalid CompIndex passed=" + TrimSigDigits( UTSCNum ) + ", Transpired Collector name=" + CompName + ", stored Transpired Collector Name for that index=" + UTSC( UTSCNum ).Name );
				}
				CheckEquipName( UTSCNum ) = false;
			}
		}

		InitTranspiredCollector( CompIndex );

		// Control point of deciding if transpired collector is active or not.
		auto & UTSC_CI( UTSC( CompIndex ) );
		auto & InletNode( UTSC_CI.InletNode );
		auto & ControlNode( UTSC_CI.ControlNode );
		UTSC_CI.IsOn = false;
		if ( ( GetCurrentScheduleValue( UTSC_CI.SchedPtr ) > 0.0 ) && ( UTSC_CI.InletMDot > 0.0 ) ) { // availability Schedule | OA system is setting mass flow
			bool ControlLTSet( false );
			bool ControlLTSchedule( false );
			bool ZoneLTSchedule( false );
			assert( equal_dimensions( InletNode, ControlNode ) );
			assert( equal_dimensions( InletNode, UTSC_CI.ZoneNode ) );
			for ( int i = InletNode.l(), e = InletNode.u(); i <= e; ++i ) {
				if ( Node( InletNode( i ) ).Temp + TempControlTol < Node( ControlNode( i ) ).TempSetPoint ) ControlLTSet = true;
				if ( Node( InletNode( i ) ).Temp + TempControlTol < GetCurrentScheduleValue( UTSC_CI.FreeHeatSetPointSchedPtr ) ) ControlLTSchedule = true;
				if ( Node( UTSC_CI.ZoneNode( i ) ).Temp + TempControlTol < GetCurrentScheduleValue( UTSC_CI.FreeHeatSetPointSchedPtr ) ) ZoneLTSchedule = true;
			}
			if ( ControlLTSet || ( ControlLTSchedule && ZoneLTSchedule ) ) UTSC_CI.IsOn = true; // heating required | free heating helpful | free heating helpful
		}

		if ( UTSC( UTSCNum ).IsOn ) {
			CalcActiveTranspiredCollector( UTSCNum );
		} else {
			CalcPassiveTranspiredCollector( UTSCNum );
		}

		UpdateTranspiredCollector( UTSCNum );

	}

	void
	GetTranspiredCollectorInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.T. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Retrieve user input and set up data structure

		// METHODOLOGY EMPLOYED:
		// usual EnergyPlus input
		// Extensible UTSC object for underlying heat transfer surfaces and for multisystem

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using DataGlobals::Pi;
		using DataGlobals::ScheduleAlwaysOn;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceData;
		using DataSurfaces::OSCM;
		using DataSurfaces::OtherSideCondModeledExt;
		using ScheduleManager::GetScheduleIndex;
		using DataLoopNode::NodeType_Air;
		using DataLoopNode::NodeConnectionType_Inlet;
		using DataLoopNode::NodeConnectionType_Outlet;
		using DataLoopNode::ObjectIsNotParent;
		using DataLoopNode::NodeConnectionType_Sensor;
		using NodeInputManager::GetOnlySingleNode;
		using DataHeatBalance::VeryRough;
		using DataHeatBalance::Rough;
		using DataHeatBalance::MediumRough;
		using DataHeatBalance::MediumSmooth;
		using DataHeatBalance::Smooth;
		using DataHeatBalance::VerySmooth;
		using BranchNodeConnections::TestCompSet;

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

		Array1D_string Alphas; // Alpha items for extensible
		// Solar Collectors:Unglazed Transpired object
		int Item; // Item to be "gotten"
		Array1D< Real64 > Numbers( 11 ); // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int MaxNumAlphas; // argumenet for call to GetObjectDefMaxArgs
		int MaxNumNumbers; // argumenet for call to GetObjectDefMaxArgs
		int Dummy; // argumenet for call to GetObjectDefMaxArgs
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int Found;
		int AlphaOffset; // local temp var
		std::string Roughness;
		int ThisSurf; // do loop counter
		Real64 AvgAzimuth; // temp for error checking
		Real64 AvgTilt; // temp for error checking
		int SurfID; // local surface "pointer"
		Real64 TiltRads; // average tilt of collector in radians
		Real64 tempHdeltaNPL; // temporary variable for bouyancy length scale
		int NumUTSCSplitter( 0 );
		Array1D_string AlphasSplit; // Alpha items for extensible
		// Solar Collectors:Unglazed Transpired object
		int ItemSplit; // Item to be "gotten"
		Array1D< Real64 > NumbersSplit( 1 ); // Numeric items for object
		int NumAlphasSplit; // Number of Alphas for each GetObjectItem call
		int NumNumbersSplit; // Number of Numbers for each GetObjectItem call
		int MaxNumAlphasSplit; // argumenet for call to GetObjectDefMaxArgs
		int MaxNumNumbersSplit; // argumenet for call to GetObjectDefMaxArgs
		int IOStatusSplit; // Used in GetObjectItem
		int NumOASys; // do loop counter
		int ACountBase; // counter for alhpasSplit
		Array1D_bool SplitterNameOK; // check for correct association of
		std::string CurrentModuleObject; // for ease in renaming.
		std::string CurrentModuleMultiObject; // for ease in renaming.

		CurrentModuleObject = "SolarCollector:UnglazedTranspired";
		GetObjectDefMaxArgs( CurrentModuleObject, Dummy, MaxNumAlphas, MaxNumNumbers );

		if ( MaxNumNumbers != 11 ) {
			ShowSevereError( "GetTranspiredCollectorInput: " + CurrentModuleObject + " Object Definition indicates not = 11 Number Objects, Number Indicated=" + TrimSigDigits( MaxNumNumbers ) );
			ErrorsFound = true;
		}
		Alphas.allocate( MaxNumAlphas );
		Numbers = 0.0;
		Alphas = "";

		NumUTSC = GetNumObjectsFound( CurrentModuleObject );
		CurrentModuleMultiObject = "SolarCollector:UnglazedTranspired:Multisystem";
		NumUTSCSplitter = GetNumObjectsFound( CurrentModuleMultiObject );

		UTSC.allocate( NumUTSC );
		CheckEquipName.dimension( NumUTSC, true );
		SplitterNameOK.dimension( NumUTSCSplitter, false );

		for ( Item = 1; Item <= NumUTSC; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// first handle alphas
			UTSC( Item ).Name = Alphas( 1 );

			// now check for multisystem
			if ( NumUTSCSplitter > 0 ) {
				GetObjectDefMaxArgs( CurrentModuleMultiObject, Dummy, MaxNumAlphasSplit, MaxNumNumbersSplit );

				if ( MaxNumNumbersSplit != 0 ) {
					ShowSevereError( "GetTranspiredCollectorInput: " + CurrentModuleMultiObject + " Object Definition indicates not = 0 Number Objects, Number Indicated=" + TrimSigDigits( MaxNumNumbersSplit ) );
					ErrorsFound = true;
				}
				if ( ! allocated( AlphasSplit ) ) AlphasSplit.allocate( MaxNumAlphasSplit );
				NumbersSplit = 0.0;
				AlphasSplit = "";
				for ( ItemSplit = 1; ItemSplit <= NumUTSCSplitter; ++ItemSplit ) {
					GetObjectItem( CurrentModuleMultiObject, ItemSplit, AlphasSplit, NumAlphasSplit, NumbersSplit, NumNumbersSplit, IOStatusSplit );
					if ( ! ( SameString( AlphasSplit( 1 ), Alphas( 1 ) ) ) ) continue;
					SplitterNameOK( ItemSplit ) = true;
					UTSC( Item ).NumOASysAttached = std::floor( NumAlphasSplit / 4.0 );
					if ( mod( ( NumAlphasSplit ), 4 ) != 1 ) {
						ShowSevereError( "GetTranspiredCollectorInput: " + CurrentModuleMultiObject + " Object Definition indicates not uniform quadtuples of nodes for " + AlphasSplit( 1 ) );
						ErrorsFound = true;
					}
					UTSC( Item ).InletNode.allocate( UTSC( Item ).NumOASysAttached );
					UTSC( Item ).InletNode = 0;
					UTSC( Item ).OutletNode.allocate( UTSC( Item ).NumOASysAttached );
					UTSC( Item ).OutletNode = 0;
					UTSC( Item ).ControlNode.allocate( UTSC( Item ).NumOASysAttached );
					UTSC( Item ).ControlNode = 0;
					UTSC( Item ).ZoneNode.allocate( UTSC( Item ).NumOASysAttached );
					UTSC( Item ).ZoneNode = 0;
					for ( NumOASys = 1; NumOASys <= UTSC( Item ).NumOASysAttached; ++NumOASys ) {
						ACountBase = ( NumOASys - 1 ) * 4 + 2;
						UTSC( Item ).InletNode( NumOASys ) = GetOnlySingleNode( AlphasSplit( ACountBase ), ErrorsFound, CurrentModuleObject, AlphasSplit( 1 ), NodeType_Air, NodeConnectionType_Inlet, NumOASys, ObjectIsNotParent );

						UTSC( Item ).OutletNode( NumOASys ) = GetOnlySingleNode( AlphasSplit( ACountBase + 1 ), ErrorsFound, CurrentModuleObject, AlphasSplit( 1 ), NodeType_Air, NodeConnectionType_Outlet, NumOASys, ObjectIsNotParent );
						TestCompSet( CurrentModuleObject, AlphasSplit( 1 ), AlphasSplit( ACountBase ), AlphasSplit( ACountBase + 1 ), "Transpired Collector Air Nodes" ); //appears that test fails by design??
						UTSC( Item ).ControlNode( NumOASys ) = GetOnlySingleNode( AlphasSplit( ACountBase + 2 ), ErrorsFound, CurrentModuleObject, AlphasSplit( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

						UTSC( Item ).ZoneNode( NumOASys ) = GetOnlySingleNode( AlphasSplit( ACountBase + 3 ), ErrorsFound, CurrentModuleObject, AlphasSplit( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

					} // Each OA System in a Multisystem
					// DEALLOCATE(AlphasSplit)
				} // each Multisystem present
			} // any UTSC Multisystem present

			UTSC( Item ).OSCMName = Alphas( 2 );
			Found = FindItemInList( UTSC( Item ).OSCMName, OSCM );
			if ( Found == 0 ) {
				ShowSevereError( cAlphaFieldNames( 2 ) + " not found=" + UTSC( Item ).OSCMName + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				ErrorsFound = true;
			}
			UTSC( Item ).OSCMPtr = Found;
			if ( lAlphaFieldBlanks( 3 ) ) {
				UTSC( Item ).SchedPtr = ScheduleAlwaysOn;
			} else {
				UTSC( Item ).SchedPtr = GetScheduleIndex( Alphas( 3 ) );
				if ( UTSC( Item ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFieldNames( 3 ) + "not found=" + Alphas( 3 ) + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
					ErrorsFound = true;
					continue;
				}
			}

			//now if UTSC(Item)%NumOASysAttached still not set, assume no multisystem
			if ( UTSC( Item ).NumOASysAttached == 0 ) {
				UTSC( Item ).NumOASysAttached = 1;
				UTSC( Item ).InletNode.allocate( 1 );
				UTSC( Item ).InletNode( 1 ) = 0;
				UTSC( Item ).OutletNode.allocate( 1 );
				UTSC( Item ).OutletNode( 1 ) = 0;
				UTSC( Item ).ControlNode.allocate( 1 );
				UTSC( Item ).ControlNode( 1 ) = 0;
				UTSC( Item ).ZoneNode.allocate( 1 );
				UTSC( Item ).ZoneNode( 1 ) = 0;

				UTSC( Item ).InletNode( 1 ) = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				UTSC( Item ).OutletNode( 1 ) = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 4 ), Alphas( 5 ), "Transpired Collector Air Nodes" );

				UTSC( Item ).ControlNode( 1 ) = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
				UTSC( Item ).ZoneNode( 1 ) = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			} //no splitter

			UTSC( Item ).FreeHeatSetPointSchedPtr = GetScheduleIndex( Alphas( 8 ) );
			if ( UTSC( Item ).FreeHeatSetPointSchedPtr == 0 ) {
				ShowSevereError( cAlphaFieldNames( 8 ) + " not found=" + Alphas( 8 ) + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				ErrorsFound = true;
				continue;
			}

			if ( SameString( Alphas( 9 ), "Triangle" ) ) {
				UTSC( Item ).Layout = Layout_Triangle;
			} else if ( SameString( Alphas( 9 ), "Square" ) ) {
				UTSC( Item ).Layout = Layout_Square;
			} else {
				ShowSevereError( cAlphaFieldNames( 9 ) + " has incorrect entry of " + Alphas( 9 ) + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				ErrorsFound = true;
				continue;
			}

			if ( SameString( Alphas( 10 ), "Kutscher1994" ) ) {
				UTSC( Item ).Correlation = Correlation_Kutscher1994;
			} else if ( SameString( Alphas( 10 ), "VanDeckerHollandsBrunger2001" ) ) {
				UTSC( Item ).Correlation = Correlation_VanDeckerHollandsBrunger2001;
			} else {
				ShowSevereError( cAlphaFieldNames( 10 ) + " has incorrect entry of " + Alphas( 9 ) + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				ErrorsFound = true;
				continue;
			}

			Roughness = Alphas( 11 );
			//Select the correct Number for the associated ascii name for the roughness type
			if ( SameString( Roughness, "VeryRough" ) ) UTSC( Item ).CollRoughness = VeryRough;
			if ( SameString( Roughness, "Rough" ) ) UTSC( Item ).CollRoughness = Rough;
			if ( SameString( Roughness, "MediumRough" ) ) UTSC( Item ).CollRoughness = MediumRough;
			if ( SameString( Roughness, "MediumSmooth" ) ) UTSC( Item ).CollRoughness = MediumSmooth;
			if ( SameString( Roughness, "Smooth" ) ) UTSC( Item ).CollRoughness = Smooth;
			if ( SameString( Roughness, "VerySmooth" ) ) UTSC( Item ).CollRoughness = VerySmooth;

			// Was it set?
			if ( UTSC( Item ).CollRoughness == 0 ) {
				ShowSevereError( cAlphaFieldNames( 11 ) + " has incorrect entry of " + Alphas( 11 ) + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				ErrorsFound = true;
			}

			AlphaOffset = 11;
			UTSC( Item ).NumSurfs = NumAlphas - AlphaOffset;
			if ( UTSC( Item ).NumSurfs == 0 ) {
				ShowSevereError( "No underlying surfaces specified in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				ErrorsFound = true;
				continue;
			}
			UTSC( Item ).SurfPtrs.allocate( UTSC( Item ).NumSurfs );
			UTSC( Item ).SurfPtrs = 0;
			for ( ThisSurf = 1; ThisSurf <= UTSC( Item ).NumSurfs; ++ThisSurf ) {
				Found = FindItemInList( Alphas( ThisSurf + AlphaOffset ), Surface );
				if ( Found == 0 ) {
					ShowSevereError( "Surface Name not found=" + Alphas( ThisSurf + AlphaOffset ) + " in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
					ErrorsFound = true;
					continue;
				}
				// check that surface is appropriate, Heat transfer, Sun, Wind,
				if ( ! Surface( Found ).HeatTransSurf ) {
					ShowSevereError( "Surface " + Alphas( ThisSurf + AlphaOffset ) + " not of Heat Transfer type in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
					ErrorsFound = true;
					continue;
				}
				if ( ! Surface( Found ).ExtSolar ) {
					ShowSevereError( "Surface " + Alphas( ThisSurf + AlphaOffset ) + " not exposed to sun in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
					ErrorsFound = true;
					continue;
				}
				if ( ! Surface( Found ).ExtWind ) {
					ShowSevereError( "Surface " + Alphas( ThisSurf + AlphaOffset ) + " not exposed to wind in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
					ErrorsFound = true;
					continue;
				}
				if ( Surface( Found ).ExtBoundCond != OtherSideCondModeledExt ) {
					ShowSevereError( "Surface " + Alphas( ThisSurf + AlphaOffset ) + " does not have OtherSideConditionsModel for exterior boundary conditions in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
					ErrorsFound = true;
					continue;
				}
				// check surface orientation, warn if upside down
				if ( ( Surface( Found ).Tilt < -95.0 ) || ( Surface( Found ).Tilt > 95.0 ) ) {
					ShowWarningError( "Suspected input problem with collector surface = " + Alphas( ThisSurf + AlphaOffset ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + UTSC( Item ).Name );
					ShowContinueError( "Surface used for solar collector faces down" );
					ShowContinueError( "Surface tilt angle (degrees from ground outward normal) = " + RoundSigDigits( Surface( Found ).Tilt, 2 ) );
				}

				UTSC( Item ).SurfPtrs( ThisSurf ) = Found;

			}

			if ( ErrorsFound ) continue; // previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

			// now that we should have all the surfaces, do some preperations and checks.

			// are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
			Real64 const surfaceArea( sum_sub( Surface, &SurfaceData::Area, UTSC( Item ).SurfPtrs ) );
//			AvgAzimuth = sum( Surface( UTSC( Item ).SurfPtrs ).Azimuth * Surface( UTSC( Item ).SurfPtrs ).Area ) / sum( Surface( UTSC( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			AvgAzimuth = sum_product_sub( Surface, &SurfaceData::Azimuth, &SurfaceData::Area, UTSC( Item ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
//			AvgTilt = sum( Surface( UTSC( Item ).SurfPtrs ).Tilt * Surface( UTSC( Item ).SurfPtrs ).Area ) / sum( Surface( UTSC( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			AvgTilt = sum_product_sub( Surface, &SurfaceData::Tilt, &SurfaceData::Area, UTSC( Item ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
			for ( ThisSurf = 1; ThisSurf <= UTSC( Item ).NumSurfs; ++ThisSurf ) {
				SurfID = UTSC( Item ).SurfPtrs( ThisSurf );
				if ( std::abs( Surface( SurfID ).Azimuth - AvgAzimuth ) > 15.0 ) {
					ShowWarningError( "Surface " + Surface( SurfID ).Name + " has Azimuth different from others in the group associated with " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				}
				if ( std::abs( Surface( SurfID ).Tilt - AvgTilt ) > 10.0 ) {
					ShowWarningError( "Surface " + Surface( SurfID ).Name + " has Tilt different from others in the group associated with " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				}

				//test that there are no windows.  Now allow windows
				// If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
				//      Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
				//         //'subtracted in the group of surfaces associated with '//TRIM(UTSC(Item)%Name))
				// endif

			}
			UTSC( Item ).Tilt = AvgTilt;
			UTSC( Item ).Azimuth = AvgAzimuth;

			// find area weighted centroid.
			//    UTSC(Item)%Centroid%x = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%x*Surface(UTSC(Item)%SurfPtrs)%Area) &
			//                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
			//    UTSC(Item)%Centroid%y = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%y*Surface(UTSC(Item)%SurfPtrs)%Area) &
			//                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
//			UTSC( Item ).Centroid.z = sum( Surface( UTSC( Item ).SurfPtrs ).Centroid.z * Surface( UTSC( Item ).SurfPtrs ).Area ) / sum( Surface( UTSC( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			UTSC( Item ).Centroid.z = sum_product_sub( Surface, &SurfaceData::Centroid, &Vector::z, Surface, &SurfaceData::Area, UTSC( Item ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage

			//now handle numbers from input object
			UTSC( Item ).HoleDia = Numbers( 1 );
			UTSC( Item ).Pitch = Numbers( 2 );
			UTSC( Item ).LWEmitt = Numbers( 3 );
			UTSC( Item ).SolAbsorp = Numbers( 4 );
			UTSC( Item ).Height = Numbers( 5 );
			UTSC( Item ).PlenGapThick = Numbers( 6 );
			if ( UTSC( Item ).PlenGapThick <= 0.0 ) {
				ShowSevereError( "Plenum gap must be greater than Zero in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				continue;
			}
			UTSC( Item ).PlenCrossArea = Numbers( 7 );
			UTSC( Item ).AreaRatio = Numbers( 8 );
			UTSC( Item ).CollectThick = Numbers( 9 );
			UTSC( Item ).Cv = Numbers( 10 );
			UTSC( Item ).Cd = Numbers( 11 );

			// Fill out data we now know
			// sum areas of HT surface areas
//			UTSC( Item ).ProjArea = sum( Surface( UTSC( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			UTSC( Item ).ProjArea = surfaceArea;
			if ( UTSC( Item ).ProjArea == 0 ) {
				ShowSevereError( "Gross area of underlying surfaces is zero in " + CurrentModuleObject + " =" + UTSC( Item ).Name );
				continue;
			}
			UTSC( Item ).ActualArea = UTSC( Item ).ProjArea * UTSC( Item ).AreaRatio;
			//  need to update this for slots as well as holes
			{ auto const SELECT_CASE_var( UTSC( Item ).Layout );
			if ( SELECT_CASE_var == Layout_Triangle ) { // 'TRIANGLE'
				UTSC( Item ).Porosity = 0.907 * pow_2( UTSC( Item ).HoleDia / UTSC( Item ).Pitch ); //Kutscher equation, Triangle layout
			} else if ( SELECT_CASE_var == Layout_Square ) { // 'SQUARE'
				UTSC( Item ).Porosity = ( Pi / 4.0 ) * pow_2( UTSC( Item ).HoleDia ) / pow_2( UTSC( Item ).Pitch ); //Waterloo equation, square layout
			}}
			TiltRads = std::abs( AvgTilt ) * DegToRadians;
			tempHdeltaNPL = std::sin( TiltRads ) * UTSC( Item ).Height / 4.0;
			UTSC( Item ).HdeltaNPL = max( tempHdeltaNPL, UTSC( Item ).PlenGapThick );

			SetupOutputVariable( "Solar Collector Heat Exchanger Effectiveness []", UTSC( Item ).HXeff, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Leaving Air Temperature [C]", UTSC( Item ).TairHX, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Outside Face Suction Velocity [m/s]", UTSC( Item ).Vsuction, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Surface Temperature [C]", UTSC( Item ).Tcoll, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Plenum Air Temperature [C]", UTSC( Item ).Tplen, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Sensible Heating Rate [W]", UTSC( Item ).SensHeatingRate, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Sensible Heating Energy [J]", UTSC( Item ).SensHeatingEnergy, "System", "Sum", UTSC( Item ).Name, _, "SolarAir", "HeatProduced", _, "System" );

			SetupOutputVariable( "Solar Collector Natural Ventilation Air Change Rate [ACH]", UTSC( Item ).PassiveACH, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Natural Ventilation Mass Flow Rate [kg/s]", UTSC( Item ).PassiveMdotVent, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Wind Natural Ventilation Mass Flow Rate [kg/s]", UTSC( Item ).PassiveMdotWind, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Buoyancy Natural Ventilation Mass Flow Rate [kg/s]", UTSC( Item ).PassiveMdotTherm, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Incident Solar Radiation [W/m2]", UTSC( Item ).Isc, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector System Efficiency []", UTSC( Item ).UTSCEfficiency, "System", "Average", UTSC( Item ).Name );
			SetupOutputVariable( "Solar Collector Surface Efficiency []", UTSC( Item ).UTSCCollEff, "System", "Average", UTSC( Item ).Name );

		}

		for ( ItemSplit = 1; ItemSplit <= NumUTSCSplitter; ++ItemSplit ) {
			if ( ! SplitterNameOK( ItemSplit ) ) {
				ShowSevereError( "Did not find a match, check names for Solar Collectors:Transpired Collector:Multisystem" );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetTranspiredCollectorInput: Errors found in input" );
		}

		Alphas.deallocate();

	}

	void
	InitTranspiredCollector( int const UTSCNum ) // compindex already checked in calling routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.T. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       B. Griffith, May 2009, added EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataGlobals;
		using DataHVACGlobals::DoSetPointTest;
		using DataHVACGlobals::SetPointErrorFlag;
		using namespace DataLoopNode;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		int UTSCUnitNum;
		static bool MySetPointCheckFlag( true );
		static Array1D_bool MyEnvrnFlag;
		int ControlNode;
		//unused  INTEGER             :: InletNode
		int SplitBranch;
		int thisUTSC;

		if ( MyOneTimeFlag ) {
			// do various one time setups and pitch adjustments across all UTSC
			for ( thisUTSC = 1; thisUTSC <= NumUTSC; ++thisUTSC ) {
				if ( UTSC( thisUTSC ).Layout == Layout_Triangle ) {
					{ auto const SELECT_CASE_var( UTSC( thisUTSC ).Correlation );
					if ( SELECT_CASE_var == Correlation_Kutscher1994 ) { // Kutscher1994
						UTSC( thisUTSC ).Pitch = UTSC( thisUTSC ).Pitch;
					} else if ( SELECT_CASE_var == Correlation_VanDeckerHollandsBrunger2001 ) { // VanDeckerHollandsBrunger2001
						UTSC( thisUTSC ).Pitch /= 1.6;
					}}
				}
				if ( UTSC( thisUTSC ).Layout == Layout_Square ) {
					{ auto const SELECT_CASE_var( UTSC( thisUTSC ).Correlation );
					if ( SELECT_CASE_var == Correlation_Kutscher1994 ) { // Kutscher1994
						UTSC( thisUTSC ).Pitch *= 1.6;
					} else if ( SELECT_CASE_var == Correlation_VanDeckerHollandsBrunger2001 ) { // VanDeckerHollandsBrunger2001
						UTSC( thisUTSC ).Pitch = UTSC( thisUTSC ).Pitch;
					}}
				}
			}

			MyEnvrnFlag.dimension( NumUTSC, true );
			MyOneTimeFlag = false;
		} //first time

		//Check that setpoint is active (from test by RJL in HVACEvapComponent)
		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			for ( UTSCUnitNum = 1; UTSCUnitNum <= NumUTSC; ++UTSCUnitNum ) {
				for ( SplitBranch = 1; SplitBranch <= UTSC( UTSCUnitNum ).NumOASysAttached; ++SplitBranch ) {
					ControlNode = UTSC( UTSCUnitNum ).ControlNode( SplitBranch );
					if ( ControlNode > 0 ) {
						if ( Node( ControlNode ).TempSetPoint == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( "Missing temperature setpoint for UTSC " + UTSC( UTSCUnitNum ).Name );
								ShowContinueError( " use a Setpoint Manager to establish a setpoint at the unit control node." );
								SetPointErrorFlag = true;
							} else {
								// need call to EMS to check node
								CheckIfNodeSetPointManagedByEMS( ControlNode, iTemperatureSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( "Missing temperature setpoint for UTSC " + UTSC( UTSCUnitNum ).Name );
									ShowContinueError( " use a Setpoint Manager to establish a setpoint at the unit control node." );
									ShowContinueError( "Or add EMS Actuator to provide temperature setpoint at this node" );
								}
							}
						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag( UTSCNum ) ) {
			UTSC( UTSCNum ).TplenLast = 22.5;
			UTSC( UTSCNum ).TcollLast = 22.0;
			MyEnvrnFlag( UTSCNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( UTSCNum ) = true;
		}

		//inits for each iteration
//		UTSC( UTSCNum ).InletMDot = sum( Node( UTSC( UTSCNum ).InletNode ).MassFlowRate ); //Autodesk:F2C++ Array subscript usage: Replaced by below
		UTSC( UTSCNum ).InletMDot = sum_sub( Node, &DataLoopNode::NodeData::MassFlowRate, UTSC( UTSCNum ).InletNode ); //Autodesk:F2C++ Functions handle array subscript usage
		UTSC( UTSCNum ).IsOn = false; // intialize then turn on if appropriate
		UTSC( UTSCNum ).Tplen = 0.0;
		UTSC( UTSCNum ).Tcoll = 0.0;
		UTSC( UTSCNum ).MdotVent = 0.0;
		UTSC( UTSCNum ).TairHX = 0.0;
		UTSC( UTSCNum ).HXeff = 0.0;
		UTSC( UTSCNum ).Isc = 0.0;

		UTSC( UTSCNum ).UTSCEfficiency = 0.0;
		UTSC( UTSCNum ).UTSCCollEff = 0.0;

	}

	void
	CalcActiveTranspiredCollector( int const UTSCNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.T. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::SkyTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::IsRain;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHFnTdbW;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceData;
		using DataHeatBalSurface::TH;
		using DataHVACGlobals::TimeStepSys;
		using ConvectionCoefficients::InitExteriorConvectionCoeff;
		using General::RoundSigDigits;
		using namespace DataHeatBalance; // , ONLY: QRadSWOutIncident, Construct, Material

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const nu( 15.66e-6 ); // kinematic viscosity (m**2/s) for air at 300 K
		// (Mills 1999 Heat Transfer)
		Real64 const k( 0.0267 ); // thermal conductivity (W/m K) for air at 300 K
		// (Mills 1999 Heat Transfer)
		Real64 const Sigma( 5.6697e-08 ); // Stefan-Boltzmann constant
		//  REAL(r64), PARAMETER  :: KelvinConv = KelvinConv         ! Conversion from Celsius to Kelvin
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		// following arrays are used to temporarily hold results from multiple underlying surfaces
		Array1D< Real64 > HSkyARR;
		Array1D< Real64 > HGroundARR;
		Array1D< Real64 > HAirARR;
		Array1D< Real64 > HPlenARR;
		Array1D< Real64 > LocalWindArr;
		//  REAL(r64), ALLOCATABLE, DIMENSION(:) :: IscARR
		//  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TsoARR

		// working variables
		//unused  INTEGER    :: InletNode  !
		Real64 RhoAir; // density of air
		Real64 CpAir; // specific heat of air
		Real64 holeArea; // area of perforations, includes corrugation of surface
		Real64 Tamb; // outdoor drybulb
		Real64 A; // projected area of collector, from sum of underlying surfaces
		Real64 Vholes; // mean velocity of air as it passes through collector holes
		Real64 Vsuction; // mean velocity of air as is approaches the collector
		Real64 Vplen; // mean velocity of air inside plenum
		Real64 HcPlen; // surface convection heat transfer coefficient for plenum surfaces
		Real64 D; // hole diameter
		Real64 ReD; // Reynolds number for holes
		Real64 P; // pitch, distance betweeen holes
		Real64 Por; // porosity, area fraction of collector that is open because of holes
		Real64 Mdot; // mass flow rate of suction air
		Real64 QdotSource; // energy flux for source/sink inside collector surface (for hybrid PV UTSC)
		int ThisSurf; // do loop counter
		int NumSurfs; // number of underlying HT surfaces associated with UTSC
		int Roughness; // parameters for surface roughness, defined in DataHeatBalance
		Real64 SolAbs; // solar absorptivity of collector
		Real64 AbsExt; // thermal emmittance of collector
		Real64 TempExt; // collector temperature
		int SurfPtr; // index of surface in main surface structure
		Real64 HMovInsul; // dummy for call to InitExteriorConvectionCoeff
		Real64 HExt; // dummy for call to InitExteriorConvectionCoeff
		int ConstrNum; // index of construction in main construction structure
		Real64 AbsThermSurf; // thermal emmittance of underlying wall.
		Real64 TsoK; // underlying surface temperature in Kelvin
		Real64 TscollK; // collector temperature in Kelvin  (lagged)
		Real64 AreaSum; // sum of contributing surfaces for area-weighted averages.
		Real64 Vwind; // localized, and area-weighted average for wind speed
		Real64 HrSky; // radiation coeff for sky, area-weighted average
		Real64 HrGround; // radiation coeff for ground, area-weighted average
		Real64 HrAtm; // radiation coeff for air (bulk atmosphere), area-weighted average
		Real64 Isc; // Incoming combined solar radiation, area-weighted average
		Real64 HrPlen; // radiation coeff for plenum surfaces, area-weighted average
		Real64 Tso; // temperature of underlying surface, area-weighted average
		Real64 HcWind; // convection coeff for high speed wind situations
		Real64 NuD; // nusselt number for Reynolds based on hole
		Real64 U; // overall heat exchanger coefficient
		Real64 HXeff; // effectiveness for heat exchanger
		Real64 t; // collector thickness
		Real64 ReS; // Reynolds number based on suction velocity and pitch
		Real64 ReW; // Reynolds number based on Wind and pitch
		Real64 ReB; // Reynolds number based on hole velocity and pitch
		Real64 ReH; // Reynolds number based on hole velocity and diameter
		Real64 Tscoll; // temperature of collector
		Real64 TaHX; // leaving air temperature from heat exchanger (entering plenum)
		Real64 Taplen; // Air temperature in plen and outlet node.
		Real64 SensHeatingRate; // Rate at which the system is heating outdoor air
		//  INTEGER, SAVE    :: VsucErrCount=0 !  warning message counter
		//  CHARACTER(len=MaxNameLength) :: VsucErrString !  warning message counter string
		Real64 AlessHoles; // Area for Kutscher's relation

		//Active UTSC calculation
		// first do common things for both correlations
		Real64 const surfaceArea( sum_sub( Surface, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) );
		if ( ! IsRain ) {
//			Tamb = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).OutDryBulbTemp * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / sum( Surface( UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			Tamb = sum_product_sub( Surface, &SurfaceData::OutDryBulbTemp, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage

		} else { // when raining we use wet bulb not drybulb
//			Tamb = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).OutWetBulbTemp * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / sum( Surface( UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			Tamb = sum_product_sub( Surface, &SurfaceData::OutWetBulbTemp, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
		}

		RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, Tamb, OutHumRat );

		CpAir = PsyCpAirFnWTdb( OutHumRat, Tamb );

		holeArea = UTSC( UTSCNum ).ActualArea * UTSC( UTSCNum ).Porosity;

		A = UTSC( UTSCNum ).ProjArea;

		Vholes = UTSC( UTSCNum ).InletMDot / RhoAir / holeArea;

		Vplen = UTSC( UTSCNum ).InletMDot / RhoAir / UTSC( UTSCNum ).PlenCrossArea;

		Vsuction = UTSC( UTSCNum ).InletMDot / RhoAir / A;

		if ( ( Vsuction < 0.001 ) || ( Vsuction > 0.08 ) ) { // warn that collector is not sized well
			if ( UTSC( UTSCNum ).VsucErrIndex == 0 ) {
				ShowWarningMessage( "Solar Collector:Unglazed Transpired=\"" + UTSC( UTSCNum ).Name + "\", Suction velocity is outside of range for a good design" );
				ShowContinueErrorTimeStamp( "Suction velocity =" + RoundSigDigits( Vsuction, 4 ) );
				if ( Vsuction < 0.003 ) {
					ShowContinueError( "Velocity is low -- suggest decreasing area of transpired collector" );
				}
				if ( Vsuction > 0.08 ) {
					ShowContinueError( "Velocity is high -- suggest increasing area of transpired collector" );
				}
				ShowContinueError( "Occasional suction velocity messages are not unexpected when simulating actual conditions" );
			}
			ShowRecurringWarningErrorAtEnd( "Solar Collector:Unglazed Transpired=\"" + UTSC( UTSCNum ).Name + "\", Suction velocity is outside of range", UTSC( UTSCNum ).VsucErrIndex, Vsuction, Vsuction, _, "[m/s]", "[m/s]" );
		}

		HcPlen = 5.62 + 3.92 * Vplen;

		D = UTSC( UTSCNum ).HoleDia;

		ReD = Vholes * D / nu;

		P = UTSC( UTSCNum ).Pitch;

		Por = UTSC( UTSCNum ).Porosity;

		Mdot = UTSC( UTSCNum ).InletMDot;

		QdotSource = UTSC( UTSCNum ).QdotSource; // for hybrid PV transpired collectors

		//loop through underlying surfaces and collect needed data
		// now collect average values for things associated with the underlying surface(s)
		NumSurfs = UTSC( UTSCNum ).NumSurfs;
		HSkyARR.dimension( NumSurfs, 0.0 );
		HGroundARR.dimension( NumSurfs, 0.0 );
		HAirARR.dimension( NumSurfs, 0.0 );
		LocalWindArr.dimension( NumSurfs, 0.0 );
		// ALLOCATE(IscARR(NumSurfs))
		// IscARR = 0.0
		HPlenARR.dimension( NumSurfs, 0.0 );
		//  ALLOCATE(TsoARR(NumSurfs))
		//  TsoARR = 0.0

		Roughness = UTSC( UTSCNum ).CollRoughness;
		SolAbs = UTSC( UTSCNum ).SolAbsorp;
		AbsExt = UTSC( UTSCNum ).LWEmitt;
		TempExt = UTSC( UTSCNum ).TcollLast;
		for ( ThisSurf = 1; ThisSurf <= NumSurfs; ++ThisSurf ) {
			SurfPtr = UTSC( UTSCNum ).SurfPtrs( ThisSurf );
			// Initializations for this surface
			HMovInsul = 0.0;
			HExt = 0.0;
			LocalWindArr( ThisSurf ) = Surface( SurfPtr ).WindSpeed;
			InitExteriorConvectionCoeff( SurfPtr, HMovInsul, Roughness, AbsExt, TempExt, HExt, HSkyARR( ThisSurf ), HGroundARR( ThisSurf ), HAirARR( ThisSurf ) );
			ConstrNum = Surface( SurfPtr ).Construction;
			AbsThermSurf = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermal;
			TsoK = TH( 1, 1, SurfPtr ) + KelvinConv;
			TscollK = UTSC( UTSCNum ).TcollLast + KelvinConv;
			HPlenARR( ThisSurf ) = Sigma * AbsExt * AbsThermSurf * ( pow_4( TscollK ) - pow_4( TsoK ) ) / ( TscollK - TsoK );
		}
//		AreaSum = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
		auto Area( array_sub( Surface, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) ); //Autodesk:F2C++ Copy of subscripted Area array for use below: This makes a copy so review wrt performance
		AreaSum = sum( Area );
		// now figure area-weighted averages from underlying surfaces.
//		Vwind = sum( LocalWindArr * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		Vwind = sum( LocalWindArr * Area ) / AreaSum;
		LocalWindArr.deallocate();
//		HrSky = sum( HSkyARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		HrSky = sum( HSkyARR * Area ) / AreaSum;
		HSkyARR.deallocate();
//		HrGround = sum( HGroundARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		HrGround = sum( HGroundARR * Area ) / AreaSum;
		HGroundARR.deallocate();
//		HrAtm = sum( HAirARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		HrAtm = sum( HAirARR * Area ) / AreaSum;
		HAirARR.deallocate();
//		HrPlen = sum( HPlenARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		HrPlen = sum( HPlenARR * Area ) / AreaSum;
		HPlenARR.deallocate();

//		Isc = sum( QRadSWOutIncident( UTSC( UTSCNum ).SurfPtrs ) * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		Isc = sum_product_sub( QRadSWOutIncident, Surface, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) / AreaSum; //Autodesk:F2C++ Functions handle array subscript usage
//		Tso = sum( TH( UTSC( UTSCNum ).SurfPtrs, 1, 1 ) * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced by below
		Tso = sum_product_sub( TH( 1, 1, _ ), Surface, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) / AreaSum; //Autodesk:F2C++ Functions handle array subscript usage

		if ( Vwind > 5.0 ) {
			HcWind = 5.62 + 3.9 * ( Vwind - 5.0 ); //McAdams forced convection correlation
		} else {
			HcWind = 0.0;
		}

		if ( IsRain ) HcWind = 1000.0;

		HXeff = 0.0; // init

		{ auto const SELECT_CASE_var( UTSC( UTSCNum ).Correlation );

		if ( SELECT_CASE_var == Correlation_Kutscher1994 ) { // Kutscher1994

			AlessHoles = A - holeArea;

			NuD = 2.75 * ( ( std::pow( P / D, -1.2 ) * std::pow( ReD, 0.43 ) ) + ( 0.011 * Por * ReD * std::pow( Vwind / Vsuction, 0.48 ) ) );
			U = k * NuD / D;
			HXeff = 1.0 - std::exp( -1.0 * ( ( U * AlessHoles ) / ( Mdot * CpAir ) ) );

		} else if ( SELECT_CASE_var == Correlation_VanDeckerHollandsBrunger2001 ) { // VanDeckerHollandsBrunger2001
			t = UTSC( UTSCNum ).CollectThick;
			ReS = Vsuction * P / nu;
			ReW = Vwind * P / nu;
			ReB = Vholes * P / nu;
			ReH = ( Vsuction * D ) / ( nu * Por );
			if ( ReD > 0.0 ) {
				if ( ReW > 0.0 ) {
					HXeff = ( 1.0 - std::pow( 1.0 + ReS * max( 1.733 * std::pow( ReW, -0.5 ), 0.02136 ), -1.0 ) ) * ( 1.0 - std::pow( 1.0 + 0.2273 * std::sqrt( ReB ), -1.0 ) ) * std::exp( -0.01895 * ( P / D ) - ( 20.62 / ReH ) * ( t / D ) );
				} else {
					HXeff = ( 1.0 - std::pow( 1.0 + ReS * 0.02136, -1.0 ) ) * ( 1.0 - std::pow( 1.0 + 0.2273 * std::sqrt( ReB ), -1.0 ) ) * std::exp( -0.01895 * ( P / D ) - ( 20.62 / ReH ) * ( t / D ) );
				}
			} else {
				HXeff = 0.0;
			}
		}}

		//now calculate collector temperature

		Tscoll = ( Isc * SolAbs + HrAtm * Tamb + HrSky * SkyTemp + HrGround * Tamb + HrPlen * Tso + HcWind * Tamb + ( Mdot * CpAir / A ) * Tamb - ( Mdot * CpAir / A ) * ( 1.0 - HXeff ) * Tamb + QdotSource ) / ( HrAtm + HrSky + HrGround + HrPlen + HcWind + ( Mdot * CpAir / A ) * HXeff );

		// Heat exchanger leaving temperature
		TaHX = HXeff * Tscoll + ( 1.0 - HXeff ) * Tamb;

		//now calculate plenum air temperature

		Taplen = ( Mdot * CpAir * TaHX + HcPlen * A * Tso ) / ( Mdot * CpAir + HcPlen * A );

		// calculate Sensible Heating Rate
		if ( Taplen > Tamb ) {
			SensHeatingRate = Mdot * CpAir * ( Taplen - Tamb );
		} else {
			SensHeatingRate = 0.0;
		}

		//now fill results into derived types
		UTSC( UTSCNum ).Isc = Isc;
		UTSC( UTSCNum ).HXeff = HXeff;
		UTSC( UTSCNum ).Tplen = Taplen;
		UTSC( UTSCNum ).Tcoll = Tscoll;
		UTSC( UTSCNum ).HrPlen = HrPlen;
		UTSC( UTSCNum ).HcPlen = HcPlen;
		UTSC( UTSCNum ).TairHX = TaHX;
		UTSC( UTSCNum ).InletMDot = Mdot;
		UTSC( UTSCNum ).InletTempDB = Tamb;
		UTSC( UTSCNum ).Vsuction = Vsuction;
		UTSC( UTSCNum ).PlenumVelocity = Vplen;
		UTSC( UTSCNum ).SupOutTemp = Taplen;
		UTSC( UTSCNum ).SupOutHumRat = OutHumRat; //stays the same with sensible heating
		UTSC( UTSCNum ).SupOutEnth = PsyHFnTdbW( UTSC( UTSCNum ).SupOutTemp, UTSC( UTSCNum ).SupOutHumRat );
		UTSC( UTSCNum ).SupOutMassFlow = Mdot;
		UTSC( UTSCNum ).SensHeatingRate = SensHeatingRate;
		UTSC( UTSCNum ).SensHeatingEnergy = SensHeatingRate * TimeStepSys * SecInHour;
		UTSC( UTSCNum ).PassiveACH = 0.0;
		UTSC( UTSCNum ).PassiveMdotVent = 0.0;
		UTSC( UTSCNum ).PassiveMdotWind = 0.0;
		UTSC( UTSCNum ).PassiveMdotTherm = 0.0;
		if ( Isc > 10.0 ) {
			UTSC( UTSCNum ).UTSCEfficiency = SensHeatingRate / ( Isc * A );
			if ( TaHX > Tamb ) {
				UTSC( UTSCNum ).UTSCCollEff = Mdot * CpAir * ( TaHX - Tamb ) / ( Isc * A );
			} else {
				UTSC( UTSCNum ).UTSCCollEff = 0.0;
			}
		} else {
			UTSC( UTSCNum ).UTSCEfficiency = 0.0;
			UTSC( UTSCNum ).UTSCCollEff = 0.0;
		}

	}

	void
	CalcPassiveTranspiredCollector( int const UTSCNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.T. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// model the effect of the a ventilated baffle covering the outside of a heat transfer surface.

		// METHODOLOGY EMPLOYED:
		// All the work is done in a subroutine .

		// REFERENCES:
		// Nat. Vent. equations from ASHRAE HoF 2001 Chapt. 26

		// USE STATEMENTS:

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutEnthalpy;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceData;
		using DataHVACGlobals::TimeStepSys;
		using ConvectionCoefficients::InitExteriorConvectionCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// local working variables
		Real64 AspRat; // Aspect Ratio of gap
		Real64 TmpTscoll;
		Real64 TmpTaPlen;
		Real64 RhoAir;
		Real64 holeArea;
		Real64 Tamb;
		Real64 HrPlen;
		Real64 HcPlen;
		Real64 Isc;
		Real64 MdotVent;
		Real64 VdotWind;
		Real64 VdotThermal;
		Real64 Twbamb;
		Real64 OutHumRatAmb;

		Real64 const surfaceArea( sum_sub( Surface, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) );
//		Tamb = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).OutDryBulbTemp * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / sum( Surface( UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
		Tamb = sum_product_sub( Surface, &SurfaceData::OutDryBulbTemp, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
//		Twbamb = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).OutWetBulbTemp * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / sum( Surface( UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
		Twbamb = sum_product_sub( Surface, &SurfaceData::OutWetBulbTemp, &SurfaceData::Area, UTSC( UTSCNum ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
		OutHumRatAmb = PsyWFnTdbTwbPb( Tamb, Twbamb, OutBaroPress );

		RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, Tamb, OutHumRatAmb );
		holeArea = UTSC( UTSCNum ).ActualArea * UTSC( UTSCNum ).Porosity;

		AspRat = UTSC( UTSCNum ).Height / UTSC( UTSCNum ).PlenGapThick;
		TmpTscoll = UTSC( UTSCNum ).TcollLast;
		TmpTaPlen = UTSC( UTSCNum ).TplenLast;

		// all the work is done in this routine located in GeneralRoutines.cc

		CalcPassiveExteriorBaffleGap( UTSC( UTSCNum ).SurfPtrs, holeArea, UTSC( UTSCNum ).Cv, UTSC( UTSCNum ).Cd, UTSC( UTSCNum ).HdeltaNPL, UTSC( UTSCNum ).SolAbsorp, UTSC( UTSCNum ).LWEmitt, UTSC( UTSCNum ).Tilt, AspRat, UTSC( UTSCNum ).PlenGapThick, UTSC( UTSCNum ).CollRoughness, UTSC( UTSCNum ).QdotSource, TmpTscoll, TmpTaPlen, HcPlen, HrPlen, Isc, MdotVent, VdotWind, VdotThermal );

		//now fill results into derived types
		UTSC( UTSCNum ).Isc = Isc;
		UTSC( UTSCNum ).Tplen = TmpTaPlen;
		UTSC( UTSCNum ).Tcoll = TmpTscoll;
		UTSC( UTSCNum ).HrPlen = HrPlen;
		UTSC( UTSCNum ).HcPlen = HcPlen;
		UTSC( UTSCNum ).TairHX = 0.0;
		UTSC( UTSCNum ).InletMDot = 0.0;
		UTSC( UTSCNum ).InletTempDB = Tamb;
		UTSC( UTSCNum ).Vsuction = 0.0;
		UTSC( UTSCNum ).PlenumVelocity = 0.0;
		UTSC( UTSCNum ).SupOutTemp = Tamb;
		UTSC( UTSCNum ).SupOutHumRat = OutHumRatAmb;
		UTSC( UTSCNum ).SupOutEnth = OutEnthalpy;
		UTSC( UTSCNum ).SupOutMassFlow = 0.0;
		UTSC( UTSCNum ).SensHeatingRate = 0.0;
		UTSC( UTSCNum ).SensHeatingEnergy = 0.0;
		UTSC( UTSCNum ).PassiveACH = ( MdotVent / RhoAir ) * ( 1.0 / ( UTSC( UTSCNum ).ProjArea * UTSC( UTSCNum ).PlenGapThick ) ) * SecInHour;
		UTSC( UTSCNum ).PassiveMdotVent = MdotVent;
		UTSC( UTSCNum ).PassiveMdotWind = VdotWind * RhoAir;
		UTSC( UTSCNum ).PassiveMdotTherm = VdotThermal * RhoAir;
		UTSC( UTSCNum ).UTSCEfficiency = 0.0;

	}

	void
	UpdateTranspiredCollector( int const UTSCNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.T. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataSurfaces::OSCM;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;
		int InletNode;
		int thisOSCM;
		int thisOASys;

		//update "last" values in Derived type
		UTSC( UTSCNum ).TplenLast = UTSC( UTSCNum ).Tplen;
		UTSC( UTSCNum ).TcollLast = UTSC( UTSCNum ).Tcoll;

		// Set the outlet air nodes of the UTSC

		if ( UTSC( UTSCNum ).IsOn ) { // Active
			if ( UTSC( UTSCNum ).NumOASysAttached == 1 ) {
				OutletNode = UTSC( UTSCNum ).OutletNode( 1 );
				InletNode = UTSC( UTSCNum ).InletNode( 1 );
				Node( OutletNode ).MassFlowRate = UTSC( UTSCNum ).SupOutMassFlow;
				Node( OutletNode ).Temp = UTSC( UTSCNum ).SupOutTemp;
				Node( OutletNode ).HumRat = UTSC( UTSCNum ).SupOutHumRat;
				Node( OutletNode ).Enthalpy = UTSC( UTSCNum ).SupOutEnth;
			} else if ( UTSC( UTSCNum ).NumOASysAttached > 1 ) {
				for ( thisOASys = 1; thisOASys <= UTSC( UTSCNum ).NumOASysAttached; ++thisOASys ) {
					Node( UTSC( UTSCNum ).OutletNode( thisOASys ) ).MassFlowRate = Node( UTSC( UTSCNum ).InletNode( thisOASys ) ).MassFlowRate; //system gets what it asked for at inlet
					Node( UTSC( UTSCNum ).OutletNode( thisOASys ) ).Temp = UTSC( UTSCNum ).SupOutTemp;
					Node( UTSC( UTSCNum ).OutletNode( thisOASys ) ).HumRat = UTSC( UTSCNum ).SupOutHumRat;
					Node( UTSC( UTSCNum ).OutletNode( thisOASys ) ).Enthalpy = UTSC( UTSCNum ).SupOutEnth;

				}
			}
		} else { // Passive and/or bypassed           Note Array assignments in following
//Autodesk:F2C++ Array subscript usage: Replaced by below
//			Node( UTSC( UTSCNum ).OutletNode ).MassFlowRate = Node( UTSC( UTSCNum ).InletNode ).MassFlowRate;
//			Node( UTSC( UTSCNum ).OutletNode ).Temp = Node( UTSC( UTSCNum ).InletNode ).Temp;
//			Node( UTSC( UTSCNum ).OutletNode ).HumRat = Node( UTSC( UTSCNum ).InletNode ).HumRat;
//			Node( UTSC( UTSCNum ).OutletNode ).Enthalpy = Node( UTSC( UTSCNum ).InletNode ).Enthalpy;
			auto const & OutletNode( UTSC( UTSCNum ).OutletNode );
			auto const & InletNode( UTSC( UTSCNum ).InletNode );
			assert( OutletNode.size() == InletNode.size() );
			for ( int io = OutletNode.l(), ii = InletNode.l(), eo = OutletNode.u(); io <= eo; ++io, ++ii ) {
				auto & outNode( Node( OutletNode( io ) ) );
				auto const & inNode( Node( InletNode( ii ) ) );
				outNode.MassFlowRate = inNode.MassFlowRate;
				outNode.Temp = inNode.Temp;
				outNode.HumRat = inNode.HumRat;
				outNode.Enthalpy = inNode.Enthalpy;
			}
		}

		// update the OtherSideConditionsModel coefficients.
		thisOSCM = UTSC( UTSCNum ).OSCMPtr;

		OSCM( thisOSCM ).TConv = UTSC( UTSCNum ).Tplen;
		OSCM( thisOSCM ).HConv = UTSC( UTSCNum ).HcPlen;
		OSCM( thisOSCM ).TRad = UTSC( UTSCNum ).Tcoll;
		OSCM( thisOSCM ).HRad = UTSC( UTSCNum ).HrPlen;

	}

	void
	SetUTSCQdotSource(
		int const UTSCNum,
		Real64 const QSource // source term in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Set" routine for updating sink term without exposing variables

		// METHODOLOGY EMPLOYED:
		// update derived type with new data , turn power into W/m2

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

		UTSC( UTSCNum ).QdotSource = QSource / UTSC( UTSCNum ).ProjArea;

	}

	void
	GetTranspiredCollectorIndex(
		int const SurfacePtr,
		int & UTSCIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Get" routine for establishing correct integer index from outside this module

		// METHODOLOGY EMPLOYED:
		// mine Surface derived type for correct index/number of surface
		// mine UTSC derived type that has the surface.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UTSCNum; // temporary
		int ThisSurf; // temporary
		int thisUTSC;
		bool Found;

		if ( GetInputFlag ) {
			GetTranspiredCollectorInput();
			GetInputFlag = false;
		}

		if ( SurfacePtr == 0 ) {
			ShowFatalError( "Invalid surface passed to GetTranspiredCollectorIndex, Surface name = " + Surface( SurfacePtr ).Name );
		}

		UTSCNum = 0;
		Found = false;
		for ( thisUTSC = 1; thisUTSC <= NumUTSC; ++thisUTSC ) {
			for ( ThisSurf = 1; ThisSurf <= UTSC( thisUTSC ).NumSurfs; ++ThisSurf ) {
				if ( SurfacePtr == UTSC( thisUTSC ).SurfPtrs( ThisSurf ) ) {
					Found = true;
					UTSCNum = thisUTSC;
				}
			}
		}

		if ( ! Found ) {
			ShowFatalError( "Did not find surface in UTSC description in GetTranspiredCollectorIndex, Surface name = " + Surface( SurfacePtr ).Name );
		} else {

			UTSCIndex = UTSCNum;

		}

	}

	void
	GetUTSCTsColl(
		int const UTSCNum,
		Real64 & TsColl
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Get" routine for collector surface temperature

		// METHODOLOGY EMPLOYED:
		// access derived type

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
		TsColl = UTSC( UTSCNum ).Tcoll;

	}

} // TranspiredCollector

} // EnergyPlus
