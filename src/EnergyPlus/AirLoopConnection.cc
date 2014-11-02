// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <AirLoopConnection.hh>
#include <DataContaminantBalance.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>

namespace EnergyPlus {

namespace AirLoopConnection {

	// Module containing the routines dealing with the object ZoneHVAC:AirLoopConnection
	// which is the method for modeling a zone as a generic HVAC (air) component

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   October 2014
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate the connections necessary to place
	// a zone in generic locations throughout the air loop and zone equipment.

	// METHODOLOGY EMPLOYED:
	// Based on a draft NFP written by Brent Griffith (NREL), the method here basically
	// establishes connections between the zone and other equipment on various air loops.
	// The purpose of this model is to allow a zone to reside in the air loop, outside
	// air system, or somewhere in the zone equipment--essentially in a location
	// different than the standard zone location in the air loop topology.


	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	// na
	
	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na
	
	// DERIVED TYPE DEFINITIONS:
	// na
	
	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	int NumOfAirLoopConnections( 0 );
	
	// Object Data
	FArray1D< AirLoopConnectionData > AirLoopCon;

	// Functions

	void
	SimAirLoopConnection(
		std::string const & CompName, // name of the air loop connection
		int CompIndex, // number of the air loop connection
		bool const & FirstCall // true for first pass through (forward simulation)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2104
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Main routine for the air loop connection.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology (get, init, calc, update,
		// report, etc. as needed)

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using General::TrimSigDigits;
		
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		std::string RoutineName( "SimAirLoopConnection: ");

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SysNum;

		// FLOW:
		if ( ! allocated( AirLoopCon ) ) {
			GetAirLoopConnection( );
		}

		// Find the correct air loop connection component
		if ( CompIndex == 0 ) {
			for ( SysNum = 1; SysNum <= NumOfAirLoopConnections; ++SysNum ) {
				if ( SameString( CompName, AirLoopCon( SysNum ).Name ) ) {
					CompIndex = SysNum;
					break;
				}
			}
			if ( CompIndex == 0 ) {
				ShowFatalError( RoutineName + "Invalid Component Name pass = " + CompName + " was not found among the ZoneHVAC:AirLoopConnection definitions in the IDF.");
			}
		} else {
			if ( ( CompIndex > NumOfAirLoopConnections ) || ( CompIndex < 1 ) ) {
				ShowFatalError( RoutineName + "Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Number of Units=" + TrimSigDigits( NumOfAirLoopConnections ) + ", Entered Unit name=" + CompName );
			}
		}

		InitAirLoopConnection( CompIndex );

		CalcAirLoopConnection( CompIndex, FirstCall );
		
		UpdateAirLoopConnection( CompIndex );

		ReportAirLoopConnection( CompIndex );

	}

	void
	GetAirLoopConnection( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for the air loop connection component.
		
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::SameString;
		using NodeInputManager::GetOnlySingleNode;
		using DataLoopNode::ObjectIsParent;
		using DataLoopNode::NodeType_Air;
		using DataLoopNode::NodeConnectionType_Inlet;
		using DataLoopNode::NodeConnectionType_Outlet;
		using DataLoopNode::NodeID;
		using DataGlobals::NumOfZones;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		std::string const RoutineName( "GetAirLoopComponent: " ); // include trailing blank space
		std::string const ObjectName( "ZoneHVAC:AirLoopConnection" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CurrentModuleObject; // for ease in getting objects
		FArray1D_string Alphas; // Alpha items for object
		FArray1D_string cAlphaFields; // Alpha field names
		FArray1D_string cNumericFields; // Numeric field names
		bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int Rest; // the rest of the items in the derived type
		int MaxAlphas; // Maximum number of alphas for these input keywords
		int MaxNumbers; // Maximum number of numbers for these input keywords
		FArray1D< Real64 > Numbers; // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumArgs; // Unused variable that is part of a subroutine call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		FArray1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		FArray1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		int ZoneNumInletNode; // Zone number for inlet node
		int ZoneNumOutletNode; // Zone number for outlet node
		int ControlledZoneLoop; // Loop index for controlled zones
		int NodeNum; // Node number index

		// FLOW:
		// Initializations and allocations
		MaxAlphas = 0;
		MaxNumbers = 0;

		GetObjectDefMaxArgs( ObjectName, NumArgs, NumAlphas, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNumbers = max( MaxNumbers, NumNumbers );

		Alphas.allocate( MaxAlphas );
		Alphas = "";
		Numbers.allocate( MaxNumbers );
		Numbers = 0.0;
		cAlphaFields.allocate( MaxAlphas );
		cAlphaFields = "";
		cNumericFields.allocate( MaxNumbers );
		cNumericFields = "";
		lAlphaBlanks.allocate( MaxAlphas );
		lAlphaBlanks = true;
		lNumericBlanks.allocate( MaxNumbers );
		lNumericBlanks = true;

		NumOfAirLoopConnections = GetNumObjectsFound( ObjectName );

		AirLoopCon.allocate( NumOfAirLoopConnections );

		CurrentModuleObject = ObjectName;
		for ( Item = 1; Item <= NumOfAirLoopConnections; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			// General user input data
			AirLoopCon( Item ).Name = Alphas( 1 );
			AirLoopCon( Item ).AirLoopInletNodeNum = GetOnlySingleNode( Alphas( 2 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			AirLoopCon( Item ).AirLoopOutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );
			AirLoopCon( Item ).ZoneInletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			AirLoopCon( Item ).ZoneOutletNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

		}

		// Error checking section.  Need to make sure that: air loop connection names are unique, zone nodes are for the same zone, and each individual node is unique.
		for ( Item = 1; Item <= NumOfAirLoopConnections; ++Item ) {
			for ( Rest = Item + 1 ; Rest <= NumOfAirLoopConnections; ++Rest ) {
				if ( SameString( AirLoopCon( Item ).Name, AirLoopCon( Rest ).Name ) ) {
					ShowSevereError( RoutineName + "Two air loop connection objects have the same name = " + AirLoopCon( Item ).Name );
					ShowContinueError( "ZoneHVAC:AirLoopConnection objects must have unique names.");
					ErrorsFound = true;
				}
				if ( AirLoopCon( Item ).AirLoopInletNodeNum == AirLoopCon( Rest ).AirLoopInletNodeNum ) {
					ShowSevereError( RoutineName + "Two air loop connection objects have the same air inlet node = " + NodeID( AirLoopCon( Item ).AirLoopInletNodeNum ) );
					ShowContinueError( "Two ZoneHVAC:AirLoopConnection cannot have the same air loop inlet node.");
					ErrorsFound = true;
				}
				if ( AirLoopCon( Item ).AirLoopOutletNodeNum == AirLoopCon( Rest ).AirLoopOutletNodeNum ) {
					ShowSevereError( RoutineName + "Two air loop connection objects have the same air outlet node = " + NodeID( AirLoopCon( Item ).AirLoopInletNodeNum ) );
					ShowContinueError( "Two ZoneHVAC:AirLoopConnection cannot have the same air loop outlet node.");
					ErrorsFound = true;
				}
				if ( AirLoopCon( Item ).ZoneInletNodeNum == AirLoopCon( Rest ).ZoneInletNodeNum ) {
					ShowSevereError( RoutineName + "Two air loop connection objects have the same zone inlet node = " + NodeID( AirLoopCon( Item ).AirLoopInletNodeNum ) );
					ShowContinueError( "Two ZoneHVAC:AirLoopConnection cannot have the same zone loop inlet node.");
					ErrorsFound = true;
				}
				if ( AirLoopCon( Item ).ZoneOutletNodeNum == AirLoopCon( Rest ).ZoneOutletNodeNum ) {
					ShowSevereError( RoutineName + "Two air loop connection objects have the same zone outlet node = " + NodeID( AirLoopCon( Item ).AirLoopInletNodeNum ) );
					ShowContinueError( "Two ZoneHVAC:AirLoopConnection cannot have the same zone loop outlet node.");
					ErrorsFound = true;
				}
			}
			// Need to check to make sure that the nodes entered correspond with an actual zone inlet and outlet node
			// and that the inlet and outlet nodes both correspond to the same zone entered.  By now, all of the necessary
			// information should be available.  First check for the inlet node being present somewhere in the zone input.
			ZoneNumInletNode = 0;
			for ( ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfZones; ++ControlledZoneLoop ) {
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ControlledZoneLoop ).NumInletNodes; ++ NodeNum ) {
					if ( ZoneEquipConfig( ControlledZoneLoop ).InletNode( NodeNum ) == AirLoopCon( Item ).ZoneInletNodeNum ) {
						ZoneNumInletNode = ControlledZoneLoop;
						break;
					}
				}
				if ( ZoneNumInletNode != 0 ) break;
			} // end loop over controlled zones
			if ( ZoneNumInletNode == 0 ) {
				ShowSevereError( RoutineName + cAlphaFields( 1 ) + "=\"" + AirLoopCon( Item ).Name + "\"" );
				ShowContinueError( "Inlet node =\"" + NodeID( AirLoopCon( Item ).ZoneInletNodeNum ) + "\" was not found as an inlet node for any zone." );
				ShowContinueError( "The ZoneHVAC:AirLoopConnection inlet node must match the inlet node for an existing zone.");
				ErrorsFound = true;
			}
			
			//check first the return air nodes then the exhaust nodes for a match of the outlet zone node
			ZoneNumOutletNode = 0;
			for ( ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfZones; ++ControlledZoneLoop ) {
				if ( ZoneEquipConfig( ControlledZoneLoop ).ReturnAirNode == AirLoopCon( Item ).ZoneOutletNodeNum ) {
					ZoneNumOutletNode = ControlledZoneLoop;
					break;
				}
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ControlledZoneLoop ).NumExhaustNodes; ++ NodeNum ) {
					if ( ZoneEquipConfig( ControlledZoneLoop ).ExhaustNode( NodeNum ) == AirLoopCon( Item ).ZoneOutletNodeNum ) {
						ZoneNumOutletNode = ControlledZoneLoop;
						break;
					}
					if ( ZoneNumOutletNode != 0 ) break;
				}
			} // end loop over controlled zones
			if ( ZoneNumOutletNode == 0 ) {
				ShowSevereError( RoutineName + cAlphaFields( 1 ) + "=\"" + AirLoopCon( Item ).Name + "\"" );
				ShowContinueError( "Outlet node =\"" + NodeID( AirLoopCon( Item ).ZoneOutletNodeNum ) + "\" was not found as an outlet node for any zone." );
				ShowContinueError( "The ZoneHVAC:AirLoopConnection outlet node must match the return or an exhaust node for an existing zone.");
				ErrorsFound = true;
			}
			if ( ZoneNumOutletNode != ZoneNumInletNode ) {
				// inlet and outlet node must be in the same zone otherwise this is an input error
				ShowSevereError( RoutineName + cAlphaFields( 1 ) + "=\"" + AirLoopCon( Item ).Name + "\"" );
				ShowContinueError( "Inlet node =\"" + NodeID( AirLoopCon( Item ).ZoneInletNodeNum ) + "\" was found in zone " + ZoneEquipConfig( ZoneNumInletNode ).ZoneName );
				ShowContinueError( "Outlet node =\"" + NodeID( AirLoopCon( Item ).ZoneOutletNodeNum ) + "\" was found in zone " + ZoneEquipConfig( ZoneNumOutletNode ).ZoneName );
				ShowContinueError( "The ZoneHVAC:AirLoopConnection inlet and outlet nodes for this object are not in the same zone.  This is NOT allowed.");
				ErrorsFound = true;
			} else { // Zone inlet and outlet node are from the same zone--now make sure the zone was not defined with a zone multiplier
				
				if ( ( Zone( ZoneEquipConfig( ZoneNumInletNode ).ActualZoneNum ).Multiplier != 1 ) || ( Zone( ZoneEquipConfig( ZoneNumInletNode ).ActualZoneNum ).ListMultiplier != 1 ) ) {
					ShowSevereError( RoutineName + cAlphaFields( 1 ) + "=\"" + AirLoopCon( Item ).Name + "\"" );
					ShowContinueError( "The zone associated with the zone inlet and outlet nodes for this ZoneHVAC:AirLoopConnection object has a zone multipler other than 1." );
					ShowContinueError( "The zone multiplier must be 1 for any zones that are defined as part of a ZoneHVAC:AirLoopConnection object.");
					ErrorsFound = true;
				}

			}

		}
		
		//  DEALLOCATE(local arrays)

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input. Preceding conditions cause termination." );
		}

		// Set up the output variables for air loop connection components
		for ( Item = 1; Item <= NumOfAirLoopConnections; ++Item ) {
			SetupOutputVariable( "Air Loop Connection Sensible Heating Rate [W]", AirLoopCon( Item ).SenHeatRate, "System", "Average", AirLoopCon( Item ).Name );
			SetupOutputVariable( "Air Loop Connection Latent Heating Rate [W]", AirLoopCon( Item ).LatHeatRate, "System", "Average", AirLoopCon( Item ).Name );
			SetupOutputVariable( "Air Loop Connection Total Heating Rate [W]", AirLoopCon( Item ).TotHeatRate, "System", "Average", AirLoopCon( Item ).Name );
			SetupOutputVariable( "Air Loop Connection Sensible Cooling Rate [W]", AirLoopCon( Item ).SenCoolRate, "System", "Average", AirLoopCon( Item ).Name );
			SetupOutputVariable( "Air Loop Connection Latent Cooling Rate [W]", AirLoopCon( Item ).LatCoolRate, "System", "Average", AirLoopCon( Item ).Name );
			SetupOutputVariable( "Air Loop Connection Total Cooling Rate [W]", AirLoopCon( Item ).TotCoolRate, "System", "Average", AirLoopCon( Item ).Name );
			SetupOutputVariable( "Air Loop Connection Sensible Heating Energy [J]", AirLoopCon( Item ).SenHeatEnergy, "System", "Sum", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Latent Heating Energy [J]", AirLoopCon( Item ).LatHeatEnergy, "System", "Sum", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Total Heating Energy [J]", AirLoopCon( Item ).TotHeatEnergy, "System", "Sum", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Sensible Cooling Energy [J]", AirLoopCon( Item ).SenCoolEnergy, "System", "Sum", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Latent Cooling Energy [J]", AirLoopCon( Item ).LatCoolEnergy, "System", "Sum", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Total Cooling Energy [J]", AirLoopCon( Item ).TotCoolEnergy, "System", "Sum", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Air Mass Flow Rate [kg/s]", AirLoopCon( Item ).MassFlowRate, "System", "Average", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Air Inlet Temperature [C]", AirLoopCon( Item ).InletAirTemp, "System", "Average", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Air Outlet Temperature [C]", AirLoopCon( Item ).OutletAirTemp, "System", "Average", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Air Inlet Humidity Ratio [kg/kg]", AirLoopCon( Item ).InletHumRat, "System", "Average", AirLoopCon( Item ).Name);
			SetupOutputVariable( "Air Loop Connection Air Outlet Humidity Ratio [kg/kg]", AirLoopCon( Item ).OutletHumRat, "System", "Average", AirLoopCon( Item ).Name);
		}
		
	}

	void
	InitAirLoopConnection(
		int const & CompIndex // Index for the air loop connection under consideration within the derived types
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables relating to air loop connection.
		
		// METHODOLOGY EMPLOYED:
		// Simply initializes whatever needs initializing.

		// REFERENCES:
		// na

		// Using/Aliasing
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

		// FLOW:
		// nothing really to initialize for this component (left here in case something needs to be done later)

	}

	void
	CalcAirLoopConnection(
		int const & CompIndex, // Index for the air loop connection under consideration within the derived types
		bool const & FirstCall
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October	2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to "simulate"
		// the air loop connection.

		// METHODOLOGY EMPLOYED:
		// Just transfer stuff as needed.  There is a pair of air loop nodes and
		// a pair of zone nodes.  The inlet air node information has to be sent to
		// the inlet zone node.  The outlet zone node information has to be sent to
		// the outlet air loop node.  Also, the mass flow rate of air needs to balance
		// between the inlet and outlet for this stream.
		
		// REFERENCES:
		// Other EnergyPlus modules

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		
		// FLOW:

		if ( FirstCall ) {
			// Forward pass through various equipment...
			// Transfer information from the air loop inlet node to the zone inlet node
			Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ) = Node ( AirLoopCon( CompIndex ).AirLoopInletNodeNum );
			
			// Pass the flow rate through (zone is a passive element) to make sure we have a mass balance on this air stream
			Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRate = Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ).MassFlowRate;
			Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRateMaxAvail = Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ).MassFlowRateMaxAvail;
			Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRateMinAvail = Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ).MassFlowRateMinAvail;

			
			// Transfer information from the zone outlet node to the air loop outlet node
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).Temp = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).Temp;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRate = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRate;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMaxAvail = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRateMaxAvail;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMinAvail = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRateMinAvail;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).HumRat = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).HumRat;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).Enthalpy = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).Enthalpy;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).Quality = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).Quality;
			Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).Press = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).Press;
			if ( Contaminant.CO2Simulation ) {
				Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).CO2 = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).CO2;
			}
			if ( Contaminant.GenericContamSimulation ) {
				Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).GenContam = Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).GenContam;
			}
		} else {
			// Backward pass for when the supply path transfers flow rates backwards through things
			Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRate = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRate;
			Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRateMaxAvail = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMaxAvail;
			Node( AirLoopCon( CompIndex ).ZoneOutletNodeNum ).MassFlowRateMinAvail = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMinAvail;
			Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ).MassFlowRate = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRate;
			Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ).MassFlowRateMaxAvail = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMaxAvail;
			Node( AirLoopCon( CompIndex ).ZoneInletNodeNum ).MassFlowRateMinAvail = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMinAvail;
			Node( AirLoopCon( CompIndex ).AirLoopInletNodeNum ).MassFlowRate = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRate;
			Node( AirLoopCon( CompIndex ).AirLoopInletNodeNum ).MassFlowRateMaxAvail = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMaxAvail;
			Node( AirLoopCon( CompIndex ).AirLoopInletNodeNum ).MassFlowRateMinAvail = Node( AirLoopCon( CompIndex ).AirLoopOutletNodeNum ).MassFlowRateMinAvail;
		}
			
	}

	void
	UpdateAirLoopConnection(
		int const & CompIndex // Index for the air loop connection under consideration within the derived types
	)
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October	2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to "update"
		// the air loop connection.
		
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.
		
		// REFERENCES:
		// Other EnergyPlus modules
		
		// Using/Aliasing
		// na
		
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		
		// SUBROUTINE PARAMETER DEFINITIONS:
		
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		
		// DERIVED TYPE DEFINITIONS
		// na
		
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		
		// FLOW:
		// nothing really to update for this component (left here in case something needs to be done later)
		
	}
	
	void
	ReportAirLoopConnection(
		int const & CompIndex // Index for the air loop connection under consideration within the derived types
	)
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October	2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to "report"
		// the air loop connection.
		
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.
		
		// REFERENCES:
		// Other EnergyPlus modules
		
		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		std::string const RoutineName( "ReportAirLoopConnection: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNodeNum;
		int OutNodeNum;
		Real64 InletEnthalpy;
		Real64 OutletEnthalpy;
		Real64 MidpointEnthalpy;
		Real64 Sensible;
		Real64 Latent;
		Real64 Total;
		Real64 DeltaHumRat;
		Real64 DeltaT;
		Real64 MassFlowRate;

		// FLOW:
		// initialize some local variables
		InNodeNum = AirLoopCon( CompIndex ).AirLoopInletNodeNum;
		OutNodeNum  = AirLoopCon( CompIndex ).AirLoopOutletNodeNum;
		
		// zero out various energy and rate terms
		AirLoopCon( CompIndex ).SenHeatRate = 0.0;
		AirLoopCon( CompIndex ).LatHeatRate = 0.0;
		AirLoopCon( CompIndex ).TotHeatRate = 0.0;
		AirLoopCon( CompIndex ).SenCoolRate = 0.0;
		AirLoopCon( CompIndex ).LatCoolRate = 0.0;
		AirLoopCon( CompIndex ).TotCoolRate = 0.0;
		
		// set-up the temperatures, flow rates, humidity ratios
		AirLoopCon( CompIndex ).InletAirTemp = Node( InNodeNum ).Temp;
		AirLoopCon( CompIndex ).OutletAirTemp = Node( OutNodeNum ).Temp;
		AirLoopCon( CompIndex ).InletHumRat = Node( InNodeNum ).HumRat;
		AirLoopCon( CompIndex ).OutletHumRat = Node( OutNodeNum ).HumRat;
		AirLoopCon( CompIndex ).MassFlowRate = Node( InNodeNum ).MassFlowRate;
		MassFlowRate = AirLoopCon( CompIndex ).MassFlowRate;
		
		// Calculate the sensible, latent, and total energy change rate to the air as it passes through the component (zone)
		InletEnthalpy = Node( InNodeNum ).Enthalpy;
		OutletEnthalpy = Node( OutNodeNum ).Enthalpy;
		Total = ( OutletEnthalpy - InletEnthalpy) * MassFlowRate;
		DeltaHumRat = Node( OutNodeNum ).HumRat - Node( InNodeNum ).HumRat;
		DeltaT = Node( OutNodeNum ).Temp - Node( InNodeNum ).Temp;
		if ( DeltaT == 0.0 ) {
			Sensible = 0.0;
			Latent = Total;
		} else if ( DeltaHumRat == 0.0 ) {
			Latent = 0.0;
			Sensible = Total;
		} else if ( DeltaT > 0.0 ) {
			MidpointEnthalpy = PsyHFnTdbW( Node( OutNodeNum ).Temp, Node( InNodeNum ).HumRat );
			Sensible = ( MidpointEnthalpy - InletEnthalpy ) * MassFlowRate;
			Latent = ( OutletEnthalpy - MidpointEnthalpy ) * MassFlowRate;
		} else { // ( DeltaHumRat > 0.0 )
			MidpointEnthalpy = PsyHFnTdbW( Node( InNodeNum ).Temp, Node( OutNodeNum ).HumRat );
			Latent = ( MidpointEnthalpy - InletEnthalpy ) * MassFlowRate;
			Sensible = ( OutletEnthalpy - MidpointEnthalpy ) * MassFlowRate;
		}
		if ( Total >= 0.0 ) {
			AirLoopCon( CompIndex ).TotHeatRate = Total;
			AirLoopCon( CompIndex ).TotCoolRate = 0.0;
		} else {
			AirLoopCon( CompIndex ).TotHeatRate = 0.0;
			AirLoopCon( CompIndex ).TotCoolRate = abs( Total );
		}
		if ( Sensible >= 0.0 ) {
			AirLoopCon( CompIndex ).SenHeatRate = Sensible;
			AirLoopCon( CompIndex ).SenCoolRate = 0.0;
		} else {
			AirLoopCon( CompIndex ).SenHeatRate = 0.0;
			AirLoopCon( CompIndex ).SenCoolRate = abs( Sensible );
		}
		if ( Latent >= 0.0 ) {
			AirLoopCon( CompIndex ).LatHeatRate = Latent;
			AirLoopCon( CompIndex ).LatCoolRate = 0.0;
		} else {
			AirLoopCon( CompIndex ).LatHeatRate = 0.0;
			AirLoopCon( CompIndex ).LatCoolRate = abs( Latent );
		}

		// Now calculate the sum terms for energy
		AirLoopCon( CompIndex ).TotHeatEnergy = AirLoopCon( CompIndex ).TotHeatRate * TimeStepSys * SecInHour;
		AirLoopCon( CompIndex ).TotCoolEnergy = AirLoopCon( CompIndex ).TotCoolRate * TimeStepSys * SecInHour;
		AirLoopCon( CompIndex ).SenHeatEnergy = AirLoopCon( CompIndex ).SenHeatRate * TimeStepSys * SecInHour;
		AirLoopCon( CompIndex ).SenCoolEnergy = AirLoopCon( CompIndex ).SenCoolRate * TimeStepSys * SecInHour;
		AirLoopCon( CompIndex ).LatHeatEnergy = AirLoopCon( CompIndex ).LatHeatRate * TimeStepSys * SecInHour;
		AirLoopCon( CompIndex ).LatCoolEnergy = AirLoopCon( CompIndex ).LatCoolRate * TimeStepSys * SecInHour;

	}

	void
	GetAirLoopConnectionNum(
		int & ConnectNum, // index number in air loop connection derived type
		std::string const CompName // name of air loop connection passed in
	)
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2104
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		
		// PURPOSE OF THIS SUBROUTINE:
		// Main routine for the air loop connection.
		
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology (get, init, calc, update,
		// report, etc. as needed)
		
		// REFERENCES:
		// na
		
		// Using/Aliasing
		using InputProcessor::SameString;
		using General::TrimSigDigits;
		
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		
		// SUBROUTINE PARAMETER DEFINITIONS:
		std::string RoutineName( "SimAirLoopConnection: ");
		
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		
		// DERIVED TYPE DEFINITIONS
		// na
		
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SysNum;
		
		// FLOW:
		if ( ! allocated( AirLoopCon ) ) {
			GetAirLoopConnection( );
		}
		
		for ( SysNum = 1; SysNum <= NumOfAirLoopConnections; ++SysNum ) {
			if ( SameString( CompName, AirLoopCon( SysNum ).Name ) ) {
				ConnectNum = SysNum;
				break;
			}
		}
		
	}
	
	void
	GetAirLoopConnectionNodeNums(
		int const ConnectNum, // index number in air loop connection derived type
		int & InletNode, // inlet node for component
		int & OutletNode // outlet node for component
	)
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2104
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		
		// PURPOSE OF THIS SUBROUTINE:
		// Main routine for the air loop connection.
		
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology (get, init, calc, update,
		// report, etc. as needed)
		
		// REFERENCES:
		// na
		
		// Using/Aliasing
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
		
		// FLOW:
		if ( ! allocated( AirLoopCon ) ) {
			GetAirLoopConnection( );
		}
		
		if ( ( ConnectNum <= 0 ) || ( ConnectNum > NumOfAirLoopConnections ) ) {
			ShowFatalError( "Illegal ZoneHVAC:AirLoopConnection index found in GetAirLoopConnectionNodeNums");
		}
		InletNode = AirLoopCon( ConnectNum ).AirLoopInletNodeNum;
		OutletNode = AirLoopCon( ConnectNum ).AirLoopOutletNodeNum;
		if ( ( InletNode <= 0 ) || ( OutletNode <= 0 ) ) {
			ShowFatalError("Node number for either the ZoneHVAC:AirLoopConnection inlet or outlet node is zero.  This should not happen." );
		}
		
	}
	
	void
	FoundAirLoopConnection(
		int const InletNodeNum, // index number in air loop connection derived type
		bool & FoundAMatch // set to true if we find a air loop connection that has this inlet node number
	)
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2104
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		
		// PURPOSE OF THIS SUBROUTINE:
		// Main routine for the air loop connection.
		
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology (get, init, calc, update,
		// report, etc. as needed)
		
		// REFERENCES:
		// na
		
		// Using/Aliasing
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
		int Item;
		
		// FLOW:

		for ( Item = 1; Item <= NumOfAirLoopConnections; ++Item ) {
			if ( InletNodeNum == AirLoopCon( Item ).AirLoopInletNodeNum ) {
				FoundAMatch = true;
				break;
			}
		}
	}

	void
	CheckZoneOutletVsCompOutlet(
		bool & SimAgain, // set to true if the air loop needs to be resimulated
		bool & SimAgain2 // set to true if the air loop needs to be resimulated
	)
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2104
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		
		// PURPOSE OF THIS SUBROUTINE:
		// Checks to see if the zone outlet is still the same temperature as the
		// component outlet.  If not, there is a zone on the air loop and we need
		// to resimulate it because the zone has changed conditions.
		
		// METHODOLOGY EMPLOYED:
		// If no components, return without doing anything.
		// If there are connection components, see if the zone temperature has
		// changed.  If it has, resimulate the air loop.
		
		// REFERENCES:
		// na
		
		// Using/Aliasing
		using DataLoopNode::Node;
		
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		
		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxAllowedTempDiff = 0.01; // in degrees C
		
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		
		// DERIVED TYPE DEFINITIONS
		// na
		
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Item;
		Real64 ZoneOutTemp;
		Real64 CompOutTemp;
		
		// FLOW:
		
		if ( NumOfAirLoopConnections > 0 ) {
			for ( Item = 1; Item <= NumOfAirLoopConnections; ++Item ) {
				ZoneOutTemp = Node( AirLoopCon( Item ).ZoneOutletNodeNum ).Temp;
				CompOutTemp = Node( AirLoopCon( Item ).AirLoopOutletNodeNum ).Temp;
				if ( abs( ZoneOutTemp - CompOutTemp ) > MaxAllowedTempDiff ) {
					SimAgain = true;
					SimAgain2 = true;
					break;
				}
			}
		}
	}
	
	
	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // AirLoopConnection

} // EnergyPlus
