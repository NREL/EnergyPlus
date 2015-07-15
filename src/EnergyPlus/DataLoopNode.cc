// EnergyPlus Headers
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataLoopNode {

	// MODULE INFORMATION:
	//       AUTHOR         Development Team
	//       DATE WRITTEN   1996...
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This details the data structure for "nodes" -- the basis of the HVAC-Plant
	// structure for EnergyPlus.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// Valid Fluid Types for Nodes
	int const NodeType_Unknown( 0 ); // 'blank'
	int const NodeType_Air( 1 ); // 'Air'
	int const NodeType_Water( 2 ); // 'Water'
	int const NodeType_Steam( 3 ); // 'Steam'
	int const NodeType_Electric( 4 ); // 'Electric'
	Array1D_string const ValidNodeFluidTypes( {0,4}, { "blank", "Air", "Water", "Steam", "Electric" } );
	int const NumValidNodeFluidTypes( 4 );

	// Valid Connection Types for Nodes
	Array1D_string const ValidConnectionTypes( 15, { "Inlet", "Outlet", "Internal", "ZoneNode", "Sensor", "Actuator", "OutdoorAir", "ReliefAir", "ZoneInlet", "ZoneReturn", "ZoneExhaust", "Setpoint", "Electric", "OutsideAirReference", "InducedAir" } );

	int const NumValidConnectionTypes( 15 );

	int const NodeConnectionType_Inlet( 1 );
	int const NodeConnectionType_Outlet( 2 );
	int const NodeConnectionType_Internal( 3 );
	int const NodeConnectionType_ZoneNode( 4 );
	int const NodeConnectionType_Sensor( 5 );
	int const NodeConnectionType_Actuator( 6 );
	int const NodeConnectionType_OutsideAir( 7 );
	int const NodeConnectionType_ReliefAir( 8 );
	int const NodeConnectionType_ZoneInlet( 9 );
	int const NodeConnectionType_ZoneReturn( 10 );
	int const NodeConnectionType_ZoneExhaust( 11 );
	int const NodeConnectionType_SetPoint( 12 );
	int const NodeConnectionType_Electric( 13 );
	int const NodeConnectionType_OutsideAirReference( 14 );
	int const NodeConnectionType_InducedAir( 15 );

	// Valid IsParent Types for Node Connections
	bool const ObjectIsParent( true );
	bool const ObjectIsNotParent( false );
	bool const IncrementFluidStreamYes( true );
	bool const IncrementFluidStreamNo( false );
	Real64 const SensedNodeFlagValue( -999.0 );
	Real64 const SensedLoadFlagValue( -999.0 );

	// DERIVED TYPE DEFINITIONS:

	//MODULE VARIABLE DECLARATIONS:
	int NumOfNodes( 0 );
	int NumofSplitters( 0 );
	int NumofMixers( 0 );

	// You will be tempted to put the following into the Node Derived type as
	// the "Name" for the Node.  Don't do it!!!  Several areas of the code have
	// the following assignments:  Node(somenodenumber)=Node(someothernodenumber) to
	// set/update Node conditions.  If the Node derived type would include the name
	// then the name would get changed and bad things would result...
	Array1D_string NodeID;

	// Object Data
	Array1D< NodeData > Node; // dim to num nodes in SimHVAC
	NodeData DefaultNodeValues( 0, 0, 0.0, 0.0, 0.0, SensedNodeFlagValue, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SensedNodeFlagValue, SensedNodeFlagValue, SensedNodeFlagValue, SensedNodeFlagValue, SensedNodeFlagValue, -1.0, 0.0, false, 0.0, 0.0, false, 0.0, 0.0, 0.0, 0.0, 0.0, false ); //Autodesk:Note If intent is default construction drop initializer to elim bug exposure | FluidType | FluidIndex | Temp {C} | TempMin {C} | TempMax {C} | TempSetPoint {C} | TempLastTimeStep {C} | MassFlowRateRequest {kg/s} | MassFlowRate {kg/s} | MassFlowRateMin {kg/s} | MassFlowRateMax {kg/s} //Autodesk:Note SensedNodeFlagValue is default initializer | MassFlowRateMinAvail {kg/s} | MassFlowRateMaxAvail {kg/s} | MassFlowRateSetPoint {kg/s} | Quality {0.0-1.0 vapor fraction/percent} | Press {Pa}   REAL(r64)     :: | Enthalpy {J/kg} | EnthalpyLastTimeStep {J/kg} | HumRat {} | HumRatMin {} | HumRatMax {} | HumRatSetPoint {} | TempSetPointHi {C} | TempSetPointLo {C} | Height {m} | OutAirDryBulb {C} | EMSOverrideOutAirDryBulb | EMSValueForOutAirDryBulb {C} | OutAirWetBulb {C} | EMSOverrideOutAirWetBulb | EMSValueForOutAirWetBulb {C} | CO2 {ppm} | CO2 setpoint {ppm} | Generic contaminant {ppm} | Generic contaminant setpoint {ppm} | Set to true when node has SPM which follows wetbulb
	Array1D< MoreNodeData > MoreNodeInfo;
	Array1D< MarkedNodeData > MarkedNode;

	// Clears the global data in DataLoopNode.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumOfNodes = 0;
		NumofSplitters = 0;
		NumofMixers = 0;
		NodeID.deallocate();
		Node.deallocate();
		DefaultNodeValues = NodeData( 0, 0, 0.0, 0.0, 0.0, SensedNodeFlagValue, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SensedNodeFlagValue, SensedNodeFlagValue, SensedNodeFlagValue, SensedNodeFlagValue, SensedNodeFlagValue, -1.0, 0.0, false, 0.0, 0.0, false, 0.0, 0.0, 0.0, 0.0, 0.0, false );
		MoreNodeInfo.deallocate();
		MarkedNode.deallocate();
	}

	//     NOTICE
	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.
	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.
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

} // DataLoopNode

} // EnergyPlus
