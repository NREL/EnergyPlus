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

} // DataLoopNode

} // EnergyPlus
