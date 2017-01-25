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

#ifndef DataLoopNode_hh_INCLUDED
#define DataLoopNode_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataLoopNode {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// Valid Fluid Types for Nodes
	extern int const NodeType_Unknown; // 'blank'
	extern int const NodeType_Air; // 'Air'
	extern int const NodeType_Water; // 'Water'
	extern int const NodeType_Steam; // 'Steam'
	extern int const NodeType_Electric; // 'Electric'
	extern Array1D_string const ValidNodeFluidTypes;
	extern int const NumValidNodeFluidTypes;

	// Valid Connection Types for Nodes
	extern Array1D_string const ValidConnectionTypes;

	extern int const NumValidConnectionTypes;

	extern int const NodeConnectionType_Inlet;
	extern int const NodeConnectionType_Outlet;
	extern int const NodeConnectionType_Internal;
	extern int const NodeConnectionType_ZoneNode;
	extern int const NodeConnectionType_Sensor;
	extern int const NodeConnectionType_Actuator;
	extern int const NodeConnectionType_OutsideAir;
	extern int const NodeConnectionType_ReliefAir;
	extern int const NodeConnectionType_ZoneInlet;
	extern int const NodeConnectionType_ZoneReturn;
	extern int const NodeConnectionType_ZoneExhaust;
	extern int const NodeConnectionType_SetPoint;
	extern int const NodeConnectionType_Electric;
	extern int const NodeConnectionType_OutsideAirReference;
	extern int const NodeConnectionType_InducedAir;

	// Valid IsParent Types for Node Connections
	extern bool const ObjectIsParent;
	extern bool const ObjectIsNotParent;
	extern bool const IncrementFluidStreamYes;
	extern bool const IncrementFluidStreamNo;
	extern Real64 const SensedNodeFlagValue;
	extern Real64 const SensedLoadFlagValue;

	// DERIVED TYPE DEFINITIONS:

	//MODULE VARIABLE DECLARATIONS:
	extern int NumOfNodes;
	extern int NumofSplitters;
	extern int NumofMixers;

	// You will be tempted to put the following into the Node Derived type as
	// the "Name" for the Node.  Don't do it!!!  Several areas of the code have
	// the following assignments:  Node(somenodenumber)=Node(someothernodenumber) to
	// set/update Node conditions.  If the Node derived type would include the name
	// then the name would get changed and bad things would result...
	extern Array1D_string NodeID;

	// Types

	struct NodeData
	{
		// Members
		int FluidType; // must be one of the valid parameters
		int FluidIndex; // For Fluid Properties
		Real64 Temp; // {C}
		Real64 TempMin; // {C}
		Real64 TempMax; // {C}
		Real64 TempSetPoint; // {C}
		Real64 TempLastTimestep; // [C}   DSU
		Real64 MassFlowRateRequest; // {kg/s}  DSU
		Real64 MassFlowRate; // {kg/s}
		Real64 MassFlowRateMin; // {kg/s}
		Real64 MassFlowRateMax; // {kg/s}
		Real64 MassFlowRateMinAvail; // {kg/s}
		Real64 MassFlowRateMaxAvail; // {kg/s}
		Real64 MassFlowRateSetPoint; // {kg/s}
		Real64 Quality; // {0.0-1.0 vapor fraction/percent}
		Real64 Press; // {Pa}
		Real64 Enthalpy; // {J/kg}
		Real64 EnthalpyLastTimestep; // {J/kg}  DSU for steam?
		Real64 HumRat; // {}
		Real64 HumRatMin; // {}
		Real64 HumRatMax; // {}
		Real64 HumRatSetPoint; // {}
		Real64 TempSetPointHi; // {C}
		Real64 TempSetPointLo; // {C}
		Real64 Height; // {m}
		//  Following are for Outdoor Air Nodes "read only"
		Real64 OutAirDryBulb; // {C}
		bool EMSOverrideOutAirDryBulb; // if true, the EMS is calling to override outdoor air node drybulb setting
		Real64 EMSValueForOutAirDryBulb; // value EMS is directing to use for outdoor air node's drybulb {C}
		Real64 OutAirWetBulb; // {C}
		bool EMSOverrideOutAirWetBulb; // if true, the EMS is calling to override outdoor air node wetbulb setting
		Real64 EMSValueForOutAirWetBulb; // value EMS is directing to use for outdoor air node's wetbulb {C}
		// Contaminant
		Real64 CO2; // {ppm}
		Real64 CO2SetPoint; // {ppm}
		Real64 GenContam; // {ppm}
		Real64 GenContamSetPoint; // {ppm}
		bool SPMNodeWetBulbRepReq; // Set to true when node has SPM which follows wetbulb

		// Default Constructor
		NodeData() :
			FluidType( 0 ),
			FluidIndex( 0 ),
			Temp( 0.0 ),
			TempMin( 0.0 ),
			TempMax( 0.0 ),
			TempSetPoint( SensedNodeFlagValue ),
			TempLastTimestep( 0.0 ),
			MassFlowRateRequest( 0.0 ),
			MassFlowRate( 0.0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( SensedNodeFlagValue ),
			MassFlowRateMinAvail( 0.0 ),
			MassFlowRateMaxAvail( 0.0 ),
			MassFlowRateSetPoint( 0.0 ),
			Quality( 0.0 ),
			Press( 0.0 ),
			Enthalpy( 0.0 ),
			EnthalpyLastTimestep( 0.0 ),
			HumRat( 0.0 ),
			HumRatMin( SensedNodeFlagValue ),
			HumRatMax( SensedNodeFlagValue ),
			HumRatSetPoint( SensedNodeFlagValue ),
			TempSetPointHi( SensedNodeFlagValue ),
			TempSetPointLo( SensedNodeFlagValue ),
			Height( -1.0 ),
			OutAirDryBulb( 0.0 ),
			EMSOverrideOutAirDryBulb( false ),
			EMSValueForOutAirDryBulb( 0.0 ),
			OutAirWetBulb( 0.0 ),
			EMSOverrideOutAirWetBulb( false ),
			EMSValueForOutAirWetBulb( 0.0 ),
			CO2( 0.0 ),
			CO2SetPoint( 0.0 ),
			GenContam( 0.0 ),
			GenContamSetPoint( 0.0 ),
			SPMNodeWetBulbRepReq( false )
		{}

		// Member Constructor
		NodeData(
			int const FluidType, // must be one of the valid parameters
			int const FluidIndex, // For Fluid Properties
			Real64 const Temp, // {C}
			Real64 const TempMin, // {C}
			Real64 const TempMax, // {C}
			Real64 const TempSetPoint, // {C}
			Real64 const TempLastTimestep, // [C}   DSU
			Real64 const MassFlowRateRequest, // {kg/s}  DSU
			Real64 const MassFlowRate, // {kg/s}
			Real64 const MassFlowRateMin, // {kg/s}
			Real64 const MassFlowRateMax, // {kg/s}
			Real64 const MassFlowRateMinAvail, // {kg/s}
			Real64 const MassFlowRateMaxAvail, // {kg/s}
			Real64 const MassFlowRateSetPoint, // {kg/s}
			Real64 const Quality, // {0.0-1.0 vapor fraction/percent}
			Real64 const Press, // {Pa}
			Real64 const Enthalpy, // {J/kg}
			Real64 const EnthalpyLastTimestep, // {J/kg}  DSU for steam?
			Real64 const HumRat, // {}
			Real64 const HumRatMin, // {}
			Real64 const HumRatMax, // {}
			Real64 const HumRatSetPoint, // {}
			Real64 const TempSetPointHi, // {C}
			Real64 const TempSetPointLo, // {C}
			Real64 const Height, // {m}
			Real64 const OutAirDryBulb, // {C}
			bool const EMSOverrideOutAirDryBulb, // if true, the EMS is calling to override outdoor air node drybulb setting
			Real64 const EMSValueForOutAirDryBulb, // value EMS is directing to use for outdoor air node's drybulb {C}
			Real64 const OutAirWetBulb, // {C}
			bool const EMSOverrideOutAirWetBulb, // if true, the EMS is calling to override outdoor air node wetbulb setting
			Real64 const EMSValueForOutAirWetBulb, // value EMS is directing to use for outdoor air node's wetbulb {C}
			Real64 const CO2, // {ppm}
			Real64 const CO2SetPoint, // {ppm}
			Real64 const GenContam, // {ppm}
			Real64 const GenContamSetPoint, // {ppm}
			bool const SPMNodeWetBulbRepReq // Set to true when node has SPM which follows wetbulb
		) :
			FluidType( FluidType ),
			FluidIndex( FluidIndex ),
			Temp( Temp ),
			TempMin( TempMin ),
			TempMax( TempMax ),
			TempSetPoint( TempSetPoint ),
			TempLastTimestep( TempLastTimestep ),
			MassFlowRateRequest( MassFlowRateRequest ),
			MassFlowRate( MassFlowRate ),
			MassFlowRateMin( MassFlowRateMin ),
			MassFlowRateMax( MassFlowRateMax ),
			MassFlowRateMinAvail( MassFlowRateMinAvail ),
			MassFlowRateMaxAvail( MassFlowRateMaxAvail ),
			MassFlowRateSetPoint( MassFlowRateSetPoint ),
			Quality( Quality ),
			Press( Press ),
			Enthalpy( Enthalpy ),
			EnthalpyLastTimestep( EnthalpyLastTimestep ),
			HumRat( HumRat ),
			HumRatMin( HumRatMin ),
			HumRatMax( HumRatMax ),
			HumRatSetPoint( HumRatSetPoint ),
			TempSetPointHi( TempSetPointHi ),
			TempSetPointLo( TempSetPointLo ),
			Height( Height ),
			OutAirDryBulb( OutAirDryBulb ),
			EMSOverrideOutAirDryBulb( EMSOverrideOutAirDryBulb ),
			EMSValueForOutAirDryBulb( EMSValueForOutAirDryBulb ),
			OutAirWetBulb( OutAirWetBulb ),
			EMSOverrideOutAirWetBulb( EMSOverrideOutAirWetBulb ),
			EMSValueForOutAirWetBulb( EMSValueForOutAirWetBulb ),
			CO2( CO2 ),
			CO2SetPoint( CO2SetPoint ),
			GenContam( GenContam ),
			GenContamSetPoint( GenContamSetPoint ),
			SPMNodeWetBulbRepReq( SPMNodeWetBulbRepReq )
		{}

	};

	struct MoreNodeData
	{
		// Members
		Real64 RelHumidity; // {%}
		Real64 ReportEnthalpy; // specific enthalpy calculated at the HVAC timestep [J/kg]
		Real64 VolFlowRateStdRho; // volume flow rate at standard density [m3/s]
		Real64 VolFlowRateCrntRho; // volume flow rate at current density, only used for air nodes [m3/s]
		Real64 WetBulbTemp; // wetbulb temperature [C]
		Real64 Density; // reported density at current temperature [kg/m3]
		Real64 AirDewPointTemp; // reported system node dewpoint temperature [C]
		Real64 SpecificHeat; // reported node specific heat [J/kg-C]

		// Default Constructor
		MoreNodeData() :
			RelHumidity( 0.0 ),
			ReportEnthalpy( 0.0 ),
			VolFlowRateStdRho( 0.0 ),
			VolFlowRateCrntRho( 0.0 ),
			WetBulbTemp( 0.0 ),
			Density( 0.0 ),
			AirDewPointTemp( 0.0 ),
			SpecificHeat( 0.0 )
		{}

	};

	struct MarkedNodeData
	{
		// Members
		bool IsMarked; // true if this is a marked node
		std::string ObjectType; // Object Type that needs it "marked"
		std::string ObjectName; // Object Name that needs it "marked"
		std::string FieldName; // FieldName that needs it "marked"

		// Default Constructor
		MarkedNodeData() :
			IsMarked( false )
		{}

	};

	// Object Data
	extern Array1D< NodeData > Node; // dim to num nodes in SimHVAC
	extern NodeData DefaultNodeValues;
	extern Array1D< MoreNodeData > MoreNodeInfo;
	extern Array1D< MarkedNodeData > MarkedNode;

	// Clears the global data in DataLoopNode.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataLoopNode

} // EnergyPlus

#endif
