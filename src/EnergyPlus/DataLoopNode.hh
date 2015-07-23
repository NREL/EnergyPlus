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

		// Default Constructor
		MoreNodeData() :
			RelHumidity( 0.0 ),
			ReportEnthalpy( 0.0 ),
			VolFlowRateStdRho( 0.0 ),
			VolFlowRateCrntRho( 0.0 ),
			WetBulbTemp( 0.0 ),
			Density( 0.0 ),
			AirDewPointTemp( 0.0 )
		{}

		// Member Constructor
		MoreNodeData(
			Real64 const RelHumidity, // {%}
			Real64 const ReportEnthalpy, // specific enthalpy calculated at the HVAC timestep [J/kg]
			Real64 const VolFlowRateStdRho, // volume flow rate at standard density [m3/s]
			Real64 const VolFlowRateCrntRho, // volume flow rate at current density, only used for air nodes [m3/s]
			Real64 const WetBulbTemp, // wetbulb temperature [C]
			Real64 const Density, // reported density at current temperature [kg/m3]
			Real64 const AirDewPointTemp // reported system node dewpoint temperature [C]
		) :
			RelHumidity( RelHumidity ),
			ReportEnthalpy( ReportEnthalpy ),
			VolFlowRateStdRho( VolFlowRateStdRho ),
			VolFlowRateCrntRho( VolFlowRateCrntRho ),
			WetBulbTemp( WetBulbTemp ),
			Density( Density ),
			AirDewPointTemp( AirDewPointTemp )
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

		// Member Constructor
		MarkedNodeData(
			bool const IsMarked, // true if this is a marked node
			std::string const & ObjectType, // Object Type that needs it "marked"
			std::string const & ObjectName, // Object Name that needs it "marked"
			std::string const & FieldName // FieldName that needs it "marked"
		) :
			IsMarked( IsMarked ),
			ObjectType( ObjectType ),
			ObjectName( ObjectName ),
			FieldName( FieldName )
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
