#ifndef DataBranchNodeConnections_hh_INCLUDED
#define DataBranchNodeConnections_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataBranchNodeConnections {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern Fstring const Blank;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumCompSets; // Number of Component Sets found in branches
	extern int NumNodeConnectionErrors; // Count of node connection errors

	extern int NumOfNodeConnections;
	extern int MaxNumOfNodeConnections;
	extern int NodeConnectionAlloc;
	extern int NumOfActualParents;
	extern int NumOfAirTerminalNodes;
	extern int MaxNumOfAirTerminalNodes;
	extern int EqNodeConnectionAlloc;

	// Types

	struct ComponentListData
	{
		// Members
		Fstring ParentCType; // Parent Object Type (Cannot be SPLITTER or MIXER)
		Fstring ParentCName; // Parent Object Name
		Fstring CType; // Component Type (Cannot be SPLITTER or MIXER)
		Fstring CName; // Component Name
		Fstring InletNodeName; // Inlet Node ID
		Fstring OutletNodeName; // Outlet Node ID
		Fstring Description; // Description of Component List Type
		bool InfoFilled; // true when all information has been filled

		// Default Constructor
		ComponentListData() :
			ParentCType( MaxNameLength, Blank ),
			ParentCName( MaxNameLength, Blank ),
			CType( MaxNameLength, Blank ),
			CName( MaxNameLength, Blank ),
			InletNodeName( MaxNameLength, Blank ),
			OutletNodeName( MaxNameLength, Blank ),
			Description( MaxNameLength, Blank ),
			InfoFilled( false )
		{}

		// Member Constructor
		ComponentListData(
			Fstring const & ParentCType, // Parent Object Type (Cannot be SPLITTER or MIXER)
			Fstring const & ParentCName, // Parent Object Name
			Fstring const & CType, // Component Type (Cannot be SPLITTER or MIXER)
			Fstring const & CName, // Component Name
			Fstring const & InletNodeName, // Inlet Node ID
			Fstring const & OutletNodeName, // Outlet Node ID
			Fstring const & Description, // Description of Component List Type
			bool const InfoFilled // true when all information has been filled
		) :
			ParentCType( MaxNameLength, ParentCType ),
			ParentCName( MaxNameLength, ParentCName ),
			CType( MaxNameLength, CType ),
			CName( MaxNameLength, CName ),
			InletNodeName( MaxNameLength, InletNodeName ),
			OutletNodeName( MaxNameLength, OutletNodeName ),
			Description( MaxNameLength, Description ),
			InfoFilled( InfoFilled )
		{}

	};

	struct NodeConnectionDef
	{
		// Members
		int NodeNumber; // Node number of this node connection
		Fstring NodeName; // Node Name of this node connection
		Fstring ObjectType; // Object/Component Type of this node connection
		Fstring ObjectName; // Name of the Object/Component Type of this node connection
		Fstring ConnectionType; // Connection Type (must be valid) for this node connection
		int FluidStream; // Fluid Stream for this node connection
		bool ObjectIsParent; // Indicator whether the object is a parent or not

		// Default Constructor
		NodeConnectionDef() :
			NodeNumber( 0 ),
			NodeName( MaxNameLength, Blank ),
			ObjectType( MaxNameLength, Blank ),
			ObjectName( MaxNameLength, Blank ),
			ConnectionType( 19, Blank ),
			FluidStream( 0 ),
			ObjectIsParent( false )
		{}

		// Member Constructor
		NodeConnectionDef(
			int const NodeNumber, // Node number of this node connection
			Fstring const & NodeName, // Node Name of this node connection
			Fstring const & ObjectType, // Object/Component Type of this node connection
			Fstring const & ObjectName, // Name of the Object/Component Type of this node connection
			Fstring const & ConnectionType, // Connection Type (must be valid) for this node connection
			int const FluidStream, // Fluid Stream for this node connection
			bool const ObjectIsParent // Indicator whether the object is a parent or not
		) :
			NodeNumber( NodeNumber ),
			NodeName( MaxNameLength, NodeName ),
			ObjectType( MaxNameLength, ObjectType ),
			ObjectName( MaxNameLength, ObjectName ),
			ConnectionType( 19, ConnectionType ),
			FluidStream( FluidStream ),
			ObjectIsParent( ObjectIsParent )
		{}

	};

	struct ParentListData
	{
		// Members
		Fstring CType; // Component Type (Cannot be SPLITTER or MIXER)
		Fstring CName; // Component Name
		Fstring InletNodeName; // Inlet Node ID
		Fstring OutletNodeName; // Outlet Node ID
		Fstring Description; // Description of Component List Type
		bool InfoFilled; // true when all information has been filled

		// Default Constructor
		ParentListData() :
			CType( MaxNameLength, Blank ),
			CName( MaxNameLength, Blank ),
			InletNodeName( MaxNameLength, Blank ),
			OutletNodeName( MaxNameLength, Blank ),
			Description( MaxNameLength, Blank ),
			InfoFilled( false )
		{}

		// Member Constructor
		ParentListData(
			Fstring const & CType, // Component Type (Cannot be SPLITTER or MIXER)
			Fstring const & CName, // Component Name
			Fstring const & InletNodeName, // Inlet Node ID
			Fstring const & OutletNodeName, // Outlet Node ID
			Fstring const & Description, // Description of Component List Type
			bool const InfoFilled // true when all information has been filled
		) :
			CType( MaxNameLength, CType ),
			CName( MaxNameLength, CName ),
			InletNodeName( MaxNameLength, InletNodeName ),
			OutletNodeName( MaxNameLength, OutletNodeName ),
			Description( MaxNameLength, Description ),
			InfoFilled( InfoFilled )
		{}

	};

	struct EqNodeConnectionDef
	{
		// Members
		Fstring NodeName; // Node Name of this node connection
		Fstring ObjectType; // Object/Component Type of this node connection
		Fstring ObjectName; // Name of the Object/Component Type of this node connection
		Fstring InputFieldName; // Input Field Name for this connection
		Fstring ConnectionType; // Connection Type (must be valid) for this node connection

		// Default Constructor
		EqNodeConnectionDef() :
			NodeName( MaxNameLength, Blank ),
			ObjectType( MaxNameLength, Blank ),
			ObjectName( MaxNameLength, Blank ),
			InputFieldName( MaxNameLength, Blank ),
			ConnectionType( 19, Blank )
		{}

		// Member Constructor
		EqNodeConnectionDef(
			Fstring const & NodeName, // Node Name of this node connection
			Fstring const & ObjectType, // Object/Component Type of this node connection
			Fstring const & ObjectName, // Name of the Object/Component Type of this node connection
			Fstring const & InputFieldName, // Input Field Name for this connection
			Fstring const & ConnectionType // Connection Type (must be valid) for this node connection
		) :
			NodeName( MaxNameLength, NodeName ),
			ObjectType( MaxNameLength, ObjectType ),
			ObjectName( MaxNameLength, ObjectName ),
			InputFieldName( MaxNameLength, InputFieldName ),
			ConnectionType( 19, ConnectionType )
		{}

	};

	// Object Data
	extern FArray1D< ComponentListData > CompSets;
	extern FArray1D< ParentListData > ParentNodeList;
	extern FArray1D< NodeConnectionDef > NodeConnections;
	extern FArray1D< NodeConnectionDef > tmpNodeConnections;
	extern FArray1D< EqNodeConnectionDef > tmpEqNodeConnections;
	extern FArray1D< EqNodeConnectionDef > AirTerminalNodeConnections;

} // DataBranchNodeConnections

} // EnergyPlus

#endif
