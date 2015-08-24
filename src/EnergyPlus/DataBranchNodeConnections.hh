#ifndef DataBranchNodeConnections_hh_INCLUDED
#define DataBranchNodeConnections_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataBranchNodeConnections {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

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
		std::string ParentCType; // Parent Object Type (Cannot be SPLITTER or MIXER)
		std::string ParentCName; // Parent Object Name
		std::string CType; // Component Type (Cannot be SPLITTER or MIXER)
		std::string CName; // Component Name
		std::string InletNodeName; // Inlet Node ID
		std::string OutletNodeName; // Outlet Node ID
		std::string Description; // Description of Component List Type
		bool InfoFilled; // true when all information has been filled

		// Default Constructor
		ComponentListData() :
			InfoFilled( false )
		{}

		// Member Constructor
		ComponentListData(
			std::string const & ParentCType, // Parent Object Type (Cannot be SPLITTER or MIXER)
			std::string const & ParentCName, // Parent Object Name
			std::string const & CType, // Component Type (Cannot be SPLITTER or MIXER)
			std::string const & CName, // Component Name
			std::string const & InletNodeName, // Inlet Node ID
			std::string const & OutletNodeName, // Outlet Node ID
			std::string const & Description, // Description of Component List Type
			bool const InfoFilled // true when all information has been filled
		) :
			ParentCType( ParentCType ),
			ParentCName( ParentCName ),
			CType( CType ),
			CName( CName ),
			InletNodeName( InletNodeName ),
			OutletNodeName( OutletNodeName ),
			Description( Description ),
			InfoFilled( InfoFilled )
		{}

	};

	struct NodeConnectionDef
	{
		// Members
		int NodeNumber; // Node number of this node connection
		std::string NodeName; // Node Name of this node connection
		std::string ObjectType; // Object/Component Type of this node connection
		std::string ObjectName; // Name of the Object/Component Type of this node connection
		std::string ConnectionType; // Connection Type (must be valid) for this node connection
		int FluidStream; // Fluid Stream for this node connection
		bool ObjectIsParent; // Indicator whether the object is a parent or not

		// Default Constructor
		NodeConnectionDef() :
			NodeNumber( 0 ),
			FluidStream( 0 ),
			ObjectIsParent( false )
		{}

		// Member Constructor
		NodeConnectionDef(
			int const NodeNumber, // Node number of this node connection
			std::string const & NodeName, // Node Name of this node connection
			std::string const & ObjectType, // Object/Component Type of this node connection
			std::string const & ObjectName, // Name of the Object/Component Type of this node connection
			std::string const & ConnectionType, // Connection Type (must be valid) for this node connection
			int const FluidStream, // Fluid Stream for this node connection
			bool const ObjectIsParent // Indicator whether the object is a parent or not
		) :
			NodeNumber( NodeNumber ),
			NodeName( NodeName ),
			ObjectType( ObjectType ),
			ObjectName( ObjectName ),
			ConnectionType( ConnectionType ),
			FluidStream( FluidStream ),
			ObjectIsParent( ObjectIsParent )
		{}

	};

	struct ParentListData
	{
		// Members
		std::string CType; // Component Type (Cannot be SPLITTER or MIXER)
		std::string CName; // Component Name
		std::string InletNodeName; // Inlet Node ID
		std::string OutletNodeName; // Outlet Node ID
		std::string Description; // Description of Component List Type
		bool InfoFilled; // true when all information has been filled

		// Default Constructor
		ParentListData() :
			InfoFilled( false )
		{}

		// Member Constructor
		ParentListData(
			std::string const & CType, // Component Type (Cannot be SPLITTER or MIXER)
			std::string const & CName, // Component Name
			std::string const & InletNodeName, // Inlet Node ID
			std::string const & OutletNodeName, // Outlet Node ID
			std::string const & Description, // Description of Component List Type
			bool const InfoFilled // true when all information has been filled
		) :
			CType( CType ),
			CName( CName ),
			InletNodeName( InletNodeName ),
			OutletNodeName( OutletNodeName ),
			Description( Description ),
			InfoFilled( InfoFilled )
		{}

	};

	struct EqNodeConnectionDef
	{
		// Members
		std::string NodeName; // Node Name of this node connection
		std::string ObjectType; // Object/Component Type of this node connection
		std::string ObjectName; // Name of the Object/Component Type of this node connection
		std::string InputFieldName; // Input Field Name for this connection
		std::string ConnectionType; // Connection Type (must be valid) for this node connection

		// Default Constructor
		EqNodeConnectionDef()
		{}

		// Member Constructor
		EqNodeConnectionDef(
			std::string const & NodeName, // Node Name of this node connection
			std::string const & ObjectType, // Object/Component Type of this node connection
			std::string const & ObjectName, // Name of the Object/Component Type of this node connection
			std::string const & InputFieldName, // Input Field Name for this connection
			std::string const & ConnectionType // Connection Type (must be valid) for this node connection
		) :
			NodeName( NodeName ),
			ObjectType( ObjectType ),
			ObjectName( ObjectName ),
			InputFieldName( InputFieldName ),
			ConnectionType( ConnectionType )
		{}

	};

	// Object Data
	extern Array1D< ComponentListData > CompSets;
	extern Array1D< ParentListData > ParentNodeList;
	extern Array1D< NodeConnectionDef > NodeConnections;
	extern Array1D< EqNodeConnectionDef > AirTerminalNodeConnections;

	// Clears the global data in DataBranchNodeConnections.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataBranchNodeConnections

} // EnergyPlus

#endif
