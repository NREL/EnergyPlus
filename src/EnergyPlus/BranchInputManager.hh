#ifndef BranchInputManager_hh_INCLUDED
#define BranchInputManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

namespace BranchInputManager {

	// Using/Aliasing
	using DataLoopNode::NodeType_Unknown;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern std::string const cMIXER;
	extern std::string const cSPLITTER;

	//DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumOfBranchLists; // Number of Branch Lists found in IDF
	extern int NumOfBranches; // Number of Branches found in IDF
	extern int NumOfConnectorLists; // Number of Connector Lists found in IDF
	extern int NumSplitters; // Number of Splitters found in IDF
	extern int NumMixers; // Number of Mixers found in IDF

	extern bool GetBranchInputFlag; // Flag used to retrieve Input
	extern bool GetBranchListInputFlag; // Flag used to retrieve Input
	extern bool GetSplitterInputFlag; // Flag used to retrieve Input
	extern bool GetMixerInputFlag; // Flag used to retrieve Input
	extern bool GetConnectorListInputFlag; // Flag used to retrieve Input
	extern bool InvalidBranchDefinitions;

	extern std::string CurrentModuleObject; // for ease in getting objects

	//SUBROUTINE SPECIFICATIONS FOR MODULE BranchInputManager
	//PUBLIC  TestAirPathIntegrity
	//PRIVATE TestSupplyAirPathIntegrity
	//PRIVATE TestReturnAirPathIntegrity
	//PUBLIC  MyPlantSizingIndex

	// Types

	struct ConnectorData
	{
		// Members
		std::string Name; // Name for this Connector
		int NumOfConnectors; // Number of Connectors in this group
		int NumOfSplitters; // Number of Splitters in this connector group
		int NumOfMixers; // Number of Mixers in this connector group
		Array1D_string ConnectorType; // Connector:Splitter or Connector:Mixer
		Array1D_string ConnectorName; // Name for that Connector:Splitter or Connector:Mixer
		Array1D_int ConnectorMatchNo; // Pointer to index where this Splitter or Mixer matches
		// Splitter => Mixer or Mixer => Splitter.  0 indicates no match

		// Default Constructor
		ConnectorData() :
			NumOfConnectors( 0 ),
			NumOfSplitters( 0 ),
			NumOfMixers( 0 )
		{}

		// Member Constructor
		ConnectorData(
			std::string const & Name, // Name for this Connector
			int const NumOfConnectors, // Number of Connectors in this group
			int const NumOfSplitters, // Number of Splitters in this connector group
			int const NumOfMixers, // Number of Mixers in this connector group
			Array1_string const & ConnectorType, // Connector:Splitter or Connector:Mixer
			Array1_string const & ConnectorName, // Name for that Connector:Splitter or Connector:Mixer
			Array1_int const & ConnectorMatchNo // Pointer to index where this Splitter or Mixer matches
		) :
			Name( Name ),
			NumOfConnectors( NumOfConnectors ),
			NumOfSplitters( NumOfSplitters ),
			NumOfMixers( NumOfMixers ),
			ConnectorType( ConnectorType ),
			ConnectorName( ConnectorName ),
			ConnectorMatchNo( ConnectorMatchNo )
		{}

	};

	struct BranchListData
	{
		// Members
		std::string Name; // Name of this Branch List
		int NumOfBranchNames; // Number of Branches on the Branch List
		Array1D_string BranchNames; // Names of the branches on this branch list
		std::string LoopName; // Name of Loop this Branch list belongs to
		std::string LoopType; // Loop type this branch is on

		// Default Constructor
		BranchListData() :
			NumOfBranchNames( 0 )
		{}

		// Member Constructor
		BranchListData(
			std::string const & Name, // Name of this Branch List
			int const NumOfBranchNames, // Number of Branches on the Branch List
			Array1_string const & BranchNames, // Names of the branches on this branch list
			std::string const & LoopName, // Name of Loop this Branch list belongs to
			std::string const & LoopType // Loop type this branch is on
		) :
			Name( Name ),
			NumOfBranchNames( NumOfBranchNames ),
			BranchNames( BranchNames ),
			LoopName( LoopName ),
			LoopType( LoopType )
		{}

	};

	struct ComponentData
	{
		// Members
		std::string CType; // Component Type (Cannot be SPLITTER or MIXER)
		std::string Name; // Component Name
		int CtrlType; // Active, Passive, Bypass (1,2,3)
		std::string InletNodeName; // Inlet Node ID
		int InletNode; // Inlet Node Number
		std::string OutletNodeName; // Outlet Node ID
		int OutletNode; // Outlet Node Number

		// Default Constructor
		ComponentData() :
			CtrlType( 0 ),
			InletNode( 0 ),
			OutletNode( 0 )
		{}

		// Member Constructor
		ComponentData(
			std::string const & CType, // Component Type (Cannot be SPLITTER or MIXER)
			std::string const & Name, // Component Name
			int const CtrlType, // Active, Passive, Bypass (1,2,3)
			std::string const & InletNodeName, // Inlet Node ID
			int const InletNode, // Inlet Node Number
			std::string const & OutletNodeName, // Outlet Node ID
			int const OutletNode // Outlet Node Number
		) :
			CType( CType ),
			Name( Name ),
			CtrlType( CtrlType ),
			InletNodeName( InletNodeName ),
			InletNode( InletNode ),
			OutletNodeName( OutletNodeName ),
			OutletNode( OutletNode )
		{}

	};

	struct BranchData
	{
		// Members
		std::string Name; // Name for this Branch
		std::string AssignedLoopName; // Loop Name for this branch
		Real64 MaxFlowRate; // Max Flow Rate of the Branch
		int PressureCurveType; // Integer index of pressure curve type
		int PressureCurveIndex; // Integer index of pressure curve
		int FluidType; // Fluid type (see DataLoopNode)
		int NumOfComponents; // Number of Components on this Branch
		Array1D< ComponentData > Component; // Component definitions for each component

		// Default Constructor
		BranchData() :
			MaxFlowRate( 0.0 ),
			PressureCurveType( 0 ),
			PressureCurveIndex( 0 ),
			FluidType( NodeType_Unknown ),
			NumOfComponents( 0 )
		{}

		// Member Constructor
		BranchData(
			std::string const & Name, // Name for this Branch
			std::string const & AssignedLoopName, // Loop Name for this branch
			Real64 const MaxFlowRate, // Max Flow Rate of the Branch
			int const PressureCurveType, // Integer index of pressure curve type
			int const PressureCurveIndex, // Integer index of pressure curve
			int const FluidType, // Fluid type (see DataLoopNode)
			int const NumOfComponents, // Number of Components on this Branch
			Array1< ComponentData > const & Component // Component definitions for each component
		) :
			Name( Name ),
			AssignedLoopName( AssignedLoopName ),
			MaxFlowRate( MaxFlowRate ),
			PressureCurveType( PressureCurveType ),
			PressureCurveIndex( PressureCurveIndex ),
			FluidType( FluidType ),
			NumOfComponents( NumOfComponents ),
			Component( Component )
		{}

	};

	struct SplitterData
	{
		// Members
		std::string Name; // Splitter Name
		std::string InletBranchName; // Splitter Inlet Branch Name
		int NumOutletBranches; // Number of outlets on this Splitter
		Array1D_string OutletBranchNames; // Names of the Outlet Branches

		// Default Constructor
		SplitterData() :
			NumOutletBranches( 0 )
		{}

		// Member Constructor
		SplitterData(
			std::string const & Name, // Splitter Name
			std::string const & InletBranchName, // Splitter Inlet Branch Name
			int const NumOutletBranches, // Number of outlets on this Splitter
			Array1_string const & OutletBranchNames // Names of the Outlet Branches
		) :
			Name( Name ),
			InletBranchName( InletBranchName ),
			NumOutletBranches( NumOutletBranches ),
			OutletBranchNames( OutletBranchNames )
		{}

	};

	struct MixerData
	{
		// Members
		std::string Name; // Mixer Name
		std::string OutletBranchName; // Mixer Outlet Branch Name
		int NumInletBranches; // Number of inlets for this Mixer
		Array1D_string InletBranchNames; // Names of Inlet Branches

		// Default Constructor
		MixerData() :
			NumInletBranches( 0 )
		{}

		// Member Constructor
		MixerData(
			std::string const & Name, // Mixer Name
			std::string const & OutletBranchName, // Mixer Outlet Branch Name
			int const NumInletBranches, // Number of inlets for this Mixer
			Array1_string const & InletBranchNames // Names of Inlet Branches
		) :
			Name( Name ),
			OutletBranchName( OutletBranchName ),
			NumInletBranches( NumInletBranches ),
			InletBranchNames( InletBranchNames )
		{}

	};

	// Object Data
	extern Array1D< BranchListData > BranchList; // Branch List data for each Branch List
	extern Array1D< BranchData > Branch; // Branch Data for each Branch
	extern Array1D< ConnectorData > ConnectorLists; // Connector List data for each Connector List
	extern Array1D< SplitterData > Splitters; // Splitter Data for each Splitter
	extern Array1D< MixerData > Mixers; // Mixer Data for each Mixer

	// Functions

	void
	ManageBranchInput();

	//==================================================================================
	//   Routines that "get" data from internal branch management structure
	//==================================================================================

	void
	GetBranchList(
		std::string const & LoopName, // Name of Loop Branch List is on
		std::string const & BranchListName, // Branch List Name from Input
		int & NumBranchNames, // Number of Branches for this Branch List
		Array1S_string BranchNames, // Names of Branches on this Branch List
		std::string const & LoopType // Type of Loop Branch list is on
	);

	int
	NumBranchesInBranchList( std::string const & BranchListName );

	void
	GetBranchData(
		std::string const & LoopName, // Loop Name of this Branch
		std::string const & BranchName, // Requested Branch Name
		Real64 & BranchMaxFlow, // Max Flow Rate for Branch
		int & PressCurveType, // Index of a pressure curve object
		int & PressCurveIndex, // Index of a pressure curve object
		int & NumComps, // Number of Components on Branch
		Array1S_string CompType, // Component Type for each item on Branch
		Array1S_string CompName, // Component Name for each item on Branch
		Array1S_string CompInletNodeNames, // Component Inlet Node IDs for each item on Branch
		Array1S_int CompInletNodeNums, // Component Inlet Node Numbers for each item on Branch
		Array1S_string CompOutletNodeNames, // Component Outlet Node IDs for each item on Branch
		Array1S_int CompOutletNodeNums, // Component Outlet Node Numbers for each item on Branch
		bool & ErrorsFound
	);

	int
	NumCompsInBranch( std::string const & BranchName );

	int
	GetAirBranchIndex(
		std::string const & CompType,
		std::string const & CompName
	);

	Real64
	GetBranchFlow( int const BranchNum );

	void
	GetBranchFanTypeName(
		int const BranchNum,
		std::string & FanType,
		std::string & FanName,
		bool & ErrFound
	);

	void
	CheckBranchForOASys(
		std::string const & CompType,
		std::string const & CompName,
		bool & OASysFlag,
		bool & ErrFound
	);

	void
	GetInternalBranchData(
		std::string const & LoopName, // Loop Name for Branch
		std::string const & BranchName, // Requested Branch Name
		Real64 & BranchMaxFlow, // Max Flow Rate for Branch
		int & PressCurveType, // Index of pressure curve object
		int & PressCurveIndex, // Index of pressure curve object
		int & NumComps, // Number of Components on Branch
		Array1S< ComponentData > BComponents, // Component data returned
		bool & ErrorsFound // True when Loop Name is already assigned and this not same loop
	);

	void
	GetNumSplitterMixerInConntrList(
		std::string const & LoopName, // Loop Name for this Splitter (used in error message)
		std::string const & ConnectorListName, // Requested Connector List Name
		int & NumSplitters, // Number of splitters in the loop
		int & NumMixers, // Number of mixers in the loop
		bool & ErrorsFound // if no connector list
	);

	void
	GetConnectorList(
		std::string const & ConnectorListName, // Requested Connector List
		ConnectorData & Connectoid, // Returned Connector Data
		Optional_int_const NumInList = _ // Number of the current connector in the list of connectors
	);

	void
	GetLoopMixer(
		std::string const & LoopName, // Loop Name for Mixer
		std::string const & ConnectorListName, // Requested Connector List Name
		std::string & MixerName, // Name of Mixer
		bool & IsMixer, // True when Mixer is on this connector
		std::string & OutletNodeName, // Outlet Node ID
		int & OutletNodeNum, // Outlet Node Number
		int & NumInletNodes, // Number of Inlet Nodes
		Array1S_string InletNodeNames, // Inlet Node IDs
		Array1S_int InletNodeNums, // Inlet Node Numbers
		bool & ErrorsFound,
		Optional_int_const ConnectorNumber = _, // number of the current item in connector list
		Optional_int MixerNumber = _ // Mixer number for this specific splitter
	);

	void
	GetLoopSplitter(
		std::string const & LoopName, // Loop Name for this Splitter
		std::string const & ConnectorListName, // Requested Connector List Name
		std::string & SplitterName, // Name of Splitter
		bool & IsSplitter, // True if splitter on this connector list
		std::string & InletNodeName, // Inlet Node ID
		int & InletNodeNum, // Inlet Node Number
		int & NumOutletNodes, // Number of Outlet Nodes
		Array1S_string OutletNodeNames, // Outlet Node IDs
		Array1S_int OutletNodeNums, // Outlet Node Numbers
		bool & ErrorsFound,
		Optional_int_const ConnectorNumber = _, // number of the current item in connector list
		Optional_int SplitterNumber = _ // splitter number for this specific splitter
	);

	std::string
	GetFirstBranchInletNodeName( std::string const & BranchListName ); // Branch List name to search

	std::string
	GetLastBranchOutletNodeName( std::string const & BranchListName ); // Branch List name to search

	void
	CheckSystemBranchFlow(
		std::string const & SystemType, // type of air loop equipment
		std::string const & SystemName, // name of air loop equipment
		Real64 & BranchFlow, // branch volumetric flow rate [m3/s]
		Real64 const BranchFanFlow, // branch flow rate [m3/s]
		bool & ErrFound // logical error flag
	);

	//==================================================================================
	//   Routines that get the input for the internal branch management structure
	//==================================================================================

	void
	GetBranchInput();

	void
	GetBranchListInput();

	void
	GetConnectorListInput();

	void
	GetSplitterInput();

	void
	GetMixerInput();

	void
	FindPlantLoopBranchConnection(
		std::string const & BranchListName,
		std::string & FoundPlantLoopName,
		int & FoundPlantLoopNum,
		std::string & FoundSupplyDemand,
		Real64 & FoundVolFlowRate,
		bool & MatchedPlantLoop
	);

	void
	FindCondenserLoopBranchConnection(
		std::string const & BranchListName,
		std::string & FoundCondLoopName,
		int & FoundCondLoopNum,
		std::string & FoundSupplyDemand,
		Real64 & FoundVolFlowRate,
		bool & MatchedCondLoop
	);

	void
	FindAirLoopBranchConnection(
		std::string const & BranchListName,
		std::string & FoundAirLoopName,
		int & FoundAirLoopNum,
		std::string & FoundAir,
		Real64 & FoundVolFlowRate,
		bool & MatchedAirLoop
	);

	void
	FindAirPlantCondenserLoopFromBranchList(
		std::string const & BranchListName, // Branch List Name
		std::string & LoopType, // LoopType (if found, Plant,Condenser or Air)
		std::string & LoopSupplyDemandAir, // Supply if "Supply" or Demand if "Demand" or Air if "Air"
		bool & MatchedLoop // true if found
	);

	//==================================================================================
	//   Routines that test branch integrity
	//==================================================================================

	void
	AuditBranches(
		bool const mustprint, // true if the warning should be printed.
		Optional_string_const CompType = _, // when mustprint (ScanPlantLoop)  use CompType in error message and scan
		Optional_string_const CompName = _ // when mustprint (ScanPlantLoop)  use CompName in error message and scan
	);

	void
	TestBranchIntegrity( bool & ErrFound );

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

} // BranchInputManager

} // EnergyPlus

#endif
