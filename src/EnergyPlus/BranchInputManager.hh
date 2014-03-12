#ifndef BranchInputManager_hh_INCLUDED
#define BranchInputManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

namespace BranchInputManager {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;
	using DataLoopNode::NodeType_Unknown;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Fstring const cMIXER;
	extern Fstring const cSPLITTER;
	extern Fstring const Blank;

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

	extern Fstring CurrentModuleObject; // for ease in getting objects

	//SUBROUTINE SPECIFICATIONS FOR MODULE BranchInputManager
	//PUBLIC  TestAirPathIntegrity
	//PRIVATE TestSupplyAirPathIntegrity
	//PRIVATE TestReturnAirPathIntegrity
	//PUBLIC  MyPlantSizingIndex

	// Types

	struct ConnectorData
	{
		// Members
		Fstring Name; // Name for this Connector
		int NumOfConnectors; // Number of Connectors in this group
		int NumOfSplitters; // Number of Splitters in this connector group
		int NumOfMixers; // Number of Mixers in this connector group
		FArray1D_Fstring ConnectorType; // Connector:Splitter or Connector:Mixer
		FArray1D_Fstring ConnectorName; // Name for that Connector:Splitter or Connector:Mixer
		FArray1D_int ConnectorMatchNo; // Pointer to index where this Splitter or Mixer matches
		// Splitter => Mixer or Mixer => Splitter.  0 indicates no match

		// Default Constructor
		ConnectorData() :
			Name( MaxNameLength, Blank ),
			NumOfConnectors( 0 ),
			NumOfSplitters( 0 ),
			NumOfMixers( 0 ),
			ConnectorType( sFstring( 32 ) ),
			ConnectorName( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		ConnectorData(
			Fstring const & Name, // Name for this Connector
			int const NumOfConnectors, // Number of Connectors in this group
			int const NumOfSplitters, // Number of Splitters in this connector group
			int const NumOfMixers, // Number of Mixers in this connector group
			FArray1_Fstring const & ConnectorType, // Connector:Splitter or Connector:Mixer
			FArray1_Fstring const & ConnectorName, // Name for that Connector:Splitter or Connector:Mixer
			FArray1_int const & ConnectorMatchNo // Pointer to index where this Splitter or Mixer matches
		) :
			Name( MaxNameLength, Name ),
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
		Fstring Name; // Name of this Branch List
		int NumOfBranchNames; // Number of Branches on the Branch List
		FArray1D_Fstring BranchNames; // Names of the branches on this branch list
		Fstring LoopName; // Name of Loop this Branch list belongs to
		Fstring LoopType; // Loop type this branch is on

		// Default Constructor
		BranchListData() :
			Name( MaxNameLength, Blank ),
			NumOfBranchNames( 0 ),
			BranchNames( sFstring( MaxNameLength ) ),
			LoopName( MaxNameLength, Blank ),
			LoopType( 20, Blank )
		{}

		// Member Constructor
		BranchListData(
			Fstring const & Name, // Name of this Branch List
			int const NumOfBranchNames, // Number of Branches on the Branch List
			FArray1_Fstring const & BranchNames, // Names of the branches on this branch list
			Fstring const & LoopName, // Name of Loop this Branch list belongs to
			Fstring const & LoopType // Loop type this branch is on
		) :
			Name( MaxNameLength, Name ),
			NumOfBranchNames( NumOfBranchNames ),
			BranchNames( BranchNames ),
			LoopName( MaxNameLength, LoopName ),
			LoopType( 20, LoopType )
		{}

	};

	struct ComponentData
	{
		// Members
		Fstring CType; // Component Type (Cannot be SPLITTER or MIXER)
		Fstring Name; // Component Name
		int CtrlType; // Active, Passive, Bypass (1,2,3)
		Fstring InletNodeName; // Inlet Node ID
		int InletNode; // Inlet Node Number
		Fstring OutletNodeName; // Outlet Node ID
		int OutletNode; // Outlet Node Number

		// Default Constructor
		ComponentData() :
			CType( MaxNameLength, Blank ),
			Name( MaxNameLength, Blank ),
			CtrlType( 0 ),
			InletNodeName( MaxNameLength, Blank ),
			InletNode( 0 ),
			OutletNodeName( MaxNameLength, Blank ),
			OutletNode( 0 )
		{}

		// Member Constructor
		ComponentData(
			Fstring const & CType, // Component Type (Cannot be SPLITTER or MIXER)
			Fstring const & Name, // Component Name
			int const CtrlType, // Active, Passive, Bypass (1,2,3)
			Fstring const & InletNodeName, // Inlet Node ID
			int const InletNode, // Inlet Node Number
			Fstring const & OutletNodeName, // Outlet Node ID
			int const OutletNode // Outlet Node Number
		) :
			CType( MaxNameLength, CType ),
			Name( MaxNameLength, Name ),
			CtrlType( CtrlType ),
			InletNodeName( MaxNameLength, InletNodeName ),
			InletNode( InletNode ),
			OutletNodeName( MaxNameLength, OutletNodeName ),
			OutletNode( OutletNode )
		{}

	};

	struct BranchData
	{
		// Members
		Fstring Name; // Name for this Branch
		Fstring AssignedLoopName; // Loop Name for this branch
		Real64 MaxFlowRate; // Max Flow Rate of the Branch
		int PressureCurveType; // Integer index of pressure curve type
		int PressureCurveIndex; // Integer index of pressure curve
		int FluidType; // Fluid type (see DataLoopNode)
		int NumOfComponents; // Number of Components on this Branch
		FArray1D< ComponentData > Component; // Component definitions for each component

		// Default Constructor
		BranchData() :
			Name( MaxNameLength, Blank ),
			AssignedLoopName( MaxNameLength, Blank ),
			MaxFlowRate( 0.0 ),
			PressureCurveType( 0 ),
			PressureCurveIndex( 0 ),
			FluidType( NodeType_Unknown ),
			NumOfComponents( 0 )
		{}

		// Member Constructor
		BranchData(
			Fstring const & Name, // Name for this Branch
			Fstring const & AssignedLoopName, // Loop Name for this branch
			Real64 const MaxFlowRate, // Max Flow Rate of the Branch
			int const PressureCurveType, // Integer index of pressure curve type
			int const PressureCurveIndex, // Integer index of pressure curve
			int const FluidType, // Fluid type (see DataLoopNode)
			int const NumOfComponents, // Number of Components on this Branch
			FArray1< ComponentData > const & Component // Component definitions for each component
		) :
			Name( MaxNameLength, Name ),
			AssignedLoopName( MaxNameLength, AssignedLoopName ),
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
		Fstring Name; // Splitter Name
		Fstring InletBranchName; // Splitter Inlet Branch Name
		int NumOutletBranches; // Number of outlets on this Splitter
		FArray1D_Fstring OutletBranchNames; // Names of the Outlet Branches

		// Default Constructor
		SplitterData() :
			Name( MaxNameLength, Blank ),
			InletBranchName( MaxNameLength, Blank ),
			NumOutletBranches( 0 ),
			OutletBranchNames( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		SplitterData(
			Fstring const & Name, // Splitter Name
			Fstring const & InletBranchName, // Splitter Inlet Branch Name
			int const NumOutletBranches, // Number of outlets on this Splitter
			FArray1_Fstring const & OutletBranchNames // Names of the Outlet Branches
		) :
			Name( MaxNameLength, Name ),
			InletBranchName( MaxNameLength, InletBranchName ),
			NumOutletBranches( NumOutletBranches ),
			OutletBranchNames( OutletBranchNames )
		{}

	};

	struct MixerData
	{
		// Members
		Fstring Name; // Mixer Name
		Fstring OutletBranchName; // Mixer Outlet Branch Name
		int NumInletBranches; // Number of inlets for this Mixer
		FArray1D_Fstring InletBranchNames; // Names of Inlet Branches

		// Default Constructor
		MixerData() :
			Name( MaxNameLength, Blank ),
			OutletBranchName( MaxNameLength, Blank ),
			NumInletBranches( 0 ),
			InletBranchNames( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		MixerData(
			Fstring const & Name, // Mixer Name
			Fstring const & OutletBranchName, // Mixer Outlet Branch Name
			int const NumInletBranches, // Number of inlets for this Mixer
			FArray1_Fstring const & InletBranchNames // Names of Inlet Branches
		) :
			Name( MaxNameLength, Name ),
			OutletBranchName( MaxNameLength, OutletBranchName ),
			NumInletBranches( NumInletBranches ),
			InletBranchNames( InletBranchNames )
		{}

	};

	// Object Data
	extern FArray1D< BranchListData > BranchList; // Branch List data for each Branch List
	extern FArray1D< BranchData > Branch; // Branch Data for each Branch
	extern FArray1D< ConnectorData > ConnectorLists; // Connector List data for each Connector List
	extern FArray1D< SplitterData > Splitters; // Splitter Data for each Splitter
	extern FArray1D< MixerData > Mixers; // Mixer Data for each Mixer

	// Functions

	void
	ManageBranchInput();

	//==================================================================================
	//   Routines that "get" data from internal branch management structure
	//==================================================================================

	void
	GetBranchList(
		Fstring const & LoopName, // Name of Loop Branch List is on
		Fstring const & BranchListName, // Branch List Name from Input
		int & NumBranchNames, // Number of Branches for this Branch List
		FArray1S_Fstring BranchNames, // Names of Branches on this Branch List
		Fstring const & LoopType // Type of Loop Branch list is on
	);

	int
	NumBranchesInBranchList( Fstring const & BranchListName );

	void
	GetBranchData(
		Fstring const & LoopName, // Loop Name of this Branch
		Fstring const & BranchName, // Requested Branch Name
		Real64 & BranchMaxFlow, // Max Flow Rate for Branch
		int & PressCurveType, // Index of a pressure curve object
		int & PressCurveIndex, // Index of a pressure curve object
		int & NumComps, // Number of Components on Branch
		FArray1S_Fstring CompType, // Component Type for each item on Branch
		FArray1S_Fstring CompName, // Component Name for each item on Branch
		FArray1S_Fstring CompInletNodeNames, // Component Inlet Node IDs for each item on Branch
		FArray1S_int CompInletNodeNums, // Component Inlet Node Numbers for each item on Branch
		FArray1S_Fstring CompOutletNodeNames, // Component Outlet Node IDs for each item on Branch
		FArray1S_int CompOutletNodeNums, // Component Outlet Node Numbers for each item on Branch
		bool & ErrorsFound
	);

	int
	NumCompsInBranch( Fstring const & BranchName );

	int
	GetAirBranchIndex(
		Fstring const & CompType,
		Fstring const & CompName
	);

	Real64
	GetBranchFlow( int const BranchNum );

	void
	GetBranchFanTypeName(
		int const BranchNum,
		Fstring & FanType,
		Fstring & FanName,
		bool & ErrFound
	);

	void
	CheckBranchForOASys(
		Fstring const & CompType,
		Fstring const & CompName,
		bool & OASysFlag,
		bool & ErrFound
	);

	void
	GetInternalBranchData(
		Fstring const & LoopName, // Loop Name for Branch
		Fstring const & BranchName, // Requested Branch Name
		Real64 & BranchMaxFlow, // Max Flow Rate for Branch
		int & PressCurveType, // Index of pressure curve object
		int & PressCurveIndex, // Index of pressure curve object
		int & NumComps, // Number of Components on Branch
		FArray1S< ComponentData > BComponents, // Component data returned
		bool & ErrorsFound // True when Loop Name is already assigned and this not same loop
	);

	void
	GetNumSplitterMixerInConntrList(
		Fstring const & LoopName, // Loop Name for this Splitter (used in error message)
		Fstring const & ConnectorListName, // Requested Connector List Name
		int & NumSplitters, // Number of splitters in the loop
		int & NumMixers, // Number of mixers in the loop
		bool & ErrorsFound // if no connector list
	);

	void
	GetConnectorList(
		Fstring const & ConnectorListName, // Requested Connector List
		ConnectorData & Connectoid, // Returned Connector Data
		Optional_int_const NumInList = _ // Number of the current connector in the list of connectors
	);

	void
	GetLoopMixer(
		Fstring const & LoopName, // Loop Name for Mixer
		Fstring const & ConnectorListName, // Requested Connector List Name
		Fstring & MixerName, // Name of Mixer
		bool & IsMixer, // True when Mixer is on this connector
		Fstring & OutletNodeName, // Outlet Node ID
		int & OutletNodeNum, // Outlet Node Number
		int & NumInletNodes, // Number of Inlet Nodes
		FArray1S_Fstring InletNodeNames, // Inlet Node IDs
		FArray1S_int InletNodeNums, // Inlet Node Numbers
		bool & ErrorsFound,
		Optional_int_const ConnectorNumber = _, // number of the current item in connector list
		Optional_int MixerNumber = _ // Mixer number for this specific splitter
	);

	void
	GetLoopSplitter(
		Fstring const & LoopName, // Loop Name for this Splitter
		Fstring const & ConnectorListName, // Requested Connector List Name
		Fstring & SplitterName, // Name of Splitter
		bool & IsSplitter, // True if splitter on this connector list
		Fstring & InletNodeName, // Inlet Node ID
		int & InletNodeNum, // Inlet Node Number
		int & NumOutletNodes, // Number of Outlet Nodes
		FArray1S_Fstring OutletNodeNames, // Outlet Node IDs
		FArray1S_int OutletNodeNums, // Outlet Node Numbers
		bool & ErrorsFound,
		Optional_int_const ConnectorNumber = _, // number of the current item in connector list
		Optional_int SplitterNumber = _ // splitter number for this specific splitter
	);

	Fstring
	GetFirstBranchInletNodeName( Fstring const & BranchListName ); // Branch List name to search

	Fstring
	GetLastBranchOutletNodeName( Fstring const & BranchListName ); // Branch List name to search

	void
	CheckSystemBranchFlow(
		Fstring const & SystemType, // type of air loop equipment
		Fstring const & SystemName, // name of air loop equipment
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
		Fstring const & BranchListName,
		Fstring & FoundPlantLoopName,
		int & FoundPlantLoopNum,
		Fstring & FoundSupplyDemand,
		Real64 & FoundVolFlowRate,
		bool & MatchedPlantLoop
	);

	void
	FindCondenserLoopBranchConnection(
		Fstring const & BranchListName,
		Fstring & FoundCondLoopName,
		int & FoundCondLoopNum,
		Fstring & FoundSupplyDemand,
		Real64 & FoundVolFlowRate,
		bool & MatchedCondLoop
	);

	void
	FindAirLoopBranchConnection(
		Fstring const & BranchListName,
		Fstring & FoundAirLoopName,
		int & FoundAirLoopNum,
		Fstring & FoundAir,
		Real64 & FoundVolFlowRate,
		bool & MatchedAirLoop
	);

	void
	FindAirPlantCondenserLoopFromBranchList(
		Fstring const & BranchListName, // Branch List Name
		Fstring & LoopType, // LoopType (if found, Plant,Condenser or Air)
		Fstring & LoopSupplyDemandAir, // Supply if "Supply" or Demand if "Demand" or Air if "Air"
		bool & MatchedLoop // true if found
	);

	//==================================================================================
	//   Routines that test branch integrity
	//==================================================================================

	void
	AuditBranches(
		bool const mustprint, // true if the warning should be printed.
		Optional_Fstring_const CompType = _, // when mustprint (ScanPlantLoop)  use CompType in error message and scan
		Optional_Fstring_const CompName = _ // when mustprint (ScanPlantLoop)  use CompName in error message and scan
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

} // BranchInputManager

} // EnergyPlus

#endif
