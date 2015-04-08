#ifndef DataAirSystems_hh_INCLUDED
#define DataAirSystems_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataPlant.hh>

namespace EnergyPlus {

namespace DataAirSystems {

	// Using/Aliasing
	using DataPlant::MeterData;
	using DataPlant::SubcomponentData;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// DERIVED TYPE DEFINITIONS

	// DefinePrimaryAirSystem contains the data for a primary air HVAC system

	// The ConnectionPoint derived type is used to link quickly between loops at connection points
	// and avoids the need for repetitive searches.

	// INTERFACE BLOCK SPECIFICATIONS
	// None

	// MODULE VARIABLE DECLARATIONS
	// For each type of air path, define an array of DefineAirPaths

	// Temporary arrays

	// Types

	struct AirLoopCompData // data for an individual component
	{
		// Members
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		int CompType_Num; // Numeric designator for CompType (TypeOf)
		int CompIndex; // Component Index in whatever is using this component
		int FlowCtrl; // Component flow control (ACTIVE/PASSIVE)
		bool ON; // When true, the designated component or operation scheme is available
		bool Parent; // When true, the designated component is made up of sub-components
		std::string NodeNameIn; // Component inlet node name
		std::string NodeNameOut; // Component outlet node name
		int NodeNumIn; // Component inlet node number
		int NodeNumOut; // Component outlet node number
		bool MeteredVarsFound;
		int NumMeteredVars;
		int NumSubComps;
		int EnergyTransComp; // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
		Real64 Capacity; // ventilation load factor
		int OpMode;
		Real64 TotPlantSupplyElec;
		Real64 PlantSupplyElecEff;
		Real64 PeakPlantSupplyElecEff;
		Real64 TotPlantSupplyGas;
		Real64 PlantSupplyGasEff;
		Real64 PeakPlantSupplyGasEff;
		Real64 TotPlantSupplyPurch;
		Real64 PlantSupplyPurchEff;
		Real64 PeakPlantSupplyPurchEff;
		Real64 TotPlantSupplyOther;
		Real64 PlantSupplyOtherEff;
		Real64 PeakPlantSupplyOtherEff;
		int AirSysToPlantPtr; // =0 No plant loop connection, >0 index to AirSysToPlant array
		Array1D< MeterData > MeteredVar; // Index of energy output report data
		Array1D< SubcomponentData > SubComp; // Component list

		// Default Constructor
		AirLoopCompData() :
			CompType_Num( 0 ),
			CompIndex( 0 ),
			FlowCtrl( 0 ),
			ON( true ),
			Parent( false ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			MeteredVarsFound( false ),
			NumMeteredVars( 0 ),
			NumSubComps( 0 ),
			EnergyTransComp( 0 ),
			Capacity( 0.0 ),
			OpMode( 0 ),
			TotPlantSupplyElec( 0.0 ),
			PlantSupplyElecEff( 0.0 ),
			PeakPlantSupplyElecEff( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			PlantSupplyGasEff( 0.0 ),
			PeakPlantSupplyGasEff( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			PlantSupplyPurchEff( 0.0 ),
			PeakPlantSupplyPurchEff( 0.0 ),
			TotPlantSupplyOther( 0.0 ),
			PlantSupplyOtherEff( 0.0 ),
			PeakPlantSupplyOtherEff( 0.0 ),
			AirSysToPlantPtr( 0 )
		{}

		// Member Constructor
		AirLoopCompData(
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			int const CompType_Num, // Numeric designator for CompType (TypeOf)
			int const CompIndex, // Component Index in whatever is using this component
			int const FlowCtrl, // Component flow control (ACTIVE/PASSIVE)
			bool const ON, // When true, the designated component or operation scheme is available
			bool const Parent, // When true, the designated component is made up of sub-components
			std::string const & NodeNameIn, // Component inlet node name
			std::string const & NodeNameOut, // Component outlet node name
			int const NodeNumIn, // Component inlet node number
			int const NodeNumOut, // Component outlet node number
			bool const MeteredVarsFound,
			int const NumMeteredVars,
			int const NumSubComps,
			int const EnergyTransComp, // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
			Real64 const Capacity, // ventilation load factor
			int const OpMode,
			Real64 const TotPlantSupplyElec,
			Real64 const PlantSupplyElecEff,
			Real64 const PeakPlantSupplyElecEff,
			Real64 const TotPlantSupplyGas,
			Real64 const PlantSupplyGasEff,
			Real64 const PeakPlantSupplyGasEff,
			Real64 const TotPlantSupplyPurch,
			Real64 const PlantSupplyPurchEff,
			Real64 const PeakPlantSupplyPurchEff,
			Real64 const TotPlantSupplyOther,
			Real64 const PlantSupplyOtherEff,
			Real64 const PeakPlantSupplyOtherEff,
			int const AirSysToPlantPtr, // =0 No plant loop connection, >0 index to AirSysToPlant array
			Array1< MeterData > const & MeteredVar, // Index of energy output report data
			Array1< SubcomponentData > const & SubComp // Component list
		) :
			TypeOf( TypeOf ),
			Name( Name ),
			CompType_Num( CompType_Num ),
			CompIndex( CompIndex ),
			FlowCtrl( FlowCtrl ),
			ON( ON ),
			Parent( Parent ),
			NodeNameIn( NodeNameIn ),
			NodeNameOut( NodeNameOut ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			MeteredVarsFound( MeteredVarsFound ),
			NumMeteredVars( NumMeteredVars ),
			NumSubComps( NumSubComps ),
			EnergyTransComp( EnergyTransComp ),
			Capacity( Capacity ),
			OpMode( OpMode ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			PlantSupplyElecEff( PlantSupplyElecEff ),
			PeakPlantSupplyElecEff( PeakPlantSupplyElecEff ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			PlantSupplyGasEff( PlantSupplyGasEff ),
			PeakPlantSupplyGasEff( PeakPlantSupplyGasEff ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			PlantSupplyPurchEff( PlantSupplyPurchEff ),
			PeakPlantSupplyPurchEff( PeakPlantSupplyPurchEff ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			PlantSupplyOtherEff( PlantSupplyOtherEff ),
			PeakPlantSupplyOtherEff( PeakPlantSupplyOtherEff ),
			AirSysToPlantPtr( AirSysToPlantPtr ),
			MeteredVar( MeteredVar ),
			SubComp( SubComp )
		{}

	};

	struct AirLoopBranchData // a branch is a sequence of components
	{
		// Members
		std::string Name; // Name of the branch
		std::string ControlType; // Control type for the branch (not used)
		Real64 MinVolFlowRate; // minimum flow rate for the branch (m3/s)
		Real64 MaxVolFlowRate; // maximum flow rate for the branch (m3/s)
		Real64 MinMassFlowRate; // minimum mass flow rate for the branch (kg/s)
		Real64 MaxMassFlowRate; // maximum mass flow rate for the branch (kg/s)
		int TotalComponents; // Total number of high level components on the branch
		Array1D_int FirstCompIndex; // Gives the component index in AllComp that corresponds to Comp
		Array1D_int LastCompIndex; // Gives comp index in AllComp that corresponds to last subcomponent
		int NodeNumIn; // Branch inlet node number
		int NodeNumOut; // Branch outlet node number
		int DuctType; // 1=main, 2=cooling, 3=heating, 4=other
		Array1D< AirLoopCompData > Comp; // Component list--high level components
		//  TYPE(ExpandedCompData), &
		//           ALLOCATABLE, DIMENSION(:) :: MegaComp              ! Component list
		//  This list would include children, grandchildren, etc.
		int TotalNodes; // total number of nodes on branch
		Array1D_int NodeNum; // node list (numbers)

		// Default Constructor
		AirLoopBranchData() :
			MinVolFlowRate( 0.0 ),
			MaxVolFlowRate( 0.0 ),
			MinMassFlowRate( 0.0 ),
			MaxMassFlowRate( 0.0 ),
			TotalComponents( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			DuctType( 0 ),
			TotalNodes( 0 )
		{}

		// Member Constructor
		AirLoopBranchData(
			std::string const & Name, // Name of the branch
			std::string const & ControlType, // Control type for the branch (not used)
			Real64 const MinVolFlowRate, // minimum flow rate for the branch (m3/s)
			Real64 const MaxVolFlowRate, // maximum flow rate for the branch (m3/s)
			Real64 const MinMassFlowRate, // minimum mass flow rate for the branch (kg/s)
			Real64 const MaxMassFlowRate, // maximum mass flow rate for the branch (kg/s)
			int const TotalComponents, // Total number of high level components on the branch
			Array1_int const & FirstCompIndex, // Gives the component index in AllComp that corresponds to Comp
			Array1_int const & LastCompIndex, // Gives comp index in AllComp that corresponds to last subcomponent
			int const NodeNumIn, // Branch inlet node number
			int const NodeNumOut, // Branch outlet node number
			int const DuctType, // 1=main, 2=cooling, 3=heating, 4=other
			Array1< AirLoopCompData > const & Comp, // Component list--high level components
			int const TotalNodes, // total number of nodes on branch
			Array1_int const & NodeNum // node list (numbers)
		) :
			Name( Name ),
			ControlType( ControlType ),
			MinVolFlowRate( MinVolFlowRate ),
			MaxVolFlowRate( MaxVolFlowRate ),
			MinMassFlowRate( MinMassFlowRate ),
			MaxMassFlowRate( MaxMassFlowRate ),
			TotalComponents( TotalComponents ),
			FirstCompIndex( FirstCompIndex ),
			LastCompIndex( LastCompIndex ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			DuctType( DuctType ),
			Comp( Comp ),
			TotalNodes( TotalNodes ),
			NodeNum( NodeNum )
		{}

	};

	struct AirLoopSplitterData // a splitter joins 1 inlet branch to multiple outlet branches
	{
		// Members
		bool Exists; // True if there is a splitter (only 1 allowed per loop)
		std::string Name; // Name of the Splitter
		int NodeNumIn; // Node number for the inlet to the splitter
		int BranchNumIn; // Reference number for branch connected to splitter inlet
		std::string NodeNameIn; // Node name for the inlet to the splitter
		int TotalOutletNodes; // Number of outlet nodes for the splitter
		Array1D_int NodeNumOut; // Node numbers for the outlets to the splitter
		Array1D_int BranchNumOut; // Reference numbers for branches connected to splitter outlet
		Array1D_string NodeNameOut; // Node names for the outlets to the splitter

		// Default Constructor
		AirLoopSplitterData() :
			Exists( false ),
			NodeNumIn( 0 ),
			BranchNumIn( 0 ),
			TotalOutletNodes( 0 )
		{}

		// Member Constructor
		AirLoopSplitterData(
			bool const Exists, // True if there is a splitter (only 1 allowed per loop)
			std::string const & Name, // Name of the Splitter
			int const NodeNumIn, // Node number for the inlet to the splitter
			int const BranchNumIn, // Reference number for branch connected to splitter inlet
			std::string const & NodeNameIn, // Node name for the inlet to the splitter
			int const TotalOutletNodes, // Number of outlet nodes for the splitter
			Array1_int const & NodeNumOut, // Node numbers for the outlets to the splitter
			Array1_int const & BranchNumOut, // Reference numbers for branches connected to splitter outlet
			Array1_string const & NodeNameOut // Node names for the outlets to the splitter
		) :
			Exists( Exists ),
			Name( Name ),
			NodeNumIn( NodeNumIn ),
			BranchNumIn( BranchNumIn ),
			NodeNameIn( NodeNameIn ),
			TotalOutletNodes( TotalOutletNodes ),
			NodeNumOut( NodeNumOut ),
			BranchNumOut( BranchNumOut ),
			NodeNameOut( NodeNameOut )
		{}

	};

	struct AirLoopMixerData // a mixer joins multiple inlet branches to a single outlet branch
	{
		// Members
		bool Exists; // True if there is a Mixer (only 1 allowed per loop)
		std::string Name; // Name of the Mixer
		int NodeNumOut; // Node number for the outlet to the mixer
		int BranchNumOut; // Reference number for branch connected to mixer outlet
		std::string NodeNameOut; // Node name for the outlet to the mixer
		int TotalInletNodes; // Number of inlet nodes for the mixer
		Array1D_int NodeNumIn; // Node numbers for the inlets to the mixer
		Array1D_int BranchNumIn; // Reference numbers for branches connected to mixer inlet
		Array1D_string NodeNameIn; // Node names for the inlets to the mixer

		// Default Constructor
		AirLoopMixerData() :
			Exists( false ),
			NodeNumOut( 0 ),
			BranchNumOut( 0 ),
			TotalInletNodes( 0 )
		{}

		// Member Constructor
		AirLoopMixerData(
			bool const Exists, // True if there is a Mixer (only 1 allowed per loop)
			std::string const & Name, // Name of the Mixer
			int const NodeNumOut, // Node number for the outlet to the mixer
			int const BranchNumOut, // Reference number for branch connected to mixer outlet
			std::string const & NodeNameOut, // Node name for the outlet to the mixer
			int const TotalInletNodes, // Number of inlet nodes for the mixer
			Array1_int const & NodeNumIn, // Node numbers for the inlets to the mixer
			Array1_int const & BranchNumIn, // Reference numbers for branches connected to mixer inlet
			Array1_string const & NodeNameIn // Node names for the inlets to the mixer
		) :
			Exists( Exists ),
			Name( Name ),
			NodeNumOut( NodeNumOut ),
			BranchNumOut( BranchNumOut ),
			NodeNameOut( NodeNameOut ),
			TotalInletNodes( TotalInletNodes ),
			NodeNumIn( NodeNumIn ),
			BranchNumIn( BranchNumIn ),
			NodeNameIn( NodeNameIn )
		{}

	};

	struct DefinePrimaryAirSystem // There is an array of these for each primary air system
	{
		// Members
		std::string Name; // name of the system
		Real64 DesignVolFlowRate; // the design total supply air flow rate (m3/s)
		int NumControllers; // number of controllers on this air path
		Array1D_string ControllerName; // name of each controller on this system
		Array1D_string ControllerType; // type of each controller on this system
		Array1D_int ControllerIndex;
		Array1D_bool CanBeLockedOutByEcono; // true if controller inactive
		// when the economizer is active
		int NumBranches; // number of branches making up this system
		Array1D< AirLoopBranchData > Branch; // data for each branch
		AirLoopSplitterData Splitter; // Data for splitter (if any)
		AirLoopMixerData Mixer; // Data for mixer (if any)
		Array1D_bool ControlConverged; // Convergence Parameter for controllers
		int NumOutletBranches;
		Array1D_int OutletBranchNum; // branch numbers of system outlets
		int NumInletBranches;
		Array1D_int InletBranchNum; // branch number of system inlets
		bool OASysExists; // true if there is an Outside Air Sys
		int OASysInletNodeNum; // node number of return air inlet to OA sys
		int OASysOutletNodeNum; // node number of mixed air outlet of OA sys
		int OAMixOAInNodeNum; // node number of the OA stream inlet to the
		// OA mixer component.
		bool RABExists; // true if there is a RAB
		int RABMixInNode; // node num of RAB mixer inlet
		int SupMixInNode; // node num of supply air inlet to mixer
		int MixOutNode; // outlet node of mixer
		int RABSplitOutNode; // node num of RAB splitter outlet
		int OtherSplitOutNode; // node num of nonRAB splitter outlet
		int NumOACoolCoils; // number of cooling coils in the outside air system
		int NumOAHeatCoils; // number of heating coils in the outside air system
		bool SizeAirloopCoil; // simulates air loop coils before calling controllers
		int SupFanNum; // index of the supply fan in the Fan data structure
		int RetFanNum; // index of the return fan in the Fan data structure
		Real64 FanDesCoolLoad; // design fan heat gain for the air loop [W]

		// Default Constructor
		DefinePrimaryAirSystem() :
			DesignVolFlowRate( 0.0 ),
			NumControllers( 0 ),
			NumBranches( 0 ),
			NumOutletBranches( 0 ),
			OutletBranchNum( 3, 0 ),
			NumInletBranches( 0 ),
			InletBranchNum( 3, 0 ),
			OASysExists( false ),
			OASysInletNodeNum( 0 ),
			OASysOutletNodeNum( 0 ),
			OAMixOAInNodeNum( 0 ),
			RABExists( false ),
			RABMixInNode( 0 ),
			SupMixInNode( 0 ),
			MixOutNode( 0 ),
			RABSplitOutNode( 0 ),
			OtherSplitOutNode( 0 ),
			NumOACoolCoils( 0 ),
			NumOAHeatCoils( 0 ),
			SizeAirloopCoil( true ),
			SupFanNum( 0 ),
			RetFanNum( 0 ),
			FanDesCoolLoad(0.0)
		{}

		// Member Constructor
		DefinePrimaryAirSystem(
			std::string const & Name, // name of the system
			Real64 const DesignVolFlowRate, // the design total supply air flow rate (m3/s)
			int const NumControllers, // number of controllers on this air path
			Array1_string const & ControllerName, // name of each controller on this system
			Array1_string const & ControllerType, // type of each controller on this system
			Array1_int const & ControllerIndex,
			Array1_bool const & CanBeLockedOutByEcono, // true if controller inactive
			int const NumBranches, // number of branches making up this system
			Array1< AirLoopBranchData > const & Branch, // data for each branch
			AirLoopSplitterData const & Splitter, // Data for splitter (if any)
			AirLoopMixerData const & Mixer, // Data for mixer (if any)
			Array1_bool const & ControlConverged, // Convergence Parameter for controllers
			int const NumOutletBranches,
			Array1_int const & OutletBranchNum, // branch numbers of system outlets
			int const NumInletBranches,
			Array1_int const & InletBranchNum, // branch number of system inlets
			bool const OASysExists, // true if there is an Outside Air Sys
			int const OASysInletNodeNum, // node number of return air inlet to OA sys
			int const OASysOutletNodeNum, // node number of mixed air outlet of OA sys
			int const OAMixOAInNodeNum, // node number of the OA stream inlet to the
			bool const RABExists, // true if there is a RAB
			int const RABMixInNode, // node num of RAB mixer inlet
			int const SupMixInNode, // node num of supply air inlet to mixer
			int const MixOutNode, // outlet node of mixer
			int const RABSplitOutNode, // node num of RAB splitter outlet
			int const OtherSplitOutNode, // node num of nonRAB splitter outlet
			int const NumOACoolCoils, // number of cooling coils in the outside air system
			int const NumOAHeatCoils, // number of heating coils in the outside air system
			bool const SizeAirloopCoil, // simulates air loop coils before calling controllers
			int const SupFanNum, // index of the supply fan in the Fan data structure
			int const RetFanNum, // index of the return fan in the Fan data structure
			Real64 const FanDesCoolLoad // air loop fan design heat gain
		) :
			Name( Name ),
			DesignVolFlowRate( DesignVolFlowRate ),
			NumControllers( NumControllers ),
			ControllerName( ControllerName ),
			ControllerType( ControllerType ),
			ControllerIndex( ControllerIndex ),
			CanBeLockedOutByEcono( CanBeLockedOutByEcono ),
			NumBranches( NumBranches ),
			Branch( Branch ),
			Splitter( Splitter ),
			Mixer( Mixer ),
			ControlConverged( ControlConverged ),
			NumOutletBranches( NumOutletBranches ),
			OutletBranchNum( 3, OutletBranchNum ),
			NumInletBranches( NumInletBranches ),
			InletBranchNum( 3, InletBranchNum ),
			OASysExists( OASysExists ),
			OASysInletNodeNum( OASysInletNodeNum ),
			OASysOutletNodeNum( OASysOutletNodeNum ),
			OAMixOAInNodeNum( OAMixOAInNodeNum ),
			RABExists( RABExists ),
			RABMixInNode( RABMixInNode ),
			SupMixInNode( SupMixInNode ),
			MixOutNode( MixOutNode ),
			RABSplitOutNode( RABSplitOutNode ),
			OtherSplitOutNode( OtherSplitOutNode ),
			NumOACoolCoils( NumOACoolCoils ),
			NumOAHeatCoils( NumOAHeatCoils ),
			SizeAirloopCoil( SizeAirloopCoil ),
			SupFanNum( SupFanNum ),
			RetFanNum( RetFanNum ),
			FanDesCoolLoad( FanDesCoolLoad )
		{}

	};

	struct ConnectionPoint
	{
		// Members
		int LoopType;
		int LoopNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		ConnectionPoint() :
			LoopType( 0 ),
			LoopNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		ConnectionPoint(
			int const LoopType,
			int const LoopNum,
			int const BranchNum,
			int const CompNum
		) :
			LoopType( LoopType ),
			LoopNum( LoopNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct ConnectZoneComp
	{
		// Members
		int ZoneEqListNum;
		int ZoneEqCompNum;
		int PlantLoopType;
		int PlantLoopNum;
		int PlantLoopBranch;
		int PlantLoopComp;
		int FirstDemandSidePtr;
		int LastDemandSidePtr;

		// Default Constructor
		ConnectZoneComp() :
			ZoneEqListNum( 0 ),
			ZoneEqCompNum( 0 ),
			PlantLoopType( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopBranch( 0 ),
			PlantLoopComp( 0 ),
			FirstDemandSidePtr( 0 ),
			LastDemandSidePtr( 0 )
		{}

		// Member Constructor
		ConnectZoneComp(
			int const ZoneEqListNum,
			int const ZoneEqCompNum,
			int const PlantLoopType,
			int const PlantLoopNum,
			int const PlantLoopBranch,
			int const PlantLoopComp,
			int const FirstDemandSidePtr,
			int const LastDemandSidePtr
		) :
			ZoneEqListNum( ZoneEqListNum ),
			ZoneEqCompNum( ZoneEqCompNum ),
			PlantLoopType( PlantLoopType ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopBranch( PlantLoopBranch ),
			PlantLoopComp( PlantLoopComp ),
			FirstDemandSidePtr( FirstDemandSidePtr ),
			LastDemandSidePtr( LastDemandSidePtr )
		{}

	};

	struct ConnectZoneSubComp
	{
		// Members
		int ZoneEqListNum;
		int ZoneEqCompNum;
		int ZoneEqSubCompNum;
		int PlantLoopType;
		int PlantLoopNum;
		int PlantLoopBranch;
		int PlantLoopComp;
		int FirstDemandSidePtr;
		int LastDemandSidePtr;

		// Default Constructor
		ConnectZoneSubComp() :
			ZoneEqListNum( 0 ),
			ZoneEqCompNum( 0 ),
			ZoneEqSubCompNum( 0 ),
			PlantLoopType( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopBranch( 0 ),
			PlantLoopComp( 0 ),
			FirstDemandSidePtr( 0 ),
			LastDemandSidePtr( 0 )
		{}

		// Member Constructor
		ConnectZoneSubComp(
			int const ZoneEqListNum,
			int const ZoneEqCompNum,
			int const ZoneEqSubCompNum,
			int const PlantLoopType,
			int const PlantLoopNum,
			int const PlantLoopBranch,
			int const PlantLoopComp,
			int const FirstDemandSidePtr,
			int const LastDemandSidePtr
		) :
			ZoneEqListNum( ZoneEqListNum ),
			ZoneEqCompNum( ZoneEqCompNum ),
			ZoneEqSubCompNum( ZoneEqSubCompNum ),
			PlantLoopType( PlantLoopType ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopBranch( PlantLoopBranch ),
			PlantLoopComp( PlantLoopComp ),
			FirstDemandSidePtr( FirstDemandSidePtr ),
			LastDemandSidePtr( LastDemandSidePtr )
		{}

	};

	struct ConnectZoneSubSubComp
	{
		// Members
		int ZoneEqListNum;
		int ZoneEqCompNum;
		int ZoneEqSubCompNum;
		int ZoneEqSubSubCompNum;
		int PlantLoopType;
		int PlantLoopNum;
		int PlantLoopBranch;
		int PlantLoopComp;
		int FirstDemandSidePtr;
		int LastDemandSidePtr;

		// Default Constructor
		ConnectZoneSubSubComp() :
			ZoneEqListNum( 0 ),
			ZoneEqCompNum( 0 ),
			ZoneEqSubCompNum( 0 ),
			ZoneEqSubSubCompNum( 0 ),
			PlantLoopType( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopBranch( 0 ),
			PlantLoopComp( 0 ),
			FirstDemandSidePtr( 0 ),
			LastDemandSidePtr( 0 )
		{}

		// Member Constructor
		ConnectZoneSubSubComp(
			int const ZoneEqListNum,
			int const ZoneEqCompNum,
			int const ZoneEqSubCompNum,
			int const ZoneEqSubSubCompNum,
			int const PlantLoopType,
			int const PlantLoopNum,
			int const PlantLoopBranch,
			int const PlantLoopComp,
			int const FirstDemandSidePtr,
			int const LastDemandSidePtr
		) :
			ZoneEqListNum( ZoneEqListNum ),
			ZoneEqCompNum( ZoneEqCompNum ),
			ZoneEqSubCompNum( ZoneEqSubCompNum ),
			ZoneEqSubSubCompNum( ZoneEqSubSubCompNum ),
			PlantLoopType( PlantLoopType ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopBranch( PlantLoopBranch ),
			PlantLoopComp( PlantLoopComp ),
			FirstDemandSidePtr( FirstDemandSidePtr ),
			LastDemandSidePtr( LastDemandSidePtr )
		{}

	};

	struct ConnectAirSysComp
	{
		// Members
		int AirLoopNum;
		int AirLoopBranch;
		int AirLoopComp;
		int PlantLoopType;
		int PlantLoopNum;
		int PlantLoopBranch;
		int PlantLoopComp;
		int FirstDemandSidePtr;
		int LastDemandSidePtr;

		// Default Constructor
		ConnectAirSysComp() :
			AirLoopNum( 0 ),
			AirLoopBranch( 0 ),
			AirLoopComp( 0 ),
			PlantLoopType( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopBranch( 0 ),
			PlantLoopComp( 0 ),
			FirstDemandSidePtr( 0 ),
			LastDemandSidePtr( 0 )
		{}

		// Member Constructor
		ConnectAirSysComp(
			int const AirLoopNum,
			int const AirLoopBranch,
			int const AirLoopComp,
			int const PlantLoopType,
			int const PlantLoopNum,
			int const PlantLoopBranch,
			int const PlantLoopComp,
			int const FirstDemandSidePtr,
			int const LastDemandSidePtr
		) :
			AirLoopNum( AirLoopNum ),
			AirLoopBranch( AirLoopBranch ),
			AirLoopComp( AirLoopComp ),
			PlantLoopType( PlantLoopType ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopBranch( PlantLoopBranch ),
			PlantLoopComp( PlantLoopComp ),
			FirstDemandSidePtr( FirstDemandSidePtr ),
			LastDemandSidePtr( LastDemandSidePtr )
		{}

	};

	struct ConnectAirSysSubComp
	{
		// Members
		int AirLoopNum;
		int AirLoopBranch;
		int AirLoopComp;
		int AirLoopSubComp;
		int PlantLoopType;
		int PlantLoopNum;
		int PlantLoopBranch;
		int PlantLoopComp;
		int FirstDemandSidePtr;
		int LastDemandSidePtr;

		// Default Constructor
		ConnectAirSysSubComp() :
			AirLoopNum( 0 ),
			AirLoopBranch( 0 ),
			AirLoopComp( 0 ),
			AirLoopSubComp( 0 ),
			PlantLoopType( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopBranch( 0 ),
			PlantLoopComp( 0 ),
			FirstDemandSidePtr( 0 ),
			LastDemandSidePtr( 0 )
		{}

		// Member Constructor
		ConnectAirSysSubComp(
			int const AirLoopNum,
			int const AirLoopBranch,
			int const AirLoopComp,
			int const AirLoopSubComp,
			int const PlantLoopType,
			int const PlantLoopNum,
			int const PlantLoopBranch,
			int const PlantLoopComp,
			int const FirstDemandSidePtr,
			int const LastDemandSidePtr
		) :
			AirLoopNum( AirLoopNum ),
			AirLoopBranch( AirLoopBranch ),
			AirLoopComp( AirLoopComp ),
			AirLoopSubComp( AirLoopSubComp ),
			PlantLoopType( PlantLoopType ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopBranch( PlantLoopBranch ),
			PlantLoopComp( PlantLoopComp ),
			FirstDemandSidePtr( FirstDemandSidePtr ),
			LastDemandSidePtr( LastDemandSidePtr )
		{}

	};

	struct ConnectAirSysSubSubComp
	{
		// Members
		int AirLoopNum;
		int AirLoopBranch;
		int AirLoopComp;
		int AirLoopSubComp;
		int AirLoopSubSubComp;
		int PlantLoopType;
		int PlantLoopNum;
		int PlantLoopBranch;
		int PlantLoopComp;
		int FirstDemandSidePtr;
		int LastDemandSidePtr;

		// Default Constructor
		ConnectAirSysSubSubComp() :
			AirLoopNum( 0 ),
			AirLoopBranch( 0 ),
			AirLoopComp( 0 ),
			AirLoopSubComp( 0 ),
			AirLoopSubSubComp( 0 ),
			PlantLoopType( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopBranch( 0 ),
			PlantLoopComp( 0 ),
			FirstDemandSidePtr( 0 ),
			LastDemandSidePtr( 0 )
		{}

		// Member Constructor
		ConnectAirSysSubSubComp(
			int const AirLoopNum,
			int const AirLoopBranch,
			int const AirLoopComp,
			int const AirLoopSubComp,
			int const AirLoopSubSubComp,
			int const PlantLoopType,
			int const PlantLoopNum,
			int const PlantLoopBranch,
			int const PlantLoopComp,
			int const FirstDemandSidePtr,
			int const LastDemandSidePtr
		) :
			AirLoopNum( AirLoopNum ),
			AirLoopBranch( AirLoopBranch ),
			AirLoopComp( AirLoopComp ),
			AirLoopSubComp( AirLoopSubComp ),
			AirLoopSubSubComp( AirLoopSubSubComp ),
			PlantLoopType( PlantLoopType ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopBranch( PlantLoopBranch ),
			PlantLoopComp( PlantLoopComp ),
			FirstDemandSidePtr( FirstDemandSidePtr ),
			LastDemandSidePtr( LastDemandSidePtr )
		{}

	};

	// Object Data
	extern Array1D< DefinePrimaryAirSystem > PrimaryAirSystem;
	extern Array1D< ConnectionPoint > DemandSideConnect; // Connections between loops
	extern Array1D< ConnectZoneComp > ZoneCompToPlant; // Connections between loops
	extern Array1D< ConnectZoneSubComp > ZoneSubCompToPlant; // Connections between loops
	extern Array1D< ConnectZoneSubSubComp > ZoneSubSubCompToPlant; // Connections between loops
	extern Array1D< ConnectAirSysComp > AirSysCompToPlant; // Connections between loops
	extern Array1D< ConnectAirSysSubComp > AirSysSubCompToPlant; // Connections between loops
	extern Array1D< ConnectAirSysSubSubComp > AirSysSubSubCompToPlant; // Connections between loops

} // DataAirSystems

} // EnergyPlus

#endif
