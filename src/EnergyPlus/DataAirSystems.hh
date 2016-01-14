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
		bool CentralHeatCoilExists; // true if there are central heating coils
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
		int NumOAHXs; // number of heat exchangers in the outside air system
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
			CentralHeatCoilExists( true ),
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
			NumOAHXs( 0 ),
			SizeAirloopCoil( true ),
			SupFanNum( 0 ),
			RetFanNum( 0 ),
			FanDesCoolLoad(0.0)
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

	//Functions
	void
	clear_state();

} // DataAirSystems

} // EnergyPlus

#endif
