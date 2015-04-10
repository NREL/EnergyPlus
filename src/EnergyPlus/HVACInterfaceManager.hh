#ifndef HVACInterfaceManager_hh_INCLUDED
#define HVACInterfaceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HVACInterfaceManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//Common Pipe Recirc Flow Directions
	extern int const NoRecircFlow;
	extern int const PrimaryRecirc; // flow from Supply-outlet/Demand-inlet to Supply-inlet/demand-outlet
	extern int const SecondaryRecirc; // flow from Supply-inlet/Demand-oulet to Supply-outlet/demand-inlet

	extern int const FlowTypeNotSet;
	extern int const ConstantFlow;
	extern int const VariableFlow;

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern bool CommonPipeSetupFinished;

	// SUBROUTINE SPECIFICATIONS FOR MODULE ConductionTransferFunctionCalc

	// Types

	struct CommonPipeData
	{
		// Members
		int CommonPipeType; // type of common pipe used if any
		int SupplySideInletPumpType;
		int DemandSideInletPumpType;
		//Following report variables are used in uncontrolled common pipe
		int FlowDir; // Direction in which flow is in Common Pipe
		Real64 Flow; // Flow in the Common Pipe
		Real64 Temp;
		//Following report variables are used in two way common pipe
		Real64 SecCPLegFlow; // Mass flow in the secondary side Common pipe leg
		Real64 PriCPLegFlow; // Mass flow in the primary side Common pipe leg
		Real64 SecToPriFlow; // Mass flow in the pipe from Secondary to primary side
		Real64 PriToSecFlow; // Mass flow in the pipe from primary to Secondary side
		Real64 PriInTemp; // Temperature at primary inlet node
		Real64 PriOutTemp; // Temperature at primary outlet node
		Real64 SecInTemp; // Temperature at secondary inlet node
		Real64 SecOutTemp; // Temperature at secondary outlet node
		Real64 PriInletSetPoint; // Setpoint at Primary inlet node
		Real64 SecInletSetPoint; // Setpoint at Secondary inlet node
		bool PriInletControlled; // True if Primary inlet node is controlled
		bool SecInletControlled; // True if secondary inlet is controlled
		Real64 PriFlowRequest; // total flow request on supply side.

		// Default Constructor
		CommonPipeData() :
			CommonPipeType( 0 ),
			SupplySideInletPumpType( FlowTypeNotSet ),
			DemandSideInletPumpType( FlowTypeNotSet ),
			FlowDir( 0 ),
			Flow( 0.0 ),
			Temp( 0.0 ),
			SecCPLegFlow( 0.0 ),
			PriCPLegFlow( 0.0 ),
			SecToPriFlow( 0.0 ),
			PriToSecFlow( 0.0 ),
			PriInTemp( 0.0 ),
			PriOutTemp( 0.0 ),
			SecInTemp( 0.0 ),
			SecOutTemp( 0.0 ),
			PriInletSetPoint( 0.0 ),
			SecInletSetPoint( 0.0 ),
			PriInletControlled( false ),
			SecInletControlled( false ),
			PriFlowRequest( 0.0 )
		{}

		// Member Constructor
		CommonPipeData(
			int const CommonPipeType, // type of common pipe used if any
			int const SupplySideInletPumpType,
			int const DemandSideInletPumpType,
			int const FlowDir, // Direction in which flow is in Common Pipe
			Real64 const Flow, // Flow in the Common Pipe
			Real64 const Temp,
			Real64 const SecCPLegFlow, // Mass flow in the secondary side Common pipe leg
			Real64 const PriCPLegFlow, // Mass flow in the primary side Common pipe leg
			Real64 const SecToPriFlow, // Mass flow in the pipe from Secondary to primary side
			Real64 const PriToSecFlow, // Mass flow in the pipe from primary to Secondary side
			Real64 const PriInTemp, // Temperature at primary inlet node
			Real64 const PriOutTemp, // Temperature at primary outlet node
			Real64 const SecInTemp, // Temperature at secondary inlet node
			Real64 const SecOutTemp, // Temperature at secondary outlet node
			Real64 const PriInletSetPoint, // Setpoint at Primary inlet node
			Real64 const SecInletSetPoint, // Setpoint at Secondary inlet node
			bool const PriInletControlled, // True if Primary inlet node is controlled
			bool const SecInletControlled, // True if secondary inlet is controlled
			Real64 const PriFlowRequest // total flow request on supply side.
		) :
			CommonPipeType( CommonPipeType ),
			SupplySideInletPumpType( SupplySideInletPumpType ),
			DemandSideInletPumpType( DemandSideInletPumpType ),
			FlowDir( FlowDir ),
			Flow( Flow ),
			Temp( Temp ),
			SecCPLegFlow( SecCPLegFlow ),
			PriCPLegFlow( PriCPLegFlow ),
			SecToPriFlow( SecToPriFlow ),
			PriToSecFlow( PriToSecFlow ),
			PriInTemp( PriInTemp ),
			PriOutTemp( PriOutTemp ),
			SecInTemp( SecInTemp ),
			SecOutTemp( SecOutTemp ),
			PriInletSetPoint( PriInletSetPoint ),
			SecInletSetPoint( SecInletSetPoint ),
			PriInletControlled( PriInletControlled ),
			SecInletControlled( SecInletControlled ),
			PriFlowRequest( PriFlowRequest )
		{}

	};

	// Object Data
	extern Array1D< CommonPipeData > PlantCommonPipe;

	// Functions

	void
	UpdateHVACInterface(
		int const AirLoopNum, // airloop number for which air loop this is
		int const CalledFrom,
		int const OutletNode, // Node number for the outlet of the side of the loop just simulated
		int const InletNode, // Node number for the inlet of the side that needs the outlet node data
		bool & OutOfToleranceFlag // True when the other side of the loop need to be (re)simulated
	);

	//***************

	void
	UpdatePlantLoopInterface(
		int const LoopNum, // The 'inlet/outlet node' loop number
		int const ThisLoopSideNum, // The 'outlet node' LoopSide number
		int const ThisLoopSideOutletNode, // Node number for the inlet of the side that needs the outlet node data
		int const OtherLoopSideInletNode, // Node number for the outlet of the side of the loop just simulated
		bool & OutOfToleranceFlag, // True when the other side of the loop need to be (re)simulated
		int const CommonPipeType
	);

	//***************

	void
	UpdateHalfLoopInletTemp(
		int const LoopNum,
		int const TankInletLoopSide,
		Real64 & TankOutletTemp
	);

	void
	UpdateCommonPipe(
		int const LoopNum,
		int const TankInletLoopSide,
		int const CommonPipeType,
		Real64 & MixedOutletTemp
	);

	void
	ManageSingleCommonPipe(
		int const LoopNum, // plant loop number
		int const LoopSide, // plant loop side number
		Real64 const TankOutletTemp, // inlet temperature to the common pipe passed in from the capacitance calculation
		Real64 & MixedOutletTemp // inlet temperature to the common pipe passed in from the capacitance calculation
	);

	void
	ManageTwoWayCommonPipe(
		int const LoopNum,
		int const LoopSide,
		Real64 const TankOutletTemp
	);

	void
	SetupCommonPipes();

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

} // HVACInterfaceManager

} // EnergyPlus

#endif
