#ifndef PlantLoopSolver_hh_INCLUDED
#define PlantLoopSolver_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantLoopSolver {

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DEFINITIONS
	extern Real64 InitialDemandToLoopSetPoint;
	extern Real64 CurrentAlterationsToDemand;
	extern Real64 UpdatedDemandToLoopSetPoint;
	extern Real64 LoadToLoopSetPointThatWasntMet; // Unmet Demand
	extern Real64 InitialDemandToLoopSetPointSAVED;
	extern int RefrigIndex; // Index denoting refrigerant used (possibly steam)

	// SUBROUTINE SPECIFICATIONS:
	//PRIVATE EvaluatePumpFlowConditions

	// Types

	struct Location
	{
		// Members
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		Location() :
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		Location(
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum
		) :
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct m_FlowControlValidator
	{
		// Members
		bool Valid; // Assume true
		Location ErrorPoint; // Branch where the error was thrown
		std::string Reason; // Brief description of error

		// Default Constructor
		m_FlowControlValidator() :
			Valid( true )
		{}

		// Member Constructor
		m_FlowControlValidator(
			bool const Valid, // Assume true
			Location const & ErrorPoint, // Branch where the error was thrown
			std::string const & Reason // Brief description of error
		) :
			Valid( Valid ),
			ErrorPoint( ErrorPoint ),
			Reason( Reason )
		{}

	};

	// Functions

	void
	PlantHalfLoopSolver(
		bool const FirstHVACIteration, // TRUE if First HVAC iteration of Time step
		int const LoopSideNum,
		int const LoopNum,
		bool & ReSimOtherSideNeeded
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//================= TOPOLOGY VALIDATION ROUTINE ====================!
	//==================================================================!

	m_FlowControlValidator
	ValidateFlowControlPaths(
		int const LoopNum,
		int const LoopSideNum
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//==================== PREDICT LOOP FLOW ===========================!
	//==================================================================!

	void
	SetupLoopFlowRequest(
		int const LoopNum,
		int const ThisSide,
		int const OtherSide,
		Real64 & LoopFlow // Once all flow requests are evaluated, this is the desired flow on this side
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//================== LOOPSIDE BRANCH SIMULATION ====================!
	//==================================================================!

	void
	SimulateAllLoopSideBranches(
		int const LoopNum,
		int const LoopSideNum,
		Real64 const ThisLoopSideFlow,
		bool const FirstHVACIteration,
		bool & LoopShutDownFlag
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//================ SINGLE BRANCH GROUP SIMULATION ==================!
	//==================================================================!

	void
	SimulateLoopSideBranchGroup(
		int const LoopNum,
		int const LoopSideNum,
		int const FirstBranchNum,
		int const LastBranchNum,
		Real64 const FlowRequest,
		bool const FirstHVACIteration,
		bool & LoopShutDownFlag,
		bool const StartingNewLoopSidePass = false
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//==================== SIMULATE LOOP SIDE PUMPS ====================!
	//==================================================================!

	void
	SimulateAllLoopSidePumps(
		int const LoopNum,
		int const ThisSide,
		Optional< Location const > SpecificPumpLocation = _,
		Optional< Real64 const > SpecificPumpFlowRate = _
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//============ EVALUATE LOAD REQUIRED FOR WHOLE LOOP ===============!
	//==================================================================!

	Real64
	CalcOtherSideDemand(
		int const LoopNum,
		int const ThisSide
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//========= EVALUATE LOAD REQUIRED TO MEET LOOP SETPOINT ===========!
	//==================================================================!

	Real64
	EvaluateLoopSetPointLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const FirstBranchNum,
		int const LastBranchNum,
		Array1S_int LastComponentSimulated
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	void
	UpdateAnyLoopDemandAlterations(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//=================== FLOW RESOLVER ROUTINE ========================!
	//==================================================================!

	void
	ResolveParallelFlows(
		int const LoopNum, // plant loop number that we are balancing flow for
		int const LoopSideNum, // plant loop number that we are balancing flow for
		Real64 const ThisLoopSideFlow, // [kg/s]  total flow to be split
		bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	void
	PropagateResolvedFlow(
		int const LoopNum,
		int const LoopSideNum,
		bool const FirstHVACIteration
	);

	//==================================================================!
	//================= EVALUATING BRANCH REQUEST ======================!
	//==================================================================!

	Real64
	DetermineBranchFlowRequest(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	void
	PushBranchFlowCharacteristics(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		Real64 const ValueToPush,
		bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
	);

	//==================================================================!
	//================== REPORT VARIABLE UPDATE ========================!
	//==================================================================!

	void
	UpdateLoopSideReportVars(
		int const LoopNum,
		int const LoopSide,
		Real64 const OtherSideDemand, // This is the 'other side' demand, based on other side flow
		Real64 const LocalRemLoopDemand // Unmet Demand after equipment has been simulated (report variable)
	);

	void
	CalcUnmetPlantDemand(
		int const LoopNum,
		int const LoopSideNum
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//==================================================================!
	//================ VERIFYING LOOP EXIT NODE STATE ==================!
	//==================================================================!

	void
	CheckLoopExitNode(
		int const LoopNum, // plant loop counter
		bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
	);

	void
	AdjustPumpFlowRequestByEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		Real64 & FlowToRequest
	);

	//==================================================================!
	//==================================================================!
	//==================================================================!

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // PlantLoopSolver

} // EnergyPlus

#endif
