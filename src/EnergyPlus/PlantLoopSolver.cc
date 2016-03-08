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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <PlantLoopSolver.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataConvergParams.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HVACInterfaceManager.hh>
#include <PlantCondLoopOperation.hh>
#include <PlantLoopEquip.hh>
#include <PlantPressureSystem.hh>
#include <PlantUtilities.hh>
#include <Pumps.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantLoopSolver {

	// MODULE INFORMATION:
	//       AUTHOR         B. Griffith,  Dan Fisher, Sankaranarayanan K P, Rich Liesen, Edwin Lee
	//       DATE WRITTEN   Feb 2010
	//         This file developed from PlantSupplySideSolvers.cc by Sankaranarayanan K P, Rich Liesen, Dan Fisher
	//       MODIFIED       na
	//       RE-ENGINEERED  Aug 2010 Edwin Lee

	// PURPOSE OF THIS MODULE:
	// This module contains subroutines to solve plant half loops of various configurations.

	// METHODOLOGY EMPLOYED:
	// Main worker calling driver for plant loop system model
	// Calls various worker routines to model flow rates around a plant half loop
	// The procedural flow depends on the pump(s), loop side, and operation scheme at the time (and current flow lock?)

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DEFINITIONS
	Real64 InitialDemandToLoopSetPoint;
	Real64 CurrentAlterationsToDemand;
	Real64 UpdatedDemandToLoopSetPoint;
	Real64 LoadToLoopSetPointThatWasntMet; // Unmet Demand
	Real64 InitialDemandToLoopSetPointSAVED;
	int RefrigIndex( 0 ); // Index denoting refrigerant used (possibly steam)

	static std::string const fluidNameSteam( "STEAM" );
	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool EstablishedCompPumpIndeces( false );
	}
	// SUBROUTINE SPECIFICATIONS:
	//PRIVATE EvaluatePumpFlowConditions

	//==================================================================!
	//=================== HYDRONIC HALF-LOOP SOLVER ====================!
	//==================================================================!

	// Functions
	void
	clear_state()
	{
		InitialDemandToLoopSetPoint = 0.0;
		CurrentAlterationsToDemand = 0.0;
		UpdatedDemandToLoopSetPoint = 0.0;
		LoadToLoopSetPointThatWasntMet = 0.0; // Unmet Demand
		InitialDemandToLoopSetPointSAVED = 0.0;
		RefrigIndex = 0 ; // Index denoting refrigerant used (possibly steam)
		EstablishedCompPumpIndeces = false;
	}


	void
	PlantHalfLoopSolver(
		bool const FirstHVACIteration, // TRUE if First HVAC iteration of Time step
		int const LoopSideNum,
		int const LoopNum,
		bool & ReSimOtherSideNeeded
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHORS:         Dan Fisher, Sankaranarayanan K P, Edwin Lee
		//       DATE WRITTEN:    April 1998
		//       MODIFIED         June 2005(Work in the Plant Super Manager Module)
		//                        July 2006
		//       RE-ENGINEERED    July 2010

		// PURPOSE OF THIS SUBROUTINE:
		// SimSupplyFlowSolution is the driver routine for plant loops.  It performs
		//  the following tasks for each half loop (supply or demand side):
		// 1. Calculate flow request for half loop
		// 2. Predict Loop Flow
		// 3. Simulate the inlet branch
		// 4. Simulate the parallel branches, distributing load if necessary
		// 5. Set flow rates on parallel branches
		// 6. Simulate outlet branch and update node and report variables

		// METHODOLOGY EMPLOYED:
		// The algorithm operates on a predictor/corrector flow setting method by simulating all available loop components
		// based on component requested flow rates, then enforcing continuity on all loop branch flows by calling
		// the flow resolver and locking those flows down.  Available components are then re-simulated using the
		// corrected flow rates.

		// Using/Aliasing
		using HVACInterfaceManager::UpdatePlantLoopInterface;
		using PlantCondLoopOperation::InitLoadDistribution;

		using PlantPressureSystem::SimPressureDropSystem;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataPlant::FlowPumpQuery;
		using DataPlant::FlowUnlocked;
		using DataPlant::FlowLocked;
		using DataPlant::PressureCall_Update;
		using DataPlant::PlantLoop;
		using DataPlant::PressureCall_Init;
		using General::RoundSigDigits;
		using DataLoopNode::Node;
		using DataGlobals::BeginTimeStepFlag;
		using PlantUtilities::BoundValueToWithinTwoValues;
		using PlantUtilities::BoundValueToNodeMinMaxAvail;
		using PlantUtilities::TightenNodeMinMaxAvails;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//~ Topology variables
		int ThisSideInletNode; // Plant loop side loop inlet
		int ThisSide;
		int OtherSide;

		//~ Initialization and validation flags

		//~ Flags
		bool LoopShutDownFlag;

		//~ Other variables
		Real64 ThisLoopSideFlow;
		Real64 TotalPumpMaxAvailFlow;
		Real64 TotalPumpMinAvailFlow;

		// Object Data
		m_FlowControlValidator IsLoopSideValid;

		// Initialize variables
		InitialDemandToLoopSetPoint = 0.0;
		CurrentAlterationsToDemand = 0.0;
		UpdatedDemandToLoopSetPoint = 0.0;
		ThisSide = LoopSideNum;
		OtherSide = 3 - ThisSide; //will give us 1 if thisside is 2, or 2 if thisside is 1
		LoopShutDownFlag = false;
		ThisSideInletNode = PlantLoop( LoopNum ).LoopSide( ThisSide ).NodeNumIn;

		// The following block is related to validating the flow control paths of the loop side
		// Since the control types are scheduled, I think BeginTimeStep should be a decent check frequency
		if ( BeginTimeStepFlag && PlantLoop( LoopNum ).LoopSide( ThisSide ).OncePerTimeStepOperations ) {

			// Initialize loop side controls -- could just be done for one loop since this routine inherently
			//  loops over all plant/condenser loops.  Not sure if the penalty is worth investigating.
			InitLoadDistribution( FirstHVACIteration );

			// Now that the op scheme types are updated, do LoopSide validation
			IsLoopSideValid = ValidateFlowControlPaths( LoopNum, ThisSide );
			if ( ! IsLoopSideValid.Valid ) {
				ShowFatalError( "ERROR:" + IsLoopSideValid.Reason );
			}

			// Set the flag to false so we won't do these again this time step
			PlantLoop( LoopNum ).LoopSide( ThisSide ).OncePerTimeStepOperations = false;

		} else {

			// Set the flag to true so that it is activated for the next time step
			PlantLoop( LoopNum ).LoopSide( ThisSide ).OncePerTimeStepOperations = true;

		}

		// Do pressure system initialize if this is the demand side (therefore once per whole loop)
		if ( ThisSide == DemandSide ) SimPressureDropSystem( LoopNum, FirstHVACIteration, PressureCall_Init );

		// First thing is to setup mass flow request information
		SetupLoopFlowRequest( LoopNum, ThisSide, OtherSide, ThisLoopSideFlow );

		// Now we know what the loop would "like" to run at, let's see the pump
		// operation range (min/max avail) to see whether it is possible this time around
		if ( allocated( PlantLoop( LoopNum ).LoopSide( ThisSide ).Pumps ) ) {

			//~ Initialize pump values
			for ( auto & e : PlantLoop( LoopNum ).LoopSide( ThisSide ).Pumps ) {
				e.CurrentMinAvail = 0.0;
				e.CurrentMaxAvail = 0.0;
			}
			PlantLoop( LoopNum ).LoopSide( ThisSide ).FlowLock = FlowPumpQuery;

			//~ Simulate pumps
			SimulateAllLoopSidePumps( LoopNum, ThisSide );

			//~ Calculate totals
			TotalPumpMinAvailFlow = TotalPumpMaxAvailFlow = 0.0;
			for ( auto const & e : PlantLoop( LoopNum ).LoopSide( ThisSide ).Pumps ) {
				TotalPumpMinAvailFlow += e.CurrentMinAvail;
				TotalPumpMaxAvailFlow += e.CurrentMaxAvail;
			}

			// Use the pump min/max avail to attempt to constrain the loop side flow
			ThisLoopSideFlow = BoundValueToWithinTwoValues( ThisLoopSideFlow, TotalPumpMinAvailFlow, TotalPumpMaxAvailFlow );

		}

		// Now we check flow restriction from the other side, both min and max avail.
		// Doing this last basically means it wins, so the pump should pull down to meet the flow restriction
		ThisLoopSideFlow = BoundValueToNodeMinMaxAvail( ThisLoopSideFlow, ThisSideInletNode );

		// Final preparation of loop inlet min/max avail if pumps exist
		if ( allocated( PlantLoop( LoopNum ).LoopSide( ThisSide ).Pumps ) ) {
			// At this point, the pump limits should have been obeyed unless a flow restriction was encountered from the other side
			// The pump may, however, have even tighter constraints than the other side
			// At this point, the inlet node doesn't know anything about those limits
			// Since we have already honored the other side flow restriction, try to honor the pump limits here
			TightenNodeMinMaxAvails( ThisSideInletNode, TotalPumpMinAvailFlow, TotalPumpMaxAvailFlow );
		}

		// Now reset the entering mass flow rate to the decided-upon flow rate
		Node( ThisSideInletNode ).MassFlowRate = ThisLoopSideFlow;

		// We also need to establish a baseline "other-side-based" loop demand based on this possible flow rate
		InitialDemandToLoopSetPoint = CalcOtherSideDemand( LoopNum, ThisSide );
		UpdatedDemandToLoopSetPoint = InitialDemandToLoopSetPoint;

		LoadToLoopSetPointThatWasntMet = 0.0;

		// We now have a loop side flow request, along with inlet min/max avails.
		// We can now make a first pass through the component simulation, requesting flow as necessary.
		// Normal "supply side" components will set a mass flow rate on their outlet node to request flow,
		// while "Demand side" components will set a a mass flow request on their inlet node to request flow.
		PlantLoop( LoopNum ).LoopSide( ThisSide ).FlowLock = FlowUnlocked;
		SimulateAllLoopSideBranches( LoopNum, ThisSide, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag );

		// DSU? discussion/comments about loop solver/flow resolver interaction
		// At this point, the components have been simulated.  They should have either:
		//  - logged a massflowrequest
		//  - or logged a MassFlowRate
		// We need to decide what the components are going to do on FlowLock=0.
		// If we want all control here at the solver level, the components just need to
		//  log their MassFlowRate on their outlet nodes, or some other mechanism.
		// Then the loop solver can scan the branch and get the max, and this will be the requested
		//  flow rate for the branch.
		// The loop solver will then set this as the branch outlet mass flow rate in preparation
		//  for the flow resolver.
		// The loop solver may need to do something to the inlet/outlet branch, but I'm not sure yet.
		// The following comment block is what I had already thought of, and it may still make sense.

		// Now that all the flow requests have been logged, we need to prepare them for the
		//  flow resolver.  This will just take the requests and determine the desired flow
		//  request for that branch according to pump placement, pump type, and other component
		//  conditions.  In many cases, this will just be to simply take the max request from
		//  the branch, which will already be within pumping limits for that flow path.
		// We can then call the flow resolver to lock down branch inlet flow rates.
		//DSU?

		// The flow resolver takes information such as requested flows and min/max available flows and
		//  sets the corrected flow on the inlet to each parallel branch
		ResolveParallelFlows( LoopNum, ThisSide, ThisLoopSideFlow, FirstHVACIteration );
		//  CALL PropagateResolvedFlow(LoopNum, ThisSide)

		// Re-Initialize variables for this next pass
		InitialDemandToLoopSetPointSAVED = InitialDemandToLoopSetPoint;
		CurrentAlterationsToDemand = 0.0;
		UpdatedDemandToLoopSetPoint = InitialDemandToLoopSetPoint;

		// Now that flow rates have been resolved, we just need to set the flow lock status
		//  flag, and resimulate.  During this simulation each component will still use the
		//  SetFlowRequest routine, but this routine will also set the outlet flow rate
		//  equal to the inlet flow rate, accoridng to flowlock logic.
		PlantLoop( LoopNum ).LoopSide( ThisSide ).FlowLock = FlowLocked;
		SimulateAllLoopSideBranches( LoopNum, ThisSide, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag );

		// A couple things are specific to which LoopSide we are on
		if ( LoopSideNum == DemandSide ) {

			// Pass the loop information via the HVAC interface manager
			UpdatePlantLoopInterface( LoopNum, LoopSideNum, PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumOut, PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn, ReSimOtherSideNeeded, PlantLoop( LoopNum ).CommonPipeType );

		} else { //LoopSide == SupplySide

			// Update pressure drop reporting, calculate total loop pressure drop for use elsewhere
			SimPressureDropSystem( LoopNum, FirstHVACIteration, PressureCall_Update );

			// Pass the loop information via the HVAC interface manager (only the flow)
			UpdatePlantLoopInterface( LoopNum, LoopSideNum, PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut, PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn, ReSimOtherSideNeeded, PlantLoop( LoopNum ).CommonPipeType );

			// Update the loop outlet node conditions
			CheckLoopExitNode( LoopNum, FirstHVACIteration );

		}

		// Update some reporting information at Plant half loop level
		UpdateLoopSideReportVars( LoopNum, LoopSideNum, InitialDemandToLoopSetPointSAVED, LoadToLoopSetPointThatWasntMet );

	}

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
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine will scan all the loop side paths and validate the component topology according
		//  to current topology rules and regulations.

		// METHODOLOGY EMPLOYED:
		// Scan this loop side and begin by scanning the first branch, then follow with the remainder of the flow paths
		//  - this would be from splitter outlet nodes all the way to the loop side outlet node.
		// The current rules are that "other types" of components (as defined below in the references) can be placed along each
		//  flow path as needed.  At this point, any number of "load-range based" components can be placed along the flow
		//  path.  After this, the user is allowed to place another set of any number of "other types" of components.
		// The key restriction is that an "other type" of component may not be sandwiched by "load-range based" components.
		// This is due to the load range based needing to be simulated all at once along each flow path.

		// REFERENCES:
		// "other types" of components: basically not load-range based heat transfer components.  This would include:
		//    - demand based components such as coils
		//    - component setpoint based operating components
		//    - heat exchanger components including waterside economizers
		// "load-range based" components are heat transfer components which are controlled based on a single load range.
		//    - currently only one load range based scheme is available at a given time, although other control types
		//      may be enabled, such as component setpoint.
		// Pumps are separate components since the pump heat is not accounted for in the flow path order.
		//  Improvements during the demand side rewrite has allowed pumps to be placed as -not- the first component on a branch
		//  Pumps can be placed anywhere, even between load-range based components, since they will not affect loop load

		// RETURN VALUE:
		// Returns a control validator flow structure, including a flag for successful or not, then if not successful
		//  the other values are filled in such as location on the loop where the error occurred and a message error description

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::LoadRangeBasedMin;
		using DataPlant::LoadRangeBasedMax;
		using DataPlant::PumpOpSchemeType;
		using DataPlant::NoControlOpSchemeType;
		using DataPlant::UnknownStatusOpSchemeType;
		using DataLoopNode::Node;

		// Return value
		m_FlowControlValidator ValidLoopSide;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const Parallel( 1 );
		int const Outlet( 2 );

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		//~ Indexing variables
		int BranchIndex;
		int CompIndex;
		int NumParallelPaths;
		int PathCounter;
		int ParallelOrOutletIndex;

		//~ General variables
		bool EncounteredLRB;
		bool EncounteredNonLRBAfterLRB;

		// set up a loopside reference
		auto & this_loop_side( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );

		//~ Initialze
		ValidLoopSide.Valid = true;
		EncounteredLRB = false;
		EncounteredNonLRBAfterLRB = false;
		NumParallelPaths = this_loop_side.TotalBranches - 2;

		// We'll start by stepping through the first branch, which may be the only branch
		// If we find a load range based, then trip the flag and just keep moving
		// If we only have one branch and all is good, then RETURN early
		// If we have parallel branches, then start looping through each flow path to
		//  decide if it is a valid path.
		// If any one path is invalid then all is wrong
		BranchIndex = 1;
		for ( CompIndex = 1; CompIndex <= this_loop_side.Branch( BranchIndex ).TotalComponents; ++CompIndex ) {

			auto & this_component( this_loop_side.Branch( BranchIndex ).Comp( CompIndex ) );

			{ auto const SELECT_CASE_var( this_component.CurOpSchemeType );

			if ( ( SELECT_CASE_var >= LoadRangeBasedMin ) && ( SELECT_CASE_var <= LoadRangeBasedMax ) ) { //~ load range based
				if ( EncounteredNonLRBAfterLRB ) {
					// We must have already encountered a LRB, then a non-LRB, and now another LRB, this is bad
					ValidLoopSide.Valid = false;
					ValidLoopSide.ErrorPoint.LoopNum = LoopNum;
					ValidLoopSide.ErrorPoint.LoopSideNum = LoopSideNum;
					ValidLoopSide.ErrorPoint.BranchNum = BranchIndex;
					ValidLoopSide.ErrorPoint.CompNum = CompIndex;
					ValidLoopSide.Reason = "Invalid: Load range based components are separated by other control type components. Load Range Based should be grouped together on each flow path.";
					return ValidLoopSide;
				} else {
					EncounteredLRB = true;
				}

			} else if ( SELECT_CASE_var == PumpOpSchemeType ) { //~ pump
				// For now this is just a placeholder, because I think pumps will be available anywhere,
				//  and they won't affect the load distribution

			} else if ( SELECT_CASE_var == NoControlOpSchemeType ) { //~ Such as pipes
				// For now this is just a placeholder, because these components shouldn't cause a problem anywhere...

			} else if ( SELECT_CASE_var == UnknownStatusOpSchemeType ) { //~ Uninitialized, this should be a sufficient place to catch for this on branch 1
				//throw fatal
				ShowSevereError( "ValidateFlowControlPaths: Uninitialized operation scheme type for component Name: " + this_component.Name );
				ShowFatalError( "ValidateFlowControlPaths: developer notice, Inlet path validation loop" );
			} else { //~ Other control type
				if ( EncounteredLRB ) {
					EncounteredNonLRBAfterLRB = true;
				} else {
					// For now don't do anything, but we'll see...
				}

			}}

		}

		// Return early if we only needed to do the one branch
		if ( NumParallelPaths <= 0 ) return ValidLoopSide;

		// Now, if we have multiple parallel branches, I think the easiest way is to go all the way from the inlet node
		//  of each parallel branch to the loop outlet node and check the flow path
		// This way we don't have to remember the conditions on each of the parallel branches when we would finally move
		//  to analyzing the outlet node when all done
		// This will reduce allocation on the heap because we will keep from storing that array
		// For each parallel path, we will need to check two branches: the parallel branch and the LoopSide outlet branch
		for ( PathCounter = 1; PathCounter <= NumParallelPaths; ++PathCounter ) {
			for ( ParallelOrOutletIndex = Parallel; ParallelOrOutletIndex <= Outlet; ++ParallelOrOutletIndex ) {
				if ( ParallelOrOutletIndex == Parallel ) {
					// The branch index will be the current pathtype + 1 to add the inlet branch
					BranchIndex = PathCounter + 1;
				} else if ( ParallelOrOutletIndex == Outlet ) {
					// The branch index will be the LoopSide outlet node
					BranchIndex = this_loop_side.TotalBranches;
				}

				// Now that we have the branch index, let's do the control type check over all the components
				for ( CompIndex = 1; CompIndex <= this_loop_side.Branch( BranchIndex ).TotalComponents; ++CompIndex ) {

					auto & this_component( this_loop_side.Branch( BranchIndex ).Comp( CompIndex ) );

					{ auto const SELECT_CASE_var( this_component.CurOpSchemeType );

					if ( ( SELECT_CASE_var >= LoadRangeBasedMin ) && ( SELECT_CASE_var <= LoadRangeBasedMax ) ) { //~ load range based
						if ( EncounteredNonLRBAfterLRB ) {
							// We must have already encountered a LRB, then a non-LRB, and now another LRB, this is bad
							ValidLoopSide.Valid = false;
							ValidLoopSide.ErrorPoint.LoopNum = LoopNum;
							ValidLoopSide.ErrorPoint.LoopSideNum = LoopSideNum;
							ValidLoopSide.ErrorPoint.BranchNum = BranchIndex;
							ValidLoopSide.ErrorPoint.CompNum = CompIndex;
							ValidLoopSide.Reason = "Invalid: Load range based components are separated by other control type components. Load Range Based should be grouped together on each flow path.";
							return ValidLoopSide;
						} else {
							EncounteredLRB = true;
						}

					} else if ( SELECT_CASE_var == NoControlOpSchemeType ) { //~ Such as pipes
						// For now this is just a placeholder, because these components shouldn't cause a problem anywhere...

					} else if ( SELECT_CASE_var == PumpOpSchemeType ) { //~ pump
						// For now this is just a placeholder, because I think pumps will be available anywhere,
						//  and they won't affect the load distribution

					} else if ( SELECT_CASE_var == UnknownStatusOpSchemeType ) { //~ Uninitialized, this should be sufficient place to catch for this on other branches
						//throw fatal error
						ShowSevereError( "ValidateFlowControlPaths: Uninitialized operation scheme type for component Name: " + this_component.Name );
						ShowFatalError( "ValidateFlowControlPaths: developer notice, problem in Parallel path validation loop" );
					} else { //~ Other control type
						if ( EncounteredLRB ) {
							EncounteredNonLRBAfterLRB = true;
						} else {
							// For now don't do anything, but we'll see...
						}

					}}

				} //~ CompIndex

			} //~ Parallel and Outlet Branches

		} //~ Parallel Paths

		return ValidLoopSide;

	}

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
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR:          Dan Fisher, Edwin Lee
		//       DATE WRITTEN:    August 2010
		//       MODIFIED:        na
		//       RE-ENGINEERED:   na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine sets up the flow request values and sums them up for each loop side
		// Then makes a decision on the desired loop flow based on loop configuration

		// METHODOLOGY EMPLOYED:
		// Scan through the components on this loop side, and look at the mass flow request
		//  values on components inlet node.
		// Check common pipe/pumping configuration for this loop side and the other loop side
		//  to determine what the LoopSide should flow

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::CommonPipe_No;
		using DataPlant::LoadRangeBasedMin;
		using DataPlant::LoadRangeBasedMax;
		using DataPlant::LoopFlowStatus_Unknown;
		using DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using DataPlant::LoopFlowStatus_TakesWhatGets;
		using DataPlant::TotNumLoops;
		using DataPlant::GenEquipTypes_Pump;
		using DataPlant::TypeOf_PumpConstantSpeed;
		using DataPlant::TypeOf_PumpBankConstantSpeed;
		using DataPlant::TypeOf_PumpCondensate;
		using DataPlant::SupplySide;
		using DataPlant::CommonPipe_TwoWay;
		using DataPlant::DemandSide;
		using DataPlant::CommonPipe_Single;
		using DataPlant::TypeOf_PumpVariableSpeed;
		using DataPlant::TypeOf_PumpBankVariableSpeed;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using Pumps::PumpEquip;
		using PlantUtilities::IntegerIsWithinTwoValues;
		using DataHVACGlobals::SmallLoad;
		using DataConvergParams::PlantLowFlowRateToler;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS
		int LoopCounter;
		int LoopSideCounter;
		int BranchCounter;
		int CompCounter;
		int CompIndex;
		int NumBranchesOnThisLoopSide;
		int NumCompsOnThisBranch;
		int NodeToCheckRequest;
		Real64 ThisBranchFlowRequestNeedAndTurnOn;
		Real64 ThisBranchFlowRequestNeedIfOn;
		Real64 InletBranchRequestNeedAndTurnOn;
		Real64 InletBranchRequestNeedIfOn;
//		static Array2D< Real64 > LoadedConstantSpeedBranchFlowRateSteps; // Values never used
		static Array2D< Real64 > NoLoadConstantSpeedBranchFlowRateSteps;
		int ParallelBranchIndex;
		Real64 OutletBranchRequestNeedAndTurnOn;
		Real64 OutletBranchRequestNeedIfOn;
		bool ThisSideHasPumps;
		bool OtherSideHasPumps;
		bool ThisLoopHasCommonPipe( false );

		//Tuned Made static: Set before use
		static Array1D_bool ThisLoopHasConstantSpeedBranchPumps( 2 );
		static Array1D< Real64 > EachSideFlowRequestNeedAndTurnOn( 2 ); // 2 for SupplySide/DemandSide
		static Array1D< Real64 > EachSideFlowRequestNeedIfOn( 2 ); // 2 for SupplySide/DemandSide
		static Array1D< Real64 > EachSideFlowRequestFinal( 2 ); // 2 for SupplySide/DemandSide

		static bool AllocatedParallelArray( false );
		int MaxParallelBranchCount;
		int FlowPriorityStatus;
		Real64 tmpLoopFlow;
		Real64 AccumFlowSteps;
		Real64 MaxBranchPumpLoopSideFlow;

		//~ One time init for array allocated
		if ( ! AllocatedParallelArray ) {
			MaxParallelBranchCount = 0;
			for ( LoopCounter = 1; LoopCounter <= TotNumLoops; ++LoopCounter ) {
				for ( LoopSideCounter = 1; LoopSideCounter <= 2; ++LoopSideCounter ) {
					MaxParallelBranchCount = max( MaxParallelBranchCount, PlantLoop( LoopCounter ).LoopSide( LoopSideCounter ).TotalBranches - 2 );
				}
			}
//			LoadedConstantSpeedBranchFlowRateSteps.allocate( MaxParallelBranchCount, 2 );
			NoLoadConstantSpeedBranchFlowRateSteps.allocate( MaxParallelBranchCount, 2 );
			AllocatedParallelArray = true;
		}

		//~ Initialize
		LoopFlow = 0.0;
		ThisLoopHasConstantSpeedBranchPumps = false;
		EachSideFlowRequestNeedAndTurnOn = 0.0;
		EachSideFlowRequestNeedIfOn = 0.0;
		EachSideFlowRequestFinal = 0.0;
		//  AtLeastOneNonLRBRequested       = .FALSE.

		// reference
		auto & loop( PlantLoop( LoopNum ) );

		//~ First we need to set up the flow requests on each LoopSide
		for ( LoopSideCounter = DemandSide; LoopSideCounter <= SupplySide; ++LoopSideCounter ) {
			// Clear things out for this LoopSide
			InletBranchRequestNeedAndTurnOn = 0.0;
			InletBranchRequestNeedIfOn = 0.0;
			Real64 ParallelBranchRequestsNeedAndTurnOn( 0.0 );
			Real64 ParallelBranchRequestsNeedIfOn( 0.0 );
			OutletBranchRequestNeedAndTurnOn = 0.0;
			OutletBranchRequestNeedIfOn = 0.0;
			EachSideFlowRequestNeedAndTurnOn( LoopSideCounter ) = 0.0;
			EachSideFlowRequestNeedIfOn( LoopSideCounter ) = 0.0;

			// reference
			auto & loop_side( loop.LoopSide( LoopSideCounter ) );

			// Now loop through all the branches on this LoopSide and get flow requests
			NumBranchesOnThisLoopSide = loop_side.TotalBranches;
			ParallelBranchIndex = 0;
			for ( BranchCounter = 1; BranchCounter <= NumBranchesOnThisLoopSide; ++BranchCounter ) {
				ThisBranchFlowRequestNeedAndTurnOn = 0.0;
				ThisBranchFlowRequestNeedIfOn = 0.0;

				// reference
				auto & branch( loop_side.Branch( BranchCounter ) );

				if ( BranchCounter > 1 && BranchCounter < NumBranchesOnThisLoopSide ) ++ParallelBranchIndex;
				NumCompsOnThisBranch = branch.TotalComponents;
				for ( CompCounter = 1; CompCounter <= NumCompsOnThisBranch; ++CompCounter ) {

					// reference
					auto & component( branch.Comp( CompCounter ) );

					NodeToCheckRequest = component.NodeNumIn;
					FlowPriorityStatus = component.FlowPriority;

					// reference
					auto & node_with_request( Node( NodeToCheckRequest ) );

					if ( component.GeneralEquipType != GenEquipTypes_Pump ) {

						if ( FlowPriorityStatus == LoopFlowStatus_Unknown ) {
							// do nothing
						} else if ( FlowPriorityStatus == LoopFlowStatus_NeedyAndTurnsLoopOn ) {
							ThisBranchFlowRequestNeedAndTurnOn = max( ThisBranchFlowRequestNeedAndTurnOn, node_with_request.MassFlowRateRequest );
							ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, node_with_request.MassFlowRateRequest );
						} else if ( FlowPriorityStatus == LoopFlowStatus_NeedyIfLoopOn ) {
							ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, node_with_request.MassFlowRateRequest );
						} else if ( FlowPriorityStatus == LoopFlowStatus_TakesWhatGets ) {
							// do nothing
						}
					} else { // handle pumps differently
						if ( ( BranchCounter == 1 ) && ( LoopSideCounter == SupplySide ) && ( loop.CommonPipeType == CommonPipe_TwoWay ) ) {
							// special primary side flow request for two way common pipe
							CompIndex = component.CompNum;
							{ auto const SELECT_CASE_var( component.TypeOf_Num );
							// remove var speed pumps from this case statement if can set MassFlowRateRequest
							if ( ( SELECT_CASE_var == TypeOf_PumpConstantSpeed ) || ( SELECT_CASE_var == TypeOf_PumpVariableSpeed ) || ( SELECT_CASE_var == TypeOf_PumpBankVariableSpeed ) ) {
								if ( CompIndex > 0 ) {
									ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, PumpEquip( CompIndex ).MassFlowRateMax );
								}
							} else if ( SELECT_CASE_var == TypeOf_PumpBankConstantSpeed ) {
								if ( CompIndex > 0 ) {
									ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, PumpEquip( CompIndex ).MassFlowRateMax / PumpEquip( CompIndex ).NumPumpsInBank );
								}
							} else {
								ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, node_with_request.MassFlowRateRequest );
							}}

						} else if ( ( BranchCounter == 1 ) && ( LoopSideCounter == SupplySide ) && ( loop.CommonPipeType == CommonPipe_Single ) ) {
							CompIndex = component.CompNum;
							{ auto const SELECT_CASE_var( component.TypeOf_Num );
							// remove var speed pumps from this case statement if can set MassFlowRateRequest
							if ( ( SELECT_CASE_var == TypeOf_PumpConstantSpeed ) || ( SELECT_CASE_var == TypeOf_PumpVariableSpeed ) || ( SELECT_CASE_var == TypeOf_PumpBankVariableSpeed ) ) {
								if ( CompIndex > 0 ) {
									ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, PumpEquip( CompIndex ).MassFlowRateMax );
								}
							} else if ( SELECT_CASE_var == TypeOf_PumpBankConstantSpeed ) {
								if ( CompIndex > 0 ) {
									ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, PumpEquip( CompIndex ).MassFlowRateMax / PumpEquip( CompIndex ).NumPumpsInBank );
								}
							} else {
								ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, node_with_request.MassFlowRateRequest );
							}}
						} else {
							CompIndex = component.CompNum;
							{ auto const SELECT_CASE_var( component.TypeOf_Num );
							if ( SELECT_CASE_var == TypeOf_PumpConstantSpeed ) {
								if ( CompIndex > 0 ) {
									auto & this_pump( PumpEquip( CompIndex ) );
									if ( ParallelBranchIndex >= 1 ) { // branch pump
										if ( branch.max_abs_Comp_MyLoad() > SmallLoad ) { //Autdesk:Tuned any( abs( Comp.MyLoad() ) > SmallLoad ) replaced for efficiency
											ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, this_pump.MassFlowRateMax );
										} else if ( loop.CommonPipeType != CommonPipe_No ) { // common pipe and constant branch pumps
											ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, this_pump.MassFlowRateMax );
										}
										ThisLoopHasConstantSpeedBranchPumps( LoopSideCounter ) = true;
										branch.HasConstantSpeedBranchPump = true;
										branch.ConstantSpeedBranchMassFlow = this_pump.MassFlowRateMax;
									} else { // inlet pump
										ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, this_pump.MassFlowRateMax );
									}
								}
							} else if ( SELECT_CASE_var == TypeOf_PumpBankConstantSpeed ) {
								if ( CompIndex > 0 ) {
									auto & this_pump( PumpEquip( CompIndex ) );
									if ( ParallelBranchIndex >= 1 ) { // branch pump
										if ( branch.max_abs_Comp_MyLoad() > SmallLoad ) { //Autdesk:Tuned any( abs( Comp.MyLoad() ) > SmallLoad ) replaced for efficiency
											ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, this_pump.MassFlowRateMax / this_pump.NumPumpsInBank );
										} else if ( loop.CommonPipeType != CommonPipe_No ) { // common pipe and constant branch pumps
											ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, this_pump.MassFlowRateMax / this_pump.NumPumpsInBank );
										}
										ThisLoopHasConstantSpeedBranchPumps( LoopSideCounter ) = true;
										branch.HasConstantSpeedBranchPump = true;
										branch.ConstantSpeedBranchMassFlow = this_pump.MassFlowRateMax / this_pump.NumPumpsInBank;
									} else { // inlet pump
										ThisBranchFlowRequestNeedIfOn = max( ThisBranchFlowRequestNeedIfOn, this_pump.MassFlowRateMax / this_pump.NumPumpsInBank );
									}
								}
							}

							//overwrite here for branch pumps
							if ( ( SELECT_CASE_var == TypeOf_PumpVariableSpeed ) || ( SELECT_CASE_var == TypeOf_PumpBankVariableSpeed ) || ( SELECT_CASE_var == TypeOf_PumpCondensate ) ) {
								CompIndex = component.CompNum;
								if ( CompIndex > 0 ) {
									auto & this_pump(PumpEquip( CompIndex ) );
									this_pump.LoopSolverOverwriteFlag = false;
								}


							}}
						}

					}

				}
				if ( BranchCounter == 1 ) { // inlet branch
					InletBranchRequestNeedAndTurnOn = ThisBranchFlowRequestNeedAndTurnOn;
					InletBranchRequestNeedIfOn = ThisBranchFlowRequestNeedIfOn;
				} else if ( BranchCounter < NumBranchesOnThisLoopSide ) { // branchcounter = 1 is already caught
					ParallelBranchRequestsNeedAndTurnOn += ThisBranchFlowRequestNeedAndTurnOn;
					ParallelBranchRequestsNeedIfOn += ThisBranchFlowRequestNeedIfOn;
				} else if ( BranchCounter == NumBranchesOnThisLoopSide ) { // outlet branch
					OutletBranchRequestNeedAndTurnOn = ThisBranchFlowRequestNeedAndTurnOn;
					OutletBranchRequestNeedIfOn = ThisBranchFlowRequestNeedIfOn;
				}

				branch.RequestedMassFlow = max( ThisBranchFlowRequestNeedIfOn, ThisBranchFlowRequestNeedAndTurnOn );

			}
			EachSideFlowRequestNeedAndTurnOn( LoopSideCounter ) = max( InletBranchRequestNeedAndTurnOn, ParallelBranchRequestsNeedAndTurnOn, OutletBranchRequestNeedAndTurnOn );
			EachSideFlowRequestNeedIfOn( LoopSideCounter ) = max( InletBranchRequestNeedIfOn, ParallelBranchRequestsNeedIfOn, OutletBranchRequestNeedIfOn );
		}

		//~ Now that we have calculated each sides different status's requests, process to find final
		if ( sum( EachSideFlowRequestNeedAndTurnOn ) < MassFlowTolerance ) {
			EachSideFlowRequestFinal = 0.0;
		} else { // some flow is needed and loop should try to run
			EachSideFlowRequestFinal( ThisSide ) = max( EachSideFlowRequestNeedAndTurnOn( ThisSide ), EachSideFlowRequestNeedIfOn( ThisSide ) );
			EachSideFlowRequestFinal( OtherSide ) = max( EachSideFlowRequestNeedAndTurnOn( OtherSide ), EachSideFlowRequestNeedIfOn( OtherSide ) );

		}
		// now store final flow requests on each loop side data structure
		auto & this_loop_side( loop.LoopSide( ThisSide ) );
		auto & other_loop_side( loop.LoopSide( OtherSide ) );
		this_loop_side.FlowRequest = EachSideFlowRequestFinal( ThisSide );
		other_loop_side.FlowRequest = EachSideFlowRequestFinal( OtherSide );

		if ( loop.CommonPipeType == CommonPipe_No ) {
			// we may or may not have a pump on this side, but the flow request is the larger of the two side's final
			if ( ! any( ThisLoopHasConstantSpeedBranchPumps ) ) {
				LoopFlow = maxval( EachSideFlowRequestFinal );
			} else { // account for stepped loop flow rates required of branch pumps

				// rules for setting flow when there are constant speed branch pumps.
				// 1. Check if above routines already selected a loop flow rate based on the constant speed branches, if so then just use it
				if ( ( ThisLoopHasConstantSpeedBranchPumps( ThisSide ) ) && ( EachSideFlowRequestFinal( ThisSide ) >= EachSideFlowRequestFinal( OtherSide ) ) ) {
					// okay, just use basic logic
					LoopFlow = maxval( EachSideFlowRequestFinal );
				} else if ( ( ThisLoopHasConstantSpeedBranchPumps( OtherSide ) ) && ( EachSideFlowRequestFinal( ThisSide ) <= EachSideFlowRequestFinal( OtherSide ) ) ) {
					// okay, just use basic logic
					LoopFlow = maxval( EachSideFlowRequestFinal );
				} else { // not okay, we have a case that will likely need special correcting
					//  2. determine which loop side has the stepped data
					if ( ( ThisLoopHasConstantSpeedBranchPumps( ThisSide ) ) && ( EachSideFlowRequestFinal( ThisSide ) < EachSideFlowRequestFinal( OtherSide ) ) ) {
						LoopSideCounter = ThisSide;
					} else if ( ( ThisLoopHasConstantSpeedBranchPumps( OtherSide ) ) && ( EachSideFlowRequestFinal( OtherSide ) < EachSideFlowRequestFinal( ThisSide ) ) ) {
						LoopSideCounter = OtherSide;
					}
					auto & loop_side( loop.LoopSide( LoopSideCounter ) );

					// 3. step through and find out needed information
					// 3a.  search the loop side with branch pumps and find the steps available with non-zero Myloads
					// 3b.  search the loop side with branch pumps and find the steps available with zero Myloads
//					LoadedConstantSpeedBranchFlowRateSteps = 0.0;
					Real64 LoadedConstantSpeedBranchFlowRateSteps_sum = 0.0;
					NoLoadConstantSpeedBranchFlowRateSteps = 0.0;
					Real64 NoLoadConstantSpeedBranchFlowRateSteps_sum = 0.0;
					ParallelBranchIndex = 0;
					NumBranchesOnThisLoopSide = loop_side.TotalBranches;
					auto const & loop_branches( loop_side.Branch );
					for ( BranchCounter = 1; BranchCounter <= NumBranchesOnThisLoopSide; ++BranchCounter ) {
						auto const & loop_branch( loop_branches( BranchCounter ) );
						if ( BranchCounter > 1 && BranchCounter < NumBranchesOnThisLoopSide ) ++ParallelBranchIndex;
						if ( loop_branch.HasConstantSpeedBranchPump ) {
							auto const branch_mass_flow( loop_branch.ConstantSpeedBranchMassFlow );
							if ( loop_branch.max_abs_Comp_MyLoad() > SmallLoad ) { //Autdesk:Tuned any( abs( Comp.MyLoad() ) > SmallLoad ) replaced for efficiency
//								LoadedConstantSpeedBranchFlowRateSteps( LoopSideCounter, ParallelBranchIndex ) = branch_mass_flow;
								LoadedConstantSpeedBranchFlowRateSteps_sum += branch_mass_flow;
							} else {
								NoLoadConstantSpeedBranchFlowRateSteps( ParallelBranchIndex, LoopSideCounter ) = branch_mass_flow;
								NoLoadConstantSpeedBranchFlowRateSteps_sum += branch_mass_flow;
							}
						}
					}

					// 4. allocate which branches to use,
					tmpLoopFlow = maxval( EachSideFlowRequestFinal );
					AccumFlowSteps = 0.0;
					MaxBranchPumpLoopSideFlow = LoadedConstantSpeedBranchFlowRateSteps_sum + NoLoadConstantSpeedBranchFlowRateSteps_sum;
					tmpLoopFlow = min( tmpLoopFlow, MaxBranchPumpLoopSideFlow );
					//  4b. first use all the branches with non-zero MyLoad
					if ( tmpLoopFlow <= LoadedConstantSpeedBranchFlowRateSteps_sum ) {
						tmpLoopFlow = LoadedConstantSpeedBranchFlowRateSteps_sum;
					} else {
						AccumFlowSteps = LoadedConstantSpeedBranchFlowRateSteps_sum;
						ParallelBranchIndex = 0;
						for ( BranchCounter = 1; BranchCounter <= NumBranchesOnThisLoopSide; ++BranchCounter ) {
							if ( BranchCounter > 1 && BranchCounter < NumBranchesOnThisLoopSide ) {
								++ParallelBranchIndex;
							} else {
								continue;
							}
							auto const steps( NoLoadConstantSpeedBranchFlowRateSteps( ParallelBranchIndex, LoopSideCounter ) );
							if ( steps > 0.0 ) { // add in branches with zero MyLoad  in branch input order until satisfied
								if ( tmpLoopFlow > AccumFlowSteps ) {
									if ( tmpLoopFlow <= AccumFlowSteps + steps ) { // found it set requests and exit
										tmpLoopFlow = AccumFlowSteps + steps;
										loop_side.Branch( BranchCounter ).RequestedMassFlow = steps;
										LoopFlow = tmpLoopFlow;
										break;
									} else {
										AccumFlowSteps += steps;
										loop_side.Branch( BranchCounter ).RequestedMassFlow = steps;
									}
								}
							}
						}
					}
				}
			}
			ThisLoopHasCommonPipe = false;
		} else if ( loop.CommonPipeType == CommonPipe_TwoWay ) {
			LoopFlow = EachSideFlowRequestFinal( ThisSide );
			ThisLoopHasCommonPipe = true;
		} else if ( loop.CommonPipeType == CommonPipe_Single ) {
			LoopFlow = EachSideFlowRequestFinal( ThisSide );
			ThisLoopHasCommonPipe = true;
		}

		// overrides the loop solver flow request to allow loop pump to turn off when not in use
		if ( this_loop_side.TotalPumps == 1) {
			if ( LoopFlow < PlantLowFlowRateToler ) {  //Update from dataconvergetols...
				NumBranchesOnThisLoopSide = this_loop_side.TotalBranches;
				for ( BranchCounter = 1; BranchCounter <= NumBranchesOnThisLoopSide; ++BranchCounter ) {
					// reference
					auto & branch( this_loop_side.Branch( BranchCounter ) );
					NumCompsOnThisBranch = branch.TotalComponents;
					for ( CompCounter = 1; CompCounter <= NumCompsOnThisBranch; ++CompCounter ) {
						auto const & component( branch.Comp( CompCounter ) );
						auto const SELECT_CASE_var( component.TypeOf_Num );
						if ( ( SELECT_CASE_var == TypeOf_PumpVariableSpeed ) || ( SELECT_CASE_var == TypeOf_PumpBankVariableSpeed ) || ( SELECT_CASE_var == TypeOf_PumpCondensate ) ) {
							CompIndex = component.CompNum;
							if ( CompIndex > 0 ) {
								auto & this_pump( PumpEquip( CompIndex ) );
								this_pump.LoopSolverOverwriteFlag = true;
							}
						}
					}
				}
			}
		}


		// do some diagnostic that are easy and fast at this point, the rest of this routine could be moved
		//?  should be caught previously in input~ Check erroneous conditions first before we do the logic below
		//~ Check loop configuration, as this will dictate the flow that we request for our loop side
		ThisSideHasPumps = ( this_loop_side.TotalPumps > 0 );
		OtherSideHasPumps = ( other_loop_side.TotalPumps > 0 );
		if ( ThisLoopHasCommonPipe && ! ThisSideHasPumps ) {
			ShowSevereError( "SetupLoopFlowRequest: Common Pipe must have pumps on both sides of loop" );
			ShowContinueError( "Occurs on plant loop name =\"" + PlantLoop( LoopNum ).Name + "\"" );
			if ( ThisSide == DemandSide ) {
				ShowContinueError( "Add a pump to the demand side of the plant loop" );
			} else if ( ThisSide == SupplySide ) {
				ShowContinueError( "Add a pump to the supply side of the plant loop" );
			}
			ShowFatalError( "Program terminates due to preceding conditions." );

		} else if ( ! ThisSideHasPumps && ! OtherSideHasPumps ) {
			ShowSevereError( "SetupLoopFlowRequest: Problem in plant topology, no pumps specified on the loop" );
			ShowContinueError( "Occurs on plant loop name =\"" + PlantLoop( LoopNum ).Name + "\"" );
			ShowContinueError( "All plant loops require at least one pump" );
			ShowFatalError( "Program terminates due to preceding conditions." );
		}

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will step through all branch groups (single branch .OR. inlet/parallels/outlet)
		//  and call the branch group simulation routine.  This routine also calls to update the splitter
		//  and mixer.

		// METHODOLOGY EMPLOYED:
		// The number of branch groups is either 1 or 3.  1 would be a single branch half-loop.  3 would
		//  be the minimum for an inlet/parallels/outlet set.  The number of branch groups can then be
		//  calculated as #BrGrps = 1 + 2*L; where L is zero for single half loop and one for parallel-type set.
		//  This calculation can be reduced to the logical/integer conversion as shown in the code.
		// The simulation then steps through each branch group.  If there are parallel branches, the splitter is
		//  updated on flowlock=0 to pass information through, then after the parallel branches the mixer is always
		//  updated.  The outlet branch "group" is then simulated.

		// Using/Aliasing
		using PlantUtilities::UpdatePlantSplitter;
		using PlantUtilities::UpdatePlantMixer;
		using DataPlant::PlantLoop;
		using DataPlant::FlowUnlocked;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const InletBranchOrOneBranchHalfLoop( 1 );
		int const ParallelBranchSet( 2 );
		int const OutletBranch( 3 );
		bool const StartingNewLoopSidePass( true );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumBranchGroups;
		int BranchesGreaterThanOne;
		int BranchGroup;

		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches > 1 ) {
			BranchesGreaterThanOne = 1;
		} else {
			BranchesGreaterThanOne = 0;
		}
		NumBranchGroups = 1 + 2 * BranchesGreaterThanOne;

		for ( BranchGroup = 1; BranchGroup <= NumBranchGroups; ++BranchGroup ) {

			if ( ( BranchGroup > 1 ) && ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches == 1 ) ) break;

			{ auto const SELECT_CASE_var( BranchGroup );

			if ( SELECT_CASE_var == InletBranchOrOneBranchHalfLoop ) { // This group would be the inlet branch, or the single half-loop branch
				SimulateLoopSideBranchGroup( LoopNum, LoopSideNum, 1, 1, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag, StartingNewLoopSidePass );

			} else if ( SELECT_CASE_var == ParallelBranchSet ) { // This group is the parallel set of branches, or the single branch between the mix/split

				UpdatePlantSplitter( LoopNum, LoopSideNum, 1 );

				SimulateLoopSideBranchGroup( LoopNum, LoopSideNum, 2, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches - 1, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag );
				UpdatePlantMixer( LoopNum, LoopSideNum, 1 );

			} else if ( SELECT_CASE_var == OutletBranch ) { // This group is the outlet branch
				SimulateLoopSideBranchGroup( LoopNum, LoopSideNum, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag );

			}}

		}

	}

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
		bool const StartingNewLoopSidePass
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will manage the component simulation on a single set of parallel branches
		// This routine also reverts to a single branch simulation if there is "only one parallel" branch

		// METHODOLOGY EMPLOYED:
		// Loop through all components, and simulate first the non-load range based on each branch.
		// When a load-range based (LRB) is encountered, the simulation moves to the next branch to do non-LRB components.
		// When all paths are exhausted the simulation begins simulating LRB components.  Before each comp, the load distribution
		//  engine is called to handle the load distribution for this current pass.  If load is successfully distributed, this is
		//  flagged, and not called again.  If load is not distributed (i.e. this component isn't ON right now), then the
		//  load distribution engine will be called again before the next component.
		// After all load distribution is done and those components are complete, the simulation moves back to do any
		//  remaining components that may be downstream.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::DemandOpSchemeType;
		using DataPlant::PumpOpSchemeType;
		using DataPlant::LoadRangeBasedMin;
		using DataPlant::LoadRangeBasedMax;
		using DataPlant::FlowLocked;
		using DataPlant::NoControlOpSchemeType;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::FreeRejectionOpSchemeType;
		using DataPlant::WSEconOpSchemeType;
		using DataPlant::UnknownStatusOpSchemeType;
		using DataPlant::PressureCall_Calc;
		using DataPlant::EMSOpSchemeType;
		using DataPlant::SupplySide;
		using DataLoopNode::Node;
		using PlantCondLoopOperation::ManagePlantLoadDistribution;
		using PlantLoopEquip::SimPlantEquip;
		using PlantPressureSystem::SimPressureDropSystem;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//~ History values
		static int LastLoopNum( -1 );
		static int LastLoopSideNum( -1 );
		static int LastFirstBranchNum( -1 );
		static int LastLastBranchNum( -1 );

		//~ Indexing variables
		int BranchCounter; // ~ This contains the index for the %Branch(:) structure
		int BranchIndex; // ~ This is a 1 - n value within the current branch group
		int CompCounter; // ~ This contains the index for the %Comp(:) structure
		int StartingComponent;
		int EndingComponent;
		int NumBranchesInRegion;

		//~ Flags

		bool LoadDistributionWasPerformed;
		bool DummyInit;
		bool const DoNotGetCompSizFac( false );
		static Array1D_string const LoopSideNames( 2, { "Demand", "Supply" } );

		//~ General variables
		static Array1D_int LastComponentSimulated;
		Real64 LoadToLoopSetPoint;

		int curCompOpSchemePtr;
		int OpSchemePtr;

		// Object Data
//		static Array1D< Location > AccessibleBranches; // Set but never used
		Location PumpLocation;

		LoadToLoopSetPoint = 0.0; //Autodesk:Init Fix possible use uninitialized

		//~ Debug variables

		// We only need to reallocate the accessible array and reset the LastComponentSimulated if
		//  either is currently NOT allocated, or if we are coming into this routine with a
		//  new simulation region.  Otherwise leave it alone and save computation time
		if ( ( ! allocated( LastComponentSimulated ) ) || ( LoopNum != LastLoopNum ) || ( LoopSideNum != LastLoopSideNum ) || ( FirstBranchNum != LastFirstBranchNum ) || ( LastBranchNum != LastLastBranchNum ) || StartingNewLoopSidePass ) { //we need to reallocate // ( ! allocated( AccessibleBranches ) ) ||

			// How many will we need?
			NumBranchesInRegion = LastBranchNum - FirstBranchNum + 1;

			// Reallocate for the number of locations we have available // No heap if size is unchanged
			if ( NumBranchesInRegion > LastComponentSimulated.isize() ) { //Tuned Changed to grow-only strategy
				LastComponentSimulated.allocate( NumBranchesInRegion );
			}
			for ( int i = 1; i <= NumBranchesInRegion; ++i ) LastComponentSimulated( i ) = 0; // Only zero the active elements
//			AccessibleBranches.allocate( NumBranchesInRegion );

//			BranchIndex = 0;
//			for ( BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter ) {
//				++BranchIndex;
//				AccessibleBranches( BranchIndex ).LoopNum = LoopNum;
//				AccessibleBranches( BranchIndex ).LoopSideNum = LoopSideNum;
//				AccessibleBranches( BranchIndex ).BranchNum = BranchCounter;
//			}

		}

		// Store the arguments for the next call
		LastLoopNum = LoopNum;
		LastLoopSideNum = LoopSideNum;
		LastFirstBranchNum = FirstBranchNum;
		LastLastBranchNum = LastBranchNum;

		// Initialize this flag to false every time so we can call other routines
		DummyInit = false;

		// We now know what plant simulation region is available to us, let's simulate this group
		bool EncounteredLRBObjDuringPass1( false );
		BranchIndex = 0;
		auto & loop( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		for ( BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter ) {
			auto & branch( loop.Branch( BranchCounter ) );
			++BranchIndex;

			//~ Always start from the last component we did the last time around + 1 and
			//~  try to make it all the way to the end of the loop
			StartingComponent = LastComponentSimulated( BranchIndex ) + 1;
			EndingComponent = branch.TotalComponents;
			for ( CompCounter = StartingComponent; CompCounter <= EndingComponent; ++CompCounter ) {

				auto & this_comp( branch.Comp( CompCounter ) );
				auto const CurOpSchemeType( this_comp.CurOpSchemeType );

				switch ( CurOpSchemeType ) {
				case WSEconOpSchemeType: //~ coils
					this_comp.MyLoad = UpdatedDemandToLoopSetPoint;
					SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					break;
				case PumpOpSchemeType: //~ pump
					PumpLocation.LoopNum = LoopNum;
					PumpLocation.LoopSideNum = LoopSideNum;
					PumpLocation.BranchNum = BranchCounter;
					PumpLocation.CompNum = CompCounter;
					if ( loop.BranchPumpsExist ) {
						SimulateAllLoopSidePumps( LoopNum, LoopSideNum, PumpLocation, branch.RequestedMassFlow );
					} else {
						SimulateAllLoopSidePumps( LoopNum, LoopSideNum, PumpLocation, FlowRequest );
					}
					break;
				case CompSetPtBasedSchemeType:
					ManagePlantLoadDistribution( LoopNum, LoopSideNum, BranchCounter, CompCounter, LoadToLoopSetPoint, LoadToLoopSetPointThatWasntMet, FirstHVACIteration, LoopShutDownFlag, LoadDistributionWasPerformed );
					SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					break;
				case EMSOpSchemeType:
					if ( LoopSideNum == SupplySide ) {
						curCompOpSchemePtr = this_comp.CurCompLevelOpNum;
						OpSchemePtr = this_comp.OpScheme( curCompOpSchemePtr ).OpSchemePtr;
						PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EMSIntVarLoopDemandRate = InitialDemandToLoopSetPoint;
					}
					ManagePlantLoadDistribution( LoopNum, LoopSideNum, BranchCounter, CompCounter, UpdatedDemandToLoopSetPoint, LoadToLoopSetPointThatWasntMet, FirstHVACIteration, LoopShutDownFlag, LoadDistributionWasPerformed );
					SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					break;
				default:
					if ( ( CurOpSchemeType >= LoadRangeBasedMin ) && ( CurOpSchemeType <= LoadRangeBasedMax ) ) { //~ load range based
						EncounteredLRBObjDuringPass1 = true;
						goto components_end; // don't do any more components on this branch
					} else { //demand, , etc.
						SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					}
				}

				// Update loop demand as needed for changes this component may have made
				UpdateAnyLoopDemandAlterations( LoopNum, LoopSideNum, BranchCounter, CompCounter );

				//~ If we didn't EXIT early, we must have simulated, so update array
				LastComponentSimulated( BranchIndex ) = CompCounter;

			} //~ CompCounter
			components_end: ;

			if ( loop.FlowLock == FlowLocked ) {
				SimPressureDropSystem( LoopNum, FirstHVACIteration, PressureCall_Calc, LoopSideNum, BranchCounter );
			}

		} //~ BranchCounter

		// So now we have made one pass through all of the available components on these branches, skipping load based
		// If we didn't encounter any load based objects during the first pass, then we must be done!
		if ( ! EncounteredLRBObjDuringPass1 ) return;

		// If we have load based now, we should go ahead and distribute the load
		// If not then this branch group is done, since flow path validation was previously done
		LoadToLoopSetPoint = UpdatedDemandToLoopSetPoint;
		LoadDistributionWasPerformed = false;

		// The way the load distribution is set up, I think I should call this for every load range based component
		//  encountered until distribution is actually performed.  If we don't call for each component then we may
		//  call for a component that is not on the current equip list and then nothing would come on.
		bool EncounteredNonLBObjDuringPass2( false );
		BranchIndex = 0;
		for ( BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter ) {
			auto const & branch( loop.Branch( BranchCounter ) );
			++BranchIndex;

			//~ Always start from the last component we did the last time around + 1 and
			//~  try to make it all the way to the end of the loop
			StartingComponent = LastComponentSimulated( BranchIndex ) + 1;
			EndingComponent = branch.TotalComponents;
			for ( CompCounter = StartingComponent; CompCounter <= EndingComponent; ++CompCounter ) {

				auto const CurOpSchemeType( branch.Comp( CompCounter ).CurOpSchemeType );

				switch ( CurOpSchemeType ) {
				case NoControlOpSchemeType: //~ pipes, for example
					SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					break;
				case DemandOpSchemeType:
				case CompSetPtBasedSchemeType:
				case FreeRejectionOpSchemeType: //~ other control types
					EncounteredNonLBObjDuringPass2 = true;
					goto components2_end; // don't do anymore components on this branch
				case PumpOpSchemeType: //~ pump
					PumpLocation.LoopNum = LoopNum;
					PumpLocation.LoopSideNum = LoopSideNum;
					PumpLocation.BranchNum = BranchCounter;
					PumpLocation.CompNum = CompCounter;
					if ( loop.BranchPumpsExist ) {
						SimulateAllLoopSidePumps( LoopNum, LoopSideNum, PumpLocation, branch.RequestedMassFlow );
					} else {
						SimulateAllLoopSidePumps( LoopNum, LoopSideNum, PumpLocation, FlowRequest );
					}
					break;
				default:
					if ( ( CurOpSchemeType >= LoadRangeBasedMin ) && ( CurOpSchemeType <= LoadRangeBasedMax ) ) { //~ load range based
						if ( ! LoadDistributionWasPerformed ) { //~ Still need to distribute load among load range based components
							ManagePlantLoadDistribution( LoopNum, LoopSideNum, BranchCounter, CompCounter, LoadToLoopSetPoint, LoadToLoopSetPointThatWasntMet, FirstHVACIteration, LoopShutDownFlag, LoadDistributionWasPerformed );
						}
						SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					}
				}

				//~ If we didn't EXIT early, we must have simulated, so update array
				LastComponentSimulated( BranchIndex ) = CompCounter;

			} //~ CompCounter
			components2_end: ;

			//~ If we are locked, go ahead and simulate the pressure components on this branch
			if ( loop.FlowLock == FlowLocked ) {
				SimPressureDropSystem( LoopNum, FirstHVACIteration, PressureCall_Calc, LoopSideNum, BranchCounter );
			}

		} //~ BranchCounter

		// So now we have made the load range based pass through all the components on each branch
		// If we didn't see any other component types, then we are done, go away
		if ( ! EncounteredNonLBObjDuringPass2 ) return;

		// If we did encounter other objects than we just need to go back through and simulate them
		BranchIndex = 0;
		for ( BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter ) {
			auto const & branch( loop.Branch( BranchCounter ) );
			++BranchIndex;

			//~ Always start from the last component we did the last time around + 1 and
			//~  try to make it all the way to the end of the loop
			StartingComponent = LastComponentSimulated( BranchIndex ) + 1;
			EndingComponent = branch.TotalComponents;
			for ( CompCounter = StartingComponent; CompCounter <= EndingComponent; ++CompCounter ) {

				auto const CurOpSchemeType( branch.Comp( CompCounter ).CurOpSchemeType );

				switch ( CurOpSchemeType ) {
				case DemandOpSchemeType: //~ coils
					SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					break;
				case PumpOpSchemeType: //~ pump
					PumpLocation.LoopNum = LoopNum;
					PumpLocation.LoopSideNum = LoopSideNum;
					PumpLocation.BranchNum = BranchCounter;
					PumpLocation.CompNum = CompCounter;
					if ( loop.BranchPumpsExist ) {
						SimulateAllLoopSidePumps( LoopNum, LoopSideNum, PumpLocation, branch.RequestedMassFlow );
					} else {
						SimulateAllLoopSidePumps( LoopNum, LoopSideNum, PumpLocation, FlowRequest );
					}
					break;
				default:
					if ( ( CurOpSchemeType >= LoadRangeBasedMin ) && ( CurOpSchemeType <= LoadRangeBasedMax ) ) { //~ load range based
						ShowFatalError( "Encountered Load Based Object after other components, invalid." );
					} else { //~ Typical control equipment
						SimPlantEquip( LoopNum, LoopSideNum, BranchCounter, CompCounter, FirstHVACIteration, DummyInit, DoNotGetCompSizFac );
					}
				}

				//~ If we didn't EXIT early, we must have simulated, so update array
				LastComponentSimulated( BranchIndex ) = CompCounter;

			} //~ CompCounter

			if ( loop.FlowLock == FlowLocked ) {
				SimPressureDropSystem( LoopNum, FirstHVACIteration, PressureCall_Calc, LoopSideNum, BranchCounter );
			}

		} //~ BranchCounter

		// I suppose I could do a check on the last component simulated to make sure we actually exhausted all branches
		// This would be the "THIRD" check on flow validation, but would be OK

	}

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
		Optional< Location const > SpecificPumpLocation,
		Optional< Real64 const > SpecificPumpFlowRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// Using/Aliasing
		using DataPlant::LoopSidePumpInformation;
		using DataPlant::PlantLoop;
		using DataPlant::TotNumLoops;
		using DataLoopNode::Node;
		using Pumps::SimPumps;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopCounter;
		int LoopSideCounter;
		int PumpCounter;
		int PumpIndexStart;
		int PumpIndexEnd;
		Real64 FlowToRequest;
		bool ThisPumpRunning;
		Real64 ThisPumpFlowRate;
		Real64 ThisPumpMinAvail;
		Real64 ThisPumpMaxAvail;
		int PumpLoopNum;
		int PumpLoopSideNum;
		int PumpBranchNum;
		int PumpCompNum;
		int PumpOutletNode;
		/////////// hoisted into namespace
		//static bool EstablishedCompPumpIndeces( false );
		//////////////////////////////
		//~ One time sweep through all loops/loopsides/pumps, assigning indeces to the pl%ls%br%comp%indexinloopsidepumps variable
		if ( ! EstablishedCompPumpIndeces ) {
			for ( LoopCounter = 1; LoopCounter <= TotNumLoops; ++LoopCounter ) {
				for ( LoopSideCounter = 1; LoopSideCounter <= 2; ++LoopSideCounter ) {
					for ( PumpCounter = 1; PumpCounter <= PlantLoop( LoopCounter ).LoopSide( LoopSideCounter ).TotalPumps; ++PumpCounter ) {
						PumpBranchNum = PlantLoop( LoopCounter ).LoopSide( LoopSideCounter ).Pumps( PumpCounter ).BranchNum;
						PumpCompNum = PlantLoop( LoopCounter ).LoopSide( LoopSideCounter ).Pumps( PumpCounter ).CompNum;
						PlantLoop( LoopCounter ).LoopSide( LoopSideCounter ).Branch( PumpBranchNum ).Comp( PumpCompNum ).IndexInLoopSidePumps = PumpCounter;
					}
				}
			}
			EstablishedCompPumpIndeces = true;
		}

		// If we have a specific loop/side/br/comp, then find the index and only do that one, otherwise do all pumps on the loop side
		if ( present( SpecificPumpLocation ) ) {
			PumpLoopNum = SpecificPumpLocation().LoopNum;
			PumpLoopSideNum = SpecificPumpLocation().LoopSideNum;
			PumpBranchNum = SpecificPumpLocation().BranchNum;
			PumpCompNum = SpecificPumpLocation().CompNum;
			PumpIndexStart = PlantLoop( PumpLoopNum ).LoopSide( PumpLoopSideNum ).Branch( PumpBranchNum ).Comp( PumpCompNum ).IndexInLoopSidePumps;
			PumpIndexEnd = PumpIndexStart;
		} else {
			PumpLoopNum = LoopNum;
			PumpLoopSideNum = ThisSide;
			PumpIndexStart = 1;
			PumpIndexEnd = PlantLoop( LoopNum ).LoopSide( ThisSide ).TotalPumps;
		}

		// If we have a flow rate to hit, then go for it, otherwise, just operate in request mode with zero flow
		if ( present( SpecificPumpFlowRate ) ) {
			FlowToRequest = SpecificPumpFlowRate;
		} else {
			FlowToRequest = 0.0;
		}

		//~ Now loop through all the pumps and simulate them, keeping track of their status
		auto & loop_side( PlantLoop( PumpLoopNum ).LoopSide( PumpLoopSideNum ) );
		auto & loop_side_branch( loop_side.Branch );
		for ( PumpCounter = PumpIndexStart; PumpCounter <= PumpIndexEnd; ++PumpCounter ) {

			//~ Set some variables
			auto & pump( loop_side.Pumps( PumpCounter ) );
			PumpBranchNum = pump.BranchNum;
			PumpCompNum = pump.CompNum;
			PumpOutletNode = pump.PumpOutletNode;

			AdjustPumpFlowRequestByEMSControls( PumpLoopNum, PumpLoopSideNum, PumpBranchNum, PumpCompNum, FlowToRequest );

			// Call SimPumps, routine takes a flow request, and returns some info about the status of the pump
			SimPumps( pump.PumpName, PumpLoopNum, FlowToRequest, ThisPumpRunning, loop_side_branch( PumpBranchNum ).PumpIndex, pump.PumpHeatToFluid );

			//~ Pull some state information from the pump outlet node
			ThisPumpFlowRate = Node( PumpOutletNode ).MassFlowRate;
			ThisPumpMinAvail = Node( PumpOutletNode ).MassFlowRateMinAvail;
			ThisPumpMaxAvail = Node( PumpOutletNode ).MassFlowRateMaxAvail;

			//~ Now update the data structure
			pump.CurrentMinAvail = ThisPumpMinAvail;
			pump.CurrentMaxAvail = ThisPumpMaxAvail;

		}

		//~ Update the LoopSide pump heat totality here
		if ( loop_side.TotalPumps > 0 ) {
			loop_side.TotalPumpHeat = sum( loop_side.Pumps, &LoopSidePumpInformation::PumpHeatToFluid );
		}

	}

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
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// To evaluate the demand to hit the loop setpoint based on the loop side inlet conditions

		// METHODOLOGY EMPLOYED:
		// This routine will simply call the evaluate loop setpoint routine but call it from
		//  the very beginning of this loop side, so that it is basically for the entire loop side

		// Return value
		Real64 Demand;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Array1D_int const InitCompArray( 1, 0 );

		Demand = EvaluateLoopSetPointLoad( LoopNum, ThisSide, 1, 1, InitCompArray );

		return Demand;

	}

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
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::LoopDemandTol;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using DataLoopNode::NodeType_Water;
		using DataLoopNode::NodeType_Steam;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetSatEnthalpyRefrig;
		using General::RoundSigDigits;

		// Return value
		Real64 LoadToLoopSetPoint; // function result

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		//~ Indexing variables
		int BranchCounter; // ~ This contains the index for the %Branch(:) structure
		int BranchIndex; // ~ This is a 1 - n value within the current branch group
		int StartingComponent; // ~ The component which "would" be simulated next

		static std::string const RoutineName( "PlantLoopSolver::EvaluateLoopSetPointLoad" );
		static std::string const RoutineNameAlt( "PlantSupplySide:EvaluateLoopSetPointLoad" );

		//~ General variables
		Real64 EnteringTemperature;
		Real64 MassFlowRate;
		Real64 SumMdotTimesTemp;
		Real64 SumMdot;
		Real64 WeightedInletTemp;
		Real64 LoopSetPointTemperature;
		Real64 LoopSetPointTemperatureHi;
		Real64 LoopSetPointTemperatureLo;
		Real64 LoadToHeatingSetPoint;
		Real64 LoadToCoolingSetPoint;
		Real64 DeltaTemp;
		int EnteringNodeNum;
		Real64 Cp;
		Real64 EnthalpySteamSatVapor; // Enthalpy of saturated vapor
		Real64 EnthalpySteamSatLiquid; // Enthalpy of saturated liquid
		Real64 LatentHeatSteam; // Latent heat of steam

		// Initialize
		LoadToLoopSetPoint = 0.0;

		// Sweep across flow paths in this group and calculate the deltaT and then the load
		BranchIndex = 0;
		SumMdotTimesTemp = 0.0;
		SumMdot = 0.0;
		for ( BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter ) {

			++BranchIndex;

			//~ Always start from the last component we did the last time around + 1 and
			//~  try to make it all the way to the end of the loop
			StartingComponent = LastComponentSimulated( BranchIndex ) + 1;
			EnteringNodeNum = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchCounter ).Comp( StartingComponent ).NodeNumIn;

			EnteringTemperature = Node( EnteringNodeNum ).Temp;
			MassFlowRate = Node( EnteringNodeNum ).MassFlowRate;

			SumMdotTimesTemp += ( EnteringTemperature * MassFlowRate );
			SumMdot += ( MassFlowRate );

		}

		if ( SumMdot < MassFlowTolerance ) {
			LoadToLoopSetPoint = 0.0;
			return LoadToLoopSetPoint;
		}

		WeightedInletTemp = SumMdotTimesTemp / SumMdot;

		if ( PlantLoop( LoopNum ).FluidType == NodeType_Water ) {

			Cp = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, WeightedInletTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

			{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {

				// Pick up the loop setpoint temperature
				LoopSetPointTemperature = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
				// Calculate the delta temperature
				DeltaTemp = LoopSetPointTemperature - WeightedInletTemp;

				// Calculate the demand on the loop
				LoadToLoopSetPoint = SumMdot * Cp * DeltaTemp;

			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {

				// Get the range of setpoints
				LoopSetPointTemperatureHi = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				LoopSetPointTemperatureLo = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointLo;

				//Calculate the demand on the loop
				if ( SumMdot > 0.0 ) {
					LoadToHeatingSetPoint = SumMdot * Cp * ( LoopSetPointTemperatureLo - WeightedInletTemp );
					LoadToCoolingSetPoint = SumMdot * Cp * ( LoopSetPointTemperatureHi - WeightedInletTemp );
					// Possible combinations:
					// 1  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
					// 2  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
					// 3  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
					// 4  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Not Feasible if LoopSetPointHi >= LoopSetPointLo
					// First trap bad set-points
					if ( LoadToHeatingSetPoint > LoadToCoolingSetPoint ) {
						ShowSevereError( "Plant Loop: the Plant Loop Demand Calculation Scheme is set to DualSetPointDeadBand, but the heating-related low setpoint appears to be above the cooling-related high setpoint." );
						ShowContinueError( "For example, if using SetpointManager:Scheduled:DualSetpoint, then check that the low setpoint is below the high setpoint." );
						ShowContinueError( "Occurs in PlantLoop=" + PlantLoop( LoopNum ).Name );
						ShowContinueError( "LoadToHeatingSetPoint=" + RoundSigDigits( LoadToHeatingSetPoint, 3 ) + ", LoadToCoolingSetPoint=" + RoundSigDigits( LoadToCoolingSetPoint, 3 ) );
						ShowContinueError( "Loop Heating Low Setpoint=" + RoundSigDigits( LoopSetPointTemperatureLo, 2 ) );
						ShowContinueError( "Loop Cooling High Setpoint=" + RoundSigDigits( LoopSetPointTemperatureHi, 2 ) );

						ShowFatalError( "Program terminates due to above conditions." );
					}
					if ( LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0 ) {
						LoadToLoopSetPoint = LoadToHeatingSetPoint;
					} else if ( LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0 ) {
						LoadToLoopSetPoint = LoadToCoolingSetPoint;
					} else if ( LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0 ) { // deadband includes zero loads
						LoadToLoopSetPoint = 0.0;
					} else {
						ShowSevereError( "DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - report to EnergyPlus Development Team" );
						ShowContinueError( "occurs in PlantLoop=" + PlantLoop( LoopNum ).Name );
						ShowContinueError( "LoadToHeatingSetPoint=" + RoundSigDigits( LoadToHeatingSetPoint, 3 ) + ", LoadToCoolingSetPoint=" + RoundSigDigits( LoadToCoolingSetPoint, 3 ) );
						ShowContinueError( "Loop Heating Setpoint=" + RoundSigDigits( LoopSetPointTemperatureLo, 2 ) );
						ShowContinueError( "Loop Cooling Setpoint=" + RoundSigDigits( LoopSetPointTemperatureHi, 2 ) );
						ShowFatalError( "Program terminates due to above conditions." );
					}
				} else {
					LoadToLoopSetPoint = 0.0;
				}

			}}

		} else if ( PlantLoop( LoopNum ).FluidType == NodeType_Steam ) {

			Cp = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, WeightedInletTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

			{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {

				// Pick up the loop setpoint temperature
				LoopSetPointTemperature = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;

				// Calculate the delta temperature
				DeltaTemp = LoopSetPointTemperature - WeightedInletTemp;

				EnthalpySteamSatVapor = GetSatEnthalpyRefrig( fluidNameSteam, LoopSetPointTemperature, 1.0, RefrigIndex, RoutineNameAlt );
				EnthalpySteamSatLiquid = GetSatEnthalpyRefrig( fluidNameSteam, LoopSetPointTemperature, 0.0, RefrigIndex, RoutineNameAlt );

				LatentHeatSteam = EnthalpySteamSatVapor - EnthalpySteamSatLiquid;

				// Calculate the demand on the loop
				LoadToLoopSetPoint = SumMdot * ( Cp * DeltaTemp + LatentHeatSteam );

			}}

		} else { // only have two types, water serves for glycol.

		}

		// Trim the demand to zero if it is very small
		if ( std::abs( LoadToLoopSetPoint ) < LoopDemandTol ) LoadToLoopSetPoint = 0.0;

		return LoadToLoopSetPoint;

	}

	//==================================================================!
	//==================================================================!
	//==================================================================!

	void
	UpdateAnyLoopDemandAlterations(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will analyze the given component and determine if any
		//  alterations need to be made to the current loop demand value.  If so,
		//  it will make the changes to the module level loop demand variables.

		// METHODOLOGY EMPLOYED:
		// Components will always supply a useful delta T, even if it happens to be zero
		// For flow rate, make decisions based on the component's current operating scheme type:
		//    Demand based: these components will have a flow request on their inlet node
		//    Pump: these components will not be included, as they no longer include heat at the pump
		//    component setpoint: these components will have a flow request

		//    on their outlet node corresponding to their calculated delta T
		//    load range based: these components do not 'alter' the load, they reject the load
		//    Therefore they are not included

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::LoadRangeBasedMin;
		using DataPlant::LoadRangeBasedMax;
		using DataPlant::FlowUnlocked;
		using DataPlant::FlowLocked;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PlantLoopSolver::UpdateAnyLoopDemandAlterations" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Init to zero, so that if we don't find anything, we exit early
		Real64 ComponentMassFlowRate( 0.0 );

		auto const & this_loop( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		auto const & this_comp( this_loop.Branch( BranchNum ).Comp( CompNum ) );

		// Get information
		int const InletNode( this_comp.NodeNumIn );
		int const OutletNode( this_comp.NodeNumOut );

		if ( this_loop.FlowLock == FlowUnlocked ) {

			// For unlocked flow, use the inlet request -- !DSU? for now
			{ auto const SELECT_CASE_var( this_comp.CurOpSchemeType );
			if ( ( SELECT_CASE_var >= LoadRangeBasedMin ) && ( SELECT_CASE_var <= LoadRangeBasedMax ) ) {
				// Don't do anything for load based components
			} else {
				// pumps pipes, etc. will be lumped in here with other component types, but they will have no delta T anyway
				ComponentMassFlowRate = Node( InletNode ).MassFlowRateRequest;
				//DSU? make sure components like economizers use the mass flow request
			}}

		} else if ( this_loop.FlowLock == FlowLocked ) {

			// For locked flow just use the mass flow rate
			{ auto const SELECT_CASE_var( this_comp.CurOpSchemeType );
			if ( ( SELECT_CASE_var >= LoadRangeBasedMin ) && ( SELECT_CASE_var <= LoadRangeBasedMax ) ) {
				// Don't do anything for load based components
			} else {
				// pumps pipes, etc. will be lumped in here with other component types, but they will have no delta T anyway
				ComponentMassFlowRate = Node( OutletNode ).MassFlowRate;
			}}

		} else { // flow pump query? problem?

		}

		// Leave early if there wasn't a mass flow rate or request
		if ( ComponentMassFlowRate < MassFlowTolerance ) return;

		// Get an average temperatre for the property call
		Real64 const InletTemp( Node( InletNode ).Temp );
		Real64 const OutletTemp( Node( OutletNode ).Temp );
		Real64 const AverageTemp( ( InletTemp + OutletTemp ) / 2.0 );
		Real64 const ComponentCp( GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, AverageTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName ) );

		// Calculate the load altered by this component
		Real64 const LoadAlteration( ComponentMassFlowRate * ComponentCp * ( OutletTemp - InletTemp ) );

		// Now alter the module level variables
		CurrentAlterationsToDemand += LoadAlteration;
		UpdatedDemandToLoopSetPoint = InitialDemandToLoopSetPoint - CurrentAlterationsToDemand;

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brandon Anderson, Dan Fisher
		//       DATE WRITTEN   October 1999
		//       MODIFIED       May 2005 Sankaranarayanan K P, Rich Liesen
		//       RE-ENGINEERED  Sept 2010 Dan Fisher, Brent Griffith for demand side update

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine takes the overall loop side flow and distributes
		// it among parallel branches. this is the main implementation of
		// flow splitting for plant splitter/mixer

		// METHODOLOGY EMPLOYED:
		// Flow through the branches is currently determined by
		// the active component on the branch, as well as the
		// order of the branches following the splitter.
		// SimPlantEquipment is run first, and the active components
		// request their flow.  These flows are compared and a simple
		// algorithm balances flow in the branches.  The flow in these
		// branches is then locked down, via MassFlowRateMaxAvail and MinAvail
		// SimPlant Equipment is then run again in order to get correct
		// properties.  Finally, Max/MinAvail are reset for the next time step.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_PumpVariableSpeed;
		using DataPlant::TypeOf_PumpBankVariableSpeed;
		using DataBranchAirLoopPlant::ControlType_Active;
		using DataBranchAirLoopPlant::ControlType_Passive;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::ControlType_Bypass;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const LoopSideName( 2, { "Demand", "Supply" } );
		int const SplitNum( 1 ); // Only one splitter/mixer combination is allowed
		int const LoopSideSingleBranch( 1 ); // For readability

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumActiveBranches; // Active branch counter
		Real64 ActiveFlowRate; // The flow available when cycling through branches
		Real64 PassiveFlowRate; // The flow available when cycling through branches
		Real64 FracFlow; // The flow available when cycling through branches
		Real64 ThisBranchRequestFrac; // The request ratio
		Real64 totalMax; // The flow available when cycling through branches
		Real64 FlowRemaining; // The flow available when cycling through branches
		int OutletNum; // Splitter outlet
		int MixerBranchOut;
		int SplitterBranchIn; // As the name implies
		int SplitterBranchOut; // As the name implies
		int LastNodeOnBranch; // intermediate value used for better readabilty
		int FirstNodeOnBranch; // intermediate value used for better readabilty
		int BranchNum; // intermediate value used for better readabilty
		int iBranch; // DO loop counter for cycling through branches
		int NumSplitOutlets; // As the name implies
		Real64 OutletBranchMinAvail;
		Real64 OutletBranchMaxAvail;
		Real64 InletBranchMinAvail;
		Real64 InletBranchMaxAvail;
		Real64 BranchFlowReq;
		Real64 BranchMinAvail;
		Real64 BranchMaxAvail;
		Real64 ParallelBranchMaxAvail;
		Real64 ParallelBranchMinAvail;
		Real64 TotParallelBranchFlowReq;
		Real64 LoopFlowRate;
		int FirstNodeOnBranchIn;
		int FirstNodeOnBranchOut;
		Real64 StartingFlowRate;
		Real64 ThisBranchRequest;
		int CompCounter;
		int CompInletNode;
		int CompOutletNode;

		auto & this_loopside( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );

		// If there is no splitter then there is no continuity to enforce.
		if ( ! this_loopside.SplitterExists ) {

			//If there's only one branch, then RETURN
			if ( this_loopside.TotalBranches == 1 ) {
				// The branch should just try to meet the request previously calculated.  This should be good,
				// just need to make sure that during FlowUnlocked, no one constrained Min/Max farther.
				// This would have been propogated down the branch, so we can check the outlet node min/max avail for this.
				auto & this_single_branch( this_loopside.Branch( LoopSideSingleBranch ) );
				LastNodeOnBranch = this_single_branch.NodeNumOut;
				FirstNodeOnBranch = this_single_branch.NodeNumIn;
				BranchMinAvail = Node( LastNodeOnBranch ).MassFlowRateMinAvail;
				BranchMaxAvail = Node( LastNodeOnBranch ).MassFlowRateMaxAvail;
				Node( FirstNodeOnBranch ).MassFlowRate = min( max( ThisLoopSideFlow, BranchMinAvail ), BranchMaxAvail );
				// now with flow locked, this single branch will just ran at the specified flow rate, so we are done
				return;
			} else {
				ShowSevereError( "Plant topology problem for PlantLoop: " + PlantLoop( LoopNum ).Name + ", " + LoopSideName( LoopSideNum ) + " side." );
				ShowContinueError( "There are multiple branches, yet no splitter.  This is an invalid configuration." );
				ShowContinueError( "Add a set of connectors, use put components on a single branch." );
				ShowFatalError( "Invalid plant topology causes program termination." );
				return;
			}
		}

		// If a splitter/mixer combination exist on the loop
		if ( this_loopside.SplitterExists && this_loopside.MixerExists ) {

			// Zero out local variables
			TotParallelBranchFlowReq = 0.0;
			NumSplitOutlets = this_loopside.Splitter( SplitNum ).TotalOutletNodes;
			if ( NumSplitOutlets < 1 ) {
				ShowSevereError( "Plant topology problem for PlantLoop: " + PlantLoop( LoopNum ).Name + ", " + LoopSideName( LoopSideNum ) + " side." );
				ShowContinueError( "Diagnostic error in PlantLoopSolver::ResolveParallelFlows." );
				ShowContinueError( "Splitter improperly specified, no splitter outlets." );
				ShowFatalError( "Invalid plant topology causes program termination." );
			}

			NumActiveBranches = 0;
			ParallelBranchMaxAvail = 0.0;
			ParallelBranchMinAvail = 0.0;
			for ( iBranch = 1; iBranch <= NumSplitOutlets; ++iBranch ) {

				BranchNum = this_loopside.Splitter( SplitNum ).BranchNumOut( iBranch );
				auto & this_branch( this_loopside.Branch( BranchNum ) );
				SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( iBranch );
				auto & this_splitter_outlet_branch( this_loopside.Branch( SplitterBranchOut ) );
				LastNodeOnBranch = this_branch.NodeNumOut;
				FirstNodeOnBranch = this_branch.NodeNumIn;
				BranchFlowReq = DetermineBranchFlowRequest( LoopNum, LoopSideNum, BranchNum );

				//now, if we are have branch pumps, here is the situation:
				// constant speed pumps lock in a flow request on the inlet node
				// variable speed pumps which have other components on the branch do not log a request themselves
				// the DetermineBranchFlowRequest routine only looks at the branch inlet node
				// for variable speed branch pumps then, this won't work because the branch will be requesting zero
				// so let's adjust for this here to make sure these branches get good representation
				for ( CompCounter = 1; CompCounter <= this_branch.TotalComponents; ++CompCounter ) {

					auto & this_comp( this_branch.Comp( CompCounter ) );

					//if this isn't a variable speed pump then just keep cycling
					if ( ( this_comp.TypeOf_Num != TypeOf_PumpVariableSpeed ) && ( this_comp.TypeOf_Num != TypeOf_PumpBankVariableSpeed ) ) {
						continue;
					}

					CompInletNode = this_comp.NodeNumIn;
					BranchFlowReq = max( BranchFlowReq, Node( CompInletNode ).MassFlowRateRequest );

				}

				BranchMinAvail = Node( LastNodeOnBranch ).MassFlowRateMinAvail;
				BranchMaxAvail = Node( LastNodeOnBranch ).MassFlowRateMaxAvail;
				//            !sum the branch flow requests to a total parallel branch flow request
				if ( this_splitter_outlet_branch.ControlType == ControlType_Active || this_splitter_outlet_branch.ControlType == ControlType_SeriesActive ) {
					TotParallelBranchFlowReq += BranchFlowReq;
					++NumActiveBranches;
				}
				Node( FirstNodeOnBranch ).MassFlowRate = BranchFlowReq;
				Node( FirstNodeOnBranch ).MassFlowRateMinAvail = BranchMinAvail;
				Node( FirstNodeOnBranch ).MassFlowRateMaxAvail = BranchMaxAvail;
				ParallelBranchMaxAvail += BranchMaxAvail;
				ParallelBranchMinAvail += BranchMinAvail;
			}
			//            ! Find branch number and flow rates at splitter inlet
			SplitterBranchIn = this_loopside.Splitter( SplitNum ).BranchNumIn;
			LastNodeOnBranch = this_loopside.Branch( SplitterBranchIn ).NodeNumOut;
			FirstNodeOnBranchIn = this_loopside.Branch( SplitterBranchIn ).NodeNumIn;
			InletBranchMinAvail = Node( LastNodeOnBranch ).MassFlowRateMinAvail;
			InletBranchMaxAvail = Node( LastNodeOnBranch ).MassFlowRateMaxAvail;
			//            ! Find branch number and flow rates at mixer outlet
			MixerBranchOut = this_loopside.Mixer( SplitNum ).BranchNumOut;
			LastNodeOnBranch = this_loopside.Branch( MixerBranchOut ).NodeNumOut;
			FirstNodeOnBranchOut = this_loopside.Branch( MixerBranchOut ).NodeNumIn;
			OutletBranchMinAvail = Node( LastNodeOnBranch ).MassFlowRateMinAvail;
			OutletBranchMaxAvail = Node( LastNodeOnBranch ).MassFlowRateMaxAvail;

			LoopFlowRate = ThisLoopSideFlow;

			auto & first_branch_inlet_node( Node( FirstNodeOnBranchIn ) );
			auto & last_branch_inlet_node( Node( FirstNodeOnBranchOut ) );

			//Reset branch inlet node flow rates for the first and last branch on loop
			first_branch_inlet_node.MassFlowRate = ThisLoopSideFlow;
			last_branch_inlet_node.MassFlowRate = ThisLoopSideFlow;

			//Reset branch inlet node Min/MaxAvails for the first and last branch on loop
			first_branch_inlet_node.MassFlowRateMaxAvail = min( first_branch_inlet_node.MassFlowRateMaxAvail, ParallelBranchMaxAvail );
			first_branch_inlet_node.MassFlowRateMaxAvail = min( first_branch_inlet_node.MassFlowRateMaxAvail, last_branch_inlet_node.MassFlowRateMaxAvail );
			first_branch_inlet_node.MassFlowRateMinAvail = max( first_branch_inlet_node.MassFlowRateMinAvail, ParallelBranchMinAvail );
			first_branch_inlet_node.MassFlowRateMinAvail = max( first_branch_inlet_node.MassFlowRateMinAvail, last_branch_inlet_node.MassFlowRateMinAvail );
			last_branch_inlet_node.MassFlowRateMinAvail = first_branch_inlet_node.MassFlowRateMinAvail;
			last_branch_inlet_node.MassFlowRateMaxAvail = first_branch_inlet_node.MassFlowRateMaxAvail;

			//Initialize the remaining flow variable
			FlowRemaining = ThisLoopSideFlow;

			//Initialize flow on passive, bypass and uncontrolled parallel branches to zero.  For these branches
			//MinAvail is not enforced
			for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
				SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
				FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
				if ( this_loopside.Branch( SplitterBranchOut ).ControlType != ControlType_Active && this_loopside.Branch( SplitterBranchOut ).ControlType != ControlType_SeriesActive ) {
					Node( FirstNodeOnBranch ).MassFlowRate = 0.0;
					PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
				}
			}

			//IF SUFFICIENT FLOW TO MEET ALL PARALLEL BRANCH FLOW REQUESTS
			if ( FlowRemaining < MassFlowTolerance ) { // no flow available at all for splitter
				for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
					SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
					for ( CompCounter = 1; CompCounter <= this_loopside.Branch( SplitterBranchOut ).TotalComponents; ++CompCounter ) {

						FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
						CompInletNode = this_loopside.Branch( SplitterBranchOut ).Comp( CompCounter ).NodeNumIn;
						CompOutletNode = this_loopside.Branch( SplitterBranchOut ).Comp( CompCounter ).NodeNumOut;
						Node( CompInletNode ).MassFlowRate = 0.0;
						Node( CompInletNode ).MassFlowRateMaxAvail = 0.0;
						Node( CompOutletNode ).MassFlowRate = 0.0;
						Node( CompOutletNode ).MassFlowRateMaxAvail = 0.0;
					}
				}
				return;
			} else if ( FlowRemaining >= TotParallelBranchFlowReq ) {

				// 1) Satisfy flow demand of ACTIVE splitter outlet branches
				for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
					SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
					FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
					if ( this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_Active || this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_SeriesActive ) {
						// branch flow is min of requested flow and remaining flow
						Node( FirstNodeOnBranch ).MassFlowRate = min( Node( FirstNodeOnBranch ).MassFlowRate, FlowRemaining );
						if ( Node( FirstNodeOnBranch ).MassFlowRate < MassFlowTolerance ) Node( FirstNodeOnBranch ).MassFlowRate = 0.0;
						PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
						FlowRemaining -= Node( FirstNodeOnBranch ).MassFlowRate;
						if ( FlowRemaining < MassFlowTolerance ) FlowRemaining = 0.0;
					}
				}
				//IF the active branches take the entire loop flow, return
				if ( FlowRemaining == 0.0 ) return;

				// 2) Distribute remaining flow to PASSIVE branches
				totalMax = 0.0;
				for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
					SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
					FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
					if ( this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_Passive ) {
						//Calculate the total max available
						totalMax += Node( FirstNodeOnBranch ).MassFlowRateMaxAvail;
					}
				}

				if ( totalMax > 0 ) {
					for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
						SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
						FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
						if ( this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_Passive ) {
							FracFlow = FlowRemaining / totalMax;
							if ( FracFlow <= 1.0 ) { //the passive branches will take all the flow
								PassiveFlowRate = FracFlow * Node( FirstNodeOnBranch ).MassFlowRateMaxAvail;
								//Check against FlowRemaining
								PassiveFlowRate = min( FlowRemaining, PassiveFlowRate );
								//Allow FlowRequest to be increased to meet minimum on branch
								PassiveFlowRate = max( PassiveFlowRate, Node( FirstNodeOnBranch ).MassFlowRateMinAvail );
								FlowRemaining = max( ( FlowRemaining - PassiveFlowRate ), 0.0 );
								Node( FirstNodeOnBranch ).MassFlowRate = PassiveFlowRate;
							} else { //Each Branch receives maximum flow and BYPASS must be used
								Node( FirstNodeOnBranch ).MassFlowRate = min( Node( FirstNodeOnBranch ).MassFlowRateMaxAvail, FlowRemaining );
								FlowRemaining -= Node( FirstNodeOnBranch ).MassFlowRate;
							}
							PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
						}
					}
				} //totalMax <=0 and flow should be assigned to active branches
				//IF the passive branches take the remaining loop flow, return
				if ( FlowRemaining == 0.0 ) return;

				// 3) Distribute remaining flow to the BYPASS
				for ( OutletNum = 1; OutletNum <= this_loopside.Splitter( SplitNum ).TotalOutletNodes; ++OutletNum ) {
					SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
					FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
					if ( this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_Bypass ) {
						Node( FirstNodeOnBranch ).MassFlowRate = min( FlowRemaining, Node( FirstNodeOnBranch ).MassFlowRateMaxAvail );
						PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
						FlowRemaining -= Node( FirstNodeOnBranch ).MassFlowRate;
					}
				}
				//IF the bypass take the remaining loop flow, return
				if ( FlowRemaining == 0.0 ) return;

				// 4) If PASSIVE branches and BYPASS are at max and there's still flow, distribute remaining flow to ACTIVE branches
				if ( NumActiveBranches > 0 ) {
					ActiveFlowRate = FlowRemaining / NumActiveBranches;
					for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
						SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
						FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
						if ( this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_Active || this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_SeriesActive ) {
							//check Remaining flow (should be correct!)
							ActiveFlowRate = min( ActiveFlowRate, FlowRemaining );
							//set the flow rate to the MIN((MassFlowRate+AvtiveFlowRate), MaxAvail)
							StartingFlowRate = Node( FirstNodeOnBranch ).MassFlowRate;
							Node( FirstNodeOnBranch ).MassFlowRate = min( ( Node( FirstNodeOnBranch ).MassFlowRate + ActiveFlowRate ), Node( FirstNodeOnBranch ).MassFlowRateMaxAvail );
							PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
							//adjust the remaining flow
							FlowRemaining -= ( Node( FirstNodeOnBranch ).MassFlowRate - StartingFlowRate );
						}
						if ( FlowRemaining == 0 ) break;
					}
					//IF the active branches take the remaining loop flow, return
					if ( FlowRemaining == 0.0 ) return;

					// 5)  Step 4) could have left ACTIVE branches < MaxAvail.  Check to makes sure all ACTIVE branches are at MaxAvail
					for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
						SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
						FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
						if ( this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_Active || this_loopside.Branch( SplitterBranchOut ).ControlType == ControlType_SeriesActive ) {
							StartingFlowRate = Node( FirstNodeOnBranch ).MassFlowRate;
							ActiveFlowRate = min( FlowRemaining, ( Node( FirstNodeOnBranch ).MassFlowRateMaxAvail - StartingFlowRate ) );
							FlowRemaining -= ActiveFlowRate;
							Node( FirstNodeOnBranch ).MassFlowRate = StartingFlowRate + ActiveFlowRate;
							PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
						}
					}
				}
				//IF the active branches take the remaining loop flow, return
				if ( FlowRemaining == 0.0 ) return;

				// 6) Adjust Inlet branch and outlet branch flow rates to match parallel branch rate
				//DSU? do we need this logic?   or should we fatal on a diagnostic error
				TotParallelBranchFlowReq = 0.0;
				for ( iBranch = 1; iBranch <= NumSplitOutlets; ++iBranch ) {
					BranchNum = this_loopside.Splitter( SplitNum ).BranchNumOut( iBranch );
					FirstNodeOnBranch = this_loopside.Branch( BranchNum ).NodeNumIn;
					//calculate parallel branch flow rate
					TotParallelBranchFlowReq += Node( FirstNodeOnBranch ).MassFlowRate;
				}
				// Reset the flow on the splitter inlet branch
				SplitterBranchIn = this_loopside.Splitter( SplitNum ).BranchNumIn;
				FirstNodeOnBranchIn = this_loopside.Branch( SplitterBranchIn ).NodeNumIn;
				Node( FirstNodeOnBranchIn ).MassFlowRate = TotParallelBranchFlowReq;
				PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchIn, Node( FirstNodeOnBranchIn ).MassFlowRate, FirstHVACIteration );
				// Reset the flow on the Mixer outlet branch
				MixerBranchOut = this_loopside.Mixer( SplitNum ).BranchNumOut;
				FirstNodeOnBranchOut = this_loopside.Branch( MixerBranchOut ).NodeNumIn;
				Node( FirstNodeOnBranchOut ).MassFlowRate = TotParallelBranchFlowReq;
				PushBranchFlowCharacteristics( LoopNum, LoopSideNum, MixerBranchOut, Node( FirstNodeOnBranchOut ).MassFlowRate, FirstHVACIteration );
				return;

				//IF INSUFFICIENT FLOW TO MEET ALL PARALLEL BRANCH FLOW REQUESTS
			} else if ( FlowRemaining < TotParallelBranchFlowReq ) {

				//DSU? didn't take the time to figure out what this should be... SplitterFlowIn = SplitterInletFlow(SplitNum)
				// 1) apportion flow based on requested fraction of total
				for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {

					SplitterBranchOut = this_loopside.Splitter( SplitNum ).BranchNumOut( OutletNum );
					ThisBranchRequest = DetermineBranchFlowRequest( LoopNum, LoopSideNum, SplitterBranchOut );
					FirstNodeOnBranch = this_loopside.Branch( SplitterBranchOut ).NodeNumIn;
					auto & this_splitter_outlet_branch( this_loopside.Branch( SplitterBranchOut ) );

					if ( ( this_splitter_outlet_branch.ControlType == ControlType_Active ) || ( this_splitter_outlet_branch.ControlType == ControlType_SeriesActive ) ) {

						// since we are calculating this fraction based on the total parallel request calculated above, we must mimic the logic to make sure the math works every time
						// that means we must make the variable speed pump correction here as well.
						for ( CompCounter = 1; CompCounter <= this_splitter_outlet_branch.TotalComponents; ++CompCounter ) {

							auto & this_comp( this_splitter_outlet_branch.Comp( CompCounter ) );

							//if this isn't a variable speed pump then just keep cycling
							if ( ( this_comp.TypeOf_Num != TypeOf_PumpVariableSpeed ) && ( this_comp.TypeOf_Num != TypeOf_PumpBankVariableSpeed ) ) {
								continue;
							}

							CompInletNode = this_comp.NodeNumIn;
							ThisBranchRequest = max( ThisBranchRequest, Node( CompInletNode ).MassFlowRateRequest );

						}

						ThisBranchRequestFrac = ThisBranchRequest / TotParallelBranchFlowReq;
						//    FracFlow = Node(FirstNodeOnBranch)%MassFlowRate/TotParallelBranchFlowReq
						//    Node(FirstNodeOnBranch)%MassFlowRate = MIN((FracFlow * Node(FirstNodeOnBranch)%MassFlowRate),FlowRemaining)
						Node( FirstNodeOnBranch ).MassFlowRate = ThisBranchRequestFrac * ThisLoopSideFlow;
						PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
						FlowRemaining -= Node( FirstNodeOnBranch ).MassFlowRate;

					}

				}

				// 1b) check if flow all apportioned
				if ( FlowRemaining > MassFlowTolerance ) {
					//Call fatal diagnostic error. !The math should work out!
					ShowSevereError( "ResolveParallelFlows: Dev note, failed to redistribute restricted flow" );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Loop side flow = " + RoundSigDigits( ThisLoopSideFlow, 8 ) + " (kg/s)" );
					ShowContinueError( "Flow Remaining = " + RoundSigDigits( FlowRemaining, 8 ) + " (kg/s)" );
					ShowContinueError( "Parallel Branch requests  = " + RoundSigDigits( TotParallelBranchFlowReq, 8 ) + " (kg/s)" );
				}

				// 2)  ! Reset the flow on the Mixer outlet branch
				MixerBranchOut = this_loopside.Mixer( SplitNum ).BranchNumOut;
				FirstNodeOnBranchOut = this_loopside.Branch( MixerBranchOut ).NodeNumIn;
				Node( FirstNodeOnBranchOut ).MassFlowRate = TotParallelBranchFlowReq;
				PushBranchFlowCharacteristics( LoopNum, LoopSideNum, MixerBranchOut, Node( FirstNodeOnBranchOut ).MassFlowRate, FirstHVACIteration );

			} // Total flow requested >= or < Total parallel request

		} // Spittler/Mixer exists

	}

	//==================================================================!
	//==================================================================!
	//==================================================================!

	void
	PropagateResolvedFlow(
		int const LoopNum,
		int const LoopSideNum,
		bool const FirstHVACIteration
	)
	{

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataPlant::PlantLoop;

		// Locals
		int const SplitNum( 1 );

		int OutletNum;
		int NumSplitOutlets;
		int SplitterBranchOut;
		int FirstNodeOnBranch;

		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).SplitterExists && PlantLoop( LoopNum ).LoopSide( LoopSideNum ).MixerExists ) {

			NumSplitOutlets = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Splitter( SplitNum ).TotalOutletNodes;
			for ( OutletNum = 1; OutletNum <= NumSplitOutlets; ++OutletNum ) {
				SplitterBranchOut = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Splitter( SplitNum ).BranchNumOut( OutletNum );
				FirstNodeOnBranch = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( SplitterBranchOut ).NodeNumIn;
				PushBranchFlowCharacteristics( LoopNum, LoopSideNum, SplitterBranchOut, Node( FirstNodeOnBranch ).MassFlowRate, FirstHVACIteration );
			}

		}

	}

	//==================================================================!
	//================= EVALUATING BRANCH REQUEST ======================!
	//==================================================================!

	Real64
	DetermineBranchFlowRequest(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   September 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will analyze the given branch and determine the representative
		//  flow request.

		// METHODOLOGY EMPLOYED:
		// Several possibilities are available.  In any case, the request is constrained to within
		//  branch outlet min/max avail.  This assumes that the component flow routines will properly
		//  propogate the min/max avail down the branch.
		// Some possibilities for flow request are:
		//  1) take the outlet flow rate -- assumes that the last component wins
		//  2) take the inlet flow rate request -- assumes that the request is propogated up and is good
		//  3) take the maximum request
		//  4) move down the loop and take the maximum "non-load-range-based" request within min/max avail bounds
		//     This assumes that load range based should not request flow for load-rejection purposes, and we
		//     should only "respond" to other component types.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::LoadRangeBasedMin;
		using DataPlant::LoadRangeBasedMax;
		using DataLoopNode::Node;
		using PlantUtilities::BoundValueToNodeMinMaxAvail;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;

		// Return value
		Real64 OverallFlowRequest;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const OutletFlowRate( 1 );
		int const InletFlowRequest( 2 );
		int const MaximumRequest( 3 );
		int const MaxNonLRBRequest( 4 );
		int const WhichRequestCalculation( InletFlowRequest );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompCounter;
		int CompInletNode;
		int BranchOutletNodeNum;
		int BranchInletNodeNum;

		auto & this_branch( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ) );

		//~ Initialize
		BranchInletNodeNum = this_branch.NodeNumIn;
		BranchOutletNodeNum = this_branch.NodeNumOut;
		OverallFlowRequest = 0.0;

		{ auto const SELECT_CASE_var( WhichRequestCalculation );

		if ( SELECT_CASE_var == OutletFlowRate ) {
			OverallFlowRequest = Node( BranchOutletNodeNum ).MassFlowRate;

		} else if ( SELECT_CASE_var == InletFlowRequest ) {
			if ( this_branch.ControlType != ControlType_SeriesActive ) {
				OverallFlowRequest = Node( BranchInletNodeNum ).MassFlowRateRequest;
			} else { // is series active, so take largest request of all the component inlet nodes
				for ( CompCounter = 1; CompCounter <= this_branch.TotalComponents; ++CompCounter ) {
					CompInletNode = this_branch.Comp( CompCounter ).NodeNumIn;
					OverallFlowRequest = max( OverallFlowRequest, Node( CompInletNode ).MassFlowRateRequest );
				}
			}

		} else if ( SELECT_CASE_var == MaximumRequest ) {
			// Assumes component inlet node is where request is held...could bandaid to include outlet node, but trying not to...
			for ( CompCounter = 1; CompCounter <= this_branch.TotalComponents; ++CompCounter ) {
				CompInletNode = this_branch.Comp( CompCounter ).NodeNumIn;
				OverallFlowRequest = max( OverallFlowRequest, Node( CompInletNode ).MassFlowRateRequest );
			}

		} else if ( SELECT_CASE_var == MaxNonLRBRequest ) {
			// Assumes component inlet node is where request is held...could bandaid to include outlet node, but trying not to...
			for ( CompCounter = 1; CompCounter <= this_branch.TotalComponents; ++CompCounter ) {
				{ auto const SELECT_CASE_var1( this_branch.Comp( CompCounter ).CurOpSchemeType );
				if ( ( SELECT_CASE_var1 >= LoadRangeBasedMin ) && ( SELECT_CASE_var1 <= LoadRangeBasedMax ) ) {
					// don't include this request
				} else {
					// include this
					CompInletNode = this_branch.Comp( CompCounter ).NodeNumIn;
					OverallFlowRequest = max( OverallFlowRequest, Node( CompInletNode ).MassFlowRateRequest );
				}}
			}

		}}

		//~ Now use a worker to bound the value to outlet min/max avail
		OverallFlowRequest = BoundValueToNodeMinMaxAvail( OverallFlowRequest, BranchOutletNodeNum );

		return OverallFlowRequest;

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   September 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine takes the flow resolved flow rate and pushes it
		//  down a branch.  In the process, if an externally connected
		//  component (air-water coil for example) is found to have a
		//  differing flow rate, the air sim flag is tripped to true, but
		//  the flow resolved flow rate is pushed down the loop to allow
		//  the plant to finish successfully.

		// METHODOLOGY EMPLOYED:
		// Push mass flow rate and max avail down each branch.  If the component
		//  is connected (or could be, for now) to an external loop such as
		//  an air loop, the current component outlet mass flow is checked
		//  vs the current resolved mass flow.  If the mass flow doesn't match,
		//  the air sim flag is tripped to true.

		// Currently this routine is only performed for starved branches, when
		//  the coil is requesting too much flow, more than the plant can provide.
		// If this were moved to every call type, including a minimum plant flow,
		//  you would need to provide a mass flow and min/max avail to push
		//  down the branch as well.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPlant; // Use the entire module to allow all TypeOf's, would be a huge ONLY list
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using PlantUtilities::CheckPlantConvergence;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompCounter;
		int BranchInletNode;
		int BranchOutletNode;
		int ComponentInletNode;
		int ComponentOutletNode;
		int ComponentTypeOfNum;
		Real64 MassFlowRateFound;
		Real64 MassFlow;
		bool PlantIsRigid;

		auto & this_loopside( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		auto & this_branch( this_loopside.Branch( BranchNum ) );

		BranchInletNode = this_branch.NodeNumIn;
		BranchOutletNode = this_branch.NodeNumOut;

		//~ Possible error handling if needed
		if ( ValueToPush != Node( BranchInletNode ).MassFlowRate ) {
			// Diagnostic problem, flow resolver isn't calling this routine properly
		}

		//~ This section would really be useful more later on if this routine has more logic regarding what to push down the branch
		MassFlow = ValueToPush;
		//MinAvail = ValueToPush
		// MaxAvail = ValueToPush

		PlantIsRigid = CheckPlantConvergence( LoopNum, LoopSideNum, FirstHVACIteration );

		//~ Loop across all component outlet nodes and update their mass flow and max avail
		for ( CompCounter = 1; CompCounter <= this_branch.TotalComponents; ++CompCounter ) {

			auto & this_comp( this_branch.Comp( CompCounter ) );

			//~ Pick up some values for convenience
			ComponentInletNode = this_comp.NodeNumIn;
			ComponentOutletNode = this_comp.NodeNumOut;
			MassFlowRateFound = Node( ComponentOutletNode ).MassFlowRate;
			ComponentTypeOfNum = this_comp.TypeOf_Num;

			//~ Push the values through
			Node( ComponentOutletNode ).MassFlowRate = MassFlow;

			if ( PlantIsRigid ) {
				Node( ComponentInletNode ).MassFlowRateMinAvail = MassFlow;
				Node( ComponentInletNode ).MassFlowRateMaxAvail = MassFlow;
				Node( ComponentOutletNode ).MassFlowRateMinAvail = MassFlow;
				Node( ComponentOutletNode ).MassFlowRateMaxAvail = MassFlow;
			}
			//Node(ComponentOutletNode)%MassFlowRateMinAvail = MinAvail
			// no this is 2-way valve which messes up flow options
			//      for demand components Node(ComponentOutletNode)%MassFlowRateMaxAvail = MaxAvail

			//~ If this value matches then we are good to move to the next component
			if ( std::abs( MassFlow - MassFlowRateFound ) < CriteriaDelta_MassFlowRate ) continue;
			//~ Since there is a difference, we have to decide what to do based on the component type:
			//~  For plant connections, don't do anything, it SHOULD work itself out
			//~  For air connections, trip the LoopSide air flag
			//~  Similar for zone, none zone, and electric load center
			{ auto const SELECT_CASE_var( ComponentTypeOfNum );

			// possibly air-connected components
			if ( ( SELECT_CASE_var == TypeOf_CoilWaterCooling ) || ( SELECT_CASE_var == TypeOf_CoilWaterDetailedFlatCooling ) || ( SELECT_CASE_var == TypeOf_CoilWaterSimpleHeating ) || ( SELECT_CASE_var == TypeOf_CoilSteamAirHeating ) || ( SELECT_CASE_var == TypeOf_CoilWAHPHeatingEquationFit ) || ( SELECT_CASE_var == TypeOf_CoilWAHPCoolingEquationFit ) || ( SELECT_CASE_var == TypeOf_CoilWAHPHeatingParamEst ) || ( SELECT_CASE_var == TypeOf_CoilWAHPCoolingParamEst ) || ( SELECT_CASE_var == TypeOf_CoilUserDefined ) || ( SELECT_CASE_var == TypeOf_CoilVSWAHPCoolingEquationFit ) || ( SELECT_CASE_var == TypeOf_CoilVSWAHPHeatingEquationFit ) || ( SELECT_CASE_var == TypeOf_PackagedTESCoolingCoil ) ) {

				this_loopside.SimAirLoopsNeeded = true;
				//sometimes these coils are children in ZoneHVAC equipment
				// PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimZoneEquipNeeded= .TRUE.

			} else if ( ( SELECT_CASE_var == TypeOf_Baseboard_Conv_Water ) || ( SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Steam ) || ( SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Water ) || ( SELECT_CASE_var == TypeOf_LowTempRadiant_VarFlow ) || ( SELECT_CASE_var == TypeOf_LowTempRadiant_ConstFlow ) || ( SELECT_CASE_var == TypeOf_CooledBeamAirTerminal ) || ( SELECT_CASE_var == TypeOf_ZoneHVACAirUserDefined ) || ( SELECT_CASE_var == TypeOf_AirTerminalUserDefined ) || ( SELECT_CASE_var == TypeOf_FourPipeBeamAirTerminal ) ) { //zone connected components

				this_loopside.SimZoneEquipNeeded = true;

			} else if ( ( SELECT_CASE_var == TypeOf_Generator_FCExhaust ) || ( SELECT_CASE_var == TypeOf_Generator_FCStackCooler ) || ( SELECT_CASE_var == TypeOf_Generator_MicroCHP ) || ( SELECT_CASE_var == TypeOf_Generator_MicroTurbine ) || ( SELECT_CASE_var == TypeOf_Generator_ICEngine ) || ( SELECT_CASE_var == TypeOf_Generator_CTurbine ) ) { //electric center connected components

				this_loopside.SimElectLoadCentrNeeded = true;

			}}

		}

	}

	//==================================================================!
	//================== REPORT VARIABLE UPDATE ========================!
	//==================================================================!

	void
	UpdateLoopSideReportVars(
		int const LoopNum,
		int const LoopSide,
		Real64 const OtherSideDemand, // This is the 'other side' demand, based on other side flow
		Real64 const LocalRemLoopDemand // Unmet Demand after equipment has been simulated (report variable)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 1998
		//       MODIFIED       Aug 2010 Edwin Lee -- add per LoopSide variable support
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update the report variables

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::PlantReport;
		using DataPlant::SupplySide;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// and delta T (inlet to SetPt)
		// This is evaluated once at the beginning of the loop side solver, before
		//  any of this side equipment alters it

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		// store node data in plant
		auto & this_supplyside( PlantLoop( LoopNum ).LoopSide( SupplySide ) );
		auto & this_loop_report( PlantReport( LoopNum ) );

		if ( LoopSide == SupplySide ) {
			this_loop_report.InletNodeFlowrate = Node( this_supplyside.NodeNumIn ).MassFlowRate;
			this_loop_report.InletNodeTemperature = Node( this_supplyside.NodeNumIn ).Temp;
			this_loop_report.OutletNodeFlowrate = Node( this_supplyside.NodeNumOut ).MassFlowRate;
			this_loop_report.OutletNodeTemperature = Node( this_supplyside.NodeNumOut ).Temp;

			// In the baseline code, only reported supply side demand. so putting in "SupplySide" IF block for now but might expand later
			if ( OtherSideDemand < 0.0 ) {
				this_loop_report.CoolingDemand = std::abs( OtherSideDemand );
				this_loop_report.HeatingDemand = 0.0;
				this_loop_report.DemandNotDispatched = -LocalRemLoopDemand; //  Setting sign based on old logic for now
			} else {
				this_loop_report.HeatingDemand = OtherSideDemand;
				this_loop_report.CoolingDemand = 0.0;
				this_loop_report.DemandNotDispatched = LocalRemLoopDemand; //  Setting sign based on old logic for now
			}

			CalcUnmetPlantDemand( LoopNum, LoopSide );

		}

	}

	void
	CalcUnmetPlantDemand(
		int const LoopNum,
		int const LoopSideNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// determine the magnitude of unmet plant loads after the half loop simulation is done

		// METHODOLOGY EMPLOYED:
		// using the loop setpoint node, look at target vs current and
		// calculate a demand based on mass flow times specific heat times delta T

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::PlantReport;
		using DataPlant::LoopDemandTol;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using DataLoopNode::NodeType_Water;
		using DataLoopNode::NodeType_Steam;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetSatEnthalpyRefrig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PlantLoopSolver::EvaluateLoopSetPointLoad" );
		static std::string const RoutineNameAlt( "PlantSupplySide:EvaluateLoopSetPointLoad" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		//~ General variables
		Real64 MassFlowRate;
		Real64 TargetTemp;
		Real64 LoopSetPointTemperature;
		Real64 LoopSetPointTemperatureHi;
		Real64 LoopSetPointTemperatureLo;
		Real64 LoadToHeatingSetPoint;
		Real64 LoadToCoolingSetPoint;
		Real64 DeltaTemp;
		Real64 Cp;
		Real64 EnthalpySteamSatVapor; // Enthalpy of saturated vapor
		Real64 EnthalpySteamSatLiquid; // Enthalpy of saturated liquid
		Real64 LatentHeatSteam; // Latent heat of steam
		Real64 LoadToLoopSetPoint;

		// Initialize
		LoadToLoopSetPoint = 0.0;
		auto & this_loop( PlantLoop( LoopNum ) );

		// Get temperature at loop setpoint node.
		TargetTemp = Node( this_loop.TempSetPointNodeNum ).Temp;
		MassFlowRate = Node( this_loop.TempSetPointNodeNum ).MassFlowRate;

		if ( this_loop.FluidType == NodeType_Water ) {

			Cp = GetSpecificHeatGlycol( this_loop.FluidName, TargetTemp, this_loop.FluidIndex, RoutineName );

			{ auto const SELECT_CASE_var( this_loop.LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {

				// Pick up the loop setpoint temperature
				LoopSetPointTemperature = this_loop.LoopSide( LoopSideNum ).TempSetPoint;
				// Calculate the delta temperature
				DeltaTemp = LoopSetPointTemperature - TargetTemp;

				// Calculate the demand on the loop
				LoadToLoopSetPoint = MassFlowRate * Cp * DeltaTemp;

			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {

				// Get the range of setpoints
				LoopSetPointTemperatureHi = Node( this_loop.TempSetPointNodeNum ).TempSetPointHi;
				LoopSetPointTemperatureLo = Node( this_loop.TempSetPointNodeNum ).TempSetPointLo;

				//Calculate the demand on the loop
				if ( MassFlowRate > 0.0 ) {
					LoadToHeatingSetPoint = MassFlowRate * Cp * ( LoopSetPointTemperatureLo - TargetTemp );
					LoadToCoolingSetPoint = MassFlowRate * Cp * ( LoopSetPointTemperatureHi - TargetTemp );
					// Possible combinations:
					// 1  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
					// 2  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
					// 3  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
					// 4  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Not Feasible if LoopSetPointHi >= LoopSetPointLo
					if ( LoadToHeatingSetPoint > 0.0 && LoadToCoolingSetPoint > 0.0 ) {
						LoadToLoopSetPoint = LoadToHeatingSetPoint;
					} else if ( LoadToHeatingSetPoint < 0.0 && LoadToCoolingSetPoint < 0.0 ) {
						LoadToLoopSetPoint = LoadToCoolingSetPoint;
					} else if ( LoadToHeatingSetPoint <= 0.0 && LoadToCoolingSetPoint >= 0.0 ) { // deadband includes zero loads
						LoadToLoopSetPoint = 0.0;
					}
				} else {
					LoadToLoopSetPoint = 0.0;
				}

			}}

		} else if ( this_loop.FluidType == NodeType_Steam ) {

			Cp = GetSpecificHeatGlycol( this_loop.FluidName, TargetTemp, this_loop.FluidIndex, RoutineName );

			{ auto const SELECT_CASE_var( this_loop.LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {

				// Pick up the loop setpoint temperature
				LoopSetPointTemperature = this_loop.LoopSide( LoopSideNum ).TempSetPoint;

				// Calculate the delta temperature
				DeltaTemp = LoopSetPointTemperature - TargetTemp;

				EnthalpySteamSatVapor = GetSatEnthalpyRefrig( fluidNameSteam, LoopSetPointTemperature, 1.0, RefrigIndex, RoutineNameAlt );
				EnthalpySteamSatLiquid = GetSatEnthalpyRefrig( fluidNameSteam, LoopSetPointTemperature, 0.0, RefrigIndex, RoutineNameAlt );

				LatentHeatSteam = EnthalpySteamSatVapor - EnthalpySteamSatLiquid;

				// Calculate the demand on the loop
				LoadToLoopSetPoint = MassFlowRate * ( Cp * DeltaTemp + LatentHeatSteam );

			}}

		} else { // only have two types, water serves for glycol.

		}

		// Trim the demand to zero if it is very small
		if ( std::abs( LoadToLoopSetPoint ) < LoopDemandTol ) LoadToLoopSetPoint = 0.0;

		PlantReport( LoopNum ).UnmetDemand = LoadToLoopSetPoint;

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   October 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the temperature
		// and mass flow rate of the plant loop supply side exit
		// node.  As written, the routine calculates the exit
		// temperature based on the fraction of loop demand met
		// by the plant equipment.  This assumes that each piece
		// of operating plant equipment produced chilled/hot water
		// at the loop setpoint temperature.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using DataPlant::PlantLoop;
		using DataPlant::SupplySide;
		using DataPlant::DemandSide;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using DataLoopNode::NodeID;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopInlet; // plant loop inlet node num.
		int LoopOutlet; // plant loop outlet node num.

		//set local variables: loop inlet and outlet nodes
		LoopInlet = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn;
		LoopOutlet = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut;
		//Check continuity invalid...loop pumps now turned on and off
		if ( ! FirstHVACIteration && ! WarmupFlag ) {
			if ( std::abs( Node( LoopOutlet ).MassFlowRate - Node( LoopInlet ).MassFlowRate ) > MassFlowTolerance ) {
				if ( PlantLoop( LoopNum ).MFErrIndex == 0 ) {
					ShowWarningError( "PlantSupplySide: PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Error (CheckLoopExitNode) -- Mass Flow Rate Calculation. Outlet and Inlet differ by more than tolerance." );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Loop inlet node=" + NodeID( LoopInlet ) + ", flowrate=" + RoundSigDigits( Node( LoopInlet ).MassFlowRate, 4 ) + " kg/s" );
					ShowContinueError( "Loop outlet node=" + NodeID( LoopOutlet ) + ", flowrate=" + RoundSigDigits( Node( LoopOutlet ).MassFlowRate, 4 ) + " kg/s" );
					ShowContinueError( "This loop might be helped by a bypass." );
				}
				ShowRecurringWarningErrorAtEnd( "PlantSupplySide: PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Error -- Mass Flow Rate Calculation -- continues ** ", PlantLoop( LoopNum ).MFErrIndex );
			}
		}
		//Reset Max loop flow rate based on pump performance
		Node( LoopOutlet ).MassFlowRateMax = Node( LoopInlet ).MassFlowRateMax;

	}

	void
	AdjustPumpFlowRequestByEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		Real64 & FlowToRequest
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// modify flow request to pump simulation if EMS is overriding pump component

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		auto & this_loopside( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		auto & this_branch( this_loopside.Branch( BranchNum ) );
		auto & this_comp( this_branch.Comp( CompNum ) );

		if ( ( this_loopside.EMSCtrl ) && ( this_loopside.EMSValue <= 0.0 ) ) {
			FlowToRequest = 0.0;
			return;
		}

		if ( ( this_branch.EMSCtrlOverrideOn ) && ( this_branch.EMSCtrlOverrideValue <= 0.0 ) ) {
			FlowToRequest = 0.0;
			return;
		}

		if ( this_comp.EMSLoadOverrideOn ) {
			if ( this_comp.EMSLoadOverrideValue == 0.0 ) {
				FlowToRequest = 0.0;
			}
		}

	}

	//==================================================================!
	//==================================================================!
	//==================================================================!

} // PlantLoopSolver

} // EnergyPlus
