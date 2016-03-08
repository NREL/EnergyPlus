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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <PlantPressureSystem.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <OutputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantPressureSystem {

	// Module containing the routines dealing with the PlantPressureSystem simulation

	// MODULE INFORMATION:
	//       AUTHOR         Edwin Lee
	//       DATE WRITTEN   August 2009
	//       MODIFIED       February 2010: Add phase 2: loop flow correction
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module manages plant pressure-based simulations

	// METHODOLOGY EMPLOYED:
	// General EnergyPlus Methodology:

	// REFERENCES:
	// na

	// OTHER NOTES:
	//  Phase 1: Pump Power Correction: -Loop/Parallel flows are not resolved based on pressure drop
	//                                  -Every flow path must see at least one branch with pressure information
	//                                  -Pump power is updated based on the required pump head
	//  Phase 2: Pump Flow Correction: -Loop flow resolved based on pump curve and loop pressure drop
	//                                 -Parallel flows not resolved
	//                                 -Every flow path must see at least one branch with pressure information
	//                                 -Pump curve must be given also
	//  Phase 3: Pressure Simulation: -Loop and parallel flows are resolved
	//                                -All branches must have pressure information and pump must have pump curve
	//                                -Not currently implemented

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::Pi;
	using namespace DataBranchAirLoopPlant;

	// Data
	// MODULE PARAMETER/ENUMERATIONS DEFINITIONS:
	static std::string const BlankString;
	namespace{
		bool InitPressureDropOneTimeInit( true );
	}
	// DERIVED TYPE DEFINITIONS:
	//TYPE, PUBLIC:: PlantPressureCurveData
	//  CHARACTER(len=MaxNameLength) :: Name                    = Blank
	//  REAL(r64)                    :: EquivDiameter           = 0.0d0   !- An effective diameter for calculation of Re & e/D [m]
	//  REAL(r64)                    :: MinorLossCoeff          = 0.0d0   !- K factor                                          [-]
	//  REAL(r64)                    :: EquivLength             = 0.0d0   !- An effective length to apply friction calculation [m]
	//  REAL(r64)                    :: EquivRoughness          = 0.0d0   !- An effective roughness (e) to calculate e/D       [m]
	//  LOGICAL                      :: ConstantFPresent        = .FALSE. !- Signal for if a constant value of f was entered
	//  REAL(r64)                    :: ConstantF               = 0.0d0   !- Constant value of f (if applicable)               [-]
	//END TYPE PlantPressureCurveData
	//          ! MODULE VARIABLE DECLARATIONS:
	//TYPE(PlantPressureCurveData), ALLOCATABLE, DIMENSION(:),PUBLIC :: PressureCurve
	//LOGICAL  :: GetInputFlag = .TRUE. !Module level, since GetInput could be called by SIMPRESSUREDROP or by BRANCHINPUTMANAGER

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Driver/Manager Routines

	// Initialization routines for module
	//PRIVATE GetPressureSystemInput

	// Algorithms/Calculation routines for the module
	//PRIVATE PressureCurveValue
	//PRIVATE CalculateMoodyFrictionFactor

	// Update routines to check convergence and update nodes

	// Utility routines for module

	// Public Utility routines
	//PUBLIC GetPressureCurveTypeAndIndex

	// Functions
	void
	clear_state()
	{
		InitPressureDropOneTimeInit = true;
	}

	void
	SimPressureDropSystem(
		int const LoopNum, // Plant Loop to update pressure information
		bool const FirstHVACIteration, // System flag
		int const CallType, // Enumerated call type
		Optional_int_const LoopSideNum, // Loop side num for specific branch simulation
		Optional_int_const BranchNum // Branch num for specific branch simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine is the public interface for pressure system simulation
		// Calls are made to private components as needed

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PressureCall_Init;
		using DataPlant::PressureCall_Calc;
		using DataPlant::PressureCall_Update;
		using DataPlant::PlantLoop;
		using DataPlant::Press_NoPressure;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		//Check if we need to get pressure curve input data
		//  IF (GetInputFlag) CALL GetPressureSystemInput

		//Exit out of any calculation routines if we don't do pressure simulation for this loop
		if ( ( PlantLoop( LoopNum ).PressureSimType == Press_NoPressure ) && ( ( CallType == PressureCall_Calc ) || ( CallType == PressureCall_Update ) ) ) return;

		//Pass to another routine based on calling flag
		{ auto const SELECT_CASE_var( CallType );
		if ( SELECT_CASE_var == PressureCall_Init ) {
			InitPressureDrop( LoopNum, FirstHVACIteration );
		} else if ( SELECT_CASE_var == PressureCall_Calc ) {
			BranchPressureDrop( LoopNum, LoopSideNum, BranchNum ); //Autodesk:OPTIONAL LoopSideNum, BranchNum used without PRESENT check
		} else if ( SELECT_CASE_var == PressureCall_Update ) {
			UpdatePressureDrop( LoopNum );
		} else {
			//Calling routines should only use the three possible keywords here
		}}

	}

	//=================================================================================================!

	//SUBROUTINE GetPressureSystemInput()
	// Getinput for PressureSystem moved to CurveManager module
	//=================================================================================================!

	void
	InitPressureDrop(
		int const LoopNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes output variables and data structure
		// On FirstHVAC, updates the demand inlet node pressure

		// METHODOLOGY EMPLOYED:
		// General EnergyPlus Methodology

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataPlant::Press_NoPressure;
		using DataPlant::CommonPipe_No;
		using DataEnvironment::StdBaroPress;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//Initialization Variables

		static Array1D_bool LoopInit;

		//Simulation Variables
		int NumBranches;
		int BranchPressureTally;
		static Array1D_bool FullParallelBranchSetFound( 2 );
		static bool CommonPipeErrorEncountered( false );

		if ( InitPressureDropOneTimeInit ) {
			//First allocate the initialization array to each plant loop
			LoopInit.allocate( size( PlantLoop ) );
			LoopInit = true;
			InitPressureDropOneTimeInit = false;
		}

		auto & loop( PlantLoop( LoopNum ) );

		// CurrentModuleObject='Curve:Functional:PressureDrop'
		if ( LoopInit( LoopNum ) ) {

			//Initialize
			bool ErrorsFound( false );
			bool SeriesPressureComponentFound( false );

			//Need to go along plant loop and set up component pressure drop data structure!
			for ( int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				auto & loop_side( loop.LoopSide( LoopSideNum ) );

				//Loop through all branches on this loop side
				for ( int BranchNum = 1; BranchNum <= isize( loop_side.Branch ); ++BranchNum ) {
					auto & branch( loop_side.Branch( BranchNum ) );

					//If this branch has valid pressure drop data
					if ( branch.PressureCurveIndex > 0 ) {

						//Update flags for higher level structure
						branch.HasPressureComponents = true;
						loop_side.HasPressureComponents = true;
						loop.HasPressureComponents = true;

						//Setup output variable
						SetupOutputVariable( "Plant Branch Pressure Difference [Pa]", branch.PressureDrop, "Plant", "Average", branch.Name );

					}

				}

				//Set up LoopSide level variables if applicable
				if ( loop_side.HasPressureComponents ) {
					if ( LoopSideNum == DemandSide ) {

						SetupOutputVariable( "Plant Demand Side Loop Pressure Difference [Pa]", loop_side.PressureDrop, "Plant", "Average", loop.Name );

					} else if ( LoopSideNum == SupplySide ) {

						SetupOutputVariable( "Plant Supply Side Loop Pressure Difference [Pa]", loop_side.PressureDrop, "Plant", "Average", loop.Name );

					}
				}

			}

			if ( loop.HasPressureComponents ) {
				FullParallelBranchSetFound( DemandSide ) = FullParallelBranchSetFound( SupplySide ) = false;

				//Set up loop level variables if applicable

				SetupOutputVariable( "Plant Loop Pressure Difference [Pa]", loop.PressureDrop, "Plant", "Average", loop.Name );

				//Check for illegal configurations on this plant loop
				for ( int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
					//Check for illegal parallel branch setups
					auto & loop_side( loop.LoopSide( LoopSideNum ) );
					BranchPressureTally = 0;
					NumBranches = size( loop_side.Branch );
					if ( NumBranches > 2 ) {
						for ( int BranchNum = 2; BranchNum <= NumBranches - 1; ++BranchNum ) {
							if ( loop_side.Branch( BranchNum ).HasPressureComponents ) {
								loop_side.HasParallelPressComps = true;
								++BranchPressureTally;
							}
						}
					}
					if ( BranchPressureTally == 0 ) {
						//no parallel branches, ok for this check
					} else if ( BranchPressureTally == isize( loop_side.Branch ) - 2 ) {
						//all parallel branches have pressure components
						FullParallelBranchSetFound( LoopSideNum ) = true;
					} else {
						//we aren't ok
						ShowSevereError( "Pressure drop component configuration error detected on loop: " + loop.Name );
						ShowContinueError( "Pressure drop components must be on ALL or NONE of the parallel branches." );
						ShowContinueError( "Partial distribution is not allowed." );
						ErrorsFound = true;
					}
					if ( loop_side.Branch( 1 ).HasPressureComponents || loop_side.Branch( NumBranches ).HasPressureComponents ) {
						//we have a series component pressure branch (whether a single branch half loop or mixer/splitter setup
						SeriesPressureComponentFound = true;
					}
				}

				//Check for full path pressure data
				if ( FullParallelBranchSetFound( DemandSide ) || FullParallelBranchSetFound( SupplySide ) || SeriesPressureComponentFound ) {
					//we are fine, either way we will always have a path with at least one pressure component hit
				} else {
					ShowSevereError( "Pressure drop component configuration error detected on loop: " + loop.Name );
					ShowContinueError( "The loop has at least one fluid path which does not encounter a pressure component." );
					ShowContinueError( "Either use at least one serial component for pressure drop OR all possible parallel paths" );
					ShowContinueError( "must be pressure drop components." );
					ErrorsFound = true;
				} //valid pressure path

			} //Has pressure components

			if ( ErrorsFound ) ShowFatalError( "Preceding errors cause program termination" );

			//Also issue one time warning if there is a mismatch between plant loop simulation type and whether objects were entered
			if ( loop.HasPressureComponents && ( loop.PressureSimType == Press_NoPressure ) ) {
				//Then we found pressure components on the branches, but the plant loop said it didn't want to do pressure simulation
				ShowWarningError( "Error for pressure simulation on plant loop: " + loop.Name );
				ShowContinueError( "Plant loop contains pressure simulation components on the branches," );
				ShowContinueError( " yet in the PlantLoop object, there is no pressure simulation specified." );
				ShowContinueError( "Simulation continues, ignoring pressure simulation data." );
			} else if ( ( ! loop.HasPressureComponents ) && ( loop.PressureSimType != Press_NoPressure ) ) {
				//Then we don't have any pressure components on the branches, yet the plant loop wants to do some sort of pressure simulation
				ShowWarningError( "Error for pressure simulation on plant loop: " + loop.Name );
				ShowContinueError( "Plant loop is requesting a pressure simulation," );
				ShowContinueError( " yet there are no pressure simulation components detected on any of the branches in that loop." );
				ShowContinueError( "Simulation continues, ignoring pressure simulation data." );
			}

			LoopInit( LoopNum ) = false;

		} //LoopInit = TRUE

		//Initialize the entire plant loop to the outdoor pressure if that loop has data
		//This value at the demand side outlet node will be used as a starting reference point
		// for pressure calcs
		//The value is smeared across the loop, however, so that any nodes before a pump will
		// have a proper value for pressure
		if ( loop.HasPressureComponents && FirstHVACIteration ) {
			for ( int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				auto const & loop_side( loop.LoopSide( LoopSideNum ) );
				for ( int BranchNum = 1, BranchNum_end = isize( loop_side.Branch ); BranchNum <= BranchNum_end; ++BranchNum ) {
					auto const & branch( loop_side.Branch( BranchNum ) );
					for ( int CompNum = 1, CompNum_end = isize( branch.Comp ); CompNum <= CompNum_end; ++CompNum ) {
						auto const & component( branch.Comp( CompNum ) );
						Node( component.NodeNumIn ).Press = StdBaroPress;
						Node( component.NodeNumOut ).Press = StdBaroPress;
					}
				}
			}
		}

		//Now tell the pump routine whether or not to use the pressure data to calculate power
		if ( loop.HasPressureComponents ) {
			loop.UsePressureForPumpCalcs = ! FirstHVACIteration;
		} else { //No Pressure Components
			loop.UsePressureForPumpCalcs = false;
		}

		//Before we leave, override any settings in case we are doing common pipe simulation
		if ( loop.HasPressureComponents ) {
			//We need to make sure we aren't doing an invalid configuration here
			if ( loop.CommonPipeType != CommonPipe_No ) {
				//There is a common pipe!
				if ( ! CommonPipeErrorEncountered ) {
					ShowSevereError( "Invalid pressure simulation configuration for Plant Loop=" + loop.Name );
					ShowContinueError( "Currently pressure simulations cannot be performed for loops with common pipes." );
					ShowContinueError( "To repair, either remove the common pipe simulation, or remove the pressure simulation." );
					ShowContinueError( "The simulation will continue, but the pump power is not updated with pressure drop data." );
					ShowContinueError( "Check all results including node pressures to ensure proper simulation." );
					ShowContinueError( "This message is reported once, but may have been encountered in multiple loops." );
					CommonPipeErrorEncountered = true;
				}
				loop.UsePressureForPumpCalcs = false;
			}
		}

	}

	//=================================================================================================!

	void
	BranchPressureDrop(
		int const LoopNum, // Plant Loop Index
		int const LoopSideNum, // LoopSide Index (1=Demand, 2=Supply) on Plant Loop LoopNum
		int const BranchNum // Branch Index on LoopSide LoopSideNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This will choose an appropriate pressure drop calculation routine based on structure flags

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus Methodology

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using DataPlant::PlantLoop;
		using CurveManager::CurveValue;
		using CurveManager::PressureCurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcPlantPressureSystem" );
		static std::string const DummyFluid;

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FluidIndex; // Plant loop level Fluid Index
		int InletNodeNum; // Component inlet node number
		int OutletNodeNum; // Component outlet node number
		int PressureCurveType; // Type of curve used to evaluate pressure drop
		int PressureCurveIndex; // Curve index for PerfCurve structure
		Real64 NodeMassFlow; // Nodal mass flow rate {kg/s}
		Real64 NodeTemperature; // Nodal temperature {C}
		Real64 NodeDensity; // Nodal density {kg/m3}
		Real64 NodeViscosity; // Nodal viscosity, assuming mu here (dynamic viscosity)
		Real64 BranchDeltaPress( 0.0 ); // Pressure drop for component, {Pa}
		static int ErrorCounter( 0 ); // For proper error handling

		//Exit early if need be
		if ( ! PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).HasPressureComponents ) {
			PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureDrop = 0.0;
			PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureEffectiveK = 0.0;
			return;
		}

		//Get data from data structure
		FluidIndex = PlantLoop( LoopNum ).FluidIndex;
		InletNodeNum = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumIn;
		OutletNodeNum = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumOut;
		PressureCurveType = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureCurveType;
		PressureCurveIndex = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureCurveIndex;

		//Get nodal conditions
		NodeMassFlow = Node( InletNodeNum ).MassFlowRate;
		NodeTemperature = Node( InletNodeNum ).Temp;
		NodeDensity = GetDensityGlycol( DummyFluid, NodeTemperature, FluidIndex, RoutineName );
		NodeViscosity = GetViscosityGlycol( DummyFluid, NodeTemperature, FluidIndex, RoutineName );

		//Call the appropriate pressure calculation routine
		{ auto const SELECT_CASE_var( PressureCurveType );
		if ( SELECT_CASE_var == PressureCurve_Pressure ) {
			//DeltaP = [f*(L/D) + K] * (rho * V^2) / 2
			BranchDeltaPress = PressureCurveValue( PressureCurveIndex, NodeMassFlow, NodeDensity, NodeViscosity );

		} else if ( SELECT_CASE_var == PressureCurve_Generic ) {
			//DeltaP = func(mdot)
			//Generic curve, only pass V1=mass flow rate
			BranchDeltaPress = CurveValue( PressureCurveIndex, NodeMassFlow );

		} else {
			//Shouldn't end up here, but just in case
			++ErrorCounter;
			if ( ErrorCounter == 1 ) {
				ShowSevereError( "Plant pressure simulation encountered a branch which contains invalid branch pressure curve type." );
				ShowContinueError( "Occurs for branch: " + PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Name );
				ShowContinueError( "This error will be issued only once, although other branches may encounter the same problem" );
				ShowContinueError( "For now, pressure drop on this branch will be set to zero." );
				ShowContinueError( "Verify all pressure inputs and pressure drop output variables to ensure proper simulation" );
			}

		}}

		//Log this pressure in the data structure to be handled by the update routine later
		PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureDrop = BranchDeltaPress;

		//Update the effective K-value for this branch
		if ( NodeMassFlow > 0.0 ) {
			PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureEffectiveK = BranchDeltaPress / pow_2( NodeMassFlow );
		} else {
			PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureEffectiveK = 0.0;
		}

	}

	//=================================================================================================!

	//REAL(r64) FUNCTION PressureCurveValue(PressureCurveIndex, MassFlow, Density, Viscosity)

	//          ! FUNCTION INFORMATION:
	//          !       AUTHOR         Edwin Lee
	//          !       DATE WRITTEN   August 2009
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS FUNCTION:
	//          ! This will evaluate the pressure drop for components which use pressure information

	//          ! METHODOLOGY EMPLOYED:
	//          ! Friction factor pressure drop equation:
	//          ! DP = [f*(L/D) + K] * (rho * V^2) / 2

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//  USE DataPlant, ONLY : MassFlowTol
	//  Use DataGlobals, ONLY : Pi

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! FUNCTION ARGUMENT DEFINITIONS:
	//  INTEGER, INTENT(IN)      ::  PressureCurveIndex
	//  REAL(r64), INTENT(IN)    ::  MassFlow
	//  REAL(r64), INTENT(IN)    ::  Density
	//  REAL(r64), INTENT(IN)    ::  Viscosity

	//          ! FUNCTION PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
	//  REAL(r64)                ::  Diameter
	//  REAL(r64)                ::  MinorLossCoeff
	//  REAL(r64)                ::  Length
	//  REAL(r64)                ::  Roughness
	//  LOGICAL                  ::  IsConstFPresent
	//  REAL(r64)                ::  ConstantF
	//  REAL(r64)                ::  FrictionFactor
	//  REAL(r64)                ::  CrossSectArea
	//  REAL(r64)                ::  Velocity
	//  REAL(r64)                ::  ReynoldsNumber
	//  REAL(r64)                ::  RoughnessRatio

	//  !Retrieve data from structure
	//  Diameter        = PressureCurve(PressureCurveIndex)%EquivDiameter
	//  MinorLossCoeff  = PressureCurve(PressureCurveIndex)%MinorLossCoeff
	//  Length          = PressureCurve(PressureCurveIndex)%EquivLength
	//  Roughness       = PressureCurve(PressureCurveIndex)%EquivRoughness
	//  IsConstFPresent = PressureCurve(PressureCurveIndex)%ConstantFPresent
	//  ConstantF       = PressureCurve(PressureCurveIndex)%ConstantF

	//  !Intermediate calculations
	//  CrossSectArea         =  (Pi / 4.0d0) * Diameter**2
	//  Velocity              =  MassFlow / (Density * CrossSectArea)
	//  ReynoldsNumber        =  Density * Diameter * Velocity / Viscosity !assuming mu here
	//  RoughnessRatio        =  Roughness / Diameter

	//  !If we don't have any flow then exit out
	//  IF (MassFlow .LT. MassFlowTol) THEN
	//    PressureCurveValue = 0.0d0
	//    RETURN
	//  END IF

	//  !Calculate the friction factor
	//  IF (IsConstFPresent) THEN   !use the constant value
	//    FrictionFactor    =  ConstantF
	//  ELSE ! must calculate f
	//    FrictionFactor    =  CalculateMoodyFrictionFactor(ReynoldsNumber,RoughnessRatio)
	//  END IF

	//  !Pressure drop calculation
	//  PressureCurveValue  =  (FrictionFactor * (Length / Diameter) + MinorLossCoeff) * (Density * Velocity**2) / 2.0d0

	//END FUNCTION PressureCurveValue

	//=================================================================================================!

	//REAL(r64) FUNCTION CalculateMoodyFrictionFactor(ReynoldsNumber, RoughnessRatio)

	//          ! FUNCTION INFORMATION:
	//          !       AUTHOR         Edwin Lee
	//          !       DATE WRITTEN   August 2009
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS FUNCTION:
	//          ! This will evaluate the moody friction factor based on Reynolds number and roughness ratio

	//          ! METHODOLOGY EMPLOYED:
	//          ! General empirical correlations for friction factor based on Moody Chart data

	//          ! REFERENCES:
	//          ! Haaland, SE (1983). "Simple and Explicit Formulas for the Friction Factor in Turbulent Flow".
	//          !   Trans. ASIVIE, J. of Fluids Engineering 103: 89-90.

	//          ! USE STATEMENTS:
	//  USE General, ONLY: RoundSigDigits

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! FUNCTION ARGUMENT DEFINITIONS:
	//  REAL(r64), INTENT(IN)    ::  ReynoldsNumber
	//  REAL(r64), INTENT(IN)    ::  RoughnessRatio

	//          ! FUNCTION PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
	//  REAL(r64)                    ::  Term1, Term2, Term3
	//  CHARACTER(len=MaxNameLength) ::  RR, Re
	//  LOGICAL, SAVE                ::  FrictionFactorErrorHasOccurred = .FALSE.

	//  !Check for no flow before calculating values
	//  IF (ReynoldsNumber .EQ. 0.0d0) THEN
	//    CalculateMoodyFrictionFactor = 0.0d0
	//    RETURN
	//  END IF

	//  !Check for no roughness also here
	//  IF (RoughnessRatio .EQ. 0.0d0) THEN
	//    CalculateMoodyFrictionFactor = 0.0d0
	//    RETURN
	//  END IF

	//  !Calculate the friction factor
	//  Term1 = (RoughnessRatio/3.7d0)**(1.11d0)
	//  Term2 = 6.9d0/ReynoldsNumber
	//  Term3 = -1.8d0 * LOG10(Term1 + Term2)
	//  IF (Term3 .NE. 0.0d0) THEN
	//    CalculateMoodyFrictionFactor = Term3 ** (-2.0d0)
	//  ELSE
	//    IF (.NOT. FrictionFactorErrorHasOccurred) THEN
	//      RR=RoundSigDigits(RoughnessRatio,7)
	//      Re=RoundSigDigits(ReynoldsNumber,1)
	//      CALL ShowSevereError('Plant Pressure System: Error in moody friction factor calculation')
	//      CALL ShowContinueError('Current Conditions: Roughness Ratio='//TRIM(RR)//'; Reynolds Number='//TRIM(Re))
	//      CALL ShowContinueError('These conditions resulted in an unhandled numeric issue.')
	//      CALL ShowContinueError('Please contact EnergyPlus support/development team to raise an alert about this issue')
	//      CALL ShowContinueError('This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations')
	//      FrictionFactorErrorHasOccurred = .TRUE.
	//    END IF
	//    CalculateMoodyFrictionFactor = 0.04d0
	//  END IF

	//  RETURN

	//END FUNCTION CalculateMoodyFrictionFactor

	//=================================================================================================!

	void
	UpdatePressureDrop( int const LoopNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Evaluate the pressure drop across an entire plant loop and places the value
		// on the PlantLoop(:) data structure for the pump to use

		// METHODOLOGY EMPLOYED:
		// Assumes that the supply inlet is the starting node, which will be set to some standard pressure
		// Then we move around the loop backward from this reference point and go until we hit a pump and stop.
		// The pressure difference from reference to pump is the new required pump head.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopSideNum;
		int BranchNum;
		int NumBranches;
		Real64 BranchPressureDrop;
		Real64 LoopSidePressureDrop;
		Real64 LoopPressureDrop;
		Array1D< Real64 > ParallelBranchPressureDrops;
		Array1D< Real64 > ParallelBranchInletPressures;
		int ParallelBranchCounter;
		Real64 SplitterInletPressure;
		Real64 MixerPressure;
		bool FoundAPumpOnBranch;
		Real64 EffectiveLoopKValue;
		Real64 EffectiveLoopSideKValue;
		Real64 TempVal_SumOfOneByRootK;

		//Exit if not needed
		if ( ! PlantLoop( LoopNum ).HasPressureComponents ) return;

		//Now go through and update the pressure drops as needed
		FoundAPumpOnBranch = false;
		LoopPressureDrop = 0.0;
		for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) { //Start at demand side outlet

			//Loop through all branches on this loop side
			LoopSidePressureDrop = 0.0;
			NumBranches = size( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch );

			//Split here based on a single branch loop or a splitter/mixer configuration
			if ( NumBranches == 1 ) { //Just do the single branch

				//***SINGLE BRANCH***!
				BranchNum = 1;
				DistributePressureOnBranch( LoopNum, LoopSideNum, BranchNum, BranchPressureDrop, FoundAPumpOnBranch );
				LoopSidePressureDrop += BranchPressureDrop;
				LoopPressureDrop += BranchPressureDrop;
				//*******************!

			} else if ( NumBranches > 1 ) { //Loop through all branches on this loop side, mixer/splitter configuration

				//***OUTLET BRANCH***!
				BranchNum = NumBranches;
				DistributePressureOnBranch( LoopNum, LoopSideNum, BranchNum, BranchPressureDrop, FoundAPumpOnBranch );
				LoopSidePressureDrop += BranchPressureDrop;
				LoopPressureDrop += BranchPressureDrop;
				//*******************!

				//***MIXER SIMULATION***!
				MixerPressure = Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumIn ).Press;
				PassPressureAcrossMixer( LoopNum, LoopSideNum, MixerPressure, NumBranches );
				//**********************!

				//***PARALLEL BRANCHES***!
				if ( allocated( ParallelBranchPressureDrops ) ) ParallelBranchPressureDrops.deallocate();
				ParallelBranchPressureDrops.allocate( NumBranches - 2 );
				if ( allocated( ParallelBranchInletPressures ) ) ParallelBranchInletPressures.deallocate();
				ParallelBranchInletPressures.allocate( NumBranches - 2 );
				ParallelBranchCounter = 0;
				for ( BranchNum = NumBranches - 1; BranchNum >= 2; --BranchNum ) { //Working backward (not necessary, but consistent)
					++ParallelBranchCounter;
					DistributePressureOnBranch( LoopNum, LoopSideNum, BranchNum, ParallelBranchPressureDrops( ParallelBranchCounter ), FoundAPumpOnBranch );
					//Store the branch inlet pressure so we can pass it properly across the splitter
					ParallelBranchInletPressures( ParallelBranchCounter ) = Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumIn ).Press;
				}

				//Now take max inlet pressure to pass across splitter and max branch pressure for bookkeeping
				SplitterInletPressure = maxval( ParallelBranchInletPressures );
				BranchPressureDrop = maxval( ParallelBranchPressureDrops );
				LoopSidePressureDrop += BranchPressureDrop;
				LoopPressureDrop += BranchPressureDrop;
				//**********************!

				//If we found pumps on the parallel branches then we are done,
				// If we are on the demand side, we have a common pipe situation and should issue a warning
				if ( FoundAPumpOnBranch ) {
					if ( LoopSideNum == DemandSide ) {
						ShowSevereError( "Pressure system information was found in a demand pump (common pipe) simulation" );
						ShowContinueError( "Currently the pressure simulation is not set up to handle common pipe simulations" );
						ShowContinueError( "Either modify simulation to avoid common pipe, or remove pressure curve information" );
						ShowFatalError( "Pressure configuration mismatch causes program termination" );
					}
					// If we are on the supply side, we simply hit the branch pump, so we exit the IF statement as
					//  we don't need to simulate the splitter or inlet branch
					// For now, not doing anything will leave the IF block
				}

				//If we haven't found a pump on the parallel branches then we need to go ahead
				// and simulate the splitter and inlet branch

				//This may all be superfluous, if we just simulate the splitter and inlet branch we may be fine
				// even if there were branch pumps found.
				if ( ! FoundAPumpOnBranch ) {

					//***SPLITTER SIMULATION***!
					PassPressureAcrossSplitter( LoopNum, LoopSideNum, SplitterInletPressure );
					//*************************!

					//***INLET BRANCH***!
					BranchNum = 1;
					DistributePressureOnBranch( LoopNum, LoopSideNum, BranchNum, BranchPressureDrop, FoundAPumpOnBranch );
					LoopSidePressureDrop += BranchPressureDrop;
					LoopPressureDrop += BranchPressureDrop;
					//******************!

					//***PLANT INTERFACE***!
					if ( LoopSideNum == DemandSide ) {
						PassPressureAcrossInterface( LoopNum );
					}
					//*********************!

				}

			}

			PlantLoop( LoopNum ).LoopSide( LoopSideNum ).PressureDrop = LoopSidePressureDrop;

		} //LoopSides on this loop

		PlantLoop( LoopNum ).PressureDrop = LoopPressureDrop;

		//Now do effective K value calculations
		EffectiveLoopKValue = 0.0;

		for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {

			EffectiveLoopSideKValue = 0.0;

			//Always take the first branch K, it may be the only branch on this half loop
			EffectiveLoopSideKValue += PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( 1 ).PressureEffectiveK;

			//If there is only one branch then move to the other loop side
			if ( size( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch ) == 1 ) continue;

			//Add parallel branches if necessary by adding them as SUM(1/(sqrt(K_i)))
			TempVal_SumOfOneByRootK = 0.0;
			for ( BranchNum = 2; BranchNum <= isize( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch ) - 1; ++BranchNum ) {

				//Only add this branch if the K value is non-zero
				if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureEffectiveK > 0.0 ) {
					TempVal_SumOfOneByRootK += ( 1.0 / std::sqrt( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureEffectiveK ) );
				}

			}

			//Add parallel branches if they are greater than zero, by taking the sum and performing (1/(SUM^2))
			if ( TempVal_SumOfOneByRootK > 0.0 ) EffectiveLoopSideKValue += ( 1.0 / pow_2( TempVal_SumOfOneByRootK ) );

			//Always take the last branch K, it will be in series
			BranchNum = size( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch );
			EffectiveLoopSideKValue += PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureEffectiveK;

			//Assign this loop side's K-value
			PlantLoop( LoopNum ).LoopSide( LoopSideNum ).PressureEffectiveK = EffectiveLoopSideKValue;

			//Keep adding the overall loop K-value
			EffectiveLoopKValue += EffectiveLoopSideKValue;

		}

		//Assign this loop's K-value
		PlantLoop( LoopNum ).PressureEffectiveK = EffectiveLoopKValue;

	}

	//=================================================================================================!

	void
	DistributePressureOnBranch(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		Real64 & BranchPressureDrop,
		bool & PumpFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Apply proper pressure to nodes along branch

		// METHODOLOGY EMPLOYED:
		// Move backward through components, passing pressure upstream
		// Account for branch pressure drop at branch inlet node
		// Update PlantLoop(:)%LoopSide(:)%Branch(:)%PressureDrop Variable

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::GenEquipTypes_Pump;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		int NumCompsOnBranch;
		Real64 TempBranchPressureDrop;

		//Initialize
		TempBranchPressureDrop = 0.0;
		BranchPressureDrop = 0.0;
		PumpFound = false;
		NumCompsOnBranch = size( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp );

		//Retrieve temporary branch pressure drop
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).HasPressureComponents ) {
			TempBranchPressureDrop = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).PressureDrop;
		}

		//If the last component on the branch is the pump, then check if a pressure drop is detected and set the flag and leave
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( NumCompsOnBranch ).GeneralEquipType == GenEquipTypes_Pump ) {
			PumpFound = true;
			if ( TempBranchPressureDrop != 0.0 ) {
				ShowSevereError( "Error in plant pressure simulation for plant loop: " + PlantLoop( LoopNum ).Name );
				if ( LoopNum == DemandSide ) {
					ShowContinueError( "Occurs for demand side, branch: " + PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Name );
				} else if ( LoopNum == SupplySide ) {
					ShowContinueError( "Occurs for supply side, branch: " + PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Name );
				}
				ShowContinueError( "Branch contains only a single pump component, yet also a pressure drop component." );
				ShowContinueError( "Either add a second component to this branch after the pump, or move pressure drop data." );
				ShowFatalError( "Preceding pressure drop error causes program termination" );
			}
			return;
		}

		//Assign official branch pressure drop
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).HasPressureComponents ) {
			BranchPressureDrop = TempBranchPressureDrop;
		}

		//Otherwise update the inlet node of the last component on the branch with this corrected pressure
		//This essentially sets all the pressure drop on the branch to be accounted for on the last component
		Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( NumCompsOnBranch ).NodeNumIn ).Press = Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( NumCompsOnBranch ).NodeNumOut ).Press + BranchPressureDrop;

		//Then Smear any internal nodes with this new node pressure by working backward through
		// all but the last component, and passing node pressure upstream
		if ( NumCompsOnBranch > 1 ) {
			for ( CompNum = NumCompsOnBranch - 1; CompNum >= 1; --CompNum ) {

				//If this component is a pump, stop passing pressure upstream, and set flag to true for calling routine
				if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).GeneralEquipType == GenEquipTypes_Pump ) {
					PumpFound = true;
					break;
				}

				//Otherwise just pass pressure upstream and move on
				Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn ).Press = Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut ).Press;

			}
		}

	}

	//=================================================================================================!

	void
	PassPressureAcrossMixer(
		int const LoopNum,
		int const LoopSideNum,
		Real64 & MixerPressure,
		int const NumBranchesOnLoopSide
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set mixer inlet pressures, or in other words, set mixer inlet branch outlet pressures

		// METHODOLOGY EMPLOYED:
		// Set outlet node pressures for all parallel branches on this LoopSide
		// Note that this is extremely simple, but is set to it's own routine to allow for clarity
		//  when possible expansion occurs during further development

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BranchNum;

		for ( BranchNum = 2; BranchNum <= NumBranchesOnLoopSide - 1; ++BranchNum ) {
			Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumOut ).Press = MixerPressure;
		}

	}

	//=================================================================================================!

	void
	PassPressureAcrossSplitter(
		int const LoopNum,
		int const LoopSideNum,
		Real64 & SplitterInletPressure
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the splitter inlet pressure in anticipation of the inlet branch pressure being simulated

		// METHODOLOGY EMPLOYED:
		// Set outlet node of LoopSide inlet branch to splitter pressure
		// Note that this is extremely simple, but is set to it's own routine to allow for clarity
		//  when possible expansion occurs during further development

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const InletBranchNum( 1 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( InletBranchNum ).NodeNumOut ).Press = SplitterInletPressure;

	}

	//=================================================================================================!

	void
	PassPressureAcrossInterface( int const LoopNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Pass pressure backward across plant demand inlet/supply outlet interface

		// METHODOLOGY EMPLOYED:
		// Set outlet node pressure of supply side equal to inlet node pressure of demand side
		// Note that this is extremely simple, but is set to it's own routine to allow for clarity
		//  when possible expansion occurs during further development

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DemandInletNodeNum;
		int SupplyOutletNodeNum;

		DemandInletNodeNum = PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn;
		SupplyOutletNodeNum = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut;

		Node( SupplyOutletNodeNum ).Press = Node( DemandInletNodeNum ).Press;

	}

	//=================================================================================================!

	//SUBROUTINE GetPressureCurveTypeAndIndex(PressureCurveName, PressureCurveType, PressureCurveIndex)

	//          ! SUBROUTINE INFORMATION:
	//          !       AUTHOR         Edwin Lee
	//          !       DATE WRITTEN   August 2009
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS SUBROUTINE:
	//          ! Given a curve name, returns the curve type and index

	//          ! METHODOLOGY EMPLOYED:
	//          ! Curve types are:
	//          !  PressureCurve_Error       = pressure name was given, but curve is not available
	//          !  PressureCurve_None        = no pressure curve for this branch
	//          !  PressureCurve_Pressure    = pressure curve based on friction/minor loss
	//          !  PressureCurve_Generic     = curvemanager held curve which is function of flow rate

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//  USE InputProcessor, ONLY : FindItemInList
	//  USE CurveManager,   ONLY : GetCurveIndex, GetCurveType
	//  USE GlobalDataConstants,  ONLY : PressureCurve_None, PressureCurve_Pressure, PressureCurve_Generic, PressureCurve_Error

	//  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

	//          ! SUBROUTINE ARGUMENT DEFINITIONS:
	//  CHARACTER(len=*), INTENT (IN)  :: PressureCurveName            ! name of the curve
	//  INTEGER, INTENT(INOUT)         :: PressureCurveType
	//  INTEGER, INTENT(INOUT)         :: PressureCurveIndex

	//          ! SUBROUTINE PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS
	//          ! na

	//          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	//  INTEGER           :: TempCurveIndex
	//  LOGICAL           :: FoundCurve
	//  CHARACTER(len=32) :: GenericCurveType

	//  !If input is not gotten, go ahead and get it now
	//  IF (GetInputFlag) CALL GetPressureSystemInput

	//  !Initialize
	//  FoundCurve = .FALSE.
	//  PressureCurveType = PressureCurve_None
	//  PressureCurveIndex = 0

	//  !Try to retrieve a curve manager object
	//  TempCurveIndex = GetCurveIndex(PressureCurveName)

	//  !See if it is valid
	//  IF (TempCurveIndex > 0) THEN
	//    !We have to check the type of curve to make sure it is single independent variable type
	//    GenericCurveType = GetCurveType(TempCurveIndex)
	//    SELECT CASE (GenericCurveType)
	//      CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'QUARTIC', 'EXPONENT')
	//        PressureCurveType = PressureCurve_Generic
	//        PressureCurveIndex = TempCurveIndex
	//      CASE DEFAULT
	//        CALL ShowSevereError('Plant Pressure Simulation: Found error for curve: '//PressureCurveName)
	//        CALL ShowContinueError('Curve type detected: '//GenericCurveType)
	//        CALL ShowContinueError('Generic curves should be single independent variable such that DeltaP = f(mdot)')
	//        CALL ShowContinueError(' Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent')
	//        CALL ShowFatalError('Errors in pressure simulation input cause program termination')
	//    END SELECT
	//    RETURN
	//  END IF

	//  !Then try to retrieve a pressure curve object
	//  IF (ALLOCATED(PressureCurve)) THEN
	//    IF (SIZE(PressureCurve) > 0) THEN
	//      TempCurveIndex = FindItemInList(PressureCurveName,PressureCurve(1:SIZE(PressureCurve))%Name,SIZE(PressureCurve))
	//    ELSE
	//      TempCurveIndex = 0
	//    END IF
	//  END IF

	//  !See if it is valid
	//  IF (TempCurveIndex > 0) THEN
	//    PressureCurveType = PressureCurve_Pressure
	//    PressureCurveIndex = TempCurveIndex
	//    RETURN
	//  END IF

	//  !If we made it here, we didn't find either type of match

	//  !Last check, see if it is blank:
	//  IF (TRIM(PressureCurveName)=='') THEN
	//    PressureCurveType = PressureCurve_None
	//    RETURN
	//  END IF

	//  !At this point, we had a non-blank user entry with no match
	//  PressureCurveType = PressureCurve_Error
	//  RETURN

	//RETURN

	//END SUBROUTINE

	//=================================================================================================!

	Real64
	ResolveLoopFlowVsPressure(
		int const LoopNum, // - Index of which plant/condenser loop is being simulated
		Real64 const SystemMassFlow, // - Initial "guess" at system mass flow rate [kg/s]
		int const PumpCurveNum, // - Pump curve to use when calling the curve manager for psi = f(phi)
		Real64 const PumpSpeed, // - Pump rotational speed, [rps] (revs per second)
		Real64 const PumpImpellerDia, // - Nominal pump impeller diameter [m]
		Real64 const MinPhi, // - Minimum allowable value of phi, requested by the pump manager from curve mgr
		Real64 const MaxPhi // - Maximum allowable value of phi, requested by the pump manager from curve mgr
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Kaustubh Phalak
		//       DATE WRITTEN   Feb 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// To provide a means to simulate a constant speed pump curve and system curve to
		//  find a more realistic operating point for the plant.

		// METHODOLOGY EMPLOYED:
		// Pressure drop of complete loop is found for a perticular flow rate.
		//  i.e. pressuredrop = K * massflow ^ 2
		// System curve is then solved with pump curve already entered
		//  and flow rate provided by the pump will be calculated.
		// This routine does not trap for errors if a pressure simulation is not to be performed.
		// Calling routine should only call this if needed.

		// REFERENCES:
		//  -

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataPlant::PlantLoop;
		using DataPlant::SupplySide;
		using DataLoopNode::Node;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using CurveManager::CurveValue;

		// Return value
		Real64 ResolvedLoopMassFlowRate;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ResolvedLoopMassFlowRate: " );
		int const MaxIters( 100 );
		static std::string const DummyFluidName;
		Real64 const PressureConvergeCriteria( 0.1 ); // Pa
		Real64 const ZeroTolerance( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 PumpPressureRise;
		Real64 NodeTemperature;
		Real64 NodeDensity;
		Real64 SystemPressureDrop;
		Real64 PhiPump;
		Real64 PhiSystem;
		Real64 PsiPump;
		int FluidIndex;
		int Iteration;
		Real64 LocalSystemMassFlow;
		Real64 LoopEffectiveK;
		bool Converged;
		static int ZeroKWarningCounter( 0 );
		static int MaxIterWarningCounter( 0 );
		Array1D< Real64 > MassFlowIterativeHistory( 3 );
		Real64 MdotDeltaLatest;
		Real64 MdotDeltaPrevious;
		Real64 DampingFactor;

		//Get loop level data
		FluidIndex = PlantLoop( LoopNum ).FluidIndex;
		LoopEffectiveK = PlantLoop( LoopNum ).PressureEffectiveK;
		SystemPressureDrop = LoopEffectiveK * pow_2( SystemMassFlow );

		//Read data off the node data structure
		NodeTemperature = Node( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn ).Temp;
		NodeDensity = GetDensityGlycol( DummyFluidName, NodeTemperature, FluidIndex, RoutineName );

		//Store the passed in (requested, design) flow to the local value for performing iterations
		LocalSystemMassFlow = SystemMassFlow;

		//Check and warn if invalid condition exists
		if ( LoopEffectiveK <= ZeroTolerance ) {
			++ZeroKWarningCounter;
			if ( ZeroKWarningCounter == 1 ) {
				ShowWarningError( "Pump pressure-flow resolution attempted, but invalid loop conditions encountered." );
				ShowContinueError( "Loop being calculated: " + PlantLoop( LoopNum ).Name );
				ShowContinueError( "An invalid pressure/flow condition existed which resulted in the approximation of" );
				ShowContinueError( "the pressure coefficient K to be zero.  The pressure simulation will use the requested (design)" );
				ShowContinueError( "pump flow in order to proceed with the simulation.  This warning is only issued once." );
			}
			ResolvedLoopMassFlowRate = SystemMassFlow;
			return ResolvedLoopMassFlowRate;
		}

		//Initialize flag
		Converged = false;

		//Initialize the mass flow history array and damping factor
		MassFlowIterativeHistory = LocalSystemMassFlow;
		DampingFactor = 0.9;

		//Start Convergence Loop
		for ( Iteration = 1; Iteration <= MaxIters; ++Iteration ) {

			//Calculate System Mass Flow Rate
			LocalSystemMassFlow = std::sqrt( SystemPressureDrop / LoopEffectiveK );

			MassFlowIterativeHistory = eoshift( MassFlowIterativeHistory, -1, LocalSystemMassFlow );

			PhiSystem = LocalSystemMassFlow / ( NodeDensity * PumpSpeed * PumpImpellerDia );

			//4th order polynomial for non-dimensional pump curve
			PhiPump = PhiSystem;

			//Constrain the value to the valid region
			PhiPump = max( PhiPump, MinPhi );
			PhiPump = min( PhiPump, MaxPhi );

			//Get the pump curve value from the curve manager
			PsiPump = CurveValue( PumpCurveNum, PhiPump );

			//Calcuate Pump Pressure rise
			PumpPressureRise = PsiPump * NodeDensity * pow_2( PumpSpeed ) * pow_2( PumpImpellerDia );

			//Convergence Criteria Based on Pressure
			if ( std::abs( SystemPressureDrop - PumpPressureRise ) < ( PressureConvergeCriteria ) ) {
				ResolvedLoopMassFlowRate = LocalSystemMassFlow;
				Converged = true;
				break;
			}

			if ( Iteration < 2 ) {
				//Don't do anything?
			} else {
				MdotDeltaLatest = std::abs( MassFlowIterativeHistory( 1 ) - MassFlowIterativeHistory( 2 ) );
				MdotDeltaPrevious = std::abs( MassFlowIterativeHistory( 2 ) - MassFlowIterativeHistory( 3 ) );
				if ( MdotDeltaLatest < MdotDeltaPrevious ) {
					//we are converging
					//DampingFactor = MIN(DampingFactor * 1.1, 0.9d0)
				} else {
					//we are stuck or diverging
					DampingFactor *= 0.9;
				}
			}

			//Update pressure value with damping factor
			SystemPressureDrop = DampingFactor * PumpPressureRise + ( 1.0 - DampingFactor ) * SystemPressureDrop;

		}

		//Check if we didn't converge
		if ( ! Converged ) {
			++MaxIterWarningCounter;
			if ( MaxIterWarningCounter == 1 ) {
				ShowWarningError( "Pump pressure-flow resolution attempted, but iteration loop did not converge." );
				ShowContinueError( "Loop being calculated: " + PlantLoop( LoopNum ).Name );
				ShowContinueError( "A mismatch between the pump curve entered and the pressure drop components" );
				ShowContinueError( "on the loop may be the cause.  The pressure simulation will use the requested (design)" );
				ShowContinueError( "pump flow in order to proceed with the simulation.  This warning is only issued once." );
			}
			ResolvedLoopMassFlowRate = SystemMassFlow;
			return ResolvedLoopMassFlowRate;
		}

		return ResolvedLoopMassFlowRate;

	}

	//=================================================================================================!

} // PlantPressureSystem

} // EnergyPlus
