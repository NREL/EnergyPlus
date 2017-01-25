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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <PondGroundHeatExchanger.hh>
#include <BranchNodeConnections.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <NodeInputManager.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PondGroundHeatExchanger {

	// Module containing the routines dealing with pond ground heat exchangers

	// MODULE INFORMATION:
	//       AUTHOR         Simon Rees
	//       DATE WRITTEN   September 2002
	//       MODIFIED       Brent Griffith Sept 2010, plant upgrades
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This model represents a shallow pond with submerged hydronic tubes through
	// which the heat transfer fluid is circulated. The model represents a 'shallow'
	// pond in that no attempt is made to model any stratification effects that may
	// be present in deeper ponds. This type of heat rejector is intended to be
	// connected in a condenser loop, with or without other forms of heat rejector.
	// The pond model is a 'lumped parameter' model where the pond is represented
	// by a single node with thermal mass. The pond surface temperature is the same
	// as the temperature at this node, i.e. the surface temperature is the same as
	// the bulk temperature. A first order differential equation is solved in the
	// model to calculated the pond temperature at each time step. This type of heat
	// rejector is modelled as several circuits connected in parallel.

	// METHODOLOGY EMPLOYED:
	// A heat balance is calculated at a single node that represents the pond.
	// heat transfer takes palce by surface convection, long-wave radiation to the
	// sky, absoption of solar energy, ground heat transfer and heat exchange with
	// the fluid. A heat exchanger analogy is used to calculate the heat transfer
	// between the heat transfer fluid and the pond. The differential equation
	// defined by the heat balance is solved using a fourth order Runge-Kutta
	// numerical integration method.

	// REFERENCES:
	// Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
	//   M.S. Thesis, Oklahoma State University, December 1999.
	// Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
	//   Simulating The Performance Of A Shallow Pond As A Supplemental Heat Rejecter
	//   With Closed-Loop Ground-Source Heat Pump Systems.
	//   ASHRAE Transactions.  106(2):107-121.

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::KelvinConv;
	using General::TrimSigDigits;
	using DataPlant::PlantLoop;
	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;
	static std::string const fluidNameWater( "WATER" );
	Real64 const SmallNum( 1.0e-30 ); // Very small number to avoid div0 errors
	Real64 const StefBoltzmann( 5.6697e-08 ); // Stefan-Boltzmann constant
	//  REAL(r64), PARAMETER :: KelvinConv    = KelvinConv           ! Conversion from Celsius to Kelvin

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// utility variables initialized once
	int NumOfPondGHEs( 0 ); // Number of pond ground heat exchangers
	// Utility variables - initialized for each instance of a pond
	//Real64 nsvOutletTemp( 0.0 ); // water outlet temperature
	//Real64 PondTemp( 0.0 ); // pond temperature
	//Real64 PastPondTemp( 0.0 ); // past pond temperature
	//Real64 TubeInDiameter( 0.0 ); // hydronic tube inside diameter
	//Real64 TubeOutDiameter( 0.0 ); // hydronic tube outside diameter
	//Real64 TubeConductivity( 0.0 ); // hydronic tube thermal conductivity
	//Real64 GrndConductivity( 0.0 ); // ground thermal conductivity
	//Real64 Concentration( 0.0 ); // fluid/glycol concentration 0.0-1.0 proportion.
	//int NumCircuits( 0 ); // number of circuits in total
	//int InletNodeNum( 0 ); // inlet node number
	//int OutletNodeNum( 0 ); // oulet node number
	// temperature object was input.
	bool GetInputFlag( true );

	// SUBROUTINE SPECIFICATIONS FOR MODULE PlantPondGroundHeatExchangers

	// Object Data
	Array1D< PondGroundHeatExchangerData > PondGHE;


	void PondGroundHeatExchangerData::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ), bool const FirstHVACIteration, Real64 & EP_UNUSED( CurLoad ), bool const EP_UNUSED( RunFlag )  ) {
		this->InitPondGroundHeatExchanger( FirstHVACIteration );
		this->CalcPondGroundHeatExchanger();
		this->UpdatePondGroundHeatExchanger();
		this->ReportPondGroundHeatExchanger();

	}

	PlantComponent * PondGroundHeatExchangerData::factory( int const EP_UNUSED( objectType ), std::string objectName ) {
		if ( GetInputFlag ) {
			GetPondGroundHeatExchanger();
			GetInputFlag = false;
		}
		for ( auto & ghx : PondGHE ) {
			if ( ghx.Name == objectName ) {
				return &ghx;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "Pond Heat Exchanger Factory: Error getting inputs for GHX named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}

	void PondGroundHeatExchangerData::getDesignCapacities( const PlantLocation & EP_UNUSED(calledFromLocation), Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad ) {
		this->InitPondGroundHeatExchanger( true );
		MaxLoad = this->DesignCapacity;
		MinLoad = 0.0;
		OptLoad = this->DesignCapacity;

	}


	void
	GetPondGroundHeatExchanger()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for hydronic Pond Ground Heat Exchangers
		// from the user input file.  This will contain all of the information
		// needed to define and simulate the pond.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using DataEnvironment::GroundTemp_Deep;
		using DataEnvironment::GroundTemp_DeepObjInput;
		using General::RoundSigDigits;
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input,
		// fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call

		// Initializations and allocations
		cCurrentModuleObject = "GroundHeatExchanger:Pond";
		NumOfPondGHEs = GetNumObjectsFound( cCurrentModuleObject );
		// allocate data structures
		if ( allocated( PondGHE ) ) PondGHE.deallocate();

		PondGHE.allocate( NumOfPondGHEs );

		// Obtain all of the user data related to the ponds...
		for ( Item = 1; Item <= NumOfPondGHEs; ++Item ) {

			// get the input data
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );

			PondGHE( Item ).WaterIndex = FindGlycol( fluidNameWater );

			// General user input data
			PondGHE( Item ).Name = cAlphaArgs( 1 );

			//get inlet node data
			PondGHE( Item ).InletNode = cAlphaArgs( 2 );
			PondGHE( Item ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PondGHE( Item ).InletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// get outlet node data
			PondGHE( Item ).OutletNode = cAlphaArgs( 3 );
			PondGHE( Item ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PondGHE( Item ).OutletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Condenser Water Nodes" );

			// pond geometry data
			PondGHE( Item ).Depth = rNumericArgs( 1 );
			PondGHE( Item ).Area = rNumericArgs( 2 );
			if ( rNumericArgs( 1 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 2 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

			// tube data
			PondGHE( Item ).TubeInDiameter = rNumericArgs( 3 );
			PondGHE( Item ).TubeOutDiameter = rNumericArgs( 4 );

			if ( rNumericArgs( 3 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 3 ) + '=' + RoundSigDigits( rNumericArgs( 3 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 4 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( rNumericArgs( 4 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 3 ) > rNumericArgs( 4 ) ) { // error
				ShowSevereError( "For " + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
				ShowContinueError( cNumericFieldNames( 3 ) + " [" + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "] > " + cNumericFieldNames( 4 ) + " [" + RoundSigDigits( rNumericArgs( 4 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			// thermal conductivity data
			PondGHE( Item ).TubeConductivity = rNumericArgs( 5 );
			PondGHE( Item ).GrndConductivity = rNumericArgs( 6 );

			if ( rNumericArgs( 5 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + '=' + RoundSigDigits( rNumericArgs( 5 ), 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 6 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 6 ) + '=' + RoundSigDigits( rNumericArgs( 6 ), 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

			// circuits
			PondGHE( Item ).NumCircuits = rNumericArgs( 7 );

			if ( rNumericArgs( 7 ) <= 0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 7 ) + '=' + RoundSigDigits( rNumericArgs( 7 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			PondGHE( Item ).CircuitLength = rNumericArgs( 8 );
			if ( rNumericArgs( 8 ) <= 0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 8 ) + '=' + RoundSigDigits( rNumericArgs( 8 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

		} // end of input loop

		// final error check
		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		// Set up the output variables
		for ( Item = 1; Item <= NumOfPondGHEs; ++Item ) {
			SetupOutputVariable( "Pond Heat Exchanger Heat Transfer Rate [W]", PondGHE( Item ).HeatTransferRate, "Plant", "Average", PondGHE( Item ).Name );
			SetupOutputVariable( "Pond Heat Exchanger Heat Transfer Energy [J]", PondGHE( Item ).Energy, "Plant", "Sum", PondGHE( Item ).Name );
			SetupOutputVariable( "Pond Heat Exchanger Mass Flow Rate [kg/s]", PondGHE( Item ).MassFlowRate, "Plant", "Average", PondGHE( Item ).Name );
			SetupOutputVariable( "Pond Heat Exchanger Inlet Temperature [C]", PondGHE( Item ).InletTemp, "Plant", "Average", PondGHE( Item ).Name );
			SetupOutputVariable( "Pond Heat Exchanger Outlet Temperature [C]", PondGHE( Item ).OutletTemp, "Plant", "Average", PondGHE( Item ).Name );
			SetupOutputVariable( "Pond Heat Exchanger Bulk Temperature [C]", PondGHE( Item ).PondTemp, "Plant", "Average", PondGHE( Item ).Name );
		}

		if ( ! GroundTemp_DeepObjInput ) {
			ShowWarningError( "GetPondGroundHeatExchanger:  No \"Site:GroundTemperature:Deep\" were input." );
			ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp_Deep, 1 ) + ") will be used." );
		}

	}

	void
	PondGroundHeatExchangerData::InitPondGroundHeatExchanger(
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine Resets the elements of the data structure as necessary
		// at the first HVAC iteration of each time step.

		// METHODOLOGY EMPLOYED:
		// One of the things done here is to update the record of the past pond
		// temperature. This is needed in order to solve the diff. eqn. to find
		// the temperature at the end of the next time step.
		// Also set module variables to data structure for this pond. Set flow rate
		// from node data and hypothetical design flow.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::BeginTimeStepFlag;
		using DataGlobals::Pi;
		using DataGlobals::WarmupFlag;
		using DataLoopNode::Node;
		using DataEnvironment::GroundTemp_Deep;
		using DataEnvironment::OutDryBulbTempAt;
		using DataPlant::TypeOf_GrndHtExchgPond;
		using DataPlant::ScanPlantLoopsForObject;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//  INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DesignVelocity( 0.5 ); // Hypothetical design max pipe velocity [m/s]
		Real64 const PondHeight( 0.0 ); // for now
		static std::string const RoutineName( "InitPondGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 DesignFlow; // Hypothetical design flow rate
		//int PondNum; // loop counter
		int LoopNum;
		int LoopSideNum;
		Real64 rho;
		Real64 Cp;
		bool errFlag;
		//repeated warm up days tend to drive the initial pond temperature toward the drybulb temperature
		//For each environment start the pond midway between drybulb and ground temp.

		if ( this->OneTimeFlag || WarmupFlag ) {
			// initialize pond temps to mean of drybulb and ground temps.
			this->BulkTemperature = this->PastBulkTemperature = 0.5 * ( OutDryBulbTempAt( PondHeight ) + GroundTemp_Deep );
			this->OneTimeFlag = false;
		}


		// Init more variables
		if ( this->MyFlag ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_GrndHtExchgPond, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitPondGroundHeatExchanger: Program terminated due to previous condition(s)." );
			}
			rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, constant_zero, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
			Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, constant_zero, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
			this->DesignMassFlowRate = Pi / 4.0 * pow_2( this->TubeInDiameter ) * DesignVelocity * rho * this->NumCircuits;
			this->DesignCapacity = this->DesignMassFlowRate * Cp * 10.0; //assume 10C delta T?
			InitComponentNodes( 0.0, this->DesignMassFlowRate, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );
			RegisterPlantCompDesignFlow( this->InletNodeNum, this->DesignMassFlowRate / rho );

			this->MyFlag = false;
		}

		// check if we are in very first call for this zone time step
		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		if ( BeginTimeStepFlag && FirstHVACIteration && PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) { //DSU
			// update past temperature
			this->PastBulkTemperature = this->BulkTemperature;
		}

		// initialize - module variables
		//InletNodeNum = this->InletNodeNum;
		//OutletNodeNum = this->OutletNodeNum;
		this->InletTemp = Node( InletNodeNum ).Temp;
		OutletTemp = Node( OutletNodeNum ).Temp;
		//TubeInDiameter = this->TubeInDiameter;
		//TubeOutDiameter = this->TubeOutDiameter;
		//TubeConductivity = this->TubeConductivity;
		//GrndConductivity = this->GrndConductivity;
		//NumCircuits = this->NumCircuits;
		// temperatures
		PondTemp = this->BulkTemperature;

		DesignFlow = RegulateCondenserCompFlowReqOp( this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, this->DesignMassFlowRate );

		SetComponentFlowRate( DesignFlow, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );

		// get the current flow rate - module variable
		this->MassFlowRate = Node( InletNodeNum ).MassFlowRate;

	}

	//==============================================================================

	void
	PondGroundHeatExchangerData::CalcPondGroundHeatExchanger()
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a pond ground heat exchanger.  Calls are made to appropriate subroutines
		// either in this module or outside of it.

		// METHODOLOGY EMPLOYED:
		// The differential equation defined by the heat balance is solved using
		// a fourth order Runge-Kutta numerical integration method. The differential
		// equation is:
		//            Mdot*Cp*dT/dt = Sum of fluxes.

		// REFERENCES:
		// Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
		//   M.S. Thesis, Oklahoma State University, December 1999.
		// Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
		//   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
		//   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
		//   ASHRAE Transactions.  106(2):107-121.

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataGlobals::SecInHour;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcPondGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 PondTempStar;
		Real64 PondTempStarStar;
		Real64 PondTempStarStarStar;
		Real64 Flux;
		Real64 FluxStar;
		Real64 FluxStarStar;
		Real64 NewPondTemp;
		Real64 SpecificHeat;
		Real64 PondMass;

		PondMass = this->Depth * this->Area * GetDensityGlycol( fluidNameWater, max( PondTemp, constant_zero ), this->WaterIndex, RoutineName );

		SpecificHeat = GetSpecificHeatGlycol( fluidNameWater, max( PondTemp, constant_zero ), this->WaterIndex, RoutineName ); //DSU bug fix here, was using working fluid index

		Flux = this->CalcTotalFLux( PondTemp );
		PondTempStar = this->PastBulkTemperature + 0.5 * SecInHour * TimeStepSys * Flux / ( SpecificHeat * PondMass );

		FluxStar = this->CalcTotalFLux( PondTempStar );
		PondTempStarStar = this->PastBulkTemperature + 0.5 * SecInHour * TimeStepSys * FluxStar / ( SpecificHeat * PondMass );

		FluxStarStar = this->CalcTotalFLux( PondTempStarStar );
		PondTempStarStarStar = this->PastBulkTemperature + SecInHour * TimeStepSys * FluxStarStar / ( SpecificHeat * PondMass );

		NewPondTemp = this->PastBulkTemperature + SecInHour * TimeStepSys * ( Flux + 2.0 * FluxStar + 2.0 * FluxStarStar + this->CalcTotalFLux( PondTempStarStarStar ) ) / ( 6.0 * SpecificHeat * PondMass );

		PondTemp = NewPondTemp;

	}

	//==============================================================================

	Real64
	PondGroundHeatExchangerData::CalcTotalFLux(
		Real64 const PondBulkTemp // pond temp for this flux calculation
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Thic calculates the summation of the heat fluxes on the pond for a
		// given pond temperature. The following heat fluxes are calculated:
		//   convection,
		//   long-wave radiation,
		//   solar gain,
		//   evaporation,
		//   ground conduction,
		//   along with heat exchange with the fluid

		// METHODOLOGY EMPLOYED:
		// Convection is calculated with the ASHRAE simple convection coefficients.
		// Evaporation is calculated assuming a fixed Lewis number - not as in
		// Chaisson model. Heat transfer with the fluid is calculated using a heat
		// exchanger Effectiveness-NTU method, where the pond is seen as a static
		// fluid - this is also different from Chaisson's original model (assumed
		// pond at average of inlet and outlet temps).

		// REFERENCES:
		// Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
		//   M.S. Thesis, Oklahoma State University, December 1999.
		// Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
		//   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
		//   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
		//   ASHRAE Transactions.  106(2):107-121.
		// Hull, J.R., K.V. Liu, W.T. Sha, J. Kamal, and C.E. Nielsen, 1984.
		//   Dependence of Ground Heat Losses Upon Solar Pond Size and Perimeter
		//   Insulation Calculated and Experimental Results. Solar Energy,33(1):25-33.

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTempAt;
		using DataEnvironment::OutWetBulbTempAt;
		using DataEnvironment::WindSpeedAt;
		using DataEnvironment::IsSnow;
		using DataEnvironment::IsRain;
		using DataEnvironment::SkyTemp;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::GroundTemp_Deep;
		using FluidProperties::GetSpecificHeatGlycol;
		using ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff;
		using namespace DataGlobals;
		using DataHeatBalance::VeryRough;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyHfgAirFnWTdb;

		// Return value
		Real64 CalcTotalFLux; // function return variable

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const PrantlAir( 0.71 ); // Prantl number for air - assumed constant
		Real64 const SchmidtAir( 0.6 ); // Schmidt number for air - assumed constant
		Real64 const PondHeight( 0.0 ); // for now
		static std::string const RoutineName( "PondGroundHeatExchanger:CalcTotalFlux" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 ConvCoef; // convection coefficient
		Real64 ExternalTemp; // external environmental temp - drybulb or wetbulb
		Real64 FluxSolAbsorbed; // absorbed solar flux
		Real64 FluxLongwave; // absorbed longwave flux
		Real64 FluxConvect; // convective flux
		Real64 FluxEvap; // evaporative heat flux
		Real64 FluxGround; // ground heat transfer flux
		Real64 Qfluid; // convective flux
		Real64 SurfTempAbs; // absolute value of surface temp
		Real64 SkyTempAbs; // absolute value of sky temp
		Real64 ThermalAbs; // thermal absorptivity
		Real64 SpecHeat; // specific heat capacity
		Real64 HumRatioFilm; // humidity ratio at pond surface/film temperature
		Real64 HumRatioAir; // humidity ratio of air
		Real64 SpecHeatAir; // air specific heat
		Real64 LatentHeatAir; // latent heat of air
		Real64 UvalueGround; // ground heat transfer coefficient
		Real64 Perimeter; // pond perimeter
		Real64 OutDryBulb; // drybulb at pond height
		Real64 OutWetBulb; // wetbulb at pond height

		// make a surface heat balance and solve for temperature
		ThermalAbs = 0.9;

		// set appropriate external temp
		// use height dependency --  if there was a height for this unit, it could be inserted.
		// parameter PondHeight=0.0 is used.
		OutDryBulb = OutDryBulbTempAt( PondHeight );
		OutWetBulb = OutWetBulbTempAt( PondHeight );
		if ( IsSnow ) {
			ExternalTemp = OutWetBulb;
		} else if ( IsRain ) {
			ExternalTemp = OutWetBulb;
		} else { // normal dry conditions
			ExternalTemp = OutDryBulb;
		}

		// absolute temperatures
		SurfTempAbs = PondBulkTemp + KelvinConv;
		SkyTempAbs = SkyTemp + KelvinConv;

		// ASHRAE simple convection coefficient model for external surfaces.
		ConvCoef = CalcASHRAESimpExtConvectCoeff( VeryRough, WindSpeedAt( PondHeight ) );
		// convective flux
		FluxConvect = ConvCoef * ( PondBulkTemp - ExternalTemp );

		// long-wave radiation between pond and sky.
		FluxLongwave = StefBoltzmann * ThermalAbs * ( pow_4( SurfTempAbs ) - pow_4( SkyTempAbs ) );

		// total absorbed solar using function - no ground solar
		FluxSolAbsorbed = CalcSolarFlux();

		// specific heat from fluid prop routines
		SpecHeat = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, max( this->InletTemp, 0.0 ), PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
		// heat transfer with fluid - heat exchanger analogy.
		Qfluid = this->MassFlowRate * SpecHeat * this->CalcEffectiveness( this->InletTemp, PondBulkTemp, this->MassFlowRate ) * ( this->InletTemp - PondBulkTemp );

		this->HeatTransferRate = Qfluid;

		// evaporation flux
		// get air properties
		HumRatioAir = PsyWFnTdbTwbPb( OutDryBulb, OutWetBulb, OutBaroPress );
		HumRatioFilm = PsyWFnTdbTwbPb( PondBulkTemp, PondBulkTemp, OutBaroPress );
		SpecHeatAir = PsyCpAirFnWTdb( HumRatioAir, OutDryBulb );
		LatentHeatAir = PsyHfgAirFnWTdb( HumRatioAir, OutDryBulb );

		FluxEvap = pow_2( PrantlAir / SchmidtAir ) / 3.0 * ConvCoef / SpecHeatAir * ( HumRatioFilm - HumRatioAir ) * LatentHeatAir;

		// ground heat transfer flux
		Perimeter = 4.0 * std::sqrt( this->Area ); // square assumption
		UvalueGround = 0.999 * ( GrndConductivity / this->Depth ) + 1.37 * ( GrndConductivity * Perimeter / this->Area );
		FluxGround = UvalueGround * ( PondBulkTemp - GroundTemp_Deep );

		CalcTotalFLux = Qfluid + this->Area * ( FluxSolAbsorbed - FluxConvect - FluxLongwave - FluxEvap - FluxGround );
		if ( BeginTimeStepFlag ) {

		}

		return CalcTotalFLux;
	}

	//==============================================================================

	Real64
	PondGroundHeatExchangerData::CalcSolarFlux()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is used to calculate the net solar flux absorbed by the pond.

		// METHODOLOGY EMPLOYED:
		// This is calculated from basic optical formula using the extinction
		// coefficient of the pond as the main parameter. This can be in a
		// wide range: 0.13 - 7.5 in the literature depending on algae, suspended
		// solids etc. ??

		// REFERENCES:
		// Duffie, J.A. and W.A. Beckman, 1991. Solar Engineering of Thermal
		//  Processes, 2 nd Edition. John Wiley and Sons.
		// Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
		//   M.S. Thesis, Oklahoma State University, December 1999.
		// Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
		//   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
		//   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
		//   ASHRAE Transactions.  106(2):107-121.

		// Using/Aliasing
		using DataEnvironment::BeamSolarRad;
		using DataEnvironment::DifSolarRad;
		using DataEnvironment::SunIsUp;
		using DataEnvironment::SOLCOS;

		// Return value
		Real64 CalcSolarFlux; // Function return variable

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const WaterRefIndex( 1.33 ); // refractive index of water
		Real64 const AirRefIndex( 1.0003 ); // refractive index of air
		Real64 const PondExtCoef( 0.3 ); // extinction coefficent of water

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 IncidAngle; // angle of incidence of beam
		Real64 RefractAngle; // angle of refraction of beam
		Real64 Transmitance; // transmitted solar
		Real64 Reflectance; // reflectance
		Real64 Absorbtance; // absorbed solar

		Real64 ParallelRad; // parallel component of irradiation
		Real64 PerpendRad; // parallel component of irradiation

		// FLOW:

		// check for sun up.
		if ( ! SunIsUp ) {
			CalcSolarFlux = 0.0;
			return CalcSolarFlux;
		}

		// get the incidence and reflection angles
		IncidAngle = std::acos( SOLCOS( 3 ) );
		RefractAngle = std::asin( std::sin( IncidAngle ) * AirRefIndex / WaterRefIndex );

		// absorbed component: Tau_a
		Absorbtance = std::exp( -PondExtCoef * this->Depth / std::cos( RefractAngle ) );

		// parallel and perpendicular components
		ParallelRad = pow_2( std::tan( RefractAngle - IncidAngle ) ) / pow_2( std::tan( RefractAngle + IncidAngle ) );
		PerpendRad = pow_2( std::sin( RefractAngle - IncidAngle ) ) / pow_2( std::sin( RefractAngle + IncidAngle ) );

		// transmittance: Tau
		Transmitance = 0.5 * Absorbtance * ( ( 1.0 - ParallelRad ) / ( 1.0 + ParallelRad ) + ( 1.0 - PerpendRad ) / ( 1.0 + PerpendRad ) );

		// reflectance: Tau_a - Tau
		Reflectance = Absorbtance - Transmitance;

		// apply reflectance to beam and diffuse solar to find flux
		CalcSolarFlux = ( 1.0 - Reflectance ) * ( SOLCOS( 3 ) * BeamSolarRad + DifSolarRad );

		return CalcSolarFlux;

	}

	//==============================================================================

	Real64
	PondGroundHeatExchangerData::CalcEffectiveness(
		Real64 const InsideTemperature, // Temperature of fluid in pipe circuit, in C
		Real64 const PondTemperature, // Temperature of pond water (i.e. outside the pipe), in C
		Real64 const MassFlowRate // Mass flow rate, in kg/s
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the "heat exchanger" effectiveness.
		// This routine is adapted from that in the low temp radiant pond model.

		// METHODOLOGY EMPLOYED:
		// The heat transfer coefficient is calculated at the pipe and
		// consists of inside and outside convection coefficients and conduction
		// through the pipe. The other assumptions are that the tube inside
		// surface temperature is equal to the "source location temperature"
		// and that it is a CONSTANT throughout the pond. External convection is
		// natural mode using Churchill and Chu correlation. Inside convection
		// calcualted using the Dittus-Boelter equation.

		// REFERENCES:
		// Incropera, F.P. and D.P. DeWitt, 1996. Introduction to Heat Transfer,
		//   3 rd Edition. John Wiley & Sons.
		// Churchill, S.W. and H.H.S. Chu. 1975. Correlating Equations for
		//   Laminar and Turbulent Free Convection from a Horizontal Cylinder.
		//   International Journal of Heat and Mass Transfer, 18: 1049-1053.
		// See also RadiantSystemLowTemp module.

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::NumOfTimeStepInHour;
		using General::RoundSigDigits;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetConductivityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using FluidProperties::GetDensityGlycol;

		// Return value
		Real64 CalcEffectiveness; // Function return variable

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		Real64 const GravConst( 9.81 ); // gravitational constant - should be fixed!
		static std::string const CalledFrom( "PondGroundHeatExchanger:CalcEffectiveness" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 NuseltNum; // Nuselt number (dimensionless)
		Real64 PrantlNum; // Prandtl number (dimensionless)
		Real64 ReynoldsNum; // Reynolds number (dimensionless)
		Real64 RayleighNum; // Rayleigh number (dimensionless)
		Real64 ThermDiff; // thermal diffusivity
		Real64 ExpansionCoef; // Expansion coefficient, in K^-1
		Real64 Viscosity; // Viscosity, in Ns/m2
		Real64 Density; // fluid density
		Real64 SpecificHeat; // Fluid specific heat
		Real64 Conductivity; // Fluid thermal conductivity
		Real64 WaterSpecHeat; // Specific heat of pond water
		Real64 NTU; // Number of transfer units, non-dimensional
		Real64 ConvCoefOut; // convection coefficient at outside of pipe
		Real64 ConvCoefIn; // convection coefficient at inside of pipe
		Real64 PipeResistance; // pipe wall thermal resistance
		Real64 TotalResistance; // total pipe thermal resistance - conduction and convection
		//  INTEGER, SAVE ::ErrCount=0
		//  INTEGER, SAVE ::ConsecutiveFrozen=0

		// evaluate properties at pipe fluid temperature for given pipe fluid

		SpecificHeat = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, InsideTemperature, PlantLoop( this->LoopNum ).FluidIndex, CalledFrom );
		Conductivity = GetConductivityGlycol( PlantLoop( this->LoopNum ).FluidName, InsideTemperature, PlantLoop( this->LoopNum ).FluidIndex, CalledFrom );
		Viscosity = GetViscosityGlycol( PlantLoop( this->LoopNum ).FluidName, InsideTemperature, PlantLoop( this->LoopNum ).FluidIndex, CalledFrom );
		Density = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, InsideTemperature, PlantLoop( this->LoopNum ).FluidIndex, CalledFrom );

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
		ReynoldsNum = 4.0 * MassFlowRate / ( Pi * Viscosity * TubeInDiameter * NumCircuits );

		PrantlNum = Viscosity * SpecificHeat / Conductivity;

		// Calculate the Nusselt number based on what flow regime one is in. h = (k)(Nu)/D
		if ( ReynoldsNum >= MaxLaminarRe ) { // Turbulent flow --> use Dittus-Boelter equation
			NuseltNum = 0.023 * std::pow( ReynoldsNum, 0.8 ) * std::pow( PrantlNum, 0.3 );
		} else { // Laminar flow --> use constant surface temperature relation
			NuseltNum = 3.66;
		}

		// inside convection resistance, from Nu
		ConvCoefIn = Conductivity * NuseltNum / TubeInDiameter;

		// now find properties of pond water - always assume pond fluid is water
		WaterSpecHeat = GetSpecificHeatGlycol( fluidNameWater, max( PondTemperature, 0.0 ), this->WaterIndex, CalledFrom );
		Conductivity = GetConductivityGlycol( fluidNameWater, max( PondTemperature, 0.0 ), this->WaterIndex, CalledFrom );
		Viscosity = GetViscosityGlycol( fluidNameWater, max( PondTemperature, 0.0 ), this->WaterIndex, CalledFrom );
		Density = GetDensityGlycol( fluidNameWater, max( PondTemperature, 0.0 ), this->WaterIndex, CalledFrom );

		// derived properties for natural convection coefficient
		// expansion coef (Beta) = -1/Rho. dRho/dT
		// The following code includes some slight modifications from Simon's original code.
		// It guarantees that the delta T is 10C and also avoids the problems associated with
		// water hitting a maximum density at around 4C. (RKS)
		ExpansionCoef = -( GetDensityGlycol( fluidNameWater, max( PondTemperature, 10.0 ) + 5.0, WaterIndex, CalledFrom ) - GetDensityGlycol( fluidNameWater, max( PondTemperature, 10.0 ) - 5.0, this->WaterIndex, CalledFrom ) ) / ( 10.0 * Density );

		ThermDiff = Conductivity / ( Density * WaterSpecHeat );
		PrantlNum = Viscosity * WaterSpecHeat / Conductivity;

		RayleighNum = Density * GravConst * ExpansionCoef * std::abs( InsideTemperature - PondTemperature ) * pow_3( TubeOutDiameter ) / ( Viscosity * ThermDiff );

		// Calculate the Nusselt number for natural convection at outside of pipe
		NuseltNum = pow_2( 0.6 + ( 0.387 * std::pow( RayleighNum, 1.0 / 6.0 ) / ( std::pow( 1.0 + 0.559 / std::pow( PrantlNum, 9.0 / 16.0 ), 8.0 / 27.0 ) ) ) );

		// outside convection resistance, from Nu
		ConvCoefOut = Conductivity * NuseltNum / TubeOutDiameter;

		// conduction resistance of pipe
		PipeResistance = TubeInDiameter / TubeConductivity * std::log( TubeOutDiameter / TubeInDiameter );

		TotalResistance = PipeResistance + 1.0 / ConvCoefIn + TubeInDiameter / ( TubeOutDiameter * ConvCoefOut );

		// Calculate the NTU parameter
		// NTU = UA/[(Mdot*Cp)min] = A/[Rtot*(Mdot*Cp)min]
		// where: Rtot = Ri,convection + Rconduction + Ro,conveciton
		//        A = Pi*D*TubeLength

		if ( MassFlowRate == 0.0 ) {
			CalcEffectiveness = 1.0;
		} else {
			NTU = Pi * TubeInDiameter * this->CircuitLength * NumCircuits / ( TotalResistance * MassFlowRate * SpecificHeat );
			// Calculate effectiveness - formula for static fluid
			CalcEffectiveness = ( 1.0 - std::exp( -NTU ) );
		}

		// Check for frozen pond
		if ( PondTemperature < 0.0 ) {
			++this->ConsecutiveFrozen;
			if ( this->FrozenErrIndex == 0 ) {
				ShowWarningMessage( "GroundHeatExchanger:Pond=\"" + this->Name + "\", is frozen; Pond model not valid. Calculated Pond Temperature=[" + RoundSigDigits( PondTemperature, 2 ) + "] C" );
				ShowContinueErrorTimeStamp( "" );
			}
			ShowRecurringWarningErrorAtEnd( "GroundHeatExchanger:Pond=\"" + this->Name + "\", is frozen", this->FrozenErrIndex, PondTemperature, PondTemperature, _, "[C]", "[C]" );
			if ( this->ConsecutiveFrozen >= NumOfTimeStepInHour * 30 ) {
				ShowFatalError( "GroundHeatExchanger:Pond=\"" + this->Name + "\" has been frozen for 30 consecutive hours.  Program terminates." );
			}
		} else {
			this->ConsecutiveFrozen = 0;
		}

		return CalcEffectiveness;

	}

	//==============================================================================

	void
	PondGroundHeatExchangerData::UpdatePondGroundHeatExchanger()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for pond
		// ground heat exchangers.   This routine must also set the outlet water
		// conditions.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PondGroundHeatExchanger:Update" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpFluid; // Specific heat of working fluid

		// Calculate the water side outlet conditions and set the
		// appropriate conditions on the correct HVAC node.
		CpFluid = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, this->InletTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
		// check for flow

		SafeCopyPlantNode( InletNodeNum, OutletNodeNum );

		if ( ( CpFluid > 0.0 ) && ( this->MassFlowRate > 0.0 ) ) {

			Node( OutletNodeNum ).Temp = this->InletTemp - this->HeatTransferRate / ( this->MassFlowRate * CpFluid );
			Node( OutletNodeNum ).Enthalpy = Node( OutletNodeNum ).Temp * CpFluid;
		}

		// keep track of the bulk temperature
		this->BulkTemperature = PondTemp;

	}

	//==============================================================================

	void
	PondGroundHeatExchangerData::ReportPondGroundHeatExchanger() // Index for the pond under consideration
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for Pond ground heat exchangers

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// update flows and temps from node data
		this->InletTemp = Node( this->InletNodeNum ).Temp;
		this->OutletTemp = Node( this->OutletNodeNum ).Temp;
		this->MassFlowRate = Node( this->InletNodeNum ).MassFlowRate;

		// update other variables from module variables
		this->Energy = this->HeatTransferRate * TimeStepSys * SecInHour;
		//this->PondTemp = PondTemp;

	}

} // PondGroundHeatExchanger

} // EnergyPlus
