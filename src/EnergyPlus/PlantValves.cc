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
#include <algorithm>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <PlantValves.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantValves {

	// Module containing the routines dealing with the <module_name>

	// MODULE INFORMATION:
	//       AUTHOR         B. Griffith
	//       DATE WRITTEN   Jan, 2006
	//       MODIFIED       Nov 2010, B. Griffith, plant upgrades
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Collect "valve" type models for Plant loops

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using General::TrimSigDigits;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumTemperingValves( 0 );
	bool GetValveInputFlag( true );
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Object Data
	Array1D< TemperValveData > TemperValve; // dimension to No. of TemperingValve objects

	// Functions

	PlantComponent * TemperValveData::factory(int objectType, std::string objectName ) {
		if ( GetValveInputFlag ) {
			GetPlantValvesInput();
			GetValveInputFlag = false;
		}

		// Now look for this particular valve in the list
		for ( auto & valve : TemperValve ) {
			if ( valve.plantTypeOf_Num == objectType && valve.Name == objectName ) {
				return &valve;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "TemperValveData::factory: Error getting inputs for valve named: " + objectName );
		// Shut up the compiler
		return nullptr;

	}

	void TemperValveData::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ), bool const EP_UNUSED( FirstHVACIteration ), Real64 & EP_UNUSED( CurLoad ), bool const EP_UNUSED( RunFlag ) ) {

		// PURPOSE OF THIS SUBROUTINE:
		// Simulation manager for Plant valves

		this->init();

		this->calc();

		this->update();

		this->report();

	}

	void
	GetPlantValvesInput()
	{

		using InputProcessor::GetNumObjectsFound; // might also use FindItemInList
		using InputProcessor::GetObjectItem;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;

		int Item; // Item to be "gotten"
		Array1D_string Alphas( 6 ); // Alpha items for object
		Array1D< Real64 > Numbers( 1 ); // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		std::string CurrentModuleObject; // for ease in renaming.

		CurrentModuleObject = "TemperingValve";
		NumTemperingValves = GetNumObjectsFound( CurrentModuleObject );

		TemperValve.allocate( NumTemperingValves );
		CheckEquipName.dimension( NumTemperingValves, true );

		for ( Item = 1; Item <= NumTemperingValves; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus );
			//  <process, noting errors>
			TemperValve( Item ).Name = Alphas( 1 );
			TemperValve( Item ).plantTypeOf_Num = DataPlant::TypeOf_ValveTempering;
			// Get Plant Inlet Node
			TemperValve( Item ).PltInletNodeNum = GetOnlySingleNode( Alphas( 2 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			// Get Plant Outlet Node
			TemperValve( Item ).PltOutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			// Get Stream 2 Source Node
			TemperValve( Item ).PltStream2NodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			// Get Mixed water Setpoint
			TemperValve( Item ).PltSetPointNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_SetPoint, 1, ObjectIsNotParent );

			// Get Pump outlet
			TemperValve( Item ).PltPumpOutletNodeNum = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			// Note most checks on user input are made in second pass thru init routine

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 2 ), Alphas( 3 ), "Supply Side Water Nodes" );

		}

		for ( Item = 1; Item <= NumTemperingValves; ++Item ) {

			SetupOutputVariable( "Tempering Valve Flow Fraction []", TemperValve( Item ).FlowDivFract, "System", "Average", TemperValve( Item ).Name );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetPlantValvesInput: " + CurrentModuleObject + " Errors found in input" );
		}

	}

	void TemperValveData::init(){
	
		using DataGlobals::BeginEnvrnFlag;
		using DataLoopNode::Node;
		using DataPlant::TypeOf_ValveTempering;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::GenEquipTypes_Pump;
		using DataBranchAirLoopPlant::ControlType_Active;
		using InputProcessor::SameString;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool InNodeOnSplitter; // input data check
		bool PumpOutNodeOkay; // input data check
		bool ErrorsFound; // input data check
		bool TwoBranchesBetwn; // input data check
		bool SetPointNodeOkay; // input data check
		bool Stream2NodeOkay; // input data check
		bool IsBranchActive; // input data check

		bool errFlag;

		{ auto const SELECT_CASE_var( this->plantTypeOf_Num );

		if ( SELECT_CASE_var == TypeOf_ValveTempering ) {

			if ( this->oneTimeFlag ) {
				this->oneTimeFlag = false;

			} else {
				// delay checks one pass so more of plant data structure gets filled in
				if ( this->twoTimeFlag && allocated( PlantLoop ) ) {
					// do some checks on input data
					// Search thru PlantLoop Data Structure to check some things.
					// Locate the component on the plant loops for later usage
					errFlag = false;
					ScanPlantLoopsForObject( this->Name, TypeOf_ValveTempering, this->location.loopNum, this->location.loopSideNum, this->location.branchNum, this->location.compNum, _, _, _, _, _, errFlag );

					if ( errFlag ) {
						ShowFatalError( "InitPlantValves: Program terminated due to previous condition(s)." );
					}
					// init logical flags
					ErrorsFound = false;
					InNodeOnSplitter = false;
					PumpOutNodeOkay = false;
					TwoBranchesBetwn = false;
					SetPointNodeOkay = false;
					Stream2NodeOkay = false;
					IsBranchActive = false;



					// is branch control type 'Active'
					if ( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Branch( this->location.branchNum ).ControlType == ControlType_Active ) IsBranchActive = true;

					// is Valve inlet node an outlet node of a splitter
					if ( allocated( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Splitter ) ) {
						for ( int m = 1, m_end = PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).NumSplitters; m <= m_end; ++m ) {
							if ( allocated( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Splitter( m ).NodeNumOut ) ) {
								if ( any_eq( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Splitter( m ).NodeNumOut, this->PltInletNodeNum ) ) {
									InNodeOnSplitter = true;
								}
							} // allocated

							// are there only 2 branches between splitter and mixer?
							if ( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Splitter( m ).TotalOutletNodes == 2 ) {
								TwoBranchesBetwn = true;
							}
						} // loop over splitters
					} // allocated %splitter

					// is stream 2 node an inlet to the mixer ?
					if ( allocated( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Mixer ) ) {
						for ( int n = 1, n_end = PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).NumMixers; n <= n_end; ++n ) {
							if ( ! allocated( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Mixer( n ).NodeNumIn ) ) continue;
							if ( any_eq( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Mixer( n ).NodeNumIn, this->PltStream2NodeNum ) ) {

								// Check other branches component's node, current branch is k
								for ( int kk = 1, kk_end = PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).TotalBranches; kk <= kk_end; ++kk ) {
									if ( this->location.branchNum == kk ) continue; //already looped into this one
									if ( ! allocated( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Branch( kk ).Comp ) ) continue;
									auto const & comp( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Branch( kk ).Comp );
									if ( std::any_of( comp.begin(), comp.end(), [ this ]( DataPlant::CompData const & e ){ return e.NodeNumOut == this->PltStream2NodeNum; } ) ) { // it is on other branch
										Stream2NodeOkay = true;
									}
								} // kk branch nested loop
							} // stream 2 node is inlet to mixer
						} // mixer loop
					} // mixer allocated

					// is pump node really the outlet of a branch with a pump?
					for ( int kk = 1, kk_end = PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).TotalBranches; kk <= kk_end; ++kk ) {
						if ( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Branch( kk ).NodeNumOut == this->PltPumpOutletNodeNum ) {
							auto const & comp( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).Branch( kk ).Comp );
							if ( std::any_of( comp.begin(), comp.end(), []( DataPlant::CompData const & e ){ return e.GeneralEquipType == DataPlant::GenEquipTypes_Pump; } ) ) { // it is on other branch
								//IF (PlantLoop(i)%LoopSide(j)%Branch(kk)%PumpPresent) THEN
								PumpOutNodeOkay = true;
							}
						}
					}

					// does sensor node agree with plant loop setpoint?
					if ( PlantLoop( this->location.loopNum ).TempSetPointNodeNum == this->PltSetPointNodeNum ) {
						SetPointNodeOkay = true;
					}


					if ( ! IsBranchActive ) {
						ShowSevereError( "TemperingValve object needs to be on an ACTIVE branch" );
						ErrorsFound = true;
					}

					if ( ! InNodeOnSplitter ) {
						ShowSevereError( "TemperingValve object needs to be between a Splitter and Mixer" );
						ErrorsFound = true;
					}

					if ( ! PumpOutNodeOkay ) {
						ShowSevereError( "TemperingValve object needs to reference a node that is the outlet of a pump on its loop" );
						ErrorsFound = true;
					}

					if ( ! TwoBranchesBetwn ) {
						ShowSevereError( "TemperingValve object needs exactly two branches between a Splitter and Mixer" );
						ErrorsFound = true;
					}

					if ( ! SetPointNodeOkay ) {
						ShowSevereError( "TemperingValve object setpoint node not valid.  Check Setpoint manager for Plant Loop Temp Setpoint" );
						ErrorsFound = true;
					}

					if ( ! Stream2NodeOkay ) {
						ShowSevereError( "TemperingValve object stream 2 source node not valid." );
						ShowContinueError( "Check that node is a component outlet, enters a mixer, and on the other branch" );
						ErrorsFound = true;
					}
					if ( ErrorsFound ) {
						ShowFatalError( "Errors found in input, TemperingValve object " + this->Name );
					}
					this->twoTimeFlag = false;
				} // my two time flag for input checking

			} // my one time flag for input checking


			if ( ( BeginEnvrnFlag ) && ( this->Init ) ) {

				if ( ( this->PltInletNodeNum > 0 ) && ( this->PltOutletNodeNum > 0 ) ) {

					InitComponentNodes( 0.0, Node( this->PltPumpOutletNodeNum ).MassFlowRateMax, this->PltInletNodeNum, this->PltOutletNodeNum, this->location.loopNum, this->location.loopSideNum, this->location.branchNum, this->location.compNum );

				}
				this->Init = false;
			}

			if ( ! BeginEnvrnFlag ) this->Init = true;

			if ( this->PltInletNodeNum > 0 ) {
				this->InletTemp = Node( this->PltInletNodeNum ).Temp;
			}
			if ( this->PltStream2NodeNum > 0 ) {
				this->Stream2SourceTemp = Node( this->PltStream2NodeNum ).Temp;
			}
			if ( this->PltSetPointNodeNum > 0 ) {
				this->SetPointTemp = Node( this->PltSetPointNodeNum ).TempSetPoint;
			}

			if ( this->PltPumpOutletNodeNum > 0 ) {
				this->MixedMassFlowRate = Node( this->PltPumpOutletNodeNum ).MassFlowRate;
			}

		} else {
			// should not come here, would have been caught already
		}}

	}

	void TemperValveData::calc()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, NREL
		//       DATE WRITTEN   Jan. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine does the calculations for Valves.
		//  Currently only one type of valve, for Tempering.

		// METHODOLOGY EMPLOYED:
		//   Tempering valve calculations involve computing a flow fraction
		//     that should be diverted.  See update routine for setting flow rates.

		// Using/Aliasing
		using DataPlant::PlantLoop;

		Real64 Tin; // local working variable for Inlet Temperature (C)
		Real64 Tset; // local working variable for Setpoint Temperature (C)
		Real64 Ts2; // local Working Variable for Stream 2 outlet Temperature (C)

		if ( DataGlobals::KickOffSimulation ) return;

		{ auto const SELECT_CASE_var( this->plantTypeOf_Num );

		if ( SELECT_CASE_var == DataPlant::TypeOf_ValveTempering ) {

			if ( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).FlowLock == 0 ) {
				Tin  = this->InletTemp;
				Tset = this->SetPointTemp;
				Ts2  = this->Stream2SourceTemp;

				if ( Ts2 <= Tset ) {
					this->FlowDivFract = 0.0;
				} else { // Divert some or all flow
					if ( Tin < Ts2 ) {
						this->FlowDivFract = ( Ts2 - Tset ) / ( Ts2 - Tin );
					} else {
						this->FlowDivFract = 1.0;
					}
				}
			} else if ( PlantLoop( this->location.loopNum ).LoopSide( this->location.loopSideNum ).FlowLock == 1 ) { // don't recalc diversion, just reuse current flows
				if ( this->MixedMassFlowRate > 0.0 ) {
					this->FlowDivFract = Node( this->PltOutletNodeNum ).MassFlowRate / this->MixedMassFlowRate;
				} else {
					this->FlowDivFract = 0.0;
				}

			}

			if ( this->FlowDivFract < 0.0 ) this->FlowDivFract = 0.0;
			if ( this->FlowDivFract > 1.0 ) this->FlowDivFract = 1.0;

		} else {
			// should not come here. would have been caught in init routine

		}}

	}

	void TemperValveData::update ()
	{
		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;


		Real64 mdot; // local fluid mass flow rate

		{ auto const SELECT_CASE_var( this->plantTypeOf_Num );

		if ( SELECT_CASE_var == DataPlant::TypeOf_ValveTempering ) {

			SafeCopyPlantNode( this->PltInletNodeNum, this->PltOutletNodeNum );

			// set mass flows in diverter path
			mdot = this->MixedMassFlowRate * this->FlowDivFract;

			if ( this->location.loopNum > 0 ) {
				SetComponentFlowRate( mdot, this->PltInletNodeNum, this->PltOutletNodeNum, this->location.loopNum, this->location.loopSideNum, this->location.branchNum, this->location.compNum );

				this->DivertedFlowRate = mdot;
			} else {
				this->DivertedFlowRate = 0.0;
			}
		} else {
			// should not come here
		}}
	}

	void TemperValveData::report()
	{
		//<this routine is typically needed only for those cases where you must transform the internal data to a reportable form.  Nothing needs to be done to reports. 
	}

} // PlantValves

} // EnergyPlus
