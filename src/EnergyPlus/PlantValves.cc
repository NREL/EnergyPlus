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
	int NumTemperingValves;
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Object Data
	Array1D< TemperValveData > TemperValve; // dimension to No. of TemperingValve objects

	// Functions

	void
	SimPlantValves(
		int const CompTypeNum,
		std::string const & CompName,
		int & CompNum,
		bool const EP_UNUSED( RunFlag ), // unused1208
		bool & InitLoopEquip,
		Real64 & EP_UNUSED( MyLoad ), // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if First iteration of simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, NREL
		//       DATE WRITTEN   Jan. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulation manager for Plant valves

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int EqNum;

		if ( GetInputFlag ) {
			GetPlantValvesInput();
			GetInputFlag = false;
		}

		// Find the correct Equipment
		if ( CompNum == 0 ) {
			EqNum = FindItemInList( CompName, TemperValve );
			if ( EqNum == 0 ) {
				ShowFatalError( "SimPlantValves: Unit not found=" + CompName );
			}
			CompNum = EqNum;
		} else {
			EqNum = CompNum;
			if ( EqNum > NumTemperingValves || EqNum < 1 ) {
				ShowFatalError( "SimPlantValves:  Invalid CompNum passed=" + TrimSigDigits( EqNum ) + ", Number of Units=" + TrimSigDigits( NumTemperingValves ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( EqNum ) ) {
				if ( CompName != TemperValve( EqNum ).Name ) {
					ShowFatalError( "SimPlantValves: Invalid CompNum passed=" + TrimSigDigits( EqNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + TemperValve( EqNum ).Name );
				}
				CheckEquipName( EqNum ) = false;
			}
		}

		if ( InitLoopEquip ) {
			MinCap = 0.0;
			MaxCap = 0.0;
			OptCap = 0.0;
			return;
		}

		InitPlantValves( CompTypeNum, CompNum );

		CalcPlantValves( CompTypeNum, CompNum );

		UpdatePlantValves( CompTypeNum, CompNum );

		ReportPlantValves(); //(Args)

	}

	void
	GetPlantValvesInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jan. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get input from user

		// METHODOLOGY EMPLOYED:
		// usual method using InputProcessor

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound; // might also use FindItemInList
		using InputProcessor::GetObjectItem;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

	void
	InitPlantValves(
		int const CompTypeNum,
		int const CompNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, NREL
		//       DATE WRITTEN   Jan. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// intialize data for valve modeling

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataLoopNode::Node;
		using DataPlant::TypeOf_ValveTempering;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::GenEquipTypes_Pump;
		using DataBranchAirLoopPlant::ControlType_Active;
		using InputProcessor::SameString;
		using DataHVACGlobals::NumPlantLoops;
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
		int InletNode; // local working variable for inlet node number
		int OutletNode; // local working variable for outlet node number
		int Strm2Node; // local working variable for stream 2 outlet node number
		int SetPntNode; // local working variable for setpoint node number
		int PumpOutNode; // local working variable for pump outlet node number

		bool InNodeOnSplitter; // input data check
		bool PumpOutNodeOkay; // input data check
		bool ErrorsFound; // input data check
		bool TwoBranchesBetwn; // input data check
		bool SetPointNodeOkay; // input data check
		bool Stream2NodeOkay; // input data check
		bool IsBranchActive; // input data check

		int numLoopSides; // set to SIZE(PlantLoop(i)%LoopSide)

		static bool MyOneTimeFlag( true ); // first pass log
		static Array1D_bool MyTwoTimeFlag; // second pass do input check
		bool errFlag;

		{ auto const SELECT_CASE_var( CompTypeNum );

		if ( SELECT_CASE_var == TypeOf_ValveTempering ) {

			if ( MyOneTimeFlag ) {
				MyOneTimeFlag = false;
				MyTwoTimeFlag.dimension( NumTemperingValves, true );
			} else {
				// delay checks one pass so more of plant data structure gets filled in
				if ( MyTwoTimeFlag( CompNum ) ) {
					// do some checks on input data
					// Search thru PlantLoop Data Structure to check some things.
					// Locate the component on the plant loops for later usage
					errFlag = false;
					ScanPlantLoopsForObject( TemperValve( CompNum ).Name, TypeOf_ValveTempering, TemperValve( CompNum ).LoopNum, TemperValve( CompNum ).LoopSideNum, TemperValve( CompNum ).BranchNum, TemperValve( CompNum ).CompNum, _, _, _, _, _, errFlag );

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

					// . A) find indexes of PlantLoop, Half loop, and Branch by searching CompData
					if ( allocated( PlantLoop ) ) {
						for ( int i = 1; i <= NumPlantLoops; ++i ) {
							if ( ! allocated( PlantLoop( i ).LoopSide ) ) continue;
							numLoopSides = size( PlantLoop( i ).LoopSide );
							for ( int j = 1; j <= numLoopSides; ++j ) {
								if ( ! allocated( PlantLoop( i ).LoopSide( j ).Branch ) ) continue;
								for ( int k = 1, k_end = PlantLoop( i ).LoopSide( j ).TotalBranches; k <= k_end; ++k ) {
									if ( ! allocated( PlantLoop( i ).LoopSide( j ).Branch( k ).Comp ) ) continue;
									for ( int l = 1, l_end = PlantLoop( i ).LoopSide( j ).Branch( k ).TotalComponents; l <= l_end; ++l ) {

										if ( ( PlantLoop( i ).LoopSide( j ).Branch( k ).Comp( l ).TypeOf_Num == CompTypeNum ) && ( PlantLoop( i ).LoopSide( j ).Branch( k ).Comp( l ).CompNum == CompNum ) ) { // we found it.

											if ( ! SameString( PlantLoop( i ).LoopSide( j ).Branch( k ).Comp( l ).Name, TemperValve( CompNum ).Name ) ) {
												// why not, maybe plant loop structures not completely filled with available data?
												//write(*,*) 'Temper Valve names', PlantLoop(i)%LoopSide(j)%Branch(k)%Comp(l)%Name, TemperValve(CompNum)%Name
											}

											// is branch control type 'Active'
											if ( PlantLoop( i ).LoopSide( j ).Branch( k ).ControlType == ControlType_Active ) IsBranchActive = true;

											// is Valve inlet node an outlet node of a splitter
											if ( allocated( PlantLoop( i ).LoopSide( j ).Splitter ) ) {
												for ( int m = 1, m_end = PlantLoop( i ).LoopSide( j ).NumSplitters; m <= m_end; ++m ) {
													if ( allocated( PlantLoop( i ).LoopSide( j ).Splitter( m ).NodeNumOut ) ) {
														if ( any_eq( PlantLoop( i ).LoopSide( j ).Splitter( m ).NodeNumOut, TemperValve( CompNum ).PltInletNodeNum ) ) {
															InNodeOnSplitter = true;
														}
													} // allocated

													// are there only 2 branches between splitter and mixer?
													if ( PlantLoop( i ).LoopSide( j ).Splitter( m ).TotalOutletNodes == 2 ) {
														TwoBranchesBetwn = true;
													}
												} // loop over splitters
											} // allocated %splitter

											// is stream 2 node an inlet to the mixer ?
											if ( allocated( PlantLoop( i ).LoopSide( j ).Mixer ) ) {
												for ( int n = 1, n_end = PlantLoop( i ).LoopSide( j ).NumMixers; n <= n_end; ++n ) {
													if ( ! allocated( PlantLoop( i ).LoopSide( j ).Mixer( n ).NodeNumIn ) ) continue;
													if ( any_eq( PlantLoop( i ).LoopSide( j ).Mixer( n ).NodeNumIn, TemperValve( CompNum ).PltStream2NodeNum ) ) {

														// Check other branches component's node, current branch is k
														for ( int kk = 1, kk_end = PlantLoop( i ).LoopSide( j ).TotalBranches; kk <= kk_end; ++kk ) {
															if ( k == kk ) continue; //already looped into this one
															if ( ! allocated( PlantLoop( i ).LoopSide( j ).Branch( kk ).Comp ) ) continue;
															auto const & comp( PlantLoop( i ).LoopSide( j ).Branch( kk ).Comp );
															if ( std::any_of( comp.begin(), comp.end(), [CompNum]( DataPlant::CompData const & e ){ return e.NodeNumOut == TemperValve( CompNum ).PltStream2NodeNum; } ) ) { // it is on other branch
																Stream2NodeOkay = true;
															}
														} // kk branch nested loop
													} // stream 2 node is inlet to mixer
												} // mixer loop
											} // mixer allocated

											// is pump node really the outlet of a branch with a pump?
											for ( int kk = 1, kk_end = PlantLoop( i ).LoopSide( j ).TotalBranches; kk <= kk_end; ++kk ) {
												if ( PlantLoop( i ).LoopSide( j ).Branch( kk ).NodeNumOut == TemperValve( CompNum ).PltPumpOutletNodeNum ) {
													auto const & comp( PlantLoop( i ).LoopSide( j ).Branch( kk ).Comp );
													if ( std::any_of( comp.begin(), comp.end(), []( DataPlant::CompData const & e ){ return e.GeneralEquipType == DataPlant::GenEquipTypes_Pump; } ) ) { // it is on other branch
														//IF (PlantLoop(i)%LoopSide(j)%Branch(kk)%PumpPresent) THEN
														PumpOutNodeOkay = true;
													}
												}
											}

											// does sensor node agree with plant loop setpoint?
											if ( PlantLoop( i ).TempSetPointNodeNum == TemperValve( CompNum ).PltSetPointNodeNum ) {
												SetPointNodeOkay = true;
											}

										} //found item

									} // comps  l
								} // Branches k
							} // Loop Sides j
						} // Plant loops i
					} // plant loop allocated

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
						ShowFatalError( "Errors found in input, TemperingValve object " + TemperValve( CompNum ).Name );
					}
					MyTwoTimeFlag( CompNum ) = false;
				} // my two time flag for input checking

			} // my one time flag for input checking

			InletNode = TemperValve( CompNum ).PltInletNodeNum;
			OutletNode = TemperValve( CompNum ).PltOutletNodeNum;
			Strm2Node = TemperValve( CompNum ).PltStream2NodeNum;
			SetPntNode = TemperValve( CompNum ).PltSetPointNodeNum;
			PumpOutNode = TemperValve( CompNum ).PltPumpOutletNodeNum;

			if ( ( BeginEnvrnFlag ) && ( TemperValve( CompNum ).Init ) ) {

				if ( ( InletNode > 0 ) && ( OutletNode > 0 ) ) {
					//   Node(InletNode)%Temp = 0.0
					InitComponentNodes( 0.0, Node( PumpOutNode ).MassFlowRateMax, TemperValve( CompNum ).PltInletNodeNum, TemperValve( CompNum ).PltOutletNodeNum, TemperValve( CompNum ).LoopNum, TemperValve( CompNum ).LoopSideNum, TemperValve( CompNum ).BranchNum, TemperValve( CompNum ).CompNum );

				}
				TemperValve( CompNum ).Init = false;
			}

			if ( ! BeginEnvrnFlag ) TemperValve( CompNum ).Init = true;

			if ( InletNode > 0 ) {
				TemperValve( CompNum ).InletTemp = Node( InletNode ).Temp;
			}
			if ( Strm2Node > 0 ) {
				TemperValve( CompNum ).Stream2SourceTemp = Node( Strm2Node ).Temp;
			}
			if ( SetPntNode > 0 ) {
				TemperValve( CompNum ).SetPointTemp = Node( SetPntNode ).TempSetPoint;
			}

			if ( PumpOutNode > 0 ) {
				TemperValve( CompNum ).MixedMassFlowRate = Node( PumpOutNode ).MassFlowRate;
			}

		} else {
			// should not come here, would have been caught already
		}}

	}

	void
	CalcPlantValves(
		int const CompTypeNum,
		int const CompNum
	)
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

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_ValveTempering;
		using DataGlobals::KickOffSimulation;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tin; // local working variable for Inlet Temperature (C)
		Real64 Tset; // local working variable for Setpoint Temperature (C)
		Real64 Ts2; // local Working Variable for Stream 2 outlet Temperature (C)
		int LoopNum;
		int LoopSideNum;

		LoopNum = TemperValve( CompNum ).LoopNum;
		LoopSideNum = TemperValve( CompNum ).LoopSideNum;

		if ( KickOffSimulation ) return;

		{ auto const SELECT_CASE_var( CompTypeNum );

		if ( SELECT_CASE_var == TypeOf_ValveTempering ) {

			if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
				Tin = TemperValve( CompNum ).InletTemp;
				Tset = TemperValve( CompNum ).SetPointTemp;
				Ts2 = TemperValve( CompNum ).Stream2SourceTemp;

				if ( Ts2 <= Tset ) {
					TemperValve( CompNum ).FlowDivFract = 0.0;
				} else { // Divert some or all flow
					if ( Tin < Ts2 ) {
						TemperValve( CompNum ).FlowDivFract = ( Ts2 - Tset ) / ( Ts2 - Tin );
					} else {
						TemperValve( CompNum ).FlowDivFract = 1.0;
					}
				}
			} else if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) { // don't recalc diversion, just reuse current flows
				if ( TemperValve( CompNum ).MixedMassFlowRate > 0.0 ) {
					TemperValve( CompNum ).FlowDivFract = Node( TemperValve( CompNum ).PltOutletNodeNum ).MassFlowRate / TemperValve( CompNum ).MixedMassFlowRate;
				} else {
					TemperValve( CompNum ).FlowDivFract = 0.0;
				}

			}

			if ( TemperValve( CompNum ).FlowDivFract < 0.0 ) TemperValve( CompNum ).FlowDivFract = 0.0;
			if ( TemperValve( CompNum ).FlowDivFract > 1.0 ) TemperValve( CompNum ).FlowDivFract = 1.0;

		} else {
			// should not come here. would have been caught in init routine

		}}

	}

	void
	UpdatePlantValves(
		int const CompTypeNum,
		int const CompNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, NREL
		//       DATE WRITTEN   Jan. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Propagate calculations to rest of program

		// METHODOLOGY EMPLOYED:
		// set values at nodes

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;
		using DataPlant::TypeOf_ValveTempering;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 mdot; // local fluid mass flow rate

		{ auto const SELECT_CASE_var( CompTypeNum );

		if ( SELECT_CASE_var == TypeOf_ValveTempering ) {

			SafeCopyPlantNode( TemperValve( CompNum ).PltInletNodeNum, TemperValve( CompNum ).PltOutletNodeNum );

			// set mass flows in diverter path
			mdot = TemperValve( CompNum ).MixedMassFlowRate * TemperValve( CompNum ).FlowDivFract;

			if ( TemperValve( CompNum ).LoopNum > 0 ) {
				SetComponentFlowRate( mdot, TemperValve( CompNum ).PltInletNodeNum, TemperValve( CompNum ).PltOutletNodeNum, TemperValve( CompNum ).LoopNum, TemperValve( CompNum ).LoopSideNum, TemperValve( CompNum ).BranchNum, TemperValve( CompNum ).CompNum );

				TemperValve( CompNum ).DivertedFlowRate = mdot;
			} else {
				TemperValve( CompNum ).DivertedFlowRate = 0.0;
			}
		} else {

			// should not come here
		}}

	}

	void
	ReportPlantValves()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, NREL
		//       DATE WRITTEN   Jan. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		//<this routine is typically needed only for those cases where you must transform the internal data to a reportable form>

		// Nothing needs to be done (yet)

	}

} // PlantValves

} // EnergyPlus
