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
#include <cassert>
#include <cmath>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <PlantCondLoopOperation.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRuntimeLanguage.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantCondLoopOperation {

	// MODIFIED       LKL Sep 03: adding integer/pointers for various parts of operation schemes
	// MODIFIED       DEF JUL 10: complete re-write to support new Plant manager

	// PURPOSE OF THIS MODULE: This module assigns loads to the equipment on
	// the plant and condenser loops that will operate
	// for a given timestep.

	// METHODOLOGY EMPLOYED:  The main driver, "ManagePlantLoadDistribution",
	// gets 'Plant Operation scheme' and 'Plant Equipment List' input.  Pointers are
	// set up in the PlantLoop data structure to allow components to directly access the
	// operation schemes and plant lists that the component shows up on.
	// ManagePlantLoadDistribution is called one time for each component on the loop.
	// It finds the operation scheme and equipment list associated with the component
	// and calculates the component load.  If the component is part of a 'load range'
	// based scheme, it also assigns a component load to each of the components on the
	// equipment list.

	// REFERENCES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataPlant;
	using DataHVACGlobals::NumPlantLoops;
	using DataHVACGlobals::NumCondLoops;
	using DataHVACGlobals::SmallLoad;
	using FluidProperties::GetSpecificHeatGlycol;

	// Data
	//MODULE PARAMETER DEFINITIONS
	int const HeatingOp( 1 ); // Constant for Heating Operation
	int const CoolingOp( 2 ); // Constant for Cooling Operation
	int const DualOp( 3 ); // Constant for Cooling or Heating Operation

	bool const TurnItemOn( true ); // Convenient for calling TurnPlantItemOnOff instead of hardwired true/false
	bool const TurnItemOff( false ); // Convenient for calling TurnPlantItemOnOff instead of hardwired true/false

	namespace{
		bool GetPlantOpInput( true ); // operation Get Input flag
		bool InitLoadDistributionOneTimeFlag( true );
		bool LoadEquipListOneTimeFlag( true );
	}
	//MODULE VARIABLE DECLARATIONS:

	//SUBROUTINE SPECIFICATIONS FOR MODULE  !SUBROUTINE SPECIFICATIONS FOR MODULE
	//Driver Routines
	//Get Input Routines
	//Initialization Routines
	//Load Distribution/Calculation Routines

	//ON/OFF Utility Routines

	//PLANT EMS Utility Routines

	// MODULE SUBROUTINES:

	// Beginning of Module Driver Subroutines
	//*************************************************************************

	// Functions
	void
	clear_state(){
		GetPlantOpInput = true;
		InitLoadDistributionOneTimeFlag = true;
		LoadEquipListOneTimeFlag = true;
	}

	void
	ManagePlantLoadDistribution(
		int const LoopNum, // PlantLoop data structure loop counter
		int const LoopSideNum, // PlantLoop data structure LoopSide counter
		int const BranchNum, // PlantLoop data structure branch counter
		int const CompNum, // PlantLoop data structure component counter
		Real64 & LoopDemand,
		Real64 & RemLoopDemand,
		bool const FirstHVACIteration,
		bool & LoopShutDownFlag, // EMS flag to tell loop solver to shut down pumps
		Optional_bool LoadDistributionWasPerformed
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1999
		//       REVISED:         March 2001
		//                        July 2001, Rick Strand (revision of pump and loop control code)
		//                        July 2010, Dan Fisher, complete rewrite to component based control

		// PURPOSE OF THIS SUBROUTINE:
		// ManageLoopOperation is the driver routine
		// for plant equipment selection.  It calls the general "Get-
		// Input" routines, initializes the loop pointers, then calls the
		// appropriate type of control algorithm (setpoint, load range based,
		// or uncontrolled) for the component

		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// Using/Aliasing
		using DataEnvironment::OutWetBulbTemp; // Current outdoor relative humidity [%]
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutDewPointTemp;
		using DataEnvironment::OutRelHum;
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

		int ListNum; // DO loop index in PlantLoop()%LoopSide()%Branch()%Comp()%OpScheme()%EquipList(ListNum)
		int CurListNum; // Current list...= ListNum,  used for error checking only
		//Indices in PlantLoop.LoopSide.Branch.Comp data structure
		int CurCompLevelOpNum; // This is set by the init routine at each FirstHVACIteration.
		//It tells which scheme for this component is currently scheduled
		//and is used to avoid a 'schedule search' on each call
		//It is used as the OpScheme index in PlantLoop.LoopSide.Branch.Comp.OpScheme(CurCompLevelOpNum)
		//Value of pointers held in PlantLoop.LoopSide.Branch.Comp() data structure
		//Used as indices in PlantLoop.OpScheme() data structure
		int CurSchemePtr; // set by PlantLoop.LoopSide.Branch.Comp.OpScheme.OpSchemePtr
		//used to locate data in PL()%OpScheme(CurSchemePtr)
		int ListPtr; // !set by PL()%LoopSide()%Branch()%Comp()%OpScheme(CurCompLevelOpNum)%EquipList(CurListNum)ListPtr
		//used to locate data in PL()%OpScheme(CurSchemePtr)%EquipList(ListPtr)
		//Local values from the PlantLoop()%OpScheme() data structure
		int CurSchemeType; // identifier set in PlantData
		Real64 RangeVariable( 0.0 ); // holds the 'loop demand', wetbulb temp, etc.
		Real64 TestRangeVariable; // abs of RangeVariable for logic tests etc.
		Real64 RangeHiLimit; // upper limit of the range variable
		Real64 RangeLoLimit; // lower limit of the range variable
		//Local values from the PlantLoop()%LoopSide()%Branch()%Comp() data structure
		int NumEquipLists; // number of equipment lists
		//Error control flags
		bool foundlist; // equipment list found
		int NumCompsOnList;
		int CompIndex;
		int EquipBranchNum;
		int EquipCompNum;

		//Shut down equipment and return if so instructed by LoopShutDownFlag
		if ( LoopShutDownFlag ) {
			TurnOffLoopEquipment( LoopNum );
			return;
		}

		// Return if there are no loop operation schemes available
		if ( ! std::any_of( PlantLoop( LoopNum ).OpScheme.begin(), PlantLoop( LoopNum ).OpScheme.end(), []( DataPlant::OperationData const & e ){ return e.Available; } ) ) return;

		// set up references
		auto & loop_side( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		auto & this_component( loop_side.Branch( BranchNum ).Comp( CompNum ) );

		//Implement EMS control commands
		ActivateEMSControls( LoopNum, LoopSideNum, BranchNum, CompNum, LoopShutDownFlag );

		//Schedules are checked and CurOpScheme updated on FirstHVACIteration in InitLoadDistribution
		//Here we just load CurOpScheme to a local variable
		CurCompLevelOpNum = this_component.CurCompLevelOpNum;
		//If no current operation scheme for component, RETURN
		if ( CurCompLevelOpNum == 0 ) return;
		//set local variables from data structure
		NumEquipLists = this_component.OpScheme( CurCompLevelOpNum ).NumEquipLists;
		CurSchemePtr = this_component.OpScheme( CurCompLevelOpNum ).OpSchemePtr;
		CurSchemeType = PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).OpSchemeType;

		// another reference
		auto & this_op_scheme( PlantLoop( LoopNum ).OpScheme( CurSchemePtr ) );

		//Load the 'range variable' according to the type of control scheme specified
		if ( ( CurSchemeType == UncontrolledOpSchemeType ) || ( CurSchemeType == CompSetPtBasedSchemeType ) ) {
			//No RangeVariable specified for these types
		} else if ( CurSchemeType == EMSOpSchemeType ) {
			InitLoadDistribution( FirstHVACIteration );
			//No RangeVariable specified for these types
		} else if ( CurSchemeType == HeatingRBOpSchemeType ) {
			// For zero demand, we need to clean things out before we leave
			if ( LoopDemand < SmallLoad ) {
				InitLoadDistribution( FirstHVACIteration );
				this_component.MyLoad = 0.0;
				this_component.ON = false;
				return;
			}
			RangeVariable = LoopDemand;
		} else if ( CurSchemeType == CoolingRBOpSchemeType ) {
			// For zero demand, we need to clean things out before we leave
			if ( LoopDemand > ( -1.0 * SmallLoad ) ) {
				InitLoadDistribution( FirstHVACIteration );
				this_component.MyLoad = 0.0;
				this_component.ON = false;
				return;
			}
			RangeVariable = LoopDemand;
		} else if ( CurSchemeType == DryBulbRBOpSchemeType ) {
			RangeVariable = OutDryBulbTemp;
		} else if ( CurSchemeType == WetBulbRBOpSchemeType ) {
			RangeVariable = OutWetBulbTemp;
		} else if ( CurSchemeType == RelHumRBOpSchemeType ) {
			RangeVariable = OutRelHum;
		} else if ( CurSchemeType == DewPointRBOpSchemeType ) {
			RangeVariable = OutDewPointTemp;
		} else if ( ( CurSchemeType == DryBulbTDBOpSchemeType ) || ( CurSchemeType == WetBulbTDBOpSchemeType ) || ( CurSchemeType == DewPointTDBOpSchemeType ) ) {
			RangeVariable = FindRangeVariable( LoopNum, CurSchemePtr, CurSchemeType );
		} else {
			// No controls specified.  This is a fatal error
			ShowFatalError( "Invalid Operation Scheme Type Requested=" + PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).TypeOf + ", in ManagePlantLoadDistribution" );
		}

		//Find the proper list within the specified scheme
		foundlist = false;
		if ( CurSchemeType == UncontrolledOpSchemeType ) {
			//!***what else do we do with 'uncontrolled' equipment?
			//There's an equipment list...but I think the idea is to just
			//Set one component to run in an 'uncontrolled' way (whatever that means!)

		} else if ( CurSchemeType == CompSetPtBasedSchemeType ) {
			//check for EMS Control
			TurnOnPlantLoopPipes( LoopNum, LoopSideNum );
			FindCompSPLoad( LoopNum, LoopSideNum, BranchNum, CompNum, CurCompLevelOpNum );
		} else if ( CurSchemeType == EMSOpSchemeType ) {
			TurnOnPlantLoopPipes( LoopNum, LoopSideNum );
			DistributeUserDefinedPlantLoad( LoopNum, LoopSideNum, BranchNum, CompNum, CurCompLevelOpNum, CurSchemePtr, LoopDemand, RemLoopDemand );
		} else { //it's a range based control type with multiple equipment lists
			CurListNum = 0;
			for ( ListNum = 1; ListNum <= NumEquipLists; ++ListNum ) {
				//setpointers to 'PlantLoop()%OpScheme()...'structure
				ListPtr = this_component.OpScheme( CurCompLevelOpNum ).EquipList( ListNum ).ListPtr;
				RangeHiLimit = this_op_scheme.EquipList( ListPtr ).RangeUpperLimit;
				RangeLoLimit = this_op_scheme.EquipList( ListPtr ).RangeLowerLimit;
				//these limits are stored with absolute values, but the LoopDemand can be negative for cooling
				TestRangeVariable = std::abs( RangeVariable );

				//trying to do something where the last stage still runs the equipment but at the hi limit.

				if ( TestRangeVariable < RangeLoLimit || TestRangeVariable > RangeHiLimit ) {
					if ( ( TestRangeVariable > RangeHiLimit ) && ( ListPtr == this_op_scheme.EquipListNumForLastStage ) ) {
						// let this go thru, later AdjustChangeInLoadForLastStageUpperRangeLimit will cap dispatch to RangeHiLimit
						CurListNum = ListNum;
						break;
					} else {
						continue;
					}
				} else {
					CurListNum = ListNum;
					break;
				}
			}

			if ( CurListNum > 0 ) {
				// there could be equipment on another list that needs to be nulled out, it may have a load from earlier iteration
				for ( ListNum = 1; ListNum <= NumEquipLists; ++ListNum ) {
					if ( ListNum == CurListNum ) continue; // leave current one alone
					NumCompsOnList = this_op_scheme.EquipList( ListNum ).NumComps;
					for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {
						EquipBranchNum = this_op_scheme.EquipList( ListNum ).Comp( CompIndex ).BranchNumPtr;
						EquipCompNum = this_op_scheme.EquipList( ListNum ).Comp( CompIndex ).CompNumPtr;
						loop_side.Branch( EquipBranchNum ).Comp( EquipCompNum ).MyLoad = 0.0;
					}
				}
				if ( this_op_scheme.EquipList( ListPtr ).NumComps > 0 ) {
					TurnOnPlantLoopPipes( LoopNum, LoopSideNum );
					DistributePlantLoad( LoopNum, LoopSideNum, CurSchemePtr, ListPtr, LoopDemand, RemLoopDemand );
					if ( present( LoadDistributionWasPerformed ) ) LoadDistributionWasPerformed = true;
				}
			}

		} // End of range based schemes

	}

	// Beginning of GetInput subroutines for the Module
	//******************************************************************************

	void
	GetPlantOperationInput( bool & GetInputOK )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   October 1998
		//       MODIFIED       July 2010, Dan Fisher, restructure input data
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine reads the primary plant loop
		// operation schemes from the input file

		// METHODOLOGY EMPLOYED: calls the Input Processor to retrieve data from input file.
		// The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
		// following keywords is reflected exactly in this subroutine:
		//    PlantEquipmentOperationSchemes
		//    CondenserEquipmentOperationSchemes

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetScheduleIndex;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetPlantOperationInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNum; // Loop counter (Plant or Cond)
		int OpNum; // Scheme counter
		int Num; // Item counter
		int NumPlantOpSchemes; // Total Number of PlantEquipmentOperationSchemes
		int NumCondOpSchemes; // Total Number of CondenserEquipmentOperationSchemes
		int NumAlphas; // Number of alpha items in the input object
		int NumNums; // Number of numeric items in the input object
		int IOStat;
		std::string PlantOpSchemeName; // Name of the plant or condenser operating scheme
		std::string CurrentModuleObject; // for ease in renaming
		std::string PlantLoopObject; // for ease in renaming
		Array1D_string OpSchemeNames; // Used to verify unique op scheme names
		bool IsNotOK;
		bool IsBlank;
		bool ErrorsFound; // Passed in from OpSchemeInput

		ErrorsFound = false;

		if ( ! allocated( PlantLoop ) ) {
			GetInputOK = false;
			return;
		} else {
			GetInputOK = true;
		}

		// get number of operation schemes
		CurrentModuleObject = "PlantEquipmentOperationSchemes";
		NumPlantOpSchemes = GetNumObjectsFound( CurrentModuleObject );

		if ( NumPlantOpSchemes > 0 ) {
			// OpSchemeListNames is used to determine if there are any duplicate operation scheme names
			OpSchemeNames.allocate( NumPlantOpSchemes );
			Num = 0;
			for ( OpNum = 1; OpNum <= NumPlantOpSchemes; ++OpNum ) {
				GetObjectItem( CurrentModuleObject, OpNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), OpSchemeNames, Num, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					continue;
				}
				++Num;
				OpSchemeNames( Num ) = cAlphaArgs( 1 );
			}

			OpSchemeNames.deallocate();

		}

		CurrentModuleObject = "CondenserEquipmentOperationSchemes";
		NumCondOpSchemes = GetNumObjectsFound( CurrentModuleObject );

		if ( NumCondOpSchemes > 0 ) {
			// OpSchemeListNames is used to determine if there are any duplicate operation scheme names
			OpSchemeNames.allocate( NumCondOpSchemes );
			Num = 0;
			for ( OpNum = 1; OpNum <= NumCondOpSchemes; ++OpNum ) {
				GetObjectItem( CurrentModuleObject, OpNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), OpSchemeNames, Num, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					continue;
				}
				++Num;
				OpSchemeNames( Num ) = cAlphaArgs( 1 );
			}

			OpSchemeNames.deallocate();

		}

		//Load the Plant data structure
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			PlantOpSchemeName = PlantLoop( LoopNum ).OperationScheme;
			if ( LoopNum <= NumPlantLoops ) {
				CurrentModuleObject = "PlantEquipmentOperationSchemes";
				PlantLoopObject = "PlantLoop";
			} else {
				CurrentModuleObject = "CondenserEquipmentOperationSchemes";
				PlantLoopObject = "CondenserLoop";
			}
			OpNum = GetObjectItemNum( CurrentModuleObject, PlantOpSchemeName );
			if ( OpNum > 0 ) {
				GetObjectItem( CurrentModuleObject, OpNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				PlantLoop( LoopNum ).NumOpSchemes = ( NumAlphas - 1 ) / 3;
				if ( PlantLoop( LoopNum ).NumOpSchemes > 0 ) {
					PlantLoop( LoopNum ).OpScheme.allocate( PlantLoop( LoopNum ).NumOpSchemes );
					for ( Num = 1; Num <= PlantLoop( LoopNum ).NumOpSchemes; ++Num ) {
						PlantLoop( LoopNum ).OpScheme( Num ).TypeOf = cAlphaArgs( Num * 3 - 1 );

						{ auto const plantLoopOperation( PlantLoop( LoopNum ).OpScheme( Num ).TypeOf );

						if ( plantLoopOperation == "LOAD RANGE BASED OPERATION" ) { // Deprecated
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = LoadRBOpSchemeType; // Deprecated
							ShowSevereError( CurrentModuleObject + " = \"" + cAlphaArgs( 1 ) + "\" deprecated field value =\"" + PlantLoop( LoopNum ).OpScheme( Num ).TypeOf + "\"." );
							ShowContinueError( "... should be replaced with PlantEquipmentOperation:CoolingLoad or PlantEquipmentOperation:HeatingLoad" );
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:COOLINGLOAD" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = CoolingRBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:HEATINGLOAD" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = HeatingRBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT" ) { //* Temp Based Control
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = CompSetPtBasedSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:THERMALENERGYSTORAGE" ) { //* Simple TES Control
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = CompSetPtBasedSchemeType; // set this to component based as it will be converted to this
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:USERDEFINED" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = EMSOpSchemeType;
							AnyEMSPlantOpSchemesInModel = true;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = DryBulbRBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORWETBULB" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = WetBulbRBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = DewPointRBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = RelHumRBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = DryBulbTDBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = WetBulbTDBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = DewPointTDBOpSchemeType;
						} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:UNCONTROLLED" ) {
							PlantLoop( LoopNum ).OpScheme( Num ).OpSchemeType = UncontrolledOpSchemeType;
						} else { // invalid op scheme type for plant loop
							ShowSevereError( RoutineName + "Invalid " + cAlphaFieldNames( Num * 3 - 1 ) + '=' + cAlphaArgs( Num * 3 - 1 ) + ", entered in " + CurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}}

						PlantLoop( LoopNum ).OpScheme( Num ).Name = cAlphaArgs( Num * 3 );
						PlantLoop( LoopNum ).OpScheme( Num ).Sched = cAlphaArgs( Num * 3 + 1 );
						PlantLoop( LoopNum ).OpScheme( Num ).SchedPtr = GetScheduleIndex( PlantLoop( LoopNum ).OpScheme( Num ).Sched );
						if ( PlantLoop( LoopNum ).OpScheme( Num ).SchedPtr == 0 ) {
							ShowSevereError( RoutineName + "Invalid " + cAlphaFieldNames( Num * 3 + 1 ) + " = \"" + cAlphaArgs( Num * 3 + 1 ) + "\", entered in " + CurrentModuleObject + "= \"" + cAlphaArgs( 1 ) + "\"." );
							ErrorsFound = true;
						}
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + cAlphaArgs( 1 ) + "\", requires at least " + cAlphaFieldNames( 2 ) + ", " + cAlphaFieldNames( 3 ) + " and " + cAlphaFieldNames( 4 ) + " to be specified." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( RoutineName + PlantLoopObject + '=' + PlantLoop( LoopNum ).Name + " is expecting" );
				ShowContinueError( CurrentModuleObject + '=' + PlantOpSchemeName + ", but not found." );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input for PlantEquipmentOperationSchemes or CondenserEquipmentOperationSchemes" );
		}

	}

	void
	GetOperationSchemeInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   October 1998
		//       MODIFIED       August 2001, LKL -- Validations
		//       RE-ENGINEERED  July 2010, Dan Fisher, restructure input data

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the primary plant loop
		// operation schemes from the input file

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.
		// The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
		// following keywords is reflected exactly in this subroutine:
		//    PlantEquipmentOperation:*

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using NodeInputManager::GetOnlySingleNode;
		using namespace DataLoopNode;
		using namespace DataSizing;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOperationSchemeInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SchemeNum;
		int Num;
		int NumAlphas;
		int NumNums;
		int IOStat;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool ErrorsFound; // May be set here and passed on
		int CLRBO; // Number ofCooling Load Range Based Operation Inputs
		int HLRBO; // Number ofHeating Load Range Based Operation Inputs
		int DBRBO; // Number ofDry Bulb Temperature Range Based Operation Inputs
		int WBRBO; // Number ofWet Bulb Temperature Range Based Operation Inputs
		int DPRBO; // Number ofDewPoint Temperature Range Based Operation Inputs
		int RHRBO; // Number ofRelative Humidity Range Based Operation Inputs
		int CSPBO; // Number of Component SetPoint Based Operation Inputs
		int DBTDBO; // Number ofDry Bulb Temperature Range Based Operation Inputs
		int WBTDBO; // Number ofWet Bulb Temperature Range Based Operation Inputs
		int DPTDBO; // Number ofDewPoint Temperature Range Based Operation Inputs
		int TESSPBO; // Number of Thermal Energy Storage Setpoint Based Operation Inputs
		int NumSchemes; // Number of Condenser equipment lists
		int NumUncontrolledSchemes; // Number of Condenser equipment lists
		int NumUserDefOpSchemes; // number of user defined EMS op schemes
		int CELists; // Number of Condenser equipment lists
		int PELists; // Number of Plant equipment lists
		int Count; // Loop counter
		int NumSchemeLists;
		int LoopNum;
		std::string CurrentModuleObject; // for ease in renaming.
		Array1D_string TempVerifyNames;

		ErrorsFound = false; //DSU CS

		//**********VERIFY THE 'PLANTEQUIPMENTOPERATION:...' KEYWORDS**********
		CLRBO = GetNumObjectsFound( "PlantEquipmentOperation:CoolingLoad" );
		HLRBO = GetNumObjectsFound( "PlantEquipmentOperation:HeatingLoad" );
		DBRBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorDryBulb" );
		WBRBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorWetBulb" );
		DPRBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorDewpoint" );
		RHRBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorRelativeHumidity" );
		CSPBO = GetNumObjectsFound( "PlantEquipmentOperation:ComponentSetpoint" ); //* Temp Based Control
		NumUserDefOpSchemes = GetNumObjectsFound( "PlantEquipmentOperation:UserDefined" );
		DBTDBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorDryBulbDifference" );
		WBTDBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorWetBulbDifference" );
		DPTDBO = GetNumObjectsFound( "PlantEquipmentOperation:OutdoorDewpointDifference" );
		TESSPBO = GetNumObjectsFound( "PlantEquipmentOperation:ThermalEnergyStorage" );
		NumSchemes = CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO + WBTDBO + DPTDBO + NumUserDefOpSchemes + TESSPBO;
		NumUncontrolledSchemes = GetNumObjectsFound( "PlantEquipmentOperation:Uncontrolled" );
		if ( ( NumSchemes + NumUncontrolledSchemes ) <= 0 ) {
			ShowFatalError( "No PlantEquipmentOperation:* objects specified. Stop simulation." );
		}

		// test for blank or duplicates -- this section just determines if there are any duplicate operation scheme names
		TempVerifyNames.allocate( NumSchemes );

		//Check for existence of duplicates in keyword names
		Count = 0;
		for ( Num = 1; Num <= NumSchemes; ++Num ) {
			if ( CLRBO > 0 && Num <= CLRBO ) {
				CurrentModuleObject = "PlantEquipmentOperation:CoolingLoad";
				Count = Num;
			} else if ( HLRBO > 0 && Num <= ( CLRBO + HLRBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:HeatingLoad";
				Count = Num - CLRBO;
			} else if ( DBRBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorDryBulb";
				Count = Num - CLRBO - HLRBO;
			} else if ( WBRBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorWetBulb";
				Count = Num - CLRBO - HLRBO - DBRBO;
			} else if ( DPRBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorDewpoint";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO;
			} else if ( RHRBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorRelativeHumidity";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO;
			} else if ( CSPBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:ComponentSetpoint";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO;
			} else if ( DBTDBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorDryBulbDifference";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO - CSPBO;
			} else if ( WBTDBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO + WBTDBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorWetBulbDifference";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO - CSPBO - DBTDBO;
			} else if ( DPTDBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO + WBTDBO + DPTDBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:OutdoorDewpointDifference";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO - CSPBO - DBTDBO - WBTDBO;
			} else if ( NumUncontrolledSchemes > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO + WBTDBO + DPTDBO + NumUncontrolledSchemes ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:Uncontrolled";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO - CSPBO - DBTDBO - WBTDBO - DPTDBO;
			} else if ( NumUserDefOpSchemes > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO + WBTDBO + DPTDBO + NumUncontrolledSchemes + NumUserDefOpSchemes ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:UserDefined";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO - CSPBO - DBTDBO - WBTDBO - DPTDBO - NumUncontrolledSchemes;
			} else if ( TESSPBO > 0 && Num <= ( CLRBO + HLRBO + DBRBO + WBRBO + DPRBO + RHRBO + CSPBO + DBTDBO + WBTDBO + DPTDBO + NumUncontrolledSchemes + NumUserDefOpSchemes + TESSPBO ) ) {
				CurrentModuleObject = "PlantEquipmentOperation:ThermalEnergyStorage";
				Count = Num - CLRBO - HLRBO - DBRBO - WBRBO - DPRBO - RHRBO - CSPBO - DBTDBO - WBTDBO - DPTDBO - NumUncontrolledSchemes - NumUserDefOpSchemes;
			} else {
				ShowFatalError( "Error in control scheme identification" );
			}

			GetObjectItem( CurrentModuleObject, Count, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), TempVerifyNames, Num - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				continue;
			}
			TempVerifyNames( Num ) = cAlphaArgs( 1 );

		}

		//**********VERIFY THE 'PlantEquipmentList' AND 'CondenserEquipmentList' KEYWORDS*********
		PELists = GetNumObjectsFound( "PlantEquipmentList" );
		CELists = GetNumObjectsFound( "CondenserEquipmentList" );
		NumSchemeLists = PELists + CELists;
		TempVerifyNames.allocate( NumSchemeLists );
		Count = 0;
		for ( Num = 1; Num <= NumSchemeLists; ++Num ) {
			if ( Num <= PELists ) {
				CurrentModuleObject = "PlantEquipmentList";
				Count = Num;
			} else {
				CurrentModuleObject = "CondenserEquipmentList";
				Count = Num - PELists;
			}
			GetObjectItem( CurrentModuleObject, Count, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), TempVerifyNames, Num - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				continue;
			}
			TempVerifyNames( Num ) = cAlphaArgs( 1 );
		}

		//**********GET INPUT AND LOAD PLANT DATA STRUCTURE*********

		//extend number of equipment lists to include one for each CSPBO
		NumSchemeLists += CSPBO + TESSPBO + NumUserDefOpSchemes;
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			for ( SchemeNum = 1; SchemeNum <= PlantLoop( LoopNum ).NumOpSchemes; ++SchemeNum ) {

				{ auto const plantLoopOperation( PlantLoop( LoopNum ).OpScheme( SchemeNum ).TypeOf );

				if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:COOLINGLOAD" ) {
					CurrentModuleObject = "PlantEquipmentOperation:CoolingLoad";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, CLRBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:HEATINGLOAD" ) {
					CurrentModuleObject = "PlantEquipmentOperation:HeatingLoad";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, HLRBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT" ) { //* Temp Based Control
					CurrentModuleObject = "PlantEquipmentOperation:ComponentSetPoint";
					FindCompSPInput( CurrentModuleObject, CSPBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:USERDEFINED" ) {
					CurrentModuleObject = "PlantEquipmentOperation:UserDefined";
					GetUserDefinedOpSchemeInput( CurrentModuleObject, NumUserDefOpSchemes, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorDryBulb";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, DBRBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORWETBULB" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorWetBulb";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, WBRBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorDewPoint";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, DPRBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorrelativeHumidity";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, RHRBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorDryBulbDifference";
					FindDeltaTempRangeInput( CurrentModuleObject, DBTDBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorWetBulbDifference";
					FindDeltaTempRangeInput( CurrentModuleObject, WBTDBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE" ) {
					CurrentModuleObject = "PlantEquipmentOperation:OutdoorDewPointDifference";
					FindDeltaTempRangeInput( CurrentModuleObject, DPTDBO, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:UNCONTROLLED" ) {
					CurrentModuleObject = "PlantEquipmentOperation:Uncontrolled";
					FindRangeBasedOrUncontrolledInput( CurrentModuleObject, NumUncontrolledSchemes, LoopNum, SchemeNum, ErrorsFound );

				} else if ( plantLoopOperation == "PLANTEQUIPMENTOPERATION:THERMALENERGYSTORAGE" ) { //* Temp Based Control
					CurrentModuleObject = "PlantEquipmentOperation:ThermalEnergyStorage";
					FindCompSPInput( CurrentModuleObject, TESSPBO, LoopNum, SchemeNum, ErrorsFound );

				} else { // invalid op scheme type for plant loop
					// DSU?  Seems like the alpha args below is incorrect....
					ShowSevereError( "Invalid operation scheme type = \"" + cAlphaArgs( Num * 3 - 1 ) + "\", entered in " + CurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}}

				// At this point, switch the thermal energy storage controls to setpoint based controls as all of the
				// internally generated setpoints and schedules have been generated and this can now be handled like
				// the long form setpoint based control.
				if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).TypeOf == "PLANTEQUIPMENTOPERATION:THERMALENERGYSTORAGE" ) {
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).TypeOf =  "PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT";
					}
			}
		}

		// Validate that component names/types in each list correspond to a valid component in input file
		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found getting inputs. Previous error(s) cause program termination." );
		}
	}

	void
	FindRangeBasedOrUncontrolledInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 2010
		//       MODIFIED       Chandan Sharma, August 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Load range based or uncontrolled input into PLANTLOOP data structure

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.
		// The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
		// following keywords is reflected exactly in this subroutine:
		//       PlantEquipmentOperation:CoolingLoad
		//       PlantEquipmentOperation:HeatingLoad
		//       PlantEquipmentOperation:OutdoorDryBulb
		//       PlantEquipmentOperation:OutdoorWetBulb
		//       PlantEquipmentOperation:OutdoorDewPoint
		//       PlantEquipmentOperation:OutdoorRelativeHumidity
		//       PlantEquipmentOperation:Uncontrolled

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const Plant( 1 ); // Used to identify whether the current loop is Plant
		int const Condenser( 2 ); // Used to identify whether the current loop is Condenser

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNums;
		int IOStat;
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//   certain object in the input file
		int Num;
		int NumEquipLists;
		int ListNum;
		std::string LoopOpSchemeObj; // Used to identify the object name for loop equipment operation scheme
		bool SchemeNameFound; // Set to FALSE if a match of OpScheme object and OpScheme name is not found
		int InnerListNum; // inner loop list number
		Real64 OuterListNumLowerLimit;
		Real64 OuterListNumUpperLimit;
		Real64 InnerListNumLowerLimit;
		Real64 InnerListNumUpperLimit;

		SchemeNameFound = true;

		// Determine max number of alpha and numeric arguments for all objects being read, in order to allocate local arrays
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNums );

		AlphArray.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNums );
		NumArray.dimension( NumNums, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNums, true );

		if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
			LoopOpSchemeObj = "PlantEquipmentOperationSchemes";
		} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) {
			LoopOpSchemeObj = "CondenserEquipmentOperationSchemes";
		}

		if ( NumSchemes > 0 ) {
			for ( Num = 1; Num <= NumSchemes; ++Num ) {
				GetObjectItem( CurrentModuleObject, Num, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
				if ( SameString( PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name, AlphArray( 1 ) ) ) break;
				if ( Num == NumSchemes ) {
					ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
					ErrorsFound = true;
					SchemeNameFound = false;
				}
			}
			if ( SchemeNameFound ) {
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists = ( NumAlphas - 1 );
				if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists <= 0 ) {
					ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", specified without equipment list." );
					ErrorsFound = true;
				} else {
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList.allocate( PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists );
					NumEquipLists = PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists;
					if ( NumNums <= 0 ) { // Uncontrolled OpScheme type
						ListNum = NumEquipLists; // NumEquipLists is always 1 for Uncontrolled OpScheme type
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Name = AlphArray( 2 );
						LoadEquipList( LoopNum, SchemeNum, ListNum, ErrorsFound );
					} else { // Range based OpScheme type
						for ( ListNum = 1; ListNum <= NumEquipLists; ++ListNum ) {
							PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit = NumArray( ListNum * 2 - 1 );
							PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeUpperLimit = NumArray( ListNum * 2 );
							PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Name = AlphArray( ListNum + 1 );
							if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeUpperLimit < 0.0 ) {
								ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", found a negative value for an upper limit in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
								ErrorsFound = true;
							}

							{ auto const plantLoopOperation( CurrentModuleObject ); // different op schemes have different lower limit check values

							if ( plantLoopOperation == "PlantEquipmentOperation:CoolingLoad" || plantLoopOperation == "PlantEquipmentOperation:HeatingLoad" || plantLoopOperation == "PlantEquipmentOperation:OutdoorrelativeHumidity" ) {
								// these should not be less than zero
								if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit < 0.0 ) {
									ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", found a negative value for a lower limit in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
									ErrorsFound = true;
								}
							} else {
								// others should not be less than -70
								if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit < -70.0 ) {
									ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", found too low of a value for a lower limit in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
									ErrorsFound = true;
								}
							}}

							if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit > PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeUpperLimit ) {
								ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", found a lower limit that is higher than an upper limit in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
								ErrorsFound = true;
							}

							LoadEquipList( LoopNum, SchemeNum, ListNum, ErrorsFound );
						}
						// now run through lists again and check that range limits do not overlap each other
						for ( ListNum = 1; ListNum <= NumEquipLists; ++ListNum ) {
							OuterListNumLowerLimit = PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit;
							OuterListNumUpperLimit = PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeUpperLimit;
							for ( InnerListNum = 1; InnerListNum <= NumEquipLists; ++InnerListNum ) {
								if ( InnerListNum == ListNum ) continue; // don't check against self.
								InnerListNumLowerLimit = PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( InnerListNum ).RangeLowerLimit;
								InnerListNumUpperLimit = PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( InnerListNum ).RangeUpperLimit;
								// Check if inner list has a lower limit that is between an outer's lower and upper limit
								if ( InnerListNumLowerLimit > OuterListNumLowerLimit && InnerListNumLowerLimit < OuterListNumUpperLimit ) {
									ShowWarningError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", detected overlapping ranges in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
									ShowContinueError( "Range # " + RoundSigDigits( InnerListNum ) + " Lower limit = " + RoundSigDigits( InnerListNumLowerLimit, 1 ) + " lies within the Range # " + RoundSigDigits( ListNum ) + " (" + RoundSigDigits( OuterListNumLowerLimit, 1 ) + " to " + RoundSigDigits( OuterListNumUpperLimit, 1 ) + ")." );
									ShowContinueError( "Check that input for load range limit values do not overlap, and the simulation continues..." );

								}
								// Check if inner list has an upper limit that is between an outer's lower and upper limit
								if ( InnerListNumUpperLimit > OuterListNumLowerLimit && InnerListNumUpperLimit < OuterListNumUpperLimit ) {
									ShowWarningError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", detected overlapping ranges in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
									ShowContinueError( "Range # " + RoundSigDigits( InnerListNum ) + " Upper limit = " + RoundSigDigits( InnerListNumUpperLimit, 1 ) + " lies within Range # " + RoundSigDigits( ListNum ) + " (" + RoundSigDigits( OuterListNumLowerLimit, 1 ) + " to " + RoundSigDigits( OuterListNumUpperLimit, 1 ) + ")." );
									ShowContinueError( "Check that input for load range limit values do not overlap, and the simulation continues..." );

								}
							}
						}

					}
				}
			}
		} else {
			ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
			ErrorsFound = true;
		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		NumArray.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	void
	FindDeltaTempRangeInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Load range based input into PLANTLOOP data structure

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.
		// The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
		// following keywords is reflected exactly in this subroutine:
		//       PlantEquipmentOperation:OutdoorDryBulbDifference
		//       PlantEquipmentOperation:OutdoorWetBulbDifference
		//       PlantEquipmentOperation:OutdoorDewPointDifference

		// REFERENCES:
		// Based on subroutine FindRangeBasedOrUncontrolledInput from Dan Fisher, July 2010

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const Plant( 1 ); // Used to identify whether the current loop is Plant
		int const Condenser( 2 ); // Used to identify whether the current loop is Condenser

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNums;
		int IOStat;
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//   certain object in the input file
		int Num;
		int NumEquipLists;
		int ListNum;
		std::string LoopOpSchemeObj; // Used to identify the object name for loop equipment operation scheme
		bool SchemeNameFound; // Set to FALSE if a match of OpScheme object and OpScheme name is not found

		SchemeNameFound = true;

		// Determine max number of alpha and numeric arguments for all objects being read, in order to allocate local arrays
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNums );

		AlphArray.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNums );
		NumArray.dimension( NumNums, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNums, true );

		if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
			LoopOpSchemeObj = "PlantEquipmentOperationSchemes";
		} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) {
			LoopOpSchemeObj = "CondenserEquipmentOperationSchemes";
		}

		if ( NumSchemes > 0 ) {
			for ( Num = 1; Num <= NumSchemes; ++Num ) {
				GetObjectItem( CurrentModuleObject, Num, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
				if ( SameString( PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name, AlphArray( 1 ) ) ) break;
				if ( Num == NumSchemes ) {
					ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
					ErrorsFound = true;
					SchemeNameFound = false;
				}
			}
			if ( SchemeNameFound ) {
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists = ( NumAlphas - 2 );
				if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists <= 0 ) {
					ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", specified without equipment list." );
					ErrorsFound = true;
				} else {
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList.allocate( PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists );
					NumEquipLists = PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists;
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).ReferenceNodeName = AlphArray( 2 );
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).ReferenceNodeNumber = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
					//For DO Loop below -- Check for lower limit > upper limit.(invalid)
					for ( ListNum = 1; ListNum <= NumEquipLists; ++ListNum ) {
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit = NumArray( ListNum * 2 - 1 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeUpperLimit = NumArray( ListNum * 2 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Name = AlphArray( ListNum + 2 );
						if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeLowerLimit > PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).RangeUpperLimit ) {
							ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", found a lower limit that is higher than an upper limit in " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
							ErrorsFound = true;
						}
						LoadEquipList( LoopNum, SchemeNum, ListNum, ErrorsFound );
					}
				}
			}
		} else {
			ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
			ErrorsFound = true;
		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		NumArray.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	void
	LoadEquipList(
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		int const ListNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 2010
		//       MODIFIED       B. Griffith Sept 2011, major rewrite
		//                      allow mixing list types across plant types, store info first time
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Load delta range based input into PLANTLOOP data structure

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.

		// REFERENCES:
		// na
		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool FoundIntendedList;
		int Num;
		int MachineNum;
		int PELists;
		int CELists;
		//  INTEGER :: NumLists
		int NumAlphas;
		int NumNums;
		int IOStat;
		bool IsNotOK;
		std::string CurrentModuleObject;
		static int TotNumLists( 0 );
		static Array1D_string EquipListsNameList;
		static Array1D_int EquipListsTypeList;
		static Array1D_int EquipListsIndexList;
		int iIndex;
		bool firstblank;

		if ( LoadEquipListOneTimeFlag ) {
			// assemble mapping between list names and indices one time
			PELists = GetNumObjectsFound( "PlantEquipmentList" );
			CELists = GetNumObjectsFound( "CondenserEquipmentList" );
			TotNumLists = PELists + CELists;
			if ( TotNumLists > 0 ) {
				EquipListsNameList.allocate( TotNumLists );
				EquipListsTypeList.allocate( TotNumLists );
				EquipListsIndexList.allocate( TotNumLists );

				//First load PlantEquipmentList info
				if ( PELists > 0 ) {
					CurrentModuleObject = "PlantEquipmentList";
					for ( Num = 1; Num <= PELists; ++Num ) {
						iIndex = Num;
						GetObjectItem( CurrentModuleObject, Num, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
						EquipListsNameList( iIndex ) = cAlphaArgs( 1 );
						EquipListsTypeList( iIndex ) = LoopType_Plant;
						EquipListsIndexList( iIndex ) = Num;
						MachineNum = 2;
						while ( MachineNum <= NumAlphas ) {
							firstblank = false;
							if ( lAlphaFieldBlanks( MachineNum ) || lAlphaFieldBlanks( MachineNum + 1 ) ) {
								if ( lAlphaFieldBlanks( MachineNum ) ) {
									ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid component specification." );
									ShowContinueError( cAlphaFieldNames( MachineNum ) + " is blank." );
									firstblank = true;
									ErrorsFound = true;
								}
								if ( lAlphaFieldBlanks( MachineNum + 1 ) ) {
									if ( ! firstblank ) {
										ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid component specification." );
									}
									ShowContinueError( cAlphaFieldNames( MachineNum + 1 ) + " is blank." );
									ErrorsFound = true;
								}
							} else {
								ValidateComponent( cAlphaArgs( MachineNum ), cAlphaArgs( MachineNum + 1 ), IsNotOK, CurrentModuleObject );
								if ( IsNotOK ) {
									ShowContinueError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Input Error." );
									ErrorsFound = true;
								}
							}
							MachineNum += 2;
						}
					}
				}
				if ( CELists > 0 ) {
					CurrentModuleObject = "CondenserEquipmentList";
					for ( Num = 1; Num <= CELists; ++Num ) {
						iIndex = Num + PELists;
						GetObjectItem( CurrentModuleObject, Num, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
						EquipListsNameList( iIndex ) = cAlphaArgs( 1 );
						EquipListsTypeList( iIndex ) = LoopType_Condenser;
						EquipListsIndexList( iIndex ) = Num;
						MachineNum = 2;
						while ( MachineNum <= NumAlphas ) {
							firstblank = false;
							if ( lAlphaFieldBlanks( MachineNum ) || lAlphaFieldBlanks( MachineNum + 1 ) ) {
								if ( lAlphaFieldBlanks( MachineNum ) ) {
									ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid component specification." );
									ShowContinueError( cAlphaFieldNames( MachineNum ) + " is blank." );
									firstblank = true;
									ErrorsFound = true;
								}
								if ( lAlphaFieldBlanks( MachineNum + 1 ) ) {
									if ( ! firstblank ) {
										ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid component specification." );
									}
									ShowContinueError( cAlphaFieldNames( MachineNum + 1 ) + " is blank." );
									ErrorsFound = true;
								}
							} else {
								ValidateComponent( cAlphaArgs( MachineNum ), cAlphaArgs( MachineNum + 1 ), IsNotOK, CurrentModuleObject );
								if ( IsNotOK ) {
									ShowContinueError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Input Error." );
									ErrorsFound = true;
								}
							}
							MachineNum += 2;
						}
					}
				}
			}
			if ( ErrorsFound ) {
				ShowFatalError( "LoadEquipList/GetEquipmentLists: Failed due to preceding errors." );
			}
			LoadEquipListOneTimeFlag = false;
		}

		FoundIntendedList = false;
		// find name in set of possible list
		for ( Num = 1; Num <= TotNumLists; ++Num ) {
			if ( SameString( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Name, EquipListsNameList( Num ) ) ) {
				FoundIntendedList = true;
				// get object item for real this time
				{ auto const SELECT_CASE_var( EquipListsTypeList( Num ) );
				if ( SELECT_CASE_var == LoopType_Plant ) {
					CurrentModuleObject = "PlantEquipmentList";
				} else if ( SELECT_CASE_var == LoopType_Condenser ) {
					CurrentModuleObject = "CondenserEquipmentList";
				}}
				GetObjectItem( CurrentModuleObject, EquipListsIndexList( Num ), cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).NumComps = ( NumAlphas - 1 ) / 2;
				if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).NumComps > 0 ) {
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Comp.allocate( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).NumComps );
					for ( MachineNum = 1; MachineNum <= PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).NumComps; ++MachineNum ) {
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Comp( MachineNum ).TypeOf = cAlphaArgs( MachineNum * 2 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Comp( MachineNum ).Name = cAlphaArgs( MachineNum * 2 + 1 );
					} //MachineList
				}
			}
		}

		if ( ! FoundIntendedList ) {
			ShowSevereError( "LoadEquipList: Failed to find PlantEquipmentList or CondenserEquipmentList object named = " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( ListNum ).Name );
			ErrorsFound = true;

		}
	}

	void
	FindCompSPInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 2010
		//       MODIFIED       B. Griffith, check setpoint nodes have setpoint managers on EMS on them.
		//                      Rick Strand, Aug 2014, added simple thermal energy storage controls
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Load component setpoint based input into PLANTLOOP data structure

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.
		// The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
		// following keywords is reflected exactly in this subroutine:
		//    PlantEquipmentOperation:ComponentSetPoint
		//    PlantEquipmentOperation:ThermalEnergyStorage

		// REFERENCES:
		// na
		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using namespace DataLoopNode;
		using NodeInputManager::GetOnlySingleNode;
		using namespace DataSizing;
		using namespace DataIPShortCuts;
		using ReportSizingManager::ReportSizingOutput;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::iTemperatureMinSetPoint;
		using EMSManager::iTemperatureMaxSetPoint;
		using ScheduleManager::GetScheduleIndex;
		using SetPointManager::SetUpNewScheduledTESSetPtMgr;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		std::string EquipNum;

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const Plant( 1 ); // Used to identify whether the current loop is Plant
		int const Condenser( 2 ); // Used to identify whether the current loop is Condenser
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNums;
		int CompNum;
		int CompInNode;
		int IOStat;
		Real64 CompFlowRate( 0.0 );
		int Num;
		std::string LoopOpSchemeObj; // Used to identify the object name for loop equipment operation scheme
		bool SchemeNameFound; // Set to FALSE if a match of OpScheme object and OpScheme name is not found
		bool NodeEMSSetPointMissing;
		std::string OnPeakSchedName;
		int OnPeakSchedPtr;
		std::string ChargeSchedName;
		int ChargeSchedPtr;
		Real64 NonChargCHWTemp;
		Real64 OffPeakCHWTemp;
		int CompNumA;
		int CompNumN;
		int CompOpType; // 1=cooling, 2=dual(or other)

		SchemeNameFound = true;

		if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
			LoopOpSchemeObj = "PlantEquipmentOperationSchemes";
		} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) {
			LoopOpSchemeObj = "CondenserEquipmentOperationSchemes";
		}

		if ( NumSchemes > 0 ) {
			for ( Num = 1; Num <= NumSchemes; ++Num ) {
				GetObjectItem( CurrentModuleObject, Num, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
				if ( SameString( PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name, cAlphaArgs( 1 ) ) ) break;
				if ( Num == NumSchemes ) {
					ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
					ErrorsFound = true;
					SchemeNameFound = false;
				}
			}
			if ( SchemeNameFound ) {
				// why only one equip list assumed here? because component setpoint managers have their own lists contained.
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists = 1;
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList.allocate( 1 );
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps = ( NumAlphas - 1 ) / 5;

				if (CurrentModuleObject == "PlantEquipmentOperation:ThermalEnergyStorage" ) {
					// Read all of the additional parameters for ice storage control scheme and error check various parameters
					OnPeakSchedName = cAlphaArgs ( 2 );
					OnPeakSchedPtr  = GetScheduleIndex( OnPeakSchedName );
					if ( OnPeakSchedPtr == 0 ) {
						ShowSevereError( "Could not find On Peak Schedule " + OnPeakSchedName + " in " + CurrentModuleObject + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\".");
						ErrorsFound = true;
					}
					ChargeSchedName = cAlphaArgs ( 3 );
					ChargeSchedPtr  = GetScheduleIndex( ChargeSchedName );
					if ( ChargeSchedPtr == 0 ) {
						ShowSevereError( "Could not find Charging Availability Schedule " + ChargeSchedName + " in " + CurrentModuleObject + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\".");
						ErrorsFound = true;
					}
					NonChargCHWTemp = rNumericArgs ( 1 );
					OffPeakCHWTemp  = rNumericArgs ( 2 );
				}

				if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps > 0 ) {
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp.allocate( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps );
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps; ++CompNum ) {
						if ( CurrentModuleObject == "PlantEquipmentOperation:ComponentSetPoint" ) {
							CompNumA = CompNum * 5;
							CompNumN = CompNum;
						} else if ( CurrentModuleObject == "PlantEquipmentOperation:ThermalEnergyStorage" ) {
							CompNumA = CompNum * 5 + 2;
							CompNumN = CompNum + 2;
						}
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).TypeOf = cAlphaArgs( CompNumA - 3 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).Name = cAlphaArgs( CompNumA - 2 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).DemandNodeName = cAlphaArgs( CompNumA - 1 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).DemandNodeNum = GetOnlySingleNode( cAlphaArgs( CompNumA - 1 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName = cAlphaArgs( CompNumA );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum = GetOnlySingleNode( cAlphaArgs( CompNumA ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointFlowRate = rNumericArgs( CompNumN );

						if ( rNumericArgs( CompNumN ) == AutoSize ) {
							for ( Num = 1; Num <= SaveNumPlantComps; ++Num ) {
								CompInNode = CompDesWaterFlow( Num ).SupNode;
								CompFlowRate = CompDesWaterFlow( Num ).DesVolFlowRate;
								if ( CompInNode == PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).DemandNodeNum ) {
									PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointFlowRate = CompFlowRate;
								} else {
									//call error...Demand node must be component inlet node for autosizing
								}
							}
							gio::write( EquipNum, fmtLD ) << Num;
							ReportSizingOutput( CurrentModuleObject, PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name, "Design Water Flow Rate [m3/s] Equipment # " + stripped( EquipNum ), CompFlowRate );
						}

						{ auto const controlType( cAlphaArgs( CompNumA + 1 ) );
						if ( controlType == "COOLING" ) {
							PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum = CoolingOp;
						} else if ( controlType == "HEATING" ) {
							if ( CurrentModuleObject == "PlantEquipmentOperation:ThermalEnergyStorage" ) {
								ShowSevereError("Equipment Operation Mode cannot be HEATING for any equipment found in " + cAlphaArgs( 1 ) + " in thermal energy storage control" );
								ErrorsFound = true;
							}
							PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum = HeatingOp;
						} else if ( controlType == "DUAL" ) {
							PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum = DualOp;
						}}

						if ( ( cAlphaArgs( CompNumA + 1 ) != "COOLING" ) && ( cAlphaArgs( CompNumA + 1 ) != "HEATING" ) && ( cAlphaArgs( CompNumA + 1 ) != "DUAL" ) ) {
							ShowSevereError( "Equipment Operation Mode should be either HEATING or COOLING or DUAL mode, for " + CurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						}

						if ( CurrentModuleObject == "PlantEquipmentOperation:ThermalEnergyStorage" ) {
							// for each component, a new scheduled setpoint manager needs to be defined to internally generate the more
							// detailed input that is necessary to get thermal energy storage to work from the simpler input.
							CompOpType = ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum ) - 1;
							if ( ( CompOpType < 1 ) || ( CompOpType > 2 ) ) CompOpType = 2;
							SetUpNewScheduledTESSetPtMgr( OnPeakSchedPtr, ChargeSchedPtr, NonChargCHWTemp, OffPeakCHWTemp, CompOpType, PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum );
						}


						//check that setpoint node has valid setpoint managers or EMS
						{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
						if ( SELECT_CASE_var == SingleSetPoint ) {
							if ( Node( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum ).TempSetPoint == SensedNodeFlagValue ) {
								if ( ! AnyEnergyManagementSystemInModel ) {
									ShowSevereError( "Missing temperature setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
									ShowContinueError( "A temperature setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
									if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
										ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=SingleSetpoint" );
									} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
									}
									ShowContinueError( " Use a setpoint manager to place a single temperature setpoint on the node" );
									ErrorsFound = true;
								} else {
									// need call to EMS to check node
									NodeEMSSetPointMissing = false;
									CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum, iTemperatureSetPoint, NodeEMSSetPointMissing );
									if ( NodeEMSSetPointMissing ) {
										ShowSevereError( "Missing temperature setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
										ShowContinueError( "A temperature setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
										if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
											ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=SingleSetpoint" );
										} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
										}
										ShowContinueError( " Use a setpoint manager or EMS actuator to place a single temperature setpoint on node" );
										ErrorsFound = true;
									}
								}
							}
						} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
							if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum == CoolingOp ) {
								if ( Node( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum ).TempSetPointHi == SensedNodeFlagValue ) {
									if ( ! AnyEnergyManagementSystemInModel ) {
										ShowSevereError( "Missing temperature high setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
										ShowContinueError( "A temperature high setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
										if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
											ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=DualSetpointDeadband" );
										} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
										}
										ShowContinueError( " Use a setpoint manager to place a dual temperature setpoint on the node" );
										ErrorsFound = true;
									} else {
										// need call to EMS to check node
										NodeEMSSetPointMissing = false;
										CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum, iTemperatureMaxSetPoint, NodeEMSSetPointMissing );
										if ( NodeEMSSetPointMissing ) {
											ShowSevereError( "Missing high temperature setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
											ShowContinueError( "A high temperature setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
											if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
												ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=DualSetpointDeadband" );
											} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
											}
											ShowContinueError( " Use a setpoint manager or EMS actuator to place a dual or high temperature setpoint on node" );
											ErrorsFound = true;
										}
									}
								}
							} else if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum == HeatingOp ) {
								if ( Node( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum ).TempSetPointLo == SensedNodeFlagValue ) {
									if ( ! AnyEnergyManagementSystemInModel ) {
										ShowSevereError( "Missing temperature low setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
										ShowContinueError( "A temperature low setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
										if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
											ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=DualSetpointDeadband" );
										} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
										}
										ShowContinueError( " Use a setpoint manager to place a dual temperature setpoint on the node" );
										ErrorsFound = true;
									} else {
										// need call to EMS to check node
										NodeEMSSetPointMissing = false;
										CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum, iTemperatureMinSetPoint, NodeEMSSetPointMissing );
										CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum, iTemperatureMaxSetPoint, NodeEMSSetPointMissing );
										if ( NodeEMSSetPointMissing ) {
											ShowSevereError( "Missing low temperature setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
											ShowContinueError( "A low temperature setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
											if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
												ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=DualSetpointDeadband" );
											} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
											}
											ShowContinueError( " Use a setpoint manager or EMS actuator to place a dual or low temperature setpoint on node" );
											ErrorsFound = true;
										}
									}
								}
							} else if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).CtrlTypeNum == DualOp ) {
								if ( ( Node( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum ).TempSetPointHi == SensedNodeFlagValue ) || ( Node( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum ).TempSetPointLo == SensedNodeFlagValue ) ) {
									if ( ! AnyEnergyManagementSystemInModel ) {
										ShowSevereError( "Missing temperature dual setpoints for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
										ShowContinueError( "A dual temperaturesetpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
										if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
											ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=DualSetpointDeadband" );
										} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
										}
										ShowContinueError( " Use a setpoint manager to place a dual temperature setpoint on the node" );
										ErrorsFound = true;
									} else {
										// need call to EMS to check node
										NodeEMSSetPointMissing = false;
										CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeNum, iTemperatureMinSetPoint, NodeEMSSetPointMissing );
										if ( NodeEMSSetPointMissing ) {
											ShowSevereError( "Missing dual temperature setpoint for " + CurrentModuleObject + " named " + cAlphaArgs( 1 ) );
											ShowContinueError( "A dual temperature setpoint is needed at the node named " + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).SetPointNodeName );
											if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
												ShowContinueError( "PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\", Plant Loop Demand Calculation Scheme=DualSetpointDeadband" );
											} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) { // not applicable to Condenser loops
											}
											ShowContinueError( " Use a setpoint manager or EMS actuator to place a dual temperature setpoint on node" );
											ErrorsFound = true;
										}
									}
								}
							}
						}}
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + cAlphaArgs( 1 ) + "\", specified without any machines." );
					ErrorsFound = true;
				}
			}
		} else {
			ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
			ErrorsFound = true;
		}
	}

	void
	GetUserDefinedOpSchemeInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace DataPlant;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;

		using DataRuntimeLanguage::EMSProgramCallManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const Plant( 1 ); // Used to identify whether the current loop is Plant
		int const Condenser( 2 ); // Used to identify whether the current loop is Condenser

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNums;
		int Num;
		int CompNum;
		int IOStat;
		bool SchemeNameFound; // Set to FALSE if a match of OpScheme object and OpScheme name is not found
		std::string LoopOpSchemeObj; // Used to identify the object name for loop equipment operation scheme
		int StackMngrNum; // local temporary for Erl program calling manager index
		static bool lDummy; //Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference

		SchemeNameFound = true;

		if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
			LoopOpSchemeObj = "PlantEquipmentOperationSchemes";
		} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) {
			LoopOpSchemeObj = "CondenserEquipmentOperationSchemes";
		}

		if ( NumSchemes > 0 ) {

			for ( Num = 1; Num <= NumSchemes; ++Num ) {
				GetObjectItem( CurrentModuleObject, Num, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				if ( SameString( PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name, cAlphaArgs( 1 ) ) ) break; //found the correct one
				if ( Num == NumSchemes ) { // did not find it
					ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
					ErrorsFound = true;
					SchemeNameFound = false;
				}
			}
			if ( SchemeNameFound ) {
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).NumEquipLists = 1;
				PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList.allocate( 1 );

				PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps = ( NumAlphas - 3 ) / 2;
				if ( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps > 0 ) {
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp.allocate( PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps );
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).NumComps; ++CompNum ) {
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).TypeOf = cAlphaArgs( CompNum * 2 + 2 );
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).Name = cAlphaArgs( CompNum * 2 + 3 );

						//Setup EMS actuators for machines' MyLoad.
						SetupEMSActuator( "Plant Equipment Operation", PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + ':' + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).Name, "Distributed Load Rate", "[W]", lDummy, PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).EMSActuatorDispatchedLoadValue );
						SetupEMSInternalVariable( "Component Remaining Current Demand Rate", PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + ':' + PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).Name, "[W]", PlantLoop( LoopNum ).OpScheme( SchemeNum ).EquipList( 1 ).Comp( CompNum ).EMSIntVarRemainingLoadValue );
					}
				}
				StackMngrNum = FindItemInList( cAlphaArgs( 2 ), EMSProgramCallManager );
				if ( StackMngrNum > 0 ) { // found it
					PlantLoop( LoopNum ).OpScheme( SchemeNum ).ErlSimProgramMngr = StackMngrNum;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + CurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "EMS Program Manager Name not found." );
					ErrorsFound = true;
				}
				if ( ! lAlphaFieldBlanks( 3 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 3 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						PlantLoop( LoopNum ).OpScheme( SchemeNum ).ErlInitProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + CurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "EMS Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				// setup internal variable for Supply Side Current Demand Rate [W]
				SetupEMSInternalVariable( "Supply Side Current Demand Rate", PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name, "[W]", PlantLoop( LoopNum ).OpScheme( SchemeNum ).EMSIntVarLoopDemandRate );
			}

		} else {
			ShowSevereError( LoopOpSchemeObj + " = \"" + PlantLoop( LoopNum ).OperationScheme + "\", could not find " + CurrentModuleObject + " = \"" + PlantLoop( LoopNum ).OpScheme( SchemeNum ).Name + "\"." );
			ErrorsFound = true;
		}

	}

	// End of GetInput subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Plant Loop Module
	//******************************************************************************

	void
	InitLoadDistribution( bool const FirstHVACIteration )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    July 2010
		//       REVISED:

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine scans equipment lists and matches a particular
		// plant component with a component on the list.  Pointers to the
		// operation scheme and equipment list are saved on the plant data
		// structure to facilitate a new load management routine that calls
		// ManageLoadDistribution for every component.

		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::emsCallFromUserDefinedComponentModel;
		using EMSManager::ManageEMS;
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopPtr;
		int LoopSidePtr;
		int BranchPtr;
		int CompPtr;
		int DummyLoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		int Index;
		int OpSchemePtr;
		int thisSchemeNum;
		int SchemeType;

		bool FoundScheme;
		bool FoundSchemeMatch;
		//  LOGICAL, SAVE                     :: FirstHVACInitsDone = .FALSE.
		//  LOGICAL, SAVE                     :: MyEnvrnFlag = .TRUE.
		int ThisTypeOfNum;
		int CompOpNum;
		int OldNumOpSchemes;
		int NewNumEquipLists;
		int NewNumOpSchemes;
		int NumSearchResults;
		bool GetInputOK; // successful Get Input

		bool errFlag1;
		bool errFlag2;
		Real64 HighestRange;

		// Object Data

		errFlag2 = false;
		//Get Input
		if ( GetPlantOpInput ) {
			GetPlantOperationInput( GetInputOK );
			if ( GetInputOK ) {
				GetOperationSchemeInput();
				GetPlantOpInput = false;
			} else {
				return;
			}
		}

		// ONE TIME INITS
		if ( InitLoadDistributionOneTimeFlag ) {
			// Set up 'component' to 'op scheme' pointers in Plant data structure
			// We're looking for matches between a component on a PlantLoop.OpScheme.List()
			// and the same component in the PlantLoop.LoopSide.Branch.Comp() data structure

			// first loop over main operation scheme data and finish filling out indexes to plant topology for the components in the lists
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				auto & this_plant_loop( PlantLoop( LoopNum ) );
				for ( int OpNum = 1, OpNum_end = this_plant_loop.NumOpSchemes; OpNum <= OpNum_end; ++OpNum ) {
					auto & this_op_scheme( this_plant_loop.OpScheme( OpNum ) );
					for ( int ListNum = 1, ListNum_end = this_op_scheme.NumEquipLists; ListNum <= ListNum_end; ++ListNum ) {
						auto & this_equip_list( this_op_scheme.EquipList( ListNum ) );
						for ( int EquipNum = 1, EquipNum_end = this_equip_list.NumComps; EquipNum <= EquipNum_end; ++EquipNum ) {
							auto & this_equip( this_equip_list.Comp( EquipNum ) );
							ThisTypeOfNum = FindItem( this_equip.TypeOf, SimPlantEquipTypes, NumSimPlantEquipTypes );
							errFlag1 = false;
							ScanPlantLoopsForObject( this_equip.Name, ThisTypeOfNum, DummyLoopNum, LoopSideNum, BranchNum, CompNum, _, _, NumSearchResults, _, LoopNum, errFlag1 );

							if ( errFlag1 ) {
								ShowSevereError( "InitLoadDistribution: Equipment specified for operation scheme not found on correct loop" );
								ShowContinueError( "Operation Scheme name = " + this_op_scheme.Name );
								ShowContinueError( "Loop name = " + this_plant_loop.Name );
								ShowContinueError( "Component name = " + this_equip.Name );
								ShowFatalError( "InitLoadDistribution: Simulation terminated because of error in operation scheme." );
							}

							this_equip.LoopNumPtr = DummyLoopNum;
							this_equip.LoopSideNumPtr = LoopSideNum;
							this_equip.BranchNumPtr = BranchNum;
							this_equip.CompNumPtr = CompNum;

							if ( ValidLoopEquipTypes( ThisTypeOfNum ) == LoopType_Plant && this_plant_loop.TypeOfLoop == LoopType_Condenser ) {
								ShowSevereError( "InitLoadDistribution: CondenserLoop=\"" + this_plant_loop.Name + "\", Operation Scheme=\"" + this_plant_loop.OperationScheme + "\"," );
								ShowContinueError( "Scheme type=" + this_op_scheme.TypeOf + ", Name=\"" + this_op_scheme.Name + "\" includes equipment that is not valid on a Condenser Loop" );
								ShowContinueError( "Component " + ccSimPlantEquipTypes( ThisTypeOfNum ) + " not allowed as supply equipment on this type of loop." );
								ShowContinueError( "Component name = " + this_equip.Name );
								errFlag2 = true;
							}
							if ( ValidLoopEquipTypes( ThisTypeOfNum ) == LoopType_Condenser && this_plant_loop.TypeOfLoop == LoopType_Plant ) {
								ShowSevereError( "InitLoadDistribution: PlantLoop=\"" + this_plant_loop.Name + "\", Operation Scheme=\"" + this_plant_loop.OperationScheme + "\"," );
								ShowContinueError( "Scheme type=" + this_op_scheme.TypeOf + ", Name=\"" + this_op_scheme.Name + "\" includes equipment that is not valid on a Plant Loop" );
								ShowContinueError( "Component " + ccSimPlantEquipTypes( ThisTypeOfNum ) + " not allowed as supply equipment on this type of loop." );
								ShowContinueError( "Component name = " + this_equip.Name );
								errFlag2 = true;
							}

						} // Equipment on List
					} // List
				} // operation scheme
			} // loop

			// second loop, fill op schemes info at each component.
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				auto & this_plant_loop( PlantLoop( LoopNum ) );
				for ( int OpNum = 1, OpNum_end = this_plant_loop.NumOpSchemes; OpNum <= OpNum_end; ++OpNum ) {
					auto & this_op_scheme( this_plant_loop.OpScheme( OpNum ) );
					for ( int ListNum = 1, ListNum_end = this_op_scheme.NumEquipLists; ListNum <= ListNum_end; ++ListNum ) {
						auto & this_equip_list( this_op_scheme.EquipList( ListNum ) );
						for ( int EquipNum = 1, EquipNum_end = this_equip_list.NumComps; EquipNum <= EquipNum_end; ++EquipNum ) {
							auto & this_equip( this_equip_list.Comp( EquipNum ) );
							// dereference indices (stored in previous loop)
							DummyLoopNum = this_equip.LoopNumPtr;
							LoopSideNum = this_equip.LoopSideNumPtr;
							BranchNum = this_equip.BranchNumPtr;
							CompNum = this_equip.CompNumPtr;
							auto & dummy_loop_equip( PlantLoop( DummyLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ) );

							if ( dummy_loop_equip.NumOpSchemes == 0 ) {
								// first op scheme for this component, allocate OpScheme and its EquipList to size 1
								dummy_loop_equip.OpScheme.allocate( 1 );
								auto & dummy_op_scheme_1( dummy_loop_equip.OpScheme( 1 ) );
								dummy_op_scheme_1.EquipList.allocate( 1 );
								dummy_loop_equip.NumOpSchemes = 1;
								dummy_op_scheme_1.NumEquipLists = 1;
								// store pointers
								dummy_op_scheme_1.OpSchemePtr = OpNum;
								dummy_op_scheme_1.EquipList( 1 ).ListPtr = ListNum;
								dummy_op_scheme_1.EquipList( 1 ).CompPtr = EquipNum;
							} else if ( dummy_loop_equip.NumOpSchemes > 0 ) { // already an op scheme
								OldNumOpSchemes = dummy_loop_equip.NumOpSchemes;

								// could be new list on existing scheme or new scheme with new list.  Check and see
								FoundSchemeMatch = false;
								for ( thisSchemeNum = 1; thisSchemeNum <= OldNumOpSchemes; ++thisSchemeNum ) { // Loop index used below
									// compare the OpScheme index, 'opnum', in the PlantLoop()%OpScheme()data structure
									// with the OpSchemePtr in the PlantLoop()%LoopSide()%Branch()%Comp() data structure.
									if ( OpNum != dummy_loop_equip.OpScheme( thisSchemeNum ).OpSchemePtr ) continue;
									FoundSchemeMatch = true;
									break;
								}
								if ( FoundSchemeMatch ) { // op scheme already exists, but need to add a list to the existing OpScheme
									auto & this_op_scheme( dummy_loop_equip.OpScheme( thisSchemeNum ) );
									NewNumEquipLists = this_op_scheme.NumEquipLists + 1;
									this_op_scheme.EquipList.redimension( NewNumEquipLists );
									this_op_scheme.NumEquipLists = NewNumEquipLists;
									this_op_scheme.EquipList( NewNumEquipLists ).ListPtr = ListNum;
									this_op_scheme.EquipList( NewNumEquipLists ).CompPtr = EquipNum;
								} else { // !FoundSchemeMatch: Add new op scheme and a new list
									NewNumOpSchemes = OldNumOpSchemes + 1;
									dummy_loop_equip.OpScheme.redimension( NewNumOpSchemes );
									auto & new_op_scheme( dummy_loop_equip.OpScheme( NewNumOpSchemes ) );
									new_op_scheme.EquipList.allocate( 1 );
									dummy_loop_equip.NumOpSchemes = NewNumOpSchemes;
									new_op_scheme.NumEquipLists = 1;
									new_op_scheme.OpSchemePtr = OpNum;
									new_op_scheme.EquipList( 1 ).ListPtr = ListNum;
									new_op_scheme.EquipList( 1 ).CompPtr = EquipNum;
								}
							}

						} // Equipment on List
					} // List
				} // operation scheme
			} // loop

			// check the pointers to see if a single component is attached to more than one type of control scheme
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				auto const & this_plant_loop( PlantLoop( LoopNum ) );
				for ( int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
					auto const & this_loop_side( this_plant_loop.LoopSide( LoopSideNum ) );
					for ( int BranchNum = 1, BranchNum_end = this_loop_side.TotalBranches; BranchNum <= BranchNum_end; ++BranchNum ) {
						auto const & this_branch( this_loop_side.Branch( BranchNum ) );
						for ( int CompNum = 1, CompNum_end = this_branch.TotalComponents; CompNum <= CompNum_end; ++CompNum ) {
							auto const & this_component( this_branch.Comp( CompNum ) );
							if ( allocated( this_component.OpScheme ) ) {
								for ( Index = 1; Index <= this_component.NumOpSchemes; ++Index ) {
									OpSchemePtr = this_component.OpScheme( Index ).OpSchemePtr;
									if ( OpSchemePtr == 0 ) {
										ShowSevereError( "InitLoadDistribution: no operation scheme index found for component on PlantLoop=" + this_plant_loop.Name );
										ShowContinueError( "Component name = " + this_component.Name );
										errFlag2 = true;
									}
									if ( Index == 1 ) {
										SchemeType = this_plant_loop.OpScheme( OpSchemePtr ).OpSchemeType;
									} else {
										if ( SchemeType != this_plant_loop.OpScheme( OpSchemePtr ).OpSchemeType ) {
											// CALL FATAL ERROR 'component may not be specified on two types of operation schemes
											// DSU?  BG do not understand.  Cannot different op schemes be in effect at different times?
											//  I thought this would be allowed??
										}
									}
								}
							}
						}
					}
				}
			}

			// fill out information on which equipment list is the "last" meaning it has the highest upper limit for load range
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				auto & this_plant_loop( PlantLoop( LoopNum ) );
				for ( int OpNum = 1, OpNum_end = this_plant_loop.NumOpSchemes; OpNum <= OpNum_end; ++OpNum ) {
					auto & this_op_scheme( this_plant_loop.OpScheme( OpNum ) );
					// skip non-load based op schemes
					if ( ( this_op_scheme.OpSchemeType != HeatingRBOpSchemeType ) && ( this_op_scheme.OpSchemeType != CoolingRBOpSchemeType ) ) continue;
					HighestRange = 0.0;
					for ( int ListNum = 1, ListNum_end = this_op_scheme.NumEquipLists; ListNum <= ListNum_end; ++ListNum ) {
						HighestRange = max( HighestRange, this_op_scheme.EquipList( ListNum ).RangeUpperLimit );
					} // List
					for ( int ListNum = 1, ListNum_end = this_op_scheme.NumEquipLists; ListNum <= ListNum_end; ++ListNum ) {
						if ( HighestRange == this_op_scheme.EquipList( ListNum ).RangeUpperLimit ) {
							this_op_scheme.EquipListNumForLastStage = ListNum;
						}
					}
				} // operation scheme
			} // loop

			InitLoadDistributionOneTimeFlag = false;
		}

		if ( AnyEMSPlantOpSchemesInModel ) { // Execute any Initialization EMS program calling managers for User-Defined operation.
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				auto & this_plant_loop( PlantLoop( LoopNum ) );
				for ( int OpNum = 1, OpNum_end = this_plant_loop.NumOpSchemes; OpNum <= OpNum_end; ++OpNum ) {
					auto & this_op_scheme( this_plant_loop.OpScheme( OpNum ) );
					if ( this_op_scheme.OpSchemeType == EMSOpSchemeType ) {
						if ( BeginEnvrnFlag && this_op_scheme.MyEnvrnFlag ) {
							if ( this_op_scheme.ErlInitProgramMngr > 0 ) {
								ManageEMS( emsCallFromUserDefinedComponentModel, this_op_scheme.ErlInitProgramMngr );
							}
							this_op_scheme.MyEnvrnFlag = false;
						}
						if ( ! BeginEnvrnFlag ) this_op_scheme.MyEnvrnFlag = true;
					}
				} // operation scheme
			} // loop
		}

		// FIRST HVAC INITS
		if ( FirstHVACIteration ) {
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				auto & this_plant_loop( PlantLoop( LoopNum ) );
				for ( int LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
					auto & this_loop_side( this_plant_loop.LoopSide( LoopSideNum ) );
					for ( int BranchNum = 1, BranchNum_end = this_loop_side.TotalBranches; BranchNum <= BranchNum_end; ++BranchNum ) {
						auto & this_branch( this_loop_side.Branch( BranchNum ) );
						for ( int CompNum = 1, CompNum_end = this_branch.TotalComponents; CompNum <= CompNum_end; ++CompNum ) {
							auto & this_component( this_branch.Comp( CompNum ) );
							// initalize components 'ON-AVAILABLE-NO LOAD-NO EMS CTRL'
							this_component.ON = true;
							this_component.Available = true;
							this_component.MyLoad = 0.0;
							this_component.EMSLoadOverrideOn = false;
							// Zero out the old curOpSchemePtr so that we don't get 'carry-over' when we update schedules
							if ( this_component.CurOpSchemeType != DemandOpSchemeType && this_component.CurOpSchemeType != PumpOpSchemeType && this_component.CurOpSchemeType != WSEconOpSchemeType && this_component.CurOpSchemeType != NoControlOpSchemeType ) {
								this_component.CurOpSchemeType = NoControlOpSchemeType;
							}
							this_component.CurCompLevelOpNum = 0;
						}
					}
				}
			}
			// Update the OpScheme schedules
			for ( int LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				FoundScheme = false;
				auto & this_loop( PlantLoop( LoopNum ) );
				for ( int OpNum = 1; OpNum <= this_loop.NumOpSchemes; ++OpNum ) {
					auto & this_op_scheme( this_loop.OpScheme( OpNum ) );
					if ( GetCurrentScheduleValue( this_op_scheme.SchedPtr ) > 0.0 ) {
						this_op_scheme.Available = true;
						FoundScheme = true;
						for ( int ListNum = 1, ListNum_end = this_op_scheme.NumEquipLists; ListNum <= ListNum_end; ++ListNum ) {
							auto & this_equip_list( this_op_scheme.EquipList( ListNum ) );
							// The component loop loads the pointers from the OpScheme data structure
							// If the component happens to be active in more than schedule, the *LAST*
							// schedule found will be activated
							for ( int CompNum = 1; CompNum <= this_equip_list.NumComps; ++CompNum ) {

								// set up a reference to the component instance on the list data structure
								auto const & this_list_component( this_equip_list.Comp( CompNum ) );

								// then look up the component topological position from this structure
								LoopPtr = this_list_component.LoopNumPtr;
								LoopSidePtr = this_list_component.LoopSideNumPtr;
								BranchPtr = this_list_component.BranchNumPtr;
								CompPtr = this_list_component.CompNumPtr;

								// then set up a reference to the component on the plant data structure
								auto & this_loop_component( PlantLoop( LoopPtr ).LoopSide( LoopSidePtr ).Branch( BranchPtr ).Comp( CompPtr ) );

								if ( this_loop_component.CurOpSchemeType != PumpOpSchemeType ) {
									this_loop_component.CurOpSchemeType = this_op_scheme.OpSchemeType;
								} else {
									ShowSevereError( "Invalid [pump] component found on equipment list.  Pumps are not allowed on equipment lists." );
									ShowContinueError( "Problem component name = " + this_op_scheme.EquipList( ListNum ).Comp( CompNum ).Name );
									ShowContinueError( "Remove pump component and place other plant equipment on the list to correct." );
									errFlag2 = true;
								}

								for ( CompOpNum = 1; CompOpNum <= this_loop_component.NumOpSchemes; ++CompOpNum ) {
									if ( this_loop_component.OpScheme( CompOpNum ).OpSchemePtr == OpNum ) {
										this_loop_component.CurCompLevelOpNum = CompOpNum;
									}
								}
							}
						}
					} else {
						this_op_scheme.Available = false;
					}

				}
				//    IF(.NOT. foundscheme)THEN
				//      !'call warning 'no current control scheme specified.  Loop Equipment will be shut down'
				//    ENDIF
			}

		}

		if ( errFlag2 ) {
			ShowFatalError( "InitLoadDistribution: Fatal error caused by previous severe error(s)." );
		}

	}

	// End Initialization Section of the Plant Loop Module
	//******************************************************************************

	// Begin Load Calculation/Distribution Section of the Plant Loop Module
	//******************************************************************************

	void
	DistributePlantLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const CurSchemePtr, // use as index in PlantLoop()OpScheme() data structure
		int const ListPtr, // use as index in PlantLoop()OpScheme() data structure
		Real64 const LoopDemand,
		Real64 & RemLoopDemand
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  July 2010
		//                      Sept 2010 B. Griffith, retain actual sign of load values
		//						July 2014 M. Mitchell, added SequentialUniformPLR and UniformPLR schemes

		// PURPOSE OF THIS SUBROUTINE: This subroutine distributes the load
		// to plant equipment according to one of two distribution schemes:
		//     OPTIMAL    = 1
		//     SEQUENTIALLOAD = 2
		//     UNIFORMLOAD  = 3
		//     UNIFORMPLR = 4
		//     SEQUENTIALUNIFORMPLR = 5
		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// Using/Aliasing
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ChangeInLoad;
		Real64 DivideLoad;
		Real64 UniformLoad;
		Real64 NewLoad;
		Real64 PlantCapacity;
		Real64 MinCompPLR;
		Real64 LargestMinCompPLR;
		Real64 PlantPLR;
		Real64 CompLoad;
		int LoadFlag;

		int BranchNum;
		int CompNum;
		int CompIndex;
		int NumCompsOnList;

		// start with some references
		auto & this_loop( PlantLoop( LoopNum ) );
		auto & this_loopside( this_loop.LoopSide( LoopSideNum ) );
		auto & this_equiplist( this_loop.OpScheme( CurSchemePtr ).EquipList( ListPtr ) );

		struct LoadPLRPoint {
			Real64 plant_capacity_to_this_point;
			Real64 largest_min_plr_to_this_point;
			LoadPLRPoint( Real64 capacity, Real64 plr ) {
				plant_capacity_to_this_point = capacity;
				largest_min_plr_to_this_point = plr;
			}
		};
		std::vector< LoadPLRPoint > accrued_load_plr_values;

		// load local variables
		NumCompsOnList = this_equiplist.NumComps;

		//Allocate array once
		accrued_load_plr_values.reserve( NumCompsOnList );
		RemLoopDemand = LoopDemand;
		if ( NumCompsOnList <= 0 ) return;
		//set flag to specify optimal or sequential loading of equipment
		LoadFlag = this_loop.LoadDistribution;

		if ( std::abs( RemLoopDemand ) < SmallLoad ) {
			//no load to distribute
		} else {

			//OPTIMAL DISTRIBUTION SCHEME
			switch ( LoadFlag ) {
			case OptimalLoading:
				//step 1: load all machines to optimal PLR
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					// look up topology from the equipment list
					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;

					if ( this_component.OptLoad > 0.0 ) {
						ChangeInLoad = min( this_component.OptLoad, std::abs( RemLoopDemand ) );
					} else {
						// this is for some components like cooling towers don't have well defined OptLoad
						ChangeInLoad = std::abs( RemLoopDemand );
					}

					AdjustChangeInLoadForLastStageUpperRangeLimit( LoopNum, CurSchemePtr, ListPtr, ChangeInLoad );

					AdjustChangeInLoadByEMSControls( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					AdjustChangeInLoadByHowServed( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					ChangeInLoad = max( 0.0, ChangeInLoad );
					this_component.MyLoad = sign( ChangeInLoad, RemLoopDemand );

					RemLoopDemand -= this_component.MyLoad;
					if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0; //CR8631 don't just exit or %MyLoad on second device isn't reset
				}

				//step 2: Evenly distribute remaining loop demand
				if ( std::abs( RemLoopDemand ) > SmallLoad ) {
					DivideLoad = std::abs( RemLoopDemand ) / NumCompsOnList;
					for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

						BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
						CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

						// create a reference to the component itself
						auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

						if ( ! this_component.Available ) continue;

						NewLoad = this_component.MyLoad;
						NewLoad = min( this_component.MaxLoad, std::abs( NewLoad ) + DivideLoad );
						ChangeInLoad = NewLoad - std::abs( this_component.MyLoad );
						this_component.MyLoad = sign( NewLoad, RemLoopDemand );
						RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );
						if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0; //CR8631 don't just exit or %MyLoad on second device isn't reset
					}
				}

				// step 3: If RemLoopDemand is still greater than zero, look for any machine
				if ( std::abs( RemLoopDemand ) > SmallLoad ) {
					for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

						BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
						CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

						// create a reference to the component itself
						auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

						if ( ! this_component.Available ) continue;
						DivideLoad = this_component.MaxLoad - std::abs( this_component.MyLoad );
						ChangeInLoad = min( std::abs( RemLoopDemand ), DivideLoad );
						this_component.MyLoad += sign( ChangeInLoad, RemLoopDemand );
						RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );
						if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0; //CR8631 don't just exit or %MyLoad on second device isn't reset
					}
				}

				break;

			//SEQUENTIALLOAD DISTRIBUTION SCHEME
			case SequentialLoading:

				// step 1: Load machines in list order
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;

					if ( this_component.MaxLoad > 0.0 ) { // apply known limit
						ChangeInLoad = min( this_component.MaxLoad, std::abs( RemLoopDemand ) );
					} else {
						// this is for some components like cooling towers don't have well defined MaxLoad
						ChangeInLoad = std::abs( RemLoopDemand );
					}

					AdjustChangeInLoadForLastStageUpperRangeLimit( LoopNum, CurSchemePtr, ListPtr, ChangeInLoad );

					AdjustChangeInLoadByEMSControls( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					AdjustChangeInLoadByHowServed( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					ChangeInLoad = max( 0.0, ChangeInLoad );
					this_component.MyLoad = sign( ChangeInLoad, RemLoopDemand );
					RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );
					if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0; //CR8631 don't just exit or %MyLoad on second device isn't reset
				}

				break;

			//UNIFORMLOAD DISTRIBUTION SCHEME
			case UniformLoading:

				// step 1: distribute load equally to all machines
				UniformLoad = std::abs( RemLoopDemand ) / NumCompsOnList;
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;
					if ( this_component.MaxLoad > 0.0 ) {
						ChangeInLoad = min( this_component.MaxLoad, UniformLoad );
					} else {
						// this is for some components like cooling towers don't have well defined MaxLoad
						ChangeInLoad = std::abs( RemLoopDemand );
					}

					AdjustChangeInLoadForLastStageUpperRangeLimit( LoopNum, CurSchemePtr, ListPtr, ChangeInLoad );

					AdjustChangeInLoadByEMSControls( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					AdjustChangeInLoadByHowServed( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );
					ChangeInLoad = max( 0.0, ChangeInLoad );
					this_component.MyLoad = sign( ChangeInLoad, RemLoopDemand );
					RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );
					if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0;
				}

				// step 2: If RemLoopDemand is not zero, then distribute remainder sequentially.
				if ( std::abs( RemLoopDemand ) > SmallLoad ) {
					for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

						BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
						CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

						// create a reference to the component itself
						auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

						if ( ! this_component.Available ) continue;
						ChangeInLoad = min( this_component.MaxLoad - std::abs( this_component.MyLoad ), std::abs( RemLoopDemand ) );
						ChangeInLoad = max( 0.0, ChangeInLoad );
						this_component.MyLoad += sign( ChangeInLoad, RemLoopDemand );
						RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );
						if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0;
					}
				}

				break;

			// UNIFORMPLR LOAD DISTRIBUTION SCHEME
			case UniformPLRLoading:
				// Get total plant capacity and remove last component from list if load is less
				// than plant capacity at min PLR
				PlantCapacity = 0.0;
				PlantPLR = 0.0;
				MinCompPLR = 0.0;
				LargestMinCompPLR = 0.0;

				// Determine PlantCapacity and LargestMinCompPLR
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;

					PlantCapacity += this_component.MaxLoad;

					if ( this_component.MaxLoad < SmallLoad ) {
						ShowWarningMessage( "Plant component " + this_component.Name + " has zero available capacity. Check component controls." );
						MinCompPLR = 0.0;
					} else {
						MinCompPLR = this_component.MinLoad/this_component.MaxLoad;
					}

					//Set LargestMinCompPLR to largest MinCompPLR
					LargestMinCompPLR = max( LargestMinCompPLR, MinCompPLR );

					//Update the array
					accrued_load_plr_values.push_back( LoadPLRPoint( PlantCapacity, LargestMinCompPLR ) );
				}

				// work backwards from full capacity down to 1 unit on
				for ( int i = accrued_load_plr_values.size() - 1; i >= 0; --i ) {

					// if i == 0 then we need to take that as the resulting value
					if ( i == 0 ) {
						PlantCapacity = accrued_load_plr_values[i].plant_capacity_to_this_point;
						LargestMinCompPLR = accrued_load_plr_values[i].largest_min_plr_to_this_point;
						break;

					// if the capacity is greater than the demand, just store the latest values and continue
					} else if ( std::abs( RemLoopDemand ) < ( accrued_load_plr_values[i].largest_min_plr_to_this_point * accrued_load_plr_values[i].plant_capacity_to_this_point ) ) {
						PlantCapacity = accrued_load_plr_values[i].plant_capacity_to_this_point;
						LargestMinCompPLR = accrued_load_plr_values[i].largest_min_plr_to_this_point;
						continue;

					// if the capacity is less than the demand, accept the last values from the previous iteration and exit
					} else {
						break;

					}
				}

				// Determine PLR for uniform PLR loading of all equipment
				if ( PlantCapacity > 0.0 ) {
					PlantPLR = min( 1.0, std::abs( RemLoopDemand ) / PlantCapacity );
				} else {
					ShowWarningError( "Zero available plant capacity for Plant Loop = " + PlantLoop( LoopNum ).Name );
				}

				// Distribute load to each machine
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					CompLoad = 0.0;

					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;

					CompLoad = PlantPLR * this_component.MaxLoad;

					if ( this_component.MaxLoad > 0.0 ) {
						ChangeInLoad = min( std::abs( RemLoopDemand ), CompLoad );
					} else {
						// this is for some components like cooling towers don't have well defined MaxLoad
						ChangeInLoad = std::abs( RemLoopDemand );
					}

					AdjustChangeInLoadForLastStageUpperRangeLimit( LoopNum, CurSchemePtr, ListPtr, ChangeInLoad );

					AdjustChangeInLoadByEMSControls( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					AdjustChangeInLoadByHowServed( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					ChangeInLoad = max( 0.0, ChangeInLoad );

					this_component.MyLoad = sign( ChangeInLoad, RemLoopDemand );

					RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );

					if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0;
				}

				break;

			// SEQUENTIALUNIFORMPLR LOAD DISTRIBUTION SCHEME
			case SequentialUniformPLRLoading:

				PlantCapacity = 0.0;
				PlantPLR = 0.0;
				MinCompPLR = 0.0;
				LargestMinCompPLR = 0.0;

				// Determine PlantCapacity and LargestMinCompPLR
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;

					PlantCapacity += this_component.MaxLoad;

					if ( this_component.MaxLoad < SmallLoad ) {
						ShowWarningMessage( "Plant component " + this_component.Name + " has zero available capacity. Check component controls." );
						MinCompPLR = 0.0;
					} else {
						MinCompPLR = this_component.MinLoad/this_component.MaxLoad;
					}

					//Set LargestMinCompPLR to largest MinCompPLR
					if ( MinCompPLR > LargestMinCompPLR ) LargestMinCompPLR = MinCompPLR;

					if ( std::abs( RemLoopDemand ) <= PlantCapacity ) {
						break;
					}
				}

				// Determine PLR for uniform PLR loading of all equipment
				if ( PlantCapacity > 0.0 ) {
					PlantPLR = min( 1.0, std::abs( RemLoopDemand ) / PlantCapacity );
				} else {
					ShowWarningError( "Zero available plant capacity for Plant Loop = " + PlantLoop( LoopNum ).Name );
				}

				// Distribute load to each machine
				for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

					CompLoad = 0.0;

					BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
					CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

					// create a reference to the component itself
					auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

					if ( ! this_component.Available ) continue;

					CompLoad = PlantPLR * this_component.MaxLoad;

					if ( this_component.MaxLoad > 0.0 ) {
						ChangeInLoad = min( std::abs( RemLoopDemand ), CompLoad );
					} else {
						// this is for some components like cooling towers don't have well defined MaxLoad
						ChangeInLoad = std::abs( RemLoopDemand );
					}

					AdjustChangeInLoadForLastStageUpperRangeLimit( LoopNum, CurSchemePtr, ListPtr, ChangeInLoad );

					AdjustChangeInLoadByEMSControls( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					AdjustChangeInLoadByHowServed( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad );

					ChangeInLoad = max( 0.0, ChangeInLoad );

					this_component.MyLoad = sign( ChangeInLoad, RemLoopDemand );

					RemLoopDemand -= sign( ChangeInLoad, RemLoopDemand );

					if ( std::abs( RemLoopDemand ) < SmallLoad ) RemLoopDemand = 0.0;
				}

			}

		} // load is small check

		// now update On flags according to result for MyLoad
		for ( CompIndex = 1; CompIndex <= NumCompsOnList; ++CompIndex ) {

			BranchNum = this_equiplist.Comp( CompIndex ).BranchNumPtr;
			CompNum = this_equiplist.Comp( CompIndex ).CompNumPtr;

			// create a reference to the component itself
			auto & this_component( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

			if ( std::abs( this_component.MyLoad ) < SmallLoad ) {
				this_component.ON = false;
			} else {
				this_component.ON = true;
			}

		}

	}

	void
	AdjustChangeInLoadForLastStageUpperRangeLimit(
		int const LoopNum, // component topology
		int const CurOpSchemePtr, // currect active operation scheme
		int const CurEquipListPtr, // current equipment list
		Real64 & ChangeInLoad // positive magnitude of load change
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   May 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// if this is the last stage for a load based operation, then limit load to upper range

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RangeHiLimit;

		if ( PlantLoop( LoopNum ).OpScheme( CurOpSchemePtr ).EquipListNumForLastStage == CurEquipListPtr ) { // at final last stage

			RangeHiLimit = PlantLoop( LoopNum ).OpScheme( CurOpSchemePtr ).EquipList( CurEquipListPtr ).RangeUpperLimit;
			ChangeInLoad = min( ChangeInLoad, RangeHiLimit );
		}

	}

	void
	AdjustChangeInLoadByHowServed(
		int const LoopNum, // component topology
		int const LoopSideNum, // component topology
		int const BranchNum, // component topology
		int const CompNum, // component topology
		Real64 & ChangeInLoad // positive magnitude of load change
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov 2011
		//       MODIFIED       March 2012, B. Griffith add controls for free cooling heat exchanger overrides of chillers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// central place to apply limits to machine load dispatch based on how the machine serves loads

		// METHODOLOGY EMPLOYED:
		// Components are machines on plant equipment operation lists.  Need to make adjustments to the
		// load dispatch to account for limits and floating capacities.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE EconomizerHeatExchanger,  ONLY: GetEconHeatExchangerCurrentCapacity
		// Using/Aliasing
		using DataLoopNode::Node;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutWetBulbTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PlantCondLoopOperation:DistributePlantLoad" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 CurMassFlowRate( 0.0 );
		static Real64 ToutLowLimit( 0.0 );
		static Real64 ToutHiLimit( 0.0 );
		static Real64 TinLowLimit( 0.0 );
		static Real64 Tinlet( 0.0 );
		static Real64 Tsensor( 0.0 );
		static Real64 CurSpecHeat( 0.0 );
		static Real64 QdotTmp( 0.0 );
		static int ControlNodeNum( 0 );

		auto & this_component( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ) );

		//start of bad band-aid, need a general and comprehensive approach for determining current capacity of all kinds of equipment
		// Need to truncate the load down in case outlet temperature will hit a lower/upper limit
		{ auto const SELECT_CASE_var( this_component.HowLoadServed );

		//Chillers
		if ( SELECT_CASE_var == HowMet_ByNominalCapLowOutLimit ) { // chillers with lower limit on outlet temperature

			//- Retrieve data from the plant loop data structure
			CurMassFlowRate = Node( this_component.NodeNumIn ).MassFlowRate;
			ToutLowLimit = this_component.MinOutletTemp;
			Tinlet = Node( this_component.NodeNumIn ).Temp;
			CurSpecHeat = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, Tinlet, PlantLoop( LoopNum ).FluidIndex, RoutineName );
			QdotTmp = CurMassFlowRate * CurSpecHeat * ( Tinlet - ToutLowLimit );

			//        !- Don't correct if Q is zero, as this could indicate a component which this hasn't been implemented or not yet turned on
			if ( CurMassFlowRate > 0.0 ) {
				ChangeInLoad = min( ChangeInLoad, QdotTmp );
			}

		} else if ( SELECT_CASE_var == HowMet_ByNominalCapFreeCoolCntrl ) {
			// for chillers with free cooling shutdown (HeatExchanger:Hydronic currently)
			// determine if free cooling controls shut off chiller
			TinLowLimit = this_component.FreeCoolCntrlMinCntrlTemp;
			{ auto const SELECT_CASE_var1( this_component.FreeCoolCntrlMode );
			if ( SELECT_CASE_var1 == FreeCoolControlMode_WetBulb ) {
				Tsensor = OutWetBulbTemp;
			} else if ( SELECT_CASE_var1 == FreeCoolControlMode_DryBulb ) {
				Tsensor = OutDryBulbTemp;
			} else if ( SELECT_CASE_var1 == FreeCoolControlMode_Loop ) {
				ControlNodeNum = this_component.FreeCoolCntrlNodeNum;
				if ( ControlNodeNum > 0 ) {
					Tsensor = Node( ControlNodeNum ).TempLastTimestep; // use lagged value for stability
				} else {
					Tsensor = 23.0;
				}
			}}

			if ( Tsensor < TinLowLimit ) { // turn off chiller to initiate free cooling
				ChangeInLoad = 0.0;
				this_component.Available = false;
				this_component.FreeCoolCntrlShutDown = true;
			} else {
				this_component.Available = true;
				this_component.FreeCoolCntrlShutDown = false;
			}

		} else if ( SELECT_CASE_var == HowMet_ByNominalCapLowOutLimitFreeCoolCntrl ) {
			// for chillers with free cooling shutdown (HeatExchanger:Hydronic currently)
			// determine if free cooling controls shut off chiller
			TinLowLimit = this_component.FreeCoolCntrlMinCntrlTemp;
			{ auto const SELECT_CASE_var1( this_component.FreeCoolCntrlMode );
			if ( SELECT_CASE_var1 == FreeCoolControlMode_WetBulb ) {
				Tsensor = OutWetBulbTemp;
			} else if ( SELECT_CASE_var1 == FreeCoolControlMode_DryBulb ) {
				Tsensor = OutDryBulbTemp;
			} else if ( SELECT_CASE_var1 == FreeCoolControlMode_Loop ) {
				ControlNodeNum = this_component.FreeCoolCntrlNodeNum;
				if ( ControlNodeNum > 0 ) {
					Tsensor = Node( ControlNodeNum ).TempLastTimestep; // use lagged value for stability
				} else {
					Tsensor = 23.0;
				}
			}}

			if ( Tsensor < TinLowLimit ) { // turn off chiller to initiate free cooling
				ChangeInLoad = 0.0;
				this_component.Available = false;
				this_component.FreeCoolCntrlShutDown = true;
			} else {
				//- Retrieve data from the plant loop data structure
				this_component.Available = true;
				this_component.FreeCoolCntrlShutDown = false;
				CurMassFlowRate = Node( this_component.NodeNumIn ).MassFlowRate;
				ToutLowLimit = this_component.MinOutletTemp;
				Tinlet = Node( this_component.NodeNumIn ).Temp;
				CurSpecHeat = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, Tinlet, PlantLoop( LoopNum ).FluidIndex, RoutineName );
				QdotTmp = CurMassFlowRate * CurSpecHeat * ( Tinlet - ToutLowLimit );

				//        !- Don't correct if Q is zero, as this could indicate a component which this hasn't been implemented or not yet turned on
				if ( CurMassFlowRate > 0.0 ) {
					ChangeInLoad = min( ChangeInLoad, QdotTmp );
				}
			}

		} else if ( SELECT_CASE_var == HowMet_ByNominalCapHiOutLimit ) { // boilers with upper limit on outlet temperature
			//- Retrieve data from the plant loop data structure
			CurMassFlowRate = Node( this_component.NodeNumIn ).MassFlowRate;
			ToutHiLimit = this_component.MaxOutletTemp;
			Tinlet = Node( this_component.NodeNumIn ).Temp;
			CurSpecHeat = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, Tinlet, PlantLoop( LoopNum ).FluidIndex, RoutineName );
			QdotTmp = CurMassFlowRate * CurSpecHeat * ( ToutHiLimit - Tinlet );

			if ( CurMassFlowRate > 0.0 ) {
				ChangeInLoad = min( ChangeInLoad, QdotTmp );
			}

		} else if ( SELECT_CASE_var == HowMet_PassiveCap ) { // need to estimate current capacity if more or less passive devices ??

		} else {

		}}

	}

	void
	FindCompSPLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		int const OpNum // index for Plant()%LoopSide()%Branch()%Comp()%OpScheme()
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   Jan 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  Dan Fisher July 2010

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the load on a component controlled by
		// Component SetPoint based scheme.

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataLoopNode::SensedNodeFlagValue;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "FindCompSPLoad" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CompDemand;
		Real64 DemandMdot;
		Real64 ActualMdot;
		Real64 TempIn;
		Real64 CurSpecHeat;
		Real64 TempSetPt( 0.0 );
		Real64 CompMinLoad;
		Real64 CompMaxLoad;
		Real64 CompOptLoad;
		int DemandNode;
		int CompPtr;
		int OpSchemePtr;
		int ListPtr;
		int SetPtNode;
		int NumEquipLists;
		Real64 rho;
		Real64 CurrentDemandForCoolingOp;
		Real64 CurrentDemandForHeatingOp;

		auto & this_component( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ) );

		//find the pointer to the 'PlantLoop()%OpScheme()'...data structure
		NumEquipLists = this_component.OpScheme( OpNum ).NumEquipLists;
		if ( NumEquipLists != 1 ) {
			//CALL Severe error) there should be exactly one list associated with component setpoint scheme
		}

		OpSchemePtr = this_component.OpScheme( OpNum ).OpSchemePtr;
		ListPtr = this_component.OpScheme( OpNum ).EquipList( 1 ).ListPtr;
		CompPtr = this_component.OpScheme( OpNum ).EquipList( 1 ).CompPtr;

		//load local variables from the data structures
		CompMinLoad = this_component.MinLoad;
		CompMaxLoad = this_component.MaxLoad;
		CompOptLoad = this_component.OptLoad;
		DemandNode = PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).DemandNodeNum;
		SetPtNode = PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).SetPointNodeNum;
		TempIn = Node( DemandNode ).Temp;
		rho = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, TempIn, PlantLoop( LoopNum ).FluidIndex, RoutineName );

		DemandMdot = PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).SetPointFlowRate * rho;
		//DSU?  DemandMDot is a constant design flow rate, next based on actual current flow rate for accurate current demand?
		ActualMdot = Node( DemandNode ).MassFlowRate;
		CurSpecHeat = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, TempIn, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		if ( ( ActualMdot > 0.0 ) && ( ActualMdot != DemandMdot ) ) {
			DemandMdot = ActualMdot;
		}

		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			TempSetPt = Node( SetPtNode ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).CtrlTypeNum == CoolingOp ) {
				TempSetPt = Node( SetPtNode ).TempSetPointHi;
			} else if ( PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).CtrlTypeNum == HeatingOp ) {
				TempSetPt = Node( SetPtNode ).TempSetPointLo;
			} else if ( PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).CtrlTypeNum == DualOp ) {
				CurrentDemandForCoolingOp = DemandMdot * CurSpecHeat * ( Node( SetPtNode ).TempSetPointHi - TempIn );
				CurrentDemandForHeatingOp = DemandMdot * CurSpecHeat * ( Node( SetPtNode ).TempSetPointLo - TempIn );
				if ( ( CurrentDemandForCoolingOp < 0.0 ) && ( CurrentDemandForHeatingOp <= 0.0 ) ) { // cooling
					TempSetPt = Node( SetPtNode ).TempSetPointHi;
				} else if ( ( CurrentDemandForCoolingOp >= 0.0 ) && ( CurrentDemandForHeatingOp > 0.0 ) ) { // heating
					TempSetPt = Node( SetPtNode ).TempSetPointLo;
				} else { // deadband
					TempSetPt = TempIn;
				}
			} else {
				assert( false );
			}
		} else {
			assert( false );
		}}

		if ( TempSetPt == SensedNodeFlagValue ) {
			this_component.ON = false;
			this_component.MyLoad = 0.0;
			this_component.EquipDemand = 0.0;
		} else {

			CompDemand = ( DemandMdot * CurSpecHeat * ( TempSetPt - TempIn ) );

			if ( std::abs( CompDemand ) < LoopDemandTol ) CompDemand = 0.0;
			this_component.EquipDemand = CompDemand;

			//set MyLoad and runflag
			if ( PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).CtrlTypeNum == CoolingOp ) {
				if ( CompDemand < ( -LoopDemandTol ) ) {
					this_component.ON = true;
					this_component.MyLoad = CompDemand;
				} else {
					this_component.ON = false;
					this_component.MyLoad = 0.0;
				}
			} else if ( PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).CtrlTypeNum == HeatingOp ) {
				if ( CompDemand > LoopDemandTol ) {
					this_component.ON = true;
					this_component.MyLoad = CompDemand;
				} else {
					this_component.ON = false;
					this_component.MyLoad = 0.0;
				}
			} else if ( PlantLoop( LoopNum ).OpScheme( OpSchemePtr ).EquipList( ListPtr ).Comp( CompPtr ).CtrlTypeNum == DualOp ) {
				if ( CompDemand > LoopDemandTol || CompDemand < ( -LoopDemandTol ) ) {
					this_component.ON = true;
					this_component.MyLoad = CompDemand;
				} else {
					this_component.ON = false;
					this_component.MyLoad = 0.0;
				}
			}

			//Check bounds on MyLoad
			if ( std::abs( this_component.MyLoad ) > CompMaxLoad ) {
				this_component.MyLoad = sign( CompMaxLoad, this_component.MyLoad );
			}
			//   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
			//   MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad,CompMaxLoad)

			if ( std::abs( this_component.MyLoad ) < CompMinLoad ) {
				this_component.MyLoad = sign( CompMinLoad, this_component.MyLoad );
			}
			//   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
			//   MAX(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad,CompMinLoad)

		} //valid setpoint (TempSetPt /= SensedNodeFlagValue)
	}

	void
	DistributeUserDefinedPlantLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		int const CurCompLevelOpNum, // index for Plant()%LoopSide()%Branch()%Comp()%OpScheme()
		int const CurSchemePtr,
		Real64 const LoopDemand,
		Real64 & EP_UNUSED( RemLoopDemand )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::emsCallFromUserDefinedComponentModel;
		using EMSManager::ManageEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompPtr;

		auto & this_component( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ) );

		// ListPtr = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(CurCompLevelOpNum)%EquipList(1)%ListPtr
		CompPtr = this_component.OpScheme( CurCompLevelOpNum ).EquipList( 1 ).CompPtr;

		// fill internal variable
		PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).EquipList( 1 ).Comp( CompPtr ).EMSIntVarRemainingLoadValue = LoopDemand;

		// Call EMS program(s)
		if ( PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).ErlSimProgramMngr > 0 ) {
			ManageEMS( emsCallFromUserDefinedComponentModel, PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).ErlSimProgramMngr );
		}

		// move actuated value to MyLoad

		this_component.MyLoad = PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).EquipList( 1 ).Comp( CompPtr ).EMSActuatorDispatchedLoadValue;
		this_component.EquipDemand = PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).EquipList( 1 ).Comp( CompPtr ).EMSActuatorDispatchedLoadValue;
		if ( std::abs( this_component.MyLoad ) > LoopDemandTol ) {
			this_component.ON = true;

		} else {
			this_component.ON = false;
		}

	}

	// End Load Calculation/Distribution Section of the Plant Loop Module
	//******************************************************************************

	//********************************

	Real64
	FindRangeVariable(
		int const LoopNum, // PlantLoop data structure loop counter
		int const CurSchemePtr, // set by PL()%LoopSide()%Branch()%Comp()%OpScheme()%OpSchemePtr
		int const CurSchemeType // identifier set in PlantData
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   Jan 2004
		//       MODIFIED       Chandan Sharma, August 2010
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using namespace DataLoopNode;
		using DataEnvironment::OutWetBulbTemp;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutDewPointTemp;

		// Return value
		Real64 FindRangeVariable( 0.0 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// used to locate data in PL()%OpScheme(CurSchemePtr)
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ReferenceNodeNum;
		Real64 NodeTemperature;

		if ( CurSchemeType == DryBulbTDBOpSchemeType ) { // drybulb temp based controls
			ReferenceNodeNum = PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).ReferenceNodeNumber;
			NodeTemperature = Node( ReferenceNodeNum ).Temp;
			FindRangeVariable = NodeTemperature - OutDryBulbTemp;
		} else if ( CurSchemeType == WetBulbTDBOpSchemeType ) { // wetbulb temp based controls
			ReferenceNodeNum = PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).ReferenceNodeNumber;
			NodeTemperature = Node( ReferenceNodeNum ).Temp;
			FindRangeVariable = NodeTemperature - OutWetBulbTemp;
		} else if ( CurSchemeType == DewPointTDBOpSchemeType ) { // dewpoint temp based controls
			ReferenceNodeNum = PlantLoop( LoopNum ).OpScheme( CurSchemePtr ).ReferenceNodeNumber;
			NodeTemperature = Node( ReferenceNodeNum ).Temp;
			FindRangeVariable = NodeTemperature - OutDewPointTemp;
		} else {
			assert( false );
		} // OperationScheme

		return FindRangeVariable;

	}

	//********************************

	// Begin Plant Loop ON/OFF Utility Subroutines
	//******************************************************************************

	void
	TurnOnPlantLoopPipes(
		int const LoopNum,
		int const LoopSideNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine sets a logical flag
		// for the loop circulation pump to TRUE.

		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MachineOnLoopNum;
		int Num;

		for ( Num = 1; Num <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++Num ) {
			for ( MachineOnLoopNum = 1; MachineOnLoopNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).TotalComponents; ++MachineOnLoopNum ) {
				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnLoopNum ).TypeOf_Num );
				if ( ( SELECT_CASE_var == TypeOf_Pipe ) || ( SELECT_CASE_var == TypeOf_PipeInterior ) || ( SELECT_CASE_var == TypeOf_PipeExterior ) || ( SELECT_CASE_var == TypeOf_PipeUnderground ) ) {
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnLoopNum ).ON = true;
				} else {
					//Don't do anything
				}}
			}
		}

	}

	void
	TurnOffLoopEquipment( int const LoopNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         D.E. Fisher
		//       DATE WRITTEN   July 1998
		//       MODIFIED       D.E. Fisher, Aug. 2010
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MachineOnBranch;
		int LoopSideNum;
		int Num;

		for ( LoopSideNum = 1; LoopSideNum <= 2; ++LoopSideNum ) {
			for ( Num = 1; Num <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++Num ) {
				for ( MachineOnBranch = 1; MachineOnBranch <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).TotalComponents; ++MachineOnBranch ) {
					//Sankar Non Integrated Economizer
					if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnBranch ).GeneralEquipType != GenEquipTypes_Pump ) {
						PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnBranch ).ON = false;
						PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnBranch ).MyLoad = 0.0;
					}
				}
			}
		}

	}

	void
	TurnOffLoopSideEquipment(
		int const LoopNum,
		int const LoopSideNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         D.E. Fisher
		//       DATE WRITTEN   July 1998
		//       MODIFIED       D.E. Fisher, Aug. 2010
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MachineOnBranch;
		int Num;

		for ( Num = 1; Num <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++Num ) {
			for ( MachineOnBranch = 1; MachineOnBranch <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).TotalComponents; ++MachineOnBranch ) {
				//Sankar Non Integrated Economizer
				if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnBranch ).GeneralEquipType != GenEquipTypes_Pump ) {
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnBranch ).ON = false;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( Num ).Comp( MachineOnBranch ).MyLoad = 0.0;
				}
			}
		}

	}

	// End Plant Loop ON/OFF Utility Subroutines
	//******************************************************************************

	// Begin Plant EMS Control Routines
	//******************************************************************************

	void
	SetupPlantEMSActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         D.E. Fisher
		//       DATE WRITTEN   Feb 2007
		//       MODIFIED       B. Griffith August 2009, D. Fisher, Aug. 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine loads the plant EMS actuators

		// METHODOLOGY EMPLOYED:
		// Call the setupAcuator routine

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// SUBROUTINE VARIABLE DEFINITIONS

		// Locals
		std::string ActuatorType;
		std::string ActuatorName;
		std::string UniqueIDName;
		static std::string Units( "[on/off]" );
		// INTEGER                      :: NumAct
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			ActuatorName = "Plant Loop Overall";
			UniqueIDName = PlantLoop( LoopNum ).Name;
			ActuatorType = "On/Off Supervisory";
			SetupEMSActuator( ActuatorName, UniqueIDName, ActuatorType, Units, PlantLoop( LoopNum ).EMSCtrl, PlantLoop( LoopNum ).EMSValue );

			ActuatorName = "Supply Side Half Loop";
			UniqueIDName = PlantLoop( LoopNum ).Name;
			ActuatorType = "On/Off Supervisory";
			SetupEMSActuator( ActuatorName, UniqueIDName, ActuatorType, Units, PlantLoop( LoopNum ).LoopSide( SupplySide ).EMSCtrl, PlantLoop( LoopNum ).LoopSide( SupplySide ).EMSValue );

			ActuatorName = "Demand Side Half Loop";
			UniqueIDName = PlantLoop( LoopNum ).Name;
			ActuatorType = "On/Off Supervisory";
			SetupEMSActuator( ActuatorName, UniqueIDName, ActuatorType, Units, PlantLoop( LoopNum ).LoopSide( DemandSide ).EMSCtrl, PlantLoop( LoopNum ).LoopSide( DemandSide ).EMSValue );

			for ( LoopSideNum = 1; LoopSideNum <= 2; ++LoopSideNum ) {
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
					if ( LoopSideNum == SupplySide ) {
						ActuatorName = "Supply Side Branch";
						UniqueIDName = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Name;
						ActuatorType = "On/Off Supervisory";
						SetupEMSActuator( ActuatorName, UniqueIDName, ActuatorType, Units, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).EMSCtrlOverrideOn, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).EMSCtrlOverrideValue );
					}
					else if ( LoopSideNum == DemandSide ) {
						ActuatorName = "Demand Side Branch";
						UniqueIDName = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Name;
						ActuatorType = "On/Off Supervisory";
						SetupEMSActuator( ActuatorName, UniqueIDName, ActuatorType, Units, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).EMSCtrlOverrideOn, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).EMSCtrlOverrideValue );
					}
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						ActuatorName = "Plant Component " + ccSimPlantEquipTypes( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num );
						UniqueIDName = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).Name;
						ActuatorType = "On/Off Supervisory";
						SetupEMSActuator( ActuatorName, UniqueIDName, ActuatorType, "[W]", PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).EMSLoadOverrideOn, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).EMSLoadOverrideValue );
					}
				}
			}
		}

	}

	void
	ActivateEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		bool & LoopShutDownFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         D.E. Fisher
		//       DATE WRITTEN   Feb 2007
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine loads the plant EMS actuators

		// METHODOLOGY EMPLOYED: The EMS flags are evaluated in hierarchical order:
		//     LOOP flags override branch and component flags
		//     BRANCH flags override component flags
		// If the loop flag (EMSCtrl) is true, then
		//     IF EMSValue <= 0, shut down the entire loop including the pumps
		//     IF EMSValue > 0, no action
		// If the LoopSide flag (EMSCtrl) is true, then:
		//     IF EMSValue <=0, shut down all components on the LoopSide except the pumps
		//     IF EMSValue > 0, no action
		// If a component flag (EMSCtrl) is true, then:
		//     EMSValue <=0, shut down the component
		//     EMSValue > 0, calc. component load: MyLoad=MIN(MaxCompLoad,MaxCompLoad*EMSValue)

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataLoopNode;

		// SUBROUTINE ARGUMENT DEFINITIONS

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS
		static std::string const RoutineName( "ActivateEMSControls" );

		// SUBROUTINE VARIABLE DEFINITIONS
		Real64 CurMassFlowRate;
		Real64 ToutLowLimit;
		Real64 Tinlet;
		Real64 CurSpecHeat;
		Real64 QTemporary;
		//unused REAL(r64)                  :: ChangeInLoad

		//MODULE VARIABLE DECLARATIONS:

		// set up some nice references to avoid lookups
		auto & this_loop( PlantLoop( LoopNum ) );
		auto & this_loopside( this_loop.LoopSide( LoopSideNum ) );
		auto & this_comp( this_loopside.Branch( BranchNum ).Comp( CompNum ) );

		//Loop Control
		if ( this_loop.EMSCtrl ) {
			if ( this_loop.EMSValue <= 0.0 ) {
				LoopShutDownFlag = true;
				TurnOffLoopEquipment( LoopNum );
				return;
			} else {
				LoopShutDownFlag = false;
			}
		} else {
			LoopShutDownFlag = false;
		}

		//Half-loop control
		if ( this_loopside.EMSCtrl ) {
			if ( this_loopside.EMSValue <= 0.0 ) {
				TurnOffLoopSideEquipment( LoopNum, LoopSideNum );
				return;
			} else {
				//do nothing:  can't turn all LoopSide equip. ON with loop switch
			}
		}

		if ( this_comp.EMSLoadOverrideOn ) {
			//EMSValue <= 0 turn component OFF
			if ( this_comp.EMSLoadOverrideValue <= 0.0 ) {
				this_comp.ON = false;
				this_comp.Available = false;
				this_comp.MyLoad = 0.0;
				return;
			} else {
				//EMSValue > 0 Set Component Load and Turn component ON
				this_comp.ON = true;
				this_comp.Available = false;
				this_comp.MyLoad = min( this_comp.MaxLoad, ( this_comp.MaxLoad * this_comp.EMSLoadOverrideValue ) );

				// Check lower/upper temperature limit for chillers
				{ auto const SELECT_CASE_var( this_comp.TypeOf_Num );
				if ( ( SELECT_CASE_var == TypeOf_Chiller_ElectricEIR ) || ( SELECT_CASE_var == TypeOf_Chiller_Electric ) || ( SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR ) ) {

					//- Retrieve data from the plant loop data structure
					CurMassFlowRate = Node( this_comp.NodeNumIn ).MassFlowRate;
					ToutLowLimit = this_comp.MinOutletTemp;
					Tinlet = Node( this_comp.NodeNumIn ).Temp;
					CurSpecHeat = GetSpecificHeatGlycol( this_loop.FluidName, Tinlet, this_loop.FluidIndex, RoutineName );
					QTemporary = CurMassFlowRate * CurSpecHeat * ( Tinlet - ToutLowLimit );

					//- Don't correct if Q is zero, as this could indicate a component which this hasn't been implemented
					if ( QTemporary > 0.0 ) {
						if ( std::abs( this_comp.MyLoad ) > this_comp.MaxLoad ) {
							this_comp.MyLoad = sign( this_comp.MaxLoad, this_comp.MyLoad );
						}
						if ( std::abs( this_comp.MyLoad ) > QTemporary ) {
							this_comp.MyLoad = sign( QTemporary, this_comp.MyLoad );
						}
					}
				} else {
					//Nothing Changes for now, could add in case statements for boilers, which would use upper limit temp check
				}}
				return;
			} //EMSValue <=> 0
		} //EMSFlag

	}

	void
	AdjustChangeInLoadByEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		Real64 & ChangeInLoad // positive magnitude of load change
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// modify load dispatch if EMS controls are in place for a specific component

		// METHODOLOGY EMPLOYED:
		// Check if Loop Side is shutdown
		//  then check if branch is shutdown
		// then  check if component is overridden and use the value if it is.
		// take ABS() of EMS value to ensure sign is correct.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// set up some nice references to avoid lookups
		auto & this_loopside( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		auto & this_branch( this_loopside.Branch( BranchNum ) );
		auto & this_comp( this_branch.Comp( CompNum ) );

		if ( ( this_loopside.EMSCtrl ) && ( this_loopside.EMSValue <= 0.0 ) ) {
			ChangeInLoad = 0.0;
			return;
		}

		if ( ( this_branch.EMSCtrlOverrideOn ) && ( this_branch.EMSCtrlOverrideValue <= 0.0 ) ) {
			ChangeInLoad = 0.0;
			return;
		}

		if ( this_comp.EMSLoadOverrideOn ) {
			if ( this_comp.EMSLoadOverrideValue == 0.0 ) {
				ChangeInLoad = 0.0;
			}
		}

	}

	//*END PLANT EMS CONTROL ROUTINES!
	//******************************************************************************

} // PlantCondLoopOperation

} // EnergyPlus
