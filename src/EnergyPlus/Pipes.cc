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

// EnergyPlus Headers
#include <Pipes.hh>
#include <BranchNodeConnections.hh>
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

namespace Pipes {

	// Module containing the routines dealing with the <module_name>

	// MODULE INFORMATION:
	//       AUTHOR         <author>
	//       DATE WRITTEN   <date_written>
	//       MODIFIED       Rahul Chillar , Jan 2005
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Added steam pipe to the module: RC

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataHVACGlobals;
	using namespace DataLoopNode;
	using DataPlant::TypeOf_Pipe;
	using DataPlant::TypeOf_PipeSteam;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	int NumLocalPipes( 0 );
	bool GetPipeInputFlag( true );

	// SUBROUTINE SPECIFICATIONS FOR MODULE Pipe

	// Object Data
	Array1D< LocalPipeData > LocalPipe; // dimension to number of pipes

	// MODULE SUBROUTINES:

	// Beginning of Plant Loop Module Driver Subroutines
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		NumLocalPipes = 0;
		GetPipeInputFlag = true;
		LocalPipe.deallocate();
	}

	void
	SimPipes(
		int const CompType,
		std::string & PipeName,
		int & CompIndex,
		Real64 const EP_UNUSED( MaxVolFlowRate ),
		bool const EP_UNUSED( InitLoopEquip ),
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using General::TrimSigDigits;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using PlantUtilities::SafeCopyPlantNode;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PipeNum;
		//  INTEGER :: LoopNum
		//  INTEGER :: LoopSideNum
		//  INTEGER :: BranchNum
		//  INTEGER :: CompNum
		int FoundOnLoop;
		bool errFlag;

		//FLOW

		//GET INPUT
		if ( GetPipeInputFlag ) {
			GetPipeInput();
			GetPipeInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			PipeNum = FindItemInList( PipeName, LocalPipe );
			if ( PipeNum == 0 ) {
				ShowFatalError( "SimPipes: Pipe requested not found =" + PipeName ); // Catch any bad names before crashing
			}
			CompIndex = PipeNum;
		} else {
			PipeNum = CompIndex;
			if ( PipeNum > NumLocalPipes || PipeNum < 1 ) {
				ShowFatalError( "SimPipes: Invalid CompIndex passed=" + TrimSigDigits( PipeNum ) + ", Number of Pipes=" + TrimSigDigits( NumLocalPipes ) + ", Pipe name=" + PipeName );
			}
			if ( LocalPipe( PipeNum ).CheckEquipName ) {
				if ( PipeName != LocalPipe( PipeNum ).Name ) {
					ShowFatalError( "SimPipes: Invalid CompIndex passed=" + TrimSigDigits( PipeNum ) + ", Pipe name=" + PipeName + ", stored Pipe Name for that index=" + LocalPipe( PipeNum ).Name );
				}
				LocalPipe( PipeNum ).CheckEquipName = false;
			}
		}

		if ( LocalPipe( PipeNum ).OneTimeInit ) {
			FoundOnLoop = 0;
			errFlag = false;
			ScanPlantLoopsForObject( LocalPipe( PipeNum ).Name, CompType, LocalPipe( PipeNum ).LoopNum, LocalPipe( PipeNum ).LoopSide, LocalPipe( PipeNum ).BranchIndex, LocalPipe( PipeNum ).CompIndex, _, _, FoundOnLoop, _, _, errFlag );
			if ( FoundOnLoop == 0 ) {
				ShowFatalError( "SimPipes: Pipe=\"" + PipeName + "\" not found on a Plant Loop." );
			}
			if ( errFlag ) {
				ShowFatalError( "SimPipes: Program terminated due to previous condition(s)." );
			}
			LocalPipe( PipeNum ).OneTimeInit = false;
		}

		if ( BeginEnvrnFlag && LocalPipe( PipeNum ).EnvrnFlag ) {
			InitComponentNodes( 0.0, PlantLoop( LocalPipe( PipeNum ).LoopNum ).MaxMassFlowRate, LocalPipe( PipeNum ).InletNodeNum, LocalPipe( PipeNum ).OutletNodeNum, LocalPipe( PipeNum ).LoopNum, LocalPipe( PipeNum ).LoopSide, LocalPipe( PipeNum ).BranchIndex, LocalPipe( PipeNum ).CompIndex );
			LocalPipe( PipeNum ).EnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) LocalPipe( PipeNum ).EnvrnFlag = true;

		SafeCopyPlantNode( LocalPipe( PipeNum ).InletNodeNum, LocalPipe( PipeNum ).OutletNodeNum, LocalPipe( PipeNum ).LoopNum );
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%FluidType            = Node(LocalPipe(PipeNum)%InletNodeNum)%FluidType
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Temp                 = Node(LocalPipe(PipeNum)%InletNodeNum)%Temp
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMin              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMin
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMax              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMax
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRate         = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRate
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMin      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMin
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMax      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMax
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMinAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMinAvail
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMaxAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMaxAvail
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Quality              = Node(LocalPipe(PipeNum)%InletNodeNum)%Quality
		//  !Only pass pressure if we aren't doing a pressure simulation
		//  IF (PlantLoop(LocalPipe(PipeNum)%LoopNum)%PressureSimType > 1) THEN
		//    !Don't do anything
		//  ELSE
		//    Node(LocalPipe(PipeNum)%OutletNodeNum)%Press              = Node(LocalPipe(PipeNum)%InletNodeNum)%Press
		//  END IF
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Enthalpy             = Node(LocalPipe(PipeNum)%InletNodeNum)%Enthalpy
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%HumRat               = Node(LocalPipe(PipeNum)%InletNodeNum)%HumRat

	}

	// End Plant Loop Module Driver Subroutines
	//******************************************************************************

	// Beginning of Plant Loop Module Get Input subroutines
	//******************************************************************************

	void
	GetPipeInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		//USE DataPlant, ONLY: LoopData

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
		int PipeNum;
		int NumWaterPipes;
		int NumSteamPipes;
		int PipeSteamNum;
		int PipeWaterNum;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		//GET NUMBER OF ALL EQUIPMENT TYPES
		NumWaterPipes = GetNumObjectsFound( "Pipe:Adiabatic" );
		NumSteamPipes = GetNumObjectsFound( "Pipe:Adiabatic:Steam" );
		NumLocalPipes = NumWaterPipes + NumSteamPipes;
		LocalPipe.allocate( NumLocalPipes );

		cCurrentModuleObject = "Pipe:Adiabatic";
		for ( PipeWaterNum = 1; PipeWaterNum <= NumWaterPipes; ++PipeWaterNum ) {
			PipeNum = PipeWaterNum;
			GetObjectItem( cCurrentModuleObject, PipeWaterNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), LocalPipe, PipeNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			LocalPipe( PipeNum ).Name = cAlphaArgs( 1 );
			LocalPipe( PipeNum ).TypeOf = TypeOf_Pipe;

			LocalPipe( PipeNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			LocalPipe( PipeNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Pipe Nodes" );
		}

		PipeNum = NumWaterPipes;
		cCurrentModuleObject = "Pipe:Adiabatic:Steam";

		for ( PipeSteamNum = 1; PipeSteamNum <= NumSteamPipes; ++PipeSteamNum ) {
			++PipeNum;
			GetObjectItem( cCurrentModuleObject, PipeSteamNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), LocalPipe, PipeNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			LocalPipe( PipeNum ).Name = cAlphaArgs( 1 );
			LocalPipe( PipeNum ).TypeOf = TypeOf_PipeSteam;
			LocalPipe( PipeNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			LocalPipe( PipeNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Pipe Nodes" );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetPipeInput: Errors getting input for pipes" );
		}

	}

	// End of Get Input subroutines for the Plant Loop Module
	//******************************************************************************

	// Beginning Initialization Section of the Plant Loop Module
	//******************************************************************************

	void
	InitializePipes(
		int const EP_UNUSED( PipeType ), // Type of Pipe
		std::string const & PipeName, // Name of Pipe
		int & PipeNum, // Index into pipe structure for name
		Real64 const EP_UNUSED( MaxVolFlowRate ) // unused at present time
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provide an external call to initialize Pipes/index numbers.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

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

		if ( GetPipeInputFlag ) {
			GetPipeInput();
			GetPipeInputFlag = false;
		}

		if ( PipeNum == 0 ) {
			PipeNum = FindItemInList( PipeName, LocalPipe );
			if ( PipeNum == 0 ) {
				ShowFatalError( "SimPipes: Pipe requested not found =" + PipeName ); // Catch any bad names before crashing
			}
		} else {
			if ( PipeNum > NumLocalPipes || PipeNum < 1 ) {
				ShowFatalError( "InitializePipe: Invalid PipeNum passed=" + TrimSigDigits( PipeNum ) + ", Number of Pipes=" + TrimSigDigits( NumLocalPipes ) + ", Pipe name=" + PipeName );
			}
			if ( LocalPipe( PipeNum ).CheckEquipName ) {
				if ( PipeName != LocalPipe( PipeNum ).Name ) {
					ShowFatalError( "InitializePipe: Invalid PipeNum passed=" + TrimSigDigits( PipeNum ) + ", Pipe name=" + PipeName + ", stored Pipe Name for that index=" + LocalPipe( PipeNum ).Name );
				}
				LocalPipe( PipeNum ).CheckEquipName = false;
			}
		}

		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%FluidType            = Node(LocalPipe(PipeNum)%InletNodeNum)%FluidType
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Temp                 = Node(LocalPipe(PipeNum)%InletNodeNum)%Temp
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMin              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMin
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMax              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMax
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRate         = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRate
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMin      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMin
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMax      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMax
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMinAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMinAvail
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMaxAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMaxAvail
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Quality              = Node(LocalPipe(PipeNum)%InletNodeNum)%Quality
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Press                = Node(LocalPipe(PipeNum)%InletNodeNum)%Press
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%Enthalpy             = Node(LocalPipe(PipeNum)%InletNodeNum)%Enthalpy
		//  Node(LocalPipe(PipeNum)%OutletNodeNum)%HumRat               = Node(LocalPipe(PipeNum)%InletNodeNum)%HumRat

	}

} // Pipes

} // EnergyPlus
