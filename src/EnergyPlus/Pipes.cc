// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

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
#include <PlantComponent.hh>

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

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataHVACGlobals;
	using namespace DataLoopNode;
	using DataPlant::TypeOf_Pipe;
	using DataPlant::TypeOf_PipeSteam;
	using namespace DataIPShortCuts;
	using NodeInputManager::GetOnlySingleNode;
	using BranchNodeConnections::TestCompSet;

	// Object Data
	Array1D< std::shared_ptr< LocalPipeData > > LocalPipe; // dimension to number of pipes

	std::shared_ptr<PlantComponent> LocalPipeData::pipeFactory( std::string objectName ){

		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;
		std::shared_ptr<LocalPipeData> thisPipe( new LocalPipeData() );

		// search through adiabatic pipes here
		std::string const cCurrentModuleObject = "Pipe:Adiabatic";
		int numWaterPipes = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
		for ( int PipeWaterNum = 1; PipeWaterNum <= numWaterPipes; ++PipeWaterNum ) {
			InputProcessor::GetObjectItem( cCurrentModuleObject, PipeWaterNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
			if ( objectName == cAlphaArgs( 1 ) ) {
				found = true;
				thisPipe->name = objectName;
				thisPipe->compType = TypeOf_Pipe;
				thisPipe->InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				thisPipe->OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Pipe Nodes" );
			}
		}

		std::string const cCurrentModuleObject2 = "Pipe:Adiabatic:Steam";
		int numSteamPipes = InputProcessor::GetNumObjectsFound( cCurrentModuleObject2 );
		for ( int PipeSteamNum = 1; PipeSteamNum <= numSteamPipes; ++PipeSteamNum ) {
			InputProcessor::GetObjectItem( cCurrentModuleObject2, PipeSteamNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
			if ( objectName == cAlphaArgs( 1 ) ) {
				found = true;
				thisPipe->name = objectName;
				thisPipe->compType = TypeOf_PipeSteam;
				thisPipe->InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject2, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				thisPipe->OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject2, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject2, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Pipe Nodes" );
			}
		}

		if ( found && !ErrorsFound ) {
			LocalPipe.push_back( thisPipe );
			return thisPipe;
		} else {
			ShowFatalError( "GetPipeInput: Errors getting input for pipes" );
			return nullptr;
		}
	}

	int LocalPipeData::performEveryTimeInit(){
		// nothing here
		return 0;
	}
	
	int LocalPipeData::performOneTimeInit(){
		bool FoundOnLoop = 0;
		bool errFlag = false;
		DataPlant::ScanPlantLoopsForObject( this->name, this->compType, this->LoopNum, this->LoopSide, this->BranchIndex, this->CompIndex, _, _, FoundOnLoop, _, _, errFlag );
		if ( this->LoopNum == 0 ) {
			ShowFatalError( "SimPipes: Pipe=\"" + this->name + "\" not found on a Plant Loop." );
		}
		if ( errFlag ) {
			ShowFatalError( "SimPipes: Program terminated due to previous condition(s)." );
		}
		return 0;
	}
	
	int LocalPipeData::performBeginEnvrnInit(){
		PlantUtilities::InitComponentNodes( 0.0, DataPlant::PlantLoop( this->LoopNum ).MaxMassFlowRate, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSide, this->BranchIndex, this->CompIndex );
		return 0;
	}
	
	int LocalPipeData::simulate(){
		PlantUtilities::SafeCopyPlantNode( this->InletNodeNum, this->OutletNodeNum, this->LoopNum );
		return 0;
	}

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

} // Pipes

} // EnergyPlus
