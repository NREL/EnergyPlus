// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <PlantPipes/Pipes.hh>
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
#include <PlantLocation.hh>

namespace EnergyPlus {

namespace Pipes {

	// MODULE INFORMATION:
	//       AUTHOR         <author>
	//       DATE WRITTEN   <date_written>
	//       MODIFIED       Rahul Chillar , Jan 2005
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Added steam pipe to the module: RC

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Object Data
	Array1D< std::shared_ptr< LocalPipeData > > LocalPipe; // dimension to number of pipes

	void
	clear_state()
	{
		LocalPipe.deallocate();
	}

	std::shared_ptr<PlantComponent> 
	LocalPipeData::pipeFactory( int objectType, std::string objectName ) {

		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;
		std::shared_ptr<LocalPipeData> thisPipe( new LocalPipeData() );

		if ( objectType == DataPlant::TypeOf_Pipe ) {

			// search through adiabatic pipes here
			std::string const cCurrentModuleObject = "Pipe:Adiabatic";
			int numWaterPipes = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int PipeWaterNum = 1; PipeWaterNum <= numWaterPipes; ++PipeWaterNum ) {
				InputProcessor::GetObjectItem( cCurrentModuleObject, PipeWaterNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat );
				if ( objectName == DataIPShortCuts::cAlphaArgs( 1 ) ) {
					found = true;
					thisPipe->name = objectName;
					thisPipe->compType = DataPlant::TypeOf_Pipe;
					thisPipe->InletNodeNum = NodeInputManager::GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
					thisPipe->OutletNodeNum = NodeInputManager::GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
					BranchNodeConnections::TestCompSet( cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 2 ), DataIPShortCuts::cAlphaArgs( 3 ), "Pipe Nodes" );
					break;
				}
			}

		} else if ( objectType == DataPlant::TypeOf_PipeSteam ) {

			std::string const cCurrentModuleObject2 = "Pipe:Adiabatic:Steam";
			int numSteamPipes = InputProcessor::GetNumObjectsFound( cCurrentModuleObject2 );
			for ( int PipeSteamNum = 1; PipeSteamNum <= numSteamPipes; ++PipeSteamNum ) {
				InputProcessor::GetObjectItem( cCurrentModuleObject2, PipeSteamNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat );
				if ( objectName == DataIPShortCuts::cAlphaArgs( 1 ) ) {
					found = true;
					thisPipe->name = objectName;
					thisPipe->compType = DataPlant::TypeOf_PipeSteam;
					thisPipe->InletNodeNum = NodeInputManager::GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject2, DataIPShortCuts::cAlphaArgs( 1 ), DataLoopNode::NodeType_Steam, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
					thisPipe->OutletNodeNum = NodeInputManager::GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject2, DataIPShortCuts::cAlphaArgs( 1 ), DataLoopNode::NodeType_Steam, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
					BranchNodeConnections::TestCompSet( cCurrentModuleObject2, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 2 ), DataIPShortCuts::cAlphaArgs( 3 ), "Pipe Nodes" );
					break;
				}
			}

		} else { 
			//bad pipe type

		}

		if ( found && !ErrorsFound ) {
			LocalPipe.push_back( thisPipe );
			return thisPipe;
		} else {
			ShowFatalError( "GetPipeInput: Errors getting input for pipes" );
			// add a dummy return here to hush up the compiler
			return nullptr;
		}
	}

	int LocalPipeData::performEveryTimeInit( const PlantLocation & EP_UNUSED(calledFromLocation) ){
		// nothing here
		return 0;
	}

	int LocalPipeData::performFirstHVACInit( const PlantLocation & EP_UNUSED(calledFromLocation) ){
		// nothing here
		return 0;
	}

	int LocalPipeData::performInitLoopEquip( const PlantLocation & EP_UNUSED(calledFromLocation) ) {
		// nothing here
		return 0;
	}

	int LocalPipeData::performOneTimeInit( const PlantLocation & EP_UNUSED(calledFromLocation) ){
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
	
	int LocalPipeData::performBeginEnvrnInit( const PlantLocation & EP_UNUSED(calledFromLocation) ){
		PlantUtilities::InitComponentNodes( 0.0, DataPlant::PlantLoop( this->LoopNum ).MaxMassFlowRate, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSide, this->BranchIndex, this->CompIndex );
		return 0;
	}
	
	int LocalPipeData::simulate( const PlantLocation & EP_UNUSED(calledFromLocation), bool const & EP_UNUSED(FirstHVACIteration) ){
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
