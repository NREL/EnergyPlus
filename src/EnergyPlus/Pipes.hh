#ifndef Pipes_hh_INCLUDED
#define Pipes_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace Pipes {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumLocalPipes;
	extern bool GetPipeInputFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Pipe

	// Types

	struct LocalPipeData
	{
		// Members
		std::string Name; // main plant (cooling) loop ID
		int TypeOf; // type of pipe
		int InletNodeNum; // Node number on the inlet side of the plant
		int OutletNodeNum; // Node number on the inlet side of the plant
		int LoopNum; // Index of plant loop where this pipe resides
		int LoopSide; // Index of plant loop side where this pipe resides
		int BranchIndex; // Index of plant Branch index where this pipe resides
		int CompIndex; // Index of plant Comp index where this pipe resides
		bool OneTimeInit;
		bool CheckEquipName;
		bool EnvrnFlag;

		// Default Constructor
		LocalPipeData() :
			TypeOf( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchIndex( 0 ),
			CompIndex( 0 ),
			OneTimeInit( true ),
			CheckEquipName( true ),
			EnvrnFlag( true )
		{}

		// Member Constructor
		LocalPipeData(
			std::string const & Name, // main plant (cooling) loop ID
			int const TypeOf, // type of pipe
			int const InletNodeNum, // Node number on the inlet side of the plant
			int const OutletNodeNum, // Node number on the inlet side of the plant
			int const LoopNum, // Index of plant loop where this pipe resides
			int const LoopSide, // Index of plant loop side where this pipe resides
			int const BranchIndex, // Index of plant Branch index where this pipe resides
			int const CompIndex, // Index of plant Comp index where this pipe resides
			bool const OneTimeInit,
			bool const CheckEquipName,
			bool const EnvrnFlag
		) :
			Name( Name ),
			TypeOf( TypeOf ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			LoopNum( LoopNum ),
			LoopSide( LoopSide ),
			BranchIndex( BranchIndex ),
			CompIndex( CompIndex ),
			OneTimeInit( OneTimeInit ),
			CheckEquipName( CheckEquipName ),
			EnvrnFlag( EnvrnFlag )
		{}

	};

	// Object Data
	extern Array1D< LocalPipeData > LocalPipe; // dimension to number of pipes

	// Functions
	void
	clear_state();

	void
	SimPipes(
		int const CompType,
		std::string & PipeName,
		int & CompIndex,
		Real64 const MaxVolFlowRate,
		bool const InitLoopEquip,
		bool const FirstHVACIteration
	);

	// End Plant Loop Module Driver Subroutines
	//******************************************************************************

	// Beginning of Plant Loop Module Get Input subroutines
	//******************************************************************************

	void
	GetPipeInput();

	// End of Get Input subroutines for the Plant Loop Module
	//******************************************************************************

	// Beginning Initialization Section of the Plant Loop Module
	//******************************************************************************

	void
	InitializePipes(
		int const PipeType, // Type of Pipe
		std::string const & PipeName, // Name of Pipe
		int & PipeNum, // Index into pipe structure for name
		Real64 const MaxVolFlowRate // unused at present time
	);

} // Pipes

} // EnergyPlus

#endif
