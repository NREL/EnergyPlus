#ifndef Pipes_hh_INCLUDED
#define Pipes_hh_INCLUDED

// C++ headers
#include <memory>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace Pipes {

	class LocalPipeData : public PlantComponent
	{
	public:
		// Members
		int InletNodeNum; // Node number on the inlet side of the plant
		int OutletNodeNum; // Node number on the inlet side of the plant
		int LoopNum; // Index of plant loop where this pipe resides
		int LoopSide; // Index of plant loop side where this pipe resides
		int BranchIndex; // Index of plant Branch index where this pipe resides
		int CompIndex; // Index of plant Comp index where this pipe resides

		// Default Constructor
		LocalPipeData() :
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchIndex( 0 ),
			CompIndex( 0 )
		{}

		static std::shared_ptr<PlantComponent> pipeFactory( std::string objectName );
		int performEveryTimeInit();
		int performOneTimeInit();
		int performBeginEnvrnInit();
		int simulate();

	};

	// Object Data
	extern Array1D< std::shared_ptr< LocalPipeData > > LocalPipe; // dimension to number of pipes

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

#endif
