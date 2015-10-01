#ifndef PLANTLOCATION_HH_INCLUDED
#define PLANTLOCATION_HH_INCLUDED

namespace EnergyPlus {

struct PlantLocation
{
	// Members
	int loopNum;
	int loopSideNum;
	int branchNum;
	int compNum;

	// Default Constructor
	PlantLocation() :
		LoopNum( 0 ),
		LoopSideNum( 0 ),
		BranchNum( 0 ),
		CompNum( 0 )

	// Default Constructor
	PlantLocation() :
		loopNum( 0 ),
		loopSideNum( 0 ),
		branchNum( 0 ),
		compNum( 0 )
	{}
};

}

#endif
