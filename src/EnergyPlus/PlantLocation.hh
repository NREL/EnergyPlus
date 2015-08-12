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
		loopNum( 0 ),
		loopSideNum( 0 ),
		branchNum( 0 ),
		compNum( 0 )
	{}

	// Member Constructor
	PlantLocation(
		int const loopNum,
		int const loopSideNum,
		int const branchNum,
		int const compNum
	) :
		loopNum( loopNum ),
		loopSideNum( loopSideNum ),
		branchNum( branchNum ),
		compNum( compNum )
	{}

};

}

#endif
