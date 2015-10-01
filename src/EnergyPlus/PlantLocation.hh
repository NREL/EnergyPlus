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
		int iloopNum,
		int iloopSideNum,
		int ibranchNum,
		int icompNum
	) :
		loopNum( iloopNum ),
		loopSideNum( iloopSideNum ),
		branchNum( ibranchNum ),
		compNum( icompNum )
	{}
};

}

#endif
