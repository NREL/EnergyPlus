#ifndef PLANTLOCATION_HH_INCLUDED
#define PLANTLOCATION_HH_INCLUDED

namespace EnergyPlus {

struct PlantLocation
{
	// Members
	int LoopNum;
	int LoopSideNum;
	int BranchNum;
	int CompNum;

	// Default Constructor
	PlantLocation() :
		LoopNum( 0 ),
		LoopSideNum( 0 ),
		BranchNum( 0 ),
		CompNum( 0 )
	{}

	// Member Constructor
	PlantLocation(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum
	) :
		LoopNum( LoopNum ),
		LoopSideNum( LoopSideNum ),
		BranchNum( BranchNum ),
		CompNum( CompNum )
	{}

};

}

#endif
