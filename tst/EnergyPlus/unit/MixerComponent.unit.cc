// EnergyPlus::Standalone MixerComponent Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::MixerComponent;

TEST(MixerComponent, GetZoneMixerIndex)
{
	ShowMessage( "Begin Test: MixerComponent, GetZoneMixerIndex" );
	// locals
	std::string CurrentModuleObject;
	std::string LINE;
	bool errFlag;
	int MixerIndex;
	// set some variables
	CurrentModuleObject = "AirLoopHVAC:ZoneMixer";
	NumMixers = 3;
	GetInputFlag = false;
	errFlag = false;
	// allocate needed arrays
	MixerCond.allocate( NumMixers );
	MixerCond( 1 ).MixerName = "SPACE1-1 ATU Mixer";
	MixerCond( 2 ).MixerName = "SPACE2-1 ATU Mixer";
	MixerCond( 3 ).MixerName = "SPACE3-1 ATU Mixer";
	GetZoneMixerIndex( MixerCond( 2 ).MixerName, MixerIndex, errFlag, CurrentModuleObject );
	EXPECT_EQ( 2, MixerIndex );
	EXPECT_FALSE( errFlag );
	GetZoneMixerIndex( "SPACE3-3 ATU Mixer", MixerIndex, errFlag, CurrentModuleObject );
	EXPECT_EQ( 0, MixerIndex );
	EXPECT_TRUE( errFlag );
}
