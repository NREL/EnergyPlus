// EnergyPlus::Standalone MixerComponent Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <MixerComponent.hh>
#include <InputProcessor.hh>
#include <DataStringGlobals.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::MixerComponent;
using namespace EnergyPlus::InputProcessor;

using namespace ObjexxFCL;

TEST(MixerComponent, GetZoneMixerIndex)
{
	// locals
	std::string CurrentModuleObject;
	std::string LINE;
	bool errFlag;
	int MixerIndex;
	int ReadStat;
	// set some variables
	CurrentModuleObject = "AirLoopHVAC:ZoneMixer";
	NumMixers = 3;
	GetInputFlag = false;
	errFlag = false;
	// allocate needed arrays
	MixerCond.allocate( NumMixers );
	MixerCond( 1 ).MixerName = "SPACE1-1 FPIU";
	MixerCond( 2 ).MixerName = "SPACE2-1 FPIU";
	MixerCond( 3 ).MixerName = "SPACE3-1 FPIU";
	GetZoneMixerIndex( MixerCond( 2 ).MixerName, MixerIndex, errFlag, CurrentModuleObject );
	EXPECT_EQ( 2, MixerIndex );
	EXPECT_EQ( false, errFlag );
	GetZoneMixerIndex( "Bad Mixer Name", MixerIndex, errFlag, CurrentModuleObject );
	EXPECT_EQ( 0, MixerIndex );
	EXPECT_EQ( true, errFlag );
}
