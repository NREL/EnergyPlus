// EnergyPlus Unit Test Driver

// Google Test Headers
#include <gtest/gtest.h>

// Google Test main
int
main( int argc, char **argv )
{
	::testing::InitGoogleTest( &argc, argv );
	return RUN_ALL_TESTS();
}
