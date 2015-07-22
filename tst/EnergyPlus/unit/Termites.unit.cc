// EnergyPlus::ConvectionCoefficients unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers

// EnergyPlus Headers

using namespace EnergyPlus;

TEST( TermiteTest, AppendageCount )
{
	EXPECT_TRUE(6, Termites.getLegCount());
}
