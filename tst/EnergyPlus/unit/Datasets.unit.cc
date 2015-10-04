// C++ Headers
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

// Google Test Headers
#include <gtest/gtest.h>

// Fixtures, etc.
#include "Fixtures/EnergyPlusFixture.hh"

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, DataSets ) {
		std::ifstream infile("../../../datasets/AirCooledChiller.idf");
		std::vector<std::string> lines;
		std::string line;
		while ( std::getline( infile, line ) )
		{
			lines.push_back( line );
		}
		std::string const idf_objects = delimited_string( lines );

		ASSERT_FALSE( process_idf( idf_objects ) );
	}

}
