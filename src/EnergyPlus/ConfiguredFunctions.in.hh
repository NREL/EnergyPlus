#ifndef CONFIGURED_FUNCTIONS_HH
#define CONFIGURED_FUNCTIONS_HH

#include <string>

namespace EnergyPlus {

	std::string configured_source_directory()
	{
		return ("${CMAKE_SOURCE_DIR}");
	}

	std::string configured_build_directory()
	{
		return ("${CMAKE_BUILD_DIR}");
	}

}

#endif // CONFIGURED_FUNCTIONS_HH
