INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/EnergyPlus )

if(MSVC)
  ADD_DEFINITIONS(/Za) # Disables MS language extensions
endif()

set( test_src
  DataPlant.unit.cc
  SortAndStringUtilities.unit.cc
  Vectors.unit.cc
  Vector.unit.cc
)

set( test_dependencies
  EnergyPlusLib
  objexx
  sqlite
  bcvtb 
  epexpat 
  epfmiimport 
  DElight
 )

if(CMAKE_HOST_UNIX)
  if(NOT APPLE)
    list(APPEND test_dependencies dl )
  endif()
endif()

# Executable name will be EnergyPlus_tests
# Execute EnergyPlus_tests --help for options using gtest runner
# Execute EnergyPlus_tests with no arguments to run all tests
CREATE_TEST_TARGETS( EnergyPlus "${test_src}" "${test_dependencies}" )
