
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/FMUParser/ )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../EnergyPlus/public )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../EnergyPlus )

set(src
  EPFMI.cpp
  ../EnergyPlus/CommandLineInterface.hh 
  ../EnergyPlus/CommandLineInterface.cc 
  ../EnergyPlus/EnergyPlusPgm.cc 
  ../EnergyPlus/public/EnergyPlusPgm.hh 
)

add_library( epfmu "${src}" )

target_link_libraries( epfmu energypluslib )

add_executable( demo Test/demo.cpp )
target_link_libraries( demo epfmu )

set(test_src
  Test/EPFMI_Test.cpp
  Test/main.cc
)

CREATE_TEST_TARGETS( epfmu "${test_src}" "epfmu" )

