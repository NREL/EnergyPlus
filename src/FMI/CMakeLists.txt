
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/FMUParser/ )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR} )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../EnergyPlus/public )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../EnergyPlus )

configure_file( Test/config.hpp.in config.hpp )

set(src
  EPFMI.hpp
  EPFMI.cpp
  EPFMIData.hpp
  EPFMIData.cpp
  ../EnergyPlus/CommandLineInterface.hh 
  ../EnergyPlus/CommandLineInterface.cc 
  ../EnergyPlus/EnergyPlusPgm.cc 
  ../EnergyPlus/public/EnergyPlusPgm.hh 
)

add_library( epfmi SHARED ${src} )

target_link_libraries( epfmi energypluslib )

set(test_src
  Test/EPFMI_Test.cpp
  Test/main.cc
)

CREATE_TEST_TARGETS( epfmi "${test_src}" "epfmi" )

install( TARGETS epfmi
  RUNTIME DESTINATION ./bin
  LIBRARY DESTINATION ./lib
  ARCHIVE DESTINATION ./lib
)

install( FILES EPFMI.hpp DESTINATION ./include )

