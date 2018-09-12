
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/FMI2/ )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )
INCLUDE_DIRECTORIES( ${CMAKE_BINARY_DIR} )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../EnergyPlus/public )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../EnergyPlus )

configure_file( Test/config.h.in "${CMAKE_BINARY_DIR}/test-config.h.in2" )
#file(GENERATE OUTPUT output-file
#     <INPUT input-file|CONTENT content>
#     [CONDITION expression])

file(GENERATE OUTPUT test-config.h INPUT "${CMAKE_BINARY_DIR}/test-config.h.in2")

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

add_executable( libfmi_test Test/EPFMI_DLL_TEST.c )
add_dependencies( libfmi_test epfmi )

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

