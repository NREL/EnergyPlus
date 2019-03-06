
INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/third_party/FMI2/ )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )
INCLUDE_DIRECTORIES( ${PROJECT_BINARY_DIR} )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR} )
INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/src/EnergyPlus/public )
INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/src/EnergyPlus )

set(src
  EPFMI.hpp
  EPFMI.cpp
  EPFMI_Export.hpp
  ../EnergyPlus/CommandLineInterface.hh 
  ../EnergyPlus/CommandLineInterface.cc 
  ../EnergyPlus/EnergyPlusPgm.cc 
  ../EnergyPlus/public/EnergyPlusPgm.hh 
)

add_library( epfmi SHARED ${src} )
set_target_properties( epfmi PROPERTIES OUTPUT_NAME "epfmi-${ENERGYPLUS_VERSION}" )

target_link_libraries( epfmi energypluslib )


if( BUILD_TESTING )
  configure_file( Test/config.h.in "${CMAKE_CURRENT_BINARY_DIR}/test-config.h.in2" )
  #file(GENERATE OUTPUT output-file
  #     <INPUT input-file|CONTENT content>
  #     [CONDITION expression])
  
  file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/test-config.h" INPUT "${CMAKE_CURRENT_BINARY_DIR}/test-config.h.in2")
  
  #add_executable( libfmi_dll_test Test/EPFMI_DLL_Test.c )
  #add_dependencies( libfmi_dll_test epfmi )
  #target_link_libraries( libfmi_dll_test ${CMAKE_DL_LIBS} )
endif()

install( TARGETS epfmi
  RUNTIME DESTINATION ./bin
  LIBRARY DESTINATION ./lib
  ARCHIVE DESTINATION ./lib
)

install( FILES EPFMI.hpp DESTINATION ./include )

export(TARGETS epfmi APPEND FILE energyplus-exports.cmake)

set(test_src
  Test/EPFMI_Test.cpp
  Test/main.cc
)

CREATE_TEST_TARGETS( epfmi "${test_src}" "epfmi" )

