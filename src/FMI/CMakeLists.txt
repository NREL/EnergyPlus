
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

configure_file( Test/config.h.in "${CMAKE_CURRENT_BINARY_DIR}/test-config.h" )

CREATE_TEST_TARGETS( epfmi "${test_src}" "epfmi" )

option(BUILD_SWIG_EPFMI_BINDINGS "Build SWIG EPFMI bindings" OFF)
if(BUILD_SWIG_EPFMI_BINDINGS)
  include(swig_epfmi.cmake)
  MAKE_SWIG_TARGET("epfmi" "epfmi" "${CMAKE_CURRENT_SOURCE_DIR}/EPFMI.i" "${CMAKE_CURRENT_SOURCE_DIR}/EPFMI.i" "epfmi" "")
endif()


