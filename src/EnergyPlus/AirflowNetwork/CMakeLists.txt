cmake_minimum_required (VERSION 2.8)

project(AirflowNetwork)

#INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )

# public API headers go in the src/EnergyPlus/public folder
#INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/public )

# Don't reconfigure this file when moving from PROFILE_GENERATE to PROFILE_USE
#if(NOT PROFILE_USE)
#  configure_file( DataStringGlobals.in.cc "${CMAKE_CURRENT_BINARY_DIR}/DataStringGlobals.cc" )
#  configure_file( ConfiguredFunctions.in.cc "${CMAKE_CURRENT_BINARY_DIR}/ConfiguredFunctions.cc" )
#  file( WRITE "${CMAKE_CURRENT_BINARY_DIR}/EmbeddedEpJSONSchema.cc" "Will be filled in later by GenerateEmbeddedEpJSONSchema" )
#endif()

set( HDRS include/AirflowNetworkSolver.hh
          include/DataAirflowNetwork.hh
)

set( SRCS src/AirflowNetworkSolver.cc
          src/DataAirflowNetwork.cc
)

#include_directories( include )

#RoomAirModelAirflowNetwork.cc
#RoomAirModelAirflowNetwork.hh
#RoomAirModelManager.cc
#RoomAirModelManager.hh
#RoomAirModelUserTempPattern.cc
#RoomAirModelUserTempPattern.hh

#CREATE_SRC_GROUPS( "${SRC}" )

if (UNIX)
  ADD_DEFINITIONS("-fPIC")
endif()

# Create a static library
# this will be linked statically to create the DLL and also the unit tests
add_library( airflownetworklib STATIC ${SRCS} ${HDRS} )

target_include_directories( airflownetworklib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include )

# second we will create the shared library that is actually packaged with EnergyPlus
#add_library( energyplusapi SHARED  CommandLineInterface.hh CommandLineInterface.cc EnergyPlusPgm.cc public/EnergyPlusPgm.hh )
#target_link_libraries( energyplusapi energypluslib )

#set_target_properties(
#  energyplusapi
#  PROPERTIES
#  INSTALL_NAME_DIR "@executable_path"
#)
#install( TARGETS energyplusapi
#  RUNTIME DESTINATION ./
#  LIBRARY DESTINATION ./
#  ARCHIVE DESTINATION ./
#)

#set_target_properties(energyplus PROPERTIES VERSION ${ENERGYPLUS_VERSION})
#set_target_properties(energyplusapi PROPERTIES VERSION ${ENERGYPLUS_VERSION})

#install( TARGETS energyplus energyplusapi DESTINATION ./ )

#if( BUILD_TESTING )
# Build the test executable
#  add_executable( TestEnergyPlusCallbacks test_ep_as_library.cc )
#  target_link_libraries( TestEnergyPlusCallbacks energyplusapi )
#  add_test(NAME "integration.TestEnergyPlusCallbacks"
#           COMMAND ${CMAKE_COMMAND}
#           -DSOURCE_DIR=${CMAKE_SOURCE_DIR}
#           -DBINARY_DIR=${CMAKE_BINARY_DIR}
#           -DIDF_FILE=1ZoneUncontrolled.idf
#           -DEPW_FILE=USA_CO_Golden-NREL.724666_TMY3.epw
#           -P ${CMAKE_SOURCE_DIR}/cmake/RunCallbackTest.cmake)
#endif()

#if(UNIX AND NOT APPLE)
#  ADD_CUSTOM_COMMAND(TARGET energyplus POST_BUILD
#  COMMAND ${CMAKE_COMMAND} -E create_symlink energyplus EnergyPlus
#  COMMAND ${CMAKE_COMMAND} -E rename EnergyPlus "${CMAKE_BINARY_DIR}/Products/EnergyPlus")
#endif()
