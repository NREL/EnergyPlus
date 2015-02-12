INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/EnergyPlus )

if(MSVC)
  ADD_DEFINITIONS(/Za) # Disables MS language extensions
endif()

set( test_src
  DataPlant.unit.cc
  DXCoils.unit.cc
  ExteriorEnergyUse.unit.cc
  HeatBalanceManager.unit.cc
  HVACStandaloneERV.unit.cc
  OutputProcessor.unit.cc
  OutputReportTabular.unit.cc
  SizingManager.unit.cc
  SolarShading.unit.cc
  SortAndStringUtilities.unit.cc
  Vectors.unit.cc
  Vector.unit.cc
  WaterThermalTanks.unit.cc
)

set( test_dependencies
  energyplusapi
#  objexx
#  sqlite
#  bcvtb 
#  epexpat 
#  epfmiimport 
#  DElight
 )

if(CMAKE_HOST_UNIX)
  if(NOT APPLE)
    list(APPEND test_dependencies dl )
  endif()
endif()

# Executable name will be EnergyPlus_tests
# Execute EnergyPlus_tests --help for options using gtest runner
# Execute EnergyPlus_tests with no arguments to run all tests
CREATE_TEST_TARGETS( energyplus "${test_src}" "${test_dependencies}" )
