INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/EnergyPlus )

set( test_src
  TestHelpers/IdfParser.cc
  TestHelpers/IdfParser.hh
  Fixtures/EnergyPlusFixture.cc
  Fixtures/EnergyPlusFixture.hh
  Fixtures/HVACFixture.hh
  Fixtures/IdfParserFixture.hh
  Fixtures/InputProcessorFixture.hh
  Fixtures/SQLiteFixture.hh
  AdvancedAFN.unit.cc
  AirflowNetworkBalanceManager.unit.cc
  AirflowNetworkSolver.unit.cc
  ChillerElectricEIR.unit.cc;
  ConvectionCoefficients.unit.cc
  DataPlant.unit.cc
  DataZoneEquipment.unit.cc
  DemandResponse.unit.cc
  DXCoils.unit.cc
  EMSManager.unit.cc
  EvaporativeCoolers.unit.cc
  ExteriorEnergyUse.unit.cc
  Fans.unit.cc
  FaultFoulingAirFilters.unit.cc
  FluidCoolers.unit.cc
  Furnaces.unit.cc
  GroundHeatExchangers.unit.cc
  HeatBalanceManager.unit.cc
  HeatRecovery.unit.cc
  Humidifiers.unit.cc
  HVACSizingSimulationManager.unit.cc 
  HVACStandaloneERV.unit.cc
  ICSCollector.unit.cc
  IdfParser.unit.cc
  InputProcessor.unit.cc
  LowTempRadiantSystem.unit.cc
  ManageElectricPower.unit.cc
  HVACUnitarySystem.unit.cc
  MixedAir.unit.cc
  MixerComponent.unit.cc
  PurchasedAirManager.unit.cc
  OutputProcessor.unit.cc
  OutputReportTabular.unit.cc
  ReportSizingManager.unit.cc
  RoomAirModelUserTempPattern.unit.cc
  SecondaryDXCoils.unit.cc
  SetPointManager.unit.cc
  SizingAnalysisObjects.unit.cc
  SizingManager.unit.cc
  SolarShading.unit.cc
  SortAndStringUtilities.unit.cc
  SQLite.unit.cc
  SwimmingPool.unit.cc
  Vectors.unit.cc
  Vector.unit.cc
  WaterCoils.unit.cc
  WaterThermalTanks.unit.cc
  WaterToAirHeatPumpSimple.unit.cc
  ZonePlenum.unit.cc
  ZoneTempPredictorCorrector.unit.cc
  main.cc
)
set( test_dependencies
  energyplusapi
 )

if(CMAKE_HOST_UNIX)
  if(NOT APPLE)
    list(APPEND test_dependencies dl )
  endif()
endif()

# Executable name will be EnergyPlus_tests
# Execute energyplus_tests --help for options using gtest runner
# Execute energyplus_tests with no arguments to run all tests
CREATE_TEST_TARGETS( energyplus "${test_src}" "${test_dependencies}" )
