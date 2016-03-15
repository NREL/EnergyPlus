INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/EnergyPlus )
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )

set( test_src
  TestHelpers/IdfParser.cc
  TestHelpers/IdfParser.hh
  Fixtures/EnergyPlusFixture.cc
  Fixtures/EnergyPlusFixture.hh
  Fixtures/IdfParserFixture.hh
  Fixtures/InputProcessorFixture.hh
  Fixtures/SQLiteFixture.hh
  AdvancedAFN.unit.cc
  AirflowNetworkBalanceManager.unit.cc
  AirflowNetworkSolver.unit.cc
  AirTerminalSingleDuct.unit.cc
  AirTerminalSingleDuctPIUReheat.unit.cc
  BranchNodeConnections.unit.cc
  ChillerElectricEIR.unit.cc
  ChillerIndirectAbsorption.unit.cc
  CondenserLoopTowers.unit.cc
  ConstructionInternalSource.unit.cc
  ConvectionCoefficients.unit.cc
  DataPlant.unit.cc
  Datasets.unit.cc
  DataSurfaces.unit.cc
  DataZoneEquipment.unit.cc
  DemandResponse.unit.cc
  DesiccantDehumidifiers.unit.cc
  DOASEffectOnZoneSizing.unit.cc
  DXCoils.unit.cc
  ElectricPowerServiceManager.unit.cc
  EMSManager.unit.cc
  EvaporativeCoolers.unit.cc
  ExteriorEnergyUse.unit.cc
  FanCoilUnits.unit.cc
  Fans.unit.cc
  FaultsManager.unit.cc
  FiniteDifferenceGroundTemperatureModel.unit.cc
  FluidCoolers.unit.cc
  FluidProperties.unit.cc
  Furnaces.unit.cc
  General.unit.cc
  GroundHeatExchangers.unit.cc
  HeatBalanceManager.unit.cc
  HeatBalFiniteDiffManager.unit.cc
  HeatPumpWaterToWaterSimple.unit.cc
  HeatRecovery.unit.cc
  Humidifiers.unit.cc
  HVACControllers.unit.cc
  HVACFourPipeBeam.unit.cc
  HVACMultiSpeedHeatPump.unit.cc
  HVACSizingSimulationManager.unit.cc
  HVACStandaloneERV.unit.cc
  HVACUnitarySystem.unit.cc
  HVACVariableRefrigerantFlow.unit.cc
  HWBaseboardRadiator.unit.cc
  ICSCollector.unit.cc
  IdfParser.unit.cc
  InputProcessor.unit.cc
  KusudaAchenbachGroundTemperatureModel.unit.cc
  LowTempRadiantSystem.unit.cc
  MixedAir.unit.cc
  MixerComponent.unit.cc
  NodeInputManager.unit.cc
  PlantHeatExchangerFluidToFluid.unit.cc
  PlantUtilities.unit.cc
  Pumps.unit.cc
  PurchasedAirManager.unit.cc
  OutdoorAirUnit.unit.cc
  OutputProcessor.unit.cc
  OutputReportData.unit.cc
  OutputReportTabular.unit.cc
  OutputReportTabularAnnual.unit.cc
  PierceSurface.unit.cc
  ReportSizingManager.unit.cc
  RoomAirflowNetwork.unit.cc
  RoomAirModelUserTempPattern.unit.cc
  ScheduleManager.unit.cc
  SecondaryDXCoils.unit.cc
  SetPointManager.unit.cc
  SimAirServingZones.unit.cc
  SingleDuct.unit.cc
  SiteBuildingSurfaceGroundTemperatures.unit.cc
  SiteDeepGroundTemperatures.unit.cc
  SiteFCFactorMethodGroundTemperatures.unit.cc
  SiteGroundDomain.unit.cc
  SiteShallowGroundTemperatures.unit.cc
  SizingAnalysisObjects.unit.cc
  SizingManager.unit.cc
  SolarShading.unit.cc
  SortAndStringUtilities.unit.cc
  SQLite.unit.cc
  SurfaceGeometry.unit.cc
  SurfaceOctree.unit.cc
  SwimmingPool.unit.cc
  ThermalComfort.unit.cc
  UnitHeater.unit.cc
  VariableSpeedCoils.unit.cc
  Vectors.unit.cc
  Vector.unit.cc
  VentilatedSlab.unit.cc
  WaterCoils.unit.cc
  WaterThermalTanks.unit.cc
  WaterToAirHeatPumpSimple.unit.cc
  WeatherManager.unit.cc
  XingGroundTemperatureModel.unit.cc
  ZoneEquipmentManager.unit.cc
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
