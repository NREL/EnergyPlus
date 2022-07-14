// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include <EnergyPlus/Data/CommonIncludes.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>

#include <memory>

namespace EnergyPlus {

EnergyPlusData::EnergyPlusData()
{
    this->dataAirLoop = std::make_unique<DataAirLoopData>();
    this->dataAirLoopHVACDOAS = std::make_unique<AirLoopHVACDOASData>();
    this->dataAirSystemsData = std::make_unique<AirSystemsData>();
    this->afn = std::make_unique<AirflowNetwork::Solver>(*this);
    this->dataBSDFWindow = std::make_unique<BSDFWindowData>();
    this->dataBaseSizerFanHeatInputs = std::make_unique<BaseSizerWithFanHeatInputsData>();
    this->dataBaseSizerScalableInputs = std::make_unique<BaseSizerWithScalableInputsData>();
    this->dataBaseboardElectric = std::make_unique<BaseboardElectricData>();
    this->dataBaseboardRadiator = std::make_unique<BaseboardRadiatorData>();
    this->dataBoilerSteam = std::make_unique<BoilerSteamData>();
    this->dataBoilers = std::make_unique<BoilersData>();
    this->dataBranchAirLoopPlant = std::make_unique<DataBranchAirLoopPlantData>();
    this->dataBranchInputManager = std::make_unique<BranchInputManagerData>();
    this->dataBranchNodeConnections = std::make_unique<BranchNodeConnectionsData>();
    this->dataCHPElectGen = std::make_unique<MicroCHPElectricGeneratorData>();
    this->dataCTElectricGenerator = std::make_unique<CTElectricGeneratorData>();
    this->dataChilledCeilingPanelSimple = std::make_unique<ChilledCeilingPanelSimpleData>();
    this->dataChillerAbsorber = std::make_unique<ChillerAbsorberData>();
    this->dataChillerElectricEIR = std::make_unique<ChillerElectricEIRData>();
    this->dataChillerExhaustAbsorption = std::make_unique<ChillerExhaustAbsorptionData>();
    this->dataChillerGasAbsorption = std::make_unique<ChillerGasAbsorptionData>();
    this->dataChillerIndirectAbsorption = std::make_unique<ChillerIndirectAbsoprtionData>();
    this->dataChillerReformulatedEIR = std::make_unique<ChillerReformulatedEIRData>();
    this->dataCoilCooingDX = std::make_unique<CoilCoolingDXData>();
    this->dataCondenserLoopTowers = std::make_unique<CondenserLoopTowersData>();
    this->dataConstruction = std::make_unique<ConstructionData>();
    this->dataContaminantBalance = std::make_unique<ContaminantBalanceData>();
    this->dataConvectionCoefficient = std::make_unique<ConvectionCoefficientsData>();
    this->dataConvergeParams = std::make_unique<ConvergParamsData>();
    this->dataCoolTower = std::make_unique<CoolTowerData>();
    this->dataCostEstimateManager = std::make_unique<CostEstimateManagerData>();
    this->dataCrossVentMgr = std::make_unique<CrossVentMgrData>();
    this->dataCurveManager = std::make_unique<CurveManagerData>();
    this->dataDXCoils = std::make_unique<DXCoilsData>();
    this->dataDXFEarClipping = std::make_unique<DXFEarClippingData>();
    this->dataDaylightingData = std::make_unique<DaylightingData>();
    this->dataDaylightingDevices = std::make_unique<DaylightingDevicesData>();
    this->dataDaylightingDevicesData = std::make_unique<DataDaylightingDevicesData>();
    this->dataDaylightingManager = std::make_unique<DaylightingManagerData>();
    this->dataDefineEquipment = std::make_unique<DefineEquipData>();
    this->dataDemandManager = std::make_unique<DemandManagerData>();
    this->dataDesiccantDehumidifiers = std::make_unique<DesiccantDehumidifiersData>();
    this->dataDispVentMgr = std::make_unique<DisplacementVentMgrData>();
    this->dataDualDuct = std::make_unique<DualDuctData>();
    this->dataEIRPlantLoopHeatPump = std::make_unique<EIRPlantLoopHeatPumpsData>();
    this->dataEMSMgr = std::make_unique<EMSManagerData>();
    this->dataEarthTube = std::make_unique<EarthTubeData>();
    this->dataEcoRoofMgr = std::make_unique<EcoRoofManagerData>();
    this->dataEconLifeCycleCost = std::make_unique<EconomicLifeCycleCostData>();
    this->dataEconTariff = std::make_unique<EconomicTariffData>();
    this->dataElectBaseboardRad = std::make_unique<ElectricBaseboardRadiatorData>();
    this->dataElectPwrSvcMgr = std::make_unique<ElectPwrSvcMgrData>();
    this->dataEnvrn = std::make_unique<EnvironmentData>();
    this->dataErrTracking = std::make_unique<ErrorTrackingData>();
    this->dataEvapCoolers = std::make_unique<EvaporativeCoolersData>();
    this->dataEvapFluidCoolers = std::make_unique<EvaporativeFluidCoolersData>();
    this->dataExteriorEnergyUse = std::make_unique<ExteriorEnergyUseData>();
    this->dataExternalInterface = std::make_unique<ExternalInterfaceData>();
    this->dataFanCoilUnits = std::make_unique<FanCoilUnitsData>();
    this->dataFans = std::make_unique<FansData>();
    this->dataFaultsMgr = std::make_unique<FaultsManagerData>();
    this->dataFluidCoolers = std::make_unique<FluidCoolersData>();
    this->dataFluidProps = std::make_unique<FluidPropertiesData>();
    this->dataFourPipeBeam = std::make_unique<FourPipeBeamData>();
    this->dataFuelCellElectGen = std::make_unique<FuelCellElectricGeneratorData>();
    this->dataFurnaces = std::make_unique<FurnacesData>();
    this->dataGeneral = std::make_unique<GeneralData>();
    this->dataGeneralRoutines = std::make_unique<GeneralRoutinesData>();
    this->dataGenerator = std::make_unique<GeneratorsData>();
    this->dataGeneratorFuelSupply = std::make_unique<GeneratorFuelSupplyData>();
    this->dataGlobal = std::make_unique<DataGlobal>();
    this->dataGlobalConst = std::make_unique<DataGlobalConstantsData>();
    this->dataGlobalNames = std::make_unique<GlobalNamesData>();
    this->dataGrndTempModelMgr = std::make_unique<GroundTemperatureManagerData>();
    this->dataGroundHeatExchanger = std::make_unique<GroundHeatExchangerData>();
    this->dataHPWaterToWaterClg = std::make_unique<HeatPumpWaterToWaterCOOLINGData>();
    this->dataHPWaterToWaterHtg = std::make_unique<HeatPumpWaterToWaterHEATINGData>();
    this->dataHPWaterToWaterSimple = std::make_unique<HeatPumpWaterToWaterSimpleData>();
    this->dataHVACAssistedCC = std::make_unique<HVACHXAssistedCoolingCoilData>();
    this->dataHVACControllers = std::make_unique<HVACControllersData>();
    this->dataHVACCooledBeam = std::make_unique<HVACCooledBeamData>();
    this->dataHVACCtrl = std::make_unique<HVACCtrlData>();
    this->dataHVACDXHeatPumpSys = std::make_unique<HVACDXHeatPumpSystemData>();
    this->dataHVACDuct = std::make_unique<HVACDuctData>();
    this->dataHVACFan = std::make_unique<HVACFanData>();
    this->dataHVACGlobal = std::make_unique<HVACGlobalsData>();
    this->dataHVACInterfaceMgr = std::make_unique<HVACInterfaceManagerData>();
    this->dataHVACMgr = std::make_unique<HVACManagerData>();
    this->dataHVACMultiSpdHP = std::make_unique<HVACMultiSpeedHeatPumpData>();
    this->dataHVACSingleDuctInduc = std::make_unique<HVACSingleDuctInducData>();
    this->dataHVACSizingSimMgr = std::make_unique<HVACSizingSimMgrData>();
    this->dataHVACStandAloneERV = std::make_unique<HVACStandAloneERVData>();
    this->dataHVACUnitaryBypassVAV = std::make_unique<HVACUnitaryBypassVAVData>();
    this->dataHVACVarRefFlow = std::make_unique<HVACVarRefFlowData>();
    this->dataHWBaseboardRad = std::make_unique<HWBaseboardRadiatorData>();
    this->dataHeatBal = std::make_unique<HeatBalanceData>();
    this->dataHeatBalAirMgr = std::make_unique<HeatBalanceAirMgrData>();
    this->dataHeatBalFanSys = std::make_unique<HeatBalFanSysData>();
    this->dataHeatBalFiniteDiffMgr = std::make_unique<HeatBalFiniteDiffMgr>();
    this->dataHeatBalHAMTMgr = std::make_unique<HeatBalHAMTMgrData>();
    this->dataHeatBalIntHeatGains = std::make_unique<HeatBalInternalHeatGainsData>();
    this->dataHeatBalIntRadExchg = std::make_unique<HeatBalanceIntRadExchgData>();
    this->dataHeatBalMgr = std::make_unique<HeatBalanceMgrData>();
    this->dataHeatBalSurf = std::make_unique<HeatBalSurfData>();
    this->dataHeatBalSurfMgr = std::make_unique<HeatBalSurfMgr>();
    this->dataHeatRecovery = std::make_unique<HeatRecoveryData>();
    this->dataHeatingCoils = std::make_unique<HeatingCoilsData>();
    this->dataHighTempRadSys = std::make_unique<HighTempRadiantSystemData>();
    this->dataHumidifiers = std::make_unique<HumidifiersData>();
    this->dataHybridModel = std::make_unique<HybridModelData>();
    this->dataHybridUnitaryAC = std::make_unique<HybridUnitaryAirConditionersData>();
    this->dataHysteresisPhaseChange = std::make_unique<HysteresisPhaseChangeData>();
    this->dataICEngElectGen = std::make_unique<ICEngineElectricGeneratorData>();
    this->dataInputProcessing = std::make_unique<DataInputProcessing>();
    this->dataIPShortCut = std::make_unique<IPShortCutsData>();
    this->dataIceThermalStorage = std::make_unique<IceThermalStorageData>();
    this->dataIntegratedHP = std::make_unique<IntegratedHeatPumpGlobalData>();
    this->dataInternalHeatGains = std::make_unique<InternalHeatGainsData>();
    this->dataLoopNodes = std::make_unique<LoopNodeData>();
    this->dataLowTempRadSys = std::make_unique<LowTempRadiantSystemData>();
    this->dataMaterial = std::make_unique<MaterialData>();
    this->dataMatrixDataManager = std::make_unique<MatrixDataManagerData>();
    this->dataMircoturbElectGen = std::make_unique<MicroturbineElectricGeneratorData>();
    this->dataMixedAir = std::make_unique<MixedAirData>();
    this->dataMixerComponent = std::make_unique<MixerComponentData>();
    this->dataMoistureBalEMPD = std::make_unique<MoistureBalanceEMPDManagerData>();
    this->dataMstBal = std::make_unique<MoistureBalanceData>();
    this->dataMstBalEMPD = std::make_unique<MoistureBalanceEMPDData>();
    this->dataMundtSimMgr = std::make_unique<MundtSimMgrData>();
    this->dataNodeInputMgr = std::make_unique<NodeInputManagerData>();
    this->dataOutAirNodeMgr = std::make_unique<OutAirNodeManagerData>();
    this->dataOutRptPredefined = std::make_unique<OutputReportPredefinedData>();
    this->dataOutRptTab = std::make_unique<OutputReportTabularData>();
    this->dataOutdoorAirUnit = std::make_unique<OutdoorAirUnitData>();
    this->dataOutput = std::make_unique<OutputsData>();
    this->dataOutputProcessor = std::make_unique<OutputProcessorData>();
    this->dataOutputReportTabularAnnual = std::make_unique<OutputReportTabularAnnualData>();
    this->dataOutputReports = std::make_unique<OutputReportsData>();
    this->dataOutsideEnergySrcs = std::make_unique<OutsideEnergySourcesData>();
    this->dataPTHP = std::make_unique<PackagedTerminalHeatPumpData>();
    this->dataPackagedThermalStorageCoil = std::make_unique<PackagedThermalStorageCoilData>();
    this->dataPhotovoltaic = std::make_unique<PhotovoltaicsData>();
    this->dataPhotovoltaicState = std::make_unique<PhotovoltaicStateData>();
    this->dataPhotovoltaicThermalCollector = std::make_unique<PhotovoltaicThermalCollectorsData>();
    this->dataPipeHT = std::make_unique<PipeHeatTransferData>();
    this->dataPipes = std::make_unique<PipesData>();
    this->dataPlantCentralGSHP = std::make_unique<PlantCentralGSHPData>();
    this->dataPlantChillers = std::make_unique<PlantChillersData>();
    this->dataPlantCompTempSrc = std::make_unique<PlantCompTempSrcData>();
    this->dataPlantCondLoopOp = std::make_unique<PlantCondLoopOperationData>();
    this->dataPlantHXFluidToFluid = std::make_unique<PlantHeatExchangerFluidToFluidData>();
    this->dataPlantLoadProfile = std::make_unique<PlantLoadProfileData>();
    this->dataPlantMgr = std::make_unique<PlantMgrData>();
    this->dataPlantPipingSysMgr = std::make_unique<PlantPipingSysMgrData>();
    this->dataPlantPressureSys = std::make_unique<PlantPressureSysData>();
    this->dataPlantUtilities = std::make_unique<PlantUtilitiesData>();
    this->dataPlantValves = std::make_unique<PlantValvesData>();
    this->dataPlnt = std::make_unique<DataPlantData>();
    this->dataPluginManager = std::make_unique<PluginManagerData>();
    this->dataPollutionModule = std::make_unique<PollutionModuleData>();
    this->dataPondGHE = std::make_unique<PondGroundHeatExchangerData>();
    this->dataPowerInductionUnits = std::make_unique<PoweredInductionUnitsData>();
    this->dataPsychrometrics = std::make_unique<PsychrometricsData>();
    this->dataPsychCache = std::make_unique<PsychrometricCacheData>();
    this->dataPumps = std::make_unique<PumpsData>();
    this->dataPurchasedAirMgr = std::make_unique<PurchasedAirManagerData>();
    this->dataRefrigCase = std::make_unique<RefrigeratedCaseData>();
    this->dataReportFlag = std::make_unique<ReportFlagData>();
    this->dataResultsFramework = std::make_unique<ResultsFrameworkData>();
    this->dataRetAirPathMrg = std::make_unique<ReturnAirPathMgr>();
    this->dataExhAirSystemMrg = std::make_unique<ExhaustAirSystemMgr>();
    this->dataExhCtrlSystemMrg = std::make_unique<ExhaustControlSystemMgr>();
    this->dataRoomAirMod = std::make_unique<RoomAirModelData>();
    this->dataRoomAirModelMgr = std::make_unique<RoomAirModelManagerData>();
    this->dataRoomAirModelTempPattern = std::make_unique<RoomAirModelUserTempPatternData>();
    this->dataRoomAirflowNetModel = std::make_unique<RoomAirModelAirflowNetworkData>();
    this->dataRootFinder = std::make_unique<RootFindingData>();
    this->dataRptCoilSelection = std::make_unique<ReportCoilSelectionData>();
    this->dataRuntimeLang = std::make_unique<RuntimeLanguageData>();
    this->dataRuntimeLangProcessor = std::make_unique<RuntimeLanguageProcessorData>();
    this->dataSQLiteProcedures = std::make_unique<SQLiteProceduresData>();
    this->dataScheduleMgr = std::make_unique<ScheduleManagerData>();
    this->dataSetPointManager = std::make_unique<SetPointManagerData>();
    this->dataShadowComb = std::make_unique<ShadowCombData>();
    this->dataSimAirServingZones = std::make_unique<SimAirServingZonesData>();
    this->dataSimulationManager = std::make_unique<SimulationManagerData>();
    this->dataSingleDuct = std::make_unique<SingleDuctData>();
    this->dataSize = std::make_unique<SizingData>();
    this->dataSizingManager = std::make_unique<SizingManagerData>();
    this->dataSolarCollectors = std::make_unique<SolarCollectorsData>();
    this->dataSolarReflectionManager = std::make_unique<SolarReflectionManagerData>();
    this->dataSolarShading = std::make_unique<SolarShadingData>();
    this->dataSplitterComponent = std::make_unique<SplitterComponentData>();
    this->dataSteamBaseboardRadiator = std::make_unique<SteamBaseboardRadiatorData>();
    this->dataSteamCoils = std::make_unique<SteamCoilsData>();
    this->dataStrGlobals = std::make_unique<DataStringGlobalsData>();
    this->dataSurfColor = std::make_unique<SurfaceColorData>();
    this->dataSurfLists = std::make_unique<SurfaceListsData>();
    this->dataSurface = std::make_unique<SurfacesData>();
    this->dataSurfaceGeometry = std::make_unique<SurfaceGeometryData>();
    this->dataSurfaceGroundHeatExchangers = std::make_unique<SurfaceGroundHeatExchangersData>();
    this->dataSwimmingPools = std::make_unique<SwimmingPoolsData>();
    this->dataSysAirFlowSizer = std::make_unique<SystemAirFlowSizerData>();
    this->dataSysRpts = std::make_unique<SystemReportsData>();
    this->dataSysVars = std::make_unique<SystemVarsData>();
    this->dataSystemAvailabilityManager = std::make_unique<SystemAvailabilityManagerData>();
    this->dataTARCOGCommon = std::make_unique<TARCOGCommonData>();
    this->dataTARCOGOutputs = std::make_unique<TARCOGOutputData>();
    this->dataThermalChimneys = std::make_unique<ThermalChimneysData>();
    this->dataThermalComforts = std::make_unique<ThermalComfortsData>();
    this->dataThermalISO15099Calc = std::make_unique<ThermalISO15099CalcData>();
    this->dataTARCOGGasses90 = std::make_unique<TARCOGGasses90Data>();
    this->dataTARCOGMain = std::make_unique<TARCOGMainData>();
    this->dataTarcogShading = std::make_unique<TarcogShadingData>();
    this->dataTimingsData = std::make_unique<DataTimingsData>();
    this->dataTranspiredCollector = std::make_unique<TranspiredCollectorData>();
    this->dataUCSDShared = std::make_unique<UCSDSharedData>();
    this->dataUFADManager = std::make_unique<UFADManagerData>();
    this->dataUnitHeaters = std::make_unique<UnitHeatersData>();
    this->dataUnitVentilators = std::make_unique<UnitVentilatorsData>();
    this->dataUnitarySystems = std::make_unique<UnitarySystemsData>();
    this->dataUserDefinedComponents = std::make_unique<UserDefinedComponentsData>();
    this->dataUtilityRoutines = std::make_unique<UtilityRoutinesData>();
    this->dataVariableSpeedCoils = std::make_unique<VariableSpeedCoilsData>();
    this->dataVectors = std::make_unique<VectorsData>();
    this->dataVentilatedSlab = std::make_unique<VentilatedSlabData>();
    this->dataViewFactor = std::make_unique<ViewFactorInfoData>();
    this->dataWaterCoils = std::make_unique<WaterCoilsData>();
    this->dataWaterData = std::make_unique<DataWaterData>();
    this->dataWaterManager = std::make_unique<WaterManagerData>();
    this->dataWaterThermalTanks = std::make_unique<WaterThermalTanksData>();
    this->dataWaterToAirHeatPump = std::make_unique<WaterToAirHeatPumpData>();
    this->dataWaterToAirHeatPumpSimple = std::make_unique<WaterToAirHeatPumpSimpleData>();
    this->dataWaterUse = std::make_unique<WaterUseData>();
    this->dataWeatherManager = std::make_unique<WeatherManagerData>();
    this->dataWindTurbine = std::make_unique<WindTurbineData>();
    this->dataWindowAC = std::make_unique<WindowACData>();
    this->dataWindowComplexManager = std::make_unique<WindowComplexManagerData>();
    this->dataWindowEquivLayer = std::make_unique<WindowEquivLayerData>();
    this->dataWindowEquivalentLayer = std::make_unique<WindowEquivalentLayerData>();
    this->dataWindowManager = std::make_unique<WindowManagerData>();
    this->dataZoneAirLoopEquipmentManager = std::make_unique<ZoneAirLoopEquipmentManagerData>();
    this->dataZoneContaminantPredictorCorrector = std::make_unique<ZoneContaminantPredictorCorrectorData>();
    this->dataZoneCtrls = std::make_unique<DataZoneControlsData>();
    this->dataZoneDehumidifier = std::make_unique<ZoneDehumidifierData>();
    this->dataZoneEnergyDemand = std::make_unique<DataZoneEnergyDemandsData>();
    this->dataZoneEquip = std::make_unique<DataZoneEquipmentData>();
    this->dataZoneEquipmentManager = std::make_unique<ZoneEquipmentManagerData>();
    this->dataZonePlenum = std::make_unique<ZonePlenumData>();
    this->dataZoneTempPredictorCorrector = std::make_unique<ZoneTempPredictorCorrectorData>();
}

EnergyPlusData::~EnergyPlusData() = default;

void EnergyPlusData::clear_state()
{
    this->ready = true;
    this->dataAirLoop->clear_state();
    this->dataAirLoopHVACDOAS->clear_state();
    this->dataAirSystemsData->clear_state();
    this->afn->clear_state();
    this->dataBSDFWindow->clear_state();
    this->dataBaseSizerFanHeatInputs->clear_state();
    this->dataBaseSizerScalableInputs->clear_state();
    this->dataBaseboardElectric->clear_state();
    this->dataBaseboardRadiator->clear_state();
    this->dataBoilerSteam->clear_state();
    this->dataBoilers->clear_state();
    this->dataBranchAirLoopPlant->clear_state();
    this->dataBranchInputManager->clear_state();
    this->dataBranchNodeConnections->clear_state();
    this->dataCHPElectGen->clear_state();
    this->dataCTElectricGenerator->clear_state();
    this->dataChilledCeilingPanelSimple->clear_state();
    this->dataChillerAbsorber->clear_state();
    this->dataChillerElectricEIR->clear_state();
    this->dataChillerExhaustAbsorption->clear_state();
    this->dataChillerGasAbsorption->clear_state();
    this->dataChillerIndirectAbsorption->clear_state();
    this->dataChillerReformulatedEIR->clear_state();
    this->dataCoilCooingDX->clear_state();
    this->dataCondenserLoopTowers->clear_state();
    this->dataConstruction->clear_state();
    this->dataContaminantBalance->clear_state();
    this->dataConvectionCoefficient->clear_state();
    this->dataConvergeParams->clear_state();
    this->dataCoolTower->clear_state();
    this->dataCostEstimateManager->clear_state();
    this->dataCrossVentMgr->clear_state();
    this->dataCurveManager->clear_state();
    this->dataDXCoils->clear_state();
    this->dataDXFEarClipping->clear_state();
    this->dataDaylightingData->clear_state();
    this->dataDaylightingDevices->clear_state();
    this->dataDaylightingDevicesData->clear_state();
    this->dataDaylightingManager->clear_state();
    this->dataDefineEquipment->clear_state();
    this->dataDemandManager->clear_state();
    this->dataDesiccantDehumidifiers->clear_state();
    this->dataDispVentMgr->clear_state();
    this->dataDualDuct->clear_state();
    this->dataEIRPlantLoopHeatPump->clear_state();
    this->dataEMSMgr->clear_state();
    this->dataEarthTube->clear_state();
    this->dataEcoRoofMgr->clear_state();
    this->dataEconLifeCycleCost->clear_state();
    this->dataEconTariff->clear_state();
    this->dataElectBaseboardRad->clear_state();
    this->dataElectPwrSvcMgr->clear_state();
    this->dataEnvrn->clear_state();
    this->dataErrTracking->clear_state();
    this->dataEvapCoolers->clear_state();
    this->dataEvapFluidCoolers->clear_state();
    this->dataExteriorEnergyUse->clear_state();
    this->dataExternalInterface->clear_state();
    this->dataFanCoilUnits->clear_state();
    this->dataFans->clear_state();
    this->dataFaultsMgr->clear_state();
    this->dataFluidCoolers->clear_state();
    this->dataFluidProps->clear_state();
    this->dataFourPipeBeam->clear_state();
    this->dataFuelCellElectGen->clear_state();
    this->dataFurnaces->clear_state();
    this->dataGeneral->clear_state();
    this->dataGeneralRoutines->clear_state();
    this->dataGenerator->clear_state();
    this->dataGeneratorFuelSupply->clear_state();
    this->dataGlobal->clear_state();
    this->dataGlobalConst->clear_state();
    this->dataGlobalNames->clear_state();
    this->dataGrndTempModelMgr->clear_state();
    this->dataGroundHeatExchanger->clear_state();
    this->dataHPWaterToWaterClg->clear_state();
    this->dataHPWaterToWaterHtg->clear_state();
    this->dataHPWaterToWaterSimple->clear_state();
    this->dataHVACAssistedCC->clear_state();
    this->dataHVACControllers->clear_state();
    this->dataHVACCooledBeam->clear_state();
    this->dataHVACCtrl->clear_state();
    this->dataHVACDXHeatPumpSys->clear_state();
    this->dataHVACDuct->clear_state();
    this->dataHVACFan->clear_state();
    this->dataHVACGlobal->clear_state();
    this->dataHVACInterfaceMgr->clear_state();
    this->dataHVACMgr->clear_state();
    this->dataHVACMultiSpdHP->clear_state();
    this->dataHVACSingleDuctInduc->clear_state();
    this->dataHVACSizingSimMgr->clear_state();
    this->dataHVACStandAloneERV->clear_state();
    this->dataHVACUnitaryBypassVAV->clear_state();
    this->dataHVACVarRefFlow->clear_state();
    this->dataHWBaseboardRad->clear_state();
    this->dataHeatBal->clear_state();
    this->dataHeatBalAirMgr->clear_state();
    this->dataHeatBalFanSys->clear_state();
    this->dataHeatBalFiniteDiffMgr->clear_state();
    this->dataHeatBalHAMTMgr->clear_state();
    this->dataHeatBalIntHeatGains->clear_state();
    this->dataHeatBalIntRadExchg->clear_state();
    this->dataHeatBalMgr->clear_state();
    this->dataHeatBalSurf->clear_state();
    this->dataHeatBalSurfMgr->clear_state();
    this->dataHeatRecovery->clear_state();
    this->dataHeatingCoils->clear_state();
    this->dataHighTempRadSys->clear_state();
    this->dataHumidifiers->clear_state();
    this->dataHybridModel->clear_state();
    this->dataHybridUnitaryAC->clear_state();
    this->dataHysteresisPhaseChange->clear_state();
    this->dataICEngElectGen->clear_state();
    this->dataIPShortCut->clear_state();
    this->dataIceThermalStorage->clear_state();
    this->dataInputProcessing->clear_state();
    this->dataIntegratedHP->clear_state();
    this->dataInternalHeatGains->clear_state();
    this->dataLoopNodes->clear_state();
    this->dataLowTempRadSys->clear_state();
    this->dataMaterial->clear_state();
    this->dataMatrixDataManager->clear_state();
    this->dataMircoturbElectGen->clear_state();
    this->dataMixedAir->clear_state();
    this->dataMixerComponent->clear_state();
    this->dataMoistureBalEMPD->clear_state();
    this->dataMstBal->clear_state();
    this->dataMstBalEMPD->clear_state();
    this->dataMundtSimMgr->clear_state();
    this->dataNodeInputMgr->clear_state();
    this->dataOutAirNodeMgr->clear_state();
    this->dataOutRptPredefined->clear_state();
    this->dataOutRptTab->clear_state();
    this->dataOutdoorAirUnit->clear_state();
    this->dataOutput->clear_state();
    this->dataOutputProcessor->clear_state();
    this->dataOutputReportTabularAnnual->clear_state();
    this->dataOutputReports->clear_state();
    this->dataOutsideEnergySrcs->clear_state();
    this->dataPTHP->clear_state();
    this->dataPackagedThermalStorageCoil->clear_state();
    this->dataPhotovoltaic->clear_state();
    this->dataPhotovoltaicState->clear_state();
    this->dataPhotovoltaicThermalCollector->clear_state();
    this->dataPipeHT->clear_state();
    this->dataPipes->clear_state();
    this->dataPlantCentralGSHP->clear_state();
    this->dataPlantChillers->clear_state();
    this->dataPlantCompTempSrc->clear_state();
    this->dataPlantCondLoopOp->clear_state();
    this->dataPlantHXFluidToFluid->clear_state();
    this->dataPlantLoadProfile->clear_state();
    this->dataPlantMgr->clear_state();
    this->dataPlantPipingSysMgr->clear_state();
    this->dataPlantPressureSys->clear_state();
    this->dataPlantUtilities->clear_state();
    this->dataPlantValves->clear_state();
    this->dataPlnt->clear_state();
    this->dataPluginManager->clear_state();
    this->dataPollutionModule->clear_state();
    this->dataPondGHE->clear_state();
    this->dataPowerInductionUnits->clear_state();
    this->dataPsychrometrics->clear_state();
    this->dataPsychCache->clear_state();
    this->dataPumps->clear_state();
    this->dataPurchasedAirMgr->clear_state();
    this->dataRefrigCase->clear_state();
    this->dataReportFlag->clear_state();
    this->dataResultsFramework->clear_state();
    this->dataRetAirPathMrg->clear_state();
    this->dataExhAirSystemMrg->clear_state();
    this->dataExhCtrlSystemMrg->clear_state();
    this->dataRoomAirMod->clear_state();
    this->dataRoomAirModelMgr->clear_state();
    this->dataRoomAirModelTempPattern->clear_state();
    this->dataRoomAirflowNetModel->clear_state();
    this->dataRootFinder->clear_state();
    this->dataRptCoilSelection->clear_state();
    this->dataRuntimeLang->clear_state();
    this->dataRuntimeLangProcessor->clear_state();
    this->dataSQLiteProcedures->clear_state();
    this->dataScheduleMgr->clear_state();
    this->dataSetPointManager->clear_state();
    this->dataShadowComb->clear_state();
    this->dataSimAirServingZones->clear_state();
    this->dataSimulationManager->clear_state();
    this->dataSingleDuct->clear_state();
    this->dataSize->clear_state();
    this->dataSizingManager->clear_state();
    this->dataSolarCollectors->clear_state();
    this->dataSolarReflectionManager->clear_state();
    this->dataSolarShading->clear_state();
    this->dataSplitterComponent->clear_state();
    this->dataSteamBaseboardRadiator->clear_state();
    this->dataSteamCoils->clear_state();
    this->dataStrGlobals->clear_state();
    this->dataSurfColor->clear_state();
    this->dataSurfLists->clear_state();
    this->dataSurface->clear_state();
    this->dataSurfaceGeometry->clear_state();
    this->dataSurfaceGroundHeatExchangers->clear_state();
    this->dataSwimmingPools->clear_state();
    this->dataSysAirFlowSizer->clear_state();
    this->dataSysRpts->clear_state();
    this->dataSysVars->clear_state();
    this->dataSystemAvailabilityManager->clear_state();
    this->dataTARCOGCommon->clear_state();
    this->dataTARCOGOutputs->clear_state();
    this->dataThermalChimneys->clear_state();
    this->dataThermalComforts->clear_state();
    this->dataThermalISO15099Calc->clear_state();
    this->dataTARCOGGasses90->clear_state();
    this->dataTARCOGMain->clear_state();
    this->dataTarcogShading->clear_state();
    this->dataTimingsData->clear_state();
    this->dataTranspiredCollector->clear_state();
    this->dataUCSDShared->clear_state();
    this->dataUFADManager->clear_state();
    this->dataUnitHeaters->clear_state();
    this->dataUnitVentilators->clear_state();
    this->dataUnitarySystems->clear_state();
    this->dataUserDefinedComponents->clear_state();
    this->dataUtilityRoutines->clear_state();
    this->dataVariableSpeedCoils->clear_state();
    this->dataVectors->clear_state();
    this->dataVentilatedSlab->clear_state();
    this->dataViewFactor->clear_state();
    this->dataWaterCoils->clear_state();
    this->dataWaterData->clear_state();
    this->dataWaterManager->clear_state();
    this->dataWaterThermalTanks->clear_state();
    this->dataWaterToAirHeatPump->clear_state();
    this->dataWaterToAirHeatPumpSimple->clear_state();
    this->dataWaterUse->clear_state();
    this->dataWeatherManager->clear_state();
    this->dataWindTurbine->clear_state();
    this->dataWindowAC->clear_state();
    this->dataWindowComplexManager->clear_state();
    this->dataWindowEquivLayer->clear_state();
    this->dataWindowEquivalentLayer->clear_state();
    this->dataWindowManager->clear_state();
    this->dataZoneAirLoopEquipmentManager->clear_state();
    this->dataZoneContaminantPredictorCorrector->clear_state();
    this->dataZoneCtrls->clear_state();
    this->dataZoneDehumidifier->clear_state();
    this->dataZoneEnergyDemand->clear_state();
    this->dataZoneEquip->clear_state();
    this->dataZoneEquipmentManager->clear_state();
    this->dataZonePlenum->clear_state();
    this->dataZoneTempPredictorCorrector->clear_state();

    this->files.debug.close();
    this->files.err_stream.reset();
    this->files.eso.close();
    this->files.mtr.close();
    this->files.mtr.close();
    this->files.shade.close();
    this->files.ssz.close();
    this->files.zsz.close();
}
} // namespace EnergyPlus
