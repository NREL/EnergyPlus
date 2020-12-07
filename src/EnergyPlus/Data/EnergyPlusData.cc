// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/Data/CommonIncludes.hh>

#include <memory>

namespace EnergyPlus {

    EnergyPlusData::EnergyPlusData() {
        // todo, try to eliminate the need for the singleton
        IOFiles::setSingleton(&files);

        this->dataAirflowNetworkBalanceManager = std::make_unique<AirflowNetworkBalanceManagerData>();
        this->dataAirLoop = std::make_unique<DataAirLoopData>();
        this->dataAirLoopHVACDOAS = std::make_unique<AirLoopHVACDOASData>();
        this->dataAirSystemsData = std::make_unique<AirSystemsData>();
        this->dataBaseboardElectric = std::make_unique<BaseboardElectricData>();
        this->dataBaseboardRadiator = std::make_unique<BaseboardRadiatorData>();
        this->dataBoilers = std::make_unique<BoilersData>();
        this->dataBoilerSteam = std::make_unique<BoilerSteamData>();
        this->dataBranchAirLoopPlant = std::make_unique<DataBranchAirLoopPlantData>();
        this->dataBranchInputManager = std::make_unique<BranchInputManagerData>();
        this->dataBranchNodeConnections = std::make_unique<BranchNodeConnectionsData>();
        this->dataBSDFWindow = std::make_unique<BSDFWindowData>();
        this->dataChilledCeilingPanelSimple = std::make_unique<ChilledCeilingPanelSimpleData>();
        this->dataChillerAbsorber = std::make_unique<ChillerAbsorberData>();
        this->dataChillerElectricEIR = std::make_unique<ChillerElectricEIRData>();
        this->dataChillerExhaustAbsorption = std::make_unique<ChillerExhaustAbsorptionData>();
        this->dataChillerGasAbsorption = std::make_unique<ChillerGasAbsorptionData>();
        this->dataChillerIndirectAbsorption = std::make_unique<ChillerIndirectAbsoprtionData>();
        this->dataChillerReformulatedEIR = std::make_unique<ChillerReformulatedEIRData>();
        this->dataCondenserLoopTowers = std::make_unique<CondenserLoopTowersData>();
        this->dataConstruction = std::make_unique<ConstructionData>();
        this->dataContaminantBalance = std::make_unique<ContaminantBalanceData>();
        this->dataConvectionCoefficient = std::make_unique<ConvectionCoefficientsData>();
        this->dataConvergeParams = std::make_unique<ConvergParamsData>();
        this->dataCoolTower = std::make_unique<CoolTowerData>();
        this->dataCostEstimateManager = std::make_unique<CostEstimateManagerData>();
        this->dataCrossVentMgr = std::make_unique<CrossVentMgrData>();
        this->dataCTElectricGenerator = std::make_unique<CTElectricGeneratorData>();
        this->dataCurveManager = std::make_unique<CurveManagerData>();
        this->dataDaylightingData = std::make_unique<DaylightingData>();
        this->dataDaylightingDevicesData = std::make_unique<DataDaylightingDevicesData>();
        this->dataDaylightingDevices = std::make_unique<DaylightingDevicesData>();
        this->dataDaylightingManager = std::make_unique<DaylightingManagerData>();
        this->dataDefineEquipment = std::make_unique<DefineEquipData>();
        this->dataEIRPlantLoopHeatPump = std::make_unique<EIRPlantLoopHeatPumpsData>();
        this->dataEnvrn = std::make_unique<EnvironmentData>();
        this->dataExteriorEnergyUse = std::make_unique<ExteriorEnergyUseData>();
        this->dataFans = std::make_unique<FansData>();
        this->dataGlobal = std::make_unique<DataGlobal>();
        this->dataPipes = std::make_unique<PipesData>();
        this->dataPlantChillers = std::make_unique<PlantChillersData>();
        this->dataPlantValves = std::make_unique<PlantValvesData>();
        this->dataSetPointManager = std::make_unique<SetPointManagerData>();
        this->dataSimulationManager = std::make_unique<SimulationManagerData>();
        this->dataSingleDuct = std::make_unique<SingleDuctData>();
        this->dataSizingManager = std::make_unique<SizingManagerData>();
        this->dataSolarCollectors = std::make_unique<SolarCollectorsData>();
        this->dataSolarReflectionManager = std::make_unique<SolarReflectionManagerData>();
        this->dataSolarShading = std::make_unique<SolarShadingData>();
        this->dataSplitterComponent = std::make_unique<SplitterComponentData>();
        this->dataSteamBaseboardRadiator = std::make_unique<SteamBaseboardRadiatorData>();
        this->dataSteamCoils = std::make_unique<SteamCoilsData>();
        this->dataSurfaceGeometry = std::make_unique<SurfaceGeometryData>();
        this->dataSurfaceGroundHeatExchangers = std::make_unique<SurfaceGroundHeatExchangersData>();
        this->dataSwimmingPools = std::make_unique<SwimmingPoolsData>();
        this->dataSystemAvailabilityManager = std::make_unique<SystemAvailabilityManagerData>();
        this->dataThermalChimneys = std::make_unique<ThermalChimneysData>();
        this->dataThermalComforts = std::make_unique<ThermalComfortsData>();
        this->dataTranspiredCollector = std::make_unique<TranspiredCollectorData>();
        this->dataTimingsData = std::make_unique<DataTimingsData>();
        this->dataUFADManager = std::make_unique<UFADManagerData>();
        this->dataUnitarySystems = std::make_unique<UnitarySystemsData>();
        this->dataUnitHeaters = std::make_unique<UnitHeatersData>();
        this->dataUnitVentilators = std::make_unique<UnitVentilatorsData>();
        this->dataUserDefinedComponents = std::make_unique<UserDefinedComponentsData>();
        this->dataUtilityRoutines = std::make_unique<UtilityRoutinesData>();
        this->dataVariableSpeedCoils = std::make_unique<VariableSpeedCoilsData>();
        this->dataVentilatedSlab = std::make_unique<VentilatedSlabData>();
        this->dataWaterCoils = std::make_unique<WaterCoilsData>();
        this->dataWaterData = std::make_unique<DataWaterData>();
        this->dataWaterManager = std::make_unique<WaterManagerData>();
        this->dataWaterThermalTanks = std::make_unique<WaterThermalTanksData>();
        this->dataWaterToAirHeatPump = std::make_unique<WaterToAirHeatPumpData>();
        this->dataWaterToAirHeatPumpSimple = std::make_unique<WaterToAirHeatPumpSimpleData>();
        this->dataWaterUse = std::make_unique<WaterUseData>();
        this->dataWeatherManager = std::make_unique<WeatherManagerData>();
        this->dataWindowAC = std::make_unique<WindowACData>();
        this->dataWindowComplexManager = std::make_unique<WindowComplexManagerData>();
        this->dataWindowEquivalentLayer = std::make_unique<WindowEquivalentLayerData>();
        this->dataWindowManager = std::make_unique<WindowManagerData>();
        this->dataWindTurbine = std::make_unique<WindTurbineData>();
        this->dataZoneAirLoopEquipmentManager = std::make_unique<ZoneAirLoopEquipmentManagerData>();
        this->dataZoneContaminantPredictorCorrector = std::make_unique<ZoneContaminantPredictorCorrectorData>();
        this->dataZoneDehumidifier = std::make_unique<ZoneDehumidifierData>();
        this->dataZoneEquipmentManager = std::make_unique<ZoneEquipmentManagerData>();
        this->dataZonePlenum = std::make_unique<ZonePlenumData>();
        this->dataZoneTempPredictorCorrector = std::make_unique<ZoneTempPredictorCorrectorData>();
    }

    void EnergyPlusData::clear_state() {
        this->dataAirflowNetworkBalanceManager->clear_state();
        this->dataAirLoop->clear_state();
        this->dataAirLoopHVACDOAS->clear_state();
        this->dataAirSystemsData->clear_state();
        this->dataBaseboardElectric->clear_state();
        this->dataBaseboardRadiator->clear_state();
        this->dataBoilers->clear_state();
        this->dataBoilerSteam->clear_state();
        this->dataBranchAirLoopPlant->clear_state();
        this->dataBranchInputManager->clear_state();
        this->dataBranchNodeConnections->clear_state();
        this->dataBSDFWindow->clear_state();
        this->dataChilledCeilingPanelSimple->clear_state();
        this->dataChillerAbsorber->clear_state();
        this->dataChillerElectricEIR->clear_state();
        this->dataChillerExhaustAbsorption->clear_state();
        this->dataChillerGasAbsorption->clear_state();
        this->dataChillerIndirectAbsorption->clear_state();
        this->dataChillerReformulatedEIR->clear_state();
        this->dataCondenserLoopTowers->clear_state();
        this->dataConstruction->clear_state();
        this->dataContaminantBalance->clear_state();
        this->dataConvectionCoefficient->clear_state();
        this->dataConvergeParams->clear_state();
        this->dataCoolTower->clear_state();
        this->dataCostEstimateManager->clear_state();
        this->dataCrossVentMgr->clear_state();
        this->dataCTElectricGenerator->clear_state();
        this->dataCurveManager->clear_state();
        this->dataDaylightingData->clear_state();
        this->dataDaylightingDevicesData->clear_state();
        this->dataDaylightingDevices->clear_state();
        this->dataDaylightingManager->clear_state();
        this->dataDefineEquipment->clear_state();
        this->dataEIRPlantLoopHeatPump->clear_state();
        this->dataEnvrn->clear_state();
        this->dataExteriorEnergyUse->clear_state();
        this->dataFans->clear_state();
        this->dataGlobal->clear_state();
        this->dataPipes->clear_state();
        this->dataPlantChillers->clear_state();
        this->dataPlantValves->clear_state();
        this->dataSetPointManager->clear_state();
        this->dataSimulationManager->clear_state();
        this->dataSingleDuct->clear_state();
        this->dataSizingManager->clear_state();
        this->dataSolarCollectors->clear_state();
        this->dataSolarReflectionManager->clear_state();
        this->dataSolarShading->clear_state();
        this->dataSplitterComponent->clear_state();
        this->dataSteamBaseboardRadiator->clear_state();
        this->dataSteamCoils->clear_state();
        this->dataSurfaceGeometry->clear_state();
        this->dataSurfaceGroundHeatExchangers->clear_state();
        this->dataSwimmingPools->clear_state();
        this->dataSystemAvailabilityManager->clear_state();
        this->dataTimingsData->clear_state();
        this->dataThermalChimneys->clear_state();
        this->dataThermalComforts->clear_state();
        this->dataTranspiredCollector->clear_state();
        this->dataUFADManager->clear_state();
        this->dataUnitarySystems->clear_state();
        this->dataUnitHeaters->clear_state();
        this->dataUnitVentilators->clear_state();
        this->dataUserDefinedComponents->clear_state();
        this->dataUtilityRoutines->clear_state();
        this->dataVariableSpeedCoils->clear_state();
        this->dataVentilatedSlab->clear_state();
        this->dataWaterCoils->clear_state();
        this->dataWaterData->clear_state();
        this->dataWaterManager->clear_state();
        this->dataWaterThermalTanks->clear_state();
        this->dataWaterToAirHeatPump->clear_state();
        this->dataWaterToAirHeatPumpSimple->clear_state();
        this->dataWaterUse->clear_state();
        this->dataWeatherManager->clear_state();
        this->dataWindowAC->clear_state();
        this->dataWindowComplexManager->clear_state();
        this->dataWindowEquivalentLayer->clear_state();
        this->dataWindowManager->clear_state();
        this->dataWindTurbine->clear_state();
        this->dataZoneAirLoopEquipmentManager->clear_state();
        this->dataZoneContaminantPredictorCorrector->clear_state();
        this->dataZoneDehumidifier->clear_state();
        this->dataZoneEquipmentManager->clear_state();
        this->dataZonePlenum->clear_state();
        this->dataZoneTempPredictorCorrector->clear_state();

        this->files.eso.close();
        this->files.err_stream.reset();
        this->files.debug.close();
        this->files.zsz.close();
        this->files.ssz.close();
        this->files.mtr.close();
        this->files.shade.close();
        this->files.mtr.close();
        this->files.err_stream.reset();
    }
}
