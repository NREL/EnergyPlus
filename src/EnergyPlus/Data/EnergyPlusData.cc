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

        this->dataAirflowNetworkBalanceManager = std::unique_ptr<AirflowNetworkBalanceManagerData>(new AirflowNetworkBalanceManagerData);
        this->dataAirLoop = std::unique_ptr<DataAirLoopData>(new DataAirLoopData);
        this->dataAirLoopHVACDOAS = std::unique_ptr<AirLoopHVACDOASData>(new AirLoopHVACDOASData);
        this->dataBaseboardElectric = std::unique_ptr<BaseboardElectricData>(new BaseboardElectricData);
        this->dataBaseboardRadiator = std::unique_ptr<BaseboardRadiatorData>(new BaseboardRadiatorData);
        this->dataBoilers = std::unique_ptr<BoilersData>(new BoilersData);
        this->dataBoilerSteam = std::unique_ptr<BoilerSteamData>(new BoilerSteamData);
        this->dataBranchInputManager = std::unique_ptr<BranchInputManagerData>(new BranchInputManagerData);
        this->dataChilledCeilingPanelSimple = std::unique_ptr<ChilledCeilingPanelSimpleData>(new ChilledCeilingPanelSimpleData);
        this->dataChillerAbsorber = std::unique_ptr<ChillerAbsorberData>(new ChillerAbsorberData);
        this->dataChillerElectricEIR = std::unique_ptr<ChillerElectricEIRData>(new ChillerElectricEIRData);
        this->dataChillerExhaustAbsorption = std::unique_ptr<ChillerExhaustAbsorptionData>(new ChillerExhaustAbsorptionData);
        this->dataChillerGasAbsorption = std::unique_ptr<ChillerGasAbsorptionData>(new ChillerGasAbsorptionData);
        this->dataChillerIndirectAbsorption = std::unique_ptr<ChillerIndirectAbsoprtionData>(new ChillerIndirectAbsoprtionData);
        this->dataChillerReformulatedEIR = std::unique_ptr<ChillerReformulatedEIRData>(new ChillerReformulatedEIRData);
        this->dataCondenserLoopTowers = std::unique_ptr<CondenserLoopTowersData>(new CondenserLoopTowersData);
        this->dataConstruction = std::unique_ptr<ConstructionData>(new ConstructionData);
        this->dataConvectionCoefficient = std::unique_ptr<ConvectionCoefficientsData>(new ConvectionCoefficientsData);
        this->dataCoolTower = std::unique_ptr<CoolTowerData>(new CoolTowerData);
        this->dataCostEstimateManager = std::unique_ptr<CostEstimateManagerData>(new CostEstimateManagerData);
        this->dataCrossVentMgr = std::unique_ptr<CrossVentMgrData>(new CrossVentMgrData);
        this->dataCTElectricGenerator = std::unique_ptr<CTElectricGeneratorData>(new CTElectricGeneratorData);
        this->dataCurveManager = std::unique_ptr<CurveManagerData>(new CurveManagerData);
        this->dataExteriorEnergyUse = std::unique_ptr<ExteriorEnergyUseData>(new ExteriorEnergyUseData);
        this->dataFans = std::unique_ptr<FansData>(new FansData);
        this->dataGlobal = std::unique_ptr<DataGlobal>(new DataGlobal);
        this->dataPipes = std::unique_ptr<PipesData>(new PipesData);
        this->dataPlantChillers = std::unique_ptr<PlantChillersData>(new PlantChillersData);
        this->dataSurfaceGroundHeatExchangers = std::unique_ptr<SurfaceGroundHeatExchangersData>(new SurfaceGroundHeatExchangersData);
        this->dataSwimmingPools = std::unique_ptr<SwimmingPoolsData>(new SwimmingPoolsData);
        this->dataThermalChimneys = std::unique_ptr<ThermalChimneysData>(new ThermalChimneysData);
        this->dataThermalComforts = std::unique_ptr<ThermalComfortsData>(new ThermalComfortsData);
        this->dataTranspiredCollector = std::unique_ptr<TranspiredCollectorData>(new TranspiredCollectorData);
        this->dataUFADManager = std::unique_ptr<UFADManagerData>(new UFADManagerData);
        this->dataUnitarySystems = std::unique_ptr<UnitarySystemsData>(new UnitarySystemsData);
        this->dataUnitHeaters = std::unique_ptr<UnitHeatersData>(new UnitHeatersData);
        this->dataUnitVentilators = std::unique_ptr<UnitVentilatorsData>(new UnitVentilatorsData);
        this->dataUserDefinedComponents = std::unique_ptr<UserDefinedComponentsData>(new UserDefinedComponentsData);
        this->dataVariableSpeedCoils = std::unique_ptr<VariableSpeedCoilsData>(new VariableSpeedCoilsData);
        this->dataVentilatedSlab = std::unique_ptr<VentilatedSlabData>(new VentilatedSlabData);
        this->dataWaterCoils = std::unique_ptr<WaterCoilsData>(new WaterCoilsData);
        this->dataWaterManager = std::unique_ptr<WaterManagerData>(new WaterManagerData);
        this->dataWaterThermalTanks = std::unique_ptr<WaterThermalTanksData>(new WaterThermalTanksData);
        this->dataWaterToAirHeatPump = std::unique_ptr<WaterToAirHeatPumpData>(new WaterToAirHeatPumpData);
        this->dataWaterToAirHeatPumpSimple = std::unique_ptr<WaterToAirHeatPumpSimpleData>(new WaterToAirHeatPumpSimpleData);
        this->dataWaterUse = std::unique_ptr<WaterUseData>(new WaterUseData);
        this->dataWeatherManager = std::unique_ptr<WeatherManagerData>(new WeatherManagerData);
        this->dataWindowAC = std::unique_ptr<WindowACData>(new WindowACData);
        this->dataWindowComplexManager = std::unique_ptr<WindowComplexManagerData>(new WindowComplexManagerData);
        this->dataWindowEquivalentLayer = std::unique_ptr<WindowEquivalentLayerData>(new WindowEquivalentLayerData);
        this->dataWindowManager = std::unique_ptr<WindowManagerData>(new WindowManagerData);
        this->dataWindTurbine = std::unique_ptr<WindTurbineData>(new WindTurbineData);
        this->dataZoneAirLoopEquipmentManager = std::unique_ptr<ZoneAirLoopEquipmentManagerData>(new ZoneAirLoopEquipmentManagerData);
        this->dataZoneContaminantPredictorCorrector = std::unique_ptr<ZoneContaminantPredictorCorrectorData>(new ZoneContaminantPredictorCorrectorData);
        this->dataZoneDehumidifier = std::unique_ptr<ZoneDehumidifierData>(new ZoneDehumidifierData);
        this->dataZoneEquipmentManager = std::unique_ptr<ZoneEquipmentManagerData>(new ZoneEquipmentManagerData);
        this->dataZonePlenum = std::unique_ptr<ZonePlenumData>(new ZonePlenumData);
        this->dataZoneTempPredictorCorrector = std::unique_ptr<ZoneTempPredictorCorrectorData>(new ZoneTempPredictorCorrectorData);
    }

    void EnergyPlusData::clear_state() {
        this->dataAirflowNetworkBalanceManager->clear_state();
        this->dataAirLoop->clear_state();
        this->dataAirLoopHVACDOAS->clear_state();
        this->dataBaseboardElectric->clear_state();
        this->dataBaseboardRadiator->clear_state();
        this->dataBoilers->clear_state();
        this->dataBoilerSteam->clear_state();
        this->dataBranchInputManager->clear_state();
        this->dataChilledCeilingPanelSimple->clear_state();
        this->dataChillerAbsorber->clear_state();
        this->dataChillerElectricEIR->clear_state();
        this->dataChillerExhaustAbsorption->clear_state();
        this->dataChillerGasAbsorption->clear_state();
        this->dataChillerIndirectAbsorption->clear_state();
        this->dataChillerReformulatedEIR->clear_state();
        this->dataCondenserLoopTowers->clear_state();
        this->dataConstruction->clear_state();
        this->dataConvectionCoefficient->clear_state();
        this->dataCoolTower->clear_state();
        this->dataCostEstimateManager->clear_state();
        this->dataCrossVentMgr->clear_state();
        this->dataCTElectricGenerator->clear_state();
        this->dataCurveManager->clear_state();
        this->dataExteriorEnergyUse->clear_state();
        this->dataFans->clear_state();
        this->dataGlobal->clear_state();
        this->dataPipes->clear_state();
        this->dataPlantChillers->clear_state();
        this->dataSurfaceGroundHeatExchangers->clear_state();
        this->dataSwimmingPools->clear_state();
        this->dataThermalChimneys->clear_state();
        this->dataThermalComforts->clear_state();
        this->dataTranspiredCollector->clear_state();
        this->dataUFADManager->clear_state();
        this->dataUnitarySystems->clear_state();
        this->dataUnitHeaters->clear_state();
        this->dataUnitVentilators->clear_state();
        this->dataUserDefinedComponents->clear_state();
        this->dataVariableSpeedCoils->clear_state();
        this->dataVentilatedSlab->clear_state();
        this->dataWaterCoils->clear_state();
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
    }
}
