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

#ifndef EnergyPlusData_hh_INCLUDED
#define EnergyPlusData_hh_INCLUDED

// C++ Headers
#include <unordered_map>
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/IOFiles.hh>

#include <unordered_map>
#include <memory>
#include <string>
#include <unordered_map>

namespace EnergyPlus {

// forward declare all structs
struct AirflowNetworkBalanceManagerData;
struct AirLoopHVACDOASData;
struct BaseboardElectricData;
struct BaseboardRadiatorData;
struct BoilersData;
struct BoilerSteamData;
struct BranchInputManagerData;
struct ChilledCeilingPanelSimpleData;
struct ChillerAbsorberData;
struct ChillerElectricEIRData;
struct ChillerExhaustAbsorptionData;
struct ChillerGasAbsorptionData;
struct ChillerIndirectAbsoprtionData;
struct ChillerReformulatedEIRData;
struct CondenserLoopTowersData;
struct ConstructionData;
struct ConvectionCoefficientsData;
struct CoolTowerData;
struct CostEstimateManagerData;
struct CrossVentMgrData;
struct CTElectricGeneratorData;
struct CurveManagerData;
struct DataAirLoopData;
struct DataGlobal;
struct ExteriorEnergyUseData;
struct FansData;
struct PipesData;
struct PlantChillersData;
struct SolarShadingData;
struct SplitterComponentData;
struct SteamBaseboardRadiatorData;
struct SteamCoilsData;
struct SurfaceGroundHeatExchangersData;
struct SwimmingPoolsData;
struct SystemAvailabilityManagerData;
struct ThermalChimneysData;
struct ThermalComfortsData;
struct TranspiredCollectorData;
struct UFADManagerData;
struct UnitarySystemsData;
struct UnitHeatersData;
struct UnitVentilatorsData;
struct UserDefinedComponentsData;
struct VariableSpeedCoilsData;
struct VentilatedSlabData;
struct WaterCoilsData;
struct WaterManagerData;
struct WaterThermalTanksData;
struct WaterToAirHeatPumpData;
struct WaterToAirHeatPumpSimpleData;
struct WaterUseData;
struct WeatherManagerData;
struct WindowACData;
struct WindowComplexManagerData;
struct WindowEquivalentLayerData;
struct WindowManagerData;
struct WindTurbineData;
struct ZoneAirLoopEquipmentManagerData;
struct ZoneContaminantPredictorCorrectorData;
struct ZoneDehumidifierData;
struct ZoneEquipmentManagerData;
struct ZonePlenumData;
struct ZoneTempPredictorCorrectorData;

struct EnergyPlusData : BaseGlobalStruct {

    IOFiles files;

    // module globals
    std::unique_ptr<AirflowNetworkBalanceManagerData> dataAirflowNetworkBalanceManager;
    std::unique_ptr<AirLoopHVACDOASData> dataAirLoopHVACDOAS;
    std::unique_ptr<BaseboardElectricData> dataBaseboardElectric;
    std::unique_ptr<BaseboardRadiatorData> dataBaseboardRadiator;
    std::unique_ptr<BoilersData> dataBoilers;
    std::unique_ptr<BoilerSteamData> dataBoilerSteam;
    std::unique_ptr<BranchInputManagerData> dataBranchInputManager;
    std::unique_ptr<ChilledCeilingPanelSimpleData> dataChilledCeilingPanelSimple;
    std::unique_ptr<ChillerAbsorberData> dataChillerAbsorber;
    std::unique_ptr<ChillerElectricEIRData> dataChillerElectricEIR;
    std::unique_ptr<ChillerExhaustAbsorptionData> dataChillerExhaustAbsorption;
    std::unique_ptr<ChillerGasAbsorptionData> dataChillerGasAbsorption;
    std::unique_ptr<ChillerIndirectAbsoprtionData> dataChillerIndirectAbsorption;
    std::unique_ptr<ChillerReformulatedEIRData> dataChillerReformulatedEIR;
    std::unique_ptr<CondenserLoopTowersData> dataCondenserLoopTowers;
    std::unique_ptr<ConstructionData> dataConstruction;
    std::unique_ptr<ConvectionCoefficientsData> dataConvectionCoefficient;
    std::unique_ptr<CoolTowerData> dataCoolTower;
    std::unique_ptr<CostEstimateManagerData> dataCostEstimateManager;
    std::unique_ptr<CrossVentMgrData> dataCrossVentMgr;
    std::unique_ptr<CTElectricGeneratorData> dataCTElectricGenerator;
    std::unique_ptr<CurveManagerData> dataCurveManager;
    std::unique_ptr<DataAirLoopData> dataAirLoop;
    std::unique_ptr<DataGlobal> dataGlobal;
    std::unique_ptr<ExteriorEnergyUseData> dataExteriorEnergyUse;
    std::unique_ptr<FansData> dataFans;
    std::unique_ptr<PipesData> dataPipes;
    std::unique_ptr<PlantChillersData> dataPlantChillers;
    std::unique_ptr<SolarShadingData> dataSolarShading;
    std::unique_ptr<SplitterComponentData> dataSplitterComponent;
    std::unique_ptr<SteamBaseboardRadiatorData> dataSteamBaseboardRadiator;
    std::unique_ptr<SteamCoilsData> dataSteamCoils;
    std::unique_ptr<SurfaceGroundHeatExchangersData> dataSurfaceGroundHeatExchangers;
    std::unique_ptr<SwimmingPoolsData> dataSwimmingPools;
    std::unique_ptr<SystemAvailabilityManagerData> dataSystemAvailabilityManager;
    std::unique_ptr<ThermalChimneysData> dataThermalChimneys;
    std::unique_ptr<ThermalComfortsData> dataThermalComforts;
    std::unique_ptr<TranspiredCollectorData> dataTranspiredCollector;
    std::unique_ptr<UFADManagerData> dataUFADManager;
    std::unique_ptr<UnitarySystemsData> dataUnitarySystems;
    std::unique_ptr<UnitHeatersData> dataUnitHeaters;
    std::unique_ptr<UnitVentilatorsData> dataUnitVentilators;
    std::unique_ptr<UserDefinedComponentsData> dataUserDefinedComponents;
    std::unique_ptr<VariableSpeedCoilsData> dataVariableSpeedCoils;
    std::unique_ptr<VentilatedSlabData> dataVentilatedSlab;
    std::unique_ptr<WaterCoilsData> dataWaterCoils;
    std::unique_ptr<WaterManagerData> dataWaterManager;
    std::unique_ptr<WaterThermalTanksData> dataWaterThermalTanks;
    std::unique_ptr<WaterToAirHeatPumpData> dataWaterToAirHeatPump;
    std::unique_ptr<WaterToAirHeatPumpSimpleData> dataWaterToAirHeatPumpSimple;
    std::unique_ptr<WaterUseData> dataWaterUse;
    std::unique_ptr<WeatherManagerData> dataWeatherManager;
    std::unique_ptr<WindowACData> dataWindowAC;
    std::unique_ptr<WindowComplexManagerData> dataWindowComplexManager;
    std::unique_ptr<WindowEquivalentLayerData> dataWindowEquivalentLayer;
    std::unique_ptr<WindowManagerData> dataWindowManager;
    std::unique_ptr<WindTurbineData> dataWindTurbine;
    std::unique_ptr<ZoneAirLoopEquipmentManagerData> dataZoneAirLoopEquipmentManager;
    std::unique_ptr<ZoneContaminantPredictorCorrectorData> dataZoneContaminantPredictorCorrector;
    std::unique_ptr<ZoneDehumidifierData> dataZoneDehumidifier;
    std::unique_ptr<ZoneEquipmentManagerData> dataZoneEquipmentManager;
    std::unique_ptr<ZonePlenumData> dataZonePlenum;
    std::unique_ptr<ZoneTempPredictorCorrectorData> dataZoneTempPredictorCorrector;

    EnergyPlusData();

    // Cannot safely copy or delete this until we eradicate all remaining
    // calls to IOFiles::getSingleton and IOFiles::setSingleton
    EnergyPlusData(const EnergyPlusData &) = delete;
    EnergyPlusData(EnergyPlusData &&) = delete;

    void clear_state() override;

};

}
#endif
