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

#include <EnergyPlus/StateManagement.hh>

#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/CoolTower.hh>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/CTElectricGenerator.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DElightManagerF.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGenerators.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataPhotovoltaics.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DemandManager.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EarthTube.hh>
#include <EnergyPlus/EconomicLifeCycleCost.hh>
#include <EnergyPlus/EconomicTariff.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/EvaporativeFluidCoolers.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidCoolers.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/GroundHeatExchangers.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/HeatPumpWaterToWaterCOOLING.hh>
#include <EnergyPlus/HeatPumpWaterToWaterHEATING.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/HVACUnitaryBypassVAV.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HeatPumpWaterToWaterSimple.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/IceThermalStorage.hh>
#include <EnergyPlus/InputProcessing/IdfParser.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InputProcessing/InputValidation.hh>
#include <EnergyPlus/IntegratedHeatPump.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/MoistureBalanceEMPDManager.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/OutputReportTabularAnnual.hh>
#include <EnergyPlus/OutsideEnergySources.hh>
#include <EnergyPlus/PVWatts.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/PipeHeatTransfer.hh>
#include <EnergyPlus/Pipes.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantHeatExchangerFluidToFluid.hh>
#include <EnergyPlus/PlantLoopHeatPumpEIR.hh>
#include <EnergyPlus/PlantLoadProfile.hh>
#include <EnergyPlus/PlantPipingSystemsManager.hh>
#include <EnergyPlus/PlantPressureSystem.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PlantValves.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/Pumps.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ResultsSchema.hh>
#include <EnergyPlus/ReturnAirPathManager.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/RuntimeLanguageProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SolarCollectors.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/ThermalChimney.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UnitHeater.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>
#include <EnergyPlus/WeatherManager.hh>

void EnergyPlus::clearThisState(EnergyPlusData &state)
{
    state.clear_state();
}
void EnergyPlus::clearAllStates(OutputFiles &outputFiles)
{
    using namespace EnergyPlus;
    // A to Z order
    CoilCoolingDX::clear_state();
    CoolTower::clear_state();
    CrossVentMgr::clear_state();
    CTElectricGenerator::clear_state();
    CurveManager::clear_state();
    AirflowNetwork::clear_state();
    dataAirflowNetworkBalanceManager.clear_state();
    DataAirLoop::clear_state();
    DataBranchAirLoopPlant::clear_state();
    DataAirSystems::clear_state();
    DataBranchNodeConnections::clear_state();
    dataConstruction.clear_state();
    DataContaminantBalance::clear_state();
    DataConvergParams::clear_state();
    DataDefineEquip::clear_state();
    DataEnvironment::clear_state();
    DataErrorTracking::clear_state();
    DataGenerators::clear_state();
    DataGlobals::clear_state(outputFiles);
    DataHeatBalance::clear_state();
    DataHeatBalFanSys::clear_state();
    DataHeatBalSurface::clear_state();
    DataHVACGlobals::clear_state();
    DataIPShortCuts::clear_state();
    DataLoopNode::clear_state();
    DataMoistureBalance::clear_state();
    DataMoistureBalanceEMPD::clear_state();
    DataOutputs::clear_state();
    DataPhotovoltaics::clear_state();
    DataPlant::clear_state();
    DataReportingFlags::clear_state();
    DataRoomAirModel::clear_state();
    DataRuntimeLanguage::clear_state();
    DataSizing::clear_state();
    DataStringGlobals::clear_state();
    DataSurfaceLists::clear_state();
    DataSurfaces::clear_state();
    DataSystemVariables::clear_state();
    DataUCSDSharedData::clear_state();
    DataViewFactorInformation::clear_state();
    DataWater::clear_state();
    DataZoneControls::clear_state();
    DataZoneEnergyDemands::clear_state();
    DataZoneEquipment::clear_state();
    DaylightingManager::clear_state();
    DemandManager::clear_state();
    DesiccantDehumidifiers::clear_state();
    DualDuct::clear_state();
    DXCoils::clear_state();
    clearFacilityElectricPowerServiceObject();
    EarthTube::clear_state();
    EconomicLifeCycleCost::clear_state();
    EconomicTariff::clear_state();
    EMSManager::clear_state();
    EvaporativeCoolers::clear_state();
    EvaporativeFluidCoolers::clear_state();
    FanCoilUnits::clear_state();
    Fans::clear_state();
    FaultsManager::clear_state();
    FluidCoolers::clear_state();
    FluidProperties::clear_state();
    Furnaces::clear_state();
    GlobalNames::clear_state();
    GroundHeatExchangers::clear_state();
    GroundTemperatureManager::clear_state();
    HeatBalanceAirManager::clear_state();
    HeatBalanceIntRadExchange::clear_state();
    HeatBalanceManager::clear_state();
    HeatBalanceSurfaceManager::clear_state();
    HeatBalFiniteDiffManager::clear_state();
    HeatPumpWaterToWaterSimple::GshpSpecs::clear_state();
    HeatPumpWaterToWaterCOOLING::clear_state();
    HeatPumpWaterToWaterHEATING::clear_state();
    HeatRecovery::clear_state();
    HeatingCoils::clear_state();
    HighTempRadiantSystem::clear_state();
    Humidifiers::clear_state();
    HVACControllers::clear_state();
    HVACDXHeatPumpSystem::clear_state();
    HVACDXSystem::clear_state();
    HVACHXAssistedCoolingCoil::clear_state();
    HVACFan::clearHVACFanObjects();
    HVACManager::clear_state();
    HVACSingleDuctInduc::clear_state();
    HVACStandAloneERV::clear_state();
    HVACUnitaryBypassVAV::clear_state();
    HVACVariableRefrigerantFlow::clear_state();
    HybridModel::clear_state();
    HybridUnitaryAirConditioners::clear_state();
    HysteresisPhaseChange::clear_state();
    EnergyPlus::inputProcessor->clear_state();
    IceThermalStorage::clear_state();
    IntegratedHeatPump::clear_state();
    InternalHeatGains::clear_state();
    LowTempRadiantSystem::clear_state();
    MicroCHPElectricGenerator::clear_state();
    MixedAir::clear_state();
    MixerComponent::clear_state();
    MoistureBalanceEMPDManager::clear_state();
    NodeInputManager::clear_state();
    OutAirNodeManager::clear_state();
    OutdoorAirUnit::clear_state();
    OutputProcessor::clear_state();
    OutputReportPredefined::clear_state();
    OutputReportTabular::clear_state();
    OutputReportTabularAnnual::clear_state();
    OutsideEnergySources::clear_state();
    PackagedTerminalHeatPump::clear_state();
    PhotovoltaicThermalCollectors::clear_state();
    Pipes::clear_state();
    PipeHeatTransfer::clear_state();
    PlantCentralGSHP::clear_state();
    PlantCondLoopOperation::clear_state();
    PlantHeatExchangerFluidToFluid::clear_state();
    PlantLoadProfile::clear_state();
    PlantManager::clear_state();
    PlantPipingSystemsManager::clear_state();
    PlantPressureSystem::clear_state();
    PlantUtilities::clear_state();
    PlantPipingSystemsManager::clear_state();
    PlantValves::clear_state();
    PluginManagement::clear_state();
    PollutionModule::clear_state();
    PoweredInductionUnits::clear_state();
    Psychrometrics::clear_state();
    Pumps::clear_state();
    PurchasedAirManager::clear_state();
    PVWatts::clear_state();
    clearCoilSelectionReportObj(); // ReportCoilSelection
    RefrigeratedCase::clear_state();
    ReturnAirPathManager::clear_state();
    RoomAirModelAirflowNetwork::clear_state();
    RoomAirModelManager::clear_state();
    RuntimeLanguageProcessor::clear_state();
    ScheduleManager::clear_state();
    SetPointManager::clear_state();
    SimAirServingZones::clear_state();
    SimulationManager::clear_state();
    SingleDuct::clear_state();
    SizingManager::clear_state();
    SolarCollectors::clear_state();
    SolarShading::clear_state();
    SplitterComponent::clear_state();
    SteamCoils::clear_state();
    SurfaceGeometry::clear_state();
    SystemAvailabilityManager::clear_state();
    SwimmingPool::clear_state();
    ThermalChimney::clear_state();
    ThermalComfort::clear_state();
    UnitarySystems::clear_state();
    UnitHeater::clear_state();
    UnitVentilator::clear_state();
    UserDefinedComponents::clear_state();
    UtilityRoutines::clear_state();
    VariableSpeedCoils::clear_state();
    VentilatedSlab::clear_state();
    WaterCoils::clear_state();
    WaterManager::clear_state();
    WaterThermalTanks::clear_state();
    WaterToAirHeatPumpSimple::clear_state();
    EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::clear_state();
    WeatherManager::clear_state();
    ResultsFramework::clear_state();
}
