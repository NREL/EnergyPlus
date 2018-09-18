// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
// A to Z order
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/BaseboardElectric.hh>
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/BoilerSteam.hh>
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChilledCeilingPanelSimple.hh>
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/ChillerExhaustAbsorption.hh>
#include <EnergyPlus/ChillerGasAbsorption.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
#include <EnergyPlus/CondenserLoopTowers.hh>
#include <EnergyPlus/CoolTower.hh>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DElightManagerF.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
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
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DemandManager.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/DirectAirManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
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
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/FluidCoolers.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/GroundHeatExchangers.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
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
#include <EnergyPlus/InputProcessing/IdfParser.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InputProcessing/InputValidation.hh>
#include <EnergyPlus/IntegratedHeatPump.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
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
#include <EnergyPlus/PipeHeatTransfer.hh>
#include <EnergyPlus/Pipes.hh>
#include <EnergyPlus/Plant/PlantLoopSolver.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantLoadProfile.hh>
#include <EnergyPlus/PlantPipingSystemsManager.hh>
#include <EnergyPlus/PlantPressureSystem.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/Pumps.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
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
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UnitHeater.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>
#include <EnergyPlus/WaterUse.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/WindowAC.hh>
#include <EnergyPlus/WindowComplexManager.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

#include <algorithm>
#include <fstream>

using json = nlohmann::json;

namespace EnergyPlus {

void EnergyPlusFixture::SetUpTestCase()
{
    EnergyPlus::inputProcessor = InputProcessor::factory();
}

void EnergyPlusFixture::SetUp()
{
    clear_all_states();

    show_message();

    this->eso_stream = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    this->eio_stream = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    this->mtr_stream = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    this->err_stream = std::unique_ptr<std::ostringstream>(new std::ostringstream);

    DataGlobals::eso_stream = this->eso_stream.get();
    DataGlobals::eio_stream = this->eio_stream.get();
    DataGlobals::mtr_stream = this->mtr_stream.get();
    DataGlobals::err_stream = this->err_stream.get();

    m_cout_buffer = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    m_redirect_cout = std::unique_ptr<RedirectCout>(new RedirectCout(m_cout_buffer));

    m_cerr_buffer = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    m_redirect_cerr = std::unique_ptr<RedirectCerr>(new RedirectCerr(m_cerr_buffer));

    UtilityRoutines::outputErrorHeader = false;

    Psychrometrics::InitializePsychRoutines();
    createCoilSelectionReportObj();
}

void EnergyPlusFixture::TearDown()
{

    clear_all_states();

    {
        IOFlags flags;
        flags.DISPOSE("DELETE");
        gio::close(OutputProcessor::OutputFileMeterDetails, flags);
        gio::close(DataGlobals::OutputFileStandard, flags);
        gio::close(DataGlobals::OutputStandardError, flags);
        gio::close(DataGlobals::OutputFileInits, flags);
        gio::close(DataGlobals::OutputFileDebug, flags);
        gio::close(DataGlobals::OutputFileZoneSizing, flags);
        gio::close(DataGlobals::OutputFileSysSizing, flags);
        gio::close(DataGlobals::OutputFileMeters, flags);
        gio::close(DataGlobals::OutputFileBNDetails, flags);
        gio::close(DataGlobals::OutputFileZonePulse, flags);
        gio::close(DataGlobals::OutputDElightIn, flags);
        gio::close(DataGlobals::OutputFileShadingFrac, flags);
    }
}

void EnergyPlusFixture::clear_all_states()
{
    // A to Z order
    AirflowNetworkBalanceManager::clear_state();
    BaseboardElectric::clear_state();
    BaseboardRadiator::clear_state();
    Boilers::clear_state();
    BoilerSteam::clear_state();
    BranchInputManager::clear_state();
    CoolingPanelSimple::clear_state();
    ChillerElectricEIR::clear_state();
    ChillerExhaustAbsorption::clear_state();
    ChillerGasAbsorption::clear_state();
    ChillerIndirectAbsorption::clear_state();
    CondenserLoopTowers::clear_state();
    CoolTower::clear_state();
    CrossVentMgr::clear_state();
    CurveManager::clear_state();
    DataAirflowNetwork::clear_state();
    DataAirLoop::clear_state();
    DataBranchAirLoopPlant::clear_state();
    DataAirSystems::clear_state();
    DataBranchNodeConnections::clear_state();
    DataContaminantBalance::clear_state();
    DataConvergParams::clear_state();
    DataDefineEquip::clear_state();
    DataEnvironment::clear_state();
    DataErrorTracking::clear_state();
    DataGenerators::clear_state();
    DataGlobals::clear_state();
    DataHeatBalance::clear_state();
    DataHeatBalFanSys::clear_state();
    DataHeatBalSurface::clear_state();
    DataHVACGlobals::clear_state();
    DataIPShortCuts::clear_state();
    DataLoopNode::clear_state();
    DataMoistureBalance::clear_state();
    DataMoistureBalanceEMPD::clear_state();
    DataOutputs::clear_state();
    DataPlant::clear_state();
    DataRoomAirModel::clear_state();
    DataRuntimeLanguage::clear_state();
    DataSizing::clear_state();
    DataSurfaceLists::clear_state();
    DataSurfaces::clear_state();
    DataSystemVariables::clear_state();
    DataUCSDSharedData::clear_state();
    DataZoneControls::clear_state();
    DataZoneEnergyDemands::clear_state();
    DataZoneEquipment::clear_state();
    DaylightingManager::clear_state();
    DemandManager::clear_state();
    DesiccantDehumidifiers::clear_state();
    DirectAirManager::clear_state();
    DualDuct::clear_state();
    DXCoils::clear_state();
    clearFacilityElectricPowerServiceObject();
    EarthTube::clear_state();
    EconomicLifeCycleCost::clear_state();
    EconomicTariff::clear_state();
    EMSManager::clear_state();
    EvaporativeCoolers::clear_state();
    EvaporativeFluidCoolers::clear_state();
    ExteriorEnergyUse::clear_state();
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
    HeatPumpWaterToWaterSimple::clear_state();
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
    HysteresisPhaseChange::clear_state();
    EnergyPlus::inputProcessor->clear_state();
    IntegratedHeatPump::clear_state();
    InternalHeatGains::clear_state();
    LowTempRadiantSystem::clear_state();
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
    Pipes::clear_state();
    PipeHeatTransfer::clear_state();
    PlantCondLoopOperation::clear_state();
    PlantChillers::clear_state();
    PlantLoadProfile::clear_state();
    PlantLoopSolver::clear_state();
    PlantManager::clear_state();
    PlantPipingSystemsManager::clear_state();
    PlantPressureSystem::clear_state();
    PlantUtilities::clear_state();
    PlantPipingSystemsManager::clear_state();
    PollutionModule::clear_state();
    PoweredInductionUnits::clear_state();
    Psychrometrics::clear_state();
    Pumps::clear_state();
    PurchasedAirManager::clear_state();
    PVWatts::clear_state();
    clearCoilSelectionReportObj(); // ReportCoilSelection
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
    SurfaceGeometry::clear_state();
    SystemAvailabilityManager::clear_state();
    SwimmingPool::clear_state();
    ThermalComfort::clear_state();
    UnitarySystems::clear_state();
    UnitHeater::clear_state();
    UnitVentilator::clear_state();
    VariableSpeedCoils::clear_state();
    VentilatedSlab::clear_state();
    WaterCoils::clear_state();
    WaterThermalTanks::clear_state();
    WaterToAirHeatPumpSimple::clear_state();
    WaterUse::clear_state();
    WeatherManager::clear_state();
    WindowAC::clear_state();
    WindowComplexManager::clear_state();
    WindowEquivalentLayer::clear_state();
    WindowManager::clear_state();
    ZoneAirLoopEquipmentManager::clear_state();
    ZoneContaminantPredictorCorrector::clear_state();
    ZoneDehumidifier::clear_state();
    ZoneEquipmentManager::clear_state();
    ZonePlenum::clear_state();
    ZoneTempPredictorCorrector::clear_state();
}

std::string EnergyPlusFixture::delimited_string(std::vector<std::string> const &strings, std::string const &delimiter)
{
    std::ostringstream compare_text;
    for (auto const &str : strings) {
        compare_text << str << delimiter;
    }
    return compare_text.str();
}

std::vector<std::string> EnergyPlusFixture::read_lines_in_file(std::string const &filePath)
{
    std::ifstream infile(filePath);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(infile, line)) {
        lines.push_back(line);
    }
    return lines;
}

bool EnergyPlusFixture::compare_eso_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->eso_stream->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->eso_stream->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_eio_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->eio_stream->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->eio_stream->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_mtr_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->mtr_stream->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->mtr_stream->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_err_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->err_stream->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->err_stream->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_cout_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->m_cout_buffer->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->m_cout_buffer->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_cerr_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->m_cerr_buffer->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->m_cerr_buffer->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::has_eso_output(bool reset_stream)
{
    auto const has_output = this->eso_stream->str().size() > 0;
    if (reset_stream) this->eso_stream->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_eio_output(bool reset_stream)
{
    auto const has_output = this->eio_stream->str().size() > 0;
    if (reset_stream) this->eio_stream->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_mtr_output(bool reset_stream)
{
    auto const has_output = this->mtr_stream->str().size() > 0;
    if (reset_stream) this->mtr_stream->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_err_output(bool reset_stream)
{
    auto const has_output = this->err_stream->str().size() > 0;
    if (reset_stream) this->err_stream->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_cout_output(bool reset_stream)
{
    auto const has_output = this->m_cout_buffer->str().size() > 0;
    if (reset_stream) this->m_cout_buffer->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_cerr_output(bool reset_stream)
{
    auto const has_output = this->m_cerr_buffer->str().size() > 0;
    if (reset_stream) this->m_cerr_buffer->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::process_idf(std::string const &idf_snippet, bool use_assertions)
{
    bool success = true;
    inputProcessor->epJSON = inputProcessor->idf_parser->decode(idf_snippet, inputProcessor->schema, success);

    if (inputProcessor->epJSON.find("Building") == inputProcessor->epJSON.end()) {
        inputProcessor->epJSON["Building"] = {{"Bldg",
                                               {{"idf_order", 0},
                                                {"north_axis", 0.0},
                                                {"terrain", "Suburbs"},
                                                {"loads_convergence_tolerance_value", 0.04},
                                                {"temperature_convergence_tolerance_value", 0.4000},
                                                {"solar_distribution", "FullExterior"},
                                                {"maximum_number_of_warmup_days", 25},
                                                {"minimum_number_of_warmup_days", 6}}}};
    }
    if (inputProcessor->epJSON.find("GlobalGeometryRules") == inputProcessor->epJSON.end()) {
        inputProcessor->epJSON["GlobalGeometryRules"] = {{"",
                                                          {{"idf_order", 0},
                                                           {"starting_vertex_position", "UpperLeftCorner"},
                                                           {"vertex_entry_direction", "Counterclockwise"},
                                                           {"coordinate_system", "Relative"},
                                                           {"daylighting_reference_point_coordinate_system", "Relative"},
                                                           {"rectangular_surface_coordinate_system", "Relative"}}}};
    }

    int MaxArgs = 0;
    int MaxAlpha = 0;
    int MaxNumeric = 0;
    inputProcessor->getMaxSchemaArgs(MaxArgs, MaxAlpha, MaxNumeric);

    DataIPShortCuts::cAlphaFieldNames.allocate(MaxAlpha);
    DataIPShortCuts::cAlphaArgs.allocate(MaxAlpha);
    DataIPShortCuts::lAlphaFieldBlanks.dimension(MaxAlpha, false);
    DataIPShortCuts::cNumericFieldNames.allocate(MaxNumeric);
    DataIPShortCuts::rNumericArgs.dimension(MaxNumeric, 0.0);
    DataIPShortCuts::lNumericFieldBlanks.dimension(MaxNumeric, false);

    bool is_valid = inputProcessor->validation->validate(inputProcessor->epJSON);
    bool hasErrors = inputProcessor->processErrors();

    inputProcessor->initializeMaps();
    SimulationManager::PostIPProcessing();
    // inputProcessor->state->printErrors();

    bool successful_processing = success && is_valid && !hasErrors;

    if (!successful_processing && use_assertions) {
        EXPECT_TRUE(compare_err_stream(""));
    }

    return successful_processing;
}

bool EnergyPlusFixture::process_idd(std::string const &idd, bool &errors_found)
{

    std::unique_ptr<std::istream> idd_stream;
    if (!idd.empty()) {
        idd_stream = std::unique_ptr<std::istringstream>(new std::istringstream(idd));
    } else {
        static auto const exeDirectory = FileSystem::getParentDirectoryPath(FileSystem::getAbsolutePath(FileSystem::getProgramPath()));
        static auto idd_location = exeDirectory + "Energy+.schema.epJSON";
        static auto file_exists = FileSystem::fileExists(idd_location);

        if (!file_exists) {
            // Energy+.schema.epJSON is in parent Products folder instead of Debug/Release/RelWithDebInfo/MinSizeRel folder of exe
            idd_location = FileSystem::getParentDirectoryPath(exeDirectory) + "Energy+.schema.epJSON";
            file_exists = FileSystem::fileExists(idd_location);
        }

        if (!file_exists) {
            EXPECT_TRUE(file_exists) << "Energy+.schema.epJSON does not exist at search location." << std::endl
                                     << "epJSON Schema search location: \"" << idd_location << "\"";
            errors_found = true;
            return errors_found;
        }

        idd_stream = std::unique_ptr<std::ifstream>(new std::ifstream(idd_location, std::ios_base::in | std::ios_base::binary));
    }

    if (!idd_stream->good()) {
        errors_found = true;
        return errors_found;
    }

    inputProcessor->schema = json::parse(*idd_stream);

    return errors_found;
}

bool EnergyPlusFixture::compare_idf(std::string const &EP_UNUSED(name),
                                    int const EP_UNUSED(num_alphas),
                                    int const EP_UNUSED(num_numbers),
                                    std::vector<std::string> const &EP_UNUSED(alphas),
                                    std::vector<bool> const &EP_UNUSED(alphas_blank),
                                    std::vector<Real64> const &EP_UNUSED(numbers),
                                    std::vector<bool> const &EP_UNUSED(numbers_blank))
{
    // using namespace InputProcessor;

    // bool has_error = OverallErrorFlag;

    // EXPECT_FALSE( OverallErrorFlag );

    // auto index = FindItemInSortedList( name, ListOfObjects, NumObjectDefs );

    // EXPECT_GT( index, 0 ) << "Could not find \"" << name << "\". Make sure to run process_idf first.";
    // if ( index < 1 ) return false;

    // index = iListOfObjects( index );
    // index = ObjectStartRecord( index );

    // EXPECT_EQ( name, IDFRecords( index ).Name );
    // if ( name != IDFRecords( index ).Name ) has_error = true;
    // EXPECT_EQ( num_alphas, IDFRecords( index ).NumAlphas );
    // if ( num_alphas != IDFRecords( index ).NumAlphas ) has_error = true;
    // EXPECT_EQ( num_numbers, IDFRecords( index ).NumNumbers );
    // if ( num_numbers != IDFRecords( index ).NumNumbers ) has_error = true;
    // if ( ! compare_containers( alphas, IDFRecords( index ).Alphas ) ) has_error = true;
    // if ( ! compare_containers( alphas_blank, IDFRecords( index ).AlphBlank ) ) has_error = true;
    // if ( ! compare_containers( numbers, IDFRecords( index ).Numbers ) ) has_error = true;
    // if ( ! compare_containers( numbers_blank, IDFRecords( index ).NumBlank ) ) has_error = true;

    // return ! has_error;
    return false;
}

} // namespace EnergyPlus
