// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataPhotovoltaics.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/DisplacementVentMgr.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/ElectricBaseboardRadiator.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/EvaporativeFluidCoolers.hh>
#include <EnergyPlus/ExternalInterface.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACCooledBeam.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HVACDuct.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HVACMultiSpeedHeatPump.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/HVACUnitaryBypassVAV.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/IntegratedHeatPump.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/OutputReportTabularAnnual.hh>
#include <EnergyPlus/OutsideEnergySources.hh>
#include <EnergyPlus/PVWatts.hh>
#include <EnergyPlus/PackagedThermalStorageCoil.hh>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/Photovoltaics.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/RoomAirModelUserTempPattern.hh>
#include <EnergyPlus/RuntimeLanguageProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>

void EnergyPlus::clearAllStates(EnergyPlusData &state)
{
    // clear the passed in state
    state.clear_state();
    // then clear any other remaining global state, the number of these here will reduce over time
    using namespace EnergyPlus;
    // A to Z order
    autosizing_clear_state();
    CoilCoolingDX::clear_state();
    AirflowNetwork::clear_state();
    DataHeatBalFanSys::clear_state();
    DataHeatBalSurface::clear_state();
    DataHVACGlobals::clear_state();
    DataIPShortCuts::clear_state();
    DataLoopNode::clear_state();
    DataMoistureBalance::clear_state();
    DataMoistureBalanceEMPD::clear_state();
    DataOutputs::clear_state();
    DataPhotovoltaics::clear_state();
    DataReportingFlags::clear_state();
    DataSizing::clear_state();
    DataStringGlobals::clear_state();
    DataSurfaceLists::clear_state();
    DataSurfaces::clear_state();
    DataSystemVariables::clear_state();
    DataUCSDSharedData::clear_state();
    DataViewFactorInformation::clear_state();
    DesiccantDehumidifiers::clear_state();
    DisplacementVentMgr::clear_state();
    DualDuct::clear_state();
    clearFacilityElectricPowerServiceObject();
    ElectricBaseboardRadiator::clear_state();
    EvaporativeCoolers::clear_state();
    EvaporativeFluidCoolers::clear_state();
    ExternalInterface::clear_state();
    FanCoilUnits::clear_state();
    Fans::clear_state();
    FaultsManager::clear_state();
    FluidProperties::clear_state();
    Furnaces::clear_state();
    General::clear_state();
    GroundTemperatureManager::clear_state();
    HeatBalanceAirManager::clear_state();
    HeatBalanceIntRadExchange::clear_state();
    HeatBalanceManager::clear_state();
    HeatBalanceSurfaceManager::clear_state();
    HeatBalFiniteDiffManager::clear_state();
    HeatRecovery::clear_state();
    HeatingCoils::clear_state();
    HighTempRadiantSystem::clear_state();
    Humidifiers::clear_state();
    HVACControllers::clear_state();
    HVACCooledBeam::clear_state();
    HVACDuct::clear_state();
    HVACDXHeatPumpSystem::clear_state();
    HVACDXSystem::clear_state();
    HVACHXAssistedCoolingCoil::clear_state();
    HVACFan::clearHVACFanObjects();
    HVACManager::clear_state();
    HVACMultiSpeedHeatPump::clear_state();
    HVACSingleDuctInduc::clear_state();
    HVACUnitaryBypassVAV::clear_state();
    HybridModel::clear_state();
    HysteresisPhaseChange::clear_state();
    InternalHeatGains::clear_state();
    LowTempRadiantSystem::clear_state();
    OutputReportTabular::clear_state(state);
    OutputReportTabularAnnual::clear_state();
    PackagedThermalStorageCoil::clear_state();
    Photovoltaics::clear_state();
    PhotovoltaicThermalCollectors::clear_state();
    PollutionModule::clear_state();
    Psychrometrics::clear_state();
    PVWatts::clear_state();
    clearCoilSelectionReportObj(); // ReportCoilSelection
    RoomAirModelAirflowNetwork::clear_state();
    RoomAirModelManager::clear_state();
    RoomAirModelUserTempPattern::clear_state();
    RuntimeLanguageProcessor::clear_state();
    ScheduleManager::clear_state();
}
