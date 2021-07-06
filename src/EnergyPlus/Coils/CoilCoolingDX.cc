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

#include <ObjexxFCL/Array1D.hh> // needs to be in BranchNodeConnections.hh

#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/WaterManager.hh>

using namespace EnergyPlus;

int CoilCoolingDX::factory(EnergyPlus::EnergyPlusData &state, std::string const &coilName)
{
    if (state.dataCoilCooingDX->coilCoolingDXGetInputFlag) {
        CoilCoolingDX::getInput(state);
        state.dataCoilCooingDX->coilCoolingDXGetInputFlag = false;
    }
    int handle = -1;
    for (auto const &thisCoil : state.dataCoilCooingDX->coilCoolingDXs) {
        handle++;
        if (EnergyPlus::UtilityRoutines::MakeUPPERCase(coilName) == EnergyPlus::UtilityRoutines::MakeUPPERCase(thisCoil.name)) {
            return handle;
        }
    }
    ShowSevereError(state, "Coil:Cooling:DX Coil not found=" + coilName);
    return -1;
}

void CoilCoolingDX::getInput(EnergyPlus::EnergyPlusData &state)
{
    int numCoolingCoilDXs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataCoilCooingDX->coilCoolingDXObjectName);
    if (numCoolingCoilDXs <= 0) {
        ShowFatalError(state, R"(No "Coil:Cooling:DX" objects in input file)");
    }
    for (int coilNum = 1; coilNum <= numCoolingCoilDXs; ++coilNum) {
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                 coilNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus);
        CoilCoolingDXInputSpecification input_specs;
        input_specs.name = state.dataIPShortCut->cAlphaArgs(1);
        input_specs.evaporator_inlet_node_name = state.dataIPShortCut->cAlphaArgs(2);
        input_specs.evaporator_outlet_node_name = state.dataIPShortCut->cAlphaArgs(3);
        input_specs.availability_schedule_name = state.dataIPShortCut->cAlphaArgs(4);
        input_specs.condenser_zone_name = state.dataIPShortCut->cAlphaArgs(5);
        input_specs.condenser_inlet_node_name = state.dataIPShortCut->cAlphaArgs(6);
        input_specs.condenser_outlet_node_name = state.dataIPShortCut->cAlphaArgs(7);
        input_specs.performance_object_name = state.dataIPShortCut->cAlphaArgs(8);
        input_specs.condensate_collection_water_storage_tank_name = state.dataIPShortCut->cAlphaArgs(9);
        input_specs.evaporative_condenser_supply_water_storage_tank_name = state.dataIPShortCut->cAlphaArgs(10);
        CoilCoolingDX thisCoil;
        thisCoil.instantiateFromInputSpec(state, input_specs);
        state.dataCoilCooingDX->coilCoolingDXs.push_back(thisCoil);
    }
}

void CoilCoolingDX::instantiateFromInputSpec(EnergyPlus::EnergyPlusData &state, const CoilCoolingDXInputSpecification &input_data)
{
    static const std::string routineName("CoilCoolingDX::instantiateFromInputSpec: ");
    this->original_input_specs = input_data;
    bool errorsFound = false;
    this->name = input_data.name;
    this->performance = CoilCoolingDXCurveFitPerformance(state, input_data.performance_object_name);

    if (!this->performance.original_input_specs.base_operating_mode_name.empty() &&
        !this->performance.original_input_specs.alternate_operating_mode_name.empty() &&
        !this->performance.original_input_specs.alternate_operating_mode2_name.empty()) {
        this->SubcoolReheatFlag = true;
    }

    // other construction below
    this->evapInletNodeIndex = NodeInputManager::GetOnlySingleNode(state,
                                                                   input_data.evaporator_inlet_node_name,
                                                                   errorsFound,
                                                                   state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                   input_data.name,
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                   NodeInputManager::compFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsNotParent);
    this->evapOutletNodeIndex = NodeInputManager::GetOnlySingleNode(state,
                                                                    input_data.evaporator_outlet_node_name,
                                                                    errorsFound,
                                                                    state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                    input_data.name,
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                    NodeInputManager::compFluidStream::Primary,
                                                                    DataLoopNode::ObjectIsNotParent);

    this->condInletNodeIndex = NodeInputManager::GetOnlySingleNode(state,
                                                                   input_data.condenser_inlet_node_name,
                                                                   errorsFound,
                                                                   state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                   input_data.name,
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                   DataLoopNode::ObjectIsNotParent);

    // Ultimately, this restriction should go away - condenser inlet node could be from anywhere
    if (!OutAirNodeManager::CheckOutAirNodeNumber(state, this->condInletNodeIndex)) {
        ShowWarningError(state, routineName + state.dataCoilCooingDX->coilCoolingDXObjectName + "=\"" + this->name + "\", may be invalid");
        ShowContinueError(state,
                          "Condenser Inlet Node Name=\"" + input_data.condenser_inlet_node_name +
                              "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
        ShowContinueError(state, "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues");
    }

    this->condOutletNodeIndex = NodeInputManager::GetOnlySingleNode(state,
                                                                    input_data.condenser_outlet_node_name,
                                                                    errorsFound,
                                                                    state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                    input_data.name,
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                    DataLoopNode::ObjectIsNotParent);

    if (!input_data.condensate_collection_water_storage_tank_name.empty()) {
        WaterManager::SetupTankSupplyComponent(state,
                                               this->name,
                                               state.dataCoilCooingDX->coilCoolingDXObjectName,
                                               input_data.condensate_collection_water_storage_tank_name,
                                               errorsFound,
                                               this->condensateTankIndex,
                                               this->condensateTankSupplyARRID);
    }

    if (!input_data.evaporative_condenser_supply_water_storage_tank_name.empty()) {
        WaterManager::SetupTankDemandComponent(state,
                                               this->name,
                                               state.dataCoilCooingDX->coilCoolingDXObjectName,
                                               input_data.evaporative_condenser_supply_water_storage_tank_name,
                                               errorsFound,
                                               this->evaporativeCondSupplyTankIndex,
                                               this->evaporativeCondSupplyTankARRID);
    }

    if (input_data.availability_schedule_name.empty()) {
        this->availScheduleIndex = DataGlobalConstants::ScheduleAlwaysOn;
    } else {
        this->availScheduleIndex = ScheduleManager::GetScheduleIndex(state, input_data.availability_schedule_name);
    }

    if (this->availScheduleIndex == 0) {
        ShowSevereError(state, routineName + state.dataCoilCooingDX->coilCoolingDXObjectName + "=\"" + this->name + "\", invalid");
        ShowContinueError(state, "...Availability Schedule Name=\"" + input_data.availability_schedule_name + "\".");
        errorsFound = true;
    }

    if (!input_data.condenser_zone_name.empty()) {
        this->isSecondaryDXCoilInZone = true;
        // Setup zone data here
    }

    BranchNodeConnections::TestCompSet(state,
                                       state.dataCoilCooingDX->coilCoolingDXObjectName,
                                       this->name,
                                       input_data.evaporator_inlet_node_name,
                                       input_data.evaporator_outlet_node_name,
                                       "Air Nodes");

    if (errorsFound) {
        ShowFatalError(state,
                       routineName + "Errors found in getting " + state.dataCoilCooingDX->coilCoolingDXObjectName +
                           " input. Preceding condition(s) causes termination.");
    }
}

void CoilCoolingDX::oneTimeInit(EnergyPlus::EnergyPlusData &state)
{

    // setup output variables, needs to be done after object is instantiated and emplaced
    SetupOutputVariable(
        state, "Cooling Coil Total Cooling Rate", OutputProcessor::Unit::W, this->totalCoolingEnergyRate, "System", "Average", this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Total Cooling Energy",
                        OutputProcessor::Unit::J,
                        this->totalCoolingEnergy,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "ENERGYTRANSFER",
                        "COOLINGCOILS",
                        _,
                        "System");
    SetupOutputVariable(
        state, "Cooling Coil Sensible Cooling Rate", OutputProcessor::Unit::W, this->sensCoolingEnergyRate, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Cooling Coil Sensible Cooling Energy", OutputProcessor::Unit::J, this->sensCoolingEnergy, "System", "Sum", this->name);
    SetupOutputVariable(
        state, "Cooling Coil Latent Cooling Rate", OutputProcessor::Unit::W, this->latCoolingEnergyRate, "System", "Average", this->name);
    SetupOutputVariable(state, "Cooling Coil Latent Cooling Energy", OutputProcessor::Unit::J, this->latCoolingEnergy, "System", "Sum", this->name);
    SetupOutputVariable(
        state, "Cooling Coil Electricity Rate", OutputProcessor::Unit::W, this->performance.powerUse, "System", "Average", this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->performance.electricityConsumption,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "Electricity",
                        "COOLING",
                        _,
                        "System");

    if (this->performance.compressorFuelType != DataGlobalConstants::ResourceType::Electricity) {
        SetupOutputVariable(state,
                            "Cooling Coil " + this->performance.compressorFuelTypeForOutput + " Rate",
                            OutputProcessor::Unit::W,
                            this->performance.compressorFuelRate,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "Cooling Coil " + this->performance.compressorFuelTypeForOutput + " Energy",
                            OutputProcessor::Unit::J,
                            this->performance.compressorFuelConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            this->performance.compressorFuelTypeForOutput,
                            "COOLING",
                            _,
                            "System");
    }

    SetupOutputVariable(
        state, "Cooling Coil Runtime Fraction", OutputProcessor::Unit::None, this->coolingCoilRuntimeFraction, "System", "Average", this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Crankcase Heater Electricity Rate",
                        OutputProcessor::Unit::W,
                        this->performance.crankcaseHeaterPower,
                        "System",
                        "Average",
                        this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Crankcase Heater Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->performance.crankcaseHeaterElectricityConsumption,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "Electricity",
                        "Cooling",
                        _,
                        "System");
    // Ported from variable speed coil
    SetupOutputVariable(
        state, "Cooling Coil Air Mass Flow Rate", OutputProcessor::Unit::kg_s, this->airMassFlowRate, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Cooling Coil Air Inlet Temperature", OutputProcessor::Unit::C, this->inletAirDryBulbTemp, "System", "Average", this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Air Inlet Humidity Ratio",
                        OutputProcessor::Unit::kgWater_kgDryAir,
                        this->inletAirHumRat,
                        "System",
                        "Average",
                        this->name);
    SetupOutputVariable(
        state, "Cooling Coil Air Outlet Temperature", OutputProcessor::Unit::C, this->outletAirDryBulbTemp, "System", "Average", this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Air Outlet Humidity Ratio",
                        OutputProcessor::Unit::kgWater_kgDryAir,
                        this->outletAirHumRat,
                        "System",
                        "Average",
                        this->name);
    SetupOutputVariable(
        state, "Cooling Coil Part Load Ratio", OutputProcessor::Unit::None, this->partLoadRatioReport, "System", "Average", this->name);
    SetupOutputVariable(state, "Cooling Coil Upper Speed Level", OutputProcessor::Unit::None, this->speedNumReport, "System", "Average", this->name);
    SetupOutputVariable(
        state, "Cooling Coil Neighboring Speed Levels Ratio", OutputProcessor::Unit::None, this->speedRatioReport, "System", "Average", this->name);
    SetupOutputVariable(state,
                        "Cooling Coil Condenser Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->condenserInletTemperature,
                        "System",
                        "Average",
                        this->name);
    SetupOutputVariable(
        state, "Cooling Coil Dehumidification Mode", OutputProcessor::Unit::None, this->dehumidificationMode, "System", "Average", this->name);
    SetupOutputVariable(state, "Cooling Coil Waste Heat Power", OutputProcessor::Unit::W, this->wasteHeatEnergyRate, "System", "Average", this->name);
    SetupOutputVariable(state, "Cooling Coil Waste Heat Energy", OutputProcessor::Unit::J, this->wasteHeatEnergy, "System", "Sum", this->name);

    if (this->performance.evapCondBasinHeatCap > 0) {
        SetupOutputVariable(state,
                            "Cooling Coil Basin Heater Electricity Rate",
                            OutputProcessor::Unit::W,
                            this->performance.basinHeaterPower,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "Cooling Coil Basin Heater Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->performance.basinHeaterElectricityConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "Electricity",
                            "COOLING",
                            _,
                            "System");
    }
    if (this->condensateTankIndex > 0) {
        SetupOutputVariable(state,
                            "Cooling Coil Condensate Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->condensateVolumeFlow,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "Cooling Coil Condensate Volume",
                            OutputProcessor::Unit::m3,
                            this->condensateVolumeConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "OnSiteWater",
                            "Condensate",
                            _,
                            "System");
    }
    if (this->evaporativeCondSupplyTankIndex > 0) {
        SetupOutputVariable(state,
                            "Cooling Coil Evaporative Condenser Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            this->evapCondPumpElecPower,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "Cooling Coil Evaporative Condenser Pump Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->evapCondPumpElecConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "Electricity",
                            "COOLING",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Evaporative Condenser Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->evaporativeCondSupplyTankVolumeFlow,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "Cooling Coil Evaporative Condenser Water Volume",
                            OutputProcessor::Unit::m3,
                            this->evaporativeCondSupplyTankConsump,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "Water",
                            "Cooling",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Evaporative Condenser Mains Supply Water Volume",
                            OutputProcessor::Unit::m3,
                            this->evaporativeCondSupplyTankConsump,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "MainsWater",
                            "Cooling",
                            _,
                            "System");
    }
    if (this->SubcoolReheatFlag) {
        SetupOutputVariable(state,
                            "SubcoolReheat Cooling Coil Operation Mode",
                            OutputProcessor::Unit::None,
                            this->performance.OperatingMode,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "SubcoolReheat Cooling Coil Operation Mode Ratio",
                            OutputProcessor::Unit::None,
                            this->performance.ModeRatio,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "SubcoolReheat Cooling Coil Recovered Heat Energy Rate",
                            OutputProcessor::Unit::W,
                            this->recoveredHeatEnergyRate,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable(state,
                            "SubcoolReheat Cooling Coil Recovered Heat Energy",
                            OutputProcessor::Unit::J,
                            this->recoveredHeatEnergy,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATRECOVERY",
                            _,
                            "System");
    }

    if (this->isSecondaryDXCoilInZone) {
        SetupOutputVariable(state,
                            "Secondary Coil Heat Rejection Rate",
                            OutputProcessor::Unit::W,
                            this->secCoilSensHeatRejEnergyRate,
                            "System",
                            "Average",
                            this->name);

        SetupOutputVariable(
            state, "Secondary Coil Heat Rejection Energy", OutputProcessor::Unit::J, this->secCoilSensHeatRejEnergy, "System", "Sum", this->name);
    }
}

int CoilCoolingDX::getNumModes()
{
    int numModes = 1;
    if (this->performance.hasAlternateMode) {
        numModes++;
    }
    return numModes;
}

int CoilCoolingDX::getOpModeCapFTIndex(bool const useAlternateMode)
{
    if (useAlternateMode) {
        return this->altModeNomSpeed().indexCapFT;
    } else {
        return this->normModeNomSpeed().indexCapFT;
    }
}

void CoilCoolingDX::setData(int fanIndex, int fanType, std::string const &fanName, int _airLoopNum)
{
    this->supplyFanIndex = fanIndex;
    this->supplyFanName = fanName;
    this->supplyFanType = fanType;
    this->airLoopNum = _airLoopNum;
}

void CoilCoolingDX::getFixedData(int &_evapInletNodeIndex,
                                 int &_evapOutletNodeIndex,
                                 int &_condInletNodeIndex,
                                 int &_normalModeNumSpeeds,
                                 CoilCoolingDXCurveFitPerformance::CapControlMethod &_capacityControlMethod,
                                 Real64 &_minOutdoorDryBulb)
{
    _evapInletNodeIndex = this->evapInletNodeIndex;
    _evapOutletNodeIndex = this->evapOutletNodeIndex;
    _condInletNodeIndex = this->condInletNodeIndex;
    _normalModeNumSpeeds = (int)this->performance.normalMode.speeds.size();
    _capacityControlMethod = this->performance.capControlMethod;
    _minOutdoorDryBulb = this->performance.minOutdoorDrybulb;
}

void CoilCoolingDX::getDataAfterSizing(Real64 &_normalModeRatedEvapAirFlowRate,
                                       Real64 &_normalModeRatedCapacity,
                                       std::vector<Real64> &_normalModeFlowRates,
                                       std::vector<Real64> &_normalModeRatedCapacities)
{
    _normalModeRatedEvapAirFlowRate = this->performance.normalMode.ratedEvapAirFlowRate;
    _normalModeFlowRates.clear();
    _normalModeRatedCapacities.clear();
    for (auto const &thisSpeed : this->performance.normalMode.speeds) {
        _normalModeFlowRates.push_back(thisSpeed.evap_air_flow_rate);
        _normalModeRatedCapacities.push_back(thisSpeed.rated_total_capacity);
    }
    _normalModeRatedCapacity = this->performance.normalMode.ratedGrossTotalCap;
}

CoilCoolingDXCurveFitSpeed &CoilCoolingDX::normModeNomSpeed()
{
    return this->performance.normalMode.speeds[this->performance.normalMode.nominalSpeedIndex];
}

CoilCoolingDXCurveFitSpeed &CoilCoolingDX::altModeNomSpeed()
{
    return this->performance.alternateMode.speeds[this->performance.alternateMode.nominalSpeedIndex];
}

Real64 CoilCoolingDX::condMassFlowRate(bool const useAlternateMode)
{
    if (useAlternateMode) {
        return this->altModeNomSpeed().RatedCondAirMassFlowRate;
    } else {
        return this->normModeNomSpeed().RatedCondAirMassFlowRate;
    }
}

void CoilCoolingDX::size(EnergyPlus::EnergyPlusData &state)
{
    this->performance.parentName = this->name;
    this->performance.size(state);
}

void CoilCoolingDX::simulate(EnergyPlus::EnergyPlusData &state,
                             int useAlternateMode,
                             Real64 PLR,
                             int speedNum,
                             Real64 speedRatio,
                             int const fanOpMode,
                             bool const singleMode,
                             Real64 LoadSHR)
{
    if (this->myOneTimeInitFlag) {
        this->oneTimeInit(state);
        this->myOneTimeInitFlag = false;
    }

    std::string RoutineName = "CoilCoolingDX::simulate";

    // get node references
    auto &evapInletNode = state.dataLoopNodes->Node(this->evapInletNodeIndex);
    auto &evapOutletNode = state.dataLoopNodes->Node(this->evapOutletNodeIndex);
    auto &condInletNode = state.dataLoopNodes->Node(this->condInletNodeIndex);
    auto &condOutletNode = state.dataLoopNodes->Node(this->condOutletNodeIndex);

    // set some reporting variables
    this->condenserInletTemperature = condInletNode.Temp;
    this->dehumidificationMode = useAlternateMode;

    // set condenser inlet/outlet nodes
    // once condenser inlet is connected to upstream components, will need to revisit
    condInletNode.MassFlowRate = this->condMassFlowRate(useAlternateMode);
    condOutletNode.MassFlowRate = condInletNode.MassFlowRate;

    // call the simulation, which returns useful data
    // TODO: check the avail schedule and reset data/pass through data as needed
    // TODO: check the minOATcompressor and reset data/pass through data as needed
    this->performance.OperatingMode = 0;
    this->performance.ModeRatio = 0.0;
    this->performance.simulate(state,
                               evapInletNode,
                               evapOutletNode,
                               useAlternateMode,
                               PLR,
                               speedNum,
                               speedRatio,
                               fanOpMode,
                               condInletNode,
                               condOutletNode,
                               singleMode,
                               LoadSHR);
    EnergyPlus::CoilCoolingDX::passThroughNodeData(evapInletNode, evapOutletNode);

    // calculate energy conversion factor
    Real64 reportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // update condensate collection tank
    if (this->condensateTankIndex > 0) {
        if (speedNum > 0) {
            // calculate and report condensation rates  (how much water extracted from the air stream)
            // water flow of water in m3/s for water system interactions
            Real64 averageTemp = (evapInletNode.Temp - evapOutletNode.Temp) / 2.0;
            Real64 waterDensity = Psychrometrics::RhoH2O(averageTemp);
            Real64 inHumidityRatio = evapInletNode.HumRat;
            Real64 outHumidityRatio = evapOutletNode.HumRat;
            this->condensateVolumeFlow = max(0.0, (evapInletNode.MassFlowRate * (inHumidityRatio - outHumidityRatio) / waterDensity));
            this->condensateVolumeConsumption = this->condensateVolumeFlow * reportingConstant;
            state.dataWaterData->WaterStorage(this->condensateTankIndex).VdotAvailSupply(this->condensateTankSupplyARRID) =
                this->condensateVolumeFlow;
            state.dataWaterData->WaterStorage(this->condensateTankIndex).TwaterSupply(this->condensateTankSupplyARRID) = evapOutletNode.Temp;
        } else {
            state.dataWaterData->WaterStorage(this->condensateTankIndex).VdotAvailSupply(this->condensateTankSupplyARRID) = 0.0;
            state.dataWaterData->WaterStorage(this->condensateTankIndex).TwaterSupply(this->condensateTankSupplyARRID) = evapOutletNode.Temp;
        }
    }

    // update requests for evaporative condenser tank
    if (this->evaporativeCondSupplyTankIndex > 0) {
        if (speedNum > 0) {
            Real64 condInletTemp =
                state.dataEnvrn->OutWetBulbTemp + (state.dataEnvrn->OutDryBulbTemp - state.dataEnvrn->OutWetBulbTemp) *
                                                      (1.0 - this->performance.normalMode.speeds[speedNum - 1].evap_condenser_effectiveness);
            Real64 condInletHumRat =
                Psychrometrics::PsyWFnTdbTwbPb(state, condInletTemp, state.dataEnvrn->OutWetBulbTemp, state.dataEnvrn->OutBaroPress, RoutineName);
            Real64 outdoorHumRat = state.dataEnvrn->OutHumRat;

            Real64 condAirMassFlow = condInletNode.MassFlowRate;
            Real64 waterDensity = Psychrometrics::RhoH2O(state.dataEnvrn->OutDryBulbTemp);
            this->evaporativeCondSupplyTankVolumeFlow = (condInletHumRat - outdoorHumRat) * condAirMassFlow / waterDensity;
            this->evaporativeCondSupplyTankConsump = this->evaporativeCondSupplyTankVolumeFlow * reportingConstant;
            if (useAlternateMode == DataHVACGlobals::coilNormalMode) {
                this->evapCondPumpElecPower = this->performance.normalMode.getCurrentEvapCondPumpPower(speedNum);
            }
            state.dataWaterData->WaterStorage(this->evaporativeCondSupplyTankIndex).VdotRequestDemand(this->evaporativeCondSupplyTankARRID) =
                this->evaporativeCondSupplyTankVolumeFlow;
        } else {
            state.dataWaterData->WaterStorage(this->evaporativeCondSupplyTankIndex).VdotRequestDemand(this->evaporativeCondSupplyTankARRID) = 0.0;
        }
    }

    // update report variables
    this->airMassFlowRate = evapOutletNode.MassFlowRate;
    this->inletAirDryBulbTemp = evapInletNode.Temp;
    this->inletAirHumRat = evapInletNode.HumRat;
    this->outletAirDryBulbTemp = evapOutletNode.Temp;
    this->outletAirHumRat = evapOutletNode.HumRat;

    CalcComponentSensibleLatentOutput(evapOutletNode.MassFlowRate,
                                      evapInletNode.Temp,
                                      evapInletNode.HumRat,
                                      evapOutletNode.Temp,
                                      evapOutletNode.HumRat,
                                      this->sensCoolingEnergyRate,
                                      this->latCoolingEnergyRate,
                                      this->totalCoolingEnergyRate);
    this->totalCoolingEnergy = this->totalCoolingEnergyRate * reportingConstant;
    this->sensCoolingEnergy = this->sensCoolingEnergyRate * reportingConstant;
    this->latCoolingEnergy = this->latCoolingEnergyRate * reportingConstant;

    this->evapCondPumpElecConsumption = this->evapCondPumpElecPower * reportingConstant;

    this->coolingCoilRuntimeFraction = this->performance.RTF;
    this->elecCoolingPower = this->performance.powerUse;
    this->elecCoolingConsumption = this->performance.powerUse * reportingConstant;
    this->wasteHeatEnergyRate = this->performance.wasteHeatRate;
    this->wasteHeatEnergy = this->performance.wasteHeatRate * reportingConstant;

    this->partLoadRatioReport = PLR;
    this->speedNumReport = speedNum;
    this->speedRatioReport = speedRatio;

    if (useAlternateMode == DataHVACGlobals::coilSubcoolReheatMode) {
        this->recoveredHeatEnergyRate = this->performance.recoveredEnergyRate;
        this->recoveredHeatEnergy = this->recoveredHeatEnergyRate * reportingConstant;
    }

    if (this->isSecondaryDXCoilInZone) {
        // call CalcSecondaryDXCoils ???
        this->secCoilSensHeatRejEnergyRate = this->totalCoolingEnergyRate + this->elecCoolingPower;
        this->secCoilSensHeatRejEnergy = this->totalCoolingEnergy + this->elecCoolingConsumption;
    }

    // Fishy global things that need to be set here, try to set the AFN stuff now
    // This appears to be the only location where airLoopNum gets used
    // DataAirLoop::LoopDXCoilRTF = max(this->coolingCoilRuntimeFraction, DXCoil(DXCoilNum).HeatingCoilRuntimeFraction);
    state.dataAirLoop->LoopDXCoilRTF = this->coolingCoilRuntimeFraction;
    state.dataHVACGlobal->DXElecCoolingPower = this->elecCoolingPower;
    if (this->airLoopNum > 0) {
        state.dataAirLoop->AirLoopAFNInfo(this->airLoopNum).AFNLoopDXCoilRTF = this->coolingCoilRuntimeFraction;
        // The original calculation is below, but no heating yet
        //        max(DXCoil(DXCoilNum).CoolingCoilRuntimeFraction, DXCoil(DXCoilNum).HeatingCoilRuntimeFraction);
    }

    // report out to the coil sizing report if needed
    if (this->reportCoilFinalSizes) {
        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingHVACSizingSimulations && !state.dataGlobal->DoingSizing) {

            // report out final coil sizing info
            Real64 ratedSensCap(0.0);
            ratedSensCap = this->performance.normalMode.ratedGrossTotalCap * this->normModeNomSpeed().grossRatedSHR;
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(state,
                                                                                  this->name,
                                                                                  state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                                  this->performance.normalMode.ratedGrossTotalCap,
                                                                                  ratedSensCap,
                                                                                  this->performance.normalMode.ratedEvapAirFlowRate,
                                                                                  -999.0);

            // report out fan information
            if (this->supplyFanType == DataHVACGlobals::FanType_SystemModelObject) {
                if (this->supplyFanIndex >= 0) {
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                             this->name,
                                                                                             state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                                             state.dataHVACFan->fanObjs[this->supplyFanIndex]->name,
                                                                                             DataAirSystems::objectVectorOOFanSystemModel,
                                                                                             this->supplyFanIndex);
                }
            } else {
                if (this->supplyFanIndex >= 1) {
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                             this->name,
                                                                                             state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                                             state.dataFans->Fan(this->supplyFanIndex).FanName,
                                                                                             DataAirSystems::structArrayLegacyFanModels,
                                                                                             this->supplyFanIndex);
                }
            }

            // report out coil rating conditions, just create a set of dummy nodes and run calculate on them
            DataLoopNode::NodeData dummyEvapInlet;
            DataLoopNode::NodeData dummyEvapOutlet;
            DataLoopNode::NodeData dummyCondInlet;
            DataLoopNode::NodeData dummyCondOutlet;
            Real64 dummyPLR = 1.0;
            int dummySpeedNum = 1;
            Real64 dummySpeedRatio = 1.0;
            int dummyFanOpMode = 1.0;
            bool dummySingleMode = false;

            Real64 const RatedInletAirTemp(26.6667);   // 26.6667C or 80F
            Real64 const RatedInletWetBulbTemp(19.44); // 19.44 or 67F
            Real64 const RatedOutdoorAirTemp(35.0);    // 35 C or 95F
            Real64 ratedOutdoorAirWetBulb = 23.9;      // from I/O ref. more precise value?

            Real64 ratedInletEvapMassFlowRate = this->performance.normalMode.ratedEvapAirMassFlowRate;
            dummyEvapInlet.MassFlowRate = ratedInletEvapMassFlowRate;
            dummyEvapInlet.Temp = RatedInletAirTemp;
            Real64 dummyInletAirHumRat =
                Psychrometrics::PsyWFnTdbTwbPb(state, RatedInletAirTemp, RatedInletWetBulbTemp, DataEnvironment::StdPressureSeaLevel, RoutineName);
            dummyEvapInlet.Press = DataEnvironment::StdPressureSeaLevel;
            dummyEvapInlet.HumRat = dummyInletAirHumRat;
            dummyEvapInlet.Enthalpy = Psychrometrics::PsyHFnTdbW(RatedInletAirTemp, dummyInletAirHumRat);

            // maybe we don't actually need to override weather below, we'll see
            dummyCondInlet.Temp = RatedOutdoorAirTemp;
            dummyCondInlet.HumRat =
                Psychrometrics::PsyWFnTdbTwbPb(state, RatedOutdoorAirTemp, ratedOutdoorAirWetBulb, DataEnvironment::StdPressureSeaLevel, RoutineName);
            dummyCondInlet.OutAirWetBulb = ratedOutdoorAirWetBulb;
            dummyCondInlet.Press = condInletNode.Press; // for now; TODO: Investigate

            // overriding outdoor conditions temporarily
            Real64 holdOutDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
            Real64 holdOutHumRat = state.dataEnvrn->OutHumRat;
            Real64 holdOutWetBulb = state.dataEnvrn->OutWetBulbTemp;
            Real64 holdOutBaroPress = state.dataEnvrn->OutBaroPress;
            state.dataEnvrn->OutDryBulbTemp = RatedOutdoorAirTemp;
            state.dataEnvrn->OutWetBulbTemp = ratedOutdoorAirWetBulb;
            state.dataEnvrn->OutBaroPress = DataEnvironment::StdPressureSeaLevel; // assume rating is for sea level.
            state.dataEnvrn->OutHumRat =
                Psychrometrics::PsyWFnTdbTwbPb(state, RatedOutdoorAirTemp, ratedOutdoorAirWetBulb, DataEnvironment::StdPressureSeaLevel, RoutineName);

            this->performance.simulate(state,
                                       dummyEvapInlet,
                                       dummyEvapOutlet,
                                       false,
                                       dummyPLR,
                                       dummySpeedNum,
                                       dummySpeedRatio,
                                       dummyFanOpMode,
                                       dummyCondInlet,
                                       dummyCondOutlet,
                                       dummySingleMode);

            // reset outdoor conditions back to previous state
            state.dataEnvrn->OutDryBulbTemp = holdOutDryBulbTemp;
            state.dataEnvrn->OutWetBulbTemp = holdOutWetBulb;
            state.dataEnvrn->OutBaroPress = holdOutBaroPress;
            state.dataEnvrn->OutHumRat = holdOutHumRat;

            // Real64 const coolingRate = dummyEvapInlet.MassFlowRate * (dummyEvapInlet.Enthalpy - dummyEvapOutlet.Enthalpy);
            // Real64 const thisMinAirHumRat = min(dummyEvapInlet.HumRat, dummyEvapOutlet.HumRat);
            // Real64 const sensCoolingRate = dummyEvapInlet.MassFlowRate * (Psychrometrics::PsyHFnTdbW(dummyEvapInlet.Temp, thisMinAirHumRat) -
            //                                                              Psychrometrics::PsyHFnTdbW(dummyEvapOutlet.Temp, thisMinAirHumRat));
            Real64 coolingRate = 0.0;
            Real64 sensCoolingRate = 0.0;
            Real64 latCoolingRate = 0.0;
            CalcComponentSensibleLatentOutput(dummyEvapInlet.MassFlowRate,
                                              dummyEvapInlet.Temp,
                                              dummyEvapInlet.HumRat,
                                              dummyEvapOutlet.Temp,
                                              dummyEvapOutlet.HumRat,
                                              sensCoolingRate,
                                              latCoolingRate,
                                              coolingRate);

            Real64 const ratedOutletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(
                state, dummyEvapOutlet.Temp, dummyEvapOutlet.HumRat, DataEnvironment::StdPressureSeaLevel, "Coil:Cooling:DX::simulate");
            state.dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(state,
                                                                                       this->name,
                                                                                       state.dataCoilCooingDX->coilCoolingDXObjectName,
                                                                                       coolingRate,
                                                                                       sensCoolingRate,
                                                                                       ratedInletEvapMassFlowRate,
                                                                                       RatedInletAirTemp,
                                                                                       dummyInletAirHumRat,
                                                                                       RatedInletWetBulbTemp,
                                                                                       dummyEvapOutlet.Temp,
                                                                                       dummyEvapOutlet.HumRat,
                                                                                       ratedOutletWetBulb,
                                                                                       RatedOutdoorAirTemp,
                                                                                       ratedOutdoorAirWetBulb,
                                                                                       this->normModeNomSpeed().RatedCBF,
                                                                                       -999.0);

            this->reportCoilFinalSizes = false;
        }
    }
}

void CoilCoolingDX::setToHundredPercentDOAS()
{
    for (auto &speed : this->performance.normalMode.speeds) {
        speed.minRatedVolFlowPerRatedTotCap = DataHVACGlobals::MinRatedVolFlowPerRatedTotCap2;
        speed.maxRatedVolFlowPerRatedTotCap = DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap2;
    }
    if (this->performance.hasAlternateMode) {
        for (auto &speed : this->performance.alternateMode.speeds) {
            speed.minRatedVolFlowPerRatedTotCap = DataHVACGlobals::MinRatedVolFlowPerRatedTotCap2;
            speed.maxRatedVolFlowPerRatedTotCap = DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap2;
        }
    }
}

void CoilCoolingDX::passThroughNodeData(EnergyPlus::DataLoopNode::NodeData &in, EnergyPlus::DataLoopNode::NodeData &out)
{
    // pass through all the other node variables that we don't update as a part of this model calculation
    out.MassFlowRate = in.MassFlowRate;
    out.Press = in.Press;
    out.Quality = in.Quality;
    out.MassFlowRateMax = in.MassFlowRateMax;
    out.MassFlowRateMin = in.MassFlowRateMin;
    out.MassFlowRateMaxAvail = in.MassFlowRateMaxAvail;
    out.MassFlowRateMinAvail = in.MassFlowRateMinAvail;
}

void CoilCoolingDX::reportAllStandardRatings(EnergyPlus::EnergyPlusData &state)
{

    if (!state.dataCoilCooingDX->coilCoolingDXs.empty()) {
        Real64 const ConvFromSIToIP(3.412141633); // Conversion from SI to IP [3.412 Btu/hr-W]
        static constexpr auto Format_990("! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, Standard Rating (Net) "
                                         "Cooling Capacity {W}, Standard Rated Net COP {W/W}, EER {Btu/W-h}, SEER {Btu/W-h}, IEER {Btu/W-h}\n");
        print(state.files.eio, "{}", Format_990);
        for (auto &coil : state.dataCoilCooingDX->coilCoolingDXs) {
            coil.performance.calcStandardRatings210240(state);

            static constexpr auto Format_991(" DX Cooling Coil Standard Rating Information, {}, {}, {:.1R}, {:.2R}, {:.2R}, {:.2R}, {:.2R}\n");
            print(state.files.eio,
                  Format_991,
                  "Coil:Cooling:DX",
                  coil.name,
                  coil.performance.standardRatingCoolingCapacity,
                  coil.performance.standardRatingEER,
                  coil.performance.standardRatingEER * ConvFromSIToIP,
                  coil.performance.standardRatingSEER * ConvFromSIToIP,
                  coil.performance.standardRatingIEER * ConvFromSIToIP);

            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilType, coil.name, "Coil:Cooling:DX");
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSI, coil.name, coil.performance.standardRatingCoolingCapacity, 1);
            // W/W is the same as Btuh/Btuh so that's fine too
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDXCoolCoilCOP, coil.name, coil.performance.standardRatingEER, 2);
            // Btu/W-h will convert to itself
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDXCoolCoilEERIP, coil.name, coil.performance.standardRatingEER * ConvFromSIToIP, 2);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDXCoolCoilSEERUserIP, coil.name, coil.performance.standardRatingSEER * ConvFromSIToIP, 2);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDXCoolCoilIEERIP, coil.name, coil.performance.standardRatingIEER * ConvFromSIToIP, 2);
            OutputReportPredefined::addFootNoteSubTable(
                state, state.dataOutRptPredefined->pdstDXCoolCoil, "ANSI/AHRI ratings account for supply air fan heat and electric power.");
        }
    }
    state.dataCoilCooingDX->stillNeedToReportStandardRatings = false;
}
