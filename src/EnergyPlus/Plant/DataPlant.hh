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

#ifndef DataPlant_hh_INCLUDED
#define DataPlant_hh_INCLUDED

// C++ Headers
#include <numeric>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/CallingOrder.hh>
#include <EnergyPlus/Plant/Enums.hh>
#include <EnergyPlus/Plant/Loop.hh>
#include <EnergyPlus/Plant/PlantAvailManager.hh>
#include <EnergyPlus/Plant/ReportLoopData.hh>

namespace EnergyPlus {

namespace DataPlant {

    // Using/Aliasing
    using DataLoopNode::SensedNodeFlagValue;

    constexpr int LoadRangeBasedMin(0);
    constexpr int LoadRangeBasedMax(2);

    // Criteria percentage limits for determining re-simulation of connected loop sides
    constexpr Real64 CriteriaDelta_MassFlowRate(0.001);
    constexpr Real64 CriteriaDelta_Temperature(0.010);
    constexpr Real64 CriteriaDelta_HeatTransferRate(0.100);

    // Parameters for loop side location
    constexpr int DemandSupply_No(0);
    constexpr int DemandSide(1);
    constexpr int SupplySide(2);

    // Parameters for tolerance
    constexpr Real64 LoopDemandTol(0.1);   // minimum significant loop cooling or heating demand
    constexpr Real64 DeltaTempTol(0.0001); // minimum significant loop temperature difference

    constexpr std::string_view cPressureSimType(DataPlant::iPressSimType const &d)
    {
        switch (d) {
        case DataPlant::iPressSimType::NoPressure:
            return "NONE";
        case DataPlant::iPressSimType::PumpPowerCorrection:
            return "PUMPPOWERCORRECTION";
        case DataPlant::iPressSimType::FlowCorrection:
            return "LOOPFLOWCORRECTION";
        case DataPlant::iPressSimType::FlowSimulation:
            return "PRESSURESIMULATION";
        default:
            assert(false);
            return "";
        }
    }

    // Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
    extern Array1D_string const SimPlantEquipTypes;
    extern Array1D<LoopType> const ValidLoopEquipTypes;

    constexpr std::array<std::string_view, static_cast<int>(PlantEquipmentType::Num)> ccSimPlantEquipTypes{
        "Invalid",
        "Boiler:HotWater",
        "Boiler:Steam",
        "Chiller:Absorption",
        "Chiller:Absorption:Indirect",
        "Chiller:CombustionTurbine",
        "Chiller:ConstantCOP",
        "ChillerHeater:Absorption:DirectFired",
        "Chiller:Electric",
        "Chiller:Electric:EIR",
        "Chiller:Electric:ReformulatedEIR",
        "Chiller:EngineDriven",
        "CoolingTower:SingleSpeed",
        "CoolingTower:TwoSpeed",
        "CoolingTower:VariableSpeed",
        "Generator:Fuelcell:ExhaustGastoWaterHeatExchanger",
        "WaterHeater:HeatPump:PumpedCondenser",
        "Heatpump:WatertoWater:Equationfit:Cooling",
        "Heatpump:WatertoWater:Equationfit:Heating",
        "Heatpump:WatertoWater:ParameterEstimation:Cooling",
        "Heatpump:WatertoWater:ParameterEstimation:Heating",
        "Pipe:Adiabatic",
        "Pipe:Adiabatic:Steam",
        "Pipe:Outdoor",
        "Pipe:Indoor",
        "Pipe:Underground",
        "DistrictCooling",
        "DistrictHeating",
        "ThermalStorage:Ice:Detailed",
        "ThermalStorage:Ice:Simple",
        "TemperingValve",
        "WaterHeater:Mixed",
        "WaterHeater:Stratified",
        "Pump:VariableSpeed",
        "Pump:ConstantSpeed",
        "Pump:VariableSpeed:Condensate",
        "HeaderedPumps:VariableSpeed",
        "HeaderedPumps:ConstantSpeed",
        "WaterUse:Connections",
        "Coil:Cooling:Water",
        "Coil:Cooling:Water:DetailedGeometry",
        "Coil:Heating:Water",
        "Coil:Heating:Steam",
        "Solarcollector:Flatplate:Water",
        "LoadProfile:Plant",
        "GroundHeatExchanger:System",
        "GroundHeatExchanger:Surface",
        "GroundHeatExchanger:Pond",
        "Generator:Microturbine",
        "Generator:InternalCombustionEngine",
        "Generator:CombustionTurbine",
        "Generator:Microchp",
        "Generator:Fuelcell:StackCooler",
        "FluidCooler:SingleSpeed",
        "FluidCooler:TwoSpeed",
        "EvaporativeFluidCooler:SingleSpeed",
        "EvaporativeFluidCooler:TwoSpeed",
        "ThermalStorage:ChilledWater:Mixed",
        "ThermalStorage:ChilledWater:Stratified",
        "SolarCollector:FlatPlate:PhotovoltaicThermal",
        "ZoneHVAC:Baseboard:Convective:Water",
        "ZoneHVAC:Baseboard:RadiantConvective:Steam",
        "ZoneHVAC:Baseboard:RadiantConvective:Water",
        "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        "ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
        "AirTerminal:SingleDuct:ConstantVolume:CooledBeam",
        "Coil:Heating:WaterToAirHeatPump:EquationFit",
        "Coil:Cooling:WaterToAirHeatPump:EquationFit",
        "Coil:Heating:WaterToAirHeatPump:ParameterEstimation",
        "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation",
        "Refrigeration:Condenser:WaterCooled",
        "Refrigeration:CompressorRack",
        "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed",
        "ChillerHeater:Absorption:DoubleEffect",
        "PipingSystem:Underground:PipeCircuit",
        "SolarCollector:IntegralCollectorStorage",
        "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit",
        "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit",
        "PlantComponent:UserDefined",
        "Coil:UserDefined",
        "ZoneHVAC:ForcedAir:UserDefined",
        "AirTerminal:SingleDuct:UserDefined",
        "AirConditioner:VariableRefrigerantFlow",
        "GroundHeatExchanger:HorizontalTrench",
        "HeatExchanger:FluidToFluid",
        "PlantComponent:TemperatureSource",
        "CentralHeatPumpSystem",
        "AirLoopHVAC:UnitarySystem",
        "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
        "CoolingTower:VariableSpeed:Merkel",
        "SwimmingPool:Indoor",
        "GroundHeatExchanger:Slinky",
        "WaterHeater:HeatPump:WrappedCondenser",
        "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam",
        "ZoneHVAC:CoolingPanel:RadiantConvective:Water",
        "HeatPump:PlantLoop:EIR:Cooling",
        "HeatPump:PlantLoop:EIR:Heating"};

    extern Array1D<Real64> const ConvergenceHistoryARR;

    // These all are going to be hard coded for now, but when we move to C++20 we will have constexpr methods available to fix this
    // const Real64 sum_ConvergenceHistoryARR(sum(ConvergenceHistoryARR));
    // const Real64 square_sum_ConvergenceHistoryARR(pow_2(sum_ConvergenceHistoryARR));
    // const Real64 sum_square_ConvergenceHistoryARR(sum(pow(ConvergenceHistoryARR, 2)));
    constexpr Real64 sum_ConvergenceHistoryARR(-10.0);
    constexpr Real64 square_sum_ConvergenceHistoryARR(100.0);
    constexpr Real64 sum_square_ConvergenceHistoryARR(30.0);

} // namespace DataPlant

struct DataPlantData : BaseGlobalStruct
{

    int TotNumLoops = 0;     // number of plant and condenser loops
    int TotNumHalfLoops = 0; // number of half loops (2 * TotNumLoops)
    bool PlantFirstSizeCompleted = false;
    bool PlantFirstSizesOkayToFinalize = false; // true if plant sizing is finishing and can save results
    bool PlantReSizingCompleted = false;
    bool PlantFirstSizesOkayToReport = false;
    bool PlantFinalSizesOkayToReport = false;
    bool AnyEMSPlantOpSchemesInModel = false;
    int PlantManageSubIterations = 0; // tracks plant iterations to characterize solver
    int PlantManageHalfLoopCalls = 0; // tracks number of half loop calls
    Array1D<DataPlant::PlantLoopData> PlantLoop;
    Array1D<DataPlant::PlantAvailMgrData> PlantAvailMgr;
    Array1D<DataPlant::ReportLoopData> VentRepPlantSupplySide;
    Array1D<DataPlant::ReportLoopData> VentRepPlantDemandSide;
    Array1D<DataPlant::ReportLoopData> VentRepCondSupplySide;
    Array1D<DataPlant::ReportLoopData> VentRepCondDemandSide;
    Array1D<DataPlant::PlantCallingOrderInfoStruct> PlantCallingOrderInfo;

    void clear_state() override
    {
        this->TotNumLoops = 0;
        this->TotNumHalfLoops = 0;
        this->PlantFirstSizeCompleted = false;
        this->PlantFirstSizesOkayToFinalize = false;
        this->PlantReSizingCompleted = false;
        this->PlantFirstSizesOkayToReport = false;
        this->PlantFinalSizesOkayToReport = false;
        this->AnyEMSPlantOpSchemesInModel = false;
        this->PlantManageSubIterations = 0;
        this->PlantManageHalfLoopCalls = 0;
        this->PlantLoop.deallocate();
        this->PlantAvailMgr.deallocate();
        this->VentRepPlantSupplySide.deallocate();
        this->VentRepPlantDemandSide.deallocate();
        this->VentRepCondSupplySide.deallocate();
        this->VentRepCondDemandSide.deallocate();
        this->PlantCallingOrderInfo.deallocate();
    }
};

} // namespace EnergyPlus

#endif
