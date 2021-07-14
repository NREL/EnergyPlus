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
    constexpr int NumSimPlantEquipTypes(96);
    extern Array1D_string const SimPlantEquipTypes;
    extern Array1D_string const ccSimPlantEquipTypes;
    extern Array1D<LoopType> const ValidLoopEquipTypes;

    constexpr int TypeOf_Other(-1);
    constexpr int TypeOf_Boiler_Simple(1);
    constexpr int TypeOf_Boiler_Steam(2);
    constexpr int TypeOf_Chiller_Absorption(3);          // older BLAST absorption chiller
    constexpr int TypeOf_Chiller_Indirect_Absorption(4); // revised absorption chiller
    constexpr int TypeOf_Chiller_CombTurbine(5);
    constexpr int TypeOf_Chiller_ConstCOP(6);
    constexpr int TypeOf_Chiller_DFAbsorption(7);
    constexpr int TypeOf_Chiller_Electric(8); // basic BLAST Chiller
    constexpr int TypeOf_Chiller_ElectricEIR(9);
    constexpr int TypeOf_Chiller_ElectricReformEIR(10);
    constexpr int TypeOf_Chiller_EngineDriven(11);
    constexpr int TypeOf_CoolingTower_SingleSpd(12);
    constexpr int TypeOf_CoolingTower_TwoSpd(13);
    constexpr int TypeOf_CoolingTower_VarSpd(14);
    constexpr int TypeOf_Generator_FCExhaust(15);
    constexpr int TypeOf_HeatPumpWtrHeaterPumped(16);
    constexpr int TypeOf_HPWaterEFCooling(17);
    constexpr int TypeOf_HPWaterEFHeating(18);
    constexpr int TypeOf_HPWaterPECooling(19);
    constexpr int TypeOf_HPWaterPEHeating(20);
    constexpr int TypeOf_Pipe(21);
    constexpr int TypeOf_PipeSteam(22);
    constexpr int TypeOf_PipeExterior(23);
    constexpr int TypeOf_PipeInterior(24);
    constexpr int TypeOf_PipeUnderground(25);
    constexpr int TypeOf_PurchChilledWater(26);
    constexpr int TypeOf_PurchHotWater(27);
    constexpr int TypeOf_TS_IceDetailed(28);
    constexpr int TypeOf_TS_IceSimple(29);
    constexpr int TypeOf_ValveTempering(30);
    constexpr int TypeOf_WtrHeaterMixed(31);
    constexpr int TypeOf_WtrHeaterStratified(32);
    constexpr int TypeOf_PumpVariableSpeed(33);
    constexpr int TypeOf_PumpConstantSpeed(34);
    constexpr int TypeOf_PumpCondensate(35);
    constexpr int TypeOf_PumpBankVariableSpeed(36);
    constexpr int TypeOf_PumpBankConstantSpeed(37);
    constexpr int TypeOf_WaterUseConnection(38);
    constexpr int TypeOf_CoilWaterCooling(39);             // demand side component
    constexpr int TypeOf_CoilWaterDetailedFlatCooling(40); // demand side component
    constexpr int TypeOf_CoilWaterSimpleHeating(41);       // demand side component
    constexpr int TypeOf_CoilSteamAirHeating(42);          // demand side component
    constexpr int TypeOf_SolarCollectorFlatPlate(43);      // demand side component
    constexpr int TypeOf_PlantLoadProfile(44);             // demand side component
    constexpr int TypeOf_GrndHtExchgSystem(45);
    constexpr int TypeOf_GrndHtExchgSurface(46);
    constexpr int TypeOf_GrndHtExchgPond(47);
    constexpr int TypeOf_Generator_MicroTurbine(48); // newer FSEC turbine
    constexpr int TypeOf_Generator_ICEngine(49);
    constexpr int TypeOf_Generator_CTurbine(50); // older BLAST turbine
    constexpr int TypeOf_Generator_MicroCHP(51);
    constexpr int TypeOf_Generator_FCStackCooler(52);
    constexpr int TypeOf_FluidCooler_SingleSpd(53);
    constexpr int TypeOf_FluidCooler_TwoSpd(54);
    constexpr int TypeOf_EvapFluidCooler_SingleSpd(55);
    constexpr int TypeOf_EvapFluidCooler_TwoSpd(56);
    constexpr int TypeOf_ChilledWaterTankMixed(57);
    constexpr int TypeOf_ChilledWaterTankStratified(58);
    constexpr int TypeOf_PVTSolarCollectorFlatPlate(59);
    constexpr int TypeOf_Baseboard_Conv_Water(60);
    constexpr int TypeOf_Baseboard_Rad_Conv_Steam(61);
    constexpr int TypeOf_Baseboard_Rad_Conv_Water(62);
    constexpr int TypeOf_LowTempRadiant_VarFlow(63);
    constexpr int TypeOf_LowTempRadiant_ConstFlow(64);
    constexpr int TypeOf_CooledBeamAirTerminal(65);
    constexpr int TypeOf_CoilWAHPHeatingEquationFit(66);
    constexpr int TypeOf_CoilWAHPCoolingEquationFit(67);
    constexpr int TypeOf_CoilWAHPHeatingParamEst(68);
    constexpr int TypeOf_CoilWAHPCoolingParamEst(69);
    constexpr int TypeOf_RefrigSystemWaterCondenser(70);
    constexpr int TypeOf_RefrigerationWaterCoolRack(71);
    constexpr int TypeOf_MultiSpeedHeatPumpRecovery(72);
    constexpr int TypeOf_Chiller_ExhFiredAbsorption(73);
    constexpr int TypeOf_PipingSystemPipeCircuit(74);
    constexpr int TypeOf_SolarCollectorICS(75);
    constexpr int TypeOf_CoilVSWAHPHeatingEquationFit(76);
    constexpr int TypeOf_CoilVSWAHPCoolingEquationFit(77);
    constexpr int TypeOf_PlantComponentUserDefined(78);
    constexpr int TypeOf_CoilUserDefined(79);
    constexpr int TypeOf_ZoneHVACAirUserDefined(80);
    constexpr int TypeOf_AirTerminalUserDefined(81);
    constexpr int TypeOf_HeatPumpVRF(82);
    constexpr int TypeOf_GrndHtExchgHorizTrench(83);
    constexpr int TypeOf_FluidToFluidPlantHtExchg(84);
    constexpr int TypeOf_WaterSource(85);
    constexpr int TypeOf_CentralGroundSourceHeatPump(86);
    constexpr int TypeOf_UnitarySysRecovery(87);
    constexpr int TypeOf_PackagedTESCoolingCoil(88);
    constexpr int TypeOf_CoolingTower_VarSpdMerkel(89);
    constexpr int TypeOf_SwimmingPool_Indoor(90);
    constexpr int TypeOf_GrndHtExchgSlinky(91);
    constexpr int TypeOf_HeatPumpWtrHeaterWrapped(92);
    constexpr int TypeOf_FourPipeBeamAirTerminal(93);
    constexpr int TypeOf_CoolingPanel_Simple(94);
    constexpr int TypeOf_HeatPumpEIRCooling(95);
    constexpr int TypeOf_HeatPumpEIRHeating(96);

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
