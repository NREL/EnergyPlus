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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatPumpWaterToWaterCOOLING.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::HeatPumpWaterToWaterCOOLING {
// Module containing the routines dealing with the Water to Water Heat Pump (Cooling)

// MODULE INFORMATION:
//       AUTHOR         ARUN
//       DATE WRITTEN   7/18/2000
//       MODIFIED       ARUN: 6/27/2002: Cycle Time
//                      L Lawrie: V1.1.1 (5/20/2003) add meters and energy to several reporting variables
//                      L Lawrie: V1.1.1 (5/20/2003) restructure modules to comply with standard templates
//                      B Griffith, Sept 2010, plant upgrades, general fluid properties
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module simulates a water to Water Heat Pump (Cooling)

// METHODOLOGY EMPLOYED:
// This simulation is based on a set of selected parameters,
// Which are obtained using Parameter Estimation technique.

// Using/Aliasing
using namespace DataLoopNode;

// MODULE PARAMETER DEFINITIONS
std::string const ModuleCompName("HeatPump:WaterToWater:ParameterEstimation:Cooling");
std::string const ModuleCompNameUC("HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING");
std::string const GSHPRefrigerant("R22"); // refrigerant name and index

PlantComponent *GshpPeCoolingSpecs::factory(EnergyPlusData &state, const std::string &objectName)
{
    if (state.dataHPWaterToWaterClg->GetWWHPCoolingInput) {
        GetGshpInput(state);
        state.dataHPWaterToWaterClg->GetWWHPCoolingInput = false;
    }
    for (auto &wwhp : state.dataHPWaterToWaterClg->GSHP) {
        if (wwhp.Name == objectName) {
            return &wwhp;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state,
                   "WWHPCoolingFactory: Error getting inputs for heat pump named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void GshpPeCoolingSpecs::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, [[maybe_unused]] bool RunFlag)
{
    // Simulate the model for the Demand "MyLoad"
    if (calledFromLocation.loopNum == this->LoadLoopNum) { // chilled water loop
        this->initialize(state);
        this->calculate(state, CurLoad);
        this->update(state);
    } else if (calledFromLocation.loopNum == this->SourceLoopNum) { // condenser loop
        PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                            this->SourceLoopNum,
                                                            this->SourceLoopSideNum,
                                                            DataPlant::TypeOf_HPWaterEFCooling,
                                                            this->SourceSideInletNodeNum,
                                                            this->SourceSideOutletNodeNum,
                                                            this->QSource,
                                                            this->SourceSideWaterInletTemp,
                                                            this->SourceSideWaterOutletTemp,
                                                            this->SourceSideWaterMassFlowRate,
                                                            FirstHVACIteration);
    } else {
        ShowFatalError(state, "SimHPWatertoWaterCOOLING:: Invalid loop connection " + ModuleCompName + ", Requested Unit=" + this->Name);
    }
}

void GshpPeCoolingSpecs::getDesignCapacities([[maybe_unused]] EnergyPlusData &state,
                                             [[maybe_unused]] const PlantLocation &calledFromLocation,
                                             Real64 &MaxLoad,
                                             Real64 &MinLoad,
                                             Real64 &OptLoad)
{
    MinLoad = this->NomCap * this->MinPartLoadRat;
    MaxLoad = this->NomCap * this->MaxPartLoadRat;
    OptLoad = this->NomCap * this->OptPartLoadRat;
}

void GshpPeCoolingSpecs::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
{
    if (this->plantScanFlag) {
        // Locate the heating on the plant loops for later usage
        bool errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_HPWaterPECooling,
                                                this->SourceLoopNum,
                                                this->SourceLoopSideNum,
                                                this->SourceBranchNum,
                                                this->SourceCompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->SourceSideInletNodeNum,
                                                _);
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_HPWaterPECooling,
                                                this->LoadLoopNum,
                                                this->LoadLoopSideNum,
                                                this->LoadBranchNum,
                                                this->LoadCompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->LoadSideInletNodeNum,
                                                _);
        if (errFlag) {
            ShowFatalError(state, "InitGshp: Program terminated due to previous condition(s).");
        }

        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->LoadLoopNum, this->LoadLoopSideNum, this->SourceLoopNum, this->SourceLoopSideNum, this->WWHPPlantTypeOfNum, true);
        this->plantScanFlag = false;
    }
}

void GetGshpInput(EnergyPlusData &state)
{
    //       SUBROUTINE INFORMATION:
    //       AUTHOR:
    //       DATE WRITTEN:    April 1998

    // PURPOSE OF THIS SUBROUTINE:
    // This routine will get the input
    // required by the GSHP models.  As such
    // it will interact with the Input Scanner to retrieve
    // information from the input file, count the number of
    // GSHPs and begin to fill the
    // arrays associated with the typeGSHP.

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using DataPlant::TypeOf_HPWaterPECooling;
    using FluidProperties::FindRefrigerant;
    using NodeInputManager::GetOnlySingleNode;
    using PlantUtilities::RegisterPlantCompDesignFlow;
    using PlantUtilities::ScanPlantLoopsForObject;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int GSHPNum;                  // Gshp counter
    int NumAlphas;                // Number of elements in the alpha array
    int NumNums;                  // Number of elements in the numeric array
    int IOStat;                   // IO Status when calling get input subroutine
    Array1D_string AlphArray(5);  // character string data
    Array1D<Real64> NumArray(23); // numeric data

    bool ErrorsFound(false);

    state.dataHPWaterToWaterClg->NumGSHPs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ModuleCompNameUC);

    if (state.dataHPWaterToWaterClg->NumGSHPs <= 0) {
        ShowSevereError(state, "No Equipment found in SimGshp");
        ErrorsFound = true;
    }

    // Allocate Arrays
    state.dataHPWaterToWaterClg->GSHP.allocate(state.dataHPWaterToWaterClg->NumGSHPs);

    for (GSHPNum = 1; GSHPNum <= state.dataHPWaterToWaterClg->NumGSHPs; ++GSHPNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state, ModuleCompNameUC, GSHPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat);
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), ModuleCompNameUC, ErrorsFound);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name = AlphArray(1);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).WWHPPlantTypeOfNum = TypeOf_HPWaterPECooling;

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).COP = NumArray(1);
        if (NumArray(1) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":COP = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        // zero values for NumArray 3 - 6 checked in input - idd

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).NomCap = NumArray(2);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).MinPartLoadRat = NumArray(3);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).MaxPartLoadRat = NumArray(4);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).OptPartLoadRat = NumArray(5);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideVolFlowRate = NumArray(6);
        if (NumArray(6) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Load Side Vol Flow Rate = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideVolFlowRate = NumArray(7);
        if (NumArray(7) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Source Side Vol Flow Rate = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideUACoeff = NumArray(8);
        if (NumArray(9) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Load Side Heat Transfer Coefficient = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideUACoeff = NumArray(9);
        if (NumArray(8) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Source Side Heat Transfer Coefficient = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).CompPistonDisp = NumArray(10);
        if (NumArray(10) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Compressor Piston displacement/Stroke = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).CompClearanceFactor = NumArray(11);
        if (NumArray(11) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Compressor Clearance Factor = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).CompSucPressDrop = NumArray(12);
        if (NumArray(12) == 0.0) {
            ShowSevereError(state, ModuleCompName + ": Pressure Drop = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SuperheatTemp = NumArray(13);
        if (NumArray(13) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Source Side SuperHeat = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).PowerLosses = NumArray(14);
        if (NumArray(14) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Compressor Power Loss = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LossFactor = NumArray(15);
        if (NumArray(15) == 0.0) {
            ShowSevereError(state, ModuleCompName + ":Efficiency = 0.0, Heatpump=" + AlphArray(1));
            ErrorsFound = true;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).HighPressCutoff = NumArray(16);
        if (NumArray(16) == 0.0) {
            state.dataHPWaterToWaterClg->GSHP(GSHPNum).HighPressCutoff = 500000000.0;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LowPressCutoff = NumArray(17);
        if (NumArray(17) == 0.0) {
            state.dataHPWaterToWaterClg->GSHP(GSHPNum).LowPressCutoff = 0.0;
        }

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideInletNodeNum = GetOnlySingleNode(state,
                                                                                              AlphArray(2),
                                                                                              ErrorsFound,
                                                                                              "HeatPump:WaterToWater Cooling",
                                                                                              AlphArray(1),
                                                                                              DataLoopNode::NodeFluidType::Water,
                                                                                              DataLoopNode::NodeConnectionType::Inlet,
                                                                                              NodeInputManager::compFluidStream::Primary,
                                                                                              ObjectIsNotParent);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideOutletNodeNum = GetOnlySingleNode(state,
                                                                                               AlphArray(3),
                                                                                               ErrorsFound,
                                                                                               "HeatPump:WaterToWater Cooling",
                                                                                               AlphArray(1),
                                                                                               DataLoopNode::NodeFluidType::Water,
                                                                                               DataLoopNode::NodeConnectionType::Outlet,
                                                                                               NodeInputManager::compFluidStream::Primary,
                                                                                               ObjectIsNotParent);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideInletNodeNum = GetOnlySingleNode(state,
                                                                                            AlphArray(4),
                                                                                            ErrorsFound,
                                                                                            "HeatPump:WaterToWater Cooling",
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeFluidType::Water,
                                                                                            DataLoopNode::NodeConnectionType::Inlet,
                                                                                            NodeInputManager::compFluidStream::Secondary,
                                                                                            ObjectIsNotParent);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideOutletNodeNum = GetOnlySingleNode(state,
                                                                                             AlphArray(5),
                                                                                             ErrorsFound,
                                                                                             "HeatPump:WaterToWater Cooling",
                                                                                             AlphArray(1),
                                                                                             DataLoopNode::NodeFluidType::Water,
                                                                                             DataLoopNode::NodeConnectionType::Outlet,
                                                                                             NodeInputManager::compFluidStream::Secondary,
                                                                                             ObjectIsNotParent);

        // Test node sets
        TestCompSet(state, ModuleCompNameUC, AlphArray(1), AlphArray(2), AlphArray(3), "Condenser Water Nodes");
        TestCompSet(state, ModuleCompNameUC, AlphArray(1), AlphArray(4), AlphArray(5), "Chilled Water Nodes");

        // save the design source side flow rate for use by plant loop sizing algorithms
        RegisterPlantCompDesignFlow(state,
                                    state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideInletNodeNum,
                                    0.5 * state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideVolFlowRate);

        state.dataHPWaterToWaterClg->GSHP(GSHPNum).QLoad = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).QSource = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).Power = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideWaterInletTemp = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideWaterInletTemp = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideWaterOutletTemp = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideWaterOutletTemp = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideWaterMassFlowRate = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideWaterMassFlowRate = 0.0;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).IsOn = false;
        state.dataHPWaterToWaterClg->GSHP(GSHPNum).MustRun = true;
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors Found in getting Gshp input");
    }

    state.dataHPWaterToWaterClg->GSHPRefrigIndex = FindRefrigerant(state, GSHPRefrigerant);
    if (state.dataHPWaterToWaterClg->GSHPRefrigIndex == 0) {
        ShowFatalError(state, "Refrigerant for HeatPump:WaterToWater Heating not found, should have been=" + GSHPRefrigerant);
        ShowFatalError(state, "FluidProperties:* objects for " + GSHPRefrigerant + " must be included in the idf file.");
    }

    // CurrentModuleObject='HeatPump:WaterToWater:ParameterEstimation:Cooling'
    for (GSHPNum = 1; GSHPNum <= state.dataHPWaterToWaterClg->NumGSHPs; ++GSHPNum) {
        SetupOutputVariable(state,
                            "Heat Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Power,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Energy,
                            "System",
                            "Sum",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name,
                            _,
                            "Electricity",
                            "Cooling",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Heat Pump Load Side Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).QLoad,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).QLoadEnergy,
                            "System",
                            "Sum",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);

        SetupOutputVariable(state,
                            "Heat Pump Source Side Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).QSource,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).QSourceEnergy,
                            "System",
                            "Sum",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);

        SetupOutputVariable(state,
                            "Heat Pump Load Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideWaterOutletTemp,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideWaterInletTemp,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Outlet Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideWaterOutletTemp,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Inlet Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideWaterInletTemp,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Load Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).LoadSideWaterMassFlowRate,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
        SetupOutputVariable(state,
                            "Heat Pump Source Side Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).SourceSideWaterMassFlowRate,
                            "System",
                            "Average",
                            state.dataHPWaterToWaterClg->GSHP(GSHPNum).Name);
    }
}

void GshpPeCoolingSpecs::initialize(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    July 2007

    // PURPOSE OF THIS SUBROUTINE:
    // initialization

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("InitGshp");

    // For each new environment
    if (state.dataGlobal->BeginEnvrnFlag && this->beginEnvironFlag) {
        this->QLoad = 0.0;
        this->QSource = 0.0;
        this->Power = 0.0;
        this->QLoadEnergy = 0.0;
        this->QSourceEnergy = 0.0;
        this->Energy = 0.0;
        this->LoadSideWaterInletTemp = 0.0;
        this->SourceSideWaterInletTemp = 0.0;
        this->LoadSideWaterOutletTemp = 0.0;
        this->SourceSideWaterOutletTemp = 0.0;
        this->SourceSideWaterMassFlowRate = 0.0;
        this->LoadSideWaterMassFlowRate = 0.0;
        this->IsOn = false;
        this->MustRun = true;

        this->beginEnvironFlag = false;
        Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                       DataGlobalConstants::CWInitConvTemp,
                                                       state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                       RoutineName);
        this->LoadSideDesignMassFlow = this->LoadSideVolFlowRate * rho;

        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->LoadSideDesignMassFlow,
                                           this->LoadSideInletNodeNum,
                                           this->LoadSideOutletNodeNum,
                                           this->LoadLoopNum,
                                           this->LoadLoopSideNum,
                                           this->LoadBranchNum,
                                           this->LoadCompNum);

        rho = FluidProperties::GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName,
                                                DataGlobalConstants::CWInitConvTemp,
                                                state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex,
                                                RoutineName);
        this->SourceSideDesignMassFlow = this->SourceSideVolFlowRate * rho;

        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->SourceSideDesignMassFlow,
                                           this->SourceSideInletNodeNum,
                                           this->SourceSideOutletNodeNum,
                                           this->SourceLoopNum,
                                           this->SourceLoopSideNum,
                                           this->SourceBranchNum,
                                           this->SourceCompNum);

        state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp = 35.0;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) this->beginEnvironFlag = true;

    // Init more variables

    // On every call
    this->Running = 0;

    this->MustRun = true; // Reset MustRun Flag to TRUE

    this->LoadSideWaterMassFlowRate = 0.0;   // Load Side mass flow rate, water side
    this->SourceSideWaterMassFlowRate = 0.0; // Source Side mass flow rate, water side
    this->Power = 0.0;                       // power consumption
    this->QLoad = 0.0;                       // heat rejection from Load Side coil
    this->QSource = 0.0;
}

void GshpPeCoolingSpecs::calculate(EnergyPlusData &state, Real64 &MyLoad)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR
    //       DATE WRITTEN   Sept. 1998
    //       MODIFIED       April 1999
    //                      September 2002, SJR
    //       RE-ENGINEERED  Mar2000

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const gamma(1.114); // Expnasion Coefficient
    Real64 const HeatBalTol(0.0005);
    Real64 const RelaxParam(0.6);
    Real64 const SmallNum(1.0e-20);
    int const IterationLimit(500);
    static std::string const RoutineName("CalcGshpModel");
    static std::string const RoutineNameLoadSideRefridgTemp("CalcGSHPModel:LoadSideRefridgTemp");
    static std::string const RoutineNameSourceSideRefridgTemp("CalcGSHPModel:SourceSideRefridgTemp");
    static std::string const RoutineNameCompressInletTemp("CalcGSHPModel:CompressInletTemp");
    static std::string const RoutineNameSuctionPr("CalcGSHPModel:SuctionPr");
    static std::string const RoutineNameCompSuctionTemp("CalcGSHPModel:CompSuctionTemp");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SourceSideEffect;      // Source Side effectiveness
    Real64 LoadSideEffect;        // Load Side effectiveness
    Real64 SourceSideRefridgTemp; // Source Side temperature
    Real64 LoadSideRefridgTemp;   // Load Side temperature
    Real64 SourceSidePressure;    // Source Side pressure
    Real64 LoadSidePressure;      // Load Side pressure
    Real64 SuctionPr;             // Suction Pressure
    Real64 DischargePr;           // Discharge Pressure
    Real64 CompressInletTemp;     // Compressor inlet temperature
    Real64 MassRef;               // mass flow rate of refrigerant
    Real64 SourceSideOutletEnth;  // Enthalpy at Source Side pressure
    Real64 LoadSideOutletEnth;    // Enthalpy at Condensor Pressure
    Real64 initialQSource;        // Guess Source Side Heat rate
    Real64 initialQLoad;          // Guess Load Side Heat rate
    Real64 qual;                  // quality
    Real64 SuperHeatEnth;
    Real64 T110;
    Real64 T111;
    Real64 CompSuctionTemp;
    Real64 CompSuctionEnth;
    Real64 CompSuctionDensity;
    Real64 CompSuctionSatTemp;
    std::string ErrString;
    Real64 DutyFactor;
    int IterationCount;

    auto &CurrentSimTime = state.dataHPWaterToWaterClg->CurrentSimTime;
    auto &PrevSimTime = state.dataHPWaterToWaterClg->PrevSimTime;

    Real64 CpSourceSide; // local temporary for fluid specific heat
    Real64 CpLoadSide;   // local temporary for fluid specific heat

    if (PrevSimTime != CurrentSimTime) {
        PrevSimTime = CurrentSimTime;
    }

    // CALCULATE THE SIMULATION TIME
    CurrentSimTime = (state.dataGlobal->DayOfSim - 1) * 24 + state.dataGlobal->HourOfDay - 1 +
                     (state.dataGlobal->TimeStep - 1) * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed;

    if (MyLoad < 0.0) {
        this->MustRun = true;
        this->IsOn = true;
    } else {
        this->MustRun = false;
        this->IsOn = false;
    }

    //*******Set flow based on "flowlock" and "run" flags**********
    // Set flows if the heat pump is not running
    if (!this->MustRun) {
        this->LoadSideWaterMassFlowRate = 0.0;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->LoadSideWaterMassFlowRate,
                                             this->LoadSideInletNodeNum,
                                             this->LoadSideOutletNodeNum,
                                             this->LoadLoopNum,
                                             this->LoadLoopSideNum,
                                             this->LoadBranchNum,
                                             this->LoadCompNum);
        this->SourceSideWaterMassFlowRate = 0.0;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->SourceSideWaterMassFlowRate,
                                             this->SourceSideInletNodeNum,
                                             this->SourceSideOutletNodeNum,
                                             this->SourceLoopNum,
                                             this->SourceLoopSideNum,
                                             this->SourceBranchNum,
                                             this->SourceCompNum);
        PlantUtilities::PullCompInterconnectTrigger(state,
                                                    this->LoadLoopNum,
                                                    this->LoadLoopSideNum,
                                                    this->LoadBranchNum,
                                                    this->LoadCompNum,
                                                    this->CondMassFlowIndex,
                                                    this->SourceLoopNum,
                                                    this->LoadLoopSideNum,
                                                    DataPlant::iCriteriaType::MassFlowRate,
                                                    this->SourceSideWaterMassFlowRate);
        // now initialize simulation variables for "heat pump off"
        this->QLoad = 0.0;
        this->QSource = 0.0;
        this->Power = 0.0;
        this->LoadSideWaterInletTemp = state.dataLoopNodes->Node(this->LoadSideInletNodeNum).Temp;
        this->LoadSideWaterOutletTemp = LoadSideWaterInletTemp;
        this->SourceSideWaterInletTemp = state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp;
        this->SourceSideWaterOutletTemp = SourceSideWaterInletTemp;
        return; // if heat pump is not running return without simulation

        // Set flows if the heat pump is running
    } else { // the heat pump must run
        this->LoadSideWaterMassFlowRate = this->LoadSideDesignMassFlow;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->LoadSideWaterMassFlowRate,
                                             this->LoadSideInletNodeNum,
                                             this->LoadSideOutletNodeNum,
                                             this->LoadLoopNum,
                                             this->LoadLoopSideNum,
                                             this->LoadBranchNum,
                                             this->LoadCompNum);

        this->SourceSideWaterMassFlowRate = this->SourceSideDesignMassFlow;
        PlantUtilities::SetComponentFlowRate(state,
                                             this->SourceSideWaterMassFlowRate,
                                             this->SourceSideInletNodeNum,
                                             this->SourceSideOutletNodeNum,
                                             this->SourceLoopNum,
                                             this->SourceLoopSideNum,
                                             this->SourceBranchNum,
                                             this->SourceCompNum);
        // get inlet temps
        this->LoadSideWaterInletTemp = state.dataLoopNodes->Node(this->LoadSideInletNodeNum).Temp;
        this->SourceSideWaterInletTemp = state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp;
        // if there's no flow, turn the "heat pump off"
        if (this->LoadSideWaterMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance ||
            this->SourceSideWaterMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
            this->LoadSideWaterMassFlowRate = 0.0;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->LoadSideWaterMassFlowRate,
                                                 this->LoadSideInletNodeNum,
                                                 this->LoadSideOutletNodeNum,
                                                 this->LoadLoopNum,
                                                 this->LoadLoopSideNum,
                                                 this->LoadBranchNum,
                                                 this->LoadCompNum);
            this->SourceSideWaterMassFlowRate = 0.0;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->SourceSideWaterMassFlowRate,
                                                 this->SourceSideInletNodeNum,
                                                 this->SourceSideOutletNodeNum,
                                                 this->SourceLoopNum,
                                                 this->SourceLoopSideNum,
                                                 this->SourceBranchNum,
                                                 this->SourceCompNum);
            PlantUtilities::PullCompInterconnectTrigger(state,
                                                        this->LoadLoopNum,
                                                        this->LoadLoopSideNum,
                                                        this->LoadBranchNum,
                                                        this->LoadCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->SourceLoopNum,
                                                        this->LoadLoopSideNum,
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->SourceSideWaterMassFlowRate);
            this->QLoad = 0.0;
            this->QSource = 0.0;
            this->Power = 0.0;
            this->LoadSideWaterInletTemp = state.dataLoopNodes->Node(this->LoadSideInletNodeNum).Temp;
            this->LoadSideWaterOutletTemp = LoadSideWaterInletTemp;
            this->SourceSideWaterInletTemp = state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp;
            this->SourceSideWaterOutletTemp = SourceSideWaterInletTemp;
            return;
        }
        PlantUtilities::PullCompInterconnectTrigger(state,
                                                    this->LoadLoopNum,
                                                    this->LoadLoopSideNum,
                                                    this->LoadBranchNum,
                                                    this->LoadCompNum,
                                                    this->CondMassFlowIndex,
                                                    this->SourceLoopNum,
                                                    this->LoadLoopSideNum,
                                                    DataPlant::iCriteriaType::MassFlowRate,
                                                    this->SourceSideWaterMassFlowRate);
    }

    //**********BEGIN THE CALCULATION**************

    // initialize the source and load side heat transfer rates for the simulation
    initialQSource = 0.0;
    initialQLoad = 0.0;
    IterationCount = 0;

    CpSourceSide = FluidProperties::GetSpecificHeatGlycol(state,
                                                          state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName,
                                                          this->SourceSideWaterInletTemp,
                                                          state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex,
                                                          RoutineName);

    CpLoadSide = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                        this->LoadSideWaterInletTemp,
                                                        state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                        RoutineName);

    // Determine effectiveness of Load Side
    LoadSideEffect = 1.0 - std::exp(-this->LoadSideUACoeff / (CpLoadSide * this->LoadSideWaterMassFlowRate));
    // Determine effectiveness of Source Side
    SourceSideEffect = 1.0 - std::exp(-this->SourceSideUACoeff / (CpSourceSide * this->SourceSideWaterMassFlowRate));

    // main iteration loop to solve model equations
    while (true) {
        ++IterationCount;

        // To determine Load Side temperature
        LoadSideRefridgTemp = this->LoadSideWaterInletTemp - initialQLoad / (LoadSideEffect * CpLoadSide * this->LoadSideWaterMassFlowRate);

        // Determine Source Side tempertaure
        SourceSideRefridgTemp =
            this->SourceSideWaterInletTemp + initialQSource / (SourceSideEffect * CpSourceSide * this->SourceSideWaterMassFlowRate);

        // Determine the evaporating and condensing pressures
        SourceSidePressure = FluidProperties::GetSatPressureRefrig(
            state, GSHPRefrigerant, SourceSideRefridgTemp, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineName);
        LoadSidePressure = FluidProperties::GetSatPressureRefrig(
            state, GSHPRefrigerant, LoadSideRefridgTemp, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineName);

        if (SourceSidePressure < this->LowPressCutoff) {
            ShowSevereError(state, ModuleCompName + "=\"" + this->Name + "\" Cooling Source Side Pressure Less than the Design Minimum");
            ShowContinueError(state,
                              format("Cooling Source Side Pressure={:.2T} and user specified Design Minimum Pressure={:.2T}",
                                     SourceSidePressure,
                                     this->LowPressCutoff));
            ShowContinueErrorTimeStamp(state, "");
            ShowFatalError(state, "Preceding Conditions cause termination.");
        }

        if (LoadSidePressure > this->HighPressCutoff) {
            ShowSevereError(state, ModuleCompName + "=\"" + this->Name + "\" Cooling Load Side Pressure greater than the Design Maximum");
            ShowContinueError(state,
                              format("Cooling Load Side Pressure={:.2T} and user specified Design Maximum Pressure={:.2T}",
                                     LoadSidePressure,
                                     this->HighPressCutoff));
            ShowContinueErrorTimeStamp(state, "");
            ShowFatalError(state, "Preceding Conditions cause termination.");
        }
        // Determine Suction Pressure at compressor inlet
        SuctionPr = LoadSidePressure - this->CompSucPressDrop;
        // Determine Discharge Pressure at compressor exit
        DischargePr = SourceSidePressure + this->CompSucPressDrop;

        if (SuctionPr < this->LowPressCutoff) {
            ShowSevereError(state, ModuleCompName + "=\"" + this->Name + "\" Cooling Suction Pressure Less than the Design Minimum");
            ShowContinueError(
                state, format("Cooling Suction Pressure={:.2T} and user specified Design Minimum Pressure={:.2T}", SuctionPr, this->LowPressCutoff));
            ShowContinueErrorTimeStamp(state, "");
            ShowFatalError(state, "Preceding Conditions cause termination.");
        }

        if (DischargePr > this->HighPressCutoff) {
            ShowSevereError(state, ModuleCompName + "=\"" + this->Name + "\" Cooling Discharge Pressure greater than the Design Maximum");
            ShowContinueError(
                state,
                format("Cooling Discharge Pressure={:.2T} and user specified Design Maximum Pressure={:.2T}", DischargePr, this->HighPressCutoff));
            ShowContinueErrorTimeStamp(state, "");
            ShowFatalError(state, "Preceding Conditions cause termination.");
        }

        // Determine the Source Side Outlet Enthalpy

        qual = 1.0;
        LoadSideOutletEnth = FluidProperties::GetSatEnthalpyRefrig(
            state, GSHPRefrigerant, LoadSideRefridgTemp, qual, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineNameLoadSideRefridgTemp);

        qual = 0.0;
        SourceSideOutletEnth = FluidProperties::GetSatEnthalpyRefrig(
            state, GSHPRefrigerant, SourceSideRefridgTemp, qual, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineNameSourceSideRefridgTemp);

        // Determine Load Side Outlet Enthalpy
        // Determine superheated temperature of the LoadSide outlet/compressor inlet
        CompressInletTemp = LoadSideRefridgTemp + this->SuperheatTemp;

        // Determine the enathalpy of the super heated fluid at Source Side outlet
        SuperHeatEnth = FluidProperties::GetSupHeatEnthalpyRefrig(
            state, GSHPRefrigerant, CompressInletTemp, LoadSidePressure, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineNameCompressInletTemp);

        // Determining the suction state of the fluid from inlet state involves interation
        // Method employed...
        // Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
        // check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

        // this routine was reenginerred from HVACsim + takes pressure in Pascals, tolrance, refrgerant # R22 =6
        CompSuctionSatTemp = FluidProperties::GetSatTemperatureRefrig(
            state, GSHPRefrigerant, SuctionPr, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineNameSuctionPr);

        T110 = CompSuctionSatTemp;
        //            Shoot into the super heated region
        T111 = CompSuctionSatTemp + 100.0;
        // Iterate to find the Suction State
        while (true) {
            CompSuctionTemp = 0.5 * (T110 + T111);

            CompSuctionEnth = FluidProperties::GetSupHeatEnthalpyRefrig(
                state, GSHPRefrigerant, CompSuctionTemp, SuctionPr, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineNameCompSuctionTemp);

            if (std::abs(CompSuctionEnth - SuperHeatEnth) / SuperHeatEnth < 0.0001) {
                goto LOOP_exit;
            }

            if (CompSuctionEnth < SuperHeatEnth) {
                T110 = CompSuctionTemp;
            } else {
                T111 = CompSuctionTemp;
            }
        }
    LOOP_exit:;

        // Determine the Mass flow rate of refrigerant
        CompSuctionDensity = FluidProperties::GetSupHeatDensityRefrig(
            state, GSHPRefrigerant, CompSuctionTemp, SuctionPr, state.dataHPWaterToWaterClg->GSHPRefrigIndex, RoutineNameCompSuctionTemp);
        MassRef = this->CompPistonDisp * CompSuctionDensity *
                  (1 + this->CompClearanceFactor - this->CompClearanceFactor * std::pow(DischargePr / SuctionPr, 1 / gamma));

        // Find the Source Side Heat Transfer

        this->QLoad = MassRef * (LoadSideOutletEnth - SourceSideOutletEnth);

        // Determine the theoretical power
        this->Power = this->PowerLosses + (MassRef * gamma / (gamma - 1) * SuctionPr / CompSuctionDensity / this->LossFactor *
                                           (std::pow(DischargePr / SuctionPr, (gamma - 1) / gamma) - 1));

        // Determine the Loadside HeatRate (this->QLoad)
        this->QSource = this->Power + this->QLoad;
        // convergence and iteration limit check
        if (std::abs((this->QSource - initialQSource) / (initialQSource + SmallNum)) < HeatBalTol || IterationCount > IterationLimit) {
            if (IterationCount > IterationLimit) {
                ShowWarningError(state, "HeatPump:WaterToWater:ParameterEstimation, Cooling did not converge");
                ShowContinueErrorTimeStamp(state, "");
                ShowContinueError(state, "Heatpump Name = " + this->Name);
                ShowContinueError(
                    state,
                    format("Heat Inbalance (%)             = {}", std::abs(100.0 * (this->QSource - initialQSource) / (initialQSource + SmallNum))));
                ShowContinueError(state, format("Load-side heat transfer rate   = {}", this->QLoad));
                ShowContinueError(state, format("Source-side heat transfer rate = {}", this->QSource));
                ShowContinueError(state, format("Source-side mass flow rate     = {}", this->SourceSideWaterMassFlowRate));
                ShowContinueError(state, format("Load-side mass flow rate       = {}", this->LoadSideWaterMassFlowRate));
                ShowContinueError(state, format("Source-side inlet temperature  = {}", this->SourceSideWaterInletTemp));
                ShowContinueError(state, format("Load-side inlet temperature    = {}", this->LoadSideWaterInletTemp));
            }
            goto LOOPSourceEnth_exit;

        } else { // update load
            initialQSource += RelaxParam * (this->QSource - initialQSource);
            initialQLoad += RelaxParam * (this->QLoad - initialQLoad);
        }
    }
LOOPSourceEnth_exit:;

    // Control Strategy
    if (std::abs(MyLoad) < this->QLoad) {
        DutyFactor = std::abs(MyLoad) / this->QLoad;
        this->QLoad = std::abs(MyLoad);
        this->Power *= DutyFactor;
        this->QSource *= DutyFactor;
        // Determine the Exterior fluid temperature at the Load Side oulet and evaporator outlet...
        this->LoadSideWaterOutletTemp = this->LoadSideWaterInletTemp - this->QLoad / (this->LoadSideWaterMassFlowRate * CpLoadSide); // Chilled water
        this->SourceSideWaterOutletTemp =
            this->SourceSideWaterInletTemp + this->QSource / (this->SourceSideWaterMassFlowRate * CpSourceSide); // cooling water
        return;
    }

    this->LoadSideWaterOutletTemp = this->LoadSideWaterInletTemp - this->QLoad / (this->LoadSideWaterMassFlowRate * CpLoadSide); // Chilled water
    this->SourceSideWaterOutletTemp = this->SourceSideWaterInletTemp + this->QSource / (this->SourceSideWaterMassFlowRate * CpSourceSide);
    this->Running = 1;
}

void GshpPeCoolingSpecs::update(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    October 1998

    if (!this->MustRun) {
        // set node temperatures
        state.dataLoopNodes->Node(this->SourceSideOutletNodeNum).Temp = state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp;
        state.dataLoopNodes->Node(this->LoadSideOutletNodeNum).Temp = state.dataLoopNodes->Node(this->LoadSideInletNodeNum).Temp;
        this->Power = 0.0;
        this->Energy = 0.0;
        this->QSource = 0.0;
        this->QLoad = 0.0;
        this->QSourceEnergy = 0.0;
        this->QLoadEnergy = 0.0;
        this->SourceSideWaterInletTemp = state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp;
        this->SourceSideWaterOutletTemp = state.dataLoopNodes->Node(this->SourceSideOutletNodeNum).Temp;
        this->LoadSideWaterInletTemp = state.dataLoopNodes->Node(this->LoadSideInletNodeNum).Temp;
        this->LoadSideWaterOutletTemp = state.dataLoopNodes->Node(this->LoadSideOutletNodeNum).Temp;

    } else {
        // set node temperatures
        state.dataLoopNodes->Node(this->LoadSideOutletNodeNum).Temp = this->LoadSideWaterOutletTemp;
        state.dataLoopNodes->Node(this->SourceSideOutletNodeNum).Temp = this->SourceSideWaterOutletTemp;

        // set node flow rates;  for these load based models
        // assume that the sufficient Source Side flow rate available

        Real64 const ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        this->Energy = this->Power * ReportingConstant;
        this->QSourceEnergy = QSource * ReportingConstant;
        this->QLoadEnergy = QLoad * ReportingConstant;
        this->SourceSideWaterInletTemp = state.dataLoopNodes->Node(this->SourceSideInletNodeNum).Temp;
        this->LoadSideWaterInletTemp = state.dataLoopNodes->Node(this->LoadSideInletNodeNum).Temp;
    }
}

} // namespace EnergyPlus::HeatPumpWaterToWaterCOOLING
