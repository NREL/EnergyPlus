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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPhotovoltaics.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace PhotovoltaicThermalCollectors {

    // Module containing the routines dealing with the photovoltaic thermal collectors

    // MODULE INFORMATION:
    //       AUTHOR         Brent. Griffith
    //       DATE WRITTEN   June-August 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // collect models related to PVT or hybrid, photovoltaic - thermal solar collectors

    // METHODOLOGY EMPLOYED:
    // The approach is to have one PVT structure that works with different models.
    //  the PVT model reuses photovoltaic modeling in Photovoltaics.cc for electricity generation.
    //  the electric load center and "generator" is all accessed thru PV objects and models.
    //  this module is for the thermal portion of PVT.
    //  the first model is a "simple" or "ideal" model useful for sizing, early design, or policy analyses
    //  Simple PV/T model just converts incoming solar to electricity and temperature rise of a working fluid.

    int const SimplePVTmodel(1001);

    Real64 const SimplePVTWaterSizeFactor(1.905e-5); // [ m3/s/m2 ] average of collectors in SolarCollectors.idf

    PlantComponent *PVTCollectorStruct::factory(EnergyPlusData &state, std::string_view objectName)
    {
        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        for (auto &thisComp : state.dataPhotovoltaicThermalCollector->PVT) {
            if (thisComp.Name == objectName) {
                return &thisComp;
            }
        }

        // If we didn't find it, fatal
        ShowFatalError(state, "Solar Thermal Collector Factory: Error getting inputs for object named: " + std::string{objectName});
        // Shut up the compiler
        return nullptr;
    }

    void PVTCollectorStruct::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
    {
        this->initialize(state, true);
        this->size(state);
    }

    void PVTCollectorStruct::simulate(EnergyPlusData &state,
                                      [[maybe_unused]] const PlantLocation &calledFromLocation,
                                      bool const FirstHVACIteration,
                                      [[maybe_unused]] Real64 &CurLoad,
                                      [[maybe_unused]] bool const RunFlag)
    {

        this->initialize(state, FirstHVACIteration);
        this->control(state);
        this->calculate(state);
        this->update(state);
    }

    void GetPVTcollectorsInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get input for PVT objects

        int Item;                // Item to be "gotten"
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine

        // Object Data
        Array1D<SimplePVTModelStruct> tmpSimplePVTperf;

        // first load the performance object info into temporary structure
        state.dataIPShortCut->cCurrentModuleObject = "SolarCollectorPerformance:PhotovoltaicThermal:Simple";
        int NumSimplePVTPerform = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        if (NumSimplePVTPerform > 0) {
            tmpSimplePVTperf.allocate(NumSimplePVTPerform);
            for (Item = 1; Item <= NumSimplePVTPerform; ++Item) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                                         Item,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound))
                    continue;

                tmpSimplePVTperf(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Fixed")) {
                    tmpSimplePVTperf(Item).ThermEfficMode = ThermEfficEnum::FIXED;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Scheduled")) {
                    tmpSimplePVTperf(Item).ThermEfficMode = ThermEfficEnum::SCHEDULED;
                } else {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                tmpSimplePVTperf(Item).ThermalActiveFract = state.dataIPShortCut->rNumericArgs(1);
                tmpSimplePVTperf(Item).ThermEffic = state.dataIPShortCut->rNumericArgs(2);

                tmpSimplePVTperf(Item).ThermEffSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                if ((tmpSimplePVTperf(Item).ThermEffSchedNum == 0) && (tmpSimplePVTperf(Item).ThermEfficMode == ThermEfficEnum::SCHEDULED)) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                tmpSimplePVTperf(Item).SurfEmissivity = state.dataIPShortCut->rNumericArgs(3);
            }
        } // NumSimplePVTPerform > 0

        // now get main PVT objects
        state.dataIPShortCut->cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
        state.dataPhotovoltaicThermalCollector->NumPVT =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        state.dataPhotovoltaicThermalCollector->PVT.allocate(state.dataPhotovoltaicThermalCollector->NumPVT);

        for (Item = 1; Item <= state.dataPhotovoltaicThermalCollector->NumPVT; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound))
                continue;

            state.dataPhotovoltaicThermalCollector->PVT(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataPhotovoltaicThermalCollector->PVT(Item).TypeNum = DataPlant::TypeOf_PVTSolarCollectorFlatPlate;

            state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataSurface->Surface);
            // check surface
            if (state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));

                    ShowContinueError(state, "Surface name cannot be blank.");
                } else {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Surface was not found.");
                }
                ErrorsFound = true;
            } else {

                if (!state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).ExtSolar) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Surface must be exposed to solar.");
                    ErrorsFound = true;
                }
                // check surface orientation, warn if upside down
                if ((state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Tilt < -95.0) ||
                    (state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Tilt > 95.0)) {
                    ShowWarningError(state,
                                     "Suspected input problem with " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " +
                                         state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Surface used for solar collector faces down");
                    ShowContinueError(state,
                                      format("Surface tilt angle (degrees from ground outward normal) = {:.2R}",
                                             state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Tilt));
                }

            } // check surface

            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(3) + ", name cannot be blank.");
                ErrorsFound = true;
            } else {
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelName = state.dataIPShortCut->cAlphaArgs(3);
                int ThisParamObj = UtilityRoutines::FindItemInList(state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelName, tmpSimplePVTperf);
                if (ThisParamObj > 0) {
                    state.dataPhotovoltaicThermalCollector->PVT(Item).Simple = tmpSimplePVTperf(ThisParamObj); // entire structure assigned
                    // do one-time setups on input data
                    state.dataPhotovoltaicThermalCollector->PVT(Item).AreaCol =
                        state.dataSurface->Surface(state.dataPhotovoltaicThermalCollector->PVT(Item).SurfNum).Area *
                        state.dataPhotovoltaicThermalCollector->PVT(Item).Simple.ThermalActiveFract;
                    state.dataPhotovoltaicThermalCollector->PVT(Item).PVTModelType = SimplePVTmodel;
                } else {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(3) + ", was not found.");
                    ErrorsFound = true;
                }
            }
            if (allocated(state.dataPhotovoltaic->PVarray)) { // then PV input gotten... but don't expect this to be true.
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVnum =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(4), state.dataPhotovoltaic->PVarray);
                // check PV
                if (state.dataPhotovoltaicThermalCollector->PVT(Item).PVnum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else {
                    state.dataPhotovoltaicThermalCollector->PVT(Item).PVname = state.dataIPShortCut->cAlphaArgs(4);
                    state.dataPhotovoltaicThermalCollector->PVT(Item).PVfound = true;
                }
            } else { // no PV or not yet gotten.
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVname = state.dataIPShortCut->cAlphaArgs(4);
                state.dataPhotovoltaicThermalCollector->PVT(Item).PVfound = false;
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Water")) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType = WorkingFluidEnum::LIQUID;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Air")) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType = WorkingFluidEnum::AIR;
            } else {
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(5) + " field cannot be blank.");
                } else {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                }
                ErrorsFound = true;
            }

            if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::LIQUID) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).PlantInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(6),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                state.dataPhotovoltaicThermalCollector->PVT(Item).PlantOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(7),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   state.dataIPShortCut->cAlphaArgs(7),
                                                   "Water Nodes");

                state.dataPhotovoltaicThermalCollector->PVT(Item).WLoopSideNum = DataPlant::DemandSupply_No;
            }

            if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::AIR) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).HVACInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(8),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                state.dataPhotovoltaicThermalCollector->PVT(Item).HVACOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(9),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(8),
                                                   state.dataIPShortCut->cAlphaArgs(9),
                                                   "Air Nodes");
            }

            state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate = state.dataIPShortCut->rNumericArgs(1);
            state.dataPhotovoltaicThermalCollector->PVT(Item).SizingInit = true;
            if (state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate == DataSizing::AutoSize) {
                state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRateWasAutoSized = true;
            }
            if (state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate != DataSizing::AutoSize) {

                if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::LIQUID) {
                    PlantUtilities::RegisterPlantCompDesignFlow(state,
                                                                state.dataPhotovoltaicThermalCollector->PVT(Item).PlantInletNodeNum,
                                                                state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate);
                } else if (state.dataPhotovoltaicThermalCollector->PVT(Item).WorkingFluidType == WorkingFluidEnum::AIR) {
                    state.dataPhotovoltaicThermalCollector->PVT(Item).MaxMassFlowRate =
                        state.dataPhotovoltaicThermalCollector->PVT(Item).DesignVolFlowRate * state.dataEnvrn->StdRhoAir;
                }
                state.dataPhotovoltaicThermalCollector->PVT(Item).SizingInit = false;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for photovoltaic thermal collectors");
        }

        if (allocated(tmpSimplePVTperf)) tmpSimplePVTperf.deallocate();
    }

    void PVTCollectorStruct::setupReportVars(EnergyPlusData &state)
    {
        SetupOutputVariable(
            state, "Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->Report.ThermPower, "System", "Average", this->Name);

        if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
            SetupOutputVariable(state,
                                "Generator Produced Thermal Energy",
                                OutputProcessor::Unit::J,
                                this->Report.ThermEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarWater",
                                "HeatProduced",
                                _,
                                "Plant");

        } else if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
            SetupOutputVariable(state,
                                "Generator Produced Thermal Energy",
                                OutputProcessor::Unit::J,
                                this->Report.ThermEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarAir",
                                "HeatProduced",
                                _,
                                "System");

            SetupOutputVariable(
                state, "Generator PVT Fluid Bypass Status", OutputProcessor::Unit::None, this->Report.BypassStatus, "System", "Average", this->Name);
        }

        SetupOutputVariable(
            state, "Generator PVT Fluid Inlet Temperature", OutputProcessor::Unit::C, this->Report.TinletWorkFluid, "System", "Average", this->Name);

        SetupOutputVariable(state,
                            "Generator PVT Fluid Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->Report.ToutletWorkFluid,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(
            state, "Generator PVT Fluid Mass Flow Rate", OutputProcessor::Unit::kg_s, this->Report.MdotWorkFluid, "System", "Average", this->Name);
    }

    void PVTCollectorStruct::initialize(EnergyPlusData &state, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2008
        //       MODIFIED       B. Griffith, May 2009, EMS setpoint check
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // init for PVT

        static constexpr std::string_view RoutineName("InitPVTcollectors");

        // Do the one time initializations
        if (this->MyOneTimeFlag) {
            this->setupReportVars(state);
            this->MyOneTimeFlag = false;
        }

        if (this->SetLoopIndexFlag) {
            if (allocated(state.dataPlnt->PlantLoop) && (this->PlantInletNodeNum > 0)) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->TypeNum,
                                                        this->WLoopNum,
                                                        this->WLoopSideNum,
                                                        this->WLoopBranchNum,
                                                        this->WLoopCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitPVTcollectors: Program terminated for previous conditions.");
                }
                this->SetLoopIndexFlag = false;
            }
        }

        // finish set up of PV, because PV get-input follows PVT's get input.
        if (!this->PVfound) {
            if (allocated(state.dataPhotovoltaic->PVarray)) {
                this->PVnum = UtilityRoutines::FindItemInList(this->PVname, state.dataPhotovoltaic->PVarray);
                if (this->PVnum == 0) {
                    ShowSevereError(state, "Invalid name for photovoltaic generator = " + this->PVname);
                    ShowContinueError(state, "Entered in flat plate photovoltaic-thermal collector = " + this->Name);
                } else {
                    this->PVfound = true;
                }
            } else {
                if ((!state.dataGlobal->BeginEnvrnFlag) && (!FirstHVACIteration)) {
                    ShowSevereError(state, "Photovoltaic generators are missing for Photovoltaic Thermal modeling");
                    ShowContinueError(state, "Needed for flat plate photovoltaic-thermal collector = " + this->Name);
                }
            }
        }

        if (!state.dataGlobal->SysSizingCalc && this->MySetPointCheckFlag && state.dataHVACGlobal->DoSetPointTest) {
            for (int PVTindex = 1; PVTindex <= state.dataPhotovoltaicThermalCollector->NumPVT; ++PVTindex) {
                if (state.dataPhotovoltaicThermalCollector->PVT(PVTindex).WorkingFluidType == WorkingFluidEnum::AIR) {
                    if (state.dataLoopNodes->Node(state.dataPhotovoltaicThermalCollector->PVT(PVTindex).HVACOutletNodeNum).TempSetPoint ==
                        DataLoopNode::SensedNodeFlagValue) {
                        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                            ShowSevereError(state, "Missing temperature setpoint for PVT outlet node  ");
                            ShowContinueError(state,
                                              "Add a setpoint manager to outlet node of PVT named " +
                                                  state.dataPhotovoltaicThermalCollector->PVT(PVTindex).Name);
                            state.dataHVACGlobal->SetPointErrorFlag = true;
                        } else {
                            // need call to EMS to check node
                            EMSManager::CheckIfNodeSetPointManagedByEMS(state,
                                                                        state.dataPhotovoltaicThermalCollector->PVT(PVTindex).HVACOutletNodeNum,
                                                                        EMSManager::SPControlType::iTemperatureSetPoint,
                                                                        state.dataHVACGlobal->SetPointErrorFlag);
                            if (state.dataHVACGlobal->SetPointErrorFlag) {
                                ShowSevereError(state, "Missing temperature setpoint for PVT outlet node  ");
                                ShowContinueError(state,
                                                  "Add a setpoint manager to outlet node of PVT named " +
                                                      state.dataPhotovoltaicThermalCollector->PVT(PVTindex).Name);
                                ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node of PVT");
                            }
                        }
                    }
                }
            }
            this->MySetPointCheckFlag = false;
        }

        if (!state.dataGlobal->SysSizingCalc && this->SizingInit && (this->WorkingFluidType == WorkingFluidEnum::AIR)) {
            this->size(state);
        }

        int InletNode = 0;
        int OutletNode = 0;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);
            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                InletNode = this->PlantInletNodeNum;
                OutletNode = this->PlantOutletNodeNum;
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
                OutletNode = this->HVACOutletNodeNum;
            } else {
                assert(false);
            }
        }

        if (state.dataGlobal->BeginEnvrnFlag && this->EnvrnInit) {

            this->MassFlowRate = 0.0;
            this->BypassDamperOff = true;
            this->CoolingUseful = false;
            this->HeatingUseful = false;
            this->Simple.LastCollectorTemp = 0.0;
            this->Simple.CollectorTemp = 0.0;
            this->Report.ThermEfficiency = 0.0;
            this->Report.ThermPower = 0.0;
            this->Report.ThermHeatGain = 0.0;
            this->Report.ThermHeatLoss = 0.0;
            this->Report.ThermEnergy = 0.0;
            this->Report.MdotWorkFluid = 0.0;
            this->Report.TinletWorkFluid = 0.0;
            this->Report.ToutletWorkFluid = 0.0;
            this->Report.BypassStatus = 0.0;

            {
                auto const SELECT_CASE_var(this->WorkingFluidType);

                if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {

                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->WLoopNum).FluidName,
                                                                   DataGlobalConstants::HWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->WLoopNum).FluidIndex,
                                                                   RoutineName);

                    this->MaxMassFlowRate = this->DesignVolFlowRate * rho;

                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       this->MaxMassFlowRate,
                                                       InletNode,
                                                       OutletNode,
                                                       this->WLoopNum,
                                                       this->WLoopSideNum,
                                                       this->WLoopBranchNum,
                                                       this->WLoopCompNum);

                    this->Simple.LastCollectorTemp = 23.0;

                } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                    this->Simple.LastCollectorTemp = 23.0;
                }
            }

            this->EnvrnInit = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) this->EnvrnInit = true;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);

            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                // heating only right now, so control flow requests based on incident solar;
                if (state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) > state.dataPhotovoltaic->MinIrradiance) {
                    this->MassFlowRate = this->MaxMassFlowRate;
                } else {
                    this->MassFlowRate = 0.0;
                }

                PlantUtilities::SetComponentFlowRate(
                    state, this->MassFlowRate, InletNode, OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum);
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                this->MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            }
        }
    }

    void PVTCollectorStruct::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing PVT flow rates that
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains hot water flow rate from the plant sizing array.

        bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing:System object and system sizing done

        // Indicator to hardsize and no sizing run
        bool HardSizeNoDesRun = !(state.dataSize->SysSizingRunDone || state.dataSize->ZoneSizingRunDone);

        if (state.dataSize->CurSysNum > 0) {
            CheckThisAirSystemForSizing(state, state.dataSize->CurSysNum, SizingDesRunThisAirSys);
        } else {
            SizingDesRunThisAirSys = false;
        }

        Real64 DesignVolFlowRateDes = 0.0; // Autosize design volume flow for reporting
        int PltSizNum = 0;                 // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound = false;

        if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {

            if (!allocated(state.dataSize->PlantSizData)) return;
            if (!allocated(state.dataPlnt->PlantLoop)) return;

            if (this->WLoopNum > 0) {
                PltSizNum = state.dataPlnt->PlantLoop(this->WLoopNum).PlantSizNum;
            }
            if (this->WLoopSideNum == DataPlant::SupplySide) {
                if (PltSizNum > 0) {
                    if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        DesignVolFlowRateDes = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate;
                    } else {
                        DesignVolFlowRateDes = 0.0;
                    }
                } else {
                    if (this->DesignVolFlowRateWasAutoSized) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            ShowSevereError(state, "Autosizing of PVT solar collector design flow rate requires a Sizing:Plant object");
                            ShowContinueError(state, "Occurs in PVT object=" + this->Name);
                            ErrorsFound = true;
                        }
                    } else { // Hardsized
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && this->DesignVolFlowRate > 0.0) {
                            BaseSizer::reportSizerOutput(state,
                                                         "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                         this->Name,
                                                         "User-Specified Design Flow Rate [m3/s]",
                                                         this->DesignVolFlowRate);
                        }
                    }
                }
            } else if (this->WLoopSideNum == DataPlant::DemandSide) {
                DesignVolFlowRateDes = this->AreaCol * SimplePVTWaterSizeFactor;
            }
            if (this->DesignVolFlowRateWasAutoSized) {
                this->DesignVolFlowRate = DesignVolFlowRateDes;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                 this->Name,
                                                 "Design Size Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateDes);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                 this->Name,
                                                 "Initial Design Size Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateDes);
                }
                PlantUtilities::RegisterPlantCompDesignFlow(state, this->PlantInletNodeNum, this->DesignVolFlowRate);

            } else { // Hardsized with sizing data
                if (this->DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport) {
                    Real64 DesignVolFlowRateUser = this->DesignVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                 this->Name,
                                                 "Design Size Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateDes,
                                                 "User-Specified Design Flow Rate [m3/s]",
                                                 DesignVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(DesignVolFlowRateDes - DesignVolFlowRateUser) / DesignVolFlowRateUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, "SizeSolarCollector: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Design Flow Rate of {:.5R} [W]", DesignVolFlowRateUser));
                            ShowContinueError(state, format("differs from Design Size Design Flow Rate of {:.5R} [W]", DesignVolFlowRateDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        } // plant component

        if (this->WorkingFluidType == WorkingFluidEnum::AIR) {

            if (state.dataSize->CurSysNum > 0) {
                if (!this->DesignVolFlowRateWasAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (this->DesignVolFlowRate > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                     this->Name,
                                                     "User-Specified Design Flow Rate [m3/s]",
                                                     this->DesignVolFlowRate);
                    }
                } else {
                    CheckSysSizing(state, "SolarCollector:FlatPlate:PhotovoltaicThermal", this->Name);
                    if (state.dataSize->CurOASysNum > 0) {
                        DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow;
                    } else {
                        {
                            auto const SELECT_CASE_var(state.dataSize->CurDuctType);
                            if (SELECT_CASE_var == DataHVACGlobals::Main) {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).SysAirMinFlowRat *
                                                       state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                            } else if (SELECT_CASE_var == DataHVACGlobals::Cooling) {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).SysAirMinFlowRat *
                                                       state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesCoolVolFlow;
                            } else if (SELECT_CASE_var == DataHVACGlobals::Heating) {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesHeatVolFlow;
                            } else {
                                DesignVolFlowRateDes = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                            }
                        }
                    }
                    Real64 DesMassFlow = state.dataEnvrn->StdRhoAir * DesignVolFlowRateDes;
                    this->MaxMassFlowRate = DesMassFlow;
                }
                if (!HardSizeNoDesRun) {
                    if (this->DesignVolFlowRateWasAutoSized) {
                        this->DesignVolFlowRate = DesignVolFlowRateDes;
                        BaseSizer::reportSizerOutput(state,
                                                     "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                     this->Name,
                                                     "Design Size Design Flow Rate [m3/s]",
                                                     DesignVolFlowRateDes);
                        this->SizingInit = false;
                    } else {
                        if (this->DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0) {
                            Real64 DesignVolFlowRateUser = this->DesignVolFlowRate;
                            BaseSizer::reportSizerOutput(state,
                                                         "SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                         this->Name,
                                                         "Design Size Design Flow Rate [m3/s]",
                                                         DesignVolFlowRateDes,
                                                         "User-Specified Design Flow Rate [m3/s]",
                                                         DesignVolFlowRateUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(DesignVolFlowRateDes - DesignVolFlowRateUser) / DesignVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeSolarCollector: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Design Flow Rate of {:.5R} [W]", DesignVolFlowRateUser));
                                    ShowContinueError(state, format("differs from Design Size Design Flow Rate of {:.5R} [W]", DesignVolFlowRateDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            } else if (state.dataSize->CurZoneEqNum > 0) {
                // PVT is not currently for zone equipment, should not come here.
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void PVTCollectorStruct::control(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // make control decisions for PVT collector

        // METHODOLOGY EMPLOYED:
        // decide if PVT should be in cooling or heat mode and if it should be bypassed or not

        if (this->WorkingFluidType == WorkingFluidEnum::AIR) {

            if (this->PVTModelType == SimplePVTmodel) {
                if (state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) > state.dataPhotovoltaic->MinIrradiance) {
                    // is heating wanted?
                    //  Outlet node is required to have a setpoint.
                    if (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint > state.dataLoopNodes->Node(this->HVACInletNodeNum).Temp) {
                        this->HeatingUseful = true;
                        this->CoolingUseful = false;
                        this->BypassDamperOff = true;
                    } else {
                        this->HeatingUseful = false;
                        this->CoolingUseful = true;
                        this->BypassDamperOff = false;
                    }
                } else {
                    // is cooling wanted?
                    if (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint < state.dataLoopNodes->Node(this->HVACInletNodeNum).Temp) {
                        this->CoolingUseful = true;
                        this->HeatingUseful = false;
                        this->BypassDamperOff = true;
                    } else {
                        this->CoolingUseful = false;
                        this->HeatingUseful = true;
                        this->BypassDamperOff = false;
                    }
                }
            }

        } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
            if (this->PVTModelType == SimplePVTmodel) {
                if (state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) > state.dataPhotovoltaic->MinIrradiance) {
                    // is heating wanted?
                    this->HeatingUseful = true;
                    this->BypassDamperOff = true;
                } else {
                    // is cooling wanted?
                    this->CoolingUseful = false;
                    this->BypassDamperOff = false;
                }
            }
        }
    }

    void PVTCollectorStruct::calculate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate PVT collector thermal

        // METHODOLOGY EMPLOYED:
        // Current model is "simple" fixed efficiency and simple night sky balance for cooling

        static constexpr std::string_view RoutineName("CalcPVTcollectors");

        int InletNode(0);

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);
            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                InletNode = this->PlantInletNodeNum;
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
            }
        }

        Real64 mdot = this->MassFlowRate;
        Real64 Tinlet = state.dataLoopNodes->Node(InletNode).Temp;

        if (this->PVTModelType == SimplePVTmodel) {

            Real64 BypassFraction(0.0);
            Real64 PotentialOutletTemp(0.0);

            if (this->HeatingUseful && this->BypassDamperOff && (mdot > 0.0)) {

                Real64 Eff(0.0);

                {
                    auto const SELECT_CASE_var(this->Simple.ThermEfficMode);

                    if (SELECT_CASE_var == ThermEfficEnum::FIXED) {
                        Eff = this->Simple.ThermEffic;
                    } else if (SELECT_CASE_var == ThermEfficEnum::SCHEDULED) {
                        Eff = ScheduleManager::GetCurrentScheduleValue(state, this->Simple.ThermEffSchedNum);
                        this->Simple.ThermEffic = Eff;
                    }
                }

                Real64 PotentialHeatGain = state.dataHeatBal->SurfQRadSWOutIncident(this->SurfNum) * Eff * this->AreaCol;

                if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                    Real64 Winlet = state.dataLoopNodes->Node(InletNode).HumRat;
                    Real64 CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                    if (mdot * CpInlet > 0.0) {
                        PotentialOutletTemp = Tinlet + PotentialHeatGain / (mdot * CpInlet);
                    } else {
                        PotentialOutletTemp = Tinlet;
                    }
                    // now compare heating potential to setpoint and figure bypass fraction
                    if (PotentialOutletTemp > state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint) { // need to modulate
                        if (Tinlet != PotentialOutletTemp) {
                            BypassFraction = (state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint - PotentialOutletTemp) /
                                             (Tinlet - PotentialOutletTemp);
                        } else {
                            BypassFraction = 0.0;
                        }
                        BypassFraction = max(0.0, BypassFraction);
                        PotentialOutletTemp = state.dataLoopNodes->Node(this->HVACOutletNodeNum).TempSetPoint;
                        PotentialHeatGain = mdot * Psychrometrics::PsyCpAirFnW(Winlet) * (PotentialOutletTemp - Tinlet);

                    } else {
                        BypassFraction = 0.0;
                    }
                } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
                    Real64 CpInlet = Psychrometrics::CPHW(Tinlet);
                    if (mdot * CpInlet != 0.0) { // protect divide by zero
                        PotentialOutletTemp = Tinlet + PotentialHeatGain / (mdot * CpInlet);
                    } else {
                        PotentialOutletTemp = Tinlet;
                    }
                    BypassFraction = 0.0;
                }

                this->Report.ThermEfficiency = Eff;
                this->Report.ThermHeatGain = PotentialHeatGain;
                this->Report.ThermPower = this->Report.ThermHeatGain;
                this->Report.ThermEnergy = this->Report.ThermPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                this->Report.ThermHeatLoss = 0.0;
                this->Report.TinletWorkFluid = Tinlet;
                this->Report.MdotWorkFluid = mdot;
                this->Report.ToutletWorkFluid = PotentialOutletTemp;
                this->Report.BypassStatus = BypassFraction;

            } else if (this->CoolingUseful && this->BypassDamperOff && (mdot > 0.0)) {
                // calculate cooling using energy balance
                Real64 HrGround(0.0);
                Real64 HrAir(0.0);
                Real64 HcExt(0.0);
                Real64 HrSky(0.0);

                ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                    this->SurfNum,
                                                                    0.0,
                                                                    DataHeatBalance::VerySmooth,
                                                                    this->Simple.SurfEmissivity,
                                                                    this->Simple.LastCollectorTemp,
                                                                    HcExt,
                                                                    HrSky,
                                                                    HrGround,
                                                                    HrAir);

                Real64 WetBulbInlet(0.0);
                Real64 DewPointInlet(0.0);
                Real64 CpInlet(0.0);

                if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                    Real64 Winlet = state.dataLoopNodes->Node(InletNode).HumRat;
                    CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                    WetBulbInlet = Psychrometrics::PsyTwbFnTdbWPb(state, Tinlet, Winlet, state.dataEnvrn->OutBaroPress, RoutineName);
                    DewPointInlet = Psychrometrics::PsyTdpFnTdbTwbPb(state, Tinlet, WetBulbInlet, state.dataEnvrn->OutBaroPress, RoutineName);
                } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
                    CpInlet = Psychrometrics::CPHW(Tinlet);
                }

                Real64 Tcollector =
                    (2.0 * mdot * CpInlet * Tinlet + this->AreaCol * (HrGround * state.dataEnvrn->OutDryBulbTemp + HrSky * state.dataEnvrn->SkyTemp +
                                                                      HrAir * state.dataSurface->SurfOutDryBulbTemp(this->SurfNum) +
                                                                      HcExt * state.dataSurface->SurfOutDryBulbTemp(this->SurfNum))) /
                    (2.0 * mdot * CpInlet + this->AreaCol * (HrGround + HrSky + HrAir + HcExt));

                PotentialOutletTemp = 2.0 * Tcollector - Tinlet;
                this->Report.ToutletWorkFluid = PotentialOutletTemp;
                // trap for air not being cooled below its wetbulb.
                if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                    if (PotentialOutletTemp < DewPointInlet) {
                        //  water removal would be needed.. not going to allow that for now.  limit cooling to dew point and model bypass
                        if (Tinlet != PotentialOutletTemp) {
                            BypassFraction = (DewPointInlet - PotentialOutletTemp) / (Tinlet - PotentialOutletTemp);
                        } else {
                            BypassFraction = 0.0;
                        }
                        BypassFraction = max(0.0, BypassFraction);
                        PotentialOutletTemp = DewPointInlet;
                    }
                }

                this->Report.MdotWorkFluid = mdot;
                this->Report.TinletWorkFluid = Tinlet;
                this->Report.ToutletWorkFluid = PotentialOutletTemp;
                this->Report.ThermHeatLoss = mdot * CpInlet * (Tinlet - this->Report.ToutletWorkFluid);
                this->Report.ThermHeatGain = 0.0;
                this->Report.ThermPower = -1.0 * this->Report.ThermHeatLoss;
                this->Report.ThermEnergy = this->Report.ThermPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
                this->Report.ThermEfficiency = 0.0;
                this->Simple.LastCollectorTemp = Tcollector;
                this->Report.BypassStatus = BypassFraction;

            } else {
                this->Report.TinletWorkFluid = Tinlet;
                this->Report.ToutletWorkFluid = Tinlet;
                this->Report.ThermHeatLoss = 0.0;
                this->Report.ThermHeatGain = 0.0;
                this->Report.ThermPower = 0.0;
                this->Report.ThermEfficiency = 0.0;
                this->Report.ThermEnergy = 0.0;
                this->Report.BypassStatus = 1.0;
                this->Report.MdotWorkFluid = mdot;
            }
        }
    }

    void PVTCollectorStruct::update(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int InletNode;
        int OutletNode;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);
            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                InletNode = this->PlantInletNodeNum;
                OutletNode = this->PlantOutletNodeNum;

                PlantUtilities::SafeCopyPlantNode(state, InletNode, OutletNode);
                state.dataLoopNodes->Node(OutletNode).Temp = this->Report.ToutletWorkFluid;

            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
                OutletNode = this->HVACOutletNodeNum;

                // Set the outlet nodes for properties that just pass through & not used
                state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
                state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;
                state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMin = state.dataLoopNodes->Node(InletNode).MassFlowRateMin;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataLoopNodes->Node(InletNode).MassFlowRateMax;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail;
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;

                // Set outlet node variables that are possibly changed
                state.dataLoopNodes->Node(OutletNode).Temp = this->Report.ToutletWorkFluid;
                state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat; // assumes dewpoint bound on cooling ....
                state.dataLoopNodes->Node(OutletNode).Enthalpy =
                    Psychrometrics::PsyHFnTdbW(this->Report.ToutletWorkFluid, state.dataLoopNodes->Node(OutletNode).HumRat);
            }
        }
    }

    void GetPVTThermalPowerProduction(EnergyPlusData &state, int const PVindex, Real64 &ThermalPower, Real64 &ThermalEnergy)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int PVTnum(0);

        // first find PVT index that is associated with this PV generator
        for (int loop = 1; loop <= state.dataPhotovoltaicThermalCollector->NumPVT; ++loop) {
            if (!state.dataPhotovoltaicThermalCollector->PVT(loop).PVfound) continue;
            if (state.dataPhotovoltaicThermalCollector->PVT(loop).PVnum == PVindex) { // we found it
                PVTnum = loop;
            }
        }

        if (PVTnum > 0) {
            ThermalPower = state.dataPhotovoltaicThermalCollector->PVT(PVTnum).Report.ThermPower;
            ThermalEnergy = state.dataPhotovoltaicThermalCollector->PVT(PVTnum).Report.ThermEnergy;
        } else {
            ThermalPower = 0.0;
            ThermalEnergy = 0.0;
        }
    }

    int GetAirInletNodeNum(EnergyPlusData &state, std::string_view PVTName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given PVT and returns the air inlet node number.
        // If incorrect PVT name is given, ErrorsFound is returned as true and node number as zero.

        int NodeNum; // node number returned
        int WhichPVT;

        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        WhichPVT = UtilityRoutines::FindItemInList(PVTName, state.dataPhotovoltaicThermalCollector->PVT);
        if (WhichPVT != 0) {
            NodeNum = state.dataPhotovoltaicThermalCollector->PVT(WhichPVT).HVACInletNodeNum;
        } else {
            ShowSevereError(state, "GetAirInletNodeNum: Could not find SolarCollector FlatPlate PhotovoltaicThermal = \"" + std::string{PVTName} + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }
    int GetAirOutletNodeNum(EnergyPlusData &state, std::string_view PVTName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given PVT and returns the air outlet node number.
        // If incorrect PVT name is given, ErrorsFound is returned as true and node number as zero.

        int NodeNum; // node number returned
        int WhichPVT;

        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        WhichPVT = UtilityRoutines::FindItemInList(PVTName, state.dataPhotovoltaicThermalCollector->PVT);
        if (WhichPVT != 0) {
            NodeNum = state.dataPhotovoltaicThermalCollector->PVT(WhichPVT).HVACOutletNodeNum;
        } else {
            ShowSevereError(state, "GetAirInletNodeNum: Could not find SolarCollector FlatPlate PhotovoltaicThermal = \"" + std::string{PVTName} + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int getPVTindexFromName(EnergyPlusData &state, std::string_view objectName)
    {
        if (state.dataPhotovoltaicThermalCollector->GetInputFlag) {
            GetPVTcollectorsInput(state);
            state.dataPhotovoltaicThermalCollector->GetInputFlag = false;
        }

        for (auto it = state.dataPhotovoltaicThermalCollector->PVT.begin(); it != state.dataPhotovoltaicThermalCollector->PVT.end(); ++it) {
            if (it->Name == objectName) {
                return static_cast<int>(std::distance(state.dataPhotovoltaicThermalCollector->PVT.begin(), it) + 1);
            }
        }

        // If we didn't find it, fatal
        ShowFatalError(state, "Solar Thermal Collector GetIndexFromName: Error getting inputs for object named: " + std::string{objectName});
        assert(false);
        return 0; // Shutup compiler
    }

    void simPVTfromOASys(EnergyPlusData &state, int const index, bool const FirstHVACIteration)
    {
        PlantLocation dummyLoc(0, 0, 0, 0);
        Real64 dummyCurLoad(0.0);
        bool dummyRunFlag(true);

        state.dataPhotovoltaicThermalCollector->PVT(index).simulate(state, dummyLoc, FirstHVACIteration, dummyCurLoad, dummyRunFlag);
    }

} // namespace PhotovoltaicThermalCollectors

} // namespace EnergyPlus
