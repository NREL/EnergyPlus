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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPhotovoltaics.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
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

    bool GetInputFlag(true); // First time, input is "gotten"

    int NumPVT(0); // count of all types of PVT in input file

    Array1D<PVTCollectorStruct> PVT;

    void clear_state()
    {
        GetInputFlag = true;
        NumPVT = 0;
        PVT.deallocate();
    }

    PlantComponent *PVTCollectorStruct::factory(std::string const &objectName)
    {
        if (GetInputFlag) {
            GetPVTcollectorsInput();
            GetInputFlag = false;
        }

        for (auto &thisComp : PVT) {
            if (thisComp.Name == objectName) {
                return &thisComp;
            }
        }

        // If we didn't find it, fatal
        ShowFatalError("Solar Thermal Collector Factory: Error getting inputs for object named: " + objectName);
        // Shut up the compiler
        return nullptr;
    }

    void PVTCollectorStruct::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation))
    {
        this->initialize(state, true);
        this->size();
    }

    void PVTCollectorStruct::simulate(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation),
                                      bool const FirstHVACIteration,
                                      Real64 &EP_UNUSED(CurLoad),
                                      bool const EP_UNUSED(RunFlag))
    {

        this->initialize(state, FirstHVACIteration);
        this->control();
        this->calculate(state, state.files);
        this->update();
    }

    void GetPVTcollectorsInput()
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
        DataIPShortCuts::cCurrentModuleObject = "SolarCollectorPerformance:PhotovoltaicThermal:Simple";
        int NumSimplePVTPerform = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
        if (NumSimplePVTPerform > 0) {
            tmpSimplePVTperf.allocate(NumSimplePVTPerform);
            for (Item = 1; Item <= NumSimplePVTPerform; ++Item) {
                inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                              Item,
                                              DataIPShortCuts::cAlphaArgs,
                                              NumAlphas,
                                              DataIPShortCuts::rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              _,
                                              DataIPShortCuts::lAlphaFieldBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);
                if (UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound)) continue;

                tmpSimplePVTperf(Item).Name = DataIPShortCuts::cAlphaArgs(1);
                if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "Fixed")) {
                    tmpSimplePVTperf(Item).ThermEfficMode = ThermEfficEnum::FIXED;
                } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "Scheduled")) {
                    tmpSimplePVTperf(Item).ThermEfficMode = ThermEfficEnum::SCHEDULED;
                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                tmpSimplePVTperf(Item).ThermalActiveFract = DataIPShortCuts::rNumericArgs(1);
                tmpSimplePVTperf(Item).ThermEffic = DataIPShortCuts::rNumericArgs(2);

                tmpSimplePVTperf(Item).ThermEffSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(3));
                if ((tmpSimplePVTperf(Item).ThermEffSchedNum == 0) && (tmpSimplePVTperf(Item).ThermEfficMode == ThermEfficEnum::SCHEDULED)) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                tmpSimplePVTperf(Item).SurfEmissivity = DataIPShortCuts::rNumericArgs(3);
            }
        } // NumSimplePVTPerform > 0

        // now get main PVT objects
        DataIPShortCuts::cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
        NumPVT = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
        PVT.allocate(NumPVT);

        for (Item = 1; Item <= NumPVT; ++Item) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          Item,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          _,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound)) continue;

            PVT(Item).Name = DataIPShortCuts::cAlphaArgs(1);
            PVT(Item).TypeNum = DataPlant::TypeOf_PVTSolarCollectorFlatPlate;

            PVT(Item).SurfNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(2), DataSurfaces::Surface);
            // check surface
            if (PVT(Item).SurfNum == 0) {
                if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Surface name cannot be blank.");
                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Surface was not found.");
                }
                ErrorsFound = true;
            } else {

                if (!DataSurfaces::Surface(PVT(Item).SurfNum).ExtSolar) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Surface must be exposed to solar.");
                    ErrorsFound = true;
                }
                // check surface orientation, warn if upside down
                if ((DataSurfaces::Surface(PVT(Item).SurfNum).Tilt < -95.0) || (DataSurfaces::Surface(PVT(Item).SurfNum).Tilt > 95.0)) {
                    ShowWarningError("Suspected input problem with " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Surface used for solar collector faces down");
                    ShowContinueError("Surface tilt angle (degrees from ground outward normal) = " +
                                      General::RoundSigDigits(DataSurfaces::Surface(PVT(Item).SurfNum).Tilt, 2));
                }

            } // check surface

            if (DataIPShortCuts::lAlphaFieldBlanks(3)) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(3) + ", name cannot be blank.");
                ErrorsFound = true;
            } else {
                PVT(Item).PVTModelName = DataIPShortCuts::cAlphaArgs(3);
                int ThisParamObj = UtilityRoutines::FindItemInList(PVT(Item).PVTModelName, tmpSimplePVTperf);
                if (ThisParamObj > 0) {
                    PVT(Item).Simple = tmpSimplePVTperf(ThisParamObj); // entire structure assigned
                    // do one-time setups on input data
                    PVT(Item).AreaCol = DataSurfaces::Surface(PVT(Item).SurfNum).Area * PVT(Item).Simple.ThermalActiveFract;
                    PVT(Item).PVTModelType = SimplePVTmodel;
                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError(DataIPShortCuts::cAlphaFieldNames(3) + ", was not found.");
                    ErrorsFound = true;
                }
            }
            if (allocated(DataPhotovoltaics::PVarray)) { // then PV input gotten... but don't expect this to be true.
                PVT(Item).PVnum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(4), DataPhotovoltaics::PVarray);
                // check PV
                if (PVT(Item).PVnum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + DataIPShortCuts::cAlphaArgs(4));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else {
                    PVT(Item).PVname = DataIPShortCuts::cAlphaArgs(4);
                    PVT(Item).PVfound = true;
                }
            } else { // no PV or not yet gotten.
                PVT(Item).PVname = DataIPShortCuts::cAlphaArgs(4);
                PVT(Item).PVfound = false;
            }

            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(5), "Water")) {
                PVT(Item).WorkingFluidType = WorkingFluidEnum::LIQUID;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(5), "Air")) {
                PVT(Item).WorkingFluidType = WorkingFluidEnum::AIR;
            } else {
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + DataIPShortCuts::cAlphaArgs(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError(DataIPShortCuts::cAlphaFieldNames(5) + " field cannot be blank.");
                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + DataIPShortCuts::cAlphaArgs(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                }
                ErrorsFound = true;
            }

            if (PVT(Item).WorkingFluidType == WorkingFluidEnum::LIQUID) {
                PVT(Item).PlantInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                  ErrorsFound,
                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                  DataLoopNode::NodeType_Water,
                                                                                  DataLoopNode::NodeConnectionType_Inlet,
                                                                                  1,
                                                                                  DataLoopNode::ObjectIsNotParent);
                PVT(Item).PlantOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                   ErrorsFound,
                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                   DataLoopNode::NodeType_Water,
                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                   1,
                                                                                   DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(6),
                                                   DataIPShortCuts::cAlphaArgs(7),
                                                   "Water Nodes");

                PVT(Item).WLoopSideNum = DataPlant::DemandSupply_No;
            }

            if (PVT(Item).WorkingFluidType == WorkingFluidEnum::AIR) {
                PVT(Item).HVACInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                 ErrorsFound,
                                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                                 DataIPShortCuts::cAlphaArgs(1),
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_Inlet,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
                PVT(Item).HVACOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(9),
                                                                                  ErrorsFound,
                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                  DataLoopNode::NodeType_Air,
                                                                                  DataLoopNode::NodeConnectionType_Outlet,
                                                                                  1,
                                                                                  DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(8),
                                                   DataIPShortCuts::cAlphaArgs(9),
                                                   "Air Nodes");
            }

            PVT(Item).DesignVolFlowRate = DataIPShortCuts::rNumericArgs(1);
            PVT(Item).SizingInit = true;
            if (PVT(Item).DesignVolFlowRate == DataSizing::AutoSize) {
                PVT(Item).DesignVolFlowRateWasAutoSized = true;
            }
            if (PVT(Item).DesignVolFlowRate != DataSizing::AutoSize) {

                if (PVT(Item).WorkingFluidType == WorkingFluidEnum::LIQUID) {
                    PlantUtilities::RegisterPlantCompDesignFlow(PVT(Item).PlantInletNodeNum, PVT(Item).DesignVolFlowRate);
                } else if (PVT(Item).WorkingFluidType == WorkingFluidEnum::AIR) {
                    PVT(Item).MaxMassFlowRate = PVT(Item).DesignVolFlowRate * DataEnvironment::StdRhoAir;
                }
                PVT(Item).SizingInit = false;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for photovoltaic thermal collectors");
        }

        if (allocated(tmpSimplePVTperf)) tmpSimplePVTperf.deallocate();
    }

    void PVTCollectorStruct::setupReportVars()
    {
        SetupOutputVariable("Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->Report.ThermPower, "System", "Average", this->Name);

        if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
            SetupOutputVariable("Generator Produced Thermal Energy",
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
            SetupOutputVariable("Generator Produced Thermal Energy",
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
                "Generator PVT Fluid Bypass Status", OutputProcessor::Unit::None, this->Report.BypassStatus, "System", "Average", this->Name);
        }

        SetupOutputVariable(
            "Generator PVT Fluid Inlet Temperature", OutputProcessor::Unit::C, this->Report.TinletWorkFluid, "System", "Average", this->Name);

        SetupOutputVariable(
            "Generator PVT Fluid Outlet Temperature", OutputProcessor::Unit::C, this->Report.ToutletWorkFluid, "System", "Average", this->Name);

        SetupOutputVariable(
            "Generator PVT Fluid Mass Flow Rate", OutputProcessor::Unit::kg_s, this->Report.MdotWorkFluid, "System", "Average", this->Name);
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

        static std::string const RoutineName("InitPVTcollectors");

        // Do the one time initializations
        if (this->MyOneTimeFlag) {
            this->setupReportVars();
            this->MyOneTimeFlag = false;
        }

        if (this->SetLoopIndexFlag) {
            if (allocated(DataPlant::PlantLoop) && (this->PlantInletNodeNum > 0)) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(state,
                    this->Name, this->TypeNum, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum, errFlag, _, _, _, _, _);
                if (errFlag) {
                    ShowFatalError("InitPVTcollectors: Program terminated for previous conditions.");
                }
                this->SetLoopIndexFlag = false;
            }
        }

        // finish set up of PV, because PV get-input follows PVT's get input.
        if (!this->PVfound) {
            if (allocated(DataPhotovoltaics::PVarray)) {
                this->PVnum = UtilityRoutines::FindItemInList(this->PVname, DataPhotovoltaics::PVarray);
                if (this->PVnum == 0) {
                    ShowSevereError("Invalid name for photovoltaic generator = " + this->PVname);
                    ShowContinueError("Entered in flat plate photovoltaic-thermal collector = " + this->Name);
                } else {
                    this->PVfound = true;
                }
            } else {
                if ((!DataGlobals::BeginEnvrnFlag) && (!FirstHVACIteration)) {
                    ShowSevereError("Photovoltaic generators are missing for Photovoltaic Thermal modeling");
                    ShowContinueError("Needed for flat plate photovoltaic-thermal collector = " + this->Name);
                }
            }
        }

        if (!DataGlobals::SysSizingCalc && this->MySetPointCheckFlag && DataHVACGlobals::DoSetPointTest) {
            for (int PVTindex = 1; PVTindex <= NumPVT; ++PVTindex) {
                if (PVT(PVTindex).WorkingFluidType == WorkingFluidEnum::AIR) {
                    if (DataLoopNode::Node(PVT(PVTindex).HVACOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) {
                        if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                            ShowSevereError("Missing temperature setpoint for PVT outlet node  ");
                            ShowContinueError("Add a setpoint manager to outlet node of PVT named " + PVT(PVTindex).Name);
                            DataHVACGlobals::SetPointErrorFlag = true;
                        } else {
                            // need call to EMS to check node
                            EMSManager::CheckIfNodeSetPointManagedByEMS(
                                PVT(PVTindex).HVACOutletNodeNum, EMSManager::iTemperatureSetPoint, DataHVACGlobals::SetPointErrorFlag);
                            if (DataHVACGlobals::SetPointErrorFlag) {
                                ShowSevereError("Missing temperature setpoint for PVT outlet node  ");
                                ShowContinueError("Add a setpoint manager to outlet node of PVT named " + PVT(PVTindex).Name);
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node of PVT");
                            }
                        }
                    }
                }
            }
            this->MySetPointCheckFlag = false;
        }

        if (!DataGlobals::SysSizingCalc && this->SizingInit && (this->WorkingFluidType == WorkingFluidEnum::AIR)) {
            this->size();
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

        if (DataGlobals::BeginEnvrnFlag && this->EnvrnInit) {

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

                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->WLoopNum).FluidName,
                                                                   DataGlobals::HWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->WLoopNum).FluidIndex,
                                                                   RoutineName);

                    this->MaxMassFlowRate = this->DesignVolFlowRate * rho;

                    PlantUtilities::InitComponentNodes(0.0,
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
        if (!DataGlobals::BeginEnvrnFlag) this->EnvrnInit = true;

        {
            auto const SELECT_CASE_var(this->WorkingFluidType);

            if (SELECT_CASE_var == WorkingFluidEnum::LIQUID) {
                // heating only right now, so control flow requests based on incident solar;
                if (DataHeatBalance::QRadSWOutIncident(this->SurfNum) > DataPhotovoltaics::MinIrradiance) {
                    this->MassFlowRate = this->MaxMassFlowRate;
                } else {
                    this->MassFlowRate = 0.0;
                }

                PlantUtilities::SetComponentFlowRate(
                    this->MassFlowRate, InletNode, OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum);
            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                this->MassFlowRate = DataLoopNode::Node(InletNode).MassFlowRate;
            }
        }
    }

    void PVTCollectorStruct::size()
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
        bool HardSizeNoDesRun = !(DataSizing::SysSizingRunDone || DataSizing::ZoneSizingRunDone);

        if (DataSizing::CurSysNum > 0) {
            CheckThisAirSystemForSizing(DataSizing::CurSysNum, SizingDesRunThisAirSys);
        } else {
            SizingDesRunThisAirSys = false;
        }

        Real64 DesignVolFlowRateDes = 0.0; // Autosize design volume flow for reporting
        int PltSizNum = 0;                 // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound = false;

        if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {

            if (!allocated(DataSizing::PlantSizData)) return;
            if (!allocated(DataPlant::PlantLoop)) return;

            if (this->WLoopNum > 0) {
                PltSizNum = DataPlant::PlantLoop(this->WLoopNum).PlantSizNum;
            }
            if (this->WLoopSideNum == DataPlant::SupplySide) {
                if (PltSizNum > 0) {
                    if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        DesignVolFlowRateDes = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
                    } else {
                        DesignVolFlowRateDes = 0.0;
                    }
                } else {
                    if (this->DesignVolFlowRateWasAutoSized) {
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            ShowSevereError("Autosizing of PVT solar collector design flow rate requires a Sizing:Plant object");
                            ShowContinueError("Occurs in PVT object=" + this->Name);
                            ErrorsFound = true;
                        }
                    } else { // Hardsized
                        if (DataPlant::PlantFinalSizesOkayToReport && this->DesignVolFlowRate > 0.0) {
                            ReportSizingManager::ReportSizingOutput("SolarCollector:FlatPlate:PhotovoltaicThermal",
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
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "SolarCollector:FlatPlate:PhotovoltaicThermal", this->Name, "Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                            this->Name,
                                                            "Initial Design Size Design Flow Rate [m3/s]",
                                                            DesignVolFlowRateDes);
                }
                PlantUtilities::RegisterPlantCompDesignFlow(this->PlantInletNodeNum, this->DesignVolFlowRate);

            } else { // Hardsized with sizing data
                if (this->DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0 && DataPlant::PlantFinalSizesOkayToReport) {
                    Real64 DesignVolFlowRateUser = this->DesignVolFlowRate;
                    ReportSizingManager::ReportSizingOutput("SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                            this->Name,
                                                            "Design Size Design Flow Rate [m3/s]",
                                                            DesignVolFlowRateDes,
                                                            "User-Specified Design Flow Rate [m3/s]",
                                                            DesignVolFlowRateUser);
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(DesignVolFlowRateDes - DesignVolFlowRateUser) / DesignVolFlowRateUser) >
                            DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("SizeSolarCollector: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError("User-Specified Design Flow Rate of " + General::RoundSigDigits(DesignVolFlowRateUser, 5) + " [W]");
                            ShowContinueError("differs from Design Size Design Flow Rate of " + General::RoundSigDigits(DesignVolFlowRateDes, 5) +
                                              " [W]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        } // plant component

        if (this->WorkingFluidType == WorkingFluidEnum::AIR) {

            if (DataSizing::CurSysNum > 0) {
                if (!this->DesignVolFlowRateWasAutoSized && !SizingDesRunThisAirSys) { // Simulation continue
                    HardSizeNoDesRun = true;
                    if (this->DesignVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                                this->Name,
                                                                "User-Specified Design Flow Rate [m3/s]",
                                                                this->DesignVolFlowRate);
                    }
                } else {
                    CheckSysSizing("SolarCollector:FlatPlate:PhotovoltaicThermal", this->Name);
                    if (DataSizing::CurOASysNum > 0) {
                        DesignVolFlowRateDes = DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesOutAirVolFlow;
                    } else {
                        {
                            auto const SELECT_CASE_var(DataSizing::CurDuctType);
                            if (SELECT_CASE_var == DataHVACGlobals::Main) {
                                DesignVolFlowRateDes = DataSizing::FinalSysSizing(DataSizing::CurSysNum).SysAirMinFlowRat *
                                                       DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesMainVolFlow;
                            } else if (SELECT_CASE_var == DataHVACGlobals::Cooling) {
                                DesignVolFlowRateDes = DataSizing::FinalSysSizing(DataSizing::CurSysNum).SysAirMinFlowRat *
                                                       DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesCoolVolFlow;
                            } else if (SELECT_CASE_var == DataHVACGlobals::Heating) {
                                DesignVolFlowRateDes = DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesHeatVolFlow;
                            } else {
                                DesignVolFlowRateDes = DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesMainVolFlow;
                            }
                        }
                    }
                    Real64 DesMassFlow = DataEnvironment::StdRhoAir * DesignVolFlowRateDes;
                    this->MaxMassFlowRate = DesMassFlow;
                }
                if (!HardSizeNoDesRun) {
                    if (this->DesignVolFlowRateWasAutoSized) {
                        this->DesignVolFlowRate = DesignVolFlowRateDes;
                        ReportSizingManager::ReportSizingOutput(
                            "SolarCollector:FlatPlate:PhotovoltaicThermal", this->Name, "Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes);
                        this->SizingInit = false;
                    } else {
                        if (this->DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0) {
                            Real64 DesignVolFlowRateUser = this->DesignVolFlowRate;
                            ReportSizingManager::ReportSizingOutput("SolarCollector:FlatPlate:PhotovoltaicThermal",
                                                                    this->Name,
                                                                    "Design Size Design Flow Rate [m3/s]",
                                                                    DesignVolFlowRateDes,
                                                                    "User-Specified Design Flow Rate [m3/s]",
                                                                    DesignVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(DesignVolFlowRateDes - DesignVolFlowRateUser) / DesignVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeSolarCollector: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Flow Rate of " + General::RoundSigDigits(DesignVolFlowRateUser, 5) +
                                                      " [W]");
                                    ShowContinueError("differs from Design Size Design Flow Rate of " +
                                                      General::RoundSigDigits(DesignVolFlowRateDes, 5) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            } else if (DataSizing::CurZoneEqNum > 0) {
                // PVT is not currently for zone equipment, should not come here.
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void PVTCollectorStruct::control()
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
                if (DataHeatBalance::QRadSWOutIncident(this->SurfNum) > DataPhotovoltaics::MinIrradiance) {
                    // is heating wanted?
                    //  Outlet node is required to have a setpoint.
                    if (DataLoopNode::Node(this->HVACOutletNodeNum).TempSetPoint > DataLoopNode::Node(this->HVACInletNodeNum).Temp) {
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
                    if (DataLoopNode::Node(this->HVACOutletNodeNum).TempSetPoint < DataLoopNode::Node(this->HVACInletNodeNum).Temp) {
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
                if (DataHeatBalance::QRadSWOutIncident(this->SurfNum) > DataPhotovoltaics::MinIrradiance) {
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

    void PVTCollectorStruct::calculate(EnergyPlusData &state, IOFiles &ioFiles)
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

        static std::string const RoutineName("CalcPVTcollectors");

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
        Real64 Tinlet = DataLoopNode::Node(InletNode).Temp;

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
                        Eff = ScheduleManager::GetCurrentScheduleValue(this->Simple.ThermEffSchedNum);
                        this->Simple.ThermEffic = Eff;
                    }
                }

                Real64 PotentialHeatGain = DataHeatBalance::QRadSWOutIncident(this->SurfNum) * Eff * this->AreaCol;

                if (this->WorkingFluidType == WorkingFluidEnum::AIR) {
                    Real64 Winlet = DataLoopNode::Node(InletNode).HumRat;
                    Real64 CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                    if (mdot * CpInlet > 0.0) {
                        PotentialOutletTemp = Tinlet + PotentialHeatGain / (mdot * CpInlet);
                    } else {
                        PotentialOutletTemp = Tinlet;
                    }
                    // now compare heating potential to setpoint and figure bypass fraction
                    if (PotentialOutletTemp > DataLoopNode::Node(this->HVACOutletNodeNum).TempSetPoint) { // need to modulate
                        if (Tinlet != PotentialOutletTemp) {
                            BypassFraction =
                                (DataLoopNode::Node(this->HVACOutletNodeNum).TempSetPoint - PotentialOutletTemp) / (Tinlet - PotentialOutletTemp);
                        } else {
                            BypassFraction = 0.0;
                        }
                        BypassFraction = max(0.0, BypassFraction);
                        PotentialOutletTemp = DataLoopNode::Node(this->HVACOutletNodeNum).TempSetPoint;
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
                this->Report.ThermEnergy = this->Report.ThermPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
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
                                                                    ioFiles,
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
                    Real64 Winlet = DataLoopNode::Node(InletNode).HumRat;
                    CpInlet = Psychrometrics::PsyCpAirFnW(Winlet);
                    WetBulbInlet = Psychrometrics::PsyTwbFnTdbWPb(Tinlet, Winlet, DataEnvironment::OutBaroPress, RoutineName);
                    DewPointInlet = Psychrometrics::PsyTdpFnTdbTwbPb(Tinlet, WetBulbInlet, DataEnvironment::OutBaroPress, RoutineName);
                } else if (this->WorkingFluidType == WorkingFluidEnum::LIQUID) {
                    CpInlet = Psychrometrics::CPHW(Tinlet);
                }

                Real64 Tcollector =
                    (2.0 * mdot * CpInlet * Tinlet + this->AreaCol * (HrGround * DataEnvironment::OutDryBulbTemp + HrSky * DataEnvironment::SkyTemp +
                                                                      HrAir * DataSurfaces::Surface(this->SurfNum).OutDryBulbTemp +
                                                                      HcExt * DataSurfaces::Surface(this->SurfNum).OutDryBulbTemp)) /
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
                this->Report.ThermEnergy = this->Report.ThermPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
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

    void PVTCollectorStruct::update()
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

                PlantUtilities::SafeCopyPlantNode(InletNode, OutletNode);
                DataLoopNode::Node(OutletNode).Temp = this->Report.ToutletWorkFluid;

            } else if (SELECT_CASE_var == WorkingFluidEnum::AIR) {
                InletNode = this->HVACInletNodeNum;
                OutletNode = this->HVACOutletNodeNum;

                // Set the outlet nodes for properties that just pass through & not used
                DataLoopNode::Node(OutletNode).Quality = DataLoopNode::Node(InletNode).Quality;
                DataLoopNode::Node(OutletNode).Press = DataLoopNode::Node(InletNode).Press;
                DataLoopNode::Node(OutletNode).MassFlowRate = DataLoopNode::Node(InletNode).MassFlowRate;
                DataLoopNode::Node(OutletNode).MassFlowRateMin = DataLoopNode::Node(InletNode).MassFlowRateMin;
                DataLoopNode::Node(OutletNode).MassFlowRateMax = DataLoopNode::Node(InletNode).MassFlowRateMax;
                DataLoopNode::Node(OutletNode).MassFlowRateMinAvail = DataLoopNode::Node(InletNode).MassFlowRateMinAvail;
                DataLoopNode::Node(OutletNode).MassFlowRateMaxAvail = DataLoopNode::Node(InletNode).MassFlowRateMaxAvail;

                // Set outlet node variables that are possibly changed
                DataLoopNode::Node(OutletNode).Temp = this->Report.ToutletWorkFluid;
                DataLoopNode::Node(OutletNode).HumRat = DataLoopNode::Node(InletNode).HumRat; // assumes dewpoint bound on cooling ....
                DataLoopNode::Node(OutletNode).Enthalpy =
                    Psychrometrics::PsyHFnTdbW(this->Report.ToutletWorkFluid, DataLoopNode::Node(OutletNode).HumRat);
            }
        }
    }

    void GetPVTThermalPowerProduction(int const PVindex, Real64 &ThermalPower, Real64 &ThermalEnergy)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int PVTnum(0);

        // first find PVT index that is associated with this PV generator
        for (int loop = 1; loop <= NumPVT; ++loop) {
            if (!PVT(loop).PVfound) continue;
            if (PVT(loop).PVnum == PVindex) { // we found it
                PVTnum = loop;
            }
        }

        if (PVTnum > 0) {
            ThermalPower = PVT(PVTnum).Report.ThermPower;
            ThermalEnergy = PVT(PVTnum).Report.ThermEnergy;
        } else {
            ThermalPower = 0.0;
            ThermalEnergy = 0.0;
        }
    }

    int GetAirInletNodeNum(std::string const &PVTName, bool &ErrorsFound)
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

        if (GetInputFlag) {
            GetPVTcollectorsInput();
            GetInputFlag = false;
        }

        WhichPVT = UtilityRoutines::FindItemInList(PVTName, PVT);
        if (WhichPVT != 0) {
            NodeNum = PVT(WhichPVT).HVACInletNodeNum;
        } else {
            ShowSevereError("GetAirInletNodeNum: Could not find SolarCollector FlatPlate PhotovoltaicThermal = \"" + PVTName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }
    int GetAirOutletNodeNum(std::string const &PVTName, bool &ErrorsFound)
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

        if (GetInputFlag) {
            GetPVTcollectorsInput();
            GetInputFlag = false;
        }

        WhichPVT = UtilityRoutines::FindItemInList(PVTName, PVT);
        if (WhichPVT != 0) {
            NodeNum = PVT(WhichPVT).HVACOutletNodeNum;
        } else {
            ShowSevereError("GetAirInletNodeNum: Could not find SolarCollector FlatPlate PhotovoltaicThermal = \"" + PVTName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int getPVTindexFromName(std::string const &objectName)
    {
        if (GetInputFlag) {
            GetPVTcollectorsInput();
            GetInputFlag = false;
        }

        for (auto it = PVT.begin(); it != PVT.end(); ++it) {
            if (it->Name == objectName) {
                return static_cast<int>(std::distance(PVT.begin(), it) + 1);
            }
        }

        // If we didn't find it, fatal
        ShowFatalError("Solar Thermal Collector GetIndexFromName: Error getting inputs for object named: " + objectName);
        assert(false);
        return 0; // Shutup compiler
    }

    void simPVTfromOASys(EnergyPlusData &state, int const index, bool const FirstHVACIteration)
    {
        PlantLocation dummyLoc(0, 0, 0, 0);
        Real64 dummyCurLoad(0.0);
        bool dummyRunFlag(true);

        PVT(index).simulate(state, dummyLoc, FirstHVACIteration, dummyCurLoad, dummyRunFlag);
    }

} // namespace PhotovoltaicThermalCollectors

} // namespace EnergyPlus
