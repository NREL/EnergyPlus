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
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantComponentTemperatureSources.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantComponentTemperatureSources {

    // MODULE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   November 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates plant supply components which operate against a
    //  predefined (but variable) boundary temperature.

    // METHODOLOGY EMPLOYED:
    // Called by PlantLoopEquipment, model accepts inputs, and calculates a
    // thermal response using new plant routines such as SetComponentFlowRate

    // MODULE PARAMETER DEFINITIONS:
    int const modTempSpecType_Constant(-1);
    int const modTempSpecType_Schedule(-2);

    // MODULE VARIABLES
    int NumSources(0);
    bool getWaterSourceInput(true); // then TRUE, calls subroutine to read input file.

    // Object Data
    Array1D<WaterSourceSpecs> WaterSource; // dimension to number of machines

    void clear_state()
    {
        NumSources = 0;
        getWaterSourceInput = true;
        WaterSource.deallocate();
    }

    PlantComponent *WaterSourceSpecs::factory(std::string const &objectName)
    {
        if (getWaterSourceInput) {
            GetWaterSourceInput();
            getWaterSourceInput = false;
        }

        // Now look for this particular pipe in the list
        for (auto &waterSource : WaterSource) {
            if (waterSource.Name == objectName) {
                return &waterSource;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalTemperatureSourceFactory: Error getting inputs for temperature source named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void WaterSourceSpecs::initialize(BranchInputManagerData &dataBranchInputManager, Real64 &MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   November 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the water source objects

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitWaterSource");

        if (this->MyFlag) {
            // setup output variables once here
            this->setupOutputVars();
            // Locate the component on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                    this->Name,
                                                    DataPlant::TypeOf_WaterSource,
                                                    this->Location.loopNum,
                                                    this->Location.loopSideNum,
                                                    this->Location.branchNum,
                                                    this->Location.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->InletNodeNum,
                                                    _);
            if (errFlag) {
                ShowFatalError(RoutineName + ": Program terminated due to previous condition(s).");
            }
            this->MyFlag = false;
        }

        // Initialize critical Demand Side Variables at the beginning of each environment
        if (this->MyEnvironFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->Location.loopNum).FluidName,
                                                           DataGlobals::InitConvTemp,
                                                           DataPlant::PlantLoop(this->Location.loopNum).FluidIndex,
                                                           RoutineName);
            this->MassFlowRateMax = this->DesVolFlowRate * rho;
            PlantUtilities::InitComponentNodes(0.0,
                                               this->MassFlowRateMax,
                                               this->InletNodeNum,
                                               this->OutletNodeNum,
                                               this->Location.loopNum,
                                               this->Location.loopSideNum,
                                               this->Location.branchNum,
                                               this->Location.compNum);

            this->MyEnvironFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvironFlag = true;
        }

        // OK, so we can set up the inlet and boundary temperatures now
        this->InletTemp = DataLoopNode::Node(this->InletNodeNum).Temp;
        if (this->TempSpecType == modTempSpecType_Schedule) {
            this->BoundaryTemp = ScheduleManager::GetCurrentScheduleValue(this->TempSpecScheduleNum);
        }

        // Calculate specific heat
        Real64 cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->Location.loopNum).FluidName,
                                                           this->BoundaryTemp,
                                                           DataPlant::PlantLoop(this->Location.loopNum).FluidIndex,
                                                           RoutineName);

        // Calculate deltaT
        Real64 delta_temp = this->BoundaryTemp - this->InletTemp;

        // If deltaT is zero then we cannot calculate a flow request, but we may still want one
        //   If myload is greater than zero, then lets request full flow at the current temperature as it may still be meeting load
        //   If myload is zero, we'll then request zero flow
        // If deltaT is non-zero then we can use the current load and deltaT to calculate a flow request:
        //   If MyLoad is > 0 then we want to heat the loop
        //   If MyLoad is < 0 then we want to cool the loop
        //   Thus, given a fixed outlet temperature (the boundary temp, Tbnd), the eq is:
        //     MyLoad = mdot * cp * (Tbnd - Tin)
        //   re-arranging:
        //     mdot = MyLoad / [cp * (Tbnd - Tin)]
        //  If there is a deltaT, but no load, the mass flow request will go to zero anyway
        if (std::abs(delta_temp) < 0.001) {
            if (std::abs(MyLoad) < 0.001) {
                this->MassFlowRate = 0.0;
            } else {
                this->MassFlowRate = this->MassFlowRateMax;
            }
        } else {
            this->MassFlowRate = MyLoad / (cp * delta_temp);
        }

        // If the mdot is negative it means we can't help the load so we will want to just go to zero.
        // If the mdot is already zero, then well, we still want to go to zero
        // If the mdot is positive, just make sure we constrain it to the design value
        if (this->MassFlowRate < 0) {
            this->MassFlowRate = 0.0;
        } else {
            if (!this->EMSOverrideOnMassFlowRateMax) {
                this->MassFlowRate = min(this->MassFlowRate, this->MassFlowRateMax);
            } else {
                this->MassFlowRate = min(this->MassFlowRate, this->EMSOverrideValueMassFlowRateMax);
            }
        }

        PlantUtilities::SetComponentFlowRate(this->MassFlowRate,
                                             this->InletNodeNum,
                                             this->OutletNodeNum,
                                             this->Location.loopNum,
                                             this->Location.loopSideNum,
                                             this->Location.branchNum,
                                             this->Location.compNum);

        // at this point the mass flow rate, inlet temp, and boundary temp structure vars have been updated
        // the calc routine will update the outlet temp and heat transfer rate/energies
    }

    void WaterSourceSpecs::setupOutputVars()
    {

        SetupOutputVariable(
            "Plant Temperature Source Component Mass Flow Rate", OutputProcessor::Unit::kg_s, this->MassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable(
            "Plant Temperature Source Component Inlet Temperature", OutputProcessor::Unit::C, this->InletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            "Plant Temperature Source Component Outlet Temperature", OutputProcessor::Unit::C, this->OutletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            "Plant Temperature Source Component Source Temperature", OutputProcessor::Unit::C, this->BoundaryTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            "Plant Temperature Source Component Heat Transfer Rate", OutputProcessor::Unit::W, this->HeatRate, "System", "Average", this->Name);
        SetupOutputVariable(
            "Plant Temperature Source Component Heat Transfer Energy", OutputProcessor::Unit::J, this->HeatEnergy, "System", "Sum", this->Name);
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSActuator("PlantComponent:TemperatureSource",
                             this->Name,
                             "Maximum Mass Flow Rate",
                             "[kg/s]",
                             this->EMSOverrideOnMassFlowRateMax,
                             this->EMSOverrideValueMassFlowRateMax);
        }
    }

    void WaterSourceSpecs::autosize()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   November 2012
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing water source design flow rate

        // METHODOLOGY EMPLOYED:
        // Obtains flow rate from the plant sizing array.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false);        // If errors detected in input
        Real64 DesVolFlowRateUser(0.0); // Hardsized design volume flow rate for reporting
        Real64 tmpVolFlowRate = this->DesVolFlowRate;
        int PltSizNum = DataPlant::PlantLoop(this->Location.loopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate; //* WaterSource(SourceNum)%SizFac
                if (!this->DesVolFlowRateWasAutoSized) tmpVolFlowRate = this->DesVolFlowRate;
            } else {
                if (this->DesVolFlowRateWasAutoSized) tmpVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->DesVolFlowRateWasAutoSized) {
                    this->DesVolFlowRate = tmpVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "PlantComponent:TemperatureSource", this->Name, "Design Size Design Fluid Flow Rate [m3/s]", tmpVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "PlantComponent:TemperatureSource", this->Name, "Initial Design Size Design Fluid Flow Rate [m3/s]", tmpVolFlowRate);
                    }
                } else {
                    if (this->DesVolFlowRate > 0.0 && tmpVolFlowRate > 0.0) {
                        DesVolFlowRateUser = this->DesVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("PlantComponent:TemperatureSource",
                                                                    this->Name,
                                                                    "Design Size Design Fluid Flow Rate [m3/s]",
                                                                    tmpVolFlowRate,
                                                                    "User-Specified Design Fluid Flow Rate [m3/s]",
                                                                    DesVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpVolFlowRate - DesVolFlowRateUser) / DesVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizePlantComponentTemperatureSource: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Fluid Flow Rate of " + General::RoundSigDigits(DesVolFlowRateUser, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Fluid Flow Rate of " +
                                                      General::RoundSigDigits(tmpVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpVolFlowRate = DesVolFlowRateUser;
                    }
                }
            }
        } else {
            if (this->DesVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of plant component temperature source flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in PlantComponent:TemperatureSource object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->DesVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport) {
                if (this->DesVolFlowRate > 0.0) {
                    ReportSizingManager::ReportSizingOutput(
                        "PlantComponent:TemperatureSource", this->Name, "User-Specified Design Fluid Flow Rate [m3/s]", this->DesVolFlowRate);
                }
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->InletNodeNum, tmpVolFlowRate);

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void WaterSourceSpecs::calculate()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   October 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        static std::string const RoutineName("CalcWaterSource");

        if (this->MassFlowRate > 0.0) {
            this->OutletTemp = this->BoundaryTemp;
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->Location.loopNum).FluidName,
                                                               this->BoundaryTemp,
                                                               DataPlant::PlantLoop(this->Location.loopNum).FluidIndex,
                                                               RoutineName);
            this->HeatRate = this->MassFlowRate * Cp * (this->OutletTemp - this->InletTemp);
            this->HeatEnergy = this->HeatRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        } else {
            this->OutletTemp = this->BoundaryTemp;
            this->HeatRate = 0.0;
            this->HeatEnergy = 0.0;
        }
    }

    void WaterSourceSpecs::update()
    {
        DataLoopNode::Node(this->OutletNodeNum).Temp = this->OutletTemp;
    }

    void WaterSourceSpecs::simulate(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation),
                                    bool EP_UNUSED(FirstHVACIteration),
                                    Real64 &CurLoad,
                                    bool EP_UNUSED(RunFlag))
    {
        this->initialize(state.dataBranchInputManager, CurLoad);
        this->calculate();
        this->update();
    }

    void WaterSourceSpecs::getDesignCapacities(const EnergyPlus::PlantLocation &, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {

        MaxLoad = DataGlobals::BigNumber;
        MinLoad = 0.0;
        OptLoad = DataGlobals::BigNumber;
    }

    void WaterSourceSpecs::getSizingFactor(Real64 &_SizFac)
    {
        _SizFac = this->SizFac;
    }

    void WaterSourceSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &)
    {
        Real64 myLoad = 0.0;
        this->initialize(state.dataBranchInputManager, myLoad);
        this->autosize();
    }

    void GetWaterSourceInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Edwin Lee
        //       DATE WRITTEN:    October 2012

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the inputs and processes them into local data structures

        // METHODOLOGY EMPLOYED:
        // Standard E+ input processor interaction

        // REFERENCES:
        // WaterSource,
        //  A1 , \field Name
        //  A2 , \field Inlet Node
        //  A3 , \field Outlet Node
        //  N1 , \field Design Volume Flow Rate
        //  A4 , \field Temperature Specification Type
        //  N2 , \field Boundary Temperature
        //  A5 ; \field Source Temperature Schedule Name

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics

        // LOCAL VARIABLES:
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        static bool ErrorsFound(false);

        // GET NUMBER OF ALL EQUIPMENT TYPES
        cCurrentModuleObject = "PlantComponent:TemperatureSource";
        NumSources = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumSources <= 0) {
            ShowSevereError("No " + cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(WaterSource)) return; // probably not possible, and probably should throw error
        WaterSource.allocate(NumSources);

        // fill arrays
        for (int SourceNum = 1; SourceNum <= NumSources; ++SourceNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          SourceNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            WaterSource(SourceNum).Name = cAlphaArgs(1);

            WaterSource(SourceNum).InletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(2),
                                                                                      ErrorsFound,
                                                                                      cCurrentModuleObject,
                                                                                      cAlphaArgs(1),
                                                                                      DataLoopNode::NodeType_Water,
                                                                                      DataLoopNode::NodeConnectionType_Inlet,
                                                                                      1,
                                                                                      DataLoopNode::ObjectIsNotParent);
            WaterSource(SourceNum).OutletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(3),
                                                                                       ErrorsFound,
                                                                                       cCurrentModuleObject,
                                                                                       cAlphaArgs(1),
                                                                                       DataLoopNode::NodeType_Water,
                                                                                       DataLoopNode::NodeConnectionType_Outlet,
                                                                                       1,
                                                                                       DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Chilled Water Nodes");

            WaterSource(SourceNum).DesVolFlowRate = rNumericArgs(1);
            if (WaterSource(SourceNum).DesVolFlowRate == DataSizing::AutoSize) {
                WaterSource(SourceNum).DesVolFlowRateWasAutoSized = true;
            }

            if (cAlphaArgs(4) == "CONSTANT") {
                WaterSource(SourceNum).TempSpecType = modTempSpecType_Constant;
                WaterSource(SourceNum).BoundaryTemp = rNumericArgs(2);
            } else if (cAlphaArgs(4) == "SCHEDULED") {
                WaterSource(SourceNum).TempSpecType = modTempSpecType_Schedule;
                WaterSource(SourceNum).TempSpecScheduleName = cAlphaArgs(5);
                WaterSource(SourceNum).TempSpecScheduleNum = ScheduleManager::GetScheduleIndex(cAlphaArgs(5));
                if (WaterSource(SourceNum).TempSpecScheduleNum == 0) {
                    ShowSevereError("Input error for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                    ShowContinueError("Invalid schedule name in field " + cAlphaFieldNames(5) + '=' + cAlphaArgs(5));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError("Input error for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ShowContinueError(R"(Invalid temperature specification type.  Expected either "Constant" or "Scheduled". Encountered ")" +
                                  cAlphaArgs(4) + "\"");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + cCurrentModuleObject);
        }
    }

} // namespace PlantComponentTemperatureSources

} // namespace EnergyPlus
