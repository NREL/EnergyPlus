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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantComponentTemperatureSources.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

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
    // Called by plantloopequipment, model accepts inputs, and calculates a
    // thermal response using new plant routines such as SetComponentFlowRate

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using DataGlobals::DisplayExtraWarnings;
    using DataHVACGlobals::SmallWaterVolFlow;
    using DataPlant::TypeOf_WaterSource;
    using General::RoundSigDigits;
    using General::TrimSigDigits;

    // MODULE PARAMETER DEFINITIONS:
    int const TempSpecType_Constant(-1);
    int const TempSpecType_Schedule(-2);

    // MODULE VARIABLES
    int NumSources(0);
    bool GetInput(true); // then TRUE, calls subroutine to read input file.

    // Object Data
    Array1D<WaterSourceSpecs> WaterSource; // dimension to number of machines

    void SimWaterSource(std::string const &SourceName,            // user-specified name for this component
                        int const EP_UNUSED(EquipFlowCtrl),       // Flow control mode for the equipment
                        int &CompIndex,                           // HX number pointer
                        bool const EP_UNUSED(RunFlag),            // simulate HX when TRUE
                        bool const EP_UNUSED(FirstHVACIteration), // initialize variables when TRUE
                        bool &InitLoopEquip,                      // If not zero, calculate the max load for operating conditions
                        Real64 &MyLoad,                           // loop demand component will meet
                        Real64 &MaxLoad,
                        Real64 &MinLoad,
                        Real64 &OptLoad,
                        bool const GetSizingFactor, // TRUE when just the sizing factor is requested
                        Real64 &SizingFactor        // sizing factor
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   October 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This is the water source model driver.  It
        //  gets the input for the models, initializes simulation variables, call
        //  the appropriate model and sets up reporting variables.

        // Using/Aliasing
        using DataGlobals::BigNumber;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SourceNum; // HX number pointer

        // GET INPUT
        if (GetInput) {
            GetWaterSource();
            GetInput = false;
        }

        // Find the correct instance
        if (CompIndex == 0) {
            SourceNum = UtilityRoutines::FindItemInList(SourceName, WaterSource);
            if (SourceNum == 0) {
                ShowFatalError("SimWaterSource: Specified heat exchanger not one of Valid heat exchangers=" + SourceName); // LCOV_EXCL_LINE
            }
            CompIndex = SourceNum;
        } else {
            SourceNum = CompIndex;
            if (SourceNum > NumSources || SourceNum < 1) {
                ShowFatalError("SimWaterSource:  Invalid CompIndex passed=" + TrimSigDigits(SourceNum) +
                               ", Number of Units=" + TrimSigDigits(NumSources) + ", Entered Unit name=" + SourceName); // LCOV_EXCL_LINE
            }
            if (WaterSource(SourceNum).CheckEquipName) {
                if (SourceName != WaterSource(SourceNum).Name) {
                    ShowFatalError("SimWaterSource: Invalid CompIndex passed=" + TrimSigDigits(SourceNum) + ", Unit name=" + SourceName +
                                   ", stored Unit Name for that index=" + WaterSource(SourceNum).Name); // LCOV_EXCL_LINE
                }
                WaterSource(SourceNum).CheckEquipName = false;
            }
        }

        if (InitLoopEquip) {
            InitWaterSource(SourceNum, MyLoad);
            SizeWaterSource(SourceNum);
            if (GetSizingFactor) {
                SizingFactor = WaterSource(SourceNum).SizFac;
            }
            MaxLoad = BigNumber;
            MinLoad = 0.0;
            OptLoad = BigNumber;
            return;
        }

        InitWaterSource(SourceNum, MyLoad);
        CalcWaterSource(SourceNum);
        UpdateWaterSource(SourceNum);
    }

    void GetWaterSource()
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
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataSizing::AutoSize;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SourceNum;
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
        for (SourceNum = 1; SourceNum <= NumSources; ++SourceNum) {
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

            WaterSource(SourceNum).InletNodeNum = GetOnlySingleNode(
                cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            WaterSource(SourceNum).OutletNodeNum = GetOnlySingleNode(
                cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Chilled Water Nodes");

            WaterSource(SourceNum).DesVolFlowRate = rNumericArgs(1);
            if (WaterSource(SourceNum).DesVolFlowRate == AutoSize) {
                WaterSource(SourceNum).DesVolFlowRateWasAutoSized = true;
            }

            if (cAlphaArgs(4) == "CONSTANT") {
                WaterSource(SourceNum).TempSpecType = TempSpecType_Constant;
                WaterSource(SourceNum).BoundaryTemp = rNumericArgs(2);
            } else if (cAlphaArgs(4) == "SCHEDULED") {
                WaterSource(SourceNum).TempSpecType = TempSpecType_Schedule;
                WaterSource(SourceNum).TempSpecScheduleName = cAlphaArgs(5);
                WaterSource(SourceNum).TempSpecScheduleNum = GetScheduleIndex(cAlphaArgs(5));
                if (WaterSource(SourceNum).TempSpecScheduleNum == 0) {
                    ShowSevereError("Input error for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                    ShowContinueError("Invalid schedule name in field " + cAlphaFieldNames(5) + '=' + cAlphaArgs(5));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError("Input error for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ShowContinueError("Invalid temperature specification type.  Expected either \"Constant\" or \"Scheduled\". Encountered \"" +
                                  cAlphaArgs(4) + "\"");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + cCurrentModuleObject);
        }

        for (SourceNum = 1; SourceNum <= NumSources; ++SourceNum) {
            SetupOutputVariable("Plant Temperature Source Component Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                WaterSource(SourceNum).MassFlowRate,
                                "System",
                                "Average",
                                WaterSource(SourceNum).Name);
            SetupOutputVariable("Plant Temperature Source Component Inlet Temperature",
                                OutputProcessor::Unit::C,
                                WaterSource(SourceNum).InletTemp,
                                "System",
                                "Average",
                                WaterSource(SourceNum).Name);
            SetupOutputVariable("Plant Temperature Source Component Outlet Temperature",
                                OutputProcessor::Unit::C,
                                WaterSource(SourceNum).OutletTemp,
                                "System",
                                "Average",
                                WaterSource(SourceNum).Name);
            SetupOutputVariable("Plant Temperature Source Component Source Temperature",
                                OutputProcessor::Unit::C,
                                WaterSource(SourceNum).BoundaryTemp,
                                "System",
                                "Average",
                                WaterSource(SourceNum).Name);
            SetupOutputVariable("Plant Temperature Source Component Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                WaterSource(SourceNum).HeatRate,
                                "System",
                                "Average",
                                WaterSource(SourceNum).Name);
            SetupOutputVariable("Plant Temperature Source Component Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                WaterSource(SourceNum).HeatEnergy,
                                "System",
                                "Sum",
                                WaterSource(SourceNum).Name);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("PlantComponent:TemperatureSource",
                                 WaterSource(SourceNum).Name,
                                 "Maximum Mass Flow Rate",
                                 "[kg/s]",
                                 WaterSource(SourceNum).EMSOverrideOnMassFlowRateMax,
                                 WaterSource(SourceNum).EMSOverrideValueMassFlowRateMax);
            }
        }
    }

    void InitWaterSource(int const SourceNum, // number of the current component being simulated
                         Real64 const MyLoad)
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

        // Using/Aliasing
        using DataGlobals::BeginEnvrnFlag;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitWaterSource");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 rho; // local fluid density
        Real64 cp;  // local specific heat
        bool errFlag;

        // Init more variables
        if (WaterSource(SourceNum).MyFlag) {
            // Locate the component on the plant loops for later usage
            errFlag = false;
            ScanPlantLoopsForObject(WaterSource(SourceNum).Name,
                                    TypeOf_WaterSource,
                                    WaterSource(SourceNum).Location.loopNum,
                                    WaterSource(SourceNum).Location.loopSideNum,
                                    WaterSource(SourceNum).Location.branchNum,
                                    WaterSource(SourceNum).Location.compNum,
                                    _,
                                    _,
                                    _,
                                    WaterSource(SourceNum).InletNodeNum,
                                    _,
                                    errFlag);
            if (errFlag) {
                ShowFatalError(RoutineName + ": Program terminated due to previous condition(s).");
            }
            WaterSource(SourceNum).MyFlag = false;
        }

        // Initialize critical Demand Side Variables at the beginning of each environment
        if (WaterSource(SourceNum).MyEnvironFlag && BeginEnvrnFlag && (PlantFirstSizesOkayToFinalize)) {

            rho = GetDensityGlycol(PlantLoop(WaterSource(SourceNum).Location.loopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   PlantLoop(WaterSource(SourceNum).Location.loopNum).FluidIndex,
                                   RoutineName);
            WaterSource(SourceNum).MassFlowRateMax = WaterSource(SourceNum).DesVolFlowRate * rho;
            InitComponentNodes(0.0,
                               WaterSource(SourceNum).MassFlowRateMax,
                               WaterSource(SourceNum).InletNodeNum,
                               WaterSource(SourceNum).OutletNodeNum,
                               WaterSource(SourceNum).Location.loopNum,
                               WaterSource(SourceNum).Location.loopSideNum,
                               WaterSource(SourceNum).Location.branchNum,
                               WaterSource(SourceNum).Location.compNum);

            WaterSource(SourceNum).MyEnvironFlag = false;
        }

        if (!BeginEnvrnFlag) {
            WaterSource(SourceNum).MyEnvironFlag = true;
        }

        // OK, so we can set up the inlet and boundary temperatures now
        WaterSource(SourceNum).InletTemp = Node(WaterSource(SourceNum).InletNodeNum).Temp;
        if (WaterSource(SourceNum).TempSpecType == TempSpecType_Schedule) {
            WaterSource(SourceNum).BoundaryTemp = GetCurrentScheduleValue(WaterSource(SourceNum).TempSpecScheduleNum);
        }

        // Calculate specific heat
        cp = GetSpecificHeatGlycol(PlantLoop(WaterSource(SourceNum).Location.loopNum).FluidName,
                                   WaterSource(SourceNum).BoundaryTemp,
                                   PlantLoop(WaterSource(SourceNum).Location.loopNum).FluidIndex,
                                   RoutineName);

        // Calculate deltaT
        Real64 delta_temp = WaterSource(SourceNum).BoundaryTemp - WaterSource(SourceNum).InletTemp;

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
        if (abs(delta_temp) < 0.001) {
            if (abs(MyLoad) < 0.001) {
                WaterSource(SourceNum).MassFlowRate = 0.0;
            } else {
                WaterSource(SourceNum).MassFlowRate = WaterSource(SourceNum).MassFlowRateMax;
            }
        } else {
            WaterSource(SourceNum).MassFlowRate = MyLoad / (cp * delta_temp);
        }

        // If the mdot is negative it means we can't help the load so we will want to just go to zero.
        // If the mdot is already zero, then well, we still want to go to zero
        // If the mdot is positive, just make sure we constrain it to the design value
        if (WaterSource(SourceNum).MassFlowRate < 0) {
            WaterSource(SourceNum).MassFlowRate = 0.0;
        } else {
            if (!WaterSource(SourceNum).EMSOverrideOnMassFlowRateMax) {
                WaterSource(SourceNum).MassFlowRate = min(WaterSource(SourceNum).MassFlowRate, WaterSource(SourceNum).MassFlowRateMax);
            } else {
                WaterSource(SourceNum).MassFlowRate =
                    min(WaterSource(SourceNum).MassFlowRate, WaterSource(SourceNum).EMSOverrideValueMassFlowRateMax);
            }
        }

        SetComponentFlowRate(WaterSource(SourceNum).MassFlowRate,
                             WaterSource(SourceNum).InletNodeNum,
                             WaterSource(SourceNum).OutletNodeNum,
                             WaterSource(SourceNum).Location.loopNum,
                             WaterSource(SourceNum).Location.loopSideNum,
                             WaterSource(SourceNum).Location.branchNum,
                             WaterSource(SourceNum).Location.compNum);

        // at this point the mass flow rate, inlet temp, and boundary temp structure vars have been updated
        // the calc routine will update the outlet temp and heat transfer rate/energies
    }

    void SizeWaterSource(int const SourceNum)
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

        // Using/Aliasing
        using namespace DataSizing;
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;
        using DataPlant::PlantLoop;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using ReportSizingManager::ReportSizingOutput;
        using namespace OutputReportPredefined;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum(0);               // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound(false);        // If errors detected in input
        Real64 tmpVolFlowRate;          // local design volume flow rate
        Real64 DesVolFlowRateUser(0.0); // Hardsized design volume flow rate for reporting

        tmpVolFlowRate = WaterSource(SourceNum).DesVolFlowRate;

        PltSizNum = PlantLoop(WaterSource(SourceNum).Location.loopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (PlantSizData(PltSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                tmpVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate; //* WaterSource(SourceNum)%SizFac
                if (!WaterSource(SourceNum).DesVolFlowRateWasAutoSized) tmpVolFlowRate = WaterSource(SourceNum).DesVolFlowRate;
            } else {
                if (WaterSource(SourceNum).DesVolFlowRateWasAutoSized) tmpVolFlowRate = 0.0;
            }
            if (PlantFirstSizesOkayToFinalize) {
                if (WaterSource(SourceNum).DesVolFlowRateWasAutoSized) {
                    WaterSource(SourceNum).DesVolFlowRate = tmpVolFlowRate;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput("PlantComponent:TemperatureSource",
                                           WaterSource(SourceNum).Name,
                                           "Design Size Design Fluid Flow Rate [m3/s]",
                                           tmpVolFlowRate);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput("PlantComponent:TemperatureSource",
                                           WaterSource(SourceNum).Name,
                                           "Initial Design Size Design Fluid Flow Rate [m3/s]",
                                           tmpVolFlowRate);
                    }
                } else {
                    if (WaterSource(SourceNum).DesVolFlowRate > 0.0 && tmpVolFlowRate > 0.0) {
                        DesVolFlowRateUser = WaterSource(SourceNum).DesVolFlowRate;
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput("PlantComponent:TemperatureSource",
                                               WaterSource(SourceNum).Name,
                                               "Design Size Design Fluid Flow Rate [m3/s]",
                                               tmpVolFlowRate,
                                               "User-Specified Design Fluid Flow Rate [m3/s]",
                                               DesVolFlowRateUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(tmpVolFlowRate - DesVolFlowRateUser) / DesVolFlowRateUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizePlantComponentTemperatureSource: Potential issue with equipment sizing for " +
                                                WaterSource(SourceNum).Name);
                                    ShowContinueError("User-Specified Design Fluid Flow Rate of " + RoundSigDigits(DesVolFlowRateUser, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Fluid Flow Rate of " + RoundSigDigits(tmpVolFlowRate, 5) +
                                                      " [m3/s]");
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
            if (WaterSource(SourceNum).DesVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of plant component temperature source flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in PlantComponent:TemperatureSource object=" + WaterSource(SourceNum).Name);
                ErrorsFound = true;
            }
            if (!WaterSource(SourceNum).DesVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport) {
                if (WaterSource(SourceNum).DesVolFlowRate > 0.0) {
                    ReportSizingOutput("PlantComponent:TemperatureSource",
                                       WaterSource(SourceNum).Name,
                                       "User-Specified Design Fluid Flow Rate [m3/s]",
                                       WaterSource(SourceNum).DesVolFlowRate);
                }
            }
        }

        RegisterPlantCompDesignFlow(WaterSource(SourceNum).InletNodeNum, tmpVolFlowRate);

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void CalcWaterSource(int const SourceNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   October 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHVACGlobals::TimeStepSys;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcWaterSource");

        if (WaterSource(SourceNum).MassFlowRate > 0.0) {
            WaterSource(SourceNum).OutletTemp = WaterSource(SourceNum).BoundaryTemp;
            Real64 Cp = GetSpecificHeatGlycol(PlantLoop(WaterSource(SourceNum).Location.loopNum).FluidName,
                                              WaterSource(SourceNum).BoundaryTemp,
                                              PlantLoop(WaterSource(SourceNum).Location.loopNum).FluidIndex,
                                              RoutineName);
            WaterSource(SourceNum).HeatRate =
                WaterSource(SourceNum).MassFlowRate * Cp * (WaterSource(SourceNum).OutletTemp - WaterSource(SourceNum).InletTemp);
            WaterSource(SourceNum).HeatEnergy = WaterSource(SourceNum).HeatRate * TimeStepSys * SecInHour;
        } else {
            WaterSource(SourceNum).OutletTemp = WaterSource(SourceNum).BoundaryTemp;
            WaterSource(SourceNum).HeatRate = 0.0;
            WaterSource(SourceNum).HeatEnergy = 0.0;
        }
    }

    void UpdateWaterSource(int const SourceNum)
    {
        int OutletNode = WaterSource(SourceNum).OutletNodeNum;
        Node(OutletNode).Temp = WaterSource(SourceNum).OutletTemp;
    }

} // namespace PlantComponentTemperatureSources

} // namespace EnergyPlus
