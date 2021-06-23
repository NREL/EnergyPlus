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
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CTElectricGenerator.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace CTElectricGenerator {

    //__________________________________________________________________________;
    // BLAST inherited generators:
    // CTElectricGenerator (COMBUSTION Turbine)

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Sept 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates the performance of the COMBUSTION turbine
    // Generators.

    // METHODOLOGY EMPLOYED:
    // Once the Electric power manager determines that the CT Generator
    // is available, it calls SimCTGenerator which in turn calls the
    // appropriate COMBUSTION turbine Generator model.
    // All CT Generator models are based on a polynomial fit of Generator
    // performance data.

    PlantComponent *CTGeneratorData::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data for generators if it hasn't been done already
        if (state.dataCTElectricGenerator->getCTInputFlag) {
            GetCTGeneratorInput(state);
            state.dataCTElectricGenerator->getCTInputFlag = false;
        }

        // Now look for this particular generator in the list
        for (auto &CTGen : state.dataCTElectricGenerator->CTGenerator) {
            if (CTGen.Name == objectName) {
                return &CTGen;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state,
                       "LocalCombustionTurbineGeneratorFactory: Error getting inputs for combustion turbine generator named: " +
                           objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void CTGeneratorData::simulate([[maybe_unused]] EnergyPlusData &state,
                                   [[maybe_unused]] const EnergyPlus::PlantLocation &calledFromLocation,
                                   [[maybe_unused]] bool FirstHVACIteration,
                                   [[maybe_unused]] Real64 &CurLoad,
                                   [[maybe_unused]] bool RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This is the CT Generator driver.  It
        // gets the input for the models, initializes simulation variables, call
        // the appropriate model and sets up reporting variables.

        // empty function to emulate current behavior as of conversion to using the PlantComponent calling structure.
        // calls from the plant side... do nothing.
        // calls from the ElectricPowerServiceManger call the init and calculation worker functions directly.
    }

    void GetCTGeneratorInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the CT Generator models.

        int NumAlphas;                // Number of elements in the alpha array
        int NumNums;                  // Number of elements in the numeric array
        int IOStat;                   // IO Status when calling get input subroutine
        Array1D_string AlphArray(12); // character string data
        Array1D<Real64> NumArray(12); // numeric data
        bool ErrorsFound(false);      // error flag

        state.dataIPShortCut->cCurrentModuleObject = "Generator:CombustionTurbine";
        state.dataCTElectricGenerator->NumCTGenerators =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataCTElectricGenerator->NumCTGenerators <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        state.dataCTElectricGenerator->CTGenerator.allocate(state.dataCTElectricGenerator->NumCTGenerators);

        // LOAD ARRAYS WITH CT CURVE FIT Generator DATA
        for (int genNum = 1; genNum <= state.dataCTElectricGenerator->NumCTGenerators; ++genNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     genNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);

            state.dataCTElectricGenerator->CTGenerator(genNum).Name = AlphArray(1);

            state.dataCTElectricGenerator->CTGenerator(genNum).RatedPowerOutput = NumArray(1);
            if (NumArray(1) == 0.0) {
                ShowSevereError(state, format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), NumArray(1)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            // Not sure what to do with electric nodes, so do not use optional arguments
            state.dataCTElectricGenerator->CTGenerator(genNum).ElectricCircuitNode =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(2),
                                                    ErrorsFound,
                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Electric,
                                                    DataLoopNode::NodeConnectionType::Electric,
                                                    NodeInputManager::compFluidStream::Primary,
                                                    DataLoopNode::ObjectIsNotParent);

            state.dataCTElectricGenerator->CTGenerator(genNum).MinPartLoadRat = NumArray(2);
            state.dataCTElectricGenerator->CTGenerator(genNum).MaxPartLoadRat = NumArray(3);
            state.dataCTElectricGenerator->CTGenerator(genNum).OptPartLoadRat = NumArray(4);

            // Load Special CT Generator Input

            state.dataCTElectricGenerator->CTGenerator(genNum).PLBasedFuelInputCurve =
                CurveManager::GetCurveIndex(state, AlphArray(3)); // convert curve name to number
            if (state.dataCTElectricGenerator->CTGenerator(genNum).PLBasedFuelInputCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + '=' + AlphArray(3));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).TempBasedFuelInputCurve =
                CurveManager::GetCurveIndex(state, AlphArray(4)); // convert curve name to number
            if (state.dataCTElectricGenerator->CTGenerator(genNum).TempBasedFuelInputCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + '=' + AlphArray(4));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).ExhaustFlowCurve =
                CurveManager::GetCurveIndex(state, AlphArray(5)); // convert curve name to number
            if (state.dataCTElectricGenerator->CTGenerator(genNum).ExhaustFlowCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + '=' + AlphArray(5));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).PLBasedExhaustTempCurve =
                CurveManager::GetCurveIndex(state, AlphArray(6)); // convert curve name to number
            if (state.dataCTElectricGenerator->CTGenerator(genNum).PLBasedExhaustTempCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + AlphArray(6));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).TempBasedExhaustTempCurve =
                CurveManager::GetCurveIndex(state, AlphArray(7)); // convert curve name to number
            if (state.dataCTElectricGenerator->CTGenerator(genNum).TempBasedExhaustTempCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphArray(7));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).QLubeOilRecoveredCurve =
                CurveManager::GetCurveIndex(state, AlphArray(8)); // convert curve name to number
            if (state.dataCTElectricGenerator->CTGenerator(genNum).QLubeOilRecoveredCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphArray(8));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).UACoef(1) = NumArray(5);
            state.dataCTElectricGenerator->CTGenerator(genNum).UACoef(2) = NumArray(6);

            state.dataCTElectricGenerator->CTGenerator(genNum).MaxExhaustperCTPower = NumArray(7);
            state.dataCTElectricGenerator->CTGenerator(genNum).DesignMinExitGasTemp = NumArray(8);
            state.dataCTElectricGenerator->CTGenerator(genNum).DesignAirInletTemp = NumArray(9);
            state.dataCTElectricGenerator->CTGenerator(genNum).FuelHeatingValue = NumArray(10);
            state.dataCTElectricGenerator->CTGenerator(genNum).DesignHeatRecVolFlowRate = NumArray(11);

            if (state.dataCTElectricGenerator->CTGenerator(genNum).DesignHeatRecVolFlowRate > 0.0) {
                state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecActive = true;
                state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(9),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        AlphArray(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                        NodeInputManager::compFluidStream::Primary,
                                                        DataLoopNode::ObjectIsNotParent);
                if (state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecInletNodeNum == 0) {
                    ShowSevereError(state,
                                    "Missing Node Name, Heat Recovery Inlet, for " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(10),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        AlphArray(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        NodeInputManager::compFluidStream::Primary,
                                                        DataLoopNode::ObjectIsNotParent);
                if (state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecOutletNodeNum == 0) {
                    ShowSevereError(
                        state, "Missing Node Name, Heat Recovery Outlet, for " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(
                    state, state.dataIPShortCut->cCurrentModuleObject, AlphArray(1), AlphArray(9), AlphArray(10), "Heat Recovery Nodes");
                PlantUtilities::RegisterPlantCompDesignFlow(state,
                                                            state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecInletNodeNum,
                                                            state.dataCTElectricGenerator->CTGenerator(genNum).DesignHeatRecVolFlowRate);
            } else {
                state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecActive = false;
                state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecInletNodeNum = 0;
                state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecOutletNodeNum = 0;
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9) || !state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    ShowWarningError(state,
                                     "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + state.dataIPShortCut->cCurrentModuleObject +
                                         '=' + AlphArray(1));
                    ShowContinueError(state, "However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }

            // Validate fuel type input
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelType(state, AlphArray(11), state.dataCTElectricGenerator->CTGenerator(genNum).FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + AlphArray(11));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
                FuelTypeError = false;
            }

            state.dataCTElectricGenerator->CTGenerator(genNum).HeatRecMaxTemp = NumArray(12);

            // begin CR7021
            if (state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                state.dataCTElectricGenerator->CTGenerator(genNum).OAInletNode = 0;
            } else {
                state.dataCTElectricGenerator->CTGenerator(genNum).OAInletNode =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(12),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        AlphArray(1),
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                        NodeInputManager::compFluidStream::Primary,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataCTElectricGenerator->CTGenerator(genNum).OAInletNode)) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + ", \"" + state.dataCTElectricGenerator->CTGenerator(genNum).Name +
                                        "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(12));
                    ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void CTGeneratorData::setupOutputVars(EnergyPlusData &state)
    {
        SetupOutputVariable(
            state, "Generator Produced AC Electricity Rate", OutputProcessor::Unit::W, this->ElecPowerGenerated, "System", "Average", this->Name);

        SetupOutputVariable(state,
                            "Generator Produced AC Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->ElecEnergyGenerated,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ElectricityProduced",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(
            state, "Generator " + this->FuelType + " Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRate, "System", "Average", this->Name);

        SetupOutputVariable(state,
                            "Generator " + this->FuelType + " Energy",
                            OutputProcessor::Unit::J,
                            this->FuelEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            this->FuelType,
                            "COGENERATION",
                            _,
                            "Plant");

        //    general fuel use report (to match other generators)
        SetupOutputVariable(
            state, "Generator Fuel HHV Basis Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRate, "System", "Average", this->Name);

        SetupOutputVariable(state, "Generator Fuel HHV Basis Energy", OutputProcessor::Unit::J, this->FuelEnergy, "System", "Sum", this->Name);

        SetupOutputVariable(
            state, "Generator " + this->FuelType + " Mass Flow Rate", OutputProcessor::Unit::kg_s, this->FuelMdot, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Generator Exhaust Air Temperature", OutputProcessor::Unit::C, this->ExhaustStackTemp, "System", "Average", this->Name);

        if (this->HeatRecActive) {
            SetupOutputVariable(
                state, "Generator Exhaust Heat Recovery Rate", OutputProcessor::Unit::W, this->QExhaustRecovered, "System", "Average", this->Name);

            SetupOutputVariable(state,
                                "Generator Exhaust Heat Recovery Energy",
                                OutputProcessor::Unit::J,
                                this->ExhaustEnergyRec,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable(
                state, "Generator Lube Heat Recovery Rate", OutputProcessor::Unit::W, this->QLubeOilRecovered, "System", "Average", this->Name);

            SetupOutputVariable(state,
                                "Generator Lube Heat Recovery Energy",
                                OutputProcessor::Unit::J,
                                this->LubeOilEnergyRec,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable(
                state, "Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->QTotalHeatRecovered, "System", "Average", this->Name);

            SetupOutputVariable(
                state, "Generator Produced Thermal Energy", OutputProcessor::Unit::J, this->TotalHeatEnergyRec, "System", "Sum", this->Name);

            SetupOutputVariable(state,
                                "Generator Heat Recovery Inlet Temperature",
                                OutputProcessor::Unit::C,
                                this->HeatRecInletTemp,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(state,
                                "Generator Heat Recovery Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->HeatRecOutletTemp,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                state, "Generator Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMdot, "System", "Average", this->Name);
        }
    }

    void CTGeneratorData::CalcCTGeneratorModel(EnergyPlusData &state,
                                               bool const RunFlag,  // TRUE when Generator operating
                                               Real64 const MyLoad, // Generator demand
                                               bool const FirstHVACIteration)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a vapor compression Generator using the CT model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data.  This model was originally
        // developed by Dale Herron for the BLAST program

        Real64 const exhaustCp(1.047); // Exhaust Gas Specific Heat (J/kg-K)
        Real64 const KJtoJ(1000.0);    // convert Kjoules to joules
        static std::string const RoutineName("CalcCTGeneratorModel");

        // min allowed operating frac full load
        Real64 minPartLoadRat = this->MinPartLoadRat;

        // max allowed operating frac full load
        Real64 maxPartLoadRat = this->MaxPartLoadRat;

        // Generator nominal capacity (W)
        Real64 ratedPowerOutput = this->RatedPowerOutput;

        // MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
        Real64 maxExhaustperCTPower = this->MaxExhaustperCTPower;

        // design turbine inlet temperature (C)
        Real64 designAirInletTemp = this->DesignAirInletTemp;

        int heatRecInNode;    // Heat Recovery Fluid Inlet Node Num
        Real64 heatRecInTemp; // Heat Recovery Fluid Inlet Temperature (C)

        Real64 heatRecMdot; // Heat Recovery Fluid Mass FlowRate (kg/s)
        Real64 heatRecCp;   // Specific Heat of the Heat Recovery Fluid (J/kg-K)

        if (this->HeatRecActive) {
            heatRecInNode = this->HeatRecInletNodeNum;
            heatRecInTemp = state.dataLoopNodes->Node(heatRecInNode).Temp;

            heatRecCp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                               heatRecInTemp,
                                                               state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                               RoutineName);
            if (FirstHVACIteration && RunFlag) {
                heatRecMdot = this->DesignHeatRecMassFlowRate;
            } else {
                heatRecMdot = state.dataLoopNodes->Node(heatRecInNode).MassFlowRate;
            }
        } else {
            heatRecInTemp = 0.0;
            heatRecCp = 0.0;
            heatRecMdot = 0.0;
        }

        // If no loop demand or Generator OFF, return
        if (!RunFlag) {
            this->ElecPowerGenerated = 0.0;
            this->ElecEnergyGenerated = 0.0;
            this->HeatRecInletTemp = heatRecInTemp;
            this->HeatRecOutletTemp = heatRecInTemp;
            this->HeatRecMdot = 0.0;
            this->QLubeOilRecovered = 0.0;
            this->QExhaustRecovered = 0.0;
            this->QTotalHeatRecovered = 0.0;
            this->LubeOilEnergyRec = 0.0;
            this->ExhaustEnergyRec = 0.0;
            this->TotalHeatEnergyRec = 0.0;
            this->FuelEnergyUseRate = 0.0;
            this->FuelEnergy = 0.0;
            this->FuelMdot = 0.0;
            this->ExhaustStackTemp = 0.0;
            return;
        }

        // CALCULATE POWER GENERATED AND PLR
        // Generator output (W)
        Real64 elecPowerGenerated = min(MyLoad, ratedPowerOutput);
        elecPowerGenerated = max(elecPowerGenerated, 0.0);

        // Generator operating part load ratio
        Real64 PLR = min(elecPowerGenerated / ratedPowerOutput, maxPartLoadRat);
        PLR = max(PLR, minPartLoadRat);
        elecPowerGenerated = PLR * ratedPowerOutput;

        // SET OFF-DESIGN AIR TEMPERATURE DIFFERENCE
        // (ATAIR) Difference between ambient actual and ambient design temperatures
        Real64 ambientDeltaT;
        if (this->OAInletNode == 0) {
            ambientDeltaT = state.dataEnvrn->OutDryBulbTemp - designAirInletTemp;
        } else {
            ambientDeltaT = state.dataLoopNodes->Node(this->OAInletNode).Temp - designAirInletTemp;
        }

        // Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
        // energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
        // The TempBasedFuelInputCurve is a correction based on deviation from design inlet air temperature conditions.
        // The first coefficient of this fit should be 1.0 to ensure that no correction is made at design conditions.
        // (EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
        Real64 FuelUseRate = elecPowerGenerated * CurveManager::CurveValue(state, this->PLBasedFuelInputCurve, PLR) *
                             CurveManager::CurveValue(state, this->TempBasedFuelInputCurve, ambientDeltaT);

        // Use Curve fit to determine Exhaust Flow.  This curve shows the ratio of exhaust gas flow (kg/s) to electric power
        // output (J/s).  The units on ExhaustFlowCurve are (kg/J).  When multiplied by the rated power of the unit,
        // it gives the exhaust flow rate in kg/s
        // (FEX) Exhaust Gas Flow Rate cubic meters per second???
        Real64 exhaustFlow = ratedPowerOutput * CurveManager::CurveValue(state, this->ExhaustFlowCurve, ambientDeltaT);

        // Use Curve fit to determine Exhaust Temperature.  This curve calculates the exhaust temperature (C) by
        // multiplying the exhaust temperature (C) for a particular part load as given by PLBasedExhaustTempCurve
        // a correction factor based on the deviation from design temperature, TempBasedExhaustTempCurve

        Real64 QExhaustRec;      // recovered exhaust heat (W)
        Real64 exhaustStackTemp; // turbine stack temp. (C)
        if ((PLR > 0.0) && ((exhaustFlow > 0.0) || (maxExhaustperCTPower > 0.0))) {

            // (TEX) Exhaust Gas Temperature in C
            Real64 exhaustTemp = CurveManager::CurveValue(state, this->PLBasedExhaustTempCurve, PLR) *
                                 CurveManager::CurveValue(state, this->TempBasedExhaustTempCurve, ambientDeltaT);

            // (UACGC) Heat Exchanger UA to Capacity
            Real64 UA_loc = this->UACoef(1) * std::pow(ratedPowerOutput, this->UACoef(2));

            // design engine stack saturated steam temp. (C)
            Real64 designMinExitGasTemp = this->DesignMinExitGasTemp;

            exhaustStackTemp = designMinExitGasTemp + (exhaustTemp - designMinExitGasTemp) /
                                                          std::exp(UA_loc / (max(exhaustFlow, maxExhaustperCTPower * ratedPowerOutput) * exhaustCp));

            QExhaustRec = max(exhaustFlow * exhaustCp * (exhaustTemp - exhaustStackTemp), 0.0);
        } else {
            exhaustStackTemp = this->DesignMinExitGasTemp;
            QExhaustRec = 0.0;
        }

        // Use Curve fit to determine Heat Recovered Lubricant heat.  This curve calculates the lube heat recovered (J/s) by
        // multiplying the total power generated by the fraction of that power that could be recovered in the lube oil at that
        // particular part load.
        // recovered lube oil heat (W)
        Real64 QLubeOilRec = elecPowerGenerated * CurveManager::CurveValue(state, this->QLubeOilRecoveredCurve, PLR);

        // Check for divide by zero
        Real64 HeatRecOutTemp; // Heat Recovery Fluid Outlet Temperature (C)
        if ((heatRecMdot > 0.0) && (heatRecCp > 0.0)) {
            HeatRecOutTemp = (QExhaustRec + QLubeOilRec) / (heatRecMdot * heatRecCp) + heatRecInTemp;
        } else {
            heatRecMdot = 0.0;
            HeatRecOutTemp = heatRecInTemp;
            QExhaustRec = 0.0;
            QLubeOilRec = 0.0;
        }

        // Now verify the maximum temperature was not exceeded
        // Heat Recovery Flow Rate if minimal heat recovery is accomplished
        Real64 MinHeatRecMdot = 0.0;

        Real64 HRecRatio; // When Max Temp is reached the amount of recovered heat has to be reduced.

        if (HeatRecOutTemp > this->HeatRecMaxTemp) {
            if (this->HeatRecMaxTemp != heatRecInTemp) {
                MinHeatRecMdot = (QExhaustRec + QLubeOilRec) / (heatRecCp * (this->HeatRecMaxTemp - heatRecInTemp));
                if (MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0;
            }

            // Recalculate Outlet Temperature, with adjusted flowrate
            if ((MinHeatRecMdot > 0.0) && (heatRecCp > 0.0)) {
                HeatRecOutTemp = (QExhaustRec + QLubeOilRec) / (MinHeatRecMdot * heatRecCp) + heatRecInTemp;
                HRecRatio = heatRecMdot / MinHeatRecMdot;
            } else {
                HeatRecOutTemp = heatRecInTemp;
                HRecRatio = 0.0;
            }
            QLubeOilRec *= HRecRatio;
            QExhaustRec *= HRecRatio;
        }

        // Calculate Energy
        // Generator output (J)
        Real64 ElectricEnergyGen = elecPowerGenerated * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // Amount of Fuel Energy Required to run COMBUSTION turbine (J)
        Real64 FuelEnergyUsed = FuelUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // recovered lube oil heat (J)
        Real64 lubeOilEnergyRec = QLubeOilRec * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // recovered exhaust heat (J)
        Real64 exhaustEnergyRec = QExhaustRec * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        this->ElecPowerGenerated = elecPowerGenerated;
        this->ElecEnergyGenerated = ElectricEnergyGen;

        this->HeatRecInletTemp = heatRecInTemp;
        this->HeatRecOutletTemp = HeatRecOutTemp;

        this->HeatRecMdot = heatRecMdot;
        this->QExhaustRecovered = QExhaustRec;
        this->QLubeOilRecovered = QLubeOilRec;
        this->QTotalHeatRecovered = QExhaustRec + QLubeOilRec;
        this->FuelEnergyUseRate = std::abs(FuelUseRate);
        this->ExhaustEnergyRec = exhaustEnergyRec;
        this->LubeOilEnergyRec = lubeOilEnergyRec;
        this->TotalHeatEnergyRec = exhaustEnergyRec + lubeOilEnergyRec;
        this->FuelEnergy = std::abs(FuelEnergyUsed);

        // Heating Value of Fuel in (kJ/kg)
        Real64 fuelHeatingValue = this->FuelHeatingValue;

        this->FuelMdot = std::abs(FuelUseRate) / (fuelHeatingValue * KJtoJ);

        this->ExhaustStackTemp = exhaustStackTemp;

        if (this->HeatRecActive) {
            int HeatRecOutletNode = this->HeatRecOutletNodeNum;
            state.dataLoopNodes->Node(HeatRecOutletNode).Temp = this->HeatRecOutletTemp;
        }
    }

    void CTGeneratorData::InitCTGenerators(EnergyPlusData &state,
                                           bool const RunFlag, // TRUE when Generator operating
                                           bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  Brent Griffith, Sept 2010 plant upgrades, generalize fluid props

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the CT generators.

        auto constexpr RoutineName("InitICEngineGenerators");

        bool errFlag;

        if (this->MyPlantScanFlag && allocated(state.dataPlnt->PlantLoop) && this->HeatRecActive) {
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    DataPlant::TypeOf_Generator_CTurbine,
                                                    this->HRLoopNum,
                                                    this->HRLoopSideNum,
                                                    this->HRBranchNum,
                                                    this->HRCompNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitCTGenerators: Program terminated due to previous condition(s).");
            }

            this->MyPlantScanFlag = false;
        }

        if (this->MyFlag) {
            this->setupOutputVars(state);
            this->MyFlag = false;
        }

        if (this->MySizeAndNodeInitFlag && (!this->MyPlantScanFlag) && this->HeatRecActive) {
            int HeatRecInletNode = this->HeatRecInletNodeNum;
            int HeatRecOutletNode = this->HeatRecOutletNodeNum;

            // size mass flow rate
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                           RoutineName);

            this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->DesignHeatRecMassFlowRate,
                                               HeatRecInletNode,
                                               HeatRecOutletNode,
                                               this->HRLoopNum,
                                               this->HRLoopSideNum,
                                               this->HRBranchNum,
                                               this->HRCompNum);

            this->MySizeAndNodeInitFlag = false;
        } // end one time inits

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag && this->HeatRecActive) {
            int HeatRecInletNode = this->HeatRecInletNodeNum;
            int HeatRecOutletNode = this->HeatRecOutletNodeNum;
            // set the node Temperature, assuming freeze control
            state.dataLoopNodes->Node(HeatRecInletNode).Temp = 20.0;
            state.dataLoopNodes->Node(HeatRecOutletNode).Temp = 20.0;
            // set the node max and min mass flow rates
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->DesignHeatRecMassFlowRate,
                                               HeatRecInletNode,
                                               HeatRecOutletNode,
                                               this->HRLoopNum,
                                               this->HRLoopSideNum,
                                               this->HRBranchNum,
                                               this->HRCompNum);

            this->MyEnvrnFlag = false;
        } // end environmental inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if (this->HeatRecActive) {
            if (FirstHVACIteration) {
                Real64 mdot;
                if (RunFlag) {
                    mdot = this->DesignHeatRecMassFlowRate;
                } else {
                    mdot = 0.0;
                }
                PlantUtilities::SetComponentFlowRate(state,
                                                     mdot,
                                                     this->HeatRecInletNodeNum,
                                                     this->HeatRecOutletNodeNum,
                                                     this->HRLoopNum,
                                                     this->HRLoopSideNum,
                                                     this->HRBranchNum,
                                                     this->HRCompNum);

            } else {
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->HeatRecMdot,
                                                     this->HeatRecInletNodeNum,
                                                     this->HeatRecOutletNodeNum,
                                                     this->HRLoopNum,
                                                     this->HRLoopSideNum,
                                                     this->HRBranchNum,
                                                     this->HRCompNum);
            }
        }
    }

} // namespace CTElectricGenerator

} // namespace EnergyPlus
