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
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/ICEngineElectricGenerator.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ICEngineElectricGenerator {

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Sept. 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates the operation of IC ENGINE Generators.

    // METHODOLOGY EMPLOYED:
    // Once the ElectricPowerManager determines that the IC ENGINE Generator
    // is available to meet an electric load demand, it calls SimICEngineGenerator
    // which in turn calls the ICEngine Generator model.

    PlantComponent *ICEngineGeneratorSpecs::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data for ICEGen if it hasn't been done already
        if (state.dataICEngElectGen->getICEInput) {
            GetICEngineGeneratorInput(state);
            state.dataICEngElectGen->getICEInput = false;
        }

        // Now look for this particular generator in the list
        for (auto &thisICE : state.dataICEngElectGen->ICEngineGenerator) {
            if (thisICE.Name == objectName) {
                return &thisICE;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state,
                       "LocalICEngineGeneratorFactory: Error getting inputs for internal combustion engine generator named: " +
                           objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void GetICEngineGeneratorInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    Sept. 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the IC ENGINE Generator models.

        int genNum;                   // Generator counter
        int NumAlphas;                // Number of elements in the alpha array
        int NumNums;                  // Number of elements in the numeric array
        int IOStat;                   // IO Status when calling get input subroutine
        Array1D_string AlphArray(10); // character string data
        Array1D<Real64> NumArray(11); // numeric data
        bool ErrorsFound(false);      // error flag

        auto &ICEngineGenerator(state.dataICEngElectGen->ICEngineGenerator);

        state.dataIPShortCut->cCurrentModuleObject = "Generator:InternalCombustionEngine";
        state.dataICEngElectGen->NumICEngineGenerators =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataICEngElectGen->NumICEngineGenerators <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        ICEngineGenerator.allocate(state.dataICEngElectGen->NumICEngineGenerators);

        // LOAD ARRAYS WITH IC ENGINE Generator CURVE FIT  DATA
        for (genNum = 1; genNum <= state.dataICEngElectGen->NumICEngineGenerators; ++genNum) {
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

            state.dataICEngElectGen->ICEngineGenerator(genNum).Name = AlphArray(1);

            state.dataICEngElectGen->ICEngineGenerator(genNum).RatedPowerOutput = NumArray(1);
            if (NumArray(1) == 0.0) {
                ShowSevereError(state, format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), NumArray(1)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            // Not sure what to do with electric nodes, so do not use optional arguments
            state.dataICEngElectGen->ICEngineGenerator(genNum).ElectricCircuitNode =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(2),
                                                    ErrorsFound,
                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Electric,
                                                    DataLoopNode::NodeConnectionType::Electric,
                                                    NodeInputManager::compFluidStream::Primary,
                                                    DataLoopNode::ObjectIsNotParent);

            state.dataICEngElectGen->ICEngineGenerator(genNum).MinPartLoadRat = NumArray(2);
            state.dataICEngElectGen->ICEngineGenerator(genNum).MaxPartLoadRat = NumArray(3);
            state.dataICEngElectGen->ICEngineGenerator(genNum).OptPartLoadRat = NumArray(4);

            // Load Special IC ENGINE Generator Curve Fit Inputs
            state.dataICEngElectGen->ICEngineGenerator(genNum).ElecOutputFuelCurve =
                CurveManager::GetCurveIndex(state, AlphArray(3)); // convert curve name to number
            if (state.dataICEngElectGen->ICEngineGenerator(genNum).ElecOutputFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + '=' + AlphArray(3));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataICEngElectGen->ICEngineGenerator(genNum).RecJacHeattoFuelCurve =
                CurveManager::GetCurveIndex(state, AlphArray(4)); // convert curve name to number
            if (state.dataICEngElectGen->ICEngineGenerator(genNum).RecJacHeattoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + '=' + AlphArray(4));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataICEngElectGen->ICEngineGenerator(genNum).RecLubeHeattoFuelCurve =
                CurveManager::GetCurveIndex(state, AlphArray(5)); // convert curve name to number
            if (state.dataICEngElectGen->ICEngineGenerator(genNum).RecLubeHeattoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + '=' + AlphArray(5));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataICEngElectGen->ICEngineGenerator(genNum).TotExhausttoFuelCurve =
                CurveManager::GetCurveIndex(state, AlphArray(6)); // convert curve name to number
            if (state.dataICEngElectGen->ICEngineGenerator(genNum).TotExhausttoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + AlphArray(6));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataICEngElectGen->ICEngineGenerator(genNum).ExhaustTempCurve =
                CurveManager::GetCurveIndex(state, AlphArray(7)); // convert curve name to number
            if (state.dataICEngElectGen->ICEngineGenerator(genNum).ExhaustTempCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphArray(7));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            } else {
                Real64 xValue = CurveManager::CurveValue(state, state.dataICEngElectGen->ICEngineGenerator(genNum).ExhaustTempCurve, 1.0);
                if (xValue < ReferenceTemp) {
                    ShowSevereError(state, "GetICEngineGeneratorInput: " + state.dataIPShortCut->cAlphaFieldNames(7) + " output has very low value.");
                    ShowContinueError(state, format("...curve generates [{:.3R} C] at PLR=1.0", xValue));
                    ShowContinueError(state,
                                      format("...this is less than the Reference Temperature [{:.2R} C] and may cause errors.", ReferenceTemp));
                }
            }

            state.dataICEngElectGen->ICEngineGenerator(genNum).UACoef(1) = NumArray(5);
            state.dataICEngElectGen->ICEngineGenerator(genNum).UACoef(2) = NumArray(6);

            state.dataICEngElectGen->ICEngineGenerator(genNum).MaxExhaustperPowerOutput = NumArray(7);
            state.dataICEngElectGen->ICEngineGenerator(genNum).DesignMinExitGasTemp = NumArray(8);
            state.dataICEngElectGen->ICEngineGenerator(genNum).FuelHeatingValue = NumArray(9);
            state.dataICEngElectGen->ICEngineGenerator(genNum).DesignHeatRecVolFlowRate = NumArray(10);
            if (state.dataICEngElectGen->ICEngineGenerator(genNum).DesignHeatRecVolFlowRate > 0.0) {
                state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecActive = true;
                state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(8),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        AlphArray(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                        NodeInputManager::compFluidStream::Primary,
                                                        DataLoopNode::ObjectIsNotParent);
                if (state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecInletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphArray(8));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(9),
                                                        ErrorsFound,
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        AlphArray(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                        NodeInputManager::compFluidStream::Primary,
                                                        DataLoopNode::ObjectIsNotParent);
                if (state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecOutletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + AlphArray(9));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(
                    state, state.dataIPShortCut->cCurrentModuleObject, AlphArray(1), AlphArray(8), AlphArray(9), "Heat Recovery Nodes");
                PlantUtilities::RegisterPlantCompDesignFlow(state,
                                                            state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecInletNodeNum,
                                                            state.dataICEngElectGen->ICEngineGenerator(genNum).DesignHeatRecVolFlowRate);
            } else {
                state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecActive = false;
                state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecInletNodeNum = 0;
                state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecOutletNodeNum = 0;
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8) || !state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    ShowWarningError(state,
                                     "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + state.dataIPShortCut->cCurrentModuleObject +
                                         '=' + AlphArray(1));
                    ShowContinueError(state, "However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }

            // Validate fuel type input
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelType(state, AlphArray(10), state.dataICEngElectGen->ICEngineGenerator(genNum).FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + AlphArray(10));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
                FuelTypeError = false;
            }

            state.dataICEngElectGen->ICEngineGenerator(genNum).HeatRecMaxTemp = NumArray(11);
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void ICEngineGeneratorSpecs::setupOutputVars(EnergyPlusData &state)
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

        //    general fuel use report to match other generators.
        SetupOutputVariable(
            state, "Generator Fuel HHV Basis Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRate, "System", "Average", this->Name);

        SetupOutputVariable(state, "Generator Fuel HHV Basis Energy", OutputProcessor::Unit::J, this->FuelEnergy, "System", "Sum", this->Name);

        SetupOutputVariable(
            state, "Generator " + this->FuelType + " Mass Flow Rate", OutputProcessor::Unit::kg_s, this->FuelMdot, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Generator Exhaust Air Temperature", OutputProcessor::Unit::C, this->ExhaustStackTemp, "System", "Average", this->Name);

        if (this->HeatRecActive) {
            SetupOutputVariable(state,
                                "Generator Heat Recovery Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->HeatRecMdotActual,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                state, "Generator Jacket Heat Recovery Rate", OutputProcessor::Unit::W, this->QJacketRecovered, "System", "Average", this->Name);

            SetupOutputVariable(state,
                                "Generator Jacket Heat Recovery Energy",
                                OutputProcessor::Unit::J,
                                this->JacketEnergyRec,
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
        }
    }

    void ICEngineGeneratorSpecs::getDesignCapacities(
        [[maybe_unused]] EnergyPlusData &state, const EnergyPlus::PlantLocation &, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MaxLoad = 0.0;
        MinLoad = 0.0;
        OptLoad = 0.0;
    }

    void ICEngineGeneratorSpecs::simulate(EnergyPlusData &state,
                                          [[maybe_unused]] const EnergyPlus::PlantLocation &calledFromLocation,
                                          bool FirstHVACIteration,
                                          [[maybe_unused]] Real64 &CurLoad,
                                          [[maybe_unused]] bool RunFlag)
    {
        // empty function to emulate current behavior as of conversion to using the PlantComponent calling structure.
        // calls from the plant side only update the plant nodes.
        // calls from the ElectricPowerServiceManger call the init, calc, and update worker functions directly.

        PlantUtilities::UpdateComponentHeatRecoverySide(state,
                                                        this->HRLoopNum,
                                                        this->HRLoopSideNum,
                                                        DataPlant::TypeOf_Generator_ICEngine,
                                                        this->HeatRecInletNodeNum,
                                                        this->HeatRecOutletNodeNum,
                                                        this->QTotalHeatRecovered,
                                                        this->HeatRecInletTemp,
                                                        this->HeatRecOutletTemp,
                                                        this->HeatRecMdotActual,
                                                        FirstHVACIteration);
    }

    void ICEngineGeneratorSpecs::CalcICEngineGeneratorModel(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 2000
        //       MODIFIED     na
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a IC ENGINE generator using the BLAST model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data:

        Real64 const ExhaustCP(1.047); // Exhaust Gas Specific Heat (J/kg-K)
        Real64 const KJtoJ(1000.0);    // convert Kjoules to joules
        static std::string const RoutineName("CalcICEngineGeneratorModel");

        // Heat Recovery Fluid Mass FlowRate (kg/s)
        Real64 HeatRecMdot;

        // Heat Recovery Fluid Inlet Temperature (C)
        Real64 HeatRecInTemp;

        if (this->HeatRecActive) {
            int HeatRecInNode = this->HeatRecInletNodeNum;
            HeatRecInTemp = state.dataLoopNodes->Node(HeatRecInNode).Temp;
            HeatRecMdot = state.dataLoopNodes->Node(HeatRecInNode).MassFlowRate;

        } else {
            HeatRecInTemp = 0.0;
            HeatRecMdot = 0.0;
        }

        // If no loop demand or Generator OFF, return
        if (!RunFlag) {
            this->ElecPowerGenerated = 0.0;
            this->ElecEnergyGenerated = 0.0;
            this->HeatRecInletTemp = HeatRecInTemp;
            this->HeatRecOutletTemp = HeatRecInTemp;
            this->HeatRecMdotActual = 0.0;
            this->QJacketRecovered = 0.0;
            this->QExhaustRecovered = 0.0;
            this->QLubeOilRecovered = 0.0;
            this->QTotalHeatRecovered = 0.0;
            this->JacketEnergyRec = 0.0;
            this->ExhaustEnergyRec = 0.0;
            this->LubeOilEnergyRec = 0.0;
            this->TotalHeatEnergyRec = 0.0;
            this->FuelEnergyUseRate = 0.0;
            this->FuelEnergy = 0.0;
            this->FuelMdot = 0.0;
            this->ExhaustStackTemp = 0.0;

            return;
        }

        // Generator output (W)
        Real64 elecPowerGenerated = min(MyLoad, this->RatedPowerOutput);
        elecPowerGenerated = max(elecPowerGenerated, 0.0);

        // Generator operating part load ratio
        Real64 PLR = min(elecPowerGenerated / this->RatedPowerOutput, this->MaxPartLoadRat);
        PLR = max(PLR, this->MinPartLoadRat);
        elecPowerGenerated = PLR * this->RatedPowerOutput;

        // DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

        // Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
        // energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
        Real64 fuelEnergyUseRate; // IC ENGINE fuel use rate (W)
        if (PLR > 0.0) {
            // (RELDC) Ratio of generator output to Fuel Energy Input
            Real64 elecOutputFuelRat = CurveManager::CurveValue(state, this->ElecOutputFuelCurve, PLR);
            fuelEnergyUseRate = elecPowerGenerated / elecOutputFuelRat;
        } else {
            fuelEnergyUseRate = 0.0;
        }

        // Use Curve fit to determine heat recovered in the water jacket.  This curve calculates the water jacket heat recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
        // particular part load.

        // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
        Real64 recJacHeattoFuelRat = CurveManager::CurveValue(state, this->RecJacHeattoFuelCurve, PLR);

        // water jacket heat recovered (W)
        Real64 QJacketRec = fuelEnergyUseRate * recJacHeattoFuelRat;

        // Use Curve fit to determine Heat Recovered Lubricant heat.  This curve calculates the lube heat recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
        // particular part load.
        // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
        Real64 recLubeHeattoFuelRat = CurveManager::CurveValue(state, this->RecLubeHeattoFuelCurve, PLR);

        // lube oil cooler heat recovered (W)
        Real64 QLubeOilRec = fuelEnergyUseRate * recLubeHeattoFuelRat;

        // Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  heat recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
        // particular part load.

        // (REXDC) Total Exhaust Energy Input to Fuel Energy Input
        Real64 totExhausttoFuelRat = CurveManager::CurveValue(state, this->TotExhausttoFuelCurve, PLR);

        // total engine exhaust heat (W)
        Real64 QExhaustTotal = fuelEnergyUseRate * totExhausttoFuelRat;

        // exhaust gas heat recovered (W)
        Real64 QExhaustRec;

        // engine stack temp. (C)
        Real64 exhaustStackTemp = 0.0;

        // Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
        // of the exhaust temperature in C to the part load ratio.
        if (PLR > 0.0) {
            // (TEX) Exhaust Gas Temp
            Real64 exhaustTemp = CurveManager::CurveValue(state, this->ExhaustTempCurve, PLR);

            if (exhaustTemp > ReferenceTemp) {

                // exhaust gas mass flow rate (kg/s)
                Real64 ExhaustGasFlow = QExhaustTotal / (ExhaustCP * (exhaustTemp - ReferenceTemp));

                // Use Curve fit to determine stack exhaustTemp after heat recovery
                // (UACDC) exhaust gas Heat Exchanger UA
                Real64 UA_loc = this->UACoef(1) * std::pow(this->RatedPowerOutput, this->UACoef(2));

                // design engine stact saturated steam exhaustTemp. (C)
                Real64 designMinExitGasTemp = this->DesignMinExitGasTemp;

                exhaustStackTemp = designMinExitGasTemp +
                                   (exhaustTemp - designMinExitGasTemp) /
                                       std::exp(UA_loc / (max(ExhaustGasFlow, this->MaxExhaustperPowerOutput * this->RatedPowerOutput) * ExhaustCP));

                QExhaustRec = max(ExhaustGasFlow * ExhaustCP * (exhaustTemp - exhaustStackTemp), 0.0);
            } else {
                if (this->ErrExhaustTempIndex == 0) {
                    ShowWarningMessage(
                        state, "CalcICEngineGeneratorModel: " + this->TypeOf + "=\"" + this->Name + "\" low Exhaust Temperature from Curve Value");
                    ShowContinueError(state, format("...curve generated temperature=[{:.3R} C], PLR=[{:.3R}].", exhaustTemp, PLR));
                    ShowContinueError(state, "...simulation will continue with exhaust heat reclaim set to 0.");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "CalcICEngineGeneratorModel: " + this->TypeOf + "=\"" + this->Name +
                                                   "\" low Exhaust Temperature continues...",
                                               this->ErrExhaustTempIndex,
                                               exhaustTemp,
                                               exhaustTemp,
                                               _,
                                               "[C]",
                                               "[C]");
                QExhaustRec = 0.0;
                exhaustStackTemp = this->DesignMinExitGasTemp;
            }
        } else {
            QExhaustRec = 0.0;
            // Bug exhaustStackTemp not set but used below
        }

        Real64 qTotalHeatRecovered = QExhaustRec + QLubeOilRec + QJacketRec;

        // When Max Temp is reached the amount of recovered heat has to be reduced.
        Real64 HRecRatio;

        if (this->HeatRecActive) {
            this->CalcICEngineGenHeatRecovery(state, qTotalHeatRecovered, HeatRecMdot, HRecRatio);
            QExhaustRec *= HRecRatio;
            QLubeOilRec *= HRecRatio;
            QJacketRec *= HRecRatio;
            qTotalHeatRecovered *= HRecRatio;
        } else {
            this->HeatRecInletTemp = HeatRecInTemp;
            this->HeatRecOutletTemp = HeatRecInTemp;
            this->HeatRecMdotActual = HeatRecMdot;
        }

        // Calculate Energy
        // Generator output (J)
        Real64 ElectricEnergyGen = elecPowerGenerated * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // IC ENGINE fuel use (J)
        Real64 FuelEnergyUsed = fuelEnergyUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // water jacket heat recovered (J)
        Real64 jacketEnergyRec = QJacketRec * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // lube oil cooler heat recovered (J)
        Real64 lubeOilEnergyRec = QLubeOilRec * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // exhaust gas heat recovered (J)
        Real64 exhaustEnergyRec = QExhaustRec * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->ElecPowerGenerated = elecPowerGenerated;
        this->ElecEnergyGenerated = ElectricEnergyGen;
        this->QJacketRecovered = QJacketRec;
        this->QLubeOilRecovered = QLubeOilRec;
        this->QExhaustRecovered = QExhaustRec;
        this->QTotalHeatRecovered = qTotalHeatRecovered;
        this->JacketEnergyRec = jacketEnergyRec;
        this->LubeOilEnergyRec = lubeOilEnergyRec;
        this->ExhaustEnergyRec = exhaustEnergyRec;
        this->QTotalHeatRecovered = (QExhaustRec + QLubeOilRec + QJacketRec);
        this->TotalHeatEnergyRec = (exhaustEnergyRec + lubeOilEnergyRec + jacketEnergyRec);
        this->FuelEnergyUseRate = std::abs(fuelEnergyUseRate);
        this->FuelEnergy = std::abs(FuelEnergyUsed);

        // Heating Value of Fuel in kJ/kg
        Real64 fuelHeatingValue = this->FuelHeatingValue;

        this->FuelMdot = std::abs(fuelEnergyUseRate) / (fuelHeatingValue * KJtoJ);
        this->ExhaustStackTemp = exhaustStackTemp;
    }

    void ICEngineGeneratorSpecs::CalcICEngineGenHeatRecovery(EnergyPlusData &state,
                                                             Real64 const EnergyRecovered,
                                                             Real64 const HeatRecMdot,
                                                             Real64 &HRecRatio)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Brandon Anderson
        //       DATE WRITTEN:    November 2000

        // PURPOSE OF THIS SUBROUTINE:
        // To perform heat recovery calculations and node updates

        // METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
        // It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
        // The chiller sets the flow on the loop first by the input design flow rate and then
        // performs a check to verify that

        static std::string const RoutineName("CalcICEngineGeneratorModel");

        // Need to set the HeatRecRatio to 1.0 if it is not modified
        HRecRatio = 1.0;

        Real64 HeatRecInTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
        Real64 HeatRecCp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                  state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                                  HeatRecInTemp,
                                                                  state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                                  RoutineName);

        // Don't divide by zero - Note This also results in no heat recovery when
        //  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
        //  In order to see what minimum heat recovery flow rate is for the design temperature
        //  The design heat recovery flow rate can be set very small, but greater than zero.

        Real64 HeatRecOutTemp;

        if ((HeatRecMdot > 0) && (HeatRecCp > 0)) {
            HeatRecOutTemp = (EnergyRecovered) / (HeatRecMdot * HeatRecCp) + HeatRecInTemp;
        } else {
            HeatRecOutTemp = HeatRecInTemp;
        }

        // Note: check to make sure the Max Temperature was not exceeded
        if (HeatRecOutTemp > this->HeatRecMaxTemp) {
            Real64 MinHeatRecMdot;
            if (this->HeatRecMaxTemp != HeatRecInTemp) {
                MinHeatRecMdot = (EnergyRecovered) / (HeatRecCp * (this->HeatRecMaxTemp - HeatRecInTemp));
                if (MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0;
            } else {
                MinHeatRecMdot = 0.0;
            }

            // Recalculate Outlet Temperature, with adjusted flowrate
            if ((MinHeatRecMdot > 0.0) && (HeatRecCp > 0.0)) {
                HeatRecOutTemp = (EnergyRecovered) / (MinHeatRecMdot * HeatRecCp) + HeatRecInTemp;
                HRecRatio = HeatRecMdot / MinHeatRecMdot;
            } else {
                HeatRecOutTemp = HeatRecInTemp;
                HRecRatio = 0.0;
            }
        }

        // Update global variables for reporting later
        this->HeatRecInletTemp = HeatRecInTemp;
        this->HeatRecOutletTemp = HeatRecOutTemp;
        this->HeatRecMdotActual = HeatRecMdot;
    }

    void ICEngineGeneratorSpecs::InitICEngineGenerators(EnergyPlusData &state, bool const RunFlag, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  Brent Griffith, Sept 2010, plant upgrades, generalize fluid props

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the IC ENGINE generators.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        static std::string const RoutineName("InitICEngineGenerators");

        bool errFlag;

        if (this->myFlag) {
            this->setupOutputVars(state);
            this->myFlag = false;
        }

        if (this->MyPlantScanFlag && allocated(state.dataPlnt->PlantLoop) && this->HeatRecActive) {
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    DataPlant::TypeOf_Generator_ICEngine,
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
                ShowFatalError(state, "InitICEngineGenerators: Program terminated due to previous condition(s).");
            }

            this->MyPlantScanFlag = false;
        }

        if (this->MySizeAndNodeInitFlag && (!this->MyPlantScanFlag) && this->HeatRecActive) {

            // size mass flow rate
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                           RoutineName);

            this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;
            this->HeatRecMdotDesign = this->DesignHeatRecMassFlowRate;

            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->DesignHeatRecMassFlowRate,
                                               this->HeatRecInletNodeNum,
                                               this->HeatRecOutletNodeNum,
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
                                                     this->HeatRecMdotActual,
                                                     this->HeatRecInletNodeNum,
                                                     this->HeatRecOutletNodeNum,
                                                     this->HRLoopNum,
                                                     this->HRLoopSideNum,
                                                     this->HRBranchNum,
                                                     this->HRCompNum);
            }
        }
    }

    void ICEngineGeneratorSpecs::update(EnergyPlusData &state)
    {
        if (this->HeatRecActive) {
            int HeatRecOutletNode = this->HeatRecOutletNodeNum;
            state.dataLoopNodes->Node(HeatRecOutletNode).Temp = this->HeatRecOutletTemp;
        }
    }

} // namespace ICEngineElectricGenerator

} // namespace EnergyPlus
