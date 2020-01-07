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
#include <EnergyPlus/CTElectricGenerator.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
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

    int NumCTGenerators(0);    // number of CT Generators specified in input
    bool getCTInputFlag(true); // then TRUE, calls subroutine to read input file.

    Array1D<CTGeneratorData> CTGenerator; // dimension to number of machines

    void clear_state()
    {
        NumCTGenerators = 0;
        getCTInputFlag = true;
        CTGenerator.deallocate();
    }

    PlantComponent *CTGeneratorData::factory(std::string const &objectName)
    {
        // Process the input data for generators if it hasn't been done already
        if (getCTInputFlag) {
            GetCTGeneratorInput();
            getCTInputFlag = false;
        }

        // Now look for this particular generator in the list
        for (auto &CTGen : CTGenerator) {
            if (CTGen.Name == objectName) {
                return &CTGen;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalCombustionTurbineGeneratorFactory: Error getting inputs for combustion turbine generator named: " +
                       objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void CTGeneratorData::simulate(const EnergyPlus::PlantLocation &EP_UNUSED(calledFromLocation),
                                   bool EP_UNUSED(FirstHVACIteration),
                                   Real64 &EP_UNUSED(CurLoad),
                                   bool EP_UNUSED(RunFlag))
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

    void GetCTGeneratorInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the CT Generator models.

        int NumAlphas;                  // Number of elements in the alpha array
        int NumNums;                    // Number of elements in the numeric array
        int IOStat;                     // IO Status when calling get input subroutine
        Array1D_string AlphArray(12);   // character string data
        Array1D<Real64> NumArray(12);   // numeric data
        static bool ErrorsFound(false); // error flag

        DataIPShortCuts::cCurrentModuleObject = "Generator:CombustionTurbine";
        NumCTGenerators = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumCTGenerators <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        CTGenerator.allocate(NumCTGenerators);

        // LOAD ARRAYS WITH CT CURVE FIT Generator DATA
        for (int genNum = 1; genNum <= NumCTGenerators; ++genNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          genNum,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          _,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            CTGenerator(genNum).Name = AlphArray(1);

            CTGenerator(genNum).RatedPowerOutput = NumArray(1);
            if (NumArray(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(NumArray(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            // Not sure what to do with electric nodes, so do not use optional arguments
            CTGenerator(genNum).ElectricCircuitNode = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                          ErrorsFound,
                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                          AlphArray(1),
                                                                                          DataLoopNode::NodeType_Electric,
                                                                                          DataLoopNode::NodeConnectionType_Electric,
                                                                                          1,
                                                                                          DataLoopNode::ObjectIsNotParent);

            CTGenerator(genNum).MinPartLoadRat = NumArray(2);
            CTGenerator(genNum).MaxPartLoadRat = NumArray(3);
            CTGenerator(genNum).OptPartLoadRat = NumArray(4);

            // Load Special CT Generator Input

            CTGenerator(genNum).PLBasedFuelInputCurve = CurveManager::GetCurveIndex(AlphArray(3)); // convert curve name to number
            if (CTGenerator(genNum).PLBasedFuelInputCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + AlphArray(3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            CTGenerator(genNum).TempBasedFuelInputCurve = CurveManager::GetCurveIndex(AlphArray(4)); // convert curve name to number
            if (CTGenerator(genNum).TempBasedFuelInputCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + '=' + AlphArray(4));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            CTGenerator(genNum).ExhaustFlowCurve = CurveManager::GetCurveIndex(AlphArray(5)); // convert curve name to number
            if (CTGenerator(genNum).ExhaustFlowCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + '=' + AlphArray(5));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            CTGenerator(genNum).PLBasedExhaustTempCurve = CurveManager::GetCurveIndex(AlphArray(6)); // convert curve name to number
            if (CTGenerator(genNum).PLBasedExhaustTempCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + AlphArray(6));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            CTGenerator(genNum).TempBasedExhaustTempCurve = CurveManager::GetCurveIndex(AlphArray(7)); // convert curve name to number
            if (CTGenerator(genNum).TempBasedExhaustTempCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + AlphArray(7));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            CTGenerator(genNum).QLubeOilRecoveredCurve = CurveManager::GetCurveIndex(AlphArray(8)); // convert curve name to number
            if (CTGenerator(genNum).QLubeOilRecoveredCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + AlphArray(8));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            CTGenerator(genNum).UACoef(1) = NumArray(5);
            CTGenerator(genNum).UACoef(2) = NumArray(6);

            CTGenerator(genNum).MaxExhaustperCTPower = NumArray(7);
            CTGenerator(genNum).DesignMinExitGasTemp = NumArray(8);
            CTGenerator(genNum).DesignAirInletTemp = NumArray(9);
            CTGenerator(genNum).FuelHeatingValue = NumArray(10);
            CTGenerator(genNum).DesignHeatRecVolFlowRate = NumArray(11);

            if (CTGenerator(genNum).DesignHeatRecVolFlowRate > 0.0) {
                CTGenerator(genNum).HeatRecActive = true;
                CTGenerator(genNum).HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(9),
                                                                                              ErrorsFound,
                                                                                              DataIPShortCuts::cCurrentModuleObject,
                                                                                              AlphArray(1),
                                                                                              DataLoopNode::NodeType_Water,
                                                                                              DataLoopNode::NodeConnectionType_Inlet,
                                                                                              1,
                                                                                              DataLoopNode::ObjectIsNotParent);
                if (CTGenerator(genNum).HeatRecInletNodeNum == 0) {
                    ShowSevereError("Missing Node Name, Heat Recovery Inlet, for " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                CTGenerator(genNum).HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(10),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               AlphArray(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Outlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);
                if (CTGenerator(genNum).HeatRecOutletNodeNum == 0) {
                    ShowSevereError("Missing Node Name, Heat Recovery Outlet, for " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(
                    DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(9), AlphArray(10), "Heat Recovery Nodes");
                PlantUtilities::RegisterPlantCompDesignFlow(CTGenerator(genNum).HeatRecInletNodeNum, CTGenerator(genNum).DesignHeatRecVolFlowRate);
            } else {
                CTGenerator(genNum).HeatRecActive = false;
                CTGenerator(genNum).HeatRecInletNodeNum = 0;
                CTGenerator(genNum).HeatRecOutletNodeNum = 0;
                if (!DataIPShortCuts::lAlphaFieldBlanks(9) || !DataIPShortCuts::lAlphaFieldBlanks(10)) {
                    ShowWarningError("Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + DataIPShortCuts::cCurrentModuleObject + '=' +
                                     AlphArray(1));
                    ShowContinueError("However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }

            // Fuel Type Case Statement
            {
                auto const SELECT_CASE_var(AlphArray(11));
                if (is_blank(SELECT_CASE_var)) { // If blank then the default is Natural Gas
                    CTGenerator(genNum).FuelType = "Gas";

                } else if (SELECT_CASE_var == "NATURALGAS") {
                    CTGenerator(genNum).FuelType = "Gas";

                } else if (SELECT_CASE_var == "DIESEL") {
                    CTGenerator(genNum).FuelType = "Diesel";

                } else if (SELECT_CASE_var == "GASOLINE") {
                    CTGenerator(genNum).FuelType = "Gasoline";

                } else if (SELECT_CASE_var == "FUELOILNO1") {
                    CTGenerator(genNum).FuelType = "FuelOil#1";

                } else if (SELECT_CASE_var == "FUELOILNO2") {
                    CTGenerator(genNum).FuelType = "FuelOil#2";

                } else if (SELECT_CASE_var == "PROPANE") {
                    CTGenerator(genNum).FuelType = "Propane";

                } else if (SELECT_CASE_var == "OTHERFUEL1") {
                    CTGenerator(genNum).FuelType = "OtherFuel1";

                } else if (SELECT_CASE_var == "OTHERFUEL2") {
                    CTGenerator(genNum).FuelType = "OtherFuel2";

                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(11) + '=' + AlphArray(11));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            CTGenerator(genNum).HeatRecMaxTemp = NumArray(12);

            // begin CR7021
            if (DataIPShortCuts::lAlphaFieldBlanks(12)) {
                CTGenerator(genNum).OAInletNode = 0;
            } else {
                CTGenerator(genNum).OAInletNode = NodeInputManager::GetOnlySingleNode(AlphArray(12),
                                                                                      ErrorsFound,
                                                                                      DataIPShortCuts::cCurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      DataLoopNode::NodeType_Air,
                                                                                      DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                      1,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(CTGenerator(genNum).OAInletNode)) {
                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", \"" + CTGenerator(genNum).Name +
                                    "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(12));
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void CTGeneratorData::setupOutputVars()
    {
        SetupOutputVariable("Generator Produced Electric Power", OutputProcessor::Unit::W, this->ElecPowerGenerated, "System", "Average", this->Name);

        SetupOutputVariable("Generator Produced Electric Energy",
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
            "Generator " + this->FuelType + " Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRate, "System", "Average", this->Name);

        SetupOutputVariable("Generator " + this->FuelType + " Energy",
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
        SetupOutputVariable("Generator Fuel HHV Basis Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRate, "System", "Average", this->Name);

        SetupOutputVariable("Generator Fuel HHV Basis Energy", OutputProcessor::Unit::J, this->FuelEnergy, "System", "Sum", this->Name);

        SetupOutputVariable(
            "Generator " + this->FuelType + " Mass Flow Rate", OutputProcessor::Unit::kg_s, this->FuelMdot, "System", "Average", this->Name);

        SetupOutputVariable("Generator Exhaust Air Temperature", OutputProcessor::Unit::C, this->ExhaustStackTemp, "System", "Average", this->Name);

        if (this->HeatRecActive) {
            SetupOutputVariable(
                "Generator Exhaust Heat Recovery Rate", OutputProcessor::Unit::W, this->QExhaustRecovered, "System", "Average", this->Name);

            SetupOutputVariable("Generator Exhaust Heat Recovery Energy",
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
                "Generator Lube Heat Recovery Rate", OutputProcessor::Unit::W, this->QLubeOilRecovered, "System", "Average", this->Name);

            SetupOutputVariable("Generator Lube Heat Recovery Energy",
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
                "Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->QTotalHeatRecovered, "System", "Average", this->Name);

            SetupOutputVariable("Generator Produced Thermal Energy", OutputProcessor::Unit::J, this->TotalHeatEnergyRec, "System", "Sum", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMdot, "System", "Average", this->Name);
        }
    }

    void CTGeneratorData::CalcCTGeneratorModel(bool const RunFlag,  // TRUE when Generator operating
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
            heatRecInTemp = DataLoopNode::Node(heatRecInNode).Temp;

            heatRecCp = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->HRLoopNum).FluidName, heatRecInTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, RoutineName);
            if (FirstHVACIteration && RunFlag) {
                heatRecMdot = this->DesignHeatRecMassFlowRate;
            } else {
                heatRecMdot = DataLoopNode::Node(heatRecInNode).MassFlowRate;
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
            ambientDeltaT = DataEnvironment::OutDryBulbTemp - designAirInletTemp;
        } else {
            ambientDeltaT = DataLoopNode::Node(this->OAInletNode).Temp - designAirInletTemp;
        }

        // Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
        // energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
        // The TempBasedFuelInputCurve is a correction based on deviation from design inlet air temperature conditions.
        // The first coefficient of this fit should be 1.0 to ensure that no correction is made at design conditions.
        // (EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
        Real64 FuelUseRate = elecPowerGenerated * CurveManager::CurveValue(this->PLBasedFuelInputCurve, PLR) *
                             CurveManager::CurveValue(this->TempBasedFuelInputCurve, ambientDeltaT);

        // Use Curve fit to determine Exhaust Flow.  This curve shows the ratio of exhaust gas flow (kg/s) to electric power
        // output (J/s).  The units on ExhaustFlowCurve are (kg/J).  When multiplied by the rated power of the unit,
        // it gives the exhaust flow rate in kg/s
        // (FEX) Exhaust Gas Flow Rate cubic meters per second???
        Real64 exhaustFlow = ratedPowerOutput * CurveManager::CurveValue(this->ExhaustFlowCurve, ambientDeltaT);

        // Use Curve fit to determine Exhaust Temperature.  This curve calculates the exhaust temperature (C) by
        // multiplying the exhaust temperature (C) for a particular part load as given by PLBasedExhaustTempCurve
        // a correction factor based on the deviation from design temperature, TempBasedExhaustTempCurve

        Real64 QExhaustRec;      // recovered exhaust heat (W)
        Real64 exhaustStackTemp; // turbine stack temp. (C)
        if ((PLR > 0.0) && ((exhaustFlow > 0.0) || (maxExhaustperCTPower > 0.0))) {

            // (TEX) Exhaust Gas Temperature in C
            Real64 exhaustTemp = CurveManager::CurveValue(this->PLBasedExhaustTempCurve, PLR) *
                                 CurveManager::CurveValue(this->TempBasedExhaustTempCurve, ambientDeltaT);

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
        Real64 QLubeOilRec = elecPowerGenerated * CurveManager::CurveValue(this->QLubeOilRecoveredCurve, PLR);

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
        Real64 ElectricEnergyGen = elecPowerGenerated * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // Amount of Fuel Energy Required to run COMBUSTION turbine (J)
        Real64 FuelEnergyUsed = FuelUseRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // recovered lube oil heat (J)
        Real64 lubeOilEnergyRec = QLubeOilRec * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // recovered exhaust heat (J)
        Real64 exhaustEnergyRec = QExhaustRec * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

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
            DataLoopNode::Node(HeatRecOutletNode).Temp = this->HeatRecOutletTemp;
        }
    }

    void CTGeneratorData::InitCTGenerators(bool const RunFlag, // TRUE when Generator operating
                                           bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Oct 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  Brent Griffith, Sept 2010 plant upgrades, generalize fluid props

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the CT generators.

        static std::string const RoutineName("InitICEngineGenerators");

        bool errFlag;

        if (this->MyPlantScanFlag && allocated(DataPlant::PlantLoop) && this->HeatRecActive) {
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
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
                ShowFatalError("InitCTGenerators: Program terminated due to previous condition(s).");
            }

            this->MyPlantScanFlag = false;
        }

        if (this->MyFlag) {
            this->setupOutputVars();
            this->MyFlag = false;
        }

        if (this->MySizeAndNodeInitFlag && (!this->MyPlantScanFlag) && this->HeatRecActive) {
            int HeatRecInletNode = this->HeatRecInletNodeNum;
            int HeatRecOutletNode = this->HeatRecOutletNodeNum;

            // size mass flow rate
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                           DataGlobals::InitConvTemp,
                                                           DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
                                                           RoutineName);

            this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

            PlantUtilities::InitComponentNodes(0.0,
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
        if (DataGlobals::BeginEnvrnFlag && this->MyEnvrnFlag && this->HeatRecActive) {
            int HeatRecInletNode = this->HeatRecInletNodeNum;
            int HeatRecOutletNode = this->HeatRecOutletNodeNum;
            // set the node Temperature, assuming freeze control
            DataLoopNode::Node(HeatRecInletNode).Temp = 20.0;
            DataLoopNode::Node(HeatRecOutletNode).Temp = 20.0;
            // set the node max and min mass flow rates
            PlantUtilities::InitComponentNodes(0.0,
                                               this->DesignHeatRecMassFlowRate,
                                               HeatRecInletNode,
                                               HeatRecOutletNode,
                                               this->HRLoopNum,
                                               this->HRLoopSideNum,
                                               this->HRBranchNum,
                                               this->HRCompNum);

            this->MyEnvrnFlag = false;
        } // end environmental inits

        if (!DataGlobals::BeginEnvrnFlag) {
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
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->HeatRecInletNodeNum,
                                                     this->HeatRecOutletNodeNum,
                                                     this->HRLoopNum,
                                                     this->HRLoopSideNum,
                                                     this->HRBranchNum,
                                                     this->HRCompNum);

            } else {
                PlantUtilities::SetComponentFlowRate(this->HeatRecMdot,
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
