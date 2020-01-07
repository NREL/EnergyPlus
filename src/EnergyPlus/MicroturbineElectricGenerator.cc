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
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MicroturbineElectricGenerator.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace MicroturbineElectricGenerator {

    // MODULE INFORMATION:
    //       AUTHOR         R. Raustad/D. Shirey
    //       DATE WRITTEN   Mar 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    //  This module simulates the performance of microturbine electric
    //  generators.

    // METHODOLOGY EMPLOYED:
    //  Once the electric power manager determines that the MT Generator
    //  is available, it calls SimMTGenerator which in turn calls the
    //  appropriate microturbine generator model.
    //  MT Generator models are based on polynomial curve fits of generator
    //  performance data.

    int NumMTGenerators(0); // number of MT Generators specified in input
    bool GetMTInput(true);  // then TRUE, calls subroutine to read input file.

    // Object Data
    Array1D<MTGeneratorSpecs> MTGenerator; // dimension to number of generators

    PlantComponent *MTGeneratorSpecs::factory(std::string const &objectName)
    {
        // Process the input data for generator if it hasn't been done already
        if (GetMTInput) {
            GetMTGeneratorInput();
            GetMTInput = false;
        }

        // Now look for this particular gen in the list
        for (auto &thisMTG : MTGenerator) {
            if (thisMTG.Name == objectName) {
                return &thisMTG;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalMicroTurbineGeneratorFactory: Error getting inputs for microturbine generator named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void GetMTGeneratorInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad/D. Shirey
        //       DATE WRITTEN   Mar 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine gets the input information for the Microturbine (MT) Generator model.

        bool ErrorsFound(false);

        DataIPShortCuts::cCurrentModuleObject = "Generator:MicroTurbine";
        NumMTGenerators = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumMTGenerators <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        MTGenerator.allocate(NumMTGenerators);

        // LOAD ARRAYS WITH MICROTURBINE GENERATOR DATA
        for (int GeneratorNum = 1; GeneratorNum <= NumMTGenerators; ++GeneratorNum) {
            int NumAlphas;
            int NumNums;
            int IOStat;
            Array1D<Real64> NumArray(19);
            Array1D_string AlphArray(20);
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          GeneratorNum,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);
            MTGenerator(GeneratorNum).Name = AlphArray(1);

            MTGenerator(GeneratorNum).RefElecPowerOutput = NumArray(1);
            if (MTGenerator(GeneratorNum).RefElecPowerOutput <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(NumArray(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(1) + " must be greater than 0.");
                ErrorsFound = true;
            }

            MTGenerator(GeneratorNum).MinElecPowerOutput = NumArray(2);
            MTGenerator(GeneratorNum).MaxElecPowerOutput = NumArray(3);

            if (MTGenerator(GeneratorNum).MinElecPowerOutput < 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(NumArray(2), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " must be greater than 0.");
                ErrorsFound = true;
            }

            if (DataIPShortCuts::lNumericFieldBlanks(3)) {
                MTGenerator(GeneratorNum).MaxElecPowerOutput = MTGenerator(GeneratorNum).RefElecPowerOutput;
            } else {
                if (MTGenerator(GeneratorNum).MaxElecPowerOutput <= 0.0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(3) + '=' + General::RoundSigDigits(NumArray(3), 2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(3) + " must be greater than 0.");
                    ErrorsFound = true;
                }
            }

            if (MTGenerator(GeneratorNum).MinElecPowerOutput >= MTGenerator(GeneratorNum).MaxElecPowerOutput) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "= " + MTGenerator(GeneratorNum).Name);
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " [" + General::RoundSigDigits(NumArray(2), 2) + "] > " +
                                  DataIPShortCuts::cNumericFieldNames(3) + " [" + General::RoundSigDigits(NumArray(3), 2) + ']');
                ShowContinueError("Minimum Full Load Electrical Power Output must be less than or equal");
                ShowContinueError("to Maximum Full Load Electrical Power Output.");
                ErrorsFound = true;
            }

            if (MTGenerator(GeneratorNum).RefElecPowerOutput > MTGenerator(GeneratorNum).MaxElecPowerOutput ||
                MTGenerator(GeneratorNum).RefElecPowerOutput < MTGenerator(GeneratorNum).MinElecPowerOutput) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "= " + MTGenerator(GeneratorNum).Name);
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(1) + " must be >= " + DataIPShortCuts::cNumericFieldNames(2));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(1) + " must be <= " + DataIPShortCuts::cNumericFieldNames(3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(1) + " = " + General::RoundSigDigits(NumArray(1), 2));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(NumArray(2), 2));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(3) + " = " + General::RoundSigDigits(NumArray(3), 2));
                ErrorsFound = true;
            }

            MTGenerator(GeneratorNum).RefElecEfficiencyLHV = NumArray(4);

            if (MTGenerator(GeneratorNum).RefElecEfficiencyLHV <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(4) + '=' + General::RoundSigDigits(NumArray(4), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(4) + " must be greater than 0.");
                ErrorsFound = true;
            }

            MTGenerator(GeneratorNum).RefCombustAirInletTemp = NumArray(5);
            MTGenerator(GeneratorNum).RefCombustAirInletHumRat = NumArray(6);
            MTGenerator(GeneratorNum).RefElevation = NumArray(7);

            if (MTGenerator(GeneratorNum).RefCombustAirInletHumRat <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(6) + '=' + General::RoundSigDigits(NumArray(6), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(6) + " must be greater than 0.");
                ErrorsFound = true;
            } else {
                // Reference barometric pressure, adjusted for reference elevation (Pa)
                Real64 RefBaroPressure = 101325.0 * std::pow(1.0 - 2.25577e-05 * MTGenerator(GeneratorNum).RefElevation, 5.2559);
                MTGenerator(GeneratorNum).RefCombustAirInletDensity = Psychrometrics::PsyRhoAirFnPbTdbW(
                    RefBaroPressure, MTGenerator(GeneratorNum).RefCombustAirInletTemp, MTGenerator(GeneratorNum).RefCombustAirInletHumRat);
            }

            MTGenerator(GeneratorNum).ElecPowFTempElevCurveNum = CurveManager::GetCurveIndex(AlphArray(2)); // Convert curve name to number
            if (MTGenerator(GeneratorNum).ElecPowFTempElevCurveNum == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + AlphArray(2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            } else {
                // Verify curve object, only legal type is BiQuadratic
                ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ElecPowFTempElevCurveNum, // Curve index
                                                            {2},                                                // Valid dimensions
                                                            "GetMTGeneratorInput: ",                            // Routine name
                                                            DataIPShortCuts::cCurrentModuleObject,              // Object Type
                                                            MTGenerator(GeneratorNum).Name,                     // Object Name
                                                            DataIPShortCuts::cAlphaFieldNames(2));              // Field Name

                if (!ErrorsFound) {
                    // Check electrical power output at reference combustion inlet temp and elevation
                    // Output of Electrical Power Output Modifier Curve (function of temp and elev)
                    Real64 ElectOutFTempElevOutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ElecPowFTempElevCurveNum,
                                                                              MTGenerator(GeneratorNum).RefCombustAirInletTemp,
                                                                              MTGenerator(GeneratorNum).RefElevation);
                    if (std::abs(ElectOutFTempElevOutput - 1.0) > 0.1) {
                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                        ShowContinueError(DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                        ShowContinueError("...Curve output at reference conditions should equal 1 (+-10%).");
                        ShowContinueError("...Reference combustion air inlet temperature = " +
                                          General::TrimSigDigits(MTGenerator(GeneratorNum).RefCombustAirInletTemp, 4) + " C");
                        ShowContinueError("...Reference elevation                        = " +
                                          General::TrimSigDigits(MTGenerator(GeneratorNum).RefElevation, 4) + " m");
                        ShowContinueError("...Curve output                               = " + General::TrimSigDigits(ElectOutFTempElevOutput, 4));
                    }
                }
            }

            MTGenerator(GeneratorNum).ElecEffFTempCurveNum = CurveManager::GetCurveIndex(AlphArray(3)); // Convert curve name to number
            if (MTGenerator(GeneratorNum).ElecEffFTempCurveNum == 0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                ShowSevereError(DataIPShortCuts::cAlphaFieldNames(3) + " not found = " + AlphArray(3));
                ErrorsFound = true;
            } else {
                // Verify curve object, only legal types are Quadratic and Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ElecEffFTempCurveNum, // Curve index
                                                            {1},                                            // Valid dimensions
                                                            "GetMTGeneratorInput: ",                        // Routine name
                                                            DataIPShortCuts::cCurrentModuleObject,          // Object Type
                                                            MTGenerator(GeneratorNum).Name,                 // Object Name
                                                            DataIPShortCuts::cAlphaFieldNames(3));          // Field Name

                if (!ErrorsFound) {
                    // Check electrical efficiency at reference combustion inlet temp
                    // Output of Electrical Efficiency Modifier Curve (function of temp)
                    Real64 ElecEfficFTempOutput =
                        CurveManager::CurveValue(MTGenerator(GeneratorNum).ElecEffFTempCurveNum, MTGenerator(GeneratorNum).RefCombustAirInletTemp);
                    if (std::abs(ElecEfficFTempOutput - 1.0) > 0.1) {
                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                        ShowContinueError(DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("... Curve output at reference condition should equal 1 (+-10%).");
                        ShowContinueError("... Reference combustion air inlet temperature = " +
                                          General::TrimSigDigits(MTGenerator(GeneratorNum).RefCombustAirInletTemp, 4) + " C");
                        ShowContinueError("... Curve output                               = " + General::TrimSigDigits(ElecEfficFTempOutput, 4));
                    }
                }
            }

            MTGenerator(GeneratorNum).ElecEffFPLRCurveNum = CurveManager::GetCurveIndex(AlphArray(4)); // Convert curve name to number
            if (MTGenerator(GeneratorNum).ElecEffFPLRCurveNum == 0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                ShowSevereError(DataIPShortCuts::cAlphaFieldNames(4) + " not found = " + AlphArray(4));
                ErrorsFound = true;
            } else {
                // Verify curve object, only legal types are Quadratic and Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ElecEffFPLRCurveNum, // Curve index
                                                            {1},                                           // Valid dimensions
                                                            "GetMTGeneratorInput: ",                       // Routine name
                                                            DataIPShortCuts::cCurrentModuleObject,         // Object Type
                                                            MTGenerator(GeneratorNum).Name,                // Object Name
                                                            DataIPShortCuts::cAlphaFieldNames(4));         // Field Name

                if (!ErrorsFound) {
                    // Check electrical efficiency at PLR = 1
                    // Output of Electrical Efficiency Modifier Curve (function of PLR)
                    Real64 ElecEfficFPLROutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ElecEffFPLRCurveNum, 1.0);
                    if (std::abs(ElecEfficFPLROutput - 1.0) > 0.1) {
                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                        ShowContinueError(DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                        ShowContinueError("... Curve output at a part-load ratio of 1 should equal 1 (+-10%).");
                        ShowContinueError("... Curve output = " + General::TrimSigDigits(ElecEfficFPLROutput, 4));
                    }

                    Real64 Var1Min(0.0);
                    Real64 Var1Max(0.0);
                    CurveManager::GetCurveMinMaxValues(MTGenerator(GeneratorNum).ElecEffFPLRCurveNum, Var1Min, Var1Max);
                    MTGenerator(GeneratorNum).MinPartLoadRat = Var1Min;
                    MTGenerator(GeneratorNum).MaxPartLoadRat = Var1Max;
                }
            }

            // Fuel Type case statement
            {
                auto const SELECT_CASE_var(AlphArray(5));
                if (is_blank(SELECT_CASE_var)) { // If blank, then the default is Natural Gas
                    MTGenerator(GeneratorNum).FuelType = "Gas";

                } else if (SELECT_CASE_var == "NATURALGAS") {
                    MTGenerator(GeneratorNum).FuelType = "Gas";

                } else if (SELECT_CASE_var == "PROPANE") {
                    MTGenerator(GeneratorNum).FuelType = "Propane";

                } else {
                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + "  = " + AlphArray(5));
                    ErrorsFound = true;
                }
            }

            MTGenerator(GeneratorNum).FuelHigherHeatingValue = NumArray(8);
            MTGenerator(GeneratorNum).FuelLowerHeatingValue = NumArray(9);

            if (MTGenerator(GeneratorNum).FuelLowerHeatingValue <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(9) + '=' + General::RoundSigDigits(NumArray(9), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " must be greater than 0.");
                ErrorsFound = true;
            }

            if (MTGenerator(GeneratorNum).FuelHigherHeatingValue <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(8) + '=' + General::RoundSigDigits(NumArray(8), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(8) + " must be greater than 0.");
                ErrorsFound = true;
            }

            if (MTGenerator(GeneratorNum).FuelLowerHeatingValue > MTGenerator(GeneratorNum).FuelHigherHeatingValue) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(8) + " must be greater than the " + DataIPShortCuts::cNumericFieldNames(9));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(8) + '=' + General::RoundSigDigits(NumArray(8), 2));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + '=' + General::RoundSigDigits(NumArray(9), 2));
                ErrorsFound = true;
            }

            MTGenerator(GeneratorNum).StandbyPower = NumArray(10);
            if (MTGenerator(GeneratorNum).StandbyPower < 0.0) {
                ShowWarningError("Invalid " + DataIPShortCuts::cNumericFieldNames(10) + '=' + General::RoundSigDigits(NumArray(10), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " must be greater than 0.");
                ShowContinueError("Resetting to 0 and the simulation continues.");
                MTGenerator(GeneratorNum).StandbyPower = 0.0;
            }

            MTGenerator(GeneratorNum).AncillaryPower = NumArray(11);
            if (MTGenerator(GeneratorNum).AncillaryPower < 0.0) {
                ShowWarningError("Invalid " + DataIPShortCuts::cNumericFieldNames(11) + '=' + General::RoundSigDigits(NumArray(11), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(11) + " must be greater than 0.");
                ShowContinueError("Resetting to 0 and the simulation continues.");
                MTGenerator(GeneratorNum).AncillaryPower = 0.0;
            }

            MTGenerator(GeneratorNum).AncillaryPowerFuelCurveNum = CurveManager::GetCurveIndex(AlphArray(6)); // Convert curve name to number
            //   If blank, then the calc routine assumes modifier curve value = 1 for entire simulation
            if (!DataIPShortCuts::lAlphaFieldBlanks(6) && MTGenerator(GeneratorNum).AncillaryPowerFuelCurveNum == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + AlphArray(6));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            } else if (MTGenerator(GeneratorNum).AncillaryPowerFuelCurveNum > 0) {
                // Verify curve object, only legal type is Quadratic
                ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).AncillaryPowerFuelCurveNum, // Curve index
                                                            {1},                                                  // Valid dimensions
                                                            "GetMTGeneratorInput: ",                              // Routine name
                                                            DataIPShortCuts::cCurrentModuleObject,                // Object Type
                                                            MTGenerator(GeneratorNum).Name,                       // Object Name
                                                            DataIPShortCuts::cAlphaFieldNames(6));                // Field Name

                if (!ErrorsFound) {
                    // Fuel mass flow rate at reference conditions (kg/s)
                    Real64 RefFuelUseMdot = (MTGenerator(GeneratorNum).RefElecPowerOutput / MTGenerator(GeneratorNum).RefElecEfficiencyLHV) /
                                            (MTGenerator(GeneratorNum).FuelLowerHeatingValue * 1000.0);
                    // Output of Ancillary Power Modifer Curve (function of temps and fuel flow)
                    Real64 AncillaryPowerOutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).AncillaryPowerFuelCurveNum, RefFuelUseMdot);
                    if (std::abs(AncillaryPowerOutput - 1.0) > 0.1) {
                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                        ShowContinueError(DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                        ShowContinueError("... Curve output at reference conditions should equal 1 (+-10%).");
                        ShowContinueError("... Reference Electrical Power Output           = " +
                                          General::TrimSigDigits(MTGenerator(GeneratorNum).RefElecPowerOutput, 2) + " W");
                        ShowContinueError("... Reference Electrical Efficiency (LHV basis) = " +
                                          General::TrimSigDigits(MTGenerator(GeneratorNum).RefElecEfficiencyLHV, 4));
                        ShowContinueError("... Fuel Lower Heating Value                    = " +
                                          General::TrimSigDigits(MTGenerator(GeneratorNum).FuelLowerHeatingValue, 2) + " kJ/kg");
                        ShowContinueError("... Calculated fuel flow                        = " + General::TrimSigDigits(RefFuelUseMdot, 4) + " kg/s");
                        ShowContinueError("... Curve output                                = " + General::TrimSigDigits(AncillaryPowerOutput, 4));
                    }
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(7)) {
                MTGenerator(GeneratorNum).HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(7),
                                                                                                    ErrorsFound,
                                                                                                    DataIPShortCuts::cCurrentModuleObject,
                                                                                                    MTGenerator(GeneratorNum).Name,
                                                                                                    DataLoopNode::NodeType_Water,
                                                                                                    DataLoopNode::NodeConnectionType_Inlet,
                                                                                                    1,
                                                                                                    DataLoopNode::ObjectIsNotParent);
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(8)) {
                MTGenerator(GeneratorNum).HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(8),
                                                                                                     ErrorsFound,
                                                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                                                     MTGenerator(GeneratorNum).Name,
                                                                                                     DataLoopNode::NodeType_Water,
                                                                                                     DataLoopNode::NodeConnectionType_Outlet,
                                                                                                     1,
                                                                                                     DataLoopNode::ObjectIsNotParent);
            }

            if (MTGenerator(GeneratorNum).HeatRecInletNodeNum > 0 && MTGenerator(GeneratorNum).HeatRecOutletNodeNum > 0) {
                BranchNodeConnections::TestCompSet(
                    DataIPShortCuts::cCurrentModuleObject, MTGenerator(GeneratorNum).Name, AlphArray(7), AlphArray(8), "Heat Recovery Nodes");
            }

            if ((MTGenerator(GeneratorNum).HeatRecOutletNodeNum > 0 && MTGenerator(GeneratorNum).HeatRecInletNodeNum == 0) ||
                (MTGenerator(GeneratorNum).HeatRecOutletNodeNum == 0 && MTGenerator(GeneratorNum).HeatRecInletNodeNum > 0)) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                ShowContinueError("... If one Heat Recovery Water Node Name is specified, then both the Inlet and Outlet Heat Recovery");
                ShowContinueError("... Water Node Names must be specified. Only one water node is being specified for this generator.");
                ErrorsFound = true;
            }

            //   Heat recovery to water input fields only valid if water nodes are defined
            if (MTGenerator(GeneratorNum).HeatRecInletNodeNum != 0 && MTGenerator(GeneratorNum).HeatRecOutletNodeNum != 0) {

                MTGenerator(GeneratorNum).HeatRecActive = true;

                MTGenerator(GeneratorNum).RefThermalEffLHV = NumArray(12);
                if (MTGenerator(GeneratorNum).RefThermalEffLHV < 0.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " must be >= 0.");
                    ShowContinueError("Resetting to 0 and the simulation continues.");
                    MTGenerator(GeneratorNum).RefThermalEffLHV = 0.0;
                }

                // Next store thermal power output ranges using nominal thermal to electrical efficiency ratio and electrical power data
                MTGenerator(GeneratorNum).RefThermalPowerOutput = MTGenerator(GeneratorNum).RefElecPowerOutput *
                                                                  MTGenerator(GeneratorNum).RefThermalEffLHV /
                                                                  MTGenerator(GeneratorNum).RefElecEfficiencyLHV;
                MTGenerator(GeneratorNum).MinThermalPowerOutput = MTGenerator(GeneratorNum).MinElecPowerOutput *
                                                                  MTGenerator(GeneratorNum).RefThermalEffLHV /
                                                                  MTGenerator(GeneratorNum).RefElecEfficiencyLHV;
                MTGenerator(GeneratorNum).MaxThermalPowerOutput = MTGenerator(GeneratorNum).MaxElecPowerOutput *
                                                                  MTGenerator(GeneratorNum).RefThermalEffLHV /
                                                                  MTGenerator(GeneratorNum).RefElecEfficiencyLHV;

                MTGenerator(GeneratorNum).RefInletWaterTemp = NumArray(13);

                if (UtilityRoutines::SameString(AlphArray(9), "InternalControl")) {
                    MTGenerator(GeneratorNum).InternalFlowControl = true; //  A9, \field Heat Recovery Water Flow Operating Mode
                    MTGenerator(GeneratorNum).PlantFlowControl = false;
                }
                if ((!(UtilityRoutines::SameString(AlphArray(9), "InternalControl"))) &&
                    (!(UtilityRoutines::SameString(AlphArray(9), "PlantControl")))) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + AlphArray(9));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Operating Mode must be INTERNAL CONTROL or PLANT CONTROL.");
                    ErrorsFound = true;
                }

                MTGenerator(GeneratorNum).RefHeatRecVolFlowRate = NumArray(14);

                if (MTGenerator(GeneratorNum).RefHeatRecVolFlowRate <= 0.0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(14) + '=' + General::RoundSigDigits(NumArray(14), 2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(14) + " must be greater than 0.");
                    ErrorsFound = true;
                }

                if (MTGenerator(GeneratorNum).InternalFlowControl) { // Get Heat Recovery Water Flow Rate Modifier Curve

                    MTGenerator(GeneratorNum).HeatRecFlowFTempPowCurveNum = CurveManager::GetCurveIndex(AlphArray(10));
                    if (MTGenerator(GeneratorNum).HeatRecFlowFTempPowCurveNum != 0) {
                        // Verify curve object, only legal type is BiQuadratic
                        ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).HeatRecFlowFTempPowCurveNum, // Curve index
                                                                    {2},                                                   // Valid dimensions
                                                                    "GetMTGeneratorInput: ",                               // Routine name
                                                                    DataIPShortCuts::cCurrentModuleObject,                 // Object Type
                                                                    MTGenerator(GeneratorNum).Name,                        // Object Name
                                                                    DataIPShortCuts::cAlphaFieldNames(10));                // Field Name
                    }

                } // End of IF (MTGenerator(GeneratorNum)%InternalFlowControl) THEN

                MTGenerator(GeneratorNum).ThermEffFTempElevCurveNum = CurveManager::GetCurveIndex(AlphArray(11)); // convert curve name to number
                if (MTGenerator(GeneratorNum).ThermEffFTempElevCurveNum != 0) {
                    // Verify curve object, only legal types are BiQuadratic and BiCubic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ThermEffFTempElevCurveNum, // Curve index
                                                                {2},                                                 // Valid dimensions
                                                                "GetMTGeneratorInput: ",                             // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,               // Object Type
                                                                MTGenerator(GeneratorNum).Name,                      // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(11));              // Field Name

                    if (!ErrorsFound) {
                        // Output of Thermal Efficiency Modifier Curve (function of temp and elevation)
                        Real64 ThermalEffTempElevOutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ThermEffFTempElevCurveNum,
                                                                                   MTGenerator(GeneratorNum).RefCombustAirInletTemp,
                                                                                   MTGenerator(GeneratorNum).RefElevation);

                        if (std::abs(ThermalEffTempElevOutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(11) + " = " + AlphArray(11));
                            ShowContinueError("... Curve output at reference conditions should equal 1 (+-10%).");
                            ShowContinueError("... Reference combustion air inlet temperature      = " +
                                              General::TrimSigDigits(MTGenerator(GeneratorNum).RefCombustAirInletTemp, 4) + " C");
                            ShowContinueError("... Reference elevation                             = " +
                                              General::TrimSigDigits(MTGenerator(GeneratorNum).RefElevation, 4) + " m");
                            ShowContinueError("... Curve output                                    = " +
                                              General::TrimSigDigits(ThermalEffTempElevOutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).HeatRecRateFPLRCurveNum = CurveManager::GetCurveIndex(AlphArray(12)); // convert curve name to number
                if (MTGenerator(GeneratorNum).HeatRecRateFPLRCurveNum != 0) {
                    // Verify curve object, only legal types are Quadratic or Cubic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).HeatRecRateFPLRCurveNum, // Curve index
                                                                {1},                                               // Valid dimensions
                                                                "GetMTGeneratorInput: ",                           // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,             // Object Type
                                                                MTGenerator(GeneratorNum).Name,                    // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(12));            // Field Name

                    if (!ErrorsFound) {
                        // Output of Heat Recovery Rate Modifier Curve (function of PLR)
                        Real64 HeatRecRateFPLROutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).HeatRecRateFPLRCurveNum, 1.0);

                        if (std::abs(HeatRecRateFPLROutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(12) + " = " + AlphArray(12));
                            ShowContinueError("... Curve output at a part-load ratio of 1 should equal 1 (+-10%).");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(HeatRecRateFPLROutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).HeatRecRateFTempCurveNum = CurveManager::GetCurveIndex(AlphArray(13)); // convert curve name to number
                if (MTGenerator(GeneratorNum).HeatRecRateFTempCurveNum != 0) {
                    // Verify curve object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).HeatRecRateFTempCurveNum, // Curve index
                                                                {1},                                                // Valid dimensions
                                                                "GetMTGeneratorInput: ",                            // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,              // Object Type
                                                                MTGenerator(GeneratorNum).Name,                     // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(13));             // Field Name

                    if (!ErrorsFound) {
                        // Output of Heat Recovery Rate Modifier Curve (function of inlet water temp)
                        Real64 HeatRecRateFTempOutput =
                            CurveManager::CurveValue(MTGenerator(GeneratorNum).HeatRecRateFTempCurveNum, MTGenerator(GeneratorNum).RefInletWaterTemp);

                        if (std::abs(HeatRecRateFTempOutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(13) + " = " + AlphArray(13));
                            ShowContinueError("... Curve output at reference condition should equal 1 (+-10%).");
                            ShowContinueError("... Reference inlet water temperature temperature      = " +
                                              General::TrimSigDigits(MTGenerator(GeneratorNum).RefInletWaterTemp, 4) + " C");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(HeatRecRateFTempOutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).HeatRecRateFWaterFlowCurveNum = CurveManager::GetCurveIndex(AlphArray(14));
                if (MTGenerator(GeneratorNum).HeatRecRateFWaterFlowCurveNum != 0) {
                    // Verify curve object, only legal type is Quadratic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).HeatRecRateFWaterFlowCurveNum, // Curve index
                                                                {1},                                                     // Valid dimensions
                                                                "GetMTGeneratorInput: ",                                 // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,                   // Object Type
                                                                MTGenerator(GeneratorNum).Name,                          // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(14));                  // Field Name

                    if (!ErrorsFound) {
                        // Output of Heat Recovery Rate Modifier Curve (function of water flow rate)
                        Real64 HeatRecRateFFlowOutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).HeatRecRateFWaterFlowCurveNum,
                                                                                 MTGenerator(GeneratorNum).RefHeatRecVolFlowRate);

                        if (std::abs(HeatRecRateFFlowOutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(14) + " = " + AlphArray(14));
                            ShowContinueError("... Curve output at reference condition should equal 1 (+-10%).");
                            ShowContinueError("... Reference Heat Recovery Water Flow Rate      = " +
                                              General::TrimSigDigits(MTGenerator(GeneratorNum).RefHeatRecVolFlowRate, 4) + " m3/s");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(HeatRecRateFFlowOutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).HeatRecMinVolFlowRate = NumArray(15);
                if (MTGenerator(GeneratorNum).HeatRecMinVolFlowRate < 0.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(15) + " must be >= 0.");
                    ShowContinueError("Resetting to 0 and the simulation continues.");
                    MTGenerator(GeneratorNum).HeatRecMinVolFlowRate = 0.0;
                }

                MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate = NumArray(16);
                if (MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate < 0.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(16) + " must be >= 0.");
                    ShowContinueError("Resetting to 0 and the simulation continues.");
                    MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate = 0.0;
                }

                if (MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate < MTGenerator(GeneratorNum).HeatRecMinVolFlowRate) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(16) + " must be >= " + DataIPShortCuts::cNumericFieldNames(15));
                    ShowContinueError("Resetting " + DataIPShortCuts::cNumericFieldNames(16) + " = " + DataIPShortCuts::cNumericFieldNames(15) +
                                      " and the simulation continues.");
                    MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate = MTGenerator(GeneratorNum).HeatRecMinVolFlowRate;
                }

                //     Check if reference heat recovery water flow rate is below the minimum flow rate
                if (MTGenerator(GeneratorNum).RefHeatRecVolFlowRate < MTGenerator(GeneratorNum).HeatRecMinVolFlowRate) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(14) + " must be >= " + DataIPShortCuts::cNumericFieldNames(15));
                    ShowContinueError("Resetting " + DataIPShortCuts::cNumericFieldNames(14) + " = " + DataIPShortCuts::cNumericFieldNames(15) +
                                      " and the simulation continues.");
                    MTGenerator(GeneratorNum).RefHeatRecVolFlowRate = MTGenerator(GeneratorNum).HeatRecMinVolFlowRate;
                }

                //     Check if reference heat recovery water flow rate is above the maximum flow rate
                if (MTGenerator(GeneratorNum).RefHeatRecVolFlowRate > MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(14) + " must be <= " + DataIPShortCuts::cNumericFieldNames(16));
                    ShowContinueError("Resetting " + DataIPShortCuts::cNumericFieldNames(14) + " = " + DataIPShortCuts::cNumericFieldNames(16) +
                                      " and the simulation continues.");
                    MTGenerator(GeneratorNum).RefHeatRecVolFlowRate = MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate;
                }

                PlantUtilities::RegisterPlantCompDesignFlow(MTGenerator(GeneratorNum).HeatRecInletNodeNum,
                                                            MTGenerator(GeneratorNum).HeatRecMaxVolFlowRate);

                MTGenerator(GeneratorNum).HeatRecMaxWaterTemp = NumArray(17);

            } // End of 'IF (MTGenerator(GeneratorNum)%HeatRecInletNodeNum .NE. 0 .AND. &
            //             MTGenerator(GeneratorNum)%HeatRecOutletNodeNum .NE. 0) THEN'

            if (!DataIPShortCuts::lAlphaFieldBlanks(15)) {
                MTGenerator(GeneratorNum).CombustionAirInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(15),
                                                                                                          ErrorsFound,
                                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                                          AlphArray(1),
                                                                                                          DataLoopNode::NodeType_Air,
                                                                                                          DataLoopNode::NodeConnectionType_Inlet,
                                                                                                          2,
                                                                                                          DataLoopNode::ObjectIsNotParent);
            }

            //    Combustion air inlet node must be an outside air node
            if (!DataIPShortCuts::lAlphaFieldBlanks(15) &&
                !OutAirNodeManager::CheckOutAirNodeNumber(MTGenerator(GeneratorNum).CombustionAirInletNodeNum)) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(15) + " is not a valid Outdoor Air Node = " + AlphArray(15));
                ShowContinueError("it does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(16)) {
                MTGenerator(GeneratorNum).CombustionAirOutletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(16),
                                                                                                           ErrorsFound,
                                                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                                                           AlphArray(1),
                                                                                                           DataLoopNode::NodeType_Air,
                                                                                                           DataLoopNode::NodeConnectionType_Outlet,
                                                                                                           2,
                                                                                                           DataLoopNode::ObjectIsNotParent);
            }

            if (MTGenerator(GeneratorNum).CombustionAirOutletNodeNum > 0 && MTGenerator(GeneratorNum).CombustionAirInletNodeNum == 0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                ShowContinueError("A " + DataIPShortCuts::cAlphaFieldNames(15) + " must be specified when a " +
                                  DataIPShortCuts::cAlphaFieldNames(16) + " is specified.");
                ErrorsFound = true;
            }

            //   Get other exhaust air inputs only if combustion air inlet and outlet nodes are valid
            if (MTGenerator(GeneratorNum).CombustionAirOutletNodeNum > 0 && MTGenerator(GeneratorNum).CombustionAirInletNodeNum > 0) {

                MTGenerator(GeneratorNum).ExhAirCalcsActive = true;
                MTGenerator(GeneratorNum).RefExhaustAirMassFlowRate = NumArray(18);
                if (MTGenerator(GeneratorNum).RefExhaustAirMassFlowRate <= 0.0 && !DataIPShortCuts::lNumericFieldBlanks(18)) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(18) + '=' + General::RoundSigDigits(NumArray(18), 2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(18) + " must be greater than 0.");
                    ErrorsFound = true;
                }

                MTGenerator(GeneratorNum).ExhFlowFTempCurveNum = CurveManager::GetCurveIndex(AlphArray(17));
                if (MTGenerator(GeneratorNum).ExhFlowFTempCurveNum != 0) {
                    // Verify curve object, only legal types are Quadratic and Cubic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ExhFlowFTempCurveNum, // Curve index
                                                                {1},                                            // Valid dimensions
                                                                "GetMTGeneratorInput: ",                        // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,          // Object Type
                                                                MTGenerator(GeneratorNum).Name,                 // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(17));         // Field Name

                    if (!ErrorsFound) {
                        // Output of Exhaust Air Flow Modifier Curve (function of inlet air temp)
                        Real64 ExhFlowFTempOutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ExhFlowFTempCurveNum,
                                                                             MTGenerator(GeneratorNum).RefCombustAirInletTemp);

                        if (std::abs(ExhFlowFTempOutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(17) + " = " + AlphArray(17));
                            ShowContinueError("... Curve output at reference condition should equal 1 (+-10%).");
                            ShowContinueError("... Reference combustion air inlet temperature      = " +
                                              General::TrimSigDigits(MTGenerator(GeneratorNum).RefCombustAirInletTemp, 4) + " C");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(ExhFlowFTempOutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).ExhFlowFPLRCurveNum = CurveManager::GetCurveIndex(AlphArray(18)); // convert curve name to number
                if (MTGenerator(GeneratorNum).ExhFlowFPLRCurveNum != 0) {
                    // Verify curve object, legal types are Quadratic or Cubic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ExhFlowFPLRCurveNum, // Curve index
                                                                {1},                                           // Valid dimensions
                                                                "GetMTGeneratorInput: ",                       // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,         // Object Type
                                                                MTGenerator(GeneratorNum).Name,                // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(18));        // Field Name

                    if (!ErrorsFound) {
                        // Output of Exhaust Air Flow Modifier Curve (function of PLR)
                        Real64 ExhFlowFPLROutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ExhFlowFPLRCurveNum, 1.0);

                        if (std::abs(ExhFlowFPLROutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(18) + " = " + AlphArray(18));
                            ShowContinueError("... Curve output at a part-load ratio of 1 should equal 1 (+-10%).");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(ExhFlowFPLROutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).NomExhAirOutletTemp = NumArray(19);

                MTGenerator(GeneratorNum).ExhAirTempFTempCurveNum = CurveManager::GetCurveIndex(AlphArray(19));
                if (MTGenerator(GeneratorNum).ExhAirTempFTempCurveNum != 0) {
                    // Verify curve object, only legal types are Quadratic and Cubic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ExhAirTempFTempCurveNum, // Curve index
                                                                {1},                                               // Valid dimensions
                                                                "GetMTGeneratorInput: ",                           // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,             // Object Type
                                                                MTGenerator(GeneratorNum).Name,                    // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(19));            // Field Name

                    if (!ErrorsFound) {
                        // Output of Exhaust Air Temperature Modifier Curve (function of inlet air temp)
                        Real64 ExhAirTempFTempOutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ExhAirTempFTempCurveNum,
                                                                                MTGenerator(GeneratorNum).RefCombustAirInletTemp);

                        if (std::abs(ExhAirTempFTempOutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(19) + " = " + AlphArray(19));
                            ShowContinueError("... Curve output at reference condition should equal 1 (+-10%).");
                            ShowContinueError("... Reference combustion air inlet temperature      = " +
                                              General::TrimSigDigits(MTGenerator(GeneratorNum).RefCombustAirInletTemp, 4) + " C");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(ExhAirTempFTempOutput, 4));
                        }
                    }
                }

                MTGenerator(GeneratorNum).ExhAirTempFPLRCurveNum = CurveManager::GetCurveIndex(AlphArray(20)); // convert curve name to number
                if (MTGenerator(GeneratorNum).ExhAirTempFPLRCurveNum != 0) {
                    // Verify curve object, legal types are Quadratic or Cubic
                    ErrorsFound |= CurveManager::CheckCurveDims(MTGenerator(GeneratorNum).ExhAirTempFPLRCurveNum, // Curve index
                                                                {1},                                              // Valid dimensions
                                                                "GetMTGeneratorInput: ",                          // Routine name
                                                                DataIPShortCuts::cCurrentModuleObject,            // Object Type
                                                                MTGenerator(GeneratorNum).Name,                   // Object Name
                                                                DataIPShortCuts::cAlphaFieldNames(20));           // Field Name

                    if (!ErrorsFound) {
                        // Output of Exhaust Air Temperature Modifier Curve (function of PLR)
                        Real64 ExhOutAirTempFPLROutput = CurveManager::CurveValue(MTGenerator(GeneratorNum).ExhAirTempFPLRCurveNum, 1.0);

                        if (std::abs(ExhOutAirTempFPLROutput - 1.0) > 0.1) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " \"" + MTGenerator(GeneratorNum).Name + "\"");
                            ShowContinueError(DataIPShortCuts::cAlphaFieldNames(20) + " = " + AlphArray(20));
                            ShowContinueError("... Curve output at a part-load ratio of 1 should equal 1 (+-10%).");
                            ShowContinueError("... Curve output = " + General::TrimSigDigits(ExhOutAirTempFPLROutput, 4));
                        }
                    }
                }

            } // End of '    IF (MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum .GT. 0 .AND. &
              //                 MTGenerator(GeneratorNum)%CombustionAirInletNodeNum .GT. 0) THEN
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void MTGeneratorSpecs::setupOutputVars()
    {
        SetupOutputVariable("Generator Produced Electric Power", OutputProcessor::Unit::W, this->ElecPowerGenerated, "System", "Average", this->Name);

        SetupOutputVariable("Generator Produced Electric Energy",
                            OutputProcessor::Unit::J,
                            this->EnergyGen,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ElectricityProduced",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Generator LHV Basis Electric Efficiency", OutputProcessor::Unit::None, this->ElectricEfficiencyLHV, "System", "Average", this->Name);

        //    Fuel specific report variables
        SetupOutputVariable(
            "Generator " + this->FuelType + " HHV Basis Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRateHHV, "System", "Average", this->Name);

        SetupOutputVariable("Generator " + this->FuelType + " HHV Basis Energy",
                            OutputProcessor::Unit::J,
                            this->FuelEnergyHHV,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            this->FuelType,
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Generator " + this->FuelType + " Mass Flow Rate", OutputProcessor::Unit::kg_s, this->FuelMdot, "System", "Average", this->Name);

        //    general fuel use report (to match other generators)
        SetupOutputVariable("Generator Fuel HHV Basis Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRateHHV, "System", "Average", this->Name);

        SetupOutputVariable("Generator Fuel HHV Basis Energy", OutputProcessor::Unit::J, this->FuelEnergyHHV, "System", "Sum", this->Name);

        //    Heat recovery (to water) report variables
        if (this->HeatRecActive) {

            SetupOutputVariable("Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->QHeatRecovered, "System", "Average", this->Name);

            SetupOutputVariable("Generator Produced Thermal Energy",
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
                "Generator Thermal Efficiency LHV Basis", OutputProcessor::Unit::None, this->ThermalEfficiencyLHV, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Water Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMdot, "System", "Average", this->Name);
        }

        if (this->StandbyPower > 0.0) { // Report Standby Power if entered by user
            SetupOutputVariable(
                "Generator Standby Electric Power", OutputProcessor::Unit::W, this->StandbyPowerRate, "System", "Average", this->Name);

            SetupOutputVariable("Generator Standby Electric Energy",
                                OutputProcessor::Unit::J,
                                this->StandbyEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Electricity",
                                "Cogeneration",
                                _,
                                "Plant");
        }

        if (this->AncillaryPower > 0.0) { // Report Ancillary Power if entered by user
            SetupOutputVariable(
                "Generator Ancillary Electric Power", OutputProcessor::Unit::W, this->AncillaryPowerRate, "System", "Average", this->Name);

            SetupOutputVariable("Generator Ancillary Electric Energy", OutputProcessor::Unit::J, this->AncillaryEnergy, "System", "Sum", this->Name);
        }

        //   Report combustion air outlet conditions if exhaust air calculations are active
        if (this->ExhAirCalcsActive) {
            SetupOutputVariable(
                "Generator Exhaust Air Mass Flow Rate", OutputProcessor::Unit::kg_s, this->ExhaustAirMassFlowRate, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Exhaust Air Temperature", OutputProcessor::Unit::C, this->ExhaustAirTemperature, "System", "Average", this->Name);
        }
    }

    void MTGeneratorSpecs::simulate(const PlantLocation &EP_UNUSED(calledFromLocation),
                                    bool EP_UNUSED(FirstHVACIteration),
                                    Real64 &EP_UNUSED(CurLoad),
                                    bool EP_UNUSED(RunFlag))
    {
        // empty function to emulate current behavior as of conversion to using the PlantComponent calling structure.
        // calls from the plant side... do nothing.
        // calls from the ElectricPowerServiceManger call the init, calc, and update worker functions
    }

    void MTGeneratorSpecs::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MaxLoad = 0.0;
        MinLoad = 0.0;
        OptLoad = 0.0;
    }

    void MTGeneratorSpecs::InitMTGenerators(bool const RunFlag,
                                            Real64 const MyLoad, // electrical load in W
                                            bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad/D. Shirey
        //       DATE WRITTEN   Mar 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  B. Griffith, Sept 2010, plant upgrades, general fluid props

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for initializations of the CT generators.

        // METHODOLOGY EMPLOYED:
        //  Uses the status flags to trigger initializations.

        std::string const RoutineName("InitMTGenerators");
        bool errFlag;

        if (this->myFlag) {
            this->setupOutputVars();
            this->myFlag = false;
        }

        if (this->MyPlantScanFlag && allocated(DataPlant::PlantLoop) && this->HeatRecActive) {
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    DataPlant::TypeOf_Generator_MicroTurbine,
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
                ShowFatalError("InitMTGenerators: Program terminated due to previous condition(s).");
            }

            this->MyPlantScanFlag = false;
        }

        if (this->MySizeAndNodeInitFlag && (!this->MyPlantScanFlag) && this->HeatRecActive) {

            // size mass flow rate
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                           DataGlobals::InitConvTemp,
                                                           DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
                                                           RoutineName);

            this->DesignHeatRecMassFlowRate = rho * this->RefHeatRecVolFlowRate;
            this->HeatRecMaxMassFlowRate = rho * this->HeatRecMaxVolFlowRate;

            PlantUtilities::InitComponentNodes(0.0,
                                               this->HeatRecMaxMassFlowRate,
                                               this->HeatRecInletNodeNum,
                                               this->HeatRecOutletNodeNum,
                                               this->HRLoopNum,
                                               this->HRLoopSideNum,
                                               this->HRBranchNum,
                                               this->HRCompNum);

            this->MySizeAndNodeInitFlag = false;

        } // end one time inits

        if (!this->HeatRecActive) return;

        // Do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && this->MyEnvrnFlag) {
            // set the node max and min mass flow rates
            PlantUtilities::InitComponentNodes(0.0,
                                               this->HeatRecMaxMassFlowRate,
                                               this->HeatRecInletNodeNum,
                                               this->HeatRecOutletNodeNum,
                                               this->HRLoopNum,
                                               this->HRLoopSideNum,
                                               this->HRBranchNum,
                                               this->HRCompNum);

            DataLoopNode::Node(this->HeatRecInletNodeNum).Temp = 20.0; // Set the node temperature, assuming freeze control
            DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = 20.0;

            this->MyEnvrnFlag = false;
        } // end environmental inits

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        // set/request flow rates
        if (FirstHVACIteration) {

            Real64 DesiredMassFlowRate;
            if (!RunFlag) {
                DesiredMassFlowRate = 0.0;

            } else if (RunFlag && this->InternalFlowControl) {
                // assume dispatch power in MyLoad is what gets produced (future, reset during calc routine and iterate)
                if (this->HeatRecFlowFTempPowCurveNum != 0) {
                    DesiredMassFlowRate =
                        this->DesignHeatRecMassFlowRate *
                        CurveManager::CurveValue(this->HeatRecFlowFTempPowCurveNum, DataLoopNode::Node(this->HeatRecInletNodeNum).Temp, MyLoad);
                } else {
                    DesiredMassFlowRate = this->DesignHeatRecMassFlowRate; // Assume modifier = 1 if curve not specified
                }

                DesiredMassFlowRate = max(DataPrecisionGlobals::constant_zero, DesiredMassFlowRate); // protect from neg. curve result

            } else if (RunFlag && (!this->InternalFlowControl)) {
                DesiredMassFlowRate = this->DesignHeatRecMassFlowRate;
            }

            PlantUtilities::SetComponentFlowRate(DesiredMassFlowRate,
                                                 this->HeatRecInletNodeNum,
                                                 this->HeatRecOutletNodeNum,
                                                 this->HRLoopNum,
                                                 this->HRLoopSideNum,
                                                 this->HRBranchNum,
                                                 this->HRCompNum);
        } else { // not FirstHVACIteration
            if (!RunFlag) {
                DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate =
                    min(DataPrecisionGlobals::constant_zero, DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRateMaxAvail);
                DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate =
                    max(DataPrecisionGlobals::constant_zero, DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRateMinAvail);

            } else if (RunFlag && this->InternalFlowControl) {
                // assume dispatch power in MyLoad is what gets produced (future, reset during calc routine and iterate)
                if (this->HeatRecFlowFTempPowCurveNum != 0) {
                    Real64 DesiredMassFlowRate =
                        this->DesignHeatRecMassFlowRate *
                        CurveManager::CurveValue(this->HeatRecFlowFTempPowCurveNum, DataLoopNode::Node(this->HeatRecInletNodeNum).Temp, MyLoad);
                    PlantUtilities::SetComponentFlowRate(DesiredMassFlowRate,
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
            } else if (RunFlag && (!this->InternalFlowControl)) {
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

    void MTGeneratorSpecs::CalcMTGeneratorModel(bool const RunFlag,  // TRUE when generator is being asked to operate
                                                Real64 const MyLoad) // Generator demand (W)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad/D. Shirey
        //       DATE WRITTEN   Mar 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Simulate a combustion generator.

        // METHODOLOGY EMPLOYED:
        //  Curve fits of performance data.

        Real64 const KJtoJ(1000.0);          // Convert kilojoules to joules
        int const MaxAncPowerIter(50);       // Maximum number of iteration (subroutine ancillary power iteration loop)
        Real64 const AncPowerDiffToler(5.0); // Tolerance for Ancillary Power Difference (W)
        Real64 const RelaxFactor(0.7);       // Relaxation factor for iteration loop
        std::string const RoutineName("CalcMTGeneratorModel");

        //   Load local variables from data structure (for code readability)
        // Min allowed operating fraction at full load
        Real64 minPartLoadRat = this->MinPartLoadRat;

        // Max allowed operating fraction at full load
        Real64 maxPartLoadRat = this->MaxPartLoadRat;

        // Generator reference capacity (W)
        Real64 ReferencePowerOutput = this->RefElecPowerOutput;

        // Reference electrical efficiency
        Real64 RefElecEfficiency = this->RefElecEfficiencyLHV;

        //   Initialize variables
        this->ElecPowerGenerated = 0.0;
        this->HeatRecInletTemp = 0.0;
        this->HeatRecOutletTemp = 0.0;
        this->HeatRecMdot = 0.0;
        this->QHeatRecovered = 0.0;
        this->ExhaustEnergyRec = 0.0;
        this->FuelEnergyUseRateHHV = 0.0;
        this->FuelMdot = 0.0;
        this->AncillaryPowerRate = 0.0;
        this->StandbyPowerRate = 0.0;
        this->FuelEnergyUseRateLHV = 0.0;
        this->ExhaustAirMassFlowRate = 0.0;
        this->ExhaustAirTemperature = 0.0;
        this->ExhaustAirHumRat = 0.0;

        Real64 HeatRecInTemp; // Heat recovery fluid inlet temperature (C)
        Real64 heatRecMdot;   // Heat recovery fluid mass flow rate (kg/s)
        Real64 HeatRecCp;     // Specific heat of the heat recovery fluid (J/kg-K)

        if (this->HeatRecActive) {
            HeatRecInTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
            HeatRecCp = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->HRLoopNum).FluidName, HeatRecInTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, RoutineName);
            heatRecMdot = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
        } else {
            HeatRecInTemp = 0.0;
            HeatRecCp = 0.0;
            heatRecMdot = 0.0;
        }

        Real64 CombustionAirInletTemp;  // Combustion air inlet temperature (C)
        Real64 CombustionAirInletPress; // Barometric pressure of combustion inlet air (Pa)
        Real64 CombustionAirInletW;     // Combustion air inlet humidity ratio (kg/kg)

        //   Set combustion inlet air temperature, humidity ratio and pressure local variables
        if (this->CombustionAirInletNodeNum == 0) { // no inlet air node specified, so use weather file values
            CombustionAirInletTemp = DataEnvironment::OutDryBulbTemp;
            CombustionAirInletW = DataEnvironment::OutHumRat;
            CombustionAirInletPress = DataEnvironment::OutBaroPress;
        } else { // use inlet node information
            CombustionAirInletTemp = DataLoopNode::Node(this->CombustionAirInletNodeNum).Temp;
            CombustionAirInletW = DataLoopNode::Node(this->CombustionAirInletNodeNum).HumRat;
            CombustionAirInletPress = DataLoopNode::Node(this->CombustionAirInletNodeNum).Press;
            if (DataLoopNode::Node(this->CombustionAirInletNodeNum).Height > 0.0) {
            }
            //     Initialize combustion outlet air conditions to inlet air conditions (all node properties)
            if (this->ExhAirCalcsActive) {
                DataLoopNode::Node(this->CombustionAirOutletNodeNum) = DataLoopNode::Node(this->CombustionAirInletNodeNum);
            }
        }

        //   If no loop demand or generator OFF, set some variables and then return
        //    IF (.NOT. RunFlag .OR. MyLoad .LE. 0.0d0) THEN
        if (MyLoad <= 0.0) {
            this->HeatRecInletTemp = HeatRecInTemp;
            this->HeatRecOutletTemp = HeatRecInTemp;
            if (RunFlag) {
                this->StandbyPowerRate = this->StandbyPower;
            }
            this->ExhaustAirTemperature = CombustionAirInletTemp;
            this->ExhaustAirHumRat = CombustionAirInletW;
            return;
        }

        //   Calculate power modifier curve value (function of inlet air temperature and elevation)
        // Power ratio as a function of inlet air temperature and elevation
        Real64 PowerFTempElev = CurveManager::CurveValue(this->ElecPowFTempElevCurveNum, CombustionAirInletTemp, DataEnvironment::Elevation);

        //   Warn user if power modifier curve output is less than 0
        if (PowerFTempElev < 0.0) {
            if (this->PowerFTempElevErrorIndex == 0) {
                //        MTGenerator(GeneratorNum)%PowerFTempElevErrorCount = MTGenerator(GeneratorNum)%PowerFTempElevErrorCount + 1
                ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                ShowContinueError("... Electrical Power Modifier curve (function of temperature and elevation) output is less than zero (" +
                                  General::TrimSigDigits(PowerFTempElev, 4) + ").");
                ShowContinueError("... Value occurs using a combustion inlet air temperature of " +
                                  General::TrimSigDigits(CombustionAirInletTemp, 2) + " C.");
                ShowContinueError("... and an elevation of " + General::TrimSigDigits(DataEnvironment::Elevation, 2) + " m.");
                ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
            }
            ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                               "\": Electrical Power Modifier curve is less than zero warning continues...",
                                           this->PowerFTempElevErrorIndex,
                                           PowerFTempElev,
                                           PowerFTempElev);
            PowerFTempElev = 0.0;
        }

        //   Calculate available full-load power output. cannot exceed maximum full-load power output.
        // Generator full-load power output at actual inlet conditions and elevation (W)
        Real64 FullLoadPowerOutput = min((ReferencePowerOutput * PowerFTempElev), this->MaxElecPowerOutput);
        //   Also can't be below the minimum full-load power output.
        FullLoadPowerOutput = max(FullLoadPowerOutput, this->MinElecPowerOutput);

        // Ancillary power used by pump (if not specified in manufacturers data)
        Real64 ancillaryPowerRate = this->AncillaryPower;

        // Difference between ancillary power rate and ancillary power rate last (last iteration)
        Real64 AncillaryPowerRateDiff = AncPowerDiffToler + 1.0; // Initialize to force through DO WHILE Loop at least once

        Real64 PLR(0.0);                    // Generator operating part load ratio
        Real64 elecPowerGenerated(0.0);     // Generator electric power output (W)
        Real64 FuelUseEnergyRateLHV(0.0);   // Rate of fuel energy required to run microturbine, LHV basis (W)
        Real64 fuelHigherHeatingValue(0.0); // Higher heating value (LLV) of fuel kJ/kg)
        Real64 fuelLowerHeatingValue(0.0);  // Lower heating value (LLV) of fuel kJ/kg)
        Real64 AnciPowerFMdotFuel(0.0);     // Ancillary power as a function of fuel flow curve output
        int AncPowerCalcIterIndex = 0;      // Index for subroutine iteration loop if Ancillary Power (function of fuel flow) is used

        while (AncillaryPowerRateDiff > AncPowerDiffToler && AncPowerCalcIterIndex <= MaxAncPowerIter) {

            ++AncPowerCalcIterIndex; // Increment iteration loop counter

            //     Calculate operating power output (gross)
            elecPowerGenerated = min(max(0.0, MyLoad + ancillaryPowerRate), FullLoadPowerOutput);

            //     Calculate PLR, but must be between the minPLR and maxPLR
            if (FullLoadPowerOutput > 0.0) {
                PLR = min(elecPowerGenerated / FullLoadPowerOutput, maxPartLoadRat);
                PLR = max(PLR, minPartLoadRat);
            } else {
                PLR = 0.0;
            }

            //     Recalculate elecPowerGenerated based on "final" PLR
            elecPowerGenerated = FullLoadPowerOutput * PLR;

            //     Calculate electrical efficiency modifier curve output (function of temp)
            // Electrical efficiency as a function of temperature curve output
            Real64 ElecEfficiencyFTemp = CurveManager::CurveValue(this->ElecEffFTempCurveNum, CombustionAirInletTemp);

            //     Warn user if efficiency modifier curve output is less than 0
            if (ElecEfficiencyFTemp < 0.0) {
                if (this->EffFTempErrorIndex == 0) {
                    //          MTGenerator(GeneratorNum)%EffFTempErrorCount = MTGenerator(GeneratorNum)%EffFTempErrorCount + 1
                    ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                    ShowContinueError("... Electrical Efficiency Modifier (function of temperature) output is less than zero (" +
                                      General::TrimSigDigits(ElecEfficiencyFTemp, 4) + ").");
                    ShowContinueError("... Value occurs using a combustion inlet air temperature of " +
                                      General::TrimSigDigits(CombustionAirInletTemp, 2) + " C.");
                    ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "GENERATOR:MICROTURBINE \"" + this->Name +
                        "\": Electrical Efficiency Modifier (function of temperature) output is less than zero warning continues...",
                    this->EffFTempErrorIndex,
                    ElecEfficiencyFTemp,
                    ElecEfficiencyFTemp);
                ElecEfficiencyFTemp = 0.0;
            }

            //     Calculate efficiency modifier curve output (function of PLR)
            // Electrical efficiency as a function of PLR curve output
            Real64 ElecEfficiencyFPLR = CurveManager::CurveValue(this->ElecEffFPLRCurveNum, PLR);

            //     Warn user if efficiency modifier curve output is less than 0
            if (ElecEfficiencyFPLR < 0.0) {
                if (this->EffFPLRErrorIndex == 0) {
                    ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                    ShowContinueError("... Electrical Efficiency Modifier (function of part-load ratio) output is less than zero (" +
                                      General::TrimSigDigits(ElecEfficiencyFPLR, 4) + ").");
                    ShowContinueError("... Value occurs using a part-load ratio of " + General::TrimSigDigits(PLR, 3) + '.');
                    ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "GENERATOR:MICROTURBINE \"" + this->Name +
                        "\": Electrical Efficiency Modifier (function of part-load ratio) output is less than zero warning continues...",
                    this->EffFPLRErrorIndex,
                    ElecEfficiencyFPLR,
                    ElecEfficiencyFPLR);
                ElecEfficiencyFPLR = 0.0;
            }

            //     Calculate operating electrical efficiency
            // Actual operating efficiency
            Real64 OperatingElecEfficiency = RefElecEfficiency * ElecEfficiencyFTemp * ElecEfficiencyFPLR;

            //     Calculate fuel use (W = J/s), LHV basis
            if (OperatingElecEfficiency > 0.0) {
                FuelUseEnergyRateLHV = elecPowerGenerated / OperatingElecEfficiency;
            } else {
                FuelUseEnergyRateLHV = 0.0; // If fuel use rate is zero, then
                elecPowerGenerated = 0.0;   //  electric power generated must be zero.
            }

            //     Set fuel heating values
            fuelHigherHeatingValue = this->FuelHigherHeatingValue;
            fuelLowerHeatingValue = this->FuelLowerHeatingValue;

            //     Calculate fuel mass flow rate
            this->FuelMdot = FuelUseEnergyRateLHV / (fuelLowerHeatingValue * KJtoJ);

            //     Calculate ancillary power requirement
            if (this->AncillaryPowerFuelCurveNum > 0) {
                AnciPowerFMdotFuel = CurveManager::CurveValue(this->AncillaryPowerFuelCurveNum, this->FuelMdot);
                //       Warn user if ancillary power modifier curve output is less than 0
                if (AnciPowerFMdotFuel < 0.0) {
                    if (this->AnciPowerFMdotFuelErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("... Ancillary Power Modifier (function of fuel input) output is less than zero (" +
                                          General::TrimSigDigits(AnciPowerFMdotFuel, 4) + ").");
                        ShowContinueError("... Value occurs using a fuel input mass flow rate of " + General::TrimSigDigits(this->FuelMdot, 4) +
                                          " kg/s.");
                        ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        "GENERATOR:MICROTURBINE \"" + this->Name +
                            "\": Ancillary Power Modifier (function of fuel input) output is less than zero warning continues...",
                        this->AnciPowerFMdotFuelErrorIndex,
                        AnciPowerFMdotFuel,
                        AnciPowerFMdotFuel);
                    AnciPowerFMdotFuel = 0.0;
                }
            } else {
                AnciPowerFMdotFuel = 1.0;
            }

            // Ancillary power used by pump from last iteration (iteration loop within this subroutine)
            Real64 AncillaryPowerRateLast = ancillaryPowerRate;

            if (this->AncillaryPowerFuelCurveNum > 0) {
                ancillaryPowerRate = RelaxFactor * this->AncillaryPower * AnciPowerFMdotFuel - (1.0 - RelaxFactor) * AncillaryPowerRateLast;
            }

            AncillaryPowerRateDiff = std::abs(ancillaryPowerRate - AncillaryPowerRateLast);
        }

        if (AncPowerCalcIterIndex > MaxAncPowerIter) {

            if (this->AnciPowerIterErrorIndex == 0) {
                ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                ShowContinueError("... Iteration loop for electric power generation is not converging within tolerance.");
                ShowContinueError("... Check the Ancillary Power Modifier Curve (function of fuel input).");
                ShowContinueError("... Ancillary Power = " + General::TrimSigDigits(ancillaryPowerRate, 1) + " W.");
                ShowContinueError("... Fuel input rate = " + General::TrimSigDigits(AnciPowerFMdotFuel, 4) + " kg/s.");
                ShowContinueErrorTimeStamp("... Simulation will continue.");
            }
            ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                               "\": Iteration loop for electric power generation is not converging within tolerance continues...",
                                           this->AnciPowerIterErrorIndex);
        }

        //   Calculate electrical power generated
        this->ElecPowerGenerated = elecPowerGenerated - ancillaryPowerRate;

        //   Report fuel energy use rate on HHV basis, which is the unit of measure when the fuel is sold
        this->FuelEnergyUseRateHHV = this->FuelMdot * fuelHigherHeatingValue * KJtoJ;
        this->AncillaryPowerRate = ancillaryPowerRate;     // Move to data structure for later reporting
        this->FuelEnergyUseRateLHV = FuelUseEnergyRateLHV; // Move to data structure for reporting calculations

        //   When generator operates, standby losses are 0
        this->StandbyPowerRate = 0.0;

        Real64 QHeatRecToWater = 0.0; // Recovered waste heat to water (W)

        //   Calculate heat recovery if active
        if (this->HeatRecActive) {

            // Thermal efficiency as a function of air temperature and elevation
            Real64 ThermalEffFTempElev;
            if (this->ThermEffFTempElevCurveNum > 0) {
                ThermalEffFTempElev = CurveManager::CurveValue(this->ThermEffFTempElevCurveNum, CombustionAirInletTemp, DataEnvironment::Elevation);
                //       Warn user if power modifier curve output is less than 0
                if (ThermalEffFTempElev < 0.0) {
                    if (this->ThermEffFTempElevErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("... Electrical Power Modifier curve (function of temperature and elevation) output is less than zero (" +
                                          General::TrimSigDigits(PowerFTempElev, 4) + ").");
                        ShowContinueError("... Value occurs using a combustion inlet air temperature of " +
                                          General::TrimSigDigits(CombustionAirInletTemp, 2) + " C.");
                        ShowContinueError("... and an elevation of " + General::TrimSigDigits(DataEnvironment::Elevation, 2) + " m.");
                        ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                                       "\": Electrical Power Modifier curve is less than zero warning continues...",
                                                   this->ThermEffFTempElevErrorIndex,
                                                   ThermalEffFTempElev,
                                                   ThermalEffFTempElev);
                    ThermalEffFTempElev = 0.0;
                }
            } else {
                ThermalEffFTempElev = 1.0; // If no curve provided, assume multiplier factor = 1.0
            }

            QHeatRecToWater = FuelUseEnergyRateLHV * this->RefThermalEffLHV * ThermalEffFTempElev;
            Real64 HeatRecRateFPLR; // Heat recovery rate as a function of PLR curve output

            //     Calculate heat recovery rate modifier curve output (function of PLR)
            if (this->HeatRecRateFPLRCurveNum > 0) {
                HeatRecRateFPLR = CurveManager::CurveValue(this->HeatRecRateFPLRCurveNum, PLR);
                //       Warn user if heat recovery modifier curve output is less than 0
                if (HeatRecRateFPLR < 0.0) {
                    if (this->HeatRecRateFPLRErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("... Heat Recovery Rate Modifier (function of part-load ratio) output is less than zero (" +
                                          General::TrimSigDigits(HeatRecRateFPLR, 4) + ").");
                        ShowContinueError("... Value occurs using a part-load ratio of " + General::TrimSigDigits(PLR, 3) + '.');
                        ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        "GENERATOR:MICROTURBINE \"" + this->Name +
                            "\": Heat Recovery Rate Modifier (function of part-load ratio) output is less than zero warning continues...",
                        this->HeatRecRateFPLRErrorIndex,
                        HeatRecRateFPLR,
                        HeatRecRateFPLR);
                    HeatRecRateFPLR = 0.0;
                }
            } else {
                HeatRecRateFPLR = 1.0; // If no curve provided, assume multiplier factor = 1.0
            }

            Real64 HeatRecRateFTemp; // Heat recovery rate as a function of inlet water temp curve output

            //     Calculate heat recovery rate modifier curve output (function of inlet water temp)
            if (this->HeatRecRateFTempCurveNum > 0) {
                HeatRecRateFTemp = CurveManager::CurveValue(this->HeatRecRateFTempCurveNum, HeatRecInTemp);
                if (HeatRecRateFTemp < 0.0) {
                    if (this->HeatRecRateFTempErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("... Heat Recovery Rate Modifier (function of inlet water temp) output is less than zero (" +
                                          General::TrimSigDigits(HeatRecRateFTemp, 4) + ").");
                        ShowContinueError("... Value occurs using an inlet water temperature temperature of " +
                                          General::TrimSigDigits(HeatRecInTemp, 2) + " C.");
                        ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        "GENERATOR:MICROTURBINE \"" + this->Name +
                            "\": Heat Recovery Rate Modifier (function of inlet water temp) output is less than zero warning continues...",
                        this->HeatRecRateFTempErrorIndex,
                        HeatRecRateFTemp,
                        HeatRecRateFTemp);
                    HeatRecRateFTemp = 0.0;
                }
            } else {
                HeatRecRateFTemp = 1.0; // If no curve provided, assume multiplier factor = 1.0
            }

            Real64 HeatRecRateFFlow; // Heat recovery rate as a function of water flow rate curve output

            //     Calculate heat recovery rate modifier curve output (function of water [volumetric] flow rate)
            if (this->HeatRecRateFWaterFlowCurveNum > 0) {
                Real64 rho = FluidProperties::GetDensityGlycol(
                    DataPlant::PlantLoop(this->HRLoopNum).FluidName, HeatRecInTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, RoutineName);

                // Heat recovery fluid flow rate (m3/s)
                Real64 HeatRecVolFlowRate = heatRecMdot / rho;
                HeatRecRateFFlow = CurveManager::CurveValue(this->HeatRecRateFWaterFlowCurveNum, HeatRecVolFlowRate);
                if (HeatRecRateFFlow < 0.0) {
                    if (this->HeatRecRateFFlowErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("... Heat Recovery Rate Modifier (function of water flow rate) output is less than zero (" +
                                          General::TrimSigDigits(HeatRecRateFFlow, 4) + ").");
                        ShowContinueError("... Value occurs using a water flow rate of " + General::TrimSigDigits(HeatRecVolFlowRate, 4) + " m3/s.");
                        ShowContinueErrorTimeStamp("... Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        "GENERATOR:MICROTURBINE \"" + this->Name +
                            "\": Heat Recovery Rate Modifier (function of water flow rate) output is less than zero warning continues...",
                        this->HeatRecRateFFlowErrorIndex,
                        HeatRecRateFFlow,
                        HeatRecRateFFlow);
                    HeatRecRateFFlow = 0.0;
                }
            } else {
                HeatRecRateFFlow = 1.0; // If no curve provided, assume multiplier factor = 1.0
            }

            QHeatRecToWater *= HeatRecRateFPLR * HeatRecRateFTemp * HeatRecRateFFlow;

            Real64 HeatRecOutTemp; // Heat recovery fluid outlet temperature (C)

            //     Check for divide by zero
            if ((heatRecMdot > 0.0) && (HeatRecCp > 0.0)) {
                HeatRecOutTemp = HeatRecInTemp + QHeatRecToWater / (heatRecMdot * HeatRecCp);
            } else {
                heatRecMdot = 0.0;
                HeatRecOutTemp = HeatRecInTemp;
                QHeatRecToWater = 0.0;
            }

            //     Now verify the maximum heat recovery temperature was not exceeded
            if (HeatRecOutTemp > this->HeatRecMaxWaterTemp) {

                Real64 MinHeatRecMdot = 0.0; // Heat recovery flow rate if minimal heat recovery is accomplished (kg/s)

                if (this->HeatRecMaxWaterTemp != HeatRecInTemp) {
                    MinHeatRecMdot = QHeatRecToWater / (HeatRecCp * (this->HeatRecMaxWaterTemp - HeatRecInTemp));
                    if (MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0;
                }

                //       Recalculate outlet water temperature with minimum flow rate (will normally match the max water outlet temp,
                //       unless the inlet water temp is greater than the max outlet temp)
                Real64 HRecRatio; // When maximum temperature is reached the amount of recovered heat has to be reduced

                if ((MinHeatRecMdot > 0.0) && (HeatRecCp > 0.0)) {
                    HeatRecOutTemp = QHeatRecToWater / (MinHeatRecMdot * HeatRecCp) + HeatRecInTemp;
                    HRecRatio = heatRecMdot / MinHeatRecMdot;
                } else {
                    HeatRecOutTemp = HeatRecInTemp;
                    HRecRatio = 0.0;
                }
                QHeatRecToWater *= HRecRatio; // Scale heat recovery rate using HRecRatio. Don't adjust flow rate.
            }

            //     Check water mass flow rate against minimum
            if (this->HeatRecMinMassFlowRate > heatRecMdot && heatRecMdot > 0.0) {
                if (this->HRMinFlowErrorIndex == 0) {
                    ShowWarningError("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                    ShowContinueError("...Heat reclaim water flow rate is below the generators minimum mass flow rate of (" +
                                      General::TrimSigDigits(this->HeatRecMinMassFlowRate, 4) + ").");
                    ShowContinueError("...Heat reclaim water mass flow rate = " + General::TrimSigDigits(heatRecMdot, 4) + '.');
                    ShowContinueErrorTimeStamp("...Check inputs for heat recovery water flow rate.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "GENERATOR:MICROTURBINE \"" + this->Name +
                        "\": Heat recovery water flow rate is below the generators minimum mass flow rate warning continues...",
                    this->HRMinFlowErrorIndex,
                    heatRecMdot,
                    heatRecMdot);
            }

            //     Check water mass flow rate against maximum
            if (heatRecMdot > this->HeatRecMaxMassFlowRate && heatRecMdot > 0.0) {
                if (this->HRMaxFlowErrorIndex == 0) {
                    ShowWarningError("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                    ShowContinueError("...Heat reclaim water flow rate is above the generators maximum mass flow rate of (" +
                                      General::TrimSigDigits(this->HeatRecMaxMassFlowRate, 4) + ").");
                    ShowContinueError("...Heat reclaim water mass flow rate = " + General::TrimSigDigits(heatRecMdot, 4) + '.');
                    ShowContinueErrorTimeStamp("...Check inputs for heat recovery water flow rate.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "GENERATOR:MICROTURBINE \"" + this->Name +
                        "\": Heat recovery water flow rate is above the generators maximum mass flow rate warning continues...",
                    this->HRMaxFlowErrorIndex,
                    heatRecMdot,
                    heatRecMdot);
            }

            //     Set report variables
            this->HeatRecInletTemp = HeatRecInTemp;
            this->HeatRecOutletTemp = HeatRecOutTemp;
            this->HeatRecMdot = heatRecMdot;
            this->QHeatRecovered = QHeatRecToWater;

        } // End of  IF (MTGenerator(GeneratorNum)%HeatRecActive) THEN

        //   Calculate combustion air outlet conditions if exhaust air calculations are active
        if (this->ExhAirCalcsActive) {

            Real64 ExhFlowFTemp; // Exhaust air flow rate as a function of temperature curve output

            if (this->ExhFlowFTempCurveNum != 0) { // Exhaust Flow Rate versus Inlet Air Temp
                ExhFlowFTemp = CurveManager::CurveValue(this->ExhFlowFTempCurveNum, CombustionAirInletTemp);
                //       Warn user if exhaust modifier curve output is less than or equal to 0
                if (ExhFlowFTemp <= 0.0) {
                    if (this->ExhFlowFTempErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("...Exhaust Air Flow Rate Modifier (function of temperature) output is less than or equal to zero (" +
                                          General::TrimSigDigits(ExhFlowFTemp, 4) + ").");
                        ShowContinueError("...Value occurs using a combustion inlet air temperature of " +
                                          General::TrimSigDigits(CombustionAirInletTemp, 2) + '.');
                        ShowContinueErrorTimeStamp("...Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        "GENERATOR:MICROTURBINE \"" + this->Name +
                            "\": Exhaust Air Flow Rate Modifier (function of temperature) output is less than or equal to zero warning continues...",
                        this->ExhFlowFTempErrorIndex,
                        ExhFlowFTemp,
                        ExhFlowFTemp);
                    ExhFlowFTemp = 0.0;
                }
            } else {
                ExhFlowFTemp = 1.0; // No curve input means modifier = 1.0 always
            }

            Real64 ExhFlowFPLR; // Exhaust air flow rate as a function of part-load ratio curve output

            if (this->ExhFlowFPLRCurveNum != 0) { // Exhaust Flow Rate versus Part-Load Ratio
                ExhFlowFPLR = CurveManager::CurveValue(this->ExhFlowFPLRCurveNum, PLR);
                //       Warn user if exhaust modifier curve output is less than or equal to 0
                if (ExhFlowFPLR <= 0.0) {
                    if (this->ExhFlowFPLRErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("...Exhaust Air Flow Rate Modifier (function of part-load ratio) output is less than or equal to zero (" +
                                          General::TrimSigDigits(ExhFlowFPLR, 4) + ").");
                        ShowContinueError("...Value occurs using a part-load ratio of " + General::TrimSigDigits(PLR, 2) + '.');
                        ShowContinueErrorTimeStamp("...Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                                       "\": Exhaust Air Flow Rate Modifier (function of part-load ratio) output is less than or "
                                                       "equal to zero warning continues...",
                                                   this->ExhFlowFPLRErrorIndex,
                                                   ExhFlowFPLR,
                                                   ExhFlowFPLR);
                    ExhFlowFPLR = 0.0;
                }
            } else {
                ExhFlowFPLR = 1.0; // No curve input means modifier = 1.0 always
            }

            //     Calculate exhaust air mass flow, accounting for temperature and PLR modifier factors
            // Actual exhaust air mass flow rate (accounting for temp and PLR modifier curves)
            Real64 ExhAirMassFlowRate = this->RefExhaustAirMassFlowRate * ExhFlowFTemp * ExhFlowFPLR;
            //     Adjust for difference in air density at reference conditions versus actual inlet air conditions

            // Density of air at actual combustion inlet air conditions (kg/m3)
            Real64 AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(CombustionAirInletPress, CombustionAirInletTemp, CombustionAirInletW);
            if (this->RefCombustAirInletDensity >= 0.0) {
                ExhAirMassFlowRate = max(0.0, ExhAirMassFlowRate * AirDensity / this->RefCombustAirInletDensity);
            } else {
                ExhAirMassFlowRate = 0.0;
            }
            this->ExhaustAirMassFlowRate = ExhAirMassFlowRate;

            Real64 ExhAirTempFTemp; // Exhaust air temperature as a function of inlet air temp curve output

            if (this->ExhAirTempFTempCurveNum != 0) { // Exhaust Air Temp versus Inlet Air Temp
                ExhAirTempFTemp = CurveManager::CurveValue(this->ExhAirTempFTempCurveNum, CombustionAirInletTemp);
                //       Warn user if exhaust modifier curve output is less than or equal to 0
                if (ExhAirTempFTemp <= 0.0) {
                    if (this->ExhTempFTempErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("...Exhaust Air Temperature Modifier (function of temperature) output is less than or equal to zero (" +
                                          General::TrimSigDigits(ExhAirTempFTemp, 4) + ").");
                        ShowContinueError("...Value occurs using a combustion inlet air temperature of " +
                                          General::TrimSigDigits(CombustionAirInletTemp, 2) + '.');
                        ShowContinueErrorTimeStamp("...Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                                       "\": Exhaust Air Temperature Modifier (function of temperature) output is less than or equal "
                                                       "to zero warning continues...",
                                                   this->ExhTempFTempErrorIndex,
                                                   ExhAirTempFTemp,
                                                   ExhAirTempFTemp);
                    ExhAirTempFTemp = 0.0;
                }
            } else {
                ExhAirTempFTemp = 1.0; // No curve input means modifier = 1.0 always
            }

            Real64 ExhAirTempFPLR; // Exhaust air temperature as a function of part-load ratio curve output

            if (this->ExhAirTempFPLRCurveNum != 0) { // Exhaust Air Temp versus Part-Load Ratio
                ExhAirTempFPLR = CurveManager::CurveValue(this->ExhAirTempFPLRCurveNum, PLR);
                //       Warn user if exhaust modifier curve output is less than or equal to 0
                if (ExhAirTempFPLR <= 0.0) {
                    if (this->ExhTempFPLRErrorIndex == 0) {
                        ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                        ShowContinueError("...Exhaust Air Temperature Modifier (function of part-load ratio) output is less than or equal to zero (" +
                                          General::TrimSigDigits(ExhAirTempFPLR, 4) + ").");
                        ShowContinueError("...Value occurs using a part-load ratio of " + General::TrimSigDigits(PLR, 2) + '.');
                        ShowContinueErrorTimeStamp("...Resetting curve output to zero and continuing simulation.");
                    }
                    ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                                       "\": Exhaust Air Temperature Modifier (function of part-load ratio) output is less than or "
                                                       "equal to zero warning continues...",
                                                   this->ExhTempFPLRErrorIndex,
                                                   ExhAirTempFPLR,
                                                   ExhAirTempFPLR);
                    ExhAirTempFPLR = 0.0;
                }
            } else {
                ExhAirTempFPLR = 1.0; // No curve input means modifier = 1.0 always
            }

            if (ExhAirMassFlowRate <= 0.0) {
                this->ExhaustAirTemperature = CombustionAirInletTemp;
                this->ExhaustAirHumRat = CombustionAirInletW;
            } else {
                //       Calculate exhaust air temperature, accounting for inlet air temperature and PLR modifier factors
                // Actual exhaust air temperature (accounting for temp and PLR modifier curves)
                Real64 ExhaustAirTemp = this->NomExhAirOutletTemp * ExhAirTempFTemp * ExhAirTempFPLR;
                this->ExhaustAirTemperature = ExhaustAirTemp;
                //       Adjust exhaust air temperature if heat recovery to water is being done
                if (QHeatRecToWater > 0.0) {
                    // Heat capacity of air (J/kg-C)
                    Real64 CpAir = Psychrometrics::PsyCpAirFnWTdb(CombustionAirInletW, CombustionAirInletTemp);
                    if (CpAir > 0.0) {
                        this->ExhaustAirTemperature = ExhaustAirTemp - QHeatRecToWater / (CpAir * ExhAirMassFlowRate);
                    }
                }
                //       Calculate exhaust air humidity ratio

                // Heat of vaporization of water (J/kg)
                Real64 H2OHtOfVap = Psychrometrics::PsyHfgAirFnWTdb(1.0, 16.0); // W not used, passing 1.0 as dummy.
                // Assume fuel is at 16C (ASHRAE HOF)
                if (H2OHtOfVap > 0.0) {
                    this->ExhaustAirHumRat = CombustionAirInletW + this->FuelMdot *
                                                                       ((fuelHigherHeatingValue - fuelLowerHeatingValue) * KJtoJ / H2OHtOfVap) /
                                                                       ExhAirMassFlowRate;
                } else {
                    this->ExhaustAirHumRat = CombustionAirInletW;
                }
            }

            if (this->ExhaustAirTemperature < CombustionAirInletTemp) {
                if (this->ExhTempLTInletTempIndex == 0) {
                    ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                    ShowContinueError(
                        "...The model has calculated the exhaust air temperature to be less than the combustion air inlet temperature.");
                    ShowContinueError("...Value of exhaust air temperature   =" + General::TrimSigDigits(this->ExhaustAirTemperature, 4) + " C.");
                    ShowContinueError("...Value of combustion air inlet temp =" + General::TrimSigDigits(CombustionAirInletTemp, 4) + " C.");
                    ShowContinueErrorTimeStamp("... Simulation will continue.");
                }
                ShowRecurringWarningErrorAtEnd("GENERATOR:MICROTURBINE \"" + this->Name +
                                                   "\": Exhaust air temperature less than combustion air inlet temperature warning continues...",
                                               this->ExhTempLTInletTempIndex,
                                               this->ExhaustAirTemperature,
                                               this->ExhaustAirTemperature);
            }

            if (this->ExhaustAirHumRat < CombustionAirInletW) {
                if (this->ExhHRLTInletHRIndex == 0) {
                    ShowWarningMessage("GENERATOR:MICROTURBINE \"" + this->Name + "\"");
                    ShowContinueError(
                        "...The model has calculated the exhaust air humidity ratio to be less than the combustion air inlet humidity ratio.");
                    ShowContinueError("...Value of exhaust air humidity ratio          =" + General::TrimSigDigits(this->ExhaustAirHumRat, 6) +
                                      " kgWater/kgDryAir.");
                    ShowContinueError("...Value of combustion air inlet humidity ratio =" + General::TrimSigDigits(CombustionAirInletW, 6) +
                                      " kgWater/kgDryAir.");
                    ShowContinueErrorTimeStamp("... Simulation will continue.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "GENERATOR:MICROTURBINE \"" + this->Name +
                        "\": Exhaust air humidity ratio less than combustion air inlet humidity ratio warning continues...",
                    this->ExhHRLTInletHRIndex,
                    this->ExhaustAirHumRat,
                    this->ExhaustAirHumRat);
            }
        }
    }

    void MTGeneratorSpecs::UpdateMTGeneratorRecords()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad/D. Shirey
        //       DATE WRITTEN   Mar 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Reporting and updating nodes if necessary.

        if (this->HeatRecActive) {
            DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
        }

        if (this->ExhAirCalcsActive) {
            DataLoopNode::Node(this->CombustionAirOutletNodeNum).MassFlowRate = this->ExhaustAirMassFlowRate;
            DataLoopNode::Node(this->CombustionAirInletNodeNum).MassFlowRate = this->ExhaustAirMassFlowRate;

            DataLoopNode::Node(this->CombustionAirOutletNodeNum).Temp = this->ExhaustAirTemperature;
            DataLoopNode::Node(this->CombustionAirOutletNodeNum).HumRat = this->ExhaustAirHumRat;
            DataLoopNode::Node(this->CombustionAirOutletNodeNum).MassFlowRateMaxAvail =
                DataLoopNode::Node(this->CombustionAirInletNodeNum).MassFlowRateMaxAvail;
            DataLoopNode::Node(this->CombustionAirOutletNodeNum).MassFlowRateMinAvail =
                DataLoopNode::Node(this->CombustionAirInletNodeNum).MassFlowRateMinAvail;
        }

        this->EnergyGen = this->ElecPowerGenerated * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->ExhaustEnergyRec = this->QHeatRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->FuelEnergyHHV = this->FuelEnergyUseRateHHV * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        if (this->FuelEnergyUseRateLHV > 0.0) {
            this->ElectricEfficiencyLHV = this->ElecPowerGenerated / this->FuelEnergyUseRateLHV;
            this->ThermalEfficiencyLHV = this->QHeatRecovered / this->FuelEnergyUseRateLHV;
        } else {
            this->ElectricEfficiencyLHV = 0.0;
            this->ThermalEfficiencyLHV = 0.0;
        }
        this->AncillaryEnergy = this->AncillaryPowerRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->StandbyEnergy = this->StandbyPowerRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    }

} // namespace MicroturbineElectricGenerator

} // namespace EnergyPlus
