// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGenerators.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/FuelCellElectricGenerator.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneratorFuelSupply.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace FuelCellElectricGenerator {

    // MODULE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August. 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates the operation of Solid oxide fuel cell Generators.

    // METHODOLOGY EMPLOYED:
    // Once the ElectricPowerManager determines that the FuelCell Generator
    // is available to meet an electric load demand, it calls SimFuelCellGenerator
    // which in turn calls the FuelCell model.
    // See DataGenerators.cc for structures and variables

    // REFERENCES:
    // IEA/ECBCS Annex 42 model specification for Solid oxide and proton exchange membrane fuel cells

    bool GetFuelCellInput(true); // When TRUE, calls subroutine to read input file.
    Array1D_bool CheckEquipName;

    void SimFuelCellGenerator(int const EP_UNUSED(GeneratorType), // type of Generator
                              std::string const &GeneratorName,   // user specified name of Generator
                              int &GeneratorIndex,
                              bool const RunFlag,  // simulate Generator when TRUE
                              Real64 const MyLoad, // demand on electric generator
                              bool const FirstHVACIteration)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   MArch 2005
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This is the Solid oxide fuel cell Generator model driver.  It
        // gets the input for the models, initializes simulation variables, call
        // the appropriate model and sets up reporting variables.

        int GenNum; // Generator number counter

        // Get Generator data from input file
        if (GetFuelCellInput) {
            GetFuelCellGeneratorInput();
            GetFuelCellInput = false;
        }

        if (GeneratorIndex == 0) {
            GenNum = UtilityRoutines::FindItemInList(GeneratorName, DataGenerators::FuelCell);
            if (GenNum == 0) ShowFatalError("SimFuelCellGenerator: Specified Generator not one of Valid FuelCell Generators " + GeneratorName);
            GeneratorIndex = GenNum;
        } else {
            GenNum = GeneratorIndex;
            if (GenNum > DataGenerators::NumFuelCellGenerators || GenNum < 1) {
                ShowFatalError("SimFuelCellGenerator: Invalid GeneratorIndex passed=" + General::TrimSigDigits(GenNum) +
                               ", Number of FuelCell Generators=" + General::TrimSigDigits(DataGenerators::NumFuelCellGenerators) + ", Generator name=" + GeneratorName);
            }
            if (CheckEquipName(GenNum)) {
                if (GeneratorName != DataGenerators::FuelCell(GenNum).Name) {
                    ShowFatalError("SimFuelCellGenerator: Invalid GeneratorIndex passed=" + General::TrimSigDigits(GenNum) +
                                   ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + DataGenerators::FuelCell(GenNum).Name);
                }
                CheckEquipName(GenNum) = false;
            }
        }

        InitFuelCellGenerators(GenNum);

        CalcFuelCellGeneratorModel(GenNum, RunFlag, MyLoad, FirstHVACIteration);

        CalcUpdateHeatRecovery(GenNum, FirstHVACIteration);

        UpdateFuelCellGeneratorRecords(RunFlag, GenNum);
    }

    void GetFuelCellGeneratorInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Brent Griffith
        //       DATE WRITTEN:    April 2005

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the FuelCell Generator models.

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        int GeneratorNum;              // Generator counter
        int NumAlphas;                 // Number of elements in the alpha array
        int NumNums;                   // Number of elements in the numeric array
        int IOStat;                    // IO Status when calling get input subroutine
        Array1D_string AlphArray(25);  // character string data
        Array1D<Real64> NumArray(200); // numeric data TODO deal with allocatable for extensible
        Array1D_bool lAlphaBlanks(25);
        static bool ErrorsFound(false); // error flag
        static bool MyOneTimeFlag(true);

        if (MyOneTimeFlag) {

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell";
            DataGenerators::NumFuelCellGenerators = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (DataGenerators::NumFuelCellGenerators <= 0) {
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            // ALLOCATE ARRAYS
            DataGenerators::FuelCell.allocate(DataGenerators::NumFuelCellGenerators); // inits handeled in derived type definitions
            CheckEquipName.dimension(DataGenerators::NumFuelCellGenerators, true);

            // first load in FuelCell names
            for (GeneratorNum = 1; GeneratorNum <= DataGenerators::NumFuelCellGenerators; ++GeneratorNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                DataGenerators::FuelCell(GeneratorNum).Name = AlphArray(1);
                DataGenerators::FuelCell(GeneratorNum).NameFCPM = AlphArray(2);
                DataGenerators::FuelCell(GeneratorNum).NameFCAirSup = AlphArray(3);
                DataGenerators::FuelCell(GeneratorNum).NameFCFuelSup = AlphArray(4);
                DataGenerators::FuelCell(GeneratorNum).NameFCWaterSup = AlphArray(5);
                DataGenerators::FuelCell(GeneratorNum).NameFCAuxilHeat = AlphArray(6);
                DataGenerators::FuelCell(GeneratorNum).NameExhaustHX = AlphArray(7);
                DataGenerators::FuelCell(GeneratorNum).NameElecStorage = AlphArray(8);
                DataGenerators::FuelCell(GeneratorNum).NameInverter = AlphArray(9);
                if (NumAlphas == 10) {
                    DataGenerators::FuelCell(GeneratorNum).NameStackCooler = AlphArray(10);
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:PowerModule";
            int NumFuelCellPMs = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFuelCellPMs <= 0) {
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            for (int FCPMNum = 1; FCPMNum <= NumFuelCellPMs; ++FCPMNum) {
                inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                              FCPMNum,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              _,
                                              lAlphaBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameFCPM);
                if (thisFuelCell > 0) { // cr9323

                    DataGenerators::FuelCell(thisFuelCell).FCPM.Name = AlphArray(1);
                    if (UtilityRoutines::SameString(AlphArray(2), "ANNEX42")) DataGenerators::FuelCell(thisFuelCell).FCPM.EffMode = DataGenerators::DirectCurveMode;
                    if (UtilityRoutines::SameString(AlphArray(2), "NORMALIZED")) DataGenerators::FuelCell(thisFuelCell).FCPM.EffMode = DataGenerators::NormalizedCurveMode;
                    if (DataGenerators::FuelCell(thisFuelCell).FCPM.EffMode == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).FCPM.EffCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                    if (DataGenerators::FuelCell(thisFuelCell).FCPM.EffCurveID == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).FCPM.NomEff = NumArray(1);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.NomPel = NumArray(2);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.NumCycles = NumArray(3);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.CyclingDegradRat = NumArray(4);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.NumRunHours = NumArray(5);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.OperateDegradRat = NumArray(6);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ThreshRunHours = NumArray(7);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.UpTranLimit = NumArray(8);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.DownTranLimit = NumArray(9);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.StartUpTime = NumArray(10) / DataGlobals::SecInHour; // convert to hours from seconds
                    DataGenerators::FuelCell(thisFuelCell).FCPM.StartUpFuel = NumArray(11);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.StartUpElectConsum = NumArray(12);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.StartUpElectProd = NumArray(13);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ShutDownTime = NumArray(14) / DataGlobals::SecInHour; // convert to hours from seconds
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ShutDownFuel = NumArray(15);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ShutDownElectConsum = NumArray(16);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ANC0 = NumArray(17);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ANC1 = NumArray(18);
                    if (UtilityRoutines::SameString(AlphArray(4), "ConstantRate")) DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::ConstantRateSkinLoss;
                    if (UtilityRoutines::SameString(AlphArray(4), "UAForProcessGasTemperature"))
                        DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::UADTSkinLoss;
                    if (UtilityRoutines::SameString(AlphArray(4), "QUADRATIC FUNCTION OF FUEL RATE"))
                        DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::QuadraticFuelNdotSkin;
                    if (DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossMode == 0) {
                        // throw error
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ZoneName = AlphArray(5);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.ZoneID = UtilityRoutines::FindItemInList(DataGenerators::FuelCell(thisFuelCell).FCPM.ZoneName, DataHeatBalance::Zone);
                    if (DataGenerators::FuelCell(thisFuelCell).FCPM.ZoneID == 0 && !lAlphaBlanks(5)) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Zone Name was not found ");
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).FCPM.RadiativeFract = NumArray(19);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.QdotSkin = NumArray(20);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.UAskin = NumArray(21);

                    DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossCurveID = CurveManager::GetCurveIndex(AlphArray(6));
                    if (DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossCurveID == 0) {
                        if (DataGenerators::FuelCell(thisFuelCell).FCPM.SkinLossMode == DataGenerators::QuadraticFuelNdotSkin) {
                            ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                            ErrorsFound = true;
                        }
                    }

                    DataGenerators::FuelCell(thisFuelCell).FCPM.NdotDilutionAir = NumArray(22);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.StackHeatLossToDilution = NumArray(23);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.DilutionInletNodeName = AlphArray(7);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.DilutionInletNode = NodeInputManager::GetOnlySingleNode(
                        AlphArray(7), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.DilutionExhaustNodeName = AlphArray(8);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.DilutionExhaustNode = NodeInputManager::GetOnlySingleNode(
                        AlphArray(8), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);

                    DataGenerators::FuelCell(thisFuelCell).FCPM.PelMin = NumArray(24);
                    DataGenerators::FuelCell(thisFuelCell).FCPM.PelMax = NumArray(25);

                    // check for other FuelCell using the same power module and fill
                    for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= DataGenerators::NumFuelCellGenerators; ++otherFuelCell) {
                        if (UtilityRoutines::SameString(DataGenerators::FuelCell(otherFuelCell).FCPM.Name, DataGenerators::FuelCell(thisFuelCell).FCPM.Name)) {
                            DataGenerators::FuelCell(otherFuelCell).FCPM = DataGenerators::FuelCell(thisFuelCell).FCPM;
                        }
                    }
                } else { // throw warning, did not find power module input
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            } // loop over NumFuelCellPMs

            GeneratorFuelSupply::GetGeneratorFuelSupplyInput();

            for (int FuelSupNum = 1; FuelSupNum <= DataGenerators::NumGeneratorFuelSups; ++FuelSupNum) {
                GeneratorFuelSupply::SetupFuelConstituentData(FuelSupNum, ErrorsFound);
            }

            // set fuel supply ID in Fuel cell structure
            for (GeneratorNum = 1; GeneratorNum <= DataGenerators::NumFuelCellGenerators; ++GeneratorNum) {
                DataGenerators::FuelCell(GeneratorNum).FuelSupNum =
                    UtilityRoutines::FindItemInList(DataGenerators::FuelCell(GeneratorNum).NameFCFuelSup, DataGenerators::FuelSupply); // Fuel Supply ID
                if (DataGenerators::FuelCell(GeneratorNum).FuelSupNum == 0) {
                    ShowSevereError("Fuel Supply Name: " + DataGenerators::FuelCell(GeneratorNum).NameFCFuelSup + " not found in " + DataGenerators::FuelCell(GeneratorNum).Name);
                    ErrorsFound = true;
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:AirSupply";
            int NumFuelCellAirSups = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFuelCellAirSups <= 0) { // Autodesk:Uninit thisFuelCell was possibly uninitialized past this condition
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            for (int FCAirSupNum = 1; FCAirSupNum <= NumFuelCellAirSups; ++FCAirSupNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, FCAirSupNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameFCAirSup);

                if (thisFuelCell > 0) {

                    DataGenerators::FuelCell(thisFuelCell).AirSup.Name = AlphArray(1);
                    DataGenerators::FuelCell(thisFuelCell).AirSup.NodeName = AlphArray(2);

                    // check the node connections
                    DataGenerators::FuelCell(thisFuelCell).AirSup.SupNodeNum = NodeInputManager::GetOnlySingleNode(
                        AlphArray(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);

                    DataGenerators::FuelCell(thisFuelCell).AirSup.BlowerPowerCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                    if (DataGenerators::FuelCell(thisFuelCell).AirSup.BlowerPowerCurveID == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Curve name was not found ");
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).AirSup.BlowerHeatLossFactor = NumArray(1);

                    if (UtilityRoutines::SameString(AlphArray(4), "AirRatiobyStoics")) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.AirSupRateMode = DataGenerators::ConstantStoicsAirRat;
                    } else if (UtilityRoutines::SameString(AlphArray(4), "QuadraticFunctionofElectricPower")) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.AirSupRateMode = DataGenerators::QuadraticFuncofPel;
                    } else if (UtilityRoutines::SameString(AlphArray(4), "QUADRATIC FUNCTION OF FUEL RATE")) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.AirSupRateMode = DataGenerators::QuadraticFuncofNdot;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).AirSup.Stoics = NumArray(2) + 1.0;

                    DataGenerators::FuelCell(thisFuelCell).AirSup.AirFuncPelCurveID = CurveManager::GetCurveIndex(AlphArray(5));
                    if ((DataGenerators::FuelCell(thisFuelCell).AirSup.AirFuncPelCurveID == 0) &&
                        (DataGenerators::FuelCell(thisFuelCell).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofPel)) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowSevereError("Curve name was not found");
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).AirSup.AirTempCoeff = NumArray(3);

                    DataGenerators::FuelCell(thisFuelCell).AirSup.AirFuncNdotCurveID = CurveManager::GetCurveIndex(AlphArray(6));
                    if ((DataGenerators::FuelCell(thisFuelCell).AirSup.AirFuncNdotCurveID == 0) &&
                        (DataGenerators::FuelCell(thisFuelCell).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofNdot)) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowSevereError("Curve name was not found");
                        ErrorsFound = true;
                    }

                    if (UtilityRoutines::SameString("RecoverBurnerInverterStorage", AlphArray(7))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverBurnInvertBatt;
                    } else if (UtilityRoutines::SameString("RecoverAuxiliaryBurner", AlphArray(7))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverAuxiliaryBurner;
                    } else if (UtilityRoutines::SameString("RecoverInverterandStorage", AlphArray(7))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverInverterBatt;
                    } else if (UtilityRoutines::SameString("RecoverInverter", AlphArray(7))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverInverter;
                    } else if (UtilityRoutines::SameString("RecoverElectricalStorage", AlphArray(7))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverBattery;
                    } else if (UtilityRoutines::SameString("NoRecovery", AlphArray(7))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::NoRecoveryOnAirIntake;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(7) + " = " + AlphArray(7));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    if (UtilityRoutines::SameString("AmbientAir", AlphArray(8))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstituentMode = DataGenerators::RegularAir;
                    } else if (UtilityRoutines::SameString("UserDefinedConstituents", AlphArray(8))) {
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstituentMode = DataGenerators::UserDefinedConstituents;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(8) + " = " + AlphArray(8));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    int NumAirConstit;

                    if (DataGenerators::FuelCell(thisFuelCell).AirSup.ConstituentMode == DataGenerators::UserDefinedConstituents) {
                        NumAirConstit = NumArray(4);
                        DataGenerators::FuelCell(thisFuelCell).AirSup.NumConstituents = NumAirConstit;

                        if (NumAirConstit > 5) {
                            ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(4) + '=' + General::RoundSigDigits(NumArray(4), 2));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                            ShowContinueError("Fuel Cell model not set up for more than 5 air constituents");
                            ErrorsFound = true;
                        }

                        for (int ConstitNum = 1; ConstitNum <= NumAirConstit; ++ConstitNum) {
                            DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitName(ConstitNum) = AlphArray(ConstitNum + 8);
                            DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract(ConstitNum) = NumArray(ConstitNum + 4);
                        }

                    } else { // regular air
                        NumAirConstit = 5;

                        DataGenerators::FuelCell(thisFuelCell).AirSup.NumConstituents = NumAirConstit;

                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitName(1) = "Nitrogen";
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract(1) = 0.7728;

                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitName(2) = "Oxygen";
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract(2) = 0.2073;

                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitName(3) = "Water";
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract(3) = 0.0104;

                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitName(4) = "Argon";
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract(4) = 0.0092;

                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitName(5) = "CarbonDioxide";
                        DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract(5) = 0.0003;
                    }

                    // check for molar fractions summing to 1.0.
                    if (std::abs(sum(DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract) - 1.0) > 0.0001) {

                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " molar fractions do not sum to 1.0");
                        ShowContinueError("..Sum was=" + General::RoundSigDigits(sum(DataGenerators::FuelCell(thisFuelCell).AirSup.ConstitMolalFract), 1));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + AlphArray(1));
                        ErrorsFound = true;
                    }

                    // check for other FuelCell using the same Air Supply module and fill
                    for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= DataGenerators::NumFuelCellGenerators; ++otherFuelCell) {
                        if (UtilityRoutines::SameString(DataGenerators::FuelCell(otherFuelCell).AirSup.Name, DataGenerators::FuelCell(thisFuelCell).AirSup.Name)) {
                            DataGenerators::FuelCell(otherFuelCell).AirSup = DataGenerators::FuelCell(thisFuelCell).AirSup;
                        }
                    }
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            for (GeneratorNum = 1; GeneratorNum <= DataGenerators::NumFuelCellGenerators; ++GeneratorNum) {
                // find molal fraction of oxygen in air supply
                int thisConstituent =
                    UtilityRoutines::FindItem("Oxygen", DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitName, DataGenerators::FuelCell(GeneratorNum).AirSup.NumConstituents);
                if (thisConstituent > 0) DataGenerators::FuelCell(GeneratorNum).AirSup.O2fraction = DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstituent);

                // Loop over air constituents and do one-time setup
                for (int i = 1; i <= DataGenerators::FuelCell(GeneratorNum).AirSup.NumConstituents; ++i) {

                    std::string thisName = DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitName(i);

                    int thisGasID = UtilityRoutines::FindItem(thisName, DataGenerators::GasPhaseThermoChemistryData, &DataGenerators::GasPropertyDataStruct::ConstituentName);

                    DataGenerators::FuelCell(GeneratorNum).AirSup.GasLibID(i) = thisGasID;
                }

                // set up gas constiuents for product gases
                DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(1) = 1; // Carbon Dioxide
                DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(2) = 2; // Nitrogen
                DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(3) = 3; // Oxygen
                DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(4) = 4; // Water
                DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(5) = 5; // Argon
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:WaterSupply";
            int NumFCWaterSups = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFCWaterSups <= 0) {
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            for (int FCWaterSupNum = 1; FCWaterSupNum <= NumFCWaterSups; ++FCWaterSupNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, FCWaterSupNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameFCWaterSup);

                if (thisFuelCell > 0) {
                    //  this is only the first instance of a FuelCell generator using this type of Water supply module
                    DataGenerators::FuelCell(thisFuelCell).WaterSup.Name = AlphArray(1);
                    DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterSupRateCurveID = CurveManager::GetCurveIndex(AlphArray(2));
                    if (DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterSupRateCurveID == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Curve name was not found ");
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).WaterSup.PmpPowerCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                    if (DataGenerators::FuelCell(thisFuelCell).WaterSup.PmpPowerCurveID == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Curve name was not found ");
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).WaterSup.PmpPowerLossFactor = NumArray(1);

                    if (UtilityRoutines::SameString("TemperatureFromAirNode", AlphArray(4))) {
                        DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformAirNode;

                        DataGenerators::FuelCell(thisFuelCell).WaterSup.NodeName = AlphArray(5);
                        DataGenerators::FuelCell(thisFuelCell).WaterSup.NodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(5),
                                                                                    ErrorsFound,
                                                                                    DataIPShortCuts::cCurrentModuleObject,
                                                                                    AlphArray(1),
                                                                                    DataLoopNode::NodeType_Air,
                                                                                    DataLoopNode::NodeConnectionType_Sensor,
                                                                                    1,
                                                                                    DataLoopNode::ObjectIsNotParent);

                    } else if (UtilityRoutines::SameString("TemperatureFromWaterNode", AlphArray(4))) {
                        DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformWaterNode;

                        DataGenerators::FuelCell(thisFuelCell).WaterSup.NodeName = AlphArray(5);
                        DataGenerators::FuelCell(thisFuelCell).WaterSup.NodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(5),
                                                                                    ErrorsFound,
                                                                                    DataIPShortCuts::cCurrentModuleObject,
                                                                                    AlphArray(1),
                                                                                    DataLoopNode::NodeType_Water,
                                                                                    DataLoopNode::NodeConnectionType_Sensor,
                                                                                    1,
                                                                                    DataLoopNode::ObjectIsNotParent);

                    } else if (UtilityRoutines::SameString("MainsWaterTemperature", AlphArray(4))) {
                        DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformMains;

                    } else if (UtilityRoutines::SameString("TemperatureFromSchedule", AlphArray(4))) {
                        DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformSchedule;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).WaterSup.SchedNum = ScheduleManager::GetScheduleIndex(AlphArray(6));
                    if ((DataGenerators::FuelCell(thisFuelCell).WaterSup.SchedNum == 0) && (DataGenerators::FuelCell(thisFuelCell).WaterSup.WaterTempMode == DataGenerators::WaterInReformSchedule)) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Schedule was not found");
                        ErrorsFound = true;
                    }

                    // check for other FuelCell using the same Water Supply module and fill
                    for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= DataGenerators::NumFuelCellGenerators; ++otherFuelCell) {
                        if (UtilityRoutines::SameString(DataGenerators::FuelCell(otherFuelCell).WaterSup.Name, DataGenerators::FuelCell(thisFuelCell).WaterSup.Name)) {
                            DataGenerators::FuelCell(otherFuelCell).WaterSup = DataGenerators::FuelCell(thisFuelCell).WaterSup;
                        }
                    }
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:AuxiliaryHeater";
            int NumFuelCellAuxilHeaters = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFuelCellAuxilHeaters <= 0) {
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            for (int FCAuxHeatNum = 1; FCAuxHeatNum <= NumFuelCellAuxilHeaters; ++FCAuxHeatNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, FCAuxHeatNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameFCAuxilHeat);

                if (thisFuelCell > 0) {
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.Name = AlphArray(1);

                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.ExcessAirRAT = NumArray(1);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.ANC0 = NumArray(2);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.ANC1 = NumArray(3);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.UASkin = NumArray(4);

                    if (UtilityRoutines::SameString("SurroundingZone", AlphArray(2))) {
                        DataGenerators::FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination = DataGenerators::SurroundingZone;
                    } else if (UtilityRoutines::SameString("AirInletForFuelCell", AlphArray(2))) {
                        DataGenerators::FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination = DataGenerators::AirInletForFC;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.ZoneName = AlphArray(3);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.ZoneID = UtilityRoutines::FindItemInList(AlphArray(3), DataHeatBalance::Zone);
                    if ((DataGenerators::FuelCell(thisFuelCell).AuxilHeat.ZoneID == 0) && (DataGenerators::FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination == DataGenerators::SurroundingZone)) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Zone name was not found ");
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.MaxPowerW = NumArray(5);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.MinPowerW = NumArray(6);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.MaxPowerkmolperSec = NumArray(7);
                    DataGenerators::FuelCell(thisFuelCell).AuxilHeat.MinPowerkmolperSec = NumArray(8);

                    // TODO finish Auxiliary heater

                    // check for other FuelCell using the same Auxiliary Heating module and fill
                    for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= DataGenerators::NumFuelCellGenerators; ++otherFuelCell) {
                        if (UtilityRoutines::SameString(DataGenerators::FuelCell(otherFuelCell).AuxilHeat.Name, DataGenerators::FuelCell(thisFuelCell).AuxilHeat.Name)) {
                            DataGenerators::FuelCell(otherFuelCell).AuxilHeat = DataGenerators::FuelCell(thisFuelCell).AuxilHeat;
                        }
                    }
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            // exhaust gas heat exchanger
            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:ExhaustGasToWaterHeatExchanger";
            int NumFCExhaustGasHXs = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
            if (NumFCExhaustGasHXs <= 0) {
                ShowWarningError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ShowContinueError("Fuel Cell model requires an " + DataIPShortCuts::cCurrentModuleObject + " object");
                ErrorsFound = true;
            }

            for (int FCHXNum = 1; FCHXNum <= NumFCExhaustGasHXs; ++FCHXNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, FCHXNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameExhaustHX);

                if (thisFuelCell > 0) {
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.Name = AlphArray(1);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterInNodeName = AlphArray(2);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterOutNodeName = AlphArray(3);
                    // find node ids for water path
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterInNode = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                     ErrorsFound,
                                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                                     AlphArray(1),
                                                                                     DataLoopNode::NodeType_Water,
                                                                                     DataLoopNode::NodeConnectionType_Inlet,
                                                                                     1,
                                                                                     DataLoopNode::ObjectIsNotParent);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterOutNode = NodeInputManager::GetOnlySingleNode(AlphArray(3),
                                                                                      ErrorsFound,
                                                                                      DataIPShortCuts::cCurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      DataLoopNode::NodeType_Water,
                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                      1,
                                                                                      DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Heat Recovery Nodes");

                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.ExhaustOutNodeName = AlphArray(4);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.ExhaustOutNode = NodeInputManager::GetOnlySingleNode(
                        AlphArray(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);

                    if (UtilityRoutines::SameString("FixedEffectiveness", AlphArray(5))) {
                        DataGenerators::FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::FixedEffectiveness;
                    } else if (UtilityRoutines::SameString("EmpiricalUAeff", AlphArray(5))) {
                        DataGenerators::FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::LMTDempiricalUAeff;
                    } else if (UtilityRoutines::SameString("FundementalUAeff", AlphArray(5))) {
                        DataGenerators::FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::LMTDfundementalUAeff;
                    } else if (UtilityRoutines::SameString("CONDENSING", AlphArray(5))) {
                        DataGenerators::FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::Condensing;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterVolumeFlowMax = NumArray(1);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.HXEffect = NumArray(2);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.hxs0 = NumArray(3);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.hxs1 = NumArray(4);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.hxs2 = NumArray(5);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.hxs3 = NumArray(6);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.hxs4 = NumArray(7);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.h0gas = NumArray(8);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.NdotGasRef = NumArray(9);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.nCoeff = NumArray(10);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.AreaGas = NumArray(11);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.h0Water = NumArray(12);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.NdotWaterRef = NumArray(13);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.mCoeff = NumArray(14);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.AreaWater = NumArray(15);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.Fadjust = NumArray(16);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.l1Coeff = NumArray(17);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.l2Coeff = NumArray(18);
                    DataGenerators::FuelCell(thisFuelCell).ExhaustHX.CondensationThresholdTemp = NumArray(19);

                    // store cooling water volume flow rate for autosizing system
                    PlantUtilities::RegisterPlantCompDesignFlow(DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterInNode, DataGenerators::FuelCell(thisFuelCell).ExhaustHX.WaterVolumeFlowMax);
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:ElectricalStorage";
            int NumFCElecStorageUnits = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFCElecStorageUnits <= 0) {
                ShowWarningError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ShowContinueError("Fuel Cell model requires an " + DataIPShortCuts::cCurrentModuleObject + " object");
                ErrorsFound = true;
            }

            for (int StorageNum = 1; StorageNum <= NumFCElecStorageUnits; ++StorageNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, StorageNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameElecStorage);

                if (thisFuelCell > 0) {
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.Name = AlphArray(1);

                    if (UtilityRoutines::SameString(AlphArray(2), "SimpleEfficiencyWithConstraints")) {
                        DataGenerators::FuelCell(thisFuelCell).ElecStorage.StorageModelMode = DataGenerators::SimpleEffConstraints;
                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.EnergeticEfficCharge = NumArray(1);
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.EnergeticEfficDischarge = NumArray(2);
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.NominalEnergyCapacity = NumArray(3);
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.MaxPowerDraw = NumArray(4);
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.MaxPowerStore = NumArray(5);
                    DataGenerators::FuelCell(thisFuelCell).ElecStorage.StartingEnergyStored = NumArray(6);

                    // check for other FuelCell using the same Electrical Storage and fill
                    for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= DataGenerators::NumFuelCellGenerators; ++otherFuelCell) {
                        if (UtilityRoutines::SameString(DataGenerators::FuelCell(otherFuelCell).ElecStorage.Name, DataGenerators::FuelCell(thisFuelCell).ElecStorage.Name)) {
                            DataGenerators::FuelCell(otherFuelCell).ElecStorage = DataGenerators::FuelCell(thisFuelCell).ElecStorage;
                        }
                    }
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:Inverter";
            int NumFCPowerCondUnits = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFCPowerCondUnits <= 0) {
                ShowWarningError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ShowContinueError("Fuel Cell model requires a " + DataIPShortCuts::cCurrentModuleObject + " object");

                ErrorsFound = true;
            }

            for (int FCPCUNum = 1; FCPCUNum <= NumFCPowerCondUnits; ++FCPCUNum) {
                inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, FCPCUNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameInverter);

                if (thisFuelCell > 0) {
                    DataGenerators::FuelCell(thisFuelCell).Inverter.Name = AlphArray(1);

                    if (UtilityRoutines::SameString(AlphArray(2), "QUADRATIC")) DataGenerators::FuelCell(thisFuelCell).Inverter.EffMode = DataGenerators::InverterEffQuadratic;
                    if (UtilityRoutines::SameString(AlphArray(2), "Constant")) DataGenerators::FuelCell(thisFuelCell).Inverter.EffMode = DataGenerators::InverterEffConstant;
                    if (DataGenerators::FuelCell(thisFuelCell).Inverter.EffMode == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }

                    DataGenerators::FuelCell(thisFuelCell).Inverter.ConstEff = NumArray(1);

                    DataGenerators::FuelCell(thisFuelCell).Inverter.EffQuadraticCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                    if ((DataGenerators::FuelCell(thisFuelCell).Inverter.EffQuadraticCurveID == 0) &&
                        (DataGenerators::FuelCell(thisFuelCell).Inverter.EffMode == DataGenerators::InverterEffQuadratic)) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Curve was not found ");
                        ErrorsFound = true;
                    }

                    // check for other FuelCell using the same Inverter and fill
                    for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= DataGenerators::NumFuelCellGenerators; ++otherFuelCell) {
                        if (UtilityRoutines::SameString(DataGenerators::FuelCell(otherFuelCell).Inverter.Name, DataGenerators::FuelCell(thisFuelCell).Inverter.Name)) {
                            DataGenerators::FuelCell(otherFuelCell).Inverter = DataGenerators::FuelCell(thisFuelCell).Inverter;
                        }
                    }
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell:StackCooler";
            int NumFCStackCoolers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumFCStackCoolers > 0) { // get stack cooler input data
                for (int FCScoolNum = 1; FCScoolNum <= NumFCStackCoolers; ++FCScoolNum) {
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  FCScoolNum,
                                                  AlphArray,
                                                  NumAlphas,
                                                  NumArray,
                                                  NumNums,
                                                  IOStat,
                                                  _,
                                                  _,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);
                    UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                    int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), DataGenerators::FuelCell, &DataGenerators::FCDataStruct::NameStackCooler);

                    if (thisFuelCell > 0) {
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.Name = AlphArray(1);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.WaterInNodeName = AlphArray(2);

                        DataGenerators::FuelCell(thisFuelCell).StackCooler.WaterOutNodeName = AlphArray(3);

                        DataGenerators::FuelCell(thisFuelCell).StackCooler.WaterInNode = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                           ErrorsFound,
                                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                                           AlphArray(1),
                                                                                           DataLoopNode::NodeType_Water,
                                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                                           1,
                                                                                           DataLoopNode::ObjectIsNotParent);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.WaterOutNode = NodeInputManager::GetOnlySingleNode(AlphArray(3),
                                                                                            ErrorsFound,
                                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeType_Water,
                                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                                            1,
                                                                                            DataLoopNode::ObjectIsNotParent);
                        BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Heat Recovery Nodes");

                        DataGenerators::FuelCell(thisFuelCell).StackCooler.TstackNom = NumArray(1);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.TstackActual = NumArray(2);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.r0 = NumArray(3);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.r1 = NumArray(4);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.r2 = NumArray(5);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.r3 = NumArray(6);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.MdotStackCoolant = NumArray(7);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.UAs_cool = NumArray(8);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.Fs_cogen = NumArray(9);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.As_cogen = NumArray(10);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.MdotCogenNom = NumArray(11);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.hCogenNom = NumArray(12);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.ns = NumArray(13);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.PstackPumpEl = NumArray(14);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.PmpPowerLossFactor = NumArray(15);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.f0 = NumArray(16);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.f1 = NumArray(17);
                        DataGenerators::FuelCell(thisFuelCell).StackCooler.f1 = NumArray(18);

                        DataGenerators::FuelCell(thisFuelCell).StackCooler.StackCoolerPresent = true;

                    } else {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                }
            }

            if (ErrorsFound) {
                ShowFatalError("Errors found in getting input for fuel cell model ");
            }

            for (GeneratorNum = 1; GeneratorNum <= DataGenerators::NumFuelCellGenerators; ++GeneratorNum) {
                SetupOutputVariable("Generator Produced Electric Power",
                                    OutputProcessor::Unit::W,
                                    DataGenerators::FuelCell(GeneratorNum).Report.ACPowerGen,
                                    "System",
                                    "Average",
                                    DataGenerators::FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Produced Electric Energy",
                                    OutputProcessor::Unit::J,
                                    DataGenerators::FuelCell(GeneratorNum).Report.ACEnergyGen,
                                    "System",
                                    "Sum",
                                    DataGenerators::FuelCell(GeneratorNum).Name,
                                    _,
                                    "ElectricityProduced",
                                    "COGENERATION",
                                    _,
                                    "Plant");
                SetupOutputVariable("Generator Produced Thermal Rate",
                                    OutputProcessor::Unit::W,
                                    DataGenerators::FuelCell(GeneratorNum).Report.qHX,
                                    "System",
                                    "Average",
                                    DataGenerators::FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Produced Thermal Energy",
                                    OutputProcessor::Unit::J,
                                    DataGenerators::FuelCell(GeneratorNum).Report.HXenergy,
                                    "System",
                                    "Sum",
                                    DataGenerators::FuelCell(GeneratorNum).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "COGENERATION",
                                    _,
                                    "Plant");

                SetupOutputVariable("Generator Fuel HHV Basis Energy",
                                    OutputProcessor::Unit::J,
                                    DataGenerators::FuelCell(GeneratorNum).Report.FuelEnergyHHV,
                                    "System",
                                    "Sum",
                                    DataGenerators::FuelCell(GeneratorNum).Name,
                                    _,
                                    "Gas",
                                    "COGENERATION",
                                    _,
                                    "Plant");
                SetupOutputVariable("Generator Fuel HHV Basis Rate",
                                    OutputProcessor::Unit::W,
                                    DataGenerators::FuelCell(GeneratorNum).Report.FuelEnergyUseRateHHV,
                                    "System",
                                    "Average",
                                    DataGenerators::FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Zone Sensible Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    DataGenerators::FuelCell(GeneratorNum).Report.SkinLossPower,
                                    "System",
                                    "Average",
                                    DataGenerators::FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Zone Sensible Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    DataGenerators::FuelCell(GeneratorNum).Report.SkinLossEnergy,
                                    "System",
                                    "Sum",
                                    DataGenerators::FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Zone Convection Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    DataGenerators::FuelCell(GeneratorNum).Report.SkinLossConvect,
                                    "System",
                                    "Average",
                                    DataGenerators::FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Zone Radiation Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    DataGenerators::FuelCell(GeneratorNum).Report.SkinLossRadiat,
                                    "System",
                                    "Average",
                                    DataGenerators::FuelCell(GeneratorNum).Name);
                if (DataGenerators::FuelCell(GeneratorNum).FCPM.ZoneID > 0) {
                    SetupZoneInternalGain(DataGenerators::FuelCell(GeneratorNum).FCPM.ZoneID,
                                          "Generator:FuelCell",
                                          DataGenerators::FuelCell(GeneratorNum).Name,
                                          DataHeatBalance::IntGainTypeOf_GeneratorFuelCell,
                                          DataGenerators::FuelCell(GeneratorNum).Report.SkinLossConvect,
                                          _,
                                          DataGenerators::FuelCell(GeneratorNum).Report.SkinLossRadiat);
                }

                if (DataGlobals::DisplayAdvancedReportVariables) { // show extra data originally needed for detailed comparative testing
                    SetupOutputVariable("Generator Air Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TairInlet,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Power Module Entering Air Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TairIntoFCPM,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Air Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotAir,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Power Module Entering Air Enthalpy",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TotAirInEnthalphy,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Blower Electric Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.BlowerPower,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Blower Electric Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.BlowerEnergy,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Blower Skin Heat Loss Rate",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.BlowerSkinLoss,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Fuel Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TfuelInlet,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Power Module Entering Fuel Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TfuelIntoFCPM,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotFuel,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Consumption LHV Basis Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.FuelEnergyLHV,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Consumption Rate LHV Basis",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.FuelEnergyUseRateLHV,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Power Module Entering Fuel Enthalpy",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TotFuelInEnthalpy,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Compressor Electric Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.FuelCompressPower,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Compressor Electric Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.FuelCompressEnergy,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Compressor Skin Heat Loss Rate",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.FuelCompressSkinLoss,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Fuel Reformer Water Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TwaterInlet,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Power Module Entering Reforming Water Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TwaterIntoFCPM,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Reformer Water Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotWater,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Reformer Water Pump Electric Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.WaterPumpPower,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Fuel Reformer Water Pump Electric Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.WaterPumpEnergy,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Power Module Entering Reforming Water Enthalpy",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.WaterIntoFCPMEnthalpy,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Product Gas Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.TprodGas,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas Enthalpy",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.EnthalProdGas,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotProdGas,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas Ar Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotProdAr,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas CO2 Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotProdCO2,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas H2O Vapor Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotProdH2O,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas N2 Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotProdN2,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Product Gas O2 Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.NdotProdO2,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Heat Recovery Exit Gas Temperature",
                                        OutputProcessor::Unit::C,
                                        DataGenerators::FuelCell(GeneratorNum).Report.THXexh,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Heat Recovery Exit Gas H2O Vapor Fraction",
                                        OutputProcessor::Unit::None,
                                        DataGenerators::FuelCell(GeneratorNum).Report.WaterVaporFractExh,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Heat Recovery Water Condensate Molar Flow Rate",
                                        OutputProcessor::Unit::kmol_s,
                                        DataGenerators::FuelCell(GeneratorNum).Report.CondensateRate,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Inverter Loss Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.PCUlosses,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Produced DC Electric Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.DCPowerGen,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator DC Power Efficiency",
                                        OutputProcessor::Unit::None,
                                        DataGenerators::FuelCell(GeneratorNum).Report.DCPowerEff,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Electric Storage Charge State",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.ElectEnergyinStorage,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name); //? 'Sum'
                    SetupOutputVariable("Generator DC Storage Charging Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.StoredPower,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator DC Storage Charging Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.StoredEnergy,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator DC Storage Discharging Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.DrawnPower,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator DC Storage Discharging Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.DrawnEnergy,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Ancillary AC Electric Power",
                                        OutputProcessor::Unit::W,
                                        DataGenerators::FuelCell(GeneratorNum).Report.ACancillariesPower,
                                        "System",
                                        "Average",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Ancillary AC Electric Energy",
                                        OutputProcessor::Unit::J,
                                        DataGenerators::FuelCell(GeneratorNum).Report.ACancillariesEnergy,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);

                    SetupOutputVariable("Generator Fuel Cell Model Iteration Count",
                                        OutputProcessor::Unit::None,
                                        DataGenerators::FuelCell(GeneratorNum).Report.SeqSubstIterations,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                    SetupOutputVariable("Generator Root Solver Iteration Count",
                                        OutputProcessor::Unit::None,
                                        DataGenerators::FuelCell(GeneratorNum).Report.RegulaFalsiIterations,
                                        "System",
                                        "Sum",
                                        DataGenerators::FuelCell(GeneratorNum).Name);
                }
            }

            MyOneTimeFlag = false;
        }
    }

    void CalcFuelCellGeneratorModel(int const GeneratorNum, // Generator number
                                    bool const RunFlag,     // TRUE when Generator operating
                                    Real64 const MyLoad,    // Generator demand
                                    bool const EP_UNUSED(FirstHVACIteration))
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED     na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a FuelCell generator using the Annex 42 model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data:
        // many subdomains such as fuel and air compressors, wa

        // REFERENCES: IEA/ECBCS Annex 42....

        Array1D<Real64> Par(3); // parameters passed in to SolveRoot

        // begin controls block to be moved out to GeneratorDynamics module
        // If no loop demand or Generator OFF, return
        if (!RunFlag) {

            // TODO zero out terms as appropriate

            if (DataGenerators::FuelCell(GeneratorNum).FCPM.HasBeenOn) {
                // FuelCell just now beginning to shut down,

                // set Day and Time of Last Shut Down
                DataGenerators::FuelCell(GeneratorNum).FCPM.FractionalDayofLastShutDown =
                    double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;
                DataGenerators::FuelCell(GeneratorNum).FCPM.HasBeenOn = false;

                if (DataGenerators::FuelCell(GeneratorNum).FCPM.ShutDownTime > 0.0) DataGenerators::FuelCell(GeneratorNum).FCPM.DuringShutDown = true;
            }

            // TODO  check to see if still in shut down mode and using fuel.
            if (DataGenerators::FuelCell(GeneratorNum).FCPM.DuringShutDown) {
            }

            return;
        }

        if (!DataGenerators::FuelCell(GeneratorNum).FCPM.HasBeenOn) {
            // fuel cell just turned on
            // set Day and Time of Last STart Up

            DataGenerators::FuelCell(GeneratorNum).FCPM.FractionalDayofLastStartUp =
                double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;

            DataGenerators::FuelCell(GeneratorNum).FCPM.HasBeenOn = true;
            ++DataGenerators::FuelCell(GeneratorNum).FCPM.NumCycles; // increment cycling counter

            if (DataGenerators::FuelCell(GeneratorNum).FCPM.StartUpTime > 0.0) DataGenerators::FuelCell(GeneratorNum).FCPM.DuringStartUp = true;
        }

        // TODO deal with things when jump out if not running?
        if (!RunFlag) {
            return;
        }

        // Note: MyLoad (input) is Pdemand (electrical Power requested)
        Real64 Pdemand = MyLoad;
        Real64 PacAncillariesTotal = 0.0;
        Real64 PpcuLosses = 0.0;
        Real64 Pstorage = 0.0;
        Real64 PgridExtra = 0.0;
        Real64 PoutofInverter = 0.0;
        bool ConstrainedFCPM = false;
        int SolverFlag;
        int iter;
        Real64 Pel;

        // BEGIN SEQUENTIAL SUBSTITUTION to handle a lot of inter-related calcs
        for (iter = 1; iter <= 20; ++iter) {
            if (iter > 1) {
                FigurePowerConditioningLosses(GeneratorNum, PoutofInverter, PpcuLosses);
                FigureACAncillaries(GeneratorNum, PacAncillariesTotal);
                Pdemand = MyLoad + PacAncillariesTotal + PpcuLosses;
            } else {
                // control Step 1a: Figure ancillary AC power draws
                FigureACAncillaries(GeneratorNum, PacAncillariesTotal);
                Pdemand = MyLoad + PacAncillariesTotal;
                // Control Step 1b: Calculate losses associated with Power conditioning
                FigurePowerConditioningLosses(GeneratorNum, Pdemand, PpcuLosses);
                Pdemand += PpcuLosses;
                Pel = Pdemand;
            }

            DataGenerators::FuelCell(GeneratorNum).Inverter.PCUlosses = PpcuLosses;

            // Control step 2: adjust for transient and startup/shut down constraints

            Real64 PelDiff;
            bool ConstrainedFCPMTrans = false;
            FigureTransientConstraints(GeneratorNum, Pel, ConstrainedFCPMTrans, PelDiff);

            // Control step 3: adjust for max and min limits on Pel

            if (Pel < DataGenerators::FuelCell(GeneratorNum).FCPM.PelMin) {
                PelDiff += (DataGenerators::FuelCell(GeneratorNum).FCPM.PelMin - Pel);
                Pel = DataGenerators::FuelCell(GeneratorNum).FCPM.PelMin;

                ConstrainedFCPM = true;
            }
            if (Pel > DataGenerators::FuelCell(GeneratorNum).FCPM.PelMax) {
                PelDiff += (DataGenerators::FuelCell(GeneratorNum).FCPM.PelMax - Pel);
                Pel = DataGenerators::FuelCell(GeneratorNum).FCPM.PelMax;
                ConstrainedFCPM = true;
            }
            if (ConstrainedFCPM) {
            }

            DataGenerators::FuelCell(GeneratorNum).FCPM.Pel = Pel;
            // Now calculate FC models.  return to controls and batter after

            // Calculation Step 1. Determine electrical Efficiency Eel

            Real64 Eel = 0.0;
            if (DataGenerators::FuelCell(GeneratorNum).FCPM.EffMode == DataGenerators::NormalizedCurveMode) {
                // Equation (8) in FuelCell Spec modified for normalized curve

                Eel = CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).FCPM.EffCurveID, Pel / DataGenerators::FuelCell(GeneratorNum).FCPM.NomPel) *
                      DataGenerators::FuelCell(GeneratorNum).FCPM.NomEff *
                      (1.0 - DataGenerators::FuelCell(GeneratorNum).FCPM.NumCycles * DataGenerators::FuelCell(GeneratorNum).FCPM.CyclingDegradRat) *
                      (1.0 - max((DataGenerators::FuelCell(GeneratorNum).FCPM.NumRunHours - DataGenerators::FuelCell(GeneratorNum).FCPM.ThreshRunHours), 0.0) *
                                 DataGenerators::FuelCell(GeneratorNum).FCPM.OperateDegradRat);

            } else if (DataGenerators::FuelCell(GeneratorNum).FCPM.EffMode == DataGenerators::DirectCurveMode) {
                // Equation (8) in DataGenerators::FuelCell Spec
                Eel = CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).FCPM.EffCurveID, Pel) *
                      (1.0 - DataGenerators::FuelCell(GeneratorNum).FCPM.NumCycles * DataGenerators::FuelCell(GeneratorNum).FCPM.CyclingDegradRat) *
                      (1.0 - max((DataGenerators::FuelCell(GeneratorNum).FCPM.NumRunHours - DataGenerators::FuelCell(GeneratorNum).FCPM.ThreshRunHours), 0.0) *
                                 DataGenerators::FuelCell(GeneratorNum).FCPM.OperateDegradRat);
            }

            DataGenerators::FuelCell(GeneratorNum).FCPM.Eel = Eel;
            // Calculation Step 2. Determine fuel rate

            // fuel flow rate
            Real64 NdotFuel = Pel / (Eel * DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).LHV * 1000000.0); // Eq. 10 solved for Ndot

            DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel = NdotFuel;
            if (Pel <= 0.0) {
                // TODO zero stuff before leaving
                Pel = 0.0;
                DataGenerators::FuelCell(GeneratorNum).FCPM.Pel = 0.0;
                return;
            } else {

                DataGenerators::FuelCell(GeneratorNum).FCPM.Pel = Pel;
            }

            // Calculation Step 3. Determine Air rate

            if (DataGenerators::FuelCell(GeneratorNum).AirSup.AirSupRateMode == DataGenerators::ConstantStoicsAirRat) { // MEthod 1
                // molar rate coeff working variable
                Real64 NdotO2 = DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).StoicOxygenRate * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel *
                         DataGenerators::FuelCell(GeneratorNum).AirSup.Stoics;

                DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir = NdotO2 / DataGenerators::FuelCell(GeneratorNum).AirSup.O2fraction;

            } else if (DataGenerators::FuelCell(GeneratorNum).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofPel) { // MEthod 2

                DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir = CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).AirSup.AirFuncPelCurveID, Pel) *
                                                      (1 + DataGenerators::FuelCell(GeneratorNum).AirSup.AirTempCoeff * DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoFCPM);

            } else if (DataGenerators::FuelCell(GeneratorNum).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofNdot) { // method 3
                DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir =
                    CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).AirSup.AirFuncNdotCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel) *
                    (1 + DataGenerators::FuelCell(GeneratorNum).AirSup.AirTempCoeff * DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoFCPM);
            }

            // Calculation Step 4. fuel compressor power

            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl =
                CurveManager::CurveValue(DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).CompPowerCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel);

            // calculation Step 5, Fuel Compressor (need outlet temperature)

            if (DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).FuelTempMode == DataGenerators::FuelInTempFromNode) {

                DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress = DataLoopNode::Node(DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).NodeNum).Temp;

            } else if (DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).FuelTempMode == DataGenerators::FuelInTempSchedule) {

                DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress =
                        ScheduleManager::GetCurrentScheduleValue(DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).SchedNum);
            }

            //  evaluate  heat capacity at average temperature using shomate
            Real64 Cp;              // temp Heat Capacity, used in thermochemistry units of (J/mol K)
            Real64 Tavg = (DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress + DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM) / 2.0;
            FigureFuelHeatCap(GeneratorNum, Tavg, Cp); // Cp in (J/mol K)

            // calculate a Temp of fuel out of compressor and into power module

            if (DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel <= 0.0) { // just pass through, domain probably collapsed in modeling
                DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM = DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress;
            } else {
                DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM =
                    ((1.0 - DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).CompPowerLossFactor) *
                            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl / (DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel * Cp * 1000.0)) +
                            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress; // 1000 Cp units mol-> kmol
            }
            // calc skin losses from fuel compressor
            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).QskinLoss =
                    DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).CompPowerLossFactor * DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl;

            if (DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).QskinLoss < 0.0) {
                ShowWarningError("problem in FuelSupply.QskinLoss " + General::RoundSigDigits(DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).QskinLoss, 3));
                DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).QskinLoss = 0.0;
            }

            // calculate tatal fuel enthalpy coming into power module

            // (Hmolfuel in KJ/mol)
            Real64 Hmolfuel;        // temp enthalpy of fuel mixture in KJ/mol
            FigureFuelEnthalpy(GeneratorNum, DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM, Hmolfuel);

            // units, NdotFuel in kmol/sec. Hmolfule in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            DataGenerators::FuelCell(GeneratorNum).FCPM.TotFuelInEnthalphy = Hmolfuel * 1000.0 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel * 1000.0;

            // Calculation Step 6, water compressor calculations

            // calculate water consumption

            DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater =
                CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).WaterSup.WaterSupRateCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel);

            // set inlet temp.  (could move to init)

            {
                auto const SELECT_CASE_var(DataGenerators::FuelCell(GeneratorNum).WaterSup.WaterTempMode);

                if (SELECT_CASE_var == DataGenerators::WaterInReformMains) {

                    DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress = DataEnvironment::WaterMainsTemp;

                } else if ((SELECT_CASE_var == DataGenerators::WaterInReformAirNode) || (SELECT_CASE_var == DataGenerators::WaterInReformWaterNode)) {

                    DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress = DataLoopNode::Node(DataGenerators::FuelCell(GeneratorNum).WaterSup.NodeNum).Temp;

                } else if (SELECT_CASE_var == DataGenerators::WaterInReformSchedule) {

                    DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress = ScheduleManager::GetCurrentScheduleValue(DataGenerators::FuelCell(GeneratorNum).WaterSup.SchedNum);
                }
            }

            DataGenerators::FuelCell(GeneratorNum).WaterSup.PwaterCompEl =
                CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).WaterSup.PmpPowerCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater);

            // 75.325  J/mol K Water at 0.1 MPa and 298 K, reference NIST WEBBOOK
            Real64 CpWater;         // heat capacity of water in molar units
            FigureLiquidWaterHeatCap(DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress, CpWater);

            if (DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater <= 0.0) { // just pass through, domain probably collapsed in modeling
                DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoFCPM = DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress;
            } else {

                DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoFCPM =
                    ((1 - DataGenerators::FuelCell(GeneratorNum).WaterSup.PmpPowerLossFactor) * DataGenerators::FuelCell(GeneratorNum).WaterSup.PwaterCompEl /
                     (DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater * CpWater * 1000.0)) +
                    DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress;
            }

            DataGenerators::FuelCell(GeneratorNum).WaterSup.QskinLoss =
                DataGenerators::FuelCell(GeneratorNum).WaterSup.PmpPowerLossFactor * DataGenerators::FuelCell(GeneratorNum).WaterSup.PwaterCompEl;

            if (DataGenerators::FuelCell(GeneratorNum).WaterSup.QskinLoss < 0.0) {
                // write(*,*) 'problem in WaterSup%QskinLoss ',DataGenerators::FuelCell(GeneratorNum)%WaterSup%QskinLoss
                DataGenerators::FuelCell(GeneratorNum).WaterSup.QskinLoss = 0.0;
            }

            Real64 HLiqWater;       // temp enthalpy of liquid water in KJ/mol   No Formation
            FigureLiquidWaterEnthalpy(DataGenerators::FuelCell(GeneratorNum).WaterSup.TwaterIntoFCPM, HLiqWater); // HLiqWater in KJ/mol

            DataGenerators::FuelCell(GeneratorNum).FCPM.WaterInEnthalpy = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater * HLiqWater * 1000.0 * 1000.0;

            // Calculation Step 7, Air compressor

            DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoBlower = DataLoopNode::Node(DataGenerators::FuelCell(GeneratorNum).AirSup.SupNodeNum).Temp;

            DataGenerators::FuelCell(GeneratorNum).AirSup.PairCompEl =
                CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).AirSup.BlowerPowerCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir);

            Tavg = (DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoBlower + DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoFCPM) / 2.0;

            FigureAirHeatCap(GeneratorNum, Tavg, Cp); // Cp in (J/mol K)

            // if PEMFC with stack cooler, then calculate stack cooler impacts
            if (DataGenerators::FuelCell(GeneratorNum).StackCooler.StackCoolerPresent) {

                DataGenerators::FuelCell(GeneratorNum).StackCooler.qs_cool =
                    (DataGenerators::FuelCell(GeneratorNum).StackCooler.r0 +
                     DataGenerators::FuelCell(GeneratorNum).StackCooler.r1 *
                         (DataGenerators::FuelCell(GeneratorNum).StackCooler.TstackActual - DataGenerators::FuelCell(GeneratorNum).StackCooler.TstackNom)) *
                    (1 + DataGenerators::FuelCell(GeneratorNum).StackCooler.r2 * Pel + DataGenerators::FuelCell(GeneratorNum).StackCooler.r3 * Pel * Pel) * Pel;

                DataGenerators::FuelCell(GeneratorNum).FCPM.QdotStackCool = DataGenerators::FuelCell(GeneratorNum).StackCooler.qs_cool;
            }

            // Figure heat recovery from Electrical Storage, power conditioning, and auxiliary burner

            {
                auto const SELECT_CASE_var(DataGenerators::FuelCell(GeneratorNum).AirSup.IntakeRecoveryMode);

                if (SELECT_CASE_var == DataGenerators::RecoverBurnInvertBatt) {
                    DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery = DataGenerators::FuelCell(GeneratorNum).AuxilHeat.QairIntake +
                                                                    DataGenerators::FuelCell(GeneratorNum).ElecStorage.QairIntake +
                                                                    DataGenerators::FuelCell(GeneratorNum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverAuxiliaryBurner) {
                    DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery = DataGenerators::FuelCell(GeneratorNum).AuxilHeat.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverInverterBatt) {
                    DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery =
                        DataGenerators::FuelCell(GeneratorNum).ElecStorage.QairIntake + DataGenerators::FuelCell(GeneratorNum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverInverter) {
                    DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery = DataGenerators::FuelCell(GeneratorNum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverBattery) {
                    DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery = DataGenerators::FuelCell(GeneratorNum).ElecStorage.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::NoRecoveryOnAirIntake) {
                    DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery = 0.0;
                }
            }

            if (DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir <= 0.0) { // just pass through, domain probably collapased in modeling
                DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoFCPM = DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoBlower;

            } else {
                DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoFCPM =
                    (((1 - DataGenerators::FuelCell(GeneratorNum).AirSup.BlowerHeatLossFactor) * DataGenerators::FuelCell(GeneratorNum).AirSup.PairCompEl +
                      DataGenerators::FuelCell(GeneratorNum).AirSup.QintakeRecovery) /
                     (DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir * Cp * 1000.0)) +
                    DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoBlower; // 1000 Cp units mol-> kmol
            }

            DataGenerators::FuelCell(GeneratorNum).AirSup.QskinLoss = DataGenerators::FuelCell(GeneratorNum).AirSup.BlowerHeatLossFactor * DataGenerators::FuelCell(GeneratorNum).AirSup.PairCompEl;

            if (DataGenerators::FuelCell(GeneratorNum).AirSup.QskinLoss < 0.0) {
                //   write(*,*) 'problem in AirSup%QskinLoss ', DataGenerators::FuelCell(GeneratorNum)%AirSup%QskinLoss
                ShowWarningError("problem in AirSup%QskinLoss " + General::RoundSigDigits(DataGenerators::FuelCell(GeneratorNum).AirSup.QskinLoss, 3));
                DataGenerators::FuelCell(GeneratorNum).AirSup.QskinLoss = 0.0;
            }

            Real64 Hmolair;         // temp enthalpy of air mixture in KJ/mol
            FigureAirEnthalpy(GeneratorNum, DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoFCPM, Hmolair); // (Hmolair in KJ/mol)

            // units, NdotAir in kmol/sec.; Hmolfuel in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            DataGenerators::FuelCell(GeneratorNum).FCPM.TotAirInEnthalphy = Hmolair * 1000.0 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir * 1000.0;

            // calculation Step 8, Figure Product Gases

            // figure stoic N dot for air
            Real64 NdotO2 = DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).StoicOxygenRate * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel;

            // Air in excess of match for fuel
            Real64 NdotStoicAir = NdotO2 / DataGenerators::FuelCell(GeneratorNum).AirSup.O2fraction;

            // figure excess air rate

            // Air in excess of match for fuel
            Real64 NdotExcessAir = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir - NdotStoicAir;

            if (NdotExcessAir < 0) { // can't meet stoichiometric fuel reaction

                ShowWarningError("Air flow rate into fuel cell is too low for stoichiometric fuel reaction");
                ShowContinueError("Increase air flow in GENERATOR:FC:AIR SUPPLY object:" + DataGenerators::FuelCell(GeneratorNum).AirSup.Name);
            }

            // figure CO2 and Water rate from products (coefs setup during one-time processing in gas phase library )

            // CO2 from reaction
            Real64 NdotCO2ProdGas = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).CO2ProductGasCoef;

            // Water from reaction
            Real64 NdotH20ProdGas = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).H20ProductGasCoef;

            //  set product gas constituent fractions  (assume five usual components)
            Real64 NdotCO2 = 0.0;         // temp CO2 molar rate coef product gas stream
            Real64 NdotN2 = 0.0;          // temp Nitrogen rate coef product gas stream
            Real64 Ndot02 = 0.0;          // temp Oxygen rate coef product gas stream
            Real64 NdotH20 = 0.0;         // temp Water rate coef product gas stream
            Real64 NdotAr = 0.0;          // temp Argon rate coef product gas stream

            // Product gas constituents are fixed (not a user defined thing)

            for (int thisGas = 1; thisGas <= DataGenerators::FuelCell(GeneratorNum).AirSup.NumConstituents; ++thisGas) {

                {
                    auto const SELECT_CASE_var(DataGenerators::FuelCell(GeneratorNum).AirSup.GasLibID(thisGas));

                    if (SELECT_CASE_var == 1) {
                        // all the CO2 coming in plus the new CO2 from reactions
                        NdotCO2 = NdotCO2ProdGas + DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas) * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir;

                    } else if (SELECT_CASE_var == 2) {
                        // all the nitrogen coming in
                        NdotN2 = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir * DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas);

                    } else if (SELECT_CASE_var == 3) {
                        // all the oxygen in the excess air stream
                        Ndot02 = NdotExcessAir * DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas);

                    } else if (SELECT_CASE_var == 4) {
                        // all the H20 coming in plus the new H20 from reactions and the H20 from water used in reforming
                        NdotH20 = NdotH20ProdGas + DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas) * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir;
                        //+ DataGenerators::FuelCell(GeneratorNum)%FCPM%NdotLiqwater

                    } else if (SELECT_CASE_var == 5) {
                        // all the argon coming in.
                        NdotAr = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir * DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas);

                    } else {
                    }
                }
            }

            DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas = NdotCO2 + NdotN2 + Ndot02 + NdotH20 + NdotAr;

            // now that we have the total, figure molar fractions

            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(1) = NdotCO2 / DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the nitrogen comming in
            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(2) = NdotN2 / DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the oxygen in the excess air stream
            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(3) = Ndot02 / DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the H20 comming in plus the new H20 from reactions and the H20 from water used in reforming
            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(4) = NdotH20 / DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the argon coming in.
            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(5) = NdotAr / DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // HmolProdGases KJ/mol)
            Real64 HmolProdGases;   // enthalpy of product gas mixture in KJ/mol
            FigureProductGasesEnthalpy(GeneratorNum, DataGenerators::FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM, HmolProdGases);

            // units, NdotProdGas in kmol/sec.; HmolProdGases in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            DataGenerators::FuelCell(GeneratorNum).FCPM.TotProdGasEnthalphy = HmolProdGases * 1000.0 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas * 1000.0;

            // calculation Step 9, Figure Skin lossess

            if (DataGenerators::FuelCell(GeneratorNum).FCPM.SkinLossMode == DataGenerators::ConstantRateSkinLoss) {
                // do nothing just use QdotSkin

            } else if (DataGenerators::FuelCell(GeneratorNum).FCPM.SkinLossMode == DataGenerators::UADTSkinLoss) {

                // get zone air temp
                if (DataGenerators::FuelCell(GeneratorNum).FCPM.ZoneID > 0) {
                    DataGenerators::FuelCell(GeneratorNum).FCPM.QdotSkin = DataGenerators::FuelCell(GeneratorNum).FCPM.UAskin *
                                                           (DataGenerators::FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM - DataHeatBalFanSys::ZT(DataGenerators::FuelCell(GeneratorNum).FCPM.ZoneID));
                }

            } else if (DataGenerators::FuelCell(GeneratorNum).FCPM.SkinLossMode == DataGenerators::QuadraticFuelNdotSkin) {

                DataGenerators::FuelCell(GeneratorNum).FCPM.QdotSkin = CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).FCPM.SkinLossCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel);
            }

            // calculation Step 10, AC FCPM power ancillaries

            DataGenerators::FuelCell(GeneratorNum).FCPM.PelancillariesAC =
                DataGenerators::FuelCell(GeneratorNum).FCPM.ANC0 + DataGenerators::FuelCell(GeneratorNum).FCPM.ANC1 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel;

            // calculation Step 11, Dilution air
            FigureAirEnthalpy(GeneratorNum, DataGenerators::FuelCell(GeneratorNum).AirSup.TairIntoBlower, Hmolair); // (Hmolair in KJ/mol)

            // units, NdotDilutionAir in kmol/sec.; Hmolair in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            DataGenerators::FuelCell(GeneratorNum).FCPM.DilutionAirInEnthalpy = Hmolair * 1000.0 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotDilutionAir * 1000.0;
            DataGenerators::FuelCell(GeneratorNum).FCPM.DilutionAirOutEnthalpy =
                DataGenerators::FuelCell(GeneratorNum).FCPM.DilutionAirInEnthalpy + DataGenerators::FuelCell(GeneratorNum).FCPM.StackHeatLossToDilution;

            // calculation Step 12, Calculate Reforming water out enthalpy
            Real64 HGasWater;       // temp enthalpy of gaseous water in KJ/mol  No Formation
            FigureGaseousWaterEnthalpy(DataGenerators::FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM, HGasWater);

            DataGenerators::FuelCell(GeneratorNum).FCPM.WaterOutEnthalpy = HGasWater * 1000.0 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater * 1000.0;

            // calculation Step 13, Calculate Heat balance
            //    move all terms in Equation 7 to RHS and calculate imbalance

            Real64 MagofImbalance = -DataGenerators::FuelCell(GeneratorNum).FCPM.TotFuelInEnthalphy - DataGenerators::FuelCell(GeneratorNum).FCPM.TotAirInEnthalphy -
                             DataGenerators::FuelCell(GeneratorNum).FCPM.WaterInEnthalpy - DataGenerators::FuelCell(GeneratorNum).FCPM.DilutionAirInEnthalpy -
                             DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).LHV * 1000000.0 -
                             DataGenerators::FuelCell(GeneratorNum).FCPM.PelancillariesAC + DataGenerators::FuelCell(GeneratorNum).FCPM.Pel +
                             DataGenerators::FuelCell(GeneratorNum).FCPM.TotProdGasEnthalphy + DataGenerators::FuelCell(GeneratorNum).FCPM.WaterOutEnthalpy +
                             DataGenerators::FuelCell(GeneratorNum).FCPM.QdotStackCool + DataGenerators::FuelCell(GeneratorNum).FCPM.QdotSkin +
                             DataGenerators::FuelCell(GeneratorNum).FCPM.DilutionAirOutEnthalpy;

            // Now find a new total prod Gas Enthalphy that would result in an energy balance
            // TODO check signs...
            Real64 tmpTotProdGasEnthalpy = DataGenerators::FuelCell(GeneratorNum).FCPM.TotProdGasEnthalphy - MagofImbalance;

            // solve for a new TprodGasLeavingFCPM using regula falsi method

            Real64 Acc = 0.01;     // guessing need to refine
            int MaxIter = 150;  // guessing need to refine
            SolverFlag = 0; // init
            Par(1) = double(GeneratorNum);
            Par(2) = tmpTotProdGasEnthalpy;
            Par(3) = DataGenerators::FuelCell(GeneratorNum).FCPM.NdotProdGas;
            Real64 tmpTprodGas = DataGenerators::FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM;
            General::SolveRoot(Acc, MaxIter, SolverFlag, tmpTprodGas, FuelCellProductGasEnthResidual, DataGenerators::MinProductGasTemp, DataGenerators::MaxProductGasTemp, Par);

            if (SolverFlag == -2) {

                ShowWarningError("CalcFuelCellGeneratorModel: Root Solver problem, flag = -2, check signs, all positive");
            }
            if (SolverFlag == -1) {
                ShowWarningError("CalcFuelCellGeneratorModel: Root Solver problem, flag = -1, check accuracy and iterations, did not converge");
            }
            if (SolverFlag > 0) {
                DataGenerators::FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM = tmpTprodGas;
                //  write(*,*) 'Number of Root Solver iterations: ', solverFlag
            }

            //  moved call to HeatBalanceInternalGains.   Call FigureFuelCellZoneGains(GeneratorNum)

            // Control Step 3 determine interaction with electrical storage
            // How much power is really going into inverter?
            Real64 PintoInverter = Pel + Pstorage; // Back out so we can reapply
            bool ConstrainedStorage;
            ManageElectStorInteractions(GeneratorNum, Pdemand, PpcuLosses, ConstrainedStorage, Pstorage, PgridExtra);
            PintoInverter = Pel - Pstorage;
            // refine power conditioning losses with more current power production

            if (DataGenerators::FuelCell(GeneratorNum).Inverter.EffMode == DataGenerators::InverterEffConstant) {

                PpcuLosses = (1.0 - DataGenerators::FuelCell(GeneratorNum).Inverter.ConstEff) * PintoInverter;
            }

            if (DataGenerators::FuelCell(GeneratorNum).Inverter.EffMode == DataGenerators::InverterEffQuadratic) {

                PpcuLosses = (1.0 - CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).Inverter.EffQuadraticCurveID, PintoInverter)) * PintoInverter;
            }

            PoutofInverter = PintoInverter - PpcuLosses;

            DataGenerators::FuelCell(GeneratorNum).ACPowerGen = PoutofInverter - DataGenerators::FuelCell(GeneratorNum).FCPM.PelancillariesAC -
                                                DataGenerators::FuelCell(GeneratorNum).AirSup.PairCompEl - DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl -
                                                DataGenerators::FuelCell(GeneratorNum).WaterSup.PwaterCompEl;
            DataGenerators::FuelCell(GeneratorNum).Inverter.PCUlosses = PpcuLosses;
            // model assumes air intake is drawn over power conditioner to recovery heat
            DataGenerators::FuelCell(GeneratorNum).Inverter.QairIntake = DataGenerators::FuelCell(GeneratorNum).Inverter.PCUlosses;

            CalcFuelCellAuxHeater(GeneratorNum);

            CalcFuelCellGenHeatRecovery(GeneratorNum);
            // calculation Step 11, If imbalance below threshold, then exit out of do loop.

            if ((std::abs(MagofImbalance) < std::abs(DataGenerators::ImBalanceTol * DataGenerators::FuelCell(GeneratorNum).FCPM.Pel)) && (iter > 2)) {
                break;
            }

        } // sequential substitution loop

        DataGenerators::FuelCell(GeneratorNum).FCPM.SeqSubstitIter = iter;
        DataGenerators::FuelCell(GeneratorNum).FCPM.RegulaFalsiIter = SolverFlag;
    }

    void ManageElectStorInteractions(int const Num, // Generator number, index for structure
                                     Real64 const Pdemand,
                                     Real64 const EP_UNUSED(PpcuLosses),
                                     bool &Constrained,
                                     Real64 &Pstorage,
                                     Real64 &PgridOverage // electricity that can't be stored and needs to go out
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // manage controls and calculations related to electrical storage in FuelCell model

        Real64 tmpPdraw;   // power draw from storage, working var
        Real64 tmpPcharge; // power charge to storage, working var
        bool drawing;      // true if drawing power
        bool charging;     // true if charging

        // initialize locals
        tmpPdraw = 0.0;
        tmpPcharge = 0.0;
        drawing = false;
        charging = false;
        Constrained = false;
        Pstorage = 0.0;
        PgridOverage = 0.0;

        // step 1 figure out what is desired of electrical storage system

        if (DataGenerators::FuelCell(Num).FCPM.Pel < (Pdemand)) {
            // draw from storage
            tmpPdraw = (Pdemand)-DataGenerators::FuelCell(Num).FCPM.Pel;
            drawing = true;
        }

        if (DataGenerators::FuelCell(Num).FCPM.Pel > (Pdemand)) {
            // add to storage
            tmpPcharge = DataGenerators::FuelCell(Num).FCPM.Pel - (Pdemand);
            charging = true;
        }

        //  step 2, figure out what is possible for electrical storage draws/charges

        if (charging) {

            if (DataGenerators::FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::SimpleEffConstraints) {

                if (DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge >= DataGenerators::FuelCell(Num).ElecStorage.NominalEnergyCapacity) {
                    // storage full!  no more allowed!
                    PgridOverage = tmpPcharge;
                    tmpPcharge = 0.0;
                    Constrained = true;
                }
                if (tmpPcharge > DataGenerators::FuelCell(Num).ElecStorage.MaxPowerStore) {
                    PgridOverage = tmpPcharge - DataGenerators::FuelCell(Num).ElecStorage.MaxPowerStore;
                    tmpPcharge = DataGenerators::FuelCell(Num).ElecStorage.MaxPowerStore;
                    Constrained = true;
                }

                // now add energy to storage from charging
                if ((DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge +
                     tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficCharge) <
                    DataGenerators::FuelCell(Num).ElecStorage.NominalEnergyCapacity) {

                    DataGenerators::FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge +
                        tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficCharge;
                } else { // would over charge this time step

                    tmpPcharge = (DataGenerators::FuelCell(Num).ElecStorage.NominalEnergyCapacity - DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge) /
                                 (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficCharge);
                    Constrained = true;
                    DataGenerators::FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge +
                        tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficCharge;
                }

                // losses go into QairIntake
                DataGenerators::FuelCell(Num).ElecStorage.QairIntake = tmpPcharge * (1.0 - DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficCharge);

            } else if (DataGenerators::FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterManwellMcGowan) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ");

            } else if (DataGenerators::FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterySaupe) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ");

            } else {

                // should not come here
            }

            Pstorage = tmpPcharge;

        } // charging

        if (drawing) {
            if (DataGenerators::FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::SimpleEffConstraints) {

                if (DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge <= 0.0) {
                    // storage empty  no more allowed!
                    tmpPdraw = 0.0;
                    Constrained = true;
                    drawing = false;
                }
                if (tmpPdraw > DataGenerators::FuelCell(Num).ElecStorage.MaxPowerDraw) {
                    tmpPdraw = DataGenerators::FuelCell(Num).ElecStorage.MaxPowerDraw;
                    Constrained = true;
                }

                // now take energy from storage by drawing  (amplified by energetic effic)
                if ((DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge -
                     tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficDischarge) > 0.0) {

                    DataGenerators::FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge -
                        tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficDischarge;
                } else { // would over drain storage this timestep so reduce tmpPdraw
                    tmpPdraw = DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge * DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficDischarge /
                               (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
                    DataGenerators::FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge -
                        tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficDischarge;

                    Constrained = true;
                }
                // losses go into QairIntake
                DataGenerators::FuelCell(Num).ElecStorage.QairIntake = tmpPdraw * (1.0 / DataGenerators::FuelCell(Num).ElecStorage.EnergeticEfficDischarge - 1.0);
            } else if (DataGenerators::FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterManwellMcGowan) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ");

            } else if (DataGenerators::FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterySaupe) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ");

            } else {

                // should not come here
            }

            Pstorage = -tmpPdraw;

        } // drawing

        if ((!charging) && (!drawing)) {

            DataGenerators::FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge = DataGenerators::FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge;
            DataGenerators::FuelCell(Num).ElecStorage.PelNeedFromStorage = 0.0;
            DataGenerators::FuelCell(Num).ElecStorage.PelFromStorage = 0.0;
            DataGenerators::FuelCell(Num).ElecStorage.QairIntake = 0.0;
        }

        if (Pstorage >= 0.0) {

            DataGenerators::FuelCell(Num).ElecStorage.PelIntoStorage = Pstorage;
            DataGenerators::FuelCell(Num).ElecStorage.PelFromStorage = 0.0;
        }
        if (Pstorage < 0.0) {

            DataGenerators::FuelCell(Num).ElecStorage.PelIntoStorage = 0.0;
            DataGenerators::FuelCell(Num).ElecStorage.PelFromStorage = -Pstorage;
        }
    }

    Real64 FuelCellProductGasEnthResidual(Real64 const TprodGas,    // temperature, this is "x" being searched
                                          Array1<Real64> const &Par // par(1) = Generator Number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith NREL
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Provide function for call to regula falsi search
        // Search for an product gas stream temperature that provides a
        // certain enthaply. (enthalpy is based on Shomate and can't be inverted)

        // METHODOLOGY EMPLOYED:
        // Calculates residual function for product gas enthalpy
        // calls procedure FigureProductGasesEnthalpy

        Real64 Residuum; // F(x)

        int GeneratorNum;
        Real64 thisHmolalProdGases;
        Real64 desiredHprodGases;
        Real64 NdotProdGases;

        GeneratorNum = std::floor(Par(1));
        desiredHprodGases = Par(2);
        NdotProdGases = Par(3);

        FigureProductGasesEnthalpy(GeneratorNum, TprodGas, thisHmolalProdGases);

        Residuum = (thisHmolalProdGases * NdotProdGases * 1000000.0) - desiredHprodGases;

        return Residuum;
    }

    void FigureAirHeatCap(int const GeneratorNum, // ID of generator FuelCell data structure
                          Real64 const FluidTemp, // degree C
                          Real64 &Cp              // (J/mol*K)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   August 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Cp from Shomate equations for fuel

        // METHODOLOGY EMPLOYED:
        // sum by weighting molar fractions of all Air constituents.
        // assumes mixture is sum of parts.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 tempCp;
        int thisConstit; // loop index
        int gasID;
        Real64 A;  // shomate coeff
        Real64 B;  // shomate coeff
        Real64 C;  // shomate coeff
        Real64 D;  // shomate coeff
        Real64 E;  // shomate coeff
        Real64 A1; // NASA poly coeff
        Real64 A2; // NASA poly coeff
        Real64 A3; // NASA poly coeff
        Real64 A4; // NASA poly coeff
        Real64 A5; // NASA poly coeff

        // loop through fuel constituents and sum up Cp

        // two different themodynamic curve fits might be used

        tempCp = 0.0;

        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin
        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= DataGenerators::FuelCell(GeneratorNum).AirSup.NumConstituents; ++thisConstit) {
            gasID = DataGenerators::FuelCell(GeneratorNum).AirSup.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {

                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FigureAirEnthalpy(int const GeneratorNum, // ID of generator FuelCell data structure
                           Real64 const FluidTemp, // degree C
                           Real64 &Hair            // (kJ/mol)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   August 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Enthalpy from Shomate equations for fuel

        // METHODOLOGY EMPLOYED:
        // sum by weighting molar fractions of all fuel constituents.
        // assumes mixture is sum of parts.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 tempHair;
        Real64 HairI;
        int thisConstit; // loop index
        int gasID;       // look up into Gas structure
        Real64 A;        // shomate coeff
        Real64 B;        // shomate coeff
        Real64 C;        // shomate coeff
        Real64 D;        // shomate coeff
        Real64 E;        // shomate coeff
        Real64 F;        // shomate coeff
        Real64 H;        // shomate coeff
        Real64 A1;       // NASA poly coeff
        Real64 A2;       // NASA poly coeff
        Real64 A3;       // NASA poly coeff
        Real64 A4;       // NASA poly coeff
        Real64 A5;       // NASA poly coeff
        Real64 A6;       // NASA poly coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)
        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin

        // loop through fuel constituents and sum up Cp

        tempHair = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_4_Tsho(pow_4(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= DataGenerators::FuelCell(GeneratorNum).AirSup.NumConstituents; ++thisConstit) {
            gasID = DataGenerators::FuelCell(GeneratorNum).AirSup.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;
                    F = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateF;
                    H = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateH;

                    HairI = (A * Tsho + B * pow_2_Tsho / 2.0 + C * pow_3_Tsho / 3.0 + D * pow_4_Tsho / 4.0 - E / Tsho + F - H);

                    tempHair += HairI * DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstit);
                }
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;
                    A6 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A6;

                    tempHair += (((A1 + A2 * Tkel / 2.0 + A3 * pow_2_Tkel / 3.0 + A4 * pow_3_Tkel / 4.0 + A5 * pow_4_Tkel / 5.0 + A6 / Tkel) *
                                  DataGenerators::RinKJperMolpK * Tkel) -
                                 DataGenerators::GasPhaseThermoChemistryData(gasID).StdRefMolarEnthOfForm) *
                                DataGenerators::FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstit);
                }
            }
        }

        Hair = tempHair;
    }

    void FigureFuelHeatCap(int const GeneratorNum, // ID of generator FuelCell data structure
                           Real64 const FluidTemp, // degree C
                           Real64 &Cp              // (J/mol*K)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   August 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Cp from Shomate equations for fuel

        // METHODOLOGY EMPLOYED:
        // sum by weighting molar fractions of all fuel constituents.
        // assumes mixture is sum of parts.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 tempCp;
        int thisConstit; // loop index
        int gasID;       // look up into Gas structure
        Real64 A;        // shomate coeff
        Real64 B;        // shomate coeff
        Real64 C;        // shomate coeff
        Real64 D;        // shomate coeff
        Real64 E;        // shomate coeff
        Real64 A1;       // NASA poly coeff
        Real64 A2;       // NASA poly coeff
        Real64 A3;       // NASA poly coeff
        Real64 A4;       // NASA poly coeff
        Real64 A5;       // NASA poly coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)
        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin

        // loop through fuel constituents and sum up Cp

        tempCp = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).NumConstituents; ++thisConstit) {
            gasID = DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FigureFuelEnthalpy(int const GeneratorNum, // ID of generator FuelCell data structure
                            Real64 const FluidTemp, // degree C
                            Real64 &Hfuel           // kJ/mol
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   August 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Enthalpy from Shomate equations for fuel

        // METHODOLOGY EMPLOYED:
        // sum by weighting molar fractions of all fuel constituents.
        // assumes mixture is sum of parts.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 tempHfuel;
        Real64 HfuelI;
        int thisConstit; // loop index
        int gasID;       // look up into Gas structure
        Real64 A;        // shomate coeff
        Real64 B;        // shomate coeff
        Real64 C;        // shomate coeff
        Real64 D;        // shomate coeff
        Real64 E;        // shomate coeff
        Real64 F;        // shomate coeff
        Real64 H;        // shomate coeff
        Real64 A1;       // NASA poly coeff
        Real64 A2;       // NASA poly coeff
        Real64 A3;       // NASA poly coeff
        Real64 A4;       // NASA poly coeff
        Real64 A5;       // NASA poly coeff
        Real64 A6;       // NASA poly coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)
        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin

        // loop through fuel constituents and sum up Cp

        tempHfuel = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_4_Tsho(pow_4(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).NumConstituents; ++thisConstit) {
            gasID = DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {
                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;
                    F = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateF;
                    H = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateH;

                    HfuelI = (A * Tsho + B * pow_2_Tsho / 2.0 + C * pow_3_Tsho / 3.0 + D * pow_4_Tsho / 4.0 - E / Tsho + F - H);

                    tempHfuel += HfuelI * DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).ConstitMolalFract(thisConstit);
                }
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {

                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;
                    A6 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A6;

                    tempHfuel += (((A1 + A2 * Tkel / 2.0 + A3 * pow_2_Tkel / 3.0 + A4 * pow_3_Tkel / 4.0 + A5 * pow_4_Tkel / 5.0 + A6 / Tkel) *
                                   DataGenerators::RinKJperMolpK * Tkel) -
                                  DataGenerators::GasPhaseThermoChemistryData(gasID).StdRefMolarEnthOfForm) *
                            DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).ConstitMolalFract(thisConstit);
                }
            }
        }

        Hfuel = tempHfuel;
    }

    void FigureProductGasesEnthalpy(int const GeneratorNum, // ID of generator FuelCell data structure
                                    Real64 const FluidTemp, // degree C
                                    Real64 &HProdGases      // kJ/mol
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   August 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Enthalpy from Shomate equations for gases

        // METHODOLOGY EMPLOYED:
        // sum by weighting molar fractions of all product gas constituents.
        // assumes mixture is sum of parts.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 tempHprodGases;
        int thisConstit; // loop index
        int gasID;       // look up into Gas structure
        Real64 A;        // shomate coeff
        Real64 B;        // shomate coeff
        Real64 C;        // shomate coeff
        Real64 D;        // shomate coeff
        Real64 E;        // shomate coeff
        Real64 F;        // shomate coeff
        Real64 H;        // shomate coeff
        Real64 A1;       // NASA poly coeff
        Real64 A2;       // NASA poly coeff
        Real64 A3;       // NASA poly coeff
        Real64 A4;       // NASA poly coeff
        Real64 A5;       // NASA poly coeff
        Real64 A6;       // NASA poly coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)
        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin

        // loop through fuel constituents and sum up Cp

        tempHprodGases = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_4_Tsho(pow_4(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= 5; ++thisConstit) {
            gasID = DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {
                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;
                    F = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateF;
                    H = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateH;

                    tempHprodGases += ((A * Tsho + B * pow_2_Tsho / 2.0 + C * pow_3_Tsho / 3.0 + D * pow_4_Tsho / 4.0 - E / Tsho + F - H) *
                            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(thisConstit));
                }
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;
                    A6 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A6;

                    tempHprodGases += (((A1 + A2 * Tkel / 2.0 + A3 * pow_2_Tkel / 3.0 + A4 * pow_3_Tkel / 4.0 + A5 * pow_4_Tkel / 5.0 + A6 / Tkel) *
                                        DataGenerators::RinKJperMolpK * Tkel) -
                                       DataGenerators::GasPhaseThermoChemistryData(gasID).StdRefMolarEnthOfForm) *
                            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(thisConstit);
                }
            } // gasid > 0
        }

        HProdGases = tempHprodGases;
    }

    void FigureProductGasHeatCap(int const GeneratorNum, // ID of generator FuelCell data structure
                                 Real64 const FluidTemp, // degree C
                                 Real64 &Cp              // (J/mol*K)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 tempCp;
        int thisConstit; // loop index
        int gasID;       // look up into Gas structure
        Real64 A;        // shomate coeff
        Real64 B;        // shomate coeff
        Real64 C;        // shomate coeff
        Real64 D;        // shomate coeff
        Real64 E;        // shomate coeff
        Real64 A1;       // NASA poly coeff
        Real64 A2;       // NASA poly coeff
        Real64 A3;       // NASA poly coeff
        Real64 A4;       // NASA poly coeff
        Real64 A5;       // NASA poly coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)
        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin

        // loop through fuel constituents and sum up Cp

        tempCp = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= isize(DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID); ++thisConstit) {
            gasID = DataGenerators::FuelCell(GeneratorNum).FCPM.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            DataGenerators::FuelCell(GeneratorNum).FCPM.ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FigureAuxilHeatGasHeatCap(int const GeneratorNum, // ID of generator FuelCell data structure
                                   Real64 const FluidTemp, // degree C
                                   Real64 &Cp              // (J/mol*K)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 tempCp;
        int thisConstit; // loop index
        int gasID;       // look up into Gas structure
        Real64 A;        // shomate coeff
        Real64 B;        // shomate coeff
        Real64 C;        // shomate coeff
        Real64 D;        // shomate coeff
        Real64 E;        // shomate coeff
        Real64 A1;       // NASA poly coeff
        Real64 A2;       // NASA poly coeff
        Real64 A3;       // NASA poly coeff
        Real64 A4;       // NASA poly coeff
        Real64 A5;       // NASA poly coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)
        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin

        // loop through fuel constituents and sum up Cp

        tempCp = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (thisConstit = 1; thisConstit <= isize(DataGenerators::FuelCell(GeneratorNum).AuxilHeat.GasLibID); ++thisConstit) {
            gasID = DataGenerators::FuelCell(GeneratorNum).AuxilHeat.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            DataGenerators::FuelCell(GeneratorNum).AuxilHeat.ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            DataGenerators::FuelCell(GeneratorNum).AuxilHeat.ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FigureGaseousWaterEnthalpy(Real64 const FluidTemp, // degree C
                                    Real64 &HGasWater       // kJ/mol
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   December 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Enthalpy from Shomate equations for gaseous water
        // No ethalphy of formation in this one.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 const A = 29.0373;  // shomate coeff
        Real64 const B = 10.2573;  // shomate coeff
        Real64 const C = 2.81048;  // shomate coeff
        Real64 const D = -0.95914; // shomate coeff
        Real64 const E = 0.11725;  // shomate coeff
        Real64 const F = -250.569; // shomate coeff
        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)

        HGasWater = A * Tsho + B * pow_2(Tsho) / 2.0 + C * pow_3(Tsho) / 3.0 + D * pow_4(Tsho) / 4.0 - E / Tsho + F; //- H
    }

    void FigureLiquidWaterEnthalpy(Real64 const FluidTemp, // degree C
                                   Real64 &HLiqWater       // kJ/mol
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   December 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate Enthalpy from Shomate equations for liquid water
        // No enthalpy of formation in this one

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 const A = -203.606;  // shomate coeff
        Real64 const B = 1523.29;   // shomate coeff
        Real64 const C = -3196.413; // shomate coeff
        Real64 const D = 2474.455;  // shomate coeff
        Real64 const E = 3.85533;   // shomate coeff
        Real64 const F = -256.5478; // shomate coeff

        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)

        HLiqWater = A * Tsho + B * pow_2(Tsho) / 2.0 + C * pow_3(Tsho) / 3.0 + D * pow_4(Tsho) / 4.0 - E / Tsho + F; //- H
    }

    void FigureLiquidWaterHeatCap(Real64 const FluidTemp, // degree C
                                  Real64 &Cp              // (J/mol*K)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   December 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate shomate eq. for pure liquid water

        Real64 const A = -203.606;  // shomate coeff
        Real64 const B = 1523.29;   // shomate coeff
        Real64 const C = -3196.413; // shomate coeff
        Real64 const D = 2474.455;  // shomate coeff
        Real64 const E = 3.85533;   // shomate coeff
        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0;

        Cp = A + B * Tsho + C * pow_2(Tsho) + D * pow_3(Tsho) + E / pow_2(Tsho);
    }

    void FigureACAncillaries(int const GeneratorNum, Real64 &PacAncill)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   March 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the AC ancillaries to determine Pel

        //  Using lagged values inside a sequential substitution loop
        PacAncill = 0.0;
        // sect. 5.9
        DataGenerators::FuelCell(GeneratorNum).FCPM.PelancillariesAC =
                DataGenerators::FuelCell(GeneratorNum).FCPM.ANC0 + DataGenerators::FuelCell(GeneratorNum).FCPM.ANC1 * DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel;

        // sect 6.0
        DataGenerators::FuelCell(GeneratorNum).AirSup.PairCompEl = CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).AirSup.BlowerPowerCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotAir);
        // sect 7.0
        DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl =
            CurveManager::CurveValue(DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).CompPowerCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotFuel);

        // sect. 8.0
        DataGenerators::FuelCell(GeneratorNum).WaterSup.PwaterCompEl =
            CurveManager::CurveValue(DataGenerators::FuelCell(GeneratorNum).WaterSup.PmpPowerCurveID, DataGenerators::FuelCell(GeneratorNum).FCPM.NdotLiqwater);

        PacAncill = DataGenerators::FuelCell(GeneratorNum).FCPM.PelancillariesAC + DataGenerators::FuelCell(GeneratorNum).AirSup.PairCompEl +
                DataGenerators::FuelSupply(DataGenerators::FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl + DataGenerators::FuelCell(GeneratorNum).WaterSup.PwaterCompEl;
    }

    void FigurePowerConditioningLosses(int const GeneratorNum, Real64 const Pdemand, Real64 &PpcuLosses)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate inverter losses

        using DataGenerators::FuelCell;

        Real64 lastPpcuLosses; // used in iterative solution
        int iter;
        Real64 Pel;

        if (FuelCell(GeneratorNum).Inverter.EffMode == DataGenerators::InverterEffConstant) {

            PpcuLosses = Pdemand * (1 - FuelCell(GeneratorNum).Inverter.ConstEff) / FuelCell(GeneratorNum).Inverter.ConstEff;
        }

        if (FuelCell(GeneratorNum).Inverter.EffMode == DataGenerators::InverterEffQuadratic) {

            // first use Pdemand instead of Pel to get initial estimate
            lastPpcuLosses = Pdemand * (1.0 - CurveManager::CurveValue(FuelCell(GeneratorNum).Inverter.EffQuadraticCurveID, Pdemand)) /
                             CurveManager::CurveValue(FuelCell(GeneratorNum).Inverter.EffQuadraticCurveID, Pdemand);

            for (iter = 1; iter <= 20; ++iter) { // seems like need to iterate (??) Need to investigate number and convergence success here

                Pel = Pdemand + lastPpcuLosses;

                lastPpcuLosses = (1.0 - CurveManager::CurveValue(FuelCell(GeneratorNum).Inverter.EffQuadraticCurveID, Pel)) * Pel;
            }

            PpcuLosses = lastPpcuLosses;
        }
    }

    void FigureTransientConstraints(int const GeneratorNum, // index number for accessing correct generator
                                    Real64 &Pel,            // DC power control setting for power module
                                    bool &Constrained,      // true if transient constraints kick in
                                    Real64 &PelDiff         // if constrained then this is the difference, positive
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 CurrentFractionalDay; // working var, time in decimal days
        Real64 EndingFractionalDay;  // working var, time is decimal days
        Real64 MaxPel;               // working variable for max allowed by transient constraint
        Real64 MinPel;               // working variabel for min allowed by transient constraint
        Real64 PelInput;             // hold initial value of inout var

        PelInput = Pel;

        // Check if in start up and if it still should be
        if (DataGenerators::FuelCell(GeneratorNum).FCPM.DuringStartUp) {

            // calculate time for end of start up period
            CurrentFractionalDay = double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;

            EndingFractionalDay = DataGenerators::FuelCell(GeneratorNum).FCPM.FractionalDayofLastStartUp + DataGenerators::FuelCell(GeneratorNum).FCPM.StartUpTime / DataGlobals::HoursInDay;

            if (CurrentFractionalDay > EndingFractionalDay) {
                // start up period is now over
                DataGenerators::FuelCell(GeneratorNum).FCPM.DuringStartUp = false;
            }
        }

        // Check if in shut down up and if it still should be
        if (DataGenerators::FuelCell(GeneratorNum).FCPM.DuringShutDown) {

            // calculate time for end of shut down period
            CurrentFractionalDay = double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;

            EndingFractionalDay = DataGenerators::FuelCell(GeneratorNum).FCPM.FractionalDayofLastShutDown + DataGenerators::FuelCell(GeneratorNum).FCPM.ShutDownTime / DataGlobals::HoursInDay;

            if (CurrentFractionalDay > EndingFractionalDay) {
                // start up period is now over
                DataGenerators::FuelCell(GeneratorNum).FCPM.DuringShutDown = false;
            }
        }
        // compare

        if (!(DataGenerators::FuelCell(GeneratorNum).FCPM.DuringShutDown) && !(DataGenerators::FuelCell(GeneratorNum).FCPM.DuringStartUp)) {
            // unit is neither starting or stopping and the only constraints would come from transient limits
            if (Pel > DataGenerators::FuelCell(GeneratorNum).FCPM.PelLastTimeStep) { // powering up
                MaxPel = DataGenerators::FuelCell(GeneratorNum).FCPM.PelLastTimeStep + DataGenerators::FuelCell(GeneratorNum).FCPM.UpTranLimit * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
                if (MaxPel < Pel) {
                    Pel = MaxPel;
                    Constrained = true;
                } else {
                    Constrained = false;
                }
            } else if (Pel < DataGenerators::FuelCell(GeneratorNum).FCPM.PelLastTimeStep) { // powering down
                MinPel = DataGenerators::FuelCell(GeneratorNum).FCPM.PelLastTimeStep - DataGenerators::FuelCell(GeneratorNum).FCPM.DownTranLimit * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
                if (Pel < MinPel) {
                    Pel = MinPel;
                    Constrained = true;
                } else {
                    Constrained = false;
                }
            } else { // the same
                // do nothing
                Constrained = false;
            }

        } // not in start up or shut down

        if (DataGenerators::FuelCell(GeneratorNum).FCPM.DuringStartUp) {
            // constant during start up modeling artifact
            Pel = DataGenerators::FuelCell(GeneratorNum).FCPM.StartUpElectProd / DataGenerators::FuelCell(GeneratorNum).FCPM.StartUpTime;
            Constrained = true;
        }

        if (DataGenerators::FuelCell(GeneratorNum).FCPM.DuringShutDown) {

            Pel = 0.0; // assumes no power generated during shut down
            Constrained = true;
        }

        PelDiff = 0.0;
        if (Constrained) {
            PelDiff = PelInput - Pel;
        }
    }

    void CalcFuelCellAuxHeater(int const Num) // Generator number
    {

        // not yet implemented, just pass product gases thru nul domain

        DataGenerators::FuelCell(Num).AuxilHeat.TauxMix = DataGenerators::FuelCell(Num).FCPM.TprodGasLeavingFCPM;
        DataGenerators::FuelCell(Num).AuxilHeat.NdotAuxMix = DataGenerators::FuelCell(Num).FCPM.NdotProdGas;
        DataGenerators::FuelCell(Num).AuxilHeat.ConstitMolalFract = DataGenerators::FuelCell(Num).FCPM.ConstitMolalFract;
        DataGenerators::FuelCell(Num).AuxilHeat.GasLibID = DataGenerators::FuelCell(Num).FCPM.GasLibID;
    }

    void CalcFuelCellGenHeatRecovery(int const Num) // Generator number
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Brent Griffith
        //       DATE WRITTEN:    Aug. 2005

        // PURPOSE OF THIS SUBROUTINE:
        // To perform heat recovery calculations and node updates

        // METHODOLOGY EMPLOYED:
        // model exhaust gas to water heat exchanger

        // REFERENCES: Annex 42 model documentation

        static std::string const RoutineName("CalcFuelCellGenHeatRecovery");

        Real64 eHX; // fixed effectiveness
        Real64 MdotWater(0.0);
        int inNodeNum(0);
        Real64 MWwater;
        Real64 NdotWater;
        Real64 TwaterIn;
        Real64 CpWaterMol;
        Real64 NdotGas;
        Real64 TprodGasIn;
        Real64 CpProdGasMol;
        Real64 NdotCp;
        Real64 qHX(0.0);
        Real64 UAeff;
        Real64 TauxMix;
        Real64 NdotCpWater;
        Real64 NdotCpAuxMix;
        Real64 THXexh(0.0);
        Real64 TwaterOut(0.0);
        Real64 hgas;
        Real64 hwater;
        static Real64 waterFract(0.0);
        Real64 NdotWaterVapor;
        Real64 TcondThresh;
        Real64 hxl1;
        Real64 hxl2;
        static Real64 NdotWaterCond(0.0);
        Real64 hfpwater;
        int i;

        Real64 qSens;
        Real64 qLatent;
        int loop;
        Real64 Cp;

        {
            auto const SELECT_CASE_var(DataGenerators::FuelCell(Num).ExhaustHX.HXmodelMode);

            if (SELECT_CASE_var == DataGenerators::FixedEffectiveness) { // Method 1

                eHX = DataGenerators::FuelCell(Num).ExhaustHX.HXEffect;

                inNodeNum = DataGenerators::FuelCell(Num).ExhaustHX.WaterInNode;

                MdotWater = DataGenerators::FuelCell(Num).ExhaustHX.WaterMassFlowRate;
                MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                NdotWater = MdotWater / MWwater;
                TwaterIn = DataGenerators::FuelCell(Num).ExhaustHX.WaterInletTemp;

                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);

                NdotGas = DataGenerators::FuelCell(Num).AuxilHeat.NdotAuxMix;
                TprodGasIn = DataGenerators::FuelCell(Num).AuxilHeat.TauxMix;
                FigureAuxilHeatGasHeatCap(Num, TprodGasIn, CpProdGasMol); // Cp in (J/mol*K)
                // factor of 1000.0 for kmol -> mol
                NdotCp = min(NdotGas * CpProdGasMol * 1000.0, NdotWater * CpWaterMol * 1000.0);

                qHX = eHX * NdotCp * (TprodGasIn - TwaterIn);

                THXexh = TprodGasIn - qHX / (NdotGas * CpProdGasMol * 1000.0);

                Cp = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(DataGenerators::FuelCell(Num).CWLoopNum).FluidName, TwaterIn, DataPlant::PlantLoop(DataGenerators::FuelCell(Num).CWLoopNum).FluidIndex, RoutineName);

                if (MdotWater * Cp <= 0.0) {
                    TwaterOut = TwaterIn;
                } else {
                    TwaterOut = TwaterIn + qHX / (MdotWater * Cp);
                }

            } else if (SELECT_CASE_var == DataGenerators::LMTDempiricalUAeff) { // method 2
                inNodeNum = DataGenerators::FuelCell(Num).ExhaustHX.WaterInNode;
                MdotWater = DataGenerators::FuelCell(Num).ExhaustHX.WaterMassFlowRate;
                MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                NdotWater = MdotWater / MWwater;
                NdotGas = DataGenerators::FuelCell(Num).AuxilHeat.NdotAuxMix;

                UAeff = DataGenerators::FuelCell(Num).ExhaustHX.hxs0 + DataGenerators::FuelCell(Num).ExhaustHX.hxs1 * NdotWater + DataGenerators::FuelCell(Num).ExhaustHX.hxs2 * pow_2(NdotWater) +
                        DataGenerators::FuelCell(Num).ExhaustHX.hxs3 * NdotGas + DataGenerators::FuelCell(Num).ExhaustHX.hxs4 * pow_2(NdotGas);

                TauxMix = DataGenerators::FuelCell(Num).AuxilHeat.TauxMix;
                TwaterIn = DataGenerators::FuelCell(Num).ExhaustHX.WaterInletTemp;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                // factor of 1000.0 for kmol -> mol
                NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                FigureAuxilHeatGasHeatCap(Num, TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero
                    // now evaluate Eq. 44
                    THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TauxMix +
                             ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TwaterIn;

                    TwaterOut = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - THXexh); // Eq. 42

                } else {
                    THXexh = TauxMix;
                    TwaterOut = TwaterIn;
                }
                // ENDIF

                if ((THXexh - TwaterIn) != 0.0) { // trap divide by zero
                    qHX = UAeff * ((TauxMix - TwaterOut) - (THXexh - TwaterIn)) / std::log((TauxMix - TwaterOut) / (THXexh - TwaterIn));
                } else {
                    qHX = 0.0;
                }

            } else if (SELECT_CASE_var == DataGenerators::LMTDfundementalUAeff) { // method 3
                NdotGas = DataGenerators::FuelCell(Num).AuxilHeat.NdotAuxMix;
                inNodeNum = DataGenerators::FuelCell(Num).ExhaustHX.WaterInNode;
                MdotWater = DataGenerators::FuelCell(Num).ExhaustHX.WaterMassFlowRate;
                MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                NdotWater = MdotWater / MWwater;

                hgas =
                        DataGenerators::FuelCell(Num).ExhaustHX.h0gas * std::pow(NdotGas / DataGenerators::FuelCell(Num).ExhaustHX.NdotGasRef, DataGenerators::FuelCell(Num).ExhaustHX.nCoeff); // Eq. 48

                hwater = DataGenerators::FuelCell(Num).ExhaustHX.h0Water *
                         std::pow(NdotWater / DataGenerators::FuelCell(Num).ExhaustHX.NdotWaterRef, DataGenerators::FuelCell(Num).ExhaustHX.mCoeff); // Eq. 48

                // now equation 47
                UAeff = 1.0 / (1.0 / (hgas * DataGenerators::FuelCell(Num).ExhaustHX.AreaGas) + 1.0 / (hwater * DataGenerators::FuelCell(Num).ExhaustHX.AreaWater) +
                        DataGenerators::FuelCell(Num).ExhaustHX.Fadjust);

                TauxMix = DataGenerators::FuelCell(Num).AuxilHeat.TauxMix;
                TwaterIn = DataGenerators::FuelCell(Num).ExhaustHX.WaterInletTemp;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                FigureAuxilHeatGasHeatCap(Num, TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero
                    // now evaluate Eq. 44
                    THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TauxMix +
                             ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TwaterIn;

                    TwaterOut = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - THXexh); // Eq. 42

                } else {
                    THXexh = TauxMix;
                    TwaterOut = TwaterIn;
                }

                if ((THXexh - TwaterIn) != 0.0) { // trap divide by zero
                    qHX = UAeff * ((TauxMix - TwaterOut) - (THXexh - TwaterIn)) / std::log((TauxMix - TwaterOut) / (THXexh - TwaterIn));
                } else {
                    qHX = 0.0;
                }

            } else if (SELECT_CASE_var == DataGenerators::Condensing) { // method 4
                inNodeNum = DataGenerators::FuelCell(Num).ExhaustHX.WaterInNode;
                MdotWater = DataGenerators::FuelCell(Num).ExhaustHX.WaterMassFlowRate;
                if (MdotWater != 0.0) {

                    MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                    NdotWater = MdotWater / MWwater;
                    NdotGas = DataGenerators::FuelCell(Num).AuxilHeat.NdotAuxMix;

                    UAeff = DataGenerators::FuelCell(Num).ExhaustHX.hxs0 + DataGenerators::FuelCell(Num).ExhaustHX.hxs1 * NdotWater +
                            DataGenerators::FuelCell(Num).ExhaustHX.hxs2 * pow_2(NdotWater) + DataGenerators::FuelCell(Num).ExhaustHX.hxs3 * NdotGas +
                            DataGenerators::FuelCell(Num).ExhaustHX.hxs4 * pow_2(NdotGas);

                    TauxMix = DataGenerators::FuelCell(Num).AuxilHeat.TauxMix;
                    TwaterIn = DataGenerators::FuelCell(Num).ExhaustHX.WaterInletTemp;
                    FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                    NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                    FigureAuxilHeatGasHeatCap(Num, TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                    NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                    // find water fraction in incoming gas stream
                    for (i = 1; i <= isize(DataGenerators::FuelCell(Num).AuxilHeat.GasLibID); ++i) {
                        if (DataGenerators::FuelCell(Num).AuxilHeat.GasLibID(i) == 4) waterFract = DataGenerators::FuelCell(Num).AuxilHeat.ConstitMolalFract(i);
                    }
                    NdotWaterVapor = waterFract * NdotGas;

                    TcondThresh = DataGenerators::FuelCell(Num).ExhaustHX.CondensationThresholdTemp;
                    hxl1 = DataGenerators::FuelCell(Num).ExhaustHX.l1Coeff;
                    hxl2 = DataGenerators::FuelCell(Num).ExhaustHX.l2Coeff;

                    NdotWaterCond = (TcondThresh - TwaterIn) * (hxl1 * (NdotWaterVapor / NdotGas) + hxl2 * pow_2(NdotWaterVapor / NdotGas));

                    if (NdotWaterCond < 0.0) NdotWaterCond = 0.0;

                    hfpwater = 4.4004e+07; // molal heat of vaporization of water J/kmol)

                    if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero

                        // now evaluate Eq. 44
                        THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                                  (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                     TauxMix +
                                 ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                                  (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                     TwaterIn;

                        TwaterOut = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - THXexh) + (NdotWaterCond * hfpwater) / NdotCpWater;

                        if (NdotWaterCond > 0) { // Eq. 44 is not correct. use its result as first guess for revised way...

                            for (loop = 1; loop <= 5;
                                 ++loop) { // iterative soluion because in condensing case THXexh is function of qSens and qLatent

                                if ((THXexh - TwaterIn) != 0.0 &&
                                    ((TauxMix - TwaterOut) / (THXexh - TwaterIn) > 0.0001)) { // trap divide by zero and negative log
                                    qSens =
                                        UAeff * ((TauxMix - TwaterOut) - (THXexh - TwaterIn)) / std::log((TauxMix - TwaterOut) / (THXexh - TwaterIn));
                                } else {
                                    qSens = 0.0;
                                }
                                qLatent = NdotWaterCond * hfpwater;
                                if (qSens > 0) {
                                    THXexh =
                                        TauxMix * ((1.0 - NdotCpAuxMix / NdotCpWater) / ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) /
                                                                                          (std::exp((UAeff * qLatent) / (NdotCpWater * qSens)))) -
                                                                                         NdotCpAuxMix / NdotCpWater)) +
                                        TwaterIn * ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) /
                                                         (std::exp((UAeff * qLatent) / (NdotCpWater * qSens))) -
                                                     1.0) /
                                                    (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) /
                                                         (std::exp((UAeff * qLatent) / (NdotCpWater * qSens))) -
                                                     NdotCpAuxMix / NdotCpWater)) -
                                        ((qLatent / NdotCpWater) / (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) /
                                                                        (std::exp((UAeff * qLatent) / (NdotCpWater * qSens))) -
                                                                    NdotCpAuxMix / NdotCpWater));
                                } else {
                                    THXexh = TauxMix;
                                }

                                TwaterOut = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - THXexh) + (NdotWaterCond * hfpwater) / NdotCpWater;
                            }
                        }

                    } else {
                        THXexh = TauxMix;
                        TwaterOut = TwaterIn;
                    }

                    if ((THXexh - TwaterIn) != 0.0 &&
                        ((TauxMix - TwaterOut) / (THXexh - TwaterIn) > 0.0001)) { // trap divide by zero and negative log

                        qHX = UAeff * ((TauxMix - TwaterOut) - (THXexh - TwaterIn)) / std::log((TauxMix - TwaterOut) / (THXexh - TwaterIn)) +
                              NdotWaterCond * hfpwater;
                    } else {
                        qHX = 0.0;
                    }
                } else { // no cooling water flow, model will blow up.
                    qHX = 0.0;
                    THXexh = DataGenerators::FuelCell(Num).AuxilHeat.TauxMix;
                    TwaterOut = DataGenerators::FuelCell(Num).ExhaustHX.WaterInletTemp;
                    NdotWaterCond = 0.0;
                    waterFract = -9999.0; // not defined
                }
            } else {
                assert(false); // Variables not set are used below
            }
        }

        // update results in data structure.
        DataGenerators::FuelCell(Num).ExhaustHX.qHX = qHX;
        DataGenerators::FuelCell(Num).ExhaustHX.THXexh = THXexh;
        DataGenerators::FuelCell(Num).ExhaustHX.WaterMassFlowRate = MdotWater;
        DataGenerators::FuelCell(Num).ExhaustHX.WaterVaporFractExh = waterFract;

        DataGenerators::FuelCell(Num).ExhaustHX.CondensateRate = NdotWaterCond;
        DataGenerators::FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterOut;
        DataGenerators::FuelCell(Num).ExhaustHX.WaterOutletEnthalpy = DataLoopNode::Node(inNodeNum).Enthalpy + qHX;
    }

    void SimFuelCellPlantHeatRecovery(std::string const &EP_UNUSED(CompType),
                                      std::string const &CompName,
                                      int const CompTypeNum,
                                      int &CompNum,
                                      bool const EP_UNUSED(RunFlag),
                                      bool &InitLoopEquip,
                                      Real64 &EP_UNUSED(MyLoad), // unused1208
                                      Real64 &MaxCap,
                                      Real64 &MinCap,
                                      Real64 &OptCap,
                                      bool const FirstHVACIteration // TRUE if First iteration of simulation
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Jan 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // makes sure input are gotten and setup from Plant loop perspective.
        // does not (re)simulate entire FuelCell model

        using DataGenerators::FuelCell;
        using DataGenerators::FCDataStruct;

        if (GetFuelCellInput) {

            // Read input data.
            GetFuelCellGeneratorInput();
            GetFuelCellInput = false;
        }

        if (InitLoopEquip) {
            if (CompTypeNum == DataPlant::TypeOf_Generator_FCExhaust) {
                CompNum = UtilityRoutines::FindItemInList(CompName, FuelCell, &FCDataStruct::NameExhaustHX);
            } else if (CompTypeNum == DataPlant::TypeOf_Generator_FCStackCooler) {
                CompNum = UtilityRoutines::FindItemInList(CompName, FuelCell, &FCDataStruct::NameStackCooler);
            }
            if (CompNum == 0) {
                ShowFatalError("SimFuelCellPlantHeatRecovery: Fuel Cell Generator Unit not found=" + CompName);
            }
            MinCap = 0.0;
            MaxCap = 0.0;
            OptCap = 0.0;
            return;
        } // End Of InitLoopEquip

        if (CompTypeNum == DataPlant::TypeOf_Generator_FCStackCooler) {
            PlantUtilities::UpdateComponentHeatRecoverySide(FuelCell(CompNum).CWLoopNum,
                                            FuelCell(CompNum).CWLoopSideNum,
                                            DataPlant::TypeOf_Generator_FCStackCooler,
                                            FuelCell(CompNum).StackCooler.WaterInNode,
                                            FuelCell(CompNum).StackCooler.WaterOutNode,
                                            FuelCell(CompNum).Report.qHX,
                                            FuelCell(CompNum).Report.HeatRecInletTemp,
                                            FuelCell(CompNum).Report.HeatRecOutletTemp,
                                            FuelCell(CompNum).Report.HeatRecMdot,
                                            FirstHVACIteration);
        } else if (CompTypeNum == DataPlant::TypeOf_Generator_FCExhaust) {
            PlantUtilities::UpdateComponentHeatRecoverySide(FuelCell(CompNum).CWLoopNum,
                                            FuelCell(CompNum).CWLoopSideNum,
                                            DataPlant::TypeOf_Generator_FCExhaust,
                                            FuelCell(CompNum).ExhaustHX.WaterInNode,
                                            FuelCell(CompNum).ExhaustHX.WaterOutNode,
                                            FuelCell(CompNum).ExhaustHX.qHX,
                                            FuelCell(CompNum).ExhaustHX.WaterInletTemp,
                                            FuelCell(CompNum).ExhaustHX.WaterOutletTemp,
                                            FuelCell(CompNum).ExhaustHX.WaterMassFlowRate,
                                            FirstHVACIteration);
        }
    }

    void InitFuelCellGenerators(int const FCnum) // index to specific fuel cell generator
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  B. Griffith Sept 2010, plant upgrades

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the FuelCell generators.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        static std::string const RoutineName("InitFuelCellGenerators");

        static bool InitGeneratorOnce(true); // flag for 1 time initialization
        static Array1D_bool MyEnvrnFlag;     // flag for init once at start of environment
        static Array1D_bool MyWarmupFlag;    // flag for init after warmup complete
        int inNode;                          // inlet index in Node array
        int outNode;                         // outlet, index in Node array
        Real64 TimeElapsed;                  // Fraction of the current hour that has elapsed (h)
        static Array1D_bool MyPlantScanFlag;
        Real64 mdot; // local temporary mass flow rate
        Real64 rho;  // local temporary fluid density
        bool errFlag;

        // Do the one time initializations
        if (InitGeneratorOnce) {
            MyEnvrnFlag.allocate(DataGenerators::NumFuelCellGenerators);
            MyWarmupFlag.allocate(DataGenerators::NumFuelCellGenerators);
            MyPlantScanFlag.allocate(DataGenerators::NumFuelCellGenerators);
            MyEnvrnFlag = true;
            MyWarmupFlag = false;
            InitGeneratorOnce = false;
            MyPlantScanFlag = true;
        } // end one time setups and inits

        if (MyPlantScanFlag(FCnum) && allocated(DataPlant::PlantLoop)) {
            errFlag = false;

            PlantUtilities::ScanPlantLoopsForObject(DataGenerators::FuelCell(FCnum).NameExhaustHX,
                                    DataPlant::TypeOf_Generator_FCExhaust,
                                                    DataGenerators::FuelCell(FCnum).CWLoopNum,
                                                    DataGenerators::FuelCell(FCnum).CWLoopSideNum,
                                                    DataGenerators::FuelCell(FCnum).CWBranchNum,
                                                    DataGenerators::FuelCell(FCnum).CWCompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);

            // if there is a stack cooler option it might be connected to plant as well

            if (errFlag) {
                ShowFatalError("InitFuelCellGenerators: Program terminated due to previous condition(s).");
            }
            MyPlantScanFlag(FCnum) = false;
        }

        // Do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag(FCnum) && !MyPlantScanFlag(FCnum)) {

            DataGenerators::FuelSupply(DataGenerators::FuelCell(FCnum).FuelSupNum).PfuelCompEl = 0.0;
            DataGenerators::FuelSupply(DataGenerators::FuelCell(FCnum).FuelSupNum).TfuelIntoFCPM = 0.0;
            DataGenerators::FuelSupply(DataGenerators::FuelCell(FCnum).FuelSupNum).TfuelIntoCompress = 0.0;
            DataGenerators::FuelSupply(DataGenerators::FuelCell(FCnum).FuelSupNum).QskinLoss = 0.0;

            DataGenerators::FuelCell(FCnum).AirSup.TairIntoFCPM = 0.0;
            DataGenerators::FuelCell(FCnum).AirSup.PairCompEl = 0.0;
            DataGenerators::FuelCell(FCnum).AirSup.TairIntoBlower = 0.0;
            DataGenerators::FuelCell(FCnum).AirSup.QskinLoss = 0.0;
            DataGenerators::FuelCell(FCnum).AirSup.QintakeRecovery = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.NumCycles = 0;
            DataGenerators::FuelCell(FCnum).FCPM.Pel = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.PelLastTimeStep = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.Eel = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.PelancillariesAC = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.NdotFuel = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.TotFuelInEnthalphy = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.NdotProdGas = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.TprodGasLeavingFCPM = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.TotProdGasEnthalphy = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.NdotAir = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.TotAirInEnthalphy = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.NdotLiqwater = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.TwaterInlet = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.WaterInEnthalpy = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.TprodGasLeavingFCPM = 200.0;
            DataGenerators::FuelCell(FCnum).FCPM.FractionalDayofLastStartUp = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.FractionalDayofLastShutDown = 0.0;
            DataGenerators::FuelCell(FCnum).FCPM.HasBeenOn = true;
            DataGenerators::FuelCell(FCnum).FCPM.DuringShutDown = false;
            DataGenerators::FuelCell(FCnum).FCPM.DuringStartUp = false;
            DataGenerators::FuelCell(FCnum).WaterSup.TwaterIntoCompress = 0.0;
            DataGenerators::FuelCell(FCnum).WaterSup.TwaterIntoFCPM = 0.0;
            DataGenerators::FuelCell(FCnum).WaterSup.PwaterCompEl = 0.0;
            DataGenerators::FuelCell(FCnum).WaterSup.QskinLoss = 0.0;
            DataGenerators::FuelCell(FCnum).AuxilHeat.TauxMix = 0.0;
            DataGenerators::FuelCell(FCnum).AuxilHeat.NdotAuxMix = 0.0;
            DataGenerators::FuelCell(FCnum).AuxilHeat.QskinLoss = 0.0;
            DataGenerators::FuelCell(FCnum).AuxilHeat.QairIntake = 0.0;
            DataGenerators::FuelCell(FCnum).ExhaustHX.NdotHXleaving = 0.0;
            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterOutletTemp = 0.0;
            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterOutletEnthalpy = 0.0;
            DataGenerators::FuelCell(FCnum).ElecStorage.LastTimeStepStateOfCharge = DataGenerators::FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            DataGenerators::FuelCell(FCnum).ElecStorage.ThisTimeStepStateOfCharge = DataGenerators::FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            DataGenerators::FuelCell(FCnum).ElecStorage.PelNeedFromStorage = 0.0;
            DataGenerators::FuelCell(FCnum).ElecStorage.IdesiredDischargeCurrent = 0.0;
            DataGenerators::FuelCell(FCnum).ElecStorage.PelFromStorage = 0.0;
            DataGenerators::FuelCell(FCnum).ElecStorage.IfromStorage = 0.0;
            DataGenerators::FuelCell(FCnum).ElecStorage.PelIntoStorage = 0.0;
            DataGenerators::FuelCell(FCnum).ElecStorage.QairIntake = 0.0;

            DataGenerators::FuelCell(FCnum).Inverter.PCUlosses = 0.0;
            DataGenerators::FuelCell(FCnum).Inverter.QairIntake = 0.0;

            rho = FluidProperties::GetDensityGlycol(
                DataPlant::PlantLoop(DataGenerators::FuelCell(FCnum).CWLoopNum).FluidName, DataGenerators::InitHRTemp, DataPlant::PlantLoop(DataGenerators::FuelCell(FCnum).CWLoopNum).FluidIndex, RoutineName);

            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterVolumeFlowMax * rho;
            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRate = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign;
            inNode = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterInNode;
            outNode = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterOutNode;
            DataLoopNode::Node(inNode).Temp = DataGenerators::InitHRTemp;
            DataLoopNode::Node(outNode).Temp = DataGenerators::InitHRTemp;

            PlantUtilities::InitComponentNodes(0.0,
                               DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign,
                               inNode,
                               outNode,
                               DataGenerators::FuelCell(FCnum).CWLoopNum,
                               DataGenerators::FuelCell(FCnum).CWLoopSideNum,
                               DataGenerators::FuelCell(FCnum).CWBranchNum,
                               DataGenerators::FuelCell(FCnum).CWCompNum);

            MyEnvrnFlag(FCnum) = false;
            MyWarmupFlag(FCnum) = true;
        } // end environmental inits

        if (!DataGlobals::BeginEnvrnFlag) {
            MyEnvrnFlag(FCnum) = true;
        }

        if (MyWarmupFlag(FCnum) && (!DataGlobals::WarmupFlag)) {
            // need to reset initial state of charge at beginning of environment but after warm up is complete
            DataGenerators::FuelCell(FCnum).ElecStorage.LastTimeStepStateOfCharge = DataGenerators::FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            DataGenerators::FuelCell(FCnum).ElecStorage.ThisTimeStepStateOfCharge = DataGenerators::FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            MyWarmupFlag(FCnum) = false;
        }

        // using and elapsed time method rather than FirstHVACIteration here
        TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
        if (DataGenerators::FuelCell(FCnum).TimeElapsed != TimeElapsed) {

            DataGenerators::FuelCell(FCnum).ElecStorage.LastTimeStepStateOfCharge = DataGenerators::FuelCell(FCnum).ElecStorage.ThisTimeStepStateOfCharge;
            DataGenerators::FuelCell(FCnum).FCPM.PelLastTimeStep = DataGenerators::FuelCell(FCnum).FCPM.Pel;

            inNode = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterInNode;
            outNode = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterOutNode;
            // intialize flow rate in water loop, this is "requesting" flow
            mdot = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign;

            PlantUtilities::SetComponentFlowRate(mdot,
                                 inNode,
                                 outNode,
                                 DataGenerators::FuelCell(FCnum).CWLoopNum,
                                 DataGenerators::FuelCell(FCnum).CWLoopSideNum,
                                 DataGenerators::FuelCell(FCnum).CWBranchNum,
                                 DataGenerators::FuelCell(FCnum).CWCompNum);

            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRate = mdot;
            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterInletTemp = DataLoopNode::Node(inNode).Temp;
            DataGenerators::FuelCell(FCnum).TimeElapsed = TimeElapsed;
        } else {
            inNode = DataGenerators::FuelCell(FCnum).ExhaustHX.WaterInNode;

            PlantUtilities::SetComponentFlowRate(DataGenerators::FuelCell(FCnum).ExhaustHX.WaterMassFlowRate,
                                 DataGenerators::FuelCell(FCnum).ExhaustHX.WaterInNode,
                                 DataGenerators::FuelCell(FCnum).ExhaustHX.WaterOutNode,
                                 DataGenerators::FuelCell(FCnum).CWLoopNum,
                                 DataGenerators::FuelCell(FCnum).CWLoopSideNum,
                                 DataGenerators::FuelCell(FCnum).CWBranchNum,
                                 DataGenerators::FuelCell(FCnum).CWCompNum);

            DataGenerators::FuelCell(FCnum).ExhaustHX.WaterInletTemp = DataLoopNode::Node(inNode).Temp;
        }
    }

    void getFuelCellGeneratorHeatRecoveryInfo(std::string const &GeneratorName, // user specified name of Generator
                                              std::string &heatRecoveryCompName)
    {

        using DataGenerators::FuelCell;

        if (GetFuelCellInput) {

            // Read input data.
            GetFuelCellGeneratorInput();
            GetFuelCellInput = false;
        }

        int thisFuelCell = UtilityRoutines::FindItemInList(GeneratorName, FuelCell);
        if (thisFuelCell > 0) {
            heatRecoveryCompName = FuelCell(thisFuelCell).ExhaustHX.Name;
        }
    }

    void FigureFuelCellZoneGains()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       BG March 2007
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Couple equpment skin losses to the Zone Heat Balance
        // calculate skin losses from different subsystems and set the value

        // METHODOLOGY EMPLOYED:
        // This routine adds up the various skin losses and then
        //  sets the values in the ZoneIntGain structure

        Real64 TotalZoneHeatGain; // working variable for zone gain [w]
        int FCnum; // number of fuel cell
        static bool MyEnvrnFlag(true);

        if (DataGenerators::NumFuelCellGenerators == 0) return;

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag) {
            for (auto &e : DataGenerators::FuelSupply)
                e.QskinLoss = 0.0;
            MyEnvrnFlag = false;
            for (int i = DataGenerators::FuelCell.l(), e = DataGenerators::FuelCell.u(); i <= e; ++i) {
                auto &cell(DataGenerators::FuelCell(i));
                cell.FCPM.HasBeenOn = false;
                cell.AirSup.PairCompEl = 0.0;
                cell.QconvZone = 0.0;
                cell.QradZone = 0.0;
                cell.AirSup.QskinLoss = 0.0;
                cell.WaterSup.QskinLoss = 0.0;
                cell.AuxilHeat.QskinLoss = 0.0;
                cell.FCPM.QdotSkin = 0.0;
                cell.Report.SkinLossConvect = 0.0;
                cell.Report.SkinLossRadiat = 0.0;
                cell.AuxilHeat.QairIntake = 0.0;
                cell.ElecStorage.QairIntake = 0.0;
                cell.Inverter.QairIntake = 0.0;
            }
        }

        if (!DataGlobals::BeginEnvrnFlag) MyEnvrnFlag = true;

        // this routine needs to do something for zone gains during sizing

        // first collect skin losses from different subsystems
        for (FCnum = 1; FCnum <= DataGenerators::NumFuelCellGenerators; ++FCnum) {
            TotalZoneHeatGain = DataGenerators::FuelCell(FCnum).AirSup.QskinLoss + DataGenerators::FuelSupply(DataGenerators::FuelCell(FCnum).FuelSupNum).QskinLoss +
                                DataGenerators::FuelCell(FCnum).WaterSup.QskinLoss + DataGenerators::FuelCell(FCnum).AuxilHeat.QskinLoss +
                                DataGenerators::FuelCell(FCnum).FCPM.QdotSkin; // intake Blower losses to zone | fuel compressor losses to zone | water pump losses to
                                                               // zone | auxil burner losses to zone | power module (stack and reformer) losses to
                                                               // zone

            // now account for other subsystems that may or may not have air intake recovery
            {
                auto const SELECT_CASE_var(DataGenerators::FuelCell(FCnum).AirSup.IntakeRecoveryMode);

                if (SELECT_CASE_var == DataGenerators::NoRecoveryOnAirIntake) { // then the heat has to go into zone
                    TotalZoneHeatGain +=
                        DataGenerators::FuelCell(FCnum).AuxilHeat.QairIntake + DataGenerators::FuelCell(FCnum).ElecStorage.QairIntake + DataGenerators::FuelCell(FCnum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverAuxiliaryBurner) {
                    TotalZoneHeatGain += DataGenerators::FuelCell(FCnum).ElecStorage.QairIntake + DataGenerators::FuelCell(FCnum).Inverter.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverInverterBatt) {
                    TotalZoneHeatGain += DataGenerators::FuelCell(FCnum).AuxilHeat.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverInverter) {
                    TotalZoneHeatGain += DataGenerators::FuelCell(FCnum).AuxilHeat.QairIntake + DataGenerators::FuelCell(FCnum).ElecStorage.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverBattery) {
                    TotalZoneHeatGain += DataGenerators::FuelCell(FCnum).AuxilHeat.QairIntake + DataGenerators::FuelCell(FCnum).Inverter.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverBurnInvertBatt) {
                    // do nothing
                }
            }

            DataGenerators::FuelCell(FCnum).QconvZone = TotalZoneHeatGain * (1 - DataGenerators::FuelCell(FCnum).FCPM.RadiativeFract);
            DataGenerators::FuelCell(FCnum).Report.SkinLossConvect = DataGenerators::FuelCell(FCnum).QconvZone;
            DataGenerators::FuelCell(FCnum).QradZone = TotalZoneHeatGain * DataGenerators::FuelCell(FCnum).FCPM.RadiativeFract;
            DataGenerators::FuelCell(FCnum).Report.SkinLossRadiat = DataGenerators::FuelCell(FCnum).QradZone;

        } // over number of Fuel cells
    }

    void CalcUpdateHeatRecovery(int const Num, // Generator number
                                bool const EP_UNUSED(FirstHVACIteration))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update plant loop interactions, do any calcs needed

        int InNodeNum;
        int OutNodeNum;

        // now update water outlet node Changing to Kg/s!
        OutNodeNum = DataGenerators::FuelCell(Num).ExhaustHX.WaterOutNode;
        InNodeNum = DataGenerators::FuelCell(Num).ExhaustHX.WaterInNode;

        PlantUtilities::SafeCopyPlantNode(InNodeNum, OutNodeNum);

        DataLoopNode::Node(OutNodeNum).Temp = DataGenerators::FuelCell(Num).ExhaustHX.WaterOutletTemp;
        DataLoopNode::Node(OutNodeNum).Enthalpy = DataGenerators::FuelCell(Num).ExhaustHX.WaterOutletEnthalpy;
    }

    void UpdateFuelCellGeneratorRecords(bool const EP_UNUSED(RunFlag), // TRUE if Generator operating
                                        int const Num                  // Generator number
    )
    {

        DataGenerators::FuelCell(Num).Report.ACPowerGen = DataGenerators::FuelCell(Num).ACPowerGen;                            // electrical power produced [W]
        DataGenerators::FuelCell(Num).Report.ACEnergyGen = DataGenerators::FuelCell(Num).ACPowerGen * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // energy produced (J)
        DataGenerators::FuelCell(Num).Report.QdotExhaust = 0.0;                                                // reporting: exhaust gas heat recovered (W)
        DataGenerators::FuelCell(Num).Report.TotalHeatEnergyRec = 0.0;                                         // reporting: total heat recovered (J)
        DataGenerators::FuelCell(Num).Report.ExhaustEnergyRec = 0.0;                                           // reporting: exhaust gas heat recovered (J)

        DataGenerators::FuelCell(Num).Report.HeatRecInletTemp = 0.0;  // reporting: Heat Recovery Loop Inlet Temperature (C)
        DataGenerators::FuelCell(Num).Report.HeatRecOutletTemp = 0.0; // reporting: Heat Recovery Loop Outlet Temperature (C)
        DataGenerators::FuelCell(Num).Report.HeatRecMdot = 0.0;       // reporting: Heat Recovery Loop Mass flow rate (kg/s)

        DataGenerators::FuelCell(Num).Report.ElectEfficiency = 0.0;
        DataGenerators::FuelCell(Num).Report.ThermalEfficiency = 0.0;
        DataGenerators::FuelCell(Num).Report.OverallEfficiency = 0.0;
        DataGenerators::FuelCell(Num).Report.ExergyEfficiency = 0.0;

        DataGenerators::FuelCell(Num).Report.TairInlet = DataGenerators::FuelCell(Num).AirSup.TairIntoBlower;                          // State point 1
        DataGenerators::FuelCell(Num).Report.TairIntoFCPM = DataGenerators::FuelCell(Num).AirSup.TairIntoFCPM;                         // State point 4
        DataGenerators::FuelCell(Num).Report.NdotAir = DataGenerators::FuelCell(Num).FCPM.NdotAir;                                     // air flow in kmol/sec
        DataGenerators::FuelCell(Num).Report.TotAirInEnthalphy = DataGenerators::FuelCell(Num).FCPM.TotAirInEnthalphy;                 // State point 4
        DataGenerators::FuelCell(Num).Report.BlowerPower = DataGenerators::FuelCell(Num).AirSup.PairCompEl;                            // electrical power used by air supply blower
        DataGenerators::FuelCell(Num).Report.BlowerEnergy = DataGenerators::FuelCell(Num).AirSup.PairCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // electrical energy
        DataGenerators::FuelCell(Num).Report.BlowerSkinLoss = DataGenerators::FuelCell(Num).AirSup.QskinLoss;                          // heat rate of losses by blower

        DataGenerators::FuelCell(Num).Report.TfuelInlet = DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).TfuelIntoCompress; // State point 2
        DataGenerators::FuelCell(Num).Report.TfuelIntoFCPM = DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).TfuelIntoFCPM;  // TEmperature state point 5 [C]
        DataGenerators::FuelCell(Num).Report.NdotFuel = DataGenerators::FuelCell(Num).FCPM.NdotFuel;                              // fuel flow in kmol/sec
        DataGenerators::FuelCell(Num).Report.TotFuelInEnthalpy = DataGenerators::FuelCell(Num).FCPM.TotFuelInEnthalphy;           // enthalpy at state point 5 [W]
        DataGenerators::FuelCell(Num).Report.FuelCompressPower = DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).PfuelCompEl;
        // electrical power used by fuel supply compressor [W]
        DataGenerators::FuelCell(Num).Report.FuelCompressEnergy = DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).PfuelCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // elect energy
        DataGenerators::FuelCell(Num).Report.FuelCompressSkinLoss = DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).QskinLoss;
        // heat rate of losses.by fuel supply compressor [W]
        DataGenerators::FuelCell(Num).Report.FuelEnergyLHV = DataGenerators::FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).LHV * 1000000.0 * DataHVACGlobals::TimeStepSys *
                                             DataGlobals::SecInHour; // reporting: Fuel Energy used (J)
        DataGenerators::FuelCell(Num).Report.FuelEnergyUseRateLHV =
            DataGenerators::FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).LHV * 1000000.0; // reporting: Fuel Energy used (W)
        DataGenerators::FuelCell(Num).Report.FuelEnergyHHV = DataGenerators::FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).HHV *
                                             DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).KmolPerSecToKgPerSec * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        DataGenerators::FuelCell(Num).Report.FuelEnergyUseRateHHV =
            DataGenerators::FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).HHV * DataGenerators::FuelSupply(DataGenerators::FuelCell(Num).FuelSupNum).KmolPerSecToKgPerSec;

        DataGenerators::FuelCell(Num).Report.FuelRateMdot = 0.0; // (Kg/s)

        DataGenerators::FuelCell(Num).Report.TwaterInlet = DataGenerators::FuelCell(Num).WaterSup.TwaterIntoCompress;
        DataGenerators::FuelCell(Num).Report.TwaterIntoFCPM = DataGenerators::FuelCell(Num).WaterSup.TwaterIntoFCPM;
        DataGenerators::FuelCell(Num).Report.NdotWater = DataGenerators::FuelCell(Num).FCPM.NdotLiqwater; // water flow in kmol/sec (reformer water)
        DataGenerators::FuelCell(Num).Report.WaterPumpPower = DataGenerators::FuelCell(Num).WaterSup.PwaterCompEl;
        DataGenerators::FuelCell(Num).Report.WaterPumpEnergy = DataGenerators::FuelCell(Num).WaterSup.PwaterCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // electrical energy
        DataGenerators::FuelCell(Num).Report.WaterIntoFCPMEnthalpy = DataGenerators::FuelCell(Num).FCPM.WaterInEnthalpy;

        DataGenerators::FuelCell(Num).Report.TprodGas = DataGenerators::FuelCell(Num).FCPM.TprodGasLeavingFCPM;      // temperature at State point 7
        DataGenerators::FuelCell(Num).Report.EnthalProdGas = DataGenerators::FuelCell(Num).FCPM.TotProdGasEnthalphy; // enthalpy at State point 7
        DataGenerators::FuelCell(Num).Report.NdotProdGas = DataGenerators::FuelCell(Num).FCPM.NdotProdGas;           // flow rate at point 7 [kmol/sec]
        DataGenerators::FuelCell(Num).Report.NdotProdAr = DataGenerators::FuelCell(Num).FCPM.ConstitMolalFract(5) * DataGenerators::FuelCell(Num).FCPM.NdotProdGas;
        DataGenerators::FuelCell(Num).Report.NdotProdCO2 = DataGenerators::FuelCell(Num).FCPM.ConstitMolalFract(1) * DataGenerators::FuelCell(Num).FCPM.NdotProdGas;
        DataGenerators::FuelCell(Num).Report.NdotProdH2O = DataGenerators::FuelCell(Num).FCPM.ConstitMolalFract(4) * DataGenerators::FuelCell(Num).FCPM.NdotProdGas;
        DataGenerators::FuelCell(Num).Report.NdotProdN2 = DataGenerators::FuelCell(Num).FCPM.ConstitMolalFract(2) * DataGenerators::FuelCell(Num).FCPM.NdotProdGas;
        DataGenerators::FuelCell(Num).Report.NdotProdO2 = DataGenerators::FuelCell(Num).FCPM.ConstitMolalFract(3) * DataGenerators::FuelCell(Num).FCPM.NdotProdGas;

        DataGenerators::FuelCell(Num).Report.qHX = DataGenerators::FuelCell(Num).ExhaustHX.qHX;
        DataGenerators::FuelCell(Num).Report.HXenergy = DataGenerators::FuelCell(Num).ExhaustHX.qHX * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        DataGenerators::FuelCell(Num).Report.THXexh = DataGenerators::FuelCell(Num).ExhaustHX.THXexh;
        DataGenerators::FuelCell(Num).Report.WaterVaporFractExh = DataGenerators::FuelCell(Num).ExhaustHX.WaterVaporFractExh;
        DataGenerators::FuelCell(Num).Report.CondensateRate = DataGenerators::FuelCell(Num).ExhaustHX.CondensateRate;

        DataGenerators::FuelCell(Num).Report.SeqSubstIterations = DataGenerators::FuelCell(Num).FCPM.SeqSubstitIter;     // number of iterations in DataGenerators::FuelCell loop
        DataGenerators::FuelCell(Num).Report.RegulaFalsiIterations = DataGenerators::FuelCell(Num).FCPM.RegulaFalsiIter; // number of iterations in Tproduct gas solving

        DataGenerators::FuelCell(Num).Report.ACancillariesPower = DataGenerators::FuelCell(Num).FCPM.PelancillariesAC;
        DataGenerators::FuelCell(Num).Report.ACancillariesEnergy = DataGenerators::FuelCell(Num).FCPM.PelancillariesAC * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        DataGenerators::FuelCell(Num).Report.PCUlosses = DataGenerators::FuelCell(Num).Inverter.PCUlosses; // inverter losses
        DataGenerators::FuelCell(Num).Report.DCPowerGen = DataGenerators::FuelCell(Num).FCPM.Pel;          // DC power out of FCPM.
        DataGenerators::FuelCell(Num).Report.DCPowerEff = DataGenerators::FuelCell(Num).FCPM.Eel;          // FCPM efficienty Eel.
        DataGenerators::FuelCell(Num).Report.ElectEnergyinStorage = DataGenerators::FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge;
        DataGenerators::FuelCell(Num).Report.StoredPower = DataGenerators::FuelCell(Num).ElecStorage.PelIntoStorage;
        DataGenerators::FuelCell(Num).Report.StoredEnergy = DataGenerators::FuelCell(Num).ElecStorage.PelIntoStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        DataGenerators::FuelCell(Num).Report.DrawnPower = DataGenerators::FuelCell(Num).ElecStorage.PelFromStorage;
        DataGenerators::FuelCell(Num).Report.DrawnEnergy = DataGenerators::FuelCell(Num).ElecStorage.PelFromStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        DataGenerators::FuelCell(Num).Report.SkinLossPower = DataGenerators::FuelCell(Num).QconvZone + DataGenerators::FuelCell(Num).QradZone;
        DataGenerators::FuelCell(Num).Report.SkinLossEnergy = (DataGenerators::FuelCell(Num).QconvZone + DataGenerators::FuelCell(Num).QradZone) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        DataGenerators::FuelCell(Num).Report.SkinLossConvect = DataGenerators::FuelCell(Num).QconvZone;
        DataGenerators::FuelCell(Num).Report.SkinLossRadiat = DataGenerators::FuelCell(Num).QradZone;
    }

    void GetFuelCellGeneratorResults(int const EP_UNUSED(GeneratorType), // type of Generator
                                     int const GeneratorIndex,
                                     Real64 &GeneratorPower,  // electrical power
                                     Real64 &GeneratorEnergy, // electrical energy
                                     Real64 &ThermalPower,    // heat power
                                     Real64 &ThermalEnergy    // heat energy
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // provide a get method to collect results at the load center level

        GeneratorPower = DataGenerators::FuelCell(GeneratorIndex).Report.ACPowerGen;
        GeneratorEnergy = DataGenerators::FuelCell(GeneratorIndex).Report.ACEnergyGen;
        ThermalPower = DataGenerators::FuelCell(GeneratorIndex).Report.qHX;
        ThermalEnergy = DataGenerators::FuelCell(GeneratorIndex).Report.HXenergy;
    }

} // namespace FuelCellElectricGenerator

} // namespace EnergyPlus
