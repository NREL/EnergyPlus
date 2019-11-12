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

    int NumFuelCellGenerators(0);
    bool GetFuelCellInput(true);
    Array1D_bool CheckEquipName;
    Array1D<FCDataStruct> FuelCell; // dimension to number of machines

    void clear_state()
    {
        NumFuelCellGenerators = 0;
        GetFuelCellInput = true;
        CheckEquipName.deallocate();
        FuelCell.deallocate();
    }

    void SimFuelCellGenerator(int const EP_UNUSED(GeneratorType), // type of Generator
                              std::string const &GeneratorName,   // user specified name of Generator
                              int &GeneratorIndex,
                              bool const RunFlag,  // simulate Generator when TRUE
                              Real64 const MyLoad, // demand on electric generator
                              bool const FirstHVACIteration)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   March 2005
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
            GenNum = UtilityRoutines::FindItemInList(GeneratorName, FuelCell);
            if (GenNum == 0) ShowFatalError("SimFuelCellGenerator: Specified Generator not one of Valid FuelCell Generators " + GeneratorName);
            GeneratorIndex = GenNum;
        } else {
            GenNum = GeneratorIndex;
            if (GenNum > NumFuelCellGenerators || GenNum < 1) {
                ShowFatalError("SimFuelCellGenerator: Invalid GeneratorIndex passed=" + General::TrimSigDigits(GenNum) +
                               ", Number of FuelCell Generators=" + General::TrimSigDigits(NumFuelCellGenerators) + ", Generator name=" + GeneratorName);
            }
            if (CheckEquipName(GenNum)) {
                if (GeneratorName != FuelCell(GenNum).Name) {
                    ShowFatalError("SimFuelCellGenerator: Invalid GeneratorIndex passed=" + General::TrimSigDigits(GenNum) +
                                   ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + FuelCell(GenNum).Name);
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

        int NumAlphas;                 // Number of elements in the alpha array
        int NumNums;                   // Number of elements in the numeric array
        int IOStat;                    // IO Status when calling get input subroutine
        Array1D_string AlphArray(25);  // character string data
        Array1D<Real64> NumArray(200); // numeric data TODO deal with allocatable for extensible
        Array1D_bool lAlphaBlanks(25);
        bool ErrorsFound(false); // error flag

        DataIPShortCuts::cCurrentModuleObject = "Generator:FuelCell";
        NumFuelCellGenerators = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumFuelCellGenerators <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        FuelCell.allocate(NumFuelCellGenerators); // inits handled in derived type definitions
        CheckEquipName.dimension(NumFuelCellGenerators, true);

        // first load in FuelCell names
        for (int GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum) {
            inputProcessor->getObjectItem(
                DataIPShortCuts::cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            FuelCell(GeneratorNum).Name = AlphArray(1);
            FuelCell(GeneratorNum).NameFCPM = AlphArray(2);
            FuelCell(GeneratorNum).NameFCAirSup = AlphArray(3);
            FuelCell(GeneratorNum).NameFCFuelSup = AlphArray(4);
            FuelCell(GeneratorNum).NameFCWaterSup = AlphArray(5);
            FuelCell(GeneratorNum).NameFCAuxilHeat = AlphArray(6);
            FuelCell(GeneratorNum).NameExhaustHX = AlphArray(7);
            FuelCell(GeneratorNum).NameElecStorage = AlphArray(8);
            FuelCell(GeneratorNum).NameInverter = AlphArray(9);
            if (NumAlphas == 10) {
                FuelCell(GeneratorNum).NameStackCooler = AlphArray(10);
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameFCPM);
            if (thisFuelCell > 0) {

                FuelCell(thisFuelCell).FCPM.Name = AlphArray(1);
                if (UtilityRoutines::SameString(AlphArray(2), "ANNEX42")) FuelCell(thisFuelCell).FCPM.EffMode = DataGenerators::DirectCurveMode;
                if (UtilityRoutines::SameString(AlphArray(2), "NORMALIZED")) FuelCell(thisFuelCell).FCPM.EffMode = DataGenerators::NormalizedCurveMode;
                if (FuelCell(thisFuelCell).FCPM.EffMode == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).FCPM.EffCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                if (FuelCell(thisFuelCell).FCPM.EffCurveID == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).FCPM.NomEff = NumArray(1);
                FuelCell(thisFuelCell).FCPM.NomPel = NumArray(2);
                FuelCell(thisFuelCell).FCPM.NumCycles = NumArray(3);
                FuelCell(thisFuelCell).FCPM.CyclingDegradRat = NumArray(4);
                FuelCell(thisFuelCell).FCPM.NumRunHours = NumArray(5);
                FuelCell(thisFuelCell).FCPM.OperateDegradRat = NumArray(6);
                FuelCell(thisFuelCell).FCPM.ThreshRunHours = NumArray(7);
                FuelCell(thisFuelCell).FCPM.UpTranLimit = NumArray(8);
                FuelCell(thisFuelCell).FCPM.DownTranLimit = NumArray(9);
                FuelCell(thisFuelCell).FCPM.StartUpTime = NumArray(10) / DataGlobals::SecInHour; // convert to hours from seconds
                FuelCell(thisFuelCell).FCPM.StartUpFuel = NumArray(11);
                FuelCell(thisFuelCell).FCPM.StartUpElectConsum = NumArray(12);
                FuelCell(thisFuelCell).FCPM.StartUpElectProd = NumArray(13);
                FuelCell(thisFuelCell).FCPM.ShutDownTime = NumArray(14) / DataGlobals::SecInHour; // convert to hours from seconds
                FuelCell(thisFuelCell).FCPM.ShutDownFuel = NumArray(15);
                FuelCell(thisFuelCell).FCPM.ShutDownElectConsum = NumArray(16);
                FuelCell(thisFuelCell).FCPM.ANC0 = NumArray(17);
                FuelCell(thisFuelCell).FCPM.ANC1 = NumArray(18);
                if (UtilityRoutines::SameString(AlphArray(4), "ConstantRate")) FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::ConstantRateSkinLoss;
                if (UtilityRoutines::SameString(AlphArray(4), "UAForProcessGasTemperature"))
                    FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::UADTSkinLoss;
                if (UtilityRoutines::SameString(AlphArray(4), "QUADRATIC FUNCTION OF FUEL RATE"))
                    FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::QuadraticFuelNdotSkin;
                if (FuelCell(thisFuelCell).FCPM.SkinLossMode == 0) {
                    // throw error
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).FCPM.ZoneName = AlphArray(5);
                FuelCell(thisFuelCell).FCPM.ZoneID = UtilityRoutines::FindItemInList(FuelCell(thisFuelCell).FCPM.ZoneName, DataHeatBalance::Zone);
                if (FuelCell(thisFuelCell).FCPM.ZoneID == 0 && !lAlphaBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Zone Name was not found ");
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).FCPM.RadiativeFract = NumArray(19);
                FuelCell(thisFuelCell).FCPM.QdotSkin = NumArray(20);
                FuelCell(thisFuelCell).FCPM.UAskin = NumArray(21);

                FuelCell(thisFuelCell).FCPM.SkinLossCurveID = CurveManager::GetCurveIndex(AlphArray(6));
                if (FuelCell(thisFuelCell).FCPM.SkinLossCurveID == 0) {
                    if (FuelCell(thisFuelCell).FCPM.SkinLossMode == DataGenerators::QuadraticFuelNdotSkin) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                }

                FuelCell(thisFuelCell).FCPM.NdotDilutionAir = NumArray(22);
                FuelCell(thisFuelCell).FCPM.StackHeatLossToDilution = NumArray(23);
                FuelCell(thisFuelCell).FCPM.DilutionInletNodeName = AlphArray(7);
                FuelCell(thisFuelCell).FCPM.DilutionInletNode = NodeInputManager::GetOnlySingleNode(
                    AlphArray(7), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
                FuelCell(thisFuelCell).FCPM.DilutionExhaustNodeName = AlphArray(8);
                FuelCell(thisFuelCell).FCPM.DilutionExhaustNode = NodeInputManager::GetOnlySingleNode(
                    AlphArray(8), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);

                FuelCell(thisFuelCell).FCPM.PelMin = NumArray(24);
                FuelCell(thisFuelCell).FCPM.PelMax = NumArray(25);

                // check for other FuelCell using the same power module and fill
                for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell) {
                    if (UtilityRoutines::SameString(FuelCell(otherFuelCell).FCPM.Name, FuelCell(thisFuelCell).FCPM.Name)) {
                        FuelCell(otherFuelCell).FCPM = FuelCell(thisFuelCell).FCPM;
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
        for (int GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum) {
            FuelCell(GeneratorNum).FuelSupNum =
                UtilityRoutines::FindItemInList(FuelCell(GeneratorNum).NameFCFuelSup, DataGenerators::FuelSupply); // Fuel Supply ID
            if (FuelCell(GeneratorNum).FuelSupNum == 0) {
                ShowSevereError("Fuel Supply Name: " + FuelCell(GeneratorNum).NameFCFuelSup + " not found in " + FuelCell(GeneratorNum).Name);
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameFCAirSup);

            if (thisFuelCell > 0) {

                FuelCell(thisFuelCell).AirSup.Name = AlphArray(1);
                FuelCell(thisFuelCell).AirSup.NodeName = AlphArray(2);

                // check the node connections
                FuelCell(thisFuelCell).AirSup.SupNodeNum = NodeInputManager::GetOnlySingleNode(
                    AlphArray(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);

                FuelCell(thisFuelCell).AirSup.BlowerPowerCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                if (FuelCell(thisFuelCell).AirSup.BlowerPowerCurveID == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Curve name was not found ");
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).AirSup.BlowerHeatLossFactor = NumArray(1);

                if (UtilityRoutines::SameString(AlphArray(4), "AirRatiobyStoics")) {
                    FuelCell(thisFuelCell).AirSup.AirSupRateMode = DataGenerators::ConstantStoicsAirRat;
                } else if (UtilityRoutines::SameString(AlphArray(4), "QuadraticFunctionofElectricPower")) {
                    FuelCell(thisFuelCell).AirSup.AirSupRateMode = DataGenerators::QuadraticFuncofPel;
                } else if (UtilityRoutines::SameString(AlphArray(4), "QUADRATIC FUNCTION OF FUEL RATE")) {
                    FuelCell(thisFuelCell).AirSup.AirSupRateMode = DataGenerators::QuadraticFuncofNdot;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).AirSup.Stoics = NumArray(2) + 1.0;

                FuelCell(thisFuelCell).AirSup.AirFuncPelCurveID = CurveManager::GetCurveIndex(AlphArray(5));
                if ((FuelCell(thisFuelCell).AirSup.AirFuncPelCurveID == 0) &&
                    (FuelCell(thisFuelCell).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofPel)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowSevereError("Curve name was not found");
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).AirSup.AirTempCoeff = NumArray(3);

                FuelCell(thisFuelCell).AirSup.AirFuncNdotCurveID = CurveManager::GetCurveIndex(AlphArray(6));
                if ((FuelCell(thisFuelCell).AirSup.AirFuncNdotCurveID == 0) &&
                    (FuelCell(thisFuelCell).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofNdot)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowSevereError("Curve name was not found");
                    ErrorsFound = true;
                }

                if (UtilityRoutines::SameString("RecoverBurnerInverterStorage", AlphArray(7))) {
                    FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverBurnInvertBatt;
                } else if (UtilityRoutines::SameString("RecoverAuxiliaryBurner", AlphArray(7))) {
                    FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverAuxiliaryBurner;
                } else if (UtilityRoutines::SameString("RecoverInverterandStorage", AlphArray(7))) {
                    FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverInverterBatt;
                } else if (UtilityRoutines::SameString("RecoverInverter", AlphArray(7))) {
                    FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverInverter;
                } else if (UtilityRoutines::SameString("RecoverElectricalStorage", AlphArray(7))) {
                    FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::RecoverBattery;
                } else if (UtilityRoutines::SameString("NoRecovery", AlphArray(7))) {
                    FuelCell(thisFuelCell).AirSup.IntakeRecoveryMode = DataGenerators::NoRecoveryOnAirIntake;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(7) + " = " + AlphArray(7));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                if (UtilityRoutines::SameString("AmbientAir", AlphArray(8))) {
                    FuelCell(thisFuelCell).AirSup.ConstituentMode = DataGenerators::RegularAir;
                } else if (UtilityRoutines::SameString("UserDefinedConstituents", AlphArray(8))) {
                    FuelCell(thisFuelCell).AirSup.ConstituentMode = DataGenerators::UserDefinedConstituents;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(8) + " = " + AlphArray(8));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                int NumAirConstit;

                if (FuelCell(thisFuelCell).AirSup.ConstituentMode == DataGenerators::UserDefinedConstituents) {
                    NumAirConstit = NumArray(4);
                    FuelCell(thisFuelCell).AirSup.NumConstituents = NumAirConstit;

                    if (NumAirConstit > 5) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(4) + '=' + General::RoundSigDigits(NumArray(4), 2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ShowContinueError("Fuel Cell model not set up for more than 5 air constituents");
                        ErrorsFound = true;
                    }

                    for (int ConstitNum = 1; ConstitNum <= NumAirConstit; ++ConstitNum) {
                        FuelCell(thisFuelCell).AirSup.ConstitName(ConstitNum) = AlphArray(ConstitNum + 8);
                        FuelCell(thisFuelCell).AirSup.ConstitMolalFract(ConstitNum) = NumArray(ConstitNum + 4);
                    }

                } else { // regular air
                    NumAirConstit = 5;

                    FuelCell(thisFuelCell).AirSup.NumConstituents = NumAirConstit;

                    FuelCell(thisFuelCell).AirSup.ConstitName(1) = "Nitrogen";
                    FuelCell(thisFuelCell).AirSup.ConstitMolalFract(1) = 0.7728;

                    FuelCell(thisFuelCell).AirSup.ConstitName(2) = "Oxygen";
                    FuelCell(thisFuelCell).AirSup.ConstitMolalFract(2) = 0.2073;

                    FuelCell(thisFuelCell).AirSup.ConstitName(3) = "Water";
                    FuelCell(thisFuelCell).AirSup.ConstitMolalFract(3) = 0.0104;

                    FuelCell(thisFuelCell).AirSup.ConstitName(4) = "Argon";
                    FuelCell(thisFuelCell).AirSup.ConstitMolalFract(4) = 0.0092;

                    FuelCell(thisFuelCell).AirSup.ConstitName(5) = "CarbonDioxide";
                    FuelCell(thisFuelCell).AirSup.ConstitMolalFract(5) = 0.0003;
                }

                // check for molar fractions summing to 1.0.
                if (std::abs(sum(FuelCell(thisFuelCell).AirSup.ConstitMolalFract) - 1.0) > 0.0001) {

                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " molar fractions do not sum to 1.0");
                    ShowContinueError("..Sum was=" + General::RoundSigDigits(sum(FuelCell(thisFuelCell).AirSup.ConstitMolalFract), 1));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + AlphArray(1));
                    ErrorsFound = true;
                }

                // check for other FuelCell using the same Air Supply module and fill
                for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell) {
                    if (UtilityRoutines::SameString(FuelCell(otherFuelCell).AirSup.Name, FuelCell(thisFuelCell).AirSup.Name)) {
                        FuelCell(otherFuelCell).AirSup = FuelCell(thisFuelCell).AirSup;
                    }
                }
            } else {
                ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(1) + " = " + AlphArray(1));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }
        }

        for (int GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum) {
            // find molal fraction of oxygen in air supply
            int thisConstituent =
                UtilityRoutines::FindItem("Oxygen", FuelCell(GeneratorNum).AirSup.ConstitName, FuelCell(GeneratorNum).AirSup.NumConstituents);
            if (thisConstituent > 0) FuelCell(GeneratorNum).AirSup.O2fraction = FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstituent);

            // Loop over air constituents and do one-time setup
            for (int i = 1; i <= FuelCell(GeneratorNum).AirSup.NumConstituents; ++i) {

                std::string thisName = FuelCell(GeneratorNum).AirSup.ConstitName(i);

                int thisGasID = UtilityRoutines::FindItem(thisName, DataGenerators::GasPhaseThermoChemistryData, &DataGenerators::GasPropertyDataStruct::ConstituentName);

                FuelCell(GeneratorNum).AirSup.GasLibID(i) = thisGasID;
            }

            // set up gas constiuents for product gases
            FuelCell(GeneratorNum).FCPM.GasLibID(1) = 1; // Carbon Dioxide
            FuelCell(GeneratorNum).FCPM.GasLibID(2) = 2; // Nitrogen
            FuelCell(GeneratorNum).FCPM.GasLibID(3) = 3; // Oxygen
            FuelCell(GeneratorNum).FCPM.GasLibID(4) = 4; // Water
            FuelCell(GeneratorNum).FCPM.GasLibID(5) = 5; // Argon
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameFCWaterSup);

            if (thisFuelCell > 0) {
                //  this is only the first instance of a FuelCell generator using this type of Water supply module
                FuelCell(thisFuelCell).WaterSup.Name = AlphArray(1);
                FuelCell(thisFuelCell).WaterSup.WaterSupRateCurveID = CurveManager::GetCurveIndex(AlphArray(2));
                if (FuelCell(thisFuelCell).WaterSup.WaterSupRateCurveID == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Curve name was not found ");
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).WaterSup.PmpPowerCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                if (FuelCell(thisFuelCell).WaterSup.PmpPowerCurveID == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Curve name was not found ");
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).WaterSup.PmpPowerLossFactor = NumArray(1);

                if (UtilityRoutines::SameString("TemperatureFromAirNode", AlphArray(4))) {
                    FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformAirNode;

                    FuelCell(thisFuelCell).WaterSup.NodeName = AlphArray(5);
                    FuelCell(thisFuelCell).WaterSup.NodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(5),
                                                                                ErrorsFound,
                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                AlphArray(1),
                                                                                DataLoopNode::NodeType_Air,
                                                                                DataLoopNode::NodeConnectionType_Sensor,
                                                                                1,
                                                                                DataLoopNode::ObjectIsNotParent);

                } else if (UtilityRoutines::SameString("TemperatureFromWaterNode", AlphArray(4))) {
                    FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformWaterNode;

                    FuelCell(thisFuelCell).WaterSup.NodeName = AlphArray(5);
                    FuelCell(thisFuelCell).WaterSup.NodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(5),
                                                                                ErrorsFound,
                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                AlphArray(1),
                                                                                DataLoopNode::NodeType_Water,
                                                                                DataLoopNode::NodeConnectionType_Sensor,
                                                                                1,
                                                                                DataLoopNode::ObjectIsNotParent);

                } else if (UtilityRoutines::SameString("MainsWaterTemperature", AlphArray(4))) {
                    FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformMains;

                } else if (UtilityRoutines::SameString("TemperatureFromSchedule", AlphArray(4))) {
                    FuelCell(thisFuelCell).WaterSup.WaterTempMode = DataGenerators::WaterInReformSchedule;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).WaterSup.SchedNum = ScheduleManager::GetScheduleIndex(AlphArray(6));
                if ((FuelCell(thisFuelCell).WaterSup.SchedNum == 0) && (FuelCell(thisFuelCell).WaterSup.WaterTempMode == DataGenerators::WaterInReformSchedule)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + AlphArray(6));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Schedule was not found");
                    ErrorsFound = true;
                }

                // check for other FuelCell using the same Water Supply module and fill
                for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell) {
                    if (UtilityRoutines::SameString(FuelCell(otherFuelCell).WaterSup.Name, FuelCell(thisFuelCell).WaterSup.Name)) {
                        FuelCell(otherFuelCell).WaterSup = FuelCell(thisFuelCell).WaterSup;
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameFCAuxilHeat);

            if (thisFuelCell > 0) {
                FuelCell(thisFuelCell).AuxilHeat.Name = AlphArray(1);

                FuelCell(thisFuelCell).AuxilHeat.ExcessAirRAT = NumArray(1);
                FuelCell(thisFuelCell).AuxilHeat.ANC0 = NumArray(2);
                FuelCell(thisFuelCell).AuxilHeat.ANC1 = NumArray(3);
                FuelCell(thisFuelCell).AuxilHeat.UASkin = NumArray(4);

                if (UtilityRoutines::SameString("SurroundingZone", AlphArray(2))) {
                    FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination = DataGenerators::SurroundingZone;
                } else if (UtilityRoutines::SameString("AirInletForFuelCell", AlphArray(2))) {
                    FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination = DataGenerators::AirInletForFC;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).AuxilHeat.ZoneName = AlphArray(3);
                FuelCell(thisFuelCell).AuxilHeat.ZoneID = UtilityRoutines::FindItemInList(AlphArray(3), DataHeatBalance::Zone);
                if ((FuelCell(thisFuelCell).AuxilHeat.ZoneID == 0) && (FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination == DataGenerators::SurroundingZone)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Zone name was not found ");
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).AuxilHeat.MaxPowerW = NumArray(5);
                FuelCell(thisFuelCell).AuxilHeat.MinPowerW = NumArray(6);
                FuelCell(thisFuelCell).AuxilHeat.MaxPowerkmolperSec = NumArray(7);
                FuelCell(thisFuelCell).AuxilHeat.MinPowerkmolperSec = NumArray(8);

                // TODO finish Auxiliary heater

                // check for other FuelCell using the same Auxiliary Heating module and fill
                for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell) {
                    if (UtilityRoutines::SameString(FuelCell(otherFuelCell).AuxilHeat.Name, FuelCell(thisFuelCell).AuxilHeat.Name)) {
                        FuelCell(otherFuelCell).AuxilHeat = FuelCell(thisFuelCell).AuxilHeat;
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameExhaustHX);

            if (thisFuelCell > 0) {
                FuelCell(thisFuelCell).ExhaustHX.Name = AlphArray(1);
                FuelCell(thisFuelCell).ExhaustHX.WaterInNodeName = AlphArray(2);
                FuelCell(thisFuelCell).ExhaustHX.WaterOutNodeName = AlphArray(3);
                // find node ids for water path
                FuelCell(thisFuelCell).ExhaustHX.WaterInNode = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                 ErrorsFound,
                                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                                 AlphArray(1),
                                                                                 DataLoopNode::NodeType_Water,
                                                                                 DataLoopNode::NodeConnectionType_Inlet,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
                FuelCell(thisFuelCell).ExhaustHX.WaterOutNode = NodeInputManager::GetOnlySingleNode(AlphArray(3),
                                                                                  ErrorsFound,
                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                  AlphArray(1),
                                                                                  DataLoopNode::NodeType_Water,
                                                                                  DataLoopNode::NodeConnectionType_Outlet,
                                                                                  1,
                                                                                  DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Heat Recovery Nodes");

                FuelCell(thisFuelCell).ExhaustHX.ExhaustOutNodeName = AlphArray(4);
                FuelCell(thisFuelCell).ExhaustHX.ExhaustOutNode = NodeInputManager::GetOnlySingleNode(
                    AlphArray(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);

                if (UtilityRoutines::SameString("FixedEffectiveness", AlphArray(5))) {
                    FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::FixedEffectiveness;
                } else if (UtilityRoutines::SameString("EmpiricalUAeff", AlphArray(5))) {
                    FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::LMTDempiricalUAeff;
                } else if (UtilityRoutines::SameString("FundementalUAeff", AlphArray(5))) {
                    FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::LMTDfundementalUAeff;
                } else if (UtilityRoutines::SameString("CONDENSING", AlphArray(5))) {
                    FuelCell(thisFuelCell).ExhaustHX.HXmodelMode = DataGenerators::Condensing;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).ExhaustHX.WaterVolumeFlowMax = NumArray(1);
                FuelCell(thisFuelCell).ExhaustHX.HXEffect = NumArray(2);
                FuelCell(thisFuelCell).ExhaustHX.hxs0 = NumArray(3);
                FuelCell(thisFuelCell).ExhaustHX.hxs1 = NumArray(4);
                FuelCell(thisFuelCell).ExhaustHX.hxs2 = NumArray(5);
                FuelCell(thisFuelCell).ExhaustHX.hxs3 = NumArray(6);
                FuelCell(thisFuelCell).ExhaustHX.hxs4 = NumArray(7);
                FuelCell(thisFuelCell).ExhaustHX.h0gas = NumArray(8);
                FuelCell(thisFuelCell).ExhaustHX.NdotGasRef = NumArray(9);
                FuelCell(thisFuelCell).ExhaustHX.nCoeff = NumArray(10);
                FuelCell(thisFuelCell).ExhaustHX.AreaGas = NumArray(11);
                FuelCell(thisFuelCell).ExhaustHX.h0Water = NumArray(12);
                FuelCell(thisFuelCell).ExhaustHX.NdotWaterRef = NumArray(13);
                FuelCell(thisFuelCell).ExhaustHX.mCoeff = NumArray(14);
                FuelCell(thisFuelCell).ExhaustHX.AreaWater = NumArray(15);
                FuelCell(thisFuelCell).ExhaustHX.Fadjust = NumArray(16);
                FuelCell(thisFuelCell).ExhaustHX.l1Coeff = NumArray(17);
                FuelCell(thisFuelCell).ExhaustHX.l2Coeff = NumArray(18);
                FuelCell(thisFuelCell).ExhaustHX.CondensationThresholdTemp = NumArray(19);

                // store cooling water volume flow rate for autosizing system
                PlantUtilities::RegisterPlantCompDesignFlow(FuelCell(thisFuelCell).ExhaustHX.WaterInNode, FuelCell(thisFuelCell).ExhaustHX.WaterVolumeFlowMax);
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameElecStorage);

            if (thisFuelCell > 0) {
                FuelCell(thisFuelCell).ElecStorage.Name = AlphArray(1);

                if (UtilityRoutines::SameString(AlphArray(2), "SimpleEfficiencyWithConstraints")) {
                    FuelCell(thisFuelCell).ElecStorage.StorageModelMode = DataGenerators::SimpleEffConstraints;
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).ElecStorage.EnergeticEfficCharge = NumArray(1);
                FuelCell(thisFuelCell).ElecStorage.EnergeticEfficDischarge = NumArray(2);
                FuelCell(thisFuelCell).ElecStorage.NominalEnergyCapacity = NumArray(3);
                FuelCell(thisFuelCell).ElecStorage.MaxPowerDraw = NumArray(4);
                FuelCell(thisFuelCell).ElecStorage.MaxPowerStore = NumArray(5);
                FuelCell(thisFuelCell).ElecStorage.StartingEnergyStored = NumArray(6);

                // check for other FuelCell using the same Electrical Storage and fill
                for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell) {
                    if (UtilityRoutines::SameString(FuelCell(otherFuelCell).ElecStorage.Name, FuelCell(thisFuelCell).ElecStorage.Name)) {
                        FuelCell(otherFuelCell).ElecStorage = FuelCell(thisFuelCell).ElecStorage;
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameInverter);

            if (thisFuelCell > 0) {
                FuelCell(thisFuelCell).Inverter.Name = AlphArray(1);

                if (UtilityRoutines::SameString(AlphArray(2), "QUADRATIC")) FuelCell(thisFuelCell).Inverter.EffMode = DataGenerators::InverterEffQuadratic;
                if (UtilityRoutines::SameString(AlphArray(2), "Constant")) FuelCell(thisFuelCell).Inverter.EffMode = DataGenerators::InverterEffConstant;
                if (FuelCell(thisFuelCell).Inverter.EffMode == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).Inverter.ConstEff = NumArray(1);

                FuelCell(thisFuelCell).Inverter.EffQuadraticCurveID = CurveManager::GetCurveIndex(AlphArray(3));
                if ((FuelCell(thisFuelCell).Inverter.EffQuadraticCurveID == 0) &&
                    (FuelCell(thisFuelCell).Inverter.EffMode == DataGenerators::InverterEffQuadratic)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Curve was not found ");
                    ErrorsFound = true;
                }

                // check for other FuelCell using the same Inverter and fill
                for (int otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell) {
                    if (UtilityRoutines::SameString(FuelCell(otherFuelCell).Inverter.Name, FuelCell(thisFuelCell).Inverter.Name)) {
                        FuelCell(otherFuelCell).Inverter = FuelCell(thisFuelCell).Inverter;
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

                int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameStackCooler);

                if (thisFuelCell > 0) {
                    FuelCell(thisFuelCell).StackCooler.Name = AlphArray(1);
                    FuelCell(thisFuelCell).StackCooler.WaterInNodeName = AlphArray(2);

                    FuelCell(thisFuelCell).StackCooler.WaterOutNodeName = AlphArray(3);

                    FuelCell(thisFuelCell).StackCooler.WaterInNode = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                       ErrorsFound,
                                                                                       DataIPShortCuts::cCurrentModuleObject,
                                                                                       AlphArray(1),
                                                                                       DataLoopNode::NodeType_Water,
                                                                                       DataLoopNode::NodeConnectionType_Inlet,
                                                                                       1,
                                                                                       DataLoopNode::ObjectIsNotParent);
                    FuelCell(thisFuelCell).StackCooler.WaterOutNode = NodeInputManager::GetOnlySingleNode(AlphArray(3),
                                                                                        ErrorsFound,
                                                                                        DataIPShortCuts::cCurrentModuleObject,
                                                                                        AlphArray(1),
                                                                                        DataLoopNode::NodeType_Water,
                                                                                        DataLoopNode::NodeConnectionType_Outlet,
                                                                                        1,
                                                                                        DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Heat Recovery Nodes");

                    FuelCell(thisFuelCell).StackCooler.TstackNom = NumArray(1);
                    FuelCell(thisFuelCell).StackCooler.TstackActual = NumArray(2);
                    FuelCell(thisFuelCell).StackCooler.r0 = NumArray(3);
                    FuelCell(thisFuelCell).StackCooler.r1 = NumArray(4);
                    FuelCell(thisFuelCell).StackCooler.r2 = NumArray(5);
                    FuelCell(thisFuelCell).StackCooler.r3 = NumArray(6);
                    FuelCell(thisFuelCell).StackCooler.MdotStackCoolant = NumArray(7);
                    FuelCell(thisFuelCell).StackCooler.UAs_cool = NumArray(8);
                    FuelCell(thisFuelCell).StackCooler.Fs_cogen = NumArray(9);
                    FuelCell(thisFuelCell).StackCooler.As_cogen = NumArray(10);
                    FuelCell(thisFuelCell).StackCooler.MdotCogenNom = NumArray(11);
                    FuelCell(thisFuelCell).StackCooler.hCogenNom = NumArray(12);
                    FuelCell(thisFuelCell).StackCooler.ns = NumArray(13);
                    FuelCell(thisFuelCell).StackCooler.PstackPumpEl = NumArray(14);
                    FuelCell(thisFuelCell).StackCooler.PmpPowerLossFactor = NumArray(15);
                    FuelCell(thisFuelCell).StackCooler.f0 = NumArray(16);
                    FuelCell(thisFuelCell).StackCooler.f1 = NumArray(17);
                    FuelCell(thisFuelCell).StackCooler.f1 = NumArray(18);

                    FuelCell(thisFuelCell).StackCooler.StackCoolerPresent = true;

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

        for (int GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum) {
            SetupOutputVariable("Generator Produced Electric Power",
                                OutputProcessor::Unit::W,
                                FuelCell(GeneratorNum).Report.ACPowerGen,
                                "System",
                                "Average",
                                FuelCell(GeneratorNum).Name);
            SetupOutputVariable("Generator Produced Electric Energy",
                                OutputProcessor::Unit::J,
                                FuelCell(GeneratorNum).Report.ACEnergyGen,
                                "System",
                                "Sum",
                                FuelCell(GeneratorNum).Name,
                                _,
                                "ElectricityProduced",
                                "COGENERATION",
                                _,
                                "Plant");
            SetupOutputVariable("Generator Produced Thermal Rate",
                                OutputProcessor::Unit::W,
                                FuelCell(GeneratorNum).Report.qHX,
                                "System",
                                "Average",
                                FuelCell(GeneratorNum).Name);
            SetupOutputVariable("Generator Produced Thermal Energy",
                                OutputProcessor::Unit::J,
                                FuelCell(GeneratorNum).Report.HXenergy,
                                "System",
                                "Sum",
                                FuelCell(GeneratorNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "COGENERATION",
                                _,
                                "Plant");

            SetupOutputVariable("Generator Fuel HHV Basis Energy",
                                OutputProcessor::Unit::J,
                                FuelCell(GeneratorNum).Report.FuelEnergyHHV,
                                "System",
                                "Sum",
                                FuelCell(GeneratorNum).Name,
                                _,
                                "Gas",
                                "COGENERATION",
                                _,
                                "Plant");
            SetupOutputVariable("Generator Fuel HHV Basis Rate",
                                OutputProcessor::Unit::W,
                                FuelCell(GeneratorNum).Report.FuelEnergyUseRateHHV,
                                "System",
                                "Average",
                                FuelCell(GeneratorNum).Name);

            SetupOutputVariable("Generator Zone Sensible Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                FuelCell(GeneratorNum).Report.SkinLossPower,
                                "System",
                                "Average",
                                FuelCell(GeneratorNum).Name);
            SetupOutputVariable("Generator Zone Sensible Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                FuelCell(GeneratorNum).Report.SkinLossEnergy,
                                "System",
                                "Sum",
                                FuelCell(GeneratorNum).Name);
            SetupOutputVariable("Generator Zone Convection Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                FuelCell(GeneratorNum).Report.SkinLossConvect,
                                "System",
                                "Average",
                                FuelCell(GeneratorNum).Name);
            SetupOutputVariable("Generator Zone Radiation Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                FuelCell(GeneratorNum).Report.SkinLossRadiat,
                                "System",
                                "Average",
                                FuelCell(GeneratorNum).Name);
            if (FuelCell(GeneratorNum).FCPM.ZoneID > 0) {
                SetupZoneInternalGain(FuelCell(GeneratorNum).FCPM.ZoneID,
                                      "Generator:FuelCell",
                                      FuelCell(GeneratorNum).Name,
                                      DataHeatBalance::IntGainTypeOf_GeneratorFuelCell,
                                      FuelCell(GeneratorNum).Report.SkinLossConvect,
                                      _,
                                      FuelCell(GeneratorNum).Report.SkinLossRadiat);
            }

            if (DataGlobals::DisplayAdvancedReportVariables) { // show extra data originally needed for detailed comparative testing
                SetupOutputVariable("Generator Air Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TairInlet,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Power Module Entering Air Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TairIntoFCPM,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Air Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotAir,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Power Module Entering Air Enthalpy",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.TotAirInEnthalphy,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Blower Electric Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.BlowerPower,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Blower Electric Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.BlowerEnergy,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Blower Skin Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.BlowerSkinLoss,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Fuel Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TfuelInlet,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Power Module Entering Fuel Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TfuelIntoFCPM,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotFuel,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Consumption LHV Basis Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.FuelEnergyLHV,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Consumption Rate LHV Basis",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.FuelEnergyUseRateLHV,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Power Module Entering Fuel Enthalpy",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.TotFuelInEnthalpy,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Compressor Electric Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.FuelCompressPower,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Compressor Electric Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.FuelCompressEnergy,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Compressor Skin Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.FuelCompressSkinLoss,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Fuel Reformer Water Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TwaterInlet,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Power Module Entering Reforming Water Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TwaterIntoFCPM,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Reformer Water Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotWater,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Reformer Water Pump Electric Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.WaterPumpPower,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Reformer Water Pump Electric Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.WaterPumpEnergy,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Power Module Entering Reforming Water Enthalpy",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.WaterIntoFCPMEnthalpy,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Product Gas Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.TprodGas,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas Enthalpy",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.EnthalProdGas,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotProdGas,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas Ar Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotProdAr,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas CO2 Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotProdCO2,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas H2O Vapor Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotProdH2O,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas N2 Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotProdN2,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Product Gas O2 Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.NdotProdO2,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Heat Recovery Exit Gas Temperature",
                                    OutputProcessor::Unit::C,
                                    FuelCell(GeneratorNum).Report.THXexh,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Heat Recovery Exit Gas H2O Vapor Fraction",
                                    OutputProcessor::Unit::None,
                                    FuelCell(GeneratorNum).Report.WaterVaporFractExh,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Heat Recovery Water Condensate Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    FuelCell(GeneratorNum).Report.CondensateRate,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Inverter Loss Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.PCUlosses,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Produced DC Electric Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.DCPowerGen,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator DC Power Efficiency",
                                    OutputProcessor::Unit::None,
                                    FuelCell(GeneratorNum).Report.DCPowerEff,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Electric Storage Charge State",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.ElectEnergyinStorage,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name); //? 'Sum'
                SetupOutputVariable("Generator DC Storage Charging Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.StoredPower,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator DC Storage Charging Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.StoredEnergy,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator DC Storage Discharging Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.DrawnPower,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator DC Storage Discharging Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.DrawnEnergy,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Ancillary AC Electric Power",
                                    OutputProcessor::Unit::W,
                                    FuelCell(GeneratorNum).Report.ACancillariesPower,
                                    "System",
                                    "Average",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Ancillary AC Electric Energy",
                                    OutputProcessor::Unit::J,
                                    FuelCell(GeneratorNum).Report.ACancillariesEnergy,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);

                SetupOutputVariable("Generator Fuel Cell Model Iteration Count",
                                    OutputProcessor::Unit::None,
                                    FuelCell(GeneratorNum).Report.SeqSubstIterations,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
                SetupOutputVariable("Generator Root Solver Iteration Count",
                                    OutputProcessor::Unit::None,
                                    FuelCell(GeneratorNum).Report.RegulaFalsiIterations,
                                    "System",
                                    "Sum",
                                    FuelCell(GeneratorNum).Name);
            }
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

        // begin controls block to be moved out to GeneratorDynamics module
        // If no loop demand or Generator OFF, return
        if (!RunFlag) {

            // TODO zero out terms as appropriate

            if (FuelCell(GeneratorNum).FCPM.HasBeenOn) {
                // FuelCell just now beginning to shut down,

                // set Day and Time of Last Shut Down
                FuelCell(GeneratorNum).FCPM.FractionalDayofLastShutDown =
                    double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;
                FuelCell(GeneratorNum).FCPM.HasBeenOn = false;

                if (FuelCell(GeneratorNum).FCPM.ShutDownTime > 0.0) FuelCell(GeneratorNum).FCPM.DuringShutDown = true;
            }

            // TODO  check to see if still in shut down mode and using fuel.
            if (FuelCell(GeneratorNum).FCPM.DuringShutDown) {
            }

            return;
        }

        if (!FuelCell(GeneratorNum).FCPM.HasBeenOn) {
            // fuel cell just turned on
            // set Day and Time of Last STart Up

            FuelCell(GeneratorNum).FCPM.FractionalDayofLastStartUp =
                double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;

            FuelCell(GeneratorNum).FCPM.HasBeenOn = true;
            ++FuelCell(GeneratorNum).FCPM.NumCycles; // increment cycling counter

            if (FuelCell(GeneratorNum).FCPM.StartUpTime > 0.0) FuelCell(GeneratorNum).FCPM.DuringStartUp = true;
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
                FuelCell(GeneratorNum).FigurePowerConditioningLosses(PoutofInverter, PpcuLosses);
                FuelCell(GeneratorNum).FigureACAncillaries(PacAncillariesTotal);
                Pdemand = MyLoad + PacAncillariesTotal + PpcuLosses;
            } else {
                // control Step 1a: Figure ancillary AC power draws
                FuelCell(GeneratorNum).FigureACAncillaries(PacAncillariesTotal);
                Pdemand = MyLoad + PacAncillariesTotal;
                // Control Step 1b: Calculate losses associated with Power conditioning
                FuelCell(GeneratorNum).FigurePowerConditioningLosses(Pdemand, PpcuLosses);
                Pdemand += PpcuLosses;
                Pel = Pdemand;
            }

            FuelCell(GeneratorNum).Inverter.PCUlosses = PpcuLosses;

            // Control step 2: adjust for transient and startup/shut down constraints

            Real64 PelDiff;
            bool ConstrainedFCPMTrans = false;
            FuelCell(GeneratorNum).FigureTransientConstraints(Pel, ConstrainedFCPMTrans, PelDiff);

            // Control step 3: adjust for max and min limits on Pel

            if (Pel < FuelCell(GeneratorNum).FCPM.PelMin) {
                PelDiff += (FuelCell(GeneratorNum).FCPM.PelMin - Pel);
                Pel = FuelCell(GeneratorNum).FCPM.PelMin;

                ConstrainedFCPM = true;
            }
            if (Pel > FuelCell(GeneratorNum).FCPM.PelMax) {
                PelDiff += (FuelCell(GeneratorNum).FCPM.PelMax - Pel);
                Pel = FuelCell(GeneratorNum).FCPM.PelMax;
                ConstrainedFCPM = true;
            }
            if (ConstrainedFCPM) {
            }

            FuelCell(GeneratorNum).FCPM.Pel = Pel;
            // Now calculate FC models.  return to controls and batter after

            // Calculation Step 1. Determine electrical Efficiency Eel

            Real64 Eel = 0.0;
            if (FuelCell(GeneratorNum).FCPM.EffMode == DataGenerators::NormalizedCurveMode) {
                // Equation (8) in FuelCell Spec modified for normalized curve

                Eel = CurveManager::CurveValue(FuelCell(GeneratorNum).FCPM.EffCurveID, Pel / FuelCell(GeneratorNum).FCPM.NomPel) *
                      FuelCell(GeneratorNum).FCPM.NomEff *
                      (1.0 - FuelCell(GeneratorNum).FCPM.NumCycles * FuelCell(GeneratorNum).FCPM.CyclingDegradRat) *
                      (1.0 - max((FuelCell(GeneratorNum).FCPM.NumRunHours - FuelCell(GeneratorNum).FCPM.ThreshRunHours), 0.0) *
                                 FuelCell(GeneratorNum).FCPM.OperateDegradRat);

            } else if (FuelCell(GeneratorNum).FCPM.EffMode == DataGenerators::DirectCurveMode) {
                // Equation (8) in FuelCell Spec
                Eel = CurveManager::CurveValue(FuelCell(GeneratorNum).FCPM.EffCurveID, Pel) *
                      (1.0 - FuelCell(GeneratorNum).FCPM.NumCycles * FuelCell(GeneratorNum).FCPM.CyclingDegradRat) *
                      (1.0 - max((FuelCell(GeneratorNum).FCPM.NumRunHours - FuelCell(GeneratorNum).FCPM.ThreshRunHours), 0.0) *
                                 FuelCell(GeneratorNum).FCPM.OperateDegradRat);
            }

            FuelCell(GeneratorNum).FCPM.Eel = Eel;
            // Calculation Step 2. Determine fuel rate

            // fuel flow rate
            Real64 NdotFuel = Pel / (Eel * DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).LHV * 1000000.0); // Eq. 10 solved for Ndot

            FuelCell(GeneratorNum).FCPM.NdotFuel = NdotFuel;
            if (Pel <= 0.0) {
                // TODO zero stuff before leaving
                Pel = 0.0;
                FuelCell(GeneratorNum).FCPM.Pel = 0.0;
                return;
            } else {

                FuelCell(GeneratorNum).FCPM.Pel = Pel;
            }

            // Calculation Step 3. Determine Air rate

            if (FuelCell(GeneratorNum).AirSup.AirSupRateMode == DataGenerators::ConstantStoicsAirRat) { // MEthod 1
                // molar rate coeff working variable
                Real64 NdotO2 = DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).StoicOxygenRate * FuelCell(GeneratorNum).FCPM.NdotFuel *
                         FuelCell(GeneratorNum).AirSup.Stoics;

                FuelCell(GeneratorNum).FCPM.NdotAir = NdotO2 / FuelCell(GeneratorNum).AirSup.O2fraction;

            } else if (FuelCell(GeneratorNum).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofPel) { // MEthod 2

                FuelCell(GeneratorNum).FCPM.NdotAir = CurveManager::CurveValue(FuelCell(GeneratorNum).AirSup.AirFuncPelCurveID, Pel) *
                                                      (1 + FuelCell(GeneratorNum).AirSup.AirTempCoeff * FuelCell(GeneratorNum).AirSup.TairIntoFCPM);

            } else if (FuelCell(GeneratorNum).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofNdot) { // method 3
                FuelCell(GeneratorNum).FCPM.NdotAir =
                    CurveManager::CurveValue(FuelCell(GeneratorNum).AirSup.AirFuncNdotCurveID, FuelCell(GeneratorNum).FCPM.NdotFuel) *
                    (1 + FuelCell(GeneratorNum).AirSup.AirTempCoeff * FuelCell(GeneratorNum).AirSup.TairIntoFCPM);
            }

            // Calculation Step 4. fuel compressor power

            DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl =
                CurveManager::CurveValue(DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).CompPowerCurveID, FuelCell(GeneratorNum).FCPM.NdotFuel);

            // calculation Step 5, Fuel Compressor (need outlet temperature)

            if (DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).FuelTempMode == DataGenerators::FuelInTempFromNode) {

                DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress = DataLoopNode::Node(DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).NodeNum).Temp;

            } else if (DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).FuelTempMode == DataGenerators::FuelInTempSchedule) {

                DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress =
                        ScheduleManager::GetCurrentScheduleValue(DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).SchedNum);
            }

            //  evaluate  heat capacity at average temperature using shomate
            Real64 Cp;              // temp Heat Capacity, used in thermochemistry units of (J/mol K)
            Real64 Tavg = (DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress + DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM) / 2.0;
            FuelCell(GeneratorNum).FigureFuelHeatCap(Tavg, Cp); // Cp in (J/mol K)

            // calculate a Temp of fuel out of compressor and into power module

            if (FuelCell(GeneratorNum).FCPM.NdotFuel <= 0.0) { // just pass through, domain probably collapsed in modeling
                DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM = DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress;
            } else {
                DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM =
                    ((1.0 - DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).CompPowerLossFactor) *
                            DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl / (FuelCell(GeneratorNum).FCPM.NdotFuel * Cp * 1000.0)) +
                            DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoCompress; // 1000 Cp units mol-> kmol
            }
            // calc skin losses from fuel compressor
            DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).QskinLoss =
                    DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).CompPowerLossFactor * DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl;

            if (DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).QskinLoss < 0.0) {
                ShowWarningError("problem in FuelSupply.QskinLoss " + General::RoundSigDigits(DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).QskinLoss, 3));
                DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).QskinLoss = 0.0;
            }

            // calculate tatal fuel enthalpy coming into power module

            // (Hmolfuel in KJ/mol)
            Real64 Hmolfuel;        // temp enthalpy of fuel mixture in KJ/mol
            FuelCell(GeneratorNum).FigureFuelEnthalpy(DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).TfuelIntoFCPM, Hmolfuel);

            // units, NdotFuel in kmol/sec. Hmolfule in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            FuelCell(GeneratorNum).FCPM.TotFuelInEnthalphy = Hmolfuel * 1000.0 * FuelCell(GeneratorNum).FCPM.NdotFuel * 1000.0;

            // Calculation Step 6, water compressor calculations

            // calculate water consumption

            FuelCell(GeneratorNum).FCPM.NdotLiqwater =
                CurveManager::CurveValue(FuelCell(GeneratorNum).WaterSup.WaterSupRateCurveID, FuelCell(GeneratorNum).FCPM.NdotFuel);

            // set inlet temp.  (could move to init)

            {
                auto const SELECT_CASE_var(FuelCell(GeneratorNum).WaterSup.WaterTempMode);

                if (SELECT_CASE_var == DataGenerators::WaterInReformMains) {

                    FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress = DataEnvironment::WaterMainsTemp;

                } else if ((SELECT_CASE_var == DataGenerators::WaterInReformAirNode) || (SELECT_CASE_var == DataGenerators::WaterInReformWaterNode)) {

                    FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress = DataLoopNode::Node(FuelCell(GeneratorNum).WaterSup.NodeNum).Temp;

                } else if (SELECT_CASE_var == DataGenerators::WaterInReformSchedule) {

                    FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress = ScheduleManager::GetCurrentScheduleValue(FuelCell(GeneratorNum).WaterSup.SchedNum);
                }
            }

            FuelCell(GeneratorNum).WaterSup.PwaterCompEl =
                CurveManager::CurveValue(FuelCell(GeneratorNum).WaterSup.PmpPowerCurveID, FuelCell(GeneratorNum).FCPM.NdotLiqwater);

            // 75.325  J/mol K Water at 0.1 MPa and 298 K, reference NIST WEBBOOK
            Real64 CpWater;         // heat capacity of water in molar units
            FigureLiquidWaterHeatCap(FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress, CpWater);

            if (FuelCell(GeneratorNum).FCPM.NdotLiqwater <= 0.0) { // just pass through, domain probably collapsed in modeling
                FuelCell(GeneratorNum).WaterSup.TwaterIntoFCPM = FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress;
            } else {

                FuelCell(GeneratorNum).WaterSup.TwaterIntoFCPM =
                    ((1 - FuelCell(GeneratorNum).WaterSup.PmpPowerLossFactor) * FuelCell(GeneratorNum).WaterSup.PwaterCompEl /
                     (FuelCell(GeneratorNum).FCPM.NdotLiqwater * CpWater * 1000.0)) +
                    FuelCell(GeneratorNum).WaterSup.TwaterIntoCompress;
            }

            FuelCell(GeneratorNum).WaterSup.QskinLoss =
                FuelCell(GeneratorNum).WaterSup.PmpPowerLossFactor * FuelCell(GeneratorNum).WaterSup.PwaterCompEl;

            if (FuelCell(GeneratorNum).WaterSup.QskinLoss < 0.0) {
                // write(*,*) 'problem in WaterSup%QskinLoss ',FuelCell(GeneratorNum)%WaterSup%QskinLoss
                FuelCell(GeneratorNum).WaterSup.QskinLoss = 0.0;
            }

            Real64 HLiqWater;       // temp enthalpy of liquid water in KJ/mol   No Formation
            FigureLiquidWaterEnthalpy(FuelCell(GeneratorNum).WaterSup.TwaterIntoFCPM, HLiqWater); // HLiqWater in KJ/mol

            FuelCell(GeneratorNum).FCPM.WaterInEnthalpy = FuelCell(GeneratorNum).FCPM.NdotLiqwater * HLiqWater * 1000.0 * 1000.0;

            // Calculation Step 7, Air compressor

            FuelCell(GeneratorNum).AirSup.TairIntoBlower = DataLoopNode::Node(FuelCell(GeneratorNum).AirSup.SupNodeNum).Temp;

            FuelCell(GeneratorNum).AirSup.PairCompEl =
                CurveManager::CurveValue(FuelCell(GeneratorNum).AirSup.BlowerPowerCurveID, FuelCell(GeneratorNum).FCPM.NdotAir);

            Tavg = (FuelCell(GeneratorNum).AirSup.TairIntoBlower + FuelCell(GeneratorNum).AirSup.TairIntoFCPM) / 2.0;

            FuelCell(GeneratorNum).FigureAirHeatCap(Tavg, Cp); // Cp in (J/mol K)

            // if PEMFC with stack cooler, then calculate stack cooler impacts
            if (FuelCell(GeneratorNum).StackCooler.StackCoolerPresent) {

                FuelCell(GeneratorNum).StackCooler.qs_cool =
                    (FuelCell(GeneratorNum).StackCooler.r0 +
                     FuelCell(GeneratorNum).StackCooler.r1 *
                         (FuelCell(GeneratorNum).StackCooler.TstackActual - FuelCell(GeneratorNum).StackCooler.TstackNom)) *
                    (1 + FuelCell(GeneratorNum).StackCooler.r2 * Pel + FuelCell(GeneratorNum).StackCooler.r3 * Pel * Pel) * Pel;

                FuelCell(GeneratorNum).FCPM.QdotStackCool = FuelCell(GeneratorNum).StackCooler.qs_cool;
            }

            // Figure heat recovery from Electrical Storage, power conditioning, and auxiliary burner

            {
                auto const SELECT_CASE_var(FuelCell(GeneratorNum).AirSup.IntakeRecoveryMode);

                if (SELECT_CASE_var == DataGenerators::RecoverBurnInvertBatt) {
                    FuelCell(GeneratorNum).AirSup.QintakeRecovery = FuelCell(GeneratorNum).AuxilHeat.QairIntake +
                                                                    FuelCell(GeneratorNum).ElecStorage.QairIntake +
                                                                    FuelCell(GeneratorNum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverAuxiliaryBurner) {
                    FuelCell(GeneratorNum).AirSup.QintakeRecovery = FuelCell(GeneratorNum).AuxilHeat.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverInverterBatt) {
                    FuelCell(GeneratorNum).AirSup.QintakeRecovery =
                        FuelCell(GeneratorNum).ElecStorage.QairIntake + FuelCell(GeneratorNum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverInverter) {
                    FuelCell(GeneratorNum).AirSup.QintakeRecovery = FuelCell(GeneratorNum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverBattery) {
                    FuelCell(GeneratorNum).AirSup.QintakeRecovery = FuelCell(GeneratorNum).ElecStorage.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::NoRecoveryOnAirIntake) {
                    FuelCell(GeneratorNum).AirSup.QintakeRecovery = 0.0;
                }
            }

            if (FuelCell(GeneratorNum).FCPM.NdotAir <= 0.0) { // just pass through, domain probably collapased in modeling
                FuelCell(GeneratorNum).AirSup.TairIntoFCPM = FuelCell(GeneratorNum).AirSup.TairIntoBlower;

            } else {
                FuelCell(GeneratorNum).AirSup.TairIntoFCPM =
                    (((1 - FuelCell(GeneratorNum).AirSup.BlowerHeatLossFactor) * FuelCell(GeneratorNum).AirSup.PairCompEl +
                      FuelCell(GeneratorNum).AirSup.QintakeRecovery) /
                     (FuelCell(GeneratorNum).FCPM.NdotAir * Cp * 1000.0)) +
                    FuelCell(GeneratorNum).AirSup.TairIntoBlower; // 1000 Cp units mol-> kmol
            }

            FuelCell(GeneratorNum).AirSup.QskinLoss = FuelCell(GeneratorNum).AirSup.BlowerHeatLossFactor * FuelCell(GeneratorNum).AirSup.PairCompEl;

            if (FuelCell(GeneratorNum).AirSup.QskinLoss < 0.0) {
                //   write(*,*) 'problem in AirSup%QskinLoss ', FuelCell(GeneratorNum)%AirSup%QskinLoss
                ShowWarningError("problem in AirSup%QskinLoss " + General::RoundSigDigits(FuelCell(GeneratorNum).AirSup.QskinLoss, 3));
                FuelCell(GeneratorNum).AirSup.QskinLoss = 0.0;
            }

            Real64 Hmolair;         // temp enthalpy of air mixture in KJ/mol
            FuelCell(GeneratorNum).FigureAirEnthalpy(FuelCell(GeneratorNum).AirSup.TairIntoFCPM, Hmolair); // (Hmolair in KJ/mol)

            // units, NdotAir in kmol/sec.; Hmolfuel in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            FuelCell(GeneratorNum).FCPM.TotAirInEnthalphy = Hmolair * 1000.0 * FuelCell(GeneratorNum).FCPM.NdotAir * 1000.0;

            // calculation Step 8, Figure Product Gases

            // figure stoic N dot for air
            Real64 NdotO2 = DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).StoicOxygenRate * FuelCell(GeneratorNum).FCPM.NdotFuel;

            // Air in excess of match for fuel
            Real64 NdotStoicAir = NdotO2 / FuelCell(GeneratorNum).AirSup.O2fraction;

            // figure excess air rate

            // Air in excess of match for fuel
            Real64 NdotExcessAir = FuelCell(GeneratorNum).FCPM.NdotAir - NdotStoicAir;

            if (NdotExcessAir < 0) { // can't meet stoichiometric fuel reaction

                ShowWarningError("Air flow rate into fuel cell is too low for stoichiometric fuel reaction");
                ShowContinueError("Increase air flow in GENERATOR:FC:AIR SUPPLY object:" + FuelCell(GeneratorNum).AirSup.Name);
            }

            // figure CO2 and Water rate from products (coefs setup during one-time processing in gas phase library )

            // CO2 from reaction
            Real64 NdotCO2ProdGas = FuelCell(GeneratorNum).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).CO2ProductGasCoef;

            // Water from reaction
            Real64 NdotH20ProdGas = FuelCell(GeneratorNum).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).H20ProductGasCoef;

            //  set product gas constituent fractions  (assume five usual components)
            Real64 NdotCO2 = 0.0;         // temp CO2 molar rate coef product gas stream
            Real64 NdotN2 = 0.0;          // temp Nitrogen rate coef product gas stream
            Real64 Ndot02 = 0.0;          // temp Oxygen rate coef product gas stream
            Real64 NdotH20 = 0.0;         // temp Water rate coef product gas stream
            Real64 NdotAr = 0.0;          // temp Argon rate coef product gas stream

            // Product gas constituents are fixed (not a user defined thing)

            for (int thisGas = 1; thisGas <= FuelCell(GeneratorNum).AirSup.NumConstituents; ++thisGas) {

                {
                    auto const SELECT_CASE_var(FuelCell(GeneratorNum).AirSup.GasLibID(thisGas));

                    if (SELECT_CASE_var == 1) {
                        // all the CO2 coming in plus the new CO2 from reactions
                        NdotCO2 = NdotCO2ProdGas + FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas) * FuelCell(GeneratorNum).FCPM.NdotAir;

                    } else if (SELECT_CASE_var == 2) {
                        // all the nitrogen coming in
                        NdotN2 = FuelCell(GeneratorNum).FCPM.NdotAir * FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas);

                    } else if (SELECT_CASE_var == 3) {
                        // all the oxygen in the excess air stream
                        Ndot02 = NdotExcessAir * FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas);

                    } else if (SELECT_CASE_var == 4) {
                        // all the H20 coming in plus the new H20 from reactions and the H20 from water used in reforming
                        NdotH20 = NdotH20ProdGas + FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas) * FuelCell(GeneratorNum).FCPM.NdotAir;
                        //+ FuelCell(GeneratorNum)%FCPM%NdotLiqwater

                    } else if (SELECT_CASE_var == 5) {
                        // all the argon coming in.
                        NdotAr = FuelCell(GeneratorNum).FCPM.NdotAir * FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisGas);

                    } else {
                    }
                }
            }

            FuelCell(GeneratorNum).FCPM.NdotProdGas = NdotCO2 + NdotN2 + Ndot02 + NdotH20 + NdotAr;

            // now that we have the total, figure molar fractions

            FuelCell(GeneratorNum).FCPM.ConstitMolalFract(1) = NdotCO2 / FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the nitrogen comming in
            FuelCell(GeneratorNum).FCPM.ConstitMolalFract(2) = NdotN2 / FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the oxygen in the excess air stream
            FuelCell(GeneratorNum).FCPM.ConstitMolalFract(3) = Ndot02 / FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the H20 comming in plus the new H20 from reactions and the H20 from water used in reforming
            FuelCell(GeneratorNum).FCPM.ConstitMolalFract(4) = NdotH20 / FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // all the argon coming in.
            FuelCell(GeneratorNum).FCPM.ConstitMolalFract(5) = NdotAr / FuelCell(GeneratorNum).FCPM.NdotProdGas;

            // HmolProdGases KJ/mol)
            Real64 HmolProdGases;   // enthalpy of product gas mixture in KJ/mol
            FuelCell(GeneratorNum).FigureProductGasesEnthalpy(FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM, HmolProdGases);

            // units, NdotProdGas in kmol/sec.; HmolProdGases in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            FuelCell(GeneratorNum).FCPM.TotProdGasEnthalphy = HmolProdGases * 1000.0 * FuelCell(GeneratorNum).FCPM.NdotProdGas * 1000.0;

            // calculation Step 9, Figure Skin lossess

            if (FuelCell(GeneratorNum).FCPM.SkinLossMode == DataGenerators::ConstantRateSkinLoss) {
                // do nothing just use QdotSkin

            } else if (FuelCell(GeneratorNum).FCPM.SkinLossMode == DataGenerators::UADTSkinLoss) {

                // get zone air temp
                if (FuelCell(GeneratorNum).FCPM.ZoneID > 0) {
                    FuelCell(GeneratorNum).FCPM.QdotSkin = FuelCell(GeneratorNum).FCPM.UAskin *
                                                           (FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM - DataHeatBalFanSys::ZT(FuelCell(GeneratorNum).FCPM.ZoneID));
                }

            } else if (FuelCell(GeneratorNum).FCPM.SkinLossMode == DataGenerators::QuadraticFuelNdotSkin) {

                FuelCell(GeneratorNum).FCPM.QdotSkin = CurveManager::CurveValue(FuelCell(GeneratorNum).FCPM.SkinLossCurveID, FuelCell(GeneratorNum).FCPM.NdotFuel);
            }

            // calculation Step 10, AC FCPM power ancillaries

            FuelCell(GeneratorNum).FCPM.PelancillariesAC =
                FuelCell(GeneratorNum).FCPM.ANC0 + FuelCell(GeneratorNum).FCPM.ANC1 * FuelCell(GeneratorNum).FCPM.NdotFuel;

            // calculation Step 11, Dilution air
            FuelCell(GeneratorNum).FigureAirEnthalpy(FuelCell(GeneratorNum).AirSup.TairIntoBlower, Hmolair); // (Hmolair in KJ/mol)

            // units, NdotDilutionAir in kmol/sec.; Hmolair in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            FuelCell(GeneratorNum).FCPM.DilutionAirInEnthalpy = Hmolair * 1000.0 * FuelCell(GeneratorNum).FCPM.NdotDilutionAir * 1000.0;
            FuelCell(GeneratorNum).FCPM.DilutionAirOutEnthalpy =
                FuelCell(GeneratorNum).FCPM.DilutionAirInEnthalpy + FuelCell(GeneratorNum).FCPM.StackHeatLossToDilution;

            // calculation Step 12, Calculate Reforming water out enthalpy
            Real64 HGasWater;       // temp enthalpy of gaseous water in KJ/mol  No Formation
            FigureGaseousWaterEnthalpy(FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM, HGasWater);

            FuelCell(GeneratorNum).FCPM.WaterOutEnthalpy = HGasWater * 1000.0 * FuelCell(GeneratorNum).FCPM.NdotLiqwater * 1000.0;

            // calculation Step 13, Calculate Heat balance
            //    move all terms in Equation 7 to RHS and calculate imbalance

            Real64 MagofImbalance = -FuelCell(GeneratorNum).FCPM.TotFuelInEnthalphy - FuelCell(GeneratorNum).FCPM.TotAirInEnthalphy -
                             FuelCell(GeneratorNum).FCPM.WaterInEnthalpy - FuelCell(GeneratorNum).FCPM.DilutionAirInEnthalpy -
                             FuelCell(GeneratorNum).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).LHV * 1000000.0 -
                             FuelCell(GeneratorNum).FCPM.PelancillariesAC + FuelCell(GeneratorNum).FCPM.Pel +
                             FuelCell(GeneratorNum).FCPM.TotProdGasEnthalphy + FuelCell(GeneratorNum).FCPM.WaterOutEnthalpy +
                             FuelCell(GeneratorNum).FCPM.QdotStackCool + FuelCell(GeneratorNum).FCPM.QdotSkin +
                             FuelCell(GeneratorNum).FCPM.DilutionAirOutEnthalpy;

            // Now find a new total prod Gas Enthalphy that would result in an energy balance
            // TODO check signs...
            Real64 tmpTotProdGasEnthalpy = FuelCell(GeneratorNum).FCPM.TotProdGasEnthalphy - MagofImbalance;

            // solve for a new TprodGasLeavingFCPM using regula falsi method

            Real64 Acc = 0.01;     // guessing need to refine
            int MaxIter = 150;  // guessing need to refine
            SolverFlag = 0; // init
            Array1D<Real64> Par(3); // parameters passed in to SolveRoot
            Par(1) = double(GeneratorNum);
            Par(2) = tmpTotProdGasEnthalpy;
            Par(3) = FuelCell(GeneratorNum).FCPM.NdotProdGas;
            Real64 tmpTprodGas = FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM;
            General::SolveRoot(Acc, MaxIter, SolverFlag, tmpTprodGas, FuelCellProductGasEnthResidual, DataGenerators::MinProductGasTemp, DataGenerators::MaxProductGasTemp, Par);

            if (SolverFlag == -2) {

                ShowWarningError("CalcFuelCellGeneratorModel: Root Solver problem, flag = -2, check signs, all positive");
            }
            if (SolverFlag == -1) {
                ShowWarningError("CalcFuelCellGeneratorModel: Root Solver problem, flag = -1, check accuracy and iterations, did not converge");
            }
            if (SolverFlag > 0) {
                FuelCell(GeneratorNum).FCPM.TprodGasLeavingFCPM = tmpTprodGas;
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

            if (FuelCell(GeneratorNum).Inverter.EffMode == DataGenerators::InverterEffConstant) {

                PpcuLosses = (1.0 - FuelCell(GeneratorNum).Inverter.ConstEff) * PintoInverter;
            }

            if (FuelCell(GeneratorNum).Inverter.EffMode == DataGenerators::InverterEffQuadratic) {

                PpcuLosses = (1.0 - CurveManager::CurveValue(FuelCell(GeneratorNum).Inverter.EffQuadraticCurveID, PintoInverter)) * PintoInverter;
            }

            PoutofInverter = PintoInverter - PpcuLosses;

            FuelCell(GeneratorNum).ACPowerGen = PoutofInverter - FuelCell(GeneratorNum).FCPM.PelancillariesAC -
                                                FuelCell(GeneratorNum).AirSup.PairCompEl - DataGenerators::FuelSupply(FuelCell(GeneratorNum).FuelSupNum).PfuelCompEl -
                                                FuelCell(GeneratorNum).WaterSup.PwaterCompEl;
            FuelCell(GeneratorNum).Inverter.PCUlosses = PpcuLosses;
            // model assumes air intake is drawn over power conditioner to recovery heat
            FuelCell(GeneratorNum).Inverter.QairIntake = FuelCell(GeneratorNum).Inverter.PCUlosses;

            CalcFuelCellAuxHeater(GeneratorNum);

            CalcFuelCellGenHeatRecovery(GeneratorNum);
            // calculation Step 11, If imbalance below threshold, then exit out of do loop.

            if ((std::abs(MagofImbalance) < std::abs(DataGenerators::ImBalanceTol * FuelCell(GeneratorNum).FCPM.Pel)) && (iter > 2)) {
                break;
            }

        } // sequential substitution loop

        FuelCell(GeneratorNum).FCPM.SeqSubstitIter = iter;
        FuelCell(GeneratorNum).FCPM.RegulaFalsiIter = SolverFlag;
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

        Real64 tmpPdraw = 0.0;
        Real64 tmpPcharge = 0.0;
        bool drawing = false;  // true if drawing power
        bool charging = false;  // true if charging
        Constrained = false;

        // step 1 figure out what is desired of electrical storage system

        if (FuelCell(Num).FCPM.Pel < (Pdemand)) {
            // draw from storage
            tmpPdraw = (Pdemand)-FuelCell(Num).FCPM.Pel;
            drawing = true;
        }

        if (FuelCell(Num).FCPM.Pel > (Pdemand)) {
            // add to storage
            tmpPcharge = FuelCell(Num).FCPM.Pel - (Pdemand);
            charging = true;
        }

        //  step 2, figure out what is possible for electrical storage draws/charges

        if (charging) {

            if (FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::SimpleEffConstraints) {

                if (FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge >= FuelCell(Num).ElecStorage.NominalEnergyCapacity) {
                    // storage full!  no more allowed!
                    PgridOverage = tmpPcharge;
                    tmpPcharge = 0.0;
                    Constrained = true;
                }
                if (tmpPcharge > FuelCell(Num).ElecStorage.MaxPowerStore) {
                    PgridOverage = tmpPcharge - FuelCell(Num).ElecStorage.MaxPowerStore;
                    tmpPcharge = FuelCell(Num).ElecStorage.MaxPowerStore;
                    Constrained = true;
                }

                // now add energy to storage from charging
                if ((FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge +
                     tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * FuelCell(Num).ElecStorage.EnergeticEfficCharge) <
                    FuelCell(Num).ElecStorage.NominalEnergyCapacity) {

                    FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge +
                        tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * FuelCell(Num).ElecStorage.EnergeticEfficCharge;
                } else { // would over charge this time step

                    tmpPcharge = (FuelCell(Num).ElecStorage.NominalEnergyCapacity - FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge) /
                                 (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * FuelCell(Num).ElecStorage.EnergeticEfficCharge);
                    Constrained = true;
                    FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge +
                        tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * FuelCell(Num).ElecStorage.EnergeticEfficCharge;
                }

                // losses go into QairIntake
                FuelCell(Num).ElecStorage.QairIntake = tmpPcharge * (1.0 - FuelCell(Num).ElecStorage.EnergeticEfficCharge);

            } else if (FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterManwellMcGowan) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ");

            } else if (FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterySaupe) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ");

            } else {

                // should not come here
            }

            Pstorage = tmpPcharge;

        } // charging

        if (drawing) {
            if (FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::SimpleEffConstraints) {

                if (FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge <= 0.0) {
                    // storage empty  no more allowed!
                    tmpPdraw = 0.0;
                    Constrained = true;
                    drawing = false;
                }
                if (tmpPdraw > FuelCell(Num).ElecStorage.MaxPowerDraw) {
                    tmpPdraw = FuelCell(Num).ElecStorage.MaxPowerDraw;
                    Constrained = true;
                }

                // now take energy from storage by drawing  (amplified by energetic effic)
                if ((FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge -
                     tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / FuelCell(Num).ElecStorage.EnergeticEfficDischarge) > 0.0) {

                    FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge -
                        tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / FuelCell(Num).ElecStorage.EnergeticEfficDischarge;
                } else { // would over drain storage this timestep so reduce tmpPdraw
                    tmpPdraw = FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge * FuelCell(Num).ElecStorage.EnergeticEfficDischarge /
                               (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
                    FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge =
                        FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge -
                        tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / FuelCell(Num).ElecStorage.EnergeticEfficDischarge;

                    Constrained = true;
                }
                // losses go into QairIntake
                FuelCell(Num).ElecStorage.QairIntake = tmpPdraw * (1.0 / FuelCell(Num).ElecStorage.EnergeticEfficDischarge - 1.0);
            } else if (FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterManwellMcGowan) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ");

            } else if (FuelCell(Num).ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterySaupe) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ");

            } else {

                // should not come here
            }

            Pstorage = -tmpPdraw;

        } // drawing

        if ((!charging) && (!drawing)) {

            FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge = FuelCell(Num).ElecStorage.LastTimeStepStateOfCharge;
            FuelCell(Num).ElecStorage.PelNeedFromStorage = 0.0;
            FuelCell(Num).ElecStorage.PelFromStorage = 0.0;
            FuelCell(Num).ElecStorage.QairIntake = 0.0;
        }

        if (Pstorage >= 0.0) {

            FuelCell(Num).ElecStorage.PelIntoStorage = Pstorage;
            FuelCell(Num).ElecStorage.PelFromStorage = 0.0;
        }
        if (Pstorage < 0.0) {

            FuelCell(Num).ElecStorage.PelIntoStorage = 0.0;
            FuelCell(Num).ElecStorage.PelFromStorage = -Pstorage;
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

        Real64 thisHmolalProdGases;
        int GeneratorNum = std::floor(Par(1));
        Real64 desiredHprodGases = Par(2);
        Real64 NdotProdGases = Par(3);

        FuelCell(GeneratorNum).FigureProductGasesEnthalpy(TprodGas, thisHmolalProdGases);

        Residuum = (thisHmolalProdGases * NdotProdGases * 1000000.0) - desiredHprodGases;

        return Residuum;
    }

    void FCDataStruct::FigureAirHeatCap(Real64 const FluidTemp, Real64 &Cp)
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

        Real64 tempCp = 0.0;

        Real64 const Tkel = (FluidTemp + DataGlobals::KelvinConv);          // temp for NASA eq. in Kelvin
        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (int thisConstit = 1; thisConstit <= this->AirSup.NumConstituents; ++thisConstit) {
            int gasID = this->AirSup.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            this->AirSup.ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {

                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            this->AirSup.ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FCDataStruct::FigureAirEnthalpy(Real64 const FluidTemp, Real64 &Hair)
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

        Real64 tempHair = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_4_Tsho(pow_4(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (int thisConstit = 1; thisConstit <= this->AirSup.NumConstituents; ++thisConstit) {
            int gasID = this->AirSup.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;
                    F = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateF;
                    H = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateH;

                    Real64 HairI = (A * Tsho + B * pow_2_Tsho / 2.0 + C * pow_3_Tsho / 3.0 + D * pow_4_Tsho / 4.0 - E / Tsho + F - H);

                    tempHair += HairI * this->AirSup.ConstitMolalFract(thisConstit);
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
                                this->AirSup.ConstitMolalFract(thisConstit);
                }
            }
        }

        Hair = tempHair;
    }

    void FCDataStruct::FigureFuelHeatCap(Real64 const FluidTemp, Real64 &Cp)
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

        Real64 tempCp = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (int thisConstit = 1; thisConstit <= DataGenerators::FuelSupply(this->FuelSupNum).NumConstituents; ++thisConstit) {
            int gasID = DataGenerators::FuelSupply(this->FuelSupNum).GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            DataGenerators::FuelSupply(this->FuelSupNum).ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            DataGenerators::FuelSupply(this->FuelSupNum).ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FCDataStruct::FigureFuelEnthalpy(Real64 const FluidTemp, Real64 &Hfuel)
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

        Real64 tempHfuel = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_4_Tsho(pow_4(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (int thisConstit = 1; thisConstit <= DataGenerators::FuelSupply(this->FuelSupNum).NumConstituents; ++thisConstit) {
            int gasID = DataGenerators::FuelSupply(this->FuelSupNum).GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {
                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;
                    F = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateF;
                    H = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateH;

                    Real64 HfuelI = (A * Tsho + B * pow_2_Tsho / 2.0 + C * pow_3_Tsho / 3.0 + D * pow_4_Tsho / 4.0 - E / Tsho + F - H);

                    tempHfuel += HfuelI * DataGenerators::FuelSupply(this->FuelSupNum).ConstitMolalFract(thisConstit);
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
                            DataGenerators::FuelSupply(this->FuelSupNum).ConstitMolalFract(thisConstit);
                }
            }
        }

        Hfuel = tempHfuel;
    }

    void FCDataStruct::FigureProductGasesEnthalpy(Real64 const FluidTemp, Real64 &HProdGases)
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

        Real64 tempHprodGases = 0.0;

        Real64 const pow_2_Tsho(pow_2(Tsho));
        Real64 const pow_3_Tsho(pow_3(Tsho));
        Real64 const pow_4_Tsho(pow_4(Tsho));
        Real64 const pow_2_Tkel(pow_2(Tkel));
        Real64 const pow_3_Tkel(pow_3(Tkel));
        Real64 const pow_4_Tkel(pow_4(Tkel));

        for (int thisConstit = 1; thisConstit <= 5; ++thisConstit) {
            int gasID = this->FCPM.GasLibID(thisConstit);
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
                            this->FCPM.ConstitMolalFract(thisConstit));
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
                            this->FCPM.ConstitMolalFract(thisConstit);
                }
            } // gasid > 0
        }

        HProdGases = tempHprodGases;
    }

    void FCDataStruct::FigureProductGasHeatCap(Real64 const FluidTemp, Real64 &Cp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 tempCp;
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

        for (int thisConstit = 1; thisConstit <= isize(this->FCPM.GasLibID); ++thisConstit) {
            int gasID = this->FCPM.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            this->FCPM.ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            this->FCPM.ConstitMolalFract(thisConstit);
                }
            }
        }

        Cp = tempCp;
    }

    void FCDataStruct::FigureAuxilHeatGasHeatCap(Real64 const FluidTemp, Real64 &Cp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 tempCp;
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

        for (int thisConstit = 1; thisConstit <= isize(this->AuxilHeat.GasLibID); ++thisConstit) {
            int gasID = this->AuxilHeat.GasLibID(thisConstit);
            if (gasID > 0) {
                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NISTShomate) {

                    A = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateA;
                    B = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateB;
                    C = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateC;
                    D = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateD;
                    E = DataGenerators::GasPhaseThermoChemistryData(gasID).ShomateE;

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) *
                            this->AuxilHeat.ConstitMolalFract(thisConstit));
                }

                if (DataGenerators::GasPhaseThermoChemistryData(gasID).ThermoMode == DataGenerators::NASAPolynomial) {
                    A1 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A1;
                    A2 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A2;
                    A3 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A3;
                    A4 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A4;
                    A5 = DataGenerators::GasPhaseThermoChemistryData(gasID).NASA_A5;

                    tempCp += (A1 + A2 * Tkel + A3 * pow_2_Tkel + A4 * pow_3_Tkel + A5 * pow_4_Tkel) * DataGenerators::RinKJperMolpK *
                            this->AuxilHeat.ConstitMolalFract(thisConstit);
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

    void FCDataStruct::FigureACAncillaries(Real64 &PacAncill)
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
        this->FCPM.PelancillariesAC =
                this->FCPM.ANC0 + this->FCPM.ANC1 * this->FCPM.NdotFuel;

        // sect 6.0
        this->AirSup.PairCompEl = CurveManager::CurveValue(this->AirSup.BlowerPowerCurveID, this->FCPM.NdotAir);
        // sect 7.0
        DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl =
            CurveManager::CurveValue(DataGenerators::FuelSupply(this->FuelSupNum).CompPowerCurveID, this->FCPM.NdotFuel);

        // sect. 8.0
        this->WaterSup.PwaterCompEl =
            CurveManager::CurveValue(this->WaterSup.PmpPowerCurveID, this->FCPM.NdotLiqwater);

        PacAncill = this->FCPM.PelancillariesAC + this->AirSup.PairCompEl +
                DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl + this->WaterSup.PwaterCompEl;
    }

    void FCDataStruct::FigurePowerConditioningLosses(Real64 const Pdemand, Real64 &PpcuLosses)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate inverter losses

        if (this->Inverter.EffMode == DataGenerators::InverterEffConstant) {
            PpcuLosses = Pdemand * (1 - this->Inverter.ConstEff) / this->Inverter.ConstEff;
        }

        if (this->Inverter.EffMode == DataGenerators::InverterEffQuadratic) {

            // first use Pdemand instead of Pel to get initial estimate
            Real64 lastPpcuLosses = Pdemand * (1.0 - CurveManager::CurveValue(this->Inverter.EffQuadraticCurveID, Pdemand)) /
                             CurveManager::CurveValue(this->Inverter.EffQuadraticCurveID, Pdemand);

            for (int iter = 1; iter <= 20; ++iter) { // seems like need to iterate (??) Need to investigate number and convergence success here

                Real64 Pel = Pdemand + lastPpcuLosses;

                lastPpcuLosses = (1.0 - CurveManager::CurveValue(this->Inverter.EffQuadraticCurveID, Pel)) * Pel;
            }

            PpcuLosses = lastPpcuLosses;
        }
    }

    void FCDataStruct::FigureTransientConstraints(Real64 &Pel,            // DC power control setting for power module
                                    bool &Constrained,      // true if transient constraints kick in
                                    Real64 &PelDiff         // if constrained then this is the difference, positive
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 PelInput = Pel;  // hold initial value of inout var

        Real64 CurrentFractionalDay = double(DataGlobals::DayOfSim) + (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) / DataGlobals::HoursInDay;

        // Check if in start up and if it still should be
        if (this->FCPM.DuringStartUp) {

            // calculate time for end of start up period
            Real64 EndingFractionalDay = this->FCPM.FractionalDayofLastStartUp + this->FCPM.StartUpTime / DataGlobals::HoursInDay;

            if (CurrentFractionalDay > EndingFractionalDay) {
                // start up period is now over
                this->FCPM.DuringStartUp = false;
            }
        }

        // Check if in shut down up and if it still should be
        if (this->FCPM.DuringShutDown) {

            // calculate time for end of shut down period
            Real64 EndingFractionalDay = this->FCPM.FractionalDayofLastShutDown + this->FCPM.ShutDownTime / DataGlobals::HoursInDay;

            if (CurrentFractionalDay > EndingFractionalDay) {
                // start up period is now over
                this->FCPM.DuringShutDown = false;
            }
        }
        // compare

        if (!(this->FCPM.DuringShutDown) && !(this->FCPM.DuringStartUp)) {
            // unit is neither starting or stopping and the only constraints would come from transient limits
            if (Pel > this->FCPM.PelLastTimeStep) { // powering up
                // working variable for max allowed by transient constraint
                Real64 MaxPel = this->FCPM.PelLastTimeStep + this->FCPM.UpTranLimit * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
                if (MaxPel < Pel) {
                    Pel = MaxPel;
                    Constrained = true;
                } else {
                    Constrained = false;
                }
            } else if (Pel < this->FCPM.PelLastTimeStep) { // powering down
               // working variable for min allowed by transient constraint
                Real64 MinPel = this->FCPM.PelLastTimeStep - this->FCPM.DownTranLimit * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
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

        if (this->FCPM.DuringStartUp) {
            // constant during start up modeling artifact
            Pel = this->FCPM.StartUpElectProd / this->FCPM.StartUpTime;
            Constrained = true;
        }

        if (this->FCPM.DuringShutDown) {

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

        FuelCell(Num).AuxilHeat.TauxMix = FuelCell(Num).FCPM.TprodGasLeavingFCPM;
        FuelCell(Num).AuxilHeat.NdotAuxMix = FuelCell(Num).FCPM.NdotProdGas;
        FuelCell(Num).AuxilHeat.ConstitMolalFract = FuelCell(Num).FCPM.ConstitMolalFract;
        FuelCell(Num).AuxilHeat.GasLibID = FuelCell(Num).FCPM.GasLibID;
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

        {
            auto const SELECT_CASE_var(FuelCell(Num).ExhaustHX.HXmodelMode);

            if (SELECT_CASE_var == DataGenerators::FixedEffectiveness) { // Method 1

                Real64 eHX = FuelCell(Num).ExhaustHX.HXEffect;

                Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                Real64 NdotWater = FuelCell(Num).ExhaustHX.WaterMassFlowRate / MWwater;
                Real64 TwaterIn = FuelCell(Num).ExhaustHX.WaterInletTemp;

                Real64 CpWaterMol;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);

                Real64 NdotGas = FuelCell(Num).AuxilHeat.NdotAuxMix;
                Real64 TprodGasIn = FuelCell(Num).AuxilHeat.TauxMix;
                Real64 CpProdGasMol;
                FuelCell(Num).FigureAuxilHeatGasHeatCap(TprodGasIn, CpProdGasMol); // Cp in (J/mol*K)
                // factor of 1000.0 for kmol -> mol
                Real64 NdotCp = min(NdotGas * CpProdGasMol * 1000.0, NdotWater * CpWaterMol * 1000.0);

                FuelCell(Num).ExhaustHX.qHX = eHX * NdotCp * (TprodGasIn - TwaterIn);

                FuelCell(Num).ExhaustHX.THXexh = TprodGasIn - FuelCell(Num).ExhaustHX.qHX / (NdotGas * CpProdGasMol * 1000.0);

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(FuelCell(Num).CWLoopNum).FluidName, TwaterIn, DataPlant::PlantLoop(FuelCell(Num).CWLoopNum).FluidIndex, RoutineName);

                if (FuelCell(Num).ExhaustHX.WaterMassFlowRate * Cp <= 0.0) {
                    FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn;
                } else {
                    FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn + FuelCell(Num).ExhaustHX.qHX / (FuelCell(Num).ExhaustHX.WaterMassFlowRate * Cp);
                }

            } else if (SELECT_CASE_var == DataGenerators::LMTDempiricalUAeff) { // method 2
                Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                Real64 NdotWater = FuelCell(Num).ExhaustHX.WaterMassFlowRate / MWwater;
                Real64 NdotGas = FuelCell(Num).AuxilHeat.NdotAuxMix;

                Real64 UAeff = FuelCell(Num).ExhaustHX.hxs0 + FuelCell(Num).ExhaustHX.hxs1 * NdotWater + FuelCell(Num).ExhaustHX.hxs2 * pow_2(NdotWater) +
                        FuelCell(Num).ExhaustHX.hxs3 * NdotGas + FuelCell(Num).ExhaustHX.hxs4 * pow_2(NdotGas);

                Real64 TauxMix = FuelCell(Num).AuxilHeat.TauxMix;
                Real64 TwaterIn = FuelCell(Num).ExhaustHX.WaterInletTemp;
                Real64 CpWaterMol;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                // factor of 1000.0 for kmol -> mol
                Real64 NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                Real64 CpProdGasMol;
                FuelCell(Num).FigureAuxilHeatGasHeatCap(TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                Real64 NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero
                    // now evaluate Eq. 44
                    FuelCell(Num).ExhaustHX.THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TauxMix +
                             ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TwaterIn;

                    FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - FuelCell(Num).ExhaustHX.THXexh); // Eq. 42

                } else {
                    FuelCell(Num).ExhaustHX.THXexh = TauxMix;
                    FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn;
                }
                // ENDIF

                if ((FuelCell(Num).ExhaustHX.THXexh - TwaterIn) != 0.0) { // trap divide by zero
                    FuelCell(Num).ExhaustHX.qHX = UAeff * ((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) - (FuelCell(Num).ExhaustHX.THXexh - TwaterIn)) / std::log((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) / (FuelCell(Num).ExhaustHX.THXexh - TwaterIn));
                } else {
                    FuelCell(Num).ExhaustHX.qHX = 0.0;
                }

            } else if (SELECT_CASE_var == DataGenerators::LMTDfundementalUAeff) { // method 3
                Real64 NdotGas = FuelCell(Num).AuxilHeat.NdotAuxMix;
                Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                Real64 NdotWater = FuelCell(Num).ExhaustHX.WaterMassFlowRate / MWwater;

                Real64 hgas = FuelCell(Num).ExhaustHX.h0gas * std::pow(NdotGas / FuelCell(Num).ExhaustHX.NdotGasRef, FuelCell(Num).ExhaustHX.nCoeff); // Eq. 48
                Real64 hwater = FuelCell(Num).ExhaustHX.h0Water *
                         std::pow(NdotWater / FuelCell(Num).ExhaustHX.NdotWaterRef, FuelCell(Num).ExhaustHX.mCoeff); // Eq. 48

                // now equation 47
                Real64 UAeff = 1.0 / (1.0 / (hgas * FuelCell(Num).ExhaustHX.AreaGas) + 1.0 / (hwater * FuelCell(Num).ExhaustHX.AreaWater) +
                        FuelCell(Num).ExhaustHX.Fadjust);

                Real64 TauxMix = FuelCell(Num).AuxilHeat.TauxMix;
                Real64 TwaterIn = FuelCell(Num).ExhaustHX.WaterInletTemp;
                Real64 CpWaterMol;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                Real64 NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                Real64 CpProdGasMol;
                FuelCell(Num).FigureAuxilHeatGasHeatCap(TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                Real64 NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero
                    // now evaluate Eq. 44
                    FuelCell(Num).ExhaustHX.THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TauxMix +
                             ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                 TwaterIn;

                    FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - FuelCell(Num).ExhaustHX.THXexh); // Eq. 42

                } else {
                    FuelCell(Num).ExhaustHX.THXexh = TauxMix;
                    FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn;
                }

                if ((FuelCell(Num).ExhaustHX.THXexh - TwaterIn) != 0.0) { // trap divide by zero
                    FuelCell(Num).ExhaustHX.qHX = UAeff * ((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) - (FuelCell(Num).ExhaustHX.THXexh - TwaterIn)) / std::log((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) / (FuelCell(Num).ExhaustHX.THXexh - TwaterIn));
                } else {
                    FuelCell(Num).ExhaustHX.qHX = 0.0;
                }

            } else if (SELECT_CASE_var == DataGenerators::Condensing) { // method 4
                if (FuelCell(Num).ExhaustHX.WaterMassFlowRate != 0.0) {

                    Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                    Real64 NdotWater = FuelCell(Num).ExhaustHX.WaterMassFlowRate / MWwater;
                    Real64 NdotGas = FuelCell(Num).AuxilHeat.NdotAuxMix;

                    Real64 UAeff = FuelCell(Num).ExhaustHX.hxs0 + FuelCell(Num).ExhaustHX.hxs1 * NdotWater +
                            FuelCell(Num).ExhaustHX.hxs2 * pow_2(NdotWater) + FuelCell(Num).ExhaustHX.hxs3 * NdotGas +
                            FuelCell(Num).ExhaustHX.hxs4 * pow_2(NdotGas);

                    Real64 TauxMix = FuelCell(Num).AuxilHeat.TauxMix;
                    Real64 TwaterIn = FuelCell(Num).ExhaustHX.WaterInletTemp;
                    Real64 CpWaterMol;
                    FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                    Real64 NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                    Real64 CpProdGasMol;
                    FuelCell(Num).FigureAuxilHeatGasHeatCap(TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                    Real64 NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                    // find water fraction in incoming gas stream
                    for (int i = 1; i <= isize(FuelCell(Num).AuxilHeat.GasLibID); ++i) {
                        if (FuelCell(Num).AuxilHeat.GasLibID(i) == 4) FuelCell(Num).ExhaustHX.WaterVaporFractExh = FuelCell(Num).AuxilHeat.ConstitMolalFract(i);
                    }
                    Real64 NdotWaterVapor = FuelCell(Num).ExhaustHX.WaterVaporFractExh * NdotGas;

                    Real64 TcondThresh = FuelCell(Num).ExhaustHX.CondensationThresholdTemp;
                    Real64 hxl1 = FuelCell(Num).ExhaustHX.l1Coeff;
                    Real64 hxl2 = FuelCell(Num).ExhaustHX.l2Coeff;

                    FuelCell(Num).ExhaustHX.CondensateRate = (TcondThresh - TwaterIn) * (hxl1 * (NdotWaterVapor / NdotGas) + hxl2 * pow_2(NdotWaterVapor / NdotGas));

                    if (FuelCell(Num).ExhaustHX.CondensateRate < 0.0) FuelCell(Num).ExhaustHX.CondensateRate = 0.0;

                    Real64 hfpwater = 4.4004e+07; // molal heat of vaporization of water J/kmol)

                    if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero

                        // now evaluate Eq. 44
                        FuelCell(Num).ExhaustHX.THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                                  (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                     TauxMix +
                                 ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                                  (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                     TwaterIn;

                        FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - FuelCell(Num).ExhaustHX.THXexh) + (FuelCell(Num).ExhaustHX.CondensateRate * hfpwater) / NdotCpWater;

                        if (FuelCell(Num).ExhaustHX.CondensateRate > 0) { // Eq. 44 is not correct. use its result as first guess for revised way...
                            // iterative solution because in condensing case THXexh is function of qSens and qLatent
                            for (int loop = 1; loop <= 5; ++loop) {

                                Real64 qSens;
                                Real64 qLatent;

                                if ((FuelCell(Num).ExhaustHX.THXexh - TwaterIn) != 0.0 &&
                                    ((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) / (FuelCell(Num).ExhaustHX.THXexh - TwaterIn) > 0.0001)) { // trap divide by zero and negative log
                                    qSens =
                                        UAeff * ((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) - (FuelCell(Num).ExhaustHX.THXexh - TwaterIn)) / std::log((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) / (FuelCell(Num).ExhaustHX.THXexh - TwaterIn));
                                } else {
                                    qSens = 0.0;
                                }
                                qLatent = FuelCell(Num).ExhaustHX.CondensateRate * hfpwater;
                                if (qSens > 0) {
                                    FuelCell(Num).ExhaustHX.THXexh =
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
                                    FuelCell(Num).ExhaustHX.THXexh = TauxMix;
                                }

                                FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - FuelCell(Num).ExhaustHX.THXexh) + (FuelCell(Num).ExhaustHX.CondensateRate * hfpwater) / NdotCpWater;
                            }
                        }

                    } else {
                        FuelCell(Num).ExhaustHX.THXexh = TauxMix;
                        FuelCell(Num).ExhaustHX.WaterOutletTemp = TwaterIn;
                    }

                    if ((FuelCell(Num).ExhaustHX.THXexh - TwaterIn) != 0.0 &&
                        ((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) / (FuelCell(Num).ExhaustHX.THXexh - TwaterIn) > 0.0001)) { // trap divide by zero and negative log

                        FuelCell(Num).ExhaustHX.qHX = UAeff * ((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) - (FuelCell(Num).ExhaustHX.THXexh - TwaterIn)) / std::log((TauxMix - FuelCell(Num).ExhaustHX.WaterOutletTemp) / (FuelCell(Num).ExhaustHX.THXexh - TwaterIn)) +
                                FuelCell(Num).ExhaustHX.CondensateRate * hfpwater;
                    } else {
                        FuelCell(Num).ExhaustHX.qHX = 0.0;
                    }
                } else { // no cooling water flow, model will blow up.
                    FuelCell(Num).ExhaustHX.qHX = 0.0;
                    FuelCell(Num).ExhaustHX.THXexh = FuelCell(Num).AuxilHeat.TauxMix;
                    FuelCell(Num).ExhaustHX.WaterOutletTemp = FuelCell(Num).ExhaustHX.WaterInletTemp;
                    FuelCell(Num).ExhaustHX.CondensateRate = 0.0;
                    FuelCell(Num).ExhaustHX.WaterVaporFractExh = -9999.0; // not defined
                }
            } else {
                assert(false); // Variables not set are used below
            }
        }

        // update results in data structure.
        FuelCell(Num).ExhaustHX.WaterOutletEnthalpy = DataLoopNode::Node(FuelCell(Num).ExhaustHX.WaterInNode).Enthalpy + FuelCell(Num).ExhaustHX.qHX;
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

        if (GetFuelCellInput) {
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

        bool errFlag;

        // Do the one time initializations
        if (FuelCell(FCnum).InitGenerator) {
            FuelCell(FCnum).InitGenerator = false;
        } // end one time setups and inits

        if (FuelCell(FCnum).MyPlantScanFlag_Init && allocated(DataPlant::PlantLoop)) {
            errFlag = false;

            PlantUtilities::ScanPlantLoopsForObject(FuelCell(FCnum).NameExhaustHX,
                                    DataPlant::TypeOf_Generator_FCExhaust,
                                                    FuelCell(FCnum).CWLoopNum,
                                                    FuelCell(FCnum).CWLoopSideNum,
                                                    FuelCell(FCnum).CWBranchNum,
                                                    FuelCell(FCnum).CWCompNum,
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
            FuelCell(FCnum).MyPlantScanFlag_Init = false;
        }

        // Do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && FuelCell(FCnum).MyEnvrnFlag_Init && !FuelCell(FCnum).MyPlantScanFlag_Init) {

            DataGenerators::FuelSupply(FuelCell(FCnum).FuelSupNum).PfuelCompEl = 0.0;
            DataGenerators::FuelSupply(FuelCell(FCnum).FuelSupNum).TfuelIntoFCPM = 0.0;
            DataGenerators::FuelSupply(FuelCell(FCnum).FuelSupNum).TfuelIntoCompress = 0.0;
            DataGenerators::FuelSupply(FuelCell(FCnum).FuelSupNum).QskinLoss = 0.0;

            FuelCell(FCnum).AirSup.TairIntoFCPM = 0.0;
            FuelCell(FCnum).AirSup.PairCompEl = 0.0;
            FuelCell(FCnum).AirSup.TairIntoBlower = 0.0;
            FuelCell(FCnum).AirSup.QskinLoss = 0.0;
            FuelCell(FCnum).AirSup.QintakeRecovery = 0.0;
            FuelCell(FCnum).FCPM.NumCycles = 0;
            FuelCell(FCnum).FCPM.Pel = 0.0;
            FuelCell(FCnum).FCPM.PelLastTimeStep = 0.0;
            FuelCell(FCnum).FCPM.Eel = 0.0;
            FuelCell(FCnum).FCPM.PelancillariesAC = 0.0;
            FuelCell(FCnum).FCPM.NdotFuel = 0.0;
            FuelCell(FCnum).FCPM.TotFuelInEnthalphy = 0.0;
            FuelCell(FCnum).FCPM.NdotProdGas = 0.0;
            FuelCell(FCnum).FCPM.TprodGasLeavingFCPM = 0.0;
            FuelCell(FCnum).FCPM.TotProdGasEnthalphy = 0.0;
            FuelCell(FCnum).FCPM.NdotAir = 0.0;
            FuelCell(FCnum).FCPM.TotAirInEnthalphy = 0.0;
            FuelCell(FCnum).FCPM.NdotLiqwater = 0.0;
            FuelCell(FCnum).FCPM.TwaterInlet = 0.0;
            FuelCell(FCnum).FCPM.WaterInEnthalpy = 0.0;
            FuelCell(FCnum).FCPM.TprodGasLeavingFCPM = 200.0;
            FuelCell(FCnum).FCPM.FractionalDayofLastStartUp = 0.0;
            FuelCell(FCnum).FCPM.FractionalDayofLastShutDown = 0.0;
            FuelCell(FCnum).FCPM.HasBeenOn = true;
            FuelCell(FCnum).FCPM.DuringShutDown = false;
            FuelCell(FCnum).FCPM.DuringStartUp = false;
            FuelCell(FCnum).WaterSup.TwaterIntoCompress = 0.0;
            FuelCell(FCnum).WaterSup.TwaterIntoFCPM = 0.0;
            FuelCell(FCnum).WaterSup.PwaterCompEl = 0.0;
            FuelCell(FCnum).WaterSup.QskinLoss = 0.0;
            FuelCell(FCnum).AuxilHeat.TauxMix = 0.0;
            FuelCell(FCnum).AuxilHeat.NdotAuxMix = 0.0;
            FuelCell(FCnum).AuxilHeat.QskinLoss = 0.0;
            FuelCell(FCnum).AuxilHeat.QairIntake = 0.0;
            FuelCell(FCnum).ExhaustHX.NdotHXleaving = 0.0;
            FuelCell(FCnum).ExhaustHX.WaterOutletTemp = 0.0;
            FuelCell(FCnum).ExhaustHX.WaterOutletEnthalpy = 0.0;
            FuelCell(FCnum).ElecStorage.LastTimeStepStateOfCharge = FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            FuelCell(FCnum).ElecStorage.ThisTimeStepStateOfCharge = FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            FuelCell(FCnum).ElecStorage.PelNeedFromStorage = 0.0;
            FuelCell(FCnum).ElecStorage.IdesiredDischargeCurrent = 0.0;
            FuelCell(FCnum).ElecStorage.PelFromStorage = 0.0;
            FuelCell(FCnum).ElecStorage.IfromStorage = 0.0;
            FuelCell(FCnum).ElecStorage.PelIntoStorage = 0.0;
            FuelCell(FCnum).ElecStorage.QairIntake = 0.0;

            FuelCell(FCnum).Inverter.PCUlosses = 0.0;
            FuelCell(FCnum).Inverter.QairIntake = 0.0;

            Real64 rho = FluidProperties::GetDensityGlycol(
                DataPlant::PlantLoop(FuelCell(FCnum).CWLoopNum).FluidName, DataGenerators::InitHRTemp, DataPlant::PlantLoop(FuelCell(FCnum).CWLoopNum).FluidIndex, RoutineName);

            FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign = FuelCell(FCnum).ExhaustHX.WaterVolumeFlowMax * rho;
            FuelCell(FCnum).ExhaustHX.WaterMassFlowRate = FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign;
            DataLoopNode::Node(FuelCell(FCnum).ExhaustHX.WaterInNode).Temp = DataGenerators::InitHRTemp;
            DataLoopNode::Node(FuelCell(FCnum).ExhaustHX.WaterOutNode).Temp = DataGenerators::InitHRTemp;

            PlantUtilities::InitComponentNodes(0.0,
                               FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign,
                                               FuelCell(FCnum).ExhaustHX.WaterInNode,
                                               FuelCell(FCnum).ExhaustHX.WaterOutNode,
                               FuelCell(FCnum).CWLoopNum,
                               FuelCell(FCnum).CWLoopSideNum,
                               FuelCell(FCnum).CWBranchNum,
                               FuelCell(FCnum).CWCompNum);

            FuelCell(FCnum).MyEnvrnFlag_Init = false;
            FuelCell(FCnum).MyWarmupFlag_Init = true;
        } // end environmental inits

        if (!DataGlobals::BeginEnvrnFlag) {
            FuelCell(FCnum).MyEnvrnFlag_Init = true;
        }

        if (FuelCell(FCnum).MyWarmupFlag_Init && (!DataGlobals::WarmupFlag)) {
            // need to reset initial state of charge at beginning of environment but after warm up is complete
            FuelCell(FCnum).ElecStorage.LastTimeStepStateOfCharge = FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            FuelCell(FCnum).ElecStorage.ThisTimeStepStateOfCharge = FuelCell(FCnum).ElecStorage.StartingEnergyStored;
            FuelCell(FCnum).MyWarmupFlag_Init = false;
        }

        // using and elapsed time method rather than FirstHVACIteration here
        Real64 TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
        if (FuelCell(FCnum).TimeElapsed != TimeElapsed) {

            FuelCell(FCnum).ElecStorage.LastTimeStepStateOfCharge = FuelCell(FCnum).ElecStorage.ThisTimeStepStateOfCharge;
            FuelCell(FCnum).FCPM.PelLastTimeStep = FuelCell(FCnum).FCPM.Pel;

            // intialize flow rate in water loop, this is "requesting" flow
            Real64 mdot = FuelCell(FCnum).ExhaustHX.WaterMassFlowRateDesign;

            PlantUtilities::SetComponentFlowRate(mdot,
                                                 FuelCell(FCnum).ExhaustHX.WaterInNode,
                                                 FuelCell(FCnum).ExhaustHX.WaterOutNode,
                                 FuelCell(FCnum).CWLoopNum,
                                 FuelCell(FCnum).CWLoopSideNum,
                                 FuelCell(FCnum).CWBranchNum,
                                 FuelCell(FCnum).CWCompNum);

            FuelCell(FCnum).ExhaustHX.WaterMassFlowRate = mdot;
            FuelCell(FCnum).ExhaustHX.WaterInletTemp = DataLoopNode::Node(FuelCell(FCnum).ExhaustHX.WaterInNode).Temp;
            FuelCell(FCnum).TimeElapsed = TimeElapsed;
        } else {

            PlantUtilities::SetComponentFlowRate(FuelCell(FCnum).ExhaustHX.WaterMassFlowRate,
                                 FuelCell(FCnum).ExhaustHX.WaterInNode,
                                 FuelCell(FCnum).ExhaustHX.WaterOutNode,
                                 FuelCell(FCnum).CWLoopNum,
                                 FuelCell(FCnum).CWLoopSideNum,
                                 FuelCell(FCnum).CWBranchNum,
                                 FuelCell(FCnum).CWCompNum);

            FuelCell(FCnum).ExhaustHX.WaterInletTemp = DataLoopNode::Node(FuelCell(FCnum).ExhaustHX.WaterInNode).Temp;
        }
    }

    void getFuelCellGeneratorHeatRecoveryInfo(std::string const &GeneratorName, // user specified name of Generator
                                              std::string &heatRecoveryCompName)
    {

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
        // Couple equipment skin losses to the Zone Heat Balance
        // calculate skin losses from different subsystems and set the value

        // METHODOLOGY EMPLOYED:
        // This routine adds up the various skin losses and then
        //  sets the values in the ZoneIntGain structure

        static bool MyEnvrnFlag(true);

        if (NumFuelCellGenerators == 0) return;

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag) {
            for (auto &e : DataGenerators::FuelSupply)
                e.QskinLoss = 0.0;
            MyEnvrnFlag = false;
            for (int i = FuelCell.l(), e = FuelCell.u(); i <= e; ++i) {
                auto &cell(FuelCell(i));
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
        for (int FCnum = 1; FCnum <= NumFuelCellGenerators; ++FCnum) {
            Real64 TotalZoneHeatGain = FuelCell(FCnum).AirSup.QskinLoss + DataGenerators::FuelSupply(FuelCell(FCnum).FuelSupNum).QskinLoss +
                                FuelCell(FCnum).WaterSup.QskinLoss + FuelCell(FCnum).AuxilHeat.QskinLoss +
                                FuelCell(FCnum).FCPM.QdotSkin; // intake Blower losses to zone | fuel compressor losses to zone | water pump losses to
                                                               // zone | auxil burner losses to zone | power module (stack and reformer) losses to
                                                               // zone

            // now account for other subsystems that may or may not have air intake recovery
            {
                auto const SELECT_CASE_var(FuelCell(FCnum).AirSup.IntakeRecoveryMode);

                if (SELECT_CASE_var == DataGenerators::NoRecoveryOnAirIntake) { // then the heat has to go into zone
                    TotalZoneHeatGain +=
                        FuelCell(FCnum).AuxilHeat.QairIntake + FuelCell(FCnum).ElecStorage.QairIntake + FuelCell(FCnum).Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverAuxiliaryBurner) {
                    TotalZoneHeatGain += FuelCell(FCnum).ElecStorage.QairIntake + FuelCell(FCnum).Inverter.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverInverterBatt) {
                    TotalZoneHeatGain += FuelCell(FCnum).AuxilHeat.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverInverter) {
                    TotalZoneHeatGain += FuelCell(FCnum).AuxilHeat.QairIntake + FuelCell(FCnum).ElecStorage.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverBattery) {
                    TotalZoneHeatGain += FuelCell(FCnum).AuxilHeat.QairIntake + FuelCell(FCnum).Inverter.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverBurnInvertBatt) {
                    // do nothing
                }
            }

            FuelCell(FCnum).QconvZone = TotalZoneHeatGain * (1 - FuelCell(FCnum).FCPM.RadiativeFract);
            FuelCell(FCnum).Report.SkinLossConvect = FuelCell(FCnum).QconvZone;
            FuelCell(FCnum).QradZone = TotalZoneHeatGain * FuelCell(FCnum).FCPM.RadiativeFract;
            FuelCell(FCnum).Report.SkinLossRadiat = FuelCell(FCnum).QradZone;

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

        // now update water outlet node Changing to Kg/s!

        PlantUtilities::SafeCopyPlantNode(FuelCell(Num).ExhaustHX.WaterInNode, FuelCell(Num).ExhaustHX.WaterOutNode);

        DataLoopNode::Node(FuelCell(Num).ExhaustHX.WaterOutNode).Temp = FuelCell(Num).ExhaustHX.WaterOutletTemp;
        DataLoopNode::Node(FuelCell(Num).ExhaustHX.WaterOutNode).Enthalpy = FuelCell(Num).ExhaustHX.WaterOutletEnthalpy;
    }

    void UpdateFuelCellGeneratorRecords(bool const EP_UNUSED(RunFlag), // TRUE if Generator operating
                                        int const Num                  // Generator number
    )
    {

        FuelCell(Num).Report.ACPowerGen = FuelCell(Num).ACPowerGen;                            // electrical power produced [W]
        FuelCell(Num).Report.ACEnergyGen = FuelCell(Num).ACPowerGen * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // energy produced (J)
        FuelCell(Num).Report.QdotExhaust = 0.0;                                                // reporting: exhaust gas heat recovered (W)
        FuelCell(Num).Report.TotalHeatEnergyRec = 0.0;                                         // reporting: total heat recovered (J)
        FuelCell(Num).Report.ExhaustEnergyRec = 0.0;                                           // reporting: exhaust gas heat recovered (J)

        FuelCell(Num).Report.HeatRecInletTemp = 0.0;  // reporting: Heat Recovery Loop Inlet Temperature (C)
        FuelCell(Num).Report.HeatRecOutletTemp = 0.0; // reporting: Heat Recovery Loop Outlet Temperature (C)
        FuelCell(Num).Report.HeatRecMdot = 0.0;       // reporting: Heat Recovery Loop Mass flow rate (kg/s)

        FuelCell(Num).Report.ElectEfficiency = 0.0;
        FuelCell(Num).Report.ThermalEfficiency = 0.0;
        FuelCell(Num).Report.OverallEfficiency = 0.0;
        FuelCell(Num).Report.ExergyEfficiency = 0.0;

        FuelCell(Num).Report.TairInlet = FuelCell(Num).AirSup.TairIntoBlower;                          // State point 1
        FuelCell(Num).Report.TairIntoFCPM = FuelCell(Num).AirSup.TairIntoFCPM;                         // State point 4
        FuelCell(Num).Report.NdotAir = FuelCell(Num).FCPM.NdotAir;                                     // air flow in kmol/sec
        FuelCell(Num).Report.TotAirInEnthalphy = FuelCell(Num).FCPM.TotAirInEnthalphy;                 // State point 4
        FuelCell(Num).Report.BlowerPower = FuelCell(Num).AirSup.PairCompEl;                            // electrical power used by air supply blower
        FuelCell(Num).Report.BlowerEnergy = FuelCell(Num).AirSup.PairCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // electrical energy
        FuelCell(Num).Report.BlowerSkinLoss = FuelCell(Num).AirSup.QskinLoss;                          // heat rate of losses by blower

        FuelCell(Num).Report.TfuelInlet = DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).TfuelIntoCompress; // State point 2
        FuelCell(Num).Report.TfuelIntoFCPM = DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).TfuelIntoFCPM;  // TEmperature state point 5 [C]
        FuelCell(Num).Report.NdotFuel = FuelCell(Num).FCPM.NdotFuel;                              // fuel flow in kmol/sec
        FuelCell(Num).Report.TotFuelInEnthalpy = FuelCell(Num).FCPM.TotFuelInEnthalphy;           // enthalpy at state point 5 [W]
        FuelCell(Num).Report.FuelCompressPower = DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).PfuelCompEl;
        // electrical power used by fuel supply compressor [W]
        FuelCell(Num).Report.FuelCompressEnergy = DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).PfuelCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // elect energy
        FuelCell(Num).Report.FuelCompressSkinLoss = DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).QskinLoss;
        // heat rate of losses.by fuel supply compressor [W]
        FuelCell(Num).Report.FuelEnergyLHV = FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).LHV * 1000000.0 * DataHVACGlobals::TimeStepSys *
                                             DataGlobals::SecInHour; // reporting: Fuel Energy used (J)
        FuelCell(Num).Report.FuelEnergyUseRateLHV =
            FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).LHV * 1000000.0; // reporting: Fuel Energy used (W)
        FuelCell(Num).Report.FuelEnergyHHV = FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).HHV *
                                             DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).KmolPerSecToKgPerSec * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        FuelCell(Num).Report.FuelEnergyUseRateHHV =
            FuelCell(Num).FCPM.NdotFuel * DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).HHV * DataGenerators::FuelSupply(FuelCell(Num).FuelSupNum).KmolPerSecToKgPerSec;

        FuelCell(Num).Report.FuelRateMdot = 0.0; // (Kg/s)

        FuelCell(Num).Report.TwaterInlet = FuelCell(Num).WaterSup.TwaterIntoCompress;
        FuelCell(Num).Report.TwaterIntoFCPM = FuelCell(Num).WaterSup.TwaterIntoFCPM;
        FuelCell(Num).Report.NdotWater = FuelCell(Num).FCPM.NdotLiqwater; // water flow in kmol/sec (reformer water)
        FuelCell(Num).Report.WaterPumpPower = FuelCell(Num).WaterSup.PwaterCompEl;
        FuelCell(Num).Report.WaterPumpEnergy = FuelCell(Num).WaterSup.PwaterCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // electrical energy
        FuelCell(Num).Report.WaterIntoFCPMEnthalpy = FuelCell(Num).FCPM.WaterInEnthalpy;

        FuelCell(Num).Report.TprodGas = FuelCell(Num).FCPM.TprodGasLeavingFCPM;      // temperature at State point 7
        FuelCell(Num).Report.EnthalProdGas = FuelCell(Num).FCPM.TotProdGasEnthalphy; // enthalpy at State point 7
        FuelCell(Num).Report.NdotProdGas = FuelCell(Num).FCPM.NdotProdGas;           // flow rate at point 7 [kmol/sec]
        FuelCell(Num).Report.NdotProdAr = FuelCell(Num).FCPM.ConstitMolalFract(5) * FuelCell(Num).FCPM.NdotProdGas;
        FuelCell(Num).Report.NdotProdCO2 = FuelCell(Num).FCPM.ConstitMolalFract(1) * FuelCell(Num).FCPM.NdotProdGas;
        FuelCell(Num).Report.NdotProdH2O = FuelCell(Num).FCPM.ConstitMolalFract(4) * FuelCell(Num).FCPM.NdotProdGas;
        FuelCell(Num).Report.NdotProdN2 = FuelCell(Num).FCPM.ConstitMolalFract(2) * FuelCell(Num).FCPM.NdotProdGas;
        FuelCell(Num).Report.NdotProdO2 = FuelCell(Num).FCPM.ConstitMolalFract(3) * FuelCell(Num).FCPM.NdotProdGas;

        FuelCell(Num).Report.qHX = FuelCell(Num).ExhaustHX.qHX;
        FuelCell(Num).Report.HXenergy = FuelCell(Num).ExhaustHX.qHX * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        FuelCell(Num).Report.THXexh = FuelCell(Num).ExhaustHX.THXexh;
        FuelCell(Num).Report.WaterVaporFractExh = FuelCell(Num).ExhaustHX.WaterVaporFractExh;
        FuelCell(Num).Report.CondensateRate = FuelCell(Num).ExhaustHX.CondensateRate;

        FuelCell(Num).Report.SeqSubstIterations = FuelCell(Num).FCPM.SeqSubstitIter;     // number of iterations in FuelCell loop
        FuelCell(Num).Report.RegulaFalsiIterations = FuelCell(Num).FCPM.RegulaFalsiIter; // number of iterations in Tproduct gas solving

        FuelCell(Num).Report.ACancillariesPower = FuelCell(Num).FCPM.PelancillariesAC;
        FuelCell(Num).Report.ACancillariesEnergy = FuelCell(Num).FCPM.PelancillariesAC * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        FuelCell(Num).Report.PCUlosses = FuelCell(Num).Inverter.PCUlosses; // inverter losses
        FuelCell(Num).Report.DCPowerGen = FuelCell(Num).FCPM.Pel;          // DC power out of FCPM.
        FuelCell(Num).Report.DCPowerEff = FuelCell(Num).FCPM.Eel;          // FCPM efficienty Eel.
        FuelCell(Num).Report.ElectEnergyinStorage = FuelCell(Num).ElecStorage.ThisTimeStepStateOfCharge;
        FuelCell(Num).Report.StoredPower = FuelCell(Num).ElecStorage.PelIntoStorage;
        FuelCell(Num).Report.StoredEnergy = FuelCell(Num).ElecStorage.PelIntoStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        FuelCell(Num).Report.DrawnPower = FuelCell(Num).ElecStorage.PelFromStorage;
        FuelCell(Num).Report.DrawnEnergy = FuelCell(Num).ElecStorage.PelFromStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        FuelCell(Num).Report.SkinLossPower = FuelCell(Num).QconvZone + FuelCell(Num).QradZone;
        FuelCell(Num).Report.SkinLossEnergy = (FuelCell(Num).QconvZone + FuelCell(Num).QradZone) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        FuelCell(Num).Report.SkinLossConvect = FuelCell(Num).QconvZone;
        FuelCell(Num).Report.SkinLossRadiat = FuelCell(Num).QradZone;
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

        GeneratorPower = FuelCell(GeneratorIndex).Report.ACPowerGen;
        GeneratorEnergy = FuelCell(GeneratorIndex).Report.ACEnergyGen;
        ThermalPower = FuelCell(GeneratorIndex).Report.qHX;
        ThermalEnergy = FuelCell(GeneratorIndex).Report.HXenergy;
    }

} // namespace FuelCellElectricGenerator

} // namespace EnergyPlus
