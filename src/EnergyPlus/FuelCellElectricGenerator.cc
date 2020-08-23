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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGenerators.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/FuelCellElectricGenerator.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneratorFuelSupply.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
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
    bool getFuelCellInputFlag(true);
    Array1D_bool CheckEquipName;
    Array1D<FCDataStruct> FuelCell; // dimension to number of machines
    bool MyEnvrnFlag(true);

    void clear_state()
    {
        NumFuelCellGenerators = 0;
        getFuelCellInputFlag = true;
        CheckEquipName.deallocate();
        FuelCell.deallocate();
        MyEnvrnFlag = true;
    }

    PlantComponent *FCDataStruct::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data
        if (getFuelCellInputFlag) {
            getFuelCellInput(state, state.files);
            getFuelCellInputFlag = false;
        }

        // Now look for this object
        for (auto &thisFC : FuelCell) {
            if (thisFC.Name == objectName) {
                return &thisFC;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalFuelCellGenFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    PlantComponent *FCDataStruct::factory_exhaust(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data
        if (getFuelCellInputFlag) {
            getFuelCellInput(state, state.files);
            getFuelCellInputFlag = false;
        }

        // Now look for this object
        for (auto &thisFC : FuelCell) {
            if (UtilityRoutines::MakeUPPERCase(thisFC.NameExhaustHX) == UtilityRoutines::MakeUPPERCase(objectName)) {
                return &thisFC;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalFuelCellGenFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void FCDataStruct::SimFuelCellGenerator(EnergyPlusData &state,
                                            BranchInputManagerData &dataBranchInputManager,
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

        this->initialize(dataBranchInputManager);
        this->CalcFuelCellGeneratorModel(state, RunFlag, MyLoad, FirstHVACIteration);
        this->CalcUpdateHeatRecovery(FirstHVACIteration);
        this->UpdateFuelCellGeneratorRecords();
    }

    void getFuelCellInput(EnergyPlusData &state, IOFiles &ioFiles)
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          GeneratorNum,
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
                if (UtilityRoutines::SameString(AlphArray(2), "NORMALIZED"))
                    FuelCell(thisFuelCell).FCPM.EffMode = DataGenerators::NormalizedCurveMode;
                if (FuelCell(thisFuelCell).FCPM.EffMode == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).FCPM.EffCurveID = CurveManager::GetCurveIndex(state, AlphArray(3));
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
                if (UtilityRoutines::SameString(AlphArray(4), "ConstantRate"))
                    FuelCell(thisFuelCell).FCPM.SkinLossMode = DataGenerators::ConstantRateSkinLoss;
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

                FuelCell(thisFuelCell).FCPM.SkinLossCurveID = CurveManager::GetCurveIndex(state, AlphArray(6));
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
                FuelCell(thisFuelCell).FCPM.DilutionInletNode = NodeInputManager::GetOnlySingleNode(AlphArray(7),
                                                                                                    ErrorsFound,
                                                                                                    DataIPShortCuts::cCurrentModuleObject,
                                                                                                    AlphArray(1),
                                                                                                    DataLoopNode::NodeType_Air,
                                                                                                    DataLoopNode::NodeConnectionType_Inlet,
                                                                                                    1,
                                                                                                    DataLoopNode::ObjectIsNotParent);
                FuelCell(thisFuelCell).FCPM.DilutionExhaustNodeName = AlphArray(8);
                FuelCell(thisFuelCell).FCPM.DilutionExhaustNode = NodeInputManager::GetOnlySingleNode(AlphArray(8),
                                                                                                      ErrorsFound,
                                                                                                      DataIPShortCuts::cCurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeType_Air,
                                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                                      1,
                                                                                                      DataLoopNode::ObjectIsNotParent);

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

        GeneratorFuelSupply::GetGeneratorFuelSupplyInput(state, ioFiles);

        for (int FuelSupNum = 1; FuelSupNum <= DataGenerators::NumGeneratorFuelSups; ++FuelSupNum) {
            GeneratorFuelSupply::SetupFuelConstituentData(ioFiles, FuelSupNum, ErrorsFound);
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          FCAirSupNum,
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameFCAirSup);

            if (thisFuelCell > 0) {

                FuelCell(thisFuelCell).AirSup.Name = AlphArray(1);
                FuelCell(thisFuelCell).AirSup.NodeName = AlphArray(2);

                // check the node connections
                FuelCell(thisFuelCell).AirSup.SupNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               AlphArray(1),
                                                                                               DataLoopNode::NodeType_Air,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);

                FuelCell(thisFuelCell).AirSup.BlowerPowerCurveID = CurveManager::GetCurveIndex(state, AlphArray(3));
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

                FuelCell(thisFuelCell).AirSup.AirFuncPelCurveID = CurveManager::GetCurveIndex(state, AlphArray(5));
                if ((FuelCell(thisFuelCell).AirSup.AirFuncPelCurveID == 0) &&
                    (FuelCell(thisFuelCell).AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofPel)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + AlphArray(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowSevereError("Curve name was not found");
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).AirSup.AirTempCoeff = NumArray(3);

                FuelCell(thisFuelCell).AirSup.AirFuncNdotCurveID = CurveManager::GetCurveIndex(state, AlphArray(6));
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
            // find molar fraction of oxygen in air supply
            int thisConstituent =
                UtilityRoutines::FindItem("Oxygen", FuelCell(GeneratorNum).AirSup.ConstitName, FuelCell(GeneratorNum).AirSup.NumConstituents);
            if (thisConstituent > 0) FuelCell(GeneratorNum).AirSup.O2fraction = FuelCell(GeneratorNum).AirSup.ConstitMolalFract(thisConstituent);

            // Loop over air constituents and do one-time setup
            for (int i = 1; i <= FuelCell(GeneratorNum).AirSup.NumConstituents; ++i) {

                std::string thisName = FuelCell(GeneratorNum).AirSup.ConstitName(i);

                int thisGasID = UtilityRoutines::FindItem(
                    thisName, DataGenerators::GasPhaseThermoChemistryData, &DataGenerators::GasPropertyDataStruct::ConstituentName);

                FuelCell(GeneratorNum).AirSup.GasLibID(i) = thisGasID;
            }

            // set up gas constituents for product gases
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          FCWaterSupNum,
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameFCWaterSup);

            if (thisFuelCell > 0) {
                //  this is only the first instance of a FuelCell generator using this type of Water supply module
                FuelCell(thisFuelCell).WaterSup.Name = AlphArray(1);
                FuelCell(thisFuelCell).WaterSup.WaterSupRateCurveID = CurveManager::GetCurveIndex(state, AlphArray(2));
                if (FuelCell(thisFuelCell).WaterSup.WaterSupRateCurveID == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Curve name was not found ");
                    ErrorsFound = true;
                }
                FuelCell(thisFuelCell).WaterSup.PmpPowerCurveID = CurveManager::GetCurveIndex(state, AlphArray(3));
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
                if ((FuelCell(thisFuelCell).WaterSup.SchedNum == 0) &&
                    (FuelCell(thisFuelCell).WaterSup.WaterTempMode == DataGenerators::WaterInReformSchedule)) {
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          FCAuxHeatNum,
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
                if ((FuelCell(thisFuelCell).AuxilHeat.ZoneID == 0) &&
                    (FuelCell(thisFuelCell).AuxilHeat.SkinLossDestination == DataGenerators::SurroundingZone)) {
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          FCHXNum,
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameExhaustHX);

            if (thisFuelCell > 0) {
                FuelCell(thisFuelCell).TypeOf = DataPlant::TypeOf_Generator_FCExhaust;
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
                BranchNodeConnections::TestCompSet(
                    DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Heat Recovery Nodes");

                FuelCell(thisFuelCell).ExhaustHX.ExhaustOutNodeName = AlphArray(4);
                FuelCell(thisFuelCell).ExhaustHX.ExhaustOutNode = NodeInputManager::GetOnlySingleNode(AlphArray(4),
                                                                                                      ErrorsFound,
                                                                                                      DataIPShortCuts::cCurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeType_Air,
                                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                                      2,
                                                                                                      DataLoopNode::ObjectIsNotParent);

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
                PlantUtilities::RegisterPlantCompDesignFlow(FuelCell(thisFuelCell).ExhaustHX.WaterInNode,
                                                            FuelCell(thisFuelCell).ExhaustHX.WaterVolumeFlowMax);
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          StorageNum,
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
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          FCPCUNum,
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

            int thisFuelCell = UtilityRoutines::FindItemInList(AlphArray(1), FuelCell, &FCDataStruct::NameInverter);

            if (thisFuelCell > 0) {
                FuelCell(thisFuelCell).Inverter.Name = AlphArray(1);

                if (UtilityRoutines::SameString(AlphArray(2), "QUADRATIC"))
                    FuelCell(thisFuelCell).Inverter.EffMode = DataGenerators::InverterEffQuadratic;
                if (UtilityRoutines::SameString(AlphArray(2), "Constant"))
                    FuelCell(thisFuelCell).Inverter.EffMode = DataGenerators::InverterEffConstant;
                if (FuelCell(thisFuelCell).Inverter.EffMode == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                FuelCell(thisFuelCell).Inverter.ConstEff = NumArray(1);

                FuelCell(thisFuelCell).Inverter.EffQuadraticCurveID = CurveManager::GetCurveIndex(state, AlphArray(3));
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
                    FuelCell(thisFuelCell).TypeOf = DataPlant::TypeOf_Generator_FCStackCooler;
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
                    BranchNodeConnections::TestCompSet(
                        DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Heat Recovery Nodes");

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

        for (int genNum = 1; genNum <= NumFuelCellGenerators; ++genNum) {
            auto &thisGen = FuelCell(genNum);
            thisGen.setupOutputVars();
        }
    }

    void FCDataStruct::setupOutputVars()
    {
        SetupOutputVariable("Generator Produced Electric Power", OutputProcessor::Unit::W, this->Report.ACPowerGen, "System", "Average", this->Name);

        SetupOutputVariable("Generator Produced Electric Energy",
                            OutputProcessor::Unit::J,
                            this->Report.ACEnergyGen,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ElectricityProduced",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable("Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->Report.qHX, "System", "Average", this->Name);

        SetupOutputVariable("Generator Produced Thermal Energy",
                            OutputProcessor::Unit::J,
                            this->Report.HXenergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable("Generator Fuel HHV Basis Energy",
                            OutputProcessor::Unit::J,
                            this->Report.FuelEnergyHHV,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "Gas",
                            "COGENERATION",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Generator Fuel HHV Basis Rate", OutputProcessor::Unit::W, this->Report.FuelEnergyUseRateHHV, "System", "Average", this->Name);

        SetupOutputVariable(
            "Generator Zone Sensible Heat Transfer Rate", OutputProcessor::Unit::W, this->Report.SkinLossPower, "System", "Average", this->Name);

        SetupOutputVariable(
            "Generator Zone Sensible Heat Transfer Energy", OutputProcessor::Unit::J, this->Report.SkinLossEnergy, "System", "Sum", this->Name);

        SetupOutputVariable(
            "Generator Zone Convection Heat Transfer Rate", OutputProcessor::Unit::W, this->Report.SkinLossConvect, "System", "Average", this->Name);

        SetupOutputVariable(
            "Generator Zone Radiation Heat Transfer Rate", OutputProcessor::Unit::W, this->Report.SkinLossRadiat, "System", "Average", this->Name);

        if (this->FCPM.ZoneID > 0) {
            SetupZoneInternalGain(this->FCPM.ZoneID,
                                  "Generator:FuelCell",
                                  this->Name,
                                  DataHeatBalance::IntGainTypeOf_GeneratorFuelCell,
                                  &this->Report.SkinLossConvect,
                                  nullptr,
                                  &this->Report.SkinLossRadiat);
        }

        if (DataGlobals::DisplayAdvancedReportVariables) { // show extra data originally needed for detailed comparative testing
            SetupOutputVariable("Generator Air Inlet Temperature", OutputProcessor::Unit::C, this->Report.TairInlet, "System", "Average", this->Name);

            SetupOutputVariable("Generator Power Module Entering Air Temperature",
                                OutputProcessor::Unit::C,
                                this->Report.TairIntoFCPM,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator Air Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->Report.NdotAir, "System", "Average", this->Name);

            SetupOutputVariable("Generator Power Module Entering Air Enthalpy",
                                OutputProcessor::Unit::W,
                                this->Report.TotAirInEnthalphy,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator Blower Electric Power", OutputProcessor::Unit::W, this->Report.BlowerPower, "System", "Average", this->Name);

            SetupOutputVariable("Generator Blower Electric Energy", OutputProcessor::Unit::J, this->Report.BlowerEnergy, "System", "Sum", this->Name);

            SetupOutputVariable(
                "Generator Blower Skin Heat Loss Rate", OutputProcessor::Unit::W, this->Report.BlowerSkinLoss, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Fuel Inlet Temperature", OutputProcessor::Unit::C, this->Report.TfuelInlet, "System", "Average", this->Name);

            SetupOutputVariable("Generator Power Module Entering Fuel Temperature",
                                OutputProcessor::Unit::C,
                                this->Report.TfuelIntoFCPM,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator Fuel Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->Report.NdotFuel, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Fuel Consumption LHV Basis Energy", OutputProcessor::Unit::J, this->Report.FuelEnergyLHV, "System", "Sum", this->Name);

            SetupOutputVariable("Generator Fuel Consumption Rate LHV Basis",
                                OutputProcessor::Unit::W,
                                this->Report.FuelEnergyUseRateLHV,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Power Module Entering Fuel Enthalpy",
                                OutputProcessor::Unit::W,
                                this->Report.TotFuelInEnthalpy,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Fuel Compressor Electric Power",
                                OutputProcessor::Unit::W,
                                this->Report.FuelCompressPower,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator Fuel Compressor Electric Energy", OutputProcessor::Unit::J, this->Report.FuelCompressEnergy, "System", "Sum", this->Name);

            SetupOutputVariable("Generator Fuel Compressor Skin Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                this->Report.FuelCompressSkinLoss,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Fuel Reformer Water Inlet Temperature",
                                OutputProcessor::Unit::C,
                                this->Report.TwaterInlet,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Power Module Entering Reforming Water Temperature",
                                OutputProcessor::Unit::C,
                                this->Report.TwaterIntoFCPM,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Fuel Reformer Water Molar Flow Rate",
                                OutputProcessor::Unit::kmol_s,
                                this->Report.NdotWater,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Fuel Reformer Water Pump Electric Power",
                                OutputProcessor::Unit::W,
                                this->Report.WaterPumpPower,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Fuel Reformer Water Pump Electric Energy",
                                OutputProcessor::Unit::J,
                                this->Report.WaterPumpEnergy,
                                "System",
                                "Sum",
                                this->Name);

            SetupOutputVariable("Generator Power Module Entering Reforming Water Enthalpy",
                                OutputProcessor::Unit::W,
                                this->Report.WaterIntoFCPMEnthalpy,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator Product Gas Temperature", OutputProcessor::Unit::C, this->Report.TprodGas, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Product Gas Enthalpy", OutputProcessor::Unit::W, this->Report.EnthalProdGas, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Product Gas Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->Report.NdotProdGas, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Product Gas Ar Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->Report.NdotProdAr, "System", "Average", this->Name);

            SetupOutputVariable("Generator Product Gas CO2 Molar Flow Rate",
                                OutputProcessor::Unit::kmol_s,
                                this->Report.NdotProdCO2,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Product Gas H2O Vapor Molar Flow Rate",
                                OutputProcessor::Unit::kmol_s,
                                this->Report.NdotProdH2O,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator Product Gas N2 Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->Report.NdotProdN2, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Product Gas O2 Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->Report.NdotProdO2, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Heat Recovery Exit Gas Temperature", OutputProcessor::Unit::C, this->Report.THXexh, "System", "Average", this->Name);

            SetupOutputVariable("Generator Heat Recovery Exit Gas H2O Vapor Fraction",
                                OutputProcessor::Unit::None,
                                this->Report.WaterVaporFractExh,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Heat Recovery Water Condensate Molar Flow Rate",
                                OutputProcessor::Unit::kmol_s,
                                this->Report.CondensateRate,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable("Generator Inverter Loss Power", OutputProcessor::Unit::W, this->Report.PCUlosses, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Produced DC Electric Power", OutputProcessor::Unit::W, this->Report.DCPowerGen, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator DC Power Efficiency", OutputProcessor::Unit::None, this->Report.DCPowerEff, "System", "Average", this->Name);

            SetupOutputVariable("Generator Electric Storage Charge State",
                                OutputProcessor::Unit::J,
                                this->Report.ElectEnergyinStorage,
                                "System",
                                "Average",
                                this->Name);

            SetupOutputVariable(
                "Generator DC Storage Charging Power", OutputProcessor::Unit::W, this->Report.StoredPower, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator DC Storage Charging Energy", OutputProcessor::Unit::J, this->Report.StoredEnergy, "System", "Sum", this->Name);

            SetupOutputVariable(
                "Generator DC Storage Discharging Power", OutputProcessor::Unit::W, this->Report.DrawnPower, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator DC Storage Discharging Energy", OutputProcessor::Unit::J, this->Report.DrawnEnergy, "System", "Sum", this->Name);

            SetupOutputVariable(
                "Generator Ancillary AC Electric Power", OutputProcessor::Unit::W, this->Report.ACancillariesPower, "System", "Average", this->Name);

            SetupOutputVariable(
                "Generator Ancillary AC Electric Energy", OutputProcessor::Unit::J, this->Report.ACancillariesEnergy, "System", "Sum", this->Name);

            SetupOutputVariable("Generator Fuel Cell Model Iteration Count",
                                OutputProcessor::Unit::None,
                                this->Report.SeqSubstIterations,
                                "System",
                                "Sum",
                                this->Name);

            SetupOutputVariable("Generator Root Solver Iteration Count",
                                OutputProcessor::Unit::None,
                                this->Report.RegulaFalsiIterations,
                                "System",
                                "Sum",
                                this->Name);
        }
    }

    void FCDataStruct::CalcFuelCellGeneratorModel(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad, bool const EP_UNUSED(FirstHVACIteration))
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

            if (this->FCPM.HasBeenOn) {
                // FuelCell just now beginning to shut down,

                // set Day and Time of Last Shut Down
                this->FCPM.FractionalDayofLastShutDown =
                    double(DataGlobals::DayOfSim) +
                    (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) /
                        DataGlobals::HoursInDay;
                this->FCPM.HasBeenOn = false;

                if (this->FCPM.ShutDownTime > 0.0) this->FCPM.DuringShutDown = true;
            }

            // TODO  check to see if still in shut down mode and using fuel.
            if (this->FCPM.DuringShutDown) {
            }

            return;
        }

        if (!this->FCPM.HasBeenOn) {
            // fuel cell just turned on
            // set Day and Time of Last STart Up

            this->FCPM.FractionalDayofLastStartUp =
                double(DataGlobals::DayOfSim) +
                (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) /
                    DataGlobals::HoursInDay;

            this->FCPM.HasBeenOn = true;
            ++this->FCPM.NumCycles; // increment cycling counter

            if (this->FCPM.StartUpTime > 0.0) this->FCPM.DuringStartUp = true;
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
                this->FigurePowerConditioningLosses(state, PoutofInverter, PpcuLosses);
                this->FigureACAncillaries(state, PacAncillariesTotal);
                Pdemand = MyLoad + PacAncillariesTotal + PpcuLosses;
            } else {
                // control Step 1a: Figure ancillary AC power draws
                this->FigureACAncillaries(state, PacAncillariesTotal);
                Pdemand = MyLoad + PacAncillariesTotal;
                // Control Step 1b: Calculate losses associated with Power conditioning
                this->FigurePowerConditioningLosses(state, Pdemand, PpcuLosses);
                Pdemand += PpcuLosses;
                Pel = Pdemand;
            }

            this->Inverter.PCUlosses = PpcuLosses;

            // Control step 2: adjust for transient and startup/shut down constraints

            Real64 PelDiff;
            bool ConstrainedFCPMTrans = false;
            this->FigureTransientConstraints(Pel, ConstrainedFCPMTrans, PelDiff);

            // Control step 3: adjust for max and min limits on Pel

            if (Pel < this->FCPM.PelMin) {
                PelDiff += (this->FCPM.PelMin - Pel);
                Pel = this->FCPM.PelMin;

                ConstrainedFCPM = true;
            }
            if (Pel > this->FCPM.PelMax) {
                PelDiff += (this->FCPM.PelMax - Pel);
                Pel = this->FCPM.PelMax;
                ConstrainedFCPM = true;
            }
            if (ConstrainedFCPM) {
            }

            this->FCPM.Pel = Pel;
            // Now calculate FC models.  return to controls and batter after

            // Calculation Step 1. Determine electrical Efficiency Eel

            Real64 Eel = 0.0;
            if (this->FCPM.EffMode == DataGenerators::NormalizedCurveMode) {
                // Equation (8) in FuelCell Spec modified for normalized curve

                Eel = CurveManager::CurveValue(state, this->FCPM.EffCurveID, Pel / this->FCPM.NomPel) * this->FCPM.NomEff *
                      (1.0 - this->FCPM.NumCycles * this->FCPM.CyclingDegradRat) *
                      (1.0 - max((this->FCPM.NumRunHours - this->FCPM.ThreshRunHours), 0.0) * this->FCPM.OperateDegradRat);

            } else if (this->FCPM.EffMode == DataGenerators::DirectCurveMode) {
                // Equation (8) in FuelCell Spec
                Eel = CurveManager::CurveValue(state, this->FCPM.EffCurveID, Pel) * (1.0 - this->FCPM.NumCycles * this->FCPM.CyclingDegradRat) *
                      (1.0 - max((this->FCPM.NumRunHours - this->FCPM.ThreshRunHours), 0.0) * this->FCPM.OperateDegradRat);
            }

            this->FCPM.Eel = Eel;
            // Calculation Step 2. Determine fuel rate

            // fuel flow rate
            Real64 NdotFuel = Pel / (Eel * DataGenerators::FuelSupply(this->FuelSupNum).LHV * 1000000.0); // Eq. 10 solved for Ndot

            this->FCPM.NdotFuel = NdotFuel;
            if (Pel <= 0.0) {
                // TODO zero stuff before leaving
                Pel = 0.0;
                this->FCPM.Pel = 0.0;
                return;
            } else {

                this->FCPM.Pel = Pel;
            }

            // Calculation Step 3. Determine Air rate

            if (this->AirSup.AirSupRateMode == DataGenerators::ConstantStoicsAirRat) { // MEthod 1
                // molar rate coeff working variable
                Real64 NdotO2 = DataGenerators::FuelSupply(this->FuelSupNum).StoicOxygenRate * this->FCPM.NdotFuel * this->AirSup.Stoics;

                this->FCPM.NdotAir = NdotO2 / this->AirSup.O2fraction;

            } else if (this->AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofPel) { // MEthod 2

                this->FCPM.NdotAir =
                    CurveManager::CurveValue(state, this->AirSup.AirFuncPelCurveID, Pel) * (1 + this->AirSup.AirTempCoeff * this->AirSup.TairIntoFCPM);

            } else if (this->AirSup.AirSupRateMode == DataGenerators::QuadraticFuncofNdot) { // method 3
                this->FCPM.NdotAir = CurveManager::CurveValue(state, this->AirSup.AirFuncNdotCurveID, this->FCPM.NdotFuel) *
                                     (1 + this->AirSup.AirTempCoeff * this->AirSup.TairIntoFCPM);
            }

            // Calculation Step 4. fuel compressor power

            DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl =
                CurveManager::CurveValue(state, DataGenerators::FuelSupply(this->FuelSupNum).CompPowerCurveID, this->FCPM.NdotFuel);

            // calculation Step 5, Fuel Compressor (need outlet temperature)

            if (DataGenerators::FuelSupply(this->FuelSupNum).FuelTempMode == DataGenerators::FuelInTempFromNode) {

                DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress =
                    DataLoopNode::Node(DataGenerators::FuelSupply(this->FuelSupNum).NodeNum).Temp;

            } else if (DataGenerators::FuelSupply(this->FuelSupNum).FuelTempMode == DataGenerators::FuelInTempSchedule) {

                DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress =
                    ScheduleManager::GetCurrentScheduleValue(DataGenerators::FuelSupply(this->FuelSupNum).SchedNum);
            }

            //  evaluate  heat capacity at average temperature using shomate
            Real64 Cp; // temp Heat Capacity, used in thermochemistry units of (J/mol K)
            Real64 Tavg =
                (DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress + DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoFCPM) / 2.0;
            this->FigureFuelHeatCap(Tavg, Cp); // Cp in (J/mol K)

            // calculate a Temp of fuel out of compressor and into power module

            if (this->FCPM.NdotFuel <= 0.0) { // just pass through, domain probably collapsed in modeling
                DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoFCPM = DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress;
            } else {
                DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoFCPM =
                    ((1.0 - DataGenerators::FuelSupply(this->FuelSupNum).CompPowerLossFactor) *
                     DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl / (this->FCPM.NdotFuel * Cp * 1000.0)) +
                    DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress; // 1000 Cp units mol-> kmol
            }
            // calc skin losses from fuel compressor
            DataGenerators::FuelSupply(this->FuelSupNum).QskinLoss =
                DataGenerators::FuelSupply(this->FuelSupNum).CompPowerLossFactor * DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl;

            if (DataGenerators::FuelSupply(this->FuelSupNum).QskinLoss < 0.0) {
                ShowWarningError("problem in FuelSupply.QskinLoss " +
                                 General::RoundSigDigits(DataGenerators::FuelSupply(this->FuelSupNum).QskinLoss, 3));
                DataGenerators::FuelSupply(this->FuelSupNum).QskinLoss = 0.0;
            }

            // calculate total fuel enthalpy coming into power module

            // (Hmolfuel in KJ/mol)
            Real64 Hmolfuel; // temp enthalpy of fuel mixture in KJ/mol
            this->FigureFuelEnthalpy(DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoFCPM, Hmolfuel);

            // units, NdotFuel in kmol/sec. Hmolfule in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            this->FCPM.TotFuelInEnthalphy = Hmolfuel * 1000.0 * this->FCPM.NdotFuel * 1000.0;

            // Calculation Step 6, water compressor calculations

            // calculate water consumption

            this->FCPM.NdotLiqwater = CurveManager::CurveValue(state, this->WaterSup.WaterSupRateCurveID, this->FCPM.NdotFuel);

            // set inlet temp.  (could move to init)

            {
                auto const SELECT_CASE_var(this->WaterSup.WaterTempMode);

                if (SELECT_CASE_var == DataGenerators::WaterInReformMains) {

                    this->WaterSup.TwaterIntoCompress = DataEnvironment::WaterMainsTemp;

                } else if ((SELECT_CASE_var == DataGenerators::WaterInReformAirNode) || (SELECT_CASE_var == DataGenerators::WaterInReformWaterNode)) {

                    this->WaterSup.TwaterIntoCompress = DataLoopNode::Node(this->WaterSup.NodeNum).Temp;

                } else if (SELECT_CASE_var == DataGenerators::WaterInReformSchedule) {

                    this->WaterSup.TwaterIntoCompress = ScheduleManager::GetCurrentScheduleValue(this->WaterSup.SchedNum);
                }
            }

            this->WaterSup.PwaterCompEl = CurveManager::CurveValue(state, this->WaterSup.PmpPowerCurveID, this->FCPM.NdotLiqwater);

            // 75.325  J/mol K Water at 0.1 MPa and 298 K, reference NIST WEBBOOK
            Real64 CpWater; // heat capacity of water in molar units
            FigureLiquidWaterHeatCap(this->WaterSup.TwaterIntoCompress, CpWater);

            if (this->FCPM.NdotLiqwater <= 0.0) { // just pass through, domain probably collapsed in modeling
                this->WaterSup.TwaterIntoFCPM = this->WaterSup.TwaterIntoCompress;
            } else {

                this->WaterSup.TwaterIntoFCPM =
                    ((1 - this->WaterSup.PmpPowerLossFactor) * this->WaterSup.PwaterCompEl / (this->FCPM.NdotLiqwater * CpWater * 1000.0)) +
                    this->WaterSup.TwaterIntoCompress;
            }

            this->WaterSup.QskinLoss = this->WaterSup.PmpPowerLossFactor * this->WaterSup.PwaterCompEl;

            if (this->WaterSup.QskinLoss < 0.0) {
                this->WaterSup.QskinLoss = 0.0;
            }

            Real64 HLiqWater;                                                    // temp enthalpy of liquid water in KJ/mol   No Formation
            FigureLiquidWaterEnthalpy(this->WaterSup.TwaterIntoFCPM, HLiqWater); // HLiqWater in KJ/mol

            this->FCPM.WaterInEnthalpy = this->FCPM.NdotLiqwater * HLiqWater * 1000.0 * 1000.0;

            // Calculation Step 7, Air compressor

            this->AirSup.TairIntoBlower = DataLoopNode::Node(this->AirSup.SupNodeNum).Temp;

            this->AirSup.PairCompEl = CurveManager::CurveValue(state, this->AirSup.BlowerPowerCurveID, this->FCPM.NdotAir);

            Tavg = (this->AirSup.TairIntoBlower + this->AirSup.TairIntoFCPM) / 2.0;

            this->FigureAirHeatCap(Tavg, Cp); // Cp in (J/mol K)

            // if PEMFC with stack cooler, then calculate stack cooler impacts
            if (this->StackCooler.StackCoolerPresent) {

                this->StackCooler.qs_cool =
                    (this->StackCooler.r0 + this->StackCooler.r1 * (this->StackCooler.TstackActual - this->StackCooler.TstackNom)) *
                    (1 + this->StackCooler.r2 * Pel + this->StackCooler.r3 * Pel * Pel) * Pel;

                this->FCPM.QdotStackCool = this->StackCooler.qs_cool;
            }

            // Figure heat recovery from Electrical Storage, power conditioning, and auxiliary burner

            {
                auto const SELECT_CASE_var(this->AirSup.IntakeRecoveryMode);

                if (SELECT_CASE_var == DataGenerators::RecoverBurnInvertBatt) {
                    this->AirSup.QintakeRecovery = this->AuxilHeat.QairIntake + this->ElecStorage.QairIntake + this->Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverAuxiliaryBurner) {
                    this->AirSup.QintakeRecovery = this->AuxilHeat.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverInverterBatt) {
                    this->AirSup.QintakeRecovery = this->ElecStorage.QairIntake + this->Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverInverter) {
                    this->AirSup.QintakeRecovery = this->Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverBattery) {
                    this->AirSup.QintakeRecovery = this->ElecStorage.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::NoRecoveryOnAirIntake) {
                    this->AirSup.QintakeRecovery = 0.0;
                }
            }

            if (this->FCPM.NdotAir <= 0.0) { // just pass through, domain probably collapsed in modeling
                this->AirSup.TairIntoFCPM = this->AirSup.TairIntoBlower;

            } else {
                this->AirSup.TairIntoFCPM = (((1 - this->AirSup.BlowerHeatLossFactor) * this->AirSup.PairCompEl + this->AirSup.QintakeRecovery) /
                                             (this->FCPM.NdotAir * Cp * 1000.0)) +
                                            this->AirSup.TairIntoBlower; // 1000 Cp units mol-> kmol
            }

            this->AirSup.QskinLoss = this->AirSup.BlowerHeatLossFactor * this->AirSup.PairCompEl;

            if (this->AirSup.QskinLoss < 0.0) {
                ShowWarningError("problem in AirSup.QskinLoss " + General::RoundSigDigits(this->AirSup.QskinLoss, 3));
                this->AirSup.QskinLoss = 0.0;
            }

            Real64 Hmolair;                                              // temp enthalpy of air mixture in KJ/mol
            this->FigureAirEnthalpy(this->AirSup.TairIntoFCPM, Hmolair); // (Hmolair in KJ/mol)

            // units, NdotAir in kmol/sec.; Hmolfuel in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            this->FCPM.TotAirInEnthalphy = Hmolair * 1000.0 * this->FCPM.NdotAir * 1000.0;

            // calculation Step 8, Figure Product Gases

            // figure stoic N dot for air
            Real64 NdotO2 = DataGenerators::FuelSupply(this->FuelSupNum).StoicOxygenRate * this->FCPM.NdotFuel;

            // Air in excess of match for fuel
            Real64 NdotStoicAir = NdotO2 / this->AirSup.O2fraction;

            // figure excess air rate

            // Air in excess of match for fuel
            Real64 NdotExcessAir = this->FCPM.NdotAir - NdotStoicAir;

            if (NdotExcessAir < 0) { // can't meet stoichiometric fuel reaction

                ShowWarningError("Air flow rate into fuel cell is too low for stoichiometric fuel reaction");
                ShowContinueError("Increase air flow in GENERATOR:FC:AIR SUPPLY object:" + this->AirSup.Name);
            }

            // figure CO2 and Water rate from products (coefs setup during one-time processing in gas phase library )

            // CO2 from reaction
            Real64 NdotCO2ProdGas = this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).CO2ProductGasCoef;

            // Water from reaction
            Real64 NdotH2OProdGas = this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).H2OProductGasCoef;

            //  set product gas constituent fractions  (assume five usual components)
            Real64 NdotCO2 = 0.0; // temp CO2 molar rate coef product gas stream
            Real64 NdotN2 = 0.0;  // temp Nitrogen rate coef product gas stream
            Real64 Ndot02 = 0.0;  // temp Oxygen rate coef product gas stream
            Real64 NdotH2O = 0.0; // temp Water rate coef product gas stream
            Real64 NdotAr = 0.0;  // temp Argon rate coef product gas stream

            // Product gas constituents are fixed (not a user defined thing)

            for (int thisGas = 1; thisGas <= this->AirSup.NumConstituents; ++thisGas) {

                {
                    auto const SELECT_CASE_var(this->AirSup.GasLibID(thisGas));

                    if (SELECT_CASE_var == 1) {
                        // all the CO2 coming in plus the new CO2 from reactions
                        NdotCO2 = NdotCO2ProdGas + this->AirSup.ConstitMolalFract(thisGas) * this->FCPM.NdotAir;

                    } else if (SELECT_CASE_var == 2) {
                        // all the nitrogen coming in
                        NdotN2 = this->FCPM.NdotAir * this->AirSup.ConstitMolalFract(thisGas);

                    } else if (SELECT_CASE_var == 3) {
                        // all the oxygen in the excess air stream
                        Ndot02 = NdotExcessAir * this->AirSup.ConstitMolalFract(thisGas);

                    } else if (SELECT_CASE_var == 4) {
                        // all the H2O coming in plus the new H2O from reactions and the H2O from water used in reforming
                        NdotH2O = NdotH2OProdGas + this->AirSup.ConstitMolalFract(thisGas) * this->FCPM.NdotAir;

                    } else if (SELECT_CASE_var == 5) {
                        // all the argon coming in.
                        NdotAr = this->FCPM.NdotAir * this->AirSup.ConstitMolalFract(thisGas);

                    } else {
                    }
                }
            }

            this->FCPM.NdotProdGas = NdotCO2 + NdotN2 + Ndot02 + NdotH2O + NdotAr;

            // now that we have the total, figure molar fractions

            this->FCPM.ConstitMolalFract(1) = NdotCO2 / this->FCPM.NdotProdGas;

            // all the nitrogen coming in
            this->FCPM.ConstitMolalFract(2) = NdotN2 / this->FCPM.NdotProdGas;

            // all the oxygen in the excess air stream
            this->FCPM.ConstitMolalFract(3) = Ndot02 / this->FCPM.NdotProdGas;

            // all the H2O comming in plus the new H2O from reactions and the H2O from water used in reforming
            this->FCPM.ConstitMolalFract(4) = NdotH2O / this->FCPM.NdotProdGas;

            // all the argon coming in.
            this->FCPM.ConstitMolalFract(5) = NdotAr / this->FCPM.NdotProdGas;

            // HmolProdGases KJ/mol)
            Real64 HmolProdGases; // enthalpy of product gas mixture in KJ/mol
            this->FigureProductGasesEnthalpy(this->FCPM.TprodGasLeavingFCPM, HmolProdGases);

            // units, NdotProdGas in kmol/sec.; HmolProdGases in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            this->FCPM.TotProdGasEnthalphy = HmolProdGases * 1000.0 * this->FCPM.NdotProdGas * 1000.0;

            // calculation Step 9, Figure Skin lossess

            if (this->FCPM.SkinLossMode == DataGenerators::ConstantRateSkinLoss) {
                // do nothing just use QdotSkin

            } else if (this->FCPM.SkinLossMode == DataGenerators::UADTSkinLoss) {

                // get zone air temp
                if (this->FCPM.ZoneID > 0) {
                    this->FCPM.QdotSkin = this->FCPM.UAskin * (this->FCPM.TprodGasLeavingFCPM - DataHeatBalFanSys::ZT(this->FCPM.ZoneID));
                }

            } else if (this->FCPM.SkinLossMode == DataGenerators::QuadraticFuelNdotSkin) {

                this->FCPM.QdotSkin = CurveManager::CurveValue(state, this->FCPM.SkinLossCurveID, this->FCPM.NdotFuel);
            }

            // calculation Step 10, AC FCPM power ancillaries

            this->FCPM.PelancillariesAC = this->FCPM.ANC0 + this->FCPM.ANC1 * this->FCPM.NdotFuel;

            // calculation Step 11, Dilution air
            this->FigureAirEnthalpy(this->AirSup.TairIntoBlower, Hmolair); // (Hmolair in KJ/mol)

            // units, NdotDilutionAir in kmol/sec.; Hmolair in KJ/mol ,
            //        factor of 1000's to get to J/s or watts
            this->FCPM.DilutionAirInEnthalpy = Hmolair * 1000.0 * this->FCPM.NdotDilutionAir * 1000.0;
            this->FCPM.DilutionAirOutEnthalpy = this->FCPM.DilutionAirInEnthalpy + this->FCPM.StackHeatLossToDilution;

            // calculation Step 12, Calculate Reforming water out enthalpy
            Real64 HGasWater; // temp enthalpy of gaseous water in KJ/mol  No Formation
            FigureGaseousWaterEnthalpy(this->FCPM.TprodGasLeavingFCPM, HGasWater);

            this->FCPM.WaterOutEnthalpy = HGasWater * 1000.0 * this->FCPM.NdotLiqwater * 1000.0;

            // calculation Step 13, Calculate Heat balance
            //    move all terms in Equation 7 to RHS and calculate imbalance

            Real64 MagofImbalance = -this->FCPM.TotFuelInEnthalphy - this->FCPM.TotAirInEnthalphy - this->FCPM.WaterInEnthalpy -
                                    this->FCPM.DilutionAirInEnthalpy -
                                    this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).LHV * 1000000.0 - this->FCPM.PelancillariesAC +
                                    this->FCPM.Pel + this->FCPM.TotProdGasEnthalphy + this->FCPM.WaterOutEnthalpy + this->FCPM.QdotStackCool +
                                    this->FCPM.QdotSkin + this->FCPM.DilutionAirOutEnthalpy;

            // Now find a new total prod Gas Enthalphy that would result in an energy balance
            // TODO check signs...
            Real64 tmpTotProdGasEnthalpy = this->FCPM.TotProdGasEnthalphy - MagofImbalance;

            // solve for a new TprodGasLeavingFCPM using regula falsi method

            Real64 Acc = 0.01;      // guessing need to refine
            int MaxIter = 150;      // guessing need to refine
            SolverFlag = 0;         // init
            Array1D<Real64> Par(2); // parameters passed in to SolveRoot
            Par(1) = tmpTotProdGasEnthalpy;
            Par(2) = this->FCPM.NdotProdGas;
            Real64 tmpTprodGas = this->FCPM.TprodGasLeavingFCPM;
            auto boundFunc = std::bind(&FCDataStruct::FuelCellProductGasEnthResidual, this, std::placeholders::_1, std::placeholders::_2);
            General::SolveRoot(
                Acc, MaxIter, SolverFlag, tmpTprodGas, boundFunc, DataGenerators::MinProductGasTemp, DataGenerators::MaxProductGasTemp, Par);

            if (SolverFlag == -2) {

                ShowWarningError("CalcFuelCellGeneratorModel: Root Solver problem, flag = -2, check signs, all positive");
            }
            if (SolverFlag == -1) {
                ShowWarningError("CalcFuelCellGeneratorModel: Root Solver problem, flag = -1, check accuracy and iterations, did not converge");
            }
            if (SolverFlag > 0) {
                this->FCPM.TprodGasLeavingFCPM = tmpTprodGas;
                //  write(*,*) 'Number of Root Solver iterations: ', solverFlag
            }

            // Control Step 3 determine interaction with electrical storage
            // How much power is really going into inverter?
            Real64 PintoInverter = Pel + Pstorage; // Back out so we can reapply
            bool ConstrainedStorage;
            this->ManageElectStorInteractions(Pdemand, PpcuLosses, ConstrainedStorage, Pstorage, PgridExtra);
            PintoInverter = Pel - Pstorage;
            // refine power conditioning losses with more current power production

            if (this->Inverter.EffMode == DataGenerators::InverterEffConstant) {

                PpcuLosses = (1.0 - this->Inverter.ConstEff) * PintoInverter;
            }

            if (this->Inverter.EffMode == DataGenerators::InverterEffQuadratic) {

                PpcuLosses = (1.0 - CurveManager::CurveValue(state, this->Inverter.EffQuadraticCurveID, PintoInverter)) * PintoInverter;
            }

            PoutofInverter = PintoInverter - PpcuLosses;

            this->ACPowerGen = PoutofInverter - this->FCPM.PelancillariesAC - this->AirSup.PairCompEl -
                               DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl - this->WaterSup.PwaterCompEl;
            this->Inverter.PCUlosses = PpcuLosses;
            // model assumes air intake is drawn over power conditioner to recovery heat
            this->Inverter.QairIntake = this->Inverter.PCUlosses;

            this->CalcFuelCellAuxHeater();

            this->CalcFuelCellGenHeatRecovery();
            // calculation Step 11, If imbalance below threshold, then exit out of do loop.

            if ((std::abs(MagofImbalance) < std::abs(DataGenerators::ImBalanceTol * this->FCPM.Pel)) && (iter > 2)) {
                break;
            }

        } // sequential substitution loop

        this->FCPM.SeqSubstitIter = iter;
        this->FCPM.RegulaFalsiIter = SolverFlag;
    }

    void FCDataStruct::ManageElectStorInteractions(Real64 const Pdemand,
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
        bool charging = false; // true if charging
        Constrained = false;

        // step 1 figure out what is desired of electrical storage system

        if (this->FCPM.Pel < (Pdemand)) {
            // draw from storage
            tmpPdraw = (Pdemand) - this->FCPM.Pel;
            drawing = true;
        }

        if (this->FCPM.Pel > (Pdemand)) {
            // add to storage
            tmpPcharge = this->FCPM.Pel - (Pdemand);
            charging = true;
        }

        //  step 2, figure out what is possible for electrical storage draws/charges

        if (charging) {

            if (this->ElecStorage.StorageModelMode == DataGenerators::SimpleEffConstraints) {

                if (this->ElecStorage.LastTimeStepStateOfCharge >= this->ElecStorage.NominalEnergyCapacity) {
                    // storage full!  no more allowed!
                    PgridOverage = tmpPcharge;
                    tmpPcharge = 0.0;
                    Constrained = true;
                }
                if (tmpPcharge > this->ElecStorage.MaxPowerStore) {
                    PgridOverage = tmpPcharge - this->ElecStorage.MaxPowerStore;
                    tmpPcharge = this->ElecStorage.MaxPowerStore;
                    Constrained = true;
                }

                // now add energy to storage from charging
                if ((this->ElecStorage.LastTimeStepStateOfCharge +
                     tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->ElecStorage.EnergeticEfficCharge) <
                    this->ElecStorage.NominalEnergyCapacity) {

                    this->ElecStorage.ThisTimeStepStateOfCharge =
                        this->ElecStorage.LastTimeStepStateOfCharge +
                        tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->ElecStorage.EnergeticEfficCharge;
                } else { // would over charge this time step

                    tmpPcharge = (this->ElecStorage.NominalEnergyCapacity - this->ElecStorage.LastTimeStepStateOfCharge) /
                                 (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->ElecStorage.EnergeticEfficCharge);
                    Constrained = true;
                    this->ElecStorage.ThisTimeStepStateOfCharge =
                        this->ElecStorage.LastTimeStepStateOfCharge +
                        tmpPcharge * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour * this->ElecStorage.EnergeticEfficCharge;
                }

                // losses go into QairIntake
                this->ElecStorage.QairIntake = tmpPcharge * (1.0 - this->ElecStorage.EnergeticEfficCharge);

            } else if (this->ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterManwellMcGowan) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ");

            } else if (this->ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterySaupe) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ");

            } else {

                // should not come here
            }

            Pstorage = tmpPcharge;

        } // charging

        if (drawing) {
            if (this->ElecStorage.StorageModelMode == DataGenerators::SimpleEffConstraints) {

                if (this->ElecStorage.LastTimeStepStateOfCharge <= 0.0) {
                    // storage empty  no more allowed!
                    tmpPdraw = 0.0;
                    Constrained = true;
                    drawing = false;
                }
                if (tmpPdraw > this->ElecStorage.MaxPowerDraw) {
                    tmpPdraw = this->ElecStorage.MaxPowerDraw;
                    Constrained = true;
                }

                // now take energy from storage by drawing  (amplified by energetic effic)
                if ((this->ElecStorage.LastTimeStepStateOfCharge -
                     tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->ElecStorage.EnergeticEfficDischarge) > 0.0) {

                    this->ElecStorage.ThisTimeStepStateOfCharge =
                        this->ElecStorage.LastTimeStepStateOfCharge -
                        tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->ElecStorage.EnergeticEfficDischarge;
                } else { // would over drain storage this timestep so reduce tmpPdraw
                    tmpPdraw = this->ElecStorage.LastTimeStepStateOfCharge * this->ElecStorage.EnergeticEfficDischarge /
                               (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
                    this->ElecStorage.ThisTimeStepStateOfCharge =
                        this->ElecStorage.LastTimeStepStateOfCharge -
                        tmpPdraw * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour / this->ElecStorage.EnergeticEfficDischarge;

                    Constrained = true;
                }
                // losses go into QairIntake
                this->ElecStorage.QairIntake = tmpPdraw * (1.0 / this->ElecStorage.EnergeticEfficDischarge - 1.0);
            } else if (this->ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterManwellMcGowan) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ");

            } else if (this->ElecStorage.StorageModelMode == DataGenerators::LeadAcidBatterySaupe) {
                ShowWarningError("ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ");

            } else {

                // should not come here
            }

            Pstorage = -tmpPdraw;

        } // drawing

        if ((!charging) && (!drawing)) {

            this->ElecStorage.ThisTimeStepStateOfCharge = this->ElecStorage.LastTimeStepStateOfCharge;
            this->ElecStorage.PelNeedFromStorage = 0.0;
            this->ElecStorage.PelFromStorage = 0.0;
            this->ElecStorage.QairIntake = 0.0;
        }

        if (Pstorage >= 0.0) {

            this->ElecStorage.PelIntoStorage = Pstorage;
            this->ElecStorage.PelFromStorage = 0.0;
        }
        if (Pstorage < 0.0) {

            this->ElecStorage.PelIntoStorage = 0.0;
            this->ElecStorage.PelFromStorage = -Pstorage;
        }
    }

    Real64 FCDataStruct::FuelCellProductGasEnthResidual(Real64 const TprodGas,     // temperature, this is "x" being searched
                                                        Array1D<Real64> const &Par // par(1) = Generator Number
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
        Real64 desiredHprodGases = Par(1);
        Real64 NdotProdGases = Par(2);

        this->FigureProductGasesEnthalpy(TprodGas, thisHmolalProdGases);

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

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) * this->AirSup.ConstitMolalFract(thisConstit));
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

        Real64 A;  // shomate coeff
        Real64 B;  // shomate coeff
        Real64 C;  // shomate coeff
        Real64 D;  // shomate coeff
        Real64 E;  // shomate coeff
        Real64 F;  // shomate coeff
        Real64 H;  // shomate coeff
        Real64 A1; // NASA poly coeff
        Real64 A2; // NASA poly coeff
        Real64 A3; // NASA poly coeff
        Real64 A4; // NASA poly coeff
        Real64 A5; // NASA poly coeff
        Real64 A6; // NASA poly coeff

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

        Real64 A;  // shomate coeff
        Real64 B;  // shomate coeff
        Real64 C;  // shomate coeff
        Real64 D;  // shomate coeff
        Real64 E;  // shomate coeff
        Real64 F;  // shomate coeff
        Real64 H;  // shomate coeff
        Real64 A1; // NASA poly coeff
        Real64 A2; // NASA poly coeff
        Real64 A3; // NASA poly coeff
        Real64 A4; // NASA poly coeff
        Real64 A5; // NASA poly coeff
        Real64 A6; // NASA poly coeff

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

        Real64 A;  // shomate coeff
        Real64 B;  // shomate coeff
        Real64 C;  // shomate coeff
        Real64 D;  // shomate coeff
        Real64 E;  // shomate coeff
        Real64 F;  // shomate coeff
        Real64 H;  // shomate coeff
        Real64 A1; // NASA poly coeff
        Real64 A2; // NASA poly coeff
        Real64 A3; // NASA poly coeff
        Real64 A4; // NASA poly coeff
        Real64 A5; // NASA poly coeff
        Real64 A6; // NASA poly coeff

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

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) * this->FCPM.ConstitMolalFract(thisConstit));
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

                    tempCp += ((A + B * Tsho + C * pow_2_Tsho + D * pow_3_Tsho + E / pow_2_Tsho) * this->AuxilHeat.ConstitMolalFract(thisConstit));
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

    void FCDataStruct::FigureGaseousWaterEnthalpy(Real64 const FluidTemp, // degree C
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
        // No enthalpy of formation in this one.

        // REFERENCES:
        // NIST Webbook on gas phase thermochemistry

        Real64 const A = 29.0373;                                           // shomate coeff
        Real64 const B = 10.2573;                                           // shomate coeff
        Real64 const C = 2.81048;                                           // shomate coeff
        Real64 const D = -0.95914;                                          // shomate coeff
        Real64 const E = 0.11725;                                           // shomate coeff
        Real64 const F = -250.569;                                          // shomate coeff
        Real64 const Tsho = (FluidTemp + DataGlobals::KelvinConv) / 1000.0; // temp for Shomate eq  in (Kelvin/1000)

        HGasWater = A * Tsho + B * pow_2(Tsho) / 2.0 + C * pow_3(Tsho) / 3.0 + D * pow_4(Tsho) / 4.0 - E / Tsho + F; //- H
    }

    void FCDataStruct::FigureLiquidWaterEnthalpy(Real64 const FluidTemp, // degree C
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

    void FCDataStruct::FigureLiquidWaterHeatCap(Real64 const FluidTemp, // degree C
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

    void FCDataStruct::FigureACAncillaries(EnergyPlusData &state, Real64 &PacAncill)
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
        this->FCPM.PelancillariesAC = this->FCPM.ANC0 + this->FCPM.ANC1 * this->FCPM.NdotFuel;

        // sect 6.0
        this->AirSup.PairCompEl = CurveManager::CurveValue(state, this->AirSup.BlowerPowerCurveID, this->FCPM.NdotAir);
        // sect 7.0
        DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl =
            CurveManager::CurveValue(state, DataGenerators::FuelSupply(this->FuelSupNum).CompPowerCurveID, this->FCPM.NdotFuel);

        // sect. 8.0
        this->WaterSup.PwaterCompEl = CurveManager::CurveValue(state, this->WaterSup.PmpPowerCurveID, this->FCPM.NdotLiqwater);

        PacAncill = this->FCPM.PelancillariesAC + this->AirSup.PairCompEl + DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl +
                    this->WaterSup.PwaterCompEl;
    }

    void FCDataStruct::FigurePowerConditioningLosses(EnergyPlusData &state, Real64 const Pdemand, Real64 &PpcuLosses)
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
            Real64 lastPpcuLosses = Pdemand * (1.0 - CurveManager::CurveValue(state, this->Inverter.EffQuadraticCurveID, Pdemand)) /
                                    CurveManager::CurveValue(state, this->Inverter.EffQuadraticCurveID, Pdemand);

            for (int iter = 1; iter <= 20; ++iter) { // seems like need to iterate (??) Need to investigate number and convergence success here

                Real64 Pel = Pdemand + lastPpcuLosses;

                lastPpcuLosses = (1.0 - CurveManager::CurveValue(state, this->Inverter.EffQuadraticCurveID, Pel)) * Pel;
            }

            PpcuLosses = lastPpcuLosses;
        }
    }

    void FCDataStruct::FigureTransientConstraints(Real64 &Pel,       // DC power control setting for power module
                                                  bool &Constrained, // true if transient constraints kick in
                                                  Real64 &PelDiff    // if constrained then this is the difference, positive
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 PelInput = Pel; // hold initial value of inout var

        Real64 CurrentFractionalDay =
            double(DataGlobals::DayOfSim) +
            (int(DataGlobals::CurrentTime) + (DataHVACGlobals::SysTimeElapsed + (DataGlobals::CurrentTime - int(DataGlobals::CurrentTime)))) /
                DataGlobals::HoursInDay;

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

    void FCDataStruct::CalcFuelCellAuxHeater() // Generator number
    {

        // not yet implemented, just pass product gases thru nul domain

        this->AuxilHeat.TauxMix = this->FCPM.TprodGasLeavingFCPM;
        this->AuxilHeat.NdotAuxMix = this->FCPM.NdotProdGas;
        this->AuxilHeat.ConstitMolalFract = this->FCPM.ConstitMolalFract;
        this->AuxilHeat.GasLibID = this->FCPM.GasLibID;
    }

    void FCDataStruct::CalcFuelCellGenHeatRecovery() // Generator number
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
            auto const SELECT_CASE_var(this->ExhaustHX.HXmodelMode);

            if (SELECT_CASE_var == DataGenerators::FixedEffectiveness) { // Method 1

                Real64 eHX = this->ExhaustHX.HXEffect;

                Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                Real64 NdotWater = this->ExhaustHX.WaterMassFlowRate / MWwater;
                Real64 TwaterIn = this->ExhaustHX.WaterInletTemp;

                Real64 CpWaterMol;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);

                Real64 NdotGas = this->AuxilHeat.NdotAuxMix;
                Real64 TprodGasIn = this->AuxilHeat.TauxMix;
                Real64 CpProdGasMol;
                this->FigureAuxilHeatGasHeatCap(TprodGasIn, CpProdGasMol); // Cp in (J/mol*K)
                // factor of 1000.0 for kmol -> mol
                Real64 NdotCp = min(NdotGas * CpProdGasMol * 1000.0, NdotWater * CpWaterMol * 1000.0);

                this->ExhaustHX.qHX = eHX * NdotCp * (TprodGasIn - TwaterIn);

                this->ExhaustHX.THXexh = TprodGasIn - this->ExhaustHX.qHX / (NdotGas * CpProdGasMol * 1000.0);

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(this->CWLoopNum).FluidName, TwaterIn, DataPlant::PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);

                if (this->ExhaustHX.WaterMassFlowRate * Cp <= 0.0) {
                    this->ExhaustHX.WaterOutletTemp = TwaterIn;
                } else {
                    this->ExhaustHX.WaterOutletTemp = TwaterIn + this->ExhaustHX.qHX / (this->ExhaustHX.WaterMassFlowRate * Cp);
                }

            } else if (SELECT_CASE_var == DataGenerators::LMTDempiricalUAeff) { // method 2
                Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                Real64 NdotWater = this->ExhaustHX.WaterMassFlowRate / MWwater;
                Real64 NdotGas = this->AuxilHeat.NdotAuxMix;

                Real64 UAeff = this->ExhaustHX.hxs0 + this->ExhaustHX.hxs1 * NdotWater + this->ExhaustHX.hxs2 * pow_2(NdotWater) +
                               this->ExhaustHX.hxs3 * NdotGas + this->ExhaustHX.hxs4 * pow_2(NdotGas);

                Real64 TauxMix = this->AuxilHeat.TauxMix;
                Real64 TwaterIn = this->ExhaustHX.WaterInletTemp;
                Real64 CpWaterMol;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                // factor of 1000.0 for kmol -> mol
                Real64 NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                Real64 CpProdGasMol;
                this->FigureAuxilHeatGasHeatCap(TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                Real64 NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero
                    // now evaluate Eq. 44
                    this->ExhaustHX.THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                                 TauxMix +
                                             ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                                 TwaterIn;

                    this->ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - this->ExhaustHX.THXexh); // Eq. 42

                } else {
                    this->ExhaustHX.THXexh = TauxMix;
                    this->ExhaustHX.WaterOutletTemp = TwaterIn;
                }
                // ENDIF

                if ((this->ExhaustHX.THXexh - TwaterIn) != 0.0) { // trap divide by zero
                    this->ExhaustHX.qHX = UAeff * ((TauxMix - this->ExhaustHX.WaterOutletTemp) - (this->ExhaustHX.THXexh - TwaterIn)) /
                                          std::log((TauxMix - this->ExhaustHX.WaterOutletTemp) / (this->ExhaustHX.THXexh - TwaterIn));
                } else {
                    this->ExhaustHX.qHX = 0.0;
                }

            } else if (SELECT_CASE_var == DataGenerators::LMTDfundementalUAeff) { // method 3
                Real64 NdotGas = this->AuxilHeat.NdotAuxMix;
                Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                Real64 NdotWater = this->ExhaustHX.WaterMassFlowRate / MWwater;

                Real64 hgas = this->ExhaustHX.h0gas * std::pow(NdotGas / this->ExhaustHX.NdotGasRef, this->ExhaustHX.nCoeff);         // Eq. 48
                Real64 hwater = this->ExhaustHX.h0Water * std::pow(NdotWater / this->ExhaustHX.NdotWaterRef, this->ExhaustHX.mCoeff); // Eq. 48

                // now equation 47
                Real64 UAeff = 1.0 / (1.0 / (hgas * this->ExhaustHX.AreaGas) + 1.0 / (hwater * this->ExhaustHX.AreaWater) + this->ExhaustHX.Fadjust);

                Real64 TauxMix = this->AuxilHeat.TauxMix;
                Real64 TwaterIn = this->ExhaustHX.WaterInletTemp;
                Real64 CpWaterMol;
                FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                Real64 NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                Real64 CpProdGasMol;
                this->FigureAuxilHeatGasHeatCap(TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                Real64 NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero
                    // now evaluate Eq. 44
                    this->ExhaustHX.THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                                 TauxMix +
                                             ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                                              (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                                 TwaterIn;

                    this->ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - this->ExhaustHX.THXexh); // Eq. 42

                } else {
                    this->ExhaustHX.THXexh = TauxMix;
                    this->ExhaustHX.WaterOutletTemp = TwaterIn;
                }

                if ((this->ExhaustHX.THXexh - TwaterIn) != 0.0) { // trap divide by zero
                    this->ExhaustHX.qHX = UAeff * ((TauxMix - this->ExhaustHX.WaterOutletTemp) - (this->ExhaustHX.THXexh - TwaterIn)) /
                                          std::log((TauxMix - this->ExhaustHX.WaterOutletTemp) / (this->ExhaustHX.THXexh - TwaterIn));
                } else {
                    this->ExhaustHX.qHX = 0.0;
                }

            } else if (SELECT_CASE_var == DataGenerators::Condensing) { // method 4
                if (this->ExhaustHX.WaterMassFlowRate != 0.0) {

                    Real64 MWwater = DataGenerators::GasPhaseThermoChemistryData(4).MolecularWeight;
                    Real64 NdotWater = this->ExhaustHX.WaterMassFlowRate / MWwater;
                    Real64 NdotGas = this->AuxilHeat.NdotAuxMix;

                    Real64 UAeff = this->ExhaustHX.hxs0 + this->ExhaustHX.hxs1 * NdotWater + this->ExhaustHX.hxs2 * pow_2(NdotWater) +
                                   this->ExhaustHX.hxs3 * NdotGas + this->ExhaustHX.hxs4 * pow_2(NdotGas);

                    Real64 TauxMix = this->AuxilHeat.TauxMix;
                    Real64 TwaterIn = this->ExhaustHX.WaterInletTemp;
                    Real64 CpWaterMol;
                    FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol);
                    Real64 NdotCpWater = NdotWater * CpWaterMol * 1000.0;
                    Real64 CpProdGasMol;
                    this->FigureAuxilHeatGasHeatCap(TauxMix, CpProdGasMol); // Cp in (J/mol*K)
                    Real64 NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

                    // find water fraction in incoming gas stream
                    for (int i = 1; i <= isize(this->AuxilHeat.GasLibID); ++i) {
                        if (this->AuxilHeat.GasLibID(i) == 4) this->ExhaustHX.WaterVaporFractExh = this->AuxilHeat.ConstitMolalFract(i);
                    }
                    Real64 NdotWaterVapor = this->ExhaustHX.WaterVaporFractExh * NdotGas;

                    Real64 TcondThresh = this->ExhaustHX.CondensationThresholdTemp;
                    Real64 hxl1 = this->ExhaustHX.l1Coeff;
                    Real64 hxl2 = this->ExhaustHX.l2Coeff;

                    this->ExhaustHX.CondensateRate =
                        (TcondThresh - TwaterIn) * (hxl1 * (NdotWaterVapor / NdotGas) + hxl2 * pow_2(NdotWaterVapor / NdotGas));

                    if (this->ExhaustHX.CondensateRate < 0.0) this->ExhaustHX.CondensateRate = 0.0;

                    Real64 hfpwater = 4.4004e+07; // molal heat of vaporization of water J/kmol)

                    if ((NdotCpWater != 0.0) && (NdotCpAuxMix != 0.0)) { // trap divide by zero

                        // now evaluate Eq. 44
                        this->ExhaustHX.THXexh = ((1.0 - NdotCpAuxMix / NdotCpWater) /
                                                  (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                                     TauxMix +
                                                 ((std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - 1.0) /
                                                  (std::exp(UAeff * (1.0 / NdotCpAuxMix - 1.0 / NdotCpWater)) - NdotCpAuxMix / NdotCpWater)) *
                                                     TwaterIn;

                        this->ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - this->ExhaustHX.THXexh) +
                                                          (this->ExhaustHX.CondensateRate * hfpwater) / NdotCpWater;

                        if (this->ExhaustHX.CondensateRate > 0) { // Eq. 44 is not correct. use its result as first guess for revised way...
                            // iterative solution because in condensing case THXexh is function of qSens and qLatent
                            for (int loop = 1; loop <= 5; ++loop) {

                                Real64 qSens;
                                Real64 qLatent;

                                if ((this->ExhaustHX.THXexh - TwaterIn) != 0.0 &&
                                    ((TauxMix - this->ExhaustHX.WaterOutletTemp) / (this->ExhaustHX.THXexh - TwaterIn) >
                                     0.0001)) { // trap divide by zero and negative log
                                    qSens = UAeff * ((TauxMix - this->ExhaustHX.WaterOutletTemp) - (this->ExhaustHX.THXexh - TwaterIn)) /
                                            std::log((TauxMix - this->ExhaustHX.WaterOutletTemp) / (this->ExhaustHX.THXexh - TwaterIn));
                                } else {
                                    qSens = 0.0;
                                }
                                qLatent = this->ExhaustHX.CondensateRate * hfpwater;
                                if (qSens > 0) {
                                    this->ExhaustHX.THXexh =
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
                                    this->ExhaustHX.THXexh = TauxMix;
                                }

                                this->ExhaustHX.WaterOutletTemp = TwaterIn + (NdotCpAuxMix / NdotCpWater) * (TauxMix - this->ExhaustHX.THXexh) +
                                                                  (this->ExhaustHX.CondensateRate * hfpwater) / NdotCpWater;
                            }
                        }

                    } else {
                        this->ExhaustHX.THXexh = TauxMix;
                        this->ExhaustHX.WaterOutletTemp = TwaterIn;
                    }

                    if ((this->ExhaustHX.THXexh - TwaterIn) != 0.0 &&
                        ((TauxMix - this->ExhaustHX.WaterOutletTemp) / (this->ExhaustHX.THXexh - TwaterIn) >
                         0.0001)) { // trap divide by zero and negative log

                        this->ExhaustHX.qHX = UAeff * ((TauxMix - this->ExhaustHX.WaterOutletTemp) - (this->ExhaustHX.THXexh - TwaterIn)) /
                                                  std::log((TauxMix - this->ExhaustHX.WaterOutletTemp) / (this->ExhaustHX.THXexh - TwaterIn)) +
                                              this->ExhaustHX.CondensateRate * hfpwater;
                    } else {
                        this->ExhaustHX.qHX = 0.0;
                    }
                } else { // no cooling water flow, model will blow up.
                    this->ExhaustHX.qHX = 0.0;
                    this->ExhaustHX.THXexh = this->AuxilHeat.TauxMix;
                    this->ExhaustHX.WaterOutletTemp = this->ExhaustHX.WaterInletTemp;
                    this->ExhaustHX.CondensateRate = 0.0;
                    this->ExhaustHX.WaterVaporFractExh = -9999.0; // not defined
                }
            } else {
                assert(false); // Variables not set are used below
            }
        }

        // update results in data structure.
        this->ExhaustHX.WaterOutletEnthalpy = DataLoopNode::Node(this->ExhaustHX.WaterInNode).Enthalpy + this->ExhaustHX.qHX;
    }

    void FCDataStruct::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MaxLoad = 0.0;
        MinLoad = 0.0;
        OptLoad = 0.0;
    }

    void FCDataStruct::simulate(EnergyPlusData &EP_UNUSED(state), const PlantLocation &EP_UNUSED(calledFromLocation),
                                bool FirstHVACIteration,
                                Real64 &EP_UNUSED(CurLoad),
                                bool EP_UNUSED(RunFlag))
    {
        if (this->TypeOf == DataPlant::TypeOf_Generator_FCStackCooler) {
            PlantUtilities::UpdateComponentHeatRecoverySide(this->CWLoopNum,
                                                            this->CWLoopSideNum,
                                                            DataPlant::TypeOf_Generator_FCStackCooler,
                                                            this->StackCooler.WaterInNode,
                                                            this->StackCooler.WaterOutNode,
                                                            this->Report.qHX,
                                                            this->Report.HeatRecInletTemp,
                                                            this->Report.HeatRecOutletTemp,
                                                            this->Report.HeatRecMdot,
                                                            FirstHVACIteration);
        } else if (this->TypeOf == DataPlant::TypeOf_Generator_FCExhaust) {
            PlantUtilities::UpdateComponentHeatRecoverySide(this->CWLoopNum,
                                                            this->CWLoopSideNum,
                                                            DataPlant::TypeOf_Generator_FCExhaust,
                                                            this->ExhaustHX.WaterInNode,
                                                            this->ExhaustHX.WaterOutNode,
                                                            this->ExhaustHX.qHX,
                                                            this->ExhaustHX.WaterInletTemp,
                                                            this->ExhaustHX.WaterOutletTemp,
                                                            this->ExhaustHX.WaterMassFlowRate,
                                                            FirstHVACIteration);
        }
    }

    void FCDataStruct::initialize(BranchInputManagerData &dataBranchInputManager) // index to specific fuel cell generator
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

        if (this->MyPlantScanFlag_Init && allocated(DataPlant::PlantLoop)) {
            bool errFlag = false;

            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                    this->NameExhaustHX,
                                                    DataPlant::TypeOf_Generator_FCExhaust,
                                                    this->CWLoopNum,
                                                    this->CWLoopSideNum,
                                                    this->CWBranchNum,
                                                    this->CWCompNum,
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
            this->MyPlantScanFlag_Init = false;
        }

        // Do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && this->MyEnvrnFlag_Init && !this->MyPlantScanFlag_Init) {

            DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl = 0.0;
            DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoFCPM = 0.0;
            DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress = 0.0;
            DataGenerators::FuelSupply(this->FuelSupNum).QskinLoss = 0.0;

            this->AirSup.TairIntoFCPM = 0.0;
            this->AirSup.PairCompEl = 0.0;
            this->AirSup.TairIntoBlower = 0.0;
            this->AirSup.QskinLoss = 0.0;
            this->AirSup.QintakeRecovery = 0.0;
            this->FCPM.NumCycles = 0;
            this->FCPM.Pel = 0.0;
            this->FCPM.PelLastTimeStep = 0.0;
            this->FCPM.Eel = 0.0;
            this->FCPM.PelancillariesAC = 0.0;
            this->FCPM.NdotFuel = 0.0;
            this->FCPM.TotFuelInEnthalphy = 0.0;
            this->FCPM.NdotProdGas = 0.0;
            this->FCPM.TprodGasLeavingFCPM = 0.0;
            this->FCPM.TotProdGasEnthalphy = 0.0;
            this->FCPM.NdotAir = 0.0;
            this->FCPM.TotAirInEnthalphy = 0.0;
            this->FCPM.NdotLiqwater = 0.0;
            this->FCPM.TwaterInlet = 0.0;
            this->FCPM.WaterInEnthalpy = 0.0;
            this->FCPM.TprodGasLeavingFCPM = 200.0;
            this->FCPM.FractionalDayofLastStartUp = 0.0;
            this->FCPM.FractionalDayofLastShutDown = 0.0;
            this->FCPM.HasBeenOn = true;
            this->FCPM.DuringShutDown = false;
            this->FCPM.DuringStartUp = false;
            this->WaterSup.TwaterIntoCompress = 0.0;
            this->WaterSup.TwaterIntoFCPM = 0.0;
            this->WaterSup.PwaterCompEl = 0.0;
            this->WaterSup.QskinLoss = 0.0;
            this->AuxilHeat.TauxMix = 0.0;
            this->AuxilHeat.NdotAuxMix = 0.0;
            this->AuxilHeat.QskinLoss = 0.0;
            this->AuxilHeat.QairIntake = 0.0;
            this->ExhaustHX.NdotHXleaving = 0.0;
            this->ExhaustHX.WaterOutletTemp = 0.0;
            this->ExhaustHX.WaterOutletEnthalpy = 0.0;
            this->ElecStorage.LastTimeStepStateOfCharge = this->ElecStorage.StartingEnergyStored;
            this->ElecStorage.ThisTimeStepStateOfCharge = this->ElecStorage.StartingEnergyStored;
            this->ElecStorage.PelNeedFromStorage = 0.0;
            this->ElecStorage.IdesiredDischargeCurrent = 0.0;
            this->ElecStorage.PelFromStorage = 0.0;
            this->ElecStorage.IfromStorage = 0.0;
            this->ElecStorage.PelIntoStorage = 0.0;
            this->ElecStorage.QairIntake = 0.0;

            this->Inverter.PCUlosses = 0.0;
            this->Inverter.QairIntake = 0.0;

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGenerators::InitHRTemp,
                                                           DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

            this->ExhaustHX.WaterMassFlowRateDesign = this->ExhaustHX.WaterVolumeFlowMax * rho;
            this->ExhaustHX.WaterMassFlowRate = this->ExhaustHX.WaterMassFlowRateDesign;
            DataLoopNode::Node(this->ExhaustHX.WaterInNode).Temp = DataGenerators::InitHRTemp;
            DataLoopNode::Node(this->ExhaustHX.WaterOutNode).Temp = DataGenerators::InitHRTemp;

            PlantUtilities::InitComponentNodes(0.0,
                                               this->ExhaustHX.WaterMassFlowRateDesign,
                                               this->ExhaustHX.WaterInNode,
                                               this->ExhaustHX.WaterOutNode,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            this->MyEnvrnFlag_Init = false;
            this->MyWarmupFlag_Init = true;
        } // end environmental inits

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag_Init = true;
        }

        if (this->MyWarmupFlag_Init && (!DataGlobals::WarmupFlag)) {
            // need to reset initial state of charge at beginning of environment but after warm up is complete
            this->ElecStorage.LastTimeStepStateOfCharge = this->ElecStorage.StartingEnergyStored;
            this->ElecStorage.ThisTimeStepStateOfCharge = this->ElecStorage.StartingEnergyStored;
            this->MyWarmupFlag_Init = false;
        }

        // using and elapsed time method rather than FirstHVACIteration here
        Real64 timeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
        if (this->TimeElapsed != timeElapsed) {

            this->ElecStorage.LastTimeStepStateOfCharge = this->ElecStorage.ThisTimeStepStateOfCharge;
            this->FCPM.PelLastTimeStep = this->FCPM.Pel;

            // intialize flow rate in water loop, this is "requesting" flow
            Real64 mdot = this->ExhaustHX.WaterMassFlowRateDesign;

            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->ExhaustHX.WaterInNode,
                                                 this->ExhaustHX.WaterOutNode,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);

            this->ExhaustHX.WaterMassFlowRate = mdot;
            this->ExhaustHX.WaterInletTemp = DataLoopNode::Node(this->ExhaustHX.WaterInNode).Temp;
            this->TimeElapsed = timeElapsed;
        } else {

            PlantUtilities::SetComponentFlowRate(this->ExhaustHX.WaterMassFlowRate,
                                                 this->ExhaustHX.WaterInNode,
                                                 this->ExhaustHX.WaterOutNode,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);

            this->ExhaustHX.WaterInletTemp = DataLoopNode::Node(this->ExhaustHX.WaterInNode).Temp;
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
            auto &thisFC = FuelCell(FCnum);
            Real64 TotalZoneHeatGain = thisFC.AirSup.QskinLoss + DataGenerators::FuelSupply(thisFC.FuelSupNum).QskinLoss + thisFC.WaterSup.QskinLoss +
                                       thisFC.AuxilHeat.QskinLoss + thisFC.FCPM.QdotSkin; // intake Blower losses to zone | fuel compressor losses to
                                                                                          // zone | water pump losses to zone | auxil burner losses to
                                                                                          // zone | power module (stack and reformer) losses to zone

            // now account for other subsystems that may or may not have air intake recovery
            {
                auto const SELECT_CASE_var(thisFC.AirSup.IntakeRecoveryMode);

                if (SELECT_CASE_var == DataGenerators::NoRecoveryOnAirIntake) { // then the heat has to go into zone
                    TotalZoneHeatGain += thisFC.AuxilHeat.QairIntake + thisFC.ElecStorage.QairIntake + thisFC.Inverter.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverAuxiliaryBurner) {
                    TotalZoneHeatGain += thisFC.ElecStorage.QairIntake + thisFC.Inverter.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverInverterBatt) {
                    TotalZoneHeatGain += thisFC.AuxilHeat.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverInverter) {
                    TotalZoneHeatGain += thisFC.AuxilHeat.QairIntake + thisFC.ElecStorage.QairIntake;
                } else if (SELECT_CASE_var == DataGenerators::RecoverBattery) {
                    TotalZoneHeatGain += thisFC.AuxilHeat.QairIntake + thisFC.Inverter.QairIntake;

                } else if (SELECT_CASE_var == DataGenerators::RecoverBurnInvertBatt) {
                    // do nothing
                }
            }

            thisFC.QconvZone = TotalZoneHeatGain * (1 - thisFC.FCPM.RadiativeFract);
            thisFC.Report.SkinLossConvect = thisFC.QconvZone;
            thisFC.QradZone = TotalZoneHeatGain * thisFC.FCPM.RadiativeFract;
            thisFC.Report.SkinLossRadiat = thisFC.QradZone;

        } // over number of Fuel cells
    }

    void FCDataStruct::CalcUpdateHeatRecovery(bool const EP_UNUSED(FirstHVACIteration))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update plant loop interactions, do any calcs needed

        // now update water outlet node Changing to Kg/s!

        PlantUtilities::SafeCopyPlantNode(this->ExhaustHX.WaterInNode, this->ExhaustHX.WaterOutNode);

        DataLoopNode::Node(this->ExhaustHX.WaterOutNode).Temp = this->ExhaustHX.WaterOutletTemp;
        DataLoopNode::Node(this->ExhaustHX.WaterOutNode).Enthalpy = this->ExhaustHX.WaterOutletEnthalpy;
    }

    void FCDataStruct::UpdateFuelCellGeneratorRecords()
    {

        this->Report.ACPowerGen = this->ACPowerGen;                                                          // electrical power produced [W]
        this->Report.ACEnergyGen = this->ACPowerGen * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // energy produced (J)
        this->Report.QdotExhaust = 0.0;        // reporting: exhaust gas heat recovered (W)
        this->Report.TotalHeatEnergyRec = 0.0; // reporting: total heat recovered (J)
        this->Report.ExhaustEnergyRec = 0.0;   // reporting: exhaust gas heat recovered (J)

        this->Report.HeatRecInletTemp = 0.0;  // reporting: Heat Recovery Loop Inlet Temperature (C)
        this->Report.HeatRecOutletTemp = 0.0; // reporting: Heat Recovery Loop Outlet Temperature (C)
        this->Report.HeatRecMdot = 0.0;       // reporting: Heat Recovery Loop Mass flow rate (kg/s)

        this->Report.ElectEfficiency = 0.0;
        this->Report.ThermalEfficiency = 0.0;
        this->Report.OverallEfficiency = 0.0;
        this->Report.ExergyEfficiency = 0.0;

        this->Report.TairInlet = this->AirSup.TairIntoBlower;          // State point 1
        this->Report.TairIntoFCPM = this->AirSup.TairIntoFCPM;         // State point 4
        this->Report.NdotAir = this->FCPM.NdotAir;                     // air flow in kmol/sec
        this->Report.TotAirInEnthalphy = this->FCPM.TotAirInEnthalphy; // State point 4
        this->Report.BlowerPower = this->AirSup.PairCompEl;            // electrical power used by air supply blower
        this->Report.BlowerEnergy = this->AirSup.PairCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // electrical energy
        this->Report.BlowerSkinLoss = this->AirSup.QskinLoss;                                                        // heat rate of losses by blower

        this->Report.TfuelInlet = DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoCompress; // State point 2
        this->Report.TfuelIntoFCPM = DataGenerators::FuelSupply(this->FuelSupNum).TfuelIntoFCPM;  // TEmperature state point 5 [C]
        this->Report.NdotFuel = this->FCPM.NdotFuel;                                              // fuel flow in kmol/sec
        this->Report.TotFuelInEnthalpy = this->FCPM.TotFuelInEnthalphy;                           // enthalpy at state point 5 [W]
        this->Report.FuelCompressPower = DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl;
        // electrical power used by fuel supply compressor [W]
        this->Report.FuelCompressEnergy =
            DataGenerators::FuelSupply(this->FuelSupNum).PfuelCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // elect energy
        this->Report.FuelCompressSkinLoss = DataGenerators::FuelSupply(this->FuelSupNum).QskinLoss;
        // heat rate of losses.by fuel supply compressor [W]
        this->Report.FuelEnergyLHV = this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).LHV * 1000000.0 *
                                     DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // reporting: Fuel Energy used (J)
        this->Report.FuelEnergyUseRateLHV =
            this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).LHV * 1000000.0; // reporting: Fuel Energy used (W)
        this->Report.FuelEnergyHHV = this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).HHV *
                                     DataGenerators::FuelSupply(this->FuelSupNum).KmolPerSecToKgPerSec * DataHVACGlobals::TimeStepSys *
                                     DataGlobals::SecInHour;

        this->Report.FuelEnergyUseRateHHV = this->FCPM.NdotFuel * DataGenerators::FuelSupply(this->FuelSupNum).HHV *
                                            DataGenerators::FuelSupply(this->FuelSupNum).KmolPerSecToKgPerSec;

        this->Report.FuelRateMdot = 0.0; // (Kg/s)

        this->Report.TwaterInlet = this->WaterSup.TwaterIntoCompress;
        this->Report.TwaterIntoFCPM = this->WaterSup.TwaterIntoFCPM;
        this->Report.NdotWater = this->FCPM.NdotLiqwater; // water flow in kmol/sec (reformer water)
        this->Report.WaterPumpPower = this->WaterSup.PwaterCompEl;
        this->Report.WaterPumpEnergy = this->WaterSup.PwaterCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // electrical energy
        this->Report.WaterIntoFCPMEnthalpy = this->FCPM.WaterInEnthalpy;

        this->Report.TprodGas = this->FCPM.TprodGasLeavingFCPM;      // temperature at State point 7
        this->Report.EnthalProdGas = this->FCPM.TotProdGasEnthalphy; // enthalpy at State point 7
        this->Report.NdotProdGas = this->FCPM.NdotProdGas;           // flow rate at point 7 [kmol/sec]
        this->Report.NdotProdAr = this->FCPM.ConstitMolalFract(5) * this->FCPM.NdotProdGas;
        this->Report.NdotProdCO2 = this->FCPM.ConstitMolalFract(1) * this->FCPM.NdotProdGas;
        this->Report.NdotProdH2O = this->FCPM.ConstitMolalFract(4) * this->FCPM.NdotProdGas;
        this->Report.NdotProdN2 = this->FCPM.ConstitMolalFract(2) * this->FCPM.NdotProdGas;
        this->Report.NdotProdO2 = this->FCPM.ConstitMolalFract(3) * this->FCPM.NdotProdGas;

        this->Report.qHX = this->ExhaustHX.qHX;
        this->Report.HXenergy = this->ExhaustHX.qHX * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Report.THXexh = this->ExhaustHX.THXexh;
        this->Report.WaterVaporFractExh = this->ExhaustHX.WaterVaporFractExh;
        this->Report.CondensateRate = this->ExhaustHX.CondensateRate;

        this->Report.SeqSubstIterations = this->FCPM.SeqSubstitIter;     // number of iterations in FuelCell loop
        this->Report.RegulaFalsiIterations = this->FCPM.RegulaFalsiIter; // number of iterations in Tproduct gas solving

        this->Report.ACancillariesPower = this->FCPM.PelancillariesAC;
        this->Report.ACancillariesEnergy = this->FCPM.PelancillariesAC * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        this->Report.PCUlosses = this->Inverter.PCUlosses; // inverter losses
        this->Report.DCPowerGen = this->FCPM.Pel;          // DC power out of FCPM.
        this->Report.DCPowerEff = this->FCPM.Eel;          // FCPM efficiency Eel.
        this->Report.ElectEnergyinStorage = this->ElecStorage.ThisTimeStepStateOfCharge;
        this->Report.StoredPower = this->ElecStorage.PelIntoStorage;
        this->Report.StoredEnergy = this->ElecStorage.PelIntoStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Report.DrawnPower = this->ElecStorage.PelFromStorage;
        this->Report.DrawnEnergy = this->ElecStorage.PelFromStorage * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        this->Report.SkinLossPower = this->QconvZone + this->QradZone;
        this->Report.SkinLossEnergy = (this->QconvZone + this->QradZone) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Report.SkinLossConvect = this->QconvZone;
        this->Report.SkinLossRadiat = this->QradZone;
    }

} // namespace FuelCellElectricGenerator

} // namespace EnergyPlus
