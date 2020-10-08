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
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SolarCollectors.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace SolarCollectors {

    // MODULE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   December 2003
    //       MODIFIED       B. Nigusse, FSEC/UCF, March 2012, added ICS Collector
    //       RE-ENGINEERED  Brent Griffith, for plant upgrade, general fluid props

    // PURPOSE OF THIS MODULE:
    // Simulates solar collectors as a component on the plant loop.  Currently only flat-plate collectors (glazed and
    // unglazed) are implemented.

    // METHODOLOGY EMPLOYED:
    // Solar collectors are called as non-zone equipment on the demand side of the plant loop.  The collector object
    // must be connected to a WATER HEATER object on the supply side of the plant loop.  Water is assumed to be
    // the heat transfer fluid.

    static std::string const fluidNameWater("WATER");

    Array1D_bool CheckEquipName;

    // MODULE VARIABLE DECLARATIONS:
    int NumOfCollectors(0);
    int NumOfParameters(0);
    bool GetInputFlag(true);

    Array1D<ParametersData> Parameters;
    Array1D<CollectorData> Collector;
    std::unordered_map<std::string, std::string> UniqueParametersNames;
    std::unordered_map<std::string, std::string> UniqueCollectorNames;

    void clear_state()
    {
        NumOfCollectors = 0;
        NumOfParameters = 0;
        GetInputFlag = true;
        Parameters.deallocate();
        Collector.deallocate();
        UniqueCollectorNames.clear();
        UniqueParametersNames.clear();
    }

    PlantComponent *CollectorData::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data
        if (GetInputFlag) {
            GetSolarCollectorInput(state);
            GetInputFlag = false;
        }
        // Now look for this particular object
        for (auto &thisSC : Collector) {
            if (thisSC.Name == objectName) {
                return &thisSC;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalSolarCollectorFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void GetSolarCollectorInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   December 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the solar collector input from the input file and sets up the parameters and collector objects.

        bool ErrorsFound(false);       // Set to true if errors in input, fatal at end of routine
        int IOStatus;                         // Used in GetObjectItem
        int NumAlphas;                        // Number of Alphas for each GetObjectItem call
        int NumNumbers;                       // Number of Numbers for each GetObjectItem call
        std::string CurrentModuleObject;      // for ease in renaming.
        std::string CurrentModuleParamObject; // for ease in renaming.

        int NumFields;  // Total number of fields in object
        int MaxAlphas;  // Maximum number of alpha fields in all objects
        int MaxNumbers; // Maximum number of numeric fields in all objects

        Array1D<Real64> Numbers;       // Numeric data
        Array1D_string Alphas;         // Alpha data
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.

        MaxNumbers = 0;
        MaxAlphas = 0;

        CurrentModuleParamObject = "SolarCollectorPerformance:FlatPlate";
        int NumOfFlatPlateParam = inputProcessor->getNumObjectsFound(CurrentModuleParamObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleParamObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "SolarCollector:FlatPlate:Water";
        int NumFlatPlateUnits = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleParamObject = "SolarCollectorPerformance:IntegralCollectorStorage";
        int NumOfICSParam = inputProcessor->getNumObjectsFound(CurrentModuleParamObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleParamObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
        int NumOfICSUnits = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        Numbers.dimension(MaxNumbers, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNumbers);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNumbers, true);

        NumOfCollectors = NumFlatPlateUnits + NumOfICSUnits;
        NumOfParameters = NumOfFlatPlateParam + NumOfICSParam;

        if (NumOfParameters > 0) {
            Parameters.allocate(NumOfParameters);

            CurrentModuleParamObject = "SolarCollectorPerformance:FlatPlate";

            for (int FlatPlateParamNum = 1; FlatPlateParamNum <= NumOfFlatPlateParam; ++FlatPlateParamNum) {

                int ParametersNum = FlatPlateParamNum;
                inputProcessor->getObjectItem(state,
                                              CurrentModuleParamObject,
                                              ParametersNum,
                                              DataIPShortCuts::cAlphaArgs,
                                              NumAlphas,
                                              DataIPShortCuts::rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              DataIPShortCuts::lNumericFieldBlanks,
                                              _,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);

                // Collector module parameters name
                GlobalNames::VerifyUniqueInterObjectName(
                    UniqueParametersNames, DataIPShortCuts::cAlphaArgs(1), CurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);
                Parameters(ParametersNum).Name = DataIPShortCuts::cAlphaArgs(1);

                // NOTE:  This values serves mainly as a reference.  The area of the associated surface object is used in all calculations.
                Parameters(ParametersNum).Area = DataIPShortCuts::rNumericArgs(1);

                {
                    auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(2));
                    if (SELECT_CASE_var == "WATER") {
                        Parameters(ParametersNum).TestFluid = FluidEnum::WATER;
                        // CASE('AIR')
                        //  Parameters(ParametersNum)%TestFluid = AIR
                    } else {
                        ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  " + DataIPShortCuts::cAlphaArgs(2) +
                                        " is an unsupported Test Fluid for " + DataIPShortCuts::cAlphaFieldNames(2));
                        ErrorsFound = true;
                    }
                }

                if (DataIPShortCuts::rNumericArgs(2) > 0.0) {
                    Parameters(ParametersNum).TestMassFlowRate = DataIPShortCuts::rNumericArgs(2) * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
                } else {
                    ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                    ":  flow rate must be greater than zero for " + DataIPShortCuts::cNumericFieldNames(2));
                    ErrorsFound = true;
                }

                {
                    auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(3));
                    if (SELECT_CASE_var == "INLET") {
                        Parameters(ParametersNum).TestType = TestTypeEnum::INLET;
                    } else if (SELECT_CASE_var == "AVERAGE") {
                        Parameters(ParametersNum).TestType = TestTypeEnum::AVERAGE;
                    } else if (SELECT_CASE_var == "OUTLET") {
                        Parameters(ParametersNum).TestType = TestTypeEnum::OUTLET;
                    } else {
                        ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  " + DataIPShortCuts::cAlphaArgs(3) +
                                        " is  not supported for " + DataIPShortCuts::cAlphaFieldNames(3));
                        ErrorsFound = true;
                    }
                }

                // Efficiency equation coefficients
                Parameters(ParametersNum).eff0 = DataIPShortCuts::rNumericArgs(3);
                Parameters(ParametersNum).eff1 = DataIPShortCuts::rNumericArgs(4);

                if (NumNumbers > 4) {
                    Parameters(ParametersNum).eff2 = DataIPShortCuts::rNumericArgs(5);
                } else {
                    Parameters(ParametersNum).eff2 = 0.0;
                }

                // Incident angle modifier coefficients
                if (NumNumbers > 5) {
                    Parameters(ParametersNum).iam1 = DataIPShortCuts::rNumericArgs(6);
                } else {
                    Parameters(ParametersNum).iam1 = 0.0;
                }

                if (NumNumbers > 6) {
                    Parameters(FlatPlateParamNum).iam2 = DataIPShortCuts::rNumericArgs(7);
                } else {
                    Parameters(ParametersNum).iam2 = 0.0;
                }
            } // ParametersNum

            if (ErrorsFound) ShowFatalError("Errors in " + CurrentModuleParamObject + " input.");
        }

        if (NumOfCollectors > 0) {
            Collector.allocate(NumOfCollectors);

            CurrentModuleObject = "SolarCollector:FlatPlate:Water";

            for (int FlatPlateUnitsNum = 1; FlatPlateUnitsNum <= NumFlatPlateUnits; ++FlatPlateUnitsNum) {

                int CollectorNum = FlatPlateUnitsNum;

                inputProcessor->getObjectItem(
                    state, CurrentModuleObject, CollectorNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNumbers, IOStatus);

                // Collector name
                GlobalNames::VerifyUniqueInterObjectName(UniqueCollectorNames, DataIPShortCuts::cAlphaArgs(1), CurrentModuleObject, ErrorsFound);
                Collector(CollectorNum).Name = DataIPShortCuts::cAlphaArgs(1);
                Collector(CollectorNum).TypeNum = DataPlant::TypeOf_SolarCollectorFlatPlate; // parameter assigned in DataPlant !DSU

                // Get parameters object
                int ParametersNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(2), Parameters);

                if (ParametersNum == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": " + CurrentModuleParamObject +
                                    " object called " + DataIPShortCuts::cAlphaArgs(2) + " not found.");
                    ErrorsFound = true;
                } else {
                    Collector(CollectorNum).Parameters = ParametersNum;
                }

                // Get surface object
                int SurfNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(3), DataSurfaces::Surface);

                if (SurfNum == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Surface " + DataIPShortCuts::cAlphaArgs(3) +
                                    " not found.");
                    ErrorsFound = true;
                    continue; // avoid hard crash
                } else {

                    if (!DataSurfaces::Surface(SurfNum).ExtSolar) {
                        ShowWarningError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Surface " +
                                         DataIPShortCuts::cAlphaArgs(3) + " is not exposed to exterior radiation.");
                    }

                    // check surface orientation, warn if upside down
                    if ((DataSurfaces::Surface(SurfNum).Tilt < -95.0) || (DataSurfaces::Surface(SurfNum).Tilt > 95.0)) {
                        ShowWarningError("Suspected input problem with " + DataIPShortCuts::cAlphaFieldNames(3) + " = " +
                                         DataIPShortCuts::cAlphaArgs(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                        ShowContinueError("Surface used for solar collector faces down");
                        ShowContinueError("Surface tilt angle (degrees from ground outward normal) = " +
                                          General::RoundSigDigits(DataSurfaces::Surface(SurfNum).Tilt, 2));
                    }

                    // Check to make sure other solar collectors are not using the same surface
                    // NOTE:  Must search over all solar collector types
                    for (int CollectorNum2 = 1; CollectorNum2 <= NumFlatPlateUnits; ++CollectorNum2) {
                        if (Collector(CollectorNum2).Surface == SurfNum) {
                            ShowSevereError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Surface " +
                                            DataIPShortCuts::cAlphaArgs(3) + " is referenced by more than one " + CurrentModuleObject);
                            ErrorsFound = true;
                            break;
                        }
                    } // CollectorNum2

                    Collector(CollectorNum).Surface = SurfNum;
                }

                // Give warning if surface area and gross area do not match within tolerance
                if (SurfNum > 0 && ParametersNum > 0 && Parameters(ParametersNum).Area > 0.0 &&
                    std::abs(Parameters(ParametersNum).Area - DataSurfaces::Surface(SurfNum).Area) / DataSurfaces::Surface(SurfNum).Area > 0.01) {

                    ShowWarningError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                     ":  Gross Area of solar collector parameters and surface object differ by more than 1%.");
                    ShowContinueError("Area of surface object will be used in all calculations.");
                }

                Collector(CollectorNum).InletNode = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(4),
                                                                                        ErrorsFound,
                                                                                        CurrentModuleObject,
                                                                                        DataIPShortCuts::cAlphaArgs(1),
                                                                                        DataLoopNode::NodeType_Water,
                                                                                        DataLoopNode::NodeConnectionType_Inlet,
                                                                                        1,
                                                                                        DataLoopNode::ObjectIsNotParent);
                Collector(CollectorNum).OutletNode = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(5),
                                                                                         ErrorsFound,
                                                                                         CurrentModuleObject,
                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                         DataLoopNode::NodeType_Water,
                                                                                         DataLoopNode::NodeConnectionType_Outlet,
                                                                                         1,
                                                                                         DataLoopNode::ObjectIsNotParent);

                if (NumNumbers > 0) {
                    Collector(CollectorNum).VolFlowRateMax =
                        DataIPShortCuts::rNumericArgs(1); // Max volumetric flow rate used for plant sizing calculation
                } else {
                    Collector(CollectorNum).VolFlowRateMax = 0.0;       // Max vol flow rate is not specified; no flow for plant sizing calculation
                    Collector(CollectorNum).MassFlowRateMax = 999999.9; // But...set a very high value so that it demands as much as possible
                }

                BranchNodeConnections::TestCompSet(CurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(4),
                                                   DataIPShortCuts::cAlphaArgs(5),
                                                   "Water Nodes");

            } // FlatPlateUnitsNum

            // Get data for ICS collector
            CurrentModuleParamObject = "SolarCollectorPerformance:IntegralCollectorStorage";

            for (int ICSParamNum = 1; ICSParamNum <= NumOfICSParam; ++ICSParamNum) {

                int ParametersNum = ICSParamNum + NumOfFlatPlateParam;

                inputProcessor->getObjectItem(state,
                                              CurrentModuleParamObject,
                                              ICSParamNum,
                                              DataIPShortCuts::cAlphaArgs,
                                              NumAlphas,
                                              DataIPShortCuts::rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              DataIPShortCuts::lNumericFieldBlanks,
                                              _,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);

                // Collector module parameters name
                GlobalNames::VerifyUniqueInterObjectName(
                    UniqueParametersNames, DataIPShortCuts::cAlphaArgs(1), CurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);
                Parameters(ParametersNum).Name = DataIPShortCuts::cAlphaArgs(1);
                // NOTE:  currently the only available choice is RectangularTank.  In the future progressive tube type will be
                //        added
                if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "RectangularTank")) {
                    Parameters(ParametersNum).ICSType_Num = TankTypeEnum::ICSRectangularTank;
                } else {
                    ShowSevereError(DataIPShortCuts::cAlphaFieldNames(2) + " not found=" + DataIPShortCuts::cAlphaArgs(2) + " in " +
                                    CurrentModuleParamObject + " =" + Parameters(ParametersNum).Name);
                    ErrorsFound = true;
                }
                // NOTE:  This collector gross area is used in all the calculations.
                Parameters(ParametersNum).Area = DataIPShortCuts::rNumericArgs(1);
                if (DataIPShortCuts::rNumericArgs(1) <= 0.0) {
                    ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Illegal " + DataIPShortCuts::cNumericFieldNames(1) + " = " +
                                      General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                    ShowContinueError(" Collector gross area must be always gretaer than zero.");
                    ErrorsFound = true;
                }
                Parameters(ParametersNum).Volume = DataIPShortCuts::rNumericArgs(2);
                if (DataIPShortCuts::rNumericArgs(2) <= 0.0) {
                    ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Illegal " + DataIPShortCuts::cNumericFieldNames(2) + " = " +
                                      General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                    ShowContinueError(" Collector water volume must be always gretaer than zero.");
                    ErrorsFound = true;
                }
                // Note: this value is used to calculate the heat loss through the bottom and side of the collector
                Parameters(ParametersNum).ULossBottom = DataIPShortCuts::rNumericArgs(3);
                Parameters(ParametersNum).ULossSide = DataIPShortCuts::rNumericArgs(4);
                Parameters(ParametersNum).AspectRatio = DataIPShortCuts::rNumericArgs(5);
                Parameters(ParametersNum).SideHeight = DataIPShortCuts::rNumericArgs(6);
                Parameters(ParametersNum).ThermalMass = DataIPShortCuts::rNumericArgs(7);
                Parameters(ParametersNum).NumOfCovers = DataIPShortCuts::rNumericArgs(8);
                Parameters(ParametersNum).CoverSpacing = DataIPShortCuts::rNumericArgs(9);

                if (Parameters(ParametersNum).NumOfCovers == 2) {
                    // Outer cover refractive index
                    Parameters(ParametersNum).RefractiveIndex(1) = DataIPShortCuts::rNumericArgs(10);
                    // Outer cover extinction coefficient times thickness of the cover
                    Parameters(ParametersNum).ExtCoefTimesThickness(1) = DataIPShortCuts::rNumericArgs(11);
                    // Outer cover Emissivity
                    Parameters(ParametersNum).EmissOfCover(1) = DataIPShortCuts::rNumericArgs(12);

                    if (!DataIPShortCuts::lNumericFieldBlanks(13) || !DataIPShortCuts::lNumericFieldBlanks(14) ||
                        !DataIPShortCuts::lNumericFieldBlanks(15)) {
                        Parameters(ParametersNum).RefractiveIndex(2) = DataIPShortCuts::rNumericArgs(13);
                        Parameters(ParametersNum).ExtCoefTimesThickness(2) = DataIPShortCuts::rNumericArgs(14);
                        Parameters(ParametersNum).EmissOfCover(2) = DataIPShortCuts::rNumericArgs(15);
                    } else {
                        ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                        ShowContinueError("Illegal input for one of the three inputs of the inner cover optical properties");
                        ErrorsFound = true;
                    }
                } else if (Parameters(ParametersNum).NumOfCovers == 1) {
                    // Outer cover refractive index
                    Parameters(ParametersNum).RefractiveIndex(1) = DataIPShortCuts::rNumericArgs(10);
                    // Outer cover extinction coefficient times thickness of the cover
                    Parameters(ParametersNum).ExtCoefTimesThickness(1) = DataIPShortCuts::rNumericArgs(11);
                    // Outer cover emissivity
                    Parameters(ParametersNum).EmissOfCover(1) = DataIPShortCuts::rNumericArgs(12);
                } else {
                    ShowSevereError(CurrentModuleParamObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("Illegal " + DataIPShortCuts::cNumericFieldNames(8) + " = " +
                                      General::RoundSigDigits(DataIPShortCuts::rNumericArgs(8), 2));
                    ErrorsFound = true;
                }
                // Solar absorptance of the absorber plate
                Parameters(ParametersNum).AbsorOfAbsPlate = DataIPShortCuts::rNumericArgs(16);
                // thermal emmissivity of the absorber plate
                Parameters(ParametersNum).EmissOfAbsPlate = DataIPShortCuts::rNumericArgs(17);

            } // end of ParametersNum

            if (ErrorsFound) ShowFatalError("Errors in " + CurrentModuleParamObject + " input.");

            CurrentModuleObject = "SolarCollector:IntegralCollectorStorage";

            for (int ICSUnitsNum = 1; ICSUnitsNum <= NumOfICSUnits; ++ICSUnitsNum) {

                int CollectorNum = ICSUnitsNum + NumFlatPlateUnits;

                inputProcessor->getObjectItem(state,
                                              CurrentModuleObject,
                                              ICSUnitsNum,
                                              DataIPShortCuts::cAlphaArgs,
                                              NumAlphas,
                                              DataIPShortCuts::rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              DataIPShortCuts::lNumericFieldBlanks,
                                              lAlphaBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);

                // Collector name
                GlobalNames::VerifyUniqueInterObjectName(
                    UniqueCollectorNames, DataIPShortCuts::cAlphaArgs(1), CurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);
                Collector(CollectorNum).Name = DataIPShortCuts::cAlphaArgs(1);
                Collector(CollectorNum).TypeNum = DataPlant::TypeOf_SolarCollectorICS; // parameter assigned in DataPlant

                Collector(CollectorNum).InitICS = true;

                // Get parameters object
                int ParametersNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(2), Parameters);

                if (ParametersNum == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": " + CurrentModuleParamObject +
                                    " object called " + DataIPShortCuts::cAlphaArgs(2) + " not found.");
                    ErrorsFound = true;
                } else {
                    Collector(CollectorNum).Parameters = ParametersNum;
                }

                if (ParametersNum > 0) {
                    // Calculate constant collector parameters only once
                    Real64 Perimeter = 2.0 * std::sqrt(Parameters(ParametersNum).Area) *
                                       (std::sqrt(Parameters(ParametersNum).AspectRatio) + 1.0 / std::sqrt(Parameters(ParametersNum).AspectRatio));
                    Collector(CollectorNum).Length = std::sqrt(Parameters(ParametersNum).Area / Parameters(ParametersNum).AspectRatio);

                    // calculate the collector side heat transfer area and loss coefficient
                    Collector(CollectorNum).ICSType_Num = Parameters(ParametersNum).ICSType_Num;
                    Collector(CollectorNum).Area = Parameters(ParametersNum).Area;
                    Collector(CollectorNum).Volume = Parameters(ParametersNum).Volume;
                    Collector(CollectorNum).SideArea = Perimeter * Parameters(ParametersNum).SideHeight;
                    Collector(CollectorNum).AreaRatio = Collector(CollectorNum).SideArea / Collector(CollectorNum).Area;
                }
                // Get surface object
                int SurfNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(3), DataSurfaces::Surface);

                if (SurfNum == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Surface " + DataIPShortCuts::cAlphaArgs(3) +
                                    " not found.");
                    ErrorsFound = true;
                    continue; // avoid hard crash
                } else {

                    if (!DataSurfaces::Surface(SurfNum).ExtSolar) {
                        ShowWarningError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Surface " +
                                         DataIPShortCuts::cAlphaArgs(3) + " is not exposed to exterior radiation.");
                    }

                    // check surface orientation, warn if upside down
                    if ((DataSurfaces::Surface(SurfNum).Tilt < -95.0) || (DataSurfaces::Surface(SurfNum).Tilt > 95.0)) {
                        ShowWarningError("Suspected input problem with " + DataIPShortCuts::cAlphaFieldNames(3) + " = " +
                                         DataIPShortCuts::cAlphaArgs(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                        ShowContinueError("Surface used for solar collector faces down");
                        ShowContinueError("Surface tilt angle (degrees from ground outward normal) = " +
                                          General::RoundSigDigits(DataSurfaces::Surface(SurfNum).Tilt, 2));
                    }

                    // Check to make sure other solar collectors are not using the same surface
                    // NOTE:  Must search over all solar collector types
                    for (int CollectorNum2 = 1; CollectorNum2 <= NumOfCollectors; ++CollectorNum2) {
                        if (Collector(CollectorNum2).Surface == SurfNum) {
                            ShowSevereError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Surface " +
                                            DataIPShortCuts::cAlphaArgs(3) + " is referenced by more than one " + CurrentModuleObject);
                            ErrorsFound = true;
                            break;
                        }
                    } // ICSNum2

                    Collector(CollectorNum).Surface = SurfNum;
                }

                // Give warning if surface area and gross area do not match within tolerance
                if (SurfNum > 0 && ParametersNum > 0 && Parameters(ParametersNum).Area > 0.0 &&
                    std::abs(Parameters(ParametersNum).Area - DataSurfaces::Surface(SurfNum).Area) / DataSurfaces::Surface(SurfNum).Area > 0.01) {

                    ShowWarningError(CurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": ");
                    ShowContinueError("Gross area of solar collector parameters and surface object differ by more than 1%.");
                    ShowContinueError("Gross collector area is always used in the calculation.  Modify the surface ");
                    ShowContinueError("coordinates to match its area with collector gross area. Otherwise, the underlying ");
                    ShowContinueError("surface is assumed to be fully shaded when it is not.");
                }

                Collector(CollectorNum).BCType = DataIPShortCuts::cAlphaArgs(4);
                if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(4), "AmbientAir")) {
                    Collector(CollectorNum).OSCMName = "";
                } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(4), "OtherSideConditionsModel")) {
                    Collector(CollectorNum).OSCMName = DataIPShortCuts::cAlphaArgs(5);
                    Collector(CollectorNum).OSCM_ON = true;
                    int Found = UtilityRoutines::FindItemInList(Collector(CollectorNum).OSCMName, DataSurfaces::OSCM);
                    if (Found == 0) {
                        ShowSevereError(DataIPShortCuts::cAlphaFieldNames(5) + " not found=" + Collector(CollectorNum).OSCMName + " in " +
                                        CurrentModuleObject + " =" + Collector(CollectorNum).Name);
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(DataIPShortCuts::cAlphaFieldNames(5) + " not found=" + Collector(CollectorNum).BCType + " in " +
                                    CurrentModuleObject + " =" + Collector(CollectorNum).Name);
                    ErrorsFound = true;
                }

                if (Collector(CollectorNum).OSCM_ON) {
                    // get index of ventilated cavity object
                    int VentCavIndex = 0;
                    SolarCollectors::CollectorData::GetExtVentedCavityIndex(SurfNum, VentCavIndex);
                    Collector(CollectorNum).VentCavIndex = VentCavIndex;
                }

                Collector(CollectorNum).InletNode = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(6),
                                                                                        ErrorsFound,
                                                                                        CurrentModuleObject,
                                                                                        DataIPShortCuts::cAlphaArgs(1),
                                                                                        DataLoopNode::NodeType_Water,
                                                                                        DataLoopNode::NodeConnectionType_Inlet,
                                                                                        1,
                                                                                        DataLoopNode::ObjectIsNotParent);
                Collector(CollectorNum).OutletNode = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(7),
                                                                                         ErrorsFound,
                                                                                         CurrentModuleObject,
                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                         DataLoopNode::NodeType_Water,
                                                                                         DataLoopNode::NodeConnectionType_Outlet,
                                                                                         1,
                                                                                         DataLoopNode::ObjectIsNotParent);

                if (NumNumbers > 0) {
                    Collector(CollectorNum).VolFlowRateMax =
                        DataIPShortCuts::rNumericArgs(1); // Max volumetric flow rate used for plant sizing calculation
                } else {
                    Collector(CollectorNum).VolFlowRateMax = 0.0;       // Max vol flow rate is not specified; no flow for plant sizing calculation
                    Collector(CollectorNum).MassFlowRateMax = 999999.9; // But...set a very high value so that it demands as much as possible
                }

                BranchNodeConnections::TestCompSet(CurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(6),
                                                   DataIPShortCuts::cAlphaArgs(7),
                                                   "Water Nodes");

            } // ICSNum

            if (ErrorsFound) ShowFatalError("Errors in " + CurrentModuleObject + " input.");

            if (NumOfCollectors > 0) {
                CheckEquipName.dimension(NumOfCollectors, true);
            }
        }
    }

    void CollectorData::setupOutputVars(EnergyPlusData &state)
    {
        if (this->TypeNum == DataPlant::TypeOf_SolarCollectorFlatPlate) {
            // Setup report variables
            SetupOutputVariable(state,
                "Solar Collector Incident Angle Modifier", OutputProcessor::Unit::None, this->IncidentAngleModifier, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Efficiency", OutputProcessor::Unit::None, this->Efficiency, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Heat Transfer Rate", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Heat Gain Rate", OutputProcessor::Unit::W, this->HeatGain, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Heat Loss Rate", OutputProcessor::Unit::W, this->HeatLoss, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarWater",
                                "HeatProduced",
                                _,
                                "Plant");
        } else if (this->TypeNum == DataPlant::TypeOf_SolarCollectorICS) {

            SetupOutputVariable(state,
                "Solar Collector Transmittance Absorptance Product", OutputProcessor::Unit::None, this->TauAlpha, "System", "Average", this->Name);

            SetupOutputVariable(state,
                "Solar Collector Overall Top Heat Loss Coefficient", OutputProcessor::Unit::W_m2C, this->UTopLoss, "System", "Average", this->Name);

            SetupOutputVariable(state,
                "Solar Collector Absorber Plate Temperature", OutputProcessor::Unit::C, this->TempOfAbsPlate, "System", "Average", this->Name);

            SetupOutputVariable(state,
                "Solar Collector Storage Water Temperature", OutputProcessor::Unit::C, this->TempOfWater, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Thermal Efficiency", OutputProcessor::Unit::None, this->Efficiency, "System", "Average", this->Name);

            SetupOutputVariable(state,
                "Solar Collector Storage Heat Transfer Rate", OutputProcessor::Unit::W, this->StoredHeatRate, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Storage Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                this->StoredHeatEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarWater",
                                "HeatProduced",
                                _,
                                "Plant");

            SetupOutputVariable(state,
                "Solar Collector Skin Heat Transfer Rate", OutputProcessor::Unit::W, this->SkinHeatLossRate, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Skin Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                this->CollHeatLossEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarWater",
                                "HeatProduced",
                                _,
                                "Plant");

            SetupOutputVariable(state, "Solar Collector Heat Transfer Rate", OutputProcessor::Unit::W, this->HeatRate, "System", "Average", this->Name);

            SetupOutputVariable(state, "Solar Collector Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                this->HeatEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "SolarWater",
                                "HeatProduced",
                                _,
                                "Plant");
        }
    }

    void CollectorData::simulate(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation),
                                 bool const EP_UNUSED(FirstHVACIteration),
                                 Real64 &EP_UNUSED(CurLoad),
                                 bool const EP_UNUSED(RunFlag))
    {
        this->initialize(state);

        {
            auto const SELECT_CASE_var(this->TypeNum);
            // Select and CALL models based on collector type
            if (SELECT_CASE_var == DataPlant::TypeOf_SolarCollectorFlatPlate) {
                this->CalcSolarCollector(state);
            } else if (SELECT_CASE_var == DataPlant::TypeOf_SolarCollectorICS) {
                this->CalcICSSolarCollector(state);
            } else {
                assert(false); // LCOV_EXCL_LINE
            }
        }

        this->update(state);

        this->report();
    }

    void CollectorData::initialize(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initializes the solar collector object during the plant simulation.

        // METHODOLOGY EMPLOYED:
        // Inlet and outlet nodes are initialized.  The maximum collector flow rate is requested.

        static std::string const RoutineName("InitSolarCollector");
        Real64 const BigNumber(9999.9); // Component desired mass flow rate

        // Do the one time initializations
        if (this->MyOneTimeFlag) {
            this->setupOutputVars(state);
            this->MyOneTimeFlag = false;
        }

        if (this->SetLoopIndexFlag) {
            if (allocated(DataPlant::PlantLoop)) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(state,
                    this->Name, this->TypeNum, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum, errFlag, _, _, _, _, _);
                if (errFlag) {
                    ShowFatalError("InitSolarCollector: Program terminated due to previous condition(s).");
                }
                this->SetLoopIndexFlag = false;
            }
        }

        if (!DataGlobals::SysSizingCalc && this->InitSizing) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->InletNode, this->VolFlowRateMax);
            this->InitSizing = false;
        }

        if (DataGlobals::BeginEnvrnFlag && this->Init) {
            // Clear node initial conditions
            if (this->VolFlowRateMax > 0) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               DataPlant::PlantLoop(this->WLoopNum).FluidName,
                                                               DataGlobals::InitConvTemp,
                                                               DataPlant::PlantLoop(this->WLoopNum).FluidIndex,
                                                               RoutineName);

                this->MassFlowRateMax = this->VolFlowRateMax * rho;
            } else {
                this->MassFlowRateMax = BigNumber;
            }

            PlantUtilities::InitComponentNodes(0.0,
                                               this->MassFlowRateMax,
                                               this->InletNode,
                                               this->OutletNode,
                                               this->WLoopNum,
                                               this->WLoopSideNum,
                                               this->WLoopBranchNum,
                                               this->WLoopCompNum);

            this->Init = false;

            if (this->InitICS) {
                this->TempOfWater = 20.0;
                this->SavedTempOfWater = this->TempOfWater;
                this->SavedTempOfAbsPlate = this->TempOfWater;
                this->TempOfAbsPlate = this->TempOfWater;
                this->TempOfInnerCover = this->TempOfWater;
                this->TempOfOuterCover = this->TempOfWater;
                this->SavedTempOfInnerCover = this->TempOfWater;
                this->SavedTempOfOuterCover = this->TempOfWater;
                this->SavedTempCollectorOSCM = this->TempOfWater;
            }
        }

        if (!DataGlobals::BeginEnvrnFlag) this->Init = true;

        if (this->SetDiffRadFlag && this->InitICS) {
            // calculates the sky and ground reflective diffuse radiation optical properties (only one time)
            int SurfNum = this->Surface;
            int ParamNum = this->Parameters;

            this->Tilt = DataSurfaces::Surface(SurfNum).Tilt;
            this->TiltR2V = std::abs(90.0 - Tilt);
            this->CosTilt = std::cos(Tilt * DataGlobalConstants::DegToRadians());
            this->SinTilt = std::sin(1.8 * Tilt * DataGlobalConstants::DegToRadians());

            // Diffuse reflectance of the cover for solar radiation diffusely reflected back from the absober
            // plate to the cover.  The diffuse solar radiation reflected back from the absober plate to the
            // cover is represented by the 60 degree equivalent incident angle.  This diffuse reflectance is
            // used to calculate the transmittance - absorptance product (Duffie and Beckman, 1991)
            Real64 Theta = 60.0 * DataGlobalConstants::DegToRadians();
            Real64 TransSys = 0.0;
            Real64 RefSys = 0.0;
            Real64 AbsCover1 = 0.0;
            Real64 AbsCover2 = 0.0;
            Real64 RefSysDiffuse = 0.0;
            this->CalcTransRefAbsOfCover(Theta, TransSys, RefSys, AbsCover1, AbsCover2, true, RefSysDiffuse);
            this->RefDiffInnerCover = RefSysDiffuse;

            // transmittance-absorptance product normal incident:
            Theta = 0.0;
            this->CalcTransRefAbsOfCover(Theta, TransSys, RefSys, AbsCover1, AbsCover2);
            this->TauAlphaNormal = TransSys * SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate /
                                   (1.0 - (1.0 - SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate) * this->RefDiffInnerCover);

            // transmittance-absorptance product for sky diffuse radiation.  Uses equivalent incident angle
            // of sky radiation (radians), and is calculated according to Brandemuehl and Beckman (1980):
            Theta = (59.68 - 0.1388 * Tilt + 0.001497 * pow_2(Tilt)) * DataGlobalConstants::DegToRadians();
            this->CalcTransRefAbsOfCover(Theta, TransSys, RefSys, AbsCover1, AbsCover2);
            this->TauAlphaSkyDiffuse = TransSys * SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate /
                                       (1.0 - (1.0 - SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate) * this->RefDiffInnerCover);
            this->CoversAbsSkyDiffuse(1) = AbsCover1;
            this->CoversAbsSkyDiffuse(2) = AbsCover2;

            // transmittance-absorptance product for ground diffuse radiation.  Uses equivalent incident angle
            // of ground radiation (radians), and is calculated according to Brandemuehl and Beckman (1980):
            Theta = (90.0 - 0.5788 * Tilt + 0.002693 * pow_2(Tilt)) * DataGlobalConstants::DegToRadians();
            this->CalcTransRefAbsOfCover(Theta, TransSys, RefSys, AbsCover1, AbsCover2);
            this->TauAlphaGndDiffuse = TransSys * SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate /
                                       (1.0 - (1.0 - SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate) * this->RefDiffInnerCover);
            this->CoversAbsGndDiffuse(1) = AbsCover1;
            this->CoversAbsGndDiffuse(2) = AbsCover2;

            this->SetDiffRadFlag = false;
        }

        this->InletTemp = DataLoopNode::Node(this->InletNode).Temp;

        this->MassFlowRate = this->MassFlowRateMax;

        // Request the mass flow rate from the plant component flow utility routine
        PlantUtilities::SetComponentFlowRate(
            this->MassFlowRate, this->InletNode, this->OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum);

        if (this->InitICS) {

            Real64 timeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;

            if (this->TimeElapsed != timeElapsed) {
                // The simulation has advanced to the next system timestep.  Save conditions from the end of the previous
                // system timestep for use as initial condition of each iteration that does not advance system timestep.
                this->SavedTempOfWater = this->TempOfWater;
                this->SavedTempOfAbsPlate = this->TempOfAbsPlate;
                this->SavedTempOfInnerCover = this->TempOfInnerCover;
                this->SavedTempOfOuterCover = this->TempOfOuterCover;
                if (this->OSCM_ON) {
                    this->SavedTempCollectorOSCM = DataSurfaces::ExtVentedCavity(this->VentCavIndex).Tbaffle;
                }
                this->TimeElapsed = timeElapsed;
            }
        }
    }

    void CollectorData::CalcSolarCollector(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the heat gain (or loss), outlet temperature, and solar energy conversion efficiency for a flat-plate
        // solar collector when there is a fluid flow.  For the no flow condition, the fluid stagnation temperature is
        // calculated as the outlet temperature.  Glazed and unglazed collectors are both handled.

        // METHODOLOGY EMPLOYED:
        // Calculation is performed using the methodology described in the ASHRAE standards and references below.  Measured
        // collector performance coefficients (available from the Solar Rating & Certification Corporation, for example)
        // are modified from the test conditions to match the actual optical (incident angle modifier) and thermal (flow rate
        // modifier) conditions.  Water is assumed to be the heat transfer fluid.

        // REFERENCES:
        // ASHRAE Standard 93-1986 (RA 91), "Methods of Testing to Determine the Thermal Performance of Solar Collectors".
        // ASHRAE Standard 96-1980 (RA 89), "Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate
        //   Liquid-Type Solar Collectors".
        // Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.  Wiley-Interscience:
        //   New York (1991).

        // NOTES:
        // This subroutine has been validated against the TRNSYS Type 1 flat-plate solar collector module.  Results are
        // identical except for slight differences at extreme incident angles (>80 degrees) and extreme surface tilts (<20
        // degrees).  The differences are due to the fact that Type 1 does not prevent the *component* incident angle
        // modifiers from being less than zero.  There is an effect on the net incident angle modifier if one or more
        // components are less than zero but the net adds up to greater than zero.  The EnergyPlus subroutine, on the other
        // hand, requires each component incident angle modifier always to be greater than zero.

        static std::string const RoutineName("CalcSolarCollector");
        Real64 efficiency = 0.0; // Thermal efficiency of solar energy conversion

        int SurfNum = this->Surface;
        int ParamNum = this->Parameters;
        Real64 incidentAngleModifier; // Net incident angle modifier combining beam, sky, and ground radiation

        // Calculate incident angle modifier
        if (DataHeatBalance::QRadSWOutIncident(SurfNum) > 0.0) {
            // Equivalent incident angle of sky radiation (radians)
            Real64 ThetaBeam = std::acos(DataHeatBalance::CosIncidenceAngle(SurfNum));

            // Calculate equivalent incident angles for sky and ground radiation according to Brandemuehl and Beckman (1980)
            // Surface tilt angle (degrees)
            Real64 tilt = DataSurfaces::Surface(SurfNum).Tilt;

            // Equivalent incident angle of sky radiation (radians)
            Real64 ThetaSky = (59.68 - 0.1388 * tilt + 0.001497 * pow_2(tilt)) * DataGlobalConstants::DegToRadians();

            // Equivalent incident angle of ground radiation (radians)
            Real64 ThetaGnd = (90.0 - 0.5788 * tilt + 0.002693 * pow_2(tilt)) * DataGlobalConstants::DegToRadians();

            incidentAngleModifier = (DataHeatBalance::QRadSWOutIncidentBeam(SurfNum) * SolarCollectors::Parameters(ParamNum).IAM(ThetaBeam) +
                                     DataHeatBalance::QRadSWOutIncidentSkyDiffuse(SurfNum) * SolarCollectors::Parameters(ParamNum).IAM(ThetaSky) +
                                     DataHeatBalance::QRadSWOutIncidentGndDiffuse(SurfNum) * SolarCollectors::Parameters(ParamNum).IAM(ThetaGnd)) /
                                    DataHeatBalance::QRadSWOutIncident(SurfNum);
        } else {
            incidentAngleModifier = 0.0;
        }

        // Inlet temperature from plant (C)
        Real64 inletTemp = this->InletTemp;

        // Mass flow rate through collector (kg/s)
        Real64 massFlowRate = this->MassFlowRate;

        // Specific heat of collector fluid (J/kg-K)
        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            state, DataPlant::PlantLoop(this->WLoopNum).FluidName, inletTemp, DataPlant::PlantLoop(this->WLoopNum).FluidIndex, RoutineName);

        // Gross area of collector (m2)
        Real64 area = DataSurfaces::Surface(SurfNum).Area;

        // = MassFlowRate * Cp / Area
        Real64 mCpA = massFlowRate * Cp / area;

        // = MassFlowRateTest * Cp / Area (tested area)
        Real64 mCpATest = SolarCollectors::Parameters(ParamNum).TestMassFlowRate * Cp / SolarCollectors::Parameters(this->Parameters).Area;

        int Iteration = 1;

        // Outlet temperature or stagnation temperature in the collector (C)
        Real64 outletTemp = 0.0;

        // Outlet temperature saved from previous iteration for convergence check (C)
        Real64 OutletTempPrev = 999.9; // Set to a ridiculous number so that DO loop runs at least once

        // Heat gain or loss to collector fluid (W)
        Real64 Q = 0.0;

        while (std::abs(outletTemp - OutletTempPrev) > DataHeatBalance::TempConvergTol) { // Check for temperature convergence

            OutletTempPrev = outletTemp; // Save previous outlet temperature

            // Modifier for test correlation type:  INLET, AVERAGE, or OUTLET
            Real64 TestTypeMod = 0.0;

            // FR * ULoss "prime" for test conditions = (eff1 + eff2 * deltaT)
            Real64 FRULpTest = 0.0;

            // Modify coefficients depending on test correlation type
            {
                auto const SELECT_CASE_var(SolarCollectors::Parameters(ParamNum).TestType);
                if (SELECT_CASE_var == TestTypeEnum::INLET) {
                    FRULpTest = SolarCollectors::Parameters(ParamNum).eff1 +
                                SolarCollectors::Parameters(ParamNum).eff2 * (inletTemp - DataSurfaces::Surface(SurfNum).OutDryBulbTemp);
                    TestTypeMod = 1.0;

                } else if (SELECT_CASE_var == TestTypeEnum::AVERAGE) {
                    FRULpTest =
                        SolarCollectors::Parameters(ParamNum).eff1 +
                        SolarCollectors::Parameters(ParamNum).eff2 * ((inletTemp + outletTemp) * 0.5 - DataSurfaces::Surface(SurfNum).OutDryBulbTemp);
                    TestTypeMod = 1.0 / (1.0 - FRULpTest / (2.0 * mCpATest));

                } else if (SELECT_CASE_var == TestTypeEnum::OUTLET) {
                    FRULpTest = SolarCollectors::Parameters(ParamNum).eff1 +
                                SolarCollectors::Parameters(ParamNum).eff2 * (outletTemp - DataSurfaces::Surface(SurfNum).OutDryBulbTemp);
                    TestTypeMod = 1.0 / (1.0 - FRULpTest / mCpATest);
                }
            }

            // FR * tau * alpha at normal incidence = Y-intercept of collector efficiency
            Real64 FRTAN = SolarCollectors::Parameters(ParamNum).eff0 * TestTypeMod;

            // FR * ULoss = 1st order coefficient of collector efficiency
            Real64 FRUL = SolarCollectors::Parameters(ParamNum).eff1 * TestTypeMod;

            // FR * ULoss / T = 2nd order coefficient of collector efficiency
            Real64 FRULT = SolarCollectors::Parameters(ParamNum).eff2 * TestTypeMod;
            FRULpTest *= TestTypeMod;

            if (massFlowRate > 0.0) { // Calculate efficiency and heat transfer with flow

                // Modifier for flow rate different from test flow rate
                Real64 FlowMod = 0.0;

                // F prime * ULoss for test conditions = collector efficiency factor * overall loss coefficient
                Real64 FpULTest;

                if ((1.0 + FRULpTest / mCpATest) > 0.0) {
                    FpULTest = -mCpATest * std::log(1.0 + FRULpTest / mCpATest);
                } else {
                    FpULTest = FRULpTest; // Avoid LOG( <0 )
                }

                if ((-FpULTest / mCpA) < 700.0) {
                    FlowMod = mCpA * (1.0 - std::exp(-FpULTest / mCpA));
                } else { // avoid EXP(too large #)
                         // FlowMod = FlowMod; // Self-assignment commented out
                }
                if ((-FpULTest / mCpATest) < 700.0) {
                    FlowMod /= (mCpATest * (1.0 - std::exp(-FpULTest / mCpATest)));
                } else {
                    // FlowMod = FlowMod; // Self-assignment commented out
                }

                // Calculate fluid heat gain (or loss)
                // Heat loss is possible if there is no incident radiation and fluid is still flowing.
                Q = (FRTAN * incidentAngleModifier * DataHeatBalance::QRadSWOutIncident(SurfNum) +
                     FRULpTest * (inletTemp - DataSurfaces::Surface(SurfNum).OutDryBulbTemp)) *
                    area * FlowMod;

                outletTemp = inletTemp + Q / (massFlowRate * Cp);

                // CR 7877 bound unreasonable result
                if (outletTemp < -100) {
                    outletTemp = -100.0;
                    Q = massFlowRate * Cp * (outletTemp - inletTemp);
                }
                if (outletTemp > 200) {
                    outletTemp = 200.0;
                    Q = massFlowRate * Cp * (outletTemp - inletTemp);
                }

                if (DataHeatBalance::QRadSWOutIncident(SurfNum) > 0.0) { // Calculate thermal efficiency
                    // NOTE: Efficiency can be > 1 if Q > QRadSWOutIncident because of favorable delta T, i.e. warm outdoor temperature
                    efficiency = Q / (DataHeatBalance::QRadSWOutIncident(SurfNum) * area); // Q has units of W; QRadSWOutIncident has units of W/m2
                } else {
                    efficiency = 0.0;
                }

            } else { // Calculate stagnation temperature of fluid in collector (no flow)
                Q = 0.0;
                efficiency = 0.0;

                // Calculate temperature of stagnant fluid in collector
                Real64 A = -FRULT;
                Real64 B = -FRUL + 2.0 * FRULT * DataSurfaces::Surface(SurfNum).OutDryBulbTemp;
                Real64 C = -FRULT * pow_2(DataSurfaces::Surface(SurfNum).OutDryBulbTemp) + FRUL * DataSurfaces::Surface(SurfNum).OutDryBulbTemp -
                           FRTAN * incidentAngleModifier * DataHeatBalance::QRadSWOutIncident(SurfNum);
                Real64 qEquation = (pow_2(B) - 4.0 * A * C);
                if (qEquation < 0.0) {
                    if (this->ErrIndex == 0) {
                        ShowSevereMessage("CalcSolarCollector: " + DataPlant::ccSimPlantEquipTypes(this->TypeNum) + "=\"" + this->Name +
                                          "\", possible bad input coefficients.");
                        ShowContinueError("...coefficients cause negative quadratic equation part in calculating temperature of stagnant fluid.");
                        ShowContinueError("...examine input coefficients for accuracy. Calculation will be treated as linear.");
                    }
                    ShowRecurringSevereErrorAtEnd("CalcSolarCollector: " + DataPlant::ccSimPlantEquipTypes(this->TypeNum) + "=\"" + this->Name +
                                                      "\", coefficient error continues.",
                                                  this->ErrIndex,
                                                  qEquation,
                                                  qEquation);
                }
                if (FRULT == 0.0 || qEquation < 0.0) { // Linear, 1st order solution
                    outletTemp = DataSurfaces::Surface(SurfNum).OutDryBulbTemp -
                                 FRTAN * incidentAngleModifier * DataHeatBalance::QRadSWOutIncident(SurfNum) / FRUL;
                } else { // Quadratic, 2nd order solution
                    outletTemp = (-B + std::sqrt(qEquation)) / (2.0 * A);
                }
            }

            if (SolarCollectors::Parameters(ParamNum).TestType == TestTypeEnum::INLET)
                break; // Inlet temperature test correlations do not need to iterate

            if (Iteration > 100) {
                if (this->IterErrIndex == 0) {
                    ShowWarningMessage("CalcSolarCollector: " + DataPlant::ccSimPlantEquipTypes(this->TypeNum) + "=\"" + this->Name +
                                       "\":  Solution did not converge.");
                }
                ShowRecurringWarningErrorAtEnd("CalcSolarCollector: " + DataPlant::ccSimPlantEquipTypes(this->TypeNum) + "=\"" + this->Name +
                                                   "\", solution not converge error continues.",
                                               this->IterErrIndex);
                break;
            } else {
                ++Iteration;
            }

        } // Check for temperature convergence

        this->IncidentAngleModifier = incidentAngleModifier;
        this->Power = Q;
        this->HeatGain = max(Q, 0.0);
        this->HeatLoss = min(Q, 0.0);
        this->OutletTemp = outletTemp;
        this->Efficiency = efficiency;
    }

    Real64 ParametersData::IAM(Real64 const IncidentAngle)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   December 2003
        //       MODIFIED       Sept 2008, BG cut off IAM beyond 60 degrees.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the incident angle modifier based on the solar collector parameters.  Both first and second order
        // correlations are allowed.

        // METHODOLOGY EMPLOYED:
        // A simple function.

        // REFERENCES:
        // ASHRAE Standard 93-1986 (RA 91), "Methods of Testing to Determine the Thermal Performance of Solar Collectors".
        // ASHRAE Standard 96-1980 (RA 89), "Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate
        //   Liquid-Type Solar Collectors".
        // Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.  Wiley-Interscience:
        //   New York (1991).

        Real64 IAM;

        // cut off IAM for angles greater than 60 degrees. (CR 7534)
        Real64 CutoffAngle = 60.0 * DataGlobalConstants::DegToRadians();
        if (std::abs(IncidentAngle) > CutoffAngle) { // cut off, model curves not robust beyond cutoff
            // curves from FSEC/SRCC testing are only certified to 60 degrees, larger angles can cause numerical problems in curves
            IAM = 0.0;
        } else {

            Real64 s = (1.0 / std::cos(IncidentAngle)) - 1.0;

            IAM = 1.0 + this->iam1 * s + this->iam2 * pow_2(s);
            IAM = max(IAM, 0.0); // Never allow to be less than zero, but greater than one is a possibility

            if (IAM > 10.0) { // Greater than 10 is probably not a possibility
                ShowSevereError("IAM Function: SolarCollectorPerformance:FlatPlate = " + this->Name +
                                ":  Incident Angle Modifier is out of bounds due to bad coefficients.");
                ShowContinueError(format("Coefficient 2 of Incident Angle Modifier = {}", this->iam1));
                ShowContinueError(format("Coefficient 3 of Incident Angle Modifier = {}", this->iam2));
                ShowContinueError(format("Calculated Incident Angle Modifier = {}", IAM));
                ShowContinueError("Expected Incident Angle Modifier should be approximately 1.5 or less.");
                ShowFatalError("Errors in SolarCollectorPerformance:FlatPlate input.");
            }

        } // not greater than cut off angle

        return IAM;
    }

    void CollectorData::CalcICSSolarCollector(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   February 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the heat transferred (gain or loss), energy stored, skin heat loss, outlet temperature, solar energy
        // conversion efficiency, and transmittance-absorptance product of an ICS solar collector.

        // METHODOLOGY EMPLOYED:
        // The governing equations for the absorber and collector water heat balance equations are solved simultaneously.
        // The two coupled first ODE are solved analytically.
        // The transmittance-absorptance product of the collector cover-absorber system is calculated using ray tracing
        // method according to Duffie and Beckman(1991).
        // REFERENCES:
        // Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, 2nd. Edition.  Wiley-Interscience:
        // New York (1991).
        // NOTES:

        static std::string const RoutineName("CalcICSSolarCollector");

        int SurfNum = this->Surface;
        int ParamNum = this->Parameters;
        Real64 SecInTimeStep = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        Real64 TempWater = this->SavedTempOfWater;
        Real64 TempAbsPlate = this->SavedTempOfAbsPlate;
        Real64 TempOutdoorAir = DataSurfaces::Surface(SurfNum).OutDryBulbTemp;

        Real64 TempOSCM; // Otherside condition model temperature [C]
        if (this->OSCM_ON) {
            TempOSCM = this->SavedTempCollectorOSCM;
        } else {
            TempOSCM = TempOutdoorAir;
        }

        // Calculate transmittance-absorptance product of the system
        // Incident angle of beam radiation (radians)
        Real64 ThetaBeam = std::acos(DataHeatBalance::CosIncidenceAngle(SurfNum));
        this->CalcTransAbsorProduct(ThetaBeam);

        Real64 inletTemp = this->InletTemp;

        Real64 massFlowRate = this->MassFlowRate;

        // Specific heat of collector fluid (J/kg-K)
        Real64 Cpw = FluidProperties::GetSpecificHeatGlycol(
            state, DataPlant::PlantLoop(this->WLoopNum).FluidName, inletTemp, DataPlant::PlantLoop(this->WLoopNum).FluidIndex, RoutineName);

        // density of collector fluid (kg/m3)
        Real64 Rhow = FluidProperties::GetDensityGlycol(
            state, DataPlant::PlantLoop(this->WLoopNum).FluidName, inletTemp, DataPlant::PlantLoop(this->WLoopNum).FluidIndex, RoutineName);

        // calculate heat transfer coefficients and covers temperature:
        this->CalcHeatTransCoeffAndCoverTemp();

        // Calc convection heat transfer coefficient between the absorber plate and water:

        // convection coeff between absorber plate and water [W/m2K]
        Real64 hConvCoefA2W =
            EnergyPlus::SolarCollectors::CollectorData::CalcConvCoeffAbsPlateAndWater(state, TempAbsPlate, TempWater, this->Length, this->TiltR2V);
        Real64 TempWaterOld = TempWater;
        Real64 TempAbsPlateOld = TempAbsPlate;

        // flag if the absorber has thermal mass or not
        bool AbsPlateMassFlag;

        Real64 a1; // coefficient of ODE for absorber temperature Tp
        Real64 a2; // coefficient of ODE for absorber temperature Tw
        Real64 a3; // constant term of ODE for absorber temperature

        // Gross area of collector (m2)
        Real64 area = SolarCollectors::Parameters(ParamNum).Area;

        if (SolarCollectors::Parameters(ParamNum).ThermalMass > 0.0) {
            AbsPlateMassFlag = true;

            // thermal mass of the absorber plate [J/K]
            Real64 ap = SolarCollectors::Parameters(ParamNum).ThermalMass * area;
            a1 = -area * (hConvCoefA2W + this->UTopLoss) / ap;
            a2 = area * hConvCoefA2W / ap;
            a3 = area * (this->TauAlpha * DataHeatBalance::QRadSWOutIncident(SurfNum) + this->UTopLoss * TempOutdoorAir) / ap;
        } else {
            AbsPlateMassFlag = false;
            a1 = -area * (hConvCoefA2W + this->UTopLoss);
            a2 = area * hConvCoefA2W;
            a3 = area * (this->TauAlpha * DataHeatBalance::QRadSWOutIncident(SurfNum) + this->UTopLoss * TempOutdoorAir);
        }

        // thermal mass of the collector water [J/K]
        Real64 aw = SolarCollectors::Parameters(ParamNum).Volume * Rhow * Cpw;

        // coefficient of ODE for water temperature Tp
        Real64 b1 = area * hConvCoefA2W / aw;

        // coefficient of ODE for water temperature Tw
        Real64 b2 = -(area * (hConvCoefA2W + this->UbLoss + this->UsLoss) + massFlowRate * Cpw) / aw;

        // constant term of ODE for water temperature
        Real64 b3 = (area * (this->UbLoss * TempOSCM + this->UsLoss * TempOutdoorAir) + massFlowRate * Cpw * inletTemp) / aw;

        EnergyPlus::SolarCollectors::CollectorData::ICSCollectorAnalyticalSolution(
            SecInTimeStep, a1, a2, a3, b1, b2, b3, TempAbsPlateOld, TempWaterOld, TempAbsPlate, TempWater, AbsPlateMassFlag);

        this->SkinHeatLossRate = area * (this->UTopLoss * (TempOutdoorAir - TempAbsPlate) + this->UsLoss * (TempOutdoorAir - TempWater) +
                                         this->UbLoss * (TempOSCM - TempWater));
        this->StoredHeatRate = aw * (TempWater - TempWaterOld) / SecInTimeStep;

        // heat gain rate (W)
        Real64 QHeatRate = massFlowRate * Cpw * (TempWater - inletTemp);
        this->HeatRate = QHeatRate;
        this->HeatGainRate = max(0.0, QHeatRate);
        this->HeatLossRate = min(0.0, QHeatRate);

        Real64 outletTemp = TempWater;
        this->OutletTemp = outletTemp;
        this->TempOfWater = TempWater;
        this->TempOfAbsPlate = TempAbsPlate;

        Real64 efficiency = 0.0; // Thermal efficiency of solar energy conversion
        if (DataHeatBalance::QRadSWOutIncident(SurfNum) > 0.0) {
            efficiency = (this->HeatGainRate + this->StoredHeatRate) / (DataHeatBalance::QRadSWOutIncident(SurfNum) * area);
            if (efficiency < 0.0) efficiency = 0.0;
        }
        this->Efficiency = efficiency;
    }

    void CollectorData::ICSCollectorAnalyticalSolution(Real64 const SecInTimeStep,     // seconds in a time step
                                                       Real64 const a1,                // coefficient of ODE for Tp
                                                       Real64 const a2,                // coefficient of ODE for Tp
                                                       Real64 const a3,                // coefficient of ODE for Tp
                                                       Real64 const b1,                // coefficient of ODE for TW
                                                       Real64 const b2,                // coefficient of ODE for TW
                                                       Real64 const b3,                // coefficient of ODE for TW
                                                       Real64 const TempAbsPlateOld,   // absorber plate temperature at previous time step [C]
                                                       Real64 const TempWaterOld,      // collector water temperature at previous time step [C]
                                                       Real64 &TempAbsPlate,           // absorber plate temperature at current time step [C]
                                                       Real64 &TempWater,              // collector water temperature at current time step [C]
                                                       bool const AbsorberPlateHasMass // flag for absorber thermal mass
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   February 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the absorber plate and collector water temperatures.
        // METHODOLOGY EMPLOYED:
        // Analytical method: Solves the coupled absorber plate and collector water energy balance
        // equations.  The two non-homogeneous ordinary differential equations of the form.
        //          Tp' = a1*Tp + a2*Tw + a3.
        //          Tw' = b1*Tp + b2*Tw + b3.
        // The general solution of these coupled equation with real routes has the following form:
        //          Tp = ConstantC1*exp(lamda1*t) + ConstantC2*exp(lamda2*t) + ConstOfTpSln
        //          Tw = r1*ConstantC2*exp(lamda1*t) + r2*ConstantC2*exp(lamda2*t) + ConstOfTwSln

        if (AbsorberPlateHasMass) {

            // coefficients of quadratic equation a*m2+b*m+c=0
            Real64 a = 1.0;
            Real64 b = -(a1 + b2);
            Real64 c = a1 * b2 - a2 * b1;
            Real64 BSquareM4TimesATimesC = pow_2(b) - 4.0 * a * c;

            if (BSquareM4TimesATimesC > 0.0) {

                // the real roots of the quadratic equation
                Real64 lamda1 = (-b + std::sqrt(BSquareM4TimesATimesC)) / (2.0 * a);
                Real64 lamda2 = (-b - std::sqrt(BSquareM4TimesATimesC)) / (2.0 * a);

                // the particular solution for the ODE
                Real64 ConstOfTpSln = (-a3 * b2 + b3 * a2) / c;
                Real64 ConstOfTwSln = (-a1 * b3 + b1 * a3) / c;

                // ratio of the ODE solution constant coefficients
                Real64 r1 = (lamda1 - a1) / a2;
                Real64 r2 = (lamda2 - a1) / a2;

                // coefficients of the ODE solution
                Real64 ConstantC2 = (TempWaterOld + r1 * ConstOfTpSln - r1 * TempAbsPlateOld - ConstOfTwSln) / (r2 - r1);
                Real64 ConstantC1 = (TempAbsPlateOld - ConstOfTpSln - ConstantC2);

                TempAbsPlate = ConstantC1 * std::exp(lamda1 * SecInTimeStep) + ConstantC2 * std::exp(lamda2 * SecInTimeStep) + ConstOfTpSln;
                TempWater = r1 * ConstantC1 * std::exp(lamda1 * SecInTimeStep) + r2 * ConstantC2 * std::exp(lamda2 * SecInTimeStep) + ConstOfTwSln;

            } else { // this should never occur
                ShowSevereError(
                    "ICSCollectorAnalyticalSoluton: Unanticipated differential equation coefficient - report to EnergyPlus Development Team");
                ShowFatalError("Program terminates due to above conditions.");
            }
        } else {
            // In the absence of absorber plate thermal mass, only the collector water heat balance has a
            // differential equation of the form: Tw' = b1*Tp + b2*Tw + b3. The absorber plate energy balance
            // equation in the absence of thermal mass is a steady state form:  b1*Tp + b2*Tw + b3 = 0
            Real64 b = b2 - b1 * (a2 / a1);
            Real64 c = b3 - b1 * (a3 / a1);
            TempWater = (TempWaterOld + c / b) * std::exp(b * SecInTimeStep) - c / b;
            TempAbsPlate = -(a2 * TempWater + a3) / a1;
        }
    }

    void CollectorData::CalcTransAbsorProduct(Real64 const IncidAngle)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket A Nigusse
        //       DATE WRITTEN   February 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates transmittance-absorptance product and the fraction of total solar radiation
        // absorbed by each cover of a multicover ICS solar collector.

        // METHODOLOGY EMPLOYED:
        // Uses a ray tracing method.

        // REFERENCES:
        // Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
        // Wiley-Interscience: New York (1991).

        Real64 TransSys = 1.0;  // cover system solar transmittance
        Real64 ReflSys = 0.0;   // cover system solar reflectance
        Real64 AbsCover1 = 0.0; // Inner cover solar absorbtance
        Real64 AbsCover2 = 0.0; // Outer cover solar absorbtance
        Real64 TuaAlpha;        // weighted trans-abs product of system
        Real64 TuaAlphaBeam;    // trans-abs product of beam radiation
        this->CoverAbs(1) = 0.0;
        this->CoverAbs(2) = 0.0;

        int SurfNum = this->Surface;
        int ParamNum = this->Parameters;

        if (DataHeatBalance::QRadSWOutIncident(SurfNum) > 0.0) {

            // cover system transmittance and reflectance from outer to inner cover
            this->CalcTransRefAbsOfCover(IncidAngle, TransSys, ReflSys, AbsCover1, AbsCover2);

            TuaAlphaBeam = TransSys * SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate /
                           (1.0 - (1.0 - SolarCollectors::Parameters(ParamNum).AbsorOfAbsPlate) * this->RefDiffInnerCover);

            this->TauAlphaBeam = max(0.0, TuaAlphaBeam);

            Array1D<Real64> CoversAbsBeam(2); // Inner and Outer Cover absorptance
            CoversAbsBeam(1) = AbsCover1;
            CoversAbsBeam(2) = AbsCover2;

            // calc total solar radiation weighted transmittance-absorptance product
            TuaAlpha = (DataHeatBalance::QRadSWOutIncidentBeam(SurfNum) * this->TauAlphaBeam +
                        DataHeatBalance::QRadSWOutIncidentSkyDiffuse(SurfNum) * this->TauAlphaSkyDiffuse +
                        DataHeatBalance::QRadSWOutIncidentGndDiffuse(SurfNum) * this->TauAlphaGndDiffuse) /
                       DataHeatBalance::QRadSWOutIncident(SurfNum);

            if (SolarCollectors::Parameters(ParamNum).NumOfCovers == 1) {
                // calc total solar radiation weighted cover absorptance
                this->CoverAbs(1) = (DataHeatBalance::QRadSWOutIncidentBeam(SurfNum) * CoversAbsBeam(1) +
                                     DataHeatBalance::QRadSWOutIncidentSkyDiffuse(SurfNum) * this->CoversAbsSkyDiffuse(1) +
                                     DataHeatBalance::QRadSWOutIncidentGndDiffuse(SurfNum) * this->CoversAbsGndDiffuse(1)) /
                                    DataHeatBalance::QRadSWOutIncident(SurfNum);

            } else if (SolarCollectors::Parameters(ParamNum).NumOfCovers == 2) {
                // Num = 1 represents outer cover and Num = 2 represents inner cover
                for (int Num = 1; Num <= SolarCollectors::Parameters(ParamNum).NumOfCovers; ++Num) {
                    this->CoverAbs(Num) = (DataHeatBalance::QRadSWOutIncidentBeam(SurfNum) * CoversAbsBeam(Num) +
                                           DataHeatBalance::QRadSWOutIncidentSkyDiffuse(SurfNum) * this->CoversAbsSkyDiffuse(Num) +
                                           DataHeatBalance::QRadSWOutIncidentGndDiffuse(SurfNum) * this->CoversAbsGndDiffuse(Num)) /
                                          DataHeatBalance::QRadSWOutIncident(SurfNum);
                }
            }

        } else {
            TuaAlpha = 0.0;
        }
        this->TauAlpha = TuaAlpha;
    }

    void CollectorData::CalcTransRefAbsOfCover(Real64 const IncidentAngle,    // Angle of incidence (radians)
                                               Real64 &TransSys,              // cover system solar transmittance
                                               Real64 &ReflSys,               // cover system solar reflectance
                                               Real64 &AbsCover1,             // Inner cover solar absorbtance
                                               Real64 &AbsCover2,             // Outer cover solar absorbtance
                                               Optional_bool_const InOUTFlag, // flag for calc. diffuse solar refl of cover from inside out
                                               Optional<Real64> RefSysDiffuse // cover system solar reflectance from inner to outer cover
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket A Nigusse
        //       DATE WRITTEN   February 2012

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the transmitance, reflectance, and absorptance of the collector covers based on
        // solar collector optical parameters specified.

        // METHODOLOGY EMPLOYED:
        // Uses a ray tracing method.

        // REFERENCES:
        // Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
        // Wiley-Interscience: New York (1991).

        Real64 const AirRefIndex(1.0003); // refractive index of air

        Array1D<Real64> TransPara(2);    // cover transmittance parallel component
        Array1D<Real64> TransPerp(2);    // cover transmittance perpendicular component
        Array1D<Real64> ReflPara(2);     // cover reflectance parallel component
        Array1D<Real64> ReflPerp(2);     // cover reflectance Perpendicular component
        Array1D<Real64> AbsorPara(2);    // cover absorbtance parallel component
        Array1D<Real64> AbsorPerp(2);    // cover absorbtance Perpendicular component
        Array1D<Real64> TransAbsOnly(2); // cover transmittance with absorptance only considered

        TransPerp = 1.0;
        TransPara = 1.0;
        ReflPerp = 0.0;
        ReflPara = 0.0;
        AbsorPerp = 0.0;
        AbsorPara = 0.0;
        TransAbsOnly = 1.0;
        TransSys = 0.0;
        ReflSys = 0.0;
        AbsCover1 = 0.0;
        AbsCover2 = 0.0;

        bool DiffRefFlag; // flag for calc. diffuse refl of cover from inside to outsidd
        if (present(InOUTFlag)) {
            DiffRefFlag = InOUTFlag;
        } else {
            DiffRefFlag = false;
        }

        // get the incidence and refraction angles
        int ParamNum = this->Parameters;
        Real64 const sin_IncAngle(std::sin(IncidentAngle));

        for (int nCover = 1; nCover <= SolarCollectors::Parameters(ParamNum).NumOfCovers; ++nCover) {

            // refractive index of collector cover
            Real64 CoverRefrIndex = SolarCollectors::Parameters(ParamNum).RefractiveIndex(nCover);

            // angle of refraction
            Real64 RefrAngle = std::asin(sin_IncAngle * AirRefIndex / CoverRefrIndex);

            // transmitted component with absorption only considered:
            TransAbsOnly(nCover) = std::exp(-SolarCollectors::Parameters(ParamNum).ExtCoefTimesThickness(nCover) / std::cos(RefrAngle));

            // parallel reflected component of unpolarized solar radiation
            Real64 ParaRad;

            // Perpendicular reflected component of unpolarized solar radiation
            Real64 PerpRad;

            // parallel and perpendicular reflection components:
            if (IncidentAngle == 0.0) {
                ParaRad = pow_2((CoverRefrIndex - AirRefIndex) / (CoverRefrIndex + AirRefIndex));
                PerpRad = pow_2((CoverRefrIndex - AirRefIndex) / (CoverRefrIndex + AirRefIndex));
            } else {
                ParaRad = pow_2(std::tan(RefrAngle - IncidentAngle)) / pow_2(std::tan(RefrAngle + IncidentAngle));
                PerpRad = pow_2(std::sin(RefrAngle - IncidentAngle)) / pow_2(std::sin(RefrAngle + IncidentAngle));
            }

            // parallel and perpendicular transmitted components:
            TransPerp(nCover) =
                TransAbsOnly(nCover) * ((1.0 - PerpRad) / (1.0 + PerpRad)) * ((1.0 - pow_2(PerpRad)) / (1.0 - pow_2(PerpRad * TransAbsOnly(nCover))));
            TransPara(nCover) =
                TransAbsOnly(nCover) * ((1.0 - ParaRad) / (1.0 + ParaRad)) * ((1.0 - pow_2(ParaRad)) / (1.0 - pow_2(ParaRad * TransAbsOnly(nCover))));

            ReflPerp(nCover) =
                (PerpRad + (pow_2(1.0 - PerpRad) * pow_2(TransAbsOnly(nCover)) * PerpRad) / (1.0 - pow_2(PerpRad * TransAbsOnly(nCover))));
            ReflPara(nCover) =
                (ParaRad + (pow_2(1.0 - ParaRad) * pow_2(TransAbsOnly(nCover)) * ParaRad) / (1.0 - pow_2(ParaRad * TransAbsOnly(nCover))));

            AbsorPerp(nCover) = 1.0 - TransPerp(nCover) - ReflPerp(nCover);
            AbsorPara(nCover) = 1.0 - TransPara(nCover) - ReflPara(nCover);
        }

        // solar absorptance of the individual cover
        AbsCover1 = 0.5 * (AbsorPerp(1) + AbsorPara(1));
        if (SolarCollectors::Parameters(ParamNum).NumOfCovers == 2) AbsCover2 = 0.5 * (AbsorPerp(2) + AbsorPara(2));

        // calculate from outer to inner cover:
        TransSys =
            0.5 * (TransPerp(1) * TransPerp(2) / (1.0 - ReflPerp(1) * ReflPerp(2)) + TransPara(1) * TransPara(2) / (1.0 - ReflPara(1) * ReflPara(2)));
        ReflSys = 0.5 * (ReflPerp(1) + TransSys * ReflPerp(2) * TransPerp(1) / TransPerp(2) + ReflPara(1) +
                         TransSys * ReflPara(2) * TransPara(1) / TransPara(2));
        if (DiffRefFlag) {
            // calculate from inner to outer cover:

            // cover system solar transmittance from inner to outer cover
            Real64 TransSysDiff = 0.5 * (TransPerp(2) * TransPerp(1) / (1.0 - ReflPerp(2) * ReflPerp(1)) +
                                         TransPara(2) * TransPara(1) / (1.0 - ReflPara(2) * ReflPara(1)));
            RefSysDiffuse = 0.5 * (ReflPerp(2) + TransSysDiff * ReflPerp(1) * TransPerp(2) / TransPerp(1) + ReflPara(2) +
                                   TransSysDiff * ReflPara(1) * TransPara(2) / TransPara(1));
        }
    }

    void CollectorData::CalcHeatTransCoeffAndCoverTemp() // Collector object number
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket A Nigusse, FSEC/UCF
        //       DATE WRITTEN   February 2012

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the various heat transfer coefficients, and collector cover temperatures.

        // METHODOLOGY EMPLOYED:

        // REFERENCES:
        // Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
        // Wiley-Interscience: New York (1991).

        Real64 tempnom;             // intermediate variable
        Real64 tempdenom;           // intermediate variable
        Real64 hRadCoefC2Sky;       // radiation coeff from collector to the sky [W/m2C]
        Real64 hRadCoefC2Gnd = 0.0; // radiation coeff from collector to the ground [W/m2C]
        Real64 hConvCoefA2C = 0.0;  // convection coeff. between abs plate and cover [W/m2C]
        Real64 hConvCoefC2C = 0.0;  // convection coeff. between covers [W/m2C]
        Real64 hConvCoefC2O = 0.0;  // convection coeff. between outer cover and the ambient [W/m2C]
        Real64 hRadCoefA2C = 0.0;   // radiation coeff. between abs plate and cover [W/m2C]
        Real64 hRadCoefC2C = 0.0;   // radiation coeff. between covers [W/m2C]
        Real64 hRadCoefC2O = 0.0;   // radiation coeff. between outer covers and the ambient [W/m2C]

        int ParamNum = this->Parameters;
        int NumCovers = SolarCollectors::Parameters(ParamNum).NumOfCovers;
        int SurfNum = this->Surface;

        Real64 TempAbsPlate = this->SavedTempOfAbsPlate;                       // absorber plate average temperature [C]
        Real64 TempInnerCover = this->SavedTempOfInnerCover;                   // inner cover average temperature [C]
        Real64 TempOuterCover = this->SavedTempOfOuterCover;                   // outer cover average temperature [C]
        Real64 TempOutdoorAir = DataSurfaces::Surface(SurfNum).OutDryBulbTemp; // outdoor air temperature [C]

        Real64 EmissOfAbsPlate = SolarCollectors::Parameters(ParamNum).EmissOfAbsPlate;   // emissivity of absorber plate
        Real64 EmissOfOuterCover = SolarCollectors::Parameters(ParamNum).EmissOfCover(1); // emissivity of outer cover
        Real64 EmissOfInnerCover = SolarCollectors::Parameters(ParamNum).EmissOfCover(2); // emissivity of inner cover
        Real64 AirGapDepth = SolarCollectors::Parameters(ParamNum).CoverSpacing;          // characteristic length [m]

        {
            auto const SELECT_CASE_var(NumCovers);
            if (SELECT_CASE_var == 1) {
                // calc linearized radiation coefficient
                tempnom = DataGlobals::StefanBoltzmann * ((TempAbsPlate + DataGlobals::KelvinConv) + (TempOuterCover + DataGlobals::KelvinConv)) *
                          (pow_2(TempAbsPlate + DataGlobals::KelvinConv) + pow_2(TempOuterCover + DataGlobals::KelvinConv));
                tempdenom = 1.0 / EmissOfAbsPlate + 1.0 / EmissOfOuterCover - 1.0;
                hRadCoefA2C = tempnom / tempdenom;
                hRadCoefC2C = 0.0;
                hConvCoefC2C = 0.0;
                // Calc convection heat transfer coefficient:
                hConvCoefA2C = EnergyPlus::SolarCollectors::CollectorData::CalcConvCoeffBetweenPlates(
                    TempAbsPlate, TempOuterCover, AirGapDepth, this->CosTilt, this->SinTilt);
            } else if (SELECT_CASE_var == 2) {
                for (int CoverNum = 1; CoverNum <= NumCovers; ++CoverNum) {
                    if (CoverNum == 1) {
                        // calc linearized radiation coefficient
                        tempnom = DataGlobals::StefanBoltzmann *
                                  ((TempAbsPlate + DataGlobals::KelvinConv) + (TempInnerCover + DataGlobals::KelvinConv)) *
                                  (pow_2(TempAbsPlate + DataGlobals::KelvinConv) + pow_2(TempInnerCover + DataGlobals::KelvinConv));
                        tempdenom = 1.0 / EmissOfAbsPlate + 1.0 / EmissOfInnerCover - 1.0;
                        hRadCoefA2C = tempnom / tempdenom;
                        // Calc convection heat transfer coefficient:
                        hConvCoefA2C = EnergyPlus::SolarCollectors::CollectorData::CalcConvCoeffBetweenPlates(
                            TempAbsPlate, TempOuterCover, AirGapDepth, this->CosTilt, this->SinTilt);
                    } else {
                        // calculate the linearized radiation coeff.
                        tempnom = DataGlobals::StefanBoltzmann *
                                  ((TempInnerCover + DataGlobals::KelvinConv) + (TempOuterCover + DataGlobals::KelvinConv)) *
                                  (pow_2(TempInnerCover + DataGlobals::KelvinConv) + pow_2(TempOuterCover + DataGlobals::KelvinConv));
                        tempdenom = 1.0 / EmissOfInnerCover + 1.0 / EmissOfOuterCover - 1.0;
                        hRadCoefC2C = tempnom / tempdenom;
                        // Calc convection heat transfer coefficient:
                        hConvCoefC2C = EnergyPlus::SolarCollectors::CollectorData::CalcConvCoeffBetweenPlates(
                            TempInnerCover, TempOuterCover, AirGapDepth, this->CosTilt, this->SinTilt);
                    }
                }
            }
        }

        // Calc collector outside surface convection heat transfer coefficient:
        hConvCoefC2O = 2.8 + 3.0 * DataSurfaces::Surface(SurfNum).WindSpeed;

        // Calc linearized radiation coefficient between outer cover and the surrounding:
        tempnom = DataSurfaces::Surface(SurfNum).ViewFactorSky * EmissOfOuterCover * DataGlobals::StefanBoltzmann *
                  ((TempOuterCover + DataGlobals::KelvinConv) + DataEnvironment::SkyTempKelvin) *
                  (pow_2(TempOuterCover + DataGlobals::KelvinConv) + pow_2(DataEnvironment::SkyTempKelvin));
        tempdenom = (TempOuterCover - TempOutdoorAir) / (TempOuterCover - DataEnvironment::SkyTemp);
        if (tempdenom < 0.0) {
            // use approximate linearized radiation coefficient
            hRadCoefC2Sky = tempnom;
        } else if (tempdenom == 0.0) {
            // if temperature difference is zero, no radiation exchange
            hRadCoefC2Sky = 0.0;
        } else {
            hRadCoefC2Sky = tempnom / tempdenom;
        }

        tempnom = DataSurfaces::Surface(SurfNum).ViewFactorGround * EmissOfOuterCover * DataGlobals::StefanBoltzmann *
                  ((TempOuterCover + DataGlobals::KelvinConv) + DataEnvironment::GroundTempKelvin) *
                  (pow_2(TempOuterCover + DataGlobals::KelvinConv) + pow_2(DataEnvironment::GroundTempKelvin));
        tempdenom = (TempOuterCover - TempOutdoorAir) / (TempOuterCover - DataEnvironment::GroundTemp);
        if (tempdenom < 0.0) {
            // use approximate linearized radiation coefficient
            hRadCoefC2Gnd = tempnom;
        } else if (tempdenom == 0.0) {
            // if temperature difference is zero, no radiation exchange
            hRadCoefC2Gnd = 0.0;
        } else {
            hRadCoefC2Gnd = tempnom / tempdenom;
        }

        // combine the radiation coefficients
        hRadCoefC2O = hRadCoefC2Sky + hRadCoefC2Gnd;

        // calculate the overall top heat loss coefficient:

        if (NumCovers == 1) {
            this->UTopLoss = 1.0 / (1.0 / (hRadCoefA2C + hConvCoefA2C) + 1.0 / (hRadCoefC2O + hConvCoefC2O));
        } else {
            this->UTopLoss = 1.0 / (1.0 / (hRadCoefA2C + hConvCoefA2C) + 1.0 / (hRadCoefC2C + hConvCoefC2C) + 1.0 / (hRadCoefC2O + hConvCoefC2O));
        }

        // calculate the side loss coefficient.  Adds the insulation resistance and the combined
        // convection-radiation coefficients in series.
        Real64 hRadConvOut = 5.7 + 3.8 * DataSurfaces::Surface(SurfNum).WindSpeed;
        this->UsLoss = 1.0 / (1.0 / (SolarCollectors::Parameters(ParamNum).ULossSide * this->AreaRatio) + 1.0 / (hRadConvOut * this->AreaRatio));

        // the bottom loss coefficient calculation depends on the boundary condition
        if (this->OSCM_ON) { // OtherSideConditionsModel
            this->UbLoss = SolarCollectors::Parameters(ParamNum).ULossBottom;
        } else { // AmbientAir
            this->UbLoss = 1.0 / (1.0 / SolarCollectors::Parameters(ParamNum).ULossBottom + 1.0 / hRadConvOut);
        }

        // Calculate current timestep covers temperature
        {
            auto const SELECT_CASE_var(NumCovers);
            if (SELECT_CASE_var == 1) {
                tempnom = this->CoverAbs(1) * DataHeatBalance::QRadSWOutIncident(SurfNum) + TempOutdoorAir * (hConvCoefC2O + hRadCoefC2O) +
                          TempAbsPlate * (hConvCoefA2C + hRadCoefA2C);
                tempdenom = (hConvCoefC2O + hRadCoefC2O) + (hConvCoefA2C + hRadCoefA2C);
                TempOuterCover = tempnom / tempdenom;
            } else if (SELECT_CASE_var == 2) {
                for (int Num = 1; Num <= NumCovers; ++Num) {
                    if (Num == 1) {
                        tempnom = this->CoverAbs(Num) * DataHeatBalance::QRadSWOutIncident(SurfNum) + TempOutdoorAir * (hConvCoefC2O + hRadCoefC2O) +
                                  TempInnerCover * (hConvCoefC2C + hRadCoefC2C);
                        tempdenom = (hConvCoefC2O + hRadCoefC2O) + (hConvCoefC2C + hRadCoefC2C);
                        TempOuterCover = tempnom / tempdenom;
                    } else if (Num == 2) {
                        tempnom = this->CoverAbs(Num) * DataHeatBalance::QRadSWOutIncident(SurfNum) + TempAbsPlate * (hConvCoefA2C + hRadCoefA2C) +
                                  TempOuterCover * (hConvCoefC2C + hRadCoefC2C);
                        tempdenom = (hConvCoefC2C + hRadCoefC2C + hConvCoefA2C + hRadCoefA2C);
                        TempInnerCover = tempnom / tempdenom;
                    }
                }
            }
        }
        this->TempOfInnerCover = TempInnerCover;
        this->TempOfOuterCover = TempOuterCover;
    }

    Real64 CollectorData::CalcConvCoeffBetweenPlates(Real64 const TempSurf1, // temperature of surface 1
                                                     Real64 const TempSurf2, // temperature of surface 1
                                                     Real64 const AirGap,    // characteristic length [m]
                                                     Real64 const CosTilt,   // cosine of surface tilt angle relative to the horizontal
                                                     Real64 const SinTilt    // sine of surface tilt angle relative to the horizontal
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   February 2012

        // PURPOSE OF THIS FUNCTION:
        //   Calculates the convection coefficient for an enclosure between two parallel surfaces
        //   at different temperatures.
        // METHODOLOGY EMPLOYED:
        //   Uses empirical correlation by Holands et al (1976) to determine free convection between
        //   inclined parallel plates at different temperature.
        // REFERENCES:
        //   Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, 2nd. Edition.
        //   Wiley-Interscience: New York (1991).
        //   Property data for air at atmospheric pressure were taken from Table A-11, Yunus A Cengel
        //   Heat Transfer: A Practical Approach, McGraw-Hill, Boston, MA, 1998.

        Real64 const gravity(9.806); // gravitational constant [m/s^2]

        int const NumOfPropDivisions(11);
        static Array1D<Real64> const Temps(NumOfPropDivisions,
                                           {-23.15, 6.85, 16.85, 24.85, 26.85, 36.85, 46.85, 56.85, 66.85, 76.85, 126.85}); // Temperature, in C
        static Array1D<Real64> const Mu(
            NumOfPropDivisions,
            {0.0000161, 0.0000175, 0.000018, 0.0000184, 0.0000185, 0.000019, 0.0000194, 0.0000199, 0.0000203, 0.0000208, 0.0000229}); // Viscosity, in
                                                                                                                                      // kg/(m.s)
        static Array1D<Real64> const Conductivity(
            NumOfPropDivisions, {0.0223, 0.0246, 0.0253, 0.0259, 0.0261, 0.0268, 0.0275, 0.0283, 0.0290, 0.0297, 0.0331}); // Conductivity, in W/mK
        static Array1D<Real64> const Pr(
            NumOfPropDivisions, {0.724, 0.717, 0.714, 0.712, 0.712, 0.711, 0.71, 0.708, 0.707, 0.706, 0.703}); // Prandtl number (dimensionless)
        static Array1D<Real64> const Density(NumOfPropDivisions,
                                             {1.413, 1.271, 1.224, 1.186, 1.177, 1.143, 1.110, 1.076, 1.043, 1.009, 0.883}); // Density, in kg/m3

        Real64 CondOfAir; // thermal conductivity of air [W/mK]
        Real64 VisDOfAir; // dynamic viscosity of air [kg/m.s]
        Real64 DensOfAir; // density of air [W/mK]
        Real64 PrOfAir;   // Prantle number of air [W/mK]
        Real64 VolExpAir; // volumetric expansion of air [1/K]

        Real64 DeltaT = std::abs(TempSurf1 - TempSurf2);
        Real64 Tref = 0.5 * (TempSurf1 + TempSurf2);
        int Index = 1;
        while (Index <= NumOfPropDivisions) {
            if (Tref < Temps(Index)) break; // DO loop
            ++Index;
        }

        // Initialize thermal properties of air
        if (Index == 1) {
            VisDOfAir = Mu(Index);
            CondOfAir = Conductivity(Index);
            PrOfAir = Pr(Index);
            DensOfAir = Density(Index);
        } else if (Index > NumOfPropDivisions) {
            Index = NumOfPropDivisions;
            VisDOfAir = Mu(Index);
            CondOfAir = Conductivity(Index);
            PrOfAir = Pr(Index);
            DensOfAir = Density(Index);
        } else {
            Real64 InterpFrac = (Tref - Temps(Index - 1)) / (Temps(Index) - Temps(Index - 1));
            VisDOfAir = Mu(Index - 1) + InterpFrac * (Mu(Index) - Mu(Index - 1));
            CondOfAir = Conductivity(Index - 1) + InterpFrac * (Conductivity(Index) - Conductivity(Index - 1));
            PrOfAir = Pr(Index - 1) + InterpFrac * (Pr(Index) - Pr(Index - 1));
            DensOfAir = Density(Index - 1) + InterpFrac * (Density(Index) - Density(Index - 1));
        }

        VolExpAir = 1.0 / (Tref + DataGlobals::KelvinConv);

        // Rayleigh number
        Real64 RaNum = gravity * pow_2(DensOfAir) * VolExpAir * PrOfAir * DeltaT * pow_3(AirGap) / pow_2(VisDOfAir);

        // Rayleigh number of air times cosine of collector tilt []
        Real64 RaNumCosTilt = RaNum * CosTilt;

        Real64 NuL = 0.0; // Nusselt number
        if (RaNum == 0.0) {
            NuL = 0.0;
        } else {
            if (RaNumCosTilt > 1708.0) {
                NuL = 1.44 * (1.0 - 1708.0 * std::pow(SinTilt, 1.6) / (RaNum * CosTilt)) * (1.0 - 1708.0 / RaNumCosTilt);
            } else {
                NuL = 0.0;
            }
        }
        if (RaNumCosTilt > 5830.0) {
            NuL += std::pow(RaNumCosTilt / 5830.0 - 1.0, 1.0 / 3.0);
        }
        ++NuL;
        Real64 hConvCoef = NuL * CondOfAir / AirGap;

        return hConvCoef;
    }

    Real64 CollectorData::CalcConvCoeffAbsPlateAndWater(EnergyPlusData &state,
                                                        Real64 const TAbsorber, // temperature of absorber plate [C]
                                                        Real64 const TWater,    // temperature of water [C]
                                                        Real64 const Lc,        // characteristic length [m]
                                                        Real64 const TiltR2V    // collector tilt angle relative to the vertical [degree]
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   February 2012

        // PURPOSE OF THIS FUNCTION:
        //  Calculates the free convection coefficient between the absorber plate and water.
        // METHODOLOGY EMPLOYED:
        //  The convection coefficient calculation were based on the Fujii and Imura emperical correlations
        // REFERENCES:
        //  T.Fujii, and H.Imura,Natural convection heat transfer from aplate with arbitrary inclination.
        //  International Journal of Heat and Mass Transfer: 15(4), (1972), 755-764.

        Real64 hConvA2W; // convection coefficient, [W/m2K]

        Real64 const gravity(9.806); // gravitational constant [m/s^2]
        static std::string const CalledFrom("SolarCollectors:CalcConvCoeffAbsPlateAndWater");

        Real64 DeltaT = std::abs(TAbsorber - TWater);
        Real64 TReference = TAbsorber - 0.25 * (TAbsorber - TWater);
        // record fluid prop index for water
        int WaterIndex = FluidProperties::FindGlycol(state, fluidNameWater);
        // find properties of water - always assume water
        Real64 WaterSpecHeat = FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, max(TReference, 0.0), WaterIndex, CalledFrom);
        Real64 CondOfWater = FluidProperties::GetConductivityGlycol(state, fluidNameWater, max(TReference, 0.0), WaterIndex, CalledFrom);
        Real64 VisOfWater = FluidProperties::GetViscosityGlycol(state, fluidNameWater, max(TReference, 0.0), WaterIndex, CalledFrom);
        Real64 DensOfWater = FluidProperties::GetDensityGlycol(state, fluidNameWater, max(TReference, 0.0), WaterIndex, CalledFrom);
        Real64 PrOfWater = VisOfWater * WaterSpecHeat / CondOfWater;
        // Requires a different reference temperature for volumetric expansion coefficient
        TReference = TWater - 0.25 * (TWater - TAbsorber);
        Real64 VolExpWater = -(FluidProperties::GetDensityGlycol(state, fluidNameWater, max(TReference, 10.0) + 5.0, WaterIndex, CalledFrom) -
                               FluidProperties::GetDensityGlycol(state, fluidNameWater, max(TReference, 10.0) - 5.0, WaterIndex, CalledFrom)) /
                             (10.0 * DensOfWater);

        // Grashof number
        Real64 GrNum = gravity * VolExpWater * DensOfWater * DensOfWater * PrOfWater * DeltaT * pow_3(Lc) / pow_2(VisOfWater);
        Real64 CosTilt = std::cos(TiltR2V * DataGlobalConstants::DegToRadians());

        Real64 RaNum; // Raleigh number
        Real64 NuL;   // Nusselt number
        if (TAbsorber > TWater) {
            // hot absorber plate facing down
            if (std::abs(TiltR2V - 90.0) < 1.0) {
                // It is a horizontal surface
                RaNum = GrNum * PrOfWater;
                if (RaNum <= 1708.0) {
                    NuL = 1.0;
                } else {
                    NuL = 0.58 * std::pow(RaNum, 0.20);
                }
            } else {
                RaNum = GrNum * PrOfWater * CosTilt;
                if (RaNum <= 1708.0) {
                    NuL = 1.0;
                } else {
                    NuL = 0.56 * root_4(RaNum);
                }
            }
        } else {
            // cold plate facing down or hot plate facing up
            RaNum = GrNum * PrOfWater;
            if (RaNum > 5.0e8) {
                NuL = 0.13 * std::pow(RaNum, 1.0 / 3.0);
            } else {
                NuL = 0.16 * std::pow(RaNum, 1.0 / 3.0);
                if (RaNum <= 1708.0) NuL = 1.0;
            }
        }
        hConvA2W = NuL * CondOfWater / Lc;

        return hConvA2W;
    }

    void CollectorData::update(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004

        // PURPOSE OF THIS SUBROUTINE:
        // Updates the node variables with local variables.

        static std::string const RoutineName("UpdateSolarCollector");

        PlantUtilities::SafeCopyPlantNode(this->InletNode, this->OutletNode);
        // Set outlet node variables that are possibly changed
        DataLoopNode::Node(this->OutletNode).Temp = this->OutletTemp;
        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            state, DataPlant::PlantLoop(this->WLoopNum).FluidName, this->OutletTemp, DataPlant::PlantLoop(this->WLoopNum).FluidIndex, RoutineName);
        DataLoopNode::Node(this->OutletNode).Enthalpy = Cp * DataLoopNode::Node(this->OutletNode).Temp;
    }

    void CollectorData::report()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables.

        Real64 TimeStepInSecond = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        this->Energy = this->Power * TimeStepInSecond;
        this->HeatEnergy = this->HeatRate * TimeStepInSecond;
        this->HeatGainEnergy = this->HeatGainRate * TimeStepInSecond;
        this->HeatLossEnergy = this->HeatLossRate * TimeStepInSecond;
        this->CollHeatLossEnergy = this->SkinHeatLossRate * TimeStepInSecond;
        this->StoredHeatEnergy = this->StoredHeatRate * TimeStepInSecond;
    }

    void CollectorData::GetExtVentedCavityIndex(int const SurfacePtr, int &VentCavIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC. Adopted from Photovoltaics module
        //       DATE WRITTEN   February 2012

        // PURPOSE OF THIS SUBROUTINE:
        // object oriented "Get" routine for establishing correct integer index from outside this module

        // METHODOLOGY EMPLOYED:
        // mine Surface derived type for correct index/number of surface
        // mine  ExtVentedCavity derived type that has the surface.
        // Adapted from Photovoltaics module, originally developed by Brent G. (2004)

        bool Found;

        if (SurfacePtr == 0) {
            // should be trapped already
            ShowFatalError("Invalid surface passed to GetExtVentedCavityIndex");
        }

        int CavNum = 0;
        Found = false;
        for (int thisCav = 1; thisCav <= DataSurfaces::TotExtVentCav; ++thisCav) {
            for (int ThisSurf = 1; ThisSurf <= DataSurfaces::ExtVentedCavity(thisCav).NumSurfs; ++ThisSurf) {
                if (SurfacePtr == DataSurfaces::ExtVentedCavity(thisCav).SurfPtrs(ThisSurf)) {
                    Found = true;
                    CavNum = thisCav;
                }
            }
        }

        if (!Found) {
            ShowFatalError("Did not find surface in Exterior Vented Cavity description in GetExtVentedCavityIndex, Surface name = " +
                           DataSurfaces::Surface(SurfacePtr).Name);
        } else {

            VentCavIndex = CavNum;
        }
    }

} // namespace SolarCollectors

} // namespace EnergyPlus
