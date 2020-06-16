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
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidCoolers.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace FluidCoolers {

    // Module containing the routines dealing with the objects FluidCooler:SingleSpeed and
    // FluidCooler:TwoSpeed

    // MODULE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       April 2010, Chandan Sharma, FSEC
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Model the performance of fluid coolers

    // REFERENCES:
    // Based on cooling tower by Shirey, Raustad: Dec 2000; Shirey, Sept 2002

    // MODULE PARAMETER DEFINITIONS:
    std::string const cFluidCooler_SingleSpeed("FluidCooler:SingleSpeed");
    std::string const cFluidCooler_TwoSpeed("FluidCooler:TwoSpeed");
    static std::string const BlankString;

    bool GetFluidCoolerInputFlag(true);
    int NumSimpleFluidCoolers(0); // Number of similar fluid coolers

    // Object Data
    Array1D<FluidCoolerspecs> SimpleFluidCooler; // dimension to number of machines
    std::unordered_map<std::string, std::string> UniqueSimpleFluidCoolerNames;

    PlantComponent *FluidCoolerspecs::factory(int objectType, std::string objectName)
    {
        if (GetFluidCoolerInputFlag) {
            GetFluidCoolerInput();
            GetFluidCoolerInputFlag = false;
        }
        // Now look for this particular fluid cooler in the list
        for (auto &fc : SimpleFluidCooler) {
            if (fc.FluidCoolerType_Num == objectType && fc.Name == objectName) {
                return &fc;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("FluidCooler::factory: Error getting inputs for cooler named: " + objectName);
        // Shut up the compiler
        return nullptr;
    }

    void FluidCoolerspecs::simulate(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation),
                                    bool const EP_UNUSED(FirstHVACIteration),
                                    Real64 &EP_UNUSED(CurLoad),
                                    bool const RunFlag)
    {
        this->initialize(state.dataBranchInputManager);
        if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
            this->calcSingleSpeed();
        } else {
            this->calcTwoSpeed();
        }
        this->update();
        this->report(RunFlag);
    }

    void FluidCoolerspecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation))
    {
        this->initialize(state.dataBranchInputManager);
        this->size();
    }

    void FluidCoolerspecs::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MaxLoad = this->FluidCoolerNominalCapacity;
        OptLoad = this->FluidCoolerNominalCapacity;
        MinLoad = 0.0;
    }

    void GetFluidCoolerInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Chandan Sharma
        //       DATE WRITTEN:    August 2008
        //       MODIFIED         Chandan Sharma, FSEC, April 2010
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for fluid coolers and stores it in SimpleFluidCooler data structure.

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in the data.

        // REFERENCES:
        // Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;                // Number of elements in the alpha array
        int NumNums;                  // Number of elements in the numeric array
        int IOStat;                   // IO Status when calling get input subroutine
        bool ErrorsFound(false);      // Logical flag set .TRUE. if errors found while getting input data
        Array1D<Real64> NumArray(16); // Numeric input data array
        Array1D_string AlphArray(5);  // Character string input data array

        // Get number of all Fluid Coolers specified in the input data file (idf)
        int const NumSingleSpeedFluidCoolers = inputProcessor->getNumObjectsFound("FluidCooler:SingleSpeed");
        int const NumTwoSpeedFluidCoolers = inputProcessor->getNumObjectsFound("FluidCooler:TwoSpeed");
        NumSimpleFluidCoolers = NumSingleSpeedFluidCoolers + NumTwoSpeedFluidCoolers;

        if (NumSimpleFluidCoolers <= 0)
            ShowFatalError("No fluid cooler objects found in input, however, a branch object has specified a fluid cooler. Search the input for "
                           "fluid cooler to determine the cause for this error.");

        // See if load distribution manager has already gotten the input
        if (allocated(SimpleFluidCooler)) return;
        GetFluidCoolerInputFlag = false;

        // Allocate data structures to hold fluid cooler input data, report data and fluid cooler inlet conditions
        SimpleFluidCooler.allocate(NumSimpleFluidCoolers);
        UniqueSimpleFluidCoolerNames.reserve(NumSimpleFluidCoolers);

        int FluidCoolerNum;

        // Load data structures with fluid cooler input data
        cCurrentModuleObject = cFluidCooler_SingleSpeed;
        for (int SingleSpeedFluidCoolerNumber = 1; SingleSpeedFluidCoolerNumber <= NumSingleSpeedFluidCoolers; ++SingleSpeedFluidCoolerNumber) {
            FluidCoolerNum = SingleSpeedFluidCoolerNumber;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          SingleSpeedFluidCoolerNumber,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(
                UniqueSimpleFluidCoolerNames, AlphArray(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            SimpleFluidCooler(FluidCoolerNum).Name = AlphArray(1);
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerType = cCurrentModuleObject;
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerType_Num = DataPlant::TypeOf_FluidCooler_SingleSpd;
            SimpleFluidCooler(FluidCoolerNum).indexInArray = FluidCoolerNum;
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerMassFlowRateMultiplier = 2.5;
            SimpleFluidCooler(FluidCoolerNum).WaterInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                                      ErrorsFound,
                                                                                                      cCurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeType_Water,
                                                                                                      DataLoopNode::NodeConnectionType_Inlet,
                                                                                                      1,
                                                                                                      DataLoopNode::ObjectIsNotParent);
            SimpleFluidCooler(FluidCoolerNum).WaterOutletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(3),
                                                                                                       ErrorsFound,
                                                                                                       cCurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeType_Water,
                                                                                                       DataLoopNode::NodeConnectionType_Outlet,
                                                                                                       1,
                                                                                                       DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");
            SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA = NumArray(1);
            if (SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUAWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerNominalCapacity = NumArray(2);
            SimpleFluidCooler(FluidCoolerNum).DesignEnteringWaterTemp = NumArray(3);
            SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirTemp = NumArray(4);
            SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirWetBulbTemp = NumArray(5);
            SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate = NumArray(6);
            if (SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate = NumArray(7);
            if (SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower = NumArray(8);
            if (SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPowerWasAutoSized = true;
            }

            //   outdoor air inlet node
            if (AlphArray(5).empty()) {
                SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum = 0;
            } else {
                SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(AlphArray(5),
                                                        ErrorsFound,
                                                        cCurrentModuleObject,
                                                        SimpleFluidCooler(FluidCoolerNum).Name,
                                                        DataLoopNode::NodeType_Air,
                                                        DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(cCurrentModuleObject + "= \"" + SimpleFluidCooler(FluidCoolerNum).Name + "\" " + cAlphaFieldNames(5) + "= \"" +
                                    AlphArray(5) + "\" not valid.");
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            ErrorsFound |=
                SimpleFluidCooler(FluidCoolerNum).validateSingleSpeedInputs(cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames);

        } // End Single-Speed fluid cooler Loop

        cCurrentModuleObject = cFluidCooler_TwoSpeed;
        for (int TwoSpeedFluidCoolerNumber = 1; TwoSpeedFluidCoolerNumber <= NumTwoSpeedFluidCoolers; ++TwoSpeedFluidCoolerNumber) {
            FluidCoolerNum = NumSingleSpeedFluidCoolers + TwoSpeedFluidCoolerNumber;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          TwoSpeedFluidCoolerNumber,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(
                UniqueSimpleFluidCoolerNames, AlphArray(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            SimpleFluidCooler(FluidCoolerNum).Name = AlphArray(1);
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerType = cCurrentModuleObject;
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerType_Num = DataPlant::TypeOf_FluidCooler_TwoSpd;
            SimpleFluidCooler(FluidCoolerNum).indexInArray = FluidCoolerNum;
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerMassFlowRateMultiplier = 2.5;
            SimpleFluidCooler(FluidCoolerNum).WaterInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(2),
                                                                                                      ErrorsFound,
                                                                                                      cCurrentModuleObject,
                                                                                                      AlphArray(1),
                                                                                                      DataLoopNode::NodeType_Water,
                                                                                                      DataLoopNode::NodeConnectionType_Inlet,
                                                                                                      1,
                                                                                                      DataLoopNode::ObjectIsNotParent);
            SimpleFluidCooler(FluidCoolerNum).WaterOutletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(3),
                                                                                                       ErrorsFound,
                                                                                                       cCurrentModuleObject,
                                                                                                       AlphArray(1),
                                                                                                       DataLoopNode::NodeType_Water,
                                                                                                       DataLoopNode::NodeConnectionType_Outlet,
                                                                                                       1,
                                                                                                       DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA = NumArray(1);
            if (SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUAWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUA = NumArray(2);
            if (SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUA == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUAWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUASizingFactor = NumArray(3);
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerNominalCapacity = NumArray(4);
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCap = NumArray(5);
            if (SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCap == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCapWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCapSizingFactor = NumArray(6);
            SimpleFluidCooler(FluidCoolerNum).DesignEnteringWaterTemp = NumArray(7);
            SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirTemp = NumArray(8);
            SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirWetBulbTemp = NumArray(9);
            SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate = NumArray(10);
            if (SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate = NumArray(11);
            if (SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower = NumArray(12);
            if (SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRate = NumArray(13);
            if (SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRateWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRateSizingFactor = NumArray(14);
            SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPower = NumArray(15);
            if (SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPower == DataSizing::AutoSize) {
                SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPowerWasAutoSized = true;
            }
            SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPowerSizingFactor = NumArray(16);

            //   outdoor air inlet node
            if (AlphArray(5).empty()) {
                SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum = 0;
            } else {
                SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(AlphArray(5),
                                                        ErrorsFound,
                                                        cCurrentModuleObject,
                                                        SimpleFluidCooler(FluidCoolerNum).Name,
                                                        DataLoopNode::NodeType_Air,
                                                        DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(cCurrentModuleObject + "= \"" + SimpleFluidCooler(FluidCoolerNum).Name + "\" " + cAlphaFieldNames(5) + "= \"" +
                                    AlphArray(5) + "\" not valid.");
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            ErrorsFound |=
                SimpleFluidCooler(FluidCoolerNum).validateTwoSpeedInputs(cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames);
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in getting fluid cooler input.");
        }
    }

    void FluidCoolerspecs::setupOutputVars()
    {

        SetupOutputVariable("Cooling Tower Inlet Temperature", OutputProcessor::Unit::C, this->InletWaterTemp, "System", "Average", this->Name);
        SetupOutputVariable("Cooling Tower Outlet Temperature", OutputProcessor::Unit::C, this->OutletWaterTemp, "System", "Average", this->Name);
        SetupOutputVariable("Cooling Tower Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Cooling Tower Heat Transfer Rate", OutputProcessor::Unit::W, this->Qactual, "System", "Average", this->Name);
        SetupOutputVariable("Cooling Tower Fan Electric Power", OutputProcessor::Unit::W, this->FanPower, "System", "Average", this->Name);
        SetupOutputVariable("Cooling Tower Fan Electric Energy",
                            OutputProcessor::Unit::J,
                            this->FanEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "Electric",
                            "HeatRejection",
                            _,
                            "Plant");
    }

    bool FluidCoolerspecs::validateSingleSpeedInputs(std::string const &cCurrentModuleObject,
                                                     Array1D<std::string> const &AlphArray,
                                                     Array1D<std::string> const &cNumericFieldNames,
                                                     Array1D<std::string> const &cAlphaFieldNames)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR:          Chandan Sharma
        //       DATE WRITTEN:    August 2008
        //       MODIFIED         Chandan Sharma, FSEC, April 2010
        //       RE-ENGINEERED    Jason Glazer, GARD Analytics, February 2015, refactor into a separate function

        // PURPOSE OF THIS FUNCTION:
        // Separate the testing of inputs related to design so that it could be called from the unit tests

        // REFERENCES:
        // Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound = false;

        //   Design entering water temperature, design entering air temperature and design entering air
        //   wetbulb temperature must be specified for the both the performance input methods
        if (this->DesignEnteringWaterTemp <= 0.0) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(3) +
                            "\", entered value <= 0.0, but must be > 0 ");
            ErrorsFound = true;
        }
        if (this->DesignEnteringAirTemp <= 0.0) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(4) +
                            "\", entered value <= 0.0, but must be > 0 ");
            ErrorsFound = true;
        }
        if (this->DesignEnteringAirWetBulbTemp <= 0.0) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(5) +
                            "\", entered value <= 0.0, but must be > 0 ");
            ErrorsFound = true;
        }
        if (this->DesignEnteringWaterTemp <= this->DesignEnteringAirTemp) {
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\"," + cNumericFieldNames(3) + " must be greater than " +
                            cNumericFieldNames(4) + '.');
            ErrorsFound = true;
        }
        if (this->DesignEnteringAirTemp <= this->DesignEnteringAirWetBulbTemp) {
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\"," + cNumericFieldNames(4) + " must be greater than " +
                            cNumericFieldNames(5) + '.');
            ErrorsFound = true;
        }
        if (this->HighSpeedAirFlowRate <= 0.0 && this->HighSpeedAirFlowRate != DataSizing::AutoSize) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(7) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->DesignWaterFlowRate <= 0.0 && !this->DesignWaterFlowRateWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(6) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->HighSpeedFanPower <= 0.0 && this->HighSpeedFanPower != DataSizing::AutoSize) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(8) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }

        //   Check various inputs for both the performance input methods
        if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
            this->PerformanceInputMethod_Num = PerfInputMethod::U_FACTOR;
            if (this->HighSpeedFluidCoolerUA <= 0.0 && this->HighSpeedFluidCoolerUA != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(1) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
            this->PerformanceInputMethod_Num = PerfInputMethod::NOMINAL_CAPACITY;
            if (this->FluidCoolerNominalCapacity <= 0.0) {
                ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(2) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
                ErrorsFound = true;
            }
            if (this->HighSpeedFluidCoolerUA != 0.0) {
                if (this->HighSpeedFluidCoolerUA > 0.0) {
                    ShowWarningError(cCurrentModuleObject + "= \"" + this->Name +
                                     "\". Nominal fluid cooler capacity and design fluid cooler UA have been specified.");
                } else {
                    ShowWarningError(cCurrentModuleObject + "= \"" + this->Name +
                                     "\". Nominal fluid cooler capacity has been specified and design fluid cooler UA is being autosized.");
                }
                ShowContinueError(
                    "Design fluid cooler UA field must be left blank when nominal fluid cooler capacity performance input method is used.");
                ShowContinueError("Design fluid cooler UA value will be reset to zero and the simulation continuous.");
                this->HighSpeedFluidCoolerUA = 0.0;
            }
        } else { // Fluid cooler performance input method is not specified as a valid "choice"
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ShowContinueError(R"(... must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
            ErrorsFound = true;
        }
        return ErrorsFound;
    }

    bool FluidCoolerspecs::validateTwoSpeedInputs(std::string const &cCurrentModuleObject,
                                                  Array1D<std::string> const &AlphArray,
                                                  Array1D<std::string> const &cNumericFieldNames,
                                                  Array1D<std::string> const &cAlphaFieldNames)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR:          Chandan Sharma
        //       DATE WRITTEN:    August 2008
        //       MODIFIED         Chandan Sharma, FSEC, April 2010
        //       RE-ENGINEERED    Jason Glazer, GARD Analytics, February 2015, refactor into a separate function

        // PURPOSE OF THIS FUNCTION:
        // Separate the testing of inputs related to design so that it could be called from the unit tests

        // REFERENCES:
        // Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound = false;

        //   Design entering water temperature, design entering air temperature and design entering air
        //   wetbulb temperature must be specified for the both the performance input methods
        if (this->DesignEnteringWaterTemp <= 0.0) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(7) +
                            "\", entered value <= 0.0, but must be > 0 ");
            ErrorsFound = true;
        }
        if (this->DesignEnteringAirTemp <= 0.0) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(8) +
                            "\", entered value <= 0.0, but must be > 0 ");
            ErrorsFound = true;
        }
        if (this->DesignEnteringAirWetBulbTemp <= 0.0) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(9) +
                            "\", entered value <= 0.0, but must be > 0 ");
            ErrorsFound = true;
        }
        if (this->DesignEnteringWaterTemp <= this->DesignEnteringAirTemp) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", " + cNumericFieldNames(7) + " must be greater than " +
                            cNumericFieldNames(8) + '.');
            ErrorsFound = true;
        }
        if (this->DesignEnteringAirTemp <= this->DesignEnteringAirWetBulbTemp) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", " + cNumericFieldNames(8) + " must be greater than " +
                            cNumericFieldNames(9) + '.');
            ErrorsFound = true;
        }

        //   Check various inputs for both the performance input methods
        if (this->DesignWaterFlowRate <= 0.0 && !this->DesignWaterFlowRateWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(10) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->HighSpeedAirFlowRate <= 0.0 && !this->HighSpeedAirFlowRateWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(11) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->LowSpeedAirFlowRate <= 0.0 && !this->LowSpeedAirFlowRateWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(13) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        //   High speed air flow rate must be greater than low speed air flow rate.
        //   Can't tell yet if autosized, check later in InitFluidCooler.
        if (this->HighSpeedAirFlowRate <= this->LowSpeedAirFlowRate && !this->HighSpeedAirFlowRateWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                            "\". Fluid cooler air flow rate at low fan speed must be less than the air flow rate at high fan speed.");
            ErrorsFound = true;
        }
        if (this->HighSpeedFanPower <= 0.0 && !this->HighSpeedFanPowerWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(12) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->LowSpeedFanPower <= 0.0 && !this->LowSpeedFanPowerWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(15) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->HighSpeedFanPower <= this->LowSpeedFanPower && !this->HighSpeedFanPowerWasAutoSized) {
            ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                            "\". Fluid cooler low speed fan power must be less than high speed fan power.");
            ErrorsFound = true;
        }

        if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
            this->PerformanceInputMethod_Num = PerfInputMethod::U_FACTOR;
            if (this->HighSpeedFluidCoolerUA <= 0.0 && !this->HighSpeedFluidCoolerUAWasAutoSized) {
                ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(1) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
                ErrorsFound = true;
            }
            if (this->LowSpeedFluidCoolerUA <= 0.0 && !this->LowSpeedFluidCoolerUAWasAutoSized) {
                ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(2) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
                ErrorsFound = true;
            }
            if (this->HighSpeedFluidCoolerUA <= this->LowSpeedFluidCoolerUA && !this->HighSpeedFluidCoolerUAWasAutoSized) {
                ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                                "\". Fluid cooler UA at low fan speed must be less than the fluid cooler UA at high fan speed.");
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
            this->PerformanceInputMethod_Num = PerfInputMethod::NOMINAL_CAPACITY;
            if (this->FluidCoolerNominalCapacity <= 0.0) {
                ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(4) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
                ErrorsFound = true;
            }
            if (this->FluidCoolerLowSpeedNomCap <= 0.0 && !this->FluidCoolerLowSpeedNomCapWasAutoSized) {
                ShowSevereError(cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(5) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
                ErrorsFound = true;
            }
            if (this->HighSpeedFluidCoolerUA != 0.0) {
                if (this->HighSpeedFluidCoolerUA > 0.0) {
                    ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method and fluid cooler UA at high fan speed have been specified.");
                } else {
                    ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method has been specified and fluid cooler UA at high fan speed is being autosized.");
                }
                ShowContinueError(
                    "Fluid cooler UA at high fan speed must be left blank when nominal fluid cooler capacity performance input method is used.");
                ErrorsFound = true;
            }
            if (this->LowSpeedFluidCoolerUA != 0.0) {
                if (this->LowSpeedFluidCoolerUA > 0.0) {
                    ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method and fluid cooler UA at low fan speed have been specified.");
                } else {
                    ShowSevereError(cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method has been specified and fluid cooler UA at low fan speed is being autosized.");
                }
                ShowContinueError(
                    "Fluid cooler UA at low fan speed must be left blank when nominal fluid cooler capacity performance input method is used.");
                ErrorsFound = true;
            }
            if (this->FluidCoolerLowSpeedNomCap >= this->FluidCoolerNominalCapacity) {
                ShowSevereError(cCurrentModuleObject + " = \"" + this->Name +
                                "\". Low-speed nominal capacity must be less than the high-speed nominal capacity.");
                ErrorsFound = true;
            }
        } else { // Fluid cooler performance input method is not specified as a valid "choice"
            ShowSevereError(cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
            ShowContinueError(R"(... must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
            ErrorsFound = true;
        }
        return ErrorsFound;
    }

    void FluidCoolerspecs::initialize(BranchInputManagerData &data)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the fluid cooler components and for
        // final checking of fluid cooler inputs (post autosizing)

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // Based on InitTower subroutine by Don Shirey Sept/Oct 2002, F Buhl Oct 2002

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitFluidCooler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // Flag if input data errors are found

        if (this->oneTimeInit) {

            this->setupOutputVars();

            // Locate the tower on the plant loops for later usage
            PlantUtilities::ScanPlantLoopsForObject(data,
                this->Name, this->FluidCoolerType_Num, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, ErrorsFound, _, _, _, _, _);

            if (ErrorsFound) {
                ShowFatalError("InitFluidCooler: Program terminated due to previous condition(s).");
            }

            this->oneTimeInit = false;
        }

        // Begin environment initializations
        if (this->beginEnvrnInit && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 const rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                 DataGlobals::InitConvTemp,
                                                                 DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                 RoutineName);
            this->DesWaterMassFlowRate = this->DesignWaterFlowRate * rho;
            PlantUtilities::InitComponentNodes(0.0,
                                               this->DesWaterMassFlowRate,
                                               this->WaterInletNodeNum,
                                               this->WaterOutletNodeNum,
                                               this->LoopNum,
                                               this->LoopSideNum,
                                               this->BranchNum,
                                               this->CompNum);
            this->beginEnvrnInit = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->beginEnvrnInit = true;
        }

        // Each time initializations
        this->WaterTemp = DataLoopNode::Node(this->WaterInletNodeNum).Temp;

        if (this->OutdoorAirInletNodeNum != 0) {
            this->AirTemp = DataLoopNode::Node(this->OutdoorAirInletNodeNum).Temp;
            this->AirHumRat = DataLoopNode::Node(this->OutdoorAirInletNodeNum).HumRat;
            this->AirPress = DataLoopNode::Node(this->OutdoorAirInletNodeNum).Press;
            this->AirWetBulb = DataLoopNode::Node(this->OutdoorAirInletNodeNum).OutAirWetBulb;
        } else {
            this->AirTemp = DataEnvironment::OutDryBulbTemp;
            this->AirHumRat = DataEnvironment::OutHumRat;
            this->AirPress = DataEnvironment::OutBaroPress;
            this->AirWetBulb = DataEnvironment::OutWetBulbTemp;
        }

        this->WaterMassFlowRate = PlantUtilities::RegulateCondenserCompFlowReqOp(
            this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, this->DesWaterMassFlowRate * this->FluidCoolerMassFlowRateMultiplier);

        PlantUtilities::SetComponentFlowRate(this->WaterMassFlowRate,
                                             this->WaterInletNodeNum,
                                             this->WaterOutletNodeNum,
                                             this->LoopNum,
                                             this->LoopSideNum,
                                             this->BranchNum,
                                             this->CompNum);
    }

    void FluidCoolerspecs::size()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2010, Chandan Sharma, FSEC
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing fluid cooler Components for which capacities and flow rates
        // have not been specified in the input. This subroutine also calculates fluid cooler UA if the user
        // has specified fluid cooler performance via the "Nominal Capacity" method.

        // METHODOLOGY EMPLOYED:
        // Obtains condenser flow rate from the plant sizing array. If fluid cooler performance is specified
        // via the "Nominal Capacity" method, the water flow rate is directly proportional to capacity.

        // REFERENCES:
        // Based on SizeTower by Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static std::string const CalledFrom("SizeFluidCooler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SolFla;                     // Flag of solver
        Real64 DesFluidCoolerLoad(0.0); // Design fluid cooler load [W]
        Real64 UA0;                     // Lower bound for UA [W/C]
        Real64 UA1;                     // Upper bound for UA [W/C]
        Real64 UA;                      // Calculated UA value
        Real64 OutWaterTempAtUA0;       // Water outlet temperature at UA0
        Real64 OutWaterTempAtUA1;       // Water outlet temperature at UA1
        Array1D<Real64> Par(5);         // Parameter array need for RegulaFalsi routine
        std::string equipName;
        Real64 Cp;                            // local specific heat for fluid
        Real64 rho;                           // local density for fluid
        Real64 tmpHighSpeedFanPower;          // local temporary for high speed fan power
        Real64 tmpHighSpeedEvapFluidCoolerUA; // local temporary for high speed cooler UA
        bool ErrorsFound;

        Real64 tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
        Real64 tmpHighSpeedAirFlowRate = this->HighSpeedAirFlowRate;
        // Find the appropriate Plant Sizing object
        int PltSizCondNum = DataPlant::PlantLoop(this->LoopNum).PlantSizNum;

        if (this->DesignWaterFlowRateWasAutoSized) {
            if (PltSizCondNum > 0) {
                if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    tmpDesignWaterFlowRate = DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                } else {
                    tmpDesignWaterFlowRate = 0.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                    }
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing error for fluid cooler object = " + this->Name);
                    ShowFatalError("Autosizing of fluid cooler condenser flow rate requires a loop Sizing:Plant object.");
                }
            }
            // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
            // temperature is less than design inlet air dry bulb temperature
            if (DataSizing::PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                ShowContinueError("Design Loop Exit Temperature (" + General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2) +
                                  " C) must be greater than design entering air dry-bulb temperature (" +
                                  General::RoundSigDigits(this->DesignEnteringAirTemp, 2) + " C) when autosizing the fluid cooler UA.");
                ShowContinueError("It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the Fluid Cooler "
                                  "design approach temperature (e.g., 4 C).");
                ShowContinueError("If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be "
                                  "> design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                ShowFatalError("Review and revise design input values as appropriate.");
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->WaterInletNodeNum, tmpDesignWaterFlowRate);

        if (this->PerformanceInputMethod_Num == PerfInputMethod::U_FACTOR && this->HighSpeedFluidCoolerUAWasAutoSized) {
            if (PltSizCondNum > 0) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                        DataGlobals::InitConvTemp,
                                                        DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                            DataSizing::PlantSizData(PltSizCondNum).ExitTemp,
                                                            DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->FluidCoolerNominalCapacity = DesFluidCoolerLoad;
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->FluidCoolerNominalCapacity = 0.0;
            }
        }

        if (this->HighSpeedFanPowerWasAutoSized) {
            // We assume the nominal fan power is 0.0105 times the design load
            if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY) {
                tmpHighSpeedFanPower = 0.0105 * this->FluidCoolerNominalCapacity;
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
            } else {
                if (DesFluidCoolerLoad > 0.0) {
                    tmpHighSpeedFanPower = 0.0105 * DesFluidCoolerLoad;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                } else if (PltSizCondNum > 0) {
                    if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
                        // temperature is less than design inlet air dry bulb temperature
                        if (DataSizing::PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp &&
                            DataPlant::PlantFirstSizesOkayToFinalize) {
                            ShowSevereError("Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                            ShowContinueError("Design Loop Exit Temperature (" +
                                              General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2) +
                                              " C) must be greater than design entering air dry-bulb temperature (" +
                                              General::RoundSigDigits(this->DesignEnteringAirTemp, 2) + " C) when autosizing the fluid cooler UA.");
                            ShowContinueError("It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the "
                                              "Fluid Cooler design approach temperature (e.g., 4 C).");
                            ShowContinueError("If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design "
                                              "Setpoint must be > design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                            ShowFatalError("Review and revise design input values as appropriate.");
                        }
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                DataGlobals::InitConvTemp,
                                                                DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                CalledFrom);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                    DataSizing::PlantSizData(PltSizCondNum).ExitTemp,
                                                                    DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                    CalledFrom);
                        DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                        tmpHighSpeedFanPower = 0.0105 * DesFluidCoolerLoad;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                    } else {
                        tmpHighSpeedFanPower = 0.0;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                    }
                } else {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        ShowSevereError("Autosizing of fluid cooler fan power requires a loop Sizing:Plant object.");
                        ShowFatalError(" Occurs in fluid cooler object = " + this->Name);
                    }
                }
            }
            if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Initial Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower);
                    }
                }
            } else if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Fan Power at High Fan Speed [W]", this->HighSpeedFanPower);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Initial Fan Power at High Fan Speed [W]", this->HighSpeedFanPower);
                    }
                }
            }
        }

        if (this->HighSpeedAirFlowRateWasAutoSized) {
            if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY) {
                tmpHighSpeedAirFlowRate = this->FluidCoolerNominalCapacity / (this->DesignEnteringWaterTemp - this->DesignEnteringAirTemp) * 4.0;
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
            } else {
                if (DesFluidCoolerLoad > 0.0) {
                    tmpHighSpeedAirFlowRate = DesFluidCoolerLoad / (this->DesignEnteringWaterTemp - this->DesignEnteringAirTemp) * 4.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
                } else if (PltSizCondNum > 0) {
                    if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
                        // temperature is less than design inlet air dry bulb temperature
                        if (DataSizing::PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp &&
                            DataPlant::PlantFirstSizesOkayToFinalize) {
                            ShowSevereError("Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                            ShowContinueError("Design Loop Exit Temperature (" +
                                              General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2) +
                                              " C) must be greater than design entering air dry-bulb temperature (" +
                                              General::RoundSigDigits(this->DesignEnteringAirTemp, 2) + " C) when autosizing the fluid cooler UA.");
                            ShowContinueError("It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the "
                                              "Fluid Cooler design approach temperature (e.g., 4 C).");
                            ShowContinueError("If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design "
                                              "Setpoint must be > design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                            ShowFatalError("Review and revise design input values as appropriate.");
                        }
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                DataGlobals::InitConvTemp,
                                                                DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                CalledFrom);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                    DataSizing::PlantSizData(PltSizCondNum).ExitTemp,
                                                                    DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                    CalledFrom);
                        DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                        tmpHighSpeedAirFlowRate = DesFluidCoolerLoad / (this->DesignEnteringWaterTemp - this->DesignEnteringAirTemp) * 4.0;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
                    } else {
                        tmpHighSpeedAirFlowRate = 0.0;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
                    }
                } else {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        ShowSevereError("Autosizing of fluid cooler air flow rate requires a loop Sizing:Plant object");
                        ShowFatalError(" Occurs in fluid cooler object = " + this->Name);
                    }
                }
            }
            if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                    }
                }
            } else if (this->FluidCoolerType == "FluidCooler:TwoSpeed") {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType, this->Name, "Initial Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate);
                    }
                }
            }
        }

        if (this->HighSpeedFluidCoolerUAWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
            if (PltSizCondNum > 0) {
                if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
                    // temperature is less than design inlet air dry bulb temperature
                    if (DataSizing::PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp && DataPlant::PlantFirstSizesOkayToFinalize) {
                        ShowSevereError("Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                        ShowContinueError("Design Loop Exit Temperature (" +
                                          General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2) +
                                          " C) must be greater than design entering air dry-bulb temperature (" +
                                          General::RoundSigDigits(this->DesignEnteringAirTemp, 2) + " C) when autosizing the fluid cooler UA.");
                        ShowContinueError("It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the Fluid "
                                          "Cooler design approach temperature (e.g., 4 C).");
                        ShowContinueError("If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint "
                                          "must be > design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                        ShowFatalError("Review and revise design input values as appropriate.");
                    }
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                            DataGlobals::InitConvTemp,
                                                            DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                DataSizing::PlantSizData(PltSizCondNum).ExitTemp,
                                                                DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                CalledFrom);
                    DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                    Par(1) = DesFluidCoolerLoad;
                    Par(2) = double(this->indexInArray);
                    Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par(4) = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                    Par(5) = Cp;
                    UA0 = 0.0001 * DesFluidCoolerLoad; // Assume deltaT = 10000K (limit)
                    UA1 = DesFluidCoolerLoad;          // Assume deltaT = 1K
                    this->WaterTemp = DataSizing::PlantSizData(PltSizCondNum).ExitTemp + DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                    this->AirTemp = this->DesignEnteringAirTemp;
                    this->AirWetBulb = this->DesignEnteringAirWetBulbTemp;
                    this->AirPress = DataEnvironment::StdBaroPress;
                    this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(this->AirTemp, this->AirWetBulb, this->AirPress, CalledFrom);
                    General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowWarningError("Iteration limit exceeded in calculating fluid cooler UA.");
                        ShowContinueError("Autosizing of fluid cooler UA failed for fluid cooler = " + this->Name);
                        ShowContinueError("The final UA value =" + General::RoundSigDigits(UA, 2) + " W/K, and the simulation continues...");
                    } else if (SolFla == -2) {
                        CalcFluidCoolerOutlet(int(Par(2)), Par(3), Par(4), UA0, OutWaterTempAtUA0);
                        CalcFluidCoolerOutlet(int(Par(2)), Par(3), Par(4), UA1, OutWaterTempAtUA1);
                        ShowSevereError(CalledFrom + ": The combination of design input values did not allow the calculation of a ");
                        ShowContinueError("reasonable UA value. Review and revise design input values as appropriate. Specifying hard");
                        ShowContinueError(R"(sizes for some "autosizable" fields while autosizing other "autosizable" fields may be )");
                        ShowContinueError("contributing to this problem.");
                        ShowContinueError("This model iterates on UA to find the heat transfer required to provide the design outlet ");
                        ShowContinueError("water temperature. Initially, the outlet water temperatures at high and low UA values are ");
                        ShowContinueError("calculated. The Design Exit Water Temperature should be between the outlet water ");
                        ShowContinueError("temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ");
                        ShowContinueError("out of this range, the solution will not converge and UA will not be calculated. ");
                        ShowContinueError("The possible solutions could be to manually input adjusted water and/or air flow rates based ");
                        ShowContinueError("on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb temperature.");
                        ShowContinueError("Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).");
                        ShowContinueError("Inputs to the fluid cooler object:");
                        ShowContinueError("Design Fluid Cooler Load [W]                       = " + General::RoundSigDigits(Par(1), 2));
                        ShowContinueError("Design Fluid Cooler Water Volume Flow Rate [m3/s]  = " +
                                          General::RoundSigDigits(this->DesignWaterFlowRate, 6));
                        ShowContinueError("Design Fluid Cooler Air Volume Flow Rate [m3/s]    = " + General::RoundSigDigits(Par(4), 2));
                        ShowContinueError("Design Fluid Cooler Air Inlet Dry-bulb Temp [C]    = " + General::RoundSigDigits(this->AirTemp, 2));
                        ShowContinueError("Inputs to the plant sizing object:");
                        ShowContinueError("Design Exit Water Temp [C]                         = " +
                                          General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2));
                        ShowContinueError("Loop Design Temperature Difference [C]             = " +
                                          General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).DeltaT, 2));
                        ShowContinueError("Design Fluid Cooler Water Inlet Temp [C]           = " + General::RoundSigDigits(this->WaterTemp, 2));
                        ShowContinueError("Calculated water outlet temp at low UA [C] (UA = " + General::RoundSigDigits(UA0, 2) +
                                          " W/K) = " + General::RoundSigDigits(OutWaterTempAtUA0, 2));
                        ShowContinueError("Calculated water outlet temp at high UA [C](UA = " + General::RoundSigDigits(UA1, 2) +
                                          " W/K) = " + General::RoundSigDigits(OutWaterTempAtUA1, 2));
                        ShowFatalError("Autosizing of Fluid Cooler UA failed for fluid cooler = " + this->Name);
                    }
                    tmpHighSpeedEvapFluidCoolerUA = UA;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA;
                    this->FluidCoolerNominalCapacity = DesFluidCoolerLoad;
                } else {
                    tmpHighSpeedEvapFluidCoolerUA = 0.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA;
                }
                if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->FluidCoolerType,
                                                                    this->Name,
                                                                    "U-factor Times Area Value at Design Air Flow Rate [W/K]",
                                                                    this->HighSpeedFluidCoolerUA);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->FluidCoolerType,
                                                                    this->Name,
                                                                    "Initial U-factor Times Area Value at Design Air Flow Rate [W/K]",
                                                                    this->HighSpeedFluidCoolerUA);
                        }
                    }
                } else if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                this->FluidCoolerType, this->Name, "U-factor Times Area Value at High Fan Speed [W/K]", this->HighSpeedFluidCoolerUA);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->FluidCoolerType,
                                                                    this->Name,
                                                                    "Initial U-factor Times Area Value at High Fan Speed [W/K]",
                                                                    this->HighSpeedFluidCoolerUA);
                        }
                    }
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing error for fluid cooler object = " + this->Name);
                    ShowFatalError("Autosizing of fluid cooler UA requires a loop Sizing:Plant object.");
                }
            }
        }

        if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY) {
            if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                        DataGlobals::InitConvTemp,
                                                        DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                            this->DesignEnteringWaterTemp,
                                                            DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                DesFluidCoolerLoad = this->FluidCoolerNominalCapacity;
                Par(1) = DesFluidCoolerLoad;
                Par(2) = double(this->indexInArray);
                Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par(4) = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                Par(5) = Cp;
                UA0 = 0.0001 * DesFluidCoolerLoad;                     // Assume deltaT = 10000K (limit)
                UA1 = DesFluidCoolerLoad;                              // Assume deltaT = 1K
                this->WaterTemp = this->DesignEnteringWaterTemp;       // design inlet water temperature
                this->AirTemp = this->DesignEnteringAirTemp;           // design inlet air dry-bulb temp
                this->AirWetBulb = this->DesignEnteringAirWetBulbTemp; // design inlet air wet-bulb temp
                this->AirPress = DataEnvironment::StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(this->AirTemp, this->AirWetBulb, this->AirPress);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowWarningError("Iteration limit exceeded in calculating fluid cooler UA.");
                    if (PltSizCondNum > 0) {
                        ShowContinueError("Autosizing of fluid cooler UA failed for fluid cooler = " + this->Name);
                    }
                    ShowContinueError("The final UA value =" + General::RoundSigDigits(UA, 2) + " W/K, and the simulation continues...");
                } else if (SolFla == -2) {
                    CalcFluidCoolerOutlet(int(Par(2)), Par(3), Par(4), UA0, OutWaterTempAtUA0);
                    CalcFluidCoolerOutlet(int(Par(2)), Par(3), Par(4), UA1, OutWaterTempAtUA1);
                    ShowSevereError(CalledFrom + ": The combination of design input values did not allow the calculation of a ");
                    ShowContinueError("reasonable UA value. Review and revise design input values as appropriate. Specifying hard");
                    ShowContinueError(R"(sizes for some "autosizable" fields while autosizing other "autosizable" fields may be )");
                    ShowContinueError("contributing to this problem.");
                    ShowContinueError("This model iterates on UA to find the heat transfer required to provide the design outlet ");
                    ShowContinueError("water temperature. Initially, the outlet water temperatures at high and low UA values are ");
                    ShowContinueError("calculated. The Design Exit Water Temperature should be between the outlet water ");
                    ShowContinueError("temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ");
                    ShowContinueError("out of this range, the solution will not converge and UA will not be calculated. ");
                    ShowContinueError("The possible solutions could be to manually input adjusted water and/or air flow rates based ");
                    ShowContinueError("on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb temperature.");
                    ShowContinueError("Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).");
                    ShowContinueError("Inputs to the fluid cooler object:");
                    ShowContinueError("Design Fluid Cooler Load [W]                       = " + General::RoundSigDigits(Par(1), 2));
                    ShowContinueError("Design Fluid Cooler Water Volume Flow Rate [m3/s]  = " +
                                      General::RoundSigDigits(this->DesignWaterFlowRate, 6));
                    ShowContinueError("Design Fluid Cooler Air Volume Flow Rate [m3/s]    = " + General::RoundSigDigits(Par(4), 2));
                    ShowContinueError("Design Fluid Cooler Air Inlet Dry-bulb Temp [C]    = " + General::RoundSigDigits(this->AirTemp, 2));
                    if (PltSizCondNum > 0) {
                        ShowContinueError("Inputs to the plant sizing object:");
                        ShowContinueError("Design Exit Water Temp [C]                         = " +
                                          General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2));
                        ShowContinueError("Loop Design Temperature Difference [C]             = " +
                                          General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).DeltaT, 2));
                    };
                    ShowContinueError("Design Fluid Cooler Water Inlet Temp [C]           = " + General::RoundSigDigits(this->WaterTemp, 2));
                    ShowContinueError("Calculated water outlet temp at low UA [C] (UA = " + General::RoundSigDigits(UA0, 2) +
                                      " W/K) = " + General::RoundSigDigits(OutWaterTempAtUA0, 2));
                    ShowContinueError("Calculated water outlet temp at high UA [C] (UA = " + General::RoundSigDigits(UA1, 2) +
                                      " W/K) = " + General::RoundSigDigits(OutWaterTempAtUA1, 2));
                    if (PltSizCondNum > 0) {
                        ShowFatalError("Autosizing of Fluid Cooler UA failed for fluid cooler = " + this->Name);
                    }
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = UA;
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = 0.0;
            }
            if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->FluidCoolerType,
                                                                this->Name,
                                                                "Fluid cooler UA value at design air flow rate based on nominal capacity input [W/K]",
                                                                this->HighSpeedFluidCoolerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType,
                            this->Name,
                            "Initial Fluid cooler UA value at design air flow rate based on nominal capacity input [W/K]",
                            this->HighSpeedFluidCoolerUA);
                    }
                }
            } else if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->FluidCoolerType,
                                                                this->Name,
                                                                "Fluid cooler UA value at high fan speed based on nominal capacity input [W/K]",
                                                                this->HighSpeedFluidCoolerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            this->FluidCoolerType,
                            this->Name,
                            "Initial Fluid cooler UA value at high fan speed based on nominal capacity input [W/K]",
                            this->HighSpeedFluidCoolerUA);
                    }
                }
            }
        }

        if (this->LowSpeedAirFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
            this->LowSpeedAirFlowRate = this->LowSpeedAirFlowRateSizingFactor * this->HighSpeedAirFlowRate;
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    this->FluidCoolerType, this->Name, "Air Flow Rate at Low Fan Speed [m3/s]", this->LowSpeedAirFlowRate);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    this->FluidCoolerType, this->Name, "Initial Air Flow Rate at Low Fan Speed [m3/s]", this->LowSpeedAirFlowRate);
            }
        }

        if (this->LowSpeedFanPowerWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
            this->LowSpeedFanPower = this->LowSpeedFanPowerSizingFactor * this->HighSpeedFanPower;
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(this->FluidCoolerType, this->Name, "Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    this->FluidCoolerType, this->Name, "Initial Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower);
            }
        }

        if (this->LowSpeedFluidCoolerUAWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
            this->LowSpeedFluidCoolerUA = this->LowSpeedFluidCoolerUASizingFactor * this->HighSpeedFluidCoolerUA;
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    this->FluidCoolerType, this->Name, "U-factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedFluidCoolerUA);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    this->FluidCoolerType, this->Name, "Initial U-factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedFluidCoolerUA);
            }
        }

        if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY &&
            this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
            if (this->FluidCoolerLowSpeedNomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                this->FluidCoolerLowSpeedNomCap = this->FluidCoolerLowSpeedNomCapSizingFactor * this->FluidCoolerNominalCapacity;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        this->FluidCoolerType, this->Name, "Low Fan Speed Nominal Capacity [W]", this->FluidCoolerLowSpeedNomCap);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        this->FluidCoolerType, this->Name, "Initial Low Fan Speed Nominal Capacity [W]", this->FluidCoolerLowSpeedNomCap);
                }
            }

            if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow && this->FluidCoolerLowSpeedNomCap > 0.0) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                        DataGlobals::InitConvTemp,
                                                        DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                            this->DesignEnteringWaterTemp,
                                                            DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                DesFluidCoolerLoad = this->FluidCoolerLowSpeedNomCap;
                Par(1) = DesFluidCoolerLoad;
                Par(2) = double(this->indexInArray);
                Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par(4) = this->LowSpeedAirFlowRate;    // Air volume flow rate at low fan speed
                Par(5) = Cp;
                UA0 = 0.0001 * DesFluidCoolerLoad;                     // Assume deltaT = 10000K (limit)
                UA1 = DesFluidCoolerLoad;                              // Assume deltaT = 1K
                this->WaterTemp = this->DesignEnteringWaterTemp;       // design inlet water temperature
                this->AirTemp = this->DesignEnteringAirTemp;           // design inlet air dry-bulb temp
                this->AirWetBulb = this->DesignEnteringAirWetBulbTemp; // design inlet air wet-bulb temp
                this->AirPress = DataEnvironment::StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(this->AirTemp, this->AirWetBulb, this->AirPress, CalledFrom);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowWarningError("Iteration limit exceeded in calculating fluid cooler UA.");
                    ShowContinueError("Autosizing of fluid cooler UA failed for fluid cooler = " + this->Name);
                    ShowContinueError("The final UA value at low fan speed =" + General::RoundSigDigits(UA, 2) +
                                      " W/C, and the simulation continues...");
                } else if (SolFla == -2) {
                    CalcFluidCoolerOutlet(int(Par(2)), Par(3), Par(4), UA0, OutWaterTempAtUA0);
                    CalcFluidCoolerOutlet(int(Par(2)), Par(3), Par(4), UA1, OutWaterTempAtUA1);
                    ShowSevereError(CalledFrom + ": The combination of design input values did not allow the calculation of a ");
                    ShowContinueError("reasonable low-speed UA value. Review and revise design input values as appropriate. ");
                    ShowContinueError(R"(Specifying hard sizes for some "autosizable" fields while autosizing other "autosizable" )");
                    ShowContinueError("fields may be contributing to this problem.");
                    ShowContinueError("This model iterates on UA to find the heat transfer required to provide the design outlet ");
                    ShowContinueError("water temperature. Initially, the outlet water temperatures at high and low UA values are ");
                    ShowContinueError("calculated. The Design Exit Water Temperature should be between the outlet water ");
                    ShowContinueError("temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ");
                    ShowContinueError("out of this range, the solution will not converge and UA will not be calculated. ");
                    ShowContinueError("The possible solutions could be to manually input adjusted water and/or air flow rates based ");
                    ShowContinueError("on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb temperature.");
                    ShowContinueError("Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).");
                    ShowContinueError("Inputs to the fluid cooler object:");
                    ShowContinueError("Design Fluid Cooler Load [W]                         = " + General::RoundSigDigits(Par(1), 2));
                    ShowContinueError("Design Fluid Cooler Water Volume Flow Rate [m3/s]    = " +
                                      General::RoundSigDigits(this->DesignWaterFlowRate, 6));
                    ShowContinueError("Design Fluid Cooler Air Volume Flow Rate [m3/s]      = " + General::RoundSigDigits(Par(4), 2));
                    ShowContinueError("Design Fluid Cooler Air Inlet Dry-bulb Temp [C]      = " + General::RoundSigDigits(this->AirTemp, 2));
                    ShowContinueError("Inputs to the plant sizing object:");
                    ShowContinueError("Design Exit Water Temp [C]                           = " +
                                      General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2));
                    ShowContinueError("Loop Design Temperature Difference [C]               = " +
                                      General::RoundSigDigits(DataSizing::PlantSizData(PltSizCondNum).DeltaT, 2));
                    ShowContinueError("Design Fluid Cooler Water Inlet Temp [C]             = " + General::RoundSigDigits(this->WaterTemp, 2));
                    ShowContinueError("Calculated water outlet temp at low UA [C](UA = " + General::RoundSigDigits(UA0, 2) +
                                      " W/C) = " + General::RoundSigDigits(OutWaterTempAtUA0, 2));
                    ShowContinueError("Calculated water outlet temp at high UA [C](UA = " + General::RoundSigDigits(UA1, 2) +
                                      " W/C) = " + General::RoundSigDigits(OutWaterTempAtUA1, 2));
                    ShowFatalError("Autosizing of Fluid Cooler UA failed for fluid cooler = " + this->Name);
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->LowSpeedFluidCoolerUA = UA;
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) this->LowSpeedFluidCoolerUA = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        this->FluidCoolerType, this->Name, "U-factor Times Area Value at Low Fan Speed [W/C]", this->LowSpeedFluidCoolerUA);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        this->FluidCoolerType, this->Name, "Initial U-factor Times Area Value at Low Fan Speed [W/C]", this->LowSpeedFluidCoolerUA);
                }
            }
        }

        ErrorsFound = false;

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = this->Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, this->FluidCoolerType);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, this->FluidCoolerNominalCapacity);
        }

        if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd && DataPlant::PlantFirstSizesOkayToFinalize) {
            if (this->DesignWaterFlowRate > 0.0) {
                if (this->HighSpeedAirFlowRate <= this->LowSpeedAirFlowRate) {
                    ShowSevereError("FluidCooler:TwoSpeed  \"" + this->Name +
                                    "\". Low speed air flow rate must be less than high speed air flow rate.");
                    ErrorsFound = true;
                }
                if (this->HighSpeedFluidCoolerUA <= this->LowSpeedFluidCoolerUA) {
                    ShowSevereError("FluidCooler:TwoSpeed  \"" + this->Name +
                                    "\". Fluid cooler UA at low fan speed must be less than the fluid cooler UA at high fan speed.");
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("SizeFluidCooler: Program terminated due to previous condition(s).");
        }
    }

    void FluidCoolerspecs::calcSingleSpeed()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   August 2008
        //       MODIFIED       Dec. 2008. BG. added RunFlag logic per original methodology
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To simulate the operation of a single-speed fan fluid cooler.

        // METHODOLOGY EMPLOYED:
        // The fluid cooler is modeled using effectiveness-NTU relationships for
        // cross flow heat exchangers (both stream unmixed)based on cooling tower model.
        // The subroutine calculates the period of time required to meet a
        // leaving water temperature setpoint. It assumes that part-load
        // operation represents a linear interpolation of two steady-state regimes.
        // Cyclic losses are neglected. The period of time required to meet the
        // leaving water temperature setpoint is used to determine the required
        // fan power and energy.
        // A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
        // or schedule, of the fluid cooler. If the fluid cooler is OFF, outlet water
        // temperature and flow rate are passed through the model from inlet node to
        // outlet node without intervention. Reports are also updated with fan power
        // and energy being zero.
        // When the RunFlag indicates an ON condition for thefluid cooler, the
        // mass flow rate and water temperature are read from the inlet node of the
        // fluid cooler (water-side). The outdoor air dry-bulb temperature is used
        // as the entering condition to thefluid cooler (air-side).Thefluid cooler
        // fan is turned on and design parameters are used to calculate the leaving
        // water temperature.If the calculated leaving water temperature is below the setpoint,
        // a fan run-time fraction is calculated and used to determine fan power. The leaving
        // water temperature setpoint is placed on the outlet node. If the calculated
        // leaving water temperature is at or above the setpoint, the calculated
        // leaving water temperature is placed on the outlet node and the fan runs at
        // full power. Water mass flow rate is passed from inlet node to outlet node
        // with no intervention.

        // REFERENCES:
        // ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
        // Based on SingleSpeedTower subroutine by Dan Fisher ,Sept 1998.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SingleSpeedFluidCooler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TempSetPoint = 0.0;

        // set inlet and outlet nodes
        auto &waterInletNode = this->WaterInletNodeNum;
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->OutletWaterTemp = DataLoopNode::Node(waterInletNode).Temp;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(this->LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                TempSetPoint = DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                TempSetPoint = DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
            }
        }

        //   MassFlowTol is a parameter to indicate a no flow condition
        if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) return;

        if (this->OutletWaterTemp < TempSetPoint) { // already there don't need to run the cooler
            return;
        }

        //   Initialize local variables
        Real64 OutletWaterTempOFF = DataLoopNode::Node(waterInletNode).Temp;
        this->OutletWaterTemp = OutletWaterTempOFF;

        Real64 UAdesign = this->HighSpeedFluidCoolerUA;
        Real64 AirFlowRate = this->HighSpeedAirFlowRate;
        Real64 FanPowerOn = this->HighSpeedFanPower;

        CalcFluidCoolerOutlet(this->indexInArray, this->WaterMassFlowRate, AirFlowRate, UAdesign, this->OutletWaterTemp);

        if (this->OutletWaterTemp <= TempSetPoint) {
            //   Setpoint was met with pump ON and fan ON, calculate run-time fraction or just wasn't needed at all
            Real64 FanModeFrac = 0.0;
            if (this->OutletWaterTemp != OutletWaterTempOFF) { // don't divide by zero
                FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (this->OutletWaterTemp - OutletWaterTempOFF);
            }
            this->FanPower = max(FanModeFrac * FanPowerOn, 0.0); // BG change
            this->OutletWaterTemp = TempSetPoint;
        } else {
            //    Setpoint was not met, fluid cooler ran at full capacity
            this->FanPower = FanPowerOn;
        }
        Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                DataLoopNode::Node(waterInletNode).Temp,
                                                                DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
        this->Qactual = this->WaterMassFlowRate * CpWater * (DataLoopNode::Node(waterInletNode).Temp - this->OutletWaterTemp);
    }

    void FluidCoolerspecs::calcTwoSpeed()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   August 2008
        //       MODIFIED       Dec. 2008. BG. added RunFlag logic per original methodology
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To simulate the operation of a fluid cooler with a two-speed fan.

        // METHODOLOGY EMPLOYED:
        // The fluid cooler is modeled using effectiveness-NTU relationships for
        // cross flow heat exchangers (both stream unmixed)based on cooling tower model.
        // The subroutine calculates the period of time required to meet a
        // leaving water temperature setpoint. It assumes that part-load
        // operation represents a linear interpolation of two steady-state regimes
        // (high-speed fan operation and low-speed fan operation).
        // Cyclic losses are neglected. The period of time required to meet the
        // leaving water temperature setpoint is used to determine the required
        // fan power and energy.
        // A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
        // or schedule, of the fluid cooler. If the fluid cooler is OFF, outlet water
        // temperature and flow rate are passed through the model from inlet node to
        // outlet node without intervention.Reports are also updated with fan power
        // and fan energy being zero.
        // When the RunFlag indicates an ON condition for the fluid cooler, the
        // mass flow rate and water temperature are read from the inlet node of the
        // fluid cooler (water-side). The outdoor air dry-bulb temperature is used
        // as the entering condition to the fluid cooler (air-side). Input deck
        // parameters are read for the low fan speed and a leaving water temperature
        // is calculated.
        // If the calculated leaving water temperature is below the setpoint,
        // a fan run-time fraction (FanModeFrac) is calculated and used to determine fan power.
        // The leaving water temperature setpoint is placed on the outlet node.
        // If the calculated leaving water temperature is at or above
        // the setpoint, the fluid cooler fan is turned on 'high speed' and the routine is
        // repeated. If the calculated leaving water temperature is below the setpoint,
        // a fan run-time fraction is calculated for the second stage fan and fan power
        // is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
        // If the calculated leaving water temperature is above the leaving water temp.
        // setpoint, the calculated leaving water temperature is placed on the outlet
        // node and the fan runs at full power (High Speed Fan Power). Water mass flow
        // rate is passed from inlet node to outlet node with no intervention.

        // REFERENCES:
        // ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
        // Based on TwoSpeedTower by Dan Fisher ,Sept. 1998.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("TwoSpeedFluidCooler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TempSetPoint = 0.0;

        auto &waterInletNode = this->WaterInletNodeNum;
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->OutletWaterTemp = DataLoopNode::Node(waterInletNode).Temp;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(this->LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                TempSetPoint = DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                TempSetPoint = DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
            }
        }

        // MassFlowTol is a parameter to indicate a no flow condition
        if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance ||
            DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == 0)
            return;

        // set local variable for fluid cooler
        this->WaterMassFlowRate = DataLoopNode::Node(waterInletNode).MassFlowRate;
        Real64 OutletWaterTempOFF = DataLoopNode::Node(waterInletNode).Temp;
        Real64 OutletWaterTemp1stStage = OutletWaterTempOFF;
        Real64 OutletWaterTemp2ndStage = OutletWaterTempOFF;
        Real64 FanModeFrac = 0.0;

        if (OutletWaterTempOFF < TempSetPoint) { // already there don't need to run the cooler
            return;
        }

        Real64 UAdesign = this->LowSpeedFluidCoolerUA;
        Real64 AirFlowRate = this->LowSpeedAirFlowRate;
        Real64 FanPowerLow = this->LowSpeedFanPower;

        CalcFluidCoolerOutlet(this->indexInArray, this->WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp1stStage);

        if (OutletWaterTemp1stStage <= TempSetPoint) {
            // Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
            if (OutletWaterTemp1stStage != OutletWaterTempOFF) { // don't divide by zero
                FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (OutletWaterTemp1stStage - OutletWaterTempOFF);
            }
            this->FanPower = FanModeFrac * FanPowerLow;
            this->OutletWaterTemp = TempSetPoint;
            this->Qactual *= FanModeFrac;
        } else {
            // Setpoint was not met, turn on fluid cooler 2nd stage fan
            UAdesign = this->HighSpeedFluidCoolerUA;
            AirFlowRate = this->HighSpeedAirFlowRate;
            Real64 FanPowerHigh = this->HighSpeedFanPower;

            CalcFluidCoolerOutlet(this->indexInArray, this->WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp2ndStage);

            if ((OutletWaterTemp2ndStage <= TempSetPoint) && UAdesign > 0.0) {
                // Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
                FanModeFrac = (TempSetPoint - OutletWaterTemp1stStage) / (OutletWaterTemp2ndStage - OutletWaterTemp1stStage);
                this->FanPower = max((FanModeFrac * FanPowerHigh) + (1.0 - FanModeFrac) * FanPowerLow, 0.0);
                this->OutletWaterTemp = TempSetPoint;
            } else {
                // Setpoint was not met, fluid cooler ran at full capacity
                this->OutletWaterTemp = OutletWaterTemp2ndStage;
                this->FanPower = FanPowerHigh;
            }
        }
        Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                DataLoopNode::Node(waterInletNode).Temp,
                                                                DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
        this->Qactual = this->WaterMassFlowRate * CpWater * (DataLoopNode::Node(waterInletNode).Temp - this->OutletWaterTemp);
    }

    void CalcFluidCoolerOutlet(int FluidCoolerNum, Real64 _WaterMassFlowRate, Real64 AirFlowRate, Real64 UAdesign, Real64 &_OutletWaterTemp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2010, Chandan Sharma, FSEC
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // See purpose for Single Speed or Two Speed Fluid Cooler model

        // METHODOLOGY EMPLOYED:
        // See methodology for Single Speed or Two Speed Fluid Cooler model

        // Locals
        Real64 _InletWaterTemp; // Water inlet temperature
        Real64 _Qactual;        // Actual heat transfer rate between fluid cooler water and air [W]

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcFluidCoolerOutlet");

        if (UAdesign == 0.0) return;

        // set local fluid cooler inlet and outlet temperature variables
        _InletWaterTemp = SimpleFluidCooler(FluidCoolerNum).WaterTemp;
        _OutletWaterTemp = _InletWaterTemp;
        Real64 InletAirTemp = SimpleFluidCooler(FluidCoolerNum).AirTemp;

        // set water and air properties
        Real64 AirDensity =
            Psychrometrics::PsyRhoAirFnPbTdbW(SimpleFluidCooler(FluidCoolerNum).AirPress, InletAirTemp, SimpleFluidCooler(FluidCoolerNum).AirHumRat);
        Real64 AirMassFlowRate = AirFlowRate * AirDensity;
        Real64 CpAir = Psychrometrics::PsyCpAirFnW(SimpleFluidCooler(FluidCoolerNum).AirHumRat);
        Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleFluidCooler(FluidCoolerNum).LoopNum).FluidName,
                                                                _InletWaterTemp,
                                                                DataPlant::PlantLoop(SimpleFluidCooler(FluidCoolerNum).LoopNum).FluidIndex,
                                                                RoutineName);

        // Calculate mass flow rates
        Real64 MdotCpWater = _WaterMassFlowRate * CpWater;
        Real64 AirCapacity = AirMassFlowRate * CpAir;

        // calculate the minimum to maximum capacity ratios of airside and waterside
        Real64 CapacityRatioMin = min(AirCapacity, MdotCpWater);
        Real64 CapacityRatioMax = max(AirCapacity, MdotCpWater);
        Real64 CapacityRatio = CapacityRatioMin / CapacityRatioMax;

        // Calculate number of transfer units (NTU)
        Real64 NumTransferUnits = UAdesign / CapacityRatioMin;
        Real64 ETA = std::pow(NumTransferUnits, 0.22);
        Real64 A = CapacityRatio * NumTransferUnits / ETA;
        Real64 effectiveness = 1.0 - std::exp((std::exp(-A) - 1.0) / (CapacityRatio / ETA));

        // calculate water to air heat transfer
        _Qactual = effectiveness * CapacityRatioMin * (_InletWaterTemp - InletAirTemp);

        if (_Qactual >= 0.0) {
            _OutletWaterTemp = _InletWaterTemp - _Qactual / MdotCpWater;
        } else {
            _OutletWaterTemp = _InletWaterTemp;
        }
    }

    Real64 SimpleFluidCoolerUAResidual(Real64 const UA,           // UA of fluid cooler
                                       Array1D<Real64> const &Par // par(1) = design fluid cooler load [W]
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Design fluid cooler load - fluid cooler Output) / Design fluid cooler load.
        // Fluid cooler output depends on the UA which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Puts UA into the fluid cooler data structure, calls CalcFluidCoolerOutlet, and calculates
        // the residual as defined above.

        // REFERENCES:
        // Based on SimpleTowerUAResidual by Fred Buhl, May 2002

        // par(2) = Fluid cooler number
        // par(3) = design water mass flow rate [kg/s]
        // par(4) = design air volume flow rate [m3/s]
        // par(5) = water specific heat [J/(kg*C)]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutWaterTemp; // outlet water temperature [C]

        int FluidCoolerIndex = int(Par(2));
        CalcFluidCoolerOutlet(FluidCoolerIndex, Par(3), Par(4), UA, OutWaterTemp);
        Real64 const Output = Par(5) * Par(3) * (SimpleFluidCooler(FluidCoolerIndex).WaterTemp - OutWaterTemp);
        Real64 Residuum = (Par(1) - Output) / Par(1);
        return Residuum;
    }

    void FluidCoolerspecs::update()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Chandan Sharma
        //       DATE WRITTEN:    August 2008
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for passing results to the outlet water node.

        // SUBROUTINE PARAMETER DEFINITIONS:


        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 LoopMinTemp;

        auto &waterOutletNode = this->WaterOutletNodeNum;
        DataLoopNode::Node(waterOutletNode).Temp = this->OutletWaterTemp;

        if (DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == 0 || DataGlobals::WarmupFlag) return;

        // Check flow rate through fluid cooler and compare to design flow rate, show warning if greater than Design * Mulitplier
        if (DataLoopNode::Node(waterOutletNode).MassFlowRate > this->DesWaterMassFlowRate * this->FluidCoolerMassFlowRateMultiplier) {
            ++this->HighMassFlowErrorCount;
            if (this->HighMassFlowErrorCount < 2) {
                ShowWarningError(this->FluidCoolerType + " \"" + this->Name + "\"");
                ShowContinueError(" Condenser Loop Mass Flow Rate is much greater than the fluid coolers design mass flow rate.");
                ShowContinueError(" Condenser Loop Mass Flow Rate = " + General::TrimSigDigits(DataLoopNode::Node(waterOutletNode).MassFlowRate, 6));
                ShowContinueError(" Fluid Cooler Design Mass Flow Rate   = " + General::TrimSigDigits(this->DesWaterMassFlowRate, 6));
                ShowContinueErrorTimeStamp("");
            } else {
                ShowRecurringWarningErrorAtEnd(
                    this->FluidCoolerType + " \"" + this->Name +
                        "\"  Condenser Loop Mass Flow Rate is much greater than the fluid coolers design mass flow rate error continues...",
                    this->HighMassFlowErrorIndex,
                    DataLoopNode::Node(waterOutletNode).MassFlowRate,
                    DataLoopNode::Node(waterOutletNode).MassFlowRate);
            }
        }

        // Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
        LoopMinTemp = DataPlant::PlantLoop(this->LoopNum).MinTemp;
        if (this->OutletWaterTemp < LoopMinTemp && this->WaterMassFlowRate > 0.0) {
            ++this->OutletWaterTempErrorCount;

            if (this->OutletWaterTempErrorCount < 2) {
                ShowWarningError(format("{} \"{}\"", this->FluidCoolerType, this->Name));
                ShowContinueError(
                    format(" Fluid cooler water outlet temperature ({.2F} C) is below the specified minimum condenser loop temp of {.2F} C",
                           this->OutletWaterTemp,
                           LoopMinTemp));
                ShowContinueErrorTimeStamp("");
            } else {
                ShowRecurringWarningErrorAtEnd(
                    this->FluidCoolerType + " \"" + this->Name +
                        "\" Fluid cooler water outlet temperature is below the specified minimum condenser loop temp error continues...",
                    this->OutletWaterTempErrorIndex,
                    this->OutletWaterTemp,
                    this->OutletWaterTemp);
            }
        }

        // Check if water mass flow rate is small (e.g. no flow) and warn user
        if (this->WaterMassFlowRate > 0.0 && this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            ++this->SmallWaterMassFlowErrorCount;
            if (this->SmallWaterMassFlowErrorCount < 2) {
                ShowWarningError(this->FluidCoolerType + " \"" + this->Name + "\"");
                ShowContinueError(" Fluid cooler water mass flow rate near zero.");
                ShowContinueErrorTimeStamp("");
                ShowContinueError("Actual Mass flow = " + General::TrimSigDigits(this->WaterMassFlowRate, 2));
            } else {
                ShowRecurringWarningErrorAtEnd(this->FluidCoolerType + " \"" + this->Name +
                                                   "\" Fluid cooler water mass flow rate near zero error continues...",
                                               this->SmallWaterMassFlowErrorIndex,
                                               this->WaterMassFlowRate,
                                               this->WaterMassFlowRate);
            }
        }
    }

    void FluidCoolerspecs::report(bool const RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Chandan Sharma
        //       DATE WRITTEN:    August 2008
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the report variables for the fluid cooler.

        Real64 ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        auto &waterInletNode = this->WaterInletNodeNum;
        if (!RunFlag) {
            this->InletWaterTemp = DataLoopNode::Node(waterInletNode).Temp;
            this->OutletWaterTemp = DataLoopNode::Node(waterInletNode).Temp;
            this->Qactual = 0.0;
            this->FanPower = 0.0;
            this->FanEnergy = 0.0;
        } else {
            this->InletWaterTemp = DataLoopNode::Node(waterInletNode).Temp;
            this->FanEnergy = this->FanPower * ReportingConstant;
        }
    }

    void clear_state()
    {
        NumSimpleFluidCoolers = 0;
        SimpleFluidCooler.clear();
        UniqueSimpleFluidCoolerNames.clear();
        GetFluidCoolerInputFlag = true;
    }

} // namespace FluidCoolers

} // namespace EnergyPlus
