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
#include <EnergyPlus/ChillerExhaustAbsorption.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MicroturbineElectricGenerator.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerExhaustAbsorption {

    // MODULE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //                   for Gas Research Institute (Original module GasAbsoptionChiller)
    //    DATE WRITTEN   March 2001
    //    MODIFIED       Brent Griffith, Nov 2010 plant upgrades, generalize fluid properties
    //                   Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Absorption Chiller
    //    RE-ENGINEERED  na
    // PURPOSE OF THIS MODULE:
    //    This module simulates the performance of the Exhaust fired double effect
    //    absorption chiller.
    // METHODOLOGY EMPLOYED:
    //    Once the PlantLoopManager determines that the exhasut fired absorber chiller
    //    is available to meet a loop cooling demand, it calls SimExhaustAbsorption
    //    which in turn calls the appropriate Exhaut Fired Absorption Chiller model.
    // REFERENCES:
    //    DOE-2.1e Supplement
    //    PG&E CoolToolsGas Mod
    //    Performnace curves obtained from manufcaturer
    // OTHER NOTES:
    //    The curves on this model follow the DOE-2 approach of using
    //    electric and heat input ratios.  In addition, the temperature
    //    correction curve has two independent variables for the
    //    chilled water temperature and either the entering or leaving
    //    condenser water temperature.
    //    The code was originally adopted from the ChillerAbsorption
    //    routine but has been extensively modified.
    //    Development of the original(GasAbsoptionChiller) module was funded by the Gas Research Institute.
    //    (Please see copyright and disclaimer information at end of module)

    PlantComponent *ExhaustAbsorberSpecs::factory(ChillerExhaustAbsorptionData &chillers, std::string const &objectName)
    {
        // Process the input data if it hasn't been done already
        if (chillers.Sim_GetInput) {
            GetExhaustAbsorberInput(chillers);
            chillers.Sim_GetInput = false;
        }
        // Now look for this particular pipe in the list
        for (auto &comp : chillers.ExhaustAbsorber) {
            if (comp.Name == objectName) {
                return &comp;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalExhaustAbsorberFactory: Error getting inputs for comp named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void ExhaustAbsorberSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {

        // kind of a hacky way to find the location of this, but it's what plantloopequip was doing
        int BranchInletNodeNum =
            DataPlant::PlantLoop(calledFromLocation.loopNum).LoopSide(calledFromLocation.loopSideNum).Branch(calledFromLocation.branchNum).NodeNumIn;

        // Match inlet node name of calling branch to determine if this call is for heating or cooling
        if (BranchInletNodeNum == this->ChillReturnNodeNum) { // Operate as chiller
            this->InCoolingMode = RunFlag != 0;
            this->initialize(state.dataBranchInputManager);
            this->calcChiller(CurLoad);
            this->updateCoolRecords(CurLoad, RunFlag);
        } else if (BranchInletNodeNum == this->HeatReturnNodeNum) { // Operate as heater
            this->InHeatingMode = RunFlag != 0;
            this->initialize(state.dataBranchInputManager);
            this->calcHeater(CurLoad, RunFlag);
            this->updateHeatRecords(CurLoad, RunFlag);
        } else if (BranchInletNodeNum == this->CondReturnNodeNum) { // called from condenser loop
            if (this->CDLoopNum > 0) {
                PlantUtilities::UpdateChillerComponentCondenserSide(this->CDLoopNum,
                                                                    this->CDLoopSideNum,
                                                                    DataPlant::TypeOf_Chiller_ExhFiredAbsorption,
                                                                    this->CondReturnNodeNum,
                                                                    this->CondSupplyNodeNum,
                                                                    this->TowerLoad,
                                                                    this->CondReturnTemp,
                                                                    this->CondSupplyTemp,
                                                                    this->CondWaterFlowRate,
                                                                    FirstHVACIteration);
            }

        } else { // Error, nodes do not match
            ShowSevereError("Invalid call to Exhaust Absorber Chiller " + this->Name);
            ShowContinueError("Node connections in branch are not consistent with object nodes.");
            ShowFatalError("Preceding conditions cause termination.");
        }
    }

    void ExhaustAbsorberSpecs::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {

        // kind of a hacky way to find the location of this, but it's what plantloopequip was doing
        int BranchInletNodeNum =
            DataPlant::PlantLoop(calledFromLocation.loopNum).LoopSide(calledFromLocation.loopSideNum).Branch(calledFromLocation.branchNum).NodeNumIn;

        // Match inlet node name of calling branch to determine if this call is for heating or cooling
        if (BranchInletNodeNum == this->ChillReturnNodeNum) { // Operate as chiller
            MinLoad = this->NomCoolingCap * this->MinPartLoadRat;
            MaxLoad = this->NomCoolingCap * this->MaxPartLoadRat;
            OptLoad = this->NomCoolingCap * this->OptPartLoadRat;
        } else if (BranchInletNodeNum == this->HeatReturnNodeNum) {            // Operate as heater
            Real64 Sim_HeatCap = this->NomCoolingCap * this->NomHeatCoolRatio; // W - nominal heating capacity
            MinLoad = Sim_HeatCap * this->MinPartLoadRat;
            MaxLoad = Sim_HeatCap * this->MaxPartLoadRat;
            OptLoad = Sim_HeatCap * this->OptPartLoadRat;
        } else if (BranchInletNodeNum == this->CondReturnNodeNum) { // called from condenser loop
            MinLoad = 0.0;
            MaxLoad = 0.0;
            OptLoad = 0.0;
        } else { // Error, nodes do not match
            ShowSevereError("SimExhaustAbsorber: Invalid call to Exhaust Absorbtion Chiller-Heater " + this->Name);
            ShowContinueError("Node connections in branch are not consistent with object nodes.");
            ShowFatalError("Preceding conditions cause termination.");
        } // Operate as Chiller or Heater
    }

    void ExhaustAbsorberSpecs::getSizingFactor(Real64 &_SizFac)
    {
        _SizFac = this->SizFac;
    }

    void ExhaustAbsorberSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
    {
        this->initialize(state.dataBranchInputManager);

        // kind of a hacky way to find the location of this, but it's what plantloopequip was doing
        int BranchInletNodeNum =
            DataPlant::PlantLoop(calledFromLocation.loopNum).LoopSide(calledFromLocation.loopSideNum).Branch(calledFromLocation.branchNum).NodeNumIn;

        if (BranchInletNodeNum == this->ChillReturnNodeNum) { // Operate as chiller
            this->size();                                     // only call from chilled water loop
        } else {
            // don't do anything here
        }
    }

    void ExhaustAbsorberSpecs::getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut)
    {
        TempDesEvapOut = this->TempDesCHWSupply;
        TempDesCondIn = this->TempDesCondReturn;
    }

    void GetExhaustAbsorberInput(ChillerExhaustAbsorptionData &chillers)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Jason Glazer
        //       DATE WRITTEN:    March 2001
        //       MODIFIED         Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Double Effect Absorption Chiller
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the Exhaust Fired Absorption chiller model in the object ChillerHeater:Absorption:DoubleEffect

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        using BranchNodeConnections::TestCompSet;
        using CurveManager::GetCurveCheck;
        using DataSizing::AutoSize;
        using GlobalNames::VerifyUniqueChillerName;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckAndAddAirNodeNumber;

        // LOCAL VARIABLES
        int AbsorberNum; // Absorber counter
        int NumAlphas;   // Number of elements in the alpha array
        int NumNums;     // Number of elements in the numeric array
        int IOStat;      // IO Status when calling get input subroutine
        std::string ChillerName;
        bool Okay;
        bool Get_ErrorsFound(false);

        // FLOW
        cCurrentModuleObject = "ChillerHeater:Absorption:DoubleEffect";
        int NumExhaustAbsorbers = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumExhaustAbsorbers <= 0) {
            ShowSevereError("No " + cCurrentModuleObject + " equipment found in input file");
            Get_ErrorsFound = true;
        }

        if (allocated(chillers.ExhaustAbsorber)) return;

        // ALLOCATE ARRAYS
        chillers.ExhaustAbsorber.allocate(NumExhaustAbsorbers);

        // LOAD ARRAYS

        for (AbsorberNum = 1; AbsorberNum <= NumExhaustAbsorbers; ++AbsorberNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          AbsorberNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, Get_ErrorsFound);

            // Get_ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueChillerName(cCurrentModuleObject, cAlphaArgs(1), Get_ErrorsFound, cCurrentModuleObject + " Name");

            auto &thisChiller = chillers.ExhaustAbsorber(AbsorberNum);
            thisChiller.Name = cAlphaArgs(1);
            ChillerName = cCurrentModuleObject + " Named " + thisChiller.Name;

            // Assign capacities
            thisChiller.NomCoolingCap = rNumericArgs(1);
            if (thisChiller.NomCoolingCap == AutoSize) {
                thisChiller.NomCoolingCapWasAutoSized = true;
            }
            thisChiller.NomHeatCoolRatio = rNumericArgs(2);
            // Assign efficiencies
            thisChiller.ThermalEnergyCoolRatio = rNumericArgs(3);
            thisChiller.ThermalEnergyHeatRatio = rNumericArgs(4);
            thisChiller.ElecCoolRatio = rNumericArgs(5);
            thisChiller.ElecHeatRatio = rNumericArgs(6);

            // Assign Node Numbers to specified nodes
            thisChiller.ChillReturnNodeNum = GetOnlySingleNode(cAlphaArgs(2),
                                                                                Get_ErrorsFound,
                                                                                cCurrentModuleObject,
                                                                                cAlphaArgs(1),
                                                                                DataLoopNode::NodeType_Water,
                                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                                1,
                                                                                DataLoopNode::ObjectIsNotParent);
            thisChiller.ChillSupplyNodeNum = GetOnlySingleNode(cAlphaArgs(3),
                                                                                Get_ErrorsFound,
                                                                                cCurrentModuleObject,
                                                                                cAlphaArgs(1),
                                                                                DataLoopNode::NodeType_Water,
                                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                                1,
                                                                                DataLoopNode::ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Chilled Water Nodes");
            // Condenser node processing depends on condenser type, see below
            thisChiller.HeatReturnNodeNum = GetOnlySingleNode(cAlphaArgs(6),
                                                                               Get_ErrorsFound,
                                                                               cCurrentModuleObject,
                                                                               cAlphaArgs(1),
                                                                               DataLoopNode::NodeType_Water,
                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                               3,
                                                                               DataLoopNode::ObjectIsNotParent);
            thisChiller.HeatSupplyNodeNum = GetOnlySingleNode(cAlphaArgs(7),
                                                                               Get_ErrorsFound,
                                                                               cCurrentModuleObject,
                                                                               cAlphaArgs(1),
                                                                               DataLoopNode::NodeType_Water,
                                                                               DataLoopNode::NodeConnectionType_Outlet,
                                                                               3,
                                                                               DataLoopNode::ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(6), cAlphaArgs(7), "Hot Water Nodes");
            if (Get_ErrorsFound) {
                ShowFatalError("Errors found in processing node input for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                Get_ErrorsFound = false;
            }

            // Assign Part Load Ratios
            thisChiller.MinPartLoadRat = rNumericArgs(7);
            thisChiller.MaxPartLoadRat = rNumericArgs(8);
            thisChiller.OptPartLoadRat = rNumericArgs(9);
            // Assign Design Conditions
            thisChiller.TempDesCondReturn = rNumericArgs(10);
            thisChiller.TempDesCHWSupply = rNumericArgs(11);
            thisChiller.EvapVolFlowRate = rNumericArgs(12);
            if (thisChiller.EvapVolFlowRate == AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            if (UtilityRoutines::SameString(cAlphaArgs(16), "AirCooled")) {
                thisChiller.CondVolFlowRate = 0.0011; // Condenser flow rate not used for this cond type
            } else {
                thisChiller.CondVolFlowRate = rNumericArgs(13);
                if (thisChiller.CondVolFlowRate == AutoSize) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.HeatVolFlowRate = rNumericArgs(14);
            if (thisChiller.HeatVolFlowRate == AutoSize) {
                thisChiller.HeatVolFlowRateWasAutoSized = true;
            }
            // Assign Curve Numbers
            thisChiller.CoolCapFTCurve = GetCurveCheck(cAlphaArgs(8), Get_ErrorsFound, ChillerName);
            thisChiller.ThermalEnergyCoolFTCurve = GetCurveCheck(cAlphaArgs(9), Get_ErrorsFound, ChillerName);
            thisChiller.ThermalEnergyCoolFPLRCurve = GetCurveCheck(cAlphaArgs(10), Get_ErrorsFound, ChillerName);
            thisChiller.ElecCoolFTCurve = GetCurveCheck(cAlphaArgs(11), Get_ErrorsFound, ChillerName);
            thisChiller.ElecCoolFPLRCurve = GetCurveCheck(cAlphaArgs(12), Get_ErrorsFound, ChillerName);
            thisChiller.HeatCapFCoolCurve = GetCurveCheck(cAlphaArgs(13), Get_ErrorsFound, ChillerName);
            thisChiller.ThermalEnergyHeatFHPLRCurve = GetCurveCheck(cAlphaArgs(14), Get_ErrorsFound, ChillerName);
            if (Get_ErrorsFound) {
                ShowFatalError("Errors found in processing curve input for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                Get_ErrorsFound = false;
            }
            if (UtilityRoutines::SameString(cAlphaArgs(15), "LeavingCondenser")) {
                thisChiller.isEnterCondensTemp = false;
            } else if (UtilityRoutines::SameString(cAlphaArgs(15), "EnteringCondenser")) {
                thisChiller.isEnterCondensTemp = true;
            } else {
                thisChiller.isEnterCondensTemp = true;
                ShowWarningError("Invalid " + cAlphaFieldNames(15) + '=' + cAlphaArgs(15));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ShowContinueError("resetting to ENTERING-CONDENSER, simulation continues");
            }
            // Assign Other Paramters
            if (UtilityRoutines::SameString(cAlphaArgs(16), "AirCooled")) {
                thisChiller.isWaterCooled = false;
            } else if (UtilityRoutines::SameString(cAlphaArgs(16), "WaterCooled")) {
                thisChiller.isWaterCooled = true;
            } else {
                thisChiller.isWaterCooled = true;
                ShowWarningError("Invalid " + cAlphaFieldNames(16) + '=' + cAlphaArgs(16));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ShowContinueError("resetting to WATER-COOLED, simulation continues");
            }
            if (!thisChiller.isEnterCondensTemp && !thisChiller.isWaterCooled) {
                thisChiller.isEnterCondensTemp = true;
                ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid value");
                ShowContinueError("Invalid to have both LeavingCondenser and AirCooled.");
                ShowContinueError("resetting to EnteringCondenser, simulation continues");
            }
            if (thisChiller.isWaterCooled) {
                if (lAlphaFieldBlanks(5)) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid value");
                    ShowContinueError("For WaterCooled chiller the condenser outlet node is required.");
                    Get_ErrorsFound = true;
                }
                thisChiller.CondReturnNodeNum = GetOnlySingleNode(cAlphaArgs(4),
                                                                                   Get_ErrorsFound,
                                                                                   cCurrentModuleObject,
                                                                                   cAlphaArgs(1),
                                                                                   DataLoopNode::NodeType_Water,
                                                                                   DataLoopNode::NodeConnectionType_Inlet,
                                                                                   2,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondSupplyNodeNum = GetOnlySingleNode(cAlphaArgs(5),
                                                                                   Get_ErrorsFound,
                                                                                   cCurrentModuleObject,
                                                                                   cAlphaArgs(1),
                                                                                   DataLoopNode::NodeType_Water,
                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                   2,
                                                                                   DataLoopNode::ObjectIsNotParent);
                TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(4), cAlphaArgs(5), "Condenser Water Nodes");
            } else {
                thisChiller.CondReturnNodeNum = GetOnlySingleNode(cAlphaArgs(4),
                                                                                   Get_ErrorsFound,
                                                                                   cCurrentModuleObject,
                                                                                   cAlphaArgs(1),
                                                                                   DataLoopNode::NodeType_Air,
                                                                                   DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                   2,
                                                                                   DataLoopNode::ObjectIsNotParent);
                // Condenser outlet node not used for air or evap cooled condenser so ingore cAlphaArgs( 5 )
                // Connection not required for air or evap cooled condenser so no call to TestCompSet here
                CheckAndAddAirNodeNumber(thisChiller.CondReturnNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs(4));
                }
            }

            thisChiller.CHWLowLimitTemp = rNumericArgs(15);
            thisChiller.SizFac = rNumericArgs(16);
            thisChiller.TypeOf = cAlphaArgs(17);

            if (UtilityRoutines::SameString(cAlphaArgs(17), "Generator:MicroTurbine")) {
                thisChiller.CompType_Num = DataGlobalConstants::iGeneratorMicroturbine;
                thisChiller.ExhuastSourceName = cAlphaArgs(18);

                auto thisMTG = MicroturbineElectricGenerator::MTGeneratorSpecs::factory(thisChiller.ExhuastSourceName);
                thisChiller.ExhaustAirInletNodeNum =
                    dynamic_cast<MicroturbineElectricGenerator::MTGeneratorSpecs *>(thisMTG)->CombustionAirOutletNodeNum;
            }
        }

        if (Get_ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + cCurrentModuleObject);
        }
    }

    void ExhaustAbsorberSpecs::setupOutputVariables()
    {
        std::string const ChillerName = this->Name;

        SetupOutputVariable("Chiller Heater Evaporator Cooling Rate", OutputProcessor::Unit::W, this->CoolingLoad, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->CoolingEnergy,
                            "System",
                            "Sum",
                            ChillerName,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller Heater Heating Rate", OutputProcessor::Unit::W, this->HeatingLoad, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Heating Energy",
                            OutputProcessor::Unit::J,
                            this->HeatingEnergy,
                            "System",
                            "Sum",
                            ChillerName,
                            _,
                            "ENERGYTRANSFER",
                            "BOILERS",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Chiller Heater Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->TowerLoad, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->TowerEnergy,
                            "System",
                            "Sum",
                            ChillerName,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Chiller Heater Cooling Source Heat COP", OutputProcessor::Unit::W_W, this->ThermalEnergyCOP, "System", "Average", ChillerName);

        SetupOutputVariable("Chiller Heater Electricity Rate", OutputProcessor::Unit::W, this->ElectricPower, "System", "Average", ChillerName);
        // Do not include this on meters, this would duplicate the cool electric and heat electric
        SetupOutputVariable("Chiller Heater Electricity Energy", OutputProcessor::Unit::J, this->ElectricEnergy, "System", "Sum", ChillerName);

        SetupOutputVariable(
            "Chiller Heater Cooling Electricity Rate", OutputProcessor::Unit::W, this->CoolElectricPower, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Cooling Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->CoolElectricEnergy,
                            "System",
                            "Sum",
                            ChillerName,
                            _,
                            "Electricity",
                            "Cooling",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Chiller Heater Heating Electricity Rate", OutputProcessor::Unit::W, this->HeatElectricPower, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Heating Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->HeatElectricEnergy,
                            "System",
                            "Sum",
                            ChillerName,
                            _,
                            "Electricity",
                            "Heating",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Chiller Heater Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->ChillReturnTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            "Chiller Heater Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->ChillSupplyTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            "Chiller Heater Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->ChillWaterFlowRate, "System", "Average", ChillerName);

        if (this->isWaterCooled) {
            SetupOutputVariable(
                "Chiller Heater Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondReturnTemp, "System", "Average", ChillerName);
            SetupOutputVariable(
                "Chiller Heater Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondSupplyTemp, "System", "Average", ChillerName);
            SetupOutputVariable(
                "Chiller Heater Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondWaterFlowRate, "System", "Average", ChillerName);
        } else {
            SetupOutputVariable(
                "Chiller Heater Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondReturnTemp, "System", "Average", ChillerName);
        }

        SetupOutputVariable(
            "Chiller Heater Heating Inlet Temperature", OutputProcessor::Unit::C, this->HotWaterReturnTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            "Chiller Heater Heating Outlet Temperature", OutputProcessor::Unit::C, this->HotWaterSupplyTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            "Chiller Heater Heating Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HotWaterFlowRate, "System", "Average", ChillerName);

        SetupOutputVariable(
            "Chiller Heater Cooling Part Load Ratio", OutputProcessor::Unit::None, this->CoolPartLoadRatio, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Maximum Cooling Rate", OutputProcessor::Unit::W, this->CoolingCapacity, "System", "Average", ChillerName);
        SetupOutputVariable(
            "Chiller Heater Heating Part Load Ratio", OutputProcessor::Unit::None, this->HeatPartLoadRatio, "System", "Average", ChillerName);
        SetupOutputVariable("Chiller Heater Maximum Heating Rate", OutputProcessor::Unit::W, this->HeatingCapacity, "System", "Average", ChillerName);

        SetupOutputVariable(
            "Chiller Heater Runtime Fraction", OutputProcessor::Unit::None, this->FractionOfPeriodRunning, "System", "Average", ChillerName);

        SetupOutputVariable(
            "Chiller Heater Source Exhaust Inlet Temperature", OutputProcessor::Unit::C, this->ExhaustInTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            "Chiller Heater Source Exhaust Inlet Mass Flow Rate", OutputProcessor::Unit::kg_s, this->ExhaustInFlow, "System", "Average", ChillerName);

        SetupOutputVariable("Chiller Heater Heating Heat Recovery Potential Rate",
                            OutputProcessor::Unit::W,
                            this->ExhHeatRecPotentialHeat,
                            "System",
                            "Average",
                            ChillerName);
        SetupOutputVariable("Chiller Heater Cooling Heat Recovery Potential Rate",
                            OutputProcessor::Unit::W,
                            this->ExhHeatRecPotentialCool,
                            "System",
                            "Average",
                            ChillerName);

        SetupOutputVariable("Chiller Heater Cooling Source Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->CoolThermalEnergyUseRate,
                            "System",
                            "Average",
                            ChillerName);
        SetupOutputVariable("Chiller Heater Heating Source Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->HeatThermalEnergyUseRate,
                            "System",
                            "Average",
                            ChillerName);
    }

    void ExhaustAbsorberSpecs::initialize(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   June 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of Exhaust Fired absorption chiller
        // components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        std::string const RoutineName("InitExhaustAbsorber");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CondInletNode;  // node number of water inlet node to the condenser
        int CondOutletNode; // node number of water outlet node from the condenser
        int HeatInletNode;  // node number of hot water inlet node
        int HeatOutletNode; // node number of hot water outlet node
        bool errFlag;
        Real64 rho;  // local fluid density
        Real64 mdot; // lcoal fluid mass flow rate

        if (this->oneTimeInit) {
            this->setupOutputVariables();
            this->oneTimeInit = false;
        }

        // Init more variables
        if (this->plantScanInit) {
            // Locate the chillers on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                    this->Name,
                                                    DataPlant::TypeOf_Chiller_ExhFiredAbsorption,
                                                    this->CWLoopNum,
                                                    this->CWLoopSideNum,
                                                    this->CWBranchNum,
                                                    this->CWCompNum,
                                                    errFlag,
                                                    this->CHWLowLimitTemp,
                                                    _,
                                                    _,
                                                    this->ChillReturnNodeNum,
                                                    _);
            if (errFlag) {
                ShowFatalError("InitExhaustAbsorber: Program terminated due to previous condition(s).");
            }

            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                    this->Name,
                                                    DataPlant::TypeOf_Chiller_ExhFiredAbsorption,
                                                    this->HWLoopNum,
                                                    this->HWLoopSideNum,
                                                    this->HWBranchNum,
                                                    this->HWCompNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->HeatReturnNodeNum,
                                                    _);
            if (errFlag) {
                ShowFatalError("InitExhaustAbsorber: Program terminated due to previous condition(s).");
            }

            if (this->isWaterCooled) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                        this->Name,
                                                        DataPlant::TypeOf_Chiller_ExhFiredAbsorption,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        this->CDBranchNum,
                                                        this->CDCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->CondReturnNodeNum,
                                                        _);
                if (errFlag) {
                    ShowFatalError("InitExhaustAbsorber: Program terminated due to previous condition(s).");
                }
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_ExhFiredAbsorption, true);
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->HWLoopNum, this->HWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_ExhFiredAbsorption, true);
            }

            PlantUtilities::InterConnectTwoPlantLoopSides(
                this->CWLoopNum, this->CWLoopSideNum, this->HWLoopNum, this->HWLoopSideNum, DataPlant::TypeOf_Chiller_ExhFiredAbsorption, true);

            // check if outlet node of chilled water side has a setpoint.
            if ((DataLoopNode::Node(this->ChillSupplyNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                (DataLoopNode::Node(this->ChillSupplyNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    if (!this->ChillSetPointErrDone) {
                        ShowWarningError("Missing temperature setpoint on cool side for chiller heater named " + this->Name);
                        ShowContinueError("  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager");
                        ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                        this->ChillSetPointErrDone = true;
                    }
                } else {
                    // need call to EMS to check node
                    errFlag = false; // but not really fatal yet, but should be.
                    EMSManager::CheckIfNodeSetPointManagedByEMS(this->ChillSupplyNodeNum, EMSManager::iTemperatureSetPoint, errFlag);
                    if (errFlag) {
                        if (!this->ChillSetPointErrDone) {
                            ShowWarningError("Missing temperature setpoint on cool side for chiller heater named " + this->Name);
                            ShowContinueError("  A temperature setpoint is needed at the outlet node of this chiller evaporator ");
                            ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                            ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ChillSetPointErrDone = true;
                        }
                    }
                }
                this->ChillSetPointSetToLoop = true;
                DataLoopNode::Node(this->ChillSupplyNodeNum).TempSetPoint =
                    DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                DataLoopNode::Node(this->ChillSupplyNodeNum).TempSetPointHi =
                    DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
            }
            // check if outlet node of hot water side has a setpoint.
            if ((DataLoopNode::Node(this->HeatSupplyNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                (DataLoopNode::Node(this->HeatSupplyNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    if (!this->HeatSetPointErrDone) {
                        ShowWarningError("Missing temperature setpoint on heat side for chiller heater named " + this->Name);
                        ShowContinueError("  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager");
                        ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                        this->HeatSetPointErrDone = true;
                    }
                } else {
                    // need call to EMS to check node
                    errFlag = false; // but not really fatal yet, but should be.
                    EMSManager::CheckIfNodeSetPointManagedByEMS(this->HeatSupplyNodeNum, EMSManager::iTemperatureSetPoint, errFlag);
                    if (errFlag) {
                        if (!this->HeatSetPointErrDone) {
                            ShowWarningError("Missing temperature setpoint on heat side for chiller heater named " + this->Name);
                            ShowContinueError("  A temperature setpoint is needed at the outlet node of this chiller heater ");
                            ShowContinueError("  use a Setpoint Manager to establish a setpoint at the heater side outlet node ");
                            ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                            ShowContinueError("  The overall loop setpoint will be assumed for heater side. The simulation continues ... ");
                            this->HeatSetPointErrDone = true;
                        }
                    }
                }
                this->HeatSetPointSetToLoop = true;
                DataLoopNode::Node(this->HeatSupplyNodeNum).TempSetPoint =
                    DataLoopNode::Node(DataPlant::PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
                DataLoopNode::Node(this->HeatSupplyNodeNum).TempSetPointLo =
                    DataLoopNode::Node(DataPlant::PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPointLo;
            }
            this->plantScanInit = false;
        }

        CondInletNode = this->CondReturnNodeNum;
        CondOutletNode = this->CondSupplyNodeNum;
        HeatInletNode = this->HeatReturnNodeNum;
        HeatOutletNode = this->HeatSupplyNodeNum;

        if (this->envrnInit && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            if (this->isWaterCooled) {
                // init max available condenser water flow rate
                if (this->CDLoopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
                } else {
                    rho = Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
                }

                this->DesCondMassFlowRate = rho * this->CondVolFlowRate;
                PlantUtilities::InitComponentNodes(0.0,
                                                   this->DesCondMassFlowRate,
                                                   CondInletNode,
                                                   CondOutletNode,
                                                   this->CDLoopNum,
                                                   this->CDLoopSideNum,
                                                   this->CDBranchNum,
                                                   this->CDCompNum);
            }

            if (this->HWLoopNum > 0) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HWLoopNum).FluidName,
                                                        DataGlobals::HWInitConvTemp,
                                                        DataPlant::PlantLoop(this->HWLoopNum).FluidIndex,
                                                        RoutineName);
            } else {
                rho = Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            }
            this->DesHeatMassFlowRate = rho * this->HeatVolFlowRate;
            // init available hot water flow rate
            PlantUtilities::InitComponentNodes(0.0,
                                               this->DesHeatMassFlowRate,
                                               HeatInletNode,
                                               HeatOutletNode,
                                               this->HWLoopNum,
                                               this->HWLoopSideNum,
                                               this->HWBranchNum,
                                               this->HWCompNum);

            if (this->CWLoopNum > 0) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
            } else {
                rho = Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            }
            this->DesEvapMassFlowRate = rho * this->EvapVolFlowRate;
            // init available hot water flow rate
            PlantUtilities::InitComponentNodes(0.0,
                                               this->DesEvapMassFlowRate,
                                               this->ChillReturnNodeNum,
                                               this->ChillSupplyNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            this->envrnInit = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->envrnInit = true;
        }

        // this component model works off setpoints on the leaving node
        // fill from plant if needed
        if (this->ChillSetPointSetToLoop) {
            DataLoopNode::Node(this->ChillSupplyNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->ChillSupplyNodeNum).TempSetPointHi =
                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        if (this->HeatSetPointSetToLoop) {
            DataLoopNode::Node(this->HeatSupplyNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->HeatSupplyNodeNum).TempSetPointLo =
                DataLoopNode::Node(DataPlant::PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPointLo;
        }

        if ((this->isWaterCooled) && ((this->InHeatingMode) || (this->InCoolingMode)) && (!this->plantScanInit)) {
            mdot = this->DesCondMassFlowRate;

            PlantUtilities::SetComponentFlowRate(
                mdot, this->CondReturnNodeNum, this->CondSupplyNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);

        } else {
            mdot = 0.0;
            if (this->CDLoopNum > 0) {
                PlantUtilities::SetComponentFlowRate(
                    mdot, this->CondReturnNodeNum, this->CondSupplyNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
            }
        }
    }

    void ExhaustAbsorberSpecs::size()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   June 2003
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Exhaust Fired absorption chiller components for which
        // capacities and flow rates have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        // SUBROUTINE PARAMETER DEFINITIONS:
        std::string const RoutineName("SizeExhaustAbsorber");

        bool ErrorsFound; // If errors detected in input
        std::string equipName;
        Real64 Cp;                     // local fluid specific heat
        Real64 rho;                    // local fluid density
        Real64 tmpNomCap;              // local nominal capacity cooling power
        Real64 tmpEvapVolFlowRate;     // local evaporator design volume flow rate
        Real64 tmpCondVolFlowRate;     // local condenser design volume flow rate
        Real64 tmpHeatRecVolFlowRate;  // local heat recovery design volume flow rate
        Real64 NomCapUser;             // Hardsized nominal capacity for reporting
        Real64 EvapVolFlowRateUser;    // Hardsized evaporator volume flow rate for reporting
        Real64 CondVolFlowRateUser;    // Hardsized condenser flow rate for reporting
        Real64 HeatRecVolFlowRateUser; // Hardsized generator flow rate for reporting

        ErrorsFound = false;
        tmpNomCap = this->NomCoolingCap;
        tmpEvapVolFlowRate = this->EvapVolFlowRate;
        tmpCondVolFlowRate = this->CondVolFlowRate;
        tmpHeatRecVolFlowRate = this->HeatVolFlowRate;

        int PltSizCondNum = 0; // Plant Sizing index for condenser loop
        if (this->isWaterCooled) PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;

        int PltSizHeatNum = DataPlant::PlantLoop(this->HWLoopNum).PlantSizNum;
        int PltSizCoolNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizCoolNum > 0) {
            if (DataSizing::PlantSizData(PltSizCoolNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
                tmpNomCap =
                    Cp * rho * DataSizing::PlantSizData(PltSizCoolNum).DeltaT * DataSizing::PlantSizData(PltSizCoolNum).DesVolFlowRate * this->SizFac;
                if (!this->NomCoolingCapWasAutoSized) tmpNomCap = this->NomCoolingCap;
            } else {
                if (this->NomCoolingCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCoolingCapWasAutoSized) {
                    this->NomCoolingCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "ChillerHeater:Absorption:DoubleEffect", this->Name, "Design Size Nominal Cooling Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "ChillerHeater:Absorption:DoubleEffect", this->Name, "Initial Design Size Nominal Cooling Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCoolingCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = this->NomCoolingCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                    this->Name,
                                                                    "Design Size Nominal Cooling Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Cooling Capacity [W]",
                                                                    NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerHeaterAbsorptionDoubleEffect: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) +
                                                      " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = NomCapUser;
                    }
                }
            }
        } else {
            if (this->NomCoolingCapWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + this->Name + "\", autosize error.");
                    ShowContinueError("Autosizing of Exhaust Fired Absorption Chiller nominal cooling capacity requires");
                    ShowContinueError("a cooling loop Sizing:Plant object.");
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (this->NomCoolingCap > 0.0) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Absorption:DoubleEffect", this->Name, "User-Specified Nominal Capacity [W]", this->NomCoolingCap);
                    }
                }
            }
        }

        if (PltSizCoolNum > 0) {
            if (DataSizing::PlantSizData(PltSizCoolNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizCoolNum).DesVolFlowRate * this->SizFac;
                if (!this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = this->EvapVolFlowRate;

            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                    this->Name,
                                                                    "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorptionDoubleEffect: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(EvapVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpEvapVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = EvapVolFlowRateUser;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + this->Name + "\", autosize error.");
                    ShowContinueError("Autosizing of Exhaust Fired Absorption Chiller evap flow rate requires");
                    ShowContinueError("a cooling loop Sizing:Plant object.");
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (this->EvapVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                this->EvapVolFlowRate);
                    }
                }
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->ChillReturnNodeNum, tmpEvapVolFlowRate);

        if (PltSizHeatNum > 0) {
            if (DataSizing::PlantSizData(PltSizHeatNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpHeatRecVolFlowRate = DataSizing::PlantSizData(PltSizHeatNum).DesVolFlowRate * this->SizFac;
                if (!this->HeatVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->HeatVolFlowRate;

            } else {
                if (this->HeatVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->HeatVolFlowRateWasAutoSized) {
                    this->HeatVolFlowRate = tmpHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "Design Size Design Hot Water Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "Initial Design Size Design Hot Water Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->HeatVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        HeatRecVolFlowRateUser = this->HeatVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                    this->Name,
                                                                    "Design Size Design Hot Water Flow Rate [m3/s]",
                                                                    tmpHeatRecVolFlowRate,
                                                                    "User-Specified Design Hot Water Flow Rate [m3/s]",
                                                                    HeatRecVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - HeatRecVolFlowRateUser) / HeatRecVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerHeaterAbsorptionDoubleEffect: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Hot Water Flow Rate of " +
                                                      General::RoundSigDigits(HeatRecVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Hot Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpHeatRecVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = HeatRecVolFlowRateUser;
                    }
                }
            }
        } else {
            if (this->HeatVolFlowRateWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + this->Name + "\", autosize error.");
                    ShowContinueError("Autosizing of Exhaust Fired Absorption Chiller hot water flow rate requires");
                    ShowContinueError("a heating loop Sizing:Plant object.");
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (this->HeatVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "User-Specified Design Hot Water Flow Rate [m3/s]",
                                                                this->HeatVolFlowRate);
                    }
                }
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->HeatReturnNodeNum, tmpHeatRecVolFlowRate);

        if (PltSizCondNum > 0 && PltSizCoolNum > 0) {
            if (DataSizing::PlantSizData(PltSizCoolNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {

                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                            this->TempDesCondReturn,
                                                            DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        this->TempDesCondReturn,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + this->ThermalEnergyCoolRatio) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                if (!this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = this->CondVolFlowRate;

            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        CondVolFlowRateUser = this->CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                    this->Name,
                                                                    "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                    CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorptionDoubleEffect: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Condenser Water Flow Rate of " +
                                                      General::RoundSigDigits(CondVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Condenser Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpCondVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = CondVolFlowRateUser;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect=\"" + this->Name + "\", autosize error.");
                    ShowSevereError("Autosizing of Exhaust Fired Absorption Chiller condenser flow rate requires a condenser");
                    ShowContinueError("loop Sizing:Plant object.");
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (this->CondVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("ChillerHeater:Absorption:DoubleEffect",
                                                                this->Name,
                                                                "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                this->CondVolFlowRate);
                    }
                }
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->isWaterCooled) PlantUtilities::RegisterPlantCompDesignFlow(this->CondReturnNodeNum, tmpCondVolFlowRate);

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = this->Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, "ChillerHeater:Absorption:DoubleEffect");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, this->ThermalEnergyCoolRatio);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, this->NomCoolingCap);
        }
    }

    void ExhaustAbsorberSpecs::calcChiller(Real64 &MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   March 2001
        //       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate exhaust fired chiller
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a Exhaust fired (Exhaust consuming) absorption chiller using
        // curves and inputs similar to DOE-2.1e

        // METHODOLOGY EMPLOYED:
        // Curve fit of performance data

        // REFERENCES:
        // 1.  DOE-2.1e Supplement and source code
        // 2.  CoolTools GasMod work

        // FlowLock = 0  if mass flow rates may be changed by loop components
        // FlowLock = 1  if mass flow rates may not be changed by loop components

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const AbsLeavingTemp(176.667); // C - Minimum temperature leaving the Chiller absorber (350 F)
        std::string const RoutineName("CalcExhaustAbsorberChillerModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // Local copies of ExhaustAbsorberSpecs Type
        // all variables that are local copies of data structure
        // variables are prefaced with an "l" for local.
        Real64 lNomCoolingCap;           // W - design nominal capacity of Absorber
        Real64 lThermalEnergyCoolRatio;  // ratio of ThermalEnergy input to cooling output
        Real64 lThermalEnergyHeatRatio;  // ratio of ThermalEnergy input to heating output
        Real64 lElecCoolRatio;           // ratio of electricity input to cooling output
        int lChillReturnNodeNum;         // Node number on the inlet side of the plant
        int lChillSupplyNodeNum;         // Node number on the outlet side of the plant
        int lCondReturnNodeNum;          // Node number on the inlet side of the condenser
        Real64 lMinPartLoadRat;          // min allowed operating frac full load
        Real64 lMaxPartLoadRat;          // max allowed operating frac full load
        int lCoolCapFTCurve;             // cooling capacity as a function of temperature curve
        int lThermalEnergyCoolFTCurve;   // ThermalEnergy-Input-to cooling output Ratio Function of Temperature Curve
        int lThermalEnergyCoolFPLRCurve; // ThermalEnergy-Input-to cooling output Ratio Function of Part Load Ratio Curve
        int lElecCoolFTCurve;            // Electric-Input-to cooling output Ratio Function of Temperature Curve
        int lElecCoolFPLRCurve;          // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
        bool lIsEnterCondensTemp;        // if using entering conderser water temperature is TRUE, exiting is FALSE
        bool lIsWaterCooled;             // if water cooled it is TRUE
        Real64 lCHWLowLimitTemp;         // Chilled Water Lower Limit Temperature
        int lExhaustAirInletNodeNum;     // Combustion Air Inlet Node number
        // Local copies of ExhaustAbsorberReportVars Type
        Real64 lCoolingLoad(0.0); // cooling load on the chiller (previously called QEvap)
        // Real64 lCoolingEnergy( 0.0 ); // variable to track total cooling load for period (was EvapEnergy)
        Real64 lTowerLoad(0.0); // load on the cooling tower/condenser (previously called QCond)
        // Real64 lTowerEnergy( 0.0 ); // variable to track total tower load for a period (was CondEnergy)
        // Real64 lThermalEnergyUseRate( 0.0 ); // instantaneous use of exhaust for period
        // Real64 lThermalEnergy( 0.0 ); // variable to track total ThermalEnergy used for a period
        Real64 lCoolThermalEnergyUseRate(0.0); // instantaneous use of exhaust for period for cooling
        // Real64 lCoolThermalEnergy( 0.0 ); // variable to track total ThermalEnergy used for a period for cooling
        Real64 lHeatThermalEnergyUseRate(0.0); // instantaneous use of exhaust for period for heating
        // Real64 lElectricPower( 0.0 ); // parasitic electric power used (was PumpingPower)
        // Real64 lElectricEnergy( 0.0 ); // track the total electricity used for a period (was PumpingEnergy)
        Real64 lCoolElectricPower(0.0); // parasitic electric power used  for cooling
        // Real64 lCoolElectricEnergy( 0.0 ); // track the total electricity used for a period for cooling
        Real64 lHeatElectricPower(0.0);        // parasitic electric power used  for heating
        Real64 lChillReturnTemp(0.0);          // reporting: evaporator inlet temperature (was EvapInletTemp)
        Real64 lChillSupplyTemp(0.0);          // reporting: evaporator outlet temperature (was EvapOutletTemp)
        Real64 lChillWaterMassFlowRate(0.0);   // reporting: evaporator mass flow rate (was Evapmdot)
        Real64 lCondReturnTemp(0.0);           // reporting: condenser inlet temperature (was CondInletTemp)
        Real64 lCondSupplyTemp(0.0);           // reporting: condenser outlet temperature (was CondOutletTemp)
        Real64 lCondWaterMassFlowRate(0.0);    // reporting: condenser mass flow rate (was Condmdot)
        Real64 lCoolPartLoadRatio(0.0);        // operating part load ratio (load/capacity for cooling)
        Real64 lHeatPartLoadRatio(0.0);        // operating part load ratio (load/capacity for heating)
        Real64 lAvailableCoolingCapacity(0.0); // current capacity after temperature adjustment
        Real64 lFractionOfPeriodRunning(0.0);
        Real64 PartLoadRat(0.0);                // actual operating part load ratio of unit (ranges from minplr to 1)
        Real64 lChillWaterMassflowratemax(0.0); // Maximum flow rate through the evaporator
        Real64 lExhaustInTemp(0.0);             // Exhaust inlet temperature
        Real64 lExhaustInFlow(0.0);             // Exhaust inlet flow rate
        Real64 lExhHeatRecPotentialCool(0.0);   // Exhaust heat recovery potential during cooling
        Real64 lExhaustAirHumRat(0.0);
        // other local variables
        Real64 ChillDeltaTemp; // chilled water temperature difference
        Real64 ChillSupplySetPointTemp(0.0);
        Real64 calcCondTemp; // the condenser temperature used for curve calculation
        // either return or supply depending on user input
        Real64 revisedEstimateAvailCap; // final estimate of available capacity if using leaving
        // condenser water temperature
        Real64 errorAvailCap; // error fraction on final estimate of AvailableCoolingCapacity
        int LoopNum;
        int LoopSideNum;
        Real64 Cp_CW;      // local fluid specific heat for chilled water
        Real64 Cp_CD = -1; // local fluid specific heat for condenser water -- initializing to negative to ensure it isn't used uninitialized
        Real64 CpAir;      // specific heat of exhaust air

        // define constant values

        // set node values to data structure values for nodes

        lChillReturnNodeNum = this->ChillReturnNodeNum;
        lChillSupplyNodeNum = this->ChillSupplyNodeNum;
        lCondReturnNodeNum = this->CondReturnNodeNum;
        lExhaustAirInletNodeNum = this->ExhaustAirInletNodeNum;

        // set local copies of data from rest of input structure
        lNomCoolingCap = this->NomCoolingCap;
        lThermalEnergyCoolRatio = this->ThermalEnergyCoolRatio;
        lThermalEnergyHeatRatio = this->ThermalEnergyHeatRatio;
        lElecCoolRatio = this->ElecCoolRatio;
        lMinPartLoadRat = this->MinPartLoadRat;
        lMaxPartLoadRat = this->MaxPartLoadRat;
        lCoolCapFTCurve = this->CoolCapFTCurve;
        lThermalEnergyCoolFTCurve = this->ThermalEnergyCoolFTCurve;
        lThermalEnergyCoolFPLRCurve = this->ThermalEnergyCoolFPLRCurve;
        lElecCoolFTCurve = this->ElecCoolFTCurve;
        lElecCoolFPLRCurve = this->ElecCoolFPLRCurve;
        lIsEnterCondensTemp = this->isEnterCondensTemp;
        lIsWaterCooled = this->isWaterCooled;
        lCHWLowLimitTemp = this->CHWLowLimitTemp;
        lHeatElectricPower = this->HeatElectricPower;
        lHeatThermalEnergyUseRate = this->HeatThermalEnergyUseRate;
        lHeatPartLoadRatio = this->HeatPartLoadRatio;

        // initialize entering conditions
        lChillReturnTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp;
        lChillWaterMassFlowRate = DataLoopNode::Node(lChillReturnNodeNum).MassFlowRate;
        lCondReturnTemp = DataLoopNode::Node(lCondReturnNodeNum).Temp;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                ChillSupplySetPointTemp = DataLoopNode::Node(lChillSupplyNodeNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                ChillSupplySetPointTemp = DataLoopNode::Node(lChillSupplyNodeNum).TempSetPointHi;
            } else {
                assert(false);
            }
        }
        ChillDeltaTemp = std::abs(lChillReturnTemp - ChillSupplySetPointTemp);
        lExhaustInTemp = DataLoopNode::Node(lExhaustAirInletNodeNum).Temp;
        lExhaustInFlow = DataLoopNode::Node(lExhaustAirInletNodeNum).MassFlowRate;
        lExhaustAirHumRat = DataLoopNode::Node(lExhaustAirInletNodeNum).HumRat;

        Cp_CW = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->CWLoopNum).FluidName, lChillReturnTemp, DataPlant::PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);
        if (this->CDLoopNum > 0) {
            Cp_CD = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->CDLoopNum).FluidName, lChillReturnTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
        }

        // If no loop demand or Absorber OFF, return
        // will need to modify when absorber can act as a boiler
        if (MyLoad >= 0 || !((this->InHeatingMode) || (this->InCoolingMode))) {
            // set node temperatures
            lChillSupplyTemp = lChillReturnTemp;
            lCondSupplyTemp = lCondReturnTemp;
            lCondWaterMassFlowRate = 0.0;
            if (lIsWaterCooled) {
                PlantUtilities::SetComponentFlowRate(lCondWaterMassFlowRate,
                                                     this->CondReturnNodeNum,
                                                     this->CondSupplyNodeNum,
                                                     this->CDLoopNum,
                                                     this->CDLoopSideNum,
                                                     this->CDBranchNum,
                                                     this->CDCompNum);
            }
            lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);

        } else {

            // if water cooled use the input node otherwise just use outside air temperature
            if (lIsWaterCooled) {
                // most manufacturers rate have tables of entering condenser water temperature
                // but a few use leaving condenser water temperature so we have a flag
                // when leaving is used it uses the previous iterations value of the value
                lCondReturnTemp = DataLoopNode::Node(lCondReturnNodeNum).Temp;
                if (lIsEnterCondensTemp) {
                    calcCondTemp = lCondReturnTemp;
                } else {
                    if (this->oldCondSupplyTemp == 0) {
                        this->oldCondSupplyTemp = lCondReturnTemp + 8.0; // if not previously estimated assume 8C greater than return
                    }
                    calcCondTemp = this->oldCondSupplyTemp;
                }
                // Set mass flow rates
                lCondWaterMassFlowRate = this->DesCondMassFlowRate;
                PlantUtilities::SetComponentFlowRate(lCondWaterMassFlowRate,
                                                     this->CondReturnNodeNum,
                                                     this->CondSupplyNodeNum,
                                                     this->CDLoopNum,
                                                     this->CDLoopSideNum,
                                                     this->CDBranchNum,
                                                     this->CDCompNum);
            } else {
                // air cooled
                DataLoopNode::Node(lCondReturnNodeNum).Temp = DataLoopNode::Node(lCondReturnNodeNum).OutAirDryBulb;
                calcCondTemp = DataLoopNode::Node(lCondReturnNodeNum).OutAirDryBulb;
                lCondReturnTemp = DataLoopNode::Node(lCondReturnNodeNum).Temp;
                lCondWaterMassFlowRate = 0.0;
                if (this->CDLoopNum > 0) {
                    PlantUtilities::SetComponentFlowRate(lCondWaterMassFlowRate,
                                                         this->CondReturnNodeNum,
                                                         this->CondSupplyNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            // Determine available cooling capacity using the setpoint temperature
            lAvailableCoolingCapacity = lNomCoolingCap * CurveManager::CurveValue(lCoolCapFTCurve, ChillSupplySetPointTemp, calcCondTemp);

            // Calculate current load for cooling
            MyLoad = sign(max(std::abs(MyLoad), lAvailableCoolingCapacity * lMinPartLoadRat), MyLoad);
            MyLoad = sign(min(std::abs(MyLoad), lAvailableCoolingCapacity * lMaxPartLoadRat), MyLoad);

            // Determine the following variables depending on if the flow has been set in
            // the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
            //    chilled water flow,
            //    cooling load taken by the chiller, and
            //    supply temperature
            lChillWaterMassflowratemax = this->DesEvapMassFlowRate;

            LoopNum = this->CWLoopNum;
            LoopSideNum = this->CWLoopSideNum;
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock);
                if (SELECT_CASE_var == 0) { // mass flow rates may be changed by loop components
                    this->PossibleSubcooling = false;
                    lCoolingLoad = std::abs(MyLoad);
                    if (ChillDeltaTemp != 0.0) {
                        lChillWaterMassFlowRate = std::abs(lCoolingLoad / (Cp_CW * ChillDeltaTemp));
                        if (lChillWaterMassFlowRate - lChillWaterMassflowratemax > DataBranchAirLoopPlant::MassFlowTolerance)
                            this->PossibleSubcooling = true;

                        PlantUtilities::SetComponentFlowRate(lChillWaterMassFlowRate,
                                                             this->ChillReturnNodeNum,
                                                             this->ChillSupplyNodeNum,
                                                             this->CWLoopNum,
                                                             this->CWLoopSideNum,
                                                             this->CWBranchNum,
                                                             this->CWCompNum);
                    } else {
                        lChillWaterMassFlowRate = 0.0;
                        ShowRecurringWarningErrorAtEnd("ExhaustAbsorberChillerModel:Cooling\"" + this->Name +
                                                           "\", DeltaTemp = 0 in mass flow calculation",
                                                       this->DeltaTempCoolErrCount);
                    }
                    lChillSupplyTemp = ChillSupplySetPointTemp;
                } else if (SELECT_CASE_var == 1) { // mass flow rates may not be changed by loop components
                    lChillWaterMassFlowRate = DataLoopNode::Node(lChillReturnNodeNum).MassFlowRate;
                    if (this->PossibleSubcooling) {
                        lCoolingLoad = std::abs(MyLoad);

                        ChillDeltaTemp = lCoolingLoad / lChillWaterMassFlowRate / Cp_CW;
                        lChillSupplyTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - ChillDeltaTemp;
                    } else {

                        ChillDeltaTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - ChillSupplySetPointTemp;
                        lCoolingLoad = std::abs(lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp);
                        lChillSupplyTemp = ChillSupplySetPointTemp;
                    }
                    // Check that the Chiller Supply outlet temp honors both plant loop temp low limit and also the chiller low limit
                    if (lChillSupplyTemp < lCHWLowLimitTemp) {
                        if ((DataLoopNode::Node(lChillReturnNodeNum).Temp - lCHWLowLimitTemp) > DataPlant::DeltaTempTol) {
                            lChillSupplyTemp = lCHWLowLimitTemp;
                            ChillDeltaTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                            lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                        } else {
                            lChillSupplyTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp;
                            ChillDeltaTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                            lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                        }
                    }
                    if (lChillSupplyTemp < DataLoopNode::Node(lChillSupplyNodeNum).TempMin) {
                        if ((DataLoopNode::Node(lChillReturnNodeNum).Temp - DataLoopNode::Node(lChillSupplyNodeNum).TempMin) >
                            DataPlant::DeltaTempTol) {
                            lChillSupplyTemp = DataLoopNode::Node(lChillSupplyNodeNum).TempMin;
                            ChillDeltaTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                            lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                        } else {
                            lChillSupplyTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp;
                            ChillDeltaTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                            lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                        }
                    }

                    // Checks Coolingload on the basis of the machine limits.
                    if (lCoolingLoad > std::abs(MyLoad)) {
                        if (lChillWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                            lCoolingLoad = std::abs(MyLoad);
                            ChillDeltaTemp = lCoolingLoad / lChillWaterMassFlowRate / Cp_CW;
                            lChillSupplyTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - ChillDeltaTemp;
                        } else {
                            lChillSupplyTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp;
                            ChillDeltaTemp = DataLoopNode::Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                            lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                        }
                    }
                }
            }

            // Calculate operating part load ratio for cooling
            PartLoadRat = min(std::abs(MyLoad) / lAvailableCoolingCapacity, lMaxPartLoadRat);
            PartLoadRat = max(lMinPartLoadRat, PartLoadRat);

            if (lAvailableCoolingCapacity > 0.0) {
                if (std::abs(MyLoad) / lAvailableCoolingCapacity < lMinPartLoadRat) {
                    lCoolPartLoadRatio = MyLoad / lAvailableCoolingCapacity;
                } else {
                    lCoolPartLoadRatio = PartLoadRat;
                }
            } else { // Else if AvailableCoolingCapacity < 0.0
                lCoolPartLoadRatio = 0.0;
            }

            // calculate the fraction of the time period that the chiller would be running
            // use maximum from heating and cooling sides
            if (lCoolPartLoadRatio < lMinPartLoadRat || lHeatPartLoadRatio < lMinPartLoadRat) {
                lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
            } else {
                lFractionOfPeriodRunning = 1.0;
            }

            // Calculate thermal energy consumption for cooling
            // Thermal Energy used for cooling availCap * TeFIR * TeFIR-FT * TeFIR-FPLR
            lCoolThermalEnergyUseRate = lAvailableCoolingCapacity * lThermalEnergyCoolRatio *
                                        CurveManager::CurveValue(lThermalEnergyCoolFTCurve, lChillSupplyTemp, calcCondTemp) *
                                        CurveManager::CurveValue(lThermalEnergyCoolFPLRCurve, lCoolPartLoadRatio) * lFractionOfPeriodRunning;

            // Calculate electric parasitics used
            // based on nominal capacity, not available capacity,
            // electric used for cooling nomCap * %OP * EIR * EIR-FT * EIR-FPLR
            lCoolElectricPower = lNomCoolingCap * lElecCoolRatio * lFractionOfPeriodRunning *
                                 CurveManager::CurveValue(lElecCoolFTCurve, lChillSupplyTemp, calcCondTemp) *
                                 CurveManager::CurveValue(lElecCoolFPLRCurve, lCoolPartLoadRatio);

            // determine conderser load which is cooling load plus the
            // ThermalEnergy used for cooling plus
            // the electricity used
            lTowerLoad = lCoolingLoad + lCoolThermalEnergyUseRate / lThermalEnergyHeatRatio + lCoolElectricPower;

            lExhaustInTemp = DataLoopNode::Node(lExhaustAirInletNodeNum).Temp;
            lExhaustInFlow = DataLoopNode::Node(lExhaustAirInletNodeNum).MassFlowRate;
            CpAir = Psychrometrics::PsyCpAirFnW(lExhaustAirHumRat);
            lExhHeatRecPotentialCool = lExhaustInFlow * CpAir * (lExhaustInTemp - AbsLeavingTemp);
            // If Microturbine Exhaust temperature and flow rate is not sufficient to run the chiller, then chiller will not run
            // lCoolThermalEnergyUseRate , lTowerLoad and  lCoolElectricPower will be set to 0.0

            if (lExhHeatRecPotentialCool < lCoolThermalEnergyUseRate) {
                if (this->ExhTempLTAbsLeavingTempIndex == 0) {
                    ShowWarningError("ChillerHeater:Absorption:DoubleEffect \"" + this->Name + "\"");
                    ShowContinueError(
                        "...Exhaust temperature and flow input from Micro Turbine is not sufficient during cooling to run the chiller ");
                    ShowContinueError("...Value of Exhaust air inlet temp =" + General::TrimSigDigits(lExhaustInTemp, 4) + " C.");
                    ShowContinueError("... and Exhaust air flow rate of " + General::TrimSigDigits(lExhaustInFlow, 2) + " kg/s.");
                    ShowContinueError("...Value of minimum absorber leaving temp =" + General::TrimSigDigits(AbsLeavingTemp, 4) + " C.");
                    ShowContinueError("...Either increase the Exhaust temperature (min required = 350 C )  or flow or both of Micro Turbine to meet "
                                      "the min available potential criteria.");
                    ShowContinueErrorTimeStamp("... Simulation will continue.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "ChillerHeater:Absorption:DoubleEffect \"" + this->Name +
                        "\": Exhaust temperature from Micro Turbine is not sufficient to run the chiller during cooling warning continues...",
                    this->ExhTempLTAbsLeavingTempIndex,
                    lExhaustInTemp,
                    AbsLeavingTemp);
                // If exhaust is not available, it means the avilable thermal energy is 0.0 and Chiller is not available
                lCoolThermalEnergyUseRate = 0.0;
                lTowerLoad = 0.0;
                lCoolElectricPower = 0.0;
                lChillSupplyTemp = lChillReturnTemp;
                lCondSupplyTemp = lCondReturnTemp;
                lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
            }
            // for water cooled condenser make sure enough flow rate
            // for air cooled condenser just set supply to return temperature
            if (lIsWaterCooled) {
                if (lCondWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    lCondSupplyTemp = lCondReturnTemp + lTowerLoad / (lCondWaterMassFlowRate * Cp_CD);
                } else {
                    ShowSevereError("CalcExhaustAbsorberChillerModel: Condenser flow = 0, for Exhaust Absorber Chiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowFatalError("Program Terminates due to previous error condition.");
                }
            } else {
                lCondSupplyTemp = lCondReturnTemp; // if air cooled condenser just set supply and return to same temperature
            }

            // save the condenser water supply temperature for next iteration if that is used in lookup
            // and if capacity is large enough error than report problem
            this->oldCondSupplyTemp = lCondSupplyTemp;
            if (!lIsEnterCondensTemp) {
                // calculate the fraction of the estimated error between the capacity based on the previous
                // iteration's value of condenser supply temperature and the actual calculated condenser supply
                // temperature.  If this becomes too common then may need to iterate a solution instead of
                // relying on previous iteration method.
                revisedEstimateAvailCap = lNomCoolingCap * CurveManager::CurveValue(lCoolCapFTCurve, ChillSupplySetPointTemp, lCondSupplyTemp);
                if (revisedEstimateAvailCap > 0.0) {
                    errorAvailCap = std::abs((revisedEstimateAvailCap - lAvailableCoolingCapacity) / revisedEstimateAvailCap);
                    if (errorAvailCap > 0.05) { // if more than 5% error in estimate
                        ShowRecurringWarningErrorAtEnd("ExhaustAbsorberChillerModel:\"" + this->Name + "\", poor Condenser Supply Estimate",
                                                       this->CondErrCount,
                                                       errorAvailCap,
                                                       errorAvailCap);
                    }
                }
            }
        } // IF(MyLoad>=0 .OR. .NOT. RunFlag)
        // Write into the Report Variables except for nodes
        this->CoolingLoad = lCoolingLoad;
        this->TowerLoad = lTowerLoad;
        this->CoolThermalEnergyUseRate = lCoolThermalEnergyUseRate;
        this->CoolElectricPower = lCoolElectricPower;
        this->CondReturnTemp = lCondReturnTemp;
        this->ChillReturnTemp = lChillReturnTemp;
        this->CondSupplyTemp = lCondSupplyTemp;
        this->ChillSupplyTemp = lChillSupplyTemp;
        this->ChillWaterFlowRate = lChillWaterMassFlowRate;
        this->CondWaterFlowRate = lCondWaterMassFlowRate;
        this->CoolPartLoadRatio = lCoolPartLoadRatio;
        this->CoolingCapacity = lAvailableCoolingCapacity;
        this->FractionOfPeriodRunning = lFractionOfPeriodRunning;
        this->ExhaustInTemp = lExhaustInTemp;
        this->ExhaustInFlow = lExhaustInFlow;
        this->ExhHeatRecPotentialCool = lExhHeatRecPotentialCool;

        // write the combined heating and cooling ThermalEnergy used and electric used
        this->ThermalEnergyUseRate = lCoolThermalEnergyUseRate + lHeatThermalEnergyUseRate;
        this->ElectricPower = lCoolElectricPower + lHeatElectricPower;
    }

    void ExhaustAbsorberSpecs::calcHeater(Real64 &MyLoad, bool RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer and Michael J. Witte
        //       DATE WRITTEN   March 2001
        //       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate exhaust fired double effect absorption chiller
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a Exhaust fired (Exhaust consuming) absorption chiller using
        // curves and inputs similar to DOE-2.1e

        // METHODOLOGY EMPLOYED:
        // Curve fit of performance data

        // REFERENCES:
        // 1.  DOE-2.1e Supplement and source code
        // 2.  CoolTools GasMod work

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // FlowLock = 0  if mass flow rates may be changed by loop components
        // FlowLock = 1  if mass flow rates may not be changed by loop components
        // FlowLock = 2  if overloaded and mass flow rates has changed to a small amount and Tout drops
        //                 below Setpoint

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const AbsLeavingTemp(176.667); // C - Minimum temperature leaving the Chiller absorber (350 F)
        static std::string const RoutineName("CalcExhaustAbsorberHeaterModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // Local copies of ExhaustAbsorberSpecs Type
        // all variables that are local copies of data structure
        // variables are prefaced with an "l" for local.
        Real64 lNomCoolingCap;            // W - design nominal capacity of Absorber
        Real64 lNomHeatCoolRatio;         // ratio of heating to cooling capacity
        Real64 lThermalEnergyHeatRatio;   // ratio of ThermalEnergy input to heating output
        Real64 lElecHeatRatio;            // ratio of electricity input to heating output
        int lHeatReturnNodeNum;           // absorber hot water inlet node number, water side
        int lHeatSupplyNodeNum;           // absorber hot water outlet node number, water side
        Real64 lMinPartLoadRat;           // min allowed operating frac full load
        Real64 lMaxPartLoadRat;           // max allowed operating frac full load
        int lHeatCapFCoolCurve;           // Heating Capacity Function of Cooling Capacity Curve
        int lThermalEnergyHeatFHPLRCurve; // ThermalEnergy Input to heat output ratio during heating only function
        // Local copies of ExhaustAbsorberReportVars Type
        Real64 lHeatingLoad(0.0); // heating load on the chiller
        // Real64 lHeatingEnergy( 0.0 ); // heating energy
        // Real64 lThermalEnergyUseRate( 0.0 ); // instantaneous use of Thermal Energy for period
        // Real64 lThermalEnergy( 0.0 ); // variable to track total Thermal Energy used for a period (reference only)
        Real64 lCoolThermalEnergyUseRate(0.0); // instantaneous use of thermal energy for period for cooling
        Real64 lHeatThermalEnergyUseRate(0.0); // instantaneous use of thermal energy for period for heating
        Real64 lCoolElectricPower(0.0);        // parasitic electric power used  for cooling
        Real64 lHeatElectricPower(0.0);        // parasitic electric power used  for heating
        Real64 lHotWaterReturnTemp(0.0);       // reporting: hot water return (inlet) temperature
        Real64 lHotWaterSupplyTemp(0.0);       // reporting: hot water supply (outlet) temperature
        Real64 lHotWaterMassFlowRate(0.0);     // reporting: hot water mass flow rate
        Real64 lCoolPartLoadRatio(0.0);        // operating part load ratio (load/capacity for cooling)
        Real64 lHeatPartLoadRatio(0.0);        // operating part load ratio (load/capacity for heating)
        Real64 lAvailableHeatingCapacity(0.0); // current heating capacity
        Real64 lFractionOfPeriodRunning(0.0);
        Real64 lExhaustInTemp(0.0);           // Exhaust inlet temperature
        Real64 lExhaustInFlow(0.0);           // Exhaust inlet flow rate
        Real64 lExhHeatRecPotentialHeat(0.0); // Exhaust heat recovery potential
        Real64 lExhaustAirHumRat(0.0);
        // other local variables
        Real64 HeatDeltaTemp(0.0); // hot water temperature difference
        Real64 HeatSupplySetPointTemp(0.0);
        int LoopNum;
        int LoopSideNum;
        Real64 Cp_HW; // local fluid specific heat for hot water
        Real64 CpAir;
        int lExhaustAirInletNodeNum; // Combustion Air Inlet Node number

        // set node values to data structure values for nodes

        lHeatReturnNodeNum = this->HeatReturnNodeNum;
        lHeatSupplyNodeNum = this->HeatSupplyNodeNum;
        lExhaustAirInletNodeNum = this->ExhaustAirInletNodeNum;

        // set local copies of data from rest of input structure

        lNomCoolingCap = this->NomCoolingCap;
        lNomHeatCoolRatio = this->NomHeatCoolRatio;
        lThermalEnergyHeatRatio = this->ThermalEnergyHeatRatio;
        lElecHeatRatio = this->ElecHeatRatio;
        lMinPartLoadRat = this->MinPartLoadRat;
        lMaxPartLoadRat = this->MaxPartLoadRat;
        lHeatCapFCoolCurve = this->HeatCapFCoolCurve;
        lThermalEnergyHeatFHPLRCurve = this->ThermalEnergyHeatFHPLRCurve;
        LoopNum = this->HWLoopNum;
        LoopSideNum = this->HWLoopSideNum;

        Cp_HW = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(LoopNum).FluidName, lHotWaterReturnTemp, DataPlant::PlantLoop(LoopNum).FluidIndex, RoutineName);

        lCoolElectricPower = this->CoolElectricPower;
        lCoolThermalEnergyUseRate = this->CoolThermalEnergyUseRate;
        lCoolPartLoadRatio = this->CoolPartLoadRatio;

        // initialize entering conditions
        lHotWaterReturnTemp = DataLoopNode::Node(lHeatReturnNodeNum).Temp;
        lHotWaterMassFlowRate = DataLoopNode::Node(lHeatReturnNodeNum).MassFlowRate;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                HeatSupplySetPointTemp = DataLoopNode::Node(lHeatSupplyNodeNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                HeatSupplySetPointTemp = DataLoopNode::Node(lHeatSupplyNodeNum).TempSetPointLo;
            } else {
                assert(false);
            }
        }
        HeatDeltaTemp = std::abs(lHotWaterReturnTemp - HeatSupplySetPointTemp);

        // If no loop demand or Absorber OFF, return
        // will need to modify when absorber can act as a boiler
        if (MyLoad <= 0 || !RunFlag) {
            // set node temperatures
            lHotWaterSupplyTemp = lHotWaterReturnTemp;
            lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
        } else {

            // Determine available heating capacity using the current cooling load
            lAvailableHeatingCapacity = this->NomHeatCoolRatio * this->NomCoolingCap *
                                        CurveManager::CurveValue(lHeatCapFCoolCurve, (this->CoolingLoad / this->NomCoolingCap));

            // Calculate current load for heating
            MyLoad = sign(max(std::abs(MyLoad), this->HeatingCapacity * lMinPartLoadRat), MyLoad);
            MyLoad = sign(min(std::abs(MyLoad), this->HeatingCapacity * lMaxPartLoadRat), MyLoad);

            // Determine the following variables depending on if the flow has been set in
            // the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
            //    chilled water flow,
            //    cooling load taken by the chiller, and
            //    supply temperature
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock);
                if (SELECT_CASE_var == 0) { // mass flow rates may be changed by loop components
                    lHeatingLoad = std::abs(MyLoad);
                    if (HeatDeltaTemp != 0) {
                        lHotWaterMassFlowRate = std::abs(lHeatingLoad / (Cp_HW * HeatDeltaTemp));

                        PlantUtilities::SetComponentFlowRate(lHotWaterMassFlowRate,
                                                             this->HeatReturnNodeNum,
                                                             this->HeatSupplyNodeNum,
                                                             this->HWLoopNum,
                                                             this->HWLoopSideNum,
                                                             this->HWBranchNum,
                                                             this->HWCompNum);

                    } else {
                        lHotWaterMassFlowRate = 0.0;
                        ShowRecurringWarningErrorAtEnd("ExhaustAbsorberChillerModel:Heating\"" + this->Name +
                                                           "\", DeltaTemp = 0 in mass flow calculation",
                                                       this->DeltaTempHeatErrCount);
                    }
                    lHotWaterSupplyTemp = HeatSupplySetPointTemp;
                } else if (SELECT_CASE_var == 1) { // mass flow rates may not be changed by loop components
                    lHotWaterSupplyTemp = HeatSupplySetPointTemp;
                    lHeatingLoad = std::abs(lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp);

                    // DSU this "2" is not a real state for flowLock
                } else if (SELECT_CASE_var ==
                           2) { // chiller is underloaded and mass flow rates has changed to a small amount and Tout drops below Setpoint

                    // MJW 07MAR01 Borrow logic from steam absorption module
                    // The following conditional statements are made to avoid extremely small EvapMdot
                    // & unreasonable EvapOutletTemp due to overloading.
                    // Avoid 'divide by zero' due to small EvapMdot
                    if (lHotWaterMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                        HeatDeltaTemp = 0.0;
                    } else {
                        HeatDeltaTemp = std::abs(MyLoad) / (Cp_HW * lHotWaterMassFlowRate);
                    }
                    lHotWaterSupplyTemp = lHotWaterReturnTemp + HeatDeltaTemp;

                    lHeatingLoad = std::abs(lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp);
                }
            }

            // Calculate operating part load ratio for cooling
            lHeatPartLoadRatio = lHeatingLoad / lAvailableHeatingCapacity;

            // Calculate ThermalEnergy consumption for heating
            // ThermalEnergy used for heating availCap * HIR * HIR-FT * HIR-FPLR

            lHeatThermalEnergyUseRate =
                lAvailableHeatingCapacity * lThermalEnergyHeatRatio * CurveManager::CurveValue(lThermalEnergyHeatFHPLRCurve, lHeatPartLoadRatio);

            // calculate the fraction of the time period that the chiller would be running
            // use maximum from heating and cooling sides
            lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);

            // Calculate electric parasitics used
            // for heating based on nominal capacity not available capacity
            lHeatElectricPower = lNomCoolingCap * lNomHeatCoolRatio * lElecHeatRatio * lFractionOfPeriodRunning;
            // Coodinate electric parasitics for heating and cooling to avoid double counting
            // Total electric is the max of heating electric or cooling electric
            // If heating electric is greater, leave cooling electric and subtract if off of heating elec
            // If cooling electric is greater, set heating electric to zero

            lExhaustInTemp = DataLoopNode::Node(lExhaustAirInletNodeNum).Temp;
            lExhaustInFlow = DataLoopNode::Node(lExhaustAirInletNodeNum).MassFlowRate;
            CpAir = Psychrometrics::PsyCpAirFnW(lExhaustAirHumRat);
            lExhHeatRecPotentialHeat = lExhaustInFlow * CpAir * (lExhaustInTemp - AbsLeavingTemp);
            if (lExhHeatRecPotentialHeat < lHeatThermalEnergyUseRate) {
                if (this->ExhTempLTAbsLeavingHeatingTempIndex == 0) {
                    ShowWarningError("ChillerHeater:Absorption:DoubleEffect \"" + this->Name + "\"");
                    ShowContinueError(
                        "...Exhaust temperature and flow input from Micro Turbine is not sufficient to run the chiller during heating .");
                    ShowContinueError("...Value of Exhaust air inlet temp =" + General::TrimSigDigits(lExhaustInTemp, 4) + " C.");
                    ShowContinueError("... and Exhaust air flow rate of " + General::TrimSigDigits(lExhaustInFlow, 2) + " kg/s.");
                    ShowContinueError("...Value of minimum absorber leaving temp =" + General::TrimSigDigits(AbsLeavingTemp, 4) + " C.");
                    ShowContinueError("...Either increase the Exhaust temperature (min required = 350 C  )  or flow or both of Micro Turbine to meet "
                                      "the min available potential criteria.");
                    ShowContinueErrorTimeStamp("... Simulation will continue.");
                }
                ShowRecurringWarningErrorAtEnd(
                    "ChillerHeater:Absorption:DoubleEffect \"" + this->Name +
                        "\": Exhaust temperature from Micro Turbine is not sufficient to run the chiller during heating warning continues...",
                    this->ExhTempLTAbsLeavingHeatingTempIndex,
                    lExhaustInTemp,
                    AbsLeavingTemp);
                // If exhaust is not available, it means the avilable thermal energy is 0.0 and Chiller is not available
                lHeatThermalEnergyUseRate = 0.0;
                lHeatElectricPower = 0.0;
                lHotWaterSupplyTemp = lHotWaterReturnTemp;
                lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
            }

            if (lHeatElectricPower <= lCoolElectricPower) {
                lHeatElectricPower = 0.0;
            } else {
                lHeatElectricPower -= lCoolElectricPower;
            }

        } // IF(MyLoad==0 .OR. .NOT. RunFlag)
        // Write into the Report Variables except for nodes
        this->HeatingLoad = lHeatingLoad;
        this->HeatThermalEnergyUseRate = lHeatThermalEnergyUseRate;
        this->HeatElectricPower = lHeatElectricPower;
        this->HotWaterReturnTemp = lHotWaterReturnTemp;
        this->HotWaterSupplyTemp = lHotWaterSupplyTemp;
        this->HotWaterFlowRate = lHotWaterMassFlowRate;
        this->HeatPartLoadRatio = lHeatPartLoadRatio;
        this->HeatingCapacity = lAvailableHeatingCapacity;
        this->FractionOfPeriodRunning = lFractionOfPeriodRunning;

        // write the combined heating and cooling ThermalEnergy used and electric used
        this->ThermalEnergyUseRate = lCoolThermalEnergyUseRate + lHeatThermalEnergyUseRate;
        this->ElectricPower = lCoolElectricPower + lHeatElectricPower;
        this->ExhaustInTemp = lExhaustInTemp;
        this->ExhaustInFlow = lExhaustInFlow;
        this->ExhHeatRecPotentialHeat = lExhHeatRecPotentialHeat;
    }

    void ExhaustAbsorberSpecs::updateCoolRecords(Real64 MyLoad, bool RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   March 2001

        // PURPOSE OF THIS SUBROUTINE:
        // reporting

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int lChillReturnNodeNum;     // Node number on the inlet side of the plant
        int lChillSupplyNodeNum;     // Node number on the outlet side of the plant
        int lCondReturnNodeNum;      // Node number on the inlet side of the condenser
        int lCondSupplyNodeNum;      // Node number on the outlet side of the condenser
        int lExhaustAirInletNodeNum; // Node number on the inlet side of the plant
        Real64 RptConstant;

        lChillReturnNodeNum = this->ChillReturnNodeNum;
        lChillSupplyNodeNum = this->ChillSupplyNodeNum;
        lCondReturnNodeNum = this->CondReturnNodeNum;
        lCondSupplyNodeNum = this->CondSupplyNodeNum;

        lExhaustAirInletNodeNum = this->ExhaustAirInletNodeNum;
        if (MyLoad == 0 || !RunFlag) {
            DataLoopNode::Node(lChillSupplyNodeNum).Temp = DataLoopNode::Node(lChillReturnNodeNum).Temp;
            if (this->isWaterCooled) {
                DataLoopNode::Node(lCondSupplyNodeNum).Temp = DataLoopNode::Node(lCondReturnNodeNum).Temp;
            }
            DataLoopNode::Node(lExhaustAirInletNodeNum).Temp = DataLoopNode::Node(lExhaustAirInletNodeNum).Temp;
            DataLoopNode::Node(lExhaustAirInletNodeNum).MassFlowRate = this->ExhaustInFlow;
        } else {
            DataLoopNode::Node(lChillSupplyNodeNum).Temp = this->ChillSupplyTemp;
            if (this->isWaterCooled) {
                DataLoopNode::Node(lCondSupplyNodeNum).Temp = this->CondSupplyTemp;
            }
            DataLoopNode::Node(lExhaustAirInletNodeNum).Temp = this->ExhaustInTemp;
            DataLoopNode::Node(lExhaustAirInletNodeNum).MassFlowRate = this->ExhaustInFlow;
        }

        // convert power to energy and instantaneous use to use over the time step
        RptConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->CoolingEnergy = this->CoolingLoad * RptConstant;
        this->TowerEnergy = this->TowerLoad * RptConstant;
        this->ThermalEnergy = this->ThermalEnergyUseRate * RptConstant;
        this->CoolThermalEnergy = this->CoolThermalEnergyUseRate * RptConstant;
        this->ElectricEnergy = this->ElectricPower * RptConstant;
        this->CoolElectricEnergy = this->CoolElectricPower * RptConstant;
        if (this->CoolThermalEnergyUseRate != 0.0) {
            this->ThermalEnergyCOP = this->CoolingLoad / this->CoolThermalEnergyUseRate;
        } else {
            this->ThermalEnergyCOP = 0.0;
        }
    }

    void ExhaustAbsorberSpecs::updateHeatRecords(Real64 MyLoad, bool RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   March 2001

        // PURPOSE OF THIS SUBROUTINE:
        // reporting

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int lHeatReturnNodeNum; // absorber steam inlet node number, water side
        int lHeatSupplyNodeNum; // absorber steam outlet node number, water side
        Real64 RptConstant;

        lHeatReturnNodeNum = this->HeatReturnNodeNum;
        lHeatSupplyNodeNum = this->HeatSupplyNodeNum;

        if (MyLoad == 0 || !RunFlag) {
            DataLoopNode::Node(lHeatSupplyNodeNum).Temp = DataLoopNode::Node(lHeatReturnNodeNum).Temp;
        } else {
            DataLoopNode::Node(lHeatSupplyNodeNum).Temp = this->HotWaterSupplyTemp;
        }

        // convert power to energy and instantaneous use to use over the time step
        RptConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->HeatingEnergy = this->HeatingLoad * RptConstant;
        this->ThermalEnergy = this->ThermalEnergyUseRate * RptConstant;
        this->HeatThermalEnergy = this->HeatThermalEnergyUseRate * RptConstant;
        this->ElectricEnergy = this->ElectricPower * RptConstant;
        this->HeatElectricEnergy = this->HeatElectricPower * RptConstant;
    }

} // namespace ChillerExhaustAbsorption

} // namespace EnergyPlus
