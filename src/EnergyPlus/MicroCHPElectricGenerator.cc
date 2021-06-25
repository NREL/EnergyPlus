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
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGenerators.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneratorDynamicsManager.hh>
#include <EnergyPlus/GeneratorFuelSupply.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::MicroCHPElectricGenerator {

// MODULE INFORMATION:
//       AUTHOR         Brent Griffith
//       DATE WRITTEN   June 2006
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module simulates the operation of Internal Combustion and Stirling Engine
//  residential-scale generators for combined heat and power.

// METHODOLOGY EMPLOYED:
// Once the ElectricPowerManager determines that the Combustion Generator
// is available to meet an electric load demand, it calls SimCombustionGenerator
// which in turn calls the Combustion model.
// See DataFuelCells.cc for structures and variables

// REFERENCES:
// IEA/ECBCS Annex 42 model specification titled: " A Generic Model Specification for
// Combustion-based Residential CHP Devices"  Alex Ferguson, Nick Kelly, IEA Annex 42.
// Module developed from

PlantComponent *MicroCHPDataStruct::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data
    if (state.dataCHPElectGen->getMicroCHPInputFlag) {
        GetMicroCHPGeneratorInput(state);
        state.dataCHPElectGen->getMicroCHPInputFlag = false;
    }

    // Now look for this object
    for (auto &thisMCHP : state.dataCHPElectGen->MicroCHP) {
        if (thisMCHP.Name == objectName) {
            return &thisMCHP;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "LocalMicroCHPGenFactory: Error getting inputs for micro-CHP gen named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void GetMicroCHPGeneratorInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Brent Griffith
    //       DATE WRITTEN:    July 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This routine will get the input
    // required by the Micro CHP Generator models.

    // METHODOLOGY EMPLOYED:
    // EnergyPlus input processor

    int NumAlphas;                 // Number of elements in the alpha array
    int NumNums;                   // Number of elements in the numeric array
    int IOStat;                    // IO Status when calling get input subroutine
    Array1D_string AlphArray(25);  // character string data
    Array1D<Real64> NumArray(200); // numeric data TODO deal with allocatable for extensible
    bool ErrorsFound(false);       // error flag

    if (state.dataCHPElectGen->MyOneTimeFlag) {

        // call to Fuel supply module to set up data there.
        GeneratorFuelSupply::GetGeneratorFuelSupplyInput(state);

        // First get the Micro CHP Parameters so they can be nested in structure later
        state.dataIPShortCut->cCurrentModuleObject = "Generator:MicroCHP:NonNormalizedParameters";
        state.dataCHPElectGen->NumMicroCHPParams =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataCHPElectGen->NumMicroCHPParams <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        state.dataCHPElectGen->MicroCHPParamInput.allocate(state.dataCHPElectGen->NumMicroCHPParams);

        for (int CHPParamNum = 1; CHPParamNum <= state.dataCHPElectGen->NumMicroCHPParams; ++CHPParamNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     CHPParamNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            std::string ObjMSGName = state.dataIPShortCut->cCurrentModuleObject + " Named " + AlphArray(1);

            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).Name = AlphArray(1);        // A1 name
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MaxElecPower = NumArray(1); // N1 Maximum Electric Power [W]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MinElecPower = NumArray(2); // N2 Minimum Electric Power [W]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MinWaterMdot = NumArray(3); // N3 Minimum Cooling Water Flow Rate [kg/s]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MaxWaterTemp = NumArray(4); // N3 Maximum Cooling Water Inlet Temp [C]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).ElecEffCurveID =
                CurveManager::GetCurveCheck(state, AlphArray(2), ErrorsFound, ObjMSGName); // Electrical Eff. ID
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).ThermalEffCurveID =
                CurveManager::GetCurveCheck(state, AlphArray(3), ErrorsFound, ObjMSGName); // Thermal Efficiency

            if (UtilityRoutines::SameString(AlphArray(4), "InternalControl")) {
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).InternalFlowControl = true; //  A4, \field Cooling Water Flow Rate Mode
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).PlantFlowControl = false;
            }
            if ((!(UtilityRoutines::SameString(AlphArray(4), "InternalControl"))) && (!(UtilityRoutines::SameString(AlphArray(4), "PlantControl")))) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + AlphArray(4));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }
            if (state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).InternalFlowControl) { // get the curve
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).WaterFlowCurveID =
                    CurveManager::GetCurveCheck(state, AlphArray(5), ErrorsFound, ObjMSGName);
                //  Curve for Cooling Water Flow Rate
            }
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).AirFlowCurveID =
                CurveManager::GetCurveCheck(state, AlphArray(6), ErrorsFound, ObjMSGName);
            //  Name of Curve for Air Flow Rate
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).DeltaPelMax =
                NumArray(5); // N5 Maximum rate of change in net electrical power [W/s]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).DeltaFuelMdotMax =
                NumArray(6);                                                             // N6 Maximum Rate of change in fuel flow rate [kg/s2]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).UAhx = NumArray(7);   // N7 Heat Exchanger UA_hx
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).UAskin = NumArray(8); // N8 Skin Loss UA_loss
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).RadiativeFraction = NumArray(9); // N9 radiative fraction for skin losses
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MCeng = NumArray(10);            // N10 Aggregated Thermal Mass of Generator MC_eng
            if (state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MCeng <= 0.0) {
                ShowSevereError(state, format("Invalid, {} = {:.5R}", state.dataIPShortCut->cNumericFieldNames(10), NumArray(10)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Thermal mass must be greater than zero");
                ErrorsFound = true;
            }
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MCcw = NumArray(11); // Aggregated Thermal Mass of Heat Recovery MC_cw
            if (state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MCcw <= 0.0) {
                ShowSevereError(state, format("Invalid, {} = {:.5R}", state.dataIPShortCut->cNumericFieldNames(11), NumArray(11)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Thermal mass must be greater than zero");
                ErrorsFound = true;
            }
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).Pstandby = NumArray(12); // N12 Standby Power [W]

            if (UtilityRoutines::SameString(AlphArray(7), "TimeDelay")) {
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).WarmUpByTimeDelay = true;
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).WarmUpByEngineTemp = false;
            }
            if ((!(UtilityRoutines::SameString(AlphArray(7), "NominalEngineTemperature"))) &&
                (!(UtilityRoutines::SameString(AlphArray(7), "TimeDelay")))) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(7) + " = " + AlphArray(7));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).kf = NumArray(13);          // N13 Warmup Fuel Flow Rate Coefficient k_f
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).TnomEngOp = NumArray(14);   // N14 Nominal Engine Operating Temperature [C]
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).kp = NumArray(15);          // N15 Warmup Power Coefficient k_p
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).Rfuelwarmup = NumArray(16); // N16 Warm Up Fuel Flow Rate Limit Ratio
            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).WarmUpDelay = NumArray(17); // N17 Warm Up Delay Time

            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).PcoolDown = NumArray(18); // N18 Cool Down Power

            state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).CoolDownDelay = NumArray(19); // N19 Cool Down Delay Time in seconds

            if (UtilityRoutines::SameString(AlphArray(8), "MandatoryCoolDown")) {
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).MandatoryFullCoolDown = true;
                state.dataCHPElectGen->MicroCHPParamInput(CHPParamNum).WarmRestartOkay = false;
            }
            if ((!(UtilityRoutines::SameString(AlphArray(8), "MandatoryCoolDown"))) &&
                (!(UtilityRoutines::SameString(AlphArray(8), "OptionalCoolDown")))) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(8) + " = " + AlphArray(8));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }
        }

        state.dataIPShortCut->cCurrentModuleObject = "Generator:MicroCHP";
        state.dataCHPElectGen->NumMicroCHPs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataCHPElectGen->NumMicroCHPs <= 0) {
            // shouldn't ever come here?
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        if (!(allocated(state.dataCHPElectGen->MicroCHP))) {
            state.dataCHPElectGen->MicroCHP.allocate(state.dataCHPElectGen->NumMicroCHPs); // inits handeled in derived type definitions
        }

        // load in Micro CHPs
        for (int GeneratorNum = 1; GeneratorNum <= state.dataCHPElectGen->NumMicroCHPs; ++GeneratorNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     GeneratorNum,
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

            // GENERATOR:MICRO CHP,
            state.dataCHPElectGen->MicroCHP(GeneratorNum).DynamicsControlID = GeneratorNum;
            state.dataCHPElectGen->MicroCHP(GeneratorNum).Name = AlphArray(1);         //  A1 Generator name
            state.dataCHPElectGen->MicroCHP(GeneratorNum).ParamObjName = AlphArray(2); //  A2 Micro CHP Parameter Object Name
            // find input structure
            int thisParamID = UtilityRoutines::FindItemInList(AlphArray(2), state.dataCHPElectGen->MicroCHPParamInput);
            if (thisParamID != 0) {
                state.dataCHPElectGen->MicroCHP(GeneratorNum).A42Model =
                    state.dataCHPElectGen->MicroCHPParamInput(thisParamID); // entire structure of input data assigned here!
            } else {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + AlphArray(2));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                state.dataCHPElectGen->MicroCHP(GeneratorNum).ZoneName = AlphArray(3); //  A3 Zone Name
                state.dataCHPElectGen->MicroCHP(GeneratorNum).ZoneID =
                    UtilityRoutines::FindItemInList(state.dataCHPElectGen->MicroCHP(GeneratorNum).ZoneName, state.dataHeatBal->Zone);
                if (state.dataCHPElectGen->MicroCHP(GeneratorNum).ZoneID == 0) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + AlphArray(3));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            } else {
                state.dataCHPElectGen->MicroCHP(GeneratorNum).ZoneID = 0;
            }
            state.dataCHPElectGen->MicroCHP(GeneratorNum).PlantInletNodeName = AlphArray(4);  //  A4 Cooling Water Inlet Node Name
            state.dataCHPElectGen->MicroCHP(GeneratorNum).PlantOutletNodeName = AlphArray(5); //  A5 Cooling Water Outlet Node Name
            // find node ids for water path
            state.dataCHPElectGen->MicroCHP(GeneratorNum).PlantInletNodeID =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(4),
                                                    ErrorsFound,
                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    NodeInputManager::compFluidStream::Primary,
                                                    DataLoopNode::ObjectIsNotParent);
            state.dataCHPElectGen->MicroCHP(GeneratorNum).PlantOutletNodeID =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(5),
                                                    ErrorsFound,
                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                    NodeInputManager::compFluidStream::Primary,
                                                    DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(
                state, state.dataIPShortCut->cCurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Heat Recovery Nodes");

            state.dataCHPElectGen->MicroCHP(GeneratorNum).AirInletNodeName = AlphArray(6); //  A6 Air Inlet Node Name
            // check the node connections
            state.dataCHPElectGen->MicroCHP(GeneratorNum).AirInletNodeID =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(6),
                                                    ErrorsFound,
                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    NodeInputManager::compFluidStream::Secondary,
                                                    DataLoopNode::ObjectIsNotParent);

            state.dataCHPElectGen->MicroCHP(GeneratorNum).AirOutletNodeName = AlphArray(7); //  A7 Air Outlet Node Name
            state.dataCHPElectGen->MicroCHP(GeneratorNum).AirOutletNodeID =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(7),
                                                    ErrorsFound,
                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                    NodeInputManager::compFluidStream::Secondary,
                                                    DataLoopNode::ObjectIsNotParent);

            state.dataCHPElectGen->MicroCHP(GeneratorNum).FuelSupplyID =
                UtilityRoutines::FindItemInList(AlphArray(8), state.dataGenerator->FuelSupply); // Fuel Supply ID
            if (state.dataCHPElectGen->MicroCHP(GeneratorNum).FuelSupplyID == 0) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(8) + " = " + AlphArray(8));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                state.dataCHPElectGen->MicroCHP(GeneratorNum).AvailabilitySchedID = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataCHPElectGen->MicroCHP(GeneratorNum).AvailabilitySchedID = ScheduleManager::GetScheduleIndex(state, AlphArray(9));
                if (state.dataCHPElectGen->MicroCHP(GeneratorNum).AvailabilitySchedID == 0) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(9) + " = " + AlphArray(9));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }
            state.dataCHPElectGen->MicroCHP(GeneratorNum).A42Model.TengLast = 20.0;      // inits
            state.dataCHPElectGen->MicroCHP(GeneratorNum).A42Model.TempCWOutLast = 20.0; // inits
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }

        // setup report variables
        for (int GeneratorNum = 1; GeneratorNum <= state.dataCHPElectGen->NumMicroCHPs; ++GeneratorNum) {
        }

        state.dataCHPElectGen->MyOneTimeFlag = false;
    }
}

void MicroCHPDataStruct::setupOutputVars(EnergyPlusData &state)
{
    SetupOutputVariable(state, "Generator Off Mode Time", OutputProcessor::Unit::s, this->A42Model.OffModeTime, "System", "Sum", this->Name);

    SetupOutputVariable(state, "Generator Standby Mode Time", OutputProcessor::Unit::s, this->A42Model.StandyByModeTime, "System", "Sum", this->Name);

    SetupOutputVariable(state, "Generator Warm Up Mode Time", OutputProcessor::Unit::s, this->A42Model.WarmUpModeTime, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Generator Normal Operating Mode Time", OutputProcessor::Unit::s, this->A42Model.NormalModeTime, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Generator Cool Down Mode Time", OutputProcessor::Unit::s, this->A42Model.CoolDownModeTime, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Generator Produced AC Electricity Rate", OutputProcessor::Unit::W, this->A42Model.ACPowerGen, "System", "Average", this->Name);

    SetupOutputVariable(state,
                        "Generator Produced AC Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->A42Model.ACEnergyGen,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        "ElectricityProduced",
                        "COGENERATION",
                        _,
                        "Plant");

    SetupOutputVariable(state, "Generator Produced Thermal Rate", OutputProcessor::Unit::W, this->A42Model.QdotHR, "system", "Average", this->Name);

    SetupOutputVariable(state,
                        "Generator Produced Thermal Energy",
                        OutputProcessor::Unit::J,
                        this->A42Model.TotalHeatEnergyRec,
                        "system",
                        "Sum",
                        this->Name,
                        _,
                        "ENERGYTRANSFER",
                        "COGENERATION",
                        _,
                        "Plant");

    SetupOutputVariable(state, "Generator Electric Efficiency", OutputProcessor::Unit::None, this->A42Model.ElecEff, "System", "Average", this->Name);

    SetupOutputVariable(state, "Generator Thermal Efficiency", OutputProcessor::Unit::None, this->A42Model.ThermEff, "System", "Average", this->Name);

    SetupOutputVariable(state, "Generator Gross Input Heat Rate", OutputProcessor::Unit::W, this->A42Model.Qgross, "system", "Average", this->Name);

    SetupOutputVariable(state,
                        "Generator Steady State Engine Heat Generation Rate",
                        OutputProcessor::Unit::W,
                        this->A42Model.Qgenss,
                        "system",
                        "Average",
                        this->Name);

    SetupOutputVariable(
        state, "Generator Engine Heat Exchange Rate", OutputProcessor::Unit::W, this->A42Model.QdotHX, "system", "Average", this->Name);

    SetupOutputVariable(state, "Generator Air Mass Flow Rate", OutputProcessor::Unit::kg_s, this->A42Model.MdotAir, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Generator Fuel Molar Flow Rate", OutputProcessor::Unit::kmol_s, this->A42Model.NdotFuel, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Generator Fuel Mass Flow Rate", OutputProcessor::Unit::kg_s, this->A42Model.MdotFuel, "System", "Average", this->Name);

    SetupOutputVariable(state, "Generator Engine Temperature", OutputProcessor::Unit::C, this->A42Model.Teng, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Generator Coolant Inlet Temperature", OutputProcessor::Unit::C, this->A42Model.HeatRecInletTemp, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Generator Coolant Outlet Temperature", OutputProcessor::Unit::C, this->A42Model.HeatRecOutletTemp, "System", "Average", this->Name);

    // this next one needs to be reconciled with non-gas fuel constituents.
    //   need custom resourceTypeKey or something for user defined fuel compositions.
    SetupOutputVariable(state,
                        "Generator Fuel HHV Basis Energy",
                        OutputProcessor::Unit::J,
                        this->A42Model.FuelEnergyHHV,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        "NaturalGas",
                        "COGENERATION",
                        _,
                        "Plant");

    SetupOutputVariable(
        state, "Generator Fuel HHV Basis Rate", OutputProcessor::Unit::W, this->A42Model.FuelEnergyUseRateHHV, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Generator Fuel LHV Basis Energy", OutputProcessor::Unit::J, this->A42Model.FuelEnergyLHV, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Generator Fuel LHV Basis Rate", OutputProcessor::Unit::W, this->A42Model.FuelEnergyUseRateLHV, "System", "Average", this->Name);

    SetupOutputVariable(state,
                        "Generator Fuel Compressor Electricity Rate",
                        OutputProcessor::Unit::W,
                        this->A42Model.FuelCompressPower,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Generator Fuel Compressor Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->A42Model.FuelCompressEnergy,
                        "System",
                        "Sum",
                        this->Name);

    SetupOutputVariable(state,
                        "Generator Fuel Compressor Skin Heat Loss Rate",
                        OutputProcessor::Unit::W,
                        this->A42Model.FuelCompressSkinLoss,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(
        state, "Generator Zone Sensible Heat Transfer Rate", OutputProcessor::Unit::W, this->A42Model.SkinLossPower, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Generator Zone Sensible Heat Transfer Energy", OutputProcessor::Unit::J, this->A42Model.SkinLossEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(state,
                        "Generator Zone Convection Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->A42Model.SkinLossConvect,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Generator Zone Radiation Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->A42Model.SkinLossRadiat,
                        "System",
                        "Average",
                        this->Name);

    if (this->ZoneID > 0) {
        SetupZoneInternalGain(state,
                              this->ZoneID,
                              "Generator:MicroCHP",
                              this->Name,
                              DataHeatBalance::IntGainTypeOf_GeneratorMicroCHP,
                              &this->A42Model.SkinLossConvect,
                              nullptr,
                              &this->A42Model.SkinLossRadiat);
    }
}

void MicroCHPDataStruct::simulate(EnergyPlusData &state,
                                  [[maybe_unused]] const EnergyPlus::PlantLocation &calledFromLocation,
                                  bool FirstHVACIteration,
                                  [[maybe_unused]] Real64 &CurLoad,
                                  [[maybe_unused]] bool RunFlag)
{
    // empty function to emulate current behavior as of conversion to using the PlantComponent calling structure.
    // calls from the plant side only update the nodes.
    // calls from the ElectricPowerServiceManger call the init, calc, and update worker functions

    PlantUtilities::UpdateComponentHeatRecoverySide(state,
                                                    this->CWLoopNum,
                                                    this->CWLoopSideNum,
                                                    DataPlant::TypeOf_Generator_MicroCHP,
                                                    this->PlantInletNodeID,
                                                    this->PlantOutletNodeID,
                                                    this->A42Model.QdotHR,
                                                    this->A42Model.HeatRecInletTemp,
                                                    this->A42Model.HeatRecOutletTemp,
                                                    this->PlantMassFlowRate,
                                                    FirstHVACIteration);
}

void MicroCHPDataStruct::onInitLoopEquip(EnergyPlusData &state, const EnergyPlus::PlantLocation &)
{
    static constexpr std::string_view RoutineName("MicroCHPDataStruct::onInitLoopEquip");

    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                   state.dataLoopNodes->Node(this->PlantInletNodeID).Temp,
                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                   RoutineName);
    if (this->A42Model.InternalFlowControl) { // got a curve
        this->PlantMassFlowRateMax =
            2.0 * CurveManager::CurveValue(
                      state, this->A42Model.WaterFlowCurveID, this->A42Model.MaxElecPower, state.dataLoopNodes->Node(this->PlantInletNodeID).Temp);
    } else if (this->CWLoopSideNum == DataPlant::SupplySide) {
        if (state.dataPlnt->PlantLoop(this->CWLoopNum).MaxMassFlowRate > 0.0) {
            this->PlantMassFlowRateMax = state.dataPlnt->PlantLoop(this->CWLoopNum).MaxMassFlowRate;
        } else if (state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum > 0) {
            this->PlantMassFlowRateMax = state.dataSize->PlantSizData(this->CWLoopNum).DesVolFlowRate * rho;
        } else {
            this->PlantMassFlowRateMax = 2.0;
        }

    } else if (this->CWLoopSideNum == DataPlant::DemandSide) {
        this->PlantMassFlowRateMax = 2.0; // would like to use plant loop max but not ready yet
    }

    PlantUtilities::RegisterPlantCompDesignFlow(state, this->PlantInletNodeID, this->PlantMassFlowRateMax / rho);

    this->A42Model.ElecEff = CurveManager::CurveValue(state,
                                                      this->A42Model.ElecEffCurveID,
                                                      this->A42Model.MaxElecPower,
                                                      this->PlantMassFlowRateMax,
                                                      state.dataLoopNodes->Node(this->PlantInletNodeID).Temp);

    this->A42Model.ThermEff = CurveManager::CurveValue(state,
                                                       this->A42Model.ThermalEffCurveID,
                                                       this->A42Model.MaxElecPower,
                                                       this->PlantMassFlowRateMax,
                                                       state.dataLoopNodes->Node(this->PlantInletNodeID).Temp);

    GeneratorDynamicsManager::SetupGeneratorControlStateManager(state, this->DynamicsControlID);
}

void MicroCHPDataStruct::InitMicroCHPNoNormalizeGenerators(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         BGriffith
    //       DATE WRITTEN   March 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    bool errFlag;

    if (this->myFlag) {
        this->setupOutputVars(state);
        this->myFlag = false;
    }

    if (this->MyPlantScanFlag && allocated(state.dataPlnt->PlantLoop)) {
        errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_Generator_MicroCHP,
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

        if (errFlag) {
            ShowFatalError(state, "InitMicroCHPNoNormalizeGenerators: Program terminated for previous conditions.");
        }

        if (!this->A42Model.InternalFlowControl) {
            // IF this is on the supply side and not internal flow control then reset flow priority to lower
            if (this->CWLoopSideNum == DataPlant::SupplySide) {
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_TakesWhatGets;
            }
        }

        this->MyPlantScanFlag = false;
    }

    if (!state.dataGlobal->SysSizingCalc && this->MySizeFlag && !this->MyPlantScanFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {
        this->MySizeFlag = false;
    }

    if (this->MySizeFlag) return;

    int DynaCntrlNum = this->DynamicsControlID;

    if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag) {
        // reset to starting condition for different environment runperiods, design days
        this->A42Model.TengLast = 20.0;
        this->A42Model.TempCWOutLast = 20.0;
        this->A42Model.TimeElapsed = 0.0;
        this->A42Model.OpMode = DataGenerators::OperatingMode::Unassigned;
        this->A42Model.OffModeTime = 0.0;
        this->A42Model.StandyByModeTime = 0.0;
        this->A42Model.WarmUpModeTime = 0.0;
        this->A42Model.NormalModeTime = 0.0;
        this->A42Model.CoolDownModeTime = 0.0;
        this->A42Model.Pnet = 0.0;
        this->A42Model.ElecEff = 0.0;
        this->A42Model.Qgross = 0.0;
        this->A42Model.ThermEff = 0.0;
        this->A42Model.Qgenss = 0.0;
        this->A42Model.NdotFuel = 0.0;
        this->A42Model.MdotFuel = 0.0;
        this->A42Model.Teng = 20.0;
        this->A42Model.TcwIn = 20.0;
        this->A42Model.TcwOut = 20.0;
        this->A42Model.MdotAir = 0.0;
        this->A42Model.QdotSkin = 0.0;
        this->A42Model.QdotConvZone = 0.0;
        this->A42Model.QdotRadZone = 0.0;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).LastOpMode = DataGenerators::OperatingMode::OpModeOff;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).CurrentOpMode = DataGenerators::OperatingMode::OpModeOff;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown = 0.0;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).FractionalDayofLastStartUp = 0.0;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).HasBeenOn = false;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).DuringStartUp = false;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).DuringShutDown = false;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep = 0.0;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).PelLastTimeStep = 0.0;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).NumCycles = 0;

        state.dataGenerator->FuelSupply(this->FuelSupplyID).QskinLoss = 0.0;

        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->PlantMassFlowRateMax,
                                           this->PlantInletNodeID,
                                           this->PlantOutletNodeID,
                                           this->CWLoopNum,
                                           this->CWLoopSideNum,
                                           this->CWBranchNum,
                                           this->CWCompNum);
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->MyEnvrnFlag = true;
    }

    Real64 TimeElapsed =
        state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed;
    if (this->A42Model.TimeElapsed != TimeElapsed) {
        // The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
        // timestep for use as the initial conditions of each iteration that does not advance the system timestep.
        this->A42Model.TengLast = this->A42Model.Teng;
        this->A42Model.TempCWOutLast = this->A42Model.TcwOut;
        this->A42Model.TimeElapsed = TimeElapsed;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).LastOpMode = state.dataGenerator->GeneratorDynamics(DynaCntrlNum).CurrentOpMode;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep = this->A42Model.MdotFuel;
        state.dataGenerator->GeneratorDynamics(DynaCntrlNum).PelLastTimeStep = this->A42Model.Pnet;
    }

    if (!this->A42Model.InternalFlowControl) {

        Real64 mdot = this->PlantMassFlowRateMax;
        PlantUtilities::SetComponentFlowRate(
            state, mdot, this->PlantInletNodeID, this->PlantOutletNodeID, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        this->PlantMassFlowRate = mdot;
    }
}

void MicroCHPDataStruct::CalcMicroCHPNoNormalizeGeneratorModel(EnergyPlusData &state,
                                                               bool const RunFlagElectCenter, // TRUE when Generator operating
                                                               bool const RunFlagPlant,
                                                               Real64 const MyElectricLoad, // Generator demand
                                                               Real64 const MyThermalLoad,
                                                               bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR        B Griffith
    //       DATE WRITTEN   July 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Main calculation subroutine for the IEA Annex 42 model

    // METHODOLOGY EMPLOYED:
    // curve fit, dynamic control limits,

    // REFERENCES:
    // IEA Annex 42 FC-COGEN-SIM "A Generic Model Specification for Combustion-based Residential CHP Devices"
    // Alex Ferguson, Nick Kelly, Version 3, June 26, 2006

    static constexpr std::string_view RoutineName("CalcMicroCHPNoNormalizeGeneratorModel");

    DataGenerators::OperatingMode CurrentOpMode = DataGenerators::OperatingMode::Unassigned;
    Real64 AllowedLoad = 0.0;
    Real64 PLRforSubtimestepStartUp(1.0);
    Real64 PLRforSubtimestepShutDown(0.0);
    bool RunFlag(false);

    GeneratorDynamicsManager::ManageGeneratorControlState(state,
                                                          GeneratorType::MicroCHP,
                                                          this->Name,
                                                          this->DynamicsControlID,
                                                          RunFlagElectCenter,
                                                          RunFlagPlant,
                                                          MyElectricLoad,
                                                          MyThermalLoad,
                                                          AllowedLoad,
                                                          CurrentOpMode,
                                                          PLRforSubtimestepStartUp,
                                                          PLRforSubtimestepShutDown,
                                                          FirstHVACIteration);

    if (RunFlagElectCenter || RunFlagPlant) RunFlag = true;

    Real64 Teng = this->A42Model.Teng;
    Real64 TcwOut = this->A42Model.TcwOut;

    Real64 thisAmbientTemp;
    if (this->ZoneID > 0) {
        thisAmbientTemp = state.dataHeatBalFanSys->MAT(this->ZoneID);
    } else { // outdoor location, no zone
        thisAmbientTemp = state.dataEnvrn->OutDryBulbTemp;
    }

    Real64 Pnetss = 0.0;
    Real64 Pstandby = 0.0; // power draw during standby, positive here means negative production
    Real64 Pcooler = 0.0;  // power draw during cool down, positive here means negative production
    Real64 NdotFuel = 0.0;
    Real64 ElecEff = 0.0;
    Real64 MdotAir = 0.0;
    Real64 Qgenss = 0.0;
    Real64 MdotCW = 0.0;
    Real64 TcwIn = 0.0;
    Real64 MdotFuel = 0.0;
    Real64 Qgross = 0.0;
    Real64 ThermEff = 0.0;

    {
        auto const SELECT_CASE_var(CurrentOpMode);

        if (SELECT_CASE_var == DataGenerators::OperatingMode::OpModeOff) { // same as standby in model spec but no Pnet standby electicity losses.

            Qgenss = 0.0;
            MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
            TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
            Pnetss = 0.0;
            Pstandby = 0.0;
            Pcooler = this->A42Model.PcoolDown * PLRforSubtimestepShutDown;
            ElecEff = 0.0;
            ThermEff = 0.0;
            Qgross = 0.0;
            NdotFuel = 0.0;
            MdotFuel = 0.0;
            MdotAir = 0.0;

            MdotCW = 0.0;
            PlantUtilities::SetComponentFlowRate(state,
                                                 MdotCW,
                                                 this->PlantInletNodeID,
                                                 this->PlantOutletNodeID,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
            this->PlantMassFlowRate = MdotCW;

        } else if (SELECT_CASE_var == DataGenerators::OperatingMode::OpModeStandby) {
            Qgenss = 0.0;
            MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
            TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
            Pnetss = 0.0;
            Pstandby = this->A42Model.Pstandby * (1.0 - PLRforSubtimestepShutDown);
            Pcooler = this->A42Model.PcoolDown * PLRforSubtimestepShutDown;
            ElecEff = 0.0;
            ThermEff = 0.0;
            Qgross = 0.0;
            NdotFuel = 0.0;
            MdotFuel = 0.0;
            MdotAir = 0.0;

            MdotCW = 0.0;
            PlantUtilities::SetComponentFlowRate(state,
                                                 MdotCW,
                                                 this->PlantInletNodeID,
                                                 this->PlantOutletNodeID,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
            this->PlantMassFlowRate = MdotCW;

        } else if (SELECT_CASE_var == DataGenerators::OperatingMode::OpModeWarmUp) {

            if (this->A42Model.WarmUpByTimeDelay) {
                // Internal combustion engine.  This is just like normal  operation but no net power yet.
                Pnetss = MyElectricLoad; // W
                Pstandby = 0.0;
                Pcooler = this->A42Model.PcoolDown * PLRforSubtimestepShutDown;
                TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
                MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
                if (this->A42Model.InternalFlowControl) {
                    MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(state, this->DynamicsControlID, Pnetss, TcwIn);
                }
                ElecEff = CurveManager::CurveValue(state, this->A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                ElecEff = max(0.0, ElecEff); // protect against bad curve result

                if (ElecEff > 0.0) {           // trap divide by bad thing
                    Qgross = Pnetss / ElecEff; // W
                } else {
                    Qgross = 0.0;
                }
                ThermEff = CurveManager::CurveValue(state, this->A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                ThermEff = max(0.0, ThermEff); // protect against bad curve result

                Qgenss = ThermEff * Qgross; // W

                MdotFuel = Qgross / (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0) *
                           state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
                //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)

                bool ConstrainedIncreasingNdot(false);
                bool ConstrainedDecreasingNdot(false);
                Real64 MdotFuelAllowed = 0.0;

                GeneratorDynamicsManager::ManageGeneratorFuelFlow(state,
                                                                  GeneratorType::MicroCHP,
                                                                  this->Name,
                                                                  this->DynamicsControlID,
                                                                  RunFlag,
                                                                  MdotFuel,
                                                                  MdotFuelAllowed,
                                                                  ConstrainedIncreasingNdot,
                                                                  ConstrainedDecreasingNdot);

                if (ConstrainedIncreasingNdot || ConstrainedDecreasingNdot) { // recalculate Pnetss with new NdotFuel with iteration
                    MdotFuel = MdotFuelAllowed;
                    NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
                    Qgross = NdotFuel * (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0);

                    for (int i = 1; i <= 20; ++i) { // iterating here  could add use of seach method
                        Pnetss = Qgross * ElecEff;
                        if (this->A42Model.InternalFlowControl) {
                            MdotCW =
                                GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(state, this->DynamicsControlID, Pnetss, TcwIn);
                        }
                        ElecEff = CurveManager::CurveValue(state, this->A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                        ElecEff = max(0.0, ElecEff); // protect against bad curve result
                    }

                    ThermEff = CurveManager::CurveValue(state, this->A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                    ThermEff = max(0.0, ThermEff); // protect against bad curve result
                    Qgenss = ThermEff * Qgross;    // W
                }
                Pnetss = 0.0; // no actually power produced here.
                NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
                MdotAir = CurveManager::CurveValue(state, this->A42Model.AirFlowCurveID, MdotFuel);
                MdotAir = max(0.0, MdotAir); // protect against bad curve result

            } else if (this->A42Model.WarmUpByEngineTemp) {
                // Stirling engine mode warm up
                //   find MdotFuelMax
                Real64 Pmax = this->A42Model.MaxElecPower;
                Pstandby = 0.0;
                Pcooler = this->A42Model.PcoolDown * PLRforSubtimestepShutDown;          // could be here with part load in cool down
                TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
                MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
                ElecEff = CurveManager::CurveValue(state, this->A42Model.ElecEffCurveID, Pmax, MdotCW, TcwIn);
                ElecEff = max(0.0, ElecEff); // protect against bad curve result
                if (ElecEff > 0.0) {         // trap divide by bad thing
                    Qgross = Pmax / ElecEff; // W
                } else {
                    Qgross = 0.0;
                }
                NdotFuel = Qgross / (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0);
                //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
                Real64 MdotFuelMax = NdotFuel * state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;

                Real64 MdotFuelWarmup;
                if (Teng > thisAmbientTemp) {
                    MdotFuelWarmup =
                        MdotFuelMax + this->A42Model.kf * MdotFuelMax * ((this->A42Model.TnomEngOp - thisAmbientTemp) / (Teng - thisAmbientTemp));
                    // check that numerical answer didn't blow up beyond limit, and reset if it did
                    if (MdotFuelWarmup > this->A42Model.Rfuelwarmup * MdotFuelMax) {
                        MdotFuelWarmup = this->A42Model.Rfuelwarmup * MdotFuelMax;
                    }
                } else { // equal would divide by zero
                    MdotFuelWarmup = this->A42Model.Rfuelwarmup * MdotFuelMax;
                }

                if (this->A42Model.TnomEngOp > thisAmbientTemp) {
                    Pnetss = Pmax * this->A42Model.kp * ((Teng - thisAmbientTemp) / (this->A42Model.TnomEngOp - thisAmbientTemp));
                } else { // equal would divide by zero
                    Pnetss = Pmax;
                }

                MdotFuel = MdotFuelWarmup;
                NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
                MdotAir = CurveManager::CurveValue(state, this->A42Model.AirFlowCurveID, MdotFuelWarmup);
                MdotAir = max(0.0, MdotAir); // protect against bad curve result
                Qgross = NdotFuel * (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0);
                ThermEff = CurveManager::CurveValue(state, this->A42Model.ThermalEffCurveID, Pmax, MdotCW, TcwIn);
                Qgenss = ThermEff * Qgross; // W
            }
            NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;

        } else if (SELECT_CASE_var == DataGenerators::OperatingMode::OpModeNormal) {
            if (PLRforSubtimestepStartUp < 1.0) {
                if (RunFlagElectCenter) Pnetss = MyElectricLoad; // W
                if (RunFlagPlant) Pnetss = AllowedLoad;
            } else {
                Pnetss = AllowedLoad;
            }
            Pstandby = 0.0;
            Pcooler = 0.0;
            TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
            MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
            if (this->A42Model.InternalFlowControl) {
                MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(state, this->DynamicsControlID, Pnetss, TcwIn);
            }

            ElecEff = CurveManager::CurveValue(state, this->A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
            ElecEff = max(0.0, ElecEff); // protect against bad curve result

            if (ElecEff > 0.0) {           // trap divide by bad thing
                Qgross = Pnetss / ElecEff; // W
            } else {
                Qgross = 0.0;
            }

            ThermEff = CurveManager::CurveValue(state, this->A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
            ThermEff = max(0.0, ThermEff); // protect against bad curve result
            Qgenss = ThermEff * Qgross;    // W
            MdotFuel = Qgross / (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0) *
                       state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
            //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)

            bool ConstrainedIncreasingNdot(false);
            bool ConstrainedDecreasingNdot(false);
            Real64 MdotFuelAllowed = 0.0;

            GeneratorDynamicsManager::ManageGeneratorFuelFlow(state,
                                                              GeneratorType::MicroCHP,
                                                              this->Name,
                                                              this->DynamicsControlID,
                                                              RunFlag,
                                                              MdotFuel,
                                                              MdotFuelAllowed,
                                                              ConstrainedIncreasingNdot,
                                                              ConstrainedDecreasingNdot);

            if (ConstrainedIncreasingNdot || ConstrainedDecreasingNdot) { // recalculate Pnetss with new NdotFuel with iteration
                MdotFuel = MdotFuelAllowed;
                NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
                Qgross = NdotFuel * (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0);

                for (int i = 1; i <= 20; ++i) { // iterating here,  could add use of seach method error signal
                    Pnetss = Qgross * ElecEff;
                    if (this->A42Model.InternalFlowControl) {
                        MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(state, this->DynamicsControlID, Pnetss, TcwIn);
                    }
                    ElecEff = CurveManager::CurveValue(state, this->A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                    ElecEff = max(0.0, ElecEff); // protect against bad curve result
                }

                ThermEff = CurveManager::CurveValue(state, this->A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                ThermEff = max(0.0, ThermEff); // protect against bad curve result
                Qgenss = ThermEff * Qgross;    // W
            }

            NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
            MdotAir = CurveManager::CurveValue(state, this->A42Model.AirFlowCurveID, MdotFuel);
            MdotAir = max(0.0, MdotAir); // protect against bad curve result
            if (PLRforSubtimestepStartUp < 1.0) {
                Pnetss = AllowedLoad;
            }

        } else if (SELECT_CASE_var == DataGenerators::OperatingMode::OpModeCoolDown) {

            Pnetss = 0.0;
            Pstandby = 0.0;
            Pcooler = this->A42Model.PcoolDown;
            TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
            MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
            if (this->A42Model.InternalFlowControl) {
                MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(state, this->DynamicsControlID, Pnetss, TcwIn);
            }
            NdotFuel = 0.0;
            MdotFuel = 0.0;
            MdotAir = 0.0;
            ElecEff = 0.0;
            ThermEff = 0.0;
            Qgross = 0.0;
            Qgenss = 0.0;
        }
    }

    for (int i = 1; i <= 20; ++i) { // sequential search with exit criteria
        // calculate new value for engine temperature
        // for Stirling in warmup, need to include dependency of Qgness on Teng
        if ((this->A42Model.WarmUpByEngineTemp) && (CurrentOpMode == DataGenerators::OperatingMode::OpModeWarmUp)) {

            Real64 Pmax = this->A42Model.MaxElecPower;
            TcwIn = state.dataLoopNodes->Node(this->PlantInletNodeID).Temp;          // C
            MdotCW = state.dataLoopNodes->Node(this->PlantInletNodeID).MassFlowRate; // kg/s
            ElecEff = CurveManager::CurveValue(state, this->A42Model.ElecEffCurveID, Pmax, MdotCW, TcwIn);
            ElecEff = max(0.0, ElecEff); // protect against bad curve result
            if (ElecEff > 0.0) {         // trap divide by bad thing
                Qgross = Pmax / ElecEff; // W
            } else {
                Qgross = 0.0;
            }
            NdotFuel = Qgross / (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0);
            //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
            Real64 MdotFuelMax = NdotFuel * state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;

            Real64 MdotFuelWarmup;
            if (Teng > thisAmbientTemp) {
                MdotFuelWarmup =
                    MdotFuelMax + this->A42Model.kf * MdotFuelMax * ((this->A42Model.TnomEngOp - thisAmbientTemp) / (Teng - thisAmbientTemp));

                // check that numerical answer didn't blow up beyond limit, and reset if it did
                if (MdotFuelWarmup > this->A42Model.Rfuelwarmup * MdotFuelMax) {
                    MdotFuelWarmup = this->A42Model.Rfuelwarmup * MdotFuelMax;
                }
                if (this->A42Model.TnomEngOp > thisAmbientTemp) {
                    Pnetss = Pmax * this->A42Model.kp * ((Teng - thisAmbientTemp) / (this->A42Model.TnomEngOp - thisAmbientTemp));
                } else { // equal would divide by zero
                    Pnetss = Pmax;
                }
            } else { // equal would divide by zero
                MdotFuelWarmup = this->A42Model.Rfuelwarmup * MdotFuelMax;
            }
            MdotFuel = MdotFuelWarmup;
            NdotFuel = MdotFuel / state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
            MdotAir = CurveManager::CurveValue(state, this->A42Model.AirFlowCurveID, MdotFuelWarmup);
            MdotAir = max(0.0, MdotAir); // protect against bad curve result
            Qgross = NdotFuel * (state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000.0 * 1000.0);
            ThermEff = CurveManager::CurveValue(state, this->A42Model.ThermalEffCurveID, Pmax, MdotCW, TcwIn);
            ThermEff = max(0.0, ThermEff); // protect against bad curve result
            Qgenss = ThermEff * Qgross;    // W
        }

        Real64 dt = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        Teng = FuncDetermineEngineTemp(
            TcwOut, this->A42Model.MCeng, this->A42Model.UAhx, this->A42Model.UAskin, thisAmbientTemp, Qgenss, this->A42Model.TengLast, dt);

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            state, state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName, TcwIn, state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);

        TcwOut =
            FuncDetermineCoolantWaterExitTemp(TcwIn, this->A42Model.MCcw, this->A42Model.UAhx, MdotCW * Cp, Teng, this->A42Model.TempCWOutLast, dt);

        // form balance and exit once met.
        bool EnergyBalOK = CheckMicroCHPThermalBalance(this->A42Model.MaxElecPower,
                                                       TcwIn,
                                                       TcwOut,
                                                       Teng,
                                                       thisAmbientTemp,
                                                       this->A42Model.UAhx,
                                                       this->A42Model.UAskin,
                                                       Qgenss,
                                                       this->A42Model.MCeng,
                                                       this->A42Model.MCcw,
                                                       MdotCW * Cp);

        if (EnergyBalOK && (i > 4)) break;
    }

    this->PlantMassFlowRate = MdotCW;
    this->A42Model.Pnet = Pnetss - Pcooler - Pstandby;
    this->A42Model.ElecEff = ElecEff;
    this->A42Model.Qgross = Qgross;
    this->A42Model.ThermEff = ThermEff;
    this->A42Model.Qgenss = Qgenss;
    this->A42Model.NdotFuel = NdotFuel;
    this->A42Model.MdotFuel = MdotFuel;
    this->A42Model.Teng = Teng;
    this->A42Model.TcwOut = TcwOut;
    this->A42Model.TcwIn = TcwIn;
    this->A42Model.MdotAir = MdotAir;
    this->A42Model.QdotSkin = this->A42Model.UAskin * (Teng - thisAmbientTemp);

    this->A42Model.OpMode = CurrentOpMode;
}

Real64 FuncDetermineEngineTemp(Real64 const TcwOut,   // hot water leaving temp
                               Real64 const MCeng,    // Fictitious mass and heat capacity of engine
                               Real64 const UAHX,     // Heat exchanger UA
                               Real64 const UAskin,   // Skin losses UA
                               Real64 const Troom,    // surrounding zone temperature C
                               Real64 const Qgenss,   // steady state generator heat generation
                               Real64 const TengLast, // engine temp at previous time step
                               Real64 const time      // elapsed time since previous evaluation
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Feb. 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate engine temperaure,

    // METHODOLOGY EMPLOYED:
    // model is dynamic in that previous condition affects current timestep
    //  solve ode for engine temp using analytical solution

    Real64 a = ((UAHX * TcwOut / MCeng) + (UAskin * Troom / MCeng) + (Qgenss / MCeng));
    Real64 b = ((-1.0 * UAHX / MCeng) + (-1.0 * UAskin / MCeng));

    return (TengLast + a / b) * std::exp(b * time) - a / b;
}

Real64 FuncDetermineCoolantWaterExitTemp(Real64 const TcwIn,      // hot water inlet temp
                                         Real64 const MCcw,       // Fictitious mass and heat capacity of coolant hx
                                         Real64 const UAHX,       // Heat exchanger UA
                                         Real64 const MdotCpcw,   // mass flow and specific heat of coolant water
                                         Real64 const Teng,       // engine mass temperature C
                                         Real64 const TcwoutLast, // coolant water leaving temp at previous time step
                                         Real64 const time        // elapsed time since previous evaluation
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Feb. 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate coolan water leaving temperaure,

    // METHODOLOGY EMPLOYED:
    // model is dynamic in that previous condition affects current timestep
    //  solve ode for coolant water outlet temp using analytical solution

    Real64 a = (MdotCpcw * TcwIn / MCcw) + (UAHX * Teng / MCcw);
    Real64 b = ((-1.0 * MdotCpcw / MCcw) + (-1.0 * UAHX / MCcw));

    if (b * time < (-1.0 * DataGlobalConstants::MaxEXPArg)) {
        return -a / b;
    } else {
        return (TcwoutLast + a / b) * std::exp(b * time) - a / b;
    }
}

bool CheckMicroCHPThermalBalance(Real64 const NomHeatGen, // nominal heat generation rate for scaling
                                 Real64 const TcwIn,      // hot water inlet temp
                                 Real64 const TcwOut,     // hot water leaving temp
                                 Real64 const Teng,       // engine mass temperature C
                                 Real64 const Troom,      // surrounding zone temperature C
                                 Real64 const UAHX,       // Heat exchanger UA
                                 Real64 const UAskin,     // Skin losses UA
                                 Real64 const Qgenss,     // steady state generator heat generation
                                 Real64 const MCeng,      // Fictitious mass and heat capacity of engine
                                 Real64 const MCcw,       // Fictitious mass and heat capacity of coolant hx
                                 Real64 const MdotCpcw    // mass flow and specific heat of coolant water
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Feb. 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Check for energy balance to test if can exit iteration loop

    // METHODOLOGY EMPLOYED:
    // put all terms of dynamic energy balances on RHS and compute magnitude of imbalance
    //  compare imbalance to scalable thresholds and make a boolean conclusion.

    // first compute derivatives using a + bT
    // derivative of engine temp wrt time
    Real64 a = ((UAHX * TcwOut / MCeng) + (UAskin * Troom / MCeng) + (Qgenss / MCeng));
    Real64 b = ((-1.0 * UAHX / MCeng) + (-1.0 * UAskin / MCeng));
    Real64 DTengDTime = a + b * Teng;

    // derivative of coolant exit temp wrt time
    Real64 c = (MdotCpcw * TcwIn / MCcw) + (UAHX * Teng / MCcw);
    Real64 d = ((-1.0 * MdotCpcw / MCcw) + (-1.0 * UAHX / MCcw));
    Real64 DCoolOutTDtime = c + d * TcwOut;

    // energy imbalance for engine control volume
    Real64 magImbalEng = UAHX * (TcwOut - Teng) + UAskin * (Troom - Teng) + Qgenss - MCeng * DTengDTime;

    // energy imbalance for coolant control volume
    Real64 magImbalCooling = MdotCpcw * (TcwIn - TcwOut) + UAHX * (Teng - TcwOut) - MCcw * DCoolOutTDtime;

    // criteria for when to call energy balance okay
    Real64 threshold = NomHeatGen / 10000000.0;

    return (threshold > magImbalEng) && (threshold > magImbalCooling);
}

void FigureMicroCHPZoneGains(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Couple equipment skin losses to the Zone Heat Balance

    // METHODOLOGY EMPLOYED:
    // This routine adds up the various skin losses and then
    //  sets the values in the ZoneIntGain structure

    if (state.dataCHPElectGen->NumMicroCHPs == 0) return;

    if (state.dataGlobal->BeginEnvrnFlag && state.dataCHPElectGen->MyEnvrnFlag) {
        for (auto &e : state.dataGenerator->FuelSupply)
            e.QskinLoss = 0.0;
        for (auto &e : state.dataCHPElectGen->MicroCHP) {
            e.A42Model.QdotSkin = 0.0;
            e.A42Model.SkinLossConvect = 0.0;
            e.A42Model.SkinLossRadiat = 0.0;
        }
        state.dataCHPElectGen->MyEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) state.dataCHPElectGen->MyEnvrnFlag = true;

    for (int CHPnum = 1; CHPnum <= state.dataCHPElectGen->NumMicroCHPs; ++CHPnum) {
        Real64 TotalZoneHeatGain = state.dataGenerator->FuelSupply(state.dataCHPElectGen->MicroCHP(CHPnum).FuelSupplyID).QskinLoss +
                                   state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.QdotSkin;

        state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.QdotConvZone =
            TotalZoneHeatGain * (1 - state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.RadiativeFraction);
        state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.SkinLossConvect = state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.QdotConvZone;
        state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.QdotRadZone =
            TotalZoneHeatGain * state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.RadiativeFraction;
        state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.SkinLossRadiat = state.dataCHPElectGen->MicroCHP(CHPnum).A42Model.QdotRadZone;
    }
}

void MicroCHPDataStruct::CalcUpdateHeatRecovery(EnergyPlusData &state) const
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Aug 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // update plant loop interactions, do any calcs needed

    static constexpr std::string_view RoutineName("CalcUpdateHeatRecovery");

    PlantUtilities::SafeCopyPlantNode(state, this->PlantInletNodeID, this->PlantOutletNodeID);

    state.dataLoopNodes->Node(this->PlantOutletNodeID).Temp = this->A42Model.TcwOut;

    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                       this->A42Model.TcwIn,
                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                       RoutineName);

    state.dataLoopNodes->Node(this->PlantOutletNodeID).Enthalpy = this->A42Model.TcwOut * Cp;
}

void MicroCHPDataStruct::getDesignCapacities(
    [[maybe_unused]] EnergyPlusData &state, const EnergyPlus::PlantLocation &, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    MaxLoad = state.dataGenerator->GeneratorDynamics(this->DynamicsControlID).QdotHXMax;
    MinLoad = state.dataGenerator->GeneratorDynamics(this->DynamicsControlID).QdotHXMin;
    OptLoad = state.dataGenerator->GeneratorDynamics(this->DynamicsControlID).QdotHXOpt;
}

void MicroCHPDataStruct::UpdateMicroCHPGeneratorRecords(EnergyPlusData &state) // Generator number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // update variables in structures linked to output reports

    static constexpr std::string_view RoutineName("UpdateMicroCHPGeneratorRecords");

    this->A42Model.ACPowerGen = this->A42Model.Pnet; // electrical power produced [W]
    this->A42Model.ACEnergyGen = this->A42Model.Pnet * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour; // energy produced (J)
    this->A42Model.QdotHX = this->A42Model.UAhx * (this->A42Model.Teng - this->A42Model.TcwOut);                           //  heat recovered rate (W)

    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                       this->A42Model.TcwIn,
                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                       RoutineName);

    this->A42Model.QdotHR = this->PlantMassFlowRate * Cp * (this->A42Model.TcwOut - this->A42Model.TcwIn);
    this->A42Model.TotalHeatEnergyRec =
        this->A42Model.QdotHR * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour; // heat recovered energy (J)

    this->A42Model.HeatRecInletTemp = this->A42Model.TcwIn;   // Heat Recovery Loop Inlet Temperature (C)
    this->A42Model.HeatRecOutletTemp = this->A42Model.TcwOut; // Heat Recovery Loop Outlet Temperature (C)

    this->A42Model.FuelCompressPower = state.dataGenerator->FuelSupply(this->FuelSupplyID).PfuelCompEl;
    // electrical power used by fuel supply compressor [W]
    this->A42Model.FuelCompressEnergy = state.dataGenerator->FuelSupply(this->FuelSupplyID).PfuelCompEl * state.dataHVACGlobal->TimeStepSys *
                                        DataGlobalConstants::SecInHour; // elect energy
    this->A42Model.FuelCompressSkinLoss = state.dataGenerator->FuelSupply(this->FuelSupplyID).QskinLoss;
    // heat rate of losses.by fuel supply compressor [W]
    this->A42Model.FuelEnergyHHV = this->A42Model.NdotFuel * state.dataGenerator->FuelSupply(this->FuelSupplyID).HHV *
                                   state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec * state.dataHVACGlobal->TimeStepSys *
                                   DataGlobalConstants::SecInHour;
    // reporting: Fuel Energy used (W)
    this->A42Model.FuelEnergyUseRateHHV = this->A42Model.NdotFuel * state.dataGenerator->FuelSupply(this->FuelSupplyID).HHV *
                                          state.dataGenerator->FuelSupply(this->FuelSupplyID).KmolPerSecToKgPerSec;
    // reporting: Fuel Energy used (J)
    this->A42Model.FuelEnergyLHV = this->A42Model.NdotFuel * state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000000.0 *
                                   state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    // reporting: Fuel Energy used (W)
    this->A42Model.FuelEnergyUseRateLHV = this->A42Model.NdotFuel * state.dataGenerator->FuelSupply(this->FuelSupplyID).LHV * 1000000.0;

    this->A42Model.SkinLossPower = this->A42Model.QdotConvZone + this->A42Model.QdotRadZone;
    this->A42Model.SkinLossEnergy =
        (this->A42Model.QdotConvZone + this->A42Model.QdotRadZone) * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->A42Model.SkinLossConvect = this->A42Model.QdotConvZone;
    this->A42Model.SkinLossRadiat = this->A42Model.QdotRadZone;

    // update node data for air inlet (and outlet)
    if (this->AirInletNodeID > 0) {
        state.dataLoopNodes->Node(this->AirInletNodeID).MassFlowRate = this->A42Model.MdotAir;
    }
    if (this->AirOutletNodeID > 0) {
        state.dataLoopNodes->Node(this->AirOutletNodeID).MassFlowRate = this->A42Model.MdotAir;
        state.dataLoopNodes->Node(this->AirOutletNodeID).Temp = this->A42Model.Teng;
    }
}
} // namespace EnergyPlus::MicroCHPElectricGenerator
