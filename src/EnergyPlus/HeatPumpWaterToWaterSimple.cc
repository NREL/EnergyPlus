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

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatPumpWaterToWaterSimple.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::HeatPumpWaterToWaterSimple {

    // MODULE INFORMATION:
    //       AUTHOR         Kenneth Tang
    //       DATE WRITTEN   March 2005
    //       MODIFIED       Brent Griffith, plant upgrades, fluid properties
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates a Water-to-Water Heat Pump Simple (Equation-Fit Model)

    // METHODOLOGY EMPLOYED:
    // This simulation is based on a set of coefficients in quadlinear curves generated from
    // the manufacturer catalog data using the generalized least square method

    // REFERENCES:
    // (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
    // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
    // Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
    // (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
    // Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
    // M.S. Thesis, Department of Mechanical and Aerospace Engineering,
    // Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataLoopNode;

    // MODULE PARAMETER DEFINITIONS
    std::string const HPEqFitHeating("HeatPump:WatertoWater:EquationFit:Heating");
    std::string const HPEqFitHeatingUC("HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING");
    std::string const HPEqFitCooling("HeatPump:WatertoWater:EquationFit:Cooling");
    std::string const HPEqFitCoolingUC("HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING");

    PlantComponent *GshpSpecs::factory(EnergyPlusData &state, int wwhp_type, std::string eir_wwhp_name)
    {
        if (state.dataHPWaterToWaterSimple->GetInputFlag) {
            GshpSpecs::GetWatertoWaterHPInput(state);
            state.dataHPWaterToWaterSimple->GetInputFlag = false;
        }

        for (auto &wwhp : state.dataHPWaterToWaterSimple->GSHP) {
            if (wwhp.Name == eir_wwhp_name && wwhp.WWHPPlantTypeOfNum == wwhp_type) {
                return &wwhp;
            }
        }

        ShowFatalError(state, "EquationFit_WWHP factory: Error getting inputs for wwhp named: " + eir_wwhp_name);
        return nullptr;
    }

    void GshpSpecs::simulate(EnergyPlusData &state,
                             const PlantLocation &calledFromLocation,
                             bool const FirstHVACIteration,
                             Real64 &CurLoad,
                             [[maybe_unused]] bool const RunFlag)
    {
        if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFCooling) {
            if (calledFromLocation.loopNum == this->LoadLoopNum) { // chilled water loop
                this->InitWatertoWaterHP(state, this->WWHPPlantTypeOfNum, this->Name, FirstHVACIteration, CurLoad);
                this->CalcWatertoWaterHPCooling(state, CurLoad);
                this->UpdateGSHPRecords();
            } else if (calledFromLocation.loopNum == this->SourceLoopNum) { // condenser loop
                PlantUtilities::UpdateChillerComponentCondenserSide(state, this->SourceLoopNum,
                                                                    this->SourceLoopSideNum,
                                                                    DataPlant::TypeOf_HPWaterEFCooling,
                                                                    this->SourceSideInletNodeNum,
                                                                    this->SourceSideOutletNodeNum,
                                                                    this->reportQSource,
                                                                    this->reportSourceSideInletTemp,
                                                                    this->reportSourceSideOutletTemp,
                                                                    this->reportSourceSideMassFlowRate,
                                                                    FirstHVACIteration);
            } else {
                ShowFatalError(state, "SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + this->Name);
            }
        } else if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFHeating) {
            if (calledFromLocation.loopNum == this->LoadLoopNum) { // chilled water loop
                this->InitWatertoWaterHP(state, this->WWHPPlantTypeOfNum, this->Name, FirstHVACIteration, CurLoad);
                this->CalcWatertoWaterHPHeating(state, CurLoad);
                this->UpdateGSHPRecords();
            } else if (calledFromLocation.loopNum == this->SourceLoopNum) { // condenser loop
                PlantUtilities::UpdateChillerComponentCondenserSide(state, this->SourceLoopNum,
                                                                    this->SourceLoopSideNum,
                                                                    DataPlant::TypeOf_HPWaterEFHeating,
                                                                    this->SourceSideInletNodeNum,
                                                                    this->SourceSideOutletNodeNum,
                                                                    -this->reportQSource,
                                                                    this->reportSourceSideInletTemp,
                                                                    this->reportSourceSideOutletTemp,
                                                                    this->reportSourceSideMassFlowRate,
                                                                    FirstHVACIteration);
            } else {
                ShowFatalError(state, "SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + this->Name);
            }
        } else {
            ShowFatalError(state, "SimHPWatertoWaterSimple: Module called with incorrect GSHPType");
        } // TypeOfEquip
    }

    void GshpSpecs::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
    {
        bool initFirstHVAC = true;
        Real64 initCurLoad = 0.0;

        this->InitWatertoWaterHP(state, this->WWHPPlantTypeOfNum, this->Name, initFirstHVAC, initCurLoad);
        if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFCooling) {
            this->sizeCoolingWaterToWaterHP(state);
        } else if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFHeating) {
            this->sizeHeatingWaterToWaterHP(state);
        }
    }

    void GshpSpecs::getDesignCapacities(EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        if (calledFromLocation.loopNum == this->LoadLoopNum) {
            if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFCooling) {
                MinLoad = 0.0;
                MaxLoad = this->RatedCapCool;
                OptLoad = this->RatedCapCool;
            } else if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFHeating) {
                MinLoad = 0.0;
                MaxLoad = this->RatedCapHeat;
                OptLoad = this->RatedCapHeat;
            } else {
                ShowFatalError(state, "SimHPWatertoWaterSimple: Module called with incorrect GSHPType");
            }
        } else {
            MinLoad = 0.0;
            MaxLoad = 0.0;
            OptLoad = 0.0;
        }
    }

    void GshpSpecs::getSizingFactor(Real64 &sizingFactor)
    {
        sizingFactor = this->sizFac;
    }

    void GshpSpecs::GetWatertoWaterHPInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kenneth Tang
        //       DATE WRITTEN   March 2005
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtain input from IDF and store them in data structures

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataPlant::TypeOf_HPWaterEFCooling;
        using DataPlant::TypeOf_HPWaterEFHeating;
        using NodeInputManager::GetOnlySingleNode;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using CurveManager::GetCurveIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int GSHPNum;     // GSHP number
        int HPNum;       // Counter
        int NumCoolCoil; // Number of Cooling Coils
        int NumHeatCoil; // Number of Heating Coils
        int NumAlphas;   // Number of elements in the alpha array
        int NumNums;     // Number of elements in the numeric array
        int IOStat;      // IO Status when calling get input subroutine

        static bool ErrorsFound(false);

        NumCoolCoil = inputProcessor->getNumObjectsFound(state, HPEqFitCoolingUC);
        NumHeatCoil = inputProcessor->getNumObjectsFound(state, HPEqFitHeatingUC);
        state.dataHPWaterToWaterSimple->NumGSHPs = NumCoolCoil + NumHeatCoil;

        if (state.dataHPWaterToWaterSimple->NumGSHPs <= 0) {
            ShowSevereError(state, "GetEquationFitWaterToWater Input: No Equipment found");
            ErrorsFound = true;
        }

        if (state.dataHPWaterToWaterSimple->NumGSHPs > 0) {
            state.dataHPWaterToWaterSimple->GSHP.allocate(state.dataHPWaterToWaterSimple->NumGSHPs);
            state.dataHPWaterToWaterSimple->HeatPumpWaterUniqueNames.reserve(state.dataHPWaterToWaterSimple->NumGSHPs);
        }

        // Load data structure for cooling coil
        for (HPNum = 1; HPNum <= NumCoolCoil; ++HPNum) {

            GSHPNum = HPNum;

            inputProcessor->getObjectItem(state,
                                          HPEqFitCoolingUC,
                                          HPNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks);
            GlobalNames::VerifyUniqueInterObjectName(state, state.dataHPWaterToWaterSimple->HeatPumpWaterUniqueNames, DataIPShortCuts::cAlphaArgs(1), HPEqFitCoolingUC, ErrorsFound);
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).WWHPPlantTypeOfNum = TypeOf_HPWaterEFCooling;
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name = DataIPShortCuts::cAlphaArgs(1);
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedLoadVolFlowCool = DataIPShortCuts::rNumericArgs(1);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedLoadVolFlowCool == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedLoadVolFlowCoolWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedSourceVolFlowCool = DataIPShortCuts::rNumericArgs(2);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedSourceVolFlowCool == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedSourceVolFlowCoolWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedCapCool = DataIPShortCuts::rNumericArgs(3);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedCapCool == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedCapCoolWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerCool = DataIPShortCuts::rNumericArgs(4);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerCool == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedPowerCoolWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).CoolCapCurveIndex = GetCurveIndex(state, DataIPShortCuts::cAlphaArgs(6));
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).CoolPowCurveIndex = GetCurveIndex(state, DataIPShortCuts::cAlphaArgs(7));
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).CoolCapCurveIndex > 0) {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).CoolCapCurveIndex,
                                                            {4},
                                                            "GetWatertoWaterHPInput",
                                                            HPEqFitCoolingUC,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name,
                                                            "Cooling Capacity Curve Name");                                                       
            }
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).CoolPowCurveIndex > 0) {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).CoolPowCurveIndex,
                                                            {4},
                                                            "GetWatertoWaterHPInput",
                                                            HPEqFitCoolingUC,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name,
                                                            "Cooling Compressor Power Curve Name");                                                       
            }

            if (NumNums > 4) {
                if (!DataIPShortCuts::lNumericFieldBlanks(5)) {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = DataIPShortCuts::rNumericArgs(5);
                } else {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = 8.0;
                }

            } else {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = 8.0;
            }

            // calculate reference COP if hard sized
            if (!state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedPowerCoolWasAutoSized && !state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedCapCoolWasAutoSized && state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerCool > 0.0) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedCapCool / state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerCool;
            }

            if (NumNums > 5) {
                if (!DataIPShortCuts::lNumericFieldBlanks(6)) {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).sizFac = DataIPShortCuts::rNumericArgs(6);
                } else {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).sizFac = 1.0;
                }
            } else {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).sizFac = 1.0;
            }

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).SourceSideInletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(2),
                                                                     ErrorsFound,
                                                                     HPEqFitCoolingUC,
                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                     NodeType_Water,
                                                                     NodeConnectionType_Inlet,
                                                                     1,
                                                                     ObjectIsNotParent);

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).SourceSideOutletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(3),
                                                                      ErrorsFound,
                                                                      HPEqFitCoolingUC,
                                                                      DataIPShortCuts::cAlphaArgs(1),
                                                                      NodeType_Water,
                                                                      NodeConnectionType_Outlet,
                                                                      1,
                                                                      ObjectIsNotParent);

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).LoadSideInletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(4),
                                                                   ErrorsFound,
                                                                   HPEqFitCoolingUC,
                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                   NodeType_Water,
                                                                   NodeConnectionType_Inlet,
                                                                   2,
                                                                   ObjectIsNotParent);

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).LoadSideOutletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(5),
                                                                    ErrorsFound,
                                                                    HPEqFitCoolingUC,
                                                                    DataIPShortCuts::cAlphaArgs(1),
                                                                    NodeType_Water,
                                                                    NodeConnectionType_Outlet,
                                                                    2,
                                                                    ObjectIsNotParent);

            // Test node sets
            TestCompSet(state, HPEqFitCoolingUC,
                        DataIPShortCuts::cAlphaArgs(1),
                        DataIPShortCuts::cAlphaArgs(2),
                        DataIPShortCuts::cAlphaArgs(3),
                        "Condenser Water Nodes");
            TestCompSet(state, HPEqFitCoolingUC,
                        DataIPShortCuts::cAlphaArgs(1),
                        DataIPShortCuts::cAlphaArgs(4),
                        DataIPShortCuts::cAlphaArgs(5),
                        "Chilled Water Nodes");

            if (NumAlphas > 7 && !DataIPShortCuts::lAlphaFieldBlanks(8)) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionName = DataIPShortCuts::cAlphaArgs(8);
            }

            // CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Cooling'
            SetupOutputVariable(state, "Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportEnergy,
                                "System",
                                "Sum",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "Plant");
            SetupOutputVariable(state, "Heat Pump Load Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportQLoadEnergy,
                                "System",
                                "Sum",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportQSourceEnergy,
                                "System",
                                "Sum",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
        }

        // Load data structure for heating coil
        for (HPNum = 1; HPNum <= NumHeatCoil; ++HPNum) {

            GSHPNum = NumCoolCoil + HPNum;

            inputProcessor->getObjectItem(state,
                                          HPEqFitHeatingUC,
                                          HPNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks);
            GlobalNames::VerifyUniqueInterObjectName(state, state.dataHPWaterToWaterSimple->HeatPumpWaterUniqueNames, DataIPShortCuts::cAlphaArgs(1), HPEqFitHeatingUC, ErrorsFound);
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).WWHPPlantTypeOfNum = TypeOf_HPWaterEFHeating;
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name = DataIPShortCuts::cAlphaArgs(1);
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedLoadVolFlowHeat = DataIPShortCuts::rNumericArgs(1);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedLoadVolFlowHeat == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedLoadVolFlowHeatWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedSourceVolFlowHeat = DataIPShortCuts::rNumericArgs(2);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedSourceVolFlowHeat == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedSourceVolFlowHeatWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedCapHeat = DataIPShortCuts::rNumericArgs(3);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedCapHeat == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedCapHeatWasAutoSized = true;
            }
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerHeat = DataIPShortCuts::rNumericArgs(4);
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerHeat == DataSizing::AutoSize) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedPowerHeatWasAutoSized = true;
            }

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).HeatCapCurveIndex = GetCurveIndex(state, DataIPShortCuts::cAlphaArgs(6));
            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).HeatPowCurveIndex = GetCurveIndex(state, DataIPShortCuts::cAlphaArgs(7));
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).HeatCapCurveIndex > 0) {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).HeatCapCurveIndex,
                                                            {4},
                                                            "GetWatertoWaterHPInput",
                                                            HPEqFitHeatingUC,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name,
                                                            "Heating Capacity Curve Name");                                                       
            }
            if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).HeatPowCurveIndex > 0) {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).HeatPowCurveIndex,
                                                            {4},
                                                            "GetWatertoWaterHPInput",
                                                            HPEqFitHeatingUC,
                                                            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name,
                                                            "Heating Compressor Power Curve Name");                                                       
            }
            if (NumNums > 4) {
                if (!DataIPShortCuts::lNumericFieldBlanks(5)) {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = DataIPShortCuts::rNumericArgs(5);
                } else {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = 7.5;
                }

            } else {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = 7.5;
            }

            // calculate reference COP if hard sized
            if (!state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedPowerHeatWasAutoSized && !state.dataHPWaterToWaterSimple->GSHP(GSHPNum).ratedCapHeatWasAutoSized && state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerHeat > 0.0) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).refCOP = state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedCapHeat / state.dataHPWaterToWaterSimple->GSHP(GSHPNum).RatedPowerHeat;
            }

            if (NumNums > 5) {
                if (!DataIPShortCuts::lNumericFieldBlanks(6)) {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).sizFac = DataIPShortCuts::rNumericArgs(6);
                } else {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).sizFac = 1.0;
                }
            } else {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).sizFac = 1.0;
            }

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).SourceSideInletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(2),
                                                                     ErrorsFound,
                                                                     HPEqFitHeatingUC,
                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                     NodeType_Water,
                                                                     NodeConnectionType_Inlet,
                                                                     1,
                                                                     ObjectIsNotParent);

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).SourceSideOutletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(3),
                                                                      ErrorsFound,
                                                                      HPEqFitHeatingUC,
                                                                      DataIPShortCuts::cAlphaArgs(1),
                                                                      NodeType_Water,
                                                                      NodeConnectionType_Outlet,
                                                                      1,
                                                                      ObjectIsNotParent);

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).LoadSideInletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(4),
                                                                   ErrorsFound,
                                                                   HPEqFitHeatingUC,
                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                   NodeType_Water,
                                                                   NodeConnectionType_Inlet,
                                                                   2,
                                                                   ObjectIsNotParent);

            state.dataHPWaterToWaterSimple->GSHP(GSHPNum).LoadSideOutletNodeNum = GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(5),
                                                                    ErrorsFound,
                                                                    HPEqFitHeatingUC,
                                                                    DataIPShortCuts::cAlphaArgs(1),
                                                                    NodeType_Water,
                                                                    NodeConnectionType_Outlet,
                                                                    2,
                                                                    ObjectIsNotParent);

            if (NumAlphas > 7 && !DataIPShortCuts::lAlphaFieldBlanks(8)) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionName = DataIPShortCuts::cAlphaArgs(8);
            }

            // Test node sets
            TestCompSet(state, HPEqFitHeatingUC,
                        DataIPShortCuts::cAlphaArgs(1),
                        DataIPShortCuts::cAlphaArgs(2),
                        DataIPShortCuts::cAlphaArgs(3),
                        "Condenser Water Nodes");
            TestCompSet(state,
                HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(4), DataIPShortCuts::cAlphaArgs(5), "Hot Water Nodes");

            // CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Heating'
            SetupOutputVariable(state, "Heat Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportEnergy,
                                "System",
                                "Sum",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "Plant");
            SetupOutputVariable(state, "Heat Pump Load Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportQLoadEnergy,
                                "System",
                                "Sum",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportQSourceEnergy,
                                "System",
                                "Sum",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
        }

        // now process companion coils, if any
        for (GSHPNum = 1; GSHPNum <= state.dataHPWaterToWaterSimple->NumGSHPs; ++GSHPNum) {
            if (!state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionName.empty()) {
                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionIndex = UtilityRoutines::FindItemInList(state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionName, state.dataHPWaterToWaterSimple->GSHP);
                if (state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionIndex == 0) {
                    ShowSevereError(state, "GetEquationFitWaterToWater Input: did not find companion heat pump named '" + state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionName +
                                    "' in heat pump called " + state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
                    ErrorsFound = true;
                } else {
                    state.dataHPWaterToWaterSimple->GSHP(GSHPNum).companionIdentified = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for Water to Water Heat Pumps");
        }

        for (GSHPNum = 1; GSHPNum <= state.dataHPWaterToWaterSimple->NumGSHPs; ++GSHPNum) {
            // setup output variables
            SetupOutputVariable(state,
                "Heat Pump Electricity Rate", OutputProcessor::Unit::W, state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportPower, "System", "Average", state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Load Side Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportQLoad,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Source Side Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportQSource,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Load Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportLoadSideOutletTemp,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Load Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportLoadSideInletTemp,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Source Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportSourceSideOutletTemp,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Source Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportSourceSideInletTemp,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Load Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportLoadSideMassFlowRate,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
            SetupOutputVariable(state, "Heat Pump Source Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).reportSourceSideMassFlowRate,
                                "System",
                                "Average",
                                state.dataHPWaterToWaterSimple->GSHP(GSHPNum).Name);
        }
    }

    void GshpSpecs::InitWatertoWaterHP(EnergyPlusData &state,
                                       int const GSHPTypeNum,                        // Type of GSHP
                                       [[maybe_unused]] std::string const &GSHPName, // User Specified Name of GSHP
                                       [[maybe_unused]] bool const FirstHVACIteration,
                                       Real64 const MyLoad // Demand Load
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kenneth Tang
        //       DATE WRITTEN   March 2005
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Water-to-Water HP Simple

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
        // (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
        // Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
        // M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

        // Using/Aliasing
        using DataHVACGlobals::SysTimeElapsed;
        using DataPlant::TypeOf_HPWaterEFCooling;
        using DataPlant::TypeOf_HPWaterEFHeating;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitGshp");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int LoadSideInletNode;             // Load Side Inlet Node
        int LoadSideOutletNode;            // Load Side Outlet Node
        int SourceSideInletNode;           // Source Side Inlet Node
        int SourceSideOutletNode;          // Source Side Outlet Node
        static Real64 CurrentSimTime(0.0); // Current Simulation Time
        static Real64 PrevSimTime(0.0);    // Previous Simulation Time

        int LoopNum;
        int LoopSideNum;
        Real64 rho; // local fluid density

        this->MustRun = true; // Reset MustRun flag to TRUE
        LoadSideInletNode = this->LoadSideInletNodeNum;
        LoadSideOutletNode = this->LoadSideOutletNodeNum;
        SourceSideInletNode = this->SourceSideInletNodeNum;
        SourceSideOutletNode = this->SourceSideOutletNodeNum;

        if (this->MyPlantScanFlag) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->WWHPPlantTypeOfNum,
                                                    this->SourceLoopNum,
                                                    this->SourceLoopSideNum,
                                                    this->SourceBranchNum,
                                                    this->SourceCompNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->SourceSideInletNodeNum,
                                                    _);
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->WWHPPlantTypeOfNum,
                                                    this->LoadLoopNum,
                                                    this->LoadLoopSideNum,
                                                    this->LoadBranchNum,
                                                    this->LoadCompNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->LoadSideInletNodeNum,
                                                    _);

            if (!errFlag) {
                PlantUtilities::InterConnectTwoPlantLoopSides(state,
                    this->LoadLoopNum, this->LoadLoopSideNum, this->SourceLoopNum, this->SourceLoopSideNum, this->WWHPPlantTypeOfNum, true);
            }

            if (errFlag) {
                ShowFatalError(state, "GetWatertoWaterHPInput: Program terminated on scan for loop data");
            }
            this->MyPlantScanFlag = false;
        }

        if (this->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
            // Initialize all report variables to a known state at beginning of simulation

            this->reportPower = 0.0;
            this->reportEnergy = 0.0;
            this->reportQLoad = 0.0;
            this->reportQLoadEnergy = 0.0;
            this->reportQSource = 0.0;
            this->reportQSourceEnergy = 0.0;
            this->reportLoadSideMassFlowRate = 0.0;
            this->reportLoadSideInletTemp = 0.0;
            this->reportLoadSideOutletTemp = 0.0;
            this->reportSourceSideMassFlowRate = 0.0;
            this->reportSourceSideInletTemp = 0.0;
            this->reportSourceSideOutletTemp = 0.0;
            this->IsOn = false;
            this->MustRun = true;

            if (this->WWHPPlantTypeOfNum == TypeOf_HPWaterEFHeating) {
                rho = GetDensityGlycol(
                    state, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName, DataGlobalConstants::HWInitConvTemp, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);
                this->LoadSideDesignMassFlow = this->RatedLoadVolFlowHeat * rho;
                rho = GetDensityGlycol(
                    state, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName, DataGlobalConstants::CWInitConvTemp, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);
                this->SourceSideDesignMassFlow = this->RatedSourceVolFlowHeat * rho;
            } else if (this->WWHPPlantTypeOfNum == TypeOf_HPWaterEFCooling) {
                rho = GetDensityGlycol(
                    state, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName, DataGlobalConstants::CWInitConvTemp, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);
                this->LoadSideDesignMassFlow = this->RatedLoadVolFlowCool * rho;
                rho = GetDensityGlycol(
                    state, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName, DataGlobalConstants::HWInitConvTemp, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);
                this->SourceSideDesignMassFlow = this->RatedSourceVolFlowCool * rho;
            }

            InitComponentNodes(0.0,
                               this->LoadSideDesignMassFlow,
                               this->LoadSideInletNodeNum,
                               this->LoadSideOutletNodeNum,
                               this->LoadLoopNum,
                               this->LoadLoopSideNum,
                               this->LoadBranchNum,
                               this->LoadCompNum);

            InitComponentNodes(0.0,
                               this->SourceSideDesignMassFlow,
                               this->SourceSideInletNodeNum,
                               this->SourceSideOutletNodeNum,
                               this->SourceLoopNum,
                               this->SourceLoopSideNum,
                               this->SourceBranchNum,
                               this->SourceCompNum);

            if (Node(this->SourceSideOutletNodeNum).TempSetPoint == SensedNodeFlagValue) Node(this->SourceSideOutletNodeNum).TempSetPoint = 0.0;
            Node(this->SourceSideInletNodeNum).Temp = Node(this->SourceSideOutletNodeNum).TempSetPoint + 30;

            this->MyEnvrnFlag = false;
        }
        // Reset the environment flag
        if (!state.dataGlobal->BeginEnvrnFlag) this->MyEnvrnFlag = true;

        if (PrevSimTime != CurrentSimTime) {
            PrevSimTime = CurrentSimTime;
        }

        // Calculate the simulation time
        CurrentSimTime = (state.dataGlobal->DayOfSim - 1) * 24 + (state.dataGlobal->HourOfDay - 1) + (state.dataGlobal->TimeStep - 1) * state.dataGlobal->TimeStepZone + SysTimeElapsed;

        LoopNum = this->LoadLoopNum;
        LoopSideNum = this->LoadLoopSideNum;

        if (MyLoad > 0.0 && GSHPTypeNum == TypeOf_HPWaterEFHeating) {
            this->MustRun = true;
            this->IsOn = true;
        } else if (MyLoad < 0.0 && GSHPTypeNum == TypeOf_HPWaterEFCooling) {
            this->MustRun = true;
            this->IsOn = true;
        } else {
            this->MustRun = false;
            this->IsOn = false;
        }

        //*******Set flow based on "flowlock" and "run" flags**********
        // Set flows if the heat pump is not running
        if (!this->MustRun) {
            this->reportLoadSideMassFlowRate = 0.0;
            this->reportSourceSideMassFlowRate = 0.0;

            SetComponentFlowRate(state, this->reportLoadSideMassFlowRate,
                                 this->LoadSideInletNodeNum,
                                 this->LoadSideOutletNodeNum,
                                 this->LoadLoopNum,
                                 this->LoadLoopSideNum,
                                 this->LoadBranchNum,
                                 this->LoadCompNum);
            SetComponentFlowRate(state, this->reportSourceSideMassFlowRate,
                                 this->SourceSideInletNodeNum,
                                 this->SourceSideOutletNodeNum,
                                 this->SourceLoopNum,
                                 this->SourceLoopSideNum,
                                 this->SourceBranchNum,
                                 this->SourceCompNum);
            PlantUtilities::PullCompInterconnectTrigger(state, this->LoadLoopNum,
                                                        this->LoadLoopSideNum,
                                                        this->LoadBranchNum,
                                                        this->LoadCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->SourceLoopNum,
                                                        this->LoadLoopSideNum, // IS THIS RIGHT?
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->reportSourceSideMassFlowRate);
            // Set flows if the heat pump is running
        } else { // the heat pump must run

            this->reportLoadSideMassFlowRate = this->LoadSideDesignMassFlow;
            this->reportSourceSideMassFlowRate = this->SourceSideDesignMassFlow;
            // now check against and request in plant
            SetComponentFlowRate(state, this->reportLoadSideMassFlowRate,
                                 this->LoadSideInletNodeNum,
                                 this->LoadSideOutletNodeNum,
                                 this->LoadLoopNum,
                                 this->LoadLoopSideNum,
                                 this->LoadBranchNum,
                                 this->LoadCompNum);
            SetComponentFlowRate(state, this->reportSourceSideMassFlowRate,
                                 this->SourceSideInletNodeNum,
                                 this->SourceSideOutletNodeNum,
                                 this->SourceLoopNum,
                                 this->SourceLoopSideNum,
                                 this->SourceBranchNum,
                                 this->SourceCompNum);
            // if there's no flowin one, turn the entire "heat pump off"
            if (this->reportLoadSideMassFlowRate <= 0.0 || this->reportSourceSideMassFlowRate <= 0.0) {

                this->reportLoadSideMassFlowRate = 0.0;
                this->reportSourceSideMassFlowRate = 0.0;
                this->MustRun = false;

                SetComponentFlowRate(state, this->reportLoadSideMassFlowRate,
                                     this->LoadSideInletNodeNum,
                                     this->LoadSideOutletNodeNum,
                                     this->LoadLoopNum,
                                     this->LoadLoopSideNum,
                                     this->LoadBranchNum,
                                     this->LoadCompNum);
                SetComponentFlowRate(state, this->reportSourceSideMassFlowRate,
                                     this->SourceSideInletNodeNum,
                                     this->SourceSideOutletNodeNum,
                                     this->SourceLoopNum,
                                     this->SourceLoopSideNum,
                                     this->SourceBranchNum,
                                     this->SourceCompNum);
                PlantUtilities::PullCompInterconnectTrigger(state, this->LoadLoopNum,
                                                            this->LoadLoopSideNum,
                                                            this->LoadBranchNum,
                                                            this->LoadCompNum,
                                                            this->CondMassFlowIndex,
                                                            this->SourceLoopNum,
                                                            this->LoadLoopSideNum,
                                                            DataPlant::iCriteriaType::MassFlowRate,
                                                            this->reportSourceSideMassFlowRate);
                return;
            }
            PlantUtilities::PullCompInterconnectTrigger(state, this->LoadLoopNum,
                                                        this->LoadLoopSideNum,
                                                        this->LoadBranchNum,
                                                        this->LoadCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->SourceLoopNum,
                                                        this->LoadLoopSideNum,
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->reportSourceSideMassFlowRate);
        }

        // Get inlet temps
        this->reportLoadSideInletTemp = Node(LoadSideInletNode).Temp;
        this->reportSourceSideInletTemp = Node(SourceSideInletNode).Temp;

        // Outlet variables
        this->reportPower = 0.0;
        this->reportEnergy = 0.0;
        this->reportQLoad = 0.0;
        this->reportQLoadEnergy = 0.0;
        this->reportQSource = 0.0;
        this->reportQSourceEnergy = 0.0;
        this->reportLoadSideOutletTemp = 0.0;
        this->reportSourceSideOutletTemp = 0.0;
    }

    void GshpSpecs::sizeCoolingWaterToWaterHP(EnergyPlusData &state)
    {

        // do sizing related calculations and reporting for cooling heat pumps
        bool errorsFound(false);
        static std::string const RoutineName("sizeCoolingWaterToWaterHP");
        Real64 tmpLoadSideVolFlowRate = this->RatedLoadVolFlowCool;
        Real64 tmpSourceSideVolFlowRate = this->RatedSourceVolFlowCool;
        Real64 tmpCoolingCap = this->RatedCapCool;
        Real64 tmpPowerDraw = this->RatedPowerCool;

        // if companion heating coil known, update info from that
        if (this->companionIdentified) {
            this->RatedLoadVolFlowHeat = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedLoadVolFlowHeat;
            this->ratedLoadVolFlowHeatWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedLoadVolFlowHeatWasAutoSized;
            this->RatedSourceVolFlowHeat = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedSourceVolFlowHeat;
            this->ratedSourceVolFlowHeatWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedSourceVolFlowHeatWasAutoSized;
            this->RatedCapHeat = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedCapHeat;
            this->ratedCapHeatWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedCapHeatWasAutoSized;
            this->RatedPowerHeat = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedPowerHeat;
            this->ratedPowerHeatWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedPowerHeatWasAutoSized;
        }

        int pltLoadSizNum = state.dataPlnt->PlantLoop(this->LoadLoopNum).PlantSizNum;
        if (pltLoadSizNum > 0) {
            if (DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
                tmpLoadSideVolFlowRate = DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate * this->sizFac;
                // now compare to companion coil and take higher
                if (this->companionIdentified) {
                    tmpLoadSideVolFlowRate = max(tmpLoadSideVolFlowRate, this->RatedLoadVolFlowHeat);
                    // store flow rate right away regardless of PlantFirstSizesOkayToFinalize so that data are available
                    this->RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
                }
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCoolingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else if (this->companionIdentified && this->RatedLoadVolFlowHeat > 0.0) {
                tmpLoadSideVolFlowRate = this->RatedLoadVolFlowHeat;
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCoolingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else {
                if (this->ratedCapCoolWasAutoSized) tmpCoolingCap = 0.0;
                if (this->ratedLoadVolFlowCoolWasAutoSized) tmpLoadSideVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->ratedCapCoolWasAutoSized) {
                    this->RatedCapCool = tmpCoolingCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                        BaseSizer::reportSizerOutput(state,
                            "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Design Size Nominal Capacity [W]", tmpCoolingCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                            "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Initial Design Size Nominal Capacity [W]", tmpCoolingCap);
                    }
                } else {
                    if (this->RatedCapCool > 0.0 && tmpCoolingCap > 0.0) {
                        Real64 nomCoolingCapUser = this->RatedCapCool;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                            if (state.dataGlobal->DoPlantSizing) {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                             this->Name,
                                                             "Design Size Nominal Capacity [W]",
                                                             tmpCoolingCap,
                                                             "User-Specified Nominal Capacity [W]",
                                                             nomCoolingCapUser);
                            } else {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                             this->Name,
                                                             "User-Specified Nominal Capacity [W]",
                                                             nomCoolingCapUser);
                            }

                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpCoolingCap - nomCoolingCapUser) / nomCoolingCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", nomCoolingCapUser));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpCoolingCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCoolingCap = nomCoolingCapUser;
                    }
                }
                if (this->ratedLoadVolFlowCoolWasAutoSized) {
                    this->RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                     this->Name,
                                                     "Design Size Load Side Volume Flow Rate [m3/s]",
                                                     tmpLoadSideVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                     this->Name,
                                                     "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                     tmpLoadSideVolFlowRate);
                    }
                } else {
                    if (this->RatedLoadVolFlowCool > 0.0 && tmpLoadSideVolFlowRate > 0.0) {
                        Real64 nomLoadSideVolFlowUser = this->RatedLoadVolFlowCool;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                            if (state.dataGlobal->DoPlantSizing) {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                             this->Name,
                                                             "Design Size Load Side Volume Flow Rate [m3/s]",
                                                             tmpLoadSideVolFlowRate,
                                                             "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                             nomLoadSideVolFlowUser);
                            } else {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                             this->Name,
                                                             "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                             nomLoadSideVolFlowUser);
                            }
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpLoadSideVolFlowRate - nomLoadSideVolFlowUser) / nomLoadSideVolFlowUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state,
                                                      format("User-Specified Load Side Volume Flow Rate of {:.2R} [m3/s]", nomLoadSideVolFlowUser));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Load Side Volume Flow Rate of {:.2R} [m3/s]", tmpLoadSideVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpLoadSideVolFlowRate = nomLoadSideVolFlowUser;
                    }
                }
            }

        } else { // did not find load side loop plant sizing to go with this.
            if (this->companionIdentified) {
                if (this->ratedLoadVolFlowHeatWasAutoSized && this->RatedLoadVolFlowHeat > 0.0) {
                    // fill load side flow rate size from companion coil
                    tmpLoadSideVolFlowRate = this->RatedLoadVolFlowHeat;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                            BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                         this->Name,
                                                         "Design Size Load Side Volume Flow Rate [m3/s]",
                                                         tmpLoadSideVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                         this->Name,
                                                         "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                         tmpLoadSideVolFlowRate);
                        }
                    }
                }
                if (this->ratedCapHeatWasAutoSized && this->RatedCapHeat > 0.0) {
                    tmpCoolingCap = this->RatedCapHeat;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->RatedCapCool = tmpCoolingCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                            BaseSizer::reportSizerOutput(state,
                                "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Design Size Nominal Capacity [W]", tmpCoolingCap);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Initial Design Size Nominal Capacity [W]", tmpCoolingCap);
                        }
                    }
                }
            } else { // no companion heatpump, no plant sizing object
                if ((this->ratedLoadVolFlowCoolWasAutoSized || this->ratedCapCoolWasAutoSized) && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Autosizing of Water to Water Heat Pump requires a loop Sizing:Plant object.");
                    ShowContinueError(state, "Occurs in HeatPump:WaterToWater:EquationFit:Cooling object = " + this->Name);
                    errorsFound = true;
                }
            }

            if (!this->ratedLoadVolFlowCoolWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "User-Specified Load Side Flow Rate [m3/s]", this->RatedLoadVolFlowCool);
            }
            if (!this->ratedCapCoolWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "User-Specified Nominal Capacity [W]", this->RatedCapCool);
            }
        }
        if (!this->ratedLoadVolFlowCoolWasAutoSized) tmpLoadSideVolFlowRate = this->RatedLoadVolFlowCool;
        int pltSourceSizNum = state.dataPlnt->PlantLoop(this->SourceLoopNum).PlantSizNum;
        if (pltSourceSizNum > 0) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex,
                                                           RoutineName);
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex,
                                                               RoutineName);
            tmpSourceSideVolFlowRate = tmpCoolingCap * (1.0 + (1.0 / this->refCOP)) / (DataSizing::PlantSizData(pltSourceSizNum).DeltaT * Cp * rho);
        } else {
            tmpSourceSideVolFlowRate = tmpLoadSideVolFlowRate; // set source side flow equal to load side flow, assumption
        }

        if (this->ratedSourceVolFlowCoolWasAutoSized) {
            this->RatedSourceVolFlowCool = tmpSourceSideVolFlowRate;
            if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                             this->Name,
                                             "Design Size Source Side Volume Flow Rate [m3/s]",
                                             tmpSourceSideVolFlowRate);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                             this->Name,
                                             "Initial Design Size Source Side Volume Flow Rate [m3/s]",
                                             tmpSourceSideVolFlowRate);
            }
        } else {
            if (this->RatedSourceVolFlowCool > 0.0 && tmpSourceSideVolFlowRate > 0.0) {
                Real64 nomSourceSideVolFlowUser = this->RatedSourceVolFlowCool;
                if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                    if (state.dataGlobal->DoPlantSizing) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                     this->Name,
                                                     "Design Size Source Side Volume Flow Rate [m3/s]",
                                                     tmpSourceSideVolFlowRate,
                                                     "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                     nomSourceSideVolFlowUser);
                    } else {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                     this->Name,
                                                     "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                     nomSourceSideVolFlowUser);
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(tmpSourceSideVolFlowRate - nomSourceSideVolFlowUser) / nomSourceSideVolFlowUser) >
                            DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage(state, "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state,
                                              format("User-Specified Source Side Volume Flow Rate of {:.2R} [m3/s]", nomSourceSideVolFlowUser));
                            ShowContinueError(
                                state, format("differs from Design Size Source Side Volume Flow Rate of {:.2R} [m3/s]", tmpSourceSideVolFlowRate));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
                tmpSourceSideVolFlowRate = nomSourceSideVolFlowUser;
            }
        }
        if (!this->ratedSourceVolFlowCoolWasAutoSized) tmpSourceSideVolFlowRate = this->RatedSourceVolFlowCool;
        if (!this->ratedCapCoolWasAutoSized) tmpCoolingCap = this->RatedCapCool;
        if (this->ratedPowerCoolWasAutoSized) {
            tmpPowerDraw = tmpCoolingCap / this->refCOP;
            this->RatedPowerCool = tmpPowerDraw;
            if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Design Size Cooling Power Consumption [W]", tmpPowerDraw);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Initial Design Size Cooling Power Consumption [W]", tmpPowerDraw);
            }
        } else {
            if (this->RatedPowerCool > 0.0 && tmpPowerDraw > 0.0) {
                Real64 nomPowerDrawUser = this->RatedPowerCool;
                if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
                    if (state.dataGlobal->DoPlantSizing) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                     this->Name,
                                                     "Design Size Cooling Power Consumption [W]",
                                                     tmpPowerDraw,
                                                     "User-Specified Cooling Power Consumption [W]",
                                                     nomPowerDrawUser);
                    } else {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Cooling",
                                                     this->Name,
                                                     "User-Specified Cooling Power Consumption [W]",
                                                     nomPowerDrawUser);
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(tmpPowerDraw - nomPowerDrawUser) / nomPowerDrawUser) > DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage(state, "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Cooling Power Consumption of {:.2R} [W]", nomPowerDrawUser));
                            ShowContinueError(state, format("differs from Design Size Cooling Power Consumption of {:.2R} [W]", tmpPowerDraw));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
                tmpPowerDraw = nomPowerDrawUser;
                this->refCOP = tmpCoolingCap / tmpPowerDraw;
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->LoadSideInletNodeNum, tmpLoadSideVolFlowRate);
        // only register half of the source side flow because we expect a companion heat pump to also register a flow and we don't want to double
        // count
        PlantUtilities::RegisterPlantCompDesignFlow(this->SourceSideInletNodeNum, tmpSourceSideVolFlowRate * 0.5);

        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myCoolingSizesReported) {
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "HeatPump:WaterToWater:EquationFit:Cooling");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->refCOP);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->RatedCapCool);
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            this->myCoolingSizesReported = true;
        }

        if (errorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void GshpSpecs::sizeHeatingWaterToWaterHP(EnergyPlusData &state)
    {

        // do sizing related calculations and reporting for heating heat pumps
        bool errorsFound(false);
        static std::string const RoutineName("sizeHeatingWaterToWaterHP");
        Real64 tmpLoadSideVolFlowRate = this->RatedLoadVolFlowHeat;
        Real64 tmpSourceSideVolFlowRate = this->RatedSourceVolFlowHeat;
        Real64 tmpHeatingCap = this->RatedCapHeat;
        Real64 tmpPowerDraw = this->RatedPowerHeat;

        // if companion cooling coil known, update info from that
        if (this->companionIdentified) {
            this->RatedLoadVolFlowCool = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedLoadVolFlowCool;
            this->ratedLoadVolFlowCoolWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedLoadVolFlowCoolWasAutoSized;
            this->RatedSourceVolFlowCool = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedSourceVolFlowCool;
            this->ratedSourceVolFlowCoolWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedSourceVolFlowCoolWasAutoSized;
            this->RatedCapCool = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedCapCool;
            this->ratedCapCoolWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedCapCoolWasAutoSized;
            this->RatedPowerCool = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).RatedPowerCool;
            this->ratedPowerCoolWasAutoSized = state.dataHPWaterToWaterSimple->GSHP(this->companionIndex).ratedPowerCoolWasAutoSized;
        }

        int pltLoadSizNum = state.dataPlnt->PlantLoop(this->LoadLoopNum).PlantSizNum;
        if (pltLoadSizNum > 0) {
            if (DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
                tmpLoadSideVolFlowRate = DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate * this->sizFac;
                // now compare to companion coil and take higher
                if (this->companionIdentified) {
                    tmpLoadSideVolFlowRate = max(tmpLoadSideVolFlowRate, this->RatedLoadVolFlowCool);
                    // store flow rate right away regardless of PlantFirstSizesOkayToFinalize so that data are available for companion when
                    // PlantFirstSizesOkayToFinalize is true
                    this->RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
                }
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobalConstants::HWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobalConstants::HWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpHeatingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else if (this->companionIdentified && this->RatedLoadVolFlowCool > 0.0) {
                tmpLoadSideVolFlowRate = this->RatedLoadVolFlowCool;
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobalConstants::HWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobalConstants::HWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpHeatingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else {
                if (this->ratedCapHeatWasAutoSized) tmpHeatingCap = 0.0;
                if (this->ratedLoadVolFlowHeatWasAutoSized) tmpLoadSideVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->ratedCapHeatWasAutoSized) {
                    this->RatedCapHeat = tmpHeatingCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                        BaseSizer::reportSizerOutput(state,
                            "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Design Size Nominal Capacity [W]", tmpHeatingCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                            "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Initial Design Size Nominal Capacity [W]", tmpHeatingCap);
                    }
                } else {
                    if (this->RatedCapHeat > 0.0 && tmpHeatingCap > 0.0) {
                        Real64 nomHeatingCapUser = this->RatedCapHeat;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                            if (state.dataGlobal->DoPlantSizing) {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                             this->Name,
                                                             "Design Size Nominal Capacity [W]",
                                                             tmpHeatingCap,
                                                             "User-Specified Nominal Capacity [W]",
                                                             nomHeatingCapUser);
                            } else {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                             this->Name,
                                                             "User-Specified Nominal Capacity [W]",
                                                             nomHeatingCapUser);
                            }
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatingCap - nomHeatingCapUser) / nomHeatingCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", nomHeatingCapUser));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpHeatingCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatingCap = nomHeatingCapUser;
                    }
                }
                if (this->ratedLoadVolFlowHeatWasAutoSized) {
                    this->RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                     this->Name,
                                                     "Design Size Load Side Volume Flow Rate [m3/s]",
                                                     tmpLoadSideVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                     this->Name,
                                                     "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                     tmpLoadSideVolFlowRate);
                    }
                } else {
                    if (this->RatedLoadVolFlowHeat > 0.0 && tmpLoadSideVolFlowRate > 0.0) {
                        Real64 nomLoadSideVolFlowUser = this->RatedLoadVolFlowHeat;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                            if (state.dataGlobal->DoPlantSizing) {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                             this->Name,
                                                             "Design Size Load Side Volume Flow Rate [m3/s]",
                                                             tmpLoadSideVolFlowRate,
                                                             "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                             nomLoadSideVolFlowUser);
                            } else {
                                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                             this->Name,
                                                             "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                             nomLoadSideVolFlowUser);
                            }
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpLoadSideVolFlowRate - nomLoadSideVolFlowUser) / nomLoadSideVolFlowUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state,
                                                      format("User-Specified Load Side Volume Flow Rate of {:.2R} [m3/s]", nomLoadSideVolFlowUser));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Load Side Volume Flow Rate of {:.2R} [m3/s]", tmpLoadSideVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpLoadSideVolFlowRate = nomLoadSideVolFlowUser;
                    }
                }
            }
        } else { // did not find plant sizing to go with this.
            if (this->companionIdentified) {
                if (this->ratedLoadVolFlowHeatWasAutoSized && this->RatedLoadVolFlowCool > 0.0) {
                    // fill load side flow rate size from companion coil
                    tmpLoadSideVolFlowRate = this->RatedLoadVolFlowCool;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                            BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                         this->Name,
                                                         "Design Size Load Side Volume Flow Rate [m3/s]",
                                                         tmpLoadSideVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                         this->Name,
                                                         "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                         tmpLoadSideVolFlowRate);
                        }
                    }
                }
                if (this->ratedCapHeatWasAutoSized && this->RatedCapCool > 0.0) {
                    tmpHeatingCap = this->RatedCapCool;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->RatedCapHeat = tmpHeatingCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                            BaseSizer::reportSizerOutput(state,
                                "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Design Size Nominal Capacity [W]", tmpHeatingCap);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Initial Design Size Nominal Capacity [W]", tmpHeatingCap);
                        }
                    }
                }

            } else { // no companion heatpump, no plant sizing object
                if ((this->ratedLoadVolFlowHeatWasAutoSized || this->ratedCapHeatWasAutoSized) && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Autosizing of Water to Water Heat Pump requires a loop Sizing:Plant object.");
                    ShowContinueError(state, "Occurs in HeatPump:WaterToWater:EquationFit:Heating object = " + this->Name);
                    errorsFound = true;
                }
            }

            if (!this->ratedLoadVolFlowHeatWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "User-Specified Load Side Flow Rate [m3/s]", this->RatedLoadVolFlowHeat);
            }
            if (!this->ratedCapHeatWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "User-Specified Nominal Capacity [W]", this->RatedCapHeat);
            }
        }
        if (!this->ratedLoadVolFlowHeatWasAutoSized) tmpLoadSideVolFlowRate = this->RatedLoadVolFlowHeat;
        int pltSourceSizNum = state.dataPlnt->PlantLoop(this->SourceLoopNum).PlantSizNum;
        if (pltSourceSizNum > 0) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName,
                                                           DataGlobalConstants::HWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex,
                                                           RoutineName);
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName,
                                                               DataGlobalConstants::HWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex,
                                                               RoutineName);
            tmpSourceSideVolFlowRate = tmpHeatingCap * (1.0 - (1.0 / this->refCOP)) / (DataSizing::PlantSizData(pltSourceSizNum).DeltaT * Cp * rho);
        } else {
            tmpSourceSideVolFlowRate = tmpLoadSideVolFlowRate; // set source side flow equal to load side flow, assumption
        }
        if (this->ratedSourceVolFlowHeatWasAutoSized) {
            this->RatedSourceVolFlowHeat = tmpSourceSideVolFlowRate;
            if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                             this->Name,
                                             "Design Size Source Side Volume Flow Rate [m3/s]",
                                             tmpSourceSideVolFlowRate);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                             this->Name,
                                             "Initial Design Size Source Side Volume Flow Rate [m3/s]",
                                             tmpSourceSideVolFlowRate);
            }
        } else {
            if (this->RatedSourceVolFlowHeat > 0.0 && tmpSourceSideVolFlowRate > 0.0) {
                Real64 nomSourceSideVolFlowUser = this->RatedSourceVolFlowHeat;
                if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                    if (state.dataGlobal->DoPlantSizing) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                     this->Name,
                                                     "Design Size Source Side Volume Flow Rate [m3/s]",
                                                     tmpSourceSideVolFlowRate,
                                                     "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                     nomSourceSideVolFlowUser);
                    } else {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                     this->Name,
                                                     "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                     nomSourceSideVolFlowUser);
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(tmpSourceSideVolFlowRate - nomSourceSideVolFlowUser) / nomSourceSideVolFlowUser) >
                            DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage(state, "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state,
                                              format("User-Specified Source Side Volume Flow Rate of {:.2R} [m3/s]", nomSourceSideVolFlowUser));
                            ShowContinueError(
                                state, format("differs from Design Size Source Side Volume Flow Rate of {:.2R} [m3/s]", tmpSourceSideVolFlowRate));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
                tmpSourceSideVolFlowRate = nomSourceSideVolFlowUser;
            }
        }
        if (!this->ratedSourceVolFlowHeatWasAutoSized) tmpSourceSideVolFlowRate = this->RatedSourceVolFlowHeat;
        if (!this->ratedCapHeatWasAutoSized) tmpHeatingCap = this->RatedCapHeat;
        if (this->ratedPowerHeatWasAutoSized) {
            tmpPowerDraw = tmpHeatingCap / this->refCOP;
            this->RatedPowerHeat = tmpPowerDraw;
            if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Design Size Heating Power Consumption [W]", tmpPowerDraw);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Initial Design Size Heating Power Consumption [W]", tmpPowerDraw);
            }
        } else {
            if (this->RatedPowerHeat > 0.0 && tmpPowerDraw > 0.0) {
                Real64 nomPowerDrawUser = this->RatedPowerHeat;
                if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
                    if (state.dataGlobal->DoPlantSizing) {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                     this->Name,
                                                     "Design Size Heating Power Consumption [W]",
                                                     tmpPowerDraw,
                                                     "User-Specified Heating Power Consumption [W]",
                                                     nomPowerDrawUser);
                    } else {
                        BaseSizer::reportSizerOutput(state, "HeatPump:WaterToWater:EquationFit:Heating",
                                                     this->Name,
                                                     "User-Specified Heating Power Consumption [W]",
                                                     nomPowerDrawUser);
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(tmpPowerDraw - nomPowerDrawUser) / nomPowerDrawUser) > DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage(state, "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Heating Power Consumption of {:.2R} [W]", nomPowerDrawUser));
                            ShowContinueError(state, format("differs from Design Size Heating Power Consumption of {:.2R} [W]", tmpPowerDraw));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
                tmpPowerDraw = nomPowerDrawUser;
                this->refCOP = tmpHeatingCap / tmpPowerDraw;
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->LoadSideInletNodeNum, tmpLoadSideVolFlowRate);
        // register half of source side flow to avoid double counting
        PlantUtilities::RegisterPlantCompDesignFlow(this->SourceSideInletNodeNum, tmpSourceSideVolFlowRate * 0.5);

        if (state.dataPlnt->PlantFinalSizesOkayToReport && !this->myHeatingSizesReported) {
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "HeatPump:WaterToWater:EquationFit:Heating");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->refCOP);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->RatedCapHeat);
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            this->myHeatingSizesReported = true;
        }

        if (errorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void GshpSpecs::CalcWatertoWaterHPCooling(EnergyPlusData &state, Real64 const MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kenneth Tang
        //       DATE WRITTEN   March 2005
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // This routine simulate the heat pump peformance in cooling mode

        // REFERENCES:
        // (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

        // Using/Aliasing
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using CurveManager::CurveValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CelsiustoKelvin(DataGlobalConstants::KelvinConv); // Conversion from Celsius to Kelvin
        Real64 const Tref(283.15);                // Reference Temperature for performance curves,10C [K]
        static std::string const RoutineName("CalcWatertoWaterHPCooling");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CoolCapRated;               // Rated Cooling Capacity [W]
        Real64 CoolPowerRated;             // Rated Cooling Power Consumption[W]
        Real64 LoadSideVolFlowRateRated;   // Rated Load Side Volumetric Flow Rate [m3/s]
        Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]

        Real64 LoadSideMassFlowRate;   // Load Side Mass Flow Rate [kg/s]
        Real64 LoadSideInletTemp;      // Load Side Inlet Temperature [C]
        Real64 LoadSideOutletTemp;     // Load side Outlet Temperature [C]
        Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
        Real64 SourceSideInletTemp;    // Source Side Inlet Temperature [C]
        Real64 SourceSideOutletTemp;   // Source Side Outlet Temperature [C]

        Real64 func1;         // Portion of the heat transfer and power equation
        Real64 func2;         // Portion of the heat transfer and power equation
        Real64 func3;         // Portion of the heat transfer and power equation
        Real64 func4;         // Portion of the heat transfer and power equation
        Real64 Power;         // Power Consumption [W]
        Real64 QLoad;         // Cooling Capacity [W]
        Real64 QSource;       // Source Side Heat Transfer Rate [W]
        Real64 PartLoadRatio; // Part-Load Ratio
        Real64 ReportingConstant;
        Real64 rhoLoadSide;
        Real64 rhoSourceSide;
        Real64 CpLoadSide;
        Real64 CpSourceSide;

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
        LoadSideVolFlowRateRated = this->RatedLoadVolFlowCool;
        SourceSideVolFlowRateRated = this->RatedSourceVolFlowCool;
        CoolCapRated = this->RatedCapCool;
        CoolPowerRated = this->RatedPowerCool;

        LoadSideMassFlowRate = this->reportLoadSideMassFlowRate;
        LoadSideInletTemp = this->reportLoadSideInletTemp;
        SourceSideMassFlowRate = this->reportSourceSideMassFlowRate;
        SourceSideInletTemp = this->reportSourceSideInletTemp;

        // If heat pump is not operating, THEN return
        if (!this->MustRun) {
            return;
        }

        rhoLoadSide =
            GetDensityGlycol(state, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        rhoSourceSide =
            GetDensityGlycol(state, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        func1 = ((LoadSideInletTemp + CelsiustoKelvin) / Tref);
        func2 = ((SourceSideInletTemp + CelsiustoKelvin) / Tref);
        func3 = (LoadSideMassFlowRate / (LoadSideVolFlowRateRated * rhoLoadSide));
        func4 = (SourceSideMassFlowRate / (SourceSideVolFlowRateRated * rhoSourceSide));

        QLoad = CoolCapRated * CurveValue(state, this->CoolCapCurveIndex, func1, func2, func3, func4);

        Power = CoolPowerRated * CurveValue(state, this->CoolPowCurveIndex, func1, func2, func3, func4);

        if ((QLoad <= 0.0 || Power <= 0.0) && !state.dataGlobal->WarmupFlag) {
            if (QLoad <= 0.0) {
                if (this->CoolCapNegativeCounter < 1) {
                    ++this->CoolCapNegativeCounter;
                    ShowWarningError(state, HPEqFitCooling + " \"" + this->Name + "\":");
                    ShowContinueError(state, format(" Cooling capacity curve output is <= 0.0 ({:.4T}).", QLoad));
                    ShowContinueError(state,
                                      format(" Zero or negative value occurs with a load-side inlet temperature of {:.2T} C,", LoadSideInletTemp));
                    ShowContinueError(state, format(" a source-side inlet temperature of {:.2T} C,", SourceSideInletTemp));
                    ShowContinueError(state, format(" a load-side mass flow rate of {:.3T} kg/s,", LoadSideMassFlowRate));
                    ShowContinueError(state, format(" and a source-side mass flow rate of {:.3T} kg/s.", SourceSideMassFlowRate));
                    ShowContinueErrorTimeStamp(state, " The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state, HPEqFitCooling + " \"" + this->Name +
                                                       "\": Cooling capacity curve output is <= 0.0 warning continues...",
                                                   this->CoolCapNegativeIndex,
                                                   QLoad,
                                                   QLoad);
                }
            }
            if (Power <= 0.0) {
                if (this->CoolPowerNegativeCounter < 1) {
                    ++this->CoolPowerNegativeCounter;
                    ShowWarningError(state, HPEqFitCooling + " \"" + this->Name + "\":");
                    ShowContinueError(state, format(" Cooling compressor power curve output is <= 0.0 ({:.4T}).", Power));
                    ShowContinueError(state,
                                      format(" Zero or negative value occurs with a load-side inlet temperature of {:.2T} C,", LoadSideInletTemp));
                    ShowContinueError(state, format(" a source-side inlet temperature of {:.2T} C,", SourceSideInletTemp));
                    ShowContinueError(state, format(" a load-side mass flow rate of {:.3T} kg/s,", LoadSideMassFlowRate));
                    ShowContinueError(state, format(" and a source-side mass flow rate of {:.3T} kg/s.", SourceSideMassFlowRate));
                    ShowContinueErrorTimeStamp(state, " The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state, HPEqFitCooling + " \"" + this->Name +
                                                       "\": Cooling compressor power curve output is <= 0.0 warning continues...",
                                                   this->CoolPowerNegativeIndex,
                                                   Power,
                                                   Power);
                }
            }

            QLoad = 0.0;
            Power = 0.0;
        }

        QSource = QLoad + Power; // assume no losses

        // Control Strategy
        if (std::abs(MyLoad) < QLoad && QLoad != 0.0) {
            PartLoadRatio = std::abs(MyLoad) / QLoad;
            QLoad = std::abs(MyLoad);
            Power *= PartLoadRatio;
            QSource *= PartLoadRatio;
        }

        CpLoadSide =
            GetSpecificHeatGlycol(state, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        CpSourceSide = GetSpecificHeatGlycol(
            state, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        LoadSideOutletTemp = LoadSideInletTemp - QLoad / (LoadSideMassFlowRate * CpLoadSide);
        SourceSideOutletTemp = SourceSideInletTemp + QSource / (SourceSideMassFlowRate * CpSourceSide);

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

        this->reportPower = Power;
        this->reportEnergy = Power * ReportingConstant;
        this->reportQSource = QSource;
        this->reportQLoad = QLoad;
        this->reportQSourceEnergy = QSource * ReportingConstant;
        this->reportQLoadEnergy = QLoad * ReportingConstant;
        this->reportLoadSideOutletTemp = LoadSideOutletTemp;
        this->reportSourceSideOutletTemp = SourceSideOutletTemp;
    }

    void GshpSpecs::CalcWatertoWaterHPHeating(EnergyPlusData &state, Real64 const MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kenneth Tang
        //       DATE WRITTEN   March 2005
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // This routine simulate the heat pump peformance in heating mode

        // REFERENCES:
        // (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        // State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        // Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

        // Using/Aliasing
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using CurveManager::CurveValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CelsiustoKelvin(DataGlobalConstants::KelvinConv); // Conversion from Celsius to Kelvin
        Real64 const Tref(283.15);                // Reference Temperature for performance curves,10C [K]
        static std::string const RoutineName("CalcWatertoWaterHPHeating");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 HeatCapRated;               // Rated Heating Capacity [W]
        Real64 HeatPowerRated;             // Rated Heating Compressor Power[W]
        Real64 LoadSideVolFlowRateRated;   // Rated Load Side Volumetric Flow Rate [m3/s]
        Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
        Real64 LoadSideMassFlowRate;       // Load Side Mass Flow Rate [kg/s]
        Real64 LoadSideInletTemp;          // Load Side Inlet Temperature [C]
        Real64 LoadSideOutletTemp;         // Load side Outlet Temperature [C]
        Real64 SourceSideMassFlowRate;     // Source Side Mass Flow Rate [kg/s]
        Real64 SourceSideInletTemp;        // Source Side Inlet Temperature [C]
        Real64 SourceSideOutletTemp;       // Source Side Outlet Temperature [C]
        Real64 func1;                      // Portion of the heat transfer and power equation
        Real64 func2;                      // Portion of the heat transfer and power equation
        Real64 func3;                      // Portion of the heat transfer and power equation
        Real64 func4;                      // Portion of the heat transfer and power equation
        Real64 Power;                      // Power Consumption [W]
        Real64 QLoad;                      // Cooling Capacity [W]
        Real64 QSource;                    // Source Side Heat Transfer Rate [W]
        Real64 PartLoadRatio;              // Part Load Ratio
        Real64 ReportingConstant;
        Real64 rhoLoadSide;
        Real64 rhoSourceSide;
        Real64 CpLoadSide;
        Real64 CpSourceSide;

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
        LoadSideVolFlowRateRated = this->RatedLoadVolFlowHeat;
        SourceSideVolFlowRateRated = this->RatedSourceVolFlowHeat;
        HeatCapRated = this->RatedCapHeat;
        HeatPowerRated = this->RatedPowerHeat;

        LoadSideMassFlowRate = this->reportLoadSideMassFlowRate;
        LoadSideInletTemp = this->reportLoadSideInletTemp;
        SourceSideMassFlowRate = this->reportSourceSideMassFlowRate;
        SourceSideInletTemp = this->reportSourceSideInletTemp;

        // If heat pump is not operating, THEN return
        if (!this->MustRun) {
            return;
        }
        rhoLoadSide =
            GetDensityGlycol(state, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        rhoSourceSide =
            GetDensityGlycol(state, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        func1 = ((LoadSideInletTemp + CelsiustoKelvin) / Tref);
        func2 = ((SourceSideInletTemp + CelsiustoKelvin) / Tref);
        func3 = (LoadSideMassFlowRate / (LoadSideVolFlowRateRated * rhoLoadSide));
        func4 = (SourceSideMassFlowRate / (SourceSideVolFlowRateRated * rhoSourceSide));

        QLoad = HeatCapRated * CurveValue(state, this->HeatCapCurveIndex, func1, func2, func3, func4);
        Power = HeatPowerRated * CurveValue(state, this->HeatPowCurveIndex, func1, func2, func3, func4);

        if ((QLoad <= 0.0 || Power <= 0.0) && !state.dataGlobal->WarmupFlag) {
            if (QLoad <= 0.0) {
                if (this->HeatCapNegativeCounter < 1) {
                    ++this->HeatCapNegativeCounter;
                    ShowWarningError(state, HPEqFitHeating + " \"" + this->Name + "\":");
                    ShowContinueError(state, format(" Heating capacity curve output is <= 0.0 ({:.4T}).", QLoad));
                    ShowContinueError(state,
                                      format(" Zero or negative value occurs with a load-side inlet temperature of {:.2T} C,", LoadSideInletTemp));
                    ShowContinueError(state, format(" a source-side inlet temperature of {:.2T} C,", SourceSideInletTemp));
                    ShowContinueError(state, format(" a load-side mass flow rate of {:.3T} kg/s,", LoadSideMassFlowRate));
                    ShowContinueError(state, format(" and a source-side mass flow rate of {:.3T} kg/s.", SourceSideMassFlowRate));
                    ShowContinueErrorTimeStamp(state, " The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state, HPEqFitHeating + " \"" + this->Name +
                                                       "\": Heating capacity curve output is <= 0.0 warning continues...",
                                                   this->HeatCapNegativeIndex,
                                                   QLoad,
                                                   QLoad);
                }
            }
            if (Power <= 0.0) {
                if (this->HeatPowerNegativeCounter < 1) {
                    ++this->HeatPowerNegativeCounter;
                    ShowWarningError(state, HPEqFitHeating + " \"" + this->Name + "\":");
                    ShowContinueError(state, format(" Heating compressor power curve output is <= 0.0 ({:.4T}).", Power));
                    ShowContinueError(state,
                                      format(" Zero or negative value occurs with a load-side inlet temperature of {:.2T} C,", LoadSideInletTemp));
                    ShowContinueError(state, format(" a source-side inlet temperature of {:.2T} C,", SourceSideInletTemp));
                    ShowContinueError(state, format(" a load-side mass flow rate of {:.3T} kg/s,", LoadSideMassFlowRate));
                    ShowContinueError(state, format(" and a source-side mass flow rate of {:.3T} kg/s.", SourceSideMassFlowRate));
                    ShowContinueErrorTimeStamp(state, " The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state, HPEqFitHeating + " \"" + this->Name +
                                                       "\": Heating compressor power curve output is <= 0.0 warning continues...",
                                                   this->HeatPowerNegativeIndex,
                                                   Power,
                                                   Power);
                }
            }

            QLoad = 0.0;
            Power = 0.0;
        }

        QSource = QLoad - Power; // assume no losses

        // Control Strategy
        if (std::abs(MyLoad) < QLoad && QLoad != 0.0) {
            PartLoadRatio = std::abs(MyLoad) / QLoad;
            QLoad = std::abs(MyLoad);
            Power *= PartLoadRatio;
            QSource *= PartLoadRatio;
        }

        CpLoadSide =
            GetSpecificHeatGlycol(state, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, state.dataPlnt->PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        CpSourceSide = GetSpecificHeatGlycol(
            state, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, state.dataPlnt->PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        LoadSideOutletTemp = LoadSideInletTemp + QLoad / (LoadSideMassFlowRate * CpLoadSide);
        SourceSideOutletTemp = SourceSideInletTemp - QSource / (SourceSideMassFlowRate * CpSourceSide);

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

        this->reportPower = Power;
        this->reportEnergy = Power * ReportingConstant;
        this->reportQSource = QSource;
        this->reportQLoad = QLoad;
        this->reportQSourceEnergy = QSource * ReportingConstant;
        this->reportQLoadEnergy = QLoad * ReportingConstant;
        this->reportLoadSideOutletTemp = LoadSideOutletTemp;
        this->reportSourceSideOutletTemp = SourceSideOutletTemp;
    }

    void GshpSpecs::UpdateGSHPRecords()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Kenneth Tang
        //       DATE WRITTEN:    March 2005

        int LoadSideOutletNode = this->LoadSideOutletNodeNum;
        int SourceSideOutletNode = this->SourceSideOutletNodeNum;

        if (!this->MustRun) {
            // Heatpump is off; just pass through conditions
            this->reportPower = 0.0;
            this->reportEnergy = 0.0;
            this->reportQSource = 0.0;
            this->reportQSourceEnergy = 0.0;
            this->reportQLoad = 0.0;
            this->reportQLoadEnergy = 0.0;
            this->reportLoadSideOutletTemp = this->reportLoadSideInletTemp;
            this->reportSourceSideOutletTemp = this->reportSourceSideInletTemp;
        }

        Node(SourceSideOutletNode).Temp = this->reportSourceSideOutletTemp;
        Node(LoadSideOutletNode).Temp = this->reportLoadSideOutletTemp;
    }

} // namespace EnergyPlus
