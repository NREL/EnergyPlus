// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <HeatPumpWaterToWaterSimple.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Plant/PlantLocation.hh>
#include <PlantComponent.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterSimple {

    // MODULE INFORMATION:
    //       AUTHOR         Kenneth Tang
    //       DATE WRITTEN   March 2005
    //       MODIFIED       Brent Griffith, plant upgrades, fluid properties
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates a Water-to-Water Heat Pump Simple (Equation-Fit Model)

    // METHODOLOGY EMPLOYED:
    // This simulation is based on a set of coefficients generated from
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
    using namespace DataPrecisionGlobals;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::BeginSimFlag;
    using DataGlobals::DayOfSim;
    using DataGlobals::HourOfDay;
    using DataGlobals::KelvinConv;
    using DataGlobals::SecInHour;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataGlobals::WarmupFlag;
    using General::TrimSigDigits;
    using namespace DataLoopNode;

    // MODULE PARAMETER DEFINITIONS
    std::string const HPEqFitHeating("HeatPump:WatertoWater:EquationFit:Heating");
    std::string const HPEqFitHeatingUC("HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING");
    std::string const HPEqFitCooling("HeatPump:WatertoWater:EquationFit:Cooling");
    std::string const HPEqFitCoolingUC("HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING");

    // MODULE VARIABLE DECLARATIONS:
    int NumGSHPs(0); // Number of GSHPs specified in input
    namespace {
        bool GetInputFlag(true); // then TRUE, calls subroutine to read input file.
    }                            // namespace

    // Object Data
    Array1D<GshpSpecs> GSHP;
    std::unordered_map<std::string, std::string> HeatPumpWaterUniqueNames;

    void GshpSpecs::clear_state()
    {
        NumGSHPs = 0;
        GetInputFlag = true;
        HeatPumpWaterUniqueNames.clear();
        GSHP.deallocate();
    }

    PlantComponent *GshpSpecs::factory(int wwhp_type, std::string eir_wwhp_name)
    {
        if (GetInputFlag) {
            GshpSpecs::GetWatertoWaterHPInput();
            GetInputFlag = false;
        }

        for (auto &wwhp : GSHP) {
            if (wwhp.Name == eir_wwhp_name && wwhp.WWHPPlantTypeOfNum == wwhp_type) {
                return &wwhp;
            }
        }

        ShowFatalError("EquationFit_WWHP factory: Error getting inputs for wwhp named: " + eir_wwhp_name);
        return nullptr;
    }

    void GshpSpecs::simulate(const PlantLocation &calledFromLocation, bool const FirstHVACIteration, Real64 &CurLoad, bool const EP_UNUSED(RunFlag))
    {
        if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFCooling) {
            if (calledFromLocation.loopNum == this->LoadLoopNum) { // chilled water loop
                this->InitWatertoWaterHP(this->WWHPPlantTypeOfNum, this->Name, FirstHVACIteration, CurLoad);
                this->CalcWatertoWaterHPCooling(CurLoad);
                this->UpdateGSHPRecords();
            } else if (calledFromLocation.loopNum == this->SourceLoopNum) { // condenser loop
                PlantUtilities::UpdateChillerComponentCondenserSide(this->SourceLoopNum,
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
                ShowFatalError("SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + this->Name);
            }
        } else if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFHeating) {
            if (calledFromLocation.loopNum == this->LoadLoopNum) { // chilled water loop
                this->InitWatertoWaterHP(this->WWHPPlantTypeOfNum, this->Name, FirstHVACIteration, CurLoad);
                this->CalcWatertoWaterHPHeating(CurLoad);
                this->UpdateGSHPRecords();
            } else if (calledFromLocation.loopNum == this->SourceLoopNum) { // condenser loop
                PlantUtilities::UpdateChillerComponentCondenserSide(this->SourceLoopNum,
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
                ShowFatalError("SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + this->Name);
            }
        } else {
            ShowFatalError("SimHPWatertoWaterSimple: Module called with incorrect GSHPType");
        } // TypeOfEquip
    }

    void GshpSpecs::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        this->InitWatertoWaterHP(
            this->WWHPPlantTypeOfNum, this->Name, true, 0.0); // Changed to true for FirstHVAC and 0.0 for Curload during this phase
        if (calledFromLocation.loopNum == this->LoadLoopNum) {
            if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFCooling) {
                this->sizeCoolingWaterToWaterHP();
                MinLoad = 0.0;
                MaxLoad = this->RatedCapCool;
                OptLoad = this->RatedCapCool;
            } else if (this->WWHPPlantTypeOfNum == DataPlant::TypeOf_HPWaterEFHeating) {
                this->sizeHeatingWaterToWaterHP();
                MinLoad = 0.0;
                MaxLoad = this->RatedCapHeat;
                OptLoad = this->RatedCapHeat;
            } else {
                ShowFatalError("SimHPWatertoWaterSimple: Module called with incorrect GSHPType");
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

    void GshpSpecs::GetWatertoWaterHPInput()
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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int GSHPNum;     // GSHP number
        int HPNum;       // Counter
        int NumCoolCoil; // Number of Cooling Coils
        int NumHeatCoil; // Number of Heating Coils
        int NumAlphas;   // Number of elements in the alpha array
        int NumNums;     // Number of elements in the numeric array
        int IOStat;      // IO Status when calling get input subroutine

        static bool ErrorsFound(false);

        NumCoolCoil = inputProcessor->getNumObjectsFound(HPEqFitCoolingUC);
        NumHeatCoil = inputProcessor->getNumObjectsFound(HPEqFitHeatingUC);
        NumGSHPs = NumCoolCoil + NumHeatCoil;

        if (NumGSHPs <= 0) {
            ShowSevereError("GetEquationFitWaterToWater Input: No Equipment found");
            ErrorsFound = true;
        }

        if (NumGSHPs > 0) {
            GSHP.allocate(NumGSHPs);
            HeatPumpWaterUniqueNames.reserve(NumGSHPs);
        }

        // Load data structure for cooling coil
        for (HPNum = 1; HPNum <= NumCoolCoil; ++HPNum) {

            GSHPNum = HPNum;

            inputProcessor->getObjectItem(HPEqFitCoolingUC,
                                          HPNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks);
            GlobalNames::VerifyUniqueInterObjectName(HeatPumpWaterUniqueNames, DataIPShortCuts::cAlphaArgs(1), HPEqFitCoolingUC, ErrorsFound);
            GSHP(GSHPNum).WWHPPlantTypeOfNum = TypeOf_HPWaterEFCooling;
            GSHP(GSHPNum).Name = DataIPShortCuts::cAlphaArgs(1);
            GSHP(GSHPNum).RatedLoadVolFlowCool = DataIPShortCuts::rNumericArgs(1);
            if (GSHP(GSHPNum).RatedLoadVolFlowCool == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedLoadVolFlowCoolWasAutoSized = true;
            }
            GSHP(GSHPNum).RatedSourceVolFlowCool = DataIPShortCuts::rNumericArgs(2);
            if (GSHP(GSHPNum).RatedSourceVolFlowCool == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedSourceVolFlowCoolWasAutoSized = true;
            }
            GSHP(GSHPNum).RatedCapCool = DataIPShortCuts::rNumericArgs(3);
            if (GSHP(GSHPNum).RatedCapCool == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedCapCoolWasAutoSized = true;
            }
            GSHP(GSHPNum).RatedPowerCool = DataIPShortCuts::rNumericArgs(4);
            if (GSHP(GSHPNum).RatedPowerCool == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedPowerCoolWasAutoSized = true;
            }
            GSHP(GSHPNum).CoolCap1 = DataIPShortCuts::rNumericArgs(5);
            GSHP(GSHPNum).CoolCap2 = DataIPShortCuts::rNumericArgs(6);
            GSHP(GSHPNum).CoolCap3 = DataIPShortCuts::rNumericArgs(7);
            GSHP(GSHPNum).CoolCap4 = DataIPShortCuts::rNumericArgs(8);
            GSHP(GSHPNum).CoolCap5 = DataIPShortCuts::rNumericArgs(9);
            GSHP(GSHPNum).CoolPower1 = DataIPShortCuts::rNumericArgs(10);
            GSHP(GSHPNum).CoolPower2 = DataIPShortCuts::rNumericArgs(11);
            GSHP(GSHPNum).CoolPower3 = DataIPShortCuts::rNumericArgs(12);
            GSHP(GSHPNum).CoolPower4 = DataIPShortCuts::rNumericArgs(13);
            GSHP(GSHPNum).CoolPower5 = DataIPShortCuts::rNumericArgs(14);

            if (NumNums > 14) {
                if (!DataIPShortCuts::lNumericFieldBlanks(15)) {
                    GSHP(GSHPNum).refCOP = DataIPShortCuts::rNumericArgs(15);
                } else {
                    GSHP(GSHPNum).refCOP = 8.0;
                }

            } else {
                GSHP(GSHPNum).refCOP = 8.0;
            }

            // calculate reference COP if hard sized
            if (!GSHP(GSHPNum).ratedPowerCoolWasAutoSized && !GSHP(GSHPNum).ratedCapCoolWasAutoSized && GSHP(GSHPNum).RatedPowerCool > 0.0) {
                GSHP(GSHPNum).refCOP = GSHP(GSHPNum).RatedCapCool / GSHP(GSHPNum).RatedPowerCool;
            }

            if (NumNums > 15) {
                if (!DataIPShortCuts::lNumericFieldBlanks(16)) {
                    GSHP(GSHPNum).sizFac = DataIPShortCuts::rNumericArgs(16);
                } else {
                    GSHP(GSHPNum).sizFac = 1.0;
                }
            } else {
                GSHP(GSHPNum).sizFac = 1.0;
            }

            GSHP(GSHPNum).SourceSideInletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(2),
                                                                     ErrorsFound,
                                                                     HPEqFitCoolingUC,
                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                     NodeType_Water,
                                                                     NodeConnectionType_Inlet,
                                                                     1,
                                                                     ObjectIsNotParent);

            GSHP(GSHPNum).SourceSideOutletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(3),
                                                                      ErrorsFound,
                                                                      HPEqFitCoolingUC,
                                                                      DataIPShortCuts::cAlphaArgs(1),
                                                                      NodeType_Water,
                                                                      NodeConnectionType_Outlet,
                                                                      1,
                                                                      ObjectIsNotParent);

            GSHP(GSHPNum).LoadSideInletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(4),
                                                                   ErrorsFound,
                                                                   HPEqFitCoolingUC,
                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                   NodeType_Water,
                                                                   NodeConnectionType_Inlet,
                                                                   2,
                                                                   ObjectIsNotParent);

            GSHP(GSHPNum).LoadSideOutletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                    ErrorsFound,
                                                                    HPEqFitCoolingUC,
                                                                    DataIPShortCuts::cAlphaArgs(1),
                                                                    NodeType_Water,
                                                                    NodeConnectionType_Outlet,
                                                                    2,
                                                                    ObjectIsNotParent);

            // Test node sets
            TestCompSet(HPEqFitCoolingUC,
                        DataIPShortCuts::cAlphaArgs(1),
                        DataIPShortCuts::cAlphaArgs(2),
                        DataIPShortCuts::cAlphaArgs(3),
                        "Condenser Water Nodes");
            TestCompSet(HPEqFitCoolingUC,
                        DataIPShortCuts::cAlphaArgs(1),
                        DataIPShortCuts::cAlphaArgs(4),
                        DataIPShortCuts::cAlphaArgs(5),
                        "Chilled Water Nodes");

            if (NumAlphas > 5 && !DataIPShortCuts::lAlphaFieldBlanks(6)) {
                GSHP(GSHPNum).companionName = DataIPShortCuts::cAlphaArgs(6);
            }

            // CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Cooling'
            SetupOutputVariable("Water to Water Heat Pump Electric Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).reportEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "Plant");
            SetupOutputVariable("Water to Water Heat Pump Load Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).reportQLoadEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).reportQSourceEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name);
        }

        // Load data structure for heating coil
        for (HPNum = 1; HPNum <= NumHeatCoil; ++HPNum) {

            GSHPNum = NumCoolCoil + HPNum;

            inputProcessor->getObjectItem(HPEqFitHeatingUC,
                                          HPNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks);
            GlobalNames::VerifyUniqueInterObjectName(HeatPumpWaterUniqueNames, DataIPShortCuts::cAlphaArgs(1), HPEqFitHeatingUC, ErrorsFound);
            GSHP(GSHPNum).WWHPPlantTypeOfNum = TypeOf_HPWaterEFHeating;
            GSHP(GSHPNum).Name = DataIPShortCuts::cAlphaArgs(1);
            GSHP(GSHPNum).RatedLoadVolFlowHeat = DataIPShortCuts::rNumericArgs(1);
            if (GSHP(GSHPNum).RatedLoadVolFlowHeat == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedLoadVolFlowHeatWasAutoSized = true;
            }
            GSHP(GSHPNum).RatedSourceVolFlowHeat = DataIPShortCuts::rNumericArgs(2);
            if (GSHP(GSHPNum).RatedSourceVolFlowHeat == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedSourceVolFlowHeatWasAutoSized = true;
            }
            GSHP(GSHPNum).RatedCapHeat = DataIPShortCuts::rNumericArgs(3);
            if (GSHP(GSHPNum).RatedCapHeat == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedCapHeatWasAutoSized = true;
            }
            GSHP(GSHPNum).RatedPowerHeat = DataIPShortCuts::rNumericArgs(4);
            if (GSHP(GSHPNum).RatedPowerHeat == DataSizing::AutoSize) {
                GSHP(GSHPNum).ratedPowerHeatWasAutoSized = true;
            }

            GSHP(GSHPNum).HeatCap1 = DataIPShortCuts::rNumericArgs(5);
            GSHP(GSHPNum).HeatCap2 = DataIPShortCuts::rNumericArgs(6);
            GSHP(GSHPNum).HeatCap3 = DataIPShortCuts::rNumericArgs(7);
            GSHP(GSHPNum).HeatCap4 = DataIPShortCuts::rNumericArgs(8);
            GSHP(GSHPNum).HeatCap5 = DataIPShortCuts::rNumericArgs(9);
            GSHP(GSHPNum).HeatPower1 = DataIPShortCuts::rNumericArgs(10);
            GSHP(GSHPNum).HeatPower2 = DataIPShortCuts::rNumericArgs(11);
            GSHP(GSHPNum).HeatPower3 = DataIPShortCuts::rNumericArgs(12);
            GSHP(GSHPNum).HeatPower4 = DataIPShortCuts::rNumericArgs(13);
            GSHP(GSHPNum).HeatPower5 = DataIPShortCuts::rNumericArgs(14);

            if (NumNums > 14) {
                if (!DataIPShortCuts::lNumericFieldBlanks(15)) {
                    GSHP(GSHPNum).refCOP = DataIPShortCuts::rNumericArgs(15);
                } else {
                    GSHP(GSHPNum).refCOP = 7.5;
                }

            } else {
                GSHP(GSHPNum).refCOP = 7.5;
            }

            // calculate reference COP if hard sized
            if (!GSHP(GSHPNum).ratedPowerHeatWasAutoSized && !GSHP(GSHPNum).ratedCapHeatWasAutoSized && GSHP(GSHPNum).RatedPowerHeat > 0.0) {
                GSHP(GSHPNum).refCOP = GSHP(GSHPNum).RatedCapHeat / GSHP(GSHPNum).RatedPowerHeat;
            }

            if (NumNums > 15) {
                if (!DataIPShortCuts::lNumericFieldBlanks(16)) {
                    GSHP(GSHPNum).sizFac = DataIPShortCuts::rNumericArgs(16);
                } else {
                    GSHP(GSHPNum).sizFac = 1.0;
                }
            } else {
                GSHP(GSHPNum).sizFac = 1.0;
            }

            GSHP(GSHPNum).SourceSideInletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(2),
                                                                     ErrorsFound,
                                                                     HPEqFitHeatingUC,
                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                     NodeType_Water,
                                                                     NodeConnectionType_Inlet,
                                                                     1,
                                                                     ObjectIsNotParent);

            GSHP(GSHPNum).SourceSideOutletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(3),
                                                                      ErrorsFound,
                                                                      HPEqFitHeatingUC,
                                                                      DataIPShortCuts::cAlphaArgs(1),
                                                                      NodeType_Water,
                                                                      NodeConnectionType_Outlet,
                                                                      1,
                                                                      ObjectIsNotParent);

            GSHP(GSHPNum).LoadSideInletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(4),
                                                                   ErrorsFound,
                                                                   HPEqFitHeatingUC,
                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                   NodeType_Water,
                                                                   NodeConnectionType_Inlet,
                                                                   2,
                                                                   ObjectIsNotParent);

            GSHP(GSHPNum).LoadSideOutletNodeNum = GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                    ErrorsFound,
                                                                    HPEqFitHeatingUC,
                                                                    DataIPShortCuts::cAlphaArgs(1),
                                                                    NodeType_Water,
                                                                    NodeConnectionType_Outlet,
                                                                    2,
                                                                    ObjectIsNotParent);

            if (NumAlphas > 5 && !DataIPShortCuts::lAlphaFieldBlanks(6)) {
                GSHP(GSHPNum).companionName = DataIPShortCuts::cAlphaArgs(6);
            }

            // Test node sets
            TestCompSet(HPEqFitHeatingUC,
                        DataIPShortCuts::cAlphaArgs(1),
                        DataIPShortCuts::cAlphaArgs(2),
                        DataIPShortCuts::cAlphaArgs(3),
                        "Condenser Water Nodes");
            TestCompSet(
                HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(4), DataIPShortCuts::cAlphaArgs(5), "Hot Water Nodes");

            // CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Heating'
            SetupOutputVariable("Water to Water Heat Pump Electric Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).reportEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "Plant");
            SetupOutputVariable("Water to Water Heat Pump Load Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).reportQLoadEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).reportQSourceEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name);
        }

        // now process companion coils, if any
        for (GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum) {
            if (!GSHP(GSHPNum).companionName.empty()) {
                GSHP(GSHPNum).companionIndex = UtilityRoutines::FindItemInList(GSHP(GSHPNum).companionName, GSHP);
                if (GSHP(GSHPNum).companionIndex == 0) {
                    ShowSevereError("GetEquationFitWaterToWater Input: did not find companion heat pump named '" + GSHP(GSHPNum).companionName +
                                    "' in heat pump called " + GSHP(GSHPNum).Name);
                    ErrorsFound = true;
                } else {
                    GSHP(GSHPNum).companionIdentified = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for Water to Water Heat Pumps");
        }

        for (GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum) {
            // setup output variables
            SetupOutputVariable("Water to Water Heat Pump Electric Power",
                                OutputProcessor::Unit::W,
                                GSHP(GSHPNum).reportPower,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                GSHP(GSHPNum).reportQLoad,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                GSHP(GSHPNum).reportQSource,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).reportLoadSideOutletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).reportLoadSideInletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).reportSourceSideOutletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).reportSourceSideInletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                GSHP(GSHPNum).reportLoadSideMassFlowRate,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                GSHP(GSHPNum).reportSourceSideMassFlowRate,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
        }
    }

    void GshpSpecs::InitWatertoWaterHP(int const GSHPTypeNum,                  // Type of GSHP
                                       std::string const &EP_UNUSED(GSHPName), // User Specified Name of GSHP
                                       bool const EP_UNUSED(FirstHVACIteration),
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
        using DataPlant::PlantLoop;
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
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    this->WWHPPlantTypeOfNum,
                                                    this->SourceLoopNum,
                                                    this->SourceLoopSideNum,
                                                    this->SourceBranchNum,
                                                    this->SourceCompNum,
                                                    _,
                                                    _,
                                                    _,
                                                    this->SourceSideInletNodeNum,
                                                    _,
                                                    errFlag);
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    this->WWHPPlantTypeOfNum,
                                                    this->LoadLoopNum,
                                                    this->LoadLoopSideNum,
                                                    this->LoadBranchNum,
                                                    this->LoadCompNum,
                                                    _,
                                                    _,
                                                    _,
                                                    this->LoadSideInletNodeNum,
                                                    _,
                                                    errFlag);

            if (!errFlag) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->LoadLoopNum, this->LoadLoopSideNum, this->SourceLoopNum, this->SourceLoopSideNum, this->WWHPPlantTypeOfNum, true);
            }

            if (errFlag) {
                ShowFatalError("GetWatertoWaterHPInput: Program terminated on scan for loop data");
            }
            this->MyPlantScanFlag = false;
        }

        if (this->MyEnvrnFlag && BeginEnvrnFlag) {
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
                    PlantLoop(this->LoadLoopNum).FluidName, DataGlobals::CWInitConvTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);
                this->LoadSideDesignMassFlow = this->RatedLoadVolFlowHeat * rho;
                rho = GetDensityGlycol(
                    PlantLoop(this->SourceLoopNum).FluidName, DataGlobals::CWInitConvTemp, PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);
                this->SourceSideDesignMassFlow = this->RatedSourceVolFlowHeat * rho;
            } else if (this->WWHPPlantTypeOfNum == TypeOf_HPWaterEFCooling) {
                rho = GetDensityGlycol(
                    PlantLoop(this->LoadLoopNum).FluidName, DataGlobals::CWInitConvTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);
                this->LoadSideDesignMassFlow = this->RatedLoadVolFlowCool * rho;
                rho = GetDensityGlycol(
                    PlantLoop(this->SourceLoopNum).FluidName, DataGlobals::CWInitConvTemp, PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);
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
        if (!BeginEnvrnFlag) this->MyEnvrnFlag = true;

        if (PrevSimTime != CurrentSimTime) {
            PrevSimTime = CurrentSimTime;
        }

        // Calculate the simulation time
        CurrentSimTime = (DayOfSim - 1) * 24 + (HourOfDay - 1) + (TimeStep - 1) * TimeStepZone + SysTimeElapsed;

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

            SetComponentFlowRate(this->reportLoadSideMassFlowRate,
                                 this->LoadSideInletNodeNum,
                                 this->LoadSideOutletNodeNum,
                                 this->LoadLoopNum,
                                 this->LoadLoopSideNum,
                                 this->LoadBranchNum,
                                 this->LoadCompNum);
            SetComponentFlowRate(this->reportSourceSideMassFlowRate,
                                 this->SourceSideInletNodeNum,
                                 this->SourceSideOutletNodeNum,
                                 this->SourceLoopNum,
                                 this->SourceLoopSideNum,
                                 this->SourceBranchNum,
                                 this->SourceCompNum);
            PlantUtilities::PullCompInterconnectTrigger(this->LoadLoopNum,
                                                        this->LoadLoopSideNum,
                                                        this->LoadBranchNum,
                                                        this->LoadCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->SourceLoopNum,
                                                        this->LoadLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
                                                        this->reportSourceSideMassFlowRate);
            // Set flows if the heat pump is running
        } else { // the heat pump must run

            this->reportLoadSideMassFlowRate = this->LoadSideDesignMassFlow;
            this->reportSourceSideMassFlowRate = this->SourceSideDesignMassFlow;
            // now check against and request in plant
            SetComponentFlowRate(this->reportLoadSideMassFlowRate,
                                 this->LoadSideInletNodeNum,
                                 this->LoadSideOutletNodeNum,
                                 this->LoadLoopNum,
                                 this->LoadLoopSideNum,
                                 this->LoadBranchNum,
                                 this->LoadCompNum);
            SetComponentFlowRate(this->reportSourceSideMassFlowRate,
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

                SetComponentFlowRate(this->reportLoadSideMassFlowRate,
                                     this->LoadSideInletNodeNum,
                                     this->LoadSideOutletNodeNum,
                                     this->LoadLoopNum,
                                     this->LoadLoopSideNum,
                                     this->LoadBranchNum,
                                     this->LoadCompNum);
                SetComponentFlowRate(this->reportSourceSideMassFlowRate,
                                     this->SourceSideInletNodeNum,
                                     this->SourceSideOutletNodeNum,
                                     this->SourceLoopNum,
                                     this->SourceLoopSideNum,
                                     this->SourceBranchNum,
                                     this->SourceCompNum);
                PlantUtilities::PullCompInterconnectTrigger(this->LoadLoopNum,
                                                            this->LoadLoopSideNum,
                                                            this->LoadBranchNum,
                                                            this->LoadCompNum,
                                                            this->CondMassFlowIndex,
                                                            this->SourceLoopNum,
                                                            this->LoadLoopSideNum,
                                                            DataPlant::CriteriaType_MassFlowRate,
                                                            this->reportSourceSideMassFlowRate);
                return;
            }
            PlantUtilities::PullCompInterconnectTrigger(this->LoadLoopNum,
                                                        this->LoadLoopSideNum,
                                                        this->LoadBranchNum,
                                                        this->LoadCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->SourceLoopNum,
                                                        this->LoadLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
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

    void GshpSpecs::sizeCoolingWaterToWaterHP()
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
            this->RatedLoadVolFlowHeat = GSHP(this->companionIndex).RatedLoadVolFlowHeat;
            this->ratedLoadVolFlowHeatWasAutoSized = GSHP(this->companionIndex).ratedLoadVolFlowHeatWasAutoSized;
            this->RatedSourceVolFlowHeat = GSHP(this->companionIndex).RatedSourceVolFlowHeat;
            this->ratedSourceVolFlowHeatWasAutoSized = GSHP(this->companionIndex).ratedSourceVolFlowHeatWasAutoSized;
            this->RatedCapHeat = GSHP(this->companionIndex).RatedCapHeat;
            this->ratedCapHeatWasAutoSized = GSHP(this->companionIndex).ratedCapHeatWasAutoSized;
            this->RatedPowerHeat = GSHP(this->companionIndex).RatedPowerHeat;
            this->ratedPowerHeatWasAutoSized = GSHP(this->companionIndex).ratedPowerHeatWasAutoSized;
        }

        int pltLoadSizNum = DataPlant::PlantLoop(this->LoadLoopNum).PlantSizNum;
        if (pltLoadSizNum > 0) {
            if (DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow) {
                tmpLoadSideVolFlowRate = DataSizing::PlantSizData(pltLoadSizNum).DesVolFlowRate * this->sizFac;
                // now compare to companion coil and take higher
                if (this->companionIdentified) {
                    tmpLoadSideVolFlowRate = max(tmpLoadSideVolFlowRate, this->RatedLoadVolFlowHeat);
                    // store flow rate right away regardless of PlantFirstSizesOkayToFinalize so that data are available
                    this->RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
                }
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobals::CWInitConvTemp,
                                                               DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCoolingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else if (this->companionIdentified && this->RatedLoadVolFlowHeat > 0.0) {
                tmpLoadSideVolFlowRate = this->RatedLoadVolFlowHeat;
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobals::CWInitConvTemp,
                                                               DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCoolingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else {
                if (this->ratedCapCoolWasAutoSized) tmpCoolingCap = 0.0;
                if (this->ratedLoadVolFlowCoolWasAutoSized) tmpLoadSideVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->ratedCapCoolWasAutoSized) {
                    this->RatedCapCool = tmpCoolingCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Design Size Nominal Capacity [W]", tmpCoolingCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Initial Design Size Nominal Capacity [W]", tmpCoolingCap);
                    }
                } else {
                    if (this->RatedCapCool > 0.0 && tmpCoolingCap > 0.0) {
                        Real64 nomCoolingCapUser = this->RatedCapCool;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            if (DataGlobals::DoPlantSizing) {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                        this->Name,
                                                                        "Design Size Nominal Capacity [W]",
                                                                        tmpCoolingCap,
                                                                        "User-Specified Nominal Capacity [W]",
                                                                        nomCoolingCapUser);
                            } else {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                        this->Name,
                                                                        "User-Specified Nominal Capacity [W]",
                                                                        nomCoolingCapUser);
                            }

                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCoolingCap - nomCoolingCapUser) / nomCoolingCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(nomCoolingCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpCoolingCap, 2) +
                                                      " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCoolingCap = nomCoolingCapUser;
                    }
                }
                if (this->ratedLoadVolFlowCoolWasAutoSized) {
                    this->RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                this->Name,
                                                                "Design Size Load Side Volume Flow Rate [m3/s]",
                                                                tmpLoadSideVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                this->Name,
                                                                "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                                tmpLoadSideVolFlowRate);
                    }
                } else {
                    if (this->RatedLoadVolFlowCool > 0.0 && tmpLoadSideVolFlowRate > 0.0) {
                        Real64 nomLoadSideVolFlowUser = this->RatedLoadVolFlowCool;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            if (DataGlobals::DoPlantSizing) {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                        this->Name,
                                                                        "Design Size Load Side Volume Flow Rate [m3/s]",
                                                                        tmpLoadSideVolFlowRate,
                                                                        "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                                        nomLoadSideVolFlowUser);
                            } else {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                        this->Name,
                                                                        "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                                        nomLoadSideVolFlowUser);
                            }
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpLoadSideVolFlowRate - nomLoadSideVolFlowUser) / nomLoadSideVolFlowUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Load Side Volume Flow Rate of " +
                                                      General::RoundSigDigits(nomLoadSideVolFlowUser, 2) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Load Side Volume Flow Rate of " +
                                                      General::RoundSigDigits(tmpLoadSideVolFlowRate, 2) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        this->RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                    this->Name,
                                                                    "Design Size Load Side Volume Flow Rate [m3/s]",
                                                                    tmpLoadSideVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                    this->Name,
                                                                    "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                                    tmpLoadSideVolFlowRate);
                        }
                    }
                }
                if (this->ratedCapHeatWasAutoSized && this->RatedCapHeat > 0.0) {
                    tmpCoolingCap = this->RatedCapHeat;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        this->RatedCapCool = tmpCoolingCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Design Size Nominal Capacity [W]", tmpCoolingCap);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Initial Design Size Nominal Capacity [W]", tmpCoolingCap);
                        }
                    }
                }
            } else { // no companion heatpump, no plant sizing object
                if ((this->ratedLoadVolFlowCoolWasAutoSized || this->ratedCapCoolWasAutoSized) && DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Water to Water Heat Pump requires a loop Sizing:Plant object.");
                    ShowContinueError("Occurs in HeatPump:WaterToWater:EquationFit:Cooling object = " + this->Name);
                    errorsFound = true;
                }
            }

            if (!this->ratedLoadVolFlowCoolWasAutoSized && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "User-Specified Load Side Flow Rate [m3/s]", this->RatedLoadVolFlowCool);
            }
            if (!this->ratedCapCoolWasAutoSized && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "User-Specified Nominal Capacity [W]", this->RatedCapCool);
            }
        }
        if (!this->ratedLoadVolFlowCoolWasAutoSized) tmpLoadSideVolFlowRate = this->RatedLoadVolFlowCool;
        int pltSourceSizNum = DataPlant::PlantLoop(this->SourceLoopNum).PlantSizNum;
        if (pltSourceSizNum > 0) {
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceLoopNum).FluidName,
                                                           DataGlobals::CWInitConvTemp,
                                                           DataPlant::PlantLoop(this->SourceLoopNum).FluidIndex,
                                                           RoutineName);
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SourceLoopNum).FluidName,
                                                               DataGlobals::CWInitConvTemp,
                                                               DataPlant::PlantLoop(this->SourceLoopNum).FluidIndex,
                                                               RoutineName);
            tmpSourceSideVolFlowRate = tmpCoolingCap * (1.0 + (1.0 / this->refCOP)) / (DataSizing::PlantSizData(pltSourceSizNum).DeltaT * Cp * rho);
        } else {
            tmpSourceSideVolFlowRate = tmpLoadSideVolFlowRate; // set source side flow equal to load side flow, assumption
        }

        if (this->ratedSourceVolFlowCoolWasAutoSized) {
            this->RatedSourceVolFlowCool = tmpSourceSideVolFlowRate;
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                        this->Name,
                                                        "Design Size Source Side Volume Flow Rate [m3/s]",
                                                        tmpSourceSideVolFlowRate);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                        this->Name,
                                                        "Initial Design Size Source Side Volume Flow Rate [m3/s]",
                                                        tmpSourceSideVolFlowRate);
            }
        } else {
            if (this->RatedSourceVolFlowCool > 0.0 && tmpSourceSideVolFlowRate > 0.0) {
                Real64 nomSourceSideVolFlowUser = this->RatedSourceVolFlowCool;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (DataGlobals::DoPlantSizing) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                this->Name,
                                                                "Design Size Source Side Volume Flow Rate [m3/s]",
                                                                tmpSourceSideVolFlowRate,
                                                                "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                                nomSourceSideVolFlowUser);
                    } else {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                this->Name,
                                                                "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                                nomSourceSideVolFlowUser);
                    }
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(tmpSourceSideVolFlowRate - nomSourceSideVolFlowUser) / nomSourceSideVolFlowUser) >
                            DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError("User-Specified Source Side Volume Flow Rate of " +
                                              General::RoundSigDigits(nomSourceSideVolFlowUser, 2) + " [m3/s]");
                            ShowContinueError("differs from Design Size Source Side Volume Flow Rate of " +
                                              General::RoundSigDigits(tmpSourceSideVolFlowRate, 2) + " [m3/s]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Design Size Cooling Power Consumption [W]", tmpPowerDraw);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Cooling", this->Name, "Initial Design Size Cooling Power Consumption [W]", tmpPowerDraw);
            }
        } else {
            if (this->RatedPowerCool > 0.0 && tmpPowerDraw > 0.0) {
                Real64 nomPowerDrawUser = this->RatedPowerCool;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (DataGlobals::DoPlantSizing) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                this->Name,
                                                                "Design Size Cooling Power Consumption [W]",
                                                                tmpPowerDraw,
                                                                "User-Specified Cooling Power Consumption [W]",
                                                                nomPowerDrawUser);
                    } else {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Cooling",
                                                                this->Name,
                                                                "User-Specified Cooling Power Consumption [W]",
                                                                nomPowerDrawUser);
                    }
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(tmpPowerDraw - nomPowerDrawUser) / nomPowerDrawUser) > DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError("User-Specified Cooling Power Consumption of " + General::RoundSigDigits(nomPowerDrawUser, 2) + " [W]");
                            ShowContinueError("differs from Design Size Cooling Power Consumption of " + General::RoundSigDigits(tmpPowerDraw, 2) +
                                              " [W]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "HeatPump:WaterToWater:EquationFit:Cooling");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->refCOP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->RatedPowerCool);
        }

        if (errorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void GshpSpecs::sizeHeatingWaterToWaterHP()
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
            this->RatedLoadVolFlowCool = GSHP(this->companionIndex).RatedLoadVolFlowCool;
            this->ratedLoadVolFlowCoolWasAutoSized = GSHP(this->companionIndex).ratedLoadVolFlowCoolWasAutoSized;
            this->RatedSourceVolFlowCool = GSHP(this->companionIndex).RatedSourceVolFlowCool;
            this->ratedSourceVolFlowCoolWasAutoSized = GSHP(this->companionIndex).ratedSourceVolFlowCoolWasAutoSized;
            this->RatedCapCool = GSHP(this->companionIndex).RatedCapCool;
            this->ratedCapCoolWasAutoSized = GSHP(this->companionIndex).ratedCapCoolWasAutoSized;
            this->RatedPowerCool = GSHP(this->companionIndex).RatedPowerCool;
            this->ratedPowerCoolWasAutoSized = GSHP(this->companionIndex).ratedPowerCoolWasAutoSized;
        }

        int pltLoadSizNum = DataPlant::PlantLoop(this->LoadLoopNum).PlantSizNum;
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
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobals::HWInitConvTemp,
                                                               DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobals::HWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpHeatingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else if (this->companionIdentified && this->RatedLoadVolFlowCool > 0.0) {
                tmpLoadSideVolFlowRate = this->RatedLoadVolFlowCool;
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                               DataGlobals::HWInitConvTemp,
                                                               DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                                   DataGlobals::HWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpHeatingCap = Cp * rho * DataSizing::PlantSizData(pltLoadSizNum).DeltaT * tmpLoadSideVolFlowRate;
            } else {
                if (this->ratedCapHeatWasAutoSized) tmpHeatingCap = 0.0;
                if (this->ratedLoadVolFlowHeatWasAutoSized) tmpLoadSideVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->ratedCapHeatWasAutoSized) {
                    this->RatedCapHeat = tmpHeatingCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Design Size Nominal Capacity [W]", tmpHeatingCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Initial Design Size Nominal Capacity [W]", tmpHeatingCap);
                    }
                } else {
                    if (this->RatedCapHeat > 0.0 && tmpHeatingCap > 0.0) {
                        Real64 nomHeatingCapUser = this->RatedCapHeat;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            if (DataGlobals::DoPlantSizing) {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                        this->Name,
                                                                        "Design Size Nominal Capacity [W]",
                                                                        tmpHeatingCap,
                                                                        "User-Specified Nominal Capacity [W]",
                                                                        nomHeatingCapUser);
                            } else {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                        this->Name,
                                                                        "User-Specified Nominal Capacity [W]",
                                                                        nomHeatingCapUser);
                            }
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatingCap - nomHeatingCapUser) / nomHeatingCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(nomHeatingCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpHeatingCap, 2) +
                                                      " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatingCap = nomHeatingCapUser;
                    }
                }
                if (this->ratedLoadVolFlowHeatWasAutoSized) {
                    this->RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                this->Name,
                                                                "Design Size Load Side Volume Flow Rate [m3/s]",
                                                                tmpLoadSideVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                this->Name,
                                                                "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                                tmpLoadSideVolFlowRate);
                    }
                } else {
                    if (this->RatedLoadVolFlowHeat > 0.0 && tmpLoadSideVolFlowRate > 0.0) {
                        Real64 nomLoadSideVolFlowUser = this->RatedLoadVolFlowHeat;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            if (DataGlobals::DoPlantSizing) {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                        this->Name,
                                                                        "Design Size Load Side Volume Flow Rate [m3/s]",
                                                                        tmpLoadSideVolFlowRate,
                                                                        "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                                        nomLoadSideVolFlowUser);
                            } else {
                                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                        this->Name,
                                                                        "User-Specified Load Side Volume Flow Rate [m3/s]",
                                                                        nomLoadSideVolFlowUser);
                            }
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpLoadSideVolFlowRate - nomLoadSideVolFlowUser) / nomLoadSideVolFlowUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Load Side Volume Flow Rate of " +
                                                      General::RoundSigDigits(nomLoadSideVolFlowUser, 2) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Load Side Volume Flow Rate of " +
                                                      General::RoundSigDigits(tmpLoadSideVolFlowRate, 2) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        this->RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                    this->Name,
                                                                    "Design Size Load Side Volume Flow Rate [m3/s]",
                                                                    tmpLoadSideVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                    this->Name,
                                                                    "Initial Design Size Load Side Volume Flow Rate [m3/s]",
                                                                    tmpLoadSideVolFlowRate);
                        }
                    }
                }
                if (this->ratedCapHeatWasAutoSized && this->RatedCapCool > 0.0) {
                    tmpHeatingCap = this->RatedCapCool;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        this->RatedCapHeat = tmpHeatingCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Design Size Nominal Capacity [W]", tmpHeatingCap);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Initial Design Size Nominal Capacity [W]", tmpHeatingCap);
                        }
                    }
                }

            } else { // no companion heatpump, no plant sizing object
                if ((this->ratedLoadVolFlowHeatWasAutoSized || this->ratedCapHeatWasAutoSized) && DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Water to Water Heat Pump requires a loop Sizing:Plant object.");
                    ShowContinueError("Occurs in HeatPump:WaterToWater:EquationFit:Heating object = " + this->Name);
                    errorsFound = true;
                }
            }

            if (!this->ratedLoadVolFlowHeatWasAutoSized && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "User-Specified Load Side Flow Rate [m3/s]", this->RatedLoadVolFlowHeat);
            }
            if (!this->ratedCapHeatWasAutoSized && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "User-Specified Nominal Capacity [W]", this->RatedCapHeat);
            }
        }
        if (!this->ratedLoadVolFlowHeatWasAutoSized) tmpLoadSideVolFlowRate = this->RatedLoadVolFlowHeat;
        int pltSourceSizNum = DataPlant::PlantLoop(this->SourceLoopNum).PlantSizNum;
        if (pltSourceSizNum > 0) {
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceLoopNum).FluidName,
                                                           DataGlobals::HWInitConvTemp,
                                                           DataPlant::PlantLoop(this->SourceLoopNum).FluidIndex,
                                                           RoutineName);
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SourceLoopNum).FluidName,
                                                               DataGlobals::HWInitConvTemp,
                                                               DataPlant::PlantLoop(this->SourceLoopNum).FluidIndex,
                                                               RoutineName);
            tmpSourceSideVolFlowRate = tmpHeatingCap * (1.0 - (1.0 / this->refCOP)) / (DataSizing::PlantSizData(pltSourceSizNum).DeltaT * Cp * rho);
        } else {
            tmpSourceSideVolFlowRate = tmpLoadSideVolFlowRate; // set source side flow equal to load side flow, assumption
        }
        if (this->ratedSourceVolFlowHeatWasAutoSized) {
            this->RatedSourceVolFlowHeat = tmpSourceSideVolFlowRate;
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                        this->Name,
                                                        "Design Size Source Side Volume Flow Rate [m3/s]",
                                                        tmpSourceSideVolFlowRate);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                        this->Name,
                                                        "Initial Design Size Source Side Volume Flow Rate [m3/s]",
                                                        tmpSourceSideVolFlowRate);
            }
        } else {
            if (this->RatedSourceVolFlowHeat > 0.0 && tmpSourceSideVolFlowRate > 0.0) {
                Real64 nomSourceSideVolFlowUser = this->RatedSourceVolFlowHeat;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (DataGlobals::DoPlantSizing) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                this->Name,
                                                                "Design Size Source Side Volume Flow Rate [m3/s]",
                                                                tmpSourceSideVolFlowRate,
                                                                "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                                nomSourceSideVolFlowUser);
                    } else {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                this->Name,
                                                                "User-Specified Source Side Volume Flow Rate [m3/s]",
                                                                nomSourceSideVolFlowUser);
                    }
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(tmpSourceSideVolFlowRate - nomSourceSideVolFlowUser) / nomSourceSideVolFlowUser) >
                            DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError("User-Specified Source Side Volume Flow Rate of " +
                                              General::RoundSigDigits(nomSourceSideVolFlowUser, 2) + " [m3/s]");
                            ShowContinueError("differs from Design Size Source Side Volume Flow Rate of " +
                                              General::RoundSigDigits(tmpSourceSideVolFlowRate, 2) + " [m3/s]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Design Size Heating Power Consumption [W]", tmpPowerDraw);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    "HeatPump:WaterToWater:EquationFit:Heating", this->Name, "Initial Design Size Heating Power Consumption [W]", tmpPowerDraw);
            }
        } else {
            if (this->RatedPowerHeat > 0.0 && tmpPowerDraw > 0.0) {
                Real64 nomPowerDrawUser = this->RatedPowerHeat;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (DataGlobals::DoPlantSizing) {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                this->Name,
                                                                "Design Size Heating Power Consumption [W]",
                                                                tmpPowerDraw,
                                                                "User-Specified Heating Power Consumption [W]",
                                                                nomPowerDrawUser);
                    } else {
                        ReportSizingManager::ReportSizingOutput("HeatPump:WaterToWater:EquationFit:Heating",
                                                                this->Name,
                                                                "User-Specified Heating Power Consumption [W]",
                                                                nomPowerDrawUser);
                    }
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(tmpPowerDraw - nomPowerDrawUser) / nomPowerDrawUser) > DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError("User-Specified Heating Power Consumption of " + General::RoundSigDigits(nomPowerDrawUser, 2) + " [W]");
                            ShowContinueError("differs from Design Size Heating Power Consumption of " + General::RoundSigDigits(tmpPowerDraw, 2) +
                                              " [W]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "HeatPump:WaterToWater:EquationFit:Heating");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->refCOP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->RatedPowerHeat);
        }
        if (errorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void GshpSpecs::CalcWatertoWaterHPCooling(Real64 const MyLoad)
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
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CelsiustoKelvin(KelvinConv); // Conversion from Celsius to Kelvin
        Real64 const Tref(283.15);                // Reference Temperature for performance curves,10C [K]
        static std::string const RoutineName("CalcWatertoWaterHPCooling");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CoolCapRated;               // Rated Cooling Capacity [W]
        Real64 CoolPowerRated;             // Rated Cooling Power Consumption[W]
        Real64 LoadSideVolFlowRateRated;   // Rated Load Side Volumetric Flow Rate [m3/s]
        Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
        Real64 CoolCapCoeff1;              // 1st coefficient of the cooling capacity performance curve
        Real64 CoolCapCoeff2;              // 2nd coefficient of the cooling capacity performance curve
        Real64 CoolCapCoeff3;              // 3rd coefficient of the cooling capacity performance curve
        Real64 CoolCapCoeff4;              // 4th coefficient of the cooling capacity performance curve
        Real64 CoolCapCoeff5;              // 5th coefficient of the cooling capacity performance curve
        Real64 CoolPowerCoeff1;            // 1st coefficient of the cooling power consumption curve
        Real64 CoolPowerCoeff2;            // 2nd coefficient of the cooling power consumption curve
        Real64 CoolPowerCoeff3;            // 3rd coefficient of the cooling power consumption curve
        Real64 CoolPowerCoeff4;            // 4th coefficient of the cooling power consumption curve
        Real64 CoolPowerCoeff5;            // 5th coefficient of the cooling power consumption curve

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
        CoolCapCoeff1 = this->CoolCap1;
        CoolCapCoeff2 = this->CoolCap2;
        CoolCapCoeff3 = this->CoolCap3;
        CoolCapCoeff4 = this->CoolCap4;
        CoolCapCoeff5 = this->CoolCap5;
        CoolPowerCoeff1 = this->CoolPower1;
        CoolPowerCoeff2 = this->CoolPower2;
        CoolPowerCoeff3 = this->CoolPower3;
        CoolPowerCoeff4 = this->CoolPower4;
        CoolPowerCoeff5 = this->CoolPower5;

        LoadSideMassFlowRate = this->reportLoadSideMassFlowRate;
        LoadSideInletTemp = this->reportLoadSideInletTemp;
        SourceSideMassFlowRate = this->reportSourceSideMassFlowRate;
        SourceSideInletTemp = this->reportSourceSideInletTemp;

        // If heat pump is not operating, THEN return
        if (!this->MustRun) {
            return;
        }

        rhoLoadSide =
            GetDensityGlycol(PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        rhoSourceSide =
            GetDensityGlycol(PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        func1 = ((LoadSideInletTemp + CelsiustoKelvin) / Tref);
        func2 = ((SourceSideInletTemp + CelsiustoKelvin) / Tref);
        func3 = (LoadSideMassFlowRate / (LoadSideVolFlowRateRated * rhoLoadSide));
        func4 = (SourceSideMassFlowRate / (SourceSideVolFlowRateRated * rhoSourceSide));

        QLoad =
            CoolCapRated * (CoolCapCoeff1 + (func1 * CoolCapCoeff2) + (func2 * CoolCapCoeff3) + (func3 * CoolCapCoeff4) + (func4 * CoolCapCoeff5));

        Power = CoolPowerRated *
                (CoolPowerCoeff1 + (func1 * CoolPowerCoeff2) + (func2 * CoolPowerCoeff3) + (func3 * CoolPowerCoeff4) + (func4 * CoolPowerCoeff5));

        if ((QLoad <= 0.0 || Power <= 0.0) && !WarmupFlag) {
            if (QLoad <= 0.0) {
                if (this->CoolCapNegativeCounter < 1) {
                    ++this->CoolCapNegativeCounter;
                    ShowWarningError(HPEqFitCooling + " \"" + this->Name + "\":");
                    ShowContinueError(" Cooling capacity curve output is <= 0.0 (" + TrimSigDigits(QLoad, 4) + ").");
                    ShowContinueError(" Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits(LoadSideInletTemp, 2) +
                                      " C,");
                    ShowContinueError(" a source-side inlet temperature of " + TrimSigDigits(SourceSideInletTemp, 2) + " C,");
                    ShowContinueError(" a load-side mass flow rate of " + TrimSigDigits(LoadSideMassFlowRate, 3) + " kg/s,");
                    ShowContinueError(" and a source-side mass flow rate of " + TrimSigDigits(SourceSideMassFlowRate, 3) + " kg/s.");
                    ShowContinueErrorTimeStamp(" The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(HPEqFitCooling + " \"" + this->Name +
                                                       "\": Cooling capacity curve output is <= 0.0 warning continues...",
                                                   this->CoolCapNegativeIndex,
                                                   QLoad,
                                                   QLoad);
                }
            }
            if (Power <= 0.0) {
                if (this->CoolPowerNegativeCounter < 1) {
                    ++this->CoolPowerNegativeCounter;
                    ShowWarningError(HPEqFitCooling + " \"" + this->Name + "\":");
                    ShowContinueError(" Cooling compressor power curve output is <= 0.0 (" + TrimSigDigits(Power, 4) + ").");
                    ShowContinueError(" Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits(LoadSideInletTemp, 2) +
                                      " C,");
                    ShowContinueError(" a source-side inlet temperature of " + TrimSigDigits(SourceSideInletTemp, 2) + " C,");
                    ShowContinueError(" a load-side mass flow rate of " + TrimSigDigits(LoadSideMassFlowRate, 3) + " kg/s,");
                    ShowContinueError(" and a source-side mass flow rate of " + TrimSigDigits(SourceSideMassFlowRate, 3) + " kg/s.");
                    ShowContinueErrorTimeStamp(" The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(HPEqFitCooling + " \"" + this->Name +
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
            GetSpecificHeatGlycol(PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        CpSourceSide = GetSpecificHeatGlycol(
            PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        LoadSideOutletTemp = LoadSideInletTemp - QLoad / (LoadSideMassFlowRate * CpLoadSide);
        SourceSideOutletTemp = SourceSideInletTemp + QSource / (SourceSideMassFlowRate * CpSourceSide);

        ReportingConstant = TimeStepSys * SecInHour;

        this->reportPower = Power;
        this->reportEnergy = Power * ReportingConstant;
        this->reportQSource = QSource;
        this->reportQLoad = QLoad;
        this->reportQSourceEnergy = QSource * ReportingConstant;
        this->reportQLoadEnergy = QLoad * ReportingConstant;
        this->reportLoadSideOutletTemp = LoadSideOutletTemp;
        this->reportSourceSideOutletTemp = SourceSideOutletTemp;
    }

    void GshpSpecs::CalcWatertoWaterHPHeating(Real64 const MyLoad)
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
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CelsiustoKelvin(KelvinConv); // Conversion from Celsius to Kelvin
        Real64 const Tref(283.15);                // Reference Temperature for performance curves,10C [K]
        static std::string const RoutineName("CalcWatertoWaterHPHeating");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 HeatCapRated;               // Rated Heating Capacity [W]
        Real64 HeatPowerRated;             // Rated Heating Compressor Power[W]
        Real64 LoadSideVolFlowRateRated;   // Rated Load Side Volumetric Flow Rate [m3/s]
        Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
        Real64 HeatCapCoeff1;              // 1st coefficient of the heating capacity performance curve
        Real64 HeatCapCoeff2;              // 2nd coefficient of the heating capacity performance curve
        Real64 HeatCapCoeff3;              // 3rd coefficient of the heating capacity performance curve
        Real64 HeatCapCoeff4;              // 4th coefficient of the heating capacity performance curve
        Real64 HeatCapCoeff5;              // 5th coefficient of the heating capacity performance curve
        Real64 HeatPowerCoeff1;            // 1st coefficient of the heating power consumption curve
        Real64 HeatPowerCoeff2;            // 2nd coefficient of the heating power consumption curve
        Real64 HeatPowerCoeff3;            // 3rd coefficient of the heating power consumption curve
        Real64 HeatPowerCoeff4;            // 4th coefficient of the heating power consumption curve
        Real64 HeatPowerCoeff5;            // 5th coefficient of the heating power consumption curve
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
        HeatCapCoeff1 = this->HeatCap1;
        HeatCapCoeff2 = this->HeatCap2;
        HeatCapCoeff3 = this->HeatCap3;
        HeatCapCoeff4 = this->HeatCap4;
        HeatCapCoeff5 = this->HeatCap5;
        HeatPowerCoeff1 = this->HeatPower1;
        HeatPowerCoeff2 = this->HeatPower2;
        HeatPowerCoeff3 = this->HeatPower3;
        HeatPowerCoeff4 = this->HeatPower4;
        HeatPowerCoeff5 = this->HeatPower5;

        LoadSideMassFlowRate = this->reportLoadSideMassFlowRate;
        LoadSideInletTemp = this->reportLoadSideInletTemp;
        SourceSideMassFlowRate = this->reportSourceSideMassFlowRate;
        SourceSideInletTemp = this->reportSourceSideInletTemp;

        // If heat pump is not operating, THEN return
        if (!this->MustRun) {
            return;
        }
        rhoLoadSide =
            GetDensityGlycol(PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        rhoSourceSide =
            GetDensityGlycol(PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        func1 = ((LoadSideInletTemp + CelsiustoKelvin) / Tref);
        func2 = ((SourceSideInletTemp + CelsiustoKelvin) / Tref);
        func3 = (LoadSideMassFlowRate / (LoadSideVolFlowRateRated * rhoLoadSide));
        func4 = (SourceSideMassFlowRate / (SourceSideVolFlowRateRated * rhoSourceSide));

        QLoad =
            HeatCapRated * (HeatCapCoeff1 + (func1 * HeatCapCoeff2) + (func2 * HeatCapCoeff3) + (func3 * HeatCapCoeff4) + (func4 * HeatCapCoeff5));
        Power = HeatPowerRated *
                (HeatPowerCoeff1 + (func1 * HeatPowerCoeff2) + (func2 * HeatPowerCoeff3) + (func3 * HeatPowerCoeff4) + (func4 * HeatPowerCoeff5));

        if ((QLoad <= 0.0 || Power <= 0.0) && !WarmupFlag) {
            if (QLoad <= 0.0) {
                if (this->HeatCapNegativeCounter < 1) {
                    ++this->HeatCapNegativeCounter;
                    ShowWarningError(HPEqFitHeating + " \"" + this->Name + "\":");
                    ShowContinueError(" Heating capacity curve output is <= 0.0 (" + TrimSigDigits(QLoad, 4) + ").");
                    ShowContinueError(" Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits(LoadSideInletTemp, 2) +
                                      " C,");
                    ShowContinueError(" a source-side inlet temperature of " + TrimSigDigits(SourceSideInletTemp, 2) + " C,");
                    ShowContinueError(" a load-side mass flow rate of " + TrimSigDigits(LoadSideMassFlowRate, 3) + " kg/s,");
                    ShowContinueError(" and a source-side mass flow rate of " + TrimSigDigits(SourceSideMassFlowRate, 3) + " kg/s.");
                    ShowContinueErrorTimeStamp(" The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(HPEqFitHeating + " \"" + this->Name +
                                                       "\": Heating capacity curve output is <= 0.0 warning continues...",
                                                   this->HeatCapNegativeIndex,
                                                   QLoad,
                                                   QLoad);
                }
            }
            if (Power <= 0.0) {
                if (this->HeatPowerNegativeCounter < 1) {
                    ++this->HeatPowerNegativeCounter;
                    ShowWarningError(HPEqFitHeating + " \"" + this->Name + "\":");
                    ShowContinueError(" Heating compressor power curve output is <= 0.0 (" + TrimSigDigits(Power, 4) + ").");
                    ShowContinueError(" Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits(LoadSideInletTemp, 2) +
                                      " C,");
                    ShowContinueError(" a source-side inlet temperature of " + TrimSigDigits(SourceSideInletTemp, 2) + " C,");
                    ShowContinueError(" a load-side mass flow rate of " + TrimSigDigits(LoadSideMassFlowRate, 3) + " kg/s,");
                    ShowContinueError(" and a source-side mass flow rate of " + TrimSigDigits(SourceSideMassFlowRate, 3) + " kg/s.");
                    ShowContinueErrorTimeStamp(" The heat pump is turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(HPEqFitHeating + " \"" + this->Name +
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
            GetSpecificHeatGlycol(PlantLoop(this->LoadLoopNum).FluidName, LoadSideInletTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        CpSourceSide = GetSpecificHeatGlycol(
            PlantLoop(this->SourceLoopNum).FluidName, SourceSideInletTemp, PlantLoop(this->SourceLoopNum).FluidIndex, RoutineName);

        LoadSideOutletTemp = LoadSideInletTemp + QLoad / (LoadSideMassFlowRate * CpLoadSide);
        SourceSideOutletTemp = SourceSideInletTemp - QSource / (SourceSideMassFlowRate * CpSourceSide);

        ReportingConstant = TimeStepSys * SecInHour;

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

} // namespace HeatPumpWaterToWaterSimple

} // namespace EnergyPlus
