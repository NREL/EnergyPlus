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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HeatPumpWaterToWaterHEATING.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterHEATING {
    // Module containing the routines dealing with the Water to Water Heat Pump (Heating)

    // MODULE INFORMATION:
    //       AUTHOR         ARUN
    //       DATE WRITTEN   7/18/2000
    //       MODIFIED       ARUN: 6/27/2002: Cycle Time
    //                      L Lawrie: V1.1.1 (5/20/2003) add meters and energy to several reporting variables
    //                      L Lawrie: V1.1.1 (5/20/2003) restructure modules to comply with standard templates
    //                      B. Griffith, Sept 2010, plant upgrades, generalize fluid properties
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates a water to Water Heat Pump (Heating)

    // METHODOLOGY EMPLOYED:
    // This simulation is based on a set of selected parameters,
    // Which are obtained using Parameter Estimation technique.

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
    using namespace DataLoopNode;

    // MODULE PARAMETER DEFINITIONS
    std::string const ModuleCompName("HeatPump:WaterToWater:ParameterEstimation:Heating");
    std::string const ModuleCompNameUC("HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING");

    std::string GSHPRefrigerant("R22"); // Refrigerent name and index
    int GSHPRefrigIndex(0);

    int NumGSHPs(0);                         // number of Gshps specified in input

    // Object Data
    bool GetWWHPHeatingInput = true;
    Array1D<GshpPeHeatingSpecs> GSHP; // dimension to number of machines

    void clear_state() {
        NumGSHPs = 0;
        GetWWHPHeatingInput = true;
        GSHP.deallocate();
    }

    PlantComponent *GshpPeHeatingSpecs::factory(const std::string& objectName) {
        if (GetWWHPHeatingInput) {
            GetGshpInput();
            GetWWHPHeatingInput = false;
        }
        for (auto &wwhp : GSHP) {
            if (wwhp.Name == objectName) {
                return &wwhp;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(
                "WWHPHeatingFactory: Error getting inputs for heat pump named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }


    void GshpPeHeatingSpecs::simulate(const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad,
                                      bool EP_UNUSED(RunFlag)) {

        // Simulate the model for the Demand "MyLoad"
        if (calledFromLocation.loopNum == this->LoadLoopNum) { // chilled water loop
            this->initialize();
            this->calculate(CurLoad);
            this->update();
        } else if (calledFromLocation.loopNum == this->SourceLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(this->SourceLoopNum,
                                                                this->SourceLoopSideNum,
                                                                DataPlant::TypeOf_HPWaterEFHeating,
                                                                this->SourceSideInletNodeNum,
                                                                this->SourceSideOutletNodeNum,
                                                                -this->QSource,
                                                                this->SourceSideWaterInletTemp,
                                                                this->SourceSideWaterOutletTemp,
                                                                this->SourceSideWaterMassFlowRate,
                                                                FirstHVACIteration);
        } else {
            ShowFatalError("SimHPWatertoWaterHEATING:: Invalid loop connection " + ModuleCompName + ", Requested Unit=" + this->Name);
        }
    }

    void GshpPeHeatingSpecs::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation),
                                                 Real64 &MaxLoad,
                                                 Real64 &MinLoad,
                                                 Real64 &OptLoad) {
        MinLoad = this->NomCap * this->MinPartLoadRat;
        MaxLoad = this->NomCap * this->MaxPartLoadRat;
        OptLoad = this->NomCap * this->OptPartLoadRat;
    }

    void GshpPeHeatingSpecs::onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation)) {
        if (this->plantScanFlag) {
            // Locate the heating on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    DataPlant::TypeOf_HPWaterPEHeating,
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
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    DataPlant::TypeOf_HPWaterPEHeating,
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
            if (errFlag) {
                ShowFatalError("InitGshp: Program terminated due to previous condition(s).");
            }

            PlantUtilities::InterConnectTwoPlantLoopSides(this->LoadLoopNum,
                                                          this->LoadLoopSideNum,
                                                          this->SourceLoopNum,
                                                          this->SourceLoopSideNum,
                                                          this->WWHPPlantTypeOfNum,
                                                          true);
            this->plantScanFlag = false;
        }
    }


#pragma clang diagnostic push
#pragma ide diagnostic ignored "readability-magic-numbers"
    void GetGshpInput()
    {
        //       SUBROUTINE INFORMATION:
        //       AUTHOR:
        //       DATE WRITTEN:    April 1998

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the GSHP models.  As such
        // it will interact with the Input Scanner to retrieve
        // information from the input file, count the number of
        // GSHPs and begin to fill the
        // arrays associated with the type GSHP.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataPlant::TypeOf_HPWaterPEHeating;
        using FluidProperties::FindRefrigerant;
        using NodeInputManager::GetOnlySingleNode;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using PlantUtilities::ScanPlantLoopsForObject;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int GSHPNum;                  // Gshp counter
        int NumAlphas;                // Number of elements in the alpha array
        int NumNums;                  // Number of elements in the numeric array
        int IOStat;                   // IO Status when calling get input subroutine
        Array1D_string AlphArray(5);  // character string data
        Array1D<Real64> NumArray(23); // numeric data

        static bool ErrorsFound(false);

        NumGSHPs = inputProcessor->getNumObjectsFound(ModuleCompName);

        if (NumGSHPs <= 0) {
            ShowSevereError(ModuleCompName + ": No Equipment found");
            ErrorsFound = true;
        }

        // Allocate Arrays
        GSHP.allocate(NumGSHPs);

        for (GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum) {
            inputProcessor->getObjectItem(ModuleCompNameUC, GSHPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat);
            UtilityRoutines::IsNameEmpty(AlphArray(1), ModuleCompNameUC, ErrorsFound);

            GSHP(GSHPNum).Name = AlphArray(1);

            GSHP(GSHPNum).WWHPPlantTypeOfNum = TypeOf_HPWaterPEHeating;

            GSHP(GSHPNum).COP = NumArray(1);
            if (NumArray(1) == 0.0) {
                ShowSevereError(ModuleCompName + ":COP = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            // zero values for NumArray 3 - 6 checked in input - idd
            GSHP(GSHPNum).NomCap = NumArray(2);

            GSHP(GSHPNum).MinPartLoadRat = NumArray(3);

            GSHP(GSHPNum).MaxPartLoadRat = NumArray(4);

            GSHP(GSHPNum).OptPartLoadRat = NumArray(5);

            GSHP(GSHPNum).LoadSideVolFlowRate = NumArray(6);
            if (NumArray(6) == 0.0) {
                ShowSevereError(ModuleCompName + ":Load Side Flow Rate = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).SourceSideVolFlowRate = NumArray(7);
            if (NumArray(7) == 0.0) {
                ShowSevereError(ModuleCompName + ":Source Side Flow Rate = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).LoadSideUACoeff = NumArray(8);
            if (NumArray(8) == 0.0) {
                ShowSevereError(ModuleCompName + ":Load Side Heat Transfer Coeffcient = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).SourceSideUACoeff = NumArray(9);
            if (NumArray(9) == 0.0) {
                ShowSevereError(ModuleCompName + ":Source Side Heat Transfer Coeffcient = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).CompPistonDisp = NumArray(10);
            if (NumArray(10) == 0.0) {
                ShowSevereError(ModuleCompName + ":Compressor Piston displacement/Storke = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).CompClearanceFactor = NumArray(11);
            if (NumArray(11) == 0.0) {
                ShowSevereError(ModuleCompName + ":Compressor Clearance Factor = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).CompSucPressDrop = NumArray(12);
            if (NumArray(12) == 0.0) {
                ShowSevereError(ModuleCompName + ": Pressure Drop = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).SuperheatTemp = NumArray(13);
            if (NumArray(13) == 0.0) {
                ShowSevereError(ModuleCompName + ":Source Side SuperHeat = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).PowerLosses = NumArray(14);
            if (NumArray(14) == 0.0) {
                ShowSevereError(ModuleCompName + ":Compressor Power Loss = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }
            GSHP(GSHPNum).LossFactor = NumArray(15);
            if (NumArray(15) == 0.0) {
                ShowSevereError(ModuleCompName + ":Efficiency = 0.0, Heatpump=" + AlphArray(1));
                ErrorsFound = true;
            }

            GSHP(GSHPNum).HighPressCutoff = NumArray(16);
            if (NumArray(16) == 0.0) {
                GSHP(GSHPNum).HighPressCutoff = 500000000.0;
                // CALL ShowWarningError(ModuleCompName//': High Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
            }

            GSHP(GSHPNum).LowPressCutoff = NumArray(17);
            if (NumArray(17) == 0.0) {
                GSHP(GSHPNum).LowPressCutoff = 0.0;
                // CALL ShowWarningError(ModuleCompName//': Low Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
            }

            GSHP(GSHPNum).SourceSideInletNodeNum = GetOnlySingleNode(
                AlphArray(2), ErrorsFound, ModuleCompName, AlphArray(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            GSHP(GSHPNum).SourceSideOutletNodeNum = GetOnlySingleNode(
                AlphArray(3), ErrorsFound, ModuleCompName, AlphArray(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

            GSHP(GSHPNum).LoadSideInletNodeNum = GetOnlySingleNode(
                AlphArray(4), ErrorsFound, ModuleCompName, AlphArray(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);

            GSHP(GSHPNum).LoadSideOutletNodeNum = GetOnlySingleNode(
                AlphArray(5), ErrorsFound, ModuleCompName, AlphArray(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);

            // Test node sets
            TestCompSet(ModuleCompNameUC, AlphArray(1), AlphArray(2), AlphArray(3), "Condenser Water Nodes");
            TestCompSet(ModuleCompNameUC, AlphArray(1), AlphArray(4), AlphArray(5), "Hot Water Nodes");

            // save the design source side flow rate for use by plant loop sizing algorithms
            RegisterPlantCompDesignFlow(GSHP(GSHPNum).SourceSideInletNodeNum, 0.5 * GSHP(GSHPNum).SourceSideVolFlowRate);

        }

        if (ErrorsFound) {
            ShowFatalError("Errors Found in getting " + ModuleCompNameUC + " Input");
        }

        GSHPRefrigIndex = FindRefrigerant(GSHPRefrigerant);
        if (GSHPRefrigIndex == 0) {
            ShowFatalError("Refrigerant for HeatPump:WaterToWater Heating not found, should have been=" + GSHPRefrigerant);
        }

        // CurrentModuleObject='HeatPump:WaterToWater:ParameterEstimation:Heating'
        for (GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum) {
            SetupOutputVariable("Water to Water Heat Pump Electric Power",
                                OutputProcessor::Unit::W,
                                GSHP(GSHPNum).Power,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Electric Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).Energy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name,
                                _,
                                "Electricity",
                                "Heating",
                                _,
                                "Plant");

            SetupOutputVariable("Water to Water Heat Pump Load Side Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                GSHP(GSHPNum).QLoad,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).QLoadEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name);

            SetupOutputVariable("Water to Water Heat Pump Source Side Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                GSHP(GSHPNum).QSource,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                GSHP(GSHPNum).QSourceEnergy,
                                "System",
                                "Sum",
                                GSHP(GSHPNum).Name);

            SetupOutputVariable("Water to Water Heat Pump Load Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).LoadSideWaterOutletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).LoadSideWaterInletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Outlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).SourceSideWaterOutletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Inlet Temperature",
                                OutputProcessor::Unit::C,
                                GSHP(GSHPNum).SourceSideWaterInletTemp,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Load Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                GSHP(GSHPNum).LoadSideWaterMassFlowRate,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
            SetupOutputVariable("Water to Water Heat Pump Source Side Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                GSHP(GSHPNum).SourceSideWaterMassFlowRate,
                                "System",
                                "Average",
                                GSHP(GSHPNum).Name);
        }
    }
#pragma clang diagnostic pop

    void GshpPeHeatingSpecs::initialize()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    July 2007

        // PURPOSE OF THIS SUBROUTINE:
        // initialization

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitGshp");

        // For each new environment
        if (BeginEnvrnFlag && this->beginEnvironFlag) {
            this->QLoad = 0.0;
            this->QSource = 0.0;
            this->Power = 0.0;
            this->QLoadEnergy = 0.0;
            this->QSourceEnergy = 0.0;
            this->Energy = 0.0;
            this->LoadSideWaterInletTemp = 0.0;
            this->SourceSideWaterInletTemp = 0.0;
            this->LoadSideWaterOutletTemp = 0.0;
            this->SourceSideWaterOutletTemp = 0.0;
            this->SourceSideWaterMassFlowRate = 0.0;
            this->LoadSideWaterMassFlowRate = 0.0;
            this->IsOn = false;
            this->MustRun = true;

            this->beginEnvironFlag = false;
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoadLoopNum).FluidName,
                                                           DataGlobals::CWInitConvTemp,
                                                           DataPlant::PlantLoop(this->LoadLoopNum).FluidIndex,
                                                           RoutineName);
            this->LoadSideDesignMassFlow = this->LoadSideVolFlowRate * rho;

            PlantUtilities::InitComponentNodes(0.0,
                                               this->LoadSideDesignMassFlow,
                                               this->LoadSideInletNodeNum,
                                               this->LoadSideOutletNodeNum,
                                               this->LoadLoopNum,
                                               this->LoadLoopSideNum,
                                               this->LoadBranchNum,
                                               this->LoadCompNum);

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceLoopNum).FluidName,
                                                    DataGlobals::CWInitConvTemp,
                                                    DataPlant::PlantLoop(this->SourceLoopNum).FluidIndex,
                                                    RoutineName);
            this->SourceSideDesignMassFlow = this->SourceSideVolFlowRate * rho;

            PlantUtilities::InitComponentNodes(0.0,
                                               this->SourceSideDesignMassFlow,
                                               this->SourceSideInletNodeNum,
                                               this->SourceSideOutletNodeNum,
                                               this->SourceLoopNum,
                                               this->SourceLoopSideNum,
                                               this->SourceBranchNum,
                                               this->SourceCompNum);
            if (Node(this->SourceSideOutletNodeNum).TempSetPoint == SensedNodeFlagValue)
                Node(this->SourceSideOutletNodeNum).TempSetPoint = 0.0;
            Node(this->SourceSideInletNodeNum).Temp = Node(this->SourceSideOutletNodeNum).TempSetPoint + 30.0;
        }

        if (!BeginEnvrnFlag) this->beginEnvironFlag = true;

        // On every call
        this->Running = 0;
        this->MustRun = true; // Reset MustRun Flag to TRUE
        this->LoadSideWaterMassFlowRate = 0.0;   // Load Side mass flow rate, water side
        this->SourceSideWaterMassFlowRate = 0.0; // Source Side mass flow rate, water side
        this->Power = 0.0;                       // power consumption
        this->QLoad = 0.0;                       // heat rejection from Load Side coil
        this->QSource = 0.0;
    }

    void GshpPeHeatingSpecs::calculate(Real64 &MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       April 1999
        //                      September 2002, SJR
        //       RE-ENGINEERED  Mar2000

        // Using/Aliasing
        using DataHVACGlobals::SysTimeElapsed;
        using namespace FluidProperties;
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataPlant::PlantLoop;
        using General::TrimSigDigits;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const gamma(1.114); // Expnasion Coefficient
        Real64 const HeatBalTol(0.0005);
        Real64 const RelaxParam(0.6);
        Real64 const SmallNum(1.0e-20);
        int const IterationLimit(500);
        static std::string const RoutineName("CalcGshpModel");
        static std::string const RoutineNameLoadSideTemp("CalcGSHPModel:LoadSideTemp");
        static std::string const RoutineNameSourceSideTemp("CalcGSHPModel:SourceSideTemp");
        static std::string const RoutineNameCompressInletTemp("CalcGSHPModel:CompressInletTemp");
        static std::string const RoutineNameSuctionPr("CalcGSHPModel:SuctionPr");
        static std::string const RoutineNameCompSuctionTemp("CalcGSHPModel:CompSuctionTemp");
        static ObjexxFCL::gio::Fmt fmtLD("*");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CompSuctionTemp;
        Real64 CompSuctionEnth;
        Real64 CompSuctionDensity;
        Real64 CompSuctionSatTemp;
        std::string ErrString;
        Real64 DutyFactor;

        static Real64 CurrentSimTime(0.0);
        static Real64 PrevSimTime(0.0);

        // Init Module level Variables
        if (PrevSimTime != CurrentSimTime) {
            PrevSimTime = CurrentSimTime;
        }

        // CALCULATE THE SIMULATION TIME
        Real64 const hoursInDay = 24.0;
        CurrentSimTime = (DayOfSim - 1) * hoursInDay + HourOfDay - 1 + (TimeStep - 1) * TimeStepZone + SysTimeElapsed;

        if (MyLoad > 0.0) {
            this->MustRun = true;
            this->IsOn = true;
        } else {
            this->MustRun = false;
            this->IsOn = false;
        }
        this->LoadSideWaterInletTemp = Node(this->LoadSideInletNodeNum).Temp;
        this->SourceSideWaterInletTemp = Node(this->SourceSideInletNodeNum).Temp;

        //*******Set flow based on "run" flags**********
        // Set flows if the heat pump is not running
        if (!this->MustRun) {
            this->LoadSideWaterMassFlowRate = 0.0;
            SetComponentFlowRate(this->LoadSideWaterMassFlowRate,
                                 this->LoadSideInletNodeNum,
                                 this->LoadSideOutletNodeNum,
                                 this->LoadLoopNum,
                                 this->LoadLoopSideNum,
                                 this->LoadBranchNum,
                                 this->LoadCompNum);
            this->SourceSideWaterMassFlowRate = 0.0;
            SetComponentFlowRate(this->SourceSideWaterMassFlowRate,
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
                                                        this->SourceSideWaterMassFlowRate);
            // now initialize simulation variables for "heat pump off"
            this->LoadSideWaterOutletTemp = this->LoadSideWaterInletTemp;
            this->SourceSideWaterOutletTemp = this->SourceSideWaterInletTemp;
            return; // if heat pump is not running return without simulation, power, Q already zeroed in init
        } else { // the heat pump must run, request design flow

            this->LoadSideWaterMassFlowRate = this->LoadSideDesignMassFlow;
            SetComponentFlowRate(this->LoadSideWaterMassFlowRate,
                                 this->LoadSideInletNodeNum,
                                 this->LoadSideOutletNodeNum,
                                 this->LoadLoopNum,
                                 this->LoadLoopSideNum,
                                 this->LoadBranchNum,
                                 this->LoadCompNum);

            this->SourceSideWaterMassFlowRate = this->SourceSideDesignMassFlow;
            SetComponentFlowRate(this->SourceSideWaterMassFlowRate,
                                 this->SourceSideInletNodeNum,
                                 this->SourceSideOutletNodeNum,
                                 this->SourceLoopNum,
                                 this->SourceLoopSideNum,
                                 this->SourceBranchNum,
                                 this->SourceCompNum);
            // if there's no flow, turn the "heat pump off"
            if (this->LoadSideWaterMassFlowRate < MassFlowTolerance || this->SourceSideWaterMassFlowRate < MassFlowTolerance) {
                this->LoadSideWaterMassFlowRate = 0.0;
                SetComponentFlowRate(this->LoadSideWaterMassFlowRate,
                                     this->LoadSideInletNodeNum,
                                     this->LoadSideOutletNodeNum,
                                     this->LoadLoopNum,
                                     this->LoadLoopSideNum,
                                     this->LoadBranchNum,
                                     this->LoadCompNum);
                this->SourceSideWaterMassFlowRate = 0.0;
                SetComponentFlowRate(this->SourceSideWaterMassFlowRate,
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
                                                            this->SourceSideWaterMassFlowRate);
                this->LoadSideWaterOutletTemp = this->LoadSideWaterInletTemp;
                this->SourceSideWaterOutletTemp = this->SourceSideWaterInletTemp;
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
                                                        this->SourceSideWaterMassFlowRate);
        }

        //***********BEGIN CALCULATION****************
        // initialize the source and load side heat transfer rates for the simulation
        Real64 initialQSource = 0.0;
        Real64 initialQLoad = 0.0;
        int IterationCount = 0;

        Real64 CpSourceSide = GetSpecificHeatGlycol(PlantLoop(this->SourceLoopNum).FluidName,
                                                    this->SourceSideWaterInletTemp,
                                                    PlantLoop(this->SourceLoopNum).FluidIndex,
                                                    RoutineName);

        Real64 CpLoadSide = GetSpecificHeatGlycol(
            PlantLoop(this->LoadLoopNum).FluidName, this->LoadSideWaterInletTemp, PlantLoop(this->LoadLoopNum).FluidIndex, RoutineName);

        // Determine effectiveness of Source Side (the Evaporator in heating mode)
        Real64 SourceSideEffect = 1.0 - std::exp(-this->SourceSideUACoeff / (CpSourceSide * this->SourceSideWaterMassFlowRate));
        // Determine effectiveness of Load Side the condenser in heating mode
        Real64 LoadSideEffect = 1.0 - std::exp(-this->LoadSideUACoeff / (CpLoadSide * this->LoadSideWaterMassFlowRate));

        while (true) { // main loop to solve model equations
            ++IterationCount;
            // Determine Source Side temperature
            Real64 SourceSideTemp = this->SourceSideWaterInletTemp - initialQSource / (SourceSideEffect * CpSourceSide * this->SourceSideWaterMassFlowRate);

            // To determine Load Side temperature condenser
            Real64 LoadSideTemp = this->LoadSideWaterInletTemp + initialQLoad / (LoadSideEffect * CpLoadSide * this->LoadSideWaterMassFlowRate);

            // Determine the evaporating and condensing pressures
            Real64 SourceSidePressure = GetSatPressureRefrig(GSHPRefrigerant, SourceSideTemp, GSHPRefrigIndex, RoutineNameSourceSideTemp);
            Real64 LoadSidePressure = GetSatPressureRefrig(GSHPRefrigerant, LoadSideTemp, GSHPRefrigIndex, RoutineNameLoadSideTemp);

            // check cutoff pressures
            if (SourceSidePressure < this->LowPressCutoff) {
                ShowSevereError(ModuleCompName + "=\"" + this->Name + "\" Heating Source Side Pressure Less than the Design Minimum");
                ShowContinueError("Source Side Pressure=" + TrimSigDigits(SourceSidePressure, 2) +
                                  " and user specified Design Minimum Pressure=" + TrimSigDigits(this->LowPressCutoff, 2));
                ShowFatalError("Preceding Conditions cause termination.");
            }
            if (LoadSidePressure > this->HighPressCutoff) {
                ShowSevereError(ModuleCompName + "=\"" + this->Name + "\" Heating Load Side Pressure greater than the Design Maximum");
                ShowContinueError("Load Side Pressure=" + TrimSigDigits(LoadSidePressure, 2) +
                                  " and user specified Design Maximum Pressure=" + TrimSigDigits(this->HighPressCutoff, 2));
                ShowFatalError("Preceding Conditions cause termination.");
            }

            // Determine Suction Pressure at compressor inlet
            Real64 SuctionPr = SourceSidePressure - this->CompSucPressDrop;
            // Determine Discharge Pressure at compressor exit
            Real64 DischargePr = LoadSidePressure + this->CompSucPressDrop;
            // check cutoff pressures
            if (SuctionPr < this->LowPressCutoff) {
                ShowSevereError(ModuleCompName + "=\"" + this->Name + "\" Heating Suction Pressure Less than the Design Minimum");
                ShowContinueError("Heating Suction Pressure=" + TrimSigDigits(SuctionPr, 2) +
                                  " and user specified Design Minimum Pressure=" + TrimSigDigits(this->LowPressCutoff, 2));
                ShowFatalError("Preceding Conditions cause termination.");
            }
            if (DischargePr > this->HighPressCutoff) {
                ShowSevereError(ModuleCompName + "=\"" + this->Name + "\" Heating Discharge Pressure greater than the Design Maximum");
                ShowContinueError("Heating Discharge Pressure=" + TrimSigDigits(DischargePr, 2) +
                                  " and user specified Design Maximum Pressure=" + TrimSigDigits(this->HighPressCutoff, 2));
                ShowFatalError("Preceding Conditions cause termination.");
            }

            // Determine the Source Side Outlet Enthalpy
            Real64 qualOne = 1.0;
            Real64 SourceSideOutletEnth = GetSatEnthalpyRefrig(GSHPRefrigerant, SourceSideTemp, qualOne, GSHPRefrigIndex, RoutineNameSourceSideTemp);

            // Determine Load Side Outlet Enthalpy
            Real64 qualZero = 0.0;
            Real64 LoadSideOutletEnth = GetSatEnthalpyRefrig(GSHPRefrigerant, LoadSideTemp, qualZero, GSHPRefrigIndex, RoutineNameLoadSideTemp);

            // Determine superheated temperature of the Source Side outlet/compressor inlet
            Real64 CompressInletTemp = SourceSideTemp + this->SuperheatTemp;
            // Determine the enathalpy of the super heated fluid at Source Side outlet
            Real64 SuperHeatEnth =
                GetSupHeatEnthalpyRefrig(GSHPRefrigerant, CompressInletTemp, SourceSidePressure, GSHPRefrigIndex, RoutineNameCompressInletTemp);

            // Determining the suction state of the fluid from inlet state involves interation
            // Method employed...
            // Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
            // check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

            CompSuctionSatTemp = GetSatTemperatureRefrig(GSHPRefrigerant, SuctionPr, GSHPRefrigIndex, RoutineNameSuctionPr);

            Real64 T110 = CompSuctionSatTemp;
            // Shoot into the super heated region
            Real64 T111 = CompSuctionSatTemp + 80;

            // Iterate to find the Suction State - given suction pressure and superheat enthalpy
            while (true) {
                CompSuctionTemp = 0.5 * (T110 + T111);

                CompSuctionEnth = GetSupHeatEnthalpyRefrig(GSHPRefrigerant, CompSuctionTemp, SuctionPr, GSHPRefrigIndex, RoutineNameCompSuctionTemp);
                if (std::abs(CompSuctionEnth - SuperHeatEnth) / SuperHeatEnth < 0.0001) {
                    goto LOOP_exit;
                }

                if (CompSuctionEnth < SuperHeatEnth) {
                    T110 = CompSuctionTemp;
                } else {
                    T111 = CompSuctionTemp;
                }
            }
        LOOP_exit:;

            // Determine the Mass flow rate of refrigerant
            CompSuctionDensity = GetSupHeatDensityRefrig(GSHPRefrigerant, CompSuctionTemp, SuctionPr, GSHPRefrigIndex, RoutineNameCompSuctionTemp);
            Real64 MassRef = this->CompPistonDisp * CompSuctionDensity * (1.0 + this->CompClearanceFactor - this->CompClearanceFactor * std::pow(DischargePr / SuctionPr, 1.0 / gamma));

            // Find the  Source Side Heat Transfer
            this->QSource = MassRef * (SourceSideOutletEnth - LoadSideOutletEnth);

            // Determine the theoretical power
            this->Power = this->PowerLosses + (MassRef * gamma / (gamma - 1) * SuctionPr / CompSuctionDensity / this->LossFactor *
                                   (std::pow(DischargePr / SuctionPr, (gamma - 1) / gamma) - 1));

            // Determine the Loadside HeatRate (QLoad)
            this->QLoad = this->Power + this->QSource;

            // convergence and iteration limit check
            if (std::abs((this->QLoad - initialQLoad) / (initialQLoad + SmallNum)) < HeatBalTol || IterationCount > IterationLimit) {
                if (IterationCount > IterationLimit) {
                    ShowWarningError(ModuleCompName + " did not converge");
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Heatpump Name = " + this->Name);
                    ObjexxFCL::gio::write(ErrString, fmtLD) << std::abs(100.0 * (this->QLoad - initialQLoad) / (initialQLoad + SmallNum));
                    ShowContinueError("Heat Inbalance (%)             = " + stripped(ErrString));
                    ObjexxFCL::gio::write(ErrString, fmtLD) << this->QLoad;
                    ShowContinueError("Load-side heat transfer rate   = " + stripped(ErrString));
                    ObjexxFCL::gio::write(ErrString, fmtLD) << this->QSource;
                    ShowContinueError("Source-side heat transfer rate = " + stripped(ErrString));
                    ObjexxFCL::gio::write(ErrString, fmtLD) << this->SourceSideWaterMassFlowRate;
                    ShowContinueError("Source-side mass flow rate     = " + stripped(ErrString));
                    ObjexxFCL::gio::write(ErrString, fmtLD) << this->LoadSideWaterMassFlowRate;
                    ShowContinueError("Load-side mass flow rate       = " + stripped(ErrString));
                    ObjexxFCL::gio::write(ErrString, fmtLD) << this->SourceSideWaterInletTemp;
                    ShowContinueError("Source-side inlet temperature  = " + stripped(ErrString));
                    ObjexxFCL::gio::write(ErrString, fmtLD) << this->LoadSideWaterInletTemp;
                    ShowContinueError("Load-side inlet temperature    = " + stripped(ErrString));
                }
                goto LOOPLoadEnth_exit;

            } else { // update load
                initialQLoad += RelaxParam * (this->QLoad - initialQLoad);
                initialQSource += RelaxParam * (this->QSource - initialQSource);
            }
        }
    LOOPLoadEnth_exit:;

        // Control Strategy
        if (std::abs(MyLoad) < this->QLoad) {
            DutyFactor = std::abs(MyLoad) / this->QLoad;
            this->QLoad = std::abs(MyLoad);
            this->Power *= DutyFactor;
            this->QSource *= DutyFactor;

            // Determine the Exterior fluid temperature at the Load Side oulet and eveporator outlet...
            // Refrigerant = "Steam"
            this->LoadSideWaterOutletTemp = this->LoadSideWaterInletTemp + this->QLoad / (this->LoadSideWaterMassFlowRate * CpLoadSide);
            this->SourceSideWaterOutletTemp = this->SourceSideWaterInletTemp - this->QSource / (this->SourceSideWaterMassFlowRate * CpSourceSide);
            return;
        }

        this->LoadSideWaterOutletTemp = this->LoadSideWaterInletTemp + this->QLoad / (this->LoadSideWaterMassFlowRate * CpLoadSide);
        this->SourceSideWaterOutletTemp = this->SourceSideWaterInletTemp - this->QSource / (this->SourceSideWaterMassFlowRate * CpSourceSide);
        // REPORT VAR
        this->Running = 1;
    }

    void GshpPeHeatingSpecs::update()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        if (!this->MustRun) {
            // set node temperatures
            Node(this->SourceSideOutletNodeNum).Temp = Node(this->SourceSideInletNodeNum).Temp;
            Node(this->LoadSideOutletNodeNum).Temp = Node(this->LoadSideInletNodeNum).Temp;
            this->Power = 0.0;
            this->Energy = 0.0;
            this->QSource = 0.0;
            this->QLoad = 0.0;
            this->QSourceEnergy = 0.0;
            this->QLoadEnergy = 0.0;
            this->SourceSideWaterInletTemp = Node(this->SourceSideInletNodeNum).Temp;
            this->SourceSideWaterOutletTemp = Node(this->SourceSideOutletNodeNum).Temp;
            this->LoadSideWaterInletTemp = Node(this->LoadSideInletNodeNum).Temp;
            this->LoadSideWaterOutletTemp = Node(this->LoadSideOutletNodeNum).Temp;

        } else {
            // set node temperatures
            Node(this->LoadSideOutletNodeNum).Temp = this->LoadSideWaterOutletTemp;
            Node(this->SourceSideOutletNodeNum).Temp = this->SourceSideWaterOutletTemp;

            // set node flow rates;  for these load based models
            // assume that the sufficient Source Side flow rate available

            Real64 const ReportingConstant = DataHVACGlobals::TimeStepSys * SecInHour;

            this->Energy = this->Power * ReportingConstant;
            this->QSourceEnergy = QSource * ReportingConstant;
            this->QLoadEnergy = QLoad * ReportingConstant;
        }

    }

} // namespace HeatPumpWaterToWaterHEATING

} // namespace EnergyPlus
