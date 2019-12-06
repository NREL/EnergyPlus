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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PondGroundHeatExchanger.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace PondGroundHeatExchanger {

    // Module containing the routines dealing with pond ground heat exchangers

    // MODULE INFORMATION:
    //       AUTHOR         Simon Rees
    //       DATE WRITTEN   September 2002
    //       MODIFIED       Brent Griffith Sept 2010, plant upgrades
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This model represents a shallow pond with submerged hydronic tubes through
    // which the heat transfer fluid is circulated. The model represents a 'shallow'
    // pond in that no attempt is made to model any stratification effects that may
    // be present in deeper ponds. This type of heat rejector is intended to be
    // connected in a condenser loop, with or without other forms of heat rejector.
    // The pond model is a 'lumped parameter' model where the pond is represented
    // by a single node with thermal mass. The pond surface temperature is the same
    // as the temperature at this node, i.e. the surface temperature is the same as
    // the bulk temperature. A first order differential equation is solved in the
    // model to calculated the pond temperature at each time step. This type of heat
    // rejector is modelled as several circuits connected in parallel.

    // METHODOLOGY EMPLOYED:
    // A heat balance is calculated at a single node that represents the pond.
    // heat transfer takes palce by surface convection, long-wave radiation to the
    // sky, absoption of solar energy, ground heat transfer and heat exchange with
    // the fluid. A heat exchanger analogy is used to calculate the heat transfer
    // between the heat transfer fluid and the pond. The differential equation
    // defined by the heat balance is solved using a fourth order Runge-Kutta
    // numerical integration method.

    // REFERENCES:
    // Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
    //   M.S. Thesis, Oklahoma State University, December 1999.
    // Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
    //   Simulating The Performance Of A Shallow Pond As A Supplemental Heat Rejecter
    //   With Closed-Loop Ground-Source Heat Pump Systems.
    //   ASHRAE Transactions.  106(2):107-121.

    static std::string const BlankString;
    static std::string const fluidNameWater("WATER");
    Real64 const SmallNum(1.0e-30);         // Very small number to avoid div0 errors
    Real64 const StefBoltzmann(5.6697e-08); // Stefan-Boltzmann constant

    int NumOfPondGHEs(0); // Number of pond ground heat exchangers

    bool GetInputFlag(true);

    Array1D<PondGroundHeatExchangerData> PondGHE;

    void PondGroundHeatExchangerData::simulate(const PlantLocation &EP_UNUSED(calledFromLocation),
                                               bool const FirstHVACIteration,
                                               Real64 &EP_UNUSED(CurLoad),
                                               bool const EP_UNUSED(RunFlag))
    {
        this->InitPondGroundHeatExchanger(FirstHVACIteration);
        this->CalcPondGroundHeatExchanger();
        this->UpdatePondGroundHeatExchanger();
        this->ReportPondGroundHeatExchanger();
    }

    PlantComponent *PondGroundHeatExchangerData::factory(int const EP_UNUSED(objectType), std::string const objectName)
    {
        if (GetInputFlag) {
            GetPondGroundHeatExchanger();
            GetInputFlag = false;
        }
        for (auto &ghx : PondGHE) {
            if (ghx.Name == objectName) {
                return &ghx;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("Pond Heat Exchanger Factory: Error getting inputs for GHX named: " + objectName);
        // Shut up the compiler
        return nullptr;
    }

    void PondGroundHeatExchangerData::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation),
                                                          Real64 &MaxLoad,
                                                          Real64 &MinLoad,
                                                          Real64 &OptLoad)
    {
        this->InitPondGroundHeatExchanger(true);
        MaxLoad = this->DesignCapacity;
        MinLoad = 0.0;
        OptLoad = this->DesignCapacity;
    }

    void GetPondGroundHeatExchanger()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads the input for hydronic Pond Ground Heat Exchangers
        // from the user input file.  This will contain all of the information
        // needed to define and simulate the pond.

        static bool ErrorsFound(false); // Set to true if errors in input,

        int IOStatus;   // Used in GetObjectItem
        int Item;       // Item to be "gotten"
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call

        // Initializations and allocations
        DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:Pond";
        NumOfPondGHEs = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
        // allocate data structures
        if (allocated(PondGHE)) PondGHE.deallocate();

        PondGHE.allocate(NumOfPondGHEs);

        // Obtain all of the user data related to the ponds...
        for (Item = 1; Item <= NumOfPondGHEs; ++Item) {

            // get the input data
            inputProcessor->getObjectItem(
                DataIPShortCuts::cCurrentModuleObject, Item, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNumbers, IOStatus, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);

            PondGHE(Item).WaterIndex = FluidProperties::FindGlycol(fluidNameWater);

            // General user input data
            PondGHE(Item).Name = DataIPShortCuts::cAlphaArgs(1);

            // get inlet node data
            PondGHE(Item).InletNode = DataIPShortCuts::cAlphaArgs(2);
            PondGHE(Item).InletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            if (PondGHE(Item).InletNodeNum == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            // get outlet node data
            PondGHE(Item).OutletNode = DataIPShortCuts::cAlphaArgs(3);
            PondGHE(Item).OutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            if (PondGHE(Item).OutletNodeNum == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + DataIPShortCuts::cAlphaArgs(3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(2), DataIPShortCuts::cAlphaArgs(3), "Condenser Water Nodes");

            // pond geometry data
            PondGHE(Item).Depth = DataIPShortCuts::rNumericArgs(1);
            PondGHE(Item).Area = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(1) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }
            if (DataIPShortCuts::rNumericArgs(2) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }

            // tube data
            PondGHE(Item).TubeInDiameter = DataIPShortCuts::rNumericArgs(3);
            PondGHE(Item).TubeOutDiameter = DataIPShortCuts::rNumericArgs(4);

            if (DataIPShortCuts::rNumericArgs(3) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(3) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(3), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }
            if (DataIPShortCuts::rNumericArgs(4) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(4) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(4), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }
            if (DataIPShortCuts::rNumericArgs(3) > DataIPShortCuts::rNumericArgs(4)) { // error
                ShowSevereError("For " + DataIPShortCuts::cCurrentModuleObject + ": " + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(3) + " [" + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(3), 2) + "] > " + DataIPShortCuts::cNumericFieldNames(4) + " [" +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(4), 2) + ']');
                ErrorsFound = true;
            }

            // thermal conductivity data
            PondGHE(Item).TubeConductivity = DataIPShortCuts::rNumericArgs(5);
            PondGHE(Item).GrndConductivity = DataIPShortCuts::rNumericArgs(6);

            if (DataIPShortCuts::rNumericArgs(5) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(5) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(5), 4));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }
            if (DataIPShortCuts::rNumericArgs(6) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(6) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(6), 4));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }

            // circuits
            PondGHE(Item).NumCircuits = DataIPShortCuts::rNumericArgs(7);

            if (DataIPShortCuts::rNumericArgs(7) <= 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(7) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(7), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }
            PondGHE(Item).CircuitLength = DataIPShortCuts::rNumericArgs(8);
            if (DataIPShortCuts::rNumericArgs(8) <= 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(8) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(8), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value must be greater than 0.0");
                ErrorsFound = true;
            }

        } // end of input loop

        // final error check
        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }

        // Set up the output variables
        for (Item = 1; Item <= NumOfPondGHEs; ++Item) {
            SetupOutputVariable("Pond Heat Exchanger Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                PondGHE(Item).HeatTransferRate,
                                "Plant",
                                "Average",
                                PondGHE(Item).Name);
            SetupOutputVariable(
                "Pond Heat Exchanger Heat Transfer Energy", OutputProcessor::Unit::J, PondGHE(Item).Energy, "Plant", "Sum", PondGHE(Item).Name);
            SetupOutputVariable("Pond Heat Exchanger Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                PondGHE(Item).MassFlowRate,
                                "Plant",
                                "Average",
                                PondGHE(Item).Name);
            SetupOutputVariable(
                "Pond Heat Exchanger Inlet Temperature", OutputProcessor::Unit::C, PondGHE(Item).InletTemp, "Plant", "Average", PondGHE(Item).Name);
            SetupOutputVariable(
                "Pond Heat Exchanger Outlet Temperature", OutputProcessor::Unit::C, PondGHE(Item).OutletTemp, "Plant", "Average", PondGHE(Item).Name);
            SetupOutputVariable(
                "Pond Heat Exchanger Bulk Temperature", OutputProcessor::Unit::C, PondGHE(Item).PondTemp, "Plant", "Average", PondGHE(Item).Name);
        }

        if (!DataEnvironment::GroundTemp_DeepObjInput) {
            ShowWarningError("GetPondGroundHeatExchanger:  No \"Site:GroundTemperature:Deep\" were input.");
            ShowContinueError("Defaults, constant throughout the year of (" + General::RoundSigDigits(DataEnvironment::GroundTemp_Deep, 1) + ") will be used.");
        }
    }

    void PondGroundHeatExchangerData::InitPondGroundHeatExchanger(bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine Resets the elements of the data structure as necessary
        // at the first HVAC iteration of each time step.

        // METHODOLOGY EMPLOYED:
        // One of the things done here is to update the record of the past pond
        // temperature. This is needed in order to solve the diff. eqn. to find
        // the temperature at the end of the next time step.
        // Also set module variables to data structure for this pond. Set flow rate
        // from node data and hypothetical design flow.

        Real64 const DesignVelocity(0.5); // Hypothetical design max pipe velocity [m/s]
        Real64 const PondHeight(0.0);     // for now
        static std::string const RoutineName("InitPondGroundHeatExchanger");

        Real64 DesignFlow; // Hypothetical design flow rate

        Real64 rho;
        Real64 Cp;
        bool errFlag;
        // repeated warm up days tend to drive the initial pond temperature toward the drybulb temperature
        // For each environment start the pond midway between drybulb and ground temp.

        if (this->OneTimeFlag || DataGlobals::WarmupFlag) {
            // initialize pond temps to mean of drybulb and ground temps.
            this->BulkTemperature = this->PastBulkTemperature = 0.5 * (DataEnvironment::OutDryBulbTempAt(PondHeight) + DataEnvironment::GroundTemp_Deep);
            this->OneTimeFlag = false;
        }

        // Init more variables
        if (this->MyFlag) {
            // Locate the hx on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(
                this->Name, DataPlant::TypeOf_GrndHtExchgPond, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, errFlag, _, _, _, _, _);
            if (errFlag) {
                ShowFatalError("InitPondGroundHeatExchanger: Program terminated due to previous condition(s).");
            }
            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, DataPrecisionGlobals::constant_zero, DataPlant::PlantLoop(this->LoopNum).FluidIndex, RoutineName);
            Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, DataPrecisionGlobals::constant_zero, DataPlant::PlantLoop(this->LoopNum).FluidIndex, RoutineName);
            this->DesignMassFlowRate = DataGlobals::Pi / 4.0 * pow_2(this->TubeInDiameter) * DesignVelocity * rho * this->NumCircuits;
            this->DesignCapacity = this->DesignMassFlowRate * Cp * 10.0; // assume 10C delta T?
            PlantUtilities::InitComponentNodes(0.0,
                               this->DesignMassFlowRate,
                               this->InletNodeNum,
                               this->OutletNodeNum,
                               this->LoopNum,
                               this->LoopSideNum,
                               this->BranchNum,
                               this->CompNum);
            PlantUtilities::RegisterPlantCompDesignFlow(this->InletNodeNum, this->DesignMassFlowRate / rho);

            this->MyFlag = false;
        }

        // check if we are in very first call for this zone time step
        if (DataGlobals::BeginTimeStepFlag && FirstHVACIteration && DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == 1) { // DSU
            // update past temperature
            this->PastBulkTemperature = this->BulkTemperature;
        }

        this->InletTemp = DataLoopNode::Node(InletNodeNum).Temp;
        OutletTemp = DataLoopNode::Node(OutletNodeNum).Temp;
        PondTemp = this->BulkTemperature;

        DesignFlow = PlantUtilities::RegulateCondenserCompFlowReqOp(this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, this->DesignMassFlowRate);

        PlantUtilities::SetComponentFlowRate(DesignFlow, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum);

        // get the current flow rate - module variable
        this->MassFlowRate = DataLoopNode::Node(InletNodeNum).MassFlowRate;
    }

    //==============================================================================

    void PondGroundHeatExchangerData::CalcPondGroundHeatExchanger()
    {

        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does all of the stuff that is necessary to simulate
        // a pond ground heat exchanger.  Calls are made to appropriate subroutines
        // either in this module or outside of it.

        // METHODOLOGY EMPLOYED:
        // The differential equation defined by the heat balance is solved using
        // a fourth order Runge-Kutta numerical integration method. The differential
        // equation is:
        //            Mdot*Cp*dT/dt = Sum of fluxes.

        // REFERENCES:
        // Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
        //   M.S. Thesis, Oklahoma State University, December 1999.
        // Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
        //   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
        //   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
        //   ASHRAE Transactions.  106(2):107-121.

        static std::string const RoutineName("CalcPondGroundHeatExchanger");

        Real64 PondTempStar;
        Real64 PondTempStarStar;
        Real64 PondTempStarStarStar;
        Real64 Flux;
        Real64 FluxStar;
        Real64 FluxStarStar;
        Real64 NewPondTemp;
        Real64 SpecificHeat;
        Real64 PondMass;

        PondMass = this->Depth * this->Area * FluidProperties::GetDensityGlycol(fluidNameWater, max(PondTemp, DataPrecisionGlobals::constant_zero), this->WaterIndex, RoutineName);

        SpecificHeat = FluidProperties::GetSpecificHeatGlycol(fluidNameWater,
                                             max(PondTemp, DataPrecisionGlobals::constant_zero),
                                             this->WaterIndex,
                                             RoutineName); // DSU bug fix here, was using working fluid index

        Flux = this->CalcTotalFLux(PondTemp);
        PondTempStar = this->PastBulkTemperature + 0.5 * DataGlobals::SecInHour * DataHVACGlobals::TimeStepSys * Flux / (SpecificHeat * PondMass);

        FluxStar = this->CalcTotalFLux(PondTempStar);
        PondTempStarStar = this->PastBulkTemperature + 0.5 * DataGlobals::SecInHour * DataHVACGlobals::TimeStepSys * FluxStar / (SpecificHeat * PondMass);

        FluxStarStar = this->CalcTotalFLux(PondTempStarStar);
        PondTempStarStarStar = this->PastBulkTemperature + DataGlobals::SecInHour * DataHVACGlobals::TimeStepSys * FluxStarStar / (SpecificHeat * PondMass);

        NewPondTemp = this->PastBulkTemperature + DataGlobals::SecInHour * DataHVACGlobals::TimeStepSys *
                                                      (Flux + 2.0 * FluxStar + 2.0 * FluxStarStar + this->CalcTotalFLux(PondTempStarStarStar)) /
                                                      (6.0 * SpecificHeat * PondMass);

        PondTemp = NewPondTemp;
    }

    Real64 PondGroundHeatExchangerData::CalcTotalFLux(Real64 const PondBulkTemp // pond temp for this flux calculation
    )
    {

        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Thic calculates the summation of the heat fluxes on the pond for a
        // given pond temperature. The following heat fluxes are calculated:
        //   convection,
        //   long-wave radiation,
        //   solar gain,
        //   evaporation,
        //   ground conduction,
        //   along with heat exchange with the fluid

        // METHODOLOGY EMPLOYED:
        // Convection is calculated with the ASHRAE simple convection coefficients.
        // Evaporation is calculated assuming a fixed Lewis number - not as in
        // Chaisson model. Heat transfer with the fluid is calculated using a heat
        // exchanger Effectiveness-NTU method, where the pond is seen as a static
        // fluid - this is also different from Chaisson's original model (assumed
        // pond at average of inlet and outlet temps).

        // REFERENCES:
        // Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
        //   M.S. Thesis, Oklahoma State University, December 1999.
        // Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
        //   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
        //   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
        //   ASHRAE Transactions.  106(2):107-121.
        // Hull, J.R., K.V. Liu, W.T. Sha, J. Kamal, and C.E. Nielsen, 1984.
        //   Dependence of Ground Heat Losses Upon Solar Pond Size and Perimeter
        //   Insulation Calculated and Experimental Results. Solar Energy,33(1):25-33.

        Real64 CalcTotalFLux; // function return variable

        Real64 const PrantlAir(0.71); // Prantl number for air - assumed constant
        Real64 const SchmidtAir(0.6); // Schmidt number for air - assumed constant
        Real64 const PondHeight(0.0); // for now
        static std::string const RoutineName("PondGroundHeatExchanger:CalcTotalFlux");

        Real64 ConvCoef;        // convection coefficient
        Real64 ExternalTemp;    // external environmental temp - drybulb or wetbulb
        Real64 FluxSolAbsorbed; // absorbed solar flux
        Real64 FluxLongwave;    // absorbed longwave flux
        Real64 FluxConvect;     // convective flux
        Real64 FluxEvap;        // evaporative heat flux
        Real64 FluxGround;      // ground heat transfer flux
        Real64 Qfluid;          // convective flux
        Real64 SurfTempAbs;     // absolute value of surface temp
        Real64 SkyTempAbs;      // absolute value of sky temp
        Real64 ThermalAbs;      // thermal absorptivity
        Real64 SpecHeat;        // specific heat capacity
        Real64 HumRatioFilm;    // humidity ratio at pond surface/film temperature
        Real64 HumRatioAir;     // humidity ratio of air
        Real64 SpecHeatAir;     // air specific heat
        Real64 LatentHeatAir;   // latent heat of air
        Real64 UvalueGround;    // ground heat transfer coefficient
        Real64 Perimeter;       // pond perimeter
        Real64 OutDryBulb;      // drybulb at pond height
        Real64 OutWetBulb;      // wetbulb at pond height

        // make a surface heat balance and solve for temperature
        ThermalAbs = 0.9;

        // set appropriate external temp
        // use height dependency --  if there was a height for this unit, it could be inserted.
        // parameter PondHeight=0.0 is used.
        OutDryBulb = DataEnvironment::OutDryBulbTempAt(PondHeight);
        OutWetBulb = DataEnvironment::OutWetBulbTempAt(PondHeight);
        if (DataEnvironment::IsSnow || DataEnvironment::IsRain) {
            ExternalTemp = OutWetBulb;
        } else { // normal dry conditions
            ExternalTemp = OutDryBulb;
        }

        // absolute temperatures
        SurfTempAbs = PondBulkTemp + DataGlobals::KelvinConv;
        SkyTempAbs = DataEnvironment::SkyTemp + DataGlobals::KelvinConv;

        // ASHRAE simple convection coefficient model for external surfaces.
        ConvCoef = ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff(DataHeatBalance::VeryRough, DataEnvironment::WindSpeedAt(PondHeight));
        // convective flux
        FluxConvect = ConvCoef * (PondBulkTemp - ExternalTemp);

        // long-wave radiation between pond and sky.
        FluxLongwave = StefBoltzmann * ThermalAbs * (pow_4(SurfTempAbs) - pow_4(SkyTempAbs));

        // total absorbed solar using function - no ground solar
        FluxSolAbsorbed = CalcSolarFlux();

        // specific heat from fluid prop routines
        SpecHeat =
            FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, max(this->InletTemp, 0.0), DataPlant::PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        // heat transfer with fluid - heat exchanger analogy.
        Qfluid = this->MassFlowRate * SpecHeat * this->CalcEffectiveness(this->InletTemp, PondBulkTemp, this->MassFlowRate) *
                 (this->InletTemp - PondBulkTemp);

        this->HeatTransferRate = Qfluid;

        // evaporation flux
        // get air properties
        HumRatioAir = Psychrometrics::PsyWFnTdbTwbPb(OutDryBulb, OutWetBulb, DataEnvironment::OutBaroPress);
        HumRatioFilm = Psychrometrics::PsyWFnTdbTwbPb(PondBulkTemp, PondBulkTemp, DataEnvironment::OutBaroPress);
        SpecHeatAir = Psychrometrics::PsyCpAirFnWTdb(HumRatioAir, OutDryBulb);
        LatentHeatAir = Psychrometrics::PsyHfgAirFnWTdb(HumRatioAir, OutDryBulb);

        FluxEvap = pow_2(PrantlAir / SchmidtAir) / 3.0 * ConvCoef / SpecHeatAir * (HumRatioFilm - HumRatioAir) * LatentHeatAir;

        // ground heat transfer flux
        Perimeter = 4.0 * std::sqrt(this->Area); // square assumption
        UvalueGround = 0.999 * (GrndConductivity / this->Depth) + 1.37 * (GrndConductivity * Perimeter / this->Area);
        FluxGround = UvalueGround * (PondBulkTemp - DataEnvironment::GroundTemp_Deep);

        CalcTotalFLux = Qfluid + this->Area * (FluxSolAbsorbed - FluxConvect - FluxLongwave - FluxEvap - FluxGround);
        if (DataGlobals::BeginTimeStepFlag) {
        }

        return CalcTotalFLux;
    }

    Real64 PondGroundHeatExchangerData::CalcSolarFlux()
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is used to calculate the net solar flux absorbed by the pond.

        // METHODOLOGY EMPLOYED:
        // This is calculated from basic optical formula using the extinction
        // coefficient of the pond as the main parameter. This can be in a
        // wide range: 0.13 - 7.5 in the literature depending on algae, suspended
        // solids etc. ??

        // REFERENCES:
        // Duffie, J.A. and W.A. Beckman, 1991. Solar Engineering of Thermal
        //  Processes, 2 nd Edition. John Wiley and Sons.
        // Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
        //   M.S. Thesis, Oklahoma State University, December 1999.
        // Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
        //   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
        //   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
        //   ASHRAE Transactions.  106(2):107-121.

        Real64 CalcSolarFlux; // Function return variable

        Real64 const WaterRefIndex(1.33); // refractive index of water
        Real64 const AirRefIndex(1.0003); // refractive index of air
        Real64 const PondExtCoef(0.3);    // extinction coefficent of water

        Real64 IncidAngle;   // angle of incidence of beam
        Real64 RefractAngle; // angle of refraction of beam
        Real64 Transmitance; // transmitted solar
        Real64 Reflectance;  // reflectance
        Real64 Absorbtance;  // absorbed solar

        Real64 ParallelRad; // parallel component of irradiation
        Real64 PerpendRad;  // parallel component of irradiation

        // check for sun up.
        if (!DataEnvironment::SunIsUp) {
            CalcSolarFlux = 0.0;
            return CalcSolarFlux;
        }

        // get the incidence and reflection angles
        IncidAngle = std::acos(DataEnvironment::SOLCOS(3));
        RefractAngle = std::asin(std::sin(IncidAngle) * AirRefIndex / WaterRefIndex);

        // absorbed component: Tau_a
        Absorbtance = std::exp(-PondExtCoef * this->Depth / std::cos(RefractAngle));

        // parallel and perpendicular components
        ParallelRad = pow_2(std::tan(RefractAngle - IncidAngle)) / pow_2(std::tan(RefractAngle + IncidAngle));
        PerpendRad = pow_2(std::sin(RefractAngle - IncidAngle)) / pow_2(std::sin(RefractAngle + IncidAngle));

        // transmittance: Tau
        Transmitance = 0.5 * Absorbtance * ((1.0 - ParallelRad) / (1.0 + ParallelRad) + (1.0 - PerpendRad) / (1.0 + PerpendRad));

        // reflectance: Tau_a - Tau
        Reflectance = Absorbtance - Transmitance;

        // apply reflectance to beam and diffuse solar to find flux
        CalcSolarFlux = (1.0 - Reflectance) * (DataEnvironment::SOLCOS(3) * DataEnvironment::BeamSolarRad + DataEnvironment::DifSolarRad);

        return CalcSolarFlux;
    }

    Real64 PondGroundHeatExchangerData::CalcEffectiveness(Real64 const InsideTemperature, // Temperature of fluid in pipe circuit, in C
                                                          Real64 const PondTemperature,   // Temperature of pond water (i.e. outside the pipe), in C
                                                          Real64 const massFlowRate       // Mass flow rate, in kg/s
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the "heat exchanger" effectiveness.
        // This routine is adapted from that in the low temp radiant pond model.

        // METHODOLOGY EMPLOYED:
        // The heat transfer coefficient is calculated at the pipe and
        // consists of inside and outside convection coefficients and conduction
        // through the pipe. The other assumptions are that the tube inside
        // surface temperature is equal to the "source location temperature"
        // and that it is a CONSTANT throughout the pond. External convection is
        // natural mode using Churchill and Chu correlation. Inside convection
        // calculated using the Dittus-Boelter equation.

        // REFERENCES:
        // Incropera, F.P. and D.P. DeWitt, 1996. Introduction to Heat Transfer,
        //   3 rd Edition. John Wiley & Sons.
        // Churchill, S.W. and H.H.S. Chu. 1975. Correlating Equations for
        //   Laminar and Turbulent Free Convection from a Horizontal Cylinder.
        //   International Journal of Heat and Mass Transfer, 18: 1049-1053.
        // See also RadiantSystemLowTemp module.

        Real64 CalcEffectiveness; // Function return variable

        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        Real64 const GravConst(9.81);      // gravitational constant - should be fixed!
        static std::string const CalledFrom("PondGroundHeatExchanger:CalcEffectiveness");

        Real64 NusseltNum;       // Nusselt number (dimensionless)
        Real64 PrantlNum;       // Prandtl number (dimensionless)
        Real64 ReynoldsNum;     // Reynolds number (dimensionless)
        Real64 RayleighNum;     // Rayleigh number (dimensionless)
        Real64 ThermDiff;       // thermal diffusivity
        Real64 ExpansionCoef;   // Expansion coefficient, in K^-1
        Real64 Viscosity;       // Viscosity, in Ns/m2
        Real64 Density;         // fluid density
        Real64 SpecificHeat;    // Fluid specific heat
        Real64 Conductivity;    // Fluid thermal conductivity
        Real64 WaterSpecHeat;   // Specific heat of pond water
        Real64 NTU;             // Number of transfer units, non-dimensional
        Real64 ConvCoefOut;     // convection coefficient at outside of pipe
        Real64 ConvCoefIn;      // convection coefficient at inside of pipe
        Real64 PipeResistance;  // pipe wall thermal resistance
        Real64 TotalResistance; // total pipe thermal resistance - conduction and convection

        // evaluate properties at pipe fluid temperature for given pipe fluid

        SpecificHeat = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, InsideTemperature, DataPlant::PlantLoop(this->LoopNum).FluidIndex, CalledFrom);
        Conductivity = FluidProperties::GetConductivityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, InsideTemperature, DataPlant::PlantLoop(this->LoopNum).FluidIndex, CalledFrom);
        Viscosity = FluidProperties::GetViscosityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, InsideTemperature, DataPlant::PlantLoop(this->LoopNum).FluidIndex, CalledFrom);

        // Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
        ReynoldsNum = 4.0 * massFlowRate / (DataGlobals::Pi * Viscosity * TubeInDiameter * NumCircuits);

        PrantlNum = Viscosity * SpecificHeat / Conductivity;

        // Calculate the Nusselt number based on what flow regime one is in. h = (k)(Nu)/D
        if (ReynoldsNum >= MaxLaminarRe) { // Turbulent flow --> use Dittus-Boelter equation
            NusseltNum = 0.023 * std::pow(ReynoldsNum, 0.8) * std::pow(PrantlNum, 0.3);
        } else { // Laminar flow --> use constant surface temperature relation
            NusseltNum = 3.66;
        }

        // inside convection resistance, from Nu
        ConvCoefIn = Conductivity * NusseltNum / TubeInDiameter;

        // now find properties of pond water - always assume pond fluid is water
        WaterSpecHeat = FluidProperties::GetSpecificHeatGlycol(fluidNameWater, max(PondTemperature, 0.0), this->WaterIndex, CalledFrom);
        Conductivity = FluidProperties::GetConductivityGlycol(fluidNameWater, max(PondTemperature, 0.0), this->WaterIndex, CalledFrom);
        Viscosity = FluidProperties::GetViscosityGlycol(fluidNameWater, max(PondTemperature, 0.0), this->WaterIndex, CalledFrom);
        Density = FluidProperties::GetDensityGlycol(fluidNameWater, max(PondTemperature, 0.0), this->WaterIndex, CalledFrom);

        // derived properties for natural convection coefficient
        // expansion coef (Beta) = -1/Rho. dRho/dT
        // The following code includes some slight modifications from Simon's original code.
        // It guarantees that the delta T is 10C and also avoids the problems associated with
        // water hitting a maximum density at around 4C. (RKS)
        ExpansionCoef = -(FluidProperties::GetDensityGlycol(fluidNameWater, max(PondTemperature, 10.0) + 5.0, WaterIndex, CalledFrom) -
                          FluidProperties::GetDensityGlycol(fluidNameWater, max(PondTemperature, 10.0) - 5.0, this->WaterIndex, CalledFrom)) /
                        (10.0 * Density);

        ThermDiff = Conductivity / (Density * WaterSpecHeat);
        PrantlNum = Viscosity * WaterSpecHeat / Conductivity;

        RayleighNum =
            Density * GravConst * ExpansionCoef * std::abs(InsideTemperature - PondTemperature) * pow_3(TubeOutDiameter) / (Viscosity * ThermDiff);

        // Calculate the Nusselt number for natural convection at outside of pipe
        NusseltNum = pow_2(0.6 + (0.387 * std::pow(RayleighNum, 1.0 / 6.0) / (std::pow(1.0 + 0.559 / std::pow(PrantlNum, 9.0 / 16.0), 8.0 / 27.0))));

        // outside convection resistance, from Nu
        ConvCoefOut = Conductivity * NusseltNum / TubeOutDiameter;

        // conduction resistance of pipe
        PipeResistance = TubeInDiameter / TubeConductivity * std::log(TubeOutDiameter / TubeInDiameter);

        TotalResistance = PipeResistance + 1.0 / ConvCoefIn + TubeInDiameter / (TubeOutDiameter * ConvCoefOut);

        // Calculate the NTU parameter
        // NTU = UA/[(Mdot*Cp)min] = A/[Rtot*(Mdot*Cp)min]
        // where: Rtot = Ri,convection + Rconduction + Ro,conveciton
        //        A = Pi*D*TubeLength

        if (massFlowRate == 0.0) {
            CalcEffectiveness = 1.0;
        } else {
            NTU = DataGlobals::Pi * TubeInDiameter * this->CircuitLength * NumCircuits / (TotalResistance * massFlowRate * SpecificHeat);
            // Calculate effectiveness - formula for static fluid
            CalcEffectiveness = (1.0 - std::exp(-NTU));
        }

        // Check for frozen pond
        if (PondTemperature < 0.0) {
            ++this->ConsecutiveFrozen;
            if (this->FrozenErrIndex == 0) {
                ShowWarningMessage("GroundHeatExchanger:Pond=\"" + this->Name + "\", is frozen; Pond model not valid. Calculated Pond Temperature=[" +
                                   General::RoundSigDigits(PondTemperature, 2) + "] C");
                ShowContinueErrorTimeStamp("");
            }
            ShowRecurringWarningErrorAtEnd("GroundHeatExchanger:Pond=\"" + this->Name + "\", is frozen",
                                           this->FrozenErrIndex,
                                           PondTemperature,
                                           PondTemperature,
                                           _,
                                           "[C]",
                                           "[C]");
            if (this->ConsecutiveFrozen >= DataGlobals::NumOfTimeStepInHour * 30) {
                ShowFatalError("GroundHeatExchanger:Pond=\"" + this->Name + "\" has been frozen for 30 consecutive hours.  Program terminates.");
            }
        } else {
            this->ConsecutiveFrozen = 0;
        }

        return CalcEffectiveness;
    }

    void PondGroundHeatExchangerData::UpdatePondGroundHeatExchanger()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does any updating that needs to be done for pond
        // ground heat exchangers.   This routine must also set the outlet water
        // conditions.

        static std::string const RoutineName("PondGroundHeatExchanger:Update");

        Real64 CpFluid; // Specific heat of working fluid

        // Calculate the water side outlet conditions and set the
        // appropriate conditions on the correct HVAC node.
        CpFluid = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName, this->InletTemp, DataPlant::PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        // check for flow

        PlantUtilities::SafeCopyPlantNode(InletNodeNum, OutletNodeNum);

        if ((CpFluid > 0.0) && (this->MassFlowRate > 0.0)) {

            DataLoopNode::Node(OutletNodeNum).Temp = this->InletTemp - this->HeatTransferRate / (this->MassFlowRate * CpFluid);
            DataLoopNode::Node(OutletNodeNum).Enthalpy = DataLoopNode::Node(OutletNodeNum).Temp * CpFluid;
        }

        // keep track of the bulk temperature
        this->BulkTemperature = PondTemp;
    }

    //==============================================================================

    void PondGroundHeatExchangerData::ReportPondGroundHeatExchanger() // Index for the pond under consideration
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   August 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simply produces output for Pond ground heat exchangers

        // update flows and temps from node data
        this->InletTemp = DataLoopNode::Node(this->InletNodeNum).Temp;
        this->OutletTemp = DataLoopNode::Node(this->OutletNodeNum).Temp;
        this->MassFlowRate = DataLoopNode::Node(this->InletNodeNum).MassFlowRate;

        // update other variables from module variables
        this->Energy = this->HeatTransferRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    }

} // namespace PondGroundHeatExchanger

} // namespace EnergyPlus
