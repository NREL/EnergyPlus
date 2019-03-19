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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

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
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutsideEnergySources.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include "OutsideEnergySources.hh"


namespace EnergyPlus {

namespace OutsideEnergySources {

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Unknown
    //       MODIFIED       na
    //       RE-ENGINEERED  Brent Griffith, Sept 2010, revised plant interactions.

    // PURPOSE OF THIS MODULE:
    // Module containing the routines dealing with the OutsideEnergySources

    // MODULE VARIABLE DECLARATIONS:
    int NumDistrictUnits(0);

    // SUBROUTINE SPECIFICATIONS FOR MODULE OutsideEnergySources
    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool SimOutsideEnergyGetInputFlag(true);
    } // namespace
    // Object Data
    Array1D<OutsideEnergySourceSpecs> EnergySource;
    std::unordered_map<std::string, std::string> EnergySourceUniqueNames;

    // Functions
    void clear_state()
    {
        NumDistrictUnits = 0;
        SimOutsideEnergyGetInputFlag = true;
        EnergySource.deallocate();
        EnergySourceUniqueNames.clear();
    }

    PlantComponent *OutsideEnergySourceSpecs::factory(int objectType, std::string objectName) {
        // Process the input data for outside energy sources if it hasn't been done already
        if (SimOutsideEnergyGetInputFlag) {
            GetOutsideEnergySourcesInput();
            SimOutsideEnergyGetInputFlag = false;
        }
        // Now look for this particular pipe in the list
        for (auto &source : EnergySource) {
            if (source.EnergyType == objectType && source.Name == objectName) {
                return &source;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(
                "OutsideEnergySourceSpecsFactory: Error getting inputs for source named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void OutsideEnergySourceSpecs::simulate(const PlantLocation &EP_UNUSED(calledFromLocation), bool EP_UNUSED(FirstHVACIteration),
                                            Real64 &CurLoad, bool RunFlag) {
        this->initialize(CurLoad);
        this->calculate(RunFlag, CurLoad);
    }

    void OutsideEnergySourceSpecs::onInitLoopEquip(const PlantLocation &) {
        this->initialize(0.0);
        this->size();
    }

    void OutsideEnergySourceSpecs::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad,
                                                       Real64 &MinLoad, Real64 &OptLoad) {
        MinLoad = 0.0;
        MaxLoad = this->NomCap;
        OptLoad = this->NomCap;
    }

    void GetOutsideEnergySourcesInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   April 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine obtains the input data puts it into the
        // component arrays. Data items in the component arrays
        // are initialized. Output variables are set up.

        // GET NUMBER OF ALL EQUIPMENT TYPES
        int const NumDistrictUnitsHeat = inputProcessor->getNumObjectsFound("DistrictHeating");
        int const NumDistrictUnitsCool = inputProcessor->getNumObjectsFound("DistrictCooling");
        NumDistrictUnits = NumDistrictUnitsHeat + NumDistrictUnitsCool;

        if (allocated(EnergySource)) return;

        EnergySource.allocate(NumDistrictUnits);
        EnergySourceUniqueNames.reserve(static_cast<unsigned>(NumDistrictUnits));

        bool ErrorsFound(false); // If errors detected in input
        int heatIndex = 0;
        int coolIndex = 0;

        for (int EnergySourceNum = 1; EnergySourceNum <= NumDistrictUnits; ++EnergySourceNum) {

            std::string reportVarPrefix;
            std::string nodeNames;
            int typeOf;
            int thisIndex;
            if (EnergySourceNum <= NumDistrictUnitsHeat) {
                DataIPShortCuts::cCurrentModuleObject = "DistrictHeating";
                reportVarPrefix = "District Heating ";
                nodeNames = "Hot Water Nodes";
                typeOf = DataPlant::TypeOf_PurchHotWater;
                heatIndex++;
                thisIndex = heatIndex;
            } else {
                DataIPShortCuts::cCurrentModuleObject = "DistrictCooling";
                reportVarPrefix = "District Cooling ";
                nodeNames = "Chilled Water Nodes";
                typeOf = DataPlant::TypeOf_PurchChilledWater;
                coolIndex++;
                thisIndex = coolIndex;
            }

            int NumAlphas = 0, NumNums = 0, IOStat = 0;
            inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, thisIndex, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, _, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames);

            if (EnergySourceNum > 1) {
                GlobalNames::VerifyUniqueInterObjectName(
                    EnergySourceUniqueNames, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);
            }
            EnergySource(EnergySourceNum).Name = DataIPShortCuts::cAlphaArgs(1);
            EnergySource(EnergySourceNum).InletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            EnergySource(EnergySourceNum).OutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(2), DataIPShortCuts::cAlphaArgs(3), nodeNames);
            EnergySource(EnergySourceNum).NomCap = DataIPShortCuts::rNumericArgs(1);
            if (EnergySource(EnergySourceNum).NomCap == DataSizing::AutoSize) {
                EnergySource(EnergySourceNum).NomCapWasAutoSized = true;
            }
            EnergySource(EnergySourceNum).EnergyTransfer = 0.0;
            EnergySource(EnergySourceNum).EnergyRate = 0.0;
            EnergySource(EnergySourceNum).EnergyType = typeOf;
            if (!DataIPShortCuts::lAlphaFieldBlanks(4)) {
                EnergySource(EnergySourceNum).CapFractionSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(4));
                if (EnergySource(EnergySourceNum).CapFractionSchedNum == 0) {
                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + EnergySource(EnergySourceNum).Name + "\", is not valid");
                    ShowContinueError(DataIPShortCuts::cAlphaFieldNames(4) + "=\"" + DataIPShortCuts::cAlphaArgs(4) + "\" was not found.");
                    ErrorsFound = true;
                }
                if (!ScheduleManager::CheckScheduleValueMinMax(EnergySource(EnergySourceNum).CapFractionSchedNum, ">=", 0.0)) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + EnergySource(EnergySourceNum).Name + "\", is not valid");
                    ShowContinueError(DataIPShortCuts::cAlphaFieldNames(4) + "=\"" + DataIPShortCuts::cAlphaArgs(4) + "\" should not have negative values.");
                    ShowContinueError("Negative values will be treated as zero, and the simulation continues.");
                }
            } else {
                EnergySource(EnergySourceNum).CapFractionSchedNum = DataGlobals::ScheduleAlwaysOn;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject + ", Preceding condition caused termination.");
        }

    }

    void OutsideEnergySourceSpecs::initialize(Real64 MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does one-time inits and sets the operating mass flow rate of this machine

        // METHODOLOGY EMPLOYED:
        // One time inits include validating source type (should happen in getinput?) and locating this
        //  component on the PlantLoop topology.
        // The mass flow rate is determined based on component load, and making use of
        //  the SetComponentFlowRate routine.
        // The mass flow rate could be an inter-connected-loop side trigger. This is not really the type of
        //  interconnect that that routine was written for, but it is the clearest example of using it.

        if (this->OneTimeInitFlag) {
            // Locate the unit on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    this->EnergyType,
                                    this->LoopNum,
                                    this->LoopSideNum,
                                    this->BranchNum,
                                    this->CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError("InitSimVars: Program terminated due to previous condition(s).");
            }
            // set limits on outlet node temps to plant loop limits
            DataPlant::PlantLoop(this->LoopNum)
                .LoopSide(this->LoopSideNum)
                .Branch(this->BranchNum)
                .Comp(this->CompNum)
                .MinOutletTemp = DataPlant::PlantLoop(this->LoopNum).MinTemp;
            DataPlant::PlantLoop(this->LoopNum)
                .LoopSide(this->LoopSideNum)
                .Branch(this->BranchNum)
                .Comp(this->CompNum)
                .MaxOutletTemp = DataPlant::PlantLoop(this->LoopNum).MaxTemp;
            // Register design flow rate for inlet node (helps to autosize comp setpoint op scheme flows
            PlantUtilities::RegisterPlantCompDesignFlow(this->InletNodeNum, DataPlant::PlantLoop(this->LoopNum).MaxVolFlowRate);

            this->OneTimeInitFlag = false;

            for (int EnergySourceNum = 1; EnergySourceNum <= NumDistrictUnits; ++EnergySourceNum) {
                std::string hotOrChilled = "Hot ";
                std::string reportVarPrefix = "District Heating ";
                std::string heatingOrCooling = "Heating";
                if (EnergySource(EnergySourceNum).EnergyType == DataPlant::TypeOf_PurchChilledWater) {
                    hotOrChilled = "Chilled ";
                    reportVarPrefix = "District Cooling ";
                    heatingOrCooling = "Cooling";
                }
                SetupOutputVariable(reportVarPrefix + hotOrChilled + "Water Energy",
                                    OutputProcessor::Unit::J,
                                    EnergySource(EnergySourceNum).EnergyTransfer,
                                    "System",
                                    "Sum",
                                    EnergySource(EnergySourceNum).Name,
                                    _,
                                    DataIPShortCuts::cCurrentModuleObject,
                                    heatingOrCooling,
                                    _,
                                    "Plant");
                SetupOutputVariable(reportVarPrefix + hotOrChilled + "Water Rate",
                                    OutputProcessor::Unit::W,
                                    EnergySource(EnergySourceNum).EnergyRate,
                                    "System",
                                    "Average",
                                    EnergySource(EnergySourceNum).Name);

                SetupOutputVariable(reportVarPrefix + "Rate",
                                    OutputProcessor::Unit::W,
                                    EnergySource(EnergySourceNum).EnergyRate,
                                    "System",
                                    "Average",
                                    EnergySource(EnergySourceNum).Name);
                SetupOutputVariable(reportVarPrefix + "Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    EnergySource(EnergySourceNum).InletTemp,
                                    "System",
                                    "Average",
                                    EnergySource(EnergySourceNum).Name);
                SetupOutputVariable(reportVarPrefix + "Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    EnergySource(EnergySourceNum).OutletTemp,
                                    "System",
                                    "Average",
                                    EnergySource(EnergySourceNum).Name);
                SetupOutputVariable(reportVarPrefix + "Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    EnergySource(EnergySourceNum).MassFlowRate,
                                    "System",
                                    "Average",
                                    EnergySource(EnergySourceNum).Name);
            }

        }

        // begin environment inits
        if (DataGlobals::BeginEnvrnFlag && this->BeginEnvrnInitFlag) {
            // component model has not design flow rates, using data for overall plant loop
            PlantUtilities::InitComponentNodes(DataPlant::PlantLoop(this->LoopNum).MinMassFlowRate,
                                               DataPlant::PlantLoop(this->LoopNum).MaxMassFlowRate,
                               this->InletNodeNum,
                               this->OutletNodeNum,
                               this->LoopNum,
                               this->LoopSideNum,
                               this->BranchNum,
                               this->CompNum);
            this->BeginEnvrnInitFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) this->BeginEnvrnInitFlag = true;

        // now do every time inits
        int const InletNode = this->InletNodeNum;
        int const OutletNode = this->OutletNodeNum;
        int const LoopNum = this->LoopNum;
        int const LoopSideNum = this->LoopSideNum;
        int const BranchIndex = this->BranchNum;
        int const CompIndex = this->CompNum;

        Real64 TempPlantMdot(0.0);
        if (std::abs(MyLoad) > 0.0) {
            TempPlantMdot = DataPlant::PlantLoop(LoopNum).MaxMassFlowRate;
        }

        // get actual mass flow to use, hold in MassFlowRate variable
        PlantUtilities::SetComponentFlowRate(TempPlantMdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex);

        this->InletTemp = DataLoopNode::Node(InletNode).Temp;
        this->MassFlowRate = TempPlantMdot;
    }

    void OutsideEnergySourceSpecs::size()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   April 2014
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for sizing capacities of district cooling and heating objects.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // If errors detected in input

        // Type name string variable to collapse the sizing for cooling and heating into one block
        std::string typeName;
        if (this->EnergyType == DataPlant::TypeOf_PurchChilledWater) {
            typeName = "Cooling";
        } else { // Heating
            typeName = "Heating";
        }

        int const PltSizNum = DataPlant::PlantLoop(this->LoopNum).PlantSizNum;
        if (PltSizNum > 0) {
            Real64 const rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                                                 DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                   "SizeDistrict" + typeName);
            Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                                                     DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                       "SizeDistrict" + typeName);
            Real64 const NomCapDes = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = NomCapDes;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("District" + typeName, this->Name, "Design Size Nominal Capacity [W]", NomCapDes);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "District" + typeName, this->Name, "Initial Design Size Nominal Capacity [W]", NomCapDes);
                    }
                } else { // Hard-size with sizing data
                    if (this->NomCap > 0.0 && NomCapDes > 0.0) {
                        Real64 const NomCapUser = this->NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("District" + typeName,
                                               this->Name,
                                               "Design Size Nominal Capacity [W]",
                                               NomCapDes,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(NomCapDes - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeDistrict" + typeName + ": Potential issue with equipment sizing for " +
                                                this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(NomCapDes, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of District " + typeName + " nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in District" + typeName + " object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && this->NomCap > 0.0 && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("District" + typeName,
                                   this->Name,
                                   "User-Specified Nominal Capacity [W]",
                                   this->NomCap);
            }
        }
        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void OutsideEnergySourceSpecs::calculate(bool runFlag, Real64 MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   July 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

         // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SimDistrictEnergy");

        // set inlet and outlet nodes
        int const LoopNum = this->LoopNum;
        Real64 const LoopMinTemp = DataPlant::PlantLoop(LoopNum).MinTemp;
        Real64 const LoopMaxTemp = DataPlant::PlantLoop(LoopNum).MaxTemp;

        Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(LoopNum).FluidName, this->InletTemp, DataPlant::PlantLoop(LoopNum).FluidIndex, RoutineName);

        //  apply power limit from input
        Real64 CapFraction = ScheduleManager::GetCurrentScheduleValue(this->CapFractionSchedNum);
        CapFraction = max(0.0, CapFraction); // ensure non negative
        Real64 const CurrentCap = this->NomCap * CapFraction;
        if (std::abs(MyLoad) > CurrentCap) {
            MyLoad = sign(CurrentCap, MyLoad);
        }

        if (this->EnergyType == DataPlant::TypeOf_PurchChilledWater) {
            if (MyLoad > 0.0) MyLoad = 0.0;
        } else if (this->EnergyType == DataPlant::TypeOf_PurchHotWater) {
            if (MyLoad < 0.0) MyLoad = 0.0;
        }

        // determine outlet temp based on inlet temp, cp, and MyLoad
        if ((this->MassFlowRate > 0.0) && runFlag) {
            this->OutletTemp = (MyLoad + this->MassFlowRate * Cp * this->InletTemp) / (this->MassFlowRate * Cp);
            // apply loop limits on temperature result to keep in check
            if (this->OutletTemp < LoopMinTemp) {
                this->OutletTemp = max(this->OutletTemp, LoopMinTemp);
                MyLoad = this->MassFlowRate * Cp * (this->OutletTemp - this->InletTemp);
            }
            if (this->OutletTemp > LoopMaxTemp) {
                this->OutletTemp = min(this->OutletTemp, LoopMaxTemp);
                MyLoad = this->MassFlowRate * Cp * (this->OutletTemp - this->InletTemp);
            }
        } else {
            this->OutletTemp = this->InletTemp;
            MyLoad = 0.0;
        }
        int const OutletNode = this->OutletNodeNum;
        DataLoopNode::Node(OutletNode).Temp = this->OutletTemp;
        this->EnergyRate = std::abs(MyLoad);
        this->EnergyTransfer = this->EnergyRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    }

} // namespace OutsideEnergySources

} // namespace EnergyPlus
