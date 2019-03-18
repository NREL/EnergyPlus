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

    void SimOutsideEnergy(std::string const &EP_UNUSED(EnergyType),
                          std::string const &EquipName,
                          int const EP_UNUSED(EquipFlowCtrl), // Flow control mode for the equipment
                          int &CompIndex,
                          bool const RunFlag,
                          bool const InitLoopEquip,
                          Real64 &MyLoad,
                          Real64 &MaxCap,
                          Real64 &MinCap,
                          Real64 &OptCap,
                          bool const EP_UNUSED(FirstHVACIteration))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manage the simulation of district (aka purchased) energy.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int EqNum;

        // GET INPUT
        if (SimOutsideEnergyGetInputFlag) {
            GetOutsideEnergySourcesInput();
            SimOutsideEnergyGetInputFlag = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            EqNum = UtilityRoutines::FindItemInList(EquipName, EnergySource);
            if (EqNum == 0) {
                ShowFatalError("SimOutsideEnergy: Unit not found=" + EquipName);
            }
            CompIndex = EqNum;
        } else {
            EqNum = CompIndex;
            if (EnergySource(EqNum).CheckEquipName) {
                if (EqNum > NumDistrictUnits || EqNum < 1) {
                    ShowFatalError("SimOutsideEnergy:  Invalid CompIndex passed=" + General::TrimSigDigits(EqNum) +
                                   ", Number of Units=" + General::TrimSigDigits(NumDistrictUnits) + ", Entered Unit name=" + EquipName);
                }
                if (EquipName != EnergySource(EqNum).Name) {
                    ShowFatalError("SimOutsideEnergy: Invalid CompIndex passed=" + General::TrimSigDigits(EqNum) + ", Unit name=" + EquipName +
                                   ", stored Unit Name for that index=" + EnergySource(EqNum).Name);
                }
                EnergySource(EqNum).CheckEquipName = false;
            }
        }

        // CALCULATE
        if (InitLoopEquip) {
            InitSimVars(EqNum, MyLoad);
            SizeDistrictEnergy(EqNum);

            MinCap = 0.0;
            MaxCap = EnergySource(EqNum).NomCap;
            OptCap = EnergySource(EqNum).NomCap;
            return;
        }

        InitSimVars(EqNum, MyLoad);
        SimDistrictEnergy(RunFlag, EqNum, MyLoad);
        UpdateRecords(MyLoad, EqNum);
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
        int const NumDistrictUnitsHeat = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
        int const NumDistrictUnitsCool = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
        NumDistrictUnits = NumDistrictUnitsHeat + NumDistrictUnitsCool;

        if (allocated(EnergySource)) return;

        EnergySource.allocate(NumDistrictUnits);
        EnergySourceUniqueNames.reserve(static_cast<unsigned>(NumDistrictUnits));

        bool ErrorsFound(false); // If errors detected in input

        for (int EnergySourceNum = 1; EnergySourceNum <= NumDistrictUnits; ++EnergySourceNum) {

            DataIPShortCuts::cCurrentModuleObject = "DistrictHeating";
            std::string reportVarPrefix = "District Heating ";
            std::string nodeNames = "Hot Water Nodes";
            int typeOf = DataPlant::TypeOf_PurchHotWater;
            if (EnergySourceNum > NumDistrictUnitsHeat) {
                DataIPShortCuts::cCurrentModuleObject = "DistrictCooling";
                reportVarPrefix = "District Cooling ";
                nodeNames = "Chilled Water Nodes";
                typeOf = DataPlant::TypeOf_PurchChilledWater;
            }

            int NumAlphas = 0, NumNums = 0, IOStat = 0;
            inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, EnergySourceNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, _, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames);

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

    void InitSimVars(int const EnergySourceNum, Real64 const MyLoad)
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

        if (EnergySource(EnergySourceNum).OneTimeInitFlag) {
            // Locate the unit on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(EnergySource(EnergySourceNum).Name,
                                                    EnergySource(EnergySourceNum).EnergyType,
                                    EnergySource(EnergySourceNum).LoopNum,
                                    EnergySource(EnergySourceNum).LoopSideNum,
                                    EnergySource(EnergySourceNum).BranchNum,
                                    EnergySource(EnergySourceNum).CompNum,
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
            DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum)
                .LoopSide(EnergySource(EnergySourceNum).LoopSideNum)
                .Branch(EnergySource(EnergySourceNum).BranchNum)
                .Comp(EnergySource(EnergySourceNum).CompNum)
                .MinOutletTemp = DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).MinTemp;
            DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum)
                .LoopSide(EnergySource(EnergySourceNum).LoopSideNum)
                .Branch(EnergySource(EnergySourceNum).BranchNum)
                .Comp(EnergySource(EnergySourceNum).CompNum)
                .MaxOutletTemp = DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).MaxTemp;
            // Register design flow rate for inlet node (helps to autosize comp setpoint op scheme flows
            PlantUtilities::RegisterPlantCompDesignFlow(EnergySource(EnergySourceNum).InletNodeNum, DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).MaxVolFlowRate);

            EnergySource(EnergySourceNum).OneTimeInitFlag = false;
        }

        // begin environment inits
        if (DataGlobals::BeginEnvrnFlag && EnergySource(EnergySourceNum).BeginEnvrnInitFlag) {
            // component model has not design flow rates, using data for overall plant loop
            PlantUtilities::InitComponentNodes(DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).MinMassFlowRate,
                                               DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).MaxMassFlowRate,
                               EnergySource(EnergySourceNum).InletNodeNum,
                               EnergySource(EnergySourceNum).OutletNodeNum,
                               EnergySource(EnergySourceNum).LoopNum,
                               EnergySource(EnergySourceNum).LoopSideNum,
                               EnergySource(EnergySourceNum).BranchNum,
                               EnergySource(EnergySourceNum).CompNum);
            EnergySource(EnergySourceNum).BeginEnvrnInitFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) EnergySource(EnergySourceNum).BeginEnvrnInitFlag = true;

        // now do every time inits
        int const InletNode = EnergySource(EnergySourceNum).InletNodeNum;
        int const OutletNode = EnergySource(EnergySourceNum).OutletNodeNum;
        int const LoopNum = EnergySource(EnergySourceNum).LoopNum;
        int const LoopSideNum = EnergySource(EnergySourceNum).LoopSideNum;
        int const BranchIndex = EnergySource(EnergySourceNum).BranchNum;
        int const CompIndex = EnergySource(EnergySourceNum).CompNum;

        Real64 TempPlantMdot(0.0);
        if (std::abs(MyLoad) > 0.0) {
            TempPlantMdot = DataPlant::PlantLoop(LoopNum).MaxMassFlowRate;
        }

        // get actual mass flow to use, hold in MassFlowRate variable
        PlantUtilities::SetComponentFlowRate(TempPlantMdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex);

        EnergySource(EnergySourceNum).InletTemp = DataLoopNode::Node(InletNode).Temp;
        EnergySource(EnergySourceNum).MassFlowRate = TempPlantMdot;
    }

    void SizeDistrictEnergy(int const EnergySourceNum)
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
        if (EnergySource(EnergySourceNum).EnergyType == DataPlant::TypeOf_PurchChilledWater) {
            typeName = "Cooling";
        } else { // Heating
            typeName = "Heating";
        }

        int const PltSizNum = DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).PlantSizNum;
        if (PltSizNum > 0) {
            Real64 const rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                                                 DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidIndex,
                                   "SizeDistrict" + typeName);
            Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                                                     DataPlant::PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidIndex,
                                       "SizeDistrict" + typeName);
            Real64 const NomCapDes = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (EnergySource(EnergySourceNum).NomCapWasAutoSized) {
                    EnergySource(EnergySourceNum).NomCap = NomCapDes;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("District" + typeName, EnergySource(EnergySourceNum).Name, "Design Size Nominal Capacity [W]", NomCapDes);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "District" + typeName, EnergySource(EnergySourceNum).Name, "Initial Design Size Nominal Capacity [W]", NomCapDes);
                    }
                } else { // Hard-size with sizing data
                    if (EnergySource(EnergySourceNum).NomCap > 0.0 && NomCapDes > 0.0) {
                        Real64 const NomCapUser = EnergySource(EnergySourceNum).NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("District" + typeName,
                                               EnergySource(EnergySourceNum).Name,
                                               "Design Size Nominal Capacity [W]",
                                               NomCapDes,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(NomCapDes - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeDistrict" + typeName + ": Potential issue with equipment sizing for " +
                                                EnergySource(EnergySourceNum).Name);
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
            if (EnergySource(EnergySourceNum).NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of District " + typeName + " nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in District" + typeName + " object=" + EnergySource(EnergySourceNum).Name);
                ErrorsFound = true;
            }
            if (!EnergySource(EnergySourceNum).NomCapWasAutoSized && EnergySource(EnergySourceNum).NomCap > 0.0 && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("District" + typeName,
                                   EnergySource(EnergySourceNum).Name,
                                   "User-Specified Nominal Capacity [W]",
                                   EnergySource(EnergySourceNum).NomCap);
            }
        }
        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void SimDistrictEnergy(bool const RunFlag, int const DistrictEqNum, Real64 &MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   July 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

         // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SimDistrictEnergy");

        // set inlet and outlet nodes
        int const LoopNum = EnergySource(DistrictEqNum).LoopNum;
        Real64 const LoopMinTemp = DataPlant::PlantLoop(LoopNum).MinTemp;
        Real64 const LoopMaxTemp = DataPlant::PlantLoop(LoopNum).MaxTemp;

        Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(LoopNum).FluidName, EnergySource(DistrictEqNum).InletTemp, DataPlant::PlantLoop(LoopNum).FluidIndex, RoutineName);

        //  apply power limit from input
        Real64 CapFraction = ScheduleManager::GetCurrentScheduleValue(EnergySource(DistrictEqNum).CapFractionSchedNum);
        CapFraction = max(0.0, CapFraction); // ensure non negative
        Real64 const CurrentCap = EnergySource(DistrictEqNum).NomCap * CapFraction;
        if (std::abs(MyLoad) > CurrentCap) {
            MyLoad = sign(CurrentCap, MyLoad);
        }

        if (EnergySource(DistrictEqNum).EnergyType == DataPlant::TypeOf_PurchChilledWater) {
            if (MyLoad > 0.0) MyLoad = 0.0;
        } else if (EnergySource(DistrictEqNum).EnergyType == DataPlant::TypeOf_PurchHotWater) {
            if (MyLoad < 0.0) MyLoad = 0.0;
        }

        // determine outlet temp based on inlet temp, cp, and MyLoad
        if ((EnergySource(DistrictEqNum).MassFlowRate > 0.0) && RunFlag) {
            EnergySource(DistrictEqNum).OutletTemp = (MyLoad + EnergySource(DistrictEqNum).MassFlowRate * Cp * EnergySource(DistrictEqNum).InletTemp) / (EnergySource(DistrictEqNum).MassFlowRate * Cp);
            // apply loop limits on temperature result to keep in check
            if (EnergySource(DistrictEqNum).OutletTemp < LoopMinTemp) {
                EnergySource(DistrictEqNum).OutletTemp = max(EnergySource(DistrictEqNum).OutletTemp, LoopMinTemp);
                MyLoad = EnergySource(DistrictEqNum).MassFlowRate * Cp * (EnergySource(DistrictEqNum).OutletTemp - EnergySource(DistrictEqNum).InletTemp);
            }
            if (EnergySource(DistrictEqNum).OutletTemp > LoopMaxTemp) {
                EnergySource(DistrictEqNum).OutletTemp = min(EnergySource(DistrictEqNum).OutletTemp, LoopMaxTemp);
                MyLoad = EnergySource(DistrictEqNum).MassFlowRate * Cp * (EnergySource(DistrictEqNum).OutletTemp - EnergySource(DistrictEqNum).InletTemp);
            }
        } else {
            EnergySource(DistrictEqNum).OutletTemp = EnergySource(DistrictEqNum).InletTemp;
            MyLoad = 0.0;
        }
    }

    void UpdateRecords(Real64 const MyLoad, int const EqNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

        // set inlet and outlet nodes
        int const InletNode = EnergySource(EqNum).InletNodeNum;
        int const OutletNode = EnergySource(EqNum).OutletNodeNum;
        DataLoopNode::Node(OutletNode).Temp = EnergySource(EqNum).OutletTemp;
        EnergySource(EqNum).InletTemp = DataLoopNode::Node(InletNode).Temp;
        EnergySource(EqNum).EnergyRate = std::abs(MyLoad);
        EnergySource(EqNum).EnergyTransfer = EnergySource(EqNum).EnergyRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    }

} // namespace OutsideEnergySources

} // namespace EnergyPlus
