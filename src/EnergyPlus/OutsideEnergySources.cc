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

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::MaxNameLength;
    using DataGlobals::SecInHour;
    using namespace DataEnvironment;
    using namespace DataHVACGlobals;
    using namespace DataLoopNode;
    using DataPlant::PlantLoop;
    using DataPlant::TypeOf_PurchChilledWater;
    using DataPlant::TypeOf_PurchHotWater;
    using General::RoundSigDigits;
    using General::TrimSigDigits;
    using PlantUtilities::ScanPlantLoopsForObject;

    // MODULE PARAMETER DEFINITIONS
    int const EnergyType_DistrictHeating(1);
    int const EnergyType_DistrictCooling(2);

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
    Array1D<ReportVars> EnergySourceReport;
    std::unordered_map<std::string, std::string> EnergySourceUniqueNames;

    // Functions
    void clear_state()
    {
        NumDistrictUnits = 0;
        SimOutsideEnergyGetInputFlag = true;
        EnergySource.deallocate();
        EnergySourceReport.deallocate();
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
        /////////// hoisted into namespace SimOutsideEnergyGetInputFlag ////////////
        // static bool GetInputFlag( true ); // Get input once and once only
        /////////////////////////////////////////////////
        int EqNum;
        Real64 InletTemp;
        Real64 OutletTemp;
        Real64 MassFlowRate;
        // FLOW

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
                    ShowFatalError("SimOutsideEnergy:  Invalid CompIndex passed=" + TrimSigDigits(EqNum) +
                                   ", Number of Units=" + TrimSigDigits(NumDistrictUnits) + ", Entered Unit name=" + EquipName);
                }
                if (EquipName != EnergySource(EqNum).Name) {
                    ShowFatalError("SimOutsideEnergy: Invalid CompIndex passed=" + TrimSigDigits(EqNum) + ", Unit name=" + EquipName +
                                   ", stored Unit Name for that index=" + EnergySource(EqNum).Name);
                }
                EnergySource(EqNum).CheckEquipName = false;
            }
        }

        // CALCULATE
        if (InitLoopEquip) {
            InitSimVars(EqNum, MassFlowRate, InletTemp, OutletTemp, MyLoad);
            SizeDistrictEnergy(EqNum);

            MinCap = 0.0;
            MaxCap = EnergySource(EqNum).NomCap;
            OptCap = EnergySource(EqNum).NomCap;
            return;
        }

        InitSimVars(EqNum, MassFlowRate, InletTemp, OutletTemp, MyLoad);
        SimDistrictEnergy(RunFlag, EqNum, MyLoad, MassFlowRate, InletTemp, OutletTemp);
        UpdateRecords(MyLoad, EqNum, MassFlowRate, OutletTemp);
    }

    // End OutsideEnergySources Module Driver Subroutines
    //******************************************************************************

    // Beginning of OutsideEnergySources Module Get Input subroutines
    //******************************************************************************

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

        // METHODOLOGY EMPLOYED: to be determined...

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::ScheduleAlwaysOn;
        using DataSizing::AutoSize;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int EnergySourceNum;
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        int NumDistrictUnitsHeat;
        int NumDistrictUnitsCool;
        int IndexCounter;
        static bool ErrorsFound(false); // If errors detected in input

        // GET NUMBER OF ALL EQUIPMENT TYPES
        cCurrentModuleObject = "DistrictHeating";
        NumDistrictUnitsHeat = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        cCurrentModuleObject = "DistrictCooling";
        NumDistrictUnitsCool = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        NumDistrictUnits = NumDistrictUnitsHeat + NumDistrictUnitsCool;

        if (allocated(EnergySource)) return;

        EnergySource.allocate(NumDistrictUnits);
        EnergySourceUniqueNames.reserve(static_cast<unsigned>(NumDistrictUnits));
        EnergySourceReport.allocate(NumDistrictUnits);

        cCurrentModuleObject = "DistrictHeating";

        EnergySourceNum = 0;
        for (IndexCounter = 1; IndexCounter <= NumDistrictUnitsHeat; ++IndexCounter) {
            ++EnergySourceNum;
            inputProcessor->getObjectItem(
                cCurrentModuleObject, EnergySourceNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames);

            if (EnergySourceNum > 1) {
                GlobalNames::VerifyUniqueInterObjectName(
                    EnergySourceUniqueNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            }
            EnergySource(EnergySourceNum).Name = cAlphaArgs(1);
            EnergySource(EnergySourceNum).PlantLoopID = "";
            EnergySource(EnergySourceNum).SecndryLoopID = "";
            EnergySource(EnergySourceNum).ScheduleID = "";
            EnergySource(EnergySourceNum).InletNodeNum = GetOnlySingleNode(
                cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            EnergySource(EnergySourceNum).OutletNodeNum = GetOnlySingleNode(
                cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Hot Water Nodes");
            EnergySource(EnergySourceNum).NomCap = rNumericArgs(1);
            if (EnergySource(EnergySourceNum).NomCap == AutoSize) {
                EnergySource(EnergySourceNum).NomCapWasAutoSized = true;
            }
            EnergySource(EnergySourceNum).EnergyTransfer = 0.0;
            EnergySource(EnergySourceNum).EnergyRate = 0.0;
            EnergySource(EnergySourceNum).EnergyType = EnergyType_DistrictHeating;
            if (!lAlphaFieldBlanks(4)) {
                EnergySource(EnergySourceNum).CapFractionSchedNum = GetScheduleIndex(cAlphaArgs(4));
                if (EnergySource(EnergySourceNum).CapFractionSchedNum == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + EnergySource(EnergySourceNum).Name + "\", is not valid");
                    ShowContinueError(cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\" was not found.");
                    ErrorsFound = true;
                }
                if (!CheckScheduleValueMinMax(EnergySource(EnergySourceNum).CapFractionSchedNum, ">=", 0.0)) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + EnergySource(EnergySourceNum).Name + "\", is not valid");
                    ShowContinueError(cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\" should not have negative values.");
                    ShowContinueError("Negative values will be treated as zero, and the simulation continues.");
                }
            } else {
                EnergySource(EnergySourceNum).CapFractionSchedNum = ScheduleAlwaysOn;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + cCurrentModuleObject + ", Preceding condition caused termination.");
        }

        EnergySourceNum = 0;
        for (IndexCounter = 1; IndexCounter <= NumDistrictUnitsHeat; ++IndexCounter) {
            ++EnergySourceNum;
            SetupOutputVariable("District Heating Hot Water Energy",
                                OutputProcessor::Unit::J,
                                EnergySource(EnergySourceNum).EnergyTransfer,
                                "System",
                                "Sum",
                                EnergySource(EnergySourceNum).Name,
                                _,
                                "DistrictHeating",
                                "Heating",
                                _,
                                "Plant");
            SetupOutputVariable("District Heating Hot Water Rate",
                                OutputProcessor::Unit::W,
                                EnergySource(EnergySourceNum).EnergyRate,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);

            SetupOutputVariable("District Heating Rate",
                                OutputProcessor::Unit::W,
                                EnergySource(EnergySourceNum).EnergyRate,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
            SetupOutputVariable("District Heating Inlet Temperature",
                                OutputProcessor::Unit::C,
                                EnergySourceReport(EnergySourceNum).InletTemp,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
            SetupOutputVariable("District Heating Outlet Temperature",
                                OutputProcessor::Unit::C,
                                EnergySourceReport(EnergySourceNum).OutletTemp,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
            SetupOutputVariable("District Heating Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                EnergySourceReport(EnergySourceNum).MassFlowRate,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
        }

        cCurrentModuleObject = "DistrictCooling";

        EnergySourceNum = NumDistrictUnitsHeat; // To initialize counter
        for (IndexCounter = 1; IndexCounter <= NumDistrictUnitsCool; ++IndexCounter) {
            ++EnergySourceNum;
            inputProcessor->getObjectItem(
                cCurrentModuleObject, IndexCounter, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames);

            if (EnergySourceNum > 1) {
                GlobalNames::VerifyUniqueInterObjectName(
                    EnergySourceUniqueNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            }
            EnergySource(EnergySourceNum).Name = cAlphaArgs(1);
            EnergySource(EnergySourceNum).PlantLoopID = "";
            EnergySource(EnergySourceNum).SecndryLoopID = "";
            EnergySource(EnergySourceNum).ScheduleID = "";
            EnergySource(EnergySourceNum).InletNodeNum = GetOnlySingleNode(
                cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            EnergySource(EnergySourceNum).OutletNodeNum = GetOnlySingleNode(
                cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Chilled Water Nodes");
            EnergySource(EnergySourceNum).NomCap = rNumericArgs(1);
            if (EnergySource(EnergySourceNum).NomCap == AutoSize) {
                EnergySource(EnergySourceNum).NomCapWasAutoSized = true;
            }
            EnergySource(EnergySourceNum).EnergyTransfer = 0.0;
            EnergySource(EnergySourceNum).EnergyRate = 0.0;
            EnergySource(EnergySourceNum).EnergyType = EnergyType_DistrictCooling;
            if (!lAlphaFieldBlanks(4)) {
                EnergySource(EnergySourceNum).CapFractionSchedNum = GetScheduleIndex(cAlphaArgs(4));
                if (EnergySource(EnergySourceNum).CapFractionSchedNum == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + EnergySource(EnergySourceNum).Name + "\", is not valid");
                    ShowContinueError(cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\" was not found.");
                    ErrorsFound = true;
                }
                if (!CheckScheduleValueMinMax(EnergySource(EnergySourceNum).CapFractionSchedNum, ">=", 0.0)) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + EnergySource(EnergySourceNum).Name + "\", is not valid");
                    ShowContinueError(cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\" should not have negative values.");
                    ShowContinueError("Negative values will be treated as zero, and the simulation continues.");
                }
            } else {
                EnergySource(EnergySourceNum).CapFractionSchedNum = ScheduleAlwaysOn;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + cCurrentModuleObject + ", Preceding condition caused termination.");
        }

        EnergySourceNum = NumDistrictUnitsHeat; // To initialize counter
        for (IndexCounter = 1; IndexCounter <= NumDistrictUnitsCool; ++IndexCounter) {
            ++EnergySourceNum;
            SetupOutputVariable("District Cooling Chilled Water Energy",
                                OutputProcessor::Unit::J,
                                EnergySource(EnergySourceNum).EnergyTransfer,
                                "System",
                                "Sum",
                                EnergySource(EnergySourceNum).Name,
                                _,
                                "DistrictCooling",
                                "Cooling",
                                _,
                                "Plant");
            SetupOutputVariable("District Cooling Chilled Water Rate",
                                OutputProcessor::Unit::W,
                                EnergySource(EnergySourceNum).EnergyRate,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);

            SetupOutputVariable("District Cooling Rate",
                                OutputProcessor::Unit::W,
                                EnergySource(EnergySourceNum).EnergyRate,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
            SetupOutputVariable("District Cooling Inlet Temperature",
                                OutputProcessor::Unit::C,
                                EnergySourceReport(EnergySourceNum).InletTemp,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
            SetupOutputVariable("District Cooling Outlet Temperature",
                                OutputProcessor::Unit::C,
                                EnergySourceReport(EnergySourceNum).OutletTemp,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
            SetupOutputVariable("District Cooling Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                EnergySourceReport(EnergySourceNum).MassFlowRate,
                                "System",
                                "Average",
                                EnergySource(EnergySourceNum).Name);
        }
    }

    // End of Get Input subroutines for the OutsideEnergySources Module
    //******************************************************************************

    // Beginning Initialization Section of the OutsideEnergySources Module
    //******************************************************************************

    void InitSimVars(int const EnergySourceNum, // Which item being initialized
                     Real64 &MassFlowRate,
                     Real64 &InletTemp,
                     Real64 &OutletTemp,
                     Real64 const MyLoad)
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

        // Using/Aliasing
        using DataGlobals::BeginEnvrnFlag;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using PlantUtilities::SetComponentFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TempTypeFlag(0);
        static Real64 TempPlantMdot(0.0); // local copy of plant flow
        int LoopNum;
        int LoopSideNum;
        int BranchIndex;
        int CompIndex;
        int InletNode;
        int OutletNode;
        bool errFlag;
        // Init more variables

        if (EnergySource(EnergySourceNum).OneTimeInitFlag) {
            if (EnergySource(EnergySourceNum).EnergyType == EnergyType_DistrictHeating) {
                TempTypeFlag = TypeOf_PurchHotWater;
            } else if (EnergySource(EnergySourceNum).EnergyType == EnergyType_DistrictCooling) {
                TempTypeFlag = TypeOf_PurchChilledWater;
            } else {
                ShowFatalError("InitSimVars: Invalid EnergyType for District Heating/Cooling=" + EnergySource(EnergySourceNum).Name);
            }
            // Locate the unit on the plant loops for later usage
            errFlag = false;
            ScanPlantLoopsForObject(EnergySource(EnergySourceNum).Name,
                                    TempTypeFlag,
                                    EnergySource(EnergySourceNum).LoopNum,
                                    EnergySource(EnergySourceNum).LoopSideNum,
                                    EnergySource(EnergySourceNum).BranchNum,
                                    EnergySource(EnergySourceNum).CompNum,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _,
                                    errFlag);
            if (errFlag) {
                ShowFatalError("InitSimVars: Program terminated due to previous condition(s).");
            }
            // set limits on outlet node temps to plant loop limits
            PlantLoop(EnergySource(EnergySourceNum).LoopNum)
                .LoopSide(EnergySource(EnergySourceNum).LoopSideNum)
                .Branch(EnergySource(EnergySourceNum).BranchNum)
                .Comp(EnergySource(EnergySourceNum).CompNum)
                .MinOutletTemp = PlantLoop(EnergySource(EnergySourceNum).LoopNum).MinTemp;
            PlantLoop(EnergySource(EnergySourceNum).LoopNum)
                .LoopSide(EnergySource(EnergySourceNum).LoopSideNum)
                .Branch(EnergySource(EnergySourceNum).BranchNum)
                .Comp(EnergySource(EnergySourceNum).CompNum)
                .MaxOutletTemp = PlantLoop(EnergySource(EnergySourceNum).LoopNum).MaxTemp;
            // Register design flow rate for inlet node (helps to autosize comp setpoint op scheme flows
            RegisterPlantCompDesignFlow(EnergySource(EnergySourceNum).InletNodeNum, PlantLoop(EnergySource(EnergySourceNum).LoopNum).MaxVolFlowRate);

            EnergySource(EnergySourceNum).OneTimeInitFlag = false;
        }

        // begin environment inits
        if (BeginEnvrnFlag && EnergySource(EnergySourceNum).BeginEnvrnInitFlag) {
            // component model has not design flow rates, using data for overall plant loop
            InitComponentNodes(PlantLoop(EnergySource(EnergySourceNum).LoopNum).MinMassFlowRate,
                               PlantLoop(EnergySource(EnergySourceNum).LoopNum).MaxMassFlowRate,
                               EnergySource(EnergySourceNum).InletNodeNum,
                               EnergySource(EnergySourceNum).OutletNodeNum,
                               EnergySource(EnergySourceNum).LoopNum,
                               EnergySource(EnergySourceNum).LoopSideNum,
                               EnergySource(EnergySourceNum).BranchNum,
                               EnergySource(EnergySourceNum).CompNum);
            EnergySource(EnergySourceNum).BeginEnvrnInitFlag = false;
        }
        if (!BeginEnvrnFlag) EnergySource(EnergySourceNum).BeginEnvrnInitFlag = true;

        // now do everytime inits
        InletNode = EnergySource(EnergySourceNum).InletNodeNum;
        OutletNode = EnergySource(EnergySourceNum).OutletNodeNum;
        InletTemp = Node(InletNode).Temp;
        OutletTemp = InletTemp;
        LoopNum = EnergySource(EnergySourceNum).LoopNum;
        LoopSideNum = EnergySource(EnergySourceNum).LoopSideNum;
        BranchIndex = EnergySource(EnergySourceNum).BranchNum;
        CompIndex = EnergySource(EnergySourceNum).CompNum;

        if (std::abs(MyLoad) > 0.0) {
            TempPlantMdot = PlantLoop(LoopNum).MaxMassFlowRate;
        } else {
            TempPlantMdot = 0.0; // expect no flow needed
        }

        // get actual mass flow to use, hold in MassFlowRate variable
        SetComponentFlowRate(TempPlantMdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex);
        MassFlowRate = TempPlantMdot;
    }

    // End Initialization Section of the OutsideEnergySources Module
    //******************************************************************************

    // Beginning of OutsideEnergySources Module Utility Subroutines
    // *****************************************************************************

    void SizeDistrictEnergy(int const EnergySourceNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   April 2014
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for sizing capacities of district cooling and heating objects.

        // USE STATEMENTS:
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;
        using DataSizing::AutoSize;
        using DataSizing::AutoVsHardSizingThreshold;
        using DataSizing::PlantSizData;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using ReportSizingManager::ReportSizingOutput;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum(0);        // Plant sizing index for hot water loop
        bool ErrorsFound(false); // If errors detected in input
        Real64 NomCapDes(0.0);   // Autosized nominal capacity for reporting
        Real64 NomCapUser(0.0);  // Hardsized nominal capacity for reporting
        Real64 rho(0.0);         // Density (kg/m3)
        Real64 Cp(0.0);          // Specific Heat (J/kg-K)

        // Type name string variable to collapse the sizing for cooling and heating into one block
        std::string typeName;
        if (EnergySource(EnergySourceNum).EnergyType == EnergyType_DistrictCooling) {
            typeName = "Cooling";
        } else { // Heating
            typeName = "Heating";
        }

        // Do the sizing here

        PltSizNum = PlantLoop(EnergySource(EnergySourceNum).LoopNum).PlantSizNum;
        if (PltSizNum > 0) {
            rho = GetDensityGlycol(PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidIndex,
                                   "SizeDistrict" + typeName);
            Cp = GetSpecificHeatGlycol(PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       PlantLoop(EnergySource(EnergySourceNum).LoopNum).FluidIndex,
                                       "SizeDistrict" + typeName);
            NomCapDes = Cp * rho * PlantSizData(PltSizNum).DeltaT * PlantSizData(PltSizNum).DesVolFlowRate;
            if (PlantFirstSizesOkayToFinalize) {
                if (EnergySource(EnergySourceNum).NomCapWasAutoSized) {
                    EnergySource(EnergySourceNum).NomCap = NomCapDes;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput("District" + typeName, EnergySource(EnergySourceNum).Name, "Design Size Nominal Capacity [W]", NomCapDes);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(
                            "District" + typeName, EnergySource(EnergySourceNum).Name, "Initial Design Size Nominal Capacity [W]", NomCapDes);
                    }
                } else { // Hard-size with sizing data
                    if (EnergySource(EnergySourceNum).NomCap > 0.0 && NomCapDes > 0.0) {
                        NomCapUser = EnergySource(EnergySourceNum).NomCap;
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput("District" + typeName,
                                               EnergySource(EnergySourceNum).Name,
                                               "Design Size Nominal Capacity [W]",
                                               NomCapDes,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(NomCapDes - NomCapUser) / NomCapUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeDistrict" + typeName + ": Potential issue with equipment sizing for " +
                                                EnergySource(EnergySourceNum).Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + RoundSigDigits(NomCapDes, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            if (EnergySource(EnergySourceNum).NomCapWasAutoSized && PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of District " + typeName + " nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in District" + typeName + " object=" + EnergySource(EnergySourceNum).Name);
                ErrorsFound = true;
            }
            if (!EnergySource(EnergySourceNum).NomCapWasAutoSized && EnergySource(EnergySourceNum).NomCap > 0.0 && PlantFinalSizesOkayToReport) {
                ReportSizingOutput("District" + typeName,
                                   EnergySource(EnergySourceNum).Name,
                                   "User-Specified Nominal Capacity [W]",
                                   EnergySource(EnergySourceNum).NomCap);
            }
        }
        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void SimDistrictEnergy(
        bool const RunFlag, int const DistrictEqNum, Real64 &MyLoad, Real64 const MassFlowRate, Real64 const InletTemp, Real64 &OutletTemp)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   July 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine needs a description.

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // REFERENCES:
        // na

        // Using/Aliasing
        using FluidProperties::GetSpecificHeatGlycol;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SimDistrictEnergy");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int LoopNum;
        int LoopSideNum;
        int BranchIndex;
        int CompIndex;
        int InletNode;
        int OutletNode;
        Real64 LoopMinTemp;
        Real64 LoopMaxTemp;
        Real64 Cp; // local cp at current temp
        Real64 CurrentCap;
        Real64 CapFraction;

        // FLOW

        // set inlet and outlet nodes
        InletNode = EnergySource(DistrictEqNum).InletNodeNum;
        OutletNode = EnergySource(DistrictEqNum).OutletNodeNum;
        LoopNum = EnergySource(DistrictEqNum).LoopNum;
        LoopSideNum = EnergySource(DistrictEqNum).LoopSideNum;
        BranchIndex = EnergySource(DistrictEqNum).BranchNum;
        CompIndex = EnergySource(DistrictEqNum).CompNum;
        LoopMinTemp = PlantLoop(LoopNum).MinTemp;
        LoopMaxTemp = PlantLoop(LoopNum).MaxTemp;

        Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum).FluidName, InletTemp, PlantLoop(LoopNum).FluidIndex, RoutineName);

        //  apply power limit from input
        CapFraction = GetCurrentScheduleValue(EnergySource(DistrictEqNum).CapFractionSchedNum);
        CapFraction = max(0.0, CapFraction); // ensure non negative
        CurrentCap = EnergySource(DistrictEqNum).NomCap * CapFraction;
        if (std::abs(MyLoad) > CurrentCap) {
            MyLoad = sign(CurrentCap, MyLoad);
        }

        if (EnergySource(DistrictEqNum).EnergyType == EnergyType_DistrictCooling) {
            if (MyLoad > 0.0) MyLoad = 0.0;
        } else if (EnergySource(DistrictEqNum).EnergyType == EnergyType_DistrictHeating) {
            if (MyLoad < 0.0) MyLoad = 0.0;
        }

        // determine outlet temp based on inlet temp, cp, and MyLoad
        if ((MassFlowRate > 0.0) && RunFlag) {
            OutletTemp = (MyLoad + MassFlowRate * Cp * InletTemp) / (MassFlowRate * Cp);
            // apply loop limits on temperature result to keep in check
            if (OutletTemp < LoopMinTemp) {
                OutletTemp = max(OutletTemp, LoopMinTemp);
                MyLoad = MassFlowRate * Cp * (OutletTemp - InletTemp);
            }
            if (OutletTemp > LoopMaxTemp) {
                OutletTemp = min(OutletTemp, LoopMaxTemp);
                MyLoad = MassFlowRate * Cp * (OutletTemp - InletTemp);
            }
        } else {
            OutletTemp = InletTemp;
            MyLoad = 0.0;
        }
    }

    // End of OutsideEnergySources Module Utility Subroutines
    // *****************************************************************************

    // Beginning of Record Keeping subroutines for the OutsideEnergySources Module
    // *****************************************************************************

    void UpdateRecords(Real64 const MyLoad, int const EqNum, Real64 const MassFlowRate, Real64 const OutletTemp)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
        //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine needs a description.

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNode;
        int OutletNode;

        // set inlet and outlet nodes
        InletNode = EnergySource(EqNum).InletNodeNum;
        OutletNode = EnergySource(EqNum).OutletNodeNum;
        Node(OutletNode).Temp = OutletTemp;
        EnergySourceReport(EqNum).MassFlowRate = MassFlowRate;
        EnergySourceReport(EqNum).InletTemp = Node(InletNode).Temp;
        EnergySourceReport(EqNum).OutletTemp = OutletTemp;
        EnergySource(EqNum).EnergyRate = std::abs(MyLoad);
        EnergySource(EqNum).EnergyTransfer = EnergySource(EqNum).EnergyRate * TimeStepSys * SecInHour;
        EnergySourceReport(EqNum).EnergyTransfer = EnergySource(EqNum).EnergyTransfer;
    }

    // End of Record Keeping subroutines for the OutsideEnergySources Module
    // *****************************************************************************

} // namespace OutsideEnergySources

} // namespace EnergyPlus
