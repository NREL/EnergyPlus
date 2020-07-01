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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataConversions.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/IndoorIceRink.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {
namespace IceRink {
    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // System types:
    static std::string const BlankString;
    std::string const cRink("IceRink:Indoor");
    std::string const cResurfacer("IceRink:Resurfacer");

    Real64 InletTemp;
    Real64 OutletTemp;
    Real64 FlowRate;
    Real64 HeatFlux;

    // Control types:
    int const SurfaceTempControl(1);     // Controls system using ice surface temperature
    int const BrineOutletTempControl(2); // Controls system using brine outlet temperature

    int NumOfRinks(0);
    int NumOfResurfacers(0);

    bool GetInput(true);

    // Object Data
    Array1D<IceRinkData> Rink;
    Array1D<ResurfacerData> Resurfacer;

    // Functions:
    void clear_state()
    {
        NumOfRinks = 0;
        GetInput = true;
        Rink.deallocate();
        Resurfacer.deallocate();
    }

    PlantComponent *IceRinkData::factory(std::string const &objectName)
    {
        // Process the input data for ice rinks if it hasn't been done yet
        if (GetInput) {
            GetIndoorIceRink();
            GetInput = false;
        }
        // Now look for this particular rink in the list
        for (auto &rink : Rink) {
            if (rink.Name == objectName) {
                return &rink;
            }
        }
        // If it is not found, fatal error
        ShowFatalError("IceRinkFactory: Error getting inputs for rink named: " + objectName);
        // Shut up the compiler
        return nullptr;
    }

    void IceRinkData::simulate(EnergyPlusData &EP_UNUSED(state),
                                       const PlantLocation &EP_UNUSED(calledFromLocation),
                                       bool const EP_UNUSED(FirstHVACIteration),
                                       Real64 &CurLoad,
                                       bool const RunFlag)
    {
        this->initialize();
        if (this->RinkType_Num == DataPlant::TypeOf_IceRink) {
            this->calculateIceRink(CurLoad);
        }
        this->update();
        this->report(RunFlag);
    }

    void GetIndoorIceRink()
    {
        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::ScheduleAlwaysOn;
        using DataHeatBalance::Zone;
        using namespace DataSurfaces;
        using FluidProperties::FindGlycol;
        using FluidProperties::FindRefrigerant;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;
        using UtilityRoutines::FindItemInList;
        using namespace DataLoopNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetIndoorIceRink: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool ErrorsFound(false); // Set to true if errors in input,
        int IOStatus;                   // Used in GetObjectItem
        int Item;                       // Item to be "gotten"
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call

        NumOfRinks = inputProcessor->getNumObjectsFound(cRink);

        if (NumOfRinks <= 0) ShowFatalError("No Rink objects found in input.");
        // GetInput = false;

        if (allocated(Rink)) Rink.deallocate();
        Rink.allocate(NumOfRinks);

        // Obtain all the user data related to rinks
        for (Item = 1; Item <= NumOfRinks; ++Item) {
            cCurrentModuleObject = "IceRink:Indoor";
            inputProcessor->getObjectItem(
                cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames);

            Rink(Item).Name = cAlphaArgs(1);
            Rink(Item).SchedName = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                Rink(Item).SchedPtr = ScheduleAlwaysOn;
            } else {
                Rink(Item).SchedPtr = GetScheduleIndex(cAlphaArgs(2));
                if (Rink(Item).SchedPtr == 0) {
                    ShowSevereError(cAlphaFieldNames(2) + " not found for " + cAlphaArgs(1));
                    ShowContinueError("Missing " + cAlphaFieldNames(2) + " is " + cAlphaArgs(2));
                    ErrorsFound = true;
                }
            }

            Rink(Item).ZoneName = cAlphaArgs(3);
            Rink(Item).ZonePtr = FindItemInList(cAlphaArgs(3), Zone);
            if (Rink(Item).ZonePtr == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).SurfaceName = cAlphaArgs(4);
            Rink(Item).SurfacePtr = FindItemInList(cAlphaArgs(4), Surface);
            if ((Rink(Item).SurfacePtr == 0) || (Surface(Rink(Item).SurfacePtr).Class != SurfaceClass_Floor) ||
                (Surface(Rink(Item).SurfacePtr).Class == SurfaceClass_Window) || (Surface(Rink(Item).SurfacePtr).PartOfVentSlabOrRadiantSurface) ||
                (!DataHeatBalance::Construct(Surface(Rink(Item).SurfacePtr).Construction).SourceSinkPresent)) {

                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Invalid Surface");
                ShowContinueError(cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                  "\" either is not defines as a floor or is defined as a window or is a part of ventilated slab or has no "
                                  "source/sink defines in it");

                ErrorsFound = true;
            }

            /*if (Surface(Rink(Item).SurfacePtr).Zone != Rink(Item).ZonePtr) {
                ShowSevereError("Surface referenced in " + cCurrentModuleObject +
                                " not in same zone as Refrigeration System, surface=" + Surface(Rink(Item).SurfacePtr).Name);
                ShowContinueError("Surface in Zone=" + Zone(Surface(Rink(Item).SurfacePtr).Zone).Name + " Direct refrigeration System in " +
                                  cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }*/

            Rink(Item).TubeDiameter = rNumericArgs(1);
            Rink(Item).TubeLength = rNumericArgs(2);

            if (UtilityRoutines::SameString(cAlphaArgs(5), "BOTC")) {
                Rink(Item).ControlStrategy = BrineOutletTempControl;
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "STC")) {
                Rink(Item).ControlStrategy = SurfaceTempControl;
            }

            Rink(Item).MaxRefrigMassFlow = rNumericArgs(3);
            Rink(Item).MinRefrigMassFlow = rNumericArgs(4);

            Rink(Item).RefrigInNode = cAlphaArgs(6);
            Rink(Item).InNode = GetOnlySingleNode(
                cAlphaArgs(6), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Unknown, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            if (Rink(Item).InNode == 0) {
                ShowSevereError("Invalid " + cAlphaFieldNames(6) + '=' + cAlphaArgs(6));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).RefrigOutNode = cAlphaArgs(7);
            Rink(Item).OutNode = GetOnlySingleNode(
                cAlphaArgs(7), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Unknown, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            if (Rink(Item).OutNode == 0) {
                ShowSevereError("Invalid " + cAlphaFieldNames(7) + '=' + cAlphaArgs(7));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }

            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(6), cAlphaArgs(7), "Refrigerant Nodes");

            Rink(Item).RefrigSetptSched = cAlphaArgs(8);
            Rink(Item).RefrigSetptSchedPtr = GetScheduleIndex(cAlphaArgs(8));
            if ((Rink(Item).RefrigSetptSchedPtr == 0) && (!lAlphaFieldBlanks(8))) {
                ShowSevereError(cAlphaFieldNames(8) + " not found: " + cAlphaArgs(8));
                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).IceSetptSched = cAlphaArgs(9);
            Rink(Item).IceSetptSchedPtr = GetScheduleIndex(cAlphaArgs(9));
            if ((Rink(Item).IceSetptSchedPtr == 0) && (!lAlphaFieldBlanks(9))) {
                ShowSevereError(cAlphaFieldNames(9) + " not found: " + cAlphaArgs(9));
                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).PeopleHeatGainSchedName = cAlphaArgs(10);
            Rink(Item).PeopleHeatGainSchedPtr = GetScheduleIndex(cAlphaArgs(10));
            if ((Rink(Item).PeopleHeatGainSchedPtr == 0) && (!lAlphaFieldBlanks(10))) {
                ShowSevereError(cAlphaFieldNames(10) + " not found: " + cAlphaArgs(10));
                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).PeopleSchedName = cAlphaArgs(11);
            Rink(Item).PeopleSchedPtr = GetScheduleIndex(cAlphaArgs(11));
            if ((Rink(Item).PeopleSchedPtr == 0) && (!lAlphaFieldBlanks(11))) {
                ShowSevereError(cAlphaFieldNames(11) + " not found: " + cAlphaArgs(11));
                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).MaxNumOfPeople = rNumericArgs(5);
            if (Rink(Item).MaxNumOfPeople < 0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with negative people.  This is not allowed.");
                ShowContinueError("The number of people has been reset to zero.");
                Rink(Item).MaxNumOfPeople = 0.0;
            }

            Rink(Item).LengthRink = rNumericArgs(6);
            if (Rink(Item).LengthRink <= 0.0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive rink length. This is not allowed");
                ShowContinueError("The rink length has been reset to 60.");
                Rink(Item).LengthRink = 60.0;
            }

            Rink(Item).WidthRink = rNumericArgs(7);
            if (Rink(Item).WidthRink <= 0.0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive rink width. This is not allowed");
                ShowContinueError("The rink width has been reset to 30.");
                Rink(Item).WidthRink = 30.0;
            }

            Rink(Item).DepthRink = rNumericArgs(8);
            if (Rink(Item).DepthRink <= 0.0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive rink depth. This is not allowed");
                ShowContinueError("The rink depth has been reset to 1.");
                Rink(Item).DepthRink = 1.0;
            }

            Rink(Item).IceThickness = rNumericArgs(9);
            if (Rink(Item).IceThickness <= 0.0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive ice thickness. This is not allowed");
                ShowContinueError("The rink ice thickness has been reset to 0.1.");
                Rink(Item).IceThickness = 0.0254;
            }

            Rink(Item).FloodWaterTemp = rNumericArgs(10);
            if (Rink(Item).FloodWaterTemp <= 0.0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive flood water temperature. This is not allowed");
                ShowContinueError("The floor water temperature has been reset to 15.0 C.");
                Rink(Item).FloodWaterTemp = 15.0;
            }
        }

        /* if (ErrorsFound) {
             ShowFatalError("Errors found in input.");
         }*/
    }

    void GetResurfacer()
    {
        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetIndoorIceRink: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool ErrorsFound(false); // Set to true if errors in input,
        int IOStatus;                   // Used in GetObjectItem
        int Item;                       // Item to be "gotten"
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call

        NumOfResurfacers = inputProcessor->getNumObjectsFound(cResurfacer);

        if (allocated(Resurfacer)) Resurfacer.deallocate();
        Resurfacer.allocate(NumOfResurfacers);

        for (Item = 1; Item <= NumOfResurfacers; ++Item) {
            cCurrentModuleObject = "IceRink:Resurfacer";

            inputProcessor->getObjectItem(
                cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames);

            Resurfacer(Item).Name = cAlphaArgs(1);
            Resurfacer(Item).ResurfacingSchedName = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                Resurfacer(Item).ResurfacingSchedPtr = 0;
            } else {
                Resurfacer(Item).ResurfacingSchedPtr = GetScheduleIndex(cAlphaArgs(2));
            }

            Resurfacer(Item).NoOfResurfEvents = rNumericArgs(1);
            if (lNumericFieldBlanks(1)) {
                Resurfacer(Item).NoOfResurfEvents = 1;
            }
            if (Resurfacer(Item).NoOfResurfEvents < 0) {
                ShowSevereError(cNumericFieldNames(1) + "not found for " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            Resurfacer(Item).ResurfacingWaterTemp = rNumericArgs(2);
            if (Resurfacer(Item).ResurfacingWaterTemp <= 0.0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive resurfacing water temperature. This is not allowed");
                ShowContinueError("The resurfacing water temperature has been reset to 15.0.");
                Resurfacer(Item).ResurfacingWaterTemp = 15.0;
            }

            Resurfacer(Item).InitWaterTemp = rNumericArgs(3);
            Resurfacer(Item).TankCapacity = rNumericArgs(4);
            if (Resurfacer(Item).TankCapacity <= 0) {
                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                 " was entered with zero or negetive resurfacer tank capacity. This is not allowed");
                ShowContinueError("The resurfacer tank capacity has been reset to 3.0.");
                Resurfacer(Item).ResurfacingWaterTemp = 3.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in input.");
        }
    }

    void IceRinkData::initialize()
    {
        // Using/Aliasing
        using DataGlobals::WarmupFlag;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const DesignVelocity(0.5); // Hypothetical design max pipe velocity [m/s]
        static std::string const RoutineName("initialize");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool errFlag;

        if ((this->MyFlag) && allocated(DataPlant::PlantLoop)) {
            errFlag = false;
            this->setupOutputVariables();
            PlantUtilities::ScanPlantLoopsForObject(
                this->Name, DataPlant::TypeOf_IceRink, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum, errFlag, _, _, _, _, _);
            if (errFlag) {
                ShowFatalError("Initialize: Program terminated due to previous condition(s).");
            }
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                           DataPrecisionGlobals::constant_zero,
                                                           DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                           RoutineName);
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                               DataPrecisionGlobals::constant_zero,
                                                               DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                               RoutineName);
            this->DesignMassFlowRate = DataGlobals::Pi / 4.0 * pow_2(this->TubeDiameter) * DesignVelocity * rho * this->NumCircuits;
            this->DesignCapacity = this->DesignMassFlowRate * Cp * 10.0; // assume 10C delta T?
            PlantUtilities::InitComponentNodes(
                0.0, this->DesignMassFlowRate, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
            PlantUtilities::RegisterPlantCompDesignFlow(this->InNode, this->DesignMassFlowRate / rho);

            this->MyFlag = false;
        }

        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag) {
            this->QSrc = 0.0;
            this->QSrcAvg = 0.0;
            this->LastQSrc = 0.0;
            this->LastSysTimeElapsed = 0.0;
            this->LastTimeStepSys = 0.0;
        }

        if (!(DataGlobals::BeginEnvrnFlag)) this->MyEnvrnFlag = true;

        this->TotalSurfaceArea = this->LengthRink * this->WidthRink;
        InletTemp = DataLoopNode::Node(this->InNode).Temp;
        OutletTemp = DataLoopNode::Node(this->OutNode).Temp;
        FlowRate = DataLoopNode::Node(this->InNode).MassFlowRate;
    }

    void IceRinkData::setupOutputVariables()
    {
        // Set up output variables CurrentModuleObject='IceRink:Indoor'
        if (this->RinkType_Num == DataPlant::TypeOf_IceRink) {
            SetupOutputVariable(
                "Rink Refrigerant Inlet Temperature", OutputProcessor::Unit::C, this->RefrigInletTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                "Rink Refrigerant Outlet Temperature", OutputProcessor::Unit::C, this->RefrigOutletTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                "Rink Refrigerant Mass Flow Rate", OutputProcessor::Unit::kg_s, this->RefrigMassFlow, "System", "Average", this->Name);
            SetupOutputVariable("Rink Cooling Rate", OutputProcessor::Unit::W, this->CoolPower, "System", "Average", this->Name);
            SetupOutputVariable("Rink Cooling Energy", OutputProcessor::Unit::J, this->CoolEnergy, "System", "Average", this->Name);
        }
        for (auto &R : Resurfacer) {
            SetupOutputVariable("Resurfacer heat rate", OutputProcessor::Unit::J, R.QResurfacing, "System", "Average", this->Name);
        }
    }

    Real64 IceRinkData::PeopleHG()
    {
        static std::string const RoutineName("PeopleHG");

        this->PeopleHeatGain = ScheduleManager::GetCurrentScheduleValue(this->PeopleHeatGainSchedPtr);
        if (this->PeopleHeatGain < 0.0) {
            ShowWarningError(RoutineName + ": Ice Rink =\"" + this->Name + " People Heat Gain Schedule =\"" + this->PeopleHeatGainSchedName +
                             " has a negative value.  This is not allowed.");
            ShowContinueError("The heat gain per person has been reset to zero.");
            this->PeopleHeatGain = 0.0;
        }

        this->NumOfPeople = ScheduleManager::GetCurrentScheduleValue(this->PeopleSchedPtr);
        if (this->NumOfPeople < 0.0) {
            ShowWarningError(RoutineName + ": Ice Rink =\"" + this->Name + " People Schedule =\"" + this->PeopleSchedName +
                             " has a negative value.  This is not allowed.");
            ShowContinueError("The number of people has been reset maximum people capacity.");
            this->NumOfPeople = this->MaxNumOfPeople;
        }

        this->TotalPeopleHG = this->PeopleHeatGain * this->NumOfPeople;

        return (this->TotalPeopleHG);
    }

    Real64 IceRinkData::IceRinkFreezing(Real64 &FreezingLoad)
    {
        Real64 QFusion(333550.00);
        Real64 CpIce(2108.00);
        static std::string const RoutineName("IceRinkFreezing");

        Real64 RhoWater = FluidProperties::GetDensityGlycol("WATER", this->FloodWaterTemp, this->WaterIndex, RoutineName);
        Real64 CpWater = FluidProperties::GetSpecificHeatGlycol("WATER", this->FloodWaterTemp, this->WaterIndex, RoutineName);
        Real64 Volume = this->LengthRink * this->WidthRink * this->IceThickness;

        if (this->IceSetptSchedPtr > 0) {
            Real64 QFreezing = 0.001 * RhoWater * Volume *
                               ((CpWater * FloodWaterTemp) + (QFusion) - (CpIce * ScheduleManager::GetCurrentScheduleValue(this->IceSetptSchedPtr)));
            FreezingLoad = QFreezing;
        }

        return (FreezingLoad);
    }

    Real64 ResurfacerData::RinkResurfacer(Real64 &ResurfacingLoad)
    {
        static std::string const RoutineName("IceRinkResurfacer");
        Real64 QFusion(333550.00);
        Real64 CpIce(2108.00);
        Real64 RhoWater = FluidProperties::GetDensityGlycol("WATER", this->ResurfacingWaterTemp, this->WaterIndex, RoutineName);
        Real64 CpWater = FluidProperties::GetSpecificHeatGlycol("WATER", this->ResurfacingWaterTemp, this->WaterIndex, RoutineName);
        QResurfacing = this->NoOfResurfEvents * 0.001 * RhoWater * this->TankCapacity *
                       ((CpWater * this->ResurfacingWaterTemp) + (QFusion) - (CpIce * this->IceTemperature));
        EHeatingWater = 0.001 * this->TankCapacity * RhoWater * CpWater * (this->ResurfacingWaterTemp - this->InitWaterTemp);
        ResurfacingLoad = QResurfacing;
        return (ResurfacingLoad);
    }

    Real64 IceRinkData::calcEffectiveness(Real64 const Temperature,    // Temperature of refrigerant entering the floor radiant system, in C))
                                          Real64 const RefrigMassFlow) // Mass flow rate of refrigerant in the floor radiant system, in kg/s
    {
        // Using/Aliasing
        using DataGlobals::NumOfTimeStepInHour;
        using DataGlobals::Pi;
        using DataPlant::PlantLoop;

        // Return value
        Real64 CalcEffectiveness; // Function return variable

        // FUNCTION PARAMETER DEFINITIONS:
        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        static std::string const RoutineName("IceRink:calcEffectiveness");
        Real64 const MaxExpPower(50.0);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 NuseltNum; // Nuselt number (dimensionless)

        Real64 SpecificHeat =
            FluidProperties::GetSpecificHeatGlycol(PlantLoop(this->LoopNum).FluidName, Temperature, PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        Real64 Conductivity =
            FluidProperties::GetConductivityGlycol(PlantLoop(this->LoopNum).FluidName, Temperature, PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        Real64 Viscosity =
            FluidProperties::GetViscosityGlycol(PlantLoop(this->LoopNum).FluidName, Temperature, PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        Real64 Density =
            FluidProperties::GetDensityGlycol(PlantLoop(this->LoopNum).FluidName, Temperature, PlantLoop(this->LoopNum).FluidIndex, RoutineName);

        // Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
        Real64 ReynoldsNum = 4.0 * RefrigMassFlow / (Pi * Viscosity * this->TubeDiameter * this->NumCircuits);

        Real64 PrantlNum = Viscosity * SpecificHeat / Conductivity;

        // Calculate the Nusselt number based on what flow regime one is in. h = (k)(Nu)/D
        if (ReynoldsNum >= MaxLaminarRe) { // Turbulent flow --> use Dittus-Boelter equation
            NuseltNum = 0.023 * std::pow(ReynoldsNum, 0.8) * std::pow(PrantlNum, 0.3);
        } else { // Laminar flow --> use constant surface temperature relation
            NuseltNum = 3.66;
        }

        Real64 NTU = Pi * Conductivity * NuseltNum * this->TubeLength / (RefrigMassFlow * SpecificHeat);

        if (NTU > MaxExpPower) {
            CalcEffectiveness = 1.0;
        } else {
            CalcEffectiveness = 1.0 - std::exp(-NTU);
        }

        return CalcEffectiveness;
    }

    void IceRinkData::calculateIceRink(Real64 &LoadMet)
    {
        // Using/Aliasing
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using DataSurfaces::HeatTransferModel_CTF;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        static std::string const RoutineName("IceRink:calculateDirectIceRink");
        Real64 FreezingLoad;
        Real64 ResurfacingLoad;
        int ZoneNum = this->ZonePtr;
        int ControlStrategy = this->ControlStrategy;
        int RefrigNodeIn = this->InNode;
        int SurfNum = this->SurfacePtr;

        if (RefrigNodeIn == 0) {
            ShowSevereError("Illegal inlet node for the refrigerant in the direct system");
            ShowFatalError("Preceding condition causes termination");
        }

        if (ControlStrategy == BrineOutletTempControl) {

            // Setting the set point temperature of refrigerant outlet
            if (this->RefrigSetptSchedPtr > 0) {
                this->RefrigSetptTemp = GetCurrentScheduleValue(this->RefrigSetptSchedPtr);
            } else {
                ShowSevereError("Illegal pointer to brine outlet control strategy");
                ShowFatalError("Preceding condition causes termination");
            }
        } else if (ControlStrategy == SurfaceTempControl) {

            // Setting the set point temperature of ice surface
            if (this->IceSetptSchedPtr > 0) {
                this->IceSetptTemp = GetCurrentScheduleValue(this->IceSetptSchedPtr);
            } else {
                ShowSevereError("Illegal pointer to surface temperature control strategy");
                ShowFatalError("Preceding condition causes termination");
            }
        } else {
            ShowSevereError("Illegal input for control strategy");
            ShowFatalError("Preceding condition causes termination");
        }

        Real64 RefrigMassFlow = Node(RefrigNodeIn).MassFlowRate;
        Real64 RefrigTempIn = Node(RefrigNodeIn).Temp;

        if (RefrigMassFlow <= 0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.

            this->QSrc = 0.0;
            Real64 ReqMassFlow = 0.0;
            SetComponentFlowRate(ReqMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
        } else {
            // Refrigerant mass flow rate is significant

            // Determine the heat exchanger "effectiveness"
            Real64 Effectiveness = calcEffectiveness(RefrigTempIn, RefrigMassFlow);
            int ConstrNum = DataSurfaces::Surface(SurfNum).Construction;
            Real64 CpRefrig =
                GetSpecificHeatGlycol(PlantLoop(this->LoopNum).FluidName, RefrigTempIn, PlantLoop(this->LoopNum).FluidIndex, RoutineName);

            Real64 Ca = RadSysTiHBConstCoef(SurfNum);
            Real64 Cb = RadSysTiHBToutCoef(SurfNum);
            Real64 Cc = RadSysTiHBQsrcCoef(SurfNum);

            Real64 Cd = RadSysToHBConstCoef(SurfNum);
            Real64 Ce = RadSysToHBTinCoef(SurfNum);
            Real64 Cf = RadSysToHBQsrcCoef(SurfNum);

            Real64 Cg = CTFTsrcConstPart(SurfNum);
            Real64 Ch = DataHeatBalance::Construct(ConstrNum).CTFTSourceQ(0);
            Real64 Ci = DataHeatBalance::Construct(ConstrNum).CTFTSourceIn(0);
            Real64 Cj = DataHeatBalance::Construct(ConstrNum).CTFTSourceOut(0);

            Real64 Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
            Real64 Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

            HeatFlux = (RefrigTempIn - Ck) / ((Cl / DataSurfaces::Surface(SurfNum).Area) + (1 / (Effectiveness * RefrigMassFlow * CpRefrig)));

            Real64 TSource = Ck + (Cl * HeatFlux);
            Real64 IceTemp = (Ca + (Cb * Cd) + HeatFlux * (Cc + (Cb * Cf))) / (1 - (Cb * Ce));
            Real64 QRadSysSourceMax = RefrigMassFlow * CpRefrig * (RefrigTempIn - TSource);

            if (ControlStrategy == BrineOutletTempControl) {
                // Finding the required mass flow rate so that the outlet temperature
                // becomes equal to the user defined refrigerant outlet temperature
                // for the given refrigerant inlet temperature.

                Real64 ReqMassFlow = (((Ck - RefrigTempIn) / (this->RefrigSetptTemp - RefrigTempIn)) - (1 / Effectiveness)) *
                                     (DataSurfaces::Surface(SurfNum).Area / (CpRefrig * Cl));

                Real64 TRefigOutCheck = RefrigTempIn - (HeatFlux) / RefrigMassFlow * CpRefrig;

                if (TRefigOutCheck <= (this->RefrigSetptTemp)) {
                    RefrigMassFlow = this->MinRefrigMassFlow;
                    SetComponentFlowRate(RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                } else {
                    if (ReqMassFlow >= this->MaxRefrigMassFlow) {
                        RefrigMassFlow = this->MaxRefrigMassFlow;
                        SetComponentFlowRate(
                            RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                    } else {
                        RefrigMassFlow = ReqMassFlow;
                        SetComponentFlowRate(
                            RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                    }
                }
            } else if (ControlStrategy == SurfaceTempControl) {
                // Finding heat thant needs to be extraced so that the
                // ice surface reaches the set point temperature.
                Real64 QSetPoint =
                    ((((1 - (Cb * Ce)) * this->IceSetptTemp) - Ca - (Cb * Cd)) / (Cc + (Cb * Cf))) * DataSurfaces::Surface(SurfNum).Area;

                Real64 ReqMassFlow = QSetPoint / ((Effectiveness * CpRefrig) * (RefrigTempIn - TSource));

                if (IceTemp <= this->IceSetptTemp) {
                    HeatFlux = 0.0;
                    RefrigMassFlow = 0.0;
                    SetComponentFlowRate(RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                } else {
                    if (QRadSysSourceMax <= QSetPoint) {
                        RefrigMassFlow = this->MaxRefrigMassFlow;
                        SetComponentFlowRate(
                            RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                    } else {
                        RefrigMassFlow = ReqMassFlow;
                        SetComponentFlowRate(
                            RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                    }
                }
            }
            this->RefrigMassFlow = RefrigMassFlow;
        }

        // "Temperature Comparision" cut-off
        // Check if QRadSysSource is positive i.e. it is actually heating the rink.
        // If so then the refrigeration system is doing opposite of its intension
        // and it needs to be shut down.

        if (HeatFlux >= 0.0) {
            RefrigMassFlow = 0.0;
            SetComponentFlowRate(RefrigMassFlow, this->InNode, this->OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
            this->RefrigMassFlow = RefrigMassFlow;
        }

        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(ZoneNum);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(ZoneNum);

        IceRinkFreezing(FreezingLoad);
        for (auto &R : Resurfacer) {
            R.RinkResurfacer(ResurfacingLoad);
        }
        LoadMet = FreezingLoad + ResurfacingLoad + HeatFlux;
    }

    void IceRinkData::update()
    {
        // Using/Aliasing
        using DataGlobals::TimeStepZone;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SafeCopyPlantNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("Update");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CpFluid; // Specific heat of working fluid
        int LoopNum;
        int LoopSideNum;

        this->QSrc = HeatFlux;

        LoopNum = this->LoopNum;
        LoopSideNum = this->LoopSide;

        if (PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock > 0) {
            if (this->LastSysTimeElapsed == SysTimeElapsed) {
                // Still iterating or reducing system time step, so subtract old values which were
                // not valid
                this->QSrcAvg -= this->LastQSrc * this->LastTimeStepSys / TimeStepZone;
            }

            // Update the running average and the "last" values with the current values of the appropriate variables
            this->QSrcAvg += this->QSrc * TimeStepSys / TimeStepZone;

            this->LastQSrc = HeatFlux;
            this->LastSysTimeElapsed = SysTimeElapsed;
            this->LastTimeStepSys = TimeStepSys;

            CpFluid = GetSpecificHeatGlycol(PlantLoop(this->LoopNum).FluidName, RefrigInletTemp, PlantLoop(this->LoopNum).FluidIndex, RoutineName);

            SafeCopyPlantNode(this->InNode, this->OutNode);
            // check for flow
            if ((CpFluid > 0.0) && (FlowRate > 0.0)) {
                Node(this->OutNode).Temp = RefrigInletTemp - this->TotalSurfaceArea * HeatFlux / (FlowRate * CpFluid);
                Node(this->OutNode).Enthalpy = Node(this->OutNode).Temp * CpFluid;
            }
        }
    }

    void IceRinkData::report(bool RunFlag)
    {
        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;

        this->RefrigInletTemp = Node(this->InNode).Temp;
        this->RefrigOutletTemp = Node(this->OutNode).Temp;
        this->RefrigMassFlow = Node(this->InNode).MassFlowRate;
        this->CoolEnergy = this->CoolPower * TimeStepSys * SecInHour;
    }
} // namespace IceRink
} // namespace EnergyPlus
