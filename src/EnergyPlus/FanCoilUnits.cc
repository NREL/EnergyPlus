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
#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SZVAVModel.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>

namespace EnergyPlus {

namespace FanCoilUnits {

    // Module containing the routines dealing with 2 and 4 pipe fan coil units

    // MODULE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2000
    //       MODIFIED       October 2003 (FSEC added cooling coil type)
    //                      June 2010    Arnaud Flament LBNL added 3-speed and variables-speed fan capacity control;
    //                                   outside air schedule; and removed coil water inlet node inputs
    //                      Sept 2010    Brent Griffith, plant upgrades for water coils, fluid properties
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms needed to simulate 2 and 4 pipe
    // fan coil units.

    // METHODOLOGY EMPLOYED:
    // Units are modeled as a collection of components: outside air mixer,
    // fan, heating coil and/or cooling coil plus an integrated control
    // algorithm that adjusts the hot or cold water flow to meet the zone
    // load. Or varies the air flow rate to meet the zone load. Or both.

    // REFERENCES: none

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace DataSizing;
    using DataHVACGlobals::ATMixer_InletSide;
    using DataHVACGlobals::ATMixer_SupplySide;
    using DataHVACGlobals::ATMixerExists;
    using DataHVACGlobals::cATMixerTypes;
    using DataHVACGlobals::cFanTypes;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataHVACGlobals::DualSetPointWithDeadBand;
    using DataHVACGlobals::SingleCoolingSetPoint;
    using DataHVACGlobals::SingleHeatCoolSetPoint;
    using DataHVACGlobals::SingleHeatingSetPoint;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;
    using namespace ScheduleManager;

    void SimFanCoilUnit(EnergyPlusData &state,
                        std::string_view CompName,   // name of the fan coil unit
                        int const ZoneNum,             // number of zone being served
                        int const ControlledZoneNum,   // index into ZoneEquipConfig array; may not be equal to ZoneNum
                        bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                        Real64 &PowerMet,              // Sensible power supplied (W)
                        Real64 &LatOutputProvided,     // Latent add/removal supplied by window AC (kg/s), dehumid = negative
                        int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2000
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages the simulation of a fan coil unit. Called from SimZone Equipment

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum; // index of fan coil unit being simulated

        // First time SimFanCoilUnit is called, get the input for all the fan coil units
        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        // Find the correct Fan Coil Equipment
        if (CompIndex == 0) {
            FanCoilNum = UtilityRoutines::FindItemInList(CompName, state.dataFanCoilUnits->FanCoil);
            if (FanCoilNum == 0) {
                ShowFatalError(state, "SimFanCoil: Unit not found=" + std::string{CompName});
            }
            CompIndex = FanCoilNum;
        } else {
            FanCoilNum = CompIndex;
            if (FanCoilNum > state.dataFanCoilUnits->NumFanCoils || FanCoilNum < 1) {
                ShowFatalError(state,
                               format("SimFanCoil:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      FanCoilNum,
                                      state.dataFanCoilUnits->NumFanCoils,
                                      CompName));
            }
            if (state.dataFanCoilUnits->CheckEquipName(FanCoilNum)) {
                if (CompName != state.dataFanCoilUnits->FanCoil(FanCoilNum).Name) {
                    ShowFatalError(state,
                                   format("SimFanCoil: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          FanCoilNum,
                                          CompName,
                                          state.dataFanCoilUnits->FanCoil(FanCoilNum).Name));
                }
                state.dataFanCoilUnits->CheckEquipName(FanCoilNum) = false;
            }
        }

        state.dataSize->ZoneEqFanCoil = true;

        // Initialize the fan coil unit
        InitFanCoilUnits(state, FanCoilNum, ZoneNum, ControlledZoneNum);

        // Select the correct unit type
        {
            auto const SELECT_CASE_var(state.dataFanCoilUnits->FanCoil(FanCoilNum).UnitType_Num);

            if (SELECT_CASE_var == FanCoilUnit_4Pipe) {

                Sim4PipeFanCoil(state, FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided);
            }
        }

        // Report the result of the simulation
        ReportFanCoilUnit(state, FanCoilNum);

        state.dataSize->ZoneEqFanCoil = false;
    }

    void GetFanCoilUnits(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2000
        //       MODIFIED       Bereket Nigusse, FSEC, April 2011: eliminated input node names
        //                                                         added OA Mixer object type
        //                                                         and fan object type
        //                      Chandan Sharma, FSEC, July 2012: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for fan coil units and stores it in fan coil data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using Fans::GetFanDesignVolumeFlowRate;
        using Fans::GetFanType;

        using NodeInputManager::GetOnlySingleNode;
        using WaterCoils::GetCoilWaterInletNode;
        auto &GetHXCoilWaterInletNode(HVACHXAssistedCoolingCoil::GetCoilWaterInletNode);
        auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);
        using DataHVACGlobals::FanType_SimpleConstVolume;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::FanType_SimpleVAV;
        using DataPlant::TypeOf_CoilWaterCooling;
        using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using HVACHXAssistedCoolingCoil::GetHXCoilTypeAndName;
        using MixedAir::GetOAMixerIndex;
        using MixedAir::GetOAMixerNodeNumbers;
        using SingleDuct::GetATMixer;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        static constexpr std::string_view RoutineName("GetFanCoilUnits: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilIndex;                // loop index
        int FanCoilNum;                  // current fan coil number
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        Array1D_int OANodeNums(4);       // Node numbers of Outdoor air mixer (OA, EA, RA, MA)
        int IOStatus;                    // Used in GetObjectItem
        bool IsNotOK;                    // Flag to verify name
        std::string CurrentModuleObject; // Object type for getting and error messages
        Array1D_string Alphas;           // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> Numbers;         // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        int CtrlZone;                    // index to loop counter
        int NodeNum;                     // index to loop counter
        std::string ATMixerName;

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);
        auto &ErrorsFound(state.dataFanCoilUnits->ErrorsFound);
        auto errFlag(state.dataFanCoilUnits->errFlag);

        // find the number of each type of fan coil unit

        CurrentModuleObject = state.dataFanCoilUnits->cMO_FanCoil;
        state.dataFanCoilUnits->Num4PipeFanCoils = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataFanCoilUnits->NumFanCoils = state.dataFanCoilUnits->Num4PipeFanCoils;
        // allocate the data structures
        state.dataFanCoilUnits->FanCoil.allocate(state.dataFanCoilUnits->NumFanCoils);
        state.dataFanCoilUnits->FanCoilNumericFields.allocate(state.dataFanCoilUnits->NumFanCoils);
        state.dataFanCoilUnits->CheckEquipName.dimension(state.dataFanCoilUnits->NumFanCoils, true);

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, CurrentModuleObject, state.dataFanCoilUnits->TotalArgs, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        Numbers.dimension(NumNumbers, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        // loop over 4 pipe fan coil units; get and load the input data
        for (FanCoilIndex = 1; FanCoilIndex <= state.dataFanCoilUnits->Num4PipeFanCoils; ++FanCoilIndex) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     FanCoilIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);

            FanCoilNum = FanCoilIndex;

            state.dataFanCoilUnits->FanCoilNumericFields(FanCoilNum).FieldNames.allocate(NumNumbers);
            state.dataFanCoilUnits->FanCoilNumericFields(FanCoilNum).FieldNames = "";
            state.dataFanCoilUnits->FanCoilNumericFields(FanCoilNum).FieldNames = cNumericFields;

            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
            FanCoil(FanCoilNum).Name = Alphas(1);
            FanCoil(FanCoilNum).UnitType = CurrentModuleObject;
            FanCoil(FanCoilNum).UnitType_Num = FanCoilUnit_4Pipe;
            FanCoil(FanCoilNum).Sched = Alphas(2);
            if (lAlphaBlanks(2)) {
                FanCoil(FanCoilNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                FanCoil(FanCoilNum).SchedPtr = GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer
                if (FanCoil(FanCoilNum).SchedPtr == 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid");
                    ShowContinueError(state, "invalid-not found: " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                    ErrorsFound = true;
                }
            }

            if (UtilityRoutines::SameString(Alphas(3), "ConstantFanVariableFlow") || UtilityRoutines::SameString(Alphas(3), "CyclingFan") ||
                UtilityRoutines::SameString(Alphas(3), "VariableFanVariableFlow") ||
                UtilityRoutines::SameString(Alphas(3), "VariableFanConstantFlow") || UtilityRoutines::SameString(Alphas(3), "MultiSpeedFan") ||
                UtilityRoutines::SameString(Alphas(3), "ASHRAE90VariableFan")) {
                FanCoil(FanCoilNum).CapCtrlMeth = Alphas(3);
                if (UtilityRoutines::SameString(Alphas(3), "ConstantFanVariableFlow")) FanCoil(FanCoilNum).CapCtrlMeth_Num = CCM::ConsFanVarFlow;
                if (UtilityRoutines::SameString(Alphas(3), "CyclingFan")) FanCoil(FanCoilNum).CapCtrlMeth_Num = CCM::CycFan;
                if (UtilityRoutines::SameString(Alphas(3), "VariableFanVariableFlow")) FanCoil(FanCoilNum).CapCtrlMeth_Num = CCM::VarFanVarFlow;
                ;
                if (UtilityRoutines::SameString(Alphas(3), "VariableFanConstantFlow")) FanCoil(FanCoilNum).CapCtrlMeth_Num = CCM::VarFanConsFlow;
                if (UtilityRoutines::SameString(Alphas(3), "MultiSpeedFan")) FanCoil(FanCoilNum).CapCtrlMeth_Num = CCM::MultiSpeedFan;
                if (UtilityRoutines::SameString(Alphas(3), "ASHRAE90VariableFan")) {
                    FanCoil(FanCoilNum).CapCtrlMeth_Num = CCM::ASHRAE;
                    FanCoil(FanCoilNum).DesZoneCoolingLoad = AutoSize;
                    FanCoil(FanCoilNum).DesZoneHeatingLoad = AutoSize;
                    FanCoil(FanCoilNum).FanOpMode = ContFanCycCoil;
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\", invalid");
                ShowContinueError(state, "illegal value: " + cAlphaFields(3) + "=\"" + Alphas(3) + "\".");
                ErrorsFound = true;
            }

            FanCoil(FanCoilNum).SchedOutAir = Alphas(4);
            FanCoil(FanCoilNum).SchedOutAirPtr = GetScheduleIndex(state, Alphas(4)); // convert schedule name to pointer
            if (FanCoil(FanCoilNum).SchedOutAirPtr == 0 && (!lAlphaBlanks(4))) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\", invalid");
                ShowContinueError(state, "illegal value: " + cAlphaFields(4) + "=\"" + Alphas(4) + "\".");
                ErrorsFound = true;
            }
            FanCoil(FanCoilNum).MaxAirVolFlow = Numbers(1);
            FanCoil(FanCoilNum).LowSpeedRatio = Numbers(2);
            FanCoil(FanCoilNum).MedSpeedRatio = Numbers(3);
            // check if low speed ratio < medium speed ratio, if not : warning & set to default values
            if (FanCoil(FanCoilNum).LowSpeedRatio > FanCoil(FanCoilNum).MedSpeedRatio) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\",");
                ShowContinueError(state, "... " + cNumericFields(2) + " is greater than the medium speed supply air flow ratio.");
                ShowContinueError(state, format("... Fan Coil Unit low speed supply air flow ratio = {:.5T} ", FanCoil(FanCoilNum).LowSpeedRatio));
                ShowContinueError(state, format("... Fan Coit Unit medium speed supply air flow ratio = {:.5T} ", FanCoil(FanCoilNum).MedSpeedRatio));
                ShowContinueError(state,
                                  "... Fan Coil Unit low speed supply air flow ratio and medium speed supply air flow ratio set to default values");
                FanCoil(FanCoilNum).LowSpeedRatio = 1.0 / 3.0;
                FanCoil(FanCoilNum).MedSpeedRatio = 2.0 / 3.0;
            }

            FanCoil(FanCoilNum).OutAirVolFlow = Numbers(4);

            FanCoil(FanCoilNum).AirInNode = GetOnlySingleNode(state,
                                                              Alphas(5),
                                                              ErrorsFound,
                                                              FanCoil(FanCoilNum).UnitType,
                                                              Alphas(1),
                                                              DataLoopNode::NodeFluidType::Air,
                                                              DataLoopNode::NodeConnectionType::Inlet,
                                                              1,
                                                              ObjectIsParent); // air input node

            FanCoil(FanCoilNum).AirOutNode = GetOnlySingleNode(state,
                                                               Alphas(6),
                                                               ErrorsFound,
                                                               FanCoil(FanCoilNum).UnitType,
                                                               Alphas(1),
                                                               DataLoopNode::NodeFluidType::Air,
                                                               DataLoopNode::NodeConnectionType::Outlet,
                                                               1,
                                                               ObjectIsParent); // air outlet node

            FanCoil(FanCoilNum).OAMixType = Alphas(7);
            FanCoil(FanCoilNum).OAMixName = Alphas(8);
            // check to see if local OA mixer specified
            if (!lAlphaBlanks(8)) {
                errFlag = false;
                ValidateComponent(state, FanCoil(FanCoilNum).OAMixType, FanCoil(FanCoilNum).OAMixName, errFlag, CurrentModuleObject);
                if (errFlag) {
                    ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name + "\".");
                    ErrorsFound = true;
                } else {
                    // Get outdoor air mixer node numbers
                    OANodeNums = GetOAMixerNodeNumbers(state, FanCoil(FanCoilNum).OAMixName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "that was specified in " + CurrentModuleObject + " = " + FanCoil(FanCoilNum).Name);
                        ShowContinueError(state, "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.");
                        ErrorsFound = true;
                    } else {
                        FanCoil(FanCoilNum).OutsideAirNode = OANodeNums(1);
                        FanCoil(FanCoilNum).AirReliefNode = OANodeNums(2);
                        FanCoil(FanCoilNum).MixedAirNode = OANodeNums(4);
                    }
                }
            }

            FanCoil(FanCoilNum).CCoilName = Alphas(12);
            FanCoil(FanCoilNum).MaxColdWaterVolFlow = Numbers(5);
            FanCoil(FanCoilNum).MinColdWaterVolFlow = Numbers(6);
            FanCoil(FanCoilNum).ColdControlOffset = Numbers(7);
            FanCoil(FanCoilNum).HCoilName = Alphas(14);
            FanCoil(FanCoilNum).HCoilType = Alphas(13);
            FanCoil(FanCoilNum).MaxHotWaterVolFlow = Numbers(8);
            FanCoil(FanCoilNum).MinHotWaterVolFlow = Numbers(9);
            FanCoil(FanCoilNum).HotControlOffset = Numbers(10);

            if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:Water") ||
                UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:Water:DetailedGeometry") ||
                UtilityRoutines::SameString(Alphas(11), "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                FanCoil(FanCoilNum).CCoilType = Alphas(11);
                if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:Water")) {
                    FanCoil(FanCoilNum).CCoilType_Num = CCoil::Water;
                    FanCoil(FanCoilNum).CCoilPlantName = FanCoil(FanCoilNum).CCoilName;
                    FanCoil(FanCoilNum).CCoilPlantTypeOfNum = TypeOf_CoilWaterCooling;
                }
                if (UtilityRoutines::SameString(Alphas(11), "Coil:Cooling:Water:DetailedGeometry")) {
                    FanCoil(FanCoilNum).CCoilType_Num = CCoil::Detailed;
                    FanCoil(FanCoilNum).CCoilPlantName = FanCoil(FanCoilNum).CCoilName;
                    FanCoil(FanCoilNum).CCoilPlantTypeOfNum = TypeOf_CoilWaterDetailedFlatCooling;
                }
                if (UtilityRoutines::SameString(Alphas(11), "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                    FanCoil(FanCoilNum).CCoilType_Num = CCoil::HXAssist;
                    GetHXCoilTypeAndName(state,
                                         FanCoil(FanCoilNum).CCoilType,
                                         FanCoil(FanCoilNum).CCoilName,
                                         ErrorsFound,
                                         FanCoil(FanCoilNum).CCoilPlantType,
                                         FanCoil(FanCoilNum).CCoilPlantName);
                    if (UtilityRoutines::SameString(FanCoil(FanCoilNum).CCoilPlantType, "Coil:Cooling:Water")) {
                        FanCoil(FanCoilNum).CCoilPlantTypeOfNum = TypeOf_CoilWaterCooling;
                    } else if (UtilityRoutines::SameString(FanCoil(FanCoilNum).CCoilPlantType, "Coil:Cooling:Water:DetailedGeometry")) {
                        FanCoil(FanCoilNum).CCoilPlantTypeOfNum = TypeOf_CoilWaterDetailedFlatCooling;
                    } else {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\", invalid");
                        ShowContinueError(state, "For: " + cAlphaFields(11) + "=\"" + Alphas(11) + "\".");
                        ShowContinueError(state,
                                          "Invalid Coil Type=" + FanCoil(FanCoilNum).CCoilPlantType + ", Name=" + FanCoil(FanCoilNum).CCoilPlantName);
                        ShowContinueError(state, "must be \"Coil:Cooling:Water\" or \"Coil:Cooling:Water:DetailedGeometry\"");
                        ErrorsFound = true;
                    }
                }
                IsNotOK = false;
                ValidateComponent(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK, FanCoil(FanCoilNum).UnitType);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\".");
                    ErrorsFound = true;
                } else {
                    if (FanCoil(FanCoilNum).CCoilType_Num != CCoil::HXAssist) {
                        // mine the cold water node from the coil object
                        FanCoil(FanCoilNum).CoolCoilFluidInletNode =
                            GetCoilWaterInletNode(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK);
                        FanCoil(FanCoilNum).CoolCoilInletNodeNum =
                            WaterCoils::GetCoilInletNode(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK);
                        FanCoil(FanCoilNum).CoolCoilOutletNodeNum =
                            WaterCoils::GetCoilOutletNode(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK);
                    } else {
                        FanCoil(FanCoilNum).CoolCoilFluidInletNode =
                            GetHXCoilWaterInletNode(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK);
                        FanCoil(FanCoilNum).CoolCoilInletNodeNum =
                            HVACHXAssistedCoolingCoil::GetCoilInletNode(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK);
                        FanCoil(FanCoilNum).CoolCoilOutletNodeNum = HVACHXAssistedCoolingCoil::GetCoilOutletNode(
                            state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, IsNotOK);
                    }
                    // Other error checks should trap before it gets to this point in the code, but including just in case.
                    if (IsNotOK) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\", invalid");
                ShowContinueError(state, "illegal value: " + cAlphaFields(11) + "=\"" + Alphas(11) + "\".");
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Water")) {
                FanCoil(FanCoilNum).HCoilType_Num = HCoil::Water;
                FanCoil(FanCoilNum).HCoilPlantTypeOfNum = TypeOf_CoilWaterSimpleHeating;
                IsNotOK = false;
                ValidateComponent(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\".");
                    ErrorsFound = true;
                } else {
                    // mine the hot water node from the coil object
                    FanCoil(FanCoilNum).HeatCoilFluidInletNode =
                        GetCoilWaterInletNode(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, IsNotOK);
                    FanCoil(FanCoilNum).HeatCoilInletNodeNum =
                        WaterCoils::GetCoilInletNode(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, IsNotOK);
                    FanCoil(FanCoilNum).HeatCoilOutletNodeNum =
                        WaterCoils::GetCoilOutletNode(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
            } else if (UtilityRoutines::SameString(Alphas(13), "Coil:Heating:Electric")) {
                FanCoil(FanCoilNum).HCoilType_Num = HCoil::Electric;
                IsNotOK = false;
                ValidateComponent(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\".");
                    ErrorsFound = true;
                } else {
                    FanCoil(FanCoilNum).DesignHeatingCapacity =
                        GetHeatingCoilCapacity(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, errFlag);
                    FanCoil(FanCoilNum).HeatCoilInletNodeNum =
                        HeatingCoils::GetCoilInletNode(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, errFlag);
                    FanCoil(FanCoilNum).HeatCoilOutletNodeNum =
                        HeatingCoils::GetCoilOutletNode(state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + FanCoil(FanCoilNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\", invalid");
                ShowContinueError(state, "illegal value: " + cAlphaFields(13) + "=\"" + Alphas(13) + "\".");
                ErrorsFound = true;
            }

            FanCoil(FanCoilNum).FanType = Alphas(9);
            FanCoil(FanCoilNum).FanName = Alphas(10);

            if (!lAlphaBlanks(15)) {
                FanCoil(FanCoilNum).AvailManagerListName = Alphas(15);
            }

            FanCoil(FanCoilNum).HVACSizingIndex = 0;
            if (!lAlphaBlanks(16)) {
                FanCoil(FanCoilNum).HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(16), state.dataSize->ZoneHVACSizing);
                if (FanCoil(FanCoilNum).HVACSizingIndex == 0) {
                    ShowSevereError(state, cAlphaFields(16) + " = " + Alphas(16) + " not found.");
                    ShowContinueError(state, "Occurs in " + state.dataFanCoilUnits->cMO_FanCoil + " = " + FanCoil(FanCoilNum).Name);
                    ErrorsFound = true;
                }
            }

            errFlag = false;
            ValidateComponent(state, FanCoil(FanCoilNum).FanType, FanCoil(FanCoilNum).FanName, errFlag, CurrentModuleObject);
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name + "\".");
                ErrorsFound = true;
            } else {
                if (!UtilityRoutines::SameString(FanCoil(FanCoilNum).FanType, "Fan:SystemModel")) {
                    GetFanType(
                        state, FanCoil(FanCoilNum).FanName, FanCoil(FanCoilNum).FanType_Num, errFlag, CurrentModuleObject, FanCoil(FanCoilNum).Name);
                    // need to grab fan index here
                    // Fans::GetFanIndex(state, FanCoil(FanCoilNum).FanName, FanCoil(FanCoilNum).FanIndex, errFlag, FanCoil(FanCoilNum).FanType);
                    FanCoil(FanCoilNum).fanAvailSchIndex =
                        Fans::GetFanAvailSchPtr(state, FanCoil(FanCoilNum).FanType, FanCoil(FanCoilNum).FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + FanCoil(FanCoilNum).Name);
                        ErrorsFound = true;
                        errFlag = false;
                    }
                    {
                        auto const SELECT_CASE_var(FanCoil(FanCoilNum).FanType_Num);
                        if ((SELECT_CASE_var == FanType_SimpleConstVolume) || (SELECT_CASE_var == FanType_SimpleVAV) ||
                            (SELECT_CASE_var == FanType_SimpleOnOff)) {
                            // Get fan air volume flow rate
                            FanCoil(FanCoilNum).FanAirVolFlow =
                                GetFanDesignVolumeFlowRate(state, FanCoil(FanCoilNum).FanType, FanCoil(FanCoilNum).FanName, IsNotOK);
                            // Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
                            if (FanCoil(FanCoilNum).MaxAirVolFlow > FanCoil(FanCoilNum).FanAirVolFlow &&
                                FanCoil(FanCoilNum).FanAirVolFlow != AutoSize) {
                                ShowWarningError(state, std::string{RoutineName} + FanCoil(FanCoilNum).UnitType + ": " + FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, "... " + cNumericFields(1) + " is greater than the maximum fan flow rate.");
                                ShowContinueError(state, format("... Fan Coil Unit flow = {:.5T} m3/s.", FanCoil(FanCoilNum).MaxAirVolFlow));
                                ShowContinueError(state,
                                                  "... Fan = " + cFanTypes(FanCoil(FanCoilNum).FanType_Num) + ": " + FanCoil(FanCoilNum).FanName);
                                ShowContinueError(state, format("... Fan flow = {:.5T} m3/s.", FanCoil(FanCoilNum).FanAirVolFlow));
                                ShowContinueError(state,
                                                  "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.");
                                FanCoil(FanCoilNum).MaxAirVolFlow = FanCoil(FanCoilNum).FanAirVolFlow;
                            }

                            // Check that the fan type match with the capacity control method selected
                            if ((FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::ConsFanVarFlow &&
                                 (FanCoil(FanCoilNum).FanType_Num == FanType_SimpleVAV)) ||
                                (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::CycFan && FanCoil(FanCoilNum).FanType_Num != FanType_SimpleOnOff) ||
                                (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::VarFanVarFlow && FanCoil(FanCoilNum).FanType_Num != FanType_SimpleVAV) ||
                                (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::VarFanConsFlow &&
                                 FanCoil(FanCoilNum).FanType_Num != FanType_SimpleVAV)) {
                                ShowSevereError(state, std::string{RoutineName} + FanCoil(FanCoilNum).UnitType + ": " + FanCoil(FanCoilNum).Name);
                                ShowContinueError(state,
                                                  "...the fan type of the object : " + FanCoil(FanCoilNum).FanName +
                                                      " does not match with the capacity control method selected : " +
                                                      FanCoil(FanCoilNum).CapCtrlMeth + " please see I/O reference");
                                ShowContinueError(state, "...for ConstantFanVariableFlow a Fan:OnOff or Fan:ConstantVolume is valid.");
                                ShowContinueError(state, "...for CyclingFan a Fan:OnOff is valid.");
                                ShowContinueError(state, "...for VariableFanVariableFlow or VariableFanConstantFlow a Fan:VariableVolume is valid.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(state, CurrentModuleObject + " = \"" + Alphas(1) + "\"");
                            ShowContinueError(state, "Fan Type must be Fan:OnOff, Fan:ConstantVolume or Fan:VariableVolume.");
                            ErrorsFound = true;
                        }
                    }
                } else if (UtilityRoutines::SameString(FanCoil(FanCoilNum).FanType, "Fan:SystemModel")) {
                    FanCoil(FanCoilNum).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                    state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, FanCoil(FanCoilNum).FanName)); // call constructor
                    FanCoil(FanCoilNum).FanIndex = HVACFan::getFanObjectVectorIndex(state, FanCoil(FanCoilNum).FanName); // zero-based
                    FanCoil(FanCoilNum).fanAvailSchIndex = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->availSchedIndex;
                    FanCoil(FanCoilNum).FanAirVolFlow = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->designAirVolFlowRate;
                    // Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
                    if (FanCoil(FanCoilNum).MaxAirVolFlow > FanCoil(FanCoilNum).FanAirVolFlow && FanCoil(FanCoilNum).FanAirVolFlow != AutoSize) {
                        ShowWarningError(state, std::string{RoutineName} + FanCoil(FanCoilNum).UnitType + ": " + FanCoil(FanCoilNum).Name);
                        ShowContinueError(state, "... " + cNumericFields(1) + " is greater than the maximum fan flow rate.");
                        ShowContinueError(state, format("... Fan Coil Unit flow = {:.5T} m3/s.", FanCoil(FanCoilNum).MaxAirVolFlow));
                        ShowContinueError(state, "... Fan = " + cFanTypes(FanCoil(FanCoilNum).FanType_Num) + ": " + FanCoil(FanCoilNum).FanName);
                        ShowContinueError(state, format("... Fan flow = {:.5T} m3/s.", FanCoil(FanCoilNum).FanAirVolFlow));
                        ShowContinueError(state, "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.");
                        FanCoil(FanCoilNum).MaxAirVolFlow = FanCoil(FanCoilNum).FanAirVolFlow;
                    }

                    // check that for VariableFanVariableFlow or VariableFanConstantFlow that the fan speed control is continuous
                    if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::VarFanVarFlow || FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::VarFanConsFlow ||
                        FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::ASHRAE) { // then expect continuous speed control fan
                        if (state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->speedControl !=
                            HVACFan::FanSystem::SpeedControlMethod::Continuous) {
                            ShowSevereError(state, std::string{RoutineName} + FanCoil(FanCoilNum).UnitType + ": " + FanCoil(FanCoilNum).Name);
                            ShowContinueError(state,
                                              "...the fan type of the object : " + FanCoil(FanCoilNum).FanName +
                                                  " does not match with the capacity control method selected : " + FanCoil(FanCoilNum).CapCtrlMeth +
                                                  " please see I/O reference");
                            ShowContinueError(
                                state,
                                "...for VariableFanVariableFlow or VariableFanConstantFlow a Fan:SystemModel should have Continuous speed control.");
                            ErrorsFound = true;
                        }
                    }
                }
            }

            // check low speed fan ratio when using ASHRAE90.1 capacity control method
            if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::ASHRAE) {
                if (FanCoil(FanCoilNum).LowSpeedRatio > 0.5) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\",");
                    ShowContinueError(state, "... " + cNumericFields(2) + " is greater than the 50% of the supply air flow ratio.");
                    ShowContinueError(state,
                                      format("... Fan Coil Unit low speed supply air flow ratio = {:.5T} ", FanCoil(FanCoilNum).LowSpeedRatio));
                } else if (FanCoil(FanCoilNum).LowSpeedRatio == 0.0) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\",");
                    ShowContinueError(state, "... " + cNumericFields(2) + " is equal to 0.");
                    ShowContinueError(state, "... Fan Coil Unit low speed supply air flow ratio should be greater than 0 to comply with ASHRAE90.1.");
                    ShowContinueError(state, "... Fan Coil Unit low speed supply air flow ratio set to 0.5");
                    FanCoil(FanCoilNum).LowSpeedRatio = 0.5;
                }
            }

            // Set defaults for convergence tolerance
            if (FanCoil(FanCoilNum).ColdControlOffset <= 0.0) {
                FanCoil(FanCoilNum).ColdControlOffset = 0.001;
            }
            if (FanCoil(FanCoilNum).HotControlOffset <= 0.0) {
                FanCoil(FanCoilNum).HotControlOffset = 0.001;
            }

            // check for inlet side air mixer
            GetATMixer(state,
                       FanCoil(FanCoilNum).Name,
                       ATMixerName,
                       state.dataFanCoilUnits->ATMixerNum,
                       state.dataFanCoilUnits->ATMixerType,
                       state.dataFanCoilUnits->ATMixerPriNode,
                       state.dataFanCoilUnits->ATMixerSecNode,
                       state.dataFanCoilUnits->ATMixerOutNode,
                       FanCoil(FanCoilNum).AirOutNode);
            if (state.dataFanCoilUnits->ATMixerType == ATMixer_InletSide) {
                // save the air terminal mixer data in the fan coil data array
                FanCoil(FanCoilNum).ATMixerExists = true;
                FanCoil(FanCoilNum).ATMixerIndex = state.dataFanCoilUnits->ATMixerNum;
                FanCoil(FanCoilNum).ATMixerName = ATMixerName;
                FanCoil(FanCoilNum).ATMixerType = ATMixer_InletSide;
                FanCoil(FanCoilNum).ATMixerPriNode = state.dataFanCoilUnits->ATMixerPriNode;
                FanCoil(FanCoilNum).ATMixerSecNode = state.dataFanCoilUnits->ATMixerSecNode;
                FanCoil(FanCoilNum).ATMixerOutNode = state.dataFanCoilUnits->ATMixerOutNode;
                // check that fan coil doesn' have local outside air
                if (!lAlphaBlanks(8)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name +
                                        "\". Fan coil unit has local as well as central outdoor air specified");
                }
                // check that the air teminal mixer out node is the fan coil inlet node
                if (FanCoil(FanCoilNum).AirInNode != state.dataFanCoilUnits->ATMixerOutNode) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name +
                                        "\". Fan coil unit air inlet node name must be the same as an air terminal mixer outlet node name.");
                    ShowContinueError(state, "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:InletSideMixer object.");
                    ShowContinueError(state, "..Fan coil unit air inlet node name = " + state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).AirInNode));
                    ErrorsFound = true;
                }
                // check for supply side air terminal mixer
            } else if (state.dataFanCoilUnits->ATMixerType == ATMixer_SupplySide) {
                // save the air terminal mixer data in the fan coil data array
                FanCoil(FanCoilNum).ATMixerExists = true;
                FanCoil(FanCoilNum).ATMixerIndex = state.dataFanCoilUnits->ATMixerNum;
                FanCoil(FanCoilNum).ATMixerName = ATMixerName;
                FanCoil(FanCoilNum).ATMixerType = ATMixer_SupplySide;
                FanCoil(FanCoilNum).ATMixerPriNode = state.dataFanCoilUnits->ATMixerPriNode;
                FanCoil(FanCoilNum).ATMixerSecNode = state.dataFanCoilUnits->ATMixerSecNode;
                FanCoil(FanCoilNum).ATMixerOutNode = state.dataFanCoilUnits->ATMixerOutNode;
                // check that fan coil doesn' have local outside air
                if (!lAlphaBlanks(8)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name +
                                        "\". Fan coil unit has local as well as central outdoor air specified");
                }
                // check that the air teminal mixer secondary air inlet node is the fan coil outlet node
                if (FanCoil(FanCoilNum).AirOutNode != state.dataFanCoilUnits->ATMixerSecNode) {
                    ShowSevereError(
                        state,
                        CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name +
                            "\". Fan coil unit air outlet node name must be the same as the air terminal mixer secondary air inlet node name.");
                    ShowContinueError(
                        state, "..Air terminal mixer secondary inlet node name is specified in AirTerminal:SingleDuct:SupplySideMixer object.");
                    ShowContinueError(state, "..Fan coil unit air outlet node name = " + state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).AirOutNode));
                    ErrorsFound = true;
                }
                // no air terminal mixer; do the normal connectivity checks
            } else {
                // check that the fan coil inlet node is the same as one of the zone exhaust nodes
                state.dataFanCoilUnits->ZoneExNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                        if (FanCoil(FanCoilNum).AirInNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                            state.dataFanCoilUnits->ZoneExNodeNotFound = false;
                        }
                    }
                }
                if (state.dataFanCoilUnits->ZoneExNodeNotFound) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name +
                                        "\". Fan coil unit air inlet node name must be the same as a zone exhaust node name.");
                    ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(state, "..Fan coil unit air inlet node name = " + state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).AirInNode));
                    ErrorsFound = true;
                }
                // check that the fan coil outlet node is the same as one of the zone inlet nodes
                state.dataFanCoilUnits->ZoneInNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                        if (FanCoil(FanCoilNum).AirOutNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                            FanCoil(FanCoilNum).ControlZoneNum = CtrlZone;
                            FanCoil(FanCoilNum).NodeNumOfControlledZone = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ZoneNode;
                            state.dataFanCoilUnits->ZoneInNodeNotFound = false;
                        }
                    }
                }
                if (state.dataFanCoilUnits->ZoneInNodeNotFound) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + FanCoil(FanCoilNum).Name +
                                        "\". Fan coil unit air outlet node name must be the same as a zone inlet node name.");
                    ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(state, "..Fan coil unit air outlet node name = " + state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).AirOutNode));

                    ErrorsFound = true;
                }
            }
            if (FanCoil(FanCoilNum).CapCtrlMeth == "MULTISPEEDFAN") {
                if (!lAlphaBlanks(17)) {
                    FanCoil(FanCoilNum).FanOpModeSchedPtr = GetScheduleIndex(state, Alphas(17));
                    if (FanCoil(FanCoilNum).FanType_Num != FanType_SimpleOnOff &&
                        FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                        ShowSevereError(state, CurrentModuleObject + " = " + FanCoil(FanCoilNum).Name);
                        ShowContinueError(state, "For " + cAlphaFields(17) + " = " + Alphas(17));
                        ShowContinueError(state, "Illegal " + cAlphaFields(9) + " = " + Alphas(9));
                        ShowContinueError(state, "...fan operating schedule is allowed for on off or system model fan type only )");
                        ErrorsFound = true;
                    } else {
                        if (FanCoil(FanCoilNum).FanOpModeSchedPtr == 0) {
                            ShowSevereError(state, CurrentModuleObject + " = " + FanCoil(FanCoilNum).Name);
                            ShowContinueError(state, "Illegal " + cAlphaFields(17) + " = " + Alphas(17));
                            ErrorsFound = true;
                        }
                    }
                } else {
                    if (FanCoil(FanCoilNum).FanType_Num == FanType_SimpleOnOff ||
                        FanCoil(FanCoilNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        FanCoil(FanCoilNum).FanOpMode = CycFanCycCoil;
                    }
                }
            }

            if (!lNumericBlanks(11)) {
                FanCoil(FanCoilNum).DesignMinOutletTemp = Numbers(11);
                if (lNumericBlanks(12)) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\",");
                    ShowContinueError(state, "... " + cNumericFields(11) + " and " + cNumericFields(12) + " must be used in unison.");
                    ErrorsFound = true;
                }
            }

            if (!lNumericBlanks(12)) {
                FanCoil(FanCoilNum).DesignMaxOutletTemp = Numbers(12);
                if (FanCoil(FanCoilNum).DesignMinOutletTemp != AutoSize && FanCoil(FanCoilNum).DesignMaxOutletTemp != AutoSize) {
                    if (FanCoil(FanCoilNum).DesignMaxOutletTemp < FanCoil(FanCoilNum).DesignMinOutletTemp) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\",");
                        ShowContinueError(state, "... " + cNumericFields(11) + " is greater than " + cNumericFields(12) + ".");
                        ShowContinueError(state, format("... {} = {:.2T} [C].", cNumericFields(11), FanCoil(FanCoilNum).DesignMinOutletTemp));
                        ShowContinueError(state, format("... {} = {:.2T} [C].", cNumericFields(12), FanCoil(FanCoilNum).DesignMaxOutletTemp));
                        ErrorsFound = true;
                    }
                }
                if (lNumericBlanks(11)) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + FanCoil(FanCoilNum).Name + "\",");
                    ShowContinueError(state, "... " + cNumericFields(11) + " and " + cNumericFields(12) + " must be used in unison.");
                    ErrorsFound = true;
                }
            }

            if (FanCoil(FanCoilNum).DesignMinOutletTemp > 0.0 && FanCoil(FanCoilNum).DesignMaxOutletTemp > 0.0) {
                FanCoil(FanCoilNum).ASHRAETempControl = true;
            } else if (FanCoil(FanCoilNum).DesignMinOutletTemp == AutoSize || FanCoil(FanCoilNum).DesignMaxOutletTemp == AutoSize) {
                FanCoil(FanCoilNum).ASHRAETempControl = true;
            }

            // Set up component set for supply fan
            if (FanCoil(FanCoilNum).OutsideAirNode > 0) {
                SetUpCompSets(state,
                              FanCoil(FanCoilNum).UnitType,
                              FanCoil(FanCoilNum).Name,
                              FanCoil(FanCoilNum).FanType,
                              FanCoil(FanCoilNum).FanName,
                              state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).MixedAirNode),
                              "UNDEFINED");
            } else {
                SetUpCompSets(state,
                              FanCoil(FanCoilNum).UnitType,
                              FanCoil(FanCoilNum).Name,
                              FanCoil(FanCoilNum).FanType,
                              FanCoil(FanCoilNum).FanName,
                              state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).AirInNode),
                              "UNDEFINED");
            }
            // Set up component set for cooling coil
            SetUpCompSets(state,
                          FanCoil(FanCoilNum).UnitType,
                          FanCoil(FanCoilNum).Name,
                          FanCoil(FanCoilNum).CCoilType,
                          FanCoil(FanCoilNum).CCoilName,
                          "UNDEFINED",
                          "UNDEFINED");

            // Set up component set for heating coil
            SetUpCompSets(state,
                          FanCoil(FanCoilNum).UnitType,
                          FanCoil(FanCoilNum).Name,
                          FanCoil(FanCoilNum).HCoilType,
                          FanCoil(FanCoilNum).HCoilName,
                          "UNDEFINED",
                          state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).AirOutNode));

            // Set up component set for OA mixer - use OA node and Mixed air node
            if (FanCoil(FanCoilNum).OutsideAirNode > 0) {
                SetUpCompSets(state,
                              FanCoil(FanCoilNum).UnitType,
                              FanCoil(FanCoilNum).Name,
                              FanCoil(FanCoilNum).OAMixType,
                              FanCoil(FanCoilNum).OAMixName,
                              state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).OutsideAirNode),
                              state.dataLoopNodes->NodeID(FanCoil(FanCoilNum).MixedAirNode));
            }
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in input. Preceding condition(s) cause termination.");
        }

        for (FanCoilNum = 1; FanCoilNum <= state.dataFanCoilUnits->NumFanCoils; ++FanCoilNum) {
            // Setup Report variables for the Fan Coils
            // CurrentModuleObject='ZoneHVAC:FourPipeFanCoil'
            SetupOutputVariable(state,
                                "Fan Coil Heating Rate",
                                OutputProcessor::Unit::W,
                                FanCoil(FanCoilNum).HeatPower,
                                "System",
                                "Average",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                FanCoil(FanCoilNum).HeatEnergy,
                                "System",
                                "Sum",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                FanCoil(FanCoilNum).TotCoolPower,
                                "System",
                                "Average",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                FanCoil(FanCoilNum).TotCoolEnergy,
                                "System",
                                "Sum",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                FanCoil(FanCoilNum).SensCoolPower,
                                "System",
                                "Average",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                FanCoil(FanCoilNum).SensCoolEnergy,
                                "System",
                                "Sum",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Fan Electricity Rate",
                                OutputProcessor::Unit::W,
                                FanCoil(FanCoilNum).ElecPower,
                                "System",
                                "Average",
                                FanCoil(FanCoilNum).Name);
            SetupOutputVariable(state,
                                "Fan Coil Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                FanCoil(FanCoilNum).ElecEnergy,
                                "System",
                                "Sum",
                                FanCoil(FanCoilNum).Name);
            if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::CycFan || FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::MultiSpeedFan) {
                SetupOutputVariable(state,
                                    "Fan Coil Runtime Fraction",
                                    OutputProcessor::Unit::None,
                                    FanCoil(FanCoilNum).PLR,
                                    "System",
                                    "Average",
                                    FanCoil(FanCoilNum).Name);
                SetupOutputVariable(state,
                                    "Fan Coil Fan Speed Level",
                                    OutputProcessor::Unit::None,
                                    FanCoil(FanCoilNum).SpeedFanSel,
                                    "System",
                                    "Average",
                                    FanCoil(FanCoilNum).Name);
                if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::MultiSpeedFan) {
                    SetupOutputVariable(state,
                                        "Fan Coil Speed Ratio",
                                        OutputProcessor::Unit::None,
                                        FanCoil(FanCoilNum).SpeedRatio,
                                        "System",
                                        "Average",
                                        FanCoil(FanCoilNum).Name);
                    SetupOutputVariable(state,
                                        "Fan Coil Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        FanCoil(FanCoilNum).PLR,
                                        "System",
                                        "Average",
                                        FanCoil(FanCoilNum).Name);
                }
            }
            if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::VarFanVarFlow || FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::VarFanConsFlow) {
                SetupOutputVariable(state,
                                    "Fan Coil Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    FanCoil(FanCoilNum).PLR,
                                    "System",
                                    "Average",
                                    FanCoil(FanCoilNum).Name);
            }
            SetupOutputVariable(state,
                                "Fan Coil Availability Status",
                                OutputProcessor::Unit::None,
                                FanCoil(FanCoilNum).AvailStatus,
                                "System",
                                "Average",
                                FanCoil(FanCoilNum).Name);
        }

        for (FanCoilNum = 1; FanCoilNum <= state.dataFanCoilUnits->NumFanCoils; ++FanCoilNum) {
            if (FanCoil(FanCoilNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         FanCoil(FanCoilNum).CCoilName,
                                                                                         FanCoil(FanCoilNum).CCoilType,
                                                                                         FanCoil(FanCoilNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         FanCoil(FanCoilNum).FanIndex);
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         FanCoil(FanCoilNum).HCoilName,
                                                                                         FanCoil(FanCoilNum).HCoilType,
                                                                                         FanCoil(FanCoilNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         FanCoil(FanCoilNum).FanIndex);
            } else {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         FanCoil(FanCoilNum).CCoilName,
                                                                                         FanCoil(FanCoilNum).CCoilType,
                                                                                         FanCoil(FanCoilNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         FanCoil(FanCoilNum).FanIndex);
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         FanCoil(FanCoilNum).HCoilName,
                                                                                         FanCoil(FanCoilNum).HCoilType,
                                                                                         FanCoil(FanCoilNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         FanCoil(FanCoilNum).FanIndex);
            }
        }
    }

    void InitFanCoilUnits(EnergyPlusData &state,
                          int const FanCoilNum,       // number of the current fan coil unit being simulated
                          int const ZoneNum,          // number of zone being served
                          int const ControlledZoneNum // index into ZoneEquipConfig array may not be equal to ZoneNum
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2000
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Fan Coil Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // Using/Aliasing
        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
        using DataPlant::TypeOf_CoilWaterCooling;
        using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::FanCoil4Pipe_Num;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitFanCoilUnits");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InNode;         // inlet node number in fan coil loop
        int OutNode;        // outlet node number in fan coil loop
        int InletNode;      // inlet node number for fan coil FanCoilNum
        int HotConNode;     // hot water control node number in fan coil loop
        int ColdConNode;    // hot water control node number in fan coil loop
        int OutsideAirNode; // outside air node number in fan coil loop
        int AirRelNode;     // relief air node number in fan coil loop
        Real64 RhoAir;      // air density at InNode
        int Loop;
        Real64 rho;
        bool errFlag;

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);

        // Do the one time initializations
        if (state.dataFanCoilUnits->InitFanCoilUnitsOneTimeFlag) {

            state.dataFanCoilUnits->MyEnvrnFlag.allocate(state.dataFanCoilUnits->NumFanCoils);
            state.dataFanCoilUnits->MySizeFlag.allocate(state.dataFanCoilUnits->NumFanCoils);
            state.dataFanCoilUnits->MyPlantScanFlag.allocate(state.dataFanCoilUnits->NumFanCoils);
            state.dataFanCoilUnits->MyZoneEqFlag.allocate(state.dataFanCoilUnits->NumFanCoils);
            state.dataFanCoilUnits->MyEnvrnFlag = true;
            state.dataFanCoilUnits->MySizeFlag = true;
            state.dataFanCoilUnits->MyPlantScanFlag = true;
            state.dataFanCoilUnits->MyZoneEqFlag = true;
            state.dataFanCoilUnits->InitFanCoilUnitsOneTimeFlag = false;
        }

        if (allocated(ZoneComp)) {
            if (state.dataFanCoilUnits->MyZoneEqFlag(FanCoilNum)) { // initialize the name of each availability manager list and zone number
                ZoneComp(FanCoil4Pipe_Num).ZoneCompAvailMgrs(FanCoilNum).AvailManagerListName = FanCoil(FanCoilNum).AvailManagerListName;
                ZoneComp(FanCoil4Pipe_Num).ZoneCompAvailMgrs(FanCoilNum).ZoneNum = ZoneNum;
                state.dataFanCoilUnits->MyZoneEqFlag(FanCoilNum) = false;
            }
            FanCoil(FanCoilNum).AvailStatus = ZoneComp(FanCoil4Pipe_Num).ZoneCompAvailMgrs(FanCoilNum).AvailStatus;
        }

        if (state.dataFanCoilUnits->MyPlantScanFlag(FanCoilNum) && allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                ScanPlantLoopsForObject(state,
                                        FanCoil(FanCoilNum).HCoilName,
                                        FanCoil(FanCoilNum).HCoilPlantTypeOfNum,
                                        FanCoil(FanCoilNum).HeatCoilLoopNum,
                                        FanCoil(FanCoilNum).HeatCoilLoopSide,
                                        FanCoil(FanCoilNum).HeatCoilBranchNum,
                                        FanCoil(FanCoilNum).HeatCoilCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);

                if (errFlag) {
                    ShowContinueError(state, "Reference Unit=\"" + FanCoil(FanCoilNum).Name + "\", type=" + FanCoil(FanCoilNum).UnitType);
                    ShowFatalError(state, "InitFanCoilUnits: Program terminated for previous conditions.");
                }

                FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum = state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum)
                                                                     .LoopSide(FanCoil(FanCoilNum).HeatCoilLoopSide)
                                                                     .Branch(FanCoil(FanCoilNum).HeatCoilBranchNum)
                                                                     .Comp(FanCoil(FanCoilNum).HeatCoilCompNum)
                                                                     .NodeNumOut;

            } else if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Electric) {
                // do nothing, valid type
            } else {
                ShowFatalError(state, "InitFanCoilUnits: FanCoil=" + FanCoil(FanCoilNum).Name + ", invalid heating coil type. Program terminated.");
            }

            if ((FanCoil(FanCoilNum).CCoilPlantTypeOfNum == TypeOf_CoilWaterCooling) ||
                (FanCoil(FanCoilNum).CCoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling)) {
                ScanPlantLoopsForObject(state,
                                        FanCoil(FanCoilNum).CCoilPlantName,
                                        FanCoil(FanCoilNum).CCoilPlantTypeOfNum,
                                        FanCoil(FanCoilNum).CoolCoilLoopNum,
                                        FanCoil(FanCoilNum).CoolCoilLoopSide,
                                        FanCoil(FanCoilNum).CoolCoilBranchNum,
                                        FanCoil(FanCoilNum).CoolCoilCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowContinueError(state, "Reference Unit=\"" + FanCoil(FanCoilNum).Name + "\", type=" + FanCoil(FanCoilNum).UnitType);
                    ShowFatalError(state, "InitFanCoilUnits: Program terminated for previous conditions.");
                }
                FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum = state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum)
                                                                     .LoopSide(FanCoil(FanCoilNum).CoolCoilLoopSide)
                                                                     .Branch(FanCoil(FanCoilNum).CoolCoilBranchNum)
                                                                     .Comp(FanCoil(FanCoilNum).CoolCoilCompNum)
                                                                     .NodeNumOut;
            } else {
                ShowFatalError(state, "InitFanCoilUnits: FanCoil=" + FanCoil(FanCoilNum).Name + ", invalid cooling coil type. Program terminated.");
            }

            state.dataFanCoilUnits->MyPlantScanFlag(FanCoilNum) = false;
        }

        if (!state.dataFanCoilUnits->InitFanCoilUnitsCheckInZoneEquipmentListFlag && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataFanCoilUnits->InitFanCoilUnitsCheckInZoneEquipmentListFlag = true;
            for (Loop = 1; Loop <= state.dataFanCoilUnits->NumFanCoils; ++Loop) {
                if (CheckZoneEquipmentList(state, FanCoil(Loop).UnitType, FanCoil(Loop).Name)) continue;
                ShowSevereError(state,
                                "InitFanCoil: FanCoil Unit=[" + FanCoil(Loop).UnitType + ',' + FanCoil(Loop).Name +
                                    "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataFanCoilUnits->MySizeFlag(FanCoilNum) &&
            !state.dataFanCoilUnits->MyPlantScanFlag(FanCoilNum)) {

            SizeFanCoilUnit(state, FanCoilNum, ControlledZoneNum);

            state.dataFanCoilUnits->MySizeFlag(FanCoilNum) = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataFanCoilUnits->MyEnvrnFlag(FanCoilNum) &&
            !state.dataFanCoilUnits->MyPlantScanFlag(FanCoilNum)) {
            InNode = FanCoil(FanCoilNum).AirInNode;
            OutNode = FanCoil(FanCoilNum).AirOutNode;
            OutsideAirNode = FanCoil(FanCoilNum).OutsideAirNode;
            RhoAir = state.dataEnvrn->StdRhoAir;
            HotConNode = FanCoil(FanCoilNum).HeatCoilFluidInletNode;
            ColdConNode = FanCoil(FanCoilNum).CoolCoilFluidInletNode;
            // set the mass flow rates from the input volume flow rates
            FanCoil(FanCoilNum).MaxAirMassFlow = RhoAir * FanCoil(FanCoilNum).MaxAirVolFlow;
            FanCoil(FanCoilNum).OutAirMassFlow = RhoAir * FanCoil(FanCoilNum).OutAirVolFlow;

            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                rho = GetDensityGlycol(state,
                                       state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).FluidName,
                                       DataGlobalConstants::HWInitConvTemp,
                                       state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).FluidIndex,
                                       RoutineName);
                FanCoil(FanCoilNum).MaxHeatCoilFluidFlow = rho * FanCoil(FanCoilNum).MaxHotWaterVolFlow;
                FanCoil(FanCoilNum).MinHotWaterFlow = rho * FanCoil(FanCoilNum).MinHotWaterVolFlow;
            }

            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).FluidName,
                                   DataGlobalConstants::CWInitConvTemp,
                                   state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).FluidIndex,
                                   RoutineName);
            FanCoil(FanCoilNum).MaxCoolCoilFluidFlow = rho * FanCoil(FanCoilNum).MaxColdWaterVolFlow;
            FanCoil(FanCoilNum).MinColdWaterFlow = rho * FanCoil(FanCoilNum).MinColdWaterVolFlow;

            // set the node max and min mass flow rates
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                InitComponentNodes(state,
                                   FanCoil(FanCoilNum).MinHotWaterFlow,
                                   FanCoil(FanCoilNum).MaxHeatCoilFluidFlow,
                                   FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                   FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                   FanCoil(FanCoilNum).HeatCoilLoopNum,
                                   FanCoil(FanCoilNum).HeatCoilLoopSide,
                                   FanCoil(FanCoilNum).HeatCoilBranchNum,
                                   FanCoil(FanCoilNum).HeatCoilCompNum);
            }

            InitComponentNodes(state,
                               FanCoil(FanCoilNum).MinColdWaterFlow,
                               FanCoil(FanCoilNum).MaxCoolCoilFluidFlow,
                               FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                               FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                               FanCoil(FanCoilNum).CoolCoilLoopNum,
                               FanCoil(FanCoilNum).CoolCoilLoopSide,
                               FanCoil(FanCoilNum).CoolCoilBranchNum,
                               FanCoil(FanCoilNum).CoolCoilCompNum);
            //  Node(HotConNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxHeatCoilFluidFlow
            //  Node(HotConNode)%MassFlowRateMin = FanCoil(FanCoilNum)%MinHotWaterFlow
            //  Node(ColdConNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxCoolCoilFluidFlow
            //  Node(ColdConNode)%MassFlowRateMin = FanCoil(FanCoilNum)%MinColdWaterFlow

            if (FanCoil(FanCoilNum).OutsideAirNode > 0) {
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMax = FanCoil(FanCoilNum).OutAirMassFlow;
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMin = 0.0;
            }
            state.dataLoopNodes->Node(OutNode).MassFlowRateMax = FanCoil(FanCoilNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(InNode).MassFlowRateMax = FanCoil(FanCoilNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
            state.dataFanCoilUnits->MyEnvrnFlag(FanCoilNum) = false;
        } // end one time inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataFanCoilUnits->MyEnvrnFlag(FanCoilNum) = true;
        }

        // These initializations are done every iteration
        InletNode = FanCoil(FanCoilNum).AirInNode;
        OutsideAirNode = FanCoil(FanCoilNum).OutsideAirNode;
        AirRelNode = FanCoil(FanCoilNum).AirReliefNode;
        FanCoil(FanCoilNum).SpeedRatio = 0.0;
        if (FanCoil(FanCoilNum).FanOpModeSchedPtr > 0) {
            if (GetCurrentScheduleValue(state, FanCoil(FanCoilNum).FanOpModeSchedPtr) == 0.0) {
                FanCoil(FanCoilNum).FanOpMode = CycFanCycCoil;
            } else {
                FanCoil(FanCoilNum).FanOpMode = ContFanCycCoil;
            }
        }
        // Set the inlet node mass flow rate
        if (((GetCurrentScheduleValue(state, FanCoil(FanCoilNum).SchedPtr) > 0.0 &&
              GetCurrentScheduleValue(state, FanCoil(FanCoilNum).fanAvailSchIndex) > 0.0) ||
             state.dataHVACGlobal->ZoneCompTurnFansOn) &&
            !state.dataHVACGlobal->ZoneCompTurnFansOff) {
            state.dataLoopNodes->Node(InletNode).MassFlowRate = FanCoil(FanCoilNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;

            if (OutsideAirNode > 0) {
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = FanCoil(FanCoilNum).OutAirMassFlow;
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = FanCoil(FanCoilNum).OutAirMassFlow;
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = FanCoil(FanCoilNum).OutAirMassFlow;
                state.dataLoopNodes->Node(AirRelNode).MassFlowRate = FanCoil(FanCoilNum).OutAirMassFlow;
                state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = FanCoil(FanCoilNum).OutAirMassFlow;
                state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = FanCoil(FanCoilNum).OutAirMassFlow;
            }

        } else {
            state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;
            if (OutsideAirNode > 0) {
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = 0.0;
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = 0.0;
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(AirRelNode).MassFlowRate = 0.0;
                state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = 0.0;
                state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = 0.0;
            }
        }
    }

    void SizeFanCoilUnit(EnergyPlusData &state,
                         int const FanCoilNum,
                         int const ControlledZoneNum // index into ZoneEquipConfig array may not be equal to ZoneNum
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   January 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Fan Coil Unit components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays and plant sizing data.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::CoolingAirflowSizing;
        using DataHVACGlobals::CoolingCapacitySizing;
        using DataHVACGlobals::HeatingAirflowSizing;
        using DataHVACGlobals::HeatingCapacitySizing;
        using Fans::GetFanDesignVolumeFlowRate;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        using HVACHXAssistedCoolingCoil::GetHXCoilType;
        using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
        using PlantUtilities::MyPlantSizingIndex;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW;
        using WaterCoils::GetCoilWaterInletNode;
        using WaterCoils::GetCoilWaterOutletNode;
        using WaterCoils::GetWaterCoilIndex;
        using WaterCoils::SetCoilDesFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeFanCoilUnit: "); // include trailing blank space
        static constexpr std::string_view RoutineNameNoSpace("SizeFanCoilUnit");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizHeatNum;  // index of plant sizing object for 1st heating loop
        int PltSizCoolNum;  // index of plant sizing object for 1st cooling loop
        bool ErrorsFound;   // TRUE if errors foind during sizing
        Real64 DesCoilLoad; // coil load used for sizing [W]
        std::string CoolingCoilName;
        std::string CoolingCoilType;
        Real64 rho;
        Real64 Cp;
        int zoneHVACIndex;              // index of zoneHVAC equipment sizing specification
        bool IsAutoSize;                // Indicator to autosize for reporting
        Real64 MaxAirVolFlowDes;        // Autosized max air flow for reporting
        Real64 MaxAirVolFlowUser;       // Hardsized max air flow for reporting
        Real64 OutAirVolFlowDes;        // Autosized outdoor air flow for reporting
        Real64 OutAirVolFlowUser;       // Hardsized outdoor air flow for reporting
        Real64 MaxHotWaterVolFlowDes;   // Autosized hot water flow for reporting
        Real64 MaxHotWaterVolFlowUser;  // Hardsized hot water flow for reporting
        Real64 MaxColdWaterVolFlowDes;  // Autosized cold water flow for reporting
        Real64 MaxColdWaterVolFlowUser; // Hardsized cold water flow for reporting
        Real64 CoolingAirVolFlowDes;    // cooling supply air flow rate
        Real64 HeatingAirVolFlowDes;    // heating supply air flow rate
        std::string CompName;           // component name
        std::string CompType;           // component type
        std::string SizingString;       // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;                // autosized value of coil input field
        int FieldNum = 1;               // IDD numeric field number where input field description is found
        int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                          // HeatingCapacitySizing, etc.)
        bool PrintFlag;   // TRUE when sizing information is reported in the eio file
        int SAFMethod(0); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                          // FractionOfAutosizedHeatingAirflow ...)
        int CapSizingMethod(0);    // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                   // FractionOfAutosizedHeatingCapacity )
        bool SizingDesRunThisZone; // test for zone sizing
        bool DoWaterCoilSizing = false; // if TRUE do water coil sizing calculation
        Real64 WaterCoilSizDeltaT;      // water coil deltaT for design water flow rate autosizing
        int CoilNum;                    // index of water coil object

        PltSizCoolNum = 0;
        PltSizHeatNum = 0;
        ErrorsFound = false;
        IsAutoSize = false;
        MaxAirVolFlowDes = 0.0;
        MaxAirVolFlowUser = 0.0;
        OutAirVolFlowDes = 0.0;
        OutAirVolFlowUser = 0.0;
        MaxHotWaterVolFlowDes = 0.0;
        MaxHotWaterVolFlowUser = 0.0;
        MaxColdWaterVolFlowDes = 0.0;
        MaxColdWaterVolFlowUser = 0.0;

        CoolingAirVolFlowDes = 0.0;
        HeatingAirVolFlowDes = 0.0;
        state.dataSize->ZoneHeatingOnlyFan = false;
        state.dataSize->ZoneCoolingOnlyFan = false;
        state.dataSize->DataScalableSizingON = false;
        state.dataSize->DataScalableCapSizingON = false;

        state.dataSize->DataFracOfAutosizedCoolingAirflow = 1.0;
        state.dataSize->DataFracOfAutosizedHeatingAirflow = 1.0;
        state.dataSize->DataFracOfAutosizedCoolingCapacity = 1.0;
        state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);

        CompType = FanCoil(FanCoilNum).UnitType;
        CompName = FanCoil(FanCoilNum).Name;
        state.dataSize->DataZoneNumber = FanCoil(FanCoilNum).ControlZoneNum;
        if (FanCoil(FanCoilNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataSize->DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
        } else {
            state.dataSize->DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
        }
        state.dataSize->DataFanIndex = FanCoil(FanCoilNum).FanIndex;
        // fan coil unit is always blow thru
        state.dataSize->DataFanPlacement = DataSizing::zoneFanPlacement::zoneBlowThru;

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

        if (state.dataSize->CurZoneEqNum > 0) {

            if (FanCoil(FanCoilNum).HVACSizingIndex > 0) {

                // initialize OA flow for sizing other inputs (e.g., inlet temp, capacity, etc.)
                if (FanCoil(FanCoilNum).OutAirVolFlow == AutoSize) {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
                } else {
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = FanCoil(FanCoilNum).OutAirVolFlow;
                }
                if (FanCoil(FanCoilNum).ATMixerExists) {                        // set up ATMixer conditions for scalable capacity sizing
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = 0.0; // Equipment OA flow should always be 0 when ATMixer is used
                    SingleDuct::setATMixerSizingProperties(state, FanCoil(FanCoilNum).ATMixerIndex, ControlledZoneNum, state.dataSize->CurZoneEqNum);
                }

                zoneHVACIndex = FanCoil(FanCoilNum).HVACSizingIndex;
                FieldNum = 1;
                PrintFlag = true;
                SizingString = state.dataFanCoilUnits->FanCoilNumericFields(FanCoilNum).FieldNames(FieldNum) + " [m3/s]";
                if (state.dataGlobal->isEpJSON) SizingString = "maximum_supply_air_flow_rate [m3/s]";
                if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod > 0) {
                    SizingMethod = CoolingAirflowSizing;
                    SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                    if (SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow) {
                        if (SAFMethod == SupplyAirFlowRate) {
                            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                                ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow =
                                    state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                            }
                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        } else if (SAFMethod == FlowPerFloorArea) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow *
                                                                                    state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                            state.dataSize->DataScalableSizingON = true;
                        } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                            state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                            TempSize = AutoSize;
                            state.dataSize->DataScalableSizingON = true;
                        } else {
                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        }
                        CoolingAirFlowSizer sizingCoolingAirFlow;
                        sizingCoolingAirFlow.overrideSizingString(SizingString);
                        // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);

                    } else if (SAFMethod == FlowPerCoolingCapacity) {
                        SizingMethod = CoolingCapacitySizing;
                        TempSize = AutoSize;
                        PrintFlag = false;
                        CoolingCapacitySizer sizerCoolingCapacity;
                        sizerCoolingCapacity.overrideSizingString(SizingString);
                        sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        state.dataSize->DataAutosizedCoolingCapacity = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod == FractionOfAutosizedCoolingCapacity) {
                            state.dataSize->DataFracOfAutosizedCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                        }
                        state.dataSize->DataFlowPerCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        PrintFlag = true;
                        TempSize = AutoSize;
                        state.dataSize->DataScalableSizingON = true;
                        CoolingAirFlowSizer sizingCoolingAirFlow;
                        sizingCoolingAirFlow.overrideSizingString(SizingString);
                        // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                    }
                } else if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod > 0) {
                    // now do heating supply air flow rate sizing
                    SizingMethod = HeatingAirflowSizing;
                    SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                    if (SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow) {
                        if (SAFMethod == SupplyAirFlowRate) {
                            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow > 0.0) {
                                ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow =
                                    state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                            }
                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        } else if (SAFMethod == FlowPerFloorArea) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow *
                                                                                    state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                            state.dataSize->DataScalableSizingON = true;
                        } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                            state.dataSize->DataFracOfAutosizedHeatingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                            TempSize = AutoSize;
                            state.dataSize->DataScalableSizingON = true;
                        } else {
                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        }
                        bool errorsFound = false;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowDes = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
                    } else if (SAFMethod == FlowPerHeatingCapacity) {
                        SizingMethod = HeatingCapacitySizing;
                        TempSize = AutoSize;
                        PrintFlag = false;
                        state.dataSize->DataScalableSizingON = true;
                        // initialize OA flow for sizing capacity
                        if (FanCoil(FanCoilNum).OutAirVolFlow == AutoSize) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow =
                                state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
                        } else {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = FanCoil(FanCoilNum).OutAirVolFlow;
                        }
                        bool errorsFound = false;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        TempSize = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
                            state.dataSize->DataFracOfAutosizedHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                        }
                        state.dataSize->DataAutosizedHeatingCapacity = TempSize;
                        state.dataSize->DataFlowPerHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        SizingMethod = HeatingAirflowSizing;
                        PrintFlag = true;
                        TempSize = AutoSize;
                        errorsFound = false;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowDes = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
                    }
                }

                if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow == AutoSize ||
                    state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow == AutoSize) {
                    IsAutoSize = true;
                    FanCoil(FanCoilNum).MaxAirVolFlow = AutoSize;
                    MaxAirVolFlowDes = max(CoolingAirVolFlowDes, HeatingAirVolFlowDes);
                } else {
                    FanCoil(FanCoilNum).MaxAirVolFlow = max(CoolingAirVolFlowDes, HeatingAirVolFlowDes);
                    MaxAirVolFlowDes = 0.0;
                }
            } else {
                // SizingString = "Supply Air Maximum Flow Rate [m3/s]";
                TempSize = FanCoil(FanCoilNum).MaxAirVolFlow;
                PrintFlag = true;
                if (FanCoil(FanCoilNum).MaxAirVolFlow == AutoSize) {
                    IsAutoSize = true;
                    SystemAirFlowSizer sizerSystemAirFlow;
                    // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    MaxAirVolFlowDes = sizerSystemAirFlow.size(state, TempSize, ErrorsFound);
                } else {
                    MaxAirVolFlowDes = 0.0;
                }
            }
        }

        if (state.dataSize->CurZoneEqNum > 0) {

            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {

            } else {
                if (MaxAirVolFlowDes < SmallAirVolFlow) {
                    MaxAirVolFlowDes = 0.0;
                }

                //     If fan is autosized, get fan volumetric flow rate
                if (FanCoil(FanCoilNum).FanAirVolFlow == AutoSize) {
                    if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                        Fans::SimulateFanComponents(state, FanCoil(FanCoilNum).FanName, true, FanCoil(FanCoilNum).FanIndex);
                        FanCoil(FanCoilNum).FanAirVolFlow =
                            GetFanDesignVolumeFlowRate(state, cFanTypes(FanCoil(FanCoilNum).FanType_Num), FanCoil(FanCoilNum).FanName, ErrorsFound);
                    } else {
                        state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, _, _, _, _);
                        FanCoil(FanCoilNum).FanAirVolFlow = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->designAirVolFlowRate;
                    }
                }
                //     Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
                if (MaxAirVolFlowDes > FanCoil(FanCoilNum).FanAirVolFlow) {
                    ShowWarningError(state, std::string{RoutineName} + FanCoil(FanCoilNum).UnitType + ": " + FanCoil(FanCoilNum).Name);
                    ShowContinueError(state, "... Maximum supply air flow rate is greater than the maximum fan flow rate.");
                    ShowContinueError(state, format("... Fan Coil Unit flow = {:.5T} [m3/s].", MaxAirVolFlowDes));
                    ShowContinueError(state, "... Fan = " + cFanTypes(FanCoil(FanCoilNum).FanType_Num) + ": " + FanCoil(FanCoilNum).FanName);
                    ShowContinueError(state, format("... Fan flow = {:.5T} [m3/s].", FanCoil(FanCoilNum).FanAirVolFlow));
                    ShowContinueError(state, "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.");
                    MaxAirVolFlowDes = FanCoil(FanCoilNum).FanAirVolFlow;
                }

                if (IsAutoSize) {
                    FanCoil(FanCoilNum).MaxAirVolFlow = MaxAirVolFlowDes;
                } else { // Hard size with sizing data
                    if (FanCoil(FanCoilNum).MaxAirVolFlow > 0.0 && MaxAirVolFlowDes > 0.0) {
                        MaxAirVolFlowUser = FanCoil(FanCoilNum).MaxAirVolFlow;
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxAirVolFlowDes - MaxAirVolFlowUser) / MaxAirVolFlowUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil(FanCoilNum).UnitType + ' ' +
                                                FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, format("User-Specified Supply Air Maximum Flow Rate of {:.5R} [m3/s]", MaxAirVolFlowUser));
                                ShowContinueError(state,
                                                  format("differs from Design Size Supply Air Maximum Flow Rate of {:.5R} [m3/s]", MaxAirVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        } else if (FanCoil(FanCoilNum).FanAirVolFlow == AutoSize) {
            if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state, FanCoil(FanCoilNum).FanName, true, FanCoil(FanCoilNum).FanIndex);
                FanCoil(FanCoilNum).FanAirVolFlow =
                    GetFanDesignVolumeFlowRate(state, cFanTypes(FanCoil(FanCoilNum).FanType_Num), FanCoil(FanCoilNum).FanName, ErrorsFound);
            } else {
                state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, _, _, _, _);
                FanCoil(FanCoilNum).FanAirVolFlow = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->designAirVolFlowRate;
            }
            //   Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
            if (FanCoil(FanCoilNum).MaxAirVolFlow > FanCoil(FanCoilNum).FanAirVolFlow) {
                ShowWarningError(state, std::string{RoutineName} + FanCoil(FanCoilNum).UnitType + ": " + FanCoil(FanCoilNum).Name);
                ShowContinueError(state, "... Maximum supply air flow rate is greater than the maximum fan flow rate.");
                ShowContinueError(state, format("... Fan Coil Unit flow = {:.5T} m3/s.", FanCoil(FanCoilNum).MaxAirVolFlow));
                ShowContinueError(state, "... Fan = " + cFanTypes(FanCoil(FanCoilNum).FanType_Num) + ": " + FanCoil(FanCoilNum).FanName);
                ShowContinueError(state, format("... Fan flow = {:.5T} m3/s.", FanCoil(FanCoilNum).FanAirVolFlow));
                ShowContinueError(state, "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.");
                FanCoil(FanCoilNum).MaxAirVolFlow = FanCoil(FanCoilNum).FanAirVolFlow;
            }
        }

        IsAutoSize = false;
        if (FanCoil(FanCoilNum).OutAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }

        if (state.dataSize->CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                if (FanCoil(FanCoilNum).OutAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 FanCoil(FanCoilNum).UnitType,
                                                 FanCoil(FanCoilNum).Name,
                                                 "User-Specified Maximum Outdoor Air Flow Rate [m3/s]",
                                                 FanCoil(FanCoilNum).OutAirVolFlow);
                }
            } else {
                CheckZoneSizing(state, FanCoil(FanCoilNum).UnitType, FanCoil(FanCoilNum).Name);
                OutAirVolFlowDes = min(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA, FanCoil(FanCoilNum).MaxAirVolFlow);
                if (OutAirVolFlowDes < SmallAirVolFlow) {
                    OutAirVolFlowDes = 0.0;
                }
                if (IsAutoSize) {
                    FanCoil(FanCoilNum).OutAirVolFlow = OutAirVolFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 FanCoil(FanCoilNum).UnitType,
                                                 FanCoil(FanCoilNum).Name,
                                                 "Design Size Maximum Outdoor Air Flow Rate [m3/s]",
                                                 OutAirVolFlowDes);
                } else {
                    if (FanCoil(FanCoilNum).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0) {
                        OutAirVolFlowUser = FanCoil(FanCoilNum).OutAirVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     FanCoil(FanCoilNum).UnitType,
                                                     FanCoil(FanCoilNum).Name,
                                                     "Design Size Maximum Outdoor Air Flow Rate [m3/s]",
                                                     OutAirVolFlowDes,
                                                     "User-Specified Maximum Outdoor Air Flow Rate [m3/s]",
                                                     OutAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(OutAirVolFlowDes - OutAirVolFlowUser) / OutAirVolFlowUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil(FanCoilNum).UnitType + ' ' +
                                                FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, format("User-Specified Maximum Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Maximum Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
            ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = FanCoil(FanCoilNum).OutAirVolFlow; // sets OA frac in sizing

            if (FanCoil(FanCoilNum).ATMixerExists) {                        // set up ATMixer conditions for use in component sizing
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = 0.0; // Equipment OA flow should always be 0 when ATMixer is used
                SingleDuct::setATMixerSizingProperties(state, FanCoil(FanCoilNum).ATMixerIndex, ControlledZoneNum, state.dataSize->CurZoneEqNum);
            }
        }

        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {

            IsAutoSize = false;
            if (FanCoil(FanCoilNum).MaxHotWaterVolFlow == AutoSize) {
                IsAutoSize = true;
            }

            if (state.dataSize->CurZoneEqNum > 0) {
                if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                    if (FanCoil(FanCoilNum).MaxHotWaterVolFlow > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     FanCoil(FanCoilNum).UnitType,
                                                     FanCoil(FanCoilNum).Name,
                                                     "User-Specified Maximum Hot Water Flow [m3/s]",
                                                     FanCoil(FanCoilNum).MaxHotWaterVolFlow);
                    }
                } else {
                    state.dataFanCoilUnits->CoilWaterInletNode =
                        WaterCoils::GetCoilWaterInletNode(state, "Coil:Heating:Water", FanCoil(FanCoilNum).HCoilName, ErrorsFound);
                    state.dataFanCoilUnits->CoilWaterOutletNode =
                        WaterCoils::GetCoilWaterOutletNode(state, "Coil:Heating:Water", FanCoil(FanCoilNum).HCoilName, ErrorsFound);
                    if (IsAutoSize) {
                        PltSizHeatNum = MyPlantSizingIndex(state,
                                                           "Coil:Heating:Water",
                                                           FanCoil(FanCoilNum).HCoilName,
                                                           state.dataFanCoilUnits->CoilWaterInletNode,
                                                           state.dataFanCoilUnits->CoilWaterOutletNode,
                                                           ErrorsFound);
                        CoilNum = WaterCoils::GetWaterCoilIndex(state, "COIL:HEATING:WATER", FanCoil(FanCoilNum).HCoilName, ErrorsFound);
                        if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
                            WaterCoilSizDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
                            DoWaterCoilSizing = true;
                        } else {
                            if (PltSizHeatNum > 0) {
                                WaterCoilSizDeltaT = state.dataSize->PlantSizData(PltSizHeatNum).DeltaT;
                                DoWaterCoilSizing = true;
                            } else {
                                DoWaterCoilSizing = false;
                                // If there is no heating Plant Sizing object and autosizing was requested, issue fatal error message
                                ShowSevereError(state, "Autosizing of water coil requires a heating loop Sizing:Plant object");
                                ShowContinueError(state, "Occurs in " + FanCoil(FanCoilNum).UnitType + " Object=" + FanCoil(FanCoilNum).Name);
                                ErrorsFound = true;
                            }
                        }
                        if (DoWaterCoilSizing) {
                            SizingMethod = HeatingCapacitySizing;
                            if (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatMassFlow > 0.0) {
                                state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatOAFlowFrac = min(
                                    FanCoil(FanCoilNum).OutAirVolFlow / state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatMassFlow,
                                    1.0);
                            } else {
                                state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatOAFlowFrac = 0.0;
                            }
                            if (FanCoil(FanCoilNum).HVACSizingIndex > 0) {
                                zoneHVACIndex = FanCoil(FanCoilNum).HVACSizingIndex;
                                CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
                                ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                    CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                    if (CapSizingMethod == HeatingDesignCapacity) {
                                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                                            ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                                            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                        }
                                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                                        if (state.dataSize->ZoneSizingRunDone) {
                                            PrintFlag = false;
                                            TempSize = AutoSize;
                                            state.dataSize->DataFlowUsedForSizing =
                                                state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow;
                                            bool errorsFound = false;
                                            HeatingCapacitySizer sizerHeatingCapacity;
                                            sizerHeatingCapacity.overrideSizingString(SizingString);
                                            sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                                                sizerHeatingCapacity.size(state, TempSize, errorsFound);
                                            ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                                        }
                                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity *
                                                   state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                                        state.dataSize->DataScalableCapSizingON = true;
                                    } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                        CheckZoneSizing(state, CompType, CompName);
                                        PrintFlag = false;
                                        TempSize = AutoSize;
                                        state.dataSize->DataFlowUsedForSizing =
                                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow;
                                        bool errorsFound = false;
                                        HeatingCapacitySizer sizerHeatingCapacity;
                                        sizerHeatingCapacity.overrideSizingString(SizingString);
                                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                                            sizerHeatingCapacity.size(state, TempSize, errorsFound);
                                        ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                                        TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad *
                                                   state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                        state.dataSize->DataScalableCapSizingON = true;
                                    }
                                }
                                SizingString = "Heating Design Capacity [W]";
                                PrintFlag = false;
                                bool errorsFound = false;
                                HeatingCapacitySizer sizerHeatingCapacity;
                                sizerHeatingCapacity.overrideSizingString(SizingString);
                                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                                state.dataSize->DataScalableCapSizingON = false;
                                state.dataSize->DataFlowUsedForSizing = 0.0;

                            } else {
                                SizingString = "Heating Design Capacity [W]";
                                PrintFlag = false;
                                TempSize = AutoSize;
                                bool errorsFound = false;
                                HeatingCapacitySizer sizerHeatingCapacity;
                                sizerHeatingCapacity.overrideSizingString(SizingString);
                                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                            }
                            FanCoil(FanCoilNum).DesHeatingLoad = DesCoilLoad;
                            if (DesCoilLoad >= SmallLoad) {
                                rho = GetDensityGlycol(state,
                                                       state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).FluidName,
                                                       DataGlobalConstants::HWInitConvTemp,
                                                       state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).FluidIndex,
                                                       RoutineNameNoSpace);
                                Cp = GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).FluidName,
                                                           DataGlobalConstants::HWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).FluidIndex,
                                                           RoutineNameNoSpace);

                                MaxHotWaterVolFlowDes = DesCoilLoad / (WaterCoilSizDeltaT * Cp * rho);
                            } else {
                                MaxHotWaterVolFlowDes = 0.0;
                            }
                        }
                    }
                }

                if (IsAutoSize) {
                    FanCoil(FanCoilNum).MaxHotWaterVolFlow = MaxHotWaterVolFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 FanCoil(FanCoilNum).UnitType,
                                                 FanCoil(FanCoilNum).Name,
                                                 "Design Size Maximum Hot Water Flow [m3/s]",
                                                 MaxHotWaterVolFlowDes);
                } else { // Hard size with sizing data
                    if (FanCoil(FanCoilNum).MaxHotWaterVolFlow > 0.0 && MaxHotWaterVolFlowDes > 0.0) {
                        MaxHotWaterVolFlowDes = FanCoil(FanCoilNum).MaxHotWaterVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     FanCoil(FanCoilNum).UnitType,
                                                     FanCoil(FanCoilNum).Name,
                                                     "Design Size Maximum Hot Water Flow [m3/s]",
                                                     MaxHotWaterVolFlowDes,
                                                     "User-Specified Maximum Hot Water Flow [m3/s]",
                                                     MaxHotWaterVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxHotWaterVolFlowDes - MaxHotWaterVolFlowUser) / MaxHotWaterVolFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil(FanCoilNum).UnitType + ' ' +
                                                FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, format("User-Specified Maximum Hot Water Flow of {:.5R} [m3/s]", MaxHotWaterVolFlowUser));
                                ShowContinueError(state,
                                                  format("differs from Design Size Maximum Hot Water Flow of {:.5R} [m3/s]", MaxHotWaterVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        } else if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Electric) {
            if (FanCoil(FanCoilNum).DesignHeatingCapacity == AutoSize) {
                CompName = FanCoil(FanCoilNum).HCoilName;
                CompType = FanCoil(FanCoilNum).HCoilType;
                SizingMethod = HeatingCapacitySizing;
                PrintFlag = false;
                TempSize = FanCoil(FanCoilNum).DesignHeatingCapacity;
                SizingString = "Nominal Heating Capacity [W]";
                bool errorsFound = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                FanCoil(FanCoilNum).DesignHeatingCapacity = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                FanCoil(FanCoilNum).DesHeatingLoad = FanCoil(FanCoilNum).DesignHeatingCapacity;
            }
        }

        IsAutoSize = false;
        if (FanCoil(FanCoilNum).MaxColdWaterVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataSize->CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                if (FanCoil(FanCoilNum).MaxColdWaterVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 FanCoil(FanCoilNum).UnitType,
                                                 FanCoil(FanCoilNum).Name,
                                                 "User-Specified Maximum Cold Water Flow [m3/s]",
                                                 FanCoil(FanCoilNum).MaxColdWaterVolFlow);
                }
            } else {
                if (UtilityRoutines::SameString(FanCoil(FanCoilNum).CCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                    CoolingCoilName = GetHXDXCoilName(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, ErrorsFound);
                    CoolingCoilType = GetHXCoilType(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, ErrorsFound);
                } else {
                    CoolingCoilName = FanCoil(FanCoilNum).CCoilName;
                    CoolingCoilType = FanCoil(FanCoilNum).CCoilType;
                }
                state.dataFanCoilUnits->CoilWaterInletNode = WaterCoils::GetCoilWaterInletNode(state, CoolingCoilType, CoolingCoilName, ErrorsFound);
                state.dataFanCoilUnits->CoilWaterOutletNode =
                    WaterCoils::GetCoilWaterOutletNode(state, CoolingCoilType, CoolingCoilName, ErrorsFound);
                if (IsAutoSize) {
                    PltSizCoolNum = MyPlantSizingIndex(state,
                                                       CoolingCoilType,
                                                       CoolingCoilName,
                                                       state.dataFanCoilUnits->CoilWaterInletNode,
                                                       state.dataFanCoilUnits->CoilWaterOutletNode,
                                                       ErrorsFound);
                    CoilNum = WaterCoils::GetWaterCoilIndex(state, CoolingCoilType, CoolingCoilName, ErrorsFound);
                    if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
                        WaterCoilSizDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
                        DoWaterCoilSizing = true;
                    } else {
                        if (PltSizCoolNum > 0) {
                            WaterCoilSizDeltaT = state.dataSize->PlantSizData(PltSizCoolNum).DeltaT;
                            DoWaterCoilSizing = true;
                        } else {
                            DoWaterCoilSizing = false;
                            // If there is no cooling Plant Sizing object and autosizing was requested, issue fatal error message
                            ShowSevereError(state, "Autosizing of water coil requires a cooling loop Sizing:Plant object");
                            ShowContinueError(state, "Occurs in " + FanCoil(FanCoilNum).UnitType + " Object=" + FanCoil(FanCoilNum).Name);
                            ErrorsFound = true;
                        }
                    }

                    if (DoWaterCoilSizing) {
                        SizingMethod = CoolingCapacitySizing;
                        if (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolMassFlow > 0.0) {
                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolOAFlowFrac =
                                min(FanCoil(FanCoilNum).OutAirVolFlow / state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolMassFlow,
                                    1.0);
                        } else {
                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolOAFlowFrac = 0.0;
                        }
                        if (FanCoil(FanCoilNum).HVACSizingIndex > 0) {
                            zoneHVACIndex = FanCoil(FanCoilNum).HVACSizingIndex;
                            CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                            if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                                if (CapSizingMethod == CoolingDesignCapacity) {
                                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0) {
                                        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                    } else {
                                        state.dataSize->DataFlowUsedForSizing =
                                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                                    }
                                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                } else if (CapSizingMethod == CapacityPerFloorArea) {
                                    if (state.dataSize->ZoneSizingRunDone) {
                                        CheckZoneSizing(state, CompType, CompName);
                                        PrintFlag = false;
                                        TempSize = AutoSize;
                                        state.dataSize->DataFlowUsedForSizing =
                                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                                        CoolingCapacitySizer sizerCoolingCapacity;
                                        sizerCoolingCapacity.overrideSizingString(SizingString);
                                        sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                                            sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                                        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                                    }
                                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity *
                                               state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                                    state.dataSize->DataScalableCapSizingON = true;
                                } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                                    PrintFlag = false;
                                    TempSize = AutoSize;
                                    state.dataSize->DataFlowUsedForSizing =
                                        state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                                    CoolingCapacitySizer sizerCoolingCapacity2;
                                    sizerCoolingCapacity2.overrideSizingString(SizingString);
                                    sizerCoolingCapacity2.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                    ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                                        sizerCoolingCapacity2.size(state, TempSize, ErrorsFound);
                                    ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                                    TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad *
                                               state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                    state.dataSize->DataScalableCapSizingON = true;
                                }
                            }
                            SizingString = "Cooling Design Capacity [W]";
                            PrintFlag = false;
                            CoolingCapacitySizer sizerCoolingCapacity3;
                            sizerCoolingCapacity3.overrideSizingString(SizingString);
                            sizerCoolingCapacity3.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                            DesCoilLoad = sizerCoolingCapacity3.size(state, TempSize, ErrorsFound);
                            state.dataSize->DataScalableCapSizingON = false;
                            state.dataSize->DataFlowUsedForSizing = 0.0;
                        } else {
                            SizingString = "Cooling Design Capacity [W]";
                            PrintFlag = false;
                            TempSize = AutoSize;
                            state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                            CoolingCapacitySizer sizerCoolingCapacity;
                            sizerCoolingCapacity.overrideSizingString(SizingString);
                            sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                            DesCoilLoad = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                        }
                        FanCoil(FanCoilNum).DesCoolingLoad = DesCoilLoad;
                        if (DesCoilLoad >= SmallLoad) {
                            rho = GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).FluidName,
                                                   5.,
                                                   state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).FluidIndex,
                                                   RoutineNameNoSpace);
                            Cp = GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).FluidName,
                                                       5.,
                                                       state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).FluidIndex,
                                                       RoutineNameNoSpace);
                            MaxColdWaterVolFlowDes = DesCoilLoad / (WaterCoilSizDeltaT * Cp * rho);
                        } else {
                            MaxColdWaterVolFlowDes = 0.0;
                        }
                    }
                }
                if (IsAutoSize) {
                    FanCoil(FanCoilNum).MaxColdWaterVolFlow = MaxColdWaterVolFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 FanCoil(FanCoilNum).UnitType,
                                                 FanCoil(FanCoilNum).Name,
                                                 "Design Size Maximum Cold Water Flow [m3/s]",
                                                 MaxColdWaterVolFlowDes);
                } else { // Hard size with sizing data
                    if (FanCoil(FanCoilNum).MaxColdWaterVolFlow > 0.0 && MaxColdWaterVolFlowDes > 0.0) {
                        MaxColdWaterVolFlowUser = FanCoil(FanCoilNum).MaxColdWaterVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     FanCoil(FanCoilNum).UnitType,
                                                     FanCoil(FanCoilNum).Name,
                                                     "Design Size Maximum Cold Water Flow [m3/s]",
                                                     MaxColdWaterVolFlowDes,
                                                     "User-Specified Maximum Cold Water Flow [m3/s]",
                                                     MaxColdWaterVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxColdWaterVolFlowDes - MaxColdWaterVolFlowUser) / MaxColdWaterVolFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil(FanCoilNum).UnitType + ' ' +
                                                FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, format("User-Specified Maximum Cold Water Flow of {:.5R}[m3/s]", MaxColdWaterVolFlowUser));
                                ShowContinueError(state,
                                                  format("differs from Design Size Maximum Cold Water Flow of {:.5R}[m3/s]", MaxColdWaterVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }

            if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::ASHRAE && !FanCoil(FanCoilNum).ASHRAETempControl) {

                CompType = FanCoil(FanCoilNum).UnitType;
                CompName = FanCoil(FanCoilNum).Name;
                PrintFlag = true;

                ZoneCoolingLoadSizer sizerZoneCoolingLoad;
                sizerZoneCoolingLoad.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                FanCoil(FanCoilNum).DesZoneCoolingLoad = sizerZoneCoolingLoad.size(state, FanCoil(FanCoilNum).DesZoneCoolingLoad, ErrorsFound);
                FanCoil(FanCoilNum).DesZoneCoolingLoad *= -1.0;

                ZoneHeatingLoadSizer sizerZoneHeatingLoad;
                sizerZoneHeatingLoad.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                FanCoil(FanCoilNum).DesZoneHeatingLoad = sizerZoneHeatingLoad.size(state, FanCoil(FanCoilNum).DesZoneHeatingLoad, ErrorsFound);

                FanCoil(FanCoilNum).DSOAPtr = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneDesignSpecOAIndex;

            } else if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::ASHRAE && FanCoil(FanCoilNum).ASHRAETempControl) {

                CompType = FanCoil(FanCoilNum).UnitType;
                CompName = FanCoil(FanCoilNum).Name;
                Real64 capacityMultiplier = 0.6; // 60% of design zone load for water coils
                state.dataSize->DataCapacityUsedForSizing = FanCoil(FanCoilNum).DesCoolingLoad * capacityMultiplier;
                CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);
                if (SizingDesRunThisZone) {
                    state.dataSize->DataCapacityUsedForSizing =
                        state.dataSize->FinalZoneSizing(FanCoil(FanCoilNum).ControlZoneNum).DesCoolLoad * capacityMultiplier;
                } else {
                    state.dataSize->DataCapacityUsedForSizing = FanCoil(FanCoilNum).DesCoolingLoad * capacityMultiplier;
                }
                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                PrintFlag = true;
                ASHRAEMinSATCoolingSizer sizerASHRAEMinSATCooling;
                sizerASHRAEMinSATCooling.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                FanCoil(FanCoilNum).DesignMinOutletTemp = sizerASHRAEMinSATCooling.size(state, FanCoil(FanCoilNum).DesignMinOutletTemp, ErrorsFound);

                if (SizingDesRunThisZone) {
                    state.dataSize->DataCapacityUsedForSizing =
                        state.dataSize->FinalZoneSizing(FanCoil(FanCoilNum).ControlZoneNum).DesHeatLoad * capacityMultiplier;
                } else {
                    state.dataSize->DataCapacityUsedForSizing = FanCoil(FanCoilNum).DesHeatingLoad * capacityMultiplier;
                }
                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow;
                ASHRAEMaxSATHeatingSizer sizerASHRAEMaxSATHeating;
                sizerASHRAEMaxSATHeating.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                FanCoil(FanCoilNum).DesignMaxOutletTemp = sizerASHRAEMaxSATHeating.size(state, FanCoil(FanCoilNum).DesignMaxOutletTemp, ErrorsFound);

                state.dataSize->DataCapacityUsedForSizing = 0.0; // reset so other routines don't use this inadvertently
                state.dataSize->DataFlowUsedForSizing = 0.0;

                SizingDesRunThisZone = false;
                CheckThisZoneForSizing(state, state.dataSize->CurZoneEqNum, SizingDesRunThisZone);

                if (SizingDesRunThisZone) {

                    FanCoil(FanCoilNum).DesZoneCoolingLoad =
                        -1.0 * (FanCoil(FanCoilNum).DesCoolingLoad / state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).CoolSizingFactor);
                    FanCoil(FanCoilNum).DesZoneHeatingLoad =
                        FanCoil(FanCoilNum).DesHeatingLoad / state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).HeatSizingFactor;
                    FanCoil(FanCoilNum).DSOAPtr = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneDesignSpecOAIndex;

                } else {

                    FanCoil(FanCoilNum).DesZoneCoolingLoad = -1.0 * FanCoil(FanCoilNum).DesCoolingLoad;
                    FanCoil(FanCoilNum).DesZoneHeatingLoad = FanCoil(FanCoilNum).DesHeatingLoad;
                }
            }

        } // if ( CurZoneEqNum > 0 )

        // set the design air flow rates for the heating and cooling coils
        if (UtilityRoutines::SameString(FanCoil(FanCoilNum).CCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
            CoolingCoilName = GetHXDXCoilName(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, ErrorsFound);
            CoolingCoilType = GetHXCoilType(state, FanCoil(FanCoilNum).CCoilType, FanCoil(FanCoilNum).CCoilName, ErrorsFound);
        } else {
            CoolingCoilName = FanCoil(FanCoilNum).CCoilName;
            CoolingCoilType = FanCoil(FanCoilNum).CCoilType;
        }
        if (state.dataSize->ZoneSizingRunDone) {
            WaterCoils::SetCoilDesFlow(
                state, CoolingCoilType, CoolingCoilName, state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow, ErrorsFound);
            WaterCoils::SetCoilDesFlow(state,
                                       FanCoil(FanCoilNum).HCoilType,
                                       FanCoil(FanCoilNum).HCoilName,
                                       state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow,
                                       ErrorsFound);
        } else {
            WaterCoils::SetCoilDesFlow(state, CoolingCoilType, CoolingCoilName, FanCoil(FanCoilNum).MaxAirVolFlow, ErrorsFound);
            WaterCoils::SetCoilDesFlow(
                state, FanCoil(FanCoilNum).HCoilType, FanCoil(FanCoilNum).HCoilName, FanCoil(FanCoilNum).MaxAirVolFlow, ErrorsFound);
        }
        if (state.dataSize->CurZoneEqNum > 0) {
            ZoneEqSizing(state.dataSize->CurZoneEqNum).MaxHWVolFlow = FanCoil(FanCoilNum).MaxHotWaterVolFlow;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).MaxCWVolFlow = FanCoil(FanCoilNum).MaxColdWaterVolFlow;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = FanCoil(FanCoilNum).MaxAirVolFlow;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad = FanCoil(FanCoilNum).DesCoolingLoad;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad = FanCoil(FanCoilNum).DesHeatingLoad;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesignSizeFromParent = true;
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void Sim4PipeFanCoil(EnergyPlusData &state,
                         int &FanCoilNum,               // number of the current fan coil unit being simulated
                         int const ZoneNum,             // number of zone being served
                         int const ControlledZoneNum,   // index into ZoneEqupConfig
                         bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                         Real64 &PowerMet,              // Sensible power supplied (W)
                         Real64 &LatOutputProvided      // Latent power supplied (kg/s), negative = dehumidification
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2000
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //       MODIFIED       Arnaud Flament June 2010 (added airflow capacity control methods)
        //       MODIFIED      R. Raustad, FSEC, Feb 2016 (added ASHRAE 90.1 SZVAV system control)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a 4 pipe fan coil unit; adjust its output to match the
        // remaining zone load.

        // METHODOLOGY EMPLOYED:
        // If unit is on, calls ControlCompOutput to obtain the desired unit output

        // REFERENCES:
        // SZVAV sysetm control:
        // ASHRAE 90.1 2010 Section 6.4.3.10 - Single Zone Variable-Air-volume Controls (described in Trane newsletter entitled Understanding
        // Single-Zone VAV Systems) Trane Engineers Newsletter -
        // https://www.trane.com/content/dam/Trane/Commercial/global/products-systems/education-training/engineers-newsletters/airside-design/admapn047en_0413.pdf
        //

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
        using General::SolveRoot;

        using PlantUtilities::SetComponentFlowRate;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using namespace DataPlant;
        using namespace DataLoopNode;
        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIterCycl(100);

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 QZnReq;        // heating or cooling needed by zone [watts]
        Real64 QUnitOut;      // heating or sens. cooling provided by fan coil unit [watts]
        Real64 QUnitOutMax;   // heating or sens. cooling provided by fan coil unit (running during an entire timestep)
        Real64 PLR;           // Part Load Ratio, fraction of time step fancoil is on
        bool UnitOn;          // TRUE if unit is on
        int ControlNode;      // the hot water or cold water inlet node
        Real64 ControlOffset; // tolerance for output control
        Real64 MaxWaterFlow;  // maximum water flow for heating or cooling [kg/sec]
        Real64 MinWaterFlow;  // minimum water flow for heating or cooling [kg/sec]
        Real64 PLRMin;        // minimum PLR used for tighter control of air and water flow rate
        Real64 PLRMax;        // maximum PLR used for tighter control of air and water flow rate
        int OutletNode;       // unit air outlet node
        int InletNode;        // unit air inlet node
        Real64 QTotUnitOut;   // total unit output [watts]
        Real64 AirMassFlow;   // air mass flow rate [kg/sec]
        Real64 QUnitOutNoHC;  // unit output with no active heating or cooling [W]
        Real64 QUnitOutMaxC;  // unit output with full active cooling [W]
        Real64 QUnitOutMaxH;  // unit output with full active heating [W]
        Real64 QCoilHeatSP;   // coil load to the heating setpoint [W]
        Real64 QCoilCoolSP;   // coil load to the cooling setpoint [W]
        Real64 LatentOutput;  // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
        Real64 SpecHumOut;    // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        Real64 SpecHumIn;     // Specific humidity ratio of inlet air (kg moisture / kg moist air)
        Real64 Error;         // Error between QZnReq and QUnitOut
        Real64 AbsError;      // Absolute error between QZnReq and QUnitOut [W]   !FB
        int Iter;             // iteration counter
        Real64 Relax;
        Real64 DelPLR;
        Real64 mdot;
        // Real64 Low_mdot;
        Real64 QSensUnitOutNoATM;     // unit output not including air added by supply side air terminal mixer
        int SolFlag;                  // return flag from RegulaFalsi for sensible load
        Array1D<Real64> Par(10);      // parameters passed to RegulaFalsi function
        Real64 ElectricHeaterControl; // 1 or 0, enables or disables heating coil
        Real64 OAVolumeFlowRate;      // OA volume flow rate based on design specifications object [m3/s]
        Real64 OAMassFlow;            // OA mass flow rate based on design specifications object [kg/s]
        Real64 RhoAir;                // density of air [kg/m3]
        Real64 MinSAMassFlowRate;     // minimum supply air mass flow rate [kg/s]
        Real64 MaxSAMassFlowRate;     // maximum supply air mass flow rate [kg/s]
        // Real64 FCOutletTempOn;        // ASHRAE outlet air temperature when coil is on [C]
        Real64 HWFlow;       // hot water mass flow rate solution [kg/s]
        Real64 MdotLockH;    // saved value of locked chilled water mass flow rate [kg/s]
        Real64 MdotLockC;    // saved value of locked hot water mass flow rate [kg/s]
        Real64 CWFlow;       // cold water mass flow rate solution [kg/s]
        Real64 CWFlowBypass; // cold water bypassed mass flow rate [kg/s]
        Real64 HWFlowBypass; // hot water bypassed mass flow rate [kg/s]
        bool ColdFlowLocked; // if true cold water flow is locked
        bool HotFlowLocked;  // if true Hot water flow is locked

        auto &Node(state.dataLoopNodes->Node);

        // initialize local variables
        UnitOn = true;
        ControlNode = 0;
        QUnitOut = 0.0;
        QUnitOutMax = 0.0;
        PLR = 0.0;
        LatentOutput = 0.0;
        QUnitOutNoHC = 0.0;
        QCoilHeatSP = 0.0;
        QCoilCoolSP = 0.0;
        QZnReq = 0.0;
        ControlOffset = 0.0;
        MaxWaterFlow = 0.0;
        MinWaterFlow = 0.0;
        OutletNode = state.dataFanCoilUnits->FanCoil(FanCoilNum).AirOutNode;
        InletNode = state.dataFanCoilUnits->FanCoil(FanCoilNum).AirInNode;
        AirMassFlow = Node(InletNode).MassFlowRate;
        Error = 1.0;
        AbsError = 2.0 * SmallLoad;
        Iter = 0;
        Relax = 1.0;
        ElectricHeaterControl = 0.0;
        HWFlow = 0.0;
        HWFlowBypass = 0.0;
        MdotLockH = 0.0;
        MdotLockC = 0.0;
        ColdFlowLocked = false;
        HotFlowLocked = false;

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);

        // select capacity control method
        {
            auto const SELECT_CASE_var(FanCoil(FanCoilNum).CapCtrlMeth_Num);

            // constant fan variable flow
            if (SELECT_CASE_var == CCM::ConsFanVarFlow) {

                if (AirMassFlow < SmallMassFlow) UnitOn = false;
                // zero the hot & cold water flows

                // set water coil flow rate to 0 to calculate coil off capacity (only valid while flow is unlocked)
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                     FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopSide,
                                     FanCoil(FanCoilNum).CoolCoilBranchNum,
                                     FanCoil(FanCoilNum).CoolCoilCompNum);
                if (state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).LoopSide(FanCoil(FanCoilNum).CoolCoilLoopSide).FlowLock ==
                    DataPlant::iFlowLock::Locked) {
                    ColdFlowLocked = true; // check for flow lock
                }
                if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                         FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopSide,
                                         FanCoil(FanCoilNum).HeatCoilBranchNum,
                                         FanCoil(FanCoilNum).HeatCoilCompNum);
                    if (state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).LoopSide(FanCoil(FanCoilNum).HeatCoilLoopSide).FlowLock ==
                        DataPlant::iFlowLock::Locked) {
                        HotFlowLocked = true; // save locked flow
                    }
                }
                // obtain unit output with no active heating/cooling
                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, 0.0);

                if (ColdFlowLocked || HotFlowLocked) {
                    QUnitOutNoHC = FanCoil(FanCoilNum).QUnitOutNoHC;
                } else { // continue to update QUnitOutNoHC while flow is unlocked
                    FanCoil(FanCoilNum).QUnitOutNoHC = QUnitOutNoHC;
                }

                // then calculate the loads at the coils
                QCoilHeatSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP - QUnitOutNoHC;
                QCoilCoolSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP - QUnitOutNoHC;

                // if cooling
                if (UnitOn && QCoilCoolSP < (-1.0 * SmallLoad) && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
                    ControlNode = FanCoil(FanCoilNum).CoolCoilFluidInletNode;
                    ControlOffset = FanCoil(FanCoilNum).ColdControlOffset;
                    MaxWaterFlow = FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    MinWaterFlow = FanCoil(FanCoilNum).MinColdWaterFlow;
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if (!FirstHVACIteration) {
                        MaxWaterFlow = Node(ControlNode).MassFlowRateMaxAvail;
                        MinWaterFlow = Node(ControlNode).MassFlowRateMinAvail;
                    }
                    // get full load result
                    mdot = MaxWaterFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                         FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopSide,
                                         FanCoil(FanCoilNum).CoolCoilBranchNum,
                                         FanCoil(FanCoilNum).CoolCoilCompNum);
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMaxC);
                    if (!ColdFlowLocked) {
                        FanCoil(FanCoilNum).QUnitOutMaxC = QUnitOutMaxC;
                    } else {
                        QUnitOutMaxC = FanCoil(FanCoilNum).QUnitOutMaxC;
                        MdotLockC = mdot; // save locked flow
                    }
                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
                    if (QUnitOutMaxC < QZnReq) {
                        // more cooling than required, find reduced water flow rate to meet the load
                        // solve for the cold water flow rate with no limit set by flow rate lockdown
                        Par(1) = double(FanCoilNum);
                        Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                        if (FirstHVACIteration) Par(2) = 1.0;
                        Par(3) = ControlledZoneNum;
                        Par(4) = QZnReq;
                        General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, CWFlow, CalcFanCoilCWLoadResidual, 0.0, MaxWaterFlow, Par);
                        if (SolFlag == -1) {
                            // tighten limits on water flow rate to see if this allows convergence
                            state.dataFanCoilUnits->CoolingLoad = true;
                            state.dataFanCoilUnits->HeatingLoad = false;
                            TightenWaterFlowLimits(state,
                                                   FanCoilNum,
                                                   state.dataFanCoilUnits->CoolingLoad,
                                                   state.dataFanCoilUnits->HeatingLoad,
                                                   FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                                   ControlledZoneNum,
                                                   FirstHVACIteration,
                                                   QZnReq,
                                                   MinWaterFlow,
                                                   MaxWaterFlow);
                            General::SolveRoot(
                                state, 0.001, MaxIterCycl, SolFlag, CWFlow, CalcFanCoilCWLoadResidual, MinWaterFlow, MaxWaterFlow, Par);
                            if (SolFlag == -1) {
                                ++FanCoil(FanCoilNum).ConvgErrCountC;
                                if (FanCoil(FanCoilNum).ConvgErrCountC < 2) {
                                    ShowWarningError(state, "Cold Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                    ShowContinueError(state, "  Iteration limit exceeded in calculating water flow rate ");
                                    Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate = CWFlow;
                                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                                    ShowContinueErrorTimeStamp(state, format("Load Request = {}, Final Capacity = {}", QZnReq, QUnitOut));
                                    ShowContinueErrorTimeStamp(
                                        state,
                                        format("Min water flow used during iterations = {}, Max water flow used during iterations = {}",
                                               MinWaterFlow,
                                               MaxWaterFlow));
                                    ShowContinueErrorTimeStamp(state, format("Water flow rate on last iteration = {}", CWFlow));
                                    ShowContinueErrorTimeStamp(state, "..Water flow rate set to last iteration value ");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   "Cold water flow Iteration limit exceeded in fan coil unit " +
                                                                       FanCoil(FanCoilNum).Name,
                                                                   FanCoil(FanCoilNum).MaxIterIndexC);
                                }
                            } else if (SolFlag == -2) {
                                ++FanCoil(FanCoilNum).LimitErrCountC;
                                if (FanCoil(FanCoilNum).LimitErrCountC < 2) {
                                    ShowWarningError(state, "Cold Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                    ShowContinueError(state, "  Bad cold water mass flow limits");
                                    ShowContinueErrorTimeStamp(state, "..Water flow rate set to lower limit ");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   "Cold Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name,
                                                                   FanCoil(FanCoilNum).BadMassFlowLimIndexC);
                                }
                            }
                        } else if (SolFlag == -2) {
                            ++FanCoil(FanCoilNum).LimitErrCountC;
                            if (FanCoil(FanCoilNum).LimitErrCountC < 2) {
                                ShowWarningError(state, "Cold Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, "  Bad cold water mass flow limits");
                                ShowContinueErrorTimeStamp(state, "..Water flow rate set to lower limit ");
                            } else {
                                ShowRecurringWarningErrorAtEnd(state,
                                                               "Cold Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name,
                                                               FanCoil(FanCoilNum).BadMassFlowLimIndexC);
                            }
                        }
                    } else {
                        // demand greater than capacity
                        CWFlow = MaxWaterFlow;
                    }
                    if (!ColdFlowLocked) {
                        mdot = CWFlow; // not flowlocked - set flow to CWFlow
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                             FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopSide,
                                             FanCoil(FanCoilNum).CoolCoilBranchNum,
                                             FanCoil(FanCoilNum).CoolCoilCompNum);
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut); // get QUnitOut
                    } else {
                        // flow lock on
                        if (MdotLockC > CWFlow) { // if mdot > CWFlow, bypass extra flow
                            Calc4PipeFanCoil(state,
                                             FanCoilNum,
                                             ControlledZoneNum,
                                             FirstHVACIteration,
                                             QUnitOut); // get QUnitOut with CWFlow; rest will be bypassed
                            Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate =
                                MdotLockC; // reset flow to locked value. Since lock is on, must do this by hand
                            Node(FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum).MassFlowRate = MdotLockC;
                            // Keep soln flow rate but reset outlet water temperature - i.e. bypass extra water
                            CWFlowBypass = MdotLockC - CWFlow;
                            // change water outlet temperature and enthalpy
                            Node(FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum).Temp =
                                (CWFlowBypass * Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).Temp +
                                 CWFlow * Node(FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum).Temp) /
                                MdotLockC;
                            Node(FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum).Enthalpy =
                                (CWFlowBypass * Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).Enthalpy +
                                 CWFlow * Node(FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum).Enthalpy) /
                                MdotLockC;
                        } else {
                            // if MdotLockC <= CWFlow use MdotLockC as is
                            Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate =
                                MdotLockC; // reset flow to locked value. Since lock is on, must do this by hand
                            Node(FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum).MassFlowRate = MdotLockC;
                            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                        }
                    }
                    CalcZoneSensibleOutput(AirMassFlow, Node(OutletNode).Temp, Node(InletNode).Temp, Node(InletNode).HumRat, QUnitOut);

                    // if heating
                } else if (UnitOn && QCoilHeatSP > SmallLoad && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint) {
                    // get full load result
                    if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) { // if HW Coil
                        ControlNode = FanCoil(FanCoilNum).HeatCoilFluidInletNode;
                        ControlOffset = FanCoil(FanCoilNum).HotControlOffset;
                        MaxWaterFlow = FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                        MinWaterFlow = FanCoil(FanCoilNum).MinHotWaterFlow;
                        // On the first HVAC iteration the system values are given to the controller, but after that
                        // the demand limits are in place and there needs to be feedback to the Zone Equipment
                        if (!FirstHVACIteration) {
                            MaxWaterFlow = Node(ControlNode).MassFlowRateMaxAvail;
                            MinWaterFlow = Node(ControlNode).MassFlowRateMinAvail;
                        }
                        mdot = MaxWaterFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                             FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopSide,
                                             FanCoil(FanCoilNum).HeatCoilBranchNum,
                                             FanCoil(FanCoilNum).HeatCoilCompNum);
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMaxH);
                        if (!HotFlowLocked) {
                            FanCoil(FanCoilNum).QUnitOutMaxH = QUnitOutMaxH;
                        } else {
                            QUnitOutMaxH = FanCoil(FanCoilNum).QUnitOutMaxH;
                            MdotLockH = mdot; // save locked flow
                        }
                    } else {
                        // not HW coil
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMaxH, 1.0);
                    }
                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
                    if (QUnitOutMaxH > QZnReq) {
                        // more heating than required, find reduced water flow rate to meet the load
                        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                            // solve for the hot water flow rate with no limit set by flow rate lockdown
                            Par(1) = double(FanCoilNum);
                            Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = ControlledZoneNum;
                            Par(4) = QZnReq;
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, HWFlow, CalcFanCoilHWLoadResidual, 0.0, MaxWaterFlow, Par);
                            if (SolFlag == -1) {
                                // tighten limits on water flow rate to see if this allows convergence
                                state.dataFanCoilUnits->CoolingLoad = false;
                                state.dataFanCoilUnits->HeatingLoad = true;
                                TightenWaterFlowLimits(state,
                                                       FanCoilNum,
                                                       state.dataFanCoilUnits->CoolingLoad,
                                                       state.dataFanCoilUnits->HeatingLoad,
                                                       FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                                       ControlledZoneNum,
                                                       FirstHVACIteration,
                                                       QZnReq,
                                                       MinWaterFlow,
                                                       MaxWaterFlow);
                                General::SolveRoot(
                                    state, 0.001, MaxIterCycl, SolFlag, HWFlow, CalcFanCoilHWLoadResidual, MinWaterFlow, MaxWaterFlow, Par);
                                if (SolFlag == -1) {
                                    ++FanCoil(FanCoilNum).ConvgErrCountH;
                                    if (FanCoil(FanCoilNum).ConvgErrCountH < 2) {
                                        ShowWarningError(state, "Hot Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                        ShowContinueError(state, "  Iteration limit exceeded in calculating water flow rate ");
                                        Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate = HWFlow;
                                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                                        ShowContinueErrorTimeStamp(state, format("Load Request = {}, Final Capacity = {}", QZnReq, QUnitOut));
                                        ShowContinueErrorTimeStamp(
                                            state,
                                            format("Min water flow used during iterations = {}, Max water flow used during iterations = {}",
                                                   MinWaterFlow,
                                                   MaxWaterFlow));
                                        ShowContinueErrorTimeStamp(state, format("Water flow rate on last iteration = {}", HWFlow));
                                        ShowContinueErrorTimeStamp(state, "..Water flow rate set to last iteration value ");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       "Hot water flow Iteration limit exceeded in fan coil unit " +
                                                                           FanCoil(FanCoilNum).Name,
                                                                       FanCoil(FanCoilNum).MaxIterIndexH);
                                    }
                                } else if (SolFlag == -2) {
                                    ++FanCoil(FanCoilNum).LimitErrCountH;
                                    if (FanCoil(FanCoilNum).LimitErrCountH < 2) {
                                        ShowWarningError(state, "Hot Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                        ShowContinueError(state, "  Bad hot water mass flow limits");
                                        ShowContinueErrorTimeStamp(state, "..Water flow rate set to lower limit ");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       "Hot Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name,
                                                                       FanCoil(FanCoilNum).BadMassFlowLimIndexH);
                                    }
                                }
                            } else if (SolFlag == -2) {
                                ++FanCoil(FanCoilNum).LimitErrCountH;
                                if (FanCoil(FanCoilNum).LimitErrCountH < 2) {
                                    ShowWarningError(state, "Hot Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                    ShowContinueError(state, "  Bad hot water mass flow limits");
                                    ShowContinueErrorTimeStamp(state, "..Water flow rate set to lower limit ");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   "Hot Water control failed in fan coil unit " + FanCoil(FanCoilNum).Name,
                                                                   FanCoil(FanCoilNum).BadMassFlowLimIndexH);
                                }
                            }
                        } else {
                            Par(1) = double(FanCoilNum);
                            Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = ControlledZoneNum;
                            Par(4) = QZnReq;
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilLoadResidual, 0.0, 1.0, Par);
                        }
                    } else {
                        // demand greater than capacity
                        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                            HWFlow = MaxWaterFlow;
                        } else {
                            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0);
                        }
                    }
                    if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                        if (!HotFlowLocked) {
                            mdot = HWFlow; // not flowlocked - set flow to HWFlow
                            SetComponentFlowRate(state,
                                                 mdot,
                                                 FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                                 FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopSide,
                                                 FanCoil(FanCoilNum).HeatCoilBranchNum,
                                                 FanCoil(FanCoilNum).HeatCoilCompNum);
                            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut); // get QUnitOut
                        } else {
                            // flow lock on
                            if (MdotLockH > HWFlow) { // if mdot > HWFlow, bypass extra flow
                                Calc4PipeFanCoil(state,
                                                 FanCoilNum,
                                                 ControlledZoneNum,
                                                 FirstHVACIteration,
                                                 QUnitOut); // get QUnitOut with HWFlow; rest will be bypassed
                                Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate =
                                    MdotLockH; // reset flow to locked value. Since lock is on, must do this by hand
                                Node(FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum).MassFlowRate = MdotLockH;
                                // Keep soln flow rate but reset outlet water temperature - i.e. bypass extra water
                                HWFlowBypass = MdotLockH - HWFlow;
                                // change outlet water temperature and enthalpy
                                Node(FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum).Temp =
                                    (HWFlowBypass * Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).Temp +
                                     HWFlow * Node(FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum).Temp) /
                                    MdotLockH;
                                Node(FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum).Enthalpy =
                                    (HWFlowBypass * Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).Enthalpy +
                                     HWFlow * Node(FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum).Enthalpy) /
                                    MdotLockH;
                            } else {
                                // if MdotLockH <= HWFlow use MdotLockH as is
                                Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate =
                                    MdotLockH; // reset flow to locked value. Since lock is on, must do this by hand
                                Node(FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum).MassFlowRate = MdotLockH;
                                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                            }
                        }
                    }
                    CalcZoneSensibleOutput(AirMassFlow, Node(OutletNode).Temp, Node(InletNode).Temp, Node(InletNode).HumRat, QUnitOut);
                } else {
                    // no action
                    QUnitOut = QUnitOutNoHC;
                }

                // CR9155 Remove specific humidity calculations
                SpecHumOut = Node(OutletNode).HumRat;
                SpecHumIn = Node(InletNode).HumRat;
                LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
                QTotUnitOut = AirMassFlow * (Node(OutletNode).Enthalpy - Node(InletNode).Enthalpy);
                // report variables
                FanCoil(FanCoilNum).HeatPower = max(0.0, QUnitOut);
                FanCoil(FanCoilNum).SensCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QUnitOut));
                FanCoil(FanCoilNum).TotCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QTotUnitOut));
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanCoil(FanCoilNum).ElecPower = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanCoil(FanCoilNum).ElecPower = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }

                PowerMet = QUnitOut;
                LatOutputProvided = LatentOutput;

                // cycling fan constant water flow AND VarFanVarFlow
            } else if ((SELECT_CASE_var == CCM::CycFan) || (SELECT_CASE_var == CCM::VarFanVarFlow)) {

                if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || AirMassFlow < SmallMassFlow) UnitOn = false;

                // zero the hot & cold water flows
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                     FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopSide,
                                     FanCoil(FanCoilNum).CoolCoilBranchNum,
                                     FanCoil(FanCoilNum).CoolCoilCompNum);
                if (state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).CoolCoilLoopNum).LoopSide(FanCoil(FanCoilNum).CoolCoilLoopSide).FlowLock ==
                    DataPlant::iFlowLock::Locked) {
                    ColdFlowLocked = true; // check for flow lock
                }
                if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                         FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopSide,
                                         FanCoil(FanCoilNum).HeatCoilBranchNum,
                                         FanCoil(FanCoilNum).HeatCoilCompNum);
                    if (state.dataPlnt->PlantLoop(FanCoil(FanCoilNum).HeatCoilLoopNum).LoopSide(FanCoil(FanCoilNum).HeatCoilLoopSide).FlowLock ==
                        DataPlant::iFlowLock::Locked) {
                        HotFlowLocked = true; // save locked flow
                    }
                }

                // obtain unit output with no active heating/cooling
                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, 0.0);

                // get the loads at the coil
                QCoilHeatSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP - QUnitOutNoHC;
                QCoilCoolSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP - QUnitOutNoHC;

                // speed fan selection only for multispeed cycling fan
                if (UnitOn && (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::CycFan)) {
                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;

                    // set water side mass flow rate
                    if (QCoilCoolSP < 0) {
                        Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate = FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    } else if (QCoilHeatSP > 0 && FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
                        Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate = FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                    }

                    Node(InletNode).MassFlowRateMax = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                    FanCoil(FanCoilNum).SpeedFanSel = 1;
                    FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).LowSpeedRatio;
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax);
                    if (std::abs(QUnitOutMax) < std::abs(QZnReq)) {
                        Node(InletNode).MassFlowRateMax = FanCoil(FanCoilNum).MedSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                        FanCoil(FanCoilNum).SpeedFanSel = 2;
                        FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).MedSpeedRatio;
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax);
                    }
                    if (std::abs(QUnitOutMax) < std::abs(QZnReq)) {
                        FanCoil(FanCoilNum).SpeedFanSel = 3;
                        FanCoil(FanCoilNum).SpeedFanRatSel = 1.0;
                        Node(InletNode).MassFlowRateMax = FanCoil(FanCoilNum).MaxAirMassFlow;
                    }
                } else {
                    FanCoil(FanCoilNum).SpeedFanSel = 0;
                }

                // meet the coil load adjusted for fan operation
                if (UnitOn && QCoilCoolSP < (-1.0 * SmallLoad) && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
                    // cooling coil action, maximum cold water flow
                    mdot = FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                         FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopSide,
                                         FanCoil(FanCoilNum).CoolCoilBranchNum,
                                         FanCoil(FanCoilNum).CoolCoilCompNum);

                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
                    ControlOffset = FanCoil(FanCoilNum).ColdControlOffset;

                    // get the maximum output of the fcu
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax); // call without PLR means PLR = 1

                    if (QUnitOutMax < QZnReq) {
                        // more cooling than required, find reduced air and water flow rate to meet the load
                        // solve for the cold water flow rate with no limit set by flow rate lockdown
                        Par(1) = double(FanCoilNum);
                        Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                        if (FirstHVACIteration) Par(2) = 1.0;
                        Par(3) = ControlledZoneNum;
                        Par(4) = QZnReq;
                        Par(5) = double(FanCoil(FanCoilNum).CoolCoilFluidInletNode);
                        General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilPLRResidual, 0.0, 1.0, Par);
                        if (SolFlag == -1) {
                            // tighten limits on water flow rate to see if this allows convergence
                            state.dataFanCoilUnits->CoolingLoad = true;
                            state.dataFanCoilUnits->HeatingLoad = false;
                            TightenAirAndWaterFlowLimits(state,
                                                         FanCoilNum,
                                                         state.dataFanCoilUnits->CoolingLoad,
                                                         state.dataFanCoilUnits->HeatingLoad,
                                                         FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                                         ControlledZoneNum,
                                                         FirstHVACIteration,
                                                         QZnReq,
                                                         PLRMin,
                                                         PLRMax);
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilPLRResidual, PLRMin, PLRMax, Par);
                            if (SolFlag == -1) {
                                ++FanCoil(FanCoilNum).ConvgErrCountC;
                                if (FanCoil(FanCoilNum).ConvgErrCountC < 2) {
                                    ShowWarningError(state, "Part-load ratio cooling control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                    ShowContinueError(state, "  Iteration limit exceeded in calculating FCU part-load ratio ");
                                    Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                                    ShowContinueErrorTimeStamp(state, format("Load Request = {}, Final Capacity = {}", QZnReq, QUnitOut));
                                    ShowContinueErrorTimeStamp(
                                        state,
                                        format(
                                            "Min part-load used during iterations = {}, Max part-load used during iterations = {}", PLRMin, PLRMax));
                                    ShowContinueErrorTimeStamp(state, format("Part-load ratio on last iteration = {}", PLR));
                                    ShowContinueErrorTimeStamp(state, "..Part-load ratio set to last iteration value ");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   "Part-load ratio cooling iteration limit exceeded in fan coil unit " +
                                                                       FanCoil(FanCoilNum).Name,
                                                                   FanCoil(FanCoilNum).MaxIterIndexC);
                                }
                            } else if (SolFlag == -2) {
                                ++FanCoil(FanCoilNum).LimitErrCountC;
                                if (FanCoil(FanCoilNum).LimitErrCountC < 2) {
                                    ShowWarningError(state, "Part-load ratio cooling control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                    ShowContinueError(state, "  Bad part-load ratio limits");
                                    ShowContinueErrorTimeStamp(state, format("..Part-load ratio set to {}", PLRMin));
                                } else {
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   "Part-load ratio cooling control failed in fan coil unit " +
                                                                       FanCoil(FanCoilNum).Name,
                                                                   FanCoil(FanCoilNum).BadMassFlowLimIndexC);
                                }
                            }
                        } else if (SolFlag == -2) {
                            ++FanCoil(FanCoilNum).LimitErrCountC;
                            if (FanCoil(FanCoilNum).LimitErrCountC < 2) {
                                ShowWarningError(state, "Part-load ratio control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, "  Bad part-load ratio limits");
                                ShowContinueErrorTimeStamp(state, "..Part-load ratio set to 0");
                            } else {
                                ShowRecurringWarningErrorAtEnd(state,
                                                               "Part-load ratio control failed in fan coil unit " + FanCoil(FanCoilNum).Name,
                                                               FanCoil(FanCoilNum).BadMassFlowLimIndexC);
                            }
                        }
                        mdot = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                             FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopSide,
                                             FanCoil(FanCoilNum).CoolCoilBranchNum,
                                             FanCoil(FanCoilNum).CoolCoilCompNum);
                    } else {
                        PLR = 1.0;
                        mdot = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                             FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopSide,
                                             FanCoil(FanCoilNum).CoolCoilBranchNum,
                                             FanCoil(FanCoilNum).CoolCoilCompNum);
                    }

                    // at the end calculate output
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);

                } else if (UnitOn && QCoilHeatSP > SmallLoad && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint) {
                    // heating coil action, maximun hot water flow

                    if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                        mdot = FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                             FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopSide,
                                             FanCoil(FanCoilNum).HeatCoilBranchNum,
                                             FanCoil(FanCoilNum).HeatCoilCompNum);
                    }

                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
                    ControlOffset = FanCoil(FanCoilNum).HotControlOffset;

                    // get the maximum output of the fcu
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax);
                    // calculate the PLR, if load greater than output, PLR = 1 (output = max)
                    if (QUnitOutMax > QZnReq) {
                        // more heating than required, find reduced water flow rate to meet the load
                        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                            // solve for the hot water flow rate with no limit set by flow rate lockdown
                            Par(1) = double(FanCoilNum);
                            Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = ControlledZoneNum;
                            Par(4) = QZnReq;
                            Par(5) = double(FanCoil(FanCoilNum).HeatCoilFluidInletNode);
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilPLRResidual, 0.0, 1.0, Par);
                            if (SolFlag == -1) {
                                // tighten limits on water flow rate to see if this allows convergence
                                state.dataFanCoilUnits->CoolingLoad = false;
                                state.dataFanCoilUnits->HeatingLoad = true;
                                TightenAirAndWaterFlowLimits(state,
                                                             FanCoilNum,
                                                             state.dataFanCoilUnits->CoolingLoad,
                                                             state.dataFanCoilUnits->HeatingLoad,
                                                             FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                                             ControlledZoneNum,
                                                             FirstHVACIteration,
                                                             QZnReq,
                                                             PLRMin,
                                                             PLRMax);
                                General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilPLRResidual, PLRMin, PLRMax, Par);
                                if (SolFlag == -1) {
                                    ++FanCoil(FanCoilNum).ConvgErrCountH;
                                    if (FanCoil(FanCoilNum).ConvgErrCountH < 2) {
                                        ShowWarningError(state,
                                                         "Part-load ratio heating control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                        ShowContinueError(state, "  Iteration limit exceeded in calculating FCU part-load ratio ");
                                        Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate =
                                            PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                                        ShowContinueErrorTimeStamp(state, format("Load Request = {}, Final Capacity = {}", QZnReq, QUnitOut));
                                        ShowContinueErrorTimeStamp(
                                            state,
                                            format("Min part-load ratio used during iterations = {}, Max part-load used during iterations = {}",
                                                   PLRMin,
                                                   PLRMax));
                                        ShowContinueErrorTimeStamp(state, format("Part-load ratio on last iteration = {}", PLR));
                                        ShowContinueErrorTimeStamp(state, "..Part-load ratio set to last iteration value ");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       "Part-load ratio heating iteration limit exceeded in fan coil unit " +
                                                                           FanCoil(FanCoilNum).Name,
                                                                       FanCoil(FanCoilNum).MaxIterIndexH);
                                    }
                                } else if (SolFlag == -2) {
                                    ++FanCoil(FanCoilNum).LimitErrCountH;
                                    if (FanCoil(FanCoilNum).LimitErrCountH < 2) {
                                        ShowWarningError(state,
                                                         "Part-load ratio heating control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                        ShowContinueError(state, "  Bad hot part-load ratio limits");
                                        ShowContinueErrorTimeStamp(state, format("..Part-load ratio set to {}", PLRMin));
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       "Part-load ratio heating control failed in fan coil unit " +
                                                                           FanCoil(FanCoilNum).Name,
                                                                       FanCoil(FanCoilNum).BadMassFlowLimIndexH);
                                    }
                                }
                            } else if (SolFlag == -2) {
                                ++FanCoil(FanCoilNum).LimitErrCountH;
                                if (FanCoil(FanCoilNum).LimitErrCountH < 2) {
                                    ShowWarningError(state, "Part-load ratio heating control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                    ShowContinueError(state, "  Bad part-load ratio limits");
                                    ShowContinueErrorTimeStamp(state, "..Part-load ratio set to 0");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   "Part-load ratio heating control failed in fan coil unit " +
                                                                       FanCoil(FanCoilNum).Name,
                                                                   FanCoil(FanCoilNum).BadMassFlowLimIndexH);
                                }
                            }
                            HWFlow = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                            SetComponentFlowRate(state,
                                                 HWFlow,
                                                 FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                                 FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopSide,
                                                 FanCoil(FanCoilNum).HeatCoilBranchNum,
                                                 FanCoil(FanCoilNum).HeatCoilCompNum);

                        } else {
                            Par(1) = double(FanCoilNum);
                            Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = ControlledZoneNum;
                            Par(4) = QZnReq;
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilLoadResidual, 0.0, 1.0, Par);
                        }
                    } else {
                        PLR = 1.0;
                        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                            mdot = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                            SetComponentFlowRate(state,
                                                 mdot,
                                                 FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                                 FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopSide,
                                                 FanCoil(FanCoilNum).HeatCoilBranchNum,
                                                 FanCoil(FanCoilNum).HeatCoilCompNum);
                        }
                    }

                    // at the end calculate output with adjusted PLR
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);

                } else {
                    // no action, zero the air flow rate, the unit is off
                    Node(InletNode).MassFlowRate = 0.0;
                    Node(OutletNode).MassFlowRate = 0.0;
                    FanCoil(FanCoilNum).SpeedFanSel = 0;
                    PLR = 0.0;
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                }

                AirMassFlow = Node(InletNode).MassFlowRate;
                // CR9155 Remove specific humidity calculations
                SpecHumOut = Node(OutletNode).HumRat;
                SpecHumIn = Node(InletNode).HumRat;
                LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
                QTotUnitOut = AirMassFlow * (Node(OutletNode).Enthalpy - Node(InletNode).Enthalpy);
                // report variables
                FanCoil(FanCoilNum).HeatPower = max(0.0, QUnitOut);
                FanCoil(FanCoilNum).SensCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QUnitOut));
                FanCoil(FanCoilNum).TotCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QTotUnitOut));
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanCoil(FanCoilNum).ElecPower = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanCoil(FanCoilNum).ElecPower = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                FanCoil(FanCoilNum).PLR = PLR;
                PowerMet = QUnitOut;
                LatOutputProvided = LatentOutput;

            } else if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::ASHRAE) {

                if (AirMassFlow < SmallMassFlow) UnitOn = false;

                //  zero the hot & cold water flows
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                     FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopSide,
                                     FanCoil(FanCoilNum).CoolCoilBranchNum,
                                     FanCoil(FanCoilNum).CoolCoilCompNum);

                if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                         FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopSide,
                                         FanCoil(FanCoilNum).HeatCoilBranchNum,
                                         FanCoil(FanCoilNum).HeatCoilCompNum);
                }

                OAMassFlow = 0.0;

                // determine minimum outdoor air flow rate
                if (FanCoil(FanCoilNum).DSOAPtr > 0 && FanCoil(FanCoilNum).OutsideAirNode > 0) {
                    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(state, FanCoil(FanCoilNum).DSOAPtr, ZoneNum, true, true);
                    RhoAir = PsyRhoAirFnPbTdbW(state,
                                               Node(FanCoil(FanCoilNum).OutsideAirNode).Press,
                                               Node(FanCoil(FanCoilNum).OutsideAirNode).Temp,
                                               Node(FanCoil(FanCoilNum).OutsideAirNode).HumRat);
                    OAMassFlow = OAVolumeFlowRate * RhoAir;
                }

                MinSAMassFlowRate =
                    min(max(OAMassFlow, FanCoil(FanCoilNum).MaxAirMassFlow * FanCoil(FanCoilNum).LowSpeedRatio), FanCoil(FanCoilNum).MaxAirMassFlow);
                MaxSAMassFlowRate = FanCoil(FanCoilNum).MaxAirMassFlow;
                state.dataFanCoilUnits->HeatingLoad = false;
                state.dataFanCoilUnits->CoolingLoad = false;
                if (UnitOn) {
                    Node(InletNode).MassFlowRate = MinSAMassFlowRate;
                    FanCoil(FanCoilNum).MaxNoCoolHeatAirMassFlow = MinSAMassFlowRate;
                    FanCoil(FanCoilNum).MaxCoolAirMassFlow = MaxSAMassFlowRate;
                    FanCoil(FanCoilNum).MaxHeatAirMassFlow = MaxSAMassFlowRate;
                    FanCoil(FanCoilNum).LowSpeedCoolFanRatio = MinSAMassFlowRate / MaxSAMassFlowRate;
                    FanCoil(FanCoilNum).LowSpeedHeatFanRatio = MinSAMassFlowRate / MaxSAMassFlowRate;

                    Calc4PipeFanCoil(state,
                                     FanCoilNum,
                                     ControlledZoneNum,
                                     FirstHVACIteration,
                                     QUnitOutNoHC,
                                     0.0); // needs PLR=0 for electric heating coil, otherwise will run at full capacity

                    QCoilCoolSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
                    QCoilHeatSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;

                    if (QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint) {
                        QZnReq = QCoilHeatSP;
                        state.dataFanCoilUnits->HeatingLoad = true;
                    } else if (QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleCoolingSetPoint) {
                        QZnReq = 0.0;
                    } else if (QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
                        QZnReq = QCoilCoolSP;
                        state.dataFanCoilUnits->CoolingLoad = true;
                    } else if (QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleHeatingSetPoint) {
                        QZnReq = 0.0;
                    } else if (QCoilHeatSP <= 0.0 && QCoilCoolSP >= 0.0) {
                        QZnReq = 0.0;
                    }
                }

                if (state.dataFanCoilUnits->CoolingLoad) {

                    Node(InletNode).MassFlowRate = MaxSAMassFlowRate;

                    mdot = FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                         FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopSide,
                                         FanCoil(FanCoilNum).CoolCoilBranchNum,
                                         FanCoil(FanCoilNum).CoolCoilCompNum);

                } else if (state.dataFanCoilUnits->HeatingLoad) {

                    Node(InletNode).MassFlowRate = MaxSAMassFlowRate;

                    if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                        mdot = FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                             FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopSide,
                                             FanCoil(FanCoilNum).HeatCoilBranchNum,
                                             FanCoil(FanCoilNum).HeatCoilCompNum);
                    }
                }

                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax);

                if ((state.dataFanCoilUnits->CoolingLoad && QUnitOutMax < QZnReq) || (state.dataFanCoilUnits->HeatingLoad && QUnitOutMax > QZnReq)) {
                    if ((state.dataFanCoilUnits->CoolingLoad && QUnitOutNoHC < QZnReq) ||
                        (state.dataFanCoilUnits->HeatingLoad && QUnitOutNoHC > QZnReq)) {
                        PLR = 0.0;
                        FanCoil(FanCoilNum).FanPartLoadRatio = 0.0;       // set SZVAV model variable
                        Node(InletNode).MassFlowRate = MinSAMassFlowRate; // = min air flow rate + ((max-min) air flow rate * FanPartLoadRatio)
                        mdot = 0.0;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                             FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopNum,
                                             FanCoil(FanCoilNum).CoolCoilLoopSide,
                                             FanCoil(FanCoilNum).CoolCoilBranchNum,
                                             FanCoil(FanCoilNum).CoolCoilCompNum);

                        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                            mdot = 0.0;
                            SetComponentFlowRate(state,
                                                 mdot,
                                                 FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                                 FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopNum,
                                                 FanCoil(FanCoilNum).HeatCoilLoopSide,
                                                 FanCoil(FanCoilNum).HeatCoilBranchNum,
                                                 FanCoil(FanCoilNum).HeatCoilCompNum);
                        }
                    } else {
                        Real64 OnOffAirFlowRatio = 1.0;
                        bool HXUnitOn = false;
                        int AirLoopNum = 0;
                        int CompressorOnFlag = 0;
                        auto &SZVAVModel(FanCoil(FanCoilNum));
                        // seems like passing these (arguments 2-n) as an array (similar to Par) would make this more uniform across different
                        // models
                        SZVAVModel::calcSZVAVModel(state,
                                                   SZVAVModel,
                                                   FanCoilNum,
                                                   FirstHVACIteration,
                                                   state.dataFanCoilUnits->CoolingLoad,
                                                   state.dataFanCoilUnits->HeatingLoad,
                                                   QZnReq,
                                                   OnOffAirFlowRatio,
                                                   HXUnitOn,
                                                   AirLoopNum,
                                                   PLR,
                                                   CompressorOnFlag);
                    }
                } else if ((state.dataFanCoilUnits->CoolingLoad && QUnitOutMax > QZnReq && QZnReq < 0.0) ||
                           (state.dataFanCoilUnits->HeatingLoad && QUnitOutMax < QZnReq && QZnReq > 0.0)) {
                    // load is larger than capacity, thus run the fancoil unit at full capacity
                    PLR = 1.0;
                }
                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                PowerMet = QUnitOut;
                AirMassFlow = Node(InletNode).MassFlowRate;
                // CR9155 Remove specific humidity calculations
                SpecHumOut = Node(OutletNode).HumRat;
                SpecHumIn = Node(InletNode).HumRat;
                // Latent rate (kg/s), dehumid = negative
                LatOutputProvided = AirMassFlow * (SpecHumOut - SpecHumIn);
                FanCoil(FanCoilNum).PLR = PLR;

                // cycling fan constant water flow AND VarFanVarFlow
            } else if (SELECT_CASE_var == CCM::VarFanConsFlow) {

                if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || AirMassFlow < SmallMassFlow) UnitOn = false;

                //  zero the hot & cold water flows
                //    Node(FanCoil(FanCoilNum)%CoolCoilFluidInletNode)%MassFlowRate = 0.0
                //    Node(FanCoil(FanCoilNum)%HeatCoilFluidInletNode)%MassFlowRate = 0.0
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                     FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopNum,
                                     FanCoil(FanCoilNum).CoolCoilLoopSide,
                                     FanCoil(FanCoilNum).CoolCoilBranchNum,
                                     FanCoil(FanCoilNum).CoolCoilCompNum);

                if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                         FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopNum,
                                         FanCoil(FanCoilNum).HeatCoilLoopSide,
                                         FanCoil(FanCoilNum).HeatCoilBranchNum,
                                         FanCoil(FanCoilNum).HeatCoilCompNum);
                }
                Calc4PipeFanCoil(state,
                                 FanCoilNum,
                                 ControlledZoneNum,
                                 FirstHVACIteration,
                                 QUnitOutNoHC,
                                 0.0); // needs PLR=0 for electric heating coil, otherwise will run at full capacity

                if (UnitOn && state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP < (-1.0 * SmallLoad) &&
                    state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
                    // cooling coil action, maximum cold water flow
                    mdot = FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                         FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopSide,
                                         FanCoil(FanCoilNum).CoolCoilBranchNum,
                                         FanCoil(FanCoilNum).CoolCoilCompNum);
                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
                    ControlOffset = FanCoil(FanCoilNum).ColdControlOffset;

                    // get the maximum output of the fcu
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax);
                    // calculate the PLR, if load greater than output, PLR = 1 (output = max)
                    if (QUnitOutMax != 0.0) PLR = std::abs(QZnReq / QUnitOutMax);
                    if (PLR > 1.0) PLR = 1.0;

                    // adjust the PLR to meet the cooling load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
                    while (std::abs(Error) > ControlOffset && std::abs(AbsError) > SmallLoad && Iter < MaxIterCycl && PLR != 1.0) {
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                        Error = (QZnReq - QUnitOut) / QZnReq;
                        AbsError = QZnReq - QUnitOut;
                        DelPLR = (QZnReq - QUnitOut) / QUnitOutMax;
                        PLR += Relax * DelPLR;
                        PLR = max(0.0, min(1.0, PLR));
                        ++Iter;
                        if (Iter == 32) Relax = 0.5;
                        if (Iter == 65) Relax = 0.25;
                    }

                    // warning if not converged
                    if (Iter > (MaxIterCycl - 1)) {
                        if (FanCoil(FanCoilNum).MaxIterIndexC == 0) {
                            ShowWarningMessage(state,
                                               "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                                   "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load "
                                                   "within the cooling convergence tolerance.");
                            ShowContinueErrorTimeStamp(state, format("Iterations={}", MaxIterCycl));
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                                           "\"  -- Exceeded max iterations error (sensible runtime) continues...",
                                                       FanCoil(FanCoilNum).MaxIterIndexC);
                    }

                    // at the end calculate output with adjusted PLR
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);

                } else if (UnitOn && state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP > SmallLoad &&
                           state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint) {
                    // heating coil action, maximun hot water flow
                    if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                        mdot = FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                             FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopSide,
                                             FanCoil(FanCoilNum).HeatCoilBranchNum,
                                             FanCoil(FanCoilNum).HeatCoilCompNum);
                    }
                    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
                    ControlOffset = FanCoil(FanCoilNum).HotControlOffset;

                    // get the maximum output of the fcu
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax);
                    // calculate the PLR, if load greater than output, PLR = 1 (output = max)
                    if (QUnitOutMax != 0.0) PLR = std::abs(QZnReq / QUnitOutMax);
                    if (PLR > 1.0) PLR = 1.0;

                    // adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
                    while (std::abs(Error) > ControlOffset && std::abs(AbsError) > SmallLoad && Iter < MaxIterCycl && PLR != 1.0) {
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                        Error = (QZnReq - QUnitOut) / QZnReq;
                        AbsError = QZnReq - QUnitOut;
                        DelPLR = (QZnReq - QUnitOut) / QUnitOutMax;
                        PLR += Relax * DelPLR;
                        PLR = max(0.0, min(1.0, PLR));
                        ++Iter;
                        if (Iter == 32) Relax = 0.5;
                        if (Iter == 65) Relax = 0.25;
                    }

                    // warning if not converged
                    if (Iter > (MaxIterCycl - 1)) {
                        if (FanCoil(FanCoilNum).MaxIterIndexH == 0) {
                            ShowWarningMessage(state,
                                               "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                                   "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load "
                                                   "within the heating convergence tolerance.");
                            ShowContinueError(state, format("...Requested zone load = {:.3T} [W]", QZnReq));
                            ShowContinueError(state, format("...Fan coil capacity   = {:.3T} [W]", QUnitOut));
                            ShowContinueErrorTimeStamp(state, format("Iterations={}", MaxIterCycl));
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                                           "\"  -- Exceeded max iterations error (sensible runtime) continues...",
                                                       FanCoil(FanCoilNum).MaxIterIndexH);
                    }

                    // at the end calculate output with adjusted PLR
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);

                    // this part of the code is just if we want ventilation in the deadband zone
                    // ELSE IF (AirMassFlow .gt. 0.0d0) THEN
                    // if fan scheduled available : just ventilation, PLR = 1
                    // QUnitOut = QUnitOutNOHC
                    // PLR = 1.

                } else {
                    // no action, zero the air flow rate, the unit is off
                    Node(InletNode).MassFlowRate = 0.0;
                    Node(OutletNode).MassFlowRate = 0.0;
                    FanCoil(FanCoilNum).SpeedFanSel = 0;
                    PLR = 0.0;
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                }

                AirMassFlow = Node(InletNode).MassFlowRate;
                // CR9155 Remove specific humidity calculations
                SpecHumOut = Node(OutletNode).HumRat;
                SpecHumIn = Node(InletNode).HumRat;
                LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
                CalcZoneSensibleOutput(AirMassFlow, Node(OutletNode).Temp, Node(InletNode).Temp, Node(InletNode).HumRat, QSensUnitOutNoATM);
                QTotUnitOut = AirMassFlow * (Node(OutletNode).Enthalpy - Node(InletNode).Enthalpy);
                // report variables
                FanCoil(FanCoilNum).HeatPower = max(0.0, QSensUnitOutNoATM);
                FanCoil(FanCoilNum).SensCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QSensUnitOutNoATM));
                FanCoil(FanCoilNum).TotCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QTotUnitOut));
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanCoil(FanCoilNum).ElecPower = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanCoil(FanCoilNum).ElecPower = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                FanCoil(FanCoilNum).PLR = PLR;
                PowerMet = QUnitOut;
                LatOutputProvided = LatentOutput;

            } else if (SELECT_CASE_var == CCM::MultiSpeedFan) {
                // call multi-speed fan staging calculation
                SimMultiStage4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                AirMassFlow = Node(InletNode).MassFlowRate;
                SpecHumOut = Node(OutletNode).HumRat;
                SpecHumIn = Node(InletNode).HumRat;
                LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
                CalcZoneSensibleOutput(AirMassFlow, Node(OutletNode).Temp, Node(InletNode).Temp, Node(InletNode).HumRat, QSensUnitOutNoATM);
                QTotUnitOut = AirMassFlow * (Node(OutletNode).Enthalpy - Node(InletNode).Enthalpy);
                // report variables
                FanCoil(FanCoilNum).HeatPower = max(0.0, QSensUnitOutNoATM);
                FanCoil(FanCoilNum).SensCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QSensUnitOutNoATM));
                FanCoil(FanCoilNum).TotCoolPower = std::abs(min(DataPrecisionGlobals::constant_zero, QTotUnitOut));
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanCoil(FanCoilNum).ElecPower = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanCoil(FanCoilNum).ElecPower = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                PowerMet = QUnitOut;
                LatOutputProvided = LatentOutput;
            }
        }
    }

    void TightenWaterFlowLimits(EnergyPlusData &state,
                                int const FanCoilNum,          // Unit index in fan coil array
                                bool const CoolingLoad,        // true if zone requires cooling
                                bool const HeatingLoad,        // true if zone requires heating
                                int const WaterControlNode,    // water control node, either cold or hot water
                                int const ControlledZoneNum,   // controlling zone index
                                bool const FirstHVACIteration, //  TRUE if 1st HVAC simulation of system timestep
                                Real64 const QZnReq,           // zone load [W]
                                Real64 &MinWaterFlow,          // minimum water flow rate
                                Real64 &MaxWaterFlow           // maximum water flow rate
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad, FSEC
        //       DATE WRITTEN   May 2016

        // PURPOSE OF THIS SUBROUTINE:
        // Find tighter limits of water flow rate for fan coil unit.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 QUnitOut; // fan coil delivered capacity [W]
        Real64 mdot;     // water flow rate passed to fan coil unit [kg/s]

        // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 10% of flow before iterating
        mdot = MaxWaterFlow * 0.1;
        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = mdot;
        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
        if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
            MaxWaterFlow = mdot;
            // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 1% of flow before iterating
            mdot *= 0.1;
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = mdot;
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
            if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                MaxWaterFlow = mdot;
                // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 0.1% of flow before iterating
                mdot *= 0.1;
                state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = mdot;
                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                    MaxWaterFlow = mdot;
                    // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 0.01% of flow before iterating
                    mdot *= 0.1;
                    state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = mdot;
                    Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                    if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                        MaxWaterFlow = mdot;
                        // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 0.001% of flow before
                        // iterating
                        mdot *= 0.1;
                        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = mdot;
                        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
                        if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                            MaxWaterFlow = mdot;
                        } else {
                            MinWaterFlow = mdot;
                        }
                    } else {
                        MinWaterFlow = mdot;
                    }
                } else {
                    MinWaterFlow = mdot;
                }
            } else {
                MinWaterFlow = mdot;
            }
        } else {
            MinWaterFlow = mdot;
        }
    }

    void TightenAirAndWaterFlowLimits(EnergyPlusData &state,
                                      int const FanCoilNum,          // Unit index in fan coil array
                                      bool const CoolingLoad,        // true if zone requires cooling
                                      bool const HeatingLoad,        // true if zone requires heating
                                      int const WaterControlNode,    // water control node, either cold or hot water
                                      int const ControlledZoneNum,   // controlling zone index
                                      bool const FirstHVACIteration, //  TRUE if 1st HVAC simulation of system timestep
                                      Real64 const QZnReq,           // zone load [W]
                                      Real64 &PLRMin,                // minimum part-load ratio
                                      Real64 &PLRMax                 // maximum part-load ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad, FSEC
        //       DATE WRITTEN   August 2016

        // PURPOSE OF THIS SUBROUTINE:
        // Find tighter limits of air and water flow rate for fan coil unit.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PLR;      // operating part-load ratio
        Real64 QUnitOut; // fan coil delivered capacity [W]

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);

        // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 100% of flow before iterating
        PLRMin = 0.0;
        PLRMax = 1.0;
        PLR = 1.0;
        if (WaterControlNode == FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
        } else if (WaterControlNode == FanCoil(FanCoilNum).HeatCoilFluidInletNode && FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
        }
        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
        if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
            PLRMax = PLR;
            PLR *= 0.1;
            // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 10% of flow before iterating
            if (WaterControlNode == FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
                state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
            } else if (WaterControlNode == FanCoil(FanCoilNum).HeatCoilFluidInletNode && FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
                state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
            }
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
            if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                PLRMax = PLR;
                PLR *= 0.1;
                // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 1% of flow before iterating
                if (WaterControlNode == FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
                    state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                } else if (WaterControlNode == FanCoil(FanCoilNum).HeatCoilFluidInletNode && FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
                    state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                }
                Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
                if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                    PLRMax = PLR;
                    PLR *= 0.1;
                    // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 0.1% of flow before iterating
                    if (WaterControlNode == FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
                        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    } else if (WaterControlNode == FanCoil(FanCoilNum).HeatCoilFluidInletNode &&
                               FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
                        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                    }
                    if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                        PLRMax = PLR;
                        PLR *= 0.1;
                        // RegulaFalsi can reach max iteration when low water flow rate is required to meet load. Test at 0.01% of flow before
                        // iterating
                        if (WaterControlNode == FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
                            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                        } else if (WaterControlNode == FanCoil(FanCoilNum).HeatCoilFluidInletNode &&
                                   FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
                            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                        }
                        if ((CoolingLoad && QUnitOut < QZnReq) || (HeatingLoad && QUnitOut > QZnReq)) {
                            PLRMax = PLR;
                        } else {
                            PLRMin = PLR;
                        }
                    } else {
                        PLRMin = PLR;
                    }
                } else {
                    PLRMin = PLR;
                }
            } else {
                PLRMin = PLR;
            }
        } else {
            PLRMin = PLR;
        }
    }

    void Calc4PipeFanCoil(EnergyPlusData &state,
                          int const FanCoilNum,          // Unit index in fan coil array
                          int const ControlledZoneNum,   // ZoneEquipConfig index
                          bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
                          Real64 &LoadMet,               // load met by unit (watts)
                          Optional<Real64> PLR,          // Part Load Ratio, fraction of time step fancoil is on
                          Real64 eHeatCoilCyclingR       // electric heating coil cycling ratio  used with MultiSpeedFan capacity control
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2000
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate the components making up the 4 pipe fan coil unit.

        // METHODOLOGY EMPLOYED:
        // Simulates the unit components sequentially in the air flow direction.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using MixedAir::SimOAMixer;
        using Psychrometrics::PsyHFnTdbW;
        using SingleDuct::SimATMixer;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;                // unit air outlet node
        int InletNode;                 // unit air inlet node
        Real64 AirMassFlow;            // total mass flow through the unit
        Real64 PartLoad;               // if PLR present PartLoad = PLR
        Real64 OASchedValue;           // value of OASchedValue, =1 if not schedule
        Real64 ElecHeaterControl(1.0); // 1 or 0, enables or disables heating coil
        Real64 FanSpeedRatio;          // ratio of actual fan flow to max design fan flow

        auto &Node(state.dataLoopNodes->Node);
        auto &FanCoil(state.dataFanCoilUnits->FanCoil);

        // if PLR present in arguments, get its value, else default PLR = 1
        if (present(PLR)) {
            PartLoad = PLR;
        } else {
            PartLoad = 1.0;
        }

        OutletNode = FanCoil(FanCoilNum).AirOutNode;
        InletNode = FanCoil(FanCoilNum).AirInNode;
        state.dataFanCoilUnits->ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;

        // Assume the unit is able to vary the flow. A cycling unit is treated as
        // if it were variable flow, with the flow being the averaqe flow over the time step
        if (((GetCurrentScheduleValue(state, FanCoil(FanCoilNum).SchedPtr) > 0.0 &&
              GetCurrentScheduleValue(state, FanCoil(FanCoilNum).fanAvailSchIndex) > 0.0) ||
             state.dataHVACGlobal->ZoneCompTurnFansOn) &&
            !state.dataHVACGlobal->ZoneCompTurnFansOff) {
            if (FanCoil(FanCoilNum).CapCtrlMeth_Num != CCM::ConsFanVarFlow) {
                if (FanCoil(FanCoilNum).CapCtrlMeth_Num != CCM::ASHRAE) Node(InletNode).MassFlowRate = PartLoad * Node(InletNode).MassFlowRateMax;
            } else {
                Node(InletNode).MassFlowRate = Node(InletNode).MassFlowRateMax;
            }
        }

        // use the value of the outside air schedule if present
        if (FanCoil(FanCoilNum).SchedOutAirPtr > 0) {
            OASchedValue = GetCurrentScheduleValue(state, FanCoil(FanCoilNum).SchedOutAirPtr);
        } else {
            OASchedValue = 1.0;
        }

        if (FanCoil(FanCoilNum).ATMixerExists) {
            state.dataFanCoilUnits->ATMixOutNode = FanCoil(FanCoilNum).ATMixerOutNode;
            if (FanCoil(FanCoilNum).ATMixerType == ATMixer_InletSide) {
                // set the primary air inlet mass flow rate
                Node(FanCoil(FanCoilNum).ATMixerPriNode).MassFlowRate =
                    min(Node(FanCoil(FanCoilNum).ATMixerPriNode).MassFlowRateMaxAvail, Node(InletNode).MassFlowRate);
                // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
                // the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
                SimATMixer(state, FanCoil(FanCoilNum).ATMixerName, FirstHVACIteration, FanCoil(FanCoilNum).ATMixerIndex);
            }
            AirMassFlow = Node(InletNode).MassFlowRate;
        } else {
            // OutdoorAir:Mixer
            if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::CycFan) {
                Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRate =
                    min(OASchedValue * Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRateMax * PartLoad * FanCoil(FanCoilNum).SpeedFanRatSel,
                        Node(InletNode).MassFlowRate);
            } else if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::MultiSpeedFan) {
                Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRate =
                    min(OASchedValue * Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRateMax * PartLoad * state.dataFanCoilUnits->FanFlowRatio,
                        Node(InletNode).MassFlowRate);
            } else {
                if (FanCoil(FanCoilNum).CapCtrlMeth_Num != CCM::ConsFanVarFlow && FanCoil(FanCoilNum).CapCtrlMeth_Num != CCM::ASHRAE) {
                    Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRate =
                        min(OASchedValue * Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRateMax * PartLoad, Node(InletNode).MassFlowRate);
                } else {
                    Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRate =
                        min(OASchedValue * Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRateMax, Node(InletNode).MassFlowRate);
                }
            }
            Node(FanCoil(FanCoilNum).AirReliefNode).MassFlowRate = Node(FanCoil(FanCoilNum).OutsideAirNode).MassFlowRate;
            AirMassFlow = Node(InletNode).MassFlowRate;
            SimOAMixer(state, FanCoil(FanCoilNum).OAMixName, FirstHVACIteration, FanCoil(FanCoilNum).OAMixIndex);
        }

        if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::CycFan) {
            // cycling fan coil unit calculation
            if (FanCoil(FanCoilNum).SpeedFanSel == 1) {
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                FanCoil(FanCoilNum).FanName,
                                                FirstHVACIteration,
                                                FanCoil(FanCoilNum).FanIndex,
                                                FanCoil(FanCoilNum).LowSpeedRatio,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
            } else if (FanCoil(FanCoilNum).SpeedFanSel == 2) {

                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                FanCoil(FanCoilNum).FanName,
                                                FirstHVACIteration,
                                                FanCoil(FanCoilNum).FanIndex,
                                                FanCoil(FanCoilNum).MedSpeedRatio,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
            } else if (FanCoil(FanCoilNum).SpeedFanSel == 3) {

                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                FanCoil(FanCoilNum).FanName,
                                                FirstHVACIteration,
                                                FanCoil(FanCoilNum).FanIndex,
                                                1.0,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
            } else { // using 1.0 here for fan speed ratio seems wrong if FCU max flow rate is different than the fan maximum flow rate
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                FanCoil(FanCoilNum).FanName,
                                                FirstHVACIteration,
                                                FanCoil(FanCoilNum).FanIndex,
                                                0.0,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, 0.0, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
            }
            if (FanCoil(FanCoilNum).CCoilType_Num == CCoil::HXAssist) {
                SimHXAssistedCoolingCoil(
                    state, FanCoil(FanCoilNum).CCoilName, FirstHVACIteration, On, 0.0, FanCoil(FanCoilNum).CCoilName_Index, ContFanCycCoil);
            } else {
                SimulateWaterCoilComponents(state, FanCoil(FanCoilNum).CCoilName, FirstHVACIteration, FanCoil(FanCoilNum).CCoilName_Index, _, 1, PLR);
            }
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                SimulateWaterCoilComponents(state, FanCoil(FanCoilNum).HCoilName, FirstHVACIteration, FanCoil(FanCoilNum).HCoilName_Index, _, 1, PLR);
            } else {
                if (Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate > 0.0) ElecHeaterControl = 0.0;
                SimulateHeatingCoilComponents(state,
                                              FanCoil(FanCoilNum).HCoilName,
                                              FirstHVACIteration,
                                              FanCoil(FanCoilNum).DesignHeatingCapacity * PartLoad * ElecHeaterControl,
                                              FanCoil(FanCoilNum).HCoilName_Index,
                                              _,
                                              false,
                                              ContFanCycCoil,
                                              PartLoad);
            }

        } else if (FanCoil(FanCoilNum).CapCtrlMeth_Num == CCM::MultiSpeedFan) {
            if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state,
                                            FanCoil(FanCoilNum).FanName,
                                            FirstHVACIteration,
                                            FanCoil(FanCoilNum).FanIndex,
                                            state.dataFanCoilUnits->FanFlowRatio,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            } else {
                // FanFlowRatio needs to be accurate here for new fan model
                Real64 ActFanFlowRatio = state.dataFanCoilUnits->FanFlowRatio * PartLoad;
                state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(
                    state, ActFanFlowRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            }
            if (FanCoil(FanCoilNum).CCoilType_Num == CCoil::HXAssist) {
                SimHXAssistedCoolingCoil(
                    state, FanCoil(FanCoilNum).CCoilName, FirstHVACIteration, On, 0.0, FanCoil(FanCoilNum).CCoilName_Index, ContFanCycCoil);
            } else {
                SimulateWaterCoilComponents(state, FanCoil(FanCoilNum).CCoilName, FirstHVACIteration, FanCoil(FanCoilNum).CCoilName_Index, _, 1, PLR);
            }
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                SimulateWaterCoilComponents(state, FanCoil(FanCoilNum).HCoilName, FirstHVACIteration, FanCoil(FanCoilNum).HCoilName_Index, _, 1, PLR);
            } else {
                if (Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate > 0.0) ElecHeaterControl = 0.0;
                Real64 QZnReq = 0.0;
                if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                    QZnReq = FanCoil(FanCoilNum).DesignHeatingCapacity * state.dataFanCoilUnits->FanFlowRatio * eHeatCoilCyclingR * ElecHeaterControl;
                } else {
                    // proportionally reduce the full flow capacity based on fan flow fraction
                    QZnReq = FanCoil(FanCoilNum).DesignHeatingCapacity * state.dataFanCoilUnits->FanFlowRatio * PartLoad * eHeatCoilCyclingR *
                             ElecHeaterControl;
                }
                SimulateHeatingCoilComponents(state,
                                              FanCoil(FanCoilNum).HCoilName,
                                              FirstHVACIteration,
                                              QZnReq,
                                              FanCoil(FanCoilNum).HCoilName_Index,
                                              _,
                                              false,
                                              FanCoil(FanCoilNum).FanOpMode, // FanCoil(FanCoilNum).FanOpMode, // ContFanCycCoil, CycFanCycCoil
                                              PartLoad);
            }
        } else { // capacity control method is VariableFanVariableFlow, VariableFanConstantFlow, or ASHRAE90.1

            // calculate fan speed ratio for Fan:OnOff or Fan:SystemModel (not used for other fan types). Only used in fan:OnOff model if performance
            // curves are present.
            FanSpeedRatio = Node(InletNode).MassFlowRate / (FanCoil(FanCoilNum).FanAirVolFlow * state.dataEnvrn->StdRhoAir);

            // Constant fan and variable flow calculation AND variable fan

            if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state,
                                            FanCoil(FanCoilNum).FanName,
                                            FirstHVACIteration,
                                            FanCoil(FanCoilNum).FanIndex,
                                            FanSpeedRatio,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            } else {
                state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->simulate(state, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            }

            if (FanCoil(FanCoilNum).CCoilType_Num == CCoil::HXAssist) {
                SimHXAssistedCoolingCoil(
                    state, FanCoil(FanCoilNum).CCoilName, FirstHVACIteration, On, 0.0, FanCoil(FanCoilNum).CCoilName_Index, ContFanCycCoil);
            } else {
                SimulateWaterCoilComponents(state, FanCoil(FanCoilNum).CCoilName, FirstHVACIteration, FanCoil(FanCoilNum).CCoilName_Index);
            }
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                SimulateWaterCoilComponents(state, FanCoil(FanCoilNum).HCoilName, FirstHVACIteration, FanCoil(FanCoilNum).HCoilName_Index);
            } else {
                if (Node(FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate > 0.0) ElecHeaterControl = 0.0;
                SimulateHeatingCoilComponents(state,
                                              FanCoil(FanCoilNum).HCoilName,
                                              FirstHVACIteration,
                                              FanCoil(FanCoilNum).DesignHeatingCapacity * PartLoad * ElecHeaterControl,
                                              FanCoil(FanCoilNum).HCoilName_Index,
                                              _,
                                              false,
                                              ContFanCycCoil,
                                              PartLoad);
            }
        }

        if (FanCoil(FanCoilNum).ATMixerExists) {
            if (FanCoil(FanCoilNum).ATMixerType == ATMixer_SupplySide) {
                // Now calculate the ATM mixer if it is on the supply side of the zone unit
                SimATMixer(state, FanCoil(FanCoilNum).ATMixerName, FirstHVACIteration, FanCoil(FanCoilNum).ATMixerIndex);
                CalcZoneSensibleOutput(Node(state.dataFanCoilUnits->ATMixOutNode).MassFlowRate,
                                       Node(state.dataFanCoilUnits->ATMixOutNode).Temp,
                                       Node(state.dataFanCoilUnits->ZoneNode).Temp,
                                       Node(state.dataFanCoilUnits->ZoneNode).HumRat,
                                       LoadMet);
            } else {
                // ATM Mixer on inlet side
                CalcZoneSensibleOutput(AirMassFlow,
                                       Node(OutletNode).Temp,
                                       Node(state.dataFanCoilUnits->ZoneNode).Temp,
                                       Node(state.dataFanCoilUnits->ZoneNode).HumRat,
                                       LoadMet);
            }
        } else {
            CalcZoneSensibleOutput(AirMassFlow, Node(OutletNode).Temp, Node(InletNode).Temp, Node(InletNode).HumRat, LoadMet);
        }
    }

    void SimMultiStage4PipeFanCoil(EnergyPlusData &state,
                                   int &FanCoilNum,               // number of the current fan coil unit being simulated
                                   int const ZoneNum,             // number of zone being served
                                   bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                   Real64 &PowerMet               // Sensible power supplied (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages multi-speed fancoil unit simulation;

        // METHODOLOGY EMPLOYED:
        // Selects the appropriate fan speed for a given zone heating or cooling load
        // and determines whether heating or cooling is required, then runs the hot
        // or chilled water coils.

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using PlantUtilities::SetComponentFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SimMultiStage4PipeFanCoil");
        // int const MaxIterCycl( 100 );

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 mdot;          // chilled or hot water flow rate through the water coils
        Real64 QZnReq;        // heating or cooling needed by zone [watts]
        Real64 QUnitOut;      // heating or sens. cooling provided by fan coil unit [watts]
        Real64 QUnitOutMax;   // heating or sens. cooling provided by fan coil unit (running during an entire timestep)
        Real64 QTotUnitOut;   // total unit output [watts]
        Real64 AirMassFlow;   // air mass flow rate [kg/sec]
        Real64 QUnitOutNoHC;  // unit output with no active heating or cooling [W]
        Real64 QCoilHeatSP;   // coil load to the heating setpoint [W]
        Real64 QCoilCoolSP;   // coil load to the cooling setpoint [W]
        Real64 SpeedRatio;    // ratio between lower and higher fan speed
        Real64 PartLoadRatio; // Part Load Ratio, fraction of time step fancoil is on
        int OutletNode;       // unit air outlet node
        int InletNode;        // unit air inlet node
        bool UnitOn;          // TRUE if unit is on

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);
        auto &HeatingLoad(state.dataFanCoilUnits->HeatingLoad);
        auto &CoolingLoad(state.dataFanCoilUnits->CoolingLoad);

        // initialize local variables
        UnitOn = true;
        SpeedRatio = 0.0;
        PartLoadRatio = 0.0;
        QZnReq = 0.0;
        QUnitOut = 0.0;
        QTotUnitOut = 0.0;
        QUnitOutMax = 0.0;
        QUnitOutNoHC = 0.0;

        OutletNode = FanCoil(FanCoilNum).AirOutNode;
        InletNode = FanCoil(FanCoilNum).AirInNode;
        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;

        if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || AirMassFlow < SmallMassFlow) UnitOn = false;

        FanCoil(FanCoilNum).SpeedFanSel = 1;
        FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).LowSpeedRatio;
        state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
        AirMassFlow = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = AirMassFlow;

        if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
            mdot = 0.0;
            SetComponentFlowRate(state,
                                 mdot,
                                 FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                 FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                 FanCoil(FanCoilNum).HeatCoilLoopNum,
                                 FanCoil(FanCoilNum).HeatCoilLoopSide,
                                 FanCoil(FanCoilNum).HeatCoilBranchNum,
                                 FanCoil(FanCoilNum).HeatCoilCompNum);
        }
        mdot = 0.0;
        SetComponentFlowRate(state,
                             mdot,
                             FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                             FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                             FanCoil(FanCoilNum).CoolCoilLoopNum,
                             FanCoil(FanCoilNum).CoolCoilLoopSide,
                             FanCoil(FanCoilNum).CoolCoilBranchNum,
                             FanCoil(FanCoilNum).CoolCoilCompNum);
        // no load output, requires setting eHeatCoilCyclingR = 0.0, for electric heating coils
        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutNoHC, _, 0.0);

        QCoilCoolSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
        QCoilHeatSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        state.dataFanCoilUnits->HeatingLoad = false;
        state.dataFanCoilUnits->CoolingLoad = false;

        if (QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint) {
            QZnReq = QCoilHeatSP;
            HeatingLoad = true;
        } else if (QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleCoolingSetPoint) {
            QZnReq = 0.0;
        } else if (QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
            QZnReq = QCoilCoolSP;
            CoolingLoad = true;
        } else if (QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleHeatingSetPoint) {
            QZnReq = 0.0;
        } else if (QCoilHeatSP <= 0.0 && QCoilCoolSP >= 0.0) {
            QZnReq = 0.0;
        }

        // Zone load calculation for constant fan systems, adopted from unitary system
        if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
            {
                auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum));
                if (SELECT_CASE_var == SingleHeatingSetPoint) {
                    CoolingLoad = false;
                    // No heating load and constant fan pushes zone below heating set point
                    if (QUnitOutNoHC < 0.0 && QCoilHeatSP < 0.0 && QUnitOutNoHC - QCoilHeatSP < -SmallLoad) {
                        HeatingLoad = true;
                        CoolingLoad = false;
                        QZnReq = QCoilHeatSP;
                    }
                } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                    HeatingLoad = false;
                    // No heating load and constant fan pushes zone above cooling set point
                    if (QUnitOutNoHC > 0.0 && QCoilCoolSP > 0.0 && QUnitOutNoHC - QCoilCoolSP > SmallLoad) {
                        HeatingLoad = false;
                        CoolingLoad = true;
                        QZnReq = QCoilCoolSP;
                    }
                } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                    // zone temp above cooling and heating set point temps
                    if (QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0) {
                        // zone pushed below heating set point
                        if (QUnitOutNoHC < 0.0 && QCoilHeatSP - QUnitOutNoHC > SmallLoad) {
                            HeatingLoad = true;
                            CoolingLoad = false;
                            QZnReq = QCoilHeatSP;
                        }
                        // zone temp below heating set point temp
                    } else if (QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0) {
                        // zone pushed above cooling set point
                        if (QUnitOutNoHC > 0.0 && QCoilCoolSP - QUnitOutNoHC > SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = true;
                            QZnReq = QCoilCoolSP;
                        }
                    }
                } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                    // zone temp above cooling and heating set point temps
                    if (QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0) {
                        // zone pushed into deadband
                        if (QUnitOutNoHC < 0.0 && QCoilCoolSP - QUnitOutNoHC > SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = false;
                            QZnReq = 0.0;
                        }
                        // zone pushed below heating set point
                        if (QUnitOutNoHC < 0.0 && QCoilHeatSP - QUnitOutNoHC > SmallLoad) {
                            HeatingLoad = true;
                            CoolingLoad = false;
                            QZnReq = QCoilHeatSP;
                        }
                        // zone temp below heating set point temp
                    } else if (QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0) {
                        // zone pushed into deadband
                        if (QUnitOutNoHC > 0.0 && QUnitOutNoHC - QCoilHeatSP > SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = false;
                            QZnReq = 0.0;
                        }
                        // zone pushed above cooling set point
                        if (QUnitOutNoHC > 0.0 && QUnitOutNoHC - QCoilCoolSP > SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = true;
                            QZnReq = QCoilCoolSP;
                        }
                        // zone temp between set point temps
                    } else if (QCoilHeatSP < 0.0 && QCoilCoolSP > 0.0) {
                        // zone pushed below heating set point
                        if (QUnitOutNoHC < 0.0 && QUnitOutNoHC - QCoilHeatSP < -SmallLoad) {
                            HeatingLoad = true;
                            CoolingLoad = false;
                            QZnReq = QCoilHeatSP;
                            // zone pushed above cooling set point
                        } else if (QUnitOutNoHC > 0.0 && QUnitOutNoHC - QCoilCoolSP > SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = true;
                            QZnReq = QCoilCoolSP;
                        }
                    }
                } else {
                }
            }
            // IF small loads to meet, just shut down unit
            if (std::abs(QZnReq) < state.dataFanCoilUnits->Small5WLoad) {
                QZnReq = 0.0;
                CoolingLoad = false;
                HeatingLoad = false;
            }
        }

        if (UnitOn && QZnReq < (-1.0 * state.dataFanCoilUnits->Small5WLoad) && CoolingLoad) {
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                     FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).HeatCoilLoopNum,
                                     FanCoil(FanCoilNum).HeatCoilLoopSide,
                                     FanCoil(FanCoilNum).HeatCoilBranchNum,
                                     FanCoil(FanCoilNum).HeatCoilCompNum);
            }
            mdot = FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
            SetComponentFlowRate(state,
                                 mdot,
                                 FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                 FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                 FanCoil(FanCoilNum).CoolCoilLoopNum,
                                 FanCoil(FanCoilNum).CoolCoilLoopSide,
                                 FanCoil(FanCoilNum).CoolCoilBranchNum,
                                 FanCoil(FanCoilNum).CoolCoilCompNum);
            // select fan speed
            FanCoil(FanCoilNum).SpeedFanSel = 1;
            FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).LowSpeedRatio;
            state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
            AirMassFlow = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = AirMassFlow;
            Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax);
            if (std::abs(QUnitOutMax) < std::abs(QZnReq)) {
                FanCoil(FanCoilNum).SpeedFanSel = 2;
                FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).MedSpeedRatio;
                state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
                AirMassFlow = FanCoil(FanCoilNum).MedSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax);
            }
            if (std::abs(QUnitOutMax) < std::abs(QZnReq)) {
                FanCoil(FanCoilNum).SpeedFanSel = 3;
                FanCoil(FanCoilNum).SpeedFanRatSel = 1.0;
                state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
                AirMassFlow = FanCoil(FanCoilNum).MaxAirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = FanCoil(FanCoilNum).MedSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
            }
            CalcMultiStage4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut);

        } else if (UnitOn && QZnReq > state.dataFanCoilUnits->Small5WLoad && HeatingLoad) {

            mdot = 0.0;
            SetComponentFlowRate(state,
                                 mdot,
                                 FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                 FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                 FanCoil(FanCoilNum).CoolCoilLoopNum,
                                 FanCoil(FanCoilNum).CoolCoilLoopSide,
                                 FanCoil(FanCoilNum).CoolCoilBranchNum,
                                 FanCoil(FanCoilNum).CoolCoilCompNum);

            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                mdot = FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                     FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).HeatCoilLoopNum,
                                     FanCoil(FanCoilNum).HeatCoilLoopSide,
                                     FanCoil(FanCoilNum).HeatCoilBranchNum,
                                     FanCoil(FanCoilNum).HeatCoilCompNum);
            }
            // select fan speed
            FanCoil(FanCoilNum).SpeedFanSel = 1;
            FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).LowSpeedRatio;
            state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
            AirMassFlow = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = AirMassFlow;
            Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax);
            if (std::abs(QUnitOutMax) < std::abs(QZnReq)) {
                FanCoil(FanCoilNum).SpeedFanSel = 2;
                FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).MedSpeedRatio;
                state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
                AirMassFlow = FanCoil(FanCoilNum).MedSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax);
            }
            if (std::abs(QUnitOutMax) < std::abs(QZnReq)) {
                FanCoil(FanCoilNum).SpeedFanSel = 3;
                FanCoil(FanCoilNum).SpeedFanRatSel = 1.0;
                state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
                AirMassFlow = FanCoil(FanCoilNum).MaxAirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = FanCoil(FanCoilNum).MedSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
            }

            CalcMultiStage4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut);

        } else {
            // SpeedRatio = 0.0;
            if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                PartLoadRatio = 1.0;
                FanCoil(FanCoilNum).SpeedFanSel = 1;
                FanCoil(FanCoilNum).SpeedFanRatSel = FanCoil(FanCoilNum).LowSpeedRatio;
                state.dataFanCoilUnits->FanFlowRatio = FanCoil(FanCoilNum).SpeedFanRatSel;
                AirMassFlow = FanCoil(FanCoilNum).LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = AirMassFlow;
            } else {
                PartLoadRatio = 0.0;
                AirMassFlow = 0.0;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = AirMassFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
                state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;
                FanCoil(FanCoilNum).SpeedFanSel = 0;
                state.dataFanCoilUnits->FanFlowRatio = 0.0;
            }

            mdot = 0.0;
            if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                SetComponentFlowRate(state,
                                     mdot,
                                     FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                     FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                     FanCoil(FanCoilNum).HeatCoilLoopNum,
                                     FanCoil(FanCoilNum).HeatCoilLoopSide,
                                     FanCoil(FanCoilNum).HeatCoilBranchNum,
                                     FanCoil(FanCoilNum).HeatCoilCompNum);
            }
            SetComponentFlowRate(state,
                                 mdot,
                                 FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                 FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                 FanCoil(FanCoilNum).CoolCoilLoopNum,
                                 FanCoil(FanCoilNum).CoolCoilLoopSide,
                                 FanCoil(FanCoilNum).CoolCoilBranchNum,
                                 FanCoil(FanCoilNum).CoolCoilCompNum);
            // No load output, eHeatCoilCyclingR = 0.0 for electric heating coil
            Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PartLoadRatio, 0.0);
        }
        // output variable
        state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        FanCoil(FanCoilNum).PLR = PartLoadRatio;
        FanCoil(FanCoilNum).SpeedRatio = SpeedRatio;
        PowerMet = QUnitOut;
    }

    void CalcMultiStage4PipeFanCoil(EnergyPlusData &state,
                                    int &FanCoilNum,               // number of the current fan coil unit being simulated
                                    int const ZoneNum,             // number of zone being served
                                    bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                    Real64 const QZnReq,           // current zone cooling or heating load
                                    Real64 &SpeedRatio,            // fan coil speed ratio
                                    Real64 &PartLoadRatio,         // fan coil part load ratio
                                    Real64 &PowerMet               // Sensible power supplied (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a multi-stage fan 4 pipe fan coil unit; adjust its output to
        // match the remaining zone load.

        // METHODOLOGY EMPLOYED:
        // If this unit is on, calculated the speed ratio when cycling between
        // consecutive fan speeds. The hot or chilled water flows either at
        // maximum or zero.  The water flow rate is set to zero if there is no
        // load.

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;

        using PlantUtilities::SetComponentFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcMultiStage4PipeFanCoil");
        int const MaxIterCycl(100);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PLR;             // Part Load Ratio, fraction of time step fancoil is on
        Real64 SRatio;          // capacity speed ratio of the for multi-stage fan fancoil unit
        Real64 mdot;            // chilled or hot water flow rate through the water coils
        Real64 QUnitOut;        // heating or sens. cooling provided by fan coil unit [watts]
        Real64 QUnitOutMax;     // max heating or sens. cooling provided by fan coil unit [watts]
        Real64 ControlOffset;   // tolerance for output control
        Real64 QUnitOutMaxHS;   // higher fan speed output
        Real64 QUnitOutMaxLS;   // lower fan speed output
        Real64 HighSpeedRatio;  // fan flow ratio at low speed
        Real64 LowSpeedRatio;   // fan flow ratio at low speed
        Real64 AirMassFlowAvg;  // supply air flow rate weighted by speed ratio
        Real64 AirMassFlowLow;  // supply air flow rate at lower speed
        Real64 AirMassFlowHigh; // supply air flow rate at higher speed
        Real64 FanElecPowerHS;  // fan electric power calculated at (fan) higher speed
        Real64 FanElecPowerLS;  // fan electric power calculated at (fan) lower speed
        Real64 Error;           // Error between QZnReq and QUnitOut
        Real64 AbsError;        // Absolute error between QZnReq and QUnitOut [W]   !FB
        Real64 Relax;
        Real64 DelPLR;
        int OutletNode;          // unit air outlet node
        int InletNode;           // unit air inlet node
        int Iter;                // iteration counter
        int SolFlag;             // return flag from RegulaFalsi for sensible load
        Array1D<Real64> Par(10); // parameters passed to RegulaFalsi function

        auto &FanCoil(state.dataFanCoilUnits->FanCoil);

        // initialize local variables
        mdot = 0.0;
        PLR = 1.0;
        SRatio = 0.0;
        QUnitOut = 0.0;
        QUnitOutMax = 0.0;
        ControlOffset = 0.0;
        FanElecPowerHS = 0.0;
        FanElecPowerLS = 0.0;
        AirMassFlowAvg = 0.0;
        AirMassFlowLow = 0.0;
        AirMassFlowHigh = 0.0;
        AbsError = 2.0 * state.dataFanCoilUnits->Small5WLoad;
        Error = 1.0;
        Relax = 1.0;
        Iter = 0;

        OutletNode = FanCoil(FanCoilNum).AirOutNode;
        InletNode = FanCoil(FanCoilNum).AirInNode;

        auto &Node(state.dataLoopNodes->Node);

        if (QZnReq < (-1.0 * state.dataFanCoilUnits->Small5WLoad) && state.dataFanCoilUnits->CoolingLoad) {
            ControlOffset = FanCoil(FanCoilNum).ColdControlOffset;
            if (FanCoil(FanCoilNum).SpeedFanSel == 1) {
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax);
                PLR = std::abs(QZnReq / QUnitOutMax);
                if (PLR > 1.0) PLR = 1.0;
                // adjust the PLR to meet the cooling load by calling Calc4PipeFanCoil repeatedly
                while (std::abs(Error) > ControlOffset && std::abs(AbsError) > state.dataFanCoilUnits->Small5WLoad && Iter < MaxIterCycl &&
                       PLR != 1.0) {
                    Node(InletNode).MassFlowRateMinAvail = Node(InletNode).MassFlowRate;
                    mdot = PLR * FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                                         FanCoil(FanCoilNum).CoolCoilFluidOutletNodeNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopNum,
                                         FanCoil(FanCoilNum).CoolCoilLoopSide,
                                         FanCoil(FanCoilNum).CoolCoilBranchNum,
                                         FanCoil(FanCoilNum).CoolCoilCompNum);
                    if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                    } else {
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
                    }
                    Error = (QZnReq - QUnitOut) / QZnReq;
                    AbsError = QZnReq - QUnitOut;
                    DelPLR = (QZnReq - QUnitOut) / QUnitOutMax;
                    PLR += Relax * DelPLR;
                    PLR = max(0.0, min(1.0, PLR));
                    ++Iter;
                    if (Iter == 32) Relax = 0.5;
                    if (Iter == 65) Relax = 0.25;
                    if (Iter > 70 && PLR == 0.0 && DelPLR < 0.0) Error = 0.0;
                }
                if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                    Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                } else {
                    Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
                }
                // warning if not converged
                if (Iter > (MaxIterCycl - 1)) {
                    if (FanCoil(FanCoilNum).MaxIterIndexC == 0) {
                        ShowWarningMessage(
                            state,
                            "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within "
                                "the cooling convergence tolerance.");
                        ShowContinueErrorTimeStamp(state, format("Iterations={}", MaxIterCycl));
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                                       "\"  -- Exceeded max iterations error (sensible runtime) continues...",
                                                   FanCoil(FanCoilNum).MaxIterIndexC);
                }

            } else {
                if (FanCoil(FanCoilNum).SpeedFanSel == 2) {
                    HighSpeedRatio = FanCoil(FanCoilNum).MedSpeedRatio;
                    LowSpeedRatio = FanCoil(FanCoilNum).LowSpeedRatio;
                } else {
                    HighSpeedRatio = 1;
                    LowSpeedRatio = FanCoil(FanCoilNum).MedSpeedRatio;
                }
                // get capacity at lower speed
                FanCoil(FanCoilNum).SpeedFanRatSel = LowSpeedRatio;
                FanCoil(FanCoilNum).SpeedFanSel = FanCoil(FanCoilNum).SpeedFanSel - 1;
                AirMassFlowLow = LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                Node(InletNode).MassFlowRate = AirMassFlowLow;
                Node(InletNode).MassFlowRateMax = AirMassFlowLow;
                Node(InletNode).MassFlowRateMaxAvail = AirMassFlowLow;
                Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                state.dataFanCoilUnits->FanFlowRatio = LowSpeedRatio;
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxLS);
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanElecPowerLS = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanElecPowerLS = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                // get capacity at higher speed
                FanCoil(FanCoilNum).SpeedFanRatSel = HighSpeedRatio;
                FanCoil(FanCoilNum).SpeedFanSel = FanCoil(FanCoilNum).SpeedFanSel + 1;
                AirMassFlowHigh = HighSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                Node(InletNode).MassFlowRate = AirMassFlowHigh;
                Node(InletNode).MassFlowRateMax = AirMassFlowHigh;
                Node(InletNode).MassFlowRateMaxAvail = AirMassFlowHigh;
                Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio;
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxHS);
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanElecPowerHS = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanElecPowerHS = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                // calc speed ratio
                if (std::abs(QZnReq) > std::abs(QUnitOutMaxHS)) {
                    SRatio = 1.0;
                    AirMassFlowAvg = AirMassFlowHigh;
                    Node(InletNode).MassFlowRate = AirMassFlowHigh;
                    Node(InletNode).MassFlowRateMax = AirMassFlowHigh;
                    Node(InletNode).MassFlowRateMaxAvail = AirMassFlowHigh;
                    Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                    state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio;
                    Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                } else {
                    SRatio = std::abs((QZnReq - QUnitOutMaxLS) / (QUnitOutMaxHS - QUnitOutMaxLS));
                    if (SRatio > 1.0) SRatio = 1.0;
                    AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * (1.0 - SRatio);
                    Node(InletNode).MassFlowRate = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMax = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMaxAvail = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                    state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * (1.0 - SRatio);
                    Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                    // adjust the PLR to meet the cooling load by calling Calc4PipeFanCoil repeatedly
                    while (std::abs(Error) > ControlOffset && std::abs(AbsError) > state.dataFanCoilUnits->Small5WLoad && Iter < MaxIterCycl &&
                           SRatio != 1.0) {
                        AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * (1.0 - SRatio);
                        state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * (1.0 - SRatio);
                        Node(InletNode).MassFlowRate = AirMassFlowAvg;
                        Node(InletNode).MassFlowRateMax = AirMassFlowAvg;
                        Node(InletNode).MassFlowRateMaxAvail = AirMassFlowAvg;
                        Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                        Error = (QZnReq - QUnitOut) / QZnReq;
                        AbsError = QZnReq - QUnitOut;
                        DelPLR = (QZnReq - QUnitOut) / (QUnitOutMaxHS - QUnitOutMaxLS);
                        SRatio += Relax * DelPLR;
                        SRatio = max(0.0, min(1.0, SRatio));
                        ++Iter;
                        if (Iter == 32) Relax = 0.5;
                        if (Iter == 65) Relax = 0.25;
                        if (Iter > 70 && SRatio == 0.0 && DelPLR < 0.0) Error = 0.0;
                    }
                }
            }
        } else if (QZnReq > state.dataFanCoilUnits->Small5WLoad && state.dataFanCoilUnits->HeatingLoad) {
            ControlOffset = FanCoil(FanCoilNum).HotControlOffset;
            if (FanCoil(FanCoilNum).SpeedFanSel == 1) {
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax);
                PLR = std::abs(QZnReq / QUnitOutMax);
                if (PLR > 1.0) PLR = 1.0;
                if (FanCoil(FanCoilNum).HCoilType_Num == HCoil::Water) {
                    // adjust the PLR to meet the heating load by calling Calc4PipeFanCoil repeatedly
                    while (std::abs(Error) > ControlOffset && std::abs(AbsError) > state.dataFanCoilUnits->Small5WLoad && Iter < MaxIterCycl &&
                           PLR != 1.0) {
                        Node(InletNode).MassFlowRateMinAvail = Node(InletNode).MassFlowRate;
                        mdot = PLR * FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
                        SetComponentFlowRate(state,
                                             mdot,
                                             FanCoil(FanCoilNum).HeatCoilFluidInletNode,
                                             FanCoil(FanCoilNum).HeatCoilFluidOutletNodeNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopNum,
                                             FanCoil(FanCoilNum).HeatCoilLoopSide,
                                             FanCoil(FanCoilNum).HeatCoilBranchNum,
                                             FanCoil(FanCoilNum).HeatCoilCompNum);
                        if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                            Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                        } else {
                            Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
                        }
                        Error = (QZnReq - QUnitOut) / QZnReq;
                        AbsError = QZnReq - QUnitOut;
                        DelPLR = (QZnReq - QUnitOut) / QUnitOutMax;
                        PLR += Relax * DelPLR;
                        PLR = max(0.0, min(1.0, PLR));
                        ++Iter;
                        if (Iter == 32) Relax = 0.5;
                        if (Iter == 65) Relax = 0.25;
                        if (Iter > 70 && PLR == 0.0 && DelPLR < 0.0) Error = 0.0; // exit loop if PLR = 0
                    }
                    if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                    } else {
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
                    }
                    // warning if not converged
                    if (Iter > (MaxIterCycl - 1)) {
                        if (FanCoil(FanCoilNum).MaxIterIndexH == 0) {
                            ShowWarningMessage(
                                state,
                                "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                    "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within "
                                    "the heating convergence tolerance.");
                            ShowContinueErrorTimeStamp(state, format("Iterations={}", MaxIterCycl));
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil(FanCoilNum).Name +
                                                           "\"  -- Exceeded max iterations error (sensible runtime) continues...",
                                                       FanCoil(FanCoilNum).MaxIterIndexH);
                    }
                } else {
                    Real64 eHeatCoilPLR = PLR;
                    // electric heating coil
                    if (QUnitOutMax > QZnReq) {
                        // heating coil output is larger than required, mudulate the electric heating coil output to meet the load
                        Node(InletNode).MassFlowRateMinAvail = Node(InletNode).MassFlowRate;
                        Par(1) = double(FanCoilNum);
                        Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                        if (FirstHVACIteration) Par(2) = 1.0;
                        Par(3) = ZoneNum;
                        Par(4) = QZnReq;
                        if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, eHeatCoilPLR, CalcFanCoilHeatCoilPLRResidual, 0.0, 1.0, Par);
                        } else {
                            General::SolveRoot(state, 0.001, MaxIterCycl, SolFlag, eHeatCoilPLR, CalcFanCoilLoadResidual, 0.0, 1.0, Par);
                        }
                        if (SolFlag == -1) {
                            ++FanCoil(FanCoilNum).ConvgErrCountH;
                            if (FanCoil(FanCoilNum).ConvgErrCountH < 2) {
                                ShowWarningError(state, "Electric heating coil control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, "  Iteration limit exceeded in calculating electric heating coil capacity modulation ");
                                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, _, eHeatCoilPLR);
                                ShowContinueErrorTimeStamp(state, format("Load Request = {}, Final Capacity = {}", QZnReq, QUnitOut));
                                ShowContinueErrorTimeStamp(
                                    state, format("Electric heating coil part load ratio used during last iterations = {}", eHeatCoilPLR));
                            } else {
                                ShowRecurringWarningErrorAtEnd(state,
                                                               "Electric heating coil Iteration limit exceeded in fan coil unit " +
                                                                   FanCoil(FanCoilNum).Name,
                                                               FanCoil(FanCoilNum).MaxIterIndexH);
                            }
                        } else if (SolFlag == -2) {
                            ++FanCoil(FanCoilNum).LimitErrCountH;
                            if (FanCoil(FanCoilNum).LimitErrCountH < 2) {
                                ShowWarningError(state,
                                                 "Part load ratio electric heating coil control failed in fan coil unit " + FanCoil(FanCoilNum).Name);
                                ShowContinueError(state, "  Bad par load ratio limits");
                                ShowContinueErrorTimeStamp(state, "..Par load ratio set to 0");
                            } else {
                                ShowRecurringWarningErrorAtEnd(state,
                                                               "Part load ratio electric heating coil control failed in fan coil unit " +
                                                                   FanCoil(FanCoilNum).Name,
                                                               FanCoil(FanCoilNum).BadMassFlowLimIndexH);
                            }
                        }
                    } else {
                        eHeatCoilPLR = 1.0;
                    }
                    PLR = eHeatCoilPLR;
                    // at the end calculate output
                    if (FanCoil(FanCoilNum).FanOpMode == ContFanCycCoil) {
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, _, eHeatCoilPLR);
                    } else {
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
                    }
                }

            } else {
                if (FanCoil(FanCoilNum).SpeedFanSel == 2) {
                    HighSpeedRatio = FanCoil(FanCoilNum).MedSpeedRatio;
                    LowSpeedRatio = FanCoil(FanCoilNum).LowSpeedRatio;
                } else {
                    HighSpeedRatio = 1;
                    LowSpeedRatio = FanCoil(FanCoilNum).MedSpeedRatio;
                }
                // get capacity at lower speed ratio
                FanCoil(FanCoilNum).SpeedFanRatSel = LowSpeedRatio;
                FanCoil(FanCoilNum).SpeedFanSel = FanCoil(FanCoilNum).SpeedFanSel - 1;
                AirMassFlowLow = LowSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                Node(InletNode).MassFlowRate = AirMassFlowLow;
                Node(InletNode).MassFlowRateMax = AirMassFlowLow;
                Node(InletNode).MassFlowRateMaxAvail = AirMassFlowLow;
                Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                state.dataFanCoilUnits->FanFlowRatio = LowSpeedRatio;
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxLS);
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanElecPowerLS = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanElecPowerLS = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                // get capacity at higher speed
                FanCoil(FanCoilNum).SpeedFanRatSel = HighSpeedRatio;
                FanCoil(FanCoilNum).SpeedFanSel = FanCoil(FanCoilNum).SpeedFanSel + 1;
                AirMassFlowHigh = HighSpeedRatio * FanCoil(FanCoilNum).MaxAirMassFlow;
                Node(InletNode).MassFlowRate = AirMassFlowHigh;
                Node(InletNode).MassFlowRateMax = AirMassFlowHigh;
                Node(InletNode).MassFlowRateMaxAvail = AirMassFlowHigh;
                Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio;
                Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxHS);
                if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    FanElecPowerHS = Fans::GetFanPower(state, FanCoil(FanCoilNum).FanIndex);
                } else {
                    FanElecPowerHS = state.dataHVACFan->fanObjs[FanCoil(FanCoilNum).FanIndex]->fanPower();
                }
                // calc speed ratio
                if (std::abs(QZnReq) > std::abs(QUnitOutMaxHS)) {
                    SRatio = 1.0;
                    AirMassFlowAvg = AirMassFlowHigh;
                    Node(InletNode).MassFlowRate = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMax = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMaxAvail = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                    state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio;
                    Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                } else {
                    SRatio = std::abs((QZnReq - QUnitOutMaxLS) / (QUnitOutMaxHS - QUnitOutMaxLS));
                    if (SRatio > 1.0) SRatio = 1.0;
                    AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * (1.0 - SRatio);
                    Node(InletNode).MassFlowRate = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMax = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMaxAvail = AirMassFlowAvg;
                    Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                    state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * (1.0 - SRatio);
                    Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                    ControlOffset = FanCoil(FanCoilNum).HotControlOffset;
                    // adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly
                    while (std::abs(Error) > ControlOffset && std::abs(AbsError) > state.dataFanCoilUnits->Small5WLoad && Iter < MaxIterCycl &&
                           SRatio != 1.0) {
                        AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * (1.0 - SRatio);
                        Node(InletNode).MassFlowRate = AirMassFlowAvg;
                        Node(InletNode).MassFlowRateMax = AirMassFlowAvg;
                        Node(InletNode).MassFlowRateMaxAvail = AirMassFlowAvg;
                        Node(InletNode).MassFlowRateMinAvail = AirMassFlowLow;
                        state.dataFanCoilUnits->FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * (1.0 - SRatio);
                        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut);
                        Error = (QZnReq - QUnitOut) / QZnReq;
                        AbsError = QZnReq - QUnitOut;
                        DelPLR = (QZnReq - QUnitOut) / (QUnitOutMaxHS - QUnitOutMaxLS);
                        SRatio += Relax * DelPLR;
                        SRatio = max(0.0, min(1.0, SRatio));
                        ++Iter;
                        if (Iter == 32) Relax = 0.5;
                        if (Iter == 65) Relax = 0.25;
                        if (Iter > 70 && SRatio == 0.0 && DelPLR < 0.0) Error = 0.0;
                    }
                }
                // FanElecPower = FanElecPowerHS * SRatio + FanElecPowerLS * ( 1.0 - SRatio ); // why set the ugly global here?
                FanCoil(FanCoilNum).ElecPower = FanElecPowerHS * SRatio + FanElecPowerLS * (1.0 - SRatio);
            }
        }
        Node(OutletNode).MassFlowRate = Node(InletNode).MassFlowRate;
        PartLoadRatio = PLR;
        SpeedRatio = SRatio;
        PowerMet = QUnitOut;
    }

    void ReportFanCoilUnit(EnergyPlusData &state, int const FanCoilNum) // number of the current fan coil unit being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Fills some of the report variables for the fan coil units

        // METHODOLOGY EMPLOYED:
        // NA

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ReportingConstant;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatEnergy = state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatPower * ReportingConstant;
        state.dataFanCoilUnits->FanCoil(FanCoilNum).SensCoolEnergy = state.dataFanCoilUnits->FanCoil(FanCoilNum).SensCoolPower * ReportingConstant;
        state.dataFanCoilUnits->FanCoil(FanCoilNum).TotCoolEnergy = state.dataFanCoilUnits->FanCoil(FanCoilNum).TotCoolPower * ReportingConstant;
        state.dataFanCoilUnits->FanCoil(FanCoilNum).ElecEnergy = state.dataFanCoilUnits->FanCoil(FanCoilNum).ElecPower * ReportingConstant;

        if (state.dataFanCoilUnits->FanCoil(FanCoilNum).FirstPass) { // reset sizing flags so other zone equipment can size normally
            if (!state.dataGlobal->SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(state, state.dataSize->CurZoneEqNum, 0, state.dataFanCoilUnits->FanCoil(FanCoilNum).FirstPass);
            }
        }
    }

    int GetFanCoilZoneInletAirNode(EnergyPlusData &state, int const FanCoilNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node for ventilation rate reporting

        // Return value
        int GetFanCoilZoneInletAirNode;

        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        GetFanCoilZoneInletAirNode = 0;
        if (FanCoilNum > 0 && FanCoilNum <= state.dataFanCoilUnits->NumFanCoils) {
            GetFanCoilZoneInletAirNode = state.dataFanCoilUnits->FanCoil(FanCoilNum).AirOutNode;
        }

        return GetFanCoilZoneInletAirNode;
    }

    int GetFanCoilOutAirNode(EnergyPlusData &state, int const FanCoilNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node for ventilation rate reporting

        // Return value
        int GetFanCoilOutAirNode;

        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        GetFanCoilOutAirNode = 0;
        if (FanCoilNum > 0 && FanCoilNum <= state.dataFanCoilUnits->NumFanCoils) {
            GetFanCoilOutAirNode = state.dataFanCoilUnits->FanCoil(FanCoilNum).OutsideAirNode;
        }

        return GetFanCoilOutAirNode;
    }

    int GetFanCoilReturnAirNode(EnergyPlusData &state, int const FanCoilNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for mixer's return node

        // Using/Aliasing
        using MixedAir::GetOAMixerReturnNodeNumber;

        // Return value
        int GetFanCoilReturnAirNode;

        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        GetFanCoilReturnAirNode = 0;
        if (FanCoilNum > 0 && FanCoilNum <= state.dataFanCoilUnits->NumFanCoils) {
            if (state.dataFanCoilUnits->FanCoil(FanCoilNum).OAMixIndex > 0) {
                GetFanCoilReturnAirNode = GetOAMixerReturnNodeNumber(state, state.dataFanCoilUnits->FanCoil(FanCoilNum).OAMixIndex);
            } else {
                GetFanCoilReturnAirNode = 0;
            }
        }

        return GetFanCoilReturnAirNode;
    }

    int GetFanCoilMixedAirNode(EnergyPlusData &state, int const FanCoilNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for mixer's return node

        // Using/Aliasing
        using MixedAir::GetOAMixerMixedNodeNumber;

        // Return value
        int GetFanCoilMixedAirNode;

        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        GetFanCoilMixedAirNode = 0;
        if (FanCoilNum > 0 && FanCoilNum <= state.dataFanCoilUnits->NumFanCoils) {
            if (state.dataFanCoilUnits->FanCoil(FanCoilNum).OAMixIndex > 0) {
                GetFanCoilMixedAirNode = GetOAMixerMixedNodeNumber(state, state.dataFanCoilUnits->FanCoil(FanCoilNum).OAMixIndex);
            } else {
                GetFanCoilMixedAirNode = 0;
            }
        }

        return GetFanCoilMixedAirNode;
    }

    int GetFanCoilInletAirNode(EnergyPlusData &state, int const FanCoilNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for inlet node for Fan Coil unit

        // Return value
        int GetFanCoilInletAirNode;

        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        GetFanCoilInletAirNode = 0;
        if (FanCoilNum > 0 && FanCoilNum <= state.dataFanCoilUnits->NumFanCoils) {
            GetFanCoilInletAirNode = state.dataFanCoilUnits->FanCoil(FanCoilNum).AirOutNode;
        }

        return GetFanCoilInletAirNode;
    }

    void GetFanCoilIndex(EnergyPlusData &state, std::string const &FanCoilName, int &FanCoilIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN   April 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the index for a given PT Unit

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        bool ErrorsFound; // for error trapping

        if (state.dataFanCoilUnits->GetFanCoilInputFlag) {
            GetFanCoilUnits(state);
            state.dataFanCoilUnits->GetFanCoilInputFlag = false;
        }

        FanCoilIndex = UtilityRoutines::FindItemInList(FanCoilName, state.dataFanCoilUnits->FanCoil);
        if (FanCoilIndex == 0) {
            ShowSevereError(state, "GetFanCoilIndex: Fan Coil Unit not found=" + FanCoilName);
        }
        ErrorsFound = true;
    }

    Real64 CalcFanCoilLoadResidual(EnergyPlusData &state,
                                   Real64 const PartLoadRatio, // coil part load ratio
                                   Array1D<Real64> const &Par  // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with electric heating coil

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (force to 0)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);

        Calc4PipeFanCoil(state,
                         FanCoilNum,
                         ControlledZoneNum,
                         FirstHVACIteration,
                         QUnitOut,
                         PartLoadRatio); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilPLRResidual(EnergyPlusData &state,
                                  Real64 const PLR,          // part-load ratio of air and water mass flow rate
                                  Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   August 2016

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (force to 0)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64)        ! Index to fan coil unit
        //       Par(2)  = 0.0                         ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64) ! zone index
        //       Par(4)  = QZnReq                      ! zone load [W]
        //       Par(5)  = REAL(WaterControlNode, r64) ! water coil control node number

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        int WaterControlNode;    // water node to control
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);
        WaterControlNode = int(Par(5));

        if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxCoolCoilFluidFlow;
            Calc4PipeFanCoil(state,
                             FanCoilNum,
                             ControlledZoneNum,
                             FirstHVACIteration,
                             QUnitOut,
                             PLR); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
        } else if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatCoilFluidInletNode &&
                   state.dataFanCoilUnits->FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = PLR * state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxHeatCoilFluidFlow;
            Calc4PipeFanCoil(state,
                             FanCoilNum,
                             ControlledZoneNum,
                             FirstHVACIteration,
                             QUnitOut,
                             PLR); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
        } else {
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR); // needs PLR=1 for electric heating coil
        }

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilHeatCoilPLRResidual(EnergyPlusData &state,
                                          Real64 const CyclingR,     // electric heating coil cycling ratio
                                          Array1D<Real64> const &Par // Function parameters
    )
    {
        // PURPOSE OF THIS SUBROUTINE:
        // Calculate electric heating coil cycling ratio of FanCoilUnit with MultiSpeedFan
        // capacity control method when running with at lowest speed for a continuous fan
        // fan operating mode.

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (force to 0)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ZoneNum;             // controoled zone index
        int WaterControlNode;    // water node to control
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]
        Real64 PLR;              // fan coil unit PLR

        // convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ZoneNum = int(Par(3));
        QZnReq = Par(4);
        WaterControlNode = int(Par(5));
        PLR = 1.0;

        // electric heating coil cycling ratio at minimum air flow for constant fan operating mode
        Calc4PipeFanCoil(state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR, CyclingR);

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilHWLoadResidual(EnergyPlusData &state,
                                     Real64 const HWFlow,       // water mass flow rate [kg/s]
                                     Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl, FSEC
        //       DATE WRITTEN   Jan 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with electric heating coil

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // Result (force to 0)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);

        state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate = HWFlow;
        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0);

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilCWLoadResidual(EnergyPlusData &state,
                                     Real64 const CWFlow,       // water mass flow rate [kg/s]
                                     Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl Jan 2016
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with electric heating coil

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (force to 0)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);

        state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).CoolCoilFluidInletNode).MassFlowRate = CWFlow;
        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0);

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }
    Real64 CalcFanCoilWaterFlowTempResidual(EnergyPlusData &state,
                                            Real64 const WaterFlow,    // water mass flow rate [kg/s]
                                            Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:

        // Use SolveRoot to CALL this Function to converge on a solution

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)  = WaterControlNode     ! CW or HW control node number

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        int WaterControlNode;    // water node to control
        Real64 OutletTemp;       // FCU outlet temperature SP [C]
        Real64 QUnitOut;         // delivered capacity [W]
        Real64 QZnReq;
        Real64 FCOutletTempOn; // FCU outlet temperature

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        OutletTemp = Par(4);
        QZnReq = Par(5);
        WaterControlNode = int(Par(6));

        if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).CoolCoilFluidInletNode ||
            (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatCoilFluidInletNode &&
             state.dataFanCoilUnits->FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric)) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = WaterFlow;
            Calc4PipeFanCoil(state,
                             FanCoilNum,
                             ControlledZoneNum,
                             FirstHVACIteration,
                             QUnitOut,
                             0.0); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
        } else {
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0); // needs PLR=1 for electric heating coil
        }

        FCOutletTempOn = state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).AirOutNode).Temp;
        // Calculate residual based on output magnitude
        Residuum = (FCOutletTempOn - OutletTemp);

        return Residuum;
    }

    Real64 CalcFanCoilWaterFlowResidual(EnergyPlusData &state,
                                        Real64 const PLR,          // coil part load ratio
                                        Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:

        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)  = WaterControlNode     ! CW or HW control node number

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        int WaterControlNode;    // water node to control
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);
        int AirInNode = int(Par(5));
        WaterControlNode = int(Par(8));
        Real64 maxCoilFluidFlow = Par(10);
        Real64 AirMassFlowRate = Par(12);
        Real64 mDot = PLR * maxCoilFluidFlow;
        if (WaterControlNode > 0) state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = mDot;
        state.dataLoopNodes->Node(AirInNode).MassFlowRate = AirMassFlowRate;

        if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).CoolCoilFluidInletNode ||
            (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatCoilFluidInletNode &&
             state.dataFanCoilUnits->FanCoil(FanCoilNum).HCoilType_Num != HCoil::Electric)) {

            Calc4PipeFanCoil(state,
                             FanCoilNum,
                             ControlledZoneNum,
                             FirstHVACIteration,
                             QUnitOut,
                             0.0); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

        } else {
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
        }

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilAirAndWaterFlowResidual(EnergyPlusData &state,
                                              Real64 const PLR,          // water and air part load ratio
                                              Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)  = AirInNode            ! cooling or heating coil inlet air node
        //       Par(8)  = WaterControlNode     ! CW or HW control node number

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool FirstHVACIteration; // FirstHVACIteration flag
        Real64 QUnitOut;         // delivered capacity [W]

        // Convert parameters to usable variables
        int FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        int ControlledZoneNum = int(Par(3));
        Real64 QZnReq = Par(4);
        int WaterControlNode = int(Par(8));
        int AirInNode = Par(5);
        Real64 MinWaterFlow = Par(9); // min water flow at low air flow rate of 0 if that calculation failed

        // set air flow rate
        state.dataLoopNodes->Node(AirInNode).MassFlowRate =
            state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxAirMassFlow *
            (state.dataFanCoilUnits->FanCoil(FanCoilNum).LowSpeedRatio + (PLR * (1.0 - state.dataFanCoilUnits->FanCoil(FanCoilNum).LowSpeedRatio)));
        // set water flow rate
        if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate =
                MinWaterFlow + (PLR * (state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxCoolCoilFluidFlow - MinWaterFlow));
        } else if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatCoilFluidInletNode) {
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate =
                MinWaterFlow + (PLR * (state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxHeatCoilFluidFlow - MinWaterFlow));
        } else {
            // developer error
            ShowFatalError(state,
                           "Developer Error - CalcFanCoilAirAndWaterFlowResidual: Water control node not found for " +
                               state.dataFanCoilUnits->FanCoil(FanCoilNum).Name);
        }
        Calc4PipeFanCoil(state,
                         FanCoilNum,
                         ControlledZoneNum,
                         FirstHVACIteration,
                         QUnitOut,
                         PLR); // needs PLR for electric heating coil to output a specific capacity

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilAirAndWaterInStepResidual(EnergyPlusData &state,
                                                Real64 const PLR,          // water and air part load ratio
                                                Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)  = WaterControlNode     ! CW or HW control node number

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        int WaterControlNode;    // water node to control
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]
        Real64 MinWaterFlow;     // water flow rate that meets zone load
        Real64 MinAirFlow;       // air flow rate that meets zone load
        Real64 MinHeaterPLR;     // PLR of heating coil prior to increasing fan coil capacity

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);
        WaterControlNode = int(Par(5));
        MinAirFlow = Par(7);

        // set air flow rate
        state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).AirInNode).MassFlowRate =
            MinAirFlow + (PLR * (state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxAirMassFlow - MinAirFlow));
        // set water flow rate
        if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).CoolCoilFluidInletNode) {
            MinWaterFlow = Par(6);
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate =
                MinWaterFlow + (PLR * (state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxCoolCoilFluidFlow - MinWaterFlow));
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0);
        } else if (WaterControlNode == 0) { // do this before the water coil else if block because 0 = 0
            MinHeaterPLR = Par(6);
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, MinHeaterPLR + (PLR * (1.0 - MinHeaterPLR)));
        } else if (WaterControlNode == state.dataFanCoilUnits->FanCoil(FanCoilNum).HeatCoilFluidInletNode) {
            MinWaterFlow = Par(6);
            state.dataLoopNodes->Node(WaterControlNode).MassFlowRate =
                MinWaterFlow + (PLR * (state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxHeatCoilFluidFlow - MinWaterFlow));
            Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0);
        } else {
            // developer error
            ShowFatalError(state,
                           "Developer Error - CalcFanCoilAirAndWaterFlowResidual: Water control node not found for " +
                               state.dataFanCoilUnits->FanCoil(FanCoilNum).Name);
        }

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilBothFlowResidual(EnergyPlusData &state,
                                       Real64 const PLR,          // water and air part load ratio
                                       Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)  = Low_mdot             ! CW or HW minimum flow
        //       Par(6)  = mdot                 ! CW or HW maximum flow

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        int WaterControlNode;    // water node to control
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]
        Real64 MinWaterFlow;     // water flow rate that meets reduced zone load with reduced air flow rate
        Real64 MaxWaterFlow;     // water flow rate that meets design zone load with maximum air flow rate

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);
        MinWaterFlow = Par(5);
        MaxWaterFlow = Par(6);
        WaterControlNode = Par(7);

        // set air flow rate
        state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).AirInNode).MassFlowRate =
            state.dataFanCoilUnits->FanCoil(FanCoilNum).MaxAirMassFlow *
            (state.dataFanCoilUnits->FanCoil(FanCoilNum).LowSpeedRatio + (PLR * (1.0 - state.dataFanCoilUnits->FanCoil(FanCoilNum).LowSpeedRatio)));
        // set water flow rate
        state.dataLoopNodes->Node(WaterControlNode).MassFlowRate = MinWaterFlow + (PLR * (MaxWaterFlow - MinWaterFlow));
        Calc4PipeFanCoil(state,
                         FanCoilNum,
                         ControlledZoneNum,
                         FirstHVACIteration,
                         QUnitOut,
                         1.0); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilElecHeatResidual(EnergyPlusData &state,
                                       Real64 const PLR,          // water and air part load ratio
                                       Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)                         ! not applicable

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        Real64 QZnReq;           // Sensible load to be met [W]
        Real64 QUnitOut;         // delivered capacity [W]
        Real64 MaxAirFlow;       // maximum fan coil fan flow rate [kg/s]

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        QZnReq = Par(4);

        if (Par(6) == -1.0) {
            MaxAirFlow = Par(5);
            // set air flow rate
            state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).AirInNode).MassFlowRate = PLR * MaxAirFlow;
        }
        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);

        // Calculate residual based on output magnitude
        if (std::abs(QZnReq) <= 100.0) {
            Residuum = (QUnitOut - QZnReq) / 100.0;
        } else {
            Residuum = (QUnitOut - QZnReq) / QZnReq;
        }

        return Residuum;
    }

    Real64 CalcFanCoilElecHeatTempResidual(EnergyPlusData &state,
                                           Real64 const PLR,          // water and air part load ratio
                                           Array1D<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   December 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the FCU with varying water flow rate

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        //   Parameter description example:
        //       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
        //       Par(4)  = QZnReq               ! zone load [W]
        //       Par(5)                         ! not applicable

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanCoilNum;          // Index to this fan coil unit
        bool FirstHVACIteration; // FirstHVACIteration flag
        int ControlledZoneNum;   // zone index
        Real64 MaxOutletTemp;    // maximum supply air Temperature [C]
        Real64 QUnitOut;         // delivered capacity [W]
        Real64 MaxAirFlow;       // maximum fan coil fan flow rate [kg/s]
        Real64 FCOutletTempOn;   // FCU outlet temperature

        // Convert parameters to usable variables
        FanCoilNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        ControlledZoneNum = int(Par(3));
        MaxOutletTemp = Par(4);

        if (Par(6) == -1.0) {
            MaxAirFlow = Par(5);
            // set air flow rate
            state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).AirInNode).MassFlowRate = max(Par(7), PLR * MaxAirFlow);
        }
        Calc4PipeFanCoil(state, FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR);
        FCOutletTempOn = state.dataLoopNodes->Node(state.dataFanCoilUnits->FanCoil(FanCoilNum).AirOutNode).Temp;

        // Calculate residual based on output magnitude
        Residuum = (FCOutletTempOn - MaxOutletTemp);

        return Residuum;
    }

} // namespace FanCoilUnits

} // namespace EnergyPlus
