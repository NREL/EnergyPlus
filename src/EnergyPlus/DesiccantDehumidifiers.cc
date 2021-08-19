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
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

namespace DesiccantDehumidifiers {

    // Module containing the routines dealing with dehumidifiers

    // MODULE INFORMATION:
    //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
    //                      for Gas Research Institute
    //       DATE WRITTEN   March 2001
    //       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
    //                        Add new control type option:
    //                          NODE LEAVING HUMRAT SETPOINT:BYPASS
    //                        Change existing control type to:
    //                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
    //                        Work supported by ASHRAE research project 1254-RP
    //                      June 2007 R. Raustad, FSEC
    //                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
    //                      Jan 2012  B. Nigusse, FSEC
    //                        Added steam and hot water heating coils

    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and routines required to model desiccant dehumidifier
    // components in the EnergyPlus HVAC simulation

    // METHODOLOGY EMPLOYED:
    // The desiccant dehumidifier emcompasses not just the component but also its
    // control. The desiccant dehumidifier removes moisture from its air inlet to meet
    // the HumRatMax setpoint at its exit node. The HumRatMax is set by
    // an external setpoint manager or is a fixed user input.

    // REFERENCES: na

    // OTHER NOTES: This module is based substantially on the Humidifiers module.
    //              authored by Fred Buhl.
    //              Development of portions of this module was funded by the Gas Research Institute.
    //              (Please see copyright and disclaimer information at end of module)

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataLoopNode;
    using DataHVACGlobals::BlowThru;
    using DataHVACGlobals::Coil_HeatingElectric;
    using DataHVACGlobals::Coil_HeatingGasOrOtherFuel;
    using DataHVACGlobals::Coil_HeatingSteam;
    using DataHVACGlobals::Coil_HeatingWater;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::DrawThru;
    using DataHVACGlobals::SmallMassFlow;
    // Use statements for access to subroutines in other modules
    using namespace ScheduleManager;
    using namespace HeatingCoils;
    using namespace Fans;
    using namespace CurveManager;
    using namespace Psychrometrics;
    using FluidProperties::GetSatDensityRefrig;

    static std::string const fluidNameSteam("STEAM");

    void SimDesiccantDehumidifier(EnergyPlusData &state,
                                  std::string const &CompName,   // name of the dehumidifier unit
                                  bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                  int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       June 2007, R. Raustad, Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manage the simulation of an air dehumidifier

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DesicDehumNum;   // index of solid desiccant unit being simulated
        Real64 HumRatNeeded; // process air leaving humidity ratio set by controller [kg water/kg air]

        if (state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier) {
            GetDesiccantDehumidifierInput(state);
            state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier = false;
        }

        // Get the desiccant dehumidifier unit index
        if (CompIndex == 0) {
            DesicDehumNum = UtilityRoutines::FindItemInList(CompName, state.dataDesiccantDehumidifiers->DesicDehum);
            if (DesicDehumNum == 0) {
                ShowFatalError(state, "SimDesiccantDehumidifier: Unit not found=" + CompName);
            }
            CompIndex = DesicDehumNum;
        } else {
            DesicDehumNum = CompIndex;
            if (DesicDehumNum > state.dataDesiccantDehumidifiers->NumDesicDehums || DesicDehumNum < 1) {
                ShowFatalError(state,
                               format("SimDesiccantDehumidifier:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      DesicDehumNum,
                                      state.dataDesiccantDehumidifiers->NumDesicDehums,
                                      CompName));
            }
            if (CompName != state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).Name) {
                ShowFatalError(state,
                               format("SimDesiccantDehumidifier: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      DesicDehumNum,
                                      CompName,
                                      state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).Name));
            }
        }

        InitDesiccantDehumidifier(state, DesicDehumNum, FirstHVACIteration);

        ControlDesiccantDehumidifier(state, DesicDehumNum, HumRatNeeded, FirstHVACIteration);

        // call the correct dehumidifier calculation routine
        {
            auto const SELECT_CASE_var(state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).DehumTypeCode);

            if (SELECT_CASE_var == DesicDehumType::Solid) {

                CalcSolidDesiccantDehumidifier(state, DesicDehumNum, HumRatNeeded, FirstHVACIteration);

            } else if (SELECT_CASE_var == DesicDehumType::Generic) {

                CalcGenericDesiccantDehumidifier(state, DesicDehumNum, HumRatNeeded, FirstHVACIteration);

            } else {
                ShowFatalError(state, "Invalid type, Desiccant Dehumidifer=" + state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).DehumType);
            }
        }

        UpdateDesiccantDehumidifier(state, DesicDehumNum);

        ReportDesiccantDehumidifier(state, DesicDehumNum);
    }

    void GetDesiccantDehumidifierInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
        //                        Add new control type option:
        //                          NODE LEAVING HUMRAT SETPOINT:BYPASS
        //                        Change existing control type to:
        //                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
        //                        Work supported by ASHRAE research project 1254-RP
        //                      June 2007 R. Raustad, FSEC
        //                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for humidifiers and stores it in dehumidifier data structures.

        // METHODOLOGY EMPLOYED:
        // Uses InputProcessor "Get" routines to obtain data.

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using NodeInputManager::GetOnlySingleNode;
        auto &GetDXCoilOutletNode(DXCoils::GetCoilOutletNode);
        using DXCoils::GetCoilCondenserInletNode;
        using DXCoils::GetDXCoilBypassedFlowFrac;
        using DXCoils::GetDXCoilIndex;
        auto &GetDXCoilCapacity(DXCoils::GetCoilCapacity);
        using HeatRecovery::GetSecondaryInletNode;
        using HeatRecovery::GetSecondaryOutletNode;
        using HeatRecovery::GetSupplyInletNode;
        using HeatRecovery::GetSupplyOutletNode;
        auto &GetHeatingCoilInletNode(HeatingCoils::GetCoilInletNode);
        auto &GetHeatingCoilOutletNode(HeatingCoils::GetCoilOutletNode);
        auto &GetHeatReclaimSourceIndexNum(HeatingCoils::GetHeatReclaimSourceIndex);
        auto &GetHeatingCoilIndex(HeatingCoils::GetCoilIndex);
        auto &GetHeatingCoilControlNodeNum(HeatingCoils::GetCoilControlNodeNum);
        using WaterCoils::GetCoilMaxWaterFlowRate;
        using WaterCoils::GetCoilWaterInletNode;
        using WaterCoils::GetWaterCoilIndex;
        auto &GetWaterCoilInletNode(WaterCoils::GetCoilInletNode);
        auto &GetWaterCoilOutletNode(WaterCoils::GetCoilOutletNode);
        auto &GetSteamCoilAirInletNode(SteamCoils::GetCoilAirInletNode);
        using SteamCoils::GetCoilAirOutletNode;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetSteamCoilIndex;
        auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
        using OutAirNodeManager::CheckAndAddAirNodeNumber;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using SteamCoils::GetSteamCoilControlNodeNum;
        using SteamCoils::GetTypeOfCoil;
        using SteamCoils::SetSteamCoilData;
        using WaterCoils::SetWaterCoilData;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetDesiccantDehumidifierInput: "); // include trailing blank space
        static std::string const dehumidifierDesiccantNoFans("Dehumidifier:Desiccant:NoFans");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DesicDehumIndex;               // Loop index
        int DesicDehumNum;                 // Current desiccant dehumidifier number
        int NumAlphas;                     // Number of Alphas for each GetObjectItem call
        int NumNumbers;                    // Number of Numbers for each GetObjectItem call
        int IOStatus;                      // Used in GetObjectItem
        bool ErrorsFound(false);           // Set to true if errors in input, fatal at end of routine
        bool ErrorsFound2(false);          // Set to true if errors in input, fatal at end of routine
        bool ErrorsFoundGeneric(false);    // Set to true if errors in input, fatal at end of routine
        bool IsNotOK;                      // Flag to verify name
        bool OANodeError;                  // Flag for check on outside air node
        std::string RegenFanInlet;         // Desiccant system regeneration air fan inlet node
        std::string RegenFanOutlet;        // Desiccant system regeneration air fan outlet node
        std::string RegenCoilInlet;        // Desiccant system regeneration air heater inlet node
        std::string RegenCoilOutlet;       // Desiccant system regeneration air heater outlet node
        std::string ProcAirInlet;          // HX process air inlet node
        std::string ProcAirOutlet;         // HX process air outlet node
        std::string RegenAirInlet;         // HX regeneration air inlet node
        std::string RegenAirOutlet;        // HX regeneration air outlet node
        std::string CurrentModuleObject;   // for ease in getting objects
        int DesuperHeaterIndex;            // Index of desuperheater heating coil
        int RegenCoilControlNodeNum;       // Control node number of regen heating coil
        Real64 CoilBypassedFlowFrac;       // Bypass air fraction for multimode DX coils
        Array1D_string Alphas;             // Alpha input items for object
        Array1D_string cAlphaFields;       // Alpha field names
        Array1D_string cNumericFields;     // Numeric field names
        Array1D<Real64> Numbers;           // Numeric input items for object
        Array1D_bool lAlphaBlanks;         // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;       // Logical array, numeric field input BLANK = .TRUE.
        int RegenCoilAirInletNode;         // regen heating coil air inlet node number
        int RegenCoilAirOutletNode;        // regen heating coil air outlet node number
        bool errFlag;                      // local error flag
        std::string RegenCoilType;         // Regen heating coil type
        std::string RegenCoilName;         // Regen heating coil name
        int SteamIndex;                    // steam coil Index
        bool RegairHeatingCoilFlag(false); // local error flag

        auto &DesicDehum(state.dataDesiccantDehumidifiers->DesicDehum);
        auto &MaxNums(state.dataDesiccantDehumidifiers->MaxNums);
        auto &MaxAlphas(state.dataDesiccantDehumidifiers->MaxAlphas);
        auto &TotalArgs(state.dataDesiccantDehumidifiers->TotalArgs);
        auto &SteamDensity(state.dataDesiccantDehumidifiers->SteamDensity);

        state.dataDesiccantDehumidifiers->NumSolidDesicDehums =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, dehumidifierDesiccantNoFans);
        state.dataDesiccantDehumidifiers->NumGenericDesicDehums =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Dehumidifier:Desiccant:System");
        state.dataDesiccantDehumidifiers->NumDesicDehums =
            state.dataDesiccantDehumidifiers->NumSolidDesicDehums + state.dataDesiccantDehumidifiers->NumGenericDesicDehums;
        // allocate the data array
        state.dataDesiccantDehumidifiers->DesicDehum.allocate(state.dataDesiccantDehumidifiers->NumDesicDehums);
        state.dataDesiccantDehumidifiers->UniqueDesicDehumNames.reserve(state.dataDesiccantDehumidifiers->NumDesicDehums);
        state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier = false;

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, dehumidifierDesiccantNoFans, TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Dehumidifier:Desiccant:System", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        Numbers.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        // loop over solid desiccant dehumidifiers and load the input data
        CurrentModuleObject = dehumidifierDesiccantNoFans;
        for (DesicDehumIndex = 1; DesicDehumIndex <= state.dataDesiccantDehumidifiers->NumSolidDesicDehums; ++DesicDehumIndex) {
            RegenCoilAirInletNode = 0;
            RegenCoilAirOutletNode = 0;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     DesicDehumIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            DesicDehumNum = DesicDehumIndex;

            GlobalNames::VerifyUniqueInterObjectName(
                state, state.dataDesiccantDehumidifiers->UniqueDesicDehumNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            DesicDehum(DesicDehumNum).Name = Alphas(1);
            DesicDehum(DesicDehumNum).DehumType = CurrentModuleObject;
            DesicDehum(DesicDehumNum).DehumTypeCode = DesicDehumType::Solid;
            DesicDehum(DesicDehumNum).Sched = Alphas(2);
            if (lAlphaBlanks(2)) {
                DesicDehum(DesicDehumNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                DesicDehum(DesicDehumNum).SchedPtr = GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer
                if (DesicDehum(DesicDehumNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ": invalid " + cAlphaFields(2) + " entered =" + Alphas(2) +
                                        " for " + cAlphaFields(1) + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // For node connections, this object is both a parent and a non-parent, because the
            // Desiccant wheel is not called out as a separate component, its nodes must be connected
            // as ObjectIsNotParent.  But for the Regen fan, the nodes are connected as ObjectIsParent
            DesicDehum(DesicDehumNum).ProcAirInNode = GetOnlySingleNode(state,
                                                                        Alphas(3),
                                                                        ErrorsFound,
                                                                        CurrentModuleObject,
                                                                        Alphas(1),
                                                                        DataLoopNode::NodeFluidType::Air,
                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                        NodeInputManager::compFluidStream::Primary,
                                                                        ObjectIsNotParent);

            DesicDehum(DesicDehumNum).ProcAirOutNode = GetOnlySingleNode(state,
                                                                         Alphas(4),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                         NodeInputManager::compFluidStream::Primary,
                                                                         ObjectIsNotParent);

            DesicDehum(DesicDehumNum).RegenAirInNode = GetOnlySingleNode(state,
                                                                         Alphas(5),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Inlet,
                                                                         NodeInputManager::compFluidStream::Secondary,
                                                                         ObjectIsNotParent);

            DesicDehum(DesicDehumNum).RegenFanInNode = GetOnlySingleNode(state,
                                                                         Alphas(6),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Internal,
                                                                         NodeInputManager::compFluidStream::Secondary,
                                                                         ObjectIsParent);

            if (UtilityRoutines::SameString(Alphas(7), "LEAVING HUMRAT:BYPASS")) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                ShowContinueError(state, "Obsolete " + cAlphaFields(7) + " = " + Alphas(7));
                ShowContinueError(state, "setting to LeavingMaximumHumidityRatioSetpoint");
                DesicDehum(DesicDehumNum).ControlType = DesicDehumCtrlType::FixedHumratBypass;
            }
            if (UtilityRoutines::SameString(Alphas(7), "LeavingMaximumHumidityRatioSetpoint"))
                DesicDehum(DesicDehumNum).ControlType = DesicDehumCtrlType::FixedHumratBypass;
            if (UtilityRoutines::SameString(Alphas(7), "SystemNodeMaximumHumidityRatioSetpoint"))
                DesicDehum(DesicDehumNum).ControlType = DesicDehumCtrlType::NodeHumratBypass;
            if (DesicDehum(DesicDehumNum).ControlType == DesicDehumCtrlType::Unassigned) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                ShowContinueError(state, "Invalid " + cAlphaFields(7) + " = " + Alphas(7));
                ShowContinueError(state, "setting to LeavingMaximumHumidityRatioSetpoint");
                DesicDehum(DesicDehumNum).ControlType = DesicDehumCtrlType::FixedHumratBypass;
            }
            DesicDehum(DesicDehumNum).HumRatSet = Numbers(1);
            DesicDehum(DesicDehumNum).NomProcAirVolFlow = Numbers(2);
            DesicDehum(DesicDehumNum).NomProcAirVel = Numbers(3);

            DesicDehum(DesicDehumNum).RegenCoilType = Alphas(8);
            DesicDehum(DesicDehumNum).RegenCoilName = Alphas(9);
            RegenCoilType = Alphas(8);
            RegenCoilName = Alphas(9);

            if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Electric") ||
                UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Fuel")) {
                if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Electric"))
                    DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingElectric;
                if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Fuel"))
                    DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingGasOrOtherFuel;
                ValidateComponent(state,
                                  DesicDehum(DesicDehumNum).RegenCoilType,
                                  DesicDehum(DesicDehumNum).RegenCoilName,
                                  ErrorsFound2,
                                  CurrentModuleObject + '=' + Alphas(1));
                if (ErrorsFound2) ErrorsFound = true;
                GetHeatingCoilIndex(state, DesicDehum(DesicDehumNum).RegenCoilName, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorsFound2);
                if (ErrorsFound2) ErrorsFound = true;

            } else if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Water")) {
                DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingWater;
                ValidateComponent(state, RegenCoilType, RegenCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object
                    errFlag = false;
                    DesicDehum(DesicDehumNum).RegenCoilIndex = GetWaterCoilIndex(state, "COIL:HEATING:WATER", RegenCoilName, errFlag);
                    if (DesicDehum(DesicDehumNum).RegenCoilIndex == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " illegal " + cAlphaFields(9) + " = " + RegenCoilName);
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Hot water Inlet or control Node number
                    errFlag = false;
                    DesicDehum(DesicDehumNum).CoilControlNode = GetCoilWaterInletNode(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Regeneration Heating Coil hot water max volume flow rate
                    errFlag = false;
                    DesicDehum(DesicDehumNum).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Regeneration Heating Coil Inlet Node
                    errFlag = false;
                    RegenCoilAirInletNode = GetWaterCoilInletNode(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                    DesicDehum(DesicDehumNum).RegenCoilInletNode = RegenCoilAirInletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Regeneration Heating Coil Outlet Node
                    errFlag = false;
                    RegenCoilAirOutletNode = GetWaterCoilOutletNode(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                    DesicDehum(DesicDehumNum).RegenCoilOutletNode = RegenCoilAirOutletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Steam")) {
                DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingSteam;
                ValidateComponent(state, Alphas(8), RegenCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from the regeneration heating coil object

                    errFlag = false;
                    DesicDehum(DesicDehumNum).RegenCoilIndex = GetSteamCoilIndex(state, "COIL:HEATING:STEAM", RegenCoilName, errFlag);
                    if (DesicDehum(DesicDehumNum).RegenCoilIndex == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " illegal " + cAlphaFields(9) + " = " + RegenCoilName);
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the regeneration Heating Coil steam inlet node number
                    errFlag = false;
                    DesicDehum(DesicDehumNum).CoilControlNode = GetCoilSteamInletNode(state, "Coil:Heating:Steam", RegenCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the regeneration heating Coil steam max volume flow rate
                    DesicDehum(DesicDehumNum).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate(state, DesicDehum(DesicDehumNum).RegenCoilIndex, errFlag);
                    if (DesicDehum(DesicDehumNum).MaxCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(
                            state, fluidNameSteam, state.dataDesiccantDehumidifiers->TempSteamIn, 1.0, SteamIndex, dehumidifierDesiccantNoFans);
                        DesicDehum(DesicDehumNum).MaxCoilFluidFlow *= SteamDensity;
                    }

                    // Get the regeneration heating Coil Inlet Node
                    errFlag = false;
                    RegenCoilAirInletNode = GetSteamCoilAirInletNode(state, DesicDehum(DesicDehumNum).RegenCoilIndex, RegenCoilName, errFlag);
                    DesicDehum(DesicDehumNum).RegenCoilInletNode = RegenCoilAirInletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the regeneration heating Coil Outlet Node
                    errFlag = false;
                    RegenCoilAirOutletNode = GetCoilAirOutletNode(state, DesicDehum(DesicDehumNum).RegenCoilIndex, RegenCoilName, errFlag);
                    DesicDehum(DesicDehumNum).RegenCoilOutletNode = RegenCoilAirOutletNode;
                    if (errFlag) {
                        ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError(state, "Illegal " + cAlphaFields(8) + " = " + DesicDehum(DesicDehumNum).RegenCoilType);
                ErrorsFound = true;
            }

            DesicDehum(DesicDehumNum).NomRotorPower = Numbers(4);
            DesicDehum(DesicDehumNum).RegenFanType = Alphas(10);
            DesicDehum(DesicDehumNum).RegenFanName = Alphas(11);

            TestCompSet(state, DesicDehum(DesicDehumNum).DehumType, DesicDehum(DesicDehumNum).Name, Alphas(3), Alphas(4), "Process Air Nodes");

            // Set up component set for regen coil
            SetUpCompSets(state, DesicDehum(DesicDehumNum).DehumType, DesicDehum(DesicDehumNum).Name, Alphas(8), Alphas(9), "UNDEFINED", "UNDEFINED");

            // Set up component set for regen fan
            SetUpCompSets(state, DesicDehum(DesicDehumNum).DehumType, DesicDehum(DesicDehumNum).Name, Alphas(10), Alphas(11), Alphas(6), "UNDEFINED");

            if ((!UtilityRoutines::SameString(Alphas(12), "Default")) && (UtilityRoutines::SameString(Alphas(12), "UserCurves"))) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + ": Invalid" + cAlphaFields(12) + " = " + Alphas(12));
                ShowContinueError(state, "resetting to Default");
                DesicDehum(DesicDehumNum).PerformanceModel_Num = PerformanceModel::Default;
            }

            if (UtilityRoutines::SameString(Alphas(12), "UserCurves")) {
                DesicDehum(DesicDehumNum).PerformanceModel_Num = PerformanceModel::UserCurves;
                DesicDehum(DesicDehumNum).ProcDryBulbCurvefTW = GetCurveIndex(state, Alphas(13));
                if (DesicDehum(DesicDehumNum).ProcDryBulbCurvefTW == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(13) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).ProcDryBulbCurvefV = GetCurveIndex(state, Alphas(14));
                if (DesicDehum(DesicDehumNum).ProcDryBulbCurvefV == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(14) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).ProcHumRatCurvefTW = GetCurveIndex(state, Alphas(15));
                if (DesicDehum(DesicDehumNum).ProcHumRatCurvefTW == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(15) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).ProcHumRatCurvefV = GetCurveIndex(state, Alphas(16));
                if (DesicDehum(DesicDehumNum).ProcHumRatCurvefV == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(16) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).RegenEnergyCurvefTW = GetCurveIndex(state, Alphas(17));
                if (DesicDehum(DesicDehumNum).RegenEnergyCurvefTW == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(17) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).RegenEnergyCurvefV = GetCurveIndex(state, Alphas(18));
                if (DesicDehum(DesicDehumNum).RegenEnergyCurvefV == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(18) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).RegenVelCurvefTW = GetCurveIndex(state, Alphas(19));
                if (DesicDehum(DesicDehumNum).RegenVelCurvefTW == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(19) + " not found.");
                    ErrorsFound2 = true;
                }
                DesicDehum(DesicDehumNum).RegenVelCurvefV = GetCurveIndex(state, Alphas(20));
                if (DesicDehum(DesicDehumNum).RegenVelCurvefV == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Curve object=" + Alphas(20) + " not found.");
                    ErrorsFound2 = true;
                }
                if (ErrorsFound2) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state, "Errors found in getting performance curves.");
                    ErrorsFound = true;
                }
                DesicDehum(DesicDehumNum).NomRegenTemp = Numbers(5);
                // Validate regen fan type, for user defined curves, can be constant or variable volume
                if ((UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "FAN:CONSTANTVOLUME")) ||
                    (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "FAN:VARIABLEVOLUME") ||
                     UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "FAN:SYSTEMMODEL"))) {
                    ValidateComponent(state,
                                      DesicDehum(DesicDehumNum).RegenFanType,
                                      DesicDehum(DesicDehumNum).RegenFanName,
                                      ErrorsFound2,
                                      CurrentModuleObject + " = " + Alphas(1));
                    if (ErrorsFound2) ErrorsFound = true;
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state, "Illegal " + cAlphaFields(10) + " = " + DesicDehum(DesicDehumNum).RegenFanType);
                    ErrorsFound = true;
                }
            } else {
                // If DEFAULT performance model, set operating limits curves.  Unit is off outside this range
                DesicDehum(DesicDehumNum).PerformanceModel_Num = PerformanceModel::Default;
                for (auto &e : DesicDehum) {
                    e.MinProcAirInTemp = 1.67;       //  35 F
                    e.MaxProcAirInTemp = 48.89;      // 120 F
                    e.MinProcAirInHumRat = 0.002857; //  20 gr/lb
                    e.MaxProcAirInHumRat = 0.02857;  // 200 gr/lb
                }
                //  If DEFAULT performance model, warn if curve names and nominal regen temp have values
                if ((!lAlphaBlanks(13)) || (!lAlphaBlanks(14)) || (!lAlphaBlanks(15)) || (!lAlphaBlanks(16)) || (!lAlphaBlanks(17)) ||
                    (!lAlphaBlanks(18)) || (!lAlphaBlanks(19)) || (!lAlphaBlanks(20))) {
                    ShowWarningError(state, CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state, "DEFAULT performance selected, curve names and nominal regen temp will be ignored.");
                }
                if (DesicDehum(DesicDehumNum).NomProcAirVel > 4.064) {
                    ShowWarningError(state, CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state,
                                      format("{} > 4.064 m/s.; Value in input={:.3R}", cNumericFields(3), DesicDehum(DesicDehumNum).NomProcAirVel));
                    ShowContinueError(state, "DEFAULT performance curves not valid outside 2.032 to 4.064 m/s (400 to 800 fpm).");
                }
                if (DesicDehum(DesicDehumNum).NomProcAirVel < 2.032) {
                    ShowWarningError(state, CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state,
                                      format("{} < 2.032 m/s.; Value in input={:.3R}", cNumericFields(3), DesicDehum(DesicDehumNum).NomProcAirVel));
                    ShowContinueError(state, "DEFAULT performance curves not valid outside 2.032 to 4.064 m/s (400 to 800 fpm).");
                }
                // Validate regen fan type, for default curves, can only variable volume
                if (DesicDehum(DesicDehumNum).RegenFanType == "FAN:VARIABLEVOLUME" || DesicDehum(DesicDehumNum).RegenFanType == "FAN:SYSTEMMODEL") {
                    ValidateComponent(state,
                                      DesicDehum(DesicDehumNum).RegenFanType,
                                      DesicDehum(DesicDehumNum).RegenFanName,
                                      ErrorsFound2,
                                      CurrentModuleObject + " = " + Alphas(1));
                    if (ErrorsFound2) ErrorsFound = true;
                } else {
                    ShowSevereError(state, CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(state, "Illegal " + cAlphaFields(10) + " = " + DesicDehum(DesicDehumNum).RegenFanType);
                    ShowContinueError(state, "For DEFAULT performance model, the regen fan type must be Fan:VariableVolume");
                    ErrorsFound = true;
                }
            }
            // process regen fan
            ErrorsFound2 = false;
            if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "Fan:SystemModel")) {
                DesicDehum(DesicDehumNum).regenFanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, DesicDehum(DesicDehumNum).RegenFanName)); // call constructor
                DesicDehum(DesicDehumNum).RegenFanIndex = HVACFan::getFanObjectVectorIndex(state, DesicDehum(DesicDehumNum).RegenFanName);
                DesicDehum(DesicDehumNum).RegenFanInNode = state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->inletNodeNum;
                DesicDehum(DesicDehumNum).RegenFanOutNode = state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->outletNodeNum;

            } else {
                GetFanType(state,
                           DesicDehum(DesicDehumNum).RegenFanName,
                           DesicDehum(DesicDehumNum).regenFanType_Num,
                           errFlag,
                           CurrentModuleObject,
                           DesicDehum(DesicDehumNum).Name);
                DesicDehum(DesicDehumNum).RegenFanInNode =
                    GetFanInletNode(state, DesicDehum(DesicDehumNum).RegenFanType, DesicDehum(DesicDehumNum).RegenFanName, ErrorsFound2);
                if (ErrorsFound2) {
                    ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ErrorsFoundGeneric = true;
                }

                ErrorsFound2 = false;
                DesicDehum(DesicDehumNum).RegenFanOutNode =
                    GetFanOutletNode(state, DesicDehum(DesicDehumNum).RegenFanType, DesicDehum(DesicDehumNum).RegenFanName, ErrorsFound2);
                GetFanIndex(state,
                            DesicDehum(DesicDehumNum).RegenFanName,
                            DesicDehum(DesicDehumNum).RegenFanIndex,
                            ErrorsFound2,
                            DesicDehum(DesicDehumNum).RegenFanType);
                if (ErrorsFound2) {
                    ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ErrorsFoundGeneric = true;
                }
            }
        }

        for (DesicDehumIndex = 1; DesicDehumIndex <= state.dataDesiccantDehumidifiers->NumGenericDesicDehums; ++DesicDehumIndex) {
            RegenCoilAirInletNode = 0;
            RegenCoilAirOutletNode = 0;

            CurrentModuleObject = "Dehumidifier:Desiccant:System";

            DesicDehumNum = DesicDehumIndex + state.dataDesiccantDehumidifiers->NumSolidDesicDehums;
            DesicDehum(DesicDehumNum).DehumType = CurrentModuleObject;
            DesicDehum(DesicDehumNum).DehumTypeCode = DesicDehumType::Generic;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     DesicDehum(DesicDehumNum).DehumType,
                                                                     DesicDehumIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            GlobalNames::VerifyUniqueInterObjectName(
                state, state.dataDesiccantDehumidifiers->UniqueDesicDehumNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFoundGeneric);
            DesicDehum(DesicDehumNum).Name = Alphas(1);

            ErrorsFound2 = false;
            ValidateComponent(state,
                              DesicDehum(DesicDehumNum).DehumType,
                              DesicDehum(DesicDehumNum).Name,
                              ErrorsFound2,
                              DesicDehum(DesicDehumNum).DehumType + " = \"" + DesicDehum(DesicDehumNum).Name + "\"");
            if (ErrorsFound2) {
                ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\" is not unique");
                ErrorsFoundGeneric = true;
            }

            DesicDehum(DesicDehumNum).Sched = Alphas(2);
            if (lAlphaBlanks(2)) {
                DesicDehum(DesicDehumNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                DesicDehum(DesicDehumNum).SchedPtr = GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer
                if (DesicDehum(DesicDehumNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ": invalid " + cAlphaFields(2) + " entered =" + Alphas(2) +
                                        " for " + cAlphaFields(1) + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }

            DesicDehum(DesicDehumNum).HXType = Alphas(3);
            DesicDehum(DesicDehumNum).HXName = Alphas(4);

            if (!UtilityRoutines::SameString(DesicDehum(DesicDehumNum).HXType, "HeatExchanger:Desiccant:BalancedFlow")) {
                ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + " = \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowContinueError(state, "Invalid " + cAlphaFields(3) + " = " + DesicDehum(DesicDehumNum).HXType);
                ErrorsFoundGeneric = true;
            } else {
                DesicDehum(DesicDehumNum).HXTypeNum = BalancedHX;
            }

            ErrorsFound2 = false;
            ValidateComponent(state,
                              DesicDehum(DesicDehumNum).HXType,
                              DesicDehum(DesicDehumNum).HXName,
                              ErrorsFound2,
                              DesicDehum(DesicDehumNum).DehumType + " = \"" + DesicDehum(DesicDehumNum).Name + "\"");
            if (ErrorsFound2) ErrorsFoundGeneric = true;

            ErrorsFound2 = false;
            DesicDehum(DesicDehumNum).HXProcInNode = GetSecondaryInletNode(state, DesicDehum(DesicDehumNum).HXName, ErrorsFound2);
            if (ErrorsFound2) {
                ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ErrorsFoundGeneric = true;
            }

            ProcAirInlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXProcInNode);

            DesicDehum(DesicDehumNum).ProcAirInNode = GetOnlySingleNode(state,
                                                                        ProcAirInlet,
                                                                        ErrorsFound,
                                                                        DesicDehum(DesicDehumNum).DehumType,
                                                                        DesicDehum(DesicDehumNum).Name,
                                                                        DataLoopNode::NodeFluidType::Air,
                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                        NodeInputManager::compFluidStream::Primary,
                                                                        ObjectIsParent);

            ErrorsFound2 = false;
            DesicDehum(DesicDehumNum).HXProcOutNode = GetSecondaryOutletNode(state, DesicDehum(DesicDehumNum).HXName, ErrorsFound2);
            if (ErrorsFound2) {
                ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ErrorsFoundGeneric = true;
            }

            ProcAirOutlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXProcOutNode);

            DesicDehum(DesicDehumNum).ProcAirOutNode = GetOnlySingleNode(state,
                                                                         ProcAirOutlet,
                                                                         ErrorsFound,
                                                                         DesicDehum(DesicDehumNum).DehumType,
                                                                         DesicDehum(DesicDehumNum).Name,
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                         NodeInputManager::compFluidStream::Primary,
                                                                         ObjectIsParent);

            TestCompSet(state, DesicDehum(DesicDehumNum).DehumType, DesicDehum(DesicDehumNum).Name, ProcAirInlet, ProcAirOutlet, "Process Air Nodes");

            ErrorsFound2 = false;
            DesicDehum(DesicDehumNum).HXRegenInNode = GetSupplyInletNode(state, DesicDehum(DesicDehumNum).HXName, ErrorsFound2);
            if (ErrorsFound2) {
                ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ErrorsFoundGeneric = true;
            }

            ErrorsFound2 = false;
            DesicDehum(DesicDehumNum).HXRegenOutNode = GetSupplyOutletNode(state, DesicDehum(DesicDehumNum).HXName, ErrorsFound2);
            if (ErrorsFound2) {
                ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ErrorsFoundGeneric = true;
            }

            DesicDehum(DesicDehumNum).ControlNodeNum = GetOnlySingleNode(state,
                                                                         Alphas(5),
                                                                         ErrorsFound,
                                                                         DesicDehum(DesicDehumNum).DehumType,
                                                                         DesicDehum(DesicDehumNum).Name,
                                                                         DataLoopNode::NodeFluidType::Air,
                                                                         DataLoopNode::NodeConnectionType::Sensor,
                                                                         NodeInputManager::compFluidStream::Primary,
                                                                         ObjectIsNotParent);

            if (DesicDehum(DesicDehumNum).ControlNodeNum == 0) {
                ShowContinueError(state, DesicDehum(DesicDehumNum).DehumType + " = \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowSevereError(state, cAlphaFields(5) + " must be specified.");
                ErrorsFoundGeneric = true;
            }

            DesicDehum(DesicDehumNum).RegenFanType = Alphas(6);
            DesicDehum(DesicDehumNum).RegenFanName = Alphas(7);

            if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "Fan:OnOff") ||
                UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "Fan:ConstantVolume") ||
                UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "Fan:SystemModel")) {
                ErrorsFound2 = false;
                ValidateComponent(state,
                                  DesicDehum(DesicDehumNum).RegenFanType,
                                  DesicDehum(DesicDehumNum).RegenFanName,
                                  ErrorsFound2,
                                  DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                if (ErrorsFound2) ErrorsFoundGeneric = true;
            } else {
                ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowContinueError(state, "Illegal " + cAlphaFields(6) + " = " + DesicDehum(DesicDehumNum).RegenFanType);
                ErrorsFoundGeneric = true;
            }

            if (UtilityRoutines::SameString(Alphas(8), "DrawThrough")) {
                DesicDehum(DesicDehumNum).RegenFanPlacement = DrawThru;
            } else if (UtilityRoutines::SameString(Alphas(8), "BlowThrough")) {
                DesicDehum(DesicDehumNum).RegenFanPlacement = BlowThru;
            } else {
                ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowContinueError(state, "Illegal " + cAlphaFields(8) + " = " + Alphas(8));
                ShowContinueError(state, "...resetting to DEFAULT of DRAW THROUGH");
                DesicDehum(DesicDehumNum).RegenFanPlacement = DrawThru;
            }

            ErrorsFound2 = false;
            if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenFanType, "Fan:SystemModel")) {
                DesicDehum(DesicDehumNum).regenFanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, DesicDehum(DesicDehumNum).RegenFanName)); // call constructor
                DesicDehum(DesicDehumNum).RegenFanIndex = HVACFan::getFanObjectVectorIndex(state, DesicDehum(DesicDehumNum).RegenFanName);
                DesicDehum(DesicDehumNum).RegenFanInNode = state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->inletNodeNum;
                DesicDehum(DesicDehumNum).RegenFanOutNode = state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->outletNodeNum;
            } else {
                GetFanType(state,
                           DesicDehum(DesicDehumNum).RegenFanName,
                           DesicDehum(DesicDehumNum).regenFanType_Num,
                           errFlag,
                           CurrentModuleObject,
                           DesicDehum(DesicDehumNum).Name);
                DesicDehum(DesicDehumNum).RegenFanInNode =
                    GetFanInletNode(state, DesicDehum(DesicDehumNum).RegenFanType, DesicDehum(DesicDehumNum).RegenFanName, ErrorsFound2);
                if (ErrorsFound2) {
                    ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ErrorsFoundGeneric = true;
                }

                ErrorsFound2 = false;
                DesicDehum(DesicDehumNum).RegenFanOutNode =
                    GetFanOutletNode(state, DesicDehum(DesicDehumNum).RegenFanType, DesicDehum(DesicDehumNum).RegenFanName, ErrorsFound2);
                GetFanIndex(state,
                            DesicDehum(DesicDehumNum).RegenFanName,
                            DesicDehum(DesicDehumNum).RegenFanIndex,
                            ErrorsFound2,
                            DesicDehum(DesicDehumNum).RegenFanType);
                if (ErrorsFound2) {
                    ShowContinueError(state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ErrorsFoundGeneric = true;
                }
            }

            DesicDehum(DesicDehumNum).RegenCoilType = Alphas(9);
            DesicDehum(DesicDehumNum).RegenCoilName = Alphas(10);
            RegenCoilType = Alphas(9);
            RegenCoilName = Alphas(10);
            DesicDehum(DesicDehumNum).RegenSetPointTemp = Numbers(1);

            if (!lAlphaBlanks(10)) {
                if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Electric") ||
                    UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Fuel")) {
                    if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Electric"))
                        DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingElectric;
                    if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Fuel"))
                        DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingGasOrOtherFuel;
                    ErrorsFound2 = false;
                    ValidateComponent(state,
                                      RegenCoilType,
                                      RegenCoilName,
                                      ErrorsFound2,
                                      DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    if (ErrorsFound2) ErrorsFoundGeneric = true;

                    if (DesicDehum(DesicDehumNum).RegenSetPointTemp <= 0.0) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(state, cNumericFields(1) + " must be greater than 0.");
                        ErrorsFoundGeneric = true;
                    }

                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).RegenCoilInletNode = GetHeatingCoilInletNode(state, RegenCoilType, RegenCoilName, ErrorsFound2);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).RegenCoilOutletNode = GetHeatingCoilOutletNode(state, RegenCoilType, RegenCoilName, ErrorsFound2);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                    ErrorsFound2 = false;
                    GetHeatingCoilIndex(state, RegenCoilName, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorsFound2);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                    ErrorsFound2 = false;
                    RegenCoilControlNodeNum = GetHeatingCoilControlNodeNum(state, RegenCoilType, RegenCoilName, ErrorsFound2);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                    if (RegenCoilControlNodeNum > 0) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(
                            state,
                            format("{} is specified as {:.3R} C in this object.", cNumericFields(1), DesicDehum(DesicDehumNum).RegenSetPointTemp));
                        ShowContinueError(state, " Do not specify a coil temperature setpoint node name in the regeneration air heater object.");
                        ShowContinueError(state, "..." + cAlphaFields(9) + " = " + DesicDehum(DesicDehumNum).RegenCoilType);
                        ShowContinueError(state, "..." + cAlphaFields(10) + " = " + DesicDehum(DesicDehumNum).RegenCoilName);
                        ShowContinueError(state,
                                          "...heating coil temperature setpoint node = " + state.dataLoopNodes->NodeID(RegenCoilControlNodeNum));
                        ShowContinueError(state, "...leave the heating coil temperature setpoint node name blank in the regen heater object.");
                        ErrorsFoundGeneric = true;
                    }

                    RegairHeatingCoilFlag = true;
                    SetHeatingCoilData(state, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorsFound2, RegairHeatingCoilFlag, DesicDehumNum);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                } else if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Water")) {
                    DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingWater;
                    ValidateComponent(state, RegenCoilType, RegenCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    } else { // mine data from heating coil object
                        errFlag = false;
                        DesicDehum(DesicDehumNum).RegenCoilIndex = GetWaterCoilIndex(state, "COIL:HEATING:WATER", RegenCoilName, errFlag);
                        if (DesicDehum(DesicDehumNum).RegenCoilIndex == 0) {
                            ShowSevereError(state, CurrentModuleObject + " illegal " + cAlphaFields(9) + " = " + RegenCoilName);
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        if (DesicDehum(DesicDehumNum).RegenSetPointTemp <= 0.0) {
                            ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                            ShowContinueError(state, cNumericFields(1) + " must be greater than 0.");
                            ErrorsFoundGeneric = true;
                        }

                        // Get the Heating Coil Hot water Inlet or control Node number
                        errFlag = false;
                        DesicDehum(DesicDehumNum).CoilControlNode = GetCoilWaterInletNode(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the Regeneration Heating Coil hot water max volume flow rate
                        errFlag = false;
                        DesicDehum(DesicDehumNum).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the Regeneration Heating Coil Inlet Node
                        errFlag = false;
                        RegenCoilAirInletNode = GetWaterCoilInletNode(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                        DesicDehum(DesicDehumNum).RegenCoilInletNode = RegenCoilAirInletNode;
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the Regeneration Heating Coil Outlet Node
                        errFlag = false;
                        RegenCoilAirOutletNode = GetWaterCoilOutletNode(state, "Coil:Heating:Water", RegenCoilName, errFlag);
                        DesicDehum(DesicDehumNum).RegenCoilOutletNode = RegenCoilAirOutletNode;
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        RegairHeatingCoilFlag = true;
                        SetWaterCoilData(state, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorsFound2, RegairHeatingCoilFlag, DesicDehumNum);
                        if (ErrorsFound2) {
                            ShowContinueError(state,
                                              "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                            ErrorsFoundGeneric = true;
                        }
                    }
                } else if (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).RegenCoilType, "Coil:Heating:Steam")) {
                    DesicDehum(DesicDehumNum).RegenCoilType_Num = Coil_HeatingSteam;
                    ValidateComponent(state, RegenCoilType, RegenCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    } else { // mine data from the regeneration heating coil object
                        if (DesicDehum(DesicDehumNum).RegenSetPointTemp <= 0.0) {
                            ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                            ShowContinueError(state, cNumericFields(1) + " must be greater than 0.");
                            ErrorsFoundGeneric = true;
                        }

                        errFlag = false;
                        DesicDehum(DesicDehumNum).RegenCoilIndex = GetSteamCoilIndex(state, "COIL:HEATING:STEAM", RegenCoilName, errFlag);
                        if (DesicDehum(DesicDehumNum).RegenCoilIndex == 0) {
                            ShowSevereError(state, CurrentModuleObject + " illegal " + cAlphaFields(9) + " = " + RegenCoilName);
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the regeneration Heating Coil steam inlet node number
                        errFlag = false;
                        DesicDehum(DesicDehumNum).CoilControlNode = GetCoilSteamInletNode(state, "Coil:Heating:Steam", RegenCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the regeneration heating Coil steam max volume flow rate
                        DesicDehum(DesicDehumNum).MaxCoilFluidFlow =
                            GetCoilMaxSteamFlowRate(state, DesicDehum(DesicDehumNum).RegenCoilIndex, errFlag);
                        if (DesicDehum(DesicDehumNum).MaxCoilFluidFlow > 0.0) {
                            SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            SteamDensity = GetSatDensityRefrig(
                                state, fluidNameSteam, state.dataDesiccantDehumidifiers->TempSteamIn, 1.0, SteamIndex, dehumidifierDesiccantNoFans);
                            DesicDehum(DesicDehumNum).MaxCoilFluidFlow *= SteamDensity;
                        }

                        // Get the regeneration heating Coil Inlet Node
                        errFlag = false;
                        RegenCoilAirInletNode = GetSteamCoilAirInletNode(state, DesicDehum(DesicDehumNum).RegenCoilIndex, RegenCoilName, errFlag);
                        DesicDehum(DesicDehumNum).RegenCoilInletNode = RegenCoilAirInletNode;
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the regeneration heating Coil Outlet Node
                        errFlag = false;
                        RegenCoilAirOutletNode = GetCoilAirOutletNode(state, DesicDehum(DesicDehumNum).RegenCoilIndex, RegenCoilName, errFlag);
                        DesicDehum(DesicDehumNum).RegenCoilOutletNode = RegenCoilAirOutletNode;
                        if (errFlag) {
                            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + DesicDehum(DesicDehumNum).Name);
                            ErrorsFound = true;
                        }
                    }

                    ErrorsFound2 = false;
                    RegenCoilControlNodeNum = GetSteamCoilControlNodeNum(state, RegenCoilType, RegenCoilName, ErrorsFound2);

                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                    if (RegenCoilControlNodeNum > 0) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(
                            state,
                            format("{} is specified as {:.3R} C in this object.", cNumericFields(1), DesicDehum(DesicDehumNum).RegenSetPointTemp));
                        ShowContinueError(state, " Do not specify a coil temperature setpoint node name in the regeneration air heater object.");
                        ShowContinueError(state, "..." + cAlphaFields(9) + " = " + DesicDehum(DesicDehumNum).RegenCoilType);
                        ShowContinueError(state, "..." + cAlphaFields(10) + " = " + DesicDehum(DesicDehumNum).RegenCoilName);
                        ShowContinueError(state,
                                          "...heating coil temperature setpoint node = " + state.dataLoopNodes->NodeID(RegenCoilControlNodeNum));
                        ShowContinueError(state, "...leave the heating coil temperature setpoint node name blank in the regen heater object.");
                        ErrorsFoundGeneric = true;
                    }

                    RegairHeatingCoilFlag = true;
                    SetSteamCoilData(state, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorsFound2, RegairHeatingCoilFlag, DesicDehumNum);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                } else {
                    ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ShowContinueError(state, "Illegal " + cAlphaFields(9) + " = " + DesicDehum(DesicDehumNum).RegenCoilType);
                    ErrorsFoundGeneric = true;
                }
            }

            RegenAirInlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXRegenInNode);

            RegenAirOutlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXRegenOutNode);

            RegenFanInlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenFanInNode);

            RegenFanOutlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenFanOutNode);

            if (!lAlphaBlanks(10)) {
                RegenCoilInlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenCoilInletNode);

                RegenCoilOutlet = state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenCoilOutletNode);
            }

            SetUpCompSets(state,
                          DesicDehum(DesicDehumNum).DehumType,
                          DesicDehum(DesicDehumNum).Name,
                          DesicDehum(DesicDehumNum).HXType,
                          DesicDehum(DesicDehumNum).HXName,
                          ProcAirInlet,
                          ProcAirOutlet);

            SetUpCompSets(state,
                          DesicDehum(DesicDehumNum).DehumType,
                          DesicDehum(DesicDehumNum).Name,
                          DesicDehum(DesicDehumNum).RegenFanType,
                          DesicDehum(DesicDehumNum).RegenFanName,
                          RegenFanInlet,
                          RegenFanOutlet);

            if (!lAlphaBlanks(10)) {
                SetUpCompSets(state,
                              DesicDehum(DesicDehumNum).DehumType,
                              DesicDehum(DesicDehumNum).Name,
                              DesicDehum(DesicDehumNum).RegenCoilType,
                              DesicDehum(DesicDehumNum).RegenCoilName,
                              RegenCoilInlet,
                              RegenCoilOutlet);
            }

            if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                DesicDehum(DesicDehumNum).RegenAirInNode = GetOnlySingleNode(state,
                                                                             RegenFanInlet,
                                                                             ErrorsFound,
                                                                             DesicDehum(DesicDehumNum).DehumType,
                                                                             DesicDehum(DesicDehumNum).Name,
                                                                             DataLoopNode::NodeFluidType::Air,
                                                                             DataLoopNode::NodeConnectionType::Inlet,
                                                                             NodeInputManager::compFluidStream::Primary,
                                                                             ObjectIsParent);
                DesicDehum(DesicDehumNum).RegenAirOutNode = GetOnlySingleNode(state,
                                                                              RegenAirOutlet,
                                                                              ErrorsFound,
                                                                              DesicDehum(DesicDehumNum).DehumType,
                                                                              DesicDehum(DesicDehumNum).Name,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              NodeInputManager::compFluidStream::Primary,
                                                                              ObjectIsParent);
                if (!lAlphaBlanks(10)) {
                    if (DesicDehum(DesicDehumNum).RegenFanOutNode != DesicDehum(DesicDehumNum).RegenCoilInletNode) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(state,
                                          "Regen fan outlet node name and regen heater inlet node name do not match for fan placement: Blow Through");
                        ShowContinueError(state,
                                          "...Regen fan outlet node   = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenFanOutNode));
                        ShowContinueError(
                            state, "...Regen heater inlet node = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenCoilInletNode));
                        ErrorsFoundGeneric = true;
                    }
                    if (DesicDehum(DesicDehumNum).RegenCoilOutletNode != DesicDehum(DesicDehumNum).HXRegenInNode) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(state,
                                          "Regen heater outlet node name and desiccant heat exchanger regen inlet node name do not match for fan "
                                          "placement: Blow Through");
                        ShowContinueError(
                            state, "...Regen heater outlet node = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenCoilOutletNode));
                        ShowContinueError(state,
                                          "...HX regen inlet node      = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXRegenInNode));
                        ErrorsFoundGeneric = true;
                    }
                } else {
                    if (DesicDehum(DesicDehumNum).RegenFanOutNode != DesicDehum(DesicDehumNum).HXRegenInNode) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(
                            state,
                            "Regen fan outlet node name and desiccant heat exchanger inlet node name do not match for fan placement: Blow Through");
                        ShowContinueError(state,
                                          "...Regen fan outlet node   = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenFanOutNode));
                        ShowContinueError(state,
                                          "...Desiccant HX inlet node = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXRegenInNode));
                        ErrorsFoundGeneric = true;
                    }
                }
            } else { // ELSE for IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
                DesicDehum(DesicDehumNum).RegenAirOutNode = GetOnlySingleNode(state,
                                                                              RegenFanOutlet,
                                                                              ErrorsFound,
                                                                              DesicDehum(DesicDehumNum).DehumType,
                                                                              DesicDehum(DesicDehumNum).Name,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              NodeInputManager::compFluidStream::Primary,
                                                                              ObjectIsParent);
                if (!lAlphaBlanks(10)) {
                    DesicDehum(DesicDehumNum).RegenAirInNode = GetOnlySingleNode(state,
                                                                                 RegenCoilInlet,
                                                                                 ErrorsFound,
                                                                                 DesicDehum(DesicDehumNum).DehumType,
                                                                                 DesicDehum(DesicDehumNum).Name,
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::NodeConnectionType::Inlet,
                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                 ObjectIsParent);
                    if (DesicDehum(DesicDehumNum).RegenCoilOutletNode != DesicDehum(DesicDehumNum).HXRegenInNode) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(state,
                                          "Regen heater outlet node name and desiccant heat exchanger regen inlet node name do not match for fan "
                                          "placement: Draw Through");
                        ShowContinueError(
                            state, "...Regen heater outlet node = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenCoilOutletNode));
                        ShowContinueError(state,
                                          "...HX regen inlet node      = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXRegenInNode));
                        ErrorsFoundGeneric = true;
                    }
                } else {
                    DesicDehum(DesicDehumNum).RegenAirInNode = GetOnlySingleNode(state,
                                                                                 RegenAirInlet,
                                                                                 ErrorsFound,
                                                                                 DesicDehum(DesicDehumNum).DehumType,
                                                                                 DesicDehum(DesicDehumNum).Name,
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::NodeConnectionType::Inlet,
                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                 ObjectIsParent);
                }
                if (DesicDehum(DesicDehumNum).RegenFanInNode != DesicDehum(DesicDehumNum).HXRegenOutNode) {
                    ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ShowContinueError(
                        state,
                        "Regen fan inlet node name and desiccant heat exchanger regen outlet node name do not match for fan placement: Draw Through");
                    ShowContinueError(state, "...Regen fan inlet node = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenFanInNode));
                    ShowContinueError(state, "...HX regen outlet node = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).HXRegenOutNode));
                    ErrorsFoundGeneric = true;
                }
            }

            DesicDehum(DesicDehumNum).CoolingCoilType = Alphas(11);
            DesicDehum(DesicDehumNum).CoolingCoilName = Alphas(12);

            if (!lAlphaBlanks(12)) {
                if ((UtilityRoutines::SameString(DesicDehum(DesicDehumNum).CoolingCoilType, "COIL:COOLING:DX:SINGLESPEED")) ||
                    (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).CoolingCoilType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE")) ||
                    (UtilityRoutines::SameString(DesicDehum(DesicDehumNum).CoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED"))) {
                    ErrorsFound2 = false;
                    ValidateComponent(state,
                                      DesicDehum(DesicDehumNum).CoolingCoilType,
                                      DesicDehum(DesicDehumNum).CoolingCoilName,
                                      ErrorsFound2,
                                      DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    if (ErrorsFound2) ErrorsFoundGeneric = true;

                    if ((UtilityRoutines::SameString(DesicDehum(DesicDehumNum).CoolingCoilType, "COIL:COOLING:DX:SINGLESPEED"))) {
                        DesicDehum(DesicDehumNum).coolingCoil_TypeNum = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
                    } else if ((UtilityRoutines::SameString(DesicDehum(DesicDehumNum).CoolingCoilType,
                                                            "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE"))) {
                        DesicDehum(DesicDehumNum).coolingCoil_TypeNum = DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl;
                    } else if ((UtilityRoutines::SameString(DesicDehum(DesicDehumNum).CoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED"))) {
                        DesicDehum(DesicDehumNum).coolingCoil_TypeNum = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                    }

                } else {
                    ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name);
                    ShowContinueError(state, "Illegal " + cAlphaFields(11) + " = " + DesicDehum(DesicDehumNum).CoolingCoilType);
                    ErrorsFoundGeneric = true;
                }

                if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                    (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).CoolingCoilOutletNode = GetDXCoilOutletNode(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                    DesicDehum(DesicDehumNum).CompanionCoilCapacity =
                        GetDXCoilCapacity(state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                    if (ErrorsFound2)
                        ShowContinueError(
                            state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).CoolingCoilName + "\"");

                    ErrorsFound2 = false;
                    GetDXCoilIndex(state,
                                   DesicDehum(DesicDehumNum).CoolingCoilName,
                                   DesicDehum(DesicDehumNum).DXCoilIndex,
                                   ErrorsFound2,
                                   DesicDehum(DesicDehumNum).CoolingCoilType,
                                   ObjexxFCL::Optional_bool_const());
                    if (ErrorsFound2)
                        ShowContinueError(
                            state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).CoolingCoilName + "\"");
                } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).CoolingCoilOutletNode = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).CompanionCoilCapacity = VariableSpeedCoils::GetCoilCapacityVariableSpeed(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                    if (ErrorsFound2)
                        ShowContinueError(
                            state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).CoolingCoilName + "\"");
                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).DXCoilIndex = VariableSpeedCoils::GetCoilIndexVariableSpeed(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                    if (ErrorsFound2)
                        ShowContinueError(
                            state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).CoolingCoilName + "\"");
                }

            } //  (DesicDehum(DesicDehumNum)%CoolingCoilName /= Blank)THEN

            if (UtilityRoutines::SameString(Alphas(13), "Yes")) {
                DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide = Selection::Yes;
            } else if (lAlphaBlanks(13)) {
                DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide = Selection::No;
            } else if (UtilityRoutines::SameString(Alphas(13), "No")) {
                DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide = Selection::No;
            } else {
                ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowContinueError(state, "Invalid choice for " + cAlphaFields(13) + " = " + Alphas(13));
                ShowContinueError(state, "...resetting to the default value of No");
                DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide = Selection::No;
            }

            if (UtilityRoutines::SameString(Alphas(14), "Yes")) {
                DesicDehum(DesicDehumNum).Preheat = Selection::Yes;
            } else if (UtilityRoutines::SameString(Alphas(14), "No")) {
                DesicDehum(DesicDehumNum).Preheat = Selection::No;
            } else if (lAlphaBlanks(14)) {
                DesicDehum(DesicDehumNum).Preheat = Selection::No;
            } else {
                ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowContinueError(state, "Invalid choice for " + cAlphaFields(14) + " = " + Alphas(14));
                ShowContinueError(state, "...resetting to the default value of NO");
                DesicDehum(DesicDehumNum).Preheat = Selection::No;
            }

            if (DesicDehum(DesicDehumNum).DXCoilIndex > 0) {

                if (DesicDehum(DesicDehumNum).Preheat == Selection::Yes) { // Companion coil waste heat used for regeneration of desiccant
                    ErrorsFound2 = false;
                    DesuperHeaterIndex = GetHeatReclaimSourceIndexNum(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                    if (ErrorsFound2) {
                        ShowContinueError(state,
                                          "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ErrorsFoundGeneric = true;
                    }

                    if (DesuperHeaterIndex > 0) {
                        ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name);
                        ShowContinueError(state,
                                          "A Coil:Heating:Desuperheater object should not be used when condenser waste heat is reclaimed for "
                                          "desiccant regeneration.");
                        ShowContinueError(state,
                                          "A Coil:Heating:Desuperheater object was found using waste heat from the " +
                                              DesicDehum(DesicDehumNum).CoolingCoilType + " \"" + DesicDehum(DesicDehumNum).CoolingCoilName +
                                              "\" object.");
                        //          ErrorsFoundGeneric = .TRUE.
                    }
                }
                if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                    (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).CondenserInletNode = GetCoilCondenserInletNode(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    ErrorsFound2 = false;
                    DesicDehum(DesicDehumNum).CondenserInletNode =
                        VariableSpeedCoils::GetVSCoilCondenserInletNode(state, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                }
                if (DesicDehum(DesicDehumNum).CondenserInletNode == 0 && DesicDehum(DesicDehumNum).Preheat == Selection::Yes) {
                    DesicDehum(DesicDehumNum).CondenserInletNode =
                        GetOnlySingleNode(state,
                                          DesicDehum(DesicDehumNum).CoolingCoilName + " Condenser Inlet Node",
                                          ErrorsFound,
                                          DesicDehum(DesicDehumNum).DehumType,
                                          DesicDehum(DesicDehumNum).Name,
                                          DataLoopNode::NodeFluidType::Air,
                                          DataLoopNode::NodeConnectionType::OutsideAirReference,
                                          NodeInputManager::compFluidStream::Secondary,
                                          ObjectIsNotParent);
                    CheckAndAddAirNodeNumber(state, DesicDehum(DesicDehumNum).CondenserInletNode, OANodeError);
                    if (!OANodeError) {
                        ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(
                            state,
                            "The " + cAlphaFields(14) +
                                " input is specified as Yes and a condenser air inlet node name was not specified for the companion cooling coil.");
                        ShowContinueError(state,
                                          "Adding condenser inlet air node for " + DesicDehum(DesicDehumNum).CoolingCoilType + " \"" +
                                              DesicDehum(DesicDehumNum).CoolingCoilName + "\"");
                        ShowContinueError(
                            state, "...condenser inlet air node name = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).CondenserInletNode));
                        ShowContinueError(state, "...this node name will be specified as an outdoor air node.");
                    }
                } else if (DesicDehum(DesicDehumNum).Preheat == Selection::Yes) {
                    if (!CheckOutAirNodeNumber(state, DesicDehum(DesicDehumNum).CondenserInletNode)) {
                        ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                        ShowContinueError(state,
                                          "The regeneration air inlet node must be specified as an outdoor air node when " + cAlphaFields(14) +
                                              " is specified as Yes.");
                        ErrorsFoundGeneric = true;
                    }
                }
            }

            if (CheckOutAirNodeNumber(state, DesicDehum(DesicDehumNum).RegenAirInNode)) {
                DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode = true;
            }

            if (DesicDehum(DesicDehumNum).DXCoilIndex == 0 && DesicDehum(DesicDehumNum).Preheat == Selection::Yes) {
                ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name);
                ShowContinueError(state,
                                  "A valid " + cAlphaFields(12) + " must be used when condenser waste heat is reclaimed for desiccant regeneration.");
                ShowContinueError(state, "... " + cAlphaFields(11) + " = " + DesicDehum(DesicDehumNum).CoolingCoilType);
                ShowContinueError(state, "... " + cAlphaFields(12) + " = " + DesicDehum(DesicDehumNum).CoolingCoilName);
                ErrorsFoundGeneric = true;
            }

            if (DesicDehum(DesicDehumNum).DXCoilIndex > 0 && DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide == Selection::Yes) {
                if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                    (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                    ErrorsFound2 = false;
                    CoilBypassedFlowFrac = GetDXCoilBypassedFlowFrac(
                        state, DesicDehum(DesicDehumNum).CoolingCoilType, DesicDehum(DesicDehumNum).CoolingCoilName, ErrorsFound2);
                } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    ErrorsFound2 = false;
                    CoilBypassedFlowFrac = 0.0; // bypass flow fraction not in VS coil model
                }
                if (ErrorsFound2)
                    ShowContinueError(
                        state, "...occurs in " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).CoolingCoilName + "\"");
                if (CoilBypassedFlowFrac > 0.0) {
                    ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name);
                    ShowContinueError(state,
                                      "A DX coil bypassed air flow fraction greater than 0 may not be used when the input for " + cAlphaFields(13) +
                                          " is specified as Yes.");
                    ShowContinueError(
                        state,
                        "A DX coil with a bypassed air flow fraction greater than 0 may be upstream of the process inlet however the input for " +
                            cAlphaFields(13) + " must be specified as No.");
                    ShowContinueError(state, "... " + cAlphaFields(11) + " = " + DesicDehum(DesicDehumNum).CoolingCoilType);
                    ShowContinueError(state, "... " + cAlphaFields(12) + " = " + DesicDehum(DesicDehumNum).CoolingCoilName);
                    ErrorsFoundGeneric = true;
                }
            } else if (DesicDehum(DesicDehumNum).DXCoilIndex == 0 && DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide == Selection::Yes) {
                ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                ShowContinueError(state, "A valid companion coil must be specified when " + cAlphaFields(13) + " is specified as Yes.");
                ErrorsFoundGeneric = true;
            }

            if (!DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode && DesicDehum(DesicDehumNum).Preheat == Selection::Yes) {
                ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name);
                ShowContinueError(state,
                                  "The desiccant dehumidifier regeneration air inlet must be specified as an outdoor air node when " +
                                      cAlphaFields(14) + " is specified as Yes.");
                ShowContinueError(state,
                                  "... desiccant dehumidifier regeneration air inlet node name = " +
                                      state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).RegenAirInNode));
                ErrorsFoundGeneric = true;
            }

            if (DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide == Selection::Yes) {
                if (DesicDehum(DesicDehumNum).ProcAirInNode != DesicDehum(DesicDehumNum).CoolingCoilOutletNode) {
                    ShowSevereError(state, "For " + DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ShowContinueError(state, "Node names are inconsistent in companion cooling coil and desiccant heat exchanger objects.");
                    ShowContinueError(state,
                                      "For companion cooling coil = " + DesicDehum(DesicDehumNum).CoolingCoilType + " \"" +
                                          DesicDehum(DesicDehumNum).CoolingCoilName + "\"");
                    ShowContinueError(state,
                                      "The outlet node name in cooling coil = " +
                                          state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).CoolingCoilOutletNode));
                    ShowContinueError(state,
                                      "For desiccant heat exchanger = " + DesicDehum(DesicDehumNum).HXType + " \"" +
                                          DesicDehum(DesicDehumNum).HXName + "\"");
                    ShowContinueError(state,
                                      "The process air inlet node name = " + state.dataLoopNodes->NodeID(DesicDehum(DesicDehumNum).ProcAirInNode));
                    ShowFatalError(state, "...previous error causes program termination.");
                }
            }

            // Exhaust Fan input
            DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate = Numbers(2);
            DesicDehum(DesicDehumNum).ExhaustFanMaxPower = Numbers(3);
            DesicDehum(DesicDehumNum).ExhaustFanCurveIndex = GetCurveIndex(state, Alphas(15));

            if (DesicDehum(DesicDehumNum).ExhaustFanCurveIndex > 0) {
                ErrorsFoundGeneric |= CurveManager::CheckCurveDims(state,
                                                                   DesicDehum(DesicDehumNum).ExhaustFanCurveIndex, // Curve index
                                                                   {1},                                            // Valid dimensions
                                                                   RoutineName,                                    // Routine name
                                                                   CurrentModuleObject,                            // Object Type
                                                                   DesicDehum(DesicDehumNum).Name,                 // Object Name
                                                                   cAlphaFields(15));                              // Field Name
            }

            if (DesicDehum(DesicDehumNum).Preheat == Selection::Yes) {
                ErrorsFound2 = false;
                if (DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate <= 0) {
                    ErrorsFound2 = true;
                }
                if (DesicDehum(DesicDehumNum).ExhaustFanMaxPower <= 0) {
                    ErrorsFound2 = true;
                }
                if (ErrorsFound2) {
                    ShowSevereError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ShowContinueError(
                        state, cNumericFields(2) + " and " + cNumericFields(3) + " must be defined if " + cAlphaFields(14) + " field is \"Yes\".");
                }
            } else if (DesicDehum(DesicDehumNum).Preheat == Selection::No) {
                if (DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate > 0.0) {
                    ShowWarningError(state, DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name + "\"");
                    ShowContinueError(state, cNumericFields(2) + " should be 0 if " + cAlphaFields(14) + " field is \"No\".");
                    ShowContinueError(state, "..." + cNumericFields(2) + " will not be used and is reset to 0.");
                    DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate = 0.0;
                }
            }
        }

        // SET UP OUTPUTS
        for (DesicDehumNum = 1; DesicDehumNum <= state.dataDesiccantDehumidifiers->NumSolidDesicDehums; ++DesicDehumNum) {
            // Setup Report variables for the Desiccant Dehumidifiers
            SetupOutputVariable(state,
                                "Dehumidifier Removed Water Mass",
                                OutputProcessor::Unit::kg,
                                DesicDehum(DesicDehumNum).WaterRemove,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Removed Water Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DesicDehum(DesicDehumNum).WaterRemoveRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Part Load Ratio",
                                OutputProcessor::Unit::None,
                                DesicDehum(DesicDehumNum).PartLoad,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Electricity Rate",
                                OutputProcessor::Unit::W,
                                DesicDehum(DesicDehumNum).ElecUseRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Electricity Energy",
                                OutputProcessor::Unit::J,
                                DesicDehum(DesicDehumNum).ElecUseEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                DesicDehum(DesicDehumNum).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Dehumidifier Regeneration Specific Energy",
                                OutputProcessor::Unit::J_kgWater,
                                DesicDehum(DesicDehumNum).SpecRegenEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Regeneration Rate",
                                OutputProcessor::Unit::W,
                                DesicDehum(DesicDehumNum).QRegen,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Regeneration Energy",
                                OutputProcessor::Unit::J,
                                DesicDehum(DesicDehumNum).RegenEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Regeneration Air Speed",
                                OutputProcessor::Unit::m_s,
                                DesicDehum(DesicDehumNum).RegenAirVel,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Regeneration Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DesicDehum(DesicDehumNum).RegenAirInMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Process Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DesicDehum(DesicDehumNum).ProcAirInMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
        }

        for (DesicDehumNum = 1; DesicDehumNum <= state.dataDesiccantDehumidifiers->NumGenericDesicDehums; ++DesicDehumNum) {
            // Setup Report variables for the Desiccant Dehumidifiers
            SetupOutputVariable(state,
                                "Dehumidifier Removed Water Mass",
                                OutputProcessor::Unit::kg,
                                DesicDehum(DesicDehumNum).WaterRemove,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Removed Water Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DesicDehum(DesicDehumNum).WaterRemoveRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            SetupOutputVariable(state,
                                "Dehumidifier Part Load Ratio",
                                OutputProcessor::Unit::None,
                                DesicDehum(DesicDehumNum).PartLoad,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                DesicDehum(DesicDehumNum).Name);
            if (DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate > 0) {
                SetupOutputVariable(state,
                                    "Dehumidifier Exhaust Fan Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    DesicDehum(DesicDehumNum).ExhaustFanPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    DesicDehum(DesicDehumNum).Name);
                SetupOutputVariable(state,
                                    "Dehumidifier Exhaust Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    DesicDehum(DesicDehumNum).ExhaustFanElecConsumption,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    DesicDehum(DesicDehumNum).Name,
                                    _,
                                    "Electricity",
                                    "Cooling",
                                    _,
                                    "System");
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in getting Dehumidifier:Desiccant:NoFans input");
        } else if (ErrorsFoundGeneric) {
            ShowFatalError(state, "Errors found in getting DESICCANT DEHUMIDIFIER input");
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();
    }

    void InitDesiccantDehumidifier(EnergyPlusData &state,
                                   int const DesicDehumNum,      // number of the current dehumidifier being simulated
                                   bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
        //                        Add setpoint validation for new control type option:
        //                          NODE LEAVING HUMRAT SETPOINT:BYPASS
        //                        Work supported by ASHRAE research project 1254-RP
        //                      June 2007 R. Raustad, FSEC
        //                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
        //                      May 2009, B. Griffith, NREL. added EMS node setpoint checks
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the dehumidifier Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &DoSetPointTest = state.dataHVACGlobal->DoSetPointTest;
        auto &SetPointErrorFlag = state.dataHVACGlobal->SetPointErrorFlag;
        using EMSManager::CheckIfNodeSetPointManagedByEMS;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using SteamCoils::SimulateSteamCoilComponents;
        auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataSizing::AutoSize;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSatDensityRefrig;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using WaterCoils::GetCoilMaxWaterFlowRate;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitDesiccantDehumidifier");
        static std::string const initCBVAV("InitCBVAV");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ProcInNode;            // inlet node number
        int RegenInNode;           // inlet node number
        int ControlNode;           // control node number
        bool ErrorsFound(false);   // Set to true if errors in input, fatal at end of routine
        int SteamIndex;            // steam coil index
        Real64 FluidDensity;       // steam or water coil fluid density
        Real64 CoilMaxVolFlowRate; // water or steam max volumetric water flow rate
        Real64 QCoilActual;        // actual CBVAV steam heating coil load met (W)
        bool ErrorFlag;            // local error flag returned from data mining

        auto &DesicDehum(state.dataDesiccantDehumidifiers->DesicDehum);
        auto &MyEnvrnFlag(state.dataDesiccantDehumidifiers->MyEnvrnFlag);
        auto &MyPlantScanFlag(state.dataDesiccantDehumidifiers->MyPlantScanFlag);

        if (state.dataDesiccantDehumidifiers->InitDesiccantDehumidifierOneTimeFlag) {

            // initialize the environment and sizing flags
            MyEnvrnFlag.allocate(state.dataDesiccantDehumidifiers->NumDesicDehums);
            MyPlantScanFlag.allocate(state.dataDesiccantDehumidifiers->NumDesicDehums);
            MyEnvrnFlag = true;

            state.dataDesiccantDehumidifiers->InitDesiccantDehumidifierOneTimeFlag = false;
            MyPlantScanFlag = true;
        }

        if (MyPlantScanFlag(DesicDehumNum) && allocated(state.dataPlnt->PlantLoop)) {
            if ((DesicDehum(DesicDehumNum).RegenCoilType_Num == Coil_HeatingWater) ||
                (DesicDehum(DesicDehumNum).RegenCoilType_Num == Coil_HeatingSteam)) {
                if (DesicDehum(DesicDehumNum).RegenCoilType_Num == Coil_HeatingWater) {
                    ErrorFlag = false;
                    ScanPlantLoopsForObject(state,
                                            DesicDehum(DesicDehumNum).RegenCoilName,
                                            TypeOf_CoilWaterSimpleHeating,
                                            DesicDehum(DesicDehumNum).LoopNum,
                                            DesicDehum(DesicDehumNum).LoopSide,
                                            DesicDehum(DesicDehumNum).BranchNum,
                                            DesicDehum(DesicDehumNum).CompNum,
                                            ErrorFlag,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _);
                    if (ErrorFlag) {
                        ShowFatalError(state, "InitDesiccantDehumidifier: Program terminated for previous conditions.");
                    }

                    ErrorFlag = false;
                    DesicDehum(DesicDehumNum).MaxCoilFluidFlow =
                        GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", DesicDehum(DesicDehumNum).RegenCoilName, ErrorFlag);
                    if (DesicDehum(DesicDehumNum).MaxCoilFluidFlow > 0.0) {
                        FluidDensity = GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(DesicDehum(DesicDehumNum).LoopNum).FluidName,
                                                        DataGlobalConstants::HWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(DesicDehum(DesicDehumNum).LoopNum).FluidIndex,
                                                        initCBVAV);
                        DesicDehum(DesicDehumNum).MaxCoilFluidFlow *= FluidDensity;
                    }

                } else if (DesicDehum(DesicDehumNum).RegenCoilType_Num == Coil_HeatingSteam) {

                    ErrorFlag = false;
                    ScanPlantLoopsForObject(state,
                                            DesicDehum(DesicDehumNum).RegenCoilName,
                                            TypeOf_CoilSteamAirHeating,
                                            DesicDehum(DesicDehumNum).LoopNum,
                                            DesicDehum(DesicDehumNum).LoopSide,
                                            DesicDehum(DesicDehumNum).BranchNum,
                                            DesicDehum(DesicDehumNum).CompNum,
                                            ErrorFlag,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _);

                    if (ErrorFlag) {
                        ShowFatalError(state, "InitDesiccantDehumidifier: Program terminated for previous conditions.");
                    }
                    ErrorFlag = false;
                    DesicDehum(DesicDehumNum).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate(state, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorFlag);

                    if (DesicDehum(DesicDehumNum).MaxCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        FluidDensity =
                            GetSatDensityRefrig(state, fluidNameSteam, state.dataDesiccantDehumidifiers->TempSteamIn, 1.0, SteamIndex, RoutineName);
                        DesicDehum(DesicDehumNum).MaxCoilFluidFlow *= FluidDensity;
                    }
                }

                // fill outlet node for regenartion hot water or steam heating coil
                DesicDehum(DesicDehumNum).CoilOutletNode = state.dataPlnt->PlantLoop(DesicDehum(DesicDehumNum).LoopNum)
                                                               .LoopSide(DesicDehum(DesicDehumNum).LoopSide)
                                                               .Branch(DesicDehum(DesicDehumNum).BranchNum)
                                                               .Comp(DesicDehum(DesicDehumNum).CompNum)
                                                               .NodeNumOut;
                MyPlantScanFlag(DesicDehumNum) = false;

            } else { // DesicDehum is not connected to plant
                MyPlantScanFlag(DesicDehumNum) = false;
            }
        } else if (MyPlantScanFlag(DesicDehumNum) && !state.dataGlobal->AnyPlantInModel) {
            MyPlantScanFlag(DesicDehumNum) = false;
        }

        {
            auto const SELECT_CASE_var((DesicDehum(DesicDehumNum).DehumTypeCode));

            if (SELECT_CASE_var == DesicDehumType::Solid) {

                if (!state.dataGlobal->SysSizingCalc && state.dataDesiccantDehumidifiers->MySetPointCheckFlag && DoSetPointTest) {
                    if (DesicDehum(DesicDehumNum).ControlType == DesicDehumCtrlType::NodeHumratBypass) {
                        ControlNode = DesicDehum(DesicDehumNum).ProcAirOutNode;
                        if (ControlNode > 0) {
                            if (state.dataLoopNodes->Node(ControlNode).HumRatMax == SensedNodeFlagValue) {
                                if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                    ShowSevereError(state, "Missing humidity ratio setpoint (HumRatMax) for ");
                                    ShowContinueError(state, "Dehumidifier:Desiccant:NoFans: " + DesicDehum(DesicDehumNum).Name);
                                    ShowContinueError(state, "Node Referenced=" + state.dataLoopNodes->NodeID(ControlNode));
                                    ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at the process air outlet node.");
                                    SetPointErrorFlag = true;
                                } else {
                                    CheckIfNodeSetPointManagedByEMS(
                                        state, ControlNode, EMSManager::SPControlType::iHumidityRatioMaxSetPoint, SetPointErrorFlag);
                                    if (SetPointErrorFlag) {
                                        ShowSevereError(state, "Missing humidity ratio setpoint (HumRatMax) for ");
                                        ShowContinueError(state, "Dehumidifier:Desiccant:NoFans: " + DesicDehum(DesicDehumNum).Name);
                                        ShowContinueError(state, "Node Referenced=" + state.dataLoopNodes->NodeID(ControlNode));
                                        ShowContinueError(state, "use a Setpoint Manager to establish a setpoint at the process air outlet node.");
                                        ShowContinueError(state, "Or use EMS Actuator to establish a setpoint at the process air outlet node.");
                                    }
                                }
                            }
                        }
                    }
                    state.dataDesiccantDehumidifiers->MySetPointCheckFlag = false;
                }
                // always do these initializations every iteration
                ProcInNode = DesicDehum(DesicDehumNum).ProcAirInNode;
                DesicDehum(DesicDehumNum).ProcAirInTemp = state.dataLoopNodes->Node(ProcInNode).Temp;
                DesicDehum(DesicDehumNum).ProcAirInHumRat = state.dataLoopNodes->Node(ProcInNode).HumRat;
                DesicDehum(DesicDehumNum).ProcAirInEnthalpy = state.dataLoopNodes->Node(ProcInNode).Enthalpy;
                DesicDehum(DesicDehumNum).ProcAirInMassFlowRate = state.dataLoopNodes->Node(ProcInNode).MassFlowRate;

                //  Determine heating coil inlet conditions by calling it with zero load
                //  Not sure if this is really a good way to do this, should revisit for next release.
                CalcNonDXHeatingCoils(state, DesicDehumNum, FirstHVACIteration, 0.0);

                RegenInNode = DesicDehum(DesicDehumNum).RegenAirInNode;
                DesicDehum(DesicDehumNum).RegenAirInTemp = state.dataLoopNodes->Node(RegenInNode).Temp;
                DesicDehum(DesicDehumNum).RegenAirInHumRat = state.dataLoopNodes->Node(RegenInNode).HumRat;
                DesicDehum(DesicDehumNum).RegenAirInEnthalpy = state.dataLoopNodes->Node(RegenInNode).Enthalpy;

                DesicDehum(DesicDehumNum).WaterRemove = 0.0;
                DesicDehum(DesicDehumNum).ElecUseEnergy = 0.0;
                DesicDehum(DesicDehumNum).ElecUseRate = 0.0;

            } else if (SELECT_CASE_var == DesicDehumType::Generic) {

                //      Do the Begin Environment initializations
                if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(DesicDehumNum)) {
                    // Change the Volume Flow Rates to Mass Flow Rates
                    DesicDehum(DesicDehumNum).ExhaustFanMaxMassFlowRate =
                        DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate * state.dataEnvrn->StdRhoAir;

                    //   set fluid-side hardware limits
                    if (DesicDehum(DesicDehumNum).CoilControlNode > 0) {
                        //    If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
                        if (DesicDehum(DesicDehumNum).MaxCoilFluidFlow == AutoSize) {
                            if (DesicDehum(DesicDehumNum).RegenCoilType_Num == Coil_HeatingWater) {
                                SimulateWaterCoilComponents(
                                    state, DesicDehum(DesicDehumNum).RegenCoilName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenCoilIndex);
                                ErrorFlag = false;
                                CoilMaxVolFlowRate =
                                    GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", DesicDehum(DesicDehumNum).RegenCoilName, ErrorFlag);
                                if (ErrorFlag) {
                                    ErrorsFound = true;
                                }
                                if (CoilMaxVolFlowRate != AutoSize) {
                                    FluidDensity = GetDensityGlycol(state,
                                                                    state.dataPlnt->PlantLoop(DesicDehum(DesicDehumNum).LoopNum).FluidName,
                                                                    DataGlobalConstants::HWInitConvTemp,
                                                                    state.dataPlnt->PlantLoop(DesicDehum(DesicDehumNum).LoopNum).FluidIndex,
                                                                    RoutineName);
                                    DesicDehum(DesicDehumNum).MaxCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity;
                                }
                            }
                            if (DesicDehum(DesicDehumNum).RegenCoilType_Num == Coil_HeatingSteam) {
                                SimulateSteamCoilComponents(state,
                                                            DesicDehum(DesicDehumNum).RegenCoilName,
                                                            FirstHVACIteration,
                                                            DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                            1.0,
                                                            QCoilActual); // simulate any load > 0 to get max capacity of steam coil
                                ErrorFlag = false;
                                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(state, DesicDehum(DesicDehumNum).RegenCoilIndex, ErrorFlag);
                                if (ErrorFlag) {
                                    ErrorsFound = true;
                                }
                                if (CoilMaxVolFlowRate != AutoSize) {
                                    SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                                    FluidDensity = GetSatDensityRefrig(
                                        state, fluidNameSteam, state.dataDesiccantDehumidifiers->TempSteamIn, 1.0, SteamIndex, RoutineName);
                                    DesicDehum(DesicDehumNum).MaxCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity;
                                }
                            }
                        }
                        InitComponentNodes(state,
                                           0.0,
                                           DesicDehum(DesicDehumNum).MaxCoilFluidFlow,
                                           DesicDehum(DesicDehumNum).CoilControlNode,
                                           DesicDehum(DesicDehumNum).CoilOutletNode,
                                           DesicDehum(DesicDehumNum).LoopNum,
                                           DesicDehum(DesicDehumNum).LoopSide,
                                           DesicDehum(DesicDehumNum).BranchNum,
                                           DesicDehum(DesicDehumNum).CompNum);
                    }

                    MyEnvrnFlag(DesicDehumNum) = false;
                }

                if (!state.dataGlobal->SysSizingCalc && state.dataDesiccantDehumidifiers->MySetPointCheckFlag && DoSetPointTest) {
                    ControlNode = DesicDehum(DesicDehumNum).ControlNodeNum;
                    if (ControlNode > 0) {
                        if (state.dataLoopNodes->Node(ControlNode).HumRatMax == SensedNodeFlagValue) {
                            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                ShowSevereError(state, "Missing maximum humidity ratio setpoint (MaxHumRat) for ");
                                ShowContinueError(state, DesicDehum(DesicDehumNum).DehumType + ": " + DesicDehum(DesicDehumNum).Name);
                                ShowContinueError(state, "Node Referenced=" + state.dataLoopNodes->NodeID(ControlNode));
                                ShowContinueError(state,
                                                  "use a Setpoint Manager to establish a \"MaxHumRat\" setpoint at the process air control node.");
                                SetPointErrorFlag = true;
                            } else {
                                CheckIfNodeSetPointManagedByEMS(
                                    state, ControlNode, EMSManager::SPControlType::iHumidityRatioMaxSetPoint, SetPointErrorFlag);
                                if (SetPointErrorFlag) {
                                    ShowSevereError(state, "Missing maximum humidity ratio setpoint (MaxHumRat) for ");
                                    ShowContinueError(state, DesicDehum(DesicDehumNum).DehumType + ": " + DesicDehum(DesicDehumNum).Name);
                                    ShowContinueError(state, "Node Referenced=" + state.dataLoopNodes->NodeID(ControlNode));
                                    ShowContinueError(
                                        state, "use a Setpoint Manager to establish a \"MaxHumRat\" setpoint at the process air control node.");
                                    ShowContinueError(state, "Or use EMS Actuator to establish a setpoint at the process air outlet node.");
                                }
                            }
                        }
                    }
                    state.dataDesiccantDehumidifiers->MySetPointCheckFlag = false;
                }
                RegenInNode = DesicDehum(DesicDehumNum).RegenAirInNode;
                DesicDehum(DesicDehumNum).RegenAirInTemp = state.dataLoopNodes->Node(RegenInNode).Temp;
                DesicDehum(DesicDehumNum).RegenAirInMassFlowRate = state.dataLoopNodes->Node(RegenInNode).MassFlowRate;

                DesicDehum(DesicDehumNum).ExhaustFanPower = 0.0;
                DesicDehum(DesicDehumNum).WaterRemoveRate = 0.0;
            }
        }
    }

    void ControlDesiccantDehumidifier(EnergyPlusData &state,
                                      int const DesicDehumNum, // number of the current dehumidifier being simulated
                                      Real64 &HumRatNeeded,    // process air leaving humidity ratio set by controller [kg water/kg air]
                                      [[maybe_unused]] bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep !unused1208
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
        //                        Add new control type option:
        //                          NODE LEAVING HUMRAT SETPOINT:BYPASS
        //                        Change existing control type to:
        //                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
        //                        Work supported by ASHRAE research project 1254-RP
        //                      June 2007 R. Raustad, FSEC
        //                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets the output required from the dehumidifier

        // METHODOLOGY EMPLOYED:
        // Uses a maximum humidity ratio setpoint to calculate required process
        // leaving humidity ratio

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
        bool UnitOn;                 // unit on flag
        Real64 ProcAirMassFlowRate;  // process air mass flow rate [kg/s]
        Real64 RegenAirMassFlowRate; // regen air mass flow rate [kg/s]

        auto &DesicDehum(state.dataDesiccantDehumidifiers->DesicDehum);

        ProcAirMassFlowRate = 0.0;
        RegenAirMassFlowRate = 0.0;
        UnitOn = true;

        {
            auto const SELECT_CASE_var((DesicDehum(DesicDehumNum).DehumTypeCode));

            if (SELECT_CASE_var == DesicDehumType::Solid) {

                if (DesicDehum(DesicDehumNum).HumRatSet <= 0.0) UnitOn = false;
                ProcAirMassFlowRate = DesicDehum(DesicDehumNum).ProcAirInMassFlowRate;
                if (ProcAirMassFlowRate <= SmallMassFlow) UnitOn = false;

                if (GetCurrentScheduleValue(state, DesicDehum(DesicDehumNum).SchedPtr) <= 0.0) UnitOn = false;

                // If incoming conditions are outside valid range for curve fits, then shut unit off, do not issue warnings

                if (UnitOn) {
                    if ((DesicDehum(DesicDehumNum).ProcAirInTemp < DesicDehum(DesicDehumNum).MinProcAirInTemp) ||
                        (DesicDehum(DesicDehumNum).ProcAirInTemp > DesicDehum(DesicDehumNum).MaxProcAirInTemp)) {
                        UnitOn = false;
                    }
                    if ((DesicDehum(DesicDehumNum).ProcAirInHumRat < DesicDehum(DesicDehumNum).MinProcAirInHumRat) ||
                        (DesicDehum(DesicDehumNum).ProcAirInHumRat > DesicDehum(DesicDehumNum).MaxProcAirInHumRat)) {
                        UnitOn = false;
                    }
                }

                if (UnitOn) {

                    // perform the correct dehumidifier control strategy
                    {
                        auto const SELECT_CASE_var1(DesicDehum(DesicDehumNum).ControlType);

                        if (SELECT_CASE_var1 == DesicDehumCtrlType::FixedHumratBypass) {

                            HumRatNeeded = DesicDehum(DesicDehumNum).HumRatSet;
                            if (HumRatNeeded <= 0.0) {
                                ShowSevereError(state, "Dehumidifier:Desiccant:NoFans: " + DesicDehum(DesicDehumNum).Name);
                                ShowContinueError(state, format("Invalid Leaving Max Humidity Ratio Setpoint={:.8T}", HumRatNeeded));
                                ShowFatalError(state, "must be > 0.0");
                            }

                        } else if (SELECT_CASE_var1 == DesicDehumCtrlType::NodeHumratBypass) {

                            HumRatNeeded = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRatMax;

                        } else {

                            ShowFatalError(state, "Invalid control type in desiccant dehumidifier = " + DesicDehum(DesicDehumNum).Name);
                        }
                    }

                    // Setpoint of zero indicates no load from setpoint manager max hum
                    if ((HumRatNeeded == 0.0) || (DesicDehum(DesicDehumNum).ProcAirInHumRat <= HumRatNeeded)) {
                        UnitOn = false;
                        HumRatNeeded = DesicDehum(DesicDehumNum).ProcAirInHumRat;
                    }
                } else {
                    HumRatNeeded = DesicDehum(DesicDehumNum).ProcAirInHumRat;
                }

            } else if (SELECT_CASE_var == DesicDehumType::Generic) {

                ProcAirMassFlowRate = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).MassFlowRate;
                if (ProcAirMassFlowRate <= SmallMassFlow) UnitOn = false;

                if (GetCurrentScheduleValue(state, DesicDehum(DesicDehumNum).SchedPtr) <= 0.0) UnitOn = false;

                if (UnitOn) {
                    if (DesicDehum(DesicDehumNum).ControlNodeNum == DesicDehum(DesicDehumNum).ProcAirOutNode) {
                        HumRatNeeded = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ControlNodeNum).HumRatMax;
                    } else {
                        if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ControlNodeNum).HumRatMax > 0.0) {
                            HumRatNeeded = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ControlNodeNum).HumRatMax -
                                           (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ControlNodeNum).HumRat -
                                            state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRat);
                        } else {
                            HumRatNeeded = 0.0;
                        }
                    }

                    // Setpoint of zero indicates no load from setpoint manager max hum
                    if ((HumRatNeeded == 0.0) || (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat <= HumRatNeeded)) {
                        HumRatNeeded = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat;
                    }
                } else {
                    HumRatNeeded = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat;
                }

            } else {
            }
        }
    }

    void CalcSolidDesiccantDehumidifier(EnergyPlusData &state,
                                        int const DesicDehumNum,      // number of the current dehumidifier being simulated
                                        Real64 const HumRatNeeded,    // process air leaving humidity ratio set by controller [kgWater/kgDryAir]
                                        bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the electricity consumption, regen heat requirements and the outlet
        // conditions for a solid desiccant dehumidifier, given the inlet conditions and
        // and the needed process leaving humidity ratio.

        // METHODOLOGY EMPLOYED:
        // Given the entering conditions, the full-load outlet conditions are calculated.
        // Adjust for part-load if required.
        // Caclulate required regen energy and call regen coil and regen fan.
        // Desiccant wheel leaving conditions and regen energy requirements are calculated
        // from empirical curve fits.  The user can select either default built-in
        // performance curves, or use custom user-defined curves.

        // REFERENCES:
        // The default performance curves represent a commerical-grade solid desiccant
        // wheel typical of HVAC applications in the early 1990's.  These curves were
        // developed for Gas Research Institute by William W. Worek, University of Illinois
        // at Chicago.

        // Using/Aliasing
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 ProcAirInHumRat;     // process inlet air humidity ratio [kgWater/kgDryAir]
        Real64 ProcAirInTemp;       // process inlet air temperature [C]
        Real64 ProcAirOutHumRat;    // process outlet air humidity ratio [kgWater/kgDryAir]
        Real64 MinProcAirOutHumRat; // minimum available process outlet air humidity ratio [kgWater/kgDryAir]
        Real64 ProcAirOutTemp;      // process outlet air temperature [C]
        Real64 ProcAirVel;          // process air velocity [m/s]
        Real64 QRegen;              // regen heat input rate requested from regen coil [W]
        Real64 QDelivered;          // regen heat actually delivered by regen coil [W]
        // REAL(r64) :: RegenAirInHumRat        ! regen inlet air humidity ratio [kgWater/kgDryAir]
        Real64 RegenAirInTemp;       // regen inlet air temperature [C]
        Real64 RegenAirVel;          // regen air velocity [m/s]
        Real64 ProcAirMassFlowRate;  // process air mass flow rate [kg/s]
        Real64 RegenAirMassFlowRate; // regen air mass flow rate [kg/s]
        Real64 SpecRegenEnergy;      // specific regen energy [J/kg of water removed]
        Real64 NomRegenTemp;         // nominal regen temperature for regen energy curve
        Real64 ElecUseRate;          // electricity consumption rate [W]
        Real64 PartLoad;             // fraction of dehumidification capacity required to meet setpoint
        bool UnitOn;                 // unit on flag

        // Variables for hardwired coefficients for default performance model

        Real64 TC0;
        Real64 TC1;
        Real64 TC2;
        Real64 TC3;
        Real64 TC4;
        Real64 TC5;
        Real64 TC6;
        Real64 TC7;
        Real64 TC8;
        Real64 TC9;
        Real64 TC10;
        Real64 TC11;
        Real64 TC12;
        Real64 TC13;
        Real64 TC14;
        Real64 TC15;

        Real64 WC0;
        Real64 WC1;
        Real64 WC2;
        Real64 WC3;
        Real64 WC4;
        Real64 WC5;
        Real64 WC6;
        Real64 WC7;
        Real64 WC8;
        Real64 WC9;
        Real64 WC10;
        Real64 WC11;
        Real64 WC12;
        Real64 WC13;
        Real64 WC14;
        Real64 WC15;

        Real64 QC0;
        Real64 QC1;
        Real64 QC2;
        Real64 QC3;
        Real64 QC4;
        Real64 QC5;
        Real64 QC6;
        Real64 QC7;
        Real64 QC8;
        Real64 QC9;
        Real64 QC10;
        Real64 QC11;
        Real64 QC12;
        Real64 QC13;
        Real64 QC14;
        Real64 QC15;

        Real64 RC0;
        Real64 RC1;
        Real64 RC2;
        Real64 RC3;
        Real64 RC4;
        Real64 RC5;
        Real64 RC6;
        Real64 RC7;
        Real64 RC8;
        Real64 RC9;
        Real64 RC10;
        Real64 RC11;
        Real64 RC12;
        Real64 RC13;
        Real64 RC14;
        Real64 RC15;

        auto &DesicDehum(state.dataDesiccantDehumidifiers->DesicDehum);
        auto &RhoAirStdInit(state.dataDesiccantDehumidifiers->RhoAirStdInit);

        // Setup internal variables for calculations

        ProcAirInTemp = DesicDehum(DesicDehumNum).ProcAirInTemp;
        ProcAirInHumRat = DesicDehum(DesicDehumNum).ProcAirInHumRat;
        ProcAirMassFlowRate = DesicDehum(DesicDehumNum).ProcAirInMassFlowRate;
        ProcAirVel = DesicDehum(DesicDehumNum).NomProcAirVel;
        PartLoad = 0.0;

        RegenAirInTemp = DesicDehum(DesicDehumNum).RegenAirInTemp;
        NomRegenTemp = DesicDehum(DesicDehumNum).NomRegenTemp;

        // Calculate min available process out humrat
        UnitOn = false;
        MinProcAirOutHumRat = 0.0; // MAX(MinProcAirOutHumRat,0.000857)

        if (HumRatNeeded < ProcAirInHumRat) {

            UnitOn = true;

            {
                auto const SELECT_CASE_var(DesicDehum(DesicDehumNum).PerformanceModel_Num); // Performance Model Part A

                if (SELECT_CASE_var == PerformanceModel::Default) {

                    WC0 = 0.0148880824323806;
                    WC1 = -0.000283393198398211;
                    WC2 = -0.87802168940547;
                    WC3 = -0.000713615831236411;
                    WC4 = 0.0311261188874622;
                    WC5 = 1.51738892142485e-06;
                    WC6 = 0.0287250198281021;
                    WC7 = 4.94796903231558e-06;
                    WC8 = 24.0771139652826;
                    WC9 = 0.000122270283927978;
                    WC10 = -0.0151657189566474;
                    WC11 = 3.91641393230322e-08;
                    WC12 = 0.126032651553348;
                    WC13 = 0.000391653854431574;
                    WC14 = 0.002160537360507;
                    WC15 = 0.00132732844211593;

                    MinProcAirOutHumRat = WC0 + WC1 * ProcAirInTemp + WC2 * ProcAirInHumRat + WC3 * ProcAirVel +
                                          WC4 * ProcAirInTemp * ProcAirInHumRat + WC5 * ProcAirInTemp * ProcAirVel +
                                          WC6 * ProcAirInHumRat * ProcAirVel + WC7 * ProcAirInTemp * ProcAirInTemp +
                                          WC8 * ProcAirInHumRat * ProcAirInHumRat + WC9 * ProcAirVel * ProcAirVel +
                                          WC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat +
                                          WC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel +
                                          WC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + WC13 * std::log(ProcAirInTemp) +
                                          WC14 * std::log(ProcAirInHumRat) + WC15 * std::log(ProcAirVel);

                    // limit to 6 grains/lb (0.000857 kg/kg)

                } else if (SELECT_CASE_var == PerformanceModel::UserCurves) {

                    MinProcAirOutHumRat = CurveValue(state, DesicDehum(DesicDehumNum).ProcHumRatCurvefTW, ProcAirInTemp, ProcAirInHumRat) *
                                          CurveValue(state, DesicDehum(DesicDehumNum).ProcHumRatCurvefV, ProcAirVel);

                } else {

                    ShowFatalError(
                        state, format("Invalid performance model in desiccant dehumidifier = {}", DesicDehum(DesicDehumNum).PerformanceModel_Num));
                }
            } // Performance Model Part A

            MinProcAirOutHumRat = max(MinProcAirOutHumRat, 0.000857);
        }

        if (MinProcAirOutHumRat >= ProcAirInHumRat) UnitOn = false;

        if (UnitOn) {

            // Calculate partload fraction of dehumidification capacity required to meet setpoint
            PartLoad = 1.0;
            if (MinProcAirOutHumRat < HumRatNeeded) PartLoad = (ProcAirInHumRat - HumRatNeeded) / (ProcAirInHumRat - MinProcAirOutHumRat);
            PartLoad = max(0.0, PartLoad);
            PartLoad = min(1.0, PartLoad);

            {
                auto const SELECT_CASE_var(DesicDehum(DesicDehumNum).PerformanceModel_Num); // Performance Model Part B

                if (SELECT_CASE_var == PerformanceModel::Default) {

                    // Calculate leaving conditions
                    TC0 = -38.7782841989449;
                    TC1 = 2.0127655837628;
                    TC2 = 5212.49360216097;
                    TC3 = 15.2362536782665;
                    TC4 = -80.4910419759181;
                    TC5 = -0.105014122001509;
                    TC6 = -229.668673645144;
                    TC7 = -0.015424703743461;
                    TC8 = -69440.0689831847;
                    TC9 = -1.6686064694322;
                    TC10 = 38.5855718977592;
                    TC11 = 0.000196395381206009;
                    TC12 = 386.179386548324;
                    TC13 = -0.801959614172614;
                    TC14 = -3.33080986818745;
                    TC15 = -15.2034386065714;

                    ProcAirOutTemp = TC0 + TC1 * ProcAirInTemp + TC2 * ProcAirInHumRat + TC3 * ProcAirVel + TC4 * ProcAirInTemp * ProcAirInHumRat +
                                     TC5 * ProcAirInTemp * ProcAirVel + TC6 * ProcAirInHumRat * ProcAirVel + TC7 * ProcAirInTemp * ProcAirInTemp +
                                     TC8 * ProcAirInHumRat * ProcAirInHumRat + TC9 * ProcAirVel * ProcAirVel +
                                     TC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat +
                                     TC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel +
                                     TC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + TC13 * std::log(ProcAirInTemp) +
                                     TC14 * std::log(ProcAirInHumRat) + TC15 * std::log(ProcAirVel);

                    // Regen energy
                    QC0 = -27794046.6291107;
                    QC1 = -235725.171759615;
                    QC2 = 975461343.331328;
                    QC3 = -686069.373946731;
                    QC4 = -17717307.3766266;
                    QC5 = 31482.2539662489;
                    QC6 = 55296552.8260743;
                    QC7 = 6195.36070023868;
                    QC8 = -8304781359.40435;
                    QC9 = -188987.543809419;
                    QC10 = 3933449.40965846;
                    QC11 = -6.66122876558634;
                    QC12 = -349102295.417547;
                    QC13 = 83672.179730172;
                    QC14 = -6059524.33170538;
                    QC15 = 1220523.39525162;

                    SpecRegenEnergy = QC0 + QC1 * ProcAirInTemp + QC2 * ProcAirInHumRat + QC3 * ProcAirVel + QC4 * ProcAirInTemp * ProcAirInHumRat +
                                      QC5 * ProcAirInTemp * ProcAirVel + QC6 * ProcAirInHumRat * ProcAirVel + QC7 * ProcAirInTemp * ProcAirInTemp +
                                      QC8 * ProcAirInHumRat * ProcAirInHumRat + QC9 * ProcAirVel * ProcAirVel +
                                      QC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat +
                                      QC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel +
                                      QC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + QC13 * std::log(ProcAirInTemp) +
                                      QC14 * std::log(ProcAirInHumRat) + QC15 * std::log(ProcAirVel);

                    // Regen face velocity
                    RC0 = -4.67358908091488;
                    RC1 = 0.0654323095468338;
                    RC2 = 396.950518702316;
                    RC3 = 1.52610165426736;
                    RC4 = -11.3955868430328;
                    RC5 = 0.00520693906104437;
                    RC6 = 57.783645385621;
                    RC7 = -0.000464800668311693;
                    RC8 = -5958.78613212602;
                    RC9 = -0.205375818291012;
                    RC10 = 5.26762675442845;
                    RC11 = -8.88452553055039e-05;
                    RC12 = -182.382479369311;
                    RC13 = -0.100289774002047;
                    RC14 = -0.486980507964251;
                    RC15 = -0.972715425435447;

                    RegenAirVel = RC0 + RC1 * ProcAirInTemp + RC2 * ProcAirInHumRat + RC3 * ProcAirVel + RC4 * ProcAirInTemp * ProcAirInHumRat +
                                  RC5 * ProcAirInTemp * ProcAirVel + RC6 * ProcAirInHumRat * ProcAirVel + RC7 * ProcAirInTemp * ProcAirInTemp +
                                  RC8 * ProcAirInHumRat * ProcAirInHumRat + RC9 * ProcAirVel * ProcAirVel +
                                  RC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat +
                                  RC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel +
                                  RC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + RC13 * std::log(ProcAirInTemp) +
                                  RC14 * std::log(ProcAirInHumRat) + RC15 * std::log(ProcAirVel);

                } else if (SELECT_CASE_var == PerformanceModel::UserCurves) {

                    ProcAirOutTemp = CurveValue(state, DesicDehum(DesicDehumNum).ProcDryBulbCurvefTW, ProcAirInTemp, ProcAirInHumRat) *
                                     CurveValue(state, DesicDehum(DesicDehumNum).ProcDryBulbCurvefV, ProcAirVel);

                    SpecRegenEnergy = CurveValue(state, DesicDehum(DesicDehumNum).RegenEnergyCurvefTW, ProcAirInTemp, ProcAirInHumRat) *
                                      CurveValue(state, DesicDehum(DesicDehumNum).RegenEnergyCurvefV, ProcAirVel);

                    RegenAirVel = CurveValue(state, DesicDehum(DesicDehumNum).RegenVelCurvefTW, ProcAirInTemp, ProcAirInHumRat) *
                                  CurveValue(state, DesicDehum(DesicDehumNum).RegenVelCurvefV, ProcAirVel);

                } else {

                    ShowFatalError(
                        state, format("Invalid performance model in desiccant dehumidifier = {}", DesicDehum(DesicDehumNum).PerformanceModel_Num));

                    // Suppress uninitialized warnings
                    ProcAirOutTemp = 0.0;
                    SpecRegenEnergy = 0.0;
                    RegenAirVel = 0.0;
                }
            } // Performance Model Part B

            ProcAirOutTemp = (1 - PartLoad) * ProcAirInTemp + (PartLoad)*ProcAirOutTemp;

            ProcAirOutHumRat = (1 - PartLoad) * ProcAirInHumRat + (PartLoad)*MinProcAirOutHumRat;

            // Calculate water removal
            DesicDehum(DesicDehumNum).WaterRemoveRate = ProcAirMassFlowRate * (ProcAirInHumRat - ProcAirOutHumRat);

            // Adjust for regen inlet temperature
            SpecRegenEnergy *= (NomRegenTemp - RegenAirInTemp) / (NomRegenTemp - ProcAirInTemp);
            SpecRegenEnergy = max(SpecRegenEnergy, 0.0);
            QRegen = SpecRegenEnergy * DesicDehum(DesicDehumNum).WaterRemoveRate;

            // Above curves are based on a 90deg regen angle and 245deg process air angle
            RegenAirMassFlowRate = ProcAirMassFlowRate * 90.0 / 245.0 * RegenAirVel / ProcAirVel;

            ElecUseRate = DesicDehum(DesicDehumNum).NomRotorPower;

        } else { // Unit is off

            ProcAirOutTemp = ProcAirInTemp;
            ProcAirOutHumRat = ProcAirInHumRat;
            SpecRegenEnergy = 0.0;
            QRegen = 0.0;
            ElecUseRate = 0.0;
            RegenAirVel = 0.0;
            RegenAirMassFlowRate = 0.0;
            DesicDehum(DesicDehumNum).WaterRemoveRate = 0.0;
            PartLoad = 0.0;

        } // UnitOn/Off

        // Set regen mass flow
        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).MassFlowRate = RegenAirMassFlowRate;
        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).MassFlowRateMaxAvail = RegenAirMassFlowRate;
        // Call regen fan
        if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
        } else {
            state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
        }

        // Call regen heating coil
        CalcNonDXHeatingCoils(state, DesicDehumNum, FirstHVACIteration, QRegen, QDelivered);

        // Verify is requestd flow was delivered (must do after heating coil has executed to pass flow to RegenAirInNode)
        if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate != RegenAirMassFlowRate) {
            // Initialize standard air density
            if (state.dataDesiccantDehumidifiers->CalcSolidDesiccantDehumidifierMyOneTimeFlag) {
                RhoAirStdInit = state.dataEnvrn->StdRhoAir;
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          "Improper flow delivered by desiccant regen fan - RESULTS INVALID! Check regen fan capacity and schedule.",
                                          DesicDehum(DesicDehumNum).RegenFanErrorIndex1);
            ShowRecurringContinueErrorAtEnd(
                state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name, DesicDehum(DesicDehumNum).RegenFanErrorIndex2);
            RhoAirStdInit = state.dataEnvrn->StdRhoAir;
            ShowRecurringContinueErrorAtEnd(state,
                                            "Flow requested [m3/s] from " + DesicDehum(DesicDehumNum).RegenFanType + '=' +
                                                DesicDehum(DesicDehumNum).RegenFanName,
                                            DesicDehum(DesicDehumNum).RegenFanErrorIndex3,
                                            (RegenAirMassFlowRate / RhoAirStdInit));
            ShowRecurringContinueErrorAtEnd(
                state,
                "Flow request varied from delivered by [m3/s]",
                DesicDehum(DesicDehumNum).RegenFanErrorIndex4,
                ((RegenAirMassFlowRate - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate) / RhoAirStdInit),
                ((RegenAirMassFlowRate - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate) / RhoAirStdInit));
        }

        // Verify is requestd heating was delivered
        if (QDelivered < QRegen) {
            ShowRecurringSevereErrorAtEnd(
                state,
                "Inadequate heat delivered by desiccant regen coil - RESULTS INVALID! Check regen coil capacity and schedule.",
                DesicDehum(DesicDehumNum).RegenCapErrorIndex1);
            ShowRecurringContinueErrorAtEnd(
                state, DesicDehum(DesicDehumNum).DehumType + '=' + DesicDehum(DesicDehumNum).Name, DesicDehum(DesicDehumNum).RegenCapErrorIndex2);
            ShowRecurringContinueErrorAtEnd(state,
                                            "Load requested [W] from " + DesicDehum(DesicDehumNum).RegenCoilType + '=' +
                                                DesicDehum(DesicDehumNum).RegenCoilName,
                                            DesicDehum(DesicDehumNum).RegenCapErrorIndex3,
                                            QRegen);
            ShowRecurringContinueErrorAtEnd(
                state, "Load request exceeded delivered by [W]", DesicDehum(DesicDehumNum).RegenCapErrorIndex4, (QRegen - QDelivered));
        }

        DesicDehum(DesicDehumNum).SpecRegenEnergy = SpecRegenEnergy;
        DesicDehum(DesicDehumNum).QRegen = QRegen;
        DesicDehum(DesicDehumNum).ElecUseRate = ElecUseRate;
        DesicDehum(DesicDehumNum).PartLoad = PartLoad;

        DesicDehum(DesicDehumNum).ProcAirOutMassFlowRate = ProcAirMassFlowRate;
        DesicDehum(DesicDehumNum).ProcAirOutTemp = ProcAirOutTemp;
        DesicDehum(DesicDehumNum).ProcAirOutHumRat = ProcAirOutHumRat;
        DesicDehum(DesicDehumNum).ProcAirOutEnthalpy = PsyHFnTdbW(ProcAirOutTemp, ProcAirOutHumRat);
        DesicDehum(DesicDehumNum).RegenAirInMassFlowRate = RegenAirMassFlowRate;
        DesicDehum(DesicDehumNum).RegenAirVel = RegenAirVel;

        //  DesicDehum(DesicDehumNum)%RegenAirOutTemp        = -999.
        //  DesicDehum(DesicDehumNum)%RegenAirOutHumRat      = -999.
        //  DesicDehum(DesicDehumNum)%RegenAirOutEnthalpy    = -999.
    }

    void CalcGenericDesiccantDehumidifier(EnergyPlusData &state,
                                          int const DesicDehumNum,      // number of the current dehumidifier being simulated
                                          Real64 const HumRatNeeded,    // process air leaving humidity ratio set by controller [kg water/kg air]
                                          bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mangesh Basarkar, FSEC
        //       DATE WRITTEN   May 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the electricity consumption, regen heat requirements and the outlet
        // conditions for a desiccant dehumidifier, given the inlet conditions,
        // DX coil part-load ratio, and/or the needed process leaving humidity ratio.

        // METHODOLOGY EMPLOYED:
        // Given the entering conditions, the full-load outlet conditions are calculated.
        // Adjust for part-load if required.
        // Calculate the required regen energy and call the regen coil and the regen fan.

        // REFERENCES:
        // Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006.
        // Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component
        //   Augmentation of the Cooling Coil. 15th Symposium on Improving Building Systems in Hot and Humid
        //   Climates, July 24-26, 2006.

        // Using/Aliasing
        using HeatRecovery::SimHeatRecovery;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MinVolFlowPerRatedTotQ(0.00002684); // m3/s per W = 200 cfm/ton,
        // min vol flow per rated evaporator capacity

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 DDPartLoadRatio;        // fraction of dehumidification capacity required to meet setpoint
        Real64 MassFlowRateNew;        // new required mass flow rate calculated to keep regen setpoint temperature (kg/s)
        Real64 CondenserWasteHeat;     // Condenser waste heat (W)
        Real64 CpAir;                  // Specific heat of air (J/kg-K)
        Real64 NewRegenInTemp;         // new temp calculated from condenser waste heat (C)
        Real64 ExhaustFanMassFlowRate; // exhaust fan mass flow rate (kg/s)
        Real64 ExhaustFanPLR;          // exhaust fan run time fraction calculated from new mass flow rate for regen side
        Real64 ExhaustFanPowerMod;     // used to calculate exhaust fan power from flow fraction
        Real64 VolFlowPerRatedTotQ;    // flow rate per rated total cooling capacity of the companion coil (m3/s/W)
        Real64 FanDeltaT;              // used to account for fan heat when calculating regeneration heater energy (C)
        Real64 OnOffFanPLF;            // save air loop fan part load fracton while calculating exhaust fan power
        Real64 RegenSetPointTemp;      // regeneration temperature setpoint (C)
        int RegenCoilIndex;            // index to regeneration heating coil, 0 when not used
        int CompanionCoilIndexNum;     // index for companion DX cooling coil, 0 when DX coil is not used
        std::string MinVol;            // character string used for error messages
        bool UnitOn;                   // unit on flag
        //  LOGICAL       :: SimFlag                    ! used to turn off additional simulation if DX Coil is off
        Real64 QRegen_OASysFanAdjust; // temporary variable used to adjust regen heater load during iteration

        auto &DesicDehum(state.dataDesiccantDehumidifiers->DesicDehum);
        auto &QRegen(state.dataDesiccantDehumidifiers->QRegen);

        UnitOn = false;
        DDPartLoadRatio = 0.0;
        RegenCoilIndex = DesicDehum(DesicDehumNum).RegenCoilIndex;
        FanDeltaT = 0.0;
        RegenSetPointTemp = DesicDehum(DesicDehumNum).RegenSetPointTemp;
        ExhaustFanMassFlowRate = 0.0;

        // Save OnOffFanPartLoadFraction while performing exhaust fan calculations
        OnOffFanPLF = state.dataHVACGlobal->OnOffFanPartLoadFraction;
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;

        if (DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide == Selection::Yes) {
            // Cooling coil directly upstream of desiccant dehumidifier, dehumidifier runs in tandem with DX coil

            CompanionCoilIndexNum = DesicDehum(DesicDehumNum).DXCoilIndex;
        } else {
            // desiccant dehumidifier determines its own PLR
            CompanionCoilIndexNum = 0;
        }

        if (state.dataDesiccantDehumidifiers->CalcGenericDesiccantDehumidifierMyOneTimeFlag) {
            state.dataDesiccantDehumidifiers->RhoAirStdInitCGDD = state.dataEnvrn->StdRhoAir;
            state.dataDesiccantDehumidifiers->CalcGenericDesiccantDehumidifierMyOneTimeFlag = false;
        }

        if (HumRatNeeded < state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat) {
            UnitOn = true;
        }

        if (DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide == Selection::Yes) {
            if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                if (state.dataDXCoils->DXCoilPartLoadRatio(DesicDehum(DesicDehumNum).DXCoilIndex) == 0.0) {
                    UnitOn = false;
                }
            }
        }

        if (UnitOn) {

            if (DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode) {
                if (DesicDehum(DesicDehumNum).HXTypeNum == BalancedHX) {
                    state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate =
                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).MassFlowRate;
                    state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRateMaxAvail =
                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).MassFlowRate;
                }
            }

            // Get conditions from DX Coil condenser if present (DXCoilIndex verified > 0 in GetInput)
            if (DesicDehum(DesicDehumNum).Preheat == Selection::Yes) {

                //     condenser waste heat is proportional to DX coil PLR
                if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                    (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                    CondenserWasteHeat = state.dataHeatBal->HeatReclaimDXCoil(DesicDehum(DesicDehumNum).DXCoilIndex).AvailCapacity;
                    state.dataHeatBal->HeatReclaimDXCoil(DesicDehum(DesicDehumNum).DXCoilIndex).AvailCapacity = 0.0;
                } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    CondenserWasteHeat = state.dataHeatBal->HeatReclaimVS_DXCoil(DesicDehum(DesicDehumNum).DXCoilIndex).AvailCapacity;
                    state.dataHeatBal->HeatReclaimVS_DXCoil(DesicDehum(DesicDehumNum).DXCoilIndex).AvailCapacity = 0.0;
                }

                CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).HumRat);

                if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                    if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                        Fans::SimulateFanComponents(
                            state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
                    } else {
                        state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
                    }
                    FanDeltaT = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanOutNode).Temp -
                                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).Temp;
                    //       Adjust setpoint to account for fan heat
                    RegenSetPointTemp -= FanDeltaT;
                }

                //     CompanionCoilIndexNum .GT. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes
                if (CompanionCoilIndexNum > 0) {
                    if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                        (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                        DDPartLoadRatio = state.dataDXCoils->DXCoilPartLoadRatio(DesicDehum(DesicDehumNum).DXCoilIndex);
                    } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        DDPartLoadRatio = 1.0; // condenser waste heat already includes modulation down
                    }
                }

                //     calculate actual condenser outlet node (regen inlet node) temperature
                if (CompanionCoilIndexNum > 0) {
                    if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                        (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                        if (state.dataDXCoils->DXCoilFanOpMode(DesicDehum(DesicDehumNum).DXCoilIndex) == ContFanCycCoil) {
                            NewRegenInTemp =
                                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp +
                                CondenserWasteHeat /
                                    (CpAir * (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate) * DDPartLoadRatio);
                            CondenserWasteHeat /= DDPartLoadRatio;
                        } else {
                            NewRegenInTemp =
                                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp +
                                CondenserWasteHeat / (CpAir * (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate));
                        }
                    } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        NewRegenInTemp =
                            state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp +
                            CondenserWasteHeat / (CpAir * (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate));
                    } else {
                        NewRegenInTemp =
                            state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp +
                            CondenserWasteHeat / (CpAir * (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate));
                    }
                } else {
                    NewRegenInTemp =
                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp +
                        CondenserWasteHeat / (CpAir * (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate));
                }

                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Temp = NewRegenInTemp;
                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Enthalpy =
                    PsyHFnTdbW(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Temp,
                               state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).HumRat);
                MassFlowRateNew = 0.0;

                if (DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate > 0) {

                    //       calculate mass flow rate required to maintain regen inlet setpoint temp
                    if (NewRegenInTemp > RegenSetPointTemp) {
                        if (RegenSetPointTemp - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp != 0.0) {
                            MassFlowRateNew =
                                max(0.0,
                                    CondenserWasteHeat /
                                        (CpAir * (RegenSetPointTemp - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp)));
                        } else {
                            MassFlowRateNew = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate;
                        }
                    }

                    //       calculate exhaust fan mass flow rate and new regen inlet temperature (may not be at setpoint)
                    if (MassFlowRateNew > state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate) {
                        ExhaustFanMassFlowRate = MassFlowRateNew - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate;
                        ExhaustFanMassFlowRate = max(0.0, min(ExhaustFanMassFlowRate, DesicDehum(DesicDehumNum).ExhaustFanMaxMassFlowRate));

                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Temp =
                            state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).Temp +
                            CondenserWasteHeat /
                                (CpAir * (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate + ExhaustFanMassFlowRate));
                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).HumRat =
                            state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).CondenserInletNode).HumRat;
                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Enthalpy =
                            PsyHFnTdbW(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Temp,
                                       state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).HumRat);
                    }
                }

                if (RegenCoilIndex > 0) {
                    if (NewRegenInTemp < RegenSetPointTemp) {
                        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).HumRat);
                    }
                    QRegen = max(0.0,
                                 (CpAir * state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate *
                                  (RegenSetPointTemp - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Temp)));
                    if (QRegen == 0.0) QRegen = -1.0;
                }

                //     CompanionCoilIndexNum .EQ. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No
                if (CompanionCoilIndexNum == 0) {

                    if (RegenCoilIndex > 0) {

                        QRegen_OASysFanAdjust = QRegen;
                        if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                            if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate > 0.0) {
                                //             For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot
                                //             reduction through fan
                                QRegen_OASysFanAdjust *= state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanOutNode).MassFlowRate /
                                                         state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).MassFlowRate;
                            }
                        }

                        CalcNonDXHeatingCoils(state, DesicDehumNum, FirstHVACIteration, QRegen_OASysFanAdjust);
                    }

                    SimHeatRecovery(state,
                                    DesicDehum(DesicDehumNum).HXName,
                                    FirstHVACIteration,
                                    DesicDehum(DesicDehumNum).CompIndex,
                                    ContFanCycCoil,
                                    1.0,
                                    true,
                                    CompanionCoilIndexNum,
                                    DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode,
                                    _,
                                    _,
                                    DesicDehum(DesicDehumNum).coolingCoil_TypeNum);

                    //       calculate desiccant part-load ratio
                    if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat !=
                        state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRat) {
                        DDPartLoadRatio = (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat - HumRatNeeded) /
                                          (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat -
                                           state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRat);
                        DDPartLoadRatio = max(0.0, min(1.0, DDPartLoadRatio));
                    } else {
                        DDPartLoadRatio = 1.0;
                    }
                }

                if (ExhaustFanMassFlowRate > 0.0) {

                    //       calculate exhaust fan mass flow rate due to desiccant system operation
                    ExhaustFanMassFlowRate *= DDPartLoadRatio;

                    //       calculate exhaust fan PLR due to desiccant system operation
                    ExhaustFanPLR = ExhaustFanMassFlowRate / DesicDehum(DesicDehumNum).ExhaustFanMaxMassFlowRate;

                    //       find exhaust fan power multiplier using exhaust fan part-load ratio
                    if (DesicDehum(DesicDehumNum).ExhaustFanCurveIndex > 0) {
                        ExhaustFanPowerMod = min(1.0, max(0.0, CurveValue(state, DesicDehum(DesicDehumNum).ExhaustFanCurveIndex, ExhaustFanPLR)));
                    } else {
                        ExhaustFanPowerMod = 1.0;
                    }

                    //       calculate exhaust fan power due to desiccant operation
                    DesicDehum(DesicDehumNum).ExhaustFanPower = DesicDehum(DesicDehumNum).ExhaustFanMaxPower * ExhaustFanPowerMod;
                }

            } else { // ELSE for IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

                if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat > HumRatNeeded) {

                    //       Get Full load output of desiccant wheel
                    if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                        if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                            Fans::SimulateFanComponents(
                                state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
                        } else {
                            state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
                        }

                        FanDeltaT = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanOutNode).Temp -
                                    state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).Temp;
                        RegenSetPointTemp -= FanDeltaT;
                    }

                    if (RegenCoilIndex > 0) {
                        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).HumRat);
                        QRegen = max(0.0,
                                     (CpAir * state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate *
                                      (RegenSetPointTemp - state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).Temp)));

                        QRegen_OASysFanAdjust = QRegen;
                        if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                            if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate > 0.0) {
                                //             For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot
                                //             reduction through fan
                                QRegen_OASysFanAdjust *= state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanOutNode).MassFlowRate /
                                                         state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).MassFlowRate;
                            }
                        }

                        if (QRegen_OASysFanAdjust == 0.0) QRegen_OASysFanAdjust = -1.0;
                        CalcNonDXHeatingCoils(state, DesicDehumNum, FirstHVACIteration, QRegen_OASysFanAdjust);
                    }

                    //       CompanionCoilIndexNum .EQ. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No
                    if (CompanionCoilIndexNum == 0) {
                        SimHeatRecovery(state,
                                        DesicDehum(DesicDehumNum).HXName,
                                        FirstHVACIteration,
                                        DesicDehum(DesicDehumNum).CompIndex,
                                        ContFanCycCoil,
                                        1.0,
                                        true,
                                        CompanionCoilIndexNum,
                                        DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode,
                                        _,
                                        _,
                                        DesicDehum(DesicDehumNum).coolingCoil_TypeNum);

                        //         calculate desiccant part-load ratio
                        if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat !=
                            state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRat) {
                            DDPartLoadRatio = (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat - HumRatNeeded) /
                                              (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat -
                                               state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRat);
                            DDPartLoadRatio = max(0.0, min(1.0, DDPartLoadRatio));
                        } else {
                            DDPartLoadRatio = 1.0;
                        }
                    } else {
                        if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                            (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                            DDPartLoadRatio = state.dataDXCoils->DXCoilPartLoadRatio(DesicDehum(DesicDehumNum).DXCoilIndex);
                        } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                            DDPartLoadRatio = 1.0; // condenser waste heat already includes modulation down
                        }
                    }
                } else { // ELSE for IF(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN
                    DDPartLoadRatio = 0.0;
                } // END IF for IF(state.dataLoopNodes->Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN

            } // END IF for IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

            DesicDehum(DesicDehumNum).PartLoad = DDPartLoadRatio;
            QRegen_OASysFanAdjust = QRegen;

            // set average regeneration air mass flow rate based on desiccant cycling ratio (DDPartLoadRatio)
            if (DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode) {
                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate *= DDPartLoadRatio;

                // **RR moved to here, only adjust regen heater load if mass flow rate is changed
                //   adjust regen heating coil capacity based on desiccant cycling ratio (PLR)
                QRegen_OASysFanAdjust *= DDPartLoadRatio;
            }

            // Call regen fan, balanced desiccant HX and heating coil
            if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(
                        state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
                } else {
                    state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
                }
            }

            if (RegenCoilIndex > 0) {

                //!   adjust regen heating coil capacity based on desiccant cycling ratio (PLR)
                //    QRegen_OASysFanAdjust = QRegen * DDPartLoadRatio

                if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                    if (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate > 0.0) {
                        //       For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
                        QRegen_OASysFanAdjust *= state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanOutNode).MassFlowRate /
                                                 state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenFanInNode).MassFlowRate;
                    }
                }

                if (QRegen_OASysFanAdjust == 0.0) QRegen_OASysFanAdjust = -1.0;
                CalcNonDXHeatingCoils(state, DesicDehumNum, FirstHVACIteration, QRegen_OASysFanAdjust);
            }

            SimHeatRecovery(state,
                            DesicDehum(DesicDehumNum).HXName,
                            FirstHVACIteration,
                            DesicDehum(DesicDehumNum).CompIndex,
                            ContFanCycCoil,
                            DDPartLoadRatio,
                            true,
                            CompanionCoilIndexNum,
                            DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode,
                            _,
                            _,
                            DesicDehum(DesicDehumNum).coolingCoil_TypeNum);

            if (DesicDehum(DesicDehumNum).RegenFanPlacement == DrawThru) {
                if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(
                        state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
                } else {
                    state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
                }
            }

            // Calculate water removal
            DesicDehum(DesicDehumNum).WaterRemoveRate = state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).MassFlowRate *
                                                        (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirInNode).HumRat -
                                                         state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).ProcAirOutNode).HumRat);

            // If preheat is Yes, exhaust fan is condenser fan, if CoilUpstreamOfProcessSide is No, DD runs an its own PLR
            if (DesicDehum(DesicDehumNum).Preheat == Selection::Yes && DesicDehum(DesicDehumNum).CoilUpstreamOfProcessSide == Selection::No) {
                //    should actually use DX coil RTF instead of PLR since fan power is being calculated
                if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                    (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                    DesicDehum(DesicDehumNum).ExhaustFanPower +=
                        max(0.0,
                            (DesicDehum(DesicDehumNum).ExhaustFanMaxPower *
                             (state.dataDXCoils->DXCoilPartLoadRatio(DesicDehum(DesicDehumNum).DXCoilIndex) - DDPartLoadRatio)));
                } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    DesicDehum(DesicDehumNum).ExhaustFanPower += max(0.0, (DesicDehum(DesicDehumNum).ExhaustFanMaxPower * (1.0 - DDPartLoadRatio)));
                }
            }

        } else { // unit must be off

            DesicDehum(DesicDehumNum).PartLoad = 0.0;

            if (DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode) {
                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate = 0.0;
                state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRateMaxAvail = 0.0;
            }

            if (DesicDehum(DesicDehumNum).RegenFanPlacement == BlowThru) {
                if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(
                        state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
                } else {
                    state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
                }
            }

            if (RegenCoilIndex > 0) {
                CalcNonDXHeatingCoils(state, DesicDehumNum, FirstHVACIteration, -1.0);
            }

            SimHeatRecovery(state,
                            DesicDehum(DesicDehumNum).HXName,
                            FirstHVACIteration,
                            DesicDehum(DesicDehumNum).CompIndex,
                            ContFanCycCoil,
                            0.0,
                            false,
                            CompanionCoilIndexNum,
                            DesicDehum(DesicDehumNum).RegenInletIsOutsideAirNode,
                            _,
                            _,
                            DesicDehum(DesicDehumNum).coolingCoil_TypeNum);

            if (DesicDehum(DesicDehumNum).RegenFanPlacement == DrawThru) {
                if (DesicDehum(DesicDehumNum).regenFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(
                        state, DesicDehum(DesicDehumNum).RegenFanName, FirstHVACIteration, DesicDehum(DesicDehumNum).RegenFanIndex);
                } else {
                    state.dataHVACFan->fanObjs[DesicDehum(DesicDehumNum).RegenFanIndex]->simulate(state, _, _, _, _);
                }
            }

            // Turn on exhaust fan if DX Coil is operating
            if (DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate > 0) {
                if (DesicDehum(DesicDehumNum).DXCoilIndex > 0) {
                    if ((DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingSingleSpeed) ||
                        (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl)) {
                        DDPartLoadRatio = state.dataDXCoils->DXCoilPartLoadRatio(DesicDehum(DesicDehumNum).DXCoilIndex);
                    } else if (DesicDehum(DesicDehumNum).coolingCoil_TypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        DDPartLoadRatio = 1.0; // condenser waste heat already includes modulation down
                    }
                    DesicDehum(DesicDehumNum).ExhaustFanPower = DesicDehum(DesicDehumNum).ExhaustFanMaxPower * DDPartLoadRatio;
                    ExhaustFanMassFlowRate = DesicDehum(DesicDehumNum).ExhaustFanMaxMassFlowRate * DDPartLoadRatio;
                }
            }

        } // UnitOn/Off

        // check condenser minimum flow per rated total capacity
        if (DDPartLoadRatio > 0.0 && DesicDehum(DesicDehumNum).ExhaustFanMaxVolFlowRate > 0.0) {
            VolFlowPerRatedTotQ =
                (state.dataLoopNodes->Node(DesicDehum(DesicDehumNum).RegenAirInNode).MassFlowRate + ExhaustFanMassFlowRate) /
                max(0.00001,
                    (DesicDehum(DesicDehumNum).CompanionCoilCapacity * DDPartLoadRatio * state.dataDesiccantDehumidifiers->RhoAirStdInitCGDD));
            if (!state.dataGlobal->WarmupFlag && (VolFlowPerRatedTotQ < MinVolFlowPerRatedTotQ)) {
                ++DesicDehum(DesicDehumNum).ErrCount;
                if (DesicDehum(DesicDehumNum).ErrCount < 2) {
                    ShowWarningError(state,
                                     format("{} \"{}\" - Air volume flow rate per watt of total condenser waste heat is below the minimum "
                                            "recommended at {:N} m3/s/W.",
                                            DesicDehum(DesicDehumNum).DehumType,
                                            DesicDehum(DesicDehumNum).Name,
                                            VolFlowPerRatedTotQ));
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state,
                                      format("Expected minimum for VolumeFlowperRatedTotalCondenserWasteHeat = [{:N}]", MinVolFlowPerRatedTotQ));
                    ShowContinueError(state, "Possible causes include inconsistent air flow rates in system components ");
                    ShowContinueError(state, "on the regeneration side of the desiccant dehumidifier.");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        DesicDehum(DesicDehumNum).DehumType + " \"" + DesicDehum(DesicDehumNum).Name +
                            "\" - Air volume flow rate per watt of rated total cooling capacity is out of range error continues...",
                        DesicDehum(DesicDehumNum).ErrIndex1,
                        VolFlowPerRatedTotQ,
                        VolFlowPerRatedTotQ);
                }
            } // flow per rated total capacity check ends
        }

        // Reset OnOffFanPartLoadFraction for process side fan calculations
        state.dataHVACGlobal->OnOffFanPartLoadFraction = OnOffFanPLF;
    }

    void UpdateDesiccantDehumidifier(EnergyPlusData &state, int const DesicDehumNum) // number of the current dehumidifier being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Moves dehumidifier output to the outlet nodes.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ProcInNode;  // process air inlet node number
        int ProcOutNode; // process air outlet node number

        {
            auto const SELECT_CASE_var(state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).DehumTypeCode);

            if (SELECT_CASE_var == DesicDehumType::Solid) {
                ProcInNode = state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ProcAirInNode;
                ProcOutNode = state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ProcAirOutNode;
                // Set the process outlet air node of the dehumidifier
                state.dataLoopNodes->Node(ProcOutNode).Temp = state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ProcAirOutTemp;
                state.dataLoopNodes->Node(ProcOutNode).HumRat = state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ProcAirOutHumRat;
                state.dataLoopNodes->Node(ProcOutNode).Enthalpy = state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ProcAirOutEnthalpy;

                // Set the process outlet nodes for properties that just pass through & not used
                state.dataLoopNodes->Node(ProcOutNode).Quality = state.dataLoopNodes->Node(ProcInNode).Quality;
                state.dataLoopNodes->Node(ProcOutNode).Press = state.dataLoopNodes->Node(ProcInNode).Press;
                state.dataLoopNodes->Node(ProcOutNode).MassFlowRate = state.dataLoopNodes->Node(ProcInNode).MassFlowRate;
                state.dataLoopNodes->Node(ProcOutNode).MassFlowRateMin = state.dataLoopNodes->Node(ProcInNode).MassFlowRateMin;
                state.dataLoopNodes->Node(ProcOutNode).MassFlowRateMax = state.dataLoopNodes->Node(ProcInNode).MassFlowRateMax;
                state.dataLoopNodes->Node(ProcOutNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(ProcInNode).MassFlowRateMinAvail;
                state.dataLoopNodes->Node(ProcOutNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(ProcInNode).MassFlowRateMaxAvail;

                //   RegenInNode =DesicDehum(DesicDehumNum)%RegenAirInNode
                //   RegenOutNode = DesicDehum(DesicDehumNum)%RegenAirOutNode
                // Set the regen outlet air node of the dehumidifier
                //   Node(RegenOutNode)%Temp         = DesicDehum(DesicDehumNum)%RegenAirOutTemp
                //   Node(RegenOutNode)%HumRat       = DesicDehum(DesicDehumNum)%RegenAirOutHumRat
                //   Node(RegenOutNode)%Enthalpy     = DesicDehum(DesicDehumNum)%RegenAirOutEnthalpy

                // Set the regen outlet nodes for properties that just pass through & not used
                //   Node(RegenOutNode)%Quality             = Node(RegenInNode)%Quality
                //   Node(RegenOutNode)%Press               = Node(RegenInNode)%Press
                //   Node(RegenOutNode)%MassFlowRate        = Node(RegenInNode)%MassFlowRate
                //   Node(RegenOutNode)%MassFlowRateMin     = Node(RegenInNode)%MassFlowRateMin
                //   Node(RegenOutNode)%MassFlowRateMax     = Node(RegenInNode)%MassFlowRateMax
                //   Node(RegenOutNode)%MassFlowRateMinAvail= Node(RegenInNode)%MassFlowRateMinAvail
                //   Node(RegenOutNode)%MassFlowRateMaxAvail= Node(RegenInNode)%MassFlowRateMaxAvail

            } else if (SELECT_CASE_var == DesicDehumType::Generic) {

                return;
            }
        }
    }

    void ReportDesiccantDehumidifier(EnergyPlusData &state, int const DesicDehumNum) // number of the current dehumidifier being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
        //                      for Gas Research Institute
        //       DATE WRITTEN   March 2001
        //       MODIFIED       June 2007, R. Raustad, Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Fill remaining report variables

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ReportingConstant;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

        {
            auto const SELECT_CASE_var(state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).DehumTypeCode);

            if (SELECT_CASE_var == DesicDehumType::Solid) {
                state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).WaterRemove =
                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).WaterRemoveRate * ReportingConstant;
                state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).RegenEnergy =
                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).QRegen * ReportingConstant;
                state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ElecUseEnergy =
                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ElecUseRate * ReportingConstant;
            } else if (SELECT_CASE_var == DesicDehumType::Generic) {
                state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).WaterRemove =
                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).WaterRemoveRate * ReportingConstant;
                state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ExhaustFanElecConsumption =
                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).ExhaustFanPower * ReportingConstant;
            }
        }
    }

    void CalcNonDXHeatingCoils(EnergyPlusData &state,
                               int const DesicDehumNum,          // Desiccant dehumidifier unit index
                               bool const FirstHVACIteration,    // flag for first HVAC iteration in the time step
                               Real64 const RegenCoilLoad,       // heating coil load to be met (Watts)
                               Optional<Real64> RegenCoilLoadmet // heating load met
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

        // METHODOLOGY EMPLOYED:
        // Simply calls the different heating coil component.  The hot water flow rate matching the coil load
        // is calculated iteratively.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;

        using HeatingCoils::SimulateHeatingCoilComponents;
        using PlantUtilities::SetComponentFlowRate;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const ErrTolerance(0.001); // convergence limit for hotwater coil
        int const SolveMaxIter(50);       // Max iteration for SolveRoot

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 RegenCoilActual; // actual heating load met
        Real64 mdot;            // heating coil steam or hot water mass flow rate
        Real64 MinWaterFlow;    // minimum hot water mass flow rate
        // unused  REAL(r64)      :: PartLoadFraction  ! heating or cooling part load fraction
        Real64 MaxHotWaterFlow; // maximum hot water mass flow rate, kg/s
        Real64 HotWaterMdot;    // actual hot water mass flow rate
        int SolFlag;

        auto &DesicDehum(state.dataDesiccantDehumidifiers->DesicDehum);

        RegenCoilActual = 0.0;
        if (RegenCoilLoad > SmallLoad) {
            {
                auto const SELECT_CASE_var(DesicDehum(DesicDehumNum).RegenCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  DesicDehum(DesicDehumNum).RegenCoilName,
                                                  FirstHVACIteration,
                                                  RegenCoilLoad,
                                                  DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                  RegenCoilActual);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    MaxHotWaterFlow = DesicDehum(DesicDehumNum).MaxCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         MaxHotWaterFlow,
                                         DesicDehum(DesicDehumNum).CoilControlNode,
                                         DesicDehum(DesicDehumNum).CoilOutletNode,
                                         DesicDehum(DesicDehumNum).LoopNum,
                                         DesicDehum(DesicDehumNum).LoopSide,
                                         DesicDehum(DesicDehumNum).BranchNum,
                                         DesicDehum(DesicDehumNum).CompNum);
                    RegenCoilActual = RegenCoilLoad;
                    // simulate the regenerator hot water heating coil
                    SimulateWaterCoilComponents(state,
                                                DesicDehum(DesicDehumNum).RegenCoilName,
                                                FirstHVACIteration,
                                                DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                RegenCoilActual);

                    if (RegenCoilActual > (RegenCoilLoad + SmallLoad)) {
                        // control water flow to obtain output matching RegenCoilLoad
                        SolFlag = 0;
                        MinWaterFlow = 0.0;
                        std::array<Real64, 3> Par;
                        Par[0] = double(DesicDehumNum);
                        if (FirstHVACIteration) {
                            Par[1] = 1.0;
                        } else {
                            Par[1] = 0.0;
                        }
                        Par[2] = RegenCoilLoad;
                        General::SolveRoot(
                            state, ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par);
                        if (SolFlag == -1) {
                            if (DesicDehum(DesicDehumNum).HotWaterCoilMaxIterIndex == 0) {
                                ShowWarningMessage(state,
                                                   "CalcNonDXHeatingCoils: Hot water coil control failed for " + DesicDehum(DesicDehumNum).DehumType +
                                                       "=\"" + DesicDehum(DesicDehumNum).Name + "\"");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state,
                                                  format("...Iteration limit [{}] exceeded in calculating hot water mass flow rate", SolveMaxIter));
                            }
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                format("CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit [{}]) for {}=\"{}\"",
                                       SolveMaxIter,
                                       DesicDehum(DesicDehumNum).DehumType,
                                       DesicDehum(DesicDehumNum).Name),
                                DesicDehum(DesicDehumNum).HotWaterCoilMaxIterIndex);
                        } else if (SolFlag == -2) {
                            if (DesicDehum(DesicDehumNum).HotWaterCoilMaxIterIndex2 == 0) {
                                ShowWarningMessage(state,
                                                   "CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for " +
                                                       DesicDehum(DesicDehumNum).DehumType + "=\"" + DesicDehum(DesicDehumNum).Name + "\"");
                                ShowContinueErrorTimeStamp(state, "");
                                ShowContinueError(state, "...Bad hot water maximum flow rate limits");
                                ShowContinueError(state, format("...Given minimum water flow rate={:.3R} kg/s", MinWaterFlow));
                                ShowContinueError(state, format("...Given maximum water flow rate={:.3R} kg/s", MaxHotWaterFlow));
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           "CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for " +
                                                               DesicDehum(DesicDehumNum).DehumType + "=\"" + DesicDehum(DesicDehumNum).Name + "\"",
                                                           DesicDehum(DesicDehumNum).HotWaterCoilMaxIterIndex2,
                                                           MaxHotWaterFlow,
                                                           MinWaterFlow,
                                                           _,
                                                           "[kg/s]",
                                                           "[kg/s]");
                        }

                        RegenCoilActual = RegenCoilLoad;
                        // simulate the regenerator hot water heating coil
                        SimulateWaterCoilComponents(state,
                                                    DesicDehum(DesicDehumNum).RegenCoilName,
                                                    FirstHVACIteration,
                                                    DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                    RegenCoilActual);
                    }
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = DesicDehum(DesicDehumNum).MaxCoilFluidFlow;
                    SetComponentFlowRate(state,
                                         mdot,
                                         DesicDehum(DesicDehumNum).CoilControlNode,
                                         DesicDehum(DesicDehumNum).CoilOutletNode,
                                         DesicDehum(DesicDehumNum).LoopNum,
                                         DesicDehum(DesicDehumNum).LoopSide,
                                         DesicDehum(DesicDehumNum).BranchNum,
                                         DesicDehum(DesicDehumNum).CompNum);
                    // simulate the regenerator steam heating coil
                    SimulateSteamCoilComponents(state,
                                                DesicDehum(DesicDehumNum).RegenCoilName,
                                                FirstHVACIteration,
                                                DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                RegenCoilLoad,
                                                RegenCoilActual);
                }
            }
        } else {
            {
                auto const SELECT_CASE_var(DesicDehum(DesicDehumNum).RegenCoilType_Num);
                if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric)) {
                    SimulateHeatingCoilComponents(state,
                                                  DesicDehum(DesicDehumNum).RegenCoilName,
                                                  FirstHVACIteration,
                                                  RegenCoilLoad,
                                                  DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                  RegenCoilActual);
                } else if (SELECT_CASE_var == Coil_HeatingWater) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         DesicDehum(DesicDehumNum).CoilControlNode,
                                         DesicDehum(DesicDehumNum).CoilOutletNode,
                                         DesicDehum(DesicDehumNum).LoopNum,
                                         DesicDehum(DesicDehumNum).LoopSide,
                                         DesicDehum(DesicDehumNum).BranchNum,
                                         DesicDehum(DesicDehumNum).CompNum);
                    RegenCoilActual = RegenCoilLoad;
                    // simulate the regenerator hot water heating coil
                    SimulateWaterCoilComponents(state,
                                                DesicDehum(DesicDehumNum).RegenCoilName,
                                                FirstHVACIteration,
                                                DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                RegenCoilActual);
                } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                    mdot = 0.0;
                    SetComponentFlowRate(state,
                                         mdot,
                                         DesicDehum(DesicDehumNum).CoilControlNode,
                                         DesicDehum(DesicDehumNum).CoilOutletNode,
                                         DesicDehum(DesicDehumNum).LoopNum,
                                         DesicDehum(DesicDehumNum).LoopSide,
                                         DesicDehum(DesicDehumNum).BranchNum,
                                         DesicDehum(DesicDehumNum).CompNum);
                    // simulate the regenerator steam heating coil
                    SimulateSteamCoilComponents(state,
                                                DesicDehum(DesicDehumNum).RegenCoilName,
                                                FirstHVACIteration,
                                                DesicDehum(DesicDehumNum).RegenCoilIndex,
                                                RegenCoilLoad,
                                                RegenCoilActual);
                }
            }
        }
        if (present(RegenCoilLoadmet)) RegenCoilLoadmet = RegenCoilActual;
    }

    Real64 HotWaterCoilResidual(EnergyPlusData &state,
                                Real64 const HWFlow,             // hot water flow rate in kg/s
                                std::array<Real64, 3> const &Par // Par(5) is the requested coil load
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   January 2012
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (RegenCoilActual - RegenCoilHeatLoad) / RegenCoilHeatLoad
        // coil actual output depends on the hot water flow rate which is varied to minimize the residual

        // METHODOLOGY EMPLOYED:
        // Calls HotWaterCoilResidual, and calculates the residual as defined above.

        // REFERENCES:

        // Using/Aliasing
        using PlantUtilities::SetComponentFlowRate;
        using WaterCoils::SimulateWaterCoilComponents;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int DesicDehumNum;
        bool FirstHVACSoln;
        Real64 RegenCoilActual;   // delivered coild load, W
        Real64 RegenCoilHeatLoad; // requested coild load, W
        Real64 mdot;

        DesicDehumNum = int(Par[0]);
        FirstHVACSoln = (Par[1] > 0.0);
        RegenCoilHeatLoad = Par[2];
        RegenCoilActual = RegenCoilHeatLoad;
        mdot = HWFlow;
        SetComponentFlowRate(state,
                             mdot,
                             state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).CoilControlNode,
                             state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).CoilOutletNode,
                             state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).LoopNum,
                             state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).LoopSide,
                             state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).BranchNum,
                             state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).CompNum);

        // simulate the hot water regenerator heating coil
        SimulateWaterCoilComponents(state,
                                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).RegenCoilName,
                                    FirstHVACSoln,
                                    state.dataDesiccantDehumidifiers->DesicDehum(DesicDehumNum).RegenCoilIndex,
                                    RegenCoilActual);
        if (RegenCoilHeatLoad != 0.0) {
            Residuum = (RegenCoilActual - RegenCoilHeatLoad) / RegenCoilHeatLoad;
        } else { // Autodesk:Return ELSE added to assure return value is set
            Residuum = 0.0;
        }
        return Residuum;
    }

    int GetProcAirInletNodeNum(EnergyPlusData &state, std::string const &DesicDehumName, bool &ErrorsFound)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given Desiccant Dehumidifier and returns the process air inlet node number.
        // If incorrect Desiccant Dehumidifier name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int NodeNum; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichDesicDehum;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier) {
            GetDesiccantDehumidifierInput(state);
            state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier = false;
        }

        WhichDesicDehum = UtilityRoutines::FindItemInList(DesicDehumName, state.dataDesiccantDehumidifiers->DesicDehum);
        if (WhichDesicDehum != 0) {
            NodeNum = state.dataDesiccantDehumidifiers->DesicDehum(WhichDesicDehum).ProcAirInNode;
        } else {
            ShowSevereError(state, "GetProcAirInletNodeNum: Could not find Desciccant Dehumidifier = \"" + DesicDehumName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int GetProcAirOutletNodeNum(EnergyPlusData &state, std::string const &DesicDehumName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given Desiccant Dehumidifier and returns the process air outlet node number.
        // If incorrect Desiccant Dehumidifier name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int NodeNum; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichDesicDehum;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier) {
            GetDesiccantDehumidifierInput(state);
            state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier = false;
        }

        WhichDesicDehum = UtilityRoutines::FindItemInList(DesicDehumName, state.dataDesiccantDehumidifiers->DesicDehum);
        if (WhichDesicDehum != 0) {
            NodeNum = state.dataDesiccantDehumidifiers->DesicDehum(WhichDesicDehum).ProcAirOutNode;
        } else {
            ShowSevereError(state, "GetProcAirInletNodeNum: Could not find Desciccant Dehumidifier = \"" + DesicDehumName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int GetRegAirInletNodeNum(EnergyPlusData &state, std::string const &DesicDehumName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given Desiccant Dehumidifier and returns the regeneration air inlet node number.
        // If incorrect Desiccant Dehumidifier name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int NodeNum; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichDesicDehum;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier) {
            GetDesiccantDehumidifierInput(state);
            state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier = false;
        }

        WhichDesicDehum = UtilityRoutines::FindItemInList(DesicDehumName, state.dataDesiccantDehumidifiers->DesicDehum);
        if (WhichDesicDehum != 0) {
            NodeNum = state.dataDesiccantDehumidifiers->DesicDehum(WhichDesicDehum).RegenAirInNode;
        } else {
            ShowSevereError(state, "GetRegAirInletNodeNum: Could not find Desciccant Dehumidifier = \"" + DesicDehumName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int GetRegAirOutletNodeNum(EnergyPlusData &state, std::string const &DesicDehumName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given Desiccant Dehumidifier and returns the regeneration air outlet node number.
        // If incorrect Desiccant Dehumidifier name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int NodeNum; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichDesicDehum;

        // Obtains and Allocates heat exchanger related parameters from input file
        if (state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier) {
            GetDesiccantDehumidifierInput(state);
            state.dataDesiccantDehumidifiers->GetInputDesiccantDehumidifier = false;
        }

        WhichDesicDehum = UtilityRoutines::FindItemInList(DesicDehumName, state.dataDesiccantDehumidifiers->DesicDehum);
        if (WhichDesicDehum != 0) {
            NodeNum = state.dataDesiccantDehumidifiers->DesicDehum(WhichDesicDehum).RegenAirOutNode;
        } else {
            ShowSevereError(state, "GetRegAirOutletNodeNum: Could not find Desciccant Dehumidifier = \"" + DesicDehumName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    //        End of Reporting subroutines for the SimAir Module
    // *****************************************************************************

    //                                 COPYRIGHT NOTICE

    //     Portions Copyright (c) Gas Research Institute 2001.  All rights reserved.

    //     GRI LEGAL NOTICE
    //     Neither GRI, members of GRI nor any person or organization acting on behalf
    //     of either:

    //     A. Makes any warranty of representation, express or implied with respect to
    //        the accuracy, completness, or usefulness of the information contained in
    //        in this program, including any warranty of merchantability or fitness of
    //        any purpose with respoect to the program, or that the use of any
    //        information disclosed in this program may not infringe privately-owned
    //        rights, or

    //     B.  Assumes any liability with respoct to the use of, or for any and all
    //         damages resulting from the use of the program or any portion thereof or
    //         any information disclosed therein.

} // namespace DesiccantDehumidifiers

} // namespace EnergyPlus
