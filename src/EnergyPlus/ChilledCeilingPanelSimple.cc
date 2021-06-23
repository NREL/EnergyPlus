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
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChilledCeilingPanelSimple.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>

namespace EnergyPlus::CoolingPanelSimple {

// Module -- (ref: Object: ZoneHVAC:CoolingPanel:RadiantConvective:Water)

// Module containing the routines dealing with the simple (chilled ceiling) cooling panels

// MODULE INFORMATION:
//       AUTHOR         Rick Strand
//       DATE WRITTEN   Aug 2014

// PURPOSE OF THIS MODULE:
// The purpose of this module is to simulate simple chilled ceiling panels.  It is similar to
// hot water radiant/convective baseboard units and the code for this model used that model as
// a starting point.

// REFERENCES:
// Existing code for hot water baseboard models (radiant-convective variety)

// USE STATEMENTS:
// MODULE PARAMETER DEFINITIONS
std::string const cCMO_CoolingPanel_Simple("ZoneHVAC:CoolingPanel:RadiantConvective:Water");

void SimCoolingPanel(EnergyPlusData &state,
                     std::string const &EquipName,
                     int const ActualZoneNum,
                     int const ControlledZoneNum,
                     bool const FirstHVACIteration,
                     Real64 &PowerMet,
                     int &CompIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Aug 2014

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the simple cooling (chilled ceiling) panel.  It borrows heavily
    // from the hot water radiant-convective baseboard model code.

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)

    // Using/Aliasing
    using DataPlant::TypeOf_CoolingPanel_Simple;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoolingPanelNum; // Index of unit in baseboard array
    Real64 QZnReq;       // Zone load not yet satisfied
    Real64 MaxWaterFlow;
    Real64 MinWaterFlow;

    if (state.dataChilledCeilingPanelSimple->GetInputFlag) {
        GetCoolingPanelInput(state);
        state.dataChilledCeilingPanelSimple->GetInputFlag = false;
    }

    // Find the correct Baseboard Equipment
    if (CompIndex == 0) {
        CoolingPanelNum = UtilityRoutines::FindItemInList(EquipName,
                                                          state.dataChilledCeilingPanelSimple->CoolingPanel,
                                                          &CoolingPanelParams::EquipID,
                                                          state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        if (CoolingPanelNum == 0) {
            ShowFatalError(state, "SimCoolingPanelSimple: Unit not found=" + EquipName);
        }
        CompIndex = CoolingPanelNum;
    } else {
        CoolingPanelNum = CompIndex;
        if (CoolingPanelNum > state.dataChilledCeilingPanelSimple->NumCoolingPanels || CoolingPanelNum < 1) {
            ShowFatalError(state,
                           format("SimCoolingPanelSimple:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  CoolingPanelNum,
                                  state.dataChilledCeilingPanelSimple->NumCoolingPanels,
                                  EquipName));
        }
        if (state.dataChilledCeilingPanelSimple->CheckEquipName(CoolingPanelNum)) {
            if (EquipName != state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID) {
                ShowFatalError(state,
                               format("SimCoolingPanelSimple: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      CoolingPanelNum,
                                      EquipName,
                                      state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID));
            }
            state.dataChilledCeilingPanelSimple->CheckEquipName(CoolingPanelNum) = false;
        }
    }

    if (CompIndex > 0) {

        auto &ThisCP(state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum));

        InitCoolingPanel(state, CoolingPanelNum, ControlledZoneNum, FirstHVACIteration);

        QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToCoolSP;

        // On the first HVAC iteration the system values are given to the controller, but after that
        // the demand limits are in place and there needs to be feedback to the Zone Equipment
        if (FirstHVACIteration) {
            MaxWaterFlow = ThisCP.WaterMassFlowRateMax;
            MinWaterFlow = 0.0;
        } else {
            MaxWaterFlow = state.dataLoopNodes->Node(ThisCP.WaterInletNode).MassFlowRateMaxAvail;
            MinWaterFlow = state.dataLoopNodes->Node(ThisCP.WaterInletNode).MassFlowRateMinAvail;
        }

        {
            auto const SELECT_CASE_var(ThisCP.EquipType);

            if (SELECT_CASE_var == TypeOf_CoolingPanel_Simple) { // 'ZoneHVAC:CoolingPanel:RadiantConvective:Water'
                ThisCP.CalcCoolingPanel(state, CoolingPanelNum);
            } else {
                ShowSevereError(state,
                                "SimCoolingPanelSimple: Errors in CoolingPanel=" +
                                    state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
                ShowContinueError(state,
                                  format("Invalid or unimplemented equipment type={}",
                                         state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipType));
                ShowFatalError(state, "Preceding condition causes termination.");
            }
        }

        PowerMet = ThisCP.TotPower;

        UpdateCoolingPanel(state, CoolingPanelNum);

        state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).ReportCoolingPanel(state);

    } else {
        ShowFatalError(state, "SimCoolingPanelSimple: Unit not found=" + EquipName);
    }
}

void GetCoolingPanelInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Aug 2014

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the input for the simple cooling panel units.

    // METHODOLOGY EMPLOYED:
    // Standard input processor calls--started from Daeho's radiant-convective water baseboard model.

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using DataLoopNode::ObjectIsNotParent;
    using DataPlant::TypeOf_CoolingPanel_Simple;
    using NodeInputManager::GetOnlySingleNode;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetCoolingPanelInput:");
    Real64 const MaxFraction(1.0);
    Real64 const MinFraction(0.0);
    Real64 const MaxWaterTempAvg(30.0);       // Maximum limit of average water temperature in degree C
    Real64 const MinWaterTempAvg(0.0);        // Minimum limit of average water temperature in degree C
    Real64 const MaxWaterFlowRate(10.0);      // Maximum limit of water volume flow rate in m3/s
    Real64 const MinWaterFlowRate(0.00001);   // Minimum limit of water volume flow rate in m3/s
    Real64 const WaterMassFlowDefault(0.063); // Default water mass flow rate in kg/s
    int const MinDistribSurfaces(1);          // Minimum number of surfaces that a baseboard heater can radiate to
    Real64 const MinThrottlingRange(0.5);     // Smallest throttling range allowed in degrees Celsius
    static std::string const MeanAirTemperature("MeanAirTemperature");
    static std::string const MeanRadiantTemperature("MeanRadiantTemperature");
    static std::string const OperativeTemperature("OperativeTemperature");
    static std::string const OutsideAirDryBulbTemperature("OutdoorDryBulbTemperature");
    static std::string const OutsideAirWetBulbTemperature("OutdoorWetBulbTemperature");
    static std::string const ZoneTotalLoad("ZoneTotalLoad");
    static std::string const ZoneConvectiveLoad("ZoneConvectiveLoad");
    static std::string const Off("Off");
    static std::string const SimpleOff("SimpleOff");
    static std::string const VariableOff("VariableOff");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AllFracsSummed; // Sum of the fractions radiant
    int CoolingPanelNum;   // Cooling panel number
    int CoolPanelNumI;     // For loop index
    int NumAlphas;         // Number of Alphas for each GetobjectItem call
    int NumNumbers;        // Number of Numbers for each GetobjectItem call
    int SurfNum;           // Surface number Do loop counter
    int IOStat;
    bool ErrorsFound(false); // If errors detected in input
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    state.dataChilledCeilingPanelSimple->NumCoolingPanels =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCMO_CoolingPanel_Simple);

    // Count total number of baseboard units

    state.dataChilledCeilingPanelSimple->CoolingPanel.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
    state.dataChilledCeilingPanelSimple->CoolingPanelSysNumericFields.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
    state.dataChilledCeilingPanelSimple->CheckEquipName.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
    state.dataChilledCeilingPanelSimple->CheckEquipName = true;

    // Get the data from the user input related to cooling panels
    for (CoolingPanelNum = 1; CoolingPanelNum <= state.dataChilledCeilingPanelSimple->NumCoolingPanels; ++CoolingPanelNum) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCMO_CoolingPanel_Simple,
                                                                 CoolingPanelNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataChilledCeilingPanelSimple->CoolingPanelSysNumericFields(CoolingPanelNum).FieldNames.allocate(NumNumbers);
        state.dataChilledCeilingPanelSimple->CoolingPanelSysNumericFields(CoolingPanelNum).FieldNames = "";
        state.dataChilledCeilingPanelSimple->CoolingPanelSysNumericFields(CoolingPanelNum).FieldNames = state.dataIPShortCut->cNumericFieldNames;

        if (CoolingPanelNum > 1) {
            for (CoolPanelNumI = 2; CoolPanelNumI <= state.dataChilledCeilingPanelSimple->NumCoolingPanels; ++CoolPanelNumI) {
                if (state.dataIPShortCut->cAlphaArgs(1) == state.dataChilledCeilingPanelSimple->CoolingPanel(CoolPanelNumI).EquipID) {
                    ErrorsFound = true;
                    ShowSevereError(state, state.dataIPShortCut->cAlphaArgs(1) + " is used as a name for more than one simple COOLING PANEL.");
                    ShowContinueError(state, "This is not allowed.");
                }
            }
        }

        auto &ThisCP(state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum));
        ThisCP.EquipID = state.dataIPShortCut->cAlphaArgs(1); // Name of this simple cooling panel
        ThisCP.EquipType = TypeOf_CoolingPanel_Simple;        //'ZoneHVAC:CoolingPanel:RadiantConvective:Water'

        // Get schedule
        ThisCP.Schedule = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            ThisCP.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            ThisCP.SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (ThisCP.SchedPtr == 0) {
                ShowSevereError(state,
                                RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\" not found.");
                ErrorsFound = true;
            }
        }

        // Get inlet node number
        ThisCP.WaterInletNode = GetOnlySingleNode(state,
                                                  state.dataIPShortCut->cAlphaArgs(3),
                                                  ErrorsFound,
                                                  cCMO_CoolingPanel_Simple,
                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                  DataLoopNode::NodeFluidType::Water,
                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                  NodeInputManager::compFluidStream::Primary,
                                                  ObjectIsNotParent);

        // Get outlet node number
        ThisCP.WaterOutletNode = GetOnlySingleNode(state,
                                                   state.dataIPShortCut->cAlphaArgs(4),
                                                   ErrorsFound,
                                                   cCMO_CoolingPanel_Simple,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   DataLoopNode::NodeFluidType::Water,
                                                   DataLoopNode::NodeConnectionType::Outlet,
                                                   NodeInputManager::compFluidStream::Primary,
                                                   ObjectIsNotParent);
        TestCompSet(state,
                    cCMO_CoolingPanel_Simple,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(3),
                    state.dataIPShortCut->cAlphaArgs(4),
                    "Chilled Water Nodes");

        ThisCP.RatedWaterTemp = state.dataIPShortCut->rNumericArgs(1);
        if (ThisCP.RatedWaterTemp > MaxWaterTempAvg + 0.001) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(1) + " was higher than the allowable maximum.");
            ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxWaterTempAvg));
            ThisCP.RatedWaterTemp = MaxWaterTempAvg;
        } else if (ThisCP.RatedWaterTemp < MinWaterTempAvg - 0.001) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(1) + " was lower than the allowable minimum.");
            ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinWaterTempAvg));
            ThisCP.RatedWaterTemp = MinWaterTempAvg;
        }

        ThisCP.RatedZoneAirTemp = state.dataIPShortCut->rNumericArgs(2);
        if (ThisCP.RatedZoneAirTemp > MaxWaterTempAvg + 0.001) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(2) + " was higher than the allowable maximum.");
            ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxWaterTempAvg));
            ThisCP.RatedZoneAirTemp = MaxWaterTempAvg;
        } else if (ThisCP.RatedZoneAirTemp < MinWaterTempAvg - 0.001) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(2) + " was lower than the allowable minimum.");
            ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinWaterTempAvg));
            ThisCP.RatedZoneAirTemp = MinWaterTempAvg;
        }

        ThisCP.RatedWaterFlowRate = state.dataIPShortCut->rNumericArgs(3);
        if (ThisCP.RatedWaterFlowRate < 0.00001 || ThisCP.RatedWaterFlowRate > 10.0) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(2) + " is an invalid Standard Water mass flow rate.");
            ShowContinueError(state, format("...reset to a default value=[{:.1R}].", WaterMassFlowDefault));
            ThisCP.RatedWaterFlowRate = WaterMassFlowDefault;
        }

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "CoolingDesignCapacity")) {
            ThisCP.CoolingCapMethod = DataSizing::CoolingDesignCapacity;
            if (!state.dataIPShortCut->lNumericFieldBlanks(4)) {
                ThisCP.ScaledCoolingCapacity = state.dataIPShortCut->rNumericArgs(4);
                if (ThisCP.ScaledCoolingCapacity < 0.0 && ThisCP.ScaledCoolingCapacity != DataSizing::AutoSize) {
                    ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                    ShowContinueError(
                        state, format("Illegal {} = {:.7T}", state.dataIPShortCut->cNumericFieldNames(4), state.dataIPShortCut->rNumericArgs(4)));
                    ErrorsFound = true;
                }
            } else {
                if ((!state.dataIPShortCut->lAlphaFieldBlanks(6)) || (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
                    ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                    ShowContinueError(state, "Input for " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(4));
                    ErrorsFound = true;
                }
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "CapacityPerFloorArea")) {
            ThisCP.CoolingCapMethod = DataSizing::CapacityPerFloorArea;
            if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
                ThisCP.ScaledCoolingCapacity = state.dataIPShortCut->rNumericArgs(5);
                if (ThisCP.CoolingCapMethod <= 0.0) {
                    ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                    ShowContinueError(state, "Input for " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(
                        state, format("Illegal {} = {:.7T}", state.dataIPShortCut->cNumericFieldNames(5), state.dataIPShortCut->rNumericArgs(5)));
                    ErrorsFound = true;
                } else if (ThisCP.ScaledCoolingCapacity == DataSizing::AutoSize) {
                    ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                    ShowContinueError(state, "Input for " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state, "Illegal " + state.dataIPShortCut->cNumericFieldNames(5) + " = Autosize");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                ShowContinueError(state, "Input for " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                ShowContinueError(state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(5));
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "FractionOfAutosizedCoolingCapacity")) {
            ThisCP.CoolingCapMethod = DataSizing::FractionOfAutosizedCoolingCapacity;
            if (!state.dataIPShortCut->lNumericFieldBlanks(6)) {
                ThisCP.ScaledCoolingCapacity = state.dataIPShortCut->rNumericArgs(6);
                if (ThisCP.ScaledCoolingCapacity < 0.0) {
                    ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                    ShowContinueError(
                        state, format("Illegal {} = {:.7T}", state.dataIPShortCut->cNumericFieldNames(6), state.dataIPShortCut->rNumericArgs(6)));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                ShowContinueError(state, "Input for " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                ShowContinueError(state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(6));
                ErrorsFound = true;
            }
        } else {
            ShowSevereError(state, cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
            ShowContinueError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
            ErrorsFound = true;
        }

        ThisCP.WaterVolFlowRateMax = state.dataIPShortCut->rNumericArgs(7);
        if ((ThisCP.WaterVolFlowRateMax <= MinWaterFlowRate) && ThisCP.WaterVolFlowRateMax != DataSizing::AutoSize) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(7) + " was less than the allowable minimum.");
            ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinWaterFlowRate));
            ThisCP.WaterVolFlowRateMax = MinWaterFlowRate;
        } else if (ThisCP.WaterVolFlowRateMax > MaxWaterFlowRate) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(7) + " was higher than the allowable maximum.");
            ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxWaterFlowRate));
            ThisCP.WaterVolFlowRateMax = MaxWaterFlowRate;
        }

        // Process the temperature control type
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), MeanAirTemperature)) {
            ThisCP.ControlType = Control::MAT;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), MeanRadiantTemperature)) {
            ThisCP.ControlType = Control::MRT;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), OperativeTemperature)) {
            ThisCP.ControlType = Control::Operative;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), OutsideAirDryBulbTemperature)) {
            ThisCP.ControlType = Control::ODB;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), OutsideAirWetBulbTemperature)) {
            ThisCP.ControlType = Control::OWB;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), ZoneTotalLoad)) {
            ThisCP.ControlType = Control::ZoneTotalLoad;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), ZoneConvectiveLoad)) {
            ThisCP.ControlType = Control::ZoneConvectiveLoad;
        } else {
            ShowWarningError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + " =" + state.dataIPShortCut->cAlphaArgs(6));
            ShowContinueError(state, "Occurs in " + RoutineName + " = " + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Control reset to MAT control for this Simple Cooling Panel.");
            ThisCP.ControlType = Control::MAT;
        }

        ThisCP.ColdThrottlRange = state.dataIPShortCut->rNumericArgs(8);
        if (ThisCP.ColdThrottlRange < MinThrottlingRange) {
            ShowWarningError(state, cCMO_CoolingPanel_Simple + "Cooling throttling range too small, reset to 0.5");
            ShowContinueError(state, "Occurs in Cooling Panel=" + ThisCP.EquipID);
            ThisCP.ColdThrottlRange = MinThrottlingRange;
        }

        ThisCP.ColdSetptSched = state.dataIPShortCut->cAlphaArgs(7);
        ThisCP.ColdSetptSchedPtr = GetScheduleIndex(state, ThisCP.ColdSetptSched);
        if ((ThisCP.ColdSetptSchedPtr == 0) && (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
            ShowSevereError(state, state.dataIPShortCut->cAlphaFieldNames(7) + " not found: " + ThisCP.ColdSetptSched);
            ShowContinueError(state, "Occurs in " + RoutineName + " = " + state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), Off)) {
            ThisCP.CondCtrlType = CondCtrl::NONE;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), SimpleOff)) {
            ThisCP.CondCtrlType = CondCtrl::SIMPLEOFF;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), VariableOff)) {
            ThisCP.CondCtrlType = CondCtrl::VARIEDOFF;
        } else {
            ThisCP.CondCtrlType = CondCtrl::SIMPLEOFF;
        }

        ThisCP.CondDewPtDeltaT = state.dataIPShortCut->rNumericArgs(9);

        ThisCP.FracRadiant = state.dataIPShortCut->rNumericArgs(10);
        if (ThisCP.FracRadiant < MinFraction) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(10) + " was lower than the allowable minimum.");
            ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinFraction));
            ThisCP.FracRadiant = MinFraction;
        }
        if (ThisCP.FracRadiant > MaxFraction) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(10) + " was higher than the allowable maximum.");
            ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
            ThisCP.FracRadiant = MaxFraction;
        }

        // Remaining fraction is added to the zone as convective heat transfer
        AllFracsSummed = ThisCP.FracRadiant;
        if (AllFracsSummed > MaxFraction) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                 "\", Fraction Radiant was higher than the allowable maximum.");
            ThisCP.FracRadiant = MaxFraction;
            ThisCP.FracConvect = 0.0;
        } else {
            ThisCP.FracConvect = 1.0 - AllFracsSummed;
        }

        ThisCP.FracDistribPerson = state.dataIPShortCut->rNumericArgs(11);
        if (ThisCP.FracDistribPerson < MinFraction) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(11) + " was lower than the allowable minimum.");
            ShowContinueError(state, format("...reset to minimum value=[{:.3R}].", MinFraction));
            ThisCP.FracDistribPerson = MinFraction;
        }
        if (ThisCP.FracDistribPerson > MaxFraction) {
            ShowWarningError(state,
                             RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                 state.dataIPShortCut->cNumericFieldNames(11) + " was higher than the allowable maximum.");
            ShowContinueError(state, format("...reset to maximum value=[{:.3R}].", MaxFraction));
            ThisCP.FracDistribPerson = MaxFraction;
        }

        ThisCP.TotSurfToDistrib = NumNumbers - 11;
        if ((ThisCP.TotSurfToDistrib < MinDistribSurfaces) && (ThisCP.FracRadiant > MinFraction)) {
            ShowSevereError(state,
                            RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", the number of surface/radiant fraction groups entered was less than the allowable minimum.");
            ShowContinueError(state, format("...the minimum that must be entered=[{}].", MinDistribSurfaces));
            ErrorsFound = true;
            ThisCP.TotSurfToDistrib = 0; // error
        }

        ThisCP.SurfaceName.allocate(ThisCP.TotSurfToDistrib);
        ThisCP.SurfaceName = "";
        ThisCP.SurfacePtr.allocate(ThisCP.TotSurfToDistrib);
        ThisCP.SurfacePtr = 0;
        ThisCP.FracDistribToSurf.allocate(ThisCP.TotSurfToDistrib);
        ThisCP.FracDistribToSurf = 0.0;

        // search zone equipment list structure for zone index
        for (int ctrlZone = 1; ctrlZone <= state.dataGlobal->NumOfZones; ++ctrlZone) {
            for (int zoneEquipTypeNum = 1; zoneEquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ctrlZone).NumOfEquipTypes; ++zoneEquipTypeNum) {
                if (state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipType_Num(zoneEquipTypeNum) == DataZoneEquipment::CoolingPanel_Num &&
                    state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipName(zoneEquipTypeNum) == ThisCP.EquipID) {
                    ThisCP.ZonePtr = ctrlZone;
                }
            }
        }
        if (ThisCP.ZonePtr <= 0) {
            ShowSevereError(state, RoutineName + cCMO_CoolingPanel_Simple + "=\"" + ThisCP.EquipID + "\" is not on any ZoneHVAC:EquipmentList.");
            ErrorsFound = true;
            continue;
        }

        AllFracsSummed = ThisCP.FracDistribPerson;
        for (SurfNum = 1; SurfNum <= ThisCP.TotSurfToDistrib; ++SurfNum) {
            ThisCP.SurfaceName(SurfNum) = state.dataIPShortCut->cAlphaArgs(SurfNum + 8);
            ThisCP.SurfacePtr(SurfNum) = HeatBalanceIntRadExchange::GetRadiantSystemSurface(
                state, cCMO_CoolingPanel_Simple, ThisCP.EquipID, ThisCP.ZonePtr, ThisCP.SurfaceName(SurfNum), ErrorsFound);
            ThisCP.FracDistribToSurf(SurfNum) = state.dataIPShortCut->rNumericArgs(SurfNum + 11);
            if (ThisCP.FracDistribToSurf(SurfNum) > MaxFraction) {
                ShowWarningError(state,
                                 RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                     state.dataIPShortCut->cNumericFieldNames(SurfNum + 8) + "was greater than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
                ThisCP.TotSurfToDistrib = MaxFraction;
            }
            if (ThisCP.FracDistribToSurf(SurfNum) < MinFraction) {
                ShowWarningError(state,
                                 RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                     state.dataIPShortCut->cNumericFieldNames(SurfNum + 8) + "was less than the allowable minimum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MinFraction));
                ThisCP.TotSurfToDistrib = MinFraction;
            }
            if (ThisCP.SurfacePtr(SurfNum) != 0) {
                state.dataSurface->SurfIntConvSurfGetsRadiantHeat(ThisCP.SurfacePtr(SurfNum)) = true;
            }

            AllFracsSummed += ThisCP.FracDistribToSurf(SurfNum);
        } // Surfaces

        if (AllFracsSummed > (MaxFraction + 0.01)) {
            ShowSevereError(state,
                            RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", Summed radiant fractions for people + surface groups > 1.0");
            ErrorsFound = true;
        }
        if ((AllFracsSummed < (MaxFraction - 0.01)) &&
            (ThisCP.FracRadiant > MinFraction)) { // User didn't distribute all of the | radiation warn that some will be lost
            ShowSevereError(state,
                            RoutineName + cCMO_CoolingPanel_Simple + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", Summed radiant fractions for people + surface groups < 1.0");
            ShowContinueError(state, "This would result in some of the radiant energy delivered by the high temp radiant heater being lost.");
            ShowContinueError(state, format("The sum of all radiation fractions to surfaces = {:.5T}", (AllFracsSummed - ThisCP.FracDistribPerson)));
            ShowContinueError(state, format("The radiant fraction to people = {:.5T}", ThisCP.FracDistribPerson));
            ShowContinueError(state, format("So, all radiant fractions including surfaces and people = {:.5T}", AllFracsSummed));
            ShowContinueError(state,
                              format("This means that the fraction of radiant energy that would be lost from the high temperature radiant heater "
                                     "would be = {:.5T}",
                                     (1.0 - AllFracsSummed)));
            ShowContinueError(state,
                              "Please check and correct this so that all radiant energy is accounted for in " + cCMO_CoolingPanel_Simple + " = " +
                                  state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, RoutineName + cCMO_CoolingPanel_Simple + "Errors found getting input. Program terminates.");
    }

    // Setup Report variables for the Coils
    for (CoolingPanelNum = 1; CoolingPanelNum <= state.dataChilledCeilingPanelSimple->NumCoolingPanels; ++CoolingPanelNum) {
        // CurrentModuleObject='ZoneHVAC:CoolingPanel:RadiantConvective:Water'
        SetupOutputVariable(state,
                            "Cooling Panel Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).Power,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
        SetupOutputVariable(state,
                            "Cooling Panel Total System Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).TotPower,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
        SetupOutputVariable(state,
                            "Cooling Panel Convective Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).ConvPower,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
        SetupOutputVariable(state,
                            "Cooling Panel Radiant Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).RadPower,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);

        SetupOutputVariable(state,
                            "Cooling Panel Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).Energy,
                            "System",
                            "Sum",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGPANEL",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Panel Total System Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).TotEnergy,
                            "System",
                            "Sum",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGPANEL",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Panel Convective Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).ConvEnergy,
                            "System",
                            "Sum",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
        SetupOutputVariable(state,
                            "Cooling Panel Radiant Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).RadEnergy,
                            "System",
                            "Sum",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);

        SetupOutputVariable(state,
                            "Cooling Panel Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).WaterMassFlowRate,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
        SetupOutputVariable(state,
                            "Cooling Panel Water Inlet Temperature",
                            OutputProcessor::Unit::C,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).WaterInletTemp,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
        SetupOutputVariable(state,
                            "Cooling Panel Water Outlet Temperature",
                            OutputProcessor::Unit::C,
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).WaterOutletTemp,
                            "System",
                            "Average",
                            state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum).EquipID);
    }
}

void InitCoolingPanel(EnergyPlusData &state, int const CoolingPanelNum, int const ControlledZoneNumSub, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Sept 2014

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the cooling panel units, and determines the UA values during simulation.

    // METHODOLOGY EMPLOYED:
    // The initialization subrotines borrowed from other sources and heat exchanger formulation for cooling panel.

    // REFERENCES:
    // Incropera and DeWitt, Fundamentals of Heat and Mass Transfer

    // Using/Aliasing
    using DataZoneEquipment::CheckZoneEquipmentList;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("ChilledCeilingPanelSimple:InitCoolingPanel");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int ZoneNode;
    int ZoneNum;
    Real64 rho; // local fluid density
    Real64 Cp;  // local fluid specific heat
    bool errFlag;

    // Do the one time initializations
    if (state.dataChilledCeilingPanelSimple->MyOneTimeFlag) {

        // Initialize the environment and sizing flags
        state.dataChilledCeilingPanelSimple->MyEnvrnFlag.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->ZeroSourceSumHATsurf.allocate(state.dataGlobal->NumOfZones);
        state.dataChilledCeilingPanelSimple->ZeroSourceSumHATsurf = 0.0;
        state.dataChilledCeilingPanelSimple->CoolingPanelSource.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->CoolingPanelSource = 0.0;
        state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg = 0.0;
        state.dataChilledCeilingPanelSimple->LastCoolingPanelSrc.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->LastCoolingPanelSrc = 0.0;
        state.dataChilledCeilingPanelSimple->LastSysTimeElapsed.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->LastSysTimeElapsed = 0.0;
        state.dataChilledCeilingPanelSimple->LastTimeStepSys.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->LastTimeStepSys = 0.0;
        state.dataChilledCeilingPanelSimple->SetLoopIndexFlag.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->MySizeFlagCoolPanel.allocate(state.dataChilledCeilingPanelSimple->NumCoolingPanels);
        state.dataChilledCeilingPanelSimple->MySizeFlagCoolPanel = true;
        state.dataChilledCeilingPanelSimple->MyEnvrnFlag = true;
        state.dataChilledCeilingPanelSimple->MyOneTimeFlag = false;
        state.dataChilledCeilingPanelSimple->SetLoopIndexFlag = true;
    }

    auto &ThisCP(state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum));
    auto &ThisInNode(state.dataLoopNodes->Node(ThisCP.WaterInletNode));

    if (ThisCP.ZonePtr <= 0) ThisCP.ZonePtr = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ActualZoneNum;

    // Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
    if (!state.dataChilledCeilingPanelSimple->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataChilledCeilingPanelSimple->ZoneEquipmentListChecked = true;
        for (Loop = 1; Loop <= state.dataChilledCeilingPanelSimple->NumCoolingPanels; ++Loop) {
            if (CheckZoneEquipmentList(state, cCMO_CoolingPanel_Simple, ThisCP.EquipID)) continue;
            ShowSevereError(state,
                            "InitCoolingPanel: Unit=[" + cCMO_CoolingPanel_Simple + ',' + ThisCP.EquipID +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
        }
    }

    if (state.dataChilledCeilingPanelSimple->SetLoopIndexFlag(CoolingPanelNum)) {
        if (allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    ThisCP.EquipID,
                                    ThisCP.EquipType,
                                    ThisCP.LoopNum,
                                    ThisCP.LoopSideNum,
                                    ThisCP.BranchNum,
                                    ThisCP.CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitCoolingPanel: Program terminated for previous conditions.");
            }
            state.dataChilledCeilingPanelSimple->SetLoopIndexFlag(CoolingPanelNum) = false;
        }
    }

    if (!state.dataGlobal->SysSizingCalc) {
        if (state.dataChilledCeilingPanelSimple->MySizeFlagCoolPanel(CoolingPanelNum) &&
            !state.dataChilledCeilingPanelSimple->SetLoopIndexFlag(CoolingPanelNum)) {
            // for each cooling panel do the sizing once.
            SizeCoolingPanel(state, CoolingPanelNum);
            state.dataChilledCeilingPanelSimple->MySizeFlagCoolPanel(CoolingPanelNum) = false;

            // set design mass flow rates
            if (ThisCP.WaterInletNode > 0) {
                rho = GetDensityGlycol(state,
                                       state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidName,
                                       DataGlobalConstants::CWInitConvTemp,
                                       state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidIndex,
                                       RoutineName);
                ThisCP.WaterMassFlowRateMax = rho * ThisCP.WaterVolFlowRateMax;
                InitComponentNodes(state,
                                   0.0,
                                   ThisCP.WaterMassFlowRateMax,
                                   ThisCP.WaterInletNode,
                                   ThisCP.WaterOutletNode,
                                   ThisCP.LoopNum,
                                   ThisCP.LoopSideNum,
                                   ThisCP.BranchNum,
                                   ThisCP.CompNum);
            }
        }
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataChilledCeilingPanelSimple->MyEnvrnFlag(CoolingPanelNum)) {
        // Initialize

        rho = GetDensityGlycol(state,
                               state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidName,
                               DataGlobalConstants::InitConvTemp,
                               state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidIndex,
                               RoutineName);

        ThisCP.WaterMassFlowRateMax = rho * ThisCP.WaterVolFlowRateMax;

        InitComponentNodes(state,
                           0.0,
                           ThisCP.WaterMassFlowRateMax,
                           ThisCP.WaterInletNode,
                           ThisCP.WaterOutletNode,
                           ThisCP.LoopNum,
                           ThisCP.LoopSideNum,
                           ThisCP.BranchNum,
                           ThisCP.CompNum);

        ThisInNode.Temp = 7.0;

        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidName,
                                   ThisInNode.Temp,
                                   state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidIndex,
                                   RoutineName);

        ThisInNode.Enthalpy = Cp * ThisInNode.Temp;
        ThisInNode.Quality = 0.0;
        ThisInNode.Press = 0.0;
        ThisInNode.HumRat = 0.0;

        state.dataChilledCeilingPanelSimple->ZeroSourceSumHATsurf = 0.0;
        state.dataChilledCeilingPanelSimple->CoolingPanelSource = 0.0;
        state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg = 0.0;
        state.dataChilledCeilingPanelSimple->LastCoolingPanelSrc = 0.0;
        state.dataChilledCeilingPanelSimple->LastSysTimeElapsed = 0.0;
        state.dataChilledCeilingPanelSimple->LastTimeStepSys = 0.0;

        state.dataChilledCeilingPanelSimple->MyEnvrnFlag(CoolingPanelNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataChilledCeilingPanelSimple->MyEnvrnFlag(CoolingPanelNum) = true;
    }

    if (state.dataGlobal->BeginTimeStepFlag && FirstHVACIteration) {
        ZoneNum = ThisCP.ZonePtr;
        state.dataChilledCeilingPanelSimple->ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(state, ZoneNum);
        state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg(CoolingPanelNum) = 0.0;
        state.dataChilledCeilingPanelSimple->LastCoolingPanelSrc(CoolingPanelNum) = 0.0;
        state.dataChilledCeilingPanelSimple->LastSysTimeElapsed(CoolingPanelNum) = 0.0;
        state.dataChilledCeilingPanelSimple->LastTimeStepSys(CoolingPanelNum) = 0.0;
    }

    // Do the every time step initializations
    ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ZoneNode;
    ThisCP.WaterMassFlowRate = ThisInNode.MassFlowRate;
    ThisCP.WaterInletTemp = ThisInNode.Temp;
    ThisCP.WaterInletEnthalpy = ThisInNode.Enthalpy;
    ThisCP.TotPower = 0.0;
    ThisCP.Power = 0.0;
    ThisCP.ConvPower = 0.0;
    ThisCP.RadPower = 0.0;
    ThisCP.TotEnergy = 0.0;
    ThisCP.Energy = 0.0;
    ThisCP.ConvEnergy = 0.0;
    ThisCP.RadEnergy = 0.0;
}

void SizeCoolingPanel(EnergyPlusData &state, int const CoolingPanelNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Sept 2016

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sizes the simple chilled ceiling panel.  The process used here
    // was derived from the low temperature radiant system model and adapted for
    // cooling only.

    using DataHVACGlobals::AutoCalculateSizing;
    using DataHVACGlobals::CoolingCapacitySizing;
    using DataHVACGlobals::SmallLoad;
    using DataSizing::AutoSize;
    using DataSizing::CapacityPerFloorArea;
    using DataSizing::CoolingDesignCapacity;
    using DataSizing::FractionOfAutosizedCoolingCapacity;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using PlantUtilities::MyPlantSizingIndex;
    using PlantUtilities::RegisterPlantCompDesignFlow;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("SizeCoolingPanel");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound(false);  // If errors detected in input
    std::string CompName;     // component name
    std::string CompType;     // component type
    bool IsAutoSize(false);   // Indicator to autosize
    Real64 DesCoilLoad;       // design autosized or user specified capacity
    int SizingMethod;         // Integer representation of sizing method name (e.g. CoolingCapacitySizing, HeatingCapacitySizing)
    int FieldNum = 1;         // IDD numeric field number where input field description is found
    bool PrintFlag;           // TRUE when sizing information is reported in the eio file
    std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
    Real64 TempSize;          // autosized value of coil input field
    int CapSizingMethod(0);   // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                              // FractionOfAutosizedHeatingCapacity )
    int PltSizCoolNum(0);     // index of plant sizing object for 1st cooling loop
    Real64 rho;
    Real64 Cp;
    Real64 WaterVolFlowMaxCoolDes(0.0);  // Design chilled water flow for reporting
    Real64 WaterVolFlowMaxCoolUser(0.0); // User hard-sized chilled water flow for reporting

    DesCoilLoad = 0.0;
    state.dataSize->DataScalableCapSizingON = false;

    auto &ThisCP(state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum));
    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

    CompType = "ZoneHVAC:CoolingPanel:RadiantConvective:Water";
    CompName = ThisCP.EquipID;

    IsAutoSize = false;
    if (ThisCP.ScaledCoolingCapacity == AutoSize) {
        IsAutoSize = true;
    }

    if (state.dataSize->CurZoneEqNum > 0) {

        SizingMethod = CoolingCapacitySizing;
        FieldNum = 4;
        PrintFlag = true;
        bool errorsFound = false;
        SizingString = state.dataChilledCeilingPanelSimple->CoolingPanelSysNumericFields(CoolingPanelNum).FieldNames(FieldNum) + " [W]";
        CapSizingMethod = ThisCP.CoolingCapMethod;
        ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;

        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // simulation continue
            if (CapSizingMethod == CoolingDesignCapacity && ThisCP.ScaledCoolingCapacity > 0.0) {
                TempSize = ThisCP.ScaledCoolingCapacity;
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                DesCoilLoad = sizerCoolingCapacity.size(state, TempSize, errorsFound);
            } else if (CapSizingMethod == CapacityPerFloorArea) {
                state.dataSize->DataScalableCapSizingON = true;
                TempSize = ThisCP.ScaledCoolingCapacity * state.dataHeatBal->Zone(ThisCP.ZonePtr).FloorArea;
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                DesCoilLoad = sizerCoolingCapacity.size(state, TempSize, errorsFound);
                state.dataSize->DataScalableCapSizingON = false;
            } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                if (ThisCP.WaterVolFlowRateMax == AutoSize) {
                    ShowSevereError(state, RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + ThisCP.EquipID + "\".");
                    ShowContinueError(state,
                                      "The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when the "
                                      "Cooling Design Capacity Method = \"FractionOfAutosizedCoolingCapacity\".");
                    ErrorsFound = true;
                }
            }
        } else { // Autosize or hard-size with sizing run
            if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                if (CapSizingMethod == CoolingDesignCapacity) {
                    if (state.dataSize->ZoneSizingRunDone) {
                        CheckZoneSizing(state, CompType, CompName);
                        SizingMethod = AutoCalculateSizing;
                        state.dataSize->DataConstantUsedForSizing =
                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesCoolLoad;
                        state.dataSize->DataFractionUsedForSizing = 1.0;
                    }
                    if (ThisCP.ScaledCoolingCapacity == AutoSize) {
                        TempSize = AutoSize;
                    } else {
                        TempSize = ThisCP.ScaledCoolingCapacity;
                    }
                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    if (state.dataSize->ZoneSizingRunDone) {
                        CheckZoneSizing(state, CompType, CompName);
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesCoolLoad;
                    }
                    TempSize = ThisCP.ScaledCoolingCapacity * state.dataHeatBal->Zone(ThisCP.ZonePtr).FloorArea;
                    state.dataSize->DataScalableCapSizingON = true;
                } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                    CheckZoneSizing(state, CompType, CompName);
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                        state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesCoolLoad;
                    TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad * ThisCP.ScaledCoolingCapacity;
                    state.dataSize->DataScalableCapSizingON = true;

                } else {
                    TempSize = ThisCP.ScaledCoolingCapacity;
                }
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                DesCoilLoad = sizerCoolingCapacity.size(state, TempSize, errorsFound);
                state.dataSize->DataConstantUsedForSizing = 0.0;
                state.dataSize->DataFractionUsedForSizing = 0.0;
                state.dataSize->DataScalableCapSizingON = false;
            } else {
                DesCoilLoad = 0.0;
            }
        }
        // finally cooling capacity is saved in this variable
        ThisCP.ScaledCoolingCapacity = DesCoilLoad;
    }

    IsAutoSize = false;
    if (ThisCP.WaterVolFlowRateMax == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // simulation continue
            if (ThisCP.WaterVolFlowRateMax > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, CompType, ThisCP.EquipID, "User-Specified Maximum Cold Water Flow [m3/s]", ThisCP.WaterVolFlowRateMax);
            }
        } else { // Autosize or hard-size with sizing run
            if (ThisCP.WaterInletNode > 0 && ThisCP.WaterOutletNode > 0) {
                PltSizCoolNum = MyPlantSizingIndex(state, CompType, ThisCP.EquipID, ThisCP.WaterInletNode, ThisCP.WaterOutletNode, ErrorsFound);
                if (PltSizCoolNum > 0) {
                    if (DesCoilLoad >= SmallLoad) {
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidName,
                                               5.,
                                               state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidIndex,
                                               RoutineName);
                        Cp = GetSpecificHeatGlycol(state,
                                                   state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidName,
                                                   5.0,
                                                   state.dataPlnt->PlantLoop(ThisCP.LoopNum).FluidIndex,
                                                   RoutineName);
                        WaterVolFlowMaxCoolDes = DesCoilLoad / (state.dataSize->PlantSizData(PltSizCoolNum).DeltaT * Cp * rho);
                    } else {
                        WaterVolFlowMaxCoolDes = 0.0;
                    }
                } else {
                    ShowSevereError(state, "Autosizing of water flow requires a cooling loop Sizing:Plant object");
                    ShowContinueError(state, "Occurs in ZoneHVAC:CoolingPanel:RadiantConvective:Water Object=" + ThisCP.EquipID);
                    ErrorsFound = true;
                }
            }

            if (IsAutoSize) {
                ThisCP.WaterVolFlowRateMax = WaterVolFlowMaxCoolDes;
                BaseSizer::reportSizerOutput(state, CompType, ThisCP.EquipID, "Design Size Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolDes);
            } else { // hard-size with sizing data
                if (ThisCP.WaterVolFlowRateMax > 0.0 && WaterVolFlowMaxCoolDes > 0.0) {
                    WaterVolFlowMaxCoolUser = ThisCP.WaterVolFlowRateMax;
                    BaseSizer::reportSizerOutput(state,
                                                 CompType,
                                                 ThisCP.EquipID,
                                                 "Design Size Maximum Cold Water Flow [m3/s]",
                                                 WaterVolFlowMaxCoolDes,
                                                 "User-Specified Maximum Cold Water Flow [m3/s]",
                                                 WaterVolFlowMaxCoolUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(WaterVolFlowMaxCoolDes - WaterVolFlowMaxCoolUser) / WaterVolFlowMaxCoolUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(
                                state,
                                "SizeCoolingPanel: Potential issue with equipment sizing for ZoneHVAC:CoolingPanel:RadiantConvective:Water = \"" +
                                    ThisCP.EquipID + "\".");
                            ShowContinueError(state, format("User-Specified Maximum Cool Water Flow of {:.5R} [m3/s]", WaterVolFlowMaxCoolUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Maximum Cool Water Flow of {:.5R} [m3/s]", WaterVolFlowMaxCoolDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    RegisterPlantCompDesignFlow(state, ThisCP.WaterInletNode, ThisCP.WaterVolFlowRateMax);

    bool SizeCoolingPanelUASuccess;
    SizeCoolingPanelUASuccess = ThisCP.SizeCoolingPanelUA(state);
    if (!SizeCoolingPanelUASuccess) ShowFatalError(state, "SizeCoolingPanelUA: Program terminated for previous conditions.");
}

bool CoolingPanelParams::SizeCoolingPanelUA(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   June 2017

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sizes UA value for the simple chilled ceiling panel.

    // Return value
    bool SizeCoolingPanelUA;

    // These initializations are mainly the calculation of the UA value for the heat exchanger formulation of the simple cooling panel
    Real64 Cp;
    Real64 MDot;
    Real64 MDotXCp;
    Real64 Qrated;
    Real64 Tinletr;
    Real64 Tzoner;
    Real64 RatCapToTheoMax; // Ratio of unit capacity to theoretical maximum output based on rated parameters

    SizeCoolingPanelUA = true;
    Cp = 4120.0; // Just an approximation, don't need to get an exact number
    MDot = this->RatedWaterFlowRate;
    MDotXCp = Cp * MDot;
    Qrated = this->ScaledCoolingCapacity;
    Tinletr = this->RatedWaterTemp;
    Tzoner = this->RatedZoneAirTemp;
    if (std::abs(Tinletr - Tzoner) < 0.5) {
        RatCapToTheoMax = std::abs(Qrated) / (MDotXCp * 0.5); // Avoid a divide by zero error
    } else {
        RatCapToTheoMax = std::abs(Qrated) / (MDotXCp * std::abs(Tinletr - Tzoner));
    }
    if ((RatCapToTheoMax < 1.1) && (RatCapToTheoMax > 0.9999)) {
        // close to unity with some graciousness given in case the approximation of Cp causes a problem
        RatCapToTheoMax = 0.9999;
    } else if (RatCapToTheoMax >= 1.1) {
        ShowSevereError(state,
                        "SizeCoolingPanelUA: Unit=[" + cCMO_CoolingPanel_Simple + ',' + this->EquipID +
                            "] has a cooling capacity that is greater than the maximum possible value.");
        ShowContinueError(state, "The result of this is that a UA value is impossible to calculate.");
        ShowContinueError(state, "Check the rated input for temperatures, flow, and capacity for this unit.");
        ShowContinueError(state, "The ratio of the capacity to the rated theoretical maximum must be less than unity.");
        ShowContinueError(state,
                          "The most likely cause for this is probably either the capacity (whether autosized or hardwired) being too high, the "
                          "rated flow being too low, rated temperatures being too close to each other, or all of those reasons.");
        ShowContinueError(state,
                          "Compare the rated capacity in your input to the product of the rated mass flow rate, Cp of water, and the difference "
                          "between the rated temperatures.");
        ShowContinueError(
            state, "If the rated capacity is higher than this product, then the cooling panel would violate the Second Law of Thermodynamics.");
        SizeCoolingPanelUA = false;
        this->UA = 1.0;
    }
    if (Tinletr >= Tzoner) {
        ShowSevereError(state,
                        "SizeCoolingPanelUA: Unit=[" + cCMO_CoolingPanel_Simple + ',' + this->EquipID +
                            "] has a rated water temperature that is higher than the rated zone temperature.");
        ShowContinueError(state,
                          "Such a situation would not lead to cooling and thus the rated water or zone temperature or both should be adjusted.");
        SizeCoolingPanelUA = false;
        this->UA = 1.0;
    } else {
        this->UA = -MDotXCp * log(1.0 - RatCapToTheoMax);
        if (this->UA <= 0.0) {
            ShowSevereError(state,
                            "SizeCoolingPanelUA: Unit=[" + cCMO_CoolingPanel_Simple + ',' + this->EquipID +
                                "] has a zero or negative calculated UA value.");
            ShowContinueError(state,
                              "This is not allowed.  Please check the rated input parameters for this device to ensure that the values are correct.");
            SizeCoolingPanelUA = false;
        }
    }

    return SizeCoolingPanelUA;
}

void CoolingPanelParams::CalcCoolingPanel(EnergyPlusData &state, int const CoolingPanelNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Sept 2014

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates both the convective and radiant heat transfer rate
    // for the simple cooling panel.  The process used here was derived from the hot
    // water baseboard radiant/convective heater and adapted for cooling.

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)
    // Incropera and DeWitt, Fundamentals of Heat and Mass Transfer

    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using FluidProperties::GetSpecificHeatGlycol;

    using PlantUtilities::SetComponentFlowRate;
    using Psychrometrics::PsyTdpFnWPb;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const MinFrac(0.0005); // Minimum fraction that delivers radiant heats to surfaces
    int const Maxiter(20);        // Maximum number of iterations to achieve tolerance
    Real64 const IterTol(0.005);  // Tolerance of 0.5%
    static std::string const RoutineName("CalcCoolingPanel");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int iter;
    Real64 RadHeat;
    Real64 CoolingPanelCool;
    Real64 waterInletTemp;
    Real64 waterOutletTemp;
    Real64 waterMassFlowRate;
    Real64 waterMassFlowRateMax;
    Real64 CapacitanceWater;
    Real64 NTU;
    Real64 Effectiveness;
    Real64 QZnReq;
    Real64 Cp;
    Real64 Tzone;
    Real64 Xr;
    Real64 MCpEpsAct;
    Real64 MCpEpsLow;
    Real64 MCpEpsHigh;
    Real64 MdotLow;
    Real64 MdotHigh;
    Real64 FracGuess;
    Real64 MdotGuess;
    Real64 MCpEpsGuess;
    Real64 ControlTemp;
    Real64 SetPointTemp;
    Real64 OffTempCool;
    Real64 FullOnTempCool;
    Real64 MassFlowFrac;
    Real64 DewPointTemp;
    Real64 LoadMet;
    bool CoolingPanelOn;
    bool ModifiedWaterInletTemp;

    ModifiedWaterInletTemp = false;
    ZoneNum = this->ZonePtr;
    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
    waterInletTemp = this->WaterInletTemp;
    waterOutletTemp = waterInletTemp;
    waterMassFlowRateMax = this->WaterMassFlowRateMax;
    Xr = this->FracRadiant;

    if (GetCurrentScheduleValue(state, this->SchedPtr) > 0) {
        CoolingPanelOn = true;
    } else {
        CoolingPanelOn = false;
    }
    // Calculate the "zone" temperature for determining the output of the cooling panel
    Tzone = Xr * state.dataHeatBal->ZoneMRT(ZoneNum) + ((1.0 - Xr) * state.dataHeatBalFanSys->MAT(ZoneNum));

    // Logical controls: if the WaterInletTemperature is higher than Tzone, do not run the panel
    if (waterInletTemp >= Tzone) CoolingPanelOn = false;

    // Condensation Controls based on dewpoint temperature of the zone.
    // The assumption here is that condensation might take place if the inlet water temperature
    // is below the dewpoint temperature of the space.  This assumption is made because we are
    // probably dealing with a metal panel and the surface temperature of the panel will be very
    // close to the inlet water temperature in certain places.  Thus, if the water inlet temperature
    // is below the dewpoint temperature, then we might have condensation.  We need to deal with this
    // possibility based on the user selected method.  The good news here is that we don't have to
    // iterate like in the low temperature radiant systems because the inlet water condition is known
    // not calculated.  So, we can deal with this upfront rather than after calculation and then more
    // iteration.
    DewPointTemp = PsyTdpFnWPb(state, state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress);

    if (waterInletTemp < (DewPointTemp + this->CondDewPtDeltaT) && (CoolingPanelOn)) {

        // Condensation is possible so invoke the three possible ways of handling this based on the user's choice...

        if (this->CondCtrlType == CondCtrl::NONE) {
            // Condensation control is "off" which means don't do anything, simply let it run and ignore condensation
        } else if (this->CondCtrlType == CondCtrl::SIMPLEOFF) {
            // For "simple off", simply turn the simple cooling panel off to avoid condensation
            waterMassFlowRate = 0.0;
            CoolingPanelOn = false;
            // Produce a warning message so that user knows the system was shut-off due to potential for condensation
            if (!state.dataGlobal->WarmupFlag) {
                if (this->CondErrIndex == 0) { // allow errors up to number of radiant systems
                    ShowWarningMessage(state,
                                       cCMO_CoolingPanel_Simple + " [" + this->EquipID +
                                           "] inlet water temperature below dew-point temperature--potential for condensation exists");
                    ShowContinueError(state, "Flow to the simple cooling panel will be shut-off to avoid condensation");
                    ShowContinueError(state, format("Water inlet temperature = {:.2R}", waterInletTemp));
                    ShowContinueError(state, format("Zone dew-point temperature + safety delta T= {:.2R}", DewPointTemp + this->CondDewPtDeltaT));
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state,
                                      format("Note that a {:.4R} C safety was chosen in the input for the shut-off criteria", this->CondDewPtDeltaT));
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               cCMO_CoolingPanel_Simple + " [" + this->EquipID + "] condensation shut-off occurrence continues.",
                                               this->CondErrIndex,
                                               DewPointTemp,
                                               DewPointTemp,
                                               _,
                                               "C",
                                               "C");
            }

        } else if (this->CondCtrlType == CondCtrl::VARIEDOFF) {
            // Varied off is the most complex because it tries to run by reducing the inlet temperature
            // As a result of this, there is some bypass/recirculation that has to take place.
            // We might not have enough flow rate to meet whatever load we have, but at least
            // the system is still running at some partial load and avoiding condensation.
            waterInletTemp = DewPointTemp + this->CondDewPtDeltaT;
            ModifiedWaterInletTemp = true;
        }
    }

    // The next IF block is to find the mass flow rate based on what type of control the user has requested.  Load based controls
    // vary the flow to meet the zone load calculated by the user-defined thermostat.  Temperature based controls vary the flow
    // based on a comparison between the control temperature and the setpoint schedule and throttling range.

    if ((this->ControlType == Control::ZoneTotalLoad) || (this->ControlType == Control::ZoneConvectiveLoad)) {

        if (QZnReq < -SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) && (CoolingPanelOn)) {

            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                       waterInletTemp,
                                       state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                       RoutineName);

            // Find the actual load: this parameter modifies what the response of the system should be.  For total load control, the system tries
            // to meet the QZnReq.  For convective load control, the convective output of the device equals QZnReq which means that the load on
            // the panel is higher as is its output.  Total load control will miss the setpoint temperature but will likely get there with time.
            // Convective load control will hit the setpoint short term better but will result in overcooling in the long run probably.
            if (this->ControlType == Control::ZoneConvectiveLoad) {
                QZnReq = QZnReq / this->FracConvect;
            }

            // Now for a small amount of iteration.  Try to find the value of mass flow rate that will come the closest to giving
            // the proper value for MCpEpsAct.  Limit iterations to avoid too much time wasting.
            MCpEpsAct = QZnReq / (waterInletTemp - Tzone);
            MCpEpsLow = 0.0;
            MdotLow = 0.0;
            MCpEpsHigh = waterMassFlowRateMax * Cp * (1.0 - exp(-this->UA / (waterMassFlowRateMax * Cp)));
            MdotHigh = waterMassFlowRateMax;
            if (MCpEpsAct <= MCpEpsLow) {
                MCpEpsAct = MCpEpsLow;
                waterMassFlowRate = 0.0;
                state.dataLoopNodes->Node(this->WaterInletNode).MassFlowRate = 0.0;
                CoolingPanelOn = false;
            } else if (MCpEpsAct >= MCpEpsHigh) {
                MCpEpsAct = MCpEpsHigh;
                waterMassFlowRate = waterMassFlowRateMax;
                state.dataLoopNodes->Node(this->WaterInletNode).MassFlowRate = waterMassFlowRateMax;
            } else {
                for (iter = 1; iter <= Maxiter; ++iter) {
                    FracGuess = (MCpEpsAct - MCpEpsLow) / (MCpEpsHigh - MCpEpsLow);
                    MdotGuess = MdotHigh * FracGuess;
                    MCpEpsGuess = MdotGuess * Cp * (1.0 - exp(-this->UA / (MdotGuess * Cp)));
                    if (MCpEpsGuess <= MCpEpsAct) {
                        MCpEpsLow = MCpEpsGuess;
                        MdotLow = MdotGuess;
                    } else { // MCpEpsGuess > MCpEpsAct
                        MCpEpsHigh = MCpEpsGuess;
                        MdotHigh = MdotGuess;
                    }
                    if (((MCpEpsAct - MCpEpsGuess) / MCpEpsAct) <= IterTol) {
                        waterMassFlowRate = MdotGuess;
                        state.dataLoopNodes->Node(this->WaterInletNode).MassFlowRate = waterMassFlowRate;
                        break;
                    }
                }
            }

        } else {
            CoolingPanelOn = false;
        }

    } else { // temperature control rather than zone load control

        if (CoolingPanelOn) {

            this->SetCoolingPanelControlTemp(state, ControlTemp, ZoneNum);

            SetPointTemp = GetCurrentScheduleValue(state, this->ColdSetptSchedPtr);
            OffTempCool = SetPointTemp - 0.5 * this->ColdThrottlRange;
            FullOnTempCool = SetPointTemp + 0.5 * this->ColdThrottlRange;

            if (ControlTemp <= OffTempCool) {
                MassFlowFrac = 0.0;
                CoolingPanelOn = false;
            } else if (ControlTemp >= FullOnTempCool) {
                MassFlowFrac = 1.0;
            } else {
                MassFlowFrac = (ControlTemp - OffTempCool) / this->ColdThrottlRange;
                if (MassFlowFrac < MinFrac) MassFlowFrac = MinFrac;
            }

            waterMassFlowRate = MassFlowFrac * waterMassFlowRateMax;
        }
    }

    if (CoolingPanelOn) {
        SetComponentFlowRate(
            state, waterMassFlowRate, this->WaterInletNode, this->WaterOutletNode, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum);
        if (waterMassFlowRate <= 0.0) CoolingPanelOn = false;
    }

    if (CoolingPanelOn) {
        // Now simulate the system...
        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                   waterInletTemp,
                                   state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                   RoutineName);
        Effectiveness = 1.0 - exp(-this->UA / (waterMassFlowRate * Cp));
        if (Effectiveness <= 0.0) {
            Effectiveness = 0.0;
        } else if (Effectiveness >= 1.0) {
            Effectiveness = 1.0;
        }
        CoolingPanelCool = (Effectiveness)*waterMassFlowRate * Cp * (waterInletTemp - Tzone);
        waterOutletTemp = this->WaterInletTemp - (CoolingPanelCool / (waterMassFlowRate * Cp));
        RadHeat = CoolingPanelCool * this->FracRadiant;
        state.dataChilledCeilingPanelSimple->CoolingPanelSource(CoolingPanelNum) = RadHeat;

        if (this->FracRadiant <= MinFrac) {
            LoadMet = CoolingPanelCool;
        } else {

            // Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
            DistributeCoolingPanelRadGains(state);
            // Now "simulate" the system by recalculating the heat balances
            HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);

            HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

            // Here an assumption is made regarding radiant heat transfer to people.
            // While the radiant heat transfer to people array will be used by the thermal comfort
            // routines, the energy transfer to people would get lost from the perspective
            // of the heat balance.  So, to avoid this net loss of energy which clearly
            // gets added to the zones, we must account for it somehow.  This assumption
            // that all energy radiated to people is converted to convective energy is
            // not very precise, but at least it conserves energy. The system impact to heat balance
            // should include this.
            LoadMet = (SumHATsurf(state, ZoneNum) - state.dataChilledCeilingPanelSimple->ZeroSourceSumHATsurf(ZoneNum)) +
                      (CoolingPanelCool * this->FracConvect) + (RadHeat * this->FracDistribPerson);
        }
        this->WaterOutletEnthalpy = this->WaterInletEnthalpy - CoolingPanelCool / waterMassFlowRate;

    } else { // cooling panel off
        CapacitanceWater = 0.0;
        NTU = 0.0;
        Effectiveness = 0.0;
        waterOutletTemp = waterInletTemp;
        CoolingPanelCool = 0.0;
        LoadMet = 0.0;
        RadHeat = 0.0;
        waterMassFlowRate = 0.0;
        state.dataChilledCeilingPanelSimple->CoolingPanelSource(CoolingPanelNum) = 0.0;
        this->WaterOutletEnthalpy = this->WaterInletEnthalpy;
    }

    this->WaterOutletTemp = waterOutletTemp;
    this->WaterMassFlowRate = waterMassFlowRate;
    this->TotPower = LoadMet;
    this->Power = CoolingPanelCool;
    this->ConvPower = CoolingPanelCool - RadHeat;
    this->RadPower = RadHeat;
}

void CoolingPanelParams::SetCoolingPanelControlTemp(EnergyPlusData &state, Real64 &ControlTemp, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   July 2016

    // METHODOLOGY EMPLOYED:
    // This subroutine sets the control temperature for the simple cooling panel.

    // Using/Aliasing

    {
        auto const SELECT_CASE_var(this->ControlType);
        if (SELECT_CASE_var == Control::MAT) {
            ControlTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
        } else if (SELECT_CASE_var == Control::MRT) {
            ControlTemp = state.dataHeatBal->ZoneMRT(ZoneNum);
        } else if (SELECT_CASE_var == Control::Operative) {
            ControlTemp = 0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
        } else if (SELECT_CASE_var == Control::ODB) {
            ControlTemp = state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;
        } else if (SELECT_CASE_var == Control::OWB) {
            ControlTemp = state.dataHeatBal->Zone(ZoneNum).OutWetBulbTemp;
        } else { // Should never get here
            ControlTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
            ShowSevereError(state, "Illegal control type in cooling panel system: " + this->EquipID);
            ShowFatalError(state, "Preceding condition causes termination.");
        }
    }
}

void UpdateCoolingPanel(EnergyPlusData &state, int const CoolingPanelNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Sept 2014
    //                      February 2001
    //       MODIFIED       Aug 2007 Daeho Kang (Add the update of radiant source)

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using PlantUtilities::SafeCopyPlantNode;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WaterInletNode;
    int WaterOutletNode;

    // First, update the running average if necessary...
    if (state.dataChilledCeilingPanelSimple->LastSysTimeElapsed(CoolingPanelNum) == SysTimeElapsed) {
        state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg(CoolingPanelNum) -=
            state.dataChilledCeilingPanelSimple->LastCoolingPanelSrc(CoolingPanelNum) *
            state.dataChilledCeilingPanelSimple->LastTimeStepSys(CoolingPanelNum) / state.dataGlobal->TimeStepZone;
    }
    // Update the running average and the "last" values with the current values of the appropriate variables
    state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg(CoolingPanelNum) +=
        state.dataChilledCeilingPanelSimple->CoolingPanelSource(CoolingPanelNum) * TimeStepSys / state.dataGlobal->TimeStepZone;

    state.dataChilledCeilingPanelSimple->LastCoolingPanelSrc(CoolingPanelNum) =
        state.dataChilledCeilingPanelSimple->CoolingPanelSource(CoolingPanelNum);
    state.dataChilledCeilingPanelSimple->LastSysTimeElapsed(CoolingPanelNum) = SysTimeElapsed;
    state.dataChilledCeilingPanelSimple->LastTimeStepSys(CoolingPanelNum) = TimeStepSys;

    auto &ThisCP(state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum));

    WaterInletNode = ThisCP.WaterInletNode;
    WaterOutletNode = ThisCP.WaterOutletNode;

    auto &ThisInNode(state.dataLoopNodes->Node(WaterInletNode));
    auto &ThisOutNode(state.dataLoopNodes->Node(WaterOutletNode));

    // Set the outlet water nodes for the panel
    SafeCopyPlantNode(state, WaterInletNode, WaterOutletNode);
    ThisOutNode.Temp = ThisCP.WaterOutletTemp;
    ThisOutNode.Enthalpy = ThisCP.WaterOutletEnthalpy;
    ThisInNode.MassFlowRate = ThisCP.WaterMassFlowRate;
    ThisOutNode.MassFlowRate = ThisCP.WaterMassFlowRate;
    ThisInNode.MassFlowRateMax = ThisCP.WaterMassFlowRateMax;
    ThisOutNode.MassFlowRateMax = ThisCP.WaterMassFlowRateMax;
}

void UpdateCoolingPanelSourceValAvg(EnergyPlusData &state,
                                    bool &CoolingPanelSysOn) // .TRUE. if the radiant system has run this zone time step
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Sept 2014

    // PURPOSE OF THIS SUBROUTINE:
    // To transfer the average value of the heat source over the entire
    // zone time step back to the heat balance routines so that the heat
    // balance algorithms can simulate one last time with the average source
    // to maintain some reasonable amount of continuity and energy balance
    // in the temperature and flux histories.

    // METHODOLOGY EMPLOYED:
    // All of the record keeping for the average term is done in the Update
    // routine so the only other thing that this subroutine does is check to
    // see if the system was even on.  If any average term is non-zero, then
    // one or more of the radiant systems was running.

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoolingPanelNum; // DO loop counter for surface index

    CoolingPanelSysOn = false;

    // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
    if (!allocated(state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg)) return;

    // If it was allocated, then we have to check to see if this was running at all...
    for (CoolingPanelNum = 1; CoolingPanelNum <= state.dataChilledCeilingPanelSimple->NumCoolingPanels; ++CoolingPanelNum) {
        if (state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg(CoolingPanelNum) != 0.0) {
            CoolingPanelSysOn = true;
            break; // DO loop
        }
    }

    state.dataChilledCeilingPanelSimple->CoolingPanelSource = state.dataChilledCeilingPanelSimple->CoolingPanelSrcAvg;

    DistributeCoolingPanelRadGains(state); // CoolingPanelRadSource has been modified so we need to redistribute gains
}

void DistributeCoolingPanelRadGains(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Sept 2014

    // PURPOSE OF THIS SUBROUTINE:
    // To distribute the gains from the hot water basebaord heater
    // as specified in the user input file.  This includes distribution
    // of long wavelength radiant gains to surfaces and "people."

    // METHODOLOGY EMPLOYED:
    // We must cycle through all of the radiant systems because each
    // surface could feel the effect of more than one radiant system.
    // Note that the energy radiated to people is assumed to affect them
    // but them it is assumed to be convected to the air.

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)

    // Using/Aliasing
    using DataHeatBalFanSys::MaxRadHeatFlux;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const SmallestArea(0.001); // Smallest area in meters squared (to avoid a divide by zero)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RadSurfNum;           // Counter for surfaces receiving radiation from radiant heater
    int CoolingPanelNum;      // Counter for the baseboard
    int SurfNum;              // Pointer to the Surface derived type
    int ZoneNum;              // Pointer to the Zone derived type
    Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

    // Initialize arrays
    state.dataHeatBalFanSys->QCoolingPanelSurf = 0.0;
    state.dataHeatBalFanSys->QCoolingPanelToPerson = 0.0;

    for (CoolingPanelNum = 1; CoolingPanelNum <= state.dataChilledCeilingPanelSimple->NumCoolingPanels; ++CoolingPanelNum) {

        auto &ThisCP(state.dataChilledCeilingPanelSimple->CoolingPanel(CoolingPanelNum));

        ZoneNum = ThisCP.ZonePtr;
        if (ZoneNum <= 0) continue;
        state.dataHeatBalFanSys->QCoolingPanelToPerson(ZoneNum) +=
            state.dataChilledCeilingPanelSimple->CoolingPanelSource(CoolingPanelNum) * ThisCP.FracDistribPerson;

        for (RadSurfNum = 1; RadSurfNum <= ThisCP.TotSurfToDistrib; ++RadSurfNum) {
            SurfNum = ThisCP.SurfacePtr(RadSurfNum);
            auto &ThisSurf(state.dataSurface->Surface(SurfNum));
            if (ThisSurf.Area > SmallestArea) {
                ThisSurfIntensity =
                    (state.dataChilledCeilingPanelSimple->CoolingPanelSource(CoolingPanelNum) * ThisCP.FracDistribToSurf(RadSurfNum) / ThisSurf.Area);
                state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) += ThisSurfIntensity;
                // CR 8074, trap for excessive intensity (throws off surface balance )
                if (ThisSurfIntensity > MaxRadHeatFlux) {
                    ShowSevereError(state, "DistributeCoolingPanelRadGains:  excessive thermal radiation heat flux intensity detected");
                    ShowContinueError(state, "Surface = " + ThisSurf.Name);
                    ShowContinueError(state, format("Surface area = {:.3R} [m2]", ThisSurf.Area));
                    ShowContinueError(state, "Occurs in " + cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                    ShowContinueError(state, format("Radiation intensity = {:.2R} [W/m2]", ThisSurfIntensity));
                    ShowContinueError(state, "Assign a larger surface area or more surfaces in " + cCMO_CoolingPanel_Simple);
                    ShowFatalError(state, "DistributeCoolingPanelRadGains:  excessive thermal radiation heat flux intensity detected");
                }
            } else {
                ShowSevereError(state, "DistributeCoolingPanelRadGains:  surface not large enough to receive thermal radiation heat flux");
                ShowContinueError(state, "Surface = " + ThisSurf.Name);
                ShowContinueError(state, format("Surface area = {:.3R} [m2]", ThisSurf.Area));
                ShowContinueError(state, "Occurs in " + cCMO_CoolingPanel_Simple + " = " + ThisCP.EquipID);
                ShowContinueError(state, "Assign a larger surface area or more surfaces in " + cCMO_CoolingPanel_Simple);
                ShowFatalError(state, "DistributeCoolingPanelRadGains:  surface not large enough to receive thermal radiation heat flux");
            }
        }
    }
}

void CoolingPanelParams::ReportCoolingPanel(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Aug 2014

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)

    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // All of the power numbers are negative for cooling.  This is because they will have a negative
    // or cooling impact on the surfaces/zones.  However, the output variables are noted as cooling.
    // So, their sign should be positive if actually cooling and we need to reverse the sign here.
    // This should not have an impact on any of the internal variables or the heat balances because
    // those use other variables.
    this->TotPower = -this->TotPower;
    this->Power = -this->Power;
    this->ConvPower = -this->ConvPower;
    this->RadPower = -this->RadPower;

    this->TotEnergy = this->TotPower * TimeStepSys * DataGlobalConstants::SecInHour;
    this->Energy = this->Power * TimeStepSys * DataGlobalConstants::SecInHour;
    this->ConvEnergy = this->ConvPower * TimeStepSys * DataGlobalConstants::SecInHour;
    this->RadEnergy = this->RadPower * TimeStepSys * DataGlobalConstants::SecInHour;
}

Real64 SumHATsurf(EnergyPlusData &state, int const ZoneNum) // Zone number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   Aug 2014

    // PURPOSE OF THIS FUNCTION:
    // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
    // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
    // and should be updated accordingly.

    // REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)

    // Using/Aliasing
    using DataSurfaces::WinShadingType;

    // Return value
    Real64 SumHATsurf;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int SurfNum; // Surface number
    Real64 Area; // Effective surface area
    SumHATsurf = 0.0;

    for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {

        auto &ThisSurf(state.dataSurface->Surface(SurfNum));

        Area = ThisSurf.Area;

        if (ThisSurf.Class == DataSurfaces::SurfaceClass::Window) {

            if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                // The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
                Area += state.dataSurface->SurfWinDividerArea(SurfNum);
            }

            if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                // Window frame contribution
                SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                              (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) * state.dataSurface->SurfWinFrameTempSurfIn(SurfNum);
            }

            if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                              (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) * state.dataSurface->SurfWinDividerTempSurfIn(SurfNum);
            }
        }

        SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * Area * state.dataHeatBalSurf->TempSurfInTmp(SurfNum);
    }

    return SumHATsurf;
}

} // namespace EnergyPlus::CoolingPanelSimple
