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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>

namespace EnergyPlus {

namespace ZoneDehumidifier {

    // Module containing the routines dealing with the ZoneDehumidifier

    // MODULE INFORMATION:
    //       AUTHOR         Don Shirey, FSEC
    //       DATE WRITTEN   July/Aug 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Calculate the performance of zone (room) air dehumidifiers.  Meant to model
    // conventional direct expansion (DX) cooling-based room air dehumidifiers
    // (reject 100% of condenser heat to the zone air), but the approach
    // might be able to be used to model other room air dehumidifier types.

    // METHODOLOGY EMPLOYED:
    // Model as a piece of zone equipment, with inputs for water removal and
    // energy factor at rated conditions (26.7C, 60% RH). Then provide curve objects
    // to describe performance at off-rated conditions. A part-load cycling curve
    // input is also provided. It is assumed that this equipment dehumidifies but
    // heats the air. If used in tandem with another system that cools and dehumidifies,
    // then the zone dehumidifier should be specified as the lowest cooling priority
    // in the ZoneHVAC:EquipmentList object. The cooling and dehumidification system
    // operates first to meet the temperature setpoint (and possibly the high humidity
    // setpoint as well). If additional dehumidification is needed, then the zone
    // dehumidifier operates. The excess sensible heat generated by the dehumidifier
    // is carried over to the next HVAC time step.

    // OTHER NOTES:
    // Example manufacturer's data at:
    //   http://www.thermastor.com/HI-E-DRY-100/HI-E-DRY-100-Spec.pdf
    //   http://www.thermastor.com/HI-E-DRY-195/HI-E-DRY-195-Spec.pdf

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace ScheduleManager;

    void SimZoneDehumidifier(EnergyPlusData &state,
                             std::string const &CompName,                    // Name of the zone dehumidifier
                             int const ZoneNum,                              // Number of zone being served
                             [[maybe_unused]] bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                             Real64 &QSensOut,                               // Sensible capacity delivered to zone (W)
                             Real64 &QLatOut,                                // Latent capacity delivered to zone (kg/s), dehumidify = negative
                             int &CompIndex                                  // Index to the zone dehumidifier
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   July/Aug 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a zone dehumidifier.

        // METHODOLOGY EMPLOYED:
        // Call appropriate subroutines to get input values, initialize variables, model performanc
        // update node information, report model outputs.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneDehumidNum;   // Index of zone dehumidifier being simulated
        Real64 QZnDehumidReq; // Zone dehumidification load required (kg moisture/sec)

        if (state.dataZoneDehumidifier->GetInputFlag) {
            GetZoneDehumidifierInput(state);
            state.dataZoneDehumidifier->GetInputFlag = false;
        }

        // Find the correct zone dehumidifier
        if (CompIndex == 0) {
            ZoneDehumidNum = UtilityRoutines::FindItemInList(CompName, state.dataZoneDehumidifier->ZoneDehumid);
            if (ZoneDehumidNum == 0) {
                ShowFatalError(state, "SimZoneDehumidifier: Unit not found= " + CompName);
            }
            CompIndex = ZoneDehumidNum;
        } else {
            ZoneDehumidNum = CompIndex;
            if (ZoneDehumidNum > state.dataZoneDehumidifier->NumDehumidifiers || ZoneDehumidNum < 1) {
                ShowFatalError(state,
                               format("SimZoneDehumidifier:  Invalid CompIndex passed= {}, Number of Units= {}, Entered Unit name= {}",
                                      ZoneDehumidNum,
                                      state.dataZoneDehumidifier->NumDehumidifiers,
                                      CompName));
            }
            if (state.dataZoneDehumidifier->CheckEquipName(ZoneDehumidNum)) {
                if (CompName != state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidNum).Name) {
                    ShowFatalError(state,
                                   format("SimZoneDehumidifier: Invalid CompIndex passed={}, Unit name= {}, stored Unit Name for that index= {}",
                                          ZoneDehumidNum,
                                          CompName,
                                          state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidNum).Name));
                }
                state.dataZoneDehumidifier->CheckEquipName(ZoneDehumidNum) = false;
            }
        }

        QZnDehumidReq = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum).RemainingOutputReqToDehumidSP; // Negative means dehumidify

        InitZoneDehumidifier(state, ZoneDehumidNum);

        CalcZoneDehumidifier(state, ZoneDehumidNum, QZnDehumidReq, QSensOut, QLatOut);

        UpdateZoneDehumidifier(state, ZoneDehumidNum);

        ReportZoneDehumidifier(state, ZoneDehumidNum);
    }

    void GetZoneDehumidifierInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   July/Aug 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Retrieve the inputs from the input data file (idf) being simulated.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology using available utility routines where appropriate.

        // Using/Aliasing
        using CurveManager::CurveValue;
        using CurveManager::GetCurveIndex;
        using NodeInputManager::GetOnlySingleNode;
        using WaterManager::SetupTankSupplyComponent;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneDehumidifierInput");
        static std::string const CurrentModuleObject("ZoneHVAC:Dehumidifier:DX");
        Real64 const RatedInletAirTemp(26.7);
        Real64 const RatedInletAirRH(60.0);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneDehumidIndex;          // Loop index
        int NumAlphas(0);              // Number of Alphas to allocate arrays, then used for each GetObjectItem call
        int NumNumbers(0);             // Number of Numbers to allocate arrays, then used for each GetObjectItem call
        int IOStatus;                  // Used in GetObjectItem
        bool ErrorsFound(false);       // Set to true if errors in input, fatal at end of routine
        Array1D_string Alphas;         // Alpha input items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D<Real64> Numbers;       // Numeric input items for object
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        int TotalArgs(0);              // Total number of alpha and numeric arguments (max)
        Real64 CurveVal;               // Output from curve object (water removal or energy factor curves)

        state.dataZoneDehumidifier->NumDehumidifiers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        state.dataZoneDehumidifier->ZoneDehumid.allocate(state.dataZoneDehumidifier->NumDehumidifiers);
        state.dataZoneDehumidifier->CheckEquipName.dimension(state.dataZoneDehumidifier->NumDehumidifiers, true);

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);

        Alphas.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        Numbers.dimension(NumNumbers, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        for (ZoneDehumidIndex = 1; ZoneDehumidIndex <= state.dataZoneDehumidifier->NumDehumidifiers; ++ZoneDehumidIndex) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     ZoneDehumidIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            // A1,  \field Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name = Alphas(1);
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).UnitType = CurrentModuleObject; // 'ZoneHVAC:Dehumidifier:DX'
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).UnitType_Num =
                state.dataZoneDehumidifier->ZoneDehumidUnit; // 'ZoneHVAC:Dehumidifier:DX' = 1

            // A2,  \field Availability Schedule Name
            if (lAlphaBlanks(2)) {
                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).SchedPtr =
                    GetScheduleIndex(state, Alphas(2)); // Convert schedule name to pointer
                if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).SchedPtr == 0) {
                    ShowSevereError(state, cAlphaFields(2) + " not found = " + Alphas(2));
                    ShowContinueError(state,
                                      "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                    ErrorsFound = true;
                }
            }

            // A3 , \field Air Inlet Node Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                                          Alphas(3),
                                                                                                          ErrorsFound,
                                                                                                          CurrentModuleObject,
                                                                                                          Alphas(1),
                                                                                                          DataLoopNode::NodeFluidType::Air,
                                                                                                          DataLoopNode::NodeConnectionType::Inlet,
                                                                                                          NodeInputManager::compFluidStream::Primary,
                                                                                                          ObjectIsNotParent);

            // A4 , \field Air Outlet Node Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                                           Alphas(4),
                                                                                                           ErrorsFound,
                                                                                                           CurrentModuleObject,
                                                                                                           Alphas(1),
                                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                                           ObjectIsNotParent);

            // N1,  \field Rated Water Removal
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).RatedWaterRemoval = Numbers(1);
            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).RatedWaterRemoval <= 0.0) {
                ShowSevereError(state, cNumericFields(1) + " must be greater than zero.");
                ShowContinueError(state, format("Value specified = {:.5T}", Numbers(1)));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                ErrorsFound = true;
            }

            // N2,  \field Rated Energy Factor
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).RatedEnergyFactor = Numbers(2);
            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).RatedEnergyFactor <= 0.0) {
                ShowSevereError(state, cNumericFields(2) + " must be greater than zero.");
                ShowContinueError(state, format("Value specified = {:.5T}", Numbers(2)));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                ErrorsFound = true;
            }

            // N3,  \field Rated Air Flow Rate
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).RatedAirVolFlow = Numbers(3);
            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).RatedAirVolFlow <= 0.0) {
                ShowSevereError(state, cNumericFields(3) + " must be greater than zero.");
                ShowContinueError(state, format("Value specified = {:.5T}", Numbers(3)));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                ErrorsFound = true;
            }

            // A5,  \field Water Removal Curve Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).WaterRemovalCurveIndex =
                GetCurveIndex(state, Alphas(5)); // Convert curve name to index number
            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).WaterRemovalCurveIndex == 0) {
                if (lAlphaBlanks(5)) {
                    ShowSevereError(state,
                                    RoutineName + ':' + CurrentModuleObject + "=\"" + cAlphaFields(5) + "\" is required, missing for " +
                                        cAlphaFields(1) + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                } else {
                    ShowSevereError(state, cAlphaFields(5) + " not found = " + Alphas(5));
                    ShowContinueError(state,
                                      "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                }
                ErrorsFound = true;
            } else {
                // Verify Curve object, only legal type is BiQuadratic
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).WaterRemovalCurveIndex, // Curve index
                                                 {2},                                                                              // Valid dimensions
                                                 RoutineName,                                                                      // Routine name
                                                 CurrentModuleObject,                                                              // Object Type
                                                 state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name,                   // Object Name
                                                 cAlphaFields(5));                                                                 // Field Name

                if (!ErrorsFound) {
                    CurveVal = CurveValue(
                        state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).WaterRemovalCurveIndex, RatedInletAirTemp, RatedInletAirRH);
                    if (CurveVal > 1.10 || CurveVal < 0.90) {
                        ShowWarningError(state, cAlphaFields(5) + " output is not equal to 1.0");
                        ShowContinueError(state, "(+ or -10%) at rated conditions for " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", CurveVal));
                    }
                }
            }

            // A6,  \field Energy Factor Curve Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).EnergyFactorCurveIndex =
                GetCurveIndex(state, Alphas(6)); // convert curve name to number
            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).EnergyFactorCurveIndex == 0) {
                if (lAlphaBlanks(6)) {
                    ShowSevereError(state,
                                    RoutineName + ':' + CurrentModuleObject + "=\"" + cAlphaFields(6) + "\" is required, missing for " +
                                        cAlphaFields(1) + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                } else {
                    ShowSevereError(state, cAlphaFields(6) + " not found = " + Alphas(6));
                    ShowContinueError(state,
                                      "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, only legal type is BiQuadratic
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).EnergyFactorCurveIndex, // Curve index
                                                 {2},                                                                              // Valid dimensions
                                                 RoutineName,                                                                      // Routine name
                                                 CurrentModuleObject,                                                              // Object Type
                                                 state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name,                   // Object Name
                                                 cAlphaFields(6));                                                                 // Field Name

                if (!ErrorsFound) {
                    CurveVal = CurveValue(
                        state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).EnergyFactorCurveIndex, RatedInletAirTemp, RatedInletAirRH);
                    if (CurveVal > 1.10 || CurveVal < 0.90) {
                        ShowWarningError(state, cAlphaFields(6) + " output is not equal to 1.0");
                        ShowContinueError(state, "(+ or -10%) at rated conditions for " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(state, format("Curve output at rated conditions = {:.3T}", CurveVal));
                    }
                }
            }

            // A7,  \field Part Load Fraction Correlation Curve Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).PartLoadCurveIndex =
                GetCurveIndex(state, Alphas(7)); // convert curve name to number
            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).PartLoadCurveIndex == 0) {
                if (lAlphaBlanks(7)) {
                    ShowSevereError(state,
                                    RoutineName + ':' + CurrentModuleObject + "=\"" + cAlphaFields(7) + "\" is required, missing for " +
                                        cAlphaFields(1) + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                } else {
                    ShowSevereError(state, cAlphaFields(7) + " not found = " + Alphas(7));
                    ShowContinueError(state,
                                      "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                }
                ErrorsFound = true;
            } else {
                // Verify Curve Object, legal types are Quadratic and Cubic
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).PartLoadCurveIndex, // Curve index
                                                 {1},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 CurrentModuleObject,                                                          // Object Type
                                                 state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name,               // Object Name
                                                 cAlphaFields(7));                                                             // Field Name
            }

            // N4,  \field Minimum Dry-Bulb Temperature for Dehumidifier Operation
            // N5,  \field Maximum Dry-Bulb Temperature for Dehumidifier Operation
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).MinInletAirTemp = Numbers(4);
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).MaxInletAirTemp = Numbers(5);

            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).MinInletAirTemp >=
                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).MaxInletAirTemp) {
                ShowSevereError(state, cNumericFields(5) + " must be greater than " + cNumericFields(4));
                ShowContinueError(state, format("{} specified = {:.1T}", cNumericFields(5), Numbers(5)));
                ShowContinueError(state, format("{} specified = {:.1T}", cNumericFields(4), Numbers(4)));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                ErrorsFound = true;
            }

            // N6,  \field Off Cycle Parasitic Electric Load
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).OffCycleParasiticLoad = Numbers(6); // Off Cycle Parasitic Load [W]

            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).OffCycleParasiticLoad < 0.0) {
                ShowSevereError(state, cNumericFields(6) + " must be >= zero.");
                ShowContinueError(state, format("Value specified = {:.2T}", Numbers(6)));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                ErrorsFound = true;
            }

            // A8;  \field Condensate Collection Water Storage Tank Name
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateCollectName = Alphas(8);
            if (lAlphaBlanks(8)) {
                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateCollectMode = state.dataZoneDehumidifier->CondensateDiscarded;
            } else {
                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateCollectMode = state.dataZoneDehumidifier->CondensateToTank;
                SetupTankSupplyComponent(state,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name,
                                         CurrentModuleObject,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateCollectName,
                                         ErrorsFound,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateTankID,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateTankSupplyARRID);
            }

        } //   DO ZoneDehumidIndex=1,NumDehumidifiers

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + ':' + CurrentModuleObject + ": Errors found in input.");
        }

        for (ZoneDehumidIndex = 1; ZoneDehumidIndex <= state.dataZoneDehumidifier->NumDehumidifiers; ++ZoneDehumidIndex) {
            // Set up report variables for the dehumidifiers
            SetupOutputVariable(state,
                                "Zone Dehumidifier Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).SensHeatingRate,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).SensHeatingEnergy,
                                "System",
                                "Sum",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Removed Water Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).WaterRemovalRate,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Removed Water Mass",
                                OutputProcessor::Unit::kg,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).WaterRemoved,
                                "System",
                                "Sum",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).ElecPower,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).ElecConsumption,
                                "System",
                                "Sum",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name,
                                _,
                                "Electricity",
                                "COOLING",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Zone Dehumidifier Off Cycle Parasitic Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).OffCycleParasiticElecPower,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Off Cycle Parasitic Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).OffCycleParasiticElecCons,
                                "System",
                                "Sum",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Part Load Ratio",
                                OutputProcessor::Unit::None,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).DehumidPLR,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Runtime Fraction",
                                OutputProcessor::Unit::None,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).DehumidRTF,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
            SetupOutputVariable(state,
                                "Zone Dehumidifier Outlet Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).OutletAirTemp,
                                "System",
                                "Average",
                                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);

            if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).CondensateCollectMode == state.dataZoneDehumidifier->CondensateToTank) {
                SetupOutputVariable(state,
                                    "Zone Dehumidifier Condensate Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).DehumidCondVolFlowRate,
                                    "System",
                                    "Average",
                                    state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name);
                SetupOutputVariable(state,
                                    "Zone Dehumidifier Condensate Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).DehumidCondVol,
                                    "System",
                                    "Sum",
                                    state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).Name,
                                    _,
                                    "OnSiteWater",
                                    "Condensate",
                                    _,
                                    "System");
            }
        }
    }

    void InitZoneDehumidifier(EnergyPlusData &state, int const ZoneDehumNum) // Number of the current zone dehumidifier being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   July/Aug 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes information for the zone dehumidifier model

        // METHODOLOGY EMPLOYED:
        // Use status flags to trigger various initializations

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataZoneEquipment::CheckZoneEquipmentList;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyWFnTdbRhPb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitZoneDehumidifier");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int LoopIndex;         // DO loop index
        int AirInletNode;      // Inlet air node number
        Real64 RatedAirHumrat; // Humidity ratio (kg/kg) at rated inlet air conditions of 26.6667C, 60% RH
        Real64 RatedAirDBTemp; // Dry-bulb air temperature at rated conditions 26.6667C
        Real64 RatedAirRH;     // Relative humidity of air (0.6 --> 60%) at rated conditions

        // Do the one time initializations
        if (state.dataZoneDehumidifier->MyOneTimeFlag) {
            state.dataZoneDehumidifier->MyEnvrnFlag.allocate(state.dataZoneDehumidifier->NumDehumidifiers);
            state.dataZoneDehumidifier->MyEnvrnFlag = true;
            state.dataZoneDehumidifier->MyOneTimeFlag = false;
        }

        // Need to check all dehumidifiers to see if they are on Zone Equipment List or issue warning
        if (!state.dataZoneDehumidifier->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataZoneDehumidifier->ZoneEquipmentListChecked = true;
            for (LoopIndex = 1; LoopIndex <= state.dataZoneDehumidifier->NumDehumidifiers; ++LoopIndex) {
                if (CheckZoneEquipmentList(
                        state, state.dataZoneDehumidifier->ZoneDehumid(LoopIndex).UnitType, state.dataZoneDehumidifier->ZoneDehumid(LoopIndex).Name))
                    continue;
                ShowSevereError(state,
                                "InitZoneDehumidifier: Zone Dehumidifier=\"" + state.dataZoneDehumidifier->ZoneDehumid(LoopIndex).UnitType + ',' +
                                    state.dataZoneDehumidifier->ZoneDehumid(LoopIndex).Name +
                                    "\" is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        AirInletNode = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).AirInletNodeNum;
        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataZoneDehumidifier->MyEnvrnFlag(ZoneDehumNum)) {

            // Set the mass flow rates from the input volume flow rates, at rated conditions of 26.6667C, 60% RH
            // Might default back to STP later after discussion with M. Witte, use StdRhoAir instead of calc'd RhoAir at rated conditions
            RatedAirDBTemp = 26.6667; // 26.6667 C, 80F
            RatedAirRH = 0.6;         // 60% RH
            RatedAirHumrat = PsyWFnTdbRhPb(state, RatedAirDBTemp, RatedAirRH, state.dataEnvrn->StdBaroPress, RoutineName);
            state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirMassFlow =
                PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, RatedAirDBTemp, RatedAirHumrat, RoutineName) *
                state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirVolFlow;

            // Set the node max and min mass flow rates on inlet node... outlet node gets updated in UPDATE subroutine
            state.dataLoopNodes->Node(AirInletNode).MassFlowRateMax = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirMassFlow;
            state.dataLoopNodes->Node(AirInletNode).MassFlowRateMaxAvail = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirMassFlow;
            state.dataLoopNodes->Node(AirInletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(AirInletNode).MassFlowRateMin = 0.0;

            state.dataZoneDehumidifier->MyEnvrnFlag(ZoneDehumNum) = false;
        } // End one time inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataZoneDehumidifier->MyEnvrnFlag(ZoneDehumNum) = true;
        }

        // These initializations are done every iteration
        state.dataLoopNodes->Node(AirInletNode).MassFlowRate = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirMassFlow;

        // Zero out the report variables
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).SensHeatingRate = 0.0;   // Zone Dehumidifier Sensible Heating Rate [W]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).SensHeatingEnergy = 0.0; // Zone Dehumidifier Sensible Heating Energy [J]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemovalRate = 0.0;  // Zone Dehumidifier Water Removal Rate [kg/s]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemoved = 0.0;      // Zone Dehumidifier Water Removed [kg]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).ElecPower = 0.0;         // Zone Dehumidifier Electric Power [W]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).ElecConsumption = 0.0;   // Zone Dehumidifier Electric Consumption [J]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).DehumidPLR = 0.0;        // Zone Dehumidifier Part-Load Ratio [-]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).DehumidRTF = 0.0;        // Zone Dehumidifier Runtime Fraction [-]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OffCycleParasiticElecPower =
            0.0; // Zone Dehumidifier Off-Cycle Parasitic Electric Power [W]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OffCycleParasiticElecCons =
            0.0; // Zone Dehumidifier Off-Cycle Parasitic Electric Consumption [J]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).DehumidCondVolFlowRate =
            0.0;                                                                    // Zone Dehumidifier Condensate Volumetric Flow Rate [m3/s]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).DehumidCondVol = 0.0; // Zone Dehumidifier Condensate Volume [m3]
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OutletAirTemp =
            state.dataLoopNodes->Node(AirInletNode).Temp; // Zone Dehumidifier Outlet Air Temperature [C]
    }

    void SizeZoneDehumidifier()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   July 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // No automatic sizing for this model (yet).  Left in place for later (autosize based on latent requirements)

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na
    }

    void CalcZoneDehumidifier(EnergyPlusData &state,
                              int const ZoneDehumNum,     // Index number of the current zone dehumidifier being simulated
                              Real64 const QZnDehumidReq, // Dehumidification load to be met (kg/s), negative value means dehumidification load
                              Real64 &SensibleOutput,     // Sensible (heating) output (W), sent to load predictor for next simulation time step
                              Real64 &LatentOutput        // Latent (dehumidification) output provided (kg/s)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   July/Aug 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the delivered capacity, electric energy consumption and water/condensate
        // removal rates for the zone dehumidifier.

        // METHODOLOGY EMPLOYED:
        // Cycle the dehumidifier as needed to meet the remaining zone dehumidification load.
        // Send excess sensible heat to zone energy balance (via SensibleOutput) for next HVAC time step,
        // so set the dehumidifier outlet air temp = inlet air temp to avoid double counting excess sensible.

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHfgAirFnWTdb;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhFnTdbWPb;
        using Psychrometrics::RhoH2O;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcZoneDehumidifier");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 WaterRemovalRateFactor; // Adjustment to  Rate Water Removal as a function of inlet air T and RH
        Real64 WaterRemovalVolRate;    // Actual water removal rate at current inlet air conditions (L/day)
        Real64 WaterRemovalMassRate;   // Actual water removal rate at current inlet air conditions (kg/s)
        Real64 EnergyFactorAdjFactor;  // Adjustment to Rate Energy Factor as a function of inlet air T and RH
        Real64 EnergyFactor;           // Actual Energy Factor as a function of inlet air T and RH
        Real64 InletAirTemp;           // Dry-bulb temperature of air entering the dehumidifier (C)
        Real64 InletAirHumRat;         // Humidity ratio of the air entering the dehumidifier (kg/kg)
        Real64 InletAirRH;             // Relative humidity of air entering the dehumidifier (%)
        Real64 OutletAirTemp;          // Dry-bulb temperature of air leaving the dehumidifier (C)
        Real64 OutletAirHumRat;        // Humidity ratio of air leaving the dehumidifier (kg/kg)
        Real64 PLR;                    // Part-load ratio = (dehumid load to be met)/(dehumid capacity of the dehumidifier)
        Real64 PLF;                    // Part-load fraction (-), RuntimeFraction = PLR/PLF
        Real64 RunTimeFraction;        // Dehumidifier runtime fraction (-)
        Real64 ElectricPowerOnCycle;   // Electric power when dehumidifier is operating (W)
        Real64 ElectricPowerAvg;       // Average electric power for this dehumidifier (W)
        Real64 hfg;                    // Enthalpy of evaporation of inlet air (J/kg)
        Real64 AirMassFlowRate;        // Air mass flow rate through this dehumidifier (kg/s)
        Real64 Cp;                     // Heat capacity of inlet air (J/kg-C)
        int AirInletNodeNum(0);        // Node number for the inlet air to the dehumidifier
        int AirOutletNodeNum(0);       // Node number for the outlet air from the dehumidifier

        SensibleOutput = 0.0;
        LatentOutput = 0.0;
        WaterRemovalRateFactor = 0.0;
        AirMassFlowRate = 0.0;
        PLR = 0.0;
        PLF = 0.0;
        EnergyFactorAdjFactor = 0.0;
        RunTimeFraction = 0.0;
        ElectricPowerAvg = 0.0;
        ElectricPowerOnCycle = 0.0;

        AirInletNodeNum = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).AirInletNodeNum;
        AirOutletNodeNum = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).AirOutletNodeNum;

        InletAirTemp = state.dataLoopNodes->Node(AirInletNodeNum).Temp;
        InletAirHumRat = state.dataLoopNodes->Node(AirInletNodeNum).HumRat;
        InletAirRH = 100.0 * PsyRhFnTdbWPb(state, InletAirTemp, InletAirHumRat, state.dataEnvrn->OutBaroPress, RoutineName); // RH in percent (%)

        if (QZnDehumidReq < 0.0 && GetCurrentScheduleValue(state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).SchedPtr) > 0.0 &&
            InletAirTemp >= state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).MinInletAirTemp &&
            InletAirTemp <= state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).MaxInletAirTemp) {
            // A dehumidification load is being requested and dehumidifier is available (schedule value > 0)
            //  and the inlet air temperature is within the min/max values specified by user input

            WaterRemovalRateFactor =
                CurveValue(state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemovalCurveIndex, InletAirTemp, InletAirRH);
            // Warn user if curve output goes negative
            if (WaterRemovalRateFactor <= 0.0) {
                if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemovalCurveErrorCount < 1) {
                    ++state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemovalCurveErrorCount;
                    ShowWarningError(state,
                                     state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name + "\":");
                    ShowContinueError(state, format(" Water Removal Rate Curve output is <= 0.0 ({:.5T}).", WaterRemovalRateFactor));
                    ShowContinueError(
                        state,
                        format(
                            " Negative value occurs using an inlet air dry-bulb temperature of {:.2T} and an inlet air relative humidity of {:.1T}.",
                            InletAirTemp,
                            InletAirRH));
                    ShowContinueErrorTimeStamp(state, " Dehumidifier turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name +
                                                       "\": Water Removal Rate Curve output is <= 0.0 warning continues...",
                                                   state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemovalCurveErrorIndex,
                                                   WaterRemovalRateFactor,
                                                   WaterRemovalRateFactor);
                }
                WaterRemovalRateFactor = 0.0;
            }

            WaterRemovalVolRate = WaterRemovalRateFactor * state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedWaterRemoval;

            WaterRemovalMassRate =
                WaterRemovalVolRate / (24.0 * DataGlobalConstants::SecInHour * 1000.0) *
                RhoH2O(max((InletAirTemp - 11.0), 1.0)); //(L/d)/(24 hr/day *3600 sec/hr * 1000 L/m3) | Density of water, minimum temp = 1.0C

            if (WaterRemovalMassRate > 0.0) {
                PLR = max(0.0, min(1.0, -QZnDehumidReq / WaterRemovalMassRate));
            } else {
                PLR = 0.0;
                RunTimeFraction = 0.0;
            }

            EnergyFactorAdjFactor =
                CurveValue(state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).EnergyFactorCurveIndex, InletAirTemp, InletAirRH);

            // Warn user if curve output goes negative
            if (EnergyFactorAdjFactor <= 0.0) {
                if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).EnergyFactorCurveErrorCount < 1) {
                    ++state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).EnergyFactorCurveErrorCount;
                    ShowWarningError(state,
                                     state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name + "\":");
                    ShowContinueError(state, format(" Energy Factor Curve output is <= 0.0 ({:.5T}).", EnergyFactorAdjFactor));
                    ShowContinueError(
                        state,
                        format(
                            " Negative value occurs using an inlet air dry-bulb temperature of {:.2T} and an inlet air relative humidity of {:.1T}.",
                            InletAirTemp,
                            InletAirRH));
                    ShowContinueErrorTimeStamp(state, " Dehumidifier turned off for this time step but simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name +
                                                       "\": Energy Factor Curve output is <= 0.0 warning continues...",
                                                   state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).EnergyFactorCurveErrorIndex,
                                                   EnergyFactorAdjFactor,
                                                   EnergyFactorAdjFactor);
                }
                ElectricPowerAvg = 0.0;
                PLR = 0.0;
                RunTimeFraction = 0.0;
            } else {
                // EnergyFactorAdjFactor is not negative, so proceed with calculations
                EnergyFactor = EnergyFactorAdjFactor * state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedEnergyFactor;

                if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).PartLoadCurveIndex > 0) {
                    PLF = CurveValue(
                        state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).PartLoadCurveIndex, PLR); // Calculate part load fraction
                } else {
                    PLF = 1.0;
                }

                if (PLF < 0.7) {
                    if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).LowPLFErrorCount < 1) {
                        ++state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).LowPLFErrorCount;
                        ShowWarningError(state,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                             state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name + "\":");
                        ShowContinueError(
                            state, format(" The Part Load Fraction Correlation Curve output is ({:.2T}) at a part-load ratio ={:.3T}", PLF, PLR));
                        ShowContinueErrorTimeStamp(state,
                                                   " PLF curve values must be >= 0.7.  PLF has been reset to 0.7 and simulation is continuing.");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                                           state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name +
                                                           "\": Part Load Fraction Correlation Curve output < 0.7 warning continues...",
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).LowPLFErrorIndex,
                                                       PLF,
                                                       PLF);
                    }
                    PLF = 0.7;
                }

                if (PLF > 1.0) {
                    if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).HighPLFErrorCount < 1) {
                        ++state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).HighPLFErrorCount;
                        ShowWarningError(state,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                             state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name + "\":");
                        ShowContinueError(
                            state, format(" The Part Load Fraction Correlation Curve output is ({:.2T}) at a part-load ratio ={:.3T}", PLF, PLR));
                        ShowContinueErrorTimeStamp(state,
                                                   " PLF curve values must be < 1.0.  PLF has been reset to 1.0 and simulation is continuing.");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                                           state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name +
                                                           "\": Part Load Fraction Correlation Curve output > 1.0 warning continues...",
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).HighPLFErrorIndex,
                                                       PLF,
                                                       PLF);
                    }
                    PLF = 1.0;
                }

                if (PLF > 0.0 && PLF >= PLR) {
                    RunTimeFraction = PLR / PLF; // Calculate dehumidifier runtime fraction
                } else {
                    if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).PLFPLRErrorCount < 1) {
                        ++state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).PLFPLRErrorCount;
                        ShowWarningError(state,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                             state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name + "\":");
                        ShowContinueError(
                            state,
                            format("The part load fraction was less than the part load ratio calculated for this time step [PLR={:.4T}, PLF={:.4T}].",
                                   PLR,
                                   PLF));
                        ShowContinueError(state, "Runtime fraction reset to 1 and the simulation will continue.");
                        ShowContinueErrorTimeStamp(state, "");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                                           state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name +
                                                           "\": Part load fraction less than part load ratio warning continues...",
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).PLFPLRErrorIndex);
                    }
                    RunTimeFraction = 1.0;
                }

                if (RunTimeFraction > 1.0 && std::abs(RunTimeFraction - 1.0) > 0.001) {
                    if (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).HighRTFErrorCount < 1) {
                        ++state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).HighRTFErrorCount;
                        ShowWarningError(state,
                                         state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                             state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name + "\":");
                        ShowContinueError(state, format("The runtime fraction for this zone dehumidifier exceeded 1.0 [{:.4T}].", RunTimeFraction));
                        ShowContinueError(state, "Runtime fraction reset to 1 and the simulation will continue.");
                        ShowContinueErrorTimeStamp(state, "");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).UnitType + " \"" +
                                                           state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).Name +
                                                           "\": Runtime fraction for zone dehumidifier exceeded 1.0 warning continues...",
                                                       state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).HighRTFErrorIndex,
                                                       RunTimeFraction,
                                                       RunTimeFraction);
                    }
                    RunTimeFraction = 1.0;
                }

                // ElectricPowerOnCycle = Water removal volumetric rate (L/day) / (Energy Factor(L/kWh) * 24 hrs/day ) * 1000 Wh/kWh
                ElectricPowerOnCycle = WaterRemovalVolRate / (EnergyFactor * 24.0) * 1000.0; // Watts
                // ElectricPowerAvg     = ElectricPowerOnCycle * RTF + (1-RTF)*OffCycleParsiticLoad
                ElectricPowerAvg =
                    ElectricPowerOnCycle * RunTimeFraction +
                    (1.0 - RunTimeFraction) * state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OffCycleParasiticLoad; // average Watts
            }

            LatentOutput = WaterRemovalMassRate * PLR; // Average moisture removal rate, kg/s, for this timestep
            hfg = PsyHfgAirFnWTdb(InletAirHumRat, InletAirTemp);
            SensibleOutput = (LatentOutput * hfg) + ElectricPowerAvg; // Average sensible output, Watts
            // Send SensibleOutput to zone air heat balance via SysDepZoneLoads in ZoneEquipmentManager

            state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRate = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirMassFlow * PLR;
            AirMassFlowRate = state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRate; // Average air mass flow for this timestep
            Cp = PsyCpAirFnW(InletAirHumRat);                                          // Heat capacity of air
            if (AirMassFlowRate > 0.0 && Cp > 0.0) {
                OutletAirTemp = InletAirTemp + (ElectricPowerOnCycle + (WaterRemovalMassRate * hfg)) /
                                                   (state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).RatedAirMassFlow * Cp);
                OutletAirHumRat = InletAirHumRat - LatentOutput / AirMassFlowRate;
            } else {
                OutletAirTemp = InletAirTemp;
                OutletAirHumRat = InletAirHumRat;
            }

        } else {

            // No load or not available or inlet air temps beyond min/max limits, then set outlet conditions
            // equal to inlet conditions and PLR = RTF = 0.0
            OutletAirTemp = InletAirTemp;
            OutletAirHumRat = InletAirHumRat;
            PLR = 0.0;
            RunTimeFraction = 0.0;
            state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRate = 0.0;
            // If available but didn't operate, then set electric power = off cycle parasitic load.
            // Else, electric power = 0.0
            if (GetCurrentScheduleValue(state, state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).SchedPtr) > 0.0) {
                ElectricPowerAvg =
                    state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OffCycleParasiticLoad; // off cycle parasitic is on entire timestep
            } else {
                ElectricPowerAvg = 0.0;
            }
        }

        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OutletAirTemp =
            OutletAirTemp; // Update report variable here. Node outlet Temp set equal
        //   to Node inlet Temp in Update subroutine
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OutletAirHumRat =
            OutletAirHumRat; // Store in structure, updated outlet node in Update subroutine

        // Use inlet air temperature in outlet air enthalpy calculation... since the sensible heat output
        // from the dehumidifier is being sent directly to the zone air heat balance for next hvac simulation time step
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OutletAirEnthalpy = PsyHFnTdbW(InletAirTemp, OutletAirHumRat);

        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).SensHeatingRate =
            SensibleOutput; // Report variable update, W,  avg sens output when unit is 'on'
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).WaterRemovalRate = LatentOutput; // Report variable update, kg/s
        LatentOutput = -LatentOutput; // change sign... negative is dehumidification in zone air balance

        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OffCycleParasiticElecPower =
            (1.0 - RunTimeFraction) * state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OffCycleParasiticLoad;
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).ElecPower = ElectricPowerAvg;
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).DehumidPLR = PLR;
        state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).DehumidRTF = RunTimeFraction;
    }

    void UpdateZoneDehumidifier(EnergyPlusData &state, int const ZoneDehumNum) // Number of the current zone dehumidifier being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for passing results to the outlet air node.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNodeNum;  // Node number corresponding to the air entering dehumidifier
        int AirOutletNodeNum; // Node number corresponding to the air leaving dehumidifier

        AirInletNodeNum = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).AirInletNodeNum;
        AirOutletNodeNum = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).AirOutletNodeNum;

        // Changed outlet node properties
        state.dataLoopNodes->Node(AirOutletNodeNum).Enthalpy = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OutletAirEnthalpy;
        state.dataLoopNodes->Node(AirOutletNodeNum).HumRat = state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumNum).OutletAirHumRat;
        // Set outlet temp = inlet temp; send excess sensible heat directly to air heat balance
        // (via SensibleOutput and QSensOut) for the next hvac simulation time step.
        state.dataLoopNodes->Node(AirOutletNodeNum).Temp = state.dataLoopNodes->Node(AirInletNodeNum).Temp;

        // Pass through output node properties
        state.dataLoopNodes->Node(AirOutletNodeNum).Quality = state.dataLoopNodes->Node(AirInletNodeNum).Quality;
        state.dataLoopNodes->Node(AirOutletNodeNum).Press = state.dataLoopNodes->Node(AirInletNodeNum).Press;
        state.dataLoopNodes->Node(AirOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRate;
        state.dataLoopNodes->Node(AirOutletNodeNum).MassFlowRateMin = state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRateMin;
        state.dataLoopNodes->Node(AirOutletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRateMax;
        state.dataLoopNodes->Node(AirOutletNodeNum).MassFlowRateMinAvail = state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(AirOutletNodeNum).MassFlowRateMaxAvail = state.dataLoopNodes->Node(AirInletNodeNum).MassFlowRateMaxAvail;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(AirOutletNodeNum).CO2 = state.dataLoopNodes->Node(AirInletNodeNum).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(AirOutletNodeNum).GenContam = state.dataLoopNodes->Node(AirInletNodeNum).GenContam;
        }
    }

    void ReportZoneDehumidifier(EnergyPlusData &state, int const DehumidNum) // Index of the current zone dehumidifier being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey, FSEC
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Fills some of the report variables for the zone dehumidifiers

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using Psychrometrics::RhoH2O;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J
        Real64 RhoWater;          // Density of condensate (water) being removed (kg/m3)
        Real64 InletAirTemp;      // Dry-bulb temperature of air entering the dehumidifier (C)
        Real64 OutletAirTemp;     // Dry-bulb temperature of air leaving the dehumidifier (C)
        int AirInletNodeNum;      // Node number corresponding to the air entering dehumidifier

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).SensHeatingEnergy =
            state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).SensHeatingRate * ReportingConstant;
        state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).WaterRemoved =
            state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).WaterRemovalRate * ReportingConstant;
        state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).ElecConsumption =
            state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).ElecPower * ReportingConstant;
        state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).OffCycleParasiticElecCons =
            state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).OffCycleParasiticElecPower * ReportingConstant;

        // Dehumidifier water collection to water storage tank (if needed)
        if (state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).CondensateCollectMode == state.dataZoneDehumidifier->CondensateToTank) {
            // Calculate and report condensation rate (how much water extracted from the air stream)
            // Volumetric flow of water in m3/s for water system interactions

            AirInletNodeNum = state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).AirInletNodeNum;
            InletAirTemp = state.dataLoopNodes->Node(AirInletNodeNum).Temp;
            OutletAirTemp = max((InletAirTemp - 11.0), 1.0); // Assume coil outlet air is 11C (20F) lower than inlet air temp
            RhoWater = RhoH2O(OutletAirTemp);                // Density of water, minimum temp = 1.0 C

            if (RhoWater > 0.0) {
                state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).DehumidCondVolFlowRate =
                    state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).WaterRemovalRate / RhoWater;
            }

            state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).DehumidCondVol =
                state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).DehumidCondVolFlowRate * ReportingConstant;

            state.dataWaterData->WaterStorage(state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).CondensateTankID)
                .VdotAvailSupply(state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).CondensateTankSupplyARRID) =
                state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).DehumidCondVolFlowRate;
            // Assume water outlet temp = air outlet temp.... same assumption in other places in code (e.g., water coil component)
            state.dataWaterData->WaterStorage(state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).CondensateTankID)
                .TwaterSupply(state.dataZoneDehumidifier->ZoneDehumid(DehumidNum).CondensateTankSupplyARRID) = OutletAirTemp;
        }
    }

    bool GetZoneDehumidifierNodeNumber(EnergyPlusData &state, int const NodeNumber) // Node being tested
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // After making sure get input is done, the node number of indicated
        // zone dehumidifier is returned.

        // Return value
        bool FindZoneDehumidifierNodeNumber; // Zone Dehumidifier Node Number Check

        int ZoneDehumidIndex; // Loop index

        if (state.dataZoneDehumidifier->GetInputFlag) {
            GetZoneDehumidifierInput(state);
            state.dataZoneDehumidifier->GetInputFlag = false;
        }

        FindZoneDehumidifierNodeNumber = false;
        for (ZoneDehumidIndex = 1; ZoneDehumidIndex <= state.dataZoneDehumidifier->NumDehumidifiers; ++ZoneDehumidIndex) {
            if (NodeNumber == state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).AirInletNodeNum) {
                FindZoneDehumidifierNodeNumber = true;
                break;
            }
            if (NodeNumber == state.dataZoneDehumidifier->ZoneDehumid(ZoneDehumidIndex).AirOutletNodeNum) {
                FindZoneDehumidifierNodeNumber = true;
                break;
            }
        }

        return FindZoneDehumidifierNodeNumber;
    }

} // namespace ZoneDehumidifier

} // namespace EnergyPlus
