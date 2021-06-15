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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CondenserLoopTowers.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>

namespace EnergyPlus {

namespace CondenserLoopTowers {

    // Module containing the routines dealing with the objects COOLING TOWER:SINGLE SPEED,
    // COOLING TOWER:TWO SPEED, and COOLING TOWER:VARIABLE SPEED

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   April 1998
    //       MODIFIED       Shirey, Raustad: Dec 2000; Shirey, Sept 2002, Raustad Mar 2005
    //                      B Griffith Aug 2006, added water consumption and water system interactions
    //                      T Hong, Aug 2008. Added fluid bypass for single speed cooling tower
    //                      Chandan Sharma, FSEC, February 2010, Added basin heater
    //                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Model the performance of cooling towers

    std::string const cCoolingTower_SingleSpeed("CoolingTower:SingleSpeed");
    std::string const cCoolingTower_TwoSpeed("CoolingTower:TwoSpeed");
    std::string const cCoolingTower_VariableSpeed("CoolingTower:VariableSpeed");
    std::string const cCoolingTower_VariableSpeedMerkel("CoolingTower:VariableSpeed:Merkel");

    PlantComponent *CoolingTower::factory(EnergyPlusData &state, std::string_view objectName)
    {
        // Process the input data for towers if it hasn't been done already
        if (state.dataCondenserLoopTowers->GetInput) {
            GetTowerInput(state);
            state.dataCondenserLoopTowers->GetInput = false;
        }
        // Now look for this particular tower in the list
        for (auto &tower : state.dataCondenserLoopTowers->towers) {
            if (tower.Name == objectName) {
                return &tower;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state, "CoolingTowerFactory: Error getting inputs for tower named: " + std::string{objectName}); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void CoolingTower::simulate(EnergyPlusData &state,
                                [[maybe_unused]] const PlantLocation &calledFromLocation,
                                [[maybe_unused]] bool const FirstHVACIteration,
                                Real64 &CurLoad,
                                bool const RunFlag)
    {
        this->initialize(state);
        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd) {
            this->calculateSingleSpeedTower(state);
        } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
            this->calculateTwoSpeedTower(state);
        } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpdMerkel) {
            this->calculateMerkelVariableSpeedTower(state, CurLoad);
        } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpd) {
            this->calculateVariableSpeedTower(state);
        }
        this->calculateWaterUsage(state);
        this->update(state);
        this->report(state, RunFlag);
    }

    void CoolingTower::getDesignCapacities([[maybe_unused]] EnergyPlusData &state,
                                           [[maybe_unused]] const PlantLocation &calledFromLocation,
                                           Real64 &MaxLoad,
                                           Real64 &MinLoad,
                                           Real64 &OptLoad)
    {
        MinLoad = 0.0;
        MaxLoad = this->TowerNominalCapacity * this->HeatRejectCapNomCapSizingRatio;
        OptLoad = this->TowerNominalCapacity;
    }

    void CoolingTower::getSizingFactor(Real64 &SizFactor)
    {
        SizFactor = this->SizFac;
    }

    void CoolingTower::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
    {
        this->initialize(state);
        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpdMerkel) {
            this->SizeVSMerkelTower(state);
        } else {
            this->SizeTower(state);
        }
    }

    void GetTowerInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 1998
        //       MODIFIED         Don Shirey, Jan 2001 and Sept/Oct 2002; Richard Raustad, FSEC, Feb 2005 (added VS tower)
        //                        B. Griffith, Aug. 2006 water consumption modeling and water system connections
        //                        T Hong, Aug. 2008: added fluid bypass for single speed tower
        //                        A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for cooling towers and stores it in towers data structure. Additional structure
        // (VSTower) stores the coefficients for each VS tower.

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in the data.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr fmt::string_view OutputFormat("{:5.2F}");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TowerNum;                      // Tower number, reference counter for towers data array
        int NumSingleSpeedTowers;          // Total number of single-speed cooling towers
        int SingleSpeedTowerNumber;        // Specific single-speed tower of interest
        int NumTwoSpeedTowers;             // Number of two-speed cooling towers
        int TwoSpeedTowerNumber;           // Specific two-speed tower of interest
        int NumVariableSpeedTowers;        // Number of variable-speed cooling towers
        int VariableSpeedTowerNumber;      // Specific variable-speed tower of interest
        int NumVSCoolToolsModelCoeffs = 0; // Number of CoolTools VS cooling tower coefficient objects
        int NumVSYorkCalcModelCoeffs = 0;  // Number of YorkCalc VS cooling tower coefficient objects
        int NumVSMerkelTowers;             // Number of Merkel variable speed cooling towers
        int MerkelVSTowerNum;              // specific merkel variable speed tower of interest
        int VSModelCoeffNum;               // Specific variable-speed tower coefficient object of interest
        int NumAlphas;                     // Number of elements in the alpha array
        int NumNums;                       // Number of elements in the numeric array
        int NumAlphas2;                    // Number of elements in the alpha2 array
        int NumNums2;                      // Number of elements in the numeric2 array
        int IOStat;                        // IO Status when calling get input subroutine
        int CoeffNum;                      // Index for reading user defined VS tower coefficients
        bool ErrorsFound(false);           // Logical flag set .TRUE. if errors found while getting input data
        std::string OutputChar;            // report variable for warning messages
        std::string OutputCharLo;          // report variable for warning messages
        std::string OutputCharHi;          // report variable for warning messages
        Array1D<Real64> NumArray(33);      // Numeric input data array
        Array1D<Real64> NumArray2(43);     // Numeric input data array for VS tower coefficients
        Array1D_string AlphArray(16);      // Character string input data array
        Array1D_string AlphArray2(1);      // Character string input data array for VS tower coefficients

        // Get number of all cooling towers specified in the input data file (idf)
        NumSingleSpeedTowers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCoolingTower_SingleSpeed);
        NumTwoSpeedTowers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCoolingTower_TwoSpeed);
        NumVariableSpeedTowers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCoolingTower_VariableSpeed);
        NumVSMerkelTowers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCoolingTower_VariableSpeedMerkel);
        state.dataCondenserLoopTowers->NumSimpleTowers = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers;

        if (state.dataCondenserLoopTowers->NumSimpleTowers <= 0)
            ShowFatalError(state,
                           "No Cooling Tower objects found in input, however, a branch object has specified a cooling tower. Search the input for "
                           "CoolingTower to determine the cause for this error.");

        state.dataCondenserLoopTowers->GetInput = false;
        // See if load distribution manager has already gotten the input
        if (allocated(state.dataCondenserLoopTowers->towers)) return;

        // Allocate data structures to hold tower input data, report data and tower inlet conditions
        state.dataCondenserLoopTowers->towers.allocate(state.dataCondenserLoopTowers->NumSimpleTowers);
        state.dataCondenserLoopTowers->UniqueSimpleTowerNames.reserve(state.dataCondenserLoopTowers->NumSimpleTowers);
        // Allocate variable-speed tower structure with data specific to this type
        if (NumVariableSpeedTowers > 0) {
            // Allow users to input model coefficients other than default
            NumVSCoolToolsModelCoeffs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "CoolingTowerPerformance:CoolTools");
            NumVSYorkCalcModelCoeffs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "CoolingTowerPerformance:YorkCalc");
        }

        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
        // Load data structures with cooling tower input data
        cCurrentModuleObject = cCoolingTower_SingleSpeed;
        for (SingleSpeedTowerNumber = 1; SingleSpeedTowerNumber <= NumSingleSpeedTowers; ++SingleSpeedTowerNumber) {
            TowerNum = SingleSpeedTowerNumber;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     SingleSpeedTowerNumber,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCondenserLoopTowers->UniqueSimpleTowerNames,
                                                     AlphArray(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            state.dataCondenserLoopTowers->towers(TowerNum).Name = AlphArray(1);
            state.dataCondenserLoopTowers->towers(TowerNum).thisTowerNum = TowerNum;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType = cCurrentModuleObject;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType_Num = DataPlant::TypeOf_CoolingTower_SingleSpd;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerMassFlowRateMultiplier = 2.5;
            state.dataCondenserLoopTowers->towers(TowerNum).WaterInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(2),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            state.dataCondenserLoopTowers->towers(TowerNum).WaterOutletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(3),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");
            state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate = NumArray(1);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate = NumArray(2);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower = NumArray(3);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA = NumArray(4);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate = NumArray(5);
            if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRateSizingFactor = NumArray(6);
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA = NumArray(7);
            if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUASizingFactor = NumArray(8);
            state.dataCondenserLoopTowers->towers(TowerNum).HeatRejectCapNomCapSizingRatio = NumArray(9);
            state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity = NumArray(10);
            if (state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacityWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap = NumArray(11);
            if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCapWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCapSizingFactor = NumArray(12);
            if (NumAlphas >= 4) {
                if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
                    state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::UFactor;
                } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
                    state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::NominalCapacity;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ErrorsFound = true;
                }
            } else {
                // Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
                state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::UFactor;
            }
            // cooling tower design inlet conditions
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp = NumArray(13);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp = 35.0;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp = NumArray(14);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp = 25.6;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesApproach = NumArray(15);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesApproach == DataSizing::AutoSize ||
                state.dataCondenserLoopTowers->towers(TowerNum).DesApproach == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesApproach = 3.9;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesRange = NumArray(16);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesRange == DataSizing::AutoSize ||
                state.dataCondenserLoopTowers->towers(TowerNum).DesRange == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesRange = 5.5;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            // set tower design water outlet and inlet temperatures
            state.dataCondenserLoopTowers->towers(TowerNum).DesOutletWaterTemp =
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp + state.dataCondenserLoopTowers->towers(TowerNum).DesApproach;
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletWaterTemp =
                state.dataCondenserLoopTowers->towers(TowerNum).DesOutletWaterTemp + state.dataCondenserLoopTowers->towers(TowerNum).DesRange;
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff = NumArray(17);
            if (NumArray(17) < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = NumArray(18);

            if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 18) {
                    state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ":\"" + state.dataCondenserLoopTowers->towers(TowerNum).Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(18) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!AlphArray(5).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, AlphArray(5));
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                         "\" basin heater schedule name \"" + AlphArray(5) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(6), "LossFactor")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::UserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(6), "SaturatedExit")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else if (AlphArray(6).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + AlphArray(6));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).UserEvapLossFactor = NumArray(19);        //  N11 , \field Evaporation Loss Factor
            state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = NumArray(20) / 100.0; //  N12, \field Drift Loss Percent

            if ((NumNums < 20) && (state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction == 0.0)) {
                // assume Drift loss not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = 0.008 / 100.0;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = NumArray(21); //  N13, \field Blowdown Concentration Ratio
            state.dataCondenserLoopTowers->towers(TowerNum).SizFac = NumArray(25);             //  N17  \field Sizing Factor
            if (state.dataCondenserLoopTowers->towers(TowerNum).SizFac <= 0.0) state.dataCondenserLoopTowers->towers(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(7), "ScheduledRate")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Schedule;
            } else if (UtilityRoutines::SameString(AlphArray(7), "ConcentrationRatio")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
            } else if (AlphArray(7).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
                if ((NumNums < 21) && (state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio == 0.0)) {
                    // assume Concetratino ratio was omitted and should be defaulted
                    state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = 3.0;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(7) + " = " + AlphArray(7));
                ErrorsFound = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(state, AlphArray(8));
            if ((state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown == 0) &&
                (state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode == Blowdown::Schedule)) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(8) + " = " + AlphArray(8));
                ErrorsFound = true;
            }

            if (AlphArray(9).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(state,
                                                       AlphArray(1),
                                                       cCurrentModuleObject,
                                                       AlphArray(9),
                                                       ErrorsFound,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankID,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankDemandARRID);
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node

            if (state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(10),
                                                        ErrorsFound,
                                                        cCurrentModuleObject,
                                                        state.dataCondenserLoopTowers->towers(TowerNum).Name,
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(10));
                    ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            //   fluid bypass for single speed tower
            if (state.dataIPShortCut->lAlphaFieldBlanks(11) || AlphArray(11).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).CapacityControl = CapacityCtrlEnum::FanCycling; // FanCycling
            } else {
                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(AlphArray(11)));
                    if (SELECT_CASE_var == "FANCYCLING") {
                        state.dataCondenserLoopTowers->towers(TowerNum).CapacityControl = CapacityCtrlEnum::FanCycling;
                    } else if (SELECT_CASE_var == "FLUIDBYPASS") {
                        state.dataCondenserLoopTowers->towers(TowerNum).CapacityControl = CapacityCtrlEnum::FluidBypass;
                    } else {
                        state.dataCondenserLoopTowers->towers(TowerNum).CapacityControl = CapacityCtrlEnum::FanCycling;
                        ShowWarningError(state,
                                         cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\" The Capacity Control is not specified correctly. The default Fan Cycling is used.");
                    }
                }
            }

            // added for multi-cell
            state.dataCondenserLoopTowers->towers(TowerNum).NumCell = NumArray(22);
            if ((NumNums < 22) && (state.dataCondenserLoopTowers->towers(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).NumCell = 1;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = NumArray(23);
            if ((NumNums < 23) && (state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = 0.33;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = NumArray(24);
            if ((NumNums < 24) && (state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = 2.5;
            }

            if (NumAlphas >= 12) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(12) || AlphArray(12).empty()) {
                    state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(12), "MinimalCell") || UtilityRoutines::SameString(AlphArray(12), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(12), "MinimalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(12), "MaximalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                        }
                    } else {
                        ShowSevereError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(12) + " = " + AlphArray(12));
                        ShowContinueError(state,
                                          "Occurs in " + state.dataCondenserLoopTowers->towers(TowerNum).TowerType + '=' +
                                              state.dataCondenserLoopTowers->towers(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
            }

            //   High speed air flow rate must be greater than free convection air flow rate.
            //   Can't tell yet if autosized, check later in initialize.
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate <=
                    state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate &&
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate != DataSizing::AutoSize) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". Free convection air flow rate must be less than the design air flow rate.");
                ErrorsFound = true;
            }

            //   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
            if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num == PIM::UFactor) {
                if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower performance input method requires a design water flow rate greater than zero.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA <=
                        state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA &&
                    state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA != DataSizing::AutoSize) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Free convection UA must be less than the design tower UA.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA > 0.0 &&
                    state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Free convection air flow rate must be greater than zero when free convection UA is greater than zero.");
                    ErrorsFound = true;
                }
            } else if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num == PIM::NominalCapacity) {
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower performance input method requires valid nominal capacity.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal capacity input method and design water flow rate have been specified.");
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Nominal capacity input method has been specified and design water flow rate is being autosized.");
                    }
                    ShowContinueError(state, "Design water flow rate will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal tower capacity and design tower UA have been specified.");
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Nominal tower capacity has been specified and design tower UA is being autosized.");
                    }
                    ShowContinueError(state, "Design tower UA will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal capacity input method and free convection UA have been specified.");
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Nominal capacity input method has been specified and free convection UA is being autosized.");
                    }
                    ShowContinueError(state, "Free convection UA will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap >=
                    state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Free convection nominal capacity must be less than the nominal (design) tower capacity.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap > 0.0 &&
                    state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Free convection air flow must be greater than zero when tower free convection capacity is specified.");
                    ErrorsFound = true;
                }
            } else { // Tower performance input method is not specified as a valid "choice"
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    R"(". Tower Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
                ShowContinueError(state, "Tower Performanace Input Method currently specified as: " + AlphArray(4));
                ErrorsFound = true;
            }
            if (NumAlphas > 12) {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = AlphArray(13);
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = "General";
            }
        } // End Single-Speed Tower Loop

        cCurrentModuleObject = cCoolingTower_TwoSpeed;
        for (TwoSpeedTowerNumber = 1; TwoSpeedTowerNumber <= NumTwoSpeedTowers; ++TwoSpeedTowerNumber) {
            TowerNum = NumSingleSpeedTowers + TwoSpeedTowerNumber;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     TwoSpeedTowerNumber,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCondenserLoopTowers->UniqueSimpleTowerNames,
                                                     AlphArray(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            state.dataCondenserLoopTowers->towers(TowerNum).Name = AlphArray(1);
            state.dataCondenserLoopTowers->towers(TowerNum).thisTowerNum = TowerNum;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType = cCurrentModuleObject;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType_Num = DataPlant::TypeOf_CoolingTower_TwoSpd;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerMassFlowRateMultiplier = 2.5;
            state.dataCondenserLoopTowers->towers(TowerNum).WaterInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(2),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            state.dataCondenserLoopTowers->towers(TowerNum).WaterOutletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(3),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            if (NumAlphas >= 4) {
                if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
                    state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::UFactor;
                } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
                    state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::NominalCapacity;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ErrorsFound = true;
                }
            } else {
                // Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
                state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::UFactor;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate = NumArray(1);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate = NumArray(2);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower = NumArray(3);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA = NumArray(4);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRate = NumArray(5);
            if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRateWasAutoSized = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRateSizingFactor = NumArray(6);
            state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedFanPower = NumArray(7);
            if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedFanPower == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedFanPowerWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedFanPowerSizingFactor = NumArray(8);
            state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA = NumArray(9);
            if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUASizingFactor = NumArray(10);
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate = NumArray(11);
            if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRateSizingFactor = NumArray(12);
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA = NumArray(13);
            if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUASizingFactor = NumArray(14);
            state.dataCondenserLoopTowers->towers(TowerNum).HeatRejectCapNomCapSizingRatio = NumArray(15);
            state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity = NumArray(16);

            state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCap = NumArray(17);
            if (state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCap == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCapWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCapSizingFactor = NumArray(18);
            state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap = NumArray(19);
            if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCapWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCapSizingFactor = NumArray(20);
            // cooling tower design inlet conditions
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp = NumArray(21);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp = 35.0;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp = NumArray(22);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp = 25.6;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesApproach = NumArray(23);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesApproach == DataSizing::AutoSize ||
                state.dataCondenserLoopTowers->towers(TowerNum).DesApproach == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesApproach = 3.9;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesRange = NumArray(24);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesRange == DataSizing::AutoSize ||
                state.dataCondenserLoopTowers->towers(TowerNum).DesRange == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesRange = 5.5;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            // set tower design water outlet and inlet temperatures
            state.dataCondenserLoopTowers->towers(TowerNum).DesOutletWaterTemp =
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp + state.dataCondenserLoopTowers->towers(TowerNum).DesApproach;
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletWaterTemp =
                state.dataCondenserLoopTowers->towers(TowerNum).DesOutletWaterTemp + state.dataCondenserLoopTowers->towers(TowerNum).DesRange;
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff = NumArray(25);
            if (NumArray(25) < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = NumArray(26);
            if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 26) {
                    state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ":\"" + state.dataCondenserLoopTowers->towers(TowerNum).Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(26) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!AlphArray(5).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, AlphArray(5));
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                         "\" basin heater schedule name \"" + AlphArray(5) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(6), "LossFactor")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::UserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(6), "SaturatedExit")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + AlphArray(6));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).UserEvapLossFactor = NumArray(27);        //  N23 , \field Evaporation Loss Factor
            state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = NumArray(28) / 100.0; //  N24, \field Drift Loss Percent
            if ((NumNums < 28) && (state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction == 0.0)) {
                // assume Drift loss not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = 0.008 / 100.0;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = NumArray(29); //  N17, \field Blowdown Concentration Ratio
            state.dataCondenserLoopTowers->towers(TowerNum).SizFac = NumArray(33);             //  N21  \field Sizing Factor
            if (state.dataCondenserLoopTowers->towers(TowerNum).SizFac <= 0.0) state.dataCondenserLoopTowers->towers(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(7), "ScheduledRate")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Schedule;
            } else if (UtilityRoutines::SameString(AlphArray(7), "ConcentrationRatio")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
                if ((NumNums < 29) && (state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio == 0.0)) {
                    // assume concentration ratio was omitted and should be defaulted
                    state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = 3.0;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphArray(7));
                ErrorsFound = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(state, AlphArray(8));
            if ((state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown == 0) &&
                (state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode == Blowdown::Schedule)) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphArray(8));
                ErrorsFound = true;
            }

            // added for multi-cell
            state.dataCondenserLoopTowers->towers(TowerNum).NumCell = NumArray(30);
            if ((NumNums < 30) && (state.dataCondenserLoopTowers->towers(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).NumCell = 1;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = NumArray(31);
            if ((NumNums < 31) && (state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = 0.33;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = NumArray(32);
            if ((NumNums < 32) && (state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = 2.5;
            }

            if (NumAlphas >= 11) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(11) || AlphArray(11).empty()) {
                    state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(11), "MinimalCell") || UtilityRoutines::SameString(AlphArray(11), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(11), "MinimalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(11), "MaximalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                        }
                    } else {
                        ShowSevereError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(12) + " = " + AlphArray(12));
                        ShowContinueError(state,
                                          "Occurs in " + state.dataCondenserLoopTowers->towers(TowerNum).TowerType + '=' +
                                              state.dataCondenserLoopTowers->towers(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(state,
                                                       AlphArray(1),
                                                       cCurrentModuleObject,
                                                       AlphArray(9),
                                                       ErrorsFound,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankID,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankDemandARRID);
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node
            if (state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(10),
                                                        ErrorsFound,
                                                        cCurrentModuleObject,
                                                        state.dataCondenserLoopTowers->towers(TowerNum).Name,
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(10));
                    ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            //   High speed air flow rate must be greater than low speed air flow rate.
            //   Can't tell yet if autosized, check later in initialize.
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate <=
                    state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRate &&
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate != DataSizing::AutoSize) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". Low speed air flow rate must be less than the high speed air flow rate.");
                ErrorsFound = true;
            }
            //   Low speed air flow rate must be greater than free convection air flow rate.
            //   Can't tell yet if autosized, check later in initialize.
            if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRate <=
                    state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate &&
                state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedAirFlowRate != DataSizing::AutoSize) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". Free convection air flow rate must be less than the low speed air flow rate.");
                ErrorsFound = true;
            }

            //   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
            if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num == PIM::UFactor) {
                if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower performance input method requires a design water flow rate greater than zero.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA <=
                        state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA &&
                    state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA != DataSizing::AutoSize) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower UA at low fan speed must be less than the tower UA at high fan speed.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA <=
                        state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA &&
                    state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA != DataSizing::AutoSize) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA > 0.0 &&
                    state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Free convection air flow rate must be greater than zero when free convection UA is greater than zero.");
                    ErrorsFound = true;
                }
            } else if (state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num == PIM::NominalCapacity) {
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower performance input method requires valid high-speed nominal capacity.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCap == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Tower performance input method requires valid low-speed nominal capacity.");
                    ErrorsFound = true;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal capacity input method and design water flow rate have been specified.");
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Nominal capacity input method has been specified and design water flow rate is being autosized.");
                    }
                    ShowContinueError(state, "Design water flow rate will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal capacity input method and tower UA at high fan speed have been specified.");
                    } else {
                        ShowSevereError(
                            state,
                            cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                "\". Nominal capacity input method has been specified and tower UA at high fan speed is being autosized.");
                    }
                    ShowContinueError(state, "Tower UA at high fan speed will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).LowSpeedTowerUA > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal capacity input method and tower UA at low fan speed have been specified.");
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Nominal capacity input method has been specified and tower UA at low fan speed is being autosized.");
                    }
                    ShowContinueError(state, "Tower UA at low fan speed will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA != 0.0) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA > 0.0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                             "\". Nominal capacity input method and free convection UA have been specified.");
                    } else {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Nominal capacity input method has been specified and free convection UA is being autosized.");
                    }
                    ShowContinueError(state, "Free convection UA will be set according to nominal tower capacity.");
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCap >=
                    state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Low-speed nominal capacity must be less than the high-speed nominal capacity.");
                    ErrorsFound = true;
                }
                if (!state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCapWasAutoSized) {
                    if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap >=
                        state.dataCondenserLoopTowers->towers(TowerNum).TowerLowSpeedNomCap) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". Free convection nominal capacity must be less than the low-speed nominal capacity.");
                        ErrorsFound = true;
                    }
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap > 0.0 &&
                    state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". Free convection air flow must be greater than zero when tower free convection capacity is specified.");
                    ErrorsFound = true;
                }
            } else { // Tower performance input method is not specified as a valid "choice"
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    R"(". Tower Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
                ShowContinueError(state, "Tower Performanace Input Method currently specified as: " + AlphArray(4));
                ErrorsFound = true;
            }
            if (NumAlphas > 11) {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = AlphArray(12);
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = "General";
            }
        } // End Two-Speed Tower Loop

        cCurrentModuleObject = cCoolingTower_VariableSpeed;
        for (VariableSpeedTowerNumber = 1; VariableSpeedTowerNumber <= NumVariableSpeedTowers; ++VariableSpeedTowerNumber) {
            TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + VariableSpeedTowerNumber;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     VariableSpeedTowerNumber,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCondenserLoopTowers->UniqueSimpleTowerNames,
                                                     AlphArray(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            state.dataCondenserLoopTowers->towers(TowerNum).VSTower = VariableSpeedTowerNumber;
            state.dataCondenserLoopTowers->towers(TowerNum).Name = AlphArray(1);
            state.dataCondenserLoopTowers->towers(TowerNum).thisTowerNum = TowerNum;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType = cCurrentModuleObject;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType_Num = DataPlant::TypeOf_CoolingTower_VarSpd;
            state.dataCondenserLoopTowers->towers(TowerNum).WaterInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(2),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            state.dataCondenserLoopTowers->towers(TowerNum).WaterOutletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(3),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            if ((UtilityRoutines::SameString(AlphArray(4), "CoolToolsUserDefined") ||
                 UtilityRoutines::SameString(AlphArray(4), "YorkCalcUserDefined")) &&
                state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name + "\" a " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + " must be specified when " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " is specified as CoolToolsUserDefined or YorkCalcUserDefined");
                ErrorsFound = true;
            } else if ((UtilityRoutines::SameString(AlphArray(4), "CoolToolsCrossFlow") || UtilityRoutines::SameString(AlphArray(4), "YorkCalc")) &&
                       !state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                ShowWarningError(
                    state,
                    cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                        "\" a Tower Model Coefficient Name is specified and the Tower Model Type is not specified as CoolToolsUserDefined "
                        "or YorkCalcUserDefined. The CoolingTowerPerformance:CoolTools (orCoolingTowerPerformance:YorkCalc) data object "
                        "will not be used.");
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName = AlphArray(5);
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                state.dataCondenserLoopTowers->towers(TowerNum).FanPowerfAirFlowCurve = CurveManager::GetCurveIndex(state, AlphArray(6));
                if (state.dataCondenserLoopTowers->towers(TowerNum).FanPowerfAirFlowCurve == 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                         "\" the Fan Power Ratio as a function of Air Flow Rate Ratio Curve Name specified as " + AlphArray(6) +
                                         " was not found. Fan Power as a function of Air Flow Rate Ratio will default to Fan Power = (Air Flow Rate "
                                         "Ratio)^3 and the simulation continues.");
                }
            }

            state.dataCondenserLoopTowers->towers(VariableSpeedTowerNumber).Coeff.allocate(35);
            state.dataCondenserLoopTowers->towers(VariableSpeedTowerNumber).Coeff = 0.0;

            if (UtilityRoutines::SameString(AlphArray(4), "CoolToolsCrossFlow")) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerModelType = ModelType::CoolToolsXFModel;
                //     set cross-flow model coefficients
                //       Outputs approach in C
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(1) = 0.52049709836241;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(2) = -10.617046395344;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(3) = 10.7292974722538;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(4) = -2.74988377158227;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(5) = 4.73629943913743;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(6) = -8.25759700874711;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(7) = 1.57640938114136;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(8) = 6.51119643791324;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(9) = 1.50433525206692;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(10) = -3.2888529287801;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(11) = 0.0257786145353773;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(12) = 0.182464289315254;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(13) = -0.0818947291400898;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(14) = -0.215010003996285;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(15) = 0.0186741309635284;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(16) = 0.0536824177590012;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(17) = -0.00270968955115031;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(18) = 0.00112277498589279;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(19) = -0.00127758497497718;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(20) = 0.0000760420796601607;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(21) = 1.43600088336017;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(22) = -0.5198695909109;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(23) = 0.117339576910507;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(24) = 1.50492810819924;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(25) = -0.135898905926974;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(26) = -0.152577581866506;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(27) = -0.0533843828114562;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(28) = 0.00493294869565511;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(29) = -0.00796260394174197;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(30) = 0.000222619828621544;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(31) = -0.0543952001568055;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(32) = 0.00474266879161693;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(33) = -0.0185854671815598;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(34) = 0.00115667701293848;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(35) = 0.000807370664460284;

                //       set minimum and maximum boundaries for CoolTools crossflow model input variables
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinInletAirWBTemp = -1.0;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxInletAirWBTemp = 26.6667;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp = 1.1111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp = 11.1111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp = 1.1111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp = 11.1111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinWaterFlowRatio = 0.75;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxWaterFlowRatio = 1.25;

            } else if (UtilityRoutines::SameString(AlphArray(4), "YorkCalc")) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerModelType = ModelType::YorkCalcModel;
                //     set counter-flow model coefficients
                //       Outputs approach in C
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(1) = -0.359741205;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(2) = -0.055053608;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(3) = 0.0023850432;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(4) = 0.173926877;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(5) = -0.0248473764;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(6) = 0.00048430224;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(7) = -0.005589849456;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(8) = 0.0005770079712;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(9) = -0.00001342427256;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(10) = 2.84765801111111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(11) = -0.121765149;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(12) = 0.0014599242;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(13) = 1.680428651;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(14) = -0.0166920786;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(15) = -0.0007190532;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(16) = -0.025485194448;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(17) = 0.0000487491696;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(18) = 0.00002719234152;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(19) = -0.0653766255555556;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(20) = -0.002278167;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(21) = 0.0002500254;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(22) = -0.0910565458;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(23) = 0.00318176316;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(24) = 0.000038621772;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(25) = -0.0034285382352;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(26) = 0.00000856589904;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(27) = -0.000001516821552;

                //       set minimum and maximum boundaries for YorkCalc model input variables
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinInletAirWBTemp = -34.4;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxInletAirWBTemp = 29.4444;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp = 1.1111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp = 22.2222;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp = 1.1111;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp = 40.0;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinWaterFlowRatio = 0.75;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxWaterFlowRatio = 1.25;
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxLiquidToGasRatio = 8.0;

            } else if (UtilityRoutines::SameString(AlphArray(4), "CoolToolsUserDefined")) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerModelType = ModelType::CoolToolsUserDefined;
                // Nested Get-input routines below.  Should pull out of here and read in beforehand.
                for (VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSCoolToolsModelCoeffs; ++VSModelCoeffNum) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(
                        state, "CoolingTowerPerformance:CoolTools", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat);
                    if (!UtilityRoutines::SameString(AlphArray2(1), state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName)) continue;
                    state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).FoundModelCoeff = true;
                    // verify the correct number of coefficients for the CoolTools model
                    if (NumNums2 != 43) {
                        ShowSevereError(state,
                                        "CoolingTower:VariableSpeed \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". The number of numeric inputs for object CoolingTowerPerformance:CoolTools \"" +
                                            state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName + "\" must equal 43.");
                        ErrorsFound = true;
                    } else {

                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinInletAirWBTemp =
                            NumArray2(1);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxInletAirWBTemp =
                            NumArray2(2);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp = NumArray2(3);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp = NumArray2(4);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp = NumArray2(5);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp = NumArray2(6);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinWaterFlowRatio =
                            NumArray2(7);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxWaterFlowRatio =
                            NumArray2(8);

                        for (CoeffNum = 9; CoeffNum <= NumNums2; ++CoeffNum) {
                            state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(CoeffNum - 8) =
                                NumArray2(CoeffNum);
                        }
                    }
                    break;
                }
                if (!state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).FoundModelCoeff) {
                    ShowSevereError(state,
                                    "CoolingTower:VariableSpeed \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". User defined name for variable speed cooling tower model coefficients object not found = " +
                                        state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName);
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(AlphArray(4), "YorkCalcUserDefined")) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerModelType = ModelType::YorkCalcUserDefined;
                // Nested Get-input routines below.  Should pull out of here and read in beforehand.
                for (VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSYorkCalcModelCoeffs; ++VSModelCoeffNum) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(
                        state, "CoolingTowerPerformance:YorkCalc", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat);
                    if (!UtilityRoutines::SameString(AlphArray2(1), state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName)) continue;
                    state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).FoundModelCoeff = true;
                    // verify the correct number of coefficients for the YorkCalc model
                    if (NumNums2 != 36) {
                        ShowSevereError(state,
                                        "CoolingTower:VariableSpeed \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                            "\". The number of numeric inputs for object CoolingTowerPerformance:YorkCalc \"" +
                                            state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName + "\" must equal 36.");
                        ErrorsFound = true;
                    } else {

                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinInletAirWBTemp =
                            NumArray2(1);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxInletAirWBTemp =
                            NumArray2(2);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp = NumArray2(3);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp = NumArray2(4);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp = NumArray2(5);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp = NumArray2(6);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinWaterFlowRatio =
                            NumArray2(7);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxWaterFlowRatio =
                            NumArray2(8);
                        state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxLiquidToGasRatio =
                            NumArray2(9);

                        for (CoeffNum = 10; CoeffNum <= NumNums2; ++CoeffNum) {
                            state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).Coeff(CoeffNum - 9) =
                                NumArray2(CoeffNum);
                        }
                    }
                    break;
                }

                if (!state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).FoundModelCoeff) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\". User defined name for variable speed cooling tower model coefficients object not found = " +
                                        state.dataCondenserLoopTowers->towers(TowerNum).ModelCoeffObjectName);
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". Illegal Tower Model Type = " + AlphArray(5));
                ShowContinueError(state,
                                  R"( Tower Model Type must be "CoolToolsCrossFlow", "YorkCalc", "CoolToolsUserDefined", or "YorkCalcUserDefined.)");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).TowerMassFlowRateMultiplier =
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxWaterFlowRatio;

            //   check user defined minimums to be greater than 0
            if (state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". User defined minimum approach temperature must be > 0");
                ErrorsFound = true;
            }
            if (state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". User defined minimum range temperature must be > 0");
                ErrorsFound = true;
            }
            if (state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinWaterFlowRatio < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". User defined minimum water flow rate ratio must be > 0");
                ErrorsFound = true;
            }

            //   check that the user defined maximums are greater than the minimums
            if (state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp <
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". User defined maximum approach temperature must be > the minimum approach temperature");
                ErrorsFound = true;
            }
            if (state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp <
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". User defined maximum range temperature must be > the minimum range temperature");
                ErrorsFound = true;
            }
            if (state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxWaterFlowRatio <
                state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinWaterFlowRatio) {
                ShowSevereError(state,
                                cCurrentModuleObject + " \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\". User defined maximum water flow rate ratio must be > the minimum water flow rate ratio");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).DesignInletWB = NumArray(1);
            if (NumArray(1) < state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinInletAirWBTemp ||
                NumArray(1) > state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxInletAirWBTemp) {
                ShowSevereError(
                    state,
                    cCurrentModuleObject.append(", \"")
                        .append(state.dataCondenserLoopTowers->towers(TowerNum).Name)
                        .append("\" the design inlet air wet-bulb temperature of ")
                        .append(format(OutputFormat, state.dataCondenserLoopTowers->towers(TowerNum).DesignInletWB))
                        .append(" must be within the model limits of ")
                        .append(
                            format(OutputFormat,
                                   state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinInletAirWBTemp))
                        .append(" and ")
                        .append(
                            format(OutputFormat,
                                   state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxInletAirWBTemp))
                        .append(" degrees C"));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).DesignApproach = NumArray(2);
            if (NumArray(2) < state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp ||
                NumArray(2) > state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp) {
                ShowSevereError(
                    state,
                    cCurrentModuleObject.append(", \"")
                        .append(state.dataCondenserLoopTowers->towers(TowerNum).Name)
                        .append("\" the design approach temperature of ")
                        .append(format(OutputFormat, state.dataCondenserLoopTowers->towers(TowerNum).DesignApproach))
                        .append(" must be within the model limits of ")
                        .append(
                            format(OutputFormat,
                                   state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinApproachTemp))
                        .append(" and ")
                        .append(
                            format(OutputFormat,
                                   state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxApproachTemp))
                        .append(" degrees C"));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).DesignRange = NumArray(3);
            if (NumArray(3) < state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp ||
                NumArray(3) > state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp) {
                ShowSevereError(
                    state,
                    cCurrentModuleObject.append(", \"")
                        .append(state.dataCondenserLoopTowers->towers(TowerNum).Name)
                        .append("\" the design range temperature of ")
                        .append(format(OutputFormat, state.dataCondenserLoopTowers->towers(TowerNum).DesignRange))
                        .append(" must be within the model limits of ")
                        .append(format(OutputFormat,
                                       state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MinRangeTemp))
                        .append(" and ")
                        .append(format(OutputFormat,
                                       state.dataCondenserLoopTowers->towers(state.dataCondenserLoopTowers->towers(TowerNum).VSTower).MaxRangeTemp))
                        .append(" degrees C"));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate = NumArray(4);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            if (NumArray(4) <= 0.0 && NumArray(4) != DataSizing::AutoSize) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" design water flow rate must be > 0");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate = NumArray(5);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            if (NumArray(5) <= 0.0 && NumArray(5) != DataSizing::AutoSize) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" design air flow rate must be > 0");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower = NumArray(6);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            if (NumArray(6) <= 0.0 && NumArray(6) != DataSizing::AutoSize) {
                ShowSevereError(
                    state, cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name + "\" design fan power must be > 0");
                ErrorsFound = true;
            }

            //   minimum air flow rate fraction must be >= 0.2 and <= 0.5, below this value the tower fan cycles to maintain the setpoint
            state.dataCondenserLoopTowers->towers(TowerNum).MinimumVSAirFlowFrac = NumArray(7);
            state.dataCondenserLoopTowers->towers(TowerNum).MinimumVSAirFlowFrac = NumArray(7);
            if (NumArray(7) < 0.2 || NumArray(7) > 0.5) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" minimum VS air flow rate ratio must be >= 0.2 and <= 0.5");
                ErrorsFound = true;
            }

            //   fraction of tower capacity in free convection regime must be >= to 0 and <= 0.2
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvectionCapacityFraction = NumArray(8);
            if (NumArray(8) < 0.0 || NumArray(8) > 0.2) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" fraction of tower capacity in free convection regime must be >= 0 and <= 0.2");
                ErrorsFound = true;
            }

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff = NumArray(9);
            if (NumArray(9) < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = NumArray(10);
            if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 10) {
                    state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ":\"" + state.dataCondenserLoopTowers->towers(TowerNum).Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(10) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            // Performance Input Method for Variable Speed Towers is assigned to be UA AND DESIGN WATER FLOW RATE
            // for autosizing calculations (see SizeTower)
            state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::UFactor;

            if (!AlphArray(7).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, AlphArray(7));
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                         "\" basin heater schedule name \"" + AlphArray(7) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(8), "LossFactor")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::UserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(8), "SaturatedExit")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphArray(8));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).UserEvapLossFactor = NumArray(11);        //  N11 , \field Evaporation Loss Factor
            state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = NumArray(12) / 100.0; //  N12, \field Drift Loss Percent
            state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = NumArray(13);        //  N13, \field Blowdown Concentration Ratio
            state.dataCondenserLoopTowers->towers(TowerNum).SizFac = NumArray(17);                    //  N14  \field Sizing Factor
            if (state.dataCondenserLoopTowers->towers(TowerNum).SizFac <= 0.0) state.dataCondenserLoopTowers->towers(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(9), "ScheduledRate")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Schedule;
            } else if (UtilityRoutines::SameString(AlphArray(9), "ConcentrationRatio")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + AlphArray(9));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(state, AlphArray(10));
            if ((state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown == 0) &&
                (state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode == Blowdown::Schedule)) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + AlphArray(10));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            // added for multi-cell
            state.dataCondenserLoopTowers->towers(TowerNum).NumCell = NumArray(14);
            if ((NumNums < 14) && (state.dataCondenserLoopTowers->towers(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).NumCell = 1;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = NumArray(15);
            if ((NumNums < 15) && (state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = 0.33;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = NumArray(16);
            if ((NumNums < 16) && (state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = 2.5;
            }

            if (NumAlphas >= 13) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(13) || AlphArray(13).empty()) {
                    state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(13), "MinimalCell") || UtilityRoutines::SameString(AlphArray(13), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(13), "MinimalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(13), "MaximalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                        }
                    } else {
                        ShowSevereError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(13) + " = " + AlphArray(13));
                        ShowContinueError(state,
                                          "Occurs in " + state.dataCondenserLoopTowers->towers(TowerNum).TowerType + '=' +
                                              state.dataCondenserLoopTowers->towers(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(state,
                                                       AlphArray(1),
                                                       cCurrentModuleObject,
                                                       AlphArray(11),
                                                       ErrorsFound,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankID,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankDemandARRID);
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node
            if (state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(12),
                                                        ErrorsFound,
                                                        cCurrentModuleObject,
                                                        state.dataCondenserLoopTowers->towers(TowerNum).Name,
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(12));
                    ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }
            if (NumAlphas > 13) {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = AlphArray(14);
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = "General";
            }

        } // End Variable-Speed Tower Loop

        cCurrentModuleObject = cCoolingTower_VariableSpeedMerkel;
        for (MerkelVSTowerNum = 1; MerkelVSTowerNum <= NumVSMerkelTowers; ++MerkelVSTowerNum) {
            TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + MerkelVSTowerNum;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     MerkelVSTowerNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCondenserLoopTowers->UniqueSimpleTowerNames,
                                                     AlphArray(1),
                                                     cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            state.dataCondenserLoopTowers->towers(TowerNum).Name = AlphArray(1);
            state.dataCondenserLoopTowers->towers(TowerNum).thisTowerNum = TowerNum;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType = cCurrentModuleObject;
            state.dataCondenserLoopTowers->towers(TowerNum).TowerType_Num = DataPlant::TypeOf_CoolingTower_VarSpdMerkel;
            state.dataCondenserLoopTowers->towers(TowerNum).WaterInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(2),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            state.dataCondenserLoopTowers->towers(TowerNum).WaterOutletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(3),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    AlphArray(1),
                                                    DataLoopNode::NodeFluidType::Water,
                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
                state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::UFactor;
            } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
                state.dataCondenserLoopTowers->towers(TowerNum).PerformanceInputMethod_Num = PIM::NominalCapacity;
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + AlphArray(4));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).FanPowerfAirFlowCurve = CurveManager::GetCurveIndex(state, AlphArray(5));
            if (state.dataCondenserLoopTowers->towers(TowerNum).FanPowerfAirFlowCurve == 0) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + '=' + AlphArray(5));
                ShowContinueError(state, "Curve name not found.");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).HeatRejectCapNomCapSizingRatio = NumArray(1);
            state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity = NumArray(2);
            if (state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacity == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerNominalCapacityWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap = NumArray(3);
            if (state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCap == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCapWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerFreeConvNomCapSizingFactor = NumArray(4);
            state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate = NumArray(5);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesignWaterFlowPerUnitNomCap = NumArray(6);
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate = NumArray(7);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DefaultedDesignAirFlowScalingFactor = state.dataIPShortCut->lNumericFieldBlanks(8);
            state.dataCondenserLoopTowers->towers(TowerNum).DesignAirFlowPerUnitNomCap = NumArray(8);
            state.dataCondenserLoopTowers->towers(TowerNum).MinimumVSAirFlowFrac = NumArray(9);
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower = NumArray(10);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesignFanPowerPerUnitNomCap = NumArray(11);
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate = NumArray(12);
            if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRate == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRateWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvAirFlowRateSizingFactor = NumArray(13);
            state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA = NumArray(14);
            if (state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA = NumArray(15);
            if (state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUA == DataSizing::AutoSize) {
                state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUAWasAutoSized = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).FreeConvTowerUASizingFactor = NumArray(16);

            state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncAirFlowRatioCurvePtr = CurveManager::GetCurveIndex(state, AlphArray(6));
            if (state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncAirFlowRatioCurvePtr == 0) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + AlphArray(6));
                ShowContinueError(state, "Curve name not found.");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncWetBulbDiffCurvePtr = CurveManager::GetCurveIndex(state, AlphArray(7));
            if (state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncWetBulbDiffCurvePtr == 0) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphArray(7));
                ShowContinueError(state, "Curve name not found.");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncWaterFlowRatioCurvePtr = CurveManager::GetCurveIndex(state, AlphArray(8));
            if (state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncWaterFlowRatioCurvePtr == 0) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphArray(8));
                ShowContinueError(state, "Curve name not found.");
                ErrorsFound = true;
            }
            // cooling tower design inlet conditions
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp = NumArray(17);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirDBTemp = 35.0;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp = NumArray(18);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp = 25.6;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesApproach = NumArray(19);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesApproach == DataSizing::AutoSize ||
                state.dataCondenserLoopTowers->towers(TowerNum).DesApproach == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesApproach = 3.9;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).DesRange = NumArray(20);
            if (state.dataCondenserLoopTowers->towers(TowerNum).DesRange == DataSizing::AutoSize ||
                state.dataCondenserLoopTowers->towers(TowerNum).DesRange == 0) {
                state.dataCondenserLoopTowers->towers(TowerNum).DesRange = 5.5;
                state.dataCondenserLoopTowers->towers(TowerNum).TowerInletCondsAutoSize = true;
            }
            // set tower design water outlet and inlet temperatures
            state.dataCondenserLoopTowers->towers(TowerNum).DesOutletWaterTemp =
                state.dataCondenserLoopTowers->towers(TowerNum).DesInletAirWBTemp + state.dataCondenserLoopTowers->towers(TowerNum).DesApproach;
            state.dataCondenserLoopTowers->towers(TowerNum).DesInletWaterTemp =
                state.dataCondenserLoopTowers->towers(TowerNum).DesOutletWaterTemp + state.dataCondenserLoopTowers->towers(TowerNum).DesRange;
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff = NumArray(21);
            if (NumArray(21) < 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                    "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = NumArray(22);
            if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 22) {
                    state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ":\"" + state.dataCondenserLoopTowers->towers(TowerNum).Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(22) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!AlphArray(9).empty()) {
                state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, AlphArray(9));
                if (state.dataCondenserLoopTowers->towers(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                         "\" basin heater schedule name \"" + AlphArray(9) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(10), "LossFactor")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::UserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(10), "SaturatedExit")) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                state.dataCondenserLoopTowers->towers(TowerNum).EvapLossMode = EvapLoss::MoistTheory;
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + AlphArray(10));
                ErrorsFound = true;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).UserEvapLossFactor = NumArray(23);        //  N23 , \field Evaporation Loss Factor
            state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = NumArray(24) / 100.0; //  N24, \field Drift Loss Percent
            if ((NumNums < 24) && (state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction == 0.0)) {
                // assume Drift loss not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).DriftLossFraction = 0.008 / 100.0;
            }

            state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = NumArray(25); //  N25, \field Blowdown Concentration Ratio
            state.dataCondenserLoopTowers->towers(TowerNum).SizFac = NumArray(29);             //  N29  \field Sizing Factor
            if (state.dataCondenserLoopTowers->towers(TowerNum).SizFac <= 0.0) state.dataCondenserLoopTowers->towers(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(11), "ScheduledRate")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Schedule;
            } else if (UtilityRoutines::SameString(AlphArray(11), "ConcentrationRatio")) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
            } else if (state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode = Blowdown::Concentration;
                if ((NumNums < 25) && (state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio == 0.0)) {
                    // assume concentration ratio was omitted and should be defaulted
                    state.dataCondenserLoopTowers->towers(TowerNum).ConcentrationRatio = 3.0;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + AlphArray(11));
                ErrorsFound = true;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(state, AlphArray(12));
            if ((state.dataCondenserLoopTowers->towers(TowerNum).SchedIDBlowdown == 0) &&
                (state.dataCondenserLoopTowers->towers(TowerNum).BlowdownMode == Blowdown::Schedule)) {
                ShowSevereError(state, cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(12) + '=' + AlphArray(12));
                ErrorsFound = true;
            }

            // added for multi-cell
            state.dataCondenserLoopTowers->towers(TowerNum).NumCell = NumArray(26);
            if ((NumNums < 26) && (state.dataCondenserLoopTowers->towers(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).NumCell = 1;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = NumArray(27);
            if ((NumNums < 27) && (state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MinFracFlowRate = 0.33;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = NumArray(28);
            if ((NumNums < 28) && (state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate = 2.5;
            }
            state.dataCondenserLoopTowers->towers(TowerNum).TowerMassFlowRateMultiplier =
                state.dataCondenserLoopTowers->towers(TowerNum).MaxFracFlowRate;
            if (NumAlphas >= 15) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(15) || AlphArray(15).empty()) {
                    state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(15), "MinimalCell") || UtilityRoutines::SameString(AlphArray(15), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(15), "MinimalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(15), "MaximalCell")) {
                            state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
                        }
                    } else {
                        ShowSevereError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(15) + " = " + AlphArray(15));
                        ShowContinueError(state,
                                          "Occurs in " + state.dataCondenserLoopTowers->towers(TowerNum).TowerType + '=' +
                                              state.dataCondenserLoopTowers->towers(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                state.dataCondenserLoopTowers->towers(TowerNum).CellCtrl_Num = CellCtrl::MaxCell;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(state,
                                                       AlphArray(1),
                                                       cCurrentModuleObject,
                                                       AlphArray(13),
                                                       ErrorsFound,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankID,
                                                       state.dataCondenserLoopTowers->towers(TowerNum).WaterTankDemandARRID);
                state.dataCondenserLoopTowers->towers(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node
            if (state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        AlphArray(14),
                                                        ErrorsFound,
                                                        cCurrentModuleObject,
                                                        state.dataCondenserLoopTowers->towers(TowerNum).Name,
                                                        DataLoopNode::NodeFluidType::Air,
                                                        DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                        1,
                                                        DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataCondenserLoopTowers->towers(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + ", \"" + state.dataCondenserLoopTowers->towers(TowerNum).Name +
                                        "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(14));
                    ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }
            if (NumAlphas > 15) {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = AlphArray(16);
            } else {
                state.dataCondenserLoopTowers->towers(TowerNum).EndUseSubcategory = "General";
            }

        } // end merkel vs tower loop

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in getting cooling tower input.");
        }
    }

    void CoolingTower::oneTimeInit(EnergyPlusData &state)
    {
        // Locate the tower on the plant loops for later usage
        bool ErrorsFound = false;
        PlantUtilities::ScanPlantLoopsForObject(
            state, this->Name, this->TowerType_Num, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, ErrorsFound, _, _, _, _, _);
        if (ErrorsFound) {
            ShowFatalError(state, "initialize: Program terminated due to previous condition(s).");
        }

        // check if setpoint on outlet node
        this->SetpointIsOnOutlet = !((state.dataLoopNodes->Node(this->WaterOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                                     (state.dataLoopNodes->Node(this->WaterOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue));
    }

    void CoolingTower::initEachEnvironment(EnergyPlusData &state)
    {
        static constexpr std::string_view RoutineName("CoolingTower::initEachEnvironment");
        Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                             DataGlobalConstants::InitConvTemp,
                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                             RoutineName);
        this->DesWaterMassFlowRate = this->DesignWaterFlowRate * rho;
        this->DesWaterMassFlowRatePerCell = this->DesWaterMassFlowRate / this->NumCell;
        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->DesWaterMassFlowRate,
                                           this->WaterInletNodeNum,
                                           this->WaterOutletNodeNum,
                                           this->LoopNum,
                                           this->LoopSideNum,
                                           this->BranchNum,
                                           this->CompNum);
    }

    void CoolingTower::initialize(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2002
        //       MODIFIED       Don Shirey Sept/Oct 2002, F Buhl Oct 2002
        //       RE-ENGINEERED  R. Raustad, Oct 2005, moved Max/MinAvail to Init and allowed more than design
        //                      water flow rate to pass through towers (up to 2.5 and 1.25 times the design flow
        //                      for 1 or 2-speed and variable speed towers, respectively). Flow multiplier for
        //                      VS Tower is defaulted to 1.25 and can be reassigned by user.

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Cooling Tower components and for
        // final checking of tower inputs (post autosizing)

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        if (this->oneTimeFlag) {
            this->setupOutputVariables(state);
            this->oneTimeInit(state);
            this->oneTimeFlag = false;
        }

        // Begin environment initializations
        if (this->envrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {
            this->initEachEnvironment(state);
            this->envrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->envrnFlag = true;
        }

        // Each time initializations
        this->WaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;

        if (this->OutdoorAirInletNodeNum != 0) {
            this->AirTemp = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).Temp;
            this->AirHumRat = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).HumRat;
            this->AirPress = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).Press;
            this->AirWetBulb = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).OutAirWetBulb;
        } else {
            this->AirTemp = state.dataEnvrn->OutDryBulbTemp;
            this->AirHumRat = state.dataEnvrn->OutHumRat;
            this->AirPress = state.dataEnvrn->OutBaroPress;
            this->AirWetBulb = state.dataEnvrn->OutWetBulbTemp;
        }

        this->WaterMassFlowRate = PlantUtilities::RegulateCondenserCompFlowReqOp(
            state, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, this->DesWaterMassFlowRate * this->TowerMassFlowRateMultiplier);

        PlantUtilities::SetComponentFlowRate(state,
                                             this->WaterMassFlowRate,
                                             this->WaterInletNodeNum,
                                             this->WaterOutletNodeNum,
                                             this->LoopNum,
                                             this->LoopSideNum,
                                             this->BranchNum,
                                             this->CompNum);

        // Added for fluid bypass. 8/2008
        this->BypassFraction = 0.0;
        this->BasinHeaterPower = 0.0;
        this->airFlowRateRatio = 0.0;
    }

    void CoolingTower::setupOutputVariables(EnergyPlusData &state)
    {
        // Set up output variables CurrentModuleObject='CoolingTower:SingleSpeed'
        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd) {
            SetupOutputVariable(
                state, "Cooling Tower Inlet Temperature", OutputProcessor::Unit::C, this->InletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Outlet Temperature", OutputProcessor::Unit::C, this->OutletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
            SetupOutputVariable(state, "Cooling Tower Heat Transfer Rate", OutputProcessor::Unit::W, this->Qactual, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Electricity Rate", OutputProcessor::Unit::W, this->FanPower, "System", "Average", this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->FanEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Electricity",
                                "HeatRejection",
                                this->EndUseSubcategory,
                                "Plant");
            // Added for fluid bypass
            SetupOutputVariable(
                state, "Cooling Tower Bypass Fraction", OutputProcessor::Unit::None, this->BypassFraction, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Operating Cells Count", OutputProcessor::Unit::None, this->NumCellOn, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Cycling Ratio", OutputProcessor::Unit::None, this->FanCyclingRatio, "System", "Average", this->Name);
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    "System",
                                    "Average",
                                    this->Name);
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }

        // CurrentModuleObject='CoolingTower:TwoSpeed'
        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
            SetupOutputVariable(
                state, "Cooling Tower Inlet Temperature", OutputProcessor::Unit::C, this->InletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Outlet Temperature", OutputProcessor::Unit::C, this->OutletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
            SetupOutputVariable(state, "Cooling Tower Heat Transfer Rate", OutputProcessor::Unit::W, this->Qactual, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Electricity Rate", OutputProcessor::Unit::W, this->FanPower, "System", "Average", this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->FanEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Electricity",
                                "HeatRejection",
                                this->EndUseSubcategory,
                                "Plant");
            SetupOutputVariable(
                state, "Cooling Tower Fan Cycling Ratio", OutputProcessor::Unit::None, this->FanCyclingRatio, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Speed Level", OutputProcessor::Unit::None, this->SpeedSelected, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Operating Cells Count", OutputProcessor::Unit::None, this->NumCellOn, "System", "Average", this->Name);
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    "System",
                                    "Average",
                                    this->Name);
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }

        // CurrentModuleObject='CoolingTower:VariableSpeed'
        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpd) {
            SetupOutputVariable(
                state, "Cooling Tower Inlet Temperature", OutputProcessor::Unit::C, this->InletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Outlet Temperature", OutputProcessor::Unit::C, this->OutletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
            SetupOutputVariable(state, "Cooling Tower Heat Transfer Rate", OutputProcessor::Unit::W, this->Qactual, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Electricity Rate", OutputProcessor::Unit::W, this->FanPower, "System", "Average", this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->FanEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Electricity",
                                "HeatRejection",
                                this->EndUseSubcategory,
                                "Plant");
            SetupOutputVariable(
                state, "Cooling Tower Air Flow Rate Ratio", OutputProcessor::Unit::None, this->AirFlowRatio, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Part Load Ratio", OutputProcessor::Unit::None, this->FanCyclingRatio, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Operating Cells Count", OutputProcessor::Unit::None, this->NumCellOn, "System", "Average", this->Name);
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    "System",
                                    "Average",
                                    this->Name);
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }

        // CurrentModuleObject='CoolingTower:VariableSpeed:Merkel'
        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpdMerkel) {
            SetupOutputVariable(
                state, "Cooling Tower Inlet Temperature", OutputProcessor::Unit::C, this->InletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Outlet Temperature", OutputProcessor::Unit::C, this->OutletWaterTemp, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
            SetupOutputVariable(state, "Cooling Tower Heat Transfer Rate", OutputProcessor::Unit::W, this->Qactual, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Cooling Tower Fan Electricity Rate", OutputProcessor::Unit::W, this->FanPower, "System", "Average", this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                this->FanEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Electricity",
                                "HeatRejection",
                                this->EndUseSubcategory,
                                "Plant");
            SetupOutputVariable(
                state, "Cooling Tower Fan Speed Ratio", OutputProcessor::Unit::None, this->AirFlowRatio, "System", "Average", this->Name);

            SetupOutputVariable(
                state, "Cooling Tower Operating Cells Count", OutputProcessor::Unit::None, this->NumCellOn, "System", "Average", this->Name);
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    "System",
                                    "Average",
                                    this->Name);
                SetupOutputVariable(state,
                                    "Cooling Tower Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }
        // setup common water reporting for all types of towers.
        if (this->SuppliedByWaterSystem) {
            SetupOutputVariable(state,
                                "Cooling Tower Make Up Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                this->MakeUpVdot,
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(state, "Cooling Tower Make Up Water Volume", OutputProcessor::Unit::m3, this->MakeUpVol, "System", "Sum", this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Storage Tank Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                this->TankSupplyVdot,
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Storage Tank Water Volume",
                                OutputProcessor::Unit::m3,
                                this->TankSupplyVol,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Water",
                                "HeatRejection",
                                _,
                                "Plant");
            SetupOutputVariable(state,
                                "Cooling Tower Starved Storage Tank Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                this->StarvedMakeUpVdot,
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Starved Storage Tank Water Volume",
                                OutputProcessor::Unit::m3,
                                this->StarvedMakeUpVol,
                                "System",
                                "Sum",
                                this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Make Up Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                this->StarvedMakeUpVol,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "MainsWater",
                                "HeatRejection",
                                _,
                                "Plant");
        } else { // tower water from mains and gets metered
            SetupOutputVariable(state,
                                "Cooling Tower Make Up Water Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                this->MakeUpVdot,
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(state,
                                "Cooling Tower Make Up Water Volume",
                                OutputProcessor::Unit::m3,
                                this->MakeUpVol,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "Water",
                                "HeatRejection",
                                _,
                                "Plant");
            SetupOutputVariable(state,
                                "Cooling Tower Make Up Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                this->MakeUpVol,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "MainsWater",
                                "HeatRejection",
                                _,
                                "Plant");
        }

        SetupOutputVariable(state,
                            "Cooling Tower Water Evaporation Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->EvaporationVdot,
                            "System",
                            "Average",
                            this->Name);
        SetupOutputVariable(
            state, "Cooling Tower Water Evaporation Volume", OutputProcessor::Unit::m3, this->EvaporationVol, "System", "Sum", this->Name);
        SetupOutputVariable(
            state, "Cooling Tower Water Drift Volume Flow Rate", OutputProcessor::Unit::m3_s, this->DriftVdot, "System", "Average", this->Name);
        SetupOutputVariable(state, "Cooling Tower Water Drift Volume", OutputProcessor::Unit::m3, this->DriftVol, "System", "Sum", this->Name);
        SetupOutputVariable(
            state, "Cooling Tower Water Blowdown Volume Flow Rate", OutputProcessor::Unit::m3_s, this->BlowdownVdot, "System", "Average", this->Name);
        SetupOutputVariable(state, "Cooling Tower Water Blowdown Volume", OutputProcessor::Unit::m3, this->BlowdownVol, "System", "Sum", this->Name);
    }

    void CoolingTower::SizeTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2002
        //       MODIFIED       Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Cooling Tower Components for which capacities and flow rates
        // have not been specified in the input. This subroutine also calculates tower UA if the user
        // has specified tower performance via the "Nominal Capacity" method.

        // METHODOLOGY EMPLOYED:
        // Obtains condenser flow rate from the plant sizing array. If tower performance is specified
        // via the "Nominal Capacity" method, the water flow rate is directly proportional to capacity.

        // SUBROUTINE PARAMETER DEFINITIONS:

        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static constexpr std::string_view RoutineName("SizeTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SolFla;                       // Flag of solver
        Real64 DesTowerLoad(0.0);         // Design tower load [W]
        Real64 UA0;                       // Lower bound for UA [W/C]
        Real64 UA1;                       // Upper bound for UA [W/C]
        Real64 UA;                        // Calculated UA value
        std::array<Real64, 6> Par;        // Parameter array need for RegulaFalsi routine
        std::string OutputChar;           // report variable for warning messages
        std::string OutputChar2;          // report variable for warning messages
        std::string OutputCharLo;         // report variable for warning messages
        std::string OutputCharHi;         // report variable for warning messages
        Real64 DesTowerInletWaterTemp;    // design tower inlet water temperature
        Real64 DesTowerExitWaterTemp;     // design tower exit water temperature
        Real64 DesTowerWaterDeltaT;       // design tower temperature range
        Real64 DesTowerApproachFromPlant; // design tower approach temperature from plant sizing object
        Real64 TolTemp(0.04);             // DeltaT and DesApproach diffs tollerance between plant sizing data and user input in cooling tower
        // for warning message reporting purpose only

        Real64 tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
        Real64 tmpHighSpeedFanPower = this->HighSpeedFanPower;
        Real64 tmpHighSpeedAirFlowRate = this->HighSpeedAirFlowRate;
        Real64 tmpLowSpeedAirFlowRate = this->LowSpeedAirFlowRate;

        auto &PlantSizData(state.dataSize->PlantSizData);

        // Find the appropriate Plant Sizing object
        int PltSizCondNum = state.dataPlnt->PlantLoop(this->LoopNum).PlantSizNum;

        if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd || this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
            if (this->TowerInletCondsAutoSize) {
                if (PltSizCondNum > 0) {
                    // use plant sizing data
                    DesTowerExitWaterTemp = PlantSizData(PltSizCondNum).ExitTemp;
                    DesTowerInletWaterTemp = DesTowerExitWaterTemp + PlantSizData(PltSizCondNum).DeltaT;
                    DesTowerWaterDeltaT = PlantSizData(PltSizCondNum).DeltaT;
                } else {
                    // set hard wired input assumptions
                    // AssumedDeltaT = 11.0;
                    // AssumedExitTemp = 21.0;
                    DesTowerWaterDeltaT = 11.0;
                    DesTowerExitWaterTemp = 21.0;
                    DesTowerInletWaterTemp = DesTowerExitWaterTemp + DesTowerWaterDeltaT;
                }
            } else {
                // use tower sizing data
                DesTowerExitWaterTemp = this->DesOutletWaterTemp;
                DesTowerInletWaterTemp = this->DesInletWaterTemp;
                DesTowerWaterDeltaT = this->DesRange;
                if (PltSizCondNum > 0) {
                    // check the tower range against the plant sizing data
                    if (std::abs(DesTowerWaterDeltaT - PlantSizData(PltSizCondNum).DeltaT) > TolTemp) {
                        ShowWarningError(state,
                                         "Error when autosizing the load for cooling tower = " + this->Name +
                                             ". Tower Design Range Temperature is different from the Design Loop Delta Temperature.");
                        ShowContinueError(state, "Tower Design Range Temperature specified in tower = " + this->Name);
                        ShowContinueError(state,
                                          "is inconsistent with Design Loop Delta Temperature specified in Sizing:Plant object = " +
                                              PlantSizData(PltSizCondNum).PlantLoopName + ".");
                        ShowContinueError(state, format("..The Design Range Temperature specified in tower is = {:.2T}", this->DesRange));
                        ShowContinueError(state,
                                          format("..The Design Loop Delta Temperature specified in plant sizing data is = {:.2T}",
                                                 PlantSizData(PltSizCondNum).DeltaT));
                    }
                    // check if the tower approach is different from plant sizing data
                    DesTowerApproachFromPlant = PlantSizData(PltSizCondNum).ExitTemp - this->DesInletAirWBTemp;
                    if (std::abs(DesTowerApproachFromPlant - this->DesApproach) > TolTemp) {
                        ShowWarningError(state,
                                         "Error when autosizing the UA for cooling tower = " + this->Name +
                                             ". Tower Design Approach Temperature is inconsistent with Approach from Plant Sizing Data.");
                        ShowContinueError(state,
                                          "The Design Approach Temperature from inputs specified in Sizing:Plant object = " +
                                              PlantSizData(PltSizCondNum).PlantLoopName);
                        ShowContinueError(state, "is inconsistent with Design Approach Temperature specified in tower = " + this->Name + ".");
                        ShowContinueError(state,
                                          format("..The Design Approach Temperature from inputs specified is = {:.2T}", DesTowerApproachFromPlant));
                        ShowContinueError(state, format("..The Design Approach Temperature specified in tower is = {:.2T}", this->DesApproach));
                    }
                }
            }
        } else { // CoolingTower_VariableSpeed
            if (PltSizCondNum > 0) {
                // use plant sizing data
                DesTowerExitWaterTemp = PlantSizData(PltSizCondNum).ExitTemp;
                DesTowerInletWaterTemp = DesTowerExitWaterTemp + PlantSizData(PltSizCondNum).DeltaT;
                DesTowerWaterDeltaT = PlantSizData(PltSizCondNum).DeltaT;
            } else {
                // set hard wired input assumptions
                DesTowerWaterDeltaT = 11.0;
                DesTowerExitWaterTemp = 21.0;
                DesTowerInletWaterTemp = DesTowerExitWaterTemp + DesTowerWaterDeltaT;
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::UFactor && (!this->HighSpeedTowerUAWasAutoSized)) {
            if (PltSizCondNum > 0) {
                Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     DesTowerExitWaterTemp,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName);
                Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         DesTowerExitWaterTemp,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName);
                DesTowerLoad = rho * Cp * this->DesignWaterFlowRate * DesTowerWaterDeltaT;
                this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;

            } else {
                Real64 AssumedDeltaT = DesTowerWaterDeltaT;
                Real64 AssumedExitTemp = DesTowerExitWaterTemp;

                Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     AssumedExitTemp,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName);
                Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         AssumedExitTemp,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName);

                DesTowerLoad = rho * Cp * this->DesignWaterFlowRate * AssumedDeltaT;
                this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
            }
        }

        if (this->DesignWaterFlowRateWasAutoSized) {
            if (PltSizCondNum > 0) {
                if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    tmpDesignWaterFlowRate = PlantSizData(PltSizCondNum).DesVolFlowRate * this->SizFac;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                } else {
                    tmpDesignWaterFlowRate = 0.0;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                }
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                }
            } else {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    ShowSevereError(state, "Autosizing error for cooling tower object = " + this->Name);
                    ShowFatalError(state, "Autosizing of cooling tower condenser flow rate requires a loop Sizing:Plant object.");
                }
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::NominalCapacity) {
            // Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
            this->DesignWaterFlowRate = 5.382e-8 * this->TowerNominalCapacity;
            tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
            if (UtilityRoutines::SameString(this->TowerType, "CoolingTower:SingleSpeed")) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Design Water Flow Rate based on tower nominal capacity [m3/s]",
                                                 this->DesignWaterFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Initial Design Water Flow Rate based on tower nominal capacity [m3/s]",
                                                 this->DesignWaterFlowRate);
                }
            } else if (UtilityRoutines::SameString(this->TowerType, "CoolingTower:TwoSpeed")) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]",
                                                 this->DesignWaterFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Initial Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]",
                                                 this->DesignWaterFlowRate);
                }
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(state, this->WaterInletNodeNum, tmpDesignWaterFlowRate);

        if (this->HighSpeedFanPowerWasAutoSized) {
            // We assume the nominal fan power is 0.0105 times the design load
            if (this->PerformanceInputMethod_Num == PIM::NominalCapacity) {
                this->HighSpeedFanPower = 0.0105 * this->TowerNominalCapacity;
                tmpHighSpeedFanPower = this->HighSpeedFanPower;
            } else {
                if (PltSizCondNum > 0) {
                    if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                             DataGlobalConstants::InitConvTemp,
                                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                             RoutineName);
                        Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                                 DesTowerExitWaterTemp,
                                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                                 RoutineName);
                        DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * DesTowerWaterDeltaT;
                        tmpHighSpeedFanPower = 0.0105 * DesTowerLoad;
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                    } else {
                        tmpHighSpeedFanPower = 0.0;
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                    }
                } else {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        ShowSevereError(state, "Autosizing of cooling tower fan power requires a loop Sizing:Plant object.");
                        ShowFatalError(state, " Occurs in cooling tower object= " + this->Name);
                    }
                }
            }
            if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd || this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpd) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower);
                }
            } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Fan Power at High Fan Speed [W]", this->HighSpeedFanPower);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Fan Power at High Fan Speed [W]", this->HighSpeedFanPower);
                }
            }
        }

        if (this->HighSpeedAirFlowRateWasAutoSized) {
            // Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.
            tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5 * (101325.0 / state.dataEnvrn->StdBaroPress) / 190.0;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;

            if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd || this->TowerType_Num == DataPlant::TypeOf_CoolingTower_VarSpd) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                }
            } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate);
                }
            }
        }

        if (this->HighSpeedTowerUAWasAutoSized) {
            if (PltSizCondNum > 0) {
                if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         DataGlobalConstants::InitConvTemp,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName);
                    Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                             DesTowerExitWaterTemp,
                                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                             RoutineName);
                    DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * DesTowerWaterDeltaT;
                    // This conditional statement is to trap when the user specified condenser/tower water design setpoint
                    //  temperature is less than design inlet air wet bulb temperature
                    if (PlantSizData(PltSizCondNum).ExitTemp <= this->DesInletAirWBTemp) {
                        ShowSevereError(state,
                                        format("Error when autosizing the UA value for cooling tower = {}. Design Loop Exit Temperature must be "
                                               "greater than {:.2T} C when autosizing the tower UA.",
                                               this->Name,
                                               this->DesInletAirWBTemp));
                        ShowContinueError(state,
                                          format("The Design Loop Exit Temperature specified in Sizing:Plant object = {} ({:.2T} C)",
                                                 PlantSizData(PltSizCondNum).PlantLoopName,
                                                 PlantSizData(PltSizCondNum).ExitTemp));
                        ShowContinueError(
                            state,
                            format("is less than or equal to the design inlet air wet-bulb temperature of {:.2T} C.", this->DesInletAirWBTemp));
                        ShowContinueError(state,
                                          format("If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design "
                                                 "Setpoint must be > {:.2T} C if autosizing the cooling tower.",
                                                 this->DesInletAirWBTemp));
                        ShowFatalError(state, "Autosizing of cooling tower fails for tower = " + this->Name + '.');
                    }

                    Par[0] = DesTowerLoad;
                    Par[1] = double(this->thisTowerNum);
                    Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par[3] = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                    Par[4] = Cp;
                    UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
                    UA1 = DesTowerLoad;          // Assume deltaT = 1K
                    this->WaterTemp = DesTowerInletWaterTemp;
                    this->AirTemp = this->DesInletAirDBTemp;    // 35.0;
                    this->AirWetBulb = this->DesInletAirWBTemp; // 25.6;
                    this->AirPress = state.dataEnvrn->StdBaroPress;
                    this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                    // auto f1 = [this](EnergyPlusData &state, Real64 x, std::array<Real64, 6> p) -> Real64 { return this->residualUA(state, x, p); };
                    auto f1 = [this](auto &state, auto x, const auto &p) { return this->residualUA(state, x, p); };
                    General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f1, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                        ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                    } else if (SolFla == -2) {
                        ShowSevereError(state, "Bad starting values for UA");
                        ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                    }

                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->HighSpeedTowerUA = UA;
                    }
                    this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                } else {
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->HighSpeedTowerUA = 0.0;
                    }
                }
                if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                                     this->HighSpeedTowerUA);
                    }
                } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA);
                    }
                }
            } else {
                if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                    Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         DataGlobalConstants::InitConvTemp,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName);
                    Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                             DesTowerExitWaterTemp,
                                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                             RoutineName);
                    DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * DesTowerWaterDeltaT;
                    // This conditional statement is to trap when the user specified condenser/tower water design setpoint
                    //  temperature is less than design inlet air wet bulb temperature
                    // Note JM 2018-11-22
                    // * If actually user-specified:
                    //  this->DesOutletWaterTemp = this->DesInletAirWBTemp
                    //                                           + this->DesApproach;
                    //  DesTowerExitWaterTemp = this->DesOutletWaterTemp;
                    //  => This basically means that approach is negative, which is impossible (must be > 0 per IDD)
                    // * If not, hardcoded above to 21C
                    if (DesTowerExitWaterTemp <= this->DesInletAirWBTemp) {
                        ShowSevereError(state,
                                        format("Error when autosizing the UA value for cooling tower = {}. Design Tower Exit Temperature must be "
                                               "greater than {:.2T} C when autosizing the tower UA.",
                                               this->Name,
                                               this->DesInletAirWBTemp));
                        ShowContinueError(state, format("The User-specified Design Loop Exit Temperature={:.2T}", DesTowerExitWaterTemp));
                        ShowContinueError(
                            state,
                            format("is less than or equal to the design inlet air wet-bulb temperature of {:.2T} C.", this->DesInletAirWBTemp));

                        if (this->TowerInletCondsAutoSize) {
                            ShowContinueError(state,
                                              format("Because you did not specify the Design Approach Temperature, and you do not have a "
                                                     "Sizing:Plant object, it was defaulted to {:.2T} C.",
                                                     DesTowerExitWaterTemp));
                        } else {
                            // Should never get there...
                            ShowContinueError(state,
                                              format("The Design Loop Exit Temperature is the sum of the design air inlet wet-bulb temperature= "
                                                     "{:.2T} C plus the cooling tower design approach temperature = {:.2T}C.",
                                                     this->DesInletAirWBTemp,
                                                     this->DesApproach));
                        }
                        ShowContinueError(state,
                                          format("If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design "
                                                 "Setpoint must be > {:.2T} C if autosizing the cooling tower.",
                                                 this->DesInletAirWBTemp));
                        ShowFatalError(state, "Autosizing of cooling tower fails for tower = " + this->Name + '.');
                    }

                    Par[0] = DesTowerLoad;
                    Par[1] = double(this->thisTowerNum);
                    Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par[3] = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                    Par[4] = Cp;
                    UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
                    UA1 = DesTowerLoad;          // Assume deltaT = 1K
                    this->WaterTemp = DesTowerInletWaterTemp;
                    this->AirTemp = this->DesInletAirDBTemp;    // 35.0;
                    this->AirWetBulb = this->DesInletAirWBTemp; // 25.6;
                    this->AirPress = state.dataEnvrn->StdBaroPress;
                    this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                    auto f = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                    General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                        ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                    } else if (SolFla == -2) {
                        ShowSevereError(state, "Bad starting values for UA");
                        ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                    }

                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->HighSpeedTowerUA = UA;
                    }
                    this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                } else {
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->HighSpeedTowerUA = 0.0;
                    }
                }
                if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                                     this->HighSpeedTowerUA);
                    }
                } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA);
                    }
                }
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::NominalCapacity) {
            if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                // nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of delivered cooling but now is
                // a user input
                Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     29.44,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName); // 85F design exiting water temp
                Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         29.44,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName); // 85F design exiting water temp

                DesTowerLoad = this->TowerNominalCapacity * this->HeatRejectCapNomCapSizingRatio;
                Par[0] = DesTowerLoad;
                Par[1] = double(this->thisTowerNum);
                Par[2] = rho * tmpDesignWaterFlowRate;      // design water mass flow rate
                Par[3] = tmpHighSpeedAirFlowRate;           // design air volume flow rate
                Par[4] = Cp;                                // 85F design exiting water temp
                UA0 = 0.0001 * DesTowerLoad;                // Assume deltaT = 10000K (limit)
                UA1 = DesTowerLoad;                         // Assume deltaT = 1K
                this->WaterTemp = this->DesInletWaterTemp;  // 35.0; // 95F design inlet water temperature
                this->AirTemp = this->DesInletAirDBTemp;    // 95F design inlet air dry-bulb temp
                this->AirWetBulb = this->DesInletAirWBTemp; // 78F design inlet air wet-bulb temp
                this->AirPress = state.dataEnvrn->StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                auto f = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                } else if (SolFla == -2) {
                    ShowSevereError(state, "Bad starting values for UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->HighSpeedTowerUA = UA;
                }
            } else {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->HighSpeedTowerUA = 0.0;
                }
            }
            if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                                 this->HighSpeedTowerUA);
                }
            } else if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA);
                }
            }
        }

        if (this->LowSpeedAirFlowRateWasAutoSized) {

            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->LowSpeedAirFlowRate = this->LowSpeedAirFlowRateSizingFactor * this->HighSpeedAirFlowRate;
                tmpLowSpeedAirFlowRate = this->LowSpeedAirFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Low Fan Speed Air Flow Rate [m3/s]", this->LowSpeedAirFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Low Fan Speed Air Flow Rate [m3/s]", this->LowSpeedAirFlowRate);
                }
            } else {
                tmpLowSpeedAirFlowRate = this->LowSpeedAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate;
            }
        }

        if (this->LowSpeedFanPowerWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->LowSpeedFanPower = this->LowSpeedFanPowerSizingFactor * this->HighSpeedFanPower;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower);
                }
            }
        }

        if (this->LowSpeedTowerUAWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            this->LowSpeedTowerUA = this->LowSpeedTowerUASizingFactor * this->HighSpeedTowerUA;
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "U-Factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedTowerUA);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "Initial U-Factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedTowerUA);
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::NominalCapacity) {
            if (this->TowerLowSpeedNomCapWasAutoSized) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->TowerLowSpeedNomCap = this->TowerLowSpeedNomCapSizingFactor * this->TowerNominalCapacity;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Low Speed Nominal Capacity [W]", this->TowerLowSpeedNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Low Speed Nominal Capacity [W]", this->TowerLowSpeedNomCap);
                    }
                }
            }
            if (this->TowerFreeConvNomCapWasAutoSized) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->TowerFreeConvNomCap = this->TowerFreeConvNomCapSizingFactor * this->TowerNominalCapacity;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                    }
                }
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::NominalCapacity && UtilityRoutines::SameString(this->TowerType, "CoolingTower:TwoSpeed")) {
            if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow && this->TowerLowSpeedNomCap > 0.0) {

                // nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now is a
                // user input
                Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     29.44,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName); // 85F design exiting water temp
                Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         29.44,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName); // 85F design exiting water temp
                DesTowerLoad = this->TowerLowSpeedNomCap * this->HeatRejectCapNomCapSizingRatio;
                Par[0] = DesTowerLoad;
                Par[1] = double(this->thisTowerNum);
                Par[2] = rho * tmpDesignWaterFlowRate;      // design water mass flow rate
                Par[3] = tmpLowSpeedAirFlowRate;            // Air volume flow rate at low fan speed
                Par[4] = Cp;                                // 85F design exiting water temp
                UA0 = 0.0001 * DesTowerLoad;                // Assume deltaT = 10000K (limit)
                UA1 = DesTowerLoad;                         // Assume deltaT = 1K
                this->WaterTemp = this->DesInletWaterTemp;  // 35.0; // 95F design inlet water temperature
                this->AirTemp = this->DesInletAirDBTemp;    // 35.0; // 95F design inlet air dry-bulb temp
                this->AirWetBulb = this->DesInletAirWBTemp; // 25.6; // 78F design inlet air wet-bulb temp
                this->AirPress = state.dataEnvrn->StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                auto f = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                } else if (SolFla == -2) {
                    ShowSevereError(state, "Bad starting values for UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->LowSpeedTowerUA = UA;
                }
            } else {
                this->LowSpeedTowerUA = 0.0;
            }
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "Low Fan Speed U-Factor Times Area Value [W/K]", this->LowSpeedTowerUA);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "Initial Low Fan Speed U-Factor Times Area Value [W/K]", this->LowSpeedTowerUA);
            }
        }

        if (this->FreeConvAirFlowRateWasAutoSized) {
            this->FreeConvAirFlowRate = this->FreeConvAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->FreeConvAirFlowRate = this->FreeConvAirFlowRateSizingFactor * this->HighSpeedAirFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                }
            }
        }

        if (this->FreeConvTowerUAWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->FreeConvTowerUA = this->FreeConvTowerUASizingFactor * this->HighSpeedTowerUA;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Free Convection U-Factor Times Area Value [W/K]", this->FreeConvTowerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "Initial Free Convection U-Factor Times Area Value [W/K]", this->FreeConvTowerUA);
                }
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::NominalCapacity) {
            if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow && this->TowerFreeConvNomCap > 0.0) {
                // nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now user
                // input
                Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     29.44,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName); // 85F design exiting water temp
                Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                         29.44,
                                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName); // 85F design exiting water temp
                DesTowerLoad = this->TowerFreeConvNomCap * this->HeatRejectCapNomCapSizingRatio;
                Par[0] = DesTowerLoad;
                Par[1] = double(this->thisTowerNum);
                Par[2] = rho * this->DesignWaterFlowRate;   // design water mass flow rate
                Par[3] = this->FreeConvAirFlowRate;         // free convection air volume flow rate
                Par[4] = Cp;                                // 85F design exiting water temp
                UA0 = 0.0001 * DesTowerLoad;                // Assume deltaT = 10000K (limit)
                UA1 = DesTowerLoad;                         // Assume deltaT = 1K
                this->WaterTemp = this->DesInletWaterTemp;  // 35.0; // 95F design inlet water temperature
                this->AirTemp = this->DesInletAirDBTemp;    // 35.0; // 95F design inlet air dry-bulb temp
                this->AirWetBulb = this->DesInletAirWBTemp; // 25.6; // 78F design inlet air wet-bulb temp
                this->AirPress = state.dataEnvrn->StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                auto f = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                } else if (SolFla == -2) {
                    ShowSevereError(state, "Bad starting values for UA calculations");
                    ShowContinueError(state, "Tower inlet design water temperature assumed to be 35.0 C.");
                    ShowContinueError(state, "Tower inlet design air dry-bulb temperature assumed to be 35.0 C.");
                    ShowContinueError(state, "Tower inlet design air wet-bulb temperature assumed to be 25.6 C.");
                    ShowContinueError(state,
                                      format("Tower load assumed to be {:.3T} times free convection capacity of {:.0T} W.",
                                             this->HeatRejectCapNomCapSizingRatio,
                                             this->TowerFreeConvNomCap));

                    Real64 OutWaterTemp; // outlet water temperature during sizing [C]
                    OutWaterTemp = this->calculateSimpleTowerOutletTemp(state, Par[2], Par[3], UA0);
                    Real64 CoolingOutput = Par[4] * Par[2] * (this->WaterTemp - OutWaterTemp); // tower capacity during sizing [W]
                    ShowContinueError(state, format("Tower capacity at lower UA guess ({:.4T}) = {:.0T} W.", UA0, CoolingOutput));

                    OutWaterTemp = this->calculateSimpleTowerOutletTemp(state, Par[2], Par[3], UA1);
                    CoolingOutput = Par[4] * Par[2] * (this->WaterTemp - OutWaterTemp);
                    ShowContinueError(state, format("Tower capacity at upper UA guess ({:.4T}) = {:.0T} W.", UA1, CoolingOutput));

                    if (CoolingOutput < DesTowerLoad) {
                        ShowContinueError(state, "Free convection capacity should be less than tower capacity at upper UA guess.");
                    }
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->FreeConvTowerUA = UA;
                }
            } else {
                this->FreeConvTowerUA = 0.0;
            }
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                                             this->TowerType,
                                             this->Name,
                                             "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                             this->FreeConvTowerUA);
            }
        }

        // calibrate variable speed tower model based on user input by finding calibration water flow rate ratio that
        // yields an approach temperature that matches user input
        if (UtilityRoutines::SameString(this->TowerType, "CoolingTower:VariableSpeed")) {

            Par[0] = this->thisTowerNum;   // Index to cooling tower
            Par[1] = 1.0;                  // air flow rate ratio
            Par[2] = this->DesignInletWB;  // inlet air wet-bulb temperature [C]
            Par[3] = this->DesignRange;    // tower range temperature [C]
            Par[4] = this->DesignApproach; // design approach temperature [C]
            Par[5] = 0.0;                  // Calculation FLAG, 0.0 = calc water flow ratio, 1.0 calc air flow ratio

            //   check range for water flow rate ratio (make sure RegulaFalsi converges)
            Real64 MaxWaterFlowRateRatio = 0.5; // maximum water flow rate ratio which yields desired approach temp
            Real64 Tapproach = 0.0;             // temporary tower approach temp variable [C]
            Real64 const FlowRateRatioStep = (state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio -
                                              state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio) /
                                             10.0;
            bool ModelCalibrated = true;
            Real64 ModelWaterFlowRatioMax = state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio *
                                            4.0; // maximum water flow rate ratio used for model calibration
            //   find a flow rate large enough to provide an approach temperature > than the user defined approach
            Real64 WaterFlowRateRatio(0.0); // tower water flow rate ratio
            while (Tapproach < this->DesignApproach && MaxWaterFlowRateRatio <= ModelWaterFlowRatioMax) {
                WaterFlowRateRatio = MaxWaterFlowRateRatio;
                Tapproach = this->calculateVariableSpeedApproach(state, WaterFlowRateRatio, 1.0, this->DesignInletWB, this->DesignRange);
                if (Tapproach < this->DesignApproach) {
                    MaxWaterFlowRateRatio += FlowRateRatioStep;
                }
                // a water flow rate large enough to provide an approach temperature > than the user defined approach does not exist
                // within the tolerances specified by the user
                if ((MaxWaterFlowRateRatio == 0.5 && Tapproach > this->DesignApproach) || MaxWaterFlowRateRatio >= ModelWaterFlowRatioMax) {
                    ModelCalibrated = false;
                    break;
                }
            }

            Real64 WaterFlowRatio(0.0); // tower water flow rate ratio found during model calibration

            if (ModelCalibrated) {
                auto f = std::bind(&CoolingTower::residualTa, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                General::SolveRoot(
                    state, Acc, MaxIte, SolFla, WaterFlowRatio, f, DataPrecisionGlobals::constant_pointfive, MaxWaterFlowRateRatio, Par);
                if (SolFla == -1) {
                    ShowSevereError(state, "Iteration limit exceeded in calculating tower water flow ratio during calibration");
                    ShowContinueError(state,
                                      "Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio "
                                      "for this variable-speed cooling tower.");
                    ShowFatalError(state, "Cooling tower calibration failed for tower " + this->Name);
                } else if (SolFla == -2) {
                    ShowSevereError(state, "Bad starting values for cooling tower water flow rate ratio calibration.");
                    ShowContinueError(state,
                                      "Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio "
                                      "for this variable-speed cooling tower.");
                    ShowFatalError(state, "Cooling tower calibration failed for tower " + this->Name + '.');
                }
            } else {
                ShowSevereError(state, "Bad starting values for cooling tower water flow rate ratio calibration.");
                ShowContinueError(state, "Design inlet air wet-bulb or range temperature must be modified to achieve the design approach");
                ShowContinueError(state,
                                  format("A water flow rate ratio of {:.6F} was calculated to yield an approach temperature of {:.2F}.",
                                         WaterFlowRateRatio,
                                         Tapproach));
                ShowFatalError(state, "Cooling tower calibration failed for tower " + this->Name + '.');
            }

            this->CalibratedWaterFlowRate = this->DesignWaterFlowRate / WaterFlowRatio;

            if (WaterFlowRatio < state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio ||
                WaterFlowRatio > state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio) {
                ShowWarningError(state,
                                 format("CoolingTower:VariableSpeed, \"{}\" the calibrated water flow rate ratio is determined to be {:9.6F}. This "
                                        "is outside the valid range of {:.2F} to {:.2F}.",
                                        this->Name,
                                        WaterFlowRatio,
                                        state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio,
                                        state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio));
            }

            Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                 (this->DesignInletWB + this->DesignApproach + this->DesignRange),
                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                 RoutineName);
            Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     (this->DesignInletWB + this->DesignApproach + this->DesignRange),
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName);

            this->TowerNominalCapacity = ((rho * tmpDesignWaterFlowRate) * Cp * this->DesignRange);
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
            }
            this->FreeConvAirFlowRate = this->MinimumVSAirFlowFrac * this->HighSpeedAirFlowRate;

            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "Air Flow Rate in free convection regime [m3/s]", this->FreeConvAirFlowRate);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->TowerType, this->Name, "Initial Air Flow Rate in free convection regime [m3/s]", this->FreeConvAirFlowRate);
            }
            this->TowerFreeConvNomCap = this->TowerNominalCapacity * this->FreeConvectionCapacityFraction;

            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                                             this->TowerType,
                                             this->Name,
                                             "Tower capacity in free convection regime at design conditions [W]",
                                             this->TowerFreeConvNomCap);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                                             this->TowerType,
                                             this->Name,
                                             "Initial Tower capacity in free convection regime at design conditions [W]",
                                             this->TowerFreeConvNomCap);
            }
        }
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, this->TowerType);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->TowerNominalCapacity);
        }

        // input error checking
        bool ErrorsFound = false;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_SingleSpd) {
                if (this->DesignWaterFlowRate > 0.0) {
                    if (this->FreeConvAirFlowRate >= this->HighSpeedAirFlowRate) {
                        ShowSevereError(state,
                                        cCoolingTower_SingleSpeed + " \"" + this->Name +
                                            "\". Free convection air flow rate must be less than the design air flow rate.");
                        ErrorsFound = true;
                    }
                    if (this->FreeConvTowerUA >= this->HighSpeedTowerUA) {
                        ShowSevereError(
                            state, cCoolingTower_SingleSpeed + " \"" + this->Name + "\". Free convection UA must be less than the design tower UA.");
                        ErrorsFound = true;
                    }
                }
            }

            if (this->TowerType_Num == DataPlant::TypeOf_CoolingTower_TwoSpd) {
                if (this->DesignWaterFlowRate > 0.0) {
                    if (this->HighSpeedAirFlowRate <= this->LowSpeedAirFlowRate) {
                        ShowSevereError(state,
                                        cCoolingTower_TwoSpeed + " \"" + this->Name +
                                            "\". Low speed air flow rate must be less than the high speed air flow rate.");
                        ErrorsFound = true;
                    }
                    if (this->LowSpeedAirFlowRate <= this->FreeConvAirFlowRate) {
                        ShowSevereError(state,
                                        cCoolingTower_TwoSpeed + " \"" + this->Name +
                                            "\". Free convection air flow rate must be less than the low speed air flow rate.");
                        ErrorsFound = true;
                    }
                    if (this->HighSpeedTowerUA <= this->LowSpeedTowerUA) {
                        ShowSevereError(state,
                                        cCoolingTower_TwoSpeed + " \"" + this->Name +
                                            "\". Tower UA at low fan speed must be less than the tower UA at high fan speed.");
                        ErrorsFound = true;
                    }
                    if (this->LowSpeedTowerUA <= this->FreeConvTowerUA) {
                        ShowSevereError(state,
                                        cCoolingTower_TwoSpeed + " \"" + this->Name +
                                            "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed.");
                        ErrorsFound = true;
                    }
                }
            }
            if (ErrorsFound) {
                ShowFatalError(state, "initialize: Program terminated due to previous condition(s).");
            }
        }
    }

    void CoolingTower::SizeVSMerkelTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static constexpr std::string_view RoutineName("SizeTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SolFla; // Flag of solver
        Real64 tmpHighSpeedFanPower;

        std::array<Real64, 6> Par;        // Parameter array need for RegulaFalsi routine
        Real64 UA0;                       // Lower bound for UA [W/C]
        Real64 UA1;                       // Upper bound for UA [W/C]
        Real64 DesTowerLoad;              // Design tower load [W]
        Real64 Cp(0);                     // local specific heat for fluid
        Real64 rho(0);                    // local density for fluid
        Real64 UA;                        // Calculated UA value
        Real64 DesTowerInletWaterTemp;    // design tower inlet water temperature
        Real64 DesTowerExitWaterTemp;     // design tower exit water temperature
        Real64 DesTowerWaterDeltaT;       // design tower temperature range
        Real64 DesTowerApproachFromPlant; // design tower approach temperature from plant sizing object
        Real64 TolTemp(0.04);             // DeltaT and DesApproach diffs tollerance between plant sizing data and user input in cooling tower
        // for warning message reporting purpose only

        // Find the appropriate Plant Sizing object
        int PltSizCondNum = state.dataPlnt->PlantLoop(this->LoopNum).PlantSizNum;

        Real64 tmpNomTowerCap = this->TowerNominalCapacity;
        Real64 tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
        Real64 tmpTowerFreeConvNomCap = this->TowerFreeConvNomCap;
        Real64 tmpDesignAirFlowRate = this->HighSpeedAirFlowRate;
        Real64 tmpFreeConvAirFlowRate = this->FreeConvAirFlowRate;
        Real64 DesTowerInletAirWBTemp = this->DesInletAirWBTemp;
        Real64 DesTowerInletAirDBTemp = this->DesInletAirDBTemp;

        auto &PlantSizData(state.dataSize->PlantSizData);

        if (this->TowerInletCondsAutoSize) {
            if (PltSizCondNum > 0) {
                // use plant sizing data
                DesTowerExitWaterTemp = PlantSizData(PltSizCondNum).ExitTemp;
                DesTowerInletWaterTemp = DesTowerExitWaterTemp + PlantSizData(PltSizCondNum).DeltaT;
                DesTowerWaterDeltaT = PlantSizData(PltSizCondNum).DeltaT;
            } else {
                // set default values to replace hard wired input assumptions
                DesTowerExitWaterTemp = this->DesOutletWaterTemp;
                DesTowerInletWaterTemp = this->DesInletWaterTemp;
                DesTowerWaterDeltaT = this->DesRange;
            }
        } else {
            // use tower sizing data
            DesTowerExitWaterTemp = this->DesOutletWaterTemp;
            DesTowerInletWaterTemp = this->DesInletWaterTemp;
            DesTowerWaterDeltaT = this->DesRange;
            if (PltSizCondNum > 0) {
                // check the tower range against the plant sizing data
                if (std::abs(DesTowerWaterDeltaT - PlantSizData(PltSizCondNum).DeltaT) > TolTemp) {
                    ShowWarningError(state,
                                     "Error when autosizing the load for cooling tower = " + this->Name +
                                         ". Tower Design Range Temperature is different from the Design Loop Delta Temperature.");
                    ShowContinueError(state, "Tower Design Range Temperature specified in tower = " + this->Name);
                    ShowContinueError(state,
                                      "is inconsistent with Design Loop Delta Temperature specified in Sizing:Plant object = " +
                                          PlantSizData(PltSizCondNum).PlantLoopName + ".");
                    ShowContinueError(state, format("..The Design Range Temperature specified in tower is = {:.2T}", this->DesRange));
                    ShowContinueError(state,
                                      format("..The Design Loop Delta Temperature specified iin plant sizing data is = {:.2T}",
                                             PlantSizData(PltSizCondNum).DeltaT));
                }
                // check if the tower approach is different from plant sizing data
                DesTowerApproachFromPlant = PlantSizData(PltSizCondNum).ExitTemp - this->DesInletAirWBTemp;
                if (std::abs(DesTowerApproachFromPlant - this->DesApproach) > TolTemp) {
                    ShowWarningError(state,
                                     "Error when autosizing the UA for cooling tower = " + this->Name +
                                         ". Tower Design Approach Temperature is inconsistent with Approach from Plant Sizing Data.");
                    ShowContinueError(state,
                                      "The Design Approach Temperature from inputs specified in Sizing:Plant object = " +
                                          PlantSizData(PltSizCondNum).PlantLoopName);
                    ShowContinueError(state, "is inconsistent with Design Approach Temperature specified in tower = " + this->Name + ".");
                    ShowContinueError(state,
                                      format("..The Design Approach Temperature from inputs specified is = {:.2T}", DesTowerApproachFromPlant));
                    ShowContinueError(state, format("..The Design Approach Temperature specified in tower is = {:.2T}", this->DesApproach));
                }
            }
        }

        if (this->PerformanceInputMethod_Num == PIM::NominalCapacity) {

            if (PltSizCondNum > 0) { // get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
                if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            DesTowerExitWaterTemp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DesTowerExitWaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
                    DesTowerLoad = rho * Cp * PlantSizData(PltSizCondNum).DesVolFlowRate * DesTowerWaterDeltaT * this->SizFac;
                    tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                } else {
                    if (this->TowerNominalCapacityWasAutoSized) tmpNomTowerCap = 0.0;
                }
            } else {                                  // PltSizCondNum = 0
                if (!this->TowerInletCondsAutoSize) { // can use design data entered into tower object
                    if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        rho = FluidProperties::GetDensityGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DesTowerExitWaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                    DesTowerExitWaterTemp,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                    RoutineName);
                        DesTowerLoad = rho * Cp * this->DesignWaterFlowRate * DesTowerWaterDeltaT * this->SizFac;
                        tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                    } else {
                        if (this->TowerNominalCapacityWasAutoSized) tmpNomTowerCap = 0.0;
                    }
                } else { // do not have enough data to size.
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize && this->TowerNominalCapacityWasAutoSized) {
                        ShowSevereError(state, "Autosizing error for cooling tower object = " + this->Name);
                        ShowFatalError(state, "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                    }
                }
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->TowerNominalCapacityWasAutoSized) {
                    this->TowerNominalCapacity = tmpNomTowerCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Design Nominal Capacity [W]", tmpNomTowerCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Design Nominal Capacity [W]", this->TowerNominalCapacity);
                    }
                } else { // Hard-sized with sizing data
                    if (this->TowerNominalCapacity > 0.0 && tmpNomTowerCap > 0.0) {
                        Real64 NomCapUser(0.0);
                        NomCapUser = this->TowerNominalCapacity;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         this->TowerType,
                                                         this->Name,
                                                         "Design Nominal Capacity [W]",
                                                         tmpNomTowerCap,
                                                         "User-Specified Nominal Capacity [W]",
                                                         NomCapUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomTowerCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeVSMerkelTower: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", NomCapUser));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpNomTowerCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                            tmpNomTowerCap = NomCapUser;
                        }
                    }
                }
            }

            tmpTowerFreeConvNomCap = tmpNomTowerCap * this->TowerFreeConvNomCapSizingFactor;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->TowerFreeConvNomCapWasAutoSized) {
                    this->TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Design Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Design Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (this->TowerFreeConvNomCap > 0.0 && tmpTowerFreeConvNomCap > 0.0) {
                        Real64 NomCapUser(0.0);
                        NomCapUser = this->TowerFreeConvNomCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         this->TowerType,
                                                         this->Name,
                                                         "Design Free Convection Nominal Capacity [W]",
                                                         tmpTowerFreeConvNomCap,
                                                         "User-Specified Free Convection Nominal Capacity [W]",
                                                         NomCapUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpTowerFreeConvNomCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeVSMerkelTower: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Free Convection Nominal Capacity of {:.2R} [W]", NomCapUser));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Free Convection Nominal Capacity of {:.2R} [W]", tmpTowerFreeConvNomCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                            tmpTowerFreeConvNomCap = NomCapUser;
                        }
                    }
                }
            }

            tmpDesignWaterFlowRate = tmpNomTowerCap * this->DesignWaterFlowPerUnitNomCap;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->DesignWaterFlowRateWasAutoSized) {
                    // for nominal cap input method, get design water flow rate from nominal cap and scalable sizing factor

                    this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                    }

                } else { // Hard-sized with sizing data
                    if (this->DesignWaterFlowRate > 0.0 && tmpDesignWaterFlowRate > 0.0) {
                        Real64 NomDesWaterFlowUser(0.0);
                        NomDesWaterFlowUser = this->DesignWaterFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         this->TowerType,
                                                         this->Name,
                                                         "Design Water Flow Rate [m3/s]",
                                                         this->DesignWaterFlowRate,
                                                         "User-Specified Design Water Flow Rate [m3/s]",
                                                         NomDesWaterFlowUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpDesignWaterFlowRate - NomDesWaterFlowUser) / NomDesWaterFlowUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeVSMerkelTower: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Design Water Flow Rate of {:.2R} [m3/s]", NomDesWaterFlowUser));
                                    ShowContinueError(state, format("differs from Design Water Flow Rate of {:.2R} [m3/s]", tmpDesignWaterFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                            tmpDesignWaterFlowRate = NomDesWaterFlowUser;
                        }
                    }
                }
            }

            PlantUtilities::RegisterPlantCompDesignFlow(state, this->WaterInletNodeNum, tmpDesignWaterFlowRate);

            if (this->DefaultedDesignAirFlowScalingFactor) {
                tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap * (101325.0 / state.dataEnvrn->StdBaroPress);
            } else {
                tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->HighSpeedAirFlowRateWasAutoSized) {
                    this->HighSpeedAirFlowRate = tmpDesignAirFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                    }
                } else { // Hard-sized with sizing data
                    Real64 DesignAirFlowRateUser(0.0);
                    DesignAirFlowRateUser = this->HighSpeedAirFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Design Air Flow Rate [m3/s]",
                                                     tmpDesignAirFlowRate,
                                                     "User-Specified Design Air Flow Rate [m3/s]",
                                                     DesignAirFlowRateUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpDesignAirFlowRate - DesignAirFlowRateUser) / DesignAirFlowRateUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeVSMerkelTower: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(state, format("User-Specified Design Air Flow Rate of {:.2R} [m3/s]", DesignAirFlowRateUser));
                                ShowContinueError(state, format("differs from Design Air Flow Rate of {:.2R} [m3/s]", tmpDesignAirFlowRate));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                        tmpDesignAirFlowRate = DesignAirFlowRateUser;
                    }
                }
            }
            tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * this->FreeConvAirFlowRateSizingFactor;

            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->FreeConvAirFlowRateWasAutoSized) {
                    this->FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Design Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Initial Design Free Convection Regime Air Flow Rate [m3/s]",
                                                     this->FreeConvAirFlowRate);
                    }
                } else { // Hard-sized with sizing data
                    Real64 FreeConvAirFlowUser(0.0);
                    FreeConvAirFlowUser = this->FreeConvAirFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Design Free Convection Regime Air Flow Rate [m3/s]",
                                                     tmpFreeConvAirFlowRate,
                                                     "User-Specified Design Free Convection Regime Air Flow Rate [m3/s]",
                                                     FreeConvAirFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpFreeConvAirFlowRate - FreeConvAirFlowUser) / FreeConvAirFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeVSMerkelTower: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(
                                    state,
                                    format("User-Specified Design Free Convection Regime Air Flow Rate of {:.2R} [m3/s]", FreeConvAirFlowUser));
                                ShowContinueError(
                                    state,
                                    format("differs from Design Free Convection Regime Air Flow Rate of {:.2R} [m3/s]", tmpFreeConvAirFlowRate));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                        tmpFreeConvAirFlowRate = FreeConvAirFlowUser;
                    }
                }
            }

            // now calcuate UA values from nominal capacities and flow rates
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (PltSizCondNum > 0) { // user has a plant sizing object
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DesTowerExitWaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
                    this->WaterTemp = DesTowerInletWaterTemp;
                } else { // probably no plant sizing object
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DataGlobalConstants::InitConvTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
                    this->WaterTemp = DesTowerInletWaterTemp; // 35.0; // design condition
                }
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                        DataGlobalConstants::InitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                        RoutineName);

                // full speed fan tower UA
                Par[0] = tmpNomTowerCap * this->HeatRejectCapNomCapSizingRatio;
                Par[1] = double(this->thisTowerNum);
                Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par[3] = tmpDesignAirFlowRate;         // design air volume flow rate
                Par[4] = Cp;
                UA0 = 0.0001 * Par[0]; // Assume deltaT = 10000K (limit)
                UA1 = Par[0];          // Assume deltaT = 1K

                this->AirTemp = this->DesInletAirDBTemp;    // 35.0;
                this->AirWetBulb = this->DesInletAirWBTemp; // 25.6;
                this->AirPress = state.dataEnvrn->StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                auto f = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                    ShowFatalError(state, "calculating cooling tower UA failed for tower " + this->Name);
                } else if (SolFla == -2) {
                    ShowSevereError(state, "Bad starting values for UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                }
                this->HighSpeedTowerUA = UA;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->TowerType, this->Name, "U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", this->HighSpeedTowerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                                 this->HighSpeedTowerUA);
                }
                // free convection tower UA
                Par[0] = tmpTowerFreeConvNomCap * this->HeatRejectCapNomCapSizingRatio;
                Par[1] = double(this->thisTowerNum);
                Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par[3] = tmpFreeConvAirFlowRate;       // design air volume flow rate
                Par[4] = Cp;
                UA0 = 0.0001 * Par[0]; // Assume deltaT = 10000K (limit)
                UA0 = max(UA0, 1.0);   // limit to 1.0
                UA1 = Par[0];          // Assume deltaT = 1K

                this->AirTemp = this->DesInletAirDBTemp;    // 35.0;
                this->AirWetBulb = this->DesInletAirWBTemp; // 25.6;
                this->AirPress = state.dataEnvrn->StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                auto f2 = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f2, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError(state, "Iteration limit exceeded in calculating tower free convection UA");
                    ShowFatalError(state, "calculating cooling tower UA failed for tower " + this->Name);
                } else if (SolFla == -2) {
                    ShowSevereError(state, "Bad starting values for UA");
                    ShowFatalError(state, "Autosizing of cooling tower UA failed for free convection tower " + this->Name);
                }
                this->FreeConvTowerUA = UA;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                                 this->FreeConvTowerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                                 this->FreeConvTowerUA);
                }
            }
        } else if (this->PerformanceInputMethod_Num == PIM::UFactor) {
            // UA input method

            if (this->DesignWaterFlowRateWasAutoSized) { // get from plant sizing
                // UA input method using plant sizing for flow rate, whereas Nominal capacity method uses scalable sizing factor per cap
                if (PltSizCondNum > 0) {
                    if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        tmpDesignWaterFlowRate = PlantSizData(PltSizCondNum).DesVolFlowRate * this->SizFac;
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(
                                    state, this->TowerType, this->Name, "Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                            }
                            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(
                                    state, this->TowerType, this->Name, "Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                            }
                        }
                    } else {
                        tmpDesignWaterFlowRate = 0.0;
                    }

                } else {
                    if (!this->TowerInletCondsAutoSize) {
                        if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                                this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
                                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                                }
                                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                                }
                            }
                        } else {
                            tmpDesignWaterFlowRate = 0.0;
                        }
                    } else {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            ShowSevereError(state, "Autosizing error for cooling tower object = " + this->Name);
                            ShowFatalError(state, "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                        }
                    }
                }
            }
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->WaterInletNodeNum, tmpDesignWaterFlowRate);

            if (this->HighSpeedTowerUAWasAutoSized) {
                // get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
                if (PltSizCondNum > 0) {
                    if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        rho = FluidProperties::GetDensityGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DesTowerExitWaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                    DesTowerExitWaterTemp,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                    RoutineName);
                        DesTowerLoad = rho * Cp * PlantSizData(PltSizCondNum).DesVolFlowRate * DesTowerWaterDeltaT * this->SizFac;
                        tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->TowerNominalCapacity = tmpNomTowerCap;
                            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                            }
                            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(
                                    state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                            }
                        }
                    } else {
                        tmpNomTowerCap = 0.0;
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->TowerNominalCapacity = tmpNomTowerCap;
                            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                            }
                            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(
                                    state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                            }
                        }
                    }
                } else {
                    if (!this->TowerInletCondsAutoSize) {
                        if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            rho = FluidProperties::GetDensityGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                    DesTowerExitWaterTemp,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                    RoutineName);
                            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                        DesTowerExitWaterTemp,
                                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                        RoutineName);
                            DesTowerLoad = rho * Cp * this->DesignWaterFlowRate * DesTowerWaterDeltaT * this->SizFac;
                            tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                                this->TowerNominalCapacity = tmpNomTowerCap;
                                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                            }
                        } else {
                            tmpNomTowerCap = 0.0;
                            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                                this->TowerNominalCapacity = tmpNomTowerCap;
                                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                            }
                        }
                    } else {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            ShowSevereError(state, "Autosizing error for cooling tower object = " + this->Name);
                            ShowFatalError(state, "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                        }
                    }
                }
                if (this->TowerFreeConvNomCapWasAutoSized) {
                    tmpTowerFreeConvNomCap = tmpNomTowerCap * this->TowerFreeConvNomCapSizingFactor;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                        }
                    }
                }
                if (this->HighSpeedAirFlowRateWasAutoSized) {
                    if (this->DefaultedDesignAirFlowScalingFactor) {
                        tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap * (101325.0 / state.dataEnvrn->StdBaroPress);
                    } else {
                        tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap;
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->HighSpeedAirFlowRate = tmpDesignAirFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                        }
                    }
                }
                if (this->FreeConvAirFlowRateWasAutoSized) {
                    tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * this->FreeConvAirFlowRateSizingFactor;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                        }
                    }
                }
                // now calcuate UA values from nominal capacities and flow rates
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            DataGlobalConstants::InitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DesTowerExitWaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);
                    // full speed fan tower UA
                    Par[0] = tmpNomTowerCap * this->HeatRejectCapNomCapSizingRatio;
                    Par[1] = double(this->thisTowerNum);
                    Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par[3] = tmpDesignAirFlowRate;         // design air volume flow rate
                    Par[4] = Cp;
                    UA0 = 0.0001 * Par[0]; // Assume deltaT = 10000K (limit)
                    UA1 = Par[0];          // Assume deltaT = 1K
                    this->WaterTemp = DesTowerInletWaterTemp;
                    this->AirTemp = this->DesInletAirDBTemp;    // 35.0;
                    this->AirWetBulb = this->DesInletAirWBTemp; // 25.6;
                    this->AirPress = state.dataEnvrn->StdBaroPress;
                    this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                    auto f = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                    General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError(state, "Iteration limit exceeded in calculating tower UA");
                        ShowFatalError(state, "calculating cooling tower UA failed for tower " + this->Name);
                    } else if (SolFla == -2) {
                        ShowSevereError(state, "Bad starting values for UA");
                        ShowFatalError(state, "Autosizing of cooling tower UA failed for tower " + this->Name);
                    }
                    this->HighSpeedTowerUA = UA;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                                     this->HighSpeedTowerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                                     this->HighSpeedTowerUA);
                    }
                    // free convection tower UA
                    Par[0] = tmpTowerFreeConvNomCap * this->HeatRejectCapNomCapSizingRatio;
                    Par[1] = double(this->thisTowerNum);
                    Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par[3] = tmpFreeConvAirFlowRate;       // design air volume flow rate
                    Par[4] = Cp;
                    UA0 = 0.0001 * Par[0]; // Assume deltaT = 10000K (limit)
                    UA1 = Par[0];          // Assume deltaT = 1K
                    this->WaterTemp = DesTowerInletWaterTemp;
                    this->AirTemp = DesTowerInletAirDBTemp;    // 35.0;
                    this->AirWetBulb = DesTowerInletAirWBTemp; // 25.6;
                    this->AirPress = state.dataEnvrn->StdBaroPress;
                    this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                    auto f3 = std::bind(&CoolingTower::residualUA, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                    General::SolveRoot(state, Acc, MaxIte, SolFla, UA, f3, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError(state, "Iteration limit exceeded in calculating tower free convection UA");
                        ShowFatalError(state, "calculating cooling tower UA failed for tower " + this->Name);
                    } else if (SolFla == -2) {
                        ShowSevereError(state, "Bad starting values for UA");
                        ShowFatalError(state, "Autosizing of cooling tower UA failed for free convection tower " + this->Name);
                    }
                    this->LowSpeedTowerUA = UA;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                                     this->FreeConvTowerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->TowerType,
                                                     this->Name,
                                                     "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                                     this->FreeConvTowerUA);
                    }
                }

            } else { // full speed UA given

                if (this->FreeConvTowerUAWasAutoSized) { // determine from scalable sizing factor
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->FreeConvTowerUA = this->HighSpeedTowerUA * this->FreeConvTowerUASizingFactor;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         this->TowerType,
                                                         this->Name,
                                                         "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                                         this->FreeConvTowerUA);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         this->TowerType,
                                                         this->Name,
                                                         "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                                         this->FreeConvTowerUA);
                        }
                    }
                }
                Real64 OutWaterTemp;
                if (this->HighSpeedAirFlowRateWasAutoSized) { // given UA but not air flow rate
                    // need an air flow rate to find capacity from UA but flow rate is scaled off capacity
                    // get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
                    if (PltSizCondNum > 0) {
                        if (PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            rho = FluidProperties::GetDensityGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                    DesTowerExitWaterTemp,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                    RoutineName);
                            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                        DesTowerExitWaterTemp,
                                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                        RoutineName);
                            DesTowerLoad = rho * Cp * PlantSizData(PltSizCondNum).DesVolFlowRate * DesTowerWaterDeltaT;
                            tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
                            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                                this->TowerNominalCapacity = tmpNomTowerCap;
                                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                            }
                        } else {
                            tmpNomTowerCap = rho = Cp = 0.0; // rho and Cp added: Used below
                            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                                this->TowerNominalCapacity = tmpNomTowerCap;
                                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(
                                        state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                                }
                            }
                        }

                    } else {
                        tmpNomTowerCap = 0.0; // Suppress uninitialized warnings
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            ShowSevereError(state, "Autosizing error for cooling tower object = " + this->Name);
                            ShowFatalError(state, "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                        }
                    }

                    if (this->DefaultedDesignAirFlowScalingFactor) {
                        tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap * (101325.0 / state.dataEnvrn->StdBaroPress);
                    } else {
                        tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap;
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->HighSpeedAirFlowRate = tmpDesignAirFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                        }
                    }

                } else { // UA and Air flow rate given, so find Nominal Cap from running model

                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            DesTowerExitWaterTemp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                DesTowerExitWaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);

                    this->WaterTemp = DesTowerInletWaterTemp;
                    this->AirTemp = DesTowerInletAirDBTemp;    // 35.0;
                    this->AirWetBulb = DesTowerInletAirWBTemp; // 25.6;
                    this->AirPress = state.dataEnvrn->StdBaroPress;
                    this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
                    OutWaterTemp =
                        this->calculateSimpleTowerOutletTemp(state, rho * tmpDesignWaterFlowRate, this->HighSpeedAirFlowRate, this->HighSpeedTowerUA);
                    tmpNomTowerCap = Cp * rho * tmpDesignWaterFlowRate * (this->WaterTemp - OutWaterTemp);
                    tmpNomTowerCap /= this->HeatRejectCapNomCapSizingRatio;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->TowerNominalCapacity = tmpNomTowerCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Initial Nominal Capacity [W]", this->TowerNominalCapacity);
                        }
                    }

                } // both UA and air flow rate given

                if (this->FreeConvAirFlowRateWasAutoSized) {
                    tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * this->FreeConvAirFlowRateSizingFactor;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, this->TowerType, this->Name, "Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate);
                        }
                    }
                }

                OutWaterTemp =
                    this->calculateSimpleTowerOutletTemp(state, rho * tmpDesignWaterFlowRate, tmpFreeConvAirFlowRate, this->FreeConvTowerUA);
                tmpTowerFreeConvNomCap = Cp * rho * tmpDesignWaterFlowRate * (this->WaterTemp - OutWaterTemp);
                tmpTowerFreeConvNomCap /= this->HeatRejectCapNomCapSizingRatio;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->TowerType, this->Name, "Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap);
                    }
                }
            }
        }

        tmpHighSpeedFanPower = tmpNomTowerCap * this->DesignFanPowerPerUnitNomCap;
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->HighSpeedFanPowerWasAutoSized) {

                this->HighSpeedFanPower = tmpHighSpeedFanPower;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Design Fan Power [W]", this->HighSpeedFanPower);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->TowerType, this->Name, "Initial Design Fan Power [W]", this->HighSpeedFanPower);
                }
            } else { // Hard-sized with sizing data
                Real64 HighSpeedFanPowerUser(0.0);
                HighSpeedFanPowerUser = this->HighSpeedAirFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->TowerType,
                                                 this->Name,
                                                 "Design Fan Power [W]",
                                                 tmpHighSpeedFanPower,
                                                 "User-Specified Design Fan Power [W]",
                                                 HighSpeedFanPowerUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(tmpHighSpeedFanPower - HighSpeedFanPowerUser) / HighSpeedFanPowerUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, "SizeVSMerkelTower: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Design Fan Power of {:.2R} [W]", HighSpeedFanPowerUser));
                            ShowContinueError(state, format("differs from Design Fan Power of {:.2R} [W]", tmpHighSpeedFanPower));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    } // namespace CondenserLoopTowers

    void CoolingTower::calculateSingleSpeedTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       Aug. 2008, T Hong, Added fluid bypass for single speed cooling tower
        //                      The OutletWaterTemp from calculateSimpleTowerOutletTemp can be lower than 0 degreeC
        //                      which may not be allowed in practice if water is the tower fluid.
        //                      Chandan Sharma, FSEC, February 2010, Added basin heater
        //                      Jul. 2010, A Flament, added multi-cell capability for the 3 types of cooling tower
        //                      Jun. 2016, R Zhang, Applied the condenser supply water temperature sensor fault model
        //                      Jul. 2016, R Zhang, Applied the cooling tower fouling fault model
        //       RE-ENGINEERED  Jan. 2001, Richard Raustad

        // PURPOSE OF THIS SUBROUTINE:
        // To simulate the operation of a single-speed fan cooling tower.

        // METHODOLOGY EMPLOYED:
        // The cooling tower is modeled using effectiveness-NTU relationships for
        // counterflow heat exchangers based on Merkel's theory.
        // The subroutine calculates the period of time required to meet a
        // leaving water temperature setpoint. It assumes that part-load
        // operation represents a linear interpolation of two steady-state regimes.
        // Cyclic losses are neglected. The period of time required to meet the
        // leaving water temperature setpoint is used to determine the required
        // fan power and energy. Free convection regime is also modeled. This
        // occurs when the pump is operating and the fan is off. If free convection
        // regime cooling is all that is required for a given time step, the leaving
        // water temperature is allowed to fall below the leaving water temperature
        // setpoint (free cooling). At times when the cooling tower fan is required,
        // the leaving water temperature is at or above the setpoint.
        // A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
        // or schedule, of the cooling tower. If the tower is OFF, outlet water
        // temperature and flow rate are passed through the model from inlet node to
        // outlet node without intervention (with the exception of free convection
        // where water temperature is allowed to float below the outlet water set
        // point). Reports are also updated with fan power and energy being zero.
        // When the RunFlag indicates an ON condition for the cooling tower, the
        // mass flow rate and water temperature are read from the inlet node of the
        // cooling tower (water-side). The outdoor air wet-bulb temperature is used
        // as the entering condition to the cooling tower (air-side). Input deck
        // parameters are read for the free convection regime (pump ON and fan OFF)
        // and a leaving water temperature is calculated. If the leaving water temperature
        // is at or below the setpoint, the calculated leaving water temperature is
        // placed on the outlet node and no fan power is used. If the calculated leaving
        // water temperature is above the setpoint, the cooling tower fan is turned on
        // and design parameters are used to again calculate the leaving water temperature.
        // If the calculated leaving water temperature is below the setpoint, a fan
        // run-time fraction is calculated and used to determine fan power. The leaving
        // water temperature setpoint is placed on the outlet node. If the calculated
        // leaving water temperature is at or above the setpoint, the calculated
        // leaving water temperature is placed on the outlet node and the fan runs at
        // full power. Water mass flow rate is passed from inlet node to outlet node
        // with no intervention.
        // If a tower has multiple cells, the specified inputs of or the autosized capacity
        //  and air/water flow rates are for the entire tower. The number of cells to operate
        //  is first determined based on the user entered minimal and maximal water flow fractions
        //  per cell. If the loads are not met, more cells (if available) will operate to meet
        //  the loads. Inside each cell, the capacity controls still apply. Each cell operates
        //  in the same way.

        // REFERENCES:
        // ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("calculateSingleSpeedTower");
        int const MaxIteration(100); // Maximum fluid bypass iteration calculations
        static constexpr std::string_view MaxItChar("100");
        Real64 const BypassFractionThreshold(0.01); // Threshold to stop bypass iteration
        Real64 const OWTLowerLimit(0.0);            // The limit of tower exit fluid temperature used in the fluid bypass
        //  calculation to avoid fluid freezing. For water, it is 0 degreeC,
        //  for glycols, it can be much lower. The fluid type is stored at the loop.
        //  Current choices are Water and Steam, needs to expand for glycols

        // set inlet and outlet nodes
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->OutletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;

        Real64 freeConvTowerUA = this->FreeConvTowerUA;
        Real64 highSpeedTowerUA = this->HighSpeedTowerUA;

        // water temperature setpoint
        Real64 TempSetPoint = 0.0;
        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                if (this->SetpointIsOnOutlet) {
                    TempSetPoint = state.dataLoopNodes->Node(this->WaterOutletNodeNum).TempSetPoint;
                } else {
                    TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                if (this->SetpointIsOnOutlet) {
                    TempSetPoint = state.dataLoopNodes->Node(this->WaterOutletNodeNum).TempSetPointHi;
                } else {
                    TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
                }
            }
        }

        // If there is a fault of condenser SWT Sensor
        if (this->FaultyCondenserSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyCondenserSWTIndex;
            Real64 TowerOutletTemp_ff = TempSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyCondenserSWTOffset = state.dataFaultsMgr->FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempSetPoint
            TempSetPoint = TowerOutletTemp_ff - this->FaultyCondenserSWTOffset;
        }

        // If there is a fault of cooling tower fouling
        if (this->FaultyTowerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyTowerFoulingIndex;
            Real64 FreeConvTowerUA_ff = this->FreeConvTowerUA;
            Real64 HighSpeedTowerUA_ff = this->HighSpeedTowerUA;

            // calculate the Faulty Tower Fouling Factor using fault information
            this->FaultyTowerFoulingFactor = state.dataFaultsMgr->FaultsTowerFouling(FaultIndex).CalFaultyTowerFoulingFactor(state);

            // update the tower UA values at faulty cases
            freeConvTowerUA = FreeConvTowerUA_ff * this->FaultyTowerFoulingFactor;
            highSpeedTowerUA = HighSpeedTowerUA_ff * this->FaultyTowerFoulingFactor;
        }

        // Added for fluid bypass. First assume no fluid bypass
        int BypassFlag = 0; // Flag indicator for fluid bypass (0 - no bypass, 1 - bypass)
        Real64 BypassFraction2 = 0.0;
        this->BypassFraction = 0.0;

        // Added for multi-cell. Determine the number of cells operating
        int NumCellMin(0);
        int NumCellMax(0);
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        if (this->DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
            Real64 WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), this->NumCell);
            NumCellMax = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), this->NumCell);
        }
        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;
        if (this->CellCtrl_Num == CellCtrl::MinCell) {
            this->NumCellOn = NumCellMin;
        } else {
            this->NumCellOn = NumCellMax;
        }
        Real64 WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;

        // Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.

        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            // for multiple cells, we assume that it's a common basin
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            return;
        }

        bool IncrNumCellFlag = true; // determine if yes or no we increase the number of cells // set value to true to enter in the loop

        Real64 UAdesign = 0.0; // UA value at design conditions (entered by user or calculated)
        Real64 OutletWaterTempOFF;
        Real64 FanModeFrac = 0.0;
        Real64 AirFlowRate = 0.0;
        while (IncrNumCellFlag) {
            IncrNumCellFlag = false;

            //   Initialize local variables to the free convection design values
            UAdesign = freeConvTowerUA / this->NumCell;
            AirFlowRate = this->FreeConvAirFlowRate / this->NumCell;
            OutletWaterTempOFF = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->OutletWaterTemp = OutletWaterTempOFF;
            FanModeFrac = 0.0;

            OutletWaterTempOFF = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRate, UAdesign);

            //   Assume Setpoint was met using free convection regime (pump ON and fan OFF)
            this->FanPower = 0.0;
            this->OutletWaterTemp = OutletWaterTempOFF;

            if (OutletWaterTempOFF > TempSetPoint) {
                //     Setpoint was not met (or free conv. not used), turn on cooling tower fan
                UAdesign = highSpeedTowerUA / this->NumCell;
                AirFlowRate = this->HighSpeedAirFlowRate / this->NumCell;

                // The fan power is for all cells operating
                Real64 const FanPowerOn = this->HighSpeedFanPower * this->NumCellOn / this->NumCell;

                this->OutletWaterTemp = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRate, UAdesign);

                if (this->OutletWaterTemp <= TempSetPoint) {
                    if (this->CapacityControl == CapacityCtrlEnum::FanCycling || this->OutletWaterTemp <= OWTLowerLimit) {
                        //           Setpoint was met with pump ON and fan ON, calculate run-time fraction
                        FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (this->OutletWaterTemp - OutletWaterTempOFF);
                        this->FanPower = FanModeFrac * FanPowerOn;
                        this->OutletWaterTemp = TempSetPoint;
                    } else {
                        // FluidBypass, fan runs at full speed for the entire time step
                        FanModeFrac = 1.0;
                        this->FanPower = FanPowerOn;
                        BypassFlag = 1;
                    }
                } else {
                    //         Setpoint was not met, cooling tower ran at full capacity
                    FanModeFrac = 1.0;
                    this->FanPower = FanPowerOn;
                    // if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
                    if (this->NumCellOn < this->NumCell && (this->WaterMassFlowRate / (this->NumCellOn + 1)) >= WaterMassFlowRatePerCellMin) {
                        ++this->NumCellOn;
                        WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;
                        IncrNumCellFlag = true;
                    }
                }
            } else if (OutletWaterTempOFF < TempSetPoint) {
                // Need to bypass in free convection cooling mode if bypass is allowed
                if (this->CapacityControl == CapacityCtrlEnum::FluidBypass) {
                    if (OutletWaterTempOFF > OWTLowerLimit) {
                        BypassFlag = 1;
                    }
                }
            }
        }

        // Calculate bypass fraction since OWTLowerLimit < OutletWaterTemp < TempSetPoint.
        // The iteraction ends when the numer of iteraction exceeds the limit or the difference
        //  between the new and old bypass fractions is less than the threshold.
        if (BypassFlag == 1) {
            // Inlet water temperature lower than setpoint, assume 100% bypass, tower fan off
            if (this->InletWaterTemp <= TempSetPoint) {
                this->FanPower = 0.0;
                this->BypassFraction = 1.0;
                this->OutletWaterTemp = this->InletWaterTemp;
            } else {
                if (std::abs(this->InletWaterTemp - this->OutletWaterTemp) <= 0.01) {
                    // Outlet temp is close enough to inlet temp, assume 100% bypass, tower fan off
                    this->BypassFraction = 1.0;
                    this->FanPower = 0.0;
                } else {
                    Real64 bypassFraction = (TempSetPoint - this->OutletWaterTemp) / (this->InletWaterTemp - this->OutletWaterTemp);
                    if (bypassFraction > 1.0 || bypassFraction < 0.0) {
                        // Bypass cannot meet setpoint, assume no bypass
                        this->BypassFraction = 0.0;
                    } else {
                        int NumIteration = 0;
                        Real64 BypassFractionPrev = bypassFraction;
                        Real64 OutletWaterTempPrev = this->OutletWaterTemp;
                        while (NumIteration < MaxIteration) {
                            ++NumIteration;
                            // need to iterate for the new OutletWaterTemp while bypassing tower water
                            this->OutletWaterTemp =
                                this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell * (1.0 - bypassFraction), AirFlowRate, UAdesign);
                            // Calc new BypassFraction based on the new OutletWaterTemp
                            if (std::abs(this->OutletWaterTemp - OWTLowerLimit) <= 0.01) {
                                BypassFraction2 = bypassFraction;
                                break;
                            } else if (this->OutletWaterTemp < OWTLowerLimit) {
                                // Set OutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
                                BypassFraction2 = BypassFractionPrev - (BypassFractionPrev - bypassFraction) * (OutletWaterTempPrev - OWTLowerLimit) /
                                                                           (OutletWaterTempPrev - this->OutletWaterTemp);
                                this->OutletWaterTemp = this->calculateSimpleTowerOutletTemp(
                                    state, WaterMassFlowRatePerCell * (1.0 - BypassFraction2), AirFlowRate, UAdesign);
                                if (this->OutletWaterTemp < OWTLowerLimit) {
                                    // Use previous iteraction values
                                    BypassFraction2 = BypassFractionPrev;
                                    this->OutletWaterTemp = OutletWaterTempPrev;
                                }
                                break;
                            } else {
                                BypassFraction2 = (TempSetPoint - this->OutletWaterTemp) / (this->InletWaterTemp - this->OutletWaterTemp);
                            }

                            // Compare two BypassFraction to determine when to stop
                            if (std::abs(BypassFraction2 - bypassFraction) <= BypassFractionThreshold) break;
                            BypassFractionPrev = bypassFraction;
                            OutletWaterTempPrev = this->OutletWaterTemp;
                            bypassFraction = BypassFraction2;
                        }
                        if (NumIteration > MaxIteration) {
                            ShowWarningError(state,
                                             "Cooling tower fluid bypass iteration exceeds maximum limit of " + std::string{MaxItChar} + " for " + this->Name);
                        }
                        this->BypassFraction = BypassFraction2;
                        // may not meet TempSetPoint due to limit of tower outlet temp to OWTLowerLimit
                        this->OutletWaterTemp = (1.0 - BypassFraction2) * this->OutletWaterTemp + BypassFraction2 * this->InletWaterTemp;
                    }
                }
            }
        }

        // output the fraction of the time step the fan is ON
        this->FanCyclingRatio = FanModeFrac;
        // Should this be water inlet node num?????
        Real64 const CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                      state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                      RoutineName);

        this->Qactual = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);
        this->airFlowRateRatio = (AirFlowRate * this->NumCell) / this->HighSpeedAirFlowRate;
    }

    void CoolingTower::calculateTwoSpeedTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       Feb. 2010, Chandan Sharma, FSEC, Added basin heater
        //                      Jul. 2010, A Flament, added multi-cell capability for the 3 types of cooling tower
        //                      Jun. 2016, R Zhang, Applied the condenser supply water temperature sensor fault model
        //                      Jul. 2016, R Zhang, Applied the cooling tower fouling fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To simulate the operation of a cooling tower with a two-speed fan.

        // METHODOLOGY EMPLOYED:
        // The cooling tower is modeled using effectiveness-NTU relationships for
        // counterflow heat exchangers based on Merkel's theory.
        // The subroutine calculates the period of time required to meet a
        // leaving water temperature setpoint. It assumes that part-load
        // operation represents a linear interpolation of three steady-state regimes
        // (high-speed fan operation, low-speed fan operation and free convection regime).
        // Cyclic losses are neglected. The period of time required to meet the
        // leaving water temperature setpoint is used to determine the required
        // fan power and energy. Free convection regime is also modeled. This
        // occures when the pump is operating and the fan is off. If free convection
        // regime cooling is all that is required for a given time step, the leaving
        // water temperature is allowed to fall below the leaving water temperature
        // setpoint (free cooling). At times when the cooling tower fan is required,
        // the leaving water temperature is at or above the setpoint.
        // A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
        // or schedule, of the cooling tower. If the tower is OFF, outlet water
        // temperature and flow rate are passed through the model from inlet node to
        // outlet node without intervention (with the exception of free convection
        // where water temperature is allowed to float below the outlet water set
        // point). Reports are also updated with fan power and fan energy being zero.
        // When the RunFlag indicates an ON condition for the cooling tower, the
        // mass flow rate and water temperature are read from the inlet node of the
        // cooling tower (water-side). The outdoor air wet-bulb temperature is used
        // as the entering condition to the cooling tower (air-side). Input deck
        // parameters are read for the free convection regime (pump ON and fan OFF)
        // and a leaving water temperature is calculated. If the leaving water temperature
        // is at or below the setpoint, the calculated leaving water temperature is
        // placed on the outlet node and no fan power is used. If the calculated leaving
        // water temperature is above the setpoint, the cooling tower fan is turned on
        // and parameters for low fan speed are used to again calculate the leaving
        // water temperature. If the calculated leaving water temperature is
        // below the setpoint, a fan run-time fraction (FanModeFrac) is calculated and
        // used to determine fan power. The leaving water temperature setpoint is placed
        // on the outlet node. If the calculated leaving water temperature is at or above
        // the setpoint, the cooling tower fan is turned on 'high speed' and the routine is
        // repeated. If the calculated leaving water temperature is below the setpoint,
        // a fan run-time fraction is calculated for the second stage fan and fan power
        // is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
        // If the calculated leaving water temperature is above the leaving water temp.
        // setpoint, the calculated leaving water temperature is placed on the outlet
        // node and the fan runs at full power (High Speed Fan Power). Water mass flow
        // rate is passed from inlet node to outlet node with no intervention.
        // If a tower has multiple cells, the specified inputs of or the autosized capacity
        //  and air/water flow rates are for the entire tower. The number of cells to operate
        //  is first determined based on the user entered minimal and maximal water flow fractions
        //  per cell. If the loads are not met, more cells (if available) will operate to meet
        //  the loads. Each cell operates in same way - same fan speed etc.
        // REFERENCES:
        // ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("calculateTwoSpeedTower");

        // init
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->OutletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;

        Real64 freeConvTowerUA = this->FreeConvTowerUA;
        Real64 highSpeedTowerUA = this->HighSpeedTowerUA;

        // water temperature setpoint
        Real64 TempSetPoint = 0.0;
        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                if (this->SetpointIsOnOutlet) {
                    TempSetPoint = state.dataLoopNodes->Node(this->WaterOutletNodeNum).TempSetPoint;
                } else {
                    TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                if (this->SetpointIsOnOutlet) {
                    TempSetPoint = state.dataLoopNodes->Node(this->WaterOutletNodeNum).TempSetPointHi;
                } else {
                    TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
                }
            }
        }

        // If there is a fault of condenser SWT Sensor
        if (this->FaultyCondenserSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyCondenserSWTIndex;
            Real64 TowerOutletTemp_ff = TempSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyCondenserSWTOffset = state.dataFaultsMgr->FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempSetPoint
            TempSetPoint = TowerOutletTemp_ff - this->FaultyCondenserSWTOffset;
        }

        // If there is a fault of cooling tower fouling
        if (this->FaultyTowerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyTowerFoulingIndex;
            Real64 FreeConvTowerUA_ff = this->FreeConvTowerUA;
            Real64 HighSpeedTowerUA_ff = this->HighSpeedTowerUA;

            // calculate the Faulty Tower Fouling Factor using fault information
            this->FaultyTowerFoulingFactor = state.dataFaultsMgr->FaultsTowerFouling(FaultIndex).CalFaultyTowerFoulingFactor(state);

            // update the tower UA values at faulty cases
            freeConvTowerUA = FreeConvTowerUA_ff * this->FaultyTowerFoulingFactor;
            highSpeedTowerUA = HighSpeedTowerUA_ff * this->FaultyTowerFoulingFactor;
        }

        // Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.
        if (state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) return; // TODO: WTF
        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            return;
        }

        // Added for multi-cell. Determine the number of cells operating
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;
        int NumCellMin(0);
        int NumCellMax(0);
        Real64 WaterMassFlowRatePerCell;
        if (this->DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
            WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), this->NumCell);
            NumCellMax = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), this->NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (this->CellCtrl_Num == CellCtrl::MinCell) {
            this->NumCellOn = NumCellMin;
        } else {
            this->NumCellOn = NumCellMax;
        }

        WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;

        bool IncrNumCellFlag = true;

        Real64 AirFlowRate = 0.0;
        Real64 FanModeFrac = 0.0;
        int SpeedSel = 0;
        while (IncrNumCellFlag) {
            IncrNumCellFlag = false;

            // set local variable for tower
            Real64 UAdesign = freeConvTowerUA / this->NumCell; // where is NumCellOn?
            AirFlowRate = this->FreeConvAirFlowRate / this->NumCell;
            Real64 OutletWaterTempOFF = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->WaterMassFlowRate = state.dataLoopNodes->Node(this->WaterInletNodeNum).MassFlowRate;
            Real64 OutletWaterTemp1stStage = this->OutletWaterTemp;
            Real64 OutletWaterTemp2ndStage = this->OutletWaterTemp;
            FanModeFrac = 0.0;

            OutletWaterTempOFF = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRate, UAdesign);

            //     Setpoint was met using free convection regime (pump ON and fan OFF)
            this->FanPower = 0.0;
            this->OutletWaterTemp = OutletWaterTempOFF;

            if (OutletWaterTempOFF > TempSetPoint) {
                //     Setpoint was not met (or free conv. not used),turn on cooling tower 1st stage fan
                UAdesign = this->LowSpeedTowerUA / this->NumCell;
                AirFlowRate = this->LowSpeedAirFlowRate / this->NumCell;
                Real64 const FanPowerLow = this->LowSpeedFanPower * this->NumCellOn / this->NumCell;

                OutletWaterTemp1stStage = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRate, UAdesign);

                if (OutletWaterTemp1stStage <= TempSetPoint) {
                    //         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
                    FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (OutletWaterTemp1stStage - OutletWaterTempOFF);
                    this->FanPower = FanModeFrac * FanPowerLow;
                    this->OutletWaterTemp = TempSetPoint;
                    this->Qactual *= FanModeFrac;
                    SpeedSel = 1;
                } else {
                    //         Setpoint was not met, turn on cooling tower 2nd stage fan
                    UAdesign = highSpeedTowerUA / this->NumCell;
                    AirFlowRate = this->HighSpeedAirFlowRate / this->NumCell;
                    Real64 const FanPowerHigh = this->HighSpeedFanPower * this->NumCellOn / this->NumCell;

                    OutletWaterTemp2ndStage = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRate, UAdesign);

                    if ((OutletWaterTemp2ndStage <= TempSetPoint) && UAdesign > 0.0) {
                        //           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
                        FanModeFrac = (TempSetPoint - OutletWaterTemp1stStage) / (OutletWaterTemp2ndStage - OutletWaterTemp1stStage);
                        this->FanPower = (FanModeFrac * FanPowerHigh) + (1.0 - FanModeFrac) * FanPowerLow;
                        this->OutletWaterTemp = TempSetPoint;
                        SpeedSel = 2;
                    } else {
                        //           Setpoint was not met, cooling tower ran at full capacity
                        this->OutletWaterTemp = OutletWaterTemp2ndStage;
                        this->FanPower = FanPowerHigh;
                        SpeedSel = 2;
                        FanModeFrac = 1.0;
                        // if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
                        if (this->NumCellOn < this->NumCell && (this->WaterMassFlowRate / (this->NumCellOn + 1)) >= WaterMassFlowRatePerCellMin) {
                            ++this->NumCellOn;
                            WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;
                            IncrNumCellFlag = true;
                        }
                    }
                }
            }
        }

        // output the fraction of the time step the fan is ON
        this->FanCyclingRatio = FanModeFrac;
        this->SpeedSelected = SpeedSel;

        Real64 const CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                      state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                      RoutineName);
        this->Qactual = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);
        this->airFlowRateRatio = (AirFlowRate * this->NumCell) / this->HighSpeedAirFlowRate;
    }

    void CoolingTower::calculateVariableSpeedTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   Feb 2005
        //       MODIFIED       Jul. 2010, A Flament, added multi-cell capability for the 3 types of cooling tower
        //                      Jul. 2010, B Griffith, general fluid props
        //                      Jun. 2016, R Zhang, Applied the condenser supply water temperature sensor fault model
        //                      Jul. 2016, R Zhang, Applied the cooling tower fouling fault model
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // To simulate the operation of a variable-speed fan cooling tower.

        // METHODOLOGY EMPLOYED:
        // For each simulation time step, a desired range temperature (Twater,inlet-Twater,setpoint) and desired approach
        // temperature (Twater,setpoint-Tair,WB) is calculated which meets the outlet water temperature setpoint. This
        // desired range and approach temperature also provides a balance point for the empirical model where:
        // Tair,WB + Twater,range + Tapproach = Node(WaterInletNode)%Temp
        // Calculation of water outlet temperature uses one of the following equations:
        // Twater,outlet = Tair,WB + Tapproach          (1)  or
        // Twater,outlet = Twater,inlet - Twater,range  (2)
        // If a solution (or balance) is found, these 2 calculation methods are equal. Equation 2 is used to calculate
        // the outlet water temperature in the free convection regime and at the minimum or maximum fan speed so that
        // if a solution is not reached, the outlet water temperature is approximately equal to the inlet water temperature
        // and the tower fan must be varied to meet the setpoint. Equation 1 is used when the fan speed is varied between
        // the minimum and maximum fan speed to meet the outlet water temperature setpoint.
        // The outlet water temperature in the free convection regime is first calculated to see if the setpoint is met.
        // If the setpoint is met, the fan is OFF and the outlet water temperature is allowed to float below the set
        // point temperature. If the setpoint is not met, the outlet water temperature is re-calculated at the minimum
        // fan speed. If the setpoint is met, the fan is cycled to exactly meet the outlet water temperature setpoint.
        // If the setpoint is not met at the minimum fan speed, the outlet water temperature is re-calculated at the
        // maximum fan speed. If the setpoint at the maximum fan speed is not met, the fan runs at maximum speed the
        // entire time step. If the setpoint is met at the maximum fan speed, the fan speed is varied to meet the setpoint.
        // If a tower has multiple cells, the specified inputs of or the autosized capacity
        //  and air/water flow rates are for the entire tower. The number of cells to operate
        //  is first determined based on the user entered minimal and maximal water flow fractions
        //  per cell. If the loads are not met, more cells (if available) will operate to meet
        //  the loads. Inside each cell, the fan speed varies in the same way.
        // REFERENCES:
        // Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
        // "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
        // ASHRAE Transactions 2002, V. 108, Pt. 1.
        // York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
        // Form 160.00-SG2 (0502). 2002.

        // SUBROUTINE PARAMETER DEFINITIONS:

        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static constexpr std::string_view RoutineName("calculateVariableSpeedTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SolFla(0);             // Flag of solver
        std::array<Real64, 6> Par; // Parameter array for regula falsi solver
        std::string OutputChar;    // character string used for warning messages
        std::string OutputChar2;   // character string used for warning messages
        std::string OutputChar3;   // character string used for warning messages
        std::string OutputChar4;   // character string used for warning messages
        std::string OutputChar5;   // character string used for warning messages

        // Added for multi-cell. Determine the number of cells operating
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;
        int NumCellMin(0);
        int NumCellMax(0);
        Real64 WaterMassFlowRatePerCell;
        if (this->DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
            WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), this->NumCell);
            NumCellMax = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), this->NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (this->CellCtrl_Num == CellCtrl::MinCell) {
            this->NumCellOn = NumCellMin;
        } else {
            this->NumCellOn = NumCellMax;
        }

        WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;

        // Initialize subroutine variables
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->OutletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;

        this->WaterUsage = 0.0;
        Real64 Twb = this->AirWetBulb;
        Real64 TwbCapped = this->AirWetBulb;

        // water temperature setpoint
        Real64 TempSetPoint(0.0); // Outlet water temperature setpoint (C)
        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
            } else {
                assert(false);
            }
        }

        // If there is a fault of condenser SWT Sensor
        if (this->FaultyCondenserSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyCondenserSWTIndex;
            Real64 TowerOutletTemp_ff = TempSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyCondenserSWTOffset = state.dataFaultsMgr->FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempSetPoint
            TempSetPoint = TowerOutletTemp_ff - this->FaultyCondenserSWTOffset;
        }

        Real64 Tr = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - TempSetPoint;
        Real64 Ta = TempSetPoint - this->AirWetBulb;

        // Do not RETURN here if flow rate is less than MassFlowTolerance. Check basin heater and then RETURN.
        if (state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) return; // TODO: WTF
        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            return;
        }

        // loop to increment NumCell if we cannot meet the setpoint with the actual number of cells calculated above
        bool IncrNumCellFlag = true;
        Real64 OutletWaterTempOFF;             // Outlet water temperature with fan OFF (C)
        Real64 OutletWaterTempON = 0.0;        // Outlet water temperature with fan ON at maximum fan speed (C)
        Real64 FreeConvectionCapFrac = 0.0;    // fraction of tower capacity in free convection
        Real64 WaterFlowRateRatioCapped = 0.0; // Water flow rate ratio passed to VS tower model
        Real64 TrCapped;                       // range temp passed to VS tower model
        Real64 TaCapped;                       // approach temp passed to VS tower model
        while (IncrNumCellFlag) {
            IncrNumCellFlag = false;
            // Initialize inlet node water properties
            Real64 const WaterDensity = FluidProperties::GetDensityGlycol(state,
                                                                          state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                          state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp,
                                                                          state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                          RoutineName);
            Real64 const WaterFlowRateRatio = WaterMassFlowRatePerCell / (WaterDensity * this->CalibratedWaterFlowRate / this->NumCell);

            // check independent inputs with respect to model boundaries
            this->checkModelBounds(state, Twb, Tr, Ta, WaterFlowRateRatio, TwbCapped, TrCapped, TaCapped, WaterFlowRateRatioCapped);

            //   determine the free convection capacity by finding the outlet temperature at full air flow and multiplying
            //   the tower's full capacity temperature difference by the percentage of tower capacity in free convection
            //   regime specified by the user

            this->airFlowRateRatio = 1.0;
            OutletWaterTempOFF = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            OutletWaterTempON = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->OutletWaterTemp = OutletWaterTempOFF;
            FreeConvectionCapFrac = this->FreeConvectionCapacityFraction;
            OutletWaterTempON = this->calculateVariableTowerOutletTemp(state, WaterFlowRateRatioCapped, this->airFlowRateRatio, TwbCapped);

            if (OutletWaterTempON > TempSetPoint) {
                this->FanCyclingRatio = 1.0;
                this->airFlowRateRatio = 1.0;
                this->FanPower = this->HighSpeedFanPower * this->NumCellOn / this->NumCell;
                this->OutletWaterTemp = OutletWaterTempON;
                // if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
                if (this->NumCellOn < this->NumCell && (this->WaterMassFlowRate / (this->NumCellOn + 1)) > WaterMassFlowRatePerCellMin) {
                    ++this->NumCellOn;
                    WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;
                    IncrNumCellFlag = true;
                }
            }
        }

        // find the correct air ratio only if full flow is  too much
        if (OutletWaterTempON < TempSetPoint) {
            //   outlet water temperature is calculated in the free convection regime
            OutletWaterTempOFF = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp -
                                 FreeConvectionCapFrac * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - OutletWaterTempON);
            //   fan is OFF
            this->FanCyclingRatio = 0.0;
            //   air flow ratio is assumed to be the fraction of tower capacity in the free convection regime (fan is OFF but air is flowing)
            this->airFlowRateRatio = FreeConvectionCapFrac;

            // Assume setpoint was met using free convection regime (pump ON and fan OFF)
            this->FanPower = 0.0;
            this->OutletWaterTemp = OutletWaterTempOFF;

            if (OutletWaterTempOFF > TempSetPoint) {
                // Setpoint was not met, turn on cooling tower fan at minimum fan speed

                this->airFlowRateRatio = this->MinimumVSAirFlowFrac;
                Real64 OutletWaterTempMIN; // Outlet water temperature with fan at minimum speed (C)
                OutletWaterTempMIN = this->calculateVariableTowerOutletTemp(state, WaterFlowRateRatioCapped, this->airFlowRateRatio, TwbCapped);

                if (OutletWaterTempMIN < TempSetPoint) {
                    //         if setpoint was exceeded, cycle the fan at minimum air flow to meet the setpoint temperature
                    if (this->FanPowerfAirFlowCurve == 0) {
                        this->FanPower = pow_3(this->airFlowRateRatio) * this->HighSpeedFanPower * this->NumCellOn / this->NumCell;
                    } else {
                        Real64 const FanCurveValue = CurveManager::CurveValue(state, this->FanPowerfAirFlowCurve, this->airFlowRateRatio);
                        this->FanPower = max(0.0, (this->HighSpeedFanPower * FanCurveValue)) * this->NumCellOn / this->NumCell;
                    }
                    //       fan is cycling ON and OFF at the minimum fan speed. Adjust fan power and air flow rate ratio according to cycling rate
                    this->FanCyclingRatio = ((OutletWaterTempOFF - TempSetPoint) / (OutletWaterTempOFF - OutletWaterTempMIN));
                    this->FanPower *= this->FanCyclingRatio;
                    this->OutletWaterTemp = TempSetPoint;
                    this->airFlowRateRatio =
                        (this->FanCyclingRatio * this->MinimumVSAirFlowFrac) + ((1 - this->FanCyclingRatio) * FreeConvectionCapFrac);
                } else {
                    //       if setpoint was not met at minimum fan speed, set fan speed to maximum
                    this->airFlowRateRatio = 1.0;
                    //         fan will not cycle and runs the entire time step
                    this->FanCyclingRatio = 1.0;

                    this->OutletWaterTemp =
                        this->calculateVariableTowerOutletTemp(state, WaterFlowRateRatioCapped, this->airFlowRateRatio, TwbCapped);

                    // Setpoint was met with pump ON and fan ON at full flow
                    // Calculate the fraction of full air flow to exactly meet the setpoint temperature

                    Par[0] = this->thisTowerNum; // Index to cooling tower
                    //         cap the water flow rate ratio and inlet air wet-bulb temperature to provide a stable output
                    Par[1] = WaterFlowRateRatioCapped; // water flow rate ratio
                    Par[2] = TwbCapped;                // Inlet air wet-bulb temperature [C]
                    //         do not cap desired range and approach temperature to provide a valid (balanced) output for this simulation time step
                    Par[3] = Tr;  // Tower range temperature [C]
                    Par[4] = Ta;  // desired approach temperature [C]
                    Par[5] = 1.0; // calculate the air flow rate ratio required for a balance
                    auto f = std::bind(&CoolingTower::residualTa, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
                    General::SolveRoot(state, Acc, MaxIte, SolFla, this->airFlowRateRatio, f, this->MinimumVSAirFlowFrac, 1.0, Par);
                    if (SolFla == -1) {
                        if (!state.dataGlobal->WarmupFlag)
                            ShowWarningError(state,
                                             "Cooling tower iteration limit exceeded when calculating air flow rate ratio for tower " + this->Name);
                        //           IF RegulaFalsi cannot find a solution then provide detailed output for debugging
                    } else if (SolFla == -2) {
                        if (!state.dataGlobal->WarmupFlag) {

                            if (this->CoolingTowerAFRRFailedCount < 1) {
                                ++this->CoolingTowerAFRRFailedCount;
                                ShowWarningError(
                                    state,
                                    format("CoolingTower:VariableSpeed \"{}\" - Cooling tower air flow rate ratio calculation failed ", this->Name));
                                ShowContinueError(state,
                                                  format("...with conditions as Twb = {:5.2F}, Trange = {:5.2F}, Tapproach = {:5.2F}, and water flow "
                                                         "rate ratio = {:5.2F}",
                                                         TwbCapped,
                                                         Tr,
                                                         Ta,
                                                         WaterFlowRateRatioCapped));
                                ShowContinueError(state, "...a solution could not be found within the valid range of air flow rate ratios");
                                ShowContinueErrorTimeStamp(
                                    state, format(" ...Valid air flow rate ratio range = {:5.2F} to 1.0.", this->MinimumVSAirFlowFrac));
                                ShowContinueError(state, "...Consider modifying the design approach or design range temperature for this tower.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(state,
                                                               "CoolingTower:VariableSpeed \"" + this->Name +
                                                                   "\" - Cooling tower air flow rate ratio calculation failed error continues.",
                                                               this->CoolingTowerAFRRFailedIndex);
                            }
                        }
                    }

                    //         Use theoretical cubic for determination of fan power if user has not specified a fan power ratio curve
                    if (this->FanPowerfAirFlowCurve == 0) {
                        this->FanPower = pow_3(this->airFlowRateRatio) * this->HighSpeedFanPower * this->NumCellOn / this->NumCell;
                    } else {
                        Real64 const FanCurveValue = CurveManager::CurveValue(state, this->FanPowerfAirFlowCurve, this->airFlowRateRatio);
                        this->FanPower = max(0.0, (this->HighSpeedFanPower * FanCurveValue)) * this->NumCellOn / this->NumCell;
                    }
                    //           outlet water temperature is calculated as the inlet air wet-bulb temperature plus tower approach temperature
                    this->OutletWaterTemp = Twb + Ta;
                } // IF(OutletWaterTempMIN .LT. TempSetPoint)THEN

            } // IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
        }     // IF(OutletWaterTempON .LT. TempSetPoint) ie if tower should not run at full capacity

        Real64 const CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                      state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                      RoutineName);
        this->Qactual = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);

        //   calculate end time of current time step
        Real64 const CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && state.dataHVACGlobal->TimeStepSys >= this->TimeStepSysLast) {
            if (state.dataCondenserLoopTowers->towers(this->VSTower).PrintLGMessage) {
                ++state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountFlowFrac;
                //       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountFlowFrac < 2) {
                    ShowWarningError(state, state.dataCondenserLoopTowers->towers(this->VSTower).LGBuffer1);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).LGBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   this->TowerType + " \"" + this->Name +
                                                       "\" - Liquid to gas ratio is out of range error continues...",
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).ErrIndexLG,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).LGLast,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).LGLast);
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        //   warn user on first occurrence if flow fraction is greater than maximum for the YorkCalc model, use recurring warning stats
        if (this->TowerModelType == ModelType::YorkCalcModel || this->TowerModelType == ModelType::YorkCalcUserDefined) {
            state.dataCondenserLoopTowers->towers(this->VSTower).PrintLGMessage = false;
            //      Do not report error message in free convection regime
            if (this->airFlowRateRatio > this->MinimumVSAirFlowFrac) {
                Real64 const FlowFraction = WaterFlowRateRatioCapped / this->airFlowRateRatio;
                //        Flow fractions greater than a MaxLiquidToGasRatio of 8 are not reliable using the YorkCalc model
                if (FlowFraction > state.dataCondenserLoopTowers->towers(this->VSTower).MaxLiquidToGasRatio) {
                    //          Report warnings only during actual simulation
                    if (!state.dataGlobal->WarmupFlag) {
                        state.dataCondenserLoopTowers->towers(this->VSTower).PrintLGMessage = true;
                        state.dataCondenserLoopTowers->towers(this->VSTower).LGBuffer1 =
                            format("{} \"{}\" - Liquid to gas ratio (L/G) is out of range at {:5.2F}.", this->TowerType, this->Name, FlowFraction);
                        state.dataCondenserLoopTowers->towers(this->VSTower).LGBuffer2 =
                            format(" ...Valid maximum ratio = {:5.2F}. Occurrence info = {}, {} {}",
                                   state.dataCondenserLoopTowers->towers(this->VSTower).MaxLiquidToGasRatio,
                                   state.dataEnvrn->EnvironmentName,
                                   state.dataEnvrn->CurMnDy,
                                   General::CreateSysTimeIntervalString(state));

                        state.dataCondenserLoopTowers->towers(this->VSTower).LGLast = FlowFraction;
                    }
                }
            }
        }
    }

    void CoolingTower::calculateMerkelVariableSpeedTower(EnergyPlusData &state, Real64 &MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.Griffith
        //       DATE WRITTEN   August 2013
        //       MODIFIED       Jun. 2016, R Zhang, Applied the condenser supply water temperature sensor fault model
        //                      Jul. 2016, R Zhang, Applied the cooling tower fouling fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate variable speed tower model using Merkel's theory with UA adjustments developed by Scheier

        // METHODOLOGY EMPLOYED:
        // Find a fan speed that operates the tower to meet MyLoad

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const DesignWetBulb(25.56); // tower outdoor air entering wetbulb for design [C]
        int const MaxIte(500);             // Maximum number of iterations for solver
        Real64 const Acc(1.e-3);           // Accuracy of solver result
        static constexpr std::string_view RoutineName("calculateMerkelVariableSpeedTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::array<Real64, 8> Par; // Parameter array passed to solver
        int SolFla(0);             // Flag of solver

        Real64 const CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                      state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp,
                                                                      state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                      RoutineName);
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->OutletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;

        Real64 freeConvTowerUA = this->FreeConvTowerUA;
        Real64 highSpeedTowerUA = this->HighSpeedTowerUA;

        // If there is a fault of condenser SWT Sensor
        if (this->FaultyCondenserSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyCondenserSWTIndex;
            // calculate the sensor offset using fault information
            this->FaultyCondenserSWTOffset = state.dataFaultsMgr->FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct(state);
        }

        // If there is a fault of cooling tower fouling
        if (this->FaultyTowerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyTowerFoulingIndex;
            Real64 FreeConvTowerUA_ff = this->FreeConvTowerUA;
            Real64 HighSpeedTowerUA_ff = this->HighSpeedTowerUA;

            // calculate the Faulty Tower Fouling Factor using fault information
            this->FaultyTowerFoulingFactor = state.dataFaultsMgr->FaultsTowerFouling(FaultIndex).CalFaultyTowerFoulingFactor(state);

            // update the tower UA values at faulty cases
            freeConvTowerUA = FreeConvTowerUA_ff * this->FaultyTowerFoulingFactor;
            highSpeedTowerUA = HighSpeedTowerUA_ff * this->FaultyTowerFoulingFactor;
        }

        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;

        // Added for multi-cell. Determine the number of cells operating
        int NumCellMin = 0;
        int NumCellMax = 0;
        Real64 WaterMassFlowRatePerCell;
        Real64 UAdesignPerCell;
        Real64 AirFlowRatePerCell;
        if (this->DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
            WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), this->NumCell);
            NumCellMax = min(int((this->WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), this->NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (this->CellCtrl_Num == CellCtrl::MinCell) {
            this->NumCellOn = NumCellMin;
        } else {
            this->NumCellOn = NumCellMax;
        }

        WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;
        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance || (MyLoad > DataHVACGlobals::SmallLoad)) {
            // for multiple cells, we assume that it's a common bassin
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            return;
        }

        if (std::abs(MyLoad) <= DataHVACGlobals::SmallLoad) {
            // tower doesn't need to do anything
            this->OutletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->FanPower = 0.0;
            this->airFlowRateRatio = 0.0;
            this->Qactual = 0.0;
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            return;
        }

        // first find free convection cooling rate
        UAdesignPerCell = freeConvTowerUA / this->NumCell;
        AirFlowRatePerCell = this->FreeConvAirFlowRate / this->NumCell;
        Real64 OutletWaterTempOFF = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
        this->WaterMassFlowRate = state.dataLoopNodes->Node(this->WaterInletNodeNum).MassFlowRate;
        OutletWaterTempOFF = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAdesignPerCell);

        Real64 FreeConvQdot = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - OutletWaterTempOFF);
        this->FanPower = 0.0;

        if (std::abs(MyLoad) <= FreeConvQdot) { // can meet load with free convection and fan off

            this->OutletWaterTemp = OutletWaterTempOFF;
            this->airFlowRateRatio = 0.0;
            this->Qactual = FreeConvQdot;
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);

            return;
        }

        // next find full fan speed cooling rate
        UAdesignPerCell = highSpeedTowerUA / this->NumCell;
        AirFlowRatePerCell = this->HighSpeedAirFlowRate / this->NumCell;
        this->airFlowRateRatio = 1.0;
        Real64 WaterFlowRateRatio = WaterMassFlowRatePerCell / this->DesWaterMassFlowRatePerCell;
        Real64 UAwetbulbAdjFac = CurveManager::CurveValue(state, this->UAModFuncWetBulbDiffCurvePtr, (DesignWetBulb - this->AirWetBulb));
        Real64 UAairflowAdjFac = CurveManager::CurveValue(state, this->UAModFuncAirFlowRatioCurvePtr, this->airFlowRateRatio);
        Real64 UAwaterflowAdjFac = CurveManager::CurveValue(state, this->UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio);
        Real64 UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
        this->OutletWaterTemp = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell);
        Real64 FullSpeedFanQdot =
            this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);
        Real64 FanPowerAdjustFac = 0.0;
        if (FullSpeedFanQdot <= std::abs(MyLoad)) { // full speed is what we want.

            if ((FullSpeedFanQdot + DataHVACGlobals::SmallLoad) < std::abs(MyLoad) && (this->NumCellOn < this->NumCell) &&
                ((this->WaterMassFlowRate / (this->NumCellOn + 1)) >= WaterMassFlowRatePerCellMin)) {
                // If full fan and not meeting setpoint, then increase number of cells until all are used or load is satisfied
                bool IncrNumCellFlag = true; // set value to true to enter in the loop
                while (IncrNumCellFlag) {
                    ++this->NumCellOn;
                    WaterMassFlowRatePerCell = this->WaterMassFlowRate / this->NumCellOn;
                    WaterFlowRateRatio = WaterMassFlowRatePerCell / this->DesWaterMassFlowRatePerCell;
                    UAwaterflowAdjFac = CurveManager::CurveValue(state, this->UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio);
                    UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
                    this->OutletWaterTemp =
                        this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell);
                    IncrNumCellFlag = (FullSpeedFanQdot + DataHVACGlobals::SmallLoad) < std::abs(MyLoad) && (this->NumCellOn < this->NumCell) &&
                                      ((this->WaterMassFlowRate / (this->NumCellOn + 1)) >= WaterMassFlowRatePerCellMin);
                }
                FullSpeedFanQdot =
                    this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);
            }
            this->Qactual = FullSpeedFanQdot;
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            // now calculate fan power
            FanPowerAdjustFac = CurveManager::CurveValue(state, this->FanPowerfAirFlowCurve, this->airFlowRateRatio);
            this->FanPower = this->HighSpeedFanPower * FanPowerAdjustFac * this->NumCellOn / this->NumCell;

            return;
        }

        // next find minimum air flow ratio cooling rate
        this->airFlowRateRatio = this->MinimumVSAirFlowFrac;
        AirFlowRatePerCell = this->airFlowRateRatio * this->HighSpeedAirFlowRate / this->NumCell;
        UAairflowAdjFac = CurveManager::CurveValue(state, this->UAModFuncAirFlowRatioCurvePtr, this->airFlowRateRatio);
        UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
        this->OutletWaterTemp = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell);
        Real64 MinSpeedFanQdot =
            this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);

        if (std::abs(MyLoad) <= MinSpeedFanQdot) { // min fan speed already exceeds load)
            this->Qactual = MinSpeedFanQdot;
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            // now calculate fan power
            FanPowerAdjustFac = CurveManager::CurveValue(state, this->FanPowerfAirFlowCurve, this->airFlowRateRatio);
            this->FanPower = this->HighSpeedFanPower * FanPowerAdjustFac * this->NumCellOn / this->NumCell;
            return;
        }

        if ((MinSpeedFanQdot < std::abs(MyLoad)) && (std::abs(MyLoad) < FullSpeedFanQdot)) {
            // load can be refined by modulating fan speed, call regula-falsi

            Par[0] = double(this->thisTowerNum);
            Par[1] = MyLoad;
            Par[2] = WaterMassFlowRatePerCell;
            Par[3] = UAdesignPerCell;
            Par[4] = UAwetbulbAdjFac;
            Par[5] = UAwaterflowAdjFac;
            Par[6] = CpWater;
            Par[7] = this->WaterMassFlowRate;

            auto f = std::bind(&CoolingTower::residualMerkelLoad, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
            General::SolveRoot(state, Acc, MaxIte, SolFla, this->airFlowRateRatio, f, this->MinimumVSAirFlowFrac, 1.0, Par);

            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (this->VSMerkelAFRErrorIter < 1) {
                        ++this->VSMerkelAFRErrorIter;
                        ShowWarningError(state,
                                         cCoolingTower_VariableSpeedMerkel +
                                             " - Iteration limit exceeded calculating variable speed fan ratio for unit = " + this->Name);
                        ShowContinueError(state,
                                          format("Estimated air flow ratio  = {:.4R}",
                                                 (std::abs(MyLoad) - MinSpeedFanQdot) / (FullSpeedFanQdot - MinSpeedFanQdot)));
                        ShowContinueError(state, format("Calculated air flow ratio = {:.4R}", this->airFlowRateRatio));
                        ShowContinueErrorTimeStamp(state,
                                                   "The calculated air flow ratio will be used and the simulation continues. Occurrence info:");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        cCoolingTower_VariableSpeedMerkel + " \"" + this->Name +
                            "\" - Iteration limit exceeded calculating air flow ratio error continues. air flow ratio statistics follow.",
                        this->VSMerkelAFRErrorIterIndex,
                        this->airFlowRateRatio,
                        this->airFlowRateRatio);
                }
            } else if (SolFla == -2) {
                this->airFlowRateRatio = (std::abs(MyLoad) - MinSpeedFanQdot) / (FullSpeedFanQdot - MinSpeedFanQdot);
                if (!state.dataGlobal->WarmupFlag) {
                    if (this->VSMerkelAFRErrorFail < 1) {
                        ++this->VSMerkelAFRErrorFail;
                        ShowWarningError(state,
                                         cCoolingTower_VariableSpeedMerkel +
                                             " - solver failed calculating variable speed fan ratio for unit = " + this->Name);
                        ShowContinueError(state, format("Estimated air flow ratio  = {:.4R}", this->airFlowRateRatio));
                        ShowContinueErrorTimeStamp(state, "The estimated air flow ratio will be used and the simulation continues. Occurrence info:");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        cCoolingTower_VariableSpeedMerkel + " \"" + this->Name +
                            "\" - solver failed calculating air flow ratio error continues. air flow ratio statistics follow.",
                        this->VSMerkelAFRErrorFailIndex,
                        this->airFlowRateRatio,
                        this->airFlowRateRatio);
                }
            }

            // now rerun to get peformance with AirFlowRateRatio
            AirFlowRatePerCell = this->airFlowRateRatio * this->HighSpeedAirFlowRate / this->NumCell;

            UAairflowAdjFac = CurveManager::CurveValue(state, this->UAModFuncAirFlowRatioCurvePtr, this->airFlowRateRatio);
            UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

            this->OutletWaterTemp = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell);
            this->Qactual = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp - this->OutletWaterTemp);
            CalcBasinHeaterPower(
                state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);

            // now calculate fan power
            FanPowerAdjustFac = CurveManager::CurveValue(state, this->FanPowerfAirFlowCurve, this->airFlowRateRatio);
            this->FanPower = this->HighSpeedFanPower * FanPowerAdjustFac * this->NumCellOn / this->NumCell;
        }
    }

    Real64 CoolingTower::residualMerkelLoad(EnergyPlusData &state,
                                            Real64 airFlowRateRatioLocal,    // fan speed ratio (1.0 is continuous, 0.0 is off)
                                            std::array<Real64, 8> const &Par // par(1) = Tower number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:
        // par(2) =MyLoad [W] , negative is cooling
        // par(3) = water mass flow per cell
        // par(4) = Design UA per cell
        // par(5) = UA adjust factor for wetbulb
        // par(6) = UA adjust factor for water flow rate
        // par(7) = specific heat of water at inlet temp
        // par(8) = water mass flow rate, total [kg/s]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        int TowerNum = int(Par[0]);
        auto const &TargetLoad = Par[1];
        auto const &WaterMassFlowRatePerCell = Par[2];
        auto const &UAdesignPerCell = Par[3];
        auto const &UAwetbulbAdjFac = Par[4];
        auto const &UAwaterflowAdjFac = Par[5];
        auto const &CpWater = Par[6];
        auto const &TotalWaterMassFlowRate = Par[7];

        Real64 const AirFlowRatePerCell = airFlowRateRatioLocal * state.dataCondenserLoopTowers->towers(TowerNum).HighSpeedAirFlowRate /
                                          state.dataCondenserLoopTowers->towers(TowerNum).NumCell;
        Real64 const UAairflowAdjFac =
            CurveManager::CurveValue(state, state.dataCondenserLoopTowers->towers(TowerNum).UAModFuncAirFlowRatioCurvePtr, airFlowRateRatioLocal);
        Real64 const UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

        Real64 OutletWaterTempTrial;
        OutletWaterTempTrial = this->calculateSimpleTowerOutletTemp(state, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell);

        Real64 const Qdot =
            TotalWaterMassFlowRate * CpWater *
            (state.dataLoopNodes->Node(state.dataCondenserLoopTowers->towers(TowerNum).WaterInletNodeNum).Temp - OutletWaterTempTrial);
        return std::abs(TargetLoad) - Qdot;
    }

    Real64 CoolingTower::calculateSimpleTowerOutletTemp(EnergyPlusData &state,
                                                        Real64 const waterMassFlowRate,
                                                        Real64 const AirFlowRate,
                                                        Real64 const UAdesign)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  Shirey, Raustad, Jan 2001

        // PURPOSE OF THIS SUBROUTINE:
        // See purpose for Single Speed or Two Speed tower model

        // METHODOLOGY EMPLOYED:
        // See methodology for Single Speed or Two Speed tower model

        // REFERENCES:
        // Merkel, F. 1925.  Verduftungskuhlung. VDI Forschungsarbeiten, Nr 275, Berlin.
        // ASHRAE     1999.  HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("calculateSimpleTowerOutletTemp");

        // initialize some local variables
        Real64 QactualLocal = 0.0; // Actual heat transfer rate between tower water and air [W]

        // set local tower inlet and outlet temperature variables
        this->InletWaterTemp = this->WaterTemp;
        Real64 OutletWaterTempLocal = this->InletWaterTemp;
        Real64 InletAirTemp = this->AirTemp;       // Dry-bulb temperature of air entering the tower [C]
        Real64 InletAirWetBulb = this->AirWetBulb; // Wetbulb temp of entering moist air [C]

        if (UAdesign == 0.0) return OutletWaterTempLocal;

        // set water and air properties
        Real64 AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state, this->AirPress, InletAirTemp, this->AirHumRat); // Density of air [kg/m3]
        Real64 AirMassFlowRate = AirFlowRate * AirDensity;                                                           // Mass flow rate of air [kg/s]
        Real64 CpAir = Psychrometrics::PsyCpAirFnW(this->AirHumRat);                                                 // Heat capacity of air [J/kg/K]
        Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                this->WaterTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName); // Heat capacity of water [J/kg/K]
        Real64 InletAirEnthalpy =
            Psychrometrics::PsyHFnTdbRhPb(state, this->AirWetBulb, 1.0, this->AirPress); // Enthalpy of entering moist air [J/kg]

        // initialize exiting wet bulb temperature before iterating on final solution
        Real64 OutletAirWetBulb = InletAirWetBulb + 6.0; // Wetbulb temp of exiting moist air [C]

        // Calculate mass flow rates
        if (waterMassFlowRate <= 0.0) {
            OutletWaterTempLocal = this->InletWaterTemp;
            return OutletWaterTempLocal;
        }

        Real64 MdotCpWater = waterMassFlowRate * CpWater; // Water mass flow rate times the heat capacity [W/K]

        int Iter = 0;
        Real64 OutletAirEnthalpy;               // Enthalpy of exiting moist air [J/kg]
        Real64 WetBulbError = 1.0;              // Calculated error for exiting wet-bulb temperature between iterations [delta K/K]
        Real64 DeltaTwb = 1.0;                  // Absolute value of difference between inlet and outlet air wet-bulb temp [C]
        Real64 OutletAirWetBulbLast;            // temporary Wetbulb temp of exiting moist air [C]
        int const IterMax(50);                  // Maximum number of iterations allowed
        Real64 const WetBulbTolerance(0.00001); // Maximum error for exiting wet-bulb temperature between iterations [delta K/K]
        Real64 const DeltaTwbTolerance(0.001);  // Maximum error (tolerance) in DeltaTwb for iteration convergence [C]
        while ((WetBulbError > WetBulbTolerance) && (Iter <= IterMax) && (DeltaTwb > DeltaTwbTolerance)) {
            ++Iter;
            //        OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0,OutBaroPress)
            OutletAirEnthalpy = Psychrometrics::PsyHFnTdbRhPb(state, OutletAirWetBulb, 1.0, this->AirPress);
            // calculate the airside specific heat and capacity
            Real64 const CpAirside =
                (OutletAirEnthalpy - InletAirEnthalpy) /
                (OutletAirWetBulb - InletAirWetBulb);               // Delta enthalpy of the tower air divides by delta air wet-bulb temp [J/kg/K]
            Real64 const AirCapacity = AirMassFlowRate * CpAirside; // MdotCp of air through the tower
            // calculate the minimum to maximum capacity ratios of airside and waterside
            Real64 const CapacityRatioMin = min(AirCapacity, MdotCpWater);    // Minimum capacity of airside and waterside
            Real64 const CapacityRatioMax = max(AirCapacity, MdotCpWater);    // Maximum capacity of airside and waterside
            Real64 const CapacityRatio = CapacityRatioMin / CapacityRatioMax; // Ratio of minimum to maximum capacity
            // Calculate heat transfer coefficient and number of transfer units (NTU)
            Real64 const UAactual = UAdesign * CpAirside / CpAir;        // UA value at actual conditions [W/C]
            Real64 const NumTransferUnits = UAactual / CapacityRatioMin; // Number of transfer Units [NTU]
            // calculate heat exchanger effectiveness
            Real64 effectiveness; // Effectiveness of the heat exchanger [-]
            if (CapacityRatio <= 0.995) {
                Real64 Exponent = NumTransferUnits * (1.0 - CapacityRatio);
                if (Exponent >= 700.0) {
                    effectiveness = NumTransferUnits / (1.0 + NumTransferUnits);
                } else {
                    effectiveness = (1.0 - std::exp(-1.0 * NumTransferUnits * (1.0 - CapacityRatio))) /
                                    (1.0 - CapacityRatio * std::exp(-1.0 * NumTransferUnits * (1.0 - CapacityRatio)));
                }
            } else {
                effectiveness = NumTransferUnits / (1.0 + NumTransferUnits);
            }
            // calculate water to air heat transfer and store last exiting WB temp of air
            QactualLocal = effectiveness * CapacityRatioMin * (this->InletWaterTemp - InletAirWetBulb);
            OutletAirWetBulbLast = OutletAirWetBulb;
            // calculate new exiting wet bulb temperature of airstream
            OutletAirWetBulb = InletAirWetBulb + QactualLocal / AirCapacity;
            // Check error tolerance and exit if satisfied
            DeltaTwb = std::abs(OutletAirWetBulb - InletAirWetBulb);
            // Add KelvinConv to denominator below convert OutletAirWetBulbLast to Kelvin to avoid divide by zero.
            // Wet bulb error units are delta K/K
            WetBulbError = std::abs((OutletAirWetBulb - OutletAirWetBulbLast) / (OutletAirWetBulbLast + DataGlobalConstants::KelvinConv));
        }

        if (QactualLocal >= 0.0) {
            OutletWaterTempLocal = this->InletWaterTemp - QactualLocal / MdotCpWater;
        } else {
            OutletWaterTempLocal = this->InletWaterTemp;
        }
        return OutletWaterTempLocal;
    }

    Real64 CoolingTower::calculateVariableTowerOutletTemp(EnergyPlusData &state,
                                                          Real64 const WaterFlowRateRatio,    // current water flow rate ratio (capped if applicable)
                                                          Real64 const airFlowRateRatioLocal, // current air flow rate ratio
                                                          Real64 const Twb // current inlet air wet-bulb temperature (C, capped if applicable)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Feb. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the leaving water temperature of the variable speed cooling tower.

        // METHODOLOGY EMPLOYED:
        // The range temperature is varied to determine balance point where model output (Tapproach),
        // range temperature and inlet air wet-bulb temperature show a balance as:
        // Twb + Tapproach + Trange = Node(WaterInletNode)%Temp

        // REFERENCES:
        // Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
        // "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
        // ASHRAE Transactions 2002, V. 108, Pt. 1.
        // York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
        // Form 160.00-SG2 (0502). 2002.

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SolFla;                                // Flag of solver
        std::array<Real64, 6> Par;                 // Parameter array for regula falsi solver
        Real64 const VSTowerMaxRangeTemp(22.2222); // set VS cooling tower range maximum value used for solver

        //   determine tower outlet water temperature
        Par[0] = this->thisTowerNum;    // Index to cooling tower
        Par[1] = WaterFlowRateRatio;    // water flow rate ratio
        Par[2] = airFlowRateRatioLocal; // air flow rate ratio
        Par[3] = Twb;                   // inlet air wet-bulb temperature [C]
        Real64 Tr;                      // range temperature which results in an energy balance
        auto f = std::bind(&CoolingTower::residualTr, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
        General::SolveRoot(state, Acc, MaxIte, SolFla, Tr, f, 0.001, VSTowerMaxRangeTemp, Par);

        Real64 OutletWaterTempLocal = this->WaterTemp - Tr;

        if (SolFla == -1) {
            ShowSevereError(state, "Iteration limit exceeded in calculating tower nominal capacity at minimum air flow ratio");
            ShowContinueError(
                state,
                "Design inlet air wet-bulb or approach temperature must be modified to achieve an acceptable range at the minimum air flow rate");
            ShowContinueError(state, "Cooling tower simulation failed to converge for tower " + this->Name);
            //    if SolFla = -2, Tr is returned as minimum value (0.001) and outlet temp = inlet temp - 0.001
        } else if (SolFla == -2) {    // decide if should run at max flow
            Real64 TempSetPoint(0.0); // local temporary for loop setpoint
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
                } else {
                    assert(false);
                }
            }
            if (this->WaterTemp > (TempSetPoint + state.dataCondenserLoopTowers->towers(this->VSTower).MaxRangeTemp)) { // run flat out
                OutletWaterTempLocal = this->WaterTemp - state.dataCondenserLoopTowers->towers(this->VSTower).MaxRangeTemp;
            }
        }
        return OutletWaterTempLocal;
    }

    Real64 CoolingTower::calculateVariableSpeedApproach(EnergyPlusData &state,
                                                        Real64 const PctWaterFlow,      // Water flow ratio of cooling tower
                                                        Real64 const airFlowRatioLocal, // Air flow ratio of cooling tower
                                                        Real64 const Twb,               // Inlet air wet-bulb temperature [C]
                                                        Real64 const Tr // Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Feb. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculate tower approach temperature (e.g. outlet water temp minus inlet air wet-bulb temp)
        // given air flow ratio, water flow ratio, inlet air wet-bulb temp, and tower range.

        // METHODOLOGY EMPLOYED:
        // Calculation method used empirical models from CoolTools or York to determine performance
        // of variable speed (variable air flow rate) cooling towers.

        // REFERENCES:
        // Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
        // "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
        // ASHRAE Transactions 2002, V. 108, Pt. 1.
        // York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
        // Form 160.00-SG2 (0502). 2002.

        Real64 approach;
        if (this->TowerModelType == ModelType::YorkCalcModel || this->TowerModelType == ModelType::YorkCalcUserDefined) {
            Real64 PctAirFlow = airFlowRatioLocal;
            Real64 FlowFactor = PctWaterFlow / PctAirFlow;
            approach = state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(1) +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(2) * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(3) * Twb * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(4) * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(5) * Twb * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(6) * Twb * Twb * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(7) * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(8) * Twb * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(9) * Twb * Twb * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(10) * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(11) * Twb * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(12) * Twb * Twb * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(13) * Tr * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(14) * Twb * Tr * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(15) * Twb * Twb * Tr * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(16) * Tr * Tr * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(17) * Twb * Tr * Tr * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(18) * Twb * Twb * Tr * Tr * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(19) * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(20) * Twb * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(21) * Twb * Twb * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(22) * Tr * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(23) * Twb * Tr * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(24) * Twb * Twb * Tr * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(25) * Tr * Tr * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(26) * Twb * Tr * Tr * FlowFactor * FlowFactor +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(27) * Twb * Twb * Tr * Tr * FlowFactor * FlowFactor;

        } else { // empirical model is CoolTools format
            //     the CoolTools model actually uses PctFanPower = AirFlowRatio^3 as an input to the model
            Real64 PctAirFlow = pow_3(airFlowRatioLocal);
            approach = state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(1) +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(2) * PctAirFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(3) * PctAirFlow * PctAirFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(4) * PctAirFlow * PctAirFlow * PctAirFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(5) * PctWaterFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(6) * PctAirFlow * PctWaterFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(7) * PctAirFlow * PctAirFlow * PctWaterFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(8) * PctWaterFlow * PctWaterFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(9) * PctAirFlow * PctWaterFlow * PctWaterFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(10) * PctWaterFlow * PctWaterFlow * PctWaterFlow +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(11) * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(12) * PctAirFlow * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(13) * PctAirFlow * PctAirFlow * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(14) * PctWaterFlow * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(15) * PctAirFlow * PctWaterFlow * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(16) * PctWaterFlow * PctWaterFlow * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(17) * Twb * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(18) * PctAirFlow * Twb * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(19) * PctWaterFlow * Twb * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(20) * Twb * Twb * Twb +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(21) * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(22) * PctAirFlow * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(23) * PctAirFlow * PctAirFlow * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(24) * PctWaterFlow * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(25) * PctAirFlow * PctWaterFlow * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(26) * PctWaterFlow * PctWaterFlow * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(27) * Twb * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(28) * PctAirFlow * Twb * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(29) * PctWaterFlow * Twb * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(30) * Twb * Twb * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(31) * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(32) * PctAirFlow * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(33) * PctWaterFlow * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(34) * Twb * Tr * Tr +
                       state.dataCondenserLoopTowers->towers(this->VSTower).Coeff(35) * Tr * Tr * Tr;
        }
        return approach;
    }

    void CoolingTower::checkModelBounds(EnergyPlusData &state,
                                        Real64 Twb,                      // current inlet air wet-bulb temperature (C)
                                        Real64 Tr,                       // requested range temperature for current time step (C)
                                        Real64 Ta,                       // requested approach temperature for current time step (C)
                                        Real64 WaterFlowRateRatio,       // current water flow rate ratio at water inlet node
                                        Real64 &TwbCapped,               // bounded value of inlet air wet-bulb temperature (C)
                                        Real64 &TrCapped,                // bounded value of range temperature (C)
                                        Real64 &TaCapped,                // bounded value of approach temperature (C)
                                        Real64 &WaterFlowRateRatioCapped // bounded value of water flow rate ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   Feb 2005
        //       MODIFIED       na
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // To verify that the empirical model's independent variables are within the limits used during the
        // development of the empirical model.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a variable speed cooling tower are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // The range of each independent variable is provided either by the CoolTools or York model limits, or
        // specified by the user if the model is User Defined (in either the CoolTools or York model format).
        // These limits are tested in this subroutine each time step and returned for use by the calling routine.
        // The independent variables capped here may or may not be passed to the empirical model in the calling
        // routine depending on their use.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string OutputChar;     // character string for warning messages
        std::string OutputCharLo;   // character string for warning messages
        std::string OutputCharHi;   // character string for warning messages
        std::string TrimValue;      // character string for warning messages
        Real64 CurrentEndTime(0.0); // end time of time step for current simulation time step
        // current end time is compared with last to see if time step changed

        //   initialize capped variables in case independent variables are in bounds
        TwbCapped = Twb;
        TrCapped = Tr;
        TaCapped = Ta;
        WaterFlowRateRatioCapped = WaterFlowRateRatio;

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;

        //   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && state.dataHVACGlobal->TimeStepSys >= this->TimeStepSysLast) {
            if (state.dataCondenserLoopTowers->towers(this->VSTower).PrintTrMessage) {
                ++state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountTR;
                if (state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountTR < 2) {
                    ShowWarningError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TrBuffer1);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TrBuffer2);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TrBuffer3);
                    ShowContinueError(state, " ...Range temperatures outside model boundaries may not adversely affect tower performance.");
                    ShowContinueError(state, " ...This is not an unexpected occurrence when simulating actual conditions.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   this->TowerType + " \"" + this->Name +
                                                       "\" - Tower range temperature is out of range error continues...",
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).ErrIndexTR,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).TrLast,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).TrLast);
                }
            }
            if (state.dataCondenserLoopTowers->towers(this->VSTower).PrintTwbMessage) {
                ++state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountIAWB;
                if (state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountIAWB < 6) {
                    ShowWarningError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TwbBuffer1);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TwbBuffer2);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TwbBuffer3);
                    ShowContinueError(state, " ...Wet-bulb temperatures outside model boundaries may not adversely affect tower performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   this->TowerType + " \"" + this->Name +
                                                       "\" - Inlet air wet-bulb temperature is out of range error continues...",
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).ErrIndexIAWB,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).TwbLast,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).TwbLast);
                }
            }
            if (state.dataCondenserLoopTowers->towers(this->VSTower).PrintTaMessage) {
                ++state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountTA;
                if (state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountTA < 2) {
                    ShowWarningError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TaBuffer1);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TaBuffer2);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).TaBuffer3);
                    ShowContinueError(state, " ...Approach temperatures outside model boundaries may not adversely affect tower performance.");
                    ShowContinueError(state, " ...This is not an unexpected occurrence when simulating actual conditions.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   this->TowerType + " \"" + this->Name +
                                                       "\" - Tower approach temperature is out of range error continues...",
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).ErrIndexTA,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).TaLast,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).TaLast);
                }
            }
            if (state.dataCondenserLoopTowers->towers(this->VSTower).PrintWFRRMessage) {
                ++state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountWFRR;
                if (state.dataCondenserLoopTowers->towers(this->VSTower).VSErrorCountWFRR < 6) {
                    ShowWarningError(state, state.dataCondenserLoopTowers->towers(this->VSTower).WFRRBuffer1);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).WFRRBuffer2);
                    ShowContinueError(state, state.dataCondenserLoopTowers->towers(this->VSTower).WFRRBuffer3);
                    ShowContinueError(state, " ...Water flow rate ratios outside model boundaries may not adversely affect tower performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   this->TowerType + " \"" + this->Name +
                                                       "\" - Water flow rate ratio is out of range error continues...",
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).ErrIndexWFRR,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).WaterFlowRateRatioLast,
                                                   state.dataCondenserLoopTowers->towers(this->VSTower).WaterFlowRateRatioLast);
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        //   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
        if (Twb < state.dataCondenserLoopTowers->towers(this->VSTower).MinInletAirWBTemp ||
            Twb > state.dataCondenserLoopTowers->towers(this->VSTower).MaxInletAirWBTemp) {
            OutputChar = format("{:.2R}", Twb);
            OutputCharLo = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MinInletAirWBTemp);
            OutputCharHi = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MaxInletAirWBTemp);
            if (Twb < state.dataCondenserLoopTowers->towers(this->VSTower).MinInletAirWBTemp) {
                TwbCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MinInletAirWBTemp;
            }
            if (Twb > state.dataCondenserLoopTowers->towers(this->VSTower).MaxInletAirWBTemp) {
                TwbCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MaxInletAirWBTemp;
            }
            if (!state.dataGlobal->WarmupFlag) {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintTwbMessage = true;
                state.dataCondenserLoopTowers->towers(this->VSTower).TwbBuffer1 =
                    this->TowerType + " \"" + this->Name + "\" - Inlet air wet-bulb temperature is outside model boundaries at " + OutputChar + '.';
                state.dataCondenserLoopTowers->towers(this->VSTower).TwbBuffer2 =
                    " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + General::CreateSysTimeIntervalString(state);
                TrimValue = format("{:.6R}", TwbCapped);
                state.dataCondenserLoopTowers->towers(this->VSTower).TwbBuffer3 =
                    " ...Inlet air wet-bulb temperature passed to the model = " + TrimValue;
                state.dataCondenserLoopTowers->towers(this->VSTower).TwbLast = Twb;
            } else {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintTwbMessage = false;
            }
        } else {
            state.dataCondenserLoopTowers->towers(this->VSTower).PrintTwbMessage = false;
        }

        if (Tr < state.dataCondenserLoopTowers->towers(this->VSTower).MinRangeTemp ||
            Tr > state.dataCondenserLoopTowers->towers(this->VSTower).MaxRangeTemp) {
            OutputChar = format("{:.2R}", Tr);
            OutputCharLo = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MinRangeTemp);
            OutputCharHi = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MaxRangeTemp);
            if (Tr < state.dataCondenserLoopTowers->towers(this->VSTower).MinRangeTemp) {
                TrCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MinRangeTemp;
            }
            if (Tr > state.dataCondenserLoopTowers->towers(this->VSTower).MaxRangeTemp) {
                TrCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MaxRangeTemp;
            }
            if (!state.dataGlobal->WarmupFlag) {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintTrMessage = true;
                state.dataCondenserLoopTowers->towers(this->VSTower).TrBuffer1 =
                    this->TowerType + " \"" + this->Name + "\" - Tower range temperature is outside model boundaries at " + OutputChar + '.';
                state.dataCondenserLoopTowers->towers(this->VSTower).TrBuffer2 =
                    " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + General::CreateSysTimeIntervalString(state);
                TrimValue = format("{:.5R}", Tr);
                state.dataCondenserLoopTowers->towers(this->VSTower).TrBuffer3 = " ...Tower range temperature passed to the model = " + TrimValue;
                state.dataCondenserLoopTowers->towers(this->VSTower).TrLast = Tr;
            } else {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintTrMessage = false;
            }
        } else {
            state.dataCondenserLoopTowers->towers(this->VSTower).PrintTrMessage = false;
        }

        if (Ta < state.dataCondenserLoopTowers->towers(this->VSTower).MinApproachTemp ||
            Ta > state.dataCondenserLoopTowers->towers(this->VSTower).MaxApproachTemp) {
            OutputChar = format("{:.2R}", Ta);
            OutputCharLo = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MinApproachTemp);
            OutputCharHi = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MaxApproachTemp);
            if (Ta < state.dataCondenserLoopTowers->towers(this->VSTower).MinApproachTemp) {
                TaCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MinApproachTemp;
            }
            if (Ta > state.dataCondenserLoopTowers->towers(this->VSTower).MaxApproachTemp) {
                TaCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MaxApproachTemp;
            }
            if (!state.dataGlobal->WarmupFlag) {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintTaMessage = true;
                state.dataCondenserLoopTowers->towers(this->VSTower).TaBuffer1 =
                    this->TowerType + " \"" + this->Name + "\" - Tower approach temperature is outside model boundaries at " + OutputChar + '.';
                state.dataCondenserLoopTowers->towers(this->VSTower).TaBuffer2 =
                    " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName + ", " +
                    state.dataEnvrn->CurMnDy + ' ' + General::CreateSysTimeIntervalString(state);
                TrimValue = format("{:.5R}", Ta);
                state.dataCondenserLoopTowers->towers(this->VSTower).TaBuffer3 = " ...Tower approach temperature passed to the model = " + TrimValue;
                state.dataCondenserLoopTowers->towers(this->VSTower).TaLast = Ta;
            } else {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintTaMessage = false;
            }
        } else {
            state.dataCondenserLoopTowers->towers(this->VSTower).PrintTaMessage = false;
        }

        if (this->TowerModelType == ModelType::YorkCalcModel || this->TowerModelType == ModelType::YorkCalcUserDefined) {
            //     Water flow rate ratio warning not valid for YorkCalc model, print liquid to gas ratio
            //     warning instead (bottom of Subroutine VariableSpeedTower)
            state.dataCondenserLoopTowers->towers(this->VSTower).PrintWFRRMessage = false;
        } else {
            if (WaterFlowRateRatio < state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio ||
                WaterFlowRateRatio > state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio) {
                OutputChar = format("{:.2R}", WaterFlowRateRatio);
                OutputCharLo = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio);
                OutputCharHi = format("{:.2R}", state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio);
                if (WaterFlowRateRatio < state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio) {
                    WaterFlowRateRatioCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MinWaterFlowRatio;
                }
                if (WaterFlowRateRatio > state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio) {
                    WaterFlowRateRatioCapped = state.dataCondenserLoopTowers->towers(this->VSTower).MaxWaterFlowRatio;
                }
                if (!state.dataGlobal->WarmupFlag) {
                    state.dataCondenserLoopTowers->towers(this->VSTower).PrintWFRRMessage = true;
                    state.dataCondenserLoopTowers->towers(this->VSTower).WFRRBuffer1 =
                        this->TowerType + " \"" + this->Name + "\" - Water flow rate ratio is outside model boundaries at " + OutputChar + '.';
                    state.dataCondenserLoopTowers->towers(this->VSTower).WFRRBuffer2 =
                        " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + state.dataEnvrn->EnvironmentName +
                        ", " + state.dataEnvrn->CurMnDy + ' ' + General::CreateSysTimeIntervalString(state);
                    TrimValue = format("{:.5R}", WaterFlowRateRatioCapped);
                    state.dataCondenserLoopTowers->towers(this->VSTower).WFRRBuffer3 = " ...Water flow rate ratio passed to the model = " + TrimValue;
                    state.dataCondenserLoopTowers->towers(this->VSTower).WaterFlowRateRatioLast = WaterFlowRateRatio;
                } else {
                    state.dataCondenserLoopTowers->towers(this->VSTower).PrintWFRRMessage = false;
                }
            } else {
                state.dataCondenserLoopTowers->towers(this->VSTower).PrintWFRRMessage = false;
            }
        }
    }

    Real64 CoolingTower::residualUA(EnergyPlusData &state,
                                    Real64 UA,                       // UA of cooling tower
                                    std::array<Real64, 6> const &Par // par(1) = design tower load [W]
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2002
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Design Tower Load - Tower Cooling Output) / Design Tower Load.
        // Tower Cooling Output depends on the UA which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Puts UA into the cooling tower data structure, calls calculateSimpleTowerOutletTemp, and calculates
        // the residual as defined above.

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = tower number
        // par(3) = design water mass flow rate [kg/s]
        // par(4) = design air volume flow rate [m3/s]
        // par(5) = water specific heat [J/(kg*C)]

        Real64 const OutWaterTemp = this->calculateSimpleTowerOutletTemp(state, Par[2], Par[3], UA);
        Real64 const CoolingOutput = Par[4] * Par[2] * (this->WaterTemp - OutWaterTemp); // tower cooling output [W]
        return (Par[0] - CoolingOutput) / Par[0];
    }

    Real64 CoolingTower::residualTr(EnergyPlusData &state,
                                    Real64 Trange,                   // cooling tower range temperature [C]
                                    std::array<Real64, 6> const &Par // par(1) = tower number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Feb 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (where residual shows a balance point of model and desired performance)
        // Tower Approach depends on the range temperature which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Varies tower range temperature until a balance point exists where the model output corresponds
        // to the desired independent variables

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = water flow ratio
        // par(3) = air flow ratio
        // par(4) = inlet air wet-bulb temperature [C]

        Real64 WaterFlowRateRatio = Par[1];    // ratio of water flow rate to design water flow rate
        Real64 AirFlowRateRatioLocal = Par[2]; // ratio of water flow rate to design water flow rate
        Real64 InletAirWB = Par[3];            // inlet air wet-bulb temperature [C]

        // call model to determine approach temperature given other independent variables (range temp is being varied to find balance)
        Real64 Tapproach = this->calculateVariableSpeedApproach(state, WaterFlowRateRatio, AirFlowRateRatioLocal, InletAirWB, Trange);
        // calculate residual based on a balance where Twb + Ta + Tr = Node(WaterInletNode)%Temp
        return (InletAirWB + Tapproach + Trange) - state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
    }

    Real64 CoolingTower::residualTa(EnergyPlusData &state,
                                    Real64 FlowRatio,                // water or air flow ratio of cooling tower
                                    std::array<Real64, 6> const &Par // par(1) = tower number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   Feb 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Desired Approach - Model Approach Output)
        // Tower Approach depends on the water (or air) flow rate ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // In SizeTower, calibrates tower water flow rate ratio at an air flow rate ratio of 1.
        // In VariableSpeedTower, calculates air flow rate ratio at the inlet water flow rate ratio.

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = water or air flow ratio (opposite of input variable)
        // par(3) = inlet air wet-bulb temp [C]
        // par(4) = tower range [C]
        // par(5) = desired approach [C]
        // par(6) = 0.0 to calculate water flow rate ratio, 1.0 for air

        Real64 AirFlowRateRatioLocal; // ratio of water flow rate to design water flow rate
        Real64 WaterFlowRateRatio;    // ratio of water flow rate to design water flow rate
        if (Par[5] == 0.0) {
            AirFlowRateRatioLocal = Par[1];
            WaterFlowRateRatio = FlowRatio;
        } else {
            AirFlowRateRatioLocal = FlowRatio;
            WaterFlowRateRatio = Par[1];
        }
        Real64 InletAirWB = Par[2];       // inlet air wet-bulb temperature [C]
        Real64 Trange = Par[3];           // tower range temperature [C]
        Real64 TapproachDesired = Par[4]; // desired tower approach temperature [C]
        // call model to determine tower approach temperature given other independent variables
        Real64 TapproachActual = this->calculateVariableSpeedApproach(state, WaterFlowRateRatio, AirFlowRateRatioLocal, InletAirWB, Trange);
        return TapproachDesired - TapproachActual;
    }

    void CoolingTower::calculateWaterUsage(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2006
        //       MODIFIED       T Hong, Aug. 2008. Added fluid bypass for single speed cooling tower
        //                      A Flament, July 2010. Added multi-cell capability
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Collect tower water useage calculations for
        // reuse by all the tower models.

        // REFERENCES:
        // Code for this routine started from VariableSpeedTower

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("calculateWaterUsage");

        Real64 EvapVdot(0.0);
        Real64 AverageWaterTemp = (this->InletWaterTemp + this->OutletWaterTemp) / 2.0;

        // Set water and air properties
        if (this->EvapLossMode == EvapLoss::MoistTheory) {

            Real64 const AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state, this->AirPress, this->AirTemp, this->AirHumRat);
            Real64 const AirMassFlowRate = this->airFlowRateRatio * this->HighSpeedAirFlowRate * AirDensity * this->NumCellOn / this->NumCell;
            Real64 const InletAirEnthalpy = Psychrometrics::PsyHFnTdbRhPb(state, this->AirWetBulb, 1.0, this->AirPress);

            if (AirMassFlowRate > 0.0) {
                // Calculate outlet air conditions for determining water usage

                Real64 const OutletAirEnthalpy = InletAirEnthalpy + this->Qactual / AirMassFlowRate;
                Real64 const OutletAirTSat = Psychrometrics::PsyTsatFnHPb(state, OutletAirEnthalpy, this->AirPress);
                Real64 const OutletAirHumRatSat = Psychrometrics::PsyWFnTdbH(state, OutletAirTSat, OutletAirEnthalpy);

                // calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
                Real64 const InSpecificHumRat = this->AirHumRat / (1 + this->AirHumRat);
                Real64 const OutSpecificHumRat = OutletAirHumRatSat / (1 + OutletAirHumRatSat);

                // calculate average air temp for density call
                Real64 const TairAvg = (this->AirTemp + OutletAirTSat) / 2.0;

                // Amount of water evaporated, get density water at air temp or 4 C if too cold
                Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                     max(TairAvg, 4.0),
                                                                     state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName);

                EvapVdot = (AirMassFlowRate * (OutSpecificHumRat - InSpecificHumRat)) / rho; // [m3/s]
                if (EvapVdot < 0.0) EvapVdot = 0.0;
            } else {
                EvapVdot = 0.0;
            }

        } else if (this->EvapLossMode == EvapLoss::UserFactor) {
            Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                 AverageWaterTemp,
                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                 RoutineName);

            EvapVdot = this->UserEvapLossFactor * (this->InletWaterTemp - this->OutletWaterTemp) * (this->WaterMassFlowRate / rho);
            if (EvapVdot < 0.0) EvapVdot = 0.0;
        } else {
            // should never come here
        }

        //   amount of water lost due to drift
        Real64 driftVdot = this->DesignWaterFlowRate * this->NumCellOn / this->NumCell * this->DriftLossFraction * this->airFlowRateRatio;

        Real64 BlowDownVdot(0.0);
        if (this->BlowdownMode == Blowdown::Schedule) {
            // Amount of water lost due to blow down (purging contaminants from tower basin)
            if (this->SchedIDBlowdown > 0) {
                BlowDownVdot = ScheduleManager::GetCurrentScheduleValue(state, this->SchedIDBlowdown);
            } else {
                BlowDownVdot = 0.0;
            }
        } else if (this->BlowdownMode == Blowdown::Concentration) {
            if (this->ConcentrationRatio > 2.0) { // protect divide by zero
                BlowDownVdot = EvapVdot / (this->ConcentrationRatio - 1) - driftVdot;
            } else {
                BlowDownVdot = EvapVdot - driftVdot;
            }
            if (BlowDownVdot < 0.0) BlowDownVdot = 0.0;
        } else {
            // should never come here
        }

        // Added for fluid bypass
        if (this->CapacityControl == CapacityCtrlEnum::FluidBypass) {
            if (this->EvapLossMode == EvapLoss::UserFactor) EvapVdot *= (1 - this->BypassFraction);
            driftVdot *= (1 - this->BypassFraction);
            BlowDownVdot *= (1 - this->BypassFraction);
        }

        Real64 const makeUpVdot = EvapVdot + driftVdot + BlowDownVdot;

        // set demand request in Water Storage if needed
        Real64 StarvedVdot = 0.0;
        Real64 tankSupplyVdot = 0.0;
        if (this->SuppliedByWaterSystem) {

            // set demand request
            state.dataWaterData->WaterStorage(this->WaterTankID).VdotRequestDemand(this->WaterTankDemandARRID) = makeUpVdot;

            Real64 const AvailTankVdot = state.dataWaterData->WaterStorage(this->WaterTankID)
                                             .VdotAvailDemand(this->WaterTankDemandARRID); // check what tank can currently provide

            tankSupplyVdot = makeUpVdot;      // init
            if (AvailTankVdot < makeUpVdot) { // calculate starved flow
                StarvedVdot = makeUpVdot - AvailTankVdot;
                tankSupplyVdot = AvailTankVdot;
            }
        } else { // supplied by mains
        }

        //   total water usage
        // update report variables
        this->EvaporationVdot = EvapVdot;
        this->EvaporationVol = EvapVdot * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        this->DriftVdot = driftVdot;
        this->DriftVol = driftVdot * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        this->BlowdownVdot = BlowDownVdot;
        this->BlowdownVol = BlowDownVdot * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        this->MakeUpVdot = makeUpVdot;
        this->MakeUpVol = makeUpVdot * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        this->TankSupplyVdot = tankSupplyVdot;
        this->TankSupplyVol = tankSupplyVdot * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        this->StarvedMakeUpVdot = StarvedVdot;
        this->StarvedMakeUpVol = StarvedVdot * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
    }

    void CoolingTower::update(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for passing results to the outlet water node.

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CharErrOut;
        std::string CharLowOutletTemp;

        // set node information
        state.dataLoopNodes->Node(this->WaterOutletNodeNum).Temp = this->OutletWaterTemp;

        if (state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked ||
            state.dataGlobal->WarmupFlag)
            return;

        // Check flow rate through tower and compare to design flow rate, show warning if greater than Design * Mulitplier
        if (state.dataLoopNodes->Node(this->WaterOutletNodeNum).MassFlowRate > this->DesWaterMassFlowRate * this->TowerMassFlowRateMultiplier) {
            ++this->HighMassFlowErrorCount;
            if (this->HighMassFlowErrorCount < 2) {
                ShowWarningError(state, this->TowerType + " \"" + this->Name + "\"");
                ShowContinueError(state, " Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate.");
                ShowContinueError(
                    state, format(" Condenser Loop Mass Flow Rate = {:.6T}", state.dataLoopNodes->Node(this->WaterOutletNodeNum).MassFlowRate));
                ShowContinueError(state, format(" Tower Design Mass Flow Rate   = {:.6T}", this->DesWaterMassFlowRate));
                ShowContinueErrorTimeStamp(state, "");
            } else {
                ShowRecurringWarningErrorAtEnd(
                    state,
                    this->TowerType + " \"" + this->Name +
                        "\"  Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate error continues...",
                    this->HighMassFlowErrorIndex,
                    state.dataLoopNodes->Node(this->WaterOutletNodeNum).MassFlowRate,
                    state.dataLoopNodes->Node(this->WaterOutletNodeNum).MassFlowRate);
            }
        }

        // Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
        Real64 const LoopMinTemp = state.dataPlnt->PlantLoop(this->LoopNum).MinTemp;
        bool const outletWaterTempTooLow = this->OutletWaterTemp < LoopMinTemp;
        bool const flowIsOn = this->WaterMassFlowRate > 0.0;
        if (outletWaterTempTooLow && flowIsOn) {
            ++this->OutletWaterTempErrorCount;
            strip(CharErrOut);
            if (this->OutletWaterTempErrorCount < 2) {
                ShowWarningError(state, this->TowerType + " \"" + this->Name + "\"");
                ShowContinueError(
                    state,
                    format("Cooling tower water outlet temperature ({:.2F} C) is below the specified minimum condenser loop temp of {:.2F} C",
                           this->OutletWaterTemp,
                           LoopMinTemp));
                ShowContinueErrorTimeStamp(state, "");
            } else {
                ShowRecurringWarningErrorAtEnd(
                    state,
                    this->TowerType + " \"" + this->Name +
                        "\" Cooling tower water outlet temperature is below the specified minimum condenser loop temp error continues...",
                    this->OutletWaterTempErrorIndex,
                    this->OutletWaterTemp,
                    this->OutletWaterTemp);
            }
        }

        // Check if water mass flow rate is small (e.g. no flow) and warn user
        if (this->WaterMassFlowRate > 0.0 && this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            ++this->SmallWaterMassFlowErrorCount;
            if (this->SmallWaterMassFlowErrorCount < 2) {
                ShowWarningError(state, this->TowerType + " \"" + this->Name + "\"");
                ShowContinueError(state, "Cooling tower water mass flow rate near zero.");
                ShowContinueErrorTimeStamp(state, "");
                ShowContinueError(state, format("Actual Mass flow = {:.2T}", this->WaterMassFlowRate));
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               this->TowerType + " \"" + this->Name +
                                                   "\"  Cooling tower water mass flow rate near zero error continues...",
                                               this->SmallWaterMassFlowErrorIndex,
                                               this->WaterMassFlowRate,
                                               this->WaterMassFlowRate);
            }
        }
    }

    void CoolingTower::report(EnergyPlusData &state, bool const RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the report variables for the tower.

        Real64 const ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        if (!RunFlag) {
            this->InletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->OutletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->Qactual = 0.0;
            this->FanPower = 0.0;
            this->FanEnergy = 0.0;
            this->AirFlowRatio = 0.0;
            this->WaterAmountUsed = 0.0;
            this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            this->FanCyclingRatio = 0.0;
            this->BypassFraction = 0.0; // added for fluid bypass
            this->NumCellOn = 0;
            this->SpeedSelected = 0;
        } else {
            this->InletWaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;
            this->FanEnergy = this->FanPower * ReportingConstant;
            this->AirFlowRatio = this->airFlowRateRatio;
            this->WaterAmountUsed = this->WaterUsage * ReportingConstant;
            this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
        }
    }

} // namespace CondenserLoopTowers

} // namespace EnergyPlus
