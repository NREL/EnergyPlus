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
#include <BranchNodeConnections.hh>
#include <CondenserLoopTowers.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <FaultsManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

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

    // Empirical Model Type
    int const CoolToolsXFModel(1);
    int const CoolToolsUserDefined(3);
    int const YorkCalcModel(4);
    int const YorkCalcUserDefined(5);

    int const EvapLossByUserFactor(80);
    int const EvapLossByMoistTheory(81);

    int const BlowdownByConcentration(90);
    int const BlowdownBySchedule(91);

    std::string const cCoolingTower_SingleSpeed("CoolingTower:SingleSpeed");
    std::string const cCoolingTower_TwoSpeed("CoolingTower:TwoSpeed");
    std::string const cCoolingTower_VariableSpeed("CoolingTower:VariableSpeed");
    std::string const cCoolingTower_VariableSpeedMerkel("CoolingTower:VariableSpeed:Merkel");

    int const PIM_NominalCapacity(1);
    int const PIM_UFactor(2);

    int const CoolingTower_SingleSpeed(1);
    int const CoolingTower_TwoSpeed(2);
    int const CoolingTower_VariableSpeed(3);
    int const CoolingTower_VariableSpeedMerkel(4);

    int const CapacityControl_FanCycling(1);
    int const CapacityControl_FluidBypass(2);

    int const CellCtrl_MinCell(1);
    int const CellCtrl_MaxCell(2);

    static std::string const BlankString;

    int NumSimpleTowers(0); // Number of similar towers
    bool GetInput(true);

    Array1D_bool CheckEquipName;

    // Object Data
    Array1D<Towerspecs> SimpleTower;           // dimension to number of machines
    std::unordered_map<std::string, std::string> UniqueSimpleTowerNames;

    // Functions
    void clear_state()
    {
        NumSimpleTowers = 0;
        GetInput = true;
        CheckEquipName.deallocate();
        SimpleTower.deallocate();
        UniqueSimpleTowerNames.clear();
    }

    void SimTowers(std::string const &TowerType,
                   std::string const &TowerName,
                   int &CompIndex,
                   bool &RunFlag,
                   bool const InitLoopEquip,
                   Real64 &MyLoad,
                   Real64 &MaxCap,
                   Real64 &MinCap,
                   Real64 &OptCap,
                   bool const GetSizingFactor, // TRUE when just the sizing factor is requested
                   Real64 &SizingFactor        // sizing factor
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Don Shirey
        //       DATE WRITTEN   Dec. 2000
        //       MODIFIED       Fred Buhl, May 2002; Richard Raustad, FSEC, Feb 2005 (added VS tower)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Main cooling tower driver subroutine.  Gets called from
        // PlantLoopEquipments.

        // METHODOLOGY EMPLOYED:
        // After being called by PlantLoopEquipments, this subroutine
        // calls GetTowerInput to get all cooling tower input info (one time only),
        // then calls the appropriate subroutine to calculate tower performance,
        // update records (node info) and writes output report info.

        int TowerNum;

        // GET INPUT
        if (GetInput) {
            GetTowerInput();
            GetInput = false;
        }

        // Find the correct CoolingTower
        if (CompIndex == 0) {
            TowerNum = UtilityRoutines::FindItemInList(TowerName, SimpleTower);
            if (TowerNum == 0) {
                ShowFatalError("SimTowers: Unit not found=" + TowerName);
            }
            CompIndex = TowerNum;
        } else {
            TowerNum = CompIndex;
            if (TowerNum > NumSimpleTowers || TowerNum < 1) {
                ShowFatalError("SimTowers:  Invalid CompIndex passed=" + General::TrimSigDigits(TowerNum) +
                               ", Number of Units=" + General::TrimSigDigits(NumSimpleTowers) + ", Entered Unit name=" + TowerName);
            }
            if (CheckEquipName(TowerNum)) {
                if (TowerName != SimpleTower(TowerNum).Name) {
                    ShowFatalError("SimTowers: Invalid CompIndex passed=" + General::TrimSigDigits(TowerNum) + ", Unit name=" + TowerName +
                                   ", stored Unit Name for that index=" + SimpleTower(TowerNum).Name);
                }
                CheckEquipName(TowerNum) = false;
            }
        }

        // CALCULATE
        {
            auto const SELECT_CASE_var(SimpleTower(TowerNum).TowerType_Num);

            if (SELECT_CASE_var == CoolingTower_SingleSpeed) {

                if (InitLoopEquip) {
                    InitTower(TowerNum, RunFlag);
                    SizeTower(TowerNum);
                    MinCap = 0.0;
                    MaxCap = SimpleTower(TowerNum).TowerNominalCapacity * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    OptCap = SimpleTower(TowerNum).TowerNominalCapacity;
                    if (GetSizingFactor) {
                        SizingFactor = SimpleTower(TowerNum).SizFac;
                    }
                    return;
                }
                InitTower(TowerNum, RunFlag);
                CalcSingleSpeedTower(TowerNum);
                CalculateWaterUseage(TowerNum);
                UpdateTowers(TowerNum);
                ReportTowers(RunFlag, TowerNum);

            } else if (SELECT_CASE_var == CoolingTower_TwoSpeed) {

                if (InitLoopEquip) {
                    InitTower(TowerNum, RunFlag);
                    SizeTower(TowerNum);
                    MinCap = 0.0;
                    MaxCap = SimpleTower(TowerNum).TowerNominalCapacity * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    OptCap = SimpleTower(TowerNum).TowerNominalCapacity;
                    if (GetSizingFactor) {
                        SizingFactor = SimpleTower(TowerNum).SizFac;
                    }
                    return;
                }
                InitTower(TowerNum, RunFlag);
                CalcTwoSpeedTower(TowerNum);
                CalculateWaterUseage(TowerNum);
                UpdateTowers(TowerNum);
                ReportTowers(RunFlag, TowerNum);

            } else if (SELECT_CASE_var == CoolingTower_VariableSpeedMerkel) {

                if (InitLoopEquip) {
                    InitTower(TowerNum, RunFlag);
                    SizeVSMerkelTower(TowerNum);
                    MinCap = 0.0;
                    MaxCap = SimpleTower(TowerNum).TowerNominalCapacity * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    OptCap = SimpleTower(TowerNum).TowerNominalCapacity;
                    if (GetSizingFactor) {
                        SizingFactor = SimpleTower(TowerNum).SizFac;
                    }
                    return;
                }
                InitTower(TowerNum, RunFlag);
                CalcMerkelVariableSpeedTower(TowerNum, MyLoad);
                CalculateWaterUseage(TowerNum);
                UpdateTowers(TowerNum);
                ReportTowers(RunFlag, TowerNum);

            } else if (SELECT_CASE_var == CoolingTower_VariableSpeed) {

                if (InitLoopEquip) {
                    InitTower(TowerNum, RunFlag);
                    SizeTower(TowerNum);
                    MinCap = 0.0;
                    MaxCap = SimpleTower(TowerNum).TowerNominalCapacity * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    OptCap = SimpleTower(TowerNum).TowerNominalCapacity;
                    if (GetSizingFactor) {
                        SizingFactor = SimpleTower(TowerNum).SizFac;
                    }
                    return;
                }
                InitTower(TowerNum, RunFlag);
                CalcVariableSpeedTower(TowerNum);
                CalculateWaterUseage(TowerNum);
                UpdateTowers(TowerNum);
                ReportTowers(RunFlag, TowerNum);

            } else {
                ShowFatalError("SimTowers: Invalid Tower Type Requested=" + TowerType);
            }
        } // TypeOfEquip
    }

    void GetTowerInput()
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
        // Obtains input data for cooling towers and stores it in SimpleTower data structure. Additional structure
        // (VSTower) stores the coefficients for each VS tower.

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in the data.

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt OutputFormat("(F5.2)");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TowerNum;                   // Tower number, reference counter for SimpleTower data array
        int NumSingleSpeedTowers;       // Total number of single-speed cooling towers
        int SingleSpeedTowerNumber;     // Specific single-speed tower of interest
        int NumTwoSpeedTowers;          // Number of two-speed cooling towers
        int TwoSpeedTowerNumber;        // Specific two-speed tower of interest
        int NumVariableSpeedTowers;     // Number of variable-speed cooling towers
        int VariableSpeedTowerNumber;   // Specific variable-speed tower of interest
        int NumVSCoolToolsModelCoeffs = 0;  // Number of CoolTools VS cooling tower coefficient objects
        int NumVSYorkCalcModelCoeffs = 0;   // Number of YorkCalc VS cooling tower coefficient objects
        int NumVSMerkelTowers;          // Number of Merkel variable speed cooling towers
        int MerkelVSTowerNum;           // specific merkel variable speed tower of interest
        int VSModelCoeffNum;            // Specific variable-speed tower coefficient object of interest
        int NumAlphas;                  // Number of elements in the alpha array
        int NumNums;                    // Number of elements in the numeric array
        int NumAlphas2;                 // Number of elements in the alpha2 array
        int NumNums2;                   // Number of elements in the numeric2 array
        int IOStat;                     // IO Status when calling get input subroutine
        int CoeffNum;                   // Index for reading user defined VS tower coefficients
        bool ErrorsFound(false); // Logical flag set .TRUE. if errors found while getting input data
        std::string OutputChar;         // report variable for warning messages
        std::string OutputCharLo;       // report variable for warning messages
        std::string OutputCharHi;       // report variable for warning messages
        Array1D<Real64> NumArray(33);   // Numeric input data array
        Array1D<Real64> NumArray2(43);  // Numeric input data array for VS tower coefficients
        Array1D_string AlphArray(16);   // Character string input data array
        Array1D_string AlphArray2(1);   // Character string input data array for VS tower coefficients

        // Get number of all cooling towers specified in the input data file (idf)
        NumSingleSpeedTowers = inputProcessor->getNumObjectsFound(cCoolingTower_SingleSpeed);
        NumTwoSpeedTowers = inputProcessor->getNumObjectsFound(cCoolingTower_TwoSpeed);
        NumVariableSpeedTowers = inputProcessor->getNumObjectsFound(cCoolingTower_VariableSpeed);
        NumVSMerkelTowers = inputProcessor->getNumObjectsFound(cCoolingTower_VariableSpeedMerkel);
        NumSimpleTowers = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers;

        if (NumSimpleTowers <= 0)
            ShowFatalError("No Cooling Tower objects found in input, however, a branch object has specified a cooling tower. Search the input for "
                           "CoolingTower to determine the cause for this error.");

        GetInput = false;
        // See if load distribution manager has already gotten the input
        if (allocated(SimpleTower)) return;

        // Allocate data structures to hold tower input data, report data and tower inlet conditions
        SimpleTower.allocate(NumSimpleTowers);
        UniqueSimpleTowerNames.reserve(NumSimpleTowers);
        CheckEquipName.dimension(NumSimpleTowers, true);
        // Allocate variable-speed tower structure with data specific to this type
        if (NumVariableSpeedTowers > 0) {
            // Allow users to input model coefficients other than default
            NumVSCoolToolsModelCoeffs = inputProcessor->getNumObjectsFound("CoolingTowerPerformance:CoolTools");
            NumVSYorkCalcModelCoeffs = inputProcessor->getNumObjectsFound("CoolingTowerPerformance:YorkCalc");
        }

        // Load data structures with cooling tower input data
        cCurrentModuleObject = cCoolingTower_SingleSpeed;
        for (SingleSpeedTowerNumber = 1; SingleSpeedTowerNumber <= NumSingleSpeedTowers; ++SingleSpeedTowerNumber) {
            TowerNum = SingleSpeedTowerNumber;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          SingleSpeedTowerNumber,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueSimpleTowerNames, AlphArray(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            SimpleTower(TowerNum).Name = AlphArray(1);
            SimpleTower(TowerNum).TowerType = cCurrentModuleObject;
            SimpleTower(TowerNum).TowerType_Num = CoolingTower_SingleSpeed;
            SimpleTower(TowerNum).TowerMassFlowRateMultiplier = 2.5;
            SimpleTower(TowerNum).WaterInletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(2), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            SimpleTower(TowerNum).WaterOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(3), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");
            SimpleTower(TowerNum).DesignWaterFlowRate = NumArray(1);
            if (SimpleTower(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).HighSpeedAirFlowRate = NumArray(2);
            if (SimpleTower(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).HighSpeedFanPower = NumArray(3);
            if (SimpleTower(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            SimpleTower(TowerNum).HighSpeedTowerUA = NumArray(4);
            if (SimpleTower(TowerNum).HighSpeedTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvAirFlowRate = NumArray(5);
            if (SimpleTower(TowerNum).FreeConvAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor = NumArray(6);
            SimpleTower(TowerNum).FreeConvTowerUA = NumArray(7);
            if (SimpleTower(TowerNum).FreeConvTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).FreeConvTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvTowerUASizingFactor = NumArray(8);
            SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio = NumArray(9);
            SimpleTower(TowerNum).TowerNominalCapacity = NumArray(10);
            if (SimpleTower(TowerNum).TowerNominalCapacity == DataSizing::AutoSize) {
                SimpleTower(TowerNum).TowerNominalCapacityWasAutoSized = true;
            }
            SimpleTower(TowerNum).TowerFreeConvNomCap = NumArray(11);
            if (SimpleTower(TowerNum).TowerFreeConvNomCap == DataSizing::AutoSize) {
                SimpleTower(TowerNum).TowerFreeConvNomCapWasAutoSized = true;
            }
            SimpleTower(TowerNum).TowerFreeConvNomCapSizingFactor = NumArray(12);
            if (NumAlphas >= 4) {
                if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
                    SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_UFactor;
                } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
                    SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_NominalCapacity;
                } else {
                    ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Invalid, " + cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ErrorsFound = true;
                }
            } else {
                // Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
                SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_UFactor;
            }
            // cooling tower design inlet conditions
            SimpleTower(TowerNum).DesInletAirDBTemp = NumArray(13);
            if (SimpleTower(TowerNum).DesInletAirDBTemp == 0) {
                SimpleTower(TowerNum).DesInletAirDBTemp = 35.0;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesInletAirWBTemp = NumArray(14);
            if (SimpleTower(TowerNum).DesInletAirWBTemp == 0) {
                SimpleTower(TowerNum).DesInletAirWBTemp = 25.6;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesApproach = NumArray(15);
            if (SimpleTower(TowerNum).DesApproach == DataSizing::AutoSize || SimpleTower(TowerNum).DesApproach == 0) {
                SimpleTower(TowerNum).DesApproach = 3.9;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesRange = NumArray(16);
            if (SimpleTower(TowerNum).DesRange == DataSizing::AutoSize || SimpleTower(TowerNum).DesRange == 0) {
                SimpleTower(TowerNum).DesRange = 5.5;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            // set tower design water outlet and inlet temperatures
            SimpleTower(TowerNum).DesOutletWaterTemp = SimpleTower(TowerNum).DesInletAirWBTemp + SimpleTower(TowerNum).DesApproach;
            SimpleTower(TowerNum).DesInletWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp + SimpleTower(TowerNum).DesRange;
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            SimpleTower(TowerNum).BasinHeaterPowerFTempDiff = NumArray(17);
            if (NumArray(17) < 0.0) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).BasinHeaterSetPointTemp = NumArray(18);

            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 18) {
                    SimpleTower(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (SimpleTower(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(cCurrentModuleObject + ":\"" + SimpleTower(TowerNum).Name + "\", " + cNumericFieldNames(18) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!AlphArray(5).empty()) {
                SimpleTower(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(AlphArray(5));
                if (SimpleTower(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" basin heater schedule name \"" + AlphArray(5) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(6), "LossFactor")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByUserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(6), "SaturatedExit")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else if (AlphArray(6).empty()) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid, " + cAlphaFieldNames(6) + " = " + AlphArray(6));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).UserEvapLossFactor = NumArray(19);        //  N11 , \field Evaporation Loss Factor
            SimpleTower(TowerNum).DriftLossFraction = NumArray(20) / 100.0; //  N12, \field Drift Loss Percent

            if ((NumNums < 20) && (SimpleTower(TowerNum).DriftLossFraction == 0.0)) {
                // assume Drift loss not entered and should be defaulted
                SimpleTower(TowerNum).DriftLossFraction = 0.008 / 100.0;
            }

            SimpleTower(TowerNum).ConcentrationRatio = NumArray(21); //  N13, \field Blowdown Concentration Ratio
            SimpleTower(TowerNum).SizFac = NumArray(25);             //  N17  \field Sizing Factor
            if (SimpleTower(TowerNum).SizFac <= 0.0) SimpleTower(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(7), "ScheduledRate")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownBySchedule;
            } else if (UtilityRoutines::SameString(AlphArray(7), "ConcentrationRatio")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
            } else if (AlphArray(7).empty()) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
                if ((NumNums < 21) && (SimpleTower(TowerNum).ConcentrationRatio == 0.0)) {
                    // assume Concetratino ratio was omitted and should be defaulted
                    SimpleTower(TowerNum).ConcentrationRatio = 3.0;
                }
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid, " + cAlphaFieldNames(7) + " = " + AlphArray(7));
                ErrorsFound = true;
            }
            SimpleTower(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(AlphArray(8));
            if ((SimpleTower(TowerNum).SchedIDBlowdown == 0) && (SimpleTower(TowerNum).BlowdownMode == BlowdownBySchedule)) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid, " + cAlphaFieldNames(8) + " = " + AlphArray(8));
                ErrorsFound = true;
            }

            if (AlphArray(9).empty()) {
                SimpleTower(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(AlphArray(1),
                                         cCurrentModuleObject,
                                         AlphArray(9),
                                         ErrorsFound,
                                         SimpleTower(TowerNum).WaterTankID,
                                         SimpleTower(TowerNum).WaterTankDemandARRID);
                SimpleTower(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node

            if (lAlphaFieldBlanks(10)) {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(10),
                                                                                 ErrorsFound,
                                                                                 cCurrentModuleObject,
                                                                                 SimpleTower(TowerNum).Name,
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(SimpleTower(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                    "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(10));
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            //   fluid bypass for single speed tower
            if (lAlphaFieldBlanks(11) || AlphArray(11).empty()) {
                SimpleTower(TowerNum).CapacityControl = CapacityControl_FanCycling; // FanCycling
            } else {
                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(AlphArray(11)));
                    if (SELECT_CASE_var == "FANCYCLING") {
                        SimpleTower(TowerNum).CapacityControl = CapacityControl_FanCycling;
                    } else if (SELECT_CASE_var == "FLUIDBYPASS") {
                        SimpleTower(TowerNum).CapacityControl = CapacityControl_FluidBypass;
                    } else {
                        SimpleTower(TowerNum).CapacityControl = CapacityControl_FanCycling;
                        ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                         "\" The Capacity Control is not specified correctly. The default Fan Cycling is used.");
                    }
                }
            }

            // added for multi-cell
            SimpleTower(TowerNum).NumCell = NumArray(22);
            if ((NumNums < 22) && (SimpleTower(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                SimpleTower(TowerNum).NumCell = 1;
            }
            SimpleTower(TowerNum).MinFracFlowRate = NumArray(23);
            if ((NumNums < 23) && (SimpleTower(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MinFracFlowRate = 0.33;
            }
            SimpleTower(TowerNum).MaxFracFlowRate = NumArray(24);
            if ((NumNums < 24) && (SimpleTower(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MaxFracFlowRate = 2.5;
            }

            if (NumAlphas >= 12) {
                if (lAlphaFieldBlanks(12) || AlphArray(12).empty()) {
                    SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(12), "MinimalCell") || UtilityRoutines::SameString(AlphArray(12), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(12), "MinimalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(12), "MaximalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                        }
                    } else {
                        ShowSevereError("Illegal " + cAlphaFieldNames(12) + " = " + AlphArray(12));
                        ShowContinueError("Occurs in " + SimpleTower(TowerNum).TowerType + '=' + SimpleTower(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
            }

            //   High speed air flow rate must be greater than free convection air flow rate.
            //   Can't tell yet if autosized, check later in InitTower.
            if (SimpleTower(TowerNum).HighSpeedAirFlowRate <= SimpleTower(TowerNum).FreeConvAirFlowRate &&
                SimpleTower(TowerNum).HighSpeedAirFlowRate != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". Free convection air flow rate must be less than the design air flow rate.");
                ErrorsFound = true;
            }

            //   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
            if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_UFactor) {
                if (SimpleTower(TowerNum).DesignWaterFlowRate == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower performance input method requires a design water flow rate greater than zero.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).HighSpeedTowerUA <= SimpleTower(TowerNum).FreeConvTowerUA &&
                    SimpleTower(TowerNum).HighSpeedTowerUA != DataSizing::AutoSize) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Free convection UA must be less than the design tower UA.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).FreeConvTowerUA > 0.0 && SimpleTower(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Free convection air flow rate must be greater than zero when free convection UA is greater than zero.");
                    ErrorsFound = true;
                }
            } else if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
                if (SimpleTower(TowerNum).TowerNominalCapacity == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower performance input method requires valid nominal capacity.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).DesignWaterFlowRate != 0.0) {
                    if (SimpleTower(TowerNum).DesignWaterFlowRate > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal capacity input method and design water flow rate have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal capacity input method has been specified and design water flow rate is being autosized.");
                    }
                    ShowContinueError("Design water flow rate will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).HighSpeedTowerUA != 0.0) {
                    if (SimpleTower(TowerNum).HighSpeedTowerUA > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal tower capacity and design tower UA have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal tower capacity has been specified and design tower UA is being autosized.");
                    }
                    ShowContinueError("Design tower UA will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).FreeConvTowerUA != 0.0) {
                    if (SimpleTower(TowerNum).FreeConvTowerUA > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal capacity input method and free convection UA have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal capacity input method has been specified and free convection UA is being autosized.");
                    }
                    ShowContinueError("Free convection UA will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).TowerFreeConvNomCap >= SimpleTower(TowerNum).TowerNominalCapacity) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Free convection nominal capacity must be less than the nominal (design) tower capacity.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).TowerFreeConvNomCap > 0.0 && SimpleTower(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Free convection air flow must be greater than zero when tower free convection capacity is specified.");
                    ErrorsFound = true;
                }
            } else { // Tower performance input method is not specified as a valid "choice"
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                R"(". Tower Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
                ShowContinueError("Tower Performanace Input Method currently specified as: " + AlphArray(4));
                ErrorsFound = true;
            }
            if (NumAlphas > 12) {
                SimpleTower(TowerNum).EndUseSubcategory = AlphArray(13);
            } else {
                SimpleTower(TowerNum).EndUseSubcategory = "General";
            }
        } // End Single-Speed Tower Loop

        cCurrentModuleObject = cCoolingTower_TwoSpeed;
        for (TwoSpeedTowerNumber = 1; TwoSpeedTowerNumber <= NumTwoSpeedTowers; ++TwoSpeedTowerNumber) {
            TowerNum = NumSingleSpeedTowers + TwoSpeedTowerNumber;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          TwoSpeedTowerNumber,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueSimpleTowerNames, AlphArray(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            SimpleTower(TowerNum).Name = AlphArray(1);
            SimpleTower(TowerNum).TowerType = cCurrentModuleObject;
            SimpleTower(TowerNum).TowerType_Num = CoolingTower_TwoSpeed;
            SimpleTower(TowerNum).TowerMassFlowRateMultiplier = 2.5;
            SimpleTower(TowerNum).WaterInletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(2), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            SimpleTower(TowerNum).WaterOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(3), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            if (NumAlphas >= 4) {
                if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
                    SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_UFactor;
                } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
                    SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_NominalCapacity;
                } else {
                    ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Invalid, " + cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ErrorsFound = true;
                }
            } else {
                // Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
                SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_UFactor;
            }
            SimpleTower(TowerNum).DesignWaterFlowRate = NumArray(1);
            if (SimpleTower(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).HighSpeedAirFlowRate = NumArray(2);
            if (SimpleTower(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).HighSpeedFanPower = NumArray(3);
            if (SimpleTower(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            SimpleTower(TowerNum).HighSpeedTowerUA = NumArray(4);
            if (SimpleTower(TowerNum).HighSpeedTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).LowSpeedAirFlowRate = NumArray(5);
            if (SimpleTower(TowerNum).LowSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).LowSpeedAirFlowRateWasAutoSized = true;
            }

            SimpleTower(TowerNum).LowSpeedAirFlowRateSizingFactor = NumArray(6);
            SimpleTower(TowerNum).LowSpeedFanPower = NumArray(7);
            if (SimpleTower(TowerNum).LowSpeedFanPower == DataSizing::AutoSize) {
                SimpleTower(TowerNum).LowSpeedFanPowerWasAutoSized = true;
            }
            SimpleTower(TowerNum).LowSpeedFanPowerSizingFactor = NumArray(8);
            SimpleTower(TowerNum).LowSpeedTowerUA = NumArray(9);
            if (SimpleTower(TowerNum).LowSpeedTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).LowSpeedTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).LowSpeedTowerUASizingFactor = NumArray(10);
            SimpleTower(TowerNum).FreeConvAirFlowRate = NumArray(11);
            if (SimpleTower(TowerNum).FreeConvAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor = NumArray(12);
            SimpleTower(TowerNum).FreeConvTowerUA = NumArray(13);
            if (SimpleTower(TowerNum).FreeConvTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).FreeConvTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvTowerUASizingFactor = NumArray(14);
            SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio = NumArray(15);
            SimpleTower(TowerNum).TowerNominalCapacity = NumArray(16);

            SimpleTower(TowerNum).TowerLowSpeedNomCap = NumArray(17);
            if (SimpleTower(TowerNum).TowerLowSpeedNomCap == DataSizing::AutoSize) {
                SimpleTower(TowerNum).TowerLowSpeedNomCapWasAutoSized = true;
            }
            SimpleTower(TowerNum).TowerLowSpeedNomCapSizingFactor = NumArray(18);
            SimpleTower(TowerNum).TowerFreeConvNomCap = NumArray(19);
            if (SimpleTower(TowerNum).TowerFreeConvNomCap == DataSizing::AutoSize) {
                SimpleTower(TowerNum).TowerFreeConvNomCapWasAutoSized = true;
            }
            SimpleTower(TowerNum).TowerFreeConvNomCapSizingFactor = NumArray(20);
            // cooling tower design inlet conditions
            SimpleTower(TowerNum).DesInletAirDBTemp = NumArray(21);
            if (SimpleTower(TowerNum).DesInletAirDBTemp == 0) {
                SimpleTower(TowerNum).DesInletAirDBTemp = 35.0;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesInletAirWBTemp = NumArray(22);
            if (SimpleTower(TowerNum).DesInletAirWBTemp == 0) {
                SimpleTower(TowerNum).DesInletAirWBTemp = 25.6;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesApproach = NumArray(23);
            if (SimpleTower(TowerNum).DesApproach == DataSizing::AutoSize || SimpleTower(TowerNum).DesApproach == 0) {
                SimpleTower(TowerNum).DesApproach = 3.9;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesRange = NumArray(24);
            if (SimpleTower(TowerNum).DesRange == DataSizing::AutoSize || SimpleTower(TowerNum).DesRange == 0) {
                SimpleTower(TowerNum).DesRange = 5.5;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            // set tower design water outlet and inlet temperatures
            SimpleTower(TowerNum).DesOutletWaterTemp = SimpleTower(TowerNum).DesInletAirWBTemp + SimpleTower(TowerNum).DesApproach;
            SimpleTower(TowerNum).DesInletWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp + SimpleTower(TowerNum).DesRange;
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            SimpleTower(TowerNum).BasinHeaterPowerFTempDiff = NumArray(25);
            if (NumArray(25) < 0.0) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).BasinHeaterSetPointTemp = NumArray(26);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 26) {
                    SimpleTower(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (SimpleTower(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(cCurrentModuleObject + ":\"" + SimpleTower(TowerNum).Name + "\", " + cNumericFieldNames(26) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!AlphArray(5).empty()) {
                SimpleTower(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(AlphArray(5));
                if (SimpleTower(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" basin heater schedule name \"" + AlphArray(5) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(6), "LossFactor")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByUserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(6), "SaturatedExit")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else if (lAlphaFieldBlanks(6)) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(6) + '=' + AlphArray(6));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).UserEvapLossFactor = NumArray(27);        //  N23 , \field Evaporation Loss Factor
            SimpleTower(TowerNum).DriftLossFraction = NumArray(28) / 100.0; //  N24, \field Drift Loss Percent
            if ((NumNums < 28) && (SimpleTower(TowerNum).DriftLossFraction == 0.0)) {
                // assume Drift loss not entered and should be defaulted
                SimpleTower(TowerNum).DriftLossFraction = 0.008 / 100.0;
            }

            SimpleTower(TowerNum).ConcentrationRatio = NumArray(29); //  N17, \field Blowdown Concentration Ratio
            SimpleTower(TowerNum).SizFac = NumArray(33);             //  N21  \field Sizing Factor
            if (SimpleTower(TowerNum).SizFac <= 0.0) SimpleTower(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(7), "ScheduledRate")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownBySchedule;
            } else if (UtilityRoutines::SameString(AlphArray(7), "ConcentrationRatio")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
            } else if (lAlphaFieldBlanks(7)) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
                if ((NumNums < 29) && (SimpleTower(TowerNum).ConcentrationRatio == 0.0)) {
                    // assume Concetration ratio was omitted and should be defaulted
                    SimpleTower(TowerNum).ConcentrationRatio = 3.0;
                }
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(7) + '=' + AlphArray(7));
                ErrorsFound = true;
            }
            SimpleTower(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(AlphArray(8));
            if ((SimpleTower(TowerNum).SchedIDBlowdown == 0) && (SimpleTower(TowerNum).BlowdownMode == BlowdownBySchedule)) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(8) + '=' + AlphArray(8));
                ErrorsFound = true;
            }

            // added for multi-cell
            SimpleTower(TowerNum).NumCell = NumArray(30);
            if ((NumNums < 30) && (SimpleTower(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                SimpleTower(TowerNum).NumCell = 1;
            }
            SimpleTower(TowerNum).MinFracFlowRate = NumArray(31);
            if ((NumNums < 31) && (SimpleTower(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MinFracFlowRate = 0.33;
            }
            SimpleTower(TowerNum).MaxFracFlowRate = NumArray(32);
            if ((NumNums < 32) && (SimpleTower(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MaxFracFlowRate = 2.5;
            }

            if (NumAlphas >= 11) {
                if (lAlphaFieldBlanks(11) || AlphArray(11).empty()) {
                    SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(11), "MinimalCell") || UtilityRoutines::SameString(AlphArray(11), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(11), "MinimalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(11), "MaximalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                        }
                    } else {
                        ShowSevereError("Illegal " + cAlphaFieldNames(12) + " = " + AlphArray(12));
                        ShowContinueError("Occurs in " + SimpleTower(TowerNum).TowerType + '=' + SimpleTower(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
            }

            if (lAlphaFieldBlanks(9)) {
                SimpleTower(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(AlphArray(1),
                                         cCurrentModuleObject,
                                         AlphArray(9),
                                         ErrorsFound,
                                         SimpleTower(TowerNum).WaterTankID,
                                         SimpleTower(TowerNum).WaterTankDemandARRID);
                SimpleTower(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node
            if (lAlphaFieldBlanks(10)) {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(10),
                                                                                 ErrorsFound,
                                                                                 cCurrentModuleObject,
                                                                                 SimpleTower(TowerNum).Name,
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(SimpleTower(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                    "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(10));
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            //   High speed air flow rate must be greater than low speed air flow rate.
            //   Can't tell yet if autosized, check later in InitTower.
            if (SimpleTower(TowerNum).HighSpeedAirFlowRate <= SimpleTower(TowerNum).LowSpeedAirFlowRate &&
                SimpleTower(TowerNum).HighSpeedAirFlowRate != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". Low speed air flow rate must be less than the high speed air flow rate.");
                ErrorsFound = true;
            }
            //   Low speed air flow rate must be greater than free convection air flow rate.
            //   Can't tell yet if autosized, check later in InitTower.
            if (SimpleTower(TowerNum).LowSpeedAirFlowRate <= SimpleTower(TowerNum).FreeConvAirFlowRate &&
                SimpleTower(TowerNum).LowSpeedAirFlowRate != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". Free convection air flow rate must be less than the low speed air flow rate.");
                ErrorsFound = true;
            }

            //   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
            if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_UFactor) {
                if (SimpleTower(TowerNum).DesignWaterFlowRate == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower performance input method requires a design water flow rate greater than zero.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).HighSpeedTowerUA <= SimpleTower(TowerNum).LowSpeedTowerUA &&
                    SimpleTower(TowerNum).HighSpeedTowerUA != DataSizing::AutoSize) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower UA at low fan speed must be less than the tower UA at high fan speed.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).LowSpeedTowerUA <= SimpleTower(TowerNum).FreeConvTowerUA &&
                    SimpleTower(TowerNum).LowSpeedTowerUA != DataSizing::AutoSize) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).FreeConvTowerUA > 0.0 && SimpleTower(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Free convection air flow rate must be greater than zero when free convection UA is greater than zero.");
                    ErrorsFound = true;
                }
            } else if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
                if (SimpleTower(TowerNum).TowerNominalCapacity == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower performance input method requires valid high-speed nominal capacity.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).TowerLowSpeedNomCap == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Tower performance input method requires valid low-speed nominal capacity.");
                    ErrorsFound = true;
                }
                if (SimpleTower(TowerNum).DesignWaterFlowRate != 0.0) {
                    if (SimpleTower(TowerNum).DesignWaterFlowRate > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal capacity input method and design water flow rate have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal capacity input method has been specified and design water flow rate is being autosized.");
                    }
                    ShowContinueError("Design water flow rate will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).HighSpeedTowerUA != 0.0) {
                    if (SimpleTower(TowerNum).HighSpeedTowerUA > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal capacity input method and tower UA at high fan speed have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal capacity input method has been specified and tower UA at high fan speed is being autosized.");
                    }
                    ShowContinueError("Tower UA at high fan speed will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).LowSpeedTowerUA != 0.0) {
                    if (SimpleTower(TowerNum).LowSpeedTowerUA > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal capacity input method and tower UA at low fan speed have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal capacity input method has been specified and tower UA at low fan speed is being autosized.");
                    }
                    ShowContinueError("Tower UA at low fan speed will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).FreeConvTowerUA != 0.0) {
                    if (SimpleTower(TowerNum).FreeConvTowerUA > 0.0) {
                        ShowWarningError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                         "\". Nominal capacity input method and free convection UA have been specified.");
                    } else {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Nominal capacity input method has been specified and free convection UA is being autosized.");
                    }
                    ShowContinueError("Free convection UA will be set according to nominal tower capacity.");
                }
                if (SimpleTower(TowerNum).TowerLowSpeedNomCap >= SimpleTower(TowerNum).TowerNominalCapacity) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Low-speed nominal capacity must be less than the high-speed nominal capacity.");
                    ErrorsFound = true;
                }
                if (!SimpleTower(TowerNum).TowerLowSpeedNomCapWasAutoSized) {
                    if (SimpleTower(TowerNum).TowerFreeConvNomCap >= SimpleTower(TowerNum).TowerLowSpeedNomCap) {
                        ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Free convection nominal capacity must be less than the low-speed nominal capacity.");
                        ErrorsFound = true;
                    }
                }
                if (SimpleTower(TowerNum).TowerFreeConvNomCap > 0.0 && SimpleTower(TowerNum).FreeConvAirFlowRate == 0.0) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". Free convection air flow must be greater than zero when tower free convection capacity is specified.");
                    ErrorsFound = true;
                }
            } else { // Tower performance input method is not specified as a valid "choice"
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                R"(". Tower Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
                ShowContinueError("Tower Performanace Input Method currently specified as: " + AlphArray(4));
                ErrorsFound = true;
            }
            if (NumAlphas > 11) {
                SimpleTower(TowerNum).EndUseSubcategory = AlphArray(12);
            } else {
                SimpleTower(TowerNum).EndUseSubcategory = "General";
            }
        } // End Two-Speed Tower Loop

        cCurrentModuleObject = cCoolingTower_VariableSpeed;
        for (VariableSpeedTowerNumber = 1; VariableSpeedTowerNumber <= NumVariableSpeedTowers; ++VariableSpeedTowerNumber) {
            TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + VariableSpeedTowerNumber;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          VariableSpeedTowerNumber,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueSimpleTowerNames, AlphArray(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            SimpleTower(TowerNum).VSTower = VariableSpeedTowerNumber;
            SimpleTower(TowerNum).Name = AlphArray(1);
            SimpleTower(TowerNum).TowerType = cCurrentModuleObject;
            SimpleTower(TowerNum).TowerType_Num = CoolingTower_VariableSpeed;
            SimpleTower(TowerNum).WaterInletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(2), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            SimpleTower(TowerNum).WaterOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(3), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            if ((UtilityRoutines::SameString(AlphArray(4), "CoolToolsUserDefined") ||
                 UtilityRoutines::SameString(AlphArray(4), "YorkCalcUserDefined")) &&
                lAlphaFieldBlanks(5)) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" a " + cAlphaFieldNames(5) +
                                " must be specified when " + cAlphaFieldNames(4) + " is specified as CoolToolsUserDefined or YorkCalcUserDefined");
                ErrorsFound = true;
            } else if ((UtilityRoutines::SameString(AlphArray(4), "CoolToolsCrossFlow") || UtilityRoutines::SameString(AlphArray(4), "YorkCalc")) &&
                       !lAlphaFieldBlanks(5)) {
                ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                 "\" a Tower Model Coefficient Name is specified and the Tower Model Type is not specified as CoolToolsUserDefined "
                                 "or YorkCalcUserDefined. The CoolingTowerPerformance:CoolTools (orCoolingTowerPerformance:YorkCalc) data object "
                                 "will not be used.");
            } else {
                SimpleTower(TowerNum).ModelCoeffObjectName = AlphArray(5);
            }

            if (!lAlphaFieldBlanks(6)) {
                SimpleTower(TowerNum).FanPowerfAirFlowCurve = CurveManager::GetCurveIndex(AlphArray(6));
                if (SimpleTower(TowerNum).FanPowerfAirFlowCurve == 0) {
                    ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                     "\" the Fan Power Ratio as a function of Air Flow Rate Ratio Curve Name specified as " + AlphArray(6) +
                                     " was not found. Fan Power as a function of Air Flow Rate Ratio will default to Fan Power = (Air Flow Rate "
                                     "Ratio)^3 and the simulation continues.");
                }
            }

            SimpleTower(VariableSpeedTowerNumber).Coeff.allocate(35);
            SimpleTower(VariableSpeedTowerNumber).Coeff = 0.0;

            if (UtilityRoutines::SameString(AlphArray(4), "CoolToolsCrossFlow")) {
                SimpleTower(TowerNum).TowerModelType = CoolToolsXFModel;
                //     set cross-flow model coefficients
                //       Outputs approach in C
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(1) = 0.52049709836241;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(2) = -10.617046395344;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(3) = 10.7292974722538;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(4) = -2.74988377158227;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(5) = 4.73629943913743;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(6) = -8.25759700874711;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(7) = 1.57640938114136;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(8) = 6.51119643791324;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(9) = 1.50433525206692;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(10) = -3.2888529287801;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(11) = 0.0257786145353773;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(12) = 0.182464289315254;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(13) = -0.0818947291400898;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(14) = -0.215010003996285;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(15) = 0.0186741309635284;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(16) = 0.0536824177590012;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(17) = -0.00270968955115031;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(18) = 0.00112277498589279;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(19) = -0.00127758497497718;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(20) = 0.0000760420796601607;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(21) = 1.43600088336017;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(22) = -0.5198695909109;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(23) = 0.117339576910507;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(24) = 1.50492810819924;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(25) = -0.135898905926974;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(26) = -0.152577581866506;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(27) = -0.0533843828114562;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(28) = 0.00493294869565511;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(29) = -0.00796260394174197;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(30) = 0.000222619828621544;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(31) = -0.0543952001568055;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(32) = 0.00474266879161693;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(33) = -0.0185854671815598;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(34) = 0.00115667701293848;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(35) = 0.000807370664460284;

                //       set minimum and maximum boundaries for CoolTools crossflow model input variables
                SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp = -1.0;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp = 26.6667;
                SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp = 1.1111;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp = 11.1111;
                SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp = 1.1111;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp = 11.1111;
                SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio = 0.75;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio = 1.25;

            } else if (UtilityRoutines::SameString(AlphArray(4), "YorkCalc")) {
                SimpleTower(TowerNum).TowerModelType = YorkCalcModel;
                //     set counter-flow model coefficients
                //       Outputs approach in C
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(1) = -0.359741205;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(2) = -0.055053608;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(3) = 0.0023850432;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(4) = 0.173926877;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(5) = -0.0248473764;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(6) = 0.00048430224;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(7) = -0.005589849456;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(8) = 0.0005770079712;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(9) = -0.00001342427256;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(10) = 2.84765801111111;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(11) = -0.121765149;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(12) = 0.0014599242;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(13) = 1.680428651;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(14) = -0.0166920786;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(15) = -0.0007190532;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(16) = -0.025485194448;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(17) = 0.0000487491696;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(18) = 0.00002719234152;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(19) = -0.0653766255555556;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(20) = -0.002278167;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(21) = 0.0002500254;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(22) = -0.0910565458;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(23) = 0.00318176316;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(24) = 0.000038621772;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(25) = -0.0034285382352;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(26) = 0.00000856589904;
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(27) = -0.000001516821552;

                //       set minimum and maximum boundaries for YorkCalc model input variables
                SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp = -34.4;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp = 29.4444;
                SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp = 1.1111;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp = 22.2222;
                SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp = 1.1111;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp = 40.0;
                SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio = 0.75;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio = 1.25;
                SimpleTower(SimpleTower(TowerNum).VSTower).MaxLiquidToGasRatio = 8.0;

            } else if (UtilityRoutines::SameString(AlphArray(4), "CoolToolsUserDefined")) {
                SimpleTower(TowerNum).TowerModelType = CoolToolsUserDefined;
                // Nested Get-input routines below.  Should pull out of here and read in beforehand.
                for (VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSCoolToolsModelCoeffs; ++VSModelCoeffNum) {
                    inputProcessor->getObjectItem(
                        "CoolingTowerPerformance:CoolTools", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat);
                    if (!UtilityRoutines::SameString(AlphArray2(1), SimpleTower(TowerNum).ModelCoeffObjectName)) continue;
                    SimpleTower(SimpleTower(TowerNum).VSTower).FoundModelCoeff = true;
                    // verify the correct number of coefficients for the CoolTools model
                    if (NumNums2 != 43) {
                        ShowSevereError("CoolingTower:VariableSpeed \"" + SimpleTower(TowerNum).Name +
                                        "\". The number of numeric inputs for object CoolingTowerPerformance:CoolTools \"" +
                                        SimpleTower(TowerNum).ModelCoeffObjectName + "\" must equal 43.");
                        ErrorsFound = true;
                    } else {

                        SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp = NumArray2(1);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp = NumArray2(2);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp = NumArray2(3);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp = NumArray2(4);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp = NumArray2(5);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp = NumArray2(6);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio = NumArray2(7);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio = NumArray2(8);

                        for (CoeffNum = 9; CoeffNum <= NumNums2; ++CoeffNum) {
                            SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(CoeffNum - 8) = NumArray2(CoeffNum);
                        }
                    }
                    break;
                }
                if (!SimpleTower(SimpleTower(TowerNum).VSTower).FoundModelCoeff) {
                    ShowSevereError("CoolingTower:VariableSpeed \"" + SimpleTower(TowerNum).Name +
                                    "\". User defined name for variable speed cooling tower model coefficients object not found = " +
                                    SimpleTower(TowerNum).ModelCoeffObjectName);
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(AlphArray(4), "YorkCalcUserDefined")) {
                SimpleTower(TowerNum).TowerModelType = YorkCalcUserDefined;
                // Nested Get-input routines below.  Should pull out of here and read in beforehand.
                for (VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSYorkCalcModelCoeffs; ++VSModelCoeffNum) {
                    inputProcessor->getObjectItem(
                        "CoolingTowerPerformance:YorkCalc", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat);
                    if (!UtilityRoutines::SameString(AlphArray2(1), SimpleTower(TowerNum).ModelCoeffObjectName)) continue;
                    SimpleTower(SimpleTower(TowerNum).VSTower).FoundModelCoeff = true;
                    // verify the correct number of coefficients for the YorkCalc model
                    if (NumNums2 != 36) {
                        ShowSevereError("CoolingTower:VariableSpeed \"" + SimpleTower(TowerNum).Name +
                                        "\". The number of numeric inputs for object CoolingTowerPerformance:YorkCalc \"" +
                                        SimpleTower(TowerNum).ModelCoeffObjectName + "\" must equal 36.");
                        ErrorsFound = true;
                    } else {

                        SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp = NumArray2(1);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp = NumArray2(2);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp = NumArray2(3);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp = NumArray2(4);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp = NumArray2(5);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp = NumArray2(6);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio = NumArray2(7);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio = NumArray2(8);
                        SimpleTower(SimpleTower(TowerNum).VSTower).MaxLiquidToGasRatio = NumArray2(9);

                        for (CoeffNum = 10; CoeffNum <= NumNums2; ++CoeffNum) {
                            SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(CoeffNum - 9) = NumArray2(CoeffNum);
                        }
                    }
                    break;
                }

                if (!SimpleTower(SimpleTower(TowerNum).VSTower).FoundModelCoeff) {
                    ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                    "\". User defined name for variable speed cooling tower model coefficients object not found = " +
                                    SimpleTower(TowerNum).ModelCoeffObjectName);
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name + "\". Illegal Tower Model Type = " + AlphArray(5));
                ShowContinueError(R"( Tower Model Type must be "CoolToolsCrossFlow", "YorkCalc", "CoolToolsUserDefined", or "YorkCalcUserDefined.)");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).TowerMassFlowRateMultiplier = SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio;

            //   check user defined minimums to be greater than 0
            if (SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp < 0.0) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". User defined minimum approach temperature must be > 0");
                ErrorsFound = true;
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp < 0.0) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name + "\". User defined minimum range temperature must be > 0");
                ErrorsFound = true;
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio < 0.0) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". User defined minimum water flow rate ratio must be > 0");
                ErrorsFound = true;
            }

            //   check that the user defined maximums are greater than the minimums
            if (SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp < SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". User defined maximum approach temperature must be > the minimum approach temperature");
                ErrorsFound = true;
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp < SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". User defined maximum range temperature must be > the minimum range temperature");
                ErrorsFound = true;
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio < SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio) {
                ShowSevereError(cCurrentModuleObject + " \"" + SimpleTower(TowerNum).Name +
                                "\". User defined maximum water flow rate ratio must be > the minimum water flow rate ratio");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).DesignInletWB = NumArray(1);
            if (NumArray(1) < SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp ||
                NumArray(1) > SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp) {
                ObjexxFCL::gio::write(OutputChar, OutputFormat) << SimpleTower(TowerNum).DesignInletWB;
                ObjexxFCL::gio::write(OutputCharLo, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp;
                ObjexxFCL::gio::write(OutputCharHi, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp;
                ShowSevereError(cCurrentModuleObject.append(", \"").append(SimpleTower(TowerNum).Name).append("\" the design inlet air wet-bulb temperature of ").append(OutputChar).append(" must be within the model limits of ").append(OutputCharLo).append(" and ").append(OutputCharHi).append(" degrees C"));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).DesignApproach = NumArray(2);
            if (NumArray(2) < SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp ||
                NumArray(2) > SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp) {
                ObjexxFCL::gio::write(OutputChar, OutputFormat) << SimpleTower(TowerNum).DesignApproach;
                ObjexxFCL::gio::write(OutputCharLo, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp;
                ObjexxFCL::gio::write(OutputCharHi, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp;
                ShowSevereError(cCurrentModuleObject.append(", \"").append(SimpleTower(TowerNum).Name).append("\" the design approach temperature of ").append(OutputChar).append(" must be within the model limits of ").append(OutputCharLo).append(" and ").append(OutputCharHi).append(" degrees C"));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).DesignRange = NumArray(3);
            if (NumArray(3) < SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp ||
                NumArray(3) > SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp) {
                ObjexxFCL::gio::write(OutputChar, OutputFormat) << SimpleTower(TowerNum).DesignRange;
                ObjexxFCL::gio::write(OutputCharLo, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp;
                ObjexxFCL::gio::write(OutputCharHi, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp;
                ShowSevereError(cCurrentModuleObject.append(", \"").append(SimpleTower(TowerNum).Name).append("\" the design range temperature of ").append(OutputChar).append(" must be within the model limits of ").append(OutputCharLo).append(" and ").append(OutputCharHi).append(" degrees C"));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).DesignWaterFlowRate = NumArray(4);
            if (SimpleTower(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            if (NumArray(4) <= 0.0 && NumArray(4) != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" design water flow rate must be > 0");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).HighSpeedAirFlowRate = NumArray(5);
            if (SimpleTower(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            if (NumArray(5) <= 0.0 && NumArray(5) != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" design air flow rate must be > 0");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).HighSpeedFanPower = NumArray(6);
            if (SimpleTower(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            if (NumArray(6) <= 0.0 && NumArray(6) != DataSizing::AutoSize) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" design fan power must be > 0");
                ErrorsFound = true;
            }

            //   minimum air flow rate fraction must be >= 0.2 and <= 0.5, below this value the tower fan cycles to maintain the setpoint
            SimpleTower(TowerNum).MinimumVSAirFlowFrac = NumArray(7);
            SimpleTower(TowerNum).MinimumVSAirFlowFrac = NumArray(7);
            if (NumArray(7) < 0.2 || NumArray(7) > 0.5) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                "\" minimum VS air flow rate ratio must be >= 0.2 and <= 0.5");
                ErrorsFound = true;
            }

            //   fraction of tower capacity in free convection regime must be >= to 0 and <= 0.2
            SimpleTower(TowerNum).FreeConvectionCapacityFraction = NumArray(8);
            if (NumArray(8) < 0.0 || NumArray(8) > 0.2) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                "\" fraction of tower capacity in free convection regime must be >= 0 and <= 0.2");
                ErrorsFound = true;
            }

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            SimpleTower(TowerNum).BasinHeaterPowerFTempDiff = NumArray(9);
            if (NumArray(9) < 0.0) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).BasinHeaterSetPointTemp = NumArray(10);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 10) {
                    SimpleTower(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (SimpleTower(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(cCurrentModuleObject + ":\"" + SimpleTower(TowerNum).Name + "\", " + cNumericFieldNames(10) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            // Performance Input Method for Variable Speed Towers is assigned to be UA AND DESIGN WATER FLOW RATE
            // for autosizing calculations (see SizeTower)
            SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_UFactor;

            if (!AlphArray(7).empty()) {
                SimpleTower(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(AlphArray(7));
                if (SimpleTower(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" basin heater schedule name \"" + AlphArray(7) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(8), "LossFactor")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByUserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(8), "SaturatedExit")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else if (lAlphaFieldBlanks(8)) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else {
                ShowSevereError("Invalid " + cAlphaFieldNames(8) + '=' + AlphArray(8));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).UserEvapLossFactor = NumArray(11);        //  N11 , \field Evaporation Loss Factor
            SimpleTower(TowerNum).DriftLossFraction = NumArray(12) / 100.0; //  N12, \field Drift Loss Percent
            SimpleTower(TowerNum).ConcentrationRatio = NumArray(13);        //  N13, \field Blowdown Concentration Ratio
            SimpleTower(TowerNum).SizFac = NumArray(17);                    //  N14  \field Sizing Factor
            if (SimpleTower(TowerNum).SizFac <= 0.0) SimpleTower(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(9), "ScheduledRate")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownBySchedule;
            } else if (UtilityRoutines::SameString(AlphArray(9), "ConcentrationRatio")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
            } else if (lAlphaFieldBlanks(9)) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
            } else {
                ShowSevereError("Invalid " + cAlphaFieldNames(9) + '=' + AlphArray(9));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }
            SimpleTower(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(AlphArray(10));
            if ((SimpleTower(TowerNum).SchedIDBlowdown == 0) && (SimpleTower(TowerNum).BlowdownMode == BlowdownBySchedule)) {
                ShowSevereError("Invalid " + cAlphaFieldNames(10) + '=' + AlphArray(10));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + AlphArray(1));
                ErrorsFound = true;
            }

            // added for multi-cell
            SimpleTower(TowerNum).NumCell = NumArray(14);
            if ((NumNums < 14) && (SimpleTower(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                SimpleTower(TowerNum).NumCell = 1;
            }
            SimpleTower(TowerNum).MinFracFlowRate = NumArray(15);
            if ((NumNums < 15) && (SimpleTower(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MinFracFlowRate = 0.33;
            }
            SimpleTower(TowerNum).MaxFracFlowRate = NumArray(16);
            if ((NumNums < 16) && (SimpleTower(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MaxFracFlowRate = 2.5;
            }

            if (NumAlphas >= 13) {
                if (lAlphaFieldBlanks(13) || AlphArray(13).empty()) {
                    SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(13), "MinimalCell") || UtilityRoutines::SameString(AlphArray(13), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(13), "MinimalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(13), "MaximalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                        }
                    } else {
                        ShowSevereError("Illegal " + cAlphaFieldNames(13) + " = " + AlphArray(13));
                        ShowContinueError("Occurs in " + SimpleTower(TowerNum).TowerType + '=' + SimpleTower(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
            }

            if (lAlphaFieldBlanks(11)) {
                SimpleTower(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(AlphArray(1),
                                         cCurrentModuleObject,
                                         AlphArray(11),
                                         ErrorsFound,
                                         SimpleTower(TowerNum).WaterTankID,
                                         SimpleTower(TowerNum).WaterTankDemandARRID);
                SimpleTower(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node
            if (lAlphaFieldBlanks(12)) {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(12),
                                                                                 ErrorsFound,
                                                                                 cCurrentModuleObject,
                                                                                 SimpleTower(TowerNum).Name,
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(SimpleTower(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                    "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(12));
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }
            if (NumAlphas > 13) {
                SimpleTower(TowerNum).EndUseSubcategory = AlphArray(14);
            } else {
                SimpleTower(TowerNum).EndUseSubcategory = "General";
            }

        } // End Variable-Speed Tower Loop

        cCurrentModuleObject = cCoolingTower_VariableSpeedMerkel;
        for (MerkelVSTowerNum = 1; MerkelVSTowerNum <= NumVSMerkelTowers; ++MerkelVSTowerNum) {
            TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + MerkelVSTowerNum;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          MerkelVSTowerNum,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueSimpleTowerNames, AlphArray(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            SimpleTower(TowerNum).Name = AlphArray(1);
            SimpleTower(TowerNum).TowerType = cCurrentModuleObject;
            SimpleTower(TowerNum).TowerType_Num = CoolingTower_VariableSpeedMerkel;
            SimpleTower(TowerNum).WaterInletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(2), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            SimpleTower(TowerNum).WaterOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                AlphArray(3), ErrorsFound, cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

            if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
                SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_UFactor;
            } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
                SimpleTower(TowerNum).PerformanceInputMethod_Num = PIM_NominalCapacity;
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid, " + cAlphaFieldNames(4) + " = " + AlphArray(4));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).FanPowerfAirFlowCurve = CurveManager::GetCurveIndex(AlphArray(5));
            if (SimpleTower(TowerNum).FanPowerfAirFlowCurve == 0) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(5) + '=' + AlphArray(5));
                ShowContinueError("Curve name not found.");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio = NumArray(1);
            SimpleTower(TowerNum).TowerNominalCapacity = NumArray(2);
            if (SimpleTower(TowerNum).TowerNominalCapacity == DataSizing::AutoSize) {
                SimpleTower(TowerNum).TowerNominalCapacityWasAutoSized = true;
            }
            SimpleTower(TowerNum).TowerFreeConvNomCap = NumArray(3);
            if (SimpleTower(TowerNum).TowerFreeConvNomCap == DataSizing::AutoSize) {
                SimpleTower(TowerNum).TowerFreeConvNomCapWasAutoSized = true;
            }
            SimpleTower(TowerNum).TowerFreeConvNomCapSizingFactor = NumArray(4);
            SimpleTower(TowerNum).DesignWaterFlowRate = NumArray(5);
            if (SimpleTower(TowerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).DesignWaterFlowPerUnitNomCap = NumArray(6);
            SimpleTower(TowerNum).HighSpeedAirFlowRate = NumArray(7);
            if (SimpleTower(TowerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).DefaultedDesignAirFlowScalingFactor = lNumericFieldBlanks(8);
            SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap = NumArray(8);
            SimpleTower(TowerNum).MinimumVSAirFlowFrac = NumArray(9);
            SimpleTower(TowerNum).HighSpeedFanPower = NumArray(10);
            if (SimpleTower(TowerNum).HighSpeedFanPower == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedFanPowerWasAutoSized = true;
            }
            SimpleTower(TowerNum).DesignFanPowerPerUnitNomCap = NumArray(11);
            SimpleTower(TowerNum).FreeConvAirFlowRate = NumArray(12);
            if (SimpleTower(TowerNum).FreeConvAirFlowRate == DataSizing::AutoSize) {
                SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor = NumArray(13);
            SimpleTower(TowerNum).HighSpeedTowerUA = NumArray(14);
            if (SimpleTower(TowerNum).HighSpeedTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).HighSpeedTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvTowerUA = NumArray(15);
            if (SimpleTower(TowerNum).FreeConvTowerUA == DataSizing::AutoSize) {
                SimpleTower(TowerNum).FreeConvTowerUAWasAutoSized = true;
            }
            SimpleTower(TowerNum).FreeConvTowerUASizingFactor = NumArray(16);

            SimpleTower(TowerNum).UAModFuncAirFlowRatioCurvePtr = CurveManager::GetCurveIndex(AlphArray(6));
            if (SimpleTower(TowerNum).UAModFuncAirFlowRatioCurvePtr == 0) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(6) + '=' + AlphArray(6));
                ShowContinueError("Curve name not found.");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).UAModFuncWetBulbDiffCurvePtr = CurveManager::GetCurveIndex(AlphArray(7));
            if (SimpleTower(TowerNum).UAModFuncWetBulbDiffCurvePtr == 0) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(7) + '=' + AlphArray(7));
                ShowContinueError("Curve name not found.");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).UAModFuncWaterFlowRatioCurvePtr = CurveManager::GetCurveIndex(AlphArray(8));
            if (SimpleTower(TowerNum).UAModFuncWaterFlowRatioCurvePtr == 0) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(8) + '=' + AlphArray(8));
                ShowContinueError("Curve name not found.");
                ErrorsFound = true;
            }
            // cooling tower design inlet conditions
            SimpleTower(TowerNum).DesInletAirDBTemp = NumArray(17);
            if (SimpleTower(TowerNum).DesInletAirDBTemp == 0) {
                SimpleTower(TowerNum).DesInletAirDBTemp = 35.0;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesInletAirWBTemp = NumArray(18);
            if (SimpleTower(TowerNum).DesInletAirWBTemp == 0) {
                SimpleTower(TowerNum).DesInletAirWBTemp = 25.6;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesApproach = NumArray(19);
            if (SimpleTower(TowerNum).DesApproach == DataSizing::AutoSize || SimpleTower(TowerNum).DesApproach == 0) {
                SimpleTower(TowerNum).DesApproach = 3.9;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            SimpleTower(TowerNum).DesRange = NumArray(20);
            if (SimpleTower(TowerNum).DesRange == DataSizing::AutoSize || SimpleTower(TowerNum).DesRange == 0) {
                SimpleTower(TowerNum).DesRange = 5.5;
                SimpleTower(TowerNum).TowerInletCondsAutoSize = true;
            }
            // set tower design water outlet and inlet temperatures
            SimpleTower(TowerNum).DesOutletWaterTemp = SimpleTower(TowerNum).DesInletAirWBTemp + SimpleTower(TowerNum).DesApproach;
            SimpleTower(TowerNum).DesInletWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp + SimpleTower(TowerNum).DesRange;
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            SimpleTower(TowerNum).BasinHeaterPowerFTempDiff = NumArray(21);
            if (NumArray(21) < 0.0) {
                ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                "\" basin heater power as a function of temperature difference must be >= 0");
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).BasinHeaterSetPointTemp = NumArray(22);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 22) {
                    SimpleTower(TowerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (SimpleTower(TowerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(cCurrentModuleObject + ":\"" + SimpleTower(TowerNum).Name + "\", " + cNumericFieldNames(22) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!AlphArray(9).empty()) {
                SimpleTower(TowerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(AlphArray(9));
                if (SimpleTower(TowerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name + "\" basin heater schedule name \"" + AlphArray(9) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            // begin water use and systems get input
            if (UtilityRoutines::SameString(AlphArray(10), "LossFactor")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByUserFactor;
            } else if (UtilityRoutines::SameString(AlphArray(10), "SaturatedExit")) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else if (lAlphaFieldBlanks(10)) {
                SimpleTower(TowerNum).EvapLossMode = EvapLossByMoistTheory;
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(10) + '=' + AlphArray(10));
                ErrorsFound = true;
            }

            SimpleTower(TowerNum).UserEvapLossFactor = NumArray(23);        //  N23 , \field Evaporation Loss Factor
            SimpleTower(TowerNum).DriftLossFraction = NumArray(24) / 100.0; //  N24, \field Drift Loss Percent
            if ((NumNums < 24) && (SimpleTower(TowerNum).DriftLossFraction == 0.0)) {
                // assume Drift loss not entered and should be defaulted
                SimpleTower(TowerNum).DriftLossFraction = 0.008 / 100.0;
            }

            SimpleTower(TowerNum).ConcentrationRatio = NumArray(25); //  N25, \field Blowdown Concentration Ratio
            SimpleTower(TowerNum).SizFac = NumArray(29);             //  N29  \field Sizing Factor
            if (SimpleTower(TowerNum).SizFac <= 0.0) SimpleTower(TowerNum).SizFac = 1.0;

            if (UtilityRoutines::SameString(AlphArray(11), "ScheduledRate")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownBySchedule;
            } else if (UtilityRoutines::SameString(AlphArray(11), "ConcentrationRatio")) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
            } else if (lAlphaFieldBlanks(11)) {
                SimpleTower(TowerNum).BlowdownMode = BlowdownByConcentration;
                if ((NumNums < 25) && (SimpleTower(TowerNum).ConcentrationRatio == 0.0)) {
                    // assume Concetration ratio was omitted and should be defaulted
                    SimpleTower(TowerNum).ConcentrationRatio = 3.0;
                }
            } else {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(11) + '=' + AlphArray(11));
                ErrorsFound = true;
            }
            SimpleTower(TowerNum).SchedIDBlowdown = ScheduleManager::GetScheduleIndex(AlphArray(12));
            if ((SimpleTower(TowerNum).SchedIDBlowdown == 0) && (SimpleTower(TowerNum).BlowdownMode == BlowdownBySchedule)) {
                ShowSevereError(cCurrentModuleObject + '=' + AlphArray(1));
                ShowContinueError("Invalid " + cAlphaFieldNames(12) + '=' + AlphArray(12));
                ErrorsFound = true;
            }

            // added for multi-cell
            SimpleTower(TowerNum).NumCell = NumArray(26);
            if ((NumNums < 26) && (SimpleTower(TowerNum).NumCell == 0)) {
                // assume Number of Cells not entered and should be defaulted
                SimpleTower(TowerNum).NumCell = 1;
            }
            SimpleTower(TowerNum).MinFracFlowRate = NumArray(27);
            if ((NumNums < 27) && (SimpleTower(TowerNum).MinFracFlowRate == 0.0)) {
                // assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MinFracFlowRate = 0.33;
            }
            SimpleTower(TowerNum).MaxFracFlowRate = NumArray(28);
            if ((NumNums < 28) && (SimpleTower(TowerNum).MaxFracFlowRate == 0.0)) {
                // assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
                SimpleTower(TowerNum).MaxFracFlowRate = 2.5;
            }
            SimpleTower(TowerNum).TowerMassFlowRateMultiplier = SimpleTower(TowerNum).MaxFracFlowRate;
            if (NumAlphas >= 15) {
                if (lAlphaFieldBlanks(15) || AlphArray(15).empty()) {
                    SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                } else {
                    if (UtilityRoutines::SameString(AlphArray(15), "MinimalCell") || UtilityRoutines::SameString(AlphArray(15), "MaximalCell")) {
                        if (UtilityRoutines::SameString(AlphArray(15), "MinimalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MinCell;
                        }
                        if (UtilityRoutines::SameString(AlphArray(15), "MaximalCell")) {
                            SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
                        }
                    } else {
                        ShowSevereError("Illegal " + cAlphaFieldNames(15) + " = " + AlphArray(15));
                        ShowContinueError("Occurs in " + SimpleTower(TowerNum).TowerType + '=' + SimpleTower(TowerNum).Name);
                        ErrorsFound = true;
                    }
                }
            } else {
                // assume Cell Control not entered and should be defaulted
                SimpleTower(TowerNum).CellCtrl_Num = CellCtrl_MaxCell;
            }

            if (lAlphaFieldBlanks(13)) {
                SimpleTower(TowerNum).SuppliedByWaterSystem = false;
            } else { // water from storage tank
                WaterManager::SetupTankDemandComponent(AlphArray(1),
                                         cCurrentModuleObject,
                                         AlphArray(13),
                                         ErrorsFound,
                                         SimpleTower(TowerNum).WaterTankID,
                                         SimpleTower(TowerNum).WaterTankDemandARRID);
                SimpleTower(TowerNum).SuppliedByWaterSystem = true;
            }

            //   outdoor air inlet node
            if (lAlphaFieldBlanks(14)) {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = 0;
            } else {
                SimpleTower(TowerNum).OutdoorAirInletNodeNum = NodeInputManager::GetOnlySingleNode(AlphArray(14),
                                                                                 ErrorsFound,
                                                                                 cCurrentModuleObject,
                                                                                 SimpleTower(TowerNum).Name,
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
                if (!OutAirNodeManager::CheckOutAirNodeNumber(SimpleTower(TowerNum).OutdoorAirInletNodeNum)) {
                    ShowSevereError(cCurrentModuleObject + ", \"" + SimpleTower(TowerNum).Name +
                                    "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray(14));
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }
            if (NumAlphas > 15) {
                SimpleTower(TowerNum).EndUseSubcategory = AlphArray(16);
            } else {
                SimpleTower(TowerNum).EndUseSubcategory = "General";
            }

        } // end merkel vs tower loop

        if (ErrorsFound) {
            ShowFatalError("Errors found in getting cooling tower input.");
        }

        // Set up output variables CurrentModuleObject='CoolingTower:SingleSpeed'
        for (TowerNum = 1; TowerNum <= NumSingleSpeedTowers; ++TowerNum) {
            SetupOutputVariable("Cooling Tower Inlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).InletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Outlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).OutletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                SimpleTower(TowerNum).WaterMassFlowRate,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).Qactual,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Power",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).FanPower,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Energy",
                                OutputProcessor::Unit::J,
                                SimpleTower(TowerNum).FanEnergy,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name,
                                _,
                                "Electric",
                                "HeatRejection",
                                SimpleTower(TowerNum).EndUseSubcategory,
                                "Plant");
            // Added for fluid bypass
            SetupOutputVariable("Cooling Tower Bypass Fraction",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).BypassFraction,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Operating Cells Count",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).NumCellOn,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Cycling Ratio",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).FanCyclingRatio,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Cooling Tower Basin Heater Electric Power",
                                    OutputProcessor::Unit::W,
                                    SimpleTower(TowerNum).BasinHeaterPower,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Basin Heater Electric Energy",
                                    OutputProcessor::Unit::J,
                                    SimpleTower(TowerNum).BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "Electric",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }

        // CurrentModuleObject='CoolingTower:TwoSpeed'
        for (TowerNum = NumSingleSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers; ++TowerNum) {
            SetupOutputVariable("Cooling Tower Inlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).InletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Outlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).OutletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                SimpleTower(TowerNum).WaterMassFlowRate,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).Qactual,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Power",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).FanPower,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Energy",
                                OutputProcessor::Unit::J,
                                SimpleTower(TowerNum).FanEnergy,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name,
                                _,
                                "Electric",
                                "HeatRejection",
                                SimpleTower(TowerNum).EndUseSubcategory,
                                "Plant");
            SetupOutputVariable("Cooling Tower Fan Cycling Ratio",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).FanCyclingRatio,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Speed Level",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).SpeedSelected,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Operating Cells Count",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).NumCellOn,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Cooling Tower Basin Heater Electric Power",
                                    OutputProcessor::Unit::W,
                                    SimpleTower(TowerNum).BasinHeaterPower,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Basin Heater Electric Energy",
                                    OutputProcessor::Unit::J,
                                    SimpleTower(TowerNum).BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "Electric",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }

        // CurrentModuleObject='CoolingTower:VariableSpeed'
        for (TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers;
             ++TowerNum) {
            SetupOutputVariable("Cooling Tower Inlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).InletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Outlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).OutletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                SimpleTower(TowerNum).WaterMassFlowRate,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).Qactual,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Power",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).FanPower,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Energy",
                                OutputProcessor::Unit::J,
                                SimpleTower(TowerNum).FanEnergy,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name,
                                _,
                                "Electric",
                                "HeatRejection",
                                SimpleTower(TowerNum).EndUseSubcategory,
                                "Plant");
            SetupOutputVariable("Cooling Tower Air Flow Rate Ratio",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).AirFlowRatio,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).FanCyclingRatio,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Operating Cells Count",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).NumCellOn,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Cooling Tower Basin Heater Electric Power",
                                    OutputProcessor::Unit::W,
                                    SimpleTower(TowerNum).BasinHeaterPower,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Basin Heater Electric Energy",
                                    OutputProcessor::Unit::J,
                                    SimpleTower(TowerNum).BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "Electric",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }

        }

        // CurrentModuleObject='CoolingTower:VariableSpeed:Merkel'
        for (TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + 1;
             TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers;
             ++TowerNum) {
            SetupOutputVariable("Cooling Tower Inlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).InletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Outlet Temperature",
                                OutputProcessor::Unit::C,
                                SimpleTower(TowerNum).OutletWaterTemp,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                SimpleTower(TowerNum).WaterMassFlowRate,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).Qactual,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Power",
                                OutputProcessor::Unit::W,
                                SimpleTower(TowerNum).FanPower,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Fan Electric Energy",
                                OutputProcessor::Unit::J,
                                SimpleTower(TowerNum).FanEnergy,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name,
                                _,
                                "Electric",
                                "HeatRejection",
                                SimpleTower(TowerNum).EndUseSubcategory,
                                "Plant");
            SetupOutputVariable("Cooling Tower Fan Speed Ratio",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).AirFlowRatio,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);

            SetupOutputVariable("Cooling Tower Operating Cells Count",
                                OutputProcessor::Unit::None,
                                SimpleTower(TowerNum).NumCellOn,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            if (SimpleTower(TowerNum).BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Cooling Tower Basin Heater Electric Power",
                                    OutputProcessor::Unit::W,
                                    SimpleTower(TowerNum).BasinHeaterPower,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Basin Heater Electric Energy",
                                    OutputProcessor::Unit::J,
                                    SimpleTower(TowerNum).BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "Electric",
                                    "HeatRejection",
                                    "BasinHeater",
                                    "Plant");
            }
        }
        // setup common water reporting for all types of towers.
        for (TowerNum = 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers; ++TowerNum) {
            if (SimpleTower(TowerNum).SuppliedByWaterSystem) {
                SetupOutputVariable("Cooling Tower Make Up Water Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    SimpleTower(TowerNum).MakeUpVdot,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Make Up Water Volume",
                                    OutputProcessor::Unit::m3,
                                    SimpleTower(TowerNum).MakeUpVol,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Storage Tank Water Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    SimpleTower(TowerNum).TankSupplyVdot,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Storage Tank Water Volume",
                                    OutputProcessor::Unit::m3,
                                    SimpleTower(TowerNum).TankSupplyVol,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "Water",
                                    "HeatRejection",
                                    _,
                                    "Plant");
                SetupOutputVariable("Cooling Tower Starved Storage Tank Water Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    SimpleTower(TowerNum).StarvedMakeUpVdot,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Starved Storage Tank Water Volume",
                                    OutputProcessor::Unit::m3,
                                    SimpleTower(TowerNum).StarvedMakeUpVol,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Make Up Mains Water Volume",
                                    OutputProcessor::Unit::m3,
                                    SimpleTower(TowerNum).StarvedMakeUpVol,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "MainsWater",
                                    "HeatRejection",
                                    _,
                                    "Plant");
            } else { // tower water from mains and gets metered
                SetupOutputVariable("Cooling Tower Make Up Water Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    SimpleTower(TowerNum).MakeUpVdot,
                                    "System",
                                    "Average",
                                    SimpleTower(TowerNum).Name);
                SetupOutputVariable("Cooling Tower Make Up Water Volume",
                                    OutputProcessor::Unit::m3,
                                    SimpleTower(TowerNum).MakeUpVol,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "Water",
                                    "HeatRejection",
                                    _,
                                    "Plant");
                SetupOutputVariable("Cooling Tower Make Up Mains Water Volume",
                                    OutputProcessor::Unit::m3,
                                    SimpleTower(TowerNum).MakeUpVol,
                                    "System",
                                    "Sum",
                                    SimpleTower(TowerNum).Name,
                                    _,
                                    "MainsWater",
                                    "HeatRejection",
                                    _,
                                    "Plant");
            }

            SetupOutputVariable("Cooling Tower Water Evaporation Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                SimpleTower(TowerNum).EvaporationVdot,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Water Evaporation Volume",
                                OutputProcessor::Unit::m3,
                                SimpleTower(TowerNum).EvaporationVol,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Water Drift Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                SimpleTower(TowerNum).DriftVdot,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Water Drift Volume",
                                OutputProcessor::Unit::m3,
                                SimpleTower(TowerNum).DriftVol,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Water Blowdown Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                SimpleTower(TowerNum).BlowdownVdot,
                                "System",
                                "Average",
                                SimpleTower(TowerNum).Name);
            SetupOutputVariable("Cooling Tower Water Blowdown Volume",
                                OutputProcessor::Unit::m3,
                                SimpleTower(TowerNum).BlowdownVol,
                                "System",
                                "Sum",
                                SimpleTower(TowerNum).Name);
        } // loop all towers
    }

    void InitTower(int const TowerNum,           // Number of the current cooling tower being simulated
                   bool const EP_UNUSED(RunFlag) // Indication of
    )
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

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TypeOf_Num(0);
        Real64 rho; // local density of fluid

        if (SimpleTower(TowerNum).oneTimeFlag) {

            if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed) {
                TypeOf_Num = DataPlant::TypeOf_CoolingTower_SingleSpd;
            } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                TypeOf_Num = DataPlant::TypeOf_CoolingTower_TwoSpd;
            } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_VariableSpeed) {
                TypeOf_Num = DataPlant::TypeOf_CoolingTower_VarSpd;
            } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_VariableSpeedMerkel) {
                TypeOf_Num = DataPlant::TypeOf_CoolingTower_VarSpdMerkel;
            } else {
                assert(false);
            }

            // Locate the tower on the plant loops for later usage
            bool ErrorsFound = false;
            PlantUtilities::ScanPlantLoopsForObject(SimpleTower(TowerNum).Name,
                                    TypeOf_Num,
                                    SimpleTower(TowerNum).LoopNum,
                                    SimpleTower(TowerNum).LoopSideNum,
                                    SimpleTower(TowerNum).BranchNum,
                                    SimpleTower(TowerNum).CompNum,
                                    ErrorsFound,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (ErrorsFound) {
                ShowFatalError("InitTower: Program terminated due to previous condition(s).");
            }

            // check if setpoint on outlet node
            SimpleTower(TowerNum).SetpointIsOnOutlet = !((DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                                                         (DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue));

            SimpleTower(TowerNum).oneTimeFlag = false;
        }

        // Begin environment initializations
        if (SimpleTower(TowerNum).envrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                   RoutineName);

            SimpleTower(TowerNum).DesWaterMassFlowRate = SimpleTower(TowerNum).DesignWaterFlowRate * rho;
            SimpleTower(TowerNum).DesWaterMassFlowRatePerCell = SimpleTower(TowerNum).DesWaterMassFlowRate / SimpleTower(TowerNum).NumCell;
            PlantUtilities::InitComponentNodes(0.0,
                               SimpleTower(TowerNum).DesWaterMassFlowRate,
                               SimpleTower(TowerNum).WaterInletNodeNum,
                               SimpleTower(TowerNum).WaterOutletNodeNum,
                               SimpleTower(TowerNum).LoopNum,
                               SimpleTower(TowerNum).LoopSideNum,
                               SimpleTower(TowerNum).BranchNum,
                               SimpleTower(TowerNum).CompNum);

            SimpleTower(TowerNum).envrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            SimpleTower(TowerNum).envrnFlag = true;
        }

        // Each time initializations
        SimpleTower(TowerNum).WaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;

        if (SimpleTower(TowerNum).OutdoorAirInletNodeNum != 0) {
            SimpleTower(TowerNum).AirTemp = DataLoopNode::Node(SimpleTower(TowerNum).OutdoorAirInletNodeNum).Temp;
            SimpleTower(TowerNum).AirHumRat = DataLoopNode::Node(SimpleTower(TowerNum).OutdoorAirInletNodeNum).HumRat;
            SimpleTower(TowerNum).AirPress = DataLoopNode::Node(SimpleTower(TowerNum).OutdoorAirInletNodeNum).Press;
            SimpleTower(TowerNum).AirWetBulb = DataLoopNode::Node(SimpleTower(TowerNum).OutdoorAirInletNodeNum).OutAirWetBulb;
        } else {
            SimpleTower(TowerNum).AirTemp = DataEnvironment::OutDryBulbTemp;
            SimpleTower(TowerNum).AirHumRat = DataEnvironment::OutHumRat;
            SimpleTower(TowerNum).AirPress = DataEnvironment::OutBaroPress;
            SimpleTower(TowerNum).AirWetBulb = DataEnvironment::OutWetBulbTemp;
        }

        SimpleTower(TowerNum).WaterMassFlowRate =
            PlantUtilities::RegulateCondenserCompFlowReqOp(SimpleTower(TowerNum).LoopNum,
                                           SimpleTower(TowerNum).LoopSideNum,
                                           SimpleTower(TowerNum).BranchNum,
                                           SimpleTower(TowerNum).CompNum,
                                           SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).TowerMassFlowRateMultiplier);

        PlantUtilities::SetComponentFlowRate(SimpleTower(TowerNum).WaterMassFlowRate,
                             SimpleTower(TowerNum).WaterInletNodeNum,
                             SimpleTower(TowerNum).WaterOutletNodeNum,
                             SimpleTower(TowerNum).LoopNum,
                             SimpleTower(TowerNum).LoopSideNum,
                             SimpleTower(TowerNum).BranchNum,
                             SimpleTower(TowerNum).CompNum);

        // Added for fluid bypass. 8/2008
        SimpleTower(TowerNum).BypassFraction = 0.0;
        SimpleTower(TowerNum).BasinHeaterPower = 0.0;
        SimpleTower(TowerNum).__AirFlowRateRatio = 0.0;
    }

    void SizeTower(int const TowerNum)
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
        static ObjexxFCL::gio::Fmt OutputFormat("(F6.2)");
        static ObjexxFCL::gio::Fmt OutputFormat2("(F9.6)");
        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static std::string const RoutineName("SizeTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizCondNum(0);           // Plant Sizing index for condenser loop
        int SolFla;                     // Flag of solver
        Real64 DesTowerLoad(0.0);       // Design tower load [W]
        Real64 UA0;                     // Lower bound for UA [W/C]
        Real64 UA1;                     // Upper bound for UA [W/C]
        Real64 UA;                      // Calculated UA value
        Real64 Twb;                     // tower inlet air wet-bulb temperature [C]
        Real64 Tr;                      // tower range temperature [C]
        Real64 Ta;                      // tower approach temperature [C]
        Real64 WaterFlowRatio(0.0);     // tower water flow rate ratio found during model calibration
        Real64 MaxWaterFlowRateRatio;   // maximum water flow rate ratio which yields desired approach temp
        Real64 WaterFlowRateRatio(0.0); // tower water flow rate ratio
        Real64 Tapproach;               // temporary tower approach temp variable [C]
        Real64 ModelWaterFlowRatioMax;  // maximum water flow rate ratio used for model calibration
        Real64 FlowRateRatioStep;       // flow rate ratio to determine maximum water flow rate ratio during calibration
        Array1D<Real64> Par(6);         // Parameter array need for RegulaFalsi routine
        bool ModelCalibrated;           // TRUE if water flow rate ratio is with the specified range
        std::string OutputChar;         // report variable for warning messages
        std::string OutputChar2;        // report variable for warning messages
        std::string OutputCharLo;       // report variable for warning messages
        std::string OutputCharHi;       // report variable for warning messages
        std::string equipName;
        Real64 Cp;                      // local specific heat for fluid
        Real64 rho;                     // local density for fluid
        Real64 tmpDesignWaterFlowRate;  // local temporary for water volume flow rate
        Real64 tmpHighSpeedFanPower;    // local temporary for high speed fan power
        Real64 tmpHighSpeedAirFlowRate; // local temporary for high speed air flow rate
        Real64 tmpLowSpeedAirFlowRate;  // local temporary for low speed air flow rate
        Real64 AssumedDeltaT;           // default delta T for nominal capacity of hard sized with UA method
        Real64 AssumedExitTemp;         // default for cp fo nominal capacity of hard sized with UA method
        bool ErrorsFound;
        Real64 OutWaterTemp;              // outlet water temperature during sizing [C]
        Real64 CoolingOutput;             // tower capacity during sizing [W]
        Real64 DesTowerInletWaterTemp;    // design tower inlet water temperature
        Real64 DesTowerExitWaterTemp;     // design tower exit water temperature
        Real64 DesTowerWaterDeltaT;       // design tower temperature range
        Real64 DesTowerApproachFromPlant; // design tower approach temperature from plant sizing object
        Real64 TolTemp(0.04);             // DeltaT and DesApproach diffs tollerance between plant sizing data and user input in cooling tower
        // for warning message reporting purpose only

        tmpDesignWaterFlowRate = SimpleTower(TowerNum).DesignWaterFlowRate;
        tmpHighSpeedFanPower = SimpleTower(TowerNum).HighSpeedFanPower;
        tmpHighSpeedAirFlowRate = SimpleTower(TowerNum).HighSpeedAirFlowRate;
        tmpLowSpeedAirFlowRate = SimpleTower(TowerNum).LowSpeedAirFlowRate;

        // Find the appropriate Plant Sizing object
        PltSizCondNum = DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).PlantSizNum;

        if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed || SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
            if (SimpleTower(TowerNum).TowerInletCondsAutoSize) {
                if (PltSizCondNum > 0) {
                    // use plant sizing data
                    DesTowerExitWaterTemp = DataSizing::PlantSizData(PltSizCondNum).ExitTemp;
                    DesTowerInletWaterTemp = DesTowerExitWaterTemp + DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                    DesTowerWaterDeltaT = DataSizing::PlantSizData(PltSizCondNum).DeltaT;
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
                DesTowerExitWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp;
                DesTowerInletWaterTemp = SimpleTower(TowerNum).DesInletWaterTemp;
                DesTowerWaterDeltaT = SimpleTower(TowerNum).DesRange;
                if (PltSizCondNum > 0) {
                    // check the tower range against the plant sizing data
                    if (std::abs(DesTowerWaterDeltaT - DataSizing::PlantSizData(PltSizCondNum).DeltaT) > TolTemp) {
                        ShowWarningError("Error when autosizing the load for cooling tower = " + SimpleTower(TowerNum).Name +
                                         ". Tower Design Range Temperature is different from the Design Loop Delta Temperature.");
                        ShowContinueError("Tower Design Range Temperature specified in tower = " + SimpleTower(TowerNum).Name);
                        ShowContinueError("is inconsistent with Design Loop Delta Temperature specified in Sizing:Plant object = " +
                                              DataSizing::PlantSizData(PltSizCondNum).PlantLoopName + ".");
                        ShowContinueError("..The Design Range Temperature specified in tower is = " +
                                              General::TrimSigDigits(SimpleTower(TowerNum).DesRange, 2));
                        ShowContinueError("..The Design Loop Delta Temperature specified in plant sizing data is = " +
                                              General::TrimSigDigits(DataSizing::PlantSizData(PltSizCondNum).DeltaT, 2));
                    }
                    // check if the tower approach is different from plant sizing data
                    DesTowerApproachFromPlant = DataSizing::PlantSizData(PltSizCondNum).ExitTemp - SimpleTower(TowerNum).DesInletAirWBTemp;
                    if (std::abs(DesTowerApproachFromPlant - SimpleTower(TowerNum).DesApproach) > TolTemp) {
                        ShowWarningError("Error when autosizing the UA for cooling tower = " + SimpleTower(TowerNum).Name +
                                         ". Tower Design Approach Temperature is inconsistent with Approach from Plant Sizing Data.");
                        ShowContinueError("The Design Approach Temperature from inputs specified in Sizing:Plant object = " +
                                              DataSizing::PlantSizData(PltSizCondNum).PlantLoopName);
                        ShowContinueError("is inconsistent with Design Approach Temperature specified in tower = " + SimpleTower(TowerNum).Name +
                                          ".");
                        ShowContinueError("..The Design Approach Temperature from inputs specified is = " +
                                              General::TrimSigDigits(DesTowerApproachFromPlant, 2));
                        ShowContinueError("..The Design Approach Temperature specified in tower is = " +
                                              General::TrimSigDigits(SimpleTower(TowerNum).DesApproach, 2));
                    }
                }
            }
        } else { // CoolingTower_VariableSpeed
            if (PltSizCondNum > 0) {
                // use plant sizing data
                DesTowerExitWaterTemp = DataSizing::PlantSizData(PltSizCondNum).ExitTemp;
                DesTowerInletWaterTemp = DesTowerExitWaterTemp + DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                DesTowerWaterDeltaT = DataSizing::PlantSizData(PltSizCondNum).DeltaT;
            } else {
                // set hard wired input assumptions
                DesTowerWaterDeltaT = 11.0;
                DesTowerExitWaterTemp = 21.0;
                DesTowerInletWaterTemp = DesTowerExitWaterTemp + DesTowerWaterDeltaT;
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_UFactor && (!SimpleTower(TowerNum).HighSpeedTowerUAWasAutoSized)) {
            if (PltSizCondNum > 0) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       DesTowerExitWaterTemp,
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName);
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           DesTowerExitWaterTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);
                DesTowerLoad = rho * Cp * SimpleTower(TowerNum).DesignWaterFlowRate * DesTowerWaterDeltaT;
                SimpleTower(TowerNum).TowerNominalCapacity = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;

            } else {
                AssumedDeltaT = DesTowerWaterDeltaT;
                AssumedExitTemp = DesTowerExitWaterTemp;

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       AssumedExitTemp,
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName);
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           AssumedExitTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);

                DesTowerLoad = rho * Cp * SimpleTower(TowerNum).DesignWaterFlowRate * AssumedDeltaT;
                SimpleTower(TowerNum).TowerNominalCapacity = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
            }
        }

        if (SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized) {
            if (PltSizCondNum > 0) {
                if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    tmpDesignWaterFlowRate = DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate * SimpleTower(TowerNum).SizFac;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) SimpleTower(TowerNum).DesignWaterFlowRate = tmpDesignWaterFlowRate;
                } else {
                    tmpDesignWaterFlowRate = 0.0;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) SimpleTower(TowerNum).DesignWaterFlowRate = tmpDesignWaterFlowRate;
                }
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Design Water Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Design Water Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ShowSevereError("Autosizing error for cooling tower object = " + SimpleTower(TowerNum).Name);
                    ShowFatalError("Autosizing of cooling tower condenser flow rate requires a loop Sizing:Plant object.");
                }
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
            // Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
            SimpleTower(TowerNum).DesignWaterFlowRate = 5.382e-8 * SimpleTower(TowerNum).TowerNominalCapacity;
            tmpDesignWaterFlowRate = SimpleTower(TowerNum).DesignWaterFlowRate;
            if (UtilityRoutines::SameString(SimpleTower(TowerNum).TowerType, "CoolingTower:SingleSpeed")) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Design Water Flow Rate based on tower nominal capacity [m3/s]",
                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Design Water Flow Rate based on tower nominal capacity [m3/s]",
                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                }
            } else if (UtilityRoutines::SameString(SimpleTower(TowerNum).TowerType, "CoolingTower:TwoSpeed")) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]",
                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]",
                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                }
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(SimpleTower(TowerNum).WaterInletNodeNum, tmpDesignWaterFlowRate);

        if (SimpleTower(TowerNum).HighSpeedFanPowerWasAutoSized) {
            // We assume the nominal fan power is 0.0105 times the design load
            if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
                SimpleTower(TowerNum).HighSpeedFanPower = 0.0105 * SimpleTower(TowerNum).TowerNominalCapacity;
                tmpHighSpeedFanPower = SimpleTower(TowerNum).HighSpeedFanPower;
            } else {
                if (PltSizCondNum > 0) {
                    if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                   DesTowerExitWaterTemp,
                                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                   RoutineName);
                        DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * DesTowerWaterDeltaT;
                        tmpHighSpeedFanPower = 0.0105 * DesTowerLoad;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) SimpleTower(TowerNum).HighSpeedFanPower = tmpHighSpeedFanPower;
                    } else {
                        tmpHighSpeedFanPower = 0.0;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) SimpleTower(TowerNum).HighSpeedFanPower = tmpHighSpeedFanPower;
                    }
                } else {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ShowSevereError("Autosizing of cooling tower fan power requires a loop Sizing:Plant object.");
                        ShowFatalError(" Occurs in cooling tower object= " + SimpleTower(TowerNum).Name);
                    }
                }
            }
            if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed ||
                SimpleTower(TowerNum).TowerType_Num == CoolingTower_VariableSpeed) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Fan Power at Design Air Flow Rate [W]",
                                       SimpleTower(TowerNum).HighSpeedFanPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Fan Power at Design Air Flow Rate [W]",
                                       SimpleTower(TowerNum).HighSpeedFanPower);
                }
            } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Fan Power at High Fan Speed [W]",
                                       SimpleTower(TowerNum).HighSpeedFanPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Fan Power at High Fan Speed [W]",
                                       SimpleTower(TowerNum).HighSpeedFanPower);
                }
            }
        }

        if (SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized) {
            // Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.
            tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5 * (101325.0 / DataEnvironment::StdBaroPress) / 190.0;
            if (DataPlant::PlantFirstSizesOkayToFinalize) SimpleTower(TowerNum).HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;

            if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed ||
                SimpleTower(TowerNum).TowerType_Num == CoolingTower_VariableSpeed) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Design Air Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).HighSpeedAirFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Design Air Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).HighSpeedAirFlowRate);
                }
            } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Air Flow Rate at High Fan Speed [m3/s]",
                                       SimpleTower(TowerNum).HighSpeedAirFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Air Flow Rate at High Fan Speed [m3/s]",
                                       SimpleTower(TowerNum).HighSpeedAirFlowRate);
                }
            }
        }

        if (SimpleTower(TowerNum).HighSpeedTowerUAWasAutoSized) {
            if (PltSizCondNum > 0) {
                if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                    DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * DesTowerWaterDeltaT;
                    // This conditional statement is to trap when the user specified condenser/tower water design setpoint
                    //  temperature is less than design inlet air wet bulb temperature
                    if (DataSizing::PlantSizData(PltSizCondNum).ExitTemp <= SimpleTower(TowerNum).DesInletAirWBTemp) {
                        ShowSevereError("Error when autosizing the UA value for cooling tower = " + SimpleTower(TowerNum).Name +
                                        ". Design Loop Exit Temperature must be greater than " +
                                            General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) + " C when autosizing the tower UA.");
                        ShowContinueError(
                            "The Design Loop Exit Temperature specified in Sizing:Plant object = " + DataSizing::PlantSizData(PltSizCondNum).PlantLoopName +
                            " (" + General::TrimSigDigits(DataSizing::PlantSizData(PltSizCondNum).ExitTemp, 2) + " C)");
                        ShowContinueError("is less than or equal to the design inlet air wet-bulb temperature of " +
                                              General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) + " C.");
                        ShowContinueError(
                            "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be > " +
                                General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) + " C if autosizing the cooling tower.");
                        ShowFatalError("Autosizing of cooling tower fails for tower = " + SimpleTower(TowerNum).Name + '.');
                    }

                    Par(1) = DesTowerLoad;
                    Par(2) = double(TowerNum);
                    Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par(4) = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                    Par(5) = Cp;
                    UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
                    UA1 = DesTowerLoad;          // Assume deltaT = 1K
                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp;
                    SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0;
                    SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6;
                    SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                    SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                    General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError("Iteration limit exceeded in calculating tower UA");
                        ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    } else if (SolFla == -2) {
                        ShowSevereError("Bad starting values for UA");
                        ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    }

                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).HighSpeedTowerUA = UA;
                    }
                    SimpleTower(TowerNum).TowerNominalCapacity = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                } else {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).HighSpeedTowerUA = 0.0;
                    }
                }
                if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "U-Factor Times Area Value at High Fan Speed [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial U-Factor Times Area Value at High Fan Speed [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                }
            } else {
                if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                    DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * DesTowerWaterDeltaT;
                    // This conditional statement is to trap when the user specified condenser/tower water design setpoint
                    //  temperature is less than design inlet air wet bulb temperature
                    // Note JM 2018-11-22
                    // * If actually user-specified:
                    //  SimpleTower(TowerNum).DesOutletWaterTemp = SimpleTower(TowerNum).DesInletAirWBTemp
                    //                                           + SimpleTower(TowerNum).DesApproach;
                    //  DesTowerExitWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp;
                    //  => This basically means that approach is negative, which is impossible (must be > 0 per IDD)
                    // * If not, hardcoded above to 21C
                    if (DesTowerExitWaterTemp <= SimpleTower(TowerNum).DesInletAirWBTemp) {
                        ShowSevereError("Error when autosizing the UA value for cooling tower = " + SimpleTower(TowerNum).Name +
                                        ". Design Tower Exit Temperature must be greater than " +
                                            General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) + " C when autosizing the tower UA.");
                        ShowContinueError("The User-specified Design Loop Exit Temperature=" + General::TrimSigDigits(DesTowerExitWaterTemp, 2));
                        ShowContinueError("is less than or equal to the design inlet air wet-bulb temperature of " +
                                              General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) + " C.");

                        if (SimpleTower(TowerNum).TowerInletCondsAutoSize) {
                            ShowContinueError(
                                "Because you did not specify the Design Approach Temperature, and you do not have a Sizing:Plant object, "
                                "it was defaulted to " +
                                    General::TrimSigDigits(DesTowerExitWaterTemp, 2) + " C.");
                        } else {
                            // Should never get there...
                            ShowContinueError("The Design Loop Exit Temperature is the sum of the design air inlet wet-bulb temperature= " +
                                                  General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) +
                                              " C plus the cooling tower design approach temperature = " +
                                                  General::TrimSigDigits(SimpleTower(TowerNum).DesApproach, 2) + "C.");
                        }
                        ShowContinueError(
                            "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be > " +
                                General::TrimSigDigits(SimpleTower(TowerNum).DesInletAirWBTemp, 2) + " C if autosizing the cooling tower.");
                        ShowFatalError("Autosizing of cooling tower fails for tower = " + SimpleTower(TowerNum).Name + '.');
                    }

                    Par(1) = DesTowerLoad;
                    Par(2) = double(TowerNum);
                    Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par(4) = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                    Par(5) = Cp;
                    UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
                    UA1 = DesTowerLoad;          // Assume deltaT = 1K
                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp;
                    SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0;
                    SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6;
                    SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                    SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                    General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError("Iteration limit exceeded in calculating tower UA");
                        ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    } else if (SolFla == -2) {
                        ShowSevereError("Bad starting values for UA");
                        ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    }

                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).HighSpeedTowerUA = UA;
                    }
                    SimpleTower(TowerNum).TowerNominalCapacity = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                } else {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).HighSpeedTowerUA = 0.0;
                    }
                }
                if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "U-Factor Times Area Value at High Fan Speed [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial U-Factor Times Area Value at High Fan Speed [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                }
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
            if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                // nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of delivered cooling but now is
                // a user input
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       29.44,
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName); // 85F design exiting water temp
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           29.44,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName); // 85F design exiting water temp

                DesTowerLoad = SimpleTower(TowerNum).TowerNominalCapacity * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                Par(1) = DesTowerLoad;
                Par(2) = double(TowerNum);
                Par(3) = rho * tmpDesignWaterFlowRate;                                           // design water mass flow rate
                Par(4) = tmpHighSpeedAirFlowRate;                                                // design air volume flow rate
                Par(5) = Cp;                                                                     // 85F design exiting water temp
                UA0 = 0.0001 * DesTowerLoad;                                                     // Assume deltaT = 10000K (limit)
                UA1 = DesTowerLoad;                                                              // Assume deltaT = 1K
                SimpleTower(TowerNum).WaterTemp = SimpleTower(TowerNum).DesInletWaterTemp;  // 35.0; // 95F design inlet water temperature
                SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 95F design inlet air dry-bulb temp
                SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 78F design inlet air wet-bulb temp
                SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Iteration limit exceeded in calculating tower UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                } else if (SolFla == -2) {
                    ShowSevereError("Bad starting values for UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).HighSpeedTowerUA = UA;
                }
            } else {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).HighSpeedTowerUA = 0.0;
                }
            }
            if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                       SimpleTower(TowerNum).HighSpeedTowerUA);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]",
                                       SimpleTower(TowerNum).HighSpeedTowerUA);
                }
            } else if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "U-Factor Times Area Value at High Fan Speed [W/C]",
                                       SimpleTower(TowerNum).HighSpeedTowerUA);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial U-Factor Times Area Value at High Fan Speed [W/C]",
                                       SimpleTower(TowerNum).HighSpeedTowerUA);
                }
            }
        }

        if (SimpleTower(TowerNum).LowSpeedAirFlowRateWasAutoSized) {

            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                SimpleTower(TowerNum).LowSpeedAirFlowRate =
                    SimpleTower(TowerNum).LowSpeedAirFlowRateSizingFactor * SimpleTower(TowerNum).HighSpeedAirFlowRate;
                tmpLowSpeedAirFlowRate = SimpleTower(TowerNum).LowSpeedAirFlowRate;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Low Fan Speed Air Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).LowSpeedAirFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Low Fan Speed Air Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).LowSpeedAirFlowRate);
                }
            } else {
                tmpLowSpeedAirFlowRate = SimpleTower(TowerNum).LowSpeedAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate;
            }
        }

        if (SimpleTower(TowerNum).LowSpeedFanPowerWasAutoSized) {
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                SimpleTower(TowerNum).LowSpeedFanPower = SimpleTower(TowerNum).LowSpeedFanPowerSizingFactor * SimpleTower(TowerNum).HighSpeedFanPower;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Fan Power at Low Fan Speed [W]",
                                       SimpleTower(TowerNum).LowSpeedFanPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Fan Power at Low Fan Speed [W]",
                                       SimpleTower(TowerNum).LowSpeedFanPower);
                }
            }
        }

        if (SimpleTower(TowerNum).LowSpeedTowerUAWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
            SimpleTower(TowerNum).LowSpeedTowerUA = SimpleTower(TowerNum).LowSpeedTowerUASizingFactor * SimpleTower(TowerNum).HighSpeedTowerUA;
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "U-Factor Times Area Value at Low Fan Speed [W/K]",
                                   SimpleTower(TowerNum).LowSpeedTowerUA);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager:: ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Initial U-Factor Times Area Value at Low Fan Speed [W/K]",
                                   SimpleTower(TowerNum).LowSpeedTowerUA);
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
            if (SimpleTower(TowerNum).TowerLowSpeedNomCapWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).TowerLowSpeedNomCap =
                        SimpleTower(TowerNum).TowerLowSpeedNomCapSizingFactor * SimpleTower(TowerNum).TowerNominalCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Low Speed Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerLowSpeedNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Low Speed Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerLowSpeedNomCap);
                    }
                }
            }
            if (SimpleTower(TowerNum).TowerFreeConvNomCapWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).TowerFreeConvNomCap =
                        SimpleTower(TowerNum).TowerFreeConvNomCapSizingFactor * SimpleTower(TowerNum).TowerNominalCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Free Convection Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerFreeConvNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Free Convection Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerFreeConvNomCap);
                    }
                }
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity &&
            UtilityRoutines::SameString(SimpleTower(TowerNum).TowerType, "CoolingTower:TwoSpeed")) {
            if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow && SimpleTower(TowerNum).TowerLowSpeedNomCap > 0.0) {

                // nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now is a
                // user input
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       29.44,
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName); // 85F design exiting water temp
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           29.44,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName); // 85F design exiting water temp
                DesTowerLoad = SimpleTower(TowerNum).TowerLowSpeedNomCap * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                Par(1) = DesTowerLoad;
                Par(2) = double(TowerNum);
                Par(3) = rho * tmpDesignWaterFlowRate;                                           // design water mass flow rate
                Par(4) = tmpLowSpeedAirFlowRate;                                                 // Air volume flow rate at low fan speed
                Par(5) = Cp;                                                                     // 85F design exiting water temp
                UA0 = 0.0001 * DesTowerLoad;                                                     // Assume deltaT = 10000K (limit)
                UA1 = DesTowerLoad;                                                              // Assume deltaT = 1K
                SimpleTower(TowerNum).WaterTemp = SimpleTower(TowerNum).DesInletWaterTemp;  // 35.0; // 95F design inlet water temperature
                SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0; // 95F design inlet air dry-bulb temp
                SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6; // 78F design inlet air wet-bulb temp
                SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Iteration limit exceeded in calculating tower UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                } else if (SolFla == -2) {
                    ShowSevereError("Bad starting values for UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).LowSpeedTowerUA = UA;
                }
            } else {
                SimpleTower(TowerNum).LowSpeedTowerUA = 0.0;
            }
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Low Fan Speed U-Factor Times Area Value [W/K]",
                                   SimpleTower(TowerNum).LowSpeedTowerUA);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Initial Low Fan Speed U-Factor Times Area Value [W/K]",
                                   SimpleTower(TowerNum).LowSpeedTowerUA);
            }
        }

        if (SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized) {
            SimpleTower(TowerNum).FreeConvAirFlowRate = SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                SimpleTower(TowerNum).FreeConvAirFlowRate =
                    SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor * SimpleTower(TowerNum).HighSpeedAirFlowRate;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Free Convection Regime Air Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).FreeConvAirFlowRate);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Free Convection Regime Air Flow Rate [m3/s]",
                                       SimpleTower(TowerNum).FreeConvAirFlowRate);
                }
            }
        }

        if (SimpleTower(TowerNum).FreeConvTowerUAWasAutoSized) {
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                SimpleTower(TowerNum).FreeConvTowerUA = SimpleTower(TowerNum).FreeConvTowerUASizingFactor * SimpleTower(TowerNum).HighSpeedTowerUA;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Free Convection U-Factor Times Area Value [W/K]",
                                       SimpleTower(TowerNum).FreeConvTowerUA);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Free Convection U-Factor Times Area Value [W/K]",
                                       SimpleTower(TowerNum).FreeConvTowerUA);
                }
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {
            if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow && SimpleTower(TowerNum).TowerFreeConvNomCap > 0.0) {
                // nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now user
                // input
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       29.44,
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName); // 85F design exiting water temp
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           29.44,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName); // 85F design exiting water temp
                DesTowerLoad = SimpleTower(TowerNum).TowerFreeConvNomCap * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                Par(1) = DesTowerLoad;
                Par(2) = double(TowerNum);
                Par(3) = rho * SimpleTower(TowerNum).DesignWaterFlowRate;                        // design water mass flow rate
                Par(4) = SimpleTower(TowerNum).FreeConvAirFlowRate;                              // free convection air volume flow rate
                Par(5) = Cp;                                                                     // 85F design exiting water temp
                UA0 = 0.0001 * DesTowerLoad;                                                     // Assume deltaT = 10000K (limit)
                UA1 = DesTowerLoad;                                                              // Assume deltaT = 1K
                SimpleTower(TowerNum).WaterTemp = SimpleTower(TowerNum).DesInletWaterTemp;  // 35.0; // 95F design inlet water temperature
                SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0; // 95F design inlet air dry-bulb temp
                SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6; // 78F design inlet air wet-bulb temp
                SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Iteration limit exceeded in calculating tower UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                } else if (SolFla == -2) {
                    ShowSevereError("Bad starting values for UA calculations");
                    ShowContinueError("Tower inlet design water temperature assumed to be 35.0 C.");
                    ShowContinueError("Tower inlet design air dry-bulb temperature assumed to be 35.0 C.");
                    ShowContinueError("Tower inlet design air wet-bulb temperature assumed to be 25.6 C.");
                    ShowContinueError("Tower load assumed to be " + General::TrimSigDigits(SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio, 3) +
                                      " times free convection capacity of " + General::TrimSigDigits(SimpleTower(TowerNum).TowerFreeConvNomCap, 0) + " W.");

                    SimSimpleTower(TowerNum, Par(3), Par(4), UA0, OutWaterTemp);
                    CoolingOutput = Par(5) * Par(3) * (SimpleTower(TowerNum).WaterTemp - OutWaterTemp);
                    ShowContinueError("Tower capacity at lower UA guess (" + General::TrimSigDigits(UA0, 4) + ") = " + General::TrimSigDigits(CoolingOutput, 0) +
                                      " W.");

                    SimSimpleTower(TowerNum, Par(3), Par(4), UA1, OutWaterTemp);
                    CoolingOutput = Par(5) * Par(3) * (SimpleTower(TowerNum).WaterTemp - OutWaterTemp);
                    ShowContinueError("Tower capacity at upper UA guess (" + General::TrimSigDigits(UA1, 4) + ") = " + General::TrimSigDigits(CoolingOutput, 0) +
                                      " W.");

                    if (CoolingOutput < DesTowerLoad) {
                        ShowContinueError("Free convection capacity should be less than tower capacity at upper UA guess.");
                    }
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).FreeConvTowerUA = UA;
                }
            } else {
                SimpleTower(TowerNum).FreeConvTowerUA = 0.0;
            }
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                   SimpleTower(TowerNum).FreeConvTowerUA);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                   SimpleTower(TowerNum).FreeConvTowerUA);
            }
        }

        // calibrate variable speed tower model based on user input by finding calibration water flow rate ratio that
        // yields an approach temperature that matches user input
        if (UtilityRoutines::SameString(SimpleTower(TowerNum).TowerType, "CoolingTower:VariableSpeed")) {

            Twb = SimpleTower(TowerNum).DesignInletWB;
            Tr = SimpleTower(TowerNum).DesignRange;
            Ta = SimpleTower(TowerNum).DesignApproach;

            Par(1) = TowerNum; // Index to cooling tower
            Par(2) = 1.0;      // air flow rate ratio
            Par(3) = Twb;      // inlet air wet-bulb temperature [C]
            Par(4) = Tr;       // tower range temperature [C]
            Par(5) = Ta;       // design approach temperature [C]
            Par(6) = 0.0;      // Calculation FLAG, 0.0 = calc water flow ratio, 1.0 calc air flow ratio

            //   check range for water flow rate ratio (make sure RegulaFalsi converges)
            MaxWaterFlowRateRatio = 0.5;
            Tapproach = 0.0;
            FlowRateRatioStep =
                (SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio - SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio) / 10.0;
            ModelCalibrated = true;
            ModelWaterFlowRatioMax = SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio * 4.0;
            //   find a flow rate large enough to provide an approach temperature > than the user defined approach
            while (Tapproach < Ta && MaxWaterFlowRateRatio <= ModelWaterFlowRatioMax) {
                WaterFlowRateRatio = MaxWaterFlowRateRatio;
                CalcVSTowerApproach(TowerNum, WaterFlowRateRatio, 1.0, Twb, Tr, Tapproach);
                if (Tapproach < Ta) {
                    MaxWaterFlowRateRatio += FlowRateRatioStep;
                }
                // a water flow rate large enough to provide an approach temperature > than the user defined approach does not exist
                // within the tolerances specified by the user
                if ((MaxWaterFlowRateRatio == 0.5 && Tapproach > Ta) || MaxWaterFlowRateRatio >= ModelWaterFlowRatioMax) {
                    ModelCalibrated = false;
                    break;
                }
            }

            if (ModelCalibrated) {
                General::SolveRoot(Acc, MaxIte, SolFla, WaterFlowRatio, SimpleTowerApproachResidual, DataPrecisionGlobals::constant_pointfive, MaxWaterFlowRateRatio, Par);
                if (SolFla == -1) {
                    ShowSevereError("Iteration limit exceeded in calculating tower water flow ratio during calibration");
                    ShowContinueError("Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio "
                                      "for this variable-speed cooling tower.");
                    ShowFatalError("Cooling tower calibration failed for tower " + SimpleTower(TowerNum).Name);
                } else if (SolFla == -2) {
                    ShowSevereError("Bad starting values for cooling tower water flow rate ratio calibration.");
                    ShowContinueError("Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio "
                                      "for this variable-speed cooling tower.");
                    ShowFatalError("Cooling tower calibration failed for tower " + SimpleTower(TowerNum).Name + '.');
                }
            } else {
                ObjexxFCL::gio::write(OutputChar2, OutputFormat2) << WaterFlowRateRatio;
                ObjexxFCL::gio::write(OutputChar, OutputFormat) << Tapproach;
                ShowSevereError("Bad starting values for cooling tower water flow rate ratio calibration.");
                ShowContinueError("Design inlet air wet-bulb or range temperature must be modified to achieve the design approach");
                ShowContinueError("A water flow rate ratio of " + OutputChar2 + " was calculated to yield an approach temperature of " + OutputChar +
                                  '.');
                ShowFatalError("Cooling tower calibration failed for tower " + SimpleTower(TowerNum).Name + '.');
            }

            SimpleTower(TowerNum).CalibratedWaterFlowRate = SimpleTower(TowerNum).DesignWaterFlowRate / WaterFlowRatio;

            if (WaterFlowRatio < SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio ||
                WaterFlowRatio > SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio) {
                ObjexxFCL::gio::write(OutputChar2, OutputFormat2) << WaterFlowRatio;
                ObjexxFCL::gio::write(OutputCharLo, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio;
                ObjexxFCL::gio::write(OutputCharHi, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio;
                ShowWarningError("CoolingTower:VariableSpeed, \"" + SimpleTower(TowerNum).Name +
                                 "\" the calibrated water flow rate ratio is determined to be " + OutputChar2 +
                                 ". This is outside the valid range of " + OutputCharLo + " to " + OutputCharHi + '.');
            }

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                   (Twb + Ta + Tr),
                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                   RoutineName);
            Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       (Twb + Ta + Tr),
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName);

            SimpleTower(TowerNum).TowerNominalCapacity = ((rho * tmpDesignWaterFlowRate) * Cp * Tr);
            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(
                    SimpleTower(TowerNum).TowerType, SimpleTower(TowerNum).Name, "Nominal Capacity [W]", SimpleTower(TowerNum).TowerNominalCapacity);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Initial Nominal Capacity [W]",
                                   SimpleTower(TowerNum).TowerNominalCapacity);
            }
            SimpleTower(TowerNum).FreeConvAirFlowRate = SimpleTower(TowerNum).MinimumVSAirFlowFrac * SimpleTower(TowerNum).HighSpeedAirFlowRate;

            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Air Flow Rate in free convection regime [m3/s]",
                                   SimpleTower(TowerNum).FreeConvAirFlowRate);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Initial Air Flow Rate in free convection regime [m3/s]",
                                   SimpleTower(TowerNum).FreeConvAirFlowRate);
            }
            SimpleTower(TowerNum).TowerFreeConvNomCap =
                SimpleTower(TowerNum).TowerNominalCapacity * SimpleTower(TowerNum).FreeConvectionCapacityFraction;

            if (DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Tower capacity in free convection regime at design conditions [W]",
                                   SimpleTower(TowerNum).TowerFreeConvNomCap);
            }
            if (DataPlant::PlantFirstSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                   SimpleTower(TowerNum).Name,
                                   "Initial Tower capacity in free convection regime at design conditions [W]",
                                   SimpleTower(TowerNum).TowerFreeConvNomCap);
            }
        }
        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = SimpleTower(TowerNum).Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, SimpleTower(TowerNum).TowerType);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, SimpleTower(TowerNum).TowerNominalCapacity);
        }

        // input error checking
        ErrorsFound = false;
        if (DataPlant::PlantFinalSizesOkayToReport) {
            if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_SingleSpeed) {
                if (SimpleTower(TowerNum).DesignWaterFlowRate > 0.0) {
                    if (SimpleTower(TowerNum).FreeConvAirFlowRate >= SimpleTower(TowerNum).HighSpeedAirFlowRate) {
                        ShowSevereError(cCoolingTower_SingleSpeed + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Free convection air flow rate must be less than the design air flow rate.");
                        ErrorsFound = true;
                    }
                    if (SimpleTower(TowerNum).FreeConvTowerUA >= SimpleTower(TowerNum).HighSpeedTowerUA) {
                        ShowSevereError(cCoolingTower_SingleSpeed + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Free convection UA must be less than the design tower UA.");
                        ErrorsFound = true;
                    }
                }
            }

            if (SimpleTower(TowerNum).TowerType_Num == CoolingTower_TwoSpeed) {
                if (SimpleTower(TowerNum).DesignWaterFlowRate > 0.0) {
                    if (SimpleTower(TowerNum).HighSpeedAirFlowRate <= SimpleTower(TowerNum).LowSpeedAirFlowRate) {
                        ShowSevereError(cCoolingTower_TwoSpeed + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Low speed air flow rate must be less than the high speed air flow rate.");
                        ErrorsFound = true;
                    }
                    if (SimpleTower(TowerNum).LowSpeedAirFlowRate <= SimpleTower(TowerNum).FreeConvAirFlowRate) {
                        ShowSevereError(cCoolingTower_TwoSpeed + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Free convection air flow rate must be less than the low speed air flow rate.");
                        ErrorsFound = true;
                    }
                    if (SimpleTower(TowerNum).HighSpeedTowerUA <= SimpleTower(TowerNum).LowSpeedTowerUA) {
                        ShowSevereError(cCoolingTower_TwoSpeed + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Tower UA at low fan speed must be less than the tower UA at high fan speed.");
                        ErrorsFound = true;
                    }
                    if (SimpleTower(TowerNum).LowSpeedTowerUA <= SimpleTower(TowerNum).FreeConvTowerUA) {
                        ShowSevereError(cCoolingTower_TwoSpeed + " \"" + SimpleTower(TowerNum).Name +
                                        "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed.");
                        ErrorsFound = true;
                    }
                }
            }
            if (ErrorsFound) {
                ShowFatalError("InitTower: Program terminated due to previous condition(s).");
            }
        }
    }

    void SizeVSMerkelTower(int const TowerNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static std::string const RoutineName("SizeTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizCondNum; // Plant Sizing index for condenser loop
        int SolFla;        // Flag of solver
        Real64 tmpNomTowerCap;
        Real64 tmpDesignWaterFlowRate;
        Real64 tmpTowerFreeConvNomCap;
        Real64 tmpDesignAirFlowRate;
        Real64 tmpHighSpeedFanPower;
        Real64 tmpFreeConvAirFlowRate;

        Array1D<Real64> Par(6); // Parameter array need for RegulaFalsi routine
        Real64 UA0;             // Lower bound for UA [W/C]
        Real64 UA1;             // Upper bound for UA [W/C]
        Real64 DesTowerLoad;    // Design tower load [W]
        Real64 Cp(0);           // local specific heat for fluid
        Real64 rho(0);          // local density for fluid
        Real64 UA;              // Calculated UA value
        Real64 OutWaterTemp;
        Real64 DesTowerInletAirDBTemp;    // design tower inlet air dry-bulb temperature
        Real64 DesTowerInletAirWBTemp;    // design tower inlet air wet-bulb temperature
        Real64 DesTowerInletWaterTemp;    // design tower inlet water temperature
        Real64 DesTowerExitWaterTemp;     // design tower exit water temperature
        Real64 DesTowerWaterDeltaT;       // design tower temperature range
        Real64 DesTowerApproachFromPlant; // design tower approach temperature from plant sizing object
        Real64 TolTemp(0.04);             // DeltaT and DesApproach diffs tollerance between plant sizing data and user input in cooling tower
        // for warning message reporting purpose only

        // Find the appropriate Plant Sizing object
        PltSizCondNum = DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).PlantSizNum;

        tmpNomTowerCap = SimpleTower(TowerNum).TowerNominalCapacity;
        tmpDesignWaterFlowRate = SimpleTower(TowerNum).DesignWaterFlowRate;

        tmpTowerFreeConvNomCap = SimpleTower(TowerNum).TowerFreeConvNomCap;
        tmpDesignAirFlowRate = SimpleTower(TowerNum).HighSpeedAirFlowRate;
        tmpFreeConvAirFlowRate = SimpleTower(TowerNum).FreeConvAirFlowRate;
        DesTowerInletAirWBTemp = SimpleTower(TowerNum).DesInletAirWBTemp;
        DesTowerInletAirDBTemp = SimpleTower(TowerNum).DesInletAirDBTemp;

        if (SimpleTower(TowerNum).TowerInletCondsAutoSize) {
            if (PltSizCondNum > 0) {
                // use plant sizing data
                DesTowerExitWaterTemp = DataSizing::PlantSizData(PltSizCondNum).ExitTemp;
                DesTowerInletWaterTemp = DesTowerExitWaterTemp + DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                DesTowerWaterDeltaT = DataSizing::PlantSizData(PltSizCondNum).DeltaT;
            } else {
                // set default values to replace hard wired input assumptions
                DesTowerExitWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp;
                DesTowerInletWaterTemp = SimpleTower(TowerNum).DesInletWaterTemp;
                DesTowerWaterDeltaT = SimpleTower(TowerNum).DesRange;
            }
        } else {
            // use tower sizing data
            DesTowerExitWaterTemp = SimpleTower(TowerNum).DesOutletWaterTemp;
            DesTowerInletWaterTemp = SimpleTower(TowerNum).DesInletWaterTemp;
            DesTowerWaterDeltaT = SimpleTower(TowerNum).DesRange;
            if (PltSizCondNum > 0) {
                // check the tower range against the plant sizing data
                if (std::abs(DesTowerWaterDeltaT - DataSizing::PlantSizData(PltSizCondNum).DeltaT) > TolTemp) {
                    ShowWarningError("Error when autosizing the load for cooling tower = " + SimpleTower(TowerNum).Name +
                                     ". Tower Design Range Temperature is different from the Design Loop Delta Temperature.");
                    ShowContinueError("Tower Design Range Temperature specified in tower = " + SimpleTower(TowerNum).Name);
                    ShowContinueError("is inconsistent with Design Loop Delta Temperature specified in Sizing:Plant object = " +
                                          DataSizing::PlantSizData(PltSizCondNum).PlantLoopName + ".");
                    ShowContinueError("..The Design Range Temperature specified in tower is = " + General::TrimSigDigits(SimpleTower(TowerNum).DesRange, 2));
                    ShowContinueError("..The Design Loop Delta Temperature specified iin plant sizing data is = " +
                                          General::TrimSigDigits(DataSizing::PlantSizData(PltSizCondNum).DeltaT, 2));
                }
                // check if the tower approach is different from plant sizing data
                DesTowerApproachFromPlant = DataSizing::PlantSizData(PltSizCondNum).ExitTemp - SimpleTower(TowerNum).DesInletAirWBTemp;
                if (std::abs(DesTowerApproachFromPlant - SimpleTower(TowerNum).DesApproach) > TolTemp) {
                    ShowWarningError("Error when autosizing the UA for cooling tower = " + SimpleTower(TowerNum).Name +
                                     ". Tower Design Approach Temperature is inconsistent with Approach from Plant Sizing Data.");
                    ShowContinueError("The Design Approach Temperature from inputs specified in Sizing:Plant object = " +
                                          DataSizing::PlantSizData(PltSizCondNum).PlantLoopName);
                    ShowContinueError("is inconsistent with Design Approach Temperature specified in tower = " + SimpleTower(TowerNum).Name + ".");
                    ShowContinueError("..The Design Approach Temperature from inputs specified is = " + General::TrimSigDigits(DesTowerApproachFromPlant, 2));
                    ShowContinueError("..The Design Approach Temperature specified in tower is = " +
                                          General::TrimSigDigits(SimpleTower(TowerNum).DesApproach, 2));
                }
            }
        }

        if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_NominalCapacity) {

            if (PltSizCondNum > 0) { // get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
                if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           DesTowerExitWaterTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                    DesTowerLoad = rho * Cp * DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate * DesTowerWaterDeltaT * SimpleTower(TowerNum).SizFac;
                    tmpNomTowerCap = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                } else {
                    if (SimpleTower(TowerNum).TowerNominalCapacityWasAutoSized) tmpNomTowerCap = 0.0;
                }
            } else {                                                  // PltSizCondNum = 0
                if (!SimpleTower(TowerNum).TowerInletCondsAutoSize) { // can use design data entered into tower object
                    if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                   DesTowerExitWaterTemp,
                                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                   RoutineName);
                        DesTowerLoad = rho * Cp * SimpleTower(TowerNum).DesignWaterFlowRate * DesTowerWaterDeltaT * SimpleTower(TowerNum).SizFac;
                        tmpNomTowerCap = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    } else {
                        if (SimpleTower(TowerNum).TowerNominalCapacityWasAutoSized) tmpNomTowerCap = 0.0;
                    }
                } else { // do not have enough data to size.
                    if (DataPlant::PlantFirstSizesOkayToFinalize && SimpleTower(TowerNum).TowerNominalCapacityWasAutoSized) {
                        ShowSevereError("Autosizing error for cooling tower object = " + SimpleTower(TowerNum).Name);
                        ShowFatalError("Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                    }
                }
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (SimpleTower(TowerNum).TowerNominalCapacityWasAutoSized) {
                    SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            SimpleTower(TowerNum).TowerType, SimpleTower(TowerNum).Name, "Design Nominal Capacity [W]", tmpNomTowerCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Design Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerNominalCapacity);
                    }
                } else { // Hard-sized with sizing data
                    if (SimpleTower(TowerNum).TowerNominalCapacity > 0.0 && tmpNomTowerCap > 0.0) {
                        Real64 NomCapUser(0.0);
                        NomCapUser = SimpleTower(TowerNum).TowerNominalCapacity;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Design Nominal Capacity [W]",
                                               tmpNomTowerCap,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomTowerCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeVSMerkelTower: Potential issue with equipment sizing for " + SimpleTower(TowerNum).Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomTowerCap, 2) +
                                                      " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                            tmpNomTowerCap = NomCapUser;
                        }
                    }
                }
            }

            tmpTowerFreeConvNomCap = tmpNomTowerCap * SimpleTower(TowerNum).TowerFreeConvNomCapSizingFactor;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (SimpleTower(TowerNum).TowerFreeConvNomCapWasAutoSized) {
                    SimpleTower(TowerNum).TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Design Free Convection Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerFreeConvNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Design Free Convection Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerFreeConvNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (SimpleTower(TowerNum).TowerFreeConvNomCap > 0.0 && tmpTowerFreeConvNomCap > 0.0) {
                        Real64 NomCapUser(0.0);
                        NomCapUser = SimpleTower(TowerNum).TowerFreeConvNomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Design Free Convection Nominal Capacity [W]",
                                               tmpTowerFreeConvNomCap,
                                               "User-Specified Free Convection Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpTowerFreeConvNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeVSMerkelTower: Potential issue with equipment sizing for " + SimpleTower(TowerNum).Name);
                                    ShowContinueError("User-Specified Free Convection Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) +
                                                      " [W]");
                                    ShowContinueError("differs from Design Size Free Convection Nominal Capacity of " +
                                                      General::RoundSigDigits(tmpTowerFreeConvNomCap, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                            tmpTowerFreeConvNomCap = NomCapUser;
                        }
                    }
                }
            }

            tmpDesignWaterFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignWaterFlowPerUnitNomCap;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized) {
                    // for nominal cap input method, get design water flow rate from nominal cap and scalable sizing factor

                    SimpleTower(TowerNum).DesignWaterFlowRate = tmpDesignWaterFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Design Water Flow Rate [m3/s]",
                                           SimpleTower(TowerNum).DesignWaterFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Design Water Flow Rate [m3/s]",
                                           SimpleTower(TowerNum).DesignWaterFlowRate);
                    }

                } else { // Hard-sized with sizing data
                    if (SimpleTower(TowerNum).DesignWaterFlowRate > 0.0 && tmpDesignWaterFlowRate > 0.0) {
                        Real64 NomDesWaterFlowUser(0.0);
                        NomDesWaterFlowUser = SimpleTower(TowerNum).DesignWaterFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Design Water Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).DesignWaterFlowRate,
                                               "User-Specified Design Water Flow Rate [m3/s]",
                                               NomDesWaterFlowUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpDesignWaterFlowRate - NomDesWaterFlowUser) / NomDesWaterFlowUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeVSMerkelTower: Potential issue with equipment sizing for " + SimpleTower(TowerNum).Name);
                                    ShowContinueError("User-Specified Design Water Flow Rate of " + General::RoundSigDigits(NomDesWaterFlowUser, 2) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Water Flow Rate of " + General::RoundSigDigits(tmpDesignWaterFlowRate, 2) +
                                                      " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                            tmpDesignWaterFlowRate = NomDesWaterFlowUser;
                        }
                    }
                }
            }

            PlantUtilities::RegisterPlantCompDesignFlow(SimpleTower(TowerNum).WaterInletNodeNum, tmpDesignWaterFlowRate);

            if (SimpleTower(TowerNum).DefaultedDesignAirFlowScalingFactor) {
                tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap * (101325.0 / DataEnvironment::StdBaroPress);
            } else {
                tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized) {
                    SimpleTower(TowerNum).HighSpeedAirFlowRate = tmpDesignAirFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Design Air Flow Rate [m3/s]",
                                           SimpleTower(TowerNum).HighSpeedAirFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Design Air Flow Rate [m3/s]",
                                           SimpleTower(TowerNum).HighSpeedAirFlowRate);
                    }
                } else { // Hard-sized with sizing data
                    Real64 DesignAirFlowRateUser(0.0);
                    DesignAirFlowRateUser = SimpleTower(TowerNum).HighSpeedAirFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Design Air Flow Rate [m3/s]",
                                           tmpDesignAirFlowRate,
                                           "User-Specified Design Air Flow Rate [m3/s]",
                                           DesignAirFlowRateUser);
                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tmpDesignAirFlowRate - DesignAirFlowRateUser) / DesignAirFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeVSMerkelTower: Potential issue with equipment sizing for " + SimpleTower(TowerNum).Name);
                                ShowContinueError("User-Specified Design Air Flow Rate of " + General::RoundSigDigits(DesignAirFlowRateUser, 2) +
                                                  " [m3/s]");
                                ShowContinueError("differs from Design Air Flow Rate of " + General::RoundSigDigits(tmpDesignAirFlowRate, 2) +
                                                  " [m3/s]");
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                        tmpDesignAirFlowRate = DesignAirFlowRateUser;
                    }
                }
            }
            tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor;

            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized) {
                    SimpleTower(TowerNum).FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Design Free Convection Regime Air Flow Rate [m3/s]",
                                           SimpleTower(TowerNum).FreeConvAirFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Design Free Convection Regime Air Flow Rate [m3/s]",
                                           SimpleTower(TowerNum).FreeConvAirFlowRate);
                    }
                } else { // Hard-sized with sizing data
                    Real64 FreeConvAirFlowUser(0.0);
                    FreeConvAirFlowUser = SimpleTower(TowerNum).FreeConvAirFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Design Free Convection Regime Air Flow Rate [m3/s]",
                                           tmpFreeConvAirFlowRate,
                                           "User-Specified Design Free Convection Regime Air Flow Rate [m3/s]",
                                           FreeConvAirFlowUser);
                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tmpFreeConvAirFlowRate - FreeConvAirFlowUser) / FreeConvAirFlowUser) > DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeVSMerkelTower: Potential issue with equipment sizing for " + SimpleTower(TowerNum).Name);
                                ShowContinueError("User-Specified Design Free Convection Regime Air Flow Rate of " +
                                                  General::RoundSigDigits(FreeConvAirFlowUser, 2) + " [m3/s]");
                                ShowContinueError("differs from Design Free Convection Regime Air Flow Rate of " +
                                                  General::RoundSigDigits(tmpFreeConvAirFlowRate, 2) + " [m3/s]");
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                        tmpFreeConvAirFlowRate = FreeConvAirFlowUser;
                    }
                }
            }

            // now calcuate UA values from nominal capacities and flow rates
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (PltSizCondNum > 0) { // user has a plant sizing object
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp;
                } else {                        // probably no plant sizing object
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp; // 35.0; // design condition
                }
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName);

                // full speed fan tower UA
                Par(1) = tmpNomTowerCap * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                Par(2) = double(TowerNum);
                Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par(4) = tmpDesignAirFlowRate;         // design air volume flow rate
                Par(5) = Cp;
                UA0 = 0.0001 * Par(1); // Assume deltaT = 10000K (limit)
                UA1 = Par(1);          // Assume deltaT = 1K

                SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0;
                SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6;
                SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Iteration limit exceeded in calculating tower UA");
                    ShowFatalError("calculating cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                } else if (SolFla == -2) {
                    ShowSevereError("Bad starting values for UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                }
                SimpleTower(TowerNum).HighSpeedTowerUA = UA;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                       SimpleTower(TowerNum).HighSpeedTowerUA);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                       SimpleTower(TowerNum).HighSpeedTowerUA);
                }
                // free convection tower UA
                Par(1) = tmpTowerFreeConvNomCap * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                Par(2) = double(TowerNum);
                Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par(4) = tmpFreeConvAirFlowRate;       // design air volume flow rate
                Par(5) = Cp;
                UA0 = 0.0001 * Par(1); // Assume deltaT = 10000K (limit)
                UA0 = max(UA0, 1.0);   // limit to 1.0
                UA1 = Par(1);          // Assume deltaT = 1K

                SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0;
                SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6;
                SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowSevereError("Iteration limit exceeded in calculating tower free convection UA");
                    ShowFatalError("calculating cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                } else if (SolFla == -2) {
                    ShowSevereError("Bad starting values for UA");
                    ShowFatalError("Autosizing of cooling tower UA failed for free convection tower " + SimpleTower(TowerNum).Name);
                }
                SimpleTower(TowerNum).FreeConvTowerUA = UA;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                       SimpleTower(TowerNum).FreeConvTowerUA);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                       SimpleTower(TowerNum).FreeConvTowerUA);
                }
            }
        } else if (SimpleTower(TowerNum).PerformanceInputMethod_Num == PIM_UFactor) {
            // UA input method

            if (SimpleTower(TowerNum).DesignWaterFlowRateWasAutoSized) { // get from plant sizing
                // UA input method using plant sizing for flow rate, whereas Nominal capacity method uses scalable sizing factor per cap
                if (PltSizCondNum > 0) {
                    if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        tmpDesignWaterFlowRate = DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate * SimpleTower(TowerNum).SizFac;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            SimpleTower(TowerNum).DesignWaterFlowRate = tmpDesignWaterFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                   SimpleTower(TowerNum).Name,
                                                   "Design Water Flow Rate [m3/s]",
                                                   SimpleTower(TowerNum).DesignWaterFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                   SimpleTower(TowerNum).Name,
                                                   "Initial Design Water Flow Rate [m3/s]",
                                                   SimpleTower(TowerNum).DesignWaterFlowRate);
                            }
                        }
                    } else {
                        tmpDesignWaterFlowRate = 0.0;
                    }

                } else {
                    if (!SimpleTower(TowerNum).TowerInletCondsAutoSize) {
                        if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                SimpleTower(TowerNum).DesignWaterFlowRate = tmpDesignWaterFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Design Water Flow Rate [m3/s]",
                                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                                }
                                if (DataPlant::PlantFirstSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Initial Design Water Flow Rate [m3/s]",
                                                       SimpleTower(TowerNum).DesignWaterFlowRate);
                                }
                            }
                        } else {
                            tmpDesignWaterFlowRate = 0.0;
                        }
                    } else {
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            ShowSevereError("Autosizing error for cooling tower object = " + SimpleTower(TowerNum).Name);
                            ShowFatalError("Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                        }
                    }
                }
            }
            PlantUtilities::RegisterPlantCompDesignFlow(SimpleTower(TowerNum).WaterInletNodeNum, tmpDesignWaterFlowRate);

            if (SimpleTower(TowerNum).HighSpeedTowerUAWasAutoSized) {
                // get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
                if (PltSizCondNum > 0) {
                    if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                   DesTowerExitWaterTemp,
                                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                   RoutineName);
                        DesTowerLoad = rho * Cp * DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate * DesTowerWaterDeltaT * SimpleTower(TowerNum).SizFac;
                        tmpNomTowerCap = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                   SimpleTower(TowerNum).Name,
                                                   "Nominal Capacity [W]",
                                                   SimpleTower(TowerNum).TowerNominalCapacity);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                   SimpleTower(TowerNum).Name,
                                                   "Initial Nominal Capacity [W]",
                                                   SimpleTower(TowerNum).TowerNominalCapacity);
                            }
                        }
                    } else {
                        tmpNomTowerCap = 0.0;
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                   SimpleTower(TowerNum).Name,
                                                   "Nominal Capacity [W]",
                                                   SimpleTower(TowerNum).TowerNominalCapacity);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                   SimpleTower(TowerNum).Name,
                                                   "Initial Nominal Capacity [W]",
                                                   SimpleTower(TowerNum).TowerNominalCapacity);
                            }
                        }
                    }
                } else {
                    if (!SimpleTower(TowerNum).TowerInletCondsAutoSize) {
                        if (SimpleTower(TowerNum).DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                   DesTowerExitWaterTemp,
                                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                   RoutineName);
                            Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                       DesTowerExitWaterTemp,
                                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                       RoutineName);
                            DesTowerLoad = rho * Cp * SimpleTower(TowerNum).DesignWaterFlowRate * DesTowerWaterDeltaT * SimpleTower(TowerNum).SizFac;
                            tmpNomTowerCap = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                                if (DataPlant::PlantFirstSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Initial Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                            }
                        } else {
                            tmpNomTowerCap = 0.0;
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                                if (DataPlant::PlantFirstSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Initial Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                            }
                        }
                    } else {
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            ShowSevereError("Autosizing error for cooling tower object = " + SimpleTower(TowerNum).Name);
                            ShowFatalError("Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                        }
                    }
                }
                if (SimpleTower(TowerNum).TowerFreeConvNomCapWasAutoSized) {
                    tmpTowerFreeConvNomCap = tmpNomTowerCap * SimpleTower(TowerNum).TowerFreeConvNomCapSizingFactor;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Free Convection Nominal Capacity [W]",
                                               SimpleTower(TowerNum).TowerFreeConvNomCap);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial Free Convection Nominal Capacity [W]",
                                               SimpleTower(TowerNum).TowerFreeConvNomCap);
                        }
                    }
                }
                if (SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized) {
                    if (SimpleTower(TowerNum).DefaultedDesignAirFlowScalingFactor) {
                        tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap * (101325.0 / DataEnvironment::StdBaroPress);
                    } else {
                        tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap;
                    }
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).HighSpeedAirFlowRate = tmpDesignAirFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Design Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).HighSpeedAirFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial Design Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).HighSpeedAirFlowRate);
                        }
                    }
                }
                if (SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized) {
                    tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Free Convection Regime Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).FreeConvAirFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial Free Convection Regime Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).FreeConvAirFlowRate);
                        }
                    }
                }
                // now calcuate UA values from nominal capacities and flow rates
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);
                    // full speed fan tower UA
                    Par(1) = tmpNomTowerCap * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    Par(2) = double(TowerNum);
                    Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par(4) = tmpDesignAirFlowRate;         // design air volume flow rate
                    Par(5) = Cp;
                    UA0 = 0.0001 * Par(1); // Assume deltaT = 10000K (limit)
                    UA1 = Par(1);          // Assume deltaT = 1K
                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp;
                    SimpleTower(TowerNum).AirTemp = SimpleTower(TowerNum).DesInletAirDBTemp;    // 35.0;
                    SimpleTower(TowerNum).AirWetBulb = SimpleTower(TowerNum).DesInletAirWBTemp; // 25.6;
                    SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                    SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                    General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError("Iteration limit exceeded in calculating tower UA");
                        ShowFatalError("calculating cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    } else if (SolFla == -2) {
                        ShowSevereError("Bad starting values for UA");
                        ShowFatalError("Autosizing of cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    }
                    SimpleTower(TowerNum).HighSpeedTowerUA = UA;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).HighSpeedTowerUA);
                    }
                    // free convection tower UA
                    Par(1) = tmpTowerFreeConvNomCap * SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    Par(2) = double(TowerNum);
                    Par(3) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                    Par(4) = tmpFreeConvAirFlowRate;       // design air volume flow rate
                    Par(5) = Cp;
                    UA0 = 0.0001 * Par(1); // Assume deltaT = 10000K (limit)
                    UA1 = Par(1);          // Assume deltaT = 1K
                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp;
                    SimpleTower(TowerNum).AirTemp = DesTowerInletAirDBTemp;    // 35.0;
                    SimpleTower(TowerNum).AirWetBulb = DesTowerInletAirWBTemp; // 25.6;
                    SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                    SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                    General::SolveRoot(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par);
                    if (SolFla == -1) {
                        ShowSevereError("Iteration limit exceeded in calculating tower free convection UA");
                        ShowFatalError("calculating cooling tower UA failed for tower " + SimpleTower(TowerNum).Name);
                    } else if (SolFla == -2) {
                        ShowSevereError("Bad starting values for UA");
                        ShowFatalError("Autosizing of cooling tower UA failed for free convection tower " + SimpleTower(TowerNum).Name);
                    }
                    SimpleTower(TowerNum).LowSpeedTowerUA = UA;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).FreeConvTowerUA);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                           SimpleTower(TowerNum).FreeConvTowerUA);
                    }
                }

            } else { // full speed UA given

                if (SimpleTower(TowerNum).FreeConvTowerUAWasAutoSized) { // determine from scalable sizing factor
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).FreeConvTowerUA =
                            SimpleTower(TowerNum).HighSpeedTowerUA * SimpleTower(TowerNum).FreeConvTowerUASizingFactor;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                               SimpleTower(TowerNum).FreeConvTowerUA);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]",
                                               SimpleTower(TowerNum).FreeConvTowerUA);
                        }
                    }
                }

                if (SimpleTower(TowerNum).HighSpeedAirFlowRateWasAutoSized) { // given UA but not air flow rate
                    // need an air flow rate to find capacity from UA but flow rate is scaled off capacity
                    // get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
                    if (PltSizCondNum > 0) {
                        if (DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                   DesTowerExitWaterTemp,
                                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                   RoutineName);
                            Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                       DesTowerExitWaterTemp,
                                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                                       RoutineName);
                            DesTowerLoad = rho * Cp * DataSizing::PlantSizData(PltSizCondNum).DesVolFlowRate * DesTowerWaterDeltaT;
                            tmpNomTowerCap = DesTowerLoad / SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                                if (DataPlant::PlantFirstSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Initial Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                            }
                        } else {
                            tmpNomTowerCap = rho = Cp = 0.0; // rho and Cp added: Used below
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                                if (DataPlant::PlantFirstSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                                       SimpleTower(TowerNum).Name,
                                                       "Initial Nominal Capacity [W]",
                                                       SimpleTower(TowerNum).TowerNominalCapacity);
                                }
                            }
                        }

                    } else {
                        tmpNomTowerCap = 0.0; // Suppress uninitialized warnings
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            ShowSevereError("Autosizing error for cooling tower object = " + SimpleTower(TowerNum).Name);
                            ShowFatalError("Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.");
                        }
                    }

                    if (SimpleTower(TowerNum).DefaultedDesignAirFlowScalingFactor) {
                        tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap * (101325.0 / DataEnvironment::StdBaroPress);
                    } else {
                        tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower(TowerNum).DesignAirFlowPerUnitNomCap;
                    }
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).HighSpeedAirFlowRate = tmpDesignAirFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Design Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).HighSpeedAirFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial Design Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).HighSpeedAirFlowRate);
                        }
                    }

                } else { // UA and Air flow rate given, so find Nominal Cap from running model

                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                           DesTowerExitWaterTemp,
                                                            DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                           RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                               DesTowerExitWaterTemp,
                                                                DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                               RoutineName);

                    SimpleTower(TowerNum).WaterTemp = DesTowerInletWaterTemp;
                    SimpleTower(TowerNum).AirTemp = DesTowerInletAirDBTemp;    // 35.0;
                    SimpleTower(TowerNum).AirWetBulb = DesTowerInletAirWBTemp; // 25.6;
                    SimpleTower(TowerNum).AirPress = DataEnvironment::StdBaroPress;
                    SimpleTower(TowerNum).AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirWetBulb, SimpleTower(TowerNum).AirPress);
                    SimSimpleTower(TowerNum,
                                   rho * tmpDesignWaterFlowRate,
                                   SimpleTower(TowerNum).HighSpeedAirFlowRate,
                                   SimpleTower(TowerNum).HighSpeedTowerUA,
                                   OutWaterTemp);
                    tmpNomTowerCap = Cp * rho * tmpDesignWaterFlowRate * (SimpleTower(TowerNum).WaterTemp - OutWaterTemp);
                    tmpNomTowerCap /= SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).TowerNominalCapacity = tmpNomTowerCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Nominal Capacity [W]",
                                               SimpleTower(TowerNum).TowerNominalCapacity);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial Nominal Capacity [W]",
                                               SimpleTower(TowerNum).TowerNominalCapacity);
                        }
                    }

                } // both UA and air flow rate given

                if (SimpleTower(TowerNum).FreeConvAirFlowRateWasAutoSized) {
                    tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower(TowerNum).FreeConvAirFlowRateSizingFactor;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        SimpleTower(TowerNum).FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Free Convection Regime Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).FreeConvAirFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                               SimpleTower(TowerNum).Name,
                                               "Initial Free Convection Regime Air Flow Rate [m3/s]",
                                               SimpleTower(TowerNum).FreeConvAirFlowRate);
                        }
                    }
                }

                SimSimpleTower(TowerNum, rho * tmpDesignWaterFlowRate, tmpFreeConvAirFlowRate, SimpleTower(TowerNum).FreeConvTowerUA, OutWaterTemp);
                tmpTowerFreeConvNomCap = Cp * rho * tmpDesignWaterFlowRate * (SimpleTower(TowerNum).WaterTemp - OutWaterTemp);
                tmpTowerFreeConvNomCap /= SimpleTower(TowerNum).HeatRejectCapNomCapSizingRatio;
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    SimpleTower(TowerNum).TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Free Convection Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerFreeConvNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                           SimpleTower(TowerNum).Name,
                                           "Initial Free Convection Nominal Capacity [W]",
                                           SimpleTower(TowerNum).TowerFreeConvNomCap);
                    }
                }
            }
        }

        tmpHighSpeedFanPower = tmpNomTowerCap * SimpleTower(TowerNum).DesignFanPowerPerUnitNomCap;
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            if (SimpleTower(TowerNum).HighSpeedFanPowerWasAutoSized) {

                SimpleTower(TowerNum).HighSpeedFanPower = tmpHighSpeedFanPower;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        SimpleTower(TowerNum).TowerType, SimpleTower(TowerNum).Name, "Design Fan Power [W]", SimpleTower(TowerNum).HighSpeedFanPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Initial Design Fan Power [W]",
                                       SimpleTower(TowerNum).HighSpeedFanPower);
                }
            } else { // Hard-sized with sizing data
                Real64 HighSpeedFanPowerUser(0.0);
                HighSpeedFanPowerUser = SimpleTower(TowerNum).HighSpeedAirFlowRate;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(SimpleTower(TowerNum).TowerType,
                                       SimpleTower(TowerNum).Name,
                                       "Design Fan Power [W]",
                                       tmpHighSpeedFanPower,
                                       "User-Specified Design Fan Power [W]",
                                       HighSpeedFanPowerUser);
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(tmpHighSpeedFanPower - HighSpeedFanPowerUser) / HighSpeedFanPowerUser) > DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("SizeVSMerkelTower: Potential issue with equipment sizing for " + SimpleTower(TowerNum).Name);
                            ShowContinueError("User-Specified Design Fan Power of " + General::RoundSigDigits(HighSpeedFanPowerUser, 2) + " [W]");
                            ShowContinueError("differs from Design Fan Power of " + General::RoundSigDigits(tmpHighSpeedFanPower, 2) + " [W]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    } // namespace CondenserLoopTowers

    void CalcSingleSpeedTower(int &TowerNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       Aug. 2008, T Hong, Added fluid bypass for single speed cooling tower
        //                      The OutletWaterTemp from SimSimpleTower can be lower than 0 degreeC
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
        static std::string const RoutineName("CalcSingleSpeedTower");
        int const MaxIteration(100); // Maximum fluid bypass iteration calculations
        static std::string const MaxItChar("100");
        Real64 const BypassFractionThreshold(0.01); // Threshold to stop bypass iteration
        Real64 const OWTLowerLimit(0.0);            // The limit of tower exit fluid temperature used in the fluid bypass
        //  calculation to avoid fluid freezing. For water, it is 0 degreeC,
        //  for glycols, it can be much lower. The fluid type is stored at the loop.
        //  Current choices are Water and Steam, needs to expand for glycols

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirFlowRate = 0.0;
        Real64 UAdesign = 0.0; // UA value at design conditions (entered by user or calculated)
        Real64 OutletWaterTempOFF;
        Real64 FanModeFrac = 0.0;
        Real64 FanPowerOn;
        Real64 CpWater;
        Real64 TempSetPoint = 0.0;

        // Added variables for fluid bypass
        int NumIteration;
        int CapacityControl;    // Capacity Control (0 - FanCycling, 1 - FluidBypass)
        int BypassFlag;         // Flag indicator for fluid bypass (0 - no bypass, 1 - bypass)
        Real64 BypassFraction;  // Fluid bypass fraction
        Real64 BypassFraction2; // Fluid bypass fraction
        Real64 BypassFractionPrev;
        Real64 OutletWaterTempPrev;

        // Added variables for multicell
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;
        int NumCellMin(0);
        int NumCellMax(0);
        int NumCellOn(0);
        Real64 WaterMassFlowRatePerCell;
        bool IncrNumCellFlag; // determine if yes or no we increase the number of cells

        int LoopNum;
        int LoopSideNum;

        // set inlet and outlet nodes
        SimpleTower(TowerNum).Qactual = 0.0;
        SimpleTower(TowerNum).FanPower = 0.0;
        SimpleTower(TowerNum).OutletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
        LoopNum = SimpleTower(TowerNum).LoopNum;
        LoopSideNum = SimpleTower(TowerNum).LoopSideNum;

        Real64 FreeConvTowerUA = SimpleTower(TowerNum).FreeConvTowerUA;
        Real64 HighSpeedTowerUA = SimpleTower(TowerNum).HighSpeedTowerUA;

        // water temperature setpoint
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if (SimpleTower(TowerNum).SetpointIsOnOutlet) {
                    TempSetPoint = DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).TempSetPoint;
                } else {
                    TempSetPoint = DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if (SimpleTower(TowerNum).SetpointIsOnOutlet) {
                    TempSetPoint = DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).TempSetPointHi;
                } else {
                    TempSetPoint = DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).TempSetPointHi;
                }
            }
        }

        // If there is a fault of condenser SWT Sensor (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyCondenserSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyCondenserSWTIndex;
            Real64 TowerOutletTemp_ff = TempSetPoint;

            // calculate the sensor offset using fault information
            SimpleTower(TowerNum).FaultyCondenserSWTOffset = FaultsManager::FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempSetPoint
            TempSetPoint = TowerOutletTemp_ff - SimpleTower(TowerNum).FaultyCondenserSWTOffset;
        }

        // If there is a fault of cooling tower fouling (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyTowerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyTowerFoulingIndex;
            Real64 FreeConvTowerUA_ff = SimpleTower(TowerNum).FreeConvTowerUA;
            Real64 HighSpeedTowerUA_ff = SimpleTower(TowerNum).HighSpeedTowerUA;

            // calculate the Faulty Tower Fouling Factor using fault information
            SimpleTower(TowerNum).FaultyTowerFoulingFactor = FaultsManager::FaultsTowerFouling(FaultIndex).CalFaultyTowerFoulingFactor();

            // update the tower UA values at faulty cases
            FreeConvTowerUA = FreeConvTowerUA_ff * SimpleTower(TowerNum).FaultyTowerFoulingFactor;
            HighSpeedTowerUA = HighSpeedTowerUA_ff * SimpleTower(TowerNum).FaultyTowerFoulingFactor;
        }

        // Added for fluid bypass. First assume no fluid bypass
        BypassFlag = 0;
        BypassFraction2 = 0.0;
        SimpleTower(TowerNum).BypassFraction = 0.0;
        CapacityControl = SimpleTower(TowerNum).CapacityControl;

        // Added for multi-cell. Determine the number of cells operating
        if (SimpleTower(TowerNum).DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MinFracFlowRate / SimpleTower(TowerNum).NumCell;
            WaterMassFlowRatePerCellMax =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MaxFracFlowRate / SimpleTower(TowerNum).NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), SimpleTower(TowerNum).NumCell);
            NumCellMax = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), SimpleTower(TowerNum).NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (SimpleTower(TowerNum).CellCtrl_Num == CellCtrl_MinCell) {
            NumCellOn = NumCellMin;
        } else {
            NumCellOn = NumCellMax;
        }

        SimpleTower(TowerNum).NumCellOn = NumCellOn;
        WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;

        // Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.

        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (SimpleTower(TowerNum).WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            // for multiple cells, we assume that it's a common basin
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            return;
        }

        IncrNumCellFlag = true; // set value to true to enter in the loop

        while (IncrNumCellFlag) {
            IncrNumCellFlag = false;

            //   Initialize local variables to the free convection design values
            UAdesign = FreeConvTowerUA / SimpleTower(TowerNum).NumCell;
            AirFlowRate = SimpleTower(TowerNum).FreeConvAirFlowRate / SimpleTower(TowerNum).NumCell;
            OutletWaterTempOFF = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempOFF;
            FanModeFrac = 0.0;

            SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTempOFF);

            //   Assume Setpoint was met using free convection regime (pump ON and fan OFF)
            SimpleTower(TowerNum).FanPower = 0.0;
            SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempOFF;

            if (OutletWaterTempOFF > TempSetPoint) {
                //     Setpoint was not met (or free conv. not used), turn on cooling tower fan
                UAdesign = HighSpeedTowerUA / SimpleTower(TowerNum).NumCell;
                AirFlowRate = SimpleTower(TowerNum).HighSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;

                // The fan power is for all cells operating
                FanPowerOn = SimpleTower(TowerNum).HighSpeedFanPower * NumCellOn / SimpleTower(TowerNum).NumCell;

                SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, SimpleTower(TowerNum).OutletWaterTemp);

                if (SimpleTower(TowerNum).OutletWaterTemp <= TempSetPoint) {
                    if (CapacityControl == CapacityControl_FanCycling || SimpleTower(TowerNum).OutletWaterTemp <= OWTLowerLimit) {
                        //           Setpoint was met with pump ON and fan ON, calculate run-time fraction
                        FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (SimpleTower(TowerNum).OutletWaterTemp - OutletWaterTempOFF);
                        SimpleTower(TowerNum).FanPower = FanModeFrac * FanPowerOn;
                        SimpleTower(TowerNum).OutletWaterTemp = TempSetPoint;
                    } else {
                        // FluidBypass, fan runs at full speed for the entire time step
                        FanModeFrac = 1.0;
                        SimpleTower(TowerNum).FanPower = FanPowerOn;
                        BypassFlag = 1;
                    }
                } else {
                    //         Setpoint was not met, cooling tower ran at full capacity
                    FanModeFrac = 1.0;
                    SimpleTower(TowerNum).FanPower = FanPowerOn;
                    // if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
                    if (NumCellOn < SimpleTower(TowerNum).NumCell && (SimpleTower(TowerNum).WaterMassFlowRate / (NumCellOn + 1)) >= WaterMassFlowRatePerCellMin) {
                        ++NumCellOn;
                        WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;
                        IncrNumCellFlag = true;
                    }
                }
            } else if (OutletWaterTempOFF < TempSetPoint) {
                // Need to bypass in free convection cooling mode if bypass is allowed
                if (CapacityControl == CapacityControl_FluidBypass) {
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
            if (SimpleTower(TowerNum).InletWaterTemp <= TempSetPoint) {
                SimpleTower(TowerNum).FanPower = 0.0;
                SimpleTower(TowerNum).BypassFraction = 1.0;
                SimpleTower(TowerNum).OutletWaterTemp = SimpleTower(TowerNum).InletWaterTemp;
            } else {
                if (std::abs(SimpleTower(TowerNum).InletWaterTemp - SimpleTower(TowerNum).OutletWaterTemp) <= 0.01) {
                    // Outlet temp is close enough to inlet temp, assume 100% bypass, tower fan off
                    SimpleTower(TowerNum).BypassFraction = 1.0;
                    SimpleTower(TowerNum).FanPower = 0.0;
                } else {
                    BypassFraction = (TempSetPoint - SimpleTower(TowerNum).OutletWaterTemp) / (SimpleTower(TowerNum).InletWaterTemp - SimpleTower(TowerNum).OutletWaterTemp);
                    if (BypassFraction > 1.0 || BypassFraction < 0.0) {
                        // Bypass cannot meet setpoint, assume no bypass
                        SimpleTower(TowerNum).BypassFraction = 0.0;
                    } else {
                        NumIteration = 0;
                        BypassFractionPrev = BypassFraction;
                        OutletWaterTempPrev = SimpleTower(TowerNum).OutletWaterTemp;
                        while (NumIteration < MaxIteration) {
                            ++NumIteration;
                            // need to iterate for the new OutletWaterTemp while bypassing tower water
                            SimSimpleTower(TowerNum, WaterMassFlowRatePerCell * (1.0 - BypassFraction), AirFlowRate, UAdesign, SimpleTower(TowerNum).OutletWaterTemp);
                            // Calc new BypassFraction based on the new OutletWaterTemp
                            if (std::abs(SimpleTower(TowerNum).OutletWaterTemp - OWTLowerLimit) <= 0.01) {
                                BypassFraction2 = BypassFraction;
                                break;
                            } else if (SimpleTower(TowerNum).OutletWaterTemp < OWTLowerLimit) {
                                // Set OutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
                                BypassFraction2 = BypassFractionPrev - (BypassFractionPrev - BypassFraction) * (OutletWaterTempPrev - OWTLowerLimit) /
                                                                           (OutletWaterTempPrev - SimpleTower(TowerNum).OutletWaterTemp);
                                SimSimpleTower(TowerNum, WaterMassFlowRatePerCell * (1.0 - BypassFraction2), AirFlowRate, UAdesign, SimpleTower(TowerNum).OutletWaterTemp);
                                if (SimpleTower(TowerNum).OutletWaterTemp < OWTLowerLimit) {
                                    // Use previous iteraction values
                                    BypassFraction2 = BypassFractionPrev;
                                    SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempPrev;
                                }
                                break;
                            } else {
                                BypassFraction2 = (TempSetPoint - SimpleTower(TowerNum).OutletWaterTemp) / (SimpleTower(TowerNum).InletWaterTemp - SimpleTower(TowerNum).OutletWaterTemp);
                            }

                            // Compare two BypassFraction to determine when to stop
                            if (std::abs(BypassFraction2 - BypassFraction) <= BypassFractionThreshold) break;
                            BypassFractionPrev = BypassFraction;
                            OutletWaterTempPrev = SimpleTower(TowerNum).OutletWaterTemp;
                            BypassFraction = BypassFraction2;
                        }
                        if (NumIteration > MaxIteration) {
                            ShowWarningError("Cooling tower fluid bypass iteration exceeds maximum limit of " + MaxItChar + " for " +
                                             SimpleTower(TowerNum).Name);
                        }
                        SimpleTower(TowerNum).BypassFraction = BypassFraction2;
                        // may not meet TempSetPoint due to limit of tower outlet temp to OWTLowerLimit
                        SimpleTower(TowerNum).OutletWaterTemp = (1.0 - BypassFraction2) * SimpleTower(TowerNum).OutletWaterTemp + BypassFraction2 * SimpleTower(TowerNum).InletWaterTemp;
                    }
                }
            }
        }

        // output the fraction of the time step the fan is ON
        SimpleTower(TowerNum).FanCyclingRatio = FanModeFrac;
        // output the number of cells operating
        SimpleTower(TowerNum).NumCellOn = NumCellOn;
        // Should this be water inlet node num?????
        CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                         DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp,
                                                         DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                        RoutineName);

        SimpleTower(TowerNum).Qactual = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);
        SimpleTower(TowerNum).__AirFlowRateRatio = (AirFlowRate * SimpleTower(TowerNum).NumCell) / SimpleTower(TowerNum).HighSpeedAirFlowRate;
    }

    void CalcTwoSpeedTower(int &TowerNum)
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
        static std::string const RoutineName("CalcTwoSpeedTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirFlowRate = 0.0;
        Real64 UAdesign; // UA value at design conditions (entered by user) [W/C]
        Real64 OutletWaterTempOFF;
        Real64 OutletWaterTemp1stStage;
        Real64 OutletWaterTemp2ndStage;
        Real64 FanModeFrac = 0.0;
        Real64 FanPowerLow;
        Real64 FanPowerHigh;
        Real64 CpWater;
        Real64 TempSetPoint = 0.0;

        int LoopNum;
        int LoopSideNum;

        int SpeedSel(0);

        // Added variables for multicell
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;
        int NumCellMin(0);
        int NumCellMax(0);
        int NumCellOn(0);
        Real64 WaterMassFlowRatePerCell;
        bool IncrNumCellFlag; // determine if yes or no we increase the number of cells

        // init
        SimpleTower(TowerNum).Qactual = 0.0;
        SimpleTower(TowerNum).FanPower = 0.0;
        SimpleTower(TowerNum).OutletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
        LoopNum = SimpleTower(TowerNum).LoopNum;
        LoopSideNum = SimpleTower(TowerNum).LoopSideNum;

        Real64 FreeConvTowerUA = SimpleTower(TowerNum).FreeConvTowerUA;
        Real64 HighSpeedTowerUA = SimpleTower(TowerNum).HighSpeedTowerUA;

        // water temperature setpoint
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if (SimpleTower(TowerNum).SetpointIsOnOutlet) {
                    TempSetPoint = DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).TempSetPoint;
                } else {
                    TempSetPoint = DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if (SimpleTower(TowerNum).SetpointIsOnOutlet) {
                    TempSetPoint = DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).TempSetPointHi;
                } else {
                    TempSetPoint = DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).TempSetPointHi;
                }
            }
        }

        // If there is a fault of condenser SWT Sensor (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyCondenserSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyCondenserSWTIndex;
            Real64 TowerOutletTemp_ff = TempSetPoint;

            // calculate the sensor offset using fault information
            SimpleTower(TowerNum).FaultyCondenserSWTOffset = FaultsManager::FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempSetPoint
            TempSetPoint = TowerOutletTemp_ff - SimpleTower(TowerNum).FaultyCondenserSWTOffset;
        }

        // If there is a fault of cooling tower fouling (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyTowerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyTowerFoulingIndex;
            Real64 FreeConvTowerUA_ff = SimpleTower(TowerNum).FreeConvTowerUA;
            Real64 HighSpeedTowerUA_ff = SimpleTower(TowerNum).HighSpeedTowerUA;

            // calculate the Faulty Tower Fouling Factor using fault information
            SimpleTower(TowerNum).FaultyTowerFoulingFactor = FaultsManager::FaultsTowerFouling(FaultIndex).CalFaultyTowerFoulingFactor();

            // update the tower UA values at faulty cases
            FreeConvTowerUA = FreeConvTowerUA_ff * SimpleTower(TowerNum).FaultyTowerFoulingFactor;
            HighSpeedTowerUA = HighSpeedTowerUA_ff * SimpleTower(TowerNum).FaultyTowerFoulingFactor;
        }

        // Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.
        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) return;
        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (SimpleTower(TowerNum).WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            return;
        }

        // Added for multi-cell. Determine the number of cells operating
        if (SimpleTower(TowerNum).DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MinFracFlowRate / SimpleTower(TowerNum).NumCell;
            WaterMassFlowRatePerCellMax =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MaxFracFlowRate / SimpleTower(TowerNum).NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), SimpleTower(TowerNum).NumCell);
            NumCellMax = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), SimpleTower(TowerNum).NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (SimpleTower(TowerNum).CellCtrl_Num == CellCtrl_MinCell) {
            NumCellOn = NumCellMin;
        } else {
            NumCellOn = NumCellMax;
        }

        SimpleTower(TowerNum).NumCellOn = NumCellOn;
        WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;

        IncrNumCellFlag = true;

        while (IncrNumCellFlag) {
            IncrNumCellFlag = false;

            // set local variable for tower
            UAdesign = FreeConvTowerUA / SimpleTower(TowerNum).NumCell; // where is NumCellOn?
            AirFlowRate = SimpleTower(TowerNum).FreeConvAirFlowRate / SimpleTower(TowerNum).NumCell;
            OutletWaterTempOFF = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).WaterMassFlowRate = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).MassFlowRate;
            OutletWaterTemp1stStage = SimpleTower(TowerNum).OutletWaterTemp;
            OutletWaterTemp2ndStage = SimpleTower(TowerNum).OutletWaterTemp;
            FanModeFrac = 0.0;

            SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTempOFF);

            //     Setpoint was met using free convection regime (pump ON and fan OFF)
            SimpleTower(TowerNum).FanPower = 0.0;
            SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempOFF;
            SpeedSel = 0;

            if (OutletWaterTempOFF > TempSetPoint) {
                //     Setpoint was not met (or free conv. not used),turn on cooling tower 1st stage fan
                UAdesign = SimpleTower(TowerNum).LowSpeedTowerUA / SimpleTower(TowerNum).NumCell;
                AirFlowRate = SimpleTower(TowerNum).LowSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;
                FanPowerLow = SimpleTower(TowerNum).LowSpeedFanPower * NumCellOn / SimpleTower(TowerNum).NumCell;

                SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp1stStage);

                if (OutletWaterTemp1stStage <= TempSetPoint) {
                    //         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
                    FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (OutletWaterTemp1stStage - OutletWaterTempOFF);
                    SimpleTower(TowerNum).FanPower = FanModeFrac * FanPowerLow;
                    SimpleTower(TowerNum).OutletWaterTemp = TempSetPoint;
                    SimpleTower(TowerNum).Qactual *= FanModeFrac;
                    SpeedSel = 1;
                } else {
                    //         Setpoint was not met, turn on cooling tower 2nd stage fan
                    UAdesign = HighSpeedTowerUA / SimpleTower(TowerNum).NumCell;
                    AirFlowRate = SimpleTower(TowerNum).HighSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;
                    FanPowerHigh = SimpleTower(TowerNum).HighSpeedFanPower * NumCellOn / SimpleTower(TowerNum).NumCell;

                    SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp2ndStage);

                    if ((OutletWaterTemp2ndStage <= TempSetPoint) && UAdesign > 0.0) {
                        //           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
                        FanModeFrac = (TempSetPoint - OutletWaterTemp1stStage) / (OutletWaterTemp2ndStage - OutletWaterTemp1stStage);
                        SimpleTower(TowerNum).FanPower = (FanModeFrac * FanPowerHigh) + (1.0 - FanModeFrac) * FanPowerLow;
                        SimpleTower(TowerNum).OutletWaterTemp = TempSetPoint;
                        SpeedSel = 2;
                    } else {
                        //           Setpoint was not met, cooling tower ran at full capacity
                        SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTemp2ndStage;
                        SimpleTower(TowerNum).FanPower = FanPowerHigh;
                        SpeedSel = 2;
                        FanModeFrac = 1.0;
                        // if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
                        if (NumCellOn < SimpleTower(TowerNum).NumCell && (SimpleTower(TowerNum).WaterMassFlowRate / (NumCellOn + 1)) >= WaterMassFlowRatePerCellMin) {
                            ++NumCellOn;
                            WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;
                            IncrNumCellFlag = true;
                        }
                    }
                }
            }
        }

        // output the fraction of the time step the fan is ON
        SimpleTower(TowerNum).FanCyclingRatio = FanModeFrac;
        SimpleTower(TowerNum).SpeedSelected = SpeedSel;
        SimpleTower(TowerNum).NumCellOn = NumCellOn;

        CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                         DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp,
                                                         DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                        RoutineName);
        SimpleTower(TowerNum).Qactual = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);
        SimpleTower(TowerNum).__AirFlowRateRatio = (AirFlowRate * SimpleTower(TowerNum).NumCell) / SimpleTower(TowerNum).HighSpeedAirFlowRate;
    }

    void CalcMerkelVariableSpeedTower(int const TowerNum, Real64 &MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.Griffith
        //       DATE WRITTEN   August 2013
        //       MODIFIED       Jun. 2016, R Zhang, Applied the condenser supply water temperature sensor fault model
        //                      Jul. 2016, R Zhang, Applied the cooling tower fouling fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate varialble speed tower model using Merkel's theory with UA adjustments developed by Scheier

        // METHODOLOGY EMPLOYED:
        // Find a fan speed that operates the tower to meet MyLoad

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const DesignWetBulb(25.56); // tower outdoor air entering wetbulb for design [C]
        int const MaxIte(500);             // Maximum number of iterations for solver
        Real64 const Acc(1.e-3);           // Accuracy of solver result
        static std::string const RoutineName("CalcMerkelVariableSpeedTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D<Real64> Par(8); // Parameter array passed to solver
        int SolFla;             // Flag of solver
        Real64 CpWater;
        int LoopNum;
        int LoopSideNum;
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;
        int NumCellMin = 0;
        int NumCellMax = 0;
        int NumCellOn;
        Real64 WaterMassFlowRatePerCell;
        Real64 UAdesignPerCell;
        Real64 AirFlowRatePerCell;
        Real64 OutletWaterTempOFF;
        Real64 FreeConvQdot;
        Real64 WaterFlowRateRatio;
        Real64 UAwetbulbAdjFac;
        Real64 UAairflowAdjFac;
        Real64 UAwaterflowAdjFac;
        Real64 UAadjustedPerCell;
        Real64 FullSpeedFanQdot;
        bool IncrNumCellFlag;
        Real64 MinSpeedFanQdot;
        Real64 FanPowerAdjustFac;

        CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                         DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp,
                                                         DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                        RoutineName);
        SimpleTower(TowerNum).Qactual = 0.0;
        SimpleTower(TowerNum).FanPower = 0.0;
        SimpleTower(TowerNum).OutletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
        LoopNum = SimpleTower(TowerNum).LoopNum;
        LoopSideNum = SimpleTower(TowerNum).LoopSideNum;

        Real64 FreeConvTowerUA = SimpleTower(TowerNum).FreeConvTowerUA;
        Real64 HighSpeedTowerUA = SimpleTower(TowerNum).HighSpeedTowerUA;

        // If there is a fault of condenser SWT Sensor (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyCondenserSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyCondenserSWTIndex;
            // calculate the sensor offset using fault information
            SimpleTower(TowerNum).FaultyCondenserSWTOffset = FaultsManager::FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct();
        }

        // If there is a fault of cooling tower fouling (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyTowerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyTowerFoulingIndex;
            Real64 FreeConvTowerUA_ff = SimpleTower(TowerNum).FreeConvTowerUA;
            Real64 HighSpeedTowerUA_ff = SimpleTower(TowerNum).HighSpeedTowerUA;

            // calculate the Faulty Tower Fouling Factor using fault information
            SimpleTower(TowerNum).FaultyTowerFoulingFactor = FaultsManager::FaultsTowerFouling(FaultIndex).CalFaultyTowerFoulingFactor();

            // update the tower UA values at faulty cases
            FreeConvTowerUA = FreeConvTowerUA_ff * SimpleTower(TowerNum).FaultyTowerFoulingFactor;
            HighSpeedTowerUA = HighSpeedTowerUA_ff * SimpleTower(TowerNum).FaultyTowerFoulingFactor;
        }

        // Added for multi-cell. Determine the number of cells operating
        if (SimpleTower(TowerNum).DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MinFracFlowRate / SimpleTower(TowerNum).NumCell;
            WaterMassFlowRatePerCellMax =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MaxFracFlowRate / SimpleTower(TowerNum).NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), SimpleTower(TowerNum).NumCell);
            NumCellMax = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), SimpleTower(TowerNum).NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (SimpleTower(TowerNum).CellCtrl_Num == CellCtrl_MinCell) {
            NumCellOn = NumCellMin;
        } else {
            NumCellOn = NumCellMax;
        }

        SimpleTower(TowerNum).NumCellOn = NumCellOn;
        WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;
        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (SimpleTower(TowerNum).WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance || (MyLoad > DataHVACGlobals::SmallLoad)) {
            // for multiple cells, we assume that it's a common bassin
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            return;
        }

        if (std::abs(MyLoad) <= DataHVACGlobals::SmallLoad) {
            // tower doesn't need to do anything
            SimpleTower(TowerNum).OutletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).FanPower = 0.0;
            SimpleTower(TowerNum).__AirFlowRateRatio = 0.0;
            SimpleTower(TowerNum).Qactual = 0.0;
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            return;
        }

        // first find free convection cooling rate
        UAdesignPerCell = FreeConvTowerUA / SimpleTower(TowerNum).NumCell;
        AirFlowRatePerCell = SimpleTower(TowerNum).FreeConvAirFlowRate / SimpleTower(TowerNum).NumCell;
        OutletWaterTempOFF = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
        SimpleTower(TowerNum).WaterMassFlowRate = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).MassFlowRate;
        SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAdesignPerCell, OutletWaterTempOFF);

        FreeConvQdot = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - OutletWaterTempOFF);
        SimpleTower(TowerNum).FanPower = 0.0;

        if (std::abs(MyLoad) <= FreeConvQdot) { // can meet load with free convection and fan off

            SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempOFF;
            SimpleTower(TowerNum).__AirFlowRateRatio = 0.0;
            SimpleTower(TowerNum).Qactual = FreeConvQdot;
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);

            return;
        }

        // next find full fan speed cooling rate
        UAdesignPerCell = HighSpeedTowerUA / SimpleTower(TowerNum).NumCell;
        AirFlowRatePerCell = SimpleTower(TowerNum).HighSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;
        SimpleTower(TowerNum).__AirFlowRateRatio = 1.0;
        WaterFlowRateRatio = WaterMassFlowRatePerCell / SimpleTower(TowerNum).DesWaterMassFlowRatePerCell;
        UAwetbulbAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncWetBulbDiffCurvePtr, (DesignWetBulb - SimpleTower(TowerNum).AirWetBulb));
        UAairflowAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncAirFlowRatioCurvePtr, SimpleTower(TowerNum).__AirFlowRateRatio);
        UAwaterflowAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio);
        UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
        SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, SimpleTower(TowerNum).OutletWaterTemp);
        FullSpeedFanQdot = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);

        if (FullSpeedFanQdot <= std::abs(MyLoad)) { // full speed is what we want.

            if ((FullSpeedFanQdot + DataHVACGlobals::SmallLoad) < std::abs(MyLoad) && (NumCellOn < SimpleTower(TowerNum).NumCell) &&
                ((SimpleTower(TowerNum).WaterMassFlowRate / (NumCellOn + 1)) >= WaterMassFlowRatePerCellMin)) {
                // If full fan and not meeting setpoint, then increase number of cells until all are used or load is satisfied
                IncrNumCellFlag = true; // set value to true to enter in the loop
                while (IncrNumCellFlag) {
                    ++NumCellOn;
                    SimpleTower(TowerNum).NumCellOn = NumCellOn;
                    WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;
                    WaterFlowRateRatio = WaterMassFlowRatePerCell / SimpleTower(TowerNum).DesWaterMassFlowRatePerCell;
                    UAwaterflowAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio);
                    UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
                    SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, SimpleTower(TowerNum).OutletWaterTemp);
                    IncrNumCellFlag = (FullSpeedFanQdot + DataHVACGlobals::SmallLoad) < std::abs(MyLoad) &&
                                      (NumCellOn < SimpleTower(TowerNum).NumCell) &&
                                      ((SimpleTower(TowerNum).WaterMassFlowRate / (NumCellOn + 1)) >= WaterMassFlowRatePerCellMin);
                }
                FullSpeedFanQdot = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);
            }
            SimpleTower(TowerNum).Qactual = FullSpeedFanQdot;
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            // now calculate fan power
            FanPowerAdjustFac = CurveManager::CurveValue(SimpleTower(TowerNum).FanPowerfAirFlowCurve, SimpleTower(TowerNum).__AirFlowRateRatio);
            SimpleTower(TowerNum).FanPower = SimpleTower(TowerNum).HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / SimpleTower(TowerNum).NumCell;

            return;
        }

        // next find minimum air flow ratio cooling rate
        SimpleTower(TowerNum).__AirFlowRateRatio = SimpleTower(TowerNum).MinimumVSAirFlowFrac;
        AirFlowRatePerCell = SimpleTower(TowerNum).__AirFlowRateRatio * SimpleTower(TowerNum).HighSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;
        UAairflowAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncAirFlowRatioCurvePtr, SimpleTower(TowerNum).__AirFlowRateRatio);
        UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
        SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, SimpleTower(TowerNum).OutletWaterTemp);
        MinSpeedFanQdot = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);

        if (std::abs(MyLoad) <= MinSpeedFanQdot) { // min fan speed already exceeds load)
            SimpleTower(TowerNum).Qactual = MinSpeedFanQdot;
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            // now calculate fan power
            FanPowerAdjustFac = CurveManager::CurveValue(SimpleTower(TowerNum).FanPowerfAirFlowCurve, SimpleTower(TowerNum).__AirFlowRateRatio);
            SimpleTower(TowerNum).FanPower = SimpleTower(TowerNum).HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / SimpleTower(TowerNum).NumCell;
            return;
        }

        if ((MinSpeedFanQdot < std::abs(MyLoad)) && (std::abs(MyLoad) < FullSpeedFanQdot)) {
            // load can be refined by modulationg fan speed, call regulafalsi

            Par(1) = double(TowerNum);
            Par(2) = MyLoad;
            Par(3) = WaterMassFlowRatePerCell;
            Par(4) = UAdesignPerCell;
            Par(5) = UAwetbulbAdjFac;
            Par(6) = UAwaterflowAdjFac;
            Par(7) = CpWater;
            Par(8) = SimpleTower(TowerNum).WaterMassFlowRate;

            General::SolveRoot(Acc, MaxIte, SolFla, SimpleTower(TowerNum).__AirFlowRateRatio, VSMerkelResidual, SimpleTower(TowerNum).MinimumVSAirFlowFrac, 1.0, Par);

            if (SolFla == -1) {
                if (!DataGlobals::WarmupFlag) {
                    if (SimpleTower(TowerNum).VSMerkelAFRErrorIter < 1) {
                        ++SimpleTower(TowerNum).VSMerkelAFRErrorIter;
                        ShowWarningError(cCoolingTower_VariableSpeedMerkel +
                                         " - Iteration limit exceeded calculating variable speed fan ratio for unit = " + SimpleTower(TowerNum).Name);
                        ShowContinueError("Estimated air flow ratio  = " +
                                              General::RoundSigDigits((std::abs(MyLoad) - MinSpeedFanQdot) / (FullSpeedFanQdot - MinSpeedFanQdot), 4));
                        ShowContinueError("Calculated air flow ratio = " + General::RoundSigDigits(SimpleTower(TowerNum).__AirFlowRateRatio, 4));
                        ShowContinueErrorTimeStamp("The calculated air flow ratio will be used and the simulation continues. Occurrence info:");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        cCoolingTower_VariableSpeedMerkel + " \"" + SimpleTower(TowerNum).Name +
                            "\" - Iteration limit exceeded calculating air flow ratio error continues. air flow ratio statistics follow.",
                        SimpleTower(TowerNum).VSMerkelAFRErrorIterIndex,
                        SimpleTower(TowerNum).__AirFlowRateRatio,
                        SimpleTower(TowerNum).__AirFlowRateRatio);
                }
            } else if (SolFla == -2) {
                SimpleTower(TowerNum).__AirFlowRateRatio = (std::abs(MyLoad) - MinSpeedFanQdot) / (FullSpeedFanQdot - MinSpeedFanQdot);
                if (!DataGlobals::WarmupFlag) {
                    if (SimpleTower(TowerNum).VSMerkelAFRErrorFail < 1) {
                        ++SimpleTower(TowerNum).VSMerkelAFRErrorFail;
                        ShowWarningError(cCoolingTower_VariableSpeedMerkel +
                                         " - solver failed calculating variable speed fan ratio for unit = " + SimpleTower(TowerNum).Name);
                        ShowContinueError("Estimated air flow ratio  = " + General::RoundSigDigits(SimpleTower(TowerNum).__AirFlowRateRatio, 4));
                        ShowContinueErrorTimeStamp("The estimated air flow ratio will be used and the simulation continues. Occurrence info:");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        cCoolingTower_VariableSpeedMerkel + " \"" + SimpleTower(TowerNum).Name +
                            "\" - solver failed calculating air flow ratio error continues. air flow ratio statistics follow.",
                        SimpleTower(TowerNum).VSMerkelAFRErrorFailIndex,
                        SimpleTower(TowerNum).__AirFlowRateRatio,
                        SimpleTower(TowerNum).__AirFlowRateRatio);
                }
            }

            // now rerun to get peformance with AirFlowRateRatio
            AirFlowRatePerCell = SimpleTower(TowerNum).__AirFlowRateRatio * SimpleTower(TowerNum).HighSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;

            UAairflowAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncAirFlowRatioCurvePtr, SimpleTower(TowerNum).__AirFlowRateRatio);
            UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

            SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, SimpleTower(TowerNum).OutletWaterTemp);
            SimpleTower(TowerNum).Qactual = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);

            // now calculate fan power
            FanPowerAdjustFac = CurveManager::CurveValue(SimpleTower(TowerNum).FanPowerfAirFlowCurve, SimpleTower(TowerNum).__AirFlowRateRatio);
            SimpleTower(TowerNum).FanPower = SimpleTower(TowerNum).HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / SimpleTower(TowerNum).NumCell;
        }
    }

    Real64 VSMerkelResidual(Real64 const _AirFlowRateRatio, // fan speed ratio (1.0 is continuous, 0.0 is off)
                            Array1<Real64> const &Par      // par(1) = Tower number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Return value
        Real64 Residuum; // residual to be minimized to zero

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
        int TowerNum;
        Real64 WaterMassFlowRatePerCell;
        Real64 TargetLoad;
        Real64 UAdesignPerCell;
        Real64 UAwetbulbAdjFac;
        Real64 UAairflowAdjFac;
        Real64 UAwaterflowAdjFac;
        Real64 CpWater;
        Real64 TotalWaterMassFlowRate;
        Real64 AirFlowRatePerCell;
        Real64 UAadjustedPerCell;
        Real64 Qdot;
        Real64 OutletWaterTempTrial;

        TowerNum = int(Par(1));
        TargetLoad = Par(2);
        WaterMassFlowRatePerCell = Par(3);
        UAdesignPerCell = Par(4);
        UAwetbulbAdjFac = Par(5);
        UAwaterflowAdjFac = Par(6);
        CpWater = Par(7);
        TotalWaterMassFlowRate = Par(8);

        AirFlowRatePerCell = _AirFlowRateRatio * SimpleTower(TowerNum).HighSpeedAirFlowRate / SimpleTower(TowerNum).NumCell;

        UAairflowAdjFac = CurveManager::CurveValue(SimpleTower(TowerNum).UAModFuncAirFlowRatioCurvePtr, _AirFlowRateRatio);
        UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

        SimSimpleTower(TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTempTrial);

        Qdot = TotalWaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - OutletWaterTempTrial);

        Residuum = std::abs(TargetLoad) - Qdot;

        return Residuum;
    }

    void CalcVariableSpeedTower(int const TowerNum)
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
        static ObjexxFCL::gio::Fmt OutputFormat("(F5.2)");
        static ObjexxFCL::gio::Fmt OutputFormat2("(F8.5)");
        int const MaxIte(500);    // Maximum number of iterations
        Real64 const Acc(0.0001); // Accuracy of result
        static std::string const RoutineName("CalcVariableSpeedTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OutletWaterTempOFF;             // Outlet water temperature with fan OFF (C)
        Real64 OutletWaterTempON = 0.0;              // Outlet water temperature with fan ON at maximum fan speed (C)
        Real64 OutletWaterTempMIN;             // Outlet water temperature with fan at minimum speed (C)
        Real64 CpWater;                        // Specific heat of water
        Real64 TempSetPoint(0.0);              // Outlet water temperature setpoint (C)
        Real64 FanCurveValue;                  // Output of fan power as a func of air flow rate ratio curve
        int SolFla;                            // Flag of solver
        Array1D<Real64> Par(6);                // Parameter array for regula falsi solver
        Real64 Twb;                            // inlet air wet-bulb temperature
        Real64 TwbCapped;                      // inlet air wet-bulb temp passed to VS tower model
        Real64 Tr;                             // range temperature
        Real64 TrCapped;                       // range temp passed to VS tower model
        Real64 Ta;                             // approach temperature
        Real64 TaCapped;                       // approach temp passed to VS tower model
        Real64 WaterFlowRateRatio = 0.0;             // Water flow rate ratio
        Real64 WaterFlowRateRatioCapped = 0.0;       // Water flow rate ratio passed to VS tower model
        Real64 WaterDensity;                   // density of inlet water
        Real64 FreeConvectionCapFrac = 0.0;          // fraction of tower capacity in free convection
        Real64 FlowFraction;                   // liquid to gas (L/G) ratio for cooling tower
        std::string OutputChar;                // character string used for warning messages
        std::string OutputChar2;               // character string used for warning messages
        std::string OutputChar3;               // character string used for warning messages
        std::string OutputChar4;               // character string used for warning messages
        std::string OutputChar5;               // character string used for warning messages
        Real64 CurrentEndTime;                 // end time of time step for current simulation time step
        int LoopNum;
        int LoopSideNum;

        // Added variables for multicell
        Real64 WaterMassFlowRatePerCellMin = 0.0;
        Real64 WaterMassFlowRatePerCellMax;
        int NumCellMin(0);
        int NumCellMax(0);
        int NumCellOn(0);
        Real64 WaterMassFlowRatePerCell;
        bool IncrNumCellFlag;

        // Added for multi-cell. Determine the number of cells operating
        if (SimpleTower(TowerNum).DesWaterMassFlowRate > 0.0) {
            WaterMassFlowRatePerCellMin =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MinFracFlowRate / SimpleTower(TowerNum).NumCell;
            WaterMassFlowRatePerCellMax =
                SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).MaxFracFlowRate / SimpleTower(TowerNum).NumCell;

            // round it up to the nearest integer
            NumCellMin = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMax) + 0.9999), SimpleTower(TowerNum).NumCell);
            NumCellMax = min(int((SimpleTower(TowerNum).WaterMassFlowRate / WaterMassFlowRatePerCellMin) + 0.9999), SimpleTower(TowerNum).NumCell);
        }

        // cap min at 1
        if (NumCellMin <= 0) NumCellMin = 1;
        if (NumCellMax <= 0) NumCellMax = 1;

        if (SimpleTower(TowerNum).CellCtrl_Num == CellCtrl_MinCell) {
            NumCellOn = NumCellMin;
        } else {
            NumCellOn = NumCellMax;
        }

        SimpleTower(TowerNum).NumCellOn = NumCellOn;
        WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;

        // Initialize subroutine variables
        SimpleTower(TowerNum).Qactual = 0.0;
        SimpleTower(TowerNum).FanPower = 0.0;
        SimpleTower(TowerNum).OutletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;

        SimpleTower(TowerNum).WaterUsage = 0.0;
        Twb = SimpleTower(TowerNum).AirWetBulb;
        TwbCapped = SimpleTower(TowerNum).AirWetBulb;
        LoopNum = SimpleTower(TowerNum).LoopNum;
        LoopSideNum = SimpleTower(TowerNum).LoopSideNum;

        // water temperature setpoint
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                TempSetPoint = DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                TempSetPoint = DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).TempSetPointHi;
            } else {
                assert(false);
            }
        }

        // If there is a fault of condenser SWT Sensor (zrp_Jul2016)
        if (SimpleTower(TowerNum).FaultyCondenserSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = SimpleTower(TowerNum).FaultyCondenserSWTIndex;
            Real64 TowerOutletTemp_ff = TempSetPoint;

            // calculate the sensor offset using fault information
            SimpleTower(TowerNum).FaultyCondenserSWTOffset = FaultsManager::FaultsCondenserSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempSetPoint
            TempSetPoint = TowerOutletTemp_ff - SimpleTower(TowerNum).FaultyCondenserSWTOffset;
        }

        Tr = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - TempSetPoint;
        Ta = TempSetPoint - SimpleTower(TowerNum).AirWetBulb;

        // Do not RETURN here if flow rate is less than MassFlowTolerance. Check basin heater and then RETURN.
        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) return;
        // MassFlowTolerance is a parameter to indicate a no flow condition
        if (SimpleTower(TowerNum).WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            CalcBasinHeaterPower(SimpleTower(TowerNum).BasinHeaterPowerFTempDiff,
                                 SimpleTower(TowerNum).BasinHeaterSchedulePtr,
                                 SimpleTower(TowerNum).BasinHeaterSetPointTemp,
                                 SimpleTower(TowerNum).BasinHeaterPower);
            return;
        }

        // loop to increment NumCell if we cannot meet the setpoint with the actual number of cells calculated above
        IncrNumCellFlag = true;
        while (IncrNumCellFlag) {
            IncrNumCellFlag = false;
            // Initialize inlet node water properties
            WaterDensity = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                             DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp,
                                                             DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                            RoutineName);
            WaterFlowRateRatio =
                WaterMassFlowRatePerCell / (WaterDensity * SimpleTower(TowerNum).CalibratedWaterFlowRate / SimpleTower(TowerNum).NumCell);

            // check independent inputs with respect to model boundaries
            CheckModelBounds(TowerNum, Twb, Tr, Ta, WaterFlowRateRatio, TwbCapped, TrCapped, TaCapped, WaterFlowRateRatioCapped);

            //   determine the free convection capacity by finding the outlet temperature at full air flow and multiplying
            //   the tower's full capacity temperature difference by the percentage of tower capacity in free convection
            //   regime specified by the user

            SimpleTower(TowerNum).__AirFlowRateRatio = 1.0;
            OutletWaterTempOFF = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            OutletWaterTempON = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempOFF;
            FreeConvectionCapFrac = SimpleTower(TowerNum).FreeConvectionCapacityFraction;

            SimVariableTower(TowerNum, WaterFlowRateRatioCapped, SimpleTower(TowerNum).__AirFlowRateRatio, TwbCapped, OutletWaterTempON);

            if (OutletWaterTempON > TempSetPoint) {
                SimpleTower(TowerNum).FanCyclingRatio = 1.0;
                SimpleTower(TowerNum).__AirFlowRateRatio = 1.0;
                SimpleTower(TowerNum).FanPower = SimpleTower(TowerNum).HighSpeedFanPower * NumCellOn / SimpleTower(TowerNum).NumCell;
                SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempON;
                // if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
                if (NumCellOn < SimpleTower(TowerNum).NumCell && (SimpleTower(TowerNum).WaterMassFlowRate / (NumCellOn + 1)) > WaterMassFlowRatePerCellMin) {
                    ++NumCellOn;
                    WaterMassFlowRatePerCell = SimpleTower(TowerNum).WaterMassFlowRate / NumCellOn;
                    IncrNumCellFlag = true;
                }
            }
        }

        // find the correct air ratio only if full flow is  too much
        if (OutletWaterTempON < TempSetPoint) {
            //   outlet water temperature is calculated in the free convection regime
            OutletWaterTempOFF = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - FreeConvectionCapFrac * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - OutletWaterTempON);
            //   fan is OFF
            SimpleTower(TowerNum).FanCyclingRatio = 0.0;
            //   air flow ratio is assumed to be the fraction of tower capacity in the free convection regime (fan is OFF but air is flowing)
            SimpleTower(TowerNum).__AirFlowRateRatio = FreeConvectionCapFrac;

            // Assume setpoint was met using free convection regime (pump ON and fan OFF)
            SimpleTower(TowerNum).FanPower = 0.0;
            SimpleTower(TowerNum).OutletWaterTemp = OutletWaterTempOFF;

            if (OutletWaterTempOFF > TempSetPoint) {
                // Setpoint was not met, turn on cooling tower fan at minimum fan speed

                SimpleTower(TowerNum).__AirFlowRateRatio = SimpleTower(TowerNum).MinimumVSAirFlowFrac;
                SimVariableTower(TowerNum, WaterFlowRateRatioCapped, SimpleTower(TowerNum).__AirFlowRateRatio, TwbCapped, OutletWaterTempMIN);

                if (OutletWaterTempMIN < TempSetPoint) {
                    //         if setpoint was exceeded, cycle the fan at minimum air flow to meet the setpoint temperature
                    if (SimpleTower(TowerNum).FanPowerfAirFlowCurve == 0) {
                        SimpleTower(TowerNum).FanPower = pow_3(SimpleTower(TowerNum).__AirFlowRateRatio) * SimpleTower(TowerNum).HighSpeedFanPower * NumCellOn / SimpleTower(TowerNum).NumCell;
                    } else {
                        FanCurveValue = CurveManager::CurveValue(SimpleTower(TowerNum).FanPowerfAirFlowCurve, SimpleTower(TowerNum).__AirFlowRateRatio);
                        SimpleTower(TowerNum).FanPower = max(0.0, (SimpleTower(TowerNum).HighSpeedFanPower * FanCurveValue)) * NumCellOn / SimpleTower(TowerNum).NumCell;
                    }
                    //       fan is cycling ON and OFF at the minimum fan speed. Adjust fan power and air flow rate ratio according to cycling rate
                    SimpleTower(TowerNum).FanCyclingRatio = ((OutletWaterTempOFF - TempSetPoint) / (OutletWaterTempOFF - OutletWaterTempMIN));
                    SimpleTower(TowerNum).FanPower *= SimpleTower(TowerNum).FanCyclingRatio;
                    SimpleTower(TowerNum).OutletWaterTemp = TempSetPoint;
                    SimpleTower(TowerNum).__AirFlowRateRatio =
                        (SimpleTower(TowerNum).FanCyclingRatio * SimpleTower(TowerNum).MinimumVSAirFlowFrac) + ((1 - SimpleTower(TowerNum).FanCyclingRatio) * FreeConvectionCapFrac);
                } else {
                    //       if setpoint was not met at minimum fan speed, set fan speed to maximum
                    SimpleTower(TowerNum).__AirFlowRateRatio = 1.0;
                    //         fan will not cycle and runs the entire time step
                    SimpleTower(TowerNum).FanCyclingRatio = 1.0;

                    SimVariableTower(TowerNum, WaterFlowRateRatioCapped, SimpleTower(TowerNum).__AirFlowRateRatio, TwbCapped, SimpleTower(TowerNum).OutletWaterTemp);

                    // Setpoint was met with pump ON and fan ON at full flow
                    // Calculate the fraction of full air flow to exactly meet the setpoint temperature

                    Par(1) = TowerNum; // Index to cooling tower
                    //         cap the water flow rate ratio and inlet air wet-bulb temperature to provide a stable output
                    Par(2) = WaterFlowRateRatioCapped; // water flow rate ratio
                    Par(3) = TwbCapped;                // Inlet air wet-bulb temperature [C]
                    //         do not cap desired range and approach temperature to provide a valid (balanced) output for this simulation time step
                    Par(4) = Tr;  // Tower range temperature [C]
                    Par(5) = Ta;  // desired approach temperature [C]
                    Par(6) = 1.0; // calculate the air flow rate ratio required for a balance

                    General::SolveRoot(
                        Acc, MaxIte, SolFla, SimpleTower(TowerNum).__AirFlowRateRatio, SimpleTowerApproachResidual, SimpleTower(TowerNum).MinimumVSAirFlowFrac, 1.0, Par);
                    if (SolFla == -1) {
                        if (!DataGlobals::WarmupFlag)
                            ShowWarningError("Cooling tower iteration limit exceeded when calculating air flow rate ratio for tower " +
                                             SimpleTower(TowerNum).Name);
                        //           IF RegulaFalsi cannot find a solution then provide detailed output for debugging
                    } else if (SolFla == -2) {
                        if (!DataGlobals::WarmupFlag) {
                            ObjexxFCL::gio::write(OutputChar, OutputFormat) << TwbCapped;
                            ObjexxFCL::gio::write(OutputChar2, OutputFormat) << Tr;
                            ObjexxFCL::gio::write(OutputChar3, OutputFormat) << Ta;
                            ObjexxFCL::gio::write(OutputChar4, OutputFormat) << WaterFlowRateRatioCapped;
                            ObjexxFCL::gio::write(OutputChar5, OutputFormat) << SimpleTower(TowerNum).MinimumVSAirFlowFrac;
                            if (SimpleTower(TowerNum).CoolingTowerAFRRFailedCount < 1) {
                                ++SimpleTower(TowerNum).CoolingTowerAFRRFailedCount;
                                ShowWarningError("CoolingTower:VariableSpeed \"" + SimpleTower(TowerNum).Name +
                                                 "\" - Cooling tower air flow rate ratio calculation failed ");
                                ShowContinueError("...with conditions as Twb = " + OutputChar + ", Trange = " + OutputChar2 +
                                                  ", Tapproach = " + OutputChar3 + ", and water flow rate ratio = " + OutputChar4);
                                ShowContinueError("...a solution could not be found within the valid range of air flow rate ratios");
                                ShowContinueErrorTimeStamp(" ...Valid air flow rate ratio range = " + OutputChar5 + " to 1.0.");
                                ShowContinueError("...Consider modifying the design approach or design range temperature for this tower.");
                            } else {
                                ShowRecurringWarningErrorAtEnd("CoolingTower:VariableSpeed \"" + SimpleTower(TowerNum).Name +
                                                                   "\" - Cooling tower air flow rate ratio calculation failed error continues.",
                                                               SimpleTower(TowerNum).CoolingTowerAFRRFailedIndex);
                            }
                        }
                    }

                    //         Use theoretical cubic for deterination of fan power if user has not specified a fan power ratio curve
                    if (SimpleTower(TowerNum).FanPowerfAirFlowCurve == 0) {
                        SimpleTower(TowerNum).FanPower = pow_3(SimpleTower(TowerNum).__AirFlowRateRatio) * SimpleTower(TowerNum).HighSpeedFanPower * NumCellOn / SimpleTower(TowerNum).NumCell;
                    } else {
                        FanCurveValue = CurveManager::CurveValue(SimpleTower(TowerNum).FanPowerfAirFlowCurve, SimpleTower(TowerNum).__AirFlowRateRatio);
                        SimpleTower(TowerNum).FanPower = max(0.0, (SimpleTower(TowerNum).HighSpeedFanPower * FanCurveValue)) * NumCellOn / SimpleTower(TowerNum).NumCell;
                    }
                    //           outlet water temperature is calculated as the inlet air wet-bulb temperature plus tower approach temperature
                    SimpleTower(TowerNum).OutletWaterTemp = Twb + Ta;
                } // IF(OutletWaterTempMIN .LT. TempSetPoint)THEN

            } // IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
        }     // IF(OutletWaterTempON .LT. TempSetPoint) ie if tower should not run at full capacity

        CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                         DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp,
                                                         DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                        RoutineName);
        SimpleTower(TowerNum).Qactual = SimpleTower(TowerNum).WaterMassFlowRate * CpWater * (DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp - SimpleTower(TowerNum).OutletWaterTemp);
        SimpleTower(TowerNum).NumCellOn = NumCellOn;

        //   calculate end time of current time step
        CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > SimpleTower(TowerNum).CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= SimpleTower(TowerNum).TimeStepSysLast) {
            if (SimpleTower(SimpleTower(TowerNum).VSTower).PrintLGMessage) {
                ++SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountFlowFrac;
                //       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountFlowFrac < 2) {
                    ShowWarningError(SimpleTower(SimpleTower(TowerNum).VSTower).LGBuffer1);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).LGBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                       "\" - Liquid to gas ratio is out of range error continues...",
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).ErrIndexLG,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).LGLast,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).LGLast);
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        SimpleTower(TowerNum).TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        SimpleTower(TowerNum).CurrentEndTimeLast = CurrentEndTime;

        //   warn user on first occurrence if flow fraction is greater than maximum for the YorkCalc model, use recurring warning stats
        if (SimpleTower(TowerNum).TowerModelType == YorkCalcModel || SimpleTower(TowerNum).TowerModelType == YorkCalcUserDefined) {
            SimpleTower(SimpleTower(TowerNum).VSTower).PrintLGMessage = false;
            //      Do not report error message in free convection regime
            if (SimpleTower(TowerNum).__AirFlowRateRatio > SimpleTower(TowerNum).MinimumVSAirFlowFrac) {
                FlowFraction = WaterFlowRateRatioCapped / SimpleTower(TowerNum).__AirFlowRateRatio;
                //        Flow fractions greater than a MaxLiquidToGasRatio of 8 are not reliable using the YorkCalc model
                if (FlowFraction > SimpleTower(SimpleTower(TowerNum).VSTower).MaxLiquidToGasRatio) {
                    //          Report warnings only during actual simulation
                    if (!DataGlobals::WarmupFlag) {
                        SimpleTower(SimpleTower(TowerNum).VSTower).PrintLGMessage = true;
                        ObjexxFCL::gio::write(OutputChar, OutputFormat) << FlowFraction;
                        ObjexxFCL::gio::write(OutputChar2, OutputFormat) << SimpleTower(SimpleTower(TowerNum).VSTower).MaxLiquidToGasRatio;
                        SimpleTower(SimpleTower(TowerNum).VSTower).LGBuffer1 = SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                                           "\" - Liquid to gas ratio (L/G) is out of range at " + OutputChar + '.';
                        SimpleTower(SimpleTower(TowerNum).VSTower).LGBuffer2 = " ...Valid maximum ratio = " + OutputChar2 +
                                                                           ". Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                            General::CreateSysTimeIntervalString();
                        SimpleTower(SimpleTower(TowerNum).VSTower).LGLast = FlowFraction;
                    }
                }
            }
        }
    }

    void SimSimpleTower(int const TowerNum, Real64 const _WaterMassFlowRate, Real64 const AirFlowRate, Real64 const UAdesign, Real64 &_OutletWaterTemp)
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

        // Locals
        Real64 _Qactual; // Actual heat transfer rate between tower water and air [W]

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const IterMax(50);                  // Maximum number of iterations allowed
        Real64 const WetBulbTolerance(0.00001); // Maximum error for exiting wet-bulb temperature between iterations
        // [delta K/K]
        Real64 const DeltaTwbTolerance(0.001); // Maximum error (tolerance) in DeltaTwb for iteration convergence [C]
        static std::string const RoutineName("SimSimpleTower");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Iter;                    // Number of iterations completed
        Real64 MdotCpWater;          // Water mass flow rate times the heat capacity [W/K]
        Real64 InletAirTemp;         // Dry-bulb temperature of air entering the tower [C]
        Real64 CpWater;              // Heat capacity of water [J/kg/K]
        Real64 CpAir;                // Heat capacity of air [J/kg/K]
        Real64 AirDensity;           // Density of air [kg/m3]
        Real64 AirMassFlowRate;      // Mass flow rate of air [kg/s]
        Real64 effectiveness;        // Effectiveness of the heat exchanger [-]
        Real64 UAactual;             // UA value at actual conditions [W/C]
        Real64 InletAirEnthalpy;     // Enthalpy of entering moist air [J/kg]
        Real64 InletAirWetBulb;      // Wetbulb temp of entering moist air [C]
        Real64 OutletAirEnthalpy;    // Enthalpy of exiting moist air [J/kg]
        Real64 OutletAirWetBulb;     // Wetbulb temp of exiting moist air [C]
        Real64 OutletAirWetBulbLast; // temporary Wetbulb temp of exiting moist air [C]
        Real64 AirCapacity;          // MdotCp of air through the tower
        Real64 CapacityRatioMin;     // Minimum capacity of airside and waterside
        Real64 CapacityRatioMax;     // Maximum capacity of airside and waterside
        Real64 CapacityRatio;        // Ratio of minimum to maximum capacity
        Real64 NumTransferUnits;     // Number of transfer Units [NTU]
        Real64 WetBulbError;         // Calculated error for exiting wet-bulb temperature between iterations [delta K/K]
        Real64 CpAirside;            // Delta enthalpy of the tower air divides by delta air wet-bulb temp [J/kg/K]
        Real64 DeltaTwb;             // Absolute value of difference between inlet and outlet air wet-bulb temp [C]

        // initialize some local variables
        _Qactual = 0.0;
        //    WetBulbTolerance  = 0.00001
        WetBulbError = 1.0;
        //    IterMax           = 50
        DeltaTwb = 1.0;
        //    DeltaTwbTolerance = 0.001

        // set local tower inlet and outlet temperature variables
        SimpleTower(TowerNum).InletWaterTemp = SimpleTower(TowerNum).WaterTemp;
        _OutletWaterTemp = SimpleTower(TowerNum).InletWaterTemp;
        InletAirTemp = SimpleTower(TowerNum).AirTemp;
        InletAirWetBulb = SimpleTower(TowerNum).AirWetBulb;

        if (UAdesign == 0.0) return;

        // set water and air properties
        AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(SimpleTower(TowerNum).AirPress, InletAirTemp, SimpleTower(TowerNum).AirHumRat);
        AirMassFlowRate = AirFlowRate * AirDensity;
        CpAir = Psychrometrics::PsyCpAirFnWTdb(SimpleTower(TowerNum).AirHumRat, InletAirTemp);
        CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                                         SimpleTower(TowerNum).WaterTemp,
                                                         DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                        RoutineName);
        InletAirEnthalpy = Psychrometrics::PsyHFnTdbRhPb(SimpleTower(TowerNum).AirWetBulb, 1.0, SimpleTower(TowerNum).AirPress);

        // initialize exiting wet bulb temperature before iterating on final solution
        OutletAirWetBulb = InletAirWetBulb + 6.0;

        // Calcluate mass flow rates
        if (_WaterMassFlowRate > 0.0) {
            MdotCpWater = _WaterMassFlowRate * CpWater;
        } else {
            _OutletWaterTemp = SimpleTower(TowerNum).InletWaterTemp;
            return;
        }
        Iter = 0;
        while ((WetBulbError > WetBulbTolerance) && (Iter <= IterMax) && (DeltaTwb > DeltaTwbTolerance)) {
            ++Iter;
            //        OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0,OutBaroPress)
            OutletAirEnthalpy = Psychrometrics::PsyHFnTdbRhPb(OutletAirWetBulb, 1.0, SimpleTower(TowerNum).AirPress);
            // calculate the airside specific heat and capacity
            CpAirside = (OutletAirEnthalpy - InletAirEnthalpy) / (OutletAirWetBulb - InletAirWetBulb);
            AirCapacity = AirMassFlowRate * CpAirside;
            // calculate the minimum to maximum capacity ratios of airside and waterside
            CapacityRatioMin = min(AirCapacity, MdotCpWater);
            CapacityRatioMax = max(AirCapacity, MdotCpWater);
            CapacityRatio = CapacityRatioMin / CapacityRatioMax;
            // Calculate heat transfer coefficient and number of transfer units (NTU)
            UAactual = UAdesign * CpAirside / CpAir;
            NumTransferUnits = UAactual / CapacityRatioMin;
            // calculate heat exchanger effectiveness
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
            _Qactual = effectiveness * CapacityRatioMin * (SimpleTower(TowerNum).InletWaterTemp - InletAirWetBulb);
            OutletAirWetBulbLast = OutletAirWetBulb;
            // calculate new exiting wet bulb temperature of airstream
            OutletAirWetBulb = InletAirWetBulb + _Qactual / AirCapacity;
            // Check error tolerance and exit if satisfied
            DeltaTwb = std::abs(OutletAirWetBulb - InletAirWetBulb);
            // Add KelvinConv to denominator below convert OutletAirWetBulbLast to Kelvin to avoid divide by zero.
            // Wet bulb error units are delta K/K
            WetBulbError = std::abs((OutletAirWetBulb - OutletAirWetBulbLast) / (OutletAirWetBulbLast + DataGlobals::KelvinConv));
        }

        if (_Qactual >= 0.0) {
            _OutletWaterTemp = SimpleTower(TowerNum).InletWaterTemp - _Qactual / MdotCpWater;
        } else {
            _OutletWaterTemp = SimpleTower(TowerNum).InletWaterTemp;
        }
    }

    void SimVariableTower(int const TowerNum,              // variable speed tower index
                          Real64 const WaterFlowRateRatio, // current water flow rate ratio (capped if applicable)
                          Real64 const _AirFlowRateRatio,   // current air flow rate ratio
                          Real64 const Twb,                // current inlet air wet-bulb temperature (C, capped if applicable)
                          Real64 &_OutletWaterTemp         // calculated tower outlet water temperature (C)
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
        int SolFla;               // Flag of solver
        Array1D<Real64> Par(4);   // Parameter array for regula falsi solver
        Real64 Tr;                // range temperature which results in an energy balance
        Real64 TempSetPoint(0.0); // local temporary for loop setpoint

        //   determine tower outlet water temperature
        Par(1) = TowerNum;           // Index to cooling tower
        Par(2) = WaterFlowRateRatio; // water flow rate ratio
        Par(3) = _AirFlowRateRatio;   // air flow rate ratio
        Par(4) = Twb;                // inlet air wet-bulb temperature [C]
        General::SolveRoot(Acc, MaxIte, SolFla, Tr, SimpleTowerTrResidual, 0.001, SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp, Par);

        _OutletWaterTemp = SimpleTower(TowerNum).WaterTemp - Tr;

        if (SolFla == -1) {
            ShowSevereError("Iteration limit exceeded in calculating tower nominal capacity at minimum air flow ratio");
            ShowContinueError(
                "Design inlet air wet-bulb or approach temperature must be modified to achieve an acceptable range at the minimum air flow rate");
            ShowContinueError("Cooling tower simulation failed to converge for tower " + SimpleTower(TowerNum).Name);
            //    if SolFla = -2, Tr is returned as minimum value (0.001) and outlet temp = inlet temp - 0.001
        } else if (SolFla == -2) { // decide if should run at max flow
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    TempSetPoint = DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).LoopSide(SimpleTower(TowerNum).LoopSideNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    TempSetPoint = DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).LoopSide(SimpleTower(TowerNum).LoopSideNum).TempSetPointHi;
                } else {
                    assert(false);
                }
            }
            if (SimpleTower(TowerNum).WaterTemp > (TempSetPoint + SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp)) { // run flat out
                _OutletWaterTemp = SimpleTower(TowerNum).WaterTemp - SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp;
            }
        }
    }

    void CalcVSTowerApproach(int const TowerNum,        // Index to cooling tower
                             Real64 const PctWaterFlow, // Water flow ratio of cooling tower
                             Real64 const AirFlowRatio, // Air flow ratio of cooling tower
                             Real64 const Twb,          // Inlet air wet-bulb temperature [C]
                             Real64 const Tr,           // Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
                             Real64 &Approach           // Calculated approach temperature [C]
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

         if (SimpleTower(TowerNum).TowerModelType == YorkCalcModel || SimpleTower(TowerNum).TowerModelType == YorkCalcUserDefined) {
            Real64 PctAirFlow = AirFlowRatio;
            Real64 FlowFactor = PctWaterFlow / PctAirFlow;
            Approach = SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(1) + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(2) * Twb +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(3) * Twb * Twb + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(4) * Tr +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(5) * Twb * Tr + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(6) * Twb * Twb * Tr +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(7) * Tr * Tr + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(8) * Twb * Tr * Tr +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(9) * Twb * Twb * Tr * Tr +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(10) * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(11) * Twb * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(12) * Twb * Twb * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(13) * Tr * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(14) * Twb * Tr * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(15) * Twb * Twb * Tr * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(16) * Tr * Tr * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(17) * Twb * Tr * Tr * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(18) * Twb * Twb * Tr * Tr * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(19) * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(20) * Twb * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(21) * Twb * Twb * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(22) * Tr * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(23) * Twb * Tr * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(24) * Twb * Twb * Tr * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(25) * Tr * Tr * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(26) * Twb * Tr * Tr * FlowFactor * FlowFactor +
                       SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(27) * Twb * Twb * Tr * Tr * FlowFactor * FlowFactor;

        } else { // empirical model is CoolTools format
            //     the CoolTools model actually uses PctFanPower = AirFlowRatio^3 as an input to the model
            Real64 PctAirFlow = pow_3(AirFlowRatio);
            Approach =
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(1) + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(2) * PctAirFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(3) * PctAirFlow * PctAirFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(4) * PctAirFlow * PctAirFlow * PctAirFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(5) * PctWaterFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(6) * PctAirFlow * PctWaterFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(7) * PctAirFlow * PctAirFlow * PctWaterFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(8) * PctWaterFlow * PctWaterFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(9) * PctAirFlow * PctWaterFlow * PctWaterFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(10) * PctWaterFlow * PctWaterFlow * PctWaterFlow +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(11) * Twb + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(12) * PctAirFlow * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(13) * PctAirFlow * PctAirFlow * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(14) * PctWaterFlow * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(15) * PctAirFlow * PctWaterFlow * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(16) * PctWaterFlow * PctWaterFlow * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(17) * Twb * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(18) * PctAirFlow * Twb * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(19) * PctWaterFlow * Twb * Twb +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(20) * Twb * Twb * Twb + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(21) * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(22) * PctAirFlow * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(23) * PctAirFlow * PctAirFlow * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(24) * PctWaterFlow * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(25) * PctAirFlow * PctWaterFlow * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(26) * PctWaterFlow * PctWaterFlow * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(27) * Twb * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(28) * PctAirFlow * Twb * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(29) * PctWaterFlow * Twb * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(30) * Twb * Twb * Tr + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(31) * Tr * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(32) * PctAirFlow * Tr * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(33) * PctWaterFlow * Tr * Tr +
                SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(34) * Twb * Tr * Tr + SimpleTower(SimpleTower(TowerNum).VSTower).Coeff(35) * Tr * Tr * Tr;
        }
    }

    void CheckModelBounds(int const TowerNum,              // index to tower
                          Real64 const Twb,                // current inlet air wet-bulb temperature (C)
                          Real64 const Tr,                 // requested range temperature for current time step (C)
                          Real64 const Ta,                 // requested approach temperature for current time step (C)
                          Real64 const WaterFlowRateRatio, // current water flow rate ratio at water inlet node
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
        // developement of the empirical model.

        // METHODOLOGY EMPLOYED:
        // The empirical models used for simulating a variable speed cooling tower are based on a limited data set.
        // Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
        // The range of each independent variable is provided either by the CoolTools or York model limits, or
        // specified by the user if the model is User Defined (in either the CoolTools or York model format).
        // These limits are tested in this subroutine each time step and returned for use by the calling routine.
        // The independent variables capped here may or may not be passed to the empirical model in the calling
        // routine depending on their use.
        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string OutputChar;         // character string for warning messages
        std::string OutputCharLo;       // character string for warning messages
        std::string OutputCharHi;       // character string for warning messages
        std::string TrimValue;          // character string for warning messages
        Real64 CurrentEndTime(0.0);     // end time of time step for current simulation time step
        // current end time is compared with last to see if time step changed

        //   initialize capped variables in case independent variables are in bounds
        TwbCapped = Twb;
        TrCapped = Tr;
        TaCapped = Ta;
        WaterFlowRateRatioCapped = WaterFlowRateRatio;

        //   calculate end time of current time step
        CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        //   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > SimpleTower(TowerNum).CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= SimpleTower(TowerNum).TimeStepSysLast) {
            if (SimpleTower(SimpleTower(TowerNum).VSTower).PrintTrMessage) {
                ++SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountTR;
                if (SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountTR < 2) {
                    ShowWarningError(SimpleTower(SimpleTower(TowerNum).VSTower).TrBuffer1);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).TrBuffer2);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).TrBuffer3);
                    ShowContinueError(" ...Range temperatures outside model boundaries may not adversely affect tower performance.");
                    ShowContinueError(" ...This is not an unexpected occurrence when simulating actual conditions.");
                } else {
                    ShowRecurringWarningErrorAtEnd(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                       "\" - Tower range temperature is out of range error continues...",
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).ErrIndexTR,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).TrLast,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).TrLast);
                }
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).PrintTwbMessage) {
                ++SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountIAWB;
                if (SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountIAWB < 6) {
                    ShowWarningError(SimpleTower(SimpleTower(TowerNum).VSTower).TwbBuffer1);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).TwbBuffer2);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).TwbBuffer3);
                    ShowContinueError(" ...Wet-bulb temperatures outside model boundaries may not adversely affect tower performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                       "\" - Inlet air wet-bulb temperature is out of range error continues...",
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).ErrIndexIAWB,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).TwbLast,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).TwbLast);
                }
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).PrintTaMessage) {
                ++SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountTA;
                if (SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountTA < 2) {
                    ShowWarningError(SimpleTower(SimpleTower(TowerNum).VSTower).TaBuffer1);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).TaBuffer2);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).TaBuffer3);
                    ShowContinueError(" ...Approach temperatures outside model boundaries may not adversely affect tower performance.");
                    ShowContinueError(" ...This is not an unexpected occurrence when simulating actual conditions.");
                } else {
                    ShowRecurringWarningErrorAtEnd(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                       "\" - Tower approach temperature is out of range error continues...",
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).ErrIndexTA,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).TaLast,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).TaLast);
                }
            }
            if (SimpleTower(SimpleTower(TowerNum).VSTower).PrintWFRRMessage) {
                ++SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountWFRR;
                if (SimpleTower(SimpleTower(TowerNum).VSTower).VSErrorCountWFRR < 6) {
                    ShowWarningError(SimpleTower(SimpleTower(TowerNum).VSTower).WFRRBuffer1);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).WFRRBuffer2);
                    ShowContinueError(SimpleTower(SimpleTower(TowerNum).VSTower).WFRRBuffer3);
                    ShowContinueError(" ...Water flow rate ratios outside model boundaries may not adversely affect tower performance.");
                } else {
                    ShowRecurringWarningErrorAtEnd(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                       "\" - Water flow rate ratio is out of range error continues...",
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).ErrIndexWFRR,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).WaterFlowRateRatioLast,
                                                   SimpleTower(SimpleTower(TowerNum).VSTower).WaterFlowRateRatioLast);
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        SimpleTower(TowerNum).TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        SimpleTower(TowerNum).CurrentEndTimeLast = CurrentEndTime;

        //   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
        if (Twb < SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp || Twb > SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp) {
            OutputChar = General::RoundSigDigits(Twb, 2);
            OutputCharLo = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp, 2);
            OutputCharHi = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp, 2);
            if (Twb < SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp) {
                TwbCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MinInletAirWBTemp;
            }
            if (Twb > SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp) {
                TwbCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MaxInletAirWBTemp;
            }
            if (!DataGlobals::WarmupFlag) {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintTwbMessage = true;
                SimpleTower(SimpleTower(TowerNum).VSTower).TwbBuffer1 = SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                                    "\" - Inlet air wet-bulb temperature is outside model boundaries at " +
                                                                    OutputChar + '.';
                SimpleTower(SimpleTower(TowerNum).VSTower).TwbBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi +
                                                                    ". Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                    General::CreateSysTimeIntervalString();
                TrimValue = General::RoundSigDigits(TwbCapped, 6);
                SimpleTower(SimpleTower(TowerNum).VSTower).TwbBuffer3 = " ...Inlet air wet-bulb temperature passed to the model = " + TrimValue;
                SimpleTower(SimpleTower(TowerNum).VSTower).TwbLast = Twb;
            } else {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintTwbMessage = false;
            }
        } else {
            SimpleTower(SimpleTower(TowerNum).VSTower).PrintTwbMessage = false;
        }

        if (Tr < SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp || Tr > SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp) {
            OutputChar = General::RoundSigDigits(Tr, 2);
            OutputCharLo = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp, 2);
            OutputCharHi = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp, 2);
            if (Tr < SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp) {
                TrCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MinRangeTemp;
            }
            if (Tr > SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp) {
                TrCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MaxRangeTemp;
            }
            if (!DataGlobals::WarmupFlag) {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintTrMessage = true;
                SimpleTower(SimpleTower(TowerNum).VSTower).TrBuffer1 = SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                                   "\" - Tower range temperature is outside model boundaries at " + OutputChar + '.';
                SimpleTower(SimpleTower(TowerNum).VSTower).TrBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi +
                                                                   ". Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                    General::CreateSysTimeIntervalString();
                TrimValue = General::RoundSigDigits(Tr, 5);
                SimpleTower(SimpleTower(TowerNum).VSTower).TrBuffer3 = " ...Tower range temperature passed to the model = " + TrimValue;
                SimpleTower(SimpleTower(TowerNum).VSTower).TrLast = Tr;
            } else {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintTrMessage = false;
            }
        } else {
            SimpleTower(SimpleTower(TowerNum).VSTower).PrintTrMessage = false;
        }

        if (Ta < SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp || Ta > SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp) {
            OutputChar = General::RoundSigDigits(Ta, 2);
            OutputCharLo = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp, 2);
            OutputCharHi = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp, 2);
            if (Ta < SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp) {
                TaCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MinApproachTemp;
            }
            if (Ta > SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp) {
                TaCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MaxApproachTemp;
            }
            if (!DataGlobals::WarmupFlag) {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintTaMessage = true;
                SimpleTower(SimpleTower(TowerNum).VSTower).TaBuffer1 = SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                                   "\" - Tower approach temperature is outside model boundaries at " + OutputChar +
                                                                   '.';
                SimpleTower(SimpleTower(TowerNum).VSTower).TaBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi +
                                                                   ". Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                    General::CreateSysTimeIntervalString();
                TrimValue = General::RoundSigDigits(Ta, 5);
                SimpleTower(SimpleTower(TowerNum).VSTower).TaBuffer3 = " ...Tower approach temperature passed to the model = " + TrimValue;
                SimpleTower(SimpleTower(TowerNum).VSTower).TaLast = Ta;
            } else {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintTaMessage = false;
            }
        } else {
            SimpleTower(SimpleTower(TowerNum).VSTower).PrintTaMessage = false;
        }

        if (SimpleTower(TowerNum).TowerModelType == YorkCalcModel || SimpleTower(TowerNum).TowerModelType == YorkCalcUserDefined) {
            //     Water flow rate ratio warning not valid for YorkCalc model, print liquid to gas ratio
            //     warning instead (bottom of Subroutine VariableSpeedTower)
            SimpleTower(SimpleTower(TowerNum).VSTower).PrintWFRRMessage = false;
        } else {
            if (WaterFlowRateRatio < SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio ||
                WaterFlowRateRatio > SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio) {
                OutputChar = General::RoundSigDigits(WaterFlowRateRatio, 2);
                OutputCharLo = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio, 2);
                OutputCharHi = General::RoundSigDigits(SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio, 2);
                if (WaterFlowRateRatio < SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio) {
                    WaterFlowRateRatioCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MinWaterFlowRatio;
                }
                if (WaterFlowRateRatio > SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio) {
                    WaterFlowRateRatioCapped = SimpleTower(SimpleTower(TowerNum).VSTower).MaxWaterFlowRatio;
                }
                if (!DataGlobals::WarmupFlag) {
                    SimpleTower(SimpleTower(TowerNum).VSTower).PrintWFRRMessage = true;
                    SimpleTower(SimpleTower(TowerNum).VSTower).WFRRBuffer1 = SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                                         "\" - Water flow rate ratio is outside model boundaries at " + OutputChar +
                                                                         '.';
                    SimpleTower(SimpleTower(TowerNum).VSTower).WFRRBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi +
                                                                         ". Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                        General::CreateSysTimeIntervalString();
                    TrimValue = General::RoundSigDigits(WaterFlowRateRatioCapped, 5);
                    SimpleTower(SimpleTower(TowerNum).VSTower).WFRRBuffer3 = " ...Water flow rate ratio passed to the model = " + TrimValue;
                    SimpleTower(SimpleTower(TowerNum).VSTower).WaterFlowRateRatioLast = WaterFlowRateRatio;
                } else {
                    SimpleTower(SimpleTower(TowerNum).VSTower).PrintWFRRMessage = false;
                }
            } else {
                SimpleTower(SimpleTower(TowerNum).VSTower).PrintWFRRMessage = false;
            }
        }
    }

    Real64 SimpleTowerUAResidual(Real64 const UA,          // UA of cooling tower
                                 Array1<Real64> const &Par // par(1) = design tower load [W]
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
        // Puts UA into the cooling tower data structure, calls SimSimpleTower, and calculates
        // the residual as defined above.

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = tower number
        // par(3) = design water mass flow rate [kg/s]
        // par(4) = design air volume flow rate [m3/s]
        // par(5) = water specific heat [J/(kg*C)]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int TowerIndex;       // index of this tower
        Real64 OutWaterTemp;  // outlet water temperature [C]
        Real64 CoolingOutput; // tower cooling output [W]

        TowerIndex = int(Par(2));
        SimSimpleTower(TowerIndex, Par(3), Par(4), UA, OutWaterTemp);
        CoolingOutput = Par(5) * Par(3) * (SimpleTower(TowerIndex).WaterTemp - OutWaterTemp);
        Residuum = (Par(1) - CoolingOutput) / Par(1);
        return Residuum;
    }

    Real64 SimpleTowerTrResidual(Real64 const Trange,      // cooling tower range temperature [C]
                                 Array1<Real64> const &Par // par(1) = tower number
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

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Locals
        Real64 _AirFlowRateRatio; // ratio of water flow rate to design water flow rate

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = water flow ratio
        // par(3) = air flow ratio
        // par(4) = inlet air wet-bulb temperature [C]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int TowerIndex;            // index of this tower
        Real64 WaterFlowRateRatio; // ratio of water flow rate to design water flow rate
        Real64 InletAirWB;         // inlet air wet-bulb temperature [C]
        Real64 Tapproach;          // tower approach temperature [C]

        TowerIndex = int(Par(1));
        WaterFlowRateRatio = Par(2);
        _AirFlowRateRatio = Par(3);
        InletAirWB = Par(4);
        Tapproach = 0.0;

        // call model to determine approach temperature given other independent variables (range temp is being varied to find balance)
        CalcVSTowerApproach(TowerIndex, WaterFlowRateRatio, _AirFlowRateRatio, InletAirWB, Trange, Tapproach);
        // calculate residual based on a balance where Twb + Ta + Tr = Node(WaterInletNode)%Temp
        Residuum = (InletAirWB + Tapproach + Trange) - DataLoopNode::Node(SimpleTower(TowerIndex).WaterInletNodeNum).Temp;

        return Residuum;
    }

    Real64 SimpleTowerApproachResidual(Real64 const FlowRatio,   // water or air flow ratio of cooling tower
                                       Array1<Real64> const &Par // par(1) = tower number
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

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Locals
        Real64 _AirFlowRateRatio; // ratio of water flow rate to design water flow rate

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = water or air flow ratio (opposite of input variable)
        // par(3) = inlet air wet-bulb temp [C]
        // par(4) = tower range [C]
        // par(5) = desired approach [C]
        // par(6) = 0.0 to calculate water flow rate ratio, 1.0 for air

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int TowerIndex;            // index of this tower
        Real64 WaterFlowRateRatio; // ratio of water flow rate to design water flow rate
        Real64 InletAirWB;         // inlet air wet-bulb temperature [C]
        Real64 Trange;             // tower range temperature [C]
        Real64 TapproachActual;    // actual tower approach temperature [C]
        Real64 TapproachDesired;   // desired tower approach temperature [C]

        TowerIndex = int(Par(1));
        if (Par(6) == 0.0) {
            _AirFlowRateRatio = Par(2);
            WaterFlowRateRatio = FlowRatio;
        } else {
            _AirFlowRateRatio = FlowRatio;
            WaterFlowRateRatio = Par(2);
        }
        InletAirWB = Par(3);
        Trange = Par(4);
        TapproachDesired = Par(5);
        TapproachActual = 0.0;

        // call model to determine tower approach temperature given other independent variables
        CalcVSTowerApproach(TowerIndex, WaterFlowRateRatio, _AirFlowRateRatio, InletAirWB, Trange, TapproachActual);
        Residuum = TapproachDesired - TapproachActual;

        return Residuum;
    }

    void CalculateWaterUseage(int const TowerNum)
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
        static std::string const RoutineName("CalculateWaterUseage");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirDensity;
        Real64 AirMassFlowRate;
        Real64 AvailTankVdot;
        Real64 BlowDownVdot(0.0);
        Real64 DriftVdot(0.0);
        Real64 EvapVdot(0.0);
        Real64 InletAirEnthalpy;
        Real64 InSpecificHumRat;
        Real64 OutSpecificHumRat;
        Real64 TairAvg;
        Real64 MakeUpVdot;
        Real64 OutletAirEnthalpy;
        Real64 OutletAirHumRatSat;
        Real64 OutletAirTSat;
        Real64 StarvedVdot;
        Real64 TankSupplyVdot;
        Real64 rho;

        Real64 AverageWaterTemp;

        AverageWaterTemp = (SimpleTower(TowerNum).InletWaterTemp + SimpleTower(TowerNum).OutletWaterTemp) / 2.0;

        // Set water and air properties
        if (SimpleTower(TowerNum).EvapLossMode == EvapLossByMoistTheory) {

            AirDensity =
                Psychrometrics::PsyRhoAirFnPbTdbW(SimpleTower(TowerNum).AirPress, SimpleTower(TowerNum).AirTemp, SimpleTower(TowerNum).AirHumRat);
            AirMassFlowRate = SimpleTower(TowerNum).__AirFlowRateRatio * SimpleTower(TowerNum).HighSpeedAirFlowRate * AirDensity * SimpleTower(TowerNum).NumCellOn /
                              SimpleTower(TowerNum).NumCell;
            InletAirEnthalpy = Psychrometrics::PsyHFnTdbRhPb(SimpleTower(TowerNum).AirWetBulb, 1.0, SimpleTower(TowerNum).AirPress);

            if (AirMassFlowRate > 0.0) {
                // Calculate outlet air conditions for determining water usage

                OutletAirEnthalpy = InletAirEnthalpy + SimpleTower(TowerNum).Qactual / AirMassFlowRate;
                OutletAirTSat = Psychrometrics::PsyTsatFnHPb(OutletAirEnthalpy, SimpleTower(TowerNum).AirPress);
                OutletAirHumRatSat = Psychrometrics::PsyWFnTdbH(OutletAirTSat, OutletAirEnthalpy);

                // calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
                InSpecificHumRat = SimpleTower(TowerNum).AirHumRat / (1 + SimpleTower(TowerNum).AirHumRat);
                OutSpecificHumRat = OutletAirHumRatSat / (1 + OutletAirHumRatSat);

                // calculate average air temp for density call
                TairAvg = (SimpleTower(TowerNum).AirTemp + OutletAirTSat) / 2.0;

                // Amount of water evaporated, get density water at air temp or 4 C if too cold
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                       max(TairAvg, 4.0),
                                                        DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                       RoutineName);

                EvapVdot = (AirMassFlowRate * (OutSpecificHumRat - InSpecificHumRat)) / rho; // [m3/s]
                if (EvapVdot < 0.0) EvapVdot = 0.0;
            } else {
                EvapVdot = 0.0;
            }

        } else if (SimpleTower(TowerNum).EvapLossMode == EvapLossByUserFactor) {
            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidName,
                                   AverageWaterTemp,
                                                    DataPlant::PlantLoop(SimpleTower(TowerNum).LoopNum).FluidIndex,
                                   RoutineName);

            EvapVdot = SimpleTower(TowerNum).UserEvapLossFactor * (SimpleTower(TowerNum).InletWaterTemp - SimpleTower(TowerNum).OutletWaterTemp) * (SimpleTower(TowerNum).WaterMassFlowRate / rho);
            if (EvapVdot < 0.0) EvapVdot = 0.0;
        } else {
            // should never come here
        }

        //   amount of water lost due to drift
        DriftVdot = SimpleTower(TowerNum).DesignWaterFlowRate * SimpleTower(TowerNum).NumCellOn / SimpleTower(TowerNum).NumCell *
                    SimpleTower(TowerNum).DriftLossFraction * SimpleTower(TowerNum).__AirFlowRateRatio;

        if (SimpleTower(TowerNum).BlowdownMode == BlowdownBySchedule) {
            // Amount of water lost due to blow down (purging contaminants from tower basin)
            if (SimpleTower(TowerNum).SchedIDBlowdown > 0) {
                BlowDownVdot = ScheduleManager::GetCurrentScheduleValue(SimpleTower(TowerNum).SchedIDBlowdown);
            } else {
                BlowDownVdot = 0.0;
            }
        } else if (SimpleTower(TowerNum).BlowdownMode == BlowdownByConcentration) {
            if (SimpleTower(TowerNum).ConcentrationRatio > 2.0) { // protect divide by zero
                BlowDownVdot = EvapVdot / (SimpleTower(TowerNum).ConcentrationRatio - 1) - DriftVdot;
            } else {
                BlowDownVdot = EvapVdot - DriftVdot;
            }
            if (BlowDownVdot < 0.0) BlowDownVdot = 0.0;
        } else {
            // should never come here
        }

        // Added for fluid bypass
        if (SimpleTower(TowerNum).CapacityControl == CapacityControl_FluidBypass) {
            if (SimpleTower(TowerNum).EvapLossMode == EvapLossByUserFactor) EvapVdot *= (1 - SimpleTower(TowerNum).BypassFraction);
            DriftVdot *= (1 - SimpleTower(TowerNum).BypassFraction);
            BlowDownVdot *= (1 - SimpleTower(TowerNum).BypassFraction);
        }

        MakeUpVdot = EvapVdot + DriftVdot + BlowDownVdot;

        // set demand request in Water STorage if needed
        StarvedVdot = 0.0;
        TankSupplyVdot = 0.0;
        if (SimpleTower(TowerNum).SuppliedByWaterSystem) {

            // set demand request
            DataWater::WaterStorage(SimpleTower(TowerNum).WaterTankID).VdotRequestDemand(SimpleTower(TowerNum).WaterTankDemandARRID) = MakeUpVdot;

            AvailTankVdot = DataWater::WaterStorage(SimpleTower(TowerNum).WaterTankID)
                                .VdotAvailDemand(SimpleTower(TowerNum).WaterTankDemandARRID); // check what tank can currently provide

            TankSupplyVdot = MakeUpVdot;      // init
            if (AvailTankVdot < MakeUpVdot) { // calculate starved flow
                StarvedVdot = MakeUpVdot - AvailTankVdot;
                TankSupplyVdot = AvailTankVdot;
            }
        } else { // supplied by mains
        }

        //   total water usage
        // update report variables
        SimpleTower(TowerNum).EvaporationVdot = EvapVdot;
        SimpleTower(TowerNum).EvaporationVol = EvapVdot * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        SimpleTower(TowerNum).DriftVdot = DriftVdot;
        SimpleTower(TowerNum).DriftVol = DriftVdot * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        SimpleTower(TowerNum).BlowdownVdot = BlowDownVdot;
        SimpleTower(TowerNum).BlowdownVol = BlowDownVdot * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        SimpleTower(TowerNum).MakeUpVdot = MakeUpVdot;
        SimpleTower(TowerNum).MakeUpVol = MakeUpVdot * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        SimpleTower(TowerNum).TankSupplyVdot = TankSupplyVdot;
        SimpleTower(TowerNum).TankSupplyVol = TankSupplyVdot * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        SimpleTower(TowerNum).StarvedMakeUpVdot = StarvedVdot;
        SimpleTower(TowerNum).StarvedMakeUpVol = StarvedVdot * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
    }

    void UpdateTowers(int const TowerNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for passing results to the outlet water node.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt LowTempFmt("(' ',F6.2)");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CharErrOut;
        std::string CharLowOutletTemp;
        int LoopNum;
        int LoopSideNum;
        Real64 LoopMinTemp;
        // set node information

        DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).Temp = SimpleTower(TowerNum).OutletWaterTemp;

        LoopNum = SimpleTower(TowerNum).LoopNum;
        LoopSideNum = SimpleTower(TowerNum).LoopSideNum;
        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0 || DataGlobals::WarmupFlag) return;

        // Check flow rate through tower and compare to design flow rate, show warning if greater than Design * Mulitplier
        if (DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).MassFlowRate > SimpleTower(TowerNum).DesWaterMassFlowRate * SimpleTower(TowerNum).TowerMassFlowRateMultiplier) {
            ++SimpleTower(TowerNum).HighMassFlowErrorCount;
            if (SimpleTower(TowerNum).HighMassFlowErrorCount < 2) {
                ShowWarningError(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name + "\"");
                ShowContinueError(" Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate.");
                ShowContinueError(" Condenser Loop Mass Flow Rate = " + General::TrimSigDigits(DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).MassFlowRate, 6));
                ShowContinueError(" Tower Design Mass Flow Rate   = " + General::TrimSigDigits(SimpleTower(TowerNum).DesWaterMassFlowRate, 6));
                ShowContinueErrorTimeStamp("");
            } else {
                ShowRecurringWarningErrorAtEnd(
                    SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                        "\"  Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate error continues...",
                    SimpleTower(TowerNum).HighMassFlowErrorIndex,
                    DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).MassFlowRate,
                    DataLoopNode::Node(SimpleTower(TowerNum).WaterOutletNodeNum).MassFlowRate);
            }
        }

        // Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
        LoopMinTemp = DataPlant::PlantLoop(LoopNum).MinTemp;
        if (SimpleTower(TowerNum).OutletWaterTemp < LoopMinTemp && SimpleTower(TowerNum).WaterMassFlowRate > 0.0) {
            ++SimpleTower(TowerNum).OutletWaterTempErrorCount;
            ObjexxFCL::gio::write(CharLowOutletTemp, LowTempFmt) << LoopMinTemp;
            ObjexxFCL::gio::write(CharErrOut, LowTempFmt) << SimpleTower(TowerNum).OutletWaterTemp;
            strip(CharErrOut);
            if (SimpleTower(TowerNum).OutletWaterTempErrorCount < 2) {
                ShowWarningError(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name + "\"");
                ShowContinueError("Cooling tower water outlet temperature (" + CharErrOut +
                                  " C) is below the specified minimum condenser loop temp of " + stripped(CharLowOutletTemp) + " C");
                ShowContinueErrorTimeStamp("");
            } else {
                ShowRecurringWarningErrorAtEnd(
                    SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                        "\" Cooling tower water outlet temperature is below the specified minimum condenser loop temp error continues...",
                    SimpleTower(TowerNum).OutletWaterTempErrorIndex,
                    SimpleTower(TowerNum).OutletWaterTemp,
                    SimpleTower(TowerNum).OutletWaterTemp);
            }
        }

        // Check if water mass flow rate is small (e.g. no flow) and warn user
        if (SimpleTower(TowerNum).WaterMassFlowRate > 0.0 && SimpleTower(TowerNum).WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
            ++SimpleTower(TowerNum).SmallWaterMassFlowErrorCount;
            if (SimpleTower(TowerNum).SmallWaterMassFlowErrorCount < 2) {
                ShowWarningError(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name + "\"");
                ShowContinueError("Cooling tower water mass flow rate near zero.");
                ShowContinueErrorTimeStamp("");
                ShowContinueError("Actual Mass flow = " + General::TrimSigDigits(SimpleTower(TowerNum).WaterMassFlowRate, 2));
            } else {
                ShowRecurringWarningErrorAtEnd(SimpleTower(TowerNum).TowerType + " \"" + SimpleTower(TowerNum).Name +
                                                   "\"  Cooling tower water mass flow rate near zero error continues...",
                                               SimpleTower(TowerNum).SmallWaterMassFlowErrorIndex,
                                               SimpleTower(TowerNum).WaterMassFlowRate,
                                               SimpleTower(TowerNum).WaterMassFlowRate);
            }
        }
    }

    void ReportTowers(bool const RunFlag, int const TowerNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the report variables for the tower.

        Real64 const ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if (!RunFlag) {
            SimpleTower(TowerNum).InletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).OutletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).WaterMassFlowRate = SimpleTower(TowerNum).WaterMassFlowRate;
            SimpleTower(TowerNum).Qactual = 0.0;
            SimpleTower(TowerNum).FanPower = 0.0;
            SimpleTower(TowerNum).FanEnergy = 0.0;
            SimpleTower(TowerNum).AirFlowRatio = 0.0;
            SimpleTower(TowerNum).WaterAmountUsed = 0.0;
            SimpleTower(TowerNum).BasinHeaterPower = SimpleTower(TowerNum).BasinHeaterPower;
            SimpleTower(TowerNum).BasinHeaterConsumption = SimpleTower(TowerNum).BasinHeaterPower * ReportingConstant;
            SimpleTower(TowerNum).FanCyclingRatio = 0.0;
            SimpleTower(TowerNum).BypassFraction = 0.0; // added for fluid bypass
            SimpleTower(TowerNum).NumCellOn = 0;
            SimpleTower(TowerNum).SpeedSelected = 0;
        } else {
            SimpleTower(TowerNum).InletWaterTemp = DataLoopNode::Node(SimpleTower(TowerNum).WaterInletNodeNum).Temp;
            SimpleTower(TowerNum).WaterMassFlowRate = SimpleTower(TowerNum).WaterMassFlowRate;
            SimpleTower(TowerNum).FanEnergy = SimpleTower(TowerNum).FanPower * ReportingConstant;
            SimpleTower(TowerNum).AirFlowRatio = SimpleTower(TowerNum).__AirFlowRateRatio; // TODO: Remove __ version
            SimpleTower(TowerNum).WaterAmountUsed = SimpleTower(TowerNum).WaterUsage * ReportingConstant;
            SimpleTower(TowerNum).BasinHeaterPower = SimpleTower(TowerNum).BasinHeaterPower;
            SimpleTower(TowerNum).BasinHeaterConsumption = SimpleTower(TowerNum).BasinHeaterPower * ReportingConstant;
            SimpleTower(TowerNum).FanCyclingRatio = SimpleTower(TowerNum).FanCyclingRatio;
            SimpleTower(TowerNum).BypassFraction = SimpleTower(TowerNum).BypassFraction; // added for fluid bypass
            SimpleTower(TowerNum).NumCellOn = SimpleTower(TowerNum).NumCellOn;
            SimpleTower(TowerNum).SpeedSelected = SimpleTower(TowerNum).SpeedSelected;
        }
    }

} // namespace CondenserLoopTowers

} // namespace EnergyPlus
