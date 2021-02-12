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
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HWBaseboardRadiator.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace HWBaseboardRadiator {

    // Module -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Water)

    // Module containing the routines dealing with the hot water baseboard heaters

    // MODULE INFORMATION:
    //       AUTHOR         Daeho Kang
    //       DATE WRITTEN   Aug 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // The purpose of this module is to simulate hot water baseboard heaters.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // 1. I=B=R Ratings for Baseboards, Baseboard Radiation,
    //   Finned Tube (Commercial) Radiation, and Indirect Fired Water Heaters, January 2007 Edition
    // 2. Incropera and DeWitt, Fundamentals of Heat and Mass Transfer, Chapter 11.3 and 11.4,
    //   eq. 11.15, 11.17, and 11.33

    // OTHER NOTES:
    // na

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using DataPlant::TypeOf_Baseboard_Rad_Conv_Water;
    using DataZoneEquipment::CheckZoneEquipmentList;

    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    // Data
    // MODULE PARAMETER DEFINITIONS

    std::string const cCMO_BBRadiator_Water("ZoneHVAC:Baseboard:RadiantConvective:Water");
    std::string const cCMO_BBRadiator_Water_Design("ZoneHVAC:Baseboard:RadiantConvective:Water:Design");

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    int NumHWBaseboards(0);
    int NumHWBaseboardDesignObjs(0);
    Array1D<Real64> QBBRadSource;         // Need to keep the last value in case we are still iterating
    Array1D<Real64> QBBRadSrcAvg;         // Need to keep the last value in case we are still iterating
    Array1D<Real64> ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source

    // Record keeping variables used to calculate QBBRadSrcAvg locally
    Array1D<Real64> LastQBBRadSrc;      // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating
    Array1D_bool MySizeFlag;
    Array1D_bool CheckEquipName;
    Array1D_bool SetLoopIndexFlag; // get loop number flag
    Array1D_string HWBaseboardDesignNames;

    // SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

    // Object Data
    Array1D<HWBaseboardParams> HWBaseboard;
    Array1D<HWBaseboardDesignData> HWBaseboardDesignObject;
    Array1D<HWBaseboardNumericFieldData> HWBaseboardNumericFields;
    Array1D<HWBaseboardDesignNumericFieldData> HWBaseboardDesignNumericFields;

    // Functions

    void SimHWBaseboard(EnergyPlusData &state,
                        std::string const &EquipName,
                        int const ActualZoneNum,
                        int const ControlledZoneNum,
                        bool const FirstHVACIteration,
                        Real64 &PowerMet,
                        int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the Baseboard Radiators.

        // Using/Aliasing
        using DataLoopNode::Node;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum;               // Index of unit in baseboard array
        static bool GetInputFlag(true); // One time get input flag
        Real64 QZnReq;                  // Zone load not yet satisfied
        Real64 MaxWaterFlow;
        Real64 MinWaterFlow;

        if (GetInputFlag) {
            GetHWBaseboardInput(state);
            GetInputFlag = false;
        }

        // Find the correct Baseboard Equipment
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(EquipName, HWBaseboard, &HWBaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError(state, "SimHWBaseboard: Unit not found=" + EquipName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > NumHWBaseboards || BaseboardNum < 1) {
                ShowFatalError(state,
                               format("SimHWBaseboard:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      BaseboardNum,
                                      NumHWBaseboards,
                                      EquipName));
            }
            if (CheckEquipName(BaseboardNum)) {
                if (EquipName != HWBaseboard(BaseboardNum).EquipID) {
                    ShowFatalError(state,
                                   format("SimHWBaseboard: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          BaseboardNum,
                                          EquipName,
                                          HWBaseboard(BaseboardNum).EquipID));
                }
                CheckEquipName(BaseboardNum) = false;
            }
        }

        if (CompIndex > 0) {
            HWBaseboardDesignData HWBaseboardDesignDataObject{HWBaseboardDesignObject(HWBaseboard(BaseboardNum).DesignObjectPtr)}; // Contains the data for variable flow hydronic systems

            InitHWBaseboard(state, BaseboardNum, ControlledZoneNum, FirstHVACIteration);

            QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToHeatSP;

            // On the first HVAC iteration the system values are given to the controller, but after that
            // the demand limits are in place and there needs to be feedback to the Zone Equipment
            if (FirstHVACIteration) {
                MaxWaterFlow = HWBaseboard(BaseboardNum).WaterMassFlowRateMax;
                MinWaterFlow = 0.0;
            } else {
                MaxWaterFlow = Node(HWBaseboard(BaseboardNum).WaterInletNode).MassFlowRateMaxAvail;
                MinWaterFlow = Node(HWBaseboard(BaseboardNum).WaterInletNode).MassFlowRateMinAvail;
            }

            {
                auto const SELECT_CASE_var(HWBaseboard(BaseboardNum).EquipType);


                if (SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Water) { // 'ZoneHVAC:Baseboard:RadiantConvective:Water'
                    ControlCompOutput(state,
                                      HWBaseboard(BaseboardNum).EquipID,
                                      cCMO_BBRadiator_Water,
                                      BaseboardNum,
                                      FirstHVACIteration,
                                      QZnReq,
                                      HWBaseboard(BaseboardNum).WaterInletNode,
                                      MaxWaterFlow,
                                      MinWaterFlow,
                                      HWBaseboardDesignDataObject.Offset,
                                      HWBaseboard(BaseboardNum).ControlCompTypeNum,
                                      HWBaseboard(BaseboardNum).CompErrIndex,
                                      _,
                                      _,
                                      _,
                                      _,
                                      _,
                                      HWBaseboard(BaseboardNum).LoopNum,
                                      HWBaseboard(BaseboardNum).LoopSideNum,
                                      HWBaseboard(BaseboardNum).BranchNum);
                } else {
                    ShowSevereError(state, "SimBaseboard: Errors in Baseboard=" + HWBaseboard(BaseboardNum).EquipID);
                    ShowContinueError(state, format("Invalid or unimplemented equipment type={}", HWBaseboard(BaseboardNum).EquipType));
                    ShowFatalError(state, "Preceding condition causes termination.");
                }
            }

            PowerMet = HWBaseboard(BaseboardNum).TotPower;

            UpdateHWBaseboard(state, BaseboardNum);

            ReportHWBaseboard(BaseboardNum);

        } else {
            ShowFatalError(state, "SimHWBaseboard: Unit not found=" + EquipName);
        }
    }

    void GetHWBaseboardInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Aug 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the baseboard units.

        // METHODOLOGY EMPLOYED:
        // Standard input processor calls.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataLoopNode::Node;
        using DataLoopNode::NodeConnectionType_Inlet;
        using DataLoopNode::NodeConnectionType_Outlet;
        using DataLoopNode::NodeType_Water;
        using DataLoopNode::ObjectIsNotParent;
        using DataSizing::AutoSize;
        using DataSizing::CapacityPerFloorArea;
        using DataSizing::FractionOfAutosizedHeatingCapacity;
        using DataSizing::HeatingDesignCapacity;
        using DataSurfaces::Surface;

        using GlobalNames::VerifyUniqueBaseboardName;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataIPShortCuts;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetHWBaseboardInput:");
        Real64 const MaxFraction(1.0);
        Real64 const MinFraction(0.0);
        Real64 const MaxWaterTempAvg(150.0);        // Maximum limit of average water temperature in degree C
        Real64 const MinWaterTempAvg(20.0);         // Minimum limit of average water temperature in degree C
        Real64 const HighWaterMassFlowRate(10.0);   // Maximum limit of water mass flow rate in kg/s
        Real64 const LowWaterMassFlowRate(0.00001); // Minimum limit of water mass flow rate in kg/s
        Real64 const MaxWaterFlowRate(10.0);        // Maximum limit of water volume flow rate in m3/s
        Real64 const MinWaterFlowRate(0.00001);     // Minimum limit of water volume flow rate in m3/s
        Real64 const WaterMassFlowDefault(0.063);   // Default water mass flow rate in kg/s
        //    INTEGER, PARAMETER   :: MaxDistribSurfaces    = 20         ! Maximum number of surfaces that a baseboard heater can radiate to
        int const MinDistribSurfaces(1);                  // Minimum number of surfaces that a baseboard heater can radiate to
        int const iHeatCAPMAlphaNum(2);                   // get input index to HW baseboard heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(3);       // get input index to HW baseboard heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(1); // get input index to HW baseboard heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            2); //  get input index to HW baseboard heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AllFracsSummed; // Sum of the fractions radiant
        int BaseboardNum;      // Baseboard number
        int BaseboardDesignNum;
        int NumAlphas;         // Number of Alphas for each GetobjectItem call
        int NumNumbers;        // Number of Numbers for each GetobjectItem call
        int SurfNum;           // Surface number Do loop counter
        int IOStat;
        static bool ErrorsFound(false); // If errors detected in input

        NumHWBaseboards = inputProcessor->getNumObjectsFound(state, cCMO_BBRadiator_Water);
        NumHWBaseboardDesignObjs = inputProcessor->getNumObjectsFound(state, cCMO_BBRadiator_Water_Design);

        // Count total number of baseboard units

        HWBaseboard.allocate(NumHWBaseboards);
        HWBaseboardDesignObject.allocate(NumHWBaseboardDesignObjs);
        CheckEquipName.allocate(NumHWBaseboards);
        HWBaseboardNumericFields.allocate(NumHWBaseboards);
        HWBaseboardDesignNumericFields.allocate(NumHWBaseboardDesignObjs);
        HWBaseboardDesignNames.allocate(NumHWBaseboardDesignObjs);
        CheckEquipName = true;

        // Get the data from the user input related to design data for baseboard heaters
        for (BaseboardDesignNum = 1; BaseboardDesignNum <= NumHWBaseboardDesignObjs; ++BaseboardDesignNum) {
            inputProcessor->getObjectItem(state,
                                          cCMO_BBRadiator_Water_Design,
                                          BaseboardDesignNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            HWBaseboardDesignNumericFields(BaseboardDesignNum).FieldNames.allocate(NumNumbers);
            HWBaseboardDesignNumericFields(BaseboardDesignNum).FieldNames = "";
            HWBaseboardDesignNumericFields(BaseboardDesignNum).FieldNames = cNumericFieldNames;

            UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueBaseboardName(state, cCMO_BBRadiator_Water_Design,
                                      cAlphaArgs(1),
                                      ErrorsFound,
                                      cCMO_BBRadiator_Water_Design + " Name");

            HWBaseboardDesignObject(BaseboardDesignNum).designName = cAlphaArgs(1);                     // Name of this baseboard design object
            HWBaseboardDesignNames(BaseboardDesignNum) = cAlphaArgs(1);

            // Determine HW radiant baseboard heating design capacity sizing method
            if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                HWBaseboardDesignObject(BaseboardDesignNum).HeatingCapMethod = HeatingDesignCapacity;
            } else if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
            HWBaseboardDesignObject(BaseboardDesignNum).HeatingCapMethod = CapacityPerFloorArea;
                if (!lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    HWBaseboardDesignObject(BaseboardDesignNum).ScaledHeatingCapacity = rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                    if (HWBaseboardDesignObject(BaseboardDesignNum).ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboardDesignObject(BaseboardDesignNum).designName);
                        ShowContinueError(state, "Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum),
                                                 rNumericArgs(iHeatCapacityPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                    } else if (HWBaseboardDesignObject(BaseboardDesignNum).ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboardDesignObject(BaseboardDesignNum).designName);
                        ShowContinueError(state, "Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state, "Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboardDesignObject(BaseboardDesignNum).designName);
                    ShowContinueError(state, "Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                HWBaseboardDesignObject(BaseboardDesignNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    HWBaseboardDesignObject(BaseboardDesignNum).ScaledHeatingCapacity = rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                    if (HWBaseboardDesignObject(BaseboardDesignNum).ScaledHeatingCapacity < 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboardDesignObject(BaseboardDesignNum).designName);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum),
                                                 rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboardDesignObject(BaseboardDesignNum).designName);
                    ShowContinueError(state, "Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            }

            HWBaseboardDesignObject(BaseboardDesignNum).Offset = rNumericArgs(3);
            // Set default convergence tolerance
            if (HWBaseboardDesignObject(BaseboardDesignNum).Offset <= 0.0) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water_Design + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(3) +
                                        " was less than the allowable minimum.");
                ShowContinueError(state, format("...reset to a default value=[{:.2R}].", MaxFraction));
                HWBaseboardDesignObject(BaseboardDesignNum).Offset = 0.001;

            }

            HWBaseboardDesignObject(BaseboardDesignNum).FracRadiant = rNumericArgs(4);
            if (HWBaseboardDesignObject(BaseboardDesignNum).FracRadiant < MinFraction) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(4) +
                                        " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinFraction));
                HWBaseboardDesignObject(BaseboardDesignNum).FracRadiant = MinFraction;
            }
            if (HWBaseboardDesignObject(BaseboardDesignNum).FracRadiant > MaxFraction) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(4) +
                                        " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
                HWBaseboardDesignObject(BaseboardDesignNum).FracRadiant = MaxFraction;
            }

            HWBaseboardDesignObject(BaseboardDesignNum).FracDistribPerson = rNumericArgs(5);
            if (HWBaseboardDesignObject(BaseboardDesignNum).FracDistribPerson < MinFraction) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(5) +
                                        " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.3R}].", MinFraction));
                HWBaseboardDesignObject(BaseboardDesignNum).FracDistribPerson = MinFraction;
            }
            if (HWBaseboardDesignObject(BaseboardDesignNum).FracDistribPerson > MaxFraction) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(5) +
                                        " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.3R}].", MaxFraction));
                HWBaseboardDesignObject(BaseboardDesignNum).FracDistribPerson = MaxFraction;
            }
        }

        // Get the data from the user input related to baseboard heaters
        for (BaseboardNum = 1; BaseboardNum <= NumHWBaseboards; ++BaseboardNum) {

            inputProcessor->getObjectItem(state,
                                          cCMO_BBRadiator_Water,
                                          BaseboardNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            HWBaseboardNumericFields(BaseboardNum).FieldNames.allocate(NumNumbers);
            HWBaseboardNumericFields(BaseboardNum).FieldNames = "";
            HWBaseboardNumericFields(BaseboardNum).FieldNames = cNumericFieldNames;
            UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueBaseboardName(state, cCMO_BBRadiator_Water, cAlphaArgs(1), ErrorsFound, cCMO_BBRadiator_Water + " Name");

            HWBaseboard(BaseboardNum).EquipID = cAlphaArgs(1);                     // Name of this baseboard
            HWBaseboard(BaseboardNum).EquipType = TypeOf_Baseboard_Rad_Conv_Water; //'ZoneHVAC:Baseboard:RadiantConvective:Water'

            HWBaseboard(BaseboardNum).designObjectName = cAlphaArgs(2);                     // Name of the design object for this baseboard
            HWBaseboard(BaseboardNum).DesignObjectPtr = UtilityRoutines::FindItemInList( HWBaseboard(BaseboardNum).designObjectName, HWBaseboardDesignNames);
            HWBaseboardDesignData HWBaseboardDesignDataObject{HWBaseboardDesignObject(HWBaseboard(BaseboardNum).DesignObjectPtr)}; // Contains the data for the design object

            // Get schedule
            HWBaseboard(BaseboardNum).Schedule = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                HWBaseboard(BaseboardNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                HWBaseboard(BaseboardNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (HWBaseboard(BaseboardNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Get inlet node number
            HWBaseboard(BaseboardNum).WaterInletNode = GetOnlySingleNode(state,
                cAlphaArgs(4), ErrorsFound, cCMO_BBRadiator_Water, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            // Get outlet node number
            HWBaseboard(BaseboardNum).WaterOutletNode = GetOnlySingleNode(state,
                cAlphaArgs(5), ErrorsFound, cCMO_BBRadiator_Water, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(state, cCMO_BBRadiator_Water, cAlphaArgs(1), cAlphaArgs(4), cAlphaArgs(5), "Hot Water Nodes");

            HWBaseboard(BaseboardNum).WaterTempAvg = rNumericArgs(1);
            if (HWBaseboard(BaseboardNum).WaterTempAvg > MaxWaterTempAvg + 0.001) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(1) +
                                 " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxWaterTempAvg));
                HWBaseboard(BaseboardNum).WaterTempAvg = MaxWaterTempAvg;
            } else if (HWBaseboard(BaseboardNum).WaterTempAvg < MinWaterTempAvg - 0.001) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(1) +
                                 " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinWaterTempAvg));
                HWBaseboard(BaseboardNum).WaterTempAvg = MinWaterTempAvg;
            }

            HWBaseboard(BaseboardNum).WaterMassFlowRateStd = rNumericArgs(2);
            if (HWBaseboard(BaseboardNum).WaterMassFlowRateStd < LowWaterMassFlowRate - 0.0001 ||
                HWBaseboard(BaseboardNum).WaterMassFlowRateStd > HighWaterMassFlowRate + 0.0001) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(2) +
                                 " is an invalid Standard Water mass flow rate.");
                ShowContinueError(state, format("...reset to a default value=[{:.1R}].", WaterMassFlowDefault));
                HWBaseboard(BaseboardNum).WaterMassFlowRateStd = WaterMassFlowDefault;
            }

            // Determine HW radiant baseboard heating design capacity sizing method
            HWBaseboard(BaseboardNum).HeatingCapMethod = HWBaseboardDesignDataObject.HeatingCapMethod;
            if (HWBaseboard(BaseboardNum).HeatingCapMethod == HeatingDesignCapacity) {
                if (!lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                    HWBaseboard(BaseboardNum).ScaledHeatingCapacity = rNumericArgs(iHeatDesignCapacityNumericNum);
                    if (HWBaseboard(BaseboardNum).ScaledHeatingCapacity < 0.0 && HWBaseboard(BaseboardNum).ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboard(BaseboardNum).EquipID);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 cNumericFieldNames(iHeatDesignCapacityNumericNum),
                                                 rNumericArgs(iHeatDesignCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboard(BaseboardNum).EquipID);
                    ShowContinueError(state, "Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state, "Blank field not allowed for " + cNumericFieldNames(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (HWBaseboard(BaseboardNum).HeatingCapMethod == CapacityPerFloorArea) {
                HWBaseboard(BaseboardNum).ScaledHeatingCapacity = HWBaseboardDesignDataObject.ScaledHeatingCapacity;

            } else if (HWBaseboard(BaseboardNum).HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
                HWBaseboard(BaseboardNum).ScaledHeatingCapacity = HWBaseboardDesignDataObject.ScaledHeatingCapacity;

            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + HWBaseboard(BaseboardNum).EquipID);
                ShowContinueError(state, "Illegal " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }

            HWBaseboard(BaseboardNum).WaterVolFlowRateMax = rNumericArgs(4);
            if (std::abs(HWBaseboard(BaseboardNum).WaterVolFlowRateMax) <= MinWaterFlowRate) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(4) +
                                 " was less than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinWaterFlowRate));
                HWBaseboard(BaseboardNum).WaterVolFlowRateMax = MinWaterFlowRate;
            } else if (HWBaseboard(BaseboardNum).WaterVolFlowRateMax > MaxWaterFlowRate) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(4) +
                                 " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxWaterFlowRate));
                HWBaseboard(BaseboardNum).WaterVolFlowRateMax = MaxWaterFlowRate;
            }


            // Remaining fraction is added to the zone as convective heat transfer
            AllFracsSummed = HWBaseboardDesignDataObject.FracDistribPerson;
            if (AllFracsSummed > MaxFraction) {
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) +
                                 "\", Fraction Radiant was higher than the allowable maximum.");
                HWBaseboardDesignDataObject.FracRadiant = MaxFraction;
                HWBaseboard(BaseboardNum).FracConvect = 0.0;
            } else {
                HWBaseboard(BaseboardNum).FracConvect = 1.0 - AllFracsSummed;
            }

            HWBaseboard(BaseboardNum).TotSurfToDistrib = NumNumbers - 4;
            //      IF (HWBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
            //        CALL ShowWarningError(state, RoutineName//cCMO_BBRadiator_Water//'="'//TRIM(cAlphaArgs(1))// &
            //          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
            //        CALL ShowContinueError(state, '...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))// &
            //           '] will be processed.')
            //        HWBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
            //      END IF
            if ((HWBaseboard(BaseboardNum).TotSurfToDistrib < MinDistribSurfaces) && (HWBaseboardDesignDataObject.FracRadiant > MinFraction)) {
                ShowSevereError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) +
                                "\", the number of surface/radiant fraction groups entered was less than the allowable minimum.");
                ShowContinueError(state, format("...the minimum that must be entered=[{}].", MinDistribSurfaces));
                ErrorsFound = true;
                HWBaseboard(BaseboardNum).TotSurfToDistrib = 0; // error
            }

            HWBaseboard(BaseboardNum).SurfaceName.allocate(HWBaseboard(BaseboardNum).TotSurfToDistrib);
            HWBaseboard(BaseboardNum).SurfaceName = "";
            HWBaseboard(BaseboardNum).SurfacePtr.allocate(HWBaseboard(BaseboardNum).TotSurfToDistrib);
            HWBaseboard(BaseboardNum).SurfacePtr = 0;
            HWBaseboard(BaseboardNum).FracDistribToSurf.allocate(HWBaseboard(BaseboardNum).TotSurfToDistrib);
            HWBaseboard(BaseboardNum).FracDistribToSurf = 0.0;

            // search zone equipment list structure for zone index
            for (int ctrlZone = 1; ctrlZone <= state.dataGlobal->NumOfZones; ++ctrlZone) {
                for (int zoneEquipTypeNum = 1; zoneEquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ctrlZone).NumOfEquipTypes; ++zoneEquipTypeNum) {
                    if (state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipType_Num(zoneEquipTypeNum) == DataZoneEquipment::BBWater_Num &&
                        state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipName(zoneEquipTypeNum) == HWBaseboard(BaseboardNum).EquipID) {
                        HWBaseboard(BaseboardNum).ZonePtr = ctrlZone;
                    }
                }
            }
            if (HWBaseboard(BaseboardNum).ZonePtr <= 0) {
                ShowSevereError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + HWBaseboard(BaseboardNum).EquipID +
                                "\" is not on any ZoneHVAC:EquipmentList.");
                ErrorsFound = true;
                continue;
            }

            AllFracsSummed = HWBaseboardDesignDataObject.FracDistribPerson;
            for (SurfNum = 1; SurfNum <= HWBaseboard(BaseboardNum).TotSurfToDistrib; ++SurfNum) {
                HWBaseboard(BaseboardNum).SurfaceName(SurfNum) = cAlphaArgs(SurfNum + 5);
                HWBaseboard(BaseboardNum).SurfacePtr(SurfNum) =
                    HeatBalanceIntRadExchange::GetRadiantSystemSurface(state, cCMO_BBRadiator_Water,
                                                                       HWBaseboard(BaseboardNum).EquipID,
                                                                       HWBaseboard(BaseboardNum).ZonePtr,
                                                                       HWBaseboard(BaseboardNum).SurfaceName(SurfNum),
                                                                       ErrorsFound);
                HWBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) = rNumericArgs(SurfNum + 4);
                if (HWBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) > MaxFraction) {
                    ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(SurfNum + 4) +
                                     "was greater than the allowable maximum.");
                    ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
                    HWBaseboard(BaseboardNum).TotSurfToDistrib = MaxFraction;
                }
                if (HWBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) < MinFraction) {
                    ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(SurfNum + 4) +
                                     "was less than the allowable minimum.");
                    ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MinFraction));
                    HWBaseboard(BaseboardNum).TotSurfToDistrib = MinFraction;
                }
                if (HWBaseboard(BaseboardNum).SurfacePtr(SurfNum) != 0) {
                    Surface(HWBaseboard(BaseboardNum).SurfacePtr(SurfNum)).IntConvSurfGetsRadiantHeat = true;
                }

                AllFracsSummed += HWBaseboard(BaseboardNum).FracDistribToSurf(SurfNum);
            } // Surfaces

            if (AllFracsSummed > (MaxFraction + 0.01)) {
                ShowSevereError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) +
                                "\", Summed radiant fractions for people + surface groups > 1.0");
                ErrorsFound = true;
            }
            if ((AllFracsSummed < (MaxFraction - 0.01)) &&
                (HWBaseboardDesignDataObject.FracRadiant > MinFraction)) { // User didn't distribute all of the | radiation warn that some will be lost
                ShowWarningError(state, RoutineName + cCMO_BBRadiator_Water + "=\"" + cAlphaArgs(1) +
                                 "\", Summed radiant fractions for people + surface groups < 1.0");
                ShowContinueError(state, "The rest of the radiant energy delivered by the baseboard heater will be lost");
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + cCMO_BBRadiator_Water + "Errors found getting input. Program terminates.");
        }

        // Setup Report variables for the Coils
        for (BaseboardNum = 1; BaseboardNum <= NumHWBaseboards; ++BaseboardNum) {
            // CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Water'
            SetupOutputVariable(state, "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                HWBaseboard(BaseboardNum).TotPower,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state, "Baseboard Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                HWBaseboard(BaseboardNum).ConvPower,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                HWBaseboard(BaseboardNum).RadPower,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                HWBaseboard(BaseboardNum).TotEnergy,
                                "System",
                                "Sum",
                                HWBaseboard(BaseboardNum).EquipID,
                                _,
                                "ENERGYTRANSFER",
                                "BASEBOARD",
                                _,
                                "System");

            SetupOutputVariable(state, "Baseboard Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                HWBaseboard(BaseboardNum).ConvEnergy,
                                "System",
                                "Sum",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                HWBaseboard(BaseboardNum).RadEnergy,
                                "System",
                                "Sum",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Hot Water Energy",
                                OutputProcessor::Unit::J,
                                HWBaseboard(BaseboardNum).Energy,
                                "System",
                                "Sum",
                                HWBaseboard(BaseboardNum).EquipID,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "BASEBOARD",
                                _,
                                "System");
            SetupOutputVariable(state, "Baseboard Hot Water Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                HWBaseboard(BaseboardNum).WaterMassFlowRate,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                HWBaseboard(BaseboardNum).AirMassFlowRate,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Air Inlet Temperature",
                                OutputProcessor::Unit::C,
                                HWBaseboard(BaseboardNum).AirInletTemp,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Air Outlet Temperature",
                                OutputProcessor::Unit::C,
                                HWBaseboard(BaseboardNum).AirOutletTemp,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Water Inlet Temperature",
                                OutputProcessor::Unit::C,
                                HWBaseboard(BaseboardNum).WaterInletTemp,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Water Outlet Temperature",
                                OutputProcessor::Unit::C,
                                HWBaseboard(BaseboardNum).WaterOutletTemp,
                                "System",
                                "Average",
                                HWBaseboard(BaseboardNum).EquipID);
        }
    }

    void InitHWBaseboard(EnergyPlusData &state, int const BaseboardNum, int const ControlledZoneNumSub, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //                      Rick Strand
        //       DATE WRITTEN   Nov 1997
        //                      Feb 2001
        //       MODIFIED       Aug 2007 Daeho Kang (Add radiant component)
        //                      Sept 2010 Brent Griffith (plant interactions)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the baseboard units, and determines the UA values during simulation.

        // METHODOLOGY EMPLOYED:
        // The initialization subrotines both in high temperature radiant radiator
        // and convective only baseboard radiator are combined and modified. In addition,
        // an UA value calculation by LMTD method is added.
        // The heater is assumed to be crossflow with both fluids unmixed.

        // REFERENCES:
        // 1. Incropera and DeWitt, Fundamentals of Heat and Mass Transfer
        // Chapter 11.3, p. 510, eq. 11.15 and 11.17
        // 2. I=B=R Ratings for Baseboards, Baseboard Radiation, Finned Tube (Commercial) Radiation,
        // and Indirect Fired Water Heaters, January 2007 Edition

        // Using/Aliasing
        using DataLoopNode::Node;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Constant(0.0062); // Constant of linear equation for air mass flow rate
        Real64 const Coeff(0.0000275); // Correlation coefficient to capacity
        static std::string const RoutineName("BaseboardRadiatorWater:InitHWBaseboard");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool MyOneTimeFlag(true);
        static bool ZoneEquipmentListChecked(false);
        static Array1D_bool MyEnvrnFlag;
        int Loop;
        int WaterInletNode;
        int ZoneNode;
        int ZoneNum;
        Real64 RhoAirStdInit;
        Real64 rho; // local fluid density
        Real64 Cp;  // local fluid specific heat
        bool errFlag;

        // Do the one time initializations
        if (MyOneTimeFlag) {

            // Initialize the environment and sizing flags
            MyEnvrnFlag.allocate(NumHWBaseboards);
            MySizeFlag.allocate(NumHWBaseboards);
            ZeroSourceSumHATsurf.dimension(state.dataGlobal->NumOfZones, 0.0);
            QBBRadSource.dimension(NumHWBaseboards, 0.0);
            QBBRadSrcAvg.dimension(NumHWBaseboards, 0.0);
            LastQBBRadSrc.dimension(NumHWBaseboards, 0.0);
            LastSysTimeElapsed.dimension(NumHWBaseboards, 0.0);
            LastTimeStepSys.dimension(NumHWBaseboards, 0.0);
            SetLoopIndexFlag.allocate(NumHWBaseboards);
            MyEnvrnFlag = true;
            MySizeFlag = true;
            MyOneTimeFlag = false;
            SetLoopIndexFlag = true;
            for (Loop = 1; Loop <= NumHWBaseboards; ++Loop) {
                // Air mass flow rate is obtained from the following linear equation (reset if autosize is used)
                // m_dot = 0.0062 + 2.75e-05*q
                HWBaseboard(Loop).AirMassFlowRateStd = Constant + Coeff * HWBaseboard(Loop).RatedCapacity;
            }
        }

        if (HWBaseboard(BaseboardNum).ZonePtr <= 0) HWBaseboard(BaseboardNum).ZonePtr = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ActualZoneNum;

        // Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
        if (!ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= NumHWBaseboards; ++Loop) {
                if (CheckZoneEquipmentList(state, cCMO_BBRadiator_Water, HWBaseboard(Loop).EquipID)) continue;
                ShowSevereError(state, "InitBaseboard: Unit=[" + cCMO_BBRadiator_Water + ',' + HWBaseboard(Loop).EquipID +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (SetLoopIndexFlag(BaseboardNum)) {
            if (allocated(state.dataPlnt->PlantLoop)) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        HWBaseboard(BaseboardNum).EquipID,
                                        HWBaseboard(BaseboardNum).EquipType,
                                        HWBaseboard(BaseboardNum).LoopNum,
                                        HWBaseboard(BaseboardNum).LoopSideNum,
                                        HWBaseboard(BaseboardNum).BranchNum,
                                        HWBaseboard(BaseboardNum).CompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowFatalError(state, "InitHWBaseboard: Program terminated for previous conditions.");
                }
                SetLoopIndexFlag(BaseboardNum) = false;
            }
        }

        if (!state.dataGlobal->SysSizingCalc && MySizeFlag(BaseboardNum) && !SetLoopIndexFlag(BaseboardNum)) {
            // For each coil, do the sizing once
            SizeHWBaseboard(state, BaseboardNum);
            MySizeFlag(BaseboardNum) = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(BaseboardNum)) {
            // Initialize
            RhoAirStdInit = state.dataEnvrn->StdRhoAir;
            WaterInletNode = HWBaseboard(BaseboardNum).WaterInletNode;

            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                   DataGlobalConstants::HWInitConvTemp,
                                   state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                   RoutineName);

            HWBaseboard(BaseboardNum).WaterMassFlowRateMax = rho * HWBaseboard(BaseboardNum).WaterVolFlowRateMax;

            InitComponentNodes(0.0,
                               HWBaseboard(BaseboardNum).WaterMassFlowRateMax,
                               HWBaseboard(BaseboardNum).WaterInletNode,
                               HWBaseboard(BaseboardNum).WaterOutletNode,
                               HWBaseboard(BaseboardNum).LoopNum,
                               HWBaseboard(BaseboardNum).LoopSideNum,
                               HWBaseboard(BaseboardNum).BranchNum,
                               HWBaseboard(BaseboardNum).CompNum);

            Node(WaterInletNode).Temp = 60.0;

            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                       Node(WaterInletNode).Temp,
                                       state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                       RoutineName);

            Node(WaterInletNode).Enthalpy = Cp * Node(WaterInletNode).Temp;
            Node(WaterInletNode).Quality = 0.0;
            Node(WaterInletNode).Press = 0.0;
            Node(WaterInletNode).HumRat = 0.0;

            ZeroSourceSumHATsurf = 0.0;
            QBBRadSource = 0.0;
            QBBRadSrcAvg = 0.0;
            LastQBBRadSrc = 0.0;
            LastSysTimeElapsed = 0.0;
            LastTimeStepSys = 0.0;

            MyEnvrnFlag(BaseboardNum) = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag(BaseboardNum) = true;
        }

        if (state.dataGlobal->BeginTimeStepFlag && FirstHVACIteration) {
            ZoneNum = HWBaseboard(BaseboardNum).ZonePtr;
            ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum);
            QBBRadSrcAvg(BaseboardNum) = 0.0;
            LastQBBRadSrc(BaseboardNum) = 0.0;
            LastSysTimeElapsed(BaseboardNum) = 0.0;
            LastTimeStepSys(BaseboardNum) = 0.0;
        }

        // Do the every time step initializations
        WaterInletNode = HWBaseboard(BaseboardNum).WaterInletNode;
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ZoneNode;
        HWBaseboard(BaseboardNum).WaterMassFlowRate = Node(WaterInletNode).MassFlowRate;
        HWBaseboard(BaseboardNum).WaterInletTemp = Node(WaterInletNode).Temp;
        HWBaseboard(BaseboardNum).WaterInletEnthalpy = Node(WaterInletNode).Enthalpy;
        HWBaseboard(BaseboardNum).AirInletTemp = Node(ZoneNode).Temp;
        HWBaseboard(BaseboardNum).AirInletHumRat = Node(ZoneNode).HumRat;

        HWBaseboard(BaseboardNum).TotPower = 0.0;
        HWBaseboard(BaseboardNum).Power = 0.0;
        HWBaseboard(BaseboardNum).ConvPower = 0.0;
        HWBaseboard(BaseboardNum).RadPower = 0.0;
        HWBaseboard(BaseboardNum).TotEnergy = 0.0;
        HWBaseboard(BaseboardNum).Energy = 0.0;
        HWBaseboard(BaseboardNum).ConvEnergy = 0.0;
        HWBaseboard(BaseboardNum).RadEnergy = 0.0;
    }

    void SizeHWBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2009 Daeho Kang (Add UA autosizing by LMTD)
        //                      Aug 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B.Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing hot water baseboard components

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::HeatingCapacitySizing;
        using DataLoopNode::Node;

        using PlantUtilities::RegisterPlantCompDesignFlow;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const AirInletTempStd(18.0); // I=B=R rating document
        Real64 const CPAirStd(1005.0);      // Average specific heat of air at between 25C and 40C in J/kg-k
        Real64 const Constant(0.0062);      // Constant of linear equation for air mass flow rate
        Real64 const Coeff(0.0000275);      // Correlation coefficient to capacity
        static std::string const RoutineName("SizeHWBaseboard");
        static std::string const RoutineNameFull("BaseboardRadiatorWater:SizeHWBaseboard");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum;     // do loop index for plant sizing
        int PltSizHeatNum; // index of plant sizing object for 1st heating loop
        Real64 DesCoilLoad;
        Real64 WaterInletTempStd;
        Real64 WaterOutletTempStd;
        Real64 AirOutletTempStd;
        Real64 DeltaT1;
        Real64 DeltaT2;
        Real64 LMTD;
        Real64 AirMassFlowRate;
        Real64 WaterMassFlowRateStd;
        Real64 rho;                     // local fluid density
        Real64 Cp;                      // local fluid specific heat
        bool ErrorsFound;               // If errors detected in input
        bool FlowAutoSize;              // Indicator to autosize for maximum water vloume flow
        bool CapAutoSize;               // Indicator to autosize for capacity
        Real64 WaterVolFlowRateMaxDes;  // Design maximum water volume flow for reproting
        Real64 WaterVolFlowRateMaxUser; // User hard-sized maximum water volume flow for reproting
        Real64 RatedCapacityDes;        // Design rated capacity for reproting

        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;          // autosized value of coil input field
        int FieldNum = 1;         // IDD numeric field number where input field description is found
        int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                          // HeatingCapacitySizing, etc.)
        bool PrintFlag;   // TRUE when sizing information is reported in the eio file
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                // FractionOfAutosizedHeatingCapacity )

        PltSizHeatNum = 0;
        PltSizNum = 0;
        DesCoilLoad = 0.0;
        ErrorsFound = false;
        FlowAutoSize = false;
        CapAutoSize = false;
        WaterVolFlowRateMaxDes = 0.0;
        WaterVolFlowRateMaxUser = 0.0;
        RatedCapacityDes = 0.0;
        DataScalableCapSizingON = false;

        if (CurZoneEqNum > 0) {

            CompType = cCMO_BBRadiator_Water;
            CompName = HWBaseboard(BaseboardNum).EquipID;
            DataHeatSizeRatio = 1.0;
            DataFracOfAutosizedHeatingCapacity = 1.0;
            DataZoneNumber = HWBaseboard(BaseboardNum).ZonePtr;
            SizingMethod = HeatingCapacitySizing;
            FieldNum = 3;
            PrintFlag = false;
            SizingString = HWBaseboardNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
            CapSizingMethod = HWBaseboard(BaseboardNum).HeatingCapMethod;
            ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
            if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                if (CapSizingMethod == HeatingDesignCapacity) {
                    if (HWBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                        CheckZoneSizing(state, CompType, CompName);
                        ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                        ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    }
                    TempSize = HWBaseboard(BaseboardNum).ScaledHeatingCapacity;

                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = HWBaseboard(BaseboardNum).ScaledHeatingCapacity * Zone(DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                    DataScalableCapSizingON = true;
                } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                    CheckZoneSizing(state, CompType, CompName);
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    DataFracOfAutosizedHeatingCapacity = HWBaseboard(BaseboardNum).ScaledHeatingCapacity;
                    ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    TempSize = AutoSize;
                    DataScalableCapSizingON = true;
                } else {
                    TempSize = HWBaseboard(BaseboardNum).ScaledHeatingCapacity;
                }
                bool errorsFound = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                TempSize = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                if (HWBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                    HWBaseboard(BaseboardNum).RatedCapacity = AutoSize;
                } else {
                    HWBaseboard(BaseboardNum).RatedCapacity = TempSize;
                }
                RatedCapacityDes = TempSize;
                DataScalableCapSizingON = false;
            }
        }

        // find the appropriate heating Plant Sizing object
        PltSizHeatNum = state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).PlantSizNum;

        if (PltSizHeatNum > 0) {
            if (CurZoneEqNum > 0) {

                if (HWBaseboard(BaseboardNum).WaterVolFlowRateMax == AutoSize) {
                    FlowAutoSize = true;
                }
                if (!FlowAutoSize && !ZoneSizingRunDone) { // Simulation continue
                    if (HWBaseboard(BaseboardNum).WaterVolFlowRateMax > 0.0) {
                        BaseSizer::reportSizerOutput(state, cCMO_BBRadiator_Water,
                                                     HWBaseboard(BaseboardNum).EquipID,
                                                     "User-Specified Maximum Water Flow Rate [m3/s]",
                                                     HWBaseboard(BaseboardNum).WaterVolFlowRateMax);
                    }
                } else {
                    CheckZoneSizing(state, cCMO_BBRadiator_Water, HWBaseboard(BaseboardNum).EquipID);
                    DesCoilLoad = RatedCapacityDes;
                    if (DesCoilLoad >= SmallLoad) {
                        Cp = GetSpecificHeatGlycol(state,
                                                   state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                                   DataGlobalConstants::HWInitConvTemp,
                                                   state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                                   RoutineName);
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                               DataGlobalConstants::HWInitConvTemp,
                                               state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                               RoutineName);
                        WaterVolFlowRateMaxDes = DesCoilLoad / (PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                    } else {
                        WaterVolFlowRateMaxDes = 0.0;
                    }

                    if (FlowAutoSize) {
                        HWBaseboard(BaseboardNum).WaterVolFlowRateMax = WaterVolFlowRateMaxDes;
                        BaseSizer::reportSizerOutput(state, cCMO_BBRadiator_Water,
                                                     HWBaseboard(BaseboardNum).EquipID,
                                                     "Design Size Maximum Water Flow Rate [m3/s]",
                                                     WaterVolFlowRateMaxDes);
                    } else { // Hard-sized with sizing data
                        if (HWBaseboard(BaseboardNum).WaterVolFlowRateMax > 0.0 && WaterVolFlowRateMaxDes > 0.0) {
                            WaterVolFlowRateMaxUser = HWBaseboard(BaseboardNum).WaterVolFlowRateMax;
                            BaseSizer::reportSizerOutput(state, cCMO_BBRadiator_Water,
                                                         HWBaseboard(BaseboardNum).EquipID,
                                                         "Design Size Maximum Water Flow Rate [m3/s]",
                                                         WaterVolFlowRateMaxDes,
                                                         "User-Specified Maximum Water Flow Rate [m3/s]",
                                                         WaterVolFlowRateMaxUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(WaterVolFlowRateMaxDes - WaterVolFlowRateMaxUser) / WaterVolFlowRateMaxUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                        "SizeHWBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:RadiantConvective:Water=\"" +
                                        HWBaseboard(BaseboardNum).EquipID + "\".");
                                    ShowContinueError(state,
                                                      format("User-Specified Maximum Water Flow Rate of {:.5R} [m3/s]", WaterVolFlowRateMaxUser));
                                    ShowContinueError(
                                        state, format("differs from Design Size Maximum Water Flow Rate of {:.5R} [m3/s]", WaterVolFlowRateMaxDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
                if (HWBaseboard(BaseboardNum).WaterTempAvg > 0.0 && HWBaseboard(BaseboardNum).WaterMassFlowRateStd > 0.0 &&
                    HWBaseboard(BaseboardNum).RatedCapacity > 0.0) {
                    DesCoilLoad = HWBaseboard(BaseboardNum).RatedCapacity;
                    WaterMassFlowRateStd = HWBaseboard(BaseboardNum).WaterMassFlowRateStd;
                } else if (HWBaseboard(BaseboardNum).RatedCapacity == AutoSize || HWBaseboard(BaseboardNum).RatedCapacity == 0.0) {
                    DesCoilLoad = RatedCapacityDes;
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                           RoutineNameFull);
                    WaterMassFlowRateStd = HWBaseboard(BaseboardNum).WaterVolFlowRateMax * rho;
                }
                if (DesCoilLoad >= SmallLoad) {
                    // Calculate UA value
                    // Air mass flow rate is obtained from the following linear equation
                    // m_dot = 0.0062 + 2.75e-05*q
                    AirMassFlowRate = Constant + Coeff * DesCoilLoad;
                    Cp = GetSpecificHeatGlycol(state,
                                               state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                               HWBaseboard(BaseboardNum).WaterTempAvg,
                                               state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                               RoutineName);
                    WaterInletTempStd = (DesCoilLoad / (2.0 * WaterMassFlowRateStd * Cp)) + HWBaseboard(BaseboardNum).WaterTempAvg;
                    WaterOutletTempStd = std::abs((2.0 * HWBaseboard(BaseboardNum).WaterTempAvg) - WaterInletTempStd);
                    AirOutletTempStd = (DesCoilLoad / (AirMassFlowRate * CPAirStd)) + AirInletTempStd;
                    HWBaseboard(BaseboardNum).AirMassFlowRateStd = AirMassFlowRate;
                    // Check Ta,out < Tw,in
                    if (AirOutletTempStd >= WaterInletTempStd) {
                        ShowSevereError(state, "SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water=\"" + HWBaseboard(BaseboardNum).EquipID + "\".");
                        ShowContinueError(state, "...Air Outlet temperature must be below the Water Inlet temperature");
                        ShowContinueError(
                            state,
                            format("...Air Outlet Temperature=[{:.2R}], Water Inlet Temperature=[{:.2R}].", AirOutletTempStd, WaterInletTempStd));
                        AirOutletTempStd = WaterInletTempStd - 0.01;
                        ShowContinueError(state, format("...Air Outlet Temperature set to [{:.2R}].", AirOutletTempStd));
                    }
                    // Check Tw,out < Ta,in
                    if (AirInletTempStd >= WaterOutletTempStd) {
                        ShowSevereError(state, "SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water=\"" + HWBaseboard(BaseboardNum).EquipID + "\".");
                        ShowContinueError(state, "...Water Outlet temperature must be below the Air Inlet temperature");
                        ShowContinueError(
                            state,
                            format("...Air Inlet Temperature=[{:.2R}], Water Outlet Temperature=[{:.2R}].", AirInletTempStd, WaterOutletTempStd));
                        WaterOutletTempStd = AirInletTempStd + 0.01;
                        ShowContinueError(state, format("...Water Outlet Temperature set to [{:.2R}].", WaterOutletTempStd));
                    }
                    // LMTD calculation
                    DeltaT1 = WaterInletTempStd - AirOutletTempStd;
                    DeltaT2 = WaterOutletTempStd - AirInletTempStd;
                    LMTD = (DeltaT1 - DeltaT2) / (std::log(DeltaT1 / DeltaT2));
                    HWBaseboard(BaseboardNum).UA = DesCoilLoad / LMTD;
                } else {
                    HWBaseboard(BaseboardNum).UA = 0.0;
                }
                // Report an UA value
                BaseSizer::reportSizerOutput(state,
                    cCMO_BBRadiator_Water, HWBaseboard(BaseboardNum).EquipID, "U-Factor times Area [W/C]", HWBaseboard(BaseboardNum).UA);
            }
        } else {
            // if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
            if (HWBaseboard(BaseboardNum).WaterVolFlowRateMax == AutoSize || HWBaseboard(BaseboardNum).RatedCapacity == AutoSize ||
                HWBaseboard(BaseboardNum).RatedCapacity == 0.0) {
                ShowSevereError(state, "Autosizing of hot water baseboard requires a heating loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Hot Water Baseboard Heater=" + HWBaseboard(BaseboardNum).EquipID);
                ErrorsFound = true;
            }
            // calculate UA from rated capacities
            HWBaseboard(BaseboardNum).RatedCapacity = RatedCapacityDes;
            DesCoilLoad = RatedCapacityDes;

            if (DesCoilLoad >= SmallLoad) {
                WaterMassFlowRateStd = HWBaseboard(BaseboardNum).WaterMassFlowRateStd;
                // m_dot = 0.0062 + 2.75e-05*q
                AirMassFlowRate = Constant + Coeff * DesCoilLoad;
                Cp = GetSpecificHeatGlycol(state,
                                           state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                           HWBaseboard(BaseboardNum).WaterTempAvg,
                                           state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                           RoutineName);
                WaterInletTempStd = (DesCoilLoad / (2.0 * WaterMassFlowRateStd * Cp)) + HWBaseboard(BaseboardNum).WaterTempAvg;
                WaterOutletTempStd = std::abs((2.0 * HWBaseboard(BaseboardNum).WaterTempAvg) - WaterInletTempStd);
                AirOutletTempStd = (DesCoilLoad / (AirMassFlowRate * CPAirStd)) + AirInletTempStd;
                HWBaseboard(BaseboardNum).AirMassFlowRateStd = AirMassFlowRate;

                // Check Ta,out < Tw,in
                if (AirOutletTempStd >= WaterInletTempStd) {
                    ShowSevereError(state, "SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water=\"" + HWBaseboard(BaseboardNum).EquipID + "\".");
                    ShowContinueError(state, "...Air Outlet temperature must be below the Water Inlet temperature");
                    ShowContinueError(
                        state, format("...Air Outlet Temperature=[{:.2R}], Water Inlet Temperature=[{:.2R}].", AirOutletTempStd, WaterInletTempStd));
                    AirOutletTempStd = WaterInletTempStd - 0.01;
                    ShowContinueError(state, format("...Air Outlet Temperature set to [{:.2R}].", AirOutletTempStd));
                }
                // Check Tw,out < Ta,in
                if (AirInletTempStd >= WaterOutletTempStd) {
                    ShowSevereError(state, "SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water=\"" + HWBaseboard(BaseboardNum).EquipID + "\".");
                    ShowContinueError(state, "...Water Outlet temperature must be below the Air Inlet temperature");
                    ShowContinueError(
                        state, format("...Air Inlet Temperature=[{:.2R}], Water Outlet Temperature=[{:.2R}].", AirInletTempStd, WaterOutletTempStd));
                    WaterOutletTempStd = AirInletTempStd + 0.01;
                    ShowContinueError(state, format("...Water Outlet Temperature set to [{:.2R}].", WaterOutletTempStd));
                }
                // LMTD calculation
                DeltaT1 = WaterInletTempStd - AirOutletTempStd;
                DeltaT2 = WaterOutletTempStd - AirInletTempStd;
                LMTD = (DeltaT1 - DeltaT2) / (std::log(DeltaT1 / DeltaT2));
                HWBaseboard(BaseboardNum).UA = DesCoilLoad / LMTD;
            } else {
                HWBaseboard(BaseboardNum).UA = 0.0;
            }
            // Report an UA value
            BaseSizer::reportSizerOutput(state,
                cCMO_BBRadiator_Water, HWBaseboard(BaseboardNum).EquipID, "U-Factor times Area [W/C]", HWBaseboard(BaseboardNum).UA);
        }
        // save the design water flow rate for use by the water loop sizing algorithms
        RegisterPlantCompDesignFlow(HWBaseboard(BaseboardNum).WaterInletNode, HWBaseboard(BaseboardNum).WaterVolFlowRateMax);

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void CalcHWBaseboard(EnergyPlusData &state, int &BaseboardNum, Real64 &LoadMet)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED       May 2000 Fred Buhl
        //                      Aug 2007 Daeho Kang (Add the calculation of radiant heat source)
        //                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates both the convective and radiant heat transfer rate
        // in a hot water baseboard heater.  The heater is assumed to be crossflowwith
        // both fluids unmixed.  The air flow is bouyancy driven and a constant airflow
        // and a constant airflow velocity of 0.5m/s is assumed.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Incropera and DeWitt, Fundamentals of Heat and Mass Transfer
        // Chapter 11.4, p. 523, eq. 11.33

        // Using/Aliasing
        using namespace DataSizing;
        using DataLoopNode::Node;
        using PlantUtilities::SetActuatedBranchFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MinFrac(0.0005); // Minimum fraction that delivers radiant heats to surfaces
        static std::string const RoutineName("CalcHWBaseboard");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        Real64 RadHeat;
        Real64 BBHeat;
        Real64 AirInletTemp;
        Real64 AirOutletTemp;
        Real64 WaterInletTemp;
        Real64 WaterOutletTemp;
        Real64 WaterMassFlowRate;
        Real64 AirMassFlowRate;
        Real64 CapacitanceAir;
        Real64 CapacitanceWater;
        Real64 CapacitanceMax;
        Real64 CapacitanceMin;
        Real64 CapacityRatio;
        Real64 NTU;
        Real64 Effectiveness;
        Real64 AA;
        Real64 BB;
        Real64 CC;
        Real64 QZnReq;
        Real64 Cp;

        ZoneNum = HWBaseboard(BaseboardNum).ZonePtr;
        QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        AirInletTemp = HWBaseboard(BaseboardNum).AirInletTemp;
        AirOutletTemp = AirInletTemp;
        WaterInletTemp = HWBaseboard(BaseboardNum).WaterInletTemp;
        WaterOutletTemp = WaterInletTemp;
        WaterMassFlowRate = Node(HWBaseboard(BaseboardNum).WaterInletNode).MassFlowRate;
        HWBaseboardDesignData HWBaseboardDesignDataObject{HWBaseboardDesignObject(HWBaseboard(BaseboardNum).DesignObjectPtr)}; // Contains the data for the design object

        if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) && (GetCurrentScheduleValue(state, HWBaseboard(BaseboardNum).SchedPtr) > 0) &&
            (WaterMassFlowRate > 0.0)) {
            // Calculate air mass flow rate
            AirMassFlowRate = HWBaseboard(BaseboardNum).AirMassFlowRateStd * (WaterMassFlowRate / HWBaseboard(BaseboardNum).WaterMassFlowRateMax);
            CapacitanceAir = PsyCpAirFnW(HWBaseboard(BaseboardNum).AirInletHumRat) * AirMassFlowRate;
            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidName,
                                       WaterInletTemp,
                                       state.dataPlnt->PlantLoop(HWBaseboard(BaseboardNum).LoopNum).FluidIndex,
                                       RoutineName);

            CapacitanceWater = Cp * WaterMassFlowRate;
            CapacitanceMax = max(CapacitanceAir, CapacitanceWater);
            CapacitanceMin = min(CapacitanceAir, CapacitanceWater);
            CapacityRatio = CapacitanceMin / CapacitanceMax;
            NTU = HWBaseboard(BaseboardNum).UA / CapacitanceMin;

            // The effectiveness is given by the following formula:
            // Effectiveness = 1. - EXP((1./CapacityRatio)*(NTU)**0.22*(EXP(-CapacityRatio*(NTU)**0.78)-1.))
            // To prevent possible underflows (numbers smaller than the computer can handle) we must break
            // the calculation up into steps and check the size of the exponential arguments.
            AA = -CapacityRatio * std::pow(NTU, 0.78);
            if (AA < -20.0) {
                BB = 0.0;
            } else {
                BB = std::exp(AA);
            }
            CC = (1.0 / CapacityRatio) * std::pow(NTU, 0.22) * (BB - 1.0);
            if (CC < -20.0) {
                Effectiveness = 1.0;
            } else {
                Effectiveness = 1.0 - std::exp(CC);
            }

            AirOutletTemp = AirInletTemp + Effectiveness * CapacitanceMin * (WaterInletTemp - AirInletTemp) / CapacitanceAir;
            WaterOutletTemp = WaterInletTemp - CapacitanceAir * (AirOutletTemp - AirInletTemp) / CapacitanceWater;
            BBHeat = CapacitanceWater * (WaterInletTemp - WaterOutletTemp);
            RadHeat = BBHeat * HWBaseboardDesignDataObject.FracRadiant;
            QBBRadSource(BaseboardNum) = RadHeat;

            if (HWBaseboardDesignDataObject.FracRadiant <= MinFrac) {
                LoadMet = BBHeat;
            } else {

                // Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
                DistributeBBRadGains(state);
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
                LoadMet = (SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)) + (BBHeat * HWBaseboard(BaseboardNum).FracConvect) +
                          (RadHeat * HWBaseboardDesignDataObject.FracDistribPerson);
            }
            HWBaseboard(BaseboardNum).WaterOutletEnthalpy = HWBaseboard(BaseboardNum).WaterInletEnthalpy - BBHeat / WaterMassFlowRate;
        } else {
            CapacitanceWater = 0.0;
            CapacitanceMax = 0.0;
            CapacitanceMin = 0.0;
            NTU = 0.0;
            Effectiveness = 0.0;
            AirOutletTemp = AirInletTemp;
            WaterOutletTemp = WaterInletTemp;
            BBHeat = 0.0;
            LoadMet = 0.0;
            RadHeat = 0.0;
            WaterMassFlowRate = 0.0;
            AirMassFlowRate = 0.0;
            QBBRadSource(BaseboardNum) = 0.0;
            HWBaseboard(BaseboardNum).WaterOutletEnthalpy = HWBaseboard(BaseboardNum).WaterInletEnthalpy;
            SetActuatedBranchFlowRate(state, WaterMassFlowRate,
                                      HWBaseboard(BaseboardNum).WaterInletNode,
                                      HWBaseboard(BaseboardNum).LoopNum,
                                      HWBaseboard(BaseboardNum).LoopSideNum,
                                      HWBaseboard(BaseboardNum).BranchNum,
                                      false);
        }

        HWBaseboard(BaseboardNum).WaterOutletTemp = WaterOutletTemp;
        HWBaseboard(BaseboardNum).AirOutletTemp = AirOutletTemp;
        HWBaseboard(BaseboardNum).WaterMassFlowRate = WaterMassFlowRate;
        HWBaseboard(BaseboardNum).AirMassFlowRate = AirMassFlowRate;
        HWBaseboard(BaseboardNum).TotPower = LoadMet;
        HWBaseboard(BaseboardNum).Power = BBHeat;
        HWBaseboard(BaseboardNum).ConvPower = BBHeat - RadHeat;
        HWBaseboard(BaseboardNum).RadPower = RadHeat;
    }

    void UpdateHWBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //                      Rick Strand
        //       DATE WRITTEN   Nov 1997
        //                      February 2001
        //       MODIFIED       Aug 2007 Daeho Kang (Add the update of radiant source)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:

        // METHODOLOGY EMPLOYED:
        // The update subrotines both in high temperature radiant radiator
        // and convective only baseboard radiator are combined and modified.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::Node;
        using PlantUtilities::SafeCopyPlantNode;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WaterInletNode;
        int WaterOutletNode;
        static int Iter(0);
        static bool MyEnvrnFlag(true);

        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag) {
            Iter = 0;
            MyEnvrnFlag = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag = true;
        }

        // First, update the running average if necessary...
        if (LastSysTimeElapsed(BaseboardNum) == SysTimeElapsed) {
            QBBRadSrcAvg(BaseboardNum) -= LastQBBRadSrc(BaseboardNum) * LastTimeStepSys(BaseboardNum) / state.dataGlobal->TimeStepZone;
        }
        // Update the running average and the "last" values with the current values of the appropriate variables
        QBBRadSrcAvg(BaseboardNum) += QBBRadSource(BaseboardNum) * TimeStepSys / state.dataGlobal->TimeStepZone;

        LastQBBRadSrc(BaseboardNum) = QBBRadSource(BaseboardNum);
        LastSysTimeElapsed(BaseboardNum) = SysTimeElapsed;
        LastTimeStepSys(BaseboardNum) = TimeStepSys;

        WaterInletNode = HWBaseboard(BaseboardNum).WaterInletNode;
        WaterOutletNode = HWBaseboard(BaseboardNum).WaterOutletNode;

        // Set the outlet air nodes of the Baseboard
        // Set the outlet water nodes for the Coil
        SafeCopyPlantNode(state, WaterInletNode, WaterOutletNode);
        Node(WaterOutletNode).Temp = HWBaseboard(BaseboardNum).WaterOutletTemp;
        Node(WaterOutletNode).Enthalpy = HWBaseboard(BaseboardNum).WaterOutletEnthalpy;
    }

    void UpdateBBRadSourceValAvg(EnergyPlusData &state, bool &HWBaseboardSysOn) // .TRUE. if the radiant system has run this zone time step
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       Aug 2007 Daeho Kang (Modification only for baseboard)
        //       RE-ENGINEERED  na

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
        int BaseboardNum; // DO loop counter for surface index


        HWBaseboardSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(QBBRadSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (BaseboardNum = 1; BaseboardNum <= NumHWBaseboards; ++BaseboardNum) {
            if (QBBRadSrcAvg(BaseboardNum) != 0.0) {
                HWBaseboardSysOn = true;
                break; // DO loop
            }
        }

        QBBRadSource = QBBRadSrcAvg;

        DistributeBBRadGains(state); // QBBRadSource has been modified so we need to redistribute gains
    }

    void DistributeBBRadGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       Aug. 2007 Daeho Kang (Modification only for baseboard)
        //                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
        //       RE-ENGINEERED  na

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
        // na

        // Using/Aliasing
        using DataHeatBalance::Zone;
        using DataHeatBalFanSys::MaxRadHeatFlux;
        using DataHeatBalFanSys::QHWBaseboardSurf;
        using DataHeatBalFanSys::QHWBaseboardToPerson;
        using DataSurfaces::Surface;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SmallestArea(0.001); // Smallest area in meters squared (to avoid a divide by zero)

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RadSurfNum;           // Counter for surfaces receiving radiation from radiant heater
        int BaseboardNum;         // Counter for the baseboard
        int SurfNum;              // Pointer to the Surface derived type
        int ZoneNum;              // Pointer to the Zone derived type
        Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface


        // Initialize arrays
        QHWBaseboardSurf = 0.0;
        QHWBaseboardToPerson = 0.0;

        for (BaseboardNum = 1; BaseboardNum <= NumHWBaseboards; ++BaseboardNum) {


            HWBaseboardDesignData HWBaseboardDesignDataObject{HWBaseboardDesignObject(HWBaseboard(BaseboardNum).DesignObjectPtr)}; // Contains the data for the design object
            ZoneNum = HWBaseboard(BaseboardNum).ZonePtr;
            if (ZoneNum <= 0) continue;
            QHWBaseboardToPerson(ZoneNum) += QBBRadSource(BaseboardNum) * HWBaseboardDesignDataObject.FracDistribPerson;

            for (RadSurfNum = 1; RadSurfNum <= HWBaseboard(BaseboardNum).TotSurfToDistrib; ++RadSurfNum) {
                SurfNum = HWBaseboard(BaseboardNum).SurfacePtr(RadSurfNum);
                if (Surface(SurfNum).Area > SmallestArea) {
                    ThisSurfIntensity =
                        (QBBRadSource(BaseboardNum) * HWBaseboard(BaseboardNum).FracDistribToSurf(RadSurfNum) / Surface(SurfNum).Area);
                    QHWBaseboardSurf(SurfNum) += ThisSurfIntensity;
                    // CR 8074, trap for excessive intensity (throws off surface balance )
                    if (ThisSurfIntensity > MaxRadHeatFlux) {
                        ShowSevereError(state, "DistributeBBRadGains:  excessive thermal radiation heat flux intensity detected");
                        ShowContinueError(state, "Surface = " + Surface(SurfNum).Name);
                        ShowContinueError(state, format("Surface area = {:.3R} [m2]", Surface(SurfNum).Area));
                        ShowContinueError(state, "Occurs in " + cCMO_BBRadiator_Water + " = " + HWBaseboard(BaseboardNum).EquipID);
                        ShowContinueError(state, format("Radiation intensity = {:.2R} [W/m2]", ThisSurfIntensity));
                        ShowContinueError(state, "Assign a larger surface area or more surfaces in " + cCMO_BBRadiator_Water);
                        ShowFatalError(state, "DistributeBBRadGains:  excessive thermal radiation heat flux intensity detected");
                    }
                } else {
                    ShowSevereError(state, "DistributeBBRadGains:  surface not large enough to receive thermal radiation heat flux");
                    ShowContinueError(state, "Surface = " + Surface(SurfNum).Name);
                    ShowContinueError(state, format("Surface area = {:.3R} [m2]", Surface(SurfNum).Area));
                    ShowContinueError(state, "Occurs in " + cCMO_BBRadiator_Water + " = " + HWBaseboard(BaseboardNum).EquipID);
                    ShowContinueError(state, "Assign a larger surface area or more surfaces in " + cCMO_BBRadiator_Water);
                    ShowFatalError(state, "DistributeBBRadGains:  surface not large enough to receive thermal radiation heat flux");
                }
            }
        }
    }

    void ReportHWBaseboard(int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Aug 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This subroutine

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::Node;
        using DataSurfaces::Surface;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        HWBaseboard(BaseboardNum).TotEnergy = HWBaseboard(BaseboardNum).TotPower * TimeStepSys * DataGlobalConstants::SecInHour;
        HWBaseboard(BaseboardNum).Energy = HWBaseboard(BaseboardNum).Power * TimeStepSys * DataGlobalConstants::SecInHour;
        HWBaseboard(BaseboardNum).ConvEnergy = HWBaseboard(BaseboardNum).ConvPower * TimeStepSys * DataGlobalConstants::SecInHour;
        HWBaseboard(BaseboardNum).RadEnergy = HWBaseboard(BaseboardNum).RadPower * TimeStepSys * DataGlobalConstants::SecInHour;
    }

    Real64 SumHATsurf(int const ZoneNum) // Zone number
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   July 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
        // and should be updated accordingly.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;

        // Return value
        Real64 SumHATsurf;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // Surface number
        Real64 Area; // Effective surface area


        SumHATsurf = 0.0;

        for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
            if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            Area = Surface(SurfNum).Area;

            if (Surface(SurfNum).Class == SurfaceClass::Window) {
                if (SurfWinShadingFlag(SurfNum) == IntShadeOn || SurfWinShadingFlag(SurfNum) == IntBlindOn) {
                    // The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
                    Area += SurfWinDividerArea(SurfNum);
                }

                if (SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    SumHATsurf += HConvIn(SurfNum) * SurfWinFrameArea(SurfNum) * (1.0 + SurfWinProjCorrFrIn(SurfNum)) *
                                  SurfWinFrameTempSurfIn(SurfNum);
                }

                if (SurfWinDividerArea(SurfNum) > 0.0 && SurfWinShadingFlag(SurfNum) != IntShadeOn &&
                    SurfWinShadingFlag(SurfNum) != IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += HConvIn(SurfNum) * SurfWinDividerArea(SurfNum) * (1.0 + 2.0 * SurfWinProjCorrDivIn(SurfNum)) *
                                  SurfWinDividerTempSurfIn(SurfNum);
                }
            }

            SumHATsurf += HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum);
        }

        return SumHATsurf;
    }

    void UpdateHWBaseboardPlantConnection(EnergyPlusData &state,
                                          int const BaseboardTypeNum,               // type index
                                          std::string const &BaseboardName,         // component name
                                          [[maybe_unused]] int const EquipFlowCtrl, // Flow control mode for the equipment
                                          [[maybe_unused]] int const LoopNum,       // Plant loop index for where called from
                                          [[maybe_unused]] int const LoopSide,      // Plant loop side index for where called from
                                          int &CompIndex,                           // Chiller number pointer
                                          [[maybe_unused]] bool const FirstHVACIteration,
                                          bool &InitLoopEquip // If not zero, calculate the max load for operating conditions
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Sept. 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update sim routine called from plant

        // METHODOLOGY EMPLOYED:
        // check input, provide comp index, call utility routines

        // Using/Aliasing
        using DataPlant::ccSimPlantEquipTypes;
        using DataPlant::TypeOf_Baseboard_Rad_Conv_Water;

        using PlantUtilities::PullCompInterconnectTrigger;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int BaseboardNum;

        // Find the correct baseboard
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(BaseboardName, HWBaseboard, &HWBaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError(state, "UpdateHWBaseboardPlantConnection: Specified baseboard not valid =" + BaseboardName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > NumHWBaseboards || BaseboardNum < 1) {
                ShowFatalError(
                    state,
                    format("UpdateHWBaseboardPlantConnection:  Invalid CompIndex passed={}, Number of baseboards={}, Entered baseboard name={}",
                           BaseboardNum,
                           NumHWBaseboards,
                           BaseboardName));
            }
            if (state.dataGlobal->KickOffSimulation) {
                if (BaseboardName != HWBaseboard(BaseboardNum).EquipID) {
                    ShowFatalError(state,
                                   format("UpdateHWBaseboardPlantConnection: Invalid CompIndex passed={}, baseboard name={}, stored baseboard Name "
                                          "for that index={}",
                                          BaseboardNum,
                                          BaseboardName,
                                          HWBaseboard(BaseboardNum).EquipID));
                }
                if (BaseboardTypeNum != TypeOf_Baseboard_Rad_Conv_Water) {
                    ShowFatalError(state,
                                   format("UpdateHWBaseboardPlantConnection: Invalid CompIndex passed={}, baseboard name={}, stored baseboard Name "
                                          "for that index={}",
                                          BaseboardNum,
                                          BaseboardName,
                                          ccSimPlantEquipTypes(BaseboardTypeNum)));
                }
            }
        }

        if (InitLoopEquip) {
            return;
        }

        PullCompInterconnectTrigger(state, HWBaseboard(BaseboardNum).LoopNum,
                                    HWBaseboard(BaseboardNum).LoopSideNum,
                                    HWBaseboard(BaseboardNum).BranchNum,
                                    HWBaseboard(BaseboardNum).CompNum,
                                    HWBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    HWBaseboard(BaseboardNum).LoopNum,
                                    HWBaseboard(BaseboardNum).LoopSideNum,
                                    DataPlant::iCriteriaType::HeatTransferRate,
                                    HWBaseboard(BaseboardNum).Power);

        PullCompInterconnectTrigger(state, HWBaseboard(BaseboardNum).LoopNum,
                                    HWBaseboard(BaseboardNum).LoopSideNum,
                                    HWBaseboard(BaseboardNum).BranchNum,
                                    HWBaseboard(BaseboardNum).CompNum,
                                    HWBaseboard(BaseboardNum).BBMassFlowReSimIndex,
                                    HWBaseboard(BaseboardNum).LoopNum,
                                    HWBaseboard(BaseboardNum).LoopSideNum,
                                    DataPlant::iCriteriaType::MassFlowRate,
                                    HWBaseboard(BaseboardNum).WaterMassFlowRate);

        PullCompInterconnectTrigger(state, HWBaseboard(BaseboardNum).LoopNum,
                                    HWBaseboard(BaseboardNum).LoopSideNum,
                                    HWBaseboard(BaseboardNum).BranchNum,
                                    HWBaseboard(BaseboardNum).CompNum,
                                    HWBaseboard(BaseboardNum).BBInletTempFlowReSimIndex,
                                    HWBaseboard(BaseboardNum).LoopNum,
                                    HWBaseboard(BaseboardNum).LoopSideNum,
                                    DataPlant::iCriteriaType::Temperature,
                                    HWBaseboard(BaseboardNum).WaterOutletTemp);
    }

    //*****************************************************************************************

} // namespace HWBaseboardRadiator

} // namespace EnergyPlus
