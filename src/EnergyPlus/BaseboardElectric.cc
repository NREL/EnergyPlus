// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <EnergyPlus/BaseboardElectric.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace BaseboardElectric {
    // Module containing the routines dealing with the BASEBOARD Electric HEATER
    // component(s).

    // MODULE INFORMATION:  Richard Liesen
    //       DATE WRITTEN   Nov 2001
    //       RE-ENGINEERED  na

    using namespace DataGlobals;

    // Use statements for access to subroutines in other modules
    using namespace ScheduleManager;

    // MODULE PARAMETER DEFINITIONS
    const char * cCMO_BBRadiator_Electric = "ZoneHVAC:Baseboard:Convective:Electric";
    constexpr Real64 SimpConvAirFlowSpeed(0.5); // m/s

    void SimElectricBaseboard(EnergyPlusData &state, std::string const &EquipName, int const ActualZoneNum, int const ControlledZoneNum, Real64 &PowerMet, int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the Electric Baseboard units.

        using DataZoneEnergyDemands::ZoneSysEnergyDemand;
        using General::TrimSigDigits;

        int BaseboardNum;               // index of unit in baseboard array
        Real64 QZnReq;                  // zone load not yet satisfied

        if (state.dataBaseboardElectric->getInputFlag) {
            GetBaseboardInput(state);
            state.dataBaseboardElectric->getInputFlag = false;
        }

        auto &baseboard = state.dataBaseboardElectric;

        // Find the correct Baseboard Equipment
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(EquipName, baseboard->Baseboard, &BaseboardParams::EquipName);
            if (BaseboardNum == 0) {
                ShowFatalError("SimElectricBaseboard: Unit not found=" + EquipName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > baseboard->NumBaseboards || BaseboardNum < 1) {
                ShowFatalError("SimElectricBaseboard:  Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) +
                               ", Number of Units=" + TrimSigDigits(baseboard->NumBaseboards) + ", Entered Unit name=" + EquipName);
            }
            if (baseboard->Baseboard(BaseboardNum).CheckEquipName) {
                if (EquipName != baseboard->Baseboard(BaseboardNum).EquipName) {
                    ShowFatalError("SimElectricBaseboard: Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) + ", Unit name=" + EquipName +
                                   ", stored Unit Name for that index=" + baseboard->Baseboard(BaseboardNum).EquipName);
                }
                baseboard->Baseboard(BaseboardNum).CheckEquipName = false;
            }
        }

        InitBaseboard(state, BaseboardNum, ControlledZoneNum);

        QZnReq = ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToHeatSP;

        // Simulate baseboard
        SimElectricConvective(state, BaseboardNum, QZnReq);

        PowerMet = baseboard->Baseboard(BaseboardNum).Power;

        baseboard->Baseboard(BaseboardNum).Energy = baseboard->Baseboard(BaseboardNum).Power * DataHVACGlobals::TimeStepSys * SecInHour;
        baseboard->Baseboard(BaseboardNum).ElecUseLoad = baseboard->Baseboard(BaseboardNum).ElecUseRate * DataHVACGlobals::TimeStepSys * SecInHour;
    }

    void GetBaseboardInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the Baseboard units.

        // METHODOLOGY EMPLOYED:
        // Standard input processor calls.

        // Using/Aliasing
        using GlobalNames::VerifyUniqueBaseboardName;
        using namespace DataIPShortCuts;
        using DataGlobals::NumOfZones;
        using DataSizing::AutoSize;
        using DataSizing::CapacityPerFloorArea;
        using DataSizing::FractionOfAutosizedHeatingCapacity;
        using DataSizing::HeatingDesignCapacity;
        using DataZoneEquipment::BBElectricConvective_Num;
        using DataZoneEquipment::ZoneEquipList;
        using General::TrimSigDigits;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetBaseboardInput: "); // include trailing blank space
        int const iHeatCAPMAlphaNum(3);                              // get input index to baseboard heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1);                  // get input index to baseboard heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(2);            // get input index to baseboard heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            3); //  get input index to baseboard heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum;
        int NumConvElecBaseboards;
        int ConvElecBBNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input

        int CtrlZone;         // index to constrolled zone number
        int ZoneEquipTypeNum; // index to zone equipment in a zone equipment list

        auto & baseboard = state.dataBaseboardElectric;
        
        cCurrentModuleObject = cCMO_BBRadiator_Electric;

        NumConvElecBaseboards = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // Calculate total number of baseboard units
        baseboard->NumBaseboards = NumConvElecBaseboards;

        baseboard->Baseboard.allocate(baseboard->NumBaseboards);
        baseboard->BaseboardNumericFields.allocate(baseboard->NumBaseboards);

        if (NumConvElecBaseboards > 0) { // Get the data for cooling schemes
            BaseboardNum = 0;
            for (ConvElecBBNum = 1; ConvElecBBNum <= NumConvElecBaseboards; ++ConvElecBBNum) {

                inputProcessor->getObjectItem(cCurrentModuleObject,
                                              ConvElecBBNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                baseboard->BaseboardNumericFields(ConvElecBBNum).FieldNames.allocate(NumNums);
                baseboard->BaseboardNumericFields(ConvElecBBNum).FieldNames = "";
                baseboard->BaseboardNumericFields(ConvElecBBNum).FieldNames = cNumericFieldNames;

                if (UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) {
                    continue;
                }

                // ErrorsFound will be set to True if problem was found, left untouched otherwise
                VerifyUniqueBaseboardName(cCurrentModuleObject, cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

                ++BaseboardNum;
                auto & thisBaseboard = baseboard->Baseboard(BaseboardNum);
                thisBaseboard.EquipName = cAlphaArgs(1);                                        // name of this baseboard
                thisBaseboard.EquipType = UtilityRoutines::MakeUPPERCase(cCurrentModuleObject); // the type of baseboard-rename change
                thisBaseboard.Schedule = cAlphaArgs(2);
                if (lAlphaFieldBlanks(2)) {
                    thisBaseboard.SchedPtr = ScheduleAlwaysOn;
                } else {
                    thisBaseboard.SchedPtr = GetScheduleIndex(cAlphaArgs(2));
                    if (thisBaseboard.SchedPtr == 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered =" + cAlphaArgs(2) +
                                        " for " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }
                // get inlet node number
                thisBaseboard.BaseboardEfficiency = rNumericArgs(4);

                // Determine baseboard electric heating design capacity sizing method
                if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                    thisBaseboard.HeatingCapMethod = HeatingDesignCapacity;
                    if (!lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                        thisBaseboard.ScaledHeatingCapacity = rNumericArgs(iHeatDesignCapacityNumericNum);
                        if (thisBaseboard.ScaledHeatingCapacity < 0.0 && thisBaseboard.ScaledHeatingCapacity != AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                            ShowContinueError("Illegal " + cNumericFieldNames(iHeatDesignCapacityNumericNum) + " = " +
                                              TrimSigDigits(rNumericArgs(iHeatDesignCapacityNumericNum), 7));
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                        ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatDesignCapacityNumericNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                    thisBaseboard.HeatingCapMethod = CapacityPerFloorArea;
                    if (!lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                        thisBaseboard.ScaledHeatingCapacity = rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                        if (thisBaseboard.ScaledHeatingCapacity <= 0.0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                            ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                            ShowContinueError("Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = " +
                                              TrimSigDigits(rNumericArgs(iHeatCapacityPerFloorAreaNumericNum), 7));
                            ErrorsFound = true;
                        } else if (thisBaseboard.ScaledHeatingCapacity == AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                            ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                            ShowContinueError("Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                        ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                    thisBaseboard.HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                    if (!lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                        thisBaseboard.ScaledHeatingCapacity = rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                        if (thisBaseboard.ScaledHeatingCapacity < 0.0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                            ShowContinueError("Illegal " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum) + " = " +
                                              TrimSigDigits(rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum), 7));
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                        ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisBaseboard.EquipName);
                    ShowContinueError("Illegal " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ErrorsFound = true;
                }

                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= ZoneEquipList(CtrlZone).NumOfEquipTypes; ++ZoneEquipTypeNum) {
                        if (ZoneEquipList(CtrlZone).EquipType_Num(ZoneEquipTypeNum) == BBElectricConvective_Num &&
                            ZoneEquipList(CtrlZone).EquipName(ZoneEquipTypeNum) == thisBaseboard.EquipName) {
                            thisBaseboard.ZonePtr = CtrlZone;
                        }
                    }
                }
            }

            if (ErrorsFound) {
                ShowFatalError(RoutineName + "Errors found in getting input.  Preceding condition(s) cause termination.");
            }
        }

        for (BaseboardNum = 1; BaseboardNum <= baseboard->NumBaseboards; ++BaseboardNum) {

            // Setup Report variables for the Electric Baseboards
            // CurrentModuleObject='ZoneHVAC:Baseboard:Convective:Electric'

            auto &thisBaseboard = baseboard->Baseboard(BaseboardNum);
            SetupOutputVariable("Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                thisBaseboard.Energy,
                                "System",
                                "Sum",
                                thisBaseboard.EquipName,
                                _,
                                "ENERGYTRANSFER",
                                "BASEBOARD",
                                _,
                                "System");

            SetupOutputVariable("Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                thisBaseboard.Power,
                                "System",
                                "Average",
                                thisBaseboard.EquipName);

            SetupOutputVariable("Baseboard Electricity Energy",
                                OutputProcessor::Unit::J,
                                thisBaseboard.ElecUseLoad,
                                "System",
                                "Sum",
                                thisBaseboard.EquipName,
                                _,
                                "Electricity",
                                "HEATING",
                                _,
                                "System");

            SetupOutputVariable("Baseboard Electricity Rate",
                                OutputProcessor::Unit::W,
                                thisBaseboard.ElecUseRate,
                                "System",
                                "Average",
                                thisBaseboard.EquipName);
        }
    }

    void InitBaseboard(EnergyPlusData &state, int const BaseboardNum, int const ControlledZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the Baseboard units during simulation.

        using DataLoopNode::Node;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::ZoneEquipConfig;
        using DataZoneEquipment::ZoneEquipInputsFilled;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNode;
        int Loop;
        static Array1D_bool MyEnvrnFlag;
        
        auto & baseboard = state.dataBaseboardElectric;

        // Do the one time initializations
        if (baseboard->MyOneTimeFlag) {
            // initialize the environment and sizing flags
            MyEnvrnFlag.allocate(baseboard->NumBaseboards);
            MyEnvrnFlag = true;

            baseboard->MyOneTimeFlag = false;
        }

        // need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
        if (!baseboard->ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            baseboard->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= baseboard->NumBaseboards; ++Loop) {
                if (CheckZoneEquipmentList(baseboard->Baseboard(Loop).EquipType, baseboard->Baseboard(Loop).EquipName)) continue;
                ShowSevereError("InitBaseboard: Unit=[" + baseboard->Baseboard(Loop).EquipType + ',' + baseboard->Baseboard(Loop).EquipName +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!SysSizingCalc && baseboard->Baseboard(BaseboardNum).MySizeFlag) {
            // for each coil, do the sizing once.
            SizeElectricBaseboard(state, BaseboardNum);

            baseboard->Baseboard(BaseboardNum).MySizeFlag = false;
        }

        // Set the reporting variables to zero at each timestep.
        baseboard->Baseboard(BaseboardNum).Energy = 0.0;
        baseboard->Baseboard(BaseboardNum).Power = 0.0;
        baseboard->Baseboard(BaseboardNum).ElecUseLoad = 0.0;
        baseboard->Baseboard(BaseboardNum).ElecUseRate = 0.0;

        // Do the every time step initializations
        ZoneNode = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
        baseboard->Baseboard(BaseboardNum).AirInletTemp = Node(ZoneNode).Temp;
        baseboard->Baseboard(BaseboardNum).AirInletHumRat = Node(ZoneNode).HumRat;
    }

    void SizeElectricBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing electric baseboard components for which nominal capacities have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
        // calculated by numerically inverting the baseboard calculation routine.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::HeatingCapacitySizing;
        using General::RoundSigDigits;
        using ReportSizingManager::ReportSizingOutput;
        using ReportSizingManager::RequestSizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeElectricBaseboard");

        auto & baseboard = state.dataBaseboardElectric;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;          // autosized value of coil input field
        int FieldNum;             // IDD numeric field number where input field description is found
        int SizingMethod;         // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                                  // HeatingCapacitySizing, etc.)
        bool PrintFlag;           // TRUE when sizing information is reported in the eio file
        int CapSizingMethod;      // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                  // FractionOfAutosizedHeatingCapacity )

        DataScalableCapSizingON = false;

        if (CurZoneEqNum > 0) {

            CompType = baseboard->Baseboard(BaseboardNum).EquipType;
            CompName = baseboard->Baseboard(BaseboardNum).EquipName;
            DataFracOfAutosizedHeatingCapacity = 1.0;
            DataZoneNumber = baseboard->Baseboard(BaseboardNum).ZonePtr;
            SizingMethod = HeatingCapacitySizing;
            FieldNum = 1;
            PrintFlag = true;
            SizingString = baseboard->BaseboardNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
            CapSizingMethod = baseboard->Baseboard(BaseboardNum).HeatingCapMethod;
            ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
            if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                if (CapSizingMethod == HeatingDesignCapacity) {
                    if (baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                        CheckZoneSizing(CompType, CompName);
                        ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                        ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    }
                    TempSize = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity * Zone(DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                    DataScalableCapSizingON = true;
                } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                    CheckZoneSizing(CompType, CompName);
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    DataFracOfAutosizedHeatingCapacity = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                    ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    TempSize = AutoSize;
                    DataScalableCapSizingON = true;
                } else {
                    TempSize = baseboard->Baseboard(BaseboardNum).ScaledHeatingCapacity;
                }
                RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                baseboard->Baseboard(BaseboardNum).NominalCapacity = TempSize;
                DataScalableCapSizingON = false;
            }
        }
    }

    void SimElectricConvective(EnergyPlusData &state, int const BaseboardNum, Real64 const LoadMet)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This subroutine calculates the heat exchange rate
        // in a pure Electricconvective baseboard heater.

        // METHODOLOGY EMPLOYED:
        // Currently this is primarily modified from HW Convective baseboard which has connections to
        //  a water loop and was necessary to calculate temps, flow rates and other things.  This
        //  model might be made more sophisticated and might use some of those data structures in the future
        //  so they are left in place even though this model does not utilize them.

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;
        using DataLoopNode::Node;
        using Psychrometrics::PsyCpAirFnW;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirInletTemp;
        Real64 CpAir;
        Real64 AirMassFlowRate;
        Real64 CapacitanceAir;
        Real64 Effic;
        Real64 AirOutletTemp;
        Real64 QBBCap;

        auto & baseboard = state.dataBaseboardElectric;

        AirInletTemp = baseboard->Baseboard(BaseboardNum).AirInletTemp;
        CpAir = PsyCpAirFnW(baseboard->Baseboard(BaseboardNum).AirInletHumRat);
        AirMassFlowRate = SimpConvAirFlowSpeed;
        CapacitanceAir = CpAir * AirMassFlowRate;
        // currently only the efficiency is used to calculate the electric consumption.  There could be some
        //  thermal loss that could be accounted for with this efficiency input.
        Effic = baseboard->Baseboard(BaseboardNum).BaseboardEfficiency;

        if (GetCurrentScheduleValue(baseboard->Baseboard(BaseboardNum).SchedPtr) > 0.0 && LoadMet >= SmallLoad) {

            // if the load exceeds the capacity than the capacity is set to the BB limit.
            if (LoadMet > baseboard->Baseboard(BaseboardNum).NominalCapacity) {
                QBBCap = baseboard->Baseboard(BaseboardNum).NominalCapacity;
            } else {
                QBBCap = LoadMet;
            }

            // this could be utilized somehow or even reported so the data structures are left in place
            AirOutletTemp = AirInletTemp + QBBCap / CapacitanceAir;

            // The Baseboard electric Load is calculated using the efficiency
            baseboard->Baseboard(BaseboardNum).ElecUseRate = QBBCap / Effic;

        } else {
            // if there is an off condition the BB does nothing.
            AirOutletTemp = AirInletTemp;
            QBBCap = 0.0;
            baseboard->Baseboard(BaseboardNum).ElecUseRate = 0.0;
        }

        baseboard->Baseboard(BaseboardNum).AirOutletTemp = AirOutletTemp;
        baseboard->Baseboard(BaseboardNum).Power = QBBCap;
    }

} // namespace BaseboardElectric

} // namespace EnergyPlus
