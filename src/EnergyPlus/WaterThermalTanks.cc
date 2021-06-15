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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/floops.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/IntegratedHeatPump.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarCollectors.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

namespace EnergyPlus::WaterThermalTanks {

// MODULE INFORMATION:
//       AUTHOR         Brandon Anderson
//       DATE WRITTEN   May 2000
//       MODIFIED       Feb 2005, PGE; July 2005, FSEC - added HPWH's and desuperheater water heating coils
//                      Jan 2007, PGE - added stratified water heater
//                      Oct 2007, BTG - extended for indirect water heater
//                      May 2008, Stovall - added desup from condenser and removed double counting
//                           (includes "d0"s from revision 145)
//                       Nov 2011, BAN; corrected use and source outlet temp. calculation of stratified tank
//       RE-ENGINEERED  Feb 2004, PGE
//                      Sep 2008, BTG - refactored, was PlantWaterHeater.cc is now PlantWaterThermalTank.cc
//                                 reuse water heater code for chilled water storage

// PURPOSE OF THIS MODULE:
// This module simulates water thermal storage tanks heaters in the plant loop.  Tanks can
// be positioned as supply side equipment or demand side equipment.  Water heater versions can be stand-alone as
// non-zone equipment.

// METHODOLOGY EMPLOYED:
// Two water thermal tank models are implemented, MIXED and STRATIFIED with hot and cold versions of each:
// WaterHeater:Mixed simulates a well-mixed, single-node tank for hot water applications.  Source (e.g. heat recovery) and
// use plant connections are allowed.  A scheduled domestic hot water demand can also be specified
// to directly utilize the hot water without use side connections.
// WaterHeater:Stratified simulates a stratified, multi-node tank for hot water applicatons.
// The model shares most of the same capabilities as WaterHeater:Mixed
// but also has up to two heating elements which can be operated in
// a master-slave mode or simultaneous mode.

// ThermalStorage:ChilledWater:Mixed simulates a well-mixed, single-node tank for chilled water applications

// ThermalStorage:ChilledWater:Stratified simulates a stratified, multi-node tank for chilled water applications.

std::string const cMixedWHModuleObj = "WaterHeater:Mixed";
std::string const cStratifiedWHModuleObj = "WaterHeater:Stratified";
std::string const cMixedCWTankModuleObj = "ThermalStorage:ChilledWater:Mixed";
std::string const cStratifiedCWTankModuleObj = "ThermalStorage:ChilledWater:Stratified";
std::string const cHPWHPumpedCondenser = "WaterHeater:HeatPump:PumpedCondenser";
std::string const cHPWHWrappedCondenser = "WaterHeater:HeatPump:WrappedCondenser";
std::string const cCoilDesuperheater = "Coil:WaterHeating:Desuperheater";
std::string const fluidNameWater = "WATER";

PlantComponent *WaterThermalTankData::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data
    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    // Now look for this object in the list
    for (auto &tank : state.dataWaterThermalTanks->WaterThermalTank) {
        if (tank.Name == objectName) {
            return &tank;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "LocalWaterTankFactory: Error getting inputs for tank named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void WaterThermalTankData::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
{
    this->initialize(state, true);
    this->MinePlantStructForInfo(state);
    if (calledFromLocation.loopNum > 0) {
        if ((this->SrcSide.loopNum == calledFromLocation.loopNum) || (this->UseSide.loopNum == calledFromLocation.loopNum)) {
            this->SizeTankForDemandSide(state);
            this->SizeDemandSidePlantConnections(state);
            this->SizeSupplySidePlantConnections(state, calledFromLocation.loopNum);
            this->SizeTankForSupplySide(state);
        } else {
            return;
        }
    } else {
        this->SizeTankForDemandSide(state);
        this->SizeDemandSidePlantConnections(state);
        this->SizeSupplySidePlantConnections(state);
        this->SizeTankForSupplySide(state);
    }

    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        if (!this->IsChilledWaterTank) {
            this->CalcStandardRatings(state);
        } else {
            this->ReportCWTankInits(state);
        }
    }
}

void WaterThermalTankData::getDesignCapacities([[maybe_unused]] EnergyPlusData &state,
                                               [[maybe_unused]] const PlantLocation &calledFromLocation,
                                               Real64 &MaxLoad,
                                               Real64 &MinLoad,
                                               Real64 &OptLoad)
{
    MinLoad = 0.0;
    MaxLoad = this->MaxCapacity;
    OptLoad = this->MaxCapacity;
}

int getTankIDX(EnergyPlusData &state, std::string_view CompName, int &CompIndex)
{
    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    int CompNum;

    if (CompIndex == 0) {
        CompNum = UtilityRoutines::FindItem(CompName, state.dataWaterThermalTanks->WaterThermalTank);
        if (CompNum == 0) {
            ShowFatalError(state, "SimWaterThermalTank_WaterTank:  Unit not found=" + std::string{CompName});
        }
        CompIndex = CompNum;
    } else {
        CompNum = CompIndex;
        if (CompNum > state.dataWaterThermalTanks->numWaterThermalTank || CompNum < 1) {
            ShowFatalError(state,
                           format("SimWaterThermalTank_WaterTank:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  CompNum,
                                  state.dataWaterThermalTanks->numWaterThermalTank,
                                  CompName));
        }
        if (state.dataWaterThermalTanks->WaterThermalTank(CompNum).CheckWTTEquipName) {
            if (CompName != state.dataWaterThermalTanks->WaterThermalTank(CompNum).Name) {
                ShowFatalError(state,
                               format("SimWaterThermalTank_WaterTank: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      CompNum,
                                      CompName,
                                      state.dataWaterThermalTanks->WaterThermalTank(CompNum).Name));
            }
            state.dataWaterThermalTanks->WaterThermalTank(CompNum).CheckWTTEquipName = false;
        }
    }

    return CompNum;
}

int getHPTankIDX(EnergyPlusData &state, std::string_view CompName, int &CompIndex)
{
    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    int CompNum;

    if (CompIndex == 0) {
        CompNum = UtilityRoutines::FindItem(CompName, state.dataWaterThermalTanks->HPWaterHeater);
        if (CompNum == 0) {
            ShowFatalError(state, "SimWaterThermalTank_HeatPump:  Unit not found=" + std::string{CompName});
        }
        CompIndex = CompNum;
    } else {
        CompNum = CompIndex;
        if (CompNum > state.dataWaterThermalTanks->numWaterThermalTank || CompNum < 1) {
            ShowFatalError(state,
                           format("SimWaterThermalTank_HeatPump:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  CompNum,
                                  state.dataWaterThermalTanks->numHeatPumpWaterHeater,
                                  CompName));
        }
        if (state.dataWaterThermalTanks->HPWaterHeater(CompNum).CheckHPWHEquipName) {
            if (CompName != state.dataWaterThermalTanks->HPWaterHeater(CompNum).Name) {
                ShowFatalError(state,
                               format("SimWaterThermalTank_HeatPump: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      CompNum,
                                      CompName,
                                      state.dataWaterThermalTanks->HPWaterHeater(CompNum).Name));
            }
            state.dataWaterThermalTanks->HPWaterHeater(CompNum).CheckHPWHEquipName = false;
        }
    }

    return CompNum;
}

void WaterThermalTankData::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, [[maybe_unused]] bool RunFlag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brandon Anderson
    //       DATE WRITTEN   May 2000
    //       MODIFIED       FSEC, July 2005
    //       RE-ENGINEERED  na

    // set the caller loop num to mimic what was happening in plant loop equip
    this->callerLoopNum = calledFromLocation.loopNum;

    if (this->myOneTimeInitFlag) {
        this->setupOutputVars(state);
        this->myOneTimeInitFlag = false;
    }

    if (this->MyOneTimeFlagWH) {
        this->MyOneTimeFlagWH = false;
    } else {
        if (this->MyTwoTimeFlagWH) {
            this->MinePlantStructForInfo(state); // call it again to get control types filled out
            this->MyTwoTimeFlagWH = false;
        }
    }
    this->UseSideLoadRequested = std::abs(CurLoad);
    if (this->UseSide.loopNum > 0 && this->UseSide.loopSideNum > 0 && !state.dataGlobal->KickOffSimulation) {
        this->UseCurrentFlowLock = state.dataPlnt->PlantLoop(this->UseSide.loopNum).LoopSide(this->UseSide.loopSideNum).FlowLock;
    } else {
        this->UseCurrentFlowLock = DataPlant::iFlowLock::Locked;
    }
    this->initialize(state, FirstHVACIteration);
    //       Plant connected water heaters may have a desuperheater heating coil attached
    if (this->DesuperheaterNum == 0) {
        if ((this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed) || (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankMixed)) {
            this->CalcWaterThermalTankMixed(state);
        } else if ((this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) || (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified)) {
            this->CalcWaterThermalTankStratified(state);
        }
    } else if (this->DesuperheaterNum > 0) {
        this->CalcDesuperheaterWaterHeater(state, FirstHVACIteration);
    }
    this->UpdateWaterThermalTank(state);
    this->ReportWaterThermalTank(state);
    // reset the caller loop num to mimic what was happening in PlantLoopEquip
    this->callerLoopNum = 0;
}

PlantComponent *HeatPumpWaterHeaterData::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data
    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    // Now look for this object in the list
    for (auto &HPWH : state.dataWaterThermalTanks->HPWaterHeater) {
        if (HPWH.Name == objectName) {
            return &HPWH;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "LocalHeatPumpWaterHeaterFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void HeatPumpWaterHeaterData::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
{
    auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(this->WaterHeaterTankNum);
    Tank.onInitLoopEquip(state, calledFromLocation);
}

void HeatPumpWaterHeaterData::getDesignCapacities([[maybe_unused]] EnergyPlusData &state,
                                                  [[maybe_unused]] const PlantLocation &calledFromLocation,
                                                  Real64 &MaxLoad,
                                                  Real64 &MinLoad,
                                                  Real64 &OptLoad)
{
    MinLoad = 0.0;
    MaxLoad = this->Capacity;
    OptLoad = this->Capacity;
}

void HeatPumpWaterHeaterData::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, [[maybe_unused]] bool RunFlag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brandon Anderson
    //       DATE WRITTEN   May 2000
    //       MODIFIED       FSEC, July 2005
    //       RE-ENGINEERED  na

    auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(this->WaterHeaterTankNum);

    // set caller loop num to mimic what plantloopequip was doing
    Tank.callerLoopNum = calledFromLocation.loopNum;

    if (this->myOneTimeInitFlag) {
        if (Tank.myOneTimeInitFlag) {
            Tank.setupOutputVars(state);
            Tank.myOneTimeInitFlag = false;
        }
        this->myOneTimeInitFlag = false;
    }

    if (this->MyOneTimeFlagHP) {
        this->MyOneTimeFlagHP = false;
    } else {
        if (this->MyTwoTimeFlagHP) {
            Tank.MinePlantStructForInfo(state); // call it again to get control types filled out
            this->MyTwoTimeFlagHP = false;
        }
    }
    Tank.UseSideLoadRequested = std::abs(CurLoad);
    if (Tank.UseSide.loopNum > 0 && Tank.UseSide.loopSideNum > 0 && !state.dataGlobal->KickOffSimulation) {
        Tank.UseCurrentFlowLock = state.dataPlnt->PlantLoop(Tank.UseSide.loopNum).LoopSide(Tank.UseSide.loopSideNum).FlowLock;
    } else {
        Tank.UseCurrentFlowLock = DataPlant::iFlowLock::Locked;
    }

    Tank.initialize(state, FirstHVACIteration);

    int InletNodeSav = this->HeatPumpAirInletNode;
    int OutletNodeSav = this->HeatPumpAirOutletNode;
    int DXINletNodeSav = this->DXCoilAirInletNode;
    int IHPFanIndexSav = this->FanNum;
    std::string IHPFanNameSave = this->FanName;
    int IHPFanplaceSav = this->FanPlacement;

    if (this->bIsIHP) // pass the tank indexes to the IHP object
    {
        state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).WHtankType = this->TypeNum;
        state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).WHtankName = this->Name;
        state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).WHtankID = this->WaterHeaterTankNum;
        IntegratedHeatPump::IHPOperationMode IHPMode = IntegratedHeatPump::GetCurWorkMode(state, this->DXCoilNum);

        if ((IntegratedHeatPump::IHPOperationMode::DWHMode == IHPMode) || (IntegratedHeatPump::IHPOperationMode::SCDWHMode == IHPMode) ||
            (IntegratedHeatPump::IHPOperationMode::SHDWHElecHeatOffMode == IHPMode) ||
            (IntegratedHeatPump::IHPOperationMode::SHDWHElecHeatOnMode == IHPMode)) { // default is to specify the air nodes for SCWH mode
            bool bDWHCoilReading = false;
            this->HeatPumpAirInletNode =
                VariableSpeedCoils::GetCoilInletNodeVariableSpeed(state,
                                                                  "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
                                                                  state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).DWHCoilName,
                                                                  bDWHCoilReading);
            this->HeatPumpAirOutletNode =
                VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(state,
                                                                   "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
                                                                   state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).DWHCoilName,
                                                                   bDWHCoilReading);
            this->DXCoilAirInletNode = this->HeatPumpAirInletNode;
        } else // default is to input outdoor fan to the the this
        {
            this->FanNum = state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).IDFanID;
            this->FanName = state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).IDFanName;
            this->FanPlacement = state.dataIntegratedHP->IntegratedHeatPumps(this->DXCoilNum).IDFanPlace;
        }
    }

    Tank.CalcHeatPumpWaterHeater(state, FirstHVACIteration);
    Tank.UpdateWaterThermalTank(state);
    Tank.ReportWaterThermalTank(state);

    this->HeatPumpAirInletNode = InletNodeSav;
    this->HeatPumpAirOutletNode = OutletNodeSav;
    this->DXCoilAirInletNode = DXINletNodeSav;
    this->FanNum = IHPFanIndexSav;
    this->FanName = IHPFanNameSave;
    this->FanPlacement = IHPFanplaceSav;
    // reset caller loop num to 0 to mimic what plantloopequip was doing
    Tank.callerLoopNum = 0;
}

void SimulateWaterHeaterStandAlone(EnergyPlusData &state, int const WaterHeaterNum, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       July 2005, FSEC - added HPWHs and desuperheater water heating coils
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine acts an interface to SimWaterHeater for stand-alone water heaters with no plant connections,
    // HPWHs not defined as zone equipment with no plant connections, and stand-alone water heaters with
    // desuperheater heating coils with no plant connections.

    // METHODOLOGY EMPLOYED:
    // The necessary control flags and dummy variables are set and passed into SimWaterHeater. This subroutine is
    // called from NonZoneEquipmentManager.

    Real64 MyLoad;

    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WaterHeaterNum);

    // Only simulate stand-alone water heaters here.  Plant connected water heaters are called by the PlantLoopEquipments.
    if (Tank.StandAlone) {
        bool localRunFlag = true;
        PlantLocation A(0, 0, 0, 0);
        Tank.simulate(state, A, FirstHVACIteration, MyLoad, localRunFlag);

        // HPWHs with inlet air from a zone and not connected to a plant loop are simulated through a CALL from ZoneEquipmentManager.
        // HPWHs that are plant connected are always simulated through a CALL from PlantLoopEquipments directly to SimWaterThermalTank.

        // NOTE: HPWHs with inlet air from a zone AND plant connected are not stand alone and are simulated in PlantLoopEquipments
    } else if (Tank.HeatPumpNum > 0) {
        //   Only HPWHs with inlet air from outdoors or scheduled HPWHs (not connected to a plant loop) are simulated here.

        auto &HPWaterHtr = state.dataWaterThermalTanks->HPWaterHeater(Tank.HeatPumpNum);

        if (HPWaterHtr.StandAlone &&
            (HPWaterHtr.InletAirConfiguration == AmbientTempEnum::OutsideAir || HPWaterHtr.InletAirConfiguration == AmbientTempEnum::Schedule)) {
            bool LocalRunFlag = true;
            PlantLocation A(0, 0, 0, 0);
            HPWaterHtr.simulate(state, A, FirstHVACIteration, MyLoad, LocalRunFlag);
        }

        // Only simulate stand-alone water heaters with desuperheater water heating coils here.  Plant connected water heaters
        // with desuperheater water heating coils are called by PlantLoopEquipments.
    } else if (Tank.DesuperheaterNum > 0) {
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(Tank.DesuperheaterNum).StandAlone) {
            bool localRunFlag = true;
            PlantLocation A(0, 0, 0, 0);
            Tank.simulate(state, A, FirstHVACIteration, MyLoad, localRunFlag);
        }
    }
}

void SimHeatPumpWaterHeater(EnergyPlusData &state,
                            std::string_view CompName,
                            bool const FirstHVACIteration,
                            Real64 &SensLoadMet, // sensible load met by this equipment and sent to zone, W
                            Real64 &LatLoadMet,  // net latent load met and sent to zone (kg/s), dehumid = negative
                            int &CompIndex)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   April 2005
    //       MODIFIED       Don Shirey, Aug 2009 (LatLoadMet)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine acts as an interface to SimWaterHeater.
    // HPWHs defined as zone equipment and not connected to a plant loop are called here by ZoneEquipmentManager

    // METHODOLOGY EMPLOYED:
    // The necessary control flags and dummy variables are set and passed into SimWaterHeater.

    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    // Find the correct Heat Pump Water Heater
    int HeatPumpNum;
    if (CompIndex == 0) {
        HeatPumpNum = UtilityRoutines::FindItemInList(CompName, state.dataWaterThermalTanks->HPWaterHeater);
        if (HeatPumpNum == 0) {
            ShowFatalError(state, "SimHeatPumpWaterHeater: Unit not found=" + std::string{CompName});
        }
        CompIndex = HeatPumpNum;
    } else {
        HeatPumpNum = CompIndex;
        if (HeatPumpNum > state.dataWaterThermalTanks->numHeatPumpWaterHeater || HeatPumpNum < 1) {
            ShowFatalError(state,
                           format("SimHeatPumpWaterHeater:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  HeatPumpNum,
                                  state.dataWaterThermalTanks->numHeatPumpWaterHeater,
                                  CompName));
        }
    }

    // Only simulate HPWHs specified as zone equipment and not connected to a plant loop.
    // HPWHs not defined as zone equipment with no plant connections are simulated in NonZoneEquipmentManager.
    // Plant connected HPWHs are called by PlantLoopEquipments (but only those on supply side ).
    // HPWH will not be included in sizing calculations, fan is initialized only during BeginEnvrnFlag (FALSE during sizing)
    // (fan will be turned off during Standard Ratings procedure yielding incorrect results)
    if (state.dataGlobal->DoingSizing) return;

    // For HPWHs, StandAlone means not connected to a plant loop (use nodes are not used, source nodes are connected to a HPWH)
    if (state.dataWaterThermalTanks->HPWaterHeater(HeatPumpNum).StandAlone) {
        bool LocalRunFlag = true;
        Real64 MyLoad;

        PlantLocation A(0, 0, 0, 0);
        state.dataWaterThermalTanks->HPWaterHeater(HeatPumpNum).simulate(state, A, FirstHVACIteration, MyLoad, LocalRunFlag);

        SensLoadMet = state.dataWaterThermalTanks->HPWaterHeater(HeatPumpNum).HPWaterHeaterSensibleCapacity;
        LatLoadMet = state.dataWaterThermalTanks->HPWaterHeater(HeatPumpNum).HPWaterHeaterLatentCapacity;
    } else {
        // HPWH is plant connected and will get simulated when called from plant SimWaterThermalTank, but need to update loads met here
        SensLoadMet = state.dataWaterThermalTanks->HPWaterHeater(HeatPumpNum).HPWaterHeaterSensibleCapacity;
        LatLoadMet = state.dataWaterThermalTanks->HPWaterHeater(HeatPumpNum).HPWaterHeaterLatentCapacity;
    }
}

void CalcWaterThermalTankZoneGains(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   March 2005
    //       MODIFIED       B. Griffith November 2011, new internal gains structure
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the zone internal gains due to water heater skin losses during sizing.
    // initializes gains to zone at begin environment.

    // METHODOLOGY EMPLOYED:
    // Sums the tank losses from all of the water heaters in the zone to add as a gain to the zone.
    // Now used to determine tank losses during sizing.  Internal gains are summed in a centralized way now

    if (state.dataWaterThermalTanks->numWaterThermalTank == 0) {

        if (!state.dataGlobal->DoingSizing) {
            return;
        } else {
            if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
                GetWaterThermalTankInput(state);
                state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
            }
            if (state.dataWaterThermalTanks->numWaterThermalTank == 0) return;
        }
    }

    if (state.dataGlobal->BeginEnvrnFlag && state.dataWaterThermalTanks->calcWaterThermalTankZoneGainsMyEnvrnFlag) {
        for (auto &e : state.dataWaterThermalTanks->WaterThermalTank) {
            e.AmbientZoneGain = 0.0;
            e.FuelEnergy = 0.0;
            e.OffCycParaFuelEnergy = 0.0;
            e.OnCycParaFuelEnergy = 0.0;
        }
        state.dataWaterThermalTanks->calcWaterThermalTankZoneGainsMyEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) state.dataWaterThermalTanks->calcWaterThermalTankZoneGainsMyEnvrnFlag = true;

    for (int WaterThermalTankNum = 1; WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterThermalTank; ++WaterThermalTankNum) {
        auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum);
        if (Tank.AmbientTempZone == 0) continue;
        if (state.dataGlobal->DoingSizing) {
            // Initialize tank temperature to setpoint
            // (use HPWH or Desuperheater heating coil set point if applicable)
            int SchIndex;
            if (Tank.HeatPumpNum > 0) {
                SchIndex = state.dataWaterThermalTanks->HPWaterHeater(Tank.HeatPumpNum).SetPointTempSchedule;
            } else if (Tank.DesuperheaterNum > 0) {
                SchIndex = state.dataWaterThermalTanks->WaterHeaterDesuperheater(Tank.DesuperheaterNum).SetPointTempSchedule;
            } else {
                SchIndex = Tank.SetPointTempSchedule;
            }

            Real64 TankTemp;
            Real64 QLossToZone = 0.0;
            if (SchIndex > 0) {
                TankTemp = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);
            } else {
                TankTemp = 20.0;
            }
            {
                auto const SELECT_CASE_var(Tank.TypeNum);
                if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                    QLossToZone = max(Tank.OnCycLossCoeff * Tank.OnCycLossFracToZone, Tank.OffCycLossCoeff * Tank.OffCycLossFracToZone) *
                                  (TankTemp - state.dataHeatBalFanSys->MAT(Tank.AmbientTempZone));
                } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                    QLossToZone = max(Tank.Node(1).OnCycLossCoeff * Tank.SkinLossFracToZone, Tank.Node(1).OffCycLossCoeff * Tank.SkinLossFracToZone) *
                                  (TankTemp - state.dataHeatBalFanSys->MAT(Tank.AmbientTempZone));
                } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankMixed) {
                    QLossToZone = Tank.OffCycLossCoeff * Tank.OffCycLossFracToZone * (TankTemp - state.dataHeatBalFanSys->MAT(Tank.AmbientTempZone));
                } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankStratified) {
                    QLossToZone =
                        Tank.Node(1).OffCycLossCoeff * Tank.SkinLossFracToZone * (TankTemp - state.dataHeatBalFanSys->MAT(Tank.AmbientTempZone));
                }
            }
            Tank.AmbientZoneGain = QLossToZone;
        }
    }
}

bool getDesuperHtrInput(EnergyPlusData &state)
{
    bool ErrorsFound = false;
    static constexpr std::string_view RoutineName = "getDesuperHtrInput";

    state.dataIPShortCut->cCurrentModuleObject = cCoilDesuperheater;
    for (int DesuperheaterNum = 1; DesuperheaterNum <= state.dataWaterThermalTanks->numWaterHeaterDesuperheater; ++DesuperheaterNum) {
        int NumAlphas;
        int NumNums;
        int IOStat;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 DesuperheaterNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        GlobalNames::VerifyUniqueCoilName(state,
                                          state.dataIPShortCut->cCurrentModuleObject,
                                          state.dataIPShortCut->cAlphaArgs(1),
                                          ErrorsFound,
                                          state.dataIPShortCut->cCurrentModuleObject + " Name");

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Type = state.dataIPShortCut->cCurrentModuleObject;

        //       convert availability schedule name to pointer
        if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr =
                ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr == 0) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        } else {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        }

        //       convert schedule name to pointer
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).SetPointTempSchedule =
            ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).SetPointTempSchedule == 0) {
            ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff <= 0.0 ||
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff > 20.0) {
            ShowSevereError(state,
                            format("{} = {}: {} must be > 0 and <= 20. {} = {:.1T}",
                                   state.dataIPShortCut->cCurrentModuleObject,
                                   state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                   state.dataIPShortCut->cNumericFieldNames(1),
                                   state.dataIPShortCut->cNumericFieldNames(1),
                                   state.dataIPShortCut->rNumericArgs(1)));
            ErrorsFound = true;
        }

        // WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = state.dataIPShortCut->rNumericArgs(2)
        // Error limits on heat reclaim efficiency applied after source type identified

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).RatedInletWaterTemp = state.dataIPShortCut->rNumericArgs(3);
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).RatedOutdoorAirTemp = state.dataIPShortCut->rNumericArgs(4);
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp = state.dataIPShortCut->rNumericArgs(5);

        if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp =
                CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " +
                                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name + ":  " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " not found = " + state.dataIPShortCut->cAlphaArgs(4));
                ErrorsFound = true;
            } else {
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp, // Curve index
                                                 {2},                                                                          // Valid dimensions
                                                 RoutineName,                                                                  // Routine name
                                                 state.dataIPShortCut->cCurrentModuleObject,                                   // Object Type
                                                 state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name, // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(4));                                   // Field Name
                if (!ErrorsFound) {
                    if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp > 0) {
                        Real64 HEffFTemp = min(1.0,
                                               max(0.0,
                                                   CurveManager::CurveValue(
                                                       state,
                                                       state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp,
                                                       state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).RatedInletWaterTemp,
                                                       state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).RatedOutdoorAirTemp)));
                        if (std::abs(HEffFTemp - 1.0) > 0.05) {
                            ShowWarningError(state,
                                             state.dataIPShortCut->cCurrentModuleObject + ", \"" +
                                                 state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name + "\":");
                            ShowContinueError(state, "The " + state.dataIPShortCut->cAlphaFieldNames(4) + " should be normalized ");
                            ShowContinueError(state, format(" to 1.0 at the rating point. Curve output at the rating point = {:.3T}", HEffFTemp));
                            ShowContinueError(state, " The simulation continues using the user-specified curve.");
                        }
                    }
                }
            }
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).WaterInletNode =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(5),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                1,
                                                DataLoopNode::ObjectIsParent);

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).WaterOutletNode =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(6),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                1,
                                                DataLoopNode::ObjectIsParent);

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).InletNodeName1 = state.dataIPShortCut->cAlphaArgs(5);
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OutletNodeName1 = state.dataIPShortCut->cAlphaArgs(6);

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankType = state.dataIPShortCut->cAlphaArgs(7);

        if (!UtilityRoutines::SameString(state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankType, cMixedWHModuleObj) &&
            !UtilityRoutines::SameString(state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankType, cStratifiedWHModuleObj)) {

            ShowSevereError(
                state, state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataWaterThermalTanks->HPWaterHeater(DesuperheaterNum).Name + ":");
            ShowContinueError(state, "Desuperheater can only be used with " + cMixedWHModuleObj + " or " + cStratifiedWHModuleObj + ".");
            ErrorsFound = true;
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankName = state.dataIPShortCut->cAlphaArgs(8);

        //       Set up comp set for water side nodes (reverse inlet/outlet for water heater)
        BranchNodeConnections::SetUpCompSets(state,
                                             state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Type,
                                             state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                             state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankType,
                                             state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankName,
                                             state.dataIPShortCut->cAlphaArgs(6),
                                             state.dataIPShortCut->cAlphaArgs(5));

        if ((UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:Condenser:AirCooled")) ||
            (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:Condenser:EvaporativeCooled")) ||
            (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:Condenser:WaterCooled"))) {
            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = 0.8;
            } else {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff =
                    state.dataIPShortCut->rNumericArgs(2);
                if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff <= 0.0 ||
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff > 0.9) {
                    ShowSevereError(state,
                                    format("{} = {}: {} must be > 0.0 and <= 0.9, Efficiency = {:.3T}",
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                           state.dataIPShortCut->cNumericFieldNames(2),
                                           state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff));
                    ErrorsFound = true;
                }
            }    // Blank Num(2)
        } else { // max is 0.3 for all other sources
            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = 0.25;
            } else {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff =
                    state.dataIPShortCut->rNumericArgs(2);
                if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff <= 0.0 ||
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff > 0.3) {
                    ShowSevereError(state,
                                    format("{} = {}: {} must be > 0.0 and <= 0.3, {} = {:.3T}",
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                           state.dataIPShortCut->cNumericFieldNames(2),
                                           state.dataIPShortCut->cNumericFieldNames(2),
                                           state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff));
                    ErrorsFound = true;
                }
            } // Blank Num(2)
        }     // setting limits on heat recovery efficiency

        //       Find the Refrigeration equipment index associated with the desuperheater heating coil.
        bool errFlag = false;
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceType = state.dataIPShortCut->cAlphaArgs(9);
        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName = state.dataIPShortCut->cAlphaArgs(10);
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:CompressorRack")) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource =
                CoilObjEnum::CompressorRackRefrigeratedCase;
            for (int RackNum = 1; RackNum <= state.dataRefrigCase->NumRefrigeratedRacks; ++RackNum) {
                if (!UtilityRoutines::SameString(state.dataHeatBal->HeatReclaimRefrigeratedRack(RackNum).Name, state.dataIPShortCut->cAlphaArgs(10)))
                    continue;
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = RackNum;
                if (allocated(state.dataHeatBal->HeatReclaimRefrigeratedRack)) {
                    DataHeatBalance::HeatReclaimDataBase &HeatReclaim = state.dataHeatBal->HeatReclaimRefrigeratedRack(
                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum);
                    if (!allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)) {
                        HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat.allocate(state.dataWaterThermalTanks->numWaterHeaterDesuperheater);
                        for (auto &num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)
                            num = 0.0;
                    }
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                    HeatReclaim.ReclaimEfficiencyTotal +=
                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;
                    if (HeatReclaim.ReclaimEfficiencyTotal > 0.3) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + " = " +
                                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            ": "
                                            " sum of heat reclaim recovery efficiencies from the same source coil: \"" +
                                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName +
                                            "\" cannot be over 0.3");
                        ErrorsFound = true;
                    }
                }
                break;
            }
        } else if ((UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:Condenser:AirCooled")) ||
                   (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:Condenser:EvaporativeCooled")) ||
                   (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Refrigeration:Condenser:WaterCooled"))) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CoilObjEnum::CondenserRefrigeration;
            for (int CondNum = 1; CondNum <= state.dataRefrigCase->NumRefrigCondensers; ++CondNum) {
                if (!UtilityRoutines::SameString(state.dataHeatBal->HeatReclaimRefrigCondenser(CondNum).Name, state.dataIPShortCut->cAlphaArgs(10)))
                    continue;
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = CondNum;
                if (allocated(state.dataHeatBal->HeatReclaimRefrigCondenser)) {
                    DataHeatBalance::HeatReclaimDataBase &HeatReclaim = state.dataHeatBal->HeatReclaimRefrigCondenser(
                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum);
                    if (!allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)) {
                        HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat.allocate(state.dataWaterThermalTanks->numWaterHeaterDesuperheater);
                        for (auto &num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)
                            num = 0.0;
                    }
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                    HeatReclaim.ReclaimEfficiencyTotal +=
                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;
                    if (HeatReclaim.ReclaimEfficiencyTotal > 0.9) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + " = " +
                                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            ": "
                                            " sum of heat reclaim recovery efficiencies from the same source coil: \"" +
                                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName +
                                            "\" cannot be over 0.9");
                        ErrorsFound = true;
                    }
                }
                break;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:SingleSpeed") ||
                   UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:TwoSpeed") ||
                   UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:MultiSpeed") ||
                   UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:SingleSpeed")) {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CoilObjEnum::DXCooling;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CoilObjEnum::DXMultiMode;
            } else {
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CoilObjEnum::DXMultiSpeed;
            }
            DXCoils::GetDXCoilIndex(state,
                                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName,
                                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum,
                                    errFlag,
                                    state.dataIPShortCut->cCurrentModuleObject,
                                    ObjexxFCL::Optional_bool_const());
            if (allocated(state.dataHeatBal->HeatReclaimDXCoil)) {
                DataHeatBalance::HeatReclaimDataBase &HeatReclaim = state.dataHeatBal->HeatReclaimDXCoil(
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum);
                if (!allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)) {
                    HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat.allocate(state.dataWaterThermalTanks->numWaterHeaterDesuperheater);
                    for (auto &num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)
                        num = 0.0;
                }
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                HeatReclaim.ReclaimEfficiencyTotal += state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;
                if (HeatReclaim.ReclaimEfficiencyTotal > 0.3) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " +
                                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                        ": "
                                        " sum of heat reclaim recovery efficiencies from the same source coil: \"" +
                                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName +
                                        "\" cannot be over 0.3");
                    ErrorsFound = true;
                }
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:DX:VariableSpeed")) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CoilObjEnum::DXVariableCooling;
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum =
                VariableSpeedCoils::GetCoilIndexVariableSpeed(
                    state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), errFlag);
            if (allocated(state.dataHeatBal->HeatReclaimVS_DXCoil)) {
                DataHeatBalance::HeatReclaimDataBase &HeatReclaim = state.dataHeatBal->HeatReclaimVS_DXCoil(
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum);
                if (!allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)) {
                    HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat.allocate(state.dataWaterThermalTanks->numWaterHeaterDesuperheater);
                    for (auto &num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)
                        num = 0.0;
                }
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                HeatReclaim.ReclaimEfficiencyTotal += state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;
                if (HeatReclaim.ReclaimEfficiencyTotal > 0.3) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " +
                                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                        ": "
                                        " sum of heat reclaim recovery efficiencies from the same source coil: \"" +
                                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName +
                                        "\" cannot be over 0.3");
                    ErrorsFound = true;
                }
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Coil:Cooling:WaterToAirHeatPump:EquationFit")) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CoilObjEnum::AirWaterHeatPumpEQ;
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum =
                WaterToAirHeatPumpSimple::GetCoilIndex(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), errFlag);
            if (allocated(state.dataHeatBal->HeatReclaimSimple_WAHPCoil)) {
                DataHeatBalance::HeatReclaimDataBase &HeatReclaim = state.dataHeatBal->HeatReclaimSimple_WAHPCoil(
                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum);
                if (!allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)) {
                    HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat.allocate(state.dataWaterThermalTanks->numWaterHeaterDesuperheater);
                    for (auto &num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)
                        num = 0.0;
                }
                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                HeatReclaim.ReclaimEfficiencyTotal += state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;
                if (HeatReclaim.ReclaimEfficiencyTotal > 0.3) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " +
                                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                        ": "
                                        " sum of heat reclaim recovery efficiencies from the same source coil: \"" +
                                        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName +
                                        "\" cannot be over 0.3");
                    ErrorsFound = true;
                }
            }
        } else {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " +
                                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
            ShowContinueError(state, " desuperheater can only be used with Coil:Cooling:DX:SingleSpeed, ");
            ShowContinueError(state,
                              " Coil:Cooling:DX:TwoSpeed, Coil:Cooling:DX:MultiSpeed, Coil:Cooling:DX:TwoStageWithHumidityControlMode, "
                              "Coil:Cooling:DX:VariableSpeed, "
                              "Coil:Cooling:WaterToAirHeatPump:EquationFit, Refrigeration:CompressorRack,");
            ShowContinueError(state, " Refrigeration:Condenser:AirCooled ,Refrigeration:Condenser:EvaporativeCooled, ");
            ShowContinueError(state, " or Refrigeration:Condenser:WaterCooled.");
            ShowContinueError(state,
                              " Invalid desuperheater heat source object: " + state.dataIPShortCut->cAlphaArgs(9) + " \"" +
                                  state.dataIPShortCut->cAlphaArgs(10) + "\"");
            ErrorsFound = true;
        }
        if (errFlag) {
            ShowContinueError(state,
                              "...occurs in " + state.dataIPShortCut->cCurrentModuleObject + '=' +
                                  state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name);
            ErrorsFound = true;
        }

        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + ", \"" +
                                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                "\" desuperheater heat source object not found: " + state.dataIPShortCut->cAlphaArgs(9) + " \"" +
                                state.dataIPShortCut->cAlphaArgs(10) + "\"");
            ErrorsFound = true;
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate = state.dataIPShortCut->rNumericArgs(6);
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate <= 0.0) {
            ShowSevereError(state,
                            format("{} = {}: {} must be greater than 0. {} = {:.6T}",
                                   state.dataIPShortCut->cCurrentModuleObject,
                                   state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                   state.dataIPShortCut->cNumericFieldNames(6),
                                   state.dataIPShortCut->cNumericFieldNames(6),
                                   state.dataIPShortCut->rNumericArgs(6)));
            ErrorsFound = true;
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower = state.dataIPShortCut->rNumericArgs(7);
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower < 0.0) {
            ShowSevereError(state,
                            format("{} = {}: {} must be >= 0. {} = {:.2T}",
                                   state.dataIPShortCut->cCurrentModuleObject,
                                   state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                   state.dataIPShortCut->cNumericFieldNames(7),
                                   state.dataIPShortCut->cNumericFieldNames(7),
                                   state.dataIPShortCut->rNumericArgs(7)));
            ErrorsFound = true;
        }

        if ((state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower /
             state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate) > 7.9264e6) {
            ShowWarningError(state,
                             format("{} = {}: {} to {} ratio > 7.9264E6. {} to {} = {:.3T}",
                                    state.dataIPShortCut->cCurrentModuleObject,
                                    state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                    state.dataIPShortCut->cNumericFieldNames(7),
                                    state.dataIPShortCut->cNumericFieldNames(6),
                                    state.dataIPShortCut->cNumericFieldNames(7),
                                    state.dataIPShortCut->cNumericFieldNames(6),
                                    (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower /
                                     state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate)));
            ShowContinueError(state,
                              " Suggest reducing " + state.dataIPShortCut->cNumericFieldNames(7) + " or increasing " +
                                  state.dataIPShortCut->cNumericFieldNames(6) + '.');
            ShowContinueError(state, " The simulation will continue using the user defined values.");
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater = state.dataIPShortCut->rNumericArgs(8);
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater < 0.0 ||
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater > 1.0) {
            ShowSevereError(state,
                            format("{} = {}: {} must be >= 0 or <= 1. {} = {:.3T}",
                                   state.dataIPShortCut->cCurrentModuleObject,
                                   state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                   state.dataIPShortCut->cNumericFieldNames(8),
                                   state.dataIPShortCut->cNumericFieldNames(8),
                                   state.dataIPShortCut->rNumericArgs(8)));
            ErrorsFound = true;
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad = state.dataIPShortCut->rNumericArgs(9);
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad < 0.0) {
            ShowSevereError(state,
                            format("{} = {}: {} must be >= 0. {} = {:.2T}",
                                   state.dataIPShortCut->cCurrentModuleObject,
                                   state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                   state.dataIPShortCut->cNumericFieldNames(9),
                                   state.dataIPShortCut->cNumericFieldNames(9),
                                   state.dataIPShortCut->rNumericArgs(9)));
            ErrorsFound = true;
        }

        state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad = state.dataIPShortCut->rNumericArgs(10);
        if (state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad < 0.0) {
            ShowSevereError(state,
                            format("{} = {}: {} must be >= 0. {} = {:.2T}",
                                   state.dataIPShortCut->cCurrentModuleObject,
                                   state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                   state.dataIPShortCut->cNumericFieldNames(10),
                                   state.dataIPShortCut->cNumericFieldNames(10),
                                   state.dataIPShortCut->rNumericArgs(10)));
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state,
                       "Errors found in getting " + state.dataIPShortCut->cCurrentModuleObject + " input. Preceding condition causes termination.");
    }

    return ErrorsFound;
} // namespace WaterThermalTanks

bool getHPWaterHeaterInput(EnergyPlusData &state)
{
    bool ErrorsFound = false;

    int const NumPumpedCondenser = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
        state, cHPWHPumpedCondenser); // number of WaterHeater:HeatPump:PumpedCondenser objects
    int nAlphaOffset;                 // the difference of array location between alpha items between pumped and wrapped condensers
    int nNumericOffset;               // the difference of array location between numeric items between pumped and wrapped condensers
    int nNumPossibleNumericArgs;      // the number of possible numeric arguments in the idd
    int nNumPossibleAlphaArgs;        // the number of possible numeric arguments in the idd

    // For looking up in IDF/epJSON, you need the index that corresponds to the actual object type (Pumped or Wrapped)
    int HPWaterHeaterNumOfSpecificType;

    for (int HPWaterHeaterNum = 1; HPWaterHeaterNum <= state.dataWaterThermalTanks->numHeatPumpWaterHeater; ++HPWaterHeaterNum) {

        // Create reference to current HPWH object in array.
        HeatPumpWaterHeaterData &HPWH = state.dataWaterThermalTanks->HPWaterHeater(HPWaterHeaterNum);

        // Initialize the offsets to zero
        nAlphaOffset = 0;
        nNumericOffset = 0;

        if (HPWaterHeaterNum <= NumPumpedCondenser) {
            // Pumped Condenser
            state.dataIPShortCut->cCurrentModuleObject = cHPWHPumpedCondenser;
            HPWH.TypeNum = DataPlant::TypeOf_HeatPumpWtrHeaterPumped;
            nNumPossibleAlphaArgs = 29;
            nNumPossibleNumericArgs = 9;
            // Actual index of Pumped type
            HPWaterHeaterNumOfSpecificType = HPWaterHeaterNum;
        } else {
            // Wrapped Condenser
            state.dataIPShortCut->cCurrentModuleObject = cHPWHWrappedCondenser;
            HPWH.TypeNum = DataPlant::TypeOf_HeatPumpWtrHeaterWrapped;
            nNumPossibleAlphaArgs = 27;
            nNumPossibleNumericArgs = 10;
            // Actual index of Wrapped type
            HPWaterHeaterNumOfSpecificType = HPWaterHeaterNum - NumPumpedCondenser;
        }

        int NumAlphas;
        int NumNums;
        int IOStat;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 HPWaterHeaterNumOfSpecificType,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        // Copy those lists into C++ std::maps
        std::map<int, std::string> hpwhAlpha;
        std::map<int, Real64> hpwhNumeric;
        std::map<int, bool> hpwhAlphaBlank;
        std::map<int, bool> hpwhNumericBlank;
        std::map<int, std::string> hpwhAlphaFieldNames;
        std::map<int, std::string> hpwhNumericFieldNames;
        for (int i = 1; i <= NumNums; ++i) {
            hpwhNumeric[i] = state.dataIPShortCut->rNumericArgs(i);
            hpwhNumericBlank[i] = state.dataIPShortCut->lNumericFieldBlanks(i);
            hpwhNumericFieldNames[i] = state.dataIPShortCut->cNumericFieldNames(i);
        }
        for (int i = NumNums + 1; i <= nNumPossibleNumericArgs; ++i) {
            hpwhNumericBlank[i] = true;
        }
        for (int i = 1; i <= NumAlphas; ++i) {
            hpwhAlpha[i] = state.dataIPShortCut->cAlphaArgs(i);
            hpwhAlphaBlank[i] = state.dataIPShortCut->lAlphaFieldBlanks(i);
            hpwhAlphaFieldNames[i] = state.dataIPShortCut->cAlphaFieldNames(i);
        }
        for (int i = NumAlphas + 1; i <= nNumPossibleAlphaArgs; ++i) {
            hpwhAlphaBlank[i] = true;
        }
        UtilityRoutines::IsNameEmpty(state, hpwhAlpha[1], state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);

        // Name and type
        HPWH.Name = hpwhAlpha[1];
        HPWH.Type = state.dataIPShortCut->cCurrentModuleObject;

        // Availability Schedule
        // convert schedule name to pointer
        if (!hpwhAlphaBlank[2]) {
            HPWH.AvailSchedPtr = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[2]);
            if (HPWH.AvailSchedPtr == 0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                ShowContinueError(state, hpwhAlphaFieldNames[2] + "=\"" + hpwhAlpha[2] + "\".");
                ErrorsFound = true;
            }
        } else {
            HPWH.AvailSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        }

        // Compressor Setpoint Temperature Schedule
        // convert schedule name to pointer
        if (!hpwhAlphaBlank[3]) {
            HPWH.SetPointTempSchedule = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[3]);
            if (HPWH.SetPointTempSchedule == 0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                ShowContinueError(state, hpwhAlphaFieldNames[3] + "=\"" + hpwhAlpha[3] + "\".");
                ErrorsFound = true;
            }
        } else {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
            ShowContinueError(state, "required " + hpwhAlphaFieldNames[3] + " is blank.");
            ErrorsFound = true;
        }

        // Dead Band Temperature Difference
        HPWH.DeadBandTempDiff = hpwhNumeric[1 + nNumericOffset];
        if (HPWH.DeadBandTempDiff <= 0.0 || HPWH.DeadBandTempDiff > 20.0) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
            ShowContinueError(state,
                              hpwhNumericFieldNames[1 + nNumericOffset] +
                                  format(" difference must be > 0 and <= 20. Dead band = {:.1T}", hpwhNumeric[1 + nNumericOffset]));
            ErrorsFound = true;
        }

        if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {

            // Condenser Inlet/Outlet Nodes
            HPWH.CondWaterInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                          hpwhAlpha[4],
                                                                          ErrorsFound,
                                                                          state.dataIPShortCut->cCurrentModuleObject,
                                                                          HPWH.Name,
                                                                          DataLoopNode::NodeFluidType::Water,
                                                                          DataLoopNode::NodeConnectionType::Inlet,
                                                                          2,
                                                                          DataLoopNode::ObjectIsParent);
            HPWH.InletNodeName1 = hpwhAlpha[4];
            HPWH.CondWaterOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                           hpwhAlpha[5],
                                                                           ErrorsFound,
                                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                                           HPWH.Name,
                                                                           DataLoopNode::NodeFluidType::Water,
                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                           2,
                                                                           DataLoopNode::ObjectIsParent);
            HPWH.OutletNodeName1 = hpwhAlpha[5];

            // Condenser Water Flow Rate
            HPWH.OperatingWaterFlowRate = hpwhNumeric[2];
            if (HPWH.OperatingWaterFlowRate <= 0.0 && hpwhNumeric[2] != DataGlobalConstants::AutoCalculate) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                ShowContinueError(state,
                                  format("{} must be greater than 0. Condenser water flow rate = {:.6T}", hpwhNumericFieldNames[2], hpwhNumeric[2]));
                ErrorsFound = true;
            }

        } else if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {

            // Wrapped Condenser Location
            HPWH.WrappedCondenserBottomLocation = hpwhNumeric[2 + nNumericOffset];
            HPWH.WrappedCondenserTopLocation = hpwhNumeric[3 + nNumericOffset];

            if (HPWH.WrappedCondenserBottomLocation < 0.0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                ShowContinueError(state,
                                  format("{} must be greater than 0. Condenser bottom location = {:.6T}",
                                         hpwhNumericFieldNames[2],
                                         HPWH.WrappedCondenserBottomLocation));
                ErrorsFound = true;
            }

            if (HPWH.WrappedCondenserBottomLocation >= HPWH.WrappedCondenserTopLocation) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                ShowContinueError(state,
                                  format("{} ({:.6T}) must be greater than {} ({:.6T}).",
                                         HPWH.WrappedCondenserTopLocation,
                                         hpwhNumericFieldNames[2],
                                         hpwhNumericFieldNames[3],
                                         HPWH.WrappedCondenserBottomLocation));
                ErrorsFound = true;
            }

            // Reset the offset
            nAlphaOffset = -2;
            nNumericOffset = 1;

        } else {
            assert(0);
        }

        // Evaporator Air Flow Rate
        HPWH.OperatingAirFlowRate = hpwhNumeric[3 + nNumericOffset];
        if (HPWH.OperatingAirFlowRate <= 0.0 && hpwhNumeric[3 + nNumericOffset] != DataGlobalConstants::AutoCalculate) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
            ShowContinueError(state,
                              hpwhNumericFieldNames[3 + nNumericOffset] +
                                  format(" must be greater than 0. Evaporator air flow rate = {:.6T}", hpwhNumeric[3 + nNumericOffset]));
            ErrorsFound = true;
        }

        // Inlet Air Configuration
        {
            auto const SELECT_CASE_var(hpwhAlpha[6 + nAlphaOffset]);

            if (SELECT_CASE_var == "SCHEDULE") {
                HPWH.InletAirConfiguration = AmbientTempEnum::Schedule;

                // Inlet Air Temperature Schedule
                if (!hpwhAlphaBlank[11 + nAlphaOffset]) {
                    HPWH.AmbientTempSchedule = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[11 + nAlphaOffset]);
                    if (HPWH.AmbientTempSchedule == 0) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                        ShowContinueError(state, hpwhAlphaFieldNames[11 + nAlphaOffset] + "=\"" + hpwhAlpha[11 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                    ShowContinueError(state, "required " + hpwhAlphaFieldNames[11 + nAlphaOffset] + " is blank.");
                    ErrorsFound = true;
                }

                // Inlet Air Humidity Schedule
                if (!hpwhAlphaBlank[12 + nAlphaOffset]) {
                    HPWH.AmbientRHSchedule = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[12 + nAlphaOffset]);
                    if (HPWH.AmbientRHSchedule == 0) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                        ShowContinueError(state, hpwhAlphaFieldNames[12 + nAlphaOffset] + "=\"" + hpwhAlpha[12 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    } else {
                        if (!ScheduleManager::CheckScheduleValueMinMax(state, HPWH.AmbientRHSchedule, ">=", 0.0, "<=", 1.0)) {
                            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", invalid values");
                            ShowContinueError(state,
                                              hpwhAlphaFieldNames[12 + nAlphaOffset] + "=\"" + hpwhAlpha[12 + nAlphaOffset] +
                                                  "\", schedule values must be (>=0., <=1.)");
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                    ShowContinueError(state, "required " + hpwhAlphaFieldNames[12 + nAlphaOffset] + " is blank.");
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "ZONEAIRONLY") {
                HPWH.InletAirConfiguration = AmbientTempEnum::TempZone;

                // Inlet Air Zone
                if (!hpwhAlphaBlank[13 + nAlphaOffset]) {
                    HPWH.AmbientTempZone = UtilityRoutines::FindItemInList(hpwhAlpha[13 + nAlphaOffset], state.dataHeatBal->Zone);
                    if (HPWH.AmbientTempZone == 0) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                        ShowContinueError(state, hpwhAlphaFieldNames[13 + nAlphaOffset] + "=\"" + hpwhAlpha[13 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                    ShowContinueError(state, "required " + hpwhAlphaFieldNames[13 + nAlphaOffset] + " is blank.");
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "OUTDOORAIRONLY") {
                HPWH.InletAirConfiguration = AmbientTempEnum::OutsideAir;

            } else if (SELECT_CASE_var == "ZONEANDOUTDOORAIR") {
                HPWH.InletAirConfiguration = AmbientTempEnum::ZoneAndOA;

                // Inlet Air Zone
                if (!hpwhAlphaBlank[13 + nAlphaOffset]) {
                    HPWH.AmbientTempZone = UtilityRoutines::FindItemInList(hpwhAlpha[13 + nAlphaOffset], state.dataHeatBal->Zone);
                    if (HPWH.AmbientTempZone == 0) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                        ShowContinueError(state, hpwhAlphaFieldNames[13 + nAlphaOffset] + "=\"" + hpwhAlpha[13 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                    ShowContinueError(state, "required " + hpwhAlphaFieldNames[13 + nAlphaOffset] + " is blank.");
                    ErrorsFound = true;
                }
            }
        }

        // Read air inlet nodes after mixer/splitter nodes have been read in (state.dataIPShortCut->cAlphaArgs 7-10),
        // Node_ConnectionType differs for inlet node if mixer/splitter node exists

        // Tank Name
        // We will verify this exists and is the right kind of tank later when the tanks are all loaded.
        HPWH.TankName = hpwhAlpha[15 + nAlphaOffset];
        HPWH.TankType = hpwhAlpha[14 + nAlphaOffset];

        // Use Side Inlet/Outlet
        // Get the water heater tank use side inlet node names for HPWHs connected to a plant loop
        // Save the name of the node for use with set up comp sets
        HPWH.InletNodeName2 = hpwhAlpha[16 + nAlphaOffset];
        HPWH.OutletNodeName2 = hpwhAlpha[17 + nAlphaOffset];

        if (!hpwhAlphaBlank[16 + nAlphaOffset] && !hpwhAlphaBlank[17 + nAlphaOffset]) {
            HPWH.WHUseInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                      HPWH.InletNodeName2,
                                                                      ErrorsFound,
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      HPWH.Name,
                                                                      DataLoopNode::NodeFluidType::Water,
                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                      1,
                                                                      DataLoopNode::ObjectIsParent);
            HPWH.WHUseOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                       HPWH.OutletNodeName2,
                                                                       ErrorsFound,
                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                       HPWH.Name,
                                                                       DataLoopNode::NodeFluidType::Water,
                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                       1,
                                                                       DataLoopNode::ObjectIsParent);
        }

        // DX Coil
        // get Coil:DX:HeatPumpWaterHeater object
        HPWH.DXCoilName = hpwhAlpha[19 + nAlphaOffset];
        HPWH.DXCoilType = hpwhAlpha[18 + nAlphaOffset];

        // check that the DX Coil exists
        bool DXCoilErrFlag = false;
        bool bIsVScoil = false;
        DXCoils::GetDXCoilIndex(state, HPWH.DXCoilName, HPWH.DXCoilNum, DXCoilErrFlag, state.dataIPShortCut->cCurrentModuleObject, true);
        if (DXCoilErrFlag) {
            // This could be a variable speed heat pump water heater
            bool bVSCoilErrFlag = false;

            bool checkIHPFirst = IntegratedHeatPump::IHPInModel(state);
            if (checkIHPFirst) {
                HPWH.DXCoilNum =
                    IntegratedHeatPump::GetCoilIndexIHP(state, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", HPWH.DXCoilName, bVSCoilErrFlag);

                if (!bVSCoilErrFlag) {
                    HPWH.bIsIHP = true;
                }
            }

            if (bVSCoilErrFlag || !checkIHPFirst) {
                bVSCoilErrFlag = false;
                HPWH.DXCoilNum = VariableSpeedCoils::GetCoilIndexVariableSpeed(
                    state, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed", HPWH.DXCoilName, bVSCoilErrFlag);

                if (bVSCoilErrFlag) {
                    ShowContinueError(state, "...occurs in " + state.dataIPShortCut->cCurrentModuleObject + " =" + HPWH.Name);
                    ShowContinueError(state, "...could not find either DXCoils::DXCoil or Variable Speed Coil " + HPWH.DXCoilName);
                    ErrorsFound = true;
                }
            }

            bIsVScoil = true;
            HPWH.DXCoilTypeNum = 0;
            if (HPWH.bIsIHP) {
                HPWH.DXCoilType = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE";
            } else {
                HPWH.DXCoilType = state.dataVariableSpeedCoils->VarSpeedCoil(HPWH.DXCoilNum).VarSpeedCoilType;
            }
        } else {
            // this is a single speed coil
            DXCoils::DXCoilData &Coil = state.dataDXCoils->DXCoil(HPWH.DXCoilNum);
            if (!UtilityRoutines::SameString(HPWH.DXCoilType, Coil.DXCoilType)) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                ShowContinueError(state, "specifies the coil " + HPWH.DXCoilType + "=\"" + HPWH.DXCoilName + "\".");
                ShowContinueError(state, "However, " + HPWH.DXCoilName + " is a coil of type " + Coil.DXCoilType + ".");
                ErrorsFound = true;
            }
            HPWH.DXCoilTypeNum = Coil.DXCoilType_Num;
        }

        // Make sure that the coil and tank are compatible.
        if (bIsVScoil) {
            if (HPWH.TypeNum != DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state,
                                  "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed can only be used with a pumped condenser heat pump "
                                  "water heater.");
                ErrorsFound = true;
            }
        } else {
            if (!((HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterPumped &&
                   HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) ||
                  (HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped &&
                   HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped))) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                std::string ExpectedCoilType;
                if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                    ExpectedCoilType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatPumpWaterHeaterPumped);
                } else if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                    ExpectedCoilType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped);
                } else {
                    assert(0);
                }
                ShowContinueError(state, "can only be used with " + ExpectedCoilType);
                ErrorsFound = true;
            }
        }

        // Dummy condenser Inlet/Outlet Nodes for wrapped tanks
        if (HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped) {
            DXCoils::DXCoilData &Coil = state.dataDXCoils->DXCoil(HPWH.DXCoilNum);

            HPWH.InletNodeName1 = "DUMMY CONDENSER INLET " + Coil.Name;
            HPWH.CondWaterInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                          HPWH.InletNodeName1,
                                                                          ErrorsFound,
                                                                          state.dataIPShortCut->cCurrentModuleObject,
                                                                          HPWH.Name,
                                                                          DataLoopNode::NodeFluidType::Water,
                                                                          DataLoopNode::NodeConnectionType::Inlet,
                                                                          2,
                                                                          DataLoopNode::ObjectIsParent);
            HPWH.OutletNodeName1 = "DUMMY CONDENSER OUTLET " + Coil.Name;
            HPWH.CondWaterOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                           HPWH.OutletNodeName1,
                                                                           ErrorsFound,
                                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                                           HPWH.Name,
                                                                           DataLoopNode::NodeFluidType::Water,
                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                           2,
                                                                           DataLoopNode::ObjectIsParent);
        }

        // Minimum Inlet Air Temperature for Compressor Operation
        HPWH.MinAirTempForHPOperation = hpwhNumeric[4 + nNumericOffset];
        if (HPWH.MinAirTempForHPOperation < -5) {
            ShowWarningError(state,
                             state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name +
                                 "\": minimum inlet air temperature for heat pump compressor operation must be greater than or equal to -5 C.");
            ShowContinueError(state, format("...Minimum inlet air temperature = {:.1T}", hpwhNumeric[4 + nNumericOffset]));
        }

        // Maximum Inlet Air Temperature for Compressor Operation
        HPWH.MaxAirTempForHPOperation = hpwhNumeric[5 + nNumericOffset];
        if (HPWH.MaxAirTempForHPOperation <= HPWH.MinAirTempForHPOperation) {
            ShowWarningError(state,
                             state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name +
                                 "\": maximum inlet air temperature for heat pump compressor operation");
            ShowContinueError(state, "must be greater than the minimum inlet air temperature for heat pump compressor operation.");
            ShowContinueError(state, format("...Minimum inlet air temperature = {:.1T}", HPWH.MinAirTempForHPOperation));
            ShowContinueError(state, format("...Maximum inlet air temperature = {:.1T}", HPWH.MaxAirTempForHPOperation));
        }

        // Compressor Location
        {
            auto const SELECT_CASE_var(hpwhAlpha[20 + nAlphaOffset]);
            if (SELECT_CASE_var == "SCHEDULE") {
                HPWH.CrankcaseTempIndicator = CrankTempEnum::Schedule;
                if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                    // Compressor Ambient Temperature Schedule
                    HPWH.CrankcaseTempSchedule = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[21 + nAlphaOffset]);
                    if (HPWH.CrankcaseTempSchedule == 0) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                        ShowContinueError(state, hpwhAlphaFieldNames[21 + nAlphaOffset] + "=\"" + hpwhAlpha[21 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                    ShowContinueError(state, "required " + hpwhAlphaFieldNames[21 + nAlphaOffset] + " is blank.");
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "ZONE") {
                HPWH.CrankcaseTempIndicator = CrankTempEnum::Zone;
                if (HPWH.InletAirConfiguration == AmbientTempEnum::OutsideAir || HPWH.InletAirConfiguration == AmbientTempEnum::Schedule) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name +
                                        "\":  Inlet Air Configuration must be Zone Air Only or Zone And");
                    ShowContinueError(state, " Outdoor Air when compressor location equals ZONE.");
                    ErrorsFound = true;
                }

                if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                    ShowWarningError(
                        state,
                        state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\"  " + hpwhAlphaFieldNames[21 + nAlphaOffset] +
                            " was provided but will not be used based on compressor location input=\"" + hpwhAlpha[20 + nAlphaOffset] + "\".");
                }
            } else if (SELECT_CASE_var == "OUTDOORS") {
                HPWH.CrankcaseTempIndicator = CrankTempEnum::Exterior;
                if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\"  " +
                                         hpwhAlphaFieldNames[21 + nAlphaOffset] + " was provided but will not be used based on " +
                                         hpwhAlphaFieldNames[21 + nAlphaOffset] + "=\"" + hpwhAlpha[20 + nAlphaOffset] + "\".");
                }
            }
        }

        // Fan Name
        HPWH.FanName = hpwhAlpha[23 + nAlphaOffset];
        HPWH.FanType = hpwhAlpha[22 + nAlphaOffset];

        // check that the fan exists
        bool errFlag = false;
        ValidateComponent(state, HPWH.FanType, HPWH.FanName, errFlag, state.dataIPShortCut->cCurrentModuleObject);

        Real64 FanVolFlow = 0.0;
        if (errFlag) {
            ShowContinueError(state, "...occurs in " + state.dataIPShortCut->cCurrentModuleObject + ", unit=\"" + HPWH.Name + "\".");
            ErrorsFound = true;
        } else {
            if (UtilityRoutines::SameString(HPWH.FanType, "Fan:SystemModel")) {
                HPWH.FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, HPWH.FanName)); // call constructor
                HPWH.FanNum = HVACFan::getFanObjectVectorIndex(state, HPWH.FanName);
                FanVolFlow = state.dataHVACFan->fanObjs[HPWH.FanNum]->designAirVolFlowRate;

            } else {
                Fans::GetFanType(state, HPWH.FanName, HPWH.FanType_Num, errFlag, state.dataIPShortCut->cCurrentModuleObject, HPWH.Name);
                Fans::GetFanIndex(state, HPWH.FanName, HPWH.FanNum, errFlag, state.dataIPShortCut->cCurrentModuleObject);
                Fans::GetFanVolFlow(state, HPWH.FanNum, FanVolFlow);
            }
        }
        // issue #5630, set fan info in coils.
        if (bIsVScoil) {
            VariableSpeedCoils::setVarSpeedHPWHFanTypeNum(state, HPWH.DXCoilNum, HPWH.FanType_Num);
            VariableSpeedCoils::setVarSpeedHPWHFanIndex(state, HPWH.DXCoilNum, HPWH.FanNum);
        } else {
            DXCoils::SetDXCoolingCoilData(state, HPWH.DXCoilNum, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HPWH.FanName);
            DXCoils::SetDXCoolingCoilData(state, HPWH.DXCoilNum, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HPWH.FanNum);
            DXCoils::SetDXCoolingCoilData(
                state, HPWH.DXCoilNum, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HPWH.FanType_Num);
        }

        if (errFlag) {
            ErrorsFound = true;
        } else if (HPWH.FanType_Num != DataHVACGlobals::FanType_SimpleOnOff && HPWH.FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\": illegal fan type specified.");
            ShowContinueError(
                state, " The fan object (" + HPWH.FanName + ") type must be Fan:SystemModel or Fan:OnOff when used with a heat pump water heater.");
            ErrorsFound = true;
        } else if (!UtilityRoutines::SameString(HPWH.FanType, "Fan:OnOff") && !UtilityRoutines::SameString(HPWH.FanType, "Fan:SystemModel")) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\": illegal fan type specified.");
            ShowContinueError(state, " The " + state.dataIPShortCut->cCurrentModuleObject + " must specify that the fan object");
            ShowContinueError(state,
                              " is of type FanSystemModel or Fan:OnOff in addition to the fan actually being of that type and defined elsewhere.");
        }

        if (FanVolFlow != DataSizing::AutoSize && !errFlag) {
            if (FanVolFlow < HPWH.OperatingAirFlowRate) {
                ShowSevereError(state,
                                format("{} - air flow rate = {:.7T} in fan object {} is less than the  HPWHs evaporator air flow rate.",
                                       state.dataIPShortCut->cCurrentModuleObject,
                                       FanVolFlow,
                                       HPWH.FanName));
                ShowContinueError(state, " The fan flow rate must be >= to the HPWHs evaporator volumetric air flow rate.");
                ShowContinueError(state, " Occurs in unit = " + HPWH.Name);
                ErrorsFound = true;
            }
        }

        // Fan Placement
        if (UtilityRoutines::SameString(hpwhAlpha[24 + nAlphaOffset], "BlowThrough")) {
            HPWH.FanPlacement = DataHVACGlobals::BlowThru;

        } else if (UtilityRoutines::SameString(hpwhAlpha[24 + nAlphaOffset], "DrawThrough")) {
            HPWH.FanPlacement = DataHVACGlobals::DrawThru;

        } else {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", invalid ");
            ShowContinueError(state, hpwhAlphaFieldNames[24 + nAlphaOffset] + "=\"" + hpwhAlpha[24 + nAlphaOffset] + "\".");
            ErrorsFound = true;
        }

        if (HPWH.DXCoilNum > 0 && !bIsVScoil) {
            // get HPWH capacity, air inlet node, and PLF curve info from DX coil object
            HPWH.Capacity = state.dataDXCoils->DXCoil(HPWH.DXCoilNum).RatedTotCap2;
            HPWH.DXCoilAirInletNode = state.dataDXCoils->DXCoil(HPWH.DXCoilNum).AirInNode;
            HPWH.DXCoilPLFFPLR = state.dataDXCoils->DXCoil(HPWH.DXCoilNum).PLFFPLR(1);
            // check the range of condenser pump power to be <= 5 gpm/ton
            if (state.dataDXCoils->DXCoil(HPWH.DXCoilNum).HPWHCondPumpElecNomPower / state.dataDXCoils->DXCoil(HPWH.DXCoilNum).RatedTotCap2 >
                0.1422) {
                ShowWarningError(
                    state,
                    state.dataDXCoils->DXCoil(HPWH.DXCoilNum).DXCoilType + "= " + state.dataDXCoils->DXCoil(HPWH.DXCoilNum).Name +
                        format(": Rated condenser pump power per watt of rated heating capacity has exceeded the recommended maximum of 0.1422 "
                               "W/W (41.67 watt/MBH). Condenser pump power per watt = {:.4T}",
                               (state.dataDXCoils->DXCoil(HPWH.DXCoilNum).HPWHCondPumpElecNomPower /
                                state.dataDXCoils->DXCoil(HPWH.DXCoilNum).RatedTotCap2)));
            }
        } else if ((HPWH.DXCoilNum > 0) && (bIsVScoil)) {

            if (HPWH.bIsIHP) {
                HPWH.Capacity = GetDWHCoilCapacityIHP(
                    state, HPWH.DXCoilType, HPWH.DXCoilName, IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode, DXCoilErrFlag);
                HPWH.DXCoilAirInletNode = IntegratedHeatPump::GetCoilInletNodeIHP(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                HPWH.DXCoilPLFFPLR = GetIHPDWHCoilPLFFPLR(
                    state, HPWH.DXCoilType, HPWH.DXCoilName, IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode, DXCoilErrFlag);
            } else {
                HPWH.Capacity = VariableSpeedCoils::GetCoilCapacityVariableSpeed(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                HPWH.DXCoilAirInletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                HPWH.DXCoilPLFFPLR = VariableSpeedCoils::GetVSCoilPLFFPLR(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
            }
            //         check the range of condenser pump power to be <= 5 gpm/ton, will be checked in the coil object
        }

        if (HPWH.OperatingWaterFlowRate == DataGlobalConstants::AutoCalculate) {
            HPWH.OperatingWaterFlowRate = 0.00000004487 * HPWH.Capacity;
            HPWH.WaterFlowRateAutoSized = true;
        }

        if (HPWH.OperatingAirFlowRate == DataGlobalConstants::AutoCalculate) {
            HPWH.OperatingAirFlowRate = 0.00005035 * HPWH.Capacity;
            HPWH.AirFlowRateAutoSized = true;
        }

        // On Cycle Parasitic Electric Load
        HPWH.OnCycParaLoad = hpwhNumeric[6 + nNumericOffset];
        if (HPWH.OnCycParaLoad < 0.0) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
            ShowContinueError(state,
                              hpwhNumericFieldNames[6 + nNumericOffset] + " must be >= 0. " + hpwhNumericFieldNames[6 + nNumericOffset] +
                                  format(" = {:.2T}", hpwhNumeric[6 + nNumericOffset]));
            ErrorsFound = true;
        }

        // Off Cycle Parasitic Electric Load
        HPWH.OffCycParaLoad = hpwhNumeric[7 + nNumericOffset];
        if (HPWH.OffCycParaLoad < 0.0) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
            ShowContinueError(state,
                              hpwhNumericFieldNames[7 + nNumericOffset] + " must be >= 0. " + hpwhNumericFieldNames[2 + nNumericOffset] +
                                  format(" = {:.2T}", hpwhNumeric[7 + nNumericOffset]));
            ErrorsFound = true;
        }

        // Parasitic Heat Rejection Location
        if (UtilityRoutines::SameString(hpwhAlpha[25 + nAlphaOffset], "Zone")) {
            HPWH.ParasiticTempIndicator = AmbientTempEnum::TempZone;
            if (HPWH.InletAirConfiguration == AmbientTempEnum::OutsideAir || HPWH.InletAirConfiguration == AmbientTempEnum::Schedule) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                ShowContinueError(state, hpwhAlphaFieldNames[25 + nAlphaOffset] + " must be ZoneAirOnly or ZoneAndOutdoorAir");
                ShowContinueError(state, " when parasitic heat rejection location equals Zone.");
                ErrorsFound = true;
            }
        } else if (UtilityRoutines::SameString(hpwhAlpha[25 + nAlphaOffset], "Outdoors")) {
            HPWH.ParasiticTempIndicator = AmbientTempEnum::OutsideAir;
        } else {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
            ShowContinueError(state, " parasitic heat rejection location must be either Zone or Outdoors.");
            ErrorsFound = true;
        }

        // Inlet Air Mixer Node
        // get mixer/splitter nodes only when Inlet Air Configuration is ZoneAndOutdoorAir
        if (!hpwhAlphaBlank[26 + nAlphaOffset]) {
            // For the inlet air mixer node, NodeConnectionType is outlet from the HPWH inlet air node
            if (HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
                HPWH.InletAirMixerNode = NodeInputManager::GetOnlySingleNode(state,
                                                                             hpwhAlpha[26 + nAlphaOffset],
                                                                             ErrorsFound,
                                                                             state.dataIPShortCut->cCurrentModuleObject + " inlet air mixer",
                                                                             HPWH.Name,
                                                                             DataLoopNode::NodeFluidType::Air,
                                                                             DataLoopNode::NodeConnectionType::Outlet,
                                                                             1,
                                                                             DataLoopNode::ObjectIsNotParent);
            } else {
                ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state,
                                  "Inlet air mixer node name specified but only required when Inlet Air Configuration is selected as "
                                  "Zone and OutdoorAir. Node name disregarded and simulation continues.");
            }
        } else if (hpwhAlphaBlank[26 + nAlphaOffset] && HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
            ShowContinueError(state, "Inlet air mixer node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir.");
            ErrorsFound = true;
        }

        // Outlet Air Splitter Node
        if (!hpwhAlphaBlank[27 + nAlphaOffset]) {
            //  For the outlet air splitter node, NodeConnectionType is inlet to the HPWH outlet air node
            if (HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
                HPWH.OutletAirSplitterNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                 hpwhAlpha[27 + nAlphaOffset],
                                                                                 ErrorsFound,
                                                                                 state.dataIPShortCut->cCurrentModuleObject + "-OUTLET AIR SPLITTER",
                                                                                 HPWH.Name,
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::NodeConnectionType::Inlet,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsNotParent);
            } else {
                ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state,
                                  "Outlet air splitter node name specified but only required when Inlet Air Configuration is selected as "
                                  "ZoneAndOutdoorAir. Node name disregarded and simulation continues.");
            }
        } else if (hpwhAlphaBlank[27 + nAlphaOffset] && HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
            ShowContinueError(state, "Outlet air splitter node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir.");
            ErrorsFound = true;
        }

        // get node data for HPWH
        if (HPWH.InletAirMixerNode != 0) {
            // when mixer/splitter nodes are used the HPWH's inlet/outlet node are set up as DataLoopNode::ObjectIsNotParent

            HPWH.HeatPumpAirInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                            hpwhAlpha[7 + nAlphaOffset],
                                                                            ErrorsFound,
                                                                            state.dataIPShortCut->cCurrentModuleObject + "-INLET AIR MIXER",
                                                                            HPWH.Name,
                                                                            DataLoopNode::NodeFluidType::Air,
                                                                            DataLoopNode::NodeConnectionType::Inlet,
                                                                            1,
                                                                            DataLoopNode::ObjectIsNotParent);

            HPWH.HeatPumpAirOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                             hpwhAlpha[8 + nAlphaOffset],
                                                                             ErrorsFound,
                                                                             state.dataIPShortCut->cCurrentModuleObject + "-OUTLET AIR SPLITTER",
                                                                             HPWH.Name,
                                                                             DataLoopNode::NodeFluidType::Air,
                                                                             DataLoopNode::NodeConnectionType::Outlet,
                                                                             1,
                                                                             DataLoopNode::ObjectIsNotParent);

            HPWH.OutsideAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                      hpwhAlpha[9 + nAlphaOffset],
                                                                      ErrorsFound,
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      HPWH.Name,
                                                                      DataLoopNode::NodeFluidType::Air,
                                                                      DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                      1,
                                                                      DataLoopNode::ObjectIsParent);
            if (!hpwhAlpha[9 + nAlphaOffset].empty()) {
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(state, HPWH.OutsideAirNode, Okay);
                if (!Okay) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name +
                                         "\": Adding outdoor air node=" + hpwhAlpha[9 + nAlphaOffset]);
                }
            }

            HPWH.ExhaustAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                      hpwhAlpha[10 + nAlphaOffset],
                                                                      ErrorsFound,
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      HPWH.Name,
                                                                      DataLoopNode::NodeFluidType::Air,
                                                                      DataLoopNode::NodeConnectionType::ReliefAir,
                                                                      1,
                                                                      DataLoopNode::ObjectIsParent);

        } else {
            // when mixer/splitter nodes are NOT used the HPWH's inlet/outlet nodes are set up as DataLoopNode::ObjectIsParent
            if (HPWH.InletAirConfiguration == AmbientTempEnum::Schedule) {
                // for scheduled HPWH's the inlet node is not on any branch or parent object, make it an outlet node
                // to avoid node connection errors
                HPWH.HeatPumpAirInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                hpwhAlpha[7 + nAlphaOffset],
                                                                                ErrorsFound,
                                                                                state.dataIPShortCut->cCurrentModuleObject,
                                                                                HPWH.Name,
                                                                                DataLoopNode::NodeFluidType::Air,
                                                                                DataLoopNode::NodeConnectionType::Outlet,
                                                                                1,
                                                                                DataLoopNode::ObjectIsParent);

                HPWH.HeatPumpAirOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                 hpwhAlpha[8 + nAlphaOffset],
                                                                                 ErrorsFound,
                                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                                 HPWH.Name,
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::NodeConnectionType::Outlet,
                                                                                 1,
                                                                                 DataLoopNode::ObjectIsParent);

            } else { // HPWH is connected to a zone with no mixer/splitter nodes
                if (HPWH.InletAirConfiguration == AmbientTempEnum::TempZone) {
                    HPWH.HeatPumpAirInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                    hpwhAlpha[7 + nAlphaOffset],
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    HPWH.Name,
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                    1,
                                                                                    DataLoopNode::ObjectIsParent);

                    HPWH.HeatPumpAirOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                     hpwhAlpha[8 + nAlphaOffset],
                                                                                     ErrorsFound,
                                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                                     HPWH.Name,
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                                     1,
                                                                                     DataLoopNode::ObjectIsParent);
                } else { // HPWH is located outdoors
                    HPWH.OutsideAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                              hpwhAlpha[9 + nAlphaOffset],
                                                                              ErrorsFound,
                                                                              state.dataIPShortCut->cCurrentModuleObject,
                                                                              HPWH.Name,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                              1,
                                                                              DataLoopNode::ObjectIsParent);
                    if (!hpwhAlphaBlank[9 + nAlphaOffset]) {
                        bool Okay;
                        OutAirNodeManager::CheckAndAddAirNodeNumber(state, HPWH.OutsideAirNode, Okay);
                        if (!Okay) {
                            ShowWarningError(state,
                                             state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name +
                                                 "\": Adding outdoor air node =" + hpwhAlpha[9 + nAlphaOffset]);
                        }
                    }

                    HPWH.ExhaustAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                              hpwhAlpha[10 + nAlphaOffset],
                                                                              ErrorsFound,
                                                                              state.dataIPShortCut->cCurrentModuleObject,
                                                                              HPWH.Name,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::ReliefAir,
                                                                              1,
                                                                              DataLoopNode::ObjectIsParent);
                }
            }
        }
        // check that required node names are present
        if (HPWH.InletAirConfiguration == AmbientTempEnum::Schedule || HPWH.InletAirConfiguration == AmbientTempEnum::TempZone) {
            if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state, "When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                ShowContinueError(state,
                                  hpwhAlphaFieldNames[7 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[8 + nAlphaOffset] + " must be specified.");
                ErrorsFound = true;
            }
        } else if (HPWH.InletAirConfiguration == AmbientTempEnum::OutsideAir) {
            if (HPWH.OutsideAirNode == 0 || HPWH.ExhaustAirNode == 0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state, "When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                ShowContinueError(state,
                                  hpwhAlphaFieldNames[9 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[10 + nAlphaOffset] + " must be specified.");
                ErrorsFound = true;
            }
        } else if (HPWH.InletAirMixerNode > 0 && HPWH.OutletAirSplitterNode > 0 && HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
            if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0 || HPWH.OutsideAirNode == 0 || HPWH.ExhaustAirNode == 0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state, "When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0) {
                    ShowContinueError(
                        state, hpwhAlphaFieldNames[7 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[8 + nAlphaOffset] + " must be specified.");
                }
                if (HPWH.OutsideAirNode == 0 || HPWH.ExhaustAirNode == 0) {
                    ShowContinueError(
                        state, hpwhAlphaFieldNames[9 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[10 + nAlphaOffset] + " must be specified.");
                }
                ErrorsFound = true;
            }
        }

        // check that the HPWH inlet and outlet nodes are in the same zone (ZoneHVAC:EquipmentConnections) when
        // Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
        if ((HPWH.InletAirConfiguration == AmbientTempEnum::TempZone || HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) &&
            HPWH.AmbientTempZone > 0) {
            if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
                DataZoneEquipment::GetZoneEquipmentData(state);
                state.dataZoneEquip->ZoneEquipInputsFilled = true;
            }
            if (allocated(state.dataZoneEquip->ZoneEquipConfig)) {
                bool FoundInletNode = false;
                bool FoundOutletNode = false;
                int ZoneNum;
                for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                    if (HPWH.AmbientTempZone == state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ActualZoneNum) break;
                }
                if (ZoneNum <= state.dataGlobal->NumOfZones) {
                    for (int SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes; ++SupAirIn) {
                        if (HPWH.HeatPumpAirOutletNode != state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InletNode(SupAirIn)) continue;
                        FoundOutletNode = true;
                    }
                    for (int ExhAirOut = 1; ExhAirOut <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumExhaustNodes; ++ExhAirOut) {
                        if (HPWH.HeatPumpAirInletNode != state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ExhaustNode(ExhAirOut)) continue;
                        FoundInletNode = true;
                    }
                    if (!FoundInletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError(state, "The HPWH's air inlet node name = " + hpwhAlpha[7 + nAlphaOffset] + " was not properly specified ");
                        ShowContinueError(state,
                                          "as an exhaust air node for zone = " + hpwhAlpha[13 + nAlphaOffset] +
                                              " in a ZoneHVAC:EquipmentConnections object.");
                        ErrorsFound = true;
                    }
                    if (!FoundOutletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError(state, "The HPWH's air outlet node name = " + hpwhAlpha[8 + nAlphaOffset] + " was not properly specified ");
                        ShowContinueError(
                            state, "as an inlet air node for zone = " + hpwhAlpha[13 + nAlphaOffset] + " in a ZoneHVAC:EquipmentConnections object.");
                        ErrorsFound = true;
                    }
                }
            } else {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state,
                                  "Heat pump water heater air inlet node name and air outlet node name must be listed in a "
                                  "ZoneHVAC:EquipmentConnections object when Inlet Air Configuration is equal to ZoneAirOnly or "
                                  "ZoneAndOutdoorAir.");
                ErrorsFound = true;
            }
        }

        // only get the inlet air mixer schedule if the inlet air configuration is zone and outdoor air
        if (!hpwhAlphaBlank[28 + nAlphaOffset] && HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
            HPWH.InletAirMixerSchPtr = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[28 + nAlphaOffset]);
            if (HPWH.InletAirMixerSchPtr == 0) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                ShowContinueError(state, hpwhAlphaFieldNames[28 + nAlphaOffset] + "=\"" + hpwhAlpha[28 + nAlphaOffset] + "\",");
                ErrorsFound = true;
            } else {
                bool ValidScheduleValue = ScheduleManager::CheckScheduleValueMinMax(state, HPWH.InletAirMixerSchPtr, ">=", 0.0, "<=", 1.0);
                if (!ValidScheduleValue) {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                    ShowContinueError(state,
                                      hpwhAlphaFieldNames[28 + nAlphaOffset] + " values out of range of 0 to 1, Schedule=\"" +
                                          hpwhAlpha[28 + nAlphaOffset] + "\".");
                    ErrorsFound = true;
                }
                //           set outlet air splitter schedule index equal to inlet air mixer schedule index
                //           (place holder for when zone pressurization/depressurization is allowed and different schedules can be used)
                HPWH.OutletAirSplitterSchPtr = ScheduleManager::GetScheduleIndex(state, hpwhAlpha[28 + nAlphaOffset]);
            }
        }

        // set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
        if (HPWH.FanPlacement == DataHVACGlobals::DrawThru) {
            if (HPWH.OutletAirSplitterNode != 0) {
                HPWH.FanOutletNode = HPWH.OutletAirSplitterNode;
            } else {
                if (HPWH.InletAirConfiguration == AmbientTempEnum::OutsideAir) {
                    HPWH.FanOutletNode = HPWH.ExhaustAirNode;
                } else {
                    HPWH.FanOutletNode = HPWH.HeatPumpAirOutletNode;
                }
            }
        } else if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
            // set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
            if (bIsVScoil) {
                if (HPWH.bIsIHP) {
                    HPWH.FanOutletNode = IntegratedHeatPump::GetDWHCoilInletNodeIHP(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                } else {
                    HPWH.FanOutletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                }
            } else {
                HPWH.FanOutletNode = state.dataDXCoils->DXCoil(HPWH.DXCoilNum).AirInNode;
            }
        }

        // check that fan outlet node is indeed correct
        int FanOutletNodeNum(0);
        if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            FanOutletNodeNum = state.dataHVACFan->fanObjs[HPWH.FanNum]->outletNodeNum;
        } else {
            errFlag = false;
            FanOutletNodeNum = Fans::GetFanOutletNode(state, HPWH.FanType, HPWH.FanName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in unit=\"" + HPWH.Name + "\".");
                ErrorsFound = true;
            }
        }
        if (FanOutletNodeNum != HPWH.FanOutletNode) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
            ShowContinueError(state, "Heat pump water heater fan outlet node name does not match next connected component.");
            if (FanOutletNodeNum != 0) {
                ShowContinueError(state, "Fan outlet node name = " + state.dataLoopNodes->NodeID(FanOutletNodeNum));
            }
            if (HPWH.FanOutletNode != 0) {
                ShowContinueError(state, "Expected fan outlet node name = " + state.dataLoopNodes->NodeID(HPWH.FanOutletNode));
            }
            ErrorsFound = true;
        }
        int FanInletNodeNum(0);
        if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            FanInletNodeNum = state.dataHVACFan->fanObjs[HPWH.FanNum]->inletNodeNum;
        } else {
            errFlag = false;
            FanInletNodeNum = Fans::GetFanInletNode(state, HPWH.FanType, HPWH.FanName, errFlag);
            if (errFlag) {
                ShowContinueError(state, "...occurs in unit=\"" + HPWH.Name + "\".");
                ErrorsFound = true;
            }
        }
        int HPWHFanInletNodeNum(0);
        if (HPWH.InletAirMixerNode != 0) {
            HPWHFanInletNodeNum = HPWH.InletAirMixerNode;
        } else {
            if (HPWH.InletAirConfiguration == AmbientTempEnum::OutsideAir) {
                HPWHFanInletNodeNum = HPWH.OutsideAirNode;
            } else {
                HPWHFanInletNodeNum = HPWH.HeatPumpAirInletNode;
            }
        }
        if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
            if (FanInletNodeNum != HPWHFanInletNodeNum) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state, "Heat pump water heater fan inlet node name does not match previous connected component.");
                if (FanOutletNodeNum != 0) {
                    ShowContinueError(state, "Fan inlet node name = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                }
                if (HPWH.FanOutletNode != 0) {
                    ShowContinueError(state, "Expected fan inlet node name = " + state.dataLoopNodes->NodeID(HPWHFanInletNodeNum));
                }
                ErrorsFound = true;
            }
        }

        int DXCoilAirOutletNodeNum(0);
        if ((HPWH.DXCoilNum > 0) && (bIsVScoil)) {
            if (HPWH.bIsIHP) {
                DXCoilAirOutletNodeNum = IntegratedHeatPump::GetDWHCoilOutletNodeIHP(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
            } else {
                DXCoilAirOutletNodeNum = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(state, HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
            }

        } else if (HPWH.DXCoilNum > 0) {
            DXCoilAirOutletNodeNum = state.dataDXCoils->DXCoil(HPWH.DXCoilNum).AirOutNode;
        }
        if (HPWH.FanPlacement == DataHVACGlobals::DrawThru) {
            if (FanInletNodeNum != DXCoilAirOutletNodeNum) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state, "Heat pump water heater fan inlet node name does not match previous connected component.");
                if (FanInletNodeNum != 0) {
                    ShowContinueError(state, "Fan inlet node name = " + state.dataLoopNodes->NodeID(FanInletNodeNum));
                }
                if (DXCoilAirOutletNodeNum != 0) {
                    ShowContinueError(state, "Expected fan inlet node name = " + state.dataLoopNodes->NodeID(DXCoilAirOutletNodeNum));
                }
                ErrorsFound = true;
            }
        } else if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
            int HPWHCoilOutletNodeNum(0);
            if (HPWH.OutletAirSplitterNode != 0) {
                HPWHCoilOutletNodeNum = HPWH.OutletAirSplitterNode;
            } else {
                if (HPWH.InletAirConfiguration == AmbientTempEnum::OutsideAir) {
                    HPWHCoilOutletNodeNum = HPWH.ExhaustAirNode;
                } else {
                    HPWHCoilOutletNodeNum = HPWH.HeatPumpAirOutletNode;
                }
            }
            if (DXCoilAirOutletNodeNum != HPWHCoilOutletNodeNum) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                ShowContinueError(state, "Heat pump water heater coil outlet node name does not match next connected component.");
                if (DXCoilAirOutletNodeNum != 0) {
                    ShowContinueError(state, "Coil outlet node name = " + state.dataLoopNodes->NodeID(DXCoilAirOutletNodeNum));
                }
                if (HPWHCoilOutletNodeNum != 0) {
                    ShowContinueError(state, "Expected coil outlet node name = " + state.dataLoopNodes->NodeID(HPWHCoilOutletNodeNum));
                }
                ErrorsFound = true;
            }
        }

        // set the max mass flow rate for outdoor fans
        if (HPWH.FanOutletNode > 0)
            state.dataLoopNodes->Node(HPWH.FanOutletNode).MassFlowRateMax =
                HPWH.OperatingAirFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, 20.0, 0.0);

        if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
            if (HPWH.InletAirMixerNode > 0) {
                HPWH.FanInletNode_str = hpwhAlpha[26 + nAlphaOffset];
                HPWH.FanOutletNode_str = "UNDEFINED";
            } else {
                if (HPWH.OutsideAirNode == 0) {
                    HPWH.FanInletNode_str = hpwhAlpha[7 + nAlphaOffset];
                    HPWH.FanOutletNode_str = "UNDEFINED";
                } else {
                    HPWH.FanInletNode_str = hpwhAlpha[9 + nAlphaOffset];
                    HPWH.FanOutletNode_str = "UNDEFINED";
                }
            }
            if (HPWH.OutletAirSplitterNode > 0) {
                HPWH.CoilInletNode_str = "UNDEFINED";
                HPWH.CoilOutletNode_str = hpwhAlpha[27 + nAlphaOffset];
            } else {
                if (HPWH.OutsideAirNode == 0) {
                    HPWH.CoilInletNode_str = "UNDEFINED";
                    HPWH.CoilOutletNode_str = hpwhAlpha[8 + nAlphaOffset];
                } else {
                    HPWH.CoilInletNode_str = "UNDEFINED";
                    HPWH.CoilOutletNode_str = hpwhAlpha[10 + nAlphaOffset];
                }
            }
        } else {
            if (HPWH.InletAirMixerNode > 0) {
                HPWH.CoilInletNode_str = hpwhAlpha[26 + nAlphaOffset];
                HPWH.CoilOutletNode_str = "UNDEFINED";
            } else {
                if (HPWH.OutsideAirNode == 0) {
                    HPWH.CoilInletNode_str = hpwhAlpha[7 + nAlphaOffset];
                    HPWH.CoilOutletNode_str = "UNDEFINED";
                } else {
                    HPWH.CoilInletNode_str = hpwhAlpha[9 + nAlphaOffset];
                    HPWH.CoilOutletNode_str = "UNDEFINED";
                }
            }
            if (HPWH.OutletAirSplitterNode > 0) {
                HPWH.FanInletNode_str = "UNDEFINED";
                HPWH.FanOutletNode_str = hpwhAlpha[27 + nAlphaOffset];
            } else {
                if (HPWH.OutsideAirNode == 0) {
                    HPWH.FanInletNode_str = "UNDEFINED";
                    HPWH.FanOutletNode_str = hpwhAlpha[8 + nAlphaOffset];
                } else {
                    HPWH.FanInletNode_str = "UNDEFINED";
                    HPWH.FanOutletNode_str = hpwhAlpha[10 + nAlphaOffset];
                }
            }
        }

        // set up comp set for air side nodes (can be blow thru or draw thru, may or may not have damper nodes)
        if (HPWH.bIsIHP) {
            BranchNodeConnections::SetUpCompSets(
                state, HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName + " Outdoor Coil", HPWH.CoilInletNode_str, HPWH.CoilOutletNode_str);
        } else {
            BranchNodeConnections::SetUpCompSets(
                state, HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName, HPWH.CoilInletNode_str, HPWH.CoilOutletNode_str);
        }

        BranchNodeConnections::SetUpCompSets(state, HPWH.Type, HPWH.Name, HPWH.FanType, HPWH.FanName, HPWH.FanInletNode_str, HPWH.FanOutletNode_str);

        // Control Logic Flag
        std::string CtrlLogicFlag = hpwhAlphaBlank[29 + nAlphaOffset] ? "SIMULTANEOUS" : hpwhAlpha[29 + nAlphaOffset];
        if (UtilityRoutines::SameString(CtrlLogicFlag, "SIMULTANEOUS")) {
            HPWH.AllowHeatingElementAndHeatPumpToRunAtSameTime = true;
        } else if (UtilityRoutines::SameString(CtrlLogicFlag, "MUTUALLYEXCLUSIVE")) {
            HPWH.AllowHeatingElementAndHeatPumpToRunAtSameTime = false;
        } else {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
            ShowContinueError(state, CtrlLogicFlag + " is not a valid value for field Tank Element Control Logic.");
            ErrorsFound = true;
        }

        // Control Sensor 1 Location In Stratified Tank
        if (!hpwhNumericBlank[8 + nNumericOffset]) {
            HPWH.ControlSensor1Height = hpwhNumeric[8 + nNumericOffset];
        } else {
            // use heater1 location, which we don't know right now
            HPWH.ControlSensor1Height = -1.0;
        }

        // Control Sensor 1 Weight
        HPWH.ControlSensor1Weight = hpwhNumericBlank[9 + nNumericOffset] ? 1.0 : hpwhNumeric[9 + nNumericOffset];

        // Control Sensor 2 Location In Stratified Tank
        if (!hpwhNumericBlank[10 + nNumericOffset]) {
            HPWH.ControlSensor2Height = hpwhNumeric[10 + nNumericOffset];
        } else {
            HPWH.ControlSensor2Height = -1.0;
        }

        // Control Sensor 2 Weight
        HPWH.ControlSensor2Weight = 1.0 - HPWH.ControlSensor1Weight;
    }

    return ErrorsFound;
}

bool getWaterHeaterMixedInputs(EnergyPlusData &state)
{
    bool ErrorsFound = false;
    state.dataIPShortCut->cCurrentModuleObject = cMixedWHModuleObj;
    static constexpr std::string_view RoutineName = "getWaterHeaterMixedInputs";

    for (int WaterThermalTankNum = 1; WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterHeaterMixed; ++WaterThermalTankNum) {
        int NumAlphas;
        int NumNums;
        int IOStat;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 WaterThermalTankNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataWaterThermalTanks->UniqueWaterThermalTankNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);

        auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum);

        Tank.Name = state.dataIPShortCut->cAlphaArgs(1);
        Tank.Type = state.dataIPShortCut->cCurrentModuleObject;
        Tank.TypeNum = DataPlant::TypeOf_WtrHeaterMixed;
        Tank.FluidIndex = Tank.waterIndex;

        // default to always on
        Tank.SourceSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        Tank.UseSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;

        // A user field will be added in a later release
        Tank.EndUseSubcategoryName = "Water Heater";

        Tank.Volume = state.dataIPShortCut->rNumericArgs(1);
        if (Tank.Volume == DataSizing::AutoSize) {
            Tank.VolumeWasAutoSized = true;
        }
        if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
            // Set volume to a really small number to simulate a tankless/instantaneous water heater
            Tank.Volume = 0.000001; // = 1 cm3
        }

        Tank.SetPointTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            ShowSevereError(
                state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", missing data.");
            ShowContinueError(state, "blank field, missing " + state.dataIPShortCut->cAlphaFieldNames(2) + " is required");
            ErrorsFound = true;
        } else if (Tank.SetPointTempSchedule == 0) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) + ":  " +
                                state.dataIPShortCut->cAlphaFieldNames(2) + " not found = " + state.dataIPShortCut->cAlphaArgs(2));
            ErrorsFound = true;
        }

        if (state.dataIPShortCut->rNumericArgs(2) > 0.0001) {
            Tank.DeadBandDeltaTemp = state.dataIPShortCut->rNumericArgs(2);
        } else {
            // Default to very small number (however it can't be TINY or it will break the algorithm)
            Tank.DeadBandDeltaTemp = 0.5;
        }

        if (state.dataIPShortCut->rNumericArgs(3) > 0.0) {
            Tank.TankTempLimit = state.dataIPShortCut->rNumericArgs(3);
        } else {
            // Default to very large number
            // BG comment why a large number here why not boilng point of water?
            Tank.TankTempLimit = 100.0; // 1.0E9
        }

        Tank.MaxCapacity = state.dataIPShortCut->rNumericArgs(4);
        if (Tank.MaxCapacity == DataSizing::AutoSize) {
            Tank.MaxCapacityWasAutoSized = true;
        }

        if ((state.dataIPShortCut->rNumericArgs(5) > Tank.MaxCapacity) && (!Tank.MaxCapacityWasAutoSized)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Heater Minimum Capacity cannot be greater than Heater Maximum Capacity");
            ErrorsFound = true;
        } else {
            Tank.MinCapacity = state.dataIPShortCut->rNumericArgs(5);
        }

        // Validate Heater Control Type
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));
            if (SELECT_CASE_var == "CYCLE") {
                Tank.ControlType = ControlTypeEnum::Cycle;
                Tank.MinCapacity = Tank.MaxCapacity;

            } else if (SELECT_CASE_var == "MODULATE") {
                Tank.ControlType = ControlTypeEnum::Modulate;

                // CASE ('MODULATE WITH OVERHEAT')  ! Not yet implemented

                // CASE ('MODULATE WITH UNDERHEAT')  ! Not yet implemented

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Control Type entered=" + state.dataIPShortCut->cAlphaArgs(3));
                ErrorsFound = true;
            }
        }
        Tank.VolFlowRateMin = state.dataIPShortCut->rNumericArgs(6);
        Tank.VolFlowRateMin = max(0.0, Tank.VolFlowRateMin);
        Tank.IgnitionDelay = state.dataIPShortCut->rNumericArgs(7); // Not yet implemented

        // Validate Heater Fuel Type
        bool FuelTypeError(false);
        bool AllowSteamAndDistrict(true);
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(4), Tank.FuelType, FuelTypeError, AllowSteamAndDistrict);
        if (FuelTypeError) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Invalid Heater Fuel Type entered=" + state.dataIPShortCut->cAlphaArgs(4));
            // Set to Electric to avoid errors when setting up output variables
            Tank.FuelType = "Electricity";
            ErrorsFound = true;
            FuelTypeError = false;
        }

        if (state.dataIPShortCut->rNumericArgs(8) > 0.0) {
            Tank.Efficiency = state.dataIPShortCut->rNumericArgs(8);
        } else {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Heater Thermal Efficiency must be greater than zero");
            ErrorsFound = true;
        }

        if (!state.dataIPShortCut->cAlphaArgs(5).empty()) {
            Tank.PLFCurve = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5));
            if (Tank.PLFCurve == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Part Load Factor curve not found = " + state.dataIPShortCut->cAlphaArgs(5));
                ErrorsFound = true;
            } else {
                bool IsValid;
                EnergyPlus::WaterThermalTanks::WaterThermalTankData::ValidatePLFCurve(state, Tank.PLFCurve, IsValid);

                if (!IsValid) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Part Load Factor curve failed to evaluate to greater than zero for all numbers in the domain of 0 to 1");
                    ErrorsFound = true;
                }

                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            Tank.PLFCurve,                              // Curve index
                                                            {1},                                        // Valid dimensions
                                                            RoutineName,                                // Routine name
                                                            state.dataIPShortCut->cCurrentModuleObject, // Object Type
                                                            Tank.Name,                                  // Object Name
                                                            state.dataIPShortCut->cAlphaFieldNames(5)); // Field Name
            }
        }

        Tank.OffCycParaLoad = state.dataIPShortCut->rNumericArgs(9);

        // Validate Off-Cycle Parasitic Fuel Type
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(6), Tank.OffCycParaFuelType, FuelTypeError, AllowSteamAndDistrict);
        if (FuelTypeError) {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(6));
            if (SELECT_CASE_var.empty()) { // If blank, default to Fuel Type for heater
                Tank.OffCycParaFuelType = Tank.FuelType;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + state.dataIPShortCut->cAlphaArgs(6));
                // Set to Electric to avoid errors when setting up output variables
                Tank.OffCycParaFuelType = "Electricity";
                ErrorsFound = true;
            }
            FuelTypeError = false;
        }

        Tank.OffCycParaFracToTank = state.dataIPShortCut->rNumericArgs(10);

        Tank.OnCycParaLoad = state.dataIPShortCut->rNumericArgs(11);

        // Validate On-Cycle Parasitic Fuel Type
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(7), Tank.OnCycParaFuelType, FuelTypeError, AllowSteamAndDistrict);
        if (FuelTypeError) {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(7));
            if (SELECT_CASE_var.empty()) { // If blank, default to Fuel Type for heater
                Tank.OnCycParaFuelType = Tank.FuelType;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid On-Cycle Parasitic Fuel Type entered=" + state.dataIPShortCut->cAlphaArgs(7));
                // Set to Electric to avoid errors when setting up output variables
                Tank.OnCycParaFuelType = "Electricity";
                ErrorsFound = true;
            }
            FuelTypeError = false;
        }

        Tank.OnCycParaFracToTank = state.dataIPShortCut->rNumericArgs(12);

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(8));
            if (SELECT_CASE_var == "SCHEDULE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::Schedule;
                Tank.AmbientTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));
                if (Tank.AmbientTempSchedule == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Ambient Temperature Schedule not found = " + state.dataIPShortCut->cAlphaArgs(9));
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "ZONE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::TempZone;
                Tank.AmbientTempZone = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(10), state.dataHeatBal->Zone);
                if (Tank.AmbientTempZone == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Ambient Temperature Zone not found = " + state.dataIPShortCut->cAlphaArgs(10));
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "OUTDOORS") {
                Tank.AmbientTempIndicator = AmbientTempEnum::OutsideAir;
                Tank.AmbientTempOutsideAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                     state.dataIPShortCut->cAlphaArgs(11),
                                                                                     ErrorsFound,
                                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                     1,
                                                                                     DataLoopNode::ObjectIsNotParent);
                if (!state.dataIPShortCut->cAlphaArgs(11).empty()) {
                    if (!OutAirNodeManager::CheckOutAirNodeNumber(state, Tank.AmbientTempOutsideAirNode)) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                            ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                        ShowContinueError(state, "...Referenced Node Name=" + state.dataIPShortCut->cAlphaArgs(11));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                    ErrorsFound = true;
                }

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Ambient Temperature Indicator entered=" + state.dataIPShortCut->cAlphaArgs(8));
                ShowContinueError(state, " Valid entries are SCHEDULE, ZONE, and OUTDOORS.");
                ErrorsFound = true;
            }
        }

        Tank.OffCycLossCoeff = state.dataIPShortCut->rNumericArgs(13);
        Tank.OffCycLossFracToZone = state.dataIPShortCut->rNumericArgs(14);

        Tank.OnCycLossCoeff = state.dataIPShortCut->rNumericArgs(15);
        Tank.OnCycLossFracToZone = state.dataIPShortCut->rNumericArgs(16);
        Real64 rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, Tank.FluidIndex, RoutineName);
        Tank.MassFlowRateMax = state.dataIPShortCut->rNumericArgs(17) * rho;

        if ((state.dataIPShortCut->cAlphaArgs(14).empty()) && (state.dataIPShortCut->cAlphaArgs(15).empty())) {
            if (!state.dataIPShortCut->cAlphaArgs(12).empty()) {
                Tank.FlowRateSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(12));
                if (Tank.FlowRateSchedule == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Flow Rate Schedule not found = " + state.dataIPShortCut->cAlphaArgs(12));
                    ErrorsFound = true;
                }
            }
        }

        if (!state.dataIPShortCut->cAlphaArgs(13).empty()) {
            Tank.UseInletTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(13));
            if (Tank.UseInletTempSchedule == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Cold Water Supply Temperature Schedule not found = " + state.dataIPShortCut->cAlphaArgs(13));
                ErrorsFound = true;
            }
        }

        if (NumNums > 17) {
            if ((state.dataIPShortCut->rNumericArgs(18) > 1) || (state.dataIPShortCut->rNumericArgs(18) < 0)) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Use Side Effectiveness is out of bounds (0 to 1)");
                ErrorsFound = true;
            }
            Tank.UseEffectiveness = state.dataIPShortCut->rNumericArgs(18);
        } else {
            Tank.UseEffectiveness = 1.0; // Default for stand-alone mode
        }

        if (NumNums > 18) {
            if ((state.dataIPShortCut->rNumericArgs(19) > 1) || (state.dataIPShortCut->rNumericArgs(19) <= 0)) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                ErrorsFound = true;
            }
            Tank.SourceEffectiveness = state.dataIPShortCut->rNumericArgs(19);
        } else {
            Tank.SourceEffectiveness = 1.0;
        }

        // If no plant nodes are connected, simulate in stand-alone mode.
        if (state.dataIPShortCut->cAlphaArgs(14).empty() && state.dataIPShortCut->cAlphaArgs(15).empty() &&
            state.dataIPShortCut->cAlphaArgs(16).empty() && state.dataIPShortCut->cAlphaArgs(17).empty()) {
            Tank.StandAlone = true;
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(20)) {
            Tank.UseDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(20);
            if (Tank.UseDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.UseDesignVolFlowRateWasAutoSized = true;
            }
        } else {
            Tank.UseDesignVolFlowRate = 0.0;
        }
        Tank.UseSide.loopSideNum = DataPlant::DemandSupply_No;

        if (!state.dataIPShortCut->lNumericFieldBlanks(21)) {
            Tank.SourceDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(21);
            if (Tank.SourceDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.SourceDesignVolFlowRateWasAutoSized = true;
            }
        } else {
            Tank.SourceDesignVolFlowRate = 0.0;
        }
        Tank.SrcSide.loopSideNum = DataPlant::DemandSupply_No;

        if (!state.dataIPShortCut->lNumericFieldBlanks(22)) {
            Tank.SizingRecoveryTime = state.dataIPShortCut->rNumericArgs(22);
        } else {
            Tank.SizingRecoveryTime = 1.5;
        }

        if ((!state.dataIPShortCut->cAlphaArgs(14).empty()) || (!state.dataIPShortCut->cAlphaArgs(15).empty())) {
            Tank.UseInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                    state.dataIPShortCut->cAlphaArgs(14),
                                                                    ErrorsFound,
                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Water,
                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                    1,
                                                                    DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName1 = state.dataIPShortCut->cAlphaArgs(14);
            Tank.UseOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                     state.dataIPShortCut->cAlphaArgs(15),
                                                                     ErrorsFound,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                     DataLoopNode::NodeFluidType::Water,
                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                     1,
                                                                     DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName1 = state.dataIPShortCut->cAlphaArgs(15);

            if (state.dataIPShortCut->rNumericArgs(17) > 0) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                     ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used");
            }

            if (Tank.FlowRateSchedule > 0) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                     ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used");
            }

            if (Tank.UseInletTempSchedule > 0) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                     ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used");
            }
        }

        if ((!state.dataIPShortCut->cAlphaArgs(16).empty()) || (!state.dataIPShortCut->cAlphaArgs(17).empty())) {
            Tank.SourceInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                       state.dataIPShortCut->cAlphaArgs(16),
                                                                       ErrorsFound,
                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                       DataLoopNode::NodeFluidType::Water,
                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                       2,
                                                                       DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName2 = state.dataIPShortCut->cAlphaArgs(16);
            Tank.SourceOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                        state.dataIPShortCut->cAlphaArgs(17),
                                                                        ErrorsFound,
                                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                        DataLoopNode::NodeFluidType::Water,
                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                        2,
                                                                        DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName2 = state.dataIPShortCut->cAlphaArgs(17);
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(18));
                if (SELECT_CASE_var == "STORAGETANK") {
                    Tank.SourceSideControlMode = SourceSideEnum::StorageTank;
                } else if (SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT") {
                    Tank.SourceSideControlMode = SourceSideEnum::IndirectHeatPrimarySetpoint;
                } else if (SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT") {
                    Tank.SourceSideControlMode = SourceSideEnum::IndirectHeatAltSetpoint;
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Invalid Control Mode entered=" + state.dataIPShortCut->cAlphaArgs(18));
                    ErrorsFound = true;
                }
            }
        } else {
            Tank.SourceSideControlMode = SourceSideEnum::IndirectHeatPrimarySetpoint;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
            Tank.SourceSideAltSetpointSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(19));
            if (Tank.SourceSideAltSetpointSchedNum == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) + ":  " +
                                    state.dataIPShortCut->cAlphaFieldNames(19) + " not found = " + state.dataIPShortCut->cAlphaArgs(19));
                ErrorsFound = true;
            }
        }
        if (NumAlphas > 19) {
            Tank.EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(20);
        }

    } // WaterThermalTankNum

    return ErrorsFound;
}

bool getWaterHeaterStratifiedInput(EnergyPlusData &state)
{
    bool ErrorsFound = false;
    static constexpr std::string_view RoutineName = "getWaterHeaterStratifiedInput";

    state.dataIPShortCut->cCurrentModuleObject = cStratifiedWHModuleObj; //'WaterHeater:Stratified'

    for (int WaterThermalTankNum = state.dataWaterThermalTanks->numWaterHeaterMixed + 1;
         WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified;
         ++WaterThermalTankNum) {
        int NumAlphas;
        int NumNums;
        int IOStat;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 WaterThermalTankNum - state.dataWaterThermalTanks->numWaterHeaterMixed,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataWaterThermalTanks->UniqueWaterThermalTankNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);

        auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum);

        Tank.Name = state.dataIPShortCut->cAlphaArgs(1);
        Tank.Type = state.dataIPShortCut->cCurrentModuleObject;
        Tank.TypeNum = DataPlant::TypeOf_WtrHeaterStratified;
        Tank.FluidIndex = Tank.waterIndex;

        // default to always on
        Tank.SourceSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        Tank.UseSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;

        Tank.EndUseSubcategoryName = state.dataIPShortCut->cAlphaArgs(2);

        Tank.Volume = state.dataIPShortCut->rNumericArgs(1);
        if (Tank.Volume == DataSizing::AutoSize) {
            Tank.VolumeWasAutoSized = true;
        }
        Real64 rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, Tank.FluidIndex, RoutineName);
        Tank.Mass = Tank.Volume * rho;
        Tank.Height = state.dataIPShortCut->rNumericArgs(2);
        if (Tank.Height == DataSizing::AutoSize) {
            Tank.HeightWasAutoSized = true;
        }

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));
            if (SELECT_CASE_var == "VERTICALCYLINDER") {
                Tank.Shape = TankShapeEnum::VertCylinder;

            } else if (SELECT_CASE_var == "HORIZONTALCYLINDER") {
                Tank.Shape = TankShapeEnum::HorizCylinder;

            } else if (SELECT_CASE_var == "OTHER") {
                Tank.Shape = TankShapeEnum::Other;
                if (state.dataIPShortCut->rNumericArgs(3) > 0.0) {
                    Tank.Perimeter = state.dataIPShortCut->rNumericArgs(3);
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER");
                    ErrorsFound = true;
                }

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Tank Shape entered=" + state.dataIPShortCut->cAlphaArgs(3));
                Tank.Shape = TankShapeEnum::VertCylinder;
                ErrorsFound = true;
            }
        }

        if (state.dataIPShortCut->rNumericArgs(4) > 0.0) {
            Tank.TankTempLimit = state.dataIPShortCut->rNumericArgs(4);
        } else {
            // Default to very large number
            Tank.TankTempLimit = 1.0e9;
        }

        // Validate Heater Priority Control
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(4));
            if (SELECT_CASE_var == "MASTERSLAVE") {
                Tank.ControlType = PriorityEnum::MasterSlave;

            } else if (SELECT_CASE_var == "SIMULTANEOUS") {
                Tank.ControlType = PriorityEnum::Simultaneous;

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Heater Priority Control entered=" + state.dataIPShortCut->cAlphaArgs(4));
                ErrorsFound = true;
            }
        }

        Tank.SetPointTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
            ShowSevereError(
                state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", missing data.");
            ShowContinueError(state, "blank field, missing " + state.dataIPShortCut->cAlphaFieldNames(5) + " is required");
            ErrorsFound = true;
        } else if (Tank.SetPointTempSchedule == 0) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) + ": " +
                                state.dataIPShortCut->cAlphaFieldNames(5) + " not found = " + state.dataIPShortCut->cAlphaArgs(5));
            ErrorsFound = true;
        }

        if (state.dataIPShortCut->rNumericArgs(5) > 0.0) {
            Tank.DeadBandDeltaTemp = state.dataIPShortCut->rNumericArgs(5);
        } else {
            // Default to very small number (however it can't be TINY or it will break the algorithm)
            Tank.DeadBandDeltaTemp = 0.0001;
        }

        Tank.MaxCapacity = state.dataIPShortCut->rNumericArgs(6);
        if (Tank.MaxCapacity == DataSizing::AutoSize) {
            Tank.MaxCapacityWasAutoSized = true;
        }

        Tank.HeaterHeight1 = state.dataIPShortCut->rNumericArgs(7);

        // adjust tank height used in these calculations for testing heater height
        Real64 tankHeightForTesting;
        if (Tank.Shape == TankShapeEnum::HorizCylinder) {
            tankHeightForTesting = 2.0 * sqrt((Tank.Volume / Tank.Height) / DataGlobalConstants::Pi);
        } else {
            tankHeightForTesting = Tank.Height;
        }

        // Test if Heater height is within range
        if ((!Tank.HeightWasAutoSized) && (Tank.HeaterHeight1 > tankHeightForTesting)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Heater 1 is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(7), state.dataIPShortCut->rNumericArgs(7)));
            ErrorsFound = true;
        }

        Tank.SetPointTempSchedule2 = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
        if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            ShowSevereError(
                state, std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", missing data.");
            ShowContinueError(state, "blank field, missing " + state.dataIPShortCut->cAlphaFieldNames(6) + " is required");
            ErrorsFound = true;
        } else if (Tank.SetPointTempSchedule2 == 0) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) + ":  " +
                                state.dataIPShortCut->cAlphaFieldNames(6) + " not found = " + state.dataIPShortCut->cAlphaArgs(6));
            ErrorsFound = true;
        }

        if (state.dataIPShortCut->rNumericArgs(5) > 0.0) {
            Tank.DeadBandDeltaTemp2 = state.dataIPShortCut->rNumericArgs(8);
        } else {
            // Default to very small number (however it can't be TINY or it will break the algorithm)
            Tank.DeadBandDeltaTemp2 = 0.0001;
        }

        Tank.MaxCapacity2 = state.dataIPShortCut->rNumericArgs(9);
        Tank.HeaterHeight2 = state.dataIPShortCut->rNumericArgs(10);

        // Test if Heater height is within range
        if ((!Tank.HeightWasAutoSized) && (Tank.HeaterHeight2 > tankHeightForTesting)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Heater 2 is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(10), state.dataIPShortCut->rNumericArgs(10)));
            ErrorsFound = true;
        }

        // Validate Heater Fuel Type
        bool FuelTypeFirstPass(false);
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(7), Tank.FuelType, FuelTypeFirstPass);
        if (FuelTypeFirstPass) {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(7));
            if (SELECT_CASE_var == "STEAM") {
                Tank.FuelType = "Steam";
            } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                Tank.FuelType = "DistrictHeating";
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Heater Fuel Type entered=" + state.dataIPShortCut->cAlphaArgs(7));
                // Set to Electric to avoid errors when setting up output variables
                Tank.FuelType = "Electricity";
                ErrorsFound = true;
            }
            FuelTypeFirstPass = false;
        }

        if (state.dataIPShortCut->rNumericArgs(11) > 0.0) {
            Tank.Efficiency = state.dataIPShortCut->rNumericArgs(11);
        } else {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Heater Thermal Efficiency must be greater than zero");
            ErrorsFound = true;
        }

        Tank.OffCycParaLoad = state.dataIPShortCut->rNumericArgs(12);

        // Validate Off-Cycle Parasitic Fuel Type
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(8), Tank.OffCycParaFuelType, FuelTypeFirstPass);
        if (FuelTypeFirstPass) {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(8));
            if (SELECT_CASE_var.empty()) { // If blank, default to Fuel Type for heater
                Tank.OffCycParaFuelType = Tank.FuelType;
            } else if (SELECT_CASE_var == "STEAM") {
                Tank.OffCycParaFuelType = "Steam";
            } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                Tank.OffCycParaFuelType = "DistrictHeating";
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + state.dataIPShortCut->cAlphaArgs(8));
                // Set to Electric to avoid errors when setting up output variables
                Tank.OffCycParaFuelType = "Electricity";
                ErrorsFound = true;
            }
            FuelTypeFirstPass = false;
        }

        Tank.OffCycParaFracToTank = state.dataIPShortCut->rNumericArgs(13);
        Tank.OffCycParaHeight = state.dataIPShortCut->rNumericArgs(14);

        Tank.OnCycParaLoad = state.dataIPShortCut->rNumericArgs(15);

        // Validate On-Cycle Parasitic Fuel Type
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(9), Tank.OnCycParaFuelType, FuelTypeFirstPass);
        if (FuelTypeFirstPass) {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(9));
            if (SELECT_CASE_var.empty()) { // If blank, default to Fuel Type for heater
                Tank.OnCycParaFuelType = Tank.FuelType;
            } else if (SELECT_CASE_var == "STEAM") {
                Tank.OnCycParaFuelType = "Steam";
            } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                Tank.OnCycParaFuelType = "DistrictHeating";
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid On-Cycle Parasitic Fuel Type entered=" + state.dataIPShortCut->cAlphaArgs(9));
                // Set to Electric to avoid errors when setting up output variables
                Tank.OnCycParaFuelType = "Electricity";
                ErrorsFound = true;
            }
            FuelTypeFirstPass = false;
        }

        Tank.OnCycParaFracToTank = state.dataIPShortCut->rNumericArgs(16);
        Tank.OnCycParaHeight = state.dataIPShortCut->rNumericArgs(17);

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(10));
            if (SELECT_CASE_var == "SCHEDULE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::Schedule;
                Tank.AmbientTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
                if (Tank.AmbientTempSchedule == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Ambient Temperature Schedule not found = " + state.dataIPShortCut->cAlphaArgs(11));
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "ZONE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::TempZone;
                Tank.AmbientTempZone = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(12), state.dataHeatBal->Zone);
                if (Tank.AmbientTempZone == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Ambient Temperature Zone not found = " + state.dataIPShortCut->cAlphaArgs(12));
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "OUTDOORS") {
                Tank.AmbientTempIndicator = AmbientTempEnum::OutsideAir;
                Tank.AmbientTempOutsideAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                     state.dataIPShortCut->cAlphaArgs(13),
                                                                                     ErrorsFound,
                                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Inlet,
                                                                                     1,
                                                                                     DataLoopNode::ObjectIsNotParent);
                if (!state.dataIPShortCut->cAlphaArgs(13).empty()) {
                    if (!OutAirNodeManager::CheckOutAirNodeNumber(state, Tank.AmbientTempOutsideAirNode)) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                            ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                        ShowContinueError(state, "...Referenced Node Name=" + state.dataIPShortCut->cAlphaArgs(13));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                    ErrorsFound = true;
                }

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Ambient Temperature Indicator entered=" + state.dataIPShortCut->cAlphaArgs(10));
                ShowContinueError(state, " Valid entries are Schedule, Zone, and Outdoors.");
                ErrorsFound = true;
            }
        }

        Tank.SkinLossCoeff = state.dataIPShortCut->rNumericArgs(18);
        Tank.SkinLossFracToZone = state.dataIPShortCut->rNumericArgs(19);
        Tank.OffCycFlueLossCoeff = state.dataIPShortCut->rNumericArgs(20);
        Tank.OffCycFlueLossFracToZone = state.dataIPShortCut->rNumericArgs(21);

        // this is temporary until we know fluid type
        rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, Tank.FluidIndex, RoutineName);
        Tank.MassFlowRateMax = state.dataIPShortCut->rNumericArgs(22) * rho;

        if ((state.dataIPShortCut->cAlphaArgs(16).empty()) && (state.dataIPShortCut->cAlphaArgs(17).empty())) {
            if (!state.dataIPShortCut->cAlphaArgs(14).empty()) {
                Tank.FlowRateSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(14));
                if (Tank.FlowRateSchedule == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Flow Rate Schedule not found = " + state.dataIPShortCut->cAlphaArgs(14));
                    ErrorsFound = true;
                }
            }
        }

        if (!state.dataIPShortCut->cAlphaArgs(15).empty()) {
            Tank.UseInletTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(15));
            if (Tank.UseInletTempSchedule == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Cold Water Supply Temperature Schedule not found = " + state.dataIPShortCut->cAlphaArgs(15));
                ErrorsFound = true;
            }
        }

        if (NumNums > 22) {
            Tank.UseEffectiveness = state.dataIPShortCut->rNumericArgs(23);
        } else {
            Tank.UseEffectiveness = 1.0; // Default for stand-alone mode
        }

        if (NumNums > 23) {
            Tank.UseInletHeight = state.dataIPShortCut->rNumericArgs(24);
        } else {
            // Defaults to bottom of tank
            Tank.UseInletHeight = 0.0;
        }
        if ((!Tank.HeightWasAutoSized) && (Tank.UseInletHeight > Tank.Height)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Use inlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(24), state.dataIPShortCut->rNumericArgs(24)));
            ErrorsFound = true;
        }

        if ((NumNums > 24) && (state.dataIPShortCut->rNumericArgs(25) != DataGlobalConstants::AutoCalculate)) {
            Tank.UseOutletHeight = state.dataIPShortCut->rNumericArgs(25);
        } else {
            // Defaults to top of tank
            Tank.UseOutletHeight = Tank.Height;
        }
        if (Tank.UseOutletHeight == DataSizing::AutoSize) {
            Tank.UseOutletHeightWasAutoSized = true;
        }
        if ((!Tank.HeightWasAutoSized) && (Tank.UseOutletHeight > Tank.Height)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Use outlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(25), state.dataIPShortCut->rNumericArgs(25)));
            ErrorsFound = true;
        }

        if (NumNums > 25) {
            if ((state.dataIPShortCut->rNumericArgs(26) > 1) || (state.dataIPShortCut->rNumericArgs(26) <= 0)) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                ErrorsFound = true;
            }
            Tank.SourceEffectiveness = state.dataIPShortCut->rNumericArgs(26);
        } else {
            Tank.SourceEffectiveness = 1.0;
        }

        if ((NumNums > 26) && (state.dataIPShortCut->rNumericArgs(27) != DataGlobalConstants::AutoCalculate)) {
            Tank.SourceInletHeight = state.dataIPShortCut->rNumericArgs(27);
        } else {
            // Defaults to top of tank
            Tank.SourceInletHeight = Tank.Height;
        }
        if (Tank.SourceInletHeight == DataSizing::AutoSize) {
            Tank.SourceInletHeightWasAutoSized = true;
        }
        if ((!Tank.HeightWasAutoSized) && (Tank.SourceInletHeight > Tank.Height)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Source inlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(27), state.dataIPShortCut->rNumericArgs(27)));
            ErrorsFound = true;
        }

        if ((NumNums > 27) && (state.dataIPShortCut->rNumericArgs(28) != DataGlobalConstants::AutoCalculate)) {
            Tank.SourceOutletHeight = state.dataIPShortCut->rNumericArgs(28);
        } else {
            // Defaults to bottom of tank
            Tank.SourceOutletHeight = 0.0;
        }
        if ((!Tank.HeightWasAutoSized) && (Tank.SourceOutletHeight > Tank.Height)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Source outlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(28), state.dataIPShortCut->rNumericArgs(28)));
            ErrorsFound = true;
        }

        // If no plant nodes are connected, simulate in stand-alone mode.
        if (state.dataIPShortCut->cAlphaArgs(16).empty() && state.dataIPShortCut->cAlphaArgs(17).empty() &&
            state.dataIPShortCut->cAlphaArgs(18).empty() && state.dataIPShortCut->cAlphaArgs(19).empty())
            Tank.StandAlone = true;

        if (!state.dataIPShortCut->lNumericFieldBlanks(29)) {
            Tank.UseDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(29);
            if (Tank.UseDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.UseDesignVolFlowRateWasAutoSized = true;
            }
        } else {
            Tank.UseDesignVolFlowRate = 0.0;
        }

        Tank.UseSide.loopSideNum = DataPlant::DemandSupply_No;

        if (!state.dataIPShortCut->lNumericFieldBlanks(30)) {
            Tank.SourceDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(30);
            if (Tank.SourceDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.SourceDesignVolFlowRateWasAutoSized = true;
            }
        } else {
            Tank.SourceDesignVolFlowRate = 0.0;
        }

        if (NumNums > 30) {
            Tank.SizingRecoveryTime = state.dataIPShortCut->rNumericArgs(31);
        } else {
            Tank.SizingRecoveryTime = 1.5;
        }

        Tank.SrcSide.loopSideNum = DataPlant::DemandSupply_No;

        if ((!state.dataIPShortCut->cAlphaArgs(16).empty()) || (!state.dataIPShortCut->cAlphaArgs(17).empty())) {
            Tank.UseInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                    state.dataIPShortCut->cAlphaArgs(16),
                                                                    ErrorsFound,
                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Water,
                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                    1,
                                                                    DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName1 = state.dataIPShortCut->cAlphaArgs(16);
            Tank.UseOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                     state.dataIPShortCut->cAlphaArgs(17),
                                                                     ErrorsFound,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                     DataLoopNode::NodeFluidType::Water,
                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                     1,
                                                                     DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName1 = state.dataIPShortCut->cAlphaArgs(17);

            if (state.dataIPShortCut->rNumericArgs(22) > 0) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                     ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used");
            }

            if (Tank.FlowRateSchedule > 0) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                     ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used");
            }

            if (Tank.UseInletTempSchedule > 0) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                     ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used");
            }
        }

        if ((!state.dataIPShortCut->cAlphaArgs(18).empty()) || (!state.dataIPShortCut->cAlphaArgs(19).empty())) {
            Tank.SourceInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                       state.dataIPShortCut->cAlphaArgs(18),
                                                                       ErrorsFound,
                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                       DataLoopNode::NodeFluidType::Water,
                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                       2,
                                                                       DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName2 = state.dataIPShortCut->cAlphaArgs(18);
            Tank.SourceOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                        state.dataIPShortCut->cAlphaArgs(19),
                                                                        ErrorsFound,
                                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                        DataLoopNode::NodeFluidType::Water,
                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                        2,
                                                                        DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName2 = state.dataIPShortCut->cAlphaArgs(19);
        }

        // Validate inlet mode
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(20));
            if (SELECT_CASE_var == "FIXED") {
                Tank.InletMode = InletModeEnum::Fixed;

            } else if (SELECT_CASE_var == "SEEKING") {
                Tank.InletMode = InletModeEnum::Seeking;
            }
        }

        Tank.Nodes = state.dataIPShortCut->rNumericArgs(32);
        int specifiedNodes = 0;
        Tank.AdditionalCond = state.dataIPShortCut->rNumericArgs(33);

        Tank.AdditionalLossCoeff.allocate(Tank.Nodes);
        Tank.AdditionalLossCoeff = 0.0;
        for (int NodeNum = 1; NodeNum <= 12; ++NodeNum) {
            int index = 33 + NodeNum;
            if (NumNums >= index) {
                if (NodeNum <= Tank.Nodes) {
                    ++specifiedNodes;
                    Tank.AdditionalLossCoeff(NodeNum) = state.dataIPShortCut->rNumericArgs(index);
                } else if (!state.dataIPShortCut->lNumericFieldBlanks(index) && (state.dataIPShortCut->rNumericArgs(index) != 0)) {
                    // If either blank, or zero (default), then do not warn
                    ++specifiedNodes;
                }
            } else {
                break;
            }
        }

        if (specifiedNodes > Tank.Nodes) {
            ShowWarningError(state,
                             state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                 ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used");
        }

        Tank.SetupStratifiedNodes(state);

        if (!state.dataIPShortCut->lAlphaFieldBlanks(21)) {
            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(21));
                if (SELECT_CASE_var == "STORAGETANK") {
                    Tank.SourceSideControlMode = SourceSideEnum::StorageTank;
                } else if (SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT") {
                    Tank.SourceSideControlMode = SourceSideEnum::IndirectHeatPrimarySetpoint;
                } else if (SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT") {
                    Tank.SourceSideControlMode = SourceSideEnum::IndirectHeatAltSetpoint;
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Invalid Control Mode entered=" + state.dataIPShortCut->cAlphaArgs(21));
                    ErrorsFound = true;
                }
            }
        } else {
            Tank.SourceSideControlMode = SourceSideEnum::IndirectHeatPrimarySetpoint;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(22)) {
            Tank.SourceSideAltSetpointSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(22));
            if (Tank.SourceSideAltSetpointSchedNum == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) + ":  " +
                                    state.dataIPShortCut->cAlphaFieldNames(22) + " not found = " + state.dataIPShortCut->cAlphaArgs(22));
                ErrorsFound = true;
            }
        }
    }

    return ErrorsFound;
}

bool getWaterTankMixedInput(EnergyPlusData &state)
{
    bool ErrorsFound = false;

    state.dataIPShortCut->cCurrentModuleObject = cMixedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Mixed'
    for (int WaterThermalTankNum = state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified + 1;
         WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified +
                                    state.dataWaterThermalTanks->numChilledWaterMixed;
         ++WaterThermalTankNum) {
        int NumAlphas;
        int NumNums;
        int IOStat;
        state.dataInputProcessing->inputProcessor->getObjectItem(
            state,
            state.dataIPShortCut->cCurrentModuleObject,
            WaterThermalTankNum - (state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified),
            state.dataIPShortCut->cAlphaArgs,
            NumAlphas,
            state.dataIPShortCut->rNumericArgs,
            NumNums,
            IOStat,
            state.dataIPShortCut->lNumericFieldBlanks,
            state.dataIPShortCut->lAlphaFieldBlanks,
            state.dataIPShortCut->cAlphaFieldNames,
            state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataWaterThermalTanks->UniqueWaterThermalTankNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);

        auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum);

        Tank.Name = state.dataIPShortCut->cAlphaArgs(1);
        Tank.Type = state.dataIPShortCut->cCurrentModuleObject;
        Tank.TypeNum = DataPlant::TypeOf_ChilledWaterTankMixed;
        Tank.FluidIndex = Tank.waterIndex;
        Tank.IsChilledWaterTank = true;
        Tank.EndUseSubcategoryName = "Chilled Water Storage";

        Tank.Volume = state.dataIPShortCut->rNumericArgs(1);
        if (Tank.Volume == DataSizing::AutoSize) {
            Tank.VolumeWasAutoSized = true;
        }

        if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
            // Set volume to a really small number to continue simulation
            Tank.Volume = 0.000001; // = 1 cm3
        }

        Tank.SetPointTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
        if (Tank.SetPointTempSchedule == 0) {
            ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));

            ErrorsFound = true;
        }

        if (state.dataIPShortCut->rNumericArgs(2) > 0.0001) {
            Tank.DeadBandDeltaTemp = state.dataIPShortCut->rNumericArgs(2);
        } else {
            // Default to very small number (however it can't be TINY or it will break the algorithm)
            Tank.DeadBandDeltaTemp = 0.5;
        }

        if (state.dataIPShortCut->rNumericArgs(3) > 0.0) {
            Tank.TankTempLimit = state.dataIPShortCut->rNumericArgs(3);
        } else {
            // default to just above freezing
            Tank.TankTempLimit = 1.0;
        }

        Tank.MaxCapacity = state.dataIPShortCut->rNumericArgs(4);
        if (Tank.MaxCapacity == DataSizing::AutoSize) {
            Tank.MaxCapacityWasAutoSized = true;
        }

        Tank.MinCapacity = 0.0;
        Tank.ControlType = ControlTypeEnum::Cycle;

        Tank.MassFlowRateMin = 0.0;
        Tank.IgnitionDelay = 0.0;
        Tank.FuelType = "Electricity";
        Tank.Efficiency = 1.0;
        Tank.PLFCurve = 0;
        Tank.OffCycParaLoad = 0.0;
        Tank.OffCycParaFuelType = "Electricity";
        Tank.OffCycParaFracToTank = 0.0;
        Tank.OnCycParaLoad = 0.0;
        Tank.OnCycParaFuelType = "Electricity";
        Tank.OnCycParaFracToTank = 0.0;

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));
            if (SELECT_CASE_var == "SCHEDULE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::Schedule;
                Tank.AmbientTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
                if (Tank.AmbientTempSchedule == 0) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Schedule was not found.");
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "ZONE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::TempZone;
                Tank.AmbientTempZone = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(5), state.dataHeatBal->Zone);
                if (Tank.AmbientTempZone == 0) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Zone was not found.");
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "OUTDOORS") {
                Tank.AmbientTempIndicator = AmbientTempEnum::OutsideAir;
                Tank.AmbientTempOutsideAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                     state.dataIPShortCut->cAlphaArgs(6),
                                                                                     ErrorsFound,
                                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                     1,
                                                                                     DataLoopNode::ObjectIsNotParent);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    if (!OutAirNodeManager::CheckOutAirNodeNumber(state, Tank.AmbientTempOutsideAirNode)) {
                        ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                        ShowContinueError(state, "Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                    ErrorsFound = true;
                }

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Ambient Temperature Indicator entered=" + state.dataIPShortCut->cAlphaArgs(3));
                ShowContinueError(state, " Valid entries are Schedule, Zone, and Outdoors.");
                ErrorsFound = true;
            }
        }

        Tank.OffCycLossCoeff = state.dataIPShortCut->rNumericArgs(5);
        Tank.OffCycLossFracToZone = 1.0;

        Tank.OnCycLossCoeff = state.dataIPShortCut->rNumericArgs(5);
        Tank.OnCycLossFracToZone = 1.0;

        Tank.MassFlowRateMax = 0.0;
        Tank.FlowRateSchedule = 0;
        Tank.UseInletTempSchedule = 0;

        // default to always on
        Tank.SourceSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        Tank.UseSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;

        if ((state.dataIPShortCut->rNumericArgs(6) > 1) || (state.dataIPShortCut->rNumericArgs(6) < 0)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Use Side Effectiveness is out of bounds (0 to 1)");
            ErrorsFound = true;
        }
        Tank.UseEffectiveness = state.dataIPShortCut->rNumericArgs(6);

        if ((state.dataIPShortCut->rNumericArgs(8) > 1) || (state.dataIPShortCut->rNumericArgs(8) <= 0)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Source Side Effectiveness is out of bounds (>0 to 1)");
            ErrorsFound = true;
        }
        Tank.SourceEffectiveness = state.dataIPShortCut->rNumericArgs(8);

        if (state.dataIPShortCut->lNumericFieldBlanks(7)) {
            Tank.UseDesignVolFlowRate = 0.0;
        } else {
            Tank.UseDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(7);
            if (Tank.UseDesignVolFlowRate) {
                Tank.UseDesignVolFlowRateWasAutoSized = true;
            }
        }

        Tank.UseSide.loopSideNum = DataPlant::DemandSupply_No;

        if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
            Tank.UseSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Tank.UseSideAvailSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));
            if (Tank.UseSideAvailSchedNum == 0) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(9) + " = " + state.dataIPShortCut->cAlphaArgs(9));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Schedule was not found.");
                ErrorsFound = true;
            }
        }

        Tank.SrcSide.loopSideNum = DataPlant::DemandSupply_No;

        if (state.dataIPShortCut->lNumericFieldBlanks(9)) {
            Tank.SourceDesignVolFlowRate = 0.0;
        } else {
            Tank.SourceDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(9);
            if (Tank.SourceDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.SourceDesignVolFlowRateWasAutoSized = true;
            }
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(12)) {
            Tank.SourceSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Tank.SourceSideAvailSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(12));
            if (Tank.SourceSideAvailSchedNum == 0) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(12) + " = " + state.dataIPShortCut->cAlphaArgs(12));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Schedule was not found.");
                ErrorsFound = true;
            }
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(10)) {
            Tank.SizingRecoveryTime = 4.0;
        } else {
            Tank.SizingRecoveryTime = state.dataIPShortCut->rNumericArgs(10);
        }

        if ((!state.dataIPShortCut->lAlphaFieldBlanks(7)) || (!state.dataIPShortCut->lAlphaFieldBlanks(8))) {
            Tank.UseInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                    state.dataIPShortCut->cAlphaArgs(7),
                                                                    ErrorsFound,
                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Water,
                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                    1,
                                                                    DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName1 = state.dataIPShortCut->cAlphaArgs(7);
            Tank.UseOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                     state.dataIPShortCut->cAlphaArgs(8),
                                                                     ErrorsFound,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                     DataLoopNode::NodeFluidType::Water,
                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                     1,
                                                                     DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName1 = state.dataIPShortCut->cAlphaArgs(8);
        }

        if ((!state.dataIPShortCut->lAlphaFieldBlanks(10)) || (!state.dataIPShortCut->lAlphaFieldBlanks(11))) {
            Tank.SourceInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                       state.dataIPShortCut->cAlphaArgs(10),
                                                                       ErrorsFound,
                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                       DataLoopNode::NodeFluidType::Water,
                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                       2,
                                                                       DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName2 = state.dataIPShortCut->cAlphaArgs(10);
            Tank.SourceOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                        state.dataIPShortCut->cAlphaArgs(11),
                                                                        ErrorsFound,
                                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                        DataLoopNode::NodeFluidType::Water,
                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                        2,
                                                                        DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName2 = state.dataIPShortCut->cAlphaArgs(11);
        }

        if (Tank.UseSide.loopSideNum == DataPlant::DemandSide && Tank.SourceInletNode != 0) {
            PlantUtilities::RegisterPlantCompDesignFlow(state, Tank.SourceInletNode, Tank.SourceDesignVolFlowRate);
        }

    } // WaterThermalTankNum

    return ErrorsFound;
}

bool getWaterTankStratifiedInput(EnergyPlusData &state)
{
    bool ErrorsFound = false;
    static constexpr std::string_view RoutineName = "getWaterTankStratifiedInput";

    state.dataIPShortCut->cCurrentModuleObject = cStratifiedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Stratified'

    for (int WaterThermalTankNum = state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified +
                                   state.dataWaterThermalTanks->numChilledWaterMixed + 1;
         WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified +
                                    state.dataWaterThermalTanks->numChilledWaterMixed + state.dataWaterThermalTanks->numChilledWaterStratified;
         ++WaterThermalTankNum) {
        int NumNums;
        int NumAlphas;
        int IOStat;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 WaterThermalTankNum - (state.dataWaterThermalTanks->numWaterHeaterMixed +
                                                                                        state.dataWaterThermalTanks->numWaterHeaterStratified +
                                                                                        state.dataWaterThermalTanks->numChilledWaterMixed),
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataWaterThermalTanks->UniqueWaterThermalTankNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);

        auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum);

        Tank.Name = state.dataIPShortCut->cAlphaArgs(1);
        Tank.Type = state.dataIPShortCut->cCurrentModuleObject;
        Tank.TypeNum = DataPlant::TypeOf_ChilledWaterTankStratified;
        Tank.FluidIndex = Tank.waterIndex;
        Tank.IsChilledWaterTank = true;
        Tank.EndUseSubcategoryName = "Chilled Water Storage";

        Tank.Volume = state.dataIPShortCut->rNumericArgs(1);
        if (Tank.Volume == DataSizing::AutoSize) {
            Tank.VolumeWasAutoSized = true;
        }
        Real64 rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, Tank.FluidIndex, RoutineName);
        Tank.Mass = Tank.Volume * rho;
        Tank.Height = state.dataIPShortCut->rNumericArgs(2);
        if (Tank.Height == DataSizing::AutoSize) {
            Tank.HeightWasAutoSized = true;
        }

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(2));
            if (SELECT_CASE_var == "VERTICALCYLINDER") {
                Tank.Shape = TankShapeEnum::VertCylinder;

            } else if (SELECT_CASE_var == "HORIZONTALCYLINDER") {
                Tank.Shape = TankShapeEnum::HorizCylinder;

            } else if (SELECT_CASE_var == "OTHER") {
                Tank.Shape = TankShapeEnum::Other;
                if (state.dataIPShortCut->rNumericArgs(3) > 0.0) {
                    Tank.Perimeter = state.dataIPShortCut->rNumericArgs(3);
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER");
                    ErrorsFound = true;
                }

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Tank Shape entered=" + state.dataIPShortCut->cAlphaArgs(2));
                Tank.Shape = TankShapeEnum::VertCylinder;
                ErrorsFound = true;
            }
        }

        if (state.dataIPShortCut->rNumericArgs(6) > 0.0) {
            Tank.TankTempLimit = state.dataIPShortCut->rNumericArgs(6);
        } else {
            // default to just above freezing
            Tank.TankTempLimit = 1.0;
        }

        Tank.SetPointTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
        if (Tank.SetPointTempSchedule == 0) {
            ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Schedule was not found.");
            ErrorsFound = true;
        }

        if (state.dataIPShortCut->rNumericArgs(4) > 0.0) {
            Tank.DeadBandDeltaTemp = state.dataIPShortCut->rNumericArgs(4);
        } else {
            // Default to very small number (however it can't be TINY or it will break the algorithm)
            Tank.DeadBandDeltaTemp = 0.0001;
        }

        Tank.HeaterHeight1 = state.dataIPShortCut->rNumericArgs(5);
        Tank.MaxCapacity = state.dataIPShortCut->rNumericArgs(7);
        if (Tank.MaxCapacity == DataSizing::AutoSize) {
            Tank.MaxCapacityWasAutoSized = true;
        }

        Tank.Efficiency = 1.0;
        Tank.SetPointTempSchedule2 = 0;
        Tank.MaxCapacity2 = 0.0;
        Tank.HeaterHeight2 = 0.0;
        Tank.FuelType = "Electricity";

        Tank.OffCycParaLoad = 0.0;
        Tank.OffCycParaFuelType = "Electricity";
        Tank.OffCycParaFracToTank = 0.0;
        Tank.OffCycParaHeight = 0.0;
        Tank.OnCycParaLoad = 0.0;
        Tank.OnCycParaFuelType = "Electricity";
        Tank.OnCycParaFracToTank = 0.0;
        Tank.OnCycParaHeight = 0.0;

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(4));
            if (SELECT_CASE_var == "SCHEDULE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::Schedule;
                Tank.AmbientTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
                if (Tank.AmbientTempSchedule == 0) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Schedule was not found.");
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == "ZONE") {
                Tank.AmbientTempIndicator = AmbientTempEnum::TempZone;
                Tank.AmbientTempZone = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(6), state.dataHeatBal->Zone);
                if (Tank.AmbientTempZone == 0) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
                    ShowContinueError(state,
                                      "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "Zone was not found.");
                    ErrorsFound = true;
                }
                Tank.OffCycLossFracToZone = 1.0;

            } else if (SELECT_CASE_var == "OUTDOORS") {
                Tank.AmbientTempIndicator = AmbientTempEnum::OutsideAir;
                Tank.AmbientTempOutsideAirNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                     state.dataIPShortCut->cAlphaArgs(7),
                                                                                     ErrorsFound,
                                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Inlet,
                                                                                     1,
                                                                                     DataLoopNode::ObjectIsNotParent);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    if (!OutAirNodeManager::CheckOutAirNodeNumber(state, Tank.AmbientTempOutsideAirNode)) {
                        ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(7) + " = " + state.dataIPShortCut->cAlphaArgs(7));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                        ShowContinueError(state, "Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                    ErrorsFound = true;
                }

            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                    ":  Invalid Ambient Temperature Indicator entered=" + state.dataIPShortCut->cAlphaArgs(4));
                ShowContinueError(state, "  Valid entries are Schedule, Zone, and Outdoors.");
                ErrorsFound = true;
            }
        }

        Tank.SkinLossCoeff = state.dataIPShortCut->rNumericArgs(8);
        Tank.SkinLossFracToZone = 1.0;
        Tank.OffCycFlueLossCoeff = 0.0;
        Tank.OffCycFlueLossFracToZone = 0.0;

        Tank.MassFlowRateMax = 0.0;
        Tank.FlowRateSchedule = 0;
        Tank.UseInletTempSchedule = 0;
        Tank.UseEffectiveness = state.dataIPShortCut->rNumericArgs(9);
        Tank.UseInletHeight = state.dataIPShortCut->rNumericArgs(10);

        // default to always on
        Tank.SourceSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        Tank.UseSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;

        if (state.dataIPShortCut->rNumericArgs(10) == DataGlobalConstants::AutoCalculate) {
            Tank.UseInletHeight = Tank.Height; // top of tank
        }
        if (Tank.UseInletHeight > Tank.Height) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Use inlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(10), state.dataIPShortCut->rNumericArgs(10)));
            ErrorsFound = true;
        }

        Tank.UseOutletHeight = state.dataIPShortCut->rNumericArgs(11);
        if (Tank.UseOutletHeight > Tank.Height) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Use outlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(11), state.dataIPShortCut->rNumericArgs(11)));
            ErrorsFound = true;
        }

        if ((state.dataIPShortCut->rNumericArgs(13) > 1) || (state.dataIPShortCut->rNumericArgs(13) <= 0)) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ":  Source Side Effectiveness is out of bounds (>0 to 1)");
            ErrorsFound = true;
        }
        Tank.SourceEffectiveness = state.dataIPShortCut->rNumericArgs(13);

        Tank.SourceInletHeight = state.dataIPShortCut->rNumericArgs(14);
        if (Tank.SourceInletHeight > Tank.Height) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Source inlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(14), state.dataIPShortCut->rNumericArgs(14)));
            ErrorsFound = true;
        }

        Tank.SourceOutletHeight = state.dataIPShortCut->rNumericArgs(15);
        if (state.dataIPShortCut->rNumericArgs(15) == DataGlobalConstants::AutoCalculate) {
            Tank.SourceOutletHeight = Tank.Height; // top of tank
        }
        if (Tank.SourceOutletHeight > Tank.Height) {
            ShowSevereError(state,
                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                ": Source outlet is located higher than overall tank height.");
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
            ShowContinueError(state, format("{} = {:.4R}", state.dataIPShortCut->cNumericFieldNames(15), state.dataIPShortCut->rNumericArgs(15)));
            ErrorsFound = true;
        }

        Tank.StandAlone = false;

        if (state.dataIPShortCut->lNumericFieldBlanks(12)) {
            Tank.UseDesignVolFlowRate = 0.0;
        } else {
            Tank.UseDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(12);
            if (Tank.UseDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.UseDesignVolFlowRateWasAutoSized = true;
            }
        }

        Tank.UseSide.loopSideNum = DataPlant::DemandSupply_No;

        if (state.dataIPShortCut->lNumericFieldBlanks(16)) {
            Tank.SourceDesignVolFlowRate = 0.0;
        } else {
            Tank.SourceDesignVolFlowRate = state.dataIPShortCut->rNumericArgs(16);
            if (Tank.SourceDesignVolFlowRate == DataSizing::AutoSize) {
                Tank.SourceDesignVolFlowRateWasAutoSized = true;
            }
        }

        Tank.SizingRecoveryTime = state.dataIPShortCut->rNumericArgs(17);

        Tank.SrcSide.loopSideNum = DataPlant::DemandSupply_No;

        if ((!state.dataIPShortCut->lAlphaFieldBlanks(8)) || (!state.dataIPShortCut->lAlphaFieldBlanks(9))) {
            Tank.UseInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                    state.dataIPShortCut->cAlphaArgs(8),
                                                                    ErrorsFound,
                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Water,
                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                    1,
                                                                    DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName1 = state.dataIPShortCut->cAlphaArgs(8);
            Tank.UseOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                     state.dataIPShortCut->cAlphaArgs(9),
                                                                     ErrorsFound,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                     DataLoopNode::NodeFluidType::Water,
                                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                                     1,
                                                                     DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName1 = state.dataIPShortCut->cAlphaArgs(9);
        }

        if ((!state.dataIPShortCut->lAlphaFieldBlanks(11)) || (!state.dataIPShortCut->lAlphaFieldBlanks(12))) {
            Tank.SourceInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                       state.dataIPShortCut->cAlphaArgs(11),
                                                                       ErrorsFound,
                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                       DataLoopNode::NodeFluidType::Water,
                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                       2,
                                                                       DataLoopNode::ObjectIsNotParent);
            Tank.InletNodeName2 = state.dataIPShortCut->cAlphaArgs(11);
            Tank.SourceOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                        state.dataIPShortCut->cAlphaArgs(12),
                                                                        ErrorsFound,
                                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                        DataLoopNode::NodeFluidType::Water,
                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                        2,
                                                                        DataLoopNode::ObjectIsNotParent);
            Tank.OutletNodeName2 = state.dataIPShortCut->cAlphaArgs(12);
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(10)) {
            Tank.UseSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Tank.UseSideAvailSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(10));
            if (Tank.UseSideAvailSchedNum == 0) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(10) + " = " + state.dataIPShortCut->cAlphaArgs(10));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Schedule was not found.");
                ErrorsFound = true;
            }
        }

        if (Tank.UseSide.loopSideNum == DataPlant::DemandSide && Tank.SourceInletNode != 0) {
            PlantUtilities::RegisterPlantCompDesignFlow(state, Tank.SourceInletNode, Tank.SourceDesignVolFlowRate);
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(13)) {
            Tank.SourceSideAvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            Tank.SourceSideAvailSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(13));
            if (Tank.SourceSideAvailSchedNum == 0) {
                ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(13) + " = " + state.dataIPShortCut->cAlphaArgs(13));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Schedule was not found.");
                ErrorsFound = true;
            }
        }

        // Validate inlet mode
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(14));
            if (SELECT_CASE_var == "FIXED") {
                Tank.InletMode = InletModeEnum::Fixed;

            } else if (SELECT_CASE_var == "SEEKING") {
                Tank.InletMode = InletModeEnum::Seeking;
            }
        }

        Tank.Nodes = state.dataIPShortCut->rNumericArgs(18);
        Tank.AdditionalCond = state.dataIPShortCut->rNumericArgs(19);

        Tank.AdditionalLossCoeff.allocate(Tank.Nodes);
        Tank.AdditionalLossCoeff = 0.0;
        for (int NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
            if (NumNums > 19 + NodeNum) {
                Tank.AdditionalLossCoeff(NodeNum) = state.dataIPShortCut->rNumericArgs(19 + NodeNum);
            } else {
                break;
            }
        }

        if (NumNums > 19 + Tank.Nodes) {
            ShowWarningError(state,
                             state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                 ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used");
        }

        Tank.SetupStratifiedNodes(state);
    }

    return ErrorsFound;
}

bool GetWaterThermalTankInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher and Brandon Anderson
    //       DATE WRITTEN   May 2000
    //       MODIFIED       R. Raustad, June 2005, added HPWH and desuperheater water heating coils
    //                      B. Griffith, Oct. 2007 extensions for indirect water heaters
    //                      B. Griffith, Feb. 2008 extensions for autosizing water heaters
    //                      BG Mar 2009.  Trap for bad heater height input for stratified water heater CR7718
    //                      B. Shen 12/2014, add air-source variable-speed heat pump water heating

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the water heater, HPWH, and/or desuperheater heating coil input from the input file.

    bool ErrorsFound = false;

    static constexpr std::string_view RoutineName("GetWaterThermalTankInput: ");
    static constexpr std::string_view RoutineNameNoColon("GetWaterThermalTankInput");

    // Make sure refrigeration input is gotten before this input
    RefrigeratedCase::CheckRefrigerationInput(state);

    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        state.dataWaterThermalTanks->numWaterHeaterMixed = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cMixedWHModuleObj);
        state.dataWaterThermalTanks->numWaterHeaterStratified =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cStratifiedWHModuleObj);
        state.dataWaterThermalTanks->numChilledWaterMixed =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cMixedCWTankModuleObj);
        state.dataWaterThermalTanks->numChilledWaterStratified =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cStratifiedCWTankModuleObj);
        state.dataWaterThermalTanks->numWaterThermalTank =
            state.dataWaterThermalTanks->numWaterHeaterMixed + state.dataWaterThermalTanks->numWaterHeaterStratified +
            state.dataWaterThermalTanks->numChilledWaterMixed + state.dataWaterThermalTanks->numChilledWaterStratified;
        state.dataWaterThermalTanks->numHeatPumpWaterHeater =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cHPWHPumpedCondenser) +
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cHPWHWrappedCondenser);
        state.dataWaterThermalTanks->numWaterHeaterDesuperheater =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCoilDesuperheater);

        if (state.dataWaterThermalTanks->numWaterThermalTank > 0) {
            static constexpr fmt::string_view Format_720(
                "! <Water Heater Information>,Type,Name,Volume {{m3}},Maximum Capacity {{W}},Standard Rated Recovery Efficiency, "
                "Standard Rated Energy Factor\n");
            static constexpr fmt::string_view Format_721(
                "! <Heat Pump Water Heater Information>,Type,Name,Volume {{m3}},Maximum Capacity {{W}},Standard Rated Recovery "
                "Efficiency,Standard Rated Energy Factor,\"DX Coil Total Cooling Rate {{W, HPWH Only}}\"\n");
            static constexpr fmt::string_view Format_722("! <Water Heater Stratified Node Information>,Node Number,Height {{m}},Volume {{m3}},Maximum Capacity "
                                             "{{W}},Off-Cycle UA {{W/K}},On-Cycle UA {{W/K}},Number Of Inlets,Number Of Outlets\n");
            static constexpr fmt::string_view Format_725("! <Chilled Water Tank Information>,Type,Name,Volume {{m3}},Use Side Design Flow Rate {{m3/s}}, "
                                             "Source Side Design Flow Rate {{m3/s}}\n");
            static constexpr fmt::string_view Format_726(
                "! <Chilled Water Tank Stratified Node Information>,Node Number,Height {{m}},Volume {{m3}},UA {{W/K}},Number Of "
                "Inlets,Number Of Outlets\n");

            // Write water heater header for EIO
            if ((state.dataWaterThermalTanks->numWaterHeaterMixed > 0) || (state.dataWaterThermalTanks->numWaterHeaterStratified > 0))
                print(state.files.eio, Format_720);
            if (state.dataWaterThermalTanks->numHeatPumpWaterHeater > 0) print(state.files.eio, Format_721);
            if (state.dataWaterThermalTanks->numWaterHeaterStratified > 0) print(state.files.eio, Format_722);
            if (state.dataWaterThermalTanks->numChilledWaterMixed > 0) print(state.files.eio, Format_725);
            if (state.dataWaterThermalTanks->numChilledWaterStratified > 0) print(state.files.eio, Format_726);
        }

        if (state.dataWaterThermalTanks->numWaterThermalTank > 0) {
            state.dataWaterThermalTanks->WaterThermalTank.allocate(state.dataWaterThermalTanks->numWaterThermalTank);
            state.dataWaterThermalTanks->UniqueWaterThermalTankNames.reserve(static_cast<unsigned>(state.dataWaterThermalTanks->numWaterThermalTank));
        }
        if (state.dataWaterThermalTanks->numHeatPumpWaterHeater > 0) {
            state.dataWaterThermalTanks->HPWaterHeater.allocate(state.dataWaterThermalTanks->numHeatPumpWaterHeater);
        }

        if (state.dataWaterThermalTanks->numWaterHeaterDesuperheater > 0) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater.allocate(state.dataWaterThermalTanks->numWaterHeaterDesuperheater);
        }

        // =======   Get Coil:WaterHeating:Desuperheater ======================================================================
        if (state.dataWaterThermalTanks->numWaterHeaterDesuperheater > 0) {
            ErrorsFound |= getDesuperHtrInput(state);
        }

        //  =======   Get HEAT PUMP:WATER HEATER ===============================================================================
        if (state.dataWaterThermalTanks->numHeatPumpWaterHeater > 0) {
            ErrorsFound |= getHPWaterHeaterInput(state);
        }

        //  =======   Get WATER HEATER:MIXED ===================================================================================
        if (state.dataWaterThermalTanks->numWaterHeaterMixed > 0) {
            ErrorsFound |= getWaterHeaterMixedInputs(state);
        }

        //  =======   Get WATER HEATER:STRATIFIED ==============================================================================
        if (state.dataWaterThermalTanks->numWaterHeaterStratified > 0) {
            ErrorsFound |= getWaterHeaterStratifiedInput(state);
        }

        //  =======   Get Chilled Water :MIXED ===================================================================================
        if (state.dataWaterThermalTanks->numChilledWaterMixed > 0) {
            ErrorsFound |= getWaterTankMixedInput(state);
        }

        //  =======   Get 'ThermalStorage:ChilledWater:Stratified' =======================================================
        if (state.dataWaterThermalTanks->numChilledWaterStratified > 0) {
            ErrorsFound |= getWaterTankStratifiedInput(state);
        }

        //   Loop through all desuperheating coils and then search all water heaters for the tank connected to the desuperheating coil
        if (state.dataWaterThermalTanks->numWaterHeaterDesuperheater > 0) {
            state.dataIPShortCut->cCurrentModuleObject = cCoilDesuperheater;
            for (int DesuperheaterNum = 1; DesuperheaterNum <= state.dataWaterThermalTanks->numWaterHeaterDesuperheater; ++DesuperheaterNum) {
                auto &DesuperHtr = state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum);
                for (int WtrHtrNum = 1; WtrHtrNum <= state.dataWaterThermalTanks->numWaterThermalTank; ++WtrHtrNum) {
                    auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(WtrHtrNum);
                    if (!UtilityRoutines::SameString(DesuperHtr.TankName, Tank.Name) || !UtilityRoutines::SameString(DesuperHtr.TankType, Tank.Type))
                        continue;
                    Tank.DesuperheaterNum = DesuperheaterNum;
                    DesuperHtr.WaterHeaterTankNum = WtrHtrNum;
                    DesuperHtr.TankTypeNum = Tank.TypeNum;
                    DesuperHtr.BackupElementCapacity = Tank.MaxCapacity;
                    if (Tank.UseInletNode == 0 && Tank.UseOutletNode == 0) DesuperHtr.StandAlone = true;

                    //         verify Desuperheater/tank source node connections
                    if (DesuperHtr.WaterInletNode != Tank.SourceOutletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + DesuperHtr.Name + ':');
                        ShowContinueError(state, "Desuperheater inlet node name does not match thermal tank source outlet node name.");
                        ShowContinueError(state,
                                          "Desuperheater water inlet and outlet node names = " + DesuperHtr.InletNodeName1 + " and " +
                                              DesuperHtr.OutletNodeName1);
                        ShowContinueError(state,
                                          "Thermal tank source side inlet and outlet node names      = " + Tank.InletNodeName2 + " and " +
                                              Tank.OutletNodeName2);
                        ErrorsFound = true;
                    }

                    if (DesuperHtr.WaterOutletNode != Tank.SourceInletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + DesuperHtr.Name + ':');
                        ShowContinueError(state, "Desuperheater water outlet node name does not match thermal tank source inlet node name.");
                        ShowContinueError(state,
                                          "Desuperheater water inlet and outlet node names = " + DesuperHtr.InletNodeName1 + " and " +
                                              DesuperHtr.OutletNodeName1);
                        ShowContinueError(state,
                                          "Thermal tank source side inlet and outlet node names      = " + Tank.InletNodeName2 + " and " +
                                              Tank.OutletNodeName2);
                        ErrorsFound = true;
                    }
                }

                if (DesuperHtr.WaterHeaterTankNum == 0) {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + DesuperHtr.Name + ':');
                    ShowContinueError(state, " Water heater tank = " + DesuperHtr.TankName + " not found.");
                    ErrorsFound = true;
                }
            }
        }

        // Loop through HPWH's and then search all water heaters for the tank connected to the HPWH
        if (state.dataWaterThermalTanks->numHeatPumpWaterHeater > 0) {

            int const NumPumpedCondenser = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
                state, cHPWHPumpedCondenser); // number of WaterHeater:HeatPump:PumpedCondenser objects
            for (int HPWaterHeaterNum = 1; HPWaterHeaterNum <= state.dataWaterThermalTanks->numHeatPumpWaterHeater; ++HPWaterHeaterNum) {

                // Create reference to current HPWH object in array.
                HeatPumpWaterHeaterData &HPWH = state.dataWaterThermalTanks->HPWaterHeater(HPWaterHeaterNum);
                if (HPWaterHeaterNum <= NumPumpedCondenser) {
                    // Pumped Condenser
                    state.dataIPShortCut->cCurrentModuleObject = cHPWHPumpedCondenser;
                } else {
                    // Wrapped Condenser
                    state.dataIPShortCut->cCurrentModuleObject = cHPWHWrappedCondenser;
                }

                // find the tank associated with the heat pump water heater and change its %TYPE to HEAT PUMP:WATER HEATER
                for (int CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= state.dataWaterThermalTanks->numWaterThermalTank; ++CheckWaterHeaterNum) {

                    auto &Tank = state.dataWaterThermalTanks->WaterThermalTank(CheckWaterHeaterNum);

                    if (!(UtilityRoutines::SameString(HPWH.TankName, Tank.Name) && UtilityRoutines::SameString(HPWH.TankType, Tank.Type))) continue;

                    // save backup element and on/off-cycle parasitic properties for use during standard rating procedure
                    HPWH.BackupElementCapacity = Tank.MaxCapacity;
                    HPWH.BackupElementEfficiency = Tank.Efficiency;
                    HPWH.WHOnCycParaLoad = Tank.OnCycParaLoad;
                    HPWH.WHOffCycParaLoad = Tank.OffCycParaLoad;
                    HPWH.WHOnCycParaFracToTank = Tank.OnCycParaFracToTank;
                    HPWH.WHOffCycParaFracToTank = Tank.OffCycParaFracToTank;
                    HPWH.WHPLFCurve = Tank.PLFCurve;

                    if (((Tank.TypeNum == DataPlant::TypeOf_WtrHeaterMixed) && (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped)) ||
                        (Tank.TypeNum == DataPlant::TypeOf_WtrHeaterStratified)) {
                        HPWH.TankType = Tank.Type;
                        HPWH.TankTypeNum = Tank.TypeNum;
                    } else {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError(state, "Invalid water heater tank type = " + Tank.Type);
                        ErrorsFound = true;
                    }

                    // Set up comp set for condenser water side nodes (reverse inlet/outlet for water heater)
                    if (HPWH.bIsIHP) {
                        BranchNodeConnections::SetUpCompSets(state,
                                                             HPWH.Type,
                                                             HPWH.Name,
                                                             HPWH.DXCoilType,
                                                             HPWH.DXCoilName + " Water Coil",
                                                             HPWH.InletNodeName1,
                                                             HPWH.OutletNodeName1,
                                                             "HPWH To Coil");
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            state, HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName, HPWH.InletNodeName1, HPWH.OutletNodeName1, "HPWH To Coil");
                    }
                    BranchNodeConnections::SetUpCompSets(
                        state, HPWH.Type, HPWH.Name, HPWH.TankType, HPWH.TankName, HPWH.OutletNodeName1, HPWH.InletNodeName1, "HPWH To Tank");

                    // If WaterHeaterMixed: do not allow modulating control for HPWH's (i.e. modulating control usually used for tankless WH's)
                    if ((Tank.TypeNum == DataPlant::TypeOf_WtrHeaterMixed) && (Tank.ControlType == ControlTypeEnum::Modulate)) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError(state, "Heater Control Type for " + Tank.Type + " = " + Tank.Name + " must be CYCLE.");
                        ErrorsFound = true;
                    }

                    Tank.HeatPumpNum = HPWaterHeaterNum;
                    HPWH.WaterHeaterTankNum = CheckWaterHeaterNum;
                    HPWH.FoundTank = true;

                    if (Tank.DesuperheaterNum > 0) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + "and Coil:WaterHeating:Desuperheater = " +
                                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(CheckWaterHeaterNum).Name +
                                            ":  cannot be connected to the same water heater tank = " + Tank.Name);
                    }

                    // check that water heater source side effectiveness is greater than 0
                    if (Tank.SourceEffectiveness <= 0.0) {
                        ShowSevereError(state,
                                        format("{} = {}:  Invalid source side effectiveness for heat pump water heater = {:.3T}",
                                               state.dataIPShortCut->cCurrentModuleObject,
                                               HPWH.Name,
                                               Tank.SourceEffectiveness));
                        ShowContinueError(state, " water heater source effectiveness will default to 1.0 and simulation continues.");
                        Tank.SourceEffectiveness = 1.0;
                    }

                    // Set up the source side nodes for wrapped condensers
                    if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                        if (Tank.SourceInletNode > 0 || Tank.SourceOutletNode > 0) {
                            ShowSevereError(state, Tank.Type + " = " + Tank.Name + " has a source inlet or outlet node specified,");
                            ShowContinueError(
                                state, "but it is attached to " + HPWH.Type + " = " + HPWH.Name + ", which doesn't permit source side connections.");
                            ShowContinueError(state, "Please leave the source side inlet and outlet fields blank.");
                            ErrorsFound = true;
                        } else {
                            Tank.SourceInletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                       HPWH.OutletNodeName1,
                                                                                       ErrorsFound,
                                                                                       Tank.Type,
                                                                                       Tank.Name,
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       2,
                                                                                       DataLoopNode::ObjectIsNotParent);
                            Tank.InletNodeName2 = HPWH.OutletNodeName1;
                            Tank.SourceOutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                        HPWH.InletNodeName1,
                                                                                        ErrorsFound,
                                                                                        Tank.Type,
                                                                                        Tank.Name,
                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        2,
                                                                                        DataLoopNode::ObjectIsNotParent);
                            Tank.OutletNodeName2 = HPWH.InletNodeName1;
                        }

                        // Mark the tank as not stand alone because it is connected now.
                        Tank.StandAlone = false;
                    }

                    // Set HPWH structure variable StandAlone to TRUE if use nodes are not connected
                    if (Tank.UseInletNode == 0 && Tank.UseOutletNode == 0) HPWH.StandAlone = true;

                    if (HPWH.WHUseInletNode != Tank.UseInletNode || HPWH.WHUseOutletNode != Tank.UseOutletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError(state,
                                          "Heat pump water heater tank use side inlet and outlet node names must match the use side inlet and "
                                          "outlet node names for water heater tank = " +
                                              HPWH.TankType + ": " + HPWH.TankName);
                        ShowContinueError(state,
                                          "Heat pump water heater use side inlet and outlet node names = " + HPWH.InletNodeName2 + " and " +
                                              HPWH.OutletNodeName2);
                        ShowContinueError(state,
                                          "Water heater tank use side inlet and outlet node names      = " + Tank.InletNodeName1 + " and " +
                                              Tank.OutletNodeName1);
                        ErrorsFound = true;
                    } else {
                        if (!HPWH.StandAlone) {
                            BranchNodeConnections::TestCompSet(state, HPWH.Type, HPWH.Name, Tank.InletNodeName1, Tank.OutletNodeName1, "Water Nodes");
                        }
                    }

                    // verify HP/tank source node connections
                    if (HPWH.CondWaterInletNode != Tank.SourceOutletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError(state,
                                          "Heat Pump condenser water inlet node name does not match water heater tank source outlet node name.");
                        ShowContinueError(
                            state, "Heat pump condenser water inlet and outlet node names = " + HPWH.InletNodeName1 + " and " + HPWH.OutletNodeName1);
                        ShowContinueError(state,
                                          "Water heater tank source side inlet and outlet node names      = " + Tank.InletNodeName2 + " and " +
                                              Tank.OutletNodeName2);
                        ErrorsFound = true;
                    }

                    if (HPWH.CondWaterOutletNode != Tank.SourceInletNode) {
                        ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError(state,
                                          "Heat Pump condenser water outlet node name does not match water heater tank source inlet node name.");
                        ShowContinueError(
                            state, "Heat pump condenser water inlet and outlet node names = " + HPWH.InletNodeName1 + " and " + HPWH.OutletNodeName1);
                        ShowContinueError(state,
                                          "Water heater tank source side inlet and outlet node names      = " + Tank.InletNodeName2 + " and " +
                                              Tank.OutletNodeName2);
                        ErrorsFound = true;
                    }

                    // verify wrapped condenser location
                    if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                        // make sure the top of the condenser is not above the tank height.
                        if (HPWH.WrappedCondenserTopLocation > Tank.Height) {
                            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError(state, "The height of the top of the wrapped condenser is greater than the height of the tank.");
                            ErrorsFound = true;
                        }
                    }

                    // Verify tank name is in a zone equipment list if HPWH Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
                    if (HPWH.InletAirConfiguration == AmbientTempEnum::TempZone || HPWH.InletAirConfiguration == AmbientTempEnum::ZoneAndOA) {
                        if (allocated(state.dataZoneEquip->ZoneEquipConfig) && allocated(state.dataZoneEquip->ZoneEquipList)) {
                            bool FoundTankInList = false;
                            bool TankNotLowestPriority = false;
                            for (int ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= state.dataGlobal->NumOfZones; ++ZoneEquipConfigNum) {
                                if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != HPWH.AmbientTempZone) continue;
                                if (ZoneEquipConfigNum <= state.dataGlobal->NumOfZones) {
                                    for (int ZoneEquipListNum = 1; ZoneEquipListNum <= state.dataGlobal->NumOfZones; ++ZoneEquipListNum) {
                                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).EquipListName !=
                                            state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).Name)
                                            continue;
                                        int TankCoolingPriority = 0;
                                        int TankHeatingPriority = 0;
                                        if (ZoneEquipConfigNum <= state.dataGlobal->NumOfZones) {
                                            for (int EquipmentTypeNum = 1;
                                                 EquipmentTypeNum <= state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).NumOfEquipTypes;
                                                 ++EquipmentTypeNum) {
                                                if (state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).EquipName(EquipmentTypeNum) != HPWH.Name)
                                                    continue;
                                                FoundTankInList = true;
                                                TankCoolingPriority =
                                                    state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).CoolingPriority(EquipmentTypeNum);
                                                TankHeatingPriority =
                                                    state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).HeatingPriority(EquipmentTypeNum);
                                                break;
                                            } // EquipmentTypeNum
                                            if (!FoundTankInList) {
                                                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                                                ShowContinueError(state,
                                                                  "Heat pump water heater type and name must be listed in the correct "
                                                                  "ZoneHVAC:EquipmentList object when Inlet Air Configuration is equal to "
                                                                  "ZoneAirOnly or ZoneAndOutdoorAir.");
                                                ErrorsFound = true;
                                            }
                                            //                     check that tank has lower priority than all other non-HPWH objects in Zone
                                            //                     Equipment List
                                            for (int EquipmentTypeNum = 1;
                                                 EquipmentTypeNum <= state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).NumOfEquipTypes;
                                                 ++EquipmentTypeNum) {
                                                if (UtilityRoutines::SameString(
                                                        state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).EquipType(EquipmentTypeNum),
                                                        state.dataIPShortCut->cCurrentModuleObject))
                                                    continue;
                                                if (TankCoolingPriority >
                                                        state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).CoolingPriority(EquipmentTypeNum) ||
                                                    TankHeatingPriority >
                                                        state.dataZoneEquip->ZoneEquipList(ZoneEquipListNum).HeatingPriority(EquipmentTypeNum)) {
                                                    TankNotLowestPriority = true;
                                                }
                                            } // EquipmentTypeNum
                                            if (TankNotLowestPriority && FoundTankInList) {
                                                ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                                                ShowContinueError(state,
                                                                  "Heat pump water heaters should be simulated first, before other space "
                                                                  "conditioning equipment.");
                                                ShowContinueError(state,
                                                                  "Poor temperature control may result if the Heating/Cooling sequence number is "
                                                                  "not 1 in the ZoneHVAC:EquipmentList.");
                                            }
                                            break;
                                        } // ZoneEquipConfigNum .LE. NumOfZoneEquipLists
                                    }     // ZoneEquipListNum
                                    break;
                                } // ZoneEquipConfigNum .LE. NumOfZones
                            }     // ZoneEquipConfigNum
                        } else {
                            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError(state,
                                              "ZoneHVAC:EquipmentList and ZoneHVAC:EquipmentConnections objects are required when Inlet Air "
                                              "Configuration is either ZoneAirOnly or ZoneAndOutdoorAir.");
                            ErrorsFound = true;
                        } // ALLOCATED
                    }     // InletAirConfiguration

                    if (Tank.TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {

                        // Nodal heat distribution fraction for stratified tank wrapped condensers
                        if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                            if (Tank.Shape == TankShapeEnum::HorizCylinder) {
                                ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ":");
                                ShowContinueError(state, "A wrapped condenser HPWH model should not be used with a horizontal stratified tank.");
                                ShowContinueError(
                                    state, "Ignoring condenser location and distributing heat evenly throughout the tank. Simulation continues.");
                                Real64 const SameFrac = 1.0 / Tank.Nodes;
                                for (int NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
                                    Tank.Node(NodeNum).HPWHWrappedCondenserHeatingFrac = SameFrac;
                                }
                            } else {
                                Real64 H0 = Tank.Height; // height of top of node
                                Real64 H;                // height of bottom of node
                                Real64 SumFrac(0.0);
                                // Get the fraction of each stratified node that is wrapped by the condenser
                                for (int NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
                                    StratifiedNodeData &CurNode = Tank.Node(NodeNum);
                                    if (NodeNum == Tank.Nodes) {
                                        H = 0.0;
                                    } else {
                                        H = H0 - CurNode.Height;
                                    }
                                    if (H < HPWH.WrappedCondenserBottomLocation && H0 > HPWH.WrappedCondenserBottomLocation) {
                                        // The bottom of the condenser starts partway through this node.
                                        CurNode.HPWHWrappedCondenserHeatingFrac = 1.0 - (HPWH.WrappedCondenserBottomLocation - H) / CurNode.Height;
                                    } else if (H >= HPWH.WrappedCondenserBottomLocation && H <= HPWH.WrappedCondenserTopLocation) {
                                        if (H0 > HPWH.WrappedCondenserTopLocation) {
                                            // the top of the condenser ends partway through this node.
                                            CurNode.HPWHWrappedCondenserHeatingFrac = (HPWH.WrappedCondenserTopLocation - H) / CurNode.Height;
                                        } else {
                                            // the entire node is wrapped by the condenser
                                            CurNode.HPWHWrappedCondenserHeatingFrac = 1.0;
                                        }
                                    } else {
                                        CurNode.HPWHWrappedCondenserHeatingFrac = 0.0;
                                    }
                                    SumFrac += CurNode.HPWHWrappedCondenserHeatingFrac;
                                    H0 = H;
                                }
                                // Normalize the fractions so they sum to 1.
                                for (int NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
                                    Tank.Node(NodeNum).HPWHWrappedCondenserHeatingFrac /= SumFrac;
                                }
                            }
                        }

                        // Stratified Tank HPWH control sensor node locations
                        if (HPWH.ControlSensor1Height < 0.0) {
                            // default to heater 1
                            HPWH.ControlSensor1Height = Tank.HeaterHeight1;
                        }
                        if (HPWH.ControlSensor2Height < 0.0) {
                            // default to heater 2
                            HPWH.ControlSensor2Height = Tank.HeaterHeight2;
                        }

                        // Get the vertical tank height depending on the type of tank
                        Real64 TankHeight;
                        if (Tank.Shape == TankShapeEnum::VertCylinder || Tank.Shape == TankShapeEnum::Other) {
                            TankHeight = Tank.Height;
                        } else {
                            assert(Tank.Shape == TankShapeEnum::HorizCylinder);
                            // For horizontal cylinders, the tank "height" is actually the length.
                            // We need to calculate the height.
                            Real64 EndArea = Tank.Volume / Tank.Height;
                            Real64 Radius = std::sqrt(EndArea / DataGlobalConstants::Pi);
                            TankHeight = 2.0 * Radius; // actual vertical height
                        }

                        // Make sure the control sensor locations are in the tank
                        if (HPWH.ControlSensor1Height < 0.0 || HPWH.ControlSensor1Height > TankHeight) {
                            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError(state, "Control Sensor 1 is located outside the tank.");
                            ErrorsFound = true;
                        }
                        if (HPWH.ControlSensor2Height < 0.0 || HPWH.ControlSensor2Height > TankHeight) {
                            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError(state, "Control Sensor 2 is located outside the tank.");
                            ErrorsFound = true;
                        }

                        // Assign the control sensors to the appropriate nodes
                        Real64 H0 = TankHeight;
                        Real64 H;
                        for (int NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
                            StratifiedNodeData const &TankNode = Tank.Node(NodeNum);
                            if (NodeNum == Tank.Nodes) {
                                H = -1.0; // Avoids rounding errors and ensures that anything at height 0.0 goes into the bottom node
                            } else {
                                H = H0 - TankNode.Height;
                            }

                            // Control Sensor 1 Node
                            if (HPWH.ControlSensor1Height <= H0 && HPWH.ControlSensor1Height > H) {
                                HPWH.ControlSensor1Node = NodeNum;
                            }

                            // Control Sensor 2 Node
                            if (HPWH.ControlSensor2Height <= H0 && HPWH.ControlSensor2Height > H) {
                                HPWH.ControlSensor2Node = NodeNum;
                            }

                            H0 = H;
                        }
                    }

                } // DO CheckWaterHeaterNum = 1, NumWaterHeater

                if (!HPWH.FoundTank) {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + " = " + HPWH.Name + ':');
                    ShowContinueError(state, "Water heater tank object not found = " + HPWH.TankType + ", " + HPWH.TankName);
                    ErrorsFound = true;
                }

            } // DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater
        }

        // Get water heater sizing input.
        state.dataIPShortCut->cCurrentModuleObject = "WaterHeater:Sizing";
        state.dataWaterThermalTanks->numWaterHeaterSizing =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataWaterThermalTanks->numWaterHeaterSizing > 0) {

            for (int WHsizingNum = 1; WHsizingNum <= state.dataWaterThermalTanks->numWaterHeaterSizing; ++WHsizingNum) {
                int NumAlphas;
                int NumNums;
                int IOStat;
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                                         WHsizingNum,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNums,
                                                                         IOStat);

                // find which water heater this object is for
                int WaterThermalTankNum =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), state.dataWaterThermalTanks->WaterThermalTank);
                if (WaterThermalTankNum == 0) {
                    // did not match name throw warning.
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + " object name: " + state.dataIPShortCut->cAlphaArgs(1) +
                                        " does not match any of the water heaters defined in the file");
                    ErrorsFound = true;
                    continue;
                } else { // we have a match
                    // store the sizing data in "sizing" nested derived type for the correct water heater

                    if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "PeakDraw")) {
                        state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeEnum::PeakDraw;
                    } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "ResidentialHUD-FHAMinimum")) {
                        state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeEnum::ResidentialMin;
                    } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "PerPerson")) {
                        state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeEnum::PerPerson;
                    } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "PerFloorArea")) {
                        state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeEnum::PerFloorArea;
                    } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "PerUnit")) {
                        state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeEnum::PerUnit;
                    } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "PerSolarCollectorArea")) {
                        state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeEnum::PerSolarColArea;
                    } else {
                        // wrong design mode entered, throw error
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + " object named: " + state.dataIPShortCut->cAlphaArgs(1) +
                                            " contains an incorrect Design Mode of: " + state.dataIPShortCut->cAlphaArgs(2));
                        ErrorsFound = true;
                    }

                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime = state.dataIPShortCut->rNumericArgs(1);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime = state.dataIPShortCut->rNumericArgs(2);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow =
                        state.dataIPShortCut->rNumericArgs(3);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms =
                        int(state.dataIPShortCut->rNumericArgs(4));
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms =
                        int(state.dataIPShortCut->rNumericArgs(5));
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson =
                        state.dataIPShortCut->rNumericArgs(6);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson =
                        state.dataIPShortCut->rNumericArgs(7);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea =
                        state.dataIPShortCut->rNumericArgs(8);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea =
                        state.dataIPShortCut->rNumericArgs(9);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits = state.dataIPShortCut->rNumericArgs(10);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit =
                        state.dataIPShortCut->rNumericArgs(11);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit =
                        state.dataIPShortCut->rNumericArgs(12);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea =
                        state.dataIPShortCut->rNumericArgs(13);
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.HeightAspectRatio =
                        state.dataIPShortCut->rNumericArgs(14);

                    {
                        auto const SELECT_CASE_var(state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode);

                        if (SELECT_CASE_var == SizeEnum::NotSet) {
                            // do nothing, error thrown if design mode not found
                        } else if (SELECT_CASE_var == SizeEnum::PeakDraw) { // need to have entered a reasonable value for TankDrawTime
                            if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime <= 0.0) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", design mode set to Peak Draw but needs a positive value for tank draw time");
                                ErrorsFound = true;
                            }
                            // constrain crazy sizes by limiting to 10 years or 8760*10
                            if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime > 87600.0) {
                                ShowWarningError(state,
                                                 state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                     ",  has input with an unreasonably large Tank Draw Time, more than 10 years");
                                ErrorsFound = true;
                            }
                            // if both volume and demand side flow connections are autosized, must be a good NominalVolForSizingDemandSideFlow
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).UseSide.loopSideNum == DataPlant::DemandSide) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized)) {
                                if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow <=
                                    0.0) {
                                    ShowWarningError(state,
                                                     state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                         " needs a value for Nominal Tank Volume for Autosizing Plant Connections");
                                    ErrorsFound = true;
                                }
                            }
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).SrcSide.loopSideNum == DataPlant::DemandSide) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized)) {
                                if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow <=
                                    0.0) {
                                    ShowWarningError(state,
                                                     state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                         " needs a value for Nominal Tank Volume for Autosizing Plant Connections");
                                    ErrorsFound = true;
                                }
                            }

                        } else if (SELECT_CASE_var == SizeEnum::ResidentialMin) {
                            // it would have to have at least on bedroom and any more than 10 is crazy for this mode
                            if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms < 1) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", mode needs at least one bedroom");
                                ErrorsFound = true;
                            }
                            if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms > 10) {
                                ShowWarningError(state,
                                                 state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                     ", probably has too many bedrooms for the selected design mode");
                            }

                        } else if (SELECT_CASE_var == SizeEnum::PerPerson) {

                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerPerson mode needs positive value input for storage capacity per person");
                                ErrorsFound = true;
                            }

                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerPerson mode needs positive value input for recovery capacity per person");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == SizeEnum::PerFloorArea) {
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerArea mode needs positive value input for storage capacity per floor area");
                                ErrorsFound = true;
                            }
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerArea mode needs positive value input for recovery capacity per floor area");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == SizeEnum::PerUnit) {
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for storage capacity per unit");
                                ErrorsFound = true;
                            }
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for number of units");
                                ErrorsFound = true;
                            }
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for recovery capacity per unit");
                                ErrorsFound = true;
                            }
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits <= 0.0)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for number of units");
                                ErrorsFound = true;
                            }
                        } else if (SELECT_CASE_var == SizeEnum::PerSolarColArea) {
                            if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea <= 0.0)) {
                                ShowSevereError(
                                    state,
                                    state.dataIPShortCut->cCurrentModuleObject + ", named " + state.dataIPShortCut->cAlphaArgs(1) +
                                        ", PerSolarCollectorArea mode needs positive value input for storage capacity per collector area");
                                ErrorsFound = true;
                            }
                        }
                    }

                } // found water heater num okay
            }     // loop over sizing objects

        } // any water heater sizing objects

        // now check that if water heater fields were autosized, that there was also a sizing object for that water heater
        if (state.dataWaterThermalTanks->numWaterThermalTank > 0) {
            for (int WaterThermalTankNum = 1; WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterThermalTank; ++WaterThermalTankNum) {

                if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                    (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == SizeEnum::NotSet)) {
                    ShowWarningError(state,
                                     "Water heater named " + state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Name +
                                         "has tank volume set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                    ErrorsFound = true;
                }
                if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                    (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == SizeEnum::NotSet)) {
                    ShowWarningError(state,
                                     "Water heater named " + state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Name +
                                         "has heater capacity set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                    ErrorsFound = true;
                }
                if ((state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                    (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == SizeEnum::NotSet)) {
                    ShowWarningError(state,
                                     "Water heater named " + state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Name +
                                         "has tank height set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                    ErrorsFound = true;
                }
            }
        }

        //    now do calls to TestCompSet for tanks, depending on nodes and heat pump water heater
        if (state.dataWaterThermalTanks->numWaterThermalTank > 0) {
            for (int WaterThermalTankNum = 1; WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterThermalTank; ++WaterThermalTankNum) {
                if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).UseInletNode > 0 &&
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).UseOutletNode > 0) {
                    if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                        // do nothing, Use nodes are tested for HeatPump:WaterHeater not tank
                    } else {
                        BranchNodeConnections::TestCompSet(state,
                                                           state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Type,
                                                           state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Name,
                                                           state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).InletNodeName1,
                                                           state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).OutletNodeName1,
                                                           "Use Side Water Nodes");
                    }
                }
                if (state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).SourceInletNode > 0 &&
                    state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).SourceOutletNode > 0) {

                    BranchNodeConnections::TestCompSet(state,
                                                       state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Type,
                                                       state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).Name,
                                                       state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).InletNodeName2,
                                                       state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).OutletNodeName2,
                                                       "Source Side Water Nodes");
                }
            }
        }

        if (state.dataWaterThermalTanks->numWaterThermalTank > 0) {
            for (int WaterThermalTankNum = 1; WaterThermalTankNum <= state.dataWaterThermalTanks->numWaterThermalTank; ++WaterThermalTankNum) {

                state.dataWaterThermalTanks->WaterThermalTank(WaterThermalTankNum).setupZoneInternalGains(state);

            } // WaterThermalTankNum
        }
    } // get input flag

    return ErrorsFound;
}

void WaterThermalTankData::setupOutputVars(EnergyPlusData &state)
{
    if ((this->TypeNum == DataPlant::TypeOf_ChilledWaterTankMixed) || (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified)) {
        this->setupChilledWaterTankOutputVars(state);
    } else {
        // moving setupWaterHeaterOutputVars to here causes big table diffs...
        this->setupWaterHeaterOutputVars(state);
    }
    // moving setupZoneInternalGains to here causes math and table diffs...
    // this->setupZoneInternalGains();
}

void WaterThermalTankData::setupChilledWaterTankOutputVars(EnergyPlusData &state)
{

    // CurrentModuleObject='ThermalStorage:ChilledWater:Mixed/ThermalStorage:ChilledWater:Stratified'
    SetupOutputVariable(
        state, "Chilled Water Thermal Storage Tank Temperature", OutputProcessor::Unit::C, this->TankTempAvg, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Chilled Water Thermal Storage Final Tank Temperature", OutputProcessor::Unit::C, this->TankTemp, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Chilled Water Thermal Storage Tank Heat Gain Rate", OutputProcessor::Unit::W, this->LossRate, "System", "Average", this->Name);
    SetupOutputVariable(
        state, "Chilled Water Thermal Storage Tank Heat Gain Energy", OutputProcessor::Unit::J, this->LossEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Use Side Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->UseMassFlowRate,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Use Side Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->UseInletTemp,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Use Side Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->UseOutletTemp,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(
        state, "Chilled Water Thermal Storage Use Side Heat Transfer Rate", OutputProcessor::Unit::W, this->UseRate, "System", "Average", this->Name);
    SetupOutputVariable(
        state, "Chilled Water Thermal Storage Use Side Heat Transfer Energy", OutputProcessor::Unit::J, this->UseEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Source Side Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->SourceMassFlowRate,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Source Side Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->SourceInletTemp,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Source Side Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->SourceOutletTemp,
                        "System",
                        "Average",
                        this->Name);

    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Source Side Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->SourceRate,
                        "System",
                        "Average",
                        this->Name);
    SetupOutputVariable(state,
                        "Chilled Water Thermal Storage Source Side Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->SourceEnergy,
                        "System",
                        "Sum",
                        this->Name);

    if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {

        for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
            SetupOutputVariable(state,
                                format("Chilled Water Thermal Storage Temperature Node {}", NodeNum),
                                OutputProcessor::Unit::C,
                                this->Node(NodeNum).TempAvg,
                                "System",
                                "Average",
                                this->Name);
        }

        for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
            SetupOutputVariable(state,
                                format("Chilled Water Thermal Storage Final Temperature Node {}", NodeNum),
                                OutputProcessor::Unit::C,
                                this->Node(NodeNum).Temp,
                                "System",
                                "Average",
                                this->Name);
        }
    }

    if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {

        for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
            static constexpr fmt::string_view Format_724("Chilled Water Tank Stratified Node Information,{},{:.4T},{:.4T},{:.4T},{},{}\n");

            print(state.files.eio,
                  Format_724,
                  NodeNum,
                  this->Node(NodeNum).Height,
                  this->Node(NodeNum).Volume,
                  this->Node(NodeNum).OffCycLossCoeff,
                  this->Node(NodeNum).Inlets,
                  this->Node(NodeNum).Outlets);
        }
    }
}

void WaterThermalTankData::setupZoneInternalGains(EnergyPlusData &state)
{
    // set up internal gains if tank is in a thermal zone
    if (this->AmbientTempZone > 0) {
        {
            auto const SELECT_CASE_var(this->TypeNum);

            if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                SetupZoneInternalGain(state,
                                      this->AmbientTempZone,
                                      "WaterHeater:Mixed",
                                      this->Name,
                                      DataHeatBalance::IntGainTypeOf_WaterHeaterMixed,
                                      &this->AmbientZoneGain);
            } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                SetupZoneInternalGain(state,
                                      this->AmbientTempZone,
                                      "WaterHeater:Stratified",
                                      this->Name,
                                      DataHeatBalance::IntGainTypeOf_WaterHeaterStratified,
                                      &this->AmbientZoneGain);
            } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankMixed) {
                SetupZoneInternalGain(state,
                                      this->AmbientTempZone,
                                      "ThermalStorage:ChilledWater:Mixed",
                                      this->Name,
                                      DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                      &this->AmbientZoneGain);
            } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankStratified) {
                SetupZoneInternalGain(state,
                                      this->AmbientTempZone,
                                      "ThermalStorage:ChilledWater:Stratified",
                                      this->Name,
                                      DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterStratified,
                                      &this->AmbientZoneGain);
            }
        }
    }
}

void WaterThermalTankData::setupWaterHeaterOutputVars(EnergyPlusData &state)
{

    // Setup report variables for WaterHeater:Mixed
    // CurrentModuleObject='WaterHeater:Mixed'
    SetupOutputVariable(state, "Water Heater Tank Temperature", OutputProcessor::Unit::C, this->TankTempAvg, "System", "Average", this->Name);

    SetupOutputVariable(state, "Water Heater Final Tank Temperature", OutputProcessor::Unit::C, this->TankTemp, "System", "Average", this->Name);

    SetupOutputVariable(state, "Water Heater Heat Loss Rate", OutputProcessor::Unit::W, this->LossRate, "System", "Average", this->Name);
    SetupOutputVariable(state, "Water Heater Heat Loss Energy", OutputProcessor::Unit::J, this->LossEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Water Heater Use Side Mass Flow Rate", OutputProcessor::Unit::kg_s, this->UseMassFlowRate, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Water Heater Use Side Inlet Temperature", OutputProcessor::Unit::C, this->UseInletTemp, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Water Heater Use Side Outlet Temperature", OutputProcessor::Unit::C, this->UseOutletTemp, "System", "Average", this->Name);

    SetupOutputVariable(state, "Water Heater Use Side Heat Transfer Rate", OutputProcessor::Unit::W, this->UseRate, "System", "Average", this->Name);
    SetupOutputVariable(state, "Water Heater Use Side Heat Transfer Energy", OutputProcessor::Unit::J, this->UseEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Water Heater Source Side Mass Flow Rate", OutputProcessor::Unit::kg_s, this->SourceMassFlowRate, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Water Heater Source Side Inlet Temperature", OutputProcessor::Unit::C, this->SourceInletTemp, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Water Heater Source Side Outlet Temperature", OutputProcessor::Unit::C, this->SourceOutletTemp, "System", "Average", this->Name);

    SetupOutputVariable(
        state, "Water Heater Source Side Heat Transfer Rate", OutputProcessor::Unit::W, this->SourceRate, "System", "Average", this->Name);
    SetupOutputVariable(state,
                        "Water Heater Source Side Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->SourceEnergy,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        "PLANTLOOPHEATINGDEMAND",
                        "DHW",
                        this->EndUseSubcategoryName,
                        "Plant");

    SetupOutputVariable(state,
                        "Water Heater Off Cycle Parasitic Tank Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->OffCycParaRateToTank,
                        "System",
                        "Average",
                        this->Name);
    SetupOutputVariable(state,
                        "Water Heater Off Cycle Parasitic Tank Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->OffCycParaEnergyToTank,
                        "System",
                        "Sum",
                        this->Name);

    SetupOutputVariable(state,
                        "Water Heater On Cycle Parasitic Tank Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->OnCycParaRateToTank,
                        "System",
                        "Average",
                        this->Name);
    SetupOutputVariable(state,
                        "Water Heater On Cycle Parasitic Tank Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->OnCycParaEnergyToTank,
                        "System",
                        "Sum",
                        this->Name);

    SetupOutputVariable(
        state, "Water Heater Total Demand Heat Transfer Rate", OutputProcessor::Unit::W, this->TotalDemandRate, "System", "Average", this->Name);
    SetupOutputVariable(
        state, "Water Heater Total Demand Heat Transfer Energy", OutputProcessor::Unit::J, this->TotalDemandEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(state, "Water Heater Heating Rate", OutputProcessor::Unit::W, this->HeaterRate, "System", "Average", this->Name);
    SetupOutputVariable(state, "Water Heater Heating Energy", OutputProcessor::Unit::J, this->HeaterEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Water Heater Unmet Demand Heat Transfer Rate", OutputProcessor::Unit::W, this->UnmetRate, "System", "Average", this->Name);
    SetupOutputVariable(
        state, "Water Heater Unmet Demand Heat Transfer Energy", OutputProcessor::Unit::J, this->UnmetEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(state, "Water Heater Venting Heat Transfer Rate", OutputProcessor::Unit::W, this->VentRate, "System", "Average", this->Name);
    SetupOutputVariable(state, "Water Heater Venting Heat Transfer Energy", OutputProcessor::Unit::J, this->VentEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(
        state, "Water Heater Net Heat Transfer Rate", OutputProcessor::Unit::W, this->NetHeatTransferRate, "System", "Average", this->Name);
    SetupOutputVariable(
        state, "Water Heater Net Heat Transfer Energy", OutputProcessor::Unit::J, this->NetHeatTransferEnergy, "System", "Sum", this->Name);

    SetupOutputVariable(state, "Water Heater Cycle On Count", OutputProcessor::Unit::None, this->CycleOnCount, "System", "Sum", this->Name);
    SetupOutputVariable(state, "Water Heater Runtime Fraction", OutputProcessor::Unit::None, this->RuntimeFraction, "System", "Average", this->Name);
    SetupOutputVariable(state, "Water Heater Part Load Ratio", OutputProcessor::Unit::None, this->PartLoadRatio, "System", "Average", this->Name);

    SetupOutputVariable(state, "Water Heater " + this->FuelType + " Rate", OutputProcessor::Unit::W, this->FuelRate, "System", "Average", this->Name);
    SetupOutputVariable(state,
                        "Water Heater " + this->FuelType + " Energy",
                        OutputProcessor::Unit::J,
                        this->FuelEnergy,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        this->FuelType,
                        "DHW",
                        this->EndUseSubcategoryName,
                        "Plant");

    SetupOutputVariable(state,
                        "Water Heater Off Cycle Parasitic " + this->OffCycParaFuelType + " Rate",
                        OutputProcessor::Unit::W,
                        this->OffCycParaFuelRate,
                        "System",
                        "Average",
                        this->Name);
    SetupOutputVariable(state,
                        "Water Heater Off Cycle Parasitic " + this->OffCycParaFuelType + " Energy",
                        OutputProcessor::Unit::J,
                        this->OffCycParaFuelEnergy,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        this->OffCycParaFuelType,
                        "DHW",
                        this->EndUseSubcategoryName,
                        "Plant");

    SetupOutputVariable(state,
                        "Water Heater On Cycle Parasitic " + this->OnCycParaFuelType + " Rate",
                        OutputProcessor::Unit::W,
                        this->OnCycParaFuelRate,
                        "System",
                        "Average",
                        this->Name);
    SetupOutputVariable(state,
                        "Water Heater On Cycle Parasitic " + this->OnCycParaFuelType + " Energy",
                        OutputProcessor::Unit::J,
                        this->OnCycParaFuelEnergy,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        this->OnCycParaFuelType,
                        "DHW",
                        this->EndUseSubcategoryName,
                        "Plant");

    SetupOutputVariable(
        state, "Water Heater Water Volume Flow Rate", OutputProcessor::Unit::m3_s, this->VolFlowRate, "System", "Average", this->Name);
    SetupOutputVariable(state,
                        "Water Heater Water Volume",
                        OutputProcessor::Unit::m3,
                        this->VolumeConsumed,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        "Water",
                        "DHW",
                        this->EndUseSubcategoryName,
                        "Plant");
    SetupOutputVariable(state,
                        "Water Heater Mains Water Volume",
                        OutputProcessor::Unit::m3,
                        this->VolumeConsumed,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        "MainsWater",
                        "DHW",
                        this->EndUseSubcategoryName,
                        "Plant");

    if (this->HeatPumpNum > 0) {
        // CurrentModuleObject='WaterHeater:HeatPump:PumpedCondenser'
        HeatPumpWaterHeaterData &HPWH = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
        SetupOutputVariable(
            state, "Water Heater Compressor Part Load Ratio", OutputProcessor::Unit::None, HPWH.HeatingPLR, "System", "Average", HPWH.Name);
        SetupOutputVariable(state,
                            "Water Heater Off Cycle Ancillary Electricity Rate",
                            OutputProcessor::Unit::W,
                            HPWH.OffCycParaFuelRate,
                            "System",
                            "Average",
                            HPWH.Name);
        SetupOutputVariable(state,
                            "Water Heater Off Cycle Ancillary Electricity Energy",
                            OutputProcessor::Unit::J,
                            HPWH.OffCycParaFuelEnergy,
                            "System",
                            "Sum",
                            HPWH.Name,
                            _,
                            "Electricity",
                            "DHW",
                            "Water Heater Parasitic",
                            "Plant");
        SetupOutputVariable(state,
                            "Water Heater On Cycle Ancillary Electricity Rate",
                            OutputProcessor::Unit::W,
                            HPWH.OnCycParaFuelRate,
                            "System",
                            "Average",
                            HPWH.Name);
        SetupOutputVariable(state,
                            "Water Heater On Cycle Ancillary Electricity Energy",
                            OutputProcessor::Unit::J,
                            HPWH.OnCycParaFuelEnergy,
                            "System",
                            "Sum",
                            HPWH.Name,
                            _,
                            "Electricity",
                            "DHW",
                            "Water Heater Parasitic",
                            "Plant");
        SetupOutputVariable(
            state, "Water Heater Heat Pump Control Tank Temperature", OutputProcessor::Unit::C, HPWH.ControlTempAvg, "System", "Average", HPWH.Name);
        SetupOutputVariable(state,
                            "Water Heater Heat Pump Control Tank Final Temperature",
                            OutputProcessor::Unit::C,
                            HPWH.ControlTempFinal,
                            "System",
                            "Average",
                            HPWH.Name);
    }

    if (this->DesuperheaterNum > 0) {
        // CurrentModuleObject='Coil:WaterHeating:Desuperheater'
        SetupOutputVariable(state,
                            "Water Heater Part Load Ratio",
                            OutputProcessor::Unit::None,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).DesuperheaterPLR,
                            "System",
                            "Average",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name);
        SetupOutputVariable(state,
                            "Water Heater On Cycle Parasitic Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).OnCycParaFuelRate,
                            "System",
                            "Average",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name);
        SetupOutputVariable(state,
                            "Water Heater On Cycle Parasitic Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).OnCycParaFuelEnergy,
                            "System",
                            "Sum",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name,
                            _,
                            "Electricity",
                            "DHW",
                            "Water Heater Parasitic",
                            "Plant");
        SetupOutputVariable(state,
                            "Water Heater Off Cycle Parasitic Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).OffCycParaFuelRate,
                            "System",
                            "Average",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name);
        SetupOutputVariable(state,
                            "Water Heater Off Cycle Parasitic Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).OffCycParaFuelEnergy,
                            "System",
                            "Sum",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name,
                            _,
                            "Electricity",
                            "DHW",
                            "Water Heater Parasitic",
                            "Plant");
        SetupOutputVariable(state,
                            "Water Heater Heat Reclaim Efficiency Modifier Multiplier",
                            OutputProcessor::Unit::None,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).HEffFTempOutput,
                            "System",
                            "Average",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name);
        SetupOutputVariable(state,
                            "Water Heater Pump Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).PumpPower,
                            "System",
                            "Average",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name);
        SetupOutputVariable(state,
                            "Water Heater Pump Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).PumpEnergy,
                            "System",
                            "Sum",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name,
                            _,
                            "Electricity",
                            "DHW",
                            "Desuperheater Pump",
                            "Plant");
        SetupOutputVariable(state,
                            "Water Heater Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).HeaterRate,
                            "System",
                            "Average",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name);
        SetupOutputVariable(state,
                            "Water Heater Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).HeaterEnergy,
                            "System",
                            "Sum",
                            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Name,
                            _,
                            "EnergyTransfer",
                            "DHW",
                            "Water Heater",
                            "Plant");
    }

    // Setup report variables for WaterHeater:Stratified
    // CurrentModuleObject='WaterHeater:Stratified'
    if (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {

        SetupOutputVariable(
            state, "Water Heater Heater 1 Heating Rate", OutputProcessor::Unit::W, this->HeaterRate1, "System", "Average", this->Name);
        SetupOutputVariable(
            state, "Water Heater Heater 2 Heating Rate", OutputProcessor::Unit::W, this->HeaterRate2, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Heater Heater 1 Heating Energy", OutputProcessor::Unit::J, this->HeaterEnergy1, "System", "Sum", this->Name);
        SetupOutputVariable(
            state, "Water Heater Heater 2 Heating Energy", OutputProcessor::Unit::J, this->HeaterEnergy2, "System", "Sum", this->Name);

        SetupOutputVariable(
            state, "Water Heater Heater 1 Cycle On Count", OutputProcessor::Unit::None, this->CycleOnCount1, "System", "Sum", this->Name);
        SetupOutputVariable(
            state, "Water Heater Heater 2 Cycle On Count", OutputProcessor::Unit::None, this->CycleOnCount2, "System", "Sum", this->Name);

        SetupOutputVariable(
            state, "Water Heater Heater 1 Runtime Fraction", OutputProcessor::Unit::None, this->RuntimeFraction1, "System", "Average", this->Name);
        SetupOutputVariable(
            state, "Water Heater Heater 2 Runtime Fraction", OutputProcessor::Unit::None, this->RuntimeFraction2, "System", "Average", this->Name);

        for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
            SetupOutputVariable(state,
                                format("Water Heater Temperature Node {}", NodeNum),
                                OutputProcessor::Unit::C,
                                this->Node(NodeNum).TempAvg,
                                "System",
                                "Average",
                                this->Name);
        }

        for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
            SetupOutputVariable(state,
                                format("Water Heater Final Temperature Node {}", NodeNum),
                                OutputProcessor::Unit::C,
                                this->Node(NodeNum).Temp,
                                "System",
                                "Average",
                                this->Name);
        }
    }

    if (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {

        for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
            static constexpr fmt::string_view Format_723("Water Heater Stratified Node Information,{},{:.4T},{:.4T},{:.3T},{:.4T},{:.4T},{},{}\n");
            print(state.files.eio,
                  Format_723,
                  NodeNum,
                  this->Node(NodeNum).Height,
                  this->Node(NodeNum).Volume,
                  this->Node(NodeNum).MaxCapacity,
                  this->Node(NodeNum).OffCycLossCoeff,
                  this->Node(NodeNum).OnCycLossCoeff,
                  this->Node(NodeNum).Inlets,
                  this->Node(NodeNum).Outlets);
        }
    }
}

void WaterThermalTankData::ValidatePLFCurve(EnergyPlusData &state, int const CurveIndex, bool &IsValid)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   February 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Validates the Part Load Factor curve by making sure it can never be less than or equal to zero
    // over the domain of Part Load Ratio inputs from 0 to 1.

    // METHODOLOGY EMPLOYED:
    // Currently can only check 0 and 1.  Need changes in CurveManager to be able to check minimums and
    // maximums.

    IsValid = true;

    // Check 0 and 1
    if (CurveManager::CurveValue(state, CurveIndex, 0.0) <= 0) IsValid = false;
    if (CurveManager::CurveValue(state, CurveIndex, 1.0) <= 0) IsValid = false;
}

void WaterThermalTankData::SetupStratifiedNodes(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Sets up node properties based on the tank shape, i.e., vertical cylinder, horizontal cylinder, or other.
    // Node height, skin area, vertical conduction area, and loss coefficients are calculated and assigned.
    // Heating elements, parasitics, and fluid inlet and outlet flows are assigned according to node height.

    // METHODOLOGY EMPLOYED:
    // Tank is divided into nodes of equal mass.  For horizontal cylinders, node heights are calculated using
    // the Newton-Raphson iterative method.  For vertical cylinders and other shapes, the node heights are calculated
    // using basic geometry.

    static constexpr std::string_view RoutineName("GetWaterThermalTankInput");

    const Real64 Tolerance(1.0e-8); // Tolerance for Newton-Raphson solution
    const Real64 FluidCond(0.6);    // Conductivity of water (W/m-K)

    int NumNodes = this->Nodes;
    this->Node.allocate(NumNodes);
    Real64 rho;
    if ((this->UseSide.loopNum > 0) && allocated(state.dataPlnt->PlantLoop)) {
        rho = FluidProperties::GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                DataGlobalConstants::InitConvTemp,
                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                RoutineName);
    } else {
        rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->FluidIndex, RoutineName);
    }

    Real64 NodeMass = this->Volume * rho / NumNodes;
    Real64 TankHeight;

    // Mixing rate set to 50% of the max value for dt = 1.0
    this->InversionMixingRate = NodeMass * 0.5 * 1.0;

    if ((this->Shape == TankShapeEnum::VertCylinder) || (this->Shape == TankShapeEnum::Other)) {
        TankHeight = this->Height;
        Real64 EndArea = this->Volume / TankHeight;
        Real64 NodeHeight = TankHeight / NumNodes;
        Real64 CondCoeff = (FluidCond + this->AdditionalCond) * EndArea / NodeHeight;

        Real64 Perimeter_loc;
        if (this->Shape == TankShapeEnum::VertCylinder) {
            Real64 Radius = std::sqrt(EndArea / DataGlobalConstants::Pi);
            Perimeter_loc = 2.0 * DataGlobalConstants::Pi * Radius;
        } else { // TankShapeOther
            Perimeter_loc = this->Perimeter;
        }

        for (int NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
            this->Node(NodeNum).Mass = NodeMass;
            this->Node(NodeNum).Volume = this->Volume / NumNodes;
            this->Node(NodeNum).Height = NodeHeight;
            this->Node(NodeNum).CondCoeffUp = CondCoeff;
            this->Node(NodeNum).CondCoeffDn = CondCoeff;

            Real64 SkinArea;
            if ((NodeNum == 1) || (NodeNum == NumNodes)) {
                SkinArea = Perimeter_loc * NodeHeight + EndArea;
            } else {
                SkinArea = Perimeter_loc * NodeHeight;
            }

            this->Node(NodeNum).OnCycLossCoeff = this->SkinLossCoeff * SkinArea + this->AdditionalLossCoeff(NodeNum);

            this->Node(NodeNum).OffCycLossCoeff = this->Node(NodeNum).OnCycLossCoeff + this->OffCycFlueLossCoeff;

        } // NodeNum

        this->Node(1).CondCoeffUp = 0.0;
        this->Node(NumNodes).CondCoeffDn = 0.0;

    } else {                              // Tank%Shape == TankShapeHorizCylinder
        Real64 TankLength = this->Height; // Height is the length in the axial direction
        Real64 EndArea = this->Volume / TankLength;
        Real64 Radius = std::sqrt(EndArea / DataGlobalConstants::Pi);
        TankHeight = 2.0 * Radius; // Actual vertical height
        Real64 NodeEndArea = EndArea / NumNodes;

        Real64 R = Radius;
        Real64 H0 = 0.0;
        Real64 ChordLength = 0.0;
        for (int NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
            this->Node(NodeNum).Mass = NodeMass;
            this->Node(NodeNum).Volume = this->Volume / NumNodes;
            Real64 H;
            if (NodeNum == NumNodes) {
                H = TankHeight;
            } else {
                // Use the Newton-Raphson method to solve the nonlinear algebraic equation for node height
                H = H0 + TankHeight / NumNodes; // Initial guess

                while (true) {
                    Real64 a = std::sqrt(H);
                    Real64 b = std::sqrt(2.0 * R - H);
                    Real64 c = 2.0 * R * R * std::atan(a / b) - (2.0 * R * R - 3.0 * H * R + H * H) * (a / b);
                    Real64 c0;
                    if (H0 > 0.0) {
                        Real64 a0 = std::sqrt(H0);
                        Real64 b0 = std::sqrt(2.0 * R - H0);
                        c0 = 2.0 * R * R * std::atan(a0 / b0) - (2.0 * R * R - 3.0 * H0 * R + H0 * H0) * (a0 / b0);
                    } else {
                        c0 = 0.0;
                    }

                    Real64 ApproxEndArea = c - c0;          // Area approximated by iteration
                    Real64 G = ApproxEndArea - NodeEndArea; // G is the function that should converge to zero

                    if (std::abs(G) < Tolerance) {
                        break; // Converged !!!
                    } else {
                        H -= G / (2.0 * a * b); // Calculate next guess:  H = Hprev - G/G'
                    }
                } // Newton-Raphson
            }

            this->Node(NodeNum).Height = H - H0;

            if (NodeNum > 1) {
                Real64 CrossArea = 2.0 * ChordLength * TankLength; // Use old ChordLength from previous node
                Real64 CondCoeff = (FluidCond + this->AdditionalCond) * CrossArea / (0.5 * (H - H0) + 0.5 * this->Node(NodeNum - 1).Height);
                this->Node(NodeNum - 1).CondCoeffUp = CondCoeff; // Set for previous node
                this->Node(NodeNum).CondCoeffDn = CondCoeff;     // Set for this node
            }

            ChordLength = std::sqrt(2.0 * R * H - H * H); // Calc new ChordLength to be used with next node

            Real64 Perimeter_loc = 2.0 * R * (std::acos((R - H) / R) - std::acos((R - H0) / R)); // Segments of circular perimeter
            Real64 SkinArea = Perimeter_loc * TankLength + 2.0 * NodeEndArea;

            this->Node(NodeNum).OnCycLossCoeff = this->SkinLossCoeff * SkinArea + this->AdditionalLossCoeff(NodeNum);

            this->Node(NodeNum).OffCycLossCoeff = this->Node(NodeNum).OnCycLossCoeff + this->OffCycFlueLossCoeff;
            // Although it doesn't make much sense to have a flue in a horizontal tank, keep it in anyway

            H0 = H;
        } // NodeNum

        this->Node(1).CondCoeffUp = 0.0;
        this->Node(NumNodes).CondCoeffDn = 0.0;
    }

    // Loop through nodes again (from top to bottom this time) and assign heating elements, parasitics, flow inlets/outlets
    // according to their vertical heights in the tank
    Real64 H0 = TankHeight;
    for (int NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
        Real64 H;
        if (NodeNum == NumNodes) {
            H = -1.0; // Avoids rounding errors and ensures that anything at height 0.0 goes into the bottom node
        } else {
            H = H0 - this->Node(NodeNum).Height;
        }

        // Assign heater elements to the nodes at the specified heights
        if ((this->HeaterHeight1 <= H0) && (this->HeaterHeight1 > H)) {
            //       sensor node will not get set if user enters 0 for this heater capacity
            //       (Tank%MaxCapacity > 0.0d0)) THEN
            this->HeaterNode1 = NodeNum;
            this->Node(NodeNum).MaxCapacity = this->MaxCapacity;
        }

        if ((this->HeaterHeight2 <= H0) && (this->HeaterHeight2 > H)) {
            //       sensor node will not get set if user enters 0 for this heater capacity
            //      .AND. (Tank%MaxCapacity2 > 0.0d0)) THEN
            this->HeaterNode2 = NodeNum;

            if ((NodeNum == this->HeaterNode1) && (this->ControlType == PriorityEnum::Simultaneous)) {
                this->Node(NodeNum).MaxCapacity += this->MaxCapacity2;
            } else {
                this->Node(NodeNum).MaxCapacity = this->MaxCapacity2;
            }
        }

        // Assign parasitic heat gains to the nodes at the specified heights
        if ((this->OffCycParaHeight <= H0) && (this->OffCycParaHeight > H)) {
            this->Node(NodeNum).OffCycParaLoad = this->OffCycParaFracToTank * this->OffCycParaLoad;
        }

        if ((this->OnCycParaHeight <= H0) && (this->OnCycParaHeight > H)) {
            this->Node(NodeNum).OnCycParaLoad = this->OnCycParaFracToTank * this->OnCycParaLoad;
        }

        // Assign inlets and outlets to the nodes at the specified heights
        if ((this->UseInletHeight <= H0) && (this->UseInletHeight > H)) {
            this->UseInletStratNode = NodeNum;

            if ((this->UseInletNode > 0) || (this->MassFlowRateMax > 0.0)) ++this->Node(NodeNum).Inlets;
        }

        if ((this->UseOutletHeight <= H0) && (this->UseOutletHeight > H)) {
            this->UseOutletStratNode = NodeNum;

            if ((this->UseOutletNode > 0) || (this->MassFlowRateMax > 0.0)) ++this->Node(NodeNum).Outlets;
        }

        if ((this->SourceInletHeight <= H0) && (this->SourceInletHeight > H) && (this->SourceInletNode > 0)) {

            this->SourceInletStratNode = NodeNum;
            ++this->Node(NodeNum).Inlets;
        }

        if ((this->SourceOutletHeight <= H0) && (this->SourceOutletHeight > H) && (this->SourceOutletNode > 0)) {

            this->SourceOutletStratNode = NodeNum;
            ++this->Node(NodeNum).Outlets;
        }

        H0 = H;
    } // NodeNum
}

void WaterThermalTankData::initialize(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   February 2004
    //       MODIFIED       FSEC, July 2005
    //                      Brent Griffith, October 2007 indirect fired water heater
    //                        B. Shen 12/2014, add air-source variable-speed heat pump water heating
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initialize the water heater, heat pump water heater, or desuperheater heating coil objects during the simulation.
    // determine flow rates thru use side and source side plant connections (if any)

    // METHODOLOGY EMPLOYED:
    // Inlet and outlet nodes are initialized.  Scheduled values are retrieved for the current timestep.

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

    static constexpr std::string_view RoutineName("InitWaterThermalTank");
    static constexpr std::string_view GetWaterThermalTankInput("GetWaterThermalTankInput");
    static constexpr std::string_view SizeTankForDemand("SizeTankForDemandSide");

    if (this->scanPlantLoopsFlag && allocated(state.dataPlnt->PlantLoop)) {
        if ((this->UseInletNode > 0) && (this->HeatPumpNum == 0)) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->TypeNum,
                                                    this->UseSide.loopNum,
                                                    this->UseSide.loopSideNum,
                                                    this->UseSide.branchNum,
                                                    this->UseSide.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->UseInletNode,
                                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitWaterThermalTank: Program terminated due to previous condition(s).");
            }
        }
        if ((this->UseInletNode > 0) && (this->HeatPumpNum > 0)) {
            // this is a heat pump water heater, need a separate block because TypeOf_HeatPumpWtrHeater shows up on Branch
            //  (input should probably have been the associated tank )
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Name,
                                                    state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).TypeNum,
                                                    this->UseSide.loopNum,
                                                    this->UseSide.loopSideNum,
                                                    this->UseSide.branchNum,
                                                    this->UseSide.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->UseInletNode,
                                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitWaterThermalTank: Program terminated due to previous condition(s).");
            }
        }
        if ((this->SourceInletNode > 0) && (this->DesuperheaterNum == 0) && (this->HeatPumpNum == 0)) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->TypeNum,
                                                    this->SrcSide.loopNum,
                                                    this->SrcSide.loopSideNum,
                                                    this->SrcSide.branchNum,
                                                    this->SrcSide.compNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->SourceInletNode,
                                                    _);
            if (this->UseInletNode > 0) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->UseSide.loopNum, this->UseSide.loopSideNum, this->SrcSide.loopNum, this->SrcSide.loopSideNum, this->TypeNum, true);
            }
            if (errFlag) {
                ShowFatalError(state, "InitWaterThermalTank: Program terminated due to previous condition(s).");
            }
        }
        this->scanPlantLoopsFlag = false;
    }

    if (this->SetLoopIndexFlag && allocated(state.dataPlnt->PlantLoop)) {
        if ((this->UseInletNode > 0) && (this->HeatPumpNum == 0)) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                           GetWaterThermalTankInput);
            this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
            this->Mass = this->Volume * rho;
            this->UseSidePlantSizNum = state.dataPlnt->PlantLoop(this->UseSide.loopNum).PlantSizNum;
            if ((this->UseDesignVolFlowRateWasAutoSized) && (this->UseSidePlantSizNum == 0)) {
                ShowSevereError(state, "InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " + this->Name);
                ShowFatalError(state, "InitWaterThermalTank: Program terminated due to previous condition(s).");
            }
        }
        if ((this->UseInletNode > 0) && (this->HeatPumpNum > 0)) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                           GetWaterThermalTankInput);
            this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
            this->Mass = this->Volume * rho;
            this->UseSidePlantSizNum = state.dataPlnt->PlantLoop(this->UseSide.loopNum).PlantSizNum;
            if ((this->UseDesignVolFlowRateWasAutoSized) && (this->UseSidePlantSizNum == 0)) {
                ShowSevereError(state, "InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " + this->Name);
                ShowFatalError(state, "InitWaterThermalTank: Program terminated due to previous condition(s).");
            }
        }
        if ((this->SourceInletNode > 0) && (this->DesuperheaterNum == 0) && (this->HeatPumpNum == 0)) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                           GetWaterThermalTankInput);
            this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
            this->SourceSidePlantSizNum = state.dataPlnt->PlantLoop(this->SrcSide.loopNum).PlantSizNum;
            if ((this->SourceDesignVolFlowRateWasAutoSized) && (this->SourceSidePlantSizNum == 0)) {
                ShowSevereError(state,
                                "InitWaterThermalTank: Did not find Sizing:Plant object for source side of plant thermal tank = " + this->Name);
                ShowFatalError(state, "InitWaterThermalTank: Program terminated due to previous condition(s).");
            }
        }
        if (((this->SourceInletNode > 0) && (this->DesuperheaterNum > 0)) || (this->HeatPumpNum > 0)) {
            this->SetLoopIndexFlag = false;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->SetLoopIndexFlag = false;
        if (this->StandAlone) {
            this->SizeStandAloneWaterHeater(state);
            this->SetLoopIndexFlag = false;
        }
    } else if (this->SetLoopIndexFlag && !state.dataGlobal->AnyPlantInModel) {
        if (this->StandAlone) {
            this->SizeStandAloneWaterHeater(state);
        }
        this->SetLoopIndexFlag = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag && !this->SetLoopIndexFlag) {

        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {

            if (this->ControlType == ControlTypeEnum::Cycle) {
                this->MinCapacity = this->MaxCapacity;
            }

            // check for sizing issues that model can not support

            // if stratified tank model, ensure that nominal change over rate is greater than one minute, avoid numerical problems.

            if ((this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) || (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified)) {
                Real64 MaxSideVolFlow = max(this->UseDesignVolFlowRate, this->SourceDesignVolFlowRate);

                if (MaxSideVolFlow > 0.0) { // protect div by zero
                    Real64 TankChangeRateScale = this->Volume / MaxSideVolFlow;
                    if (TankChangeRateScale < 60.0) { // nominal change over in less than one minute
                        ShowSevereError(state, "InitWaterThermalTank: Detected problem for stratified tank model.  Model cannot be applied.");
                        ShowContinueError(state, "Occurs for stratified tank name = " + this->Name);
                        ShowContinueError(state, format("Tank volume = {:.4R} [m3]", this->Volume));
                        ShowContinueError(state, format("Tank use side volume flow rate = {:.4R} [m3/s]", this->UseDesignVolFlowRate));
                        ShowContinueError(state, format("Tank source side volume flow rate = {:.4R} [m3/s]", this->SourceDesignVolFlowRate));
                        ShowContinueError(state, format("Nominal tank change over rate = {:.2R} [s]", TankChangeRateScale));
                        ShowContinueError(
                            state, "Change over rate is too fast, increase tank volume, decrease connection flow rates or use mixed tank model");

                        ShowFatalError(state, "InitWaterThermalTank: Simulation halted because of sizing problem in stratified tank model.");
                    }
                }
            }
        }

        // Clear node initial conditions
        if (this->UseInletNode > 0 && this->UseOutletNode > 0) {
            state.dataLoopNodes->Node(this->UseInletNode).Temp = 0.0;
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                           GetWaterThermalTankInput);
            this->MassFlowRateMin = this->VolFlowRateMin * rho;
            this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
            PlantUtilities::InitComponentNodes(state,
                                               this->MassFlowRateMin,
                                               this->PlantUseMassFlowRateMax,
                                               this->UseInletNode,
                                               this->UseOutletNode,
                                               this->UseSide.loopNum,
                                               this->UseSide.loopSideNum,
                                               this->UseSide.branchNum,
                                               this->UseSide.compNum);
            this->UseOutletTemp = 0.0;
            this->UseMassFlowRate = 0.0;
            this->SavedUseOutletTemp = 0.0;

            this->Mass = this->Volume * rho;
            this->UseBranchControlType = state.dataPlnt->PlantLoop(this->UseSide.loopNum)
                                             .LoopSide(this->UseSide.loopSideNum)
                                             .Branch(this->UseSide.branchNum)
                                             .Comp(this->UseSide.compNum)
                                             .FlowCtrl;
        }

        if ((this->SourceInletNode > 0) && (this->DesuperheaterNum == 0) && (this->HeatPumpNum == 0)) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                           DataGlobalConstants::InitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                           GetWaterThermalTankInput);
            this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->PlantSourceMassFlowRateMax,
                                               this->SourceInletNode,
                                               this->SourceOutletNode,
                                               this->SrcSide.loopNum,
                                               this->SrcSide.loopSideNum,
                                               this->SrcSide.branchNum,
                                               this->SrcSide.compNum);

            this->SourceOutletTemp = 0.0;
            this->SourceMassFlowRate = 0.0;
            this->SavedSourceOutletTemp = 0.0;

            this->SourceBranchControlType = state.dataPlnt->PlantLoop(this->SrcSide.loopNum)
                                                .LoopSide(this->SrcSide.loopSideNum)
                                                .Branch(this->SrcSide.branchNum)
                                                .Comp(this->SrcSide.compNum)
                                                .FlowCtrl;
        }

        if ((this->SourceInletNode > 0) && ((this->DesuperheaterNum > 0) || (this->HeatPumpNum > 0))) {
            state.dataLoopNodes->Node(this->SourceInletNode).Temp = 0.0;
            this->SourceOutletTemp = 0.0;
            this->SourceMassFlowRate = 0.0;
            this->SavedSourceOutletTemp = 0.0;
            Real64 rho =
                FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->FluidIndex, SizeTankForDemand);
            this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
        }

        // Initialize tank temperature to setpoint of first hour of warm up period
        // (use HPWH or Desuperheater heating coil set point if applicable)
        int SchIndex;
        if (this->HeatPumpNum > 0) {
            state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Mode = state.dataWaterThermalTanks->floatMode;
            state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SaveMode = state.dataWaterThermalTanks->floatMode;
            state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SaveWHMode = state.dataWaterThermalTanks->floatMode;
            SchIndex = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTempSchedule;
        } else if (this->DesuperheaterNum > 0) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Mode = state.dataWaterThermalTanks->floatMode;
            SchIndex = state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).SetPointTempSchedule;
        } else {
            SchIndex = this->SetPointTempSchedule;
        }

        if (SchIndex > 0) {
            this->TankTemp = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);
            this->SavedTankTemp = this->TankTemp;

            if (this->Nodes > 0) {
                for (auto &e : this->Node) {
                    e.Temp = e.SavedTemp = this->TankTemp;
                }
            }
        } else {
            this->TankTemp = 20.0;
            this->SavedTankTemp = this->TankTemp;

            if (this->Nodes > 0) {
                for (auto &e : this->Node) {
                    e.Temp = e.SavedTemp = this->TankTemp;
                }
            }
        }
        this->SourceOutletTemp = this->SavedTankTemp;
        this->SavedSourceOutletTemp = this->SavedTankTemp;
        this->UseOutletTemp = this->SavedTankTemp;
        this->SavedUseOutletTemp = this->SavedTankTemp;
        this->TankTempAvg = this->SavedTankTemp;

        this->SavedHeaterOn1 = false;
        this->SavedHeaterOn2 = false;
        this->Mode = state.dataWaterThermalTanks->floatMode;
        this->SavedMode = state.dataWaterThermalTanks->floatMode;
        this->FirstRecoveryDone = false;
        this->FirstRecoveryFuel = 0.0;
        this->UnmetEnergy = 0.0;
        this->LossEnergy = 0.0;
        this->FlueLossEnergy = 0.0;
        this->UseEnergy = 0.0;
        this->TotalDemandEnergy = 0.0;
        this->SourceEnergy = 0.0;
        this->HeaterEnergy = 0.0;
        this->HeaterEnergy1 = 0.0;
        this->HeaterEnergy2 = 0.0;
        this->FuelEnergy = 0.0;
        this->FuelEnergy1 = 0.0;
        this->FuelEnergy2 = 0.0;
        this->VentEnergy = 0.0;
        this->OffCycParaFuelEnergy = 0.0;
        this->OffCycParaEnergyToTank = 0.0;
        this->OnCycParaFuelEnergy = 0.0;
        this->OnCycParaEnergyToTank = 0.0;
        this->NetHeatTransferEnergy = 0.0;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) this->MyEnvrnFlag = true;

    if (this->WarmupFlag && (!state.dataGlobal->WarmupFlag)) {
        // reInitialize tank temperature to setpoint of first hour (use HPWH or Desuperheater heating coil set point if applicable)
        // BG's interpretation here is that its better to reset initial condition to setpoint once warm up is over.
        // (otherwise with a dynamic storage model it is difficult for the user to see the initial performance if it isn't periodic.)
        int SchIndex;
        if (this->HeatPumpNum > 0) {
            state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Mode = state.dataWaterThermalTanks->floatMode;
            SchIndex = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTempSchedule;
        } else if (this->DesuperheaterNum > 0) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).Mode = state.dataWaterThermalTanks->floatMode;
            SchIndex = state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).SetPointTempSchedule;
        } else {
            SchIndex = this->SetPointTempSchedule;
        }

        if (SchIndex > 0) {
            this->TankTemp = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);
            this->SavedTankTemp = this->TankTemp;

            if (this->Nodes > 0) {
                for (auto &e : this->Node) {
                    e.Temp = e.SavedTemp = this->TankTemp;
                }
            }
        } else {
            this->TankTemp = 20.0;
            this->SavedTankTemp = this->TankTemp;

            if (this->Nodes > 0) {
                for (auto &e : this->Node) {
                    e.Temp = e.SavedTemp = this->TankTemp;
                }
            }
        }
        this->SourceOutletTemp = this->SavedTankTemp;
        this->SavedSourceOutletTemp = this->SavedTankTemp;
        this->UseOutletTemp = this->SavedTankTemp;
        this->SavedUseOutletTemp = this->SavedTankTemp;
        this->SavedHeaterOn1 = false;
        this->SavedHeaterOn2 = false;
        this->Mode = 0;
        this->SavedMode = 0;
        this->WarmupFlag = false;
    }
    if (state.dataGlobal->WarmupFlag) this->WarmupFlag = true;

    if (FirstHVACIteration) {
        // Get all scheduled values
        int SchIndex = this->SetPointTempSchedule;
        this->SetPointTemp = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);

        if (!this->IsChilledWaterTank) {
            if (this->SetPointTemp > this->TankTempLimit) {
                // Setpoint temperature scheduled higher than maximum tank temperature limit
                this->SetPointTemp = this->TankTempLimit - 1.0;

                if (this->ShowSetPointWarning) {
                    ShowSevereError(state,
                                    "Water heater = " + this->Name +
                                        ":  Water heater tank set point temperature is greater than the maximum tank temperature limit.");
                    ShowContinueErrorTimeStamp(state,
                                               format("Water heater tank set point temperature is reset to Tank Temperature Limit minus 1 C "
                                                      "({:.2T}) and simulation continues.",
                                                      this->SetPointTemp));
                    this->ShowSetPointWarning = false;
                }
            }
        } else {
            if (this->SetPointTemp < this->TankTempLimit) {
                // Setpoint temperature scheduled lower than minimum tank temperature limit
                this->SetPointTemp = this->TankTempLimit + 1.0;

                if (this->ShowSetPointWarning) {
                    ShowSevereError(state,
                                    "Chilled Water Tank = " + this->Name +
                                        ":  Water heater tank set point temperature is lower than the minimum tank temperature limit.");
                    ShowContinueErrorTimeStamp(state,
                                               format("Chilled water tank set point temperature is reset to Tank Temperature Limit plus 1 C "
                                                      "({:.2T}) and simulation continues.",
                                                      this->SetPointTemp));
                    this->ShowSetPointWarning = false;
                }
            }
        }

        SchIndex = this->SetPointTempSchedule2;
        if (SchIndex > 0) {
            this->SetPointTemp2 = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);
        }

        {
            auto const SELECT_CASE_var(this->AmbientTempIndicator);
            if (SELECT_CASE_var == AmbientTempEnum::Schedule) {
                SchIndex = this->AmbientTempSchedule;
                this->AmbientTemp = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);

            } else if (SELECT_CASE_var == AmbientTempEnum::TempZone) {
                this->AmbientTemp = state.dataHeatBalFanSys->MAT(this->AmbientTempZone);

            } else if (SELECT_CASE_var == AmbientTempEnum::OutsideAir) {
                this->AmbientTemp = state.dataLoopNodes->Node(this->AmbientTempOutsideAirNode).Temp;
            }
        }

        if (this->UseInletNode == 0) { // Stand-alone operation

            SchIndex = this->UseInletTempSchedule;
            if (SchIndex > 0) {
                this->UseInletTemp = ScheduleManager::GetCurrentScheduleValue(state, SchIndex);
            } else {
                this->UseInletTemp = state.dataEnvrn->WaterMainsTemp;
            }

            SchIndex = this->FlowRateSchedule;
            if (SchIndex > 0) {
                this->UseMassFlowRate = ScheduleManager::GetCurrentScheduleValue(state, SchIndex) * this->MassFlowRateMax;

                this->VolFlowRate = this->UseMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            } else {
                this->UseMassFlowRate = this->MassFlowRateMax;
                this->VolFlowRate = this->UseMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            }
        }

        if (this->HeatPumpNum > 0) {
            state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTemp =
                ScheduleManager::GetCurrentScheduleValue(state, state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTempSchedule);
            if (state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTemp >= this->TankTempLimit) {
                // HP setpoint temperature scheduled equal to or higher than tank temperature limit
                state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTemp = this->TankTempLimit - 1.0;

                if (state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).ShowSetPointWarning) {
                    ShowSevereError(
                        state,
                        "Heat Pump Water Heater = " + state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Name +
                            ":  Heat Pump water heater set point temperature is equal to or greater than the maximum tank temperature limit.");
                    ShowContinueErrorTimeStamp(state,
                                               format("Heat Pump water heater tank set point temperature is reset to Tank Temperature Limit "
                                                      "minus 1 C ({:.2T}) and simulation continues.",
                                                      state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).SetPointTemp));
                    state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).ShowSetPointWarning = false;
                }
            }
        }

        if (this->DesuperheaterNum > 0) {
            state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).SetPointTemp = ScheduleManager::GetCurrentScheduleValue(
                state, state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).SetPointTempSchedule);
        }

    } // first HVAC Iteration

    if (this->UseInletNode > 0 && !this->SetLoopIndexFlag) { // setup mass flows for plant connections
        Real64 DeadBandTemp;
        if (this->IsChilledWaterTank) {
            DeadBandTemp = this->SetPointTemp + this->DeadBandDeltaTemp;
        } else {
            DeadBandTemp = this->SetPointTemp - this->DeadBandDeltaTemp;
        }

        Real64 mdotUse = this->PlantMassFlowRatesFunc(state,
                                                      this->UseInletNode,
                                                      FirstHVACIteration,
                                                      SideEnum::Use,
                                                      this->UseSide.loopSideNum,
                                                      this->UseSideSeries,
                                                      this->UseBranchControlType,
                                                      this->SavedUseOutletTemp,
                                                      DeadBandTemp,
                                                      this->SetPointTemp);
        PlantUtilities::SetComponentFlowRate(state,
                                             mdotUse,
                                             this->UseInletNode,
                                             this->UseOutletNode,
                                             this->UseSide.loopNum,
                                             this->UseSide.loopSideNum,
                                             this->UseSide.branchNum,
                                             this->UseSide.compNum);

        this->UseInletTemp = state.dataLoopNodes->Node(this->UseInletNode).Temp;
        this->UseMassFlowRate = mdotUse;
    }

    if (this->SourceInletNode > 0 && !this->SetLoopIndexFlag) { // setup mass flows for plant connections
        Real64 DeadBandTemp;
        if (this->IsChilledWaterTank) {
            DeadBandTemp = this->SetPointTemp + this->DeadBandDeltaTemp;
        } else {
            DeadBandTemp = this->SetPointTemp - this->DeadBandDeltaTemp;
        }

        Real64 sensedTemp;
        if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {
            int tmpNodeNum = this->HeaterNode1;
            sensedTemp = this->Node(tmpNodeNum).SavedTemp;
        } else {
            sensedTemp = this->SavedSourceOutletTemp;
        }

        Real64 mdotSource = this->PlantMassFlowRatesFunc(state,
                                                         this->SourceInletNode,
                                                         FirstHVACIteration,
                                                         SideEnum::Source,
                                                         this->SrcSide.loopSideNum,
                                                         this->SourceSideSeries,
                                                         this->SourceBranchControlType,
                                                         sensedTemp,
                                                         DeadBandTemp,
                                                         this->SetPointTemp);
        if (this->SrcSide.loopNum > 0) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 mdotSource,
                                                 this->SourceInletNode,
                                                 this->SourceOutletNode,
                                                 this->SrcSide.loopNum,
                                                 this->SrcSide.loopSideNum,
                                                 this->SrcSide.branchNum,
                                                 this->SrcSide.compNum);
        } else { // not really plant connected (desuperheater or heat pump)
            state.dataLoopNodes->Node(this->SourceInletNode).MassFlowRate = mdotSource;
            state.dataLoopNodes->Node(this->SourceOutletNode).MassFlowRate = mdotSource;
        }

        this->SourceInletTemp = state.dataLoopNodes->Node(this->SourceInletNode).Temp;
        this->SourceMassFlowRate = mdotSource;
    }

    // initialize HPWHs each iteration
    if (this->HeatPumpNum > 0) {

        int HPNum = this->HeatPumpNum;

        if (this->MyHPSizeFlag) {
            //     autosize info must be calculated in GetWaterThermalTankInputFlag for use in StandardRating procedure
            //       (called at end of GetWaterThermalTankInputFlag)
            //     report autosizing information here (must be done after GetWaterThermalTankInputFlag is complete)
            if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).WaterFlowRateAutoSized &&
                (state.dataPlnt->PlantFirstSizesOkayToReport || !state.dataGlobal->AnyPlantInModel || this->AlreadyRated)) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataWaterThermalTanks->HPWaterHeater(HPNum).Type,
                                             state.dataWaterThermalTanks->HPWaterHeater(HPNum).Name,
                                             "Condenser water flow rate [m3/s]",
                                             state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingWaterFlowRate);
            }
            if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).AirFlowRateAutoSized &&
                (state.dataPlnt->PlantFirstSizesOkayToReport || !state.dataGlobal->AnyPlantInModel || this->AlreadyRated)) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataWaterThermalTanks->HPWaterHeater(HPNum).Type,
                                             state.dataWaterThermalTanks->HPWaterHeater(HPNum).Name,
                                             "Evaporator air flow rate [m3/s]",
                                             state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingAirFlowRate);
            }
            state.dataSize->DataNonZoneNonAirloopValue = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingAirFlowRate;
            state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingAirMassFlowRate =
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingAirFlowRate * state.dataEnvrn->StdRhoAir;
            if (state.dataSize->CurZoneEqNum > 0) {
                ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirFlow = true;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirVolFlow = state.dataSize->DataNonZoneNonAirloopValue;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport || !state.dataGlobal->AnyPlantInModel || this->AlreadyRated) this->MyHPSizeFlag = false;
        }

        int HPAirInletNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirInletNode;
        int HPAirOutletNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirOutletNode;
        int OutdoorAirNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode;
        int ExhaustAirNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).ExhaustAirNode;
        int HPWaterInletNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).CondWaterInletNode;
        int HPWaterOutletNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).CondWaterOutletNode;
        int InletAirMixerNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode;
        int OutletAirSplitterNode = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutletAirSplitterNode;

        {
            auto const SELECT_CASE_var(state.dataWaterThermalTanks->HPWaterHeater(HPNum).CrankcaseTempIndicator);
            if (SELECT_CASE_var == CrankTempEnum::Zone) {
                state.dataHVACGlobal->HPWHCrankcaseDBTemp =
                    state.dataHeatBalFanSys->MAT(state.dataWaterThermalTanks->HPWaterHeater(HPNum).AmbientTempZone);
            } else if (SELECT_CASE_var == CrankTempEnum::Exterior) {
                state.dataHVACGlobal->HPWHCrankcaseDBTemp = state.dataEnvrn->OutDryBulbTemp;
            } else if (SELECT_CASE_var == CrankTempEnum::Schedule) {
                state.dataHVACGlobal->HPWHCrankcaseDBTemp =
                    ScheduleManager::GetCurrentScheduleValue(state, state.dataWaterThermalTanks->HPWaterHeater(HPNum).CrankcaseTempSchedule);
            }
        }

        //   initialize HPWH report variables to 0 and set tank inlet node equal to outlet node
        state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWaterHeaterSensibleCapacity = 0.0;
        state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWaterHeaterLatentCapacity = 0.0;
        this->SourceMassFlowRate = 0.0;
        state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatingPLR = 0.0;
        this->SourceInletTemp = this->SourceOutletTemp;

        //   determine HPWH inlet air conditions based on inlet air configuration (Zone, ZoneAndOA, OutdoorAir, or Schedule)
        Real64 HPInletDryBulbTemp = 0.0;
        Real64 HPInletHumRat = 0.0;
        Real64 HPInletRelHum;
        {
            auto const SELECT_CASE_var(state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirConfiguration);
            if (SELECT_CASE_var == AmbientTempEnum::TempZone) {
                state.dataWaterThermalTanks->mixerInletAirSchedule = 0.0;
                HPInletDryBulbTemp = state.dataLoopNodes->Node(HPAirInletNode).Temp;
                HPInletHumRat = state.dataLoopNodes->Node(HPAirInletNode).HumRat;
            } else if (SELECT_CASE_var == AmbientTempEnum::ZoneAndOA) {
                if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerSchPtr > 0) {
                    //         schedule values are checked for boundary of 0 and 1 in GetWaterThermalTankInputFlag
                    state.dataWaterThermalTanks->mixerInletAirSchedule =
                        ScheduleManager::GetCurrentScheduleValue(state, state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerSchPtr);
                } else {
                    state.dataWaterThermalTanks->mixerInletAirSchedule = 0.0;
                }
                HPInletDryBulbTemp = state.dataWaterThermalTanks->mixerInletAirSchedule * state.dataLoopNodes->Node(OutdoorAirNode).Temp +
                                     (1.0 - state.dataWaterThermalTanks->mixerInletAirSchedule) * state.dataLoopNodes->Node(HPAirInletNode).Temp;
                HPInletHumRat = state.dataWaterThermalTanks->mixerInletAirSchedule * state.dataLoopNodes->Node(OutdoorAirNode).HumRat +
                                (1.0 - state.dataWaterThermalTanks->mixerInletAirSchedule) * state.dataLoopNodes->Node(HPAirInletNode).HumRat;
            } else if (SELECT_CASE_var == AmbientTempEnum::OutsideAir) {
                state.dataWaterThermalTanks->mixerInletAirSchedule = 1.0;
                HPInletDryBulbTemp = state.dataLoopNodes->Node(OutdoorAirNode).Temp;
                HPInletHumRat = state.dataLoopNodes->Node(OutdoorAirNode).HumRat;

            } else if (SELECT_CASE_var == AmbientTempEnum::Schedule) {
                HPInletDryBulbTemp =
                    ScheduleManager::GetCurrentScheduleValue(state, state.dataWaterThermalTanks->HPWaterHeater(HPNum).AmbientTempSchedule);
                HPInletRelHum = ScheduleManager::GetCurrentScheduleValue(state, state.dataWaterThermalTanks->HPWaterHeater(HPNum).AmbientRHSchedule);
                HPInletHumRat = Psychrometrics::PsyWFnTdbRhPb(state, HPInletDryBulbTemp, HPInletRelHum, state.dataEnvrn->OutBaroPress, RoutineName);
                state.dataLoopNodes->Node(HPAirInletNode).Temp = HPInletDryBulbTemp;
                state.dataLoopNodes->Node(HPAirInletNode).HumRat = HPInletHumRat;
                state.dataLoopNodes->Node(HPAirInletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(HPInletDryBulbTemp, HPInletHumRat);
                state.dataLoopNodes->Node(HPAirInletNode).Press = state.dataEnvrn->OutBaroPress;

            } else {
                assert(false);
            }
        }

        state.dataWaterThermalTanks->mdotAir = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingAirMassFlowRate;

        //   set up initial conditions on nodes
        if (InletAirMixerNode > 0) {
            state.dataLoopNodes->Node(InletAirMixerNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(InletAirMixerNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
            state.dataLoopNodes->Node(InletAirMixerNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
            state.dataLoopNodes->Node(InletAirMixerNode).Temp = HPInletDryBulbTemp;
            state.dataLoopNodes->Node(InletAirMixerNode).HumRat = HPInletHumRat;
            state.dataLoopNodes->Node(InletAirMixerNode).Enthalpy = Psychrometrics::PsyHFnTdbW(HPInletDryBulbTemp, HPInletHumRat);
            state.dataLoopNodes->Node(HPAirInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(HPAirOutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ExhaustAirNode).MassFlowRate = 0.0;
        } else {
            if (OutdoorAirNode == 0) {
                state.dataLoopNodes->Node(HPAirInletNode).MassFlowRate = 0.0;
                state.dataLoopNodes->Node(HPAirInletNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
                state.dataLoopNodes->Node(HPAirInletNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
                state.dataLoopNodes->Node(HPAirOutletNode).MassFlowRate = 0.0;
            } else {
                state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRate = 0.0;
                state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
                state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
                state.dataLoopNodes->Node(ExhaustAirNode).MassFlowRate = 0.0;
            }
        }

        if (OutletAirSplitterNode > 0) state.dataLoopNodes->Node(OutletAirSplitterNode).MassFlowRate = 0.0;
        // these are water nodes are not managed by plant. the HP connects
        // directly to the WH without using plant.
        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
            state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(HPWaterOutletNode).MassFlowRate = 0.0;
        }

        //   set the max mass flow rate for outdoor fans
        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax =
            state.dataWaterThermalTanks->mdotAir;

        //   Curve objects in DXCoils::CalcHPWHDXCoil will use inlet conditions to HPWH not inlet air conditions to DX Coil
        //   HPWHInletDBTemp and HPWHInletWBTemp are DataHVACGlobals to pass info to HPWHDXCoil
        state.dataHVACGlobal->HPWHInletDBTemp = HPInletDryBulbTemp;
        state.dataHVACGlobal->HPWHInletWBTemp =
            Psychrometrics::PsyTwbFnTdbWPb(state, state.dataHVACGlobal->HPWHInletDBTemp, HPInletHumRat, state.dataEnvrn->OutBaroPress);

        // initialize flow rates at speed levels for variable-speed HPWH
        if ((state.dataWaterThermalTanks->HPWaterHeater(HPNum).bIsIHP) &&
            (0 == state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed)) // use SCWH coil represents
        {
            IntegratedHeatPump::SizeIHP(state, state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum); //
            // IntegratedHeatPump::SimIHP(modBlankString, HPWaterHeater(HPNum).DXCoilNum,
            //    0, EMP1, EMP2, EMP3, 0, 0.0, 1, 0.0, 0.0, 0.0, false, 0.0); //conduct the sizing operation in the IHP
            int VSCoilID = state.dataIntegratedHP->IntegratedHeatPumps(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
            state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).NumOfSpeeds;

        } else if (UtilityRoutines::SameString(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilType,
                                               "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") &&
                   (state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed == 0)) {
            Real64 EMP1 = 4.0;
            Real64 EMP2 = 0.0;
            Real64 EMP3 = 0.0;
            VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                      std::string(),
                                                      state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                                      0,
                                                      EMP1,
                                                      EMP2,
                                                      EMP3,
                                                      0,
                                                      0.0,
                                                      1,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      0.0); // conduct the sizing operation in the VS WSHP
            int VSCoilID = state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum;
            state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).NumOfSpeeds;
            // below pass the flow rates from the VS coil to the water heater object
        }

        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed > 0) {
            int VSCoilID;
            if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).bIsIHP)
                VSCoilID = state.dataIntegratedHP->IntegratedHeatPumps(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
            else
                VSCoilID = state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum;

            // scale air flow rates
            Real64 MulSpeedFlowScale = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).RatedAirVolFlowRate /
                                       state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedAirVolFlowRate(
                                           state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).NormSpedLevel);
            for (int Iter = 1; Iter <= state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale;
            }

            // check fan flow rate, should be larger than the max flow rate of the VS coil
            Real64 FanVolFlow = 0.0;
            if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                FanVolFlow = state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->designAirVolFlowRate;
            } else if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SimpleOnOff) {
                Fans::GetFanVolFlow(state, state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum, FanVolFlow);
            }

            if (FanVolFlow < state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(
                                 state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed)) { // but this is the not the scaled mas flow
                // if ( FanVolFlow  < HPWaterHeater( HPNum ).HPWHAirVolFlowRate( HPWaterHeater( HPNum ).NumofSpeed ) ) {

                ShowWarningError(state,
                                 format("InitWaterThermalTank: -air flow rate = {:.7T} in fan object  is less than the MSHP system air flow rate "
                                        "when waterheating is required({:.7T}).",
                                        FanVolFlow,
                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(
                                            state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed)));
                ShowContinueError(state,
                                  " The MSHP system flow rate when waterheating is required is reset to the"
                                  " fan flow rate and the simulation continues.");
                ShowContinueError(state, " Occurs in " + state.dataWaterThermalTanks->HPWaterHeater(HPNum).Name);
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed) =
                    FanVolFlow;
                // Check flow rates in other speeds and ensure flow rates are not above the max flow rate
                for (int Iter = state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed - 1; Iter >= 1; --Iter) {
                    if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) >
                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1)) {
                        ShowContinueError(state,
                                          format(" The MSHP system flow rate when waterheating is required is reset to the flow rate at higher "
                                                 "speed and the simulation continues at Speed{}.",
                                                 Iter));
                        ShowContinueError(state, " Occurs in " + state.dataWaterThermalTanks->HPWaterHeater(HPNum).Name);
                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) =
                            state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1);
                    }
                }
            }

            for (int Iter = 1; Iter <= state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).MSAirSpeedRatio(Iter) =
                    state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) /
                    state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(
                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed);
            }

            // scale water flow rates
            MulSpeedFlowScale = state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).RatedWaterVolFlowRate /
                                state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(
                                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).NormSpedLevel);
            for (int Iter = 1; Iter <= state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHWaterVolFlowRate(Iter) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(Iter) * MulSpeedFlowScale;
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHWaterMassFlowRate(Iter) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedWaterMassFlowRate(Iter) * MulSpeedFlowScale;
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).MSWaterSpeedRatio(Iter) =
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(Iter) /
                    state.dataVariableSpeedCoils->VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(
                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed);
            }

            Real64 rhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, HPInletDryBulbTemp, HPInletHumRat);

            for (int Iter = 1; Iter <= state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirMassFlowRate(Iter) =
                    state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) * rhoAir;
            }

            //   set the max mass flow rate for outdoor fans
            state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax =
                state.dataWaterThermalTanks->HPWaterHeater(HPNum).HPWHAirMassFlowRate(state.dataWaterThermalTanks->HPWaterHeater(HPNum).NumofSpeed);
        }

    } //  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN

    // calling CalcStandardRatings early bypasses fan sizing since DataSizing::DataNonZoneNonAirloopValue has not been set yet
    if (!this->AlreadyRated) {
        if (this->IsChilledWaterTank) {
            this->AlreadyRated = true;
        } else {
            if (!state.dataGlobal->AnyPlantInModel || state.dataPlnt->PlantFirstSizesOkayToReport || this->MaxCapacity > 0.0 ||
                this->HeatPumpNum > 0) {
                this->CalcStandardRatings(state);
            }
        }
    }
}

void WaterThermalTankData::CalcWaterThermalTankMixed(EnergyPlusData &state) // Water Heater being simulated
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates a well-mixed, single node water heater tank.

    // METHODOLOGY EMPLOYED:
    // This model uses analytical calculations based on the differential equation describing the tank energy
    // balance.  The model operates in three different modes:  heating, floating, and venting.  Temperatures and
    // energies change dynamically over the timestep.  The final reported tank temperature is the average over
    // the timestep.  The final reported heat rates are averages based on the total energy transfer over the
    // timestep.

    static constexpr std::string_view RoutineName("CalcWaterThermalTankMixed");

    Real64 TimeElapsed_loc =
        state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed;

    if (this->TimeElapsed != TimeElapsed_loc) {
        // The simulation has advanced to the next system DataGlobals::TimeStep.  Save conditions from the end of the previous system
        // DataGlobals::TimeStep for use as the initial conditions of each iteration that does not advance the system DataGlobals::TimeStep.
        this->SavedTankTemp = this->TankTemp;
        this->SavedMode = this->Mode;

        // Save outlet temperatures for demand-side flow control
        this->SavedUseOutletTemp = this->UseOutletTemp;
        this->SavedSourceOutletTemp = this->SourceOutletTemp;

        this->TimeElapsed = TimeElapsed_loc;
    }

    Real64 TankTemp_loc = this->SavedTankTemp;
    int Mode_loc = this->SavedMode;

    Real64 Qmaxcap = this->MaxCapacity;
    Real64 Qmincap = this->MinCapacity;
    Real64 Qoffcycfuel = this->OffCycParaLoad;
    Real64 Qoffcycheat = Qoffcycfuel * this->OffCycParaFracToTank;
    Real64 Qoncycfuel = this->OnCycParaLoad;
    Real64 Qoncycheat = Qoncycfuel * this->OnCycParaFracToTank;

    Real64 SetPointTemp_loc = this->SetPointTemp;
    Real64 DeadBandTemp = this->getDeadBandTemp();
    Real64 MaxTemp = this->TankTempLimit;
    Real64 AmbientTemp_loc = this->AmbientTemp;

    Real64 UseInletTemp_loc = this->UseInletTemp;
    Real64 UseMassFlowRate_loc = this->UseMassFlowRate * this->UseEffectiveness;
    Real64 SourceInletTemp_loc = this->SourceInletTemp;
    Real64 SourceMassFlowRate_loc = this->SourceMassFlowRate * this->SourceEffectiveness;

    Real64 rho;
    if (this->UseSide.loopNum > 0) {
        rho = FluidProperties::GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                TankTemp_loc,
                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                RoutineName);
    } else {
        rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, TankTemp_loc, this->waterIndex, RoutineName);
    }

    Real64 TankMass = rho * this->Volume;

    Real64 Cp;
    if (this->UseSide.loopNum > 0) {
        Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                    TankTemp_loc,
                                                    state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                    RoutineName);
    } else {
        Cp = FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, TankTemp_loc, this->waterIndex, RoutineName);
    }

    Real64 SecInTimeStep = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    Real64 TimeRemaining = SecInTimeStep;
    int CycleOnCount_loc = 0;
    int MaxCycles = SecInTimeStep;
    Real64 Runtime = 0.0;
    bool SetPointRecovered = false;

    Real64 Tsum = 0.0;
    Real64 Eloss = 0.0;
    Real64 Elosszone = 0.0;
    Real64 Euse = 0.0;
    Real64 Esource = 0.0;
    Real64 Eheater = 0.0;
    Real64 Event = 0.0;
    Real64 Eneeded = 0.0;
    Real64 Eunmet = 0.0;
    Real64 Efuel = 0.0;
    Real64 Eoncycfuel = 0.0;
    Real64 Eoffcycfuel = 0.0;
    Real64 PLR = 0.0;
    Real64 PLRsum = 0.0;

    Real64 Qheat = 0.0;
    Real64 Qheater = 0.0;
    Real64 Qvent = 0.0;
    Real64 Qneeded = 0.0;
    Real64 Qunmet = 0.0;
    Real64 Qfuel = 0.0;

    // Calculate the heating rate from the heat pump.
    Real64 HPWHCondenserDeltaT = 0.0;

    if (this->HeatPumpNum > 0) {
        HeatPumpWaterHeaterData const &HeatPump = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
        DataLoopNode::NodeData const &HPWHCondWaterInletNode = state.dataLoopNodes->Node(HeatPump.CondWaterInletNode);
        DataLoopNode::NodeData const &HPWHCondWaterOutletNode = state.dataLoopNodes->Node(HeatPump.CondWaterOutletNode);
        HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
    }
    assert(HPWHCondenserDeltaT >= 0);

    Real64 Qheatpump;
    Real64 Qsource;
    EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcMixedTankSourceSideHeatTransferRate(
        HPWHCondenserDeltaT, SourceInletTemp_loc, Cp, SetPointTemp_loc, SourceMassFlowRate_loc, Qheatpump, Qsource);

    // Calculate steady-state use heat rate.
    Real64 Quse = UseMassFlowRate_loc * Cp * (UseInletTemp_loc - SetPointTemp_loc);

    while (TimeRemaining > 0.0) {

        Real64 TimeNeeded = 0.0;

        Real64 NewTankTemp = TankTemp_loc;
        Real64 LossCoeff_loc = 0.0;
        Real64 LossFracToZone = 0.0;

        {
            auto const SELECT_CASE_var(Mode_loc);

            if (SELECT_CASE_var == state.dataWaterThermalTanks->heatMode) { // Heater is on

                // Calculate heat rate needed to maintain the setpoint at steady-state conditions
                LossCoeff_loc = this->OnCycLossCoeff;
                LossFracToZone = this->OnCycLossFracToZone;
                Real64 Qloss = LossCoeff_loc * (AmbientTemp_loc - SetPointTemp_loc);
                Qneeded = -Quse - Qsource - Qloss - Qoncycheat;

                if (TankTemp_loc > SetPointTemp_loc) {
                    // Heater is not needed after all, possibly due to step change in scheduled SetPointTemp

                    Qheater = 0.0;
                    Qunmet = 0.0;
                    Mode_loc = state.dataWaterThermalTanks->floatMode;
                    continue;

                } else if (TankTemp_loc < SetPointTemp_loc) {
                    // Attempt to recover to the setpoint as quickly as possible by using maximum heater capacity

                    // Qneeded is calculated above
                    // Qneeded does not account for the extra energy needed to recover to the setpoint
                    Qheater = Qmaxcap;
                    Qunmet = max(Qneeded - Qheater, 0.0);
                    Qheat = Qoncycheat + Qheater + Qheatpump;

                    // Calculate time needed to recover to the setpoint at maximum heater capacity
                    TimeNeeded = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTimeNeeded(TankTemp_loc,
                                                                                                     SetPointTemp_loc,
                                                                                                     AmbientTemp_loc,
                                                                                                     UseInletTemp_loc,
                                                                                                     SourceInletTemp_loc,
                                                                                                     TankMass,
                                                                                                     Cp,
                                                                                                     UseMassFlowRate_loc,
                                                                                                     SourceMassFlowRate_loc,
                                                                                                     LossCoeff_loc,
                                                                                                     Qheat);

                    if (TimeNeeded > TimeRemaining) {
                        // Heater is at maximum capacity and heats for all of the remaining time
                        // Setpoint temperature WILL NOT be recovered

                        TimeNeeded = TimeRemaining;

                        NewTankTemp = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp_loc,
                                                                                                        AmbientTemp_loc,
                                                                                                        UseInletTemp_loc,
                                                                                                        SourceInletTemp_loc,
                                                                                                        TankMass,
                                                                                                        Cp,
                                                                                                        UseMassFlowRate_loc,
                                                                                                        SourceMassFlowRate_loc,
                                                                                                        LossCoeff_loc,
                                                                                                        Qheat,
                                                                                                        TimeNeeded);

                    } else { // TimeNeeded <= TimeRemaining
                        // Heater is at maximum capacity but will not heat for all of the remaining time (at maximum anyway)
                        // Setpoint temperature WILL be recovered

                        NewTankTemp = SetPointTemp_loc;

                        SetPointRecovered = true;

                    } // TimeNeeded > TimeRemaining

                } else { // TankTemp == SetPointTemp
                    // Attempt to maintain the setpoint by using the needed heater capacity (modulating, if allowed)

                    if (Qneeded <= 0.0) {
                        // Heater is not needed

                        Qneeded = 0.0;
                        Qheater = 0.0;
                        Qunmet = 0.0;
                        Mode_loc = state.dataWaterThermalTanks->floatMode;
                        continue;

                    } else if (Qneeded < Qmincap) {
                        // Heater is required at less than the minimum capacity
                        // If cycling, Qmincap = Qmaxcap.  Once the setpoint is reached, heater will almost always be shut off here

                        {
                            auto const SELECT_CASE_var1(this->ControlType);

                            if (SELECT_CASE_var1 == ControlTypeEnum::Cycle) {
                                // Control will cycle on and off based on DeadBandTemp
                                Qheater = 0.0;
                                Qunmet = 0.0;
                                Mode_loc = state.dataWaterThermalTanks->floatMode;
                                continue;

                            } else if (SELECT_CASE_var1 == ControlTypeEnum::Modulate) {
                                // Control will cycle on and off based on DeadBandTemp until Qneeded > Qmincap again
                                Qheater = 0.0;
                                Qunmet = Qneeded;
                                Mode_loc = state.dataWaterThermalTanks->floatMode;
                                continue;

                                // CASE (ControlTypeModulateWithOverheat)  ! Not yet implemented
                                // Calculate time to reach steady-state temp; check for venting at MaxTemp limit
                                // Qheater = Qmincap

                                // CASE (ControlTypeModulateWithUnderheat)  ! Not yet implemented
                                // Heater must not come back on until Qneeded >= Qmincap
                                // Mode = modfloatMode
                            }
                        }

                    } else if (Qneeded <= Qmaxcap) {
                        // Heater can exactly meet the needed heat rate (usually by modulating) and heats for all of the remaining time
                        // Setpoint temperature WILL be maintained

                        TimeNeeded = TimeRemaining;

                        Qheater = Qneeded;
                        Qunmet = 0.0;

                        NewTankTemp = SetPointTemp_loc;

                    } else { // Qneeded > Qmaxcap
                        // Heater is at maximum capacity and heats for all of the remaining time
                        // Setpoint temperature WILL NOT be maintained

                        TimeNeeded = TimeRemaining;

                        Qheater = Qmaxcap;
                        Qunmet = Qneeded - Qheater;
                        Qheat = Qoncycheat + Qheater + Qheatpump;

                        NewTankTemp = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp_loc,
                                                                                                        AmbientTemp_loc,
                                                                                                        UseInletTemp_loc,
                                                                                                        SourceInletTemp_loc,
                                                                                                        TankMass,
                                                                                                        Cp,
                                                                                                        UseMassFlowRate_loc,
                                                                                                        SourceMassFlowRate_loc,
                                                                                                        LossCoeff_loc,
                                                                                                        Qheat,
                                                                                                        TimeNeeded);

                    } // Qneeded > Qmaxcap

                } // TankTemp > SetPointTemp

                // Update summed values
                Eneeded += Qneeded * TimeNeeded;
                Eheater += Qheater * TimeNeeded;
                Eunmet += Qunmet * TimeNeeded;
                Eoncycfuel += Qoncycfuel * TimeNeeded;

                if (Qmaxcap > 0.0) PLR = Qheater / Qmaxcap;
                Real64 PLF = this->PartLoadFactor(state, PLR);
                Efuel += Qheater * TimeNeeded / (PLF * this->Efficiency);

                Runtime += TimeNeeded;
                PLRsum += PLR * TimeNeeded;

                if (!this->FirstRecoveryDone) {
                    this->FirstRecoveryFuel += Efuel + Eoffcycfuel + Eoncycfuel;
                    if (SetPointRecovered) this->FirstRecoveryDone = true;
                }

            } else if ((SELECT_CASE_var == state.dataWaterThermalTanks->floatMode) ||
                       (SELECT_CASE_var == state.dataWaterThermalTanks->coolMode)) { // Heater is off

                // Calculate heat rate needed to maintain the setpoint at steady-state conditions
                LossCoeff_loc = this->OffCycLossCoeff;
                LossFracToZone = this->OffCycLossFracToZone;
                Real64 Qloss = LossCoeff_loc * (AmbientTemp_loc - SetPointTemp_loc);
                Qneeded = -Quse - Qsource - Qloss - Qoffcycheat;

                // This section really needs to work differently depending on ControlType
                // CYCLE will look at TankTemp, MODULATE will look at Qneeded

                if ((TankTemp_loc < DeadBandTemp) && (!this->IsChilledWaterTank)) {
                    // Tank temperature is already below the minimum, possibly due to step change in scheduled SetPointTemp

                    Mode_loc = state.dataWaterThermalTanks->heatMode;
                    ++CycleOnCount_loc;
                    continue;

                } else if ((TankTemp_loc >= DeadBandTemp) && (!this->IsChilledWaterTank)) {

                    Qheat = Qoffcycheat + Qheatpump;

                    // Calculate time needed for tank temperature to fall to minimum (setpoint - deadband)
                    TimeNeeded = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTimeNeeded(TankTemp_loc,
                                                                                                     DeadBandTemp,
                                                                                                     AmbientTemp_loc,
                                                                                                     UseInletTemp_loc,
                                                                                                     SourceInletTemp_loc,
                                                                                                     TankMass,
                                                                                                     Cp,
                                                                                                     UseMassFlowRate_loc,
                                                                                                     SourceMassFlowRate_loc,
                                                                                                     LossCoeff_loc,
                                                                                                     Qheat);

                    if (TimeNeeded <= TimeRemaining) {
                        // Heating will be needed in this DataGlobals::TimeStep

                        NewTankTemp = DeadBandTemp;
                        Mode_loc = state.dataWaterThermalTanks->heatMode;
                        ++CycleOnCount_loc;

                    } else { // TimeNeeded > TimeRemaining
                        // Heating will not be needed for all of the remaining time

                        NewTankTemp = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp_loc,
                                                                                                        AmbientTemp_loc,
                                                                                                        UseInletTemp_loc,
                                                                                                        SourceInletTemp_loc,
                                                                                                        TankMass,
                                                                                                        Cp,
                                                                                                        UseMassFlowRate_loc,
                                                                                                        SourceMassFlowRate_loc,
                                                                                                        LossCoeff_loc,
                                                                                                        Qheat,
                                                                                                        TimeRemaining);

                        if ((NewTankTemp < MaxTemp) || (this->IsChilledWaterTank)) {
                            // Neither heating nor venting is needed for all of the remaining time

                            TimeNeeded = TimeRemaining;

                        } else { // NewTankTemp >= MaxTemp
                            // Venting will be needed in this DataGlobals::TimeStep

                            // Calculate time needed for tank temperature to rise to the maximum
                            TimeNeeded = CalcTimeNeeded(TankTemp_loc,
                                                        MaxTemp,
                                                        AmbientTemp_loc,
                                                        UseInletTemp_loc,
                                                        SourceInletTemp_loc,
                                                        TankMass,
                                                        Cp,
                                                        UseMassFlowRate_loc,
                                                        SourceMassFlowRate_loc,
                                                        LossCoeff_loc,
                                                        Qheat);

                            // if limit NewTankTemp >= MaxTemp
                            if (TankTemp_loc >= MaxTemp) {
                                TimeNeeded = TimeRemaining;
                            }
                            NewTankTemp = MaxTemp;
                            Mode_loc = state.dataWaterThermalTanks->ventMode;

                        } // NewTankTemp >= MaxTemp

                    } // TimeNeeded <= TimeRemaining

                } else if ((TankTemp_loc > DeadBandTemp) && (this->IsChilledWaterTank)) {
                    Mode_loc = state.dataWaterThermalTanks->coolMode;
                    Qheat = Qheatpump;

                    NewTankTemp = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp_loc,
                                                                                                    AmbientTemp_loc,
                                                                                                    UseInletTemp_loc,
                                                                                                    SourceInletTemp_loc,
                                                                                                    TankMass,
                                                                                                    Cp,
                                                                                                    UseMassFlowRate_loc,
                                                                                                    SourceMassFlowRate_loc,
                                                                                                    LossCoeff_loc,
                                                                                                    Qheat,
                                                                                                    TimeRemaining);
                    TimeNeeded = TimeRemaining;
                } else if ((TankTemp_loc <= DeadBandTemp) && (this->IsChilledWaterTank)) {

                    if (TankTemp_loc < SetPointTemp_loc) Mode_loc = state.dataWaterThermalTanks->floatMode;

                    Qheat = Qheatpump;

                    NewTankTemp = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp_loc,
                                                                                                    AmbientTemp_loc,
                                                                                                    UseInletTemp_loc,
                                                                                                    SourceInletTemp_loc,
                                                                                                    TankMass,
                                                                                                    Cp,
                                                                                                    UseMassFlowRate_loc,
                                                                                                    SourceMassFlowRate_loc,
                                                                                                    LossCoeff_loc,
                                                                                                    Qheat,
                                                                                                    TimeRemaining);
                    TimeNeeded = TimeRemaining;
                } // TankTemp vs DeadBandTemp for heaters and chilled water tanks

                // Update summed values
                Eneeded += Qneeded * TimeNeeded;
                Eunmet += Qunmet * TimeNeeded; // Qunmet may be propagated thru from the previous iteration
                Eoffcycfuel += Qoffcycfuel * TimeNeeded;

            } else if (SELECT_CASE_var == state.dataWaterThermalTanks->ventMode) { // Excess heat is vented

                LossCoeff_loc = this->OffCycLossCoeff;
                LossFracToZone = this->OffCycLossFracToZone;
                Qheat = Qoffcycheat + Qheatpump;

                NewTankTemp = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTankTemp(TankTemp_loc,
                                                                                                AmbientTemp_loc,
                                                                                                UseInletTemp_loc,
                                                                                                SourceInletTemp_loc,
                                                                                                TankMass,
                                                                                                Cp,
                                                                                                UseMassFlowRate_loc,
                                                                                                SourceMassFlowRate_loc,
                                                                                                LossCoeff_loc,
                                                                                                Qheat,
                                                                                                TimeRemaining);

                if (NewTankTemp < MaxTemp) {
                    // Venting is no longer needed because conditions have changed

                    Mode_loc = state.dataWaterThermalTanks->floatMode;
                    continue;

                } else { // NewTankTemp >= MaxTemp

                    TimeNeeded = TimeRemaining;

                    // Calculate the steady-state venting rate needed to maintain the tank at maximum temperature
                    Real64 Qloss = LossCoeff_loc * (AmbientTemp_loc - MaxTemp);
                    Quse = UseMassFlowRate_loc * Cp * (UseInletTemp_loc - MaxTemp);
                    Qsource = SourceMassFlowRate_loc * Cp * (SourceInletTemp_loc - MaxTemp);
                    Qvent = -Quse - Qsource - Qloss - Qoffcycheat;

                    NewTankTemp = MaxTemp;

                } // NewTankTemp < MaxTemp

                // Update summed values
                Event += Qvent * TimeNeeded;
                Eoffcycfuel += Qoffcycfuel * TimeNeeded;

            } else {
                // No default
                assert(false);
            }
        }

        Real64 deltaTsum = EnergyPlus::WaterThermalTanks::WaterThermalTankData::CalcTempIntegral(TankTemp_loc,
                                                                                                 NewTankTemp,
                                                                                                 AmbientTemp_loc,
                                                                                                 UseInletTemp_loc,
                                                                                                 SourceInletTemp_loc,
                                                                                                 TankMass,
                                                                                                 Cp,
                                                                                                 UseMassFlowRate_loc,
                                                                                                 SourceMassFlowRate_loc,
                                                                                                 LossCoeff_loc,
                                                                                                 Qheat,
                                                                                                 TimeNeeded);

        // Update summed values
        Tsum += deltaTsum;
        Eloss += LossCoeff_loc * (AmbientTemp_loc * TimeNeeded - deltaTsum);
        Elosszone += LossFracToZone * LossCoeff_loc * (AmbientTemp_loc * TimeNeeded - deltaTsum);
        Euse += UseMassFlowRate_loc * Cp * (UseInletTemp_loc * TimeNeeded - deltaTsum);
        if (this->HeatPumpNum > 0) {
            Esource += Qheatpump * TimeNeeded;
        } else {
            Esource += SourceMassFlowRate_loc * Cp * (SourceInletTemp_loc * TimeNeeded - deltaTsum);
        }

        TankTemp_loc = NewTankTemp; // Update tank temperature

        TimeRemaining -= TimeNeeded;

        if (CycleOnCount_loc > MaxCycles) {

            if (!state.dataGlobal->WarmupFlag) {
                if (this->MaxCycleErrorIndex == 0) {
                    ShowWarningError(state, "WaterHeater:Mixed = " + this->Name + ":  Heater is cycling on and off more than once per second.");
                    ShowContinueError(state, "Try increasing Deadband Temperature Difference or Tank Volume");
                    ShowContinueErrorTimeStamp(state, "");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "WaterHeater:Mixed = " + this->Name + " Heater is cycling on and off more than once per second:",
                                               this->MaxCycleErrorIndex);
            }

            break;

        } // CycleOnCount > MaxCycles

    } // TimeRemaining > 0.0

    // Calculate average values over the DataGlobals::TimeStep based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
    Real64 TankTempAvg_loc = Tsum / SecInTimeStep;
    Real64 Qloss = Eloss / SecInTimeStep;
    Real64 Qlosszone = Elosszone / SecInTimeStep;
    Quse = Euse / SecInTimeStep;
    Qsource = Esource / SecInTimeStep;
    Qheater = Eheater / SecInTimeStep;
    Qoffcycfuel = Eoffcycfuel / SecInTimeStep;
    Qoffcycheat = Qoffcycfuel * this->OffCycParaFracToTank;
    Qoncycfuel = Eoncycfuel / SecInTimeStep;
    Qoncycheat = Qoncycfuel * this->OnCycParaFracToTank;
    Qvent = Event / SecInTimeStep;
    Qneeded = Eneeded / SecInTimeStep;
    Qunmet = Eunmet / SecInTimeStep;
    Real64 RTF = Runtime / SecInTimeStep;
    PLR = PLRsum / SecInTimeStep;

    if (this->ControlType == ControlTypeEnum::Cycle) {
        // Recalculate Part Load Factor and fuel energy based on Runtime Fraction, instead of Part Load Ratio
        Real64 PLF = this->PartLoadFactor(state, RTF);
        Efuel = Eheater / (PLF * this->Efficiency);
    }

    Qfuel = Efuel / SecInTimeStep;

    this->Mode = Mode_loc;               // Operating mode for carry-over to next DataGlobals::TimeStep
    this->TankTemp = TankTemp_loc;       // Final tank temperature for carry-over to next DataGlobals::TimeStep
    this->TankTempAvg = TankTempAvg_loc; // Average tank temperature over the DataGlobals::TimeStep for reporting

    if (!state.dataGlobal->WarmupFlag) {
        // Warn for potential freezing when avg of final temp over all nodes is below 2°C (nearing 0°C)
        if (this->TankTemp < 2) {
            if (this->FreezingErrorIndex == 0) {
                ShowWarningError(state,
                                 format("{}: {} = '{}':  Temperature of tank < 2C indicates of possibility of freeze. Tank Temperature = {:.2R} C.",
                                        RoutineName,
                                        this->Type,
                                        this->Name,
                                        this->TankTemp));
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           this->Type + " = '" + this->Name + "':  Temperature of tank < 2C indicates of possibility of freeze",
                                           this->FreezingErrorIndex,
                                           this->TankTemp, // Report Max
                                           this->TankTemp, // Report Min
                                           _,              // Don't report Sum
                                           "{C}",          // Max Unit
                                           "{C}");         // Min Unit
        }
    }
    this->UseOutletTemp = TankTempAvg_loc;    // Because entire tank is at same temperature
    this->SourceOutletTemp = TankTempAvg_loc; // Because entire tank is at same temperature
    if (this->HeatPumpNum > 0) {
        this->SourceInletTemp =
            TankTempAvg_loc + HPWHCondenserDeltaT; // Update the source inlet temperature to the average over the DataGlobals::TimeStep
    }

    this->LossRate = Qloss;
    this->UseRate = Quse;
    this->SourceRate = Qsource;
    this->OffCycParaRateToTank = Qoffcycheat;
    this->OnCycParaRateToTank = Qoncycheat;
    this->TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat;
    this->HeaterRate = Qheater;
    this->UnmetRate = Qunmet;
    this->VentRate = Qvent;
    this->NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent;

    this->CycleOnCount = CycleOnCount_loc;
    this->RuntimeFraction = RTF;
    this->PartLoadRatio = PLR;

    this->FuelRate = Qfuel;
    this->OffCycParaFuelRate = Qoffcycfuel;
    this->OnCycParaFuelRate = Qoncycfuel;

    // Add water heater skin losses and venting losses to ambient zone, if specified
    if (this->AmbientTempZone > 0) this->AmbientZoneGain = -Qlosszone - Qvent;
}

void WaterThermalTankData::CalcMixedTankSourceSideHeatTransferRate(
    Real64 HPWHCondenserDeltaT, // input, The temperature difference (C) across the heat pump, zero if
                                // there is no heat pump or if the heat pump is off
    Real64 SourceInletTemp,     // input, Source inlet temperature (C)
    Real64 Cp,                  // Specific heat of fluid (J/kg deltaC)
    Real64 SetPointTemp,        // input, Mixed tank set point temperature
    Real64 &SourceMassFlowRate, // source mass flow rate (kg/s)
    Real64 &Qheatpump,          // heat transfer rate from heat pump
    Real64 &Qsource             // steady state heat transfer rate from a constant temperature source side flow
)
{
    // Function Information:
    //        Author: Noel Merket
    //        Date Written: January 2015
    //        Modified: na
    //        Re-engineered: na

    // Purpose of this function:
    // Determines if the source side heat transfer is coming from a heat pump.
    // If so it treats the source side heat transfer as a constant heat source
    // If it is not coming from a heat pump it treats the source side heat transfer
    // as a constant temperature.

    // Determine if the source side heating is coming from a heat pump.
    Qheatpump = SourceMassFlowRate * Cp * HPWHCondenserDeltaT;
    if (Qheatpump > 0.0) {
        SourceMassFlowRate = 0.0; // Handle this heating as a constant heat source
        Qsource = Qheatpump;
    } else {
        Qsource = SourceMassFlowRate * Cp * (SourceInletTemp - SetPointTemp);
    }
}

Real64 WaterThermalTankData::CalcTimeNeeded(Real64 const Ti, // Initial tank temperature (C)
                                            Real64 const Tf, // Final tank temperature (C)
                                            Real64 const Ta, // Ambient environment temperature (C)
                                            Real64 const T1, // Temperature of flow 1 (C)
                                            Real64 const T2, // Temperature of flow 2 (C)
                                            Real64 const m,  // Mass of tank fluid (kg)
                                            Real64 const Cp, // Specific heat of fluid (J/kg deltaC)
                                            Real64 const m1, // Mass flow rate 1 (kg/s)
                                            Real64 const m2, // Mass flow rate 2 (kg/s)
                                            Real64 const UA, // Heat loss coefficient to ambient environment (W/deltaC)
                                            Real64 const Q   // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   February 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the time needed for the tank temperature to change from Ti to Tf given heat loss,
    // mass flow rates and temperatures, and net heat transfer due to heater and parasitics.

    // METHODOLOGY EMPLOYED:
    // Equations are derived by solving the differential equation governing the tank energy balance.
    // Special cases which cause the natural logarithm to blow up are trapped and interpreted as
    // requiring an infinite amount of time because Tf can never be reached under the given conditions.

    // Return value
    Real64 CalcTimeNeeded;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const Infinity(99999999.9); // A time interval much larger than any single DataGlobals::TimeStep (s)

    Real64 t; // Time elapsed from Ti to Tf (s)

    if (Tf == Ti) {
        // Already at Tf; no time is needed
        t = 0.0;

    } else {
        Real64 a;        // Intermediate variable
        Real64 b;        // Intermediate variable
        Real64 Tm;       // Mixed temperature after an infinite amount of time has passed (C)
        Real64 quotient; // Intermediate variable

        if (UA / Cp + m1 + m2 == 0.0) {

            if (Q == 0.0) {
                // With no mass flow and no heat flow and Tf<>Ti, then Tf can never be reached
                t = Infinity;

            } else {
                a = Q / (m * Cp);

                t = (Tf - Ti) / a;
            }

        } else {
            a = (Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2) / m;
            b = -(UA / Cp + m1 + m2) / m;

            // Calculate the mixed temperature Tm of the tank after an infinite amount of time has passed
            Tm = -a / b;

            if (Tm == Ti) {
                // Mixed temperature is the same as Ti; if Tf<>Ti, then Tf can never be reached
                t = Infinity;

            } else if (Tm == Tf) {
                // Tf only approaches Tm; it can never actually get there in finite time (also avoids divide by zero error)
                t = Infinity;

            } else {
                quotient = (Tf - Tm) / (Ti - Tm);

                if (quotient <= 0.0) { // Autodesk:Num Changed < to <= to elim poss floating point error in LOG call
                    // Tm is in between Ti and Tf; Tf can never be reached
                    t = Infinity;

                } else {
                    t = std::log(quotient) / b;
                }
            }
        }

        if (t < 0.0) t = Infinity; // If negative time, Tf can never be reached in the future
    }

    CalcTimeNeeded = t;

    return CalcTimeNeeded;
}

Real64 WaterThermalTankData::CalcTankTemp(Real64 const Ti, // Initial tank temperature (C)
                                          Real64 const Ta, // Ambient environment temperature (C)
                                          Real64 const T1, // Temperature of flow 1 (C)
                                          Real64 const T2, // Temperature of flow 2 (C)
                                          Real64 const m,  // Mass of tank fluid (kg)
                                          Real64 const Cp, // Specific heat of fluid (J/kg deltaC)
                                          Real64 const m1, // Mass flow rate 1 (kg/s)
                                          Real64 const m2, // Mass flow rate 2 (kg/s)
                                          Real64 const UA, // Heat loss coefficient to ambient environment (W/deltaC)
                                          Real64 const Q,  // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
                                          Real64 const t   // Time elapsed from Ti to Tf (s)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   February 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the final tank temperature Tf after time t has elapsed given heat loss,
    // mass flow rates and temperatures, and net heat transfer due to heater and parasitics.

    // METHODOLOGY EMPLOYED:
    // Equations are derived by solving the differential equation governing the tank energy balance.

    // Return value
    Real64 CalcTankTemp;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 a;  // Intermediate variable
    Real64 b;  // Intermediate variable
    Real64 Tf; // Final tank temperature (C)

    if (UA / Cp + m1 + m2 == 0.0) {
        a = Q / (m * Cp);

        Tf = a * t + Ti;

    } else {
        a = (Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2) / m;
        b = -(UA / Cp + m1 + m2) / m;

        Tf = (a / b + Ti) * std::exp(b * t) - a / b;
    }

    CalcTankTemp = Tf;

    return CalcTankTemp;
}

Real64 WaterThermalTankData::CalcTempIntegral(Real64 const Ti, // Initial tank temperature (C)
                                              Real64 const Tf, // Final tank temperature (C)
                                              Real64 const Ta, // Ambient environment temperature (C)
                                              Real64 const T1, // Temperature of flow 1 (C)
                                              Real64 const T2, // Temperature of flow 2 (C)
                                              Real64 const m,  // Mass of tank fluid (kg)
                                              Real64 const Cp, // Specific heat of fluid (J/kg deltaC)
                                              Real64 const m1, // Mass flow rate 1 (kg/s)
                                              Real64 const m2, // Mass flow rate 2 (kg/s)
                                              Real64 const UA, // Heat loss coefficient to ambient environment (W/deltaC)
                                              Real64 const Q,  // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
                                              Real64 const t   // Time elapsed from Ti to Tf (s)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   February 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the integral of the tank temperature from Ti to Tf.  The integral is added to a sum which is
    // later divided by the elapsed time to yield the average tank temperature over the DataGlobals::TimeStep.

    // METHODOLOGY EMPLOYED:
    // Equations are the mathematical integrals of the governing differential equations.

    // Return value
    Real64 CalcTempIntegral;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 a;     // Intermediate variable
    Real64 b;     // Intermediate variable
    Real64 dTsum; // Integral of tank temperature (C s)

    if (t == 0.0) {
        dTsum = 0.0;

    } else if (Tf == Ti) { // Steady-state conditions
        dTsum = Tf * t;

    } else if (UA / Cp + m1 + m2 == 0.0) {
        a = Q / (m * Cp);

        // Integral of T(t) = a * t + Ti, evaluated from 0 to t
        dTsum = 0.5 * a * t * t + Ti * t;

    } else {
        a = (Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2) / m;
        b = -(UA / Cp + m1 + m2) / m;

        // Integral of T(t) = (a / b + Ti) * EXP(b * t) - a / b, evaluated from 0 to t
        dTsum = (a / b + Ti) * (std::exp(b * t) - 1.0) / b - a * t / b;
    }

    CalcTempIntegral = dTsum;

    return CalcTempIntegral;
}

Real64 WaterThermalTankData::PartLoadFactor(EnergyPlusData &state, Real64 const PartLoadRatio_loc)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the Part Load Factor (PLF) based on a curve correlated to Part Load Ratio, if Heater Control Type
    // is MODULATE, or correlated to Runtime Fraction, if Heater Control Type is CYCLE.

    if (this->PLFCurve > 0) {
        return max(CurveManager::CurveValue(state, this->PLFCurve, PartLoadRatio_loc), 0.1);
    } else {
        return 1.0;
    }
}

void WaterThermalTankData::CalcWaterThermalTankStratified(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Noel Merket, originally by Peter Graham Ellis
    //       DATE WRITTEN   January 2007
    //       MODIFIED       Nov 2011, BAN; modified the use and source outlet temperature calculation
    //       RE-ENGINEERED  Noel Merket, November 2018

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates a stratified, multi-node water heater tank with up to two heating elements.

    // METHODOLOGY EMPLOYED:
    // This model uses a numerical calculation based on an analytical solution of the ODE dT/dt = a*T + b.
    // A heat balance is calculated for each node.
    // Temperatures and energies change dynamically over the system time step.
    // Final node temperatures are reported as final instantaneous values as well as averages over the
    // time step.  Heat transfer rates are averages over the time step.

    static constexpr std::string_view RoutineName("CalcWaterThermalTankStratified");
    const Real64 TemperatureConvergenceCriteria = 0.0001;
    const Real64 SubTimestepMax = 60.0 * 10.0; // seconds
    const Real64 SubTimestepMin = 10.0;        // seconds
    Real64 dt;

    // Tank object reference
    const Real64 &nTankNodes = this->Nodes;

    // Fraction of the current hour that has elapsed (h)
    const Real64 TimeElapsed_loc =
        state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + state.dataHVACGlobal->SysTimeElapsed;

    // Seconds in one DataGlobals::TimeStep (s)
    const Real64 SecInTimeStep = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // Advance tank simulation to the next system DataGlobals::TimeStep, if applicable
    if (this->TimeElapsed != TimeElapsed_loc) {
        // The simulation has advanced to the next system DataGlobals::TimeStep.  Save conditions from the end of the previous system
        // DataGlobals::TimeStep for use as the initial conditions of each iteration that does not advance the system DataGlobals::TimeStep.
        for (auto &e : this->Node)
            e.SavedTemp = e.Temp;

        this->SavedHeaterOn1 = this->HeaterOn1;
        this->SavedHeaterOn2 = this->HeaterOn2;

        // Save outlet temperatures for demand-side flow control
        this->SavedUseOutletTemp = this->UseOutletTemp;
        this->SavedSourceOutletTemp = this->SourceOutletTemp;

        this->TimeElapsed = TimeElapsed_loc;
    }

    // Reset node temperatures to what they were at the beginning of the system DataGlobals::TimeStep.
    for (auto &e : this->Node)
        e.Temp = e.SavedTemp;

    this->HeaterOn1 = this->SavedHeaterOn1;
    this->HeaterOn2 = this->SavedHeaterOn2;

    // Condenser configuration of heat pump water heater
    const int HPWHCondenserConfig = this->HeatPumpNum > 0 ? state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).TypeNum : 0;

    // Heat rate from the heat pump (W)
    const Real64 Qheatpump = [&, this] { // BLB
        if (this->HeatPumpNum == 0) return 0.0;
        HeatPumpWaterHeaterData const &HPWH = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
        Real64 CoilTotalHeatingEnergyRate;
        if (HPWH.NumofSpeed > 0) {
            // VSHPWH
            VariableSpeedCoils::VariableSpeedCoilData const &Coil = state.dataVariableSpeedCoils->VarSpeedCoil(HPWH.DXCoilNum);
            CoilTotalHeatingEnergyRate = Coil.TotalHeatingEnergyRate;
        } else {
            // Single speed HPWH
            DXCoils::DXCoilData const &Coil = state.dataDXCoils->DXCoil(HPWH.DXCoilNum);
            CoilTotalHeatingEnergyRate = Coil.TotalHeatingEnergyRate;
        }
        return CoilTotalHeatingEnergyRate * this->SourceEffectiveness;
    }();

    // Minimum tank temperatures
    const Real64 MinTemp1 = this->SetPointTemp - this->DeadBandDeltaTemp;
    const Real64 MinTemp2 = this->SetPointTemp2 - this->DeadBandDeltaTemp2;

    // Specific Heat of water (J/kg K)
    const Real64 Cp = [&] {
        if (this->UseSide.loopNum > 0) {
            return FluidProperties::GetSpecificHeatGlycol(state,
                                                          state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                          this->TankTemp,
                                                          state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                          RoutineName);
        } else {
            return FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, this->TankTemp, this->waterIndex, RoutineName);
        }
    }();

    Real64 Eloss = 0.0;             // Energy change due to ambient losses over the DataGlobals::TimeStep (J)
    Real64 Euse = 0.0;              // Energy change due to use side mass flow over the DataGlobals::TimeStep (J)
    Real64 Esource = 0.0;           // Energy change due to source side mass flow over the DataGlobals::TimeStep (J)
    Real64 Eheater1 = 0.0;          // Energy change due to heater 1 over the DataGlobals::TimeStep (J)
    Real64 Eheater2 = 0.0;          // Energy change due to heater 2 over the DataGlobals::TimeStep (J)
    Real64 Eunmet = 0.0;            // Energy change unmet over the DataGlobals::TimeStep (J)
    Real64 Event = 0.0;             // Energy change due to venting over the DataGlobals::TimeStep (J)
    int CycleOnCount1_loc = 0;      // Number of times heater 1 cycles on in the current time step
    int CycleOnCount2_loc = 0;      // Number of times heater 2 cycles on in the current time step
    Real64 Runtime = 0.0;           // Time that either heater is running (s)
    Real64 Runtime1 = 0.0;          // Time that heater 1 is running (s)
    Real64 Runtime2 = 0.0;          // Time that heater 2 is running (s)
    bool SetPointRecovered = false; // Flag to indicate when set point is recovered for the first time
    // Added three variables for desuperheater sourceinlet temperature update
    Real64 MdotDesuperheaterWater;        // mass flow rate of desuperheater source side water, kg/s
    Real64 DesuperheaterPLR = 0.0;        // Desuperheater part load ratio
    Real64 DesuperheaterHeaterRate = 0.0; // Desuperheater heater rate (W)
    Real64 SourceInletTempSum = 0.0;      // Sum the source inlet temperature in sub time step to calculate average tempearature
    Real64 Qheater1;                      // Heating rate of burner or electric heating element 1 (W)
    Real64 Qheater2;                      // Heating rate of burner or electric heating element 2 (W)

    if (this->InletMode == InletModeEnum::Fixed) CalcNodeMassFlows(InletModeEnum::Fixed);

    // Time remaining in the current DataGlobals::TimeStep (s)
    Real64 TimeRemaining = SecInTimeStep;

    // Diff Eq. Coefficients for each node
    std::vector<Real64> A;
    A.resize(nTankNodes);
    std::vector<Real64> B;
    B.resize(nTankNodes);

    // Temperature at the end of the internal DataGlobals::TimeStep
    std::vector<Real64> Tfinal;
    Tfinal.resize(nTankNodes);

    // Average temperature of each node over the internal DataGlobals::TimeStep
    std::vector<Real64> Tavg;
    Tavg.resize(nTankNodes);

    int SubTimestepCount = 0;

    while (TimeRemaining > 0.0) {

        ++SubTimestepCount;

        bool PrevHeaterOn1 = this->HeaterOn1;
        bool PrevHeaterOn2 = this->HeaterOn2;

        if (this->InletMode == InletModeEnum::Seeking) CalcNodeMassFlows(InletModeEnum::Seeking);

        // Heater control logic
        if (this->IsChilledWaterTank) {
            // Chilled Water Tank, no heating
            Qheater1 = 0.0;
            Qheater2 = 0.0;
        } else {
            // Control the first heater element (master)
            if (this->MaxCapacity > 0.0) {
                const Real64 &NodeTemp = this->Node(this->HeaterNode1).Temp;

                if (this->HeaterOn1) {
                    if (NodeTemp >= this->SetPointTemp) {
                        this->HeaterOn1 = false;
                        SetPointRecovered = true;
                    }
                } else { // Heater is off
                    if (NodeTemp < MinTemp1) {
                        this->HeaterOn1 = true;
                        ++CycleOnCount1_loc;
                    }
                }
            }

            if (this->HeaterOn1) {
                Qheater1 = this->MaxCapacity;
            } else {
                Qheater1 = 0.0;
            }

            // Control the second heater element (slave)
            if (this->MaxCapacity2 > 0.0) {
                if ((this->ControlType == PriorityEnum::MasterSlave) && this->HeaterOn1) {
                    this->HeaterOn2 = false;

                } else {
                    const Real64 &NodeTemp = this->Node(this->HeaterNode2).Temp;

                    if (this->HeaterOn2) {
                        if (NodeTemp >= this->SetPointTemp2) {
                            this->HeaterOn2 = false;
                            SetPointRecovered = true;
                        }
                    } else { // Heater is off
                        if (NodeTemp < MinTemp2) {
                            this->HeaterOn2 = true;
                            ++CycleOnCount2_loc;
                        }
                    }
                }
            }

            if (this->HeaterOn2) {
                Qheater2 = this->MaxCapacity2;
            } else {
                Qheater2 = 0.0;
            }
        }

        if (SubTimestepCount == 1) {
            dt = SubTimestepMin;
        } else {

            // Set the maximum tank temperature change allowed
            Real64 dT_max = std::numeric_limits<Real64>::max();
            if (this->HeaterOn1) {
                if (this->Node(this->HeaterNode1).Temp < this->SetPointTemp) {
                    // Node temperature is less than setpoint and heater is on
                    dT_max = min(dT_max, this->SetPointTemp - this->Node(this->HeaterNode1).Temp);
                } else {
                    // Node temperature is greater than or equal to setpoint and heater is on
                    // Heater will turn off next time around, calculate assuming that
                    dT_max = min(dT_max, this->Node(this->HeaterNode1).Temp - MinTemp1);
                }
            } else { // Heater off
                if (this->Node(this->HeaterNode1).Temp >= MinTemp1) {
                    // Node temperature is greater than or equal to cut in temperature and heater is off
                    dT_max = min(dT_max, this->Node(this->HeaterNode1).Temp - MinTemp1);
                } else {
                    // Heater will turn on next time around, calculate to setpoint
                    dT_max = min(dT_max, this->SetPointTemp - this->Node(this->HeaterNode1).Temp);
                }
            }
            if (this->HeaterOn2) {
                if (this->Node(this->HeaterNode2).Temp < this->SetPointTemp2) {
                    // Node temperature is less than setpoint and heater is on
                    dT_max = min(dT_max, this->SetPointTemp2 - this->Node(this->HeaterNode2).Temp);
                } else {
                    // Node temperature is greater than or equal to setpoint and heater is on
                    // Heater will turn off next time around, calculate assuming that
                    dT_max = min(dT_max, this->Node(this->HeaterNode2).Temp - MinTemp2);
                }
            } else { // Heater off
                if (this->Node(this->HeaterNode2).Temp >= MinTemp2) {
                    // Node temperature is greater than or equal to cut in temperature and heater is off
                    dT_max = min(dT_max, this->Node(this->HeaterNode2).Temp - MinTemp2);
                } else {
                    // Heater will turn on next time around, calculate to setpoint
                    dT_max = min(dT_max, this->SetPointTemp2 - this->Node(this->HeaterNode2).Temp);
                }
            }

            // Make adjustments to A and B to account for heaters being on or off now
            if (this->HeaterOn1 and !PrevHeaterOn1) {
                // If heater 1 is on now and wasn't before add the heat rate to the B term
                B[this->HeaterNode1 - 1] += Qheater1 / (this->Node(this->HeaterNode1).Mass * Cp);
            } else if (!this->HeaterOn1 and PrevHeaterOn1) {
                // If heater 1 is off now and was on before, remove the heat rate from the B term
                B[this->HeaterNode1 - 1] -= this->MaxCapacity / (this->Node(this->HeaterNode1).Mass * Cp);
            }
            if (this->HeaterOn2 and !PrevHeaterOn2) {
                // If heater 2 is on now and wasn't before add the heat rate to the B term
                B[this->HeaterNode2 - 1] += Qheater2 / (this->Node(this->HeaterNode2).Mass * Cp);
            } else if (!this->HeaterOn2 and PrevHeaterOn2) {
                // If heater 1 is off now and was on before, remove the heat rate from the B term
                B[this->HeaterNode2 - 1] -= this->MaxCapacity / (this->Node(this->HeaterNode2).Mass * Cp);
            }

            if ((this->HeaterOn1 || this->HeaterOn2) and !(PrevHeaterOn1 || PrevHeaterOn2)) {
                // Remove off cycle loads
                // Apply on cycle loads
                for (int i = 0; i < nTankNodes; i++) {
                    auto &node(this->Node[i]);
                    Real64 NodeCapacitance = node.Mass * Cp;
                    A[i] += (node.OffCycLossCoeff - node.OnCycLossCoeff) / NodeCapacitance;
                    B[i] += (-node.OffCycParaLoad + node.OnCycParaLoad + (node.OnCycLossCoeff - node.OffCycLossCoeff) * this->AmbientTemp) /
                            NodeCapacitance;
                }
            } else if (!(this->HeaterOn1 || this->HeaterOn2) and (PrevHeaterOn1 || PrevHeaterOn2)) {
                // Remove on cycle loads
                // Apply off cycle loads
                for (int i = 0; i < nTankNodes; i++) {
                    auto &node(this->Node[i]);
                    Real64 NodeCapacitance = node.Mass * Cp;
                    A[i] -= (node.OffCycLossCoeff - node.OnCycLossCoeff) / NodeCapacitance;
                    B[i] -= (-node.OffCycParaLoad + node.OnCycParaLoad + (node.OnCycLossCoeff - node.OffCycLossCoeff) * this->AmbientTemp) /
                            NodeCapacitance;
                }
            }

            // Set the sub DataGlobals::TimeStep (dt)
            dt = TimeRemaining;
            for (int i = 0; i < nTankNodes; ++i) {
                const Real64 Denominator = fabs(A[i] * Tavg[i] + B[i]);
                if (Denominator != 0.0) dt = min(dt, dT_max / Denominator);
            }
            dt = max(min(SubTimestepMin, TimeRemaining), dt);
            dt = min(SubTimestepMax, dt);
        }

        // Make initial guess that average and final temperatures over the DataGlobals::TimeStep are equal to the starting temperatures
        for (int i = 0; i < nTankNodes; i++) {
            const auto &NodeTemp = this->Node[i].Temp;
            Tfinal[i] = NodeTemp;
            Tavg[i] = NodeTemp;
        }

        for (int ConvergenceCounter = 1; ConvergenceCounter <= 10; ConvergenceCounter++) {

            std::fill(A.begin(), A.end(), 0.0);
            std::fill(B.begin(), B.end(), 0.0);

            // Heater Coefficients
            B[this->HeaterNode1 - 1] += Qheater1;
            B[this->HeaterNode2 - 1] += Qheater2;

            for (int i = 0; i < nTankNodes; i++) {
                const int NodeNum = i + 1;
                const auto &tank_node(this->Node(NodeNum));

                // Parasitic Loads and Losses to Ambient
                if (this->HeaterOn1 || this->HeaterOn2) {
                    // Parasitic Loads
                    B[i] += tank_node.OnCycParaLoad;
                    // Losses to Ambient
                    A[i] += -tank_node.OnCycLossCoeff;
                    B[i] += tank_node.OnCycLossCoeff * this->AmbientTemp;
                } else {
                    // Parasitic Loads
                    B[i] += tank_node.OffCycParaLoad;
                    // Losses to Ambient
                    A[i] += -tank_node.OffCycLossCoeff;
                    B[i] += tank_node.OffCycLossCoeff * this->AmbientTemp;
                }

                // Conduction to adjacent nodes
                A[i] += -(tank_node.CondCoeffDn + tank_node.CondCoeffUp);
                if (NodeNum > 1) B[i] += tank_node.CondCoeffUp * Tavg[i - 1];
                if (NodeNum < nTankNodes) B[i] += tank_node.CondCoeffDn * Tavg[i + 1];

                // Use side plant connection
                const Real64 use_e_mdot_cp = tank_node.UseMassFlowRate * Cp;
                A[i] += -use_e_mdot_cp;
                B[i] += use_e_mdot_cp * this->UseInletTemp;

                // Source side heat transfer rate
                if ((this->HeatPumpNum > 0) && (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterPumped)) {
                    // Pumped Condenser Heat Pump Water Heater
                    if (tank_node.SourceMassFlowRate > 0.0) B[i] += Qheatpump;
                } else {
                    // Source side plant connection (constant temperature)
                    const Real64 src_e_mdot_cp = tank_node.SourceMassFlowRate * Cp;
                    A[i] += -src_e_mdot_cp;
                    B[i] += src_e_mdot_cp * this->SourceInletTemp;
                }

                // Wrapped condenser heat pump water heater
                if ((this->HeatPumpNum > 0) && (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped)) {
                    B[i] += Qheatpump * tank_node.HPWHWrappedCondenserHeatingFrac;
                }

                // Internodal flow
                A[i] += -(tank_node.MassFlowFromUpper + tank_node.MassFlowFromLower) * Cp;
                if (NodeNum > 1) B[i] += tank_node.MassFlowFromUpper * Cp * Tavg[i - 1];
                if (NodeNum < nTankNodes) B[i] += tank_node.MassFlowFromLower * Cp * Tavg[i + 1];

                // Divide by mass and specific heat
                // m * cp * dT/dt = q_net  =>  dT/dt = a * T + b
                A[i] /= tank_node.Mass * Cp;
                B[i] /= tank_node.Mass * Cp;

            } // end for each node

            // Calculate the average and final temperatures over the interval
            Real64 TfinalDiff = 0.0;
            for (int i = 0; i < nTankNodes; ++i) {
                const Real64 Tstart = this->Node[i].Temp;
                const Real64 b_a = B[i] / A[i];
                const Real64 e_a_dt = exp(A[i] * dt);
                Tavg[i] = (Tstart + b_a) * (e_a_dt - 1.0) / (A[i] * dt) - b_a;
                const Real64 Tfinal_old = Tfinal[i];
                Tfinal[i] = (Tstart + b_a) * e_a_dt - b_a;
                TfinalDiff = max(fabs(Tfinal[i] - Tfinal_old), TfinalDiff);
            }

            if (TfinalDiff < TemperatureConvergenceCriteria) break;

            if (this->DesuperheaterNum > 0) {
                DesuperheaterPLR = state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).DesuperheaterPLR;
                DesuperheaterHeaterRate = state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).HeaterRate;
                MdotDesuperheaterWater = state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum).OperatingWaterFlowRate *
                                         Psychrometrics::RhoH2O(Tavg[this->SourceOutletStratNode - 1]);
                if (DesuperheaterPLR > 0.0 && MdotDesuperheaterWater > 0.0) {
                    this->SourceInletTemp =
                        Tavg[this->SourceOutletStratNode - 1] + (DesuperheaterHeaterRate / DesuperheaterPLR) / (MdotDesuperheaterWater * Cp);
                } else {
                    this->SourceInletTemp = Tavg[this->SourceOutletStratNode - 1];
                }
            }
        } // end temperature convergence loop

        // Inversion mixing
        bool HasInversion;
        do {
            HasInversion = false;
            // Starting from the top of the tank check if the node below has a temperature inversion.
            for (int j = 0; j < nTankNodes - 1; ++j) {
                if (Tfinal[j] < Tfinal[j + 1]) {

                    // Temperature inversion!
                    HasInversion = true;

                    // From the node above the inversion, move down calculating a weighted average
                    // of node temperatures until the node below the group of mixed nodes isn't hotter
                    // or we hit the bottom of the tank.
                    Real64 Tmixed = 0.0;
                    Real64 MassMixed = 0.0;
                    int m;
                    for (m = j; m < nTankNodes; ++m) {
                        Tmixed += Tfinal[m] * this->Node[m].Mass;
                        MassMixed += this->Node[m].Mass;
                        if ((m == nTankNodes - 1) or (Tmixed / MassMixed > Tfinal[m + 1])) break;
                    }
                    Tmixed /= MassMixed;

                    // Now we have a range of nodes (j = top, m = bottom) that are mixed
                    // and the mixed temperature (Tmixed).
                    // Move through the mixed nodes and set the final temperature to the mixed temperature.
                    // Also calculate a corrected average temperature for each node.
                    for (int k = j; k <= m; ++k) {
                        Real64 FinalFactorMixing;
                        Real64 AvgFactorMixing;
                        const Real64 NodeCapacitance = this->Node[k].Mass * Cp;
                        if (A[k] == 0.0) {
                            FinalFactorMixing = dt / NodeCapacitance;
                            AvgFactorMixing = FinalFactorMixing / 2.0;
                        } else {
                            FinalFactorMixing = (exp(A[k] * dt) - 1.0) / A[k] / NodeCapacitance;
                            AvgFactorMixing = ((exp(A[k] * dt) - 1.0) / A[k] / dt - 1.0) / A[k] / NodeCapacitance;
                        }
                        const Real64 Q_AdiabaticMixing = (Tmixed - Tfinal[k]) / FinalFactorMixing;
                        Tfinal[k] = Tmixed;
                        Tavg[k] += Q_AdiabaticMixing * AvgFactorMixing;
                    }

                    // Since we mixed, get out of here and start from the top to check again for mixing.
                    break;
                }
            }
        } while (HasInversion);

        // Venting
        if (!this->IsChilledWaterTank) {
            if (Tfinal[0] > this->TankTempLimit) {
                for (int i = 0; i < nTankNodes; ++i) {
                    if (Tfinal[i] > this->TankTempLimit) {
                        Event += this->Node[i].Mass * Cp * (this->TankTempLimit - Tfinal[i]);
                        Tfinal[i] = this->TankTempLimit;
                    }
                }
            }
        }

        // Increment to next internal time step
        TimeRemaining -= dt;
        Real64 Qloss = 0.0;
        for (int i = 0; i < nTankNodes; ++i) {
            auto &node = this->Node[i];
            node.Temp = Tfinal[i];
            node.TempSum += Tavg[i] * dt;

            // Bookkeeping for reporting variables, mostly for Qunmet.
            Real64 Qloss_node = (this->AmbientTemp - Tavg[i]);
            Real64 Qheat_node;
            const Real64 Quse_node = node.UseMassFlowRate * Cp * (this->UseInletTemp - Tavg[i]);
            const Real64 Qsource_node = [&] {
                if (this->HeatPumpNum > 0) {
                    if (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                        if (node.SourceMassFlowRate > 0.0) {
                            return Qheatpump;
                        } else {
                            return 0.0;
                        }
                    } else {
                        assert(HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped);
                        return Qheatpump * node.HPWHWrappedCondenserHeatingFrac;
                    }
                } else {
                    return node.SourceMassFlowRate * Cp * (this->SourceInletTemp - Tavg[i]);
                }
            }();

            if (this->HeaterOn1 || this->HeaterOn2) {
                Qloss_node *= node.OnCycLossCoeff;
                Qheat_node = node.OnCycParaLoad * this->OnCycParaFracToTank;
            } else {
                Qloss_node *= node.OffCycLossCoeff;
                Qheat_node = node.OffCycParaLoad * this->OffCycParaFracToTank;
            }
            Qloss += Qloss_node;
            const Real64 Qneeded_node = max(-Quse_node - Qsource_node - Qloss_node - Qheat_node, 0.0);
            const Real64 Qunmet_node = max(Qneeded_node - Qheater1 - Qheater2, 0.0);
            Eunmet += Qunmet_node * dt;
        }
        SourceInletTempSum += this->SourceInletTemp * dt;
        // More bookkeeping for reporting variables
        Eloss += Qloss * dt;
        const Real64 Quse = (this->UseOutletStratNode > 0)
                                ? this->UseEffectiveness * this->UseMassFlowRate * Cp * (this->UseInletTemp - Tavg[this->UseOutletStratNode - 1])
                                : 0.0;
        Euse += Quse * dt;
        const Real64 Qsource = [&] {
            if (this->HeatPumpNum > 0) {
                if (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                    return Qheatpump;
                } else {
                    assert(HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped);
                    return 0.0;
                }
            } else {
                if (this->SourceOutletStratNode > 0) {
                    return this->SourceEffectiveness * this->SourceMassFlowRate * Cp *
                           (this->SourceInletTemp - Tavg[this->SourceOutletStratNode - 1]);
                } else {
                    return 0.0;
                }
            }
        }();
        Esource += Qsource * dt;
        if (this->HeaterOn1) Runtime1 += dt;
        if (this->HeaterOn2) Runtime2 += dt;
        if (this->HeaterOn1 || this->HeaterOn2) Runtime += dt;
        Eheater1 += Qheater1 * dt;
        Eheater2 += Qheater2 * dt;

        // Calculation for standard ratings
        if (!this->FirstRecoveryDone) {
            Real64 Qrecovery;
            if (this->HeaterOn1 || this->HeaterOn2) {
                Qrecovery = (Qheater1 + Qheater1) / this->Efficiency + this->OnCycParaLoad;
            } else {
                Qrecovery = this->OffCycParaLoad;
            }
            this->FirstRecoveryFuel += Qrecovery * dt;
            if (SetPointRecovered) this->FirstRecoveryDone = true;
        }
    } // end while TimeRemaining > 0.0

    for (auto &e : this->Node) {
        e.TempAvg = e.TempSum / SecInTimeStep;
        e.TempSum = 0.0;
    }

    this->TankTemp = sum(this->Node, &StratifiedNodeData::Temp) / this->Nodes;
    this->TankTempAvg = sum(this->Node, &StratifiedNodeData::TempAvg) / this->Nodes;

    if (!state.dataGlobal->WarmupFlag) {
        // Warn for potential freezing when avg of final temp over all nodes is below 2°C (nearing 0°C)
        if (this->TankTemp < 2) {
            if (this->FreezingErrorIndex == 0) {
                ShowWarningError(state,
                                 format("{}: {} = '{}':  Temperature of tank < 2C indicates of possibility of freeze. Tank Temperature = {:.2R} C.",
                                        RoutineName,
                                        this->Type,
                                        this->Name,
                                        this->TankTemp));
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           this->Type + " = '" + this->Name + "':  Temperature of tank < 2C indicates of possibility of freeze",
                                           this->FreezingErrorIndex,
                                           this->TankTemp, // Report Max
                                           this->TankTemp, // Report Min
                                           _,              // Don't report Sum
                                           "{C}",          // Max Unit
                                           "{C}");         // Min Unit
        }
    }

    if (this->UseOutletStratNode > 0) {
        this->UseOutletTemp = this->Node(this->UseOutletStratNode).TempAvg;
        // Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
        if (this->UseMassFlowRate > 0.0) {
            this->UseOutletTemp = this->UseInletTemp * (1.0 - this->UseEffectiveness) + this->UseOutletTemp * this->UseEffectiveness;
        }
    }

    if (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
        // If we have a wrapped condenser HPWH, set the source outlet to the weighted average of the node
        // temperatures the condenser sees
        Real64 WeightedAverageSourceOutletTemp(0.0);
        for (int i = 1; i <= this->Nodes; ++i) {
            WeightedAverageSourceOutletTemp += this->Node(i).TempAvg * this->Node(i).HPWHWrappedCondenserHeatingFrac;
        }
        this->SourceOutletTemp = WeightedAverageSourceOutletTemp;
    } else if (this->SourceOutletStratNode > 0) {
        // otherwise set it to the temperature of the source outlet node
        this->SourceOutletTemp = this->Node(this->SourceOutletStratNode).TempAvg;
        // Output the average inlet temperature for the DataGlobals::TimeStep
        this->SourceInletTemp = SourceInletTempSum / SecInTimeStep;
    }
    if (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
        // For pumped condensers, set the source inlet and outlets to match the delta T
        // across the water side of the DX coil.
        HeatPumpWaterHeaterData const &HeatPump = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
        DataLoopNode::NodeData const &HPWHCondWaterInletNode = state.dataLoopNodes->Node(HeatPump.CondWaterInletNode);
        DataLoopNode::NodeData const &HPWHCondWaterOutletNode = state.dataLoopNodes->Node(HeatPump.CondWaterOutletNode);
        Real64 const HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
        this->SourceInletTemp = this->SourceOutletTemp + HPWHCondenserDeltaT;
    }

    // Revised source outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
    if (this->SourceOutletStratNode > 0) {
        if (this->SourceMassFlowRate > 0.0) {
            this->SourceOutletTemp = this->SourceInletTemp * (1.0 - this->SourceEffectiveness) + this->SourceOutletTemp * this->SourceEffectiveness;
        }
    }

    this->LossRate = Eloss / SecInTimeStep;
    this->UseRate = Euse / SecInTimeStep;
    Real64 WrappedCondenserHeatPumpRate = 0.0;
    if ((this->HeatPumpNum > 0) && (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterPumped)) {
        this->SourceRate = Qheatpump;
    } else {
        this->SourceRate = Esource / SecInTimeStep;
        WrappedCondenserHeatPumpRate = Qheatpump;
    }

    this->OffCycParaFuelRate = this->OffCycParaLoad * (SecInTimeStep - Runtime) / SecInTimeStep;
    this->OnCycParaFuelRate = this->OnCycParaLoad * Runtime / SecInTimeStep;
    this->OffCycParaRateToTank = this->OffCycParaFuelRate * this->OffCycParaFracToTank;
    this->OnCycParaRateToTank = this->OnCycParaFuelRate * this->OnCycParaFracToTank;
    this->TotalDemandRate =
        -this->UseRate - this->SourceRate - this->LossRate - this->OffCycParaRateToTank - this->OnCycParaRateToTank - WrappedCondenserHeatPumpRate;
    this->HeaterRate1 = Eheater1 / SecInTimeStep;
    this->HeaterRate2 = Eheater2 / SecInTimeStep;
    this->HeaterRate = this->HeaterRate1 + this->HeaterRate2;

    this->UnmetRate = Eunmet / SecInTimeStep;
    this->VentRate = Event / SecInTimeStep;
    this->NetHeatTransferRate = this->UseRate + this->SourceRate + this->LossRate + this->OffCycParaRateToTank + this->OnCycParaRateToTank +
                                this->HeaterRate + this->VentRate + WrappedCondenserHeatPumpRate;

    this->CycleOnCount = CycleOnCount1_loc + CycleOnCount2_loc;
    this->CycleOnCount1 = CycleOnCount1_loc;
    this->CycleOnCount2 = CycleOnCount2_loc;

    this->RuntimeFraction = Runtime / SecInTimeStep;
    this->RuntimeFraction1 = Runtime1 / SecInTimeStep;
    this->RuntimeFraction2 = Runtime2 / SecInTimeStep;

    this->FuelRate = (Eheater1 + Eheater2) / this->Efficiency / SecInTimeStep;

    // Add water heater skin losses and venting losses to ambient zone, if specified
    if (this->AmbientTempZone > 0) this->AmbientZoneGain = -this->LossRate * this->SkinLossFracToZone - this->VentRate;
}

void WaterThermalTankData::CalcNodeMassFlows(InletModeEnum inletMode)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Determines mass flow rates between nodes according to the locations of the use- and source-side inlet and outlet
    // nodes.

    // METHODOLOGY EMPLOYED:
    // In 'Seeking' mode, nodes are searched between the user-specified inlet and outlet nodes to find the node closest
    // in temperature to the inlet fluid temperature.  In 'Fixed' mode, the user-specified nodes are always used.
    // Upward and downward flows are added to each node between an inlet and outlet.  Flows in both directions cancel out
    // to leave only the net flow in one direction.

    int useInletStratNod = this->UseInletStratNode;
    int useOutletStratNode = this->UseOutletStratNode;
    int sourceInletStratNode = this->SourceInletStratNode;
    int sourceOutletStratNode = this->SourceOutletStratNode;

    Real64 useMassFlowRate = this->UseMassFlowRate * this->UseEffectiveness;
    Real64 sourceMassFlowRate = this->SourceMassFlowRate * this->SourceEffectiveness;

    for (auto &e : this->Node) {
        e.UseMassFlowRate = 0.0;
        e.SourceMassFlowRate = 0.0;
        e.MassFlowFromUpper = 0.0;
        e.MassFlowFromLower = 0.0;
        e.MassFlowToUpper = 0.0;
        e.MassFlowToLower = 0.0;
    }

    if (inletMode == InletModeEnum::Seeking) {
        // 'Seek' the node with the temperature closest to the inlet temperature
        // Start at the user-specified inlet node and search to the user-specified outlet node
        int Step;
        if (useMassFlowRate > 0.0) {
            if (useInletStratNod > useOutletStratNode) {
                Step = -1;
            } else {
                Step = 1;
            }
            Real64 MinDeltaTemp = 1.0e6; // Some big number
            int const NodeNum_stop(floop_end(useInletStratNod, useOutletStratNode, Step));
            for (int NodeNum = useInletStratNod; NodeNum != NodeNum_stop; NodeNum += Step) {
                Real64 DeltaTemp = std::abs(this->Node(NodeNum).Temp - this->UseInletTemp);
                if (DeltaTemp < MinDeltaTemp) {
                    MinDeltaTemp = DeltaTemp;
                    useInletStratNod = NodeNum;
                } else if (DeltaTemp > MinDeltaTemp) {
                    break;
                }
            }
        }

        if (sourceMassFlowRate > 0.0) {
            if (sourceInletStratNode > sourceOutletStratNode) {
                Step = -1;
            } else {
                Step = 1;
            }
            Real64 MinDeltaTemp = 1.0e6; // Some big number
            int const NodeNum_stop(floop_end(sourceInletStratNode, sourceOutletStratNode, Step));
            for (int NodeNum = sourceInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step) {
                Real64 DeltaTemp = std::abs(this->Node(NodeNum).Temp - this->SourceInletTemp);
                if (DeltaTemp < MinDeltaTemp) {
                    MinDeltaTemp = DeltaTemp;
                    sourceInletStratNode = NodeNum;
                } else if (DeltaTemp > MinDeltaTemp) {
                    break;
                }
            }
        }
    }

    if (useInletStratNod > 0) this->Node(useInletStratNod).UseMassFlowRate = useMassFlowRate;
    if (sourceInletStratNode > 0) this->Node(sourceInletStratNode).SourceMassFlowRate = sourceMassFlowRate;

    if (useMassFlowRate > 0.0) {
        if (useOutletStratNode > useInletStratNod) {
            // Use-side flow is down
            for (int NodeNum = useInletStratNod; NodeNum <= useOutletStratNode - 1; ++NodeNum) {
                this->Node(NodeNum).MassFlowToLower += useMassFlowRate;
            }
            for (int NodeNum = useInletStratNod + 1; NodeNum <= useOutletStratNode; ++NodeNum) {
                this->Node(NodeNum).MassFlowFromUpper += useMassFlowRate;
            }

        } else if (useOutletStratNode < useInletStratNod) {
            // Use-side flow is up
            for (int NodeNum = useOutletStratNode; NodeNum <= useInletStratNod - 1; ++NodeNum) {
                this->Node(NodeNum).MassFlowFromLower += useMassFlowRate;
            }
            for (int NodeNum = useOutletStratNode + 1; NodeNum <= useInletStratNod; ++NodeNum) {
                this->Node(NodeNum).MassFlowToUpper += useMassFlowRate;
            }

        } else {
            // Use-side flow is across the node; no flow to other nodes
        }
    }

    if (sourceMassFlowRate > 0.0) {
        if (sourceOutletStratNode > sourceInletStratNode) {
            // Source-side flow is down
            for (int NodeNum = sourceInletStratNode; NodeNum <= sourceOutletStratNode - 1; ++NodeNum) {
                this->Node(NodeNum).MassFlowToLower += sourceMassFlowRate;
            }
            for (int NodeNum = sourceInletStratNode + 1; NodeNum <= sourceOutletStratNode; ++NodeNum) {
                this->Node(NodeNum).MassFlowFromUpper += sourceMassFlowRate;
            }

        } else if (sourceOutletStratNode < sourceInletStratNode) {
            // Source-side flow is up
            for (int NodeNum = sourceOutletStratNode; NodeNum <= sourceInletStratNode - 1; ++NodeNum) {
                this->Node(NodeNum).MassFlowFromLower += sourceMassFlowRate;
            }
            for (int NodeNum = sourceOutletStratNode + 1; NodeNum <= sourceInletStratNode; ++NodeNum) {
                this->Node(NodeNum).MassFlowToUpper += sourceMassFlowRate;
            }

        } else {
            // Source-side flow is across the node; no flow to other nodes
        }
    }

    // Cancel out any up and down flows
    for (int NodeNum = 1; NodeNum <= this->Nodes; ++NodeNum) {
        this->Node(NodeNum).MassFlowFromUpper = max((this->Node(NodeNum).MassFlowFromUpper - this->Node(NodeNum).MassFlowToUpper), 0.0);
        this->Node(NodeNum).MassFlowFromLower = max((this->Node(NodeNum).MassFlowFromLower - this->Node(NodeNum).MassFlowToLower), 0.0);
    }
}

void WaterThermalTankData::CalcDesuperheaterWaterHeater(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates a refrigerant desuperheater to heat water

    // METHODOLOGY EMPLOYED:
    // This model uses the rated heat reclaim recovery efficiency, recovery efficiency modifier curve,
    // set point temperature, and dead band temperature difference to simulate the desuperheater coil
    // and sets up inputs to the tank model associated with the desuperheater coil

    int const MaxIte(500); // Maximum number of iterations for RegulaFalsi

    Array1D<Real64> Par(5); // Parameters passed to RegulaFalsi

    auto &DesupHtr = state.dataWaterThermalTanks->WaterHeaterDesuperheater(this->DesuperheaterNum);

    int WaterInletNode = DesupHtr.WaterInletNode;
    int WaterOutletNode = DesupHtr.WaterOutletNode;

    // store first iteration tank temperature and desuperheater mode of operation
    if (FirstHVACIteration && !state.dataHVACGlobal->ShortenTimeStepSys && DesupHtr.FirstTimeThroughFlag) {
        // Save conditions from end of previous system timestep
        // Every iteration that does not advance time should reset to these values
        this->SavedTankTemp = this->TankTemp;
        this->SavedSourceOutletTemp = this->SourceOutletTemp;
        DesupHtr.SaveMode = DesupHtr.Mode;
        DesupHtr.FirstTimeThroughFlag = false;
    }

    else if (!FirstHVACIteration) {
        DesupHtr.FirstTimeThroughFlag = true;
    }

    // initialize variables before invoking any RETURN statement
    this->SourceMassFlowRate = 0.0;
    // reset tank inlet temp from previous time step
    this->SourceInletTemp = this->SavedSourceOutletTemp;
    DesupHtr.DesuperheaterPLR = 0.0;

    state.dataLoopNodes->Node(WaterInletNode).MassFlowRate = 0.0;
    state.dataLoopNodes->Node(WaterOutletNode).MassFlowRate = 0.0;
    state.dataLoopNodes->Node(WaterOutletNode).Temp = this->SavedSourceOutletTemp;

    DesupHtr.DesuperheaterPLR = 0.0;
    DesupHtr.OnCycParaFuelRate = 0.0;
    DesupHtr.OnCycParaFuelEnergy = 0.0;
    DesupHtr.OffCycParaFuelRate = 0.0;
    DesupHtr.OffCycParaFuelEnergy = 0.0;
    DesupHtr.HEffFTempOutput = 0.0;
    DesupHtr.HeaterRate = 0.0;
    DesupHtr.HeaterEnergy = 0.0;
    DesupHtr.PumpPower = 0.0;
    DesupHtr.PumpEnergy = 0.0;

    // simulate only the water heater tank if the desuperheater coil is scheduled off
    Real64 AvailSchedule = ScheduleManager::GetCurrentScheduleValue(state, DesupHtr.AvailSchedPtr);
    if (AvailSchedule == 0.0) {
        DesupHtr.Mode = state.dataWaterThermalTanks->floatMode;
        this->CalcWaterThermalTank(state);
        return;
    }

    // simulate only the water heater tank if the lowest temperature available from the desuperheater coil
    // is less than water inlet temperature if the reclaim source is a refrigeration condenser
    if (DesupHtr.ValidSourceType) {
        int SourceID = DesupHtr.ReclaimHeatingSourceIndexNum;
        if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::CondenserRefrigeration) {
            if (state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).AvailTemperature <= this->SourceInletTemp) {
                DesupHtr.Mode = state.dataWaterThermalTanks->floatMode;
                this->CalcWaterThermalTank(state);
                ShowRecurringWarningErrorAtEnd(state,
                                               "WaterHeating:Desuperheater " + DesupHtr.Name +
                                                   " - Waste heat source temperature was too low to be useful.",
                                               DesupHtr.InsuffTemperatureWarn);
                return;
            } // Temp too low
        }     // desuperheater source is condenser_refrigeration
    }         // validsourcetype

    DesupHtr.OffCycParaFuelRate = DesupHtr.OffCycParaLoad;
    DesupHtr.OffCycParaFuelEnergy = DesupHtr.OffCycParaFuelRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // check that water heater tank cut-in temp is greater than desuperheater cut-in temp
    Real64 desupHtrSetPointTemp = DesupHtr.SetPointTemp;
    Real64 DeadBandTempDiff = DesupHtr.DeadBandTempDiff;
    if ((desupHtrSetPointTemp - DeadBandTempDiff) <= this->SetPointTemp) {
        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
            Real64 MinTemp = desupHtrSetPointTemp - DeadBandTempDiff;
            ++DesupHtr.SetPointError;
            if (DesupHtr.SetPointError < 5) {
                ShowWarningError(state,
                                 DesupHtr.Type + " \"" + DesupHtr.Name +
                                     "\":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the "
                                     "desuperheater. Desuperheater will be disabled.");
                ShowContinueErrorTimeStamp(state, format(" ...Desuperheater cut-in temperature = {:.2R}", MinTemp));
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               DesupHtr.Type + " \"" + DesupHtr.Name +
                                                   "\":  Water heater tank set point temperature is greater than or equal to the cut-in "
                                                   "temperature of the desuperheater. Desuperheater will be disabled warning continues...",
                                               DesupHtr.SetPointErrIndex1,
                                               MinTemp,
                                               MinTemp);
            }
        }

        //   Simulate tank if desuperheater unavailable for water heating
        this->CalcWaterThermalTank(state);
        return;
    }

    Real64 Effic = DesupHtr.HeatReclaimRecoveryEff;

    state.dataLoopNodes->Node(WaterInletNode).Temp = this->SavedSourceOutletTemp;
    DesupHtr.Mode = DesupHtr.SaveMode;

    Real64 HEffFTemp;
    if (DesupHtr.HEffFTemp > 0) {
        HEffFTemp = max(0.0, CurveManager::CurveValue(state, DesupHtr.HEffFTemp, this->SavedTankTemp, state.dataEnvrn->OutDryBulbTemp));
    } else {
        HEffFTemp = 1.0;
    }

    // set limits on heat recovery efficiency
    if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::CondenserRefrigeration) {
        if ((HEffFTemp * Effic) > 0.9) HEffFTemp = 0.9 / Effic;
    } else { // max is 0.3 for all other sources
        if ((HEffFTemp * Effic) > 0.3) HEffFTemp = 0.3 / Effic;
    } // setting limits on heat recovery efficiency

    // Access the appropriate structure to find the average heating capacity of the desuperheater heating coil
    Real64 AverageWasteHeat = 0.0;
    if (DesupHtr.ValidSourceType) {
        int SourceID = DesupHtr.ReclaimHeatingSourceIndexNum;
        if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::CompressorRackRefrigeratedCase) {
            // Refrigeration systems are solved outside the time step iteration, so the
            //  appropriate decrement for other waste heat applications is handled differently
            AverageWasteHeat = state.dataHeatBal->HeatReclaimRefrigeratedRack(SourceID).AvailCapacity -
                               state.dataHeatBal->HeatReclaimRefrigeratedRack(SourceID).HVACDesuperheaterReclaimedHeatTotal;
            DesupHtr.DXSysPLR = 1.0;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::CondenserRefrigeration) {
            AverageWasteHeat = state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).AvailCapacity -
                               state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).HVACDesuperheaterReclaimedHeatTotal;
            DesupHtr.DXSysPLR = 1.0;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXCooling || DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXMultiSpeed ||
                   DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXMultiMode) {
            AverageWasteHeat = state.dataHeatBal->HeatReclaimDXCoil(SourceID).AvailCapacity -
                               state.dataHeatBal->HeatReclaimDXCoil(SourceID).HVACDesuperheaterReclaimedHeatTotal;
            DesupHtr.DXSysPLR = state.dataDXCoils->DXCoil(SourceID).PartLoadRatio;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXVariableCooling) {
            AverageWasteHeat = state.dataHeatBal->HeatReclaimVS_DXCoil(SourceID).AvailCapacity -
                               state.dataHeatBal->HeatReclaimVS_DXCoil(SourceID).HVACDesuperheaterReclaimedHeatTotal;
            DesupHtr.DXSysPLR = state.dataVariableSpeedCoils->VarSpeedCoil(SourceID).PartLoadRatio;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::AirWaterHeatPumpEQ) {
            AverageWasteHeat = state.dataHeatBal->HeatReclaimSimple_WAHPCoil(SourceID).AvailCapacity -
                               state.dataHeatBal->HeatReclaimSimple_WAHPCoil(SourceID).HVACDesuperheaterReclaimedHeatTotal;
            DesupHtr.DXSysPLR = state.dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(SourceID).PartLoadRatio;
        }
    } else {
        AverageWasteHeat = 0.0;
    }

    // simulate only water heater tank if reclaim heating source is off
    if (DesupHtr.DXSysPLR == 0.0) {
        this->CalcWaterThermalTank(state);
        return;
    }

    // If the set point is higher than the maximum water temp, reset both the set point and the dead band temperature difference
    if (desupHtrSetPointTemp > DesupHtr.MaxInletWaterTemp) {
        Real64 CutInTemp = desupHtrSetPointTemp - DeadBandTempDiff;
        desupHtrSetPointTemp = DesupHtr.MaxInletWaterTemp;
        DeadBandTempDiff = max(0.0, (desupHtrSetPointTemp - CutInTemp));
    }

    Real64 Acc; // Accuracy of result from RegulaFalsi
    if (DesupHtr.TankTypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
        Acc = 0.001;
    } else {
        Acc = 0.00001;
    }

    // set the water-side mass flow rate
    Real64 CpWater = Psychrometrics::CPHW(state.dataLoopNodes->Node(WaterInletNode).Temp);
    Real64 MdotWater = DesupHtr.OperatingWaterFlowRate * Psychrometrics::RhoH2O(state.dataLoopNodes->Node(WaterInletNode).Temp);
    Real64 QHeatRate = 0.0;
    if (state.dataLoopNodes->Node(WaterInletNode).Temp <= DesupHtr.MaxInletWaterTemp + Acc) {
        QHeatRate = ((AverageWasteHeat * Effic * HEffFTemp) / DesupHtr.DXSysPLR) + (DesupHtr.PumpElecPower * DesupHtr.PumpFracToWater);
    }

    // change to tanktypenum using parameters?
    Real64 partLoadRatio = 0.0;
    {
        auto const TankType(DesupHtr.TankTypeNum);

        if (TankType == DataPlant::TypeOf_WtrHeaterMixed || TankType == DataPlant::TypeOf_WtrHeaterStratified) {

            DesupHtr.SaveWHMode = this->Mode;
            Real64 PreTankAvgTemp = this->TankTempAvg;
            Real64 NewTankAvgTemp = 0.0; // Initialization
            int max_count = 200;
            int count = 0;
            bool firstThrough = true;
            auto boundPLRFunc = std::bind(&WaterThermalTanks::WaterThermalTankData::PLRResidualWaterThermalTank,
                                          this,
                                          std::placeholders::_1,
                                          std::placeholders::_2,
                                          std::placeholders::_3);

            {
                auto const SELECT_CASE_var1(DesupHtr.Mode);
                if (SELECT_CASE_var1 == state.dataWaterThermalTanks->heatMode) {
                    // Calculate until consistency of desuperheater and tank source side energy transfer achieved
                    while ((std::abs(PreTankAvgTemp - NewTankAvgTemp) > DataHVACGlobals::SmallTempDiff || firstThrough) && count < max_count) {
                        count++;
                        firstThrough = false;
                        PreTankAvgTemp = this->TankTempAvg;
                        partLoadRatio = DesupHtr.DXSysPLR;
                        if (MdotWater > 0.0) {
                            state.dataLoopNodes->Node(WaterOutletNode).Temp = this->SourceOutletTemp + QHeatRate / (MdotWater * CpWater);
                        } else {
                            state.dataLoopNodes->Node(WaterOutletNode).Temp = this->SourceOutletTemp;
                        }

                        //         set the full load outlet temperature on the water heater source inlet node (init has already been called)
                        this->SourceInletTemp = state.dataLoopNodes->Node(WaterOutletNode).Temp;

                        //         set the source mass flow rate for the tank
                        this->SourceMassFlowRate = MdotWater * partLoadRatio;

                        this->MaxCapacity = DesupHtr.BackupElementCapacity;
                        this->MinCapacity = DesupHtr.BackupElementCapacity;
                        DesupHtr.DesuperheaterPLR = partLoadRatio;
                        DesupHtr.HeaterRate = QHeatRate * partLoadRatio;
                        this->CalcWaterThermalTank(state);
                        Real64 NewTankTemp = this->TankTemp;

                        if (NewTankTemp > desupHtrSetPointTemp) {
                            //           Only revert to floating mode if the tank temperature is higher than the cut out temperature
                            if (NewTankTemp > DesupHtr.SetPointTemp) {
                                DesupHtr.Mode = state.dataWaterThermalTanks->floatMode;
                            }
                            Par(1) = desupHtrSetPointTemp;
                            Par(2) = DesupHtr.SaveWHMode;
                            if (FirstHVACIteration) {
                                Par(4) = 1.0;
                            } else {
                                Par(4) = 0.0;
                            }
                            Par(5) = MdotWater;
                            int SolFla;
                            std::string IterNum;
                            General::SolveRoot(state, Acc, MaxIte, SolFla, partLoadRatio, boundPLRFunc, 0.0, DesupHtr.DXSysPLR, Par);
                            if (SolFla == -1) {
                                IterNum = fmt::to_string(MaxIte);
                                if (!state.dataGlobal->WarmupFlag) {
                                    ++DesupHtr.IterLimitExceededNum1;
                                    if (DesupHtr.IterLimitExceededNum1 == 1) {
                                        ShowWarningError(state, DesupHtr.Type + " \"" + DesupHtr.Name + "\"");
                                        ShowContinueError(state,
                                                          format("Iteration limit exceeded calculating desuperheater unit part-load ratio, "
                                                                 "maximum iterations = {}. Part-load ratio returned = {:.3R}",
                                                                 IterNum,
                                                                 partLoadRatio));
                                        ShowContinueErrorTimeStamp(state, "This error occurred in heating mode.");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       DesupHtr.Type + " \"" + DesupHtr.Name +
                                                                           "\":  Iteration limit exceeded in heating mode warning continues. "
                                                                           "Part-load ratio statistics follow.",
                                                                       DesupHtr.IterLimitErrIndex1,
                                                                       partLoadRatio,
                                                                       partLoadRatio);
                                    }
                                }
                            } else if (SolFla == -2) {
                                partLoadRatio = max(
                                    0.0, min(DesupHtr.DXSysPLR, (desupHtrSetPointTemp - this->SavedTankTemp) / (NewTankTemp - this->SavedTankTemp)));
                                this->SourceMassFlowRate = MdotWater * partLoadRatio;
                                this->CalcWaterThermalTank(state);
                                if (!state.dataGlobal->WarmupFlag) {
                                    ++DesupHtr.RegulaFalsiFailedNum1;
                                    if (DesupHtr.RegulaFalsiFailedNum1 == 1) {
                                        ShowWarningError(state, DesupHtr.Type + " \"" + DesupHtr.Name + "\"");
                                        ShowContinueError(state,
                                                          format("Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 "
                                                                 "exceeded. Part-load ratio used = {:.3R}",
                                                                 partLoadRatio));
                                        ShowContinueError(state, "Please send this information to the EnergyPlus support group.");
                                        ShowContinueErrorTimeStamp(state, "This error occurred in heating mode.");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(state,
                                                                       DesupHtr.Type + " \"" + DesupHtr.Name +
                                                                           "\":  Part-load ratio calculation failed in heating mode warning "
                                                                           "continues. Part-load ratio statistics follow.",
                                                                       DesupHtr.RegulaFalsiFailedIndex1,
                                                                       partLoadRatio,
                                                                       partLoadRatio);
                                    }
                                }
                            }
                        } else {
                            partLoadRatio = DesupHtr.DXSysPLR;
                        }
                        NewTankAvgTemp = this->TankTempAvg;
                    }
                } else if (SELECT_CASE_var1 == state.dataWaterThermalTanks->floatMode) {
                    if (MdotWater > 0.0) {
                        state.dataLoopNodes->Node(WaterOutletNode).Temp =
                            state.dataLoopNodes->Node(WaterInletNode).Temp + QHeatRate / (MdotWater * CpWater);
                    } else {
                        state.dataLoopNodes->Node(WaterOutletNode).Temp = state.dataLoopNodes->Node(WaterInletNode).Temp;
                    }
                    //         check tank temperature by setting source inlet mass flow rate to zero
                    partLoadRatio = 0.0;

                    //         set the full load outlet temperature on the water heater source inlet node (init has already been called)
                    this->SourceInletTemp = state.dataLoopNodes->Node(WaterOutletNode).Temp;

                    //         check tank temperature by setting source inlet mass flow rate to zero
                    this->SourceMassFlowRate = 0.0;

                    //         disable the tank heater to find PLR of the HPWH
                    this->MaxCapacity = 0.0;
                    this->MinCapacity = 0.0;
                    DesupHtr.DesuperheaterPLR = partLoadRatio;
                    DesupHtr.HeaterRate = QHeatRate * partLoadRatio;
                    this->CalcWaterThermalTank(state);
                    Real64 NewTankTemp = this->TankTemp;

                    if (NewTankTemp <= (desupHtrSetPointTemp - DeadBandTempDiff)) {
                        this->Mode = DesupHtr.SaveWHMode;
                        if ((this->SavedTankTemp - NewTankTemp) != 0.0) {
                            partLoadRatio =
                                min(DesupHtr.DXSysPLR,
                                    max(0.0, ((desupHtrSetPointTemp - DeadBandTempDiff) - NewTankTemp) / (this->SavedTankTemp - NewTankTemp)));
                        } else {
                            partLoadRatio = DesupHtr.DXSysPLR;
                        }
                        while ((std::abs(PreTankAvgTemp - NewTankAvgTemp) > DataHVACGlobals::SmallTempDiff || firstThrough) && count < max_count) {
                            count++;
                            firstThrough = false;
                            PreTankAvgTemp = this->TankTempAvg;
                            DesupHtr.Mode = state.dataWaterThermalTanks->heatMode;
                            if (MdotWater > 0.0) {
                                state.dataLoopNodes->Node(WaterOutletNode).Temp = this->SourceOutletTemp + QHeatRate / (MdotWater * CpWater);
                            } else {
                                state.dataLoopNodes->Node(WaterOutletNode).Temp = this->SourceOutletTemp;
                            }

                            //           set the full load outlet temperature on the water heater source inlet node
                            this->SourceInletTemp = state.dataLoopNodes->Node(WaterOutletNode).Temp;

                            //           set the source mass flow rate for the tank and enable backup heating element
                            this->SourceMassFlowRate = MdotWater * partLoadRatio;
                            this->MaxCapacity = DesupHtr.BackupElementCapacity;
                            this->MinCapacity = DesupHtr.BackupElementCapacity;
                            DesupHtr.DesuperheaterPLR = partLoadRatio;
                            DesupHtr.HeaterRate = QHeatRate * partLoadRatio;
                            this->CalcWaterThermalTank(state);
                            NewTankTemp = this->TankTemp;

                            if (NewTankTemp > desupHtrSetPointTemp) {
                                Par(1) = desupHtrSetPointTemp;
                                Par(2) = DesupHtr.SaveWHMode;
                                if (FirstHVACIteration) {
                                    Par(4) = 1.0;
                                } else {
                                    Par(4) = 0.0;
                                }
                                Par(5) = MdotWater;
                                int SolFla;
                                std::string IterNum;
                                General::SolveRoot(state, Acc, MaxIte, SolFla, partLoadRatio, boundPLRFunc, 0.0, DesupHtr.DXSysPLR, Par);
                                if (SolFla == -1) {
                                    IterNum = fmt::to_string(MaxIte);
                                    if (!state.dataGlobal->WarmupFlag) {
                                        ++DesupHtr.IterLimitExceededNum2;
                                        if (DesupHtr.IterLimitExceededNum2 == 1) {
                                            ShowWarningError(state, DesupHtr.Type + " \"" + DesupHtr.Name + "\"");
                                            ShowContinueError(state,
                                                              format("Iteration limit exceeded calculating desuperheater unit part-load ratio, "
                                                                     "maximum iterations = {}. Part-load ratio returned = {:.3R}",
                                                                     IterNum,
                                                                     partLoadRatio));
                                            ShowContinueErrorTimeStamp(state, "This error occurred in float mode.");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(state,
                                                                           DesupHtr.Type + " \"" + DesupHtr.Name +
                                                                               "\":  Iteration limit exceeded in float mode warning continues. "
                                                                               "Part-load ratio statistics follow.",
                                                                           DesupHtr.IterLimitErrIndex2,
                                                                           partLoadRatio,
                                                                           partLoadRatio);
                                        }
                                    }
                                } else if (SolFla == -2) {
                                    partLoadRatio = max(
                                        0.0,
                                        min(DesupHtr.DXSysPLR, (desupHtrSetPointTemp - this->SavedTankTemp) / (NewTankTemp - this->SavedTankTemp)));
                                    if (!state.dataGlobal->WarmupFlag) {
                                        ++DesupHtr.RegulaFalsiFailedNum2;
                                        if (DesupHtr.RegulaFalsiFailedNum2 == 1) {
                                            ShowWarningError(state, DesupHtr.Type + " \"" + DesupHtr.Name + "\"");
                                            ShowContinueError(state,
                                                              format("Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to "
                                                                     "1 exceeded. Part-load ratio used = {:.3R}",
                                                                     partLoadRatio));
                                            ShowContinueError(state, "Please send this information to the EnergyPlus support group.");
                                            ShowContinueErrorTimeStamp(state, "This error occurred in float mode.");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                DesupHtr.Type + " \"" + DesupHtr.Name +
                                                    "\": Part-load ratio calculation failed in float mode warning "
                                                    "continues. Part-load ratio statistics follow.",
                                                state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedIndex2,
                                                partLoadRatio,
                                                partLoadRatio);
                                        }
                                    }
                                }
                            }
                            NewTankAvgTemp = this->TankTempAvg;
                        }
                    } else {
                        this->MaxCapacity = DesupHtr.BackupElementCapacity;
                        this->MinCapacity = DesupHtr.BackupElementCapacity;
                    }

                } else {
                }
            }

            //   should never get here, case is checked in GetWaterThermalTankInput
        } else {
            ShowFatalError(state,
                           "Coil:WaterHeating:Desuperheater = " + state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).Name +
                               ":  invalid water heater tank type and name entered = " +
                               state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankType + ", " +
                               state.dataWaterThermalTanks->WaterHeaterDesuperheater(DesuperheaterNum).TankName);
        }
    }

    if (QHeatRate == 0) partLoadRatio = 0.0;

    state.dataLoopNodes->Node(WaterOutletNode).MassFlowRate = MdotWater * partLoadRatio;
    DesupHtr.HEffFTempOutput = HEffFTemp;
    DesupHtr.HeaterRate = QHeatRate * partLoadRatio;
    this->SourceMassFlowRate = MdotWater * partLoadRatio;

    if (partLoadRatio == 0) {
        this->SourceInletTemp = this->SourceOutletTemp;
        state.dataLoopNodes->Node(WaterOutletNode).Temp = this->SourceOutletTemp;
        DesupHtr.HEffFTempOutput = 0.0;
        DesupHtr.HeaterRate = 0.0;
    }

    DesupHtr.HeaterEnergy = DesupHtr.HeaterRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    DesupHtr.DesuperheaterPLR = partLoadRatio;
    DesupHtr.OnCycParaFuelRate = DesupHtr.OnCycParaLoad * partLoadRatio;
    DesupHtr.OnCycParaFuelEnergy = DesupHtr.OnCycParaFuelRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    DesupHtr.OffCycParaFuelRate = DesupHtr.OffCycParaLoad * (1 - partLoadRatio);
    DesupHtr.OffCycParaFuelEnergy = DesupHtr.OffCycParaFuelRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    DesupHtr.PumpPower = DesupHtr.PumpElecPower * (partLoadRatio);
    DesupHtr.PumpEnergy = DesupHtr.PumpPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    // Update used waste heat (just in case multiple users of waste heat use same source)
    if (DesupHtr.ValidSourceType) {
        int SourceID = DesupHtr.ReclaimHeatingSourceIndexNum;
        if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::CompressorRackRefrigeratedCase) {
            state.dataHeatBal->HeatReclaimRefrigeratedRack(SourceID).WaterHeatingDesuperheaterReclaimedHeat(DesuperheaterNum) = DesupHtr.HeaterRate;
            state.dataHeatBal->HeatReclaimRefrigeratedRack(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0;
            for (auto &num : state.dataHeatBal->HeatReclaimRefrigeratedRack(SourceID).WaterHeatingDesuperheaterReclaimedHeat)
                state.dataHeatBal->HeatReclaimRefrigeratedRack(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal += num;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::CondenserRefrigeration) {
            state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).WaterHeatingDesuperheaterReclaimedHeat(DesuperheaterNum) = DesupHtr.HeaterRate;
            state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0;
            for (auto &num : state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).WaterHeatingDesuperheaterReclaimedHeat)
                state.dataHeatBal->HeatReclaimRefrigCondenser(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal += num;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXCooling || DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXMultiSpeed ||
                   DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXMultiMode) {
            state.dataHeatBal->HeatReclaimDXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat(DesuperheaterNum) = DesupHtr.HeaterRate;
            state.dataHeatBal->HeatReclaimDXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0;
            for (auto &num : state.dataHeatBal->HeatReclaimDXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat)
                state.dataHeatBal->HeatReclaimDXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal += num;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::DXVariableCooling) {
            state.dataHeatBal->HeatReclaimVS_DXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat(DesuperheaterNum) = DesupHtr.HeaterRate;
            state.dataHeatBal->HeatReclaimVS_DXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0;
            for (auto &num : state.dataHeatBal->HeatReclaimVS_DXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat)
                state.dataHeatBal->HeatReclaimVS_DXCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal += num;
        } else if (DesupHtr.ReclaimHeatingSource == CoilObjEnum::AirWaterHeatPumpEQ) {
            state.dataHeatBal->HeatReclaimSimple_WAHPCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat(DesuperheaterNum) = DesupHtr.HeaterRate;
            state.dataHeatBal->HeatReclaimSimple_WAHPCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0;
            for (auto &num : state.dataHeatBal->HeatReclaimSimple_WAHPCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat)
                state.dataHeatBal->HeatReclaimSimple_WAHPCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeatTotal += num;
        }
    }
}

void WaterThermalTankData::CalcHeatPumpWaterHeater(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   March 2005
    //       MODIFIED       B. Griffith, Jan 2012 for stratified tank
    //                        B. Shen 12/2014, add air-source variable-speed heat pump water heating
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates a heat pump water heater

    // METHODOLOGY EMPLOYED:
    // Simulate the water heater tank, DX coil, and fan to meet the water heating requirements.

    int const MaxIte(500);   // maximum number of iterations
    Real64 const Acc(0.001); // Accuracy of result from RegulaFalsi

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MdotWater;                                                                             // mass flow rate of condenser water, kg/s
    IntegratedHeatPump::IHPOperationMode IHPMode(IntegratedHeatPump::IHPOperationMode::IdleMode); // IHP working mode
    Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0);

    // References to objects used in this function
    HeatPumpWaterHeaterData &HeatPump = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);

    // initialize local variables
    int AvailSchedule = ScheduleManager::GetCurrentScheduleValue(state, HeatPump.AvailSchedPtr);
    int HPAirInletNode = HeatPump.HeatPumpAirInletNode;
    int HPAirOutletNode = HeatPump.HeatPumpAirOutletNode;
    int OutdoorAirNode = HeatPump.OutsideAirNode;
    int ExhaustAirNode = HeatPump.ExhaustAirNode;
    int HPWaterInletNode = HeatPump.CondWaterInletNode;
    int HPWaterOutletNode = HeatPump.CondWaterOutletNode;
    int InletAirMixerNode = HeatPump.InletAirMixerNode;
    int OutletAirSplitterNode = HeatPump.OutletAirSplitterNode;
    int DXCoilAirInletNode = HeatPump.DXCoilAirInletNode;
    state.dataWaterThermalTanks->hpPartLoadRatio = 0.0;
    int CompOp = 0; // DX compressor operation; 1=on, 0=off
    HeatPump.OnCycParaFuelRate = 0.0;
    HeatPump.OnCycParaFuelEnergy = 0.0;
    HeatPump.OffCycParaFuelRate = 0.0;
    HeatPump.OffCycParaFuelEnergy = 0.0;
    state.dataLoopNodes->Node(HPWaterOutletNode) = state.dataLoopNodes->Node(HPWaterInletNode);
    int MaxSpeedNum = HeatPump.NumofSpeed; // speed number of variable speed HPWH coil

    // assign set point temperature (cut-out) and dead band temp diff (cut-in = cut-out minus dead band temp diff)
    Real64 HPSetPointTemp = HeatPump.SetPointTemp;
    Real64 DeadBandTempDiff = HeatPump.DeadBandTempDiff;
    Real64 RhoWater = Psychrometrics::RhoH2O(HPSetPointTemp); // initialize

    // store first iteration tank temperature and HP mode of operation
    // this code can be called more than once with FirstHVACIteration = .TRUE., use FirstTimeThroughFlag to control save
    if (FirstHVACIteration && !state.dataHVACGlobal->ShortenTimeStepSys && HeatPump.FirstTimeThroughFlag) {
        this->SavedTankTemp = this->TankTemp;
        HeatPump.SaveMode = HeatPump.Mode;
        HeatPump.SaveWHMode = this->Mode;
        HeatPump.FirstTimeThroughFlag = false;
    }

    if (!FirstHVACIteration) HeatPump.FirstTimeThroughFlag = true;

    // check if HPWH is off for some reason and simulate HPWH air- and water-side mass flow rates of 0
    // simulate only water heater tank if HP compressor is scheduled off
    //   simulate only water heater tank if HP compressor cut-out temperature is lower than the tank's cut-in temperature
    //    simulate only water heater tank if HP inlet air temperature is below minimum temperature for HP compressor operation
    //    if the tank maximum temperature limit is less than the HPWH set point temp, disable HPWH
    if (AvailSchedule == 0.0 || (HPSetPointTemp - DeadBandTempDiff) <= this->SetPointTemp ||
        state.dataHVACGlobal->HPWHInletDBTemp < HeatPump.MinAirTempForHPOperation ||
        state.dataHVACGlobal->HPWHInletDBTemp > HeatPump.MaxAirTempForHPOperation || HPSetPointTemp >= this->TankTempLimit ||
        (!HeatPump.AllowHeatingElementAndHeatPumpToRunAtSameTime && this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed &&
         this->SavedMode == state.dataWaterThermalTanks->heatMode) ||
        (!HeatPump.AllowHeatingElementAndHeatPumpToRunAtSameTime && this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified &&
         (this->SavedHeaterOn1 || this->SavedHeaterOn2))) {
        //   revert to float mode any time HPWH compressor is OFF
        HeatPump.Mode = state.dataWaterThermalTanks->floatMode;
        if (InletAirMixerNode > 0) {
            state.dataLoopNodes->Node(InletAirMixerNode) = state.dataLoopNodes->Node(HPAirInletNode);
        }
        //   pass node info and simulate crankcase heater
        if (MaxSpeedNum > 0) {
            int VSCoilNum = HeatPump.DXCoilNum;

            if (HeatPump.bIsIHP) {
                VSCoilNum = state.dataIntegratedHP->IntegratedHeatPumps(VSCoilNum).SCWHCoilIndex;
            }
            // set the SCWH mode
            Real64 SpeedRatio = 1.0; // speed ratio for interpolating between two speed levels
            int SpeedNum = 1;
            if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                if (HeatPump.bIsIHP)
                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              "",
                                                              VSCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                else
                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              HeatPump.DXCoilName,
                                                              VSCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
            } else {
                this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                if (HeatPump.bIsIHP)
                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              "",
                                                              VSCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                else
                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              HeatPump.DXCoilName,
                                                              VSCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
            }

            // set the DWH mode
            if (HeatPump.bIsIHP) {
                VSCoilNum = state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).DWHCoilIndex;

                if (VSCoilNum > 0) // if DWH coil exists
                {
                    if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                        if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                        }
                        this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                        VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                                  "",
                                                                  VSCoilNum,
                                                                  DataHVACGlobals::CycFanCycCoil,
                                                                  EMP1,
                                                                  EMP2,
                                                                  EMP3,
                                                                  1,
                                                                  state.dataWaterThermalTanks->hpPartLoadRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  0.0,
                                                                  0.0,
                                                                  1.0);
                    } else {
                        this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                        VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                                  "",
                                                                  VSCoilNum,
                                                                  DataHVACGlobals::CycFanCycCoil,
                                                                  EMP1,
                                                                  EMP2,
                                                                  EMP3,
                                                                  1,
                                                                  state.dataWaterThermalTanks->hpPartLoadRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  0.0,
                                                                  0.0,
                                                                  1.0);
                        if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                        }
                    }
                }
            }

        } else {
            if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                DXCoils::SimDXCoil(state,
                                   HeatPump.DXCoilName,
                                   CompOp,
                                   FirstHVACIteration,
                                   HeatPump.DXCoilNum,
                                   DataHVACGlobals::CycFanCycCoil,
                                   state.dataWaterThermalTanks->hpPartLoadRatio);
            } else {
                DXCoils::SimDXCoil(state,
                                   HeatPump.DXCoilName,
                                   CompOp,
                                   FirstHVACIteration,
                                   HeatPump.DXCoilNum,
                                   DataHVACGlobals::CycFanCycCoil,
                                   state.dataWaterThermalTanks->hpPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
            }
        }

        if (OutletAirSplitterNode > 0) {
            state.dataLoopNodes->Node(HPAirOutletNode) = state.dataLoopNodes->Node(OutletAirSplitterNode);
        }

        //   Simulate tank if HP compressor unavailable for water heating
        this->CalcWaterThermalTank(state);

        //   If HPWH compressor is available and unit is off for another reason, off-cycle parasitics are calculated
        if (AvailSchedule != 0) {
            HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * (1.0 - state.dataWaterThermalTanks->hpPartLoadRatio);
            HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        }

        //   Warn if HPWH compressor cut-in temperature is less than the water heater tank's set point temp
        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
            if ((HPSetPointTemp - DeadBandTempDiff) <= this->SetPointTemp) {
                Real64 HPMinTemp = HPSetPointTemp - DeadBandTempDiff;
                const auto HPMinTempChar = fmt::to_string(HPMinTemp);
                ++HeatPump.HPSetPointError;
                //  add logic for warmup, DataGlobals::KickOffSimulation and doing sizing here
                if (HeatPump.HPSetPointError == 1) {
                    ShowWarningError(state,
                                     HeatPump.Type + " \"" + HeatPump.Name +
                                         ":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the heat "
                                         "pump water heater. Heat Pump will be disabled and simulation continues.");
                    ShowContinueErrorTimeStamp(state, " ...Heat Pump cut-in temperature=" + HPMinTempChar);
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   HeatPump.Type + " \"" + HeatPump.Name +
                                                       ":  Water heater tank set point temperature is greater than or equal to the cut-in "
                                                       "temperature of the heat pump water heater. Heat Pump will be disabled error continues...",
                                                   HeatPump.HPSetPointErrIndex1,
                                                   HPMinTemp,
                                                   HPMinTemp);
                }
            }
        }
        return;
    }
    Real64 savedTankTemp = this->SavedTankTemp;
    HeatPump.Mode = HeatPump.SaveMode;

    RhoWater = Psychrometrics::RhoH2O(savedTankTemp); // update water density using tank temp

    // set the heat pump air- and water-side mass flow rate
    MdotWater = HeatPump.OperatingWaterFlowRate * Psychrometrics::RhoH2O(savedTankTemp);

    // Select mode of operation (float mode or heat mode) from last iteration.
    // Determine if heating will occur this iteration and get an estimate of the PLR
    if (HeatPump.Mode == state.dataWaterThermalTanks->heatMode) {
        // HPWH was heating last iteration and will continue to heat until the set point is reached
        state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
        if (savedTankTemp > HPSetPointTemp) { // tank set point temp may have been reduced since last iteration and float mode may be needed
            HeatPump.Mode = state.dataWaterThermalTanks->floatMode;
            state.dataWaterThermalTanks->hpPartLoadRatio = 0.0;
            // check to see if HP needs to operate
            // set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
            {
                auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                    state.dataLoopNodes->Node(HPWaterInletNode).Temp = savedTankTemp;
                    state.dataLoopNodes->Node(HPWaterOutletNode).Temp = savedTankTemp;
                } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                    state.dataLoopNodes->Node(HPWaterInletNode).Temp = this->SourceOutletTemp;
                    state.dataLoopNodes->Node(HPWaterOutletNode).Temp = this->SourceInletTemp;
                }
            }
            state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(HPWaterOutletNode).MassFlowRate = 0.0;

            // Check tank temperature by setting source inlet mass flow rate to zero.
            state.dataWaterThermalTanks->hpPartLoadRatio = 0.0;

            // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
            this->SourceInletTemp = state.dataLoopNodes->Node(HPWaterOutletNode).Temp;

            // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
            this->MaxCapacity = 0.0;
            this->MinCapacity = 0.0;
            this->SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
            Real64 SourceEffectivenessBackup = this->SourceEffectiveness;
            this->SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
            this->CalcWaterThermalTank(state);
            this->SourceEffectiveness = SourceEffectivenessBackup;
            Real64 NewTankTemp = this->GetHPWHSensedTankTemp(state);

            // Reset the tank's internal heating element capacity.
            this->MaxCapacity = HeatPump.BackupElementCapacity;
            this->MinCapacity = HeatPump.BackupElementCapacity;

            // Check to see if the tank drifts below set point if no heating happens.
            if (NewTankTemp <= (HPSetPointTemp - DeadBandTempDiff)) {

                // HPWH is now in heating mode
                HeatPump.Mode = state.dataWaterThermalTanks->heatMode;

                // Reset the water heater's mode (call above may have changed modes)
                this->Mode = HeatPump.SaveWHMode;

                state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
            }
        } else { // or use side nodes may meet set point without need for heat pump compressor operation
            // check to see if HP needs to operate
            {
                auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                    state.dataLoopNodes->Node(HPWaterInletNode).Temp = savedTankTemp;
                    state.dataLoopNodes->Node(HPWaterOutletNode).Temp = savedTankTemp;
                } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                    state.dataLoopNodes->Node(HPWaterInletNode).Temp = this->SourceOutletTemp;
                    state.dataLoopNodes->Node(HPWaterOutletNode).Temp = this->SourceInletTemp;
                }
            }
            // Check tank temperature by setting source inlet mass flow rate to zero.
            state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(HPWaterOutletNode).MassFlowRate = 0.0;

            state.dataWaterThermalTanks->hpPartLoadRatio = 0.0;

            // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
            this->SourceInletTemp = state.dataLoopNodes->Node(HPWaterOutletNode).Temp;

            // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
            this->MaxCapacity = 0.0;
            this->MinCapacity = 0.0;
            this->SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
            Real64 SourceEffectivenessBackup = this->SourceEffectiveness;
            this->SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
            this->CalcWaterThermalTank(state);
            this->SourceEffectiveness = SourceEffectivenessBackup;
            Real64 NewTankTemp = this->GetHPWHSensedTankTemp(state);

            // Reset the tank's internal heating element capacity.
            this->MaxCapacity = HeatPump.BackupElementCapacity;
            this->MinCapacity = HeatPump.BackupElementCapacity;

            // Check to see if the tank meets set point if no heating happens.
            if (NewTankTemp > HPSetPointTemp) {

                // HPWH is now in floating mode
                HeatPump.Mode = state.dataWaterThermalTanks->floatMode;

            } else {

                // HPWH remains in heating mode
                state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
            }

            // Reset the water heater's mode (call above may have changed modes)
            this->Mode = HeatPump.SaveWHMode;
        }
    } else {
        assert(HeatPump.Mode == state.dataWaterThermalTanks->floatMode);
        // HPWH was floating last iteration and will continue to float until the cut-in temperature is reached

        // set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
        {
            auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
            if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                state.dataLoopNodes->Node(HPWaterInletNode).Temp = savedTankTemp;
                state.dataLoopNodes->Node(HPWaterOutletNode).Temp = savedTankTemp;
            } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                state.dataLoopNodes->Node(HPWaterInletNode).Temp = this->SourceOutletTemp;
                state.dataLoopNodes->Node(HPWaterOutletNode).Temp = this->SourceInletTemp;
            }
        }
        state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(HPWaterOutletNode).MassFlowRate = 0.0;

        // Check tank temperature by setting source inlet mass flow rate to zero.
        state.dataWaterThermalTanks->hpPartLoadRatio = 0.0;

        // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
        this->SourceInletTemp = state.dataLoopNodes->Node(HPWaterOutletNode).Temp;

        // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
        this->MaxCapacity = 0.0;
        this->MinCapacity = 0.0;
        this->SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
        Real64 SourceEffectivenessBackup = this->SourceEffectiveness;
        this->SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
        this->CalcWaterThermalTank(state);
        this->SourceEffectiveness = SourceEffectivenessBackup;
        Real64 NewTankTemp = this->GetHPWHSensedTankTemp(state);

        // Reset the tank's internal heating element capacity.
        this->MaxCapacity = HeatPump.BackupElementCapacity;
        this->MinCapacity = HeatPump.BackupElementCapacity;

        // Check to see if the tank drifts below set point if no heating happens.
        if (NewTankTemp <= (HPSetPointTemp - DeadBandTempDiff)) {

            // HPWH is now in heating mode
            HeatPump.Mode = state.dataWaterThermalTanks->heatMode;

            // Reset the water heater's mode (call above may have changed modes)
            this->Mode = HeatPump.SaveWHMode;

            state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
        }
    }

    if (HeatPump.bIsIHP) // mark the water heating call, if existing
    {
        if (state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
            if (1 == HeatPump.Mode)
                state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).IsWHCallAvail = true;
            else
                state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).IsWHCallAvail = false;
        }
    }

    // If the HPWH was in heating mode during the last DataGlobals::TimeStep or if it was determined that
    // heating would be needed during this DataGlobals::TimeStep to maintain setpoint, do the heating calculation.
    int SpeedNum = 0;
    Real64 SpeedRatio = 0.0;
    if (HeatPump.Mode == state.dataWaterThermalTanks->heatMode) {

        // set up air flow on DX coil inlet node
        state.dataLoopNodes->Node(DXCoilAirInletNode).MassFlowRate =
            state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio;

        // set the condenser inlet node mass flow rate prior to calling the DXCoils::CalcHPWHDXCoil
        state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = MdotWater * state.dataWaterThermalTanks->hpPartLoadRatio;
        this->SourceMassFlowRate = MdotWater * state.dataWaterThermalTanks->hpPartLoadRatio;

        // Do the coil and tank calculations at full PLR to see if it overshoots setpoint.
        bool bIterSpeed = false;
        if (MaxSpeedNum > 0) { // lowest speed of VS HPWH coil
            SpeedRatio = 1.0;
            state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
            bIterSpeed = true; // prepare for iterating between speed levels
            SpeedNum = 1;
            this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

            if (HeatPump.bIsIHP) {
                bIterSpeed = false; // don't iterate speed unless match conditions below
                IHPMode = IntegratedHeatPump::GetCurWorkMode(state, HeatPump.DXCoilNum);

                if (state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                    int VSCoilNum;
                    if (IntegratedHeatPump::IHPOperationMode::DWHMode == IHPMode) {
                        VSCoilNum = state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).DWHCoilIndex;
                        state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IntegratedHeatPump::IHPOperationMode::DWHMode;
                    } else {
                        VSCoilNum = state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).SCWHCoilIndex;
                        state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode =
                            IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode;
                    }

                    this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              "",
                                                              VSCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);

                    state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IHPMode;
                    this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                } else {
                    SpeedNum = IntegratedHeatPump::GetLowSpeedNumIHP(state, HeatPump.DXCoilNum);

                    this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    IntegratedHeatPump::SimIHP(state,
                                               HeatPump.DXCoilName,
                                               HeatPump.DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               EMP1,
                                               EMP2,
                                               EMP3,
                                               1,
                                               state.dataWaterThermalTanks->hpPartLoadRatio,
                                               SpeedNum,
                                               SpeedRatio,
                                               0.0,
                                               0.0,
                                               true,
                                               false,
                                               1.0);

                    if ((IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode == IHPMode) ||
                        (IntegratedHeatPump::IHPOperationMode::DWHMode == IHPMode)) {
                        bIterSpeed = true;
                    } else {
                        this->SourceMassFlowRate = state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).TankSourceWaterMassFlowRate;
                        MdotWater = this->SourceMassFlowRate;
                    }

                    if (IntegratedHeatPump::IHPOperationMode::SHDWHElecHeatOffMode == IHPMode) // turn off heater element
                    {
                        this->MaxCapacity = 0.0;
                        this->MinCapacity = 0.0;
                    }
                }
            } else {
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          HeatPump.DXCoilName,
                                                          HeatPump.DXCoilNum,
                                                          DataHVACGlobals::CycFanCycCoil,
                                                          EMP1,
                                                          EMP2,
                                                          EMP3,
                                                          1,
                                                          state.dataWaterThermalTanks->hpPartLoadRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          0.0,
                                                          0.0,
                                                          1.0);
            }

            this->CalcWaterThermalTank(state);
        } else {
            this->ConvergeSingleSpeedHPWHCoilAndTank(state, state.dataWaterThermalTanks->hpPartLoadRatio);
        }

        Real64 NewTankTemp = this->GetHPWHSensedTankTemp(state);
        Real64 LowSpeedTankTemp = NewTankTemp;
        Real64 HPWHCondInletNodeLast = state.dataLoopNodes->Node(HPWaterInletNode).Temp;

        Array1D<Real64> Par(5); // Parameters passed to RegulaFalsi
        if (NewTankTemp > HPSetPointTemp) {
            HeatPump.Mode = state.dataWaterThermalTanks->floatMode;
            Par(1) = HPSetPointTemp;
            Par(2) = HeatPump.SaveWHMode;
            if (FirstHVACIteration) {
                Par(4) = 1.0;
            } else {
                Par(4) = 0.0;
            }
            Par(5) = MdotWater;
            auto boundPLRFunc = std::bind(
                &WaterThermalTanks::WaterThermalTankData::PLRResidualHPWH, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
            Real64 zeroResidual = 1.0;
            if (MaxSpeedNum > 0) {
                // square the solving, and avoid warning
                // due to very small capacity at lowest speed of VSHPWH coil
                if (bIterSpeed)
                    zeroResidual = this->PLRResidualHPWH(state, 0.0, Par);
                else
                    zeroResidual = -1.0;
            }

            if (zeroResidual > 0.0) { // then iteration
                int SolFla;
                General::SolveRoot(state, Acc, MaxIte, SolFla, state.dataWaterThermalTanks->hpPartLoadRatio, boundPLRFunc, 0.0, 1.0, Par);
                if (SolFla == -1) {
                    std::string IterNum;
                    IterNum = fmt::to_string(MaxIte);
                    if (!state.dataGlobal->WarmupFlag) {
                        ++HeatPump.IterLimitExceededNum2;
                        if (HeatPump.IterLimitExceededNum2 == 1) {
                            ShowWarningError(state, HeatPump.Type + " \"" + HeatPump.Name + "\"");
                            ShowContinueError(state,
                                              format("Iteration limit exceeded calculating heat pump water heater compressor part-load ratio, "
                                                     "maximum iterations = {}. Part-load ratio returned = {:.3R}",
                                                     IterNum,
                                                     state.dataWaterThermalTanks->hpPartLoadRatio));
                            ShowContinueErrorTimeStamp(state, "This error occurred in float mode.");
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                HeatPump.Type + " \"" + HeatPump.Name +
                                    "\":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.",
                                HeatPump.IterLimitErrIndex2,
                                state.dataWaterThermalTanks->hpPartLoadRatio,
                                state.dataWaterThermalTanks->hpPartLoadRatio);
                        }
                    }
                } else if (SolFla == -2) {
                    state.dataWaterThermalTanks->hpPartLoadRatio =
                        max(0.0, min(1.0, (HPSetPointTemp - savedTankTemp) / (NewTankTemp - savedTankTemp)));
                    if (!state.dataGlobal->WarmupFlag) {
                        ++HeatPump.RegulaFalsiFailedNum2;
                        if (HeatPump.RegulaFalsiFailedNum2 == 1) {
                            ShowWarningError(state, HeatPump.Type + " \"" + HeatPump.Name + "\"");
                            ShowContinueError(state,
                                              format("Heat pump water heater compressor part-load ratio calculation failed: PLR limits of 0 to 1 "
                                                     "exceeded. Part-load ratio used = {:.3R}",
                                                     state.dataWaterThermalTanks->hpPartLoadRatio));
                            ShowContinueError(state, "Please send this information to the EnergyPlus support group.");
                            ShowContinueErrorTimeStamp(state, "This error occurred in float mode.");
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                HeatPump.Type + " \"" + HeatPump.Name +
                                    "\": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.",
                                HeatPump.RegulaFalsiFailedIndex2,
                                state.dataWaterThermalTanks->hpPartLoadRatio,
                                state.dataWaterThermalTanks->hpPartLoadRatio);
                        }
                    }
                }
            } else {
                state.dataWaterThermalTanks->hpPartLoadRatio = 0.0;
            }

            // Re-calculate the HPWH Coil to get the correct heat transfer rate.
            state.dataLoopNodes->Node(HPWaterInletNode).Temp = this->SourceOutletTemp;
            if (MaxSpeedNum > 0) {
                SpeedRatio = 1.0;
                SpeedNum = 1;

                this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                if (HeatPump.bIsIHP) {
                    if (bIterSpeed) {
                        IntegratedHeatPump::SimIHP(state,
                                                   HeatPump.DXCoilName,
                                                   HeatPump.DXCoilNum,
                                                   DataHVACGlobals::CycFanCycCoil,
                                                   EMP1,
                                                   EMP2,
                                                   EMP3,
                                                   1,
                                                   state.dataWaterThermalTanks->hpPartLoadRatio,
                                                   SpeedNum,
                                                   SpeedRatio,
                                                   0.0,
                                                   0.0,
                                                   true,
                                                   false,
                                                   1.0);
                    }
                } else {
                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                }

            } else {
                DXCoils::CalcHPWHDXCoil(state, HeatPump.DXCoilNum, state.dataWaterThermalTanks->hpPartLoadRatio);
            }
        } else if (bIterSpeed) {
            for (int loopIter = 1; loopIter <= 4; ++loopIter) {
                HeatPump.Mode = state.dataWaterThermalTanks->heatMode; // modHeatMode is important for system convergence
                state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
                SpeedRatio = 1.0;
                int LowSpeedNum = 2;
                if (HeatPump.bIsIHP) {
                    LowSpeedNum = IntegratedHeatPump::GetLowSpeedNumIHP(state, HeatPump.DXCoilNum);
                    MaxSpeedNum = IntegratedHeatPump::GetMaxSpeedNumIHP(state, HeatPump.DXCoilNum);
                }

                for (int i = LowSpeedNum; i <= MaxSpeedNum; ++i) {
                    SpeedNum = i;
                    this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    if (HeatPump.bIsIHP) {
                        IntegratedHeatPump::SimIHP(state,
                                                   HeatPump.DXCoilName,
                                                   HeatPump.DXCoilNum,
                                                   DataHVACGlobals::CycFanCycCoil,
                                                   EMP1,
                                                   EMP2,
                                                   EMP3,
                                                   1,
                                                   state.dataWaterThermalTanks->hpPartLoadRatio,
                                                   SpeedNum,
                                                   SpeedRatio,
                                                   0.0,
                                                   0.0,
                                                   true,
                                                   false,
                                                   1.0);
                    } else {
                        VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                                  HeatPump.DXCoilName,
                                                                  HeatPump.DXCoilNum,
                                                                  DataHVACGlobals::CycFanCycCoil,
                                                                  EMP1,
                                                                  EMP2,
                                                                  EMP3,
                                                                  1,
                                                                  state.dataWaterThermalTanks->hpPartLoadRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  0.0,
                                                                  0.0,
                                                                  1.0);
                    }

                    // HPWH condenser water temperature difference
                    Real64 CondenserDeltaT = state.dataLoopNodes->Node(HPWaterOutletNode).Temp - state.dataLoopNodes->Node(HPWaterInletNode).Temp;

                    //           move the full load outlet temperature rate to the water heater structure variables
                    //           (water heaters source inlet node temperature/mdot are set in Init, set it here after DXCoils::CalcHPWHDXCoil has
                    //           been called)
                    this->SourceInletTemp = state.dataLoopNodes->Node(HPWaterInletNode).Temp + CondenserDeltaT;
                    //           this CALL does not update node temps, must use WaterThermalTank variables
                    // select tank type
                    {
                        auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                        if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                            this->CalcWaterThermalTankMixed(state);
                            NewTankTemp = this->TankTemp;
                        } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                            this->CalcWaterThermalTankStratified(state);
                            NewTankTemp = this->FindStratifiedTankSensedTemp(state);
                        }
                    }

                    if (NewTankTemp > HPSetPointTemp) {
                        SpeedNum = i;
                        break;
                    } else {
                        LowSpeedTankTemp = NewTankTemp;
                    }
                }

                Array1D<Real64> ParVS(10); // Parameters passed to RegulaFalsi, for variable-speed HPWH
                if (NewTankTemp > HPSetPointTemp) {
                    ParVS(2) = this->HeatPumpNum;
                    ParVS(3) = SpeedNum;
                    ParVS(4) = HPWaterInletNode;
                    ParVS(5) = HPWaterOutletNode;
                    ParVS(6) = RhoWater;
                    ParVS(7) = HPSetPointTemp;
                    ParVS(8) = HeatPump.SaveWHMode;
                    if (FirstHVACIteration) {
                        ParVS(9) = 1.0;
                    } else {
                        ParVS(9) = 0.0;
                    }

                    int SolFla;
                    std::string IterNum;
                    auto boundPLRFunc = std::bind(&WaterThermalTanks::WaterThermalTankData::PLRResidualIterSpeed,
                                                  this,
                                                  std::placeholders::_1,
                                                  std::placeholders::_2,
                                                  std::placeholders::_3);
                    General::SolveRoot(state, Acc, MaxIte, SolFla, SpeedRatio, boundPLRFunc, 1.0e-10, 1.0, ParVS);

                    if (SolFla == -1) {
                        IterNum = fmt::to_string(MaxIte);
                        if (!state.dataGlobal->WarmupFlag) {
                            ++HeatPump.IterLimitExceededNum1;
                            if (HeatPump.IterLimitExceededNum1 == 1) {
                                ShowWarningError(state, HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                ShowContinueError(state,
                                                  format("Iteration limit exceeded calculating heat pump water heater speed speed ratio ratio, "
                                                         "maximum iterations = {}. speed ratio returned = {:.3R}",
                                                         IterNum,
                                                         SpeedRatio));
                                ShowContinueErrorTimeStamp(state, "This error occurred in heating mode.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    state,
                                    HeatPump.Type + " \"" + HeatPump.Name +
                                        "\":  Iteration limit exceeded in heating mode warning continues. speed ratio statistics follow.",
                                    HeatPump.IterLimitErrIndex1,
                                    SpeedRatio,
                                    SpeedRatio);
                            }
                        }
                    } else if (SolFla == -2) {
                        SpeedRatio = max(0.0, min(1.0, (HPSetPointTemp - LowSpeedTankTemp) / (NewTankTemp - LowSpeedTankTemp)));
                        if (!state.dataGlobal->WarmupFlag) {
                            ++HeatPump.RegulaFalsiFailedNum1;
                            if (HeatPump.RegulaFalsiFailedNum1 == 1) {
                                ShowWarningError(state, HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                ShowContinueError(state,
                                                  format("Heat pump water heater speed ratio calculation failed: speed ratio limits of 0 to 1 "
                                                         "exceeded. speed ratio used = {:.3R}",
                                                         SpeedRatio));
                                ShowContinueError(state, "Please send this information to the EnergyPlus support group.");
                                ShowContinueErrorTimeStamp(state, "This error occurred in heating mode.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    state,
                                    HeatPump.Type + " \"" + HeatPump.Name +
                                        "\":  Speed ratio calculation failed in heating mode warning continues. Speed ratio statistics follow.",
                                    HeatPump.RegulaFalsiFailedIndex1,
                                    SpeedRatio,
                                    SpeedRatio);
                            }
                        }
                    }
                } else {
                    SpeedNum = MaxSpeedNum;
                    SpeedRatio = 1.0;
                }

                state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
                this->SetVSHPWHFlowRates(state, HeatPump, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                if (HeatPump.bIsIHP) {
                    IntegratedHeatPump::SimIHP(state,
                                               HeatPump.DXCoilName,
                                               HeatPump.DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               EMP1,
                                               EMP2,
                                               EMP3,
                                               1,
                                               state.dataWaterThermalTanks->hpPartLoadRatio,
                                               SpeedNum,
                                               SpeedRatio,
                                               0.0,
                                               0.0,
                                               true,
                                               false,
                                               1.0);
                } else {
                    VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                              HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              state.dataWaterThermalTanks->hpPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                }

                // HPWH condenser water temperature difference
                Real64 CondenserDeltaT = state.dataLoopNodes->Node(HPWaterOutletNode).Temp - state.dataLoopNodes->Node(HPWaterInletNode).Temp;

                //           move the full load outlet temperature rate to the water heater structure variables
                //           (water heaters source inlet node temperature/mdot are set in Init, set it here after DXCoils::CalcHPWHDXCoil has been
                //           called)
                this->SourceInletTemp = state.dataLoopNodes->Node(HPWaterInletNode).Temp + CondenserDeltaT;
                //           this CALL does not update node temps, must use WaterThermalTank variables
                // select tank type
                {
                    auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                    if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                        this->CalcWaterThermalTankMixed(state);
                        NewTankTemp = this->TankTemp;
                    } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                        this->CalcWaterThermalTankStratified(state);
                        NewTankTemp = this->FindStratifiedTankSensedTemp(state);
                    }
                }
                // update inlet temp
                state.dataLoopNodes->Node(HPWaterInletNode).Temp = this->SourceOutletTemp;
                if (std::abs(state.dataLoopNodes->Node(HPWaterInletNode).Temp - HPWHCondInletNodeLast) < DataHVACGlobals::SmallTempDiff) break;
                HPWHCondInletNodeLast = state.dataLoopNodes->Node(HPWaterInletNode).Temp;
            }

        } else {
            // Set the PLR to 1 if we're not going to reach setpoint during this DataGlobals::TimeStep.
            state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
        }
    }

    if (HeatPump.bIsIHP) {
        if (state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
            IntegratedHeatPump::ClearCoils(state, HeatPump.DXCoilNum); // clear node info when checking the heating load
        }
    }

    // set air-side mass flow rate for final calculation
    if (InletAirMixerNode > 0) {
        state.dataLoopNodes->Node(InletAirMixerNode).MassFlowRate =
            state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio;
        state.dataLoopNodes->Node(HPAirInletNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio *
                                                                 (1.0 - state.dataWaterThermalTanks->mixerInletAirSchedule);
        state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRate =
            state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio * state.dataWaterThermalTanks->mixerInletAirSchedule;
        //   IF HPWH is off, pass zone node conditions through HPWH air-side
        if (state.dataWaterThermalTanks->hpPartLoadRatio == 0)
            state.dataLoopNodes->Node(InletAirMixerNode) = state.dataLoopNodes->Node(HPAirInletNode);
    } else {
        if (OutdoorAirNode == 0) {
            state.dataLoopNodes->Node(HPAirInletNode).MassFlowRate =
                state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio;
        } else {
            state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRate =
                state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio;
        }
    }
    if (state.dataWaterThermalTanks->hpPartLoadRatio == 0) this->SourceInletTemp = this->SourceOutletTemp;

    // set water-side mass flow rate for final calculation
    state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = MdotWater * state.dataWaterThermalTanks->hpPartLoadRatio;

    if (MaxSpeedNum > 0) {

        // it is important to use mdotAir to reset the notes, otherwise, could fail to converge
        if (InletAirMixerNode > 0) {
            state.dataLoopNodes->Node(InletAirMixerNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
            state.dataLoopNodes->Node(InletAirMixerNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
        } else {
            if (OutdoorAirNode == 0) {
                state.dataLoopNodes->Node(HPAirInletNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
                state.dataLoopNodes->Node(HPAirInletNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
            } else {
                state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
                state.dataLoopNodes->Node(OutdoorAirNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
            }
        }

        //   set the max mass flow rate for outdoor fans
        state.dataLoopNodes->Node(HeatPump.FanOutletNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;

        if (HeatPump.bIsIHP) {
            // pass node information using resulting PLR
            if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                IntegratedHeatPump::SimIHP(state,
                                           HeatPump.DXCoilName,
                                           HeatPump.DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           EMP1,
                                           EMP2,
                                           EMP3,
                                           CompOp,
                                           state.dataWaterThermalTanks->hpPartLoadRatio,
                                           SpeedNum,
                                           SpeedRatio,
                                           0.0,
                                           0.0,
                                           true,
                                           false,
                                           1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                IntegratedHeatPump::SimIHP(state,
                                           HeatPump.DXCoilName,
                                           HeatPump.DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           EMP1,
                                           EMP2,
                                           EMP3,
                                           CompOp,
                                           state.dataWaterThermalTanks->hpPartLoadRatio,
                                           SpeedNum,
                                           SpeedRatio,
                                           0.0,
                                           0.0,
                                           true,
                                           false,
                                           1.0);
            } else {
                //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                IntegratedHeatPump::SimIHP(state,
                                           HeatPump.DXCoilName,
                                           HeatPump.DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           EMP1,
                                           EMP2,
                                           EMP3,
                                           CompOp,
                                           state.dataWaterThermalTanks->hpPartLoadRatio,
                                           SpeedNum,
                                           SpeedRatio,
                                           0.0,
                                           0.0,
                                           true,
                                           false,
                                           1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                IntegratedHeatPump::SimIHP(state,
                                           HeatPump.DXCoilName,
                                           HeatPump.DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           EMP1,
                                           EMP2,
                                           EMP3,
                                           CompOp,
                                           state.dataWaterThermalTanks->hpPartLoadRatio,
                                           SpeedNum,
                                           SpeedRatio,
                                           0.0,
                                           0.0,
                                           true,
                                           false,
                                           1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
            }
        } else {
            // pass node information using resulting PLR
            if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          HeatPump.DXCoilName,
                                                          HeatPump.DXCoilNum,
                                                          DataHVACGlobals::CycFanCycCoil,
                                                          EMP1,
                                                          EMP2,
                                                          EMP3,
                                                          CompOp,
                                                          state.dataWaterThermalTanks->hpPartLoadRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          0.0,
                                                          0.0,
                                                          1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          HeatPump.DXCoilName,
                                                          HeatPump.DXCoilNum,
                                                          DataHVACGlobals::CycFanCycCoil,
                                                          EMP1,
                                                          EMP2,
                                                          EMP3,
                                                          CompOp,
                                                          state.dataWaterThermalTanks->hpPartLoadRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          0.0,
                                                          0.0,
                                                          1.0);
            } else {
                //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          HeatPump.DXCoilName,
                                                          HeatPump.DXCoilNum,
                                                          DataHVACGlobals::CycFanCycCoil,
                                                          EMP1,
                                                          EMP2,
                                                          EMP3,
                                                          CompOp,
                                                          state.dataWaterThermalTanks->hpPartLoadRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          0.0,
                                                          0.0,
                                                          1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                          HeatPump.DXCoilName,
                                                          HeatPump.DXCoilNum,
                                                          DataHVACGlobals::CycFanCycCoil,
                                                          EMP1,
                                                          EMP2,
                                                          EMP3,
                                                          CompOp,
                                                          state.dataWaterThermalTanks->hpPartLoadRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          0.0,
                                                          0.0,
                                                          1.0);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
                } else {
                    Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
            }
        }
    } else { // single speed

        // pass node information using resulting PLR
        if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
            //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
            } else {
                Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
            }
            DXCoils::SimDXCoil(state,
                               HeatPump.DXCoilName,
                               CompOp,
                               FirstHVACIteration,
                               HeatPump.DXCoilNum,
                               DataHVACGlobals::CycFanCycCoil,
                               state.dataWaterThermalTanks->hpPartLoadRatio);
            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
            } else {
                Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
            }
            DXCoils::SimDXCoil(state,
                               HeatPump.DXCoilName,
                               CompOp,
                               FirstHVACIteration,
                               HeatPump.DXCoilNum,
                               DataHVACGlobals::CycFanCycCoil,
                               state.dataWaterThermalTanks->hpPartLoadRatio);
        } else {
            //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
            DXCoils::SimDXCoil(state,
                               HeatPump.DXCoilName,
                               CompOp,
                               FirstHVACIteration,
                               HeatPump.DXCoilNum,
                               DataHVACGlobals::CycFanCycCoil,
                               state.dataWaterThermalTanks->hpPartLoadRatio);
            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
            } else {
                Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
            }
            DXCoils::SimDXCoil(state,
                               HeatPump.DXCoilName,
                               CompOp,
                               FirstHVACIteration,
                               HeatPump.DXCoilNum,
                               DataHVACGlobals::CycFanCycCoil,
                               state.dataWaterThermalTanks->hpPartLoadRatio);
            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[HeatPump.FanNum]->simulate(state, _, _, _, _);
            } else {
                Fans::SimulateFanComponents(state, HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
            }
        }
    }

    // Call the tank one more time with the final PLR
    if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
        this->CalcWaterThermalTankMixed(state);
    } else if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
        this->CalcWaterThermalTankStratified(state);
    } else {
        assert(0);
    }

    // set HPWH outlet node equal to the outlet air splitter node conditions if outlet air splitter node exists
    if (OutletAirSplitterNode > 0) {
        state.dataLoopNodes->Node(HPAirOutletNode) = state.dataLoopNodes->Node(OutletAirSplitterNode);
        state.dataLoopNodes->Node(ExhaustAirNode) = state.dataLoopNodes->Node(OutletAirSplitterNode);
    }

    // Check schedule to divert air-side cooling to outdoors.
    if (HeatPump.OutletAirSplitterSchPtr > 0) {
        Real64 OutletAirSplitterSch = ScheduleManager::GetCurrentScheduleValue(state, HeatPump.OutletAirSplitterSchPtr);
        state.dataLoopNodes->Node(HPAirOutletNode).MassFlowRate =
            state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio * (1.0 - OutletAirSplitterSch);
        state.dataLoopNodes->Node(ExhaustAirNode).MassFlowRate =
            state.dataWaterThermalTanks->mdotAir * state.dataWaterThermalTanks->hpPartLoadRatio * OutletAirSplitterSch;
    }

    HeatPump.HeatingPLR = state.dataWaterThermalTanks->hpPartLoadRatio;
    HeatPump.OnCycParaFuelRate = HeatPump.OnCycParaLoad * state.dataWaterThermalTanks->hpPartLoadRatio;
    HeatPump.OnCycParaFuelEnergy = HeatPump.OnCycParaFuelRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * (1.0 - state.dataWaterThermalTanks->hpPartLoadRatio);
    HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
        HeatPump.ControlTempAvg = this->TankTempAvg;
        HeatPump.ControlTempFinal = this->TankTemp;
    } else if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
        HeatPump.ControlTempAvg = this->FindStratifiedTankSensedTemp(state, true);
        HeatPump.ControlTempFinal = this->FindStratifiedTankSensedTemp(state);
    } else {
        assert(0);
    }

    {
        auto const SELECT_CASE_var(HeatPump.InletAirConfiguration);

        //   no sensible capacity to zone for outdoor and scheduled HPWH
        if (SELECT_CASE_var == AmbientTempEnum::OutsideAir) {
            HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
            HeatPump.HPWaterHeaterLatentCapacity = 0.0;

        } else if (SELECT_CASE_var == AmbientTempEnum::Schedule) {
            HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
            HeatPump.HPWaterHeaterLatentCapacity = 0.0;

            //   calculate sensible capacity to zone for inlet air configuration equals Zone Only or Zone And Outdoor Air configurations
        } else {
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(state.dataLoopNodes->Node(HPAirInletNode).HumRat);

            //     add parasitics to zone heat balance if parasitic heat load is to zone otherwise neglect parasitics
            if (HeatPump.ParasiticTempIndicator == AmbientTempEnum::TempZone) {
                HeatPump.HPWaterHeaterSensibleCapacity =
                    (state.dataLoopNodes->Node(HPAirOutletNode).MassFlowRate * CpAir *
                     (state.dataLoopNodes->Node(HPAirOutletNode).Temp - state.dataLoopNodes->Node(HPAirInletNode).Temp)) +
                    HeatPump.OnCycParaFuelRate + HeatPump.OffCycParaFuelRate;
            } else {
                HeatPump.HPWaterHeaterSensibleCapacity =
                    state.dataLoopNodes->Node(HPAirOutletNode).MassFlowRate * CpAir *
                    (state.dataLoopNodes->Node(HPAirOutletNode).Temp - state.dataLoopNodes->Node(HPAirInletNode).Temp);
            }

            HeatPump.HPWaterHeaterLatentCapacity =
                state.dataLoopNodes->Node(HPAirOutletNode).MassFlowRate *
                (state.dataLoopNodes->Node(HPAirOutletNode).HumRat - state.dataLoopNodes->Node(HPAirInletNode).HumRat);
        }
    }
}

void WaterThermalTankData::CalcWaterThermalTank(EnergyPlusData &state)
{
    if (this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
        this->CalcWaterThermalTankMixed(state);
    } else if (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
        this->CalcWaterThermalTankStratified(state);
    } else {
        assert(false);
    }
}

Real64 WaterThermalTankData::GetHPWHSensedTankTemp(EnergyPlusData &state)
{
    if (this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
        return this->TankTemp;
    } else {
        assert(this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified);
        return this->FindStratifiedTankSensedTemp(state);
    }
}

void WaterThermalTankData::ConvergeSingleSpeedHPWHCoilAndTank(EnergyPlusData &state, Real64 const partLoadRatio)
{
    HeatPumpWaterHeaterData &HPWH = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
    DXCoils::DXCoilData &Coil = state.dataDXCoils->DXCoil(HPWH.DXCoilNum);

    Real64 PrevTankTemp = this->SourceOutletTemp;
    for (int i = 1; i <= 10; ++i) {

        DXCoils::CalcHPWHDXCoil(state, HPWH.DXCoilNum, partLoadRatio);
        this->SourceInletTemp = state.dataLoopNodes->Node(HPWH.CondWaterOutletNode).Temp;

        this->CalcWaterThermalTank(state);
        state.dataLoopNodes->Node(Coil.WaterInNode).Temp = this->SourceOutletTemp;

        if (std::abs(this->SourceOutletTemp - PrevTankTemp) < DataHVACGlobals::SmallTempDiff) {
            break;
        }

        PrevTankTemp = this->SourceOutletTemp;
    }
}

void WaterThermalTankData::SetVSHPWHFlowRates(EnergyPlusData &state,
                                              HeatPumpWaterHeaterData &HPWH, // heat pump coil
                                              int const SpeedNum,            // upper speed number
                                              Real64 const SpeedRatio,       // interpolation ration between upper and lower speed
                                              Real64 const WaterDens,        // tank water density
                                              Real64 &MdotWater,             // water flow rate
                                              bool const FirstHVACIteration)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         B.Shen, ORNL, 12/2014
    //       DATE WRITTEN   May 2005
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    //  set water and air flow rates driven by the variable-speed HPWH coil
    //  calculate resultant HPWH coil output

    int SpeedLow = SpeedNum - 1;
    if (SpeedLow < 1) SpeedLow = 1;

    int HPWaterInletNode = HPWH.CondWaterInletNode;
    int DXCoilAirInletNode = HPWH.DXCoilAirInletNode;
    if (HPWH.bIsIHP) {
        HPWH.OperatingWaterFlowRate = IntegratedHeatPump::GetWaterVolFlowRateIHP(state, HPWH.DXCoilNum, SpeedNum, SpeedRatio, true);
        state.dataWaterThermalTanks->mdotAir = IntegratedHeatPump::GetAirMassFlowRateIHP(state, HPWH.DXCoilNum, SpeedNum, SpeedRatio, true);
        HPWH.OperatingAirFlowRate = IntegratedHeatPump::GetAirVolFlowRateIHP(state, HPWH.DXCoilNum, SpeedNum, SpeedRatio, true);
        state.dataLoopNodes->Node(DXCoilAirInletNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir;
        state.dataLoopNodes->Node(DXCoilAirInletNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
        state.dataLoopNodes->Node(DXCoilAirInletNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
    } else {
        HPWH.OperatingWaterFlowRate = HPWH.HPWHWaterVolFlowRate(SpeedNum) * SpeedRatio + HPWH.HPWHWaterVolFlowRate(SpeedLow) * (1.0 - SpeedRatio);
        HPWH.OperatingAirFlowRate = HPWH.HPWHAirVolFlowRate(SpeedNum) * SpeedRatio + HPWH.HPWHAirVolFlowRate(SpeedLow) * (1.0 - SpeedRatio);
        state.dataWaterThermalTanks->mdotAir =
            HPWH.HPWHAirMassFlowRate(SpeedNum) * SpeedRatio + HPWH.HPWHAirMassFlowRate(SpeedLow) * (1.0 - SpeedRatio);
    }

    MdotWater = HPWH.OperatingWaterFlowRate * WaterDens;
    this->SourceMassFlowRate = MdotWater;

    state.dataLoopNodes->Node(DXCoilAirInletNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir;
    state.dataLoopNodes->Node(HPWaterInletNode).MassFlowRate = MdotWater;
    this->SourceMassFlowRate = MdotWater;

    if (HPWH.InletAirMixerNode > 0) {
        state.dataLoopNodes->Node(HPWH.InletAirMixerNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir;
        state.dataLoopNodes->Node(HPWH.InletAirMixerNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
    } else {
        if (HPWH.OutsideAirNode == 0) {
            state.dataLoopNodes->Node(HPWH.HeatPumpAirInletNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir;
            state.dataLoopNodes->Node(HPWH.HeatPumpAirInletNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
        } else {
            state.dataLoopNodes->Node(HPWH.OutsideAirNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir;
            state.dataLoopNodes->Node(HPWH.OutsideAirNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
        }
    }

    // put fan component first, regardless placement, to calculate fan power
    int FanInNode;
    if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
        FanInNode = state.dataHVACFan->fanObjs[HPWH.FanNum]->inletNodeNum;
    } else {
        FanInNode = state.dataFans->Fan(HPWH.FanNum).InletNodeNum;
    }

    state.dataLoopNodes->Node(FanInNode).MassFlowRate = state.dataWaterThermalTanks->mdotAir;
    state.dataLoopNodes->Node(FanInNode).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
    state.dataLoopNodes->Node(FanInNode).MassFlowRateMax = state.dataWaterThermalTanks->mdotAir;
    if (HPWH.FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
        state.dataFans->Fan(HPWH.FanNum).MassFlowRateMaxAvail = state.dataWaterThermalTanks->mdotAir;
    } // system fan will use the inlet node max avail.

    if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
        state.dataHVACFan->fanObjs[HPWH.FanNum]->simulate(state, _, _, _, _);
    } else {
        Fans::SimulateFanComponents(state, HPWH.FanName, FirstHVACIteration, HPWH.FanNum);
    }
}

Real64 WaterThermalTankData::PLRResidualIterSpeed(EnergyPlusData &state,
                                                  Real64 const SpeedRatio, // speed ratio between two speed levels
                                                  Array1D<Real64> const &Par)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         B.Shen, ORNL, 12/2014
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function (desired tank temp - actual tank temp), when iterating speed ration between two speed levels
    //  HP water heater output depends on the speed ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls residuals to get tank temperature at the given speed ratio between a lower and an upper speed levels
    //  and calculates the residual as defined respectively for DataPlant::TypeOf_WtrHeaterMixed or DataPlant::TypeOf_WtrHeaterStratified

    Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling variable-speed coil function

    int HPNum = int(Par(2));
    int SpeedNum = int(Par(3));
    int HPWaterInletNode = int(Par(4));
    int HPWaterOutletNode = int(Par(5));
    Real64 RhoWater = Par(6);
    this->Mode = int(Par(8));
    bool FirstHVACIteration = (Par(9) == 1.0);

    state.dataWaterThermalTanks->hpPartLoadRatio = 1.0;
    Real64 MdotWater = 0.0;

    auto &HPWH = state.dataWaterThermalTanks->HPWaterHeater(HPNum);

    this->SetVSHPWHFlowRates(state, HPWH, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

    if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).bIsIHP) {
        IntegratedHeatPump::SimIHP(state,
                                   state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                   state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                   DataHVACGlobals::CycFanCycCoil,
                                   EMP1,
                                   EMP2,
                                   EMP3,
                                   1,
                                   state.dataWaterThermalTanks->hpPartLoadRatio,
                                   SpeedNum,
                                   SpeedRatio,
                                   0.0,
                                   0.0,
                                   true,
                                   false,
                                   1.0);
    } else {
        VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                  state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                                  state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                                  DataHVACGlobals::CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  state.dataWaterThermalTanks->hpPartLoadRatio,
                                                  SpeedNum,
                                                  SpeedRatio,
                                                  0.0,
                                                  0.0,
                                                  1.0);
    }

    Real64 CondenserDeltaT;
    CondenserDeltaT = state.dataLoopNodes->Node(HPWaterOutletNode).Temp - state.dataLoopNodes->Node(HPWaterInletNode).Temp;

    //           move the full load outlet temperature rate to the water heater structure variables
    //           (water heaters source inlet node temperature/mdot are set in Init, set it here after DXCoils::CalcHPWHDXCoil has been called)
    this->SourceInletTemp = state.dataLoopNodes->Node(HPWaterInletNode).Temp + CondenserDeltaT;

    //           this CALL does not update node temps, must use WaterThermalTank variables
    // select tank type
    Real64 NewTankTemp = 0.0;
    {
        auto const SELECT_CASE_var1(state.dataWaterThermalTanks->HPWaterHeater(HPNum).TankTypeNum);
        if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
            this->CalcWaterThermalTankMixed(state);
            NewTankTemp = this->TankTemp;
        } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
            this->CalcWaterThermalTankStratified(state);
            NewTankTemp = this->FindStratifiedTankSensedTemp(state);
        }
    }

    return Par(7) - NewTankTemp;
}

Real64 WaterThermalTankData::PLRResidualWaterThermalTank(EnergyPlusData &state,
                                                         Real64 const HPPartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                         Array1D<Real64> const &Par    // par(1) = HP set point temperature [C]

)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   May 2005
    //       MODIFIED       Yueyue Zhou
    //       DATE MODIFIED  May 2019
    //       MODIFICATION   Combined the PLR functions for both mixed and stratified tank
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function (desired tank temp - actual tank temp)
    //  HP water heater output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls CalcWaterThermalTank to get tank temperature at the given part load ratio (source water mass flow rate)
    //  and calculates the residual as defined above

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = tank mode
    // par(3) = water heater num
    // par(4) = FirstHVACIteration
    // par(5) = MdotWater

    this->Mode = int(Par(2));
    this->SourceMassFlowRate = Par(5) * HPPartLoadRatio;
    this->CalcWaterThermalTank(state);
    Real64 NewTankTemp = this->TankTemp;
    Real64 PLRResidualWaterThermalTank = Par(1) - NewTankTemp;
    return PLRResidualWaterThermalTank;
}

Real64 WaterThermalTankData::PLRResidualHPWH(EnergyPlusData &state, Real64 const HPPartLoadRatio, Array1D<Real64> const &Par)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         B.Griffith,  Richard Raustad
    //       DATE WRITTEN   Jan 2012
    //       MODIFIED
    //       RE-ENGINEERED  Noel Merket, Oct 2015

    // PURPOSE OF THIS FUNCTION:
    //  Calculates residual function (desired tank temp - actual tank temp)
    //  HP water heater output depends on the part load ratio which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    //  Calls with CalcWaterThermalTankMixed or CalcWaterThermalTankStratified to get tank temperature at the given part load ratio (source water
    //  mass flow rate) and calculates the residual as defined above

    // Par(1) = HP set point temperature [C]
    // Par(2) = tank mode
    // Par(3) = water heater num
    // Par(4) = FirstHVACIteration
    // Par(5) = MdotWater

    HeatPumpWaterHeaterData &HeatPump = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
    bool const isVariableSpeed = (HeatPump.NumofSpeed > 0);
    this->Mode = int(Par(2));
    // Apply the PLR
    if (this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
        // For a mixed tank, the PLR is applied to the source mass flow rate.
        this->SourceMassFlowRate = Par(5) * HPPartLoadRatio;
        this->CalcWaterThermalTankMixed(state);
    } else {
        assert(this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified);
        // For a stratified tank, the PLR is applied to the Coil.TotalHeatingEnergyRate
        // whether that's a VarSpeedCoil or DXCoils::DXCoil.
        // Here we create a pointer to the TotalHeatingEnergyRate for the appropriate coil type.
        Real64 *CoilTotalHeatingEnergyRatePtr;
        if (isVariableSpeed) {
            if (HeatPump.bIsIHP)
                CoilTotalHeatingEnergyRatePtr = &state.dataIntegratedHP->IntegratedHeatPumps(HeatPump.DXCoilNum).TotalWaterHeatingRate;
            else
                CoilTotalHeatingEnergyRatePtr = &state.dataVariableSpeedCoils->VarSpeedCoil(HeatPump.DXCoilNum).TotalHeatingEnergyRate;
        } else {
            CoilTotalHeatingEnergyRatePtr = &state.dataDXCoils->DXCoil(HeatPump.DXCoilNum).TotalHeatingEnergyRate;
        }
        // Copy the value of the total heating energy rate
        Real64 const CoilTotalHeatingEnergyRateBackup = *CoilTotalHeatingEnergyRatePtr;
        // Apply the PLR
        *CoilTotalHeatingEnergyRatePtr *= HPPartLoadRatio;
        // Tank Calculation
        this->CalcWaterThermalTankStratified(state);
        // Restore the original value
        *CoilTotalHeatingEnergyRatePtr = CoilTotalHeatingEnergyRateBackup;
    }
    Real64 NewTankTemp = this->GetHPWHSensedTankTemp(state);
    return Par(1) - NewTankTemp;
}

bool WaterThermalTankData::SourceHeatNeed(EnergyPlusData &state, Real64 const OutletTemp, Real64 const DeadBandTemp, Real64 const SetPointTemp_loc)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Yueyue Zhou
    //       DATE WRITTEN   May 2019
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Determine by tank type, tank temperature and control mode if source side flow is needed

    // return value initialization
    bool NeedsHeatOrCool = false;

    if (!this->IsChilledWaterTank) {
        if (this->SourceSideControlMode == SourceSideEnum::IndirectHeatPrimarySetpoint) {
            if (OutletTemp < DeadBandTemp) {
                NeedsHeatOrCool = true;
            } else if ((OutletTemp >= DeadBandTemp) && (OutletTemp < SetPointTemp_loc)) {
                // inside the deadband, use saved mode from water heater calcs
                if (this->SavedMode == state.dataWaterThermalTanks->heatMode) {
                    NeedsHeatOrCool = true;
                } else if (this->SavedMode == state.dataWaterThermalTanks->floatMode) {
                    NeedsHeatOrCool = false;
                }

            } else if (OutletTemp >= SetPointTemp_loc) {
                NeedsHeatOrCool = false;
            }
        } else if (this->SourceSideControlMode == SourceSideEnum::IndirectHeatAltSetpoint) {
            // get alternate setpoint
            Real64 const AltSetpointTemp = ScheduleManager::GetCurrentScheduleValue(state, this->SourceSideAltSetpointSchedNum);
            Real64 const AltDeadBandTemp = AltSetpointTemp - this->DeadBandDeltaTemp;
            if (OutletTemp < AltDeadBandTemp) {
                NeedsHeatOrCool = true;
            } else if ((OutletTemp >= AltDeadBandTemp) && (OutletTemp < AltSetpointTemp)) {
                // inside the deadband, use saved mode from water heater calcs
                if (this->SavedMode == state.dataWaterThermalTanks->heatMode) {
                    NeedsHeatOrCool = true;
                } else if (this->SavedMode == state.dataWaterThermalTanks->floatMode) {
                    NeedsHeatOrCool = false;
                }

            } else if (OutletTemp >= AltSetpointTemp) {
                NeedsHeatOrCool = false;
            }
        } else if (this->SourceSideControlMode == SourceSideEnum::StorageTank) {
            if (OutletTemp < this->TankTempLimit) {
                NeedsHeatOrCool = true;
            } else {
                NeedsHeatOrCool = false;
            }
        }
    } else { // is a chilled water tank so flip logic
        if (OutletTemp > DeadBandTemp) {
            NeedsHeatOrCool = true;
        } else if ((OutletTemp <= DeadBandTemp) && (OutletTemp > SetPointTemp_loc)) {
            // inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
            if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankMixed) {
                if (this->SavedMode == state.dataWaterThermalTanks->coolMode) {
                    NeedsHeatOrCool = true;
                } else if (this->SavedMode == state.dataWaterThermalTanks->floatMode) {
                    NeedsHeatOrCool = false;
                }
            } else if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {
                NeedsHeatOrCool = true;
            }

        } else if (OutletTemp <= SetPointTemp_loc) {
            NeedsHeatOrCool = false;
        }
    }
    return NeedsHeatOrCool;
}

Real64 WaterThermalTankData::PlantMassFlowRatesFunc(EnergyPlusData &state,
                                                    int const InNodeNum,
                                                    bool const FirstHVACIteration,
                                                    SideEnum const WaterThermalTankSide,
                                                    int const PlantLoopSide,
                                                    [[maybe_unused]] bool const PlumbedInSeries,
                                                    DataBranchAirLoopPlant::ControlTypeEnum const BranchControlType,
                                                    Real64 const OutletTemp,
                                                    Real64 const DeadBandTemp,
                                                    Real64 const SetPointTemp_loc)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   October 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // collect routines for setting flow rates for Water heaters
    // with plant connections.

    int const PassingFlowThru(1);
    int const MaybeRequestingFlow(2);
    int const ThrottlingFlow(3);

    // determine current mode.  there are three possible
    //  1.  passing thru what was given to inlet node
    //  2.  potentially making a flow request
    //  3.  throttling flow in response to Plant's restrictions (MassFlowRateMaxAvail)
    // init default mode to passing thru
    int CurrentMode = PassingFlowThru; // default

    if (PlantLoopSide == DataPlant::DemandSupply_No) {
        CurrentMode = PassingFlowThru;
    } else if (PlantLoopSide == DataPlant::SupplySide) {
        // If FlowLock is False (0), the tank sets the plant loop mdot
        // If FlowLock is True (1),  the new resolved plant loop mdot is used
        if (this->UseCurrentFlowLock == DataPlant::iFlowLock::Unlocked) {
            CurrentMode = PassingFlowThru;
            if ((this->UseSideLoadRequested > 0.0) && (WaterThermalTankSide == SideEnum::Use)) {
                CurrentMode = MaybeRequestingFlow;
            }
        } else {
            CurrentMode = PassingFlowThru;
        }
        if (WaterThermalTankSide == SideEnum::Source) {
            CurrentMode = MaybeRequestingFlow;
        }
    } else if (PlantLoopSide == DataPlant::DemandSide) {

        //  2.  Might be Requesting Flow.
        if (FirstHVACIteration) {
            if (BranchControlType == DataBranchAirLoopPlant::ControlTypeEnum::Bypass) {
                CurrentMode = PassingFlowThru;
            } else {
                CurrentMode = MaybeRequestingFlow;
            }
        } else {
            if (BranchControlType == DataBranchAirLoopPlant::ControlTypeEnum::Bypass) {
                CurrentMode = PassingFlowThru;
            } else {
                CurrentMode = ThrottlingFlow;
            }
        }
    }

    // evaluate Availability schedule,
    bool ScheduledAvail = true;
    if (WaterThermalTankSide == SideEnum::Use) {
        if (ScheduleManager::GetCurrentScheduleValue(state, this->UseSideAvailSchedNum) == 0.0) {
            ScheduledAvail = false;
        }
    } else if (WaterThermalTankSide == SideEnum::Source) {
        if (ScheduleManager::GetCurrentScheduleValue(state, this->SourceSideAvailSchedNum) == 0.0) {
            ScheduledAvail = false;
        }
    }

    // now act based on current mode
    Real64 FlowResult = 0.0;
    {
        auto const SELECT_CASE_var(CurrentMode);

        if (SELECT_CASE_var == PassingFlowThru) {
            if (!ScheduledAvail) {
                FlowResult = 0.0;
            } else {
                FlowResult = state.dataLoopNodes->Node(InNodeNum).MassFlowRate;
            }

        } else if (SELECT_CASE_var == ThrottlingFlow) {
            // first determine what mass flow would be if it is to requested
            Real64 MassFlowRequest = 0.0;
            if (!ScheduledAvail) {
                MassFlowRequest = 0.0;
            } else {
                if (WaterThermalTankSide == SideEnum::Use) {
                    MassFlowRequest = this->PlantUseMassFlowRateMax;
                } else if (WaterThermalTankSide == SideEnum::Source) {
                    MassFlowRequest = this->PlantSourceMassFlowRateMax;
                } else {
                    assert(false);
                }
            }

            // next determine if tank temperature is such that source side flow might be requested
            bool NeedsHeatOrCool = this->SourceHeatNeed(state, OutletTemp, DeadBandTemp, SetPointTemp_loc);

            if (MassFlowRequest > 0.0) {
                if (WaterThermalTankSide == SideEnum::Use) {
                    FlowResult = MassFlowRequest;
                } else if (WaterThermalTankSide == SideEnum::Source) {
                    if (NeedsHeatOrCool) {
                        FlowResult = MassFlowRequest;
                    } else {
                        FlowResult = 0.0;
                    }
                } else {
                    assert(false);
                }
            } else {
                FlowResult = 0.0;
            }

            // now throttle against MassFlowRateMaxAvail, MassFlowRateMinAvail, MassFlowRateMax, and MassFlowRateMin
            // see notes about reverse dd compliance (specifically 5ZoneWaterSystems file)
            FlowResult = max(state.dataLoopNodes->Node(InNodeNum).MassFlowRateMinAvail, FlowResult); // okay for compliance (reverse dd)
            FlowResult = max(state.dataLoopNodes->Node(InNodeNum).MassFlowRateMin, FlowResult);      // okay for compliance (reverse dd)
            FlowResult = min(state.dataLoopNodes->Node(InNodeNum).MassFlowRateMaxAvail, FlowResult);
            //=> following might take out of reverse dd compliance
            FlowResult = min(state.dataLoopNodes->Node(InNodeNum).MassFlowRateMax, FlowResult);

        } else if (SELECT_CASE_var == MaybeRequestingFlow) {

            // first determine what mass flow would be if it is to requested
            Real64 MassFlowRequest = 0.0;
            if (!ScheduledAvail) {
                MassFlowRequest = 0.0;
            } else {
                if (WaterThermalTankSide == SideEnum::Use) {
                    if ((this->IsChilledWaterTank) && (this->UseSideLoadRequested > 0.0)) {
                        MassFlowRequest = this->PlantUseMassFlowRateMax;
                    } else if ((this->IsChilledWaterTank) && (this->UseSideLoadRequested == 0.0)) {
                        MassFlowRequest = 0.0;
                    } else {
                        MassFlowRequest = this->PlantUseMassFlowRateMax;
                    }

                } else if (WaterThermalTankSide == SideEnum::Source) {
                    MassFlowRequest = this->PlantSourceMassFlowRateMax;
                }
            }

            if (WaterThermalTankSide == SideEnum::Source) { // temperature dependent controls for indirect heating/cooling
                bool NeedsHeatOrCool = this->SourceHeatNeed(state, OutletTemp, DeadBandTemp, SetPointTemp_loc);
                if (MassFlowRequest > 0.0) {
                    if (NeedsHeatOrCool) {
                        FlowResult = MassFlowRequest;
                    } else {
                        FlowResult = 0.0;
                    }
                } else {
                    FlowResult = 0.0;
                }
            } else { // end source side, begin use side
                if (MassFlowRequest > 0.0) {
                    FlowResult = MassFlowRequest;
                } else {
                    FlowResult = 0.0;
                }
            }
        }
    }

    return FlowResult;
}

void WaterThermalTankData::MinePlantStructForInfo(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   October 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // get information from plant loop data structure
    // check what we can learn from plant structure against user inputs

    bool ErrorsFound = false;

    if (allocated(state.dataPlnt->PlantLoop) && this->UseSide.loopNum > 0) {

        // check plant structure for useful data.

        int PlantLoopNum = this->UseSide.loopNum;
        int LoopSideNum = this->UseSide.loopSideNum;

        if ((this->UseDesignVolFlowRateWasAutoSized) && (this->UseSidePlantSizNum == 0)) {
            ShowSevereError(state,
                            "Water heater = " + this->Name + " for autosizing Use side flow rate, did not find Sizing:Plant object " +
                                state.dataPlnt->PlantLoop(PlantLoopNum).Name);
            ErrorsFound = true;
        }
        // Is this wh Use side plumbed in series (default) or are there other branches in parallel?
        if (state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.Exists) {
            if (any_eq(state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.NodeNumOut,
                       this->UseInletNode)) { // this wh is on the splitter
                if (state.dataPlnt->PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.TotalOutletNodes > 1) {
                    this->UseSideSeries = false;
                }
            }
        }
    }

    if (allocated(state.dataPlnt->PlantLoop) && this->SrcSide.loopNum > 0) {
        // was user's input correct for plant loop name?
        if ((this->SourceDesignVolFlowRateWasAutoSized) && (this->SourceSidePlantSizNum == 0) && (this->DesuperheaterNum == 0) &&
            (this->HeatPumpNum == 0)) {
            ShowSevereError(state,
                            "Water heater = " + this->Name + "for autosizing Source side flow rate, did not find Sizing:Plant object " +
                                state.dataPlnt->PlantLoop(this->SrcSide.loopNum).Name);
            ErrorsFound = true;
        }
        // Is this wh Source side plumbed in series (default) or are there other branches in parallel?
        if (state.dataPlnt->PlantLoop(this->SrcSide.loopNum).LoopSide(this->SrcSide.loopSideNum).Splitter.Exists) {
            if (any_eq(state.dataPlnt->PlantLoop(this->SrcSide.loopNum).LoopSide(this->SrcSide.loopSideNum).Splitter.NodeNumOut,
                       this->SourceInletNode)) { // this wh is on the splitter
                if (state.dataPlnt->PlantLoop(this->SrcSide.loopNum).LoopSide(this->SrcSide.loopSideNum).Splitter.TotalOutletNodes > 1) {
                    this->SourceSideSeries = false;
                }
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding water heater input errors cause program termination");
    }
}

void WaterThermalTankData::SizeSupplySidePlantConnections(EnergyPlusData &state, Optional_int_const LoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   October 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing water heater plant connection flow rates
    // on the supply that have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    // This routine is called later in the simulation than the sizing routine for the demand side
    //  because the simulation needs to be further along before the needed data are available.
    // For water heaters sides on Supply LoopSide, obtains hot water flow rate from the plant sizing array
    //  (adapted approach from boiler sizing routines)

    static constexpr std::string_view RoutineName("SizeSupplySidePlantConnections");

    auto &PlantSizData(state.dataSize->PlantSizData);

    Real64 tmpUseDesignVolFlowRate = this->UseDesignVolFlowRate;
    Real64 tmpSourceDesignVolFlowRate = this->SourceDesignVolFlowRate;

    int tmpLoopNum;
    if (!present(LoopNum)) {
        tmpLoopNum = this->SrcSide.loopNum;
    } else {
        tmpLoopNum = LoopNum;
    }

    if ((this->UseInletNode > 0) && (tmpLoopNum == this->UseSide.loopNum)) {
        if (this->UseDesignVolFlowRateWasAutoSized) {
            int PltSizNum = this->UseSidePlantSizNum;
            if (PltSizNum > 0) { // we have a Plant Sizing Object
                if (this->UseSide.loopSideNum == DataPlant::SupplySide) {
                    if (PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->UseDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                        } else {
                            tmpUseDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                        }
                    } else {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->UseDesignVolFlowRate = 0.0;
                        } else {
                            tmpUseDesignVolFlowRate = 0.0;
                        }
                    }
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Use Side Design Flow Rate [m3/s]", this->UseDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->Type, this->Name, "Initial Use Side Design Flow Rate [m3/s]", this->UseDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->UseInletNode, this->UseDesignVolFlowRate);
                    } else {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->UseInletNode, tmpUseDesignVolFlowRate);
                    }

                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                                   DataGlobalConstants::InitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                                   RoutineName);
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
                    } else {
                        this->PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
                    }
                }
            } else {
                // do nothing
            } // plant sizing object
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->UseInletNode, this->UseDesignVolFlowRate);
            Real64 rho;
            if (this->UseSide.loopNum > 0) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                        DataGlobalConstants::InitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                        RoutineName);
            } else {
                rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->waterIndex, RoutineName);
            }

            this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;

        } // autosizing needed.
    }     // connected to plant

    if ((this->SourceInletNode > 0) && (tmpLoopNum == this->SrcSide.loopNum)) {
        if (this->SourceDesignVolFlowRateWasAutoSized) {
            int PltSizNum = this->SourceSidePlantSizNum;
            if (PltSizNum > 0) {
                if (this->SrcSide.loopSideNum == DataPlant::SupplySide) {
                    if (PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->SourceDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                        } else {
                            tmpSourceDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                        }
                    } else {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->SourceDesignVolFlowRate = 0.0;
                        } else {
                            tmpSourceDesignVolFlowRate = 0.0;
                        }
                    }
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->Type, this->Name, "Source Side Design Flow Rate [m3/s]", this->SourceDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->Type, this->Name, "Initial Source Side Design Flow Rate [m3/s]", this->SourceDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->SourceInletNode, this->SourceDesignVolFlowRate);
                    } else {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->SourceInletNode, tmpSourceDesignVolFlowRate);
                    }
                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                                   DataGlobalConstants::InitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                                   RoutineName);
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
                    } else {
                        this->PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
                    }
                } // plant loop allocation
            } else {
                // do nothing
            } // plant sizing object
        } else {
            if (this->SrcSide.loopSideNum == DataPlant::SupplySide) {
                PlantUtilities::RegisterPlantCompDesignFlow(state, this->SourceInletNode, this->SourceDesignVolFlowRate);
                Real64 rho;
                if (this->SrcSide.loopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                            DataGlobalConstants::InitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                            RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->waterIndex, RoutineName);
                }
                this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
            }
        } // autosizing needed.
    }     // connected to plant
}

void WaterThermalTankData::SizeTankForDemandSide(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing water heater tank volume and heater
    //  as best we can at this point in simulation. (prior to demand side
    //  sizing that needs volume).

    // METHODOLOGY EMPLOYED:
    //  depending on the sizing design mode...

    // REFERENCES:
    // BA benchmark report for residential design mode

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizeTankForDemandSide");
    Real64 const GalTocubicMeters(0.0037854);
    Real64 const kBtuPerHrToWatts(293.1);

    Real64 Tstart = 14.44;
    Real64 Tfinish = 57.22;

    Real64 tmpTankVolume = this->Volume;
    Real64 tmpMaxCapacity = this->MaxCapacity;

    {
        auto const SELECT_CASE_var(this->Sizing.DesignMode);

        if (SELECT_CASE_var == SizeEnum::NotSet) {

        } else if (SELECT_CASE_var == SizeEnum::PeakDraw) {

        } else if (SELECT_CASE_var == SizeEnum::ResidentialMin) {

            // assume can propagate rules for gas to other fuels.
            bool FuelTypeIsLikeGas = false;
            if (UtilityRoutines::SameString(this->FuelType, "NaturalGas")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "Diesel")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "Gasoline")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "Coal")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "FuelOilNo1")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "FuelOilNo2")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "Propane")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "Steam")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "OtherFuel1")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "OtherFuel2")) {
                FuelTypeIsLikeGas = true;
            } else if (UtilityRoutines::SameString(this->FuelType, "DistrictHeating")) {
                FuelTypeIsLikeGas = true;
            }

            if (this->Sizing.NumberOfBedrooms == 1) {
                if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                    if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 2.5 * 1000.0; // 2.5 kW
                } else if (FuelTypeIsLikeGas) {
                    if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; // 27kBtu/hr
                }

            } else if (this->Sizing.NumberOfBedrooms == 2) {
                if (this->Sizing.NumberOfBathrooms <= 1.5) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 3.5 * 1000.0; // 3.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                    }
                } else if ((this->Sizing.NumberOfBathrooms > 1.5) && (this->Sizing.NumberOfBathrooms < 3.0)) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                    }
                } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                    }
                }
            } else if (this->Sizing.NumberOfBedrooms == 3) {
                if (this->Sizing.NumberOfBathrooms <= 1.5) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                    }
                } else if ((this->Sizing.NumberOfBathrooms > 1.5) && (this->Sizing.NumberOfBathrooms < 3.0)) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                    }
                } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                    }
                }
            } else if (this->Sizing.NumberOfBedrooms == 4) {
                if (this->Sizing.NumberOfBathrooms <= 1.5) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                    }
                } else if ((this->Sizing.NumberOfBathrooms > 1.5) && (this->Sizing.NumberOfBathrooms < 3.0)) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                    }
                } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                    }
                }
            } else if (this->Sizing.NumberOfBedrooms == 5) {
                if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                    if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                } else if (FuelTypeIsLikeGas) {
                    if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; // 47 kBtu/hr
                }
            } else if (this->Sizing.NumberOfBedrooms >= 6) {
                if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                    if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                } else if (FuelTypeIsLikeGas) {
                    if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; // 50 kBtu/hr
                }
            }

            if (this->VolumeWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->Volume = tmpTankVolume;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Volume [m3]", this->Volume);
                }
            }
            if (this->MaxCapacityWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->MaxCapacity = tmpMaxCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        } else if (SELECT_CASE_var == SizeEnum::PerPerson) {
            // how to get number of people?

            Real64 SumPeopleAllZones = sum(state.dataHeatBal->Zone, &DataHeatBalance::ZoneData::TotOccupants);
            if (this->VolumeWasAutoSized) tmpTankVolume = this->Sizing.TankCapacityPerPerson * SumPeopleAllZones;

            if (this->MaxCapacityWasAutoSized) {
                Real64 rho;
                Real64 Cp;
                if (this->UseSide.loopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                            ((Tfinish + Tstart) / 2.0),
                                                            state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                            RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                                RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                }

                tmpMaxCapacity = SumPeopleAllZones * this->Sizing.RecoveryCapacityPerPerson * (Tfinish - Tstart) *
                                 (1.0 / DataGlobalConstants::SecInHour) * rho * Cp; // m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
            }

            if (this->VolumeWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->Volume = tmpTankVolume;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Volume [m3]", this->Volume);
                }
            }
            if (this->MaxCapacityWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->MaxCapacity = tmpMaxCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        } else if (SELECT_CASE_var == SizeEnum::PerFloorArea) {

            Real64 SumFloorAreaAllZones = sum(state.dataHeatBal->Zone, &DataHeatBalance::ZoneData::FloorArea);
            if (this->VolumeWasAutoSized) tmpTankVolume = this->Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
            if (this->MaxCapacityWasAutoSized) {
                Real64 rho;
                Real64 Cp;
                if (this->UseSide.loopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                            ((Tfinish + Tstart) / 2.0),
                                                            state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                            RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                                RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                }
                tmpMaxCapacity = SumFloorAreaAllZones * this->Sizing.RecoveryCapacityPerArea * (Tfinish - Tstart) *
                                 (1.0 / DataGlobalConstants::SecInHour) * rho * Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
            }
            if (this->VolumeWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->Volume = tmpTankVolume;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Volume [m3]", this->Volume);
                }
            }
            if (this->MaxCapacityWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->MaxCapacity = tmpMaxCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        } else if (SELECT_CASE_var == SizeEnum::PerUnit) {

            if (this->VolumeWasAutoSized) tmpTankVolume = this->Sizing.TankCapacityPerUnit * this->Sizing.NumberOfUnits;

            if (this->MaxCapacityWasAutoSized) {
                Real64 rho;
                Real64 Cp;
                if (this->UseSide.loopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                            ((Tfinish + Tstart) / 2.0),
                                                            state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                            RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                                RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                }
                tmpMaxCapacity = this->Sizing.NumberOfUnits * this->Sizing.RecoveryCapacityPerUnit * (Tfinish - Tstart) *
                                 (1.0 / DataGlobalConstants::SecInHour) * rho * Cp; // m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
            }

            if (this->VolumeWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->Volume = tmpTankVolume;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Volume [m3]", this->Volume);
                }
            }
            if (this->MaxCapacityWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->MaxCapacity = tmpMaxCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        } else if (SELECT_CASE_var == SizeEnum::PerSolarColArea) {
        }
    }

    // if stratified, might set height.
    if ((this->VolumeWasAutoSized) && (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) &&
        state.dataPlnt->PlantFirstSizesOkayToFinalize) { // might set height
        if ((this->HeightWasAutoSized) && (!this->VolumeWasAutoSized)) {
            this->Height = std::pow((4.0 * this->Volume * pow_2(this->Sizing.HeightAspectRatio)) / DataGlobalConstants::Pi, 0.3333333333333333);
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Height [m]", this->Height);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Height [m]", this->Height);
            }
            // check if DataGlobalConstants::AutoCalculate() Use outlet and source inlet are still set to autosize by earlier
            if (this->UseOutletHeightWasAutoSized) {
                this->UseOutletHeight = this->Height;
            }
            if (this->SourceInletHeightWasAutoSized) {
                this->SourceInletHeight = this->Height;
            }
        }
    }
}

void WaterThermalTankData::SizeTankForSupplySide(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing water heater tank volume and heater
    //   at a later point in the simulation when more of the plant is ready.

    // METHODOLOGY EMPLOYED:
    //  depending on the sizing design mode...

    // REFERENCES:
    // BA benchmark report for residential design mode

    static constexpr std::string_view RoutineName("SizeTankForSupplySide");

    Real64 Tstart = 14.44;
    Real64 Tfinish = 57.22;

    Real64 tmpTankVolume = this->Volume;
    Real64 tmpMaxCapacity = this->MaxCapacity;

    {
        auto const SELECT_CASE_var(this->Sizing.DesignMode);

        if (SELECT_CASE_var == SizeEnum::PeakDraw) {
            if (this->VolumeWasAutoSized)
                tmpTankVolume =
                    this->Sizing.TankDrawTime * this->UseDesignVolFlowRate * DataGlobalConstants::SecInHour; // hours | m3/s | (3600 s/1 hour)
            if (this->VolumeWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->Volume = tmpTankVolume;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Volume [m3]", this->Volume);
                }
            }
            if (this->MaxCapacityWasAutoSized) {
                if (this->Sizing.RecoveryTime > 0.0) {
                    Real64 rho;
                    Real64 Cp;
                    if (this->SrcSide.loopNum > 0) {
                        rho = FluidProperties::GetDensityGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                                RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                                    ((Tfinish + Tstart) / 2.0),
                                                                    state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                                    RoutineName);
                    } else {
                        rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    }
                    tmpMaxCapacity = (this->Volume * rho * Cp * (Tfinish - Tstart)) /
                                     (this->Sizing.RecoveryTime * DataGlobalConstants::SecInHour); // m3 | kg/m3 | J/Kg/K | K | seconds
                } else {
                    ShowFatalError(state,
                                   "SizeTankForSupplySide: Tank=\"" + this->Name +
                                       "\", requested sizing for max capacity but entered Recovery Time is zero.");
                }
            }

            if (this->MaxCapacityWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->MaxCapacity = tmpMaxCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        } else if (SELECT_CASE_var == SizeEnum::PerSolarColArea) {

            this->Sizing.TotalSolarCollectorArea = 0.0;
            for (int CollectorNum = 1; CollectorNum <= state.dataSolarCollectors->NumOfCollectors; ++CollectorNum) {
                this->Sizing.TotalSolarCollectorArea += state.dataSurface->Surface(state.dataSolarCollectors->Collector(CollectorNum).Surface).Area;
            }

            if (this->VolumeWasAutoSized) tmpTankVolume = this->Sizing.TotalSolarCollectorArea * this->Sizing.TankCapacityPerCollectorArea;
            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 0.0;
            if (this->VolumeWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->Volume = tmpTankVolume;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Volume [m3]", this->Volume);
                }
            }
            if (this->MaxCapacityWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->MaxCapacity = tmpMaxCapacity;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        }
    }

    if ((this->VolumeWasAutoSized) && (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) &&
        state.dataPlnt->PlantFirstSizesOkayToFinalize) { // might set height
        if ((this->HeightWasAutoSized) && (!this->VolumeWasAutoSized)) {
            this->Height = std::pow((4.0 * this->Volume * pow_2(this->Sizing.HeightAspectRatio)) / DataGlobalConstants::Pi, 0.3333333333333333);
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Height [m]", this->Height);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Initial Tank Height [m]", this->Height);
            }
        }
    }
}

void WaterThermalTankData::SizeDemandSidePlantConnections(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   October 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing water heater plant connection flow rates
    // on the demand side that have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    // For water heater sides on the Demand side, hot water flow rates are modeled entirely from user input data
    // because the plant loop is not yet set up nor is plant sizing info populated.
    // sizing is done by calculating an initial
    //  recovery rate that if continued would reheat tank in user specified amount of time.
    //  initial and final tank temperatures are 14.44 and reheat to 57.22 (values from CalcStandardRatings routine)

    static constexpr std::string_view RoutineName("SizeDemandSidePlantConnections");

    auto &PlantSizData(state.dataSize->PlantSizData);

    Real64 tankRecoverhours = this->SizingRecoveryTime;
    bool ErrorsFound = false;
    Real64 tmpUseDesignVolFlowRate = this->UseDesignVolFlowRate;
    Real64 tmpSourceDesignVolFlowRate = this->SourceDesignVolFlowRate;

    Real64 Tstart;
    Real64 Tfinish;
    if (!this->IsChilledWaterTank) {
        Tstart = 14.44;
        Tfinish = 57.22;
    } else {
        Tstart = 14.44;
        Tfinish = 9.0;
    }

    // determine tank volume to use for sizing.
    Real64 TankVolume = this->Volume;
    if (this->VolumeWasAutoSized) {
        TankVolume = this->Sizing.NominalVolForSizingDemandSideFlow;
    }

    if (this->UseInletNode > 0) {
        if (this->UseDesignVolFlowRateWasAutoSized) {
            int PltSizNum = this->UseSidePlantSizNum;
            if (PltSizNum > 0) { // we have a Plant Sizing Object
                if (this->UseSide.loopSideNum == DataPlant::DemandSide) {
                    // probably shouldn't come here as Use side is unlikley to be on demand side (?)
                    // but going to treat component with symetry so if connections are reversed it'll still work
                    // choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
                    // in user specified hours.
                    //  using the plant inlet design temp for sizing.
                    Real64 Tpdesign = PlantSizData(PltSizNum).ExitTemp;
                    Real64 eff = this->UseEffectiveness;
                    if ((Tpdesign >= 58.0) && (!this->IsChilledWaterTank)) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->UseDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                         std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        } else {
                            tmpUseDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                      std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        }
                    } else if ((Tpdesign <= 8.0) && (this->IsChilledWaterTank)) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->UseDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                         std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        } else {
                            tmpUseDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                      std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        }
                    } else {
                        if (!this->IsChilledWaterTank) {
                            // plant sizing object design temperature is set too low throw warning.
                            ShowSevereError(state,
                                            "Autosizing of Use side water heater design flow rate requires Sizing:Plant object to have an exit "
                                            "temperature >= 58C");
                            ShowContinueError(state, "Occurs for water heater object=" + this->Name);
                        } else {
                            // plant sizing object design temperature is set too hi throw warning.
                            ShowSevereError(state,
                                            "Autosizing of Use side chilled water tank design flow rate requires Sizing:Plant object to have an "
                                            "exit temperature <= 8C");
                            ShowContinueError(state, "Occurs for chilled water storage tank object=" + this->Name);
                        }
                        ErrorsFound = true;
                    }
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Use Side Design Flow Rate [m3/s]", this->UseDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->Type, this->Name, "Initial Use Side Design Flow Rate [m3/s]", this->UseDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->UseInletNode, this->UseDesignVolFlowRate);
                    } else {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->UseInletNode, tmpUseDesignVolFlowRate);
                    }
                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                                   DataGlobalConstants::InitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                                   RoutineName);
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
                    } else {
                        this->PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
                    }
                } // Demand side
            } else {
                // do nothing
            } // plant sizing object

        } else {
            // not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->UseInletNode, this->UseDesignVolFlowRate);
            Real64 rho;
            if (this->UseSide.loopNum > 0) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidName,
                                                        DataGlobalConstants::InitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->UseSide.loopNum).FluidIndex,
                                                        RoutineName);
            } else {
                rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->waterIndex, RoutineName);
            }
            this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
        } // autosizing needed.
    }     // connected to plant

    if (this->SourceInletNode > 0) {
        if (this->SourceDesignVolFlowRateWasAutoSized) {
            int PltSizNum = this->SourceSidePlantSizNum;
            if (PltSizNum > 0) {
                if (this->SrcSide.loopSideNum == DataPlant::DemandSide) {
                    //  choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
                    // in user specified hours.
                    //  using the plant inlet design temp for sizing.
                    Real64 Tpdesign = PlantSizData(PltSizNum).ExitTemp;
                    Real64 eff = this->SourceEffectiveness;
                    if ((Tpdesign >= 58.0) && (!this->IsChilledWaterTank)) {

                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->SourceDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                            std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        } else {
                            tmpSourceDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                         std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        }
                    } else if ((Tpdesign <= 8.0) && (this->IsChilledWaterTank)) {
                        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                            this->SourceDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                            std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        } else {
                            tmpSourceDesignVolFlowRate = -1.0 * (TankVolume / (tankRecoverhours * DataGlobalConstants::SecInHour * eff)) *
                                                         std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                        }
                    } else {
                        if (!this->IsChilledWaterTank) {
                            // plant sizing object design temperature is set too low throw warning.
                            ShowSevereError(state,
                                            "Autosizing of Source side water heater design flow rate requires Sizing:Plant object to have an "
                                            "exit temperature >= 58C");
                            ShowContinueError(state, "Occurs for WaterHeater:Mixed object=" + this->Name);
                        } else {
                            // plant sizing object design temperature is set too hi throw warning.
                            ShowSevereError(state,
                                            "Autosizing of Source side chilled water tank design flow rate requires Sizing:Plant object to have "
                                            "an exit temperature <= 8C");
                            ShowContinueError(state, "Occurs for chilled water storage tank object=" + this->Name);
                        }
                        ErrorsFound = true;
                    }
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->Type, this->Name, "Source Side Design Flow Rate [m3/s]", this->SourceDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, this->Type, this->Name, "Initial Source Side Design Flow Rate [m3/s]", this->SourceDesignVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->SourceInletNode, this->SourceDesignVolFlowRate);
                    } else {
                        PlantUtilities::RegisterPlantCompDesignFlow(state, this->SourceInletNode, tmpSourceDesignVolFlowRate);
                    }
                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                                   DataGlobalConstants::InitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                                   RoutineName);
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
                    } else {
                        this->PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
                    }
                } // demand side
            } else {
                // do nothing
            } // plant sizing object

        } else {
            // not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->SourceInletNode, this->SourceDesignVolFlowRate);
            Real64 rho;
            if (this->SrcSide.loopNum > 0) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidName,
                                                        DataGlobalConstants::InitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->SrcSide.loopNum).FluidIndex,
                                                        RoutineName);
            } else {
                rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->waterIndex, RoutineName);
            }
            this->PlantSourceMassFlowRateMax = this->SourceDesignVolFlowRate * rho;
        } // autosizing needed.
    }     // connected to plant

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void WaterThermalTankData::SizeStandAloneWaterHeater(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   October 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // allow autosizing of tank volume and heat capacity for stand alone tanks

    // METHODOLOGY EMPLOYED:
    // same as for plant connected water heaters, only draws are scheduled.

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const GalTocubicMeters(0.0037854);
    Real64 const kBtuPerHrToWatts(293.1);
    static constexpr std::string_view RoutineName("SizeStandAloneWaterHeater");

    Real64 Tstart = 14.44;
    Real64 Tfinish = 57.22;
    Real64 tmpTankVolume = this->Volume;
    Real64 tmpMaxCapacity = this->MaxCapacity;

    if (this->VolumeWasAutoSized || this->MaxCapacityWasAutoSized) {

        {
            auto const SELECT_CASE_var(this->Sizing.DesignMode);

            if (SELECT_CASE_var == SizeEnum::PeakDraw) {
                // get draw rate from maximum in schedule
                Real64 rho =
                    FluidProperties::GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::InitConvTemp, this->waterIndex, RoutineName);
                Real64 DrawDesignVolFlowRate = ScheduleManager::GetScheduleMaxValue(state, this->FlowRateSchedule) * this->MassFlowRateMax / rho;

                if (this->VolumeWasAutoSized) {
                    tmpTankVolume =
                        this->Sizing.TankDrawTime * DrawDesignVolFlowRate * DataGlobalConstants::SecInHour; // hours | m3/s | (3600 s/1 hour)
                    this->Volume = tmpTankVolume;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (this->MaxCapacityWasAutoSized) {
                    if (this->Sizing.RecoveryTime > 0.0) {
                        rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                        Real64 Cp =
                            FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);

                        tmpMaxCapacity = (this->Volume * rho * Cp * (Tfinish - Tstart)) /
                                         (this->Sizing.RecoveryTime * DataGlobalConstants::SecInHour); // m3 | kg/m3 | J/Kg/K | K | seconds
                    } else {
                        ShowFatalError(state,
                                       "SizeStandAloneWaterHeater: Tank=\"" + this->Name +
                                           "\", requested sizing for max capacity but entered Recovery Time is zero.");
                    }
                    this->MaxCapacity = tmpMaxCapacity;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }

            } else if (SELECT_CASE_var == SizeEnum::ResidentialMin) {
                // assume can propagate rules for gas to other fuels.
                bool FuelTypeIsLikeGas = false;
                if (UtilityRoutines::SameString(this->FuelType, "NaturalGas")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Diesel")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Gasoline")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Coal")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "FuelOilNo1")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "FuelOilNo2")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Propane")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Steam")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "OtherFuel1")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "OtherFuel2")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "DistrictHeating")) {
                    FuelTypeIsLikeGas = true;
                }

                if (this->Sizing.NumberOfBedrooms == 1) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 2.5 * 1000.0; // 2.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; // 27kBtu/hr
                    }

                } else if (this->Sizing.NumberOfBedrooms == 2) {
                    if (this->Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 3.5 * 1000.0; // 3.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((this->Sizing.NumberOfBathrooms > 1.5) && (this->Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    }
                } else if (this->Sizing.NumberOfBedrooms == 3) {
                    if (this->Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((this->Sizing.NumberOfBathrooms > 1.5) && (this->Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    }
                } else if (this->Sizing.NumberOfBedrooms == 4) {
                    if (this->Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((this->Sizing.NumberOfBathrooms > 1.5) && (this->Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    }
                } else if (this->Sizing.NumberOfBedrooms == 5) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; // 47 kBtu/hr
                    }
                } else if (this->Sizing.NumberOfBedrooms >= 6) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electricity")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; // 50 kBtu/hr
                    }
                }
                if (this->VolumeWasAutoSized) {
                    this->Volume = tmpTankVolume;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (this->MaxCapacityWasAutoSized) {
                    this->MaxCapacity = tmpMaxCapacity;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }

            } else if (SELECT_CASE_var == SizeEnum::PerPerson) {
                // how to get number of people?

                Real64 SumPeopleAllZones = sum(state.dataHeatBal->Zone, &DataHeatBalance::ZoneData::TotOccupants);
                if (this->VolumeWasAutoSized) {
                    tmpTankVolume = this->Sizing.TankCapacityPerPerson * SumPeopleAllZones;
                }
                if (this->MaxCapacityWasAutoSized) {
                    Real64 rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    Real64 Cp =
                        FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    tmpMaxCapacity = SumPeopleAllZones * this->Sizing.RecoveryCapacityPerPerson * (Tfinish - Tstart) *
                                     (1.0 / DataGlobalConstants::SecInHour) * rho *
                                     Cp; // m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                }

                if (this->VolumeWasAutoSized) {
                    this->Volume = tmpTankVolume;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (this->MaxCapacityWasAutoSized) {
                    this->MaxCapacity = tmpMaxCapacity;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }

            } else if (SELECT_CASE_var == SizeEnum::PerFloorArea) {

                Real64 SumFloorAreaAllZones = sum(state.dataHeatBal->Zone, &DataHeatBalance::ZoneData::FloorArea);
                if (this->VolumeWasAutoSized) {
                    tmpTankVolume = this->Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
                }

                if (this->MaxCapacityWasAutoSized) {
                    Real64 rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    Real64 Cp =
                        FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    tmpMaxCapacity = SumFloorAreaAllZones * this->Sizing.RecoveryCapacityPerArea * (Tfinish - Tstart) *
                                     (1.0 / DataGlobalConstants::SecInHour) * rho *
                                     Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                }
                if (this->VolumeWasAutoSized) {
                    this->Volume = tmpTankVolume;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (this->MaxCapacityWasAutoSized) {
                    this->MaxCapacity = tmpMaxCapacity;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            } else if (SELECT_CASE_var == SizeEnum::PerUnit) {

                if (this->VolumeWasAutoSized) tmpTankVolume = this->Sizing.TankCapacityPerUnit * this->Sizing.NumberOfUnits;

                if (this->MaxCapacityWasAutoSized) {
                    Real64 rho = FluidProperties::GetDensityGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    Real64 Cp =
                        FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, ((Tfinish + Tstart) / 2.0), this->waterIndex, RoutineName);
                    tmpMaxCapacity = this->Sizing.NumberOfUnits * this->Sizing.RecoveryCapacityPerUnit * (Tfinish - Tstart) *
                                     (1.0 / DataGlobalConstants::SecInHour) * rho * Cp; // m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                }

                if (this->VolumeWasAutoSized) {
                    this->Volume = tmpTankVolume;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (this->MaxCapacityWasAutoSized) {
                    this->MaxCapacity = tmpMaxCapacity;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            } else if (SELECT_CASE_var == SizeEnum::PerSolarColArea) {
                this->Sizing.TotalSolarCollectorArea = 0.0;
                for (int CollectorNum = 1; CollectorNum <= state.dataSolarCollectors->NumOfCollectors; ++CollectorNum) {
                    this->Sizing.TotalSolarCollectorArea +=
                        state.dataSurface->Surface(state.dataSolarCollectors->Collector(CollectorNum).Surface).Area;
                }

                if (this->VolumeWasAutoSized) tmpTankVolume = this->Sizing.TotalSolarCollectorArea * this->Sizing.TankCapacityPerCollectorArea;
                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 0.0;
                if (this->VolumeWasAutoSized) {
                    this->Volume = tmpTankVolume;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Tank Volume [m3]", this->Volume);
                }
                if (this->MaxCapacityWasAutoSized) {
                    this->MaxCapacity = tmpMaxCapacity;
                    BaseSizer::reportSizerOutput(state, this->Type, this->Name, "Maximum Heater Capacity [W]", this->MaxCapacity);
                }
            }
        }
    }
}

void WaterThermalTankData::UpdateWaterThermalTank(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brandon Anderson
    //       DATE WRITTEN   May 2000
    //       MODIFIED       na
    //                      Nov 2011, BAN; removed the use and source heat rate re-calculation for stratified tank
    //                                     for energy conservation verification.
    //       RE-ENGINEERED  Feb 2004, PGE

    // PURPOSE OF THIS SUBROUTINE:
    // Updates the node variables with local variables.

    if (this->UseInletNode > 0 && this->UseOutletNode > 0) {
        state.dataLoopNodes->Node(UseOutletNode) = state.dataLoopNodes->Node(this->UseInletNode); // this could wipe out setpoints on outlet node

        state.dataLoopNodes->Node(this->UseOutletNode).Temp = this->UseOutletTemp;
    }

    if (this->SourceInletNode > 0 && this->SourceOutletNode > 0) {
        state.dataLoopNodes->Node(this->SourceOutletNode) = state.dataLoopNodes->Node(this->SourceInletNode);

        state.dataLoopNodes->Node(this->SourceOutletNode).Temp = this->SourceOutletTemp;
    }
}

void WaterThermalTankData::ReportWaterThermalTank(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brandon Anderson
    //       DATE WRITTEN   May 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  Feb 2004, PGE

    Real64 SecInTimeStep = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    this->UnmetEnergy = this->UnmetRate * SecInTimeStep;
    this->LossEnergy = this->LossRate * SecInTimeStep;
    this->FlueLossEnergy = this->FlueLossRate * SecInTimeStep;
    this->UseEnergy = this->UseRate * SecInTimeStep;
    this->TotalDemandEnergy = this->TotalDemandRate * SecInTimeStep;
    this->SourceEnergy = this->SourceRate * SecInTimeStep;
    this->HeaterEnergy = this->HeaterRate * SecInTimeStep;
    this->HeaterEnergy1 = this->HeaterRate1 * SecInTimeStep;
    this->HeaterEnergy2 = this->HeaterRate2 * SecInTimeStep;
    this->FuelEnergy = this->FuelRate * SecInTimeStep;
    this->VentEnergy = this->VentRate * SecInTimeStep;
    this->OffCycParaFuelEnergy = this->OffCycParaFuelRate * SecInTimeStep;
    this->OffCycParaEnergyToTank = this->OffCycParaRateToTank * SecInTimeStep;
    this->OnCycParaFuelEnergy = this->OnCycParaFuelRate * SecInTimeStep;
    this->OnCycParaEnergyToTank = this->OnCycParaRateToTank * SecInTimeStep;
    this->NetHeatTransferEnergy = this->NetHeatTransferRate * SecInTimeStep;
    this->VolumeConsumed = this->VolFlowRate * SecInTimeStep;
}

void WaterThermalTankData::CalcStandardRatings(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2005
    //       MODIFIED       R. Raustad, July 2005 - added HPWH to ratings procedure
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the water heater standard ratings, such as Energy Factor and Recovery Efficiency.  Results are written
    // to the EIO file.  Standard ratings are not calculated for storage-only tanks, i.e., MaxCapacity = 0, nor for Integrated Heat Pumps

    // METHODOLOGY EMPLOYED:
    // Water heater inputs are set to the specified test conditions. For HPWHs, the heating capacity and COP are assumed
    // to be the primary element in the water heater and are used during the rating procedure.  CalcWaterThermalTankMixed
    // is iteratively called in a self-contained, 24 hour simulation of the standard test procedure.

    // REFERENCES:
    // Title 10, Code of Federal Regulations, Part 430- Energy Conservation Program for Consumer Products, Appendix E to
    // Subpart B- Uniform Test Procedure for Measuring the Energy Consumption of Water Heaters, January 1, 2004.

    if (this->AlreadyRated) { // bail we already did this one
        return;
    }

    bool FirstTimeFlag; // used during HPWH rating procedure
    bool bIsVSCoil = false;
    Real64 RecoveryEfficiency;
    Real64 EnergyFactor;
    Real64 RatedDXCoilTotalCapacity = 0.0;
    if (this->MaxCapacity > 0.0 || this->HeatPumpNum > 0) {
        // Set test conditions
        this->AmbientTemp = 19.7222;   // 67.5 F
        this->UseInletTemp = 14.4444;  // 58 F
        this->SetPointTemp = 57.2222;  // 135 F
        this->SetPointTemp2 = 57.2222; // 135 F
        this->TankTemp = 57.2222;      // Initialize tank temperature
        if (this->Nodes > 0)
            for (auto &e : this->Node)
                e.Temp = 57.2222;

        Real64 TotalDrawMass = 0.243402 * Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp); // 64.3 gal * rho
        Real64 DrawMass = TotalDrawMass / 6.0;                                                       // 6 equal draws
        Real64 SecInTimeStep = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        Real64 DrawMassFlowRate = DrawMass / SecInTimeStep;
        Real64 FuelEnergy_loc = 0.0;
        FirstTimeFlag = true;

        int TimeStepPerHour = int(1.0 / state.dataHVACGlobal->TimeStepSys);
        // Simulate 24 hour test
        for (int Step = 1; Step <= TimeStepPerHour * 24; ++Step) {

            if (Step == 1 || Step == (1 + TimeStepPerHour) || Step == (1 + TimeStepPerHour * 2) || Step == (1 + TimeStepPerHour * 3) ||
                Step == (1 + TimeStepPerHour * 4) || Step == (1 + TimeStepPerHour * 5)) { // Hour 1 | Hour 2 | Hour 3 | Hour 4 | Hour 5 | Hour 6

                this->UseMassFlowRate = DrawMassFlowRate;
            } else {
                this->UseMassFlowRate = 0.0;
            }

            this->SavedTankTemp = this->TankTemp;
            this->SavedMode = this->Mode;
            if (this->Nodes > 0) {
                for (auto &e : this->Node)
                    e.SavedTemp = e.Temp;
                this->SavedHeaterOn1 = this->HeaterOn1;
                this->SavedHeaterOn2 = this->HeaterOn2;
            }

            if (this->HeatPumpNum == 0) {

                {
                    auto const SELECT_CASE_var(this->TypeNum);

                    if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                        this->CalcWaterThermalTankMixed(state);

                    } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                        this->CalcWaterThermalTankStratified(state);

                    } else {
                        //         Unhandled water heater type
                    }
                }

            } else {

                int HPNum = this->HeatPumpNum;  // Convenience variable
                Real64 AmbientHumRat = 0.00717; // Humidity ratio at 67.5 F / 50% RH

                //       set the heat pump air- and water-side mass flow rate
                Real64 MdotWater = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingWaterFlowRate * Psychrometrics::RhoH2O(this->TankTemp);
                Real64 mdotAir = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OperatingAirMassFlowRate;

                // ?? why is HPWH condenser inlet node temp reset inside the for loop? shouldn't it chnage with the tank temp throughout these
                // iterations?
                if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                    // set the condenser inlet node mass flow rate and temperature
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).CondWaterInletNode).MassFlowRate = MdotWater;
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).CondWaterInletNode).Temp = this->TankTemp;
                }

                //       initialize temperatures for HPWH DX Coil heating capacity and COP curves
                state.dataHVACGlobal->HPWHInletDBTemp = this->AmbientTemp;
                state.dataHVACGlobal->HPWHInletWBTemp =
                    Psychrometrics::PsyTwbFnTdbWPb(state, state.dataHVACGlobal->HPWHInletDBTemp, AmbientHumRat, state.dataEnvrn->OutBaroPress);

                //       set up full air flow on DX coil inlet node
                if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode > 0) {
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRate = mdotAir;
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRateMaxAvail = mdotAir;
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode).Temp = this->AmbientTemp;
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode).HumRat = AmbientHumRat;
                    state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).InletAirMixerNode).Enthalpy =
                        Psychrometrics::PsyHFnTdbW(this->AmbientTemp, AmbientHumRat);
                } else {
                    if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode == 0) {
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRate = mdotAir;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRateMaxAvail =
                            mdotAir;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirInletNode).Temp = this->AmbientTemp;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirInletNode).HumRat = AmbientHumRat;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).HeatPumpAirInletNode).Enthalpy =
                            Psychrometrics::PsyHFnTdbW(this->AmbientTemp, AmbientHumRat);
                    } else {
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode).MassFlowRate = mdotAir;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode).MassFlowRateMaxAvail = mdotAir;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode).Temp = this->AmbientTemp;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode).HumRat = AmbientHumRat;
                        state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).OutsideAirNode).Enthalpy =
                            Psychrometrics::PsyHFnTdbW(this->AmbientTemp, AmbientHumRat);
                    }
                }

                state.dataHVACGlobal->HPWHCrankcaseDBTemp = this->AmbientTemp;

                if (UtilityRoutines::SameString(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilType,
                                                "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") ||
                    (state.dataWaterThermalTanks->HPWaterHeater(HPNum).bIsIHP)) {
                    bIsVSCoil = true;
                    std::string VSCoilName = state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName;
                    int VSCoilNum = state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum;
                    if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).bIsIHP) {
                        VSCoilNum =
                            state.dataIntegratedHP->IntegratedHeatPumps(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                        VSCoilName =
                            state.dataIntegratedHP->IntegratedHeatPumps(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).SCWHCoilName;
                    }

                    Real64 RhoWater = Psychrometrics::RhoH2O(this->TankTemp);
                    auto &HPWH = state.dataWaterThermalTanks->HPWaterHeater(HPNum);
                    this->SetVSHPWHFlowRates(
                        state,
                        HPWH,
                        state.dataVariableSpeedCoils->VarSpeedCoil(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                        1.0,
                        RhoWater,
                        MdotWater,
                        true);
                    //       simulate the HPWH coil/fan to find heating capacity
                    Real64 EMP1 = 0.0;
                    Real64 EMP2 = 0.0;
                    Real64 EMP3 = 0.0;
                    if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanPlacement == DataHVACGlobals::BlowThru) {
                        //   simulate fan and DX coil twice
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                        VariableSpeedCoils::SimVariableSpeedCoils(
                            state,
                            VSCoilName,
                            VSCoilNum,
                            DataHVACGlobals::CycFanCycCoil,
                            EMP1,
                            EMP2,
                            EMP3,
                            1,
                            1.0,
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                            1.0,
                            0.0,
                            0.0,
                            1.0);
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                        VariableSpeedCoils::SimVariableSpeedCoils(
                            state,
                            VSCoilName,
                            VSCoilNum,
                            DataHVACGlobals::CycFanCycCoil,
                            EMP1,
                            EMP2,
                            EMP3,
                            1,
                            1.0,
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                            1.0,
                            0.0,
                            0.0,
                            1.0);
                    } else {
                        //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                        VariableSpeedCoils::SimVariableSpeedCoils(
                            state,
                            VSCoilName,
                            VSCoilNum,
                            DataHVACGlobals::CycFanCycCoil,
                            EMP1,
                            EMP2,
                            EMP3,
                            1,
                            1.0,
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                            1.0,
                            0.0,
                            0.0,
                            1.0);
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                        VariableSpeedCoils::SimVariableSpeedCoils(
                            state,
                            VSCoilName,
                            VSCoilNum,
                            DataHVACGlobals::CycFanCycCoil,
                            EMP1,
                            EMP2,
                            EMP3,
                            1,
                            1.0,
                            state.dataVariableSpeedCoils->VarSpeedCoil(state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                            1.0,
                            0.0,
                            0.0,
                            1.0);
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                    }

                    this->MaxCapacity = state.dataVariableSpeedCoils->VSHPWHHeatingCapacity;
                    this->MinCapacity = state.dataVariableSpeedCoils->VSHPWHHeatingCapacity;
                    this->Efficiency = state.dataVariableSpeedCoils->VSHPWHHeatingCOP;
                } else {
                    bIsVSCoil = false;
                    //       simulate the HPWH coil/fan to find heating capacity
                    if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanPlacement == DataHVACGlobals::BlowThru) {
                        if (FirstTimeFlag) { // first time DXCoils::DXCoil is called, it's sized at the RatedCondenserWaterInlet temp, size and
                                             // reset water inlet temp. If already sized, no harm.
                            if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(state,
                                                            state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                            true,
                                                            state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                            }
                            DXCoils::SimDXCoil(state,
                                               state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                               1,
                                               true,
                                               state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               1.0);
                            state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).CondWaterInletNode).Temp = this->TankTemp;
                        }
                        // ?? should only need to call twice if PLR<1 since this might affect OnOffFanPartLoadFraction which impacts fan energy.
                        // PLR=1 here.
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                        DXCoils::SimDXCoil(state,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                           1,
                                           true,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           1.0);
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                        DXCoils::SimDXCoil(state,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                           1,
                                           true,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           1.0);
                    } else {
                        if (FirstTimeFlag) { // first time DXCoils::DXCoil is called, it's sized at the RatedCondenserWaterInlet temp, size and
                                             // reset water inlet temp. If already sized, no harm.
                            DXCoils::SimDXCoil(state,
                                               state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                               1,
                                               true,
                                               state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               1.0);
                            state.dataLoopNodes->Node(state.dataWaterThermalTanks->HPWaterHeater(HPNum).CondWaterInletNode).Temp = this->TankTemp;
                        }
                        // ?? should only need to call twice if PLR<1 since this might affect OnOffFanPartLoadFraction which impacts fan energy.
                        // PLR=1 here.
                        DXCoils::SimDXCoil(state,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                           1,
                                           true,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           1.0);
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                        DXCoils::SimDXCoil(state,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilName,
                                           1,
                                           true,
                                           state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilNum,
                                           DataHVACGlobals::CycFanCycCoil,
                                           1.0);
                        if (state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            state.dataHVACFan->fanObjs[state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum]->simulate(state, _, _, _, _);
                        } else {
                            Fans::SimulateFanComponents(state,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanName,
                                                        true,
                                                        state.dataWaterThermalTanks->HPWaterHeater(HPNum).FanNum);
                        }
                    }

                    this->MaxCapacity = state.dataDXCoils->HPWHHeatingCapacity;
                    this->MinCapacity = state.dataDXCoils->HPWHHeatingCapacity;
                    this->Efficiency = state.dataDXCoils->HPWHHeatingCOP;
                }

                if (FirstTimeFlag) {
                    RatedDXCoilTotalCapacity = state.dataHVACGlobal->DXCoilTotalCapacity;
                    FirstTimeFlag = false;
                }

                //       Switch the HPWH info with the tank info and call CalcWaterThermalTankMixed to get Standard Rating
                //       (backup element is assumed to be disabled during the rating procedure)
                this->SourceMassFlowRate = 0.0;
                this->OnCycParaLoad = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OnCycParaLoad;
                this->OffCycParaLoad = state.dataWaterThermalTanks->HPWaterHeater(HPNum).OffCycParaLoad;
                this->OffCycParaFracToTank = 0.0;
                this->OnCycParaFracToTank = 0.0;
                this->PLFCurve = state.dataWaterThermalTanks->HPWaterHeater(HPNum).DXCoilPLFFPLR;

                {
                    auto const SELECT_CASE_var(this->TypeNum);

                    if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                        if (this->Efficiency > 0.0) this->CalcWaterThermalTankMixed(state);

                    } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                        if (this->Efficiency > 0.0) this->CalcWaterThermalTankStratified(state);

                    } else {
                        //         Unhandled water heater type
                    }
                }

                //       reset the water heater data to original values
                this->MaxCapacity = state.dataWaterThermalTanks->HPWaterHeater(HPNum).BackupElementCapacity;
                this->MinCapacity = state.dataWaterThermalTanks->HPWaterHeater(HPNum).BackupElementCapacity;
                this->Efficiency = state.dataWaterThermalTanks->HPWaterHeater(HPNum).BackupElementEfficiency;
                this->OnCycParaLoad = state.dataWaterThermalTanks->HPWaterHeater(HPNum).WHOnCycParaLoad;
                this->OffCycParaLoad = state.dataWaterThermalTanks->HPWaterHeater(HPNum).WHOffCycParaLoad;
                this->OnCycParaFracToTank = state.dataWaterThermalTanks->HPWaterHeater(HPNum).WHOnCycParaFracToTank;
                this->OffCycParaFracToTank = state.dataWaterThermalTanks->HPWaterHeater(HPNum).WHOffCycParaFracToTank;
                this->PLFCurve = state.dataWaterThermalTanks->HPWaterHeater(HPNum).WHPLFCurve;
            }

            FuelEnergy_loc += (this->FuelRate + this->OffCycParaFuelRate + this->OnCycParaFuelRate) * SecInTimeStep;

        } // Step

        if (this->FirstRecoveryDone && this->FirstRecoveryFuel > 0.0) {
            // Calculate Recovery Efficiency based on energy used to recover from the first draw
            // FirstRecoveryFuel is recorded inside the CalcWaterThermalTank subroutine
            RecoveryEfficiency = DrawMass * Psychrometrics::CPHW(57.2222) * (57.2222 - 14.4444) / this->FirstRecoveryFuel;

            // Calculate Energy Factor based on total energy (including parasitics) used over entire test
            EnergyFactor = TotalDrawMass * Psychrometrics::CPHW(57.2222) * (57.2222 - 14.4444) / FuelEnergy_loc;

        } else {
            RecoveryEfficiency = 0.0;
            EnergyFactor = 0.0;
            // If this a regular tank, or an HPWH that's not an Integrated one
            if ((this->HeatPumpNum == 0) || !state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).bIsIHP) {
                ShowWarningError(state,
                                 "Water heater = " + this->Name +
                                     ":  Recovery Efficiency and Energy Factor could not be calculated during the test for standard ratings");
                ShowContinueError(state, "Setpoint was never recovered and/or heater never turned on");
            }
        }

    } else {

        // Storage-only tank
        RecoveryEfficiency = 0.0;
        EnergyFactor = 0.0;

    } // WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0

    // create predefined report
    // Store values for the input verification and summary report
    std::string equipName;
    if (this->HeatPumpNum == 0) {
        equipName = this->Name;
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHType, equipName, this->Type);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHVol, equipName, this->Volume);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHHeatIn, equipName, this->MaxCapacity);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHThEff, equipName, this->Efficiency);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHRecEff, equipName, RecoveryEfficiency);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHEnFac, equipName, EnergyFactor);
    } else {
        equipName = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Name;
        OutputReportPredefined::PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchSWHType, equipName, state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Type);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHVol, equipName, this->Volume);
        if (bIsVSCoil) {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchSWHHeatIn, equipName, state.dataVariableSpeedCoils->VSHPWHHeatingCapacity);
        } else {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchSWHHeatIn, equipName, state.dataDXCoils->HPWHHeatingCapacity);
        }
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHThEff, equipName, this->Efficiency);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHRecEff, equipName, RecoveryEfficiency);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchSWHEnFac, equipName, EnergyFactor);
    }

    // Write test results
    if (this->HeatPumpNum == 0) {
        Real64 MaxCapacity_loc;
        if (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
            if (this->ControlType == PriorityEnum::MasterSlave) {
                MaxCapacity_loc = max(this->MaxCapacity, this->MaxCapacity2);
            } else { // PrioritySimultaneous
                MaxCapacity_loc = this->MaxCapacity + this->MaxCapacity2;
            }
        } else { // WaterHeaterMixed
            MaxCapacity_loc = this->MaxCapacity;
        }

        static constexpr fmt::string_view Format_720("Water Heater Information,{},{},{:.4T},{:.1T},{:.3T},{:.4T}\n");
        print(state.files.eio, Format_720, this->Type, this->Name, this->Volume, MaxCapacity_loc, RecoveryEfficiency, EnergyFactor);
    } else {
        static constexpr fmt::string_view Format_721("Heat Pump Water Heater Information,{},{},{:.4T},{:.1T},{:.3T},{:.4T},{:.0T}\n");
        print(state.files.eio,
              Format_721,
              state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Type,
              state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum).Name,
              this->Volume,
              state.dataDXCoils->HPWHHeatingCapacity,
              RecoveryEfficiency,
              EnergyFactor,
              RatedDXCoilTotalCapacity);
    }

    this->AlreadyRated = true;
}

void WaterThermalTankData::ReportCWTankInits(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   March 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // send chilled water tank info to EIO

    if (this->myOneTimeInitFlag) {
        this->setupOutputVars(state);
        this->myOneTimeInitFlag = false;
    }

    if (this->AlreadyReported) { // bail we already did this one
        return;
    }

    static constexpr fmt::string_view Format_728("Chilled Water Tank Information,{},{},{:.4T},{:.4T},{:.4T}\n");
    print(state.files.eio, Format_728, this->Type, this->Name, this->Volume, this->UseDesignVolFlowRate, this->SourceDesignVolFlowRate);

    this->AlreadyReported = true;
}

Real64 WaterThermalTankData::FindStratifiedTankSensedTemp(EnergyPlusData &state, bool UseAverage)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  Noel Merket, April 2015

    // PURPOSE OF THIS FUNCTION:
    // find tank temperature depending on how sensed

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    HeatPumpWaterHeaterData const &HPWH = state.dataWaterThermalTanks->HPWaterHeater(this->HeatPumpNum);
    Real64 ControlSensor1Temp;
    Real64 ControlSensor2Temp;

    if (UseAverage) {
        ControlSensor1Temp = this->Node(HPWH.ControlSensor1Node).TempAvg;
        ControlSensor2Temp = this->Node(HPWH.ControlSensor2Node).TempAvg;
    } else {
        ControlSensor1Temp = this->Node(HPWH.ControlSensor1Node).Temp;
        ControlSensor2Temp = this->Node(HPWH.ControlSensor2Node).Temp;
    }

    Real64 SensedTemp = ControlSensor1Temp * HPWH.ControlSensor1Weight + ControlSensor2Temp * HPWH.ControlSensor2Weight;

    return SensedTemp;
}

Real64 WaterThermalTankData::getDeadBandTemp()
{
    if (this->IsChilledWaterTank) {
        return (this->SetPointTemp + this->DeadBandDeltaTemp);
    } else {
        return (this->SetPointTemp - this->DeadBandDeltaTemp);
    }
}

bool GetHeatPumpWaterHeaterNodeNumber(EnergyPlusData &state, int const NodeNumber)
{
    // PURPOSE OF THIS FUNCTION:
    // Check if a node is used by a heat pump water heater
    // and can be excluded from an airflow network.

    // Return value
    bool HeatPumpWaterHeaterNodeException;

    int HeatPumpWaterHeaterIndex;

    if (state.dataWaterThermalTanks->getWaterThermalTankInputFlag) {
        GetWaterThermalTankInput(state);
        state.dataWaterThermalTanks->getWaterThermalTankInputFlag = false;
    }

    HeatPumpWaterHeaterNodeException = false;

    for (HeatPumpWaterHeaterIndex = 1; HeatPumpWaterHeaterIndex <= state.dataWaterThermalTanks->numHeatPumpWaterHeater; ++HeatPumpWaterHeaterIndex) {

        // Get heat pump water heater data
        HeatPumpWaterHeaterData &HPWH = state.dataWaterThermalTanks->HPWaterHeater(HeatPumpWaterHeaterIndex);

        // "Zone and outdoor air" configuration is expected break the conservation of mass
        if (HPWH.InletAirConfiguration != AmbientTempEnum::ZoneAndOA) {

            // Air outlet node
            if (NodeNumber == HPWH.HeatPumpAirOutletNode) {
                HeatPumpWaterHeaterNodeException = true;
                break;
            }

            // Air inlet node
            if (NodeNumber == HPWH.HeatPumpAirInletNode) {
                HeatPumpWaterHeaterNodeException = true;
                break;
            }

            // Get fan inlet node index
            bool ErrorsFound{false};
            int FanInletNodeIndex(0);
            if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                FanInletNodeIndex = state.dataHVACFan->fanObjs[HPWH.FanNum]->inletNodeNum;
            } else {
                FanInletNodeIndex = Fans::GetFanInletNode(state, HPWH.FanType, HPWH.FanName, ErrorsFound);
                if (ErrorsFound) {
                    ShowWarningError(state, "Could not retrieve fan outlet node for this unit=\"" + HPWH.Name + "\".");
                    ErrorsFound = true;
                }
            }

            // Fan inlet node
            if (NodeNumber == FanInletNodeIndex) {
                HeatPumpWaterHeaterNodeException = true;
                break;
            }

            // Fan outlet node
            if (NodeNumber == HPWH.FanOutletNode) {
                HeatPumpWaterHeaterNodeException = true;
                break;
            }

            // Outside air node
            if (NodeNumber == HPWH.OutsideAirNode) {
                HeatPumpWaterHeaterNodeException = true;
                break;
            }

            // Exhaust air node
            if (NodeNumber == HPWH.ExhaustAirNode) {
                HeatPumpWaterHeaterNodeException = true;
                break;
            }
        }
    }

    return HeatPumpWaterHeaterNodeException;
}

} // namespace EnergyPlus::WaterThermalTanks
