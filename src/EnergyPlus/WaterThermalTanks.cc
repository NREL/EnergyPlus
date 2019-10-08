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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/floops.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
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
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarCollectors.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

namespace EnergyPlus {

namespace WaterThermalTanks {

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

    std::string const cMixedWHModuleObj("WaterHeater:Mixed");
    std::string const cStratifiedWHModuleObj("WaterHeater:Stratified");
    std::string const cMixedCWTankModuleObj("ThermalStorage:ChilledWater:Mixed");
    std::string const cStratifiedCWTankModuleObj("ThermalStorage:ChilledWater:Stratified");
    std::string const cHPWHPumpedCondenser("WaterHeater:HeatPump:PumpedCondenser");
    std::string const cHPWHWrappedCondenser("WaterHeater:HeatPump:WrappedCondenser");
    std::string const modFluidNameWater("WATER");
    std::string const modBlankString;

    int const modHeatMode(1);  // heating source is on, source will not turn off until setpoint temp is reached
    int const modFloatMode(0); // heating source is off, source will not turn on until cut-in temp is reached
    int const modVentMode(-1); // tank temp is above maximum temperature and water is venting
    int const modCoolMode(2);  // cooling source is on, source will not turn off until setpoint temp is reached

    int const modAmbientTempSchedule(1);   // ambient temperature around tank (or HPWH inlet air) is scheduled
    int const modAmbientTempZone(2);       // tank is located in a zone or HPWH inlet air is zone air only
    int const modAmbientTempOutsideAir(3); // tank is located outdoors or HPWH inlet air is outdoor air only
    int const modAmbientTempZoneAndOA(4);  // applicable to HPWH only, inlet air is mixture of OA and zone air

    int const modCrankcaseTempSchedule(1); // temperature controlling compressor crankcase heater is scheduled
    int const modCrankcaseTempZone(2);     // temperature controlling compressor crankcase heater is zone air
    int const modCrankcaseTempExterior(3); // temperature controlling compressor crankcase heater is outdoor air

    int const modControlTypeCycle(1);    // water heater only, cycling heating source control
    int const modControlTypeModulate(2); // water heater only, modulating heating source control

    int const modTankShapeVertCylinder(1);  // tank shape is a vertical cylinder
    int const modTankShapeHorizCylinder(2); // tank shape is a horizontal cylinder
    int const modTankShapeOther(3);         // tank shape has an arbitrary perimeter shape

    int const modPriorityMasterSlave(1);  // water heater only, master-slave priority control of heater elements
    int const modPrioritySimultaneous(2); // water heater only, simultaneous control of heater elements

    int const modInletModeFixed(1);   // water heater only, inlet water always enters at the user-specified height
    int const modInletModeSeeking(2); // water heater only, inlet water seeks out the node with the closest temperature

    // reclaim heat object types for Coil:WaterHeating:Desuperheater object
    int const modCOMPRESSORRACK_REFRIGERATEDCASE(1); // reclaim heating source is refrigerated case compressor rack
    int const modCOIL_DX_COOLING(2);                 // reclaim heating source is DX cooling coil
    int const modCOIL_DX_MULTISPEED(3);              // reclaim heating source is DX multispeed coil
    int const modCOIL_DX_MULTIMODE(4);               // reclaim heating source is DX multimode coil
    int const modCONDENSER_REFRIGERATION(5);         // reclaim heating source is detailed refrigeration system condenser
    int const modCOIL_DX_VARIABLE_COOLING(6);        // reclaim heating source is Variable Speed DX cooling coil
    int const modCOIL_AIR_WATER_HEATPUMP_EQ(7);      // reclaim heating source is Water to air heat pump cooling coil

    int const modUseSide(101);    // Indicates Use side of water heater
    int const modSourceSide(102); // Indicates Source side of water heater

    int const modSizeNotSet(200);
    int const modSizePeakDraw(201);
    int const modSizeResidentialMin(202);
    int const modSizePerPerson(203);
    int const modSizePerFloorArea(204);
    int const modSizePerUnit(205);
    int const modSizePerSolarColArea(206);

    int const modSourceSideStorageTank(600);
    int const modSourceSideIndirectHeatPrimarySetpoint(601);
    int const modSourceSideIndirectHeatAltSetpoint(602);

    int modWaterIndex(1);

    // MODULE VARIABLE DECLARATIONS:
    int NumChilledWaterMixed(0);        // number of mixed chilled water tanks
    int NumChilledWaterStratified(0);   // number of stratified chilled water tanks
    int NumWaterHeaterMixed(0);         // number of mixed water heaters
    int NumWaterHeaterStratified(0);    // number of stratified water heaters
    int NumWaterThermalTank(0);         // total number of water thermal tanks, hot and cold (MIXED + STRATIFIED)
    int NumWaterHeaterDesuperheater(0); // number of desuperheater heating coils
    int NumHeatPumpWaterHeater(0);      // number of heat pump water heaters
    int NumWaterHeaterSizing(0);        // Number of sizing/design objects for water heaters.

    Real64 modHPPartLoadRatio(0.0);             // part load ratio of HPWH
    Real64 modMixerInletAirSchedule(0.0);       // output of inlet air mixer node schedule
    Real64 modMdotAir(0.0);                     // mass flow rate of evaporator air, kg/s

    // Object Data
    Array1D<WaterThermalTankData> WaterThermalTank;
    Array1D<HeatPumpWaterHeaterData> HPWaterHeater;
    Array1D<WaterHeaterDesuperheaterData> WaterHeaterDesuperheater;
    std::unordered_map<std::string, std::string> UniqueWaterThermalTankNames;

    static ObjexxFCL::gio::Fmt fmtLD("*");

    bool modInitWaterThermalTanksOnce(true); // flag for 1 time initialization
    bool modGetWaterThermalTankInputFlag(true); // Calls to Water Heater from multiple places in code
    bool modSimWaterThermalTank_OneTimeSetupFlag(true);
    bool modCalcWaterThermalTankZoneGains_MyEnvrnFlag(true);

    void clear_state()
    {
        NumChilledWaterMixed = 0;
        NumChilledWaterStratified = 0;
        NumWaterHeaterMixed = 0;
        NumWaterHeaterStratified = 0;
        NumWaterThermalTank = 0;
        NumWaterHeaterDesuperheater = 0;
        NumHeatPumpWaterHeater = 0;
        NumWaterHeaterSizing = 0;

        modHPPartLoadRatio = 0.0;
        modMixerInletAirSchedule = 0.0;
        modMdotAir = 0.0;

        WaterThermalTank.deallocate();
        HPWaterHeater.deallocate();
        WaterHeaterDesuperheater.deallocate();
        UniqueWaterThermalTankNames.clear();

        modInitWaterThermalTanksOnce = true;
        modGetWaterThermalTankInputFlag = true;
        modSimWaterThermalTank_OneTimeSetupFlag = true;
        modCalcWaterThermalTankZoneGains_MyEnvrnFlag = true;
    }

    void SimWaterThermalTank(int const CompType,
                             std::string const &CompName,
                             int &CompIndex,
                             bool const EP_UNUSED(RunFlag), // unused1208
                             bool const InitLoopEquip,
                             Real64 &MyLoad,
                             Real64 &MaxCap,
                             Real64 &MinCap,
                             Real64 &OptCap,
                             bool const FirstHVACIteration, // TRUE if First iteration of simulation
                             Optional_int_const LoopNum,
                             Optional_int_const LoopSideNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brandon Anderson
        //       DATE WRITTEN   May 2000
        //       MODIFIED       FSEC, July 2005
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The main subroutine for simulating a water heater, heat pump water heater, or desuperheater
        // heating coil.  This routine will:
        // 1. Gets Input if necessary
        // 2. Determines the load the water heater (or heat pump water heater) must support
        // 3. Determine the type of water heater, heat pump water heater, or desuperheater
        //    heating coil to be simulated
        // 4. Calls simulation routines

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // FLOW:
        if (modGetWaterThermalTankInputFlag) {
            GetWaterThermalTankInput();
            modGetWaterThermalTankInputFlag = false;
        }

        if (modSimWaterThermalTank_OneTimeSetupFlag) {
            modSimWaterThermalTank_OneTimeSetupFlag = false;
        }

        int CompNum;
        // Find the correct Equipment
        if (CompType != DataPlant::TypeOf_HeatPumpWtrHeaterPumped && CompType != DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
            if (CompIndex == 0) {
                CompNum = UtilityRoutines::FindItem(CompName, WaterThermalTank);
                if (CompNum == 0) {
                    ShowFatalError("SimWaterThermalTank:  Unit not found=" + CompName);
                }
                CompIndex = CompNum;
            } else {
                CompNum = CompIndex;
                if (CompNum > NumWaterThermalTank || CompNum < 1) {
                    ShowFatalError("SimWaterThermalTank:  Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                                   ", Number of Units=" + General::TrimSigDigits(NumWaterThermalTank) + ", Entered Unit name=" + CompName);
                }
                if (WaterThermalTank(CompNum).CheckWTTEquipName) {
                    if (CompName != WaterThermalTank(CompNum).Name) {
                        ShowFatalError("SimWaterThermalTank: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) + ", Unit name=" + CompName +
                                       ", stored Unit Name for that index=" + WaterThermalTank(CompNum).Name);
                    }
                    WaterThermalTank(CompNum).CheckWTTEquipName = false;
                }
            }
        } else {
            if (CompIndex == 0) {
                CompNum = UtilityRoutines::FindItem(CompName, HPWaterHeater);
                if (CompNum == 0) {
                    ShowFatalError("SimWaterThermalTank:  Unit not found=" + CompName);
                }
                CompIndex = CompNum;
            } else {
                CompNum = CompIndex;
                if (CompNum > NumWaterThermalTank || CompNum < 1) {
                    ShowFatalError("SimWaterThermalTank:  Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                                   ", Number of Units=" + General::TrimSigDigits(NumHeatPumpWaterHeater) + ", Entered Unit name=" + CompName);
                }
                if (HPWaterHeater(CompNum).CheckHPWHEquipName) {
                    if (CompName != HPWaterHeater(CompNum).Name) {
                        ShowFatalError("SimWaterThermalTank: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) + ", Unit name=" + CompName +
                                       ", stored Unit Name for that index=" + HPWaterHeater(CompNum).Name);
                    }
                    HPWaterHeater(CompNum).CheckHPWHEquipName = false;
                }
            }
        }

        // this case statement needs integerization.
        {
            auto const SELECT_CASE_var(CompType);

            // string comparisons to remove here.
            // =========================  Water Heater and Chilled Water Storage
            if ((SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) || (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) ||
                (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankMixed) || (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankStratified)) {

                if (InitLoopEquip) {
                    if (present(LoopNum)) {
                        InitWaterThermalTank(CompNum, FirstHVACIteration, LoopNum, LoopSideNum);
                    } else {
                        InitWaterThermalTank(CompNum, FirstHVACIteration);
                    }
                    WaterThermalTank(CompNum).MinePlantStructForInfo();
                    if (present(LoopNum)) {
                        if (((WaterThermalTank(CompNum).SourceSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(CompNum).SourceSidePlantLoopSide == LoopSideNum)) ||
                            ((WaterThermalTank(CompNum).UseSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(CompNum).UseSidePlantLoopSide == LoopSideNum))) {

                            WaterThermalTank(CompNum).SizeTankForDemandSide();
                            WaterThermalTank(CompNum).SizeDemandSidePlantConnections();
                            WaterThermalTank(CompNum).SizeSupplySidePlantConnections(LoopNum, LoopSideNum);
                            WaterThermalTank(CompNum).SizeTankForSupplySide();
                        } else {
                            return;
                        }
                    } else {
                        WaterThermalTank(CompNum).SizeTankForDemandSide();
                        WaterThermalTank(CompNum).SizeDemandSidePlantConnections();
                        WaterThermalTank(CompNum).SizeSupplySidePlantConnections();
                        WaterThermalTank(CompNum).SizeTankForSupplySide();
                    }

                    // Calculate and report water heater standard ratings to EIO file (now that sizing is done)
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (!WaterThermalTank(CompNum).IsChilledWaterTank) {
                            WaterThermalTank(CompNum).CalcStandardRatings(CompNum);
                        } else {
                            WaterThermalTank(CompNum).ReportCWTankInits();
                        }
                    }
                    MinCap = 0.0;
                    MaxCap = WaterThermalTank(CompNum).MaxCapacity;
                    OptCap = WaterThermalTank(CompNum).MaxCapacity;
                    if (present(LoopNum)) {
                        InitWaterThermalTank(CompNum, FirstHVACIteration, LoopNum, LoopSideNum);
                    } else {
                        InitWaterThermalTank(CompNum, FirstHVACIteration);
                    }
                    return;
                }

                if (WaterThermalTank(CompNum).MyOneTimeFlagWH) {
                    WaterThermalTank(CompNum).MyOneTimeFlagWH = false;
                } else {
                    if (WaterThermalTank(CompNum).MyTwoTimeFlagWH) {
                        WaterThermalTank(CompNum).MinePlantStructForInfo(); // call it again to get control types filled out
                        WaterThermalTank(CompNum).MyTwoTimeFlagWH = false;
                    }
                }
                WaterThermalTank(CompNum).UseSideLoadRequested = std::abs(MyLoad);
                int tmpLoopNum = WaterThermalTank(CompNum).UseSidePlantLoopNum;
                if (tmpLoopNum > 0 && WaterThermalTank(CompNum).UseSidePlantLoopSide > 0 && !DataGlobals::KickOffSimulation) {
                    WaterThermalTank(CompNum).UseCurrentFlowLock = DataPlant::PlantLoop(tmpLoopNum).LoopSide(LoopSideNum).FlowLock;
                } else {
                    WaterThermalTank(CompNum).UseCurrentFlowLock = 1;
                }
                tmpLoopNum = WaterThermalTank(CompNum).SourceSidePlantLoopNum;
                if (tmpLoopNum > 0 && WaterThermalTank(CompNum).SourceSidePlantLoopSide > 0 && !DataGlobals::KickOffSimulation) {
                    WaterThermalTank(CompNum).SourceCurrentFlowLock = DataPlant::PlantLoop(tmpLoopNum).LoopSide(LoopSideNum).FlowLock;
                } else {
                    WaterThermalTank(CompNum).SourceCurrentFlowLock = 1;
                }
                InitWaterThermalTank(CompNum, FirstHVACIteration);
                //       Plant connected water heaters may have a desuperheater heating coil attached
                if (WaterThermalTank(CompNum).DesuperheaterNum == 0) {
                    if ((WaterThermalTank(CompNum).TypeNum == DataPlant::TypeOf_WtrHeaterMixed) || (WaterThermalTank(CompNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankMixed)) {
                        WaterThermalTank(CompNum).CalcWaterThermalTankMixed(CompNum);
                    } else if ((WaterThermalTank(CompNum).TypeNum == DataPlant::TypeOf_WtrHeaterStratified) ||
                               (WaterThermalTank(CompNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified)) {
                        WaterThermalTank(CompNum).CalcWaterThermalTankStratified(CompNum);
                    }
                } else if (WaterThermalTank(CompNum).DesuperheaterNum > 0) {
                    CalcDesuperheaterWaterHeater(CompNum, FirstHVACIteration);
                }
                WaterThermalTank(CompNum).UpdateWaterThermalTank();
                WaterThermalTank(CompNum).ReportWaterThermalTank();

                // =========================  Heat Pump Water Heater
            } else if (SELECT_CASE_var == DataPlant::TypeOf_HeatPumpWtrHeaterPumped || SELECT_CASE_var == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                if (InitLoopEquip) {
                    // CompNum is index to heatpump model, not tank so get the tank index
                    int TankNum = HPWaterHeater(CompNum).WaterHeaterTankNum;
                    if (present(LoopNum)) {
                        InitWaterThermalTank(TankNum, FirstHVACIteration, LoopNum, LoopSideNum);
                    } else {
                        InitWaterThermalTank(TankNum, FirstHVACIteration);
                    }
                    WaterThermalTank(TankNum).MinePlantStructForInfo();
                    if (present(LoopNum)) {
                        if (((WaterThermalTank(TankNum).SourceSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(TankNum).SourceSidePlantLoopSide == LoopSideNum)) ||
                            ((WaterThermalTank(TankNum).UseSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(TankNum).UseSidePlantLoopSide == LoopSideNum))) {
                            WaterThermalTank(CompNum).SizeTankForDemandSide();
                            WaterThermalTank(CompNum).SizeDemandSidePlantConnections();
                            WaterThermalTank(TankNum).SizeSupplySidePlantConnections(LoopNum, LoopSideNum);
                            WaterThermalTank(TankNum).SizeTankForSupplySide();
                        } else {
                            return;
                        }
                    } else {
                        WaterThermalTank(CompNum).SizeTankForDemandSide();
                        WaterThermalTank(CompNum).SizeDemandSidePlantConnections();
                        WaterThermalTank(TankNum).SizeSupplySidePlantConnections();
                        WaterThermalTank(TankNum).SizeTankForSupplySide();
                    }

                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        WaterThermalTank(TankNum).CalcStandardRatings(TankNum);
                        DataSizing::DataNonZoneNonAirloopValue = 0.0;
                    }
                    MinCap = 0.0;
                    MaxCap = HPWaterHeater(CompNum).Capacity;
                    OptCap = HPWaterHeater(CompNum).Capacity;

                    return;
                }

                if (HPWaterHeater(CompNum).MyOneTimeFlagHP) {
                    HPWaterHeater(CompNum).MyOneTimeFlagHP = false;
                } else {
                    if (HPWaterHeater(CompNum).MyTwoTimeFlagHP) {
                        WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).MinePlantStructForInfo(); // call it again to get control types filled out
                        HPWaterHeater(CompNum).MyTwoTimeFlagHP = false;
                    }
                }
                WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseSideLoadRequested = std::abs(MyLoad);
                int tmpLoopNum = WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseSidePlantLoopNum;
                int tmpLoopSideNum = WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseSidePlantLoopSide;
                if (tmpLoopNum > 0 && tmpLoopSideNum > 0 && !DataGlobals::KickOffSimulation) {
                    WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseCurrentFlowLock =
                        DataPlant::PlantLoop(tmpLoopNum).LoopSide(LoopSideNum).FlowLock;
                } else {
                    WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseCurrentFlowLock = 1;
                }
                if (present(LoopNum)) {
                    InitWaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum, FirstHVACIteration, LoopNum, LoopSideNum);
                } else {
                    InitWaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum, FirstHVACIteration);
                }

                int InletNodeSav = HPWaterHeater(CompNum).HeatPumpAirInletNode;
                int OutletNodeSav = HPWaterHeater(CompNum).HeatPumpAirOutletNode;
                int DXINletNodeSav = HPWaterHeater(CompNum).DXCoilAirInletNode;
                int IHPFanIndexSav = HPWaterHeater(CompNum).FanNum;
                std::string IHPFanNameSave = HPWaterHeater(CompNum).FanName;
                int IHPFanplaceSav = HPWaterHeater(CompNum).FanPlacement;

                if (HPWaterHeater(CompNum).bIsIHP) // pass the tank indexes to the IHP object
                {
                    IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).WHtankType = CompType;
                    IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).WHtankName = CompName;
                    IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).WHtankID = CompIndex;
                    if (present(LoopNum)) {
                        IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).LoopNum = LoopNum;
                        IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).LoopSideNum = LoopSideNum;
                    }

                    IntegratedHeatPump::IHPOperationMode IHPMode = IntegratedHeatPump::GetCurWorkMode(HPWaterHeater(CompNum).DXCoilNum);
                    if ((IntegratedHeatPump::IHPOperationMode::DWHMode == IHPMode) || (IntegratedHeatPump::IHPOperationMode::SCDWHMode == IHPMode) ||
                        (IntegratedHeatPump::IHPOperationMode::SHDWHElecHeatOffMode == IHPMode) ||
                        (IntegratedHeatPump::IHPOperationMode::SHDWHElecHeatOnMode == IHPMode)) { // default is to specify the air nodes for SCWH mode
                        bool bDWHCoilReading = false;
                        HPWaterHeater(CompNum).HeatPumpAirInletNode =
                            VariableSpeedCoils::GetCoilInletNodeVariableSpeed("COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
                                                          IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).DWHCoilName,
                                                          bDWHCoilReading);
                        HPWaterHeater(CompNum).HeatPumpAirOutletNode =
                            VariableSpeedCoils::GetCoilOutletNodeVariableSpeed("COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
                                                           IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).DWHCoilName,
                                                           bDWHCoilReading);
                        HPWaterHeater(CompNum).DXCoilAirInletNode = HPWaterHeater(CompNum).HeatPumpAirInletNode;
                    } else // default is to input outdoor fan to the the HPWH
                    {
                        HPWaterHeater(CompNum).FanNum = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).IDFanID;
                        HPWaterHeater(CompNum).FanName = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).IDFanName;
                        HPWaterHeater(CompNum).FanPlacement = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).IDFanPlace;
                    }
                }

                CalcHeatPumpWaterHeater(HPWaterHeater(CompNum).WaterHeaterTankNum, FirstHVACIteration);
                WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UpdateWaterThermalTank();
                WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).ReportWaterThermalTank();

                HPWaterHeater(CompNum).HeatPumpAirInletNode = InletNodeSav;
                HPWaterHeater(CompNum).HeatPumpAirOutletNode = OutletNodeSav;
                HPWaterHeater(CompNum).DXCoilAirInletNode = DXINletNodeSav;
                HPWaterHeater(CompNum).FanNum = IHPFanIndexSav;
                HPWaterHeater(CompNum).FanName = IHPFanNameSave;
                HPWaterHeater(CompNum).FanPlacement = IHPFanplaceSav;

            } else {
                ShowSevereError("SimWaterThermalTank: Invalid Water Thermal Tank Equipment Type=" + General::TrimSigDigits(CompType));
                ShowContinueError("Occurs in Water Thermal Tank Equipment named = " + CompName);
                ShowFatalError("Preceding condition causes termination.");
            }
        }
    }

    void SimulateWaterHeaterStandAlone(int const WaterHeaterNum, bool const FirstHVACIteration)
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
        Real64 MinCap;
        Real64 MaxCap;
        Real64 OptCap;

        // FLOW:
        if (modGetWaterThermalTankInputFlag) {
            GetWaterThermalTankInput();
            modGetWaterThermalTankInputFlag = false;
        }

        // Only simulate stand-alone water heaters here.  Plant connected water heaters are called by the PlantLoopEquipments.
        if (WaterThermalTank(WaterHeaterNum).StandAlone) {
            bool LocalRunFlag = true;
            bool LocalInitLoopEquip = false;
            int TestNum = WaterHeaterNum;
            SimWaterThermalTank(WaterThermalTank(WaterHeaterNum).TypeNum,
                                WaterThermalTank(WaterHeaterNum).Name,
                                TestNum,
                                LocalRunFlag,
                                LocalInitLoopEquip,
                                MyLoad,
                                MinCap,
                                MaxCap,
                                OptCap,
                                FirstHVACIteration);
            if (TestNum != WaterHeaterNum) {
                ShowFatalError("SimulateWaterHeaterStandAlone: Input WaterHeater Num [" + General::TrimSigDigits(WaterHeaterNum) +
                               "] does not match returned WaterHeater Num[" + General::TrimSigDigits(TestNum) + "] Name=\"" +
                               WaterThermalTank(WaterHeaterNum).Name + "\".");
            }

            // HPWHs with inlet air from a zone and not connected to a plant loop are simulated through a CALL from ZoneEquipmentManager.
            // HPWHs that are plant connected are always simulated through a CALL from PlantLoopEquipments directly to SimWaterThermalTank.

            // NOTE: HPWHs with inlet air from a zone AND plant connected are not stand alone and are simulated in PlantLoopEquipments
        } else if (WaterThermalTank(WaterHeaterNum).HeatPumpNum > 0) {
            //   Only HPWHs with inlet air from outdoors or scheduled HPWHs (not connected to a plant loop) are simulated here.
            if (HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).StandAlone &&
                (HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).InletAirConfiguration == modAmbientTempOutsideAir ||
                 HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).InletAirConfiguration == modAmbientTempSchedule)) {
                bool LocalRunFlag = true;
                bool LocalInitLoopEquip = false;
                SimWaterThermalTank(HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).TypeNum,
                                    HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).Name,
                                    WaterThermalTank(WaterHeaterNum).HeatPumpNum,
                                    LocalRunFlag,
                                    LocalInitLoopEquip,
                                    MyLoad,
                                    MinCap,
                                    MaxCap,
                                    OptCap,
                                    FirstHVACIteration);
            }

            // Only simulate stand-alone water heaters with desuperheater water heating coils here.  Plant connected water heaters
            // with desuperheater water heating coils are called by PlantLoopEquipments.
        } else if (WaterThermalTank(WaterHeaterNum).DesuperheaterNum > 0) {
            if (WaterHeaterDesuperheater(WaterThermalTank(WaterHeaterNum).DesuperheaterNum).StandAlone) {
                bool LocalRunFlag = true;
                bool LocalInitLoopEquip = false;
                int TestNum = WaterHeaterNum;
                SimWaterThermalTank(WaterThermalTank(WaterHeaterNum).TypeNum,
                                    WaterThermalTank(WaterHeaterNum).Name,
                                    TestNum,
                                    LocalRunFlag,
                                    LocalInitLoopEquip,
                                    MyLoad,
                                    MinCap,
                                    MaxCap,
                                    OptCap,
                                    FirstHVACIteration);
                if (TestNum != WaterHeaterNum) {
                    ShowFatalError("SimulateWaterHeaterStandAlone: Input WaterHeater Num [" + General::TrimSigDigits(WaterHeaterNum) +
                                   "] does not match returned WaterHeater Num[" + General::TrimSigDigits(TestNum) + "] Name=\"" +
                                   WaterThermalTank(WaterHeaterNum).Name + "\".");
                }
            }
        }
    }

    void SimHeatPumpWaterHeater(std::string const &CompName,
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

        // FLOW:
        if (modGetWaterThermalTankInputFlag) {
            GetWaterThermalTankInput();
            modGetWaterThermalTankInputFlag = false;
        }

        // Find the correct Heat Pump Water Heater
        int HeatPumpNum;
        if (CompIndex == 0) {
            HeatPumpNum = UtilityRoutines::FindItemInList(CompName, HPWaterHeater);
            if (HeatPumpNum == 0) {
                ShowFatalError("SimHeatPumpWaterHeater: Unit not found=" + CompName);
            }
            CompIndex = HeatPumpNum;
        } else {
            HeatPumpNum = CompIndex;
            if (HeatPumpNum > NumHeatPumpWaterHeater || HeatPumpNum < 1) {
                ShowFatalError("SimHeatPumpWaterHeater:  Invalid CompIndex passed=" + General::TrimSigDigits(HeatPumpNum) +
                               ", Number of Units=" + General::TrimSigDigits(NumHeatPumpWaterHeater) + ", Entered Unit name=" + CompName);
            }
        }

        // Only simulate HPWHs specified as zone equipment and not connected to a plant loop.
        // HPWHs not defined as zone equipment with no plant connections are simulated in NonZoneEquipmentManager.
        // Plant connected HPWHs are called by PlantLoopEquipments (but only those on supply side ).
        // HPWH will not be included in sizing calculations, fan is initialized only during BeginEnvrnFlag (FALSE during sizing)
        // (fan will be turned off during Standard Ratings procedure yielding incorrect results)
        if (DataGlobals::DoingSizing) return;

        // For HPWHs, StandAlone means not connected to a plant loop (use nodes are not used, source nodes are connected to a HPWH)
        if (HPWaterHeater(HeatPumpNum).StandAlone) {
            bool LocalRunFlag = true;
            bool LocalInitLoopEquip = false;
            Real64 MyLoad;
            Real64 MinCap;
            Real64 MaxCap;
            Real64 OptCap;
            SimWaterThermalTank(HPWaterHeater(HeatPumpNum).TypeNum,
                                HPWaterHeater(HeatPumpNum).Name,
                                HeatPumpNum,
                                LocalRunFlag,
                                LocalInitLoopEquip,
                                MyLoad,
                                MinCap,
                                MaxCap,
                                OptCap,
                                FirstHVACIteration);
            SensLoadMet = HPWaterHeater(HeatPumpNum).HPWaterHeaterSensibleCapacity;
            LatLoadMet = HPWaterHeater(HeatPumpNum).HPWaterHeaterLatentCapacity;
        } else {
            // HPWH is plant connected and will get simulated when called from plant SimWaterThermalTank, but need to update loads met here
            SensLoadMet = HPWaterHeater(HeatPumpNum).HPWaterHeaterSensibleCapacity;
            LatLoadMet = HPWaterHeater(HeatPumpNum).HPWaterHeaterLatentCapacity;
        }
    }

    void CalcWaterThermalTankZoneGains()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   March 2005
        //       MODIFIED       B. Griffith November 2011, new internal gains structure
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the zone internal gains due to water heater skin losses during sizing.
        // initilizes gains to zone at begin environment.

        // METHODOLOGY EMPLOYED:
        // Sums the tank losses from all of the water heaters in the zone to add as a gain to the zone.
        // Now used to determine tank losses during sizing.  Internal gains are summed in a centralized way now

        // FLOW:
        if (NumWaterThermalTank == 0) {

            if (!DataGlobals::DoingSizing) {
                return;
            } else {
                if (modGetWaterThermalTankInputFlag) {
                    GetWaterThermalTankInput();
                    modGetWaterThermalTankInputFlag = false;
                }
                if (NumWaterThermalTank == 0) return;
            }
        }

        if (DataGlobals::BeginEnvrnFlag && modCalcWaterThermalTankZoneGains_MyEnvrnFlag) {
            for (auto &e : WaterThermalTank) {
                e.AmbientZoneGain = 0.0;
                e.FuelEnergy = 0.0;
                e.OffCycParaFuelEnergy = 0.0;
                e.OnCycParaFuelEnergy = 0.0;
            }
            modCalcWaterThermalTankZoneGains_MyEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) modCalcWaterThermalTankZoneGains_MyEnvrnFlag = true;

        for (int WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {
            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) continue;
            if (DataGlobals::DoingSizing) {
                // Initialize tank temperature to setpoint
                // (use HPWH or Desuperheater heating coil set point if applicable)
                int SchIndex;
                if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                    SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule;
                } else if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                    SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule;
                } else {
                    SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
                }

                Real64 TankTemp;
                Real64 QLossToZone = 0.0;
                if (SchIndex > 0) {
                    TankTemp = ScheduleManager::GetCurrentScheduleValue(SchIndex);
                } else {
                    TankTemp = 20.0;
                }
                {
                    auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).TypeNum);
                    if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                        QLossToZone =
                            max(WaterThermalTank(WaterThermalTankNum).OnCycLossCoeff * WaterThermalTank(WaterThermalTankNum).OnCycLossFracToZone,
                                WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff * WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone) *
                            (TankTemp - DataHeatBalFanSys::MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                        QLossToZone = max(WaterThermalTank(WaterThermalTankNum).Node(1).OnCycLossCoeff *
                                              WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone,
                                          WaterThermalTank(WaterThermalTankNum).Node(1).OffCycLossCoeff *
                                              WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone) *
                                      (TankTemp - DataHeatBalFanSys::MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankMixed) {
                        QLossToZone = WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff *
                                      WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone *
                                      (TankTemp - DataHeatBalFanSys::MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankStratified) {
                        QLossToZone = WaterThermalTank(WaterThermalTankNum).Node(1).OffCycLossCoeff *
                                      WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone *
                                      (TankTemp - DataHeatBalFanSys::MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    }
                }
                WaterThermalTank(WaterThermalTankNum).AmbientZoneGain = QLossToZone;
            }
        }
    }

    bool GetWaterThermalTankInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher and Brandon Anderson
        //       DATE WRITTEN   May 2000
        //       MODIFIED       R. Raustad, June 2005, added HPWH and desuperheater water heating coils
        //                      B. Griffith, Oct. 2007 extensions for indirect water heaters
        //                      B. Griffith, Feb. 2008 extensions for autosizing water heaters
        //                      BG Mar 2009.  Trap for bad heater height input for stratefied water heater CR7718
        //						B. Shen 12/2014, add air-source variable-speed heat pump water heating

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the water heater, HPWH, and/or desuperheater heating coil input from the input file.

        bool ErrorsFound = false;

        static std::string const RoutineName("GetWaterThermalTankInput: ");
        static std::string const RoutineNameNoColon("GetWaterThermalTankInput");

        struct WaterHeaterSaveNodes
        {
            // Members
            std::string InletNodeName1;
            std::string OutletNodeName1;
            std::string InletNodeName2;
            std::string OutletNodeName2;

            // Default Constructor
            WaterHeaterSaveNodes() = default;
        };

        // Object Data
        Array1D<WaterHeaterSaveNodes> HPWHSaveNodeNames; // temporary for HPWH node names used in later checks
        Array1D<WaterHeaterSaveNodes> WHSaveNodeNames;   // temporary for WH node names used in later checks
        Array1D<WaterHeaterSaveNodes> CoilSaveNodeNames; // temporary for coil node names used in later checks

        // Formats
        static ObjexxFCL::gio::Fmt Format_720("('! <Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},Standard Rated Recovery Efficiency, "
                                   "','Standard Rated Energy Factor')");
        static ObjexxFCL::gio::Fmt Format_721("('! <Heat Pump Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},','Standard Rated Recovery "
                                   "Efficiency,Standard Rated Energy Factor,\"DX Coil Total Cooling Rate {W, HPWH Only}\"')");
        static ObjexxFCL::gio::Fmt Format_722("('! <Water Heater Stratified Node Information>,Node Number,Height {m},Volume {m3},Maximum Capacity "
                                   "{W},','Off-Cycle UA {W/K},On-Cycle UA {W/K},Number Of Inlets,Number Of Outlets')");
        static ObjexxFCL::gio::Fmt Format_725(
            "('! <Chilled Water Tank Information>,Type,Name,Volume {m3},Use Side Design Flow Rate {m3/s}, ','Source Side Design Flow Rate {m3/s}')");
        static ObjexxFCL::gio::Fmt Format_726("('! <Chilled Water Tank Stratified Node Information>,Node Number,Height {m},Volume {m3},','UA {W/K},Number Of "
                                   "Inlets,Number Of Outlets')");
        static ObjexxFCL::gio::Fmt Format_723("('Water Heater Stratified Node Information',8(',',A))");
        static ObjexxFCL::gio::Fmt Format_724("('Chilled Water Tank Stratified Node Information',6(',',A))");

        // FLOW:

        // Make sure refrigeration input is gotten before this input
        RefrigeratedCase::CheckRefrigerationInput();

        if (modGetWaterThermalTankInputFlag) {
            NumWaterHeaterMixed = inputProcessor->getNumObjectsFound(cMixedWHModuleObj);
            NumWaterHeaterStratified = inputProcessor->getNumObjectsFound(cStratifiedWHModuleObj);
            NumChilledWaterMixed = inputProcessor->getNumObjectsFound(cMixedCWTankModuleObj);
            NumChilledWaterStratified = inputProcessor->getNumObjectsFound(cStratifiedCWTankModuleObj);
            NumWaterThermalTank = NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + NumChilledWaterStratified;
            NumHeatPumpWaterHeater =
                inputProcessor->getNumObjectsFound(cHPWHPumpedCondenser) + inputProcessor->getNumObjectsFound(cHPWHWrappedCondenser);
            NumWaterHeaterDesuperheater = inputProcessor->getNumObjectsFound("Coil:WaterHeating:Desuperheater");

            if (NumWaterThermalTank > 0) {
                // Write water heater header for EIO
                if ((NumWaterHeaterMixed > 0) || (NumWaterHeaterStratified > 0)) ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_720);
                if (NumHeatPumpWaterHeater > 0) ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_721);
                if (NumWaterHeaterStratified > 0) ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_722);
                if (NumChilledWaterMixed > 0) ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_725);
                if (NumChilledWaterStratified > 0) ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_726);
            }

            if (NumWaterThermalTank > 0) {
                WaterThermalTank.allocate(NumWaterThermalTank);
                UniqueWaterThermalTankNames.reserve(static_cast<unsigned>(NumWaterThermalTank));
                WHSaveNodeNames.allocate(NumWaterThermalTank);
            }
            if (NumHeatPumpWaterHeater > 0) {
                HPWaterHeater.allocate(NumHeatPumpWaterHeater);
                HPWHSaveNodeNames.allocate(NumHeatPumpWaterHeater);

                for (int IHPIndex = 1; IHPIndex <= NumHeatPumpWaterHeater; ++IHPIndex) HPWaterHeater(IHPIndex).bIsIHP = false;
            }

            if (NumWaterHeaterDesuperheater > 0) {
                WaterHeaterDesuperheater.allocate(NumWaterHeaterDesuperheater);
                CoilSaveNodeNames.allocate(NumWaterHeaterDesuperheater);
            }

            // =======   Get Coil:WaterHeating:Desuperheater ======================================================================
            if (NumWaterHeaterDesuperheater > 0) {
                DataIPShortCuts::cCurrentModuleObject = "Coil:WaterHeating:Desuperheater";
                for (int DesuperheaterNum = 1; DesuperheaterNum <= NumWaterHeaterDesuperheater; ++DesuperheaterNum) {
                    int NumAlphas;
                    int NumNums;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  DesuperheaterNum,
                                                  DataIPShortCuts::cAlphaArgs,
                                                  NumAlphas,
                                                  DataIPShortCuts::rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  DataIPShortCuts::lNumericFieldBlanks,
                                                  DataIPShortCuts::lAlphaFieldBlanks,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);
                    UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                    // ErrorsFound will be set to True if problem was found, left untouched otherwise
                    GlobalNames::VerifyUniqueCoilName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

                    WaterHeaterDesuperheater(DesuperheaterNum).Name = DataIPShortCuts::cAlphaArgs(1);
                    WaterHeaterDesuperheater(DesuperheaterNum).Type = DataIPShortCuts::cCurrentModuleObject;

                    //       convert availability schedule name to pointer
                    if (!DataIPShortCuts::lAlphaFieldBlanks(2)) {
                        WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
                        if (WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr == 0) {
                            ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    } else {
                        WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr = DataGlobals::ScheduleAlwaysOn;
                    }

                    //       convert schedule name to pointer
                    WaterHeaterDesuperheater(DesuperheaterNum).SetPointTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(3));
                    if (WaterHeaterDesuperheater(DesuperheaterNum).SetPointTempSchedule == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff = DataIPShortCuts::rNumericArgs(1);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff <= 0.0 ||
                        WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff > 20.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        DataIPShortCuts::cNumericFieldNames(1) + " must be > 0 and <= 20. " + DataIPShortCuts::cNumericFieldNames(1) + " = " +
                                        General::TrimSigDigits(DataIPShortCuts::rNumericArgs(1), 1));
                        ErrorsFound = true;
                    }

                    // WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = DataIPShortCuts::rNumericArgs(2)
                    // Error limits on heat reclaim efficiency applied after source type identified

                    WaterHeaterDesuperheater(DesuperheaterNum).RatedInletWaterTemp = DataIPShortCuts::rNumericArgs(3);
                    WaterHeaterDesuperheater(DesuperheaterNum).RatedOutdoorAirTemp = DataIPShortCuts::rNumericArgs(4);
                    WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp = DataIPShortCuts::rNumericArgs(5);

                    if (!DataIPShortCuts::lAlphaFieldBlanks(4)) {
                        WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(4));
                        if (WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ":  " +
                                            DataIPShortCuts::cAlphaFieldNames(4) + " not found = " + DataIPShortCuts::cAlphaArgs(4));
                            ErrorsFound = true;
                        } else {
                            ErrorsFound |= CurveManager::CheckCurveDims(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp, // Curve index
                                                                        {2},                                                  // Valid dimensions
                                                                        RoutineName,                                          // Routine name
                                                                        DataIPShortCuts::cCurrentModuleObject,                                 // Object Type
                                                                        WaterHeaterDesuperheater(DesuperheaterNum).Name,      // Object Name
                                                                        DataIPShortCuts::cAlphaFieldNames(4));                                 // Field Name
                            Real64 HEffFTemp = 0.0;
                            if (!ErrorsFound) {
                                if (WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp > 0) {
                                    HEffFTemp = min(1.0,
                                                    max(0.0,
                                                        CurveManager::CurveValue(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp,
                                                                   WaterHeaterDesuperheater(DesuperheaterNum).RatedInletWaterTemp,
                                                                   WaterHeaterDesuperheater(DesuperheaterNum).RatedOutdoorAirTemp)));
                                    if (std::abs(HEffFTemp - 1.0) > 0.05) {
                                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name + "\":");
                                        ShowContinueError("The " + DataIPShortCuts::cAlphaFieldNames(4) + " should be normalized ");
                                        ShowContinueError(" to 1.0 at the rating point. Curve output at the rating point = " +
                                                          General::TrimSigDigits(HEffFTemp, 3));
                                        ShowContinueError(" The simulation continues using the user-specified curve.");
                                    }
                                }
                            }
                        }
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).WaterInletNode = NodeInputManager::GetOnlySingleNode(
                        DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsParent);

                    WaterHeaterDesuperheater(DesuperheaterNum).WaterOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                                   ErrorsFound,
                                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                                   1,
                                                                                                   DataLoopNode::ObjectIsParent);

                    CoilSaveNodeNames(DesuperheaterNum).InletNodeName1 = DataIPShortCuts::cAlphaArgs(5);
                    CoilSaveNodeNames(DesuperheaterNum).OutletNodeName1 = DataIPShortCuts::cAlphaArgs(6);

                    WaterHeaterDesuperheater(DesuperheaterNum).TankType = DataIPShortCuts::cAlphaArgs(7);

                    if (!UtilityRoutines::SameString(WaterHeaterDesuperheater(DesuperheaterNum).TankType, cMixedWHModuleObj) &&
                        !UtilityRoutines::SameString(WaterHeaterDesuperheater(DesuperheaterNum).TankType, cStratifiedWHModuleObj)) {

                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWaterHeater(DesuperheaterNum).Name + ':');
                        ShowContinueError("Desuperheater can only be used with " + cMixedWHModuleObj + " or " + cStratifiedWHModuleObj + '.');
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).TankName = DataIPShortCuts::cAlphaArgs(8);

                    //       Set up comp set for water side nodes (reverse inlet/outlet for water heater)
                    BranchNodeConnections::SetUpCompSets(WaterHeaterDesuperheater(DesuperheaterNum).Type,
                                  WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                  WaterHeaterDesuperheater(DesuperheaterNum).TankType,
                                  WaterHeaterDesuperheater(DesuperheaterNum).TankName,
                                  DataIPShortCuts::cAlphaArgs(6),
                                  DataIPShortCuts::cAlphaArgs(5));

                    //       Find the Refrigeration equipment index associated with the desuperheater heating coil.
                    bool errFlag = false;
                    WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceType = DataIPShortCuts::cAlphaArgs(9);
                    WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName = DataIPShortCuts::cAlphaArgs(10);
                    if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Refrigeration:CompressorRack")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCOMPRESSORRACK_REFRIGERATEDCASE;
                        for (int RackNum = 1; RackNum <= DataHeatBalance::NumRefrigeratedRacks; ++RackNum) {
                            if (!UtilityRoutines::SameString(DataHeatBalance::HeatReclaimRefrigeratedRack(RackNum).Name, DataIPShortCuts::cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = RackNum;
                            if (allocated(DataHeatBalance::HeatReclaimRefrigeratedRack)) WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                            break;
                        }
                    } else if ((UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Refrigeration:Condenser:AirCooled")) ||
                               (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Refrigeration:Condenser:EvaporativeCooled")) ||
                               (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Refrigeration:Condenser:WaterCooled"))) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCONDENSER_REFRIGERATION;
                        for (int CondNum = 1; CondNum <= DataHeatBalance::NumRefrigCondensers; ++CondNum) {
                            if (!UtilityRoutines::SameString(DataHeatBalance::HeatReclaimRefrigCondenser(CondNum).Name, DataIPShortCuts::cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = CondNum;
                            if (allocated(DataHeatBalance::HeatReclaimRefrigCondenser)) WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                            break;
                        }
                    } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Coil:Cooling:DX:SingleSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCOIL_DX_COOLING;
                        DXCoils::GetDXCoilIndex(WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName, WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum, errFlag, DataIPShortCuts::cCurrentModuleObject);
                        if (allocated(DataHeatBalance::HeatReclaimDXCoil)) WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                    } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Coil:Cooling:DX:TwoSpeed") || UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Coil:Cooling:DX:MultiSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCOIL_DX_MULTISPEED;
                        DXCoils::GetDXCoilIndex(WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName, WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum, errFlag, DataIPShortCuts::cCurrentModuleObject);
                        if (allocated(DataHeatBalance::HeatReclaimDXCoil)) WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                    } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCOIL_DX_MULTIMODE;
                        DXCoils::GetDXCoilIndex(WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName, WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum, errFlag, DataIPShortCuts::cCurrentModuleObject);
                        if (allocated(DataHeatBalance::HeatReclaimDXCoil)) WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                    } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Coil:Cooling:DX:VariableSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCOIL_DX_VARIABLE_COOLING;
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = VariableSpeedCoils::GetCoilIndexVariableSpeed(DataIPShortCuts::cAlphaArgs(9), DataIPShortCuts::cAlphaArgs(10), errFlag);
                        if (allocated(DataHeatBalance::HeatReclaimVS_DXCoil)) WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                    } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Coil:Cooling:WaterToAirHeatPump:EquationFit")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = modCOIL_AIR_WATER_HEATPUMP_EQ;
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = WaterToAirHeatPumpSimple::GetCoilIndex(DataIPShortCuts::cAlphaArgs(9), DataIPShortCuts::cAlphaArgs(10), errFlag);
                        if (allocated(DataHeatBalance::HeatReclaimSimple_WAHPCoil)) {
                            DataHeatBalance::HeatReclaimHPCoilData &HeatReclaim = DataHeatBalance::HeatReclaimSimple_WAHPCoil(WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum);
                            if (!allocated(HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat)){
                            HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat.allocate(NumWaterHeaterDesuperheater);
                            for (auto& num : HeatReclaim.WaterHeatingDesuperheaterReclaimedHeat) num = 0.0;
                            }
                            WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType = true;
                        }
                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
                        ShowContinueError(" desuperheater can only be used with Coil:Cooling:DX:SingleSpeed, ");
                        ShowContinueError(
                            " Coil:Cooling:DX:TwoSpeed, Coil:Cooling:DX:MultiSpeed, Coil:Cooling:DX:TwoStageWithHumidityControlMode, Coil:Cooling:DX:VariableSpeed, "
                            "Coil:Cooling:WaterToAirHeatPump:EquationFit, Refrigeration:CompressorRack,");
                        ShowContinueError(" Refrigeration:Condenser:AirCooled ,Refrigeration:Condenser:EvaporativeCooled, ");
                        ShowContinueError(" or Refrigeration:Condenser:WaterCooled.");
                        ShowContinueError(" Invalid desuperheater heat source object: " + DataIPShortCuts::cAlphaArgs(9) + " \"" + DataIPShortCuts::cAlphaArgs(10) + "\"");
                        ErrorsFound = true;
                    }
                    if (errFlag) {
                        ShowContinueError("...occurs in " + DataIPShortCuts::cCurrentModuleObject + '=' + WaterHeaterDesuperheater(DesuperheaterNum).Name);
                        ErrorsFound = true;
                    }

                    if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                        "\" desuperheater heat source object not found: " + DataIPShortCuts::cAlphaArgs(9) + " \"" + DataIPShortCuts::cAlphaArgs(10) + "\"");
                        ErrorsFound = true;
                    }

                    // Now have source type, so set limits on heat recovery efficiency
                    if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCONDENSER_REFRIGERATION) {
                        if (DataIPShortCuts::lNumericFieldBlanks(2)) {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = 0.8;
                        } else {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = DataIPShortCuts::rNumericArgs(2);
                            if (WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff <= 0.0 ||
                                WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff > 0.9) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                                DataIPShortCuts::cNumericFieldNames(2) + " must be > 0.0 and <= 0.9, Efficiency = " +
                                                General::TrimSigDigits(WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff, 3));
                                ErrorsFound = true;
                            }
                        }    // Blank Num(2)
                    } else { // max is 0.3 for all other sources
                        if (DataIPShortCuts::lNumericFieldBlanks(2)) {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = 0.25;
                        } else {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = DataIPShortCuts::rNumericArgs(2);
                            if (WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff <= 0.0 ||
                                WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff > 0.3) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                                DataIPShortCuts::cNumericFieldNames(2) + " must be > 0.0 and <= 0.3, " + DataIPShortCuts::cNumericFieldNames(2) + " = " +
                                                General::TrimSigDigits(WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff, 3));
                                ErrorsFound = true;
                            }
                        } // Blank Num(2)
                    }     // setting limits on heat recovery efficiency

                    WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate = DataIPShortCuts::rNumericArgs(6);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate <= 0.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        DataIPShortCuts::cNumericFieldNames(6) + " must be greater than 0. " + DataIPShortCuts::cNumericFieldNames(6) + " = " +
                                        General::TrimSigDigits(DataIPShortCuts::rNumericArgs(6), 6));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower = DataIPShortCuts::rNumericArgs(7);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower < 0.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        DataIPShortCuts::cNumericFieldNames(7) + " must be >= 0. " + DataIPShortCuts::cNumericFieldNames(7) + " = " +
                                        General::TrimSigDigits(DataIPShortCuts::rNumericArgs(7), 2));
                        ErrorsFound = true;
                    }

                    if ((WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower /
                         WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate) > 7.9264e6) {
                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                         DataIPShortCuts::cNumericFieldNames(7) + " to " + DataIPShortCuts::cNumericFieldNames(6) + " ratio > 7.9264E6. " + DataIPShortCuts::cNumericFieldNames(7) +
                                         " to " + DataIPShortCuts::cNumericFieldNames(6) + " = " +
                                         General::TrimSigDigits((WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower /
                                                        WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate),
                                                       3));
                        ShowContinueError(" Suggest reducing " + DataIPShortCuts::cNumericFieldNames(7) + " or increasing " + DataIPShortCuts::cNumericFieldNames(6) + '.');
                        ShowContinueError(" The simulation will continue using the user defined values.");
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater = DataIPShortCuts::rNumericArgs(8);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater < 0.0 ||
                        WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater > 1.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        DataIPShortCuts::cNumericFieldNames(8) + " must be >= 0 or <= 1. " + DataIPShortCuts::cNumericFieldNames(8) + " = " +
                                        General::TrimSigDigits(DataIPShortCuts::rNumericArgs(8), 3));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad = DataIPShortCuts::rNumericArgs(9);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad < 0.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        DataIPShortCuts::cNumericFieldNames(9) + " must be >= 0. " + DataIPShortCuts::cNumericFieldNames(9) + " = " +
                                        General::TrimSigDigits(DataIPShortCuts::rNumericArgs(9), 2));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad = DataIPShortCuts::rNumericArgs(10);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad < 0.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        DataIPShortCuts::cNumericFieldNames(10) + " must be >= 0. " + DataIPShortCuts::cNumericFieldNames(10) + " = " +
                                        General::TrimSigDigits(DataIPShortCuts::rNumericArgs(10), 2));
                        ErrorsFound = true;
                    }
                }

                if (ErrorsFound) {
                    ShowFatalError("Errors found in getting " + DataIPShortCuts::cCurrentModuleObject + " input. Preceding condition causes termination.");
                }
            }

            //  =======   Get HEAT PUMP:WATER HEATER ===============================================================================

            //   get input for heat pump water heater object
            if (NumHeatPumpWaterHeater > 0) {
                int const NumPumpedCondenser =
                    inputProcessor->getNumObjectsFound(cHPWHPumpedCondenser); // number of WaterHeater:HeatPump:PumpedCondenser objects
                int nAlphaOffset;            // the difference of array location between alpha items between pumped and wrapped condensers
                int nNumericOffset;          // the difference of array location between numeric items between pumped and wrapped condensers
                int nNumPossibleNumericArgs; // the number of possible numeric arguments in the idd
                int nNumPossibleAlphaArgs;   // the number of possible numeric arguments in the idd

                for (int HPWaterHeaterNum = 1; HPWaterHeaterNum <= NumHeatPumpWaterHeater; ++HPWaterHeaterNum) {

                    // Create reference to current HPWH object in array.
                    HeatPumpWaterHeaterData &HPWH = HPWaterHeater(HPWaterHeaterNum);
                    WaterHeaterSaveNodes &HPWHSaveNode = HPWHSaveNodeNames(HPWaterHeaterNum);

                    // Initialize the offsets to zero
                    nAlphaOffset = 0;
                    nNumericOffset = 0;
                    if (HPWaterHeaterNum <= NumPumpedCondenser) {
                        // Pumped Condenser
                        DataIPShortCuts::cCurrentModuleObject = cHPWHPumpedCondenser;
                        HPWH.TypeNum = DataPlant::TypeOf_HeatPumpWtrHeaterPumped;
                        nNumPossibleAlphaArgs = 29;
                        nNumPossibleNumericArgs = 9;
                    } else {
                        // Wrapped Condenser
                        DataIPShortCuts::cCurrentModuleObject = cHPWHWrappedCondenser;
                        HPWH.TypeNum = DataPlant::TypeOf_HeatPumpWtrHeaterWrapped;
                        nNumPossibleAlphaArgs = 27;
                        nNumPossibleNumericArgs = 10;
                    }

                    int NumAlphas;
                    int NumNums;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  HPWaterHeaterNum,
                                                  DataIPShortCuts::cAlphaArgs,
                                                  NumAlphas,
                                                  DataIPShortCuts::rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  DataIPShortCuts::lNumericFieldBlanks,
                                                  DataIPShortCuts::lAlphaFieldBlanks,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);

                    // Copy those lists into C++ std::maps
                    std::map<int, std::string> hpwhAlpha;
                    std::map<int, Real64> hpwhNumeric;
                    std::map<int, bool> hpwhAlphaBlank;
                    std::map<int, bool> hpwhNumericBlank;
                    std::map<int, std::string> hpwhAlphaFieldNames;
                    std::map<int, std::string> hpwhNumericFieldNames;
                    for (int i = 1; i <= NumNums; ++i) {
                        hpwhNumeric[i] = DataIPShortCuts::rNumericArgs(i);
                        hpwhNumericBlank[i] = DataIPShortCuts::lNumericFieldBlanks(i);
                        hpwhNumericFieldNames[i] = DataIPShortCuts::cNumericFieldNames(i);
                    }
                    for (int i = NumNums + 1; i <= nNumPossibleNumericArgs; ++i) {
                        hpwhNumericBlank[i] = true;
                    }
                    for (int i = 1; i <= NumAlphas; ++i) {
                        hpwhAlpha[i] = DataIPShortCuts::cAlphaArgs(i);
                        hpwhAlphaBlank[i] = DataIPShortCuts::lAlphaFieldBlanks(i);
                        hpwhAlphaFieldNames[i] = DataIPShortCuts::cAlphaFieldNames(i);
                    }
                    for (int i = NumAlphas + 1; i <= nNumPossibleAlphaArgs; ++i) {
                        hpwhAlphaBlank[i] = true;
                    }
                    UtilityRoutines::IsNameEmpty(hpwhAlpha[1], DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                    // Name and type
                    HPWH.Name = hpwhAlpha[1];
                    HPWH.Type = DataIPShortCuts::cCurrentModuleObject;

                    // Availability Schedule
                    // convert schedule name to pointer
                    if (!hpwhAlphaBlank[2]) {
                        HPWH.AvailSchedPtr = ScheduleManager::GetScheduleIndex(hpwhAlpha[2]);
                        if (HPWH.AvailSchedPtr == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                            ShowContinueError(hpwhAlphaFieldNames[2] + "=\"" + hpwhAlpha[2] + "\".");
                            ErrorsFound = true;
                        }
                    } else {
                        HPWH.AvailSchedPtr = DataGlobals::ScheduleAlwaysOn;
                    }

                    // Compressor Setpoint Temperature Schedule
                    // convert schedule name to pointer
                    if (!hpwhAlphaBlank[3]) {
                        HPWH.SetPointTempSchedule = ScheduleManager::GetScheduleIndex(hpwhAlpha[3]);
                        if (HPWH.SetPointTempSchedule == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                            ShowContinueError(hpwhAlphaFieldNames[3] + "=\"" + hpwhAlpha[3] + "\".");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                        ShowContinueError("required " + hpwhAlphaFieldNames[3] + " is blank.");
                        ErrorsFound = true;
                    }

                    // Dead Band Temperature Difference
                    HPWH.DeadBandTempDiff = hpwhNumeric[1 + nNumericOffset];
                    if (HPWH.DeadBandTempDiff <= 0.0 || HPWH.DeadBandTempDiff > 20.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                        ShowContinueError(hpwhNumericFieldNames[1 + nNumericOffset] +
                                          " difference must be > 0 and <= 20. Dead band = " + General::TrimSigDigits(hpwhNumeric[1 + nNumericOffset], 1));
                        ErrorsFound = true;
                    }

                    if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {

                        // Condenser Inlet/Outlet Nodes
                        HPWH.CondWaterInletNode = NodeInputManager::GetOnlySingleNode(
                            hpwhAlpha[4], ErrorsFound, DataIPShortCuts::cCurrentModuleObject, HPWH.Name, DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsParent);
                        HPWHSaveNode.InletNodeName1 = hpwhAlpha[4];
                        HPWH.CondWaterOutletNode = NodeInputManager::GetOnlySingleNode(
                            hpwhAlpha[5], ErrorsFound, DataIPShortCuts::cCurrentModuleObject, HPWH.Name, DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsParent);
                        HPWHSaveNode.OutletNodeName1 = hpwhAlpha[5];

                        // Condenser Water Flow Rate
                        HPWH.OperatingWaterFlowRate = hpwhNumeric[2];
                        if (HPWH.OperatingWaterFlowRate <= 0.0 && hpwhNumeric[2] != DataGlobals::AutoCalculate) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError(hpwhNumericFieldNames[2] +
                                              " must be greater than 0. Condenser water flow rate = " + General::TrimSigDigits(hpwhNumeric[2], 6));
                            ErrorsFound = true;
                        }

                    } else if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {

                        // Wrapped Condenser Location
                        HPWH.WrappedCondenserBottomLocation = hpwhNumeric[2 + nNumericOffset];
                        HPWH.WrappedCondenserTopLocation = hpwhNumeric[3 + nNumericOffset];

                        if (HPWH.WrappedCondenserBottomLocation < 0.0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError(hpwhNumericFieldNames[2] + " must be greater than 0. Condenser bottom location = " +
                                              General::TrimSigDigits(HPWH.WrappedCondenserBottomLocation, 6));
                            ErrorsFound = true;
                        }

                        if (HPWH.WrappedCondenserBottomLocation >= HPWH.WrappedCondenserTopLocation) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError(hpwhNumericFieldNames[3] + " (" + General::TrimSigDigits(HPWH.WrappedCondenserTopLocation, 6) +
                                              ") must be greater than " + hpwhNumericFieldNames[2] + " (" +
                                              General::TrimSigDigits(HPWH.WrappedCondenserBottomLocation, 6) + ").");
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
                    if (HPWH.OperatingAirFlowRate <= 0.0 && hpwhNumeric[3 + nNumericOffset] != DataGlobals::AutoCalculate) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                        ShowContinueError(hpwhNumericFieldNames[3 + nNumericOffset] +
                                          " must be greater than 0. Evaporator air flow rate = " + General::TrimSigDigits(hpwhNumeric[3 + nNumericOffset], 6));
                        ErrorsFound = true;
                    }

                    // Inlet Air Configuration
                    {
                        auto const SELECT_CASE_var(hpwhAlpha[6 + nAlphaOffset]);

                        if (SELECT_CASE_var == "SCHEDULE") {
                            HPWH.InletAirConfiguration = modAmbientTempSchedule;

                            // Inlet Air Temperature Schedule
                            if (!hpwhAlphaBlank[11 + nAlphaOffset]) {
                                HPWH.AmbientTempSchedule = ScheduleManager::GetScheduleIndex(hpwhAlpha[11 + nAlphaOffset]);
                                if (HPWH.AmbientTempSchedule == 0) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[11 + nAlphaOffset] + "=\"" + hpwhAlpha[11 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[11 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                            // Inlet Air Humidity Schedule
                            if (!hpwhAlphaBlank[12 + nAlphaOffset]) {
                                HPWH.AmbientRHSchedule = ScheduleManager::GetScheduleIndex(hpwhAlpha[12 + nAlphaOffset]);
                                if (HPWH.AmbientRHSchedule == 0) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[12 + nAlphaOffset] + "=\"" + hpwhAlpha[12 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                } else {
                                    if (!ScheduleManager::CheckScheduleValueMinMax(HPWH.AmbientRHSchedule, ">=", 0.0, "<=", 1.0)) {
                                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", invalid values");
                                        ShowContinueError(hpwhAlphaFieldNames[12 + nAlphaOffset] + "=\"" + hpwhAlpha[12 + nAlphaOffset] +
                                                          "\", schedule values must be (>=0., <=1.)");
                                        ErrorsFound = true;
                                    }
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[12 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONEAIRONLY") {
                            HPWH.InletAirConfiguration = modAmbientTempZone;

                            // Inlet Air Zone
                            if (!hpwhAlphaBlank[13 + nAlphaOffset]) {
                                HPWH.AmbientTempZone = UtilityRoutines::FindItemInList(hpwhAlpha[13 + nAlphaOffset], DataHeatBalance::Zone);
                                if (HPWH.AmbientTempZone == 0) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[13 + nAlphaOffset] + "=\"" + hpwhAlpha[13 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[13 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORAIRONLY") {
                            HPWH.InletAirConfiguration = modAmbientTempOutsideAir;

                        } else if (SELECT_CASE_var == "ZONEANDOUTDOORAIR") {
                            HPWH.InletAirConfiguration = modAmbientTempZoneAndOA;

                            // Inlet Air Zone
                            if (!hpwhAlphaBlank[13 + nAlphaOffset]) {
                                HPWH.AmbientTempZone = UtilityRoutines::FindItemInList(hpwhAlpha[13 + nAlphaOffset], DataHeatBalance::Zone);
                                if (HPWH.AmbientTempZone == 0) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[13 + nAlphaOffset] + "=\"" + hpwhAlpha[13 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[13 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Read air inlet nodes after mixer/splitter nodes have been read in (DataIPShortCuts::cAlphaArgs 7-10),
                    // Node_ConnectionType differs for inlet node if mixer/splitter node exists

                    // Tank Name
                    // We will verify this exists and is the right kind of tank later when the tanks are all loaded.
                    HPWH.TankName = hpwhAlpha[15 + nAlphaOffset];
                    HPWH.TankType = hpwhAlpha[14 + nAlphaOffset];

                    // Use Side Inlet/Outlet
                    // Get the water heater tank use side inlet node names for HPWHs connected to a plant loop
                    // Save the name of the node for use with set up comp sets
                    HPWHSaveNode.InletNodeName2 = hpwhAlpha[16 + nAlphaOffset];
                    HPWHSaveNode.OutletNodeName2 = hpwhAlpha[17 + nAlphaOffset];

                    if (!hpwhAlphaBlank[16 + nAlphaOffset] && !hpwhAlphaBlank[17 + nAlphaOffset]) {
                        HPWH.WHUseInletNode = NodeInputManager::GetOnlySingleNode(HPWHSaveNode.InletNodeName2,
                                                                ErrorsFound,
                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                HPWH.Name,
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                1,
                                                                DataLoopNode::ObjectIsParent);
                        HPWH.WHUseOutletNode = NodeInputManager::GetOnlySingleNode(HPWHSaveNode.OutletNodeName2,
                                                                 ErrorsFound,
                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                 HPWH.Name,
                                                                 DataLoopNode::NodeType_Water,
                                                                 DataLoopNode::NodeConnectionType_Outlet,
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
                    DXCoils::GetDXCoilIndex(HPWH.DXCoilName, HPWH.DXCoilNum, DXCoilErrFlag, DataIPShortCuts::cCurrentModuleObject, true);
                    if (DXCoilErrFlag) {
                        // This could be a variable speed heat pump water heater
                        bool bVSCoilErrFlag = false;

                        bool checkIHPFirst = IntegratedHeatPump::IHPInModel();
                        if (checkIHPFirst) {
                            HPWH.DXCoilNum = IntegratedHeatPump::GetCoilIndexIHP("COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", HPWH.DXCoilName, bVSCoilErrFlag);

                            if (!bVSCoilErrFlag) {
                                HPWH.bIsIHP = true;
                            }
                        }

                        if (bVSCoilErrFlag || !checkIHPFirst) {
                            bVSCoilErrFlag = false;
                            HPWH.DXCoilNum =
                                VariableSpeedCoils::GetCoilIndexVariableSpeed("Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed", HPWH.DXCoilName, bVSCoilErrFlag);

                            if (bVSCoilErrFlag) {
                                ShowContinueError("...occurs in " + DataIPShortCuts::cCurrentModuleObject + " =" + HPWH.Name);
                                ShowContinueError("...could not find either DXCoils::DXCoil or Variable Speed Coil " + HPWH.DXCoilName);
                                ErrorsFound = true;
                            }
                        }

                        bIsVScoil = true;
                        HPWH.DXCoilTypeNum = 0;
                        if (HPWH.bIsIHP) {
                            HPWH.DXCoilType = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE";
                        } else {
                            HPWH.DXCoilType = VariableSpeedCoils::VarSpeedCoil(HPWH.DXCoilNum).VarSpeedCoilType;
                        }
                    } else {
                        // this is a single speed coil
                        DXCoils::DXCoilData &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);
                        if (!UtilityRoutines::SameString(HPWH.DXCoilType, Coil.DXCoilType)) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError("specifies the coil " + HPWH.DXCoilType + "=\"" + HPWH.DXCoilName + "\".");
                            ShowContinueError("However, " + HPWH.DXCoilName + " is a coil of type " + Coil.DXCoilType + ".");
                            ErrorsFound = true;
                        }
                        HPWH.DXCoilTypeNum = Coil.DXCoilType_Num;
                    }

                    // Make sure that the coil and tank are compatible.
                    if (bIsVScoil) {
                        if (HPWH.TypeNum != DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed can only be used with a pumped condenser heat pump "
                                              "water heater.");
                            ErrorsFound = true;
                        }
                    } else {
                        if (!((HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterPumped &&
                               HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) ||
                              (HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped &&
                               HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped))) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            std::string ExpectedCoilType;
                            if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                                ExpectedCoilType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatPumpWaterHeaterPumped);
                            } else if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                                ExpectedCoilType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped);
                            } else {
                                assert(0);
                            }
                            ShowContinueError("can only be used with " + ExpectedCoilType);
                            ErrorsFound = true;
                        }
                    }

                    // Dummy condenser Inlet/Outlet Nodes for wrapped tanks
                    if (HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped) {
                        DXCoils::DXCoilData &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);

                        HPWHSaveNode.InletNodeName1 = "DUMMY CONDENSER INLET " + Coil.Name;
                        HPWH.CondWaterInletNode = NodeInputManager::GetOnlySingleNode(HPWHSaveNode.InletNodeName1,
                                                                    ErrorsFound,
                                                                    DataIPShortCuts::cCurrentModuleObject,
                                                                    HPWH.Name,
                                                                    DataLoopNode::NodeType_Water,
                                                                    DataLoopNode::NodeConnectionType_Inlet,
                                                                    2,
                                                                    DataLoopNode::ObjectIsParent);
                        HPWHSaveNode.OutletNodeName1 = "DUMMY CONDENSER OUTLET " + Coil.Name;
                        HPWH.CondWaterOutletNode = NodeInputManager::GetOnlySingleNode(HPWHSaveNode.OutletNodeName1,
                                                                     ErrorsFound,
                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                     HPWH.Name,
                                                                     DataLoopNode::NodeType_Water,
                                                                     DataLoopNode::NodeConnectionType_Outlet,
                                                                     2,
                                                                     DataLoopNode::ObjectIsParent);
                    }

                    // Minimum Inlet Air Temperature for Compressor Operation
                    HPWH.MinAirTempForHPOperation = hpwhNumeric[4 + nNumericOffset];
                    if (HPWH.MinAirTempForHPOperation < -5) {
                        ShowWarningError(
                            DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name +
                            "\": minimum inlet air temperature for heat pump compressor operation must be greater than or equal to -5 C.");
                        ShowContinueError("...Minimum inlet air temperature = " + General::TrimSigDigits(hpwhNumeric[4 + nNumericOffset], 1));
                    }

                    // Maximum Inlet Air Temperature for Compressor Operation
                    HPWH.MaxAirTempForHPOperation = hpwhNumeric[5 + nNumericOffset];
                    if (HPWH.MaxAirTempForHPOperation <= HPWH.MinAirTempForHPOperation) {
                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name +
                                         "\": maximum inlet air temperature for heat pump compressor operation");
                        ShowContinueError("must be greater than the minimum inlet air temperature for heat pump compressor operation.");
                        ShowContinueError("...Minimum inlet air temperature = " + General::TrimSigDigits(HPWH.MinAirTempForHPOperation, 1));
                        ShowContinueError("...Maximum inlet air temperature = " + General::TrimSigDigits(HPWH.MaxAirTempForHPOperation, 1));
                    }

                    // Compressor Location
                    {
                        auto const SELECT_CASE_var(hpwhAlpha[20 + nAlphaOffset]);
                        if (SELECT_CASE_var == "SCHEDULE") {
                            HPWH.CrankcaseTempIndicator = modCrankcaseTempSchedule;
                            if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                                // Compressor Ambient Temperature Schedule
                                HPWH.CrankcaseTempSchedule = ScheduleManager::GetScheduleIndex(hpwhAlpha[21 + nAlphaOffset]);
                                if (HPWH.CrankcaseTempSchedule == 0) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[21 + nAlphaOffset] + "=\"" + hpwhAlpha[21 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[21 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            HPWH.CrankcaseTempIndicator = modCrankcaseTempZone;
                            if (HPWH.InletAirConfiguration == modAmbientTempOutsideAir || HPWH.InletAirConfiguration == modAmbientTempSchedule) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name +
                                                "\":  Inlet Air Configuration must be Zone Air Only or Zone And");
                                ShowContinueError(" Outdoor Air when compressor location equals ZONE.");
                                ErrorsFound = true;
                            }

                            if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                                ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\"  " + hpwhAlphaFieldNames[21 + nAlphaOffset] +
                                                 " was provided but will not be used based on compressor location input=\"" +
                                                 hpwhAlpha[20 + nAlphaOffset] + "\".");
                            }
                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            HPWH.CrankcaseTempIndicator = modCrankcaseTempExterior;
                            if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                                ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\"  " + hpwhAlphaFieldNames[21 + nAlphaOffset] +
                                                 " was provided but will not be used based on " + hpwhAlphaFieldNames[21 + nAlphaOffset] + "=\"" +
                                                 hpwhAlpha[20 + nAlphaOffset] + "\".");
                            }
                        }
                    }

                    // Fan Name
                    HPWH.FanName = hpwhAlpha[23 + nAlphaOffset];
                    HPWH.FanType = hpwhAlpha[22 + nAlphaOffset];

                    // check that the fan exists
                    bool errFlag = false;
                    ValidateComponent(HPWH.FanType, HPWH.FanName, errFlag, DataIPShortCuts::cCurrentModuleObject);

                    Real64 FanVolFlow = 0.0;
                    if (errFlag) {
                        ShowContinueError("...occurs in " + DataIPShortCuts::cCurrentModuleObject + ", unit=\"" + HPWH.Name + "\".");
                        ErrorsFound = true;
                    } else {
                        if (UtilityRoutines::SameString(HPWH.FanType, "Fan:SystemModel")) {
                            HPWH.FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                            HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(HPWH.FanName)); // call constructor
                            HPWH.FanNum = HVACFan::getFanObjectVectorIndex(HPWH.FanName);
                            FanVolFlow = HVACFan::fanObjs[HPWH.FanNum]->designAirVolFlowRate;

                        } else {
                            Fans::GetFanType(HPWH.FanName, HPWH.FanType_Num, errFlag, DataIPShortCuts::cCurrentModuleObject, HPWH.Name);
                            Fans::GetFanIndex(HPWH.FanName, HPWH.FanNum, errFlag, DataIPShortCuts::cCurrentModuleObject);
                            Fans::GetFanVolFlow(HPWH.FanNum, FanVolFlow);
                        }
                    }
                    // issue #5630, set fan info in coils.
                    if (bIsVScoil) {
                        VariableSpeedCoils::setVarSpeedHPWHFanTypeNum(HPWH.DXCoilNum, HPWH.FanType_Num);
                        VariableSpeedCoils::setVarSpeedHPWHFanIndex(HPWH.DXCoilNum, HPWH.FanNum);
                    } else {
                        DXCoils::SetDXCoolingCoilData(
                            HPWH.DXCoilNum, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HPWH.FanName);
                        DXCoils::SetDXCoolingCoilData(HPWH.DXCoilNum, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HPWH.FanNum);
                        DXCoils::SetDXCoolingCoilData(
                            HPWH.DXCoilNum, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, HPWH.FanType_Num);
                    }

                    if (errFlag) {
                        ErrorsFound = true;
                    } else if (HPWH.FanType_Num != DataHVACGlobals::FanType_SimpleOnOff &&
                               HPWH.FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\": illegal fan type specified.");
                        ShowContinueError(" The fan object (" + HPWH.FanName +
                                          ") type must be Fan:SystemModel or Fan:OnOff when used with a heat pump water heater.");
                        ErrorsFound = true;
                    } else if (!UtilityRoutines::SameString(HPWH.FanType, "Fan:OnOff") &&
                               !UtilityRoutines::SameString(HPWH.FanType, "Fan:SystemModel")) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\": illegal fan type specified.");
                        ShowContinueError(" The " + DataIPShortCuts::cCurrentModuleObject + " must specify that the fan object");
                        ShowContinueError(
                            " is of type FanSystemModel or Fan:OnOff in addition to the fan actually being of that type and defined elsewhere.");
                    }

                    if (FanVolFlow != DataSizing::AutoSize && !errFlag) {
                        if (FanVolFlow < HPWH.OperatingAirFlowRate) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " - air flow rate = " + General::TrimSigDigits(FanVolFlow, 7) + " in fan object " +
                                            HPWH.FanName + " is less than the  HPWHs evaporator air flow rate.");
                            ShowContinueError(" The fan flow rate must be >= to the HPWHs evaporator volumetric air flow rate.");
                            ShowContinueError(" Occurs in unit = " + HPWH.Name);
                            ErrorsFound = true;
                        }
                    }

                    // Fan Placement
                    if (UtilityRoutines::SameString(hpwhAlpha[24 + nAlphaOffset], "BlowThrough")) {
                        HPWH.FanPlacement = DataHVACGlobals::BlowThru;

                    } else if (UtilityRoutines::SameString(hpwhAlpha[24 + nAlphaOffset], "DrawThrough")) {
                        HPWH.FanPlacement = DataHVACGlobals::DrawThru;

                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", invalid ");
                        ShowContinueError(hpwhAlphaFieldNames[24 + nAlphaOffset] + "=\"" + hpwhAlpha[24 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    }

                    if (HPWH.DXCoilNum > 0 && !bIsVScoil) {
                        // get HPWH capacity, air inlet node, and PLF curve info from DX coil object
                        HPWH.Capacity = DXCoils::DXCoil(HPWH.DXCoilNum).RatedTotCap2;
                        HPWH.DXCoilAirInletNode = DXCoils::DXCoil(HPWH.DXCoilNum).AirInNode;
                        HPWH.DXCoilPLFFPLR = DXCoils::DXCoil(HPWH.DXCoilNum).PLFFPLR(1);
                        // check the range of condenser pump power to be <= 5 gpm/ton
                        if (DXCoils::DXCoil(HPWH.DXCoilNum).HPWHCondPumpElecNomPower / DXCoils::DXCoil(HPWH.DXCoilNum).RatedTotCap2 > 0.1422) {
                            ShowWarningError(
                                DXCoils::DXCoil(HPWH.DXCoilNum).DXCoilType + "= " + DXCoils::DXCoil(HPWH.DXCoilNum).Name +
                                ": Rated condenser pump power per watt of rated heating capacity has exceeded the recommended maximum of 0.1422 W/W "
                                "(41.67 watt/MBH). Condenser pump power per watt = " +
                                General::TrimSigDigits((DXCoils::DXCoil(HPWH.DXCoilNum).HPWHCondPumpElecNomPower / DXCoils::DXCoil(HPWH.DXCoilNum).RatedTotCap2), 4));
                        }
                    } else if ((HPWH.DXCoilNum > 0) && (bIsVScoil)) {

                        if (HPWH.bIsIHP) {
                            HPWH.Capacity = GetDWHCoilCapacityIHP(HPWH.DXCoilType, HPWH.DXCoilName, IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode, DXCoilErrFlag);
                            HPWH.DXCoilAirInletNode = IntegratedHeatPump::GetCoilInletNodeIHP(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            HPWH.DXCoilPLFFPLR =
                                GetIHPDWHCoilPLFFPLR(HPWH.DXCoilType, HPWH.DXCoilName, IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode, DXCoilErrFlag);
                        } else {
                            HPWH.Capacity = VariableSpeedCoils::GetCoilCapacityVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            HPWH.DXCoilAirInletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            HPWH.DXCoilPLFFPLR = VariableSpeedCoils::GetVSCoilPLFFPLR(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                        }
                        //         check the range of condenser pump power to be <= 5 gpm/ton, will be checked in the coil object
                    }

                    if (HPWH.OperatingWaterFlowRate == DataGlobals::AutoCalculate) {
                        HPWH.OperatingWaterFlowRate = 0.00000004487 * HPWH.Capacity;
                        HPWH.WaterFlowRateAutoSized = true;
                    }

                    if (HPWH.OperatingAirFlowRate == DataGlobals::AutoCalculate) {
                        HPWH.OperatingAirFlowRate = 0.00005035 * HPWH.Capacity;
                        HPWH.AirFlowRateAutoSized = true;
                    }

                    // On Cycle Parasitic Electric Load
                    HPWH.OnCycParaLoad = hpwhNumeric[6 + nNumericOffset];
                    if (HPWH.OnCycParaLoad < 0.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                        ShowContinueError(hpwhNumericFieldNames[6 + nNumericOffset] + " must be >= 0. " + hpwhNumericFieldNames[6 + nNumericOffset] +
                                          " = " + General::TrimSigDigits(hpwhNumeric[6 + nNumericOffset], 2));
                        ErrorsFound = true;
                    }

                    // Off Cycle Parasitic Electric Load
                    HPWH.OffCycParaLoad = hpwhNumeric[7 + nNumericOffset];
                    if (HPWH.OffCycParaLoad < 0.0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                        ShowContinueError(hpwhNumericFieldNames[7 + nNumericOffset] + " must be >= 0. " + hpwhNumericFieldNames[2 + nNumericOffset] +
                                          " = " + General::TrimSigDigits(hpwhNumeric[7 + nNumericOffset], 2));
                        ErrorsFound = true;
                    }

                    // Parasitic Heat Rejection Location
                    if (UtilityRoutines::SameString(hpwhAlpha[25 + nAlphaOffset], "Zone")) {
                        HPWH.ParasiticTempIndicator = modAmbientTempZone;
                        if (HPWH.InletAirConfiguration == modAmbientTempOutsideAir || HPWH.InletAirConfiguration == modAmbientTempSchedule) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                            ShowContinueError(hpwhAlphaFieldNames[25 + nAlphaOffset] + " must be ZoneAirOnly or ZoneAndOutdoorAir");
                            ShowContinueError(" when parasitic heat rejection location equals Zone.");
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(hpwhAlpha[25 + nAlphaOffset], "Outdoors")) {
                        HPWH.ParasiticTempIndicator = modAmbientTempOutsideAir;
                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError(" parasitic heat rejection location must be either Zone or Outdoors.");
                        ErrorsFound = true;
                    }

                    // Inlet Air Mixer Node
                    // get mixer/splitter nodes only when Inlet Air Configuration is ZoneAndOutdoorAir
                    if (!hpwhAlphaBlank[26 + nAlphaOffset]) {
                        // For the inlet air mixer node, NodeConnectionType is outlet from the HPWH inlet air node
                        if (HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                            HPWH.InletAirMixerNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[26 + nAlphaOffset],
                                                                       ErrorsFound,
                                                                       DataIPShortCuts::cCurrentModuleObject + " inlet air mixer",
                                                                       HPWH.Name,
                                                                       DataLoopNode::NodeType_Air,
                                                                       DataLoopNode::NodeConnectionType_Outlet,
                                                                       1,
                                                                       DataLoopNode::ObjectIsNotParent);
                        } else {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Inlet air mixer node name specified but only required when Inlet Air Configuration is selected as "
                                              "Zone and OutdoorAir. Node name disregarded and simulation continues.");
                        }
                    } else if (hpwhAlphaBlank[26 + nAlphaOffset] && HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError("Inlet air mixer node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir.");
                        ErrorsFound = true;
                    }

                    // Outlet Air Splitter Node
                    if (!hpwhAlphaBlank[27 + nAlphaOffset]) {
                        //  For the outlet air splitter node, NodeConnectionType is inlet to the HPWH outlet air node
                        if (HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                            HPWH.OutletAirSplitterNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[27 + nAlphaOffset],
                                                                           ErrorsFound,
                                                                           DataIPShortCuts::cCurrentModuleObject + "-OUTLET AIR SPLITTER",
                                                                           HPWH.Name,
                                                                           DataLoopNode::NodeType_Air,
                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                           1,
                                                                           DataLoopNode::ObjectIsNotParent);
                        } else {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Outlet air splitter node name specified but only required when Inlet Air Configuration is selected as "
                                              "ZoneAndOutdoorAir. Node name disregarded and simulation continues.");
                        }
                    } else if (hpwhAlphaBlank[27 + nAlphaOffset] && HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError("Outlet air splitter node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir.");
                        ErrorsFound = true;
                    }

                    // get node data for HPWH
                    if (HPWH.InletAirMixerNode != 0) {
                        // when mixer/splitter nodes are used the HPWH's inlet/outlet node are set up as DataLoopNode::ObjectIsNotParent

                        HPWH.HeatPumpAirInletNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[7 + nAlphaOffset],
                                                                      ErrorsFound,
                                                                      DataIPShortCuts::cCurrentModuleObject + "-INLET AIR MIXER",
                                                                      HPWH.Name,
                                                                      DataLoopNode::NodeType_Air,
                                                                      DataLoopNode::NodeConnectionType_Inlet,
                                                                      1,
                                                                      DataLoopNode::ObjectIsNotParent);

                        HPWH.HeatPumpAirOutletNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[8 + nAlphaOffset],
                                                                       ErrorsFound,
                                                                       DataIPShortCuts::cCurrentModuleObject + "-OUTLET AIR SPLITTER",
                                                                       HPWH.Name,
                                                                       DataLoopNode::NodeType_Air,
                                                                       DataLoopNode::NodeConnectionType_Outlet,
                                                                       1,
                                                                       DataLoopNode::ObjectIsNotParent);

                        HPWH.OutsideAirNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[9 + nAlphaOffset],
                                                                ErrorsFound,
                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                HPWH.Name,
                                                                DataLoopNode::NodeType_Air,
                                                                DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                1,
                                                                DataLoopNode::ObjectIsParent);
                        if (hpwhAlpha[9 + nAlphaOffset] != "") {
                            bool Okay;
                            OutAirNodeManager::CheckAndAddAirNodeNumber(HPWH.OutsideAirNode, Okay);
                            if (!Okay) {
                                ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name +
                                                 "\": Adding outdoor air node=" + hpwhAlpha[9 + nAlphaOffset]);
                            }
                        }

                        HPWH.ExhaustAirNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[10 + nAlphaOffset],
                                                                ErrorsFound,
                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                HPWH.Name,
                                                                DataLoopNode::NodeType_Air,
                                                                DataLoopNode::NodeConnectionType_ReliefAir,
                                                                1,
                                                                DataLoopNode::ObjectIsParent);

                    } else {
                        // when mixer/splitter nodes are NOT used the HPWH's inlet/outlet nodes are set up as DataLoopNode::ObjectIsParent
                        if (HPWH.InletAirConfiguration == modAmbientTempSchedule) {
                            // for scheduled HPWH's the inlet node is not on any branch or parent object, make it an outlet node
                            // to avoid node connection errors
                            HPWH.HeatPumpAirInletNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[7 + nAlphaOffset],
                                                                          ErrorsFound,
                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                          HPWH.Name,
                                                                          DataLoopNode::NodeType_Air,
                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                          1,
                                                                          DataLoopNode::ObjectIsParent);

                            HPWH.HeatPumpAirOutletNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[8 + nAlphaOffset],
                                                                           ErrorsFound,
                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                           HPWH.Name,
                                                                           DataLoopNode::NodeType_Air,
                                                                           DataLoopNode::NodeConnectionType_Outlet,
                                                                           1,
                                                                           DataLoopNode::ObjectIsParent);

                        } else { // HPWH is connected to a zone with no mixer/splitter nodes
                            if (HPWH.InletAirConfiguration == modAmbientTempZone) {
                                HPWH.HeatPumpAirInletNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[7 + nAlphaOffset],
                                                                              ErrorsFound,
                                                                              DataIPShortCuts::cCurrentModuleObject,
                                                                              HPWH.Name,
                                                                              DataLoopNode::NodeType_Air,
                                                                              DataLoopNode::NodeConnectionType_Inlet,
                                                                              1,
                                                                              DataLoopNode::ObjectIsParent);

                                HPWH.HeatPumpAirOutletNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[8 + nAlphaOffset],
                                                                               ErrorsFound,
                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                               HPWH.Name,
                                                                               DataLoopNode::NodeType_Air,
                                                                               DataLoopNode::NodeConnectionType_Outlet,
                                                                               1,
                                                                               DataLoopNode::ObjectIsParent);
                            } else { // HPWH is located outdoors
                                HPWH.OutsideAirNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[9 + nAlphaOffset],
                                                                        ErrorsFound,
                                                                        DataIPShortCuts::cCurrentModuleObject,
                                                                        HPWH.Name,
                                                                        DataLoopNode::NodeType_Air,
                                                                        DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                        1,
                                                                        DataLoopNode::ObjectIsParent);
                                if (!hpwhAlphaBlank[9 + nAlphaOffset]) {
                                    bool Okay;
                                    OutAirNodeManager::CheckAndAddAirNodeNumber(HPWH.OutsideAirNode, Okay);
                                    if (!Okay) {
                                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name +
                                                         "\": Adding outdoor air node =" + hpwhAlpha[9 + nAlphaOffset]);
                                    }
                                }

                                HPWH.ExhaustAirNode = NodeInputManager::GetOnlySingleNode(hpwhAlpha[10 + nAlphaOffset],
                                                                        ErrorsFound,
                                                                        DataIPShortCuts::cCurrentModuleObject,
                                                                        HPWH.Name,
                                                                        DataLoopNode::NodeType_Air,
                                                                        DataLoopNode::NodeConnectionType_ReliefAir,
                                                                        1,
                                                                        DataLoopNode::ObjectIsParent);
                            }
                        }
                    }
                    // check that required node names are present
                    if (HPWH.InletAirConfiguration == modAmbientTempSchedule || HPWH.InletAirConfiguration == modAmbientTempZone) {
                        if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                            ShowContinueError(hpwhAlphaFieldNames[7 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[8 + nAlphaOffset] +
                                              " must be specified.");
                            ErrorsFound = true;
                        }
                    } else if (HPWH.InletAirConfiguration == modAmbientTempOutsideAir) {
                        if (HPWH.OutsideAirNode == 0 || HPWH.ExhaustAirNode == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                            ShowContinueError(hpwhAlphaFieldNames[9 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[10 + nAlphaOffset] +
                                              " must be specified.");
                            ErrorsFound = true;
                        }
                    } else if (HPWH.InletAirMixerNode > 0 && HPWH.OutletAirSplitterNode > 0 && HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                        if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0 || HPWH.OutsideAirNode == 0 ||
                            HPWH.ExhaustAirNode == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                            if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0) {
                                ShowContinueError(hpwhAlphaFieldNames[7 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[8 + nAlphaOffset] +
                                                  " must be specified.");
                            }
                            if (HPWH.OutsideAirNode == 0 || HPWH.ExhaustAirNode == 0) {
                                ShowContinueError(hpwhAlphaFieldNames[9 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[10 + nAlphaOffset] +
                                                  " must be specified.");
                            }
                            ErrorsFound = true;
                        }
                    }

                    // check that the HPWH inlet and outlet nodes are in the same zone (ZoneHVAC:EquipmentConnections) when
                    // Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
                    if ((HPWH.InletAirConfiguration == modAmbientTempZone || HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) &&
                        HPWH.AmbientTempZone > 0) {
                        if (!DataZoneEquipment::ZoneEquipInputsFilled) {
                            DataZoneEquipment::GetZoneEquipmentData();
                            DataZoneEquipment::ZoneEquipInputsFilled = true;
                        }
                        if (allocated(DataZoneEquipment::ZoneEquipConfig)) {
                            bool FoundInletNode = false;
                            bool FoundOutletNode = false;
                            int ZoneNum;
                            for (ZoneNum = 1; ZoneNum <= DataGlobals::NumOfZones; ++ZoneNum) {
                                if (HPWH.AmbientTempZone == DataZoneEquipment::ZoneEquipConfig(ZoneNum).ActualZoneNum) break;
                            }
                            if (ZoneNum <= DataGlobals::NumOfZones) {
                                for (int SupAirIn = 1; SupAirIn <= DataZoneEquipment::ZoneEquipConfig(ZoneNum).NumInletNodes; ++SupAirIn) {
                                    if (HPWH.HeatPumpAirOutletNode != DataZoneEquipment::ZoneEquipConfig(ZoneNum).InletNode(SupAirIn)) continue;
                                    FoundOutletNode = true;
                                }
                                for (int ExhAirOut = 1; ExhAirOut <= DataZoneEquipment::ZoneEquipConfig(ZoneNum).NumExhaustNodes; ++ExhAirOut) {
                                    if (HPWH.HeatPumpAirInletNode != DataZoneEquipment::ZoneEquipConfig(ZoneNum).ExhaustNode(ExhAirOut)) continue;
                                    FoundInletNode = true;
                                }
                                if (!FoundInletNode) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                                    ShowContinueError("The HPWH's air inlet node name = " + hpwhAlpha[7 + nAlphaOffset] +
                                                      " was not properly specified ");
                                    ShowContinueError("as an exhaust air node for zone = " + hpwhAlpha[13 + nAlphaOffset] +
                                                      " in a ZoneHVAC:EquipmentConnections object.");
                                    ErrorsFound = true;
                                }
                                if (!FoundOutletNode) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                                    ShowContinueError("The HPWH's air outlet node name = " + hpwhAlpha[8 + nAlphaOffset] +
                                                      " was not properly specified ");
                                    ShowContinueError("as an inlet air node for zone = " + hpwhAlpha[13 + nAlphaOffset] +
                                                      " in a ZoneHVAC:EquipmentConnections object.");
                                    ErrorsFound = true;
                                }
                            }
                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Heat pump water heater air inlet node name and air outlet node name must be listed in a "
                                              "ZoneHVAC:EquipmentConnections object when Inlet Air Configuration is equal to ZoneAirOnly or "
                                              "ZoneAndOutdoorAir.");
                            ErrorsFound = true;
                        }
                    }

                    // only get the inlet air mixer schedule if the inlet air configuration is zone and outdoor air
                    if (!hpwhAlphaBlank[28 + nAlphaOffset] && HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                        HPWH.InletAirMixerSchPtr = ScheduleManager::GetScheduleIndex(hpwhAlpha[28 + nAlphaOffset]);
                        if (HPWH.InletAirMixerSchPtr == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                            ShowContinueError(hpwhAlphaFieldNames[28 + nAlphaOffset] + "=\"" + hpwhAlpha[28 + nAlphaOffset] + "\",");
                            ErrorsFound = true;
                        } else {
                            bool ValidScheduleValue = ScheduleManager::CheckScheduleValueMinMax(HPWH.InletAirMixerSchPtr, ">=", 0.0, "<=", 1.0);
                            if (!ValidScheduleValue) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                ShowContinueError(hpwhAlphaFieldNames[28 + nAlphaOffset] + " values out of range of 0 to 1, Schedule=\"" +
                                                  hpwhAlpha[28 + nAlphaOffset] + "\".");
                                ErrorsFound = true;
                            }
                            //           set outlet air splitter schedule index equal to inlet air mixer schedule index
                            //           (place holder for when zone pressurization/depressurization is allowed and different schedules can be used)
                            HPWH.OutletAirSplitterSchPtr = ScheduleManager::GetScheduleIndex(hpwhAlpha[28 + nAlphaOffset]);
                        }
                    }

                    // set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
                    if (HPWH.FanPlacement == DataHVACGlobals::DrawThru) {
                        if (HPWH.OutletAirSplitterNode != 0) {
                            HPWH.FanOutletNode = HPWH.OutletAirSplitterNode;
                        } else {
                            if (HPWH.InletAirConfiguration == modAmbientTempOutsideAir) {
                                HPWH.FanOutletNode = HPWH.ExhaustAirNode;
                            } else {
                                HPWH.FanOutletNode = HPWH.HeatPumpAirOutletNode;
                            }
                        }
                    } else if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
                        // set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
                        if (bIsVScoil) {
                            if (HPWH.bIsIHP) {
                                HPWH.FanOutletNode = IntegratedHeatPump::GetDWHCoilInletNodeIHP(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            } else {
                                HPWH.FanOutletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            }
                        } else {
                            HPWH.FanOutletNode = DXCoils::DXCoil(HPWH.DXCoilNum).AirInNode;
                        }
                    }

                    // check that fan outlet node is indeed correct
                    int FanOutletNodeNum(0);
                    if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        FanOutletNodeNum = HVACFan::fanObjs[HPWH.FanNum]->outletNodeNum;
                    } else {
                        errFlag = false;
                        FanOutletNodeNum = Fans::GetFanOutletNode(HPWH.FanType, HPWH.FanName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in unit=\"" + HPWH.Name + "\".");
                            ErrorsFound = true;
                        }
                    }
                    if (FanOutletNodeNum != HPWH.FanOutletNode) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError("Heat pump water heater fan outlet node name does not match next connected component.");
                        if (FanOutletNodeNum != 0) {
                            ShowContinueError("Fan outlet node name = " + DataLoopNode::NodeID(FanOutletNodeNum));
                        }
                        if (HPWH.FanOutletNode != 0) {
                            ShowContinueError("Expected fan outlet node name = " + DataLoopNode::NodeID(HPWH.FanOutletNode));
                        }
                        ErrorsFound = true;
                    }
                    int FanInletNodeNum(0);
                    if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        FanInletNodeNum = HVACFan::fanObjs[HPWH.FanNum]->inletNodeNum;
                    } else {
                        errFlag = false;
                        FanInletNodeNum = Fans::GetFanInletNode(HPWH.FanType, HPWH.FanName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in unit=\"" + HPWH.Name + "\".");
                            ErrorsFound = true;
                        }
                    }
                    int HPWHFanInletNodeNum(0);
                    if (HPWH.InletAirMixerNode != 0) {
                        HPWHFanInletNodeNum = HPWH.InletAirMixerNode;
                    } else {
                        if (HPWH.InletAirConfiguration == modAmbientTempOutsideAir) {
                            HPWHFanInletNodeNum = HPWH.OutsideAirNode;
                        } else {
                            HPWHFanInletNodeNum = HPWH.HeatPumpAirInletNode;
                        }
                    }
                    if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
                        if (FanInletNodeNum != HPWHFanInletNodeNum) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Heat pump water heater fan inlet node name does not match previous connected component.");
                            if (FanOutletNodeNum != 0) {
                                ShowContinueError("Fan inlet node name = " + DataLoopNode::NodeID(FanInletNodeNum));
                            }
                            if (HPWH.FanOutletNode != 0) {
                                ShowContinueError("Expected fan inlet node name = " + DataLoopNode::NodeID(HPWHFanInletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    }

                    int DXCoilAirOutletNodeNum(0);
                    if ((HPWH.DXCoilNum > 0) && (bIsVScoil)) {
                        if (HPWH.bIsIHP) {
                            DXCoilAirOutletNodeNum = IntegratedHeatPump::GetDWHCoilOutletNodeIHP(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                        } else {
                            DXCoilAirOutletNodeNum = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                        }

                    } else if (HPWH.DXCoilNum > 0) {
                        DXCoilAirOutletNodeNum = DXCoils::DXCoil(HPWH.DXCoilNum).AirOutNode;
                    }
                    if (HPWH.FanPlacement == DataHVACGlobals::DrawThru) {
                        if (FanInletNodeNum != DXCoilAirOutletNodeNum) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Heat pump water heater fan inlet node name does not match previous connected component.");
                            if (FanInletNodeNum != 0) {
                                ShowContinueError("Fan inlet node name = " + DataLoopNode::NodeID(FanInletNodeNum));
                            }
                            if (DXCoilAirOutletNodeNum != 0) {
                                ShowContinueError("Expected fan inlet node name = " + DataLoopNode::NodeID(DXCoilAirOutletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    } else if (HPWH.FanPlacement == DataHVACGlobals::BlowThru) {
                        int HPWHCoilOutletNodeNum(0);
                        if (HPWH.OutletAirSplitterNode != 0) {
                            HPWHCoilOutletNodeNum = HPWH.OutletAirSplitterNode;
                        } else {
                            if (HPWH.InletAirConfiguration == modAmbientTempOutsideAir) {
                                HPWHCoilOutletNodeNum = HPWH.ExhaustAirNode;
                            } else {
                                HPWHCoilOutletNodeNum = HPWH.HeatPumpAirOutletNode;
                            }
                        }
                        if (DXCoilAirOutletNodeNum != HPWHCoilOutletNodeNum) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Heat pump water heater coil outlet node name does not match next connected component.");
                            if (DXCoilAirOutletNodeNum != 0) {
                                ShowContinueError("Coil outlet node name = " + DataLoopNode::NodeID(DXCoilAirOutletNodeNum));
                            }
                            if (HPWHCoilOutletNodeNum != 0) {
                                ShowContinueError("Expected coil outlet node name = " + DataLoopNode::NodeID(HPWHCoilOutletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    }

                    // set the max mass flow rate for outdoor fans
                    if (HPWH.FanOutletNode > 0)
                        DataLoopNode::Node(HPWH.FanOutletNode).MassFlowRateMax = HPWH.OperatingAirFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);

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
                        BranchNodeConnections::SetUpCompSets(HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName + " Outdoor Coil", HPWH.CoilInletNode_str, HPWH.CoilOutletNode_str);
                    } else {
                        BranchNodeConnections::SetUpCompSets(HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName, HPWH.CoilInletNode_str, HPWH.CoilOutletNode_str);
                    }

                    BranchNodeConnections::SetUpCompSets(HPWH.Type, HPWH.Name, HPWH.FanType, HPWH.FanName, HPWH.FanInletNode_str, HPWH.FanOutletNode_str);

                    // Control Logic Flag
                    std::string CtrlLogicFlag = hpwhAlphaBlank[29 + nAlphaOffset] ? "SIMULTANEOUS" : hpwhAlpha[29 + nAlphaOffset];
                    if (UtilityRoutines::SameString(CtrlLogicFlag, "SIMULTANEOUS")) {
                        HPWH.AllowHeatingElementAndHeatPumpToRunAtSameTime = true;
                    } else if (UtilityRoutines::SameString(CtrlLogicFlag, "MUTUALLYEXCLUSIVE")) {
                        HPWH.AllowHeatingElementAndHeatPumpToRunAtSameTime = false;
                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError(CtrlLogicFlag + " is not a valid value for field Tank Element Control Logic.");
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

                } // DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

            } // IF (NumHeatPumpWaterHeater > 0) THEN

            //  =======   Get WATER HEATER:MIXED ===================================================================================
            if (NumWaterHeaterMixed > 0) {
                DataIPShortCuts::cCurrentModuleObject = cMixedWHModuleObj;
                for (int WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterHeaterMixed; ++WaterThermalTankNum) {
                    int NumAlphas;
                    int NumNums;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  WaterThermalTankNum,
                                                  DataIPShortCuts::cAlphaArgs,
                                                  NumAlphas,
                                                  DataIPShortCuts::rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  DataIPShortCuts::lNumericFieldBlanks,
                                                  DataIPShortCuts::lAlphaFieldBlanks,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = DataIPShortCuts::cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = DataIPShortCuts::cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = DataPlant::TypeOf_WtrHeaterMixed;
                    WaterThermalTank(WaterThermalTankNum).FluidIndex = modWaterIndex;

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;

                    // A user field will be added in a later release
                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = "Water Heater";

                    WaterThermalTank(WaterThermalTankNum).Volume = DataIPShortCuts::rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                        // Set volume to a really small number to simulate a tankless/instantaneous water heater
                        WaterThermalTank(WaterThermalTankNum).Volume = 0.000001; // = 1 cm3
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
                    if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
                        ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", missing data.");
                        ShowContinueError("blank field, missing " + DataIPShortCuts::cAlphaFieldNames(2) + " is required");
                        ErrorsFound = true;
                    } else if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  " + DataIPShortCuts::cAlphaFieldNames(2) + " not found = " + DataIPShortCuts::cAlphaArgs(2));
                        ErrorsFound = true;
                    }

                    if (DataIPShortCuts::rNumericArgs(2) > 0.0001) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = DataIPShortCuts::rNumericArgs(2);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.5;
                    }

                    if (DataIPShortCuts::rNumericArgs(3) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = DataIPShortCuts::rNumericArgs(3);
                    } else {
                        // Default to very large number
                        // BG comment why a large number here why not boilng point of water?
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 100.0; // 1.0E9
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = DataIPShortCuts::rNumericArgs(4);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    if ((DataIPShortCuts::rNumericArgs(5) > WaterThermalTank(WaterThermalTankNum).MaxCapacity) &&
                        (!WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                        ":  Heater Minimum Capacity cannot be greater than Heater Maximum Capacity");
                        ErrorsFound = true;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = DataIPShortCuts::rNumericArgs(5);
                    }

                    // Validate Heater Control Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(3));
                        if (SELECT_CASE_var == "CYCLE") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = modControlTypeCycle;
                            WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;

                        } else if (SELECT_CASE_var == "MODULATE") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = modControlTypeModulate;

                            // CASE ('MODULATE WITH OVERHEAT')  ! Not yet implemented

                            // CASE ('MODULATE WITH UNDERHEAT')  ! Not yet implemented

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Control Type entered=" + DataIPShortCuts::cAlphaArgs(3));
                            ErrorsFound = true;
                        }
                    }
                    WaterThermalTank(WaterThermalTankNum).VolFlowRateMin = DataIPShortCuts::rNumericArgs(6);
                    WaterThermalTank(WaterThermalTankNum).VolFlowRateMin = max(0.0, WaterThermalTank(WaterThermalTankNum).VolFlowRateMin);
                    WaterThermalTank(WaterThermalTankNum).IgnitionDelay = DataIPShortCuts::rNumericArgs(7); // Not yet implemented

                    // Validate Heater Fuel Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(4));
                        if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";

                        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Gas";

                        } else if (SELECT_CASE_var == "DIESEL") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Diesel";

                        } else if (SELECT_CASE_var == "GASOLINE") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Gasoline";

                        } else if (SELECT_CASE_var == "COAL") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Coal";

                        } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                                   (SELECT_CASE_var == "DISTILLATE OIL")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "FuelOil#1";

                        } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "FuelOil#2";

                        } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                                   (SELECT_CASE_var == "PROPANE GAS")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Propane";

                        } else if (SELECT_CASE_var == "OTHERFUEL1") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "OtherFuel1";

                        } else if (SELECT_CASE_var == "OTHERFUEL2") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "OtherFuel2";

                        } else if (SELECT_CASE_var == "STEAM") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Steam";

                        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "DistrictHeating";

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Heater Fuel Type entered=" + DataIPShortCuts::cAlphaArgs(4));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    if (DataIPShortCuts::rNumericArgs(8) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).Efficiency = DataIPShortCuts::rNumericArgs(8);
                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Heater Thermal Efficiency must be greater than zero");
                        ErrorsFound = true;
                    }

                    if (!DataIPShortCuts::cAlphaArgs(5).empty()) {
                        WaterThermalTank(WaterThermalTankNum).PLFCurve = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(5));
                        if (WaterThermalTank(WaterThermalTankNum).PLFCurve == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Part Load Factor curve not found = " + DataIPShortCuts::cAlphaArgs(5));
                            ErrorsFound = true;
                        } else {
                            bool IsValid;
                            ValidatePLFCurve(WaterThermalTank(WaterThermalTankNum).PLFCurve, IsValid);

                            if (!IsValid) {
                                ShowSevereError(
                                    DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                    ":  Part Load Factor curve failed to evaluate to greater than zero for all numbers in the domain of 0 to 1");
                                ErrorsFound = true;
                            }

                            ErrorsFound |= CurveManager::CheckCurveDims(WaterThermalTank(WaterThermalTankNum).PLFCurve, // Curve index
                                                                        {1},                                            // Valid dimensions
                                                                        RoutineName,                                    // Routine name
                                                                        DataIPShortCuts::cCurrentModuleObject,                           // Object Type
                                                                        WaterThermalTank(WaterThermalTankNum).Name,     // Object Name
                                                                        DataIPShortCuts::cAlphaFieldNames(5));                           // Field Name
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = DataIPShortCuts::rNumericArgs(9);

                    // Validate Off-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(6));
                        if (SELECT_CASE_var == "") { // If blank, default to Fuel Type for heater
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = WaterThermalTank(WaterThermalTankNum).FuelType;

                        } else if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";

                        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Gas";

                        } else if (SELECT_CASE_var == "DIESEL") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Diesel";

                        } else if (SELECT_CASE_var == "GASOLINE") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Gasoline";

                        } else if (SELECT_CASE_var == "COAL") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Coal";

                        } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                                   (SELECT_CASE_var == "DISTILLATE OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "FuelOil#1";

                        } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "FuelOil#2";

                        } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                                   (SELECT_CASE_var == "PROPANE GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Propane";

                        } else if (SELECT_CASE_var == "OTHERFUEL1") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "OtherFuel1";

                        } else if (SELECT_CASE_var == "OTHERFUEL2") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "OtherFuel2";

                        } else if (SELECT_CASE_var == "STEAM") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Steam";

                        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "DistrictHeating";

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + DataIPShortCuts::cAlphaArgs(6));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = DataIPShortCuts::rNumericArgs(10);

                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = DataIPShortCuts::rNumericArgs(11);

                    // Validate On-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(7));
                        if (SELECT_CASE_var == "") { // If blank, default to Fuel Type for heater
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = WaterThermalTank(WaterThermalTankNum).FuelType;

                        } else if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";

                        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Gas";

                        } else if (SELECT_CASE_var == "DIESEL") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Diesel";

                        } else if (SELECT_CASE_var == "GASOLINE") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Gasoline";

                        } else if (SELECT_CASE_var == "COAL") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Coal";

                        } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                                   (SELECT_CASE_var == "DISTILLATE OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "FuelOil#1";

                        } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "FuelOil#2";

                        } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                                   (SELECT_CASE_var == "PROPANE GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Propane";

                        } else if (SELECT_CASE_var == "OTHERFUEL1") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "OtherFuel1";

                        } else if (SELECT_CASE_var == "OTHERFUEL2") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "OtherFuel2";

                        } else if (SELECT_CASE_var == "STEAM") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Steam";

                        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "DistrictHeating";

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid On-Cycle Parasitic Fuel Type entered=" + DataIPShortCuts::cAlphaArgs(7));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = DataIPShortCuts::rNumericArgs(12);

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(8));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(9));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                ":  Ambient Temperature Schedule not found = " + DataIPShortCuts::cAlphaArgs(9));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(10), DataHeatBalance::Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                ":  Ambient Temperature Zone not found = " + DataIPShortCuts::cAlphaArgs(10));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode =
                                NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(11),
                                                  ErrorsFound,
                                                  DataIPShortCuts::cCurrentModuleObject,
                                                  DataIPShortCuts::cAlphaArgs(1),
                                                  DataLoopNode::NodeType_Air,
                                                  DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                  1,
                                                  DataLoopNode::ObjectIsNotParent);
                            if (DataIPShortCuts::cAlphaArgs(11) != "") {
                                if (!OutAirNodeManager::CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ShowContinueError("...Referenced Node Name=" + DataIPShortCuts::cAlphaArgs(11));
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + DataIPShortCuts::cAlphaArgs(8));
                            ShowContinueError(" Valid entries are SCHEDULE, ZONE, and OUTDOORS.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff = DataIPShortCuts::rNumericArgs(13);
                    WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone = DataIPShortCuts::rNumericArgs(14);

                    WaterThermalTank(WaterThermalTankNum).OnCycLossCoeff = DataIPShortCuts::rNumericArgs(15);
                    WaterThermalTank(WaterThermalTankNum).OnCycLossFracToZone = DataIPShortCuts::rNumericArgs(16);
                    Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, WaterThermalTank(WaterThermalTankNum).FluidIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = DataIPShortCuts::rNumericArgs(17) * rho;

                    if ((DataIPShortCuts::cAlphaArgs(14).empty()) && (DataIPShortCuts::cAlphaArgs(15).empty())) {
                        if (!DataIPShortCuts::cAlphaArgs(12).empty()) {
                            WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(12));
                            if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule == 0) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Flow Rate Schedule not found = " + DataIPShortCuts::cAlphaArgs(12));
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (!DataIPShortCuts::cAlphaArgs(13).empty()) {
                        WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(13));
                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Cold Water Supply Temperature Schedule not found = " + DataIPShortCuts::cAlphaArgs(13));
                            ErrorsFound = true;
                        }
                    }

                    if (NumNums > 17) {
                        if ((DataIPShortCuts::rNumericArgs(18) > 1) || (DataIPShortCuts::rNumericArgs(18) < 0)) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Use Side Effectiveness is out of bounds (0 to 1)");
                            ErrorsFound = true;
                        }
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = DataIPShortCuts::rNumericArgs(18);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = 1.0; // Default for stand-alone mode
                    }

                    if (NumNums > 18) {
                        if ((DataIPShortCuts::rNumericArgs(19) > 1) || (DataIPShortCuts::rNumericArgs(19) <= 0)) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                            ErrorsFound = true;
                        }
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = DataIPShortCuts::rNumericArgs(19);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = 1.0;
                    }

                    // If no plant nodes are connected, simulate in stand-alone mode.
                    if (DataIPShortCuts::cAlphaArgs(14).empty() && DataIPShortCuts::cAlphaArgs(15).empty() && DataIPShortCuts::cAlphaArgs(16).empty() && DataIPShortCuts::cAlphaArgs(17).empty()) {
                        WaterThermalTank(WaterThermalTankNum).StandAlone = true;
                    }

                    if (!DataIPShortCuts::lNumericFieldBlanks(20)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = DataIPShortCuts::rNumericArgs(20);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    }
                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if (!DataIPShortCuts::lNumericFieldBlanks(21)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = DataIPShortCuts::rNumericArgs(21);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    }
                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if (!DataIPShortCuts::lNumericFieldBlanks(22)) {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = DataIPShortCuts::rNumericArgs(22);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = 1.5;
                    }

                    if ((!DataIPShortCuts::cAlphaArgs(14).empty()) || (!DataIPShortCuts::cAlphaArgs(15).empty())) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(14),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = DataIPShortCuts::cAlphaArgs(14);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(15),
                                                                                                ErrorsFound,
                                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                                DataLoopNode::NodeType_Water,
                                                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = DataIPShortCuts::cAlphaArgs(15);

                        if (DataIPShortCuts::rNumericArgs(17) > 0) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule > 0) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule > 0) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used");
                        }
                    }

                    if ((!DataIPShortCuts::cAlphaArgs(16).empty()) || (!DataIPShortCuts::cAlphaArgs(17).empty())) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(16),
                                                                                                  ErrorsFound,
                                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                                  DataLoopNode::NodeType_Water,
                                                                                                  DataLoopNode::NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = DataIPShortCuts::cAlphaArgs(16);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(17),
                                                                                                   ErrorsFound,
                                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = DataIPShortCuts::cAlphaArgs(17);
                    }

                    if (!DataIPShortCuts::lAlphaFieldBlanks(18)) {
                        {
                            auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(18));
                            if (SELECT_CASE_var == "STORAGETANK") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideStorageTank;
                            } else if (SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideIndirectHeatPrimarySetpoint;
                            } else if (SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideIndirectHeatAltSetpoint;
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Control Mode entered=" + DataIPShortCuts::cAlphaArgs(18));
                                ErrorsFound = true;
                            }
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideIndirectHeatPrimarySetpoint;
                    }

                    if (!DataIPShortCuts::lAlphaFieldBlanks(19)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(19));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  " + DataIPShortCuts::cAlphaFieldNames(19) +
                                            " not found = " + DataIPShortCuts::cAlphaArgs(19));
                            ErrorsFound = true;
                        }
                    }
                    if (NumAlphas > 19) {
                        WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = DataIPShortCuts::cAlphaArgs(20);
                    }

                } // WaterThermalTankNum
            }

            //  =======   Get WATER HEATER:STRATIFIED ==============================================================================
            if (NumWaterHeaterStratified > 0) {
                DataIPShortCuts::cCurrentModuleObject = cStratifiedWHModuleObj; //'WaterHeater:Stratified'

                for (int WaterThermalTankNum = NumWaterHeaterMixed + 1; WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified;
                     ++WaterThermalTankNum) {
                    int NumAlphas;
                    int NumNums;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  WaterThermalTankNum - NumWaterHeaterMixed,
                                                  DataIPShortCuts::cAlphaArgs,
                                                  NumAlphas,
                                                  DataIPShortCuts::rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  DataIPShortCuts::lNumericFieldBlanks,
                                                  DataIPShortCuts::lAlphaFieldBlanks,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = DataIPShortCuts::cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = DataIPShortCuts::cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = DataPlant::TypeOf_WtrHeaterStratified;
                    WaterThermalTank(WaterThermalTankNum).FluidIndex = modWaterIndex;

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;

                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = DataIPShortCuts::cAlphaArgs(2);

                    WaterThermalTank(WaterThermalTankNum).Volume = DataIPShortCuts::rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, WaterThermalTank(WaterThermalTankNum).FluidIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                    WaterThermalTank(WaterThermalTankNum).Height = DataIPShortCuts::rNumericArgs(2);
                    if (WaterThermalTank(WaterThermalTankNum).Height == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized = true;
                    }

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(3));
                        if (SELECT_CASE_var == "VERTICALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeVertCylinder;

                        } else if (SELECT_CASE_var == "HORIZONTALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeHorizCylinder;

                        } else if (SELECT_CASE_var == "OTHER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeOther;
                            if (DataIPShortCuts::rNumericArgs(3) > 0.0) {
                                WaterThermalTank(WaterThermalTankNum).Perimeter = DataIPShortCuts::rNumericArgs(3);
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Tank Shape entered=" + DataIPShortCuts::cAlphaArgs(3));
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeVertCylinder;
                            ErrorsFound = true;
                        }
                    }

                    if (DataIPShortCuts::rNumericArgs(4) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = DataIPShortCuts::rNumericArgs(4);
                    } else {
                        // Default to very large number
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 1.0e9;
                    }

                    // Validate Heater Priority Control
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(4));
                        if (SELECT_CASE_var == "MASTERSLAVE") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = modPriorityMasterSlave;

                        } else if (SELECT_CASE_var == "SIMULTANEOUS") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = modPrioritySimultaneous;

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Heater Priority Control entered=" + DataIPShortCuts::cAlphaArgs(4));
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(5));
                    if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                        ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", missing data.");
                        ShowContinueError("blank field, missing " + DataIPShortCuts::cAlphaFieldNames(5) + " is required");
                        ErrorsFound = true;
                    } else if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": " + DataIPShortCuts::cAlphaFieldNames(5) + " not found = " + DataIPShortCuts::cAlphaArgs(5));
                        ErrorsFound = true;
                    }

                    if (DataIPShortCuts::rNumericArgs(5) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = DataIPShortCuts::rNumericArgs(5);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.0001;
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = DataIPShortCuts::rNumericArgs(6);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).HeaterHeight1 = DataIPShortCuts::rNumericArgs(7);

                    // adjust tank height used in these calculations for testing heater height
                    Real64 tankHeightForTesting = 0.0;
                    if (WaterThermalTank(WaterThermalTankNum).Shape == modTankShapeHorizCylinder) {
                        tankHeightForTesting =
                            2.0 *
                            sqrt((WaterThermalTank(WaterThermalTankNum).Volume / WaterThermalTank(WaterThermalTankNum).Height) / DataGlobals::Pi);
                    } else {
                        tankHeightForTesting = WaterThermalTank(WaterThermalTankNum).Height;
                    }

                    // Test if Heater height is within range
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).HeaterHeight1 > tankHeightForTesting)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Heater 1 is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(7) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(7), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2 = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(6));
                    if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                        ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\", missing data.");
                        ShowContinueError("blank field, missing " + DataIPShortCuts::cAlphaFieldNames(6) + " is required");
                        ErrorsFound = true;
                    } else if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2 == 0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  " + DataIPShortCuts::cAlphaFieldNames(6) + " not found = " + DataIPShortCuts::cAlphaArgs(6));
                        ErrorsFound = true;
                    }

                    if (DataIPShortCuts::rNumericArgs(5) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp2 = DataIPShortCuts::rNumericArgs(8);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp2 = 0.0001;
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity2 = DataIPShortCuts::rNumericArgs(9);
                    WaterThermalTank(WaterThermalTankNum).HeaterHeight2 = DataIPShortCuts::rNumericArgs(10);

                    // Test if Heater height is within range
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).HeaterHeight2 > tankHeightForTesting)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Heater 2 is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 4));
                        ErrorsFound = true;
                    }

                    // Validate Heater Fuel Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(7));
                        if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";

                        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Gas";

                        } else if (SELECT_CASE_var == "DIESEL") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Diesel";

                        } else if (SELECT_CASE_var == "GASOLINE") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Gasoline";

                        } else if (SELECT_CASE_var == "COAL") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Coal";

                        } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                                   (SELECT_CASE_var == "DISTILLATE OIL")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "FuelOil#1";

                        } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "FuelOil#2";

                        } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                                   (SELECT_CASE_var == "PROPANE GAS")) {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Propane";

                        } else if (SELECT_CASE_var == "OTHERFUEL1") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "OtherFuel1";

                        } else if (SELECT_CASE_var == "OTHERFUEL2") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "OtherFuel2";

                        } else if (SELECT_CASE_var == "STEAM") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Steam";

                        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                            WaterThermalTank(WaterThermalTankNum).FuelType = "DistrictHeating";

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Heater Fuel Type entered=" + DataIPShortCuts::cAlphaArgs(7));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    if (DataIPShortCuts::rNumericArgs(11) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).Efficiency = DataIPShortCuts::rNumericArgs(11);
                    } else {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Heater Thermal Efficiency must be greater than zero");
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = DataIPShortCuts::rNumericArgs(12);

                    // Validate Off-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(8));
                        if (SELECT_CASE_var == "") { // If blank, default to Fuel Type for heater
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = WaterThermalTank(WaterThermalTankNum).FuelType;

                        } else if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";

                        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Gas";

                        } else if (SELECT_CASE_var == "DIESEL") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Diesel";

                        } else if (SELECT_CASE_var == "GASOLINE") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Gasoline";

                        } else if (SELECT_CASE_var == "COAL") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Coal";

                        } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                                   (SELECT_CASE_var == "DISTILLATE OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "FuelOil#1";

                        } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "FuelOil#2";

                        } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                                   (SELECT_CASE_var == "PROPANE GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Propane";

                        } else if (SELECT_CASE_var == "OTHERFUEL1") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "OtherFuel1";

                        } else if (SELECT_CASE_var == "OTHERFUEL2") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "OtherFuel2";

                        } else if (SELECT_CASE_var == "STEAM") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Steam";

                        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "DistrictHeating";

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + DataIPShortCuts::cAlphaArgs(8));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = DataIPShortCuts::rNumericArgs(13);
                    WaterThermalTank(WaterThermalTankNum).OffCycParaHeight = DataIPShortCuts::rNumericArgs(14);

                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = DataIPShortCuts::rNumericArgs(15);

                    // Validate On-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(9));
                        if (SELECT_CASE_var == "") { // If blank, default to Fuel Type for heater
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = WaterThermalTank(WaterThermalTankNum).FuelType;

                        } else if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";

                        } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Gas";

                        } else if (SELECT_CASE_var == "DIESEL") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Diesel";

                        } else if (SELECT_CASE_var == "GASOLINE") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Gasoline";

                        } else if (SELECT_CASE_var == "COAL") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Coal";

                        } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                                   (SELECT_CASE_var == "DISTILLATE OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "FuelOil#1";

                        } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "FuelOil#2";

                        } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                                   (SELECT_CASE_var == "PROPANE GAS")) {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Propane";

                        } else if (SELECT_CASE_var == "OTHERFUEL1") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "OtherFuel1";

                        } else if (SELECT_CASE_var == "OTHERFUEL2") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "OtherFuel2";

                        } else if (SELECT_CASE_var == "STEAM") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Steam";

                        } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "DistrictHeating";

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid On-Cycle Parasitic Fuel Type entered=" + DataIPShortCuts::cAlphaArgs(9));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = DataIPShortCuts::rNumericArgs(16);
                    WaterThermalTank(WaterThermalTankNum).OnCycParaHeight = DataIPShortCuts::rNumericArgs(17);

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(10));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(11));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                ":  Ambient Temperature Schedule not found = " + DataIPShortCuts::cAlphaArgs(11));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(12), DataHeatBalance::Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                ":  Ambient Temperature Zone not found = " + DataIPShortCuts::cAlphaArgs(12));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(13),
                                                                                                                ErrorsFound,
                                                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                                                DataLoopNode::NodeType_Air,
                                                                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                                                                1,
                                                                                                                DataLoopNode::ObjectIsNotParent);
                            if (DataIPShortCuts::cAlphaArgs(13) != "") {
                                if (!OutAirNodeManager::CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ShowContinueError("...Referenced Node Name=" + DataIPShortCuts::cAlphaArgs(13));
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + DataIPShortCuts::cAlphaArgs(10));
                            ShowContinueError(" Valid entries are Schedule, Zone, and Outdoors.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SkinLossCoeff = DataIPShortCuts::rNumericArgs(18);
                    WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone = DataIPShortCuts::rNumericArgs(19);
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossCoeff = DataIPShortCuts::rNumericArgs(20);
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossFracToZone = DataIPShortCuts::rNumericArgs(21);

                    // this is temporary until we know fluid type
                    rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, WaterThermalTank(WaterThermalTankNum).FluidIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = DataIPShortCuts::rNumericArgs(22) * rho;

                    if ((DataIPShortCuts::cAlphaArgs(16).empty()) && (DataIPShortCuts::cAlphaArgs(17).empty())) {
                        if (!DataIPShortCuts::cAlphaArgs(14).empty()) {
                            WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(14));
                            if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule == 0) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Flow Rate Schedule not found = " + DataIPShortCuts::cAlphaArgs(14));
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (!DataIPShortCuts::cAlphaArgs(15).empty()) {
                        WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(15));
                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Cold Water Supply Temperature Schedule not found = " + DataIPShortCuts::cAlphaArgs(15));
                            ErrorsFound = true;
                        }
                    }

                    if (NumNums > 22) {
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = DataIPShortCuts::rNumericArgs(23);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = 1.0; // Default for stand-alone mode
                    }

                    if (NumNums > 23) {
                        WaterThermalTank(WaterThermalTankNum).UseInletHeight = DataIPShortCuts::rNumericArgs(24);
                    } else {
                        // Defaults to bottom of tank
                        WaterThermalTank(WaterThermalTankNum).UseInletHeight = 0.0;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).UseInletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Use inlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(24) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(24), 4));
                        ErrorsFound = true;
                    }

                    if ((NumNums > 24) && (DataIPShortCuts::rNumericArgs(25) != DataGlobals::AutoCalculate)) {
                        WaterThermalTank(WaterThermalTankNum).UseOutletHeight = DataIPShortCuts::rNumericArgs(25);
                    } else {
                        // Defaults to top of tank
                        WaterThermalTank(WaterThermalTankNum).UseOutletHeight = WaterThermalTank(WaterThermalTankNum).Height;
                    }
                    if (WaterThermalTank(WaterThermalTankNum).UseOutletHeight == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).UseOutletHeightWasAutoSized = true;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).UseOutletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Use outlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(25) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(25), 4));
                        ErrorsFound = true;
                    }

                    if (NumNums > 25) {
                        if ((DataIPShortCuts::rNumericArgs(26) > 1) || (DataIPShortCuts::rNumericArgs(26) <= 0)) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                            ErrorsFound = true;
                        }
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = DataIPShortCuts::rNumericArgs(26);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = 1.0;
                    }

                    if ((NumNums > 26) && (DataIPShortCuts::rNumericArgs(27) != DataGlobals::AutoCalculate)) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletHeight = DataIPShortCuts::rNumericArgs(27);
                    } else {
                        // Defaults to top of tank
                        WaterThermalTank(WaterThermalTankNum).SourceInletHeight = WaterThermalTank(WaterThermalTankNum).Height;
                    }
                    if (WaterThermalTank(WaterThermalTankNum).SourceInletHeight == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletHeightWasAutoSized = true;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).SourceInletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Source inlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(27) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(27), 4));
                        ErrorsFound = true;
                    }

                    if ((NumNums > 27) && (DataIPShortCuts::rNumericArgs(28) != DataGlobals::AutoCalculate)) {
                        WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = DataIPShortCuts::rNumericArgs(28);
                    } else {
                        // Defaults to bottom of tank
                        WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = 0.0;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).SourceOutletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Source outlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(28) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(28), 4));
                        ErrorsFound = true;
                    }

                    // If no plant nodes are connected, simulate in stand-alone mode.
                    if (DataIPShortCuts::cAlphaArgs(16).empty() && DataIPShortCuts::cAlphaArgs(17).empty() && DataIPShortCuts::cAlphaArgs(18).empty() && DataIPShortCuts::cAlphaArgs(19).empty())
                        WaterThermalTank(WaterThermalTankNum).StandAlone = true;

                    if (!DataIPShortCuts::lNumericFieldBlanks(29)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = DataIPShortCuts::rNumericArgs(29);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    }

                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if (!DataIPShortCuts::lNumericFieldBlanks(30)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = DataIPShortCuts::rNumericArgs(30);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    }

                    if (NumNums > 30) {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = DataIPShortCuts::rNumericArgs(31);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = 1.5;
                    }

                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if ((!DataIPShortCuts::cAlphaArgs(16).empty()) || (!DataIPShortCuts::cAlphaArgs(17).empty())) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(16),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = DataIPShortCuts::cAlphaArgs(16);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(17),
                                                                                                ErrorsFound,
                                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                                DataLoopNode::NodeType_Water,
                                                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = DataIPShortCuts::cAlphaArgs(17);

                        if (DataIPShortCuts::rNumericArgs(22) > 0) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule > 0) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule > 0) {
                            ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used");
                        }
                    }

                    if ((!DataIPShortCuts::cAlphaArgs(18).empty()) || (!DataIPShortCuts::cAlphaArgs(19).empty())) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(18),
                                                                                                  ErrorsFound,
                                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                                  DataLoopNode::NodeType_Water,
                                                                                                  DataLoopNode::NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = DataIPShortCuts::cAlphaArgs(18);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(19),
                                                                                                   ErrorsFound,
                                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = DataIPShortCuts::cAlphaArgs(19);
                    }

                    // Validate inlet mode
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(20));
                        if (SELECT_CASE_var == "FIXED") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = modInletModeFixed;

                        } else if (SELECT_CASE_var == "SEEKING") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = modInletModeSeeking;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).Nodes = DataIPShortCuts::rNumericArgs(32);
                    WaterThermalTank(WaterThermalTankNum).AdditionalCond = DataIPShortCuts::rNumericArgs(33);

                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff.allocate(WaterThermalTank(WaterThermalTankNum).Nodes);
                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff = 0.0;
                    for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                        if (NumNums > 32 + NodeNum) {
                            WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff(NodeNum) = DataIPShortCuts::rNumericArgs(33 + NodeNum);
                        } else {
                            break;
                        }
                    }

                    if (NumNums > 33 + WaterThermalTank(WaterThermalTankNum).Nodes) {
                        ShowWarningError(
                            DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                            ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used");
                    }

                    SetupStratifiedNodes(WaterThermalTankNum);

                    if (!DataIPShortCuts::lAlphaFieldBlanks(21)) {
                        {
                            auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(21));
                            if (SELECT_CASE_var == "STORAGETANK") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideStorageTank;
                            } else if (SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideIndirectHeatPrimarySetpoint;
                            } else if (SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideIndirectHeatAltSetpoint;
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Control Mode entered=" + DataIPShortCuts::cAlphaArgs(21));
                                ErrorsFound = true;
                            }
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = modSourceSideIndirectHeatPrimarySetpoint;
                    }

                    if (!DataIPShortCuts::lAlphaFieldBlanks(22)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(22));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum == 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  " + DataIPShortCuts::cAlphaFieldNames(22) +
                                            " not found = " + DataIPShortCuts::cAlphaArgs(22));
                            ErrorsFound = true;
                        }
                    }

                } // WaterThermalTankNum
            }

            //  =======   Get Chilled Water :MIXED ===================================================================================
            if (NumChilledWaterMixed > 0) {
                DataIPShortCuts::cCurrentModuleObject = cMixedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Mixed'
                for (int WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + 1;
                     WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed;
                     ++WaterThermalTankNum) {
                    int NumAlphas;
                    int NumNums;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  WaterThermalTankNum - (NumWaterHeaterMixed + NumWaterHeaterStratified),
                                                  DataIPShortCuts::cAlphaArgs,
                                                  NumAlphas,
                                                  DataIPShortCuts::rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  DataIPShortCuts::lNumericFieldBlanks,
                                                  DataIPShortCuts::lAlphaFieldBlanks,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = DataIPShortCuts::cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = DataIPShortCuts::cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = DataPlant::TypeOf_ChilledWaterTankMixed;
                    WaterThermalTank(WaterThermalTankNum).FluidIndex = modWaterIndex;
                    WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank = true;
                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = "Chilled Water Storage";

                    WaterThermalTank(WaterThermalTankNum).Volume = DataIPShortCuts::rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                        // Set volume to a really small number to continue simulation
                        WaterThermalTank(WaterThermalTankNum).Volume = 0.000001; // = 1 cm3
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
                    if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + DataIPShortCuts::cAlphaArgs(2));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));

                        ErrorsFound = true;
                    }

                    if (DataIPShortCuts::rNumericArgs(2) > 0.0001) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = DataIPShortCuts::rNumericArgs(2);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.5;
                    }

                    if (DataIPShortCuts::rNumericArgs(3) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = DataIPShortCuts::rNumericArgs(3);
                    } else {
                        // default to just above freezing
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 1.0;
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = DataIPShortCuts::rNumericArgs(4);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).MinCapacity = 0.0;
                    WaterThermalTank(WaterThermalTankNum).ControlType = modControlTypeCycle;

                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMin = 0.0;
                    WaterThermalTank(WaterThermalTankNum).IgnitionDelay = 0.0;
                    WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";
                    WaterThermalTank(WaterThermalTankNum).Efficiency = 1.0;
                    WaterThermalTank(WaterThermalTankNum).PLFCurve = 0;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";
                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";
                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = 0.0;

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(3));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(4));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + DataIPShortCuts::cAlphaArgs(4));
                                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError("Schedule was not found.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(5), DataHeatBalance::Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + DataIPShortCuts::cAlphaArgs(5));
                                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError("Zone was not found.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode =
                                NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                  ErrorsFound,
                                                  DataIPShortCuts::cCurrentModuleObject,
                                                  DataIPShortCuts::cAlphaArgs(1),
                                                  DataLoopNode::NodeType_Air,
                                                  DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                  1,
                                                  DataLoopNode::ObjectIsNotParent);
                            if (!DataIPShortCuts::lAlphaFieldBlanks(6)) {
                                if (!OutAirNodeManager::CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
                                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                    ShowContinueError("Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + DataIPShortCuts::cAlphaArgs(3));
                            ShowContinueError(" Valid entries are Schedule, Zone, and Outdoors.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff = DataIPShortCuts::rNumericArgs(5);
                    WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone = 1.0;

                    WaterThermalTank(WaterThermalTankNum).OnCycLossCoeff = DataIPShortCuts::rNumericArgs(5);
                    WaterThermalTank(WaterThermalTankNum).OnCycLossFracToZone = 1.0;

                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = 0.0;
                    WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = 0;
                    WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = 0;

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;

                    if ((DataIPShortCuts::rNumericArgs(6) > 1) || (DataIPShortCuts::rNumericArgs(6) < 0)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Use Side Effectiveness is out of bounds (0 to 1)");
                        ErrorsFound = true;
                    }
                    WaterThermalTank(WaterThermalTankNum).UseEffectiveness = DataIPShortCuts::rNumericArgs(6);

                    if ((DataIPShortCuts::rNumericArgs(8) > 1) || (DataIPShortCuts::rNumericArgs(8) <= 0)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                        ErrorsFound = true;
                    }
                    WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = DataIPShortCuts::rNumericArgs(8);

                    if (DataIPShortCuts::lNumericFieldBlanks(7)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = DataIPShortCuts::rNumericArgs(7);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if (DataIPShortCuts::lAlphaFieldBlanks(9)) {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(9));
                        if (WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(9) + " = " + DataIPShortCuts::cAlphaArgs(9));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if (DataIPShortCuts::lNumericFieldBlanks(9)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = DataIPShortCuts::rNumericArgs(9);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    if (DataIPShortCuts::lAlphaFieldBlanks(12)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(12));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(12) + " = " + DataIPShortCuts::cAlphaArgs(12));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }
                    if (DataIPShortCuts::lNumericFieldBlanks(10)) {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = 4.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = DataIPShortCuts::rNumericArgs(10);
                    }

                    if ((!DataIPShortCuts::lAlphaFieldBlanks(7)) || (!DataIPShortCuts::lAlphaFieldBlanks(8))) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = DataIPShortCuts::cAlphaArgs(7);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                                ErrorsFound,
                                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                                DataLoopNode::NodeType_Water,
                                                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = DataIPShortCuts::cAlphaArgs(8);
                    }

                    if ((!DataIPShortCuts::lAlphaFieldBlanks(10)) || (!DataIPShortCuts::lAlphaFieldBlanks(11))) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(10),
                                                                                                  ErrorsFound,
                                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                                  DataLoopNode::NodeType_Water,
                                                                                                  DataLoopNode::NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = DataIPShortCuts::cAlphaArgs(10);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(11),
                                                                                                   ErrorsFound,
                                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = DataIPShortCuts::cAlphaArgs(11);
                    }

                    if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DataPlant::DemandSide &&
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode != 0) {
                        PlantUtilities::RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                    }

                } // WaterThermalTankNum
            }

            // end chilled water mixed storage

            //  =======   Get 'ThermalStorage:ChilledWater:Stratified' =======================================================
            if (NumChilledWaterStratified > 0) {
                DataIPShortCuts::cCurrentModuleObject = cStratifiedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Stratified'

                for (int WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + 1;
                     WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + NumChilledWaterStratified;
                     ++WaterThermalTankNum) {
                    int NumNums;
                    int NumAlphas;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                                  WaterThermalTankNum - (NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed),
                                                  DataIPShortCuts::cAlphaArgs,
                                                  NumAlphas,
                                                  DataIPShortCuts::rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  DataIPShortCuts::lNumericFieldBlanks,
                                                  DataIPShortCuts::lAlphaFieldBlanks,
                                                  DataIPShortCuts::cAlphaFieldNames,
                                                  DataIPShortCuts::cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = DataIPShortCuts::cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = DataIPShortCuts::cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = DataPlant::TypeOf_ChilledWaterTankStratified;
                    WaterThermalTank(WaterThermalTankNum).FluidIndex = modWaterIndex;
                    WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank = true;
                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = "Chilled Water Storage";

                    WaterThermalTank(WaterThermalTankNum).Volume = DataIPShortCuts::rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, WaterThermalTank(WaterThermalTankNum).FluidIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                    WaterThermalTank(WaterThermalTankNum).Height = DataIPShortCuts::rNumericArgs(2);
                    if (WaterThermalTank(WaterThermalTankNum).Height == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized = true;
                    }

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(2));
                        if (SELECT_CASE_var == "VERTICALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeVertCylinder;

                        } else if (SELECT_CASE_var == "HORIZONTALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeHorizCylinder;

                        } else if (SELECT_CASE_var == "OTHER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeOther;
                            if (DataIPShortCuts::rNumericArgs(3) > 0.0) {
                                WaterThermalTank(WaterThermalTankNum).Perimeter = DataIPShortCuts::rNumericArgs(3);
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                                ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Invalid Tank Shape entered=" + DataIPShortCuts::cAlphaArgs(2));
                            WaterThermalTank(WaterThermalTankNum).Shape = modTankShapeVertCylinder;
                            ErrorsFound = true;
                        }
                    }

                    if (DataIPShortCuts::rNumericArgs(6) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = DataIPShortCuts::rNumericArgs(6);
                    } else {
                        // default to just above freezing
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 1.0;
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(3));
                    if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + DataIPShortCuts::cAlphaArgs(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                        ShowContinueError("Schedule was not found.");
                        ErrorsFound = true;
                    }

                    if (DataIPShortCuts::rNumericArgs(4) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = DataIPShortCuts::rNumericArgs(4);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.0001;
                    }

                    WaterThermalTank(WaterThermalTankNum).HeaterHeight1 = DataIPShortCuts::rNumericArgs(5);
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = DataIPShortCuts::rNumericArgs(7);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == DataSizing::AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).Efficiency = 1.0;
                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2 = 0;
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity2 = 0.0;
                    WaterThermalTank(WaterThermalTankNum).HeaterHeight2 = 0.0;
                    WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";

                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";
                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaHeight = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";
                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaHeight = 0.0;

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(4));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(5));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " = " + DataIPShortCuts::cAlphaArgs(5));
                                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError("Schedule was not found.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(6), DataHeatBalance::Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
                                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError("Zone was not found.");
                                ErrorsFound = true;
                            }
                            WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone = 1.0;

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = modAmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                                                ErrorsFound,
                                                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                                                DataLoopNode::NodeType_Air,
                                                                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                                                                1,
                                                                                                                DataLoopNode::ObjectIsNotParent);
                            if (!DataIPShortCuts::lAlphaFieldBlanks(7)) {
                                if (!OutAirNodeManager::CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(7) + " = " + DataIPShortCuts::cAlphaArgs(7));
                                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                    ShowContinueError("Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + DataIPShortCuts::cAlphaArgs(4));
                            ShowContinueError("  Valid entries are Schedule, Zone, and Outdoors.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SkinLossCoeff = DataIPShortCuts::rNumericArgs(8);
                    WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone = 1.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossCoeff = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossFracToZone = 0.0;

                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = 0.0;
                    WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = 0;
                    WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = 0;
                    WaterThermalTank(WaterThermalTankNum).UseEffectiveness = DataIPShortCuts::rNumericArgs(9);
                    WaterThermalTank(WaterThermalTankNum).UseInletHeight = DataIPShortCuts::rNumericArgs(10);

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;

                    if (DataIPShortCuts::rNumericArgs(10) == DataGlobals::AutoCalculate) {
                        WaterThermalTank(WaterThermalTankNum).UseInletHeight = WaterThermalTank(WaterThermalTankNum).Height; // top of tank
                    }
                    if (WaterThermalTank(WaterThermalTankNum).UseInletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Use inlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).UseOutletHeight = DataIPShortCuts::rNumericArgs(11);
                    if (WaterThermalTank(WaterThermalTankNum).UseOutletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Use outlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(11) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(11), 4));
                        ErrorsFound = true;
                    }

                    if ((DataIPShortCuts::rNumericArgs(13) > 1) || (DataIPShortCuts::rNumericArgs(13) <= 0)) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                        ErrorsFound = true;
                    }
                    WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = DataIPShortCuts::rNumericArgs(13);

                    WaterThermalTank(WaterThermalTankNum).SourceInletHeight = DataIPShortCuts::rNumericArgs(14);
                    if (WaterThermalTank(WaterThermalTankNum).SourceInletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Source inlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(14) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(14), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = DataIPShortCuts::rNumericArgs(15);
                    if (DataIPShortCuts::rNumericArgs(15) == DataGlobals::AutoCalculate) {
                        WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = WaterThermalTank(WaterThermalTankNum).Height; // top of tank
                    }
                    if (WaterThermalTank(WaterThermalTankNum).SourceOutletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) + ": Source outlet is located higher than overall tank height.");
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(2) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 4));
                        ShowContinueError(DataIPShortCuts::cNumericFieldNames(15) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(15), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).StandAlone = false;

                    if (DataIPShortCuts::lNumericFieldBlanks(12)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = DataIPShortCuts::rNumericArgs(12);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if (DataIPShortCuts::lNumericFieldBlanks(16)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = DataIPShortCuts::rNumericArgs(16);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == DataSizing::AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = DataIPShortCuts::rNumericArgs(17);

                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DataPlant::DemandSupply_No;

                    if ((!DataIPShortCuts::lAlphaFieldBlanks(8)) || (!DataIPShortCuts::lAlphaFieldBlanks(9))) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = DataIPShortCuts::cAlphaArgs(8);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(9),
                                                                                                ErrorsFound,
                                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                                DataLoopNode::NodeType_Water,
                                                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = DataIPShortCuts::cAlphaArgs(9);
                    }

                    if ((!DataIPShortCuts::lAlphaFieldBlanks(11)) || (!DataIPShortCuts::lAlphaFieldBlanks(12))) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(11),
                                                                                                  ErrorsFound,
                                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                                  DataLoopNode::NodeType_Water,
                                                                                                  DataLoopNode::NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = DataIPShortCuts::cAlphaArgs(11);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(12),
                                                                                                   ErrorsFound,
                                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   DataLoopNode::ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = DataIPShortCuts::cAlphaArgs(12);
                    }

                    if (DataIPShortCuts::lAlphaFieldBlanks(10)) {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(10));
                        if (WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(10) + " = " + DataIPShortCuts::cAlphaArgs(10));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }

                    if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DataPlant::DemandSide &&
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode != 0) {
                        PlantUtilities::RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                    }

                    if (DataIPShortCuts::lAlphaFieldBlanks(13)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = DataGlobals::ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(13));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(13) + " = " + DataIPShortCuts::cAlphaArgs(13));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }

                    // Validate inlet mode
                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(14));
                        if (SELECT_CASE_var == "FIXED") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = modInletModeFixed;

                        } else if (SELECT_CASE_var == "SEEKING") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = modInletModeSeeking;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).Nodes = DataIPShortCuts::rNumericArgs(18);
                    WaterThermalTank(WaterThermalTankNum).AdditionalCond = DataIPShortCuts::rNumericArgs(19);

                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff.allocate(WaterThermalTank(WaterThermalTankNum).Nodes);
                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff = 0.0;
                    for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                        if (NumNums > 19 + NodeNum) {
                            WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff(NodeNum) = DataIPShortCuts::rNumericArgs(19 + NodeNum);
                        } else {
                            break;
                        }
                    }

                    if (NumNums > 19 + WaterThermalTank(WaterThermalTankNum).Nodes) {
                        ShowWarningError(
                            DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                            ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used");
                    }

                    SetupStratifiedNodes(WaterThermalTankNum);

                } // WaterThermalTankNum
            }
            //   end stratified chilled water storage

            //  =======   Check Water Heaters ======================================================================================

            //   Loop through all desuperheating coils and then search all water heaters for the tank connected to the desuperheating coil
            if (NumWaterHeaterDesuperheater > 0) {
                DataIPShortCuts::cCurrentModuleObject = "Coil:WaterHeating:Desuperheater";
                for (int DesuperheaterNum = 1; DesuperheaterNum <= NumWaterHeaterDesuperheater; ++DesuperheaterNum) {
                    for (int CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= NumWaterThermalTank; ++CheckWaterHeaterNum) {
                        if (!UtilityRoutines::SameString(WaterHeaterDesuperheater(DesuperheaterNum).TankName,
                                                         WaterThermalTank(CheckWaterHeaterNum).Name) ||
                            !UtilityRoutines::SameString(WaterHeaterDesuperheater(DesuperheaterNum).TankType,
                                                         WaterThermalTank(CheckWaterHeaterNum).Type))
                            continue;
                        WaterThermalTank(CheckWaterHeaterNum).DesuperheaterNum = DesuperheaterNum;
                        WaterHeaterDesuperheater(DesuperheaterNum).WaterHeaterTankNum = CheckWaterHeaterNum;
                        WaterHeaterDesuperheater(DesuperheaterNum).TankTypeNum = WaterThermalTank(CheckWaterHeaterNum).TypeNum;
                        WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity = WaterThermalTank(CheckWaterHeaterNum).MaxCapacity;
                        if (WaterThermalTank(CheckWaterHeaterNum).UseInletNode == 0 && WaterThermalTank(CheckWaterHeaterNum).UseOutletNode == 0)
                            WaterHeaterDesuperheater(DesuperheaterNum).StandAlone = true;

                        //         verify Desuperheater/tank source node connections
                        if (WaterHeaterDesuperheater(DesuperheaterNum).WaterInletNode != WaterThermalTank(CheckWaterHeaterNum).SourceOutletNode) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
                            ShowContinueError("Desuperheater inlet node name does not match thermal tank source outlet node name.");
                            ShowContinueError(
                                "Desuperheater water inlet and outlet node names = " + CoilSaveNodeNames(DesuperheaterNum).InletNodeName1 + " and " +
                                CoilSaveNodeNames(DesuperheaterNum).OutletNodeName1);
                            ShowContinueError(
                                "Thermal tank source side inlet and outlet node names      = " + WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName2 +
                                " and " + WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName2);
                            ErrorsFound = true;
                        }

                        if (WaterHeaterDesuperheater(DesuperheaterNum).WaterOutletNode != WaterThermalTank(CheckWaterHeaterNum).SourceInletNode) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
                            ShowContinueError("Desuperheater water outlet node name does not match thermal tank source inlet node name.");
                            ShowContinueError(
                                "Desuperheater water inlet and outlet node names = " + CoilSaveNodeNames(DesuperheaterNum).InletNodeName1 + " and " +
                                CoilSaveNodeNames(DesuperheaterNum).OutletNodeName1);
                            ShowContinueError(
                                "Thermal tank source side inlet and outlet node names      = " + WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName2 +
                                " and " + WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName2);
                            ErrorsFound = true;
                        }

                    } // DO CheckWaterHeaterNum = 1, NumWaterHeater

                    if (WaterHeaterDesuperheater(DesuperheaterNum).WaterHeaterTankNum == 0) {
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
                        ShowContinueError(" Water heater tank = " + WaterHeaterDesuperheater(DesuperheaterNum).TankName + " not found.");
                        ErrorsFound = true;
                    }

                } // DO DesuperheaterNum = 1, NumWaterHeaterDesuperheater
            }

            // Loop through HPWH's and then search all water heaters for the tank connected to the HPWH
            if (NumHeatPumpWaterHeater > 0) {

                int const NumPumpedCondenser =
                    inputProcessor->getNumObjectsFound(cHPWHPumpedCondenser); // number of WaterHeater:HeatPump:PumpedCondenser objects
                for (int HPWaterHeaterNum = 1; HPWaterHeaterNum <= NumHeatPumpWaterHeater; ++HPWaterHeaterNum) {

                    // Create reference to current HPWH object in array.
                    HeatPumpWaterHeaterData &HPWH = HPWaterHeater(HPWaterHeaterNum);
                    if (HPWaterHeaterNum <= NumPumpedCondenser) {
                        // Pumped Condenser
                        DataIPShortCuts::cCurrentModuleObject = cHPWHPumpedCondenser;
                    } else {
                        // Wrapped Condenser
                        DataIPShortCuts::cCurrentModuleObject = cHPWHWrappedCondenser;
                    }

                    // find the tank associated with the heat pump water heater and change its %TYPE to HEAT PUMP:WATER HEATER
                    for (int CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= NumWaterThermalTank; ++CheckWaterHeaterNum) {

                        // Create reference to the tank
                        WaterThermalTankData &Tank = WaterThermalTank(CheckWaterHeaterNum);

                        if (!(UtilityRoutines::SameString(HPWH.TankName, Tank.Name) && UtilityRoutines::SameString(HPWH.TankType, Tank.Type)))
                            continue;

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
                            // use typenum parameter to simulate heatpumpwaterheater in standard ratings procedure
                            // WaterThermalTank.TypeNum = HeatPumpWaterHeater for a HPWH
                            // Tank.TypeNum = HPWH.TypeNum
                        } else {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Invalid water heater tank type = " + Tank.Type);
                            ErrorsFound = true;
                        }

                        // Set up comp set for condenser water side nodes (reverse inlet/outlet for water heater)
                        WaterHeaterSaveNodes const &HPWHSaveNode = HPWHSaveNodeNames(HPWaterHeaterNum);
                        if (HPWH.bIsIHP) {
                            BranchNodeConnections::SetUpCompSets(HPWH.Type,
                                          HPWH.Name,
                                          HPWH.DXCoilType,
                                          HPWH.DXCoilName + " Water Coil",
                                          HPWHSaveNode.InletNodeName1,
                                          HPWHSaveNode.OutletNodeName1,
                                          "HPWH To Coil");
                        } else {
                            BranchNodeConnections::SetUpCompSets(HPWH.Type,
                                          HPWH.Name,
                                          HPWH.DXCoilType,
                                          HPWH.DXCoilName,
                                          HPWHSaveNode.InletNodeName1,
                                          HPWHSaveNode.OutletNodeName1,
                                          "HPWH To Coil");
                        }
                        BranchNodeConnections::SetUpCompSets(HPWH.Type,
                                      HPWH.Name,
                                      HPWH.TankType,
                                      HPWH.TankName,
                                      HPWHSaveNode.OutletNodeName1,
                                      HPWHSaveNode.InletNodeName1,
                                      "HPWH To Tank");

                        // do not allow modulating control for HPWH's (i.e. modulating control usually used for tankless WH's)
                        if (Tank.ControlType == modControlTypeModulate) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Heater Control Type for " + Tank.Type + " = " + Tank.Name + " must be CYCLE.");
                            ErrorsFound = true;
                        }

                        Tank.HeatPumpNum = HPWaterHeaterNum;
                        HPWH.WaterHeaterTankNum = CheckWaterHeaterNum;
                        HPWH.FoundTank = true;

                        if (Tank.DesuperheaterNum > 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name +
                                            "and Coil:WaterHeating:Desuperheater = " + WaterHeaterDesuperheater(CheckWaterHeaterNum).Name +
                                            ":  cannot be connected to the same water heater tank = " + Tank.Name);
                        }

                        // check that water heater source side effectiveness is greater than 0
                        if (Tank.SourceEffectiveness <= 0.0) {
                            ShowSevereError(
                                DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name +
                                ":  Invalid source side effectiveness for heat pump water heater = " + General::TrimSigDigits(Tank.SourceEffectiveness, 3));
                            ShowContinueError(" water heater source effectiveness will default to 1.0 and simulation continues.");
                            Tank.SourceEffectiveness = 1.0;
                        }

                        // Set up the source side nodes for wrapped condensers
                        if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                            if (Tank.SourceInletNode > 0 || Tank.SourceOutletNode > 0) {
                                ShowSevereError(Tank.Type + " = " + Tank.Name + " has a source inlet or outlet node specified,");
                                ShowContinueError("but it is attached to " + HPWH.Type + " = " + HPWH.Name +
                                                  ", which doesn't permit source side connections.");
                                ShowContinueError("Please leave the source side inlet and outlet fields blank.");
                                ErrorsFound = true;
                            } else {
                                WaterHeaterSaveNodes &HPWHNodeNames = HPWHSaveNodeNames(HPWaterHeaterNum);
                                WaterHeaterSaveNodes &TankNodenames = WHSaveNodeNames(CheckWaterHeaterNum);
                                Tank.SourceInletNode = NodeInputManager::GetOnlySingleNode(HPWHNodeNames.OutletNodeName1,
                                                                         ErrorsFound,
                                                                         Tank.Type,
                                                                         Tank.Name,
                                                                         DataLoopNode::NodeType_Water,
                                                                         DataLoopNode::NodeConnectionType_Inlet,
                                                                         2,
                                                                         DataLoopNode::ObjectIsNotParent);
                                TankNodenames.InletNodeName2 = HPWHNodeNames.OutletNodeName1;
                                Tank.SourceOutletNode = NodeInputManager::GetOnlySingleNode(HPWHNodeNames.InletNodeName1,
                                                                          ErrorsFound,
                                                                          Tank.Type,
                                                                          Tank.Name,
                                                                          DataLoopNode::NodeType_Water,
                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                          2,
                                                                          DataLoopNode::ObjectIsNotParent);
                                TankNodenames.OutletNodeName2 = HPWHNodeNames.InletNodeName1;
                            }

                            // Mark the tank as not stand alone because it is connected now.
                            Tank.StandAlone = false;
                        }

                        // Set HPWH structure variable StandAlone to TRUE if use nodes are not connected
                        if (Tank.UseInletNode == 0 && Tank.UseOutletNode == 0) HPWH.StandAlone = true;

                        if (HPWH.WHUseInletNode != Tank.UseInletNode || HPWH.WHUseOutletNode != Tank.UseOutletNode) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Heat pump water heater tank use side inlet and outlet node names must match the use side inlet and "
                                              "outlet node names for water heater tank = " +
                                              HPWH.TankType + ": " + HPWH.TankName);
                            ShowContinueError("Heat pump water heater use side inlet and outlet node names = " +
                                              HPWHSaveNodeNames(HPWaterHeaterNum).InletNodeName2 + " and " +
                                              HPWHSaveNodeNames(HPWaterHeaterNum).OutletNodeName2);
                            ShowContinueError("Water heater tank use side inlet and outlet node names      = " +
                                              WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName1 + " and " +
                                              WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName1);
                            ErrorsFound = true;
                        } else {
                            if (!HPWH.StandAlone) {
                                //              removed next to avoid duplicate comp set issue, (should change so that Branch has tank object)
                                //              CALL BranchNodeConnections::SetUpCompSets(HPWH%Type, HPWH%Name, &
                                //                     HPWH%TankType, &
                                //                     HPWH%TankName, &
                                //                     WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName1,WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName1)
                                BranchNodeConnections::TestCompSet(HPWH.Type,
                                            HPWH.Name,
                                            WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName1,
                                            WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName1,
                                            "Water Nodes");
                            }
                        }

                        // verify HP/tank source node connections
                        if (HPWH.CondWaterInletNode != Tank.SourceOutletNode) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Heat Pump condenser water inlet node name does not match water heater tank source outlet node name.");
                            ShowContinueError(
                                "Heat pump condenser water inlet and outlet node names = " + HPWHSaveNodeNames(HPWaterHeaterNum).InletNodeName1 +
                                " and " + HPWHSaveNodeNames(HPWaterHeaterNum).OutletNodeName1);
                            ShowContinueError("Water heater tank source side inlet and outlet node names      = " +
                                              WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName2 + " and " +
                                              WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName2);
                            ErrorsFound = true;
                        }

                        if (HPWH.CondWaterOutletNode != Tank.SourceInletNode) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Heat Pump condenser water outlet node name does not match water heater tank source inlet node name.");
                            ShowContinueError(
                                "Heat pump condenser water inlet and outlet node names = " + HPWHSaveNodeNames(HPWaterHeaterNum).InletNodeName1 +
                                " and " + HPWHSaveNodeNames(HPWaterHeaterNum).OutletNodeName1);
                            ShowContinueError("Water heater tank source side inlet and outlet node names      = " +
                                              WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName2 + " and " +
                                              WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName2);
                            ErrorsFound = true;
                        }

                        // verify wrapped condenser location
                        if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                            // make sure the top of the condenser is not above the tank height.
                            if (HPWH.WrappedCondenserTopLocation > Tank.Height) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("The height of the top of the wrapped condenser is greater than the height of the tank.");
                                ErrorsFound = true;
                            }
                        }

                        // Verify tank name is in a zone equipment list if HPWH Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
                        if (HPWH.InletAirConfiguration == modAmbientTempZone || HPWH.InletAirConfiguration == modAmbientTempZoneAndOA) {
                            if (allocated(DataZoneEquipment::ZoneEquipConfig) && allocated(DataZoneEquipment::ZoneEquipList)) {
                                bool FoundTankInList = false;
                                bool TankNotLowestPriority = false;
                                for (int ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= DataGlobals::NumOfZones; ++ZoneEquipConfigNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != HPWH.AmbientTempZone) continue;
                                    if (ZoneEquipConfigNum <= DataGlobals::NumOfZones) {
                                        for (int ZoneEquipListNum = 1; ZoneEquipListNum <= DataGlobals::NumOfZones; ++ZoneEquipListNum) {
                                            if (DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).EquipListName != DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).Name) continue;
                                            int TankCoolingPriority = 0;
                                            int TankHeatingPriority = 0;
                                            if (ZoneEquipConfigNum <= DataGlobals::NumOfZones) {
                                                for (int EquipmentTypeNum = 1; EquipmentTypeNum <= DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).NumOfEquipTypes;
                                                     ++EquipmentTypeNum) {
                                                    if (DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).EquipName(EquipmentTypeNum) != HPWH.Name) continue;
                                                    FoundTankInList = true;
                                                    TankCoolingPriority = DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).CoolingPriority(EquipmentTypeNum);
                                                    TankHeatingPriority = DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).HeatingPriority(EquipmentTypeNum);
                                                    break;
                                                } // EquipmentTypeNum
                                                if (!FoundTankInList) {
                                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                                                    ShowContinueError("Heat pump water heater type and name must be listed in the correct "
                                                                      "ZoneHVAC:EquipmentList object when Inlet Air Configuration is equal to "
                                                                      "ZoneAirOnly or ZoneAndOutdoorAir.");
                                                    ErrorsFound = true;
                                                }
                                                //                     check that tank has lower priority than all other non-HPWH objects in Zone
                                                //                     Equipment List
                                                for (int EquipmentTypeNum = 1; EquipmentTypeNum <= DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).NumOfEquipTypes;
                                                     ++EquipmentTypeNum) {
                                                    if (UtilityRoutines::SameString(DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).EquipType(EquipmentTypeNum),
                                                                                    DataIPShortCuts::cCurrentModuleObject))
                                                        continue;
                                                    if (TankCoolingPriority > DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).CoolingPriority(EquipmentTypeNum) ||
                                                        TankHeatingPriority > DataZoneEquipment::ZoneEquipList(ZoneEquipListNum).HeatingPriority(EquipmentTypeNum)) {
                                                        TankNotLowestPriority = true;
                                                    }
                                                } // EquipmentTypeNum
                                                if (TankNotLowestPriority && FoundTankInList) {
                                                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                                                    ShowContinueError("Heat pump water heaters should be simulated first, before other space "
                                                                      "conditioning equipment.");
                                                    ShowContinueError("Poor temperature control may result if the Heating/Cooling sequence number is "
                                                                      "not 1 in the ZoneHVAC:EquipmentList.");
                                                }
                                                break;
                                            } // ZoneEquipConfigNum .LE. NumOfZoneEquipLists
                                        }     // ZoneEquipListNum
                                        break;
                                    } // ZoneEquipConfigNum .LE. NumOfZones
                                }     // ZoneEquipConfigNum
                            } else {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("ZoneHVAC:EquipmentList and ZoneHVAC:EquipmentConnections objects are required when Inlet Air "
                                                  "Configuration is either ZoneAirOnly or ZoneAndOutdoorAir.");
                                ErrorsFound = true;
                            } // ALLOCATED
                        }     // InletAirConfiguration

                        if (Tank.TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {

                            // Nodal heat distribution fraction for stratified tank wrapped condensers
                            if (HPWH.TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped) {
                                if (Tank.Shape == modTankShapeHorizCylinder) {
                                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ":");
                                    ShowContinueError("A wrapped condenser HPWH model should not be used with a horizontal stratified tank.");
                                    ShowContinueError(
                                        "Ignoring condenser location and distributing heat evenly throughout the tank. Simulation continues.");
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
                                            CurNode.HPWHWrappedCondenserHeatingFrac =
                                                1.0 - (HPWH.WrappedCondenserBottomLocation - H) / CurNode.Height;
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
                            if (Tank.Shape == modTankShapeVertCylinder || Tank.Shape == modTankShapeOther) {
                                TankHeight = Tank.Height;
                            } else {
                                assert(Tank.Shape == modTankShapeHorizCylinder);
                                // For horizontal cylinders, the tank "height" is actually the length.
                                // We need to calculate the height.
                                Real64 EndArea = Tank.Volume / Tank.Height;
                                Real64 Radius = std::sqrt(EndArea / DataGlobals::Pi);
                                TankHeight = 2.0 * Radius; // actual vertical height
                            }

                            // Make sure the control sensor locations are in the tank
                            if (HPWH.ControlSensor1Height < 0.0 || HPWH.ControlSensor1Height > TankHeight) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("Control Sensor 1 is located outside the tank.");
                                ErrorsFound = true;
                            }
                            if (HPWH.ControlSensor2Height < 0.0 || HPWH.ControlSensor2Height > TankHeight) {
                                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("Control Sensor 2 is located outside the tank.");
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
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError("Water heater tank object not found = " + HPWH.TankType + ", " + HPWH.TankName);
                        ErrorsFound = true;
                    }

                } // DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater
            }

            // Get water heater sizing input.
            DataIPShortCuts::cCurrentModuleObject = "WaterHeater:Sizing";
            NumWaterHeaterSizing = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumWaterHeaterSizing > 0) {

                for (int WHsizingNum = 1; WHsizingNum <= NumWaterHeaterSizing; ++WHsizingNum) {
                    int NumAlphas;
                    int NumNums;
                    int IOStat;
                    inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject, WHsizingNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat);

                    // find which water heater this object is for
                    int WaterThermalTankNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(1), WaterThermalTank);
                    if (WaterThermalTankNum == 0) {
                        // did not match name throw warning.
                        ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " object name: " + DataIPShortCuts::cAlphaArgs(1) +
                                        " does not match any of the water heaters defined in the file");
                        ErrorsFound = true;
                        continue;
                    } else { // we have a match
                        // store the sizing data in "sizing" nested derived type for the correct water heater

                        if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "PeakDraw")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = modSizePeakDraw;
                        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "ResidentialHUD-FHAMinimum")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = modSizeResidentialMin;
                        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "PerPerson")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = modSizePerPerson;
                        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "PerFloorArea")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = modSizePerFloorArea;
                        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "PerUnit")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = modSizePerUnit;
                        } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(2), "PerSolarCollectorArea")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = modSizePerSolarColArea;
                        } else {
                            // wrong design mode entered, throw error
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " object named: " + DataIPShortCuts::cAlphaArgs(1) +
                                            " contains an incorrect Design Mode of: " + DataIPShortCuts::cAlphaArgs(2));
                            ErrorsFound = true;
                        }

                        WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime = DataIPShortCuts::rNumericArgs(1);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime = DataIPShortCuts::rNumericArgs(2);
                        WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow = DataIPShortCuts::rNumericArgs(3);
                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms = int(DataIPShortCuts::rNumericArgs(4));
                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms = int(DataIPShortCuts::rNumericArgs(5));
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson = DataIPShortCuts::rNumericArgs(6);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson = DataIPShortCuts::rNumericArgs(7);
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea = DataIPShortCuts::rNumericArgs(8);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea = DataIPShortCuts::rNumericArgs(9);
                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits = DataIPShortCuts::rNumericArgs(10);
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit = DataIPShortCuts::rNumericArgs(11);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit = DataIPShortCuts::rNumericArgs(12);
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea = DataIPShortCuts::rNumericArgs(13);
                        WaterThermalTank(WaterThermalTankNum).Sizing.HeightAspectRatio = DataIPShortCuts::rNumericArgs(14);

                        {
                            auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode);

                            if (SELECT_CASE_var == modSizeNotSet) {
                                // do nothing, error thrown if design mode not found
                            } else if (SELECT_CASE_var == modSizePeakDraw) { // need to have entered a reasonable value for TankDrawTime
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime <= 0.0) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", design mode set to Peak Draw but needs a positive value for tank draw time");
                                    ErrorsFound = true;
                                }
                                // constrain crazy sizes by limiting to 10 years or 8760*10
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime > 87600.0) {
                                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                     ",  has input with an unreasonably large Tank Draw Time, more than 10 years");
                                    ErrorsFound = true;
                                }
                                // if both volume and demand side flow connections are autosized, must be a good NominalVolForSizingDemandSideFlow
                                if ((WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DataPlant::DemandSide) &&
                                    (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized)) {
                                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow <= 0.0) {
                                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                         " needs a value for Nominal Tank Volume for Autosizing Plant Connections");
                                        ErrorsFound = true;
                                    }
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide == DataPlant::DemandSide) &&
                                    (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized)) {
                                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow <= 0.0) {
                                        ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                         " needs a value for Nominal Tank Volume for Autosizing Plant Connections");
                                        ErrorsFound = true;
                                    }
                                }

                            } else if (SELECT_CASE_var == modSizeResidentialMin) {
                                // it would have to have at least on bedroom and any more than 10 is crazy for this mode
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms < 1) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) + ", mode needs at least one bedroom");
                                    ErrorsFound = true;
                                }
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms > 10) {
                                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                     ", probably has too many bedrooms for the selected design mode");
                                }

                            } else if (SELECT_CASE_var == modSizePerPerson) {

                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerPerson mode needs positive value input for storage capacity per person");
                                    ErrorsFound = true;
                                }

                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerPerson mode needs positive value input for recovery capacity per person");
                                    ErrorsFound = true;
                                }

                            } else if (SELECT_CASE_var == modSizePerFloorArea) {
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerArea mode needs positive value input for storage capacity per floor area");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerArea mode needs positive value input for recovery capacity per floor area");
                                    ErrorsFound = true;
                                }

                            } else if (SELECT_CASE_var == modSizePerUnit) {
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for storage capacity per unit");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for number of units");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for recovery capacity per unit");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits <= 0.0)) {
                                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for number of units");
                                    ErrorsFound = true;
                                }
                            } else if (SELECT_CASE_var == modSizePerSolarColArea) {
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea <= 0.0)) {
                                    ShowSevereError(
                                        DataIPShortCuts::cCurrentModuleObject + ", named " + DataIPShortCuts::cAlphaArgs(1) +
                                        ", PerSolarCollectorArea mode needs positive value input for storage capacity per collector area");
                                    ErrorsFound = true;
                                }
                            }
                        }

                    } // found water heater num okay
                }     // loop over sizing objects

            } // any water heater sizing objects

            // now check that if water heater fields were autosized, that there was also a sizing object for that water heater
            if (NumWaterThermalTank > 0) {
                for (int WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {

                    if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == modSizeNotSet)) {
                        ShowWarningError("Water heater named " + WaterThermalTank(WaterThermalTankNum).Name +
                                         "has tank volume set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                        ErrorsFound = true;
                    }
                    if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == modSizeNotSet)) {
                        ShowWarningError("Water heater named " + WaterThermalTank(WaterThermalTankNum).Name +
                                         "has heater capacity set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                        ErrorsFound = true;
                    }
                    if ((WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == modSizeNotSet)) {
                        ShowWarningError("Water heater named " + WaterThermalTank(WaterThermalTankNum).Name +
                                         "has tank height set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                        ErrorsFound = true;
                    }
                }
            }

            //    now do calls to TestCompSet for tanks, depending on nodes and heat pump water heater
            if (NumWaterThermalTank > 0) {
                for (int WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {
                    if (WaterThermalTank(WaterThermalTankNum).UseInletNode > 0 && WaterThermalTank(WaterThermalTankNum).UseOutletNode > 0) {
                        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                            // do nothing, Use nodes are tested for HeatPump:WaterHeater not tank
                        } else {
                            BranchNodeConnections::TestCompSet(WaterThermalTank(WaterThermalTankNum).Type,
                                        WaterThermalTank(WaterThermalTankNum).Name,
                                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1,
                                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1,
                                        "Use Side Water Nodes");
                        }
                    }
                    if (WaterThermalTank(WaterThermalTankNum).SourceInletNode > 0 && WaterThermalTank(WaterThermalTankNum).SourceOutletNode > 0) {

                        BranchNodeConnections::TestCompSet(WaterThermalTank(WaterThermalTankNum).Type,
                                    WaterThermalTank(WaterThermalTankNum).Name,
                                    WHSaveNodeNames(WaterThermalTankNum).InletNodeName2,
                                    WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2,
                                    "Source Side Water Nodes");
                    }
                }
            }

            if (NumWaterThermalTank > 0) {
                for (int WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {
                    if ((WaterThermalTank(WaterThermalTankNum).TypeNum != DataPlant::TypeOf_ChilledWaterTankMixed) &&
                        (WaterThermalTank(WaterThermalTankNum).TypeNum != DataPlant::TypeOf_ChilledWaterTankStratified)) {
                        // Setup report variables for WaterHeater:Mixed
                        // CurrentModuleObject='WaterHeater:Mixed'
                        SetupOutputVariable("Water Heater Tank Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).TankTempAvg,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Final Tank Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).TankTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Heat Loss Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).LossRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Heat Loss Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).LossEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Use Side Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            WaterThermalTank(WaterThermalTankNum).UseMassFlowRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Use Side Inlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).UseInletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Use Side Outlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).UseOutletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Use Side Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).UseRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Use Side Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).UseEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Source Side Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Source Side Inlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).SourceInletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Source Side Outlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).SourceOutletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Source Side Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).SourceRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Source Side Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).SourceEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name,
                                            _,
                                            "PLANTLOOPHEATINGDEMAND",
                                            "DHW",
                                            WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName,
                                            "Plant");

                        SetupOutputVariable("Water Heater Off Cycle Parasitic Tank Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).OffCycParaRateToTank,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Off Cycle Parasitic Tank Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).OffCycParaEnergyToTank,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater On Cycle Parasitic Tank Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).OnCycParaRateToTank,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater On Cycle Parasitic Tank Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).OnCycParaEnergyToTank,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Total Demand Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).TotalDemandRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Total Demand Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).TotalDemandEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Heating Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).HeaterRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Heating Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).HeaterEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Unmet Demand Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).UnmetRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Unmet Demand Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).UnmetEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Venting Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).VentRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Venting Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).VentEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Net Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).NetHeatTransferRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Net Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).NetHeatTransferEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Water Heater Cycle On Count",
                                            OutputProcessor::Unit::None,
                                            WaterThermalTank(WaterThermalTankNum).CycleOnCount,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Runtime Fraction",
                                            OutputProcessor::Unit::None,
                                            WaterThermalTank(WaterThermalTankNum).RuntimeFraction,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Part Load Ratio",
                                            OutputProcessor::Unit::None,
                                            WaterThermalTank(WaterThermalTankNum).PartLoadRatio,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            SetupOutputVariable("Water Heater Electric Power",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).FuelRate,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                        } else {
                            SetupOutputVariable("Water Heater " + WaterThermalTank(WaterThermalTankNum).FuelType + " Rate",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).FuelRate,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                        }
                        SetupOutputVariable("Water Heater " + WaterThermalTank(WaterThermalTankNum).FuelType + " Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).FuelEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name,
                                            _,
                                            WaterThermalTank(WaterThermalTankNum).FuelType,
                                            "DHW",
                                            WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName,
                                            "Plant");
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType, "Electric")) {
                            SetupOutputVariable("Water Heater Off Cycle Parasitic Electric Power",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).OffCycParaFuelRate,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                        } else {
                            SetupOutputVariable("Water Heater Off Cycle Parasitic " + WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType +
                                                    " Rate",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).OffCycParaFuelRate,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                        }
                        SetupOutputVariable("Water Heater Off Cycle Parasitic " + WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType +
                                                " Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name,
                                            _,
                                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType,
                                            "DHW",
                                            WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName,
                                            "Plant");
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType, "Electric")) {
                            SetupOutputVariable("Water Heater On Cycle Parasitic Electric Power",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).OnCycParaFuelRate,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                        } else {
                            SetupOutputVariable("Water Heater On Cycle Parasitic " + WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType +
                                                    " Rate",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).OnCycParaFuelRate,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                        }

                        SetupOutputVariable("Water Heater On Cycle Parasitic " + WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType + " Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name,
                                            _,
                                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType,
                                            "DHW",
                                            WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName,
                                            "Plant");

                        SetupOutputVariable("Water Heater Water Volume Flow Rate",
                                            OutputProcessor::Unit::m3_s,
                                            WaterThermalTank(WaterThermalTankNum).VolFlowRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Water Heater Water Volume",
                                            OutputProcessor::Unit::m3,
                                            WaterThermalTank(WaterThermalTankNum).VolumeConsumed,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name,
                                            _,
                                            "Water",
                                            "DHW",
                                            WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName,
                                            "Plant");
                        SetupOutputVariable("Water Heater Mains Water Volume",
                                            OutputProcessor::Unit::m3,
                                            WaterThermalTank(WaterThermalTankNum).VolumeConsumed,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name,
                                            _,
                                            "MainsWater",
                                            "DHW",
                                            WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName,
                                            "Plant");

                        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                            // CurrentModuleObject='WaterHeater:HeatPump:PumpedCondenser'
                            HeatPumpWaterHeaterData &HPWH = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum);
                            SetupOutputVariable("Water Heater Compressor Part Load Ratio",
                                                OutputProcessor::Unit::None,
                                                HPWH.HeatingPLR,
                                                "System",
                                                "Average",
                                                HPWH.Name);
                            SetupOutputVariable("Water Heater Off Cycle Ancillary Electric Power",
                                                OutputProcessor::Unit::W,
                                                HPWH.OffCycParaFuelRate,
                                                "System",
                                                "Average",
                                                HPWH.Name);
                            SetupOutputVariable("Water Heater Off Cycle Ancillary Electric Energy",
                                                OutputProcessor::Unit::J,
                                                HPWH.OffCycParaFuelEnergy,
                                                "System",
                                                "Sum",
                                                HPWH.Name,
                                                _,
                                                "Electric",
                                                "DHW",
                                                "Water Heater Parasitic",
                                                "Plant");
                            SetupOutputVariable("Water Heater On Cycle Ancillary Electric Power",
                                                OutputProcessor::Unit::W,
                                                HPWH.OnCycParaFuelRate,
                                                "System",
                                                "Average",
                                                HPWH.Name);
                            SetupOutputVariable("Water Heater On Cycle Ancillary Electric Energy",
                                                OutputProcessor::Unit::J,
                                                HPWH.OnCycParaFuelEnergy,
                                                "System",
                                                "Sum",
                                                HPWH.Name,
                                                _,
                                                "Electric",
                                                "DHW",
                                                "Water Heater Parasitic",
                                                "Plant");
                            SetupOutputVariable("Water Heater Heat Pump Control Tank Temperature",
                                                OutputProcessor::Unit::C,
                                                HPWH.ControlTempAvg,
                                                "System",
                                                "Average",
                                                HPWH.Name);
                            SetupOutputVariable("Water Heater Heat Pump Control Tank Final Temperature",
                                                OutputProcessor::Unit::C,
                                                HPWH.ControlTempFinal,
                                                "System",
                                                "Average",
                                                HPWH.Name);
                        }

                        if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                            // CurrentModuleObject='Coil:WaterHeating:Desuperheater'
                            SetupOutputVariable("Water Heater Part Load Ratio",
                                                OutputProcessor::Unit::None,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).DesuperheaterPLR,
                                                "System",
                                                "Average",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name);
                            SetupOutputVariable("Water Heater On Cycle Parasitic Electric Power",
                                                OutputProcessor::Unit::W,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).OnCycParaFuelRate,
                                                "System",
                                                "Average",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name);
                            SetupOutputVariable("Water Heater On Cycle Parasitic Electric Energy",
                                                OutputProcessor::Unit::J,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).OnCycParaFuelEnergy,
                                                "System",
                                                "Sum",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name,
                                                _,
                                                "Electric",
                                                "DHW",
                                                "Water Heater Parasitic",
                                                "Plant");
                            SetupOutputVariable("Water Heater Off Cycle Parasitic Electric Power",
                                                OutputProcessor::Unit::W,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).OffCycParaFuelRate,
                                                "System",
                                                "Average",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name);
                            SetupOutputVariable("Water Heater Off Cycle Parasitic Electric Energy",
                                                OutputProcessor::Unit::J,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).OffCycParaFuelEnergy,
                                                "System",
                                                "Sum",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name,
                                                _,
                                                "Electric",
                                                "DHW",
                                                "Water Heater Parasitic",
                                                "Plant");
                            SetupOutputVariable("Water Heater Heat Reclaim Efficiency Modifier Multiplier",
                                                OutputProcessor::Unit::None,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).HEffFTempOutput,
                                                "System",
                                                "Average",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name);
                            SetupOutputVariable("Water Heater Pump Electric Power",
                                                OutputProcessor::Unit::W,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).PumpPower,
                                                "System",
                                                "Average",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name);
                            SetupOutputVariable("Water Heater Pump Electric Energy",
                                                OutputProcessor::Unit::J,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).PumpEnergy,
                                                "System",
                                                "Sum",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name,
                                                _,
                                                "Electric",
                                                "DHW",
                                                "Desuperheater Pump",
                                                "Plant");
                            SetupOutputVariable("Water Heater Heating Rate",
                                                OutputProcessor::Unit::W,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).HeaterRate,
                                                "System",
                                                "Average",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name);
                            SetupOutputVariable("Water Heater Heating Energy",
                                                OutputProcessor::Unit::J,
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).HeaterEnergy,
                                                "System",
                                                "Sum",
                                                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Name,
                                                _,
                                                "EnergyTransfer",
                                                "DHW",
                                                "Water Heater",
                                                "Plant");
                        }

                        // Setup report variables for WaterHeater:Stratified
                        // CurrentModuleObject='WaterHeater:Stratified'
                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {

                            SetupOutputVariable("Water Heater Heater 1 Heating Rate",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).HeaterRate1,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                            SetupOutputVariable("Water Heater Heater 2 Heating Rate",
                                                OutputProcessor::Unit::W,
                                                WaterThermalTank(WaterThermalTankNum).HeaterRate2,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);

                            SetupOutputVariable("Water Heater Heater 1 Heating Energy",
                                                OutputProcessor::Unit::J,
                                                WaterThermalTank(WaterThermalTankNum).HeaterEnergy1,
                                                "System",
                                                "Sum",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                            SetupOutputVariable("Water Heater Heater 2 Heating Energy",
                                                OutputProcessor::Unit::J,
                                                WaterThermalTank(WaterThermalTankNum).HeaterEnergy2,
                                                "System",
                                                "Sum",
                                                WaterThermalTank(WaterThermalTankNum).Name);

                            SetupOutputVariable("Water Heater Heater 1 Cycle On Count",
                                                OutputProcessor::Unit::None,
                                                WaterThermalTank(WaterThermalTankNum).CycleOnCount1,
                                                "System",
                                                "Sum",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                            SetupOutputVariable("Water Heater Heater 2 Cycle On Count",
                                                OutputProcessor::Unit::None,
                                                WaterThermalTank(WaterThermalTankNum).CycleOnCount2,
                                                "System",
                                                "Sum",
                                                WaterThermalTank(WaterThermalTankNum).Name);

                            SetupOutputVariable("Water Heater Heater 1 Runtime Fraction",
                                                OutputProcessor::Unit::None,
                                                WaterThermalTank(WaterThermalTankNum).RuntimeFraction1,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);
                            SetupOutputVariable("Water Heater Heater 2 Runtime Fraction",
                                                OutputProcessor::Unit::None,
                                                WaterThermalTank(WaterThermalTankNum).RuntimeFraction2,
                                                "System",
                                                "Average",
                                                WaterThermalTank(WaterThermalTankNum).Name);

                            for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Water Heater Temperature Node " + General::TrimSigDigits(NodeNum),
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).TempAvg,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }

                            for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Water Heater Final Temperature Node " + General::TrimSigDigits(NodeNum),
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Temp,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }
                        }

                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {

                            for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_723)
                                    << General::TrimSigDigits(NodeNum) << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Height, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Volume, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).MaxCapacity, 3)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).OffCycLossCoeff, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).OnCycLossCoeff, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Inlets)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Outlets);
                            }
                        }

                    } else if ((WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankMixed) ||
                               (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified)) {
                        // CurrentModuleObject='ThermalStorage:ChilledWater:Mixed/ThermalStorage:ChilledWater:Stratified'
                        SetupOutputVariable("Chilled Water Thermal Storage Tank Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).TankTempAvg,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Final Tank Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).TankTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Tank Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).LossRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Chilled Water Thermal Storage Tank Heat Gain Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).LossEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Use Side Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            WaterThermalTank(WaterThermalTankNum).UseMassFlowRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Use Side Inlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).UseInletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Use Side Outlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).UseOutletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Use Side Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).UseRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Chilled Water Thermal Storage Use Side Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).UseEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Source Side Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Source Side Inlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).SourceInletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Source Side Outlet Temperature",
                                            OutputProcessor::Unit::C,
                                            WaterThermalTank(WaterThermalTankNum).SourceOutletTemp,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        SetupOutputVariable("Chilled Water Thermal Storage Source Side Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            WaterThermalTank(WaterThermalTankNum).SourceRate,
                                            "System",
                                            "Average",
                                            WaterThermalTank(WaterThermalTankNum).Name);
                        SetupOutputVariable("Chilled Water Thermal Storage Source Side Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            WaterThermalTank(WaterThermalTankNum).SourceEnergy,
                                            "System",
                                            "Sum",
                                            WaterThermalTank(WaterThermalTankNum).Name);

                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {

                            for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Chilled Water Thermal Storage Temperature Node " + General::TrimSigDigits(NodeNum) + "",
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).TempAvg,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }

                            for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Chilled Water Thermal Storage Final Temperature Node " + General::TrimSigDigits(NodeNum) + "",
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Temp,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }
                        }

                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {

                            for (int NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_724)
                                    << General::TrimSigDigits(NodeNum) << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Height, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Volume, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).OffCycLossCoeff, 4)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Inlets)
                                    << General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Outlets);
                            }
                        }
                    }

                    // set up internal gains if tank is in a thermal zone
                    if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone > 0) {
                        {
                            auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).TypeNum);

                            if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "WaterHeater:Mixed",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      DataHeatBalance::IntGainTypeOf_WaterHeaterMixed,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "WaterHeater:Stratified",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      DataHeatBalance::IntGainTypeOf_WaterHeaterStratified,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankMixed) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "ThermalStorage:ChilledWater:Mixed",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            } else if (SELECT_CASE_var == DataPlant::TypeOf_ChilledWaterTankStratified) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "ThermalStorage:ChilledWater:Stratified",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterStratified,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            }
                        }
                    }

                } // WaterThermalTankNum
            }

        } // get input flag

        return ErrorsFound;
    }

    void ValidatePLFCurve(int const CurveIndex, bool &IsValid)
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

        // FLOW:
        IsValid = true;

        // Check 0 and 1
        if (CurveManager::CurveValue(CurveIndex, 0.0) <= 0) IsValid = false;
        if (CurveManager::CurveValue(CurveIndex, 1.0) <= 0) IsValid = false;
    }

    void SetupStratifiedNodes(int const WaterThermalTankNum) // Water Heater being simulated
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

        static std::string const RoutineName("GetWaterThermalTankInput");

        const Real64 Tolerance(1.0e-8); // Tolerance for Newton-Raphson solution
        const Real64 FluidCond(0.6);    // Conductivity of water (W/m-K)

        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);

        // FLOW:
        int NumNodes = Tank.Nodes;
        Tank.Node.allocate(NumNodes);
        Real64 rho;
        if ((Tank.UseSidePlantLoopNum > 0) && allocated(DataPlant::PlantLoop)) {
            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(Tank.UseSidePlantLoopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   DataPlant::PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex,
                                   RoutineName);
        } else {
            rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, WaterThermalTank(WaterThermalTankNum).FluidIndex, RoutineName);
        }

        Real64 NodeMass = Tank.Volume * rho / NumNodes;
        Real64 TankHeight;

        // Mixing rate set to 50% of the max value for dt = 1.0
        Tank.InversionMixingRate = NodeMass * 0.5 * 1.0;

        if ((Tank.Shape == modTankShapeVertCylinder) || (Tank.Shape == modTankShapeOther)) {
            TankHeight = Tank.Height;
            Real64 EndArea = Tank.Volume / TankHeight;
            Real64 NodeHeight = TankHeight / NumNodes;
            Real64 CondCoeff = (FluidCond + Tank.AdditionalCond) * EndArea / NodeHeight;

            Real64 Perimeter;
            if (Tank.Shape == modTankShapeVertCylinder) {
                Real64 Radius = std::sqrt(EndArea / DataGlobals::Pi);
                Perimeter = 2.0 * DataGlobals::Pi * Radius;
            } else { // TankShapeOther
                Perimeter = Tank.Perimeter;
            }

            for (int NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                Tank.Node(NodeNum).Mass = NodeMass;
                Tank.Node(NodeNum).Volume = Tank.Volume / NumNodes;
                Tank.Node(NodeNum).Height = NodeHeight;
                Tank.Node(NodeNum).CondCoeffUp = CondCoeff;
                Tank.Node(NodeNum).CondCoeffDn = CondCoeff;

                Real64 SkinArea;
                if ((NodeNum == 1) || (NodeNum == NumNodes)) {
                    SkinArea = Perimeter * NodeHeight + EndArea;
                } else {
                    SkinArea = Perimeter * NodeHeight;
                }

                Tank.Node(NodeNum).OnCycLossCoeff = Tank.SkinLossCoeff * SkinArea + Tank.AdditionalLossCoeff(NodeNum);

                Tank.Node(NodeNum).OffCycLossCoeff = Tank.Node(NodeNum).OnCycLossCoeff + Tank.OffCycFlueLossCoeff;

            } // NodeNum

            Tank.Node(1).CondCoeffUp = 0.0;
            Tank.Node(NumNodes).CondCoeffDn = 0.0;

        } else {                      // Tank%Shape == TankShapeHorizCylinder
            Real64 TankLength = Tank.Height; // Height is the length in the axial direction
            Real64 EndArea = Tank.Volume / TankLength;
            Real64 Radius = std::sqrt(EndArea / DataGlobals::Pi);
            TankHeight = 2.0 * Radius; // Actual vertical height
            Real64 NodeEndArea = EndArea / NumNodes;

            Real64 R = Radius;
            Real64 H0 = 0.0;
            Real64 ChordLength = 0.0;
            for (int NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                Tank.Node(NodeNum).Mass = NodeMass;
                Tank.Node(NodeNum).Volume = Tank.Volume / NumNodes;
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

                Tank.Node(NodeNum).Height = H - H0;

                if (NodeNum > 1) {
                    Real64 CrossArea = 2.0 * ChordLength * TankLength; // Use old ChordLength from previous node
                    Real64 CondCoeff = (FluidCond + Tank.AdditionalCond) * CrossArea / (0.5 * (H - H0) + 0.5 * Tank.Node(NodeNum - 1).Height);
                    Tank.Node(NodeNum - 1).CondCoeffUp = CondCoeff; // Set for previous node
                    Tank.Node(NodeNum).CondCoeffDn = CondCoeff;     // Set for this node
                }

                ChordLength = std::sqrt(2.0 * R * H - H * H); // Calc new ChordLength to be used with next node

                Real64 Perimeter = 2.0 * R * (std::acos((R - H) / R) - std::acos((R - H0) / R)); // Segments of circular perimeter
                Real64 SkinArea = Perimeter * TankLength + 2.0 * NodeEndArea;

                Tank.Node(NodeNum).OnCycLossCoeff = Tank.SkinLossCoeff * SkinArea + Tank.AdditionalLossCoeff(NodeNum);

                Tank.Node(NodeNum).OffCycLossCoeff = Tank.Node(NodeNum).OnCycLossCoeff + Tank.OffCycFlueLossCoeff;
                // Although it doesn't make much sense to have a flue in a horizontal tank, keep it in anyway

                H0 = H;
            } // NodeNum

            Tank.Node(1).CondCoeffUp = 0.0;
            Tank.Node(NumNodes).CondCoeffDn = 0.0;
        }

        // Loop through nodes again (from top to bottom this time) and assign heating elements, parasitics, flow inlets/outlets
        // according to their vertical heights in the tank.
        Real64 H0 = TankHeight;
        for (int NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
            Real64 H;
            if (NodeNum == NumNodes) {
                H = -1.0; // Avoids rounding errors and ensures that anything at height 0.0 goes into the bottom node
            } else {
                H = H0 - Tank.Node(NodeNum).Height;
            }

            // Assign heater elements to the nodes at the specified heights
            if ((Tank.HeaterHeight1 <= H0) && (Tank.HeaterHeight1 > H)) {
                //       sensor node will not get set if user enters 0 for this heater capacity
                //       (Tank%MaxCapacity > 0.0d0)) THEN
                Tank.HeaterNode1 = NodeNum;
                Tank.Node(NodeNum).MaxCapacity = Tank.MaxCapacity;
            }

            if ((Tank.HeaterHeight2 <= H0) && (Tank.HeaterHeight2 > H)) {
                //       sensor node will not get set if user enters 0 for this heater capacity
                //      .AND. (Tank%MaxCapacity2 > 0.0d0)) THEN
                Tank.HeaterNode2 = NodeNum;

                if ((NodeNum == Tank.HeaterNode1) && (Tank.ControlType == modPrioritySimultaneous)) {
                    Tank.Node(NodeNum).MaxCapacity += Tank.MaxCapacity2;
                } else {
                    Tank.Node(NodeNum).MaxCapacity = Tank.MaxCapacity2;
                }
            }

            // Assign parasitic heat gains to the nodes at the specified heights
            if ((Tank.OffCycParaHeight <= H0) && (Tank.OffCycParaHeight > H)) {
                Tank.Node(NodeNum).OffCycParaLoad = Tank.OffCycParaFracToTank * Tank.OffCycParaLoad;
            }

            if ((Tank.OnCycParaHeight <= H0) && (Tank.OnCycParaHeight > H)) {
                Tank.Node(NodeNum).OnCycParaLoad = Tank.OnCycParaFracToTank * Tank.OnCycParaLoad;
            }

            // Assign inlets and outlets to the nodes at the specified heights
            if ((Tank.UseInletHeight <= H0) && (Tank.UseInletHeight > H)) {
                Tank.UseInletStratNode = NodeNum;

                if ((Tank.UseInletNode > 0) || (Tank.MassFlowRateMax > 0.0)) ++Tank.Node(NodeNum).Inlets;
            }

            if ((Tank.UseOutletHeight <= H0) && (Tank.UseOutletHeight > H)) {
                Tank.UseOutletStratNode = NodeNum;

                if ((Tank.UseOutletNode > 0) || (Tank.MassFlowRateMax > 0.0)) ++Tank.Node(NodeNum).Outlets;
            }

            if ((Tank.SourceInletHeight <= H0) && (Tank.SourceInletHeight > H) && (Tank.SourceInletNode > 0)) {

                Tank.SourceInletStratNode = NodeNum;
                ++Tank.Node(NodeNum).Inlets;
            }

            if ((Tank.SourceOutletHeight <= H0) && (Tank.SourceOutletHeight > H) && (Tank.SourceOutletNode > 0)) {

                Tank.SourceOutletStratNode = NodeNum;
                ++Tank.Node(NodeNum).Outlets;
            }

            H0 = H;
        } // NodeNum
    }

    void InitWaterThermalTank(int const WaterThermalTankNum,
                              bool const FirstHVACIteration,
                              Optional_int_const EP_UNUSED(LoopNum),
                              Optional_int_const EP_UNUSED(LoopSideNum))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2004
        //       MODIFIED       FSEC, July 2005
        //                      Brent Griffith, October 2007 indirect fired water heater
        //						B. Shen 12/2014, add air-source variable-speed heat pump water heating
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initialize the water heater, heat pump water heater, or desuperheater heating coil objects during the simulation.
        // determine flow rates thru use side and source side plant connections (if any)

        // METHODOLOGY EMPLOYED:
        // Inlet and outlet nodes are initialized.  Scheduled values are retrieved for the current timestep.

        static std::string const RoutineName("InitWaterThermalTank");
        static std::string const GetWaterThermalTankInput("GetWaterThermalTankInput");
        static std::string const SizeTankForDemand("SizeTankForDemandSide");

        if (modInitWaterThermalTanksOnce) {
            modInitWaterThermalTanksOnce = false;
        }

        int UseInletNode = WaterThermalTank(WaterThermalTankNum).UseInletNode;
        int UseOutletNode = WaterThermalTank(WaterThermalTankNum).UseOutletNode;
        int SourceInletNode = WaterThermalTank(WaterThermalTankNum).SourceInletNode;
        int SourceOutletNode = WaterThermalTank(WaterThermalTankNum).SourceOutletNode;

        if (WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag && allocated(DataPlant::PlantLoop)) {

            if ((UseInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(WaterThermalTank(WaterThermalTankNum).Name,
                                                        WaterThermalTank(WaterThermalTankNum).TypeNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        UseInletNode,
                                                        _);
                if (errFlag) {
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum =
                    DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).PlantSizNum;
                if ((WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized) &&
                    (WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum == 0)) {
                    ShowSevereError("InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " +
                                    WaterThermalTank(WaterThermalTankNum).Name);
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
            }
            if ((UseInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0)) {
                // this is a heat pump water heater, need a separate block because TypeOf_HeatPumpWtrHeater shows up on Branch
                //  (input should probably have been the associated tank )
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Name,
                                                        HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).TypeNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        UseInletNode,
                                                        _);
                if (errFlag) {
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum =
                    DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).PlantSizNum;
                if ((WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized) &&
                    (WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum == 0)) {
                    ShowSevereError("InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " +
                                    WaterThermalTank(WaterThermalTankNum).Name);
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
            }
            if ((SourceInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum == 0) &&
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(WaterThermalTank(WaterThermalTankNum).Name,
                                                        WaterThermalTank(WaterThermalTankNum).TypeNum,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        SourceInletNode,
                                                        _);
                if (UseInletNode > 0) {
                    PlantUtilities::InterConnectTwoPlantLoopSides(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                                  WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                  WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                                  WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                                  WaterThermalTank(WaterThermalTankNum).TypeNum,
                                                  true);
                }

                if (errFlag) {
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum =
                    DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).PlantSizNum;
                if ((WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized) &&
                    (WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum == 0)) {
                    ShowSevereError("InitWaterThermalTank: Did not find Sizing:Plant object for source side of plant thermal tank = " +
                                    WaterThermalTank(WaterThermalTankNum).Name);
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
            }
            if (((SourceInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0)) ||
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0)) {
                WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag = false;
            }

            if (DataPlant::PlantFirstSizesOkayToFinalize) WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag = false;
            if (WaterThermalTank(WaterThermalTankNum).StandAlone) {
                WaterThermalTank(WaterThermalTankNum).SizeStandAloneWaterHeater();
                WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag = false;
            }

        } else if (WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag && !DataGlobals::AnyPlantInModel) {
            if (WaterThermalTank(WaterThermalTankNum).StandAlone) {
                WaterThermalTank(WaterThermalTankNum).SizeStandAloneWaterHeater();
            }
            WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag = false;
        }

        if (DataGlobals::BeginEnvrnFlag && WaterThermalTank(WaterThermalTankNum).MyEnvrnFlag && !WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag) {

            if (DataPlant::PlantFirstSizesOkayToFinalize) {

                if (WaterThermalTank(WaterThermalTankNum).ControlType == modControlTypeCycle) {
                    WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;
                }

                // check for sizing issues that model can not support

                // if stratified tank model, ensure that nominal change over rate is greater than one minute, avoid numerical problems.

                if ((WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_WtrHeaterStratified) ||
                    (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified)) {
                    Real64 MaxSideVolFlow = max(WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate,
                                         WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);

                    if (MaxSideVolFlow > 0.0) { // protect div by zero
                        Real64 TankChangeRateScale = WaterThermalTank(WaterThermalTankNum).Volume / MaxSideVolFlow;
                        if (TankChangeRateScale < 60.0) { // nominal change over in less than one minute
                            ShowSevereError("InitWaterThermalTank: Detected problem for stratified tank model.  Model cannot be applied.");
                            ShowContinueError("Occurs for stratified tank name = " + WaterThermalTank(WaterThermalTankNum).Name);
                            ShowContinueError("Tank volume = " + General::RoundSigDigits(WaterThermalTank(WaterThermalTankNum).Volume, 4) + " [m3]");
                            ShowContinueError("Tank use side volume flow rate = " +
                                              General::RoundSigDigits(WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate, 4) + " [m3/s]");
                            ShowContinueError("Tank source side volume flow rate = " +
                                              General::RoundSigDigits(WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate, 4) + " [m3/s]");
                            ShowContinueError("Nominal tank change over rate = " + General::RoundSigDigits(TankChangeRateScale, 2) + " [s]");
                            ShowContinueError(
                                "Change over rate is too fast, increase tank volume, decrease connection flow rates or use mixed tank model");

                            ShowFatalError("InitWaterThermalTank: Simulation halted because of sizing problem in stratified tank model.");
                        }
                    }
                }
            }

            // Clear node initial conditions
            if (UseInletNode > 0 && UseOutletNode > 0) {
                DataLoopNode::Node(UseInletNode).Temp = 0.0;
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).MassFlowRateMin = WaterThermalTank(WaterThermalTankNum).VolFlowRateMin * rho;
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                PlantUtilities::InitComponentNodes(WaterThermalTank(WaterThermalTankNum).MassFlowRateMin,
                                   WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax,
                                   UseInletNode,
                                   UseOutletNode,
                                   WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                   WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                   WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                   WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum);
                WaterThermalTank(WaterThermalTankNum).UseOutletTemp = 0.0;
                WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = 0.0;
                WaterThermalTank(WaterThermalTankNum).SavedUseOutletTemp = 0.0;

                WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                WaterThermalTank(WaterThermalTankNum).UseBranchControlType = DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum)
                                                                                 .LoopSide(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide)
                                                                                 .Branch(WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum)
                                                                                 .Comp(WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum)
                                                                                 .FlowCtrl;
            }

            if ((SourceInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum == 0) &&
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                PlantUtilities::InitComponentNodes(0.0,
                                   WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax,
                                   SourceInletNode,
                                   SourceOutletNode,
                                   WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                   WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                   WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum,
                                   WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum);

                WaterThermalTank(WaterThermalTankNum).SourceOutletTemp = 0.0;
                WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
                WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp = 0.0;

                WaterThermalTank(WaterThermalTankNum).SourceBranchControlType =
                    DataPlant::PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum)
                        .LoopSide(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide)
                        .Branch(WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum)
                        .Comp(WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum)
                        .FlowCtrl;
            }

            if ((SourceInletNode > 0) &&
                ((WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) || (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0))) {
                DataLoopNode::Node(SourceInletNode).Temp = 0.0;
                WaterThermalTank(WaterThermalTankNum).SourceOutletTemp = 0.0;
                WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
                WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp = 0.0;
                Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, WaterThermalTank(WaterThermalTankNum).FluidIndex, SizeTankForDemand);
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
            }

            // Initialize tank temperature to setpoint of first hour of warm up period
            // (use HPWH or Desuperheater heating coil set point if applicable)
            int SchIndex;
            if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Mode = modFloatMode;
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SaveMode = modFloatMode;
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SaveWHMode = modFloatMode;
                SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule;
            } else if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Mode = modFloatMode;
                SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule;
            } else {
                SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
            }

            if (SchIndex > 0) {
                WaterThermalTank(WaterThermalTankNum).TankTemp = ScheduleManager::GetCurrentScheduleValue(SchIndex);
                WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                if (WaterThermalTank(WaterThermalTankNum).Nodes > 0) {
                    for (auto &e : WaterThermalTank(WaterThermalTankNum).Node) {
                        e.Temp = e.SavedTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                    }
                }
            } else {
                WaterThermalTank(WaterThermalTankNum).TankTemp = 20.0;
                WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                if (WaterThermalTank(WaterThermalTankNum).Nodes > 0) {
                    for (auto &e : WaterThermalTank(WaterThermalTankNum).Node) {
                        e.Temp = e.SavedTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                    }
                }
            }
            WaterThermalTank(WaterThermalTankNum).SourceOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).UseOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).SavedUseOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).TankTempAvg = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;

            WaterThermalTank(WaterThermalTankNum).SavedHeaterOn1 = false;
            WaterThermalTank(WaterThermalTankNum).SavedHeaterOn2 = false;
            WaterThermalTank(WaterThermalTankNum).Mode = modFloatMode;
            WaterThermalTank(WaterThermalTankNum).SavedMode = modFloatMode;
            WaterThermalTank(WaterThermalTankNum).FirstRecoveryDone = false;
            WaterThermalTank(WaterThermalTankNum).FirstRecoveryFuel = 0.0;
            WaterThermalTank(WaterThermalTankNum).UnmetEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).LossEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).FlueLossEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).UseEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).TotalDemandEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).SourceEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).HeaterEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).HeaterEnergy1 = 0.0;
            WaterThermalTank(WaterThermalTankNum).HeaterEnergy2 = 0.0;
            WaterThermalTank(WaterThermalTankNum).FuelEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).FuelEnergy1 = 0.0;
            WaterThermalTank(WaterThermalTankNum).FuelEnergy2 = 0.0;
            WaterThermalTank(WaterThermalTankNum).VentEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).OffCycParaEnergyToTank = 0.0;
            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelEnergy = 0.0;
            WaterThermalTank(WaterThermalTankNum).OnCycParaEnergyToTank = 0.0;
            WaterThermalTank(WaterThermalTankNum).NetHeatTransferEnergy = 0.0;
        }

        if (!DataGlobals::BeginEnvrnFlag) WaterThermalTank(WaterThermalTankNum).MyEnvrnFlag = true;

        if (WaterThermalTank(WaterThermalTankNum).WarmupFlag && (!DataGlobals::WarmupFlag)) {
            // reInitialize tank temperature to setpoint of first hour (use HPWH or Desuperheater heating coil set point if applicable)
            // BG's interpretation here is that its better to reset initial condition to setpoint once warm up is over.
            // (otherwise with a dynamic storage model it is difficult for the user to see the initial performance if it isn't periodic.)
            int SchIndex;
            if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Mode = modFloatMode;
                SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule;
            } else if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Mode = modFloatMode;
                SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule;
            } else {
                SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
            }

            if (SchIndex > 0) {
                WaterThermalTank(WaterThermalTankNum).TankTemp = ScheduleManager::GetCurrentScheduleValue(SchIndex);
                WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                if (WaterThermalTank(WaterThermalTankNum).Nodes > 0) {
                    for (auto &e : WaterThermalTank(WaterThermalTankNum).Node) {
                        e.Temp = e.SavedTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                    }
                }
            } else {
                WaterThermalTank(WaterThermalTankNum).TankTemp = 20.0;
                WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                if (WaterThermalTank(WaterThermalTankNum).Nodes > 0) {
                    for (auto &e : WaterThermalTank(WaterThermalTankNum).Node) {
                        e.Temp = e.SavedTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                    }
                }
            }
            WaterThermalTank(WaterThermalTankNum).SourceOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).UseOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).SavedUseOutletTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
            WaterThermalTank(WaterThermalTankNum).SavedHeaterOn1 = false;
            WaterThermalTank(WaterThermalTankNum).SavedHeaterOn2 = false;
            WaterThermalTank(WaterThermalTankNum).Mode = 0;
            WaterThermalTank(WaterThermalTankNum).SavedMode = 0;
            WaterThermalTank(WaterThermalTankNum).WarmupFlag = false;
        }
        if (DataGlobals::WarmupFlag) WaterThermalTank(WaterThermalTankNum).WarmupFlag = true;

        if (FirstHVACIteration) {
            // Get all scheduled values
            int SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
            WaterThermalTank(WaterThermalTankNum).SetPointTemp = ScheduleManager::GetCurrentScheduleValue(SchIndex);

            if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                if (WaterThermalTank(WaterThermalTankNum).SetPointTemp > WaterThermalTank(WaterThermalTankNum).TankTempLimit) {
                    // Setpoint temperature scheduled higher than maximum tank temperature limit
                    WaterThermalTank(WaterThermalTankNum).SetPointTemp = WaterThermalTank(WaterThermalTankNum).TankTempLimit - 1.0;

                    if (WaterThermalTank(WaterThermalTankNum).ShowSetPointWarning) {
                        ShowSevereError("Water heater = " + WaterThermalTank(WaterThermalTankNum).Name +
                                        ":  Water heater tank set point temperature is greater than the maximum tank temperature limit.");
                        ShowContinueErrorTimeStamp("Water heater tank set point temperature is reset to Tank Temperature Limit minus 1 C (" +
                                                   General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).SetPointTemp, 2) +
                                                   ") and simulation continues.");
                        WaterThermalTank(WaterThermalTankNum).ShowSetPointWarning = false;
                    }
                }
            } else {
                if (WaterThermalTank(WaterThermalTankNum).SetPointTemp < WaterThermalTank(WaterThermalTankNum).TankTempLimit) {
                    // Setpoint temperature scheduled lower than minimum tank temperature limit
                    WaterThermalTank(WaterThermalTankNum).SetPointTemp = WaterThermalTank(WaterThermalTankNum).TankTempLimit + 1.0;

                    if (WaterThermalTank(WaterThermalTankNum).ShowSetPointWarning) {
                        ShowSevereError("Chilled Water Tank = " + WaterThermalTank(WaterThermalTankNum).Name +
                                        ":  Water heater tank set point temperature is lower than the minimum tank temperature limit.");
                        ShowContinueErrorTimeStamp("Chilled water tank set point temperature is reset to Tank Temperature Limit plus 1 C (" +
                                                   General::TrimSigDigits(WaterThermalTank(WaterThermalTankNum).SetPointTemp, 2) +
                                                   ") and simulation continues.");
                        WaterThermalTank(WaterThermalTankNum).ShowSetPointWarning = false;
                    }
                }
            }

            SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2;
            if (SchIndex > 0) {
                WaterThermalTank(WaterThermalTankNum).SetPointTemp2 = ScheduleManager::GetCurrentScheduleValue(SchIndex);
            }

            {
                auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator);
                if (SELECT_CASE_var == modAmbientTempSchedule) {
                    SchIndex = WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule;
                    WaterThermalTank(WaterThermalTankNum).AmbientTemp = ScheduleManager::GetCurrentScheduleValue(SchIndex);

                } else if (SELECT_CASE_var == modAmbientTempZone) {
                    WaterThermalTank(WaterThermalTankNum).AmbientTemp = DataHeatBalFanSys::MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone);

                } else if (SELECT_CASE_var == modAmbientTempOutsideAir) {
                    WaterThermalTank(WaterThermalTankNum).AmbientTemp = DataLoopNode::Node(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode).Temp;
                }
            }

            if (UseInletNode == 0) { // Stand-alone operation

                SchIndex = WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule;
                if (SchIndex > 0) {
                    WaterThermalTank(WaterThermalTankNum).UseInletTemp = ScheduleManager::GetCurrentScheduleValue(SchIndex);
                } else {
                    WaterThermalTank(WaterThermalTankNum).UseInletTemp = DataEnvironment::WaterMainsTemp;
                }

                SchIndex = WaterThermalTank(WaterThermalTankNum).FlowRateSchedule;
                if (SchIndex > 0) {
                    WaterThermalTank(WaterThermalTankNum).UseMassFlowRate =
                        ScheduleManager::GetCurrentScheduleValue(SchIndex) * WaterThermalTank(WaterThermalTankNum).MassFlowRateMax;

                    WaterThermalTank(WaterThermalTankNum).VolFlowRate =
                        WaterThermalTank(WaterThermalTankNum).UseMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
                } else {
                    WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = WaterThermalTank(WaterThermalTankNum).MassFlowRateMax;
                    WaterThermalTank(WaterThermalTankNum).VolFlowRate =
                        WaterThermalTank(WaterThermalTankNum).UseMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
                }
            }

            if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTemp =
                    ScheduleManager::GetCurrentScheduleValue(HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule);
                if (HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTemp >=
                    WaterThermalTank(WaterThermalTankNum).TankTempLimit) {
                    // HP setpoint temperature scheduled equal to or higher than tank temperature limit
                    HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTemp =
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit - 1.0;

                    if (HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).ShowSetPointWarning) {
                        ShowSevereError(
                            "Heat Pump Water Heater = " + HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Name +
                            ":  Heat Pump water heater set point temperature is equal to or greater than the maximum tank temperature limit.");
                        ShowContinueErrorTimeStamp(
                            "Heat Pump water heater tank set point temperature is reset to Tank Temperature Limit minus 1 C (" +
                            General::TrimSigDigits(HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTemp, 2) +
                            ") and simulation continues.");
                        HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).ShowSetPointWarning = false;
                    }
                }
            }

            if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTemp =
                    ScheduleManager::GetCurrentScheduleValue(WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule);
            }

        } // first HVAC Iteration

        if (UseInletNode > 0 && !WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag) { // setup mass flows for plant connections
            Real64 DeadBandTemp;
            if (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp + WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            } else {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp - WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            }

            Real64 mdotUse = PlantMassFlowRatesFunc(WaterThermalTankNum,
                                                    UseInletNode,
                                                    FirstHVACIteration,
                                                    modUseSide,
                                                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                    WaterThermalTank(WaterThermalTankNum).UseSideSeries,
                                                    WaterThermalTank(WaterThermalTankNum).UseBranchControlType,
                                                    WaterThermalTank(WaterThermalTankNum).SavedUseOutletTemp,
                                                    DeadBandTemp,
                                                    WaterThermalTank(WaterThermalTankNum).SetPointTemp);
            PlantUtilities::SetComponentFlowRate(mdotUse,
                                 UseInletNode,
                                 UseOutletNode,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum);

            WaterThermalTank(WaterThermalTankNum).UseInletTemp = DataLoopNode::Node(UseInletNode).Temp;
            WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = mdotUse;
        }

        if (SourceInletNode > 0 && !WaterThermalTank(WaterThermalTankNum).SetLoopIndexFlag) { // setup mass flows for plant connections
            Real64 DeadBandTemp;
            if (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp + WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            } else {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp - WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            }

            Real64 sensedTemp;
            if (WaterThermalTank(WaterThermalTankNum).TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {
                int tmpNodeNum = WaterThermalTank(WaterThermalTankNum).HeaterNode1;
                sensedTemp = WaterThermalTank(WaterThermalTankNum).Node(tmpNodeNum).SavedTemp;
            } else {
                sensedTemp = WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp;
            }

            Real64 mdotSource = PlantMassFlowRatesFunc(WaterThermalTankNum,
                                                       SourceInletNode,
                                                       FirstHVACIteration,
                                                       modSourceSide,
                                                       WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                                       WaterThermalTank(WaterThermalTankNum).SourceSideSeries,
                                                       WaterThermalTank(WaterThermalTankNum).SourceBranchControlType,
                                                       sensedTemp,
                                                       DeadBandTemp,
                                                       WaterThermalTank(WaterThermalTankNum).SetPointTemp);
            if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum > 0) {
                PlantUtilities::SetComponentFlowRate(mdotSource,
                                     SourceInletNode,
                                     SourceOutletNode,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum);
            } else { // not really plant connected (desuperheater or heat pump)
                DataLoopNode::Node(SourceInletNode).MassFlowRate = mdotSource;
                DataLoopNode::Node(SourceOutletNode).MassFlowRate = mdotSource;
            }

            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(SourceInletNode).Temp;
            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = mdotSource;
        }

        // initialize HPWHs each iteration
        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {

            int HPNum = WaterThermalTank(WaterThermalTankNum).HeatPumpNum;

            if (WaterThermalTank(WaterThermalTankNum).MyHPSizeFlag) {
                //     autosize info must be calculated in GetWaterThermalTankInputFlag for use in StandardRating procedure
                //       (called at end of GetWaterThermalTankInputFlag)
                //     report autosizing information here (must be done after GetWaterThermalTankInputFlag is complete)
                if (HPWaterHeater(HPNum).WaterFlowRateAutoSized &&
                    (DataPlant::PlantFirstSizesOkayToReport || !DataGlobals::AnyPlantInModel || WaterThermalTank(WaterThermalTankNum).AlreadyRated)) {
                    ReportSizingManager::ReportSizingOutput(HPWaterHeater(HPNum).Type,
                                       HPWaterHeater(HPNum).Name,
                                       "Condenser water flow rate [m3/s]",
                                       HPWaterHeater(HPNum).OperatingWaterFlowRate);
                }
                if (HPWaterHeater(HPNum).AirFlowRateAutoSized &&
                    (DataPlant::PlantFirstSizesOkayToReport || !DataGlobals::AnyPlantInModel || WaterThermalTank(WaterThermalTankNum).AlreadyRated)) {
                    ReportSizingManager::ReportSizingOutput(HPWaterHeater(HPNum).Type,
                                       HPWaterHeater(HPNum).Name,
                                       "Evaporator air flow rate [m3/s]",
                                       HPWaterHeater(HPNum).OperatingAirFlowRate);
                }
                DataSizing::DataNonZoneNonAirloopValue = HPWaterHeater(HPNum).OperatingAirFlowRate;
                HPWaterHeater(HPNum).OperatingAirMassFlowRate = HPWaterHeater(HPNum).OperatingAirFlowRate * DataEnvironment::StdRhoAir;
                if (DataSizing::CurZoneEqNum > 0) {
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirFlow = true;
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirVolFlow = DataSizing::DataNonZoneNonAirloopValue;
                }
                if (DataPlant::PlantFirstSizesOkayToReport || !DataGlobals::AnyPlantInModel || WaterThermalTank(WaterThermalTankNum).AlreadyRated) WaterThermalTank(WaterThermalTankNum).MyHPSizeFlag = false;
            }

            int HPAirInletNode = HPWaterHeater(HPNum).HeatPumpAirInletNode;
            int HPAirOutletNode = HPWaterHeater(HPNum).HeatPumpAirOutletNode;
            int OutdoorAirNode = HPWaterHeater(HPNum).OutsideAirNode;
            int ExhaustAirNode = HPWaterHeater(HPNum).ExhaustAirNode;
            int HPWaterInletNode = HPWaterHeater(HPNum).CondWaterInletNode;
            int HPWaterOutletNode = HPWaterHeater(HPNum).CondWaterOutletNode;
            int InletAirMixerNode = HPWaterHeater(HPNum).InletAirMixerNode;
            int OutletAirSplitterNode = HPWaterHeater(HPNum).OutletAirSplitterNode;

            {
                auto const SELECT_CASE_var(HPWaterHeater(HPNum).CrankcaseTempIndicator);
                if (SELECT_CASE_var == modCrankcaseTempZone) {
                    DataHVACGlobals::HPWHCrankcaseDBTemp = DataHeatBalFanSys::MAT(HPWaterHeater(HPNum).AmbientTempZone);
                } else if (SELECT_CASE_var == modCrankcaseTempExterior) {
                    DataHVACGlobals::HPWHCrankcaseDBTemp = DataEnvironment::OutDryBulbTemp;
                } else if (SELECT_CASE_var == modCrankcaseTempSchedule) {
                    DataHVACGlobals::HPWHCrankcaseDBTemp = ScheduleManager::GetCurrentScheduleValue(HPWaterHeater(HPNum).CrankcaseTempSchedule);
                }
            }

            //   initialize HPWH report variables to 0 and set tank inlet node equal to outlet node
            HPWaterHeater(HPNum).HPWaterHeaterSensibleCapacity = 0.0;
            HPWaterHeater(HPNum).HPWaterHeaterLatentCapacity = 0.0;
            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
            HPWaterHeater(HPNum).HeatingPLR = 0.0;
            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;

            //   determine HPWH inlet air conditions based on inlet air configuration (Zone, ZoneAndOA, OutdoorAir, or Schedule)
            Real64 HPInletDryBulbTemp = 0.0;
            Real64 HPInletHumRat = 0.0;
            Real64 HPInletRelHum;
            {
                auto const SELECT_CASE_var(HPWaterHeater(HPNum).InletAirConfiguration);
                if (SELECT_CASE_var == modAmbientTempZone) {
                    modMixerInletAirSchedule = 0.0;
                    HPInletDryBulbTemp = DataLoopNode::Node(HPAirInletNode).Temp;
                    HPInletHumRat = DataLoopNode::Node(HPAirInletNode).HumRat;
                } else if (SELECT_CASE_var == modAmbientTempZoneAndOA) {
                    if (HPWaterHeater(HPNum).InletAirMixerSchPtr > 0) {
                        //         schedule values are checked for boundary of 0 and 1 in GetWaterThermalTankInputFlag
                        modMixerInletAirSchedule = ScheduleManager::GetCurrentScheduleValue(HPWaterHeater(HPNum).InletAirMixerSchPtr);
                    } else {
                        modMixerInletAirSchedule = 0.0;
                    }
                    HPInletDryBulbTemp =
                            modMixerInletAirSchedule * DataLoopNode::Node(OutdoorAirNode).Temp + (1.0 - modMixerInletAirSchedule) * DataLoopNode::Node(HPAirInletNode).Temp;
                    HPInletHumRat = modMixerInletAirSchedule * DataLoopNode::Node(OutdoorAirNode).HumRat + (1.0 - modMixerInletAirSchedule) * DataLoopNode::Node(HPAirInletNode).HumRat;
                } else if (SELECT_CASE_var == modAmbientTempOutsideAir) {
                    modMixerInletAirSchedule = 1.0;
                    HPInletDryBulbTemp = DataLoopNode::Node(OutdoorAirNode).Temp;
                    HPInletHumRat = DataLoopNode::Node(OutdoorAirNode).HumRat;

                } else if (SELECT_CASE_var == modAmbientTempSchedule) {
                    HPInletDryBulbTemp = ScheduleManager::GetCurrentScheduleValue(HPWaterHeater(HPNum).AmbientTempSchedule);
                    HPInletRelHum = ScheduleManager::GetCurrentScheduleValue(HPWaterHeater(HPNum).AmbientRHSchedule);
                    HPInletHumRat = Psychrometrics::PsyWFnTdbRhPb(HPInletDryBulbTemp, HPInletRelHum, DataEnvironment::OutBaroPress, RoutineName);
                    DataLoopNode::Node(HPAirInletNode).Temp = HPInletDryBulbTemp;
                    DataLoopNode::Node(HPAirInletNode).HumRat = HPInletHumRat;
                    DataLoopNode::Node(HPAirInletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(HPInletDryBulbTemp, HPInletHumRat);
                    DataLoopNode::Node(HPAirInletNode).Press = DataEnvironment::OutBaroPress;

                } else {
                    assert(false);
                }
            }

            modMdotAir = HPWaterHeater(HPNum).OperatingAirMassFlowRate;

            //   set up initial conditions on nodes
            if (InletAirMixerNode > 0) {
                DataLoopNode::Node(InletAirMixerNode).MassFlowRate = 0.0;
                DataLoopNode::Node(InletAirMixerNode).MassFlowRateMax = modMdotAir;
                DataLoopNode::Node(InletAirMixerNode).MassFlowRateMaxAvail = modMdotAir;
                DataLoopNode::Node(InletAirMixerNode).Temp = HPInletDryBulbTemp;
                DataLoopNode::Node(InletAirMixerNode).HumRat = HPInletHumRat;
                DataLoopNode::Node(InletAirMixerNode).Enthalpy = Psychrometrics::PsyHFnTdbW(HPInletDryBulbTemp, HPInletHumRat);
                DataLoopNode::Node(HPAirInletNode).MassFlowRate = 0.0;
                DataLoopNode::Node(HPAirOutletNode).MassFlowRate = 0.0;
                DataLoopNode::Node(OutdoorAirNode).MassFlowRate = 0.0;
                DataLoopNode::Node(ExhaustAirNode).MassFlowRate = 0.0;
            } else {
                if (OutdoorAirNode == 0) {
                    DataLoopNode::Node(HPAirInletNode).MassFlowRate = 0.0;
                    DataLoopNode::Node(HPAirInletNode).MassFlowRateMax = modMdotAir;
                    DataLoopNode::Node(HPAirInletNode).MassFlowRateMaxAvail = modMdotAir;
                    DataLoopNode::Node(HPAirOutletNode).MassFlowRate = 0.0;
                } else {
                    DataLoopNode::Node(OutdoorAirNode).MassFlowRate = 0.0;
                    DataLoopNode::Node(OutdoorAirNode).MassFlowRateMax = modMdotAir;
                    DataLoopNode::Node(OutdoorAirNode).MassFlowRateMaxAvail = modMdotAir;
                    DataLoopNode::Node(ExhaustAirNode).MassFlowRate = 0.0;
                }
            }

            if (OutletAirSplitterNode > 0) DataLoopNode::Node(OutletAirSplitterNode).MassFlowRate = 0.0;
            // these are water nodes are not managed by plant. the HP connects
            // directly to the WH without using plant. will not change this code for DSU because of this
            if (HPWaterHeater(HPNum).TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                DataLoopNode::Node(HPWaterInletNode).MassFlowRate = 0.0;
                DataLoopNode::Node(HPWaterOutletNode).MassFlowRate = 0.0;
            }

            //   set the max mass flow rate for outdoor fans
            DataLoopNode::Node(HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax = modMdotAir;

            //   Curve objects in DXCoils::CalcHPWHDXCoil will use inlet conditions to HPWH not inlet air conditions to DX Coil
            //   HPWHInletDBTemp and HPWHInletWBTemp are DataHVACGlobals to pass info to HPWHDXCoil
            DataHVACGlobals::HPWHInletDBTemp = HPInletDryBulbTemp;
            DataHVACGlobals::HPWHInletWBTemp = Psychrometrics::PsyTwbFnTdbWPb(DataHVACGlobals::HPWHInletDBTemp, HPInletHumRat, DataEnvironment::OutBaroPress);

            // initialize flow rates at speed levels for variable-speed HPWH
            if ((HPWaterHeater(HPNum).bIsIHP) && (0 == HPWaterHeater(HPNum).NumofSpeed)) // use SCWH coil represents
            {
                IntegratedHeatPump::SizeIHP(HPWaterHeater(HPNum).DXCoilNum); //
                // IntegratedHeatPump::SimIHP(modBlankString, HPWaterHeater(HPNum).DXCoilNum,
                //	0, EMP1, EMP2, EMP3, 0, 0.0, 1, 0.0, 0.0, 0.0, false, 0.0); //conduct the sizing operation in the IHP
                int VSCoilID = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                HPWaterHeater(HPNum).NumofSpeed = VariableSpeedCoils::VarSpeedCoil(VSCoilID).NumOfSpeeds;

            } else if (UtilityRoutines::SameString(HPWaterHeater(HPNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") &&
                       (HPWaterHeater(HPNum).NumofSpeed == 0)) {
                Real64 EMP1 = 4.0;
                Real64 EMP2 = 0.0;
                Real64 EMP3 = 0.0;
                VariableSpeedCoils::SimVariableSpeedCoils(modBlankString,
                                                          HPWaterHeater(HPNum).DXCoilNum,
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
                int VSCoilID = HPWaterHeater(HPNum).DXCoilNum;
                HPWaterHeater(HPNum).NumofSpeed = VariableSpeedCoils::VarSpeedCoil(VSCoilID).NumOfSpeeds;
                // below pass the flow rates from the VS coil to the water heater object
            }

            if (HPWaterHeater(HPNum).NumofSpeed > 0) {
                int VSCoilID;
                if (HPWaterHeater(HPNum).bIsIHP)
                    VSCoilID = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                else
                    VSCoilID = HPWaterHeater(HPNum).DXCoilNum;

                // scale air flow rates
                Real64 MulSpeedFlowScale =
                    VariableSpeedCoils::VarSpeedCoil(VSCoilID).RatedAirVolFlowRate / VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedAirVolFlowRate(VariableSpeedCoils::VarSpeedCoil(VSCoilID).NormSpedLevel);
                for (int Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) = VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale;
                }

                // check fan flow rate, should be larger than the max flow rate of the VS coil
                Real64 FanVolFlow = 0.0;
                if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    FanVolFlow = HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->designAirVolFlowRate;
                } else if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SimpleOnOff) {
                    Fans::GetFanVolFlow(HPWaterHeater(HPNum).FanNum, FanVolFlow);
                }

                if (FanVolFlow <
                    HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed)) { // but this is the not the scaled mas flow
                    // if ( FanVolFlow  < HPWaterHeater( HPNum ).HPWHAirVolFlowRate( HPWaterHeater( HPNum ).NumofSpeed ) ) {

                    ShowWarningError("InitWaterThermalTank: -air flow rate = " + General::TrimSigDigits(FanVolFlow, 7) +
                                     " in fan object "
                                     " is less than the MSHP system air flow rate"
                                     " when waterheating is required(" +
                                     General::TrimSigDigits(HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed), 7) + ").");
                    ShowContinueError(" The MSHP system flow rate when waterheating is required is reset to the"
                                      " fan flow rate and the simulation continues.");
                    ShowContinueError(" Occurs in " + HPWaterHeater(HPNum).Name);
                    HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed) = FanVolFlow;
                    // Check flow rates in other speeds and ensure flow rates are not above the max flow rate
                    for (int Iter = HPWaterHeater(HPNum).NumofSpeed - 1; Iter >= 1; --Iter) {
                        if (HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) > HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1)) {
                            ShowContinueError(" The MSHP system flow rate when waterheating is required is reset to the"
                                              " flow rate at higher speed and the simulation continues at Speed" +
                                              General::TrimSigDigits(Iter) + '.');
                            ShowContinueError(" Occurs in " + HPWaterHeater(HPNum).Name);
                            HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) = HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1);
                        }
                    }
                }

                for (int Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).MSAirSpeedRatio(Iter) =
                        HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) / HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed);
                }

                // scale water flow rates
                MulSpeedFlowScale = VariableSpeedCoils::VarSpeedCoil(VSCoilID).RatedWaterVolFlowRate /
                                    VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(VariableSpeedCoils::VarSpeedCoil(VSCoilID).NormSpedLevel);
                for (int Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).HPWHWaterVolFlowRate(Iter) = VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(Iter) * MulSpeedFlowScale;
                    HPWaterHeater(HPNum).HPWHWaterMassFlowRate(Iter) = VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedWaterMassFlowRate(Iter) * MulSpeedFlowScale;
                    HPWaterHeater(HPNum).MSWaterSpeedRatio(Iter) = VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(Iter) /
                                                                   VariableSpeedCoils::VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(HPWaterHeater(HPNum).NumofSpeed);
                }

                Real64 rhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, HPInletDryBulbTemp, HPInletHumRat);

                for (int Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).HPWHAirMassFlowRate(Iter) = HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) * rhoAir;
                }

                //   set the max mass flow rate for outdoor fans
                DataLoopNode::Node(HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax = HPWaterHeater(HPNum).HPWHAirMassFlowRate(HPWaterHeater(HPNum).NumofSpeed);
            }

        } //  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN

        // calling CalcStandardRatings early bypasses fan sizing since DataSizing::DataNonZoneNonAirloopValue has not been set yet
        if (!WaterThermalTank(WaterThermalTankNum).AlreadyRated) {
            if (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                WaterThermalTank(WaterThermalTankNum).AlreadyRated = true;
            } else {
                if (!DataGlobals::AnyPlantInModel || DataPlant::PlantFirstSizesOkayToReport || WaterThermalTank(WaterThermalTankNum).MaxCapacity > 0.0 ||
                    WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                    WaterThermalTank(WaterThermalTankNum).CalcStandardRatings(WaterThermalTankNum);
                }
            }
        }
    }

    void WaterThermalTankData::CalcWaterThermalTankMixed(int const WaterThermalTankNum) // Water Heater being simulated
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

        static std::string const RoutineName("CalcWaterThermalTankMixed");

        // Reference to objects
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum); // Reference to the tank object to save typing

        // FLOW:
        Real64 TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;

        if (Tank.TimeElapsed != TimeElapsed) {
            // The simulation has advanced to the next system DataGlobals::TimeStep.  Save conditions from the end of the previous system
            // DataGlobals::TimeStep for use as the initial conditions of each iteration that does not advance the system DataGlobals::TimeStep.
            Tank.SavedTankTemp = Tank.TankTemp;
            Tank.SavedMode = Tank.Mode;

            // Save outlet temperatures for demand-side flow control
            Tank.SavedUseOutletTemp = Tank.UseOutletTemp;
            Tank.SavedSourceOutletTemp = Tank.SourceOutletTemp;

            Tank.TimeElapsed = TimeElapsed;
        }

        Real64 TankTemp = Tank.SavedTankTemp;
        int Mode = Tank.SavedMode;

        Real64 Qmaxcap = Tank.MaxCapacity;
        Real64 Qmincap = Tank.MinCapacity;
        Real64 Qoffcycfuel = Tank.OffCycParaLoad;
        Real64 Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
        Real64 Qoncycfuel = Tank.OnCycParaLoad;
        Real64 Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;

        Real64 SetPointTemp = Tank.SetPointTemp;
        Real64 DeadBandTemp = Tank.getDeadBandTemp();
        Real64 MaxTemp = Tank.TankTempLimit;
        Real64 AmbientTemp = Tank.AmbientTemp;

        Real64 UseInletTemp = Tank.UseInletTemp;
        Real64 UseMassFlowRate = Tank.UseMassFlowRate * Tank.UseEffectiveness;
        Real64 SourceInletTemp = Tank.SourceInletTemp;
        Real64 SourceMassFlowRate = Tank.SourceMassFlowRate * Tank.SourceEffectiveness;

        Real64 rho;
        if (Tank.UseSidePlantLoopNum > 0) {
            rho = FluidProperties::GetDensityGlycol(
                DataPlant::PlantLoop(Tank.UseSidePlantLoopNum).FluidName, TankTemp, DataPlant::PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex, RoutineName);
        } else {
            rho = FluidProperties::GetDensityGlycol(modFluidNameWater, TankTemp, modWaterIndex, RoutineName);
        }

        Real64 TankMass = rho * Tank.Volume;

        Real64 Cp;
        if (Tank.UseSidePlantLoopNum > 0) {
            Cp = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(Tank.UseSidePlantLoopNum).FluidName, TankTemp, DataPlant::PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex, RoutineName);
        } else {
            Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, TankTemp, modWaterIndex, RoutineName);
        }

        Real64 SecInTimeStep = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        Real64 TimeRemaining = SecInTimeStep;
        Real64 TimeNeeded = 0.0;
        int CycleOnCount = 0;
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

        if (Tank.HeatPumpNum > 0) {
            HeatPumpWaterHeaterData const &HeatPump = HPWaterHeater(Tank.HeatPumpNum);
            DataLoopNode::NodeData const &HPWHCondWaterInletNode = DataLoopNode::Node(HeatPump.CondWaterInletNode);
            DataLoopNode::NodeData const &HPWHCondWaterOutletNode = DataLoopNode::Node(HeatPump.CondWaterOutletNode);
            HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
        }
        assert(HPWHCondenserDeltaT >= 0);

        Real64 Qheatpump;
        Real64 Qsource;
        CalcMixedTankSourceSideHeatTransferRate(HPWHCondenserDeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);

        // Calculate steady-state use heat rate.
        Real64 Quse = UseMassFlowRate * Cp * (UseInletTemp - SetPointTemp);

        while (TimeRemaining > 0.0) {

            TimeNeeded = 0.0;

            Real64 NewTankTemp = TankTemp;
            Real64 LossCoeff = 0.0;
            Real64 LossFracToZone = 0.0;

            {
                auto const SELECT_CASE_var(Mode);

                if (SELECT_CASE_var == modHeatMode) { // Heater is on

                    // Calculate heat rate needed to maintain the setpoint at steady-state conditions
                    LossCoeff = Tank.OnCycLossCoeff;
                    LossFracToZone = Tank.OnCycLossFracToZone;
                    Real64 Qloss = LossCoeff * (AmbientTemp - SetPointTemp);
                    Qneeded = -Quse - Qsource - Qloss - Qoncycheat;

                    if (TankTemp > SetPointTemp) {
                        // Heater is not needed after all, possibly due to step change in scheduled SetPointTemp

                        Qheater = 0.0;
                        Qunmet = 0.0;
                        Mode = modFloatMode;
                        continue;

                    } else if (TankTemp < SetPointTemp) {
                        // Attempt to recover to the setpoint as quickly as possible by using maximum heater capacity

                        // Qneeded is calculated above
                        // Qneeded does not account for the extra energy needed to recover to the setpoint
                        Qheater = Qmaxcap;
                        Qunmet = max(Qneeded - Qheater, 0.0);
                        Qheat = Qoncycheat + Qheater + Qheatpump;

                        // Calculate time needed to recover to the setpoint at maximum heater capacity
                        TimeNeeded = CalcTimeNeeded(TankTemp,
                                                    SetPointTemp,
                                                    AmbientTemp,
                                                    UseInletTemp,
                                                    SourceInletTemp,
                                                    TankMass,
                                                    Cp,
                                                    UseMassFlowRate,
                                                    SourceMassFlowRate,
                                                    LossCoeff,
                                                    Qheat);

                        if (TimeNeeded > TimeRemaining) {
                            // Heater is at maximum capacity and heats for all of the remaining time
                            // Setpoint temperature WILL NOT be recovered

                            TimeNeeded = TimeRemaining;

                            NewTankTemp = CalcTankTemp(TankTemp,
                                                       AmbientTemp,
                                                       UseInletTemp,
                                                       SourceInletTemp,
                                                       TankMass,
                                                       Cp,
                                                       UseMassFlowRate,
                                                       SourceMassFlowRate,
                                                       LossCoeff,
                                                       Qheat,
                                                       TimeNeeded);

                        } else { // TimeNeeded <= TimeRemaining
                            // Heater is at maximum capacity but will not heat for all of the remaining time (at maximum anyway)
                            // Setpoint temperature WILL be recovered

                            NewTankTemp = SetPointTemp;

                            SetPointRecovered = true;

                        } // TimeNeeded > TimeRemaining

                    } else { // TankTemp == SetPointTemp
                        // Attempt to maintain the setpoint by using the needed heater capacity (modulating, if allowed)

                        if (Qneeded <= 0.0) {
                            // Heater is not needed

                            Qneeded = 0.0;
                            Qheater = 0.0;
                            Qunmet = 0.0;
                            Mode = modFloatMode;
                            continue;

                        } else if (Qneeded < Qmincap) {
                            // Heater is required at less than the minimum capacity
                            // If cycling, Qmincap = Qmaxcap.  Once the setpoint is reached, heater will almost always be shut off here

                            {
                                auto const SELECT_CASE_var1(Tank.ControlType);

                                if (SELECT_CASE_var1 == modControlTypeCycle) {
                                    // Control will cycle on and off based on DeadBandTemp
                                    Qheater = 0.0;
                                    Qunmet = 0.0;
                                    Mode = modFloatMode;
                                    continue;

                                } else if (SELECT_CASE_var1 == modControlTypeModulate) {
                                    // Control will cycle on and off based on DeadBandTemp until Qneeded > Qmincap again
                                    Qheater = 0.0;
                                    Qunmet = Qneeded;
                                    Mode = modFloatMode;
                                    continue;

                                    // CASE (ControlTypeModulateWithOverheat)  ! Not yet implemented
                                    // Calculate time to reach steady-state temp; check for venting at MaxTemp limit
                                    // Qheater = Qmincap

                                    // CASE (ControlTypeModulateWithUnderheat)  ! Not yet implemented
                                    // Heater must not come back on until Qneeded >= Qmincap
                                    // Mode = modFloatMode
                                }
                            }

                        } else if (Qneeded <= Qmaxcap) {
                            // Heater can exactly meet the needed heat rate (usually by modulating) and heats for all of the remaining time
                            // Setpoint temperature WILL be maintained

                            TimeNeeded = TimeRemaining;

                            Qheater = Qneeded;
                            Qunmet = 0.0;

                            NewTankTemp = SetPointTemp;

                        } else { // Qneeded > Qmaxcap
                            // Heater is at maximum capacity and heats for all of the remaining time
                            // Setpoint temperature WILL NOT be maintained

                            TimeNeeded = TimeRemaining;

                            Qheater = Qmaxcap;
                            Qunmet = Qneeded - Qheater;
                            Qheat = Qoncycheat + Qheater + Qheatpump;

                            NewTankTemp = CalcTankTemp(TankTemp,
                                                       AmbientTemp,
                                                       UseInletTemp,
                                                       SourceInletTemp,
                                                       TankMass,
                                                       Cp,
                                                       UseMassFlowRate,
                                                       SourceMassFlowRate,
                                                       LossCoeff,
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
                    Real64 PLF = PartLoadFactor(WaterThermalTankNum, PLR);
                    Efuel += Qheater * TimeNeeded / (PLF * Tank.Efficiency);

                    Runtime += TimeNeeded;
                    PLRsum += PLR * TimeNeeded;

                    if (!Tank.FirstRecoveryDone) {
                        Tank.FirstRecoveryFuel += Efuel + Eoffcycfuel + Eoncycfuel;
                        if (SetPointRecovered) Tank.FirstRecoveryDone = true;
                    }

                } else if ((SELECT_CASE_var == modFloatMode) || (SELECT_CASE_var == modCoolMode)) { // Heater is off

                    // Calculate heat rate needed to maintain the setpoint at steady-state conditions
                    LossCoeff = Tank.OffCycLossCoeff;
                    LossFracToZone = Tank.OffCycLossFracToZone;
                    Real64 Qloss = LossCoeff * (AmbientTemp - SetPointTemp);
                    Qneeded = -Quse - Qsource - Qloss - Qoffcycheat;

                    // This section really needs to work differently depending on ControlType
                    // CYCLE will look at TankTemp, MODULATE will look at Qneeded

                    if ((TankTemp < DeadBandTemp) && (!Tank.IsChilledWaterTank)) {
                        // Tank temperature is already below the minimum, possibly due to step change in scheduled SetPointTemp

                        Mode = modHeatMode;
                        ++CycleOnCount;
                        continue;

                    } else if ((TankTemp >= DeadBandTemp) && (!Tank.IsChilledWaterTank)) {

                        Qheat = Qoffcycheat + Qheatpump;

                        // Calculate time needed for tank temperature to fall to minimum (setpoint - deadband)
                        TimeNeeded = CalcTimeNeeded(TankTemp,
                                                    DeadBandTemp,
                                                    AmbientTemp,
                                                    UseInletTemp,
                                                    SourceInletTemp,
                                                    TankMass,
                                                    Cp,
                                                    UseMassFlowRate,
                                                    SourceMassFlowRate,
                                                    LossCoeff,
                                                    Qheat);

                        if (TimeNeeded <= TimeRemaining) {
                            // Heating will be needed in this DataGlobals::TimeStep

                            NewTankTemp = DeadBandTemp;
                            Mode = modHeatMode;
                            ++CycleOnCount;

                        } else { // TimeNeeded > TimeRemaining
                            // Heating will not be needed for all of the remaining time

                            NewTankTemp = CalcTankTemp(TankTemp,
                                                       AmbientTemp,
                                                       UseInletTemp,
                                                       SourceInletTemp,
                                                       TankMass,
                                                       Cp,
                                                       UseMassFlowRate,
                                                       SourceMassFlowRate,
                                                       LossCoeff,
                                                       Qheat,
                                                       TimeRemaining);

                            if ((NewTankTemp < MaxTemp) || (Tank.IsChilledWaterTank)) {
                                // Neither heating nor venting is needed for all of the remaining time

                                TimeNeeded = TimeRemaining;

                            } else { // NewTankTemp >= MaxTemp
                                // Venting will be needed in this DataGlobals::TimeStep

                                // Calculate time needed for tank temperature to rise to the maximum
                                TimeNeeded = CalcTimeNeeded(TankTemp,
                                                            MaxTemp,
                                                            AmbientTemp,
                                                            UseInletTemp,
                                                            SourceInletTemp,
                                                            TankMass,
                                                            Cp,
                                                            UseMassFlowRate,
                                                            SourceMassFlowRate,
                                                            LossCoeff,
                                                            Qheat);

                                // if limit NewTankTemp >= MaxTemp
                                if (TankTemp >= MaxTemp) {
                                    TimeNeeded = TimeRemaining;
                                }
                                NewTankTemp = MaxTemp;
                                Mode = modVentMode;

                            } // NewTankTemp >= MaxTemp

                        } // TimeNeeded <= TimeRemaining

                    } else if ((TankTemp > DeadBandTemp) && (Tank.IsChilledWaterTank)) {
                        Mode = modCoolMode;
                        Qheat = Qheatpump;

                        NewTankTemp = CalcTankTemp(TankTemp,
                                                   AmbientTemp,
                                                   UseInletTemp,
                                                   SourceInletTemp,
                                                   TankMass,
                                                   Cp,
                                                   UseMassFlowRate,
                                                   SourceMassFlowRate,
                                                   LossCoeff,
                                                   Qheat,
                                                   TimeRemaining);
                        TimeNeeded = TimeRemaining;
                    } else if ((TankTemp <= DeadBandTemp) && (Tank.IsChilledWaterTank)) {

                        if (TankTemp < SetPointTemp) Mode = modFloatMode;

                        Qheat = Qheatpump;

                        NewTankTemp = CalcTankTemp(TankTemp,
                                                   AmbientTemp,
                                                   UseInletTemp,
                                                   SourceInletTemp,
                                                   TankMass,
                                                   Cp,
                                                   UseMassFlowRate,
                                                   SourceMassFlowRate,
                                                   LossCoeff,
                                                   Qheat,
                                                   TimeRemaining);
                        TimeNeeded = TimeRemaining;
                    } // TankTemp vs DeadBandTemp for heaters and chilled water tanks

                    // Update summed values
                    Eneeded += Qneeded * TimeNeeded;
                    Eunmet += Qunmet * TimeNeeded; // Qunmet may be propagated thru from the previous iteration
                    Eoffcycfuel += Qoffcycfuel * TimeNeeded;

                } else if (SELECT_CASE_var == modVentMode) { // Excess heat is vented

                    LossCoeff = Tank.OffCycLossCoeff;
                    LossFracToZone = Tank.OffCycLossFracToZone;
                    Qheat = Qoffcycheat + Qheatpump;

                    NewTankTemp = CalcTankTemp(TankTemp,
                                               AmbientTemp,
                                               UseInletTemp,
                                               SourceInletTemp,
                                               TankMass,
                                               Cp,
                                               UseMassFlowRate,
                                               SourceMassFlowRate,
                                               LossCoeff,
                                               Qheat,
                                               TimeRemaining);

                    if (NewTankTemp < MaxTemp) {
                        // Venting is no longer needed because conditions have changed

                        Mode = modFloatMode;
                        continue;

                    } else { // NewTankTemp >= MaxTemp

                        TimeNeeded = TimeRemaining;

                        // Calculate the steady-state venting rate needed to maintain the tank at maximum temperature
                        Real64 Qloss = LossCoeff * (AmbientTemp - MaxTemp);
                        Quse = UseMassFlowRate * Cp * (UseInletTemp - MaxTemp);
                        Qsource = SourceMassFlowRate * Cp * (SourceInletTemp - MaxTemp);
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

            Real64 deltaTsum = CalcTempIntegral(TankTemp,
                                         NewTankTemp,
                                         AmbientTemp,
                                         UseInletTemp,
                                         SourceInletTemp,
                                         TankMass,
                                         Cp,
                                         UseMassFlowRate,
                                         SourceMassFlowRate,
                                         LossCoeff,
                                         Qheat,
                                         TimeNeeded);

            // Update summed values
            Tsum += deltaTsum;
            Eloss += LossCoeff * (AmbientTemp * TimeNeeded - deltaTsum);
            Elosszone += LossFracToZone * LossCoeff * (AmbientTemp * TimeNeeded - deltaTsum);
            Euse += UseMassFlowRate * Cp * (UseInletTemp * TimeNeeded - deltaTsum);
            if (Tank.HeatPumpNum > 0) {
                Esource += Qheatpump * TimeNeeded;
            } else {
                Esource += SourceMassFlowRate * Cp * (SourceInletTemp * TimeNeeded - deltaTsum);
            }

            TankTemp = NewTankTemp; // Update tank temperature

            TimeRemaining -= TimeNeeded;

            if (CycleOnCount > MaxCycles) {

                if (!DataGlobals::WarmupFlag) {
                    if (Tank.MaxCycleErrorIndex == 0) {
                        ShowWarningError("WaterHeater:Mixed = " + Tank.Name + ":  Heater is cycling on and off more than once per second.");
                        ShowContinueError("Try increasing Deadband Temperature Difference or Tank Volume");
                        ShowContinueErrorTimeStamp("");
                    }
                    ShowRecurringWarningErrorAtEnd("WaterHeater:Mixed = " + Tank.Name + " Heater is cycling on and off more than once per second:",
                                                   Tank.MaxCycleErrorIndex);
                }

                break;

            } // CycleOnCount > MaxCycles

        } // TimeRemaining > 0.0

        // Calculate average values over the DataGlobals::TimeStep based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
        Real64 TankTempAvg = Tsum / SecInTimeStep;
        Real64 Qloss = Eloss / SecInTimeStep;
        Real64 Qlosszone = Elosszone / SecInTimeStep;
        Quse = Euse / SecInTimeStep;
        Qsource = Esource / SecInTimeStep;
        Qheater = Eheater / SecInTimeStep;
        Qoffcycfuel = Eoffcycfuel / SecInTimeStep;
        Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
        Qoncycfuel = Eoncycfuel / SecInTimeStep;
        Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;
        Qvent = Event / SecInTimeStep;
        Qneeded = Eneeded / SecInTimeStep;
        Qunmet = Eunmet / SecInTimeStep;
        Real64 RTF = Runtime / SecInTimeStep;
        PLR = PLRsum / SecInTimeStep;

        if (Tank.ControlType == modControlTypeCycle) {
            // Recalculate Part Load Factor and fuel energy based on Runtime Fraction, instead of Part Load Ratio
            Real64 PLF = PartLoadFactor(WaterThermalTankNum, RTF);
            Efuel = Eheater / (PLF * Tank.Efficiency);
        }

        Qfuel = Efuel / SecInTimeStep;

        Tank.Mode = Mode; // Operating mode for carry-over to next DataGlobals::TimeStep

        Tank.TankTemp = TankTemp;            // Final tank temperature for carry-over to next DataGlobals::TimeStep
        Tank.TankTempAvg = TankTempAvg;      // Average tank temperature over the DataGlobals::TimeStep for reporting

        if (!DataGlobals::WarmupFlag) {
            // Warn for potential freezing when avg of final temp over all nodes is below 2°C (nearing 0°C)
            if (Tank.TankTemp < 2) {
                if (Tank.FreezingErrorIndex == 0) {
                    ShowWarningError(RoutineName + ": " + Tank.Type +" = '"
                            + Tank.Name + "':  Temperature of tank < 2C indicates of possibility of freeze. Tank Temperature = "
                            + General::RoundSigDigits(Tank.TankTemp, 2) + " C.");
                    ShowContinueErrorTimeStamp("");
                }
                ShowRecurringWarningErrorAtEnd(Tank.Type +" = '" + Tank.Name + "':  Temperature of tank < 2C indicates of possibility of freeze",
                                               Tank.FreezingErrorIndex,
                                               Tank.TankTemp, // Report Max
                                               Tank.TankTemp, // Report Min
                                               _,             // Don't report Sum
                                               "{C}",         // Max Unit
                                               "{C}");        // Min Unit
            }
        }
        Tank.UseOutletTemp = TankTempAvg;    // Because entire tank is at same temperature
        Tank.SourceOutletTemp = TankTempAvg; // Because entire tank is at same temperature
        if (Tank.HeatPumpNum > 0) {
            Tank.SourceInletTemp = TankTempAvg + HPWHCondenserDeltaT; // Update the source inlet temperature to the average over the DataGlobals::TimeStep
        }

        Tank.LossRate = Qloss;
        Tank.UseRate = Quse;
        Tank.SourceRate = Qsource;
        Tank.OffCycParaRateToTank = Qoffcycheat;
        Tank.OnCycParaRateToTank = Qoncycheat;
        Tank.TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat;
        Tank.HeaterRate = Qheater;
        Tank.UnmetRate = Qunmet;
        Tank.VentRate = Qvent;
        Tank.NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent;

        Tank.CycleOnCount = CycleOnCount;
        Tank.RuntimeFraction = RTF;
        Tank.PartLoadRatio = PLR;

        Tank.FuelRate = Qfuel;
        Tank.OffCycParaFuelRate = Qoffcycfuel;
        Tank.OnCycParaFuelRate = Qoncycfuel;

        // Add water heater skin losses and venting losses to ambient zone, if specified
        if (Tank.AmbientTempZone > 0) Tank.AmbientZoneGain = -Qlosszone - Qvent;
    }

    void CalcMixedTankSourceSideHeatTransferRate(Real64 HPWHCondenserDeltaT, // input, The temperature difference (C) across the heat pump, zero if
                                                                             // there is no heat pump or if the heat pump is off
                                                 Real64 SourceInletTemp,     // input, Source inlet temperature (C)
                                                 Real64 Cp,                  // Specific heat of fluid (J/kg deltaC)
                                                 Real64 SetPointTemp,        // input, Mixed tank set point temperature
                                                 Real64 &SourceMassFlowRate, // source mass flow rate (kg/s)
                                                 Real64 &Qheatpump,          // heat transfer rate from heat pump
                                                 Real64 &Qsource // steady state heat transfer rate from a constant temperature source side flow
    )
    {
        // Function Information:
        //		Author: Noel Merket
        //		Date Written: January 2015
        //		Modified: na
        //		Re-engineered: na

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

    Real64 CalcTimeNeeded(Real64 const Ti, // Initial tank temperature (C)
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

        Real64 t;        // Time elapsed from Ti to Tf (s)

        // FLOW:
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

    Real64 CalcTankTemp(Real64 const Ti, // Initial tank temperature (C)
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

        // FLOW:
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

    Real64 CalcTempIntegral(Real64 const Ti, // Initial tank temperature (C)
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

        // FLOW:
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

    Real64 PartLoadFactor(int const WaterThermalTankNum, Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the Part Load Factor (PLF) based on a curve correlated to Part Load Ratio, if Heater Control Type
        // is MODULATE, or correlated to Runtime Fraction, if Heater Control Type is CYCLE.

        // Return value
        Real64 PartLoadFactor;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FLOW:
        if (WaterThermalTank(WaterThermalTankNum).PLFCurve > 0) {
            PartLoadFactor = CurveManager::CurveValue(WaterThermalTank(WaterThermalTankNum).PLFCurve, PartLoadRatio);

            PartLoadFactor = max(PartLoadFactor, 0.1);
        } else {
            // No curve was defined
            PartLoadFactor = 1.0;
        }

        return PartLoadFactor;
    }

    void WaterThermalTankData::CalcWaterThermalTankStratified(int const WaterThermalTankNum)
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

        static std::string const RoutineName("CalcWaterThermalTankStratified");
        const Real64 TemperatureConvergenceCriteria = 0.0001;
        const Real64 SubTimestepMax = 60.0 * 10.0; // seconds
        const Real64 SubTimestepMin = 10.0; // seconds
        Real64 dt;

        // Tank object reference
        const Real64 &nTankNodes = this->Nodes;

        // Fraction of the current hour that has elapsed (h)
        const Real64 TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;

        // Seconds in one DataGlobals::TimeStep (s)
        const Real64 SecInTimeStep = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // Advance tank simulation to the next system DataGlobals::TimeStep, if applicable
        if (this->TimeElapsed != TimeElapsed) {
            // The simulation has advanced to the next system DataGlobals::TimeStep.  Save conditions from the end of the previous system
            // DataGlobals::TimeStep for use as the initial conditions of each iteration that does not advance the system DataGlobals::TimeStep.
            for (auto &e : this->Node)
                e.SavedTemp = e.Temp;

            this->SavedHeaterOn1 = this->HeaterOn1;
            this->SavedHeaterOn2 = this->HeaterOn2;

            // Save outlet temperatures for demand-side flow control
            this->SavedUseOutletTemp = this->UseOutletTemp;
            this->SavedSourceOutletTemp = this->SourceOutletTemp;

            this->TimeElapsed = TimeElapsed;
        }

        // Reset node temperatures to what they were at the beginning of the system DataGlobals::TimeStep.
        for (auto &e : this->Node)
            e.Temp = e.SavedTemp;

        this->HeaterOn1 = this->SavedHeaterOn1;
        this->HeaterOn2 = this->SavedHeaterOn2;

        // Condenser configuration of heat pump water heater
        const int HPWHCondenserConfig = this->HeatPumpNum > 0 ? HPWaterHeater(this->HeatPumpNum).TypeNum : 0;

        // Heat rate from the heat pump (W)
        const Real64 Qheatpump = [this]{
            if (this->HeatPumpNum == 0) return 0.0;
            HeatPumpWaterHeaterData const &HPWH = HPWaterHeater(this->HeatPumpNum);
            Real64 CoilTotalHeatingEnergyRate;
            if (HPWH.NumofSpeed > 0) {
                // VSHPWH
                VariableSpeedCoils::VariableSpeedCoilData const &Coil = VariableSpeedCoils::VarSpeedCoil(HPWH.DXCoilNum);
                CoilTotalHeatingEnergyRate = Coil.TotalHeatingEnergyRate;
            } else {
                // Single speed HPWH
                DXCoils::DXCoilData const &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);
                CoilTotalHeatingEnergyRate = Coil.TotalHeatingEnergyRate;
            }
            return CoilTotalHeatingEnergyRate * this->SourceEffectiveness;
        }();

        // Minimum tank temperatures
        const Real64 MinTemp1 = this->SetPointTemp - this->DeadBandDeltaTemp;
        const Real64 MinTemp2 = this->SetPointTemp2 - this->DeadBandDeltaTemp2;

        // Specific Heat of water (J/kg K)
        const Real64 Cp = [&]{
            if (this->UseSidePlantLoopNum > 0) {
                return FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName, this->TankTemp, DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex, RoutineName);
            } else {
                return FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, this->TankTemp, modWaterIndex, RoutineName);
            }
        }();

        Real64 Eloss = 0.0;                 // Energy change due to ambient losses over the DataGlobals::TimeStep (J)
        Real64 Euse = 0.0;                  // Energy change due to use side mass flow over the DataGlobals::TimeStep (J)
        Real64 Esource = 0.0;               // Energy change due to source side mass flow over the DataGlobals::TimeStep (J)
        Real64 Eheater1 = 0.0;              // Energy change due to heater 1 over the DataGlobals::TimeStep (J)
        Real64 Eheater2 = 0.0;              // Energy change due to heater 2 over the DataGlobals::TimeStep (J)
        Real64 Eunmet = 0.0;                // Energy change unmet over the DataGlobals::TimeStep (J)
        Real64 Event = 0.0;                 // Energy change due to venting over the DataGlobals::TimeStep (J)
        int CycleOnCount1 = 0;              // Number of times heater 1 cycles on in the current time step
        int CycleOnCount2 = 0;              // Number of times heater 2 cycles on in the current time step
        Real64 Runtime = 0.0;               // Time that either heater is running (s)
        Real64 Runtime1 = 0.0;              // Time that heater 1 is running (s)
        Real64 Runtime2 = 0.0;              // Time that heater 2 is running (s)
        bool SetPointRecovered = false;     // Flag to indicate when set point is recovered for the first time
        //Added three variables for desuperheater sourceinlet temperature update
        Real64 MdotDesuperheaterWater = 0.0;      // mass flow rate of desuperheater source side water, kg/s
        Real64 DesuperheaterPLR = 0.0;            // Desuperheater part load ratio
        Real64 DesuperheaterHeaterRate = 0.0;     // Desuperheater heater rate (W)
        Real64 SourceInletTempSum = 0.0;          // Sum the source inlet temperature in sub time step to calculate average tempearature
        Real64 Qheater1;            // Heating rate of burner or electric heating element 1 (W)
        Real64 Qheater2;            // Heating rate of burner or electric heating element 2 (W)

        if (this->InletMode == modInletModeFixed) CalcNodeMassFlows(WaterThermalTankNum, modInletModeFixed);

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

        while(TimeRemaining > 0.0) {

            ++SubTimestepCount;

            bool PrevHeaterOn1 = this->HeaterOn1;
            bool PrevHeaterOn2 = this->HeaterOn2;

            if (this->InletMode == modInletModeSeeking) CalcNodeMassFlows(WaterThermalTankNum, modInletModeSeeking);

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
                            ++CycleOnCount1;
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
                    if ((this->ControlType == modPriorityMasterSlave) && this->HeaterOn1) {
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
                                ++CycleOnCount2;
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
                        B[i] += (- node.OffCycParaLoad + node.OnCycParaLoad + (node.OnCycLossCoeff - node.OffCycLossCoeff) * this->AmbientTemp) / NodeCapacitance;
                    }
                } else if (!(this->HeaterOn1 || this->HeaterOn2) and (PrevHeaterOn1 || PrevHeaterOn2)) {
                    // Remove on cycle loads
                    // Apply off cycle loads
                    for (int i = 0; i < nTankNodes; i++) {
                        auto &node(this->Node[i]);
                        Real64 NodeCapacitance = node.Mass * Cp;
                        A[i] -= (node.OffCycLossCoeff - node.OnCycLossCoeff) / NodeCapacitance;
                        B[i] -= (- node.OffCycParaLoad + node.OnCycParaLoad + (node.OnCycLossCoeff - node.OffCycLossCoeff) * this->AmbientTemp) / NodeCapacitance;
                    }
                }

                // Set the sub DataGlobals::TimeStep (dt)
                dt = TimeRemaining;
                for (int i = 0; i < nTankNodes; ++i) {
                    const Real64 Denominator = fabs(A[i] * Tavg[i] + B[i]);
                    if (Denominator != 0.0)
                        dt = min(dt, dT_max / Denominator);
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
                    if (NodeNum > 1) B[i] += tank_node.CondCoeffUp * Tavg[i-1];
                    if (NodeNum < nTankNodes) B[i] += tank_node.CondCoeffDn * Tavg[i+1];

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
                    A[i] += - (tank_node.MassFlowFromUpper + tank_node.MassFlowFromLower) * Cp;
                    if (NodeNum > 1) B[i] += tank_node.MassFlowFromUpper * Cp * Tavg[i-1];
                    if (NodeNum < nTankNodes) B[i] += tank_node.MassFlowFromLower * Cp * Tavg[i+1];

                    // Divide by mass and specific heat
                    // m * cp * dT/dt = q_net  =>  dT/dt = a * T + b
                    A[i] /= tank_node.Mass * Cp;
                    B[i] /= tank_node.Mass * Cp;


                } // end for each node

                // Calculate the average and final temperatures over the interval
                Real64 TfinalDiff = 0.0;
                for (int i=0; i < nTankNodes; ++i) {
                    const Real64 Tstart = this->Node[i].Temp;
                    const Real64 b_a = B[i] / A[i];
                    const Real64 e_a_dt = exp(A[i] * dt);
                    Tavg[i] = (Tstart + b_a) * (e_a_dt - 1.0) / (A[i] * dt) - b_a;
                    const Real64 Tfinal_old = Tfinal[i];
                    Tfinal[i] = (Tstart + b_a) * e_a_dt - b_a;
                    TfinalDiff = max(fabs(Tfinal[i] - Tfinal_old), TfinalDiff);
                }

                if (TfinalDiff < TemperatureConvergenceCriteria) break;

                if (this->DesuperheaterNum > 0){
                    DesuperheaterPLR = WaterHeaterDesuperheater(this->DesuperheaterNum).DesuperheaterPLR;
                    DesuperheaterHeaterRate = WaterHeaterDesuperheater(this->DesuperheaterNum).HeaterRate;
                    MdotDesuperheaterWater = WaterHeaterDesuperheater(this->DesuperheaterNum).OperatingWaterFlowRate * Psychrometrics::RhoH2O(Tavg[this->SourceOutletStratNode - 1]);
                    if (DesuperheaterPLR > 0.0 && MdotDesuperheaterWater > 0.0){
                        this->SourceInletTemp = Tavg[this->SourceOutletStratNode - 1] + (DesuperheaterHeaterRate/DesuperheaterPLR) / (MdotDesuperheaterWater * Cp);
                    }else{
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
            } while ( HasInversion );

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
                Real64 Qheat_node = 0.0;
                const Real64 Quse_node = node.UseMassFlowRate * Cp * (this->UseInletTemp - Tavg[i]);
                const Real64 Qsource_node = [&]{
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
            const Real64 Quse = (this->UseOutletStratNode > 0) ? this->UseEffectiveness * this->UseMassFlowRate * Cp * (this->UseInletTemp - Tavg[this->UseOutletStratNode - 1]) : 0.0;
            Euse += Quse * dt;
            const Real64 Qsource = [&]{
                if (this->HeatPumpNum > 0) {
                    if (HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                        return Qheatpump;
                    } else {
                        assert(HPWHCondenserConfig == DataPlant::TypeOf_HeatPumpWtrHeaterWrapped);
                        return 0.0;
                    }
                } else {
                    if (this->SourceOutletStratNode > 0) {
                        return this->SourceEffectiveness * this->SourceMassFlowRate * Cp * (this->SourceInletTemp - Tavg[this->SourceOutletStratNode - 1]);
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

        if (!DataGlobals::WarmupFlag) {
            // Warn for potential freezing when avg of final temp over all nodes is below 2°C (nearing 0°C)
            if (this->TankTemp < 2) {
                if (this->FreezingErrorIndex == 0) {
                    ShowWarningError(RoutineName + ": " + this->Type +" = '"
                            + this->Name + "':  Temperature of tank < 2C indicates of possibility of freeze. Tank Temperature = "
                            + General::RoundSigDigits(this->TankTemp, 2) + " C.");
                    ShowContinueErrorTimeStamp("");
                }
                ShowRecurringWarningErrorAtEnd(this->Type +" = '" + this->Name + "':  Temperature of tank < 2C indicates of possibility of freeze",
                                               this->FreezingErrorIndex,
                                               this->TankTemp, // Report Max
                                               this->TankTemp, // Report Min
                                               _,             // Don't report Sum
                                               "{C}",         // Max Unit
                                               "{C}");        // Min Unit
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
            HeatPumpWaterHeaterData const &HeatPump = HPWaterHeater(this->HeatPumpNum);
            DataLoopNode::NodeData const &HPWHCondWaterInletNode = DataLoopNode::Node(HeatPump.CondWaterInletNode);
            DataLoopNode::NodeData const &HPWHCondWaterOutletNode = DataLoopNode::Node(HeatPump.CondWaterOutletNode);
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
        this->TotalDemandRate = -this->UseRate - this->SourceRate - this->LossRate - this->OffCycParaRateToTank - this->OnCycParaRateToTank - WrappedCondenserHeatPumpRate;
        this->HeaterRate1 = Eheater1 / SecInTimeStep;
        this->HeaterRate2 = Eheater2 / SecInTimeStep;
        this->HeaterRate = this->HeaterRate1 + this->HeaterRate2;

        this->UnmetRate = Eunmet / SecInTimeStep;
        this->VentRate = Event / SecInTimeStep;
        this->NetHeatTransferRate = this->UseRate + this->SourceRate + this->LossRate + this->OffCycParaRateToTank + this->OnCycParaRateToTank + this->HeaterRate + this->VentRate + WrappedCondenserHeatPumpRate;

        this->CycleOnCount = CycleOnCount1 + CycleOnCount2;
        this->CycleOnCount1 = CycleOnCount1;
        this->CycleOnCount2 = CycleOnCount2;

        this->RuntimeFraction = Runtime / SecInTimeStep;
        this->RuntimeFraction1 = Runtime1 / SecInTimeStep;
        this->RuntimeFraction2 = Runtime2 / SecInTimeStep;

        this->FuelRate = (Eheater1 + Eheater2) / this->Efficiency / SecInTimeStep;

        // Add water heater skin losses and venting losses to ambient zone, if specified
        if (this->AmbientTempZone > 0) this->AmbientZoneGain = -this->LossRate * this->SkinLossFracToZone - this->VentRate;

    }

    void CalcNodeMassFlows(int const WaterThermalTankNum, // Water Heater being simulated
                           int const InletMode            // InletModeFixed or InletModeSeeking
    )
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

        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);

        int UseInletStratNode = Tank.UseInletStratNode;
        int UseOutletStratNode = Tank.UseOutletStratNode;
        int SourceInletStratNode = Tank.SourceInletStratNode;
        int SourceOutletStratNode = Tank.SourceOutletStratNode;

        Real64 UseMassFlowRate = Tank.UseMassFlowRate * Tank.UseEffectiveness;
        Real64 SourceMassFlowRate = Tank.SourceMassFlowRate * Tank.SourceEffectiveness;

        for (auto &e : Tank.Node) {
            e.UseMassFlowRate = 0.0;
            e.SourceMassFlowRate = 0.0;
            e.MassFlowFromUpper = 0.0;
            e.MassFlowFromLower = 0.0;
            e.MassFlowToUpper = 0.0;
            e.MassFlowToLower = 0.0;
        }

        if (InletMode == modInletModeSeeking) {
            // 'Seek' the node with the temperature closest to the inlet temperature
            // Start at the user-specified inlet node and search to the user-specified outlet node
            int Step;
            if (UseMassFlowRate > 0.0) {
                if (UseInletStratNode > UseOutletStratNode) {
                    Step = -1;
                } else {
                    Step = 1;
                }
                Real64 MinDeltaTemp = 1.0e6; // Some big number
                int const NodeNum_stop(floop_end(UseInletStratNode, UseOutletStratNode, Step));
                for (int NodeNum = UseInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step) {
                    Real64 DeltaTemp = std::abs(Tank.Node(NodeNum).Temp - Tank.UseInletTemp);
                    if (DeltaTemp < MinDeltaTemp) {
                        MinDeltaTemp = DeltaTemp;
                        UseInletStratNode = NodeNum;
                    } else if (DeltaTemp > MinDeltaTemp) {
                        break;
                    }
                }
            }

            if (SourceMassFlowRate > 0.0) {
                if (SourceInletStratNode > SourceOutletStratNode) {
                    Step = -1;
                } else {
                    Step = 1;
                }
                Real64 MinDeltaTemp = 1.0e6; // Some big number
                int const NodeNum_stop(floop_end(SourceInletStratNode, SourceOutletStratNode, Step));
                for (int NodeNum = SourceInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step) {
                    Real64 DeltaTemp = std::abs(Tank.Node(NodeNum).Temp - Tank.SourceInletTemp);
                    if (DeltaTemp < MinDeltaTemp) {
                        MinDeltaTemp = DeltaTemp;
                        SourceInletStratNode = NodeNum;
                    } else if (DeltaTemp > MinDeltaTemp) {
                        break;
                    }
                }
            }
        }

        if (UseInletStratNode > 0) Tank.Node(UseInletStratNode).UseMassFlowRate = UseMassFlowRate;
        if (SourceInletStratNode > 0) Tank.Node(SourceInletStratNode).SourceMassFlowRate = SourceMassFlowRate;

        if (UseMassFlowRate > 0.0) {
            if (UseOutletStratNode > UseInletStratNode) {
                // Use-side flow is down
                for (int NodeNum = UseInletStratNode; NodeNum <= UseOutletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToLower += UseMassFlowRate;
                }
                for (int NodeNum = UseInletStratNode + 1; NodeNum <= UseOutletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromUpper += UseMassFlowRate;
                }

            } else if (UseOutletStratNode < UseInletStratNode) {
                // Use-side flow is up
                for (int NodeNum = UseOutletStratNode; NodeNum <= UseInletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromLower += UseMassFlowRate;
                }
                for (int NodeNum = UseOutletStratNode + 1; NodeNum <= UseInletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToUpper += UseMassFlowRate;
                }

            } else {
                // Use-side flow is across the node; no flow to other nodes
            }
        }

        if (SourceMassFlowRate > 0.0) {
            if (SourceOutletStratNode > SourceInletStratNode) {
                // Source-side flow is down
                for (int NodeNum = SourceInletStratNode; NodeNum <= SourceOutletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToLower += SourceMassFlowRate;
                }
                for (int NodeNum = SourceInletStratNode + 1; NodeNum <= SourceOutletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromUpper += SourceMassFlowRate;
                }

            } else if (SourceOutletStratNode < SourceInletStratNode) {
                // Source-side flow is up
                for (int NodeNum = SourceOutletStratNode; NodeNum <= SourceInletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromLower += SourceMassFlowRate;
                }
                for (int NodeNum = SourceOutletStratNode + 1; NodeNum <= SourceInletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToUpper += SourceMassFlowRate;
                }

            } else {
                // Source-side flow is across the node; no flow to other nodes
            }
        }

        // Cancel out any up and down flows
        for (int NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
            Tank.Node(NodeNum).MassFlowFromUpper = max((Tank.Node(NodeNum).MassFlowFromUpper - Tank.Node(NodeNum).MassFlowToUpper), 0.0);
            Tank.Node(NodeNum).MassFlowFromLower = max((Tank.Node(NodeNum).MassFlowFromLower - Tank.Node(NodeNum).MassFlowToLower), 0.0);
        }
    }

    void CalcDesuperheaterWaterHeater(int const WaterThermalTankNum, // Water Heater being simulated
                                      bool const FirstHVACIteration  // TRUE if First iteration of simulation
    )
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

        int const MaxIte(500);     // Maximum number of iterations for RegulaFalsi

        Array1D<Real64> Par(5);     // Parameters passed to RegulaFalsi

        // FLOW:
        int DesuperheaterNum = WaterThermalTank(WaterThermalTankNum).DesuperheaterNum;
        int WaterInletNode = WaterHeaterDesuperheater(DesuperheaterNum).WaterInletNode;
        int WaterOutletNode = WaterHeaterDesuperheater(DesuperheaterNum).WaterOutletNode;

        // store first iteration tank temperature and desuperheater mode of operation
        if (FirstHVACIteration && !DataHVACGlobals::ShortenTimeStepSys && WaterHeaterDesuperheater(DesuperheaterNum).FirstTimeThroughFlag) {
            // Save conditions from end of previous system timestep
            // Every iteration that does not advance time should reset to these values
            WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
            WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
            WaterHeaterDesuperheater(DesuperheaterNum).SaveMode = WaterHeaterDesuperheater(DesuperheaterNum).Mode;
            WaterHeaterDesuperheater(DesuperheaterNum).FirstTimeThroughFlag = false;
        }

        else if (!FirstHVACIteration) {
            WaterHeaterDesuperheater(DesuperheaterNum).FirstTimeThroughFlag = true;
        }

        // initialize variables before invoking any RETURN statement
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
        // reset tank inlet temp from previous time step
        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp;
        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = 0.0;

        DataLoopNode::Node(WaterInletNode).MassFlowRate = 0.0;
        DataLoopNode::Node(WaterOutletNode).MassFlowRate = 0.0;
        DataLoopNode::Node(WaterOutletNode).Temp = WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp;

        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelRate = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelEnergy = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelEnergy = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).HEffFTempOutput = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).HeaterEnergy = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).PumpPower = 0.0;
        WaterHeaterDesuperheater(DesuperheaterNum).PumpEnergy = 0.0;

        // simulate only the water heater tank if the desuperheater coil is scheduled off
        Real64 AvailSchedule = ScheduleManager::GetCurrentScheduleValue(WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr);
        if (AvailSchedule == 0.0) {
            WaterHeaterDesuperheater(DesuperheaterNum).Mode = modFloatMode;
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
            return;
        }

        // simulate only the water heater tank if the lowest temperature available from the desuperheater coil
        // is less than water inlet temperature if the reclaim source is a refrigeration condenser
        if (WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType) {
            int SourceID = WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum;
            if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCONDENSER_REFRIGERATION) {
                if (DataHeatBalance::HeatReclaimRefrigCondenser(SourceID).AvailTemperature <= WaterThermalTank(WaterThermalTankNum).SourceInletTemp) {
                    WaterHeaterDesuperheater(DesuperheaterNum).Mode = modFloatMode;
                    WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
                    ShowRecurringWarningErrorAtEnd("WaterHeating:Desuperheater " + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                       " - Waste heat source temperature was too low to be useful.",
                                                   WaterHeaterDesuperheater(DesuperheaterNum).InsuffTemperatureWarn);
                    return;
                } // Temp too low
            }     // desuperheater source is condenser_refrigeration
        }         // validsourcetype

        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate = WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad;
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelEnergy =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // check that water heater tank cut-in temp is greater than desuperheater cut-in temp
        Real64 SetPointTemp = WaterHeaterDesuperheater(DesuperheaterNum).SetPointTemp;
        Real64 DeadBandTempDiff = WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff;
        if ((SetPointTemp - DeadBandTempDiff) <= WaterThermalTank(WaterThermalTankNum).SetPointTemp) {
            if (!DataGlobals::WarmupFlag && !DataGlobals::DoingSizing && !DataGlobals::KickOffSimulation) {
                Real64 MinTemp = SetPointTemp - DeadBandTempDiff;
                ++WaterHeaterDesuperheater(DesuperheaterNum).SetPointError;
                if (WaterHeaterDesuperheater(DesuperheaterNum).SetPointError < 5) {
                    ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                     "\":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the "
                                     "desuperheater. Desuperheater will be disabled.");
                    ShowContinueErrorTimeStamp(" ...Desuperheater cut-in temperature = " + General::RoundSigDigits(MinTemp, 2));
                } else {
                    ShowRecurringWarningErrorAtEnd(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                       WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                       "\":  Water heater tank set point temperature is greater than or equal to the cut-in "
                                                       "temperature of the desuperheater. Desuperheater will be disabled warning continues...",
                                                   WaterHeaterDesuperheater(DesuperheaterNum).SetPointErrIndex1,
                                                   MinTemp,
                                                   MinTemp);
                }
            }

            //   Simulate tank if desuperheater unavailable for water heating
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
            return;
        }

        Real64 Effic = WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;

        Real64 TankTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
        DataLoopNode::Node(WaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp;
        WaterHeaterDesuperheater(DesuperheaterNum).Mode = WaterHeaterDesuperheater(DesuperheaterNum).SaveMode;

        Real64 HEffFTemp;
        if (WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp > 0) {
            HEffFTemp = max(0.0, CurveManager::CurveValue(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp, TankTemp, DataEnvironment::OutDryBulbTemp));
        } else {
            HEffFTemp = 1.0;
        }

        // set limits on heat recovery efficiency
        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCONDENSER_REFRIGERATION) {
            if ((HEffFTemp * Effic) > 0.9) HEffFTemp = 0.9 / Effic;
        } else { // max is 0.3 for all other sources
            if ((HEffFTemp * Effic) > 0.3) HEffFTemp = 0.3 / Effic;
        } // setting limits on heat recovery efficiency

        // Access the appropriate structure to find the average heating capacity of the desuperheater heating coil
        Real64 AverageWasteHeat;
        if (WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType) {
            int SourceID = WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum;
            if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOMPRESSORRACK_REFRIGERATEDCASE) {
                // Refrigeration systems are solved outside the time step iteration, so the
                //  appropriate decrement for other waste heat applications is handled differently
                AverageWasteHeat = DataHeatBalance::HeatReclaimRefrigeratedRack(SourceID).AvailCapacity - DataHeatBalance::HeatReclaimRefrigeratedRack(SourceID).UsedHVACCoil;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = 1.0;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCONDENSER_REFRIGERATION) {
                AverageWasteHeat = DataHeatBalance::HeatReclaimRefrigCondenser(SourceID).AvailCapacity - DataHeatBalance::HeatReclaimRefrigCondenser(SourceID).UsedHVACCoil;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = 1.0;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_COOLING ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_MULTISPEED ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_MULTIMODE) {
                AverageWasteHeat = DataHeatBalance::HeatReclaimDXCoil(SourceID).AvailCapacity;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = DXCoils::DXCoil(SourceID).PartLoadRatio;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_VARIABLE_COOLING) {
                AverageWasteHeat = DataHeatBalance::HeatReclaimVS_DXCoil(SourceID).AvailCapacity;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = VariableSpeedCoils::VarSpeedCoil(SourceID).PartLoadRatio;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_AIR_WATER_HEATPUMP_EQ) {
                AverageWasteHeat = DataHeatBalance::HeatReclaimSimple_WAHPCoil(SourceID).AvailCapacity;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = WaterToAirHeatPumpSimple::SimpleWatertoAirHP(SourceID).PartLoadRatio;
            }
        } else {
            AverageWasteHeat = 0.0;
        }

        // simulate only water heater tank if reclaim heating source is off
        if (WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR == 0.0) {
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
            return;
        }

        // If the set point is higher than the maximum water temp, reset both the set point and the dead band temperature difference
        if (SetPointTemp > WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp) {
            Real64 CutInTemp = SetPointTemp - DeadBandTempDiff;
            SetPointTemp = WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp;
            DeadBandTempDiff = max(0.0, (SetPointTemp - CutInTemp));
        }

        Real64 Acc; // Accuracy of result from RegulaFalsi
        if (WaterHeaterDesuperheater(DesuperheaterNum).TankTypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
            Acc = 0.001;
        }
        else {
            Acc = 0.00001;
        }

        // set the water-side mass flow rate
        Real64 CpWater = Psychrometrics::CPHW(DataLoopNode::Node(WaterInletNode).Temp);
        Real64 MdotWater = WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate * Psychrometrics::RhoH2O(DataLoopNode::Node(WaterInletNode).Temp);
        Real64 QHeatRate = 0.0;
        if (DataLoopNode::Node(WaterInletNode).Temp <= WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp + Acc) {
            QHeatRate = ((AverageWasteHeat * Effic * HEffFTemp) / WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR) +
                        (WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower * WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater);
        }

        if (MdotWater > 0.0) {
            DataLoopNode::Node(WaterOutletNode).Temp = DataLoopNode::Node(WaterInletNode).Temp + QHeatRate / (MdotWater * CpWater);
        } else {
            DataLoopNode::Node(WaterOutletNode).Temp = DataLoopNode::Node(WaterInletNode).Temp;
        }

        // change to tanktypenum using parameters?
        Real64 PartLoadRatio =0.0;
        {
            auto const TankType(WaterHeaterDesuperheater(DesuperheaterNum).TankTypeNum);

            if (TankType == DataPlant::TypeOf_WtrHeaterMixed||TankType == DataPlant::TypeOf_WtrHeaterStratified) {

                WaterHeaterDesuperheater(DesuperheaterNum).SaveWHMode = WaterThermalTank(WaterThermalTankNum).Mode;

                {
                    auto const SELECT_CASE_var1(WaterHeaterDesuperheater(DesuperheaterNum).Mode);
                    if (SELECT_CASE_var1 == modHeatMode) {

                        PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR;

                        //         set the full load outlet temperature on the water heater source inlet node (init has already been called)
                        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(WaterOutletNode).Temp;

                        //         set the source mass flow rate for the tank
                        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater * PartLoadRatio;

                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = PartLoadRatio;
                        WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = QHeatRate * PartLoadRatio;
                        WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
                        Real64 NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                        if (NewTankTemp > SetPointTemp) {
                            //           Only revert to floating mode if the tank temperature is higher than the cut out temperature
                            if (NewTankTemp > WaterHeaterDesuperheater(DesuperheaterNum).SetPointTemp) {
                                WaterHeaterDesuperheater(DesuperheaterNum).Mode = modFloatMode;
                            }
                            Par(1) = SetPointTemp;
                            Par(2) = WaterHeaterDesuperheater(DesuperheaterNum).SaveWHMode;
                            Par(3) = WaterThermalTankNum;
                            if (FirstHVACIteration) {
                                Par(4) = 1.0;
                            } else {
                                Par(4) = 0.0;
                            }
                            Par(5) = MdotWater;
                            int SolFla;
                            std::string IterNum;
                            General::SolveRoot(Acc,
                                      MaxIte,
                                      SolFla,
                                      PartLoadRatio,
                                      PLRResidualWaterThermalTank,
                                      0.0,
                                      WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                      Par);
                            if (SolFla == -1) {
                                ObjexxFCL::gio::write(IterNum, fmtLD) << MaxIte;
                                strip(IterNum);
                                if (!DataGlobals::WarmupFlag) {
                                    ++WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum1;
                                    if (WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum1 == 1) {
                                        ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                         WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                        ShowContinueError(
                                            "Iteration limit exceeded calculating desuperheater unit part-load ratio, maximum iterations = " +
                                            IterNum + ". Part-load ratio returned = " + General::RoundSigDigits(PartLoadRatio, 3));
                                        ShowContinueErrorTimeStamp("This error occurred in heating mode.");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(
                                            WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                "\":  Iteration limit exceeded in heating mode warning continues. Part-load ratio statistics follow.",
                                            WaterHeaterDesuperheater(DesuperheaterNum).IterLimitErrIndex1,
                                            PartLoadRatio,
                                            PartLoadRatio);
                                    }
                                }
                            } else if (SolFla == -2) {
                                PartLoadRatio = max(
                                    0.0,
                                    min(WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR, (SetPointTemp - TankTemp) / (NewTankTemp - TankTemp)));
                                if (!DataGlobals::WarmupFlag) {
                                    ++WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum1;
                                    if (WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum1 == 1) {
                                        ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                         WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                        ShowContinueError("Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. "
                                                          "Part-load ratio used = " +
                                                          General::RoundSigDigits(PartLoadRatio, 3));
                                        ShowContinueError("Please send this information to the EnergyPlus support group.");
                                        ShowContinueErrorTimeStamp("This error occurred in heating mode.");
                                    } else {
                                        ShowRecurringWarningErrorAtEnd(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                                           WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                                           "\":  Part-load ratio calculation failed in heating mode warning "
                                                                           "continues. Part-load ratio statistics follow.",
                                                                       WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedIndex1,
                                                                       PartLoadRatio,
                                                                       PartLoadRatio);
                                    }
                                }
                            }
                            NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                        } else {
                            PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR;
                        }

                    } else if (SELECT_CASE_var1 == modFloatMode) {

                        //         check tank temperature by setting source inlet mass flow rate to zero
                        PartLoadRatio = 0.0;

                        //         set the full load outlet temperature on the water heater source inlet node (init has already been called)
                        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(WaterOutletNode).Temp;

                        //         check tank temperature by setting source inlet mass flow rate to zero
                        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;

                        //         disable the tank heater to find PLR of the HPWH
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = 0.0;
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = 0.0;
                        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = PartLoadRatio;
                        WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = QHeatRate * PartLoadRatio;
                        WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
                        Real64 NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                        if (NewTankTemp <= (SetPointTemp - DeadBandTempDiff)) {
                            WaterHeaterDesuperheater(DesuperheaterNum).Mode = modHeatMode;
                            WaterThermalTank(WaterThermalTankNum).Mode = WaterHeaterDesuperheater(DesuperheaterNum).SaveWHMode;
                            if ((TankTemp - NewTankTemp) != 0.0) {
                                PartLoadRatio = min(WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                                    max(0.0, ((SetPointTemp - DeadBandTempDiff) - NewTankTemp) / (TankTemp - NewTankTemp)));
                            } else {
                                PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR;
                            }

                            //           set the full load outlet temperature on the water heater source inlet node
                            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(WaterOutletNode).Temp;

                            //           set the source mass flow rate for the tank and enable backup heating element
                            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater * PartLoadRatio;
                            WaterThermalTank(WaterThermalTankNum).MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                            WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                            WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = PartLoadRatio;
                            WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = QHeatRate * PartLoadRatio;
                            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
                            NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                            if (NewTankTemp > SetPointTemp) {
                                Par(1) = SetPointTemp;
                                Par(2) = WaterHeaterDesuperheater(DesuperheaterNum).SaveWHMode;
                                Par(3) = WaterThermalTankNum;
                                if (FirstHVACIteration) {
                                    Par(4) = 1.0;
                                } else {
                                    Par(4) = 0.0;
                                }
                                Par(5) = MdotWater;
                                int SolFla;
                                std::string IterNum;
                                General::SolveRoot(Acc,
                                          MaxIte,
                                          SolFla,
                                          PartLoadRatio,
                                          PLRResidualWaterThermalTank,
                                          0.0,
                                          WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                          Par);
                                if (SolFla == -1) {
                                    ObjexxFCL::gio::write(IterNum, fmtLD) << MaxIte;
                                    strip(IterNum);
                                    if (!DataGlobals::WarmupFlag) {
                                        ++WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum2;
                                        if (WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum2 == 1) {
                                            ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                             WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                            ShowContinueError(
                                                "Iteration limit exceeded calculating desuperheater unit part-load ratio, maximum iterations = " +
                                                IterNum + ". Part-load ratio returned = " + General::RoundSigDigits(PartLoadRatio, 3));
                                            ShowContinueErrorTimeStamp("This error occurred in float mode.");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                                               WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                                               "\":  Iteration limit exceeded in float mode warning continues. "
                                                                               "Part-load ratio statistics follow.",
                                                                           WaterHeaterDesuperheater(DesuperheaterNum).IterLimitErrIndex2,
                                                                           PartLoadRatio,
                                                                           PartLoadRatio);
                                        }
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadRatio = max(0.0,
                                                        min(WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                                            (SetPointTemp - TankTemp) / (NewTankTemp - TankTemp)));
                                    if (!DataGlobals::WarmupFlag) {
                                        ++WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum2;
                                        if (WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum2 == 1) {
                                            ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                             WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                            ShowContinueError("Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. "
                                                              "Part-load ratio used = " +
                                                              General::RoundSigDigits(PartLoadRatio, 3));
                                            ShowContinueError("Please send this information to the EnergyPlus support group.");
                                            ShowContinueErrorTimeStamp("This error occurred in float mode.");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                                               WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                                               "\": Part-load ratio calculation failed in float mode warning "
                                                                               "continues. Part-load ratio statistics follow.",
                                                                           WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedIndex2,
                                                                           PartLoadRatio,
                                                                           PartLoadRatio);
                                        }
                                    }
                                }
                            }
                        } else {
                            WaterThermalTank(WaterThermalTankNum).MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                            WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                        }

                    } else {
                    }
                }

                //   should never get here, case is checked in GetWaterThermalTankInput
            } else {
                ShowFatalError("Coil:WaterHeating:Desuperheater = " + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                               ":  invalid water heater tank type and name entered = " + WaterHeaterDesuperheater(DesuperheaterNum).TankType + ", " +
                               WaterHeaterDesuperheater(DesuperheaterNum).TankName);
            }
        }

        if (QHeatRate == 0) PartLoadRatio = 0.0;

        DataLoopNode::Node(WaterOutletNode).MassFlowRate = MdotWater * PartLoadRatio;
        WaterHeaterDesuperheater(DesuperheaterNum).HEffFTempOutput = HEffFTemp;
        WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = QHeatRate * PartLoadRatio;
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater * PartLoadRatio;

        if (PartLoadRatio == 0) {
            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
            DataLoopNode::Node(WaterOutletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
            WaterHeaterDesuperheater(DesuperheaterNum).HEffFTempOutput = 0.0;
            WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = 0.0;
        }

        WaterHeaterDesuperheater(DesuperheaterNum).HeaterEnergy = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = PartLoadRatio;
        WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelRate = WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad * PartLoadRatio;
        WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelEnergy =
            WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad * (1 - PartLoadRatio);
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelEnergy =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        WaterHeaterDesuperheater(DesuperheaterNum).PumpPower = WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower * (PartLoadRatio);
        WaterHeaterDesuperheater(DesuperheaterNum).PumpEnergy = WaterHeaterDesuperheater(DesuperheaterNum).PumpPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // Update remaining waste heat (just in case multiple users of waste heat use same source)
        if (WaterHeaterDesuperheater(DesuperheaterNum).ValidSourceType) {
            int SourceID = WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum;
            if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOMPRESSORRACK_REFRIGERATEDCASE) {
                //    Refrigeration systems are simulated at the zone time step, do not decrement available capacity
                DataHeatBalance::HeatReclaimRefrigeratedRack(SourceID).UsedWaterHeater = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCONDENSER_REFRIGERATION) {
                DataHeatBalance::HeatReclaimRefrigCondenser(SourceID).UsedWaterHeater = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_COOLING ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_MULTISPEED ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_MULTIMODE) {
                DataHeatBalance::HeatReclaimDXCoil(SourceID).AvailCapacity -= WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_DX_VARIABLE_COOLING) {
                DataHeatBalance::HeatReclaimVS_DXCoil(SourceID).AvailCapacity -= WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == modCOIL_AIR_WATER_HEATPUMP_EQ) {
                DataHeatBalance::HeatReclaimSimple_WAHPCoil(SourceID).AvailCapacity -= WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
                DataHeatBalance::HeatReclaimSimple_WAHPCoil(SourceID).WaterHeatingDesuperheaterReclaimedHeat(DesuperheaterNum) = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            }
        }
    }

    void CalcHeatPumpWaterHeater(int const WaterThermalTankNum, // Water Heater tank being simulated
                                 bool const FirstHVACIteration  // TRUE if First iteration of simulation
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   March 2005
        //       MODIFIED       B. Griffith, Jan 2012 for stratified tank
        //						B. Shen 12/2014, add air-source variable-speed heat pump water heating
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates a heat pump water heater

        // METHODOLOGY EMPLOYED:
        // Simulate the water heater tank, DX coil, and fan to meet the water heating requirements.

        int const MaxIte(500);   // maximum number of iterations
        Real64 const Acc(0.001); // Accuracy of result from RegulaFalsi

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MdotWater;            // mass flow rate of condenser water, kg/s
        IntegratedHeatPump::IHPOperationMode IHPMode(IntegratedHeatPump::IHPOperationMode::IdleMode); // IHP working mode
        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0);

        // References to objects used in this function
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);
        HeatPumpWaterHeaterData &HeatPump = HPWaterHeater(Tank.HeatPumpNum);

        // FLOW:
        // initialize local variables
        int AvailSchedule = ScheduleManager::GetCurrentScheduleValue(HeatPump.AvailSchedPtr);
        int HPAirInletNode = HeatPump.HeatPumpAirInletNode;
        int HPAirOutletNode = HeatPump.HeatPumpAirOutletNode;
        int OutdoorAirNode = HeatPump.OutsideAirNode;
        int ExhaustAirNode = HeatPump.ExhaustAirNode;
        int HPWaterInletNode = HeatPump.CondWaterInletNode;
        int HPWaterOutletNode = HeatPump.CondWaterOutletNode;
        int InletAirMixerNode = HeatPump.InletAirMixerNode;
        int OutletAirSplitterNode = HeatPump.OutletAirSplitterNode;
        int DXCoilAirInletNode = HeatPump.DXCoilAirInletNode;
        modHPPartLoadRatio = 0.0;
        int CompOp = 0;                  // DX compressor operation; 1=on, 0=off
        HeatPump.OnCycParaFuelRate = 0.0;
        HeatPump.OnCycParaFuelEnergy = 0.0;
        HeatPump.OffCycParaFuelRate = 0.0;
        HeatPump.OffCycParaFuelEnergy = 0.0;
        DataLoopNode::Node(HPWaterOutletNode) = DataLoopNode::Node(HPWaterInletNode);
        int MaxSpeedNum = HeatPump.NumofSpeed;  // speed number of variable speed HPWH coil

        // assign set point temperature (cut-out) and dead band temp diff (cut-in = cut-out minus dead band temp diff)
        Real64 SetPointTemp = HeatPump.SetPointTemp;
        Real64 DeadBandTempDiff = HeatPump.DeadBandTempDiff;
        Real64 RhoWater = Psychrometrics::RhoH2O(SetPointTemp); // initialize

        // store first iteration tank temperature and HP mode of operation
        // this code can be called more than once with FirstHVACIteration = .TRUE., use FirstTimeThroughFlag to control save
        if (FirstHVACIteration && !DataHVACGlobals::ShortenTimeStepSys && HeatPump.FirstTimeThroughFlag) {
            Tank.SavedTankTemp = Tank.TankTemp;
            HeatPump.SaveMode = HeatPump.Mode;
            HeatPump.SaveWHMode = Tank.Mode;
            HeatPump.FirstTimeThroughFlag = false;
        }

        if (!FirstHVACIteration) HeatPump.FirstTimeThroughFlag = true;

        // check if HPWH is off for some reason and simulate HPWH air- and water-side mass flow rates of 0
        // simulate only water heater tank if HP compressor is scheduled off
        //   simulate only water heater tank if HP compressor cut-out temperature is lower than the tank's cut-in temperature
        //    simulate only water heater tank if HP inlet air temperature is below minimum temperature for HP compressor operation
        //    if the tank maximum temperature limit is less than the HPWH set point temp, disable HPWH
        if (AvailSchedule == 0.0 || (SetPointTemp - DeadBandTempDiff) <= Tank.SetPointTemp || DataHVACGlobals::HPWHInletDBTemp < HeatPump.MinAirTempForHPOperation ||
            DataHVACGlobals::HPWHInletDBTemp > HeatPump.MaxAirTempForHPOperation || SetPointTemp >= Tank.TankTempLimit ||
            (!HeatPump.AllowHeatingElementAndHeatPumpToRunAtSameTime && Tank.TypeNum == DataPlant::TypeOf_WtrHeaterMixed && Tank.SavedMode == modHeatMode) ||
            (!HeatPump.AllowHeatingElementAndHeatPumpToRunAtSameTime && Tank.TypeNum == DataPlant::TypeOf_WtrHeaterStratified &&
             (Tank.SavedHeaterOn1 || Tank.SavedHeaterOn2))) {
            //   revert to float mode any time HPWH compressor is OFF
            HeatPump.Mode = modFloatMode;
            if (InletAirMixerNode > 0) {
                DataLoopNode::Node(InletAirMixerNode) = DataLoopNode::Node(HPAirInletNode);
            }
            //   pass node info and simulate crankcase heater
            if (MaxSpeedNum > 0) {
                int VSCoilNum = HeatPump.DXCoilNum;

                if (HeatPump.bIsIHP) {
                    VSCoilNum = IntegratedHeatPump::IntegratedHeatPumps(VSCoilNum).SCWHCoilIndex;
                }
                // set the SCWH mode
                Real64 SpeedRatio = 1.0;  // speed ratio for interpolating between two speed levels
                int SpeedNum = 1;
                if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    if (HeatPump.bIsIHP)
                        VariableSpeedCoils::SimVariableSpeedCoils(
                                "", VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                    else
                        VariableSpeedCoils::SimVariableSpeedCoils(
                                HeatPump.DXCoilName, VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                } else {
                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    if (HeatPump.bIsIHP)
                        VariableSpeedCoils::SimVariableSpeedCoils(
                                "", VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                    else
                        VariableSpeedCoils::SimVariableSpeedCoils(
                                HeatPump.DXCoilName, VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                }

                // set the DWH mode
                if (HeatPump.bIsIHP) {
                    VSCoilNum = IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).DWHCoilIndex;

                    if (VSCoilNum > 0) // if DWH coil exists
                    {
                        if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                            }
                            SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                            VariableSpeedCoils::SimVariableSpeedCoils(
                                    "", VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                        } else {
                            SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                            VariableSpeedCoils::SimVariableSpeedCoils(
                                    "", VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                            }
                        }
                    }
                }

            } else {
                if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    DXCoils::SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, DataHVACGlobals::CycFanCycCoil, modHPPartLoadRatio);
                } else {
                    DXCoils::SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, DataHVACGlobals::CycFanCycCoil, modHPPartLoadRatio);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                }
            }

            if (OutletAirSplitterNode > 0) {
                DataLoopNode::Node(HPAirOutletNode) = DataLoopNode::Node(OutletAirSplitterNode);
            }

            //   Simulate tank if HP compressor unavailable for water heating
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);

            //   If HPWH compressor is available and unit is off for another reason, off-cycle parasitics are calculated
            if (AvailSchedule != 0) {
                HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * (1.0 - modHPPartLoadRatio);
                HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            }

            //   Warn if HPWH compressor cut-in temperature is less than the water heater tank's set point temp
            if (!DataGlobals::WarmupFlag && !DataGlobals::DoingSizing && !DataGlobals::KickOffSimulation) {
                if ((SetPointTemp - DeadBandTempDiff) <= Tank.SetPointTemp) {
                    Real64 HPMinTemp = SetPointTemp - DeadBandTempDiff;
                    std::string HPMinTempChar;
                    ObjexxFCL::gio::write(HPMinTempChar, fmtLD) << HPMinTemp;
                    ++HeatPump.HPSetPointError;
                    //  add logic for warmup, DataGlobals::KickOffSimulation and doing sizing here
                    if (HeatPump.HPSetPointError == 1) {
                        ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name +
                                         ":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the heat "
                                         "pump water heater. Heat Pump will be disabled and simulation continues.");
                        ShowContinueErrorTimeStamp(" ...Heat Pump cut-in temperature=" + HPMinTempChar);
                    } else {
                        ShowRecurringWarningErrorAtEnd(HeatPump.Type + " \"" + HeatPump.Name +
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
        Real64 TankTemp = Tank.SavedTankTemp;
        HeatPump.Mode = HeatPump.SaveMode;

        RhoWater = Psychrometrics::RhoH2O(TankTemp); // update water density using tank temp

        // set the heat pump air- and water-side mass flow rate
        MdotWater = HeatPump.OperatingWaterFlowRate * Psychrometrics::RhoH2O(TankTemp);

        // Select mode of operation (float mode or heat mode) from last iteration.
        // Determine if heating will occur this iteration and get an estimate of the PLR
        if (HeatPump.Mode == modHeatMode) {
            // HPWH was heating last iteration and will continue to heat until the set point is reached
            modHPPartLoadRatio = 1.0;
            if (TankTemp > SetPointTemp) { // tank set point temp may have been reduced since last iteration and float mode may be needed
                HeatPump.Mode = modFloatMode;
                modHPPartLoadRatio = 0.0;
                // check to see if HP needs to operate
                // set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
                {
                    auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                    if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                        DataLoopNode::Node(HPWaterInletNode).Temp = TankTemp;
                        DataLoopNode::Node(HPWaterOutletNode).Temp = TankTemp;
                    } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                        DataLoopNode::Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                        DataLoopNode::Node(HPWaterOutletNode).Temp = Tank.SourceInletTemp;
                    }
                }
                DataLoopNode::Node(HPWaterInletNode).MassFlowRate = 0.0;
                DataLoopNode::Node(HPWaterOutletNode).MassFlowRate = 0.0;

                // Check tank temperature by setting source inlet mass flow rate to zero.
                modHPPartLoadRatio = 0.0;

                // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
                Tank.SourceInletTemp = DataLoopNode::Node(HPWaterOutletNode).Temp;

                // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
                Tank.MaxCapacity = 0.0;
                Tank.MinCapacity = 0.0;
                Tank.SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
                Real64 SourceEffectivenessBackup = Tank.SourceEffectiveness;
                Tank.SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
                Tank.SourceEffectiveness = SourceEffectivenessBackup;
                Real64 NewTankTemp = Tank.GetHPWHSensedTankTemp();

                // Reset the tank's internal heating element capacity.
                Tank.MaxCapacity = HeatPump.BackupElementCapacity;
                Tank.MinCapacity = HeatPump.BackupElementCapacity;

                // Check to see if the tank drifts below set point if no heating happens.
                if (NewTankTemp <= (SetPointTemp - DeadBandTempDiff)) {

                    // HPWH is now in heating mode
                    HeatPump.Mode = modHeatMode;

                    // Reset the water heater's mode (call above may have changed modes)
                    Tank.Mode = HeatPump.SaveWHMode;

                    modHPPartLoadRatio = 1.0;
                }
            } else { // or use side nodes may meet set point without need for heat pump compressor operation
                // check to see if HP needs to operate
                {
                    auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                    if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                        DataLoopNode::Node(HPWaterInletNode).Temp = TankTemp;
                        DataLoopNode::Node(HPWaterOutletNode).Temp = TankTemp;
                    } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                        DataLoopNode::Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                        DataLoopNode::Node(HPWaterOutletNode).Temp = Tank.SourceInletTemp;
                    }
                }
                // Check tank temperature by setting source inlet mass flow rate to zero.
                DataLoopNode::Node(HPWaterInletNode).MassFlowRate = 0.0;
                DataLoopNode::Node(HPWaterOutletNode).MassFlowRate = 0.0;

                modHPPartLoadRatio = 0.0;

                // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
                Tank.SourceInletTemp = DataLoopNode::Node(HPWaterOutletNode).Temp;

                // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
                Tank.MaxCapacity = 0.0;
                Tank.MinCapacity = 0.0;
                Tank.SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
                Real64 SourceEffectivenessBackup = Tank.SourceEffectiveness;
                Tank.SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
                Tank.SourceEffectiveness = SourceEffectivenessBackup;
                Real64 NewTankTemp = Tank.GetHPWHSensedTankTemp();

                // Reset the tank's internal heating element capacity.
                Tank.MaxCapacity = HeatPump.BackupElementCapacity;
                Tank.MinCapacity = HeatPump.BackupElementCapacity;

                // Check to see if the tank meets set point if no heating happens.
                if (NewTankTemp > SetPointTemp) {

                    // HPWH is now in floating mode
                    HeatPump.Mode = modFloatMode;

                } else {

                    // HPWH remains in heating mode
                    modHPPartLoadRatio = 1.0;
                }

                // Reset the water heater's mode (call above may have changed modes)
                Tank.Mode = HeatPump.SaveWHMode;
            }
        } else {
            assert(HeatPump.Mode == modFloatMode);
            // HPWH was floating last iteration and will continue to float until the cut-in temperature is reached

            // set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
            {
                auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                    DataLoopNode::Node(HPWaterInletNode).Temp = TankTemp;
                    DataLoopNode::Node(HPWaterOutletNode).Temp = TankTemp;
                } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                    DataLoopNode::Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                    DataLoopNode::Node(HPWaterOutletNode).Temp = Tank.SourceInletTemp;
                }
            }
            DataLoopNode::Node(HPWaterInletNode).MassFlowRate = 0.0;
            DataLoopNode::Node(HPWaterOutletNode).MassFlowRate = 0.0;

            // Check tank temperature by setting source inlet mass flow rate to zero.
            modHPPartLoadRatio = 0.0;

            // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
            Tank.SourceInletTemp = DataLoopNode::Node(HPWaterOutletNode).Temp;

            // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
            Tank.MaxCapacity = 0.0;
            Tank.MinCapacity = 0.0;
            Tank.SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
            Real64 SourceEffectivenessBackup = Tank.SourceEffectiveness;
            Tank.SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
            Tank.SourceEffectiveness = SourceEffectivenessBackup;
            Real64 NewTankTemp = Tank.GetHPWHSensedTankTemp();

            // Reset the tank's internal heating element capacity.
            Tank.MaxCapacity = HeatPump.BackupElementCapacity;
            Tank.MinCapacity = HeatPump.BackupElementCapacity;

            // Check to see if the tank drifts below set point if no heating happens.
            if (NewTankTemp <= (SetPointTemp - DeadBandTempDiff)) {

                // HPWH is now in heating mode
                HeatPump.Mode = modHeatMode;

                // Reset the water heater's mode (call above may have changed modes)
                Tank.Mode = HeatPump.SaveWHMode;

                modHPPartLoadRatio = 1.0;
            }
        }

        if (HeatPump.bIsIHP) // mark the water heating call, if existing
        {
            if (IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                if (1 == HeatPump.Mode)
                    IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).IsWHCallAvail = true;
                else
                    IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).IsWHCallAvail = false;
            }
        }

        // If the HPWH was in heating mode during the last DataGlobals::TimeStep or if it was determined that
        // heating would be needed during this DataGlobals::TimeStep to maintain setpoint, do the heating calculation.
        int SpeedNum = 0;
        Real64 SpeedRatio = 0.0;
        if (HeatPump.Mode == modHeatMode) {

            // set up air flow on DX coil inlet node
            DataLoopNode::Node(DXCoilAirInletNode).MassFlowRate = modMdotAir * modHPPartLoadRatio;

            // set the condenser inlet node mass flow rate prior to calling the DXCoils::CalcHPWHDXCoil
            DataLoopNode::Node(HPWaterInletNode).MassFlowRate = MdotWater * modHPPartLoadRatio;
            Tank.SourceMassFlowRate = MdotWater * modHPPartLoadRatio;

            // Do the coil and tank calculations at full PLR to see if it overshoots setpoint.
            bool bIterSpeed = false;
            if (MaxSpeedNum > 0) { // lowest speed of VS HPWH coil
                SpeedRatio = 1.0;
                modHPPartLoadRatio = 1.0;
                bIterSpeed = true; // prepare for iterating between speed levels
                SpeedNum = 1;
                SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                if (HeatPump.bIsIHP) {
                    bIterSpeed = false; // don't iterate speed unless match conditions below
                    IHPMode = IntegratedHeatPump::GetCurWorkMode(HeatPump.DXCoilNum);

                    if (IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                        int VSCoilNum;
                        if (IntegratedHeatPump::IHPOperationMode::DWHMode == IHPMode) {
                            VSCoilNum = IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).DWHCoilIndex;
                            IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IntegratedHeatPump::IHPOperationMode::DWHMode;
                        } else {
                            VSCoilNum = IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).SCWHCoilIndex;
                            IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode;
                        }

                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                        VariableSpeedCoils::SimVariableSpeedCoils(
                                "", VSCoilNum, DataHVACGlobals::CycFanCycCoil, EMP1, EMP2, EMP3, 1, modHPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);

                        IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IHPMode;
                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    } else {
                        SpeedNum = IntegratedHeatPump::GetLowSpeedNumIHP(HeatPump.DXCoilNum);

                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                        IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                                   HeatPump.DXCoilNum,
                                                   DataHVACGlobals::CycFanCycCoil,
                                                   EMP1,
                                                   EMP2,
                                                   EMP3,
                                                   1,
                                                   modHPPartLoadRatio,
                                                   SpeedNum,
                                                   SpeedRatio,
                                                   0.0,
                                                   0.0,
                                                   true,
                                                   false,
                                                   1.0);

                        if ((IntegratedHeatPump::IHPOperationMode::SCWHMatchWHMode == IHPMode) || (IntegratedHeatPump::IHPOperationMode::DWHMode == IHPMode)) {
                            bIterSpeed = true;
                        } else {
                            Tank.SourceMassFlowRate = IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).TankSourceWaterMassFlowRate;
                            MdotWater = Tank.SourceMassFlowRate;
                        }

                        if (IntegratedHeatPump::IHPOperationMode::SHDWHElecHeatOffMode == IHPMode) // turn off heater element
                        {
                            Tank.MaxCapacity = 0.0;
                            Tank.MinCapacity = 0.0;
                        }
                    }
                } else {
                    VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              1,
                                                              modHPPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                }

                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
            } else {
                ConvergeSingleSpeedHPWHCoilAndTank(WaterThermalTankNum, modHPPartLoadRatio);
            }

            Real64 NewTankTemp = Tank.GetHPWHSensedTankTemp();
            Real64 LowSpeedTankTemp = NewTankTemp;
            Real64 HPWHCondInletNodeLast = DataLoopNode::Node(HPWaterInletNode).Temp;

            Array1D<Real64> Par(5);      // Parameters passed to RegulaFalsi
            if (NewTankTemp > SetPointTemp) {
                HeatPump.Mode = modFloatMode;
                Par(1) = SetPointTemp;
                Par(2) = HeatPump.SaveWHMode;
                Par(3) = WaterThermalTankNum;
                if (FirstHVACIteration) {
                    Par(4) = 1.0;
                } else {
                    Par(4) = 0.0;
                }
                Par(5) = MdotWater;

                Real64 zeroResidual = 1.0;
                if (MaxSpeedNum > 0) {
                    // square the solving, and avoid warning
                    // due to very small capacity at lowest speed of VSHPWH coil
                    if (bIterSpeed)
                        zeroResidual = PLRResidualHPWH(0.0, Par);
                    else
                        zeroResidual = -1.0;
                }

                if (zeroResidual > 0.0) { // then iteration
                    int SolFla;
                    General::SolveRoot(Acc, MaxIte, SolFla, modHPPartLoadRatio, PLRResidualHPWH, 0.0, 1.0, Par);
                    if (SolFla == -1) {
                        std::string IterNum;
                        ObjexxFCL::gio::write(IterNum, fmtLD) << MaxIte;
                        strip(IterNum);
                        if (!DataGlobals::WarmupFlag) {
                            ++HeatPump.IterLimitExceededNum2;
                            if (HeatPump.IterLimitExceededNum2 == 1) {
                                ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                ShowContinueError(
                                    "Iteration limit exceeded calculating heat pump water heater compressor part-load ratio, maximum iterations = " +
                                    IterNum + ". Part-load ratio returned = " + General::RoundSigDigits(modHPPartLoadRatio, 3));
                                ShowContinueErrorTimeStamp("This error occurred in float mode.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    HeatPump.Type + " \"" + HeatPump.Name +
                                        "\":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.",
                                    HeatPump.IterLimitErrIndex2,
                                    modHPPartLoadRatio,
                                    modHPPartLoadRatio);
                            }
                        }
                    } else if (SolFla == -2) {
                        modHPPartLoadRatio = max(0.0, min(1.0, (SetPointTemp - TankTemp) / (NewTankTemp - TankTemp)));
                        if (!DataGlobals::WarmupFlag) {
                            ++HeatPump.RegulaFalsiFailedNum2;
                            if (HeatPump.RegulaFalsiFailedNum2 == 1) {
                                ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                ShowContinueError("Heat pump water heater compressor part-load ratio calculation failed: PLR limits of 0 to 1 "
                                                  "exceeded. Part-load ratio used = " +
                                                  General::RoundSigDigits(modHPPartLoadRatio, 3));
                                ShowContinueError("Please send this information to the EnergyPlus support group.");
                                ShowContinueErrorTimeStamp("This error occurred in float mode.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    HeatPump.Type + " \"" + HeatPump.Name +
                                        "\": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.",
                                    HeatPump.RegulaFalsiFailedIndex2,
                                    modHPPartLoadRatio,
                                    modHPPartLoadRatio);
                            }
                        }
                    }
                } else {
                    modHPPartLoadRatio = 0.0;
                }

                // Re-calculate the HPWH Coil to get the correct heat transfer rate.
                DataLoopNode::Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                if (MaxSpeedNum > 0) {
                    SpeedRatio = 1.0;
                    SpeedNum = 1;

                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                    if (HeatPump.bIsIHP) {
                        if (bIterSpeed) {
                            IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                                       HeatPump.DXCoilNum,
                                                       DataHVACGlobals::CycFanCycCoil,
                                                       EMP1,
                                                       EMP2,
                                                       EMP3,
                                                       1,
                                                       modHPPartLoadRatio,
                                                       SpeedNum,
                                                       SpeedRatio,
                                                       0.0,
                                                       0.0,
                                                       true,
                                                       false,
                                                       1.0);
                        }
                    } else {
                        VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                                  HeatPump.DXCoilNum,
                                                                  DataHVACGlobals::CycFanCycCoil,
                                                                  EMP1,
                                                                  EMP2,
                                                                  EMP3,
                                                                  1,
                                                                  modHPPartLoadRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  0.0,
                                                                  0.0,
                                                                  1.0);
                    }

                    bIterSpeed = false;

                } else {
                    DXCoils::CalcHPWHDXCoil(HeatPump.DXCoilNum, modHPPartLoadRatio);
                }
            } else if (bIterSpeed) {
                for (int loopIter = 1; loopIter <= 4; ++loopIter) {
                    HeatPump.Mode = modHeatMode; // modHeatMode is important for system convergence
                    modHPPartLoadRatio = 1.0;
                    SpeedRatio = 1.0;
                    int LowSpeedNum = 2;
                    if (HeatPump.bIsIHP) {
                        LowSpeedNum = IntegratedHeatPump::GetLowSpeedNumIHP(HeatPump.DXCoilNum);
                        MaxSpeedNum = IntegratedHeatPump::GetMaxSpeedNumIHP(HeatPump.DXCoilNum);
                    }

                    for (int i = LowSpeedNum; i <= MaxSpeedNum; ++i) {
                        SpeedNum = i;
                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                        if (HeatPump.bIsIHP) {
                            IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                                       HeatPump.DXCoilNum,
                                                       DataHVACGlobals::CycFanCycCoil,
                                                       EMP1,
                                                       EMP2,
                                                       EMP3,
                                                       1,
                                                       modHPPartLoadRatio,
                                                       SpeedNum,
                                                       SpeedRatio,
                                                       0.0,
                                                       0.0,
                                                       true,
                                                       false,
                                                       1.0);
                        } else {
                            VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                                      HeatPump.DXCoilNum,
                                                                      DataHVACGlobals::CycFanCycCoil,
                                                                      EMP1,
                                                                      EMP2,
                                                                      EMP3,
                                                                      1,
                                                                      modHPPartLoadRatio,
                                                                      SpeedNum,
                                                                      SpeedRatio,
                                                                      0.0,
                                                                      0.0,
                                                                      1.0);
                        }

                        // HPWH condenser water temperature difference
                        Real64 CondenserDeltaT = DataLoopNode::Node(HPWaterOutletNode).Temp - DataLoopNode::Node(HPWaterInletNode).Temp;

                        //           move the full load outlet temperature rate to the water heater structure variables
                        //           (water heaters source inlet node temperature/mdot are set in Init, set it here after DXCoils::CalcHPWHDXCoil has been
                        //           called)
                        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(HPWaterInletNode).Temp + CondenserDeltaT;
                        //				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

                        //           this CALL does not update node temps, must use WaterThermalTank variables
                        // select tank type
                        {
                            auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                            if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankMixed(WaterThermalTankNum);
                                NewTankTemp = Tank.TankTemp;
                            } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankStratified(WaterThermalTankNum);
                                NewTankTemp = Tank.FindStratifiedTankSensedTemp();
                            }
                        }

                        if (NewTankTemp > SetPointTemp) {
                            SpeedNum = i;
                            break;
                        } else {
                            LowSpeedTankTemp = NewTankTemp;
                        }
                    }

                    Array1D<Real64> ParVS(10);   // Parameters passed to RegulaFalsi, for variable-speed HPWH
                    if (NewTankTemp > SetPointTemp) {
                        ParVS(1) = WaterThermalTankNum;
                        ParVS(2) = Tank.HeatPumpNum;
                        ParVS(3) = SpeedNum;
                        ParVS(4) = HPWaterInletNode;
                        ParVS(5) = HPWaterOutletNode;
                        ParVS(6) = RhoWater;
                        ParVS(7) = SetPointTemp;
                        ParVS(8) = HeatPump.SaveWHMode;
                        if (FirstHVACIteration) {
                            ParVS(9) = 1.0;
                        } else {
                            ParVS(9) = 0.0;
                        }

                        int SolFla;
                        std::string IterNum;
                        General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, PLRResidualIterSpeed, 1.0e-10, 1.0, ParVS);

                        if (SolFla == -1) {
                            ObjexxFCL::gio::write(IterNum, fmtLD) << MaxIte;
                            strip(IterNum);
                            if (!DataGlobals::WarmupFlag) {
                                ++HeatPump.IterLimitExceededNum1;
                                if (HeatPump.IterLimitExceededNum1 == 1) {
                                    ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                    ShowContinueError("Iteration limit exceeded calculating heat pump water heater speed"
                                                      " speed ratio ratio, maximum iterations = " +
                                                      IterNum + ". speed ratio returned = " + General::RoundSigDigits(SpeedRatio, 3));
                                    ShowContinueErrorTimeStamp("This error occurred in heating mode.");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(
                                        HeatPump.Type + " \"" + HeatPump.Name +
                                            "\":  Iteration limit exceeded in heating mode warning continues. speed ratio statistics follow.",
                                        HeatPump.IterLimitErrIndex1,
                                        SpeedRatio,
                                        SpeedRatio);
                                }
                            }
                        } else if (SolFla == -2) {
                            SpeedRatio = max(0.0, min(1.0, (SetPointTemp - LowSpeedTankTemp) / (NewTankTemp - LowSpeedTankTemp)));
                            if (!DataGlobals::WarmupFlag) {
                                ++HeatPump.RegulaFalsiFailedNum1;
                                if (HeatPump.RegulaFalsiFailedNum1 == 1) {
                                    ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                    ShowContinueError("Heat pump water heater speed ratio calculation failed: speed ratio limits "
                                                      "of 0 to 1 exceeded. speed ratio used = " +
                                                      General::RoundSigDigits(SpeedRatio, 3));
                                    ShowContinueError("Please send this information to the EnergyPlus support group.");
                                    ShowContinueErrorTimeStamp("This error occurred in heating mode.");
                                } else {
                                    ShowRecurringWarningErrorAtEnd(
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

                    modHPPartLoadRatio = 1.0;
                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                    if (HeatPump.bIsIHP) {
                        IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                                   HeatPump.DXCoilNum,
                                                   DataHVACGlobals::CycFanCycCoil,
                                                   EMP1,
                                                   EMP2,
                                                   EMP3,
                                                   1,
                                                   modHPPartLoadRatio,
                                                   SpeedNum,
                                                   SpeedRatio,
                                                   0.0,
                                                   0.0,
                                                   true,
                                                   false,
                                                   1.0);
                    } else {
                        VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                                  HeatPump.DXCoilNum,
                                                                  DataHVACGlobals::CycFanCycCoil,
                                                                  EMP1,
                                                                  EMP2,
                                                                  EMP3,
                                                                  1,
                                                                  modHPPartLoadRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  0.0,
                                                                  0.0,
                                                                  1.0);
                    }

                    // HPWH condenser water temperature difference
                    Real64 CondenserDeltaT = DataLoopNode::Node(HPWaterOutletNode).Temp - DataLoopNode::Node(HPWaterInletNode).Temp;

                    //           move the full load outlet temperature rate to the water heater structure variables
                    //           (water heaters source inlet node temperature/mdot are set in Init, set it here after DXCoils::CalcHPWHDXCoil has been called)
                    WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(HPWaterInletNode).Temp + CondenserDeltaT;
                    //				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

                    //           this CALL does not update node temps, must use WaterThermalTank variables
                    // select tank type
                    {
                        auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                        if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankMixed(WaterThermalTankNum);
                            NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                        } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankStratified(WaterThermalTankNum);
                            NewTankTemp = WaterThermalTank(WaterThermalTankNum).FindStratifiedTankSensedTemp();
                        }
                    }
                    // update inlet temp
                    DataLoopNode::Node(HPWaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
                    if (std::abs(DataLoopNode::Node(HPWaterInletNode).Temp - HPWHCondInletNodeLast) < DataHVACGlobals::SmallTempDiff) break;
                    HPWHCondInletNodeLast = DataLoopNode::Node(HPWaterInletNode).Temp;
                }

            } else {
                // Set the PLR to 1 if we're not going to reach setpoint during this DataGlobals::TimeStep.
                modHPPartLoadRatio = 1.0;
            }
        }

        if (HeatPump.bIsIHP) {
            if (IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                IntegratedHeatPump::ClearCoils(HeatPump.DXCoilNum); // clear node info when checking the heating load
            }
        }

        // set air-side mass flow rate for final calculation
        if (InletAirMixerNode > 0) {
            DataLoopNode::Node(InletAirMixerNode).MassFlowRate = modMdotAir * modHPPartLoadRatio;
            DataLoopNode::Node(HPAirInletNode).MassFlowRate = modMdotAir * modHPPartLoadRatio * (1.0 - modMixerInletAirSchedule);
            DataLoopNode::Node(OutdoorAirNode).MassFlowRate = modMdotAir * modHPPartLoadRatio * modMixerInletAirSchedule;
            //   IF HPWH is off, pass zone node conditions through HPWH air-side
            if (modHPPartLoadRatio == 0) DataLoopNode::Node(InletAirMixerNode) = DataLoopNode::Node(HPAirInletNode);
        } else {
            if (OutdoorAirNode == 0) {
                DataLoopNode::Node(HPAirInletNode).MassFlowRate = modMdotAir * modHPPartLoadRatio;
            } else {
                DataLoopNode::Node(OutdoorAirNode).MassFlowRate = modMdotAir * modHPPartLoadRatio;
            }
        }
        if (modHPPartLoadRatio == 0) Tank.SourceInletTemp = Tank.SourceOutletTemp;

        // set water-side mass flow rate for final calculation
        DataLoopNode::Node(HPWaterInletNode).MassFlowRate = MdotWater * modHPPartLoadRatio;

        if (MaxSpeedNum > 0) {

            // it is important to use MdotAir to reset the notes, otherwise, could fail to converge
            if (InletAirMixerNode > 0) {
                DataLoopNode::Node(InletAirMixerNode).MassFlowRateMax = modMdotAir;
                DataLoopNode::Node(InletAirMixerNode).MassFlowRateMaxAvail = modMdotAir;
            } else {
                if (OutdoorAirNode == 0) {
                    DataLoopNode::Node(HPAirInletNode).MassFlowRateMax = modMdotAir;
                    DataLoopNode::Node(HPAirInletNode).MassFlowRateMaxAvail = modMdotAir;
                } else {
                    DataLoopNode::Node(OutdoorAirNode).MassFlowRateMax = modMdotAir;
                    DataLoopNode::Node(OutdoorAirNode).MassFlowRateMaxAvail = modMdotAir;
                }
            }

            //   set the max mass flow rate for outdoor fans
            DataLoopNode::Node(HeatPump.FanOutletNode).MassFlowRateMax = modMdotAir;

            if (HeatPump.bIsIHP) {
                // pass node information using resulting PLR
                if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                    //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                               HeatPump.DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               EMP1,
                                               EMP2,
                                               EMP3,
                                               CompOp,
                                               modHPPartLoadRatio,
                                               SpeedNum,
                                               SpeedRatio,
                                               0.0,
                                               0.0,
                                               true,
                                               false,
                                               1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                               HeatPump.DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               EMP1,
                                               EMP2,
                                               EMP3,
                                               CompOp,
                                               modHPPartLoadRatio,
                                               SpeedNum,
                                               SpeedRatio,
                                               0.0,
                                               0.0,
                                               true,
                                               false,
                                               1.0);
                } else {
                    //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                    IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                               HeatPump.DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               EMP1,
                                               EMP2,
                                               EMP3,
                                               CompOp,
                                               modHPPartLoadRatio,
                                               SpeedNum,
                                               SpeedRatio,
                                               0.0,
                                               0.0,
                                               true,
                                               false,
                                               1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    IntegratedHeatPump::SimIHP(HeatPump.DXCoilName,
                                               HeatPump.DXCoilNum,
                                               DataHVACGlobals::CycFanCycCoil,
                                               EMP1,
                                               EMP2,
                                               EMP3,
                                               CompOp,
                                               modHPPartLoadRatio,
                                               SpeedNum,
                                               SpeedRatio,
                                               0.0,
                                               0.0,
                                               true,
                                               false,
                                               1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                }
            } else {
                // pass node information using resulting PLR
                if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                    //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              CompOp,
                                                              modHPPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              CompOp,
                                                              modHPPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                } else {
                    //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                    VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              CompOp,
                                                              modHPPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    VariableSpeedCoils::SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                              HeatPump.DXCoilNum,
                                                              DataHVACGlobals::CycFanCycCoil,
                                                              EMP1,
                                                              EMP2,
                                                              EMP3,
                                                              CompOp,
                                                              modHPPartLoadRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              0.0,
                                                              0.0,
                                                              1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                }
            }
        } else { // single speed

            // pass node information using resulting PLR
            if (HeatPump.FanPlacement == DataHVACGlobals::BlowThru) {
                //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                DXCoils::SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, DataHVACGlobals::CycFanCycCoil, modHPPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                DXCoils::SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, DataHVACGlobals::CycFanCycCoil, modHPPartLoadRatio);
            } else {
                //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                DXCoils::SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, DataHVACGlobals::CycFanCycCoil, modHPPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                DXCoils::SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, DataHVACGlobals::CycFanCycCoil, modHPPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
            }
        }

        // Call the tank one more time with the final PLR
        if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankMixed(WaterThermalTankNum);
        } else if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankStratified(WaterThermalTankNum);
        } else {
            assert(0);
        }

        // set HPWH outlet node equal to the outlet air splitter node conditions if outlet air splitter node exists
        if (OutletAirSplitterNode > 0) {
            DataLoopNode::Node(HPAirOutletNode) = DataLoopNode::Node(OutletAirSplitterNode);
            DataLoopNode::Node(ExhaustAirNode) = DataLoopNode::Node(OutletAirSplitterNode);
        }

        // Check schedule to divert air-side cooling to outdoors.
        if (HeatPump.OutletAirSplitterSchPtr > 0) {
            Real64 OutletAirSplitterSch = ScheduleManager::GetCurrentScheduleValue(HeatPump.OutletAirSplitterSchPtr);
            DataLoopNode::Node(HPAirOutletNode).MassFlowRate = modMdotAir * modHPPartLoadRatio * (1.0 - OutletAirSplitterSch);
            DataLoopNode::Node(ExhaustAirNode).MassFlowRate = modMdotAir * modHPPartLoadRatio * OutletAirSplitterSch;
        }

        HeatPump.HeatingPLR = modHPPartLoadRatio;
        HeatPump.OnCycParaFuelRate = HeatPump.OnCycParaLoad * modHPPartLoadRatio;
        HeatPump.OnCycParaFuelEnergy = HeatPump.OnCycParaFuelRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * (1.0 - modHPPartLoadRatio);
        HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
            HeatPump.ControlTempAvg = Tank.TankTempAvg;
            HeatPump.ControlTempFinal = Tank.TankTemp;
        } else if (HeatPump.TankTypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
            HeatPump.ControlTempAvg = Tank.FindStratifiedTankSensedTemp(true);
            HeatPump.ControlTempFinal = Tank.FindStratifiedTankSensedTemp();
        } else {
            assert(0);
        }

        {
            auto const SELECT_CASE_var(HeatPump.InletAirConfiguration);

            //   no sensible capacity to zone for outdoor and scheduled HPWH
            if (SELECT_CASE_var == modAmbientTempOutsideAir) {
                HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
                HeatPump.HPWaterHeaterLatentCapacity = 0.0;

            } else if (SELECT_CASE_var == modAmbientTempSchedule) {
                HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
                HeatPump.HPWaterHeaterLatentCapacity = 0.0;

                //   calculate sensible capacity to zone for inlet air configuration equals Zone Only or Zone And Outdoor Air configurations
            } else {
                Real64 CpAir = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(HPAirInletNode).HumRat, DataLoopNode::Node(HPAirInletNode).Temp);

                //     add parasitics to zone heat balance if parasitic heat load is to zone otherwise neglect parasitics
                if (HeatPump.ParasiticTempIndicator == modAmbientTempZone) {
                    HeatPump.HPWaterHeaterSensibleCapacity =
                        (DataLoopNode::Node(HPAirOutletNode).MassFlowRate * CpAir * (DataLoopNode::Node(HPAirOutletNode).Temp - DataLoopNode::Node(HPAirInletNode).Temp)) +
                        HeatPump.OnCycParaFuelRate + HeatPump.OffCycParaFuelRate;
                } else {
                    HeatPump.HPWaterHeaterSensibleCapacity =
                        DataLoopNode::Node(HPAirOutletNode).MassFlowRate * CpAir * (DataLoopNode::Node(HPAirOutletNode).Temp - DataLoopNode::Node(HPAirInletNode).Temp);
                }

                HeatPump.HPWaterHeaterLatentCapacity =
                    DataLoopNode::Node(HPAirOutletNode).MassFlowRate * (DataLoopNode::Node(HPAirOutletNode).HumRat - DataLoopNode::Node(HPAirInletNode).HumRat);
            }
        }
    }

    void WaterThermalTankData::CalcWaterThermalTank(int const WaterThermalTankNum)
    {
        if (this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
           this->CalcWaterThermalTankMixed(WaterThermalTankNum);
        } else if (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
            this->CalcWaterThermalTankStratified(WaterThermalTankNum);
        } else {
            assert(false);
        }
    }

    Real64 WaterThermalTankData::GetHPWHSensedTankTemp()
    {
        if (this->TypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
            return this->TankTemp;
        } else {
            assert(this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified);
            return this->FindStratifiedTankSensedTemp();
        }
    }

    void ConvergeSingleSpeedHPWHCoilAndTank(int const WaterThermalTankNum, // Index of WaterThermalTank
                                            Real64 const PartLoadRatio     // Part Load Ratio of the Coil
    )
    {
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);
        HeatPumpWaterHeaterData &HPWH = HPWaterHeater(Tank.HeatPumpNum);
        DXCoils::DXCoilData &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);

        Real64 PrevTankTemp = Tank.SourceOutletTemp;
        for (int i = 1; i <= 10; ++i) {

            DXCoils::CalcHPWHDXCoil(HPWH.DXCoilNum, PartLoadRatio);
            Tank.SourceInletTemp = DataLoopNode::Node(HPWH.CondWaterOutletNode).Temp;

            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
            DataLoopNode::Node(Coil.WaterInNode).Temp = Tank.SourceOutletTemp;

            if (std::abs(Tank.SourceOutletTemp - PrevTankTemp) < DataHVACGlobals::SmallTempDiff) {
                break;
            }

            PrevTankTemp = Tank.SourceOutletTemp;
        }
    }

    void SetVSHPWHFlowRates(int const WaterThermalTankNum, // Water Heater tank being simulated
                            int const HPNum,               // index of heat pump coil
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

        WaterThermalTankData &Tank(WaterThermalTank(WaterThermalTankNum));
        HeatPumpWaterHeaterData &HPWH(HPWaterHeater(HPNum));

        int HPWaterInletNode = HPWH.CondWaterInletNode;
        int DXCoilAirInletNode = HPWH.DXCoilAirInletNode;
        if (HPWH.bIsIHP) {
            HPWH.OperatingWaterFlowRate = IntegratedHeatPump::GetWaterVolFlowRateIHP(HPWH.DXCoilNum, SpeedNum, SpeedRatio, true);
            modMdotAir = IntegratedHeatPump::GetAirMassFlowRateIHP(HPWH.DXCoilNum, SpeedNum, SpeedRatio, true);
            HPWH.OperatingAirFlowRate = IntegratedHeatPump::GetAirVolFlowRateIHP(HPWH.DXCoilNum, SpeedNum, SpeedRatio, true);
            DataLoopNode::Node(DXCoilAirInletNode).MassFlowRate = modMdotAir;
            DataLoopNode::Node(DXCoilAirInletNode).MassFlowRateMaxAvail = modMdotAir;
            DataLoopNode::Node(DXCoilAirInletNode).MassFlowRateMax = modMdotAir;
        } else {
            HPWH.OperatingWaterFlowRate = HPWH.HPWHWaterVolFlowRate(SpeedNum) * SpeedRatio +
                                                          HPWH.HPWHWaterVolFlowRate(SpeedLow) * (1.0 - SpeedRatio);
            HPWH.OperatingAirFlowRate = HPWH.HPWHAirVolFlowRate(SpeedNum) * SpeedRatio +
                                                        HPWH.HPWHAirVolFlowRate(SpeedLow) * (1.0 - SpeedRatio);
            modMdotAir = HPWH.HPWHAirMassFlowRate(SpeedNum) * SpeedRatio +
                      HPWH.HPWHAirMassFlowRate(SpeedLow) * (1.0 - SpeedRatio);
        }

        MdotWater = HPWH.OperatingWaterFlowRate * WaterDens;
        Tank.SourceMassFlowRate = MdotWater;

        DataLoopNode::Node(DXCoilAirInletNode).MassFlowRate = modMdotAir;
        DataLoopNode::Node(HPWaterInletNode).MassFlowRate = MdotWater;
        Tank.SourceMassFlowRate = MdotWater;

        if (HPWH.InletAirMixerNode > 0) {
            DataLoopNode::Node(HPWH.InletAirMixerNode).MassFlowRate = modMdotAir;
            DataLoopNode::Node(HPWH.InletAirMixerNode).MassFlowRateMaxAvail = modMdotAir;
        } else {
            if (HPWH.OutsideAirNode == 0) {
                DataLoopNode::Node(HPWH.HeatPumpAirInletNode).MassFlowRate = modMdotAir;
                DataLoopNode::Node(HPWH.HeatPumpAirInletNode).MassFlowRateMaxAvail = modMdotAir;
            } else {
                DataLoopNode::Node(HPWH.OutsideAirNode).MassFlowRate = modMdotAir;
                DataLoopNode::Node(HPWH.OutsideAirNode).MassFlowRateMaxAvail = modMdotAir;
            }
        }

        // put fan component first, regardless placement, to calculate fan power
        int FanInNode;
        if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            FanInNode = HVACFan::fanObjs[HPWH.FanNum]->inletNodeNum;
        } else {
            FanInNode = Fans::Fan(HPWH.FanNum).InletNodeNum;
        }

        DataLoopNode::Node(FanInNode).MassFlowRate = modMdotAir;
        DataLoopNode::Node(FanInNode).MassFlowRateMaxAvail = modMdotAir;
        DataLoopNode::Node(FanInNode).MassFlowRateMax = modMdotAir;
        if (HPWH.FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::Fan(HPWH.FanNum).MassFlowRateMaxAvail = modMdotAir;
        } // system fan will use the inlet node max avail.

        if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            HVACFan::fanObjs[HPWH.FanNum]->simulate(_, _, _, _);
        } else {
            Fans::SimulateFanComponents(HPWH.FanName, FirstHVACIteration, HPWH.FanNum);
        }
    }

    Real64 PLRResidualIterSpeed(Real64 const SpeedRatio,  // speed ratio between two speed levels
                                Array1<Real64> const &Par //
    )
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

        int WaterThermalTankNum = int(Par(1));
        int HPNum = int(Par(2));
        int SpeedNum = int(Par(3));
        int HPWaterInletNode = int(Par(4));
        int HPWaterOutletNode = int(Par(5));
        Real64 RhoWater = Par(6);
        WaterThermalTank(WaterThermalTankNum).Mode = int(Par(8));
        bool FirstHVACIteration = (Par(9) == 1.0);

        modHPPartLoadRatio = 1.0;
        Real64 MdotWater = 0.0;
        SetVSHPWHFlowRates(WaterThermalTankNum, HPNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

        if (HPWaterHeater(HPNum).bIsIHP) {
            IntegratedHeatPump::SimIHP(HPWaterHeater(HPNum).DXCoilName,
                                       HPWaterHeater(HPNum).DXCoilNum,
                                       DataHVACGlobals::CycFanCycCoil,
                                       EMP1,
                                       EMP2,
                                       EMP3,
                                       1,
                                       modHPPartLoadRatio,
                                       SpeedNum,
                                       SpeedRatio,
                                       0.0,
                                       0.0,
                                       true,
                                       false,
                                       1.0);
        } else {
            VariableSpeedCoils::SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName,
                                                      HPWaterHeater(HPNum).DXCoilNum,
                                                      DataHVACGlobals::CycFanCycCoil,
                                                      EMP1,
                                                      EMP2,
                                                      EMP3,
                                                      1,
                                                      modHPPartLoadRatio,
                                                      SpeedNum,
                                                      SpeedRatio,
                                                      0.0,
                                                      0.0,
                                                      1.0);
        }

        Real64 CondenserDeltaT;
        CondenserDeltaT = DataLoopNode::Node(HPWaterOutletNode).Temp - DataLoopNode::Node(HPWaterInletNode).Temp;

        //           move the full load outlet temperature rate to the water heater structure variables
        //           (water heaters source inlet node temperature/mdot are set in Init, set it here after DXCoils::CalcHPWHDXCoil has been called)
        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = DataLoopNode::Node(HPWaterInletNode).Temp + CondenserDeltaT;

        //           this CALL does not update node temps, must use WaterThermalTank variables
        // select tank type
        Real64 NewTankTemp = 0.0;
        {
            auto const SELECT_CASE_var1(HPWaterHeater(HPNum).TankTypeNum);
            if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterMixed) {
                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankMixed(WaterThermalTankNum);
                NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
            } else if (SELECT_CASE_var1 == DataPlant::TypeOf_WtrHeaterStratified) {
                WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankStratified(WaterThermalTankNum);
                NewTankTemp = WaterThermalTank(WaterThermalTankNum).FindStratifiedTankSensedTemp();
            }
        }

        Real64 PLRResidualIterSpeed = Par(7) - NewTankTemp;
        return PLRResidualIterSpeed;
    }

    Real64 PLRResidualWaterThermalTank(Real64 const HPPartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                Array1<Real64> const &Par     // par(1) = HP set point temperature [C]
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

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WaterThermalTankNum = int(Par(3));
        WaterThermalTank(WaterThermalTankNum).Mode = int(Par(2));
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = Par(5) * HPPartLoadRatio;
        WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTank(WaterThermalTankNum);
        Real64 NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
        Real64 PLRResidualWaterThermalTank = Par(1) - NewTankTemp;
        return PLRResidualWaterThermalTank;
    }

    Real64 PLRResidualHPWH(Real64 const HPPartLoadRatio, Array1<Real64> const &Par)
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

        int const WaterThermalTankNum = int(Par(3));
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);
        HeatPumpWaterHeaterData &HeatPump = HPWaterHeater(Tank.HeatPumpNum);
        bool const isVariableSpeed = (HeatPump.NumofSpeed > 0);
        Tank.Mode = int(Par(2));
        // Apply the PLR
        if (Tank.TypeNum == DataPlant::TypeOf_WtrHeaterMixed) {
            // For a mixed tank, the PLR is applied to the source mass flow rate.
            Tank.SourceMassFlowRate = Par(5) * HPPartLoadRatio;
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankMixed(WaterThermalTankNum);
        } else {
            assert(Tank.TypeNum == DataPlant::TypeOf_WtrHeaterStratified);
            // For a stratified tank, the PLR is applied to the Coil.TotalHeatingEnergyRate
            // whether that's a VariableSpeedCoils::VarSpeedCoil or DXCoils::DXCoil.
            // Here we create a pointer to the TotalHeatingEnergyRate for the appropriate coil type.
            Real64 *CoilTotalHeatingEnergyRatePtr;
            if (isVariableSpeed) {
                if (HeatPump.bIsIHP)
                    CoilTotalHeatingEnergyRatePtr = &IntegratedHeatPump::IntegratedHeatPumps(HeatPump.DXCoilNum).TotalWaterHeatingRate;
                else
                    CoilTotalHeatingEnergyRatePtr = &VariableSpeedCoils::VarSpeedCoil(HeatPump.DXCoilNum).TotalHeatingEnergyRate;
            } else {
                CoilTotalHeatingEnergyRatePtr = &DXCoils::DXCoil(HeatPump.DXCoilNum).TotalHeatingEnergyRate;
            }
            // Copy the value of the total heating energy rate
            Real64 const CoilTotalHeatingEnergyRateBackup = *CoilTotalHeatingEnergyRatePtr;
            // Apply the PLR
            *CoilTotalHeatingEnergyRatePtr *= HPPartLoadRatio;
            // Tank Calculation
            WaterThermalTank(WaterThermalTankNum).CalcWaterThermalTankStratified(WaterThermalTankNum);
            // Restore the original value
            *CoilTotalHeatingEnergyRatePtr = CoilTotalHeatingEnergyRateBackup;
        }
        Real64 const NewTankTemp = Tank.GetHPWHSensedTankTemp();
        Real64 const PLRResidualHPWH = Par(1) - NewTankTemp;
        return PLRResidualHPWH;
    }

    bool WaterThermalTankData::SourceHeatNeed(Real64 const OutletTemp,
                        Real64 const DeadBandTemp,
                        Real64 const SetPointTemp
        ){
        // FUNCTION INFORMATION:
        //       AUTHOR         Yueyue Zhou
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Determine by tank type, tank temperature and control mode if source side flow is needed


        //return value initialization
        bool NeedsHeatOrCool = false;

        if (!this->IsChilledWaterTank) {
            if (this->SourceSideControlMode == modSourceSideIndirectHeatPrimarySetpoint) {
                if (OutletTemp < DeadBandTemp) {
                    NeedsHeatOrCool = true;
                } else if ((OutletTemp >= DeadBandTemp) && (OutletTemp < SetPointTemp)) {
                    // inside the deadband, use saved mode from water heater calcs
                    if (this->SavedMode == modHeatMode) {
                        NeedsHeatOrCool = true;
                    } else if (this->SavedMode == modFloatMode) {
                        NeedsHeatOrCool = false;
                    }

                } else if (OutletTemp >= SetPointTemp) {
                    NeedsHeatOrCool = false;
                }
            } else if (this->SourceSideControlMode == modSourceSideIndirectHeatAltSetpoint) {
                // get alternate setpoint
                Real64 const AltSetpointTemp = ScheduleManager::GetCurrentScheduleValue(this->SourceSideAltSetpointSchedNum);
                Real64 const AltDeadBandTemp = AltSetpointTemp - this->DeadBandDeltaTemp;
                if (OutletTemp < AltDeadBandTemp) {
                    NeedsHeatOrCool = true;
                } else if ((OutletTemp >= AltDeadBandTemp) && (OutletTemp < AltSetpointTemp)) {
                    // inside the deadband, use saved mode from water heater calcs
                    if (this->SavedMode == modHeatMode) {
                        NeedsHeatOrCool = true;
                    } else if (this->SavedMode == modFloatMode) {
                        NeedsHeatOrCool = false;
                    }

                } else if (OutletTemp >= AltSetpointTemp) {
                    NeedsHeatOrCool = false;
                }
            } else if (this->SourceSideControlMode == modSourceSideStorageTank) {
                if (OutletTemp < this->TankTempLimit) {
                    NeedsHeatOrCool = true;
                } else {
                    NeedsHeatOrCool = false;
                }
            }
        } else { // is a chilled water tank so flip logic
            if (OutletTemp > DeadBandTemp) {
                NeedsHeatOrCool = true;
            } else if ((OutletTemp <= DeadBandTemp) && (OutletTemp > SetPointTemp)) {
                // inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
                if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankMixed) {
                    if (this->SavedMode == modCoolMode) {
                        NeedsHeatOrCool = true;
                    } else if (this->SavedMode == modFloatMode) {
                        NeedsHeatOrCool = false;
                    }
                } else if (this->TypeNum == DataPlant::TypeOf_ChilledWaterTankStratified) {
                    NeedsHeatOrCool = true;
                }

            } else if (OutletTemp <= SetPointTemp) {
                NeedsHeatOrCool = false;
            }
        }
        return NeedsHeatOrCool;
    }

    Real64 PlantMassFlowRatesFunc(int const WaterThermalTankNum,
                                  int const InNodeNum,
                                  bool const FirstHVACIteration,
                                  int const WaterThermalTankSide,
                                  int const PlantLoopSide,
                                  bool const EP_UNUSED(PlumbedInSeries), // !unused1208
                                  int const BranchControlType,
                                  Real64 const OutletTemp,
                                  Real64 const DeadBandTemp,
                                  Real64 const SetPointTemp)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   October 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // collect routines for setting flow rates for Water heaters
        // with plant connections.

        // Return value
        Real64 PlantMassFlowRatesFunc;

        // FUNCTION PARAMETER DEFINITIONS:
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
            if (WaterThermalTank(WaterThermalTankNum).UseCurrentFlowLock == 0) {
                CurrentMode = PassingFlowThru;
                if ((WaterThermalTank(WaterThermalTankNum).UseSideLoadRequested > 0.0) && (WaterThermalTankSide == modUseSide)) {
                    CurrentMode = MaybeRequestingFlow;
                }
            } else {
                CurrentMode = PassingFlowThru;
            }
            if (WaterThermalTankSide == modSourceSide) {
                CurrentMode = MaybeRequestingFlow;
            }
        } else if (PlantLoopSide == DataPlant::DemandSide) {
            //  1.  pass thru is default
            CurrentMode = PassingFlowThru;

            //  2.  Might be Requesting Flow.
            if (FirstHVACIteration) {
                if (BranchControlType == DataBranchAirLoopPlant::ControlType_Bypass) {
                    CurrentMode = PassingFlowThru;
                } else {
                    CurrentMode = MaybeRequestingFlow;
                }
            } else {
                if (BranchControlType == DataBranchAirLoopPlant::ControlType_Bypass) {
                    CurrentMode = PassingFlowThru;
                } else {
                    CurrentMode = ThrottlingFlow;
                }
            }
        }

        // evaluate Availability schedule,
        bool ScheduledAvail = true;
        if (WaterThermalTankSide == modUseSide) {
            if (ScheduleManager::GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum) == 0.0) {
                ScheduledAvail = false;
            }
        } else if (WaterThermalTankSide == modSourceSide) {
            if (ScheduleManager::GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum) == 0.0) {
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
                    FlowResult = DataLoopNode::Node(InNodeNum).MassFlowRate;
                }

            } else if (SELECT_CASE_var == ThrottlingFlow) {
                // first determine what mass flow would be if it is to requested
                Real64 MassFlowRequest = 0.0;
                if (!ScheduledAvail) {
                    MassFlowRequest = 0.0;
                } else {
                    if (WaterThermalTankSide == modUseSide) {
                        MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax;
                    } else if (WaterThermalTankSide == modSourceSide) {
                        MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax;
                    } else {
                        assert(false);
                    }
                }

                // next determine if tank temperature is such that source side flow might be requested
                bool NeedsHeatOrCool = WaterThermalTank(WaterThermalTankNum).SourceHeatNeed(OutletTemp, DeadBandTemp, SetPointTemp);

                if (MassFlowRequest > 0.0) {
                    if (WaterThermalTankSide == modUseSide) {
                        FlowResult = MassFlowRequest;
                    } else if (WaterThermalTankSide == modSourceSide) {
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
                FlowResult = max(DataLoopNode::Node(InNodeNum).MassFlowRateMinAvail, FlowResult); // okay for compliance (reverse dd)
                FlowResult = max(DataLoopNode::Node(InNodeNum).MassFlowRateMin, FlowResult);      // okay for compliance (reverse dd)
                FlowResult = min(DataLoopNode::Node(InNodeNum).MassFlowRateMaxAvail, FlowResult);
                //=> following might take out of reverse dd compliance
                FlowResult = min(DataLoopNode::Node(InNodeNum).MassFlowRateMax, FlowResult);

                // DSU> use PlantUtilities::SetComponentFlowRate for above?

            } else if (SELECT_CASE_var == MaybeRequestingFlow) {

                // first determine what mass flow would be if it is to requested
                Real64 MassFlowRequest = 0.0;
                if (!ScheduledAvail) {
                    MassFlowRequest = 0.0;
                } else {
                    if (WaterThermalTankSide == modUseSide) {
                        if ((WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) &&
                            (WaterThermalTank(WaterThermalTankNum).UseSideLoadRequested > 0.0)) {
                            MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax;
                        } else if ((WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) &&
                                   (WaterThermalTank(WaterThermalTankNum).UseSideLoadRequested == 0.0)) {
                            MassFlowRequest = 0.0;
                        } else {
                            MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax;
                        }

                    } else if (WaterThermalTankSide == modSourceSide) {
                        MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax;
                    }
                }

                if (WaterThermalTankSide == modSourceSide) { // temperature dependent controls for indirect heating/cooling
                    bool NeedsHeatOrCool = WaterThermalTank(WaterThermalTankNum).SourceHeatNeed(OutletTemp, DeadBandTemp, SetPointTemp);
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

        PlantMassFlowRatesFunc = FlowResult;

        return PlantMassFlowRatesFunc;
    }

    void WaterThermalTankData::MinePlantStructForInfo()
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

        // IF (WaterThermalTank(WaterThermalTankNum)%PlantStructureCheck .AND. ALLOCATED(PlantLoop)) THEN
        if (allocated(DataPlant::PlantLoop) && this->UseSidePlantLoopNum > 0) {

            // check plant structure for useful data.

            int UseInletNode = this->UseInletNode;
            int PlantLoopNum = this->UseSidePlantLoopNum;
            int LoopSideNum = this->UseSidePlantLoopSide;

            if ((this->UseDesignVolFlowRateWasAutoSized) &&
                (this->UseSidePlantSizNum == 0)) {
                ShowSevereError("Water heater = " + this->Name +
                                " for autosizing Use side flow rate, did not find Sizing:Plant object " + DataPlant::PlantLoop(PlantLoopNum).Name);
                ErrorsFound = true;
            }
            // Is this wh Use side plumbed in series (default) or are there other branches in parallel?
            if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).SplitterExists) {
                if (any_eq(DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.NodeNumOut,
                           UseInletNode)) { // this wh is on the splitter
                    if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.TotalOutletNodes > 1) {
                        this->UseSideSeries = false;
                    }
                }
            }
        }

        if (allocated(DataPlant::PlantLoop) && this->SourceSidePlantLoopNum > 0) {
            int SourceInletNode = this->SourceInletNode;
            int PlantLoopNum = this->SourceSidePlantLoopNum;
            int LoopSideNum = this->SourceSidePlantLoopSide;
            // was user's input correct for plant loop name?
            if ((this->SourceDesignVolFlowRateWasAutoSized) &&
                (this->SourceSidePlantSizNum == 0) && (this->DesuperheaterNum == 0) &&
                (this->HeatPumpNum == 0)) {
                ShowSevereError("Water heater = " + this->Name +
                                "for autosizing Source side flow rate, did not find Sizing:Plant object " + DataPlant::PlantLoop(PlantLoopNum).Name);
                ErrorsFound = true;
            }
            // Is this wh Source side plumbed in series (default) or are there other branches in parallel?
            if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).SplitterExists) {
                if (any_eq(DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.NodeNumOut,
                           SourceInletNode)) { // this wh is on the splitter
                    if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter.TotalOutletNodes > 1) {
                        this->SourceSideSeries = false;
                    }
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding water heater input errors cause program termination");
        }
    }

    void WaterThermalTankData::SizeSupplySidePlantConnections(Optional_int_const LoopNum, Optional_int_const LoopSideNum)
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

        static std::string const RoutineName("SizeSupplySidePlantConnections");

        bool ErrorsFound = false; // If errors detected in input
        Real64 tmpUseDesignVolFlowRate = this->UseDesignVolFlowRate;
        Real64 tmpSourceDesignVolFlowRate = this->SourceDesignVolFlowRate;

        int tmpLoopNum;
        int tmpLoopSideNum;
        if (!present(LoopSideNum)) {
            tmpLoopSideNum = DataPlant::SupplySide;
        } else {
            tmpLoopSideNum = LoopSideNum;
        }
        if (!present(LoopNum)) {
            tmpLoopNum = this->SourceSidePlantLoopNum;
        } else {
            tmpLoopNum = LoopNum;
        }

        if ((this->UseInletNode > 0) && (tmpLoopNum == this->UseSidePlantLoopNum)) {
            if (this->UseDesignVolFlowRateWasAutoSized) {
                int PltSizNum = this->UseSidePlantSizNum;
                if (PltSizNum > 0) { // we have a Plant Sizing Object
                    if (this->UseSidePlantLoopSide == DataPlant::SupplySide) {
                        if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->UseDesignVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
                            } else {
                                tmpUseDesignVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
                            }
                        } else {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->UseDesignVolFlowRate = 0.0;
                            } else {
                                tmpUseDesignVolFlowRate = 0.0;
                            }
                        }
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Use Side Design Flow Rate [m3/s]",
                                               this->UseDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Initial Use Side Design Flow Rate [m3/s]",
                                               this->UseDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->UseInletNode,
                                                        this->UseDesignVolFlowRate);
                        } else {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->UseInletNode, tmpUseDesignVolFlowRate);
                        }

                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            this->PlantUseMassFlowRateMax =
                                this->UseDesignVolFlowRate * rho;
                        } else {
                            this->PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
                        }
                    }
                } else {
                    // do nothing
                } // plant sizing object
            } else {
                PlantUtilities::RegisterPlantCompDesignFlow(this->UseInletNode,
                                            this->UseDesignVolFlowRate);
                Real64 rho;
                if (this->UseSidePlantLoopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, modWaterIndex, RoutineName);
                }

                this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;

            } // autosizing needed.
        }     // connected to plant

        if ((this->SourceInletNode > 0) &&
            (tmpLoopNum == this->SourceSidePlantLoopNum)) {
            if (this->SourceDesignVolFlowRateWasAutoSized) {
                int PltSizNum = this->SourceSidePlantSizNum;
                if (PltSizNum > 0) {
                    if (this->SourceSidePlantLoopSide == DataPlant::SupplySide) {
                        if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->SourceDesignVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
                            } else {
                                tmpSourceDesignVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
                            }
                        } else {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->SourceDesignVolFlowRate = 0.0;
                            } else {
                                tmpSourceDesignVolFlowRate = 0.0;
                            }
                        }
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Source Side Design Flow Rate [m3/s]",
                                               this->SourceDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Initial Source Side Design Flow Rate [m3/s]",
                                               this->SourceDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->SourceInletNode,
                                                        this->SourceDesignVolFlowRate);
                        } else {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->SourceInletNode, tmpSourceDesignVolFlowRate);
                        }
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            this->PlantSourceMassFlowRateMax =
                                this->SourceDesignVolFlowRate * rho;
                        } else {
                            this->PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
                        }
                    } // plant loop allocation
                } else {
                    // do nothing
                } // plant sizing object
            } else {
                if (this->SourceSidePlantLoopSide == DataPlant::SupplySide) {
                    PlantUtilities::RegisterPlantCompDesignFlow(this->SourceInletNode,
                                                this->SourceDesignVolFlowRate);
                    Real64 rho;
                    if (this->SourceSidePlantLoopNum > 0) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                    } else {
                        rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, modWaterIndex, RoutineName);
                    }
                    this->PlantSourceMassFlowRateMax =
                        this->SourceDesignVolFlowRate * rho;
                }
            } // autosizing needed.
        }     // connected to plant

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void WaterThermalTankData::SizeTankForDemandSide()
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
        static std::string const RoutineName("SizeTankForDemandSide");
        Real64 const GalTocubicMeters(0.0037854);
        Real64 const kBtuPerHrToWatts(293.1);

        Real64 Tstart = 14.44;
        Real64 Tfinish = 57.22;

        Real64 tmpTankVolume = this->Volume;
        Real64 tmpMaxCapacity = this->MaxCapacity;

        {
            auto const SELECT_CASE_var(this->Sizing.DesignMode);

            if (SELECT_CASE_var == modSizeNotSet) {

            } else if (SELECT_CASE_var == modSizePeakDraw) {

            } else if (SELECT_CASE_var == modSizeResidentialMin) {

                // assume can propagate rules for gas to other fuels.
                bool FuelTypeIsLikeGas = false;
                if (UtilityRoutines::SameString(this->FuelType, "Gas")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Diesel")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Gasoline")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "Coal")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "FuelOil#1")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(this->FuelType, "FuelOil#2")) {
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
                    if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 2.5 * 1000.0; // 2.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; // 27kBtu/hr
                    }

                } else if (this->Sizing.NumberOfBedrooms == 2) {
                    if (this->Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 3.5 * 1000.0; // 3.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((this->Sizing.NumberOfBathrooms > 1.5) &&
                               (this->Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    }
                } else if (this->Sizing.NumberOfBedrooms == 3) {
                    if (this->Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((this->Sizing.NumberOfBathrooms > 1.5) &&
                               (this->Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    }
                } else if (this->Sizing.NumberOfBedrooms == 4) {
                    if (this->Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((this->Sizing.NumberOfBathrooms > 1.5) &&
                               (this->Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    }
                } else if (this->Sizing.NumberOfBedrooms == 5) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; // 47 kBtu/hr
                    }
                } else if (this->Sizing.NumberOfBedrooms >= 6) {
                    if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; // 50 kBtu/hr
                    }
                }

                if (this->VolumeWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->Volume = tmpTankVolume;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Tank Volume [m3]",
                                           this->Volume);
                    }
                }
                if (this->MaxCapacityWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->MaxCapacity = tmpMaxCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == modSizePerPerson) {
                // how to get number of people?

                Real64 SumPeopleAllZones = sum(DataHeatBalance::Zone, &DataHeatBalance::ZoneData::TotOccupants);
                if (this->VolumeWasAutoSized)
                    tmpTankVolume = this->Sizing.TankCapacityPerPerson * SumPeopleAllZones;

                if (this->MaxCapacityWasAutoSized) {
                    Real64 rho;
                    Real64 Cp;
                    if (this->UseSidePlantLoopNum > 0) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                                                RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                                                    ((Tfinish + Tstart) / 2.0),
                                                                    DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                                                    RoutineName);
                    } else {
                        rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                    }

                    tmpMaxCapacity = SumPeopleAllZones * this->Sizing.RecoveryCapacityPerPerson * (Tfinish - Tstart) *
                                     (1.0 / DataGlobals::SecInHour) * rho * Cp; // m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                }

                if (this->VolumeWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->Volume = tmpTankVolume;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Tank Volume [m3]",
                                           this->Volume);
                    }
                }
                if (this->MaxCapacityWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->MaxCapacity = tmpMaxCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == modSizePerFloorArea) {

                Real64 SumFloorAreaAllZones = sum(DataHeatBalance::Zone, &DataHeatBalance::ZoneData::FloorArea);
                if (this->VolumeWasAutoSized)
                    tmpTankVolume = this->Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
                if (this->MaxCapacityWasAutoSized) {
                    Real64 rho;
                    Real64 Cp;
                    if (this->UseSidePlantLoopNum > 0) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                                                RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                                                    ((Tfinish + Tstart) / 2.0),
                                                                    DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                                                    RoutineName);
                    } else {
                        rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                    }
                    tmpMaxCapacity = SumFloorAreaAllZones * this->Sizing.RecoveryCapacityPerArea *
                                     (Tfinish - Tstart) * (1.0 / DataGlobals::SecInHour) * rho *
                                     Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                }
                if (this->VolumeWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->Volume = tmpTankVolume;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Tank Volume [m3]",
                                           this->Volume);
                    }
                }
                if (this->MaxCapacityWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->MaxCapacity = tmpMaxCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == modSizePerUnit) {

                if (this->VolumeWasAutoSized)
                    tmpTankVolume =
                        this->Sizing.TankCapacityPerUnit * this->Sizing.NumberOfUnits;

                if (this->MaxCapacityWasAutoSized) {
                    Real64 rho;
                    Real64 Cp;
                    if (this->UseSidePlantLoopNum > 0) {
                        rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                                                ((Tfinish + Tstart) / 2.0),
                                                                DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                                                RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                                                    ((Tfinish + Tstart) / 2.0),
                                                                    DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                                                    RoutineName);
                    } else {
                        rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                    }
                    tmpMaxCapacity = this->Sizing.NumberOfUnits *
                                     this->Sizing.RecoveryCapacityPerUnit * (Tfinish - Tstart) * (1.0 / DataGlobals::SecInHour) *
                                     rho * Cp; // m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                }

                if (this->VolumeWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->Volume = tmpTankVolume;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Tank Volume [m3]",
                                           this->Volume);
                    }
                }
                if (this->MaxCapacityWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->MaxCapacity = tmpMaxCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == modSizePerSolarColArea) {
            }
        }

        // if stratified, might set height.
        if ((this->VolumeWasAutoSized) && (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) &&
            DataPlant::PlantFirstSizesOkayToFinalize) { // might set height
            if ((this->HeightWasAutoSized) && (!this->VolumeWasAutoSized)) {
                this->Height = std::pow(
                    (4.0 * this->Volume * pow_2(this->Sizing.HeightAspectRatio)) / DataGlobals::Pi,
                    0.3333333333333333);
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(this->Type,
                                       this->Name,
                                       "Tank Height [m]",
                                       this->Height);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(this->Type,
                                       this->Name,
                                       "Initial Tank Height [m]",
                                       this->Height);
                }
                // check if DataGlobals::AutoCalculate Use outlet and source inlet are still set to autosize by earlier
                if (this->UseOutletHeightWasAutoSized) {
                    this->UseOutletHeight = this->Height;
                }
                if (this->SourceInletHeightWasAutoSized) {
                    this->SourceInletHeight = this->Height;
                }
            }
        }
    }

    void WaterThermalTankData::SizeTankForSupplySide()
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

        static std::string const RoutineName("SizeTankForSupplySide");

        Real64 Tstart = 14.44;
        Real64 Tfinish = 57.22;

        Real64 tmpTankVolume = this->Volume;
        Real64 tmpMaxCapacity = this->MaxCapacity;

        {
            auto const SELECT_CASE_var(this->Sizing.DesignMode);

            if (SELECT_CASE_var == modSizePeakDraw) {
                if (this->VolumeWasAutoSized)
                    tmpTankVolume = this->Sizing.TankDrawTime *
                                    this->UseDesignVolFlowRate * DataGlobals::SecInHour; // hours | m3/s | (3600 s/1 hour)
                if (this->VolumeWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->Volume = tmpTankVolume;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Tank Volume [m3]",
                                           this->Volume);
                    }
                }
                if (this->MaxCapacityWasAutoSized) {
                    if (this->Sizing.RecoveryTime > 0.0) {
                        Real64 rho;
                        Real64 Cp;
                        if (this->SourceSidePlantLoopNum > 0) {
                            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidName,
                                                   ((Tfinish + Tstart) / 2.0),
                                                   DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidIndex,
                                                   RoutineName);
                            Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidName,
                                                       ((Tfinish + Tstart) / 2.0),
                                                       DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidIndex,
                                                       RoutineName);
                        } else {
                            rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                            Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        }
                        tmpMaxCapacity = (this->Volume * rho * Cp * (Tfinish - Tstart)) /
                                         (this->Sizing.RecoveryTime * DataGlobals::SecInHour); // m3 | kg/m3 | J/Kg/K | K | seconds
                    } else {
                        ShowFatalError("SizeTankForSupplySide: Tank=\"" + this->Name +
                                       "\", requested sizing for max capacity but entered Recovery Time is zero.");
                    }
                }

                if (this->MaxCapacityWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->MaxCapacity = tmpMaxCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == modSizePerSolarColArea) {

                this->Sizing.TotalSolarCollectorArea = 0.0;
                for (int CollectorNum = 1; CollectorNum <= SolarCollectors::NumOfCollectors; ++CollectorNum) {
                    this->Sizing.TotalSolarCollectorArea += DataSurfaces::Surface(SolarCollectors::Collector(CollectorNum).Surface).Area;
                }

                if (this->VolumeWasAutoSized)
                    tmpTankVolume = this->Sizing.TotalSolarCollectorArea *
                                    this->Sizing.TankCapacityPerCollectorArea;
                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 0.0;
                if (this->VolumeWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->Volume = tmpTankVolume;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Tank Volume [m3]",
                                           this->Volume);
                    }
                }
                if (this->MaxCapacityWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->MaxCapacity = tmpMaxCapacity;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            }
        }

        if ((this->VolumeWasAutoSized) && (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) &&
            DataPlant::PlantFirstSizesOkayToFinalize) { // might set height
            if ((this->HeightWasAutoSized) && (!this->VolumeWasAutoSized)) {
                this->Height = std::pow(
                    (4.0 * this->Volume * pow_2(this->Sizing.HeightAspectRatio)) / DataGlobals::Pi,
                    0.3333333333333333);
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(this->Type,
                                       this->Name,
                                       "Tank Height [m]",
                                       this->Height);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(this->Type,
                                       this->Name,
                                       "Initial Tank Height [m]",
                                       this->Height);
                }
            }
        }
    }

    void WaterThermalTankData::SizeDemandSidePlantConnections()
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
        //  intial and final tank temperatures are 14.44 and reheat to 57.22 (values from CalcStandardRatings routine)
        
        static std::string const RoutineName("SizeDemandSidePlantConnections");

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
                    if (this->UseSidePlantLoopSide == DataPlant::DemandSide) {
                        // probably shouldn't come here as Use side is unlikley to be on demand side (?)
                        // but going to treat component with symetry so if connections are reversed it'll still work
                        // choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
                        // in user specified hours.
                        //  using the plant inlet design temp for sizing.
                        Real64 Tpdesign = DataSizing::PlantSizData(PltSizNum).ExitTemp;
                        Real64 eff = this->UseEffectiveness;
                        if ((Tpdesign >= 58.0) && (!this->IsChilledWaterTank)) {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->UseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpUseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else if ((Tpdesign <= 8.0) && (this->IsChilledWaterTank)) {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->UseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpUseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else {
                            if (!this->IsChilledWaterTank) {
                                // plant sizing object design temperature is set too low throw warning.
                                ShowSevereError("Autosizing of Use side water heater design flow rate requires Sizing:Plant object to have an exit "
                                                "temperature >= 58C");
                                ShowContinueError("Occurs for water heater object=" + this->Name);
                            } else {
                                // plant sizing object design temperature is set too hi throw warning.
                                ShowSevereError("Autosizing of Use side chilled water tank design flow rate requires Sizing:Plant object to have an "
                                                "exit temperature <= 8C");
                                ShowContinueError("Occurs for chilled water storage tank object=" + this->Name);
                            }
                            ErrorsFound = true;
                        }
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Use Side Design Flow Rate [m3/s]",
                                               this->UseDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Initial Use Side Design Flow Rate [m3/s]",
                                               this->UseDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->UseInletNode,
                                                        this->UseDesignVolFlowRate);
                        } else {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->UseInletNode, tmpUseDesignVolFlowRate);
                        }
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            this->PlantUseMassFlowRateMax =
                                this->UseDesignVolFlowRate * rho;
                        } else {
                            this->PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
                        }
                    } // Demand side
                } else {
                    // do nothing
                } // plant sizing object

            } else {
                // not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
                PlantUtilities::RegisterPlantCompDesignFlow(this->UseInletNode,
                                            this->UseDesignVolFlowRate);
                Real64 rho;
                if (this->UseSidePlantLoopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           DataPlant::PlantLoop(this->UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, modWaterIndex, RoutineName);
                }
                this->PlantUseMassFlowRateMax = this->UseDesignVolFlowRate * rho;
            } // autosizing needed.
        }     // connected to plant

        if (this->SourceInletNode > 0) {
            if (this->SourceDesignVolFlowRateWasAutoSized) {
                int PltSizNum = this->SourceSidePlantSizNum;
                if (PltSizNum > 0) {
                    if (this->SourceSidePlantLoopSide == DataPlant::DemandSide) {
                        //  choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
                        // in user specified hours.
                        //  using the plant inlet design temp for sizing.
                        Real64 Tpdesign = DataSizing::PlantSizData(PltSizNum).ExitTemp;
                        Real64 eff = this->SourceEffectiveness;
                        if ((Tpdesign >= 58.0) && (!this->IsChilledWaterTank)) {

                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->SourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpSourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else if ((Tpdesign <= 8.0) && (this->IsChilledWaterTank)) {
                            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                                this->SourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpSourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * DataGlobals::SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else {
                            if (!this->IsChilledWaterTank) {
                                // plant sizing object design temperature is set too low throw warning.
                                ShowSevereError("Autosizing of Source side water heater design flow rate requires Sizing:Plant object to have an "
                                                "exit temperature >= 58C");
                                ShowContinueError("Occurs for WaterHeater:Mixed object=" + this->Name);
                            } else {
                                // plant sizing object design temperature is set too hi throw warning.
                                ShowSevereError("Autosizing of Source side chilled water tank design flow rate requires Sizing:Plant object to have "
                                                "an exit temperature <= 8C");
                                ShowContinueError("Occurs for chilled water storage tank object=" + this->Name);
                            }
                            ErrorsFound = true;
                        }
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Source Side Design Flow Rate [m3/s]",
                                               this->SourceDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(this->Type,
                                               this->Name,
                                               "Initial Source Side Design Flow Rate [m3/s]",
                                               this->SourceDesignVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->SourceInletNode,
                                                        this->SourceDesignVolFlowRate);
                        } else {
                            PlantUtilities::RegisterPlantCompDesignFlow(this->SourceInletNode, tmpSourceDesignVolFlowRate);
                        }
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (DataPlant::PlantFirstSizesOkayToFinalize) {
                            this->PlantSourceMassFlowRateMax =
                                this->SourceDesignVolFlowRate * rho;
                        } else {
                            this->PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
                        }
                    } // demand side
                } else {
                    // do nothing
                } // plant sizing object

            } else {
                // not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
                PlantUtilities::RegisterPlantCompDesignFlow(this->SourceInletNode,
                                            this->SourceDesignVolFlowRate);
                Real64 rho;
                if (this->SourceSidePlantLoopNum > 0) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           DataPlant::PlantLoop(this->SourceSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                } else {
                    rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, modWaterIndex, RoutineName);
                }
                this->PlantSourceMassFlowRateMax =
                    this->SourceDesignVolFlowRate * rho;
            } // autosizing needed.
        }     // connected to plant

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void WaterThermalTankData::SizeStandAloneWaterHeater()
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
        static std::string const RoutineName("SizeStandAloneWaterHeater");

        Real64 Tstart = 14.44;
        Real64 Tfinish = 57.22;
        Real64 tmpTankVolume = this->Volume;
        Real64 tmpMaxCapacity = this->MaxCapacity;

        if (this->VolumeWasAutoSized || this->MaxCapacityWasAutoSized) {

            {
                auto const SELECT_CASE_var(this->Sizing.DesignMode);

                if (SELECT_CASE_var == modSizePeakDraw) {
                    // get draw rate from maximum in schedule
                    Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, DataGlobals::InitConvTemp, modWaterIndex, RoutineName);
                    Real64 DrawDesignVolFlowRate = ScheduleManager::GetScheduleMaxValue(this->FlowRateSchedule) *
                                            this->MassFlowRateMax / rho;

                    if (this->VolumeWasAutoSized) {
                        tmpTankVolume = this->Sizing.TankDrawTime * DrawDesignVolFlowRate *
                                        DataGlobals::SecInHour; // hours | m3/s | (3600 s/1 hour)
                        this->Volume = tmpTankVolume;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        if (this->Sizing.RecoveryTime > 0.0) {
                            Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);

                            tmpMaxCapacity =
                                (this->Volume * rho * Cp * (Tfinish - Tstart)) /
                                (this->Sizing.RecoveryTime * DataGlobals::SecInHour); // m3 | kg/m3 | J/Kg/K | K | seconds
                        } else {
                            ShowFatalError("SizeStandAloneWaterHeater: Tank=\"" + this->Name +
                                           "\", requested sizing for max capacity but entered Recovery Time is zero.");
                        }
                        this->MaxCapacity = tmpMaxCapacity;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }

                } else if (SELECT_CASE_var == modSizeResidentialMin) {
                    // assume can propagate rules for gas to other fuels.
                    bool FuelTypeIsLikeGas = false;
                    if (UtilityRoutines::SameString(this->FuelType, "Gas")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(this->FuelType, "Diesel")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(this->FuelType, "Gasoline")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(this->FuelType, "Coal")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(this->FuelType, "FuelOil#1")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(this->FuelType, "FuelOil#2")) {
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
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 2.5 * 1000.0; // 2.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; // 27kBtu/hr
                        }

                    } else if (this->Sizing.NumberOfBedrooms == 2) {
                        if (this->Sizing.NumberOfBathrooms <= 1.5) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 3.5 * 1000.0; // 3.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if ((this->Sizing.NumberOfBathrooms > 1.5) &&
                                   (this->Sizing.NumberOfBathrooms < 3.0)) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        }
                    } else if (this->Sizing.NumberOfBedrooms == 3) {
                        if (this->Sizing.NumberOfBathrooms <= 1.5) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if ((this->Sizing.NumberOfBathrooms > 1.5) &&
                                   (this->Sizing.NumberOfBathrooms < 3.0)) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                            }
                        }
                    } else if (this->Sizing.NumberOfBedrooms == 4) {
                        if (this->Sizing.NumberOfBathrooms <= 1.5) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if ((this->Sizing.NumberOfBathrooms > 1.5) &&
                                   (this->Sizing.NumberOfBathrooms < 3.0)) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                            }
                        } else if (this->Sizing.NumberOfBathrooms >= 3.0) {
                            if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (this->MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                            }
                        }
                    } else if (this->Sizing.NumberOfBedrooms == 5) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; // 47 kBtu/hr
                        }
                    } else if (this->Sizing.NumberOfBedrooms >= 6) {
                        if (UtilityRoutines::SameString(this->FuelType, "Electric")) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (this->VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; // 50 kBtu/hr
                        }
                    }
                    if (this->VolumeWasAutoSized) {
                        this->Volume = tmpTankVolume;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        this->MaxCapacity = tmpMaxCapacity;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }

                } else if (SELECT_CASE_var == modSizePerPerson) {
                    // how to get number of people?

                    Real64 SumPeopleAllZones = sum(DataHeatBalance::Zone, &DataHeatBalance::ZoneData::TotOccupants);
                    if (this->VolumeWasAutoSized) {
                        tmpTankVolume = this->Sizing.TankCapacityPerPerson * SumPeopleAllZones;
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        tmpMaxCapacity = SumPeopleAllZones * this->Sizing.RecoveryCapacityPerPerson *
                                         (Tfinish - Tstart) * (1.0 / DataGlobals::SecInHour) * rho *
                                         Cp; // m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                    }

                    if (this->VolumeWasAutoSized) {
                        this->Volume = tmpTankVolume;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        this->MaxCapacity = tmpMaxCapacity;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }

                } else if (SELECT_CASE_var == modSizePerFloorArea) {

                    Real64 SumFloorAreaAllZones = sum(DataHeatBalance::Zone, &DataHeatBalance::ZoneData::FloorArea);
                    if (this->VolumeWasAutoSized) {
                        tmpTankVolume = this->Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
                    }

                    if (this->MaxCapacityWasAutoSized) {
                        Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        tmpMaxCapacity = SumFloorAreaAllZones * this->Sizing.RecoveryCapacityPerArea *
                                         (Tfinish - Tstart) * (1.0 / DataGlobals::SecInHour) * rho *
                                         Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                    }
                    if (this->VolumeWasAutoSized) {
                        this->Volume = tmpTankVolume;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        this->MaxCapacity = tmpMaxCapacity;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                } else if (SELECT_CASE_var == modSizePerUnit) {

                    if (this->VolumeWasAutoSized)
                        tmpTankVolume = this->Sizing.TankCapacityPerUnit *
                                        this->Sizing.NumberOfUnits;

                    if (this->MaxCapacityWasAutoSized) {
                        Real64 rho = FluidProperties::GetDensityGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(modFluidNameWater, ((Tfinish + Tstart) / 2.0), modWaterIndex, RoutineName);
                        tmpMaxCapacity = this->Sizing.NumberOfUnits *
                                         this->Sizing.RecoveryCapacityPerUnit * (Tfinish - Tstart) *
                                         (1.0 / DataGlobals::SecInHour) * rho * Cp; // m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                    }

                    if (this->VolumeWasAutoSized) {
                        this->Volume = tmpTankVolume;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        this->MaxCapacity = tmpMaxCapacity;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                } else if (SELECT_CASE_var == modSizePerSolarColArea) {
                    this->Sizing.TotalSolarCollectorArea = 0.0;
                    for (int CollectorNum = 1; CollectorNum <= SolarCollectors::NumOfCollectors; ++CollectorNum) {
                        this->Sizing.TotalSolarCollectorArea += DataSurfaces::Surface(SolarCollectors::Collector(CollectorNum).Surface).Area;
                    }

                    if (this->VolumeWasAutoSized)
                        tmpTankVolume = this->Sizing.TotalSolarCollectorArea *
                                        this->Sizing.TankCapacityPerCollectorArea;
                    if (this->MaxCapacityWasAutoSized) tmpMaxCapacity = 0.0;
                    if (this->VolumeWasAutoSized) {
                        this->Volume = tmpTankVolume;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Tank Volume [m3]",
                                           this->Volume);
                    }
                    if (this->MaxCapacityWasAutoSized) {
                        this->MaxCapacity = tmpMaxCapacity;
                        ReportSizingManager::ReportSizingOutput(this->Type,
                                           this->Name,
                                           "Maximum Heater Capacity [W]",
                                           this->MaxCapacity);
                    }
                }
            }
        }
    }

    void WaterThermalTankData::UpdateWaterThermalTank()
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

        int UseInletNode = this->UseInletNode;
        int UseOutletNode = this->UseOutletNode;
        int SourceInletNode = this->SourceInletNode;
        int SourceOutletNode = this->SourceOutletNode;

        if (UseInletNode > 0 && UseOutletNode > 0) {
            DataLoopNode::Node(UseOutletNode) = DataLoopNode::Node(UseInletNode); // this could wipe out setpoints on outlet node

            DataLoopNode::Node(UseOutletNode).Temp = this->UseOutletTemp;
        }

        if (SourceInletNode > 0 && SourceOutletNode > 0) {
            DataLoopNode::Node(SourceOutletNode) = DataLoopNode::Node(SourceInletNode);

            DataLoopNode::Node(SourceOutletNode).Temp = this->SourceOutletTemp;
        }
    }

    void WaterThermalTankData::ReportWaterThermalTank()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brandon Anderson
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  Feb 2004, PGE

        Real64 SecInTimeStep = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

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

    void WaterThermalTankData::CalcStandardRatings(int const WaterThermalTankNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2005
        //       MODIFIED       R. Raustad, July 2005 - added HPWH to ratings procedure
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the water heater standard ratings, such as Energy Factor and Recovery Efficiency.  Results are written
        // to the EIO file.  Standard ratings are not calculated for storage-only tanks, i.e., MaxCapacity = 0.

        // METHODOLOGY EMPLOYED:
        // Water heater inputs are set to the specified test conditions. For HPWHs, the heating capacity and COP are assumed
        // to be the primary element in the water heater and are used during the rating procedure.  CalcWaterThermalTankMixed
        // is iteratively called in a self-contained, 24 hour simulation of the standard test procedure.

        // REFERENCES:
        // Title 10, Code of Federal Regulations, Part 430- Energy Conservation Program for Consumer Products, Appendix E to
        // Subpart B- Uniform Test Procedure for Measuring the Energy Consumption of Water Heaters, January 1, 2004.
        
        // Formats
        static ObjexxFCL::gio::Fmt Format_720("('Water Heater Information',6(',',A))");
        static ObjexxFCL::gio::Fmt Format_721("('Heat Pump Water Heater Information',7(',',A))");

        if (this->AlreadyRated) { // bail we already did this one
            return;
        }

        // FLOW:
        bool FirstTimeFlag;              // used during HPWH rating procedure
        bool bIsVSCoil = false;
        Real64 RecoveryEfficiency;
        Real64 EnergyFactor;
        Real64 RatedDXCoilTotalCapacity;
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

            Real64 TotalDrawMass = 0.243402 * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp); // 64.3 gal * rho
            Real64 DrawMass = TotalDrawMass / 6.0;                               // 6 equal draws
            Real64 SecInTimeStep = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            Real64 DrawMassFlowRate = DrawMass / SecInTimeStep;
            Real64 FuelEnergy = 0.0;
            FirstTimeFlag = true;

            int TimeStepPerHour = int(1.0 / DataHVACGlobals::TimeStepSys);
            int HPNum = 0;
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
                            this->CalcWaterThermalTankMixed(WaterThermalTankNum);

                        } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                            this->CalcWaterThermalTankStratified(WaterThermalTankNum);

                        } else {
                            //         Unhandled water heater type
                        }
                    }

                } else {

                    HPNum = this->HeatPumpNum;
                    Real64 AmbientHumRat = 0.00717; // Humidity ratio at 67.5 F / 50% RH

                    //       set the heat pump air- and water-side mass flow rate
                    Real64 MdotWater = HPWaterHeater(HPNum).OperatingWaterFlowRate * Psychrometrics::RhoH2O(this->TankTemp);
                    Real64 MdotAir = HPWaterHeater(HPNum).OperatingAirMassFlowRate;

                    // ?? why is HPWH condenser inlet node temp reset inside the for loop? shouldn't it chnage with the tank temp throughout these
                    // iterations?
                    if (HPWaterHeater(HPNum).TypeNum == DataPlant::TypeOf_HeatPumpWtrHeaterPumped) {
                        // set the condenser inlet node mass flow rate and temperature
                        DataLoopNode::Node(HPWaterHeater(HPNum).CondWaterInletNode).MassFlowRate = MdotWater;
                        DataLoopNode::Node(HPWaterHeater(HPNum).CondWaterInletNode).Temp = this->TankTemp;
                    }

                    //       initialize temperatures for HPWH DX Coil heating capacity and COP curves
                    DataHVACGlobals::HPWHInletDBTemp = this->AmbientTemp;
                    DataHVACGlobals::HPWHInletWBTemp = Psychrometrics::PsyTwbFnTdbWPb(DataHVACGlobals::HPWHInletDBTemp, AmbientHumRat, DataEnvironment::OutBaroPress);

                    //       set up full air flow on DX coil inlet node
                    if (HPWaterHeater(HPNum).InletAirMixerNode > 0) {
                        DataLoopNode::Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRate = MdotAir;
                        DataLoopNode::Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
                        DataLoopNode::Node(HPWaterHeater(HPNum).InletAirMixerNode).Temp = this->AmbientTemp;
                        DataLoopNode::Node(HPWaterHeater(HPNum).InletAirMixerNode).HumRat = AmbientHumRat;
                        DataLoopNode::Node(HPWaterHeater(HPNum).InletAirMixerNode).Enthalpy =
                            Psychrometrics::PsyHFnTdbW(this->AmbientTemp, AmbientHumRat);
                    } else {
                        if (HPWaterHeater(HPNum).OutsideAirNode == 0) {
                            DataLoopNode::Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRate = MdotAir;
                            DataLoopNode::Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRateMaxAvail = MdotAir;
                            DataLoopNode::Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).Temp = this->AmbientTemp;
                            DataLoopNode::Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).HumRat = AmbientHumRat;
                            DataLoopNode::Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).Enthalpy =
                                Psychrometrics::PsyHFnTdbW(this->AmbientTemp, AmbientHumRat);
                        } else {
                            DataLoopNode::Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRate = MdotAir;
                            DataLoopNode::Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRateMaxAvail = MdotAir;
                            DataLoopNode::Node(HPWaterHeater(HPNum).OutsideAirNode).Temp = this->AmbientTemp;
                            DataLoopNode::Node(HPWaterHeater(HPNum).OutsideAirNode).HumRat = AmbientHumRat;
                            DataLoopNode::Node(HPWaterHeater(HPNum).OutsideAirNode).Enthalpy =
                                Psychrometrics::PsyHFnTdbW(this->AmbientTemp, AmbientHumRat);
                        }
                    }

                    DataHVACGlobals::HPWHCrankcaseDBTemp = this->AmbientTemp;

                    if (UtilityRoutines::SameString(HPWaterHeater(HPNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") ||
                        (HPWaterHeater(HPNum).bIsIHP)) {
                        bIsVSCoil = true;
                        std::string VSCoilName = HPWaterHeater(HPNum).DXCoilName;
                        int VSCoilNum = HPWaterHeater(HPNum).DXCoilNum;
                        if (HPWaterHeater(HPNum).bIsIHP) {
                            VSCoilNum = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                            VSCoilName = IntegratedHeatPump::IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilName;
                        }

                        Real64 RhoWater = Psychrometrics::RhoH2O(this->TankTemp);
                        SetVSHPWHFlowRates(
                            WaterThermalTankNum, HPNum, VariableSpeedCoils::VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0, RhoWater, MdotWater, true);
                        //       simulate the HPWH coil/fan to find heating capacity
                        Real64 EMP1 = 0.0;
                        Real64 EMP2 = 0.0;
                        Real64 EMP3 = 0.0;
                        if (HPWaterHeater(HPNum).FanPlacement == DataHVACGlobals::BlowThru) {
                            //   simulate fan and DX coil twice
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            VariableSpeedCoils::SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  DataHVACGlobals::CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VariableSpeedCoils::VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            VariableSpeedCoils::SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  DataHVACGlobals::CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VariableSpeedCoils::VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                        } else {
                            //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                            VariableSpeedCoils::SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  DataHVACGlobals::CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VariableSpeedCoils::VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            VariableSpeedCoils::SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  DataHVACGlobals::CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VariableSpeedCoils::VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                        }

                        this->MaxCapacity = VariableSpeedCoils::VSHPWHHeatingCapacity;
                        this->MinCapacity = VariableSpeedCoils::VSHPWHHeatingCapacity;
                        this->Efficiency = VariableSpeedCoils::VSHPWHHeatingCOP;
                    } else {
                        bIsVSCoil = false;
                        //       simulate the HPWH coil/fan to find heating capacity
                        if (HPWaterHeater(HPNum).FanPlacement == DataHVACGlobals::BlowThru) {
                            if (FirstTimeFlag) { // first time DXCoils::DXCoil is called, it's sized at the RatedCondenserWaterInlet temp, size and reset water
                                                 // inlet temp. If already sized, no harm.
                                if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                    HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                                } else {
                                    Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                                }
                                DXCoils::SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, DataHVACGlobals::CycFanCycCoil, 1.0);
                                DataLoopNode::Node(HPWaterHeater(HPNum).CondWaterInletNode).Temp = this->TankTemp;
                            }
                            // ?? should only need to call twice if PLR<1 since this might affect OnOffFanPartLoadFraction which impacts fan energy.
                            // PLR=1 here.
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            DXCoils::SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, DataHVACGlobals::CycFanCycCoil, 1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            DXCoils::SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, DataHVACGlobals::CycFanCycCoil, 1.0);
                        } else {
                            if (FirstTimeFlag) { // first time DXCoils::DXCoil is called, it's sized at the RatedCondenserWaterInlet temp, size and reset water
                                                 // inlet temp. If already sized, no harm.
                                DXCoils::SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, DataHVACGlobals::CycFanCycCoil, 1.0);
                                DataLoopNode::Node(HPWaterHeater(HPNum).CondWaterInletNode).Temp = this->TankTemp;
                            }
                            // ?? should only need to call twice if PLR<1 since this might affect OnOffFanPartLoadFraction which impacts fan energy.
                            // PLR=1 here.
                            DXCoils::SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, DataHVACGlobals::CycFanCycCoil, 1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            DXCoils::SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, DataHVACGlobals::CycFanCycCoil, 1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                        }

                        this->MaxCapacity = DXCoils::HPWHHeatingCapacity;
                        this->MinCapacity = DXCoils::HPWHHeatingCapacity;
                        this->Efficiency = DXCoils::HPWHHeatingCOP;
                    }

                    if (FirstTimeFlag) {
                        RatedDXCoilTotalCapacity = DataHVACGlobals::DXCoilTotalCapacity;
                        FirstTimeFlag = false;
                    }

                    //       Switch the HPWH info with the tank info and call CalcWaterThermalTankMixed to get Standard Rating
                    //       (backup element is assumed to be disabled during the rating procedure)
                    this->SourceMassFlowRate = 0.0;
                    this->OnCycParaLoad = HPWaterHeater(HPNum).OnCycParaLoad;
                    this->OffCycParaLoad = HPWaterHeater(HPNum).OffCycParaLoad;
                    this->OffCycParaFracToTank = 0.0;
                    this->OnCycParaFracToTank = 0.0;
                    this->PLFCurve = HPWaterHeater(HPNum).DXCoilPLFFPLR;

                    {
                        auto const SELECT_CASE_var(this->TypeNum);

                        if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterMixed) {
                            if (this->Efficiency > 0.0) this->CalcWaterThermalTankMixed(WaterThermalTankNum);

                        } else if (SELECT_CASE_var == DataPlant::TypeOf_WtrHeaterStratified) {
                            if (this->Efficiency > 0.0) this->CalcWaterThermalTankStratified(WaterThermalTankNum);

                        } else {
                            //         Unhandled water heater type
                        }
                    }

                    //       reset the water heater data to original values
                    this->MaxCapacity = HPWaterHeater(HPNum).BackupElementCapacity;
                    this->MinCapacity = HPWaterHeater(HPNum).BackupElementCapacity;
                    this->Efficiency = HPWaterHeater(HPNum).BackupElementEfficiency;
                    this->OnCycParaLoad = HPWaterHeater(HPNum).WHOnCycParaLoad;
                    this->OffCycParaLoad = HPWaterHeater(HPNum).WHOffCycParaLoad;
                    this->OnCycParaFracToTank = HPWaterHeater(HPNum).WHOnCycParaFracToTank;
                    this->OffCycParaFracToTank = HPWaterHeater(HPNum).WHOffCycParaFracToTank;
                    this->PLFCurve = HPWaterHeater(HPNum).WHPLFCurve;
                }

                FuelEnergy += (this->FuelRate + this->OffCycParaFuelRate +
                               this->OnCycParaFuelRate) *
                              SecInTimeStep;

            } // Step

            if (this->FirstRecoveryDone && this->FirstRecoveryFuel > 0.0) {
                // Calculate Recovery Efficiency based on energy used to recover from the first draw
                // FirstRecoveryFuel is recorded inside the CalcWaterThermalTank subroutine
                RecoveryEfficiency = DrawMass * Psychrometrics::CPHW(57.2222) * (57.2222 - 14.4444) / this->FirstRecoveryFuel;

                // Calculate Energy Factor based on total energy (including parasitics) used over entire test
                EnergyFactor = TotalDrawMass * Psychrometrics::CPHW(57.2222) * (57.2222 - 14.4444) / FuelEnergy;

            } else {
                RecoveryEfficiency = 0.0;
                EnergyFactor = 0.0;
                if (HPWaterHeater.empty() || !HPWaterHeater(HPNum).bIsIHP) {
                    ShowWarningError("Water heater = " + this->Name +
                                     ":  Recovery Efficiency and Energy Factor could not be calculated during the test for standard ratings");
                    ShowContinueError("Setpoint was never recovered and/or heater never turned on");
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
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHType, equipName, this->Type);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHVol, equipName, this->Volume);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHHeatIn, equipName, this->MaxCapacity);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHThEff, equipName, this->Efficiency);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHRecEff, equipName, RecoveryEfficiency);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHEnFac, equipName, EnergyFactor);
        } else {
            equipName = HPWaterHeater(this->HeatPumpNum).Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHType, equipName, HPWaterHeater(this->HeatPumpNum).Type);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHVol, equipName, this->Volume);
            if (bIsVSCoil) {
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHHeatIn, equipName, VariableSpeedCoils::VSHPWHHeatingCapacity);
            } else {
                OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHHeatIn, equipName, DXCoils::HPWHHeatingCapacity);
            }
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHThEff, equipName, this->Efficiency);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHRecEff, equipName, RecoveryEfficiency);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchSWHEnFac, equipName, EnergyFactor);
        }

        // Write test results
        if (this->HeatPumpNum == 0) {
            Real64 MaxCapacity;
            if (this->TypeNum == DataPlant::TypeOf_WtrHeaterStratified) {
                if (this->ControlType == modPriorityMasterSlave) {
                    MaxCapacity = max(this->MaxCapacity, this->MaxCapacity2);
                } else { // PrioritySimultaneous
                    MaxCapacity = this->MaxCapacity + this->MaxCapacity2;
                }
            } else { // WaterHeaterMixed
                MaxCapacity = this->MaxCapacity;
            }

            ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_720) << this->Type << this->Name
                                                    << General::TrimSigDigits(this->Volume, 4) << General::TrimSigDigits(MaxCapacity, 1)
                                                    << General::TrimSigDigits(RecoveryEfficiency, 3) << General::TrimSigDigits(EnergyFactor, 4);
        } else {
            ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_721)
                << HPWaterHeater(this->HeatPumpNum).Type
                << HPWaterHeater(this->HeatPumpNum).Name
                << General::TrimSigDigits(this->Volume, 4) << General::TrimSigDigits(DXCoils::HPWHHeatingCapacity, 1)
                << General::TrimSigDigits(RecoveryEfficiency, 3) << General::TrimSigDigits(EnergyFactor, 4) << General::TrimSigDigits(RatedDXCoilTotalCapacity, 0);
        }

        this->AlreadyRated = true;
    }

    void WaterThermalTankData::ReportCWTankInits()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // send chilled water tank info to EIO

        // Formats
        static ObjexxFCL::gio::Fmt Format_728("('Chilled Water Tank Information',5(',',A))");

        if (this->MyOneTimeSetupFlag) {
            this->MyOneTimeSetupFlag = false;
        }

        if (this->AlreadyReported) { // bail we already did this one
            return;
        }

        ObjexxFCL::gio::write(DataGlobals::OutputFileInits, Format_728) << this->Type << this->Name
                                                << General::TrimSigDigits(this->Volume, 4)
                                                << General::TrimSigDigits(this->UseDesignVolFlowRate, 4)
                                                << General::TrimSigDigits(this->SourceDesignVolFlowRate, 4);

        this->AlreadyReported = true;
    }

    Real64 WaterThermalTankData::FindStratifiedTankSensedTemp(bool UseAverage)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  Noel Merket, April 2015

        // PURPOSE OF THIS FUNCTION:
        // find tank temperature depending on how sensed

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        HeatPumpWaterHeaterData const &HPWH = HPWaterHeater(this->HeatPumpNum);
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

} // namespace WaterThermalTanks

} // namespace EnergyPlus
